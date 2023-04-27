namespace Godot.FSharp.SourceGenerators

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open Fantomas.Core
open Ionide.ProjInfo
open Myriad.Core
open FSharp.Compiler.Text

module Helper =
    type NodeScriptAttribute() =
        inherit System.Attribute()

    type NodeAttribute() =
        inherit System.Attribute()

    type StateAttribute() =
        inherit System.Attribute()

    type ExportAttribute() =
        inherit System.Attribute()

    [<AttributeUsage(AttributeTargets.Class)>]
    type ResourceAttribute() =
        inherit System.Attribute()
    
    type NodeOrState =
        | Node of SynComponentInfo
        | State of SynComponentInfo

    type INodeWithState<'Node, 'State> =
        abstract member GetState: unit -> 'State
        abstract member SetState: 'State -> unit
        abstract member GetNode: unit -> 'Node

    let defaultPropertyUsage = "int(PropertyUsageFlags.Default ||| PropertyUsageFlags.Editor)"
    
    // let hasAttribute<'a> (a: SynAttributes) =
    //     a
    //     |> List.exists (fun a -> a.Attributes |> List.exists (fun x -> typeNameMatches typeof<'a> x))

    let rec extractTypes (moduleDecls: SynModuleDecl list) (ns: LongIdent) =
        [
            for moduleDecl in moduleDecls do
                match moduleDecl with
                | SynModuleDecl.Types(types, _) -> yield (ns, types)
                | SynModuleDecl.NestedModule(synComponentInfo,
                                             isRec,
                                             decls,
                                             isContinuing,
                                             range,
                                             synModuleDeclNestedModuleTrivia) ->
                    match synComponentInfo with
                    | SynComponentInfo(attribs,
                                       typeParams,
                                       constraints,
                                       longId,
                                       xmlDoc,
                                       preferPostfix,
                                       accessibility,
                                       range) ->
                        let combined = longId |> List.append ns
                        yield! (extractTypes decls combined)
                | other -> yield! extractTypes [ other ] ns
        ]

    let extractTypesNonRecusrive (moduleDecls: SynModuleDecl list) =
        [
            for moduleDecl in moduleDecls do
                match moduleDecl with
                | SynModuleDecl.Types(types, _) -> yield! types
                | _ -> ()
        ]

    // let extractNodeType (typ: SynTypeDefn) =
    //     let SynTypeDefn(typeInfo, typeRepr, members, implicitConstructor, range, trivia) as ty =
    //         typ
    //
    //     if hasAttribute<NodeAttribute> typeInfo.attributes then
    //         match typeRepr with
    //         | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev(detail, rhsType, range), _) -> Some rhsType
    //         | _ -> None
    //     else
    //         None

    // let extractStateType (typ: SynTypeDefn) =
    //     let SynTypeDefn(typeInfo, typeRepr, members, implicitConstructor, range, trivia) as ty =
    //         typ
    //
    //     if hasAttribute<StateAttribute> typeInfo.attributes then
    //         match typeRepr with
    //         | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, recordFields, _), _) ->
    //             Some(typeInfo, recordFields)
    //         | _ -> None
    //     else
    //         None

    let extractFunctions (moduleDecls: SynModuleDecl list) =
        [
            for moduleDecl in moduleDecls do
                match moduleDecl with
                | SynModuleDecl.Let(isRecursive, bindings, range) ->
                    bindings
                    |> List.choose (fun x ->
                        let SynBinding(accessibility,
                                       kind,
                                       isInline,
                                       isMutable,
                                       attributes,
                                       xmlDoc,
                                       valData,
                                       headPat,
                                       returnInfo,
                                       expr,
                                       range,
                                       debugPoint,
                                       trivia) as _ =
                            x

                        match (accessibility, valData) with
                        | _, SynValData(_, SynValInfo([], _), _) -> None
                        | (Some(SynAccess.Public(_) | SynAccess.Internal(_)) | None),
                          SynValData(_, SynValInfo(x, _), _) -> Some(valData, headPat, x)
                        | (Some(SynAccess.Private(_)), _) -> None)
                | _ -> ()
        ]

    // let extractPart moduleDecls =
    //     extractTypesNonRecusrive moduleDecls
    //     |> List.choose (fun x ->
    //         let state = extractStateType x
    //         let node = extractNodeType x
    //
    //         match (state, node) with
    //         | (Some x, Some y) ->
    //
    //             Some(x, y)
    //         | (None, _) -> "Missing state in node module" |> Exception |> raise
    //         | (_, None) -> "Missing node in node module" |> Exception |> raise)
    
    let getFileResultsFromGeneratorContext (context : GeneratorContext) =
        let checker =
            FSharpChecker.Create(keepAssemblyContents = true)

        let projectContext =
            match context.ProjectContext with
            | None -> "No project context" |> Exception |> raise
            | Some projectContext -> projectContext

        let projectDirectory =
            DirectoryInfo(Path.GetDirectoryName projectContext.projectPath)

        let toolsPath = Init.init projectDirectory None

        let defaultLoader: IWorkspaceLoader = WorkspaceLoader.Create(toolsPath, [])

        let projectOptions =
            defaultLoader.LoadProjects [ projectContext.projectPath ]
            |> FCS.mapManyOptions
            |> Seq.head

        let file =
            context.InputFilename
            |> File.ReadAllText
            |> SourceText.ofString            

        let _, answer =
            checker.ParseAndCheckFileInProject(context.InputFilename, 1, file, projectOptions)
            |> Async.RunSynchronously
        
        match answer with
        | FSharpCheckFileAnswer.Aborted -> "Could not parse file" |> Exception |> raise
        | FSharpCheckFileAnswer.Succeeded x -> x
    
    let getImplementationFileContentsFromFileResults (fileResults : FSharpCheckFileResults) =
        match fileResults.ImplementationFile with
        | None -> "Could not parse file" |> Exception |> raise
        | Some fileContents -> fileContents
        
    let getImplementationFileContentsFromGeneratorContext (context : GeneratorContext) =
        getImplementationFileContentsFromFileResults <| getFileResultsFromGeneratorContext context
                