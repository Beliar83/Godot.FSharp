namespace Godot.FSharp.SourceGenerators

open System
open FSharp.Compiler.Syntax
open Fantomas.Core
open Myriad.Core.Ast


module Helper =
    type NodeScriptAttribute() =
        inherit System.Attribute()

    type NodeAttribute() =
        inherit System.Attribute()

    type StateAttribute() =
        inherit System.Attribute()

    type ExportAttribute() =
        inherit System.Attribute()

    type NodeOrState =
        | Node of SynComponentInfo
        | State of SynComponentInfo

    type INodeWithState<'Node, 'State> =
        abstract member GetState: unit -> 'State
        abstract member SetState: 'State -> unit
        abstract member GetNode: unit -> 'Node

    let hasAttribute<'a> (a: SynAttributes) =
        a
        |> List.exists (fun a -> a.Attributes |> List.exists (fun x -> typeNameMatches typeof<'a> x))

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

    let extractNodeType (typ: SynTypeDefn) =
        let SynTypeDefn(typeInfo, typeRepr, members, implicitConstructor, range, trivia) as ty =
            typ

        if hasAttribute<NodeAttribute> typeInfo.attributes then
            match typeRepr with
            | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev(detail, rhsType, range), _) -> Some rhsType
            | _ -> None
        else
            None

    let extractStateType (typ: SynTypeDefn) =
        let SynTypeDefn(typeInfo, typeRepr, members, implicitConstructor, range, trivia) as ty =
            typ

        if hasAttribute<StateAttribute> typeInfo.attributes then
            match typeRepr with
            | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, recordFields, _), _) ->
                Some(typeInfo, recordFields)
            | _ -> None
        else
            None

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

    let extractPart moduleDecls =
        extractTypesNonRecusrive moduleDecls
        |> List.choose (fun x ->
            let state = extractStateType x
            let node = extractNodeType x

            match (state, node) with
            | (Some x, Some y) ->

                Some(x, y)
            | (None, _) -> "Missing state in node module" |> Exception |> raise
            | (_, None) -> "Missing node in node module" |> Exception |> raise)
