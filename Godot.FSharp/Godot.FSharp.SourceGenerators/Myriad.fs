namespace MyriadgodotMyriad

open System
open System.IO
open FSharp.Compiler
open FSharp.Compiler.Syntax
open Myriad.Core
open FsAst
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

    let hasAttribute<'a> (a: SynAttributes) =
        a
        |> List.exists (fun a -> a.Attributes |> List.exists (fun attr -> Ast.typeNameMatches typeof<'a> attr))

    let rec extractTypes (moduleDecls: SynModuleDecl list) (ns: LongIdent) =
        [ for moduleDecl in moduleDecls do
              match moduleDecl with
              | SynModuleDecl.Types(types, _) -> yield (ns, types)
              | SynModuleDecl.NestedModule(
                  synComponentInfo,
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
              | other -> () ]

[<MyriadGenerator("example")>]
type Example() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(context: GeneratorContext) =

            let exampleNamespace =
                context.ConfigKey
                |> Option.map context.ConfigGetter
                |> Option.bind (Seq.tryPick (fun (n, v) -> if n = "namespace" then Some(v :?> string) else None))
                |> Option.defaultValue "UnknownNamespace"

            let letHelloWorld =
                SynModuleDecl.CreateLet
                    [ { SynBindingRcd.Let with
                          Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString "hello", [])
                          Expr = SynExpr.CreateConst(SynConst.CreateString "world!") } ]

            let (ast: ParsedInput, _) =
                Ast.fromFilename context.InputFilename
                |> Async.RunSynchronously
                //myriad throws everything but the first thing away... guess it does that for a reason..
                |> Array.head

            let modules =
                match ast with
                | ParsedInput.ImplFile(ParsedImplFileInput(_name,
                                                           _isScript,
                                                           _qualifiedNameOfFile,
                                                           _scopedPragmas,
                                                           _hashDirectives,
                                                           modules,
                                                           _g)) -> modules
                | _ -> "Not a valid file" |> System.Exception |> raise

            let modulesWithAttribute =
                modules
                |> List.filter (fun x ->
                    let SynModuleOrNamespace(namespaceId,
                                             _isRec: bool,
                                             _isModule,
                                             moduleDecls,
                                             _preXmlDoc,
                                             attributes,
                                             _access,
                                             _range) as ns: SynModuleOrNamespace =
                        x

                    attributes
                    |> List.exists (fun (x: SynAttributeList) ->
                        x.Attributes
                        |> Seq.exists (fun x -> Ast.typeNameMatches typeof<Helper.NodeScriptAttribute> x)))

            let nodeTypesAndStateTypes =
                modulesWithAttribute
                |> List.map (fun typeModule ->
                    let SynModuleOrNamespace(namespaceId,
                                             _isRec,
                                             _isModule,
                                             moduleDecls,
                                             _preXmlDoc,
                                             _attributes,
                                             _access,
                                             _range) as ns =
                        typeModule

                    let types = Helper.extractTypes moduleDecls namespaceId

                    let toGenerate =
                        types
                        |> List.map (fun (fullModName, types) ->
                            let typesToGenerate =
                                types
                                |> List.choose (fun (y: SynTypeDefn) ->
                                    match y with
                                    | SynTypeDefn(typeInfo, typeRepr, members, synMemberDefnOption, range, synTypeDefnTrivia) ->
                                        match typeInfo with
                                        | SynComponentInfo(
                                            attributes,
                                            typeParams,
                                            constraints,
                                            longId,
                                            xmlDoc,
                                            preferPostfix,
                                            accessibility,
                                            range) ->
                                            let hasNodeAttribute =
                                                attributes |> Helper.hasAttribute<Helper.NodeAttribute>

                                            let hasStateAttribute =
                                                attributes |> Helper.hasAttribute<Helper.StateAttribute>

                                            match hasNodeAttribute, hasStateAttribute with
                                            | (true, true) ->
                                                let x =
                                                    System.Exception "Type is both node and state. This is invalid"

                                                x.Data.Add("Type:", fullModName)
                                                raise x
                                            | (false, false) -> None
                                            | (true, false) -> typeInfo |> Helper.Node |> Some
                                            | (false, true) -> typeInfo |> Helper.State |> Some)

                            if typesToGenerate.Length <> 2 then

                                let exc = "Too many types with attributes found" |> System.Exception
                                exc.Data.Add("Modname", fullModName)
                                exc.Data.Add("List", typesToGenerate)
                                raise exc

                            let nodeExtension =
                                typesToGenerate
                                |> List.choose (function
                                    | Helper.NodeOrState.Node x -> Some x
                                    | _ -> None)
                                |> Seq.head

                            let state =
                                typesToGenerate
                                |> List.choose (function
                                    | Helper.NodeOrState.State x -> Some x
                                    | _ -> None)
                                |> Seq.head

                            nodeExtension, state)

                    toGenerate)

            let myModule =
                File.ReadAllLines context.InputFilename
                |> Seq.map (fun moduleName ->
                    let componentInfo = SynComponentInfoRcd.Create [ Ident.Create(moduleName) ]
                    let module' = SynModuleDecl.CreateNestedModule(componentInfo, [ letHelloWorld ])
                    module')
                |> Seq.toList

            let namespaceOrModule =
                { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong exampleNamespace) with
                    Declarations = myModule }

            Output.Ast [ namespaceOrModule.FromRcd ]