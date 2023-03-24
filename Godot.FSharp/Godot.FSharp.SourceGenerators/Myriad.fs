namespace Godot.FSharp.SourceGenerators

open System
open System.IO
open FSharp.Compiler.Syntax
open Fantomas.Core
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml
open FSharp.Compiler.SyntaxTrivia
open Myriad.Core
open Myriad.Core.Ast
open Godot.FSharp.SourceGenerators.GodotStubs


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
                | other -> ()
        ]

open Generator
open GodotStubs

[<MyriadGenerator("fsharp.godot.example")>]
type Example() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(context: GeneratorContext) =


            // let (ast: ParsedInput, _) =
            //     Ast.fromFilename context.InputFilename
            //     |> Async.RunSynchronously
            //     //myriad throws everything but the first thing away... guess it does that for a reason..
            //     |> Array.head

            // let modules =
            //     match ast with
            //     | ParsedInput.ImplFile(ParsedImplFileInput(_name,
            //                                                _isScript,
            //                                                _qualifiedNameOfFile,
            //                                                _scopedPragmas,
            //                                                _hashDirectives,
            //                                                modules,
            //                                                _g,
            //                                                _)) -> modules
            //     | _ -> "Not a valid file" |> System.Exception |> raise

            // let modulesWithAttribute =
            //     modules
            //     |> List.filter (fun x ->
            //         let SynModuleOrNamespace(namespaceId,
            //                                  _isRec: bool,
            //                                  _isModule,
            //                                  moduleDecls,
            //                                  _preXmlDoc,
            //                                  attributes,
            //                                  _access,
            //                                  _range,
            //                                  _) as ns: SynModuleOrNamespace =
            //             x

            //         attributes
            //         |> List.exists (fun (x: SynAttributeList) ->
            //             x.Attributes
            //             |> Seq.exists (fun x -> Ast.typeNameMatches typeof<Helper.NodeScriptAttribute> x)))

            // let nodeTypesAndStateTypes =
            //     modulesWithAttribute
            //     |> List.map (fun typeModule ->
            //         let SynModuleOrNamespace(namespaceId,
            //                                  _isRec,
            //                                  _isModule,
            //                                  moduleDecls,
            //                                  _preXmlDoc,
            //                                  _attributes,
            //                                  _access,
            //                                  _range,
            //                                  _) as ns =
            //             typeModule

            //         let types = Helper.extractTypes moduleDecls namespaceId

            //         let toGenerate =
            //             types
            //             |> List.map (fun (fullModName, types) ->
            //                 let typesToGenerate =
            //                     types
            //                     |> List.choose (fun (y: SynTypeDefn) ->
            //                         match y with
            //                         | SynTypeDefn(typeInfo,
            //                                       typeRepr,
            //                                       members,
            //                                       synMemberDefnOption,
            //                                       range,
            //                                       synTypeDefnTrivia) ->
            //                             match typeInfo with
            //                             | SynComponentInfo(attributes,
            //                                                typeParams,
            //                                                constraints,
            //                                                longId,
            //                                                xmlDoc,
            //                                                preferPostfix,
            //                                                accessibility,
            //                                                range) ->
            //                                 let hasNodeAttribute =
            //                                     attributes |> Helper.hasAttribute<Helper.NodeAttribute>

            //                                 let hasStateAttribute =
            //                                     attributes |> Helper.hasAttribute<Helper.StateAttribute>

            //                                 match hasNodeAttribute, hasStateAttribute with
            //                                 | (true, true) ->
            //                                     let x =
            //                                         System.Exception "Type is both node and state. This is invalid"

            //                                     x.Data.Add("Type:", fullModName)
            //                                     raise x
            //                                 | (false, false) -> None
            //                                 | (true, false) -> typeInfo |> Helper.Node |> Some
            //                                 | (false, true) -> typeInfo |> Helper.State |> Some)

            //                 if typesToGenerate.Length <> 2 then

            //                     let exc = "Too many types with attributes found" |> System.Exception
            //                     exc.Data.Add("Modname", fullModName)
            //                     exc.Data.Add("List", typesToGenerate)
            //                     raise exc

            //                 let nodeExtension =
            //                     typesToGenerate
            //                     |> List.choose (function
            //                         | Helper.NodeOrState.Node x -> Some x
            //                         | _ -> None)
            //                     |> Seq.head

            //                 let state =
            //                     typesToGenerate
            //                     |> List.choose (function
            //                         | Helper.NodeOrState.State x -> Some x
            //                         | _ -> None)
            //                     |> Seq.head

            //                 nodeExtension, state)

            //         toGenerate)
            try
                let toGenerate: List<ToGenerateInfo> =
                    [
                        {

                            Extending = "Node"
                            Name = "MyNode"
                            methods =
                                [
                                    {
                                        MethodName = "_Ready"
                                        IsOverride = true
                                        MethodParams = []
                                        MethodFlags = MethodFlags.Default
                                    }
                                    {
                                        MethodName = "_Process"
                                        IsOverride = true
                                        MethodParams =
                                            [
                                                {

                                                    Name = "delta"
                                                    OfTypeName = "double"
                                                    OfType = Type.Float
                                                    PropertyHint = PropertyHint.None
                                                    UsageFlags = PropertyUsageFlags.Default
                                                    HintText = ""
                                                }
                                            ]
                                        MethodFlags = MethodFlags.Default
                                    }
                                ]

                            StateToGenerate =
                                {
                                    Name = "BasicState"
                                    ExportedFields =
                                        [
                                            {
                                                Name = "Hello"
                                                OfTypeName = "int"
                                                OfType = Type.Int
                                                PropertyHint = PropertyHint.None
                                                HintText = ""
                                                UsageFlags = PropertyUsageFlags.Default ||| PropertyUsageFlags.ScriptVariable
                                            }
                                        ]
                                    InnerFields =
                                        [
                                            {
                                                Name = "IAmInner"
                                                OfTypeName = "string"
                                                OfType = Type.String
                                                PropertyHint = PropertyHint.None
                                                HintText = ""
                                                UsageFlags = PropertyUsageFlags.ScriptVariable
                                            }
                                        ]
                                }
                            InNamespace = "GeneratedNodes"
                            ModuleNameToOpen = "TestFSharpGodot.Say"
                        }
                    ]

                let generatedStr = toGenerate |> Seq.map generateClass |> String.concat "\n\n"
                let logger = File.CreateText "./output.debug.myriad.txt"
                logger.Write generatedStr
                logger.Flush()
                logger.Close()
                Output.Source generatedStr
            with x ->
                let logger = File.CreateText "./output.debug.myriad.txt"
                logger.Write x
                logger.Flush()
                logger.Close()
                raise x
