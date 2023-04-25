namespace Godot.FSharp.SourceGenerators

open System
open System.IO
open FSharp.Compiler.Syntax
open Fantomas.Core
open Myriad.Core
open Godot.FSharp.SourceGenerators.GodotStubs

open Generator
open GodotStubs

// [<MyriadGenerator("fsharp.godot.example")>]
// type Example() =
//     interface IMyriadGenerator with
//         member _.ValidInputExtensions = seq { ".fs" }
//
//         member _.Generate(context: GeneratorContext) =
//
//             let (ast: ParsedInput, _) =
//                 Ast.fromFilename context.InputFilename
//                 |> Async.RunSynchronously
//                 //myriad throws everything but the first thing away... guess it does that for a reason..
//                 |> Array.head
//
//             let modules =
//                 match ast with
//                 | ParsedInput.ImplFile(ParsedImplFileInput(_name,
//                                                            _isScript,
//                                                            _qualifiedNameOfFile,
//                                                            _scopedPragmas,
//                                                            _hashDirectives,
//                                                            modules,
//                                                            _g,
//                                                            _)) -> modules
//                 | _ -> "Not a valid file" |> Exception |> raise
//
//             let modulesWithAttribute =
//                 modules
//                 |> List.filter (fun x ->
//                     let SynModuleOrNamespace(namespaceId,
//                                              _isRec: bool,
//                                              _isModule,
//                                              moduleDecls,
//                                              _preXmlDoc,
//                                              attributes,
//                                              _access,
//                                              _range,
//                                              _) as _ =
//                         x
//
//                     attributes
//                     |> List.exists (fun (x: SynAttributeList) ->
//                         x.Attributes
//                         |> Seq.exists (fun x -> Ast.typeNameMatches typeof<Helper.NodeScriptAttribute> x)))
//
//             let parsedNodeModules =
//                 modulesWithAttribute
//                 |> List.map (fun modul ->
//                     //get every type with the Node attribute
//                     let SynModuleOrNamespace(namespaceId,
//                                              _isRec,
//                                              _isModule,
//                                              moduleDecls,
//                                              _preXmlDoc,
//                                              _attributes,
//                                              _access,
//                                              _range,
//                                              _) as _ =
//                         modul
//
//                     let (state, node) = Helper.extractPart moduleDecls |> List.head
//                     //get all the methods here!
//                     let functions = Helper.extractFunctions moduleDecls
//                     (state, node, functions)
//
//                 )
//
//             try
//                 let toGenerate: List<ToGenerateInfo> =
//                     [
//                         {
//
//                             Extending = "Node"
//                             Name = "MyNode"
//                             methods =
//                                 [
//                                     {
//                                         MethodName = "_Ready"
//                                         IsOverride = true
//                                         MethodParams = []
//                                         MethodFlags = MethodFlags.Default
//                                     }
//                                     {
//                                         MethodName = "_Process"
//                                         IsOverride = true
//                                         MethodParams =
//                                             [
//                                                 {
//
//                                                     Name = "delta"
//                                                     OfTypeName = "double"
//                                                     OfType = Type.Float
//                                                     PropertyHint = PropertyHint.None
//                                                     UsageFlags = PropertyUsageFlags.Default
//                                                     HintText = ""
//                                                 }
//                                             ]
//                                         MethodFlags = MethodFlags.Default
//                                     }
//                                 ]
//
//                             StateToGenerate =
//                                 {
//                                     Name = "BasicState"
//                                     ExportedFields =
//                                         [
//                                             {
//                                                 Name = "Hello"
//                                                 OfTypeName = "int"
//                                                 OfType = Type.Int
//                                                 PropertyHint = PropertyHint.None
//                                                 HintText = ""
//                                                 UsageFlags =
//                                                     PropertyUsageFlags.Default ||| PropertyUsageFlags.ScriptVariable
//                                             }
//                                         ]
//                                     InnerFields =
//                                         [
//                                             {
//                                                 Name = "IAmInner"
//                                                 OfTypeName = "string"
//                                                 OfType = Type.String
//                                                 PropertyHint = PropertyHint.None
//                                                 HintText = ""
//                                                 UsageFlags = PropertyUsageFlags.ScriptVariable
//                                             }
//                                         ]
//                                 }
//                             InNamespace = "GeneratedNodes"
//                             ModuleNameToOpen = "TestFSharpGodot.Say"
//                         }
//                     ]
//
//                 let generatedStr = toGenerate |> Seq.map generateClass |> String.concat "\n\n"
//                 let logger = File.CreateText "./output.debug.myriad.txt"
//                 logger.Write generatedStr
//                 logger.Flush()
//                 logger.Close()
//                 Output.Source generatedStr
//             with x ->
//                 let logger = File.CreateText "./output.debug.myriad.txt"
//                 logger.Write x
//                 logger.Flush()
//                 logger.Close()
//                 raise x
