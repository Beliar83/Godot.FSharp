namespace Godot.FSharp.SourceGenerators

open System
open System.Text
open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Godot.FSharp.SourceGenerators.GodotStubs
open Godot.FSharp.SourceGenerators.Variant
open Ionide.ProjInfo
open Myriad.Core
open System.IO
open Fantomas.Core

[<MyriadGenerator("ResourceGenerator")>]
type ResourceGenerator() =

    let listDelimiter = ", "

    let handleUnion (builder: StringBuilder) (union: FSharpEntity) =
        let name = union.DisplayName
        let cases = union.UnionCases

        let scope =
            match union.DeclaringEntity with
            | Some entity -> entity.FullName
            | None -> "global"

        let caseHint =
            let cases =
                cases |> Seq.map (fun x -> x.DisplayName)

            "Unset," + String.Join(",", cases)

        let casePropertyDefinitionBuilder = StringBuilder()
        let getTypeBuilder = StringBuilder()
        let setTypeBuilder = StringBuilder()
        let caseGetBuilder = StringBuilder()
        let caseSetBuilder = StringBuilder()

        casePropertyDefinitionBuilder
            .AppendLine("\t\tmatch internalValue with")
            .AppendLine("\t\t| None -> ()")
            .AppendLine("\t\t| Some union ->")
            .AppendLine("\t\t\tmatch union with")
        |> ignore

        for i, case in cases |> Seq.mapi (fun i x -> (i, x)) do
            let caseName = case.DisplayName

            getTypeBuilder.AppendLine($"\t\t\t\t| {scope}.{name}.{caseName} _ -> Variant.CreateFrom {i + 1}")
            |> ignore

            casePropertyDefinitionBuilder.AppendLine($"\t\t\t| {scope}.{name}.{caseName} _ ->")
            |> ignore

            let defaultValues = ResizeArray()
            let fieldGetBuilder = StringBuilder()
            let fieldSetBuilder = StringBuilder()
            let fieldNames = ResizeArray()

            for field in case.Fields do

                let fieldName = field.DisplayName
                fieldNames.Add <| fieldName

                fieldGetBuilder.AppendLine(
                    $"\t\t\t\t\t\t| \"{fieldName}\" -> Variant.CreateFrom {fieldName.ToLowerInvariant()}"
                )
                |> ignore

                let godotType =
                    match getTypeNameFromIdent.convertFSharpTypeToVariantType field.FieldType with
                    | None -> Type.Nil
                    | Some value ->
                        match value with
                        | None -> Type.Nil
                        | Some value -> value

                let defaultValue =
                    match getGodotDefault godotType with
                    | Object -> $"new {field.FieldType.TypeDefinition.FullName}()"
                    | Defined value -> value

                defaultValues.Add defaultValue

                casePropertyDefinitionBuilder
                    .AppendLine("\t\t\t\tlet propertyDefinition = new Dictionary()")
                    .AppendLine($"\t\t\t\tpropertyDefinition.Add(\"name\", \"{fieldName}\")")
                    .AppendLine($"\t\t\t\tpropertyDefinition.Add(\"type\", \"{int godotType}\")")
                    .AppendLine(
                        "\t\t\t\tpropertyDefinition.Add(\"usage\", int(PropertyUsageFlags.Default ||| PropertyUsageFlags.Editor))"
                    )
                    .AppendLine("\t\t\t\tpropertyList.Add propertyDefinition")
                    .AppendLine()
                |> ignore

            let fieldNamesLower =
                fieldNames
                |> Seq.map (fun x -> x.ToLowerInvariant())

            let fieldsString =
                String.Join(listDelimiter, fieldNamesLower)

            let fieldTupleString =
                if fieldNames.Count > 1 then
                    $"({fieldsString})"
                else
                    fieldsString

            let copyDuWithValue position =
                fieldNamesLower
                |> Seq.mapi (fun i x -> if i = position then "value.As()" else x)

            for i, fieldName in fieldNames |> Seq.mapi (fun i x -> (i, x)) do
                let copiedFieldsString =
                    String.Join(listDelimiter, copyDuWithValue i)

                fieldSetBuilder
                    .AppendLine($"\t\t\t\t\t| \"{fieldName}\" ->")
                    .AppendLine($"\t\t\t\t\t\tinternalValue <- Some({scope}.{name}.{caseName}({copiedFieldsString}))")
                    .AppendLine("\t\t\t\t\t\ttrue")
                |> ignore



            setTypeBuilder.AppendLine(
                $"\t\t\t\t| {i + 1} -> Some({scope}.{name}.{caseName}({String.Join(listDelimiter, defaultValues)}))"
            )
            |> ignore


            caseGetBuilder
                .AppendLine($"\t\t\t\t\t| {scope}.{name}.{caseName} {fieldTupleString} ->")
                .AppendLine("\t\t\t\t\t\tmatch property.ToString() with")
                .Append(fieldGetBuilder)
                .AppendLine("\t\t\t\t\t\t| _ -> base._Get(property)")
            |> ignore

            caseSetBuilder
                .AppendLine(
                    $"\t\t\t\t| {scope}.{name}.{caseName} {if fieldNames.Count > 1 then
                                                               fieldTupleString
                                                           else
                                                               '_'.ToString()} ->"
                )
                .AppendLine("\t\t\t\t\tmatch property.ToString() with")
                .Append(fieldSetBuilder)
                .AppendLine("\t\t\t\t\t| _ -> false")
            |> ignore

        builder
            .AppendLine($"type {name}()=")
            .AppendLine("\tinherit Resource()")
            .AppendLine()
            .AppendLine($"\tlet mutable internalValue : Option<{scope}.{name}> = None")
            .AppendLine()
            .AppendLine("\toverride this._GetPropertyList() =")
            .AppendLine("\t\tlet propertyList = Array<Dictionary>()")
            .AppendLine()
            .AppendLine("\t\tlet propertyDefinition = new Dictionary()")
            .AppendLine("\t\tpropertyDefinition.Add(\"name\", \"Type\")")
            .AppendLine("\t\tpropertyDefinition.Add(\"type\", int Variant.Type.Int)")
            .AppendLine(
                "\t\tpropertyDefinition.Add(\"usage\", int(PropertyUsageFlags.Default ||| PropertyUsageFlags.Editor))"
            )
            .AppendLine("\t\tpropertyDefinition.Add(\"hint\", int PropertyHint.Enum )")
            .AppendLine($"\t\tpropertyDefinition.Add(\"hint_string\", \"{caseHint}\")")
            .AppendLine("\t\tpropertyList.Add propertyDefinition")
            .AppendLine()
            .Append(casePropertyDefinitionBuilder.ToString())
            .AppendLine("\t\tpropertyList")
            .AppendLine()
            .AppendLine()
            .AppendLine("\toverride this._Get(property) =")
            .AppendLine("\t\tmatch property.ToString() with")
            .AppendLine("\t\t| \"Type\" ->")
            .AppendLine("\t\t\tmatch internalValue with")
            .AppendLine("\t\t\t| None -> Variant.CreateFrom 0")
            .AppendLine("\t\t\t| Some union -> ")
            .AppendLine("\t\t\t\tmatch union with")
            .Append(getTypeBuilder.ToString())
            .AppendLine("\t\t| _ ->")
            .AppendLine("\t\t\tmatch internalValue with")
            .AppendLine("\t\t\t\t| None -> base._Get(property)")
            .AppendLine("\t\t\t\t| Some union -> ")
            .AppendLine("\t\t\t\t\tmatch union with")
            .Append(caseGetBuilder.ToString())
            .AppendLine("\toverride this._Set(property, value) =")
            .AppendLine("\t\tif property.ToString() = \"Type\" then")
            .AppendLine("\t\t\tinternalValue <-")
            .AppendLine("\t\t\t\tmatch value.AsInt32() with")
            .AppendLine("\t\t\t\t| 0 -> None")
            .Append(setTypeBuilder.ToString())
            .AppendLine("\t\t\t\t| _ -> internalValue")
            .AppendLine("\t\t\tthis.NotifyPropertyListChanged()")
            .AppendLine("\t\t\ttrue")
            .AppendLine("\t\telse")
            .AppendLine("\t\t\tmatch internalValue with")
            .AppendLine("\t\t\t| None -> false")
            .AppendLine("\t\t\t| Some union ->")
            .AppendLine("\t\t\t\tmatch union with")
            .Append(caseSetBuilder.ToString())
            .AppendLine()
        |> ignore

    let rec writeResourcesOfScope (builder: StringBuilder) (namespaceOrModule: string) (resources: FSharpEntity list) =
        let resourcesStringBuilder = StringBuilder()

        for resource in resources do
            if resource.IsFSharpUnion then
                handleUnion resourcesStringBuilder resource

        if resourcesStringBuilder.Length > 0 then
            builder
                .AppendLine($"namespace Resources.{namespaceOrModule}")
                .AppendLine("open System")
                .AppendLine("open Godot")
                .AppendLine("open Godot.Collections")
                .Append(resourcesStringBuilder.ToString())
            |> ignore

    let rec extractRecordsAndUnions (declarations: FSharpImplementationFileDeclaration list) =
        declarations
        |> List.choose
            (fun x ->
                match x with
                | FSharpImplementationFileDeclaration.Entity (entity, declarations) -> Some(entity, declarations)
                | _ -> None)
        |> List.collect
            (fun (x, entityDeclarations) ->
                if x.IsNamespace || x.IsFSharpModule then
                    extractRecordsAndUnions entityDeclarations
                elif x.IsFSharpRecord || x.IsFSharpUnion then
                    [ x ]
                else
                    [ x ])

    interface IMyriadGenerator with
        member _.Generate(context: GeneratorContext) =
            let checker =
                FSharpChecker.Create(keepAssemblyContents = true)

            let projectContext =
                match context.ProjectContext with
                | None -> "No project context" |> Exception |> raise
                | Some projectContext -> projectContext

            let sourceBuilder = StringBuilder()

            let projectDirectory =
                DirectoryInfo(Path.GetDirectoryName projectContext.projectPath)

            let toolsPath = Init.init projectDirectory None

            let defaultLoader: IWorkspaceLoader = WorkspaceLoader.Create(toolsPath, [])

            let options =
                defaultLoader.LoadProjects [ projectContext.projectPath ]
                |> FCS.mapManyOptions
                |> Seq.head

            let file =
                context.InputFilename
                |> File.ReadAllText
                |> SourceText.ofString


            let _, answer =
                checker.ParseAndCheckFileInProject(context.InputFilename, 1, file, options)
                |> Async.RunSynchronously


            let answer =
                match answer with
                | FSharpCheckFileAnswer.Aborted -> "Could not parse file" |> Exception |> raise
                | FSharpCheckFileAnswer.Succeeded x -> x

            let contents =
                match answer.ImplementationFile with
                | None -> "Could not parse file" |> Exception |> raise
                | Some fileContents -> fileContents

            let resources =
                extractRecordsAndUnions contents.Declarations
                |> List.filter
                    (fun x ->
                        x.Attributes
                        |> Seq.exists (fun x -> x.IsAttribute<Helper.ResourceAttribute>()))

            let resourcesByNamespaceOrModule =
                resources
                |> List.groupBy
                    (fun x ->
                        match x.DeclaringEntity with
                        | None -> "global"
                        | Some value -> value.DisplayName)


            for namespaceOrModule, resources in resourcesByNamespaceOrModule do
                writeResourcesOfScope sourceBuilder namespaceOrModule resources


            Output.Source
            <| sourceBuilder.ToString().Replace("\t", "    ")

        member _.ValidInputExtensions = seq { ".fs" }
