namespace Godot.FSharp.SourceGenerators

open System
open System.Text
open FSharp.Compiler.Symbols
open Godot.FSharp.SourceGenerators.GodotStubs
open Godot.FSharp.SourceGenerators.Variant
open Myriad.Core
open System.IO
open Fantomas.Core

type ResourceGenerator() =

    let listDelimiter = ", "


    let getScope (entity: FSharpEntity) =
        match entity.DeclaringEntity with
        | Some declaringEntity ->
            match declaringEntity.Namespace with
            | Some value -> $"{value}.{declaringEntity.DisplayName}"
            | None -> declaringEntity.FullName
        | None ->
            match entity.Namespace with
            | Some value -> value
            | None -> "global"

    let handleRecord (builder: StringBuilder) (record: FSharpEntity) =
        let hasDefaultMember =
            record.MembersFunctionsAndValues
            |> Seq.exists
                (fun x ->
                    x.IsMember
                    && not <| x.IsInstanceMember
                    && x.ReturnParameter.Type.TypeDefinition = record
                    && x.DisplayName = "Default")

        if not <| hasDefaultMember then
            "Resource records mus have a static 'Default' member which should return an instance of the record"
            |> Exception
            |> raise

        let name = record.DisplayName
        let scope = getScope record

        let propertyDefinitionBuilder = StringBuilder()
        let getBuilder = StringBuilder()
        let setBuilder = StringBuilder()

        let publicFields =
            record.FSharpFields
            |> Seq.filter (fun x -> x.Accessibility.IsPublic)
            |> Seq.filter (fun x -> x.DisplayName <> "Default")

        for field in publicFields do
            let godotType =
                match getTypeNameFromIdent.convertFSharpTypeToVariantType field.FieldType with
                | None -> Type.Nil
                | Some value ->
                    match value with
                    | None -> Type.Nil
                    | Some value -> value

            let fieldName = field.DisplayName

            propertyDefinitionBuilder
                .AppendLine("\t\tlet propertyDefinition = new Dictionary()")
                .AppendLine($"\t\tpropertyDefinition.Add(\"name\", \"{fieldName}\")")
                .AppendLine($"\t\tpropertyDefinition.Add(\"type\", \"{int godotType}\")")
                .AppendLine($"\t\tpropertyDefinition.Add(\"usage\", {Helper.defaultPropertyUsage})")
                .AppendLine("\t\tpropertyList.Add propertyDefinition")
                .AppendLine()
            |> ignore

            getBuilder.AppendLine($"\t\t| \"{fieldName}\" -> Variant.CreateFrom internalValue.{fieldName}")
            |> ignore

            setBuilder
                .AppendLine($"\t\t| \"{fieldName}\" ->")
                .AppendLine($"\t\t\tinternalValue <- {{ internalValue with {fieldName} = value.As() }}")
                .AppendLine("\t\t\ttrue")
            |> ignore


        builder
            .AppendLine($"type {name}()=")
            .AppendLine("\tinherit Resource()")
            .AppendLine()
            .AppendLine($"\tlet mutable internalValue : {scope}.{name} = {scope}.{name}.Default")
            .AppendLine()
            .AppendLine("\toverride this._GetPropertyList() =")
            .AppendLine("\t\tlet propertyList = Array<Dictionary>()")
            .AppendLine()
            .Append(propertyDefinitionBuilder.ToString())
            .AppendLine()
            .AppendLine("\t\tpropertyList")
            .AppendLine()
            .AppendLine("\toverride this._Get(property) =")
            .AppendLine("\t\tmatch property.ToString() with")
            .Append(getBuilder.ToString())
            .AppendLine("\t\t| _ -> base._Get(property)")
            .AppendLine("\toverride this._Set(property, value) =")
            .AppendLine("\t\tmatch property.ToString() with")
            .Append(setBuilder.ToString())
            .AppendLine("\t\t| _ -> false")
            .AppendLine()
        |> ignore

    let handleUnion (builder: StringBuilder) (union: FSharpEntity) =
        let name = union.DisplayName
        let cases = union.UnionCases

        let scope = getScope union

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
                    .AppendLine($"\t\t\t\tpropertyDefinition.Add(\"usage\", {Helper.defaultPropertyUsage})")
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
            .AppendLine($"\t\tpropertyDefinition.Add(\"usage\", {Helper.defaultPropertyUsage})")
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

    let writeCSharpClass (outputNamespace: string) (outputFolder: string) (record: FSharpEntity) =

        let outputNamespace =
            match record.DeclaringEntity with
            | None -> outputNamespace
            | Some value -> $"{outputNamespace}.{value.FullName}"

        let recordName = record.DisplayNameCore
        Directory.CreateDirectory outputFolder |> ignore

        let writer =
            File.CreateText $"{outputFolder}/{recordName}.cs"

        writer.Write
            $"""using Godot;
using Godot.Collections;

namespace {outputNamespace};

[Tool]
public partial class {recordName} : Resources.{record.FullName}
{{
	/// <inheritdoc />
	public override Array<Dictionary> _GetPropertyList()
	{{
		return base._GetPropertyList();
	}}

	/// <inheritdoc />
	public override Variant _Get(StringName property)
	{{
		return base._Get(property);
	}}

	/// <inheritdoc />
	public override bool _Set(StringName property, Variant value)
	{{
		return base._Set(property, value);
	}}
}}
"""

        writer.Flush()
        writer.Close()

    let rec writeResourcesOfScope
        (builder: StringBuilder)
        (writeCSharpClass: FSharpEntity -> unit)
        (namespaceOrModule: string)
        (resources: FSharpEntity list)
        =

        let resourcesStringBuilder = StringBuilder()

        for resource in resources do

            if resource.IsFSharpUnion then
                handleUnion resourcesStringBuilder resource
                writeCSharpClass resource

            if resource.IsFSharpRecord then
                handleRecord resourcesStringBuilder resource
                writeCSharpClass resource

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

    let extractResources (contents: FSharpImplementationFileContents) =
        extractRecordsAndUnions contents.Declarations
        |> List.filter
            (fun x ->
                x.Attributes
                |> Seq.exists (fun x -> x.IsAttribute<Helper.ResourceAttribute>()))

    interface IGodotGenerator with
        member _.Generate(context: GeneratorContext) =

            let contents =
                Helper.getImplementationFileContentsFromGeneratorContext context

            let sourceBuilder = StringBuilder()
            let writeResourcesOfScope = writeResourcesOfScope sourceBuilder

            let resources = extractResources contents

            if resources.Length = 0 then
                Output.Ast []
            else
                let resourcesByNamespaceOrModule =
                    resources
                    |> List.groupBy
                        (fun x ->
                            match x.DeclaringEntity with
                            | None -> "global"
                            | Some declaringEntity ->
                                match declaringEntity.Namespace with
                                | Some entityNamespace -> $"{entityNamespace}.{declaringEntity.DisplayName}"
                                | None -> declaringEntity.FullName)

                let config = context.ConfigGetter "godot"

                let outputFolder =
                    config
                    |> Seq.tryPick
                        (fun (n, v) ->
                            if n = "csOutputFolder" then
                                Some(v :?> string)
                            else
                                None)
                    |> Option.defaultValue "./"

                let outputFolder =
                    $"{Path.GetDirectoryName context.InputFilename}/{outputFolder}"

                let outputNamespace =
                    config
                    |> Seq.tryPick
                        (fun (n, v) ->
                            if n = "namespace" then
                                Some(v :?> string)
                            else
                                None)
                    |> Option.defaultValue "UnknownNamespace"

                let writeResourcesOfScope =
                    writeResourcesOfScope (writeCSharpClass outputNamespace outputFolder)

                for namespaceOrModule, resources in resourcesByNamespaceOrModule do
                    writeResourcesOfScope namespaceOrModule resources

                Output.Source
                <| sourceBuilder.ToString().Replace("\t", "    ")

        member this.GetNumberOfGeneratedTypes(context) =
            let contents =
                Helper.getImplementationFileContentsFromGeneratorContext context

            (extractResources contents).Length
