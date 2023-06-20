namespace Godot.FSharp.SourceGenerators

open System
open System.IO
open System.Text
open FSharp.Compiler.Symbols
open Fantomas.Core
open Myriad.Core
open Godot.FSharp.SourceGenerators.GodotStubs
open FSharp.Compiler.Syntax

type ExtraParamCountCheckMode =
    | ZeroOrMore
    | Exact of int

module ObjectGenerator =
    type NodeScriptAttribute() =
        inherit Attribute()

    type NodeAttribute() =
        inherit Attribute()

    type StateAttribute() =
        inherit Attribute()

    type ExportAttribute() =
        inherit Attribute()

    type NodeOrState =
        | Node of SynComponentInfo
        | State of SynComponentInfo

    type INodeWithState<'Node, 'State> =
        abstract member GetState: unit -> 'State
        abstract member SetState: 'State -> unit
        abstract member GetNode: unit -> 'Node

    type MethodParam =
        { Name: string
          OfTypeName: string
          OfType: Type
          PropertyHint: PropertyHint
          HintText: string
          UsageFlags: PropertyUsageFlags }

    type MethodsToGenerate =
        {

          IsOverride: bool
          MethodParams: List<MethodParam>
          MethodName: string
          MethodFlags: MethodFlags }

    type Field =
        { Name: string
          OfTypeName: string
          OfType: Type
          PropertyHint: GodotStubs.PropertyHint
          HintText: string
          UsageFlags: PropertyUsageFlags }

    type StateToGenerate =
        { Name: string
          ExportedFields: List<Field>
          InnerFields: List<Field> }

    type ToGenerateInfo =
        { InNamespace: string
          ModuleNameToOpen: string
          Extending: string
          Name: string
          StateToGenerate: StateToGenerate
          methods: List<MethodsToGenerate> }

    let private concat = String.concat "\n"
    let private mapAndConcat func = Seq.map func >> concat

    let private generateIfPart isFirst = if isFirst then "if" else "else if"

    let private mapWithFirst a =
        Seq.mapi (fun k v -> v, (k = 0)) >> Seq.map a

    let private generateMethods (methods: List<MethodsToGenerate>) : string =
        let generateInputParams (a: seq<MethodParam>) =
            a
            |> Seq.map (fun x -> x.Name)
            |> String.concat ","

        let generateParamsToSend (a: seq<MethodParam>) =
            a
            |> Seq.map (fun x -> x.Name)
            |> String.concat " "

        let generateAccess isOverride =
            if isOverride then
                "override"
            else
                "member public"

        let generateMethod (method: MethodsToGenerate) =
            $"
    {generateAccess method.IsOverride} this.{method.MethodName} ({generateInputParams method.MethodParams}) =
        let currentState = getState ()
        let newState = {method.MethodName} this {generateParamsToSend method.MethodParams} currentState
        setState newState
    "

        methods |> mapAndConcat generateMethod

    let private generateMethodList (methods: List<MethodsToGenerate>) =
        let generateParams (param: List<MethodParam>) =
            let generateParamPart (param: MethodParam) =
                $"
                        Bridge.PropertyInfo(
                            (LanguagePrimitives.EnumOfValue<_,_> {LanguagePrimitives.EnumToValue param.OfType}L),
                            \"{param.Name}\",
                            (LanguagePrimitives.EnumOfValue<_,_>{LanguagePrimitives.EnumToValue param.PropertyHint}L),
                            \"{param.HintText}\",
                            (LanguagePrimitives.EnumOfValue<_,_>{LanguagePrimitives.EnumToValue param.UsageFlags}),
                            false
                        )
                "

            param |> mapAndConcat generateParamPart

        let generateMethod (method: MethodsToGenerate) =
            $"
        methods.Add(
            MethodInfo(
                \"{method.MethodName}\",
                PropertyInfo(
                    Variant.Type.Nil,
                    \"\",
                    PropertyHint.None,
                    \"\",
                    PropertyUsageFlags.Default,
                    false
                ),
                (LanguagePrimitives.EnumOfValue<_,_> {LanguagePrimitives.EnumToValue method.MethodFlags}),
                ResizeArray (
                    [|
                        {generateParams method.MethodParams}
                    |]
                ),
                ResizeArray ()

            )
        )
            "

        methods |> mapAndConcat generateMethod

    let private generateInvokeGodotClassMethods (methods: List<MethodsToGenerate>) =
        let generateParamsForCall (paramsOfMethod: List<MethodParam>) =
            let generateParamForCall (position: int) (param: MethodParam) =
                $"Godot.NativeInterop.VariantUtils.ConvertTo<{param.OfTypeName}>(&args[{position}])"

            paramsOfMethod
            |> Seq.mapi generateParamForCall
            |> String.concat ","

        let generateInvokeGodotClassMethod (method: MethodsToGenerate, isFirst) =
            $"
        {generateIfPart isFirst} (StringName.op_Equality (\"{method.MethodName}\",&method) && args.Count = {method.MethodParams.Length}) then
            this.{method.MethodName}({generateParamsForCall method.MethodParams})
            true
            "

        methods
        |> mapWithFirst generateInvokeGodotClassMethod
        |> concat

    let private generateHasGodotClassMethod (methods: List<MethodsToGenerate>) =
        let generateHasGodotMethod (method: MethodsToGenerate) =
            $"
            StringName.op_Equality(\"{method.MethodName}\", &method)
            "

        methods
        |> Seq.map generateHasGodotMethod
        |> String.concat "||"

    let private generateSetFields (fields: List<Field>) : string =

        let generateSetField (field: Field, isFirst) : string =
            $"    {generateIfPart isFirst} StringName.op_Equality (\"{field.Name}\",&name) then
            let castedValue = VariantUtils.ConvertTo<{field.OfTypeName}>(&value)
            let currentState = getState()
            let newState = {{
                currentState with
                    {field.Name} = castedValue
            }}
            setState newState
            true
            "

        let fields =
            fields |> mapWithFirst generateSetField |> concat

        fields
        + "
        else
            base.SetGodotClassPropertyValue(&name, &value)
        "

    let private generateGetFields (fields: List<Field>) : string =
        let generateGetField (field: Field, isFirst: bool) =
            $"    {generateIfPart isFirst} StringName.op_Equality (\"{field.Name}\", &name) then
            let state = getState ()
            let fromState = state.{field.Name}
            let casted = VariantUtils.CreateFrom<{field.OfTypeName}>(&fromState)
            value <- casted
            true
            "

        let fields =
            fields |> mapWithFirst generateGetField |> concat

        fields
        + "
        else
            base.GetGodotClassPropertyValue(&name,&value)
        "

    let private generatePropertyList (fields: List<Field>) (isExported: bool) : string =
        let generatePropertyItem (field: Field) : string =
            $"
        properties.Add(
            Bridge.PropertyInfo(
                (LanguagePrimitives.EnumOfValue<_, _> {LanguagePrimitives.EnumToValue field.OfType}L),
                \"{field.Name}\",
                (LanguagePrimitives.EnumOfValue<_, _> {LanguagePrimitives.EnumToValue field.PropertyHint}L),
                \"{field.HintText}\",
                (LanguagePrimitives.EnumOfValue<_, _> {LanguagePrimitives.EnumToValue field.UsageFlags}L),
                {isExported.ToString().ToLower()}
            )
        )
            "

        fields |> mapAndConcat generatePropertyItem

    let private generateGodotSaveObjectData (fields: List<Field>) =
        let generateGodotSingleSaveObjectData (field: Field) =
            $"
        let {field.Name} = state.{field.Name}
        info.AddProperty(\"{field.Name}\",Godot.Variant.From<{field.OfTypeName}>(&{field.Name}))
        "

        fields
        |> mapAndConcat generateGodotSingleSaveObjectData

    let private generateRestoreGodotObjectData (fields: List<Field>) =
        let generateRestoreGodotObjectData (field: Field) =
            $"
        let mutable _value_{field.Name}: Variant = new Variant()
        let mutable newState = getState ()
        if(info.TryGetProperty(\"{field.Name}\",&_value_{field.Name})) then
            newState <- {{
                newState with
                    {field.Name} = (_value_{field.Name}.As<_> ())
            }}
            "

        fields
        |> mapAndConcat generateRestoreGodotObjectData

    let private generateGodotPropertyDefaultValues (fields: List<Field>) =
        let generateSingleGodotPropertyDefaultValue (field: Field) =
            $"
        let __{field.Name}_default_value = defaultState.{field.Name}
        values.Add(\"{field.Name}\",Godot.Variant.From<{field.OfTypeName}>(&__{field.Name}_default_value))
        "

        fields
        |> mapAndConcat generateSingleGodotPropertyDefaultValue

    let generateClass (toGenerate: ToGenerateInfo) : string =
        $"
namespace {toGenerate.InNamespace}
open Godot
open Godot.Bridge
open Godot.NativeInterop
open {toGenerate.ModuleNameToOpen}
open Godot.FSharp.SourceGenerators.ObjectGenerator
type {toGenerate.Name}() =
    inherit {toGenerate.Extending}()
    let mutable state: {toGenerate.StateToGenerate.Name} = {toGenerate.StateToGenerate.Name}.Default ()
    let setState s = state <- s
    let getState () = state
    interface INodeWithState<{toGenerate.Name},{toGenerate.StateToGenerate.Name}> with
        member _.SetState s =  setState s
        member _.GetState () = getState ()
        member this.GetNode () = this
    interface INodeWithState<{toGenerate.Extending},{toGenerate.StateToGenerate.Name}> with
        member _.SetState s =  setState s
        member _.GetState () = getState ()
        member this.GetNode () = this

    {generateMethods toGenerate.methods}
    static member GetGodotMethodList() =
        let methods = ResizeArray()
        {generateMethodList toGenerate.methods}
        methods
    member this._InvokeGodotClassMethod(method : inref<_>, args : NativeVariantPtrArgs, ret : outref<_>) =
        {generateInvokeGodotClassMethods toGenerate.methods}
        else
            base.InvokeGodotClassMethod(&method, args, &ret)

    member this._HasGodotClassMethod(method : inref<_>) =
        {generateHasGodotClassMethod toGenerate.methods} || base.HasGodotClassMethod(&method)

    member this._SetGodotClassPropertyValue(name : inref<_>, value : inref<_>) =
    {generateSetFields toGenerate.StateToGenerate.ExportedFields}
    member this._GetGodotClassPropertyValue(name : inref<_>, value : outref<_>) =
    {generateGetFields toGenerate.StateToGenerate.ExportedFields}
    static member GetGodotPropertyList() =
        let properties = ResizeArray()
        {generatePropertyList toGenerate.StateToGenerate.ExportedFields true}
        {generatePropertyList toGenerate.StateToGenerate.InnerFields false}
        properties

    member this._SaveGodotObjectData(info : inref<_>) =
        base.SaveGodotObjectData info
        {generateGodotSaveObjectData toGenerate.StateToGenerate.ExportedFields}
    member this._RestoreGodotObjectData(info : inref<_>) =
        base.RestoreGodotObjectData info
        {generateRestoreGodotObjectData toGenerate.StateToGenerate.ExportedFields}
#if TOOLS
    static member GetGodotPropertyDefaultValues() =
        let values =
            new System.Collections.Generic.Dictionary<Godot.StringName, Godot.Variant>({toGenerate.StateToGenerate.ExportedFields.Length})
        let defaultState = {toGenerate.StateToGenerate.Name}.Default ()
        {generateGodotPropertyDefaultValues toGenerate.StateToGenerate.ExportedFields}
        values
#endif
    "

    type Generator() =

        let extractTypesNonRecursive (moduleDecls: FSharpImplementationFileDeclaration list) =
            moduleDecls
            |> List.choose
                (fun x ->
                    match x with
                    | FSharpImplementationFileDeclaration.Entity (entity, declarations) -> Some(entity, declarations)
                    | _ -> None)


        let extractNodeType (typ: FSharpEntity) =
            if typ.Attributes
               |> Seq.exists (fun a -> a.IsAttribute<NodeAttribute>()) then
                if typ.IsFSharpAbbreviation then
                    Some(typ.AbbreviatedType)
                else
                    None
            else
                None

        let extractStateType (typ: FSharpEntity) =

            if typ.Attributes
               |> Seq.exists (fun a -> a.IsAttribute<StateAttribute>()) then
                Some(typ)
            else
                None

        let extractNodeDefinition moduleDecls =
            let entities = extractTypesNonRecursive moduleDecls

            let state =
                entities
                |> List.choose (fun (entity, _) -> extractStateType entity)
                |> List.tryHead

            let node =
                entities
                |> List.choose (fun (entity, _) -> extractNodeType entity)
                |> List.tryHead

            match (state, node) with
            | Some x, Some y ->

                (x, y)
            | None, _ ->
                "Missing state in node module"
                |> Exception
                |> raise
            | _, None ->
                "Missing node in node module"
                |> Exception
                |> raise


        let extractNodes (contents: FSharpImplementationFileContents) =
            GeneratorHelper.extractModules contents.Declarations
            |> List.filter
                (fun (x, _) ->
                    x.Attributes
                    |> Seq.exists (fun x -> x.IsAttribute<NodeScriptAttribute>()))

        let isValidNodeMethod
            (method: FSharpMemberOrFunctionOrValue)
            (state: FSharpEntity)
            (node: FSharpEntity)
            extraParamCountCheckMode
            =
            let parameterCountIsValid =
                match extraParamCountCheckMode with
                | ExtraParamCountCheckMode.ZeroOrMore -> method.CurriedParameterGroups.Count >= 2
                | Exact count -> method.CurriedParameterGroups.Count = 2 + count

            if not <| parameterCountIsValid then
                false
            elif method.CurriedParameterGroups
                 |> Seq.head
                 |> Seq.length
                 <> 1 then
                false
            elif method.CurriedParameterGroups
                 |> Seq.last
                 |> Seq.length
                 <> 1 then
                false
            else
                let nodeArgument =
                    (method.CurriedParameterGroups
                     |> Seq.head
                     |> Seq.head)
                        .Type.StripAbbreviations()

                let nodeArgumentTypeDefinition = nodeArgument.TypeDefinition

                let stateArgument =
                    (method.CurriedParameterGroups
                     |> Seq.last
                     |> Seq.head)
                        .Type.StripAbbreviations()

                let stateArgument = stateArgument.TypeDefinition

                nodeArgumentTypeDefinition = node
                && stateArgument = state
                && method.ReturnParameter.Type.TypeDefinition = state

        let isValidReadySignature (method: FSharpMemberOrFunctionOrValue) (state: FSharpEntity) (node: FSharpEntity) =
            isValidNodeMethod method state node (Exact(0))

        let isValidProcessSignature (method: FSharpMemberOrFunctionOrValue) (state: FSharpEntity) (node: FSharpEntity) =
            if not
               <| isValidNodeMethod method state node (Exact(1)) then
                false
            elif method.CurriedParameterGroups[1].Count <> 1 then
                false
            else
                let deltaArgument = (method.CurriedParameterGroups[1][0]).Type.StripAbbreviations()

                let deltaArgumentTypeDefinition = deltaArgument.TypeDefinition
                deltaArgumentTypeDefinition.FullName = typeof<Double>.FullName

        let generateInfo (entity: FSharpEntity) (state: FSharpEntity) (node: FSharpType) outputNamespace =
            let outputNamespace =
                match entity.DeclaringEntity with
                | None -> outputNamespace
                | Some value -> $"{outputNamespace}.{value.FullName}"

            let exportedFields =
                state.FSharpFields
                |> Seq.filter
                    (fun x ->
                        x.PropertyAttributes
                        |> Seq.exists (fun x -> x.IsAttribute<ExportAttribute>()))

            let notExportedFields =
                state.FSharpFields
                |> Seq.filter
                    (fun x ->
                        not
                        <| (x.PropertyAttributes
                            |> Seq.exists (fun x -> x.IsAttribute<ExportAttribute>())))

            let methods =
                entity.MembersFunctionsAndValues
                |> Seq.filter (fun x -> x.IsFunction)
                |> List.ofSeq

            for method in methods do

                let checkCustomMethod () =
                    if not
                       <| isValidNodeMethod method state node.TypeDefinition ExtraParamCountCheckMode.ZeroOrMore then
                        $"{method.DisplayName} has an invalid signature. It should be '{node.TypeDefinition.DisplayName} [...] {state.DisplayName} -> {state.DisplayName}'"
                        |> Exception
                        |> raise

                if method.DisplayName.StartsWith '_' then
                    if method.DisplayName = "_Ready" then
                        if not
                           <| isValidReadySignature method state node.TypeDefinition then
                            $"_Ready should have the signature '{node.TypeDefinition.DisplayName} {state.DisplayName} -> {state.DisplayName}'"
                            |> Exception
                            |> raise
                    elif method.DisplayName = "_Process" then
                        if not
                           <| isValidProcessSignature method state node.TypeDefinition then
                            $"_Process should have the signature '{node.TypeDefinition.DisplayName} double {state.DisplayName} -> {state.DisplayName}'"
                            |> Exception
                            |> raise
                    else
                        checkCustomMethod ()
                else
                    checkCustomMethod ()


            let isOverride (method: FSharpMemberOrFunctionOrValue) =
                let nodeMethods =
                    (GeneratorHelper.extractMethods node)
                    |> List.map (fun x -> x.DisplayName)

                nodeMethods |> List.contains method.DisplayName


            [ {

                Extending = node.TypeDefinition.DisplayName
                Name = entity.DisplayName
                methods =
                    [ for method in methods do
                          { MethodName = method.DisplayName
                            IsOverride = isOverride (method)
                            MethodParams =
                                [
                                  // The first and last parameters are internal parameters for fsharp
                                  for param in
                                      method.CurriedParameterGroups
                                      |> Seq.tail
                                      |> Seq.rev
                                      |> Seq.tail
                                      |> Seq.collect id do
                                      let paramType =
                                          match getTypeNameFromIdent.convertFSharpTypeToVariantType param.Type with
                                          | None -> Type.Nil
                                          | Some value ->
                                              match value with
                                              | None -> Type.Nil
                                              | Some value -> value

                                      { MethodParam.Name = param.DisplayName
                                        OfTypeName = param.Type.TypeDefinition.DisplayName
                                        OfType = paramType
                                        PropertyHint = PropertyHint.None
                                        UsageFlags = PropertyUsageFlags.Default
                                        HintText = "" } ]
                            MethodFlags = MethodFlags.Default } ]

                StateToGenerate =
                    { Name = state.DisplayName
                      ExportedFields =
                          [ for field in exportedFields do
                                { Name = field.DisplayName
                                  OfTypeName = field.FieldType.TypeDefinition.DisplayName
                                  OfType =
                                      match getTypeNameFromIdent.convertFSharpTypeToVariantType field.FieldType with
                                      | None -> Type.Nil
                                      | Some value ->
                                          match value with
                                          | None -> Type.Nil
                                          | Some value -> value
                                  PropertyHint = PropertyHint.None
                                  HintText = ""
                                  UsageFlags =
                                      PropertyUsageFlags.Default
                                      ||| PropertyUsageFlags.ScriptVariable } ]
                      InnerFields =
                          [ for field in notExportedFields do
                                { Name = field.DisplayName
                                  OfTypeName = field.FieldType.TypeDefinition.DisplayName
                                  OfType =
                                      match getTypeNameFromIdent.convertFSharpTypeToVariantType field.FieldType with
                                      | None -> Type.Nil
                                      | Some value ->
                                          match value with
                                          | None -> Type.Nil
                                          | Some value -> value
                                  PropertyHint = PropertyHint.None
                                  HintText = ""
                                  UsageFlags =
                                      PropertyUsageFlags.Default
                                      ||| PropertyUsageFlags.ScriptVariable } ] }
                InNamespace = $"GeneratedNodes.{outputNamespace}"
                ModuleNameToOpen =
                    match entity.DeclaringEntity with
                    | None -> $"{entity.FullName}"
                    | Some value -> $"{value.FullName}.{entity.DisplayName}" } ]

        let writeCSharpClass (outputNamespace: string) (outputFolder: string) (node: FSharpEntity) =

            let outputNamespace =
                match node.DeclaringEntity with
                | None -> outputNamespace
                | Some value -> $"{outputNamespace}.{value.FullName}"

            let nodeName = node.DisplayNameCore
            Directory.CreateDirectory outputFolder |> ignore

            let writer =
                File.CreateText $"{outputFolder}/{nodeName}.cs"

            writer.Write
                $"""using Godot;
using Godot.Bridge;

namespace {outputNamespace};

[DisableGenerators(new[]{{"ScriptSerialization"}})]
public abstract partial class {nodeName} : GeneratedNodes.{outputNamespace}.{nodeName}
{{
	/// <inheritdoc />
	protected override void SaveGodotObjectData(GodotSerializationInfo info)
	{{
		_SaveGodotObjectData(info);
	}}

	/// <inheritdoc />
	protected override void RestoreGodotObjectData(GodotSerializationInfo info)
	{{
		_RestoreGodotObjectData(info);
	}}
}}
"""

            writer.Flush()
            writer.Close()

        interface IGodotGenerator with
            member this.Generate(context: GeneratorContext) =
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

                let writeDebug =
                    config
                    |> Seq.tryPick
                        (fun (n, v) ->
                            if n = "debug" then
                                Some(v :?> bool)
                            else
                                None)
                    |> Option.defaultValue false

                let logBuilder = StringBuilder()

                let contents =
                    GeneratorHelper.getImplementationFileContentsFromGeneratorContext context

                let modulesWithAttribute = extractNodes contents

                logBuilder.AppendLine $"Found {modulesWithAttribute.Length} nodes in {context.InputFilename}"
                |> ignore

                try
                    let entity, state, node =
                        modulesWithAttribute
                        |> List.map
                            (fun (entity, declarations) ->
                                //get every type with the Node attribute
                                let state, typ =
                                    extractNodeDefinition (List.ofSeq <| declarations)

                                (entity, state, typ))
                        |> List.head


                    let toGenerate: List<ToGenerateInfo> =
                        generateInfo entity state node outputNamespace

                    let generatedStr =
                        toGenerate
                        |> Seq.map generateClass
                        |> String.concat "\n\n"

                    if writeDebug then
                        let logger =
                            File.CreateText "./output.debug.myriad.txt"

                        logger.Write(logBuilder.ToString())
                        logger.Write "-------------------"
                        logger.Write generatedStr
                        logger.Flush()
                        logger.Close()

                    writeCSharpClass outputNamespace outputFolder entity
                    Output.Source generatedStr
                with
                | x ->
                    if writeDebug then
                        let logger =
                            File.CreateText "./output.debug.myriad.txt"

                        logger.Write(logBuilder.ToString())
                        logger.Write "-------------------"
                        logger.Write x
                        logger.Flush()
                        logger.Close()

                    raise x

            member this.GetNumberOfGeneratedTypes(context: GeneratorContext) =
                let contents =
                    GeneratorHelper.getImplementationFileContentsFromGeneratorContext context

                extractNodes contents |> List.length
