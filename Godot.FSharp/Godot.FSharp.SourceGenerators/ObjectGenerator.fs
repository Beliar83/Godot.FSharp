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
          MethodFlags: MethodFlags
          ReturnParameter : Option<MethodParam>
          }

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
          ExtendingNamespace : string
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
            let builder = StringBuilder();
        
            builder
                .AppendLine($"\t{generateAccess method.IsOverride} this.{method.MethodName} ({generateInputParams method.MethodParams}) =")
                .AppendLine($"\t\tlet currentState = getState ()")
                |> ignore
            match method.ReturnParameter with
            | None ->
                builder
                    .AppendLine($"\t\tlet newState = {method.MethodName} this {generateParamsToSend method.MethodParams} currentState")
                    .AppendLine($"\t\tsetState newState")
                    |> ignore
            | Some _ ->
                builder
                    .AppendLine($"\t\tlet (newState, returnVal) = {method.MethodName} this {generateParamsToSend method.MethodParams} currentState")
                    .AppendLine($"\t\tsetState newState")
                    .AppendLine($"\t\treturnVal")
                    |> ignore
            builder.ToString().Replace("\t", "    ")

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
        let builder = StringBuilder()
        if methods.Length = 0 then
            builder
                .AppendLine("\t\tbase.InvokeGodotClassMethod(&method, args, &ret)")
                |> ignore
        else
            let generateParamsForCall (paramsOfMethod: List<MethodParam>) =
                let generateParamForCall (position: int) (param: MethodParam) =
                    $"Godot.NativeInterop.VariantUtils.ConvertTo<{param.OfTypeName}>(&args[{position}])"

                paramsOfMethod
                |> Seq.mapi generateParamForCall
                |> String.concat ","
            
            let generateInvokeGodotClassMethod (method: MethodsToGenerate) (isFirst) =                
                builder
                    .AppendLine($"\t\t{generateIfPart isFirst} (StringName.op_Equality (\"{method.MethodName}\",&method) && args.Count = {method.MethodParams.Length}) then")
                    |> ignore
                match method.ReturnParameter with
                | None ->
                    builder
                        .AppendLine($"\t\t\tthis.{method.MethodName}({generateParamsForCall method.MethodParams})")
                        |> ignore
                | Some value ->
                    builder
                        .AppendLine($"\t\t\tlet returnVal = this.{method.MethodName}({generateParamsForCall method.MethodParams})")
                        .AppendLine($"\t\t\tret <- Godot.NativeInterop.VariantUtils.CreateFrom<{value.OfTypeName}>(&returnVal)")
                        |> ignore

                builder
                    .AppendLine($"\t\t\ttrue")
                    |> ignore
            generateInvokeGodotClassMethod methods.Head true
            for method in methods.Tail do
                generateInvokeGodotClassMethod method false

            builder
                .AppendLine("\t\telse")
                .AppendLine("\t\t\tbase.InvokeGodotClassMethod(&method, args, &ret)")
                |> ignore


        builder.ToString().Replace("\t", "    ")

    let private generateHasGodotClassMethod (methods: List<MethodsToGenerate>) =
        let builder = StringBuilder()
        for method in methods do
            builder
                .AppendLine($"\t\tStringName.op_Equality(\"{method.MethodName}\", &method) ||")
                |> ignore

        builder
            .AppendLine("\t\tbase.HasGodotClassMethod(&method)")
            |> ignore
        builder.ToString().Replace("\t", "    ")

    let private generateSetFields (fields: List<Field>) : string =

        let builder = StringBuilder();
        if fields.Length = 0 then
            builder
                .AppendLine("\t\tbase.SetGodotClassPropertyValue(&name, &value)")
                |> ignore            
        else        
            let generateSetField (field: Field) (isFirst) =
                builder
                    .AppendLine($"\t\t{generateIfPart isFirst} StringName.op_Equality (\"{field.Name}\",&name) then")
                    .AppendLine($"\t\t\tlet castedValue = VariantUtils.ConvertTo<{field.OfTypeName}>(&value)")
                    .AppendLine($"\t\t\tlet currentState = getState()")
                    .AppendLine($"\t\t\tlet newState = {{")
                    .AppendLine($"\t\t\t\tcurrentState with")
                    .AppendLine($"\t\t\t\t\t{field.Name} = castedValue")
                    .AppendLine($"\t\t\t}}")
                    .AppendLine($"\t\t\tsetState newState")
                    .AppendLine($"\t\t\ttrue")
                    |> ignore

            generateSetField (fields |> List.head) true
            if fields.Length > 1 then
                for field in fields |> List.tail do
                    generateSetField field false        
            
            builder
                .AppendLine("\t\telse")
                .AppendLine("\t\t\tbase.SetGodotClassPropertyValue(&name, &value)")
                |> ignore
        builder.ToString().Replace("\t", "    ")
        
    let private generateGetFields (fields: List<Field>) : string =
        let builder = StringBuilder();
        if fields.Length = 0 then
         builder
                .AppendLine("\t\tbase.GetGodotClassPropertyValue(&name,&value)")
                |> ignore        
        else            
            let generateGetField (field: Field) (isFirst: bool) =
                builder
                    .AppendLine($"\t\t{generateIfPart isFirst} StringName.op_Equality (\"{field.Name}\", &name) then")
                    .AppendLine($"\t\t\tlet state = getState ()")
                    .AppendLine($"\t\t\tlet fromState = state.{field.Name}")
                    .AppendLine($"\t\t\tlet casted = VariantUtils.CreateFrom<{field.OfTypeName}>(&fromState)")
                    .AppendLine($"\t\t\tvalue <- casted")
                    .AppendLine($"\t\t\ttrue")
                    |> ignore                       

            generateGetField (fields |> List.head) true            
            if fields.Length > 1 then
                for field in fields |> List.tail do
                    generateGetField field false
            
            builder
                .AppendLine("\t\telse")
                .AppendLine("\t\t\tbase.GetGodotClassPropertyValue(&name,&value)")
                |> ignore

        builder.ToString().Replace("\t", "    ")

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

    let generateClass (scriptPath: string) (toGenerate: ToGenerateInfo) : string =
        $"
namespace {toGenerate.InNamespace}
open Godot
open Godot.Bridge
open Godot.NativeInterop
open {toGenerate.ModuleNameToOpen}
open Godot.FSharp.SourceGenerators.ObjectGenerator
open {toGenerate.ExtendingNamespace}

[<ScriptPath(\"res://{scriptPath}\")>]
[<Tool>]
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
        let methods = ResizeArray<MethodInfo>()
        {generateMethodList toGenerate.methods}
        methods
    override this.InvokeGodotClassMethod(method : inref<_>, args : NativeVariantPtrArgs, ret : byref<_>) =
{generateInvokeGodotClassMethods toGenerate.methods}

    override this.HasGodotClassMethod(method : inref<_>) =
{generateHasGodotClassMethod toGenerate.methods}

    override this.SetGodotClassPropertyValue(name : inref<_>, value : inref<_>) =
{generateSetFields toGenerate.StateToGenerate.ExportedFields}
    override this.GetGodotClassPropertyValue(name : inref<_>, value) =
{generateGetFields toGenerate.StateToGenerate.ExportedFields}
    static member GetGodotPropertyList() =
        let properties = ResizeArray<PropertyInfo>()
        {generatePropertyList toGenerate.StateToGenerate.ExportedFields true}
        {generatePropertyList toGenerate.StateToGenerate.InnerFields false}
        properties

    override this.SaveGodotObjectData(info : GodotSerializationInfo) =
        base.SaveGodotObjectData info
        {generateGodotSaveObjectData toGenerate.StateToGenerate.ExportedFields}
    override this.RestoreGodotObjectData(info : GodotSerializationInfo) =
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

                let returnParameterType = method.ReturnParameter.Type
                nodeArgumentTypeDefinition = node
                && stateArgument = state
                && (
                    
                    (
                        returnParameterType.IsTupleType
                        && returnParameterType.GenericArguments.Count = 2
                        && returnParameterType.GenericArguments[0].StripAbbreviations().TypeDefinition = state
                        )
                        ||
                        returnParameterType.TypeDefinition = state
                    ) 

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
        
        let isValidGetPropertyListSignature (method: FSharpMemberOrFunctionOrValue) (state: FSharpEntity) (node: FSharpEntity) =
            if not
               <| isValidNodeMethod method state node (Exact(0)) then
                false
            elif not <| method.ReturnParameter.Type.IsTupleType then
                false
            elif method.ReturnParameter.Type.GenericArguments.Count <> 2 then
                false
            elif method.ReturnParameter.Type.GenericArguments[0].StripAbbreviations().TypeDefinition <> state then
                false
            else
                let returnType = method.ReturnParameter.Type.GenericArguments[1].StripAbbreviations()
                if $"{returnType.TypeDefinition.AccessPath}.{returnType.TypeDefinition.DisplayName}"  <> "Godot.Collections.Array" then
                    false
                elif returnType.GenericArguments.Count <> 1 then
                    false
                else
                    let arrayItemItem = returnType.GenericArguments[0].StripAbbreviations()
                    if $"{arrayItemItem.TypeDefinition.AccessPath}.{arrayItemItem.TypeDefinition.DisplayName}" <> "Godot.Collections.Dictionary" then                        
                        false
                    else
                        returnType.GenericArguments[0].GenericArguments.Count = 0               

        let generateInfo (entity: FSharpEntity) (state: FSharpEntity) (node: FSharpType) outputNamespace =
            let outputNamespace = [outputNamespace; GeneratorHelper.getScope entity] |> String.concat "."

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
                        $"{method.DisplayName} has an invalid signature. It should be '{node.TypeDefinition.DisplayName} [...] {state.DisplayName} -> {state.DisplayName}' or '{node.TypeDefinition.DisplayName} [...] {state.DisplayName} -> ({state.DisplayName}, <ReturnType>)'"
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
                    elif method.DisplayName = "_GetPropertyList" then
                        if not
                        <| isValidGetPropertyListSignature method state node.TypeDefinition then
                            $"_GetPropertyList should have the signature '{node.TypeDefinition.DisplayName} {state.DisplayName} -> ({state.DisplayName}, Godot.Collections.Array<Godot.Collections.Dictionary>>"
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
                ExtendingNamespace = GeneratorHelper.getScope node.TypeDefinition 
                Name = entity.DisplayName
                methods =
                    [ for method in methods do
                          let returnParameter =
                              method.ReturnParameter
                          let returnParameter =
                              if returnParameter.Type.IsTupleType then
                                let paramType = returnParameter.Type.GenericArguments[1]
                                let typeName = GeneratorHelper.getTypeString paramType
                                let paramType, propertyHint, hintString =
                                        match getTypeNameFromIdent.convertFSharpTypeToVariantType paramType with
                                        | None -> (Type.Nil, PropertyHint.None, "")
                                        | Some value ->
                                            match value with
                                            | None -> (Type.Nil, PropertyHint.None, "")
                                            | Some value -> value
                                Some({ MethodParam.Name = "Return"
                                       OfTypeName = typeName
                                       OfType = paramType
                                       PropertyHint = propertyHint
                                       UsageFlags = PropertyUsageFlags.Default
                                       HintText = hintString
                                  })
                              else
                                  None
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
                                      |> Seq.rev
                                      |> Seq.collect id do
                                      let paramType = param.Type
                                      let typeName = GeneratorHelper.getTypeString paramType
                   
                                      let paramType, propertyHint, hintString =
                                            match getTypeNameFromIdent.convertFSharpTypeToVariantType paramType with
                                            | None -> (Type.Nil, PropertyHint.None, "")
                                            | Some value ->
                                                match value with
                                                | None -> (Type.Nil, PropertyHint.None, "")
                                                | Some value -> value
                                      { MethodParam.Name = param.DisplayName
                                        OfTypeName = typeName
                                        OfType = paramType
                                        PropertyHint = propertyHint
                                        UsageFlags = PropertyUsageFlags.Default
                                        HintText = hintString } ]
                            MethodFlags = MethodFlags.Default
                            ReturnParameter = returnParameter } ]

                StateToGenerate =
                    { Name = state.DisplayName
                      ExportedFields =
                          [ for field in exportedFields do
                                let typeName = GeneratorHelper.getTypeString (field.FieldType.StripAbbreviations())
                                let typ, propertyHint, hintString =
                                      match getTypeNameFromIdent.convertFSharpTypeToVariantType field.FieldType with
                                      | None -> (Type.Nil, PropertyHint.None, "")
                                      | Some value ->
                                          match value with
                                          | None -> (Type.Nil, PropertyHint.None, "")
                                          | Some value -> value  
                                { Name = field.DisplayName
                                  OfTypeName = typeName                            
                                  OfType = typ                                      
                                  PropertyHint = propertyHint
                                  HintText = hintString
                                  UsageFlags =
                                      PropertyUsageFlags.Default
                                      ||| PropertyUsageFlags.ScriptVariable } ]
                      InnerFields =
                          [ for field in notExportedFields do
                                let typeName = GeneratorHelper.getTypeString (field.FieldType.StripAbbreviations())
                                let typ, propertyHint, hintString =
                                      match getTypeNameFromIdent.convertFSharpTypeToVariantType field.FieldType with
                                      | None -> (Type.Nil, PropertyHint.None, "")
                                      | Some value ->
                                          match value with
                                          | None -> (Type.Nil, PropertyHint.None, "")
                                          | Some value -> value   
                                { Name = field.DisplayName
                                  OfTypeName = typeName
                                  OfType = typ
                                  PropertyHint = propertyHint
                                  HintText = hintString
                                  UsageFlags =
                                      PropertyUsageFlags.Default
                                      ||| PropertyUsageFlags.ScriptVariable } ] }
                InNamespace = $"{outputNamespace}"
                ModuleNameToOpen = $"{GeneratorHelper.getScope entity}.{entity.DisplayName}" } ]

        let writeCSharpClass (sourceFile : string) (scriptPath : string) (godotProjectFolder : string) =           
            // Paths are assumed to be relative to godotProjectFolder
            
            let scriptPath = Path.GetFullPath(scriptPath, godotProjectFolder)
            
            let folder = Path.GetDirectoryName scriptPath
            
            Directory.CreateDirectory folder |> ignore

            let writer =
                File.CreateText scriptPath

            writer.Write
                $"""//This is a generated dummy file for godot to display.
// Please check res://{sourceFile.Replace("\\", "/")} for the actual code"""

            writer.Flush()
            writer.Close()

        interface IGodotGenerator with
            member this.Generate(context: GeneratorContext) =
                let config = context.ConfigGetter "godot"
                let csOutputFolder =
                    config
                    |> Seq.tryPick
                        (fun (n, v) ->
                            if n = "csOutputFolder" then
                                Some(v :?> string)
                            else
                                None)
                    |> Option.defaultValue "Generated"

                let projectFolder =
                    config
                    |> Seq.tryPick
                        (fun (n, v) ->
                            if n = "projectFolder" then
                                Some(v :?> string)
                            else
                                None)
                    |> Option.defaultValue "."

                let projectFolder = Path.GetFullPath projectFolder
                
                let outputNamespace =
                    config
                    |> Seq.tryPick
                        (fun (n, v) ->
                            if n = "namespace" then
                                Some(v :?> string)
                            else
                                None)
                    |> Option.defaultValue "Generated"

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

                    let scriptPath = [csOutputFolder; GeneratorHelper.getScope entity; $"{entity.DisplayName}.cs"] |> String.concat "/"

                    let generatedStr =
                        toGenerate
                        |> Seq.map (generateClass scriptPath)
                        |> String.concat "\n\n"

                    if writeDebug then
                        let logger =
                            File.CreateText "./output.debug.myriad.txt"

                        logger.Write(logBuilder.ToString())
                        logger.Write "-------------------"
                        logger.Write generatedStr
                        logger.Flush()
                        logger.Close()

                    let sourceFile = Path.GetRelativePath(projectFolder, context.InputFilename)
                    
                    
                    writeCSharpClass sourceFile scriptPath projectFolder
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

                    reraise()

            member this.GetNumberOfGeneratedTypes(context: GeneratorContext) =
                let contents =
                    GeneratorHelper.getImplementationFileContentsFromGeneratorContext context

                extractNodes contents |> List.length
