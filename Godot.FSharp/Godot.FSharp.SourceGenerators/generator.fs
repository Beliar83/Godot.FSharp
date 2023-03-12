namespace MyriadgodotMyriad

open Godot
open Godot.Bridge
open Godot.NativeInterop

module Generator =
    type MethodParam =
        { Name: string
          OfTypeName: string
          OfType: Variant.Type
          PropertyHint: PropertyHint
          HintText: string
          UsageFlags: PropertyUsageFlags }

    type MethodsToGenerate =
        { MethodParams: List<MethodParam>
          MethodName: string
          MethodFlags: Godot.MethodFlags }

    type Field =
        { Name: string
          OfTypeName: string
          OfType: Variant.Type
          PropertyHint: PropertyHint
          HintText: string
          UsageFlags: PropertyUsageFlags }

    type StateToGenerate =
        { Name: string
          ExportedFields: List<Field>
          InnerFields: List<Field>

        }

    type ToGenerateInfo =
        { Extending: string
          Name: string
          StateToGenerate: StateToGenerate
          methods: List<MethodsToGenerate> }

    let private concat = String.concat "\n"
    let private mapAndConcat func = Seq.map func >> concat

    let private generateIfPart isFirst =
        if isFirst then "        if" else "        else if"

    let private mapWithFirst a =
        Seq.mapi (fun k v -> v, (k = 0)) >> Seq.map a

    let private generateMethods (methods: List<MethodsToGenerate>) : string =
        let generateParamsIncomming (a: seq<MethodParam>) =
            a |> Seq.map (fun x -> x.Name) |> String.concat ","

        let generateParamsSend (a: seq<MethodParam>) =
            a |> Seq.map (fun x -> x.Name) |> String.concat " "

        let generateMethod (method: MethodsToGenerate) =
            $"
    publi member this.{method.MethodName} ({generateParamsIncomming method.MethodParams}) =
        let currentState = this.GetState ()
        let newState = {method.MethodName} this {method.MethodParams} {generateParamsSend method.MethodParams} currentState
        this.SetState newState
    "

        methods |> mapAndConcat generateMethod

    let private generateMethodList (methods: List<MethodsToGenerate>) =
        let generateParams (param: List<MethodParam>) =
            let generateParamPart (param: MethodParam) =
                $"
                        Bridge.PropertyInfo(
                            (LanguagePrimitives.EnumOfValue<_,_> {param.OfType}),
                            \"{param.Name}\",
                            (LanguagePrimitives.EnumOfValue<_,_>{param.PropertyHint}),
                            false
                        )
                "

            param |> mapAndConcat generateParamPart

        let generateMethod (method: MethodsToGenerate) =
            $"
        methods.Add(
            Godot.Bridge.MethodInfo(
                \"{method.MethodName}\",
                Godot.Bridge.PropertyInfo(
                    Variant.Type.Nil,
                    \"\",
                    PropertyHint.None,
                    \"\",
                    PropertyUsageFlags.Default,
                    false
                ),
                (LanguagePrimitives.EnumOfValue<_,_> {method.MethodFlags},
                ResizeArray(
                    [|
                        {generateParams method.MethodParams}
                    |]
                )

            )
        )
            "

        methods |> mapAndConcat generateMethod

    let private generateInvokeGodotClassMethods (methods: List<MethodsToGenerate>) =
        let generateParamsForCall (paramsOfMethod: List<MethodParam>) =
            let generateParamForCall (position: int) (param: MethodParam) =
                $"Godot.NativeInterop.VariantUtils.ConvertTo<{param.OfTypeName}>(&args[{position}])"

            paramsOfMethod |> Seq.mapi generateParamForCall |> String.concat ","

        let generateInvokeGodotClassMethod (method: MethodsToGenerate, isFirst) =
            $"
        {generateIfPart isFirst} (StringName.op_Equality ({method.MethodName},&method) && args.Count = {method.MethodParams.Length}) then
            this.{method.MethodName}({generateParamsForCall method.MethodParams})

            "

        methods |> mapWithFirst generateInvokeGodotClassMethod |> concat

    let private generateHasGodotClassMethod (methods: List<MethodsToGenerate>) =
        let generateHasGodotMethod (method: MethodsToGenerate) =
            $"
            StringName.op_Equality(\"{method.MethodName}\", &method)
            "

        methods |> Seq.map generateHasGodotMethod |> String.concat "||"

    let private generateSetFields (fields: List<Field>) : string =

        let generateSetField ((field: Field), (isFirst)) : string =
            $"        {generateIfPart isFirst} StringName.op_Equality (\"{field.Name}\",value) then
            let castedValue = VariantUtils.ConvertTo<{field.OfTypeName}>(&value)
            let currentState = this.GetState()
            let newState = {{
                currentState with
                {field.Name} = castedValue
            }}
            this.SetState newState
            "

        let fields = fields |> mapWithFirst generateSetField |> concat

        fields
        + "
        else
            base.SetGodotClassPropertyValue(&name, &value)
        "

    let private generateGetFields (fields: List<Field>) : string =
        let generateGetField (field: Field, isFirst: bool) =
            $"        {generateIfPart isFirst} StringName.op_Equality (\"{field.Name}\")
            let fromState = this.state.{field.Name}
            let casted = VariantUtils.CreateFrom<{field.OfTypeName}>(&fromState)
            value <- casted
            true
            "

        let fields = fields |> mapWithFirst generateGetField |> concat

        fields
        + "
        else
            base.GetGodotClassPropertyValue(&name,&value)
        "

    let private generatePropertyList (fields: List<Field>) (isExported: bool) : string =
        let generatePropetyItem (field: Field) : string =
            let z =
                PropertyInfo(field.OfType, field.Name, field.PropertyHint, field.HintText, field.UsageFlags, isExported)

            $"
        properties.Add(
            Bridge.PropertyInfo(
                (LanguagePrimitives.EnumOfValue<_, _> {field.OfType}L),
                \"{field.Name}\",
                (LanguagePrimitives.EnumOfValue<_, _> {field.PropertyHint}L),
                \"{field.HintText}\",
                (LanguagePrimitives.EnumOfValue<_, _> {field.UsageFlags}L),
                {isExported}
            )
        )
            "

        fields |> mapAndConcat generatePropetyItem

    let private generateGodotSaveObjectData (fields: List<Field>) =
        let generateGodotSingleSaveObjectData (field: Field) =
            $"
            let {field.Name} = state.{field.Name}
            info.AddProperty(\"{field.Name}\",Godot.Variant.From<{field.OfTypeName}>(&{field.Name}))
            "

        fields |> mapAndConcat generateGodotSingleSaveObjectData

    let private generateRestoreGodotObjectData (fields: List<Field>) =
        let generateRestoreGodotObjectData (field: Field) =
            $"
        let mutable _value_{field.Name}: Variant = new Variant()
        let mutable newState = this.GetState()
        if(info.TryGetProperty(\"{field.Name}\",&_value_{field.Name})) then
            newState <- {{
                newState with
                {field.Name} = (_value_TestSimpleExport.As<_>())
            }}
            "

        fields |> mapAndConcat generateRestoreGodotObjectData

    let private generateGodotPropertyDefaultValues (fields: List<Field>) =
        let generateSingleGodotPropertyDefaultValu (field: Field) =
            $"
        let __{field.Name}_default_value = defaultState.{field.Name}
        values.Add(\"{field.Name}\",Godot.Variant.From<{field.OfTypeName}>(&__{field.Name}_default_value))
        "

        fields |> mapAndConcat generateSingleGodotPropertyDefaultValu

    let generateClass (toGenerate: ToGenerateInfo) : string =
        $"type {toGenerate.Name}() =
    extends {toGenerate.Extending}()
    interface INodeWithState<{toGenerate.Name},{toGenerate.StateToGenerate.Name}> with
        member _.SetState s =  state <- s
        member _.GetState () = state
        member this.GetNode () = this
    let mutable state: {toGenerate.StateToGenerate.Name} = {toGenerate.StateToGenerate.Name}.Default ()
    {generateMethods toGenerate.methods}
    static member GetGodotMethodList() = 
        let methods = ResizeArray()
        {generateMethodList toGenerate.methods}
        methods
    override this.InvokeGodotClassMethod(method, args, ret) =
        {generateInvokeGodotClassMethods toGenerate.methods}
        else
            base.InvokeGodotClassMethod(&method, args, &ret)

    override this.HasGodotClassMethod(method) =
        {generateHasGodotClassMethod toGenerate.methods} || base.HasGodotClassMethod(&method)

    override this.SetGodotClassPropertyValue(name, value) =
    {generateSetFields toGenerate.StateToGenerate.ExportedFields}
    override this.GetGodotClassPropertyValue(name,value) =
    {generateGetFields toGenerate.StateToGenerate.ExportedFields}
    static member GetGodotPropertyList() =
        let properties = ResizeArray()
        {generatePropertyList toGenerate.StateToGenerate.ExportedFields true}
        {generatePropertyList toGenerate.StateToGenerate.InnerFields false}
        properties

    override this.SaveGodotObjectData(info) = 
        base.SaveGodotObjectData info
        {generateGodotSaveObjectData toGenerate.StateToGenerate.ExportedFields}
    override this.RestoreGodotObjectData(info) = 
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
