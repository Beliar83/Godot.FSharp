namespace Godot.FSharp.SourceGenerators

module Generator =
    type MethodParam =
        {
            Name: string
            OfTypeName: string
            OfType: GodotStubs.Type
            PropertyHint: GodotStubs.PropertyHint
            HintText: string
            UsageFlags: GodotStubs.PropertyUsageFlags
        }

    type MethodsToGenerate =
        {

            IsOverride: bool
            MethodParams: List<MethodParam>
            MethodName: string
            MethodFlags: GodotStubs.MethodFlags
        }

    type Field =
        {
            Name: string
            OfTypeName: string
            OfType: GodotStubs.Type
            PropertyHint: GodotStubs.PropertyHint
            HintText: string
            UsageFlags: GodotStubs.PropertyUsageFlags
        }

    type StateToGenerate =
        {
            Name: string
            ExportedFields: List<Field>
            InnerFields: List<Field>
        }

    type ToGenerateInfo =
        {
            InNamespace: string
            ModuleNameToOpen: string
            Extending: string
            Name: string
            StateToGenerate: StateToGenerate
            methods: List<MethodsToGenerate>
        }

    let private concat = String.concat "\n"
    let private mapAndConcat func = Seq.map func >> concat

    let private generateIfPart isFirst = if isFirst then "if" else "else if"

    let private mapWithFirst a =
        Seq.mapi (fun k v -> v, (k = 0)) >> Seq.map a

    let private generateMethods (methods: List<MethodsToGenerate>) : string =
        let generateParamsIncomming (a: seq<MethodParam>) =
            a |> Seq.map (fun x -> x.Name) |> String.concat ","

        let generateParamsSend (a: seq<MethodParam>) =
            a |> Seq.map (fun x -> x.Name) |> String.concat " "

        let generateAccess isOverride =
            if isOverride then "override" else "member public"

        let generateMethod (method: MethodsToGenerate) =
            $"
    {generateAccess method.IsOverride} this.{method.MethodName} ({generateParamsIncomming method.MethodParams}) =
        let currentState = getState ()
        let newState = {method.MethodName} this {generateParamsSend method.MethodParams} currentState
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

            paramsOfMethod |> Seq.mapi generateParamForCall |> String.concat ","

        let generateInvokeGodotClassMethod (method: MethodsToGenerate, isFirst) =
            $"
        {generateIfPart isFirst} (StringName.op_Equality (\"{method.MethodName}\",&method) && args.Count = {method.MethodParams.Length}) then
            this.{method.MethodName}({generateParamsForCall method.MethodParams})
            true
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

        let fields = fields |> mapWithFirst generateSetField |> concat

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

        let fields = fields |> mapWithFirst generateGetField |> concat

        fields
        + "
        else
            base.GetGodotClassPropertyValue(&name,&value)
        "

    let private generatePropertyList (fields: List<Field>) (isExported: bool) : string =
        let generatePropetyItem (field: Field) : string =
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
        let mutable newState = getState ()
        if(info.TryGetProperty(\"{field.Name}\",&_value_{field.Name})) then
            newState <- {{
                newState with
                    {field.Name} = (_value_{field.Name}.As<_> ())
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
        $"
namespace {toGenerate.InNamespace}
open Godot
open Godot.Bridge
open Godot.NativeInterop
open {toGenerate.ModuleNameToOpen}
open Godot.FSharp.SourceGenerators.Helper
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
