namespace Godot.FSharp.SourceGenerators

module getTypeNameFromIdent =
    open System
    open FSharp.Compiler.Symbols

    type VariantType = GodotStubs.Type

    type public TypeMatcher(matchAgainst: GodotStubs.Type) =
        let matchesAgainst = ResizeArray()

        member this.Add<'a>() =
            matchesAgainst.Add typeof<'a>.FullName
            this

        member _.Matches comp =
            matchesAgainst |> Seq.exists (fun x -> x = comp)

        member this.TryGetType comp =
            match this.Matches comp with
            | true -> Some matchAgainst
            | false -> None

    let private add<'a> () (on: TypeMatcher) = on.Add<'a>()

    let tryAndMatch fullName =
        Seq.choose (fun (matcher: TypeMatcher) -> fullName |> matcher.TryGetType |> Option.map Some)
        >> Seq.tryHead
    //the below code is pretty much just a direct translation of the existing code
    //for this from the source generators
    //
    //Some lines have changed to accomidate the fact that we don't have accesss to the GodotSharp .DLL
    //
    //Original code: https://github.com/godotengine/godot/blob/21d080ead4ff09a0796574c920a76e66e8b8a3e4/modules/mono/editor/Godot.NET.Sdk/Godot.SourceGenerators/MarshalUtils.cs#LL86C46-L86C46

    let convertManagedTypeToVariantType (typeToConvert: FSharpEntity) =

        let fullName = typeToConvert.FullName

        [
            TypeMatcher(VariantType.Bool).Add<bool>()
            VariantType.Int
            |> TypeMatcher
            |> add<char> ()
            |> add<sbyte> ()
            |> add<int16> ()
            |> add<int32> ()
            |> add<int64> ()
            |> add<int8> ()
            |> add<uint> ()
            |> add<uint16> ()
            |> add<uint32> ()
            |> add<uint64> ()
            |> add<uint16> ()
            VariantType.Float |> TypeMatcher |> add<float> () |> add<double> ()
            VariantType.String |> TypeMatcher |> add<string> ()
        ]
        |> tryAndMatch fullName
        |> Option.orElseWith (fun () ->
            if typeToConvert.IsEnum then
                VariantType.Int |> Some |> Some
            else
                None)
        |> Option.orElseWith (fun () ->
            if
                typeToConvert.IsValueType
                && typeToConvert.Assembly.SimpleName.Contains("GodotSharp")
                && typeToConvert.Namespace
                   |> Option.map (fun name -> name.Contains("Godot"))
                   |> Option.defaultValue false
            then
                match typeToConvert.FullName with
                | "GodotSharp.Godot.Vector2" -> Some VariantType.Vector2
                | "GodotSharp.Godot.Vector2I" -> Some VariantType.Vector2I
                | "GodotSharp.Godot.Rect2" -> Some VariantType.Rect2
                | "GodotSharp.Godot.Rect2I" -> Some VariantType.Rect2I
                | "GodotSharp.Godot.Transform2D" -> Some VariantType.Transform2D
                | "GodotSharp.Godot.Vector3" -> Some VariantType.Vector3
                | "GodotSharp.Godot.Vector3I" -> Some VariantType.Vector3I
                | "GodotSharp.Godot.Basis" -> Some VariantType.Basis
                | "GodotSharp.Godot.Quaternion" -> Some VariantType.Quaternion
                | "GodotSharp.Godot.Transform3D" -> Some VariantType.Transform3D
                | "GodotSharp.Godot.Vector4" -> Some VariantType.Vector4
                | "GodotSharp.Godot.Vector4I" -> Some VariantType.Vector4I
                | "GodotSharp.Godot.Projection" -> Some VariantType.Projection
                | "GodotSharp.Godot.Aabb" -> Some VariantType.Aabb
                | "GodotSharp.Godot.Color" -> Some VariantType.Color
                | "GodotSharp.Godot.Plane" -> Some VariantType.Plane
                | "GodotSharp.Godot.Rid" -> Some VariantType.Rid
                | "GodotSharp.Godot.Callable" -> Some VariantType.Callable
                | "GodotSharp.Godot.Signal" -> Some VariantType.Signal
                | "GodotSharp.Godot.Variant" -> Some VariantType.Nil
                | _ -> None
                |> Some
            else
                None)
        |> Option.orElseWith (fun () ->
            if typeToConvert.IsArrayType then
                if typeToConvert.ArrayRank <> 1 then
                    Some None
                else
                    let generic =
                        typeToConvert.GenericParameters |> Seq.head

                    [
                        TypeMatcher(VariantType.PackedByteArray).Add<byte>()
                        TypeMatcher(VariantType.PackedInt32Array).Add<int32>()
                        TypeMatcher(VariantType.PackedInt64Array).Add<int64>()
                        TypeMatcher(VariantType.PackedFloat32Array).Add<float32>()
                        TypeMatcher(VariantType.PackedFloat64Array).Add<float>()
                        TypeMatcher(VariantType.PackedStringArray).Add<string>()
                    ]
                    |> tryAndMatch generic.FullName
                    |> Option.orElseWith (fun () ->
                        //if generic.Assembly.QualifiedName = "GodotSharp" && generic.
                        None)
            else
                None)

    let convertFSharpTypeToVariantType (typeToConvert: FSharpType) =
        if typeToConvert.IsAbbreviation then
            convertManagedTypeToVariantType typeToConvert.AbbreviatedType.TypeDefinition
        else
            convertManagedTypeToVariantType typeToConvert.TypeDefinition
