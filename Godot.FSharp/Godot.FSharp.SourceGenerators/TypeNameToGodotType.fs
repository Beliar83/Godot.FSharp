﻿namespace Godot.FSharp.SourceGenerators

open Godot.FSharp.SourceGenerators.GodotStubs
open Microsoft.FSharp.Core

module getTypeNameFromIdent =
    open System
    open FSharp.Compiler.Symbols

    type VariantType = GodotStubs.Type

    type public TypeMatcher(matchAgainst: GodotStubs.Type, propertyHint: PropertyHint, hintString : string) =
        let matchesAgainst = ResizeArray()

        member this.Add<'a>() =
            matchesAgainst.Add typeof<'a>.FullName
            this

        member _.Matches comp =
            matchesAgainst |> Seq.exists (fun x -> x = comp)

        member this.TryGetType comp =
            match this.Matches comp with
            | true -> Some (matchAgainst, propertyHint, hintString)
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
            TypeMatcher(VariantType.Bool, PropertyHint.None, "").Add<bool>()
            
            TypeMatcher(VariantType.Int, PropertyHint.None, "")
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
            TypeMatcher(VariantType.Float, PropertyHint.None, "") |> add<float> () |> add<double> ()
            TypeMatcher(VariantType.String, PropertyHint.None, "") |> add<string> ()
        ]
        |> tryAndMatch fullName
        |> Option.orElseWith (fun () ->
            if typeToConvert.IsEnum then
                (VariantType.Int, PropertyHint.Enum, String.Join (",", typeToConvert.FSharpFields |> Seq.map (fun x -> x.Name)) ) |> Some |> Some
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
                | "GodotSharp.Godot.Vector2" -> Some (VariantType.Vector2, PropertyHint.None, "")
                | "GodotSharp.Godot.Vector2I" -> Some (VariantType.Vector2I, PropertyHint.None, "")
                | "GodotSharp.Godot.Rect2" -> Some (VariantType.Rect2, PropertyHint.None, "")
                | "GodotSharp.Godot.Rect2I" -> Some (VariantType.Rect2I, PropertyHint.None, "")
                | "GodotSharp.Godot.Transform2D" -> Some (VariantType.Transform2D, PropertyHint.None, "")
                | "GodotSharp.Godot.Vector3" -> Some (VariantType.Vector3, PropertyHint.None, "")
                | "GodotSharp.Godot.Vector3I" -> Some (VariantType.Vector3I, PropertyHint.None, "")
                | "GodotSharp.Godot.Basis" -> Some (VariantType.Basis, PropertyHint.None, "")
                | "GodotSharp.Godot.Quaternion" -> Some (VariantType.Quaternion, PropertyHint.None, "")
                | "GodotSharp.Godot.Transform3D" -> Some (VariantType.Transform3D, PropertyHint.None, "")
                | "GodotSharp.Godot.Vector4" -> Some (VariantType.Vector4, PropertyHint.None, "")
                | "GodotSharp.Godot.Vector4I" -> Some (VariantType.Vector4I, PropertyHint.None, "")
                | "GodotSharp.Godot.Projection" -> Some (VariantType.Projection, PropertyHint.None, "")
                | "GodotSharp.Godot.Aabb" -> Some (VariantType.Aabb, PropertyHint.None, "")
                | "GodotSharp.Godot.Color" -> Some (VariantType.Color, PropertyHint.None, "")
                | "GodotSharp.Godot.Plane" -> Some (VariantType.Plane, PropertyHint.None, "")
                | "GodotSharp.Godot.Rid" -> Some (VariantType.Rid, PropertyHint.None, "")
                | "GodotSharp.Godot.Callable" -> Some (VariantType.Callable, PropertyHint.None, "")
                | "GodotSharp.Godot.Signal" -> Some (VariantType.Signal, PropertyHint.None, "")
                | "GodotSharp.Godot.Variant" -> Some (VariantType.Nil, PropertyHint.None, "")
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
                        TypeMatcher(VariantType.PackedByteArray, PropertyHint.ArrayType, "").Add<byte>()
                        TypeMatcher(VariantType.PackedInt32Array, PropertyHint.ArrayType, "").Add<int32>()
                        TypeMatcher(VariantType.PackedInt64Array, PropertyHint.ArrayType, "").Add<int64>()
                        TypeMatcher(VariantType.PackedFloat32Array, PropertyHint.ArrayType, "").Add<float32>()
                        TypeMatcher(VariantType.PackedFloat64Array, PropertyHint.ArrayType, "").Add<float>()
                        TypeMatcher(VariantType.PackedStringArray, PropertyHint.ArrayType, "").Add<string>()
                    ]
                    |> tryAndMatch generic.FullName
                    |> Option.orElseWith (fun () ->
                        //if generic.Assembly.QualifiedName = "GodotSharp" && generic.
                        None)
            else
                let isGodotArray (entity : FSharpEntity) =
                    entity.FullName = "Godot.Collections.Array"
                
                let rec isGodotObject (entity : FSharpEntity) =
                    if entity.FullName = "Godot.Resource" then (true, PropertyHint.ResourceType)
                    elif entity.FullName = "Godot.Node" then (true, PropertyHint.NodeType)
                    elif entity.FullName = "Godot.GodotObject" then (true, PropertyHint.None)
                    else
                        match entity.BaseType with
                        | None -> (false, PropertyHint.None)
                        | Some value -> isGodotObject (value.StripAbbreviations()).TypeDefinition
                
                if isGodotArray typeToConvert then
                    Some(VariantType.Array, PropertyHint.None, "") |> Some
                else
                    let isObject, propertyHint = isGodotObject typeToConvert
                    
                    let hintString =
                        match propertyHint with
                        | PropertyHint.ResourceType -> typeToConvert.DisplayName
                        | _ -> ""
                    
                    if isObject then
                        Some (VariantType.Object, propertyHint, hintString) |> Some
                    else                
                        None
            )

    let convertFSharpTypeToVariantType (typeToConvert: FSharpType) =
        convertManagedTypeToVariantType (typeToConvert.StripAbbreviations()).TypeDefinition
