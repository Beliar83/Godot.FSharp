namespace Godot.FSharp.SourceGenerators

open GodotStubs

module Variant =

    type DefaultValue =
        | Defined of string
        | Object

    let getGodotDefault (variantType: Type) =
        match variantType with
        | Type.Nil -> DefaultValue.Defined "null"
        | Type.Int -> DefaultValue.Defined "0"
        | Type.Float -> DefaultValue.Defined "0.0"
        | Type.Bool -> DefaultValue.Defined "false"
        | Type.String -> DefaultValue.Defined "String.Empty"
        | Type.Dictionary -> DefaultValue.Defined "new Godot.Dictionary()"
        | Type.Array -> DefaultValue.Defined "new Godot.Array()"
        | Type.Object -> DefaultValue.Object
        | _ -> DefaultValue.Defined $"new {variantType.ToString()}()"
