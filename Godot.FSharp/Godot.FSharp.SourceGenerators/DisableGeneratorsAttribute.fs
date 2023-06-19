namespace Godot

open System

[<AttributeUsage(validOn = AttributeTargets.Class, Inherited = false)>]
type DisableGeneratorsAttribute() =
    inherit Attribute()
    new (generators: string[]) = DisableGeneratorsAttribute()