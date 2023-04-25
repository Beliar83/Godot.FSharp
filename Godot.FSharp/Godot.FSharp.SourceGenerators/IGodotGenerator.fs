namespace Godot.FSharp.SourceGenerators

open Myriad.Core

type IGodotGenerator =
    abstract member GetNumberOfGeneratedTypes: GeneratorContext -> int
    abstract member Generate: GeneratorContext -> Output