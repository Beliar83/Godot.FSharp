module Godot.FSharp.SourceGenerators.GodotGenerator

open System
open Myriad.Core

[<MyriadGenerator("Godot.FSharp")>]
type GodotGenerator() =
    interface IMyriadGenerator with
        member this.Generate(context) =
            let generators: array<IGodotGenerator> = [|ResourceGenerator.Generator(); ObjectGenerator.Generator()|]

            let generators =
                generators
                |> Array.map (fun x -> (x.GetNumberOfGeneratedTypes context, x))
                |> Array.filter (fun (i, _) -> i > 0)

            if generators.Length = 0 then
                Output.Source $"No generators found for {context.InputFilename}"
            else
                if generators.Length > 1
                   || generators |> Array.exists (fun (i, _) -> i > 1) then
                    $"{context.InputFilename} should only contain one godot type to generate"
                    |> Exception
                    |> raise

                let _, generator = generators |> Array.head

                generator.Generate context

        member this.ValidInputExtensions = seq { ".fs" }
