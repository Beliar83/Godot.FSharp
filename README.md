# Godot.FSharp
Godot.FSharp contains Myriad based source generators for easier usage of Godot with F#

## Basic setup
- Create a Godot project with C# Solution and Project.
- Add F# Project to solution in same folder as C# project (Different folder needs configuration)
- Add reference to F# Project in C# project 
- Add the Godot.FSharp.SourceGenerators.CSharp package to the C# project with the version matching the Godot.NET.Sdk version
- Add the following to the C# Project:
```msbuild
<Project>
    <!-- ... -->
  <Target Name="DisableDefaultGodotSourceGenerator" BeforeTargets="CoreCompile">
    <ItemGroup>
      <Analyzer Remove="@(Analyzer)" Condition="'%(Filename)' == 'Godot.SourceGenerators'" />
    </ItemGroup>
  </Target>
  
</Project>
```

## Configuration (Myriad.toml)
``` toml
[godot]
namespace = "MyNamespace" #Namespace for generated nodes and resources. Default is 'Generated'
csOutputFolder = "./" #Folder, relative to f# project, to put generated C# files
```

## Examples

### Node

#### MyNode.fs

``` fsharp
namespace MyNodes
open Godot.FSharp.SourceGenerators.ObjectGenerator

[<NodeScript>] // Node to generate
module MyNode =
    [<Node>] // Godot class to inherit from
    type Node = Godot.Node2D
    
    [<State>] // Fields of the node
    type State =
        {
           [<Export>]ExportedVariable : int
           InternalVariable : float
        }
    
    // This is mandatory
    static member Default() = { ExportedVariable = 0; InternalVariable = 0.0 }
    
    // Methods signatures need to be like this Node -> ... additional parameters ... -> State -> State 
    // Node is a reference to the base node
    // State contains the current state
    // The return value should be the the new state
    
    let _Ready (node : Node) (state: State) =
        state
    
    let _Process (node : Node) (delta: double) (state: State) =
        state
        
    let Custom (node : Node) (value : int) (state: State) =        
        { state with ExportedVariable = value }        
```

#### Project file (f#)

``` msbuild
<Compile Include="MyNode.fs" /> <!-- Should already be there -->
<Compile Include="MyNode.generated.fs">
    <MyriadFile>MyNode.fs</MyriadFile>
</Compile>
```

If you build the solution there should now be 2 Files that are generated:

- MyNode.generated.fs 
- MyNodes/MyNode.cs

The script to be attached in godot is MyNode.cs

### Union as Resource

#### MyUnion.fs
```fsharp
module MyResources
open Godot.FSharp.SourceGenerators.ResourceGenerator

[<Resource>]
type MyUnion =
    | A of Int: int
    | B of Int: int * Float: float
    | C of String: string
    | D of int
    | E of int * float
```

#### Project file (f#)

``` msbuild
<Compile Include="MyUnion.fs" /> <!-- Should already be there -->
<Compile Include="MyUnion.generated.fs">
    <MyriadFile>MyUnion.fs</MyriadFile>
</Compile>
```

If you build the solution there should now be 2 Files that are generated:

- MyUnion.generated.fs
- MyResources/MyUnion.cs

If you attach MyUnion.cs to a Resource there should be at minimum a field "Type" with a dropdown that is set to "Unset"
The dropdown should, besides Unset, contain items depending on the Union.
The specific values can be set after changing the Type.

### Record as resource

#### MyRecord.fs
```fsharp
namespace MyResources

open Godot.FSharp.SourceGenerators.ResourceGenerator

[<Resource>]
type MyRecord =
    { a : int
      b : float
      c : string }
    static member Default = { a = 0; b = 0.0; c = "" } // Mandatory
```

#### Project file (f#)

``` msbuild
<Compile Include="MyRecord.fs" /> <!-- Should already be there -->
<Compile Include="MyRecord.generated.fs">
    <MyriadFile>MyRecord.fs</MyriadFile>
</Compile>
```

If you build the solution there should now be 2 Files that are generated:

- MyRecord.generated.fs
- MyResources/MyRecord.cs

- If you attach MyRecord.cs to a Resource it should have all fields of the record. 