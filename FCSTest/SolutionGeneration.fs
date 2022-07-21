namespace FCSTest

open System
open System.Diagnostics
open System.IO
open Microsoft.Build.Evaluation

module SolutionGenerator =
    let generate (dir : string) (leafCount : int) =
        Directory.CreateDirectory dir |> ignore
        let makePath suffix = Path.Combine(dir, suffix)
        let sourceFilePath = makePath "library.fs"
        
        let makeEmptyProject () =
            let p = Project(NewProjectFileOptions.None)
            p.Xml.Sdk <- "Microsoft.NET.Sdk"
            p.SetProperty("TargetFramework", "net6.0") |> ignore
            p.SetProperty("DisableImplicitFSharpCoreReference", "true") |> ignore
            p
        
        let writeProject (name : string) =
            if Directory.Exists (makePath name) then Directory.Delete (makePath name, true)
            Directory.CreateDirectory (makePath name) |> ignore
            let dst = makePath (Path.Combine(name, "library.fs"))
            File.Copy(sourceFilePath, dst)
            File.WriteAllText(dst, File.ReadAllText(dst).Replace("module FCSTest.library", $"module {name}"))
            let p = makeEmptyProject()
            p.AddItem("Compile", "library.fs") |> ignore
            p.Save(makePath (Path.Combine(name, $"{name}.fsproj")))
        
        let name = "root"
        // Directory.Delete(makePath name, true)
        let slnName = "solution"
        let slnPath = makePath $"{slnName}.sln"
        if File.Exists slnPath then File.Delete slnPath
        Directory.CreateDirectory (makePath name) |> ignore
        let dst = makePath (Path.Combine(name, "library.fs"))
        File.Copy(sourceFilePath, dst)
        let nrs = [1..leafCount]
        let x =
            nrs
            |> List.map (fun i -> $"open leaf_{i}")
            |> fun l -> String.Join(Environment.NewLine, l)
        let y =
            nrs
            |> List.map (fun i -> $"let x{i} = leaf_{i}.Z1.Y1.X1.sum1 2 3")
            |> fun l -> String.Join(Environment.NewLine, l)
        let x = $"{x}{Environment.NewLine}{y}"
            
        File.WriteAllText(dst, File.ReadAllText(dst).Replace("module FCSTest.library", $"module FCSTest.library{Environment.NewLine}{x}"))
        let root = makeEmptyProject()
        root.AddItem("Compile", "library.fs") |> ignore
        
        let runX (args : string) = 
            let psi = ProcessStartInfo("dotnet", args)
            psi.WorkingDirectory <- dir
            let p = Process.Start(psi)
            p.WaitForExit()
            printfn $"Running 'dotnet {args}' in '{dir}' returned with exit code {p.ExitCode}"
        
        runX "new sln -n solution"
        
        for i in nrs do
            let name = $"leaf_{i}"
            writeProject name
            root.AddItem("ProjectReference", $"{name}/{name}.fsproj") |> ignore
            runX $"sln add {name}"
        
        root.Save(makePath (Path.Combine(name, $"{name}.fsproj")))
        runX $"sln add {name}"
