namespace Microsoft.FSharp.Build
open System
open System.IO
open System.Text
open Microsoft.Build.Tasks
open Microsoft.Build.Utilities

type CreateFSharpManifestResourceName public () =
    inherit CreateCSharpManifestResourceName()
    
    override this.CreateManifestName 
                ((fileName:string), 
                    (linkFileName:string),
                    (rootNamespace:string), (* may be null *)  
                    (dependentUponFileName:string), (* may be null *) 
                    (binaryStream:System.IO.Stream) (* may be null *)) : string = 

        // The Visual CSharp and XBuild CSharp toolchains transform resource names like this:
        //     SubDir\abc.resx --> SubDir.abc.resources
        //     SubDir\abc.txt --> SubDir.abc.txt
        //
        // For resx resources, both the Visual FSharp and XBuild FSHarp toolchains do the right thing, i.e.
        //     SubDir\abc.resx --> SubDir.abc.resources
        //
        // However for non-resx resources, for some reason Visual FSharp does _not_ add the directory name to the resource name.
        // It is very unclear where the directory name gets dropped in the Visual FSharp implementation 
        // - is it in Microsoft.Common.targets, Microfost.FSharp.targets or how the base type CreateCSharpManifestResourceName 
        // is created and used - who knows, the code is not easy to understand despite it doing something very simple. That's
        // the nature of MSBuild/XBuild....
        //
        // Anyway, dropping the directory name seems like a mistake. But we attempt to replicate the behaviour here
        // for consistency with Visual FSharp. This may not be the right place to do this and this many not be consistent
        // when cultures are used - that case has not been tested.

        let fileName = if fileName.EndsWith(".resources", StringComparison.OrdinalIgnoreCase) then fileName else Path.GetFileName(fileName)
        let linkFileName = if linkFileName.EndsWith(".resources", StringComparison.OrdinalIgnoreCase) then linkFileName else Path.GetFileName(linkFileName)

        let embeddedFileName = 
            match linkFileName with
            |   null -> fileName
            |   _ -> linkFileName

        // since we do not support resources dependent on a form, we always pass null for a binary stream 
        // rootNamespace is always empty - we do not support it
        let cSharpResult = base.CreateManifestName(fileName, linkFileName,  "", dependentUponFileName, null)
        //printfn "(fileName,linkFileName,embeddedFileName,rootNamespace) = '%A'" (fileName, linkFileName, embeddedFileName, rootNamespace)
        //printfn "cSharpResult = '%s'" cSharpResult
        // Workaround that makes us keep .resources extension on both 3.5 and 3.5SP1
        // 3.5 stripped ".resources", 3.5 SP1 does not. We should do 3.5SP1  thing
        let extensionToWorkaround = ".resources"
        let fSharpResult = 
          if embeddedFileName.EndsWith(extensionToWorkaround, StringComparison.OrdinalIgnoreCase) 
                && not (cSharpResult.EndsWith(extensionToWorkaround, StringComparison.OrdinalIgnoreCase)) then
            cSharpResult + extensionToWorkaround
          else
            cSharpResult

        //printfn "fSharpResult = '%s'" fSharpResult
        fSharpResult
            
        
    override this.IsSourceFile (filename:string) = 
        let extension = Path.GetExtension(filename)
        (String.Equals(extension, ".fs", StringComparison.OrdinalIgnoreCase) ||
            String.Equals(extension, ".ml", StringComparison.OrdinalIgnoreCase))