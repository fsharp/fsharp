namespace iOSFSharpLanguageTests
namespace LanguageUnitTests

open System
open System.Reflection
open MonoTouch.UIKit
open MonoTouch.Foundation
open MonoTouch.NUnit.UI

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()

    let window = new UIWindow (UIScreen.MainScreen.Bounds)
    let runner = new TouchRunner (window)

    // This method is invoked when the application is ready to run.
    override this.FinishedLaunching (app, options) =
        // register every tests included in the main application/assembly
        runner.Add (Assembly.GetExecutingAssembly ())
        window.RootViewController <- new UINavigationController (runner.GetViewController ())
        window.MakeKeyAndVisible ()
        true



module Main =
    [<EntryPoint>]
    let main args =
        UIApplication.Main (args, null, "AppDelegate")
        0

