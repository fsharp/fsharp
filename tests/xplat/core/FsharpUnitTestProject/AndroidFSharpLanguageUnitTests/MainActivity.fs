
namespace AndroidFSharpLanguageUnitTests

open System
open System.Reflection
open System.Collections.Generic
open System.Linq
open System.Text

open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open Xamarin.Android.NUnitLite
open NUnit.Framework

[<Activity (Label = "AndroidFSharpLanguageUnitTests", MainLauncher = true)>]
type MainActivity() =
  inherit TestSuiteActivity()

    override x.OnCreate(bundle) =
        x.AddTest(Assembly.GetExecutingAssembly())
        base.OnCreate (bundle)

[<TestFixture; Description("Blah blah")>]
type ``My tests``() =
    [<Test>]
    member x.``Subtype Tests``() =
        let results = true
        Assert.IsTrue(results)


