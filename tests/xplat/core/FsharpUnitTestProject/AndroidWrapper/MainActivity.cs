using System.Reflection;
using Android.App;
using Android.OS;
using Xamarin.Android.NUnitLite;

namespace CUnitTests
{
    [Activity (Label = "CSUnitTests", MainLauncher = true)]
    public class MainActivity : TestSuiteActivity
    {
        protected override void OnCreate (Bundle bundle)
        {
            // tests can be inside the main assembly
	    AddTest (typeof(Core_access).Assembly );
            // or in any reference assemblies
            // AddTest (typeof (Your.Library.TestClass).Assembly);

            // Once you called base.OnCreate(), you cannot add more assemblies.
            base.OnCreate (bundle);
        }
    }
}

