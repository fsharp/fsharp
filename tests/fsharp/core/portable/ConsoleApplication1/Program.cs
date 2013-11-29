using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;

namespace PortableTestEntry
{
    // driver program for F# portable tests
    // a number of existing test code files are conditionally refactored into a module such that tests run in the static constructor
    // thus, we just need to access the "aa" property to trigger test code to be run
    class Program
    {
        static int returnCode = 0;

        static int Main(string[] args)
        {
            SetHooks();

            Run("Core_access", () => { var x = Core_access.RUN(); });
            Run("Core_apporder", () => { var x = Core_apporder.RUN(); });
            Run("Core_array", () => { var x = Core_array.RUN(); });
            Run("Core_attributes", () => { var x = Core_attributes.RUN(); });
            Run("Core_comprehensions", () => { var x = Core_comprehensions.RUN(); });
            Run("Core_control", () => { var x = Core_control.RUN(); });
            Run("Core_controlChamenos", () => { var x = Core_controlChamenos.RUN(); });
            Run("Core_controlMailBox", () => { var x = Core_controlMailBox.RUN(); });
            Run("Core_controlStackOverflow", () => { var x = Core_controlStackOverflow.RUN(); });
            Run("Core_csext", () => { var x = Core_csext.RUN(); });
            Run("Core_innerpoly", () => { var x = Core_innerpoly.RUN(); });
            Run("Core_int32", () => { var x = Core_int32.RUN(); });
            Run("Core_lazy", () => { var x = Core_lazy.RUN(); });
            Run("Core_letrec", () => { var x = Core_letrec.RUN(); });
            Run("Core_libtest", () => { var x = Core_libtest.RUN(); });
            Run("Core_lift", () => { var x = Core_lift.RUN(); });
            Run("Core_longnames", () => { var x = Core_longnames.RUN(); });
            Run("Core_map", () => { var x = Core_map.RUN(); });
            Run("Core_measures", () => { var x = Core_measures.RUN(); });
            Run("Core_genericMeasures", () => { var x = Core_genericMeasures.RUN<int>(); });
            Run("Core_nested", () => { var x = Core_nested.RUN(); });
            Run("Core_patterns", () => { var x = Core_patterns.RUN(); });
            Run("Core_printf", () => { var x = Core_printf.RUN(); });
            Run("Core_queriesCustomQueryOps", () => { var x = Core_queriesCustomQueryOps.RUN(); });
            Run("Core_queriesLeafExpressionConvert", () => { var x = Core_queriesLeafExpressionConvert.RUN(); });
            Run("Core_queriesNullableOperators", () => { var x = Core_queriesNullableOperators.RUN(); });
            Run("Core_queriesOverIEnumerable", () => { var x = Core_queriesOverIEnumerable.RUN(); });
            Run("Core_queriesOverIQueryable", () => { var x = Core_queriesOverIQueryable.RUN(); });
            Run("Core_quotes", () => { var x = Core_quotes.RUN(); });
            Run("Core_seq", () => { var x = Core_seq.RUN(); });
            Run("Core_subtype", () => { var x = Core_subtype.RUN(); });
            Run("Core_syntax", () => { var x = Core_syntax.RUN(); });
            Run("Core_tlr", () => { var x = Core_tlr.RUN(); });
            Run("Core_unicode", () => { var x = Core_unicode.RUN(); });

            return returnCode;
        }

        // portable libraries don't have access to a number of APIs, so set hooks to work around this
        static void SetHooks()
        {
            // System.Console
            InitialHook.setWrite((msg) => Console.Write(msg));

            // System.Environment
            InitialHook.setGetEnvironmentVariable((varName) => Environment.GetEnvironmentVariable(varName));
            InitialHook.setMajorVersion(() => Environment.Version.Major);
            InitialHook.setMinorVersion(() => Environment.Version.Minor);

            // System.IO.Directory
            InitialHook.setGetFiles((dir, pattern) => Directory.GetFiles(dir, pattern));
            InitialHook.setGetDirectories((dir) => Directory.GetDirectories(dir));
            InitialHook.setDirectoryExists((dir) => Directory.Exists(dir));

            // System.IO.File
            InitialHook.setWriteAllText((path, contents) => File.WriteAllText(path, contents));
            InitialHook.setWriteAllLines((path, contents) => File.WriteAllLines(path, contents));
            InitialHook.setAppendAllText((path, contents) => File.AppendAllText(path, contents));
            InitialHook.setReadAllLines((path) => File.ReadAllLines(path));

            // System.IO.FileStream
            InitialHook.setGetFileStream((path) => new FileStream(path, FileMode.OpenOrCreate));

            // System.IO.Path
            InitialHook.setGetCurrentDirectory(() => Environment.CurrentDirectory);
            InitialHook.setGetDirectoryName((path) => Path.GetDirectoryName(path));
            InitialHook.setGetFileName((path) => Path.GetFileName(path));

            // System.Threading.Thread
            InitialHook.setSleep((timeout) => Thread.Sleep(timeout));           
        }

        // execute and handle errors for individual test areas
        static void Run(string testArea, Action action)
        {
            Console.WriteLine("Running area {0}", testArea);

            try
            {                
                action();
            }
            catch (Exception e)
            {
                returnCode = -1;
                Console.WriteLine("\tFailure!");
                Console.WriteLine(e.ToString());
            }

            Console.WriteLine();
        }
    }
}