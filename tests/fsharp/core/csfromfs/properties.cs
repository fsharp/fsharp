
//****************************************************************************
// properties
//****************************************************************************

namespace CSharpProperties
{
	public class Class
	{
		public int prop {
			get {
				return propertyValue;
			}
			set {
				propertyValue = value;
			}
		}

		private int propertyValue = 12;
        
	}

	public struct Struct
	{
		public int prop {
			get {
				return propertyValue;
			}
			set {
				propertyValue = value;
			}
		}

		private int propertyValue;
	}
	public interface Interface
	{
		object objectProperty {
			get; 
		}

		string stringProperty {
			get;
		}
	}

	public interface Interface2
	{
		object objectProperty {
			get; 
		}

		string stringProperty {
			get;
		}
	}

	public abstract class AbstractBase
	{
		public abstract int splitProperty {
			get;
			set;
		}

		public string stringProperty {
			get {
				return string.Copy("foo");
			}
		}
	}

	public abstract class DerivedAbstractClass : AbstractBase
	{
		public override int splitProperty {
			set {
				myValue = value;
			}
		}

		protected int myValue;

		public object objectProperty {
			get {
				return null;
			}
		}

		new public string stringProperty() {
			return null;
		}
	}

	public class DerivedClass : DerivedAbstractClass, Interface {
		public override int splitProperty {
			get {
				return myValue;
			}
		}

		new private object objectProperty {
			get {
				return null;
			}
		}

		new public int stringProperty {
			get {
				return 0;
			}
		}
	}

	public class ExplicitInterfaceClass : Interface, Interface2
	{
		public object objectProperty {
			get {
				return null;
			}
		}

		object Interface2.objectProperty {
			get {
				return null;
			}
		}

		public string stringProperty {
			get {
				return string.Copy("foo");
			}
		}

		string Interface.stringProperty {
			get {
				return null;
			}
		}
	}
	public struct StructImplementingInterface : Interface
	{
		public object objectProperty {
			get {
				return objectPropertyValue;
			}
			set {
				objectPropertyValue = value;
			}
		}

		private object objectPropertyValue;
		public string stringProperty {
			get {
				return stringPropertyValue;
			}
			set {
				stringPropertyValue = value;
			}
		}

		private string stringPropertyValue;
	}

}


// See bug 6389: used to test implicit setting of C# properties through named arguments
namespace Ninject.Planning.Bindings
{
    public interface IRequest
    {
    }
 
    public interface IBinding
    {
        System.Type Service { get; }
        System.Func<IRequest, bool> Condition { get; set; }
    }
 
    public class Binding : IBinding
    {

        public System.Type Service { get; private set; }

        public System.Func<IRequest, bool> Condition { get; set; }
        
    }
 
    public class Request : IRequest
    {
    }
}
 

 
// See bug 6389: used to test implicit setting of C# fields through named arguments
namespace Ninject.Planning.Bindings2
{
    public interface IRequest
    {
    }
 
    public class Binding 
    {

        public System.Type Service;
        public System.Func<IRequest, bool> Condition;
        
    }
 
    public class Request : IRequest
    {
    }
}
 
