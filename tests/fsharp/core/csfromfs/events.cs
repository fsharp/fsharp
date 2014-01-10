
using System; using System.Diagnostics;

//****************************************************************************
// Events
//****************************************************************************

namespace Events
{
    public delegate void Handler(int i);
    
    public class C : I
    {
        public event Handler HandlerEvent;
        private Handler myHandler;
        public event Handler HandlerPropEvent {
            add { myHandler += value; }
            remove { myHandler -= value;}
        }
        
        static public event Handler HandlerEventStatic;
        static private Handler myHandlerStatic;
        static public event Handler HandlerPropEventStatic {
            add { myHandlerStatic += value; }
            remove { myHandlerStatic -= value;}
        }
        public event Handler InterfaceHandler;
        public void FireHandlerEvent(int x) { HandlerEvent(x); }
        public void FireHandlerPropEvent(int x) { myHandler(x); }
        public static void FireHandlerEventStatic(int x) { HandlerEventStatic(x); }
        public static void FireHandlerPropEventStatic(int x) { myHandlerStatic(x); }
    }

    public interface I
    {
        event Handler InterfaceHandler;
    }
}

