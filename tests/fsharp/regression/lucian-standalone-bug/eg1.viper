// This is the "Wanderer" example from the VIPER docs.

class Mode {bool Tracking;}
class Position {int x; int y;}
type Color = Position + important Mode
class Bumper {bool l1; bool l2;}
class Motor {int M1; int M2;}
class Find {bool Find;}
class Track {bool Track;}
class FindTrack {bool Find; bool Track;}
type StateSet = Find + Track + FindTrack
class Success {string s;}
class Failure {string f;}
extern void setm(string id,int m1,int m2);
extern int f(int x,int y);
extern int g(int x,int y);
extern int rand(int min,int max);
extern void print(string);

extern activity BumperHW : Bumper {} 
extern activity ColorHW : Color {}
extern activity Timer {on Delay(int *i):int {} }



activity MyState : FindTrack
{ FindTrack s = new FindTrack(Find=false, Track=false);

  on Set(Find *m):FindTrack
  { n = new FindTrack(Find=m.Find, Track=s.Track) |
    set =  s := n |
    signal set |
    return set
  }
  on Set(Track *m):FindTrack
  { n = new FindTrack(Find=s.Find, Track=m.Track) |
    set =  s := n |
    signal set |
    return set
  }
  on Set(FindTrack *m):FindTrack
  { set =  s := m |
    signal set |
    return set
  }

  on Get(void *m):FindTrack
  { return {m} s
  }
}


activity MotorAct
{ Motor s = new Motor(M1=0, M2=0);
  string id = "dev102";

  on Set(Motor *m):Motor
  { hwset = external setm(id,m.M1,m.M2) |
    iset = s := m |
    return {hwset} iset
  }

  on Get(void *m):Motor
  { return {m} s
  }
}


activity BumperAct : Bumper
{ Bumper s = new Bumper(l1=false, l2=false);
  string id = "dev105";

  on BumperHW(Bumper m)
  { iset = s := m |
    signal iset
  }

  on Get(void *m):Bumper
  { return {m} s
  }
}



activity Wander
{

  on MyState(FindTrack m)
  { if (m.Find) then s = new Motor(M1=7, M2=7) |
    call MotorAct.Set(s)
  }

  on ColorHW(important Mode *ct)
  { if (ct.Tracking) then track=true |
    s = {track} call MyState.Get(void) |
    if (s.Find) then m=new Find(Find=false) |
    call MyState.Set(m)
  }

  on ColorHW(Position ct)
  { s = {ct} call MyState.Get(void) |
    if (!s.Find && s.Track) then istracking=true |
    m = {istracking} new Motor(M1=external f(ct.x,ct.y), M2=external g(ct.x,ct.y)) |
    call MotorAct.Set(m)
  }

  on BumperAct(Bumper b)
  { s2 = {b} call MyState.Set(new FindTrack(Find=false, Track=false)) |
    {s2} call MotorAct.Set(new Motor(M1=-7, M2=-7)) |
    t1 = {s2} 2 |
    t2 = call Timer.Delay(t1) |
    if {t2} (b.l1 && b.l2) then c1 = new Motor(M1=7,M2=-7)
    else if (!b.l1 && b.l2) then c2 = new Motor(M1=-7,M2=7)
    else if (b.l1 && !b.l2) then c3 = new Motor(M1=external rand(-7,7),M2=external rand(-7,7)) |
    dc = first(c1,c2,c3) |
    t3 = {dc} call Timer.Delay(t1) |
    call MotorAct.Set(dc) |
    t4 = {t3} new FindTrack(Find=false, Track=false) |
    call MyState.Set(t4)
  }
}
