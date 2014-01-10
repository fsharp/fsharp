// This code has a race condition. The important message
// tells us to reset the counter. The unimportant one tells
// us to increment it. The sequence
//  [unimp]GET, [imp]SET, [unimp]SET
// has a race because once we do [imp]SET then we're not allowed
// to do any SETs which depend on a GET prior to that [imp]SET.


extern activity Tick : void {}
extern activity Reset : important void {}

activity Race : int
{ int count = 0;

  on Tick(void m)
  { t = {m} count := count+1
  }

  on Reset(important void *m)
  { t = {m} count := 0
  }

}