// This code has a "release" problem, where the readwriter is not released

extern activity ReadBad : void {}
extern activity WriteBad : void {}
extern activity DoGood : important void {}

activity Race : int
{ int count = 0;
  readwrite rw = new readwrite(rcount=0,wcount=0);

  on ReadBad(void m)
  { lock = {m} begin_read(rw) |
    t = {lock} count := count+1 |
    // "release" error because we didn't release the lock
  }

  on WriteBad(void m)
  { lock = {m} begin_write(rw) |
    t = {lock} count := 0 |
    // "release" error because we didn't release the lock
  }

  on DoGood(important void *a)
  { b = {a} begin_write(rw) |
    c = {b} count := count+1 |
    d = {c} end_write(rw) |
    e = {d} count := count+1 |  // yes I know there's a "race" error here
    f = {e} begin_read(rw) |
    g = {f} count := count+1 |
    h = {g} end_read(rw) |
    signal {h} count
  }

}