@0xec169a28a3ba7b46;

const ping :Float32 = 10.0;
using Id = UInt64;

struct Target {
  union {
    global @0 :Void;
    personal @1 :UInt64;
  }
}

struct Package {
  from @0 :Target;
  to @1 :Target;
  timestamp @2 :Int32;
  content: union {
    message @3 :Text;
    pack @4 :Data;
  }
}

struct Chat {
  id @0 :Id;
  packages @1 :List(Package);
}
