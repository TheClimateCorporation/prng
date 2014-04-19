package com.climate.prng.generators.mersenne_twister;

import clojure.lang.RT;


public class BitShift {

  static long bitOpsCast(Object x){
    Class xc = x.getClass();
    if (xc == Long.class || xc == Integer.class || xc == Short.class || xc == Byte.class) {
      return RT.longCast(x);
    }
    throw new IllegalArgumentException("bit operation not supported for: " + xc);
  }

  static public int unsignedShiftRightInt(int x, int n) {
    return x >>> n;
  }

  static public long unsignedShiftRight(Object x, Object y) {
    return unsignedShiftRight(bitOpsCast(x), bitOpsCast(y));
  }

  static public long unsignedShiftRight(Object x, long y) {
    return unsignedShiftRight(bitOpsCast(x), y);
  }

  static public long unsignedShiftRight(long x, Object y) {
    return unsignedShiftRight(x, bitOpsCast(y));
  }

  static public long unsignedShiftRight(long x, long n) {
    return x >>> n;
  }

}
