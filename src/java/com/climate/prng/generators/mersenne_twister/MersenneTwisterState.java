package com.climate.prng.generators.mersenne_twister;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;


public class MersenneTwisterState extends Object {

  /** Size of the storage array */
  private final int N;

  /** Array for storing bits. */
  private long[] storage;

  /** Current index in the storage array. */
  private int index;

  public MersenneTwisterState(int size) {
    if (!(size > 0)) {
      throw new IllegalArgumentException("size must be greater than zero");
    }
    N = size;
    storage = new long[N];
    index = 0;
  }

  public MersenneTwisterState(long[] array) {
    if ((array == null) || (array.length == 0)) {
      throw new IllegalArgumentException("array must be non-null and non-empty");
    }
    N = array.length;
    storage = new long[N];
    System.arraycopy(array, 0, storage, 0, N);
    index = 0;
  }

  public int size() {
    return N;
  }

  public long get(int i) {
    return storage[(index + i) % N];
  }

  public void set(int i, long x) {
    storage[(index + i) % N] = x;
    return;
  }

  public void set(long[] array) {
    if ((array == null) || (array.length != N)) {
      throw new IllegalArgumentException(
          "array must be non-null and expected length: " + N);
    }
    System.arraycopy(array, 0, storage, 0, N);
    index = 0;
  }

  public void addToThis(MersenneTwisterState state) {
    if (state.size() != N) {
      throw new IllegalArgumentException(
          "state does not have the expected size: " + N);
    }
    for (int i = 0; i < N; i++) {
      storage[(i + index) % N] ^= state.get(i);
    }
    return;
  }

  public void rotateLeft() {
    index = (index + 1) % N;
    return;
  }

  public void rotateRight() {
    index = (index - 1 + N) % N;
    return;
  }

  public long[] toArray() {
    long[] out = new long[N];
    for (int i = 0; i < N; i++) {
      out[i] = storage[((i + index) % N)];
    }
    return out;
  }

  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (obj == this) {
      return true;
    }
    if (obj.getClass() != getClass()) {
      return false;
    }
    MersenneTwisterState rhs = (MersenneTwisterState) obj;
    return new EqualsBuilder().append(this.toArray(), rhs.toArray()).isEquals();
  }

  public int hashCode() {
    return new HashCodeBuilder(151, 131071).append(this.toArray()).toHashCode();
  }

  public MersenneTwisterState clone() {
    return new MersenneTwisterState(toArray());
  }

}
