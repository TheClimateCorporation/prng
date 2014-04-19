package com.climate.prng.generators.threefish;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.bouncycastle.crypto.engines.ThreefishEngine;


public class ThreefishState extends Object {

  /** The block cipher */
  private ThreefishEngine engine;

  /** Key for the block cipher */
  private long[] key;

  /** Number of 64-bit words per block, also equal to the key length */
  public int N;

  /** Input block */
  private long[] in;

  /** Output block */
  private long[] out;

  /** Flag to indicate whether the output block is stale */
  private boolean stale;

  public ThreefishState(long[] key) {
    this(key, null);
  }

  public ThreefishState(long[] key, long[] in) {
    if ((key == null) || (key.length == 0)) {
      throw new IllegalArgumentException(
          "key must be non-null and non-empty");
    }
    N = key.length;
    this.key = new long[N];
    System.arraycopy(key, 0, this.key, 0, N);
    this.in = new long[N];
    if (in != null) {
      System.arraycopy(in, 0, this.in, 0, N);
    }
    switch (N) {
      case 4:
        engine = new ThreefishEngine(ThreefishEngine.BLOCKSIZE_256);
        break;
      case 8:
        engine = new ThreefishEngine(ThreefishEngine.BLOCKSIZE_512);
        break;
      case 16:
        engine = new ThreefishEngine(ThreefishEngine.BLOCKSIZE_1024);
        break;
      default:
        throw new IllegalArgumentException(
            "Key/block size of " + N + " words is not supported.");
    }
    engine.init(true, key, null); // forEncryption is true, tweak is null
    out = new long[N];
    stale = true;
  }

  public long[] key() {
    long[] ret = new long[N];
    System.arraycopy(key, 0, ret, 0, N);
    return ret;
  }

  /** Return an element from the input block */
  public long getInput(int i) {
    return in[i];
  }

  /** Process the current input block and return a copy of the input
   * block */
  public long[] getInput() {
    long[] ret = new long[N];
    System.arraycopy(in, 0, ret, 0, N);
    return ret;
  }

  /** Set an element in the input block. The output block is not updated
   * until getOutput() is called. */
  public void setInput(int i, long x) {
    in[i] = x;
    stale = true;
    return;
  }

  /** Set all elements in the input block. */
  public void setInput(long[] array) {
    if ((array == null) || (array.length != N)) {
      throw new IllegalArgumentException(
          "array must be non-null and have the expected length: " + N);
    }
    System.arraycopy(array, 0, in, 0, N);
    stale = true;
    return;
  }

  /** Process the current input block to update the output block and
   * return an element from the output block */
  public long getOutput(int i) {
    if (stale) {
      processBlock(); // update stale flag as a side effect
    }
    return out[i];
  }

  /** Process the current input block to update the output block and
   * return a copy of the output block */
  public long[] getOutput() {
    if (stale) {
      processBlock();
    }
    long[] ret = new long[N];
    System.arraycopy(out, 0, ret, 0, N);
    return ret;
  }

  private void processBlock() {
    engine.processBlock(in, out);
    stale = false;
    return;
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
    ThreefishState rhs = (ThreefishState) obj;
    return new EqualsBuilder()
        .append(this.key(), rhs.key())
        .append(this.getInput(), rhs.getInput())
        .isEquals();
  }

  public int hashCode() {
    return new HashCodeBuilder(151, 131071)
        .append(this.key())
        .append(this.getInput())
        .toHashCode();
  }

  public ThreefishState clone() {
    return new ThreefishState(key(), getInput());
  }

}
