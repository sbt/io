/*
 * sbt IO
 * Copyright Scala Center, Lightbend, and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * SPDX-License-Identifier: Apache-2.0
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package sbt.internal.io;

import com.sun.jna.Structure;
import java.util.List;
import java.util.Arrays;

public class MacJNA {

  public static class Attrlist extends Structure {
    public short bitmapcount;
    public short reserved;
    public int commonattr;
    public int volattr;
    public int dirattr;
    public int fileattr;
    public int forkattr;
    @Override
    public List<String> getFieldOrder() {
      return Arrays.asList(new String[]{
        "bitmapcount", "reserved", "commonattr",
        "volattr", "dirattr", "fileattr", "forkattr"
      });
    };
  }

  public static class Timespec extends Structure {
    public long tv_sec;
    public long tv_nsec;
    @Override
    public List<String> getFieldOrder() {
      return Arrays.asList(new String[]{
        "tv_sec", "tv_nsec"
      });
    };
  }

  public static class TimeBuf extends Structure {
    public int size;
    public long tv_sec;
    public long tv_nsec;
    @Override
    public List<String> getFieldOrder() {
      return Arrays.asList(new String[]{
        "size", "tv_sec", "tv_nsec"
      });
    };
    public TimeBuf() {
      setAlignType(Structure.ALIGN_NONE);
    }
  }
}
