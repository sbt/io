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

package sbt.io.parallel

/** The zip format itself — what the writers, their probes and the suites reading them back agree with. */
private[io] object ZipConstants {

  // ── signatures ───────────────────────────────────────────────────────

  final val LocSig = 0x04034b50L
  final val ExtSig = 0x08074b50L
  final val CenSig = 0x02014b50L
  final val EndSig = 0x06054b50L
  final val Zip64EndSig = 0x06064b50L
  final val Zip64LocatorSig = 0x07064b50L

  // ── version needed ───────────────────────────────────────────────────

  final val VersionStored = 10
  final val VersionDeflated = 20
  final val VersionZip64 = 45

  // ── general-purpose flags ────────────────────────────────────────────

  final val Utf8Flag = 0x800
  final val DescriptorFlag = 0x8

  // ── zip64 ────────────────────────────────────────────────────────────

  final val Zip64Magic = 0xffffffffL
  final val Zip64ExtraId = 0x0001
  final val Zip64FieldBytes = 8

  /** A local header's zip64 field: the two sizes it moves, and the two together with their header. */
  final val Zip64LocalDataBytes = 16
  final val Zip64LocalFieldBytes = 20

  final val Zip64EndTrailingBytes = 44L
  final val OneDisk = 1L
  final val MaxEntriesWithoutZip64 = 0xffff

  // ── field sizes and limits ───────────────────────────────────────────

  final val MaxFieldBytes = 0xffff
  final val CentralHeaderBytes = 46
  final val ExtraHeaderBytes = 4

  /** The end record with no comment after it, which is the whole of an archive holding no entries. */
  final val EndRecordBytes = 22

  // ── local header field offsets ───────────────────────────────────────

  final val LocFlagOffset = 6
  final val LocTimeOffset = 10
  final val LocNameLengthOffset = 26
  final val LocExtraLengthOffset = 28
  final val LocBytes = 30
  final val CenNameLengthOffset = 28
  final val CenExtraLengthOffset = 30
  final val CenMadeByOffset = 4
  final val CenAttributesOffset = 38

  // ── sentinel ─────────────────────────────────────────────────────────

  final val Unset = -1L

  // ── reading fields back ──────────────────────────────────────────────

  /** A 16 bit little-endian field, which is how every zip length, flag and id is stored. */
  def u16(b: Array[Byte], at: Int): Int =
    (b(at) & 0xff) | ((b(at + 1) & 0xff) << 8)

  /** The 32 bit form, unsigned, which is how a time, an offset and a set of attributes is stored. */
  def u32(b: Array[Byte], at: Int): Long =
    u16(b, at).toLong | (u16(b, at + 2).toLong << 16)
}
