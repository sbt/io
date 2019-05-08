package sbt.internal.nio

import java.nio.file.Path

import sbt.io._
import sbt.nio.file.Glob.FullFileGlob
import sbt.nio.file.RelativeGlob.{ Matcher, NoPath }
import sbt.nio.file.{ AnyPath, Glob, RecursiveGlob, RelativeGlob }

private[sbt] object Globs {
  private[sbt] def apply(base: Path, recursive: Boolean, fileFilter: FileFilter): Glob = {
    fileFilterToRelativeGlob(fileFilter) match {
      case Some(relativeGlob) =>
        Glob(base, if (recursive) RecursiveGlob / relativeGlob else relativeGlob)
      case None =>
        FullFileGlob(base, recursive, fileFilter)
    }
  }
  private[sbt] def fileFilterToRelativeGlob(fileFilter: FileFilter): Option[RelativeGlob] =
    fileFilter match {
      case nameFilter: NameFilter => nameFilterToRelativeGlob(nameFilter)
      case af: AndFilter =>
        if (af.left == NothingFilter || af.right == NothingFilter) Some(NoPath)
        else if (af.left == AllPassFilter) fileFilterToRelativeGlob(af.right)
        else if (af.right == AllPassFilter) fileFilterToRelativeGlob(af.left)
        else {
          (fileFilterToRelativeGlob(af.left), fileFilterToRelativeGlob(af.right)) match {
            case (no @ Some(NoPath), _)                      => no
            case (_, no @ Some(NoPath))                      => no
            case (Some(AnyPath), right)                      => right
            case (left, Some(AnyPath))                       => left
            case (Some(left: Matcher), Some(right: Matcher)) => Some(Matcher.and(left, right))
            case _                                           => None
          }
        }
      case nf: NotFilter =>
        nf.fileFilter match {
          case NothingFilter => Some(AnyPath)
          case AllPassFilter => Some(NoPath)
          case f =>
            fileFilterToRelativeGlob(f).collect {
              case m: Matcher => Matcher.not(m)
            }
        }
      case of: OrFilter =>
        if (of.left == AllPassFilter || of.right == AllPassFilter) Some(AnyPath)
        else if (of.left == NothingFilter) fileFilterToRelativeGlob(of.right)
        else if (of.right == NothingFilter) fileFilterToRelativeGlob(of.left)
        else {
          (fileFilterToRelativeGlob(of.left), fileFilterToRelativeGlob(of.right)) match {
            case (Some(NoPath), right)                       => right
            case (left, Some(NoPath))                        => left
            case (any @ Some(AnyPath), _)                    => any
            case (_, any @ Some(AnyPath))                    => any
            case (Some(left: Matcher), Some(right: Matcher)) => Some(Matcher.and(left, right))
            case _                                           => None
          }
        }
      case AllPassFilter | HiddenFileFilter => Some(AnyPath)
      case NothingFilter                    => Some(NoPath)
      case _                                => None
    }
  private[sbt] def nameFilterToRelativeGlob(nameFilter: NameFilter): Option[Matcher] =
    nameFilter match {
      case AllPassFilter => Some(AnyPath)
      case af: AndNameFilter =>
        if (af.left == NothingFilter || af.right == NothingFilter) Some(NoPath)
        else if (af.left == AllPassFilter) nameFilterToRelativeGlob(af.right)
        else if (af.right == AllPassFilter) nameFilterToRelativeGlob(af.left)
        else
          (nameFilterToRelativeGlob(af.left), nameFilterToRelativeGlob(af.right)) match {
            case (Some(AnyPath), right) => right
            case (left, Some(AnyPath))  => left
            case (no @ Some(NoPath), _) => no
            case (_, no @ Some(NoPath)) => no
            case (Some(l), Some(r))     => Some(Matcher.and(l, r))
            case _                      => None
          }
      case ef: ExactFilter => Some(Matcher(ef.matchName))
      case ef: ExtensionFilter =>
        ef.extensions match {
          case extensions if extensions.length == 1 => Some(Matcher(s"*.${extensions.head}"))
          case extensions                           => Some(Matcher(s"*.${extensions.mkString("{", ",", "}")}"))
        }
      case NothingFilter => Some(NoPath)
      case nf: NotNameFilter =>
        if (nf.fileFilter == NothingFilter) Some(AnyPath)
        else nameFilterToRelativeGlob(nf.fileFilter).map(Matcher.not)
      case of: OrNameFilter =>
        if (of.left == AllPassFilter || of.right == AllPassFilter) Some(AnyPath)
        else if (of.left == NothingFilter) nameFilterToRelativeGlob(of.right)
        else if (of.right == NothingFilter) nameFilterToRelativeGlob(of.left)
        else
          (nameFilterToRelativeGlob(of.left), nameFilterToRelativeGlob(of.right)) match {
            case (any @ Some(AnyPath), _) => any
            case (_, any @ Some(AnyPath)) => any
            case (Some(NoPath), right)    => right
            case (left, Some(NoPath))     => left
            case (Some(l), Some(r))       => Some(Matcher.or(l, r))
            case _                        => None
          }
      case pf: PatternFilter => Some(Matcher(s"${pf.parts.mkString("*")}"))
      case pf: PrefixFilter  => Some(Matcher(s"${pf.prefix}*"))
      case sf: SimpleFilter  => Some(Matcher(sf.acceptFunction))
      case sf: SuffixFilter  => Some(Matcher(s"*${sf.suffix}"))
    }
}
