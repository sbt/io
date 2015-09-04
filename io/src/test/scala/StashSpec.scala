/* sbt -- Simple Build Tool
 * Copyright 2010 Mark Harrah */

package sbt.io

import org.scalatest._

import IO._
import java.io.File
import Function.tupled

class StashSpec extends FlatSpec with Matchers {
  "stash" should "handle empty files" in {
    stash(Set()) {}
    assert(true)
  }

  it should "move files during execution" in {
    WithFiles(TestFiles: _*)(checkMove)
  }

  it should "restore files on exceptions but not errors" in {
    WithFiles(TestFiles: _*)(checkRestore)
  }

  def checkRestore(seq: Seq[File]): Unit = {
    allCorrect(seq)

    stash0(seq, throw new TestRuntimeException) shouldBe false
    allCorrect(seq)

    stash0(seq, throw new TestException) shouldBe false
    allCorrect(seq)

    stash0(seq, throw new TestError) shouldBe false
    noneExist(seq)
  }
  def checkMove(seq: Seq[File]): Unit = {
    allCorrect(seq)
    assert(stash0(seq, ()))
    noneExist(seq)
  }
  def stash0(seq: Seq[File], post: => Unit): Boolean =
    try {
      stash(Set() ++ seq) {
        noneExist(seq)
        post
      }
      true
    } catch {
      case _: TestError | _: TestException | _: TestRuntimeException => false
    }

  def allCorrect(s: Seq[File]): Unit = (s.toList zip TestFiles.toList).foreach((correct _).tupled)
  def correct(check: File, ref: (File, String)): Unit =
    {
      assert(check.exists)
      read(check) shouldBe ref._2
    }
  def noneExist(s: Seq[File]): Unit = s.forall(!_.exists) shouldBe true

  lazy val TestFiles =
    Seq(
      "a/b/c" -> "content1",
      "a/b/e" -> "content1",
      "c" -> "",
      "e/g" -> "asdf",
      "a/g/c" -> "other") map {
        case (f, c) => (new File(f), c)
      }
}
class TestError extends Error
class TestRuntimeException extends RuntimeException
class TestException extends Exception
