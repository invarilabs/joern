package io.joern.clang2cpg.parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.collection.mutable
class ClangCapturesTests extends AnyWordSpec with Matchers {
  "Bounded native captures" should {
    "overlap captures while consuming deterministically in source order" in {
      val second = new CountDownLatch(1)
      val consumed = mutable.ArrayBuffer.empty[Int]
      ClangCaptures.foreach(Seq(1,2,3),2) { value =>
        if (value == 1) require(second.await(3,TimeUnit.SECONDS), "second capture did not overlap")
        if (value == 2) second.countDown()
        value
      } { case (_, value) => consumed += value }
      consumed.toSeq shouldBe Seq(1,2,3)
    }
    "propagate capture rejection without consuming partial results" in {
      val consumed = mutable.ArrayBuffer.empty[Int]
      intercept[IllegalArgumentException] {
        ClangCaptures.foreach(Seq(1,2,3),2) { value =>
          if (value == 1) throw new IllegalArgumentException("capture rejected")
          value
        } { case (_,value) => consumed += value }
      }.getMessage shouldBe "capture rejected"
      consumed shouldBe empty
    }
    "reject invalid parallelism" in {
      intercept[IllegalArgumentException] {ClangCaptures.foreach(Seq(1),0)(identity[Int])(_ => ())}
    }
  }
}
