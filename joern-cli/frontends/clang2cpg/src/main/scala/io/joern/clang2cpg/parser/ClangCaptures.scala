package io.joern.clang2cpg.parser
import java.util.concurrent.{Callable, ExecutionException, Executors, Future, TimeUnit}
import scala.collection.mutable

/** A bounded prefetch window; graph mutation and consumption remain ordered. */
object ClangCaptures {
  def foreach[A,B](inputs: Seq[A], workers: Int)(capture: A => B)(consume: ((A,B)) => Unit): Unit = {
    require(workers > 0, "Clang capture worker count must be positive")
    val executor = Executors.newFixedThreadPool(workers)
    val pending = mutable.Queue.empty[(A,Future[B])]
    val remaining = inputs.iterator
    def enqueue(): Unit = if (remaining.hasNext) {
      val input = remaining.next()
      pending.enqueue(input -> executor.submit(new Callable[B] { override def call(): B = capture(input) }))
    }
    try {
      (0 until math.min(workers,inputs.size)).foreach(_ => enqueue())
      while (pending.nonEmpty) {
        val (input,future) = pending.dequeue()
        val result = try future.get() catch {
          case error: ExecutionException =>
            require(error.getCause != null, "Native capture failed without a cause")
            throw error.getCause
        }
        consume(input -> result)
        enqueue()
      }
    } finally {
      pending.foreach(_._2.cancel(true))
      executor.shutdownNow()
      require(executor.awaitTermination(30,TimeUnit.SECONDS), "Clang capture workers did not terminate; inspect native process diagnostics")
    }
  }
}
