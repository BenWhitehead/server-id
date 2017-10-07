package serverid

import scala.annotation.tailrec
import scala.collection.mutable

case class Context[T](existingIds: Set[T]) {
  def record(a: T): Context[T] = Context(existingIds + a)
  def remove(a: T): Context[T] = {
    if (existingIds(a)) Context(existingIds - a)
    else this
  }
}
object Context {
  def empty[T]: Context[T] = new Context[T](Set.empty)
}

trait Id[T] {
  def zero: T
  def increment(a: T): T
  def difference(l: T, r: T): Int
}

trait Cluster[T] {
  def allocateServerId(serverSet: String): T
  def deallocateServerId(serverSet: String, id: T): Unit
}
object Cluster {
  def empty[T](implicit idT: Id[T], ord: Ordering[T]): Cluster[T] = new C[T](mutable.Map[String, ServerSet[T]]())
}

trait ServerSet[T] {
  def alloc(): T
  def dealloc(t: T): Unit
}
object ServerSet {
  def empty[T](implicit idT: Id[T], ord: Ordering[T]): ServerSet[T] = new SS[T](Context.empty)
}

object SeverIdGen {
  def generateId[T](context: Context[T])(implicit idT: Id[T], ord: Ordering[T]): T = {
    val base = (Some(idT.zero): Option[T], None: Option[T])
    val search = foldLeftWhile(context.existingIds.toList.sorted, base) {
      case ((_, None)) => true
      case _ => false
    } {
      case ((Some(id), None), i)  if idT.difference(i, id) > 1   => (Some(i), Some(idT.increment(id)))
      case ((Some(id), None), i)  if idT.difference(i, id) <= 1  => (Some(i), None)
      case ((o, s @ Some(_)), _)                                 => (o, s)
    }
    search match {
      case (_, Some(found)) => found
      case (Some(id), None) => idT.increment(id)
      case (None, None) => idT.increment(idT.zero)
    }
  }

  private[this] def foldLeftWhile[U, V](l: List[V], acc: U)(p: U => Boolean)(f: (U, V) => U): U = {
    @tailrec
    def loop(l: List[V], acc: U): U = {
      if (p(acc)) l match {
        case Nil => acc
        case x :: xs => loop(xs, f(acc, x))
      }
      else acc
    }
    loop(l, acc)
  }
}

private[serverid] final class C[T] private[serverid](
  private[serverid] val data: mutable.Map[String, ServerSet[T]]
)(implicit idT: Id[T], ord: Ordering[T]) extends Cluster[T] {
  def allocateServerId(serverSet: String): T = {
    val ss = data.getOrElseUpdate(serverSet, ServerSet.empty)
    ss.alloc()
  }

  override def deallocateServerId(serverSet: String, id: T): Unit = {
    data.getOrElseUpdate(serverSet, ServerSet.empty).dealloc(id)
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case c: C[T] => data == c.data
    case _ => false
  }

  override def toString: String = {
    s"C(${data.toString})"
  }
}
private[serverid] final class SS[T] private[serverid](
  private[serverid] var context: Context[T]
)(implicit idT: Id[T], ord: Ordering[T]) extends ServerSet[T] {
  override def alloc(): T = {
    val newId = serverid.SeverIdGen.generateId(context)
    context = context.record(newId)
    newId
  }

  override def dealloc(t: T): Unit = {
    context = context.remove(t)
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case ss: SS[T] => context == ss.context
    case _ => false
  }

  override def toString: String = {
    s"SS(${context.toString})"
  }
}
