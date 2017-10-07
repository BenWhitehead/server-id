package serverid

import serverid.SeverIdGen._
import org.scalatest.FreeSpec

import scala.collection.mutable


class SeverIdGenTest extends FreeSpec {

  private[this] implicit val idT: Id[Int] = new Id[Int] {
    override val zero: Int = 0
    override def increment(a: Int): Int = a + 1
    override def difference(l: Int, r: Int): Int = l - r
  }

  "Should be able to generate an id for a server" - {
    "empty context" in {
      assert(generateId(Context(Set[Int]())) === 1)
    }
    "context with single value of 1" in {
      assert(generateId(Context(Set(1))) === 2)
    }
    "context with single value of 2" in {
      assert(generateId(Context(Set(2))) === 1)
    }
    "context with span 1, 3" in {
      assert(generateId(Context(Set(1, 3))) === 2)
    }
    "context with span 1, 2, 3, 5" in {
      assert(generateId(Context(Set(1, 2, 3, 5))) === 4)
    }
    "context with two spans" in {
      assert(generateId(Context(Set(1, 3, 5))) === 2)
    }
  }

  "Cluster" - {
    "handling each server set should have it's own id set" in {
      val c = Cluster.empty
      assert(c.allocateServerId("web") === 1)
      assert(c.allocateServerId("chat") === 1)
      assert(c.allocateServerId("email") === 1)
    }

    "should properly deallocate a server id" in {
      val c = new C(mutable.Map[String, ServerSet[Int]]("web" -> new SS(Context(Set(1, 2, 3)))))

      c.deallocateServerId("web", 3)

      val expected = new C(mutable.Map[String, ServerSet[Int]]("web" -> new SS(Context(Set(1, 2)))))
      assert(expected === c)
    }

    "complex simulation" in {
      val c = Cluster.empty

      val ops =
        (1 to 3).map(_ => "web")   ++ Vector(("web", 3)) ++
        (1 to 6).map(_ => "chat")  ++ Vector(1, 2, 5).map(("chat", _)) ++
        (1 to 9).map(_ => "email") ++ (1 to 8).map(("email", _))

      ops.foreach {
        case s: String => val _ = c.allocateServerId(s)
        case (s: String, id: Int) => c.deallocateServerId(s, id)
        case _ => fail("err during setup")
      }

      val expected = new C(mutable.Map[String, ServerSet[Int]](
        "web" -> new SS(Context(Set(1, 2))),
        "chat" -> new SS(Context(Set(3, 4, 6))),
        "email" -> new SS(Context(Set(9)))
      ))
      c match {
        case cc: C[Int] => assert(expected === cc)
        case _ => fail("unable to extract test data for assertions")
      }

    }

  }

}
