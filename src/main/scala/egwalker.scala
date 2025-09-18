final case class Id(agent: String, seq: Int)

type LV = Long

sealed trait Op[+T] {
    def pos: Int;
    def id: Id;
    def parents: List[LV]

    def withMeta(id: Id, parents: List[LV]): Op[T]
}

final case class Ins[T](
    content: T, 
    pos: Int, 
    id: Id, 
    parents: List[LV]
) extends Op[T] {
    def withMeta(newId: Id, newParents: List[LV]): Ins[T] = 
        copy(id = newId, parents = newParents)
}

final case class Del(
    pos: Int, 
    id: Id, 
    parents: List[LV]
) extends Op[Nothing] {
    def withMeta(newId: Id, newParents: List[LV]): Del = 
        copy(id = newId, parents = newParents)
}

/**
Last known sequence number for every agent
**/
type RemoteVersion = Map[String, Int]

/**
The version of an EG represents all items of out-degree 0 (and all it's parents)
The frontier by this definition is a list of all 'leaf' nodes' local versions, which encompass all paths in the graph.
**/
final case class OpLog[T](
    ops: List[Op[T]], 
    frontier: List[LV],

    version: RemoteVersion,
)

/**
Return a default empty operation log object
**/
def createOpLog[T](): OpLog[T] = {
    return OpLog(
        List.empty[Op[T]], 
        List.empty[LV], 
        Map.empty[String, Int]
    )
}

def pushLocalOp[T](oplog: OpLog[T], agent: String, op: Op[T]) = {
    val seq = oplog.version.getOrElse(agent, -1) + 1
    val lv = oplog.ops.length

    val stamped = op.withMeta(Id(agent, seq), oplog.frontier)

    oplog.copy(
        ops = oplog.ops :+ stamped,
        frontier = List(lv),
        version = oplog.version.updated(agent, seq) // update the latest known seq no. for that peer
    )
}

def localInsert[T](oplog: OpLog[T], agent: String, pos: Int, content: List[T]) = {
    content.zipWithIndex.foldLeft(oplog) { case (log, (c, i)) =>
        val op = Ins(c, pos + i, Id("tmp", 0), Nil)
        pushLocalOp(oplog, agent, op)
    }
}

def localDelete[T](oplog: OpLog[T], agent: String, pos: Int, delLen: Int) = {
    (0 until delLen).foldLeft(oplog) { case (log, i) =>
        val op = Del(pos + i, Id("tmp", 0), Nil)
        pushLocalOp(oplog, agent, op)
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        val oplog0 = createOpLog[String]() // Scala type inference initializes an Oplog with content of string
        val oplog1 = localInsert(oplog0, "kez", 0, List("hello"))
    
        println(oplog1.ops)
    }
}
