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
    val lv = oplog.ops.length.toLong

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
        pushLocalOp(log, agent, op)
    }
}

def localDelete[T](oplog: OpLog[T], agent: String, pos: Int, delLen: Int) = {
    (0 until delLen).foldLeft(oplog) { case (log, i) =>
        val op = Del(pos + i, Id("tmp", 0), Nil)
        pushLocalOp(log, agent, op)
    }
}

/**
  * Returns index in oplog (LV) where the given id is associated with the op
  */
def idToLV[T](oplog: OpLog[T], id: Id): LV = {
    val idx = oplog.ops.indexWhere(op => op.id.equals(id))
    if (idx < 0) throw Error("Could not find id in oplog")
    return idx.toLong
}

def advanceFrontier(frontier: List[LV], lv: LV, parents: List[LV]): List[LV] = {
    val f = frontier.filter(v => parents.contains(v)) // parents have a out-degree >= 1
    f :+ lv
    return f.sorted
}

def pushRemoteOp[T](oplog: OpLog[T], op: Op[T], parentIds: List[Id]): Unit = {
    val Id(agent, seq) = op.id
    val lastKnownSeq = oplog.version.getOrElse(agent, -1) 
    if (lastKnownSeq >= seq) return // return a no-op if agent already has the op

    val lv = oplog.ops.length.toLong
    val parents = parentIds.map(id => idToLV(oplog, id)).sorted

    oplog.ops :+ op.withMeta(op.id, parents)
    if (seq.!=(lastKnownSeq + 1)) throw Error("Sequence numbers are out of order")
    oplog.copy(
        oplog.ops,
        frontier = advanceFrontier(oplog.frontier, lv, parents),
        version = oplog.version.updated(agent, seq)
    )
}

/**
 * Copies operations from source to dest if missing in dest client
 * Assumes work done in-memory; a proper server-side implementation would compare remote versions
 */
def mergeInto[T](dest: OpLog[T], src: OpLog[T]) = {
    for (op <- src.ops) {
        val parentIds = op.parents.map(lv => src.ops(lv.toInt).id) // map to the source version
        pushRemoteOp(dest, op, parentIds)
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        val oplog0 = createOpLog[Char]() // Scala type inference initializes an Oplog with content of string
        val oplog1 = localInsert(oplog0, "kez", 0, "hello".toList)
    
        println(oplog1.ops)
    }
}
