final case class Id(agent: string, seq: number)

/**
We choose to 'type' LV as a case class instead of a type alias despite slight overhead
to be consistent with the other classes and provide compile-time safety.
**/
final case class LV(value: Long)

sealed trait Op[+T]

final case class Ins[T](
    content: T, 
    pos: Int, 
    id: Id, 
    parents: List[LV]
) extends Op[T]

final case class Del[T](
    pos: Int, 
    id: Id, 
    parents: List[LV]
) extends Op[T]

final case class OpLog[T]