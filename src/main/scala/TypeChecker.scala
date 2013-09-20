package irxground

sealed trait TypeStructure
case class Func(ret: TypeStructure, args: List[TypeStructure])
case class Ident(name: String)

import scala.collection.immutable.Map
case class NameTable(
  objTable: Map[String, NameTable] = Map(),
  typeTable: Map[String, NameTable] = Map(),
  funcTable: Map[String, Map[TypeStructure, (List[String], Block)]] = Map()
) {
}

object TypeChecker {
  @inline private[this] def TODO = throw new NotImplementedError()

  def apply(decs: DeclarationBlock, table: NameTable = NameTable()): NameTable = {
    var t = table;
    decs.block.foreach {
      case ClassDec(name, body) =>;
      case ObjDec(name, body) =>;
      case FuncDec(name, args, ret, block) =>


      case OpDec(op, left, right, ret, block) =>;
    }
    return t;
  }

  def getType(table: NameTable, name: String): (NameTable, NameTable) = {
    table.typeTable.get(name) match {
      case Some(nt: NameTable) => (table, nt)
      case None =>
        val t = NameTable()
        val nt = table.copy(typeTable = table.typeTable + (name -> t))
        (nt, t)
    }
  }
}

