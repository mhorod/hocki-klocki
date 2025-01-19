package hocki.klocki.parsing

import hocki.klocki.ast.{Abstra, BlockId, BuiltinSchema, ConnectionDecl, IfaceBinding, Link, SchemaBinding, SchemaExpr, SchemaId, Statement, Toplevel, VertexBinding, VertexId, VertexRef, VertexUse}
import hocki.klocki.entities.Dim

import scala.util.parsing.combinator.RegexParsers

object DflParser extends RegexParsers:
  def program: Parser[Toplevel] = phrase(toplevel)

  private def toplevel: Parser[Toplevel] = rep(statement) ~ opt(link) ^^ {
    case statements ~ link => Toplevel(statements, link)
  }

  // Statement

  private def statement: Parser[Statement] = schemaDef | blockUse

  private def schemaDef: Parser[Statement.SchemaDef] =
    ("def" ~> schemaId) ~ ("=" ~> abstra) ^^ {
      case schemaId ~ abstra => Statement.SchemaDef(SchemaBinding(schemaId), abstra)
    }

  private def blockUse: Parser[Statement.BlockUse] =
    ("use" ~> schemaExpr) ~ externalIfaceBinding ~ opt("as" ~> blockId) ^^ {
      case expr ~ ifaceDef ~ blockId => Statement.BlockUse(expr, ifaceDef, blockId)
    }

  // Iface

  private def internalIfaceBinding: Parser[IfaceBinding.Internal] = bracketed(supplierList ~ ("|" ~> consumerList)) ^^ {
    case suppliers ~ consumers => IfaceBinding.Internal(suppliers, consumers)
  }

  private def externalIfaceBinding: Parser[IfaceBinding.External] = bracketed(consumerList ~ ("|" ~> supplierList)) ^^ {
    case consumers ~ suppliers => IfaceBinding.External(consumers, suppliers)
  }

  private def supplierList: Parser[List[VertexBinding.Supplier]] = vertexBindingList(supplierBinding)

  private def consumerList: Parser[List[VertexBinding.Consumer]] = vertexBindingList(consumerBinding)

  private def vertexBindingList[T <: VertexBinding](single: Parser[T]): Parser[List[T]] = repsep(single, ",")

  private def supplierBinding: Parser[VertexBinding.Supplier] = vertexId ^^ { VertexBinding.Supplier(_) }

  private def consumerBinding: Parser[VertexBinding.Consumer] = vertexId ^^ { VertexBinding.Consumer(_) }

  // SchemaExpr

  private def schemaExpr: Parser[SchemaExpr] = primitive | schemaRef | app

  private def schemaRef: Parser[SchemaExpr.SchemaRef] = schemaId ^^ {
    SchemaExpr.SchemaRef(_)
  }

  private def app: Parser[SchemaExpr.App] = parenthesized(schemaExpr ~ schemaExpr) ^^ {
    case left ~ right => SchemaExpr.App(left, right)
  }

  private def primitive: Parser[SchemaExpr.Primitive] = builtinSchema ^^ {
    SchemaExpr.Primitive(_)
  }

  // Builtin

  private def builtinSchema: Parser[BuiltinSchema] = union | add | remove

  private def union: Parser[BuiltinSchema.Union] = "U" ~> braced(naturalNumber) ^^ {
    BuiltinSchema.Union(_)
  }

  private def add: Parser[BuiltinSchema.Add] = "+" ~> dim ^^ {
    BuiltinSchema.Add(_)
  }

  private def remove: Parser[BuiltinSchema.Remove] = "-" ~> dim ^^ {
    BuiltinSchema.Remove(_)
  }

  // Abstra

  private def abstra: Parser[Abstra] = onIface | onSchema

  private def onIface: Parser[Abstra.OnIface] = internalIfaceBinding ~ rep(statement) ~ link ^^ {
    case iface ~ body ~ link => Abstra.OnIface(iface, body, link)
  }

  private def onSchema: Parser[Abstra.OnSchema] = schemaBinding ~ abstra ^^ {
    case schemaBinding ~ abstra => Abstra.OnSchema(schemaBinding, abstra)
  }
  
  private def schemaBinding: Parser[SchemaBinding] = ("λ" | "\\") ~> schemaId <~ "." ^^ { SchemaBinding(_) }

  // Links

  private def link: Parser[Link] = ("link" ~> repsep(connectionDecl, ",")) ^^ {
    Link(_)
  }

  private def connectionDecl: Parser[ConnectionDecl] = supplierUse ~ (">->" ~> consumerUse) ^^ {
    case u ~ v => ConnectionDecl(u, v)
  }

  private def supplierUse: Parser[VertexUse.Supplier] = vertexRef ^^ { VertexUse.Supplier(_) }

  private def consumerUse: Parser[VertexUse.Consumer] = vertexRef ^^ { VertexUse.Consumer(_) }

  private def vertexRef: Parser[VertexRef] = scoped | plain

  private def plain: Parser[VertexRef.Plain] = vertexId ^^ {
    VertexRef.Plain(_)
  }

  private def scoped: Parser[VertexRef.Scoped] = blockId ~ ("." ~> vertexId) ^^ {
    case blockId ~ vertexId => VertexRef.Scoped(blockId, vertexId)
  }

  // Identifiers

  private def schemaId: Parser[SchemaId] =
    """[a-z_][a-zA-Z0-9_]*""".r ^^ {
      SchemaId(_)
    }

  private def vertexId: Parser[VertexId] =
    """[A-Z][a-zA-Z0-9_]*""".r ^^ {
      VertexId(_)
    }

  private def blockId: Parser[BlockId] =
    """[A-Z][a-zA-Z0-9_]*""".r ^^ {
      BlockId(_)
    }

  private def dim: Parser[Dim] =
    """[a-z][a-zA-Z0-9_]*""".r ^^ {
      Dim(_)
    }

  private def naturalNumber: Parser[Int] =
    """[1-9][0-9]*""".r ^^ {
      _.toInt
    }

  // Common utils

  private def delimited[T](left: String, inner: Parser[T], right: String): Parser[T] = left ~> inner <~ right

  private def parenthesized[T](inner: Parser[T]): Parser[T] = delimited("(", inner, ")")

  private def bracketed[T](inner: Parser[T]): Parser[T] = delimited("[", inner, "]")

  private def braced[T](inner: Parser[T]): Parser[T] = delimited("{", inner, "}")
