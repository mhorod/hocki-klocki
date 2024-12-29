package hocki.klocki.parsing

import hocki.klocki.ast.{
  Abstra,
  BlockId,
  BuiltinSchema,
  ConnectionDecl,
  IfaceDef,
  Link,
  Toplevel,
  SchemaExpr,
  SchemaId,
  Statement,
  VertexDef,
  VertexId,
  VertexRef,
}
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
      case schemaId ~ abstra => Statement.SchemaDef(schemaId, abstra)
    }

  private def blockUse: Parser[Statement.BlockUse] =
    ("use" ~> schemaExpr) ~ ifaceDef ~ opt("as" ~> blockId) ^^ {
      case expr ~ ifaceDef ~ blockId => Statement.BlockUse(expr, ifaceDef, blockId)
    }

  // Iface

  private def ifaceDef: Parser[IfaceDef] = bracketed(vertexDefList ~ ("|" ~> vertexDefList)) ^^ {
    case in ~ out => IfaceDef(in, out)
  }

  private def vertexDefList: Parser[List[VertexDef]] = repsep(vertexDef, ",")

  private def vertexDef: Parser[VertexDef] = vertexId ^^ {
    VertexDef(_)
  }

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

  private def onIface: Parser[Abstra.OnIface] = ifaceDef ~ rep(statement) ~ link ^^ {
    case iface ~ body ~ link => Abstra.OnIface(iface, body, link)
  }

  private def onSchema: Parser[Abstra.OnSchema] = (("\\" | "λ") ~> schemaId <~ ".") ~ abstra ^^ {
    case schemaBinding ~ abstra => Abstra.OnSchema(schemaBinding, abstra)
  }

  // Links

  private def link: Parser[Link] = ("link" ~> repsep(connectionDecl, ",")) ^^ {
    Link(_)
  }

  private def connectionDecl: Parser[ConnectionDecl] = vertexRef ~ (">->" ~> vertexRef) ^^ {
    case u ~ v => ConnectionDecl(u, v)
  }

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
