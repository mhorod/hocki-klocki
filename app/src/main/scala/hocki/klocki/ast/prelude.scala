package hocki.klocki.ast

import hocki.klocki.ast.dim.DimParams
import hocki.klocki.ast.schema.{IfaceBinding, SchemaBinding, SchemaId}
import hocki.klocki.ast.vertex.{VertexBinding, VertexId, VertexRef}

def addPrelude(toplevel: Toplevel): Toplevel =
  Toplevel(implicitDefs ++ toplevel.statements, toplevel.link)

private lazy val splitDef = Statement.SchemaDef(
  SchemaBinding(SchemaId("<>")),
  DimParams.empty,
  Abstra.OnIface(
    IfaceBinding.Internal(
      List(VertexBinding.Supplier(VertexId("X"))),
      List(VertexBinding.Consumer(VertexId("Y0")), VertexBinding.Consumer(VertexId("Y1"))),
    ),
    List(),
    Link(
      List(
        ConnectionDecl(
          VertexUse.Supplier(VertexRef.Plain(VertexId("X"))),
          VertexUse.Consumer(VertexRef.Plain(VertexId("Y0"))),
        ),
        ConnectionDecl(
          VertexUse.Supplier(VertexRef.Plain(VertexId("X"))),
          VertexUse.Consumer(VertexRef.Plain(VertexId("Y1"))),
        ),
      ),
    ),
  ),
)

private lazy val implicitDefs = List(splitDef)
