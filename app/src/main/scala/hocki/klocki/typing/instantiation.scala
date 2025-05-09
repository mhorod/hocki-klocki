package hocki.klocki.typing

import hocki.klocki.analysis.ResolvedNames
import hocki.klocki.ast.Abstra.OnIface
import hocki.klocki.ast.Statement.{BlockUse, LocalExistentialDim, SchemaDef}
import hocki.klocki.ast.dim.{DimArgs, DimBinding, DimParams}
import hocki.klocki.ast.schema.SchemaExpr.Leaf
import hocki.klocki.ast.schema.{IfaceBinding, SchemaBinding, SchemaExpr, SchemaRef}
import hocki.klocki.ast.vertex.VertexBinding
import hocki.klocki.ast.{Abstra, Statement, Toplevel}
import hocki.klocki.entities.{Dim, DimSetVar}
import hocki.klocki.typing.Constraint.InductionUnnamed

import scala.collection.immutable
import scala.collection.mutable

private case class SchemaDims
(
  universals: Map[DimBinding, Dim],
  existentials: Map[DimBinding, Dim],
  locals: Map[DimBinding, Dim],
):
  lazy val all: Map[DimBinding, Dim] = universals ++ existentials ++ locals

private case class SchemaDimSetVars
(
  ins: Map[VertexBinding, DimSetVar],
  outs: Map[VertexBinding, DimSetVar],
  internal: Map[VertexBinding, DimSetVar],
):
  lazy val iface: Map[VertexBinding, DimSetVar] = ins ++ outs

  lazy val all: Map[VertexBinding, DimSetVar] = iface ++ internal


def instantiateSchemata(toplevel: Toplevel, nr: ResolvedNames): Map[SchemaBinding, Schema] =
  given ResolvedNames = nr

  val schemaDims = nr.schemaDefs.map(d => d.binding -> createDims(d)).toMap
  val schemaDsvs = nr.schemaDefs.map(d => d.binding -> createDimSetVars(d)).toMap
  val definedIfaces = constructDefinedIfaces(schemaDims, schemaDsvs)

  instantiateSchemata(schemaDims, schemaDsvs, definedIfaces)

def createDims(schemaDef: SchemaDef): SchemaDims =
  val universals = instantiateBound(schemaDef.params.universals, Dim(_))
  val existentials = instantiateBound(schemaDef.params.existentials, Dim(_))
  val locals = instantiateBound(
    schemaDef.impl.asInstanceOf[OnIface]
      .body
      .collect { case exists: LocalExistentialDim => exists.binding },
    Dim(_),
  )
  SchemaDims(universals, existentials, locals)

def createDimSetVars(schemaDef: SchemaDef): SchemaDimSetVars =
  val impl = schemaDef.impl.asInstanceOf[OnIface]
  val ins = instantiateBound[VertexBinding, DimSetVar](impl.iface.suppliers, DimSetVar(_))
  val outs = instantiateBound[VertexBinding, DimSetVar](impl.iface.consumers, DimSetVar(_))
  val internal = impl.blockUses.flatMap(use => instantiateBound(use.iface.allVerticesInOrder, DimSetVar(_))).toMap
  SchemaDimSetVars(ins, outs, internal)

def instantiateSchemata
(
  schemaDims: Map[SchemaBinding, SchemaDims],
  schemaDsvs: Map[SchemaBinding, SchemaDimSetVars],
  definedIfaces: Map[SchemaBinding, SchemaIface],
)
(using nr: ResolvedNames): Map[SchemaBinding, Schema] =
  nr.schemaDefs.map(d =>
    val impl = d.impl.asInstanceOf[OnIface]
    val dims = schemaDims(d.binding)
    d.binding -> Schema(
      definedIfaces(d.binding),
      constructInternalsInImpl(
        impl,
        dims,
        schemaDsvs(d.binding),
        definedIfaces,
      ),
      constructRenamersInImpl(
        d.binding,
        impl,
        dims,
        schemaDsvs(d.binding),
        definedIfaces,
      ),
    )
  ).toMap
    ++
    nr.primitives.map(
      (primitive, binding) =>
        val ty = getTypeOfPrimitive(primitive)
        binding -> Schema(
          ty.iface,
          SchemaInternals(Set(), Set(), Set()),
          Map()
        )
    )

def constructInternalsInImpl
(
  impl: OnIface,
  dims: SchemaDims,
  dsvs: SchemaDimSetVars,
  definedIfaces: Map[SchemaBinding, SchemaIface]
)
(using nr: ResolvedNames): SchemaInternals =
  val edges = impl.link.connections.map(
    connection =>
      val from = nr.vertexNames(connection.from)
      val to = nr.vertexNames(connection.to)
      (dsvs.all(from), dsvs.all(to))
  )
  SchemaInternals(
    dims.locals.values.toSet,
    dsvs.internal.values.toSet,
    edges.toSet,
  )

def constructRenamersInImpl
(
  binding: SchemaBinding,
  impl: OnIface,
  dims: SchemaDims,
  dsvs: SchemaDimSetVars,
  definedIfaces: Map[SchemaBinding, SchemaIface]
)
(using nr: ResolvedNames): Map[SchemaBinding, Set[Renamer]] =
  impl.blockUses.map(use =>
    val usedSchema = use.expr.asInstanceOf[Leaf]
    val binding = nr.schemaNames(usedSchema.schemaRef)
    val usedIface = constructUsedIface(dims, usedSchema.dimArgs, dsvs, use.iface)
    binding -> Renamer(definedIfaces(binding), usedIface, dims)
  ).groupMap(_._1)(_._2)

def constructDefinedIfaces
(
  dims: Map[SchemaBinding, SchemaDims],
  dsvs: Map[SchemaBinding, SchemaDimSetVars],
)(using nr: ResolvedNames): Map[SchemaBinding, SchemaIface] = {
  nr.schemaDefs.map(schemaDef =>
    val impl = schemaDef.impl.asInstanceOf[OnIface]
    schemaDef.binding -> constructDefinedIface(
      dims(schemaDef.binding),
      schemaDef.params,
      dsvs(schemaDef.binding),
      impl.iface,
    )
  ).toMap
  ++
  nr.primitives.map(
    (primitive, binding) =>
      binding -> getTypeOfPrimitive(primitive).iface
  )
}

def constructDefinedIface
(
  dims: SchemaDims,
  params: DimParams,
  dsvs: SchemaDimSetVars,
  iface: IfaceBinding.Internal,
): SchemaIface =
  SchemaIface(
    params.universals.map(dims.universals),
    params.existentials.map(dims.existentials),
    iface.suppliers.map(dsvs.ins),
    iface.consumers.map(dsvs.outs),
  )

def constructUsedIface
(
  dims: SchemaDims,
  args: DimArgs,
  dsvs: SchemaDimSetVars,
  vertexIface: IfaceBinding.External
)(using nr: ResolvedNames): SchemaIface =
  SchemaIface(
    args.universals.map(d => dims.universals(nr.dimNames(d))),
    args.existentials.map(d => dims.existentials(nr.dimNames(d))),
    vertexIface.consumers.map(v => dsvs.all(v)),
    vertexIface.suppliers.map(v => dsvs.all(v))
  )

