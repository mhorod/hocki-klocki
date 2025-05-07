package hocki.klocki.typing

import hocki.klocki.analysis.ResolvedNames
import hocki.klocki.ast.Statement.{BlockUse, LocalExistentialDim, SchemaDef}
import hocki.klocki.ast.dim.{DimArgs, DimBinding, DimParams}
import hocki.klocki.ast.schema.{IfaceBinding, SchemaBinding, SchemaExpr, SchemaRef}
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

def inferTypes(toplevel: Toplevel, nr: ResolvedNames): Map[SchemaBinding, SchemaTy] =
  given ResolvedNames = nr

  val schemaDims = nr.schemaDefs.map(d => d.binding -> createDims(d)).toMap
  val definedIfaces = constructDefinedIfaces(schemaDims, nr.schemaDefs)

  ???

def createDims(schemaDef: SchemaDef): SchemaDims =
  val universals = schemaDef.params.universals.map(binding => binding -> Dim(binding.id.name)).toMap
  val existentials = schemaDef.params.existentials.map(binding => binding -> Dim(binding.id.name)).toMap
  val locals =
    getMonomorphic(schemaDef.impl)
      .body
      .collect { case exists: LocalExistentialDim => exists.binding -> Dim(exists.binding.id.name) }
      .toMap
  SchemaDims(universals, existentials, locals)


def constructDefinedIfaces(schemaDims: Map[SchemaBinding, SchemaDims], defs: Set[SchemaDef]): Map[SchemaBinding, SchemaIface] =
  defs.map(schemaDef =>
    schemaDef.binding -> constructDefinedIface(
      schemaDims(schemaDef.binding),
      schemaDef.params, 
      getMonomorphic(schemaDef.impl).iface)
  ).toMap

def constructDefinedIface
(
  dims: SchemaDims, 
  params: DimParams,
  vertexIface: IfaceBinding.Internal,
): SchemaIface =
  SchemaIface(
    params.universals.map(dims.universals),
    params.existentials.map(dims.existentials),
    vertexIface.suppliers.map(v => DimSetVar(v.id.name)),
    vertexIface.consumers.map(v => DimSetVar(v.id.name))
  )

def constructUsedIface(args: DimArgs, vertexIface: IfaceBinding.External)(using nr: ResolvedNames): SchemaIface = ???
//  SchemaIface(
//    args.universals.map(nr.dimNames)
//  )
