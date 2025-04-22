package hocki.klocki.typing

import hocki.klocki.analysis.ResolvedNames
import hocki.klocki.ast.Statement.{LocalExistentialDim, SchemaDef}
import hocki.klocki.ast.dim.DimBinding
import hocki.klocki.ast.schema.{Primitive, SchemaBinding, SchemaExpr, SchemaRef}
import hocki.klocki.ast.{Abstra, Statement, Toplevel}
import hocki.klocki.entities.{Dim, DimSetVar, Edge}
import hocki.klocki.typing.Constraint.{DependsOnAll, DependsOnDim, In, InUnion, InductionNamed, InductionUnnamed, MinIn, NotIn}
import hocki.klocki.visualize.printConstraints

import scala.collection.immutable
import scala.collection.mutable
import scala.reflect.ClassTag

def inferTypes(toplevel: Toplevel, nr: ResolvedNames): Map[SchemaBinding, SchemaTy] =
  val schemaDefs = toplevel.statements.collect { case schemaDef: Statement.SchemaDef => schemaDef }
  val typing = mutable.Map[SchemaBinding, SchemaTy]()

  given mutable.Map[SchemaBinding, SchemaTy] = typing
  given ResolvedNames = nr
  given Map[SchemaBinding, SchemaDef] =
    schemaDefs.map[(SchemaBinding, SchemaDef)](schemaDef => schemaDef.binding -> schemaDef).toMap
  given Map[DimBinding, Dim] =
    nr.globalDims.map(binding => binding -> Dim(binding.id.name)).toMap

  schemaDefs.foreach(schemaDef => inferTypeFor(schemaDef.binding))

  typing.toMap

private def inferTypeFor
(schema: SchemaBinding)
(
  using
  nr: ResolvedNames,
  typing: mutable.Map[SchemaBinding, SchemaTy],
  schemaDefs: Map[SchemaBinding, SchemaDef],
  globalDims: Map[DimBinding, Dim],
)
: Unit =
  if typing.contains(schema) then
    return
  val schemaDef = schemaDefs(schema)
  val impl: Abstra.OnIface = schemaDef.impl match
    case onIface: Abstra.OnIface => onIface
    case _ => throw RuntimeException("Rank 1+ definitions verboten")

  val universals = schemaDef.params.universals.map(binding => binding -> Dim(binding.id.name))
  val existentials = schemaDef.params.existentials.map(binding => binding -> Dim(binding.id.name))
  val localDims = nr.localExistentialDims(schemaDef.binding).map(binding => binding -> Dim(binding.id.name))
  val dimParams = universals.toMap ++ existentials.toMap ++ globalDims ++ localDims

  val ins = impl.iface.suppliers.map(binding => DimSetVar(binding.id.name))
  val outs = impl.iface.consumers.map(binding => DimSetVar(binding.id.name))

  val usedDims =
    impl.body.flatMap {
      case use: Statement.BlockUse => getTypeOf(use.expr).usedDims
      case _ => Set()
    }

  val blocks =
    impl
      .body
      .map {
        case use: Statement.BlockUse =>
          val ty = getTypeOf(use.expr)
          if ty.ins.size != use.iface.consumers.size then
            throw IllegalStateException("Wrong number of consumers")
          if ty.outs.size != use.iface.suppliers.size then
            throw IllegalStateException("Wrong number of suppliers")
          val blockName = use.name.getOrElse("<anon>")
          val blockIns = use.iface.consumers.map(binding => DimSetVar(s"$blockName.${binding.id}"))
          val blockOuts = use.iface.suppliers.map(binding => DimSetVar(s"$blockName.${binding.id}"))
          val freshMapping = (ty.ins.zip(blockIns) ++ ty.outs.zip(blockOuts)).toMap
          val dimMapping = use.expr match
            case ref: SchemaExpr.Leaf =>
              if ref.dimArgs.universals.size != ty.universalDims.size then
                throw IllegalStateException(s"Wrong number of universal dim arguments in use of $blockName in $schema. Expected ${ty.universalDims.size}, got ${ref.dimArgs.universals.size}")
              if ref.dimArgs.existentials.size != ty.existentialDims.size || ty.existentialDims.nonEmpty then
                throw IllegalStateException(s"Wrong number of existential dim arguments in use of $blockName in $schema. Expected 0, got ${ref.dimArgs.existentials.size}")
              (ty.universalDims.zip(ref.dimArgs.universals) ++ ty.existentialDims.zip(ref.dimArgs.existentials))
                .map((inType, inRef) => inType -> dimParams(nr.dimNames(inRef)))
                .toMap
            case app: SchemaExpr.App => throw IllegalStateException("Rank 1+ uses verboten")
          val constraints = ty.constraints.map(_.mapDimSetVars(freshMapping).mapDims(dimMapping.withDefault(d => d)))
          val usedDims = ty.usedDims.map(dimMapping.withDefault(d => d))
          (use.iface.allVerticesInOrder.zip(blockIns ++ blockOuts).toMap, constraints, usedDims)
        case schemaDef: Statement.SchemaDef => throw IllegalStateException("Nested defs verboten")
        case _: LocalExistentialDim => throw IllegalStateException("Fresh dims verboten")
      }
      .toSet
  val allUsedDims = blocks.flatMap(_._3)
  val bindingsToDimSetVars = impl.iface.allVerticesInOrder.zip(ins ++ outs).toMap ++ blocks.flatMap(_._1)
  val constraints = blocks.flatMap {
    case (blockIface, blockConstraints, blockUsedDims) =>
      val additionalConstraints =
        blockConstraints.flatMap {
          case InductionUnnamed(from, to) => (allUsedDims diff blockUsedDims).map(from ~_~> to) // ~_~
          case _ => Set()
        }
      blockConstraints ++ additionalConstraints
  }
  val links =
    impl
      .link
      .connections
      .map(conn =>
        val from = bindingsToDimSetVars(nr.vertexNames(conn.from))
        val to = bindingsToDimSetVars(nr.vertexNames(conn.to))
        (from, to)
      )
      .toSet
  println(s"Inferring type for schema ${schemaDef.binding.id.name}")
  typing.put(
    schema,
    inferTypeFromConstraints(
      universals.map(_._2),
      existentials.map(_._2),
      localDims.map(_._2),
      ins, outs, constraints, links)
  )

private def getTypeOf
(expr: SchemaExpr)
(using
 nr: ResolvedNames,
 typing: mutable.Map[SchemaBinding, SchemaTy],
 schemaDefs: Map[SchemaBinding, SchemaDef],
 globalDims: Map[DimBinding, Dim],
): SchemaTy =
  expr match
    case leaf: SchemaExpr.Leaf =>
      leaf.schemaRef match
        case named: SchemaRef.Named =>
          val binding = nr.schemaNames(named)
          inferTypeFor(binding)
          typing(binding)
        case builtin: SchemaRef.Builtin => getTypeOfPrimitive(builtin.primitive)
    case app: SchemaExpr.App => throw IllegalStateException("Rank 1+ usages verboten")
