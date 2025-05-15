package hocki.klocki.visualize.graph

import hocki.klocki.analysis.{ResolvedNames, getExistentialDims}
import hocki.klocki.ast.*
import hocki.klocki.ast.Statement.{BlockUse, SchemaDef}
import hocki.klocki.ast.schema.{SchemaBinding, SchemaExpr, SchemaRef}
import hocki.klocki.names.{NameGenerator, SimpleNameGenerator}

import scala.collection.mutable

class IdGenerator {
  private var nextSchemaId = 0
  private var nextDimId = 0
  private var nextDimSetVarId = 0

  private val dims: mutable.Map[DimId, String] = mutable.Map()
  private val dimSetVars: mutable.Map[DimSetVarId, String] = mutable.Map()

  def getDims: Map[DimId, String] = dims.toMap
  def getDimSetVars: Map[DimSetVarId, String] = dimSetVars.toMap


  def schemaId: SchemaId =
    val id = nextSchemaId
    nextSchemaId += 1
    SchemaId(id)

  def dimId(name: String): DimId =
    val id = nextDimId
    nextDimId += 1
    dims.put(DimId(id), name)
    DimId(id)

  def dimSetVarId(name: String): DimSetVarId =
    val id = nextDimSetVarId
    nextDimSetVarId += 1
    dimSetVars.put(DimSetVarId(id), name)
    DimSetVarId(id)
}

class SchemataInfo
(
  val builtinIds: Map[SchemaRef.Builtin, SchemaId],
  val schemaIds: Map[SchemaBinding, SchemaId],
  val interfaces: Map[SchemaId, SchemaInterface]
):
  def apply(builtin: SchemaRef.Builtin): SchemaId = builtinIds(builtin)

  def apply(schema: SchemaBinding): SchemaId = schemaIds(schema)

def buildProgram(ast: Toplevel, nr: ResolvedNames): Program =
  given NameGenerator = SimpleNameGenerator()

  val idGenerator = IdGenerator()

  given IdGenerator = idGenerator

  val schemaDefs = extractSchemata(ast)
  val builtIns = extractUsedBuiltIns(ast)

  val schemaIds = schemaDefs.map(schemaDef => schemaDef.binding -> idGenerator.schemaId).toMap
  val builtinSchemata = builtIns.map(b => b -> fromAst(b.primitive)).toMap
  val builtinIds = builtIns.map(b => b -> builtinSchemata(b).id).toMap

  val interfaces = schemaDefs.map(schemaDef => schemaIds(schemaDef.binding) -> createInterface(schemaDef)).toMap
    ++
    builtIns.map(b => builtinIds(b) -> builtinSchemata(b).interface)

  val schemataInfo = SchemataInfo(
    builtinIds,
    schemaIds,
    interfaces
  )

  val schemata =
    schemaDefs.map(schemaDef => buildSchema(schemaDef, nr, schemataInfo)) ++ builtinSchemata.values

  Program(
    idGenerator.getDims,
    idGenerator.getDimSetVars,
    schemata
  )

def createInterface
(schemaDef: SchemaDef)
(using idGenerator: IdGenerator): SchemaInterface =
  schemaDef.impl match
    case onSchema: Abstra.OnSchema => throw UnsupportedOperationException("Higher rank is not supported yet")
    case onIface: Abstra.OnIface =>
      val inVertices = onIface.iface.suppliers.map(v => idGenerator.dimSetVarId(v.id.name))
      val outVertices = onIface.iface.consumers.map(v => idGenerator.dimSetVarId(v.id.name))
      SchemaInterface(
        schemaDef.params.universals.map(binding => idGenerator.dimId(binding.id.name)),
        schemaDef.params.existentials.map(ref => idGenerator.dimId(ref.dimId.name)),
        inVertices,
        outVertices,
      )

def extractSchemata(node: AstNode): List[SchemaDef] =
  val children = node.children.flatMap(extractSchemata)
  node match
    case s: SchemaDef => children :+ s
    case s: AstNode => children

def extractUsedBuiltIns(node: AstNode): List[SchemaRef.Builtin] =
  val children = node.children.flatMap(extractUsedBuiltIns)
  node match
    case s: BlockUse => s.expr match
      case s: SchemaExpr.Leaf =>
        s.schemaRef match
          case builtin: SchemaRef.Builtin => children :+ builtin
          case _ => children
      case _ => children
    case s: AstNode => children

def buildSchema
(
  schemaDef: SchemaDef,
  nr: ResolvedNames,
  schemataInfo: SchemataInfo,
)(using nameGenerator: NameGenerator, idGenerator: IdGenerator): Schema =
  schemaDef.impl match
    case onSchema: Abstra.OnSchema => throw UnsupportedOperationException("Higher rank is not supported yet")
    case onIface: Abstra.OnIface => buildSchema(schemaDef, nr, schemataInfo, onIface)

def buildSchema
(
  schemaDef: SchemaDef,
  nr: ResolvedNames,
  schemataInfo: SchemataInfo,
  body: Abstra.OnIface
)(using nameGenerator: NameGenerator, idGenerator: IdGenerator): Schema =
  val schemaId = schemataInfo(schemaDef.binding)
  val interface = schemataInfo.interfaces(schemaId)
  val schemaVertices = body.iface.suppliers.zip(interface.inVertices) ++ body.iface.consumers.zip(interface.outVertices)

  val blockUses = extractBlockUses(body.body)
  val internalInVertices = blockUses
    .flatMap(_.iface.consumers)
    .map(v => v -> idGenerator.dimSetVarId(v.id.name))
  val internalOutVertices = blockUses
    .flatMap(_.iface.suppliers)
    .map(v => v -> idGenerator.dimSetVarId(v.id.name))
  val allVertices = (schemaVertices ++ internalInVertices ++ internalOutVertices).toMap

  val ifaceUniversalDims = schemaDef.params.universals.zip(interface.universalDims)
  val ifaceExistentialDims = schemaDef.params.existentials.zip(interface.existentialDims)
  val localExistentialDims = getExistentialDims(body).map(binding => (binding, idGenerator.dimId(binding.id.name)))
  val allDims = (ifaceUniversalDims ++ ifaceExistentialDims ++ localExistentialDims).toMap

  println(s"Local: $localExistentialDims")
  println(s"All: $allDims")

  val blocks = blockUses.map { use =>
    val (internalId, iface, universals, existentials) = use.expr match
      case leaf: SchemaExpr.Leaf =>
        val id = leaf.schemaRef match
          case named: SchemaRef.Named => schemataInfo(nr.schemaNames(named))
          case builtin: SchemaRef.Builtin => schemataInfo(builtin)
        val internalInterface = schemataInfo.interfaces(id)
        (
          id,
          internalInterface,
          leaf.dimArgs.universals.map(ref => allDims(nr.dimNames(ref))),
          leaf.dimArgs.existentials.map(binding => allDims(binding))
        )
      case app: SchemaExpr.App => throw UnsupportedOperationException("Higher rank is not supported yet")

    val inVerticesMapping = use.iface.consumers.map(allVertices)
    val outVerticesMapping = use.iface.suppliers.map(allVertices)
    val universalsMapping = iface.universalDims.zip(universals).toMap
    val existentialsMapping = iface.existentialDims.zip(existentials).toMap

    val block = Block(
      internalId,
      iface.inVertices.zip(inVerticesMapping).toMap
        ++
        iface.outVertices.zip(outVerticesMapping).toMap,
      universalsMapping ++ existentialsMapping
    )
    block
  }.toSet

  val edges = body.link.connections.map { conn =>
    allVertices(nr.vertexNames(conn.from)) -> allVertices(nr.vertexNames(conn.to))
  }.toSet

  Schema(
    schemaId,
    schemaDef.binding.id.name,
    interface,
    blocks,
    edges
  )

def extractBlockUses(statements: List[Statement]): List[Statement.BlockUse] =
  statements.collect { case s: Statement.BlockUse => s }
