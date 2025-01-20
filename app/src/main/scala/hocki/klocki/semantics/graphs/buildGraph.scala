package hocki.klocki.semantics.graphs

import hocki.klocki.analysis.ResolvedNames
import hocki.klocki.ast.Statement.{BlockUse, SchemaDef}
import hocki.klocki.ast.{Abstra, AstNode, BuiltinSchema, SchemaBinding, SchemaExpr, Statement, Toplevel, VertexBinding}
import hocki.klocki.entities.DimSetVar
import hocki.klocki.names.{NameGenerator, SimpleNameGenerator}

class SchemaInterface
(
  val inVertices: List[DimSetVar],
  val outVertices: List[DimSetVar]
)

class SchemataInfo
(
  val builtinIds: Map[BuiltinSchema, BlockSchemaId],
  val schemaIds: Map[SchemaBinding, BlockSchemaId],
  val interfaces: Map[BlockSchemaId, SchemaInterface]
):
  def apply(builtin: BuiltinSchema): BlockSchemaId = builtinIds(builtin)

  def apply(schema: SchemaBinding): BlockSchemaId = schemaIds(schema)

def buildGraph(ast: Toplevel, nr: ResolvedNames): List[BlockSchema] =
  given NameGenerator = SimpleNameGenerator()

  val schemaDefs = extractSchemata(ast)
  val builtIns = extractUsedBuiltIns(ast)

  val schemaIds = schemaDefs.map(schemaDef => schemaDef.binding -> BlockSchemaId()).toMap
  val builtinSchemata = builtIns.map(b => b -> fromAst(b)).toMap
  val builtinIds = builtIns.map(b => b -> builtinSchemata(b).id).toMap


  val interfaces = schemaDefs.map(schemaDef => schemaIds(schemaDef.binding) -> createInterface(schemaDef)).toMap
    ++
    builtIns.map(b => {
      val id = builtinIds(b)
      val schema = builtinSchemata(b)
      id -> SchemaInterface(schema.inVertices, schema.outVertices)
    })

  val schemataInfo = SchemataInfo(
    builtinIds,
    schemaIds,
    interfaces
  )

  val schemata =
    schemaDefs.map(schemaDef => buildSchema(schemaDef, nr, schemataInfo)) ++ builtinSchemata.values

  schemata

def createInterface(schemaDef: SchemaDef)(using nameGenerator: NameGenerator): SchemaInterface =
  schemaDef.impl match
    case onSchema: Abstra.OnSchema => throw UnsupportedOperationException("Higher rank is not supported yet")
    case onIface: Abstra.OnIface =>
      val inVertices = onIface.iface.suppliers.map(v => nameGenerator.freshInDimSetVar())
      val outVertices = onIface.iface.consumers.map(v => nameGenerator.freshOutDimSetVar())
      SchemaInterface(inVertices, outVertices)

def extractSchemata(node: AstNode): List[SchemaDef] =
  val children = node.children.flatMap(extractSchemata)
  node match
    case s: SchemaDef => children :+ s
    case s: AstNode => children

def extractUsedBuiltIns(node: AstNode): List[BuiltinSchema] =
  val children = node.children.flatMap(extractUsedBuiltIns)
  node match
    case s: BlockUse => s.expr match
      case s: SchemaExpr.Primitive => children :+ s.builtin
      case _ => children
    case s: AstNode => children

def buildSchema
(
  schemaDef: SchemaDef,
  nr: ResolvedNames,
  schemataInfo: SchemataInfo
)(using nameGenerator: NameGenerator): BlockSchema =
  schemaDef.impl match
    case onSchema: Abstra.OnSchema => throw UnsupportedOperationException("Higher rank is not supported yet")
    case onIface: Abstra.OnIface => buildSchema(schemaDef, nr, schemataInfo, onIface)

def buildSchema
(
  schemaDef: SchemaDef,
  nr: ResolvedNames,
  schemataInfo: SchemataInfo,
  body: Abstra.OnIface
)(using nameGenerator: NameGenerator): BlockSchema =
  val schemaId = schemataInfo(schemaDef.binding)
  val interface = schemataInfo.interfaces(schemaId)
  val schemaVertices = body.iface.suppliers.zip(interface.inVertices) ++ body.iface.consumers.zip(interface.outVertices)

  val blockUses = extractBlockUses(body.body)
  val internalInVertices = blockUses.flatMap(_.iface.consumers).map(v => v -> nameGenerator.freshInDimSetVar())
  val internalOutVertices = blockUses.flatMap(_.iface.suppliers).map(v => v -> nameGenerator.freshOutDimSetVar())
  val allVertices = (schemaVertices ++ internalInVertices ++ internalOutVertices).toMap

  val blocks = blockUses.map { use =>
    val schemaId = use.expr match
      case ref: SchemaExpr.SchemaRef => schemataInfo(nr.schemaNames(ref))
      case prim: SchemaExpr.Primitive => schemataInfo(prim.builtin)
      case app: SchemaExpr.App => throw UnsupportedOperationException("Higher rank is not supported yet")

    val internalInterface = schemataInfo.interfaces(schemaId)
    val inVerticesMapping = use.iface.consumers.map(v => allVertices(v))
    val outVerticesMapping = use.iface.suppliers.map(v => allVertices(v))
    Block(schemaId,
      internalInterface.inVertices.zip(inVerticesMapping).toMap
        ++
        internalInterface.outVertices.zip(outVerticesMapping).toMap
    )
  }.toSet

  val edges = body.link.connections.map { conn =>
    allVertices(nr.vertexNames(conn.from)) -> allVertices(nr.vertexNames(conn.to))
  }.toSet

  BlockSchema(
    schemaId,
    schemaDef.binding.id.name,
    schemataInfo.interfaces(schemaId).inVertices,
    schemataInfo.interfaces(schemaId).outVertices,
    blocks,
    edges
  )

def extractBlockUses(statements: List[Statement]): List[Statement.BlockUse] =
  statements.flatMap {
    case use: Statement.BlockUse => List(use)
    case _ => List()
  }