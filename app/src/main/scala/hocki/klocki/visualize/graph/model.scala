package hocki.klocki.visualize.graph

import upickle.default.ReadWriter
import upickle.default.readwriter

case class SchemaId(id: Int)
implicit val SchemaIdWriter: ReadWriter[SchemaId] = readwriter[Int].bimap[SchemaId](_.id, SchemaId(_))

case class DimId(id: Int) derives ReadWriter
implicit val DimIdWriter: ReadWriter[DimId] = readwriter[Int].bimap[DimId](_.id, DimId(_))

case class DimSetVarId(id: Int) derives ReadWriter
implicit val DimSetVarIdWriter: ReadWriter[DimSetVarId] = readwriter[Int].bimap[DimSetVarId](_.id, DimSetVarId(_))

case class SchemaInterface
(
  universalDims: List[DimId],
  existentialDims: List[DimId],
  inVertices: List[DimSetVarId],
  outVertices: List[DimSetVarId],
)derives ReadWriter

case class Schema
(
  id: SchemaId,
  name: String,
  interface: SchemaInterface,
  blocks: Set[Block],
  edges: Set[(DimSetVarId, DimSetVarId)],
)derives ReadWriter

case class Block
(
  schemaId: SchemaId,
  interfaceMapping: Map[DimSetVarId, DimSetVarId],
  dimMapping: Map[DimId, DimId],
)derives ReadWriter

case class Program
(
  dims: Map[DimId, String],
  globalDims: Set[DimId],
  dimSetVars: Map[DimSetVarId, String],
  schemata: List[Schema]
)derives ReadWriter