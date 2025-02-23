package hocki.klocki.ast.vertex

import hocki.klocki.ast.vertex.{BlockId, VertexId}

enum VertexRef:
  case Plain(vertexId: VertexId)
  case Scoped(blockId: BlockId, vertexId: VertexId)

  override def toString: String = this match
    case VertexRef.Plain(vertexId) => vertexId.toString
    case VertexRef.Scoped(blockId, vertexId) => s"$blockId.$vertexId"
