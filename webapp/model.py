from dataclasses import dataclass
from typing import List, Dict

@dataclass
class SchemaInterface:
    universal_dims: list
    existential_dims: list
    in_vertices: list
    out_vertices: list

    def from_dto(obj):
        return SchemaInterface(
            obj["universalDims"],
            obj["existentialDims"],
            obj["inVertices"],
            obj["outVertices"]
        )

    def vertices(self):
        return self.in_vertices + self.out_vertices

    def dims(self):
        return self.universal_dims + self.existential_dims

@dataclass
class Block:
    schema_id: int
    interface_mapping: Dict[int, int]
    dim_mapping: Dict[int, int]

    def from_dto(obj):
        return Block(
            obj["schemaId"],
            map_from_list_of_pairs(obj["interfaceMapping"]),
            map_from_list_of_pairs(obj["dimMapping"])
        )

@dataclass
class Schema:
    id: int
    name: str
    interface: SchemaInterface
    blocks: List[Block]
    edges: set

    def from_dto(obj):
        return Schema(
            obj["id"],
            obj["name"],
            SchemaInterface.from_dto(obj["interface"]),
            [Block.from_dto(b) for b in obj["blocks"]],
            set([tuple (e) for e in obj["edges"]])
        )

    def vertices(self):
        return self.interface.vertices() + [v for block in self.blocks for v in block.interface_mapping.values()]


@dataclass
class Program:
    dims: dict
    global_dims: List[int]
    dim_set_vars: dict
    schemata: List[Schema]

    def from_dto(obj):
        return Program(
            map_from_list_of_pairs(obj["dims"]),
            obj["globalDims"],
            map_from_list_of_pairs(obj["dimSetVars"]),
            [Schema.from_dto(s) for s in obj["schemata"]]
        )

    def get_schema_by_id(self, id):
        for s in self.schemata:
            if s.id == id:
                return s

def map_from_list_of_pairs(pairs):
    return { key: value for key, value in pairs }