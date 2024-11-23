from dataclasses import dataclass

from abc import ABC, abstractmethod
from typing import *
from typing import Dict

type Dim = str
type DimSetVar = str

class DirectConstraint[V](ABC):
    @abstractmethod
    def mapped[W](self, mapping: Dict[V, W]) -> 'DirectConstraint[W]':
        pass

class IndirectConstraint[V](ABC):
    @abstractmethod
    def mapped[W](self, mapping: Dict[V, W]) -> 'IndirectConstraint[W]':
        pass

# a \in Union(X)
@dataclass
class InUnion[V](DirectConstraint):
    dim: Dim
    dim_set_vars: Set[V]

    def mapped[W](self, mapping: Dict[V, W]) -> 'InUnion[W]':
        return InUnion(self.dim, {mapping[d] for d in self.dim_set_vars})

# a \notin X
@dataclass
class NotIn[V](DirectConstraint):
    dim: Dim
    dim_set_var: V

    def mapped[W](self, mapping: Dict[V, W]) -> 'NotIn[W]':
        return NotIn(self.dim, mapping[self.dim_set_var])

# X = Y
@dataclass(frozen=True)
class Equal[V](DirectConstraint):
    lhs: V
    rhs: V

    def mapped[W](self, mapping: Dict[V, W]) -> 'DirectConstraint[W]':
        return Equal(mapping[self.lhs], mapping[self.rhs])
    

# Y <== X
@dataclass
class InducedBy[V](IndirectConstraint):
    induced: V
    inducer: V

    def mapped[W](self, mapping: Dict[V, W]) -> 'InducedBy[W]':
        return InducedBy(mapping[self.induced], mapping[self.inducer])

# a depends on everything from X except explicitly removed dims
# a -> X
@dataclass
class DependsOn[V](IndirectConstraint):
    dim: Dim
    dim_set_var: V

    def mapped[W](self, mapping: Dict[V, W]) -> 'InducedBy[W]':
        return InducedBy(mapping[self.induced], mapping[self.inducer])

@dataclass
class BlockTy:
    in_dim_vars: List[DimSetVar]
    out_dim_vars: List[DimSetVar]
    direct_constraints: Set[DirectConstraint[DimSetVar]]
    indirect_constraints: Set[IndirectConstraint[DimSetVar]]

class Vertex:
    pass

class BlockSchema:
    in_vertices: List[Vertex]
    out_vertices: List[Vertex]
    blocks: Set['Block']
    edges: Dict[Vertex, List[Vertex]]

    def __init__(self, in_vertices, out_vertices, blocks, edges):
        self.in_vertices = in_vertices
        self.out_vertices = out_vertices
        self.blocks = blocks
        self.edges = edges

    def instantiate(self):
        return Block(self, 
                     {
                         v: Vertex()
                         for v in self.in_vertices + self.out_vertices
                     })


@dataclass
class Block:
    schema: BlockSchema
    mapping: Dict[Vertex, Vertex]

type Typing = Dict[BlockSchema, BlockTy]

def builtin_schema(in_count: int, out_count: int) -> BlockSchema:
    in_vertices = [Vertex() for _ in range(in_count)]
    out_vertices = [Vertex() for _ in range(out_count)]
    return BlockSchema(in_vertices, out_vertices, set(), dict())

def union_type(in_count: int) -> BlockTy:
    in_dim_set_vars = [f"d_in_{i}" for i in range(in_count)]
    out_dim_set_var = "d_out"
    constraints = [InducedBy(out_dim_set_var, in_dim_set_var) for in_dim_set_var in in_dim_set_vars] 
    return BlockTy(in_dim_set_vars, [out_dim_set_var], [], constraints)

def remove_dim_type(dim: Dim) -> BlockTy:
    in_dim_set_var = "d_in"
    out_dim_set_var = "d_out"
    in_constraint = InUnion(dim, {in_dim_set_var})
    out_constraint = NotIn(dim, out_dim_set_var)
    direct_constraints = [in_constraint, out_constraint]
    indirect_constraints = [InducedBy(out_dim_set_var, in_dim_set_var)]
    return BlockTy([in_dim_set_var], [out_dim_set_var], direct_constraints, indirect_constraints)

def add_fresh_dim_type(dim: Dim) -> BlockTy:
    in_dim_set_var = "d_in"
    out_dim_set_var = "d_out"
    in_constraint = [InUnion(dim, {out_dim_set_var}), NotIn(dim, in_dim_set_var)]
    dependency_constraint = DependsOn(dim, in_dim_set_var)
    induction_constraint = InducedBy(out_dim_set_var, in_dim_set_var)
    indirect_constraints = [dependency_constraint, induction_constraint]
    return BlockTy([in_dim_set_var], [out_dim_set_var], [in_constraint], indirect_constraints)



def infer_types(schema: BlockSchema, typing: Typing) -> BlockTy:
    if schema in typing:
        return typing[schema]
    
    facts = set()
    
    for block in schema.blocks:
        if block.schema not in typing:
            block_typing = typing[block.schema] = infer_types(block.schema, typing)
            dim_set_var_mapping = {
                k : block.mapping[v]
                 for k, v in 
                [zip(block_typing.in_dim_vars, block.schema.in_vertices)]
                +
                [zip(block_typing.out_dim_vars, block.schema.out_vertices)]
            }
            for constraint in block_typing.direct_constraints:
                facts.add(constraint.mapped(dim_set_var_mapping))
            for constraint in block_typing.secondary_constraints:
                facts.add(constraint.mapped(dim_set_var_mapping))
    
    for source, destinations in schema.edges.items():
        for destination in destinations:
            facts.add(Equal(source, destination))
            facts.add(Equal(destination, source))
    
    changed = True
    while changed:
        changed = False
        for f1 in facts:
            for f2 in facts:
                conclusion = apply_inference(f1, f2)
                if conclusion is not None and conclusion not in facts:
                    changed = True
                    facts.add(conclusion)
        
def apply_inference(f1, f2):
    if isinstance(f1, Equal):
        if isinstance(f2, InducedBy) and f1.rhs is f2.induced:
            return InducedBy(f1.lhs, f2.inducer)
        if isinstance(f2, NotIn) and f1.rhs is f2.dim_set_var:
            return NotIn(f2.dim, f1.lhs)
    return None

add_dim_schema_0 = builtin_schema(1, 1)
add_dim_ty_0 = add_fresh_dim_type("fresh0")

add_dim_schema_1 = builtin_schema(1, 1)
add_dim_ty_1 = add_fresh_dim_type("fresh1")

b0 = add_dim_schema_0.instantiate()
b1 = add_dim_schema_1.instantiate()
v0 = Vertex()
v1 = b0.mapping[add_dim_schema_0.in_vertices[0]]
v2 = b0.mapping[add_dim_schema_0.out_vertices[0]]
v3 = b1.mapping[add_dim_schema_1.in_vertices[0]]
v4 = b1.mapping[add_dim_schema_1.out_vertices[0]]
v5 = Vertex()
block = BlockSchema([v0], [v1], [b0, b1], {
    v0: [v1],
    v2: [v3],
    v4: [v5],
})

typing = {
    add_dim_schema_0: add_dim_ty_0,
    add_dim_schema_1: add_dim_ty_1
}
infer_types(block, typing)