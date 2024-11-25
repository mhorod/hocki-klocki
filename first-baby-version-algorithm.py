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
@dataclass(frozen=True)
class InUnion[V](DirectConstraint):
    dim: Dim
    dim_set_vars: Tuple[V]

    def mapped[W](self, mapping: Dict[V, W]) -> 'InUnion[W]':
        return InUnion(self.dim, tuple(mapping[d] for d in self.dim_set_vars))

# a \notin X
@dataclass(frozen=True)
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
    

# Y <== Union(X_1, ..., X_n) ? A
@dataclass(frozen=True)
class InducedBy[V](IndirectConstraint):
    induced: V
    inducers: Tuple[V]
    filtered_dims: Tuple[Dim]

    def mapped[W](self, mapping: Dict[V, W]) -> 'InducedBy[W]':
        mapped_inducers = tuple(mapping[i] for i in self.inducers)
        return InducedBy(mapping[self.induced], mapped_inducers, self.filtered_dims)

# a depends on everything from X except explicitly removed dims
# a -> X
@dataclass(frozen=True)
class DependsOn[V](IndirectConstraint):
    dim: Dim
    dim_set_var: V

    def mapped[W](self, mapping: Dict[V, W]) -> 'InducedBy[W]':
        return DependsOn(self.dim, mapping[self.dim_set_var])

@dataclass
class BlockTy:
    in_dim_vars: List[DimSetVar]
    out_dim_vars: List[DimSetVar]
    direct_constraints: Set[DirectConstraint[DimSetVar]]
    indirect_constraints: Set[IndirectConstraint[DimSetVar]]

class Vertex:
    next_index = 0

    def __init__(self, name_root):
        self.name_root = name_root
        self.index = Vertex.next_index
        Vertex.next_index += 1

    @classmethod
    def input(cls):
        return cls("X")

    @classmethod
    def output(cls):
        return cls("Y")
    
    def __repr__(self):
        return f'{self.name_root}{self.index}'

    def __hash__(self):
        return id(self)

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
                         v: Vertex.input()
                         for v in self.in_vertices
                     } | {v: Vertex.output() for v in self.out_vertices})


@dataclass
class Block:
    schema: BlockSchema
    mapping: Dict[Vertex, Vertex]

type Typing = Dict[BlockSchema, BlockTy]

def builtin_schema(in_count: int, out_count: int) -> BlockSchema:
    in_vertices = [Vertex.input() for _ in range(in_count)]
    out_vertices = [Vertex.output() for _ in range(out_count)]
    return BlockSchema(in_vertices, out_vertices, set(), dict())

def union_type(in_count: int) -> BlockTy:
    in_dim_set_vars = [f"d_in_{i}" for i in range(in_count)]
    out_dim_set_var = "d_out"
    constraints = [InducedBy(out_dim_set_var, in_dim_set_var, tuple()) for in_dim_set_var in in_dim_set_vars] 
    return BlockTy(in_dim_set_vars, [out_dim_set_var], [], constraints)

def remove_dim_type(dim: Dim) -> BlockTy:
    in_dim_set_var = "d_in"
    out_dim_set_var = "d_out"
    in_constraint = InUnion(dim, (in_dim_set_var, ))
    out_constraint = NotIn(dim, out_dim_set_var)
    direct_constraints = [in_constraint, out_constraint]
    indirect_constraints = [InducedBy(out_dim_set_var, (in_dim_set_var,), (dim, ))]
    return BlockTy([in_dim_set_var], [out_dim_set_var], direct_constraints, indirect_constraints)

def add_fresh_dim_type(dim: Dim) -> BlockTy:
    in_dim_set_var = "d_in"
    out_dim_set_var = "d_out"
    in_constraints = [InUnion(dim, (out_dim_set_var, )), NotIn(dim, in_dim_set_var)]
    dependency_constraint = DependsOn(dim, in_dim_set_var)
    induction_constraint = InducedBy(out_dim_set_var, (in_dim_set_var, ), (dim, ))
    indirect_constraints = [dependency_constraint, induction_constraint]
    return BlockTy([in_dim_set_var], [out_dim_set_var], in_constraints, indirect_constraints)



def infer_types(schema: BlockSchema, typing: Typing) -> BlockTy:
    if schema in typing:
        return typing[schema]
    
    facts = set()
    
    for block in schema.blocks:
        if block.schema not in typing:
            typing[block.schema] = infer_types(block.schema, typing)
        block_typing = typing[block.schema]

        dim_set_var_mapping = {
            k : block.mapping[v]
            for k, v in 
            list(zip(block_typing.in_dim_vars, block.schema.in_vertices))
            +
            list(zip(block_typing.out_dim_vars, block.schema.out_vertices))
        }
        for constraint in block_typing.direct_constraints:
            facts.add(constraint.mapped(dim_set_var_mapping))
        for constraint in block_typing.indirect_constraints:
            facts.add(constraint.mapped(dim_set_var_mapping))
    print('facts before:')
    for f in facts:
        print(f)

    print()
    for source, destinations in schema.edges.items():
        for destination in destinations:
            facts.add(Equal(source, destination))
            facts.add(Equal(destination, source))
    
    changed = True
    while changed:
        changed = False
        new_facts = set()
        for f1 in facts:
            for f2 in facts:
                conclusions = apply_inference(f1, f2)
                if type(conclusions) is not list:
                    conclusions = [conclusions]
                
                for conclusion in conclusions:
                    if conclusion is not None and conclusion not in facts:
                        changed = True
                        new_facts.add(conclusion)
        facts = facts.union(new_facts)
    
    relevant_facts = set()
    relevant_vertices = set(schema.in_vertices + schema.out_vertices)
    print("Relevant vertices:", relevant_vertices)
    print("facts after")
    for f in facts:
        print(f)
        if isinstance(f, InducedBy):
            if f.induced in relevant_vertices and all(d in relevant_vertices for d in f.inducers):
                relevant_facts.add(f)
        elif isinstance(f, NotIn):
            if f.dim_set_var in relevant_vertices:
                relevant_facts.add(f)
        elif isinstance(f, InUnion):
            if all(d in relevant_vertices for d in f.dim_set_vars):
                relevant_facts.add(f)


    print()
    print("relevant facts")
    for f in relevant_facts:
        print(f)


    print()
    

def apply_inference(f1, f2):
    if isinstance(f1, Equal):
        if isinstance(f2, Equal) and f1.rhs is f2.lhs and f1.lhs is not f2.rhs:
            print("VERY SUS")
            return Equal(f1.lhs, f2.rhs)
        elif isinstance(f2, InducedBy) and f1.rhs is f2.induced:
            #   X = Y       Y <== Union(X_1,...,X_n) ? A
            #  --------------------------------------------
            #          X <== Union(X_1,...,X_n) ? A
            return InducedBy(f1.lhs, f2.inducers, f2.filtered_dims)
        elif isinstance(f2, InducedBy) and f1.lhs in f2.inducers:
            #   X = Y       Z <== Union(X, X_1,...,X_n) ? A
            #  --------------------------------------------
            #          X <== Union(Y, X_1,...,X_n) ? A
            inducers = set(f2.inducers)
            inducers.remove(f1.lhs)
            inducers.add(f1.rhs)
            return InducedBy(f2.induced, tuple(inducers), f2.filtered_dims)
        elif isinstance(f2, NotIn) and f1.rhs is f2.dim_set_var:
            return NotIn(f2.dim, f1.lhs)
        elif isinstance(f2, InUnion) and f1.lhs in f2.dim_set_vars:
            dim_set_vars = set(f2.dim_set_vars)
            dim_set_vars.remove(f1.lhs)
            dim_set_vars.add(f1.rhs)
            return InUnion(f2.dim, tuple(dim_set_vars))
            

    elif isinstance(f1, InducedBy) and isinstance(f2, InducedBy) and f2.induced in f1.inducers:
        #  Z <== Union(Y, Y_1,...,Y_k) ? B      Y <== Union(X_1,...,X_n) ? A      
        # -------------------------------------------------------------------
        #      Y <== Union(Y_1,...,Y_k, X_1, ..., X_n) ? (A intersect B)
        filtered_dims = tuple(set(f1.filtered_dims).intersection(f2.filtered_dims))
        inducers = set(f1.inducers).union(f2.inducers)
        inducers.remove(f2.induced)
        return InducedBy(f1.induced, tuple(inducers), filtered_dims)
    elif isinstance(f1, InducedBy) and isinstance(f2, InducedBy) and f1.induced is f2.induced:
        #  Z <== Union(X_1,...,X_n) ? A     Z <== Union(Y_1,...,Y_k) ? B
        # ---------------------------------------------------------------
        #    Z <== Union(X_1, ..., X_n, Y_1,...,Y_k) ? (A intersect B)
        inducers = set(f1.inducers).union(f2.inducers)
        filtered_dims = tuple(set(f1.filtered_dims).intersection(f2.filtered_dims))
        return InducedBy(f1.induced, tuple(inducers), filtered_dims)
    elif isinstance(f1, InducedBy) and isinstance(f2, NotIn) and f1.induced is f2.dim_set_var:
        # Y <== Union(X_1,...,X_n) ? A   a not in A     a not in Y   
        # ----------------------------------------------------------
        #           a not X_1     ...     a not in X_n
        if f2.dim not in f1.filtered_dims:
            return [NotIn(f2.dim, inducer) for inducer in f1.inducers]
    elif isinstance(f1, InducedBy) and isinstance(f2, InUnion):
        # Y <== Union(X_1,...,X_n) ? A     a not in A   a in Union(X_i1,...,X_ik)  
        # ---------------------------------------------------------------------
        #                             a in Y
        if f2.dim not in f1.filtered_dims and all(d in f1.inducers for d in f2.dim_set_vars):
            return InUnion(f2.dim, (f1.induced, ))
    return None


def convert_to_graphviz(schema: BlockSchema):
    subgraphs = ""
    fake_edges = []
    for i, block in enumerate(schema.blocks):
        in_vs = [block.mapping[v] for v in block.schema.in_vertices]
        out_vs = [block.mapping[v] for v in block.schema.out_vertices]
        fake_edges += [f'{i} -> {o}' for i in in_vs for o in out_vs]
        vertices = ";".join(str(v) for v in in_vs + out_vs)
        subgraph = f"subgraph cluster_{i} {{ {vertices} }}"
        subgraphs += subgraph + "\n"

    edges = ""
    for v, us in schema.edges.items():
        for u in us:
            edges += f"{v} -> {u}\n"

    inner = subgraphs + edges + '\nedge[style=invis] ' + '\n'.join(fake_edges)

    return f"strict digraph {{{inner}\n}}"


add_dim_schema_0 = builtin_schema(1, 1)
add_dim_ty_0 = add_fresh_dim_type("fresh0")

add_dim_schema_1 = builtin_schema(1, 1)
add_dim_ty_1 = add_fresh_dim_type("fresh1")

b0 = add_dim_schema_0.instantiate()
b1 = add_dim_schema_1.instantiate()
v0 = Vertex.input()
v1 = b0.mapping[add_dim_schema_0.in_vertices[0]]
v2 = b0.mapping[add_dim_schema_0.out_vertices[0]]
v3 = b1.mapping[add_dim_schema_1.in_vertices[0]]
v4 = b1.mapping[add_dim_schema_1.out_vertices[0]]
v5 = Vertex.output()
block_schema = BlockSchema([v0], [v5], [b0, b1], {
    v0: [v1],
    v2: [v3],
    v4: [v5],
})

#      X1
#      |
#    [+ fresh0]  
#      |
#      Y1
#      |
#      X2
#      |
#    [+ fresh1]
#      |
#      Y2
#
typing = {
    add_dim_schema_0: add_dim_ty_0,
    add_dim_schema_1: add_dim_ty_1
}
block_type = infer_types(block_schema, typing)

print(block_type)
print(convert_to_graphviz(block_schema))