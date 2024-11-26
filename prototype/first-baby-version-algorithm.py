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
    
    def __str__(self):
        dim_set_vars_str = ",".join(str(d) for d in self.dim_set_vars)
        if len(self.dim_set_vars) == 1:
            return f"{self.dim} in {dim_set_vars_str}"
        else:
            return f"{self.dim} in Union({dim_set_vars_str})"


# a \notin X
@dataclass(frozen=True)
class NotIn[V](DirectConstraint):
    dim: Dim
    dim_set_var: V

    def mapped[W](self, mapping: Dict[V, W]) -> 'NotIn[W]':
        return NotIn(self.dim, mapping[self.dim_set_var])

    def __str__(self):
        return f"{self.dim} not in {self.dim_set_var}"

# X = Y
@dataclass(frozen=True)
class Equal[V](DirectConstraint):
    lhs: V
    rhs: V

    def mapped[W](self, mapping: Dict[V, W]) -> 'DirectConstraint[W]':
        return Equal(mapping[self.lhs], mapping[self.rhs])
    
    def str(self):
        return f"{self.lhs} == {self.rhs}"
    

@dataclass(frozen=True)
class Inducer[V]:
    dim_set_var: V
    filtered_dims: Tuple[Dim]

    def mapped[W](self, mapping: Dict[V, W]) -> 'Inducer[W]':
        return Inducer(mapping[self.dim_set_var], self.filtered_dims)
    
    def __str__(self):
        if len(self.filtered_dims) == 0:
            return str(self.dim_set_var)
        else:
            dims_str = ",".join(str(d) for d in self.filtered_dims)
            return f"{self.dim_set_var} ? {{{dims_str}}}"

# Y <== Union(X_1 ? A_1, ..., X_n ? A_n)
@dataclass(frozen=True)
class InducedBy[V](IndirectConstraint):
    induced: V
    inducers: Tuple[Inducer[V]]

    def mapped[W](self, mapping: Dict[V, W]) -> 'InducedBy[W]':
        mapped_inducers = tuple(i.mapped(mapping) for i in self.inducers)
        return InducedBy(mapping[self.induced], mapped_inducers)
    
    def inducer_vars(self):
        return set(i.dim_set_var for i in self.inducers)

    
    def __str__(self):
        inducers_str = ",".join(str(i) for i in self.inducers)
        if len(self.inducers) == 1:
            return f"{self.induced} <== {inducers_str}"
        return f"{self.induced} <== Union({inducers_str})"

# a depends on everything from X except explicitly removed dims
# a -> X
@dataclass(frozen=True)
class DependsOn[V](IndirectConstraint):
    dim: Dim
    dim_set_var: V
    filtered_dims: Tuple[Dim]

    def mapped[W](self, mapping: Dict[V, W]) -> 'InducedBy[W]':
        return DependsOn(self.dim, mapping[self.dim_set_var], self.filtered_dims)

    def __str__(self):
        filtered_dims_str = ",".join(str(d) for d in self.filtered_dims)
        if len(self.filtered_dims) == 0:
            return f"{self.dim} -> {self.dim_set_var}"
        else:
            return f"{self.dim} -> {self.dim_set_var} ? {{{filtered_dims_str}}}"
            

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
    inducers = [Inducer(d, tuple()) for d in in_dim_set_vars]
    constraints = [InducedBy(out_dim_set_var, tuple(inducers))]
    return BlockTy(in_dim_set_vars, [out_dim_set_var], [], constraints)

def remove_dim_type(dim: Dim) -> BlockTy:
    in_dim_set_var = "d_in"
    out_dim_set_var = "d_out"
    in_constraint = InUnion(dim, (in_dim_set_var, ))
    out_constraint = NotIn(dim, out_dim_set_var)
    direct_constraints = [in_constraint, out_constraint]
    indirect_constraints = [InducedBy(out_dim_set_var, (Inducer(in_dim_set_var, (dim,)),) )]
    return BlockTy([in_dim_set_var], [out_dim_set_var], direct_constraints, indirect_constraints)

def add_fresh_dim_type(dim: Dim) -> BlockTy:
    in_dim_set_var = "d_in"
    out_dim_set_var = "d_out"
    in_constraints = [InUnion(dim, (out_dim_set_var, )), NotIn(dim, in_dim_set_var)]
    dependency_constraint = DependsOn(dim, in_dim_set_var, tuple())
    induction_constraint = InducedBy(out_dim_set_var, (Inducer(in_dim_set_var, (dim, ) ),))
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
    print("facts after")
    for f in facts:
        print(f)
        if isinstance(f, InducedBy):
            if f.induced in relevant_vertices and all(d in relevant_vertices for d in f.inducer_vars()):
                relevant_facts.add(f)
        elif isinstance(f, NotIn):
            if f.dim_set_var in relevant_vertices:
                relevant_facts.add(f)
        elif isinstance(f, InUnion):
            if all(d in relevant_vertices for d in f.dim_set_vars):
                relevant_facts.add(f)
        elif isinstance(f, DependsOn):
            if f.dim_set_var in relevant_vertices:
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
            print(f1.lhs, f2.rhs)
            return Equal(f1.lhs, f2.rhs)
        elif isinstance(f2, InducedBy) and f1.rhs is f2.induced:
            #   X = Y       Y <== Union(X_1 ? A_1,...,X_n ? A_n)
            #  --------------------------------------------
            #          X <== Union(X_1 ? A_1,...,X_n ? A_n) 
            return InducedBy(f1.lhs, f2.inducers)
        elif isinstance(f2, InducedBy) and f1.lhs in f2.inducer_vars():
            #   X = Y       Z <== Union(X, X_1,...,X_n) ? A
            #  --------------------------------------------
            #          Z <== Union(Y, X_1,...,X_n) ? A
            inducers = {i.dim_set_var:i.filtered_dims for i in f2.inducers}
            inducers[f1.rhs] = inducers[f1.lhs]
            del inducers[f1.lhs]
            inducers = tuple(Inducer(d, f) for d, f in inducers.items())
            return InducedBy(f2.induced, tuple(inducers))
        elif isinstance(f2, NotIn) and f1.lhs is f2.dim_set_var:
            return NotIn(f2.dim, f1.rhs)
        elif isinstance(f2, InUnion) and f1.lhs in f2.dim_set_vars:
            dim_set_vars = set(f2.dim_set_vars)
            dim_set_vars.remove(f1.lhs)
            dim_set_vars.add(f1.rhs)
            return InUnion(f2.dim, tuple(dim_set_vars))
        elif isinstance(f2, DependsOn) and f1.lhs is f2.dim_set_var:
            return DependsOn(f2.dim, f1.rhs, f2.filtered_dims)

    elif isinstance(f1, InducedBy) and isinstance(f2, InducedBy) and f2.induced in f1.inducer_vars():
        return infer_transitive_induction(f1, f2)
        
    elif isinstance(f1, InducedBy) and isinstance(f2, InducedBy) and f1.induced is f2.induced:
        return infer_induction_union(f1, f2)
    elif isinstance(f1, InducedBy) and isinstance(f2, NotIn) and f1.induced is f2.dim_set_var:
        return infer_not_in_induction(f1, f2)
    elif isinstance(f1, InducedBy) and isinstance(f2, InUnion):
        return infer_in_union_induction(f1, f2)
    elif isinstance(f1, InducedBy) and isinstance(f2, DependsOn):
        return infer_dependency_induction(f1, f2)
    return None

def infer_transitive_induction(lhs: InducedBy, rhs: InducedBy):
    #  Z <== Union(Y ? B, Y_1 ? B_1,...,Y_k ? B_k)    Y <== Union(X_1 ? A_1,...,X_n ? A_n)    
    # --------------------------------------------------------------------------------------
    #      Z <== Union(X_1 ? (A_1 u B), ..., X_n ? (A_n u B), Y_1 ? B_1,...,Y_k ? B_k)
    #
    #  If Y_i = X_j then we get X_j ? ((A_j union B) intersect B_i)

    old_inducers = { i.dim_set_var:i.filtered_dims for i in lhs.inducers }
    rhs_induced_filtered = old_inducers[rhs.induced]
    new_inducers = { i.dim_set_var:set(i.filtered_dims).union(rhs_induced_filtered) for i in rhs.inducers }

    redundant_inducers = set(i for i in old_inducers if i in new_inducers)
    for i in redundant_inducers:
        new_inducers[i] = new_inducers[i].intersection(old_inducers[i])
        del old_inducers[i]
    
    del old_inducers[rhs.induced]

    inducers = []
    for i in old_inducers:
        inducers.append(Inducer(i,tuple(old_inducers[i])))
    for i in new_inducers:
        inducers.append(Inducer(i,tuple(new_inducers[i])))

    return InducedBy(lhs.induced, tuple(inducers))

# is this even needed? we should never get two inductions such that one does not reduce to the other
def infer_induction_union(lhs: InducedBy, rhs: InducedBy):
    #  Z <== Union(X_1 ? A_1,...,X_n ? A_n)    Z <== Union(Y_1 ? B_1,..., Y_k ? B_k)
    # ---------------------------------------------------------------
    #    Z <== Union(X_1 ? A_1, ..., X_n ? A_n, Y_1 ? B,...,Y_k ? B)
    pass

def infer_in_union_induction(lhs: InducedBy, rhs: InUnion):
    inducers = { i.dim_set_var : i.filtered_dims for i in lhs.inducers }
    if all(d in inducers and rhs.dim not in inducers[d] for d in rhs.dim_set_vars):
        # Y <== Union(X_1 ? A_1, ..., X_n ? A_n)    a in Union(X_i1,..., X_ik)   a not in Union(A_i1, ..., A_ik)
        # ------------------------------------------------------------------------------------------------------
        #                           a in Y
        return InUnion(rhs.dim, (lhs.induced,))
    elif lhs.induced in rhs.dim_set_vars:
        # Y <== Union(X_1 ? A_1, ..., X_n ? A_n)    a in Y 
        # --------------------------------------------------
        #              a in Union(X_1, ..., X_n)

        return InUnion(rhs.dim, tuple(lhs.inducer_vars()))

def infer_not_in_induction(lhs: InducedBy, rhs: NotIn):
    if rhs.dim_set_var is lhs.induced:
        # Y <== Union(X_1 ? A_1,..., A_n) ? A    a not in Y   a not in A_i
        # -----------------------------------------------------------------
        #                           a not in X_i
        return [NotIn(rhs.dim, inducer.dim_set_var) for inducer in lhs.inducers if rhs.dim not in inducer.filtered_dims]


def infer_dependency_induction(lhs: InducedBy, rhs: DependsOn):
    # Y <== Union(X_1 ? A_1, ..., X_n ? A_n)     a -> Y
    # -----------------------------------
    #           a -> X_i ? A_i
    if rhs.dim_set_var is lhs.induced:
        return [DependsOn(rhs.dim, i.dim_set_var, i.filtered_dims) for i in lhs.inducers]
    

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

def chained_dimension_introductions_example():
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

def chained_dimension_removal_example():
    remove_dim_schema_0 = builtin_schema(1, 1)
    remove_dim_ty_0 = remove_dim_type("fresh0")

    remove_dim_schema_1 = builtin_schema(1, 1)
    remove_dim_ty_1 = remove_dim_type("fresh1")

    b0 = remove_dim_schema_0.instantiate()
    b1 = remove_dim_schema_1.instantiate()
    v0 = Vertex.input()
    v1 = b0.mapping[remove_dim_schema_0.in_vertices[0]]
    v2 = b0.mapping[remove_dim_schema_0.out_vertices[0]]
    v3 = b1.mapping[remove_dim_schema_1.in_vertices[0]]
    v4 = b1.mapping[remove_dim_schema_1.out_vertices[0]]
    v5 = Vertex.output()
    block_schema = BlockSchema([v0], [v5], [b0, b1], {
        v0: [v1],
        v2: [v3],
        v4: [v5],
    })

    #      X1
    #      |
    #    [- fresh0]  
    #      |
    #      Y1
    #      |
    #      X2
    #      |
    #    [- fresh1]
    #      |
    #      Y2
    #
    typing = {
        remove_dim_schema_0: remove_dim_ty_0,
        remove_dim_schema_1: remove_dim_ty_1
    }
    block_type = infer_types(block_schema, typing)

    print(convert_to_graphviz(block_schema))

def parallel_dimension_removal_example():
    X1 = Vertex.input()
    X2 = Vertex.input()
    Y1 = Vertex.output()

    sum_ty = union_type(2)
    remove_a_ty = remove_dim_type('a')
    remove_b_ty = remove_dim_type('b')
    sum_schema = builtin_schema(2, 1)
    remove_a_schema = builtin_schema(1, 1)
    remove_b_schema = builtin_schema(1, 1)

    s0 = sum_schema.instantiate()
    s1 = sum_schema.instantiate()
    a0 = remove_a_schema.instantiate()
    b0 = remove_b_schema.instantiate()

    v0 = s0.mapping[sum_schema.in_vertices[0]]
    v1 = s0.mapping[sum_schema.in_vertices[1]]
    v2 = b0.mapping[remove_b_schema.in_vertices[0]]

    v3 = s0.mapping[sum_schema.out_vertices[0]]
    v4 = b0.mapping[remove_b_schema.out_vertices[0]]

    v5 = a0.mapping[remove_a_schema.in_vertices[0]]
    v6 = a0.mapping[remove_a_schema.out_vertices[0]]

    v7 = s1.mapping[sum_schema.in_vertices[0]]
    v8 = s1.mapping[sum_schema.in_vertices[1]]
    v9 = s1.mapping[sum_schema.out_vertices[0]]

    schema = BlockSchema(
        [X1, X2], [Y1],
        [s0, s1, a0, b0],
        {
            X1: [v0],
            X2: [v1, v2],
            v3: [v5],
            v4: [v8],
            v6: [v7],
            v9: [Y1]
        }
                         )

    typing = {
        sum_schema: sum_ty,
        remove_a_schema: remove_a_ty,
        remove_b_schema: remove_b_ty
    }


    infer_types(schema, typing)
    print(convert_to_graphviz(schema))


parallel_dimension_removal_example()