import json
from textwrap import dedent

from model import *

class IdGenerator:
    def __init__(self):
        self.next_id = 0
    
    def next(self):
        result = self.next_id
        self.next_id += 1
        return result

def json_to_graphviz(path, expansion_depth):
    with open(path, "r") as f:
        obj = json.load(f)
    return program_to_graphviz(Program.from_dto(obj), expansion_depth)

def program_to_graphviz(program: Program, depth: int):
    id_generator = IdGenerator()
    schemata_str = "\n".join(schema_to_graphviz(s, program, depth, id_generator) for s in program.schemata)
    return dedent(f"""\
    digraph G {{ 
        rankdir = TB
        fontname="Courier New"
        node [shape=box, fontname="Courier New"]
        edge [headport=n, tailport=s]
        {schemata_str}
    }}"""
    )

def schema_to_graphviz(
    schema: Schema, 
    program: Program, 
    depth: int, 
    id_generator: IdGenerator,
    interface_mapping=None,
    dim_mapping=None
    ):
    interface_mapping = interface_mapping or {}
    dim_mapping = dim_mapping or {}

    cluster_name = f"cluster_{id_generator.next()}"
    block_title = schema.name.replace("builtin ", "")

    def vertex_name(id):
        return program.dim_set_vars[id]
    
    def dim_name(id):
        return program.dims[id]

    vertex_name_mapping = { 
        v : interface_mapping[v] if v in interface_mapping 
            else f"V_{id_generator.next()}"
        for v in schema.vertices()
     }

    interface_vertices = "\n".join(
        f'{vertex_name_mapping[v]} [label={vertex_name(v)}]' 
        for v in schema.interface.vertices()
    )

    dim_name_mapping = {
        d : dim_mapping[d] if d in dim_mapping
            else dim_name(d)
            for d in schema.interface.dims()
    }
    universal_params = ",".join(dim_name_mapping[d] for d in schema.interface.universal_dims)
    existential_params = ",".join(dim_name_mapping[d] for d in schema.interface.existential_dims)
    generics = f"<{universal_params}|{existential_params}>"

    if len(schema.interface.dims()):
        block_title += generics

    blocks = ""
    edges = ""
    bultin_edges = ""

    edges_drawn = set()

    if depth > 0:
        edges = "\n".join(
            f"{vertex_name_mapping[v]} -> {vertex_name_mapping[u]}"
            for v, u in schema.edges
        )
        for v, u in schema.edges:
            edges_drawn.add((v, u))
        blocks = "\n".join(
            schema_to_graphviz(
                program.get_schema_by_id(block.schema_id),
                program,
                depth - 1,
                id_generator,
                { 
                    v : vertex_name_mapping[block.interface_mapping[v]]
                    for v in block.interface_mapping
                },
                {
                    d : dim_name_mapping[block.dim_mapping[d]] if d in dim_name_mapping
                        else dim_name(block.dim_mapping[d])
                    for d in block.dim_mapping
                }
            )
            for block in schema.blocks
        )
    
    if "builtin" in schema.name:
        bultin_edges = "\n".join(
            f"{vertex_name_mapping[v]} -> {vertex_name_mapping[u]} [style=dashed]"
            for v in schema.interface.in_vertices
            for u in schema.interface.out_vertices
        )
        for v in schema.interface.in_vertices:
            for u in schema.interface.out_vertices:
                edges_drawn.add((v, u))

    structural_edges = "" if depth > 0 else "\n".join(
        f"{vertex_name_mapping[v]} -> {vertex_name_mapping[u]} [style=invis, weight=0]"
        for v in schema.interface.in_vertices
        for u in schema.interface.out_vertices
        if not (v, u) in edges_drawn
    )

    return dedent(f"""\
    subgraph {cluster_name} {{
        graph [label="{block_title}",
            labeljust=l,
            rankdir=TB,
        ]
        {interface_vertices}
        {structural_edges}
        {bultin_edges}
        {blocks}
        {edges}
    }}
    """
    )
