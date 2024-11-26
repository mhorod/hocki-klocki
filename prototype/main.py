from dataclasses import dataclass

from typing import List, Dict

@dataclass
class Shape:
    inputs: List[str]
    outputs: List[str]

@dataclass
class Edge:
    source: str
    target: str


@dataclass
class Dim:
    name: str
    depends_on: List[str]

@dataclass
class BlockDimensions:
    dimensions: List[str] # local dim names, expected in the input
    fresh: List[Dim] # introduced dims and their relation to `dimensions`
    inputs: List[List[str]]
    outputs: List[List[str]]

    def input_dims(self):
        return set(sum(self.inputs, []))

    def output_dims(self):
        return set(sum(self.outputs, []))

@dataclass
class BlockSchema:
    shape: Shape
    parameters: List[Shape]
    edges: List[Edge]
    dimensions: None | BlockDimensions


@dataclass
class Tensor:
    dims: List[int]
    dependencies: List[(int, int)]

@dataclass
class Block:
    schema: BlockSchema
    arguments: List['Block']
    dimensions: Dict[str, int] # local to global map


add_schema = BlockSchema(
    Shape(["x", "y"], ["sum"]),
    [],
    [],
    BlockDimensions([], [], [[], []], [[]])
)

sum_along_schema = BlockSchema(
    Shape(["xs"], ["sum"]),
    [],
    [],
    BlockDimensions(["d"], [], [["d"]], [])
)

sort_schema = BlockSchema(
    Shape(["xs"], ["xs_sorted"]),
    [],
    [],
    BlockDimensions(["d"], [], [["d"]], [["d"]])
)

split_chars_schema = BlockSchema(
    Shape(["string"], ["char"]),
    [],
    [],
    BlockDimensions([], [Dim("d", [])], [[]], [["d"]])
)

class FreshNameGenerator:

    def __init__(self):
        self.next_id = 0

    def fresh(self) -> int:
        result = self.next_id
        self.next_id += 1
        return result


fresh_name_generator = FreshNameGenerator()
split_chars_block = Block(split_chars_schema, [], {"d": fresh_name_generator.fresh()})

# :pepela:
# tego nie przemyślałem a to chyba istotne jest
# D u {x} -> [split_chars]                   ----> E
#                           ---- ... ----> 
# E u {x} -> [split_chars]                   ----> D
# ale to by ta funkcja musiała coś wiedzieć o wymiarach na które ma wyjebongo
# no może mieć dim-set-variable
# to jest niezmiennik który nie zawsze ma sens i nie zawsze się zachowuje
# ale może warto to reprezentować
# ej :kekwait: ale to można dać funkcji różne wymiary na inputach?
# ja myślałem że jak wymiary inputów się różnią to tylko jak explicite powiemy, a reszta ma się zgadzać :thunk:
# :thunk: no to o to trzeba dopytać, ja myślałem że tam mogą być jakiekolwiek wymiary i my tylko podajemy których wymagamy