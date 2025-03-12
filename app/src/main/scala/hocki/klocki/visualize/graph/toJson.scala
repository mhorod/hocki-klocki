package hocki.klocki.visualize.graph

import upickle.default._


def programToJson(program: Program): String = write(program)
