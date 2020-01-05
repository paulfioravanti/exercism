from typing import List

__COLORS = [
    "black",
    "brown",
    "red",
    "orange",
    "yellow",
    "green",
    "blue",
    "violet",
    "grey",
    "white"
]


def value(colors: List[str]) -> int:
    return __COLORS.index(colors[0]) * 10 + __COLORS.index(colors[1])
