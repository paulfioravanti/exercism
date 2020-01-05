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


def color_code(color: str):
    return __COLORS.index(color)


def colors() -> List[str]:
    return __COLORS
