from collections import defaultdict, namedtuple
import itertools

SAMPLE = [
    ".#.",
    "..#",
    "###",
]

DATA = [
    "...###.#",
    "#.#.##..",
    ".##.##..",
    "..##...#",
    ".###.##.",
    ".#..##..",
    ".....###",
    ".####..#",
]

ACTIVE = '#'
INACTIVE = '.'

class Field(defaultdict):
    RULES = {
        ACTIVE: lambda p, ns: {p: INACTIVE} if ns not in [2,3] else None,
        INACTIVE: lambda p, ns: {p: ACTIVE} if ns == 3 else None,
    }

    def __init__(self):
        super().__init__(lambda: INACTIVE)

    def active(self):
        return [p for p, s in self.items() if s == ACTIVE]

    def points_to_transition(self, get_neighbors):
        tracked = set(self.active())
        return tracked.union(itertools.chain.from_iterable([get_neighbors(point) for point in tracked]))

    def get_transition(self, point, get_neighbors):
        active_neighbors = [p for p in get_neighbors(point) if self[p] == ACTIVE]
        return self.RULES[self[point]](point, len(active_neighbors))

    def get_transitions(self, get_neighbors):
        return [self.get_transition(point, get_neighbors) for point in self.points_to_transition(get_neighbors)]

    def transition(self, transitions):
        for t in [t for t in transitions if t is not None]:
            self.update(t)

    def count_active(self):
        return len(self.active())

Point = namedtuple('Point', 'x y z')

def d(n): return [n-1, n, n+1]

def neighboring(point):
    return [
        Point(dx, dy, dz)
        for dx in d(point.x) for dy in d(point.y) for dz in d(point.z)
        if (dx,dy,dz) != (point.x, point.y, point.z)
    ]

HyperPoint = namedtuple('HyperPoint', 'x y z w')

def hyper_neighboring(point):
    return [
        HyperPoint(dx, dy, dz, dw)
        for dx in d(point.x) for dy in d(point.y) for dz in d(point.z) for dw in d(point.w)
        if (dx,dy,dz,dw) != (point.x, point.y, point.z, point.w)
    ]

if __name__ == "__main__":
    field = Field()

    for x, row in enumerate(DATA):
        for y, value in enumerate(row):
            field[HyperPoint(x,y,0,0)] = value

    print(field.count_active())
    for i in range(0, 6):
        field.transition(field.get_transitions(hyper_neighboring))
        print(i, field.count_active())
