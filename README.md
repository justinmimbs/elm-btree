# elm-btree

An alternative implementation of the core library `Dict`, as a B-tree of order
4 (a 2-3-4 tree).

```elm
import Dict.BTree as Dict
```

`Dict.BTree.fromList` can convert lists with sorted, distinct keys to dicts
quickly (over 50x faster than the core library `Dict.fromList` in the
benchmarks below), which it leverages for faster transform and combine
operations.

## Benchmarks

This table compares `Dict.BTree` to the core library `Dict`. These results
were run in Chrome, using dicts of 100 elements with ints as keys.

|    | _(ops/sec)_ Dict | _(ops/sec)_ Dict.BTree | Scale |
|:---|-----------------:|-----------------------:|------:|
| __Build__ |  |  |  |
| `fromList` (sorted) | 4,993 | 274,347 | 54.94 |
| `fromList` (almost sorted) | 5,040 | 132,047 | 26.20 |
| `fromList` (half sorted) | 5,182 | 19,744 | 3.81 |
| `fromList` (unsorted) | 4,878 | 10,744 | 2.20 |
| `insert` (from empty) | 4,747 | 8,716 | 1.84 |
| __Query__ |  |  |  |
| `get` | 39,109 | 49,091 | 1.26 |
| __Modify__ |  |  |  |
| `insert` (doubling size) | 7,396 | 9,588 | 1.30 |
| `remove` (until empty) | 8,321 | 12,083 | 1.45 |
| `remove` (each key once) | 5,624 | 12,033 | 2.14 |
| `update` (insert, doubling size) | 7,903 | 8,504 | 1.08 |
| `update` (remove, until empty) | 7,912 | 10,020 | 1.27 |
| `update` (replace all values) | 13,756 | 14,860 | 1.08 |
| `update` (no change) | 11,948 | 13,131 | 1.10 |
| __Transform__ |  |  |  |
| `filter` | 10,532 | 120,166 | 11.41 |
| `partition` | 5,520 | 99,467 | 18.02 |
| __Combine__ (fragmented intersections) |  |  |  |
| `union` | 4,210 | 30,847 | 7.33 |
| `intersect` | 5,996 | 41,374 | 6.90 |
| `diff` | 5,477 | 37,602 | 6.86 |
| __Combine__ (contiguous intersection) |  |  |  |
| `union` | 3,229 | 34,850 | 10.79 |
| `intersect` | 4,260 | 45,066 | 10.58 |
| `diff` | 4,856 | 43,881 | 9.04 |
| __Combine__ (disjoint ranges) |  |  |  |
| `union` | 2,062 | 30,189 | 14.64 |
| `intersect` | 17,455 | 1,666,587 | 95.48 |
| `diff` | 6,172 | 1,658,602 | 268.75 |
| __Combine__ (with one empty) |  |  |  |
| `union` | 5,018 | 17,957,185 | 3,578.53 |
| `intersect` | 130,866 | 3,072,202 | 23.48 |
| `diff` | 48,471 | 3,050,227 | 62.93 |
