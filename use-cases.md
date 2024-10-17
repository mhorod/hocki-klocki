### Examples / Use cases

```
# scalar function
# dim: D => D => D
def add(x: Int, y: Int) -> Int

# function taking vector and reducing it to a scalar 
# dim: D + {d} => D
def sum_along[d](xs: Int{d}) -> Int

# function taking vector and returing a vector
# dim: D -> D
def sort[d](xs: Int{d}) -> Int{d}

# function introducing fresh dimension as a result
# dim: D => D + {x} with x -> D
def split_chars(s: String) -> Char{fresh}

# for each element return smaller ones - this requires a new dimension, different for each index in d
# dim: D + {d} => D + {d, x} with x -> D + {d}
def smaller_elements{d}(xs: Int{d}) -> Int{d, fresh x} with x -> d

# for each element return (sum * x, sum * (x + 1), sum * (x+2), ..., sum * (x + log sum)) 
# dim: D + {d} => D + {d, x} but x is independent of d (it's the same x for all elements)
def multiplied_sum{d}(xs: Int{d}) -> Int{d, fresh}

# matrix multiplication - takes multidimensional values and reduces one dimension
# dim: D + {a, b} => D + {b, c} => D + {a, c}
def matmul{a, b, c}(x: Int{a, b}, y: Int{b, c}) -> Int{a, c}

# joins different dimensions together 
# dim: D + {d1} => D + {d2} => D + {d1, d2}
def cross[T, U]{d1, d2}(xs: T{d1}, ys: U{d2}) -> (T, U){d1, d2}

# repeats value along provided dimension assuming it's not present in the input
# dim: D => D + {d}
def repeat[T]{d}(x: T) -> T{d}
```


### Representation

- List of dimensions expected on the input (inputs)
- dimensions present in each input
- dimensions present in the output
- dependencies between fresh dimensions and input


### Dependent dimensions

Since we can introduce dimensions that depend on other, we need to prepare to handle any dependencies in the input.

We don't introduce dependencies in existing dimensions so there should never be a cycle.

How does reducing dimensions that are not minimal work?
It probably shouldn't be allowed since all dependent dimensions are incompatible with each other.
However, if we reduce all dependent dimensions as well then it seems ok.

e.g.
```
def f[T]{x, y}(m: T{x, y}) -> T
```
would work even for `y -> x` and `x -> y`, but not for some other `z -> x` or `z -> y`


### Composing dimensions

What happens if we provide different dimensions on each input?

```
def f(a: Int, b: Int, c: Int) -> Int

f(A: Int{x}, B: Int{x, y}, C: Int{y, z}): Int{x, y, z} = T

T[x, y, z] = f(A[x], B[x, y], C[y, z]) ?
```

What if function expects a vector?
```
def f{d}(a: Int{d}, b: Int) -> Int{d}

f{x}(A: Int{x}, B: {y, z}): Int{x, y, z} = T

T[y, z] = f(A, B[y, z])
```

Case with different dimensions:
```
def f{d}(a: Int{d}, b: Int) -> Int{d}

f{x}(A: Int{x, z}, B: {y, z}): Int{x, y, z} = T

T[x, y, z] = f(A[z], B[y, z])[x]
```

Is this legal? Does explicitly saying `a` that is a vector disallow using `d` in other inputs?
```
def f{d}(a: Int{d}, b: Int) -> Int{d}

f{x}(A: Int{x, z}, B: {x, y, z}): Int{x, y, z} = T

T[x, y, z] = f(A[z], B[x, y, z])[x]
```


```
def left(a: Int, b: Int): Int = a

# does it lift like this one:
left(A{x, y, z}, B{u, v, w}): Int{x, y, z, u, v, w} # ??? 
# or this one:
left(A{x, y, z}, B{u, v, w}): Int{x, y, z} # ???
```