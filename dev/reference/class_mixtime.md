# Base S7 class for mixtime vector objects

`class_mixtime` is the base S7 class for all mixtime vector objects,
inheriting from
[vecvec::class_vecvec](https://pkg.mitchelloharawild.com/vecvec/reference/class_vecvec.html).
While not intended to be used directly, this S7 class is suitable to use
when defining S7 methods for mixtime vectors. S3 methods can be defined
using the
[`mixtime::mixtime`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mixtime.md)
class.

## Usage

``` r
class_mixtime(x = list(), i = seq_len(sum(lengths(x))))
```

## Arguments

- x:

  A list of `"mt_time"` vectors, see
  [`new_time()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_time.md)
  for details.

- i:

  A vector of integers specifying the location of each element in `x` as
  if they were combined in order. The values in `i` must be between 1
  and the total number of elements across all vectors in `x`, and can
  contain duplicates. If not provided, it defaults to a sequence from 1
  to the total number of elements across all vectors in `x`.

## Value

When used as a class definition (e.g., in
`S7::method(generic, class_mixtime)`), an S7 class object representing
the `mixtime` class, inheriting from
[vecvec::class_vecvec](https://pkg.mitchelloharawild.com/vecvec/reference/class_vecvec.html).
When called as a constructor (`class_mixtime(list(...))`), a mixtime
vector of S7 class `mixtime` (also inheriting the S3 class `"mixtime"`),
containing the supplied list of time vectors as a
[vecvec::class_vecvec](https://pkg.mitchelloharawild.com/vecvec/reference/class_vecvec.html)
structure. End users should prefer
[`mixtime()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mixtime.md)
or
[`new_mixtime()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_mixtime.md)
for construction.

## See also

[`mixtime()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/mixtime.md)
for creating mixtime vectors, and
[`new_mixtime()`](https://pkg.mitchelloharawild.com/mixtime/dev/reference/new_mixtime.md)
for the low-level constructor function of this S7 class.
