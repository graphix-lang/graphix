# Recursive Types

Type aliases can be used to define recursive types, and this is a very powerful
modeling tool for repeating structure. If you want to see an advanced example
look no further than the `Tui` type in `graphix-shell`. Tui's are a set of
mutually recursive types that define the tree structure of a UI. For a less
overwhelming example consider a classic,

```graphix
type List<'a> = [
  `Cons('a, List<'a>),
  `Nil
]
```

This defines a singly linked list as a set of two variant cases. Either the list
is empty (nil), or it is a cons cell with a `'a` and a list, which itself could
be either a cons cell or nil. If you've never heard the term "cons" and "nil"
they come from lisp, the original functional programming language from the late
1950s. Anyway, lets define some functions to work on our new list type,

```graphix
type List<'a> = [
  `Cons('a, List<'a>),
  `Nil
];

// cons a new item on the head of the list
let cons = |l: List<'a>, v: 'a| -> List<'a> `Cons(v, l);

// compute the length of the list
let len = |l: List<'a>| {
  let rec len_int = |l: List<'a>, n: i64| select l {
    `Cons(_, tl) => len_int(tl, n + 1),
    `Nil => n
  };
  len_int(l, 0)
};

// map f over the list
let rec map = |l: List<'a>, f: fn('a) -> 'b| -> List<'b> select l {
  `Cons(v, tl) => `Cons(f(v), map(tl, f)),
  `Nil => `Nil
};

// fold f over the list
let rec fold = |l: List<'a>, init: 'b, f: fn('b, 'a) -> 'b| -> 'b select l {
  `Cons(v, tl) => fold(tl, f(init, v), f),
  `Nil => init
}
```

You can probably see where functional programming gets it's (partly deserved)
reputation for being elegant and simple. Lets try them out,

```graphix
let l = cons(cons(cons(cons(`Nil, 1), 2), 3), 4);
l
```

running this we get,

```
$ graphix test.gx
`Cons(4, `Cons(3, `Cons(2, `Cons(1, `Nil))))
```

Lets try something more complex,

```graphix
map(l, |x| x * x)
```

results in

```
$ graphix test.gx
`Cons(16, `Cons(9, `Cons(4, `Cons(1, `Nil))))
```

as expected. Finally lets sum the list with fold,

```graphix
fold(l, 0, |acc, v| acc + v)
```

and as expected we get,

```
$ graphix test.gx
10
```

So with recursive types and recursive functions you can do some really powerful
things. When you add these capabilities to the data flow nature of Graphix, it
only multiplies the power even further.
