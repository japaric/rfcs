- Feature Name: unsized_structs
- Start Date: (fill me in with today's date, YYYY-MM-DD)
- RFC PR: (leave this empty)
- Rust Issue: (leave this empty)

# Summary

Add an `unsized` keyword that let user create unsized types like `[T]` and `str`. This in turn:

- Increases the usability of the `Deref`/`Index` traits, as they now can return unsized types other
  than `[T]`, `str` and newtypes over them.
- Lets user-defined types reap the ergonomics of "re-borrow semantics".

# Motivation [WIP]

Right now, this is mostly a collection of pain points encountered while developing [linalg]. This
section needs to be re-structured to sound less like complaints and more like a motivation :-).

[linalg]: https://github.com/japaric/linalg.rs

## Limited usability of the `Index`/`Deref` traits

In general, it's not possible to return anything else than `&SizedTy`, `&[T]`, or `&str` (or
newtypes over them) from the `index` functions. This limits the usefulness of the indexing/slicing
syntax. The same problem occurs with the `Deref` trait.

As an example, one may want to use the `Index` trait to emulate the slicing syntax that other
programming languages provide for multi-dimensional arrays:

``` rust
/// Syntax for matrix (2D array) slicing

// Python
row = A[1, :]
col = A[:, 2]
submat = A[:2, 1:3]

// Octave
// (Note: index starts at 1, and ranges are inclusive)
row = A(2, :)
col = A(:, 3)
submat = A(:2, 2:3)

// Rust
let row = &A[(1, ..)];
let col = &A[(.., 2)];
let submat = &A[(..2, 1..3)];
```

However this is not possible due to the signature of the `Index` trait.

``` rust
// Owned matrix. 2D version of `Box<[T]>`
struct Mat<T> { .. }

// An immutably borrowed submatrix. 2D version of `&'a [T]`
struct SubMat<'a, T> { .. }

impl<'a, T> Index<Range<usize>, Range<usize>> for Mat<T> {
    type Output = SubMat<'a, T>;

    // Note that the `'a` lifetime is unconstrained
    fn index<'b>(&'b self, (r, c): (Range<usize>, Range<usize>)) -> &'b SubMat<'a, T> {
        let v = SubMat { .. };

        // Can't return a reference to `v`, because `v` is only valid in the
        // scope of this function
        &v  //~ error
    }
}
```

This would be solved if `SubMat` was an unsized type and `&SubMat` was a fat pointer
(`sizeof(&SubMat) > 1 word`).

``` rust
// `SubMat` is now a 2D version of `[T]`
unsized struct SubMat<T> { .. }

impl<T> Index<Range<usize>, Range<usize>> for SubMat<T> {
    type Output = SubMat<T>;

    fn index<'a>(&'a self, (r, c): (Range<usize>, Range<usize>)) -> &'a SubMat<T> {
        ..
    }
}
```

## Re-borrow semantics

Borrows that outlive their borrowee.

``` rust
fn foo<T>(mut x: [T; 20]) {
    let c = {
        // `a` is a mutable borrow of `x`
        let (a, _) = x.split_at_mut(10);

        // Errors as expected because `x` has "frozen" `a`
        //&x[0];

        // `c` is a mutable borrow of `a`
        let (c, _) = a.split_at_mut(5);

        // Errors as expected because `a` has "frozen" by `c`
        //&a[0];

        // Although `c` is a mutable borrow of `a`, `c` can outlive `a`
        c
    };  // `a` is dropped here

    // OK, valid access because `x` is still "alive"
    &c[0];
}
```

To understand why this is possible, check the signature of `split_at_mut`:

``` rust
fn split_at_mut<'a, T>(x: &'a mut [T], at: usize) -> (&'a mut [T], &'a mut [T]) { .. }
```

This function has some interesting properties:

- `x` is *not* moved into the function
- The returned slices "freeze" `x`.
- The returned slices are not tied to the lifetime of the reference `x`, but instead are valid for
  as long as the original data is valid.

A signature like that is not possible to replicate with sized types:

Version 1:

``` rust
// Mutable version of `SubMat`. 2D version of `&'a mut [T]`
struct SubMatMut<'a, T> { .. }

impl<'a, T> SubMatMut<'a, T> {
    // Splits a sub-matrix horizontally in two mutable parts
    fn hsplit_mut<'b>(&'b mut self, at_row: usize) -> (SubMatMut<'b, T>, SubMatMut<'b, T>) { .. }
}
```

Good:

- `self` is not moved
- `self` gets frozen

Bad:

- Returned submatrices are tied to `self` via `'b`

Other consequences:

Each method call "reduces" the lifetime of the caller, this can result in problems like this one:

``` rust
mat.rows_mut().flat_map(|row| {
    // The mutable iterator is tied to `row` so it can't be returned from this closure
    row.iter_mut()
    //~^ error: `row` does not live long enough
})
```

Or this:

``` rust
fn foo(mut mat: Mat<f32>) {
    let guards = vec![];

    for row in mat.rows_mut() {
        for elem in row.iter_mut() {
            //~^ `row` does not live long enough
            guards.push(thread::scoped(|| { elem; }));
        }

        // `row` dropped here
    }

    mem::drop(guards);
}  // `mat` and its elements are valid up to this point
```

Version 2:

``` rust
impl<'a, T> SubMatMut<'a, T> {
    // Splits a sub-matrix horizontally in two mutable parts
    fn hsplit_mut(self, at_row: usize) -> (SubMatMut<'a, T>, SubMatMut<'a, T>) { .. }
}
```

Good:

- Returned submatrices are valid for as long as the original data is valid

Bad:

- `self` is moved into the function, instead of borrowed

Other consequences: Taking `self` by value makes generic programming hard. You have to implement
traits for `&'a Foo`, your bounds become `where &'a Foo: Trait`, etc. Not to mention that all
method calls end up moving the mutable reference, so you can't use it afterwards. You can use
manual re-borrows like [this] to mitigate the problem but things can get verbose quickly.

[this]: https://github.com/huonw/strided-rs/blob/05648e9afc3f9f4161316d6e6ea60cc14609caff/src/lib.rs#L86

## Other problems

Having `SubMat<'a, T>` and `SubMatMut<'a, T>` instead of `&'a SubMat` and `&'a mut SubMat` results
in more implementation work as you have more types.

If you have type `Mat<T>` that's equivalent to `Box<[T]>`, using smart pointers with said type
results in double indirection: `Rc<Mat<T>>` is like `Rc<Box<[T]>>`. Unsized types can remove the
extra layer of indirection: given an unsized `Mat<T>` with `impl Drop for Mat<T>`, you can have
`Box<Mat<T>>` and `Rc<Mat<T>>` which are equivalent to `Box<[T]>` and `Rc<[T]>` respectively.

# Detailed design

[Detailed design]: #detailed-design

## `unsized`

Add a `unsized` keyword that allows defining "unsized" structs:

``` rust
// An unsized matrix
unsized struct Mat<T> {
    data: *mut T,
    ncols: usize,
    nrows: usize,
}
```

`unsized` structs do not implement the `Sized` trait and can't be used as bare types, but only
behind a pointer like `&` or `Box`.

Just like `&[T]` is a fat pointer with raw representation: `(*mut T, usize)` (exposed as
`std::raw::Slice`). `&Mat` is also a fat pointer with representation `(*mut T, usize, usize)`, this
effectively means that `Mat` is the raw representation of the fat pointers: `&Mat`, `&mut Mat`,
`Box<Mat>`, etc.

``` rust
// On x86_64
mem::size_of::<Mat<f32>>();         //~ error: `Mat` does not implement `Sized`
mem::size_of::<&Mat<f32>>();        // 24 bytes
mem::size_of::<&mut Mat<f32>>();    // 24 bytes
mem::size_of::<*const Mat<f32>>();  // 24 bytes
```

## Requirements

`unsized` structs must fulfill the following requirements:

- All its fields must be `Copy`.
    - Rationale: The raw representation must be `Copy` because the fat pointers: `&Unsized`,
      `*const Unsized` are `Copy`
- All its fields must be `Sized`.
- All its fields must be private (not marked with `pub`).
- The struct must contain at least one raw pointer

NOTE This set of restrictions is not final and may change/grow/shrink as we gain experience with
the feature.

## Construction of fat pointers

Currently, the standard way to create a fat pointer is to `transmute` its "raw" representation.
Check the `std::slice::from_raw_parts` function:

``` rust
unsafe fn from_raw_parts<'a, T>(data: *const T, len: usize) -> &'a [T] {
    use std::{mem, raw};

    mem::transmute(raw::Slice {
        data: data,
        len: len,
    })
}
```

`unsized` structs will use normal struct initialization syntax to create fat pointers. Note that
this operation is unsafe because lifetimes are left unconstrained and the user must ensure that the
pointers are valid/unique/etc.

``` rust
impl<T> Mat<T> {
    // Creates a new owned matrix from an existing owned slice
    fn new(mut elems: Box<[T]>, (nrows, ncols): (usize, usize)) -> Box<Mat<T>> {
        assert!(elems.len() == nrows * ncols);

        let data = elems.as_mut_ptr();

        unsafe {
            // `Mat` destructor will take care of freeing the buffer
            mem::forget(elems);

            // The raw representation gets coerced into `Box<Mat>`
            Mat {
                data: data,
                ncols: ncols,
                nrows: nrows,
            }
        }
    }

    // Reshapes a contiguous slice as a matrix (2D array)
    fn reshape<'a>(elems: &'a [T], (nrows, ncols): (usize, usize)) -> &'a Mat<T> {
        assert!(elems.len() == nrows * ncols);

        unsafe {
            // OK to cast to `*mut T`, because the return type `&Mat` will only
            // allow immutable access to the underlying data
            let data = elems.as_ptr() as *mut T;

            // Coerces the raw representation into `&Mat`
            Mat {
                data: data,
                ncols: ncols,
                nrows: nrows,
            }
        }
    }
}
```

Usual privacy rules apply. Fat pointers can't be created this way from outside the module where the
unsized struct is defined.

``` rust
use mat::Mat;

mod mat {
    pub unsized struct Mat<T> { .. }
}

fn mk_mat<'a, T>() -> &'a Mat<T> {
    unsafe {
        Mat {   //~ error: can't initialize fat pointers outside of the module where they were defined
        }
    }
}
```

## Field access

As long as privacy allows, the fields of a fat pointer can be accessed with the normal field access
syntax.

``` rust
impl<T> Mat<T> {
    // `self` has type `&Mat<T>` and is a fat pointer with repr: `(*mut T, usize, usize)`
    fn size(&self) -> (usize, usize) {
        (self.nrows, self.ncols)
    }
}
```

De-structuring is also allowed

``` rust
impl<T> Drop for Mat<T> {
    fn drop(&mut self) {
        unsafe {
            // This is equivalent to: `Slice { data, len } = self.repr()` where `self: &[T]`
            let Mat { data, nrows, ncols } = self;

            let len = nrows * ncols;

            mem::drop(Vec::from_raw_parts(data, len, len))
        }
    }
}
```

## Potential users in libstd

- `CStr` can be implemented as `unsized struct CStr { ptr: *mut c_char }` resolving the FIXME
  (`sizeof(&CStr) == sizeof(&c_char)`) attached to the struct definition.

- `[T]` and `str` could be implemented as `unsized` structs in libcore and removed from the
  compiler (i.e. remove `ty_str` from the compiler, and modify `ty_vec` to represent only
  fixed-size arrays).

## Feature gate and backward-compatibility

This feature is fully backward-compatible as it allows the creation of new data structures and
increases the flexibility of the `Deref` and `Index` traits.

Converting `CStr` to an `unsized` struct is an implementation detail (the current `CStr` is already
unsized) and it's also a backward-compatible change.

This feature can land in the nightly channel and behind `#![feature(unsized)]` until its deemed
stable.

# Drawbacks

None that I can think of

# Alternatives

## Use a different syntax

If we want to use the reserved `unsized` word for something else, we could "overload" the
`impl !Sized` syntax:

``` rust
struct Mat<T> {
    data: *mut T,
    ncols: usize,
    nrows: usize,
}

// Use this instead of `unsized struct Mat<T> { .. }`
impl<T> !Sized for Mat<T> {}
```

Another option is to use a marker like `std::marker::Unsized`.

# Unresolved questions

- Are there any use cases where an unsized struct doesn't need to contain a raw pointer?
- Should we allow mutation of fat pointers? It seems potentially confusing For example:

``` rust
// This doesn't modify the original fat pointer, because `m` is a copy of the original
fn foo(mut m: &Mat) -> &Mat {
    m.nrows -= 1;
    m
}

let m: &Mat = Mat { .. };
println!("{}", m.nrows);  // 5
let n = foo(m);
println!("{}", m.nrows);  // 5
println!("{}", n.nrows);  // 4
```

Or, in other words, to actually modify the fat pointer you need `&mut &Mat`, which one don't
usually puts in type signatures.

Also, syntax may get weird for methods:

```
impl Mat {
    // How to specify mutability of `self`, which is a fat pointer? `mut &self`?
    fn bar(mut &self) -> &Mat {
        self.ncols -= 1;
        self
    }
}
```
