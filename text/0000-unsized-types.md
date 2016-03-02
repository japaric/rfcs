- Feature Name: unsized_types
- Start Date: (fill me in with today's date, YYYY-MM-DD)
- RFC PR: (leave this empty)
- Rust Issue: (leave this empty)

**WARNING** This is a ~~draft~~ braindump I wrote like one year ago.

# Summary

> One para explanation of the feature.

Let users define their own "unsized types" (a.k.a. Dynamically Sized Types or DST), like `[T]`.

# Motivation

## Increase the usability of the `Index`/`Deref` traits

In general, it's not possible to return anything else than `&SizedTy`, `&[T]`, or `&str` (or
newtypes over them) from the `index` function. This limits the usefulness of the indexing/slicing
syntax. The same problem occurs with the `Deref` trait.

As an example, one may want to use the `Index` trait to emulate the indexing/slicing syntax that
other programming languages provide for multi-dimensional arrays:

``` rust
// Syntax for matrix (2D array) slicing/indexing

// Python
second_row = A[1, :]
third_column = A[:, 2]
sub_matrix = A[:2, 1:3]

// Rust
let second_row = &A[(1, ..)];
let third_column = &A[(.., 2)];
let sub_matrix = &A[(..2, 1..3)];
```

However, as shown below, it's only possible to implement the row indexing, but not the column
indexing or the sub-matrix slicing:

``` rust
// An owned matrix stored in contiguous memory. (`OwnedMat` is equivalent to `Box<[T]>`)
struct OwnedMat<T> {
    data: *mut T,
    nrows: usize,
    ncols: usize,
}

impl<T> Drop for OwnedMat<T> { .. }

// A row vector
struct Row<T>([T]);

impl<'a, T> Index<(usize, RangeFull)> for OwnedMat<T> {
    type Output = Row<T>;

    fn index<'s>(&'s self, (r, _): (usize, RangeFull)) -> &'s Row<T> {
        let &OwnedMat { data, nrows, ncols } = self;

        assert!(r < nrows);

        unsafe {
            mem::transmute(slice::from_raw_parts(data.offset((r * ncols) as isize), ncols))
        }
    }
}
```

Indexing works here, because `Row` is unsized, and `&Row` is a fat pointer that holds enough data,
a pointer and a length, to represent the row, which is a (contiguous) slice.

``` rust
// A column vector. (`Col<'a, T>` is equivalent to a strided version of `&'a [T]`)
struct Col<'a, T: 'a> {
    data: *mut T,
    len: usize,
    stride: usize,
    _marker: PhantomData<&'a T>,
}

impl<'a, T> Index<(RangeFull, usize)> for OwnedMat<T> {
    type Output = Col<'a, T>;

    // Note that `'a` is unconstrained!
    fn index<'s>(&'s self, (_, c): (RangeFull, usize)) -> &'s Col<'a, T> {
        let &OwnedMat { data, nrows, ncols } = self;

        assert!(c < ncols);

        Col { data: data.offset(c as isize), len: nrows, stride: ncols, _marker: PhantomData }
        //~^ error: expected `&Col`, got `Col`
    }
}
```

Here we actually want to return `Col<'s, T>` (a strided slice) but the function signature demands a
reference (a single pointer).

---

This proposal would solve the problem by letting the user define `Col<T>` as an unsized type, where
`&Col<T>` is a fat pointer with representation: `(*T, usize, usize)`. To draw a parallel, this
is exactly the same as: `[T]` being an unsized type, where `&[T]` is a fat pointer with
representation: `(*T, usize)` (see `std::raw::Slice`).

Then the last `Index` implementation would look like this:

``` rust
impl<T> Index<(RangeFull, usize)> for Mat<T> {
    type Output = Col<T>;

    // Note that the unconstrained `'a` is gone
    fn index<'s>(&'s self, (_, c): (RangeFull, usize)) -> &'s Col<T> {
        let &Mat { data, nrows, ncols } = self;

        assert!(c < ncols);

        // This is the same as `mem::transmute(raw::Slice { data: data, len: len })` which is
        // what `slice::from_raw_parts(data, len)` does
        unsafe {
            mem::transmute(raw::Col { data: data.offset(c as isize), len: nrows, stride: ncols })
        }
    }
}
```

## Re-borrow semantics

Some functions that work with fat `&-` pointers have re-borrow semantics that can't be reproduced
with equivalent structs. An example is the `[T]::split_at_mut` method:

```
fn split_at_mut<'s>(&'s mut self, at: usize) -> (&'s mut [T], &'s mut [T]) { .. }
```

This function has three interesting properties:

- The caller is *not* consumed
- The caller gets "frozen" until the returned slices go out of scope.
- The returned slices are valid for as long as the original data (which has lifetime `'s`).

It's easier to spot these properties with an example:

``` rust
let mut array: [i32; 3] = [0, 1, 2];

let first = {
    let slice: &mut [i32] = &mut array[..];

    {
        let (left, right) = slice.split_at_mut(1);

        // 2) `slice` got frozen
        //slice.len();
        //~^ error: cannot borrow `slice` as immutable because it is also borrowed as mutable
    }

    // 1) `slice` was not consumed in the previous call, so it can be used again
    let (left, right) = slice.split_at_mut(2);

    let (first, second) = left.split_at_mut(1);

    // 3) Although `first` came from `left`, and `left` came from `slice`, `first` can outlive both
    // slices because it's valid for `'array` (the lifetime of `array`)
    first
};
```

To illustrate why these properties can't be emulated with structs, we'll try to implemented this
method on `ColMut<'a, T>`, a mutable version of the `Col` struct we used before, with three
different approaches.

``` rust
// A "mutable" column vector. (`ColMut<'a, T>` is equivalent to a strided version of `&'a mut [T]`)
struct ColMut<'a, T: 'a> {
    data: *mut T,
    len: usize,
    stride: usize,
    _marker: PhantomData<&'a mut T>,
}
```

### Approach 1: Take `&mut self`

``` rust
impl<'a, T> ColMut<'a, T> {
    fn split_at_mut<'s>(&'s mut self, at: usize) -> (ColMut<'s, T>, ColMut<'s, T>) { .. }
}
```

This method doesn't consume the caller, and it properly freezes it, but the returned references
are only valid for the lifetime of `self`: `'s` rather than for `'a` -- the type system effectively
"forgot" for how long the original data is valid for.

This negatively affects composability because the returned vector can't outlive `self`:

``` rust
fn first_half<'a, T>(mut z: ColMut<'a, T>) -> ColMut<'a, T> {
    let n = z.len();
    let (left, right) = z.split_at_mut(n / 2);
    left  //~ error: `left` does not live long enough
}
```

Whereas, that function would have just worked with slices:

``` rust
fn first_half<'a, T>(z: &'a mut [T]) -> &'a mut [T] {
    let n = z.len();
    let (left, right) = z.split_at_mut(n / 2);
    left  // OK
}
```

### Approach 2: Take `&mut self`, but tweak the lifetimes

``` rust
impl<'a, T> ColMut<'a, T> {
    fn split_at_mut<'s>(&'s mut self, at: usize) -> (ColMut<'a, T>, ColMut<'a, T>) { .. }
}
```

This doesn't consume the caller, and the returned slices have the right lifetime parameter. But
method doesn't actually freeze the caller, and it's actually a **safety hole** because it allows
aliasing mutable memory:

```
fn alias_first_half<'a>(mut z: ColMut<'a, T>) -> (ColMut<'a, T>, ColMut<'a, T>) {
    let n = z.len();
    let (a, b) = z.split_at_mut(n / 2);
    let (c, d) = z.split_at_mut(n / 2);  // !!!

    // `a` and `c` provide mutable access to the same memory
    (a, c)
}
```

### Approach 3: Take `self`

``` rust
impl<'a, T> ColMut<'a, T> {
    fn split_at_mut(self, at: usize) -> (ColMut<'a, T>, ColMut<'a, T>) { .. }
}
```

The returned vectors have the right lifetime, but the caller gets consumed, so it can't be used
afterwards.

```
let col: ColMut<i32> = { .. };

{
    let (left, right) = col.split_at_mut(5);
    //~^ help: `col` moved here
}

println!("{:?}", col);  //~ error: use of moved value: `col`
```

The move problem can be addressed with explicit re-borrows:

```
impl<'a, T> ColMut<'a, T> {
    // Note that lifetime `'a` is "lost" in the process
    fn reborrow_mut<'b>(&'b mut self) -> ColMut<'b, T> { .. }
}

let col: ColMut<i32> = { .. };

{
    let (left, right) = col.reborrow_mut().split_at_mut(5);
}

println!("{:?}", col);  // OK
```

However these explicit re-borrows have the same problem that the first approach -- they lose
the real lifetime of the data.

---

Again, this RFC would solve the issue by letting the user define `Col` as an unsized type, just
like before, and then implement the method directly on the unsized type.

``` rust
impl<T> Col<T> {
    fn split_at_mut<'a>(&'a mut self, at: usize) -> (&'a mut Col<T>, &'a mut Col<T>) { .. }
}
```

This implementation has the same properties as `[T]::split_at_mut`.

## Re-borrow ergonomics

Fat `&-` pointers are ergonomic to use because they get implicitly re-borrowed at function call
sites:

``` rust
fn foo<T>(_: &[T]) {}
fn foo_mut<T>(_: &mut [T]) {}

fn test<T>(z: &mut [T]) {
    foo(z);  // `z` is re-borrowed as `&[T]` here. This call is equivalent to `foo(&*z)`
    foo_mut(z);  // `z` is not consumed here, instead is re-borrowed
    foo_mut(z);  // `z` can be used again here
}
```

Whereas the equivalent struct is not because it requires explicit re-borrows to avoid being
consumed by functions:

``` rust
impl<'a, T> ColMut<'a, T> {
    fn reborrow<'s>(&'s self) -> ColMut<'s, T> { .. }
}

fn foo<T>(_: Col<T>) {}
fn foo_mut<T>(_: ColMut<T>) {}

fn test<'z, T>(z: ColMut<'z, T>) {
    //foo(z);  //~ error: wrong type, expected `Col`, got `ColMut`
    foo(z.reborrow());
    //~^ We have to explicitly reborrow, and the lifetime `'z` is lost in the process

    foo_mut(z);  //~ `z` is moved here
    foo_mut(z);  //~ error: use of moved value `z`
}
```

Again, if `Col<T>` was defined as an unsized type instead of as a struct, it would enjoy of these
re-borrow ergonomics.

## Less implementation work

Having different structs to represent the immutable and mutable versions of the same data structure
requires double implementation work as one must implement by-ref methods on both structs:

``` rust
impl<'a, T> Col<'a, T> {
    fn len(&self) -> usize { .. }
}

impl<'a, T> ColMut<'a, T> {
    fn len(&self) -> usize { .. }
}
```

On the other hand, if `Col` was unsized one would need to implement the `len` method once:

``` rust
impl<T> Col<T> {
    fn len(&self) -> usize { .. }
}
```

And it would work with `&Col` and with `&mut Col`.

It seems one might be able to avoid the extra work by having `ColMut<'a, T>` `deref` to
`Col<'a, T>` like this:

``` rust
struct Col<'a, T> {
    data: *mut T,
    len: usize,
    stride: usize,
    _marker: PhantomData<&'a T>,
}

impl<'a, T> Clone for Col<'a, T> { .. }
impl<'a, T> Copy for Col<'a, T> {}

// `ColMut` and `Col` have the same memory layout
struct ColMut<'a, T>(Col<'a, T>);

impl<'a, T> Deref for ColMut<'a, T> {
    type Target = Col<'a, T>;

    fn deref(&self) -> &Col<'a, T> {
        &self.0
    }
}

impl<'a, T> Col<'a, T> {
    fn len(&self) -> usize { .. }
}

let col_mut: ColMut<i32> = { .. };
let col: Col<i32> = { .. };

col_mut.len();  // OK
col.len();  // OK
```

But this is actually a **safety hole** because it lets you create immutable and mutable references
to the same piece of memory:

``` rust
let col_mut: ColMut<i32> = { .. };
let col: Col<i32> = *col_mut;  // !!! Doesn't freeze `col_mut`

col.foo();  // Immutable access
col_mut.foo_mut();  // Can still mutate the underlying data
```

# Detailed design

NOTE: The semantics of unsized types are the same as of built-in "DST". Conceptually, you can think
of `[T]`, `str` and `Trait` as unsized types that have been defined inside the compiler.

## Syntax

Unsized types are declared using the following syntax:

``` rust
unsized type Ty<A: Bound, B, C=Foo> where B: Bound;
```

The grammar of an `unsized type` item is the same as the grammar of a field-less `struct` item,
modulo the initial keyword(s).

## `Sized`ness

An unsized type does not implement the `Sized` trait, but it can become `Sized` if it appears
behind a pointer:

``` rust
unsized type Slice<T>;

fn bad(x: Slice<i32>, y: [i32]) {}
//~^ error: the trait `Sized` is not implemented for the type `Slice<i32>`
//~^ error: the trait `Sized` is not implemented for the type `[i32]`

fn good(x: &Slice<i32>, y: &[i32]) {}
```

## Fat pointers

A pointer to an unsized type is a "fat" pointer. A fat pointer is a two field struct, where the
first field is the *data* pointer itself, and the second field carries extra *information*.

The representation of fat pointers will be exposed in the `std::raw` module:

``` rust
// Defined in the `core` crate, re-exported in the `std` crate
pub mod raw {
    // The representation of a fat pointer
    pub struct FatPtr<Data, Info> {
        /// The data pointer
        pub data: *mut Data,
        /// Extra information
        pub info: Info,
    }
}
```

To illustrate, `&[T]` is a fat pointer where the extra information field is the length of the
slice. `&[T]` is represented as `FatPtr<T, usize>` in memory.

## The `Unsized` trait

An unsized type will be "connected" to its fat pointer representation via the `Unsized` trait:

``` rust
// Defined in the `core` crate, re-exported in the `std` crate
pub mod marker {
    /// An unsized type
    #[lang = "unsized"]
    pub trait Unsized {
        /// The type of the data the fat pointer points to
        type Data: Sized;

        /// Extra information
        type Info: Copy + Sized;

        // The rest of the definition will be shown later
    }
}
```

The representation of the fat pointers: `*const T`, `&mut T`, `Box<T>` where `T: Unsized` is
`FatPtr<T::Data, T::Info>`.

As an example, we can re-implement `[T]`:

``` rust
/// A slice, like `[T]`, but implemented outside the compiler
unsized type Slice<T>;

impl<T> Unsized for Slice<T> {
    /// The type of the elements contained in the slice
    type Data = T;

    /// The length of the slice
    type Info = usize;

    // ..
}

mem::size_of::<usize>();             // 8

// All these have the same layout in memory
mem::size_of::<FatPtr<T, usize>>();  // 16
mem::size_of::<&Slice<i32>>();       // 16
mem::size_of::<*mut Slice<i32>>();   // 16
mem::size_of::<Box<Slice<i32>>>();   // 16
```

## Construction of fat pointers

We provide safe functions to construct a fat pointer `*mut T` from its `FatPtr` representation and
vice versa. The `Unsized` trait is used as a bound to provide type safety:

``` rust
// Defined in `core` crate, re-exported in the `std` crate
mod fat_ptr {
    /// Creates a fat pointer from its "raw" representation
    pub fn new<T: Unsized>(repr: FatPtr<Self::Data, Self::Info>) -> *mut T {
        ..
    }

    /// Returns the "raw" representation of a fat pointer
    pub fn repr<T: Unsized>(fat_ptr: &T) -> FatPtr<Self::Data, Self::Info> {
        ..
    }
}
```

An example, below:

``` rust
/// A matrix stored in contiguous memory
unsized type Mat<T>;

impl<T> Unsized for Mat<T> {
    /// The type of the elements contained in the matrix
    type Data = T;

    /// The dimensions of the matrix: `(nrows, ncols)`
    type Info = (usize, usize);

    ..
}

impl<T> Mat<T> {
    /// Reshapes a contiguous slice into a matrix with dimensions `(nrows, ncols)`
    ///
    ///                                [0, 1, 2]
    /// [0, 1, 2, 3, 4, 5, 6, 7, 8] -> [3, 4, 5]
    ///                                [6, 7, 8]
    ///                                         (3x3)
    pub fn reshape<'a>(slice: &'a [T], (nrows, ncols): (usize, usize)) -> &'a Mat<T> {
        assert_eq!(slice.len(), nrows * ncols);

        let data = slice.as_ptr() as *mut T;
        let info = (nrows, ncols);

        // It's safe to construct a "raw" fat pointer
        let fat_ptr: *mut Mat<T> = fat_ptr::new(FatPtr { data: data, info: info });

        // Unsafe: you are asserting that the fat pointer will be valid for the lifetime `'a`
        unsafe {
            &*fat_ptr
        }
    }

    // Convenience method to get the `FatPtr` representation
    fn repr(&self) -> FatPtr<T, (usize, usize)> {
        fat_ptr::repr(self)
    }

    /// Returns the number of rows of the matrix
    pub fn nrows(&self) -> usize {
        self.repr().info.0
    }

    /// Returns the number of columns of the matrix
    pub fn ncols(&self) -> usize {
        self.repr().info.1
    }
}
```

## Well-formedness

All unsized types must implement the `Unsized` trait, and the `Unsized` trait can only be
implemented by unsized types.

`Unsized` implementations, just like `Drop` implementations, can't be specialized; a single
implementation must cover all the instances of the unsized type. In practice, this means that the
implementation must carry the same type parameters and bounds that the unsized type definition
does:

``` rust
unsized type Good<T: Copy>;

impl<T: Copy> Unsized for Good<T> { .. }

unsized type Bad<T>;

impl<T: Copy> Unsized for Bad<T> { .. }
//~^ error: The requirement `T: Copy` is added only by the Unsized impl.

impl Unsized for Bad<Vec<i32>> { .. }
//~^ error: Implementations of Unsized cannot be specialized
```

## `size_of_val`, `align_of_val`, and destructors

Currently, `Box` doesn't implement the `Drop` trait, instead its destructor is hard coded inside
the compiler. We'll remove the hard coded implementation and replace it with the following
(equivalent) `Drop` implementation: (which is very similar to `Rc`'s `Drop` implementation)

``` rust
impl<T: ?Sized> Drop for Box<T> {
    fn drop(&mut self) {
        unsafe {
            let ptr: *mut T = mem::transmute_copy(self);

            if !(ptr as *const ()).is_null() && ptr as *const() as usize != mem::POST_DROP_USIZE {
                // Drop the box contents. In other words, given `Box<Foo>`, this calls `Foo`'s
                // destructor, if it has one
                intrinsics::drop_in_place(ptr);

                let size = mem::size_of_val(&*ptr);

                if size != 0 {  // <-- (LLVM will optimize away this block for zero sized types)
                    // Deallocate the box itself
                    heap::deallocate(ptr as *mut u8, size, mem::align_of_val(&*ptr));
                }
            }
        }
    }
}
```

To support destructors on `Box`ed (or `Rc`ed, etc) unsized types, we'll need to define the
semantics of the `size_of_val` and the `align_of_val` intrinsics for it. These will be provided by
the `Unsized` implementation, here's the full definition of the `Unsized` trait:

``` rust
trait Unsized {
    type Data;
    type Info;

    /// The definition of the `align_of_val` intrinsic
    fn align_of_val(Self::Info) -> usize {
        mem::align_of::<Self::Data>()
    }

    /// The definition of the `size_of_val` intrinsic
    fn size_of_val(Self::Info) -> usize;
}
```

To illustrate the mechanics, here's an example with `Box<Slice<T>>`:

``` rust
impl<T> Unsized for Slice<T> {
    type Data = T;
    type Info = usize;

    fn size_of_val(len: usize) -> usize {
        len * mem::size_of::<T>()
    }
}

impl<T> Slice<T> {
    /// Creates an owned slice of length `n`, where each of its element is a clone of `elem`
    pub fn from_elem(n: usize, elem: T) -> Box<Slice<T>> where T: Clone {
        let mut v: Vec<_> = iter::repeat(elem).take(n).collect();

        let data = v.as_mut_ptr();
        let len = v.len();

        mem::forget(v);

        unsafe {
            Box::from_raw(fat_ptr::new(FatPtr { data: data, info: len }))
        }
    }
}

mem::drop(Slice::from_elem(3, 0)); // OK
mem::drop(Slice::from_elem(3, Box::new(0)));  // Oops, leaks 3 boxed integers
```

The `Drop` implementation of `Box` will deallocate the owned slice, but it will not touch its
elements. Dropping each element must be handled by `Slice`s destructor:

``` rust
// (`[T]` has a similar destructor hard-coded in the compiler)
impl<T> Drop for Slice<T> {
    fn drop(&mut self) {
        for x in self.iter() {
            // "Drop" each element of the slice
            ptr::read(x);
        }
    }
}

mem::drop(Slice::from_elem(3, Box::new(0)));  // OK!
```

A final note, implementing `Drop` on an unsized type does *not* add a drop flag to its fat pointer
representation struct, however it's *not* necessary to check for the "filling drop" pattern in its
`Drop` implementation, because the `Box`/smart pointer destructor will take care of checking it
before calling the unsized type destructor.

## Other DST features

The only feature that custom unsized types are missing (for now) is unsizing coercions. This RFC
doesn't commit to a design to expose the mechanism, but it can be added afterwards in a backward
compatible way (see Alternatives section, for a possible implementation). Other than that, all the
features available on built-in DST are also available on custom unsized types; to name a few:

- Cast from fat raw pointers to thin raw pointers: `*const [T] -> *const T`
- Unsized structs, the last field of a struct can be a built-in DST or a custom unsized type.
- Null-pointer enum optimization: Both `Slice<T>`, and `Option<Slice<T>>` are two words in size.

Not having unsizing coercions sounds like one won't be able to use custom unsized types with smart
pointers like `Rc` or `Arc` -- that's not correct, layout-wise custom unsized types already work
with smart pointers, what's missing are library routines to e.g create an `Rc<T>` from `Box<T>` for
`T: ?Sized`.

``` rust
// Unsafe magic, don't try this at home

let z: Box<[usize]> = Box::new([1, 1, 3, 5, 7]);
//                              ~~~~ strong and weak count
let x: Rc<Slice<usize>> = unsafe {
    let data = z.as_ptr();
    let len = 3;
    mem::forget(z);
    mem::transmute(slice::from_raw_parts(data, len))
};
println!("{:?}", x);  // [3, 5, 7]
mem::drop(x);  // valgrind: :-)
```

## Potential users in `std`

This section shows possible uses of unsized types in the `std` library, however this RFC doesn't
commit to their implementation. The goal of this section is to make other subteams
(compiler/library) aware of the possibilities.

### `CStr`

The `CStr` type is currently defined as a newtype over `[u8]`, and creating a `&CStr` from a C
string (`CStr::from_ptr`) requires linear time because it involves calculating the length of the C
string. This runtime cost can be eliminated if `CStr` is re-implemented as an unsized type. There
are a few possible re-implementations with different trade-offs, here's one of them:

``` rust
/// Representation of a borrowed C string
unsized type CStr;

impl Unsized for CStr {
    type Data = c_char;
    /// The length of the C string
    type Info = Option<NonZero<usize>>;

    /// NOTE: This function returns `0` when the length of the C string is not known by the fat
    /// pointer.
    fn size_of_val(len: Option<NonZero<usize>>) -> usize {
        mem::size_::<c_char>() * len.map(|nz| *nz).unwrap_or(0)
    }
}

impl CStr {
    // Constructing `&CStr` is now zero-cost rather than `O(N)`
    pub unsafe fn from_ptr<'a>(ptr: *const c_char) -> &'a CStr {
        &*fat_ptr::new(FatPtr { data: ptr, info: None) })
    }

    pub fn as_ptr(&self) -> *const c_char {
        self.repr().data
    }

    pub fn to_bytes(&self) -> &[u8] {
        unsafe {
            slice::from_raw_parts(self.as_ptr(), self.len() - 1)
        }
    }

    pub fn to_bytes_with_nul(&self) -> &[u8] {
        unsafe {
            slice::from_raw_parts(self.as_ptr(), self.len())
        }
    }

    /// Returns the length of the C string.
    // This is zero-cost when used via a `CString` deref, and `O(N)` when used on a `CStr` created
    // via `from_ptr`.
    fn len(&self) -> usize {
        let FatPtr { data, info: len } = self.repr();

        len.map(|nz| *nz).unwrap_or_else(|| unsafe {
            libc::strlen(data) as usize + 1
        })
    }

    fn repr(&self) -> FatPtr<T, Option<NonZero<usize>>> {
        fat_ptr::repr(self)
    }
}

/// A type representing an owned C-compatible string
struct CString(Box<[u8]>);

// `deref`-ing a `CString` to `CStr`, preserves the information about the length of the string
impl Deref for CString {
    type Target = CStr;

    fn deref(&self) -> &CStr {
        let data = self.0.as_ptr() as *mut c_char;
        let len = self.0.len();

        unsafe {
            &*fat_ptr::new(FatPtr { data: data, info: Some(NonZero::new(len)) })
        }
    }
}
```

Another option is to not carry extra information (`type Info = ()`). This makes `&CStr` smaller (1
word rather than 2), but then `deref`ing `CString` to `CStr` would lose the information about the
length of the string.

### `[T]`

As shown in several examples throughout this document, it's possible to re-implement the built-in
slice type (`[T]`) as an unsized type in a library. However, a such re-implementation would be
missing features like slice patterns in `match` expressions, unsizing coercions, etc.

``` rust
#[lang = "slice"]
unsized type Slice<T>;
```

``` rust
type [T] = Slice<T>;
```

What can be moved out of the compiler:

- The `TySlice` "type variant"
- `[T]` built-in destructor can be replaced with a `Drop` implementation

What will remain in the compiler:

- The `[T]` name
- Unsizing coercions.
- Slice patterns in `match` expressions.

## More elaborated examples

### Dense matrix with Python/Octave/Julia-like slicing syntax

### Sparse matrix

### Managing GPU memory

# Drawbacks

> Why should we *not* do this?

To stabilize this feature, we'll have to stabilize the current fat pointer ABI, and stabilize
things that used to be implementation details.

# Alternatives

> What other designs have been considered?

## Unsizing coercions (possible implementation)

This RFC doesn't commit to a design to expose the unsizing coercion mechanism, but here's a
possible design:

``` rust
trait UnsizeFrom<Sized>: Unsized {
    fn unsized_info() -> Self::Info;
}
```

``` rust
impl<T, usize N> UnsizeFrom<[T; N]> for Slice<T> {
    fn unsized_info() -> usize {
        N
    }
}
```

``` rust
/// Coerces a thin pointer `&SizedTy` to a fat pointer `&UnsizedTy`
fn coerce_ref<S: Sized, U: Unsized>(x: &S) -> &U {
    let data = x as *const S as *const U::Data;
    let info: U::Info = UnsizedFrom<S>::unsized_info();

    unsafe {
        &*fat_ptr::new(FatPtr { data: data, info: info })
    }
}
```

``` rust
let array: [i32; 4] = [0, 1, 2, 3];
let slice: &Slice<i32> = &array;
```

Unsizing structs is a similar procedure, the data pointer is the pointer to the struct itself, and
the info comes from unsizing the last field of the struct.

``` rust
struct Foo<T> {
    head: i32,
    tail: T,
}

let sized: Foo<[i32; 4]> = Foo { head: 0, tail: [1, 2, 3, 4] };
let fat_ptr: &Foo<Slice<i32>> = &sized;
```

### Other syntaxes

A more free form:

``` rust
unsized type Foo<A, B> = Bar<C, D>;
```

> What is the impact of not doing this?

Live with the problems outlined in the motivation.

There are other features that can address those problems, but either they don't solve all the
issues, or are more complex to design/implement.

## Operator overloading via HKT

## Nested DST

eddyb suggested supporting `[[T]]` as the "existential" form of `[[T; N]; M]` to handle the case
of contiguous matrices. The fat pointer `&[[T]]` would be represented in memory as
`((*T, usize), usize)`. And it'll be possible to coerce a nested fixed-size array to this type:
`let mat: &[[i32]] = &[[0; 3]; 5]`.

This custom unsized types proposal has the advantage of giving more control over the memory layout
of the fat pointer, such that one can handle not only contiguous matrices, but also strided
matrices, strided slices, sparse matrices and even small variations to the contiguous matrix
representation, like using `u32` instead of `usize` for the dimensions.

The advantage of the nested DST proposal is that you get unsizing coercions for "free". But this
advantage is immediately lost if you newtype a nested DST, as in `struct Mat<T>([[T]])`. And
newtyping `[[T]]` is necessary for the matrix case to be able to support indexing, slicing, pretty
printing and arithmetic operations, as one cannot implement the `Add`, `Index`, `Debug` traits for
`[[T]]`.

# Unresolved questions

> What parts of the design are still TBD?

- Should the first field of `FatPtr<Data, Info>` be `*mut Data` or `*const Data`?
