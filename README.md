
![Logo](https://raw.githubusercontent.com/goswinr/ResizeArray/main/Docs/img/logo128.png)
# ResizeArray

[![ResizeArray on nuget.org](https://img.shields.io/nuget/v/ResizeArray)](https://www.nuget.org/packages/ResizeArray/)
[![Build Status](https://github.com/goswinr/ResizeArray/actions/workflows/build.yml/badge.svg)](https://github.com/goswinr/ResizeArray/actions/workflows/build.yml)
[![Docs Build Status](https://github.com/goswinr/ResizeArray/actions/workflows/docs.yml/badge.svg)](https://github.com/goswinr/ResizeArray/actions/workflows/docs.yml)
[![Test Status](https://github.com/goswinr/ResizeArray/actions/workflows/test.yml/badge.svg)](https://github.com/goswinr/ResizeArray/actions/workflows/test.yml)
[![license](https://img.shields.io/github/license/goswinr/ResizeArray)](LICENSE.md)
![code size](https://img.shields.io/github/languages/code-size/goswinr/ResizeArray.svg)


ResizeArray is an F# extension and module library for `ResizeArray<'T>` ( = `Collection.Generic.List<'T>`)

It also works in JS and TS with [Fable](https://fable.io/).

This library was designed for use with F# scripting.
Functions and methods never return null.
Only functions starting with `try...` will return an F# Option.
Otherwise when a function fails on invalid input it will throw a descriptive exception.

I was always annoyed that an IndexOutOfRangeException does not include the actual index that was out of bounds nor the actual size of the array.
This library fixes that in `resizeArray.Get`, `resizeArray.Set`, `resizeArray.Slice` and other item access functions.

### It Includes:

- A `ResizeArray` module that has a corresponding functions for  **all**  functions in the  [`Array` module from `FSharp.Core`](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-arraymodule.html). Including those for parallel computing.
- A  Computational Expressions `resizeArray` that can be used like existing ones for `seq`.
- Support for F# slicing operator and indexing from the end. e.g: `items.[ 1 .. ^1]`
- Extension members on `ResizeArray` like `.Get` `.Set` `.First` `.Last` `.SecondLast` and more.
With nicer IndexOutOfRangeExceptions that include the bad index and the actual size.

- **All** Tests from the from `FSharp.Core`'s `Array` module ported and adapted to run in both javascript and dotnet.


See also https://github.com/goswinr/ArrayT/ for a similar library for `array<'T>`.

### Usage
Just open the namespace

```fsharp
open ResizeArray
```
this namespace contains:
- a module also called `ResizeArray`
- a  Computational Expressions called `resizeArray`
- this will also auto open the extension members on `ResizeArray<'T>`

then you can do:

```fsharp
let evenNumbers =
    resizeArray {  // a Computational Expressions like seq
        for i = 0 t 99 do
            if i % 2 = 0 then
                i
    }

let oddNumbers = evenNumbers |> ResizeArray.map (fun x -> x + 1) // ResizeArray module

let hundred = oddNumbers.Last // Extension member to access the last item in list

```

### Why ?
Yes, F# Arrays and (linked) Lists can do these kind of operations on collections too.
But ResizeArray (being mutable)  still offers the best performance for collections that expand or shrink and need random access.

In fact FSharp.Core uses [a very similar module internally](https://github.com/dotnet/fsharp/blob/main/src/Compiler/Utilities/ResizeArray.fs)

### Full API Documentation

[goswinr.github.io/ResizeArray](https://goswinr.github.io/ResizeArray/reference/resizearray.html)


### Tests
All Tests run in both javascript and dotnet.
Successful Fable compilation to typescript is verified too.
Go to the tests folder:

```bash
cd Tests
```

For testing with .NET using Expecto:

```bash
dotnet run
```

for JS testing with Fable.Mocha and TS verification:

```bash
npm test
```

### License
[MIT](https://github.com/goswinr/ResizeArray/blob/main/LICENSE.md)

### Changelog
see [CHANGELOG.md](https://github.com/goswinr/ResizeArray/blob/main/CHANGELOG.md)

