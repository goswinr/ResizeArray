
![Logo](https://raw.githubusercontent.com/goswinr/ResizeArray/main/Docs/img/logo128.png)
# ResizeArray

[![ResizeArray on nuget.org](https://img.shields.io/nuget/v/ResizeArray)](https://www.nuget.org/packages/ResizeArray/)
[![Build Status](https://github.com/goswinr/ResizeArray/actions/workflows/build.yml/badge.svg)](https://github.com/goswinr/ResizeArray/actions/workflows/build.yml)
[![Docs Build Status](https://github.com/goswinr/ResizeArray/actions/workflows/docs.yml/badge.svg)](https://github.com/goswinr/ResizeArray/actions/workflows/docs.yml)
[![Test Status](https://github.com/goswinr/ResizeArray/actions/workflows/test.yml/badge.svg)](https://github.com/goswinr/ResizeArray/actions/workflows/test.yml)
[![license](https://img.shields.io/github/license/goswinr/ResizeArray)](LICENSE.md)
![code size](https://img.shields.io/github/languages/code-size/goswinr/ResizeArray.svg)


ResizeArray is an F# extension and module library for `ResizeArray<'T>` ( = `Collection.Generic.List<'T>`).\
It provides all the functions from the `Array` module in `FSharp.Core` for `ResizeArray`.\
And more.

It also works in Javascript and Typescript with [Fable](https://fable.io/).

This library was designed for use with F# scripting.\
Functions and methods never return null.\
When a function fails on invalid input it will throw a descriptive exception.\
Functions starting with `try...` will return an F# option.

I was always annoyed that an `IndexOutOfRangeException` does not include the actual bad index nor the actual size of the array.\
This library fixes that in `resizeArray.Get`, `resizeArray.Set`, `resizeArray.Slice` and all other item access functions.\
I made a similar a similar library for `array<'T>`: https://github.com/goswinr/ArrayT/ .

### Why ?
Yes, F#'s array and list modules can do these kind of operations on collections too.\
But ResizeArray (being mutable)  still offers the best performance for collections that can expand & shrink and need random access via an index.\
In fact FSharp.Core uses [a very similar module internally](https://github.com/dotnet/fsharp/blob/main/src/Compiler/Utilities/ResizeArray.fs).

### It Includes:

- A `ResizeArray` module that has  **all**  functions from [`Array` module from `FSharp.Core`] reimplemented (https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-arraymodule.html).\
 Including the sub module for Parallel computing.

- A  Computational Expressions `resizeArray` that can be used like existing ones for `seq`.

- Support for F# slicing operator and indexing from the end. e.g: `items.[ 1 .. ^1]`.

- Extension members on `ResizeArray` like `.Get` `.Set` `.First` `.Last` `.SecondLast` and more.\
With nicer IndexOutOfRangeExceptions that include the bad index and the actual size.

- All Tests from the from `FSharp.Core`'s `Array` module ported and adapted to run in both javascript and dotnet.



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

