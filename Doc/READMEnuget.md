

# ResizeArray

![code size](https://img.shields.io/github/languages/code-size/goswinr/ResizeArray.svg) 
[![license](https://img.shields.io/github/license/goswinr/ResizeArray)](LICENSE)

ResizeArray is an F# extension and module library for `ResizeArray<'T>` ( = `Collection.Generic.List<'T>`)

It also works with [Fable](https://fable.io/).


![Logo](https://raw.githubusercontent.com/goswinr/ResizeArray/main/Doc/logo.png)

### It Includes: 

- A `ResizeArray` module that has a corseponding functions for  **all**  functions in the  [`Array` module from `FSharp.Core`](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-arraymodule.html). Including those for parallel computing.
- A  Computational Expressions `resizeArray` that can be used like existing ones for `seq`.
- Support for F# slicing operator and indexing from the end. e.g: `items.[ 1 .. ^1]`
- Extension members on `ResizeArray` like `.Get` `.Set` `.First` `.Last` `.SecondLast` and more.
With nicer IndexOutOfRangeExceptions that include the bad index and the actual size.

- **All** Tests from the from `FSharp.Core`'s `Array` module ported and adapted to run in both javascript and dotnet.

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


### License
[MIT](https://raw.githubusercontent.com/goswinr/ResizeArray/main/LICENSE.txt)

### Test
All Tests from the from `FSharp.Core`'s `Array` module ported and adapted to run in both javascript and dotnet.
go to the tests folder

```bash
cd Tests
```

For testing with .NET using Expecto run 

```bash
dotnet run
```

for testing with Fable.Mocha run 

```bash
npm test
```
    
    
### Changelog

`0.15.0`
- implementation ported from `Rarr` type in https://github.com/goswinr/FsEx/blob/main/Src/RarrModule.fs 