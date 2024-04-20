namespace ResizeArray

open System
open System.Collections.Generic

/// Open this module to get access to yhe operators +++ and ++
/// to combines the contents of two Sequences or ICollection<'T> into a new ResizeArray
module Operators =

    /// Operator +++ to copies the contents of two Sequences into a new ResizeArray
    let inline (+++) (a : seq<'T>) ( b : seq<'T>) =
        let r = new ResizeArray<'T>()
        r.AddRange a
        r.AddRange b
        r

    /// Operator ++ to copies the contents of two ICollection<'T> into a new ResizeArray.
    /// The capacity of the new ResizeArray is the sum of the count of the two ICollection<'T>.
    let inline (++) (a : ICollection<'T>) ( b : ICollection<'T>) =
        let c: int = a.Count + b.Count
        let l = new ResizeArray<'T>(c)
        l.AddRange a
        l.AddRange b
        ResizeArray(l)

[<Obsolete("not Obsolete but hidden, needs to be visible for inlining")>]
module UtilResizeArray =

    /// This module is set to auto open when opening ResizeArray namespace.
    /// Static Extension methods on Exceptions to cal Exception.Raise "%A" x with F# printf string formatting
    [<AutoOpen>]
    module AutoOpenExtensionsExceptions =

        // type ArgumentException with

        //     /// Raise ArgumentException with F# printf string formatting
        //     /// this is also the base class of ArgumentOutOfRangeException and ArgumentNullException
        //     static member RaiseBase msg =
        //         Printf.kprintf (fun s -> raise (ArgumentException(s))) msg


        type ArgumentOutOfRangeException with

            /// Raise ArgumentOutOfRangeException with F# printf string formatting
            static member Raise msg =
                Printf.kprintf (fun s -> raise (ArgumentOutOfRangeException(s))) msg

        type KeyNotFoundException with

            /// Raise KeyNotFoundException with F# printf string formatting
            static member Raise msg =
                Printf.kprintf (fun s -> raise (KeyNotFoundException(s))) msg


        type ArgumentNullException with

            /// Check if null and raise System.ArgumentNullException
            static member Raise msg =
                    raise (System.ArgumentNullException("ResizeArray." + msg + " input is null!"))


    let toNiceString (x: 'T) = // TODO replace with better implementation
        match box x with
        | null -> "null"
        #if FABLE_COMPILER
        | :? ResizeArray<'T> as xs -> sprintf "A ResizeArray with %d items." xs.Count
        #else
        | :? ResizeArray<'T> as xs -> sprintf "A ResizeArray<%s> with %d items." (typeof<'T>).FullName xs.Count
        #endif
        | _ -> x.ToString()

    let toNiceStringLong (x: 'T) = // TODO replace with better implementation
        toNiceString x

    /// Converts negative indices to positive ones.
    /// Correct results from -length up to length-1
    /// e.g.: -1 is  last item .
    /// (from the release of F# 5 on a negative index can also be done with '^' prefix. E.g. ^0 for the last item)
    let inline negIdx i len =
        let ii = if i < 0 then len + i else i
        if ii < 0 || ii >= len then
            ArgumentOutOfRangeException.Raise "UtilResizeArray.negIdx: Bad index %d for items count %d." i len
        ii

    /// Any int will give a valid index for given collection size.
    /// Converts negative indices to positive ones and loops to start after last index is reached.
    /// Returns a valid index for a collection of 'length' items for any integer
    let inline negIdxLooped i length =
        let t = i % length
        if t >= 0 then t else t + length

    let inline isNullSeq x =
        match x with
        |null -> true
        | _ -> false

