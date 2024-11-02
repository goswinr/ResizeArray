namespace ResizeArray

open System



/// This module is automatically opened when the namespace ResizeArray is opened.
/// It provides a computational expression builder for ResizeArray<'T>.
/// <c>resizeArray { ... }</c>
/// This builder allows you to create a ResizeArray just like you would create a IEnumerable with seq expressions <c>seq { ... }</c>.
[<AutoOpen>]
module AutoOpenComputationalExpression  =

    //[<InlineIfLambda>] needs F# 6.0

    //TODO: optimize with
    // [<InlineIfLambda>] as in https://gist.github.com/Tarmil/afcf5f50e45e90200eb7b01615b0ffc0
    // or https://github.com/fsharp/fslang-design/blob/main/FSharp-6.0/FS-1099-list-collector.md
    // or https://github.com/fsbolero/Bolero/blob/master/src/Bolero.Server/Html.fs

    // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions
    // https://fssnip.net/8aq/title/Computation-Expression-Stub

    //[<NoComparison; NoEquality>]
    type ComputationalExpressionBuilderResizeArray<'T> () =

        member inline _.Yield (x: 'T) =
            fun (r: ResizeArray<'T>) ->
                r.Add(x)

        member inline _.YieldFrom (xs: #seq<'T>) =
            fun (r: ResizeArray<'T>) ->
                r.AddRange(xs)

        member inline _.Combine ([<InlineIfLambda>] f, [<InlineIfLambda>] g) =
            fun (r: ResizeArray<'T>) ->
                f r;
                g r

        member inline _.Delay ([<InlineIfLambda>] f) =
            fun (r: ResizeArray<'T>) -> (f()) r

        /// Called for empty else branches of if...then expressions in computation expressions.
        member inline _.Zero () =
            ignore

        /// always allocates a seq and an enumerator,
        /// even for 'i=0 to x' . To avoid that use a while loop
        member inline _.For (xs: seq<'U>, [<InlineIfLambda>] body: 'U -> ResizeArray<'T> -> unit) =
            fun (r: ResizeArray<'T>) ->
                use e = xs.GetEnumerator()
                while e.MoveNext() do
                    body e.Current r

        // This DOES not work unfortunately
        // member inline _.For (fromIndex:int, toIndex:int, [<InlineIfLambda>] body: int -> ResizeArray<'T> -> unit) =
        //     fun (r: ResizeArray<'T>) ->
        //         for i=fromIndex to toIndex do
        //             body i r

        member inline _.While ([<InlineIfLambda>] predicate: unit -> bool, [<InlineIfLambda>] body: ResizeArray<'T> -> unit) =
            fun (r: ResizeArray<'T>) ->
                while predicate () do
                    body r

        member inline _.Run ([<InlineIfLambda>] body: ResizeArray<'T> -> unit) =
            let r = ResizeArray<'T>()
            do body r
            r

        member inline  _.TryWith([<InlineIfLambda>] body: ResizeArray<'T> -> unit, [<InlineIfLambda>] handler: exn ->  ResizeArray<'T> -> unit) =
            fun (r: ResizeArray<'T>) ->
                try body r
                with e -> handler e r

        member inline  _.TryFinally([<InlineIfLambda>] body: ResizeArray<'T> -> unit, [<InlineIfLambda>] compensation:  ResizeArray<'T> -> unit) =
            fun (r: ResizeArray<'T>) ->
                try body r
                finally compensation  r

        member inline this.Using(disposable: #IDisposable, [<InlineIfLambda>] body: #IDisposable -> ResizeArray<'T> -> unit) =
            this.TryFinally( body disposable ,  fun (_: ResizeArray<'T>)  ->
                if not <| Object.ReferenceEquals(disposable,null) then // might be disposed already
                    disposable.Dispose()
            )


    /// A computational expression builder for ResizeArray<'T>.
    /// <c>resizeArray { ... }</c>
    /// It allows you to create a ResizeArray just like you would create a IEnumerable with seq expressions <c>seq { ... }</c>.
    let resizeArray<'T> = new ComputationalExpressionBuilderResizeArray<'T> ()