namespace ResizeArray

open System

module ComputationalExpressions =

    //TODO: optimize with 
    // [<InlineIfLambda>] as in https://gist.github.com/Tarmil/afcf5f50e45e90200eb7b01615b0ffc0
    // or https://github.com/fsharp/fslang-design/blob/main/FSharp-6.0/FS-1099-list-collector.md

    type ResizeArrayBuilder<'T> () = 
    
        member inline _.Yield (x: 'T) = 
            fun (r: ResizeArray<'T>) -> 
                r.Add(x)

        member inline _.YieldFrom (xs: #seq<'T>) = 
            fun (r: ResizeArray<'T>) -> 
                r.AddRange(xs)

        member inline _.Combine (f, g) = 
            fun (r: ResizeArray<'T>) -> 
                f r; 
                g r

        member inline _.Delay f = 
            fun (r: ResizeArray<'T>) -> (f()) r

        member inline _.Zero () =  
            ignore

        member inline _.For (xs: 'U seq, body: 'U -> ResizeArray<'T> -> unit) = 
            fun (r: ResizeArray<'T>) ->
                use e = xs.GetEnumerator()
                while e.MoveNext() do    
                    (body e.Current) r

        member inline _.While (predicate: unit -> bool, body: ResizeArray<'T> -> unit) = 
            fun (r: ResizeArray<'T>) -> 
                while predicate () do 
                    body r

        member inline _.Run (body: ResizeArray<'T> -> unit) = 
            let r = ResizeArray<'T>()
            do body r
            r
        
        member inline  _.TryWith(body: ResizeArray<'T> -> unit, handler: exn ->  ResizeArray<'T> -> unit) =
            fun (r: ResizeArray<'T>) -> 
                try body r 
                with e -> handler e r

        member inline  _.TryFinally(body: ResizeArray<'T> -> unit, compensation:  ResizeArray<'T> -> unit) =
            fun (r: ResizeArray<'T>) -> 
                try body r 
                finally compensation  r

        member inline this.Using(disposable: #IDisposable, body: #IDisposable -> ResizeArray<'T> -> unit) =            
            this.TryFinally( body disposable ,  fun (r: ResizeArray<'T>)  ->  
                if not <| Object.ReferenceEquals(disposable,null) then // might be disposed already   
                    disposable.Dispose() 
            )                      

[<AutoOpen>]
module AutoOpenComputationalExpressions  =     

    /// Computational Expression:  use 'yield' to add elements to a ResizeArray (a Collections.Generic.List).
    let resizeArray<'T> = new ComputationalExpressions.ResizeArrayBuilder<'T> () 