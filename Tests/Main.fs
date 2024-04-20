namespace Tests

open ResizeArray

module Main =

    #if FABLE_COMPILER

    open Fable.Mocha
    Mocha.runTests Tests.Extensions.tests |> ignore
    Mocha.runTests Tests.Module.tests |> ignore
    Mocha.runTests Tests.Module2.tests |> ignore

    #else

    open Expecto
    [<EntryPoint>]
    let main argv =
        runTestsWithCLIArgs [] [||] Tests.Extensions.tests
        |||
        runTestsWithCLIArgs [] [||] Tests.Module.tests
        |||
        runTestsWithCLIArgs [] [||] Tests.Module2.tests


    #endif