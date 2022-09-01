module Main

open Elmish
open Elmish.LiveSubs
open System

[<EntryPoint>]
let main argv =

    let initArg = ()

    Program.mkProgram App.init App.update App.view
    |> Program.withLiveSubs App.subscribe
    #if DEBUG
    |> Program.withConsoleTrace
    #endif
    |> Program.runWith initArg

    // keeps it running initially
    Console.ReadLine() |> ignore
#if DEBUG
    //printf "Press ENTER to exit"
    //Console.ReadLine() |> ignore
#endif

    0 // return an integer exit code

