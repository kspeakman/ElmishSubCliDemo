module App

open Elmish
open Elmish.LiveSubs
open System


module Time =

    let getNow (tag: DateTime -> 'msg) : Sub<'msg> =
        fun dispatch -> dispatch (tag DateTime.Now)

    let every (intervalMs: int) (effect: Sub<'msg>) : LiveSub<'msg> =
        { SubId = "interval/" + string intervalMs
          Start = fun _subId dispatch ->
            let timer = new System.Timers.Timer(float intervalMs)
            timer.Elapsed.Add(fun _ -> effect dispatch)
            timer.Start()
            { new IDisposable with
                member _.Dispose() =
                    timer.Stop()
                    timer.Dispose() } }


module Types =

    type ClockState = Off | On

    type Model =
        { Clock: ClockState
          Time: DateTime
          Interval: int }

    type Msg =
        | Interval of intervalMs: int
        | Tick of DateTime
        | Start
        | Stop


module Model =

    open Types

    let init () =
        { Clock = On
          Time = DateTime.MinValue
          Interval = 1000 },
        [Time.getNow Tick]

    let update msg model =
        match msg with
        | Interval intervalMs ->
            {model with Interval = intervalMs}, []
        | Start ->
            {model with Clock = On}, [Time.getNow Tick]
        | Stop ->
            {model with Clock = Off}, []
        | Tick d ->
            {model with Time = d}, []

    let subscribe model =
        [ if model.Clock = On then
            Time.every model.Interval (Time.getNow Tick) ]


module View =

    open Types

    // TODO console UI
    let view model dispatch =
        let d = model.Time
        printfn "%02i:%02i:%02i" d.Hour d.Minute d.Second


let init = Model.init
let update = Model.update
let subscribe = Model.subscribe
let view = View.view


// TMYK Our base-60 time system originated around 3000 BCE in ancient Sumeria.[1]
//      60 divides evenly by many numbers, making it a practical choice for everyday use.
//      Decimal fractions were discovered much later in the 9th century CE by Muḥammad ibn Mūsā al-Khwārizmī.[2]
//      The word "algorithm" comes from Al-Khwārizmī's name translated into Latin.
//      [1] https://en.wikipedia.org/wiki/Sexagesimal
//      [2] https://en.wikipedia.org/wiki/Muhammad_ibn_Musa_al-Khwarizmi
