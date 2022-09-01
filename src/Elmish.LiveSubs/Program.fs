namespace Elmish.LiveSubs

module Program =

    open Elmish
    open System

    type SubMsg<'msg> =
        | Stopped of SubId
        | Started of SubId * IDisposable
        | UserMsg of 'msg

    type SubModel<'model> =
        {
            Subs: Map<SubId, IDisposable>
            UserModel: 'model
        }


    module Fx =

        let warnDupe onError (subId) =
            fun _dispatch ->
                let ex = exn "Duplicate SubId"
                onError ("Duplicate SubId: " + subId, ex)

        let start onError sub =
            fun dispatch ->
                try
                    let stop = sub.Start sub.SubId (UserMsg >> dispatch)
                    dispatch (Started (sub.SubId, stop)) // no dispatch on error
                with ex ->
                    onError ("Error starting subscription: " + sub.SubId, ex)

        let stop onError (subId, sub: IDisposable) =
            fun dispatch ->
                try
                    sub.Dispose()
                with ex ->
                    onError ("Error stopping subscription: " + subId, ex)
                dispatch (Stopped subId) // always

        let fromDiff onError (dupes, toStop, _toKeep, toStart) =
            Cmd.batch [
                dupes |> List.map (warnDupe onError)
                toStop |> List.map (stop onError)
                toStart |> List.map (start onError)
            ]


    module LiveSubs =

        module NewSubs =

            let (_dupes, _newKeys, _newSubs) as init =
                List.empty, Set.empty, List.empty

            let update ({SubId = subId} as newSub) (dupes, newKeys, newSubs) =
                if Set.contains subId newKeys then
                    subId :: dupes, newKeys, newSubs
                else
                    dupes, Set.add subId newKeys, newSub :: newSubs

            let calculate subs =
                List.foldBack update subs init

        let diff (activeSubs: Map<SubId, IDisposable>) (subs: LiveSub<'msg> list) =
            let keys = activeSubs |> Map.keys |> Set.ofSeq
            let dupes, newKeys, newSubs = NewSubs.calculate subs
            if keys = newKeys then
                dupes, [], activeSubs, []
            else
                let toKeep, toStop = activeSubs |> Map.partition (fun k _ -> Set.contains k newKeys)
                let toStart = newSubs |> List.filter (fun x -> not (Set.contains x.SubId keys))
                dupes, Map.toList toStop, toKeep, toStart


    let withLiveSubs
        (subscribe : 'model -> LiveSub<'msg> list)
        (program : Program<'arg, 'model, 'msg, 'view>)
        : Program<'arg, SubModel<'model>, SubMsg<'msg>, 'view> =

        let onError = Program.onError program

        let mapInit userInit arg =
            let userModel, userCmd = userInit arg
            let subs = subscribe userModel
            { UserModel = userModel; Subs = Map.empty },
            Cmd.batch [
                List.map (Fx.start onError) subs
                Cmd.map UserMsg userCmd
            ]

        let mapUpdate userUpdate msg model =
            match msg with
            | Stopped subId ->
                {model with
                    Subs = Map.remove subId model.Subs}, []
            | Started (subId, stop) ->
                {model with
                    Subs = Map.add subId stop model.Subs}, []
            | UserMsg userMsg ->
                let userModel, userCmd = userUpdate userMsg model.UserModel
                let subs = subscribe userModel
                { model with UserModel = userModel },
                Cmd.batch [
                    LiveSubs.diff model.Subs subs |> Fx.fromDiff onError
                    Cmd.map UserMsg userCmd
                ]

        let mapView userView model dispatch =
            userView model.UserModel (UserMsg >> dispatch)

        let mapSetState userSetState model dispatch =
            userSetState model.UserModel (UserMsg >> dispatch)

        let mapSubscribe userSubscribe model =
            userSubscribe model.UserModel |> Cmd.map UserMsg

        program
        |> Program.map mapInit mapUpdate mapView mapSetState mapSubscribe

