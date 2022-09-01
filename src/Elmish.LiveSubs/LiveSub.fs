namespace Elmish.LiveSubs

open Elmish
open System

/// SubId - Subscription ID, alias for string
type SubId = string

/// Subscription - Generates new messages when running
type LiveSub<'msg> =
    { SubId: SubId
      Start: SubId -> Dispatch<'msg> -> IDisposable }

module LiveSub =

    /// None - no subscriptions, also known as `[]`
    let none : LiveSub<'msg> list =
        []

    /// Aggregate multiple subscriptions
    let batch (subs: #seq<LiveSub<'msg> list>) : LiveSub<'msg> list =
        subs |> List.concat

    /// When emitting the message, map to another type.
    /// To avoid ID conflicts with other components, scope SubIds with a prefix.
    let map (idPrefix: string) (f: 'a -> 'msg) (subs: LiveSub<'a> list)
        : LiveSub<'msg> list =
        subs |> List.map (fun sub ->
            { SubId = idPrefix + sub.SubId
              Start = fun subId dispatch -> sub.Start subId (f >> dispatch) })

