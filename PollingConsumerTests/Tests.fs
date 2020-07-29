module Tests

open FsCheck.Xunit
open Swensen.Unquote
open PollCons
open PollCons.PollingConsumer

[<Property>]
let ``transitionFromNoMessage returns correct result when it idles`` 
    (nm : NoMessageData)
    (idleRes : Timed<unit>) =

    let shouldIdle _ = true
    let idle _ = idleRes

    let actual : State = 
        transitionFromNoMessage shouldIdle idle nm
    
    let expected = 
        idleRes |> Untimed.withResult nm.Result |> ReadyState
    
    expected =! actual


[<Property>]
let ``transitionFromNoMessage returns correct result when it does not idle``
   (nm : NoMessageData)
   (idleRes : Timed<unit>) =

   let shouldIdle _ = false
   let idle _ = idleRes

   let actual : State = 
       transitionFromNoMessage shouldIdle idle nm
   
   let expected = 
       StoppedState
   
   expected =! actual
