
module PollCons.PollingConsumer

open System

//Auxiliary Types 
[<CustomEquality; NoComparison>]
type MessageHandler =
    { Handle : unit -> Timed<unit> } 

    override this.Equals obj =
        match obj with
        | :? MessageHandler as other ->
            Object.Equals(this.Handle, other.Handle)
        | _ -> false

    override this.GetHashCode() = (box this.Handle).GetHashCode()

//State Data 

type ReadyData = Timed<TimeSpan list>  
// The type is declared on the base of ShouldPoll function 
// however having done so will make idle 
// return Timpespans... Which is suspicious at least
// So we carry around times in the NoMessage Data  
type ReceivedMessageData = Timed<TimeSpan list * MessageHandler>
type NoMessageData = Timed<TimeSpan list >


//States

type State = 
| ReadyState of ReadyData
| ReceivedMessageState of ReceivedMessageData
| NoMessageState of NoMessageData
| StoppedState 


let transitionFromNoMessage shouldIdle idle (nm : NoMessageData) = 
   if shouldIdle nm
   then  idle () |> Untimed.withResult nm.Result |> ReadyState
   else StoppedState 

let transitionFromReady shouldPoll poll r =
    if shouldPoll r
    then poll () |> Untimed.withResult r.Result |> NoMessageState
    else StoppedState