module LabListener

open System
open System.Threading
open System.Collections.Generic
open System.Windows.Forms          // Note that you'll need references for System.Windows.Forms and
open System.Drawing                // System.Drawing  (See the "solution explorer", on the right in VS...)  ----->>> 
open Microsoft.FSharp.Core.Printf

/// Generate a random positive integer in 1..n
let random = let myRand = Random() in myRand.Next
             
/// Sleep for n milliseconds
let sleep (n:int) = Thread.Sleep n

/// Sleep for between 1 and some number of milliseconds
let sleepUpTo (n:int) = sleep (1 + random n)

/// Make a named thread that runs f
let mkThread name f = Thread (ThreadStart f, Name=name)
/// Make a named thread and start it immediately
let startThread name f = (mkThread name f).Start()

/// wait until another thread signals the given object has changed
let waitFor obj = ignore(Monitor.Wait obj)
/// wake up all threads waiting on a given object
let wakeWaiters obj = Monitor.PulseAll obj

let logger (str:string) = System.Console.WriteLine(str)


type lab (labID, rules) =
    let busy = ref false
    let usingClID = ref None  // Stores the client currently using the lab, if there is one.

    member this.Rules = rules
    
    /// Send an experiment to a lab.  The continuation is called with the result of the experiment
    //  when it is complete.
    member this.DoExp delay clID (continuation : bool->unit) =  
       startThread ("Lab"+ string labID) <| fun ()->
          if !busy then  let str = sprintf "BANG! lab%d explodes - host%d is already using it" labID (!usingClID).Value
                         logger str //; failwith str // uncomment this to have the whole program fail.
          usingClID := Some clID              
          busy:=true
          sleep delay  // Doing experiment (the delay is provided by the client for ease of testing the prototype)
          busy:=false
          usingClID := None
          if random 2 = 1 then continuation true  else continuation false

let laba = new lab(1, [])

let delay = 300

let result = ref None



type ToyLocker () =
    let workQueue = Queue([1..2])
    
    let stop_work = ref false
    
    let queuelength_internal = ref workQueue.Count
    let work_loop () = 
        while(not stop_work.Value) do
            lock queuelength_internal <| fun() -> 
                while(queuelength_internal.Value = 0) do
                    
                    waitFor queuelength_internal
                logger (sprintf "Thread: %s woke up because of new data: " Thread.CurrentThread.Name)
                logger (sprintf "Data: %d" (workQueue.Dequeue()))
                queuelength_internal := (queuelength_internal.Value - 1)
                wakeWaiters queuelength_internal
        ()
            
    member this.enqueue obj = lock queuelength_internal <| fun() -> 
        workQueue.Enqueue obj
        queuelength_internal := (queuelength_internal.Value + 1)
        wakeWaiters queuelength_internal
    member this.dequeue = workQueue.Dequeue; queuelength_internal := (queuelength_internal.Value - 1)
    member this.listener = startThread "Thread Listener" work_loop
    member this.queuelength = queuelength_internal
    member this.log str = printfn str


logger "Start Listener"
let locker = new ToyLocker()

locker.listener

//////////////////////////////////////////////////////////////////////////////////////////////////
// You may find the following useful to make actions run after a lock is released - this is otherwise 
// slightly tricky when those actions depend on variables with scopes within the holding of the lock.  E.g.
//     hlock obj <| fun later -> let v = something()
//                               later <| fun()-> someActionUsing v   // Makes the action happen after lock release.

/// Lock an object while running f, but also pass a "hook" to f for making functions run when the lock is released.  
let hLock obj f = let onUnlock = ref (fun() -> ())
                  let later action =  onUnlock := (let u = !onUnlock in fun () -> u(); action())
                  let res = lock obj <| fun()-> f later                 // Execute f, providing the later function
                  (!onUnlock)()
                  res                                   



// No hooklock
//for i in [1..10] do
//    startThread (sprintf "%d" i) (fun() ->
//        logger "Launching Lab Experiment Request"
//        lock result <| fun() -> 
//            laba.DoExp (random 500) 1 (fun(res) -> (lock result <| fun() -> result := Some res; logger "Finished Experiment"; wakeWaiters result) )
//            while(result.Value.IsNone) do 
//                logger (sprintf "Thread: %s is waiting" Thread.CurrentThread.Name)
//                waitFor result
//            result := None
//            logger "Got result"
//            wakeWaiters result
//        logger (sprintf "Number of Enqueued items: %d" locker.queuelength.Value))


// Hooklock
let flag = ref true
for i in [1..20] do 
    startThread (sprintf "%d" i) (fun() ->
        logger "Launching Lab Experiment Request"

        lock flag <| fun() ->
            while(flag.Value) do
                flag := false
                waitFor flag
            logger "Got access to do experiment"
            hLock result <| fun later -> let v = laba.DoExp (random 500) 1 (fun(res) -> (lock result <| fun() -> result := Some res; logger "Finished Experiment"; wakeWaiters result) )
                                         later <| fun() -> lock result <| fun() -> 
                                                                while(result.Value.IsNone) do 
                                                                    logger (sprintf "Thread: %s is waiting" Thread.CurrentThread.Name)
                                                                    waitFor result
                                                                result := None
                                                                logger "Got result"
                                                                wakeWaiters result
            flag := true
            wakeWaiters flag
    )


    