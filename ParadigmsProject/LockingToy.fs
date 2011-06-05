#light

// Programming Paradigms Project

module LockingToy

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

type ToyLocker () =
    let workQueue = Queue([1..2])
    
    let stop_work = ref false
    
    let queuelength_internal = ref workQueue.Count
    let work_loop () = 
        while(not stop_work.Value) do
            lock queuelength_internal <| fun() -> 
                while(queuelength_internal.Value = 0) do
                    printfn "Thread: %s is waiting" Thread.CurrentThread.Name
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

    

let sharedInt = ref 0



if false then
    let lockaholic () = 
        for i in [1..1000] do
            lock sharedInt <| fun() -> 
                sleep (random 5)
                sharedInt := (sharedInt.Value + 1) 
                printfn "Thread: %s incremented sharedInt: %d" Thread.CurrentThread.Name, sharedInt.Value
            |> ignore
        ()

    let threada = Thread (ThreadStart lockaholic, Name="Thread A")
    startThread "Thread B" lockaholic
    threada.Start()

logger "Start Listener"
let locker = new ToyLocker()

locker.listener


while(true) do
    System.Console.ReadLine() |> ignore
    logger "Enqueueing item to listener"
    locker.enqueue(random 10)
    logger (sprintf "Number of Enqueued items: %d" locker.queuelength.Value)
