#light

// Programming Paradigms Project

//module DistributedExperimentUnification

open System
open System.Threading
open System.Collections.Generic
open System.Windows.Forms          // Note that you'll need references for System.Windows.Forms and
open System.Drawing                // System.Drawing  (See the "solution explorer", on the right in VS...)  ----->>> 

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


////////////////////////////////////// Debugging Output 

Application.EnableVisualStyles()
let newLine = Environment.NewLine

/// A window with colour text output indicating on the object a thread is in.
type outputForm() =
       let pr = ref (fun cl str -> ())  // stub - the actual function is put in later
       let ready = ref false
       member this.Pr str = (!pr) str
       member this.WaitUntilReady() = lock this <| fun()-> while not !ready do waitFor this
       member this.Run() =                              // You can adjust the window size, etc, below as appropriate.
           let form = new Form(Text="Event interleaving",Size=Drawing.Size(1600, 1000),Visible=true)  
           let textB = new RichTextBox(Dock= DockStyle.Fill, Multiline=true, ReadOnly=true, 
                                       Font=new Font(familyName="Courier New", emSize=8.0f), BackColor=Color.Black)
           do textB.SelectionStart<-textB.TextLength ; textB.ScrollToCaret()
           let color n = let colors = [| Color.DodgerBlue; Color.Coral; Color.OliveDrab; Color.Violet;  Color.Gold; 
                                         Color.DimGray; Color.IndianRed;  Color.GreenYellow;  Color.BlueViolet; Color.White |]
                         if n>=0 then colors.[n%colors.Length] else Color.White
           pr := fun cl str -> let doPr() = textB.SelectionStart <- textB.TextLength; textB.SelectionColor<-color cl; 
                                            textB.SelectedText <- str+newLine; form.Show()                                          
                               if textB.InvokeRequired then 
                                  form.Invoke (Action doPr) |> ignore
                               else doPr()
           do form.Controls.Add(textB)
           do form.Show()
           do ready:=true; lock this <| fun()-> wakeWaiters this
           textB.Focus() |> ignore
           Application.Run(form)

let form = outputForm()                        
startThread "WinForms" form.Run
form.WaitUntilReady()

/// The time the system started.
let startTime = ref DateTime.Now // This is reset when a test sequence is started.

// Various functions for printing events to both the console and the coloured events window.
// They generally can be easily added at the end of a line to print a result via |> prIdent n prefixString
// Use a clientID of -1 for a print not related to a client.
let prLock = ref ()
let prRaw clientID str = lock prLock <|fun()->form.Pr clientID str; printfn "%s" str
let prFmt format args = prRaw 0 (sprintf format args)                                
let prStamp clientID pre str = let prefix = sprintf "%6.0f %s" (DateTime.Now - !startTime).TotalMilliseconds pre
                               let indent =  String.map (fun _-> ' ')  prefix
                               prRaw clientID ((sprintf "%s %s" prefix str).Replace("\n","\n"+indent))
let pr clientID pre res = prStamp clientID pre (sprintf "%A" res) ; res
let pr0 pre res = pr 0 pre res
let prIndStr clientID pre str = let indent =  String ( Array.map (fun _-> ' ') [|1..8+8*clientID|] )
                                prStamp clientID (indent+pre) str
let prIndent clientID pre res = prIndStr clientID pre (sprintf "%A" res); res

// These can be handy for debugging: swap them to the commented versions to turn on debug prints.
// (The VS threads window may also be useful for debugging.)
let dbg clientID pre res = res // pr clientID pre res
let dbgIndent clientID pre res = res // prIdent clientID pre res 





//////////////////////////////////////// Types for experiments, rules, etc.

type exp = A | B | Mix of exp * exp | Var of string

/// The value (e, ee) represents that "e suffices instead of ee" or "e suff ee" for short - see the rule type.
type sufficency = exp * exp  

/// The value (e, ee), [(e1,ee1) ... (eN,eeN)] represents the rule that: "E suff EE provided that for all i in 1..N, Ei suff EEi".  
/// Here the E, EE, Ei, EEi are the same as e, ee, eI, eeI except with each variable (Var Vj) replaced by an experiment EEEj that 
/// contains no vars (the same EEEj each time Vj appears in the rule).  The rule holds for every such substitution for the vars. 
type rule = Rule of sufficency * (sufficency list)  

type ruleGen = unit -> rule      // A rule generator creates new variables for a rule each time it is called, to avoid clashes
                                 // with variable names in other rules, or when the rule is used multiple times.
type labRules = ruleGen list     // Each lab has a list of rules, represented as generators.


// Types for identifying labs and clients
type labID = int
type clientID = int

/// The number of Bases and Mixes in an experiment
let rec expSize = function A|B -> 1
                         | Mix (x, y) -> 1+expSize x + expSize y
                         | Var _ -> raise (Exception "expSize for a Var")       // This shouldn't happen



/////////////////////////////////////////////////                        
/////  Put your code for the first part here                         
/////////////////////////////////////////////////

// Suffices checks whether exp1 suffices instead of exp2 according to rules.
let suffices rules (exp1, exp2) = false  // You'll need to implement this properly!


/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Hints:  First, see the hints on the project handout. Then the following are hints to help get you started. 
//  1. Decompose the problem of finding possible combined experiments into simpler steps.
//  2. The basic step will be checking "e suffices instead of E" for two experiments, using the set of 
//     rules for a particular lab.  Use a function "suffices" taking the rules and two experiments.
//  3. To use a rule to show a particular sufficiency like "E suffices instead of EE", all you need to do is choose 
//     an appropriate experiment EEEj for each variable Vj in the rule in order to turn e into E and ee into EE, and then 
//     attempt to show sufficiency for each pair (E1, EE1) ... (En, EEn) - these are called the subgoals.  Note that if n=0 there is
//     nothing more to do, and every lab has at least one rule like this.  (The rules will always be designed so that
//     it is impossible to keep following subgoals forever, although not if you have bugs, so you may want to limit this.) 
//
//     Note that variables can appear many times, including many times in the same experiment.
//     E.g., the following represents a rule that roughly means 
//     "doing an experiment x always suffices instead of doing x twice and mixing the results"
//          (Var "x", Mix (Var "x", Var "x")), []
// 4. Each time a rule is used, a single experiment is chosen to replace each variable.  But,
//    if the rule is used again, different experiments can be chosen.
//
// 5. To match experiments to rules, you will probably need a function "unify" that takes two exp's and returns a 
//    substitution that makes the exp's exactly the same when it is used to replace the vars in one of the exp's 
//    (if such a substitution exists).   For example:
//
//           unify (Mix (A, B)) (Mix (Var "x", A)) yields no substitution the two exp's can't be made the same.
//
//           unify (Mix (A, B)) (Mix (Var "x", Var "y")) yields a substitution that maps: "x" to A  and "y" to B
//
//           unify (Mix (A, A)) (Mix (Var "x", Var "x")) yields a substitution that maps: "x" to A 
//
//            unify (Mix (A, B)) (Mix (Var "x", Var "x")) yields no substitution, because no matter
//               what we replace Var "x" with, the two exp's won't end up the same 
//               ("x" can't be equal to both A and B).
//
//  6. Write unify by considering the possible cases for the two exps, and using recursion appropriately.
//  7. You will need a type for substitutions - mappings from variable names to exp's they should be replaced with.
//  8. Be careful not the create substitutions that map a variable to an exp that contains the same variable. (!)
//  9. You can modify the exact form of the unify function if you want.
// 10. Use this function write one that tries unifying one exp with each part of another exp.
// 11. Eventually you need a function that takes a number clients exps, generates all possible combined exps,
//     then chooses between them using an efficiency function like the one above.
// 12. A smaller experiment with the same number of clients should be preferred over a larger one.
//////////////////////////////////////////////////////////////////////////////////////////////////////////////


                         

// These are some handy functions for creating rule generators with different numbers of variables

let newVar =   let n = ref 0 in   fun v -> n:=!n+1;
                                           Var (v + string !n)

let newVar2 v1 v2 = newVar v1, newVar v2
let newVar3 (v1,v2,v3) = newVar v1, newVar v2, newVar v3
let newVar4 (v1,v2,v3,v4) = newVar v1, newVar v2, newVar v3, newVar v4


// These are some rules you can use for testing.
// Consider creating your own and include any interesting ones in your submission.

let rule1 () = let x = newVar "x"
               Rule ((x, x), [])

let rule2 () = let x, xx, y = newVar3 ("x", "xx", "y")
               Rule ((Mix(x,xx), y),  [(x,y)])

let rule3 () = let x, xx, y = newVar3 ("x", "xx", "y")
               Rule ((Mix(x,xx), y),  [(xx,y)])

let rule4 () = Rule ((Mix(A, B), Mix(B, A)), [])
let rule5 () = Rule ((Mix(B, A), Mix(A, B)), [])
let rule6 () = let x, xx, y, yy = newVar4 ("x", "xx", "y", "yy")
               Rule ((Mix(x,xx), Mix(y,yy)),  [(x,y); (xx,yy)])

let rule7 () = let x, xx, y, yy = newVar4 ("x", "xx", "y", "yy")
               Rule ((Mix(x,xx), Mix(y,yy)),  [(x,yy); (xx,y)])

let rule8 () = let x = newVar "x"
               Rule ((x, Mix(x,x)),  [])

let rule9 () = let x, xx, y, z = newVar4 ("x", "xx", "y", "z")
               Rule ( (Mix (x,xx),y),  [(x,z); (z,y)] )

let rule10() = let x, xx, y, z = newVar4 ("x", "xx", "y", "z")
               Rule ( (Mix (x,xx),y),  [(xx,z); (z,y)] )

let rule11() = Rule ( (A,B),  [] )
let rule12() = Rule ( (B,A),  [] )


// These are some example sets of rules.  Again, submit any interesting sets you use during testing.  

let rulesA = [rule1; rule2; rule3; rule6]
let rulesB = [rule1; rule2; rule3; rule4; rule5; rule6; rule7; rule8]
let rulesC = [rule4; rule5; rule9; rule10; rule11; rule12]         // Rules 9,10 are slightly tricky. 
                                                                   // Focus on rules like the others first.
let prTest pre res = pr0 (pre + " = ") res |> ignore

suffices [rule1] (A, A) |> prTest "suffices [rule1] (A, A)" 
suffices [rule1] (A, B) |> prTest "suffices [rule1] (A, B)" 
prRaw 0 "\n"

suffices rulesA (A, B) |> prTest "suffices rulesA (A, B)"
suffices rulesA (Mix (A, B), A) |> prTest "suffices rulesA (Mix (A, B),A)"

suffices rulesA (Mix (Mix (A, B), B),A) |> prTest "suffices rulesA (Mix (Mix (A, B), B),A)"
suffices rulesA (Mix (Mix (B, B), B),A) |> prTest "suffices rulesA (Mix (Mix (B, B), B),A)"
suffices rulesA (Mix (Mix (B, B), B), Mix (B, B)) |> prTest "suffices rulesA (Mix (Mix (B, B), B), Mix (B, B))"
suffices rulesA (Mix (Mix (A, B), B), Mix (B, A)) |> prTest "suffices rulesA (Mix (Mix (A, B), B), Mix (B, A))"
prRaw 0 "\n"


suffices rulesB (A, B) |> prTest "suffices rulesB (A, B)"
suffices rulesB (Mix (A, B), A) |> prTest "suffices rulesB (Mix (A, B),A)"

suffices rulesB (Mix (Mix (A, B), B),A) |> prTest "suffices rulesB (Mix (Mix (A, B), B),A)"
suffices rulesB (Mix (Mix (B, B), B),A) |> prTest "suffices rulesB (Mix (Mix (B, B), B),A)"
suffices rulesB (Mix (Mix (B, B), B), Mix (B, B)) |> prTest "suffices rulesB (Mix (Mix (B, B), B), Mix (B, B))"
suffices rulesB (Mix (Mix (A, B), B), Mix (B, A)) |> prTest "suffices rulesB (Mix (Mix (A, B), B), Mix (B, A))"
prRaw 0 "\n"


suffices rulesC (A, B) |> prTest "suffices rulesC (A, B)"
suffices rulesC (Mix (A, B), A) |> prTest "suffices rulesC (Mix (A, B),A)"

suffices rulesC (Mix (Mix (A, B), B),A) |> prTest "suffices rulesC (Mix (Mix (A, B), B),A)"
suffices rulesC (Mix (Mix (B, B), B),A) |> prTest "suffices rulesC (Mix (Mix (B, B), B),A)"
suffices rulesC (Mix (Mix (B, B), B), Mix (B, B)) |> prTest "suffices rulesC (Mix (Mix (B, B), B), Mix (B, B))"
suffices rulesC (Mix (Mix (A, B), B), Mix (B, A)) |> prTest "suffices rulesC (Mix (Mix (A, B), B), Mix (B, A))"





///////////////////////////////////////////////////////////////////////////
/// The following is the simulation of the labs you should use test your code.
//     NOTE: threads must never contact labs except via DoExp, and only when when the lab is unused.
//     This includes creating locks on the lab objects.
//     You do NOT need to change this class. (You can change it, but I'm likely to change it back during marking.)
//////////////////////////////////////////////////////////////////////////
type lab (labID, rules) =
    let busy = ref false
    let usingClID = ref None  // Stores the client currently using the lab, if there is one.

    member this.Rules = rules
    
    /// Send an experiment to a lab.  The continuation is called with the result of the experiment
    //  when it is complete.
    member this.DoExp delay (exp:exp) clID (continuation : bool->unit) =  
       startThread ("Lab"+ string labID) <| fun ()->
          if !busy then  let str = sprintf "BANG! lab%d explodes - host%d is already using it" labID (!usingClID).Value
                         prStamp -1 str "" //; failwith str // uncomment this to have the whole program fail.
          usingClID := Some clID              
          busy:=true
          sleep delay  // Doing experiment (the delay is provided by the client for ease of testing the prototype)
          busy:=false
          usingClID := None
          if random 2 = 1 then continuation true  else continuation false


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

                                                
///////////////////////////////////////////////////////////////////////////////////
/// Add your code for the second part here and below, including in the client class below.
////////////////////////////////////////////////////////////////////////////////////

// Hints:
// 1. You must be very careful here that your implementation is a suitable prototype for a decentralized system,
//    even though you are only building the prototype, not the final system.
// 2. One client's thread may call members on another client via the clients array - this is considered to 
//    indicate that in the final (non-prototype) system a request will sent via the network (and similarly for
//    the value returned by the member).   (This is usually called a Remote Method Invocation.)
// 3. Clients should NEVER pass (nor return) to each other data that is mutable or contains elements that are mutable.
//    Mutable data cannot be sent over the network in the final version.
// 4. Clients should never be contacted when they do not require a lab, and have no lab, except by other labs that
//    have been inactive and have yet to discover that the first client no longer holds a lab.
// 5. You will need a number of other members in the client class for clients to communicate with each other
//    in various situations.
// 6. You will need locking in some of the code in the this class, but be careful of deadlocks.
// 7. You will need some additional mutable state variables in the client class.
// 8. No static mutable variables are allowed, since these can't be decentralized. 
// 9. You will probably want to define a type for queues, but be careful not to pass mutable values between clients.
      

/// A class for clients that coordinate and unify experiments before sending them to labs.  
/// (You need to finish this class.)

type client (clientID, numLabs) =
    let clients:client[] ref = ref Array.empty
    let labs:lab[] ref = ref Array.empty
    /// The client coordinating each lab, according to the most recent information known by this client.
    let lastKnownCoord = Array.init numLabs (fun labID -> labID)  // Initially client i has lab i, for i=0..numLabs-1

    
    // printing functions for this client
    let prStr (pre:string) str = prIndStr clientID (sprintf "Client%d: %s" clientID pre) str 
    let pr (pre:string) res = prStr pre (sprintf "%A" res); res

    member this.ClientID = clientID  // So other clients can find our ID easily
    member this.InitClients theClients theLabs =  clients:=theClients; labs:=theLabs

    /// This will be called each time a scientist on this host wants to submit an experiment.
    member this.DoExp delay exp =    // You need to write this member.
        //  The following code doesn't coordinate the clients at all.  Replace it with code that does.
        let result = ref None
        (!labs).[0].DoExp delay exp clientID (fun res -> result:=Some res)
        while (!result).IsNone do ()  // This is busy waiting, which isn't allowed - you'll need to fix it.
        (!result).Value

    // Add any additional members for client here - you will at least need some that can be called from
    // other instances of the client class in order to coordinate requests.



//////////////////////////////////////////////////////////////// Top level testing code follows.      

// The following creates and initializes the instances of the lab and client classes
let mkClientsAndLabs numClients (labsRules: labRules list) = 
    let numLabs = labsRules.Length
    let clients = [| for i in 0..numClients-1 -> client (i, numLabs) |]
    let labs = [| for (i,rules) in List.zip [0..numLabs-1] labsRules -> lab (i,rules) |]
    Array.iter (fun (cl:client) -> cl.InitClients clients labs) clients
    (clients, labs)

let prClient cl pre str = prIndStr cl (sprintf "Host%d: %s" cl pre) str  // for client output



// Some simple testing code follows.  You almost certainly want to add your own tests to this.
// scheduledClient and randomTest are designed to make it easy to build other tests.

/// This function runs a test where the requests and times for each client are specified in a list (see below).
/// The list contains tuples (delayBeforeRequestTime, busyDoingExpTime, exp) in sequence.
let scheduledClient (clients:client[]) clID sched = mkThread ("Client"+string clID) <| fun () ->
    sched |> List.iteri (fun i (delay,busy,exp) ->
             sleep delay
             prClient clID "" (sprintf "requests %A" exp)
             prIndent clID (sprintf "Exp%d%c result:" clID (char (i+(int 'a')))) (clients.[clID].DoExp busy exp) |> ignore )
    prClient clID "COMPLETED EXPERIMENTS" ""
    
/// Run the threads for a test, and wait for them to finish.    
let doTest threads = startTime:=DateTime.Now
                     prStamp -1 "Test started" ""
                     List.iter (fun (th:Thread)-> th.Start()) threads
                     List.iter (fun (th:Thread)-> th.Join()) threads
                     prStamp -1 "" "ALL HOSTS COMPLETED"


// The following creates a random test via the scheduledTest class above.
let randBase() = match random 2 with 0->A | 1->B

let rec randTerm () =     
    let rec intern maxDepth = match random 2, maxDepth with
                          | 1, _ -> randBase()
                          | _, 1 -> randBase()
                          | _ -> Mix (intern (maxDepth-1), intern (maxDepth-1))
    Mix (intern 2, intern 2)
 
/// Create a random test thread for a client-ID with the given average times and number of experiment requests.
let randomTestClient clients clID avgWait avgBusyTime numExp =
    scheduledClient clients clID 
       ( List.map (fun _ -> (random avgWait*2, random avgBusyTime*2, randTerm() )) [1..numExp] )

let randomTest avgWait avgBusyTime numExp numClients labsRules =
    let clients, _ = mkClientsAndLabs numClients labsRules 
    doTest [for i in 0..numClients-1 -> randomTestClient clients i avgWait avgBusyTime numExp  ]

do let clients, _ = mkClientsAndLabs 5 [rulesA; rulesB] 
   doTest [scheduledClient clients 0 [(0, 500, A)];     // Request a lab at the very start, use it for "A" for 0.5 seconds
           scheduledClient clients 1 [(200, 300, Mix (Mix (A,Mix (A,A)),B))] ;   // Request after 0.2s, release 0.3s later.
                 
           scheduledClient clients 2 [(300, 200, Mix (A,Mix (A,A)))];   // These three will all be waiting for a lab.
           scheduledClient clients 3 [(400, 200, Mix (A,A))];           // Client 2 should include the others as guests.
           scheduledClient clients 4 [(400, 200, A)]
          ]


randomTest 10 50 4 8 [rulesB; rulesB] |> ignore            // A smaller random test.
randomTest 5 20 5 20 [rulesA; rulesB; rulesC] |> ignore    // A larger random test.
