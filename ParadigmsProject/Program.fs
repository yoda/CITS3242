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
type substitution = (exp * exp) 
type substitutionlist = substitution list
// Suffices checks whether exp1 suffices instead of exp2 according to rules.\
//suffices rulesA (Mix (Mix (A, B), B),A) |> prTest "suffices rulesA (Mix (Mix (A, B), B),A)"
//
//let rule2 () = let x, xx, y = newVar3 ("x", "xx", "y")
//               Rule ((Mix(x,xx), y),  [(x,y)])
let rec matchRule exp1 exp2 = 
    match exp1, exp2 with
    |Mix(e1,e2),Mix(e3,e4) -> matchRule e1 e3 && matchRule e2 e4
    |_ -> false //TODO need to check the basic pattern of the thing matches

let rec suffices rules (exp1, exp2) = 
    //unify 
    match rules with
    |[] -> true
    |x::xs -> false
    //false //go through list or something to determine suffices
                
                

//let reconcileSubstitutions slist2 = 
//    match slist1, slist2 with
//    | 

//let sameparticles exp1 exp2 = 
//    match exp1, exp2 with
//    | A,e2 | e2,A -> match e2 with
//                     | Var -> Some(Var, A)
//                     | A -> Some()
//                     | _ -> None
//    | B,e2 | e2,B -> match e2 with
//                     | Var -> Some(Var, B)
//                     | B -> Some()
//                     | _ -> None
//    | e1,e2 -> e1 = e2
//    

//So far the only bit of code I'd have any confidence in so far... ;)
let rec getAssigned v sublist =
    match sublist with
    |[] -> None
    |x::xs -> 
        match x with
        |v1, a -> 
            if v = v1 then 
                Some(a)
            else 
                getAssigned v xs
               
            
//Just to aid printing neatly.
let mapParticleToString = function
    | A -> "A"
    | B-> "B" 
    | _ -> "WTF?" //What the fudge? Who put this particle in the machine?
                
//Decomposes an experiment recursively to create a string representation
let rec exptostring exp =
    match exp with
    |Mix(e1,e2) -> sprintf "Mix(%s,%s)" (exptostring e1) (exptostring e2)
    |A|B -> mapParticleToString exp
    |Var(x) -> sprintf "Var(\"%s\")" x

//Unify
let rec unify exp1 exp2 sublist = 
    prRaw -1 (sprintf "Unify Called with %s and %s and subList %O " (exptostring exp1) (exptostring exp2) sublist) |> ignore
    match exp1, exp2, sublist with
    | _,_,None -> 
        prRaw -1 (sprintf "_ _ None!!") |> ignore
        None
    | Mix(e1,e2), Mix(e3,e4), s -> 
        prRaw -1 (sprintf "mix %s %s and mix %s %s being unified "(exptostring e1) (exptostring e2) (exptostring e3) (exptostring e4)) |> ignore
        unify e2 e4 (unify e1 e3 s)  //Unify the second pair in light of any substitutions in the first pair
    | Var(v1), Var(v2), Some(s) -> 
        let a1 = getAssigned v1 s
        let a2 = getAssigned v2 s
        prRaw -1 (sprintf "var %s and var %s being unified " v1 v2) |> ignore
        match a1, a2 with
        | None, Some(a) -> 
            prRaw -1 (sprintf "One found in sublist ") |> ignore
            Some(s @ [(v1,a)])  //The second var is in the substitution list, the first one isn't. This means we can assign the second var's mapping to the first var's mapping as well.
        | Some(a), None -> 
            prRaw -1 (sprintf "One found in sublist") |> ignore
            Some(s @ [(v2,a)])  //The first var is in the substitution list, the second one isn't. This means we can assign the first var's mapping to the second var's mapping as well.
        | None, None -> 
            prRaw -1 (sprintf "Neither found in sublist ") |> ignore
            sublist //Neither var is in the substitution list, so we can resolve it any further, just return the existing mapping.
        | Some(a), Some(aa) ->  //Both vars are in the substitution list. This means that we need to check they have the same mapping, else a problem may have occured earlier.
        
            if a = aa then
                prRaw -1 (sprintf "Both found and both are equal ") |> ignore
                sublist
            else
                prRaw -1 (sprintf "Both found and they are different, maybe a bug?") |> ignore
                None //(Not sure whether this state is reachable or not without a prior bug).
    | e, Var(v), Some(s) | Var(v), e, Some(s) -> //A non var expression and a var
        prRaw -1 (sprintf "var %s and exp %O being unified " v e) |> ignore
        let a = getAssigned v s
        match a with
        |None ->
            prRaw -1 (sprintf "No match found for %s " v) |> ignore 
            let z = Some(s@[(v,e)])
            prRaw -1 (sprintf "%O " z) |> ignore 
            z
        |Some(a) -> 
            prRaw -1 (sprintf "Match found for %s " v) |> ignore 
            if e = a then
                sublist
            else
                None //This variable has already been assigned to a different expression. #TDDO Should this be a unification attempt of the assigned expression and the current expression? :s
    | A,B, s | B,A, s -> None //Two different particles
    | A,A, s | B,B, s -> s    //Two identical particles
    | _,_, _ -> None          //A mystery to us all.

    

//Some quick tests
let noneString = "Substitution is None Type"

let rec printSubList sl intendedresult = 
    match sl with 
    |None -> prRaw -1 (sprintf "%s\nIntended Result:\n%s\n"noneString intendedresult)
    |Some([]) -> prRaw -1  (sprintf"Empty substitution list\nIntended Result:\n%s\n"intendedresult)
    |Some(sl) ->
        [for v, a in sl ->prRaw -1  (sprintf "Entry %s -> %s" v (mapParticleToString a)) ] |> ignore
        prRaw -1 (sprintf "End of Substitution List\nIntended Result:\n%s \n" intendedresult)
        
printSubList (unify (Mix(A, B)) (Mix(B, A)) (Some(List.empty))) noneString |> ignore //Simple test Works 
printSubList (unify (Var("a")) (A) (Some(List.empty))) "Entry a -> A"|> ignore //Simple test with variable assignment Works (true)
printSubList (unify (Var("a")) (A) (Some([("b", B)]))) "Entry b -> B\nEntry a -> A"|> ignore //Simple test with irrelevant variable assignemnt Works (true)
printSubList (unify (Var("anew")) (A) (Some([("aold", A)]))) "Entry t -> A\n(Make sure no duplicates)"|> ignore //Simple test with correct variable preassignment Works (true)
printSubList (unify (Var("b")) (A) (Some([("b", B)])))  noneString|> ignore //Simple test with incorrect variable preassignment Works (false)
printSubList (unify (Mix(Var("b"), A)) (Mix(B, A)) (Some(List.empty))) "Entry b -> B"|> ignore //Doesn't work :(
//let rule1 () = let x = newVar "x"
//               Rule ((x, x), [])
//let rule1 () = let x = newVar "x"
//               Rule ((x, x), [])                       
//suffices [rule1] (A, A) |> prTest "suffices [rule1] (A, A)" 

                       
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

// Need a queue structure to hold the current queues for each of the labs needs to be mirrored on all clients
// Will need to have:
// Client ID that is requesting the experiment.
// Experiment being requested.
// Maybe the time its being requested.
// Lab1 Queue [_,_,_,_,_]
// Lab2 Queue [_,_,_,_,_]
// Lab3 Queue [_,_,_,_,_]
// Lab4 Queue [_,_,_,_,_]
// LabN Queue [_,_,_,_,_]

// An enqueued experiment
type enqueuedExperiment (clientID, exp) =
    member this.ClientID = clientID
    member this.Experiment = exp
    member this.Timestamp = DateTime.Now

// Queue for a particular lab
type labQueue (labID) =
    let enqueuedExperiments:enqueuedExperiment list ref = ref List.empty
    member this.LabID = labID
    member this.Enqueue exp = enqueuedExperiments := List.append !enqueuedExperiments [exp] // Takes in an enqueuedExperiment
    member this.getQueue = enqueuedExperiments

// Queue manager
type labQueueMan (numQueues) = 
    let labQueues:labQueue list ref = ref ([for i in [0..numQueues] -> labQueue i])
    do printfn "Creating labQueueMan for %d labs" numQueues
    member this.getQueues = !labQueues
    member this.queueForLab labID = [for lq in labQueues.contents do if lq.LabID = labID then yield lq].Head // Can only ever be 1
    
prRaw 5 "Queue Tests"
prRaw 5 "Using 5 labs"
let z = labQueueMan 5
let labToGet = 2
prRaw 5 (sprintf "Attempting to get lab: %d" labToGet)
prRaw 5 (sprintf "LabID of found lab: %d" (z.queueForLab labToGet).LabID)
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
let prClient cl pre str = prIndStr cl (sprintf "Host%d: %s" cl pre) str  // for client output

type client (clientID, numLabs) =
    let clients:client[] ref = ref Array.empty
    let labs:lab[] ref = ref Array.empty
    /// The client coordinating each lab, according to the most recent information known by this client.
    let lastKnownCoord = Array.init numLabs (fun labID -> labID)  // Initially client i has lab i, for i=0..numLabs-1

    
    // printing functions for this client
    let prStr (pre:string) str = prIndStr clientID (sprintf "Client%d: %s" clientID pre) str 
    let pr (pre:string) res = prStr pre (sprintf "%A" res); res

    // Setup the lab queue manager for this client
    let queueManager:labQueueMan = labQueueMan (numLabs)

    let isFreeLab:bool = Array.exists (fun lkc -> lkc < 0) lastKnownCoord
    let getFreeLab:labID = if isFreeLab then Array.get [|for x in lastKnownCoord do if x < 0 then yield x|] 0 else -1
    let mutable labControlled = -1 // -1 is I dont control a lab
    let mutable experimentCompleted = false

    let completeExperimentJob = experimentCompleted <- true
    

    do printfn "Last known coords"
    do lastKnownCoord |> Array.iter (fun lkc -> (printfn "%d is in a lab" lkc))
    member this.ClientID = clientID  // So other clients can find our ID easily
    member this.InitClients theClients theLabs =  clients:=theClients; labs:=theLabs; labControlled <- if ((Array.length !labs) - 1) < clientID then -1 else clientID 
    
    
   
    // This will be called each time a scientist on this host wants to submit an experiment.
    member this.DoExp delay exp =    // You need to write this dick.
        experimentCompleted <- false
        let result = ref None
        if labControlled > -1 // If i control a lab then do it
            then 
                prClient clientID "DEBUG" (sprintf "Attempting to do experiment")
                let currentLab = labs.contents.[labControlled]
                hLock currentLab <| fun labJob ->
                    // The continueation function fed into DoExp lab function needs to notifiy the caller of the result
                    let experimentJob = currentLab.DoExp delay exp clientID (fun res -> result:= Some res; completeExperimentJob) // After the thread that got the lock does its work then it can report its results
                    // After calling the experiment thread continue with...
                    labJob <| fun() -> experimentJob; prClient clientID "DEBUG" (sprintf "Ran Experiment, Completed?: %b" experimentCompleted)
            else 
                (queueManager.queueForLab 0).Enqueue( enqueuedExperiment(this.ClientID, exp)) |> ignore
       
        if experimentCompleted
            then
                experimentCompleted <- false
                prClient clientID "DEBUG" (sprintf "Succeeded at doing experiment")
                (!result).Value
            else
                prClient clientID "DEBUG" (sprintf "Failed at doing experiment")
                false
                
                
            

       
       
       
        

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





// Some simple testing code follows.  You almost certainly want to add your own tests to this.
// scheduledClient and randomTest are designed to make it easy to build other tests.

/// This function runs a test where the requests and times for each client are specified in a list (see below).
/// The list contains tuples (delayBeforeRequestTime, busyDoingExpTime, exp) in sequence.
let scheduledClient (clients:client[]) clID sched = mkThread ("Client"+string clID) <| fun () ->
    sched |> List.iteri (fun i (delay,busy,exp) -> // For each of the scheduled list 
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
    let rec intern maxDepth = 
        match random 2, maxDepth with
        | 1, _ -> randBase()
        | _, 1 -> randBase()
        | _ -> Mix (intern (maxDepth-1), intern (maxDepth-1))
    Mix (intern 2, intern 2)
 
/// Create a random test thread for a client-ID with the given average times and number of experiment requests.
// clID is an integer 0 -> numClients (array index)
let randomTestClient clients clID avgWait avgBusyTime numExp = 
    // scheduledClient is function
    scheduledClient clients clID 
    // Build a schedule randomising params for 1..numExp
       ( List.map (fun _ -> (random avgWait*2, random avgBusyTime*2, randTerm() )) [1..numExp] )

// avgWait is the average wait time in the experiments
// avgBusyTime is the average time it takes to complete the experiment
// numExp is the number of experiments
// numClients is the number of clients and labs?
// labsRules is the rules for the labs
let randomTest avgWait avgBusyTime numExp numClients (labsRules: labRules list) =
    prRaw 5 "================================================================="
    prRaw 5 "Doing Random Test"
    prRaw 5 (sprintf "Number of Experiments: %d" numExp)
    prRaw 5 (sprintf "Number of Clients: %d" numClients)
    prRaw 5 (sprintf "Number of Labs: %d" (labsRules.Length))
    prRaw 5 "================================================================="
    let clients, _ = mkClientsAndLabs numClients labsRules  // Make clients and labs equal in number with the labsRules
    // For each of the client id's do a randomTestClient 
    doTest [for i in 0..numClients-1 -> randomTestClient clients i avgWait avgBusyTime numExp  ]


//do let clients, _ = mkClientsAndLabs 5 [rulesA; rulesB] 
//   doTest [scheduledClient clients 0 [(0, 500, A)];     // Request a lab at the very start, use it for "A" for 0.5 seconds
//           scheduledClient clients 1 [(200, 300, Mix (Mix (A,Mix (A,A)),B))] ;   // Request after 0.2s, release 0.3s later.
//                 
//           scheduledClient clients 2 [(300, 200, Mix (A,Mix (A,A)))];   // These three will all be waiting for a lab.
//           scheduledClient clients 3 [(400, 200, Mix (A,A))];           // Client 2 should include the others as guests.
//           scheduledClient clients 4 [(400, 200, A)]
//          ]

randomTest 10 50 4 1 [rulesB] |> ignore
//randomTest 10 50 4 2 [rulesB; rulesB] |> ignore
//randomTest 10 50 4 8 [rulesB; rulesB] |> ignore            // A smaller random test.
//randomTest 5 20 5 20 [rulesA; rulesB; rulesC] |> ignore    // A larger random test.
