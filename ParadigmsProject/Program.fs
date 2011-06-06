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
let myprint string  = prRaw -1 (sprintf string)

//A subsitition maps a Var to and exp
type substitution = (exp * exp) 
type substitutionlist = substitution list

//Loops through the given substitution list and returns the assigned value for the given variable, if it exists in the
//substitution list. If not, returns None.
let rec getAssigned (v:exp) sublist =
    match sublist with
    |[] -> None
    |x::xs -> 
        match x, v with
        |(Var(str1), a),Var(str2) -> 
            if str1 = str2 then 
                Some(a)
            else 
                getAssigned v xs
        |_-> None
            
        
//Just to aid printing neatly.
let mapParticleToString = function
    | A -> "A"
    | B-> "B" 
    | _ -> "WTF?" //What the fudge? Who put this particle in the machine? (Or who passed this function a Var or Mix?)
                
//Decomposes an experiment recursively to create a string representation. 
//Does not perform subsitutions for variables. See getExperimentAsStringWithSubs for that functionality.
let rec exptostring exp =
    match exp with
    |Mix(e1,e2) -> sprintf "Mix(%s,%s)" (exptostring e1) (exptostring e2)
    |A|B -> mapParticleToString exp
    |Var(x) -> sprintf "Var(\"%s\")" x

//Returns a string representation of the given substitution list.
let rec getSubListString sl = 
    match sl with 
    |None -> "|Substitution is None Type\n"  
    |Some([]) -> "|Empty substitution list\n" 
    |Some(sl) ->
        List.fold (fun acc (v, a) -> acc + (sprintf "|Sub List Entry: %s \t-> %s" (exptostring v) (exptostring a))) "" sl

//Prints a substitution list. Used this for testing.
let rec printSubList sl intendedresult = 
    match sl with 
    |None -> prRaw -1 (sprintf "%s\nIntended Result:\nSubstitution is None type\n" intendedresult)
    |Some([]) -> prRaw -1  (sprintf"Empty substitution list\nIntended Result:\n%s\n"intendedresult)
    |Some(sl) ->
        [for v, a in sl ->prRaw -1  (sprintf "Entry %s -> %s" (exptostring v) (exptostring a)) ] |> ignore
        prRaw -1 (sprintf "End of Substitution List\nIntended Result:\n%s \n" intendedresult)

//For string output. Recusively breakes down an experiment and turns it into a human-readable string
//Performs substitutions for Variables.
let rec getExperimentAsStringWithSubs exp (sl:substitutionlist option) = 
    match sl with 
    |None -> "Subsitution list was None, unable to substitute rule"
    |Some(s) -> 
        match exp with
        |Mix(e1,e2) -> sprintf "Mix(%s,%s)" (getExperimentAsStringWithSubs e1 sl) (getExperimentAsStringWithSubs e2 sl)
        |A|B -> mapParticleToString exp
        |Var(x) -> 
            match getAssigned (Var(x)) s with 
            |None -> sprintf "Var(%s)" x
            |Some(sub) -> sprintf "%s" (exptostring sub)
       
//For string output. (Convienience)
let rec getSufficesAsString (exp1, exp2) (sl:substitutionlist option) =
     sprintf "%s suffices %s" (getExperimentAsStringWithSubs exp1 sl) (getExperimentAsStringWithSubs exp2 sl)

//An empty 'Optionified' empty list for convenience
let newSubString = Some(List.empty)

//Unify. Takes two experiments and recursively pairs the items in it. 
//If there are variables in the experiments, it attempts to substitute it with items from the substitution list.
//It compares the unit expressions (A,B,Var) with the equivalent position and if the item matches, and all other items match in the same
//manner, the unification succeeds, and returns a new substitution list.  If there is no match, it returns None. 
//When comparing a Var with another type (including another Var), the substitution list is consulted and possbly updated. 
//For specific logic on how it works, the actual code is more explanatory.
let rec unify exp1 exp2 sublist = 
    match sublist with
    |None -> None                   //No sublist, so no subsitutions! (This occurs when unifys have been chained together, and the output of one is put as input to the next, and the first fails.
    |Some(sl) ->                    //A sublist, so we can continue
        match (getAssigned (exp1) sl), (getAssigned (exp2) sl) with //Get the assigned values, if they exist (Will only succeed for Var types)
        | None, Some(a) ->          //First was not a var or was not assigned, the second was a Var, 
            unify exp1 a sublist    //so repeat this unify with that substitution.
        | Some(a), None ->          //Second was not a var or was not assigned, the first was a Var,
            unify a exp2 sublist    //so repeat this unify with that substitution.
        | Some(a), Some(aa) ->  //Both vars are in the substitution list. This means that we need to check they have the same mapping, else a problem may have occured earlier.
            if a = aa then
                unify a aa sublist  //Have the same mapping. Because that mapping was generated by a unify call, it is already fully mapped.
            else
                None //(Not sure whether this state is reachable or not without a prior bug).
        |_ ->
            match exp1, exp2 with
            | Mix(e1,e2), Mix(e3,e4) ->                 //Two Mixes.
                 unify e1 e3 sublist |> unify e2 e4     //Unify the second pair in light of any substitutions in the first pair
            | e, Var(v) | Var(v), e ->                  //A non var expression and a var
                let a = getAssigned (Var(v)) sl         //Get the assigned value of the Var (None if not assigned.)
                match a with
                |None ->                    //The Var has not been assigned, so go ahead
                    Some(sl@[(Var(v),e)])   //and assign the other expression to it, and then put that in the subsitution list.
                |Some(a) -> 
                        unify e a sublist   //This variable has already been assigned to a different expression. 
                                            //This means we need to attempt unification of the variable and the expression to make sure they are compatible. (unifiable?)
            | A,B | B,A -> None             //Two different particles
            | A,A | B,B -> sublist          //Two identical particles
            | _,_ -> None                   //A mystery to us all. A mix and non-mix possibly.

//Returns a boolean indicating whether all vairbales have been resolved in the given proposal (pair of experiments).
//Not currently used in the program, kept for possible future use (unlikely)
let rec allVariablesAccountedFor proposal substitutions = 
    match proposal with
    |Mix(e1,e2) -> allVariablesAccountedFor e1 substitutions && allVariablesAccountedFor e2 substitutions
    |A|B -> true
    |Var(x) -> 
        match getAssigned (Var(x)) substitutions with
        |None -> false
        |_ -> true

//Generates a sequence of substitution lists that are generated from matching rules to the subgoal. This is a proper match in the sense that not only does the rule match, but all of it
//subgoals were successful and all resolved to a subgoal-less rule, in light of the given substitution list.
let rec GetValidRuleList rules subgoal (substitutionList: (exp * exp) list) checkSubGoalsSucceedFunc =
    seq {for rule in rules do                                                               //For each rule
                match rule(), subgoal with
                |Rule((r1, r2), subgoals), (p1,p2) ->                                       //Pattern match the executed rule.
                    match unify r1 p1 (Some(substitutionList)) |> unify r2 p2  with         //Attempt to unify it with the subgoal.
                    |None -> ()                                                             //No match, so don't ass this rule ot the list.
                    |Some(sl) ->                                                            //A Match!
                       yield! checkSubGoalsSucceedFunc rules subgoals sl}                   //Now confirm the subgoals, and after that return all the possible substitution lists
                                                                                            //that are generated from different successful logic paths. This will be empty if not
                                                                                            //all subgoals were succesful.


//Check subgoals is a function that runs recursively through a list of subgoals.
//If a particular branch fails, it rolls back and tries a different path (ie. multiple rules can possibly match the same subgoal, so if 
//one turns out to be no good, it continues trying with the other match.
let rec CheckSubGoals rules subgoals (substitutionList: (exp * exp) list) =
    match subgoals with
    |[] ->                      //No SubGoals, rule succeeded
        [substitutionList]
    |sg::sgs ->                 //Oh alright then, I'll do some work! (Subgoals are still unresolved)
        [for sl in (GetValidRuleList rules sg substitutionList CheckSubGoals) do    //Follow the white rabbit down the rabbithole for each rule that matches with the current subgoal, and 
            yield! CheckSubGoals rules sgs sl]                                      //with the result, try the each possibility with all following the subgoals, and yield the result. If any 
                                                                                    //one of them succeeds to the last, the result will be non-empty.
    

//Suffices
let rec suffices (rules : ruleGen list) (exp1, exp2) = 
    match CheckSubGoals rules [(exp1, exp2)] [] with
    |[] -> false
    |_ -> true
                       
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

suffices [rule1] (A, A) |> prTest "suffices [rule1] (A, A) should be true" 
suffices [rule1] (A, B) |> prTest "suffices [rule1] (A, B) should be false" 
prRaw 0 "\n"

suffices rulesA (A, B) |> prTest "suffices rulesA (A, B) should be false"
suffices rulesA (Mix (A, B), A) |> prTest "suffices rulesA (Mix (A, B),A) should be true"
//
suffices rulesA (Mix (Mix (A, B), B),A) |> prTest "suffices rulesA (Mix (Mix (A, B), B),A) should be true"
suffices rulesA (Mix (Mix (B, B), B),A) |> prTest "suffices rulesA (Mix (Mix (B, B), B),A) should be false"
suffices rulesA (Mix (Mix (B, B), B), Mix (B, B)) |> prTest "suffices rulesA (Mix (Mix (B, B), B), Mix (B, B)) should be true"
suffices rulesA (Mix (Mix (A, B), B), Mix (B, A)) |> prTest "suffices rulesA (Mix (Mix (A, B), B), Mix (B, A)) should be false"
prRaw 0 "\n"


suffices rulesB (A, B) |> prTest "suffices rulesB (A, B) should be false"
suffices rulesB (Mix (A, B), A) |> prTest "suffices rulesB (Mix (A, B),A) should be true"

suffices rulesB (Mix (Mix (A, B), B),A) |> prTest "suffices rulesB (Mix (Mix (A, B), B),A) should be true"
suffices rulesB (Mix (Mix (B, B), B),A) |> prTest "suffices rulesB (Mix (Mix (B, B), B),A) should be false"
suffices rulesB (Mix (Mix (B, B), B), Mix (B, B)) |> prTest "suffices rulesB (Mix (Mix (B, B), B), Mix (B, B)) should be true"
suffices rulesB (Mix (Mix (A, B), B), Mix (B, A)) |> prTest "suffices rulesB (Mix (Mix (A, B), B), Mix (B, A)) should be true"
prRaw 0 "\n"



suffices rulesC (A, B) |> prTest "suffices rulesC (A, B) should be true (Rule 11)"
suffices rulesC (Mix (A, B), A) |> prTest "suffices rulesC (Mix (A, B),A) should be true"
//
suffices rulesC (Mix (Mix (A, B), B),A) |> prTest "suffices rulesC (Mix (Mix (A, B), B),A) should be true"
suffices rulesC (Mix (Mix (B, B), B),A) |> prTest "suffices rulesC (Mix (Mix (B, B), B),A) should be true"
suffices rulesC (Mix (Mix (B, B), B), Mix (B, B)) |> prTest "suffices rulesC (Mix (Mix (B, B), B), Mix (B, B)) should be false"
suffices rulesC (Mix (Mix (A, B), B), Mix (B, A)) |> prTest "suffices rulesC (Mix (Mix (A, B), B), Mix (B, A)) should be false"

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

let logger (str:string) = System.Console.WriteLine(str)

// An enqueued experiment
type asyncExperiment (clientID, exp, delay) =
    member this.ClientID = clientID
    member this.Delay = delay
    member this.Experiment  = exp:exp
    member this.Timestamp = DateTime.Now

// Queue for a particular lab
type labQueue (labID) =
    // Internal working queue for lab
    let workQueue:asyncExperiment Queue ref = ref (Queue ())
    // Queuelength used when not actually running a lab
    let mutable queuelength = 0

    // Ref for workqueue count to be used as monitor for change of the queue length
    let cwq () = workQueue.Value.Count
    
    // Notify the listener working thread to stop.
    let stop_work = ref false
    
    // The internal queuelength
    let queuelength_internal = ref cwq

    // Recent result
    let result = ref None

    // semaphore
    let flag = ref false

    // Work loop to be run only from this.listener member theoretically allows one client to run multiple labs.
    let work_loop (alab:lab) = 
        while(not stop_work.Value) do
            lock queuelength_internal <| fun() -> 
                while(queuelength_internal.Value() = 0) do
                    printfn "Thread: %s is waiting" Thread.CurrentThread.Name
                    waitFor queuelength_internal
                logger (sprintf "Thread: %s woke up because of new data: " Thread.CurrentThread.Name)
                let query = workQueue.Value.Dequeue()
                logger "Launching Lab Experiment Request"
                lock flag <| fun() ->
                    while(flag.Value) do
                        flag := true
                        waitFor flag
                    logger "Got access to do experiment"
                    hLock result <| fun later -> let v = alab.DoExp query.Delay query.Experiment query.ClientID (fun(res) -> (lock result <| fun() -> result := Some res; logger "Finished Experiment"; wakeWaiters result))
                                                 later <| fun() -> lock result <| fun() -> 
                                                                                    while(result.Value.IsNone) do 
                                                                                        logger (sprintf "Thread: %s is waiting" Thread.CurrentThread.Name)
                                                                                        waitFor result
                                                                                    // Notification of results @TODO
                                                                                    result := None
                                                                                    logger "Got result"
                                                                                    wakeWaiters result
                    flag := false
                    wakeWaiters flag
                    
                wakeWaiters queuelength_internal
        ()
            
    // Enqueue and notify listener of new job
    member this.enqueue obj = lock queuelength_internal <| fun() -> 
        workQueue.Value.Enqueue obj
        wakeWaiters queuelength_internal

    // Dequeue, should not be used
    member this.dequeue = workQueue.Value.Dequeue

    // Start a new lab worker 
    member this.listener (alab:lab) = startThread "Lab Worker" (fun() -> (work_loop alab))
    member this.enqueuedExperiments = workQueue
    member this.LabID = labID

    // Get set the foreign queuelength entries
    member this.getQueueLength = queuelength
    member this.setQueueLength len = queuelength <- len



// Queue manager
type labQueueMan (numQueues) = 
    let labQueues:labQueue list ref = ref ([for i in [0..numQueues] -> labQueue i])
    do printfn "Creating labQueueMan for %d labs" numQueues

    member this.getQueues = !labQueues
    member this.queueForLab labID = [for lq in labQueues.Value do if lq.LabID = labID then yield lq].Head // Can only ever be 1
    
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
    
    
    
    let labControlled = ref None // None is I dont control a lab this gets reinitialised at InitClients.
    let runningLab = ref None // Initially I am not actually operating a lab even though I may have one.
    
    // Recursively call a client asking whether it owns the lab your looking for and if it doesnt ask it who they
    // think had it last. Once you have asked everyone in the path then return that client id
    let rec getCurrentLabOwner (cid:int) (lid:int) (foreignLastKnownCoord:int[]) =
        if clients.Value.[cid].ownsLab lid then
            cid
        else
            let nextForeignLastKnownCoord:int[] = clients.Value.[cid].getLastKnownCoord
            getCurrentLabOwner nextForeignLastKnownCoord.[lid] lid nextForeignLastKnownCoord  
    

    let runLab labid =
        ()


        
    member this.ClientID = clientID  // So other clients can find our ID easily
    member this.getLastKnownCoord = lastKnownCoord
    member this.getQueueForLab labid = queueManager.queueForLab labid // Probably mutable object however not using it as a shared data store and will clone it.
    
    // Determines if you own the Lab in question
    member this.ownsLab labid = lastKnownCoord.[labid] = this.ClientID
    
    member this.InitClients theClients theLabs =  clients:=theClients; labs:=theLabs; labControlled := (if ((Array.length !labs) - 1) < clientID then (None) else (Some clientID))
     

    member this.UpdateLabState (labid:int) (clientid:int) (queuelength:int) = 
        // Creates a new array with the updated clientid who owns that lab
        let lastKnownCoord = [|for labindex in [0..(lastKnownCoord.Length - 1)] do if labid = labindex then yield clientid else yield lastKnownCoord.[labindex]|]
        queueManager.queueForLab(labid).setQueueLength queuelength
        ()
    
    // Assumes that the caller has already checked for the most appropriate lab, basically just does what its told with no smarts.
    member this.EnqueueExperiment (experiment:asyncExperiment) (labid:int) =
        if this.ownsLab labid then
            // add the "exp" to the lab queue (i do this, since I have control of the lab)
            queueManager.queueForLab(labid).enqueuedExperiments.Value.Enqueue(experiment)
            // Call the originator of the experiment and update their last known coords for this lab
            clients.Value.[experiment.ClientID].UpdateLabState labid this.ClientID (queueManager.queueForLab(labid).enqueuedExperiments.Value.Count)
        else
            clients.Value.[lastKnownCoord.[labid]].EnqueueExperiment experiment labid
            // Call "EnqueueExpriment" on the "clientid" that I *think* has control of the lab
            // e.g. proxy/forward this request on
        ()

    // Gets information about the lab in question, this allows the caller to make an informed decision of which lab to enqueue at.
    member this.getLabQueueInformation clid labid =
        if this.ownsLab labid then
            // add the "exp" to the lab queue (i do this, since I have control of the lab)
            // Call the originator of the experiment and update their last known coords for this lab
            clients.Value.[clid].UpdateLabState labid this.ClientID (queueManager.queueForLab(labid).enqueuedExperiments.Value.Count)
        else
            clients.Value.[lastKnownCoord.[labid]].getLabQueueInformation clid labid
            // Call "getLabQueueInformation" on the "clientid" that I *think* has control of the lab
            // e.g. proxy/forward this request on
        ()
    
    // Priliminary result notifcation callback.
    member this.resultNotification (experiment:asyncExperiment) (result:bool) = 
        prStamp this.ClientID "DEBUG" (sprintf "Result for experiment: %s = %b"  (getExperimentAsStringWithSubs experiment.Experiment (Some [])) result)
    
    // This will be called each time a scientist on this host wants to submit an experiment.
    member this.DoExp delay (exp:exp) =    // You need to write this dick.
        let result = ref None
        let experiment = asyncExperiment(this.ClientID, exp, delay)
        
        // For each labid 
        for labid in [0..(numLabs - 1)] do
            // Update the information about each lab
            this.getLabQueueInformation this.ClientID labid
        
        // For each of the lab owners find out if anyone has an empty queue
        let freeLab = ref None

        freeLab :=
            try // Find the first client that owns a lab that has nothing in its queue (aka its free)
                Some (List.find (fun(labid) -> queueManager.queueForLab(labid).getQueueLength = 0) [0..(numLabs - 1)])
            with // List.find throws an exception if nothing is found. This means that there are no free labs.
                | :? KeyNotFoundException -> prStamp this.ClientID "DEBUG" "No Free Labs"; None
        
        if freeLab.Value.IsNone then
            // There are no free labs, need to enqueue to all labs.
            for labID in [0..(numLabs - 1)] do
                this.EnqueueExperiment experiment labID
        else
            
            // There is a free lab and I have a free lab (just use my free lab)
            if false then//(queueManager.queueForLab(this.ClientID).getQueueLength = 0) then
                ()            
            
            else
                // There is a lab available and I dont have one   
                if labControlled.Value.IsNone then
                    ()
                // Otherwise I have a lab already I should just continue processing. And enqueue to all labs.
                else
                    for labID in [0..(numLabs - 1)] do
                        this.EnqueueExperiment experiment labID
                    if runningLab.Value.IsNone then
                        // What we have a lab and need to do work and havent started using our lab!
                        // Start lab.
                        queueManager.queueForLab(labControlled.Value.Value).listener(labs.Value.[labControlled.Value.Value])
                        ()
                    else // Its ok we are working...
                        ()
                    ()
                ()
            ()
        ()

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
       ( List.map (fun _ -> (random avgWait*2, random avgBusyTime*2, randTerm () )) [1..numExp] )

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

randomTest 10 50 4 5 [rulesB; rulesB] |> ignore
//randomTest 10 50 4 2 [rulesB; rulesB] |> ignore
//randomTest 10 50 4 8 [rulesB; rulesB] |> ignore            // A smaller random test.
//randomTest 5 20 5 20 [rulesA; rulesB; rulesC] |> ignore    // A larger random test.
