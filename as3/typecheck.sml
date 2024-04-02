signature TYPECHECK =
sig
  val tc : Absyn.prog -> unit
  (* if there are errors, these are reported through ErrorMsg.error *)

  val sub: Absyn.tp * Absyn.tp -> bool
  val join: (string->unit) -> Absyn.tp * Absyn.tp -> Absyn.tp
end

structure TypeCheck :> TYPECHECK =
struct

 structure A = Absyn
     
 fun list2string nil = ""
   | list2string [t] = t
   | list2string (h::t) = h ^ "," ^ list2string t

 fun tp2string A.Inttp = "int"
   | tp2string (A.Tupletp tps) = "<" ^ (list2string (map tp2string tps)) ^ ">"
   | tp2string (A.Arrowtp (tp1, tp2)) = tp2string tp1 ^ " -> " ^ tp2string tp2
   | tp2string (A.Reftp tp) = tp2string tp ^ " ref"

 type context = A.tp Symbol.table

 exception UNIMPLEMENTED

(* subtyping *)
 fun sub (t1,t2) =
  let
   fun width(tl1, tl2) = if length(tl1) >= length(tl2) then true else false
   fun depth(tl1, tl2) = 
      case tl2
      of tp2::tl2' => (case tl1 of tp1::tl1'=> if sub(tp1,tp2) then depth(tl1',tl2') else false | _ => false)
      | nil => true
  in
   case t1
   of A.Inttp => if t2= A.Inttp then true else false
    | A.Tupletp tps1 => 
        (case t2 of A.Tupletp tps2 => if width(tps1, tps2) then depth (tps1, tps2) else false
                  | _ => false)
    | A.Arrowtp (t1p1, t1p2) => (case t2 of A.Arrowtp (t2p1, t2p2) => if sub(t2p1,t1p1) andalso sub(t1p2,t2p2) then true else false 
                                 | _ => false)
    | A.Reftp tp1 => (case t2 of A.Reftp tp2 => if tp1=tp2 then true else false | _ => false)
  end

 fun check_sub pos (tp1, tp2) = 
   if sub (tp1, tp2) then ()
   else ErrorMsg.error (pos, "make a better error message than this!")

(* subtype join *)
 fun join complain (t1,t2) : A.tp = 
  let
   fun check_join (st1,st2) =  (*check if st1 and st2 can be joined or not *)
      case st1
      of A.Inttp => if sub(st1,st2) then true else false
      | A.Tupletp tps1 => 
                 (case st2 of A.Tupletp tps2 => true
                  | _ => false)
      | A.Arrowtp (t1p1, t1p2) =>
        (case st2 of A.Arrowtp (t2p1, t2p2) => check_join(t1p1,t2p1) andalso check_join(t1p2,t2p2)
                  | _ => false)
      | A.Reftp tp1 =>
        (case st2 of A.Reftp tp2 => if tp1=tp2 then true else false
                  | _ => false)
   fun rec_join complain (tl1, tl2)  = (* Join tl1 list and tl2 list *)
      case tl1
      of tp1::tl1' => 
          (case tl2 of tp2::tl2' => if check_join(tp1,tp2) then join complain (tp1,tp2)::(rec_join complain (tl1', tl2')) else nil
           | nil => nil)
       | nil => nil
  in
   case t1
   of A.Inttp => if sub(t1,t2) then A.Inttp 
                 else (complain ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1)
    | A.Tupletp tps1 => 
        (case t2 of A.Tupletp tps2 => A.Tupletp (rec_join complain (tps1, tps2))
                  | _ => (complain ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1))
    | A.Arrowtp (t1p1, t1p2) =>
        (case t2 of A.Arrowtp (t2p1, t2p2) => A.Arrowtp (join complain (t1p1,t2p1), join complain (t1p2,t2p2))
                  | _ => (complain ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1))
    | A.Reftp tp1 => 
        (case t2 of A.Reftp tp2 => if tp1=tp2 then t1 
                             else (complain ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1)
                  | _ => (complain ("t1(" ^ tp2string t1 ^ ") and t2(" ^ tp2string t2 ^ ") cannot join"); t1))
  end
 
 fun complain pos err = ErrorMsg.error (pos, err)

(* expression typing *)
 fun tc_exp ctxt pos e : A.tp =
   (case e
     of A.Int num => A.Inttp
      | A.Id id => 
          (case Symbol.look (ctxt, id)
          of SOME x => x
              | NONE => (ErrorMsg.error (pos, "ID: Type error"); A.Inttp))
      | A. Op (oper, explist) =>
          (case (oper, map (fn x => tc_exp ctxt pos x) explist)
            of (A.Add, [A.Inttp, A.Inttp]) => A.Inttp
             | (A.Sub, [A.Inttp, A.Inttp]) => A.Inttp
             | (A.Mul, [A.Inttp, A.Inttp]) => A.Inttp
             | (A.LT, [A.Inttp, A.Inttp]) => A.Inttp
             | (A.Eq, [A.Inttp, A.Inttp]) => A.Inttp
             | (A.Ref, [tp]) => A.Reftp tp
             | (A.Get, [A.Reftp tp]) => tp
             | (A.Set, [A.Reftp tp1, tp2]) => A.Tupletp []
             | (_, _) => (ErrorMsg.error (pos, "Op: type error"); A.Inttp))            
      | A.Tuple explist => A.Tupletp (map (fn x => tc_exp ctxt pos x) explist)
      | A.Proj (num, exp) =>
          (case tc_exp ctxt pos exp
            of A.Tupletp tplist => List.nth (tplist, num))
      | A.If (exp1, exp2, exp3) =>
          (case (tc_exp ctxt pos exp2, tc_exp ctxt pos exp3)
          of (A.Tupletp [], _) => A.Tupletp []
              | (tp2, tp3) => join (fn x => ErrorMsg.error (pos, x)) (tp2, tp3))
      | A.While (exp1, exp2) =>
          (case (tc_exp ctxt pos exp1, tc_exp ctxt pos exp2)
            of (A.Inttp, A.Tupletp []) => A.Tupletp[])
      | A.Call (exp1, exp2) =>
          (case (tc_exp ctxt pos exp1, tc_exp ctxt pos exp2)
            of (A.Arrowtp (tp1, tp2), tp3) => 
            if sub (tp3, tp1) then tp2
            else (ErrorMsg.error (pos, "Call: Type error"); A.Inttp))
      | A.Let (id, exp1, exp2) =>
          let
            val tp1 = tc_exp ctxt pos exp1
            val ctxt' = Symbol.enter (ctxt, id, tp1)
          in
            tc_exp ctxt' pos exp2
          end
      | A.Constrain (exp, tp) => tp
      | A.Pos (pos, exp) => tc_exp ctxt pos exp) 
         


 fun tc_fundec ctxt ((pos, (f, x, tp1, tp2, exp)): A.fundec) =
 let
   val ctxt' = Symbol.enter(ctxt,x,tp1)
     val tp = tc_exp ctxt' pos exp
 in check_sub pos (tp, tp2)
 end 

 fun do_another_fun ((pos, fdec), ctxt) =
  let val (f, x, tp1, tp2, exp) = fdec
  in 
    case Symbol.look(ctxt, f) 
    of SOME x => (ErrorMsg.error(pos,"function name (" ^ Symbol.name f ^ ") is duplicated"); ctxt)  (*check if the function name is duplicated *)
     | NONE => if (Symbol.name f) = "main" then   (*check if main function has int->int type *)
                   if (tp1 = A.Inttp) andalso (tp2 = A.Inttp) then Symbol.enter(ctxt,f, A.Arrowtp (tp1, tp2))
                   else (ErrorMsg.error(pos,"main function has wrong type"); Symbol.enter(ctxt,f, A.Arrowtp (tp1, tp2)))
               else Symbol.enter(ctxt,f, A.Arrowtp (tp1, tp2))
 end

 fun build_global_context (fundecs) =
  foldl do_another_fun (Symbol.enter(Symbol.empty,Symbol.symbol("printint"), A.Arrowtp (A.Inttp, A.Tupletp nil))) fundecs

 fun tc (fundecs : A.prog)  = 
  let val ctxt = build_global_context(fundecs) 
   in app (tc_fundec ctxt) fundecs
  end 

end
