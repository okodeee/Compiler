functor FunLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Fun_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn
structure S = Symbol


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\094\000\006\000\047\000\007\000\046\000\008\000\094\000\
\\009\000\045\000\010\000\044\000\011\000\043\000\013\000\042\000\
\\014\000\041\000\016\000\040\000\018\000\039\000\020\000\094\000\
\\023\000\038\000\025\000\094\000\026\000\094\000\028\000\094\000\
\\029\000\094\000\000\000\
\\001\000\001\000\007\000\029\000\005\000\000\000\
\\001\000\002\000\037\000\003\000\036\000\007\000\035\000\010\000\034\000\
\\012\000\033\000\017\000\032\000\019\000\031\000\021\000\030\000\
\\022\000\029\000\024\000\028\000\027\000\027\000\000\000\
\\001\000\003\000\008\000\000\000\
\\001\000\003\000\010\000\000\000\
\\001\000\003\000\015\000\007\000\014\000\000\000\
\\001\000\003\000\049\000\000\000\
\\001\000\005\000\048\000\006\000\047\000\007\000\046\000\008\000\072\000\
\\009\000\045\000\010\000\044\000\011\000\043\000\013\000\042\000\
\\014\000\041\000\016\000\040\000\018\000\039\000\023\000\038\000\000\000\
\\001\000\005\000\048\000\006\000\047\000\007\000\046\000\008\000\073\000\
\\009\000\045\000\010\000\044\000\011\000\043\000\013\000\042\000\
\\014\000\041\000\016\000\040\000\018\000\039\000\023\000\038\000\000\000\
\\001\000\005\000\048\000\006\000\047\000\007\000\046\000\009\000\045\000\
\\010\000\044\000\011\000\043\000\013\000\042\000\014\000\041\000\
\\016\000\040\000\018\000\039\000\020\000\071\000\023\000\038\000\000\000\
\\001\000\005\000\048\000\006\000\047\000\007\000\046\000\009\000\045\000\
\\010\000\044\000\011\000\043\000\013\000\042\000\014\000\041\000\
\\016\000\040\000\018\000\039\000\023\000\038\000\025\000\070\000\000\000\
\\001\000\005\000\048\000\006\000\047\000\007\000\046\000\009\000\045\000\
\\010\000\044\000\011\000\043\000\013\000\042\000\014\000\041\000\
\\016\000\040\000\018\000\039\000\023\000\038\000\026\000\078\000\000\000\
\\001\000\005\000\048\000\006\000\047\000\007\000\046\000\009\000\045\000\
\\010\000\044\000\011\000\043\000\013\000\042\000\014\000\041\000\
\\016\000\040\000\018\000\039\000\023\000\038\000\028\000\077\000\000\000\
\\001\000\006\000\011\000\000\000\
\\001\000\006\000\021\000\000\000\
\\001\000\007\000\009\000\000\000\
\\001\000\008\000\018\000\021\000\017\000\030\000\016\000\000\000\
\\001\000\008\000\022\000\021\000\017\000\030\000\016\000\000\000\
\\001\000\014\000\024\000\021\000\017\000\030\000\016\000\000\000\
\\001\000\014\000\069\000\000\000\
\\001\000\029\000\005\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\005\000\048\000\006\000\047\000\007\000\046\000\009\000\045\000\
\\010\000\044\000\011\000\043\000\013\000\042\000\014\000\041\000\
\\016\000\040\000\018\000\039\000\023\000\038\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\021\000\017\000\030\000\016\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\095\000\007\000\046\000\000\000\
\\096\000\007\000\046\000\009\000\045\000\010\000\044\000\011\000\043\000\
\\013\000\042\000\014\000\041\000\000\000\
\\097\000\007\000\046\000\000\000\
\\098\000\007\000\046\000\000\000\
\\099\000\007\000\046\000\011\000\043\000\000\000\
\\100\000\007\000\046\000\011\000\043\000\000\000\
\\101\000\007\000\046\000\000\000\
\\102\000\007\000\046\000\009\000\045\000\010\000\044\000\011\000\043\000\
\\013\000\042\000\014\000\041\000\000\000\
\\103\000\007\000\046\000\009\000\045\000\010\000\044\000\011\000\043\000\
\\013\000\042\000\014\000\041\000\000\000\
\\104\000\007\000\046\000\009\000\045\000\010\000\044\000\011\000\043\000\000\000\
\\105\000\007\000\046\000\009\000\045\000\010\000\044\000\011\000\043\000\000\000\
\\106\000\006\000\047\000\007\000\046\000\009\000\045\000\010\000\044\000\
\\011\000\043\000\013\000\042\000\014\000\041\000\016\000\040\000\
\\018\000\039\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\021\000\017\000\030\000\016\000\000\000\
\\110\000\006\000\047\000\007\000\046\000\009\000\045\000\010\000\044\000\
\\011\000\043\000\013\000\042\000\014\000\041\000\016\000\040\000\
\\018\000\039\000\023\000\038\000\000\000\
\\111\000\006\000\047\000\007\000\046\000\009\000\045\000\010\000\044\000\
\\011\000\043\000\013\000\042\000\014\000\041\000\016\000\040\000\
\\018\000\039\000\023\000\038\000\000\000\
\\112\000\005\000\048\000\006\000\047\000\007\000\046\000\009\000\045\000\
\\010\000\044\000\011\000\043\000\013\000\042\000\014\000\041\000\
\\016\000\040\000\018\000\039\000\023\000\038\000\000\000\
\\113\000\007\000\046\000\000\000\
\"
val actionRowNumbers =
"\021\000\002\000\023\000\004\000\
\\024\000\022\000\016\000\005\000\
\\014\000\006\000\027\000\017\000\
\\006\000\026\000\006\000\029\000\
\\015\000\018\000\028\000\006\000\
\\030\000\019\000\003\000\046\000\
\\025\000\007\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\032\000\033\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\006\000\003\000\020\000\
\\011\000\036\000\052\000\010\000\
\\035\000\037\000\034\000\008\000\
\\045\000\042\000\041\000\043\000\
\\044\000\040\000\039\000\038\000\
\\009\000\048\000\001\000\003\000\
\\003\000\003\000\031\000\047\000\
\\013\000\012\000\050\000\003\000\
\\003\000\051\000\049\000\000\000"
val gotoT =
"\
\\001\000\079\000\002\000\002\000\003\000\001\000\000\000\
\\002\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\011\000\005\000\010\000\000\000\
\\000\000\
\\000\000\
\\004\000\017\000\005\000\010\000\000\000\
\\000\000\
\\004\000\018\000\005\000\010\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\021\000\005\000\010\000\000\000\
\\000\000\
\\000\000\
\\006\000\024\000\007\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\048\000\007\000\023\000\000\000\
\\006\000\049\000\007\000\023\000\000\000\
\\006\000\050\000\007\000\023\000\000\000\
\\006\000\051\000\007\000\023\000\000\000\
\\006\000\052\000\007\000\023\000\000\000\
\\006\000\053\000\007\000\023\000\000\000\
\\006\000\054\000\007\000\023\000\000\000\
\\006\000\055\000\007\000\023\000\000\000\
\\000\000\
\\000\000\
\\006\000\056\000\007\000\023\000\000\000\
\\006\000\057\000\007\000\023\000\000\000\
\\006\000\058\000\007\000\023\000\000\000\
\\006\000\059\000\007\000\023\000\000\000\
\\006\000\060\000\007\000\023\000\000\000\
\\006\000\061\000\007\000\023\000\000\000\
\\006\000\062\000\007\000\023\000\000\000\
\\006\000\063\000\007\000\023\000\000\000\
\\006\000\064\000\007\000\023\000\000\000\
\\004\000\065\000\005\000\010\000\000\000\
\\006\000\066\000\007\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\072\000\007\000\023\000\000\000\
\\006\000\073\000\007\000\023\000\000\000\
\\006\000\074\000\007\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\077\000\007\000\023\000\000\000\
\\006\000\078\000\007\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 80
val numrules = 32
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = ErrorMsg.pos
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | PROJ of unit ->  (int) | ID of unit ->  (string)
 | INT of unit ->  (int) | exps of unit ->  (A.exp list)
 | exp of unit ->  (A.exp) | tps of unit ->  (A.tp list)
 | tp of unit ->  (A.tp) | fundeclist of unit ->  (A.fundec list)
 | fundec of unit ->  (A.fundec) | prog of unit ->  (A.prog)
end
type svalue = MlyValue.svalue
type result = A.prog
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 18) => true | (T 19) => true | (T 23) => true | (T 24) => true
 | (T 25) => true | (T 26) => true | (T 27) => true | (T 28) => true
 | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 24))::
(nil
,nil
 $$ (T 25))::
(nil
,nil
 $$ (T 6))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "INT"
  | (T 2) => "ID"
  | (T 3) => "COMMA"
  | (T 4) => "SEMICOLON"
  | (T 5) => "COLON"
  | (T 6) => "LPAREN"
  | (T 7) => "RPAREN"
  | (T 8) => "PLUS"
  | (T 9) => "MINUS"
  | (T 10) => "TIMES"
  | (T 11) => "PROJ"
  | (T 12) => "LT"
  | (T 13) => "EQ"
  | (T 14) => "GT"
  | (T 15) => "AND"
  | (T 16) => "NOT"
  | (T 17) => "OR"
  | (T 18) => "WHILE"
  | (T 19) => "DO"
  | (T 20) => "REF"
  | (T 21) => "BANG"
  | (T 22) => "ASSIGN"
  | (T 23) => "IF"
  | (T 24) => "THEN"
  | (T 25) => "ELSE"
  | (T 26) => "LET"
  | (T 27) => "IN"
  | (T 28) => "FUN"
  | (T 29) => "ARROW"
  | (T 30) => "UMINUS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, EOF1right)) :: ( _, ( MlyValue.fundeclist 
fundeclist1, fundeclist1left, _)) :: rest671)) => let val  result = 
MlyValue.prog (fn _ => let val  (fundeclist as fundeclist1) = 
fundeclist1 ()
 in (fundeclist)
end)
 in ( LrTable.NT 0, ( result, fundeclist1left, EOF1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.fundeclist (fn _ => let
 val  (fundec as fundec1) = fundec1 ()
 in ([fundec])
end)
 in ( LrTable.NT 2, ( result, fundec1left, fundec1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.fundec fundec1, _, fundec1right)) :: ( _, ( 
MlyValue.fundeclist fundeclist1, fundeclist1left, _)) :: rest671)) =>
 let val  result = MlyValue.fundeclist (fn _ => let val  (fundeclist
 as fundeclist1) = fundeclist1 ()
 val  (fundec as fundec1) = fundec1 ()
 in (fundeclist @ [fundec])
end)
 in ( LrTable.NT 2, ( result, fundeclist1left, fundec1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.exp exp1, _, (expright as exp1right))) :: _
 :: ( _, ( MlyValue.tp tp2, _, _)) :: _ :: _ :: ( _, ( MlyValue.tp tp1
, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (FUNleft as FUN1left), _)) :: 
rest671)) => let val  result = MlyValue.fundec (fn _ => let val  ID1 =
 ID1 ()
 val  ID2 = ID2 ()
 val  tp1 = tp1 ()
 val  tp2 = tp2 ()
 val  (exp as exp1) = exp1 ()
 in (
(FUNleft, expright), (S.symbol ID1, S.symbol ID2, A.Inttp, A.Inttp, exp)
)
end)
 in ( LrTable.NT 1, ( result, FUN1left, exp1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.tp (fn _ => let val  ID1 = ID1 ()
 in (A.Inttp)
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.tps tps1, tps1left, tps1right)) :: rest671))
 => let val  result = MlyValue.tp (fn _ => let val  (tps as tps1) = 
tps1 ()
 in (A.Tupletp(tps))
end)
 in ( LrTable.NT 3, ( result, tps1left, tps1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.tp tp2, _, tp2right)) :: _ :: ( _, ( 
MlyValue.tp tp1, tp1left, _)) :: rest671)) => let val  result = 
MlyValue.tp (fn _ => let val  tp1 = tp1 ()
 val  tp2 = tp2 ()
 in (A.Arrowtp(tp1, tp2))
end)
 in ( LrTable.NT 3, ( result, tp1left, tp2right), rest671)
end
|  ( 7, ( ( _, ( _, _, REF1right)) :: ( _, ( MlyValue.tp tp1, tp1left,
 _)) :: rest671)) => let val  result = MlyValue.tp (fn _ => let val  (
tp as tp1) = tp1 ()
 in (A.Reftp(tp))
end)
 in ( LrTable.NT 3, ( result, tp1left, REF1right), rest671)
end
|  ( 8, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.tp tp1, _, _)
) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.tp (fn _ => let val  (tp as tp1) = tp1 ()
 in (tp)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 5, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), (IDright as 
ID1right))) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (ID as ID1) = ID1 ()
 in (A.Pos((IDleft, IDright), A.Id (S.symbol(ID))))
end)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.INT INT1, (INTleft as INT1left), (INTright
 as INT1right))) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (INT as INT1) = INT1 ()
 in (A.Pos((INTleft, INTright), A.Int(INT)))
end)
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left, exp2right), A.Tuple([exp1, exp2])))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp1, _, (expright as exp1right))) :: (
 _, ( _, (MINUSleft as MINUS1left), _)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (A.Pos((MINUSleft, expright), A.Op(A.Sub, [A.Int(0), exp])))
end)
 in ( LrTable.NT 5, ( result, MINUS1left, exp1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp1, _, (expright as exp1right))) :: (
 _, ( _, (NOTleft as NOT1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (A.Pos((NOTleft, expright), A.Op(A.Sub, [A.Int(0), exp])))
end)
 in ( LrTable.NT 5, ( result, NOT1left, exp1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp1, _, (expright as exp1right))) :: (
 _, ( _, (BANGleft as BANG1left), _)) :: rest671)) => let val  result
 = MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (A.Pos((BANGleft, expright), A.Op(A.Get, [exp])))
end)
 in ( LrTable.NT 5, ( result, BANG1left, exp1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp1, _, (expright as exp1right))) :: (
 _, ( MlyValue.PROJ PROJ1, (PROJleft as PROJ1left), _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (PROJ as PROJ1) =
 PROJ1 ()
 val  (exp as exp1) = exp1 ()
 in (A.Pos((PROJleft, expright), A.Proj(PROJ, exp)))
end)
 in ( LrTable.NT 5, ( result, PROJ1left, exp1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left, exp2right), A.Op(A.Add, [exp1, exp2])))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left, exp2right), A.Op(A.Sub, [exp1, exp2])))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left, exp2right), A.Op(A.Mul, [exp1, exp2])))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left, exp2right), A.If(exp1, exp2, A.Int(0))))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left, exp2right), A.If(exp1, A.Int(1), exp2)))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left, exp2right), A.Op(A.Eq, [exp1, exp2])))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left, exp2right), A.Op(A.LT, [exp1, exp2])))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left, exp2right), A.Op(A.Set, [exp1, exp2])))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exps exps1, exps1left, exps1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (exps
 as exps1) = exps1 ()
 in (A.Tuple(exps))
end)
 in ( LrTable.NT 5, ( result, exps1left, exps1right), rest671)
end
|  ( 26, ( ( _, ( _, _, (RPARENright as RPAREN1right))) :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left,
 _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  
exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left,RPARENright), A.Call(exp1,exp2)))
end)
 in ( LrTable.NT 5, ( result, exp1left, RPAREN1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.tp tp1, _, (tpright as tp1right))) :: _ :: 
( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  tp1 = tp1 ()
 in (A.Pos((exp1left,tpright), A.Constrain(exp1, A.Inttp)))
end)
 in ( LrTable.NT 5, ( result, exp1left, tp1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (A.Pos((IFleft, exp3right), A.If(exp1, exp2, exp3)))
end)
 in ( LrTable.NT 5, ( result, IF1left, exp3right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((WHILEleft, exp2right), A.While(exp1, exp2)))
end)
 in ( LrTable.NT 5, ( result, WHILE1left, exp2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: (
 _, ( _, (LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((LETleft, exp2right), A.Let(S.symbol(ID), exp1, exp2)))
end
)
 in ( LrTable.NT 5, ( result, LET1left, exp2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.exp exp1, _, (expright as exp1right))) :: (
 _, ( _, (REFleft as REF1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (A.Pos((REFleft, expright), A.Op(A.Ref, [exp])))
end)
 in ( LrTable.NT 5, ( result, REF1left, exp1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Fun_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun PROJ (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.PROJ (fn () => i),p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun REF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun BANG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
end
end
