functor FunLexFun(structure Tokens: Fun_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
COMMENT | INITIAL
    structure UserDeclarations = 
      struct

type pos = ErrorMsg.pos
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult  = (svalue,ErrorMsg.pos) token

val newLine = ErrorMsg.newLine

fun make_pos (yypos,yytext) : ErrorMsg.pos2
    = (yypos, yypos + String.size(yytext) - 1)

(* Function for PROJ
   Get the project number and make the proper type *)
fun proj_cal (text, pos2) : int * ErrorMsg.pos*ErrorMsg.pos
    = let val v = valOf (Int.fromString (substring(text,1,size(text)-1)))
          val (p1,p2) = pos2
      in (v,p1,p2) end

(* Function for ID to change from pos2 to 2 poses and make the proper type *)
fun id_cal (text, pos2) : string * ErrorMsg.pos*ErrorMsg.pos
    = let val (p1,p2) = pos2
      in (text,p1,p2) end

(* Function for NUM to change from pos2 to 2 poses and make the proper type *)
fun num_cal (text, pos2) : int * ErrorMsg.pos*ErrorMsg.pos
    = let val v = valOf (Int.fromString (text))
          val (p1,p2) = pos2
      in (v,p1,p2) end

(* ref int for indicating depth of nested comments *)
val comment_depth = ref 0;
(* position at which comments begin *)
val comment_start = ref 0;

(* Handling EOF.  Note that this function reports the wrong file-position for
   end-of-file.  Because of a design infelicity of ML-Lex, it's possible but
   not easy to get access to the correct file position.  There is a way to 
   do it using the %arg feature of ML-Lex, but you don't need to bother 
   with it for this exercise. 
*)
(* If comments are unclosed, it shows error messages indicating the start point of the outermost comments. *)
fun eof () = 
     (if !comment_depth>0 then (let val p1=(!comment_start,!comment_start):ErrorMsg.pos2 in (comment_depth:=0; ErrorMsg.error (p1, "Error: Unclosed nested comment \n")) end )else comment_depth:=0; Tokens.EOF(0,0))

fun error (pos2,text) =
     ErrorMsg.error (pos2,("Error: Undefined token : " ^ text))



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ARROW(make_pos(yypos,yytext)))
      end
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.FUN(make_pos(yypos,yytext)))
      end
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.IN(make_pos(yypos,yytext)))
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LET(make_pos(yypos,yytext)))
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ELSE(make_pos(yypos,yytext)))
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.THEN(make_pos(yypos,yytext)))
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.IF(make_pos(yypos,yytext)))
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ASSIGN(make_pos(yypos,yytext)))
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.BANG(make_pos(yypos,yytext)))
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.REF(make_pos(yypos,yytext)))
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.DO(make_pos(yypos,yytext)))
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.WHILE(make_pos(yypos,yytext)))
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.OR(make_pos(yypos,yytext)))
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.NOT(make_pos(yypos,yytext)))
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.AND(make_pos(yypos,yytext)))
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.GT(make_pos(yypos,yytext)))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.EQ(make_pos(yypos,yytext)))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LT(make_pos(yypos,yytext)))
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.PROJ(proj_cal(yytext,make_pos(yypos,yytext))))
      end
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.TIMES(make_pos(yypos,yytext)))
      end
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.MINUS(make_pos(yypos,yytext)))
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.PLUS(make_pos(yypos,yytext)))
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.RPAREN(make_pos(yypos,yytext)))
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LPAREN(make_pos(yypos,yytext)))
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.COLON(make_pos(yypos,yytext)))
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.SEMICOLON(make_pos(yypos,yytext)))
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.COMMA(make_pos(yypos,yytext)))
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ID(id_cal(yytext,make_pos(yypos,yytext))))
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.INT(num_cal(yytext,make_pos(yypos,yytext))))
      end
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (newLine yypos; continue ()))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT; comment_depth := !comment_depth + 1; comment_start:=yypos; continue()))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (comment_depth := !comment_depth + 1; continue()))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (newLine yypos; continue()))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (comment_depth := !comment_depth-1; if !comment_depth = 0 then YYBEGIN INITIAL else YYBEGIN COMMENT;  continue()))
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (error(make_pos(yypos,yytext),yytext); continue())
      end
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ38(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction27(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp = #"`"
              then yyAction27(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction11(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction11(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = #"`"
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ43(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ42(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ41(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"h"
              then yyQ40(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"h"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction5(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction5(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp = #"`"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ46(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ45(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"h"
              then yyQ44(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"h"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction9(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction9(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp = #"`"
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ47(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction13(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction13(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction13(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction13(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp = #"`"
              then yyAction13(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ50(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ49(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction3(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"`"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ52(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ51(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"`"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction27(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction27(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                      else yyAction27(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ54(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"f"
                  then yyQ53(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction1(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction1(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"`"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ56(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ55(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"`"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ59(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ58(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ57(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction10(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction10(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = #"`"
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ60(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction27(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp = #"`"
              then yyAction27(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ61(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ62(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"0"
              then yyAction28(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ62(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ62(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"0"
              then yyAction28(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ62(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ63(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ64(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ66(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"0"
              then yyAction18(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ66(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ66(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ65(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ66(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ25(strm', lastMatch)
            else if inp < #"="
              then if inp = #"'"
                  then yyQ8(strm', lastMatch)
                else if inp < #"'"
                  then if inp = #" "
                      then yyQ9(strm', lastMatch)
                    else if inp < #" "
                      then if inp = #"\v"
                          then yyQ8(strm', lastMatch)
                        else if inp < #"\v"
                          then if inp = #"\t"
                              then yyQ9(strm', lastMatch)
                            else if inp = #"\n"
                              then yyQ10(strm', lastMatch)
                              else yyQ8(strm', lastMatch)
                        else if inp = #"\r"
                          then yyQ9(strm', lastMatch)
                          else yyQ8(strm', lastMatch)
                    else if inp = #"#"
                      then yyQ12(strm', lastMatch)
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ11(strm', lastMatch)
                          else yyQ8(strm', lastMatch)
                    else if inp = #"&"
                      then yyQ13(strm', lastMatch)
                      else yyQ8(strm', lastMatch)
                else if inp = #"."
                  then yyQ8(strm', lastMatch)
                else if inp < #"."
                  then if inp = #"+"
                      then yyQ17(strm', lastMatch)
                    else if inp < #"+"
                      then if inp = #")"
                          then yyQ15(strm', lastMatch)
                        else if inp = #"("
                          then yyQ14(strm', lastMatch)
                          else yyQ16(strm', lastMatch)
                    else if inp = #","
                      then yyQ18(strm', lastMatch)
                      else yyQ19(strm', lastMatch)
                else if inp = #":"
                  then yyQ22(strm', lastMatch)
                else if inp < #":"
                  then if inp = #"/"
                      then yyQ20(strm', lastMatch)
                      else yyQ21(strm', lastMatch)
                else if inp = #";"
                  then yyQ23(strm', lastMatch)
                  else yyQ24(strm', lastMatch)
            else if inp = #"m"
              then yyQ27(strm', lastMatch)
            else if inp < #"m"
              then if inp = #"e"
                  then yyQ29(strm', lastMatch)
                else if inp < #"e"
                  then if inp = #"["
                      then yyQ8(strm', lastMatch)
                    else if inp < #"["
                      then if inp = #"?"
                          then yyQ8(strm', lastMatch)
                        else if inp < #"?"
                          then yyQ26(strm', lastMatch)
                        else if inp <= #"@"
                          then yyQ8(strm', lastMatch)
                          else yyQ27(strm', lastMatch)
                    else if inp = #"a"
                      then yyQ27(strm', lastMatch)
                    else if inp < #"a"
                      then yyQ8(strm', lastMatch)
                    else if inp = #"d"
                      then yyQ28(strm', lastMatch)
                      else yyQ27(strm', lastMatch)
                else if inp = #"i"
                  then yyQ31(strm', lastMatch)
                else if inp < #"i"
                  then if inp = #"f"
                      then yyQ30(strm', lastMatch)
                      else yyQ27(strm', lastMatch)
                else if inp = #"l"
                  then yyQ32(strm', lastMatch)
                  else yyQ27(strm', lastMatch)
            else if inp = #"u"
              then yyQ27(strm', lastMatch)
            else if inp < #"u"
              then if inp = #"r"
                  then yyQ34(strm', lastMatch)
                else if inp < #"r"
                  then if inp = #"n"
                      then yyQ33(strm', lastMatch)
                      else yyQ27(strm', lastMatch)
                else if inp = #"s"
                  then yyQ27(strm', lastMatch)
                  else yyQ35(strm', lastMatch)
            else if inp = #"{"
              then yyQ8(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"w"
                  then yyQ36(strm', lastMatch)
                  else yyQ27(strm', lastMatch)
            else if inp = #"|"
              then yyQ37(strm', lastMatch)
              else yyQ8(strm', lastMatch)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ6(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ7(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ4(strm', lastMatch)
            else if inp < #"*"
              then if inp = #"\n"
                  then yyQ3(strm', lastMatch)
                  else yyQ2(strm', lastMatch)
            else if inp = #"/"
              then yyQ5(strm', lastMatch)
              else yyQ2(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COMMENT => yyQ0(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ1(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
