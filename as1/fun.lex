type pos = ErrorMsg.pos
type svalue = Tokens.svalue
type ('svalue,'pos) token = ('svalue,'pos) Tokens.token
type lexresult  = (svalue,ErrorMsg.pos) token

val newLine = ErrorMsg.newLine
val comment_level = ref 0

fun make_pos (yypos,yytext) : ErrorMsg.pos2
    = (yypos, yypos + String.size(yytext) - 1)

(* Handling EOF.  Note that this function reports the wrong file-position for
   end-of-file.  Because of a design infelicity of ML-Lex, it's possible but
   not easy to get access to the correct file position.  There is a way to 
   do it using the %arg feature of ML-Lex, but you don't need to bother 
   with it for this exercise. 
*)
fun eof () = 
     Tokens.EOF(0,0)

fun make_string_args (yypos, yytext) : string * ErrorMsg.pos * ErrorMsg.pos =
let
     val (pos1, pos2) = make_pos(yypos, yytext)
     val value = yytext
in
       (value, pos1, pos2)
end

fun make_int_args (yypos, yytext) : int * ErrorMsg.pos * ErrorMsg.pos =
let
     val (pos1, pos2) = make_pos(yypos, yytext)
     val value = valOf(Int.fromString(yytext))
in
       (value, pos1, pos2)
end

fun for_PROJ (yypos, yytext) : int * ErrorMsg.pos * ErrorMsg.pos =
let
     val (pos1, pos2) = make_pos(yypos, yytext)
     val value = valOf(Int.fromString(substring(yytext, 1, (size yytext) - 1)))
in
       (value, pos1, pos2)
end
     

%%

%s COMMENT;
%header (functor FunLexFun(structure Tokens: Fun_TOKENS));

alpha = [A-Za-z];
digit = [0-9];
ws    = [\ \t\r];

%%
<INITIAL>fun   => (Tokens.FUN(make_pos(yypos,yytext)));
<INITIAL>in    => (Tokens.IN(make_pos(yypos,yytext)));
<INITIAL>let   => (Tokens.LET(make_pos(yypos,yytext)));
<INITIAL>while => (Tokens.WHILE(make_pos(yypos,yytext)));
<INITIAL>do    => (Tokens.DO(make_pos(yypos,yytext)));
<INITIAL>if    => (Tokens.IF(make_pos(yypos,yytext)));
<INITIAL>then  => (Tokens.THEN(make_pos(yypos,yytext)));
<INITIAL>else  => (Tokens.ELSE(make_pos(yypos,yytext)));
<INITIAL>ref   => (Tokens.REF(make_pos(yypos,yytext)));
<INITIAL>not   => (Tokens.NOT(make_pos(yypos,yytext)));
<INITIAL>type  => (Tokens.TYPE(make_pos(yypos, yytext)));

<INITIAL>\n    => (newLine yypos; continue ());
<INITIAL>{ws}+ => (continue());

<INITIAL>"/*"  => (YYBEGIN COMMENT; comment_level := 1; continue());
<COMMENT>"/*"  => (comment_level := !comment_level + 1; continue());
<COMMENT>"*/"  => (comment_level := !comment_level - 1;
                  if !comment_level = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT>\n    => (newLine yypos; continue());
<COMMENT>.     => (continue());

<INITIAL>{alpha}[A-Za-z0-9_]* => (Tokens.ID(make_string_args(yypos, yytext)));
<INITIAL>{digit}+ => (Tokens.INT(make_int_args(yypos, yytext)));

<INITIAL>"->"  => (Tokens.ARROW(make_pos(yypos,yytext)));
<INITIAL>"!"   => (Tokens.BANG(make_pos(yypos,yytext)));
<INITIAL>":="  => (Tokens.ASSIGN(make_pos(yypos,yytext)));
<INITIAL>"("   => (Tokens.LPAREN(make_pos(yypos,yytext)));
<INITIAL>")"   => (Tokens.RPAREN(make_pos(yypos,yytext)));
<INITIAL>"||"  => (Tokens.OR(make_pos(yypos,yytext)));
<INITIAL>"&"   => (Tokens.AND(make_pos(yypos,yytext)));
<INITIAL>"="   => (Tokens.EQ(make_pos(yypos,yytext)));
<INITIAL>">"   => (Tokens.GT(make_pos(yypos,yytext)));
<INITIAL>"<"   => (Tokens.LT(make_pos(yypos,yytext)));
<INITIAL>"*"   => (Tokens.TIMES(make_pos(yypos,yytext)));
<INITIAL>"-"   => (Tokens.MINUS(make_pos(yypos,yytext)));
<INITIAL>"+"   => (Tokens.PLUS(make_pos(yypos,yytext)));
<INITIAL>";"   => (Tokens.SEMICOLON(make_pos(yypos,yytext)));
<INITIAL>","   => (Tokens.COMMA(make_pos(yypos,yytext)));
<INITIAL>":"   => (Tokens.COLON(make_pos(yypos,yytext)));
<INITIAL>"#"[0-9]{digit}*  => (Tokens.PROJ(for_PROJ(yypos, yytext)));


