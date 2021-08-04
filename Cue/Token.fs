/// See cuelang.org/go/cue/token
module rec CueFSharp.Cue.Token

type Token =
    | ILLEGAL
    | EOF
    | COMMENT
    | ATTRIBUTE

    | LiteralBeg

    | IDENT
    | INT
    | FLOAT

    | STRING
    | INTERPOLATION
    | BOTTOM

    | LiteralEnd

    | OperatorBeg

    | ADD
    | SUB
    | MUL
    | POW
    | QUO

    | IQUO
    | IREM
    | IDIV
    | IMOD

    | AND
    | OR

    | LAND
    | LOR

    | BIND
    | EQL
    | LSS
    | GTR
    | NOT
    | ARROW

    | NEQ
    | LEQ
    | GEQ

    | MAT
    | NMAT

    | LPAREN
    | LBRACK
    | LBRACE
    | COMMA
    | PERIOD
    | ELLIPSIS

    | RPAREN
    | RBRACK
    | RBRACE
    | SEMICOLON
    | COLON
    | ISA
    | OPTION
    | OperatorEnd

    | KeywordBeg

    | IF
    | FOR
    | IN
    | LET

    | TRUE
    | FALSE
    | NULL

    | KeywordEnd

    override this.ToString() = 
        (tokens: Map<Token, string>).[this]


let tokens =
    Map<Token, string>([ 
    (ILLEGAL, "ILLEGAL")

    (EOF,     "EOF")
    (COMMENT, "COMMENT")

    (IDENT,         "IDENT")
    (INT,           "INT")
    (FLOAT,         "FLOAT")
    (STRING,        "STRING")
    (INTERPOLATION, "INTERPOLATION")
    (ATTRIBUTE,     "ATTRIBUTE")

    (ADD, "+")
    (SUB, "-")
    (MUL, "*")
    (POW, "^")
    (QUO, "/")

    (IQUO, "quo")
    (IREM, "rem")
    (IDIV, "div")
    (IMOD, "mod")

    (AND, "&")
    (OR,  "|")

    (LAND, "&&")
    (LOR,  "||")

    (BIND,  "=")
    (EQL,   "==")
    (LSS,   "<")
    (GTR,   ">")
    (NOT,   "!")
    (ARROW, "<-")

    (NEQ, "!=")
    (LEQ, "<=")
    (GEQ, ">=")

    (MAT,  "=~")
    (NMAT, "!~")

    (LPAREN,   "(")
    (LBRACK,   "[")
    (LBRACE,   "{")
    (COMMA,    ")")
    (PERIOD,   ".")
    (ELLIPSIS, "...")

    (RPAREN,    ")")
    (RBRACK,    "]")
    (RBRACE,    "}")
    (SEMICOLON, ";")
    (COLON,     ",")
    (ISA,       ",,")
    (OPTION,    "?")

    (BOTTOM, "_|_")

    (FALSE, "false")
    (TRUE,  "true")
    (NULL,  "null")

    (FOR, "for")
    (IF,  "if")
    (IN,  "in")
    (LET, "let") ])

