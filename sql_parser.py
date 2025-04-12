import ply.lex as lex
import ply.yacc as yacc

# -------------------- Lexer --------------------

# List of token names
tokens = (
    'SELECT',
    'FROM',
    'WHERE',
    'AND',
    'OR',
    'IDENTIFIER',
    'COMMA',
    'OPERATOR',
    'NUMBER',
    'STRING',
    'LPAREN',
    'RPAREN',
    'SEMICOLON',
    'STAR'
)

# Reserved keywords
reserved = {
    'select': 'SELECT',
    'from': 'FROM',
    'where': 'WHERE',
    'and': 'AND',
    'or': 'OR'
}

# Regular expression rules for simple tokens
t_COMMA = r','
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_SEMICOLON = r';'
t_STAR = r'\*'

# A regular expression rule for identifiers (including column and table names)
def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value.lower(), 'IDENTIFIER')  # Check for reserved words
    return t

# Regular expression for operators
def t_OPERATOR(t):
    r'[=<>!]=|[<>]'
    return t

# Regular expression for numbers
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Regular expression for strings
def t_STRING(t):
    r"'[^']*'|\"[^\"]*\""
    t.value = t.value[1:-1]  # Remove quotes
    return t

# Define a rule to track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'

# Error handling rule
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# -------------------- Parser --------------------

# Grammar rules

def p_query(p):
    '''query : select_statement'''
    p[0] = p[1]
    print("Valid SQL query")

def p_select_statement(p):
    '''select_statement : SELECT select_list FROM table_references where_clause SEMICOLON'''
    p[0] = ('SELECT', p[2], p[4], p[5])

def p_select_list_star(p):
    '''select_list : STAR'''
    p[0] = ['*']

def p_select_list_single(p):
    '''select_list : IDENTIFIER'''
    p[0] = [p[1]]

def p_select_list_multiple(p):
    '''select_list : select_list COMMA IDENTIFIER'''
    p[0] = p[1] + [p[3]]

def p_table_references_single(p):
    '''table_references : IDENTIFIER'''
    p[0] = [p[1]]

def p_table_references_multiple(p):
    '''table_references : table_references COMMA IDENTIFIER'''
    p[0] = p[1] + [p[3]]

def p_where_clause(p):
    '''where_clause : WHERE condition
                    | empty'''
    if len(p) == 3:
        p[0] = p[2]
    else:
        p[0] = None

def p_condition_comparison(p):
    '''condition : IDENTIFIER OPERATOR value'''
    p[0] = ('CONDITION', p[1], p[2], p[3])

def p_condition_grouped(p):
    '''condition : LPAREN condition RPAREN'''
    p[0] = p[2]

def p_condition_and(p):
    '''condition : condition AND condition'''
    p[0] = ('AND', p[1], p[3])

def p_condition_or(p):
    '''condition : condition OR condition'''
    p[0] = ('OR', p[1], p[3])

def p_value(p):
    '''value : NUMBER
             | STRING
             | IDENTIFIER'''
    p[0] = p[1]

def p_empty(p):
    '''empty :'''
    pass

def p_error(p):
    if p:
        print(f"Syntax error at token '{p.value}' at line {p.lineno}")
    else:
        print("Syntax error at EOF")

# Build the parser
parser = yacc.yacc()

# -------------------- Main Loop --------------------

if __name__ == '__main__':
    while True:
        try:
            s = input('SQL > ')
        except EOFError:
            break
        if not s:
            continue

        # Tokenize (optional for debug)
        lexer.input(s)
        print("\nTokens:")
        while True:
            tok = lexer.token()
            if not tok:
                break
            print(tok)

        # Parse input
        print("\nParsing:")
        result = parser.parse(s)
        print("Parse Tree:", result)
