##############################################################################
# Nathan Dunn
# CS 3180
# Project 1
# RUSH lang 
##############################################################################

import sys
sys.path.insert(0,"../..")

# Python Version differences
if sys.version_info[0] >= 3:
    raw_input = input

tokens = ('NUM', 'NAME', 'IF', 'ELSE', 'DO', 'WHILE', 'ASSIGN', 'TRUE', 'FALSE',)
literals = ['+', '*', '(', ')', '-', '/', ';']

t_NAME = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_DO = r'\$DO'
t_TRUE = r'\$TRUE'
t_FALSE = r'\$FALSE'
t_WHILE = r'\$WHILE'
t_IF = r'\$IF'
t_ELSE = r'\$ELSE'
t_ASSIGN = r':='
t_ignore = " \t\r"

def t_NUM(t):
    r'[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?'
    t.value = float(t.value)
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
import ply.lex as lex
lex.lex()

########################################################################################
# BNF FOR RUSH
#
# <program> ::= <block_content>
# 
# <block_content> ::= <statement>
#                 | <block_content> <statement>
#
#<statement> ::= <expr> ";"
#
# <expr> ::- <term> '+' <expr>
#        | <term> '-' <expr>
#        | <term>
#        | NAME ASSIGN <expr>
#
# <term> ::- <factor> '*' <term>
#        |   <factor> '/' <term>
#        |   <factor>
#
# <factor> ::- '(' <expr> ')'
#        | NUM
#        | NAME
#        | TRUE
#        | FALSE
########################################################################################
class Node:
    symbols = { }
    
    def __init__(self):
        self.children = []
        self.text = "invalid"
        self.function = lambda node: node.text

    def interp(self):
        """ Interprets the parse tree rooted at self """
        return self.function(self)
############################ HELPERS ####################################################
# PROGRAM
def program_interp_helper(node):
    result = 0
    for child in node.children :
        result = child.interp()
    return result

# ASSIGNMENT
def set_var_value_helper(node):
    Node.symbols[node.children[0]] = node.children[1].interp()
    return Node.symbols[node.children[0]]

def get_var_value_helper(node):
    try:
        return Node.symbols[node.text]
    except LookupError:
        print("Undefined name '%s'" % node.text)
        return 0

############################ PARSER FUNCTIONS ###########################################
# PROGRAM
def p_program_block_item_list(p):
    '''program : block_content '''
    p[0] = Node()
    p[0].children = p[1]
    p[0].text = 'program'
    p[0].function = program_interp_helper


#BLOCK CONTENT
def p_block_content_statement(p):
    ' block_content : statement '
    p[0] = [p[1]]

def p_block_content_compound(p):
    ' block_content : block_content statement'
    p[0] = p[1]
    p[0].append(p[2])

# STATEMENT
def p_statement_exp(p):
    'statement : expr ";"'
    p[0] = p[1]

# EXPRESSION
def p_expr_term(p):
    'expr : term'
    p[0] = p[1]

def p_expr_add(p):
    'expr : term "+" expr'
    p[0] = Node()
    p[0].text = "+"
    p[0].children = [p[1], p[3]]
    p[0].function = lambda node: node.children[0].interp() + node.children[1].interp()

def p_expr_sub(p):
    'expr : term "-" expr'
    p[0] = Node()
    p[0].text = "+"
    p[0].children = [p[1], p[3]]
    p[0].function = lambda node: node.children[0].interp() - node.children[1].interp()

def p_expr_assign(p):
    'expr : NAME ASSIGN expr'
    p[0] = Node()
    p[0].text = '='
    p[0].children = [p[1],p[3]]
    p[0].function = lambda node: set_var_value_helper(node)

# TERM
def p_term_multiply(p):
    '''term : factor '*' term '''
    p[0] = Node()
    p[0].text = "*"
    p[0].children = [p[1], p[3]]
    p[0].function = lambda node: node.children[0].interp() * node.children[1].interp()

def p_term_divide(p):
    '''term : factor '/' term '''
    p[0] = Node()
    p[0].text = "/"
    p[0].children = [p[1], p[3]]
    p[0].function = lambda node: node.children[0].interp() / node.children[1].interp()

def p_term_factor(p):
    '''term : factor'''
    p[0] = p[1]
# FACTOR
def p_factor_parens(p):
    '''factor : "(" expr ")"'''
    p[0] = p[2]

def p_factor_number(p):
    '''factor : NUM'''
    p[0] = Node()
    p[0].text = str(p[1])
    tmp = p[1]
    p[0].function = lambda node: tmp

def p_factor_name(p):
    '''factor : NAME'''
    p[0] = Node()
    p[0].text = p[1]
    p[0].function = lambda node: get_var_value_helper(node)

def p_factor_true(p):
    ''' factor : TRUE '''
    p[0] = Node()
    p[0].text = p[1]
    p[0].function = lambda node: True

def p_factor_false(p):
    ''' factor : FALSE '''
    p[0] = Node()
    p[0].text = p[1]
    p[0].function = lambda node: False

# ERROR
def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")

##################################### DRIVER #############################################
import ply.yacc as yacc
yacc.yacc()

if 1 < len(sys.argv):
    with open(sys.argv[1], 'r') as myfile:
        data=myfile.read()
    print data
    resultNode = yacc.parse(data+'\n') # parse returns None upon error
    if None != resultNode:
        print resultNode.interp()

else:
    while 1:
        try:
            s = raw_input('RUSH > ')
        except EOFError:
            break
        if not s: continue
        resultNode = yacc.parse(s+'\n') # parse returns None upon error
        if None != resultNode:
            print resultNode.interp()
