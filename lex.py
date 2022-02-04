#!/usr/bin/python3

import ply.lex as lex
import sys

tokens = (
    'PLUS', 'MINUS', 'TIMES', 'DIV', 'MOD',   
    'EQ', 'NEQ', 'LE', 'GE', 'LEQ', 'GEQ',        
    'LPAR', 'RPAR', 'COLON', 'SCOLON', 'COMMA',    
    'ASSIGN', 'NUM', 'PID',                       
    'VAR', 'BEGIN', 'END',                     
    'IF', 'THEN', 'ELSE', 'ENDIF',                 
    'WHILE', 'DO', 'ENDWHILE',                      
    'REPEAT', 'UNTIL',                             
    'FOR', 'FROM', 'TO', 'DOWNTO', 'ENDFOR',       
    'READ', 'WRITE'                                 
)

t_PLUS      = r'PLUS'
t_MINUS     = r'MINUS'
t_TIMES     = r'TIMES'
t_DIV       = r'DIV'
t_MOD       = r'MOD'
t_EQ        = r'EQ'
t_NEQ       = r'NEQ'
t_LE        = r'LE'
t_GE        = r'GE'
t_LEQ       = r'LEQ'
t_GEQ       = r'GEQ'
t_LPAR      = r'\['
t_RPAR      = r'\]'
t_COLON     = r':'
t_SCOLON    = r';'
t_COMMA     = r','
t_ASSIGN    = r'ASSIGN'
t_VAR       = r'VAR'
t_BEGIN     = r'BEGIN'
t_END       = r'END'
t_IF        = r'IF'
t_THEN      = r'THEN'
t_ELSE      = r'ELSE'
t_ENDIF     = r'ENDIF'
t_WHILE     = r'WHILE'
t_DO        = r'DO'
t_ENDWHILE  = r'ENDWHILE'
t_REPEAT    = r'REPEAT'
t_UNTIL     = r'UNTIL'
t_FOR       = r'FOR'
t_FROM      = r'FROM'
t_TO        = r'TO'
t_DOWNTO    = r'DOWNTO'
t_ENDFOR    = r'ENDFOR'
t_READ      = r'READ'
t_WRITE     = r'WRITE'
t_ignore = ' \t'

def t_NUM(t):
    r'-?\d+'
    t.value = int(t.value)
    return t

def t_PID(t):
    r'[_a-z]+'
    t.value = str(t.value)
    return t

def t_COM(t):
    r'\([^\)]*\)'
    pass

def t_NL(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print('Nielegalny znak {0:} w linii {1: d}'.format(t.value[0], t.lexer.lineno))
    t.lexer.skip(1)

def build_lex():
    return lex.lex()

if __name__ == '__main__':
    data = ''
    with open(sys.argv[1], 'r') as f:
        data = f.read()
    lexer = build_lex()
    lexer.input(data)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)