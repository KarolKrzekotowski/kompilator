
import ply.yacc as yacc
import sys
from lex import build_lex, tokens

symbol_list = [] # name, tab, start, end, memory index, initialized 
memory_index = 0 


# syntax error

def p_error(p):
    raise SyntaxError('Błąd składniowy w linii {0:d}'.format(p.lexer.lineno))

# dodawanie symbolu do listy
def add_symbol(name, tab, start, stop, lineno):
    global symbol_list, memory_index
    n = None
    
    for k in range(len(symbol_list)):
        if symbol_list[k][0] == name:
            n = k
            break
    if n is not None:
        raise Exception('Błąd w linii {0:d}: Druga deklaracja zmiennej {1:}'.format(lineno, name))
    if tab:
        if start > stop:
            raise Exception('Błąd w linii {0:d}: Niewłaściwy zakres w deklaracji tablicy'.format(lineno))
    symbol_list.append([name, tab, start, stop, memory_index, False])
    memory_index += stop - start + 1

# szukanie adresu w liście
def symbol_by_memory_id(mem_id):
    global symbol_list
    for i in range(len(symbol_list)):
        if symbol_list[i][4] == mem_id: 
            return i
        elif symbol_list[i][1]:
            if mem_id > symbol_list[i][4] and mem_id <= symbol_list[i][4] + symbol_list[i][3] - symbol_list[i][2]:     
                return i

# Program 
def p_program(p):
    '''program : VAR declarations BEGIN commands END
               | BEGIN commands END'''
    if len(p) == 6:
        p[0] = p[4] + 'HALT'
    else:
        p[0] = p[2] + 'HALT'

# declarations
def p_var_variable(p):
    '''declarations : declarations COMMA PID
                    | PID'''
    if p[1] != None:
        name = p[1]
        ln = p.lineno(1)
    else:
        name = p[3]
        ln = p.lineno(3)
    add_symbol(name, False, 0, 0, ln)

def p_var_array(p):
    '''declarations : declarations COMMA PID LPAR NUM COLON NUM RPAR
                    | PID LPAR NUM COLON NUM RPAR'''
   
    if p[1] == None:
        name = p[3]
        start = p[5]
        stop = p[7]
        ln = p.lineno(3)
    else:
        name = p[1]
        start = p[3]
        stop = p[5]
        ln = p.lineno(1)
    add_symbol(name, True, start, stop, ln)
    
# commands

def p_commands(p):
    '''commands : commands command
                | command'''
    if len(p) == 3:
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]

# ładowanie wartości

def load_value(val):
    result = ''
    counter = 0 
    g_inc = ''
    while val != 0:
        if val > 0:
            if val > 15:
                if val % 2 == 0:
                    g_inc =''
                    while val % 2 == 0:
                        val //= 2
                        counter += 1
                    while counter != 0:
                        counter -= 1
                        g_inc +='INC g\n'
                    result = '{}SHIFT g\nRESET g\n'.format(g_inc)+ result
                    
                else:
                    result = 'INC a\n' + result
                    val -= 1
            else:
                while val != 0:
                    result = 'INC a\n' + result
                    val -= 1
        else:
            if val < -15:
                if val % 2 == 0:
                    g_inc = ''
                    while val % 2 == 0:
                        val //= 2
                        counter += 1
                    while counter != 0:
                        counter -= 1
                        g_inc += 'INC g\n'
                    result = '{}SHIFT g\nRESET g\n'.format(g_inc)+ result
                else:
                    result = 'DEC a\n' + result
                    val += 1
            else:
                while val != 0:
                    result = 'DEC a\n' + result
                    val += 1
    result = 'RESET a\nRESET g\n' + result 
    return result

def find_memory(val):
    result =''
    g_inc = ''
    counter = 0
    while val != 0:
        if val > 15:
            if val % 2 == 0:
                g_inc = ''
                while val % 2 ==0:
                    val //=2
                    counter +=1
                while counter != 0:
                    counter -= 1
                    g_inc += 'INC g\n' 
                result = '{}SHIFT g\nRESET g\n'.format(g_inc) + result
            else:
                result = 'INC a\n' + result
                val -= 1
        else:
            result = 'INC a\n' + result
            val -= 1
        
    result = 'RESET a\nRESET g\n' + result + 'SWAP b\n'
    return result

def load_variable(var,ln):
    result = ''
    i = find_memory(var)
    a = symbol_by_memory_id(var)
    result += i
    if not symbol_list[a][5]:
        raise Exception('Błąd w linii {0:}: Użycie niezainicjowanej zmiennej {1:}'.format(ln, symbol_list[a][0]))   
    result += 'LOAD b\n'
    return result

def load_array(cell_id,tab_addr,min_tab_id,ln):
    result = ''
    result += load_variable(cell_id,ln) + 'SWAP d\n' + load_value(tab_addr) + 'ADD d\n'+\
        'SWAP d\n' + load_value(min_tab_id) + 'SWAP d\n' + 'SUB d\n' + 'LOAD a\n' 
    return result

# assigment

def p_assingment(p):
    'command : identifier ASSIGN expression SCOLON'
    global symbol_list
    result = p[3]
    if type(p[1]) is tuple: #tablica indeksowana zmienna
        result += load_variable(p[1][0], p.lineno(2)) +'SWAP d\n' + load_value(p[1][1]) + \
                'ADD d\nSWAP d\n' + load_value(p[1][2])+ 'SWAP d\n' + 'SUB d\n'
        i =  symbol_by_memory_id(p[1][1])
        
    else: #zmienna lub tablica indeksowana liczba
        i = symbol_by_memory_id(p[1])
        result += load_value(p[1])
    result += "SWAP b\n"
    symbol_list[i][5] = True
    if symbol_list[i][0].startswith('LOOP'):
        raise Exception('Błąd w linii {0:d}: Modyfikacja wartości iteratora pętli'.format(p.lineno(2)))
    result += 'SWAP c\nSTORE b\nRESET b\n'
    p[0]=result
    

# if 

def p_if_statment(p):
    'command : IF conditionition THEN commands ENDIF'
    
    result = p[2]
    commands_len = len(p[4].split('\n'))
    result += 'JZERO 2\n' + 'JUMP {}\n'.format(commands_len) + p[4]
    p[0] = result

def p_if_else_statement(p):
    'command : IF conditionition THEN commands ELSE commands ENDIF'  

    result = p[2]
    com1_len = len(p[4].split('\n')) + 1
    com2_len = len(p[6].split('\n'))
    result += 'JZERO 2\n' + 'JUMP {}\n'.format(com1_len) + p[4] + 'JUMP {}\n'.format(com2_len) + p[6]
    p[0] = result

# loops 

def p_while_loop(p):
    'command : WHILE conditionition DO commands ENDWHILE'
   
    result = p[2]
    condition_len = len(p[2].split('\n'))
    commands_len = len(p[4].split('\n'))
    result += 'JZERO 2\n' + 'JUMP {}\n'.format(commands_len + 1) + p[4] + 'JUMP -{}\n'.format(commands_len + condition_len)
    p[0] = result
    

def p_repeat_until_loop(p):
    'command : REPEAT commands UNTIL conditionition SCOLON'
    
    condition_len = len(p[2].split('\n'))
    commands_len = len(p[4].split('\n'))
    p[0] = p[2] + p[4] + 'JZERO 2\n' + 'JUMP -{}\n'.format(commands_len + condition_len-1)

def p_for_iter(p):
    'iter : PID'
    global symbol_list
    add_symbol('LOOP'+p[1], False, 0, 0, p.lineno(1)) # dodawanie tymczasowej zmiennej do listy
    add_symbol('ENDLOOP'+p[1], False, 0, 0, p.lineno(1)) # dodawanie tymczasowej zmiennej końca petli do listy
    i = None
    name = 'LOOP'+p[1]
    for k in range(len(symbol_list)):
        if symbol_list[k][0] == name:
            i = k
            break
    if i == None:
        raise Exception('Błąd w linii {0:d}: użycie niezadeklarowanej zmiennej {1:}'.format(p.lineno(1), name))
    symbol_list[i][5] = True
    p[0] = p[1]

def p_for_to_loop(p):
    'command : FOR iter FROM value TO value DO commands ENDFOR'
    
    global symbol_list, memory_index
    
    iter_name = 'LOOP'+p[2]
    iter_address= None
    end_address = None
    end_name = 'ENDLOOP' + p[2]
    for k in range(len(symbol_list)):
        if symbol_list[k][0] == iter_name:
            iter_address = symbol_list[k][4]
    for k in range(len(symbol_list)):
        if symbol_list[k][0] == end_name:
            end_address = symbol_list[k][4]
    if iter_address == None:
        raise Exception('Błąd w linii {0:d}: użycie niezadeklarowanej zmiennej {1:}'.format(p.lineno(1), iter_name))
    if end_address == None:
        raise Exception('Błąd w linii {0:d}: użycie niezadeklarowanej zmiennej {1:}'.format(p.lineno(1), end_name))
    
    result = '' 
    
    if  p[4][1]:
        if type(p[4][0]) is tuple:
            result += load_array(*p[4][0],p.lineno(2))
        else:
            result += load_variable(p[4][0],p.lineno(2))
        result += 'SWAP c\n'
        result += load_value(iter_address)
        result += 'SWAP c\n'
        result += 'STORE c\n'
        if p[6][1]:
            if type(p[6][0]) is tuple:
                result += load_array(*p[6][0],p.lineno(2))
            else:
                result += load_variable(p[6][0],p.lineno(2))
        else:
            result += load_value(p[6][0])
        result += 'INC c\nSTORE c\n'
    else:
        result += load_value(p[4][0])
        result += 'SWAP c\n'
        result += load_value(iter_address)
        result += 'SWAP c\n'
        result += 'STORE c\n'
        if p[6][1]:
            if type(p[6][0]) is tuple:
                result += load_array(*p[6][0],p.lineno(2))
            else:
                result += load_variable(p[6][0],p.lineno(2))
        else:
            result += load_value(p[6][0])
        result += 'INC c\nSTORE c\n'
    i =symbol_by_memory_id(end_address)
    symbol_list[i][5] = True
    load_end =  load_variable(end_address,p.lineno(1)) + 'SWAP f\n'
    load_end_len = len(load_end.split('\n'))
    load_iter = load_variable(iter_address,p.lineno(1)) 
    load_iter_len = len(load_iter.split('\n'))
    load_iter_address= load_value(iter_address) + 'SWAP h\n'
    load_iter_address_len = len(load_iter_address.split('\n'))
    commands_len = len(p[8].split('\n'))
    result += load_end + load_iter + 'SUB f\n'  + 'JPOS {}\n'.format(commands_len + load_end_len + load_iter_len + load_iter_address_len + 4) + p[8] + load_iter_address+ load_iter + 'INC a\n' + 'STORE h\n' + 'SWAP d\n' + load_end+ 'SWAP f\n'+\
                      'SUB d\nJPOS -{}\n'.format(commands_len + load_end_len + load_iter_len + load_iter_address_len + 1)+ 'JZERO -{}\n'.format(commands_len + load_end_len + load_iter_len + load_iter_address_len + 2)
    p[0] = result
    memory_index -= 2
    symbol_list.pop(symbol_by_memory_id(iter_address))
    symbol_list.pop(symbol_by_memory_id(end_address))        


def p_for_down_loop(p):
    'command : FOR iter FROM value DOWNTO value DO commands ENDFOR'

    global symbol_list, memory_index
    iter_name = 'LOOP'+p[2]
    iter_address= None
    end_address = None
    end_name = 'ENDLOOP' + p[2]
    for k in range(len(symbol_list)):
        if symbol_list[k][0] == iter_name:
            iter_address = symbol_list[k][4]
    for k in range(len(symbol_list)):
        if symbol_list[k][0] == end_name:
            end_address = symbol_list[k][4]
    if iter_address == None:
        raise Exception('Błąd w linii {0:d}: użycie niezadeklarowanej zmiennej {1:}'.format(p.lineno(1), iter_name))
    if end_address == None:
        raise Exception('Błąd w linii {0:d}: użycie niezadeklarowanej zmiennej {1:}'.format(p.lineno(1), end_name))
    result = '' 
    i =symbol_by_memory_id(iter_address)
    symbol_list[i][5] = True
    if  p[4][1]:
        if type(p[4][0]) is tuple:
            result += load_array(*p[4][0],p.lineno(2))
        else:
            result += load_variable(p[4][0],p.lineno(2))
        result += 'SWAP c\n'
        result += load_value(iter_address)
        result += 'SWAP c\n'
        result += 'STORE c\n'
        if p[6][1]:
            if type(p[6][0]) is tuple:
                result += load_array(*p[6][0],p.lineno(2))
            else:
                result += load_variable(p[6][0],p.lineno(2))
        else:
            result += load_value(p[6][0])
        result += 'INC c\nSTORE c\n'
    else:
        result += load_value(p[4][0])
        result += 'SWAP c\n'
        result += load_value(iter_address)
        result += 'SWAP c\n'
        result += 'STORE c\n'
        if p[6][1]:   
            if type(p[6][0]) is tuple:
                result += load_array(*p[6][0],p.lineno(2))
            else:
                result += load_variable(p[6][0],p.lineno(2))
        else:
            result += load_value(p[6][0])
        result += 'INC c\nSTORE c\n'
    symbol_list[i+1][5] = True
    load_end =  load_variable(end_address,p.lineno(1)) + 'SWAP f\n'
    load_end_len = len(load_end.split('\n'))
    load_iter = load_variable(iter_address,p.lineno(1)) 
    load_iter_len = len(load_iter.split('\n'))
    load_iter_address= load_value(iter_address) + 'SWAP h\n'
    load_iter_address_len = len(load_iter_address.split('\n'))
    commands_len = len(p[8].split('\n'))
    result += load_end + load_iter + 'SUB f\n'  + 'JNEG {}\n'.format(commands_len + load_end_len + load_iter_len + load_iter_address_len + 4) + p[8] + load_iter_address+ load_iter + 'DEC a\n' + 'STORE h\n' + 'SWAP d\n' + load_end+ 'SWAP f\n'+\
                      'SUB d\nJNEG -{}\n'.format(commands_len + load_end_len + load_iter_len + load_iter_address_len + 1)+ 'JZERO -{}\n'.format(commands_len + load_end_len + load_iter_len + load_iter_address_len + 2)
    p[0] = result
    memory_index -= 2
    symbol_list.pop(symbol_by_memory_id(iter_address))
    symbol_list.pop(symbol_by_memory_id(end_address))     
    
# IN/OUT

def p_read(p):
    'command : READ identifier SCOLON'
    result = ''
    if type(p[2]) is tuple: # tablica indeksowana zmienna
        result += load_variable(p[2][0], p.lineno(3)) +"SWAP c\n"+ load_value(p[2][1]) +\
                'ADD c\nSWAP c\n' + load_value(p[2][2]) +"SWAP c\n" + 'SUB c\n' 
    else: #zmienna lub tablica z liczba
        result += load_value(p[2])
        i = symbol_by_memory_id(p[2]) 
        symbol_list[i][5] = True 
        if symbol_list[i][0].startswith('LOOP'):
            raise Exception('Błąd w linii {0:d}: Modyfikacja wartości iteratora pętli'.format(p.lineno(2)))
    result += 'SWAP b\n'
    result += 'GET\n'
    result += 'STORE b\n'
    p[0] = result
def p_write(p):
    'command : WRITE value SCOLON'
    global memory_index
    result = ''
    if p[2][1]:
        if type(p[2][0]) is tuple: #tablica indeksowana zmienna
            result += load_array(*p[2][0],p.lineno(1)) 
        else:
            result += load_variable(p[2][0],p.lineno(1))#zmienna lub tablica numerowana      
    else:
        result+= load_value(p[2][0])
    
    result += 'PUT\n'
    p[0]=result
# expression

def p_expr_value(p):
    'expression : value'
    global symbol_list
    result = ''
    
    if p[1][1]: #zmienna
        if type(p[1][0]) is tuple: #tablica indeksowana przez zmienna
            result += load_array(*p[1][0], p.linespan(1)[0])
        else:#zmiena/ tablica numerowa
            result += load_variable(p[1][0], p.linespan(1)[0])
    else:
         # liczba
         result += load_value(p[1][0])
    result += 'SWAP c\n'
    p[0] = result
# operations
def p_expr_add(p):
    '''expression : value PLUS value'''
    
    result ='' 
    if  p[3][1]:#tablica/zmienna
        if type(p[3][0]) is tuple:
            if p[1][1]:
                if type(p[1][0]) is tuple:#tab tab
                     result += load_array(*p[1][0], p.lineno(2))
                else: # zmienna tab
                    result += load_variable(p[1][0],p.lineno(2))
            else:#liczba tablica
                result += load_value(p[1][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_array(*p[3][0],p.lineno(2))
        else:#3 zmienna
            if p[1][1]:
                if type(p[1][0]) is tuple:#tab zmienna
                    result += load_array(*p[1][0], p.lineno(2))
                else: # zmienna zmienna
                    result += load_variable(p[1][0],p.lineno(2))
            else:#liczba zmienna
                result += load_value(p[1][0])
            result += 'SWAP c\n' + load_variable(p[3][0],p.lineno(2))      
    else: 
        if p[1][1]:
            if type(p[1][0]) is tuple:#tab licz
                    result += load_array(*p[1][0], p.lineno(2)) 
            else: # z licz
                result += load_variable(p[1][0],p.lineno(2))
        else:#liczba licz
            result += load_value(p[1][0])
        result += 'SWAP c\n'
        result += load_value(p[3][0])
    result += 'ADD c\nSWAP c\n'
    p[0] = result

def p_expr_sub(p):
    '''expression : value MINUS value'''

    result ='' 
    if  p[3][1]:
        if type(p[3][0]) is tuple:
            if p[1][1]:
                if type(p[1][0]) is tuple:#tab tab
                     result += load_array(*p[1][0], p.lineno(2))
                else: # zmienna tab
                    result += load_variable(p[1][0],p.lineno(2))
            else:#liczba tablica
                result += load_value(p[1][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_array(*p[3][0],p.lineno(2))
        else:#3 zmienna
            if p[1][1]:
                if type(p[1][0]) is tuple:#tab zmienna
                    result += load_array(*p[1][0], p.lineno(2))
                else: # zmienna zmienna
                    result += load_variable(p[1][0],p.lineno(2))
            else:#liczba zmienna
                result += load_value(p[1][0])
            result += 'SWAP c\n' + load_variable(p[3][0],p.lineno(2))      
    else: 
        if p[1][1]:
            if type(p[1][0]) is tuple:#tab licz
                    result += load_array(*p[1][0], p.lineno(2)) 
            else: # z licz
                result += load_variable(p[1][0],p.lineno(2))
        else:#liczba licz
            result += load_value(p[1][0])
        result += 'SWAP c\n'
        result += load_value(p[3][0])
    result += 'SWAP c\nSUB c\nSWAP c\n'
    p[0] = result

def p_expr_mul(p):
    'expression : value TIMES value'
    result = ''

    if  p[1][1]:
        if type(p[1][0]) is tuple:
            if p[3][1]:
                if type(p[3][0]) is tuple:#tab tab
                     result += load_array(*p[3][0], p.lineno(2))
                else: # zmienna tab
                    result += load_variable(p[3][0],p.lineno(2))
            else:#liczba tablica
                result += load_value(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_array(*p[1][0],p.lineno(2))
            result += 'SWAP c\n'
           
        else:#1 zmienna
            if p[3][1]:
                if type(p[3][0]) is tuple:#tab zmienna
                    result += load_array(*p[3][0], p.lineno(2))
                else: # zmienna zmienna
                    result += load_variable(p[3][0],p.lineno(2))
            else:#liczba zmienna
                result += load_value(p[3][0])
            result += 'SWAP c\n' + load_variable(p[1][0],p.lineno(2))  
            result += 'SWAP c\n'
            
    else: 
        if p[3][1]:
            if type(p[3][0]) is tuple:#tab licz
                    result += load_array(*p[3][0], p.lineno(2)) 
            else: # z licz
                result += load_variable(p[3][0],p.lineno(2))
        else:#liczba licz
            result += load_value(p[3][0])
        result += 'SWAP c\n'
        result += load_value(p[1][0])
        result += 'SWAP c\n'
        
        
    
    
   
    result += 'RESET d\n' + 'RESET e\n' +'RESET f\n' + 'RESET g\n' + 'RESET h\n'
    result +=   'INC e\n' + 'DEC d\n' + 'JZERO 7\n' + 'JPOS 4\n' + 'DEC h\n' + 'SWAP  g\n' + 'SUB g\n' + 'SWAP  c\n' + 'SWAP  f\n' + 'SWAP  c\n' + 'JZERO 20\n' + 'SWAP  c\n' + 'RESET g\n' + 'SWAP  g\n' +\
                'ADD c\n' + 'SHIFT d\n' + 'SHIFT e\n' + 'SUB c\n' + 'JZERO 4\n' + 'SWAP  g\n' + 'ADD f\n' + 'JUMP 2\n' + 'SWAP  g\n' + 'SWAP  f\n' + 'SHIFT e\n' + 'SWAP  f\n' + 'SWAP  c\n' + 'SHIFT d\n' + 'SWAP  c\n' +\
                'JUMP -20\n' + 'SWAP  h\n' + 'JZERO 4\n' + 'RESET a\n' + 'SUB c\n' +\
                'SWAP c\n'

    
    p[0] = result


def p_expr_div(p):
    'expression : value DIV value'

    result = ''
    
    if  p[1][1]:
        if type(p[1][0]) is tuple:
            if p[3][1]:
                if type(p[3][0]) is tuple:#tab tab
                     result += load_array(*p[3][0], p.lineno(2))
                else: # zmienna tab
                    result += load_variable(p[3][0],p.lineno(2))
            else:#liczba tablica
                result += load_value(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_array(*p[1][0],p.lineno(2))
        else:#1 zmienna
            if p[3][1]:
                if type(p[3][0]) is tuple:#tab zmienna
                    result += load_array(*p[3][0], p.lineno(2))
                else: # zmienna zmienna
                    result += load_variable(p[3][0],p.lineno(2))
            else:#liczba zmienna
                result += load_value(p[3][0])
            result += 'SWAP c\n' + load_variable(p[1][0],p.lineno(2))  
    else: 
        if p[3][1]:
            if type(p[3][0]) is tuple:#tab licz
                    result += load_array(*p[3][0], p.lineno(2))  
            else: # z licz
                result += load_variable(p[3][0],p.lineno(2))  
        else:#liczba licz
            result += load_value(p[3][0])
        result += 'SWAP c\n'
        result += load_value(p[1][0])
        

    result +=   'RESET b\n' + 'RESET d\n' + 'RESET e\n' +'RESET f\n' + 'RESET g\n' + 'RESET h\n'
    result +=   'SWAP b\n' + 'SWAP c\n' + 'INC c\n' + 'DEC d\n' + 'JZERO 66\n' + 'JPOS 6\n' + 'INC h\n' + 'INC h\n' + 'SWAP g\n' + 'SUB g\n' + 'RESET g\n' + 'SWAP b\n' + 'JZERO 58\n' +\
                'JPOS 5\n' + 'INC h\n' + 'SWAP g\n' + 'SUB g\n' + 'RESET g\n' + 'SWAP f\n' + 'ADD f\n' + 'JZERO 4\n' + 'SHIFT d\n' + 'INC e\n' + 'JUMP -3\n' +\
                'SWAP f\n' + 'SWAP b\n' +'SWAP g\n' + 'ADD g\n' + 'JZERO 4\n' + 'SHIFT d\n' + 'INC f\n' + 'JUMP -3\n' +\
                'SWAP g\n' + 'SWAP b\n' + 'SWAP e\n' 'SUB f\n' + 'JNEG 35\n' + 'RESET g\n' + 'SWAP g\n' + 'ADD g\n' + 'SWAP g\n' + 'INC g\n' +\
                'JZERO 6\n' + 'SWAP b\n' + 'SHIFT c\n' + 'DEC b\n' + 'SWAP b\n' + 'JUMP -5\n' + 'SWAP e\n' + 'RESET e\n' + 'SWAP g\n' +\
                'JZERO 19\n' + 'RESET f\n' + 'SWAP f\n' + 'ADD g\n' + 'SUB b\n' + 'SWAP e\n' + 'SHIFT c\n' + 'SWAP e\n' +\
                'JNEG 5\n' + 'INC e\n' + 'SWAP g\n' + 'SUB b\n' + 'SWAP g\n' + 'SWAP b\n' + 'SHIFT d\n' + 'SWAP b\n' + 'SWAP f\n' + 'DEC a\n' + 'JUMP -18\n' +\
                'JUMP 6\n' + 'SWAP e\n' + 'SWAP g\n' + 'ADD g\n' + 'SWAP g\n' + 'RESET e\n' + 'SWAP e\n' + 'SWAP h\n' +\
                'JZERO 16\n' + 'DEC a\n' + 'JZERO 4\n' + 'DEC a\n' + 'JZERO 2\n' + 'JUMP 11\n' + 'RESET f\n' + 'SWAP h\n' +\
                'SWAP f\n' + 'SUB f\n' + 'SWAP h\n' + 'SWAP g\n' + 'JZERO 4\n' + 'SWAP h\n' + 'SUB c\n' + 'SWAP h\n' + 'SWAP h\n' +\
                'SWAP c\n'
    p[0] = result

def p_expr_mod(p):
    'expression : value MOD value'

    result = ''
    if  p[1][1]:
        if type(p[1][0]) is tuple:
            if p[3][1]:
                if type(p[3][0]) is tuple:#tab tab
                     result += load_array(*p[3][0], p.lineno(2))
                else: # zmienna tab
                    result += load_variable(p[3][0],p.lineno(2))
            else:#liczba tablica
                result += load_value(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_array(*p[1][0],p.lineno(2))
        else:#1 zmienna
            if p[3][1]:
                if type(p[3][0]) is tuple:#tab zmienna
                    result += load_array(*p[3][0], p.lineno(2))
                else: # zmienna zmienna
                    result += load_variable(p[3][0],p.lineno(2))
            else:#liczba zmienna
                result += load_value(p[3][0])
            result += 'SWAP c\n' + load_variable(p[1][0],p.lineno(2))  
    else: 
        if p[3][1]:
            if type(p[3][0]) is tuple:#tab licz
                    result += load_array(*p[3][0], p.lineno(2)) 
            else: # z licz
                result += load_variable(p[3][0],p.lineno(2))
        else:#liczba licz
            result += load_value(p[3][0])
        result += 'SWAP c\n'
        result += load_value(p[1][0])
        
    result +=   'RESET b\n' + 'RESET d\n' + 'RESET e\n' +'RESET f\n' + 'RESET g\n' + 'RESET h\n'
    result +=   'SWAP b\n' + 'SWAP c\n' + 'DEC d\n' + 'SWAP c\n' + 'ADD c\n' + 'SWAP c\n' + 'JZERO 74\n' + 'JPOS 6\n' + 'INC f\n' + 'INC f\n' + 'SWAP e\n' + 'SUB e\n' + 'RESET e\n' + 'SWAP b\n' +\
                'JZERO 66\n' + 'JPOS 5\n' + 'INC f\n' + 'SWAP e\n' + 'SUB e\n' + 'RESET e\n' + 'SWAP h\n' + 'ADD h\n' + 'JZERO 4\n' + 'SHIFT d\n' + 'INC g\n' + 'JUMP -3\n' +\
                'SWAP h\n' + 'SWAP b\n' + 'SWAP e\n' + 'ADD e\n' + 'JZERO 4\n' + 'SHIFT d\n' + 'INC h\n' + 'JUMP -3\n' + 'SWAP e\n' + 'SWAP b\n' + 'SWAP g\n' + 'SUB h\n' + 'JNEG 44\n' +\
                'RESET e\n' + 'SWAP e\n' + 'ADD e\n' + 'SWAP e\n' + 'INC e\n' + 'JZERO 10\n' + 'SWAP b\n' + 'INC d\n' + 'INC d\n' + 'SHIFT d\n' + 'DEC d\n' + 'DEC d\n' + 'DEC b\n' + 'SWAP b\n' + 'JUMP -9\n' +\
                'SWAP g\n' + 'RESET g\n' + 'SWAP e\n' + 'JZERO 23\n' + 'RESET h\n' + 'SWAP h\n' + 'ADD e\n' + 'SUB b\n' + 'SWAP g\n' + 'INC d\n' + 'INC d\n' + 'SHIFT d\n' + 'DEC d\n' + 'DEC d\n' + 'SWAP g\n' +\
                'JNEG 5\n' + 'INC g\n' + 'SWAP e\n' + 'SUB b\n' + 'SWAP e\n' + 'SWAP b\n' + 'SHIFT d\n' + 'SWAP b\n' + 'SWAP h\n' + 'DEC a\n' + 'JUMP -22\n' +\
                'SWAP e\n' + 'JUMP 2\n' + 'SWAP g\n' + 'SWAP f\n' + 'JZERO 21\n' + 'DEC a\n' + 'JZERO 13\n' + 'DEC a\n' + 'JZERO 7\n' + 'RESET h\n' + 'SWAP f\n' + 'SWAP h\n' + 'SUB h\n' + 'SWAP f\n' +\
                'JUMP 11\n' + 'SWAP f\n' + 'ADD c\n' + 'SWAP f\n' + 'JUMP 7\n' + 'RESET h\n' + 'SWAP f\n' + 'SWAP h\n' + 'SUB h\n' + 'ADD c\n'  + 'SWAP f\n'+ 'SWAP f\n'  + 'SWAP c\n'
            
    p[0] = result

# conditions

def p_condition_eq(p):
    'conditionition : value EQ value'
    result = ''
    if p[1][1] == False:
        if p[3][1] == False:#liczba liczba
            result += load_value(p[3][0])
        else:
            if type(p[3][1]) is tuple: #liczba tablica zmienna
                result += load_array(*p[3][0,p.lineno(2)])
            else:#liczba zmienna
                result += load_variable(p[3][0],p.lineno(2))      
        result += 'SWAP c\n'    
        result += load_value(p[1][0])
    else:#zmienna 1
        if type(p[1][0])is tuple: # 1 tablica index zmienna
            if p[3][1] == False:# 2 liczba
                result += load_value(p[3][0])
            else:
                if type(p[3][0]) is tuple:# 2 tablica index zmienna
                    result += load_array(*p[3][0],p.lineno(2))
                else:
                    result += load_variable(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_array(*p[1][0],p.lineno(2)) 
        else:# 1 jest zmienna
            if p[3][1] == False:# 2 liczba
                result += load_value(p[3][0])
            else:
                if type(p[3][0]) is tuple:# 2 tablica index zmienna
                    result += load_array(*p[3][0],p.lineno(2))
                else:
                    result += load_variable(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_variable(p[1][0],p.lineno(2))
    result += 'SUB c\nRESET c\n'
    p[0] = result

def p_condition_neq(p):
    'conditionition : value NEQ value'
    
    result = ''
    if p[1][1] == False:
        if p[3][1] == False:#liczba liczba
            result += load_value(p[3][0])
        else:
            if type(p[3][1]) is tuple: #liczba tablica zmienna
                result += load_array(*p[3][0,p.lineno(2)])
            else:#liczba zmienna
                result += load_variable(p[3][0],p.lineno(2))      
        result += 'SWAP c\n'    
        result += load_value(p[1][0])
    else:#zmienna 1
        if type(p[1][0])is tuple: # 1 tablica index zmienna
            if p[3][1] == False:# 2 liczba
                result += load_value(p[3][0])
            else:
                if type(p[3][0]) is tuple:# 2 tablica index zmienna
                    result += load_array(*p[3][0],p.lineno(2))
                else:
                    result += load_variable(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_array(*p[1][0],p.lineno(2)) 
        else:# 1 jest zmienna
            if p[3][1] == False:# 2 liczba
                result += load_value(p[3][0])
            else:
                if type(p[3][0]) is tuple:# 2 tablica index zmienna
                    result += load_array(*p[3][0],p.lineno(2))
                else:
                    result += load_variable(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_variable(p[1][0],p.lineno(2))
        
    result += 'SUB c\nRESET c\nJZERO 3\nRESET a\nJUMP 2\nINC a\n'
   
    p[0] = result


def p_condition_le(p):
    'conditionition : value LE value'

    result = ''
    if p[1][1] == False:

        if p[3][1] == False:#liczba liczba
            result += load_value(p[3][0])
        else:
            if type(p[3][1]) is tuple: #liczba tablica zmienna
                result += load_array(*p[3][0,p.lineno(2)])
            else:#liczba zmienna
                result += load_variable(p[3][0],p.lineno(2))      
        result += 'SWAP c\n'    
        result += load_value(p[1][0])
    else:#zmienna 1
        if type(p[1][0])is tuple: # 1 tablica index zmienna
            if p[3][1] == False:# 2 liczba
                result += load_value(p[3][0])
            else:
                if type(p[3][0]) is tuple:# 2 tablica index zmienna
                    result += load_array(*p[3][0],p.lineno(2))
                else:
                    result += load_variable(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_array(*p[1][0],p.lineno(2)) 
        else:# 1 jest zmienna
            if p[3][1] == False:# 2 liczba
                result += load_value(p[3][0])
            else:
                if type(p[3][0]) is tuple:# 2 tablica index zmienna
                    result += load_array(*p[3][0],p.lineno(2))
                else:
                    result += load_variable(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_variable(p[1][0],p.lineno(2))
    
    result += 'SUB c\nJPOS 4\nJZERO 3\nRESET a\nJUMP 3\nRESET a\nINC a\n'
    p[0] = result

def p_condition_ge(p):
    '''conditionition : value GE value'''

    result = ''
    if p[1][1] == False:
        if p[3][1] == False:#liczba liczba
            result += load_value(p[3][0])
        else:
            if type(p[3][1]) is tuple: #liczba tablica zmienna
                result += load_array(*p[3][0,p.lineno(2)])
            else:#liczba zmienna
                result += load_variable(p[3][0],p.lineno(2))      
        result += 'SWAP c\n'    
        result += load_value(p[1][0])
    else:#zmienna 1
        if type(p[1][0])is tuple: # 1 tablica index zmienna
            if p[3][1] == False:# 2 liczba
                result += load_value(p[3][0])
            else:
                if type(p[3][0]) is tuple:# 2 tablica index zmienna
                    result += load_array(*p[3][0],p.lineno(2))
                else:
                    result += load_variable(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_array(*p[1][0],p.lineno(2)) 
        else:# 1 jest zmienna
            if p[3][1] == False:# 2 liczba
                result += load_value(p[3][0])
            else:
                if type(p[3][0]) is tuple:# 2 tablica index zmienna
                    result += load_array(*p[3][0],p.lineno(2))
                else:
                    result += load_variable(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_variable(p[1][0],p.lineno(2))
    result += 'SUB c\nJNEG 4\nJZERO 3\nRESET a\nJUMP 3\nRESET a\nINC a\n'
    p[0] = result


def p_condition_leq_geq(p):
    '''conditionition : value LEQ value
                 | value GEQ value'''

    result = ''
    if p[1][1] == False:

        if p[3][1] == False:#liczba liczba
            result += load_value(p[3][0])
        else:
            if type(p[3][1]) is tuple: #liczba tablica zmienna
                result += load_array(*p[3][0,p.lineno(2)])
            else:#liczba zmienna
                result += load_variable(p[3][0],p.lineno(2))      
        result += 'SWAP c\n'    
        result += load_value(p[1][0])
    else:#zmienna 1
        if type(p[1][0])is tuple: # 1 tablica index zmienna
            if p[3][1] == False:# 2 liczba
                result += load_value(p[3][0])
            else:
                if type(p[3][0]) is tuple:# 2 tablica index zmienna
                    result += load_array(*p[3][0],p.lineno(2))
                else:
                    result += load_variable(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_array(*p[1][0],p.lineno(2)) 
        else:# 1 jest zmienna
            if p[3][1] == False:# 2 liczba
                result += load_value(p[3][0])
            else:
                if type(p[3][0]) is tuple:# 2 tablica index zmienna
                    result += load_array(*p[3][0],p.lineno(2))
                else:
                    result += load_variable(p[3][0],p.lineno(2))
            result += 'SWAP c\n'
            result += load_variable(p[1][0],p.lineno(2))

    if p[2] == 'LEQ':
        result += 'SUB c\nJPOS 3\nRESET a\nJUMP 3\nRESET a\nINC a\n'
    else:
        result += 'SUB c\nJNEG 3\nRESET a\nJUMP 3\nRESET a\nINC a\n'
    p[0] = result

# value

def p_value_num(p):
    'value : NUM'
    p[0] = (p[1], False) # liczba

def p_value_id(p):
    'value : identifier'
    p[0] = (p[1], True) # zmienna

# identifier

def p_identifier_variable(p):
    'identifier : PID'

    global symbol_list
    n = None
    name = 'LOOP'+p[1]
    for k in range(len(symbol_list)):
        if symbol_list[k][0] == name:
            n = k
            break
    if n == None:
        name = p[1]
        for k in range(len(symbol_list)):
            if symbol_list[k][0] == name:
                n = k
                break
    if n == None:
        raise Exception('Błąd w linii {0:d}: użycie niezadeklarowanej zmiennej {1:}'.format(p.lineno(0), name))
    if symbol_list[n][1]:
        raise Exception('Błąd w linii {0:d}: Niewłaściwe użycie zmiennej tablicowej {1:}'.format(p.lineno(1), p[1]))
    p[0] = symbol_list[n][4] 

def p_identifier_array_pid(p):
    'identifier : PID LPAR PID RPAR'
    global symbol_list
    
    id_mem = None
    for k in range(len(symbol_list)):
        if symbol_list[k][0] == 'LOOP'+p[3]:
            id_mem = symbol_list[k][4]
    if id_mem == None:
        for k in range(len(symbol_list)):
            if symbol_list[k][0] == p[3]:
                id_mem = symbol_list[k][4]
    if id_mem == None:
        raise Exception('Błąd w linii {0:d}: użycie niezadeklarowanej zmiennej {1:}'.format(p.lineno(3), id_mem))
    n = None
    name = p[1]
    for k in range(len(symbol_list)):
        if symbol_list[k][0] == name:
            n = k
            break
    if n == None:
        raise Exception('Błąd w linii {0:d}: użycie niezadeklarowanej zmiennej {1:}'.format(p.lineno(1), name))
    if not symbol_list[n][1]:
        raise Exception('Błąd w linii {0:d}: Zmienna {1:} nie jest tablicą'.format(p.lineno(1), p[1]))
    p[0] = (id_mem, symbol_list[n][4], symbol_list[n][2]) 

def p_identifier_array_num(p):
    'identifier : PID LPAR NUM RPAR'
    
    n = None
    name = p[1]
    for k in range(len(symbol_list)):
        if symbol_list[k][0] == name:
            n = k
            break
    if n is None:
        raise Exception('Błąd w linii {0:d}: użycie niezadeklarowanej zmiennej {1:}'.format(p.lineno(1), name))
    if not symbol_list[n][1]:
        raise Exception('Błąd w linii {0:d}: Zmienna {1:} nie jest tablicą'.format(p.lineno(1), p[1]))    
    
    else:
        if p[3] < symbol_list[n][2] or p[3] > symbol_list[n][3]:
            raise Exception('Błąd w linii {0:d}: Indeks {1:d} jest poza zakresem tablicy {2:}'.format(p.lineno(1), p[3], p[1]))  
        p[0] = symbol_list[n][4] + p[3] - symbol_list[n][2] 

# main

if __name__ == '__main__':
    data = ''
    with open(sys.argv[1], 'r') as f:
        data = f.read()
    lexer = build_lex()
    parser = yacc.yacc()
    try:
        compiled = parser.parse(data, tracking=True)
        with open(sys.argv[2], 'w') as f:
            f.write(compiled)
    except Exception as e:
        print(e)
