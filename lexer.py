import re
from collections import namedtuple

# Definición de un Token
Token = namedtuple('Token', ['type', 'value', 'line', 'column'])

def tokenize(code):
    """
    Analizador Léxico para el subconjunto de FORTRAN77.
    """
    # Especificación de tokens (expresiones regulares)
    token_specification = [
        # --- Comentario de FORTRAN 77 (C, c, o * en la columna 1) ---
        ('COMMENT',  r'^[C\*].*'), 
        
        # --- Palabras clave de FORTRAN77 ---
        ('IF',       r'IF'),
        ('THEN',     r'THEN'),
        ('ELSE',     r'ELSE'),
        ('ENDIF',    r'ENDIF'),
        
        # --- Operadores Relacionales ---
        ('GT',       r'\.GT\.'),
        ('LT',       r'\.LT\.'),
        ('EQ',       r'\.EQ\.'),
        
        # --- Gramática original ---
        ('NUMBER',   r'\d+'),           
        ('ASSIGN',   r'='),             
        ('PLUS',     r'\+'),            
        ('MULTIPLY', r'\*'),            
        ('LPAREN',   r'\('),            
        ('RPAREN',   r'\)'),            
        ('ID',       r'[A-Z][A-Z0-9]*'), 
        ('SKIP',     r'[ \t]+'),        
        ('NEWLINE',  r'\n'),           
        ('MISMATCH', r'.'),             
    ]
    
    # --- ESTA LÍNEA ES LA SIMPLIFICADA ---
    # Compila las expresiones regulares usando una list comprehension
    tok_regex = re.compile(
        '|'.join(f'(?P<{pair[0]}>{pair[1]})' for pair in token_specification), 
        re.MULTILINE
    )
    
    line_num = 1
    line_start = 0
    tokens = [] 

    # Analizar el código (convertimos a mayúsculas)
    for mo in tok_regex.finditer(code.upper()):
        kind = mo.lastgroup
        value = mo.group()
        column = mo.start() - line_start
        
        if kind == 'NUMBER':
            tokens.append(Token(kind, int(value), line_num, column))
        elif kind in ['ID', 'IF', 'THEN', 'ELSE', 'ENDIF', 'GT', 'LT', 'EQ',
                      'ASSIGN', 'PLUS', 'MULTIPLY', 'LPAREN', 'RPAREN']:
            tokens.append(Token(kind, value, line_num, column))
        elif kind == 'NEWLINE':
            line_start = mo.end()
            line_num += 1
        elif kind == 'SKIP':
            pass 
        elif kind == 'COMMENT': 
            pass 
        elif kind == 'MISMATCH':
            raise RuntimeError(f'Error Léxico: Caracter inesperado {value!r} '
                               f'en línea {line_num}, columna {column}')

    tokens.append(Token('END_OF_INPUT', '$', line_num, 0))
    return tokens