import re
from collections import namedtuple

# Definición de un Token
Token = namedtuple('Token', ['type', 'value', 'line', 'column'])

def tokenize(code):
    """
    Analizador Léxico para el subconjunto de FORTRAN77.
    """
    # Especificación de tokens (expresiones regulares)
    # FORTRAN77 ignora espacios.
    # Las palabras clave de FORTRAN77 (.GT., .EQ., etc.) irían aquí.
    token_specification = [
        ('NUMBER',   r'\d+'),           # Un número
        ('ASSIGN',   r'='),             # Símbolo de asignación
        ('PLUS',     r'\+'),            # Operador de suma
        ('MULTIPLY', r'\*'),            # Operador de multiplicación
        ('LPAREN',   r'\('),            # Paréntesis izquierdo
        ('RPAREN',   r'\)'),            # Paréntesis derecho
        ('ID',       r'[A-Z][A-Z0-9]*'), # Identificadores (simplificado)
        ('SKIP',     r'[ \t]+'),        # Ignorar espacios y tabs
        ('NEWLINE',  r'\n'),           # Para contar líneas
        ('MISMATCH', r'.'),             # Cualquier otro caracter (error)
    ]
    
    # Compilar las expresiones regulares
    tok_regex = re.compile('|'.join('(?P<%s>%s)' % pair for pair in token_specification))
    
    line_num = 1
    line_start = 0
    tokens = [] # Lista de tokens generados

    # Analizar el código línea por línea
    for mo in tok_regex.finditer(code):
        kind = mo.lastgroup
        value = mo.group()
        column = mo.start() - line_start
        
        if kind == 'NUMBER':
            tokens.append(Token(kind, int(value), line_num, column))
        elif kind == 'ID':
            tokens.append(Token(kind, value, line_num, column))
        elif kind in ['ASSIGN', 'PLUS', 'MULTIPLY', 'LPAREN', 'RPAREN']:
            tokens.append(Token(kind, value, line_num, column))
        elif kind == 'NEWLINE':
            line_start = mo.end()
            line_num += 1
        elif kind == 'SKIP':
            pass # Ignorar espacios
        elif kind == 'MISMATCH':
            # Requisito: Detección de errores léxicos [cite: 142]
            raise RuntimeError(f'Error Léxico: Caracter inesperado {value!r} '
                               f'en línea {line_num}, columna {column}')

    # Añadir el token especial de fin de entrada
    tokens.append(Token('END_OF_INPUT', '$', line_num, 0))
    return tokens