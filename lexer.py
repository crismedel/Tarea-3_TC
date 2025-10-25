import re
from collections import namedtuple

# Definición de un Token
Token = namedtuple('Token', ['type', 'value', 'line', 'column'])

def tokenize(code):
    """
    Analizador Léxico para el subconjunto de FORTRAN77.
    """
    # Especificación de tokens (expresiones regulares)
    # IMPORTANTE: Las palabras clave (keywords) deben ir ANTES que el ID.
    token_specification = [
        # --- Comentario de FORTRAN 77 (C, c, o * en la columna 1) ---
        ('COMMENT',  r'^[C\*].*'), # El ancla ^ marca el inicio de una línea
        
        # --- Palabras clave de FORTRAN77 ---
        ('IF',       r'IF'),
        ('THEN',     r'THEN'),
        ('ELSE',     r'ELSE'),
        ('ENDIF',    r'ENDIF'),
        
        # --- Operadores Relacionales ---
        # (FORTRAN77 usa .GT., .LT., .EQ.)
        ('GT',       r'\.GT\.'),
        ('LT',       r'\.LT\.'),
        ('EQ',       r'\.EQ\.'),
        
        # --- Gramática original ---
        ('NUMBER',   r'\d+'),           # Un número
        ('ASSIGN',   r'='),             # Símbolo de asignación
        ('PLUS',     r'\+'),            # Operador de suma
        ('MULTIPLY', r'\*'),            # Operador de multiplicación
        ('LPAREN',   r'\('),            # Paréntesis izquierdo
        ('RPAREN',   r'\)'),            # Paréntesis derecho
        ('ID',       r'[A-Z][A-Z0-9]*'), # Identificadores (DEBE IR DESPUÉS DE KEYWORDS)
        ('SKIP',     r'[ \t]+'),        # Ignorar espacios y tabs
        ('NEWLINE',  r'\n'),           # Para contar líneas (simplificado, FORTRAN real es más complejo)
        ('MISMATCH', r'.'),             # Cualquier otro caracter (error)
    ]
    
    # Compilar las expresiones regulares
    tok_regex = re.compile('|'.join('(?P<%s>%s)' % pair for pair in token_specification), re.MULTILINE)
    
    line_num = 1
    line_start = 0
    tokens = [] # Lista de tokens generados

    # Analizar el código (convertimos a mayúsculas)
    # Usamos .upper() porque FORTRAN77 es case-insensitive
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
            pass # Ignorar espacios
        elif kind == 'COMMENT': # <--- AÑADIR ESTE ELIF
            pass # Ignorar la línea de comentario
        elif kind == 'MISMATCH':
            # Requisito: Detección de errores léxicos
            raise RuntimeError(f'Error Léxico: Caracter inesperado {value!r} '
                               f'en línea {line_num}, columna {column}')

    # Añadir el token especial de fin de entrada
    tokens.append(Token('END_OF_INPUT', '$', line_num, 0))
    return tokens