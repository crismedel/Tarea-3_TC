import sys
from lexer import tokenize
from parser_ll1 import ParserLL1

# --- Definición de la Gramática y Tabla (AMPLIADA) ---
# (Recuerda: las listas de reglas están en orden REVERSO para la Pila)

start_symbol = 'Program' # <--- Símbolo inicial cambiado

parsing_table = {
    # Símbolo Inicial
    'Program': {
        # FIRST(StatementList) + FOLLOW(StatementList) si StatementList -> epsilon
        'ID': ['END_OF_INPUT', 'StatementList'],
        'IF': ['END_OF_INPUT', 'StatementList'],
        'ENDIF': ['END_OF_INPUT', 'StatementList'],
        'ELSE': ['END_OF_INPUT', 'StatementList'],
        'END_OF_INPUT': ['END_OF_INPUT', 'StatementList'],
    },
    
    # StatementList -> Statement StatementList | epsilon
    'StatementList': {
        # FIRST(Statement)
        'ID': ['StatementList', 'Statement'],
        'IF': ['StatementList', 'Statement'],
        # FOLLOW(StatementList)
        'ENDIF': ['EPSILON'],
        'ELSE': ['EPSILON'],
        'END_OF_INPUT': ['EPSILON'],
    },
    
    # Statement -> Asignación | Condicional
    'Statement': {
        'ID': ['E', 'ASSIGN', 'ID'], # Regla de Asignación
        'IF': ['ENDIF', 'ElseBlock', 'StatementList', 'THEN', 'RPAREN', 'Condition', 'LPAREN', 'IF'] # Regla IF
    },

    # ElseBlock -> ELSE StatementList | epsilon
    'ElseBlock': {
        'ELSE': ['StatementList', 'ELSE'], # Regla ELSE
        'ENDIF': ['EPSILON'] # Regla Epsilon (FOLLOW(ElseBlock))
    },
    
    # Condition -> E RelOp E
    'Condition': {
        # FIRST(E)
        'ID': ['E', 'RelOp', 'E'],
        'NUMBER': ['E', 'RelOp', 'E'],
        'LPAREN': ['E', 'RelOp', 'E'],
    },
    
    # RelOp -> GT | LT | EQ
    'RelOp': {
        'GT': ['GT'],
        'LT': ['LT'],
        'EQ': ['EQ'],
    },
    
    # --- Reglas de Expresión Aritmética (Sin cambios) ---
    'E': {
        'ID': ["E'", 'T'],
        'NUMBER': ["E'", 'T'],
        'LPAREN': ["E'", 'T']
    },
    "E'": {
        'PLUS': ["E'", 'T', 'PLUS'],
        'RPAREN': ['EPSILON'],
        'END_OF_INPUT': ['EPSILON'],
        'GT': ['EPSILON'],
        'LT': ['EPSILON'],
        'EQ': ['EPSILON'],
        'IF': ['EPSILON'], 
        'ID': ['EPSILON'],
        'ELSE': ['EPSILON'],  # <--- AÑADIR ESTA LÍNEA
        'ENDIF': ['EPSILON'], # <--- AÑADIR ESTA LÍNEA
    },
    'T': {
        'ID': ["T'", 'F'],
        'NUMBER': ["T'", 'F'],
        'LPAREN': ["T'", 'F']
    },
    "T'": {
        'PLUS': ['EPSILON'],
        'MULTIPLY': ["T'", 'F', 'MULTIPLY'],
        'RPAREN': ['EPSILON'],
        'END_OF_INPUT': ['EPSILON'],
        'GT': ['EPSILON'],
        'LT': ['EPSILON'],
        'EQ': ['EPSILON'],
        'IF': ['EPSILON'],
        'ID': ['EPSILON'],
        'ELSE': ['EPSILON'],  # <--- AÑADIR ESTA LÍNEA
        'ENDIF': ['EPSILON'], # <--- AÑADIR ESTA LÍNEA
    },
    'F': {
        'ID': ['ID'],
        'NUMBER': ['NUMBER'],
        'LPAREN': ['RPAREN', 'E', 'LPAREN']
    }
}
# --- Fin de la Definición ---


def main():
    # Ruta del archivo
    filepath = 'test.f'
    
    try:
        with open(filepath, 'r') as f:
            code = f.read() # Ya no usamos .upper() aquí, el lexer lo hace
            
        # 1. Fase de Implementación: Análisis Léxico
        print("--- Fase Léxica ---")
        tokens = tokenize(code)
        print("Tokens generados:")
        for token in tokens:
            print(f"  {token}")
        print("-" * 50)
        
        # 2. Fase de Implementación: Análisis Sintáctico
        print("\n--- Fase Sintáctica (LL(1)) ---")
        parser = ParserLL1(parsing_table, start_symbol)
        parser.parse(tokens)
        
    except FileNotFoundError:
        print(f"Error: El archivo '{filepath}' no fue encontrado.")
    except RuntimeError as e:
        print(e)
        sys.exit(1)

if __name__ == "__main__":
    main()