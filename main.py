import sys
from lexer import tokenize
from parser_ll1 import ParserLL1

# --- Definición de la Gramática y Tabla ---
# (La misma tabla 'parsing_table' y 'start_symbol' definida al inicio)
parsing_table = {
    'S': {'ID': ['END_OF_INPUT', 'E', 'ASSIGN', 'ID']},
    'E': {'ID': ["E'", 'T'], 'NUMBER': ["E'", 'T'], 'LPAREN': ["E'", 'T']},
    "E'": {'PLUS': ["E'", 'T', 'PLUS'], 'RPAREN': ['EPSILON'], 'END_OF_INPUT': ['EPSILON']},
    'T': {'ID': ["T'", 'F'], 'NUMBER': ["T'", 'F'], 'LPAREN': ["T'", 'F']},
    "T'": {'PLUS': ['EPSILON'], 'MULTIPLY': ["T'", 'F', 'MULTIPLY'], 'RPAREN': ['EPSILON'], 'END_OF_INPUT': ['EPSILON']},
    'F': {'ID': ['ID'], 'NUMBER': ['NUMBER'], 'LPAREN': ['RPAREN', 'E', 'LPAREN']}
}
start_symbol = 'S'
# --- Fin de la Definición ---


def main():
    filepath = 'test.f'

    try:
        with open(filepath, 'r') as f:
            code = f.read().upper() # FORTRAN es case-insensitive
            
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