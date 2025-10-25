# Este es un generador de analizadores LL(1)
# Contiene la gramática y los algoritmos para FIRST, FOLLOW, y la Tabla de Parsing.

# --- 1. Definición de la Gramática ---
# Usamos un diccionario donde las claves son No-Terminales.
# Cada valor es una lista de producciones (reglas).
# Cada producción es una lista de símbolos.
# 'EPSILON' es la palabra clave para la cadena vacía.

GRAMMAR = {
    'Program': [
        ['StatementList', 'END_OF_INPUT']
    ],
    'StatementList': [
        ['Statement', 'StatementList'],
        ['EPSILON']
    ],
    'Statement': [
        ['ID', 'ASSIGN', 'E'],
        ['IF', 'LPAREN', 'Condition', 'RPAREN', 'THEN', 'StatementList', 'ElseBlock', 'ENDIF']
    ],
    'ElseBlock': [
        ['ELSE', 'StatementList'],
        ['EPSILON']
    ],
    'Condition': [
        ['E', 'RelOp', 'E']
    ],
    'RelOp': [
        ['GT'], ['LT'], ['EQ']
    ],
    'E': [
        ['T', "E'"]
    ],
    "E'": [
        ['PLUS', 'T', "E'"],
        ['EPSILON']
    ],
    'T': [
        ['F', "T'"]
    ],
    "T'": [
        ['MULTIPLY', 'F', "T'"],
        ['EPSILON']
    ],
    'F': [
        ['LPAREN', 'E', 'RPAREN'],
        ['ID'],
        ['NUMBER']
    ]
}

START_SYMBOL = 'Program'

# --- 2. Funciones Auxiliares ---

def get_terminals_and_non_terminals(grammar):
    """Extrae los terminales y no-terminales de la gramática."""
    non_terminals = set(grammar.keys())
    terminals = set()
    
    for productions in grammar.values():
        for production in productions:
            for symbol in production:
                if symbol not in non_terminals and symbol != 'EPSILON':
                    terminals.add(symbol)
    return terminals, non_terminals

TERMINALS, NON_TERMINALS = get_terminals_and_non_terminals(GRAMMAR)
TERMINALS.add('END_OF_INPUT') # Agregamos el fin de entrada

# --- 3. Cálculo de Conjuntos FIRST ---

def compute_first_sets(grammar):
    """Calcula los conjuntos FIRST para la gramática dada."""
    first_sets = {nt: set() for nt in NON_TERMINALS}
    first_sets.update({t: {t} for t in TERMINALS})
    first_sets['EPSILON'] = {'EPSILON'}

    changed = True
    while changed:
        changed = False
        for non_terminal, productions in grammar.items():
            for production in productions:
                # Caso 1: Producción Epsilon
                if production == ['EPSILON']:
                    if 'EPSILON' not in first_sets[non_terminal]:
                        first_sets[non_terminal].add('EPSILON')
                        changed = True
                    continue
                
                # Caso 2: Producción con símbolos
                can_be_epsilon = True
                for symbol in production:
                    first_of_symbol = first_sets[symbol]
                    
                    # Añadir FIRST(symbol) - {EPSILON} a FIRST(non_terminal)
                    for terminal in first_of_symbol - {'EPSILON'}:
                        if terminal not in first_sets[non_terminal]:
                            first_sets[non_terminal].add(terminal)
                            changed = True
                    
                    # Si EPSILON no está en FIRST(symbol), paramos
                    if 'EPSILON' not in first_of_symbol:
                        can_be_epsilon = False
                        break
                
                # Si todos los símbolos en la producción pueden ser EPSILON
                if can_be_epsilon:
                    if 'EPSILON' not in first_sets[non_terminal]:
                        first_sets[non_terminal].add('EPSILON')
                        changed = True
                        
    return first_sets

# --- 4. Cálculo de Conjuntos FOLLOW ---

def compute_follow_sets(grammar, start_symbol, first_sets):
    """Calcula los conjuntos FOLLOW para la gramática."""
    follow_sets = {nt: set() for nt in NON_TERMINALS}
    follow_sets[start_symbol].add('END_OF_INPUT')

    changed = True
    while changed:
        changed = False
        for non_terminal, productions in grammar.items():
            for production in productions:
                for i in range(len(production)):
                    B = production[i]
                    if B not in NON_TERMINALS:
                        continue
                    
                    # Regla 1: A -> αBβ
                    beta = production[i+1:]
                    if beta:
                        # Calcular FIRST(β)
                        first_of_beta = set()
                        can_beta_be_epsilon = True
                        for symbol in beta:
                            first_of_symbol = first_sets[symbol]
                            first_of_beta.update(first_of_symbol - {'EPSILON'})
                            if 'EPSILON' not in first_of_symbol:
                                can_beta_be_epsilon = False
                                break
                        
                        # Añadir FIRST(β) - {EPSILON} a FOLLOW(B)
                        for terminal in first_of_beta:
                            if terminal not in follow_sets[B]:
                                follow_sets[B].add(terminal)
                                changed = True
                        
                        # Regla 2.b: Si β puede ser EPSILON, añadir FOLLOW(A) a FOLLOW(B)
                        if can_beta_be_epsilon:
                            for terminal in follow_sets[non_terminal]:
                                if terminal not in follow_sets[B]:
                                    follow_sets[B].add(terminal)
                                    changed = True
                    
                    # Regla 2.a: A -> αB
                    else:
                        for terminal in follow_sets[non_terminal]:
                            if terminal not in follow_sets[B]:
                                follow_sets[B].add(terminal)
                                changed = True
    return follow_sets

# --- 5. Construcción de la Tabla de Parsing LL(1) ---

def build_parsing_table(grammar, first_sets, follow_sets):
    """Construye la tabla de parsing LL(1) M[A, t]."""
    table = {nt: {} for nt in NON_TERMINALS}

    for non_terminal, productions in grammar.items():
        for production in productions:
            
            # Calcular FIRST(production)
            first_of_production = set()
            can_be_epsilon = True
            for symbol in production:
                first_of_symbol = first_sets.get(symbol, {symbol})
                first_of_production.update(first_of_symbol - {'EPSILON'})
                if 'EPSILON' not in first_of_symbol:
                    can_be_epsilon = False
                    break
            if can_be_epsilon:
                first_of_production.add('EPSILON')

            # Regla 1: Para cada t en FIRST(production) donde t != EPSILON
            # añadir M[A, t] = production
            for terminal in first_of_production - {'EPSILON'}:
                if terminal in table[non_terminal]:
                    raise ValueError(f"Conflicto LL(1) en M[{non_terminal}, {terminal}]")
                # Almacenamos la regla INVERTIDA para la pila
                table[non_terminal][terminal] = production[::-1]

            # Regla 2: Si EPSILON está en FIRST(production)
            if 'EPSILON' in first_of_production:
                # Para cada t en FOLLOW(A)
                for terminal in follow_sets[non_terminal]:
                    if terminal in table[non_terminal]:
                        raise ValueError(f"Conflicto LL(1) en M[{non_terminal}, {terminal}]")
                    # Usamos ['EPSILON'] para la regla vacía
                    table[non_terminal][terminal] = ['EPSILON']

    return table

# --- Función Principal del Generador ---

def generate_ll1_table():
    """Ejecuta todos los pasos para generar la tabla."""
    
    # Paso 1: Calcular FIRST
    first_sets = compute_first_sets(GRAMMAR)
    
    # Paso 2: Calcular FOLLOW
    follow_sets = compute_follow_sets(GRAMMAR, START_SYMBOL, first_sets)
    
    # Paso 3: Construir Tabla
    try:
        parsing_table = build_parsing_table(GRAMMAR, first_sets, follow_sets)
        
        # (Opcional) Imprimir los conjuntos para tu informe
        print("--- Conjuntos FIRST ---")
        for k, v in first_sets.items(): print(f"{k}: {v}")
        print("\n--- Conjuntos FOLLOW ---")
        for k, v in follow_sets.items(): print(f"{k}: {v}")
            
        return parsing_table, START_SYMBOL
        
    except ValueError as e:
        print(f"Error de Gramática: {e}")
        print("La gramática NO es LL(1).")
        return None, None

if __name__ == "__main__":
    # Esto te permite ejecutar "python ll1_generator.py" para ver los conjuntos
    generate_ll1_table()