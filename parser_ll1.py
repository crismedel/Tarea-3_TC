class ParserLL1:
    """
    Implementación de un parser LL(1) basado en tabla.
    """
    def __init__(self, table, start_symbol):
        self.table = table
        self.start_symbol = start_symbol
        # Terminales son todas las claves en las tablas anidadas
        terminals = set()
        for non_terminal in table:
            terminals.update(table[non_terminal].keys())
        self.terminals = terminals
        self.non_terminals = set(table.keys())

    def parse(self, tokens):
        """
        Analiza una lista de tokens.
        """
        # La pila del parser. Empezamos con $ y el símbolo inicial
        stack = ['END_OF_INPUT', self.start_symbol]
        
        token_idx = 0
        current_token = tokens[token_idx]
        
        print("Iniciando parsing...")
        print(f"{'Pila':<30} | {'Entrada':<20}")
        print("-" * 50)

        # Requisito: Generar una traza de derivación 
        self._print_step(stack, tokens[token_idx:])

        while len(stack) > 0:
            top = stack.pop()
            
            if top in self.terminals:
                # Caso 1: Top es un terminal
                if top == current_token.type:
                    # Coincidencia! Avanzar al siguiente token
                    token_idx += 1
                    current_token = tokens[token_idx]
                else:
                    # Error de sintaxis
                    self._raise_error(current_token, [top])
            
            elif top in self.non_terminals:
                # Caso 2: Top es un no-terminal
                try:
                    # Buscar la regla en la tabla M[Top, Token]
                    rule = self.table[top][current_token.type]
                except KeyError:
                    # Error de sintaxis: no hay regla
                    expected = list(self.table[top].keys())
                    self._raise_error(current_token, expected)
                
                # Aplicar la regla: meter los símbolos en la pila
                if rule != ['EPSILON']:
                    # La regla viene pre-invertida en nuestra tabla de ejemplo
                    # Si no, haríamos rule.reverse()
                    stack.extend(rule)
            
            elif top == 'END_OF_INPUT':
                # Caso 3: Top es $ (fin de pila)
                if current_token.type != 'END_OF_INPUT':
                    # Error: fin de pila pero quedan tokens
                    self._raise_error(current_token, ['Fin de archivo'])

            self._print_step(stack, tokens[token_idx:])

        # Si salimos del bucle y todo fue bien
        if current_token.type == 'END_OF_INPUT':
            print("¡Análisis sintáctico exitoso!")
            return True # O retornar el Árbol Sintáctico
        else:
            # Esto no debería ocurrir si la gramática es correcta
            print("Error: La pila está vacía pero quedan tokens.")
            return False

    def _print_step(self, stack, tokens):
        stack_str = ' '.join(reversed(stack))
        tokens_str = ' '.join(t.value for t in tokens)
        print(f"{stack_str:<30} | {tokens_str:<20}")

    def _raise_error(self, token, expected):
        # Requisito: Detección de errores sintácticos [cite: 142]
        raise RuntimeError(f'Error Sintáctico en línea {token.line}, col {token.column}: '
                           f"Se encontró '{token.value}' "
                           f"pero se esperaba uno de: {expected}")