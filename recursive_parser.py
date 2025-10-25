# recursive_parser.py
# Implementación de un parser recursivo descendente que construye un AST.
from ast_nodes import *

class RecursiveDescentParser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0 # Posición actual en la lista de tokens

    def parse(self):
        """Inicia el análisis. Llamado desde main."""
        ast = self.parse_program()
        if not self.is_at_end():
            self._raise_error("Tokens inesperados al final del archivo.")
        return ast

    # --- Métodos de Ayuda (Helper) ---
    def peek(self):
        """Retorna el token actual sin consumirlo."""
        return self.tokens[self.pos]

    def advance(self):
        """Consume el token actual y avanza al siguiente."""
        token = self.tokens[self.pos]
        self.pos += 1
        return token

    def is_at_end(self):
        """Verdadero si llegamos al final de la lista de tokens."""
        # Comparamos con -1 porque el último token es END_OF_INPUT
        return self.pos >= len(self.tokens) - 1
    
    def match(self, expected_type):
        """
        Consume el token actual si es del tipo esperado.
        Si no, lanza un error.
        """
        token = self.peek()
        if token.type == expected_type:
            return self.advance()
        self._raise_error(f"Se esperaba '{expected_type}' pero se encontró '{token.type}'")

    def _raise_error(self, message):
        token = self.peek()
        raise RuntimeError(f'Error Sintáctico en línea {token.line}, col {token.column}: {message}')

    # --- Métodos del Parser (uno por cada No-Terminal) ---

    def parse_program(self):
        # Program -> StatementList END_OF_INPUT
        statements = self.parse_statement_list()
        self.match('END_OF_INPUT')
        return ProgramNode(statements)

    def parse_statement_list(self):
        # StatementList -> Statement StatementList | EPSILON
        statements = []
        # FIRST(Statement) = {'ID', 'IF'}
        while self.peek().type in ('ID', 'IF'):
            statements.append(self.parse_statement())
        
        # El caso EPSILON se maneja si el loop no se ejecuta.
        return statements

    def parse_statement(self):
        # Statement -> Asignación | Condicional
        if self.peek().type == 'ID':
            # Statement -> ID ASSIGN E
            var_id_token = self.match('ID')
            self.match('ASSIGN')
            expression = self.parse_expression()
            return AssignmentNode(IdNode(var_id_token.value), expression)
        
        elif self.peek().type == 'IF':
            # Statement -> IF ( Condition ) THEN StatementList ElseBlock ENDIF
            self.match('IF')
            self.match('LPAREN')
            condition = self.parse_condition()
            self.match('RPAREN')
            self.match('THEN')
            then_block = self.parse_statement_list()
            
            else_block = []
            if self.peek().type == 'ELSE':
                # ElseBlock -> ELSE StatementList
                self.match('ELSE')
                else_block = self.parse_statement_list()
            
            # El caso EPSILON de ElseBlock se maneja si no hay 'ELSE'.
            self.match('ENDIF')
            return IfNode(condition, then_block, else_block)
        
        else:
            self._raise_error(f"Se esperaba una sentencia (ID o IF) pero se encontró '{self.peek().type}'")

    def parse_condition(self):
        # Condition -> E RelOp E
        left = self.parse_expression()
        op_token = self.parse_relop()
        right = self.parse_expression()
        return ConditionNode(left, op_token.type, right)

    def parse_relop(self):
        # RelOp -> GT | LT | EQ
        token_type = self.peek().type
        if token_type in ('GT', 'LT', 'EQ'):
            return self.advance()
        self._raise_error(f"Se esperaba un operador relacional (.GT., .LT., .EQ.)")

    def parse_expression(self):
        # E -> T E'
        # Implementamos E' usando un bucle while (iterativo)
        node = self.parse_term() # Parsea T
        
        # E' -> PLUS T E' | EPSILON
        while self.peek().type == 'PLUS':
            op_token = self.match('PLUS')
            right = self.parse_term()
            node = BinaryOpNode(left=node, op=op_token.value, right=right)
        
        return node

    def parse_term(self):
        # T -> F T'
        # Implementamos T' usando un bucle while (iterativo)
        node = self.parse_factor() # Parsea F
        
        # T' -> MULTIPLY F T' | EPSILON
        while self.peek().type == 'MULTIPLY':
            op_token = self.match('MULTIPLY')
            right = self.parse_factor()
            node = BinaryOpNode(left=node, op=op_token.value, right=right)
        
        return node

    def parse_factor(self):
        # F -> ( E ) | ID | NUMBER
        token = self.peek()
        
        if token.type == 'LPAREN':
            self.match('LPAREN')
            node = self.parse_expression() # Parsea la E interna
            self.match('RPAREN')
            return node
        
        elif token.type == 'ID':
            self.match('ID')
            return IdNode(token.value)
        
        elif token.type == 'NUMBER':
            self.match('NUMBER')
            return NumberNode(token.value)
        
        else:
            self._raise_error(f"Se esperaba '(', ID, o NUMBER pero se encontró '{token.type}'")