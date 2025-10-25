# ast_nodes.py
# Define las clases para cada nodo del AST usando dataclasses
from dataclasses import dataclass
from typing import List, Optional # Necesario para type hints

class Node:
    """Clase base para todos los nodos del AST."""
    pass

@dataclass
class ProgramNode(Node):
    """Nodo raíz. Contiene una lista de sentencias."""
    statements: List[Node]

@dataclass
class AssignmentNode(Node):
    """Representa una asignación: variable = expresion"""
    variable: 'IdNode'
    expression: Node

@dataclass
class IfNode(Node):
    """Representa un IF: IF (condicion) THEN ... ELSE ... ENDIF"""
    condition: 'ConditionNode'
    then_block: List[Node]
    else_block: List[Node]

@dataclass
class ConditionNode(Node):
    """Representa una condición: expr1 .OP. expr2"""
    left: Node
    op: str # String: 'GT', 'LT', 'EQ'
    right: Node

@dataclass
class BinaryOpNode(Node):
    """Representa una operación binaria: izquierda OP derecha"""
    left: Node
    op: str # String: '+', '*'
    right: Node

@dataclass
class IdNode(Node):
    """Representa un identificador (variable)."""
    name: str

@dataclass
class NumberNode(Node):
    """Representa un número literal."""
    value: int