import sys
import tkinter as tk
from tkinter import scrolledtext, font

from lexer import tokenize
from recursive_parser import RecursiveDescentParser
from ast_nodes import * # Importamos los nodos

# --- 1. Función para Imprimir el AST (Modificada) ---
# Esta versión RETORNA un string en lugar de imprimir a la consola.

def format_ast_string(node, indent=0):
    """
    Función de ayuda para convertir el AST a un string legible.
    """
    prefix = '  ' * indent
    result = ""
    
    if isinstance(node, ProgramNode):
        result += f"{prefix}Program:\n"
        for stmt in node.statements:
            result += format_ast_string(stmt, indent + 1)
            
    elif isinstance(node, AssignmentNode):
        result += f"{prefix}Assignment:\n"
        result += format_ast_string(node.variable, indent + 1)
        result += format_ast_string(node.expression, indent + 1)
        
    elif isinstance(node, IfNode):
        result += f"{prefix}If:\n"
        result += f"{prefix}  Condition:\n"
        result += format_ast_string(node.condition, indent + 2)
        result += f"{prefix}  Then Block:\n"
        for stmt in node.then_block:
            result += format_ast_string(stmt, indent + 2)
        if node.else_block:
            result += f"{prefix}  Else Block:\n"
            for stmt in node.else_block:
                result += format_ast_string(stmt, indent + 2)
                
    elif isinstance(node, ConditionNode):
        result += f"{prefix}Condition ({node.op}):\n"
        result += format_ast_string(node.left, indent + 1)
        result += format_ast_string(node.right, indent + 1)
        
    elif isinstance(node, BinaryOpNode):
        result += f"{prefix}BinaryOp ({node.op}):\n"
        result += format_ast_string(node.left, indent + 1)
        result += format_ast_string(node.right, indent + 1)
        
    elif isinstance(node, IdNode):
        result += f"{prefix}Id: {node.name}\n"
        
    elif isinstance(node, NumberNode):
        result += f"{prefix}Number: {node.value}\n"
        
    else:
        result += f"{prefix}Nodo Desconocido: {node}\n"
    
    return result

# --- 2. Lógica Principal del Analizador ---

def analyze_code(code_string):
    """
    Toma un string de código, lo analiza y retorna un string
    con los tokens y el AST.
    """
    # 1. Fase Léxica
    tokens = tokenize(code_string)
    
    token_str = "--- Fase Léxica ---\n"
    for token in tokens:
        token_str += f"  {token}\n"
    token_str += "-" * 50 + "\n\n"
    
    # 2. Fase Sintáctica
    parser = RecursiveDescentParser(tokens)
    ast = parser.parse()
    
    ast_str = "--- Árbol Sintáctico Abstracto (AST) ---\n"
    ast_str += format_ast_string(ast)
    
    return token_str + ast_str

# --- 3. Función de la GUI (Callback del Botón) ---

def on_analyze_button_click():
    """
    Se ejecuta cuando el usuario presiona el botón "Analizar".
    """
    # 1. Obtener el código del cuadro de entrada
    code_input = code_text.get("1.0", tk.END)
    
    # 2. Limpiar el cuadro de salida
    output_text.config(state=tk.NORMAL) # Habilitar escritura
    output_text.delete("1.0", tk.END)
    
    try:
        # 3. Ejecutar el análisis
        result_string = analyze_code(code_input)
        
        # 4. Mostrar el resultado
        output_text.insert("1.0", result_string)
        
    except RuntimeError as e:
        # 5. Mostrar errores
        output_text.insert("1.0", f"--- ¡ERROR! ---\n\n{e}")
        
    finally:
        output_text.config(state=tk.DISABLED) # Deshabilitar escritura


# --- 4. Configuración de la Ventana Principal (GUI) ---

def main():
    global code_text, output_text # Hacemos globales para el botón

    # Configuración de la ventana
    root = tk.Tk()
    root.title("Analizador Léxico y Sintáctico - FORTRAN 77")
    root.geometry("1000x700")

    # Definir una fuente monoespaciada
    mono_font = font.Font(family="Courier New", size=10)

    # Frame principal
    main_frame = tk.Frame(root)
    main_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)

    # --- Panel Izquierdo (Entrada de Código) ---
    left_frame = tk.Frame(main_frame)
    left_frame.pack(side=tk.LEFT, fill=tk.BOTH, expand=True, padx=5)
    
    # Encadenamos .pack()
    tk.Label(left_frame, text="Código Fuente (FORTRAN 77):", font=("Arial", 12)).pack(anchor="w")
    
    code_text = scrolledtext.ScrolledText(left_frame, wrap=tk.WORD, font=mono_font, undo=True)
    code_text.pack(fill=tk.BOTH, expand=True)
    code_text.insert("1.0", "C Pega tu código de prueba aquí...\n\nX = 10\n\nIF (X .GT. 5) THEN\n    Y = 2\nENDIF\n")

    # --- Panel Derecho (Salida) ---
    right_frame = tk.Frame(main_frame)
    right_frame.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True, padx=5)

    # Encadenamos .pack()
    tk.Label(right_frame, text="Resultado (Tokens y AST):", font=("Arial", 12)).pack(anchor="w")

    output_text = scrolledtext.ScrolledText(right_frame, wrap=tk.WORD, font=mono_font)
    output_text.pack(fill=tk.BOTH, expand=True)
    output_text.config(state=tk.DISABLED) # Hacerlo de solo lectura

    # --- Botón de Análisis ---
    button_frame = tk.Frame(root)
    button_frame.pack(fill=tk.X, padx=10, pady=(0, 10))

    # Encadenamos .pack()
    analyze_button = tk.Button(button_frame, text="Analizar Código", 
                               command=on_analyze_button_click, 
                               font=("Arial", 12, "bold"), 
                               bg="#4CAF50", fg="white", 
                               width=20, height=2)
    analyze_button.pack()

    # Iniciar el bucle de la aplicación
    root.mainloop()


if __name__ == "__main__":
    main()
