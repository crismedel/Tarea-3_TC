C --- test_lexical_error_symbol.f ---
C Debería fallar en el analizador léxico.

X = 10
Y = 5 ! 3  C El símbolo '!' no es un token válido.
