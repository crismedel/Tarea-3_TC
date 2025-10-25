C --- test_valid.f ---
C Un archivo de prueba válido que prueba todas las reglas.

C 1. Asignaciones simples y complejas
X = 10
Y = (A + B) * (C + 5)

C 2. Un IF-THEN-ELSE simple
IF (X .GT. 5) THEN
    A = 1
ELSE
    A = 2
ENDIF

C 3. Un IF anidado con un bloque ELSE vacío
IF (Y .EQ. 0) THEN
    IF (A .LT. 1) THEN
        Z = 100
    ENDIF
ELSE
    Z = 200
ENDIF

C 4. Un IF con un bloque THEN vacío
IF (Z .GT. 100) THEN
ELSE
    W = 1
ENDIF

C 5. Sentencia final
FINAL = A * Z + W