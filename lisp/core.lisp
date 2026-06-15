;; =============================================================================================================================
;;                                         FUNCIONES AUXILIARES
;; =============================================================================================================================

;; =============================================================================================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura (No genera efectos secundarios y ante los mismos argumentos siempre devuelve el mismo resultado)
;; ESTRATEGIA: Función Condicional (Utiliza un predicado lógico de tipo 'if' para validar y bifurcar el flujo)
;; IMPACTO: No Destructiva (Realiza operaciones aritméticas elementales sin alterar ninguna estructura en memoria)
;; =============================================================================================================================


(defun duracion-ciclo (duracion_rojo duracion_verde duracion_amarillo)
  (if (and (numberp duracion_rojo) (numberp duracion_verde) (numberp duracion_amarillo)) ; Valida que los segundos de los 3 colores sean numericos
      (+ duracion_rojo duracion_verde duracion_amarillo)                                 ; realiza la suma para tener el total en un ciclo
      nil)                                                                               ; devuelve nil en caso de que los segundos alguno de los 3 colores no sea numerico
)


;; =============================================================================================================================
;; FUNCIÓN: obtener-timestamp
;; NATURALEZA: Impura. Consulta el reloj del hardware del sistema operativo, retornando valores mutables en el tiempo.
;; ESTRATEGIA: Función Simple. Aplica una resta directa de valores de época en base a una constante temporal fija.
;; IMPACTO: No Destructiva. Produce un entero atómico independiente sin manipular ni corromper punteros en memoria.
;; =============================================================================================================================


(defun obtener-timestamp ()
    (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970))
)


;; =============================================================================================================================
;; FUNCIÓN: convertir-color
;; NATURALEZA: Pura (Realiza un mapeo directo de símbolos sin alterar el estado global ni interactuar con la E/S)
;; ESTRATEGIA: Función Condicional (Estructurada mediante un bloque 'cond' para evaluar múltiples ramas simbólicas)
;; IMPACTO: No Destructiva (Retorna un símbolo nuevo o el mismo valor de entrada sin modificar datos existentes)
;; =============================================================================================================================


(defun convertir-color (color)
    (cond
        ((equal color 'rojo) 'en-rojo)
        ((equal color 'verde) 'en-verde)
        ((equal color 'amarillo) 'en-amarillo)
        (t color)
    )
)


;; =============================================================================================================================
;; FUNCIÓN: validar-transiciones-p
;; NATURALEZA: Pura (Solo analiza los colores que recibe sin tocar nada de la memoria del sistema)
;; ESTRATEGIA: Función Predicado (Una funcion de control que solo responde T o NIL)
;; IMPACTO: No destructiva (Determina el resultado mediante operadores booleanos sin alterar estructuras)
;; =============================================================================================================================


(defun validar-transiciones-p (color-actual cambiar-a)
    (cond
        ((and (equal color-actual 'en-rojo) (equal cambiar-a 'verde))       t)
        ((and (equal color-actual 'en-verde) (equal cambiar-a 'amarillo))   t)
        ((and (equal color-actual 'en-amarillo) (equal cambiar-a 'rojo))    t)
        (t nil)
    )
)




;; ===========================================           REQUERIMIENTO 1             ===========================================


;; =============================================================================================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura. Modela la máquina de estados abstracta de forma aislada sin dependencias de variables externas.
;; ESTRATEGIA: Función Condicional. Determina la validez de la mutación de luces invocando a un predicado externo.
;; IMPACTO: No Destructiva. Utiliza el constructor 'list' para generar una estructura dinámica nueva en memoria.
;; =============================================================================================================================


(defun transicion (color-actual cambiar-a)
    (if (validar-transiciones-p color-actual cambiar-a)
      (list color-actual (format nil "cambiar-a-~a" cambiar-a))
      (list color-actual 'accion-por-defecto)
    )
)



;; ===========================================           REQUERIMIENTO 2             ===========================================



;; =============================================================================================================================
;; FUNCIÓN: ftimer
;; NATURALEZA: Pura. Recibe un parámetro de tiempo inmutable y computa el color matemáticamente sin usar el reloj global.
;; ESTRATEGIA: Función Condicional. Usa un bloque 'cond' para clasificar la congruencia modular del ciclo temporal.
;; IMPACTO: No Destructiva. Define una variable local con 'let' que se destruye automáticamente al salir del ámbito.
;; =============================================================================================================================


;; Le llamo ftimer y no timer por que SBCL no me lo tomaba

(defun ftimer (ts duracion_rojo duracion_verde duracion_amarillo)
	(let ((ciclo (duracion-ciclo duracion_rojo duracion_verde duracion_amarillo)))
		(cond
            ((or (not ciclo) (not (numberp ts))) "Error datos no numericos")
			((< (mod ts ciclo) duracion_rojo)                            'rojo)
			((< (mod ts ciclo) (+ duracion_rojo duracion_verde))         'verde)
			( t			 			                                     'amarillo)
		)
	)
)



;; ===========================================           REQUERIMIENTO 3             ===========================================



;; =============================================================================================================================
;; FUNCIÓN: auditoria
;; NATURALEZA: Impura. Modifica el flujo secundario de la terminal escribiendo logs históricos a través de 'format'.
;; ESTRATEGIA: Función Recursiva de Cola. La autollamada es la última expresión evaluada de la rama.
;; IMPACTO: No Destructiva. Pasa las actualizaciones de estado mediante la pila de argumentos sin usar mutación local.
;; =============================================================================================================================



(defun auditoria (color-anterior duracion_rojo duracion_verde duracion_amarillo cambios) 
    (let* ((color-nuevo (ftimer (obtener-timestamp) duracion_rojo duracion_verde duracion_amarillo ))
            (colores-validos '(rojo verde amarillo)))
        (cond
            ((or 
                (stringp color-anterior) 
                (not (duracion-ciclo duracion_rojo duracion_verde duracion_amarillo)) 
                (not (numberp cambios)) 
                (not (member color-anterior colores-validos))) "Error: No se puede iniciar la auditoria con datos invalidos")

            ((= cambios 0) "Cantidad de transiciones o cambios mostrados")
            ((validar-transiciones-p (convertir-color color-anterior) color-nuevo)
                (format t "~%Tiempo ~a: la luz ha cambiado de ~a a ~a" (obtener-timestamp) color-anterior color-nuevo)
                (sleep 1)
                (auditoria color-nuevo duracion_rojo duracion_verde duracion_amarillo (- cambios 1))
            )
            (t  (sleep 1) 
                (auditoria color-anterior duracion_rojo duracion_verde duracion_amarillo cambios)
            )
        )
    )
)


;; ===========================================           REQUERIMIENTO 4             ===========================================



;; =============================================================================================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura. Evalúa un entero y retorna un string estático predecible sin alterar configuraciones de la red.
;; ESTRATEGIA: Función Condicional. Emplea un condicional múltiple 'cond' para determinar rangos de optimización vial.
;; IMPACTO: No Destructiva. Analiza el parámetro de manera síncrona sin causar mutaciones en las variables de origen.
;; =============================================================================================================================



(defun recomendacion-ciclo (duracion)
  (cond
      ((and (numberp duracion) (< duracion 35)) "Recomendacion: aumentar la duracion del ciclo para mejorar la fluidez")
      ((and (numberp duracion) (> duracion 150)) "Recomendacion: reducir la duracion del ciclo para evitar la frustracion")
      ((numberp duracion) "Recomendacion: la duracion del ciclo es optima, no se requieren cambios.")
      (t "Error: el tipo de ciclo suministrado no es valido.")
  )
)



;; ===========================================           REQUERIMIENTO 5             ===========================================



;; =============================================================================================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura. Ejecuta proyecciones numéricas deterministas aisladas sin accesos a disco o periféricos.
;; ESTRATEGIA: Función Simple. Aplica transformaciones de escala y una división entera por truncamiento con 'truncate'.
;; IMPACTO: No Destructiva. Confina el cálculo seguro a un entorno local inmutable mediante la estructura 'let'.
;; =============================================================================================================================

(defun ciclos-por-tiempo (minutos duracion_rojo duracion_verde duracion_amarillo)

    (if (and (numberp minutos) (duracion-ciclo duracion_rojo duracion_verde duracion_amarillo))
        (let (  (minutos-en-seg (* minutos 60)) )
            (truncate minutos-en-seg (duracion-ciclo duracion_rojo duracion_verde duracion_amarillo))
        )
      "Error: datos no numericos")
)



;; ===========================================           REQUERIMIENTO 6             ===========================================



;; =============================================================================================================================
;; FUNCION: informe-distribucion
;; NATURALEZA: Impura. Genera efectos colaterales visuales imprimiendo un reporte de ingeniería en la consola estándar.
;; ESTRATEGIA: Función Simple. Encadena los cálculos matemáticos porcentuales por medio de un bloque secuencial 'let*'.
;; IMPACTO: No Destructiva. Muestra los datos computados en el flujo de salida sin destruir ni alterar las entradas.
;; =============================================================================================================================



(defun informe-distribucion (duracion_rojo duracion_verde duracion_amarillo)
    (if (and (numberp duracion_rojo) (numberp duracion_verde) (numberp duracion_amarillo))
        (let* (
            ; realizo los calculos para sacar el porcentaje que aparece cada color en una hora

            (total_ciclo (duracion-ciclo duracion_rojo duracion_verde duracion_amarillo))
            (pct-rojo (* (/ duracion_rojo total_ciclo) 100.0))
            (pct-amarillo (* (/ duracion_amarillo total_ciclo) 100.0))
            (pct-verde (* (/ duracion_verde total_ciclo) 100.0)))
            
            ;imprimo los resultados
            (format t "~%INFORME DE DISTRIBUCION TEMPORAL (1 HORA):")
            (format t "~%ROJO: ~,2f%" pct-rojo)
            (format t "~%VERDE: ~,2f%" pct-verde)
            (format t "~%AMARILLO: ~,2f%" pct-amarillo)

        )
        (format t "~%Error: Todas las duraciones de los colores deben ser numeros.") 
    )
)



;; =============================================================================================================================
;;                                         REQUERIMIENTO 7: ASEGURAMIENTO DE LA CALIDAD
;; =============================================================================================================================

;; ==========================================
;; PRUEBAS: REQUERIMIENTO 1 - transicion
;; ==========================================

;; Camino Normal: Transición válida de Rojo a Verde 
(transicion 'en-rojo 'verde)
;; Esperado -> (EN-ROJO "cambiar-a-verde")

;; Camino Alternativo: Transición válida de Amarillo a Rojo
(transicion 'en-amarillo 'rojo)
;; Esperado -> (EN-AMARILLO "cambiar-a-rojo")

;; Caso de Error: Intento de transición inválida 
(transicion 'en-rojo 'amarillo)
;; Esperado -> (EN-ROJO ACCION-POR-DEFECTO)

;; Caso de Error: Parámetros inválidos 
(transicion 'en-azul 'verde)
;; Esperado -> (EN-AZUL ACCION-POR-DEFECTO)


;; ==========================================
;; PRUEBAS: REQUERIMIENTO 2 - ftimer
;; ==========================================

;; Camino Normal: 
(ftimer 45 90 120 6)


;; Camino Alternativo:
(ftimer 100 90 120 6)


;; Caso de Error: Datos no numéricos en el Timestamp (String)
(ftimer "150" 90 120 6)
;; Esperado -> "Error datos no numericos"

;; Caso de Error: Datos no numéricos en las configuraciones de tiempos (Símbolo)
(ftimer 10 'rojo 120 6)
;; Esperado -> "Error datos no numericos"


;; ==========================================
;; PRUEBAS: REQUERIMIENTO 3 - auditoria
;; ==========================================

;; Camino Normal: Inicio de auditoría con color correcto y 2 transiciones solicitadas 
(auditoria 'rojo 1 1 1 2)
;; Esperado -> Imprime los cambios en consola secuencialmente y finaliza con:
;;             "Cantidad de transiciones o cambios mostrados"

;; Caso de Error: Color inicial inválido
(auditoria 'azul 90 120 6 3)
;; Esperado -> "Error: No se puede iniciar la auditoria con datos invalidos"

;; Caso de Error: Argumento de cambios no numérico
(auditoria 'verde 90 120 6 'muchos)
;; Esperado -> "Error: No se puede iniciar la auditoria con datos invalidos"


;; ==========================================
;; PRUEBAS: REQUERIMIENTO 4 - recomendacion-ciclo
;; ==========================================

;; Camino Normal: Duración óptima (Dentro del rango psicológico estándar vial 35-150s)
(recomendacion-ciclo (duracion-ciclo 50 50 20))
;; Esperado -> "Recomendacion: la duracion del ciclo es optima, no se requieren cambios."


;; Camino alternativo (Pasarle directamente el total de la duracion de mi ciclo)
; Duración óptima (Dentro del rango psicológico estándar vial 35-150s)
(recomendacion-ciclo 120)
;; Esperado -> "Recomendacion: la duracion del ciclo es optima, no se requieren cambios."


;; Camino Alternativo (Pasarle directamente el total de la duracion de mi ciclo)
;Ciclo excesivamente corto (Menor a 35 segundos)
(recomendacion-ciclo 30)
;; Esperado -> "Recomendacion: aumentar la duracion del ciclo para mejorar la fluidez"


;; Camino Alternativo (Pasarle directamente el total de la duracion de mi ciclo)
;Ciclo excesivamente largo (Mayor a 150 segundos)
(recomendacion-ciclo 180)
;; Esperado -> "Recomendacion: reducir la duracion del ciclo para evitar la frustracion"


;; Caso de Error: Se suministra un dato no numérico
(recomendacion-ciclo nil)
;; Esperado -> "Error: el tipo de ciclo suministrado no es valido."


;; ==========================================
;; PRUEBAS: REQUERIMIENTO 5 - ciclos-por-tiempo
;; ==========================================

;; Camino Normal: Cálculo de ciclos completos en 10 minutos con tiempos estándar (10 min * 60s = 600s / 60s)
(ciclos-por-tiempo 10 20 20 20)
;; Esperado -> 10

;; Camino Alternativo: Duración exacta que equivale a un número entero de ciclos (3.6 minutos = 216 segundos)
(ciclos-por-tiempo 3.6 90 120 6)
;; Esperado -> 1

;; Caso de Error: Minutos pasados como string
(ciclos-por-tiempo "10" 90 120 6)
;; Esperado -> "Error: datos no numericos"


;; ==========================================
;; PRUEBAS: REQUERIMIENTO 6 - informe-distribucion
;; ==========================================

;; Camino Normal: Generación del informe porcentual con datos estándar
(informe-distribucion 90 120 6)
;; Esperado -> Imprime en consola:
;;             INFORME DE DISTRIBUCION TEMPORAL (1 HORA):
;;             ROJO: 41.67%
;;             VERDE: 55.56%
;;             AMARILLO: 2.78%

;; Caso de Error: datos no numéricos
(informe-distribucion 90 'cien 6)
;; Esperado -> Imprime en consola:
;;             Error: Todas las duraciones de los colores deben ser numeros.



;; =============================================================================================================================
;;                                     PRUEBAS ADICIONALES: FUNCIONES AUXILIARES
;; =============================================================================================================================

;; ==========================================
;; PRUEBAS: AUXILIAR 1 - duracion-ciclo
;; ==========================================

;; Camino Normal: Cálculo correcto con los tiempos clásicos de la Fase 1 (90 + 120 + 6)
(duracion-ciclo 90 120 6)
;; Esperado -> 216

;; Camino Alternativo: Tiempos de ciclo reducidos o de prueba
(duracion-ciclo 10 20 5)
;; Esperado -> 35

;; Caso de Error: Uno de los argumentos es un símbolo (no numérico)
(duracion-ciclo 90 'verde 6)
;; Esperado -> NIL

;; Caso de Error: Se omiten datos pasando NIL
(duracion-ciclo nil 120 6)
;; Esperado -> NIL


;; ==========================================
;; PRUEBAS: AUXILIAR 2 - obtener-timestamp
;; ==========================================

;; Camino Normal: Retorna un número entero grande (segundos transcurridos desde 1978)
(obtener-timestamp)
;; Esperado -> Un entero (Ej: 151381242)


;; ==========================================
;; PRUEBAS: AUXILIAR 3 - convertir-color
;; ==========================================

;; Camino Normal: Mapeo directo del símbolo 'rojo al estado de la máquina 'en-rojo
(convertir-color 'rojo)
;; Esperado -> EN-ROJO

;; Camino Alternativo: Mapeo de otro color elemental ('verde)
(convertir-color 'verde)
;; Esperado -> EN-VERDE

;; Camino Alternativo: Si ya recibe el formato final 'en-amarillo, lo mantiene igual (rama 't' del cond)
(convertir-color 'en-amarillo)
;; Esperado -> EN-AMARILLO



;; ==========================================
;; PRUEBAS: AUXILIAR 4 - validar-transiciones-p
;; ==========================================

;; Camino Normal: Transición vial reglamentaria de Verde a Amarillo
(validar-transiciones-p 'en-verde 'amarillo)
;; Esperado -> T

;; Camino Alternativo: Transición vial reglamentaria de Amarillo a Rojo
(validar-transiciones-p 'en-amarillo 'rojo)
;; Esperado -> T

;; Caso de Error: Transición peligrosa/inválida directa de Verde a Rojo (Debería bloquearse)
(validar-transiciones-p 'en-verde 'rojo)
;; Esperado -> NIL

;; Caso de Error: El color actual no usa el prefijo "en-" esperado por la máquina de estados
(validar-transiciones-p 'rojo 'verde)
;; Esperado -> NIL