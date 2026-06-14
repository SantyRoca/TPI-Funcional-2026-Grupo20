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
    (- (get-universal-time) (encode-universal-time 0 0 0 25 6 1978)) ;; fecha del primer mundial de argentina ganado
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


