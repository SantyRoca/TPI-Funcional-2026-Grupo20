;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (No cambia nada afuera; si le pasas los mismos colores, siempre te devuelve el mismo resultado)
;; ESTRATEGIA: Función Condicional (Elige que hacer usando una condicion logica de tipo 'if')
;; IMPACTO: No destructiva (No borra ni altera datos, crea una lista nueva para dar la respuesta)
;; ========================================================
(defun transicion (color-actual cambiar-a)
    (if (validar-transiciones-p color-actual cambiar-a)
      (list color-actual (format nil "cambiar-a-~a" cambiar-a))
      (list color-actual 'accion-por-defecto)
    )
)

;; ========================================================
;; FUNCIÓN: validar-transiciones-p
;; NATURALEZA: Pura (Solo analiza los colores que recibe sin tocar nada de la memoria del sistema)
;; ESTRATEGIA: Función Predicado (Una funcion de control que solo responde T o NIL)
;; IMPACTO: No destructiva (Determina el resultado mediante operadores booleanos sin alterar estructuras)
;; ========================================================

(defun validar-transiciones-p (color-actual cambiar-a)
    (cond
        ((and (equal color-actual 'en-rojo) (equal cambiar-a 'verde)) t)
        ((and (equal color-actual 'en-verde) (equal cambiar-a 'amarillo)) t)
        ((and (equal color-actual 'en-amarillo) (equal cambiar-a 'rojo)) t)
        (t nil)
    )
)

;; ========================================================
;; FUNCIÓN: obtener-timestamp
;; NATURALEZA: Impura (Pregunta la hora a la computadora; como el tiempo corre, cada vez que la llamas da un numero distinto)
;; ESTRATEGIA: Función simple (Hace cuentas matematicas basicas)
;; IMPACTO: No destructiva (Hace calculos con numeros sueltos sin romper ni modificar variables del programa)
;; ========================================================

(defun obtener-timestamp ()
	(- (get-universal-time)
		;segundos minutos horas fecha mes anio
		(encode-universal-time 0 0 0 1 1 1970)
	)
)

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (recibe un timestamp y devuelve el color del semaforo correspondiente a ese tiempo)
;; ESTRATEGIA: Función Condicional (Usa un 'cond' para revisar en que tramo de segundos cae el tiempo y decidir el color)
;; IMPACTO: No destructiva (Crea una variable temporal con 'let' que desaparece apenas la función termina su trabajo)
;; ========================================================

(defun timer (ts)
	(let ((tiempo (mod ts 216) ))
		(cond
			((<= tiempo 90 ) 'rojo)
			((<= tiempo 96) 'amarillo)
			(t 'verde)
		)
	)
)

;; ========================================================
;; FUNCIÓN: auditoria
;; NATURALEZA: Impura (realiza una auditoria del sistema, registrando los cambios de estado del semaforo cada 6 segundos)
;; ESTRATEGIA: Función recursiva de cola 
;; IMPACTO: No destructiva (No modifica ninguna estructura de datos en la memoria del programa)
;; ========================================================

(defun auditoria (color-anterior)
    (sleep 6) ; tiempo maximo entre cambios de estado, para asegurar que se registren cambios significativos
    (format t "~%testeando")
    (let ((color-nuevo (timer (obtener-timestamp))))
        (cond
            ((not (equal color-anterior color-nuevo))
                (format t "~%Tiempo ~a: la luz ha cambiado de ~a a ~a" (obtener-timestamp) color-anterior color-nuevo)
                (auditoria color-nuevo)
            )
            (t (auditoria color-anterior))
        )
    )
)