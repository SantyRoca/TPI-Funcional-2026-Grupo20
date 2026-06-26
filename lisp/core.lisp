;; Cargo mi libreria local-time
(Load #P"C:/Users/WinterOS/quicklisp/setup.lisp")
(ql:quickload "local-time")

(shadow 'timer); Oculta el temporizador interno de SBCL para evitar conflictos de nombres.

;;; ===========================================           ITERACION 2             ===========================================


;;; =============================================================================================================================
;;;                                                   FUNCIONES AUXILIARES
;;; =============================================================================================================================


;;; =============================================================================================================================
;;; FUNCIÓN: duracion-ciclo
;;; NATURALEZA: Pura (No genera efectos secundarios y ante los mismos argumentos siempre devuelve el mismo resultado)
;;; ESTRATEGIA: Función Condicional (Utiliza una bifurcación 'if' para validar tipos y calcular la ventana de tiempo del ciclo)
;;; IMPACTO: No Destructiva (Efectúa adiciones numéricas elementales agregando los 9 segundos de margen de seguridad vial)
;;; =============================================================================================================================


(defun duracion-ciclo (duracion-rojo duracion-verde duracion-amarillo)
  (if (and (numberp duracion-rojo) (numberp duracion-verde) (numberp duracion-amarillo)) ; Valida que los segundos de los 3 colores sean numericos
      (+ duracion-rojo duracion-verde duracion-amarillo 9) ; realiza la suma para tener el total en un ciclo + 9 que es la suma de los 3 intermitentes
      nil)) ; devuelve nil en caso de que los segundos alguno de los 3 colores no sea numerico


;;; =============================================================================================================================
;;; FUNCIÓN: obtener-timestamp
;;; NATURALEZA: Impura (Interactúa de forma directa con el reloj de hardware del SO, retornando un valor mutable cronológicamente)
;;; ESTRATEGIA: Función Simple (Desplaza linealmente la época base de Lisp aplicando una sustracción aritmética de enteros)
;;; IMPACTO: No Destructiva (Computa y retorna un entero atómico independiente sin alterar estructuras ni punteros en memoria)
;;; =============================================================================================================================


(defun obtener-timestamp ()
  (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970)))


;;; =============================================================================================================================
;;; FUNCIÓN: obtener-timestamp-humano
;;; NATURALEZA: Impura (Efectúa consultas síncronas al estado del tiempo del sistema mediante la biblioteca externa local-time)
;;; ESTRATEGIA: Función de Interoperabilidad (Mapea una estructura temporal atómica a una representación en cadena formateada)
;;; IMPACTO: No Destructiva (Genera un String nuevo en memoria sin producir efectos colaterales de modificación de datos)
;;; =============================================================================================================================


;; me da la fecha y la hora de manera legible para un ser humano
(defun obtener-timestamp-humano ()
  (local-time:format-timestring nil 
    (local-time:timestamp- (local-time:now) 3 :hour) ; Restamos 3 horas
    :format '((:year 4) #\- (:month 2) #\- (:day 2) " " (:hour 2) #\: (:min 2) #\: (:sec 2))))



;;; =============================================================================================================================
;;; FUNCIÓN: convertir-color
;;; NATURALEZA: Pura (Establece una traducción determinista de símbolos aislada de variables globales y de operaciones de E/S)
;;; ESTRATEGIA: Función Condicional (Estructurada mediante un bloque condicional múltiple 'cond' para mapear la red de focos)
;;; IMPACTO: No Destructiva (Retorna referencias simbólicas estáticas sin mutar bajo ningún concepto el argumento de origen)
;;; =============================================================================================================================


;; convierte el color, por ejemplo pasa de rojo a en-rojo
(defun convertir-color (color)
  (cond ((eq color 'rojo) 'en-rojo)
        ((eq color 'rojo-intermitente) 'en-rojo-intermitente)
        ((eq color 'verde) 'en-verde)
        ((eq color 'verde-intermitente) 'en-verde-intermitente)
        ((eq color 'amarillo) 'en-amarillo)
        ((eq color 'amarillo-intermitente) 'en-amarillo-intermitente)
        (t color)))



;;; =============================================================================================================================
;;; FUNCIÓN: validar-transiciones-p
;;; NATURALEZA: Pura (Actúa como predicado lógico determinista analizando exclusivamente los valores de sus argumentos)
;;; ESTRATEGIA: Función Predicado (Utiliza combinadores booleanos de control secuencial 'and' para validar la seguridad vial)
;;; IMPACTO: No Destructiva (Devuelve literales booleanos T o NIL sin realizar reasignaciones ni alterar la memoria)
;;; =============================================================================================================================


(defun validar-transiciones-p (color-actual cambiar-a)
  (cond ((and (eq color-actual 'en-rojo) (eq cambiar-a 'rojo-intermitente)) t)
        ((and (eq color-actual 'en-rojo-intermitente) (eq cambiar-a 'verde)) t)
        ((and (eq color-actual 'en-verde) (eq cambiar-a 'verde-intermitente)) t)
        ((and (eq color-actual 'en-verde-intermitente) (eq cambiar-a 'amarillo)) t)
        ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'amarillo-intermitente)) t)
        ((and (eq color-actual 'en-amarillo-intermitente) (eq cambiar-a 'rojo)) t)
        (t nil)))



;;; =============================================================================================================================
;;;                                               REQUERIMIENTO 1
;;; =============================================================================================================================


;;; =============================================================================================================================
;;; FUNCIÓN: transicion
;;; NATURALEZA: Pura (Modela las transiciones válidas de la máquina de estados abstracta sin interactuar con agentes externos)
;;; ESTRATEGIA: Función Condicional (Evalúa la seguridad de la mutación mediante una llamada al predicado de control vial)
;;; IMPACTO: No Destructiva (Construye una nueva estructura de lista dinámica en memoria mediante el uso de 'list' y 'format')
;;; =============================================================================================================================


(defun transicion (color-actual cambiar-a)
  (if (validar-transiciones-p color-actual cambiar-a) ;valida la transicion
      (list color-actual (format nil "cambiar-a-~a" cambiar-a)) ;caso en que la transicion sea valida 
      (list color-actual 'accion-por-defecto))) ;caso en que la transicion no sea valida



;;; =============================================================================================================================
;;;                                              REQUERIMIENTO 2
;;; =============================================================================================================================

;;; =============================================================================================================================
;;; FUNCIÓN: timer
;;; NATURALEZA: Pura (Procesa proyecciones del estado de luces matemáticamente a partir de parámetros de tiempo inmutables)
;;; ESTRATEGIA: Función Condicional (Utiliza congruencia aritmética mediante 'mod' para segmentar secuencialmente las fases)
;;; IMPACTO: No Destructiva (Instancia un entorno local mediante 'let' que se libera automáticamente al salir del ámbito)
;;; =============================================================================================================================



(defun timer (ts duracion-rojo duracion-verde duracion-amarillo)
    (let ((ciclo (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo))) 
        (cond ((or (not ciclo) (not (numberp ts))) "Error datos no numericos")
            ((< (mod ts ciclo) duracion-rojo) 'rojo) 
            ((< (mod ts ciclo) (+ duracion-rojo 3)) 'rojo-intermitente) 
            ((< (mod ts ciclo) (+ duracion-rojo duracion-verde 3)) 'verde)
            ((< (mod ts ciclo) (+ duracion-rojo duracion-verde 6)) 'verde-intermitente)
            ((< (mod ts ciclo) (+ duracion-rojo duracion-verde duracion-amarillo 6)) 'amarillo)
            (t 'amarillo-intermitente))))


;;; =============================================================================================================================
;;;                                               REQUERIMIENTO 3
;;; =============================================================================================================================

;;; =============================================================================================================================
;;; FUNCIÓN: auditoria
;;; NATURALEZA: Impura (Provoca efectos secundarios secuenciales en la salida estándar e introduce retardos mediante 'sleep')
;;; ESTRATEGIA: Función Recursiva (Genera un flujo acumulativo no destructivo evaluando transiciones viales cronológicas)
;;; IMPACTO: No Destructiva (Preserva la inmutabilidad de datos usando el constructor 'cons' para construir la pila de registros)
;;; =============================================================================================================================



(defun auditoria (color-anterior duracion-rojo duracion-verde duracion-amarillo cambios)
  (let ((color-nuevo (timer (obtener-timestamp) duracion-rojo duracion-verde duracion-amarillo))
        (colores-validos '(rojo rojo-intermitente verde verde-intermitente amarillo amarillo-intermitente)))
    (cond ((or (stringp color-anterior)
               (not (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)) 
               (not (numberp cambios)) 
               (not (member color-anterior colores-validos))) 
           "Error: No se puede iniciar la auditoria con datos invalidos")
          ((= cambios 0) nil) ;finalizacion de mi funcion recursiva
          ((validar-transiciones-p (convertir-color color-anterior) color-nuevo) ;Caso de encontrar una transicion
           (sleep 1)
           (cons (list (format nil "[~a]" (obtener-timestamp-humano)) color-anterior color-nuevo) 
                 (auditoria color-nuevo duracion-rojo duracion-verde duracion-amarillo (- cambios 1))))
          (t (sleep 1) ; caso para volver a llamarse
             (auditoria color-anterior duracion-rojo duracion-verde duracion-amarillo cambios)))))



;;; =============================================================================================================================
;;;                                               REQUERIMIENTO 4
;;; =============================================================================================================================

;;; =============================================================================================================================
;;; FUNCIÓN: recomendacion-ciclo
;;; NATURALEZA: Pura (Retorna cadenas descriptivas predecibles a partir de una evaluación numérica estática y síncrona)
;;; ESTRATEGIA: Función Condicional (Clasifica rangos métricos de ingeniería de tráfico mediante un bloque de evaluación 'cond')
;;; IMPACTO: No Destructiva (No produce mutación de variables ni altera los parámetros provistos por el módulo invocador)
;;; =============================================================================================================================


(defun recomendacion-ciclo (duracion)
  (cond ((and (numberp duracion) (< duracion 35)) "Recomendacion: aumentar la duracion del ciclo para mejorar la fluidez") ; caso de que la duracion de un ciclo sea menor a 35 segundos
        ((and (numberp duracion) (> duracion 150)) "Recomendacion: reducir la duracion del ciclo para evitar la frustracion") ; caso de que la duracion de un ciclo sea mayor a 150 segundos
        ((numberp duracion) "Recomendacion: la duracion del ciclo es optima, no se requieren cambios.") ; caso optimo donde la duracion del ciclo esta entre los 35 y 150 segundos
        (t "Error: el tipo de ciclo suministrado no es valido."))) ; caso de que la duracion del ciclo no sea numerico


;;; =============================================================================================================================
;;;                                              REQUERIMIENTO 5
;;; =============================================================================================================================

;;; =============================================================================================================================
;;; FUNCIÓN: ciclos-por-tiempo
;;; NATURALEZA: Pura (Realiza estimaciones matemáticas abstractas sin accesos a E/S, almacenamiento local o periféricos)
;;; ESTRATEGIA: Función Simple (Aplica transformaciones de escala temporal seguidas de una división entera mediante 'truncate')
;;; IMPACTO: No Destructiva (Encapsula de forma segura los cálculos en un contexto léxico inmutable estructurado con 'let')
;;; =============================================================================================================================


(defun ciclos-por-tiempo (minutos duracion-rojo duracion-verde duracion-amarillo)
  (if (and (numberp minutos) (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo))
      (let ((minutos-en-seg (* minutos 60)))
        (truncate minutos-en-seg (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo)))
      "Error: datos no numericos"))


;;; =============================================================================================================================
;;;                                               REQUERIMIENTO 6
;;; =============================================================================================================================

;;; =============================================================================================================================
;;; FUNCIÓN: informe-distribucion
;;; NATURALEZA: Impura (Genera efectos colaterales de salida visual escribiendo el reporte en la consola estándar del REPL)
;;; ESTRATEGIA: Función Simple (Calcula y encadena ratios porcentuales de tiempos viales mediante el constructor léxico 'let*')
;;; IMPACTO: No Destructiva (Transmite los datos procesados al flujo de salida sin corromper ni reasignar las variables base)
;;; =============================================================================================================================


(defun informe-distribucion (duracion-rojo duracion-verde duracion-amarillo)
  (if (and (numberp duracion-rojo) (numberp duracion-verde) (numberp duracion-amarillo))
      (let* ((total-ciclo (duracion-ciclo duracion-rojo duracion-verde duracion-amarillo))
             (pct-rojo (* (/ duracion-rojo total-ciclo) 100.0))
             (pct-amarillo (* (/ duracion-amarillo total-ciclo) 100.0))
             (pct-verde (* (/ duracion-verde total-ciclo) 100.0))
             (pct-transiciones (* (/ 9 total-ciclo) 100.0)))
        (format t "~%INFORME DE DISTRIBUCION TEMPORAL (1 HORA):")
        (format t "~%ROJO: ~,2f%" pct-rojo)
        (format t "~%VERDE: ~,2f%" pct-verde)
        (format t "~%AMARILLO: ~,2f%" pct-amarillo)
        (format t "~%TRANSICIONES: ~,2f%" pct-transiciones))
      (format t "~%Error: Todos los argumentos deben ser numeros.")))


;;; =============================================================================================================================
;;;                                       EXTENSION 2: (INFORME)
;;; =============================================================================================================================

;;; =============================================================================================================================
;;; FUNCIÓN: escribir-lineas-informe
;;; NATURALEZA: Impura (Provoca efectos secundarios en el sistema de archivos al escribir líneas de texto mediante un Stream)
;;; ESTRATEGIA: Función Recursiva de Cola (Itera sobre los nodos de la lista procesando el 'car' y delegando el 'cdr' en posición final)
;;; IMPACTO: No Destructiva (Navega los registros de auditoría de forma lineal sin emplear bucles imperativos destructivos)
;;; =============================================================================================================================

;; FUNCIÓN AUXILIAR

(defun escribir-lineas-informe (lista-datos stream)
  (cond ((null lista-datos) t)
        (t (let* ((registro-actual (car lista-datos))
                 (timestamp (car registro-actual))   ; le paso la fecha y hora
                 (anterior (cadr registro-actual))  ; le paso el color anterior   
                 (nuevo (caddr registro-actual))) ; le paso el color nuevo 
              (format stream "~a - Transición: ~a -> ~a~%" timestamp anterior nuevo)
              (escribir-lineas-informe (cdr lista-datos) stream)))))


;;; =============================================================================================================================
;;; FUNCIÓN: informe
;;; NATURALEZA: Impura (Establece comunicación directa con el disco duro externo abriendo, mutando y sobreescribiendo un archivo)
;;; ESTRATEGIA: Función Ensambladora (Utiliza la macro funcional 'with-open-file' para asegurar un flujo y cierre de stream seguro)
;;; IMPACTO: No Destructiva (Lee la lista de logs históricos de manera pasiva sin alterar el estado de sus sublistas internas)
;;; =============================================================================================================================


(defun informe (datos)
  (if (null datos)
      "Error: No hay datos para generar el informe."
      (with-open-file (stream "C:/Segundo anio/integrador/informe-ejecucion-semaforo.txt" 
                              :direction :output ; Configura el archivo en modo escritura
                              :if-exists :supersede ; Si el archivo ya existe lo sobrescribe todo
                              :if-does-not-exist :create) ; Si el archivo no existe lo crea automaticamente
        (format stream "Informe de Ejecución del Sistema Semafórico~%")
        (format stream "=========================================~%")
        (escribir-lineas-informe datos stream)
        (format stream "~%--- Fin del Informe ---")
        "Informe generado con éxito en 'C:/Segundo anio/integrador/informe-ejecucion-semaforo.txt'")))

































;;; =============================================================================================================================
;;;                                           REQUERIMIENTO 7: ASEGURAMIENTO DE LA CALIDAD
;;; =============================================================================================================================

;;; ==========================================
;;; PRUEBAS: REQUERIMIENTO 1 - transicion
;;; ==========================================

;; Camino Normal:
(transicion 'en-rojo 'rojo-intermitente)
;; Esperado -> (EN-ROJO "cambiar-a-rojo-intermitente")


;; Camino Alternativo: 
(transicion 'en-verde-intermitente 'amarillo)
;; Esperado -> (EN-VERDE-INTERMITENTE "cambiar-a-amarillo")


;; Caso de Error: 
(transicion 'en-rojo 'verde)
;; Esperado -> (EN-ROJO ACCION-POR-DEFECTO)



;;; ==========================================
;;; PRUEBAS: REQUERIMIENTO 2 - timer
;;; ==========================================


;; Camino Normal:
(timer (obtener-timestamp) 30 20 5)


;; Caso de Error:
(timer 'times 30 20 5)
;; Esperado -> "Error datos no numericos"


;; Caso de Error: 
(timer 10 30 'veinte 5)
;; Esperado -> "Error datos no numericos"


;;; ==========================================
;;; PRUEBAS: REQUERIMIENTO 3 - auditoria
;;; ==========================================


;; Camino Normal: 
(auditoria 'rojo 30 20 5 3)
;; Esperado ->   (("[fecha-actual hora-actual]" ROJO ROJO-INTERMITENTE) ("[fecha-actual hora-actual]" ROJO-INTERMITENTE VERDE) ...)

;; Camino alternativo: 
(auditoria (timer (obtener-timestamp) 30 20 5) 30 20 5 3)
;; Esperado -> "Error: No se puede iniciar la auditoria con datos invalidos"

;; Caso de Error: 
(auditoria 'verde 30 20 5 'tres)
;; Esperado -> "Error: No se puede iniciar la auditoria con datos invalidos"


;;; ==========================================
;;; PRUEBAS: REQUERIMIENTO 4 - recomendacion-ciclo
;;; ==========================================

;; Camino Normal:
(recomendacion-ciclo (duracion-ciclo 30 20 5))
;; Esperado -> "Recomendacion: la duracion del ciclo es optima, no se requieren cambios."

;; Camino Alternativo: 
(recomendacion-ciclo 90)
;; Esperado -> "Recomendacion: la duracion del ciclo es optima, no se requieren cambios."

;; Camino Alternativo: 
(recomendacion-ciclo 25)
;; Esperado -> "Recomendacion: aumentar la duracion del ciclo para mejorar la fluidez"

;; Camino Alternativo: 
(recomendacion-ciclo 180)
;; Esperado -> "Recomendacion: reducir la duracion del ciclo para evitar la frustracion"

;; Caso de Error: 
(recomendacion-ciclo 'noventa)
;; Esperado -> "Error: el tipo de ciclo suministrado no es valido."


;;; ==========================================
;;; PRUEBAS: REQUERIMIENTO 5 - ciclos-por-tiempo
;;; ==========================================

;; Camino Normal:
(ciclos-por-tiempo 60 30 20 5)
;; Esperado -> 56

;; Camino Alternativo: 
(ciclos-por-tiempo 1 30 20 1)
;; Esperado -> 1

;; Caso de Error: 
(ciclos-por-tiempo "60" 30 20 5)
;; Esperado -> "Error: datos no numericos"


;;; ==========================================
;;; PRUEBAS: REQUERIMIENTO 6 - informe-distribucion
;;; ==========================================

;; Camino Normal:
(informe-distribucion 30 20 5)
;; Esperado -> Imprime en consola:
;;             INFORME DE DISTRIBUCION TEMPORAL (1 HORA):
;;             ROJO: 46.88%
;;             VERDE: 31.25%
;;             AMARILLO: 7.81%
;;             TRANSICIONES: 14.06%


;; Caso de Error: Uno de los parámetros de color no es numérico

(informe-distribucion 30 'veinte 5)
;; Esperado -> Error: Todos los argumentos deben ser numeros.


;;; ==========================================
;;; EXTENSION 2: PERSISTENCIA - informe
;;; ==========================================

;; Camino Normal: 
(informe '(("[2026-06-14 22:55:10]" ROJO ROJO-INTERMITENTE) ("[2026-06-14 22:55:11]" ROJO-INTERMITENTE VERDE)))
;; Esperado -> "Informe generado con éxito en 'C:/Segundo anio/integrador/informe-ejecucion-semaforo.txt'"


;; Camino Normal:
(informe (auditoria 'rojo 1 1 1 10))
;; Esperado -> "Informe generado con éxito en 'C:/Segundo anio/integrador/informe-ejecucion-semaforo.txt'"


;; Caso de Error: 
(informe nil)
;; Esperado -> "Error: No hay datos para generar el informe."




;;; =============================================================================================================================
;;;                                            PRUEBAS ADICIONALES: FUNCIONES AUXILIARES
;;; =============================================================================================================================

;;; ==========================================
;;; PRUEBAS: AUXILIAR 1 - duracion-ciclo
;;; ==========================================

;; Camino Normal:
(duracion-ciclo 30 20 5)
;; Esperado -> 64

;; Caso de Error:
(duracion-ciclo 30 'veinte 5)
;; Esperado -> NIL


;;; ==========================================
;;; PRUEBAS: AUXILIAR 2 - obtener-timestamp
;;; ==========================================

;; Camino Normal:
(obtener-timestamp)
;; Esperado -> Un entero largo (Ej: 1781477710)


;;; ==========================================
;;; PRUEBAS: AUXILIAR 3 - obtener-timestamp-humano
;;; ==========================================

;; Camino Normal:
(obtener-timestamp-humano)
;; Esperado ->(Ej: "2026-06-14 22:55:10")


;;; ==========================================
;;; PRUEBAS: AUXILIAR 4 - convertir-color
;;; ==========================================

;; Camino Normal:
(convertir-color 'rojo-intermitente)
;; Esperado -> EN-ROJO-INTERMITENTE

;; Camino Alternativo:
(convertir-color 'en-verde)
;; Esperado -> EN-VERDE


;;; ==========================================
;;; PRUEBAS: AUXILIAR 5 - validar-transiciones-p
;;; ==========================================

;; Camino Normal:
(validar-transiciones-p 'en-verde 'verde-intermitente)
;; Esperado -> T

;; Caso de Error:
(validar-transiciones-p 'en-rojo 'verde)
;; Esperado -> NIL