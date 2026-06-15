(Load #P"C:/Users/WinterOS/quicklisp/setup.lisp")

(ql:quickload "local-time")


;; ===========================================           ITERACION 2             ===========================================


;; =============================================================================================================================
;;                                               FUNCIONES AUXILIARES
;; =============================================================================================================================


;; =============================================================================================================================
;; FUNCIÓN: duracion-ciclo-IT2
;; NATURALEZA: Pura (No genera efectos secundarios y ante los mismos argumentos siempre devuelve el mismo resultado)
;; ESTRATEGIA: Función Condicional (Utiliza una bifurcación 'if' para validar tipos y calcular la ventana de tiempo del ciclo)
;; IMPACTO: No Destructiva (Efectúa adiciones numéricas elementales agregando los 9 segundos de margen de seguridad vial)
;; =============================================================================================================================


(defun duracion-ciclo-IT2 (duracion_rojo duracion_verde duracion_amarillo)
  (if (and (numberp duracion_rojo) (numberp duracion_verde) (numberp duracion_amarillo)) ; Valida que los segundos de los 3 colores sean numericos
      (+ duracion_rojo duracion_verde duracion_amarillo 9)                               ; realiza la suma para tener el total en un ciclo + 9 que es la suma de los 3 intermitentes
      nil)                                                                               ; devuelve nil en caso de que los segundos alguno de los 3 colores no sea numerico
)


;; =============================================================================================================================
;; FUNCIÓN: obtener-timestamp
;; NATURALEZA: Impura (Interactúa de forma directa con el reloj de hardware del SO, retornando un valor mutable cronológicamente)
;; ESTRATEGIA: Función Simple (Desplaza linealmente la época base de Lisp aplicando una sustracción aritmética de enteros)
;; IMPACTO: No Destructiva (Computa y retorna un entero atómico independiente sin alterar estructuras ni punteros en memoria)
;; =============================================================================================================================


(defun obtener-timestamp ()
    (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970))
)




;; =============================================================================================================================
;; FUNCIÓN: obtener-timestamp-humano
;; NATURALEZA: Impura (Efectúa consultas síncronas al estado del tiempo del sistema mediante la biblioteca externa local-time)
;; ESTRATEGIA: Función de Interoperabilidad (Mapea una estructura temporal atómica a una representación en cadena formateada)
;; IMPACTO: No Destructiva (Genera un String nuevo en memoria sin producir efectos colaterales de modificación de datos)
;; =============================================================================================================================

(defun obtener-timestamp-humano ()
  ;Devuelve la fecha y hora actual en el uso horario local en un formato legible string: YYYY-MM-DD HH:MM:SS
  (local-time:format-timestring nil 
    (local-time:timestamp- (local-time:now) 3 :hour) ;; <-- Restamos 3 horas puramente al vuelo
    :format '((:year 4) #\- (:month 2) #\- (:day 2) " " (:hour 2) #\: (:min 2) #\: (:sec 2)))
)



;; =============================================================================================================================
;; FUNCIÓN: convertir-color-IT2
;; NATURALEZA: Pura (Establece una traducción determinista de símbolos aislada de variables globales y de operaciones de E/S)
;; ESTRATEGIA: Función Condicional (Estructurada mediante un bloque condicional múltiple 'cond' para mapear la red de focos)
;; IMPACTO: No Destructiva (Retorna referencias simbólicas estáticas sin mutar bajo ningún concepto el argumento de origen)
;; =============================================================================================================================


(defun convertir-color-IT2 (color)
    (cond
        ((equal color 'rojo) 'en-rojo)
        ((equal color 'rojo-intermitente) 'en-rojo-intermitente)

        ((equal color 'verde) 'en-verde)
        ((equal color 'verde-intermitente) 'en-verde-intermitente)

        ((equal color 'amarillo) 'en-amarillo)
        ((equal color 'amarillo-intermitente) 'en-amarillo-intermitente)

        (t color)
    )
)


;; =============================================================================================================================
;; FUNCIÓN: validar-transiciones-p-IT2
;; NATURALEZA: Pura (Actúa como predicado lógico determinista analizando exclusivamente los valores de sus argumentos)
;; ESTRATEGIA: Función Predicado (Utiliza combinadores booleanos de control secuencial 'and' para validar la seguridad vial)
;; IMPACTO: No Destructiva (Devuelve literales booleanos T o NIL sin realizar reasignaciones ni alterar la memoria)
;; =============================================================================================================================


(defun validar-transiciones-p-IT2 (color-actual cambiar-a)
    (cond
        ((and (equal color-actual 'en-rojo) (equal cambiar-a 'rojo-intermitente))           t)
        ((and (equal color-actual 'en-rojo-intermitente) (equal cambiar-a 'verde))          t)

        ((and (equal color-actual 'en-verde) (equal cambiar-a 'verde-intermitente))         t)
        ((and (equal color-actual 'en-verde-intermitente) (equal cambiar-a 'amarillo))      t)

        ((and (equal color-actual 'en-amarillo) (equal cambiar-a 'amarillo-intermitente))   t)
        ((and (equal color-actual 'en-amarillo-intermitente) (equal cambiar-a 'rojo))       t)

        (t nil)
    )
)



;; =============================================================================================================================
;;                                           REQUERIMIENTO 1 - IT2
;; =============================================================================================================================


;; =============================================================================================================================
;; FUNCIÓN: transicion-IT2
;; NATURALEZA: Pura (Modela las transiciones válidas de la máquina de estados abstracta sin interactuar con agentes externos)
;; ESTRATEGIA: Función Condicional (Evalúa la seguridad de la mutación mediante una llamada al predicado de control vial)
;; IMPACTO: No Destructiva (Construye una nueva estructura de lista dinámica en memoria mediante el uso de 'list' y 'format')
;; =============================================================================================================================


(defun transicion-IT2 (color-actual cambiar-a)
    (if (validar-transiciones-p-IT2 color-actual cambiar-a)
      (list color-actual (format nil "cambiar-a-~a" cambiar-a))
      (list color-actual 'accion-por-defecto)
    )
)




;; =============================================================================================================================
;;                                           REQUERIMIENTO 2 - IT2
;; =============================================================================================================================

;; =============================================================================================================================
;; FUNCIÓN: timer-IT2
;; NATURALEZA: Pura (Procesa proyecciones del estado de luces matemáticamente a partir de parámetros de tiempo inmutables)
;; ESTRATEGIA: Función Condicional (Utiliza congruencia aritmética mediante 'mod' para segmentar secuencialmente las fases)
;; IMPACTO: No Destructiva (Instancia un entorno local mediante 'let' que se libera automáticamente al salir del ámbito)
;; =============================================================================================================================


(defun timer-IT2 (ts duracion_rojo duracion_verde duracion_amarillo)
    (let ((ciclo (duracion-ciclo-IT2 duracion_rojo duracion_verde duracion_amarillo) )) 
        (cond
            ((or (not ciclo) (not (numberp ts))) "Error datos no numericos")
            ((< (mod ts ciclo) duracion_rojo)                                           'rojo) 
            ((< (mod ts ciclo) (+ duracion_rojo 3))                                     'rojo-intermitente) 
            ((< (mod ts ciclo) (+ duracion_rojo duracion_verde 3) )                     'verde)
            ((< (mod ts ciclo) (+ duracion_rojo duracion_verde 6) )                     'verde-intermitente)
            ((< (mod ts ciclo) (+ duracion_rojo duracion_verde duracion_amarillo 6) )   'amarillo)
            ( t                                                                         'amarillo-intermitente)
        )
    )
)



;; =============================================================================================================================
;;                                           REQUERIMIENTO 3 - IT2
;; =============================================================================================================================

;; =============================================================================================================================
;; FUNCIÓN: auditoria-IT2
;; NATURALEZA: Impura (Provoca efectos secundarios secuenciales en la salida estándar e introduce retardos mediante 'sleep')
;; ESTRATEGIA: Función Recursiva (Genera un flujo acumulativo no destructivo evaluando transiciones viales cronológicas)
;; IMPACTO: No Destructiva (Preserva la inmutabilidad de datos usando el constructor 'cons' para construir la pila de registros)
;; =============================================================================================================================



(defun auditoria-IT2 (color-anterior duracion_rojo duracion_verde duracion_amarillo cambios)

    (let* ((ts-humano (obtener-timestamp-humano))
           (color-nuevo (timer-IT2 (obtener-timestamp) duracion_rojo duracion_verde duracion_amarillo))
           (colores-validos '(rojo rojo-intermitente verde verde-intermitente amarillo amarillo-intermitente)))
        (cond
            ((or
                (stringp color-anterior)
                (not (duracion-ciclo-IT2 duracion_rojo duracion_verde duracion_amarillo)) 
                (not (numberp cambios)) 
                (not (member color-anterior colores-validos))) "Error: No se puede iniciar la auditoria con datos invalidos")

            ((= cambios 0) nil) ;finalizacion de mi funcion recursiva

            ((validar-transiciones-p-IT2 (convertir-color-IT2 color-anterior) color-nuevo) ;Caso de encontrar una transicion
                (sleep 1)
                (cons (list (format nil "[~a]" ts-humano) color-anterior color-nuevo) 
                    (auditoria-IT2 color-nuevo duracion_rojo duracion_verde duracion_amarillo (- cambios 1)))
            )
            (t  (sleep 1) ; caso para volver a llamarse
                (auditoria-IT2 color-anterior duracion_rojo duracion_verde duracion_amarillo cambios)) 
        )
    )
)



;; =============================================================================================================================
;;                                           REQUERIMIENTO 4 - IT2
;; =============================================================================================================================

;; =============================================================================================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura (Retorna cadenas descriptivas predecibles a partir de una evaluación numérica estática y síncrona)
;; ESTRATEGIA: Función Condicional (Clasifica rangos métricos de ingeniería de tráfico mediante un bloque de evaluación 'cond')
;; IMPACTO: No Destructiva (No produce mutación de variables ni altera los parámetros provistos por el módulo invocador)
;; =============================================================================================================================


(defun recomendacion-ciclo (duracion)
  (cond
      ((and (numberp duracion) (< duracion 35)) "Recomendacion: aumentar la duracion del ciclo para mejorar la fluidez")
      ((and (numberp duracion) (> duracion 150)) "Recomendacion: reducir la duracion del ciclo para evitar la frustracion")
      ((numberp duracion) "Recomendacion: la duracion del ciclo es optima, no se requieren cambios.")
      (t "Error: el tipo de ciclo suministrado no es valido.")
  )
)



;; =============================================================================================================================
;;                                           REQUERIMIENTO 5 - IT2
;; =============================================================================================================================

;; =============================================================================================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura (Realiza estimaciones matemáticas abstractas sin accesos a E/S, almacenamiento local o periféricos)
;; ESTRATEGIA: Función Simple (Aplica transformaciones de escala temporal seguidas de una división entera mediante 'truncate')
;; IMPACTO: No Destructiva (Encapsula de forma segura los cálculos en un contexto léxico inmutable estructurado con 'let')
;; =============================================================================================================================


(defun ciclos-por-tiempo (minutos duracion_rojo duracion_verde duracion_amarillo)

    (if (and (numberp minutos) (duracion-ciclo-IT2 duracion_rojo duracion_verde duracion_amarillo))
        (let ( (minutos-en-seg (* minutos 60)) )
            (truncate minutos-en-seg (duracion-ciclo-IT2 duracion_rojo duracion_verde duracion_amarillo))
        )
      "Error: datos no numericos")
)


;; =============================================================================================================================
;;                                           REQUERIMIENTO 6 - IT2
;; =============================================================================================================================

;; =============================================================================================================================
;; FUNCIÓN: informe-distribucion-IT2
;; NATURALEZA: Impura (Genera efectos colaterales de salida visual escribiendo el reporte en la consola estándar del REPL)
;; ESTRATEGIA: Función Simple (Calcula y encadena ratios porcentuales de tiempos viales mediante el constructor léxico 'let*')
;; IMPACTO: No Destructiva (Transmite los datos procesados al flujo de salida sin corromper ni reasignar las variables base)
;; =============================================================================================================================


(defun informe-distribucion-IT2 (duracion_rojo duracion_verde duracion_amarillo)
    (if (and (numberp duracion_rojo) (numberp duracion_verde) (numberp duracion_amarillo))
        (let* (
               ; realizo los calculos para sacar el porcentaje que aparece cada color en una hora
                (total_ciclo (duracion-ciclo-IT2 duracion_rojo duracion_verde duracion_amarillo))
                (pct-rojo (* (/ duracion_rojo total_ciclo) 100.0))
                (pct-amarillo (* (/ duracion_amarillo total_ciclo) 100.0))
                (pct-verde (* (/ duracion_verde total_ciclo) 100.0))
                (pct-transiciones(* (/ 9 total_ciclo) 100.0)))

            ;imprimo los resultados
            (format t "~%INFORME DE DISTRIBUCION TEMPORAL (1 HORA):")
            (format t "~%ROJO: ~,2f%" pct-rojo)
            (format t "~%VERDE: ~,2f%" pct-verde)
            (format t "~%AMARILLO: ~,2f%" pct-amarillo)
            (format t "~%TRANSICIONES: ~,2f%" pct-transiciones)

        )
        (format t "~%Error: Todos los argumentos deben ser numeros.") 
    )
)

;; =============================================================================================================================
;;                                       EXTENSION 2: PERSISTENCIA DE DATOS (INFORME)
;; =============================================================================================================================

;; =============================================================================================================================
;; FUNCIÓN: escribir-lineas-informe-IT2
;; NATURALEZA: Impura (Provoca efectos secundarios en el sistema de archivos al escribir líneas de texto mediante un Stream)
;; ESTRATEGIA: Función Recursiva de Cola (Itera sobre los nodos de la lista procesando el 'car' y delegando el 'cdr' en posición final)
;; IMPACTO: No Destructiva (Navega los registros de auditoría de forma lineal sin emplear bucles imperativos destructivos)
;; =============================================================================================================================

;; FUNCIÓN AUXILIAR RECURSIVA
(defun escribir-lineas-informe-IT2 (lista-datos stream)
  (cond 

    ((null lista-datos) t)
    

    (t 
      (let* ((registro-actual (car lista-datos))
             (timestamp (first registro-actual)) ; le paso la fecha y hora
             (anterior  (second registro-actual)) ; le paso el color anterior
             (nuevo     (third registro-actual))) ; le paso el color nuevo 
        

        (format stream "~a - Transición: ~a -> ~a~%" timestamp anterior nuevo)
        

        (escribir-lineas-informe-IT2 (cdr lista-datos) stream)
      )
    )
  )
)


;; =============================================================================================================================
;; FUNCIÓN: informe-IT2
;; NATURALEZA: Impura (Establece comunicación directa con el disco duro externo abriendo, mutando y sobreescribiendo un archivo)
;; ESTRATEGIA: Función Ensambladora (Utiliza la macro funcional 'with-open-file' para asegurar un flujo y cierre de stream seguro)
;; IMPACTO: No Destructiva (Lee la lista de logs históricos de manera pasiva sin alterar el estado de sus sublistas internas)
;; =============================================================================================================================


(defun informe-IT2 (datos)
  (if (null datos)
      "Error: No hay datos para generar el informe."

      (with-open-file (stream "C:/Segundo anio/integrador/informe-ejecucion-semaforo.txt" 
                              :direction :output 
                              :if-exists :supersede 
                              :if-does-not-exist :create)
        
        (format stream "Informe de Ejecución del Sistema Semafórico~%")
        (format stream "=========================================~%")
        
        (escribir-lineas-informe-IT2 datos stream)
        
        (format stream "~%--- Fin del Informe ---")
        "Informe generado con éxito en 'C:/Segundo anio/integrador/informe-ejecucion-semaforo.txt'"
      )
  )
)



































;; =============================================================================================================================
;;                                           REQUERIMIENTO 7: ASEGURAMIENTO DE LA CALIDAD
;; =============================================================================================================================

;; ==========================================
;; PRUEBAS: REQUERIMIENTO 1 - transicion-IT2
;; ==========================================

;; Camino Normal: Transición válida de Rojo a su siguiente fase de intermitencia
(transicion-IT2 'en-rojo 'rojo-intermitente)
;; Esperado -> (EN-ROJO "cambiar-a-rojo-intermitente")


;; Camino Alternativo: Transición válida de Verde Intermitente a Amarillo
(transicion-IT2 'en-verde-intermitente 'amarillo)
;; Esperado -> (EN-VERDE-INTERMITENTE "cambiar-a-amarillo")


;; Caso de Error: Intento de transición inválida directa (saltearse la intermitencia)
(transicion-IT2 'en-rojo 'verde)
;; Esperado -> (EN-ROJO ACCION-POR-DEFECTO)



;; ==========================================
;; PRUEBAS: REQUERIMIENTO 2 - timer-IT2
;; ==========================================


;; Camino Normal:
(timer-IT2 (obtener-timestamp) 30 20 5)


;; Caso de Error: Datos alfanuméricos en el Timestamp actual
(timer-IT2 'times 30 20 5)
;; Esperado -> "Error datos no numericos"


;; Caso de Error: Configuraciones de tiempos pasadas como símbolos
(timer-IT2 10 30 'veinte 5)
;; Esperado -> "Error datos no numericos"


;; ==========================================
;; PRUEBAS: REQUERIMIENTO 3 - auditoria-IT2
;; ==========================================


;; Camino Normal: Inicio de auditoría recursiva con color base válido capturando 3 transiciones del reloj
(auditoria-IT2 'rojo 30 20 5 3)
;; Esperado ->   (("[fecha-actual hora-actual]" ROJO ROJO-INTERMITENTE) ("[fecha-actual hora-actual]" ROJO-INTERMITENTE VERDE) ...)

;; Camino alternativo: utilizar la funcion timer para pasar el color que esta prendido ahora mismo
(auditoria-IT2 (timer-IT2 (obtener-timestamp) 30 20 5) 30 20 5 3)
;; Esperado -> "Error: No se puede iniciar la auditoria con datos invalidos"

;; Caso de Error: El argumento de cantidad de cambios no es un valor numérico entero
(auditoria-IT2 'verde 30 20 5 'tres)
;; Esperado -> "Error: No se puede iniciar la auditoria con datos invalidos"


;; ==========================================
;; PRUEBAS: REQUERIMIENTO 4 - recomendacion-ciclo
;; ==========================================

;; Camino Normal: Evaluación dinámica calculando la duración real de tu ciclo (30+20+5 + 9s intermitencias = 64s)
(recomendacion-ciclo (duracion-ciclo-IT2 30 20 5))
;; Esperado -> "Recomendacion: la duracion del ciclo es optima, no se requieren cambios."

;; Camino Alternativo: Envío directo de una métrica de tiempo óptima dentro del rango (35-150s)
(recomendacion-ciclo 90)
;; Esperado -> "Recomendacion: la duracion del ciclo es optima, no se requieren cambios."

;; Camino Alternativo: Ciclo excesivamente bajo que compromete la fluidez (Menor a 35 segundos)
(recomendacion-ciclo 25)
;; Esperado -> "Recomendacion: aumentar la duracion del ciclo para mejorar la fluidez"

;; Camino Alternativo: Ciclo excesivamente alto que genera demora (Mayor a 150 segundos)
(recomendacion-ciclo 180)
;; Esperado -> "Recomendacion: reducir la duracion del ciclo para evitar la frustracion"

;; Caso de Error: El tipo de dato suministrado no es un número
(recomendacion-ciclo 'noventa)
;; Esperado -> "Error: el tipo de ciclo suministrado no es valido."


;; ==========================================
;; PRUEBAS: REQUERIMIENTO 5 - ciclos-por-tiempo
;; ==========================================

;; Camino Normal: Proyección de ciclos completos en una hora (60 min) sobre un ciclo de 64s (3600s / 64s)
(ciclos-por-tiempo 60 30 20 5)
;; Esperado -> 56

;; Camino Alternativo: Tiempo exacto que equivale de forma matemática a un único ciclo (1.066 minutos)
(ciclos-por-tiempo 1 30 20 1)
;; Esperado -> 1

;; Caso de Error: Envío del parámetro temporal en formato String
(ciclos-por-tiempo "60" 30 20 5)
;; Esperado -> "Error: datos no numericos"


;; ==========================================
;; PRUEBAS: REQUERIMIENTO 6 - informe-distribucion-IT2
;; ==========================================

;; Camino Normal: Generación y salida por REPL de los porcentajes incluyendo los 9s de transiciones intermitentes
(informe-distribucion-IT2 30 20 5)
;; Esperado -> Imprime en consola:
;;             INFORME DE DISTRIBUCION TEMPORAL (1 HORA):
;;             ROJO: 46.88%
;;             VERDE: 31.25%
;;             AMARILLO: 7.81%
;;             TRANSICIONES: 14.06%


;; Caso de Error: Uno de los parámetros de color no es numérico

(informe-distribucion-IT2 30 'veinte 5)
;; Esperado -> Imprime en consola:
;;             Error: Todos los argumentos deben ser numeros.


;; ==========================================
;; EXTENSION 2: PERSISTENCIA - informe-IT2
;; ==========================================

;; Camino Normal: 
(informe-IT2 '(("[2026-06-14 22:55:10]" ROJO ROJO-INTERMITENTE) ("[2026-06-14 22:55:11]" ROJO-INTERMITENTE VERDE)))
;; Esperado -> "Informe generado con éxito en 'C:/Segundo anio/integrador/informe-ejecucion-semaforo.txt'"


;; Camino Normal:
(informe-it2 (auditoria-it2 'rojo 1 1 1 10))
;; Esperado -> "Informe generado con éxito en 'C:/Segundo anio/integrador/informe-ejecucion-semaforo.txt'"


;; Caso de Error: 
(informe-IT2 nil)
;; Esperado -> "Error: No hay datos para generar el informe."




;; =============================================================================================================================
;;                                            PRUEBAS ADICIONALES: FUNCIONES AUXILIARES
;; =============================================================================================================================

;; ==========================================
;; PRUEBAS: AUXILIAR 1 - duracion-ciclo-IT2
;; ==========================================

;; Camino Normal: Suma de focos base más los 9 segundos fijos de márgenes de seguridad vial (30 + 20 + 5 + 9)
(duracion-ciclo-IT2 30 20 5)
;; Esperado -> 64

;; Caso de Error: Presencia de un elemento simbólico en los argumentos viales
(duracion-ciclo-IT2 30 'veinte 5)
;; Esperado -> NIL


;; ==========================================
;; PRUEBAS: AUXILIAR 2 - obtener-timestamp
;; ==========================================

;; Camino Normal: Retorna el timestamp de Unix calculado matemáticamente de forma pura desde la época base
(obtener-timestamp)
;; Esperado -> Un entero largo (Ej: 1781477710)


;; ==========================================
;; PRUEBAS: AUXILIAR 3 - obtener-timestamp-humano
;; ==========================================

;; Camino Normal: Retorna la cadena de fecha y hora ajustada -3 horas al huso local de forma pura e inmutable
(obtener-timestamp-humano)
;; Esperado -> Un String formateado (Ej: "2026-06-14 22:55:10")


;; ==========================================
;; PRUEBAS: AUXILIAR 4 - convertir-color-IT2
;; ==========================================

;; Camino Normal: Mapeo de un color elemental a su nomenclatura interna de estado vial
(convertir-color-IT2 'rojo-intermitente)
;; Esperado -> EN-ROJO-INTERMITENTE

;; Camino Alternativo: Si el color ya se encuentra formateado, lo retorna intacto por la rama por defecto
(convertir-color-IT2 'en-verde)
;; Esperado -> EN-VERDE


;; ==========================================
;; PRUEBAS: AUXILIAR 5 - validar-transiciones-p-IT2
;; ==========================================

;; Camino Normal: Transición reglamentaria permitida por la máquina de estados
(validar-transiciones-p-IT2 'en-verde 'verde-intermitente)
;; Esperado -> T

;; Caso de Error: Transición directa prohibida por seguridad por saltearse la fase intermitente
(validar-transiciones-p-IT2 'en-rojo 'verde)
;; Esperado -> NIL