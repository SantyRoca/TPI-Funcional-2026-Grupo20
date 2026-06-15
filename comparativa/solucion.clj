;; ===========================================           FASE 3             ===========================================

;; =============================================================================================================================
;;                                               FUNCIONES AUXILIARES
;; =============================================================================================================================

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura (Cálculo matemático determinista)
;; ESTRATEGIA: Condicional de validación de tipos mediante 'if'
;; IMPACTO: No destructiva
;; ========================================================

(defn duracion-ciclo [duracion-rojo duracion-verde duracion-amarillo]
  (if (and (number? duracion-rojo) (number? duracion-verde) (number? duracion-amarillo))
    (+ duracion-rojo duracion-verde duracion-amarillo 9)
    false))


;; ========================================================
;; FUNCIÓN: obtener-timestamp
;; NATURALEZA: Impura (Depende de un estado externo: el reloj de la CPU)
;; ESTRATEGIA: Interoperabilidad nativa con la API de tiempo de la JVM (java.time)
;; IMPACTO: No destructiva
;; ========================================================

(defn obtener-timestamp []
  (.getEpochSecond (java.time.Instant/now)))

;; =============================================================================================================================
;; FUNCION: convertir-color
;; NATURALEZA: Pura (Establece una traduccion determinista de simbolos aislada de variables globales y de operaciones de E/S)
;; ESTRATEGIA: Funcion Condicional 
;; IMPACTO: No Destructiva (Retorna referencias simbolicas estaticas sin mutar bajo ningun concepto el argumento de origen)
;; =============================================================================================================================

(defn convertir-color [color]
  (cond
    (= color :rojo) :en-rojo
    (= color :rojo-intermitente) :en-rojo-intermitente

    (= color :verde) :en-verde
    (= color :verde-intermitente) :en-verde-intermitente

    (= color :amarillo) :en-amarillo
    (= color :amarillo-intermitente) :en-amarillo-intermitente

    :else color))

;; ========================================================
;; FUNCIÓN: validar-transiciones-p
;; NATURALEZA: Pura (Predicado lógico estricto)
;; ESTRATEGIA: Condicional 
;; IMPACTO: No destructiva
;; ========================================================
(defn validar-transiciones-p [color-actual cambiar-a]
  (cond
    (and (= color-actual :en-rojo) (= cambiar-a :rojo-intermitente)) true
    (and (= color-actual :en-rojo-intermitente) (= cambiar-a :verde)) true
    (and (= color-actual :en-verde) (= cambiar-a :verde-intermitente)) true
    (and (= color-actual :en-verde-intermitente) (= cambiar-a :amarillo)) true
    (and (= color-actual :en-amarillo) (= cambiar-a :amarillo-intermitente)) true
    (and (= color-actual :en-amarillo-intermitente) (= cambiar-a :rojo)) true
    :else false))