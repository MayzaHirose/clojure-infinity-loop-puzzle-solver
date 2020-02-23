(ns clojure-infinity-loop-puzzle-solver.core)

;; lista com as representações dos blocos como caracteres
(def blocos-reps (seq " ╹╺┗╻┃┏┣╸┛━┻┓┫┳╋"))

;; *****************************INICIO TAMANHO DO JOGO**********************************
;; Tamanho representa o tamanho de um jogo em altura e largura
;;     altura : Número - quantidade de linhas do jogo.
;;    largura : Número - quantidade de colunas do jogo.
(defstruct tamanho :altura :largura)

;; Jogo -> Struct tamanho
;; ----------------------
;; Retorna uma struct tamanho com as dimensões do jogo
;; Exemplo: (jogo-tamanho '((3 14 12) (3 11 12)))
;;          > (tamanho 2 3)
(defn jogo-tamanho [jogo]
        (if (= (count jogo) 0)
          0,
          (struct tamanho (count jogo)(count (first jogo)))))
;; *****************************FIM TAMANHO DO JOGO**********************************

;; *****************************INICIO ROTACIONAR*************************************
;; Bloco -> Bloco
;; --------------
;; Rotaciona um bloco 90 graus em sentido horário
;; Exemplo: (rotacionar 5)
;;          > 10
(defn rotacionar [bloco]
  (+ (bit-and (- (bit-shift-left 1 4) 1) (bit-shift-left (bit-shift-left bloco 1) 0)) (bit-shift-right bloco -3)))

;; Bloco -> List-Possibilidades
;; --------------
;; Recebe um bloco e retorna uma lista com as rotações dele em decimal
;; Exemplo: (bloco-possibilidades 3)
;;          > '(3 6 12 9)
(defn bloco-possibilidades [bloco]
  (def r1 (rotacionar bloco))
  (def r2 (rotacionar r1))
  (def r3 (rotacionar r2))
  (cond (= 15 bloco) (list bloco -1 0 0)
        (= 5 bloco) (list bloco 10 -1 0)
        (= 10 bloco) (list bloco 5 -1 0)
        (= 0 bloco) (list bloco -1 0 0)
        :else (list bloco r1 r2 r3)))

;; Jogo -> List-Possibilidades
;; --------------
;; Recebe uma lista em decimal referente ao jogo e cria uma lista com todas as possibilidades de rotação de cada bloco
;; Exemplo: (jogo-possibilidades '((3 14 12) (3 11 12)))
;;          > '((3 6 12 9) (14 13 11 7) (12 9 3 6)) ((3 6 12 9) (11 7 14 13) (12 9 3 6))
(defn jogo-possibilidades [jogo]
        (if (= (count jogo) 0)
          [],
          (if (list? (first jogo))
           (conj (jogo-possibilidades (first jogo))
                   (jogo-possibilidades (rest jogo))),
           (cons (bloco-possibilidades (first jogo))
                (jogo-possibilidades (rest jogo))))))

;; *****************************FIM ROTACIONAR*************************************