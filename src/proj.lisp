;; proj.lisp
;; Includes all relevant files for our project
;; Grupo 47

;; as nossas bibliotecas genericas
(load "list.lisp")


;; estrutura problema
(load "problema.lisp")

;; estruturas dependentes do problema
(load "node.lisp")
(load "conjunto_explorados.lisp")
(load "queue.lisp")

;; formulacoes do problema
(load "formulacao_a.lisp")
(load "formulacao_b.lisp")

;; formulacoes do problema
(load "formulacao_a.lisp")

;; debug/test problemas e formulacoes
(load "formulacao_debug.lisp")

;; procuras nao informadas
(load "pcu.lisp")
(load "plp.lisp")
(load "ppp.lisp")
(load "ppl.lisp")
(load "ppi.lisp")
(load "pg.lisp")
(load "pa.lisp")

;; procuras informadas
; soon (TM)
;(load "pg.lisp")
;(load "pastar.lisp")
(load "rbfs.lisp")

;; problema de satisfacao de restricoes
(load "csp.lisp")
(load "psr.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Daqui para baixo comentar para a entrega !! ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; function profiling
;(sb-profile:profile pcua pcug)
;(sb-profile:profile plpa plpg)
;(sb-profile:profile pppa pppg)
;(sb-profile:profile ppla pplg)
;(sb-profile:profile ppia ppig)
;; to view report use: (sb-profile:report)

;; testes
(load "test-problemas.lisp")

(defvar teste1)
(setf teste1 (faz-accoes-possiveis *test-tarefas-1*))

(defvar testef1)
(setf testef1 (faz-accoes-possiveis1 *test-tarefas-1*))

(defvar problema)
(setf problema (formulacao-problema *test-tarefas-1*))




(defvar problemaf1)
(setf problemaf1 (formulacao-problema1 *test-tarefas-1*))



(defvar problema1)
(setf problema1 (formulacao-problema *test-tarefas-c3*))

(defvar teste2)
(setf teste2 (faz-accoes-possiveis *test-tarefas-c1*))

(defvar teste3)
(setf teste3 (faz-accoes-possiveis *test-tarefas-c2*))

(defvar teste4)
(setf teste4 (faz-accoes-possiveis *test-tarefas-c3*))

(defvar teste5)
(setf teste5 (faz-accoes-possiveis *test-tarefas-c4*))

(defvar ex5)
(setf ex5 (formulacao-problema *ex5*))

(defvar ex6)
(setf ex6 (formulacao-problema *ex6*))


;Formulacao problema

(defvar problema1_b)
(setf problema1_b (formulacao-problema1 *test-tarefas-c3*))

(defvar teste2_b)
(setf teste2_b (faz-accoes-possiveis1 *test-tarefas-c1*))

(defvar teste3_b)
(setf teste3_b (faz-accoes-possiveis1 *test-tarefas-c2*))

(defvar teste4_b)
(setf teste4_b (faz-accoes-possiveis1 *test-tarefas-c3*))

(defvar teste5_b)
(setf teste5_b (faz-accoes-possiveis1 *test-tarefas-c4*))

(defvar ex5_b)
(setf ex5_b (formulacao-problema1 *ex5*))

(defvar ex6_b)
(setf ex6_b (formulacao-problema1 *ex6*))

(defvar htest1_b)
(setf htest1_b (formulacao-problema1 *htest1*))

(defvar htest2_b)
(setf htest2_b (formulacao-problema1 *htest2*))