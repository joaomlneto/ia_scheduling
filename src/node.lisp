; node.lisp
;
; Structure that represents a search node
; From Artificial Intelligence: A Modern Approach

; - Estrutura node
;
; Representa um no de uma arvore de procura
;
; estado: o estado associado ao node (depende da formulacao)
; 
(defstruct node estado node-pai accao custo-caminho custo-heuristica custo)

; - node-filho
;
; Com base um node e uma accao, devolve o node filho, no contexto de um dado problema
; 
; problema: a estrutura problema
; node-pai: o node de origem
; accao   : a accao aplicada no node-pai
;
; devolve: estrutura do tipo node, resultante de aplicar a accao a node-pai no
;          contexto do problema
(defun node-filho (problema node-pai accao)
	(make-node
	   ; STATE = problem.RESULT(parent.STATE, action)
		:estado        (funcall (problema-resultado problema) (node-estado node-pai) accao)
		; PARENT = parent
		:node-pai      node-pai
		; ACTION = action
		:accao         accao
		; PATH-COST = parent.PATH-COST + problem.STEP-COST(parent.STATE, action)
		:custo-caminho (+ (node-custo-caminho node-pai)
		                  (funcall (problema-custo-caminho problema) (node-estado node-pai) accao))))

(defun node-filho-com-heuristica (problema node-pai accao funcao-heuristica)
	(let ((node-filho (node-filho problema node-pai accao)))
		(setf (node-custo-heuristica node-filho) (funcall funcao-heuristica (node-estado node-filho)))
		(setf (node-custo node-filho) (+ (node-custo-caminho node-filho) (node-custo-heuristica node-filho)))
		node-filho))

; - node-inicial
;
; Com base num problema, cria o node com o estado inicial
;
; problema: a estrutura problema
;
; devolve: estrutura do tipo node, com estado inicial do problema, sem node-pai,
;          sem accao e com custo-caminho 0
(defun node-inicial (problema)
	(make-node
		:estado           (problema-estado-inicial problema)
		:node-pai         nil
		:accao            nil
		:custo-caminho    0
		:custo-heuristica 0
		:custo            0))

