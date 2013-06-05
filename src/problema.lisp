

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problema
(defstruct problema estado-inicial solucao? accoes resultado custo-caminho (solucao #'identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subtarefa
(defun faz-subtarefa (&key dia hora id)
	(list dia hora id))

(defun subtarefa-dia (subtarefa)
	(nth 0 subtarefa))

(defun subtarefa-hora (subtarefa)
	(nth 1 subtarefa))

(defun subtarefa-id (subtarefa)
	(nth 2 subtarefa))

; verifica se duas sub-tarefas ocorrem no mesmo dia e hora
(defun subtarefas-simultaneasp (a b)
	(and (eq (subtarefa-dia  a) (subtarefa-dia  b))
	     (eq (subtarefa-hora a) (subtarefa-hora b))))

; verifica se as duas subtarefas representam a mesma subtarefa
; i.e. tem o mesmo identificador, dia e hora
(defun subtarefas-iguaisp (a b)
	(and (subtarefas-simultaneasp a b)
	     (equal (subtarefa-id a) (subtarefa-id b))))

; verifica se duas subtarefas com identificadores diferentes
; irao ocorrer no mesmo dia e hora - conflito
(defun subtarefas-conflitop (a b)
	(and (subtarefas-simultaneasp a b)
	     (not (equal (subtarefa-id a) (subtarefa-id b)))))

; devolve o estado anterior de um determinado estado
(defun estado-anterior (estado)
	(subseq estado 0 (1- (list-length estado))))

; dada uma lista de tarefas, devolve a lista de alternativas da tarefa no indice
; especificado 
(defun tarefa-indice (tarefas indice)
	(nth indice tarefas))

; dada uma lista de alternativas, devolve a lista de subtarefas dessa
; no indice especificado
(defun alternativa-indice (alternativas indice)
	(nth indice alternativas))

; dada uma lista de subtarefas, devolve a subtarefa dessa lista com o indice
; especificado
(defun subtarefa-indice (subtarefas indice)
	(nth indice subtarefas))

; devolve as subtarefas de uma dada alternativa de uma tarefa da lista de
; tarefas
(defun subtarefas-tarefa (tarefas indice-tarefa indice-alternativa)
	(alternativa-indice (tarefa-indice tarefas indice-tarefa)
	                    indice-alternativa))

; devolve a accao que levou ao estado
; a accao e um par
; -> car: indice da tarefa
; -> car: indice da alternativa escolhida
(defun ultima-accao (estado)
	(cons (1- (list-length estado)) (list-last estado)))

; devolve a lista de subtarefas da ultima tarefa de um estado para a qual foi
; escolhida uma alternativa
(defun subtarefas-ultima (tarefas estado)
	(let ((ultima-accao (ultima-accao estado)))
		(subtarefas-tarefa tarefas (car ultima-accao) (cdr ultima-accao))))

; remove subtarefas iguais
(defun remove-subtarefas-iguais (subtarefas)
	(remove-duplicates subtarefas :test #'subtarefas-iguaisp))

; faz a uniao de dois conjuntos de subtarefas
(defun junta-conjunto-subtarefas (conjunto-subtarefas1 conjunto-subtarefas2)
	(union conjunto-subtarefas1 conjunto-subtarefas2 :test #'subtarefas-iguaisp))

; compara duas listas de subtarefas
; se houver conflito entre uma subtarefa da lista 1 e uma subtarefa da lista 2
; devolve true
; caso contrario, devolve nil
;
; TODO: recursao!! isto assim nao é lisp-like!
(defun conflito-listas-subtarefasp (lst1 lst2)
	(loop for sub1 in lst1 do
		(loop for sub2 in lst2 do
			(if (subtarefas-conflitop sub1 sub2)
				(return-from conflito-listas-subtarefasp T))))
	nil)

; devolve o conjunto de todas as subtarefas de um estado
;(defun subtarefas-estado (tarefas estado)
;	(if (list-empty estado)
;		'()
;		(let ((subtarefas-anteriores (subtarefas-estado tarefas (estado-anterior estado)))
;		      (subtarefas-ultima (subtarefas-ultima tarefas estado)))
;			(junta-conjunto-subtarefas subtarefas-anteriores subtarefas-ultima))))

(defun subtarefas-estado (elemento)
	(cond ((null elemento) nil)
	      ((null (first elemento)) (subtarefas-estado (rest elemento)))
	      (t (append (first elemento) (subtarefas-estado (rest elemento))))))


