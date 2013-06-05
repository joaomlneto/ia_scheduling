;; formulacao_a.lisp
;; Problemas e coisas associadas
;; Grupo 47

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Funcoes para as procuras (descrevem um nó)
;;

; - faz-solucao?
; Funcao de ordem superior que devolve uma funcao parametrizada pelo tarefas
; que devolve 'T caso o estado seja solucao, 'NIL caso contrario
;
; NOTA: Nao verifica a integridade do estado, apenas se este e uma atribuicao
;       completa
;
; SUGESTAO: optimizacao - tornar a funcao inline (+ rapida, + memoria)
; (declaim (inline f))
(defun faz-solucao? (tarefas)
	#'(lambda (estado)
		(eq (list-length estado) (list-length tarefas))))  


(defun faz-accoes-possiveis (tarefas)
	#'(lambda (estado)
		(let ((num-tarefa (length estado))
		      (alternativas-tarefa '())
		      (alternativas-validas-i '()) ;lista de alternativas (constituidas por subtarefas) para a tarefa em causa
		      (subtarefas-alternativa '())
		      (subtarefas-estado (subtarefas-estado estado))
		      (numSub 0))
		     ;(format t "1Subtarefas estado: ~a ~%" subtarefas-estado)
		     (setf alternativas-tarefa (nth num-tarefa tarefas))
		     ;(format t "2Alternativas à tarefa actual: ~a ~%~%" alternativas-tarefa)
		     (dotimes (i (length alternativas-tarefa) alternativas-validas-i) ;percorre alternativas
				(setf numSub 0)
				(setf subtarefas-alternativa (nth i alternativas-tarefa))
				(dotimes (j (length subtarefas-alternativa)) ;percorre subtarefas
					  (dotimes (k (length subtarefas-estado) )
					    ;(format t "3Comparacao subtarefa-alternativa com cada subtarefa-estado ~a ~a ~%~%" (nth j subtarefas-alternativa) (nth k subtarefas-estado))
					    (if (not (subtarefas-conflitop (nth j subtarefas-alternativa) (nth k subtarefas-estado)))
					      (incf numSub)
					    )))
					(if (equal numSub (* (length (nth i alternativas-tarefa)) (length subtarefas-estado)))
					   (progn 
					      ;(format t "4Alternativas-validas-i antiga: ~a  alternativas-validas-i actual: ~a ~%~%" alternativas-validas-i (append alternativas-validas-i (list (nth i alternativas-tarefa))))
					      (setf alternativas-validas-i (append alternativas-validas-i (list (nth i alternativas-tarefa))))
			   )
			)
		alternativas-validas-i))))
		

(defun faz-resultado (tarefas)
	tarefas ; just to suppress warning
	#'(lambda (estado accao)
		; originalmente (append (list accao) estado)
		(cons accao estado)))
		
;; (defun faz-resultado1 (tarefas )
;;       #'(lambda (estado)
;; 	(if (list-empty estado)
;; 		'()
;; 		(let ((subtarefas-anteriores (subtarefas-estado tarefas (estado-anterior estado)))
;; 		      (subtarefas-ultima (subtarefas-ultima tarefas estado)))
;; 			(junta-conjunto-subtarefas subtarefas-anteriores subtarefas-ultima)))))

(defun faz-custo-caminho (tarefas)
	tarefas
	#'(lambda (estado accao)
		(let ((estado-seguinte (append (list accao) estado)))
			(- (list-length (remove-duplicates (subtarefas-estado estado-seguinte) :test #'subtarefas-iguaisp))
			   (list-length (remove-duplicates (subtarefas-estado estado) :test #'subtarefas-iguaisp))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formulacoes de problema
(defun formulacao-problema (tarefas)
	(make-problema
		:estado-inicial '()
		:solucao?       (faz-solucao? tarefas)
		:accoes         (faz-accoes-possiveis tarefas)
		:resultado      (faz-resultado tarefas)
		:custo-caminho  (faz-custo-caminho tarefas)))


;====================================================== HEURISTICAS ============================


; [Formulacao A - Heuristica 1]
; Admissivel?  Sim
; Consistente? Sim
;
; Pao pao, queijo queijo
(defun faz-heuristica-a-1 (tarefas)
	tarefas
	#'(lambda (node)
		node
		0))

; [Formulacao A - Heuristica 2]
; Admissivel?  Nao
; Consistente? Nao
;
; Devolve o numero de tarefas as quais faltam atribuir alternativas.
;
; Nao e optima se por ex. faltar uma tarefa mas ha uma alternativa
; com custo zero - demasiado frequente para ser uma boa heuristica.
;
; Sugestoes para melhorar:
; - Testar ao nivel de alternativas, em vez de ao nivel de tarefas (H3).
(defun faz-heuristica-a-2 (tarefas)
	#'(lambda (estado)
		(- (list-length tarefas) (list-length estado))))

; [Formulacao A - Heuristica 3]
; Admissivel?  Nao
; Consistente? Nao
;
; Devolve a soma dos custos de uma das alternativas de menor custo para
; cada tarefa as quais ainda faltam atribuir alternativas.
;
; Nao tem em conta todas as alternativas possiveis - nem sempre consegue
; fazer uma estimativa optimista, muito menos consistente.
;
; Sugestoes para melhorar:
; - Ter em conta todas as alternativas de menor custo, em vez de uma
; escolhida ao acaso ou de forma ordenada (H4)
; // TODO


; [Formulacao A - Heuristica 4]
; Admissivel?  Nao
; Consistente? Nao
;
; Tem em conta todas as alternativas de menor custo de todas as tarefas
; para as quais ainda faltam atribuir alternativas.
;
; Nao e optimista caso a alternativa que leva a solucao de custo optimo
; nao e a alternativa que tem o custo mais baixo entre as alternativas
; para a mesma tarefa.
;
; Sugestoes para melhorar:
; - Ter em conta todas as alternativas possiveis, mesmo as que nao
; tenham o menor custo (H5)
; // TODO

; [Formulacao A - Heuristica 5]
; Admissivel?  Sim
; Consistente? Sim (?)
;
; Esta heuristica tem em conta todas as alternativas de todas as tarefas
; para as quais ainda nao foram escolhidas alternativas.
; Devolve um custo optimista, tendo um erro correspondente ao numero
; de horas de diferenca do somatorio de todas as iteracoes (sendo que em
; cada iteracao e visitada uma tarefa diferente) da diferenca entre a
; alternativa de menor custo e a alternativa que leva a solucao optima.
;
; i.e. se uma alternativa tiver custo 2, mas a que leva a solucao optima
; tem um custo 3, entao a heuristica devolve um custo optimista de 2
; nessa iteracao.
;
; Esta heuristica NAO tem em conta problemas de conflitos entre
; subtarefas - e uma heuristica gerada a partir de um problema relaxado.
;
; Sugestoes para melhorar:
; - detectar que um determinado caminho so leva a conflitos
; - detectar que a melhor alternativa nao e a que tem o menor custo
(defun faz-heuristica-a-5 (tarefas)
	#'(lambda (estado)
		(labels
			((h-aux (subtarefas-estado tarefas)
				(let ((custo-minimo most-positive-fixnum)
				      (subtarefas-alt-com-estado nil)
						(subtarefas-tudo subtarefas-estado))
					; condicao de paragem - nao faltam tarefas
					(if (null tarefas)
						(progn
							0)
						(progn
							(loop for alt in (first tarefas) do
								(setf subtarefas-alt-com-estado (junta-conjunto-subtarefas alt subtarefas-estado))
								(setf subtarefas-tudo (junta-conjunto-subtarefas alt subtarefas-tudo))
								(if (< (- (list-length subtarefas-alt-com-estado) (list-length subtarefas-estado))
								       custo-minimo)
									(setf custo-minimo (- (list-length subtarefas-alt-com-estado) (list-length subtarefas-estado)))))
							(+ (h-aux subtarefas-tudo (rest tarefas)) custo-minimo))))))
			(h-aux (subtarefas-estado estado) (nthcdr (list-length estado) tarefas)))))

; conflict-avoider A-5
(defun faz-heuristica-a-6 (tarefas)
	#'(lambda (estado)
		(let ((subtarefas-estado-inicial (remove-subtarefas-iguais (subtarefas-estado estado))))
			(labels
				((h-aux (subtarefas-estado tarefas)
					(let ((custo-minimo most-positive-fixnum)
					      (subtarefas-alt-com-estado nil)
					      (subtarefas-tudo subtarefas-estado)
					      (alternativas-sem-conflito (list-length (first tarefas))))
						; condicao de paragem - nao faltam tarefas
						(if (null tarefas)
							(return-from h-aux 0))
						; para cada uma das alternativas da tarefa da iteracao actual
						(loop for alt in (first tarefas) do
							; uniao das subtarefas desta alternativa com subtarefas do conj anterior
							(setf subtarefas-alt-com-estado (junta-conjunto-subtarefas alt subtarefas-estado))
							; uniao das subtarefas desta alternativa com todas as subtarefas vistas ate agora
							(setf subtarefas-tudo (junta-conjunto-subtarefas alt subtarefas-tudo))
							; se esta alternativa tiver um custo menor, actualizar custo-minimo da tarefa
							(if (< (- (list-length subtarefas-alt-com-estado) (list-length subtarefas-estado))
							       custo-minimo)
								(setf custo-minimo (- (list-length subtarefas-alt-com-estado) (list-length subtarefas-estado))))
							; se esta alternativa entrar em conflito com subtarefas-estado-inicial, decrementar variavel
							(if (conflito-listas-subtarefasp subtarefas-estado-inicial alt)
								(progn
									(setf alternativas-sem-conflito (1- alternativas-sem-conflito))
									; se a variavel chegar a zero, devolver infinito! este caminho nao tem solucoes!
									(if (zerop alternativas-sem-conflito)
										(return-from h-aux most-positive-fixnum)))))
						; <fim do loop>
						; somar custo desta tarefa com o das tarefas que faltam (recursivamente)
						(+ (h-aux subtarefas-tudo (rest tarefas)) custo-minimo))))
				; passar apenas as tarefas para as quais ainda nao foram atribuidas alternativas
				(h-aux subtarefas-estado-inicial (nthcdr (list-length estado) tarefas))))))


