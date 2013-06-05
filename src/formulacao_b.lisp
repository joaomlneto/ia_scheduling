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
(defun faz-solucao1? (tarefas)
	#'(lambda (estado)
		(eq (list-length estado) (list-length tarefas))))  

;nova formulacao
;transforma o estado num lista de pares Tarefa-alternativa
(defun converte-estado-tarefa-alternativa (estado)
		(loop for alternativa in estado
			  for j from 0 collect
			(list j alternativa)))

;nova formulacao
;transforma as tarefas numa lista de pares Tarefa-Alternativa
(defun converte-tarefas-tarefa-alternativa (tarefas)
	(let ((lst nil))
		(loop for tarefa in tarefas
			  for j from 0 do
			  (loop for alternativa in tarefa collect
					(setf lst (append lst (list (list j alternativa))))))
		lst))

;nova formulacao
;retira todos os pares Tarefa-Alternativa da tarefa que já foi escolhida no estado
(defun remove-pares-tarefa-ja-escolhida (tarefas_tarefa-alternativa estado)
	(let ((lst tarefas_tarefa-alternativa))
		(dotimes (i (list-length estado) lst)
			(loop for par in tarefas_tarefa-alternativa do
				  (if (equal (first par) i)
						(setf lst (remove par lst)))))))

;nova formulacao
;retira todas os pares Tarefa-Alternativa que entrem em conflito
(defun remove-pares-conflitos (accoes-possiveis_tarefa-alternativa estado_tarefa-alternativa)
	(let ((lst accoes-possiveis_tarefa-alternativa))
		(loop for par-estado in estado_tarefa-alternativa do
			(loop for par-accoes-possiveis in accoes-possiveis_tarefa-alternativa do
				;(format t "Comparacao entre ~a e ~a ~%" (first (rest (first (rest par-estado)))) (first (rest par-accoes-possiveis)))
				  (if (conflito-listas-subtarefasp (first (rest (first (rest par-estado)))) (first (rest par-accoes-possiveis)))
				  	(progn 
				  			;(format t "Conflito entre ~a e ~a ~%"  (first (rest par-estado)) (first (rest par-accoes-possiveis)))
							(setf lst (remove par-accoes-possiveis lst))))))
	lst))

;nova formulacao
(defun converte-par-para-solucao (accoes-possiveis_tarefa-alternativa)
	(if (null accoes-possiveis_tarefa-alternativa)
		nil
		(cons (first (rest (first accoes-possiveis_tarefa-alternativa))) (converte-par-para-solucao (rest accoes-possiveis_tarefa-alternativa)))))


(defun faz-accoes-possiveis1 (tarefas)
	#'(lambda (estado)
		(let ((estado_tarefa-alternativa nil)
				(tarefas_tarefa-alternativa nil)
				(accoes-possiveis_tarefa-alternativa nil))
			;converte o estado num lista de pares Tarefa-alternativa
			(setf estado_tarefa-alternativa (converte-estado-tarefa-alternativa estado))

			;converte as tarefas numa lista de pares Tarefa-Alternativa
			(setf tarefas_tarefa-alternativa (converte-tarefas-tarefa-alternativa tarefas))

			;retira todos os pares Tarefa-Alternativa da tarefa que já foi escolhida no estado
			(setf accoes-possiveis_tarefa-alternativa 
				(remove-pares-tarefa-ja-escolhida tarefas_tarefa-alternativa estado))

			;retira todas os pares Tarefa-Alternativa que entrem em conflito
			(setf accoes-possiveis_tarefa-alternativa 
				(remove-pares-conflitos accoes-possiveis_tarefa-alternativa estado_tarefa-alternativa))
			accoes-possiveis_tarefa-alternativa
		))
)
		

(defun faz-resultado1 (tarefas)
	tarefas ; just to suppress warning
	#'(lambda (estado accao)
		; originalmente (append (list accao) estado)
		(cons accao estado)))

(defun faz-custo-caminho1 (tarefas)
	tarefas
	#'(lambda (estado accao)
		(let ((estado-seguinte (append (list accao) estado)))
			(- (list-length (remove-duplicates (subtarefas-estado estado-seguinte) :test #'subtarefas-iguaisp))
			   (list-length (remove-duplicates (subtarefas-estado estado) :test #'subtarefas-iguaisp))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formulacoes de problema
(defun formulacao-problema1 (tarefas)
	(make-problema
		:estado-inicial '()
		:solucao?       (faz-solucao1? tarefas)
		:accoes         (faz-accoes-possiveis1 tarefas)
		:resultado      (faz-resultado1 tarefas)
		:custo-caminho  (faz-custo-caminho1 tarefas)))


;====================================================== HEURISTICAS ============================


; [Formulacao A - Heuristica 1]
; Admissivel?  Sim
; Consistente? Sim
;
; Pao pao, queijo queijo
(defun faz-heuristica-b-1 (tarefas)
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
(defun faz-heuristica-b-2 (tarefas)
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
(defun faz-heuristica-b-5 (tarefas)
	#'(lambda (estado_b)
		(let ((tarefas_tarefa-alternativa nil))
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
			(setf tarefas_tarefa-alternativa (converte-tarefas-tarefa-alternativa tarefas))
			(setf tarefas_tarefa-alternativa 
				(remove-pares-tarefa-ja-escolhida tarefas_tarefa-alternativa estado_b))
			(setf tarefas_tarefa-alternativa (converte-par-para-solucao tarefas_tarefa-alternativa))
			(h-aux (subtarefas-estado (mapcar (lambda (x) (first (rest x))) estado_b)) tarefas_tarefa-alternativa)))))

; conflict-avoider A-5
(defun faz-heuristica-b-6 (tarefas)
	#'(lambda (estado_b)
		(let ((subtarefas-estado-inicial (remove-subtarefas-iguais (subtarefas-estado (mapcar (lambda (x) (first (rest x))) estado_b)))))
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
				(h-aux subtarefas-estado-inicial (nthcdr (list-length (mapcar (lambda (x) (first (rest x))) estado_b)) tarefas))))))
