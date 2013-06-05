;;
;; Queremos reavaliar!
;;



;; list.lisp
;;
;; Extensao da biblioteca de listas do lisp

; - list-empty
; verifica se lst e uma lista vazia
(defun list-empty (lst)
	(zerop (list-length lst)))

; - list-last
; devolve o ultimo elemento da lista
(defun list-last (lst)
	(first (last lst)))

; - flatten
; remove todos os niveis da lista
(defun flatten (structure)
  (cond ((null structure) nil)
          ((atom structure) `(,structure))
			         (t (mapcan #'flatten structure))))


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

;; - node_list.lisp
;;
;; Wrapper para a estrutura que armazena/procura nodes
;; Utilizado para as procuras em grafo
;;
;; Implementacao actual: hash table (interna do lisp)

; Cria um conjunto de nos explorados vazio
(defun faz-conjunto-explorados ()
	(make-hash-table :test #'EQUAL))

; Insere no num conjunto de nos explorados
; [AVISO] OPERACAO DESTRUTIVA
; (vantagem: nao e necessario usar setf)
(defun insere-conjunto-explorados (hash node)
	(setf (gethash node hash) node))

; Verifica se um no existe num conjunto de nos explorados
; Devolve o proprio no caso seja encontrado
; Devolve nil caso contrario
;
; [AVISO] a procura por 'nil devolve sempre nil,
;         independentemente de la estar ou nao.
(defun procura-conjunto-explorados (hash node)
	;(if node
		(nth-value 1 (gethash node hash)))
	;	nil))

; Imprime todos os elementos no conjunto de nos explorados
(defun imprime-conjunto-explorados (hash)
	(print "--INICIO HASH TABLE--")
	(loop for node being the hash-value of hash do (print node))
	(print "--FIM HASH TABLE--"))
; queue.lisp
;
; Tipo abstracto de dados fila/FIFO

; - make-queue
; Construtor da fila
; return: uma nova queue vazia
(defun make-queue ()
	'())

; - queue-emptyp
; Verifica se uma fila esta vazia
; queue: uma fila
; return: true caso esteja vazia, false caso contrario
(defun queue-emptyp (queue)
	(null queue))

; - queue-pop
; Devolve o primeiro elemento da lista, retirando-o da fila
; queue: uma fila
; return: o primeiro elemento da lista
; efeito: a fila original e modificada
(defun queue-pop (queue)
	(pop queue))

; - aplica-funcao-elementos
; Aplica uma funcao a cada elemento individual da fila
; queue: uma fila
; funcao: uma funcao que recebe um elemento como argumento
; return: devolve uma fila em que cada posicao corresponde ao resultado
;         da avaliacao da funcao a cada elemento
(defun aplica-funcao-elementos (queue funcao)
	(mapcar funcao queue))

; - queue-first
; Devolve o primeiro elemento de uma fila
; queue: uma fila
; return: o primeiro elemento da fila
(defun queue-first (queue)
	(first queue))

; QUEUE-INSERT(element, queue)
; QUEUE-INSERT-OR-REPLACE(element, queue)

(defun queue-add-or-replace-node (queue item)
  (labels ((queue-add-or-replace-node-aux (queue item addedp)
		  ; o estado e o mesmo?
		  ;(print (list "qaorn" queue item addedp))

		  ; a fila esta vazia?
		  (if (not (first queue)) 
			(return-from queue-add-or-replace-node-aux
						 (if addedp nil (list item))))

		  (if (equal (node-estado (first queue))
					 (node-estado item))
			; se ja foi adicionado antes, ignora este
			(if addedp (rest queue)
			  ; se nao, troca este pelo novo se for melhor
			  (if (> (node-custo (first queue))
					 (node-custo item))
				(cons item (rest queue))
				queue))
			; o estado nao e o mesmo?
			; entao, insere-o se o custo-caminho for melhor
			(if (and (not addedp)
					 (> (node-custo (first queue)) 
						(node-custo item)))
			  (cons item
					(cons (first queue) 
						  (queue-add-or-replace-node-aux (rest queue) item T)))
			  (cons (first queue) 
					(queue-add-or-replace-node-aux (rest queue) item addedp))))))
	(queue-add-or-replace-node-aux queue item nil)))

;(defun queue-add-or-replace-node (queue item)
;	(sort (copy-list (append (list item) queue) #'< :key #'node-custo-caminho)))

(defun queue-add-node (fila item &optional (funcao-custo #'node-custo))
	(if (or (equal nil (first fila)) (> (funcall funcao-custo (first  fila)) (funcall funcao-custo item)))
			(cons item fila)
			(cons (first fila) (queue-add-node (rest fila) item funcao-custo))))


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
(defun gera-heuristica1 (tarefas)
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
(defun gera-heuristica2 (tarefas)
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
(defun gera-heuristica5 (tarefas)
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
(defun gera-heuristica (tarefas)
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


; A4-dominant A-6
;(defun gera-heuristica7 (tarefas)
;	#'(lambda
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
;; pcu.lisp
;; Procura de custo-caminho Uniforme
;; Grupo 47

(defun pcua (problema)
  (let* ((estado-inicial (problema-estado-inicial problema))
		 (fila (list (make-node :estado estado-inicial
								:custo-caminho 0
								:custo 0)))
		 (no-actual nil)
		 (estado-actual nil)
		 (estados-sucessores nil)
		 (node-sucessor nil))	; estrutura do tipo "node" com estado + custo-caminho
	(loop
	  ; fila vazia?
	  (if (not fila) (return-from pcua nil))

	  (setf no-actual (pop fila))
	  (setf estado-actual (node-estado no-actual))

	  ; if problem.GOAL-TEST(node.STATE)
	  (if (funcall (problema-solucao? problema) estado-actual) ; "este estado é uma solução para este problema?"
		; then return SOLUTION(node)
		(return-from pcua estado-actual))	; yay!
	  ; else
	  ; foreach action in problem.ACTIONS(node.STATE) do
	  (setf estados-sucessores (funcall (problema-accoes problema) estado-actual))
	  (loop for sucessor in estados-sucessores  do
			; child <- CHILD-NODE(problem, node, action)
			(setf node-sucessor (make-node 	:estado (funcall (problema-resultado problema) estado-actual sucessor)
										   	:custo-caminho 	(+ 	(node-custo-caminho no-actual)
													 			(funcall (faz-custo-caminho problema) estado-actual sucessor))
											:custo 			(+ 	(node-custo-caminho no-actual)
													 			(funcall (faz-custo-caminho problema) estado-actual sucessor))))
			; if sucessor is not in queue, add it;
			; else if sucessor is in queue but with a higher path-cost, replace it with this one
			(setf fila (queue-add-node fila node-sucessor))))))

(defun pcug (problema)
  (let* ((estado-inicial (problema-estado-inicial problema))
		 (fila (list (make-node :estado estado-inicial
								:custo-caminho 0
		 						:custo 0)))
		 (no-actual nil)
		 (estado-actual nil)
		 (estados-sucessores nil)
		 (node-sucessor nil)	; estrutura do tipo "node" com estado + custo-caminho
		 (conjunto-explorados (faz-conjunto-explorados))) ; conjunto de nos explorados
	(loop
	  ; fila vazia?
	  (if (not fila) (return-from pcug nil))

	  (setf no-actual (pop fila))
	  (setf estado-actual (node-estado no-actual))

	  ; if problem.GOAL-TEST(node.STATE)
	  (if (funcall (problema-solucao? problema) estado-actual) ; "este estado é uma solução para este problema?"
		; then return SOLUTION(node)
		(return-from pcug estado-actual))	; yay!
	  ; else
	  ; GRAPH SEARCH: add the node to the explored set
	  (insere-conjunto-explorados conjunto-explorados estado-actual)
	  ; foreach action in problem.ACTIONS(node.STATE) do
	  (setf estados-sucessores (funcall (problema-accoes problema) estado-actual))
	  (loop for sucessor in estados-sucessores  do
			; GRAPH SEARCH: do not repeat explored nodes
			(if (not (procura-conjunto-explorados conjunto-explorados sucessor))
			  (progn
				; child <- CHILD-NODE(problem, node, action)
				(setf node-sucessor (make-node 	:estado 		(funcall (problema-resultado problema) estado-actual sucessor)
											   	:custo-caminho 	(+ 	(node-custo-caminho no-actual)
														 			(funcall (faz-custo-caminho problema) estado-actual sucessor))
												:custo 			(+ 	(node-custo-caminho no-actual)
										 							(funcall (faz-custo-caminho problema) estado-actual sucessor))))
				; if sucessor is not in queue, add it;
				; else if sucessor is in queue but with a higher path-cost, replace it with this one
				(setf fila (queue-add-or-replace-node fila node-sucessor))))))))
;; plp.lisp
;; Procura em Largura Primeiro
;; Grupo 47

(defun plpa (problema)
    (labels ((plpa-aux (sucessores)
	(let* ((estado (pop sucessores))
	      (accoes-possiveis (funcall (problema-accoes problema) estado))
	      (filhos nil))
		  (dotimes (i (length accoes-possiveis) filhos)
			(setf filhos (append filhos (list (funcall (problema-resultado problema) estado (nth i accoes-possiveis))))))
		  (cond ((funcall (problema-solucao? problema) estado) estado)
			((and (null filhos) sucessores) (plpa-aux sucessores))
			((and filhos (null sucessores)) (plpa-aux filhos))
			((and filhos sucessores) (plpa-aux (append sucessores filhos)))
			(t nil)))))
	  (plpa-aux (list (problema-estado-inicial problema)) )))


	  
(defun plpg (problema)
(let ((conjunto-explorados (faz-conjunto-explorados))) ; visited nodes set
    (labels ((plpg-aux (sucessores)
	(let* ((estado (pop sucessores))
	      (accoes-possiveis (funcall (problema-accoes problema) estado))
	      (filhos nil))
		(if (not (procura-conjunto-explorados conjunto-explorados estado))
		 (progn 
		    (dotimes (i (length accoes-possiveis) filhos)
			  (setf filhos (append filhos (list (funcall (problema-resultado problema) estado (nth i accoes-possiveis))))))
		    ;;(format t "Fila de estados: ~a ~%" sucessores) (format t "Estado a verificar: ~a ~%~%" estado)
		    ;;(format t "Tamanho hash table: ~a ~%~%" (hash-table-count conjunto-explorados))
		    (cond ((funcall (problema-solucao? problema) estado) estado)
			  ((and (null filhos) sucessores) 
			  	(insere-conjunto-explorados conjunto-explorados estado) (plpg-aux sucessores))
			  ((and filhos (null sucessores)) 
			  	(insere-conjunto-explorados conjunto-explorados estado) (plpg-aux filhos))
			  ((and filhos sucessores) 
			  	(insere-conjunto-explorados conjunto-explorados estado) (plpg-aux (append sucessores filhos)))
			  (t nil)))
		    (plpg-aux sucessores))
			  
			  )))
	  (plpg-aux (list (problema-estado-inicial problema)) ))));; ppp.lisp
;; Procura em Profundidade Primeiro
;; Grupo 47

;; Procura em Profundidade Primeiro - TREE SEARCH
(defun pppa (problema)
	(labels ((pppa-aux (estado)
		(let ((accoes-possiveis nil) ; for code readability
				(estado-sucessor nil)  ; stores child nodes
		      (resultado nil))       ; stores recursive search result
			; if problem.GOAL-TEST(node.STATE)
			(if (funcall (problema-solucao? problema) estado)
				; then return SOLUTION(NODE)
				(return-from pppa-aux estado))
			; else
			; foreach action in problem.ACTIONS(node.STATE) do
			(setf accoes-possiveis (funcall (problema-accoes problema) estado))
			(loop for accao in accoes-possiveis do
				; child <- CHILD-NODE(problem, node, action)
				(setf estado-sucessor (funcall (problema-resultado problema) estado accao))
				; result <- RECURSIVE-DFS(child, problem, limit-1)
				(setf resultado (pppa-aux estado-sucessor))
				; if result != failure then return result
				(if resultado (return-from pppa-aux resultado)))
			; no solution found - return FAILURE
			nil)))
		(pppa-aux (problema-estado-inicial problema))))

;; Procura em Profundidade Primeiro - GRAPH SEARCH
(defun pppg (problema)
	; [GRAPH-SEARCH] initialize the explored set to be empty
	(let ((conjunto-explorados (faz-conjunto-explorados))) ; explored set
		(labels ((pppg-aux (estado)
			(let ((accoes-possiveis nil) ; for code readability
			     (estado-sucessor nil)   ; stores child nodes
			     (resultado nil))        ; stores recursive search result
				; if problem.GOAL-TEST(node.STATE)
				(if (funcall (problema-solucao? problema) estado)
					; then return SOLUTION(NODE)
					(return-from pppg-aux estado))
				; else
				; [GRAPH-SEARCH] add the node to the explored set
				(insere-conjunto-explorados conjunto-explorados estado)
				; foreach action in problem.ACTIONS(node.STATE) do
				(setf accoes-possiveis (funcall (problema-accoes problema) estado))
				(loop for accao in accoes-possiveis do
					; child <- CHILD-NODE(problem, node, action)
					(setf estado-sucessor (funcall (problema-resultado problema) estado accao))
					; [GRAPH-SEARCH] if child node not in explored set
					(if (not (procura-conjunto-explorados conjunto-explorados estado-sucessor))
						(progn
							; [GRAPH-SEARCH] then expand child
							; result <- RECURSIVE-DFS(child, problem, limit-1)
							(setf resultado (pppg-aux estado-sucessor))
							; if result != failure then return result
							(if resultado (return-from pppg-aux resultado)))))
				; no solution found - return FAILURE
				nil)))
			(pppg-aux (problema-estado-inicial problema)))))
;; plp.lisp
;; Procura em Profundidade Limitada
;; Grupo 47


;; Procura em Profundidade Limitada - TREE SEARCH
(defun ppla (problema limite)
	(labels ((ppla-aux (estado limite)
		(let ((accoes-possiveis nil) ; for code readability
				(estado-sucessor nil)  ; stores child nodes
		      (resultado nil)        ; stores recursive search result
		      (ocorreu-corte? nil))  ; set to T if cutoff ocurred
			; if problem.GOAL-TEST(node.STATE)
			(if (funcall (problema-solucao? problema) estado)
				; then return SOLUTION(NODE)
				(return-from ppla-aux estado))
			; else if limit = 0
			(if (zerop limite)
				; then return CUTOFF
				(return-from ppla-aux 'CORTE))
			; else
			; foreach action in problem.ACTIONS(node.STATE) do
			(setf accoes-possiveis (funcall (problema-accoes problema) estado))
			(loop for accao in accoes-possiveis do
				; child <- CHILD-NODE(problem, node, action)
				(setf estado-sucessor (funcall (problema-resultado problema) estado accao))
				; result <- RECURSIVE-DFS(child, problem, limit-1)
				(setf resultado (ppla-aux estado-sucessor (1- limite)))
				; if result == cutoff then cutoff_ocurred? <- true
				(if (eq resultado 'CORTE) (setf ocorreu-corte? t))
				; if result != failure then return result
				(if resultado (return-from ppla-aux resultado)))
			; no solution found - return FAILURE
			ocorreu-corte?)))
		(ppla-aux (problema-estado-inicial problema) limite)))


;; Procura em Profundidade Limitada - GRAPH SEARCH
(defun pplg (problema limite)
	; [GRAPH-SEARCH] initialize the explored set to be empty
	(let ((conjunto-explorados (faz-conjunto-explorados))) ;explored set
		(labels ((pplg-aux (estado limite)
			(let ((accoes-possiveis nil) ; for code readability
					(estado-sucessor nil)  ; stores child nodes
			      (resultado nil)        ; stores recursive search result
			      (ocorreu-corte? nil))  ; set to T if cutoff ocurred
				; if problem.GOAL-TEST(node.STATE)
				(if (funcall (problema-solucao? problema) estado)
					; then return SOLUTION(NODE)
					(return-from pplg-aux estado))
				; else if limit = 0
				(if (zerop limite)
					; then return CUTOFF
					(return-from pplg-aux 'CORTE))
				; else
				; [GRAPH-SEARCH] add the node to the explored set
				(insere-conjunto-explorados conjunto-explorados estado)
				; foreach action in problem.ACTIONS(node.STATE) do
				(setf accoes-possiveis (funcall (problema-accoes problema) estado))
				(loop for accao in accoes-possiveis do
					; child <- CHILD-NODE(problem, node, action)
					(setf estado-sucessor (funcall (problema-resultado problema) estado accao))
					; [GRAPH-SEARCH] if child node not in explored set
					(if (not (procura-conjunto-explorados conjunto-explorados estado-sucessor))
						(progn
							; [GRAPH-SEARCH] then expand child
							; result <- RECURSIVE-DFS(child, problem, limit-1)
							(setf resultado (pplg-aux estado-sucessor (1- limite)))
							; if result == cutoff then cutoff_ocurred? <- true
							(if (eq resultado 'CORTE) (setf ocorreu-corte? t))
							; if result != failure then return result
							(if resultado (return-from pplg-aux resultado)))))
				; no solution found - return FAILURE
				ocorreu-corte?)))
			(pplg-aux (problema-estado-inicial problema) limite))))

;; plp.lisp
;; Procura em Profundidade Iterativa
;; Grupo 47

(defun ppia (problema)
	(let ((result nil))
		; for depth = 0 to infinity do
		(loop for profundidade from 0 do
			; result <- depth-limited-search(problem, depth)
			(setf result (ppla problema profundidade))
			; if result != cutoff then return result
			(if (not (eq result 'CORTE)) (return-from ppia result)))))


(defun ppig (problema)
	(let ((result nil))
		; for depth = 0 to infinity do
		(loop for profundidade from 0 do
			; result <- depth-limited-search(problem, depth)
			(setf result (pplg problema profundidade))
			; if result != cutoff then return result
			(if (not (eq result 'CORTE)) (return-from ppig result)))))


;; pg.lisp
;; Procura gananciosa
;; Grupo 47

(defun pga (problema heuristica)
  (let* ((estado-inicial (problema-estado-inicial problema))
		 (fila (list (make-node :estado estado-inicial
								:custo-caminho 0
								:custo-heuristica 0
								:custo 0)))
		 (no-actual nil)
		 (estado-actual nil)
		 (estados-sucessores nil)
		 (node-sucessor nil))	; estrutura do tipo "node" com estado + custo-caminho
	(loop
	  ; fila vazia?
	  (if (not fila) (return-from pga nil))

	  (setf no-actual (pop fila))
	  (setf estado-actual (node-estado no-actual))

	  ; if problem.GOAL-TEST(node.STATE)
	  (if (funcall (problema-solucao? problema) estado-actual) ; "este estado é uma solução para este problema?"
		; then return SOLUTION(node)
		(return-from pga estado-actual))	; yay!
	  ; else
	  ; foreach action in problem.ACTIONS(node.STATE) do
	  (setf estados-sucessores (funcall (problema-accoes problema) estado-actual))
	  (loop for sucessor in estados-sucessores  do
			; child <- CHILD-NODE(problem, node, action)
			(setf node-sucessor (make-node 	:estado (funcall (problema-resultado problema) estado-actual sucessor)
											;:custo-heuristica (funcall heuristica (funcall (problema-resultado problema) estado-actual sucessor))
										   	:custo (funcall heuristica (funcall (problema-resultado problema) estado-actual sucessor))))
			; if sucessor is not in queue, add it;
			; else if sucessor is in queue but with a higher path-cost, replace it with this one
			(if (< (node-custo node-sucessor) most-positive-fixnum)
				(setf fila (queue-add-node fila node-sucessor)))))))

(defun pgg (problema heuristica)
  (let* ((estado-inicial (problema-estado-inicial problema))
		 (fila (list (make-node :estado estado-inicial
								:custo-caminho 0
								:custo 0)))
		 (no-actual nil)
		 (estado-actual nil)
		 (estados-sucessores nil)
		 (node-sucessor nil)	; estrutura do tipo "node" com estado + custo-caminho
		 (conjunto-explorados (faz-conjunto-explorados))) ; conjunto de nos explorados
	(loop
	  ; fila vazia?
	  (if (not fila) (return-from pgg nil))

	  (setf no-actual (pop fila))
	  (setf estado-actual (node-estado no-actual))

	  ; if problem.GOAL-TEST(node.STATE)
	  (if (funcall (problema-solucao? problema) estado-actual) ; "este estado é uma solução para este problema?"
		; then return SOLUTION(node)
		(return-from pgg estado-actual))	; yay!
	  ; else
	  ; GRAPH SEARCH: add the node to the explored set
	  (insere-conjunto-explorados conjunto-explorados estado-actual)
	  ; foreach action in problem.ACTIONS(node.STATE) do
	  (setf estados-sucessores (funcall (problema-accoes problema) estado-actual))
	  (loop for sucessor in estados-sucessores  do
			; GRAPH SEARCH: do not repeat explored nodes
			(if (not (procura-conjunto-explorados conjunto-explorados sucessor))
			  (progn
				; child <- CHILD-NODE(problem, node, action)
				(setf node-sucessor (make-node :estado (funcall (problema-resultado problema) estado-actual sucessor)
											   :custo (funcall heuristica estado-actual)))
				; if sucessor is not in queue, add it;
				; else if sucessor is in queue but with a higher path-cost, replace it with this one
				(if (< (node-custo node-sucessor) most-positive-fixnum)
					(setf fila (queue-add-or-replace-node fila node-sucessor)))))))))
;; pa.lisp
;; Procura A*
;; Grupo 47

(defun pa*a (problema heuristica)
  (let* ((estado-inicial (problema-estado-inicial problema))
		 (fila (list (make-node :estado estado-inicial
								:custo-caminho 0
								:custo-heuristica 0
								:custo 0)))
		 (no-actual nil)
		 (estado-actual nil)
		 (estados-sucessores nil)
		 (node-sucessor nil))	; estrutura do tipo "node" com estado + custo-caminho
	(loop
	  ; fila vazia?
	  (if (not fila) (return-from pa*a nil))

	  (setf no-actual (pop fila))
	  (setf estado-actual (node-estado no-actual))

	  ; if problem.GOAL-TEST(node.STATE)
	  (if (funcall (problema-solucao? problema) estado-actual) ; "este estado é uma solução para este problema?"
		; then return SOLUTION(node)
		(return-from pa*a estado-actual))	; yay!
	  ; else
	  ; foreach action in problem.ACTIONS(node.STATE) do
	  (setf estados-sucessores (funcall (problema-accoes problema) estado-actual))
	  (loop for sucessor in estados-sucessores  do
			; child <- CHILD-NODE(problem, node, action)
			(setf node-sucessor (make-node 	:estado (funcall (problema-resultado problema) estado-actual sucessor)
										   	:custo-caminho 	(+ 	(node-custo-caminho no-actual)
													 			(funcall (faz-custo-caminho problema) estado-actual sucessor))
										   	:custo-heuristica (funcall heuristica (funcall (problema-resultado problema) estado-actual sucessor))
										   	:custo 			(+ 	(+ 	(node-custo-caminho no-actual)
													  				(funcall (faz-custo-caminho problema) estado-actual sucessor))
										   						(funcall heuristica (funcall (problema-resultado problema) estado-actual sucessor))
															)))
			; if sucessor is not in queue, add it;
			; else if sucessor is in queue but with a higher path-cost, replace it with this one
			(if (< (node-custo node-sucessor) most-positive-fixnum)
				(setf fila (queue-add-node fila node-sucessor)))))))

(defun pa*g (problema heuristica)
  (let* ((estado-inicial (problema-estado-inicial problema))
		 (fila (list (make-node :estado estado-inicial
								:custo-caminho 0
								:custo 0)))
		 (no-actual nil)
		 (estado-actual nil)
		 (estados-sucessores nil)
		 (node-sucessor nil)	; estrutura do tipo "a*node" com estado + custo-caminho
		 (conjunto-explorados (faz-conjunto-explorados))) ; conjunto de nos explorados
	(loop
	  ; fila vazia?
	  (if (not fila) (return-from pa*g nil))

	  (setf no-actual (pop fila))
	  (setf estado-actual (node-estado no-actual))

	  ; if problem.GOAL-TEST(node.STATE)
	  (if (funcall (problema-solucao? problema) estado-actual) ; "este estado é uma solução para este problema?"
		; then return SOLUTION(node)
		(return-from pa*g estado-actual))	; yay!
	  ; else
	  ; GRAPH SEARCH: add the node to the explored set
	  (insere-conjunto-explorados conjunto-explorados estado-actual)
	  ; foreach action in problem.ACTIONS(node.STATE) do
	  (setf estados-sucessores (funcall (problema-accoes problema) estado-actual))
	  (loop for sucessor in estados-sucessores  do
			; GRAPH SEARCH: do not repeat explored nodes
			(if (not (procura-conjunto-explorados conjunto-explorados sucessor))
			  (progn
				; child <- CHILD-NODE(problem, node, action)
				(setf node-sucessor (make-node :estado (funcall (problema-resultado problema) estado-actual sucessor)
											   :custo-caminho (+ 	(node-custo-caminho no-actual)
														 			(funcall (faz-custo-caminho problema) estado-actual sucessor))
											   :custo (+ 	(+ 	(node-custo-caminho no-actual)
														  		(funcall (faz-custo-caminho problema) estado-actual sucessor))
											   				(funcall heuristica estado-actual)) ))
				; if sucessor is not in queue, add it;
				; else if sucessor is in queue but with a higher path-cost, replace it with this one
				(if (< (node-custo node-sucessor) most-positive-fixnum)
					(setf fila (queue-add-or-replace-node fila node-sucessor)))))))))

(defun rbfs-actualiza-custo (custo-procura-anterior funcao-heuristica)
	(lambda (node)
		(setf (node-custo node)
			(max custo-procura-anterior
				(+ (node-custo-caminho node)
				   (funcall funcao-heuristica (node-estado node)))))))

(defun rbfs (problema funcao-heuristica)
	(let ((rbfs-resultado))
		(labels
			((rbfs-aux (node custo-limite)
				(let ((sucessores (make-queue))
				      (accoes-possiveis nil)
						(melhor-sucessor nil)
						(custo-melhor-alternativa nil)
						(resultado nil))
					; if problem.GOAL-TEST(node.STATE)
					(if (funcall (problema-solucao? problema) (node-estado node))
						; then return SOLUTION(node)
						(return-from rbfs-aux (node-estado node)))
					; for each action in problem.ACTIONS(node.STATE) do
					(setf accoes-possiveis (funcall (problema-accoes problema) (node-estado node)))
					(loop for accao in accoes-possiveis do
						; add CHILD-NODE(problem, node, action) into successors
						(setf melhor-sucessor (node-filho-com-heuristica problema node accao funcao-heuristica))
						(if (< (node-custo melhor-sucessor) most-positive-fixnum)
							(setf sucessores (queue-add-node sucessores melhor-sucessor))))
					; if successors is empty
					(if (queue-emptyp sucessores)
						; then return (failure, +infinity)
						(return-from rbfs-aux (cons :failure most-positive-fixnum)))
					; for each s in successors do
					(loop for sucessor in sucessores do
						; s.f <- max(s.g+s.h, node.f)
						(aplica-funcao-elementos sucessores (rbfs-actualiza-custo (node-custo node) funcao-heuristica)))
					(loop
						; best <- the lowest f-value node in successors
						(setf melhor-sucessor (pop sucessores))
						; if best.f > f_limit
						(if (> (node-custo node) custo-limite)
							; then return (failure, best.f)
							(return-from rbfs-aux (cons :failure (node-custo node))))
						; alternative <- the second lowest f-value among successors
						(if (queue-emptyp sucessores)
							; pode nao haver alternativa!
							(setf custo-melhor-alternativa custo-limite)
							(setf custo-melhor-alternativa (node-custo (queue-first sucessores))))
						; (result, best.f) <- RBFS(problem, best, min(f_limit, alternativa))
						(setf resultado (rbfs-aux melhor-sucessor (min custo-limite custo-melhor-alternativa)))
						(setf (node-custo melhor-sucessor) (cdr resultado))
						; if result != failure
						(if (not (equal (car resultado) :failure))
							; then return result
							(return-from rbfs-aux resultado))
						; voltar a inserir o "best" na fila successors
						(if (< (node-custo melhor-sucessor) most-positive-fixnum)
							(setf sucessores (queue-add-node sucessores melhor-sucessor)))))))
			; return RBFS(problem, MAKE-NODE(problem.INITIAL-STATE), +infinity)
			(setf rbfs-resultado (rbfs-aux (node-inicial problema) most-positive-fixnum))
			(if (equal (car rbfs-resultado) :failure)
				'FALHOU
				rbfs-resultado))))
;;; csp.lisp
;;;
;;; Estrutura para armazenar variaveis, dominios e restricoes
;;;

; "forward declaration"
(defun imprime-dominio (&rest rest) (declare (ignore rest)))
(defun imprime-restricao-variavel (&rest rest) (declare (ignore rest)))
(defun imprime-variavel (&rest rest) (declare (ignore rest)))
(defun imprime-atribuicao (&rest rest) (declare (ignore rest)))
(defun imprime-csp (&rest rest) (declare (ignore rest)))
(defun imprime-valor (&rest rest) (declare (ignore rest)))
(defun valor-valor (&rest rest) (declare (ignore rest)))
(defun imprime-restricao (&rest rest) (declare (ignore rest)))

; Estrutura CSP - representa um CSP baseado numa lista de tarefas
; variaveis: lista de variaveis
; dominios: lista de dominios para as variaveis
; restricoes: lista de restricoes
(defstruct (csp (:print-function imprime-csp)) variaveis dominios restricoes)

(defun imprime-csp (csp output-stream depth)
	(declare (ignore depth))
	(let ((strdivision "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"))
		(format output-stream "~%@@@@@@@@@@@@@~%")
		(format output-stream
			"@@@ [CSP] @@@ ~%~A~%VARIAVEIS~%~S: ~%~A~%DOMINIO~%~S ~%~A~%RESTRICOES~%~S~%~A"
			strdivision (csp-variaveis csp)
			strdivision (csp-dominios csp)
			strdivision (csp-restricoes csp)
			strdivision)))

; Estrutura CSP-variavel - representa uma variavel
; identificador: identificador da variavel
; dominio: referencia para o dominio da variavel
; restricoes: lista de referencias para as restricoes as quais a variavel faz parte
(defstruct (variavel (:print-function imprime-variavel)) identificador valor dominio restricoes)

(defun imprime-variavel (variavel output-stream depth)
	(let ((spacer ""))
		(loop for i from 0 to depth do
			(setf spacer (concatenate 'string spacer " ")))
		(format output-stream "~%~A[VARIAVEL ~S]" spacer (variavel-identificador variavel))
		(format output-stream " Valor: ~S" (variavel-valor variavel))
		(format output-stream "~A~A" spacer (variavel-dominio variavel))
		(format output-stream "~%~ARESTRICOES: ~S" spacer (list-length (variavel-restricoes variavel)))))

; Estrutura CSP-atribuicao - representa a atribuicao de um valor a uma variavel
; variavel: referencia para a variavel
; valor: valor atribuido a variavel
(defstruct (atribuicao (:print-function imprime-atribuicao)) variavel valor)

(defun imprime-atribuicao (atribuicao output-stream depth)
	(format output-stream "~%")
	(loop for i from 0 to depth do
		(format output-stream "  "))
	(format output-stream "ATRIBUICAO VARIAVEL ~S ~S~%" (variavel-identificador (atribuicao-variavel atribuicao)) (atribuicao-valor atribuicao)))


; Estrutura CSP-restricao-variavel - representa uma restricao da propria variavel
; valor: valor atribuido a variavel
; atribuicao-outra-variavel: atribuicao a outra variavel que causa conflitos
(defstruct (restricao-variavel (:print-function imprime-restricao-variavel)) valor atribuicao-outra-variavel)

(defun imprime-restricao-variavel (restricao-variavel output-stream depth)
	(format output-stream "~%")
	(loop for i from 0 to depth do
		(format output-stream "  "))
	(format output-stream "RESTRICAO-VARIAVEL Valor: ~S Variavel-r: ~S Valor-r: ~S" 
		(valor-valor (restricao-variavel-valor restricao-variavel))
		(variavel-identificador (atribuicao-variavel (restricao-variavel-atribuicao-outra-variavel restricao-variavel)))
		(valor-valor (atribuicao-valor (restricao-variavel-atribuicao-outra-variavel restricao-variavel)))))

; Estrutura CSP-dominio - representa o dominio de uma variavel
; valores: valores que a variavel pode tomar
(defstruct (dominio (:print-function imprime-dominio)) variavel valores)

(defun imprime-dominio (dominio output-stream depth)
	(format output-stream "~%")
	(loop for i from 0 to depth do
		(format output-stream "  "))
	(format output-stream "DOMINIO VAR ~S: ~S" (variavel-identificador (dominio-variavel dominio)) (dominio-valores dominio)))

; Estrutura CSP-dominio-valores - representa um valor no dominio de uma variavel
; valor: um valor que pode ser atribuido a variavel
; restricoes: lista das restricoes associadas a atribuicao daquele valor a variavel
(defstruct (valor (:print-function imprime-valor)) valor restricoes)

(defun imprime-valor (valor output-stream depth)
	;(format output-stream "~%")
	(loop for i from 0 to depth do
		(format output-stream "  "))
	(format output-stream "VALOR ~S, Restricoes: ~S" (valor-valor valor) (list-length (valor-restricoes valor))))


; Estrutura CSP-restricao - representa uma restricao binaria
; atribuicao1: uma atribuicao
; atribuicao2: outra atribuicao
(defstruct (restricao (:print-function imprime-restricao)) atribuicao1 atribuicao2)

(defun imprime-restricao (restricao output-stream depth)
	(format output-stream "~%")
	(loop for i from 0 to depth do
		(format output-stream "  "))
	(format output-stream "VAR~S VALOR ~S"
		(variavel-identificador (atribuicao-variavel (restricao-atribuicao1 restricao)))
		(atribuicao-valor (restricao-atribuicao1 restricao)))
	(format output-stream "~%")
	(loop for i from 0 to depth do
		(format output-stream "  "))
	(format output-stream "VAR~S VALOR ~S"
		(variavel-identificador (atribuicao-variavel (restricao-atribuicao2 restricao)))
		(atribuicao-valor (restricao-atribuicao2 restricao))))

;;;;;;;;;;;;;;;;;
;;; variaveis ;;;
;;;;;;;;;;;;;;;;;
(defun faz-variaveis (tarefas)
	(loop for i from 0 to (1- (list-length tarefas))
		collect (make-variavel
			:identificador i)))

;;;;;;;;;;;;;;;;
;;; dominios ;;;
;;;;;;;;;;;;;;;;
(defun faz-dominios (variaveis tarefas)
	(loop for variavel in variaveis
	      for tarefa   in tarefas   collect
		(setf (variavel-dominio variavel)
			(make-dominio
				:variavel variavel
				:valores
					(loop for valor in tarefa collect
						(make-valor
							:valor valor
							:restricoes '()))))))

;;;;;;;;;;;;;;;;;;
;;; restricoes ;;;
;;;;;;;;;;;;;;;;;;

; adiciona uma restricao ao CSP
; insere restricao numa lista de conflitos generica e nas listas de conflitos das
; variaveis e valores em questao (5 sitios no total)
(defun adiciona-restricao (variavel1 valor1 variavel2 valor2)
	; adiciona restricao a variavel1
	(push (make-restricao-variavel
		:valor valor1
		:atribuicao-outra-variavel
			(make-atribuicao
				:variavel variavel2
				:valor valor2)) (variavel-restricoes variavel1))
	; adiciona restricao a variavel2
	(push (make-restricao-variavel
		:valor valor2
		:atribuicao-outra-variavel
			(make-atribuicao
				:variavel variavel1
				:valor valor1)) (variavel-restricoes variavel2))
	; adiciona restricao a valor1
	(push (make-atribuicao
		:variavel variavel2
		:valor valor2) (valor-restricoes valor1))
	; adiciona restricao a valor2
	(push (make-atribuicao
		:variavel variavel1
		:valor valor1) (valor-restricoes valor2))
	; adiciona restricao a lista de conflitos
	(make-restricao
		:atribuicao1
			(make-atribuicao
				:variavel variavel1
				:valor    valor1)
		:atribuicao2
			(make-atribuicao
				:variavel variavel2
				:valor    valor2))
)

; devolve a lista de conflitos entre duas variaveis
(defun conflitos-entre-duas-variaveis (variavel1 variavel2)
	(let ((conflitos nil)
	      (valores-dominio1 (dominio-valores (variavel-dominio variavel1)))
	      (valores-dominio2 (dominio-valores (variavel-dominio variavel2))))
		(loop for valor1 in valores-dominio1 do
			;(format t "~% - valor1: ~S" valor1)
			(loop for valor2 in valores-dominio2 do
				;(format t "~%   - valor2: ~S" valor2)
				(if (conflito-listas-subtarefasp (valor-valor valor1) (valor-valor valor2))
					(push (adiciona-restricao variavel1 valor1 variavel2 valor2) conflitos))))
		conflitos))

; devolve uma lista de conflitos de uma variavel
; com base uma lista de variaveis para comparacao
(defun conflitos-variavel (variavel variaveis-restantes)
	(loop for variavel-restante in variaveis-restantes collect
		(conflitos-entre-duas-variaveis variavel variavel-restante)))

; devolve a lista de restricoes para o CSP
(defun faz-restricoes (variaveis)
	(let ((variaveis-restantes variaveis))
		(loop for variavel in variaveis collect
			(progn
				(pop variaveis-restantes)
				(conflitos-variavel variavel variaveis-restantes)))))

;;;;;;;;;;;
;;; CSP ;;;
;;;;;;;;;;;
(defun faz-csp (tarefas)
	(let* ((variaveis (faz-variaveis tarefas))
	       (dominios (faz-dominios variaveis tarefas))
	       (restricoes (flatten (faz-restricoes variaveis))))
	(make-csp
		:variaveis  variaveis
		:dominios   dominios
		:restricoes restricoes)))


;;; psr.lisp
;;;
;;; procura de satisfacao com restricoes

; verifica se uma determinada atribuicao e completa
; i.e. se e solucao
(defun psr-atribuicao-completa? (atribuicoes csp)
	;(format t "~%atribuicoes: ~S" (list-length atribuicoes))
	;(format t "~%numero variaveis: ~S" (list-length (csp-variaveis csp)))
	(eql (list-length atribuicoes) (list-length (csp-variaveis csp))))

; devolve a lista de variaveis do csp nao existentes na lista de atribuicoes
(defun variaveis-nao-atribuidas (atribuicoes csp)
	(set-difference
		(csp-variaveis csp)
		atribuicoes
		:test (lambda (a b) (eq (variavel-identificador a) (variavel-identificador b)))))


; devolve a proxima variavel a qual nao foi atribuida nenhum valor
; >> HEURISTICA DO MAIOR GRAU
(defun psr-prox-variavel-nao-atribuida (atribuicoes csp)
	(let* ((variaveis-nao-atribuidas (variaveis-nao-atribuidas atribuicoes csp))
	       (prox-variavel (first variaveis-nao-atribuidas)))
		(loop for variavel in (rest variaveis-nao-atribuidas) do
			(if (> (list-length (variavel-restricoes variavel))
			       (list-length (variavel-restricoes prox-variavel)))
				(setf prox-variavel variavel)))
		prox-variavel))

; devolve a lista de valores que podem ser atribuidos a uma variavel
; >> HEURISTICA DO VALOR MENOS RESTRITIVO
(defun lista-valores-variavel (variavel)
	(sort (dominio-valores (variavel-dominio variavel)) #'<
		:key (lambda (valor) (list-length (valor-restricoes valor)))))

; verifica se a atribuicao a uma variavel nao viola restricoes
(defun atribuicao-consistente? (variavel valor)
	(loop for restricao in (variavel-restricoes variavel) do
		(if (eq (restricao-variavel-valor restricao) valor)
			(if (eq (atribuicao-valor (restricao-variavel-atribuicao-outra-variavel restricao))
			        (variavel-valor (atribuicao-variavel (restricao-variavel-atribuicao-outra-variavel restricao))))
				(return-from atribuicao-consistente? nil))))
	T)

;;;;;;;;;;;;;;;;;;;;;;
;; CSP BACKTRACKING ;;
;;;;;;;;;;;;;;;;;;;;;;
(defun psr-backtrack (atribuicoes csp)
	; if assignment is complete
	(if (psr-atribuicao-completa? atribuicoes csp)
		; then return assignment
		(return-from psr-backtrack atribuicoes))
		; var <- select-unassigned-variable(csp)
	(let ((var (psr-prox-variavel-nao-atribuida atribuicoes csp))
	      (resultado nil))
		; for each value in ORDER-DOMAIN-VALUES(var,assignment,csp) do
		(loop for valor in (lista-valores-variavel var) do
			;(print valor)
			; if value is consistent with assignment then
			(if (atribuicao-consistente? var valor)
				(progn
					; add {var=value} to assignment
					(setf (variavel-valor var) valor)
					;
					; INFERENCES GO HERE
					;
					(push var atribuicoes)
					(setf resultado (psr-backtrack atribuicoes csp))
					(if (not (equal resultado :FALHOU))
						(return-from psr-backtrack resultado))
					(pop atribuicoes)))))
	:FALHOU)

(defun converte-resposta (resposta)
	(if (equal resposta :FALHOU)
		resposta
		(loop for variavel in resposta collect
			(valor-valor (variavel-valor variavel)))))

(defun ordena-resposta (resposta)
	(sort resposta #'> :key #'variavel-identificador))

; Procura de satisfacao de restricoes
(defun psr (csp)
	(converte-resposta (ordena-resposta (psr-backtrack '() csp))))


