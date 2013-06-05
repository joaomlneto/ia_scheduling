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


