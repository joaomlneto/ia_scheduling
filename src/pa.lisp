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
