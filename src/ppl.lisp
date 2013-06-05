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

