;; ppp.lisp
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
