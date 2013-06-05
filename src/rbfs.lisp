
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
