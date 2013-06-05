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


