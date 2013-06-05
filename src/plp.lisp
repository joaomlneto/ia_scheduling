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
	  (plpg-aux (list (problema-estado-inicial problema)) ))))