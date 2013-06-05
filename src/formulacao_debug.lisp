;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formulacao de Problemas para Debug ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun faz-solucao?-debug (fn-solucao)
	#'(lambda (estado)
		(let ((resultado (funcall fn-solucao estado)))
			(if resultado
				(format t " -> ESTADO FINAL!"))
			resultado)))

(defun faz-accoes-debug (fn-accoes)
	#'(lambda (estado)
		(let ((resultado (funcall fn-accoes estado)))
			(format t "~%Numero de accoes possiveis: ~S" (list-length resultado))
			resultado)))

(defun faz-resultado-debug (fn-resultado)
	#'(lambda (estado accao)
		(let ((resultado (funcall fn-resultado estado accao)))
			(format t "~%ESTADO: ~S" resultado)
			resultado)))

(defun faz-custo-caminho-debug (fn-custo-caminho)
	#'(lambda (estado accao)
		(let ((resultado (funcall fn-custo-caminho estado accao)))
			(format t "~%CUSTO: ~S" resultado)
			resultado)))

(defun formulacao-debug (problema)
	(format t "~%ESTADO INICIAL: ~S" (problema-estado-inicial problema))
	(make-problema
		:estado-inicial (problema-estado-inicial problema)
		:solucao?       (faz-solucao?-debug      (problema-solucao?      problema))
		:accoes         (faz-accoes-debug        (problema-accoes        problema))
		:resultado      (faz-resultado-debug     (problema-resultado     problema))
		:custo-caminho  (faz-custo-caminho-debug (problema-custo-caminho problema))))

