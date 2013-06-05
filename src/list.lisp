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
