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
