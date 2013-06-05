;;; psr.lisp
;;;
;;; procura de satisfacao com restricoes

; verifica se uma determinada atribuicao e completa
; i.e. se e solucao
(defun psr-atribuicao-completa? (atribuicoes csp)
	;(format t "~%atribuicoes: ~S" (list-length atribuicoes))
	;(format t "~%numero variaveis: ~S" (list-length (csp-variaveis csp)))
	(eql (list-length atribuicoes) (list-length (csp-variaveis csp))))

; devolve a lista de variaveis do csp nao existentes na lista de atribuicoes
(defun variaveis-nao-atribuidas (atribuicoes csp)
	(set-difference
		(csp-variaveis csp)
		atribuicoes
		:test (lambda (a b) (eq (variavel-identificador a) (variavel-identificador b)))))


; devolve a proxima variavel a qual nao foi atribuida nenhum valor
; >> HEURISTICA DO MAIOR GRAU
(defun psr-prox-variavel-nao-atribuida (atribuicoes csp)
	(let* ((variaveis-nao-atribuidas (variaveis-nao-atribuidas atribuicoes csp))
	       (prox-variavel (first variaveis-nao-atribuidas)))
		(loop for variavel in (rest variaveis-nao-atribuidas) do
			(if (> (list-length (variavel-restricoes variavel))
			       (list-length (variavel-restricoes prox-variavel)))
				(setf prox-variavel variavel)))
		prox-variavel))

; devolve a lista de valores que podem ser atribuidos a uma variavel
; >> HEURISTICA DO VALOR MENOS RESTRITIVO
(defun lista-valores-variavel (variavel)
	(sort (dominio-valores (variavel-dominio variavel)) #'<
		:key (lambda (valor) (list-length (valor-restricoes valor)))))

; verifica se a atribuicao a uma variavel nao viola restricoes
(defun atribuicao-consistente? (variavel valor)
	(loop for restricao in (variavel-restricoes variavel) do
		(if (eq (restricao-variavel-valor restricao) valor)
			(if (eq (atribuicao-valor (restricao-variavel-atribuicao-outra-variavel restricao))
			        (variavel-valor (atribuicao-variavel (restricao-variavel-atribuicao-outra-variavel restricao))))
				(return-from atribuicao-consistente? nil))))
	T)

;;;;;;;;;;;;;;;;;;;;;;
;; CSP BACKTRACKING ;;
;;;;;;;;;;;;;;;;;;;;;;
(defun psr-backtrack (atribuicoes csp)
	; if assignment is complete
	(if (psr-atribuicao-completa? atribuicoes csp)
		; then return assignment
		(return-from psr-backtrack atribuicoes))
		; var <- select-unassigned-variable(csp)
	(let ((var (psr-prox-variavel-nao-atribuida atribuicoes csp))
	      (resultado nil))
		; for each value in ORDER-DOMAIN-VALUES(var,assignment,csp) do
		(loop for valor in (lista-valores-variavel var) do
			;(print valor)
			; if value is consistent with assignment then
			(if (atribuicao-consistente? var valor)
				(progn
					; add {var=value} to assignment
					(setf (variavel-valor var) valor)
					;
					; INFERENCES GO HERE
					;
					(push var atribuicoes)
					(setf resultado (psr-backtrack atribuicoes csp))
					(if (not (equal resultado :FALHOU))
						(return-from psr-backtrack resultado))
					(pop atribuicoes)))))
	:FALHOU)

(defun converte-resposta (resposta)
	(if (equal resposta :FALHOU)
		resposta
		(loop for variavel in resposta collect
			(valor-valor (variavel-valor variavel)))))

(defun ordena-resposta (resposta)
	(sort resposta #'> :key #'variavel-identificador))

; Procura de satisfacao de restricoes
(defun psr (csp)
	(converte-resposta (ordena-resposta (psr-backtrack '() csp))))


