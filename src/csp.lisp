;;; csp.lisp
;;;
;;; Estrutura para armazenar variaveis, dominios e restricoes
;;;

; "forward declaration"
(defun imprime-dominio (&rest rest) (declare (ignore rest)))
(defun imprime-restricao-variavel (&rest rest) (declare (ignore rest)))
(defun imprime-variavel (&rest rest) (declare (ignore rest)))
(defun imprime-atribuicao (&rest rest) (declare (ignore rest)))
(defun imprime-csp (&rest rest) (declare (ignore rest)))
(defun imprime-valor (&rest rest) (declare (ignore rest)))
(defun valor-valor (&rest rest) (declare (ignore rest)))
(defun imprime-restricao (&rest rest) (declare (ignore rest)))

; Estrutura CSP - representa um CSP baseado numa lista de tarefas
; variaveis: lista de variaveis
; dominios: lista de dominios para as variaveis
; restricoes: lista de restricoes
(defstruct (csp (:print-function imprime-csp)) variaveis dominios restricoes)

(defun imprime-csp (csp output-stream depth)
	(declare (ignore depth))
	(let ((strdivision "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"))
		(format output-stream "~%@@@@@@@@@@@@@~%")
		(format output-stream
			"@@@ [CSP] @@@ ~%~A~%VARIAVEIS~%~S: ~%~A~%DOMINIO~%~S ~%~A~%RESTRICOES~%~S~%~A"
			strdivision (csp-variaveis csp)
			strdivision (csp-dominios csp)
			strdivision (csp-restricoes csp)
			strdivision)))

; Estrutura CSP-variavel - representa uma variavel
; identificador: identificador da variavel
; dominio: referencia para o dominio da variavel
; restricoes: lista de referencias para as restricoes as quais a variavel faz parte
(defstruct (variavel (:print-function imprime-variavel)) identificador valor dominio restricoes)

(defun imprime-variavel (variavel output-stream depth)
	(let ((spacer ""))
		(loop for i from 0 to depth do
			(setf spacer (concatenate 'string spacer " ")))
		(format output-stream "~%~A[VARIAVEL ~S]" spacer (variavel-identificador variavel))
		(format output-stream " Valor: ~S" (variavel-valor variavel))
		(format output-stream "~A~A" spacer (variavel-dominio variavel))
		(format output-stream "~%~ARESTRICOES: ~S" spacer (list-length (variavel-restricoes variavel)))))

; Estrutura CSP-atribuicao - representa a atribuicao de um valor a uma variavel
; variavel: referencia para a variavel
; valor: valor atribuido a variavel
(defstruct (atribuicao (:print-function imprime-atribuicao)) variavel valor)

(defun imprime-atribuicao (atribuicao output-stream depth)
	(format output-stream "~%")
	(loop for i from 0 to depth do
		(format output-stream "  "))
	(format output-stream "ATRIBUICAO VARIAVEL ~S ~S~%" (variavel-identificador (atribuicao-variavel atribuicao)) (atribuicao-valor atribuicao)))


; Estrutura CSP-restricao-variavel - representa uma restricao da propria variavel
; valor: valor atribuido a variavel
; atribuicao-outra-variavel: atribuicao a outra variavel que causa conflitos
(defstruct (restricao-variavel (:print-function imprime-restricao-variavel)) valor atribuicao-outra-variavel)

(defun imprime-restricao-variavel (restricao-variavel output-stream depth)
	(format output-stream "~%")
	(loop for i from 0 to depth do
		(format output-stream "  "))
	(format output-stream "RESTRICAO-VARIAVEL Valor: ~S Variavel-r: ~S Valor-r: ~S" 
		(valor-valor (restricao-variavel-valor restricao-variavel))
		(variavel-identificador (atribuicao-variavel (restricao-variavel-atribuicao-outra-variavel restricao-variavel)))
		(valor-valor (atribuicao-valor (restricao-variavel-atribuicao-outra-variavel restricao-variavel)))))

; Estrutura CSP-dominio - representa o dominio de uma variavel
; valores: valores que a variavel pode tomar
(defstruct (dominio (:print-function imprime-dominio)) variavel valores)

(defun imprime-dominio (dominio output-stream depth)
	(format output-stream "~%")
	(loop for i from 0 to depth do
		(format output-stream "  "))
	(format output-stream "DOMINIO VAR ~S: ~S" (variavel-identificador (dominio-variavel dominio)) (dominio-valores dominio)))

; Estrutura CSP-dominio-valores - representa um valor no dominio de uma variavel
; valor: um valor que pode ser atribuido a variavel
; restricoes: lista das restricoes associadas a atribuicao daquele valor a variavel
(defstruct (valor (:print-function imprime-valor)) valor restricoes)

(defun imprime-valor (valor output-stream depth)
	;(format output-stream "~%")
	(loop for i from 0 to depth do
		(format output-stream "  "))
	(format output-stream "VALOR ~S, Restricoes: ~S" (valor-valor valor) (list-length (valor-restricoes valor))))


; Estrutura CSP-restricao - representa uma restricao binaria
; atribuicao1: uma atribuicao
; atribuicao2: outra atribuicao
(defstruct (restricao (:print-function imprime-restricao)) atribuicao1 atribuicao2)

(defun imprime-restricao (restricao output-stream depth)
	(format output-stream "~%")
	(loop for i from 0 to depth do
		(format output-stream "  "))
	(format output-stream "VAR~S VALOR ~S"
		(variavel-identificador (atribuicao-variavel (restricao-atribuicao1 restricao)))
		(atribuicao-valor (restricao-atribuicao1 restricao)))
	(format output-stream "~%")
	(loop for i from 0 to depth do
		(format output-stream "  "))
	(format output-stream "VAR~S VALOR ~S"
		(variavel-identificador (atribuicao-variavel (restricao-atribuicao2 restricao)))
		(atribuicao-valor (restricao-atribuicao2 restricao))))

;;;;;;;;;;;;;;;;;
;;; variaveis ;;;
;;;;;;;;;;;;;;;;;
(defun faz-variaveis (tarefas)
	(loop for i from 0 to (1- (list-length tarefas))
		collect (make-variavel
			:identificador i)))

;;;;;;;;;;;;;;;;
;;; dominios ;;;
;;;;;;;;;;;;;;;;
(defun faz-dominios (variaveis tarefas)
	(loop for variavel in variaveis
	      for tarefa   in tarefas   collect
		(setf (variavel-dominio variavel)
			(make-dominio
				:variavel variavel
				:valores
					(loop for valor in tarefa collect
						(make-valor
							:valor valor
							:restricoes '()))))))

;;;;;;;;;;;;;;;;;;
;;; restricoes ;;;
;;;;;;;;;;;;;;;;;;

; adiciona uma restricao ao CSP
; insere restricao numa lista de conflitos generica e nas listas de conflitos das
; variaveis e valores em questao (5 sitios no total)
(defun adiciona-restricao (variavel1 valor1 variavel2 valor2)
	; adiciona restricao a variavel1
	(push (make-restricao-variavel
		:valor valor1
		:atribuicao-outra-variavel
			(make-atribuicao
				:variavel variavel2
				:valor valor2)) (variavel-restricoes variavel1))
	; adiciona restricao a variavel2
	(push (make-restricao-variavel
		:valor valor2
		:atribuicao-outra-variavel
			(make-atribuicao
				:variavel variavel1
				:valor valor1)) (variavel-restricoes variavel2))
	; adiciona restricao a valor1
	(push (make-atribuicao
		:variavel variavel2
		:valor valor2) (valor-restricoes valor1))
	; adiciona restricao a valor2
	(push (make-atribuicao
		:variavel variavel1
		:valor valor1) (valor-restricoes valor2))
	; adiciona restricao a lista de conflitos
	(make-restricao
		:atribuicao1
			(make-atribuicao
				:variavel variavel1
				:valor    valor1)
		:atribuicao2
			(make-atribuicao
				:variavel variavel2
				:valor    valor2))
)

; devolve a lista de conflitos entre duas variaveis
(defun conflitos-entre-duas-variaveis (variavel1 variavel2)
	(let ((conflitos nil)
	      (valores-dominio1 (dominio-valores (variavel-dominio variavel1)))
	      (valores-dominio2 (dominio-valores (variavel-dominio variavel2))))
		(loop for valor1 in valores-dominio1 do
			;(format t "~% - valor1: ~S" valor1)
			(loop for valor2 in valores-dominio2 do
				;(format t "~%   - valor2: ~S" valor2)
				(if (conflito-listas-subtarefasp (valor-valor valor1) (valor-valor valor2))
					(push (adiciona-restricao variavel1 valor1 variavel2 valor2) conflitos))))
		conflitos))

; devolve uma lista de conflitos de uma variavel
; com base uma lista de variaveis para comparacao
(defun conflitos-variavel (variavel variaveis-restantes)
	(loop for variavel-restante in variaveis-restantes collect
		(conflitos-entre-duas-variaveis variavel variavel-restante)))

; devolve a lista de restricoes para o CSP
(defun faz-restricoes (variaveis)
	(let ((variaveis-restantes variaveis))
		(loop for variavel in variaveis collect
			(progn
				(pop variaveis-restantes)
				(conflitos-variavel variavel variaveis-restantes)))))

;;;;;;;;;;;
;;; CSP ;;;
;;;;;;;;;;;
(defun faz-csp (tarefas)
	(let* ((variaveis (faz-variaveis tarefas))
	       (dominios (faz-dominios variaveis tarefas))
	       (restricoes (flatten (faz-restricoes variaveis))))
	(make-csp
		:variaveis  variaveis
		:dominios   dominios
		:restricoes restricoes)))


