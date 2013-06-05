;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;; Alguns problemas para testar ;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  - [test-problema-1]
;; Este e o problema do enunciado
;;
;; SOLUCAO OPTIMA:
;; (((1 9 T121) (1 11 T132)) ((1 9 T121) (1 10 T122)))
;;
;; Nota: Todas as outras procuras nao-optimas dao o mesmo resultado
;;
(defvar *test-tarefas-1*
	(list
		(list ; TAREFA #1
			(list ; - Tarefa #1 - alternativa #1
				(faz-subtarefa :dia 1 :hora 9  :id 't121) ;optimizacao--1
				(faz-subtarefa :dia 1 :hora 10 :id 't122))
			(list ; - Tarefa #1 - alternativa #2
				(faz-subtarefa :dia 2 :hora 10 :id 't123)
				(faz-subtarefa :dia 2 :hora 11 :id 't124))) ;conflito--1
		(list ; TAREFA #2
			(list ; - Tarefa #2 - alternativa #1
				(faz-subtarefa :dia 1 :hora 9  :id 't121) ;optimizacao--1
				(faz-subtarefa :dia 1 :hora 11 :id 't132))
			(list ; - Tarefa #2 - alternativa #2
				(faz-subtarefa :dia 2 :hora 11 :id 't133)
				(faz-subtarefa :dia 2 :hora 12 :id 't134))))) ;conflito--1
			
;Depois de escolher uma alternativa para a tarefa1 e uma alternativa para a tarefa2
;esgotam-se as alternativas possivais pois entram todas em comflito
(defvar *test-tarefas-c1*
	(list
		(list ; TAREFA #1
			(list ; - Tarefa #1 - alternativa #1
				(faz-subtarefa :dia 1 :hora 9  :id 't121))
			(list ; - Tarefa #1 - alternativa #2
				(faz-subtarefa :dia 2 :hora 10 :id 't123)))
		(list ; TAREFA #2
			(list ; - Tarefa #2 - alternativa #1
				(faz-subtarefa :dia 1 :hora 9  :id 't124))
			(list ; - Tarefa #2 - alternativa #2
				(faz-subtarefa :dia 2 :hora 10 :id 't125)))
		(list ; TAREFA #3
			(list ; - Tarefa #3 - alternativa #1
				(faz-subtarefa :dia 1 :hora 9  :id 't126))
			(list ; - Tarefa #3 - alternativa #2
				(faz-subtarefa :dia 2 :hora 10 :id 't127)))))
		
;Depois de escolher uma alternativa para a tarefa1 nao e possivel escolher mais
;nenhuma pois entram todas em conflito
(defvar *test-tarefas-c2*
	(list
		(list ; TAREFA #1
			(list ; - Tarefa #1 - alternativa #1
				(faz-subtarefa :dia 1 :hora 9  :id 't121mario1))
			(list ; - Tarefa #1 - alternativa #2
				(faz-subtarefa :dia 1 :hora 9 :id 't122mario2))
			(list ; - Tarefa #1 - alternativa #2
				(faz-subtarefa :dia 1 :hora 9 :id 't123mario3)))
		(list ; TAREFA #2
			(list ; - Tarefa #2 - alternativa #1
				(faz-subtarefa :dia 1 :hora 9  :id 't131))
			(list ; - Tarefa #2 - alternativa #2
				(faz-subtarefa :dia 1 :hora 9 :id 't132))
			(list ; - Tarefa #2 - alternativa #2
				(faz-subtarefa :dia 1 :hora 9 :id 't133)))
		(list ; TAREFA #3
			(list ; - Tarefa #3 - alternativa #1
				(faz-subtarefa :dia 1 :hora 9  :id 't141))
			(list ; - Tarefa #3 - alternativa #2
				(faz-subtarefa :dia 1 :hora 9 :id 't142)))
		(list ; TAREFA #4
			(list ; - Tarefa #4 - alternativa #1
				(faz-subtarefa :dia 1 :hora 9  :id 't151))
			(list ; - Tarefa #4 - alternativa #2
				(faz-subtarefa :dia 1 :hora 9 :id 't152)))))
				
(defvar *test-tarefas-c4*
	(list
		(list ; TAREFA #1
			(list ; - Tarefa #1 - alternativa #1
				(faz-subtarefa :dia 1 :hora 9  :id 't121))
			(list ; - Tarefa #1 - alternativa #2
				(faz-subtarefa :dia 1 :hora 10 :id 't122))
			(list ; - Tarefa #1 - alternativa #2
				(faz-subtarefa :dia 1 :hora 11 :id 't123)))
		(list ; TAREFA #2
			(list ; - Tarefa #2 - alternativa #1
				(faz-subtarefa :dia 1 :hora 12  :id 't131))
			(list ; - Tarefa #2 - alternativa #2
				(faz-subtarefa :dia 1 :hora 9 :id 't132))
			(list ; - Tarefa #2 - alternativa #2
				(faz-subtarefa :dia 1 :hora 13 :id 't133)))
		(list ; TAREFA #3
			(list ; - Tarefa #3 - alternativa #1
				(faz-subtarefa :dia 1 :hora 14  :id 't141))
			(list ; - Tarefa #3 - alternativa #2
				(faz-subtarefa :dia 1 :hora 15 :id 't142)))
		(list ; TAREFA #4
			(list ; - Tarefa #4 - alternativa #1
				(faz-subtarefa :dia 1 :hora 9  :id 't151))
			(list ; - Tarefa #4 - alternativa #2
				(faz-subtarefa :dia 1 :hora 9 :id 't152)))))
				
;Nada entra em conflito todas as alternativas são possiveis e nao ha solucao
;optima
(defvar *test-tarefas-c3*
	(list
		(list ; TAREFA #1
			(list ; - Tarefa #1 - alternativa #1
				(faz-subtarefa :dia 1 :hora 9  :id 't121)
				(faz-subtarefa :dia 1 :hora 10 :id 't122))
			(list ; - Tarefa #1 - alternativa #2
				(faz-subtarefa :dia 2 :hora 11 :id 't123)
				(faz-subtarefa :dia 2 :hora 12 :id 't124)))
		(list ; TAREFA #2
			(list ; - Tarefa #2 - alternativa #1
				(faz-subtarefa :dia 1 :hora 13  :id 't131)
				(faz-subtarefa :dia 1 :hora 14 :id 't132))
			(list ; - Tarefa #2 - alternativa #2
				(faz-subtarefa :dia 2 :hora 15 :id 't133)
				(faz-subtarefa :dia 2 :hora 16 :id 't134)))))

; Problemas do anuncio de dia 22/novembro
(defparameter *ex5*
  (list ;de tarefas
   (list (list (faz-subtarefa :dia 1 :hora 9 :id 't1))
	 (list (faz-subtarefa :dia 2 :hora 9 :id 't2))
	 (list (faz-subtarefa :dia 3 :hora 9 :id 't3))
	 (list (faz-subtarefa :dia 1 :hora 10 :id 't4))
	 (list (faz-subtarefa :dia 2 :hora 10 :id 't5))
	 (list (faz-subtarefa :dia 3 :hora 10 :id 't6))
	 (list (faz-subtarefa :dia 1 :hora 11 :id 't7))
	 (list (faz-subtarefa :dia 2 :hora 11 :id 't8))
	 (list (faz-subtarefa :dia 3 :hora 11 :id 't9)))
   (list (list (faz-subtarefa :dia 1 :hora 19 :id 't01))
	 (list (faz-subtarefa :dia 2 :hora 19 :id 't02))
	 (list (faz-subtarefa :dia 3 :hora 19 :id 't03)))
   (list (list (faz-subtarefa :dia 3 :hora 10 :id 't11))
	 (list (faz-subtarefa :dia 4 :hora 10 :id 't12))
	 (list (faz-subtarefa :dia 5 :hora 10 :id 't13)))
   (list (list (faz-subtarefa :dia 3 :hora 11 :id 't21))
	 (list (faz-subtarefa :dia 4 :hora 11 :id 't22))
	 (list (faz-subtarefa :dia 5 :hora 11 :id 't23)))
   (list (list (faz-subtarefa :dia 3 :hora 12 :id 't31))
	 (list (faz-subtarefa :dia 4 :hora 12 :id 't32))
	 (list (faz-subtarefa :dia 5 :hora 12 :id 't33)))
   (list (list (faz-subtarefa :dia 3 :hora 13 :id 't41))
	 (list (faz-subtarefa :dia 4 :hora 13 :id 't42))
	 (list (faz-subtarefa :dia 5 :hora 13 :id 't43)))
   (list (list (faz-subtarefa :dia 3 :hora 14 :id 't51))
	 (list (faz-subtarefa :dia 4 :hora 14 :id 't52))
	 (list (faz-subtarefa :dia 5 :hora 14 :id 't53)))
   (list (list (faz-subtarefa :dia 3 :hora 15 :id 't61))
	 (list (faz-subtarefa :dia 4 :hora 15 :id 't62))
	 (list (faz-subtarefa :dia 5 :hora 15 :id 't63)))
   (list (list (faz-subtarefa :dia 3 :hora 16 :id 't71))
	 (list (faz-subtarefa :dia 4 :hora 16 :id 't72))
	 (list (faz-subtarefa :dia 5 :hora 16 :id 't73)))
   (list (list (faz-subtarefa :dia 3 :hora 17 :id 't81))
	 (list (faz-subtarefa :dia 4 :hora 17 :id 't82))
	 (list (faz-subtarefa :dia 5 :hora 17 :id 't83)))
   (list (list (faz-subtarefa :dia 3 :hora 18 :id 't91))
	 (list (faz-subtarefa :dia 4 :hora 18 :id 't92))
	 (list (faz-subtarefa :dia 5 :hora 18 :id 't93)))
   (list  (list (faz-subtarefa :dia 1 :hora 9 :id 't101)
		(faz-subtarefa :dia 2 :hora 9 :id 't102)
		(faz-subtarefa :dia 3 :hora 9 :id 't103)
		(faz-subtarefa :dia 1 :hora 10 :id 't104)
		;(faz-subtarefa :dia 2 :hora 10 :id 't105)
		(faz-subtarefa :dia 3 :hora 10 :id 't106)
		(faz-subtarefa :dia 1 :hora 11 :id 't107)
		(faz-subtarefa :dia 2 :hora 11 :id 't108)
		(faz-subtarefa :dia 3 :hora 11 :id 't109)
		))))

; Problemas do anuncio de dia 22/Novembro
(defparameter *ex6*
  (list ;de tarefas
   (list  (list (faz-subtarefa :dia 1 :hora 9 :id 't101)
		(faz-subtarefa :dia 2 :hora 9 :id 't102)
		(faz-subtarefa :dia 3 :hora 9 :id 't103)
		(faz-subtarefa :dia 1 :hora 10 :id 't104)
		;(faz-subtarefa :dia 2 :hora 10 :id 't105)
		(faz-subtarefa :dia 3 :hora 10 :id 't106)
		(faz-subtarefa :dia 1 :hora 11 :id 't107)
		(faz-subtarefa :dia 2 :hora 11 :id 't108)
		(faz-subtarefa :dia 3 :hora 11 :id 't109)))
   (list (list (faz-subtarefa :dia 1 :hora 19 :id 't01))
	 (list (faz-subtarefa :dia 2 :hora 19 :id 't02))
	 (list (faz-subtarefa :dia 3 :hora 19 :id 't03)))
   (list (list (faz-subtarefa :dia 3 :hora 10 :id 't11))
	 (list (faz-subtarefa :dia 4 :hora 10 :id 't12))
	 (list (faz-subtarefa :dia 5 :hora 10 :id 't13)))
   (list (list (faz-subtarefa :dia 3 :hora 11 :id 't21))
	 (list (faz-subtarefa :dia 4 :hora 11 :id 't22))
	 (list (faz-subtarefa :dia 5 :hora 11 :id 't23)))
   (list (list (faz-subtarefa :dia 3 :hora 12 :id 't31))
	 (list (faz-subtarefa :dia 4 :hora 12 :id 't32))
	 (list (faz-subtarefa :dia 5 :hora 12 :id 't33)))
   (list (list (faz-subtarefa :dia 3 :hora 13 :id 't41))
	 (list (faz-subtarefa :dia 4 :hora 13 :id 't42))
	 (list (faz-subtarefa :dia 5 :hora 13 :id 't43)))
   (list (list (faz-subtarefa :dia 3 :hora 14 :id 't51))
	 (list (faz-subtarefa :dia 4 :hora 14 :id 't52))
	 (list (faz-subtarefa :dia 5 :hora 14 :id 't53)))
   (list (list (faz-subtarefa :dia 3 :hora 15 :id 't61))
	 (list (faz-subtarefa :dia 4 :hora 15 :id 't62))
	 (list (faz-subtarefa :dia 5 :hora 15 :id 't63)))
   (list (list (faz-subtarefa :dia 3 :hora 16 :id 't71))
	 (list (faz-subtarefa :dia 4 :hora 16 :id 't72))
	 (list (faz-subtarefa :dia 5 :hora 16 :id 't73)))
   (list (list (faz-subtarefa :dia 3 :hora 17 :id 't81))
	 (list (faz-subtarefa :dia 4 :hora 17 :id 't82))
	 (list (faz-subtarefa :dia 5 :hora 17 :id 't83)))
   (list (list (faz-subtarefa :dia 3 :hora 18 :id 't91))
	 (list (faz-subtarefa :dia 4 :hora 18 :id 't92))
	 (list (faz-subtarefa :dia 5 :hora 18 :id 't93)))
   (list (list (faz-subtarefa :dia 1 :hora 9 :id 't1))
	 (list (faz-subtarefa :dia 2 :hora 9 :id 't2))
	 (list (faz-subtarefa :dia 3 :hora 9 :id 't3))
	 (list (faz-subtarefa :dia 1 :hora 10 :id 't4))
	 (list (faz-subtarefa :dia 2 :hora 10 :id 't5))
	 (list (faz-subtarefa :dia 3 :hora 10 :id 't6))
	 (list (faz-subtarefa :dia 1 :hora 11 :id 't7))
	 (list (faz-subtarefa :dia 2 :hora 11 :id 't8))
	 (list (faz-subtarefa :dia 3 :hora 11 :id 't9)))
))



;;
;; Teste heuristico #1
;; Utilizado para calcular erros nas heuristicas
;;
;; Solucao optima:
;; (((2 1 TX) (2 2 TY) (2 3 TZ)) ((2 1 TX) (2 2 TY) (2 3 TZ)) ((2 1 TX) (2 2 TY) (2 3 TZ)))
;;
(defparameter *htest1*
	(list
		(list ; Tarefa 1
			(list ; Alternativa 1
				(faz-subtarefa :dia 1 :hora 1 :id 'tA)
				(faz-subtarefa :dia 1 :hora 2 :id 'tB))
			(list ; Alternativa 2
				(faz-subtarefa :dia 2 :hora 1 :id 'tX)
				(faz-subtarefa :dia 2 :hora 2 :id 'tY)
				(faz-subtarefa :dia 2 :hora 3 :id 'tZ)))
		(list ; Tarefa 2
			(list ; Alternativa 1
				(faz-subtarefa :dia 1 :hora 3 :id 'tC)
				(faz-subtarefa :dia 1 :hora 4 :id 'tD))
			(list ; Alternativa 2
				(faz-subtarefa :dia 2 :hora 1 :id 'tX)
				(faz-subtarefa :dia 2 :hora 2 :id 'tY)
				(faz-subtarefa :dia 2 :hora 3 :id 'tZ)))
		(list ; Tarefa 3
			(list ; Alternativa 1
				(faz-subtarefa :dia 1 :hora 5 :id 'tE)
				(faz-subtarefa :dia 1 :hora 6 :id 'tF))
			(list ; Alternativa 2
				(faz-subtarefa :dia 2 :hora 1 :id 'tX)
				(faz-subtarefa :dia 2 :hora 2 :id 'tY)
				(faz-subtarefa :dia 2 :hora 3 :id 'tZ)))))
(defvar htest1 (formulacao-problema *htest1*))

;;
;; Teste heuristico #2
;; Utilizado para calcular erros nas heuristicas
;;
;; Solucao optima:
;; (((2 1 TX) (2 2 TY) (2 3 TZ)) ((2 1 TX) (2 2 TY) (2 3 TZ)) ((2 1 TX) (2 2 TY) (2 3 TZ)))
;;
(defparameter *htest2*
	(list
		(list
			(list
				(faz-subtarefa :dia 0 :hora 0 :id 'tSTART)))
		(list ; Tarefa 1
			(list ; Alternativa 1
				(faz-subtarefa :dia 1 :hora 1 :id 'tA)
				(faz-subtarefa :dia 1 :hora 2 :id 'tB))
			(list ; Alternativa 2
				(faz-subtarefa :dia 2 :hora 1 :id 'tX)
				(faz-subtarefa :dia 2 :hora 2 :id 'tY)
				(faz-subtarefa :dia 2 :hora 3 :id 'tZ)))
		(list ; Tarefa 2
			(list ; Alternativa 1
				(faz-subtarefa :dia 1 :hora 3 :id 'tC)
				(faz-subtarefa :dia 1 :hora 4 :id 'tD))
			(list ; Alternativa 2
				(faz-subtarefa :dia 2 :hora 1 :id 'tX)
				(faz-subtarefa :dia 2 :hora 2 :id 'tY)
				(faz-subtarefa :dia 2 :hora 3 :id 'tZ)))
		(list ; Tarefa 3
			(list ; Alternativa 1
				(faz-subtarefa :dia 1 :hora 5 :id 'tE)
				(faz-subtarefa :dia 1 :hora 6 :id 'tF))
			(list ; Alternativa 2
				(faz-subtarefa :dia 2 :hora 1 :id 'tX)
				(faz-subtarefa :dia 2 :hora 2 :id 'tY)
				(faz-subtarefa :dia 2 :hora 3 :id 'tZ)))
		(list
			(list
				(faz-subtarefa :dia 0 :hora 1 :id 'tEND)))))
(defvar htest2 (formulacao-problema *htest2*))




;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; PROFILING ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Funcao para correr e analisar varias procuras
(defun corre-procuras (tarefas formulacao heuristica)

	(print "PLPA") (print (plpa formulacao))
	(print "PLPG") (print (plpg formulacao))

	(print "PPPA") (print (pppa formulacao))
	(print "PPPG") (print (pppg formulacao))

	(print "PPIA") (print (ppia formulacao))
	(print "PPIG") (print (ppig formulacao))

	(print "PCUA") (print (pcua formulacao))
	(print "PCUG") (print (pcug formulacao))
	
	(print "PGA") (print (pga formulacao heuristica))
	(print "PGG") (print (pgg formulacao heuristica))

	(print "PA*A") (print (pa*a formulacao heuristica))
	(print "PA*G") (print (pa*g formulacao heuristica))

	(print "RBFS") (print (rbfs formulacao heuristica))

	(print "CSP") (print (psr (faz-csp tarefas)))
	
)

(defun analisa-procuras (tarefas &optional (formulacao #'formulacao-problema) (heuristica #'faz-heuristica-a-6))
	(sb-profile:unprofile) ; reset everything
	(sb-profile:profile pcua pcug)
	(sb-profile:profile pga pgg)
	(sb-profile:profile pa*a pa*g)
	(sb-profile:profile plpa plpg)
	(sb-profile:profile pppa pppg)
	(sb-profile:profile ppla pplg) ; via PPI
	(sb-profile:profile ppia ppig)
	(sb-profile:profile rbfs)
	(sb-profile:profile psr)

	; aux functions
	(corre-procuras tarefas (funcall formulacao tarefas) (funcall heuristica tarefas))
	(sb-profile:report))
