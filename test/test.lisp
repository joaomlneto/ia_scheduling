;; problema.lisp
;; Problemas e coisas associadas
;; Grupo 47

;; testes

(load "proj.lisp")
(load "test-problemas.lisp")

(defvar *t11* (faz-subtarefa :dia 1 :hora 9  :id 't111))
(defvar *t12* (faz-subtarefa :dia 1 :hora 9  :id 't112))
(defvar *t21* (faz-subtarefa :dia 1 :hora 10 :id 't121))
(defvar *t22* (faz-subtarefa :dia 1 :hora 10 :id 't122))

; testa "subtarefas-simultaneasp"
(if (subtarefas-simultaneasp *t11* *t12*)
  (print "Tarefas simultaneas sao simultaneas - ok")
  (print "Tarefas simultaneas nao sao sao? - FAIL"))

; testa "subtarefas-iguaisp"
(if (subtarefas-iguaisp *t11* *t11*)
  (print "Tarefas iguais sao iguais - ok")
  (print "Tarefas iguais nao sao iguais? - FAIL"))

(if (subtarefas-iguaisp *t11* *t12*)
  (print "Tarefas diferentes sao iguais? - FAIL")
  (print "Tarefas diferentes nao sao iguais - ok"))

; testa "subtarefas-conflitop"
(if (subtarefas-conflitop *t11* *t11*)
  (print "Uma tarefa entra em conflito consigo mesma? - FAIL")
  (print "Uma tarefa nao entra em conflito consigo mesma - ok"))

(if (subtarefas-conflitop *t11* *t12*)
  (print "Uma tarefa entra em conflito com outra simultanea - ok")
  (print "Uma tarefa nao entra em conflito com outra simultanea? - FAIL"))

(if (subtarefas-conflitop *t11* *t21*)
  (print "Uma tarefa entra em conflito com outra nao simultanea? - FAIL")
  (print "Uma tarefa nao entra em conflito com outra nao simultanea - ok"))

(defvar node1 (make-node :estado '(1 1) :custo 5))
(defvar node2 (make-node :estado '(1 2) :custo 7))
(defvar node3 (make-node :estado '(2 1) :custo 9))
(defvar node4 (make-node :estado '(2 2) :custo 11))

(defvar nodet1 (make-node :estado '(2 1) :custo 6))  ; teste 1
(defvar nodet2 (make-node :estado '(2 3) :custo 14)) ; teste 2
(defvar nodet3 (make-node :estado '(2 3) :custo 6))  ; teste 3
(defvar nodet4 (make-node :estado '(2 3) :custo 1))  ; teste 4
(defvar nodet5 (make-node :estado '(1 1) :custo 3))  ; teste 5
(defvar nodet6 (make-node :estado '(1 1) :custo 20)) ; teste 6
(defvar nodet7 (make-node :estado '(1 2) :custo 20)) ; teste 7
(defvar nodet8 (make-node :estado '(2 2) :custo 20)) ; teste 8
(defvar nodet9 (make-node :estado '(2 1) :custo 8))  ; teste 9
(defvar nodet10 (make-node :estado '(1 1) :custo 6))  ; teste 10
(defvar nodet11 (make-node :estado '(2 1) :custo 8))  ; teste 11
(defvar nodet12 (make-node :estado '(2 2) :custo 10)) ; teste 12
(defvar nodet13 (make-node :estado '(2 2) :custo 12)) ; teste 13

(defvar testlist (list node1 node2 node3 node4))

(print "A testar queue-add-or-replace-node.")
(print "Teste 1: (e3 6) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet1)
		   (list node1 nodet1 node2 node4))
  (print "Teste bem sucedido")
  (print "ERRO"))

(print "Teste 2: (e5 13) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet2)
		   (list node1 node2 node3 node4 nodet2))
  (print "Teste bem sucedido")
  (print "ERRO"))

(print "Teste 3: (e5 6) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet3)
		   (list node1 nodet3 node2 node3 node4))
  (print "Teste bem sucedido")
  (print "ERRO"))

(print "Teste 4: (e5 1) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet4)
		   (list nodet4 node1 node2 node3 node4))
  (print "Teste bem sucedido")
  (print "ERRO"))

(print "Teste 5: (e1 3) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet5)
		   (list nodet5 node2 node3 node4))
  (print "Teste bem sucedido")
  (print "ERRO"))

(print "Teste 6: (e1 20) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet6)
		   testlist)
  (print "Teste bem sucedido")
  (print "ERRO"))

(print "Teste 7: (e2 20) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet7)
		   testlist)
  (print "Teste bem sucedido")
  (print "ERRO"))

(print "Teste 8: (e4 20) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet8)
		   testlist)
  (print "Teste bem sucedido")
  (print "ERRO"))

(print "Teste 9: (e3 8) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet9)
		   (list node1 node2 nodet9 node4))
  (print "Teste bem sucedido")
  (print "ERRO"))

(print "Teste 10: (e1 6) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet10)
		   testlist)
  (print "Teste bem sucedido")
  (print "ERRO"))

(print "Teste 11: (e3 8) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet11)
		   (list node1 node2 nodet11 node4))
  (print "Teste bem sucedido")
  (print "ERRO"))

(print "Teste 12: (e4 10) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet12)
		   (list node1 node2 node3 nodet12))
  (print "Teste bem sucedido")
  (print "ERRO"))

(print "Teste 13: (e4 12) --> ((e1 5), (e2 7), (e3 9), (e4 11))")
(if (equal (queue-add-or-replace-node testlist nodet13)
		   testlist)
  (print "Teste bem sucedido")
  (print "ERRO"))

