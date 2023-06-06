
#|
This test defines a simple set of related objects, a room which
contains a light and a switch, the switch connecteing to the light.
When we add a switch event, for either "on" or "off", the rules run
and propagate the appropriate state to the light. Test dhere is basic
edge addition and inference via queuing, and deletion of edges as we
keep mutable state.

When the run method runs, foundation rules will run, then the switch
tests, and the output should be like this (with perhaps a different
node nunmber:

(N297 UNDEFINED LIT) 
(N297 LIT UNLIT) 
(N297 UNLIT LIT) 
|#

(defc basic-test foundation nil
  (let ()

	(defm init ()
	  (clear-counters)
	  (clear-perf-stats)
	  (foundation-init)
	  (do-defg)
	  )

	(defm run ()
	  (let ()
		(execute-global-all-objs-loop)
		(let ((switch (query '((?s type switch)) '?s)))
		  (clear-queue)
		  (add switch 'event 'on)
		  (execute-queue)
		  (add switch 'event 'off)
		  (execute-queue)
		  (add switch 'event 'on)
		  (execute-queue))))


	(defm do-defg ()
	  (defg
		'(
		  (rule
		   (name basic-test-data)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node rule ?r)
			(?r name basic-test-data)
			(?light new-node sn5)
			(?switch new-node sn6)
			(?room new-node sn7)
			(?add-out new-node sn8)
			(?add new-node sn9)
			(?add2 new-node sn10))
		   (add
			(print basic-test-data)
			(off light-illum-mapping unlit)
			(on  light-illum-mapping lit)
			(occupied room-switch-mapping on)
			(unoccupied room-switch-mapping off)
			(?light is switch-room-obj)
			(?light name light1)
			(?light state undefined)
			(?light type light)
			(?switch is switch-room-obj)
			(?switch type switch)
			(?switch name switch1)
			(?switch state undefined)
			(?switch connected-to ?light)
			(?room is switch-room-obj)
			(?room type room)
			(?room name room1)
			(?room contains ?switch)
			(?room state undefined)
			(?add-out type add-out)
			(?add-out name add-out1)
			(?add-out value 0)
			(?add name add1)
			(?add type add)
			(?add input1 1)
			(?add input2 0)
			(?add output ?add-out)
			(?add2 name add2)
			(?add2 type addx)
			(?add2 input1 0)
			(?add2 input2 0)
			(?add2 value 0)
			(+ 1 2 3)
			(+ 2 2 4)
			(+ 2 3 5))
		   (del
			(global-node rule ?this-rule)))

		  (rule
		   (name add-rule)
		   (pred
			(?m type add)
			(?m input1 ?x))
		   (add
			(print add-rule-triggered ?m ?x)
			(?m rule (rule
					  (name add-aux-rule)
					  (root-var ?m)
					  (pred 
					   (?m input2 ?y)
					   (?x ?y + ?z)
					   (?m output ?o)
					   (?o value ?v))
					  (del
					   (?o value ?v))
					  (add
					   (?o value ?z))))))

		  (rule
		   (name addx-rule)
		   (pred
			(?m type addx)
			(?m input1 ?x)
			(?m input2 ?y)
			(?x ?y + ?z))
		   (add
			(print addx-rule ?m ?x ?y ?z)
			(?m value ?z)))

		  (rule 
		   (name switch-rule)
		   (local)
		   (pred
			(?s type switch)
			(?s event ?e)
			(?e light-illum-mapping ?i)
			(?s state ?x)
			(?s connected-to ?l)
			(?l state ?y))
		   (del
			(?s event ?e)
			(?s state ?x)
			(?l state ?y))
		   (add
			;; (print switch ?s state ?e name ?m light ?l old-state ?y old-illumination ?oldn new-illumination ?newn)
			(print ?s ?y ?i)
			(?s state ?e)
			(?l state ?i)))

		  (rule
		   (name room-rule)
		   (local)
		   (pred 
			(?room type room)
			(?room event ?e)
			(?e room-switch-mapping ?m)
			(?room state ?r)
			(?room contains ?switch)
			(?switch type switch))
		   (del
			(?room event ?e)
			(?room state ?r))
		   (add
			(print room ?room ?e ?switch)
			(?room state ?e)
			(?switch event ?m)))

		  (rule
		   (name switch-room-obj-rule)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred 
			(global-node local-rule-pool ?p)
			(?p lrp-rule ?room-rule)
			(?p lrp-rule ?switch-rule)
			(?room-rule name room-rule)
			(?switch-rule name switch-rule))
		   (add
			(print switch-room-obj-rule)
			(switch-room-obj rule ?room-rule)
			(switch-room-obj rule ?switch-rule))
		   (del
			(global-node rule ?this-rule)))

		  )))))