;; Change tags
;;
;; rem-add-main -- Get rid of the add-main rule processing. Find another way to get pretty rule graphs.
;;

(defc dumper nil ()
  (let ((g nil)
		(gv-attrs nil)
		(nest-prefix 0)
		(node-map (make-sur-map))
		(node-set (make-sur-map))
		(designated-rule-edges (make-sur-map))
		(pair-list nil)
		(gv-dir (if (is-laptop) "c:/Program Files/Graphviz" "c:/Program Files (x86)/Graphviz2.38")))

	(defm set-graph (graph)
	  (setq g graph)
	  (setq gv-attrs (mapcar (lambda (x) x) (! (g query) '((?x gv-attr)) '(?x)))))
	
	(defm downcase (x)
	  (cond
	   ((symbolp x)
		(string-downcase (symbol-name x)))
	   ((stringp x)
		(string-downcase x))
	   (t x)))

	(defm designated-rule-entry-node (rule-node)
	  (let ((preds (! ((! (g get-rule-components) rule-node) preds))))
		(let ((s (sort preds (lambda (x y) (< (sxhash x) (sxhash y))))))
		  (symcat nest-prefix '- (! (g hget) rule-node 'name) '- (first (first s))))))

	;; Given a file <x>.gv, produce <x>.svg or <x>.jpg. Current wd is used unless given absolute paths

	(defm gv-to-image (gv-file &key
							   (edit-svg t)			;; T to edit the svg to take out scale-down limits. But it also imposes scale-up limits.
							   (n2 t)				;; T to do "leveled" layout; nil for circular as in Ladybug
							   (file-type :svg))	;; one-of (:svg :jpg)
	  (defr
	    (defl cat (&rest x)
	      (apply #'concatenate 'string x))
		(defl file-root (gv-file)
		  (subseq gv-file 0 (search ".gv" gv-file)))
		(let ((gv-file-root (file-root gv-file)))
	      (let ((cmd (format nil (cat "\"~a/bin/dot.exe\" ~a.gv | "
									  "\"~a/bin/gvpack.exe\" -m0 | "
									  "\"~a/bin/neato.exe\" -s ~a -T~a "
									  "~a"
									  " > ~a.~a")
							 gv-dir
							 gv-file-root
							 gv-dir
							 gv-dir
							 (if n2 "-n2" "")
							 (case file-type (:svg "svg") (:jpg "jpg"))
							 (if (and (eq file-type :svg) edit-svg) "| sed -e \"s/<svg.*$/\<svg/\"" "")
							 gv-file-root
							 (case file-type (:svg "svg") (:jpg "jpg")))))
			(let ((cmd (format nil "~a" cmd)))
			  (shell-cmd cmd))))))

	;; This svg dump assumes a rule-30 graph. See the ca-to-svg fcn below which generates fast and large rule-30 svg
	;; files. Leave this code here since it may later prove a good basis for generating other graphs directly to
	;; svg. However the xcoord-generating stuff in the rules will go, since it's slow and no longer needed.

	(defm dump-svg (file)
	  (with-open-file (s file :direction :output)
	    (let ((header '("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
			    "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
			    "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
			    "<svg"
			    "viewBox=\"0.00 0.00 5000.00 5000.00\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">"
			    "<g id=\"graph0\" class=\"graph\" transform=\"scale(1 1) rotate(0) translate(  2500.00 30.00)\">")))
	      (defr
		(defl emit-node (node)
		  (let ((v (! (g hget) node 'rule30val)))
		    (when v
		      (let ((xcoord (! (g hget) node 'xcoord)))
			(when xcoord
			  (let ((xcoord (if (! (g edge-exists) (list node 'neg)) (- xcoord) xcoord)))
			    (let ((ycoord (! (g hget) node 'level)))
			      (let ((x (* xcoord 20)))
				(let ((y (* ycoord 20)))
				  (let ((rad 10))
				    (format s "<g id=\"~a\" class=\"node\"><title>~a</title>~%" node node)
				    ($comment
				     (format s "<ellipse fill=\"~a\" stroke=\"~a\" cx=\"~a\" cy=\"~a\" rx=\"10\" ry=\"10\"/>~%"
					     (if (= v 0) "cyan" "magenta") (if (= v 0) "cyan" "magenta")
					     x y))
				    (format s "<polygon fill=\"~a\" stroke=\"~a\" points=\"~a,~a ~a,~a ~a,~a ~a,~a\"/>~%"
					    (if (= v 0) "cyan" "magenta") (if (= v 0) "cyan" "magenta")
					    (- x rad) (- y rad)
					    (- x rad) (+ y rad)
					    (+ x rad) (+ y rad)
					    (+ x rad) (- y rad))
				    (format s "</g>~%")))))))))))
		(dolist (h header)
		  (format s "~a~%" h))
		(dolist (node (! (g get-all-nodes)))
		  (emit-node node))
		(format s "</g>~%")
		(format s "</svg>~%")))))


	;;  "c:\Program Files (x86)\Graphviz2.38\bin\dot.exe" x.gv | "c:\Program Files (x86)\Graphviz2.38\bin\gvpack.exe" -m0  | "c:\Program Files (x86)\Graphviz2.38\bin\neato.exe" -s  -Tsvg | sed -e "s/<svg.*$/\<svg/" > x.svg

	;;  dot x.gv | gvpack -m0  | neato -s -n2 -Tsvg | sed -e "s/<svg.*$/\<svg/" > x.svg

	;; "c:\Program Files (x86)\Graphviz2.38\bin\dot.exe" -Tjpeg fe.gv -o fe.jpg
	;; (let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "yyy3.gv" '(sigma even-func)))

	;; an attr is simply a node which must appear somewhere in an edge
	;; attrs is a list of attrs to draw. If attrs = t, draw all
	;; omitted-attrs is the set of attrs whose presence causes the edge not to be drawn, except if it's explicitly in the attrs list
	;; rules is a list of rules to draw. If rules = t, draw all
	;;
	;; Some attributes of nodes which affect drawing:
	;;
	;; (<node separate-display-node {t|nil} When t, a given node, when encountered as a target, will be cloned into a
	;;									    new node. Thus for these nodes we don't get a cluster of refs to them from
	;;									    various other nodes. Handy for numbers, hence we have the arg below
	;;									    separate-number-nodes. Default is nil, but we set this externally for rules.

	(defm dump-gv-edges (file &key
							  (edges nil)	;; list of edges to draw (e.g, from a query). nil means use the other filters, such as attrs. 
											;; If it's a list, then that supersedes all other filters.
							  (attrs t)
							  (attrs-fcn (lambda (e) t))
							  (omitted-attrs nil)
							  (rules t)
							  (omitted-rules nil)
							  (admit-string-attrs nil) ;; Admit edges with a string as the second member, regardless of whether it's in the attrs list
							  (as-2d-asc-facets nil)
							  (rule-labels-only nil)   ;; Only rule nodes will have labels. Edge labels unaffected.
							  (emit-labels t)
							  (omit-unmatched-rules t)
							  (emit-legend t)
							  (separate-number-nodes nil)	;; t means emit each number as its own node, as with length-2 edges
							  (graph-type :digraph)	;; :digraph -- The data and each rule is a digraph, with its own cluster box.
													;; :subgraph -- The data is a digraph with data edges and the rules are subgraphs
													;;		 of the data graph, each rule with its own cluster box.
													;; :single-graph -- ??? ;; If true, generates a subgraph per rule, but not s separate digraph perf rule.
													;;		The rules in this way are shown directly connected to the data.
							  (gv-graph-props nil) ;; String of gv props, e.g., "rankdir=BT;"	Kind of a hack.
							  )
	  (let ()
		(defstruct node-entry
		  (name nil)
		  (rule-name nil) ;; If non-nil, then this node is inside a rule
		  (is-new-node-var nil)
		  (do-not-emit nil)
		  (gv-attr-map (make-sur-map))) ;; gv-attr -> attr value
		(let ((omitted-attrs (append omitted-attrs '(triggered add pred del binding left right print note))))
		  (with-open-file (s file :direction :output)
			(defr
			  (defl string-concat (x y)
				(concatenate 'string x y))

			  (defl admit-edge (v)
				(let ((r 
					   (or 
						(and admit-string-attrs
							 (stringp (second v)))
						(and (eq attrs t) ;; attrs defined (i.e. other than t) will supersede specification of an attrs-fcn
							 (funcall attrs-fcn v)
							 (not (intersect v omitted-attrs)))
						(and (not (eq attrs t))
							 (intersect v attrs)
							 (not (intersect (set-subtract v (intersect v attrs)) omitted-attrs))))))
				  ;; (print (list v r))
				  r))

			  ;; create-node-entry should only be called when we define all the node enrties as the first pass.

			  (defl create-node-entry (n &key two-input-op as-prop rule-name do-not-emit is-new-node-var)
				(let ()
				  (let ((ne (! (node-map lookup-one) n)))
					(when (null ne)
					  (! (node-set insert) n nil)
					  (setq ne (make-node-entry))
					  (! (node-map insert-one) n ne)
					  (setf (node-entry-name ne) (format nil "~a~a"
														 (if rule-name (format nil "~a-" rule-name) "")
														 n))
					  (setf (node-entry-do-not-emit ne) do-not-emit)
					  (setf (node-entry-rule-name ne) rule-name)
					  (if (and emit-labels
							   (not rule-labels-only)) ;; Rule labels are set separately - ???
						  (set-gv-attr n 'label n)
						  (set-gv-attr n 'label "")))
					(when rule-name
					  (cond 
					   ((or (node-entry-is-new-node-var ne)
							is-new-node-var)
						(setf (node-entry-is-new-node-var ne) t)
						(set-gv-attr n 'shape 'octagon)
						(set-gv-attr n 'color 'mistyrose))
					   ((is-var-name n)
						(set-gv-attr n 'color 'paleturquoise))
					   (t
						(set-gv-attr n 'color 'palegreen))))

					(set-gv-attr n 'fontname 'arial)
					(set-gv-attr n 'style 'filled)

					(when (memq 'rule (! (g hget-all) n 'type))
					  (let ((name (! (g hget) n 'name)))
						(set-gv-attr n 'label name)
						(set-gv-attr n 'shape 'rectangle)
						(set-gv-attr n 'color 'mistyrose)))

					(when as-prop
					  (set-gv-attr n 'shape 'none))
					(dolist (gv-attr gv-attrs)
					  (when (! (g hget) n gv-attr)
						(! ((node-entry-gv-attr-map ne) insert-one) gv-attr (! (g hget) n gv-attr))))
					;; (print (list 'd1 n ne (! ((node-entry-gv-attr-map ne) as-list))))
					nil)))

			  (defl clone-node-entry (n) ;; Returns new sym
				(let ((r (gensym)))
				  (! (node-set insert) r nil)
				  (let ((old-ne (! (node-map lookup-one) n)))
					(let ((ne (make-node-entry)))
					  (setf (node-entry-name ne) r)
					  (setf (node-entry-gv-attr-map ne) (node-entry-gv-attr-map old-ne)) ;; Note we don't copy the attr map
					  (setf (node-entry-rule-name ne) (node-entry-rule-name old-ne))
					  ;; (setf (node-entry-do-not-emit ne) (node-entry-do-not-emit old-ne))			;; Bugs -- with this in we get incorrect behavior from the two-input-ops
					  (! (node-map insert) r ne)
					  ;; (print (list 'cne1 ne old-ne n r (! ((node-entry-gv-attr-map ne) as-list))))
					  r))))

			  (defl do-not-emit (n)
				(let ((ne (! (node-map lookup-one) n)))
				  (node-entry-do-not-emit ne)))

			  (defl set-gv-attr (n attr value)
				(let ((ne (! (node-map lookup-one) n)))
				  (! ((node-entry-gv-attr-map ne) insert-one) attr value)
				  nil))

			  (defl get-gv-attr (n attr)
				(let ((ne (! (node-map lookup-one) n)))
				  (! ((node-entry-gv-attr-map ne) lookup-one) attr)))

			  (defl prepend-node-name (n l)
				(let ((ne (create-node-entry n)))
				  (setf (node-entry-name ne) (symcat l '- (node-entry-name ne)))
				  nil))
			  (defl get-node-name (n)
				(let ((ne (create-node-entry n)))
				  (node-entry-name ne)))

			  (defl get-format (n &key as-prop)
				(let ()
				  (let ((ne (! (node-map lookup-one) n)))
					(let ((gv-attr-info-list (! ((node-entry-gv-attr-map ne) as-list))))
					  (let ((r (if as-prop (format nil "[") (format nil "\"~a\" [" (node-entry-name ne)))))
						(let ((comma ""))
						  (dolist (gv-attr-info gv-attr-info-list)
							(let ((attr (first gv-attr-info)))
							  (let ((value (first (second gv-attr-info))))
								(when value
								  (setq r (string-concat r (format nil "~a~a=\"~a\"" comma attr value)))
								  (setq comma ",")))))
						  (setq r (string-concat r (format nil "];" comma)))
						  r))))))
			  (defl dump-gv-nodes ()
				(let ((nodes (! (node-set inputs))))
				  (dolist (n nodes)
					(when (not (do-not-emit n))
					  (format s "~a~%" (get-format n)))
					)))
			  (defl dump-gv-notes ()
				(when emit-legend
				  ;; Can add a large edge and cause undiagnosed some
				  ;; explosion in subqet processing. So now just
				  ;; append it following.
				  ;;
				  ;; (! (g add-edge) `(note body "attributes:" "\\n" ,@attrs))
				  (let ((title-fontsize (! (g query) '((set note title fontsize ?x)) '?x)))
					(let ((body-fontsize (! (g query) '((set note body fontsize ?x)) '?x)))
					  (let ((footer-fontsize (! (g query) '((set note footer fontsize ?x)) '?x)))
						(let ((notes (! (g get-edges) 'note)))
						  (let ((xxxnotes (cons `(note body "attributes:" "\\n" ,@attrs) notes))) ;; !!!!!!!!!!!!!!!!!!! Nop !!!!!!!!!!!!!!!!
							(let ((title "")
								  (body "")
								  (footer ""))
							  (let ((n ""))
								(dolist (note notes)
								  (when (eq (first note) 'note)
									(let ((type-data (if (member (second note) '(title footer body) :test #'eq)
														 (list (second note) (rest (rest note)))
														 (list 'body (rest note)))))
									  (let ((type (first type-data))
											(data (second type-data)))
										(dolist (p data)
										  (setq n (format nil "~a ~a" n p)))
										;; (setq n (format nil "~a~%" n))
										(setq n (format nil "~a\\n" n))
										(cond
										 ((eq type 'title)
										  (setq title (format nil "~a~a" title n)))
										 ((eq type 'body)
										  (setq body (format nil "~a~a" body n)))
										 ((eq type 'footer)
										  (setq footer (format nil "~a~a" footer n))))
										(setq n ""))))))
							  (let ()
								(format s "digraph Notes {~%")
								(format s "subgraph cluster {~%")
								(format s "style=filled;~%")
								(format s "color=springgreen;~%")
								(format s "label = \"Legend\";~%")
								(when (not (equal title ""))
								  (format s "__x1 [label = \"~a\",fontsize=~a,shape=rectangle,color=springgreen,style=filled];~%" title (or title-fontsize 18)))
								(when (not (equal body ""))
								  (format s "__x2 [label = \"~a\",fontsize=~a,shape=rectangle,color=springgreen,style=filled];~%" body (or body-fontsize 12)))
								(when (not (equal footer ""))
								  (format s "__x3 [label = \"~a\",fontsize=~a,shape=rectangle,color=springgreen,style=filled];~%" footer(or footer-fontsize 8)))
								(format s "__x1 -> __x2 -> __x3 [style=invis];~%")
								(format s "}~%")
								(format s "}~%"))))))))))
			  (defl dump-gv-edges-data (v)
				(when (not (and (eq (second v) 'type) (eq (third v) 'rule)))
				  (when (admit-edge v)
					($comment
					 (when (and (>= (length v) 1)
								(! (g hget) (first v) 'color))
					   (format s "\"~a\" [color=~a,style=filled];~%" (first v) (! (g hget) (first v) 'color)))
					 (when (and (= (length v) 4)
								(! (g edge-exists) `(,(third v) two-input-op))
								(! (g hget) (second v) 'color))
					   (format s "\"~a\" [color=~a,style=filled];~%" (second v) (! (g hget) (second v) 'color)))
					 (when (and (= (length v) 4)
								(! (g edge-exists) `(,(third v) two-input-op))
								(! (g hget) (first v) 'color))
					   (format s "\"~a\" [color=~a,style=filled];~%" (first v) (! (g hget) (first v) 'color)))
					 )
					(let ((l (length v)))
					  (cond
					   ((and (= l 2)
							 (eq (second v) 'next))
						(format s "\"~a\" -> \"~a\" [label=\"~a\",style=\"setlinewidth(1)\"];~%"
								(first v) (first v) 
								(if emit-labels (second v) "")))
					   ((and (= l 3)
							 (eq (second v) 'fcn-color))
						nil)
					   ((and (= l 3) ;; Special-case coloring for rule30 nodes  *******************
							 (eq (second v) 'rule30val))
						(format s "\"~a\" [color=~a,style=filled];~%" (first v) (if (and (numberp (third v)) (= (third v) 1)) "cyan" "magenta"))
						)
					   ((and as-2d-asc-facets
							 (= l 3))
						(let ((n1 (first v)))
						  (let ((n2 (second v)))
							(let ((n3 (third v)))
							  (when (not (member (list n1 n2) pair-list :test #'set-equal))
								(setq pair-list (cons (list n1 n2) pair-list))
								(create-node-entry n1)
								(create-node-entry n2)
								(format s "\"~a\" -> \"~a\" [arrowhead=none];~%" n1 n2))
							  (when (not (member (list n2 n3) pair-list :test #'set-equal))
								(setq pair-list (cons (list n2 n3) pair-list))
								(create-node-entry n2)
								(create-node-entry n3)
								(format s "\"~a\" -> \"~a\" [arrowhead=none];~%" n2 n3))
							  (when (not (member (list n3 n1) pair-list :test #'set-equal))
								(setq pair-list (cons (list n3 n1) pair-list))
								(create-node-entry n3)
								(create-node-entry n1)
								(format s "\"~a\" -> \"~a\" [arrowhead=none];~%" n3 n1))))))
					   ((= l 3)
						(block b
						  (let ((in-node (first v)))
							(let ((out-node (third v)))
							  (let ((prop (second v)))
								(create-node-entry in-node)
								(create-node-entry out-node)
								(create-node-entry prop :as-prop t)
								(let ((out-node (if (or (and separate-number-nodes  (numberp out-node))
														(! (g hget) out-node 'separate-display-node))
													(clone-node-entry out-node)
													out-node)))
								  (create-node-entry prop :do-not-emit t)
								  (let ((prop-label (get-gv-attr prop 'label)))
									(let ((edge-color (! (g hget) prop 'edge-color)))
									  (let ((edge-attr-string ""))
										(dolist (r (list in-node out-node))
										  (when (memq 'rule (! (g hget-all) r 'type))
											(let ((n (! (g hget) r 'name)))
											  (when (member n omitted-attrs :test #'eq)
												(return-from b nil))
											  #|
											  (set-gv-attr r 'label n)
											  (set-gv-attr r 'shape 'rectangle)
											  (set-gv-attr r 'color 'mistyrose)
											  |#
											  (when (memq graph-type '(:subgraph :single-graph))
												(let ((rule-entry-node (designated-rule-entry-node r)))
												  (when (not (! (designated-rule-edges lookup-one) (list r rule-entry-node)))
													(! (designated-rule-edges insert) (list r rule-entry-node) (list r rule-entry-node))
													(format s "\"~a\" -> \"~a\" [style=dotted,color=blue;weight=50];~%" 
															r rule-entry-node)))))))
										(when (eq prop 'note)
										  (set-gv-attr out-node 'shape 'rectangle))
										(setq edge-attr-string (get-format prop :as-prop t))
										#|
										(when edge-color
										(setq edge-attr-string (concatenate 'string edge-attr-string (format nil ",color=~a" edge-color))))
										|#
										(if emit-labels
											
											#| (format s "\"~a\" -> \"~a\" [fontname=arial,label=\"~a\",style=\"setlinewidth(1)\"~a];~%" 
															  in-node out-node prop-label edge-attr-string) |#

											(format s "\"~a\" -> \"~a\"              ~a~%" 
															  in-node out-node edge-attr-string)


											
											(format s "\"~a\" -> \"~a\" [fontname=arial,label=\"~a\",style=\"setlinewidth(1)\"~a];~%"
													in-node out-node "" edge-color-string)))))))))))

					   #|
					   ((eq (second v) 'zero)
					   (create-node-entry (first v))
					   (if emit-labels
					   (set-gv-attr (first v) 'label (symcat (first v) '-z))
					   (set-gv-attr (first v) 'label "")))
					   |#
					   
					   ((and t
							 (= l 4)
							 (! (g edge-exists) `(,(third v) two-input-op)))
						(let ((i1 (first v))
							  (i2 (second v))
							  (fcn (third v))
							  (o (fourth v)))
						  (create-node-entry i1)
						  (create-node-entry i2)
						  (create-node-entry o)
						  (create-node-entry fcn :do-not-emit t)
						  (let ((fcn (clone-node-entry fcn)))
							(format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\"];~%" i1 fcn)
							(format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\"];~%" i2 fcn)
							(format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\"];~%" fcn o))))
					   ((and (= l 4)
							 (eq (second v) 'freq))
						(let ((p1 (first v))
							  (freq (third v))
							  (p2 (fourth v)))
						  (create-node-entry p1)
						  (create-node-entry p2)
						  (format s "\"~a\" -> \"~a\" [fontname=arial,label=\"~a\",style=\"setlinewidth(1)\"];~%" p1 p2 freq)))
					   ((= l 2)
						(let ((n (first v))
							  (p (second v)))
						  (create-node-entry n)
						  (create-node-entry p)
						  (let ((p (clone-node-entry p)))
							(set-gv-attr p 'shape 'none)
							(set-gv-attr p 'style 'solid)
							(format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\",label=\"\",arrowhead=none];~%" n p))))
					   (t
						($comment
						 (dotimes (i l)
						   (let ((node (nth i v)))
							 (format s "\"~a\" " node)
							 (when (< i (- l 1))
							   (format s "-> "))))
						 (format s ";~%")
						 )
						(maplist
						 (lambda (x)
						   (let ((n1 (first x)))
							 (let ((n2 (second x)))
							   (when (and n1 n2 (not (member (list n1 n2) pair-list :test #'equal)))
								 (create-node-entry n1)
								 (create-node-entry n2)
								 (setq pair-list (cons (list n1 n2) pair-list))
								 (format s "\"~a\" -> \"~a\";~%" n1 n2)))))
						 v)))))))
			  (defl dump-gv-edges-rules (v)
				(when (and (eq (second v) 'type)
						   (eq (third v) 'rule)
						   (or (eq rules t)
							   (member (! (g hget) (first v) 'name) rules))
						   (not (member (! (g hget) (first v) 'name) omitted-rules))
						   (not (! (g hget-edge-inverse) (first v) 'nested-rule)))
				  (let ((rule-node (first v)))
					(when (or (not omit-unmatched-rules)
						  (> (! (g get-matched) rule-node) 0))
					  (let ((rule-name (! (g hget) rule-node 'name)))
						(when (eq graph-type :digraph)
						  (format s "digraph \"~a\" {~%" rule-name))
						(dump-gv-rule rule-node)
						(when (eq graph-type :digraph)
						  (format s "}~%")))))))
			  (defl dump-gv-rule (rule)
				(when (or (not omit-unmatched-rules)
						  (> (! (g get-matched) rule) 0))
				  (let ((rule-components (! (g get-rule-components) rule))
						(rule-name (! (g hget) rule 'name)))
					(let ((p-rule-name (symcat nest-prefix '- rule-name)))
					  (let ((pred-edges (! (rule-components preds)))
							(del-edges (! (rule-components dels)))
							(add-edges (! (rule-components adds)))		;; rem-add-main -- need prob here to determine how to expand nested rules, rule-modifying-rules, etc.
							(not-edges (! (rule-components nots)))
							(rule-nodes (! (rule-components all-nodes)))
							(i 0))
						(setq nest-prefix (+ nest-prefix 1))
						(setq node-map (make-sur-map))
						(setq node-set (make-sur-map))
						(when (memq graph-type '(:digraph :subgraph))
						  (format s "subgraph \"cluster-~a\" {~%" rule-name)
						  (format s "label = \"~a\";~%" rule-name))
						;; (format s "\"~a\" [label=\"~a\"];~%" rule rule-name)
						;; (format s "\"~a\" -> \"~a\" [label=\"~a\"];~%" "rules" rule-name  "")
						(dolist (rule-edges (list pred-edges del-edges add-edges not-edges))
						  (when (or (eq rule-edges pred-edges)
									(eq rule-edges add-edges)
									(eq rule-edges del-edges))
							(setq i (+ i 1))
							(dolist (rule-edge rule-edges)
							  (when (not (and (eq rule-edges add-edges)
											  (or (eq (first rule-edge) 'print)
												  (eq (first rule-edge) 'note)
												  #| (eq (second rule-edge) 'rule) |# )))
								(let ((l (length rule-edge)))
								  (cond
								   ((and (= l 2)
										 (eq (second rule-edge) 'next))
									(format s "\"~a-~a\" [label=\"~a\",fontname=arial];~%" p-rule-name (first rule-edge) (first rule-edge))
									(format s "\"~a-~a\" -> \"~a-~a\" [label=\"~a\",style=~a,fontname=arial,color=~a];~%"
											p-rule-name (first rule-edge)
											p-rule-name (first rule-edge)
											(second rule-edge)
											(if (eq rule-edges add-edges) "dashed" "solid")
											(if (eq rule-edges add-edges) "red" "black")))
								   ((= l 2)
									(let ((n (first rule-edge))
										  (p (second rule-edge)))
									  (create-node-entry n :rule-name p-rule-name)
									  (create-node-entry p :as-prop t :rule-name p-rule-name)
									  (format s "\"~a-~a\" -> \"~a-~a\" [label=\"\",style=~a,fontname=arial,color=~a,arrowhead=none];~%"
											  p-rule-name n
											  p-rule-name p
											  (if (eq rule-edges add-edges) "dashed" "solid")
											  (if (eq rule-edges add-edges) "red" "black"))))
								   ((and (= l 3)
										 (eq rule-edges pred-edges)
										 (eq (second rule-edge) 'new-node))
									(create-node-entry (first rule-edge) :rule-name p-rule-name :is-new-node-var t))
								   ((= l 3)
									;; (when (admit-edge (list (second rule-edge)))						;; !!!!!!!!!!!!!!!!!!!
									(when (not (member (second rule-edge) omitted-attrs :test #'eq))		;; Do we ever want to prune these out in rule displays?
									  (create-node-entry (first rule-edge) :rule-name p-rule-name)
									  (create-node-entry (third rule-edge) :rule-name p-rule-name)
									  (format s "\"~a-~a\" -> \"~a-~a\" [label=\"~a~a\",style=~a,fontname=arial,color=~a];~%"
											  p-rule-name (first rule-edge)
											  p-rule-name (third rule-edge)
											  (if (member rule-edge del-edges :test #'equal) "X " "")
											  (second rule-edge)
											  (if (eq rule-edges add-edges) "dashed" "solid")
											  (cond
											   ((eq rule-edges add-edges) "red")
											   ((member rule-edge del-edges :test #'equal) "blue")
											   (t "black")))))
								   ((and (= l 4)
										 (! (g edge-exists) `(,(third rule-edge) two-input-op)))
									(let ((i1 (first rule-edge))
										  (i2 (second rule-edge))
										  (fcn-name (third rule-edge))
										  (o (fourth rule-edge)))
									  (let ((fcn (format nil "~a-~a-~a-~a-~a" p-rule-name fcn-name i1 i2 o))
											(color (! (g hget) fcn-name 'color)))
										(format s "\"~a\" [label=\"~a\",fontname=arial];~%" fcn fcn-name)
										(when color
										  (format s "\"~a\" [color=~a,style=filled]~%" fcn color))
										(format s "\"~a-~a\" -> \"~a\"" p-rule-name i1 fcn)
										(format s "[style=~a,color=~a];~%" 
												(if (eq rule-edges add-edges) "dashed" "solid")
												(if (eq rule-edges add-edges) "red" "black"))
										(format s "\"~a-~a\" -> \"~a\"" p-rule-name i2 fcn)
										(format s "[style=~a,color=~a];~%"
												(if (eq rule-edges add-edges) "dashed" "solid")
												(if (eq rule-edges add-edges) "red" "black"))
										(format s "\"~a\" -> \"~a-~a\"" fcn p-rule-name o)
										(format s "[style=~a,color=~a];~%"
												(if (eq rule-edges add-edges) "dashed" "solid")
												(if (eq rule-edges add-edges) "red" "black")))))
								   (t
									(dotimes (i l)
									  (let ((rule-node (nth i rule-edge)))
										(format s "\"~a-~a\" [label=\"~a\",fontname=arial];~%" p-rule-name rule-node rule-node)))
									(dotimes (i l)
									  (let ((rule-node (nth i rule-edge)))
										(format s "\"~a-~a\"" p-rule-name rule-node)
										(when (< i (- l 1))
										  (format s " -> "))))
									(format s "[style=~a,fontname=arial,color=~a];~%"
											(if (eq rule-edges add-edges) "dashed" "solid")
											(if (eq rule-edges add-edges) "red" "black")))))))))
						(dump-gv-nodes)
						
						#|
					   (when (memq graph-type '(:digraph :subgraph))
						(format s "\"~a\" [label=\"~a\"];~%" rule rule))
					   |#

						(when (eq graph-type :single-graph)
									  (let ((rule-entry-node (designated-rule-entry-node rule)))
										(format s "\"~a\" -> \"~a\" [style=dotted,color=blue;weight=50];~%" 
												(get-node-name rule) (get-node-name rule-entry-node))))

						(let ((nested-rules (! (g hget-all) rule 'nested-rule)))
						  (dolist (nested-rule nested-rules)
							(let ((nested-rule-name (! (g hget) nested-rule 'name)))
							  (dump-gv-rule nested-rule)
							  (format s "\"~a\" -> \"~a\" [label=\"~a\"];~%" rule nested-rule  "NESTED-RULE"))))
						(when (memq graph-type '(:digraph :subgraph))
						  (format s "}~%"))
						(setq nest-prefix (- nest-prefix 1)))))))
			  (let ((*print-case* :downcase))
				(block b
				  (dump-gv-notes)
				  (format s "digraph G {~%")
				  (when gv-graph-props
					(format s "~a" gv-graph-props))
				  (! (g dump-edges) :edges edges :dump-fcn #'dump-gv-edges-data :sort nil)
				  (dump-gv-nodes)
				  (when (memq graph-type '(:digraph))
					(format s "}~%"))
				  (! (g dump-edges) :edges edges :dump-fcn #'dump-gv-edges-rules :sort nil)
				  (when (memq graph-type '(:subgraph :single-graph))
					(format s "}~%")))))))))))

;; n must be odd
;;
;; Writes an svg file of n cycles of the evolution of the 1-d binary cellular automaton given by rule-no. Rule numbering
;; follows Wolfram NKS. The CA is assumed to start from a single 1 or black cell, with all surrounding cells to the left
;; and right 0 or white. The set of cells upon which the ca operates is assmed to be infinitely enumerated in both
;; directions. This is how NKS works, but a formal description of the algorithm is not in the book and seems to be
;; implicit.  Thus the start cell is the "center". To get proper behavior in the infinite sequence of cells, we detect
;; when all-zeros (or all-ones) results in one (or zero), and flip the default for the remainder of the infinite
;; values. Lack of such detection is a flaw in the rule-30 h-machine rule; thus that can only do even numbered rules.
;;
;; When colorized is true, each cell's color is assigned based on the rule which produced it.
;;
;; Examples:
;;
;; (ca-to-svg "y.svg" 101 30 :colorized t)		;; 101 levels of rule 30, with colors
;; 
;; Experiments so far show that 1001 levels is pushing it for browsers and is really slow. The svg file in that case is a
;; good 74M.  A good large size which is tolerably slow is 401.

(defun ca-to-svg (file n rule-no &key colorized (viewbox '(10000.0 10000.0)) xlate)
  (with-open-file (s file :direction :output)
	(defr
	  (defl make-rule-array (rule-no)
		(let ((r (make-array '(2 2 2))))
		  (dotimes (i 2)
			(dotimes (j 2)
			  (dotimes (k 2)
				(setf (aref r i j k) (mod rule-no 2))
				(setq rule-no (floor (/ rule-no 2))))))
		  r))
	  (defl make-rule-color-array ()
		(let ((r (make-array '(2 2 2))))
		  (let ((colors (make-array 8 :initial-contents '(cyan magenta green red turquoise blue pink orange))))
			(let ((c 0))
			  (dotimes (i 2)
				(dotimes (j 2)
				  (dotimes (k 2)
					(setf (aref r i j k) (aref colors c))
					(setq c (+ c 1)))))))
		  r))
	  (defl write-line (a ca ycoord)
		(let ((y (* ycoord 20)))
		  (dotimes (i (length a))
			(let ((xcoord i))
			  (let ((v (if colorized
						   (aref ca xcoord)
						   (if (= (aref a xcoord) 0) "cyan" "magenta"))))
				(let ((x (* xcoord 20)))
				  (let ((rad 10))
					(let ((node (gensym)))
					  (format s "<g id=\"~a\" class=\"node\"><title>~a</title>~%" node node)
					  (format s "<polygon fill=\"~a\" stroke=\"~a\" points=\"~a,~a ~a,~a ~a,~a ~a,~a\"/>~%"
							  ;; (if (= v 0) "cyan" "magenta") (if (= v 0) "cyan" "magenta")
							  v v
							  (- x rad) (- y rad)
							  (- x rad) (+ y rad)
							  (+ x rad) (+ y rad)
							  (+ x rad) (- y rad))
					  (format s "</g>~%")))))))))
	  (let ((vx (first viewbox)))
		(let ((vy (second viewbox)))
		  (let ((xlate (or xlate (/ vx 2))))
			(let ((header `("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
							"<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
							"\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
							"<svg"
							,(format nil "viewBox=\"0.00 0.00 ~a ~a\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">" vx vy)
							;; "viewBox=\"0.00 0.00 20000.00 20000.00\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">"
							,(format nil "<g id=\"graph0\" class=\"graph\" transform=\"scale(1 1) rotate(0) translate(  ~a 30.00)\">" xlate))))
							;; "<g id=\"graph0\" class=\"graph\" transform=\"scale(1 1) rotate(0) translate(  10000.00 30.00)\">")))
			  (dolist (h header)
				(format s "~a~%" h))
			  (let ((r (make-rule-array rule-no)))
				(let ((color-array (make-rule-color-array)))
				  (let ((a0 (make-array n :initial-element 0)))
					(let ((a1 (make-array n :initial-element 0)))
					  (let ((ca (make-array n :initial-element 'cyan)))
						(let ((c (/ (- n 1) 2)))
						  (setf (aref a0 c) 1)
						  (let ((default-l 0))
							(let ((default-r 0))
							  (dotimes (j (/ (- n 1) 2))
								;; (print a0)
								(write-line a0 ca j)
								(let ()
								  (dotimes (i n)
									(cond
									 ((= i 0)
									  (let ((is-all-x (and (= default-l (aref a0 0) (aref a0 1))
														   (= (aref r default-l (aref a0 i) (aref a0 (+ i 1))) (- 1 default-l)))))
										(setf (aref a1 i) (aref r default-l (aref a0 i) (aref a0 (+ i 1))))
										(setf (aref ca i) (aref color-array default-l (aref a0 i) (aref a0 (+ i 1))))
										(when is-all-x
										  (setq default-l (- 1 default-l)))))
									 ((= i (- n 1))
									  (let ((is-all-x (and (= (aref a0 (- i 1)) (aref a0 i) default-r)
														   (= (aref r (aref a0 (- i 1)) (aref a0 i) default-r) (- 1 default-r)))))
										(setf (aref a1 i) (aref r (aref a0 (- i 1)) (aref a0 i) default-r))
										(setf (aref ca i) (aref color-array (aref a0 (- i 1)) (aref a0 i) default-r))
										(when is-all-x
										  (setq default-r (- 1 default-r)))))
									 (t 
									  (setf (aref a1 i) (aref r (aref a0 (- i 1)) (aref a0 i) (aref a0 (+ i 1))))
									  (setf (aref ca i) (aref color-array (aref a0 (- i 1)) (aref a0 i) (aref a0 (+ i 1)))))))
								  (dotimes (i n)
									(setf (aref a0 i) (aref a1 i)))))))))))))
			  (format s "</g>~%")
			  (format s "</svg>~%"))))))))

;; The transcript/log from perf -- look for fftperf and rule30perf in test.lisp -- has three main sections of interet,
;; the perf-stats output, the rule-stats output, and the output of (room t). Perf indiciators are marked with "||" and
;; rule stat indiciators are marked with "|". These disambiguate the names such that we don't need heuristics based on
;; spaces. Can't easily change the sytstem output. The entry for a given var specs a section type as well as a colno to
;; grab.
;;
;; I considered adding this info within perf-stats, but that doesn't seem to buy anything, since that already specs a
;; set of names which are refs to the defined names, which are scattered throughout the code. Trying to unify these refs
;; does not seem to be useful.  The next step I suppose is to unify based on classes of stats or something.
;;
;; plot-log takes a data subclass, e.g: 
;;
;;		(setq gp (make-gnuplot)) => #<COMPILED-FUNCTION MAKE-GNUPLOT-1>
;;		(! (gp plot-log) (make-rule-30-perf-stats-info))
;;
;; If do-grep is false, it won't scrape the log, just use the last data files made and refresh the graphics.
;;

(defc gnuplot nil nil
  (let ((gp-prog "C:/Program Files/gnuplot/bin/gnuplot.exe"))
	  ;; Prints out the command line need to get the info out of the transcript file, and prints the needed gnuplot
	  ;; commands to produce a window per graph
	  (defm plot-log (info &key (do-grep t))
		(defr
		  (let ((log-file (! (info get-filename))))
			(let ((l (! (info get-vars))))
			  (let ((grep-cmd-file "gp-grep-log-cmd.sh"))
				(let ((plot-cmd-file "gp-plot-cmd"))
				  (let ((tmp-file-root "gp-tmp"))
					(with-open-file (s grep-cmd-file :direction :output)
					  (let ((i 1))
						(dolist (e l)
						  (let ((name (first e)))
							(let ((token-no (second e)))
							  (let ((section (third e)))
								(format s "grep \"~a~a~a\" \"~a\" | gawk '{ print $~a; }' >~a~a ~%" 
										(if (eq section :perf) "||"
											(if (eq section :rules) "|"
												""))
										(symbol-name name)
										(if (eq section :perf) "     "
											(if (eq section :rules) " " 
												""))
										log-file token-no tmp-file-root i)
								(setq i (+ i 1))))))))
					(with-open-file (s plot-cmd-file :direction :output)
					  (let ((i 0))
						(let ((m 7))
						  (let ((H 250))	;; 500 Normal on rosencrantz
							(let ((V 125))	;; 300 Normal on rosencrantz
							  (dolist (e l)
								(format s "set terminal win ~a size ~a,~a position ~a,~a~%" i
										H V
										(+ (* (mod i m) (+ H 15)) 20)
										(+ (* (+ V 50) (floor (/ i m))) 20))
								(format s "plot \"~a~a\" with lines title \"~a\" ~%" tmp-file-root (+ i 1) (first e))
								(setq i (+ i 1))))))))
					(when do-grep
					  (shell-script-file grep-cmd-file))
					(shell-cmd (format nil "\"~a\" ~a -" gp-prog plot-cmd-file)))))))))))

(defc base-perf-stats-info nil ()
  (let ((filename ""))
	(let ((vars
		   '(
			 (main								  2 :perf) ;; log entry to search for, token number to extract via gawk, section of log: one-of { :perf :rules :room }
			 (me-tested							  5 :perf)
			 (me-matched						  5 :perf)
			 (me-failed							  5 :perf)
			 (me-matched-new-edges				  5 :perf)
			 (me-matched-not-new-edges			  5 :perf)
			 (me-efficiency						  2 :perf)
			 (me-redundancy						  2 :perf)
			 (me-failure						  2 :perf)
			 (all-matches						  2 :perf)
			 (all-matches-aux2					  2 :perf)
			 (possible-match-fcn				  2 :perf)
			 (possible-match					  2 :perf)
;;			 (expand-rule-obj-edges				  2 :perf)
;;			 (var-match-filter-edges			  2 :perf)
			 (env-no-conflict-dedup				  2 :perf)
			 (add-consequent-edges				  2 :perf)
;;			 (env-triggered-insert				  2 :perf)
;;			 (already-env-triggered-rules		  5 :perf)
			 (add-subqets						  2 :perf)
			 (num-edges-from-subqet				  2 :perf)
			 (get-edges-from-subqet				  2 :perf)
			 (count-edges-from-subqet			  2 :perf)
			 (count-edges-from-subqet			  5 :perf)
			 (count-edges-from-subqet			  4 :perf)
;;		 	 (count-edges-from-subqet-cached	  4 :perf)
			 (subst-match						  2 :perf)
			 (subst-match						  5 :perf)
			 (subst-match						  4 :perf)
			 (match								  2 :perf)
			 (match								  5 :perf)
			 (match								  4 :perf)
			 (find-min-edges-k					  2 :perf)
			 (find-min-edges-k					  5 :perf)
			 (match-num-envs					  2 :perf)
		 	 (match-num-envs					  3 :perf)
			 (match-doloop-cnt					  2 :perf)
		 	 (match-doloop-cnt					  3 :perf)
			 (cross-aux2						  2 :perf)
;;			 (match-pat-obj-edge-lists			  2 :perf)
			 (match-and-execute-rule-true		  2 :perf)
			 (match-and-execute-rule-false		  2 :perf)
			 (execute-obj						  2 :perf)
			 )))
	  (defm get-vars ()	;; Must be overridden
		vars)
	  (defm get-filename ()
		filename))))

(defc rule-30-perf-stats-info base-perf-stats-info nil
  (let ((vars
		 '(
		   (rule-30-next-rule-0-0-1  3 :rules)		;; The desired col is "tested"
		   (rule-30-max-rule-0-1     3 :rules)
		   (rule-30-zero-rule-1-1    3 :rules)
		   (rule-30-center           3 :rules)
		   )))
	(defm init ()
	  (setq filename "rule30perf"))
	(defm get-vars ()
	  (append (base-perf-stats-info-get-vars) vars))))

(defc fft-perf-stats-info base-perf-stats-info nil
  (let ((vars
		 '(
		   (fft-comb-rule-next   3 :rules)
		   )))
	(defm init ()
	  (setq filename "fftperf"))
	(defm get-vars ()
	  (append (base-perf-stats-info-get-vars) vars))))

(defc fe-perf-stats-info base-perf-stats-info nil
  (let ((vars
		 '(
		   (fwd-fe-rule		3 :rules)
		   (copy-rule-rule	3 :rules)
		   )))
	(defm init ()
	  (setq filename "feperf"))
	(defm get-vars ()
	  (append (base-perf-stats-info-get-vars) vars))))

;; Local Variables:
;; eval: (emacs-file-locals)
;; End:
