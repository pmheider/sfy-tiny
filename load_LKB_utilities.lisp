
(if (not (find-package 'snepslog))
    (load *load-sneps-command*))

(setf lkb::*show-parse-p* nil) 
(setf *predicate-relations-directory* '())

(defun sntell (x)
  (snepslog::tell x))

(defun clear-old-parse-variables () 
  (setf *parse-directory* '())

  )

(defun parse (sentence)
  (clear-old-parse-variables)

  ;; Parse the sentences 
  (lkb::do-parse-tty sentence) 

  (if (string-equal *sfy-parse-mode* "nested-lists")
      (parse-to-nested-lists)
    (format nil "ERROR:  Unknown parse mode selected.~%~%"))
  
  )

(defun parse-to-nested-lists ()

  ;; We can either just run on the first parse which LKB has ranked 
  ;; to a certain extent or just run the gamut 
  (if (> (length lkb::*parse-record*) 0)
      (let ((parses (length lkb::*parse-record*)))
	;; For every parse up to our limit 
	(loop for p in (subseq lkb::*parse-record* 0 parses) 
		       ;; Pull out the semantic representation 
		       ;; This m is frequently typed as psoa in the LKB 
		       ;; functions 
	    for m = (mrs::extract-mrs p) 
		    ;; If it exists, keep on chugging 
	    when m do 

	      ;;(lkb::show-parse p nil)
	      (format t "~A~%" p)

	      (let ((scopes (length (mrs::make-scoped-mrs m)))) 

		;; Pierce's function to cache *bindings* 
		(scope-mrs m) 

		;; Pierce's function to create a hash of handels and their 
		;; relations 
		(index-handels m)

		;(format t "~A~%" m)

		;; Create a SNePS representation from the MRS      
		(mrs::output-mrs m 'mrs::indexed) 

		(create-predicate-lists m)

		;; Add each equivalent handle to the SNePS network 
		;; via sneps:tell "Qeq(hX, hY)."
		;(create-qeqs)
		
		) 
	      ) 
	)
    )
  
  (if (not *sfy-quiet-parse*)
      *parse-directory*)
  )

(defun create-predicate-lists (m) 
  (setf *current-parse* '())
  
  (loop 
      for r in (mrs::psoa-liszt m) 
      for h = (mrs::rel-handel r) 
      do 
	;; Research loop syntax and possibly remove this let 
	;; TODO 
	(let ((fpred (string-upcase (mrs::rel-pred r))))

	  (setf argument-list '())
	  
          (loop  
	      for fp in (mrs::rel-flist r)  
	      do  
		;; TODO  
		(let ((fpv (mrs::fvpair-value fp)))  
                  (if (typep fpv 'string)
		      (setf argument-list (cons fpv argument-list))
		    (setf argument-list (cons (intern
					       (string-upcase
						(mrs::var-string fpv)))
					      argument-list)))
		  )
		)
	  
	  (setf argument-list (reverse argument-list))
	  
	  (setf predicate-data (cons (intern
				      (string-upcase (mrs::var-string h)))
				 (cons (intern fpred)
				       (cons argument-list '()))))
	  )
	
	(setf *current-parse*
	  (cons predicate-data *current-parse*))
	
	)
  
  (setf *parse-directory*
    (cons
     (cons *current-parse* (create-qeqs))
     *parse-directory*))
  )

(defun mrs-to-sneps (predicate-list)
  (loop
   for current-predicate in predicate-list
   do

   (let ((semantic-forms (car current-predicate))
	 (ambiguity-mappings (car (cdr current-predicate))))
   
     (loop
      for current-semantic-form in semantic-forms
      do
      
      (let ((handle (car current-semantic-form))
	    (predicate-name (car (cdr current-semantic-form)))
	    (predicate-arguments (car (cdr (cdr current-semantic-form)))))

	(let ((argument-count (length predicate-arguments)))

	  (let ((full-predicate-name (format nil "~A_~A"
					     predicate-name
					     argument-count)))

	    (let ((prior-relations (assoc (intern full-predicate-name)
					  *predicate-relations-directory*))
		  (generic-argument-list "")
		  (predicate-argument-list ""))

	      (loop
	       for argument in predicate-arguments
	       do

	       (setf generic-argument-list (format nil "~A ~A"
						   generic-argument-list
						   (gensym "ARG")))
	       (setf predicate-argument-list
		     (format nil "~A,~A"
			     predicate-argument-list
			     (gensym 
			      (format nil "~A" argument)))))

	      (if (not prior-relations)
		  (let ((frame-definition
			 (format nil "define-frame ~A(PRED_NAME~A)."
				 full-predicate-name
				 generic-argument-list)))
		    
		    (format t "~A~%" frame-definition)
		    (sntell (format nil "~A~%" frame-definition))

		    (setf prior-relations '())
		    ))
				 
	      (setf *predicate-relations-directory*
		    (acons (intern full-predicate-name)
			   (cons predicate-arguments
				 prior-relations)
			   *predicate-relations-directory*))
	  
	      (format t "~A(~A~A).~%"
		      full-predicate-name
		      predicate-name
		      predicate-argument-list)
	      (sntell (format nil "~A(~A~A).~%"
		      full-predicate-name
		      predicate-name
		      predicate-argument-list))

	      )

	    )
	  )
	)
      )

     (format t "  ~A~%" ambiguity-mappings))
   
   )
  )

(defun create-qeqs ()
  (setf qeq-directory '())
  
  (loop
      for b in (subseq *bindings* 0)
      do
	(let ((bow (first b))
	      (stern (cdr b)))
	  (if (not (= bow stern))
	      (let ((h-form (cons
			     (intern (format nil "H~A" bow))
			     (cons
			      (intern (format nil "H~A" stern)) '()))))
		(setf qeq-directory (cons h-form
					  qeq-directory)))
	    )
	  )
	)
  
  (cons qeq-directory '())
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;; 
;;;;  FUNCTIONS THAT WOULD BE GOOD TO USE
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; RELOAD-LEX-FILES

;; SHOW-LEX-TTY

;; SHOW-TYPE-TTY

;; SHOW-WORDS-TTY

;; APPLY-LEX-TTY

;; INDEX-FOR-GENERATOR
;; DO-GENERATE-TTY
;; SHOW-GEN-RESULT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;; 
;;;;  FUNCTIONS FROM PIERCE'S ORIGINAL CODE     
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; Caches the bindings for the current MRS structure.  The bindings 
;; are stored in an alist mapping between handle ids: 
;; 
;;  ((1 . 1) (3 . 3) (6 . 3) (5 . 5) (18 . 5) (9 . 9) (16 . 9) 
;;   (13 . 13) (15 . 13) (14 . 14) (7 . 14)) 
;; 
(defun scope-mrs (psoa) 
  (setf *bindings* 
    (mrs::canonical-bindings 
     (first (mrs::make-scoped-mrs psoa))))) 

;; Caches the mapping from handle ids to relations for the current MRS 
;; structure.  Two indices are constructed: (1) a mapping from handle 
;; ids to handle objects, (2) a mapping from handles to lists of 
;; relations.  The first supports FOLLOW-HANDEL, and the second 
;; supports HANDEL-RELATIONS. 
(defun index-handels (psoa) 
  (setf *handel-rlist* (make-hash-table)) 
  (setf *handel-index* (make-hash-table :test #'eql)) 
  (loop 
      for r in (mrs::psoa-liszt psoa) 
      for h = (mrs::rel-handel r) 
      do 
	(setf (gethash (mrs::var-id h) *handel-index*) h) 
	    (push r (gethash h *handel-rlist*)))) 
