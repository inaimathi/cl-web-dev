;;;; cl-web-dev.lisp
(in-package #:cl-web-dev)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; General utility
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body then-form)
  `(aif ,test-form (progn ,@then-form)))

(defmacro with-overwrite (stream file-name &body body)
  (with-gensyms (fname)
    `(let ((,fname ,file-name))
       (ensure-directories-exist ,fname)
       (with-open-file (,stream ,fname :direction :output :if-exists :supersede :if-does-not-exist :create)
	 ,@body))))

(defmethod to-file ((fname string) dat) 
  (to-file (pathname fname) dat))

(defmethod to-file ((fname pathname) (dat string))
  (with-overwrite stream fname
    (format stream dat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Hunchentoot-related
(defun easy-start (port &optional static-dir)
  (let ((server (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))))
    (when static-dir
      (push (hunchentoot:create-folder-dispatcher-and-handler "/static/" (concatenate 'string static-dir "/"))
	    hunchentoot:*dispatch-table*))
    server))

(defmacro define-handler (name args &body body)
  `(define-easy-handler (,name :uri ,(string-downcase (concatenate 'string "/" (symbol-name name)))) ,args
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; HTML-related 
(defmacro html-str (&body body)
  "Shortcut for with-html-output-to-string."
  `(with-html-output-to-string (*standard-output*)
     ,@body))

(defmacro html (&body body)
  "Shortcut for with-html-output."
  `(with-html-output (*standard-output*)
     ,@body))

(defun scripts (&rest files)
  "Shortcut for declaring js includes on the front-end."
  (html (dolist (f files)
	  (htm (:script :type "text/javascript"
			:src (concatenate 'string "/static/js/" f))))))

(defun styles (&rest files)
  "Shortcut for declaring CSS includes on the front-end."
  (html (dolist (f files)
	  (htm (:link :rel "stylesheet" :type "text/css"
		      :href (concatenate 'string "/static/css/" f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Parenscript-related
;;;;;;;;;;;;;;;;;;;; JS basics
(defpsmacro obj->string (thing)
  `(chain -j-s-o-n (stringify ,thing)))

(defpsmacro string->obj (thing)
  `(chain j-query (parse-j-s-o-n ,thing)))

(defpsmacro fn (&body body) `(lambda () ,@body))

;;;;;;;;;;;;;;;;;;;; jQuery Basics
(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector) ,@chains))

(defpsmacro $int (selector &optional (start 0))
  `(parse-int (chain ($ ,selector (text)) (substring ,start))))

(defpsmacro $float (selector &optional (start 0))
  `(parse-int (chain ($ ,selector (text)) (substring ,start))))

(defpsmacro doc-ready (&body body) 
  `($ document (ready (fn ,@body))))

(defpsmacro $map (lst &body body)
  `(chain j-query (map ,lst (lambda (elem i) ,@body))))

(defpsmacro $grep (lst &body body)
  `(chain j-query (grep ,lst (lambda (elem i) ,@body))))

(defpsmacro $post (uri arg-plist &body body)
  `(chain j-query 
	  (post ,uri (create ,@arg-plist)
		(lambda (data status jqXHR)
		  (let ((res (string->obj (@ jqXHR response-text))))
		    ,@body)))))

(defpsmacro $highlight (target)
  `($ ,target (stop t t) (effect :highlight nil 500)))

(defpsmacro $droppable (target &rest class/action-list)
  `($ ,target (droppable 
	       (create 
		:drop (lambda (event ui)
			(let ((dropped (@ ui helper context)))
			  ;; not sure if this should be a cond or a list of independent whens
			  (cond ,@(loop for (class action) in class/action-list
				     collect `(($ dropped (has-class ,class)) ,action)))))))))

(defpsmacro $draggable (target (&key revert handle cancel) &body body)
  `($ ,target (draggable (create :stop (lambda (event ui) ,@body)
				 ,@(when revert `(:revert ,revert))
				 ,@(when handle `(:handle ,handle))
				 ,@(when cancel `(:cancel ,cancel))))))

(defpsmacro $click (target &rest body)
  `($ ,target (click (lambda (event) ,@body))))

(defpsmacro $right-click (target &rest body)
  (with-gensyms (fn)
    `(let ((,fn (lambda (event) ,@body)))
       ($ ,target
	  (bind :contextmenu
		(lambda (event)
		  (,fn event)
		  (chain event (prevent-default))))
	  (bind :oncontextmenu
		(lambda (event)
		  (,fn event)
		  (setf (@ window event return-value) false)))))))

(defpsmacro event-source (uri &body name/body-list)
  (with-gensyms (stream handlers ev)
    `(let ((,stream (new (-event-source ,uri)))
	   (,handlers (create ,@(loop for (name . fn-body) in name/body-list
				  collect `,name collect `(lambda (ev) ,@fn-body)))))
       (setf (@ ,stream onopen) (lambda (e) (log "Stream OPENED!"))
	     (@ ,stream onerror) (lambda (e) (log "Stream ERRORED!" e))
	     (@ ,stream onmessage)
	     (lambda (e) 
	       (let ((,ev (string->obj (@ e data))))
		 ((@ ,handlers (@ ,ev type)) ,ev))))
       ,stream)))