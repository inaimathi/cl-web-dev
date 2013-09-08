;;;; package.lisp
(defpackage #:cl-web-dev
  (:use #:cl #:parenscript)
  (:import-from #:hunchentoot #:define-easy-handler #:stop
		#:session-start #:session-value #:delete-session-value #:remove-session)
  (:import-from #:cl-who #:with-html-output-to-string #:with-html-output #:htm #:fmt #:str)
  (:export
   ;; general
   #:with-gensyms #:aif #:awhen #:with-overwrite #:with-append #:to-file
   
   ;; hunchentoot interaction
   #:define-handler #:easy-start #:stop #:session-start #:session-value #:delete-session-value #:remove-session

   ;; cl-who interaction
   #:html #:html-str #:html-prologue #:scripts #:styles #:htm #:fmt #:str

   ;; parenscript interaction (the cl-web-dev includer will still need parenscript)
   #:obj->string #:string->obj #:fn #:$exists? #:map-markup #:log #:*debugging?*
   #:$ #:$val #:$int #:$float #:$on #:$button #:$click #:$right-click #:$keydown
   #:$append #:$prepend #:$replace
   #:doc-ready #:$map #:$grep #:$highlight 
   #:$get #:$post #:$droppable #:$draggable))

