(defun main(archivo resp)
	;(readFile archivo)
	(setq example '((A B C) ((0 1 2 3 4 5 6 7 8 9) (2 4 6 8) (1 3 5 7 9)) ((> A B) (= A C) (= B(− C 1)))))
    (setq var (car example))
    (setq dom  (cadr example))
    (setq rest (caddr example))
    ;(rtoTable rest dom ())
	  (elimVar (arcoCon (domCon V (rtoTable R) D)))
)

(defun rtoTable (R D res) 
    "Documentation for rtoTable."
    (cond ((null R) res)
      (t (rtoTable (cadr R) D (actRes (car R) D res)) 

        )
    )
)
(defun actRes (R D res)


)
(defun domCon(V R D)
  (cond ( (null D)  )
    (t (domCon V R (actDom R D) )) 
    )
)





;Sacada de http://www.gigamonkeys.com/book/files-and-file-io.html
(defun readFile(archivo res)
	(let ((in (open archivo :if-does-not-exist nil)))
  		(when in
    		(loop for line = (read-line in nil)
         		while line do (print line )
         	)
    		(close in)
    	)
    )
)