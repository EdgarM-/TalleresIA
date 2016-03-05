;Edgar Manuel Amezquita
;Octavo Semestre
;Taller 1 Inteligencia Artificial

(defun main(archivo resp)
	;(readFile archivo)
	(setq example '((A B C) ((0 1 2 3 4 5 6 7 8 9) (2 4 6 8) (1 3 5 7 9)) ((> A B) (= A C) (= B(− C 1)))))
    (setq var (car example))
    (setq dom  (cadr example))
    (setq rest (caddr example))
    (rtoTable var rest dom nil)
	  ;(elimVar (arcoCon (domCon V (rtoTable var rest dom ()) D)))
)

(defun rtoTable (V R D res) 
    "Documentation for rtoTable."
    (cond ((null R) res)
      (t (rtoTable (cadr R) D (actRes V (car R) D res)) 

        )
    )
)

(defun actRes (V R D res)
  (cond ((null R) res)
    (t checkRes(V (cadr R) (caddr R) (car R) D res))
   )
)

(defun checkRes (V V1 V2 R D res)
  (print v1)
  (print v2)
  (print R)
  (print res)

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