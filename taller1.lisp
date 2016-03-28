;Edgar Manuel Amezquita
;Octavo Semestre
;Taller 1 Inteligencia Artificial


;Funcion principal donde se leeria el archivo y se llama al metodo de eliminar Variables
(defun main(resp)
	;(readFile archivo)
	(setq example '((A B C) ((0 1 2 3 4 5 6 7 8 9) (2 4 6 8) (1 3 5 7 9)) ((> A B) (= A C) (= B(− C 1)))))
    (setq var (car example))
    (setq dom  (cadr example))
    (setq rest (caddr example))
    (rtoTable (varDom var dom ()) rest nil)
	  ;(elimVar (arcoCon (domCon V (rtoTable var rest dom ()) D)))
)

;Devuelve un lista de listas con la variables y su dominio en la forma ((A 1 2 3) (B 2 3 5) ... (X 5 6 7)) para saber el dominio de cada variable
(defun varDom (V D res)
  (cond ((null V) (inverse res ()))
    (t (varDom (cdr V) (cdr D) (cons (append (cons (car V) () ) (car D)) res) ))
    )
  )
;Invierte una lista 
(defun inverse (lista resp)
    (cond ((null lista) resp)
          (t (inverse (cdr lista)(cons (car lista) resp)))))
;Mapea a una tabla las restricciones dejandolas como las variable y luego los dominios que cumplen con la restriccion en su debido orden
(defun rtoTable (VD R res) 
    "Documentation for rtoTable."
    (cond ((null R) res)
      (t (rtoTable VD (cdr R) (actRes VD (car R) res)) 

        )
    )
)
;Actualiza la restriccion
(defun actRes (VD R res)
  (cond ((null R) res)
    (t checkRes(V (cadr R) (caddr R) (car R) D res))
   )
)
; Revisa la restriccion entre v1 y v2
(defun checkRes (VD V1 V2 R res)
  (cond ((null R) res)
    ((eval (R (elmDom V1 VD) (elmDom V2 VD))) (append  res))
    )

  )
;DA un elemento del dominio de la variable
(defun elmDom (V VD)

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