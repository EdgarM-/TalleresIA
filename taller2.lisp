;Edgar Manuel Amezquita
;Octavo Semestre
;Taller 2 Inteligencia Artificial

;Ejemplos
;'(forall (x y) (imp (and (P x) (Q y)) (forall x (R x))))
; 
;Funcion principal donde llama las funciones necesarias para poder pasar la formula a FNC
(defun main(exp)
	(disAndOr (applyNeg (resolveImp (resolveDoubleI (resolveEx (resolveFA exp ))))))
)

(defun resolveFA(exp)
	(newresolveFA exp nil nil nil)
	)
;Resuelve el para todo haciendo el renombramiento de variable cuando es necesario, revisando las variable que se usaron previamente 
(defun newresolveFA(exp var reemp vars)
	(cond ((equal (car exp) forall) (append ))
		)	
	)
;Revisa si existe un elemento en una lista
(defun in(c lista)
	(cond ((null lista) nil)
		((equal c (car lista)) t)
		(t (in c (cdr lista)))
		)
)
;Resuelve la doble implicacion cambiando la por dos implicaciones en ambos sentidos.
(defun resolveDoubleI (exp)
	(cond ((equal (car exp) 'sys) (cons 'and (list (list 'imp  (resolveDoubleI (cadr exp)) (resolveDoubleI (caddr exp))) (list 'imp  (resolveDoubleI (caddr exp)) (resolveDoubleI (cadr exp))))))
		((not (in (car exp) '(sys imp or and not))) (append exp))
		(t (append (car exp)))

		)
	)
;Resuelve la implicacion cambiandola por no p o q.
(defun resolveImp (exp)
	(cond ((equal (car exp) 'imp) (append (list 'or) (list (list 'not  (resolveImp (cadr exp)))) (resolveImp (caddr exp))))
		((not (in (car exp) '(or and not))) (cons exp))
		(t (append (list (car exp) (resolveImp (cadr exp)) (resolveImp (caddr exp)))))
		)
	)

;Llama la funcion auxiliar con los parametros iniciales para calcular la negacion 
(defun applyNeg (exp)
	(newapplyNeg exp 0)
)
;Aplica la negacion donde esta se encuentre y la distribuye

(defun newapplyNeg (exp flag)
	(cond ((and (equal (car exp) 'not) (equal flag '0)) (newapplyNeg (cadr exp) '1))
		((and (equal (car exp) 'not) (equal flag '1)) (newapplyNeg  (cadr exp) '0))
		((and (not (in (car exp) '(or and))) (equal flag '0)) (append exp))
		((and (not (in (car exp) '(or and))) (equal flag '1)) (append (list 'not exp)))
		((and (equal (car exp) 'and) (equal flag '1)) (append (list 'or (newapplyNeg (cadr exp) '1) (newapplyNeg (caddr exp) '1))))
		((and (equal (car exp) 'or) (equal flag '1)) (append (list 'and (newapplyNeg (cadr exp) '1) (newapplyNeg (caddr exp) '1))))
		(t (append (list (car exp) (newapplyNeg (cadr exp) '0) (newapplyNeg (caddr exp) '0))))

		)
	)	

;Llama a la funcion newdisAndOr con lo parametros iniciales
(defun disAndOr (exp)
	;(newdisAndOr exp 0 0 )
)
;Distribuye and/or
;(defun newdisAndOr (exp andF orF)

;	)