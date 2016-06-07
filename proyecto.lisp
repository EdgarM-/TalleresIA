;Edgar Manuel Amezquita
;Octavo Semestre
;Proyecto Inteligencia Artificial

;Funcion Principal
(defun main ()
	(testNet testSet (trainNet trainSet (initNet '( ((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) )  ( (0 0 0 0) (0 0 0 0) (0 0 0 0) )  ( (0 0 0 0) (0 0 0 0) (0 0 0) )) ( ) )))
)

;Inicializa la red con valores aleatorios entre -1 y 1
(defun initNet (net initialized)
	(cond ((null net) initialized)
		(t (initNet (cdr net) (append initialized (list (fillwithRandom (car net) ())))))
		)
	)

;Funcion auxiliar para llernar la lista de listas con valores aleatorios
(defun fillwithRandom (elems res)
	(cond ((null elems) res)
		(t (fillwithRandom (cdr elems) (append res (list (fillRandom (car elems) ())))))
		)
	)

;Funcion auxiliar para llenar una lista con valores aleatorios
(defun fillRandom (elems res)
	(cond ((null elems) res)
		(t (fillRandom (cdr elems) (append res (list (random-value)))))
		)
	)
;Funcion auxiliar que genera valores aleatorios entre -1 y 1, usando el random propio de lisp
(defun randomValue (x)
	(cond ((= (random 2)  1) (- (random 1.1)) ) 
		(t (random 1.0)) 
		)
	)
;Entrena la red, apartir de la red y un conjuto de ejemplo de entrenamiento
(defun trainNet (trainSet net)
	(cond ((null trainSet) net)
		(t (trainNet (cdr trainSet) (compare net (car trainSet))))
		)
	)
;Funcion auxiliar que compara el resultado del forward con el resultado esperado y si no es parecido usa el backward para corregir la red
(defun compare (net example)
	
	)

(defun forward (net example nLayers)
	(cond ((= nLayers 0) net)
		(t )
		)
	)
;Multiplica los pesos por los valores, de cada nodo 
(defun multiply (weights values res)
	(cond ((null weights) res)
		(t (multiply (cdr weights) values (append res (list (map 'list #' (lambda(x y) (* x y)) (car weights) values))) ))
		)
	)
;Auxiliar que llama a la funcion suma para cada sublista
(defun sumInterface (values res)
	(cond ((null values) res)
		(t (sumInterface (cdr values) (append res (list (sum (car values) 0 )))))
		)
	)
;Suma todos los elementos de una lista y devuelve el valor de la suma
(defun sum (sum res)
	(cond ((null sum) res)
		(t (sum (cdr sum) (+ (car sum) res)))
		)
	)
;Usando la funcion map, aplica la funcion de activacion a cada elemento de la lista y devuelve la lista con los valores
(defun activation(toApply)
	(map 'list #' (lambda(x) (/ 1 (+ 1 (exp (- x))))) toApply)	
	)


;Bases tomadas de https://www.gnu.org/software/emacs/manual/html_node/elisp/Comparison-of-Numbers.html, compara dos numeros dado un error.
(defun approx-equal (x y error)
  (or (= x y)
      (< (/ (abs (- x y))
            (max (abs x) (abs y)))
         error)
      )
  )

(defun testNet (testNet net)
	)


	