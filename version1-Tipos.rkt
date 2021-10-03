#lang eopl
;; Sara María Eraso Lasso - 1924232
;; Lady Joana  Escobar Ortiz - 1910051
;; Juan Sebastián Estupiñán Cifuentes - 1924846

;; BNF

;;  <program>       ::= <globales> <expression>
;;                      a-program (glo exp)

;;  <globales>      ::= ({<identificador> = <expresion>}*(,))
;;                      global (ids body)

;;  <expresion>     ::= <entero>
;;                      entero-lit (num)
;;                  ::= <flotante>
;;                      flotante-lit (num)
;;                  ::= x8({<entero>}*)
;;                      octal-lit (nums)
;;                  ::= <identificador>
;;                      id-lit (id)
;;                  ::= &<identificador>
;;                      id-ref (id)
;;                  ::= "{<identificador>}*"
;;                      cadena-lit (body)
;;                  ::= '<caracter>'
;;                      caracter-lit (carac)
;;                  ::= var({<identificador> = <expresion>}*(,)) in <expresion>
;;                      var-exp (ids exps body)
;;                  ::= cons({<identificador> = <expresion>}*(,)) in <expresion>
;;                      cons-exp (ids exps body)
;;                  ::= rec{identificador ({identificador}*(,)){<expresion>}}* in <expresion>
;;                      rec-exp (proc-names idss bodies bodyrec)
;;                  ::= unic({<identificador> = <expresion>}*(,)) in <expresion>
;;                      unic-exp (ids exps body)
;;                  ::= [{<expresion >}*(,)]
;;                      lista-exp (exps)
;;                  ::=	vector({<expresion >} *(,))
;;                      vector-exp (exps)
;;                  ::= reg{{<identificador> = <expresion>}+(,)}
;;                      reg-exp (ids body)
;;                  ::= funcion({<identificador>}*(,)){<expression>}
;;                      definir-proc (ids body)
;;                  ::= invoca <expresion>({<expresion>}*(,))
;;                      invocar-proc (exp1 exps)
;;                  ::= funcion!({<identificador>}*(,)){<expression>}
;;                      definir-proc-rec (ids body)
;;                  ::= -> <identificador> = <expresion>
;;                      modificar-exp (id exp)
;;                  ::= <primitiva-unaria> <expresion>
;;                      evalprim-un-exp (prim-un exp)
;;                  ::= (<expresion> <primitiva-binaria> <expresion>)
;;                      evalprim-bin-exp (exp1 prim-bin exp2)
;;                  ::= octal<primitiva-octal-unaria> <expresion>
;;                      evalprim-octal-un-exp (prim exp)
;;                  ::= octalbin(<expresion> <primitiva-octal-binaria> <expresion>)
;;                      evalprim-octal-bin-exp (exp1 prim epx2)
;;                  ::= <cadena-primitive>({<expresion>}*(,))
;;                      cadena-primapp-exp (exps)
;;                  ::= vacio
;;                      vacio ()
;;                  ::= <lista-primitive>(<expresion>)
;;                      lista-primapp-exp (prim exp)
;;                  ::= <lista-primitive-s>({<expresion>}*(,))
;;                      lista-primapp-exp-s (prim exps)
;;                  ::= ref-vector <identificador>
;;                      vector-ref (id)
;;                  ::= vector?(<expresion>)
;;                      es-vector-exp (exp)
;;                  ::= crear-vector({<expresion>}*(,))
;;                      crear-vector-exp (exps)
;;                  ::= set-vector <identificador> = ({<expresion>}*(,))
;;                      set-vector-exp (id body)
;;                  ::= registro? (<expresion>)
;;                      es-registro-exp (exp)
;;                  ::= ref-registro <identificador>
;;                      registro-ref (id)
;;                  ::= crear-registro({<identificador> = <expresion>}*(,))
;;                      crear-registro-exp (ids body)
;;                  ::= set-registro <identificador> = ({<identificador> = <expresion>}*(,))
;;                      set-registro-exp (id ids body)
;;                  ::= <expresion-bool> 
;;                      exp-bool-exp (exp)
;;                  ::= sequence {<expresion> ;}+ end
;;                      seq-exp (exps)
;;                  ::= if <expresion-bool> then <expresion> else <expresion> end
;;                      if-exp (exp-bool exp1 exp2)
;;                  ::= cond {[(<expresion>) (<expresion>)]}* else <expresion>
;;                      cond-exp (cond-exps exps exp)
;;                  ::= while(<expresion-bool>) do <expresion> done
;;                      while-exp (exp-bool exp)
;;                  ::= for <identificador> = <expresion> <conteo-expresion> <expresion> do <expresion> done
;;                      for-exp (id exp1 cont-exp exp2 exp3)
;;                  ::= print(<expresion>)
;;                      print-exp (exp)

;;  <expresion-conteo>::= to
;;                        to-exp()
;;                    ::= downto
;;                        downto-exp()

;;  <expresion-bool>::= compare <expresion> <pred-prim> <expresion>
;;                      compare-booleano-exp (exp pim exp)
;;                  ::= <oper-bin-bool>(<expresion-bool>)(<expresion-bool>)
;;                      evalprim-booleano-bin-exp (prim exp1 exp2)
;;                  ::= true | false
;;                      booleano-lit (dato)
;;                  ::= <oper-un-bool> <expresion-bool>
;;                      evalprim-booleano-un-exp (prim exp)

;;  <primitiva-binaria> ::=  + | ¬ | * | % | /
;;  <primitiva-unaria>  ::= ++ | ¬¬

;;  <primitiva-octal-binaria> ::=  + | ¬ | * 
;;  <primitiva-octal-unaria>  ::= ++ | ¬¬

;;  <pred-prim>     ::= < | > | <= | >= | == | <>

;;  <oper-bin-bool> ::= and | or | xor

;;  <oper-un-bool>  ::= not

;;  <cadena-primitive> ::= longitud-cadena
;;                     ::= concatenar

;;  <lista-primitive> ::= vacio?
;;                    ::= lista? es-lista
;;                    ::= cabeza
;;                    ::= cuerpo

;;  <lista-primitive-s> ::= crear-lista
;;                      ::= append


(define lexica
  '(
    (epacio (whitespace) skip)
    (comentario ("//" (arbno (not #\newline)) ) skip)
    (identificador  (letter (arbno (or letter digit))) symbol)
    (entero (digit (arbno digit)) number)
    (entero ("-" digit (arbno digit)) number)
    (flotante (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (caracter ("'" letter "'") symbol)
    (cadena ("\"" (arbno (or letter whitespace digit)) "\"")  symbol)
    )
  )

(define gramatica

  '(
    (programa (globales expresion) un-programa)

    (globales ("global" "(" (separated-list identificador "=" expresion ",") ")") global)
    (expresion (cadena) cadena-lit)
    (expresion (caracter) caracter-lit)
    (expresion (entero) entero-lit)
    (expresion (flotante) flotante-lit)
    (expresion ("x8" "(" (arbno entero) ")") octal-lit)
    (expresion (identificador) id-lit)
    (expresion ("&" identificador) id-ref)

    ;;Para unic
    (expresion-unic ("C-VID-VAL") c_vid_val-lit)
    (expresion-unic (expresion) exp-unic)

    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion) var-exp)
    
    (expresion ("cons" (separated-list identificador "=" expresion ",") "in" expresion) cons-exp)
    
    (expresion ("id-cons" identificador) cons-id)
    
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "{" expresion "}")  "in" expresion) rec-exp)
    (expresion ("unic" (separated-list identificador "=" expresion-unic",") "in" expresion) unic-exp)
   
    (expresion ("[" (separated-list expresion ",")"]") lista-exp)
    (expresion ("vector" "(" (separated-list expresion ",")")") vector-exp)
    (expresion ("reg" "(" identificador "=" expresion "," (separated-list identificador "=" expresion ",") ")") reg-exp)
   
    (expresion ( "funcion" "(" (separated-list type-exp identificador ",") ")" "{" expresion "}" ) definir-proc)
    (expresion ( "invoca" expresion "(" (separated-list expresion ",") ")") invocar-proc)
    (expresion ( "funcion!" "(" (separated-list identificador ",") ")" "{" expresion "}" ) definir-proc-rec)
    (expresion ("->" identificador "=" expresion) modificar-exp)
   
    (expresion (primitiva-unaria expresion) evalprim-un-exp)
    (expresion ("(" expresion primitiva-binaria expresion ")") evalprim-bin-exp)
   
    (expresion ("octal" primitiva-octal-unaria expresion) evalprim-octal-un-exp)
    (expresion ("octalbin" "(" expresion primitiva-octal-binaria expresion ")") evalprim-octal-bin-exp)

    ;;Primitivas
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("¬") primitiva-resta)
    (primitiva-binaria ("*") primitiva-mult)
    (primitiva-binaria ("%") primitiva-residuo)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-unaria ("++") primitiva-incrementar)
    (primitiva-unaria ("¬¬") primitiva-disminuir)

    ;;Para octales
    (primitiva-octal-binaria ("+") primitiva-octal-suma)
    (primitiva-octal-binaria ("¬") primitiva-octal-resta)
    (primitiva-octal-binaria ("*") primitiva-octal-mult)
    (primitiva-octal-unaria ("++") primitiva-octal-incrementar)
    (primitiva-octal-unaria ("¬¬") primitiva-octal-disminuir)
   
    ;;Para cadenas  
    (cadena-primitive ("longitud-cadena") longitud-cadena)
    (cadena-primitive ("concatenar") concatenar)
    (expresion (cadena-primitive "(" (separated-list expresion ",")")") cadena-primapp-exp)
   
    ;;Para listas
    (expresion ("vacio") vacio-exp)
    (lista-primitive ("vacio?") es-vacio)
    (lista-primitive ("lista?") es-lista)
    (lista-primitive ("cabeza") cabeza)
    (lista-primitive ("cuerpo") cuerpo)
    (lista-primitive-s ("crear-lista") crear-lista)
    (lista-primitive-s ("append") append-l)
    (expresion (lista-primitive "(" expresion ")") lista-primapp-exp)
    (expresion (lista-primitive-s "(" (separated-list expresion ",")")") lista-primapp-exp-s)
   
    ;;Para vectores
    (expresion ("ref-vector" "(" entero ")" identificador) vector-reff)
    (expresion ("vector?" "(" expresion ")") es-vector-exp)
    (expresion ("crear-vector" "(" (separated-list expresion ",")")") crear-vector-exp)
   
    ;; Cambio
    (expresion ("vector-pos" identificador "(" entero ")" ) vector-pos-exp)
   
    ;; Modifica el elemento en la posicion n de un vector
    (expresion ("set-vector" identificador "(" entero "," expresion ")") set-vector-exp)
   
    ;;Para registros
    (expresion ("registro?" "(" expresion ")") es-registro-exp)
    (expresion ("ref-registro" identificador) registro-ref)
    (expresion ("crear-registro" "(" (separated-list identificador "=" expresion ",") ")") crear-registro-exp)

    ;; Cambio
    (expresion ("registro-pos" identificador "(" entero ")" ) registro-pos-exp)
    (expresion ("set-registro" identificador "(" entero "," expresion ")") set-registro-exp)

    ;;Para booleanos
    (expresion (expresion-bool) exp-bool-exp)
    (expresion-bool ("compare" expresion pred-prim expresion) compare-booleano-exp)
    (expresion-bool (oper-bin-bool "(" expresion-bool ")" "(" expresion-bool ")") evalprim-booleano-bin-exp)
    (expresion-bool ("true") booleano-lit-true)
    (expresion-bool ("false") booleano-lit-false)
    (expresion-bool (oper-un-bool "(" expresion-bool ")") evalprim-booleano-un-exp)
                    
    (pred-prim ("<") menor-que)
    (pred-prim (">") mayor-que)
    (pred-prim ("<=") menor-igual)
    (pred-prim (">=") mayor-igual)
    (pred-prim ("==") igual-igual)
    (pred-prim ("<>") diferente)
    (oper-bin-bool ("and") and-oper-bin)
    (oper-bin-bool ("or") or-oper-bin)
    (oper-bin-bool ("xor") xor-oper-bin)
    (oper-un-bool ("not") not-oper-un)

    (expresion ("sequence" expresion ";" (arbno expresion ";" ) "end") seq-exp)
    (expresion ("if" expresion-bool "then" expresion "else" expresion "end") if-exp) 
    (expresion ("cond" (arbno  "[" "(" expresion ")" "(" expresion ")" "]"  ) "else" expresion) cond-exp)
    (expresion ("while" "(" expresion-bool ")" "do"  expresion  "done") while-exp)
    (expresion ("for" identificador "=" expresion expresion-conteo expresion "do" expresion  "done") for-exp )
    (expresion-conteo ("to") to-exp)
    (expresion-conteo ("downto") downto-exp)
    (expresion ("print" "(" expresion ")" ) print-exp)


;adicionales
    (type-exp ("int") int-type-exp)
    (type-exp ("bool") bool-type-exp)
    (type-exp ("float") float-type-exp)
    (type-exp ("char") char-type-exp)
    (type-exp ("octal") octal-type-exp)
    (type-exp ("(" (separated-list type-exp "*") "->" type-exp ")")
              proc-type-exp)
    
    (expresion ("ffalse") false-exp)
    (expresion ("ttrue") true-exp)

    
    )
  )

(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))

;; Interpretador
(define interpretador
  (sllgen:make-rep-loop  "> "
                         (lambda (prog) (eval-program  prog)) 
                         (sllgen:make-stream-parser 
                          lexica
                          gramatica)))
(define interpretador-tipos
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (aux-interpretador  pgm)) 
    (sllgen:make-stream-parser 
      lexica
      gramatica)))

(define aux-interpretador
  (lambda (x)
    (if (type? (type-of-program x)) (eval-program x) 'error)))

;; eval-program:
(define eval-program
  (lambda (prog)
    (cases programa prog
      (un-programa (globals exps)
                   (eval-globales globals exps (global-env))))))

;; eval-globales: evalua las globales
(define eval-globales
  (lambda (glo exps env)
    (cases globales glo
      (global (idss bodies)
              (if (= (globales-repetidas? idss) 0)
                  (let ((args (eval-rands-unic-var-global bodies env)))
                    (eval-expresiones exps (extend-env idss args env)))
                  (eopl:error 'eval-globales "No puede tener globales repetidas: ~s" idss))
      ))))

;; Determina si las globales se repiten, el 1 representa sí el 0 no
(define (globales-repetidas? lista-ids)
  (if (null? lista-ids)
      0
      (if (<= (contar-ocurrencias (car lista-ids) lista-ids) 1)
          (globales-repetidas? (cdr lista-ids))
          1)))

;; aux: cuenta las ocurriencias de un elemento en una lista, si está una vez la ocurrencia es 1
(define (contar-ocurrencias elem lst)
  (cond
    [(null? lst) 0]
    [(list? (car lst))(+ (contar-ocurrencias elem (car lst))(contar-ocurrencias elem (cdr lst )))]
    [(eqv? (car lst) elem)(+ 1 (contar-ocurrencias elem (cdr lst)))]
    [else (contar-ocurrencias elem (cdr lst))]
    )
  )

;; Global-env: ambiente para las variables globales
(define global-env  
  (lambda ()
    (empty-env)))  

;; Evalua las expresiones
(define eval-expresiones
  (lambda (exp env) 
    (cases expresion exp

      ;; Tipos de datos
      (entero-lit (num) num)
      (caracter-lit (caract) caract)
      (flotante-lit (flot) flot)
      (cadena-lit (cadena) cadena)
      (id-lit (id) (re-apply-env env id))

      (octal-lit (oct)
                 (oct-exp-unparse oct env))

      (id-ref (id-ref) id-ref)
      
      (var-exp (ids exps body)
               (let ((args (eval-rands-unic-var-global exps env)))
                 (eval-expresiones body (extend-env ids args env))))
      
      (cons-exp (ids exps body)
                (let ((args (eval-rands exps env)))
                  (eval-expresiones body (extend-env ids args env))))

      (cons-id (ids)0)
      
      (rec-exp (proc-names idss bodies rec-body)
               (eval-expresiones rec-body
                                 (extend-env-recursive proc-names idss bodies env)))
      
      (unic-exp (ids exps body)
                (let ((args (eval-rands-unic-var-global exps env)))
                 (eval-expresiones body (extend-env ids args env))))


      (cadena-primapp-exp (prim exps)
                (let ((args (eval-rands exps env)))
                 (apply-cadena-primitive prim args)))

      (vacio-exp ()empty)
      

      ;; Constructores
      (lista-exp (exps)
               (car(list (eval-rands exps env))))

      (lista-primapp-exp (prim exp)
                       (apply-lista-primitive prim (eval-expresiones exp env)                       
                       ))
      (lista-primapp-exp-s (prim exps)
                         (let ((args (eval-rands exps env)))
                           (apply-lista-primitive-s prim args
                         ))) 
      
      (vector-exp (exps)
               (let ((args (eval-rands exps env)))
                 (list->vector args)))

      (es-vector-exp (exp)
               (if (vector? exp)#t #f))

      (crear-vector-exp (exps)
               (let ((args (eval-rands exps env)))
                 (list->vector args)))

      (vector-pos-exp (id pos)
                    (let ((vect (re-apply-env env id)))
                      (vector-ref vect pos)))
                   
      (set-vector-exp (id pos exp)
                 (let ((vect (re-apply-env env id)))
                      (vector-set! vect pos (eval-expresiones exp env))
                   ))
      ;; (reg-exp (id exp ids exps) id exp ids exps)

      ;; Primitivas
      (evalprim-bin-exp (exp1 prim-bin exp2) (eval-prim exp1 prim-bin exp2 env))
      (evalprim-un-exp (prim-un exp) (apply-primitiva-unaria prim-un (eval-rand exp env)))

      ;; Procedimientos
      (definir-proc (args-texps ids body)(closure ids body env))
      
      (invocar-proc (rator rands)
                    (let ((proc (eval-expresiones rator env))
                          (args (eval-rands rands env)))
                      (if (procedimiento? proc)
                          (apply-procedimiento proc args)
                          (eopl:error 'eval-expresiones "Intenta aplicar algo que no es un procedimiento: ~s" proc))))

      ;; Asignación de variables
      ;;(modificar-exp (id exp) (setref! (apply-env-ref env id) (eval-expresiones exp env)))
      (modificar-exp (id exp)
               (let ((once-eval (apply-env env id)))
                 (if (list? once-eval)
                 (cond
                   
                   ;;Para unic
                   [(eqv? (car once-eval) 'unic)
                    (if (eqv? (car (cdr once-eval)) "C-VID-VAL")
                             (setref! (apply-env-ref env id) (eval-expresiones exp env))
                         (eopl:error 'modificar-exp "No es una variable mutable ~s" id))]

                   ;;Para var
                   [(eqv? (car once-eval) 'var) (setref! (apply-env-ref env id) (list 'var (eval-expresiones exp env)))]
                   
                   ;;Para todo lo demás
                   [else (eopl:error 'modificar-exp "No es una variable mutable ~s" id)])
                 (eopl:error 'modificar-exp "No es una variable mutable ~s" id))))
    
      ;;  Estructuras de control
      (seq-exp (exp exps)
               (let loop ((acc (eval-expresiones exp env)) (exps exps))
                 (if (null? exps)
                     acc
                     (loop (eval-expresiones (car exps) env) (cdr exps)))))
      
      (if-exp (bool-exp true-exp false-exp)
              (if (apply-exp-bool bool-exp env)
                  (eval-expresiones true-exp env)
                  (eval-expresiones false-exp env)))
      
      (cond-exp (conds express eelse)          
                (auxiliar-cond conds express eelse env ))
      
       (while-exp (bool-exp exp)
                  (auxiliar-while bool-exp exp env))
      
       (for-exp (id exp1 conteo-exp exp2 exp3)
               (auxiliar-for id (eval-expresiones exp1 env)conteo-exp exp2 exp3 env))


      (exp-bool-exp (bool-ex)(apply-exp-bool bool-ex env))

      (print-exp (exp)(eval-expresiones exp env))
      
      (else "llegóaqui1")
      )))

;; Determina si es necesario evaluar nuevamente el id o no
(define re-apply-env
  (lambda (env id)
    (let ((id-evaluado (apply-env env id)))
      (if (list? id-evaluado)
    (cond

     ;;Para unic
     [(eqv? (car id-evaluado) 'unic)
      (if (eqv? (car (cdr id-evaluado)) "C-VID-VAL")
          "C-VID-VAL"
          (car (cdr id-evaluado)))]

     ;;Para var
     [(eqv? (car id-evaluado) 'var)
      (car (cdr id-evaluado))]
     )
     id-evaluado))))

;;___________________________________________________________________________

;; oct-exp-unparse: extrae la lista de una expresion octal
(define oct-exp-unparse
  (lambda (oct env)
    (eval-rands (make-struct-octal oct) env)))

(define make-struct-octal
  (lambda (rands)
    (map (lambda (x) (entero-lit x)) rands)))

;;___________________________________________________________________________

;; oct-parse: convierte un número en base 10 a base N
(define oct-parse
  (lambda (num N)
    (if(eqv? num 0) '()
       (cons (modulo num N) (oct-parse (quotient num N) N)))))

;; Ambiente extendido para rec
(define extend-env-recursive
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
           (lambda (pos ids body)
             (vector-set! vec pos (closure ids body env)))
           (iota len) idss bodies)
          env)))))

;; Funcion Iota
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
          (cons next (loop (+ 1 next)))))))

;;___________________________________________________________________________

;; Buscar un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
        (deref (apply-env-ref env sym))))

;; aply-env-ref
(define apply-env-ref
  (lambda (env id)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No se puede enlazar: ~s" id))
      (extended-env-record (ids vals env)
                           (let ((pos (rib-find-position id ids)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env id))))
      )))

;; Funciones auxiliares para encontrar la posición de un símbolo

;; Cuenta los elementos en una lista
(define contar-lista
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (contar-lista (cdr l)))
        )))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

;; Evaluar primitivas binarias
(define apply-primitiva-binaria
  (lambda (exp1 prim exp2)
    (if (and (number? exp1) (number? exp2))
    (cases primitiva-binaria prim
      (primitiva-suma () (+ exp1 exp2))
      (primitiva-resta () (- exp1 exp2))
      (primitiva-mult () (* exp1 exp2))
      (primitiva-div () (/ exp1 exp2))
      (primitiva-residuo () (remainder exp1 exp2)))
    (eopl:error 'apply-primitiva-binaria "No se puede realizar esta operación entre los argumentos dados ~s" (list exp1 exp2))
    )))

;; Evalua operadores en una operación
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

;; aux
(define eval-rand
  (lambda (rand env)
    (eval-expresiones rand env)))

;; Para unic y var
(define eval-rands-unic-var-global
  (lambda (rands env)
    (map (lambda (x) (eval-rand-unic-or-var x env)) rands)))

;; aux
(define eval-rand-unic-or-var
  (lambda (rand env)
    (if (expresion-unic? rand)
    (cases expresion-unic rand
        (exp-unic (expre) (list 'unic (eval-expresiones expre env)))
        (c_vid_val-lit () (list 'unic "C-VID-VAL")))
    (list 'var (eval-expresiones rand env)))
    ))

;; Funciones auxiliares para aplicar apply-primitiva-binaria
(define eval-prim
  (lambda (exp1 prim-bin exp2 env)
    (apply-primitiva-binaria (eval-rand exp1 env) prim-bin (eval-rand exp2 env))))

;; Evaluar primitivas unarias
(define apply-primitiva-unaria
  (lambda (prim exp)
    (if (number? exp)
    (cases primitiva-unaria prim
      (primitiva-incrementar () (+ exp 1))
      (primitiva-disminuir () (- exp 1)))
    (eopl:error 'apply-primitiva-unaria "No se puede realizar, no es el argumento esperado ~s" exp)
    )))

;; Procedimientos, clausura
(define-datatype procedimiento procedimiento?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))

;; apply-procedimiento
(define apply-procedimiento
  (lambda (proc args)
    (cases procedimiento proc
      (closure (ids body env)
               (if (eqv? (contar-lista args) (contar-lista ids))
                   (eval-expresiones body (extend-env ids args env))
                   (eopl:error 'apply-procedimiento "El número de argumentos esperado no coincide con el número dado: ~s" (contar-lista args))
                   )))))

;;___________________________________________________________________________

;; Referencias al vector (Store)
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;; Desreferenciar 
(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

;; Aux
(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;;___________________________________________________________________________

;; Estructuras de control
;; apply-pred-prim condicionales
(define apply-pred-prim
  (lambda (pred-pr args)
    (cases pred-prim pred-pr
      (menor-que () (if (< (car args) (cadr args) ) #t #f))
      (mayor-que () (if (> (car args) (cadr args) ) #t #f))
      (menor-igual ()(if (<= (cadr args) ) #t #f))
      (mayor-igual ()(if (>= (car args) (cadr args) ) #t #f))
      (igual-igual ()(if (= (car args) (cadr args) ) #t #f))
      (diferente ()(if  (not( = (car args) (cadr args) )) #t #f);revisar
                 ))))

;;Funcion auxiliar cond 
(define auxiliar-cond
  (lambda (conds express eelse env)
    (if (null? conds)
        (eval-expresiones eelse env)
        (if (eval-expresiones (car conds) env)
            (eval-expresiones (car express) env)
            (auxiliar-cond (cdr conds)(cdr express) eelse env)
            ))))
        
;;Funcion auxiliar while
(define auxiliar-while
  (lambda (bool-exp exp env)
    (if (apply-exp-bool bool-exp env)
        (cons(eval-expresiones exp env)
             (auxiliar-while bool-exp exp env))
        empty)))

;;Función auxiliar for
(define auxiliar-for
  (lambda (id num_i conteo-exp limite exp3 env)
      (if (equal? num_i (eval-expresiones limite env))
           (eval-expresiones exp3 (extend-env (list id) (list num_i) env))
           (cons (eval-expresiones exp3 (extend-env (list id) (list num_i) env)) (auxiliar-for id (nuevo-arg conteo-exp num_i) conteo-exp limite exp3 env)
         ))))
                                                                 
;; Funcion nuevo-arg : función auxiliar de auxiliar-for -> sirve para evaluar to/downto 
(define nuevo-arg 
    (lambda (conteo num )
       (apply-expresion-conteo conteo num)))

;;apply-exp-bool estructuras booleanas   
(define apply-exp-bool
  (lambda (exp-b env)
    (cases expresion-bool exp-b
      (compare-booleano-exp (exp1 pred-prim exp2)
                            (let ((args (eval-rands (list exp1 exp2) env)))
                              (apply-pred-prim pred-prim args)))

      (evalprim-booleano-bin-exp (oper-bin exp-b1 exp-b2)
                                 (let ((args (list (apply-exp-bool exp-b1 env)(apply-exp-bool exp-b2 env))))
                                   (apply-oper-bin-bool oper-bin args)))

      (booleano-lit-true ()#t)

      (booleano-lit-false ()#f)

      (evalprim-booleano-un-exp (oper-un exp-b1)
                                (let ((arg (apply-exp-bool exp-b1 env)))
                                  (apply-un-bool oper-un arg)))
      )))

;; apply-oper-bin-bool
(define apply-oper-bin-bool
  (lambda (oper-bin args)
    (cases oper-bin-bool oper-bin
      (and-oper-bin () (if (and (car args) (cadr args)) #t #f))
      (or-oper-bin ()(if (or (car args) (cadr args)) #t #f))
      (xor-oper-bin ()(if (equal? (car args) (cadr args)) #f #t))
      )))

;;apply-oper-un-bool
(define apply-un-bool
  (lambda (oper-un-b arg)
    (cases oper-un-bool oper-un-b
      (not-oper-un ()(if arg #f #t))
      )))

;;apply-expresion-conteo 
(define apply-expresion-conteo
  (lambda (exp-conteo num)
    (cases expresion-conteo exp-conteo
      (to-exp() (+ num 1) )
      (downto-exp()(- num 1))
      )))

;;apply-primitivas-cadena

(define apply-cadena-primitive
  (lambda (prim exps)
    (cases cadena-primitive prim
      (longitud-cadena() 15)
      (concatenar()(string-append exps))
      )))
      

;;apply- primitivas lista
(define apply-lista-primitive
  (lambda (prim lista)
          (cases lista-primitive prim
            (es-vacio() (if (null? lista)#t #f))
            (es-lista() (list? lista))
            (cabeza ()(car lista))
            (cuerpo ()(cdr lista))
            )))
(define apply-lista-primitive-s
  (lambda (prim args)
          (cases lista-primitive-s prim
            (crear-lista()(cons (car args) (cadr args)))
            (append-l()(append (car args)(cadr args)))
            )))
            

;;___________________________________________________________________________

;; Ambiente:
(define-datatype environment environment?
  (empty-env-record)
  
  (extended-env-record (syms (list-of symbol?))
                       (vec vector?)
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

;; Ambiente extendido:

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;; Ambiente vacío:
(define empty-env  
  (lambda ()
    (empty-env-record)))


;----------------------------------------------------------------------------------------------------------------------------
;---------------------------------------------CHEQUEO DE TIPOS---------------------------------------------------------------


;Ambiente de tipos 

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
    (syms (list-of symbol?))
    (vals (list-of type?))
    (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
        (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-tenv env sym)))))))

;definicion de tipos 
(define-datatype type type?
  (atomic-type
   (name symbol?))
  (proc-type
   (arg-types (list-of type?))
   (result-type type?)))

(define int-type
  (atomic-type 'int))
(define bool-type
  (atomic-type 'bool))
(define float-type
  (atomic-type 'float))
(define char-type
  (atomic-type 'char))
(define octal-type
  (atomic-type 'octal))

(define expand-type-expression
  (lambda (texp)
    (cases type-exp texp
      (int-type-exp () int-type)
      (bool-type-exp () bool-type)
      (float-type-exp () float-type)
      (octal-type-exp () octal-type)
      (char-type-exp () char-type)
      (proc-type-exp (arg-texps result-texp)
                     (proc-type
                      (expand-type-expressions arg-texps)
                      (expand-type-expression result-texp))))))

(define expand-type-expressions
  (lambda (texps)
    (map expand-type-expression texps)))


;type-of-program: <programa> -> type
; función que chequea el tipo de un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define type-of-program
  (lambda (prog)
    (cases programa prog
      (un-programa (globales exps)
                   (eval-globales-chequeo globales exps (empty-tenv))))))

(define eval-globales-chequeo
  (lambda (glo exps env)
    (cases globales glo
      (global (idss bodies)
              (if (= (globales-repetidas? idss) 0)
                  (let ((args (eval-rands-unic-var-global bodies env)))
                    (type-of-expression exps (extended-tenv-record idss args env)))
                  (eopl:error 'eval-globales "No puede tener globales repetidas: ~s" idss))
      ))))

;eval-expression: <expression> <enviroment> -> type
; chequea el tipo de la expresión en el ambiente de entrada
(define type-of-expression
  (lambda (exp tenv)
    (cases expresion exp

      (entero-lit (number)
               int-type)

      (flotante-lit (numberf)
                    float-type)

      (octal-lit (ooctal)
                 octal-type)

      (caracter-lit (caract)
                    char-type)

      (id-lit(id)
             (apply-tenv tenv id))

      (false-exp ()
                bool-type)

      (true-exp ()
                 bool-type)

      (var-exp (id exp body )
               (type-of-var-exp id exp body tenv))

      (cons-exp (id exp body )
               (type-of-var-exp id exp body tenv))

      (unic-exp (id exp body )
               (type-of-var-exp id exp body tenv))

      (invocar-proc (rator rands)
               (type-of-application
                (type-of-expression rator tenv)
                (types-of-expressions rands tenv)
                rator rands exp))


      (evalprim-bin-exp (rand1 prim rand2)
                   (type-of-application
                    (type-of-primitiva-binaria prim)
                    (types-of-expressions (list rand1 rand2) tenv)
                    prim (list rand1 rand2) exp))

      (exp-bool-exp (bool-ex)(type-of-exp-bool bool-ex tenv))
      
      
      (if-exp (test-exp true-exp false-exp)
              (let ((test-type (type-of-exp-bool test-exp tenv))
                    (false-type (type-of-expression false-exp tenv))
                    (true-type (type-of-expression true-exp tenv)))
                (check-equal-type! test-type bool-type test-exp)
                (check-equal-type! true-type false-type exp)
                true-type))

      (cond-exp (conds express eelse) ; La expresion cond verifica que el tipo de todas las expresiones(express)sean iguales
                 (let ((express-type (types-of-expressions express tenv))
                    (exp1-type (type-of-expression (car express) tenv)))
                (aux-check-equal express-type exp)
                exp1-type))

      (while-exp (bool-exp exp)
                 (let ((expp-type(type-of-expression exp tenv)))
                   expp-type))
      
      ;(for-exp (id exp1 conteo-exp exp2 exp3)
      ;        (auxiliar-for id (eval-expresiones exp1 env)conteo-exp exp2 exp3 env))
      
      
      (definir-proc (texps ids body)
                (type-of-proc-exp texps ids body tenv))

      (print-exp (exp)(type-of-expression exp tenv))

      (else 0)
      )))

;check-equal-type!: <type> <type> <expression> -> 
; verifica si dos tipos son iguales, muestra un mensaje de error en caso de que no lo sean
(define check-equal-type!
  (lambda (t1 t2 exp)
    (if (not (equal? t1 t2))
        (eopl:error 'check-equal-type!
                    "Types didn’t match: ~s != ~s in~%~s"
                    (type-to-external-form t1)
                    (type-to-external-form t2)
                    exp)
        #t)))

;funcion auxiliar para realizar check en una lista de tipos
(define aux-check-equal
  (lambda (args exp)
    (if (equal? (length args) 1)
        #t
    (if (check-equal-type! (car args)(cadr args) exp )
        (aux-check-equal (cdr args) exp)
        #f
    ))))

;type-to-external-form: <type> -> lista o simbolo
; recibe un tipo y devuelve una representación del tipo facil de leer
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (proc-type (arg-types result-type)
                 (append
                  (arg-types-to-external-form arg-types)
                  '(->)
                  (list (type-to-external-form result-type)))))))

(define arg-types-to-external-form
  (lambda (types)
    (if (null? types)
        '()
        (if (null? (cdr types))
            (list (type-to-external-form (car types)))
            (cons
             (type-to-external-form (car types))
             (cons '*
                   (arg-types-to-external-form (cdr types))))))))

;type-of-proc-exp: (list-of <type-exp>) (list-of <symbol>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión de creación de procedimiento
(define type-of-proc-exp
  (lambda (texps ids body tenv)
    (let ((arg-types (expand-type-expressions texps)))
      (let ((result-type
             (type-of-expression body
                                 (extend-tenv ids arg-types tenv))))
        (proc-type arg-types result-type)))))

(define types-of-expressions
  (lambda (rands tenv)
    (map (lambda (exp) (type-of-expression exp tenv)) rands)))




;type-of-application: <type> (list-of <type>) <symbol> (list-of <symbol>) <expresion> -> <type>
; función auxiliar para determinar el tipo de una expresión de aplicación
(define type-of-application
  (lambda (rator-type rand-types rator rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
                 (if (= (length arg-types) (length rand-types))
                     (begin
                       (for-each
                        check-equal-type!
                        rand-types arg-types rands)
                       result-type)
                     (eopl:error 'type-of-expression
                                 (string-append
                                  "Wrong number of arguments in expression ~s:"
                                  "~%expected ~s~%got ~s")
                                 exp
                                 (map type-to-external-form arg-types)
                                 (map type-to-external-form rand-types))))
      (else
       (eopl:error 'type-of-expression
                   "Rator not a proc type:~%~s~%had rator type ~s"
                   rator (type-to-external-form rator-type))))))

;función auxiliar para determinar el tipo de una variable
(define type-of-var-exp
  (lambda (ids rands body tenv)
    (let ((tenv-for-body
           (extend-tenv
            ids
            (types-of-expressions rands tenv)
            tenv)))
      (type-of-expression body tenv-for-body))))


;type-of-primitive: <primitive> -> <type>
; función auxiliar para determinar el tipo de una primitiva
(define type-of-primitiva-binaria
  (lambda (prim)
    (cases primitiva-binaria prim
      (primitiva-suma ()
                (proc-type (list int-type int-type) int-type))
      (primitiva-resta ()
                (proc-type (list int-type int-type) int-type))
      (primitiva-mult ()
                (proc-type (list int-type int-type) int-type))
      (primitiva-residuo ()
                (proc-type (list int-type int-type) int-type))
      (primitiva-div ()
                (proc-type (list int-type int-type) int-type))
      )))


;función auxiliar para determinar el tipo de una primitiva lógica
(define type-of-pred-prim
  (lambda (pred-pr)
    (cases pred-prim pred-pr
      (menor-que () (proc-type (list int-type int-type) bool-type))
      (mayor-que () (proc-type (list int-type int-type) bool-type))
      (menor-igual ()(proc-type (list int-type int-type) bool-type))
      (mayor-igual ()(proc-type (list int-type int-type) bool-type))
      (igual-igual ()(proc-type (list int-type int-type) bool-type))
      (diferente () (proc-type (list int-type int-type) bool-type))
                 )))
;función auxiliar para determinar el tipo de un operador lógico
(define type-of-oper-bin-bool
  (lambda (oper-bin)
    (cases oper-bin-bool oper-bin
      (and-oper-bin () (proc-type (list bool-type bool-type) bool-type))
      (or-oper-bin ()(proc-type (list bool-type bool-type) bool-type))
      (xor-oper-bin ()(proc-type (list bool-type bool-type) bool-type))
      )))
(define type-of-un-bool
  (lambda (oper-un-b )
    (cases oper-un-bool oper-un-b
      (not-oper-un () (proc-type (list bool-type) bool-type))
      )))

;función auxiliar para determinar el tipo de expresiones booleanas 
(define type-of-exp-bool
  (lambda (exp-b tenv)
    (cases expresion-bool exp-b
      (compare-booleano-exp (exp1 pred-prim exp2)
                            (type-of-application
                             (type-of-pred-prim pred-prim)
                             (types-of-expressions (list exp1 exp2) tenv)
                             pred-prim (list exp1 exp2) exp)) 
                             

      (evalprim-booleano-bin-exp (oper-bin exp-b1 exp-b2)
                                 (type-of-application
                                  (type-of-oper-bin-bool oper-bin)
                                  (types-of-expressions (list exp-b1 exp-b2) tenv)
                                  oper-bin (list exp-b1 exp-b2) exp)) 

      (booleano-lit-true () bool-type)

      (booleano-lit-false ()bool-type)

      (evalprim-booleano-un-exp (oper-un exp-b1)
                                (type-of-application
                                  (type-of-un-bool oper-un)
                                  (types-of-expressions exp-b1 tenv)
                                  oper-un exp-b1 exp))

      
      )))




;Ejemplos
  
(type-of-program(scan&parse "global()
if compare 1 < 2
then 1
else 2
end"))



;(interpretador-tipos)