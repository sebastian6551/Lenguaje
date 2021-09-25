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
     (cadena ("\"" (arbno (or letter whitespace)) "\"")  symbol)
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

   ;; Cambio
   (expresion ("C-VID-VAL") c_vid_val-lit)

   (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion) var-exp)
   (expresion ("cons" (separated-list identificador "=" expresion ",") "in" expresion) cons-exp)
   (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "{" expresion "}")  "in" expresion) rec-exp)
   (expresion ("unic" (separated-list identificador "=" expresion ",") "in" expresion) unic-exp)
   
   (expresion ("[" (separated-list expresion ",")"]") lista-exp)
   (expresion ("vector" "(" (separated-list expresion ",")")") vector-exp)
   (expresion ("reg" "(" identificador "=" expresion "," (separated-list identificador "=" expresion ",") ")") reg-exp)
   
   (expresion ( "funcion" "(" (separated-list identificador ",") ")" "{" expresion "}" ) definir-proc)
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
   (expresion ("vacio") vacio)
   (lista-primitive ("vacio?") es-vacio)
   (lista-primitive ("lista?") es-lista)
   (lista-primitive ("cabeza") cabeza)
   (lista-primitive ("cuerpo") cuerpo)
   (lista-primitive-s ("crear-lista") crear-lista)
   (lista-primitive-s ("append") append)
   (expresion (lista-primitive "(" expresion ")") lista-primapp-exp)
   (expresion (lista-primitive-s "(" (separated-list expresion ",")")") lista-primapp-exp-s)
   
   ;;Para vectores
   (expresion ("ref-vector" "(" entero ")" identificador) vector-ref)
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

;; eval-program:
(define eval-program
  (lambda (prog)
    (cases programa prog
      (un-programa (globals exps)
                 (eval-globales globals (global-env))
                 (eval-expresiones exps (empty-env))))))

(define init-env
  (lambda ()
    (extend-env-for-asign
     '(x y)
     (list 4 3)
     (empty-env-rec))))

;; Evalua las globales
(define eval-globales
  (lambda (glo env)
    (cases globales glo
      (global (idss bodies) idss))
    ;;De aquí se mandaría el cuerpo a eval-expresiones
      ))

;; Global-env: ambiente para las variables globales
(define global-env  
  (lambda ()
    (empty-env-record)))  

;; Evalua las expresiones
(define eval-expresiones
  (lambda (exp env) 
    (cases expresion exp

      ;; Tipos de datos
      (entero-lit (num) num)
      (caracter-lit (caract) caract)
      (flotante-lit (flot) flot)
      (cadena-lit (cadena) cadena)
      (id-lit (id) (apply-env env id))

      (octal-lit (oct)
                 (oct-exp-unparse oct env))
      
      (c_vid_val-lit () "C-VID-VAL")

      (id-ref (id-ref) id-ref)
      
      (var-exp (ids exps body)
               (let ((args (eval-rands exps env)))
                 (eval-expresiones body (extend-env ids args env))))
      
      (cons-exp (ids exps body) ids)
      
      (rec-exp (proc-names idss bodies letrec-body)
               (eval-expresiones letrec-body
                                (extend-env-recursive proc-names idss bodies env)))
      
   ;; (unic-exp (ids exps  body)  ids)      


   ;; Constructores ;;;;;;
      
   ;; (lista-exp (exps) exps env)
   ;; (vector-exp (exps) exps env)
   ;; (reg-exp (id exp ids exps) id exp ids exps)
   ;; (exp-bool-exp (exp) exp)

      ;; Primitivas
      (evalprim-bin-exp (exp1 prim-bin exp2) (eval-prim exp1 prim-bin exp2 env))
      (evalprim-un-exp (prim-un exp) (apply-primitiva-unaria prim-un (eval-rand exp env)))

      ;; Procedimientos
      (definir-proc (ids body)(closure ids body env))
      (invocar-proc (rator rands)
                    (let ((proc (eval-expresiones rator env))
                          (args (eval-rands rands env)))
                      (if (procedimiento? proc)
                          (apply-procedimiento proc args)
                          (eopl:error 'eval-expresiones "Intenta aplicar algo que no es un procedimiento: ~s" proc))))
      
      ;;(definir-proc-rec (proc-names idss bodies letrec-body)
      ;;            (eval-expresiones letrec-body
      ;;                             (extend-env-recursively proc-names idss bodies env)))
      
      ;; Procedimientos recursivos
      
      ;; (expresion ( "invoca" expresion "(" (separated-list expresion ",") ")") invocar-proc)
      ;; (expresion ( "funcion!" "(" (separated-list identificador ",") ")" "{" expresion "}" ) definir-proc-rec)

     ;; Asignación de variables
      (modificar-exp (id exp)
                     (begin (setref! (apply-env-ref env id) (eval-expresiones exp env)) 1))
      
    ;; (expresion ("->" identificador "=" expresion) modificar-exp)
      
    ;;  Estructuras de control
      
    ;;  (seq-exp (exp exps)
    ;;           (let loop ((acc (eval-expresiones exp env)) (exps exps))
    ;;               (if (null? exps)
    ;;                   acc
    ;;                   (loop (eval-expresiones (car exps) env) (cdr exps)))))
      
      (if-exp (bool-exp true-exp false-exp)
             (if (apply-exp-bool bool-exp env)
               (eval-expresiones true-exp env)
               (eval-expresiones false-exp env)))
      
      (cond-exp (conds express eelse)          
                (auxiliar-cond conds express eelse env ))
      
     ;; (while-exp (bool-exp exp)
     ;;            (auxiliar-while bool-exp exp env))
      
     ;; (for-exp (id exp1 conteo-exp exp2 exp3)
     ;;          (auxiliar-for id exp1 conteo-exp exp2 exp3 env))


      (exp-bool-exp (bool-ex)(apply-exp-bool bool-ex env))

      (print-exp (exp)(eval-expresiones exp env))
      
      (else exp)
      )))

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

;; Funcion 
(define extend-env-recursive
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (recursively-extended-env-record proc-names vec old-env)))
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
    (cases environment env
      (empty-env-record () (eopl:error 'apply-env "No se puede enlazar: ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym))))

      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))
    ))

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
    (cases primitiva-binaria prim
      (primitiva-suma () (+ exp1 exp2))
      (primitiva-resta () (- exp1 exp2))
      (primitiva-mult () (* exp1 exp2))
      (primitiva-div () (/ exp1 exp2))
      (primitiva-residuo () (remainder exp1 exp2)))))

;; Evalua operadores en una operación
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expresiones rand env)))

;; Funciones auxiliares para aplicar apply-primitiva-binaria
(define eval-prim
  (lambda (exp1 prim-bin exp2 env)
    (apply-primitiva-binaria (eval-rand exp1 env) prim-bin (eval-rand exp2 env))))

;; Evaluar primitivas unarias
(define apply-primitiva-unaria
  (lambda (prim exp)  
    (cases primitiva-unaria prim
      (primitiva-incrementar () (+ exp 1))
      (primitiva-disminuir () (- exp 1)))
    ))

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

;; Asinar variables
;; Referencias al vector (Store)
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;; aply-env-ref
(define apply-env-ref
  (lambda (env id)
    (cases environmentSet env
      (empty-env-rec ()
                        (eopl:error 'apply-env-ref "No se puede enlazar: ~s" id))
      (extended-env-rec (ids vals env)
                           (let ((pos (rib-find-position id ids)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env id))))
      (else 1)
      )))

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

;Funcion auxiliar cond 
(define auxiliar-cond
  (lambda (conds express eelse env)
    (if (null? conds)
        (eval-expresiones eelse env)
        (if (eval-expresiones (car conds) env)
            (eval-expresiones (car express) env)
            (auxiliar-cond (cdr conds)(cdr express) eelse env)
            )
        )
    )
  )

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
     (lambda (exp-conteo)
       (cases expresion-conteo exp-conteo
         (to-exp()0)
         (downto-exp()0)
         )))

;;___________________________________________________________________________

;; Ambiente inmutable
(define-datatype environment environment?
  (empty-env-record)
  
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

;; Ambiente vacío:
(define empty-env  
  (lambda ()
    (empty-env-record)))

;; Ambiente extendido:
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;;___________________________________________________________________________

;;Ambiente para asignación, variables mutables. Usa vectores

(define-datatype environmentSet environmentSet?
  (empty-env-rec)
  (extended-env-rec
   (syms (list-of symbol?))
   (vec vector?)
   (env environmentSet?)))

(define extend-env-for-asign
  (lambda (syms vals env)
    (extended-env-rec syms (list->vector vals) env))) 

;;Ejemplos

;;Globales
(scan&parse "
global (x = 4, y = 6)
3
")

;;Cadena
(scan&parse "
global ()
\"prueba de cadena\"
")

;;Caracter
(scan&parse "
global ()
'a'
")

;;Entero
(scan&parse "
global ()
4
")

;;Flotante
(scan&parse "
global ()
0.24
")

;;Octal
(scan&parse "
global ()
x8(1 2 4)
")

;;Booleano
(scan&parse "
global ()
true
")

;;Identificador
(scan&parse "
global ()
x
")

;;Identificador paso por referencia
(scan&parse "
global (suma = funcion(x,y){(x+y)})
invoca suma(&x, y)
")

;; Ejemplo var
(scan&parse "
global (a=2, b =3)
var x = 4 in 1")

;; Ejemplo cons
(scan&parse "
global (a=1, b =6)
cons x = 8,  z = 2 in 4")

;; Ejemplo rec
(scan&parse "
global (a=5, b =6)
rec f(y) {12} in 'a'")

 (scan&parse "
global ()
rec
    funcionX (d, e, f) {2}
    funcionY (a) {5}
    proce (g,h) {(h + g)}
in
    invoca proce(3,2)")

;; Ejemplo Unic
(scan&parse "
global (a=5, b =6)
unic x = 0 in 9")  

;; Ejemplo constructor de Datos definido lista
(scan&parse "
global (a=10, b =11)
[2,0,1,6,8]")

;; Ejemplo constructor de Datos definido vector
(scan&parse "
global (a=5, b =6)
var x = vector(1,2,3,4,5) in 9")

;; Ejemplo set-vector
(scan&parse "
global (a=5, b =6)
sequence
var x = vector(1,2,3,4,5) in 9;
set-vector x (2, 4);
end
")

;; Ejemplo paso por referencia vector
(scan&parse "
global (s = 4)
invoca validaVector(ref-vector (0) v, y)
")

;; Ejemplo get posicion vector
(scan&parse "
global (suma = funcion(x,y){(x+y)}, x = vector(1,2,3))
invoca suma(vector-pos x(0), y)
")

;; Ejemplo constructor de Datos definido registro
(scan&parse "
global (a=2, b=5)
reg (x = 20, y = 18)")

;; Ejemplo paso por referencia registro
(scan&parse "
global (s = 4)
invoca validaRegistro(ref-registro v, y)
")

;; Ejemplo get elemento posicion registro
(scan&parse "
global (s = 3, x = reg (a = 1, b = 2, c = 3))
var y = registro-pos x(1) in 8
")

;; Ejemplo set-registro
(scan&parse "
global (a = 2, b = 5, y = reg (x = 5, w = 4) )
set-registro y (0, 2) ")

;;Procedimiento
(scan&parse "
global (z = 5)
cons x = funcion(n){->z=n} in
invoca x(6)")

;;Procedimiento recursivo
(scan&parse "
global ()
var longitudLista = funcion!(L){if compare L == vacio
                    then 0
                    else (1+ invoca longitudLista(cuerpo(L))) end}
in
invoca longitudLista([1,2,3])
")

;;Invocar procedimientos
(scan&parse "
global ()
invoca longitudLista([1,2,3])
")

;;Modificar/actualizar variables 
(scan&parse "
global ()
var x = 4 in
sequence
-> x = 3;
end
")

;;Primitiva unaria
(scan&parse "
global ()
¬¬x
")

;;Primitiva binaria
(scan&parse "
global ()
(1+2)
")

;;Primitiva unaria para octales
(scan&parse "
global ()
octal ++x8(1 1)
")

;;Primitiva binaria para octales
(scan&parse "
global ()
octalbin (x8(1 1) + x8(1 2))
")

;Para cadenas
(scan&parse "
global (x = 5, y = 6)
longitud-cadena ( [f,u,n,d,a,m,e,n,t,o,s] )
")

(scan&parse "
global (x = 5, y = 6)
concatenar (\"hola\",\"mundo\")
")

;Para listas
(scan&parse "
global (x = 5, y = 6)
crear-lista(x,y)
")

(scan&parse "
global (lista = [x,y,z])
cabeza ( lista ) 
")

;Para vectores
(scan&parse "
global (v1 = vector (x,y,z))
vector? ( v1 )
")

;Para registros
(scan&parse "
global (x=1, y=2)
crear-registro ( m = x, n = y)
")

;Expresion-bool
(scan&parse "
global (x=1, y=2)
and (compare  x > 0) (compare y > 0)
")

;Estructura sequence
(scan&parse "
global (x=1, y=2)
sequence
if compare x > y then 1 else 0 end;
(x * y);
end
")

;Estructura condicional if
(scan&parse "
global (x = 5, y = 6)
if compare x < 3  then ++ x else 0
end
")

;Estructura condicional cond
(scan&parse"
global (x = 5, y = 6)
cond
[(compare x > 0)(\"positivo\")]
[(compare x < 0)(\"negativo\")]
[(compare x == 0)(\"cero\")]
else \"indefinido\"
")

;Esrtructura iteración while
(scan&parse"
global (x = 5, y = 6)
while (compare y<10) do 
sequence
++y;
print(y);
end
done
")

;Esrtructura iteración for
(scan&parse "
global (x = 5, y = 6)
for i=0 to 5
do 
print (i)
done
")

;;Ejemplo Print
(scan&parse "
global(x = 5, y = 3)
var z = 4 in
sequence
->x =z;
->z =9;
print(x);
print(z);
end
")

;;Ejemplo filtro
(scan&parse "
global( listaA = [1,[4,5],a,'o',[k,l]])
 var
 filtro = funcion!(lista, pred)
 {
                        if compare lista == vacio
                         then vacio
                        else
                         cond
                          [(invoca pred(cabeza (lista)) ) (crear-lista ( cabeza (lista) , invoca filtro( pred , cuerpo (lista) )  ) )]                    
                            else invoca filtro ( pred, cuerpo (lista) )
                        end }
 in 9
")

;;Ejemplo Función factorial
(scan&parse "
global ()
var factorial = funcion!(n){if compare n < 2
                then 1
                else (n * invoca factorial(¬¬ n)) end}
in
invoca factorial(5)
")
