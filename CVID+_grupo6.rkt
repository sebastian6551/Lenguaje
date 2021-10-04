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

    (globales ("global" "(" (separated-list type-exp identificador "=" expresion ",") ")") global)
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

    (expresion ("var" (separated-list type-exp identificador "=" expresion ",") "in" expresion) var-exp)
    
    (expresion ("cons" (separated-list type-exp identificador "=" expresion ",") "in" expresion) cons-exp)
    
    (expresion ("id-cons" identificador) cons-id)
    
    (expresion ("rec" (arbno type-exp identificador "(" (separated-list type-exp identificador ",") ")" "{" expresion "}")  "in" expresion) rec-exp)
    (expresion ("unic" (separated-list type-exp identificador "=" expresion-unic",") "in" expresion) unic-exp)
   
    (expresion ("[" (separated-list expresion ",")"]") lista-exp)
    (expresion ("vector" "(" (separated-list expresion ",")")") vector-exp)
    (expresion ("reg" "("type-exp identificador "=" expresion "," (separated-list type-exp identificador "=" expresion ",") ")") reg-exp)
   
     (expresion ( "funcion" "(" (separated-list type-exp identificador ",") ")" "{" expresion "}" ) definir-proc)
    (expresion ( "invoca" expresion "(" (separated-list expresion ",") ")") invocar-proc)
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
    (expresion (cadena-primitive "(" (separated-list cadena ",")")") cadena-primapp-exp)
   
    ;;Para listas
    (expresion ("vacio") vacio)
    (lista-primitive ("vacio?") es-vacio)
    (lista-primitive ("lista?") es-lista)
    (lista-primitive ("cabeza") cabeza)
    (lista-primitive ("cuerpo") cuerpo)
    (lista-primitive-s ("crear-lista") crear-lista)
    (lista-primitive-s ("append") append-l)
    (expresion (lista-primitive "(" expresion ")") lista-primapp-exp)
    (expresion (lista-primitive-s "(" (separated-list expresion ",")")") lista-primapp-exp-s)
   
     ;; Para vectores
    (expresion ("vector?" "(" expresion ")") es-vector-exp)
    (expresion ("crear-vector" "(" (separated-list expresion ",")")") crear-vector-exp)     
    (expresion ("ref-vector" "(" expresion "," entero ")" ) vector-ref-exp)
   
    ;; Modifica el elemento en la posicion n de un vector
    (expresion ("set-vector" expresion "(" entero "," expresion ")") set-vector-exp)
   
    ;; Para registros
    (reg-primitive ("registro?")es-registro-exp)
    (reg-primitive ("ref-registro") registro-ref)
    (reg-primitive ("crear-registro")crear-registro-exp)
    (reg-primitive ("set-reg") set-registro)

    (expresion (reg-primitive "(" (separated-list expresion "," identificador  "," )")")
              reg-primapp-exp)
    
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
    (expresion ("for" type-exp identificador "=" expresion expresion-conteo expresion "do" expresion  "done") for-exp )
    (expresion-conteo ("to") to-exp)
    (expresion-conteo ("downto") downto-exp)
    (expresion ("print" "(" expresion ")" ) print-exp)
    (expresion ("ffalse") false-exp)
    (expresion ("ttrue") true-exp)

    (type-exp ("int") int-type-exp)
    (type-exp ("bool") bool-type-exp)
    (type-exp ("float") float-type-exp)
    (type-exp ("char") char-type-exp)
    (type-exp ("octal") octal-type-exp)
    (type-exp ("lista") lista-type-exp)
    (type-exp ("vec") vector-type-exp)
    (type-exp ("registro") registro-type-exp)
    (type-exp ("Fun") funcion-type-exp)
    (type-exp ("(" (separated-list type-exp "*") "->" type-exp ")")
              proc-type-exp)
   
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
      (global (ids-types idss bodies)
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
      
      (var-exp (ids-types ids exps body)
               (let ((args (eval-rands-unic-var-global exps env)))
                 (eval-expresiones body (extend-env ids args env))))
      
      (cons-exp (ids-types ids exps body)
                (let ((args (eval-rands exps env)))
                  (eval-expresiones body (extend-env ids args env))))

      (cons-id (ids)0)
      
      (rec-exp (ids-types proc-names idss-types idss bodies rec-body)
               (eval-expresiones rec-body
                                 (extend-env-recursive proc-names idss bodies env)))
      
      (unic-exp (ids-types ids exps body)
                (let ((args (eval-rands-unic-var-global exps env)))
                 (eval-expresiones body (extend-env ids args env))))


      ;; Cadenas
      (cadena-primapp-exp (prim exps)
                          (if (null? exps)
                              (eopl:error 'primitiva-cadena "Se espera algún argumento")
                              (apply-prim-cadena prim exps)))
      
      ;; Listas
      (lista-exp (exps) (car (list (eval-rands exps env))))

      (vacio () empty)

      (lista-primapp-exp (prim exp)
                         (apply-lista-primitive prim (eval-expresiones exp env)))
      
      (lista-primapp-exp-s (prim exps)
                           (let ((args (eval-rands exps env)))
                             (apply-lista-primitive-s prim args)))

      ;; Vectores
      (vector-exp (exps)
               (let ((args (eval-rands exps env)))
                 (list->vector args)))

      (es-vector-exp (exp)
               (if (vector? (eval-expresiones exp env))#t #f))

      (crear-vector-exp (exps)
               (let ((args (eval-rands exps env)))
                 (list->vector args)))

      (vector-ref-exp (id pos)
                      (let ((vec (eval-expresiones id env)))
                        (if (vector? vec)
                            (if (and (> (vector-length vec) pos) (<= 0 pos))
                                (vector-ref vec pos)
                                (eopl:error 'vector-ref "La posición debe ser menor a ~s y no puede ser negativa" (vector-length vec)))
                            (eopl:error 'vector-ref "Se espera un vector como argumento no ~s" vec))))
                   
      (set-vector-exp (id pos exp)
                      (let ((vec (eval-expresiones id env)))
                        (if (vector? vec)
                            (if (and (> (vector-length vec) pos) (<= 0 pos))
                                (vector-set! vec pos (eval-expresiones exp env))
                                (eopl:error 'vector-ref "La posición debe ser menor a ~s y no puede ser negativa" (vector-length vec)))
                            (eopl:error 'vector-ref "Se espera un vector como argumento no ~s" vec))))
      
      ;; Registros
      (reg-exp (id-type id exp ids-type ids exps) (reg-unparse id exp ids exps env))

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
      
       (for-exp (id-type id exp1 conteo-exp exp2 exp3)
               (auxiliar-for id (eval-expresiones exp1 env)conteo-exp exp2 exp3 env))


      (exp-bool-exp (bool-ex)(apply-exp-bool bool-ex env))

      (print-exp (exp)(eval-expresiones exp env))
      
      (else "llegóaqui1")
      )))

;; Para cadenas
(define apply-prim-cadena
  (lambda (prim exps)
    (cases cadena-primitive prim
      (longitud-cadena  ()
                        (if (= 1 (contar-lista exps))
                            (-(string-length (car exps)) 2)
                            (eopl:error 'longitud-cadena "Se requiere 1 cadena no ~s cadenas" (contar-lista exps))))
      (concatenar () (lista-to-string exps))
        )))

;; aux apply-prim-cadena
(define lista-to-string
  (lambda (lst)
    (if (null? lst)
        ""
        (string-append (substring (car lst) 1 (- (string-length (car lst)) 1)) (lista-to-string (cdr lst))))))

;; Determina si es necesario evaluar nuevamente el id o no
(define re-apply-env
  (lambda (env id)
    (let ((id-evaluado (apply-env env id)))
      (if (list? id-evaluado)
          
          (if (null? id-evaluado)
              id-evaluado
              (cond

                ;;Para unic
                [(eqv? (car id-evaluado) 'unic)
                 (if (eqv? (car (cdr id-evaluado)) "C-VID-VAL")
                     "C-VID-VAL"
                     (car (cdr id-evaluado)))]

                ;;Para var
                [(eqv? (car id-evaluado) 'var)
                 (car (cdr id-evaluado))]

                ;;Si es una lista
                [else id-evaluado]
                ))
          id-evaluado))))

;;___________________________________________________________________________

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

;; oct-unparse: convierte un número en base N a base 10
(define oct-unparse
  (lambda (L N C)
    (if (null? L) 0
    (+ (* (car L) (expt N C)) (oct-unparse (cdr L) N (+ 1 C))))))

;; Funcion AUX sobre primitivas
;; reg-unparse:
(define reg-unparse
  (lambda (id exp ids exps env)
    (cons (list id (eval-rand exp env)) (map (lambda (id exp) (list id (eval-rand exp env))) ids exps))))
;---------------------------------------------------------------------
;; apply-reg-primitive:
(define apply-reg-primitive
  (lambda (prim args ids env)
    (cases reg-primitive prim
      (es-registro-exp()(is-reg?(car args)))
      (crear-registro-exp () (create-registro args ids))
      (registro-ref() (reference-reg (car args) (car ids)))
      (set-registro() (setter-reg (car args) (cadr args) (car ids)))
      )))

;; is-reg?
(define is-reg?
  (lambda (reg)
    (if (null? reg)
        'true
        (if(symbol? (caar reg))
           (is-reg? (cdr reg))
           'false
       ))))

;; create registro
(define create-registro
  (lambda (args ids)
    (if (or (null? args) (null? ids))
        '()
        (cons (list (car ids)(car args)) (create-registro (cdr args) (cdr ids))))))

;; reference-reg

(define reference-reg
  (lambda (args id)     
       (if (null? args)
          (eopl:error "Error: No se encontró" id)
           (if (eqv? id (caar args))
              (car (cdar args))
              (reference-reg (cdr args) id)))
    ))

;; setter-reg
(define setter-reg
 (lambda (L v id)
  (if (eqv? id (caar L))
       (cons (list id v) (cdr L))
    (cons (car L) (setter-reg id v (cdr L)))
   )))


;------------------------------------------------------------------


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
      (igual-igual ()(if (equal? (car args) (cadr args) ) #t #f))
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
            )
        )
    )
  )
;;Funcion auxiliar while
(define auxiliar-while
  (lambda (bool-exp exp env)
    (if (apply-exp-bool bool-exp env)
        (cons(eval-expresiones exp env)
             (auxiliar-while bool-exp exp env))
        empty
        )
    )
  )
      
;;Función auxiliar for
(define auxiliar-for
  (lambda (id num_i conteo-exp limite exp3 env)
      (if (equal? num_i (eval-expresiones limite env))
           (cons (eval-expresiones exp3 (extend-env (list id) (list num_i) env)) empty)
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

      

;;apply- primitivas lista
(define apply-lista-primitive
  (lambda (prim lista)
        (cases lista-primitive prim
            (es-vacio() (if (null? lista)#t #f))
            (es-lista() (list? lista))
            (cabeza ()
                    (if (list? lista)
                        (if (null? lista)
                            (eopl:error 'cabeza "Se espera una lista con elementos")
                            (car lista))
                        (eopl:error 'primitiva-lista "El argumento debe ser una lista no ~s" lista)))
          
            (cuerpo ()
                    (if (list? lista)
                        (if (null? lista)
                            (eopl:error 'cabeza "Se espera una lista con elementos")
                            (cdr lista))
                        (eopl:error 'primitiva-lista "El argumento debe ser una lista no ~s" lista))))))
  
(define apply-lista-primitive-s
  (lambda (prim args)
          (cases lista-primitive-s prim
            (crear-lista()
                        (if (= (contar-lista args) 2)
                            (if (list? (cadr args))
                                (cons (car args) (cadr args))
                                (eopl:error 'crear-lista
                                    "El segundo argumento debe ser una lista no ~s " (cadr args)))
                        (eopl:error 'crear-lista
                                    "El número de argumentos es incorrecto, se esperan 2 argumentos, no ~s"
                                    (contar-lista args))))
            (append-l ()
                      (if (lista-de-listas? args)
                      (list-append args)
                      (eopl:error 'append
                                    "Todos los argumentos deben ser listas")
                      )))))
;; aux de append
(define (list-append lst1)
  (cond
    [(null? lst1) lst1]
    [else (append (car lst1) (list-append (cdr lst1)))]
    ))

;; aux de append
(define lista-de-listas?
  (lambda (lst)
    (if (null? lst)
        #t
        (if (list?(car lst))
        (and #t (lista-de-listas? (cdr lst)))
        #f))))
            

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
(define lista-type
  (atomic-type 'lista))
(define vector-type
  (atomic-type 'vector))
(define registro-type
  (atomic-type 'registro))

(define funcion-type
  ;(proc-type list atomic-type))
  proc-type)

(define expand-type-expression
  (lambda (texp)
    (cases type-exp texp
      (int-type-exp () int-type)
      (bool-type-exp () bool-type)
      (float-type-exp () float-type)
      (octal-type-exp () octal-type)
      (char-type-exp () char-type)
      (lista-type-exp () lista-type)
      (vector-type-exp () vector-type)
      (registro-type-exp () registro-type)

      (funcion-type-exp () funcion-type)
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
  (lambda (glo exps tenv)
    (cases globales glo
      (global (ids-types idss bodies)
              (let ((idds-typess (expand-type-expressions ids-types))
                    (bodies-types (types-of-expressions bodies tenv)))
                (aux-check-listas bodies-types idds-typess exp)
                (let ((tenv-for-body
                       (extend-tenv idss idds-typess tenv)))
                  (type-of-expression exps tenv-for-body)))))))


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

      (var-exp (ids-types id exp body )
               (let ((var-type (type-of-var-exp id exp body tenv))
                     (exp-types (types-of-expressions  exp tenv))
                    (idss-type (expand-type-expressions ids-types)))                 
                 (aux-check-listas exp-types idss-type exp)
                 var-type))
               
            
      (cons-exp (ids-types id exp body )
               (let ((var-type (type-of-var-exp id exp body tenv))
                     (exp-types (types-of-expressions  exp tenv))
                    (idss-type (expand-type-expressions ids-types)))                 
                 (aux-check-listas exp-types idss-type exp)
                 var-type))

      (unic-exp (ids-types id exp body )
              (let ((var-type (type-of-var-exp id exp body tenv))
                     (exp-types (types-of-expressions  exp tenv))
                    (idss-type (expand-type-expressions ids-types)))                 
                 (aux-check-listas exp-types idss-type exp)
                 var-type))

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

      (evalprim-un-exp (prim-un exp)
                        (type-of-application
                    (type-of-primitiva-unaria prim-un)
                    (types-of-expressions (list exp) tenv)
                    prim-un (list exp) exp))

      (modificar-exp (id exp1)
                      (let ((id-type(apply-tenv tenv id))
                        (exp1-type (type-of-expression exp1 tenv)))
                        (check-equal-type! id-type exp1-type exp)
                          exp1-type))

      (lista-exp (exps)
               lista-type)

      (lista-primapp-exp (prim exp1)
                 (type-of-application
                 (type-of-lista-primitive prim)
                 (types-of-expressions (list exp1) tenv)
                   prim (list exp1) exp))

      (lista-primapp-exp-s (prim exps)
                 (type-of-application
                 (type-of-lista-primitive-s prim)
                 (types-of-expressions exps tenv)
                   prim exps exp))

      (vector-exp (exps)
                 vector-type)

      (es-vector-exp (exp)
                     ;(let (()))
                     (proc-type (list vector-type)bool-type))

      (set-vector-exp (id pos exp)
                       (let ((id-type(apply-tenv tenv id)))                   
                         (check-equal-type! id-type vector-type exp)
                         id-type))

      (vector-ref-exp (id ent)
                      (let ((id-type(apply-tenv tenv id)))                   
                         (check-equal-type! id-type vector-type exp)
                         id-type))

      (reg-exp (id-type id exp  ids-type ids exps)
              (let (
                     (exp-types (types-of-expressions (append (list exp) exps) tenv))
                    (idss-type (expand-type-expressions (append (list id-type) ids-type))))                 
                 (aux-check-listas exp-types idss-type exp)
                 registro-type))
      
      (reg-primapp-exp(prim exps ids)
                      registro-type)

      (exp-bool-exp (bool-ex)(type-of-exp-bool bool-ex tenv))


      (seq-exp (exp exps)
               (let ((ultima-exp (retorna-ultimo-lista (list exp exps))))
                 (let ((ultima-type (type-of-expression (car ultima-exp) tenv)))
                   ultima-type)))
                 
      
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
      
      (for-exp (id-type id exp1 conteo-exp exp2 exp3)
               (let ((tenv-for-body
                      (extend-tenv
                       (list id)
                       (list (type-of-expression exp1 tenv))
                       tenv))
                    (idd-type (expand-type-expressions (list id-type)))
                    (exp1-type (type-of-expression exp1 tenv))
                    (exp2-type (type-of-expression exp2 tenv)))
                 (let (
                       (exp3-type (type-of-expression exp3 tenv-for-body)))
                   (check-equal-type! (car idd-type) exp1-type exp)
                   (check-equal-type! exp1-type exp2-type exp)
                   exp3-type)))
      
      (definir-proc (texps ids body)
                (type-of-proc-exp texps ids body tenv))

      (rec-exp  (result-texps proc-names texpss idss bodies letrec-body)
                  (type-of-letrec-exp result-texps proc-names texpss idss bodies
                                      letrec-body tenv))

      (print-exp (exp)(type-of-expression exp tenv))

      (else int-type)
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
;funcion auxiliar para realizar check-equal-type!(uno a uno) entre los elementos de dos listas 
(define aux-check-listas
  (lambda (list1 list2 exp)
    (if (equal? (length list1) 1)
        (check-equal-type!(car list1)(car list2)exp)
   (if (check-equal-type!(car list1)(car list2)exp)
       (aux-check-listas (cdr list1)(cdr list2) exp)
       #f ;nunca llega, porque check-equal-type! arrojaría el error primero

    )

  )))

;funcion auxiliar que dada una lista, retorna su ultimo elemento
(define retorna-ultimo-lista
  (lambda (l)
    (if (equal? 1 (length l))
        (car l)
        (retorna-ultimo-lista (cdr l))
        )
  ))

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

(define types-of-bools-exp
  (lambda (rands tenv)
    (map (lambda (exp) (type-of-exp-bool exp tenv)) rands)))


; función auxiliar para determinar el tipo de una expresión letrec
(define type-of-letrec-exp
  (lambda (result-texps proc-names texpss idss bodies letrec-body tenv)
    (let ((arg-typess (map (lambda (texps)
                             (expand-type-expressions texps))
                           texpss))
          (result-types (expand-type-expressions result-texps)))
      (let ((the-proc-types
             (map proc-type arg-typess result-types)))
        (let ((tenv-for-body
               (extend-tenv proc-names the-proc-types tenv)))
          (for-each
           (lambda (ids arg-types body result-type)
             (check-equal-type!
              (type-of-expression
               body
               (extend-tenv ids arg-types tenv-for-body))
              result-type
              body))
           idss arg-typess bodies result-types)
          (type-of-expression letrec-body tenv-for-body))))))




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
; función auxiliar para determinar el tipo de una primitiva binaria
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

; función auxiliar para determinar el tipo de una primitiva unaria
(define type-of-primitiva-unaria
  (lambda (prim)
    (cases primitiva-unaria prim
      (primitiva-incrementar ()
                (proc-type (list int-type) int-type))
      (primitiva-disminuir ()
                (proc-type (list int-type) int-type))
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
                                  (types-of-bools-exp (list exp-b1 exp-b2) tenv)
                                  oper-bin (list exp-b1 exp-b2) exp)) 

      (booleano-lit-true () bool-type)

      (booleano-lit-false ()bool-type)

      (evalprim-booleano-un-exp (oper-un exp-b1)
                                (type-of-application
                                  (type-of-un-bool oper-un)
                                  (list (type-of-exp-bool exp-b1 tenv))
                                  oper-un (list exp-b1) exp))

      
      )))

;Funciones para el tipo de primitivas de listas
(define type-of-lista-primitive-s
(lambda (prim)
    (cases lista-primitive-s prim
      (crear-lista ()
                (proc-type (list lista-type lista-type) lista-type))
      (append-l () (proc-type (list lista-type lista-type) lista-type))
      )))

(define type-of-lista-primitive
  (lambda (prim)
          (cases lista-primitive prim
            (es-vacio() (proc-type (list lista-type) bool-type))
            (es-lista() (proc-type (list lista-type) bool-type))
            (cabeza () (proc-type (list lista-type) bool-type));pendiente
            (cuerpo () (proc-type (list lista-type) lista-type))
            )))




(interpretador-tipos)