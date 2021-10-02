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
;;                 ::=  <reg-primitive> (separated-list <expresion> ",")
;;                               <reg-primapp-exp (exps)>
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
;;  <reg-primitive>         ::= reg? | create-reg | ref-reg | set-reg

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
    (lista-primitive-s ("append") append-l)
    (expresion (lista-primitive "(" expresion ")") lista-primapp-exp)
    (expresion (lista-primitive-s "(" (separated-list expresion ",")")") lista-primapp-exp-s)
   
    ;;Para vectores
    (expresion ("ref-vector" "(" entero ")" identificador) vector-reff)
    (expresion ("vector?" "(" expresion ")") es-vector-exp)
    (expresion ("crear-vector" "(" (separated-list expresion ",")")") crear-vector-exp)

 ;;Operacion sobre primitiva
     (expresion (reg-primitive "(" (separated-list expresion "," identificador  "," )")")
              reg-primapp-exp)
    
    ;; Cambio
    (expresion ("vector-pos" identificador "(" entero ")" ) vector-pos-exp)
   
    ;; Modifica el elemento en la posicion n de un vector
    (expresion ("set-vector" identificador "(" entero "," expresion ")") set-vector-exp)
   
    ;;Para registros
     ;;Para registros
    (reg-primitive ("registro?")es-registro-exp)
    (reg-primitive ("ref-registro") registro-ref)
    (reg-primitive ("crear-registro")crear-registro-exp)
    (reg-primitive ("set-reg") set-registro)

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
      
      (rec-exp (proc-names idss bodies rec-body)
               (eval-expresiones rec-body
                                 (extend-env-recursive proc-names idss bodies env)))
      
      (unic-exp (ids exps body)
                (let ((args (eval-rands-unic-var-global exps env)))
                 (eval-expresiones body (extend-env ids args env))))      

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
      (reg-exp (id exp ids exps) (reg-unparse id exp ids exps env))
      
      ;; Primitivas
      (evalprim-bin-exp (exp1 prim-bin exp2) (eval-prim exp1 prim-bin exp2 env))
      (evalprim-un-exp (prim-un exp) (apply-primitiva-unaria prim-un (eval-rand exp env)))

       (reg-primapp-exp(prim exps ids)
                       (let ((args (eval-rands exps env)))
                                (apply-reg-primitive prim  args ids env)))

      ;; Procedimientos
      (definir-proc (ids body)(closure ids body env))
      
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
      
      (else exp)
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

;; Funcion AUX sobre primitivas
;; reg-unparse:
(define reg-unparse
  (lambda (id exp ids exps env)
    (cons (list id (eval-rand exp env)) (map (lambda (id exp) (list id (eval-rand exp env))) ids exps))))


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

;;setter-reg
(define setter-reg
 (lambda (L v id)
  (if (eqv? id (caar L))
       (cons (list id v) (cdr L))
    (cons (car L) (setter-reg id v (cdr L)))
   )))
;; set-reg(reg (x=8, y=9)x, 5, x)



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

;; Ejemplos

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
invoca suma(vector-pos x(0), 1)
")

;; Ejemplo constructor de Datos definido registro
(scan&parse "
global (a=2, b=5)
reg (x = 20, y = 18)")

;; Ejemplo paso por referencia registro
;;(scan&parse "
;;global (s = 4)
;;invoca validaRegistro(ref-registro v, y)
;;")

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
global (x=1, y=2, z=3, v1 = vector (x,y,z))
vector? ( v1 )
")

;Para registros
;;(scan&parse "
;;global (x=1, y=2)
;;crear-registro ( m = x, n = y)
;;")

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
sequence 
if compare x > 3  then ->x =++x else 0
end;
print(x);
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

;;Ejemplo Factorial con rec
(scan&parse "
global ()
rec factorial (n){if compare n < 2
                then 1
                else (n * invoca factorial(¬¬ n)) end}
in
invoca factorial(5)
")

(interpretador)
