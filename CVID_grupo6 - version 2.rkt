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


;**************************************************************************************
;                               Lexica
;**************************************************************************************
  
  (define scanner-spec-interpretador-proyecto
  '(
  '(
     (epacio (whitespace) skip)
     (comentario ("//" (arbno (not #\newline)) ) skip)
     (comentario ("%" (arbno (not #\newline)) ) skip)
     (identificador  (letter (arbno (or letter digit))) symbol)
     (entero (digit (arbno digit)) number)
     (entero ("-" digit (arbno digit)) number)
     (flotante (digit (arbno digit) "." digit (arbno digit)) number)
     (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)
     (caracter ("'" letter "'") symbol)
    )
  )

;*******************************************************************************************
;                                     Gramatica
;*******************************************************************************************

(define gramatica-interpretador-proyecto
'(
   (programa (globales expresion) un-programa)

   (globales ("global" "(" (separated-list identificador "=" expresion ",") ")") global)
   
   (expresion ("\""  (arbno identificador) "\"")  cadena-lit)
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
   (expresion ("ref-vector" identificador) vector-ref)
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
   (expresion-bool ("true") booleano-lit)
   (expresion-bool ("false") booleano-lit)
   (expresion-bool (oper-un-bool expresion-bool) evalprim-booleano-un-exp)
                    
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

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "> "
    (lambda (prog) (eval-program  prog)) 
    (sllgen:make-stream-parser 
      lexica
      gramatica)))

;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (prog)
    (cases programa prog
      (un-programa (globals exps)
                 (eval-globales globals (empty-env))
                 (eval-expresiones exps (empty-env))))))

;; Evalua las globales
(define eval-globales
  (lambda (glo env)
    (cases globales glo
      (global (idss bodies) idss))
    ;;De aquí se mandaría el cuerpo a eval-expresiones
      ))

;; Evalua las expresiones
(define eval-expresiones
  (lambda (exp env) 
    (cases expresion exp
      (entero-lit (num) num)
      ;;Aquí van los cases de las expresiones     
      (else exp)
      )))

;Ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

;empty-env:
(define empty-env  
  (lambda ()
    (empty-env-record)))  

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
invoca validaVector(ref-vector v, y)
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
;******************************************************************************************
;                               Construidos automáticamente
;******************************************************************************************

(sllgen:make-define-datatypes scanner-spec-interpretador-proyecto gramatica-interpretador-proyecto)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-interpretador-proyecto gramatica-interpretador-proyecto)))

;*******************************************************************************************
;                                      Parser & Scanner
;********************************************************************************************
;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
;*********************************************************************************************
(define scan&parse
  (sllgen:make-string-parser scanner-spec-interpretador-proyecto gramatica-interpretador-proyecto))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-interpretador-proyecto gramatica-interpretador-proyecto))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-interpretador-proyecto
      gramatica-interpretador-proyecto)))


;*******************************************************************************************
;                                   El Interprete
;******************************************************************************************
;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp)
                 (eval-expression exp body (init-env))))))
;**********************************************************************************************
;                                              Ambiente inicial
;**********************************************************************************************
(define init-env
    (lambda ()
 (empty-env)))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
      (cases expression exp

   ; tipos de datos      
        (global (idglob glob)glob)
       (entero-lit (num) num)
        (symbol (letra) letra)
        (caracter-lit (caracter) caracter)
        (cadena-lit (arbno cadena) cadena)
        (flotante-lit (float) float)
        (octal-lit (arbno oct) oct)
        (id-lit (id) (ap-env env id))
       (id-ref (id-ref) id-ref)
       (var-exp (ids exps body) exps)
       (cons-exp (ids exps body) ids)
       (rec-exp (proc-names idss bodies letrec-body))
       (unic-exp (ids exps  body)  ids)      

      ;;;;;; Constructores ;;;;;;
      (lista-exp (exps)exps)
      (vector-exp (exps) exps)
      (reg-exp (id exp ids exps) id exp ids exps)
      (exp-bool-exp (exp)exp)

     ;;;;;; Estructuras de control ;;;;;;

      (seq-exp (exp exps)
                 (let loop ((acc (eval-expression exp env))
                            (exps exps))
                   (if (null? exps) acc
                       (loop (eval-expression (car exps) env) (cdr exps)))))
      (if-exp (exp-bool true-exp false-exp)
              (if (eqv? (apply-exp-bool exp-bool env) 'true)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (while-exp (exp-bool exp) (while exp-bool exp env))
      (for-exp (id var exp-for exp1 exp2) (for id var exp-for exp1 exp2 env 'null))

        ))
      
    
