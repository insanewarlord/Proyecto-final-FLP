#lang eopl

;******************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expresion>
;;                      <a-program (exp)>
;;  <expresion>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expresion>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expresion>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {<identificador> = <expresion>}* in <expresion>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expresion>
;;                      <proc-exp (ids body)>
;;                  ::= (<expresion> {<expresion>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identificador ({identificador}(,)) = <expresion>} in <expresion>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expresion> {; <expresion>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identificador> = <expresion>
;;                     <set-exp (id rhsexp)>
;;  <primitive>     ::= + | - | * | add1 | sub1 

;******************************

;******************************
; Especificación Léxica

(define scanner-spec-simple-interpreter
  '((white-sp
     (whitespace) skip)
    (comment
     ("%" (arbno (not #\newline))) skip)
    (identificador
     (letter (arbno (or letter digit "?"))) symbol)
    (digitoDecimal
     (digit (arbno digit)) number)
    (digitoDecimal
     ("-" digit (arbno digit)) number)
    (digitoBinario
     ("b" (or "0" "1") (arbno (or "0" "1"))) string)
    (digitoBinario
     ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
    (digitoOctal
     ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
    (digitoOctal
     ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
    (digitoHexadecimal
     ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
    (digitoHexadecimal
     ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
    (flotante
     (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante
     ("-" digit (arbno digit) "." digit (arbno digit)) number)))

; Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program ( (arbno struct-decl) expresion) a-program)
    
    ;; Expresiones numéricas
    (expresion (numero-exp) num-exp)  
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) flotante-num)
    
    ;; Expresiones aritméticas
    (expresion ("(" expresion primitive expresion ")") prim-num-exp)
    
    ;; Primitivas numéricas
    (primitive ("+") sum-prim)
    (primitive ("-") minus-prim)
    (primitive ("*") mult-prim)
    (primitive ("mod") mod-prim)
    (primitive ("pow") elevar-prim)
    (primitive ("<") menor-prim)
    (primitive (">") mayor-prim)
    (primitive ("<=") menorigual-prim)
    (primitive (">=") mayorigual-prim)
    (primitive ("!=") diferente-prim)
    (primitive ("==") igual-prim)

    ;; Identificadores y cadenas
    (expresion (identificador) var-exp)
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)

    ;; Listas
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)
    
    ;; Primitivas listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ;; Condicionales
    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)

    ;; Funciones y llamadas
    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    (expresion ("call" expresion "(" (separated-list expresion ",") ")") app-exp)
    
    ;; Variables
    (var-decl ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (var-decl ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)
    (expresion (var-decl) decl-exp)

    ;; Bloques de expresiones
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;; Primitivas de cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)

    ;; Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)
    
    ;; Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;; Arrays y primitivas de arrays
    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)

    ;; Reconocimiento de patrones
    (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)
    
    ;; Expresiones regulares
    (regular-exp (identificador "::" identificador) list-match-exp)
    (regular-exp ("numero" "(" identificador ")") num-match-exp)
    (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)

    ;; Primitivas booleanas
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)

    ;; Estructuras (no implementadas)
    (struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)
    
    ;; Instanciación y uso de estructuras
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    (expresion ("get" expresion "." identificador) get-struct-exp)
    (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)))


(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (estructura body)
                 (actulizar-ambiente-struct! estructura)
                 (eval-expresion body (empty-env))))))


; Función para evaluar expresiones en un entorno dado
(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      ; Evaluación de variables
      (var-exp (id)
        (apply-env env id))  ; Obtiene el valor de la variable del entorno

      ; Evaluación de expresiones booleanas
      (true-exp ()
        #t)  ; Retorna verdadero
      
      (false-exp ()
        #f)  ; Retorna falso

      ; Evaluación de expresiones condicionales
      (if-exp (test-exp true-exp false-exp)
        (if (eval-expresion test-exp env)
            (eval-expresion true-exp env)
            (eval-expresion false-exp env)))  ; Evaluación de expresión if

      ; Evaluación de expresiones de funciones
      (func-exp (ids body)
        (closure ids body env))  ; Crea un cierre (closure) con parámetros y cuerpo

      ; Aplicación de funciones
      (app-exp (rator rands)
        (let ((proc (eval-expresion rator env))
              (args (eval-rands rands env)))
          (if (procval? proc)
              (apply-procedure proc args env)  ; Aplica el procedimiento si es válido
              (eopl:error 'eval-expresion
                          "Attempt to apply non-procedure ~s" proc))))  ; Error si no es un procedimiento válido

      ; Evaluación de expresiones begin
      (begin-exp (exp exps)
        (let loop ((acc (eval-expresion exp env))
                   (exps exps))
          (if (null? exps) 
              acc
              (loop (eval-expresion (car exps) env)
                    (cdr exps)))))  ; Evaluación secuencial de expresiones

      ; Declaración de variables
      (decl-exp (declarcion_ambiente) 
        (cases var-decl declarcion_ambiente
          (let-exp (ids rands body)
            (let ((args (eval-rands rands env)))
              (eval-expresion body
                              (extend-env ids args env))))  ; Declaración let
          (lvar-exp (ids rands body)
            (let ((args (eval-rands rands (mod-extend-env ids (list->vector rands) env))))
              (eval-expresion body
                              (mod-extend-env ids (list->vector args) env))))))  ; Declaración var

      ; Evaluación de bucles while
      (while-exp (boolean_exp body_exp)
        (cond 
          [(eval-expresion boolean_exp env)
           (eval-expresion body_exp env)
           (eval-expresion exp env)]
          [else 'void]))  ; Evaluación de bucle while

      ; Evaluación de bucles for
      (for-exp (var start-exp end-exp sum-exp body-exp)
        (let ((start (eval-expresion start-exp env))
              (end (eval-expresion end-exp env))
              (sum (eval-expresion sum-exp env)))
          (for-eval body-exp var start sum end env)))  ; Evaluación de bucle for

      ; Evaluación de expresiones switch
      (switch-exp (var_exp list_caso list_exp default_exp)
        (letrec ((valor (eval-expresion var_exp env))
                 (coinciden
                   (lambda (caso list_e valor)
                     (cond
                       [(null? caso) (eval-expresion default_exp env)]
                       [(equal? valor (eval-expresion (car caso) env))
                        (eval-expresion (car list_e) env)]
                       [else (coinciden (cdr caso) (cdr list_e) valor)]))))
          (coinciden list_caso list_exp valor)))  ; Evaluación de expresión switch

      ; Evaluación de expresiones match
      (match-exp (exp_var list_casos lista_exp)
        (let ((valor (eval-expresion exp_var env)))
          (detector_patron valor list_casos lista_exp env)))  ; Evaluación de expresión match

      ; Evaluación de números
      (num-exp (tipo_numero)
        (cases numero-exp tipo_numero
          (decimal-num (dato) dato)  ; Número decimal
          (bin-num (dato) dato)  ; Número binario
          (octal-num (dato) dato)  ; Número octal
          (hex-num (dato) dato)  ; Número hexadecimal
          (flotante-num (dato) dato)))  ; Número en punto flotante

      ; Evaluación de primitivas booleanas
      (prim-bool-exp (prim args)
        (operaciones_para_boleanos prim (eval-rands args env)))  ; Evaluación de primitiva booleana

      ; Evaluación de primitivas numéricas
      (prim-num-exp (exp1 prim exp2)
        (let ((eexp1 (eval-expresion exp1 env))
              (eexp2 (eval-expresion exp2 env)))
          (apply-primitive prim eexp1 eexp2)))  ; Evaluación de primitiva numérica

      ; Evaluación de primitivas de cadenas
      (prim-cad-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive_string prim args)))  ; Evaluación de primitiva de cadena

      ; Evaluación de cadenas
      (cadena-exp (primero resto)
        (letrec ((crear-string
                  (lambda (lids)
                    (cond
                      [(null? lids) ""]
                      [else (string-append " " (symbol->string (car lids)) (crear-string (cdr lids)))]))))
          (string-append (symbol->string primero) (crear-string resto)))) ; Concatenación de cadenas

      ; Evaluación de listas
      (lista-exp (Lexp)
        (eval-rands Lexp env))  ; Evaluación de expresiones en la lista

      ; Evaluación de cons (construcción de listas)
      (cons-exp (exp1 exp2)
        (cons (eval-rand exp1 env) (eval-rand exp2 env)))  ; Construcción de lista

      ; Evaluación de primitivas de listas
      (prim-list-exp (prim exp)
        (let ((arg (eval-rand exp env)))
          (apply-list prim arg)))  ; Aplicación de operación primitiva de lista

      ; Evaluación de arrays
      (array-exp (lista)
        (list->vector (eval-rands lista env)))  ; Conversión de lista a vector

      ; Evaluación de primitivas de arrays
      (prim-array-exp (primitiva lista_argumentos)
        (primitiva-array primitiva (eval-rands lista_argumentos env)))  ; Aplicación de operación primitiva de array

      ; Evaluación de listas vacías
      (empty-list-exp ()
        '())  ; Retorna lista vacía

      ; Evaluación de set (asignación de valores)
      (set-exp (identifi expresion)
        (let ((args (eval-expresion expresion env)))
          (set-valor-ambiente env identifi args)
        )
      ) 

      ; Evaluación de expresiones de estructuras (no implementadas en detalle)
      (new-struct-exp (identi lista_atributos)
        (let ((struc (buscar-structura identi)))
          (if (= (length (struct-decl->atributos struc)) (length lista_atributos))
            (list (struct-decl->atributos struc) (list->vector (eval-rands lista_atributos env)))
            (eopl:error "lista no coincide")
          )
        )
      )
      
      (get-struct-exp (struc atributo)
        (let ((struc (eval-rand struc env)))
          (vector-ref  (cadr struc) (encontrar-atributo (car struc) atributo 0) )
        )
      ) 

      (set-struct-exp (strucVar atributo nuevo_valor)
        (let ((struc (eval-rand strucVar env)) (valor_nuevo (eval-rand nuevo_valor env)))
          (vector-set! (cadr struc) (encontrar-atributo (car struc) atributo 0 ) valor_nuevo)
        )  
      ) 
    )
  )
)

  
; Fin de función eval-expresion


; funciones auxiliares para aplicar eval-expresion a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expresion rand env)))

; Funciones para operaciones lógicas
(define operaciones_para_boleanos
(lambda (op lista_argumentos)
  (cases primitivaBooleana op 
    (and-prim () 
      (and (car lista_argumentos) (cadr lista_argumentos)))
    (or-prim () 
      (or (car lista_argumentos) (cadr lista_argumentos)))
    (not-prim () 
      (not (car lista_argumentos)))
    (xor-prim () 
      (or (and (car lista_argumentos) (not (cadr lista_argumentos))) (and (not (car lista_argumentos)) (cadr lista_argumentos)))))))


;; Métodos para números

; Función principal para eliminar un carácter específico de una cadena
(define eliminar-caracter
  (lambda (stri ch)
    ; Definir la función auxiliar quitar_caracter dentro de un letrec
    (letrec ((quitar_caracter 
               (lambda (str ch)
                 (cond
                   [(null? str) '()]
                   [else 
                    (if (char=? ch (car str))
                        (quitar_caracter (cdr str) ch)
                        (cons (car str) (quitar_caracter (cdr str) ch)))
                   ]
                 ))))
      ; Convertir la cadena a una lista de caracteres, eliminar el carácter y volver a convertir a cadena
      (list->string (quitar_caracter (string->list stri) ch))
    )
  )
)

; Función principal para reemplazar caracteres en una cadena
(define sustituir-caracter
  (lambda (stri ch new-ch)
    (letrec 
      ((reemplazar-caracter 
         (lambda (str caracter nuevo-caracter)
           (cond
             [(null? str) '()]
             [(char=? caracter (car str)) (cons nuevo-caracter (reemplazar-caracter (cdr str) caracter nuevo-caracter))]
             [else (cons (car str) (reemplazar-caracter (cdr str) caracter nuevo-caracter))]
           ))))
      (list->string (reemplazar-caracter (string->list stri) ch new-ch))
    )
  )
)
(define es-binario?
  (lambda (num)
    (or (and (equal? (string-ref num 0) #\b)
             (or (equal? (string-ref num 1) #\1) (equal? (string-ref num 1) #\0))
             (or (equal? (string-ref num (- (string-length num) 1)) #\1) (equal? (string-ref num (- (string-length num) 1)) #\0))
        )
        (and (equal? (string-ref num 0) #\-)
             (equal? (string-ref num 1) #\b)
             (or (equal? (string-ref num 2) #\1) (equal? (string-ref num 2) #\0))
             (or (equal? (string-ref num (- (string-length num) 1)) #\1) (equal? (string-ref num (- (string-length num) 1)) #\0))
        )))
)

(define es-hexadecimal?
  (lambda (num)
    (or (and (>= (string-length num) 2)
             (equal? (string-ref num 0) #\h)
             (equal? (string-ref num 1) #\x))
        (and (>= (string-length num) 3)
             (equal? (string-ref num 0) #\-)
             (equal? (string-ref num 1) #\h)
             (equal? (string-ref num 2) #\x)))))


(define (es-octal? num)
  (or (and (>= (string-length num) 2)
           (equal? (string-ref num 0) #\0)
           (equal? (string-ref num 1) #\x))
      (and (>= (string-length num) 3)
           (equal? (string-ref num 0) #\-)
           (equal? (string-ref num 1) #\0)
           (equal? (string-ref num 2) #\x))))

;funcion que opera 2 numero y le aplica la funcion transformar
(define mandejador_de_bases
  (lambda (operacion num1 num2 transformar)
    (cond
      [(string? num1)
       (cond
          ;; Verificar binario: empieza con b o -b seguido de 0 o 1
          [(and (es-binario? num1) (es-binario? num2))

            (transformar 
              (operacion 
                (string->number (eliminar-caracter  num1 #\b) 2) 
                (string->number (eliminar-caracter  num2 #\b) 2))
              2)
          ]
          ;; Verificar hexadecimal: empieza con hx o -hx
          [(and (es-hexadecimal? num1) (es-hexadecimal? num2))

            (transformar 
              (operacion 
                (string->number (sustituir-caracter num1 #\h #\#) 16) 
                (string->number (sustituir-caracter num2 #\h #\#) 16))
              16)
          ]
          ;; Verificar octal: empieza con 0x o -0x
          [(and (es-octal? num1) (es-octal? num2))
            (transformar 
              (operacion 
                (string->number (eliminar-caracter  num1 #\x) 8) 
                (string->number (eliminar-caracter  num2 #\x) 8))
              8)
          ]
          [else (eopl:error "Error numero no valido")]
        )
      ]
      [(and (number? num1) (number? num2)) (operacion num1 num2)]
      [else (eopl:error "error numero no valido")]
    )
  )
)

; Función para convertir un número a cadena según la base
(define convertir-numero
  (lambda (num base)
    (let ((negativo? (< num 0)))      ; Verificar si el número es negativo
      (case base
        [(2) (if negativo? 
          (string-append "-" "b" (number->string (abs num) 2))   ; Convertir a binario
          (string-append "b" (number->string (abs num) 2)))]     ; Convertir a binario
        [(8) (if negativo? 
          (string-append "-" "0x" (number->string (abs num) 8))  ; Convertir a octal
          (string-append "0x" (number->string (abs num) 8)))]    ; Convertir a octal
        [(16) (if negativo? 
          (string-append "-" "hx" (number->string (abs num) 16)) ; Convertir a hexadecimal
          (string-append "hx" (number->string (abs num) 16)))]   ; Convertir a hexadecimal
        [else (eopl:error "Base no soportada")]   ; Caso de base no soportada
      )
    )
  )
)


(define apply-primitive
  (lambda (prim num1 num2)
    (letrec
      (
        (retornar_un_boleano
          (lambda (out base) out)
        )
        (retornar_valor
          (lambda (operador arg1 arg2)
            (mandejador_de_bases operador arg1 arg2 retornar_un_boleano)
          )
        )
        (convertir_valor
          (lambda (operador arg1 arg2)
            (mandejador_de_bases operador arg1 arg2 convertir-numero)
          )
        )
      )
      (cases primitive prim
        (sum-prim () (convertir_valor + num1 num2  ))
        (minus-prim () (convertir_valor - num1 num2 ))
        (mult-prim () (convertir_valor * num1 num2 ))
        (mayor-prim () (retornar_valor > num1 num2 ))
        (menor-prim () (retornar_valor < num1 num2 ))
        (menorigual-prim () (retornar_valor <= num1 num2 ))
        (mayorigual-prim () (retornar_valor >= num1 num2 ))
        (diferente-prim ()( not (equal? num1 num2)))
        (igual-prim () (equal? num1 num2))
        (mod-prim () (convertir_valor remainder num1 num2 ))
        (elevar-prim () (convertir_valor expt num1 num2))
      )
    )
  )
)


; apply-primitive_string: <primitiva> <list-of-expressions> -> numero
; Función principal para aplicar primitivas de cadenas
(define apply-primitive_string
  (lambda (prim args)
    (letrec
      (
        ; Función auxiliar para concatenar una lista de cadenas
        (concat-strings 
          (lambda (string-list) (apply string-append string-list)))
      )
      ; Aplicar la primitiva de cadena adecuada
      (cases primitivaCadena prim
        (concat-primCad () (concat-strings args))  ; Concatenar todas las cadenas
        (length-primCad () (string-length (car args)))  ; Obtener la longitud de la primera cadena
        (index-primCad () (string (string-ref (car args) (cadr args))))))))  ; Obtener el carácter en el índice

    
; Función principal para aplicar primitivas de listas
(define apply-list
(lambda (prim arg)
  (cases primitivaListas prim
    ; Devuelve el primer elemento de la lista
    (first-primList () (car arg))
    ; Devuelve el resto de la lista excluyendo el primer elemento
    (rest-primList () (cdr arg))
    ; Verifica si la lista está vacía
    (empty-primList () (null? arg)))))

; Función principal para aplicar primitivas de arreglos
(define primitiva-array
(lambda (prim arg)
(letrec
  (
    ; Función auxiliar para obtener un subvector de un vector dado un rango
    (subvector
      (lambda (vect inicio final)
        (cond
          [(= inicio final) (cons (vector-ref vect inicio) '())]  ; Si inicio es igual a final, devolver el elemento en inicio
          [else (cons (vector-ref vect inicio) (subvector vect (+ inicio 1) final))]))))  ; Agregar el elemento en inicio al resultado recursivo

  ; Aplicar la primitiva de arreglo adecuada
  (cases primitivaArray prim
    ; Obtener la longitud del vector
    (length-primArr () (vector-length (car arg)))
    ; Obtener el elemento en un índice específico del vector
    (index-primArr () (vector-ref (car arg) (cadr arg)))
    ; Obtener un subvector dado un rango
    (slice-primArr () (list->vector (subvector (car arg) (cadr arg) (caddr arg))))
    ; Establecer un valor en un índice específico del vector y devolver el vector
    (setlist-primArr () 
      (vector-set! (car arg) (cadr arg) (caddr arg))
      (car arg))))))

; Función para evaluar un bucle for
(define for-eval
(lambda (body-exp var i sum end env)
(cond
  ; Si i es menor que end, ejecutar el cuerpo del bucle
  [(< i end) 
    (eval-expresion body-exp (extend-env (list var) (list i) env))  ; Evaluar la expresión del cuerpo con i en el entorno extendido
    (for-eval body-exp var (+ i sum) sum end env)  ; Llamar recursivamente a for-eval incrementando i en sum
  ]
  ; Cuando i no es menor que end, devolver 'void'
  [else 'void])))

; Función para realizar coincidencias de patrones
(define detector_patron 
(lambda (valor primt_match expresi_match env)
(cases regular-exp (car primt_match)
; Coincidencia para expresión vacía
(empty-match-exp ()
  (if (null? valor)
      (eval-expresion (car expresi_match) env)  ; Evaluar expresión si valor es vacío
      (detector_patron valor (cdr primt_match) (cdr expresi_match) env)  ; Seguir buscando coincidencia
  )
)
; Coincidencia para lista
(list-match-exp (cabeza cola)
  (if (and (list? valor) (not (null? valor)))
      (eval-expresion (car expresi_match)
                      (extend-env (cons cabeza (cons cola '())) (list (car valor) (cdr valor)) env))  ; Evaluar expresión con valores de la lista
      (detector_patron valor (cdr primt_match) (cdr expresi_match) env)  ; Seguir buscando coincidencia
  )
)
; Coincidencia para número
(num-match-exp (ids)
  (if (number? valor)
      (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))  ; Evaluar expresión con el número
      (if (string? valor)
        (cond
          [(or (es-binario? valor) (es-hexadecimal? valor) (es-octal? valor))
          (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))] 
          [else (detector_patron valor (cdr primt_match) (cdr expresi_match) env)]  ; Seguir buscando coincidencia en otros casos
        )
        (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
      )
  )
)
; Coincidencia para cadena
(cad-match-exp (ids)
  (if (string? valor) 
    (cond
      [(and (not (es-binario? valor)) (not (es-hexadecimal? valor) (not (es-octal? valor))))
      (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))]
      [else (detector_patron valor (cdr primt_match) (cdr expresi_match) env)]
    )
    (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
  )
)
; Coincidencia para booleano
(bool-match-exp (ids)
  (if (boolean? valor)
      (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))  ; Evaluar expresión con el valor booleano
      (detector_patron valor (cdr primt_match) (cdr expresi_match) env)  ; Seguir buscando coincidencia
  )
)
; Coincidencia para arreglo
(array-match-exp (lids)
  (if (vector? valor)
      (eval-expresion (car expresi_match) (extend-env lids (vector->list valor) env))  ; Evaluar expresión con el vector convertido a lista
      (detector_patron valor (cdr primt_match) (cdr expresi_match) env)  ; Seguir buscando coincidencia
  )
)
; Coincidencia por defecto
(default-match-exp ()
  (eval-expresion (car expresi_match) env)))))  ; Evaluar expresión por defecto


; Fin de estructuras





;*******************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))


;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args env-actual)
    (cases procval proc
      (closure (ids body env)
        (eval-expresion body (extend-env ids args env-actual))
      )
    )
  )
)

;*******************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (syms (list-of symbol?))
   (valores (list-of scheme-value?))
   (env environment?))
  (mod-extend-env
   (syms (list-of symbol?))
   (valores vector?)
   (env environment?))
)

(define scheme-value? (lambda (v) #t))



;funcion para buscar una variable en un ambiente
(define buscador 
  (lambda (listIds listVal valor_vuscado next-amb acc env)
    (cond
      [(null? listIds) (apply-env next-amb valor_vuscado)]
      [(equal? (car listIds) valor_vuscado) 
        (cond
          [(vector? listVal) (if (expresion? (vector-ref listVal acc)) (eval-expresion (vector-ref listVal acc) next-amb) (vector-ref listVal acc))]
          [(list? listVal) (list-ref listVal acc)]
        )
      ]
      [else (buscador (cdr listIds) listVal valor_vuscado next-amb (+ acc 1) env)]
    )  
  ) 
)

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env () (eopl:error "variable existe en ningun ambiente " sym))
      (extend-env (lid lval next-env)
        (buscador lid lval sym next-env 0 env)
      )
      (mod-extend-env (lid lval next-env)
        (buscador lid lval sym next-env 0 env)
      )
    ) 
  )
)

;funcion para buscar una variable en un ambiente
(define buscador! 
  (lambda (listIds listVal valor_vuscado next-amb acc valor_nuevo)
    (cond
      [(null? listIds) (set-valor-ambiente next-amb valor_vuscado valor_nuevo)]
      [(equal? (car listIds) valor_vuscado) 
        (cond
          [(vector? listVal) (vector-set! listVal acc valor_nuevo) ]
          [(list? listVal) (set-valor-ambiente next-amb valor_vuscado valor_nuevo)]
        )
      ]
      [else (buscador! (cdr listIds) listVal valor_vuscado next-amb (+ acc 1) valor_nuevo)]
    )  
  ) 
)
;función que busca un símbolo en un ambiente
(define set-valor-ambiente
  (lambda (env sym valor_nuevo)
    (cases environment env
      (empty-env () (eopl:error "variable existe en ningun ambiente " sym))
      (extend-env (lid lval next-env)
        (set-valor-ambiente next-env sym valor_nuevo)
      )
      (mod-extend-env (lid lval next-env)
        (buscador! lid lval sym next-env 0 valor_nuevo)
      )
    ) 
  )
)

;******************************
;structuras

(define struct-decl->nombre
  (lambda (struct)
    (cases struct-decl struct
      (struct-exp (struct-name field-list)
        struct-name)
      (else (eopl:error "Invalid struct"))
    )
  )
)

(define struct-decl->atributos
  (lambda (struct)
    (cases struct-decl struct
      (struct-exp (struct-name field-list)
        field-list)
      (else (eopl:error "Invalid struct"))
    )
  )
)

(define struct-env '())


(define actulizar-ambiente-struct!
  (lambda (s-decls)
    (set! struct-env s-decls)))

(define buscar-structura
  (lambda (name)
    (let loop ((env struct-env))
      (cond
        [(null? env) (eopl:error "Unknown struct: " name)]
        [(eqv? (struct-decl->nombre (car env)) name) (car env)]
        [else (loop (cdr env))]
      )
    )
  )
)

(define encontrar-atributo
  (lambda (attributes attr acc)
    (cond
      [(null? attributes) (eopl:error "Attribute not found: " attr)]
      [(equal? (car attributes) attr) acc]
      [else (encontrar-atributo (cdr attributes) attr (+ acc 1))]
    )
  )
)

(show-the-datatypes)

;(define un-programa-dificil (a-program una-expresion-dificil))

;; Descomentar para pruebas, vuelva a comentar para envitar
(interpretador)

#|
No tocar
Exporar funciones
|#
(provide (all-defined-out))
