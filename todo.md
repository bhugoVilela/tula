# TODO
- Pattern matching
- Evaluation through type checking
- Interpreter flags
    - A way to pass information to the interpreter so it can choose what to do 
    - ie. 
    ```
    case S a b -> S2 [print | printState | printDebug | printTape | print (a)]
    case S a b -> S2 [(on-eval print)]
    ```
