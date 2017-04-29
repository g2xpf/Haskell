# Haskell Tutorial
These files are for local use.

## How To Use
Firstly, you should compile the 'TMInterpreter.hs'.
Secondly, execute ./TMInterpreter, and type 'delta.tm'.
You can get a new file 'delta.hs'.
Thirdly, compile 'delta.hs' and execute './delta'.
Finally, type the tape symbols such as 'B 1010B', 'B 0B', and so on.

## How To Write A TM File
Write δ for each cases as below :

(q, s) -> (q', s', s'');

q, q' : State
s : Tape symbol

Do not forget to add a semicolon at the end.

## File Tree
- TMInterpreter.hs
- delta.tm
- Library
  - EmulateTM.hs
