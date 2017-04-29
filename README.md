# Haskell Tutorial
These files are for local use.

## How To Use
- You should compile the 'TMInterpreter.hs'.
- Execute ./TMInterpreter, and type 'delta.tm'. You'll get a new file 'delta.hs'.
- Compile 'delta.hs' and execute './delta'.
- Type the tape symbols such as 'B 1010B', 'B 0B', and so on.

## How To Write A TM File
Write δ for each cases as below :

(q, s) -> (q', s', s'');

q, q' : State
s : Tape symbol

Do not forget to add a semicolon at the end.

## File Tree
\- TMInterpreter.hs

\- delta.tm

\- Library
    \- EmulateTM.hs
