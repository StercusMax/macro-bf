# Introduction
Macro-bf isn't a Brainfuck-inspired programming language, it's just a preprocessor.

I remember creating a new language in Brainfuck in the past, but it's not as fun to use as it is to create. If we want to stick to native Brainfuck, the poor modularity of Brainfuck imposes certain limits on code writing. This is where Macro-bf comes into play. Inspired by the C preprocessor (adapted for Brainfuck), it provides the power to include files and create macros, along with various other useful features. It translates quickly and easily to Brainfuck since it's just a preprocessor.

##Write your first macro-bf program
```bf
#D add1 : +#
#$ add1#
```
The first line defines a macro named add1, with the semicolon delimiting the macro name and the content you want to include.

##Macro-bf to bf
To compile your Macro-bf code, simply take the preprocessor binary and type:
```bash
./mbf file.mbf [needed] output.bf [facultative, by default output.bf]```
This will generate a file with the content:
``bf
+`` 
which is exactly what we want

##Available actions
Here’s a summary of the available macro actions. However, I recommend checking the ``docs`` for more detailed information regarding naming rules, implementation specifics, and more. This summary is mainly targeted at people who have already reviewed that.



Definition:
``#D MACRO : MBFI#``
with arguments:
``#D MACRO(x, y) : MBFI#``

Undefine:
``#U MACRO#``

if def:
``#I MACRO : MBFI#``
ifndef:
``#IN MACRO : MBFI#``

Usage:
``#$ MACRO#``
with arguments:
``#$MACRO(</>, </>)#`` 

Integrated macro:
``#"STRING"`` Supports every ASCII escaped sequence of C

Include file:
``#%FILE#``

##Contributing
Contributions are welcome, some bugs are probably hiding in my implementation.

Happy brainfucking
