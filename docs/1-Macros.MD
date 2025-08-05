# Macros

``#D MACRO : MBFI#``  
For the rest of the docs, MBFI are macro actions, BF instructions, and comments you can put.

Actions in macro-BF always start with an ``#`` followed by a character in caps or a symbol. This is useful to avoid Brainfuck program incompatibilities, as BF comments are often reserved in related projects.

So the ``#D`` is necessary, ``#d`` doesn't work.

After it, you will have the name of the macro. The preprocessor ignores spaces (\n, \t, and ' ') when you define the name, so  
``#DMACRO#`` == ``#D MACRO#`` == ``#D M	A C RO#`` (all the same).  
The ':' delimits the macro name and the MBFI (Note 1: you can use ':' after the first one, it won’t do anything and will count as an MBFI. The spaces before a non-space character won't be counted for every line you jump, to avoid some issues). The MACRONAME is a maximum of 100 characters long.  
However, not every macro wants MBFI, for those,  
``#D MACRO#`` is perfectly valid, as mentioned earlier.

The last `#` is used to delimit since macro actions can take multiple lines.

After defining the macro, we can use it by typing:  
``#$MACRO#``  
Like the others, it doesn't care about spaces.

## Edge Cases

A macro definition can contain no name, it won't return an error. For example, ``#D: +#`` defines a macro with no name that can be used like this: ``#$#``. This can be useful.

A macro definition can contain ':' at the end, like this one: ``#DMACRO:#``, it won't return an error, but it's not recommended since it's redundant compared to the one without ':'.

Using an undefined macro will return an error, defining an already defined one will redefine it.

## Undefining a Macro

You can use the macro action ``#U MACRO#`` to undefine a macro.

## Macro Arguments

You can define a macro that takes arguments like this:  
```bf
#D MACRO(x, y) : MBFI#
```  
The names placed between commas are only commentary, you can write this as well: ``#D MACRO(,)#``.  
Then  
```bf
#$MACRO(</>, </>)#
```  
where '<', and '>' are reserved, and the other characters are only commentary.

However, arguments aren't that easy in Brainfuck, so there are important things to know.

When writing this, the preprocessor will copy the values given in the macro, using a copy cell, since we don't want to lose the cells of the arguments.

To do a thing like this, we need to set a convention. To make it simpler, here's a visualization of memory:

```
-------------------------    
|cpycell|arg1|arg2||argn|
-------------------------
```
The implementation considers the current cell as the copy cell and assumes that the cpycell and argn are set to zero. It uses the copy cell to copy into the nargs of the macro and gives back the values of the given arguments. The only way to do this is through a loop in Brainfuck.

For instance:  
```bf
#$MACRO(<<, <)#
```  
generates the following Brainfuck code (if MACRO doesn't have a null MBFI - see note 2)

```bf
    <<[->>+>+<<<]>>[-<<+>>]<[->+>>+<<<]>[-<+>]
```
There are some issues with this. The first one is about the risk of going out of the cell range since the preprocessor doesn't check the given arguments. The second is the time it takes to copy everything if the distance is too big. The third is the question about signed/unsigned numbers, which is partially solved since the preprocessor allows you to define macros that take negative cells: 
```bf
#D MACRO(-,) : MBFI#.
``` 
The minus symbol indicates to the preprocessor to loop especially for negative cell arguments, it can create issues if the user of the macro inputs the opposite sign of what the macro specified.
The preprocessor, after this step (so at the start of the actual macro actions), points to the copy cell.
Since using macro arguments as the preprocessor provides can be a constraint, the preprocessor also allows you to skip the copy step and gives you the responsibility of filling the cells with the symbol '!', which you can put in: 
```bf
#$!MACRO#
```
By convention, a macro with arguments is composed of cpycell, args, and return. Macros can keep args safe by not editing them or making it seem that they weren't edited. If it's not argsafe, the arg(s) are the return(s), if it's the return(s), they are distinct cell(s). Feel free to respect or not this convention, as long you don't lose yourself.

## Strings
The preprocessor has an integrated macro for strings (a macro that cannot be defined that easily using MBF, so defined with C). You cannot interact with it like the macros you would define.
```bf
#"String"
``` 
(Note 3: as you can see, it's delimited with ``#""``, no ``#`` at the end like other macro actions, it's a way to make it easier to read and not redundant).
The string macro supposes that every cells are null, like seen previously, the preprocessor cannot reset cells safely.

Here's a string, the preprocessor just takes the ASCII values of each character and puts it in Brainfuck cells. The first character is in the current cell, and the last character is in current cell - 1.
```bf
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ c=83
>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ c=116
>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ c=114
>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ c=105
>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ c=110
>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ c=103
>
```

The preprocessor writes the given ASCII value of each character with c=N to facilitate reading when translating to BF. The #"STRING" also supports escape sequences like \n, octal hex, and a good part of escape sequences in C.
