set stack = "" 
alias cPUSH            'set stack = (\!:1 $stack)' 
alias rPUSH            'set stack = ($\!:1 $stack)' 
alias sPUSH            '@ stack[1] = $stack[1] + 1 ; set stack[1] = $stack[$stack[1]]' 
alias LOAD            'eval "set \!:1 = \$stack[1] ; shift stack"' 
alias OP2                'eval "@ stack[2] = \$stack[2] \!:1 \$stack[1]"; shift stack' 
alias cJUMP           'set tos = $stack[1]; shift stack; if ($tos == 0) goto \!:1' 
alias JUMP             goto 
alias PRINT            'echo $stack[1]; shift stack' 
alias READ            'eval "set \!:1 = $< " '

