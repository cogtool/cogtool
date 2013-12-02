# Kleine Routine, die den Wert des Maschinenstacks zurückliefert.

        .text

#    extern void* getSP (void);
        .globl getSP
        .align 2
        .ent getSP
getSP:  move $2,$sp   # get stack pointer
        j $31         # return
        .end getSP

