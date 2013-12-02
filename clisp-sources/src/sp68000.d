! Kleine Routine, die den Wert des Maschinenstacks zurückliefert.

           .text

           .globl getSP
           .globl setSP

!    extern void* getSP (void);
getSP:     lea sp@(4),a0   ! aktueller Wert von SP + 4 wegen Unterprogrammaufruf
           movel a0,d0     ! in D0 = Ergebnisregister
           rts

!    extern void setSP (void* sp_init_address);
setSP:     movel sp@+,a0   ! Returnadresse nach A0
           movel sp@,sp    ! SP auf den übergebenen Wert setzen
           jmp a0@         ! zurückspringen

