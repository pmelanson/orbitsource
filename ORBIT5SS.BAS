        DEFDBL A-Z
        DIM P(39, 11), Px(39, 5), Py(39, 5), Vx(39), Vy(39), B(1, 250), Ztel(50), nme$(48), clr(8), clrr(4)
        DIM DEVICEs(56), SOURCEs(5, 30), SHORT(5)
        ON ERROR GOTO 5000
        PALETTE 6, 5
        CLS
97      LOCATE 5, 5
        PRINT "Simulation Error Activation Utility for ORBIT v. 5O"
        PRINT
        INPUT ; "    path to main program: ", zpath$
        IF UCASE$(zpath$) = "QUIT" THEN END
        IF zpath$ = "*" THEN zpath$ = "\\Pavilion\c\qbasic\"
98      OPEN "I", #1, zpath$ + "OSBACKUP"
        CLOSE #1
        clr(0) = 10
        clr(1) = 11
        clr(2) = 14
        clr(3) = 13
        clr(4) = 12
        clrr(0) = 10
        clrr(1) = 6
        clrr(2) = 12
        WINDref = -1
        SOURCEs(1, 4) = 10000
        SOURCEs(2, 4) = 120
        SOURCEs(3, 4) = 120
        SOURCEs(4, 4) = 100000
        SOURCEs(5, 4) = 100000
        SOURCEs(0, 1) = 1
        SOURCEs(0, 2) = 1
        GOSUB 120
        CLS
        COLOR 9, 0
        LOCATE 1, 18: PRINT "ENTER";
        COLOR 10, 0
        LOCATE 1, 24: PRINT "UPDATED";
        COLOR 7, 0


        'Initialization Parameters
        'Radius P(i,5) and Mass P(i,4) of solar system objects
        OPEN "I", #1, "starsr"
        FOR i = 1 TO 3021
         INPUT #1, z
         INPUT #1, z
         INPUT #1, z
        NEXT i
        FOR i = 1 TO 241
         INPUT #1, z
         INPUT #1, z
        NEXT i
        FOR i = 0 TO 39
         INPUT #1, P(i, 0)
         INPUT #1, P(i, 4)
         INPUT #1, P(i, 5)
         INPUT #1, P(i, 8)
         INPUT #1, P(i, 9)
         INPUT #1, P(i, 10)
        NEXT i
        INPUT #1, year, day, hr, min, sec
        FOR i = 0 TO 35
         INPUT #1, Px(i, 3), Py(i, 3), Vx(i), Vy(i), P(i, 1), P(i, 2)
        NEXT i
        FOR i = 0 TO 39
         INPUT #1, nme$(i)
        NEXT i
        Px(37, 3) = 4446370.8284487# + Px(3, 3): Py(37, 3) = 4446370.8284487# + Py(3, 3): Vx(37) = Vx(3): Vy(37) = Vy(3)
        CLOSE #1
        nme$(40) = "  progrd"
        nme$(41) = "retrogrd"
        nme$(42) = " deptarg"
        nme$(43) = " apptarg"
        nme$(44) = " proVtrg"
        nme$(45) = " retVtrg"


        Zufo = 2
        Ztarg = 38
        Zorient = 0
        Zrefalt = 100000
        VUFO = 1
        AU = 149597890000#
        RAD = 57.295779515#
        G = 6.673E-11
        Px(0, 3) = 0
        Py(0, 3) = 0
        Vx(0) = 0
        Vy(0) = 0
        P(0, 1) = 0
        P(0, 2) = 0
        GOSUB 800
        GOSUB 830
        GOSUB 300
        GOSUB 400
        tt = TIMER

        'MAIN PROGRAM
1       z$ = INKEY$
        IF z$ = CHR$(27) THEN END
        IF z$ = CHR$(0) + "H" OR z$ = CHR$(0) + "P" OR z$ = CHR$(0) + "=" THEN 2
        IF (z$ = CHR$(0) + "K" OR z$ = CHR$(0) + "M") AND ufoFLAG = 0 THEN 2
        IF z$ <> "" THEN COLOR 12, 4: LOCATE 1, 24: PRINT "UPDATE"; : COLOR 7, 0: PRINT " ";
2       IF z$ = CHR$(13) THEN GOSUB 100
        IF z$ = "a" THEN DEVICEs(1) = DEVICEs(1) + 1: IF DEVICEs(1) > 2 THEN DEVICEs(1) = 0
        IF z$ = "b" THEN DEVICEs(2) = DEVICEs(2) + 1: IF DEVICEs(2) > 2 THEN DEVICEs(2) = 0
        IF z$ = "c" THEN DEVICEs(3) = DEVICEs(3) + 1: IF DEVICEs(3) > 2 THEN DEVICEs(3) = 0
        IF z$ = "d" THEN DEVICEs(6) = DEVICEs(6) + 1: IF DEVICEs(6) > 2 THEN DEVICEs(6) = 0
        IF z$ = "e" THEN DEVICEs(7) = DEVICEs(7) + 1: IF DEVICEs(7) > 2 THEN DEVICEs(7) = 0
        IF z$ = "f" THEN DEVICEs(8) = DEVICEs(8) + 1: IF DEVICEs(8) > 2 THEN DEVICEs(8) = 0
        IF z$ = "g" THEN DEVICEs(9) = DEVICEs(9) + 1: IF DEVICEs(9) > 2 THEN DEVICEs(9) = 0
        IF z$ = "h" THEN DEVICEs(10) = DEVICEs(10) + 1: IF DEVICEs(10) > 2 THEN DEVICEs(10) = 0
        IF z$ = "i" THEN DEVICEs(11) = DEVICEs(11) + 1: IF DEVICEs(11) > 2 THEN DEVICEs(11) = 0
        IF z$ = "j" THEN DEVICEs(12) = DEVICEs(12) + 1: IF DEVICEs(12) > 2 THEN DEVICEs(12) = 0
        IF z$ = "k" THEN DEVICEs(13) = DEVICEs(13) + 1: IF DEVICEs(13) > 2 THEN DEVICEs(13) = 0
        IF z$ = "l" THEN DEVICEs(19) = DEVICEs(19) + 1: IF DEVICEs(19) > 2 THEN DEVICEs(19) = 0
        IF z$ = "m" THEN DEVICEs(20) = DEVICEs(20) + 1: IF DEVICEs(20) > 2 THEN DEVICEs(20) = 0
        IF z$ = "n" THEN DEVICEs(21) = DEVICEs(21) + 1: IF DEVICEs(21) > 2 THEN DEVICEs(21) = 0
        IF z$ = "o" THEN DEVICEs(22) = DEVICEs(22) + 1: IF DEVICEs(22) > 2 THEN DEVICEs(22) = 0
        IF z$ = "p" THEN DEVICEs(23) = DEVICEs(23) + 1: IF DEVICEs(23) > 2 THEN DEVICEs(23) = 0
        IF z$ = "q" THEN DEVICEs(34) = DEVICEs(34) + 1: IF DEVICEs(34) > 2 THEN DEVICEs(34) = 0
        IF z$ = "r" THEN DEVICEs(35) = DEVICEs(35) + 1: IF DEVICEs(35) > 2 THEN DEVICEs(35) = 0
        IF z$ = "s" THEN DEVICEs(36) = DEVICEs(36) + 1: IF DEVICEs(36) > 2 THEN DEVICEs(36) = 0
        IF z$ = "t" THEN DEVICEs(37) = DEVICEs(37) + 1: IF DEVICEs(37) > 2 THEN DEVICEs(37) = 0
        IF z$ = "u" THEN DEVICEs(38) = DEVICEs(38) + 1: IF DEVICEs(38) > 2 THEN DEVICEs(38) = 0
        IF z$ = "v" THEN DEVICEs(39) = DEVICEs(39) + 1: IF DEVICEs(39) > 2 THEN DEVICEs(39) = 0
        IF z$ = "w" THEN DEVICEs(40) = DEVICEs(40) + 1: IF DEVICEs(40) > 2 THEN DEVICEs(40) = 0
        IF z$ = "x" THEN DEVICEs(55) = DEVICEs(55) + 1: IF DEVICEs(55) > 2 THEN DEVICEs(55) = 0
        IF z$ = "y" THEN DEVICEs(27) = DEVICEs(27) + 1: IF DEVICEs(27) > 2 THEN DEVICEs(27) = 0
        IF z$ = "z" THEN DEVICEs(28) = DEVICEs(28) + 1: IF DEVICEs(28) > 2 THEN DEVICEs(28) = 0
        IF z$ = "1" THEN DEVICEs(29) = DEVICEs(29) + 1: IF DEVICEs(29) > 2 THEN DEVICEs(29) = 0
        IF z$ = "2" THEN DEVICEs(30) = DEVICEs(30) + 1: IF DEVICEs(30) > 2 THEN DEVICEs(30) = 0
        IF z$ = "3" THEN DEVICEs(31) = DEVICEs(31) + 1: IF DEVICEs(31) > 2 THEN DEVICEs(31) = 0
        IF z$ = "4" THEN DEVICEs(32) = DEVICEs(32) + 1: IF DEVICEs(32) > 2 THEN DEVICEs(32) = 0
        IF z$ = "5" THEN DEVICEs(33) = DEVICEs(33) + 1: IF DEVICEs(33) > 2 THEN DEVICEs(33) = 0
        IF z$ = "6" THEN DEVICEs(41) = DEVICEs(41) + 1: IF DEVICEs(41) > 2 THEN DEVICEs(41) = 0
        IF z$ = "7" THEN DEVICEs(42) = DEVICEs(42) + 1: IF DEVICEs(42) > 2 THEN DEVICEs(42) = 0
        IF z$ = "8" THEN DEVICEs(43) = DEVICEs(43) + 1: IF DEVICEs(43) > 2 THEN DEVICEs(43) = 0
        IF z$ = "9" THEN DEVICEs(44) = DEVICEs(44) + 1: IF DEVICEs(44) > 2 THEN DEVICEs(44) = 0
        
        IF z$ = CHR$(0) + "<" THEN ufoFLAG = 1 - ufoFLAG: CLS : GOSUB 300: GOSUB 400
        IF z$ = CHR$(0) + "=" THEN UFOdisp = 1 * (1 - SGN(UFOdisp))
        IF z$ = CHR$(0) + ">" THEN UFOdisp = 2 * (1 - SGN(UFOdisp))
        IF z$ = CHR$(0) + "?" THEN UFOdisp = 3 * (1 - SGN(UFOdisp))
        IF ABS(Px(38, 3)) + ABS(Py(38, 3)) = 0 THEN UFOdisp = 0
        IF z$ = "[" AND WINDref > -1 THEN WINDref = WINDref - 1
        IF z$ = "]" AND WINDref < 31 THEN WINDref = WINDref + 1
        IF windACC >= 20 THEN DELwind = 10 ELSE DELwind = 1
        IF windACC <= 5 THEN DELwind = .1
        IF z$ = "{" AND windACC > .1 THEN windACC = windACC - DELwind
        IF z$ = "}" THEN windACC = windACC + DELwind
        IF z$ = "\" THEN WINDangle = WINDangle + 90
        IF WINDangle > 280 THEN WINDangle = 0
        IF WINDref = -1 THEN windACC = 0: WINDangle = 0
        IF z$ = "," AND HABfuelleak > 0 THEN HABfuelleak = HABfuelleak - .01
        IF z$ = "." THEN HABfuelleak = HABfuelleak + .01
        IF z$ = "<" AND AYSEfuelleak > 0 THEN AYSEfuelleak = AYSEfuelleak - .01
        IF z$ = ">" THEN AYSEfuelleak = AYSEfuelleak + .01
        IF z$ = CHR$(0) + ";" THEN master = 1 - master
        IF z$ = CHR$(0) + "D" THEN GOSUB 140
        IF z$ = CHR$(0) + CHR$(133) THEN RUN "orbit5sy.exe"
        IF z$ = CHR$(0) + CHR$(134) THEN RUN "orbit5sx.exe"
       
        IF ufoFLAG = 1 THEN 8
        IF z$ = CHR$(0) + "H" THEN SOURCEs(0, 1) = SOURCEs(0, 1) - 1
        IF z$ = CHR$(0) + "P" THEN SOURCEs(0, 1) = SOURCEs(0, 1) + 1
        IF z$ = CHR$(0) + "M" THEN SOURCEs(0, 2) = SOURCEs(0, 2) + 1
        IF z$ = CHR$(0) + "K" THEN SOURCEs(0, 2) = SOURCEs(0, 2) - 1
        IF SOURCEs(0, 1) > 5 THEN SOURCEs(0, 1) = 1
        IF SOURCEs(0, 1) < 1 THEN SOURCEs(0, 1) = 5
        IF SOURCEs(0, 2) > 4 THEN SOURCEs(0, 2) = 1
        IF SOURCEs(0, 2) < 1 THEN SOURCEs(0, 2) = 4
        IF SOURCEs(0, 2) = 1 AND z$ = "+" THEN SOURCEs(SOURCEs(0, 1), 1) = SOURCEs(SOURCEs(0, 1), 1) + 1
        IF SOURCEs(0, 2) = 1 AND z$ = "-" THEN SOURCEs(SOURCEs(0, 1), 1) = SOURCEs(SOURCEs(0, 1), 1) - 1: IF SOURCEs(SOURCEs(0, 1), 1) < -1 * SOURCEs(SOURCEs(0, 1), 4) THEN SOURCEs(SOURCEs(0, 1), 1) = -1 * SOURCEs(SOURCEs(0, 1), 4)
       
        IF SOURCEs(0, 2) = 2 AND z$ = "+" THEN SOURCEs(SOURCEs(0, 1), 2) = SOURCEs(SOURCEs(0, 1), 2) + 1
        IF SOURCEs(0, 2) = 2 AND z$ = "-" THEN SOURCEs(SOURCEs(0, 1), 2) = SOURCEs(SOURCEs(0, 1), 2) - 1: IF SOURCEs(SOURCEs(0, 1), 2) < 0 THEN SOURCEs(SOURCEs(0, 1), 2) = 0

        IF SOURCEs(0, 2) = 3 AND (z$ = "+" OR z$ = "-") THEN SOURCEs(SOURCEs(0, 1), 0) = 1 - SOURCEs(SOURCEs(0, 1), 0)
       
        IF SOURCEs(0, 2) = 4 AND z$ = "+" THEN SOURCEs(SOURCEs(0, 1), 3) = SOURCEs(SOURCEs(0, 1), 3) + 1: IF SOURCEs(SOURCEs(0, 1), 3) > 30 THEN SOURCEs(SOURCEs(0, 1), 3) = 30
        IF SOURCEs(0, 2) = 4 AND z$ = "-" THEN SOURCEs(SOURCEs(0, 1), 3) = SOURCEs(SOURCEs(0, 1), 3) - 1: IF SOURCEs(SOURCEs(0, 1), 3) < 0 THEN SOURCEs(SOURCEs(0, 1), 3) = 0
        

8       IF ufoFLAG = 0 THEN 5
        IF z$ = CHR$(0) + "H" THEN VUFO = VUFO - 1: IF VUFO = 0 THEN VUFO = 8
        IF z$ = CHR$(0) + "P" THEN VUFO = VUFO + 1: IF VUFO = 9 THEN VUFO = 1
        IF z$ = CHR$(0) + "M" THEN ON VUFO GOSUB 2000, 2010, 2070, 2020, 2060, 2030, 2040, 2050
        IF z$ = CHR$(0) + "K" THEN ON VUFO GOSUB 2000, 2110, 2170, 2120, 2160, 2130, 2140, 2150
        IF z$ = CHR$(0) + "R" THEN Ztarg = 38
        IF z$ = CHR$(0) + "S" THEN Ztarg = 39
        IF z$ = CHR$(0) + "O" THEN Ztarg = 28
        IF z$ = CHR$(0) + "Q" THEN Ztarg = 32
        IF z$ = CHR$(0) + "G" THEN Zref = 3
        IF z$ = CHR$(0) + "I" THEN Zref = 28

5       LOCATE 1, 1: COLOR 9, 0: PRINT "F1"; : COLOR 10 + (master * 2): PRINT " MASTER ALARM";
        LOCATE 2, 1: COLOR 9, 0: PRINT "a "; : COLOR clrr(DEVICEs(1)), 0: PRINT "RAD 1";
        LOCATE 3, 1: COLOR 9, 0: PRINT "b "; : COLOR clrr(DEVICEs(2)), 0: PRINT "RAD 1";
        LOCATE 4, 1: COLOR 9, 0: PRINT "c "; : COLOR clrr(DEVICEs(3)), 0: PRINT "AGRAV";
        LOCATE 5, 1: COLOR 9, 0: PRINT "d "; : COLOR clrr(DEVICEs(6)), 0: PRINT "ACC 1";
        LOCATE 6, 1: COLOR 9, 0: PRINT "e "; : COLOR clrr(DEVICEs(7)), 0: PRINT "ION 1";
        LOCATE 7, 1: COLOR 9, 0: PRINT "f "; : COLOR clrr(DEVICEs(8)), 0: PRINT "ACC 2";
        LOCATE 8, 1: COLOR 9, 0: PRINT "g "; : COLOR clrr(DEVICEs(9)), 0: PRINT "ION 2";
        LOCATE 9, 1: COLOR 9, 0: PRINT "h "; : COLOR clrr(DEVICEs(10)), 0: PRINT "ACC 3";
        LOCATE 10, 1: COLOR 9, 0: PRINT "i "; : COLOR clrr(DEVICEs(11)), 0: PRINT "ION 3";
        LOCATE 11, 1: COLOR 9, 0: PRINT "j "; : COLOR clrr(DEVICEs(12)), 0: PRINT "ACC 4";
        LOCATE 12, 1: COLOR 9, 0: PRINT "k "; : COLOR clrr(DEVICEs(13)), 0: PRINT "ION 4";
        LOCATE 2, 12: COLOR 9, 0: PRINT "l "; : COLOR clrr(DEVICEs(19)), 0: PRINT "RCSP ";
        LOCATE 3, 12: COLOR 9, 0: PRINT "m "; : COLOR clrr(DEVICEs(20)), 0: PRINT "COM  ";
        LOCATE 4, 12: COLOR 9, 0: PRINT "n "; : COLOR clrr(DEVICEs(21)), 0: PRINT "LN 1 ";
        LOCATE 5, 12: COLOR 9, 0: PRINT "o "; : COLOR clrr(DEVICEs(22)), 0: PRINT "LN 2 ";
        LOCATE 6, 12: COLOR 9, 0: PRINT "p "; : COLOR clrr(DEVICEs(23)), 0: PRINT "LN 3 ";
        LOCATE 7, 12: COLOR 9, 0: PRINT "q "; : COLOR clrr(DEVICEs(34)), 0: PRINT "Einj1";
        LOCATE 8, 12: COLOR 9, 0: PRINT "r "; : COLOR clrr(DEVICEs(35)), 0: PRINT "Einj2";
        LOCATE 9, 12: COLOR 9, 0: PRINT "s "; : COLOR clrr(DEVICEs(36)), 0: PRINT "DUMP ";
        LOCATE 10, 12: COLOR 9, 0: PRINT "t "; : COLOR clrr(DEVICEs(37)), 0: PRINT "LOAD ";
        LOCATE 11, 12: COLOR 9, 0: PRINT "u "; : COLOR clrr(DEVICEs(38)), 0: PRINT "Rinj1";
        LOCATE 12, 12: COLOR 9, 0: PRINT "v "; : COLOR clrr(DEVICEs(39)), 0: PRINT "Rinj2";
        LOCATE 13, 12: COLOR 9, 0: PRINT "w "; : COLOR clrr(DEVICEs(40)), 0: PRINT "FCinj";
        LOCATE 14, 12: COLOR 9, 0: PRINT "x "; : COLOR clrr(DEVICEs(55)), 0: PRINT "NAV  ";
        LOCATE 2, 24: COLOR 9, 0: PRINT "y "; : COLOR clrr(DEVICEs(27)), 0: PRINT "Acon1";
        LOCATE 3, 24: COLOR 9, 0: PRINT "z "; : COLOR clrr(DEVICEs(28)), 0: PRINT "Acon2";
        LOCATE 4, 24: COLOR 9, 0: PRINT "1 "; : COLOR clrr(DEVICEs(29)), 0: PRINT "GPD1 ";
        LOCATE 5, 24: COLOR 9, 0: PRINT "2 "; : COLOR clrr(DEVICEs(30)), 0: PRINT "GPD2 ";
        LOCATE 6, 24: COLOR 9, 0: PRINT "3 "; : COLOR clrr(DEVICEs(31)), 0: PRINT "GPD3 ";
        LOCATE 7, 24: COLOR 9, 0: PRINT "4 "; : COLOR clrr(DEVICEs(32)), 0: PRINT "GPD4 ";
        LOCATE 8, 24: COLOR 9, 0: PRINT "5 "; : COLOR clrr(DEVICEs(33)), 0: PRINT "TTC  ";
        LOCATE 9, 24: COLOR 9, 0: PRINT "6 "; : COLOR clrr(DEVICEs(41)), 0: PRINT "DUMP ";
        LOCATE 10, 24: COLOR 9, 0: PRINT "7 "; : COLOR clrr(DEVICEs(42)), 0: PRINT "LOAD ";
        LOCATE 11, 24: COLOR 9, 0: PRINT "8 "; : COLOR clrr(DEVICEs(43)), 0: PRINT "Rinj1";
        LOCATE 12, 24: COLOR 9, 0: PRINT "9 "; : COLOR clrr(DEVICEs(44)), 0: PRINT "Rinj2";
        LOCATE 16, 1: COLOR 11, 0: PRINT "Wind Accel:"; : COLOR 9, 0: PRINT " []";
        LOCATE 17, 1: COLOR 11, 0: PRINT "     accel:"; : COLOR 9, 0: PRINT " {}"; : COLOR 11, 0: LOCATE 17, 26: PRINT "m/s"; CHR$(253);
        LOCATE 18, 1: COLOR 11, 0: PRINT "     angle:"; : COLOR 9, 0: PRINT " \";
        COLOR 14, 0
        LOCATE 16, 17: IF WINDref = -1 THEN PRINT "     OFF";  ELSE PRINT nme$(WINDref);
        LOCATE 17, 19: PRINT USING "####.#"; windACC;
        LOCATE 18, 20
        IF WINDangle = 0 THEN PRINT "   IN";
        IF WINDangle = 180 THEN PRINT "  OUT";
        IF WINDangle = 90 THEN PRINT "RIGHT";
        IF WINDangle = 270 THEN PRINT " LEFT";
        LOCATE 11, 55: PRINT "-"; : PRINT USING "##.##"; HABfuelleak; : COLOR 9: PRINT " ,."; : COLOR 14, 0
        LOCATE 12, 55: PRINT "-"; : PRINT USING "##.##"; AYSEfuelleak; : COLOR 9: PRINT " <>";
        COLOR 9, 0
        LOCATE 25, 1
        IF ufoFLAG = 0 THEN PRINT "arrows + -";  ELSE PRINT "          ";
        LOCATE 16, 35: COLOR 9, 0: PRINT "F10"; : COLOR 11, 0: PRINT " Clear Codes";
        LOCATE 17, 35: COLOR 9, 0: PRINT "F11"; : COLOR 11, 0: PRINT " Power Grid ";
        LOCATE 18, 35: COLOR 9, 0: PRINT "F12"; : COLOR 11, 0: PRINT " Telemetry  ";

        COLOR 11, 0
        IF ufoFLAG = 0 THEN 6
        COLOR 11
        LOCATE 17, 70: PRINT "UFO:";
        LOCATE 18, 70: PRINT "REF:";
        LOCATE 19, 70: PRINT "TRG:";
        LOCATE 20, 70: PRINT "ALT:";
        LOCATE 22, 70: PRINT "ANG:";
        LOCATE 24, 70: PRINT "ACC:";
        LOCATE 25, 70: PRINT "MSL:";
        IF VUFO = 1 THEN COLOR 10, 1 ELSE COLOR 10, 0
        LOCATE 17, 77: IF Zufo = 2 THEN PRINT "OFF";  ELSE PRINT " ON";
        IF VUFO = 2 THEN COLOR 10, 1 ELSE COLOR 10, 0
        zz$ = nme$(Zref)
        FOR j = 1 TO 8
         IF MID$(zz$, j, 1) <> " " THEN zz$ = MID$(zz$, j, 6): GOTO 57
        NEXT j
57      IF LEN(zz$) < 6 THEN zz$ = zz$ + SPACE$(6 - LEN(zz$))
        LOCATE 18, 74: PRINT zz$;
        IF VUFO = 3 THEN COLOR 10, 1 ELSE COLOR 10, 0
        zz$ = nme$(Ztrg)
        FOR j = 1 TO 8
         IF MID$(zz$, j, 1) <> " " THEN zz$ = MID$(zz$, j, 6): GOTO 58
        NEXT j
58      IF LEN(zz$) < 6 THEN zz$ = zz$ + SPACE$(6 - LEN(zz$))
        LOCATE 19, 74: PRINT zz$;
        COLOR 10
        LOCATE 21, 74: PRINT USING "####.#"; ZtrgAngle * RAD;
        IF VUFO = 4 THEN COLOR 10, 1 ELSE COLOR 10, 0
        LOCATE 20, 74: PRINT USING "######"; Zrefalt / 1000;
        IF VUFO = 6 THEN COLOR 10, 1 ELSE COLOR 10, 0
        LOCATE 23, 72: PRINT nme$(Zorient + 40);
        IF VUFO = 5 THEN COLOR 10, 1 ELSE COLOR 10, 0
        LOCATE 22, 74: PRINT USING "####.#"; Zangle * RAD;
        IF VUFO = 7 THEN COLOR 10, 1 ELSE COLOR 10, 0
        LOCATE 24, 74: PRINT USING "######"; Zaccel;
        IF VUFO = 8 THEN COLOR 10, 1 ELSE COLOR 10, 0
        zz$ = nme$(Ztarg)
        FOR j = 1 TO 8
         IF MID$(zz$, j, 1) <> " " THEN zz$ = MID$(zz$, j, 6): GOTO 59
        NEXT j
59      IF LEN(zz$) < 6 THEN zz$ = zz$ + SPACE$(6 - LEN(zz$))
        LOCATE 25, 74
        IF Ztarg = 39 THEN PRINT "ABORT ";
        IF Ztarg = 38 THEN PRINT "OFF   ";
        IF Ztarg < 38 THEN PRINT zz$;
       
       
6       COLOR 7, 0
        IF TIMER - tt >= 5 THEN tt = TIMER: GOSUB 800: GOSUB 830: GOSUB 300: GOSUB 400: GOSUB 900
        GOSUB 3000
        GOTO 1


        'SUBROUTINE: Save error codes to code file
100     OPEN "O", #1, zpath$ + "orb5rEs.txt"
        FOR i = 1 TO 5
         PRINT #1, SOURCEs(i, 1)
         PRINT #1, SOURCEs(i, 2)
         PRINT #1, SOURCEs(i, 0)
        NEXT i
        FOR i = 1 TO 55
         IF i < 34 THEN PRINT #1, SGN(DEVICEs(i))
         IF i > 44 AND i < 55 THEN PRINT #1, SGN(DEVICEs(i))
         IF i = 55 THEN PRINT #1, DEVICEs(i)
         IF i > 33 AND i < 45 THEN PRINT #1, DEVICEs(i)
        NEXT i
        FOR i = 1 TO 56
         IF DEVICEs(i) = 2 THEN PRINT #1, 1 ELSE PRINT #1, 0
        NEXT i
        FOR i = 1 TO 4
         PRINT #1, 25 - SOURCEs(i, 3)
        NEXT i
        PRINT #1, HABfuelleak
        PRINT #1, AYSEfuelleak
        IF WINDref > -1 THEN PRINT #1, WINDref ELSE PRINT #1, 0
        PRINT #1, windACC
        PRINT #1, WINDangle / RAD
        PRINT #1, Zufo
        PRINT #1, Zref
        PRINT #1, Zrefalt
        PRINT #1, Zangle
        PRINT #1, Zorient
        PRINT #1, Zaccel
        PRINT #1, Ztarg
        PRINT #1, master
        CLOSE #1
        SOUND 10000, 1
        COLOR 10, 1
        LOCATE 1, 24: PRINT "UPDATED";
        COLOR 7, 0
110     RETURN


        'SUBROUTINE: Upload existing error codes from code file
120     OPEN "I", #1, zpath$ + "orb5rEs.txt"
        FOR i = 1 TO 5
         INPUT #1, SOURCEs(i, 1)
         INPUT #1, SOURCEs(i, 2)
         INPUT #1, SOURCEs(i, 0)
        NEXT i
        FOR i = 1 TO 55
         INPUT #1, DEVICEs(i)
        NEXT i
        FOR i = 1 TO 56
         INPUT #1, z
         IF z = 1 THEN DEVICEs(i) = 2
        NEXT i
        FOR i = 1 TO 4
         INPUT #1, SOURCEs(i, 3): SOURCEs(i, 3) = 25 - SOURCEs(i, 3)
        NEXT i
        INPUT #1, HABfuelleak
        INPUT #1, AYSEfuelleak
        INPUT #1, WINDref
        IF WINDref = 0 THEN WINDref = -1
        INPUT #1, windACC
        INPUT #1, WINDangle
        WINDangle = WINDangle * RAD
        INPUT #1, Zufo
        INPUT #1, Zref
        INPUT #1, Zrefalt
        INPUT #1, Zangle
        INPUT #1, Zorient
        INPUT #1, Zaccel
        INPUT #1, Ztarg
        INPUT #1, master
        CLOSE #1
        SOUND 10000, 1
        COLOR 10, 0
        LOCATE 1, 24: PRINT "UPDATED";
        COLOR 7, 0
130     RETURN


        'SUBROUTINE: Clear error codes
140     FOR i = 1 TO 5
         SOURCEs(i, 1) = 0
         SOURCEs(i, 2) = 0
         SOURCEs(i, 0) = 0
        NEXT i
        FOR i = 1 TO 55
         DEVICEs(i) = 0
        NEXT i
        FOR i = 1 TO 4
        SOURCEs(i, 3) = 0
        NEXT i
        HABfuelleak = 0
        AYSEfuelleak = 0
        WINDref = -1
        windACC = 0
        WINDangle = 0
        Zufo = 0
        Zref = 0
        Zrefalt = 0
        Zangle = 0
        Zorient = 0
        Zaccel = 0
        Ztarg = 0
        master = 0
        GOSUB 400
        RETURN


        'SUBROUTINE: Calculate orbit parameters
        'Calculate gravitaional attraction to reference object
300     difx = Px(ref, 3) - Px(28, 3)
        dify = Py(ref, 3) - Py(28, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        A = G * P(ref, 4) / (R ^ 2)
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Vref = SQR(G * P(ref, 4) / R)
        Aref = angle
        Dref = R
        ACCref = A
        
        IF Zref = Ztrg THEN ZtrgAngle = 0: GOTO 311
        difx = Px(Zref, 3) - Px(Ztrg, 3)
        dify = Py(Zref, 3) - Py(Ztrg, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        A = G * P(ref, 4) / (R ^ 2)
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        ZtrgAngle = angle


        'Calculate gravitational attraction to target object
311     difx = Px(targ, 3) - Px(28, 3)
        dify = Py(targ, 3) - Py(28, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        A = G * P(targ, 4) / (R ^ 2)
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Vtarg = SQR(G * P(targ, 4) / R)
        Atarg = angle
        Dtarg = R
        Acctarg = A
        VVx = Vx(targ) - Vx(28)
        VVy = Vy(targ) - Vy(28)
        IF VVy = 0 THEN IF VVx < 0 THEN Vvangle = .5 * 3.1415926535# ELSE Vvangle = 1.5 * 3.1415926535# ELSE Vvangle = ATN(VVx / VVy)
        IF VVy > 0 THEN Vvangle = Vvangle + 3.1415926535#
        IF Vvangle < 0 THEN Vvangle = Vvangle + 6.283185
        VangleDIFF = Atarg - Vvangle
        AtoTARG = Acctarg
        IF COS(VangleDIFF) <> 0 AND Dtarg - P(targ, 5) <> 0 THEN AtoTARG = AtoTARG + ((((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2) / (2 * (Dtarg - P(targ, 5)))) * COS(VangleDIFF))



        ' Calculate atmospheric effects
        FOR i = 1 TO 34
         IF i = 28 THEN 321
         IF i = 32 THEN 321
         dist = SQR((Px(i, 3) - Px(28, 3)) ^ 2 + (Py(i, 3) - Py(28, 3)) ^ 2)
         IF dist <= P(i, 5) + (1000 * P(i, 10)) THEN atm = i: Ratm = (dist - P(i, 5)) / 1000
321     NEXT i
        IF atm = 0 THEN 322
        VVrx = Vx(atm) - Vx(28)
        VVry = Vy(atm) - Vy(28)
        IF VVry = 0 THEN IF VVrx < 0 THEN VvRangle = .5 * 3.1415926535# ELSE VvRangle = 1.5 * 3.1415926535# ELSE VvRangle = ATN(VVrx / VVry)
        IF VVry > 0 THEN VvRangle = VvRangle + 3.1415926535#
        IF VvRangle < 0 THEN VvRangle = VvRangle + 6.283185
        VVr = SQR((VVrx ^ 2) + (VVry ^ 2))
        IF Ratm < 0 THEN Pr = P(atm, 8) ELSE Pr = P(atm, 8) * (2.71828 ^ (-1 * Ratm / P(atm, 9)))
        Are = Pr * VVr * VVr * .0002
        IF Are * ts > VVr / 2 THEN Are = (VVr / 2) / ts
322     DelVvRangle = VvRangle - Sangle
        



       
        'Calculate gravitational attraction to UFO
        IF UFOdisp = 0 THEN 301
        IF UFOdisp = 1 THEN zr1 = 38: zr2 = Zref
        IF UFOdisp = 2 THEN zr1 = 38: zr2 = Ztrg
        IF UFOdisp = 3 THEN zr1 = Ztrg: zr2 = Zref
        difx = Px(zr1, 3) - Px(zr2, 3)
        dify = Py(zr1, 3) - Py(zr2, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF R = 0 THEN A = 0 ELSE A = G * P(zr2, 4) / (R ^ 2)
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        IF R = 0 THEN VrefUFO = 0 ELSE VrefUFO = SQR(G * P(zr2, 4) / R)
        Aufo = angle
        Dufo = R
        AccUFO = A
        VVx = Vx(zr2) - Vx(zr1)
        VVy = Vy(zr2) - Vy(zr1)
        IF VVy = 0 THEN IF VVx < 0 THEN Vvangle = .5 * 3.1415926535# ELSE Vvangle = 1.5 * 3.1415926535# ELSE Vvangle = ATN(VVx / VVy)
        IF VVy > 0 THEN Vvangle = Vvangle + 3.1415926535#
        IF Vvangle < 0 THEN Vvangle = Vvangle + 6.283185
        VangleDIFFufo = Aufo - Vvangle
        IF COS(VangleDIFFufo) <> 0 AND Dufo - P(zr2, 5) <> 0 THEN AccUFO = AccUFO + ((((Vx(zr2) - Vx(zr1)) ^ 2 + (Vy(zr2) - Vy(zr1)) ^ 2) / (2 * (Dufo - P(zr2, 5)))) * COS(VangleDIFFufo))
       

       
301     VVx = 0 - Vx(28)
        VVy = 0 - Vy(28)
        IF VVy = 0 THEN IF VVx < 0 THEN Vvangle = .5 * 3.1415926535# ELSE Vvangle = 1.5 * 3.1415926535# ELSE Vvangle = ATN(VVx / VVy)
        IF VVy > 0 THEN Vvangle = Vvangle + 3.1415926535#
        IF Vvangle < 0 THEN Vvangle = Vvangle + 6.283185


        'Calculate reference velocities
        Vhab = SQR(Vx(28) ^ 2 + Vy(28) ^ 2)
        Vrefhab = SQR((Vx(28) - Vx(ref)) ^ 2 + (Vy(28) - Vy(ref)) ^ 2)
        Vtarghab = SQR((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2)
        VcenTARG = SQR(((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2)) * -1 * COS(VangleDIFF)
        VtanTARG = SQR(((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2)) * ABS(SIN(VangleDIFF))
       
        IF UFOdisp > 0 THEN Dtarg = Dufo: Vtarghab = VrefUFO: Atarg = Aufo: AtoTARG = AccUFO
        IF UFOdisp > 0 THEN VcenTARG = SQR(((Vx(zr1) - Vx(zr2)) ^ 2 + (Vy(zr1) - Vy(zr2)) ^ 2)) * -1 * COS(VangleDIFFufo)
        IF UFOdisp > 0 THEN VtanTARG = SQR(((Vx(zr1) - Vx(zr2)) ^ 2 + (Vy(zr1) - Vy(zr2)) ^ 2)) * ABS(SIN(VangleDIFFufo))
               
        IF Sangle < 0 THEN Sangle = 6.2831852# + Sangle
        RETURN



        'SUBROUTINE: Write telemetry data to screen
400     COLOR 11: LOCATE 1, 35: PRINT "               REFERENCE      ";
        IF UFOdisp = 0 THEN PRINT "    TARGET";
        IF UFOdisp = 1 THEN PRINT "UFO to REF";
        IF UFOdisp = 2 THEN PRINT "UFO to TRG";
        IF UFOdisp = 3 THEN PRINT "TRG to REF";
        LOCATE 2, 35: PRINT "                "; nme$(ref); "        ";
        IF UFOdisp = 0 THEN PRINT nme$(targ);  ELSE PRINT nme$(zr2);
        COLOR 11: LOCATE 3, 35: PRINT "DISTANCE: ";
        COLOR 15
        IF UFOdisp = 0 THEN Dtarg = Dtarg - P(targ, 5) - P(28, 5) ELSE Dtarg = Dtarg - P(zr2, 5) - P(zr1, 5)
        IF Dref > 99999999999# THEN PRINT USING "##.#######^^^^"; Dref / 1000;  ELSE PRINT USING "###,###,###.##"; (Dref - P(ref, 5) - P(28, 5)) / 1000;
        IF Dtarg > 99999999999# THEN PRINT USING "  ##.#######^^^^"; Dtarg / 1000;  ELSE PRINT USING "#####,###,###.##"; Dtarg / 1000;
        COLOR 11: LOCATE 4, 35: PRINT "VELOCITY: ";
        COLOR 15
        IF ABS(Vrefhab) > 99999999999# THEN PRINT USING "##.#######^^^^"; Vrefhab / 1000;  ELSE PRINT USING "###,###,###.##"; Vrefhab / 1000;
        IF ABS(Vtarghab) > 99999999999# THEN PRINT USING "  ##.#######^^^^"; Vtarghab / 1000;  ELSE PRINT USING "#####,###,###.##"; Vtarghab / 1000;
        COLOR 11: LOCATE 5, 35: PRINT "  CEN:                  ";
        COLOR 15
        IF ABS(VcenTARG) > 99999999999# THEN PRINT USING "  ##.#######^^^^"; VcenTARG / 1000;  ELSE PRINT USING "#####,###,###.##"; VcenTARG / 1000;
        COLOR 11: LOCATE 6, 35: PRINT "  TAN:                  ";
        COLOR 15
        IF ABS(VtanTARG) > 99999999999# THEN PRINT USING "  ##.#######^^^^"; VtanTARG / 1000;  ELSE PRINT USING "#####,###,###.##"; VtanTARG / 1000;
        COLOR 11: LOCATE 7, 35: PRINT "ANGLE:    ";
        COLOR 15
        PRINT USING "###,###,###.##"; Aref * RAD;
        PRINT USING "#####,###,###.##"; Atarg * RAD;
        COLOR 11: LOCATE 8, 35: PRINT "STOP ACC:               ";
        COLOR 15
        IF ABS(AtoTARG) > 999999999# THEN PRINT USING "  ##.#######^^^^"; AtoTARG;  ELSE PRINT USING "#####,###,###.##"; AtoTARG;
              
        IF AYSE = 150 THEN thrust = 64000000#: Hmass = fuel + AYSEfuel + 20275000 ELSE thrust = 175000: Hmass = fuel + 275000
        Haccel = (thrust * eng) / Hmass
        COLOR 11: LOCATE 9, 35: PRINT "ENGINE:   "; : COLOR 15: PRINT USING "####.###"; eng;
        COLOR 11: LOCATE 10, 35: PRINT "ACCEL:    "; : COLOR 15: PRINT USING "####.###"; Haccel;
        COLOR 11: LOCATE 11, 35: PRINT "H fuel:   "; : COLOR 15: PRINT USING "########"; fuel;
        COLOR 11: LOCATE 12, 35: PRINT "A fuel:   "; : COLOR 15: PRINT USING "########"; AYSEfuel;
        COLOR 11: LOCATE 9, 60: PRINT "HEADING:"; : COLOR 15: PRINT USING "####.##"; Sangle * RAD;
        COLOR 11: LOCATE 10, 61: PRINT "COURSE:"; : COLOR 15: PRINT USING "####.##"; Vvangle * RAD;
        COLOR 11: LOCATE 11, 69: PRINT "ATM: "; : COLOR 15: IF Are < 9999.99 THEN PRINT USING "####.##"; Are;  ELSE PRINT USING "####.##"; 9999.99;
        COLOR 11: LOCATE 12, 67: PRINT "V"; CHR$(237); "ATM: "; : COLOR 15: PRINT USING "####.##"; DelVvRangle * RAD;
       
        COLOR 11: LOCATE 13, 35: PRINT "NAV MODE: ";
        COLOR 15
          IF Sflag = 0 THEN PRINT "ccw prograde  "; : GOTO 401
          IF Sflag = 4 THEN PRINT "ccw retrograde"; : GOTO 401
          IF Sflag = 1 THEN PRINT "manual        "; : GOTO 401
          IF Sflag = 2 THEN PRINT "approach targ "; : GOTO 401
          IF Sflag = 5 THEN PRINT "pro Vtrg      "; : GOTO 401
          IF Sflag = 6 THEN PRINT "retr Vtrg     "; : GOTO 401
          IF Sflag = 3 THEN PRINT "depart ref    ";
401     COLOR 11: LOCATE 14, 35: PRINT "TIME:     "; : COLOR 15: PRINT USING "####"; year; : LOCATE 14, 51: PRINT USING "###"; day; hr; min; sec;
        COLOR 11: LOCATE 15, 35: PRINT "F rate:     "; : COLOR 15: PRINT USING "##.##"; ts;
       
        COLOR 14, 0
        LOCATE 16, 17: IF WINDref = -1 THEN PRINT "     OFF";  ELSE PRINT nme$(WINDref);
        LOCATE 17, 19: PRINT USING "####.#"; windACC;
        LOCATE 18, 20
        IF WINDangle = 0 THEN PRINT "   IN";
        IF WINDangle = 180 THEN PRINT "  OUT";
        IF WINDangle = 90 THEN PRINT "RIGHT";
        IF WINDangle = 270 THEN PRINT " LEFT";
       
        COLOR 11
        'LOCATE 18, 1: PRINT "AYSE DOCKING    HAB DOCKING";
        'LOCATE 17, 35: PRINT "HAB DRIVE        AYSE DRIVE";
        'COLOR 15
        'LOCATE 19, 17: COLOR 12 - (AG * 2):   PRINT "Inertial Damper"; : COLOR 15
        'LOCATE 20, 17: COLOR 12 - (AG * 2):   PRINT "Artificial Grav"; : COLOR 15
        'LOCATE 21, 17: COLOR 12 - (DH * 2):   PRINT "Dck Hatch"; : COLOR 15
        'LOCATE 22, 17: COLOR 12 - (DHL * 2):  PRINT "Dck Htch Lck"; : COLOR 15
        'LOCATE 23, 17: COLOR 12 - (DHLA * 2): PRINT "Dck Htch Lk-ARM"; : COLOR 15
        'LOCATE 19, 1: COLOR 12 - (AYSEad * 2):  PRINT "Auto Docking  "; : COLOR 15
        'LOCATE 20, 1: COLOR 12 - (AYSEdl * 2):  PRINT "Docking Latch "; : COLOR 15
        'LOCATE 21, 1: COLOR 12 - (AYSEdla * 2): PRINT "Dck Latch ARM "; : COLOR 15
        'LOCATE 22, 1: COLOR 12 - (AYSEtr * 2):  PRINT "Thermal Roll  "; : COLOR 15
        'LOCATE 18, 52: COLOR 9 + TTC: PRINT "TTC             ";
        'LOCATE 19, 52: COLOR 9 + GPD: PRINT "GPDs            ";
        'LOCATE 20, 52: COLOR 9 + Agen: PRINT "Generator Voltge";
        'LOCATE 21, 52: COLOR 9 + Acon1: PRINT "React Confnmt 1 ";
        'LOCATE 22, 52: COLOR 9 + Acon2: PRINT "React Confnmt 2 ";
        'LOCATE 23, 52: COLOR 9 + Abr: PRINT "System Breakers ";
        'LOCATE 24, 52: COLOR 9 + AT: PRINT "System Temp    ";
        'COLOR 9 + Fcon1: LOCATE 18, 35: PRINT "React Confnmt 1 ";
        'COLOR 9 + Fcon2: LOCATE 19, 35: PRINT "React Confnmt 2 ";
        'COLOR 9 + ion: LOCATE 20, 35: PRINT "Ion Voltage     ";
        'COLOR 9 + acc: LOCATE 21, 35: PRINT "Accel Voltage   ";
        'COLOR 9 + Hbr: LOCATE 22, 35: PRINT "System Breakers ";
        'COLOR 9 + HT: LOCATE 23, 35: PRINT "System Temp     ";
        'COLOR 10 + (NAVfailure * 2): LOCATE 24, 35: PRINT "NAV systems     ";
        'COLOR clr(ENGfailure * 4): LOCATE 25, 35: PRINT "ENGINE systems  ";
        'COLOR 15
        RETURN





        'SUBROUTINE: Timed telemetry retrieval
800     OPEN "I", #1, zpath$ + "OSbackup"
801     IF EOF(1) THEN 803
        INPUT #1, z$
        IF z$ <> "ORBIT5S" THEN LOCATE 11, 60: PRINT filename$; " is unusable"; : CLOSE #1: z$ = INPUT$(1): END
        FOR i = 1 TO 32
         IF EOF(1) THEN 803
         INPUT #1, Ztel(i)
        NEXT i
        eng = Ztel(1)
        vflag = Ztel(2)
        Aflag = Ztel(3)
        Sflag = Ztel(4)
        SYSflag = Ztel(5)
        'mag = Ztel(6)
        Sangle = Ztel(7)
        'cen = Ztel(8)
        targ = Ztel(9)
        ref = Ztel(10)
        'trail = Ztel(11)
        'tr = Ztel(12)
        dte = Ztel(13)
        ts = Ztel(14)
        Eflag = Ztel(15)
        year = Ztel(16)
        day = Ztel(17)
        hr = Ztel(18)
        min = Ztel(19)
        sec = Ztel(20)
        explosionHAB = Ztel(31)
        explosionAYSE = Ztel(32)
        Px(0, 3) = 0
        Py(0, 3) = 0
        Vx(0) = 0
        Vy(0) = 0
        P(0, 1) = 0
        P(0, 2) = Ztel(6)
        FOR i = 0 TO 39
         FOR j = 1 TO 6
          IF EOF(1) THEN 803
          INPUT #1, Ztel(j)
         NEXT j
         IF i = 0 THEN NAVfailure = Ztel(1) ELSE Px(i, 3) = Ztel(1)
         IF i = 0 THEN ENGfailure = Ztel(2) ELSE Py(i, 3) = Ztel(2)
         Vx(i) = Ztel(3)
         Vy(i) = Ztel(4)
         P(i, 1) = Ztel(5)
         P(i, 2) = Ztel(6)
        NEXT i
        INPUT #1, fuel
        INPUT #1, AYSEfuel
        Px(37, 3) = 4446370.8284487# + Px(3, 3): Py(37, 3) = 4446370.8284487# + Py(3, 3): Vx(37) = Vx(3): Vy(37) = Vy(3)
802     CLOSE #1
        IF explosionHAB = 1 THEN GOSUB 7000
        IF explosionAYSE = 1 THEN GOSUB 7100
        RETURN
803     CLOSE #1
        GOTO 800


830     OPEN "I", #1, zpath$ + "ORBITSse"
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, z
        INPUT #1, Htemp
        INPUT #1, Atemp
        INPUT #1, FCtemp
        INPUT #1, BATT1temp
        INPUT #1, BATT3temp
        INPUT #1, Is1
        INPUT #1, Is2
        INPUT #1, Is3
        INPUT #1, Is4
        INPUT #1, Is5
        INPUT #1, Vbus1
        INPUT #1, Vbus2
        INPUT #1, Vbus3
        INPUT #1, Vbus4
        INPUT #1, Vbus5
        INPUT #1, Ibus1
        INPUT #1, Ibus2
        INPUT #1, Ibus3
        INPUT #1, Ibus4
        INPUT #1, Ibus5
        CLOSE #1
        RETURN

900     destructCODE$ = ""
        OPEN "I", #2, zpath$ + "ORBITMC"
        IF EOF(2) THEN 901
        INPUT #2, destructCODE$
901     CLOSE #2
        IF destructCODE$ = "*$&##@@#@" THEN SOURCEs(1, 3) = 31: SOURCEs(1, 0) = 0: SOURCEs(2, 0) = 0: SOURCEs(3, 0) = 0: SOURCEs(4, 0) = 0: SOURCEs(5, 0) = 0: GOSUB 100
        RETURN



1000    DATA "     Sun"
        DATA " Mercury"
        DATA "   Venus"
        DATA "   Earth"
        DATA "    Mars"
        DATA " Jupiter"
        DATA "  Saturn"
        DATA "  Uranus"
        DATA " Neptune"
        DATA "   Pluto"
        DATA "    Moon"
        DATA "  Phobos"
        DATA "  Deimos"
        DATA "      Io"
        DATA "  Europa"
        DATA "Ganymede"
        DATA "Callisto"
        DATA "  Tethys"
        DATA "   Dione"
        DATA "    Rhea"
        DATA "   Titan"
        DATA " Iapatus"
        DATA "   Ariel"
        DATA " Umbriel"
        DATA " Titania"
        DATA "  Oberon"
        DATA "  Triton"
        DATA "  Charon"
        DATA " Habitat"
        DATA "     Ida"
        DATA "Borrelly"
        DATA "   Vesta"
        DATA "    AYSE"
        DATA "   Sedna"
        DATA "  Quaoar"
        DATA "     ISS"
        DATA "  MODULE"
        DATA "   OCESS"
        DATA "    UFO1"
        DATA "    UFO2"
        DATA "  progrd"
        DATA "retrogrd"
        DATA " deptarg"
        DATA " apptarg"


2000    IF Zufo = 1 THEN Zufo = 2 ELSE Zufo = 1
        RETURN
2010    Zref = Zref + 1
        IF Zref = 36 THEN Zref = 0
        RETURN
2020    Zrefalt = Zrefalt + 10000
        IF Zrefalt > 9000000 THEN Zrefalt = 0
        RETURN
2030    Zorient = Zorient + 1
        IF Zorient = 4 THEN Zorient = 0
        RETURN
2040    Zaccel = Zaccel + 1
        IF Zaccel > 500 THEN Zaccel = 0
        RETURN
2050    Ztarg = Ztarg + 1
        IF Ztarg = 40 THEN Ztarg = 0
        RETURN
2060    Zangle = Zangle + .0087266
        IF Zangle > 6.2831853# THEN Zangle = 0
        RETURN
2070    Ztrg = Ztrg + 1
        IF Ztrg = 36 THEN Ztrg = 0
        RETURN

2110    Zref = Zref - 1
        IF Zref < 0 THEN Zref = 35
        RETURN
2120    Zrefalt = Zrefalt - 10000
        IF Zrefalt < 0 THEN Zrefalt = 9000000
        RETURN
2130    Zorient = Zorient - 1
        IF Zorient < 0 THEN Zorient = 3
        RETURN
2140    Zaccel = Zaccel - 1
        IF Zaccel < 0 THEN Zaccel = 500
        RETURN
2150    Ztarg = Ztarg - 1
        IF Ztarg < 0 THEN Ztarg = 39
        RETURN
2160    Zangle = Zangle - .0087266
        IF Zangle < 0 THEN Zangle = 2 * 3.1415926535#
        RETURN
2170    Ztrg = Ztrg - 1
        IF Ztrg < 0 THEN Ztrg = 35
        RETURN

3000    COLOR 11, 0: LOCATE 20, 1: PRINT "BUS:"; : COLOR 14, 0: PRINT " VOLTS";
        LOCATE 21, 1: COLOR 11, 0: PRINT "  1:"; : COLOR 10, 0: PRINT USING "######"; Vbus1;
        LOCATE 22, 1: COLOR 11, 0: PRINT "  2:"; : COLOR 10, 0: PRINT USING "######"; Vbus2;
        LOCATE 23, 1: COLOR 11, 0: PRINT "  3:"; : COLOR 10, 0: PRINT USING "######"; Vbus3;
        LOCATE 24, 1: COLOR 11, 0: PRINT "  4:"; : COLOR 10, 0: PRINT USING "######"; Vbus4;
        COLOR 14, 0: LOCATE 20, 13: PRINT "CURRENT  SHORT";
        LOCATE 21, 13: COLOR 10, 0: PRINT USING "#######"; Ibus1;
        LOCATE 22, 13: COLOR 10, 0: PRINT USING "#######"; Ibus2;
        LOCATE 23, 13: COLOR 10, 0: PRINT USING "#######"; Ibus3;
        LOCATE 24, 13: COLOR 10, 0: PRINT USING "#######"; Ibus4;
        LOCATE 20, 30: COLOR 11, 0: PRINT "SOURCE:"; : COLOR 14, 0: LOCATE 20, 38: PRINT "OUTPUT";
        LOCATE 21, 31: COLOR 11, 0: PRINT "H Rct:"; : COLOR 10, 0: PRINT USING "#######"; Is1;
        LOCATE 22, 31: COLOR 11, 0: PRINT "   FC:"; : COLOR 10, 0: PRINT USING "#######"; Is2;
        LOCATE 23, 31: COLOR 11, 0: PRINT " BAT1:"; : COLOR 10, 0: PRINT USING "#######"; Is3;
        LOCATE 25, 31: COLOR 11, 0: PRINT "A Rct:"; : COLOR 10, 0: PRINT USING "#######"; Is5;
        LOCATE 24, 31: COLOR 11, 0: PRINT " BAT3:"; : COLOR 10, 0: PRINT USING "#######"; Is4;
        COLOR 14, 0: LOCATE 20, 45: PRINT "TEMP  VOLTS  MALFC  FIX";
        LOCATE 21, 45:  COLOR 10, 0: PRINT USING "####"; Htemp;
        LOCATE 22, 45:  COLOR 10, 0: PRINT USING "####"; FCtemp;
        LOCATE 23, 45:  COLOR 10, 0: PRINT USING "####"; BATT1temp;
        LOCATE 25, 45:  COLOR 10, 0: PRINT USING "####"; Atemp;
        LOCATE 24, 45:  COLOR 10, 0: PRINT USING "####"; BATT3temp;
        FOR i = 1 TO 5
         IF SOURCEs(0, 1) = i AND SOURCEs(0, 2) = 1 THEN Bclr = 1 ELSE Bclr = 0
         IF SOURCEs(i, 1) <> 0 THEN COLOR 12, Bclr ELSE COLOR 10, Bclr * ABS(1 - ufoFLAG)
         LOCATE 20 + i, 50: PRINT USING "######"; SOURCEs(i, 1) + SOURCEs(i, 4);
        
         IF SOURCEs(0, 1) = i AND SOURCEs(0, 2) = 2 THEN Bclr = 1 ELSE Bclr = 0
         IF SOURCEs(i, 2) <> 0 THEN COLOR 12, Bclr ELSE COLOR 10, Bclr * ABS(1 - ufoFLAG)
         LOCATE 20 + i, 58: PRINT USING "#####"; SOURCEs(i, 2);
        
         IF SOURCEs(0, 1) = i AND SOURCEs(0, 2) = 3 THEN Bclr = 1 ELSE Bclr = 0
         IF SOURCEs(i, 0) <> 0 THEN COLOR 10, Bclr ELSE COLOR 2, Bclr * ABS(1 - ufoFLAG)
         LOCATE 20 + i, 65: PRINT USING "###"; SOURCEs(i, 0);
        
         IF SOURCEs(0, 1) = i AND SOURCEs(0, 2) = 4 THEN Bclr = 1 ELSE Bclr = 0
         IF SOURCEs(i, 3) <> 0 THEN COLOR 12, Bclr ELSE COLOR 10, Bclr * ABS(1 - ufoFLAG)
         IF i < 5 THEN LOCATE 20 + i, 22: PRINT USING "#####"; SOURCEs(i, 3);
        NEXT i
        RETURN

7000    master = 1
        DEVICEs(1) = 2
        DEVICEs(2) = 2
        DEVICEs(3) = 2
        DEVICEs(6) = 2
        DEVICEs(7) = 2
        DEVICEs(8) = 2
        DEVICEs(9) = 2
        DEVICEs(10) = 2
        DEVICEs(11) = 2
        DEVICEs(12) = 2
        DEVICEs(13) = 2
        DEVICEs(19) = 2
        DEVICEs(20) = 2
        DEVICEs(21) = 2
        DEVICEs(22) = 2
        DEVICEs(23) = 2
        DEVICEs(34) = 2
        DEVICEs(35) = 2
        DEVICEs(36) = 2
        DEVICEs(37) = 2
        DEVICEs(38) = 2
        DEVICEs(39) = 2
        DEVICEs(40) = 2
        DEVICEs(55) = 2
        GOSUB 100
        RETURN

7100    master = 1
        DEVICEs(27) = 2
        DEVICEs(28) = 2
        DEVICEs(29) = 2
        DEVICEs(30) = 2
        DEVICEs(31) = 2
        DEVICEs(32) = 2
        DEVICEs(33) = 2
        DEVICEs(41) = 2
        DEVICEs(42) = 2
        DEVICEs(43) = 2
        DEVICEs(44) = 2
        GOSUB 100
        RETURN

5000    IF ERL = 800 OR ERL = 801 THEN tt = tt + .5: RESUME 803
        IF ERL = 830 THEN tt = tt + .5: : CLOSE #1: RESUME 830
        IF ERL = 900 THEN tt = tt + .5: RESUME 901
        IF ERL = 100 THEN CLOSE #1: RESUME 110
        IF ERL = 98 AND ERR = 76 THEN CLS : LOCATE 15, 5: PRINT "'"; zpath$; "' Path not found"; : RESUME 97
        IF ERL = 98 AND ERR = 53 THEN CLS : LOCATE 15, 5: PRINT "Backup file not found in '"; zpath$; "'"; : RESUME 97
        LOCATE 1, 30: PRINT "unknown error: "; ERR; ERL
        z$ = INPUT$(1)
        END



