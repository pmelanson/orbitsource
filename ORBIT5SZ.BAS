        ON ERROR GOTO 2000
        DIM switch(100, 7), switchlist(255), switchlabel$(30), source(10, 5), BattAH(10), BATT(10), reactor(10), GPD(10), ln(10), RS(10), ion(10), Acc(10), rc(10), dispCHR$(255)
        DIM SOURCEs(6, 3), DEVICEs(55), SWITCHs(56), SHORT(10)
        CLS
        PALETTE 6, 52
        z$ = "H"
        zpath$ = ""
        reload$ = "Y"
        
        CLS
        source(1, 1) = .00025
        source(2, 1) = .00325
        source(3, 1) = .01
        source(5, 1) = .00003
        source(4, 1) = .0055
        FOR i = 1 TO 3
         SHORT(i) = 1E+25
        NEXT i
        IF z$ = "H" THEN RCcap = 2000
        IF z$ = "H" THEN Htemp = 81
        IF z$ = "H" THEN Atemp = 81
        Vbus4 = 100000
        HABfuel# = 100000
        AYSEfuel# = 15000000
        BattAH(1) = 10000
        BattAH(2) = 9999
        BattAH(3) = 99
        FOR i = 0 TO 33
         READ switch(i, 1)
         READ switch(i, 2)
         READ switch(i, 3)
         READ switch(i, 4)
         READ dispCHR$(i)
         switchlist(switch(i, 1)) = i
         IF dispCHR$(i) = "" THEN LOCATE switch(i, 2), switch(i, 3): COLOR 6, 0: PRINT CHR$(switch(i, 1));
         IF dispCHR$(i) <> "" THEN LOCATE switch(i, 2), switch(i, 3) + 1 - LEN(dispCHR$(i)): COLOR 6, 0: PRINT dispCHR$(i);
        NEXT i
        FOR i = 34 TO 55
         READ switch(i, 1)
         READ switch(i, 2)
         READ switch(i, 3)
         READ switchlabel$(i - 33)
         switchlist(switch(i, 1)) = i
         LOCATE switch(i, 2), switch(i, 3): COLOR 6, 0: PRINT CHR$(switch(i, 1));
         LOCATE switch(i, 2), switch(i, 3) + 1: COLOR 9, 0: PRINT switchlabel$(i - 33);
        NEXT i
        i = 56
        READ switch(i, 1)
        READ switch(i, 2)
        READ switch(i, 3)
        READ switch(i, 4)
        READ dispCHR$(i)
        switchlist(switch(i, 1)) = i
        IF dispCHR$(i) = "" THEN LOCATE switch(i, 2), switch(i, 3): COLOR 6, 0: PRINT CHR$(switch(i, 1));
        IF dispCHR$(i) <> "" THEN LOCATE switch(i, 2), switch(i, 3) + 1 - LEN(dispCHR$(i)): COLOR 6, 0: PRINT dispCHR$(i);
        COLOR 6, 0
        LOCATE 22, 54: PRINT "F11";
        LOCATE 24, 54: PRINT "F12";
               
        IF z$ = "H" THEN switch(38, 5) = 1
        IF z$ = "H" THEN switch(39, 5) = 1
        IF z$ = "H" THEN switch(43, 5) = 1
        IF z$ = "H" THEN switch(44, 5) = 1
        switch(27, 5) = 1
        switch(28, 5) = 1
        IF z$ = "H" THEN switch(40, 5) = 1
        IF z$ = "H" THEN switch(18, 5) = 1
        switch(15, 5) = 0
        switch(17, 5) = 1
        IF z$ = "H" THEN switch(14, 5) = 1
        switch(16, 5) = 1
        switch(4, 5) = 1
        IF z$ = "H" THEN switch(20, 5) = 1
        switch(0, 5) = 1
        switch(56, 5) = 1

        FOR i = 1 TO 4
         LOCATE 2, 51 + (6 * i): COLOR 1, 10 + ion(i): PRINT "ION"; : PRINT USING "#"; i;
         LOCATE 4, 50 + (6 * i): COLOR 1, 10 + Acc(i): PRINT "ACC"; : PRINT USING "#"; i;
         LOCATE 3, 53 + (6 * i) + 1: COLOR 14, 0: PRINT CHR$(186);
         LOCATE 4, 53 + (6 * i) + 1: COLOR 14, 0: PRINT CHR$(186);
         LOCATE 5, 53 + (6 * i) + 1: COLOR 14, 0: PRINT CHR$(186);
         LOCATE 5, 50 + (6 * i) + 1: COLOR 14, 0: PRINT CHR$(186);
        NEXT i
        LOCATE 4, 37: COLOR 1, 10 + rc(1): PRINT "R-CON1";
        LOCATE 5, 38: COLOR 14, 0: PRINT CHR$(186);
        LOCATE 4, 44: COLOR 1, 10 + rc(2): PRINT "R-CON2";
        LOCATE 5, 45: COLOR 14, 0: PRINT CHR$(186);
        LOCATE 4, 29: COLOR 1, 10 + AG: PRINT "A GRAV";
        LOCATE 5, 30: COLOR 14, 0: PRINT CHR$(186);
        LOCATE 4, 15: COLOR 1, 10 + RS(1): PRINT "RAD-S1";
        LOCATE 5, 16: COLOR 14, 0: PRINT CHR$(186);
        LOCATE 4, 22: COLOR 1, 10 + RS(2): PRINT "RAD-S2";
        LOCATE 5, 23: COLOR 14, 0: PRINT CHR$(186);
        LOCATE 6, 15: COLOR 1, 10 + HP: PRINT " HAB POWER BUS: "; : PRINT USING "###########"; HPw; : PRINT " W"; SPACE$(36);
        LOCATE 14, 15: COLOR 1, 10 + AP: PRINT "AYSE POWER BUS: "; : PRINT USING "###########"; APw; : PRINT " W"; SPACE$(36);
        LOCATE 10, 25: COLOR 1, 10 + SP: PRINT "2"; CHR$(248); " BUS: "; : PRINT USING "######"; SPw; : PRINT " W         ";
        LOCATE 10, 52: COLOR 1, 10 + TP: PRINT "3"; CHR$(248); " BUS: "; : PRINT USING "####"; SPw; : PRINT " W      ";
        LOCATE 12, 25: COLOR 1, 10 + fcell: PRINT "FC";
        LOCATE 4, 51: PRINT "RCSP";
        
        LOCATE 8, 29: COLOR 1, 10 + comm: PRINT "COM";
        LOCATE 8, 34: COLOR 1, 10 + ln(1): PRINT "ln1";
        LOCATE 8, 39: COLOR 1, 10 + ln(1): PRINT "ln2";
        LOCATE 8, 44: COLOR 1, 10 + ln(1): PRINT "ln3";
        LOCATE 8, 56: COLOR 1, 10 + ln(1): PRINT "Guidance";
        LOCATE 8, 66: COLOR 1, 10 + ln(1): PRINT "Network";
        LOCATE 12, 66: COLOR 1, 10 + ln(1): PRINT "Life Sup";
        LOCATE 9, 57: COLOR 14, 0: PRINT CHR$(186);
        LOCATE 9, 67: COLOR 14, 0: PRINT CHR$(186);
        LOCATE 11, 67: COLOR 14, 0: PRINT CHR$(186);
        LOCATE 8, 48: COLOR 14, 0: PRINT CHR$(201);
        LOCATE 8, 49: COLOR 14, 0: PRINT CHR$(205); CHR$(205); CHR$(17); CHR$(205);
        LOCATE 8, 53: COLOR 14, 0: PRINT CHR$(187);
        LOCATE 9, 53: COLOR 14, 0: PRINT CHR$(186);
        

        FOR i = 1 TO 4
         LOCATE 16, 50 + (5 * i): COLOR 1, 10: PRINT "GPD"; : PRINT USING "#"; i;
         LOCATE 15, 51 + (5 * i) + 1: COLOR 14, 0: PRINT CHR$(186);
        NEXT i
        LOCATE 16, 75: COLOR 1, 10: PRINT "TTC ";
        LOCATE 15, 76: COLOR 14, 0: PRINT CHR$(186);

        LOCATE 16, 40: COLOR 1, 10 + rc(3): PRINT "R-CON1";
        LOCATE 15, 42: COLOR 14, 0: PRINT CHR$(186);
        LOCATE 16, 47: COLOR 1, 10 + rc(4): PRINT "R-CON2";
        LOCATE 15, 49: COLOR 14, 0: PRINT CHR$(186);
        
        COLOR 1, 10 + reactor(2)
        LOCATE 16, 16: PRINT "AYSE   ";
        LOCATE 17, 16: PRINT "REACTOR";
        LOCATE 18, 16: PRINT "       ";
        LOCATE 15, 19: COLOR 14, 0: PRINT CHR$(186);



        COLOR 14, 0
        COLOR 1, 10 + reactor(1)
        LOCATE 8, 16: PRINT "HABITAT";
        LOCATE 9, 16: PRINT "REACTOR";
        LOCATE 10, 16: PRINT "       ";
        COLOR 14, 0
        FOR i = 7 TO 8
         LOCATE i, 76: PRINT CHR$(186);
         LOCATE i + 2, 76: PRINT CHR$(186);
        NEXT i
        COLOR 7, 0
        LOCATE 1, 2: FOR i = 1 TO 78: PRINT CHR$(196); : NEXT i
        LOCATE 21, 2: FOR i = 1 TO 78: PRINT CHR$(196); : NEXT i
        LOCATE 1, 1: PRINT CHR$(218);
        LOCATE 1, 14: PRINT CHR$(194);
        LOCATE 21, 1: PRINT CHR$(192);
        LOCATE 1, 80: PRINT CHR$(191);
        LOCATE 21, 80: PRINT CHR$(217);
        FOR i = 2 TO 20
         LOCATE i, 80: PRINT CHR$(179);
         LOCATE i, 14: PRINT CHR$(179);
         LOCATE i, 1: PRINT CHR$(179);
        NEXT i
        LOCATE 13, 1
        PRINT CHR$(195);
        FOR i = 1 TO 78: PRINT CHR$(196); : NEXT i
        PRINT CHR$(180);
        COLOR 7, 0
        'LOCATE 7, 1: PRINT CHR$(218); : FOR i = 1 TO 12: PRINT CHR$(196); : NEXT i
        LOCATE 13, 14: PRINT CHR$(197);
        'LOCATE 7, 14: PRINT CHR$(180);
        LOCATE 21, 14: PRINT CHR$(193);
        LOCATE 11, 75: PRINT CHR$(218); CHR$(215); CHR$(191);
        LOCATE 12, 75: PRINT CHR$(179);
        LOCATE 12, 77: PRINT CHR$(179);
        LOCATE 13, 75: PRINT CHR$(193); CHR$(215); CHR$(193);
        LOCATE 8, 25: COLOR 1, 10 + reg1: PRINT "TRN";
        LOCATE 8, 75: COLOR 1, 10 + reg2: PRINT "TRN";
        
        LOCATE 6, 2: COLOR 9, 0: PRINT "FUEL";
        LOCATE 15, 4: COLOR 9, 0: PRINT "FUEL";

        LOCATE 22, 18: PRINT "DETACH MODULE";
        LOCATE 23, 18: PRINT "DOCK MODULE";
        LOCATE 25, 18: PRINT "THERMAL ROLL";


        LOCATE 22, 36: PRINT "HAB DOCK HATCH";
        LOCATE 23, 36: PRINT "DOCK HATCH LOCK";
        LOCATE 24, 36: PRINT "HATCH LOCK ARM";
        
        LOCATE 22, 58: PRINT "AYSE AUTO DOCKING";
        COLOR 6, 0
        LOCATE 23, 56: PRINT CHR$(217);
        COLOR 9, 0
        LOCATE 23, 58: PRINT "DOCKING LATCH";
        LOCATE 24, 58: PRINT "DOCKING LATCH ARM";
        LOCATE 25, 58: PRINT "IN POSITION";
        tt = TIMER
        
        LOCATE 22, 1
        COLOR 6, 0
        PRINT "TAB";
        COLOR 9, 0
        PRINT " temp";

        LOCATE 23, 1
        COLOR 6, 0
        PRINT "`";
        COLOR 9, 0
        PRINT "   curr (kA)";

        IF UCASE$(LEFT$(reload$, 1)) = "Y" THEN GOSUB 830


1       warnFLAG = warnFLAG + 1: IF warnFLAG = 1000 THEN warnFLAG = 0
        COLOR 9, 0: LOCATE 2, 2: PRINT "HAB:  "; : COLOR 11, 0: IF AYSE = 150 THEN PRINT " OFF  ";  ELSE PRINT USING "###.#"; eng; : COLOR 9, 0: PRINT "%";
        COLOR 9, 0: LOCATE 14, 2: PRINT "AYSE: "; : COLOR 11, 0: IF AYSE = 0 THEN PRINT " OFF  ";  ELSE PRINT USING "###.#"; eng; : COLOR 9, 0: PRINT "%";
        
        COLOR 11, 0: LOCATE 7, 2: PRINT USING "########"; HABfuel#; : COLOR 9, 0: PRINT "kg";
        COLOR 11, 0: LOCATE 8, 2: PRINT USING "########"; HABfuelRATE#; : COLOR 9, 0: PRINT "kg/h";
        
        COLOR 10, 0
        COLOR 11, 0: LOCATE 16, 2: PRINT USING "########"; AYSEfuel#; : COLOR 9, 0: PRINT "kg";
        COLOR 11, 0: LOCATE 17, 2: PRINT USING "########"; AYSEfuelRATE#; : COLOR 9, 0: PRINT "kg/h";
       
        LOCATE 3, 29: COLOR 6, 1: PRINT "y";
        LOCATE 3, 30: COLOR 6, 1 + switch(45, 5): PRINT " ";
        COLOR 9 - (8 * switch(45, 5)), 1 + switch(45, 5): PRINT USING "####"; AgravAcc;
        LOCATE 3, 15: COLOR 6, 1: PRINT CHR$(17); CHR$(16); " ";
        COLOR 9, 1: PRINT USING "########"; Rshield; : PRINT "% "
       
        COLOR 14, 0
        LOCATE 11, 76: PRINT CHR$(215);
        LOCATE 13, 76: PRINT CHR$(215);
        FOR i = 0 TO 33
         LOCATE switch(i, 2), switch(i, 4): COLOR 14, 0: PRINT CHR$(206 - (switch(i, 5) * 20));
         IF i = 16 THEN LOCATE 11, 53: COLOR 14, 0: PRINT CHR$(206 - ((1 - switch(i, 5)) * 20));
        NEXT i
        i = 56
        LOCATE switch(i, 2), switch(i, 4): COLOR 14, 0: PRINT CHR$(206 - (switch(i, 5) * 20));
        FOR i = 34 TO 55
         IF i = 46 OR i = 47 THEN 3
         clr = 0
         IF i < 45 AND DEVICEs(i) = 1 AND warnFLAG > 500 THEN clr = 12
         IF i < 45 AND DEVICEs(i) = 2 THEN clr = 12
         LOCATE switch(i, 2), switch(i, 3) + 1: COLOR 9 + switch(i, 5), clr: PRINT switchlabel$(i - 33);
3       NEXT i
        i = 46: LOCATE switch(i, 2), switch(i, 3) + 1: COLOR 9 + switch(i, 5), 1: PRINT switchlabel$(i - 33);
        IF Htemp < 50 THEN clr = 8
        IF Htemp > 49 THEN clr = 9
        IF Htemp > 70 THEN clr = 3
        IF Htemp > 80 THEN clr = 10
        IF Htemp > 100 THEN clr = 14
        IF Htemp > 110 THEN clr = 5
        IF Htemp > 115 THEN clr = 12
        COLOR clr, 1
        PRINT USING "###"; Htemp;
       
        i = 47: LOCATE switch(i, 2), switch(i, 3) + 1: COLOR 9 + switch(i, 5), 1: PRINT switchlabel$(i - 33);
        IF Atemp < 50 THEN clr = 8
        IF Atemp > 49 THEN clr = 9
        IF Atemp > 70 THEN clr = 3
        IF Atemp > 80 THEN clr = 10
        IF Atemp > 100 THEN clr = 14
        IF Atemp > 110 THEN clr = 5
        IF Atemp > 115 THEN clr = 12
        COLOR clr, 1
        PRINT USING "###"; Atemp;
        COLOR 14, 0

        LOCATE 12, 76: IF AYSE = 150 THEN PRINT CHR$(186);  ELSE PRINT CHR$(205);
        'LOCATE 7, 18: IF AYSE = 0 THEN PRINT CHR$(186);  ELSE PRINT CHR$(206);
        COLOR 1 + (9 * (AYSE / 150)), 0: LOCATE 23, 58: PRINT "DOCKING LATCH";
        IF AYSE = 150 THEN COLOR 8, 0 ELSE COLOR 10, 0
       
        IF atm > .05 THEN clr = 10 ELSE clr = 9
        COLOR clr, 0: LOCATE 5, 4: PRINT "AIR INTAKE";
             
        LOCATE 12, 52: COLOR 1, 10 + BATT(2): PRINT "BAT   "; : LOCATE 12, 58: PRINT USING "#####"; BattAH(2); : PRINT "Ah";
                                
        'COLOR 12, 5: LOCATE switch(SWc, 2), switch(SWc, 3): PRINT "#";

        
        FOR i = 1 TO 4
         IF switch(5 + (2 * i), 5) = 1 AND Vbus1 > 8333 THEN clr = 2 ELSE clr = 6
         IF switch(5 + (2 * i), 5) = 0 OR Vbus1 < 100 THEN clr = 7
         IF switch(5 + (2 * i), 6) > 0 THEN current = Vbus1 / switch(5 + (2 * i), 6) ELSE current = 0
         IF DEVICEs(5 + (2 * i)) = 1 AND warnFLAG > 500 THEN clr = 12
         Zuse$ = "####"
         IF current < 100000 THEN Zuse$ = "##.#"
         IF current < 10000 THEN Zuse$ = "#.##"
         LOCATE 2, 51 + (6 * i)
         COLOR 1, clr
         IF LOADdisp = 0 THEN PRINT "ION"; : PRINT USING "#"; i;  ELSE PRINT USING Zuse$; current / 1000;
         IF switch(4 + (2 * i), 5) = 1 AND Vbus1 > 8333 THEN clr = 2 ELSE clr = 6
         IF switch(4 + (2 * i), 5) = 0 OR Vbus1 < 100 THEN clr = 7
         IF switch(4 + (2 * i), 6) > 0 THEN current = Vbus1 / switch(4 + (2 * i), 6) ELSE current = 0
         IF DEVICEs(4 + (2 * i)) = 1 AND warnFLAG > 500 THEN clr = 12
         Zuse$ = "####"
         IF current < 100000 THEN Zuse$ = "##.#"
         IF current < 10000 THEN Zuse$ = "#.##"
         LOCATE 4, 50 + (6 * i)
         COLOR 1, clr
         IF LOADdisp = 0 THEN PRINT "ACC"; : PRINT USING "#"; i;  ELSE PRINT USING Zuse$; current / 1000;
        NEXT i
        COLOR 1, 2
        LOCATE 9, 16
        IF LOADdisp = 0 THEN PRINT "REACTOR";  ELSE PRINT USING "##.####"; Vbus1 * Hreactor / 1000;
        LOCATE 17, 16
        IF LOADdisp = 0 THEN PRINT "REACTOR";  ELSE PRINT USING "##.####"; Vbus4 * Areactor / 1000;
        clr = 2
        IF RCcap <= 0 THEN clr = 7: RCcap = 0
        IF RCcap <= 0 AND Htemp >= 30 THEN source(1, 2) = 1
        IF Htemp < 30 THEN clr = 7
        IF switch(4, 6) > 0 THEN current = Vbus1 / switch(4, 6) ELSE current = 0
        LOCATE 4, 37: COLOR 1, clr + rc(1): IF LOADdisp = 0 THEN PRINT "R-CON1";  ELSE PRINT USING "#.####"; current / 1000;
        IF switch(5, 6) > 0 THEN current = Vbus1 / switch(5, 6) ELSE current = 0
        LOCATE 4, 44: COLOR 1, clr + rc(2): IF LOADdisp = 0 THEN PRINT "R-CON2";  ELSE PRINT USING "#.####"; current / 1000;
        IF switch(19, 5) = 1 AND Vbus1 > 8333 THEN clr = 2 ELSE clr = 6
        IF switch(19, 5) = 0 OR Vbus1 < 100 THEN clr = 7
        IF DEVICEs(19) = 1 AND warnFLAG > 500 THEN clr = 12
        LOCATE 4, 51: COLOR 1, clr + RCSP: PRINT "RCSP";
        IF switch(3, 5) = 1 AND Vbus1 > 8333 THEN clr = 2 ELSE clr = 6
        IF switch(3, 5) = 0 OR Vbus1 < 100 THEN clr = 7
        IF switch(3, 6) > 0 THEN current = Vbus1 / switch(3, 6) ELSE current = 0
        IF DEVICEs(3) = 1 AND warnFLAG > 500 THEN clr = 12
        Zuse$ = "######"
        IF current < 100000 THEN Zuse$ = "##.###"
        IF current < 10000 THEN Zuse$ = "#.####"
        LOCATE 4, 29: COLOR 1, clr: IF LOADdisp = 0 THEN PRINT "A GRAV";  ELSE PRINT USING Zuse$; current / 1000;
        IF switch(1, 5) = 1 AND Vbus1 > 8333 THEN clr = 2 ELSE clr = 6
        IF switch(1, 5) = 0 OR Vbus1 < 100 THEN clr = 7
        IF switch(1, 6) > 0 THEN current = Vbus1 / switch(1, 6) ELSE current = 0
        IF DEVICEs(1) = 1 AND warnFLAG > 500 THEN clr = 12
        Zuse$ = "###.##"
        IF current < 100000 THEN Zuse$ = "##.###"
        IF current < 10000 THEN Zuse$ = "#.####"
        LOCATE 4, 15: COLOR 1, clr: IF LOADdisp = 0 THEN PRINT "RAD-S1";  ELSE PRINT USING Zuse$; current / 1000;
        IF switch(2, 5) = 1 AND Vbus1 > 8333 THEN clr = 2 ELSE clr = 6
        IF switch(2, 5) = 0 OR Vbus1 < 100 THEN clr = 7
        IF switch(2, 6) > 0 THEN current = Vbus1 / switch(2, 6) ELSE current = 0
        IF DEVICEs(2) = 1 AND warnFLAG > 500 THEN clr = 12
        Zuse$ = "###.##"
        IF current < 100000 THEN Zuse$ = "##.###"
        IF current < 10000 THEN Zuse$ = "#.####"
        LOCATE 4, 22: COLOR 1, clr: IF LOADdisp = 0 THEN PRINT "RAD-S2";  ELSE PRINT USING Zuse$; current / 1000;
        IF switch(20, 5) = 1 AND Vbus2 > 100 THEN clr = 2 ELSE clr = 6
        IF switch(20, 5) = 0 OR Vbus2 < 10 THEN clr = 7
        IF DEVICEs(20) = 1 AND warnFLAG > 500 THEN clr = 12
        IF switch(20, 6) > 0 THEN current = Vbus2 / switch(2, 6) ELSE current = 0
        LOCATE 8, 29: COLOR 1, clr: IF LOADdisp = 0 THEN PRINT "COM";  ELSE PRINT USING ".##"; current / 1000;
        IF switch(21, 5) = 1 AND Vbus2 > 100 THEN clr = 2 ELSE clr = 6
        IF switch(21, 5) = 0 OR Vbus2 < 10 THEN clr = 7
        IF DEVICEs(21) = 1 AND warnFLAG > 500 THEN clr = 12
        LOCATE 8, 34: COLOR 1, clr + ln(1): PRINT "ln1";
        IF switch(22, 5) = 1 AND Vbus2 > 100 THEN clr = 2 ELSE clr = 6
        IF switch(22, 5) = 0 OR Vbus2 < 10 THEN clr = 7
        IF DEVICEs(22) = 1 AND warnFLAG > 500 THEN clr = 12
        LOCATE 8, 39: COLOR 1, clr + ln(2): PRINT "ln2";
        IF switch(23, 5) = 1 AND Vbus2 > 100 THEN clr = 2 ELSE clr = 6
        IF switch(23, 5) = 0 OR Vbus2 < 10 THEN clr = 7
        IF DEVICEs(23) = 1 AND warnFLAG > 500 THEN clr = 12
        LOCATE 8, 44: COLOR 1, clr + ln(3): PRINT "ln3";
        IF Vbus3 < 10 THEN clr = 7 ELSE clr = 2
        IF DEVICEs(55) = 1 AND warnFLAG > 500 THEN clr = 12
        IF DEVICEs(55) = 2 THEN clr = 12
        LOCATE 8, 56: COLOR 1, clr + ln(4): PRINT "Guidance";
        IF Vbus3 < 10 THEN clr = 7 ELSE clr = 2
        LOCATE 8, 66: COLOR 1, clr + ln(5): PRINT "Network";
        LOCATE 12, 66: COLOR 1, clr + ln(6): PRINT "Life Sup";
       
        FOR i = 1 TO 4
         IF switch(28 + i, 5) = 1 AND Vbus4 > 8333 THEN clr = 2 ELSE clr = 6
         IF switch(28 + i, 5) = 0 OR Vbus4 < 100 THEN clr = 7
         IF DEVICEs(28 + i) = 1 AND warnFLAG > 500 THEN clr = 12
         IF switch(28 + i, 6) > 0 THEN current = Vbus4 / switch(28 + i, 6) ELSE current = 0
         Zuse$ = "####"
         IF current < 100000 THEN Zuse$ = "##.#"
         IF current < 10000 THEN Zuse$ = "#.##"
         LOCATE 16, 50 + (5 * i): COLOR 1, clr: IF LOADdisp = 0 THEN PRINT "GPD"; : PRINT USING "#"; i;  ELSE PRINT USING Zuse$; current / 1000;
        NEXT i
        IF switch(33, 5) = 1 AND Vbus4 > 8333 THEN clr = 2 ELSE clr = 6
        IF switch(33, 5) = 0 OR Vbus4 < 100 THEN clr = 7
        IF DEVICEs(33) = 1 AND warnFLAG > 500 THEN clr = 12
        IF switch(33, 6) > 0 THEN current = Vbus4 / switch(33, 6) ELSE current = 0
        Zuse$ = "####"
        IF current < 100000 THEN Zuse$ = "##.#"
        IF current < 10000 THEN Zuse$ = "#.##"
        LOCATE 16, 75: COLOR 1, clr: IF LOADdisp = 0 THEN PRINT "TTC ";  ELSE PRINT USING Zuse$; current / 1000;
        AreactorRC = 0
        IF switch(27, 5) = 1 AND Vbus4 > 8333 THEN clr = 2 ELSE clr = 6
        IF Atemp < 30 THEN clr = 7
        IF DEVICEs(27) = 1 AND warnFLAG > 500 THEN clr = 12
        IF switch(27, 5) = 0 OR Vbus4 < 100 THEN clr = 7: AreactorRC = AreactorRC + 1
        IF switch(27, 6) > 0 THEN current = Vbus4 / switch(27, 6) ELSE current = 0
        Zuse$ = "######"
        IF current < 100000 THEN Zuse$ = "##.###"
        IF current < 10000 THEN Zuse$ = "#.####"
        LOCATE 16, 40: COLOR 1, clr: IF LOADdisp = 0 THEN PRINT "R-CON1";  ELSE PRINT USING Zuse$; current / 1000;
        IF switch(28, 5) = 1 AND Vbus4 > 8333 THEN clr = 2 ELSE clr = 6
        IF Atemp < 30 THEN clr = 7
        IF DEVICEs(28) = 1 AND warnFLAG > 500 THEN clr = 12
        IF switch(28, 5) = 0 OR Vbus4 < 100 THEN clr = 7: AreactorRC = AreactorRC + 1
        IF switch(28, 6) > 0 THEN current = Vbus4 / switch(28, 6) ELSE current = 0
        Zuse$ = "######"
        IF current < 100000 THEN Zuse$ = "##.###"
        IF current < 10000 THEN Zuse$ = "#.####"
        LOCATE 16, 47: COLOR 1, clr: IF LOADdisp = 0 THEN PRINT "R-CON2";  ELSE PRINT USING Zuse$; current / 1000;
        IF Atemp < 30 THEN AreactorRC = 0
        IF AYSEdist < 300 THEN COLOR 10, 0 ELSE COLOR 9, 0
        LOCATE 25, 58: PRINT "IN POSITION";
        IF switch(55, 5) = 1 THEN COLOR 10, 0 ELSE COLOR 9, 0
        LOCATE 24, 58: PRINT "DOCKING LATCH ARM";

        IF switch(48, 5) + switch(49, 5) = 2 THEN switch(48, 5) = 0: switch(49, 5) = 0
        IF switch(48, 5) = 1 THEN COLOR 10, 0 ELSE COLOR 9, 0
        LOCATE 22, 18: PRINT "DETACH MODULE";
        IF switch(49, 5) = 1 THEN COLOR 10, 0 ELSE COLOR 9, 0
        LOCATE 23, 18: PRINT "DOCK MODULE";
        IF switch(50, 5) = 1 THEN COLOR 10, 0 ELSE COLOR 9, 0
        LOCATE 25, 18: PRINT "THERMAL ROLL";

        IF switch(51, 5) = 1 THEN COLOR 10, 0 ELSE COLOR 9, 0
        LOCATE 22, 36: PRINT "HAB DOCK HATCH";
        IF switch(52, 5) = 1 THEN COLOR 10, 0 ELSE COLOR 9, 0
        LOCATE 23, 36: PRINT "DOCK HATCH LOCK";
        IF switch(53, 5) = 1 THEN COLOR 10, 0 ELSE COLOR 9, 0
        LOCATE 24, 36: PRINT "HATCH LOCK ARM";
       


        z$ = INKEY$
        'IF z$ = " " THEN SWc = SWc + 1
        IF z$ = "`" THEN LOADdisp = 1 - LOADdisp
        IF z$ = CHR$(27) THEN RUN "orbit5st.exe"
        IF z$ = CHR$(9) THEN DISPtemp = 1 - DISPtemp: GOTO 2
        IF z$ = "" THEN 2

2       IF AYSE = 150 THEN FOR i = 6 TO 13: switch(i, 5) = 0: NEXT i
        IF AYSE = 0 THEN FOR i = 29 TO 33: switch(i, 5) = 0: NEXT i
        COLOR 14, 0
        IF TIMER - tt < 2 THEN 1
        GOSUB 830
        GOSUB 810
        LOCATE 20, 60
        PRINT USING "#####_:"; year;
        PRINT USING "####_:"; day;
        PRINT USING "###_:"; hr;
        PRINT USING "##_:"; min;
        PRINT USING "##"; sec;

        LOCATE 19, 35
        IF master = 1 THEN COLOR 12, 4: PRINT "MASTER ALARM"; : GOSUB 6000 ELSE COLOR 8, 0: PRINT "MASTER ALARM";
        COLOR 7, 0

        GOSUB 200
        GOSUB 300
        
        IF RCcap < 1400 THEN clr = 14 ELSE clr = 10
        IF RCcap < 250 THEN clr = 12
        IF RCcap < 0 THEN RCcap = 0
        LOCATE 3, 37: COLOR clr, 1: PRINT USING "#########"; RCcap; : PRINT " As ";
        IF switch(38, 5) = 1 THEN switch(46, 5) = 0
        IF switch(39, 5) = 1 THEN switch(46, 5) = 0
        IF Vbus1 > 9900 THEN clr = 10 ELSE clr = 6
        IF Vbus1 < 9800 THEN clr = 4
        LOCATE 6, 15: COLOR 1, 10 + HP: PRINT "HAB POWER BUS:  "; : COLOR clr, 10 + HP: PRINT USING "###,###,###,###,###.#"; Ibus1 * Vbus1 / 1000; : PRINT "kW"; SPACE$(18);
        LOCATE 6, 55: COLOR clr, 10 + HP: PRINT USING "##,###,###.#"; Ibus1; : PRINT "A";
        LOCATE 6, 68: COLOR clr, 10 + HP: PRINT USING "##,###,###"; Vbus1; : PRINT "V";
       
        IF Vbus2 > 115 THEN clr = 10 ELSE clr = 6
        IF Vbus2 < 100 THEN clr = 4
        LOCATE 10, 45: COLOR clr, 10 + SP: PRINT USING "####"; Vbus2; : PRINT "V";
        LOCATE 10, 40: COLOR clr, 10 + SP: PRINT USING "####"; Ibus2; : PRINT "A";
        Pbus2 = Vbus2 * Ibus2
        LOCATE 10, 33: COLOR clr, 10 + SP: PRINT USING "###.#"; Pbus2 / 1000; : PRINT "kW";
       
        IF Vbus3 > 115 THEN clr = 10 ELSE clr = 6
        IF Vbus3 < 100 THEN clr = 4
        LOCATE 10, 69: COLOR clr, 10 + TP: PRINT USING "####"; Vbus3; : PRINT "V";
        LOCATE 10, 64: COLOR clr, 10 + SP: PRINT USING "####"; Ibus3; : PRINT "A";
        Pbus3 = Vbus3 * Ibus3
        LOCATE 10, 59: COLOR clr, 10 + SP: PRINT USING "##.#"; Pbus3 / 1000; : PRINT "kW";
       
        IF switch(43, 5) = 1 THEN switch(47, 5) = 0
        IF switch(44, 5) = 1 THEN switch(47, 5) = 0
        IF Vbus4 > 99000 THEN clr = 10 ELSE clr = 6
        IF Vbus4 < 98000 THEN clr = 4
        LOCATE 14, 15: COLOR 1, 10 + HP: PRINT "AYSE POWER BUS: "; : COLOR clr, 10 + HP: PRINT USING "###,###,###,###,###.#"; Ibus4 * Vbus4 / 1000; : PRINT "kW";
        LOCATE 14, 55: COLOR clr, 10 + HP: PRINT USING "##,###,###.#"; Ibus4; : PRINT "A";
        LOCATE 14, 68: COLOR clr, 10 + HP: PRINT USING "##,###,###"; Vbus4; : PRINT "V";
        
        oldHABfuelRATE# = HABfuelRATE#
        oldAYSEfuelRATE# = AYSEfuelRATE#
       
        Phab# = (Is1 * Vr)
        LOCATE 10, 16: COLOR 1, 2 + (2 * source(1, 2))
        IF Is1 < 1000000 THEN PRINT USING "######"; Is1; : PRINT "A";  ELSE PRINT USING "#####"; Is1 / 1000; : PRINT "kA";
        HABfuelRATE# = (.05# * Phab# / 50000000#) * 3600#


        Phabfc# = Is2 * Vfc
        HABfuelRATE# = HABfuelRATE# + ((.002# * Phabfc# / 2000#) * 3600#)
       
        HABfuel# = HABfuel# - (((HABfuelRATE# / 2#) + (oldHABfuelRATE# / 2#)) * DELtime# / 3600#)
        HABfuel# = HABfuel# - (HABleak * DELtime#)
        oldHABfuelRATE# = HABfuelRATE#

        IF Htemp > 79.5 AND (switch(38, 5) + switch(39, 5) > 0) THEN Heat = .51 ELSE Heat = 0
        Heat = Heat + (Phab# / 11400000000#)
        IF switch(46, 5) = 1 THEN Heater = 3 * ((Vbus1 ^ 2) / 1000) / 500000 ELSE Heater = 0
        delHtemp = (Htemp ^ 2) * .000078#
        Htemp = Htemp + Heater - delHtemp + Heat
        IF Htemp > 999 THEN Htemp = 999
        IF RND < (Htemp - 110) / 1000 THEN source(1, 2) = 1
       
       
        FCheat = Is2 * Is2 * source(2, 1) / 2000
        delFCtemp = (FCtemp ^ 2) * .00017 / 2
        IF delFCtemp > FCtemp / 2 THEN delFCtemp = FCtemp / 2
        FCtemp = FCtemp - delFCtemp + FCheat
        IF FCtemp > 999 THEN FCtemp = 999
        IF FCtemp < 0 THEN FCtemp = 0
        IF FCtemp > 90 THEN clr = 6 ELSE clr = 1
        IF FCtemp > 100 THEN clr = 14
        IF FCtemp > 110 THEN clr = 4
        IF FCtemp > 115 THEN clr = 12
        LOCATE 12, 25
        COLOR clr, 2 + (2 * source(2, 2))
        IF DISPtemp = 1 THEN 75
        PRINT "FC";
        IF Is2 < 99999 THEN PRINT USING "#####"; Is2; : PRINT "A";  ELSE PRINT USING "####"; Is2 / 1000; : PRINT "kA";
75      IF DISPtemp = 1 THEN PRINT "TEMP"; : PRINT USING "####"; FCtemp;
        IF RND < (FCtemp - 110) / 1000 THEN source(2, 2) = 1

      
        BATT1heat = Is3 * Is3 * source(3, 1) / 100
        delBATT1temp = (BATT1temp ^ 2) * .000035# / 1
        IF delBATTtemp > BATT1temp / 2 THEN delBATT1temp = BATT1temp / 2
        BATT1temp = BATT1temp - delBATT1temp + BATT1heat
        IF BATT1temp > 999 THEN BATT1temp = 999
        IF BATT1temp < 0 THEN BATT1temp = 0
        IF BATT1temp > 90 THEN clr = 6 ELSE clr = 1
        IF BATT1temp > 100 THEN clr = 14
        IF BATT1temp > 110 THEN clr = 4
        IF BATT1temp > 115 THEN clr = 12
        LOCATE 12, 34
        COLOR clr, 2 + (2 * source(3, 2))
        PRINT "BAT";
        IF DISPtemp = 1 THEN 76
        IF Is3 < 9999 THEN PRINT USING "####"; Is3; : PRINT "A";  ELSE PRINT USING "###"; Is3 / 1000; : PRINT "kA";
        PRINT USING "######"; BattAH(1); : PRINT "Ah";
76      IF DISPtemp = 1 THEN PRINT "     TEMP"; : PRINT USING "####"; BATT1temp;
        IF RND < (BATT1temp - 110) / 1000 THEN source(3, 2) = 1


        BATT3heat = Is4 * Is4 * source(4, 1) / 100
        delBATT3temp = (BATT3temp ^ 2) * .00076
        IF delBATT3temp > BATT3temp / 2 THEN delBATT3temp = BATT3temp / 2
        BATT3temp = BATT3temp - delBATT3temp + BATT3heat
        IF BATT3temp > 999 THEN BATT3temp = 999
        IF BATT3temp < 0 THEN BATT3temp = 0
        IF BATT3temp > 90 THEN clr = 6 ELSE clr = 1
        IF BATT3temp > 100 THEN clr = 14
        IF BATT3temp > 110 THEN clr = 4
        IF BATT3temp > 115 THEN clr = 12
        LOCATE 16, 25
        COLOR clr, 2 + (2 * source(4, 2))
        PRINT "BAT";
        IF DISPtemp = 1 THEN 77
        IF Is4 < 9999 THEN PRINT USING "####"; Is4; : PRINT "A";  ELSE PRINT USING "###"; Is4 / 1000; : PRINT "kA";
        PRINT USING "###"; BattAH(3); : PRINT "Ah";
77      IF DISPtemp = 1 THEN PRINT "  TEMP"; : PRINT USING "####"; BATT3temp;
        IF RND < (BATT3temp - 110) / 1000 THEN source(4, 2) = 1
       
        Payse# = (Is5 * VrA)
        LOCATE 18, 16: COLOR 1, 2 + (2 * source(5, 2))
        IF Is5 < 999999 THEN PRINT USING "######"; Is5; : PRINT "A";  ELSE PRINT USING "#####"; Is5 / 1000; : PRINT "kA";
        AYSEfuelRATE# = (.175# * Payse# / 960000000#) * 3600

        AYSEfuel# = AYSEfuel# - (((AYSEfuelRATE# / 2) + (oldAYSEfuelRATE# / 2)) * DELtime# / 3600#)
        AYSEfuel# = AYSEfuel# - (AYSEleak * DELtime#)
        oldAYSEfuelRATE# = AYSEfuelRATE#

        IF Atemp > 79.5 AND (switch(43, 5) + switch(44, 5) > 0) THEN Heat = .84 ELSE Heat = 0
        Heat = Heat + (Payse# / 151158200000#)
        IF switch(47, 5) = 1 THEN Heater = 5 * ((Vbus4 ^ 2) / 100000) / 500000 ELSE Heater = 0
        delAtemp = (Atemp ^ 2) * .000128#
        Atemp = Atemp + Heater - delAtemp + Heat
        IF Atemp > 999 THEN Atemp = 999
        IF RND < (Atemp - 110) / 1000 THEN source(5, 2) = 1

        IF Atemp < 30 THEN Areactor = 0
        IF AreactorRC = 2 THEN AreactorD = AreactorD + 1.1
        IF AreactorD > 0 THEN AreactorD = AreactorD - .1
        IF AreactorD > 59.99 THEN source(5, 2) = 1
        LOCATE 20, 16
        clr = 0: bk = 0
        IF AreactorD > 0 THEN clr = 4: bk = 1
        IF AreactorRC = 2 THEN clr = 12: bk = 1
        IF source(5, 2) = 1 THEN clr = 0: bk = 0: AreactorD = 60
        COLOR clr, bk
        PRINT USING "#######"; 60 - AreactorD;

        
        IF AYSE = 150 AND switch(37, 5) = 1 AND AYSEfuel# > 100 THEN HABfuel# = HABfuel# + 1000: AYSEfuel# = AYSEfuel# - 1000
        IF AYSE = 0 AND switch(37, 5) = 1 AND OCESSdist < 500 THEN HABfuel# = HABfuel# + 1000
        IF AYSE = 150 AND switch(36, 5) = 1 THEN HABfuel# = HABfuel# - 1000: AYSEfuel# = AYSEfuel# + 1000
        IF AYSE = 0 AND switch(36, 5) = 1 THEN HABfuel# = HABfuel# - 1000
        IF switch(41, 5) = 1 THEN AYSEfuel# = AYSEfuel# - 1000
        IF switch(42, 5) = 1 THEN AYSEfuel# = AYSEfuel# + (1000 * Pressure)
        IF AYSEfuel# < 0 THEN switch(41, 5) = 0: AYSEfuel# = 0
        IF HABfuel# < 0 THEN switch(36, 5) = 0: HABfuel# = 0
        IF AYSEfuel# > 20000000 THEN switch(42, 5) = 0: AYSEfuel# = 20000000
        IF HABfuel# > 150000 THEN switch(37, 5) = 0: HABfuel# = 150000

        COLOR 14, 0
        tt = TIMER
        GOTO 1
        END


100     'Main HAB bus Resistors
        switch(4, 6) = 0
        switch(5, 6) = 0
        IF Rshield > 0 THEN switch(1, 6) = switch(1, 5) / (73.86 * Rshield / 100#) ELSE switch(1, 6) = 0
        IF Rshield > 0 THEN switch(2, 6) = switch(2, 5) / (73.86 * Rshield / 100#) ELSE switch(2, 6) = 0
        switch(3, 6) = 0
        AgravAcc = ABS(Accel# - 10) - (50 * switch(45, 5))
        IF AgravAcc < 0 THEN AgravAcc = 0
        IF AgravAcc > 1 THEN switch(3, 6) = switch(3, 5) * (1 / ((1 / 50) + (AgravAcc / 4.4)))
        FOR i = 7 TO 13 STEP 2
         switch(i, 6) = 0
         IF eng > 0 THEN switch(i, 6) = switch(i, 5) / (.61 * eng / 100#)
        NEXT i
        FOR i = 6 TO 12 STEP 2
         switch(i, 6) = 0
         IF eng > 0 THEN switch(i, 6) = switch(i, 5) / (11.5775 * eng / 100#)
        NEXT i
       
        Rt = 1 / SHORT(1)
        FOR i = 1 TO 13
         IF switch(i, 6) > 0 THEN Rt = Rt + (1 / switch(i, 6))
        NEXT i
        IF switch(46, 5) = 1 THEN Rt = Rt + (1 / 1000)
        IF switch(19, 5) = 1 THEN Rt = Rt + (1 / 100000)
        RCflag = 0
        IF switch(4, 5) + switch(5, 5) > 0 AND RCcap < 2000 THEN RCflag = 1: Rt = Rt + (1 / 1000)
       
109     Hreactor = 0
        IF switch(38, 5) = 1 THEN Hreactor = Hreactor + .00003
        IF switch(39, 5) = 1 THEN Hreactor = Hreactor + .00003
        IF Htemp > 30 THEN Hreactor = Hreactor + .0004
        IF Htemp > 80 THEN Hreactor = Hreactor + .00004
        Rt = Rt + (switch(0, 5) * Hreactor)
        Tsw = switch(4, 5) + switch(5, 5)
        boost = 0
        res = Rt + (switch(0, 5) * link * (1 / (Rt4 / 100)))
        IF 1 / res < .017 THEN boost = (.00000003# * res / .17)
        IF switch(4, 5) = 1 THEN switch(4, 6) = (1 / ((res) * (.001016 + boost))) * Tsw ELSE switch(4, 6) = 1E+15
        IF switch(5, 5) = 1 THEN switch(5, 6) = (1 / ((res) * (.001016 + boost))) * Tsw ELSE switch(5, 6) = 1E+15
        IF switch(4, 6) < 1E+14 AND switch(4, 6) > 10000 THEN switch(4, 6) = 10000
        IF switch(5, 6) < 1E+14 AND switch(5, 6) > 10000 THEN switch(5, 6) = 10000
        IF switch(4, 5) > 0 THEN Rt = Rt + (1 / switch(4, 6))
        IF switch(5, 5) > 0 THEN Rt = Rt + (1 / switch(5, 6))
        Rt = 1 / Rt
        RETURN

        'HAB BUS 2 resistors
110     Rt1 = 1 / SHORT(2)
        Rcom = (1E+12 / (OCESSdist + 1))
        IF Rcom < .2 THEN Rcom = .2
        IF switch(20, 5) = 1 THEN Rt1 = Rt1 + (1 / Rcom) + (1 / 8)
        IF switch(21, 5) = 1 THEN Rt1 = Rt1 + (1 / 8)
        IF switch(22, 5) = 1 THEN Rt1 = Rt1 + (1 / 8)
        IF switch(23, 5) = 1 THEN Rt1 = Rt1 + (1 / 8)
        IF switch(18, 5) = 0 AND BattAH(1) < 10000 THEN Rt1 = Rt1 + (1 / 2500)
        Rt1 = 1 / Rt1
        RETURN
     
        'HAB BUS 3 resistors
120     Rt2 = 1 / 8
        IF switch(16, 5) = 1 AND BattAH(2) < 9999 THEN Rt2 = Rt2 + (1 / 2500)
        Rt2 = 1 / Rt2
        RETURN


        'AYSE BUS resistors
130     Rt4 = 1 / SHORT(3)
        switch(33, 6) = 0
        IF eng > 0 THEN switch(33, 6) = switch(33, 5) / (16 * eng / 1850)
        IF switch(33, 5) = 0 THEN TTCeff = 100 ELSE TTCeff = 1
        FOR i = 29 TO 32
         switch(i, 6) = 0
         IF eng > 0 THEN switch(i, 6) = switch(i, 5) / ((40.5 / TTCeff) * eng / 1850)
        NEXT i
        FOR i = 29 TO 33
         IF switch(i, 6) > 0 THEN Rt4 = Rt4 + (1 / switch(i, 6))
        NEXT i
        IF Atemp > 0 AND Vbus4 < 1000 THEN switch(26, 5) = 1: switch(43, 5) = 0: switch(44, 5) = 0
        IF switch(26, 5) = 0 AND BattAH(3) < 10 THEN Rt4 = Rt4 + (1 / 2500000)
        IF switch(47, 5) = 1 THEN Rt4 = Rt4 + (1 / 100000)

139     Areactor = 0
        IF switch(43, 5) = 1 THEN Areactor = Areactor + .00003
        IF switch(44, 5) = 1 THEN Areactor = Areactor + .00003
        IF Atemp > 30 THEN Areactor = Areactor + .00002
        IF Atemp > 80 THEN Areactor = Areactor + .000005
        Rt4 = Rt4 + Areactor
        Tsw = switch(27, 5) + switch(28, 5)
        boost = 0
        IF 1 / Rt4 < .017 THEN boost = (.00000003# * Rt4 / .17)
        IF switch(27, 5) = 1 THEN switch(27, 6) = (1 / ((Rt4) * (.001016 + boost))) * Tsw ELSE switch(27, 6) = 1E+25
        IF switch(28, 5) = 1 THEN switch(28, 6) = (1 / ((Rt4) * (.001016 + boost))) * Tsw ELSE switch(28, 6) = 1E+25
        IF switch(27, 6) < 1E+24 AND switch(27, 6) > 1000000 THEN switch(27, 6) = 1000000
        IF switch(28, 6) < 1E+24 AND switch(28, 6) > 1000000 THEN switch(28, 6) = 1000000
        IF switch(27, 5) > 0 THEN Rt4 = Rt4 + (1 / switch(27, 6))
        IF switch(28, 5) > 0 THEN Rt4 = Rt4 + (1 / switch(28, 6))
        Rt4 = 1 / Rt4
        RETURN


        'Source Power Capacities
200     Hreactor = 0
        Vr = 0
        Vfc = 0
        Vb1 = 0
        Vb2 = 0
        Vb3 = 0
        VrA = 0
        GOSUB 800
        FOR i = 1 TO 56
         IF SWITCHs(i) = 1 THEN switch(i, 5) = 0
        NEXT i

        IF HABfuel# <= 0 THEN 201
        IF switch(0, 5) = 0 THEN 203
        IF Htemp > 80 AND switch(38, 5) = 1 THEN Vr = 10000 + SOURCEs(1, 1)
        IF Htemp > 80 AND switch(39, 5) = 1 THEN Vr = 10000 + SOURCEs(1, 1)
203     IF switch(40, 5) * switch(14, 5) = 1 THEN Vfc = 120 + SOURCEs(2, 1)
201     IF BattAH(1) > 0 AND switch(18, 5) = 1 THEN Vb1 = 120 + SOURCEs(3, 1)
        IF BattAH(2) > 0 AND switch(16, 5) = 0 THEN Vb2 = 120 + SOURCEs(6, 1)
        IF BattAH(3) > 0 AND switch(26, 5) = 1 THEN Vb3 = 100000 + SOURCEs(4, 1)
        IF AYSEfuel# <= 0 THEN 204
        IF switch(56, 5) = 0 THEN 204
        IF Atemp > 80 AND switch(43, 5) = 1 THEN VrA = 100000 + SOURCEs(5, 1)
        IF Atemp > 80 AND switch(44, 5) = 1 THEN VrA = 100000 + SOURCEs(5, 1)
      


204     IF source(1, 2) = 1 THEN Vr = 0
        IF source(2, 2) = 1 THEN Vfc = 0
        IF source(3, 2) = 1 THEN Vb1 = 0
        IF source(4, 2) = 1 THEN Vb3 = 0
        IF source(5, 2) = 1 THEN VrA = 0
        link = switch(24, 5) * switch(25, 5) * (AYSE / 150)
        GOSUB 130
        GOSUB 100
        GOSUB 110
        GOSUB 120
        Vbus1max = 0
        Vbus2max = 0
        Vbus3max = 0
        Vbus4max = 0
        source(1, 1) = .00025 * (1 + SOURCEs(1, 2))
        source(2, 1) = .00325 * (1 + SOURCEs(2, 2))
        source(5, 1) = .00003 * (1 + SOURCEs(5, 2))
        source(3, 1) = .01 * (1 + (BATT1temp / 100) ^ 2) * (1 + SOURCEs(3, 2))
        source(4, 1) = .0055 * (1 + (BATT3temp / 100) ^ 2) * (1 + SOURCEs(4, 2))
        'LOCATE 25, 25
        'PRINT USING "##.####^^^^"; Rt; : PRINT " ";
        'PRINT USING "##.####^^^^"; Rt1; : PRINT " ";
        'PRINT USING "##.####^^^^"; Rt2; : PRINT " ";
        'PRINT USING "##.####^^^^"; Rt4; : PRINT " ";

        '*******************************************
        'Hab Reactor Output
        'BUS1
        V1# = Vr
        V2# = (switch(15, 5) * switch(17, 5)) * Vfc * 83.33334
        V3# = (switch(15, 5) * switch(17, 5)) * Vb1 * 83.33334
        IF V1# = 0 THEN R1# = 100000 ELSE R1# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5))
        R2# = source(2, 1) * 6944.445
        R3# = source(3, 1) * 6944.445
        V4# = Vb3 * link / 10
        V5# = VrA * link / 10
        R4# = source(4, 1) / 100
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5))) / 100
        R6# = Rt
        GOSUB 900
        Ibus1R = I1#
        IF Vbus1max < V1# THEN Vbus1max = V1#
        IF Vbus1max < V2# THEN Vbus1max = V2#
        IF Vbus1max < V3# THEN Vbus1max = V3#
        IF Vbus1max < V4# THEN Vbus1max = V4#
        IF Vbus1max < V5# THEN Vbus1max = V5#

       
        'BUS2
        V1# = Vr * (switch(15, 5) * switch(17, 5))
        V2# = Vfc * 83.33334
        V3# = Vb1 * 83.33334
        IF V1# = 0 THEN R1# = 0 ELSE R1# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5))
        R2# = source(2, 1) * 6944.445
        R3# = source(3, 1) * 6944.445
        V4# = Vb3 * (switch(15, 5) * switch(17, 5)) * link / 10
        V5# = VrA * (switch(15, 5) * switch(17, 5)) * link / 10
        R4# = source(4, 1) / 100
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5))) / 100
        R6# = Rt1 * 6944.445
        GOSUB 900
        Ibus2R = I1#
        

        'BUS3
        V1# = Vr * switch(15, 5) * switch(17, 5) * switch(16, 5)
        V2# = Vfc * switch(16, 5) * 83.33334
        V3# = Vb1 * switch(16, 5) * 83.33334
        IF V1# = 0 THEN R1# = 0 ELSE R1# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5))
        R2# = source(2, 1) * 6944.445
        R3# = source(3, 1) * 6944.445
        V4# = Vb3 * switch(15, 5) * switch(17, 5) * switch(16, 5) * link / 10
        V5# = VrA * switch(15, 5) * switch(17, 5) * switch(16, 5) * link / 10
        R4# = source(4, 1) / 100
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5))) / 100
        R6# = Rt2 * 6944.445
        GOSUB 900
        Ibus3R = I1#


        'BUS4
        V1# = Vr * link
        V2# = Vfc * 83.33334 * link * (switch(15, 5) * switch(17, 5))
        V3# = Vb1 * 83.33334 * link * (switch(15, 5) * switch(17, 5))
        IF V1# = 0 THEN R1# = 0 ELSE R1# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5))
        R2# = source(2, 1) * 6944.445
        R3# = source(3, 1) * 6944.445
        V4# = Vb3 / 10
        V5# = VrA / 10
        R4# = source(4, 1) / 100
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5))) / 100
        R6# = Rt4 / 100
        GOSUB 900
        Ibus4R = I1#
        IF Vbus4max < V1# * 10 THEN Vbus4max = V1# * 10
        IF Vbus4max < V2# * 10 THEN Vbus4max = V2# * 10
        IF Vbus4max < V3# * 10 THEN Vbus4max = V3# * 10
        IF Vbus4max < V4# * 10 THEN Vbus4max = V4# * 10
        IF Vbus4max < V5# * 10 THEN Vbus4max = V5# * 10
        '*******************************************

        '*******************************************    
        'Fuel Cell Output
        'BUS1
        V2# = Vr / 83.33334
        V1# = (switch(15, 5) * switch(17, 5)) * Vfc
        V3# = (switch(15, 5) * switch(17, 5)) * Vb1
        IF V2# = 0 THEN R2# = 0 ELSE R2# = (source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5))) / 6944.445
        R1# = source(2, 1)
        R3# = source(3, 1)
        V4# = Vb3 * link / 833.3334
        V5# = VrA * link / 833.3334
        R4# = source(4, 1) / 694444.5
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5))) / 694444.5
        R6# = Rt / 6944.445
        GOSUB 900
        Ibus1fc = I1#

        'BUS2
        V2# = Vr * (switch(15, 5) * switch(17, 5)) / 83.33334
        V1# = Vfc
        V3# = Vb1
        IF V2# = 0 THEN R2# = 0 ELSE R2# = (source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5))) / 6944.445
        R1# = source(2, 1)
        R3# = source(3, 1)
        V4# = Vb3 * (switch(15, 5) * switch(17, 5)) * link / 833.3334
        V5# = VrA * (switch(15, 5) * switch(17, 5)) * link / 833.3334
        R4# = source(4, 1) / 694444.5
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5))) / 694444.5
        R6# = Rt1
        GOSUB 900
        Ibus2fc = I1#
        IF Vbus2max < V1# THEN Vbus2max = V1#
        IF Vbus2max < V2# THEN Vbus2max = V2#
        IF Vbus2max < V3# THEN Vbus2max = V3#
        IF Vbus2max < V4# THEN Vbus2max = V4#
        IF Vbus2max < V5# THEN Vbus2max = V5#

        'BUS3
        V2# = Vr * (switch(15, 5) * switch(17, 5) * switch(16, 5)) / 83.33334
        V1# = Vfc * switch(16, 5)
        V3# = Vb1 * switch(16, 5)
        IF V2# = 0 THEN R2# = 0 ELSE R2# = (source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5))) / 6944.445
        R1# = source(2, 1)
        R3# = source(3, 1)
        V4# = Vb3 * switch(16, 5) * (switch(15, 5) * switch(17, 5)) * link / 833.3334
        V5# = VrA * switch(16, 5) * (switch(15, 5) * switch(17, 5)) * link / 833.3334
        R4# = source(4, 1) / 694444.5
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5))) / 694444.5
        R6# = Rt2
        GOSUB 900
        Ibus3fc = I1#
        IF Vbus3max < V1# THEN Vbus3max = V1#
        IF Vbus3max < V2# THEN Vbus3max = V2#
        IF Vbus3max < V3# THEN Vbus3max = V3#
        IF Vbus3max < V4# THEN Vbus3max = V4#
        IF Vbus3max < V5# THEN Vbus3max = V5#

        'BUS4
        V2# = Vr * link / 83.33334
        V1# = Vfc * (switch(15, 5) * switch(17, 5)) * link
        V3# = Vb1 * (switch(15, 5) * switch(17, 5)) * link
        IF V2# = 0 THEN R2# = 0 ELSE R2# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5)) / 6944.445
        R1# = source(2, 1)
        R3# = source(3, 1)
        V4# = Vb3 / 833.3334
        V5# = VrA / 833.3334
        R4# = source(4, 1) / 694444.5
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5))) / 694444.5
        R6# = Rt4 / 694444.5
        GOSUB 900
        Ibus4fc = I1#
        '*******************************************


        '*******************************************  
        'BUS 2 Battery Output
        'BUS1
        V2# = Vr / 83.33334
        V3# = (switch(15, 5) * switch(17, 5)) * Vfc
        V1# = (switch(15, 5) * switch(17, 5)) * Vb1
        IF V2# = 0 THEN R2# = 0 ELSE R2# = (source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5))) / 6944.445
        R3# = source(2, 1)
        R1# = source(3, 1)
        V4# = Vb3 * link / 833.3334
        V5# = VrA * link / 833.3334
        R4# = source(4, 1) / 694444.5
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5))) / 694444.5
        R6# = Rt / 6944.445
        GOSUB 900
        Ibus1b1 = I1#

        'BUS2
        V2# = Vr * (switch(15, 5) * switch(17, 5)) / 83.33334
        V3# = Vfc
        V1# = Vb1
        IF V2# = 0 THEN R2# = 0 ELSE R2# = (source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5))) / 6944.445
        R3# = source(2, 1)
        R1# = source(3, 1)
        V4# = Vb3 * (switch(15, 5) * switch(17, 5)) * link / 833.3334
        V5# = VrA * (switch(15, 5) * switch(17, 5)) * link / 833.3334
        R4# = source(4, 1) / 694444.5
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5))) / 694444.5
        R6# = Rt1
        GOSUB 900
        Ibus2b1 = I1#

        'BUS3
        V2# = Vr * (switch(15, 5) * switch(17, 5) * switch(16, 5)) / 83.33334
        V3# = Vfc * switch(16, 5)
        V1# = Vb1 * switch(16, 5)
        IF V2# = 0 THEN R2# = 0 ELSE R2# = (source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5))) / 6944.445
        R3# = source(2, 1)
        R1# = source(3, 1)
        V4# = Vb3 * switch(16, 5) * (switch(15, 5) * switch(17, 5)) * link / 833.3334
        V5# = VrA * switch(16, 5) * (switch(15, 5) * switch(17, 5)) * link / 833.3334
        R4# = source(4, 1) / 694444.5
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5))) / 694444.5
        R6# = Rt2
        GOSUB 900
        Ibus3b1 = I1#

        'BUS4
        V2# = Vr * link / 83.33334
        V3# = Vfc * (switch(15, 5) * switch(17, 5)) * link
        V1# = Vb1 * (switch(15, 5) * switch(17, 5)) * link
        IF V2# = 0 THEN R2# = 0 ELSE R2# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5)) / 6944.445
        R3# = source(2, 1)
        R1# = source(3, 1)
        V4# = Vb3 / 833.3334
        V5# = VrA / 833.3334
        R4# = source(4, 1) / 694444.5
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5))) / 694444.5
        R6# = Rt4 / 694444.5
        GOSUB 900
        Ibus4b1 = I1#
        '*******************************************
       
        '*******************************************
        'AYSE Reactor Output
        'BUS1
        V5# = Vr * 10
        V2# = (switch(15, 5) * switch(17, 5)) * Vfc * 833.3334
        V3# = (switch(15, 5) * switch(17, 5)) * Vb1 * 833.3334
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5)) * 100
        R2# = source(2, 1) * 694444.5
        R3# = source(3, 1) * 694444.5
        V4# = Vb3 * link
        V1# = VrA * link
        R4# = source(4, 1)
        IF V1# = 0 THEN R1# = 100000 ELSE R1# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5)))
        R6# = Rt * 100
        GOSUB 900
        Ibus1A = I1#

      
        'BUS2
        V5# = Vr * (switch(15, 5) * switch(17, 5)) * 10
        V2# = Vfc * 833.3334
        V3# = Vb1 * 833.3334
        IF V5# = 0 THEN R5# = 0 ELSE R5# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5)) * 100
        R2# = source(2, 1) * 694444.5
        R3# = source(3, 1) * 694444.5
        V4# = Vb3 * (switch(15, 5) * switch(17, 5)) * link
        V1# = VrA * (switch(15, 5) * switch(17, 5)) * link
        R4# = source(4, 1)
        IF V1# = 0 THEN R1# = 100000 ELSE R1# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5)))
        R6# = Rt1 * 694444.5
        GOSUB 900
        Ibus2A = I1#
        
        'LOCATE 23, 1: PRINT USING "##.####^^^^"; V5#; V2#; V3#; V4#; V1#;
        'LOCATE 24, 1: PRINT USING "##.####^^^^"; R5#; R2#; R3#; R4#; R1#; R6#;
        'LOCATE 25, 1: PRINT USING "##.####^^^^"; I1#;
       

        'BUS3
        V5# = Vr * switch(15, 5) * switch(17, 5) * switch(16, 5) * 10
        V2# = Vfc * switch(16, 5) * 833.3334
        V3# = Vb1 * switch(16, 5) * 833.3334
        IF V5# = 0 THEN R5# = 0 ELSE R5# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5)) * 100
        R2# = source(2, 1) * 694444.5
        R3# = source(3, 1) * 694444.5
        V4# = Vb3 * switch(15, 5) * switch(17, 5) * switch(16, 5) * link
        V1# = VrA * switch(15, 5) * switch(17, 5) * switch(16, 5) * link
        R4# = source(4, 1)
        IF V1# = 0 THEN R1# = 100000 ELSE R1# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5)))
        R6# = Rt2 * 694444.5
        GOSUB 900
        Ibus3A = I1#


        'BUS4
        V5# = Vr * link * 10
        V2# = Vfc * 833.3334 * link * (switch(15, 5) * switch(17, 5))
        V3# = Vb1 * 833.3334 * link * (switch(15, 5) * switch(17, 5))
        IF V5# = 0 THEN R5# = 0 ELSE R5# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5)) * 100
        R2# = source(2, 1) * 694444.5
        R3# = source(3, 1) * 694444.5
        V4# = Vb3
        V1# = VrA
        R4# = source(4, 1)
        IF V1# = 0 THEN R1# = 100000 ELSE R1# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5)))
        R6# = Rt4
        GOSUB 900
        Ibus4A = I1#
        '*******************************************

        '*******************************************
        'BATT 3 Output
        'BUS1
        V5# = Vr * 10
        V2# = (switch(15, 5) * switch(17, 5)) * Vfc * 833.3334
        V3# = (switch(15, 5) * switch(17, 5)) * Vb1 * 833.3334
        IF V5# = 0 THEN R5# = 100000 ELSE R5# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5)) * 100
        R2# = source(2, 1) * 694444.5
        R3# = source(3, 1) * 694444.5
        V1# = Vb3 * link
        V4# = VrA * link
        R1# = source(4, 1)
        IF V4# = 0 THEN R4# = 100000 ELSE R4# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5)))
        R6# = Rt * 100
        GOSUB 900
        Ibus1b3 = I1#
     
        'BUS2
        V5# = Vr * (switch(15, 5) * switch(17, 5)) * 10
        V2# = Vfc * 833.3334
        V3# = Vb1 * 833.3334
        IF V5# = 0 THEN R5# = 0 ELSE R5# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5)) * 100
        R2# = source(2, 1) * 694444.5
        R3# = source(3, 1) * 694444.5
        V1# = Vb3 * (switch(15, 5) * switch(17, 5)) * link
        V4# = VrA * (switch(15, 5) * switch(17, 5)) * link
        R1# = source(4, 1)
        IF V4# = 0 THEN R4# = 100000 ELSE R4# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5)))
        R6# = Rt1 * 694444.5
        GOSUB 900
        Ibus2b3 = I1#
      

        'BUS3
        V5# = Vr * switch(15, 5) * switch(17, 5) * switch(16, 5) * 10
        V2# = Vfc * switch(16, 5) * 833.3334
        V3# = Vb1 * switch(16, 5) * 833.3334
        IF V5# = 0 THEN R5# = 0 ELSE R5# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5)) * 100
        R2# = source(2, 1) * 694444.5
        R3# = source(3, 1) * 694444.5
        V1# = Vb3 * switch(15, 5) * switch(17, 5) * switch(16, 5) * link
        V4# = VrA * switch(15, 5) * switch(17, 5) * switch(16, 5) * link
        R1# = source(4, 1)
        IF V4# = 0 THEN R4# = 100000 ELSE R4# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5)))
        R6# = Rt2 * 694444.5
        GOSUB 900
        Ibus3b3 = I1#


        'BUS4
        V5# = Vr * link * 10
        V2# = Vfc * 833.3334 * link * (switch(15, 5) * switch(17, 5))
        V3# = Vb1 * 833.3334 * link * (switch(15, 5) * switch(17, 5))
        IF V5# = 0 THEN R5# = 0 ELSE R5# = source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5)) * 100
        R2# = source(2, 1) * 694444.5
        R3# = source(3, 1) * 694444.5
        V1# = Vb3
        V4# = VrA
        R1# = source(4, 1)
        IF V4# = 0 THEN R4# = 100000 ELSE R4# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5)))
        R6# = Rt4
        GOSUB 900
        Ibus4b3 = I1#
        '*******************************************
        'LOCATE 17, 24: PRINT USING "#########.##"; Ibus1R; Ibus2R * 83.33334; Ibus3R * 83.33334; Ibus4R / 10;
        'LOCATE 18, 24: PRINT USING "#########.##"; Ibus1fc / 83.33334; Ibus2fc; Ibus3fc; Ibus4fc / 833.3334;
        'LOCATE 19, 24: PRINT USING "#########.##"; Ibus1b1 / 83.33334; Ibus2b1; Ibus3b1; Ibus4b1 / 833.3334;
        'LOCATE 20, 24: PRINT USING "#########.##"; Ibus1b3 * 10; Ibus2b3 * 833.3334; Ibus3b3 * 833.3334; Ibus4b3;
        'LOCATE 21, 24: PRINT USING "#########.##"; Ibus1A * 10; Ibus2A * 833.3334; Ibus3A * 833.3334; Ibus4A;
        Ibus1 = Ibus1R + ((Ibus1fc + Ibus1b1) / 83.33334) + ((Ibus1A + Ibus1b3) * 10)
        Ibus2 = (Ibus2R * 83.33334) + Ibus2fc + Ibus2b1 + ((Ibus2A + Ibus2b3) * 833.3334)
        Ibus3 = (Ibus3R * 83.33334) + Ibus3fc + Ibus3b1 + ((Ibus3A + Ibus3b3) * 833.3334)
        Ibus4 = (Ibus4R / 10) + ((Ibus4fc + Ibus4b1) / 833.3334) + ((Ibus4A + Ibus4b3))
        Is1 = Ibus1R + Ibus2R + Ibus3R + Ibus4R
        Is2 = Ibus1fc + Ibus2fc + Ibus3fc + Ibus4fc
        Is3 = Ibus1b1 + Ibus2b1 + Ibus3b1 + Ibus4b1
        Is4 = Ibus1b3 + Ibus2b3 + Ibus3b3 + Ibus4b3
        Is5 = Ibus1A + Ibus2A + Ibus3A + Ibus4A
        R2# = (source(1, 1) * 2 / (.00001 + switch(38, 5) + switch(39, 5)))
        V1out = Vr - (Is1 * R2#)
        V2out = Vfc - (Is2 * source(2, 1))
        V3out = Vb1 - (Is2 * source(2, 1))
        V4out = Vb3 - (Is4 * source(4, 1))
        R2# = (source(5, 1) * 2 / (.00001 + switch(43, 5) + switch(44, 5)))
        V5out = VrA - (Is5 * R2#)
        'LOCATE 20, 30: PRINT V4out; Is4; Rt4;
        IF Ibus1 = 0 THEN Vbus1 = Vbus1max ELSE Vbus1 = (V1out * Ibus1R / Ibus1) + (V2out * Ibus1fc / Ibus1) + (V3out * Ibus1b1 / Ibus1) + (V4out * Ibus1b3 / Ibus1) + (V5out * Ibus1A / Ibus1)
        IF Ibus2 = 0 THEN Vbus2 = Vbus2max ELSE Vbus2 = (V1out * Ibus2R / Ibus2) + (V2out * Ibus2fc / Ibus2) + (V3out * Ibus2b1 / Ibus2) + (V4out * Ibus2b3 / Ibus2) + (V5out * Ibus2A / Ibus2)
        IF Ibus3 = 0 THEN Vbus3 = Vbus3max ELSE Vbus3 = (V1out * Ibus3R / Ibus3) + (V2out * Ibus3fc / Ibus3) + (V3out * Ibus3b1 / Ibus3) + (V4out * Ibus3b3 / Ibus3) + (V5out * Ibus3A / Ibus3)
        IF Ibus4 < .00000001# THEN Vbus4 = Vbus4max ELSE Vbus4 = (V1out * Ibus4R / Ibus4) + (V2out * Ibus4fc / Ibus4) + (V3out * Ibus4b1 / Ibus4) + (V4out * Ibus4b3 / Ibus4) + (V5out * Ibus4A / Ibus4)
        IF Ibus1 < 0 THEN Ibus1 = 0
        IF Ibus2 < 0 THEN Ibus2 = 0
        IF Ibus3 < 0 THEN Ibus3 = 0
        IF Ibus4 < 0 THEN Ibus4 = 0
        IF Vbus1 < 0 THEN Vbus1 = 0
        IF Vbus2 < 0 THEN Vbus2 = 0
        IF Vbus3 < 0 THEN Vbus3 = 0
        IF Vbus4 < 0 THEN Vbus4 = 0
        'IF Vbus4 < 9000 THEN switch(29, 5) = 0: switch(30, 5) = 0: switch(31, 5) = 0: switch(32, 5) = 0: switch(33, 5) = 0: switch(56, 5) = 0: switch(26, 5) = 1

        IF switch(18, 5) = 0 AND BattAH(1) < 10000 THEN BattAH(1) = BattAH(1) + (DELtime# * Vbus2 / 2500)
        BattAH(1) = BattAH(1) - (DELtime# * Is3 / 3600)
        IF BattAH(1) < 0 THEN BattAH(1) = 0
        IF source(3, 2) = 1 THEN BattAH(1) = 0

        IF switch(26, 5) = 0 AND BattAH(3) < 99 THEN BattAH(3) = BattAH(3) + (DELtime# * Vbus4 / 2500000)
        BattAH(3) = BattAH(3) - (DELtime# * Is4 / 3600)
        IF BattAH(3) < 0 THEN BattAH(3) = 0
        IF source(4, 2) = 1 THEN BattAH(3) = 0
       
        IF switch(16, 5) = 1 AND Vbus2 < 90 AND BattAH(2) > 0 THEN switch(16, 5) = 0
        IF switch(16, 5) = 1 AND BattAH(2) < 9999 THEN BattAH(2) = BattAH(2) + (DELtime# * Vbus3 / 2500)
        IF switch(16, 5) = 1 THEN 202
        IF BattAH(2) > 0 THEN Vbus3 = 120: Is6 = 15: Ibus3 = 15: BattAH(2) = BattAH(2) - (DELtime# * 15 / 3600) ELSE Vbus3 = 0: Is6 = 0: Ibus3 = 0
        IF BattAH(2) < 0 THEN BattAH(2) = 0
202     RETURN

300     IF switch(4, 5) = 0 THEN Vdel1 = 10000 ELSE Vdel1 = 10000 - Vbus1
        IF switch(5, 5) = 0 THEN Vdel2 = 10000 ELSE Vdel2 = 10000 - Vbus1
        Ireactor = 10000 * Hreactor
        IrcNEEDED# = (Is1 * .001)
        'LOCATE 21, 20: PRINT USING "####.#########"; Ireactor, IrcNEEDED#;
        IF Ireactor + Is1 > 0 AND IrcNEEDED# < 1 THEN IrcNEEDED# = 1#
        IrcNEEDED# = IrcNEEDED# + Ireactor
        IrcGEN# = ((Vbus1) / switch(4, 6)) + ((Vbus1) / switch(5, 6)) + (switch(0, 5) * Vbus1 * Hreactor)
        IF IrcGEN# < IrcNEEDED# THEN RCcap = RCcap + (IrcGEN# - IrcNEEDED#)
        'LOCATE 21, 25: PRINT USING "#######.#####"; IrcGEN#; IrcNEEDED#; Ireactor;
        IF IrcGEN# < IrcNEEDED AND Htemp > 30 THEN 301
        IF switch(4, 5) + switch(5, 5) > 0 AND RCcap < 2000 THEN RCcap = RCcap + (Vbus1 / 1000)
301     RETURN


900     'LOCATE 22, 1: PRINT USING "######"; V1#; V2#; V3#; V4#; V5#;
        'LOCATE 23, 1: PRINT USING "#######.#####"; R1#; R2#; R3#; R4#; R5#; R6#;
        IF V1# = 0 THEN I1# = 0: GOTO 901
        Rdenom# = 0 - R1# - R6#
        IF V2# > 0 THEN Rdenom# = Rdenom# - (R1# * R6# / R2#)
        IF V3# > 0 THEN Rdenom# = Rdenom# - (R1# * R6# / R3#)
        IF V4# > 0 THEN Rdenom# = Rdenom# - (R1# * R6# / R4#)
        IF V5# > 0 THEN Rdenom# = Rdenom# - (R1# * R6# / R5#)
        I1# = 0 - V1#
        IF V2# > 0 THEN I1# = I1# - (R6# * V1# / R2#)
        IF V2# > 0 THEN I1# = I1# + (R6# * V2# / R2#)
        IF V3# > 0 THEN I1# = I1# - (R6# * V1# / R3#)
        IF V3# > 0 THEN I1# = I1# + (R6# * V3# / R3#)
        IF V4# > 0 THEN I1# = I1# - (R6# * V1# / R4#)
        IF V4# > 0 THEN I1# = I1# + (R6# * V4# / R4#)
        IF V5# > 0 THEN I1# = I1# - (R6# * V1# / R5#)
        IF V5# > 0 THEN I1# = I1# + (R6# * V5# / R5#)

        I1# = I1# / Rdenom#
        P1# = R1# * I1# * I1#
        Vout# = V1# - (R1# * I1#)
901     'LOCATE 24, 1: PRINT USING "######.######"; I1#;
        'z$ = INPUT$(1)
        RETURN

800     destruct = 0
        OPEN "I", #1, zpath$ + "orb5rEs.txt"
        FOR i = 1 TO 5
         INPUT #1, SOURCEs(i, 1)
         INPUT #1, SOURCEs(i, 2)
         INPUT #1, z
         IF z = 1 THEN source(i, 2) = 0
        NEXT i
        FOR i = 1 TO 55
         INPUT #1, DEVICEs(i)
        NEXT i
        FOR i = 1 TO 56
         INPUT #1, SWITCHs(i)
        NEXT i
        FOR i = 1 TO 4
         INPUT #1, z
         IF i = 1 AND z = 31 THEN destruct = 1
         SHORT(i) = 10 ^ z
        NEXT i
        INPUT #1, HABleak
        INPUT #1, AYSEleak
        INPUT #1, windCEN
        INPUT #1, windACC
        INPUT #1, windANG
        INPUT #1, Zufo
        INPUT #1, Zref
        INPUT #1, Zrefalt
        INPUT #1, Zangle
        INPUT #1, Zorient
        INPUT #1, Zaccel
        INPUT #1, Ztarg
        INPUT #1, master
        CLOSE #1
        IF destruct = 1 THEN source(1, 2) = 1: source(2, 2) = 1: source(3, 2) = 1: source(4, 2) = 1: source(5, 2) = 1: HABexploded = 0: AYSEexploded = 0
        RETURN

810     OPEN "I", #1, zpath$ + "OSbackup"
        INPUT #1, z$
        INPUT #1, eng, vflag, Aflag, Sflag, SYSflag
        INPUT #1, mag, Sangle, cen, targ, ref
        INPUT #1, trail, tr, dte, ts, Eflag
        INPUT #1, year, day, hr, min, sec
        INPUT #1, z, z, z, z, z, z                         'AYSEad, AYSEdl, AYSEdla, AYSEtr, AYSEadFLAG, AYSE
        INPUT #1, z, MODULEflag, AYSEdist, OCESSdist, z, z
        INPUT #1, z, z, z, z, Pressure, Accel#
        CLOSE #1
        OLDtime# = NEWtime#
        NEWtime# = sec + (min * 60#) + (hr * 3600#) + (day * 24# * 3600#) + (year * 365# * 24# * 3600#)
        DELtime# = NEWtime# - OLDtime#
        IF DELtime# < 0 THEN DELtime# = 0
        IF OLDtime# = 0 THEN DELtime# = 0
        eng = ABS(eng)
        RETURN

820     HABengineMAX = 0
        FOR i = 1 TO 4
         IF switch(5 + (2 * i), 5) > 0 THEN c1 = Vbus1 / 10000 ELSE c1 = 0
         IF switch(4 + (2 * i), 5) > 0 THEN c2 = Vbus1 / 10000 ELSE c2 = 0
         IF c1 * c2 > 0 THEN HABengineMAX = HABengineMAX + (c1 * .25)
        NEXT i
        HABengineMAX = HABengineMAX * ((switch(34, 5) * .5) + (switch(35, 5) * .5))
       
        AYSEengineMAX = 0
        FOR i = 1 TO 4
         IF switch(28 + i, 5) > 0 THEN AYSEengineMAX = AYSEengineMAX + (.25 * Vbus4 / 100000)
        NEXT i
        COLOR 14, 0
      
        IF switch(1, 6) > 0 THEN current1 = Vbus1 / switch(1, 6) ELSE current = 0
        IF switch(2, 6) > 0 THEN current2 = Vbus1 / switch(2, 6) ELSE current = 0
        RADmax = (current1 + current2) / 1477000.14#
       
        IF AYSE = 150 THEN ENGINEforceMAX = 64000000 * AYSEengineMAX ELSE ENGINEforceMAX = 175000 * HABengineMAX
      
        IF switch(19, 5) = 1 AND Vbus1 > 8333 THEN RSCPop = 1 ELSE RSCPop = 0
        NAVmode = 0
        IF DEVICEs(55) = 2 THEN NAVmode = 1
        IF master = 1 THEN tsLIMIT = 1 ELSE tsLIMIT = 0
        module = 0
        IF switch(48, 5) = 1 THEN module = 2
        IF switch(49, 5) = 1 THEN module = 1

        HABexplosion = 0
        AYSEexplosion = 0
        IF source(1, 2) = 1 AND HABexploded = 0 THEN HABexplosion = 1
        IF source(5, 2) = 1 AND AYSEexploded = 0 THEN AYSEexplosion = 1
        IF source(1, 2) = 1 THEN HABexploded = 1 ELSE HABexploded = 0
        IF source(5, 2) = 1 THEN AYSEexploded = 1 ELSE AYSEexploded = 0
       

        OPEN "O", #1, zpath$ + "ORBITSse"
        PRINT #1, NAVmode
        PRINT #1, ENGINEforceMAX
        PRINT #1, HABfuel#
        PRINT #1, AYSEfuel#
        PRINT #1, HABexplosion
        PRINT #1, AYSEexplosion
        PRINT #1, module                'release / recapture
        PRINT #1, tsLIMIT
        PRINT #1, RADmax
        PRINT #1, RSCPop
        PRINT #1, AYSEad
        PRINT #1, AYSEdl
        PRINT #1, AYSE
        PRINT #1, windCEN
        PRINT #1, windACC
        PRINT #1, windANG
        PRINT #1, Zufo
        PRINT #1, Zref
        PRINT #1, Zrefalt
        PRINT #1, Zangle
        PRINT #1, Zorient
        PRINT #1, Zaccel
        PRINT #1, Ztarg
        PRINT #1, Htemp
        PRINT #1, Atemp
        PRINT #1, FCtemp
        PRINT #1, BATT1temp
        PRINT #1, BATT3temp
        PRINT #1, Is1
        PRINT #1, Is2
        PRINT #1, Is3
        PRINT #1, Is4
        PRINT #1, Is5
        PRINT #1, Vbus1
        PRINT #1, Vbus2
        PRINT #1, Vbus3
        PRINT #1, Vbus4
        PRINT #1, Vbus5
        PRINT #1, Ibus1
        PRINT #1, Ibus2
        PRINT #1, Ibus3
        PRINT #1, Ibus4
        PRINT #1, Ibus5
        FOR i = 0 TO 56
         PRINT #1, switch(i, 5)
        NEXT i
        PRINT #1, Rshield
        PRINT #1, Agrav
        PRINT #1, eng
        PRINT #1, master
        CLOSE #1
        RETURN


830     OPEN "I", #1, zpath$ + "ORBITSse"
        INPUT #1, NAVmode
        INPUT #1, ENGINEforceMAX
        INPUT #1, HABfuel#
        INPUT #1, AYSEfuel#
        INPUT #1, HABexplosion
        INPUT #1, AYSEexplosion
        INPUT #1, module                'release / recapture
        INPUT #1, tsLIMIT
        INPUT #1, RADmax
        INPUT #1, RSCPop
        INPUT #1, AYSEad
        INPUT #1, AYSEdl
        INPUT #1, AYSE
        INPUT #1, windCEN
        INPUT #1, windACC
        INPUT #1, windANG
        INPUT #1, Zufo
        INPUT #1, Zref
        INPUT #1, Zrefalt
        INPUT #1, Zangle
        INPUT #1, Zorient
        INPUT #1, Zaccel
        INPUT #1, Ztarg
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
        FOR i = 0 TO 56
         INPUT #1, switch(i, 5)
        NEXT i
        INPUT #1, Rshield
        INPUT #1, Agrav
        INPUT #1, eng
        INPUT #1, master
        CLOSE #1
        RETURN


1000    DATA 122,  7, 18, 19,""
       
        DATA  97,  5, 15, 16,""
        DATA  98,  5, 22, 23,""  
        DATA  99,  5, 29, 30,""
        DATA 100,  5, 37, 38,""
        DATA 101,  5, 44, 45,""
        DATA 102,  5, 56, 57,""
        DATA 103,  3, 59, 60,""
        DATA 104,  5, 62, 63,""
        DATA 105,  3, 65, 66,""
        DATA 106,  5, 68, 69,""
                           
        DATA 107,  3, 71, 72,""
        DATA 108,  5, 74, 75,""
        DATA 109,  3, 77, 78,""
        DATA 110, 11, 25, 26,""
        DATA 111,  9, 25, 26,""   
        DATA 112,  9, 47, 48,""
        DATA 113,  7, 25, 26,""
        DATA 114, 11, 34, 35,""
        DATA 115,  5, 51, 52,""
        DATA 116,  9, 29, 30,""
       
        DATA 117,  9, 34, 35,""
        DATA 118,  9, 39, 40,""
        DATA 119,  9, 44, 45,""
        DATA   1,  7, 78, 76,"F1"
        DATA   2,  9, 78, 76,"F2"
        DATA   3, 15, 26, 27,"F3"
        DATA   4, 15, 41, 42,"F4"
        DATA   5, 15, 48, 49,"F5"
        DATA   6, 15, 56, 57,"F6"
        DATA   7, 15, 61, 62,"F7"
     
        DATA   8, 15, 66, 67,"F8"
        DATA   9, 15, 71, 72,"F9"
        DATA  10, 15, 79, 76,"F10"
        DATA  49,  3,  3,  "INJECTOR 1"
        DATA  50,  4,  3,  "INJECTOR 2"
        DATA  51,  9,  3,  "DUMP"
        DATA  52,  9,  9,  "LOAD"
        DATA  53, 10,  3,  "REACT INJ1"
        DATA  54, 11,  3,  "REACT INJ2"
        DATA  55, 12,  3,  "F-CELL INJ"
       
        DATA  56, 18,  3,  "DUMP"
        DATA  57, 18,  9,  "LOAD"
        DATA  91, 19,  3,  "REACT INJ1"
        DATA  93, 20,  3,  "REACT INJ2"
        DATA 121,  3, 29,  ""
        DATA 120, 11, 15,  "TEMP"
        DATA  48, 19, 15,  "TEMP"
        DATA  80, 22, 16,  ""
        DATA  81, 23, 16,  ""
        DATA  82, 25, 16,  ""
       
        DATA  83, 22, 34,  ""
        DATA  84, 23, 34,  ""
        DATA  85, 24, 34,  ""
        DATA 150, 22, 56,  ""
        DATA 151, 24, 56,  ""
        DATA  61, 15, 18, 19,""

        
2000    IF ERL = 800 THEN CLOSE #1: GOTO 2010
        IF ERL = 810 THEN CLOSE #1: GOTO 2020
        IF ERL = 820 THEN CLOSE #1: GOTO 2030
        IF ERL = 830 THEN CLOSE #1: GOTO 2040
        LOCATE 25, 1: PRINT ERR, ERL;
        END



2010    tupload = TIMER
2011    IF TIMER - tupload < .5 THEN 2011
        RESUME 800

2020    tupload = TIMER
2021    IF TIMER - tupload < .5 THEN 2021
        RESUME 810

2030    tupload = TIMER
2031    IF TIMER - tupload < .5 THEN 2031
        RESUME 820

2040    tupload = TIMER
2041    IF TIMER - tupload < .5 THEN 2041
        RESUME 830

6000    FOR snd = 1000 TO 2000 STEP 15
         SOUND snd, .05
        NEXT snd
        RETURN

