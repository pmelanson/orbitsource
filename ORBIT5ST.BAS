        DEFDBL A-Z
        DIM P(39, 11), Px(39, 5), Py(39, 5), Vx(39), Vy(39), b(1, 250), Ztel(50), nme$(39), clr(4), switch(60, 5)
        ON ERROR GOTO 5000
        CLS
        clr(0) = 10
        clr(1) = 11
        clr(2) = 14
        clr(3) = 13
        clr(4) = 12
97      LOCATE 5, 5
        PRINT "Telemetry Monitoring Utility for ORBIT v. 5O"
        INPUT ; "    path to main program: ", zpath$
        IF UCASE$(zpath$) = "QUIT" THEN END
        IF zpath$ = "*" THEN zpath$ = "\\Pavilion\c\qbasic\"
98      OPEN "I", #1, zpath$ + "OSBACKUP"
        CLOSE #1
        destruct$ = "nastyratsnest"
        CLS

        'Initialization Parameters
        'Radius P(i,5) and Mass P(i,4) of solar system objects
        P(0, 5) = 6.96E+08:      P(0, 4) = 1.989E+30
        P(1, 5) = 4878000 / 2:   P(1, 4) = 3.32E+23
        P(2, 5) = 12104000 / 2:  P(2, 4) = 4.87E+24
        P(3, 5) = 12576000 / 2:  P(3, 4) = 5.98E+24
        P(4, 5) = 6787000 / 2:   P(4, 4) = 6.42E+23
        P(5, 5) = 143800000 / 2: P(5, 4) = 1.9E+27
        P(6, 5) = 120660000 / 2: P(6, 4) = 5.69E+26
        P(7, 5) = 52290000 / 2:  P(7, 4) = 8.69E+25
        P(8, 5) = 49500000 / 2:  P(8, 4) = 1.03E+26
        P(9, 5) = 3000000 / 2:   P(9, 4) = 1.1E+22
        P(10, 5) = 3476000 / 2:  P(10, 4) = 3000 * 4 * 3.14159265# * P(10, 5) * P(10, 5) * P(10, 5) / 3
        P(11, 5) = 11000:        P(11, 4) = 3000 * 4 * 3.14159265# * P(11, 5) * P(11, 5) * P(11, 5) / 3
        P(12, 5) = 6200:         P(12, 4) = 3000 * 4 * 3.14159265# * P(12, 5) * P(12, 5) * P(12, 5) / 3
        P(13, 5) = 1820000:      P(13, 4) = P(5, 4) * .000047
        P(14, 5) = 1500000:      P(14, 4) = P(5, 4) * .000026
        P(15, 5) = 2640000:      P(15, 4) = P(5, 4) * .000078
        P(16, 5) = 2500000:      P(16, 4) = P(5, 4) * .000056
        P(17, 5) = 530000:       P(17, 4) = P(6, 4) * .0000013
        P(18, 5) = 560000:       P(18, 4) = P(6, 4) * .00000185#
        P(19, 5) = 765000:       P(19, 4) = P(6, 4) * .0000044
        P(20, 5) = 2560000:      P(20, 4) = P(6, 4) * .000236
        P(21, 5) = 730000:       P(21, 4) = P(6, 4) * .0000033
        P(22, 5) = 665000:       P(22, 4) = 3000 * 4 * 3.14159265# * P(22, 5) * P(22, 5) * P(22, 5) / 3
        P(23, 5) = 555000:       P(23, 4) = 3000 * 4 * 3.14159265# * P(23, 5) * P(23, 5) * P(23, 5) / 3
        P(24, 5) = 800000:       P(24, 4) = 3000 * 4 * 3.14159265# * P(24, 5) * P(24, 5) * P(24, 5) / 3
        P(25, 5) = 815000:       P(25, 4) = 3000 * 4 * 3.14159265# * P(25, 5) * P(25, 5) * P(25, 5) / 3
        P(26, 5) = 1700000:      P(26, 4) = 3000 * 4 * 3.14159265# * P(26, 5) * P(26, 5) * P(26, 5) / 3
        P(27, 5) = 1300000:      P(27, 4) = .0002 * P(3, 4)
        P(28, 5) = 80:           P(28, 4) = 100000
        P(29, 5) = 40000 / 2:    P(29, 4) = 1E+17
        P(30, 5) = 1400 / 2:     P(30, 4) = 4.31E+12
        P(31, 5) = 960000 / 2:   P(31, 4) = 8.7E+20
        P(32, 5) = 500:          P(31, 4) = 1000000
        P(33, 5) = 1800000:      P(33, 4) = 1255 * 4 * 3.14159265# * P(33, 5) * P(33, 5) * P(33, 5) / 3
        P(34, 5) = 1287000:      P(34, 4) = 1955 * 4 * 3.14159265# * P(34, 5) * P(34, 5) * P(34, 5) / 3
        P(35, 5) = 500:          P(35, 4) = 1000000
        P(36, 5) = 70
      

        AU = 149597890000#
        RAD = 57.295779515#
        G = 6.673E-11
        Px(0, 3) = 0
        Py(0, 3) = 0
        Vx(0) = 0
        Vy(0) = 0
        P(0, 1) = 0
        P(0, 2) = 0
        RESTORE 1000
        FOR i = 0 TO 39
         READ nme$(i)
        NEXT i
        GOSUB 800
        GOSUB 400
        tt = TIMER

        'MAIN PROGRAM
1       z$ = INKEY$
        IF z$ = CHR$(13) THEN RUN "orbit5sz"
        IF AUTOdestruct = 1 THEN DESTRUCTcode$ = DESTRUCTcode$ + z$
        IF LEN(DESTRUCTcode$) > 13 THEN AUTOdestruct = 0: DESTRUCTcode$ = ""
        IF z$ = CHR$(0) + ";" THEN AUTOdestruct = 1 - ABS(AUTOdestruct): DESTRUCTcode$ = ""
        IF z$ = CHR$(27) THEN END
        COLOR 7, 0
        IF z$ < "0" OR z$ > "U" THEN 2
        IF z$ = "T" AND MODULEflag = 0 THEN 2
        alt = ASC(z$) - 48
2       IF TIMER - tt >= 5 THEN GOSUB 800: GOSUB 900: tt = TIMER
        LOCATE 2, 5
        IF AUTOdestruct = 1 THEN COLOR 12, 0: PRINT "DESTRUCT CODE"; : COLOR 15, 0 ELSE PRINT "             ";
        LOCATE 3, 5
        IF AUTOdestruct = 0 THEN PRINT "             ";
        IF AUTOdestruct = 1 THEN COLOR 12, 0: PRINT STRING$(LEN(DESTRUCTcode$), 42); STRING$(LEN(destruct$) - LEN(DESTRUCTcode$), 219);
        GOTO 1


        'SUBROUTINE: Calculate orbit parameters
        'Calculate gravitaional attraction to reference object
300     difx = Px(28, 3) - Px(ref, 3)
        dify = Py(28, 3) - Py(ref, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        A = G * P(ref, 4) / (R ^ 2)
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Vref = SQR(G * P(ref, 4) / R)
        Aref = angle
        Dref = R
        ACCref = A
        
        'Calculate gravitational attraction to target object
        difx = Px(28, 3) - Px(targ, 3)
        dify = Py(28, 3) - Py(targ, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        A = G * P(targ, 4) / (R ^ 2)
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Vtarg = SQR(G * P(targ, 4) / R)
        Atarg = angle
        Dtarg = R
        ACCtarg = A
       
        VVx = Vx(targ) - Vx(28)
        VVy = Vy(targ) - Vy(28)
        IF VVy = 0 THEN IF VVx < 0 THEN Vvangle = .5 * 3.1415926535# ELSE Vvangle = 1.5 * 3.1415926535# ELSE Vvangle = ATN(VVx / VVy)
        IF VVy > 0 THEN Vvangle = Vvangle + 3.1415926535#
        IF Vvangle < 0 THEN Vvangle = Vvangle + 6.283185
        VangleDIFF = Atarg - Vvangle


        'Calculate gravitational attraction to alternate object
        difx = Px(28, 3) - Px(alt, 3)
        dify = Py(28, 3) - Py(alt, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF R = 0 THEN A = 0 ELSE A = G * P(targ, 4) / (R ^ 2)
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        IF R = 0 THEN Valt = 0 ELSE Valt = SQR(G * P(targ, 4) / R)
        Aalt = angle
        Dalt = R
        ACCalt = A

        VVx = Vx(alt) - Vx(28)
        VVy = Vy(alt) - Vy(28)
        IF VVy = 0 THEN IF VVx < 0 THEN Vvangle = .5 * 3.1415926535# ELSE Vvangle = 1.5 * 3.1415926535# ELSE Vvangle = ATN(VVx / VVy)
        IF VVy > 0 THEN Vvangle = Vvangle + 3.1415926535#
        IF Vvangle < 0 THEN Vvangle = Vvangle + 6.283185
        VangleDIFF1 = Aalt - Vvangle


      
        'Calculate reference velocities
        Vhab = SQR(Vx(28) ^ 2 + Vy(28) ^ 2)
        Vrefhab = SQR((Vx(28) - Vx(ref)) ^ 2 + (Vy(28) - Vy(ref)) ^ 2)
        Vtarghab = SQR((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2)
        Valthab = SQR((Vx(28) - Vx(alt)) ^ 2 + (Vy(28) - Vy(alt)) ^ 2)
        AtoTARG = ACCtarg
        IF COS(VangleDIFF) <> 0 AND Dtarg - P(targ, 5) <> 0 THEN AtoTARG = AtoTARG + ((((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2) / (2 * (Dtarg - P(targ, 5)))) * COS(VangleDIFF))
        VcenTARG = SQR(((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2)) * -1 * COS(VangleDIFF)
        VtanTARG = SQR(((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2)) * ABS(SIN(VangleDIFF))
        AtoALT = ACCalt
        IF COS(VangleDIFF1) <> 0 AND Dalt - P(alt, 5) <> 0 THEN AtoALT = AtoALT + ((((Vx(28) - Vx(alt)) ^ 2 + (Vy(28) - Vy(alt)) ^ 2) / (2 * (Dalt - P(alt, 5)))) * COS(VangleDIFF1))
        VcenALT = SQR(((Vx(28) - Vx(alt)) ^ 2 + (Vy(28) - Vy(alt)) ^ 2)) * -1 * COS(VangleDIFF1)
        VtanALT = SQR(((Vx(28) - Vx(alt)) ^ 2 + (Vy(28) - Vy(alt)) ^ 2)) * ABS(SIN(VangleDIFF1))

        IF Sangle < 0 THEN Sangle = 6.2831852# + Sangle
       
        difx = Px(alt, 3) - Px(targ, 3)
        dify = Py(alt, 3) - Py(targ, 3)
        DaltTARG = SQR((dify ^ 2) + (difx ^ 2)) - P(alt, 5) - P(targ, 5)
        difx = Px(alt, 3) - Px(ref, 3)
        dify = Py(alt, 3) - Py(ref, 3)
        DaltREF = SQR((dify ^ 2) + (difx ^ 2)) - P(alt, 5) - P(ref, 5)
       
        'Calculate angle from target to reference object
        difx = Px(targ, 3) - Px(ref, 3)
        dify = Py(targ, 3) - Py(ref, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        atrh = ABS(angle - Aref)
        IF atrh > 3.1415926353# THEN atrh = ABS(atrh - 6.283185307#)
        IF targ = ref THEN atrh = 0
        RETURN



        'SUBROUTINE: Write telemetry data to screen
400     GOSUB 300
        IF master = 0 THEN COLOR 10, 1 ELSE COLOR 12, 4: BEEP
        LOCATE 5, 5: PRINT "MASTER ALARM";
        COLOR 15, 0
        COLOR 11: LOCATE 1, 25: PRINT "              REFERENCE          TARGET       ALTERNATE";
        LOCATE 2, 20: PRINT TAB(40); nme$(ref); TAB(56); nme$(targ); TAB(72); nme$(alt);
        COLOR 11: LOCATE 3, 25: PRINT "DISTANCE: ";
        COLOR 15
        Dref = Dref - P(ref, 5) - P(28, 5)
        Dtarg = Dtarg - P(targ, 5) - P(28, 5)
        Dalt = Dalt - P(alt, 5) - P(28, 5)
        PRINT TAB(35); : IF Dref > 99999999999# THEN PRINT USING "#.#######^^^^"; Dref / 1000;  ELSE PRINT USING "##,###,###.##"; Dref / 1000;
        PRINT TAB(50); : IF Dtarg > 99999999999# THEN PRINT USING "##.#######^^^^"; Dtarg / 1000;  ELSE PRINT USING "###,###,###.##"; Dtarg / 1000;
        PRINT TAB(66); : IF Dalt > 99999999999# THEN PRINT USING "##.#######^^^^"; Dalt / 1000;  ELSE PRINT USING "###,###,###.##"; Dalt / 1000;
        COLOR 11: LOCATE 4, 25: PRINT "VELOCITY: ";
        COLOR 15
        PRINT TAB(35); : IF Vrefhab > 99999999999# THEN PRINT USING "#.#######^^^^"; Vrefhab / 1000;  ELSE PRINT USING "##,###,###.##"; Vrefhab / 1000;
        PRINT TAB(50); : IF Vtarghab > 99999999999# THEN PRINT USING "##.#######^^^^"; Vtarghab / 1000;  ELSE PRINT USING "###,###,###.##"; Vtarghab / 1000;
        PRINT TAB(66); : IF Valthab > 99999999999# THEN PRINT USING "##.#######^^^^"; Vlathab / 1000;  ELSE PRINT USING "###,###,###.##"; Valthab / 1000;
        COLOR 11: LOCATE 5, 28: PRINT "  CEN:";
        COLOR 15
        PRINT TAB(50); : IF VcenTARG > 99999999999# THEN PRINT USING "##.#######^^^^"; VcenTARG / 1000;  ELSE PRINT USING "###,###,###.##"; VcenTARG / 1000;
        PRINT TAB(66); : IF VcenALT > 99999999999# THEN PRINT USING "##.#######^^^^"; VcenALT / 1000;  ELSE PRINT USING "###,###,###.##"; VcenALT / 1000;
        COLOR 11: LOCATE 6, 28: PRINT "  TAN:";
        COLOR 15
        PRINT TAB(50); : IF VtanTARG > 99999999999# THEN PRINT USING "##.#######^^^^"; VtanTARG / 1000;  ELSE PRINT USING "###,###,###.##"; VtanTARG / 1000;
        PRINT TAB(66); : IF VtanALT > 99999999999# THEN PRINT USING "##.#######^^^^"; VtanALT / 1000;  ELSE PRINT USING "###,###,###.##"; VtanALT / 1000;
        COLOR 11: LOCATE 7, 25: PRINT "ANGLE:    ";
        COLOR 15
        PRINT TAB(42); : PRINT USING "###.##"; Aref * RAD;
        COLOR 11: PRINT TAB(49); "("; : COLOR 15: PRINT USING "###.##"; atrh * RAD; : COLOR 11: PRINT ")"; : COLOR 15
        PRINT TAB(58); : PRINT USING "###.##"; Atarg * RAD;
        PRINT TAB(74); : PRINT USING "###.##"; Aalt * RAD;
        COLOR 11: LOCATE 8, 25: PRINT "STOP ACC:  ";
        COLOR 15
        PRINT TAB(50); : IF AtoTARG > 999999999# THEN PRINT USING "##.#######^^^^"; AtoTARG;  ELSE PRINT USING "###,###,###.##"; AtoTARG;
        PRINT TAB(66); : IF AtoALT > 999999999# THEN PRINT USING "##.#######^^^^"; AtoALT;  ELSE PRINT USING "###,###,###.##"; AtoALT;
       
        IF AYSE = 150 THEN thrust = 64000000#: Hmass = fuel + AYSEfuel + 20275000 ELSE thrust = 175000: Hmass = fuel + 275000
        Haccel = (thrust * eng) / Hmass
        COLOR 11: LOCATE 9, 25: PRINT "ENGINE:   "; : COLOR 15: PRINT USING "####.###"; eng;
        COLOR 11: LOCATE 10, 25: PRINT "ACCEL:    "; : COLOR 15: PRINT USING "####.###"; Haccel;
        COLOR 11: LOCATE 11, 25: PRINT "H fuel:   "; : COLOR 15: PRINT USING "########"; fuel;
        COLOR 11: LOCATE 12, 25: PRINT "A fuel:   "; : COLOR 15: PRINT USING "########"; AYSEfuel;
        COLOR 11: LOCATE 9, 49: PRINT "HEADING:"; : COLOR 15: PRINT USING "####.##"; Sangle * RAD;
        COLOR 11: LOCATE 10, 50: PRINT "COURSE:"; : COLOR 15: PRINT USING "####.##"; Vvangle * RAD;
        COLOR 11: LOCATE 9, 66: PRINT "Dist. to REF";
        IF alt = ref THEN DaltREF = 0
        COLOR 15: LOCATE 10, 66: IF DaltREF > 99999999999# THEN PRINT USING "##.#######^^^^"; DaltREF / 1000;  ELSE PRINT USING "###,###,###.##"; (DaltREF) / 1000;
        COLOR 11: LOCATE 11, 66: PRINT "         TARG";
        IF alt = targ THEN DaltTARG = 0
        COLOR 15: LOCATE 12, 66: IF DaltTARG > 99999999999# THEN PRINT USING "##.#######^^^^"; DaltTARG / 1000;  ELSE PRINT USING "###,###,###.##"; (DaltTARG) / 1000;
        COLOR 11: LOCATE 13, 25: PRINT "NAV MODE: ";
        COLOR 15
          IF Sflag = 0 THEN PRINT "ccw prograde  "; : GOTO 401
          IF Sflag = 4 THEN PRINT "ccw retrograde"; : GOTO 401
          IF Sflag = 1 THEN PRINT "manual        "; : GOTO 401
          IF Sflag = 2 THEN PRINT "approach targ "; : GOTO 401
          IF Sflag = 5 THEN PRINT "pro Vtrg      "; : GOTO 401
          IF Sflag = 6 THEN PRINT "retr Vtrg     "; : GOTO 401
          IF Sflag = 3 THEN PRINT "depart ref    ";
401     COLOR 11: LOCATE 14, 25: PRINT "TIME:     "; : COLOR 15: PRINT USING "####:"; year; : LOCATE 14, 40: PRINT USING "###:"; day; : LOCATE 14, 44: PRINT USING "##:"; hr; min; : LOCATE 14, 50: PRINT USING "##"; sec;
        COLOR 11: LOCATE 15, 25: PRINT "F rate:     "; : COLOR 15: PRINT USING "##.##"; ts;
        LOCATE 18, 1: COLOR 9, 0: PRINT "0-U"; : COLOR 11, 0: PRINT "   Select Alt";
        LOCATE 19, 1: COLOR 9, 0: PRINT "enter"; : COLOR 11, 0: PRINT " Power Grid";
        LOCATE 20, 1: COLOR 9, 0: PRINT "F1"; : COLOR 11, 0: PRINT "    Destruct Code";
        LOCATE 21, 1: COLOR 9, 0: PRINT "F2"; : COLOR 11, 0: PRINT "    Transmit Code";
        RETURN


        'SUBROUTINE: Timed telemetry retrieval
800     OPEN "I", #1, zpath$ + "Osbackup"
801     IF EOF(1) THEN 803
        INPUT #1, z$
        IF z$ <> "ORBIT5S" THEN 803
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
        AYSEad = Ztel(21)
        AYSEdl = Ztel(22)
        AYSEdla = Ztel(23)
        AYSEtr = Ztel(24)
        AYSEadFLAG = Ztel(25)
        AYSE = Ztel(26)
        'master = Ztel(27)
        MODULEflag = Ztel(28)
        AYSEdist = Ztel(29)
        OCESSdist = Ztel(30)
        Px(0, 3) = 0
        Py(0, 3) = 0
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
        GOSUB 400
802     CLOSE #1
        GOTO 830
803     CLOSE #1
        GOTO 800


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

        COLOR 11, 0: LOCATE 18, 25: PRINT "BUS:"; : COLOR 14, 0: PRINT " VOLTS";
        LOCATE 19, 25: COLOR 11, 0: PRINT "  1:"; : COLOR 10, 0: PRINT USING "######"; Vbus1;
        LOCATE 20, 25: COLOR 11, 0: PRINT "  2:"; : COLOR 10, 0: PRINT USING "######"; Vbus2;
        LOCATE 21, 25: COLOR 11, 0: PRINT "  3:"; : COLOR 10, 0: PRINT USING "######"; Vbus3;
        LOCATE 22, 25: COLOR 11, 0: PRINT "  4:"; : COLOR 10, 0: PRINT USING "######"; Vbus4;
        COLOR 14, 0: LOCATE 18, 37: PRINT "CURRENT";
        LOCATE 19, 37: COLOR 10, 0: PRINT USING "#######"; Ibus1;
        LOCATE 20, 37: COLOR 10, 0: PRINT USING "#######"; Ibus2;
        LOCATE 21, 37: COLOR 10, 0: PRINT USING "#######"; Ibus3;
        LOCATE 22, 37: COLOR 10, 0: PRINT USING "#######"; Ibus4;
        LOCATE 18, 48: COLOR 11, 0: PRINT "SOURCE:"; : COLOR 14, 0: LOCATE 18, 56: PRINT "OUTPUT";
        LOCATE 19, 49: COLOR 11, 0: PRINT "H Rct:"; : COLOR 10, 0: PRINT USING "#######"; Is1;
        LOCATE 20, 49: COLOR 11, 0: PRINT "   FC:"; : COLOR 10, 0: PRINT USING "#######"; Is2;
        LOCATE 21, 49: COLOR 11, 0: PRINT " BAT1:"; : COLOR 10, 0: PRINT USING "#######"; Is3;
        LOCATE 23, 49: COLOR 11, 0: PRINT "A Rct:"; : COLOR 10, 0: PRINT USING "#######"; Is5;
        LOCATE 22, 49: COLOR 11, 0: PRINT " BAT3:"; : COLOR 10, 0: PRINT USING "#######"; Is4;
        COLOR 14, 0: LOCATE 18, 63: PRINT "TEMP";
        LOCATE 19, 63:  COLOR 10, 0: PRINT USING "####"; Htemp;
        LOCATE 20, 63:  COLOR 10, 0: PRINT USING "####"; FCtemp;
        LOCATE 21, 63:  COLOR 10, 0: PRINT USING "####"; BATT1temp;
        LOCATE 23, 63:  COLOR 10, 0: PRINT USING "####"; Atemp;
        LOCATE 22, 63:  COLOR 10, 0: PRINT USING "####"; BATT3temp;


        'COLOR 14, 0: LOCATE 18, 5: PRINT "BUS VOLTAGE:";
        'COLOR 9, 0: PRINT "    1:"; : COLOR 10, 0: PRINT USING "######"; Vbus1;
        'COLOR 9, 0: PRINT "    2:"; : COLOR 10, 0: PRINT USING "######"; Vbus2;
        'COLOR 9, 0: PRINT "    3:"; : COLOR 10, 0: PRINT USING "######"; Vbus3;
        'COLOR 9, 0: PRINT "    4:"; : COLOR 10, 0: PRINT USING "######"; Vbus4;
        'COLOR 14, 0: LOCATE 19, 5: PRINT "BUS CURRENT:";
        'COLOR 9, 0: PRINT "    1:"; : COLOR 10, 0: PRINT USING "######"; Ibus1;
        'COLOR 9, 0: PRINT "    2:"; : COLOR 10, 0: PRINT USING "######"; Ibus2;
        'COLOR 9, 0: PRINT "    3:"; : COLOR 10, 0: PRINT USING "######"; Ibus3;
        'COLOR 9, 0: PRINT "    4:"; : COLOR 10, 0: PRINT USING "######"; Ibus4;
        'COLOR 14, 0: LOCATE 20, 5: PRINT "OUTPUT:     ";
        'COLOR 9, 0: PRINT "   Rh:"; : COLOR 10, 0: PRINT USING "######"; Is1;
        'COLOR 9, 0: PRINT "   FC:"; : COLOR 10, 0: PRINT USING "######"; Is2;
        'COLOR 9, 0: PRINT " BAT1:"; : COLOR 10, 0: PRINT USING "######"; Is3;
        'COLOR 9, 0: PRINT "   Ra:"; : COLOR 10, 0: PRINT USING "######"; Is5;
        'COLOR 9, 0: PRINT " BAT3:"; : COLOR 10, 0: PRINT USING "######"; Is4;
        'COLOR 14, 0: LOCATE 21, 5: PRINT "TEMP:       ";
        'COLOR 9, 0: PRINT "   Rh:"; : COLOR 10, 0: PRINT USING "######"; Htemp;
        'COLOR 9, 0: PRINT "   FC:"; : COLOR 10, 0: PRINT USING "######"; FCtemp;
        'COLOR 9, 0: PRINT " BAT1:"; : COLOR 10, 0: PRINT USING "######"; BATT1temp;
        'COLOR 9, 0: PRINT "   Ra:"; : COLOR 10, 0: PRINT USING "######"; Atemp;
        'COLOR 9, 0: PRINT " BAT3:"; : COLOR 10, 0: PRINT USING "######"; BATT3temp;
       
        RETURN


900     IF AUTOdestruct = -1 THEN 902
        OPEN "O", #2, zpath$ + "ORBITMC"
        IF DESTRUCTcode$ <> destruct$ THEN PRINT #2, "*$&#@@##@": GOTO 901
        IF DESTRUCTcode$ = destruct$ THEN PRINT #2, "*$&##@@#@"
        COLOR 12, 0
        LOCATE 3, 5
        PRINT "TRANSMITTED  ";
        AUTOdestruct = -1
        DESTRUCTcode$ = ""
901     CLOSE #2
902     RETURN


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
        DATA "   Ceres"
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


5000    IF ERL = 800 OR ERL = 801 THEN tt = TIMER + .5: RESUME 803
        IF ERL = 900 THEN tt = TIMER + .5: RESUME 901
        IF ERL = 98 AND ERR = 76 THEN CLS : LOCATE 15, 5: PRINT "'"; zpath$; "' Path not found"; : RESUME 97
        IF ERL = 98 AND ERR = 53 THEN CLS : LOCATE 15, 5: PRINT "Backup file not found in '"; zpath$; "'"; : RESUME 97
        IF ERL = 830 THEN 5100
        LOCATE 1, 30: PRINT "unknown error: "; ERR; ERL
        z$ = INPUT$(1)
        END

5100    tUP = TIMER
5101    IF TIMER - tUP < .1 THEN 5101
        CLOSE #1
        RESUME 830

