1111    ON ERROR GOTO 9000
        SCREEN 12
        PALETTE 8, 19 + (19 * 256) + (19 * 65536)
        DEFDBL A-Z
        DIM P(39, 11), Px(39, 5), Py(39, 5), Vx(39), Vy(39), B(1, 250), Ztel(46), Znme$(39)
        DIM Pz(3021, 2) AS SINGLE
91      OPEN "I", #1, "starsr"
        FOR i = 1 TO 3021
         INPUT #1, Pz(i, 0)
         INPUT #1, Pz(i, 1)
         INPUT #1, Pz(i, 2)
        NEXT i
        FOR i = 1 TO 241
         INPUT #1, B(0, i)
         INPUT #1, B(1, i)
        NEXT i
        FOR i = 0 TO 39
         INPUT #1, P(i, 0)
         INPUT #1, P(i, 4)
         INPUT #1, P(i, 5)
         INPUT #1, P(i, 8)
         INPUT #1, P(i, 9)
         INPUT #1, P(i, 10)
        NEXT i
        INPUT #1, year, day, HR, min, sec
        FOR i = 0 TO 35
         INPUT #1, Px(i, 3), Py(i, 3), Vx(i), Vy(i), P(i, 1), P(i, 2)
        NEXT i
        FOR i = 0 TO 39
         INPUT #1, Znme$(i)
        NEXT i
        Px(37, 3) = 4446370.8284487# + Px(3, 3): Py(37, 3) = 4446370.8284487# + Py(3, 3): Vx(37) = Vx(3): Vy(37) = Vy(3)
        CLOSE #1

        'System variables
99      eng = 0:    vflag = 0:  Aflag = 0:   Sflag = 0:  SYSflag = 0
        mag = 25:   Sangle = 0: cen = 0:     targ = 0:   ref = 3
        trail = 1:  tr = 0:     dte = 0:     ts = .25:   Eflag = 0
        ID = 0:     AG = 0:     DH = 0:      DHL = 0:    DHLA = 0
        AYSEad = 0: AYSEdl = 0: AYSEdla = 0: AYSEtr = 0: AYSEadFLAG = 0
        TTC = 1:    GPD = 1:    Agen = 1:    Acon1 = 1:  Acon2 = 1:     Abr = 1
        Fcon1 = 1:  Fcon2 = 1:  ion = 1:     acc = 1:    Hbr = 1
        HT = 0:     AT = 0:     master = 0:  MODULEflag = 0
        fuel = 100000: AYSEfuel = 15120000
        AU = 149597890000#
        RAD = 57.295779515#
        G = 6.673E-11
        Px(0, 3) = 0
        Py(0, 3) = 0
        Vx(0) = 0
        Vy(0) = 0
        P(0, 1) = 0
        P(0, 2) = 0
        GUIDO$ = ""
    
        CLS
97      LOCATE 5, 5
        PRINT "Telemetry Echoing Utility for ORBIT v. 5O"
        PRINT
        Zpath$ = ""
        OPEN "I", #1, Zpath$ + "OSBACKUP"
        CLOSE #1
        GOSUB 800
        CLS
        GOSUB 400

        'Initialize frame rate timer
100     tttt = TIMER
              
        'Zero acceleration variables
        FOR i = 0 TO 35: P(i, 1) = 0: P(i, 2) = 0: NEXT i
        P(38, 1) = 0
        P(38, 2) = 0
        P(39, 1) = 0
        P(39, 2) = 0
       
        'Erase target vector
        IF dispFLAG = 0 THEN LINE (30, 120)-(30 + (20 * SIN(Atarg)), 120 + (20 * COS(Atarg))), 0
        IF SQR(((Px(28, 3) - cenX) * mag / AU) ^ 2 + ((Py(28, 3) - cenY) * mag * 1 / AU) ^ 2) > 400 THEN 131
        IF vflag = 1 THEN LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (30 * SIN(Atarg)) + (Px(28, 3) - cenX) * mag / AU, 220 + (30 * COS(Atarg)) + (Py(28, 3) - cenY) * mag * 1 / AU), 0
        
131     CONflag = 0
        atm = 0
        'Calculate gravitational acceleration for each object pair
        FOR i = 1 TO 166 + (ufo1 * 38) + (ufo2 * 37)
         IF B(1, i) = 32 AND AYSE = 150 THEN 106
         difx = Px(B(1, i), 3) - Px(B(0, i), 3)
         dify = Py(B(1, i), 3) - Py(B(0, i), 3)
         R = SQR((dify ^ 2) + (difx ^ 2))
         IF R < .01 THEN 106
         a = G * P(B(0, i), 4) / (R ^ 2)
         IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
         IF dify > 0 THEN angle = angle + 3.1415926535#
         IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
         P(B(1, i), 1) = P(B(1, i), 1) + (a * SIN(angle))
         P(B(1, i), 2) = P(B(1, i), 2) + (a * COS(angle))
         IF B(1, i) = 32 AND R <= P(B(0, i), 5) + P(32, 5) THEN CONflag2 = 1: CONflag3 = B(0, i)
         IF B(1, i) = 28 AND P(B(0, i), 10) > -150 AND R <= P(B(0, i), 5) + P(28, 5) THEN CONflag = 1: CONtarg = B(0, i): Dcon = R: Acon = angle: CONacc = a
         IF B(1, i) = 28 AND B(0, i) <> 32 AND R <= P(B(0, i), 5) + (1000 * P(B(0, i), 10)) THEN atm = B(0, i): Ratm = (R - P(B(0, i), 5)) / 1000
         IF B(0, i) = ref AND B(1, i) = 28 THEN Vref = SQR(G * P(B(0, i), 4) / R): Aref = angle
         IF B(0, i) = targ AND B(1, i) = 28 THEN Atarg = angle: Dtarg = R: Acctarg = a
         IF i = 156 AND R < 2100000 AND eng + warp = 0 THEN sednaFLAG = 1
106     NEXT i
       

        'Calculate angle from target to reference object
114     IF targ = ref THEN Atargref = 0: GOTO 101
        difx = Px(targ, 3) - Px(ref, 3)
        dify = Py(targ, 3) - Py(ref, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Atargref = ABS(angle - Aref)
        IF Atargref > 3.1415926535# THEN Atargref = 6.283185307# - Atargref

        'Record old center position
101     cenX = Px(cen, 3) + cenXoff
        cenY = Py(cen, 3) + cenYoff
       
        'Erase velocity, approach velocity, and orientation vectors
        IF dispFLAG = 0 THEN LINE (30, 120)-(30 + (5 * SIN(Sangle)), 120 + (5 * COS(Sangle))), 0
        IF dispFLAG = 0 THEN LINE (30, 120)-(30 + (10 * SIN(Vvangle)), 120 + (10 * COS(Vvangle))), 0
        IF SQR(((Px(28, 3) - cenX) * mag / AU) ^ 2 + ((Py(28, 3) - cenY) * mag * 1 / AU) ^ 2) > 400 THEN 132
        IF vflag = 1 THEN LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (20 * SIN(Vvangle)) + (Px(28, 3) - cenX) * mag / AU, 220 + (20 * COS(Vvangle)) + (Py(28, 3) - cenY) * mag * 1 / AU), 0
       
        'Update object velocities and erase old positions
132     FOR i = 37 + ufo1 + ufo2 TO 0 STEP -1
         IF i = 28 THEN GOSUB 301
         'IF i = 38 THEN GOSUB 7200
         Vx(i) = Vx(i) + (P(i, 1) * ts)
         Vy(i) = Vy(i) + (P(i, 2) * ts)
117      IF i = 36 AND MODULEflag = 0 THEN 108
         IF SQR(((Px(i, 3) - cenX) * mag / AU) ^ 2 + ((Py(i, 3) - cenY) * mag * 1 / AU) ^ 2) - (P(i, 5) * mag / AU) > 400 THEN 108
         IF cen * tr > 0 THEN 108
         IF mag * P(i, 5) / AU > 3200 THEN 118
         IF mag * P(i, 5) / AU < 1.1 THEN PSET (300 + (Px(i, 3) - cenX) * mag / AU, 220 + (Py(i, 3) - cenY) * mag * 1 / AU), 8 * trail: GOTO 108
         clr = 8 * trail
         IF i = 28 THEN vnSA = oldSA: GOSUB 128: GOTO 108
         IF i = 35 THEN GOSUB 138: GOTO 108
         IF i = 37 THEN GOSUB 148: GOTO 108
         CIRCLE (300 + (Px(i, 3) - cenX) * mag / AU, 220 + (Py(i, 3) - cenY) * mag * 1 / AU), mag * P(i, 5) / AU, 8 * trail: GOTO 108

118      difx = Px(i, 3) - cenX
         dify = Py(i, 3) - cenY
         R = SQR((dify ^ 2) + (difx ^ 2)) - P(i, 5)
         IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
         IF dify > 0 THEN angle = angle + 3.1415926535#
         IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
         dist = (SQR((cenX - Px(i, 3)) ^ 2 + (cenY - Py(i, 3)) ^ 2) - P(i, 5)) * mag / AU
         csz = COS(angle - 1.5707964#)
         snz = SIN(angle - 1.5707964#)
         LINE (300 - (dist * csz) - (300 * snz), 220 + (dist * snz) - (300 * csz))-(300 - (dist * csz) + (300 * snz), 220 + (dist * snz) + (300 * csz)), 8 * trail
108     NEXT i
        GOTO 102
       
         'Paint objects with a shape
         'Paint Habitat
128      CIRCLE (300 + (Px(i, 3) - cenX) * mag / AU, 220 + (Py(i, 3) - cenY) * mag * 1 / AU), mag * P(i, 5) / AU, clr
         CIRCLE (300 + (Px(i, 3) - cenX - (P(i, 5) * .8 * SIN(vnSA))) * mag / AU, 220 + (Py(i, 3) - cenY - (P(i, 5) * .8 * COS(vnSA))) * mag * 1 / AU), mag * P(i, 5) * .2 / AU, clr
         CIRCLE (300 + (Px(i, 3) - cenX - (P(i, 5) * 1.2 * SIN(vnSA + .84))) * mag / AU, 220 + (Py(i, 3) - cenY - (P(i, 5) * 1.2 * COS(vnSA + .84))) * mag * 1 / AU), mag * P(i, 5) * .2 / AU, clr
         CIRCLE (300 + (Px(i, 3) - cenX - (P(i, 5) * 1.2 * SIN(vnSA - .84))) * mag / AU, 220 + (Py(i, 3) - cenY - (P(i, 5) * 1.2 * COS(vnSA - .84))) * mag * 1 / AU), mag * P(i, 5) * .2 / AU, clr
         RETURN

         'Paint ISS
138      LINE (300 + (Px(i, 3) - cenX - 450) * mag / AU, 220 + (Py(i, 3) - cenY - 150) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 450) * mag / AU, 220 + (Py(i, 3) - cenY + 150) * mag * 1 / AU), clr, B
         LINE (300 + (Px(i, 3) - cenX + 500) * mag / AU, 220 + (Py(i, 3) - cenY + 25) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 450) * mag / AU, 220 + (Py(i, 3) - cenY - 25) * mag * 1 / AU), clr, B
         LINE (300 + (Px(i, 3) - cenX - 500) * mag / AU, 220 + (Py(i, 3) - cenY + 25) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX - 450) * mag / AU, 220 + (Py(i, 3) - cenY - 25) * mag * 1 / AU), clr, B
         LINE (300 + (Px(i, 3) - cenX - 350) * mag / AU, 220 + (Py(i, 3) - cenY + 200) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 350) * mag / AU, 220 + (Py(i, 3) - cenY + 355) * mag * 1 / AU), clr, B
         LINE (300 + (Px(i, 3) - cenX - 350) * mag / AU, 220 + (Py(i, 3) - cenY - 200) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 350) * mag / AU, 220 + (Py(i, 3) - cenY - 355) * mag * 1 / AU), clr, B
         LINE (300 + (Px(i, 3) - cenX - 50) * mag / AU, 220 + (Py(i, 3) - cenY + 200) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 50) * mag / AU, 220 + (Py(i, 3) - cenY + 150) * mag * 1 / AU), clr, B
         LINE (300 + (Px(i, 3) - cenX - 50) * mag / AU, 220 + (Py(i, 3) - cenY - 200) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 50) * mag / AU, 220 + (Py(i, 3) - cenY - 150) * mag * 1 / AU), clr, B
         RETURN

         'Paint OCESS
148      px1 = ((Px(37, 3) + 97.5305) - cenX) * mag / AU: py1 = ((Py(37, 3) + 69.2462) - cenY) * mag / AU
         px2 = ((Px(37, 3) + 69.2462) - cenX) * mag / AU: py2 = ((Py(37, 3) + 97.5305) - cenY) * mag / AU
         px3 = ((Px(37, 3) + 21.2132) - cenX) * mag / AU: py3 = ((Py(37, 3) - 21.2132) - cenY) * mag / AU
         px4 = ((Px(37, 3) - 21.2132) - cenX) * mag / AU: py4 = ((Py(37, 3) + 21.2132) - cenY) * mag / AU
         px5 = ((Px(37, 3) - 55.1041) - cenX) * mag / AU: py5 = ((Py(37, 3) - 111.6726) - cenY) * mag / AU
         px6 = ((Px(37, 3) - 111.6726) - cenX) * mag / AU: py6 = ((Py(37, 3) - 55.1041) - cenY) * mag / AU
         LINE (300 + px1, 220 + py1)-(300 + px5, 220 + py5), clr
         LINE (300 + px5, 220 + py5)-(300 + px6, 220 + py6), clr
         LINE (300 + px6, 220 + py6)-(300 + px2, 220 + py2), clr
         LINE (300 + px2, 220 + py2)-(300 + px1, 220 + py1), clr
         LINE (300 + px1, 220 + py1)-(300 + px4, 220 + py4), clr
         LINE (300 + px4, 220 + py4)-(300 + px5, 220 + py5), clr
         LINE (300 + px6, 220 + py6)-(300 + px3, 220 + py3), clr
         LINE (300 + px3, 220 + py3)-(300 + px2, 220 + py2), clr
         RETURN

       
        'Detect contact with an object
102     IF CONflag = 0 THEN 112
        IF CONtarg > 37 THEN ufo2 = 0: explFLAG1 = 1: eng = 0: targ = ref: GOTO 112
        IF ((Dcon - P(CONtarg, 5) - P(28, 5)) <= 0) AND ((Sflag < 3) OR (Aacc < CONacc * 1.1)) THEN Vx(28) = Vx(CONtarg): Vy(28) = Vy(CONtarg)
        IF ((Dcon - P(CONtarg, 5) - P(28, 5)) > -.5) THEN GOTO 112
        eng = 0
        Px(28, 3) = Px(CONtarg, 3) + ((P(CONtarg, 5) + P(28, 5) - .1) * SIN(Acon + 3.1415926#))
        Py(28, 3) = Py(CONtarg, 3) + ((P(CONtarg, 5) + P(28, 5) - .1) * COS(Acon + 3.1415926#))
       
        'Docked with AYSE drive module
112     explFLAG2 = 0
        IF AYSE = 150 OR AYSEadFLAG = 1 THEN Vx(32) = Vx(28): Vy(32) = Vy(28)
        IF CONflag2 = 1 AND CONflag4 = 0 THEN CONflag4 = 1: explFLAG2 = 1
        IF CONflag2 = 1 AND CONflag3 < 38 THEN Vx(32) = Vx(CONflag3): Vy(32) = Vy(CONflag3)

        'Timed Telemetry Retreival
        IF bkt - TIMER > 120 THEN bkt = TIMER
        IF TELEupFLAG = 1 THEN bkt = TIMER
        IF bkt + 5 < TIMER THEN bkt = TIMER: GOSUB 800
       
       
        'Update object positions
        FOR i = 0 TO 37 + ufo1 + ufo2
         Px(i, 3) = Px(i, 3) + (Vx(i) * ts)
         Py(i, 3) = Py(i, 3) + (Vy(i) * ts)
        NEXT i
        IF ts > 10 THEN GOSUB 3100
        IF MODULEflag > 0 THEN Px(36, 3) = P(36, 1) + Px(MODULEflag, 3): Py(36, 3) = P(36, 2) + Py(MODULEflag, 3): Vx(36) = Vx(MODULEflag): Vy(36) = Vy(MODULEflag)
        Px(37, 3) = 4446370.8284487# + Px(3, 3): Py(37, 3) = 4446370.8284487# + Py(3, 3): Vx(37) = Vx(3): Vy(37) = Vy(3)
       
        'Record new center position
        cenX = Px(cen, 3) + cenXoff
        cenY = Py(cen, 3) + cenYoff
     
        'Dock with AYSE drive module
        IF targ <> 32 THEN AYSEad = 0
        IF AYSEad = 0 THEN 111
        IF eng > 0 AND AYSE = 0 AND AYSEadFLAG = 0 THEN AYSEad = 0: GOTO 111
        IF TTC * GPD * Agen * Abr = 0 THEN 111
        IF Dtarg > 1000000 THEN AYSEadFLAG = 0: AYSEad = 0: GOTO 111
        IF AYSEadFLAG = 0 THEN 111
        CONflag2 = 0
        CONflag4 = 0
        Px(32, 3) = Px(28, 3) + (Dtarg * .2475 * SIN(Atarg) / ts)
        Py(32, 3) = Py(28, 3) + (Dtarg * .2475 * COS(Atarg) / ts)
        IF Dtarg < 250 THEN AYSEadFLAG = 0: AYSEad = 2
         
        'Record telemetry to a file
111     IF TELEflag = 1 AND TIMER - TELEbk > 1 THEN TELEbk = TIMER: PRINT #2, Px(28, 3) - Px(ref, 3), Py(28, 3) - Py(ref, 3), Vrefhab
       
        'Repaint objects to the screen
        FOR i = 37 + ufo1 + ufo2 TO 0 STEP -1
         IF i = 36 AND MODULEflag = 0 THEN 109
         IF SQR(((Px(i, 3) - cenX) * mag / AU) ^ 2 + ((Py(i, 3) - cenY) * mag * 1 / AU) ^ 2) - (P(i, 5) * mag / AU) > 400 THEN 109
         IF mag * P(i, 5) / AU > 3200 THEN 119
         pld = 0
         IF i = 28 THEN pld = 2 * ABS(SGN(eng))
         IF mag * P(i, 5) / AU < 1.1 THEN PSET (300 + (Px(i, 3) - cenX) * mag / AU, 220 + (Py(i, 3) - cenY) * mag * 1 / AU), P(i, 0) + pld: GOTO 109
         IF i = 28 THEN clr = 12 + pld: vnSA = Sangle: GOSUB 128: GOTO 109
         IF i = 35 THEN clr = 12: GOSUB 138: GOTO 109
         IF i = 37 THEN clr = 12: GOSUB 148: GOTO 109
         CIRCLE (300 + (Px(i, 3) - cenX) * mag / AU, 220 + (Py(i, 3) - cenY) * mag * 1 / AU), mag * P(i, 5) / AU, P(i, 0) + pld: GOTO 109

119      difx = Px(i, 3) - cenX
         dify = Py(i, 3) - cenY
         R = SQR((dify ^ 2) + (difx ^ 2)) - P(i, 5)
         IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
         IF dify > 0 THEN angle = angle + 3.1415926535#
         IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
         dist = (SQR((cenX - Px(i, 3)) ^ 2 + (cenY - Py(i, 3)) ^ 2) - P(i, 5)) * mag / AU
         csz = COS(angle - 1.5707964#)
         snz = SIN(angle - 1.5707964#)
         LINE (300 - (dist * csz) - (300 * snz), 220 + (dist * snz) - (300 * csz))-(300 - (dist * csz) + (300 * snz), 220 + (dist * snz) + (300 * csz)), P(i, 0)

109     NEXT i
        oldSA = Sangle
      

        'Re-paint target vector
        IF dispFLAG = 0 THEN LINE (30, 120)-(30 + (20 * SIN(Atarg)), 120 + (20 * COS(Atarg))), 8
       
       
        'Repaint velocity and orientation vectors
        VVx = Vx(targ) - Vx(28)
        VVy = Vy(targ) - Vy(28)
        IF VVy = 0 THEN IF VVx < 0 THEN Vvangle = .5 * 3.1415926535# ELSE Vvangle = 1.5 * 3.1415926535# ELSE Vvangle = ATN(VVx / VVy)
        IF VVy > 0 THEN Vvangle = Vvangle + 3.1415926535#
        IF Vvangle < 0 THEN Vvangle = Vvangle + 6.283185
        IF telTRACK = 1 THEN GOSUB 6050

       
        IF dispFLAG = 0 THEN LINE (30, 120)-(30 + (10 * SIN(Vvangle)), 120 + (10 * COS(Vvangle))), 12
        IF dispFLAG = 0 THEN LINE (30, 120)-(30 + (5 * SIN(Sangle)), 120 + (5 * COS(Sangle))), 10
        IF dispFLAG = 0 THEN PSET (30, 120), 1
        IF SQR(((Px(28, 3) - cenX) * mag / AU) ^ 2 + ((Py(28, 3) - cenY) * mag * 1 / AU) ^ 2) > 400 THEN 133
        IF vflag = 1 THEN LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (30 * SIN(Atarg)) + (Px(28, 3) - cenX) * mag / AU, 220 + (30 * COS(Atarg)) + (Py(28, 3) - cenY) * mag * 1 / AU), 8
        IF vflag = 1 THEN LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (20 * SIN(Vvangle)) + (Px(28, 3) - cenX) * mag / AU, 220 + (20 * COS(Vvangle)) + (Py(28, 3) - cenY) * mag * 1 / AU), 12
        'IF vflag = 1 THEN LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (20 * SIN(VvangleREV)) + (Px(28, 3) - cenX) * mag / AU, 220 + (20 * COS(VvangleREV)) + (Py(28, 3) - cenY) * mag * 1 / AU), 14
133     VangleDIFF = Atarg - Vvangle

        'Refueling
        'rfl = AYSE
        'IF targ = 37 AND Dtarg < 400 THEN rfl = 1
        'IF rfl = 0 THEN refuel = 0
        'IF rfl = 150 AND AYSEfuel = 0 THEN refuel = 0
        'fuel = fuel + ((refuel * 100) * ts) - (((ventfuel * 100) + Ztel(29)) * ts)
        'IF rfl = 150 THEN AYSEfuel = AYSEfuel - (refuel * 100 * ts) + (ventfuel * 100 * ts)
        'IF fuel > 200000 THEN fuel = 200000: refuel = 0
        'IF fuel < 0 THEN fuel = 0: ventfuel = 0
        'IF AYSE < 150 THEN AYSErefuel = 0
        'AYSEfuel = AYSEfuel + (AYSErefuel * 10 * Pr * ts) - (((1000 * AYSEventfuel) + Ztel(30)) * ts)
        'IF AYSEfuel > 20000000 THEN AYSEfuel = 20000000
        'IF AYSEfuel < 0 THEN AYSEfuel = 0

        'Update simulation time
        sec = sec + ts
        IF sec > 60 THEN min = min + 1: sec = sec - 60
        IF dte = 1 AND ts < 60 THEN LOCATE 25, 72: PRINT USING "###"; min; sec;
        IF min = 60 THEN HR = HR + 1: min = 0
        IF HR = 24 THEN day = day + 1: HR = 0
        IF dte = 1 THEN LOCATE 25, 66: PRINT USING "###"; day; HR; min;
        dayNUM = 365
        IF INT(year / 4) * 4 = year THEN dayNUM = 366
        IF INT(year / 100) * 100 = year THEN dayNUM = 365
        IF INT(year / 400) * 400 = year THEN dayNUM = 366
        IF day = dayNUM + 1 THEN year = year + 1: day = 1: IF dte = 1 THEN LOCATE 25, 60: PRINT USING "####"; year; : LOCATE 25, 66: PRINT USING "###"; day;
       
       
        'Print Simulation data
        IF COS(VangleDIFF) <> 0 AND Dtarg - P(targ, 5) <> 0 THEN Acctarg = Acctarg + ((((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2) / (2 * (Dtarg - P(targ, 5)))) * COS(VangleDIFF))
        oldAcctarg = Acctarg
        IF dispFLAG = 1 THEN 113
        COLOR 12
        LOCATE 8, 16: IF CONSTacc = 1 THEN PRINT CHR$(67 + (10 * MATCHacc));  ELSE PRINT " ";
        COLOR 15
        IF ref = 33 AND sednaFLAG = 0 THEN Vref = 0
        LOCATE 2, 12
        IF Vref > 9999999 THEN PRINT USING "##.####^^^^"; Vref ELSE PRINT USING "########.##"; Vref;
        Vrefhab = SQR((Vx(28) - Vx(ref)) ^ 2 + (Vy(28) - Vy(ref)) ^ 2)
        LOCATE 3, 12
        IF Vrefhab > 9999999 THEN PRINT USING "##.####^^^^"; Vrefhab;  ELSE PRINT USING "########.##"; Vrefhab;
        Vreftarg = SQR((Vx(targ) - Vx(ref)) ^ 2 + (Vy(targ) - Vy(ref)) ^ 2)
        LOCATE 4, 12
        IF Vreftarg > 9999999 THEN PRINT USING "##.####^^^^"; Vreftarg;  ELSE PRINT USING "########.##"; Vreftarg;
        LOCATE 14, 7
        IF ABS(Acctarg) > 9999 THEN PRINT USING "##.##^^^^"; Acctarg;  ELSE PRINT USING "######.##"; Acctarg;
        LOCATE 13, 2
        IF Dtarg > 9.9E+11 THEN PRINT USING "##.########^^^^"; (Dtarg - P(targ, 5) - P(28, 5)) / 1000;  ELSE PRINT USING "###,###,###.###"; (Dtarg - P(targ, 5) - P(28, 5)) / 1000;
        LOCATE 15, 9: PRINT USING "####.##"; Atargref * RAD;
        LOCATE 7, 8: PRINT USING "#####.###"; Are;
        LOCATE 8, 8: PRINT USING "#####.##"; Aacc;
        LOCATE 11, 6
        IF Dfuel = 0 THEN PRINT "H"; : PRINT USING "#########"; fuel; : PRINT CHR$(32 + (refuel * 11) + (ventfuel * 13));  ELSE PRINT "A"; : PRINT USING "#########"; AYSEfuel; : PRINT CHR$(32 + (AYSErefuel * 11) + (AYSEventfuel * 13));
        LOCATE 18, 9: PRINT USING "####.##"; DIFFangle;
        IF TELEupFLAG = 1 THEN LOCATE 23, 66: PRINT "TELEMETRY OFF";
        IF TELEflag = 1 THEN LOCATE 22, 66: PRINT "REC: "; filename$;
       
        IF PROJflag = 0 THEN 113
        Vcen = SQR(((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2)) * -1 * COS(VangleDIFF)
        Vtan = SQR(((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2)) * ABS(SIN(VangleDIFF))
        LOCATE 16, 7
        IF ABS(Vcen) > 99999 THEN PRINT USING "##.##^^^^"; Vcen;  ELSE PRINT USING "######.##"; Vcen;
        LOCATE 17, 7
        IF ABS(Vtan) > 99999 THEN PRINT USING "##.##^^^^"; Vtan;  ELSE PRINT USING "######.##"; Vtan;

113     IF SYSflag = 2 THEN LOCATE 2, 60: COLOR 12 - (2 * AYSEad): PRINT "Auto Docking    "; : COLOR 15
       

        'Control input
103     z$ = INKEY$
        IF z$ = "" THEN 105
        IF z$ = "q" THEN GOSUB 900
        IF z$ = "`" THEN dispFLAG = 1 - dispFLAG: CLS : IF dispFLAG = 0 THEN GOSUB 400
        IF z$ = CHR$(27) THEN GOSUB 910
        IF z$ = " " THEN cen = targ: cenXoff = Px(28, 3) - Px(cen, 3): cenYoff = Py(28, 3) - Py(cen, 3)
        IF z$ = CHR$(9) THEN Aflag = Aflag + 1: IF Aflag = 3 THEN Aflag = 0: GOSUB 400 ELSE GOSUB 400
        IF z$ = CHR$(0) + ";" THEN Sflag = 1: GOSUB 400
        IF z$ = CHR$(0) + "<" THEN Sflag = 0: GOSUB 400
        IF z$ = CHR$(0) + "=" THEN Sflag = 4: GOSUB 400
        IF z$ = CHR$(0) + ">" THEN Sflag = 2: GOSUB 400
        IF z$ = CHR$(0) + "?" THEN Sflag = 3: GOSUB 400
        IF z$ = CHR$(0) + "@" THEN SYSflag = SYSflag + 1: GOSUB 400
        IF SYSflag > 0 THEN 121
        IF z$ = CHR$(0) + "A" THEN Sflag = 5: GOSUB 400
        IF z$ = CHR$(0) + "B" THEN Sflag = 6: GOSUB 400
        IF z$ = CHR$(0) + "C" THEN OFFSET = -1 * (1 - ABS(OFFSET)): GOSUB 400
        IF z$ = CHR$(0) + "D" THEN OFFSET = 1 - ABS(OFFSET): GOSUB 400
        IF z$ = CHR$(0) + CHR$(134) THEN ventfuel = 1 - ventfuel: refuel = 0
        IF z$ = CHR$(0) + CHR$(133) THEN refuel = 1 - refuel: ventfuel = 0
       
121     IF z$ = "w" THEN Dfuel = 1 - Dfuel
        IF z$ = "a" THEN CONSTacc = 1 - CONSTacc: Accel = Aacc: MATCHacc = 0
        IF z$ = "g" THEN MATCHacc = 1 - MATCHacc: CONSTacc = MATCHacc
        IF z$ = "+" AND mag < 130000000000# THEN mag = mag / .75: CLS : GOSUB 400
        IF z$ = "-" AND mag > 6.8E-11 THEN mag = mag * .75: CLS : GOSUB 400
        IF z$ = CHR$(0) + "I" THEN LINE (30, 120)-(30 + (5 * SIN(Sangle)), 120 + (5 * COS(Sangle))), 0: Sangle = Sangle - (1 / RAD)
        IF z$ = CHR$(0) + "G" THEN LINE (30, 120)-(30 + (5 * SIN(Sangle)), 120 + (5 * COS(Sangle))), 0: Sangle = Sangle + (1 / RAD)
       
        IF z$ = "[" THEN GOSUB 460
        IF z$ = "]" THEN GOSUB 465
        IF z$ < "0" OR z$ > "U" THEN 110
        z = ASC(z$) - 48
        IF z = 36 AND MODULEflag = 0 THEN 110
        IF Aflag = 0 THEN cen = z: CLS : cenXoff = 0: cenYoff = 0: GOSUB 400
        IF z = 28 THEN 110
        IF Aflag = 1 THEN targ = z: GOSUB 400
        IF Aflag = 2 THEN ref = z: GOSUB 400
        
      
110     IF z$ = CHR$(0) + "S" THEN eng = eng + .1: GOSUB 400
        IF z$ = CHR$(0) + "R" THEN eng = eng - .1: GOSUB 400
        IF z$ = CHR$(0) + "Q" THEN eng = eng + 1: GOSUB 400
        IF z$ = CHR$(0) + "O" THEN eng = eng - 1: GOSUB 400
        IF z$ = "\" THEN eng = eng * -1: GOSUB 400
        IF z$ = CHR$(13) THEN eng = 100: GOSUB 400
        IF z$ = CHR$(8) THEN eng = 0: MATCHacc = 0: CONSTacc = 0: GOSUB 400
        IF z$ = CHR$(0) + "H" THEN vern = .1: vernA = 0
        IF z$ = CHR$(0) + "K" THEN vern = .1: vernA = 90
        IF z$ = CHR$(0) + "M" THEN vern = .1: vernA = -90
        IF z$ = CHR$(0) + "P" THEN vern = .1: vernA = 180
       
        IF z$ <> "v" THEN 107
        vflag = 1 - vflag
        LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (20 * SIN(Vvangle)) + (Px(28, 3) - cenX) * mag / AU, 220 + (20 * COS(Vvangle)) + (Py(28, 3) - cenY) * mag * 1 / AU), 0
        LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (30 * SIN(Atarg)) + (Px(28, 3) - cenX) * mag / AU, 220 + (30 * COS(Atarg)) + (Py(28, 3) - cenY) * mag * 1 / AU), 0
107     IF z$ = "t" THEN trail = 1 - trail: CLS : GOSUB 400
        IF z$ = "u" THEN tr = 1 - tr: CLS : GOSUB 400
        IF z$ = "d" THEN dte = 1 - dte: LOCATE 25, 60: PRINT USING "####"; year; : LOCATE 25, 66: PRINT USING "###"; day; HR; min; sec: GOSUB 400
        IF z$ = "p" THEN PROJflag = 1 - PROJflag: GOSUB 400
        IF z$ = "o" THEN GOSUB 3000
        IF z$ = "f" THEN GOSUB 3000
        IF z$ = "n" THEN GOSUB 3000
        IF z$ = "c" THEN CLOSE #2: telTRACK = 0: CLS : GOSUB 400
        IF z$ = "x" THEN IF TELEupFLAG = 0 THEN TELEupFLAG = 1: LOCATE 23, 66: PRINT "TELEMETRY OFF";  ELSE TELEupFLAG = 0: LOCATE 23, 66: PRINT "             ";
        IF z$ = "Z" THEN TELEflag = 0: telTRACK = 0: CLOSE #2: GOSUB 6000
        IF z$ = "z" THEN TELEflag = 0: telTRACK = 0: CLOSE #2: GOSUB 6005
        IF z$ = "y" THEN GOSUB 6100: CLS : GOSUB 400
        IF z$ <> "/" THEN 104
        IF ts <= .5 THEN ts = ts - .125: GOSUB 400
        IF ts = 1 THEN ts = .5: GOSUB 400
        IF ts = 2 THEN ts = 1: GOSUB 400
        IF ts = 10 THEN ts = 2: GOSUB 400
        IF ts > 10 THEN ts = ts - 10: GOSUB 400
104     IF z$ <> "*" THEN 105
        IF ts > 9 THEN ts = ts + 10: GOSUB 400
        IF ts = 2 THEN ts = 10: GOSUB 400
        IF ts = 1 THEN ts = 2: GOSUB 400
        IF ts = .5 THEN ts = 1: GOSUB 400
        IF ts < .5 THEN ts = ts + .125: GOSUB 400
105     IF SYSflag > 0 AND LEN(z$) > 1 THEN GOSUB 500
        IF z$ = "s" THEN GOSUB 600
        IF z$ = "r" THEN GOSUB 700
        IF tttt - TIMER > ts * 10 THEN tttt = TIMER + (ts / 2)
        IF ts < .3 AND TIMER - tttt < ts THEN 103
        GOTO 100


        'SUBROUTINE Automatic space craft orientation calculations
301     IF Ztel(1) = 1 THEN Sflag = 1: MATCHacc = 0: CONSTacc = 0
        COLOR 15
        Aoffset = ATN((P(targ, 5) * 1.01) / Dtarg): Atarg = Atarg - (Aoffset * OFFSET)
        VVtx = Vx(targ) - Vx(28)
        VVty = Vy(targ) - Vy(28)
        IF VVty = 0 THEN IF VVtx < 0 THEN VvTangle = .5 * 3.1415926535# ELSE VvTangle = 1.5 * 3.1415926535# ELSE VvTangle = ATN(VVtx / VVty)
        IF VVty > 0 THEN VvTangle = VvTangle + 3.1415926535#
        IF VvTangle < 0 THEN VvTangle = VvTangle + 6.283185
        DIFFangle = (Atarg - VvTangle) * RAD
        IF DIFFangle > 180 THEN DIFFangle = -360 + DIFFangle
        IF DIFFangle < -180 THEN DIFFangle = 360 + DIFFangle
       
        IF Sflag = 1 THEN 302
        trg = ref
        IF Sflag = 2 THEN trg = targ
        difx = Px(i, 3) - Px(trg, 3)
        dify = Py(i, 3) - Py(trg, 3)
        IF dify = 0 THEN IF difx < 0 THEN Sangle = .5 * 3.1415926535# ELSE Sangle = 1.5 * 3.1415926535# ELSE Sangle = ATN(difx / dify)
        IF dify > 0 THEN Sangle = Sangle + 3.1415926535#
        Sangle = Sangle - (Aoffset * OFFSET)
        IF Sflag = 5 THEN Sangle = VvTangle
        IF Sflag = 6 THEN Sangle = VvTangle + 3.1415926535#
        IF Sflag = 0 THEN Sangle = Sangle - (90 / RAD)
        IF Sflag = 4 THEN Sangle = Sangle + (90 / RAD)
        IF Sflag = 3 THEN Sangle = Sangle - (180 / RAD)

302     IF oldAcctarg < 0 THEN MATCHacc = 0
        IF ABS(eng) > 100 * (1 - Ztel(2)) THEN eng = SGN(eng) * 100 * (1 - Ztel(2)): CONSTacc = 0
        IF AYSE = 150 THEN eng = eng * TTC * GPD * Agen * Abr * SGN(AYSEfuel) ELSE eng = eng * ion * acc * Hbr * SGN(fuel)
        'CONSTacc = CONSTacc * SGN(eng)
        LOCATE 5, 16: COLOR 15: PRINT USING "####.#"; eng;
        COLOR 15 - (3 * SGN(Ztel(2)))
        IF AG < 1 AND ABS(Aacc) > 60 THEN eng = eng - SGN(eng): LOCATE 5, 2: PRINT "Engine"; : LOCATE 5, 16: PRINT USING "####.#"; eng;
        COLOR 15

        IF MATCHacc = 1 THEN Accel = oldAcctarg
        IF AYSE = 150 THEN 303
        HABmass = 275000 + fuel
        IF CONSTacc = 1 THEN Aacc = Accel: eng = Aacc * HABmass / 175000 ELSE Aacc = (175000 * eng) / HABmass
        Av = (175000 * vern) / HABmass
        GOTO 304
303     HABmass = fuel + AYSEfuel + 20275000
        IF CONSTacc = 1 THEN Aacc = Accel: eng = Aacc * HABmass / 64000000# ELSE Aacc = (64000000# * eng) / HABmass
        Av = 0

304     IF ABS(eng) > 105 THEN Accel = Accel * ABS(105 / eng): eng = 105
        P(i, 1) = P(i, 1) + (Aacc * SIN(Sangle))
        P(i, 2) = P(i, 2) + (Aacc * COS(Sangle))
        P(i, 1) = P(i, 1) + Av * SIN(Sangle + (vernA / RAD))
        P(i, 2) = P(i, 2) + Av * COS(Sangle + (vernA / RAD))
        vern = 0


        IF atm = 0 THEN 319
        VVrx = Vx(atm) - Vx(28)
        VVry = Vy(atm) - Vy(28)
        IF VVry = 0 THEN IF VVrx < 0 THEN VvRangle = .5 * 3.1415926535# ELSE VvRangle = 1.5 * 3.1415926535# ELSE VvRangle = ATN(VVrx / VVry)
        IF VVry > 0 THEN VvRangle = VvRangle + 3.1415926535#
        IF VvRangle < 0 THEN VvRangle = VvRangle + 6.283185
        VVr = SQR((VVrx ^ 2) + (VVry ^ 2))
        IF Ratm < 0 THEN Pr = P(atm, 8) ELSE Pr = P(atm, 8) * (2.71828 ^ (-1 * Ratm / P(atm, 9)))
        Are = Pr * VVr * VVr * .0002
        IF Are * ts > VVr / 2 THEN Are = (VVr / 2) / ts
        P(i, 1) = P(i, 1) - (Are * SIN(VvRangle))
        P(i, 2) = P(i, 2) - (Are * COS(VvRangle))
319     IF AYSE = 0 THEN fuel = fuel - (.05 * ABS(eng) * ts) - (.5 * vern * ts) ELSE AYSEfuel = AYSEfuel - (.175 * ABS(eng) * ts)
        RETURN


       
        'SUBROUTINE print control variable names to screen
400     IF ts < .125 THEN ts = .125
        IF ts > 60 THEN ts = 60
        IF ufo2 = 1 THEN ts = .25
        COLOR 8
        LOCATE 1, 1: PRINT CHR$(201); STRING$(9, 205); CHR$(209); STRING$(11, 205); CHR$(187);
        LOCATE 2, 1: PRINT CHR$(186); : LOCATE 2, 11: PRINT CHR$(179); : LOCATE 2, 23: PRINT CHR$(186);
        LOCATE 3, 1: PRINT CHR$(186); : LOCATE 3, 11: PRINT CHR$(179); : LOCATE 3, 23: PRINT CHR$(186);
        LOCATE 4, 1: PRINT CHR$(186); : LOCATE 4, 11: PRINT CHR$(179); : LOCATE 4, 23: PRINT CHR$(186);
        LOCATE 5, 1: PRINT CHR$(186); : LOCATE 5, 11: PRINT CHR$(179); : LOCATE 5, 23: PRINT CHR$(186);
        LOCATE 6, 1: PRINT CHR$(204); STRING$(9, 205); CHR$(207); STRING$(5, 205); CHR$(203); STRING$(5, 205); CHR$(188);
        COLOR 7
        IF Ztel(1) = 1 THEN Sflag = 1
        'IF eng > ((50 * ion * acc * Hbr) + (AYSE * TTC * GPD * Agen * Abr)) * (1 - Ztel(2)) THEN eng = ((50 * ion * acc * Hbr) + (AYSE * TTC * GPD * Agen * Abr)) * (1 - Ztel(2))
        LOCATE 2, 2: PRINT "ref Vo";
        'LOCATE 2, 2: PRINT "V hab ";
        LOCATE 3, 2: PRINT "V hab-ref";
        LOCATE 4, 2: PRINT "Vtarg-ref";
        COLOR 7 + (5 * SGN(Ztel(2)))
        LOCATE 5, 2: PRINT "Engine"; : LOCATE 5, 16: COLOR 15: PRINT USING "####.#"; eng;
        COLOR 7 + (5 * Ztel(1))
        LOCATE 25, 2: PRINT "NAVmode";
        COLOR 14
        LOCATE 25, 10
        IF OFFSET = -1 THEN PRINT "-";
        IF OFFSET = 0 THEN PRINT " ";
        IF OFFSET = 1 THEN PRINT "+";
        COLOR 15
        LOCATE 25, 11
        IF Sflag = 0 THEN PRINT "ccw prog "; : GOTO 401
        IF Sflag = 4 THEN PRINT "ccw retro"; : GOTO 401
        IF Sflag = 1 THEN PRINT "manual   "; : GOTO 401
        IF Sflag = 2 THEN PRINT "app targ "; : GOTO 401
        IF Sflag = 5 THEN PRINT "pro Vtrg "; : GOTO 401
        IF Sflag = 6 THEN PRINT "retr Vtrg"; : GOTO 401
        IF Sflag = 3 THEN PRINT "deprt ref";
401     COLOR 8
        LOCATE 21, 1: PRINT CHR$(204); STRING$(7, 205); CHR$(209); STRING$(7, 205); CHR$(202); CHR$(205); CHR$(205); CHR$(187);
        LOCATE 22, 1: PRINT CHR$(186); : LOCATE 22, 9: PRINT CHR$(179); : LOCATE 22, 20: PRINT CHR$(186);
        LOCATE 23, 1: PRINT CHR$(186); : LOCATE 23, 9: PRINT CHR$(179); : LOCATE 23, 20: PRINT CHR$(186);
        LOCATE 24, 1: PRINT CHR$(186); : LOCATE 24, 9: PRINT CHR$(179); : LOCATE 24, 20: PRINT CHR$(186);
        LOCATE 25, 1: PRINT CHR$(186); : LOCATE 25, 9: PRINT CHR$(179); : LOCATE 25, 20: PRINT CHR$(186);
        LOCATE 26, 1: PRINT CHR$(200); STRING$(7, 205); CHR$(207); STRING$(10, 205); CHR$(188);
        LOCATE 10, 1: PRINT CHR$(204); STRING$(15, 205); CHR$(185);
        LOCATE 11, 1: PRINT CHR$(186); : LOCATE 11, 17: PRINT CHR$(186);
        LOCATE 12, 1: PRINT CHR$(199); STRING$(15, 196); CHR$(182);
        LOCATE 13, 1: PRINT CHR$(186); : LOCATE 13, 17: PRINT CHR$(186);
        LOCATE 14, 1: PRINT CHR$(186); : LOCATE 14, 17: PRINT CHR$(186);
        LOCATE 15, 1: PRINT CHR$(186); : LOCATE 15, 17: PRINT CHR$(186);
        LOCATE 16, 1: PRINT CHR$(186); : LOCATE 16, 17: PRINT CHR$(186);
        LOCATE 17, 1: PRINT CHR$(186); : LOCATE 17, 17: PRINT CHR$(186);
        LOCATE 18, 1: PRINT CHR$(186); : LOCATE 18, 17: PRINT CHR$(186);
        LOCATE 19, 1: PRINT CHR$(186); : LOCATE 19, 17: PRINT CHR$(186);
        LOCATE 20, 1: PRINT CHR$(186); : LOCATE 20, 17: PRINT CHR$(186);
        LOCATE 7, 1: PRINT CHR$(186); : LOCATE 7, 17: PRINT CHR$(186);
        LOCATE 8, 1: PRINT CHR$(186); : LOCATE 8, 17: PRINT CHR$(186);
        LOCATE 9, 1: PRINT CHR$(186); : LOCATE 9, 17: PRINT CHR$(186);
        IF dte = 1 THEN LOCATE 24, 59: PRINT CHR$(201); STRING$(18, 205); CHR$(187);
        IF dte = 1 THEN LOCATE 25, 59: PRINT CHR$(186); : LOCATE 25, 78: PRINT CHR$(186);
        IF dte = 1 THEN LOCATE 26, 59: PRINT CHR$(200); STRING$(18, 205); CHR$(188);
        IF Aflag = 0 THEN COLOR 10 ELSE COLOR 7
        LOCATE 22, 2: PRINT "center "; : COLOR 15: LOCATE 22, 11: PRINT Znme$(cen);
        IF Aflag = 1 THEN COLOR 10 ELSE COLOR 7
        LOCATE 23, 2: PRINT "target "; : COLOR 15: LOCATE 23, 11: PRINT Znme$(targ);
        IF Aflag = 2 THEN COLOR 10 ELSE COLOR 7
        LOCATE 24, 2: PRINT "ref    "; : COLOR 15: LOCATE 24, 11: PRINT Znme$(ref);
        COLOR 15
        LOCATE 9, 11: PRINT USING "##.###"; ts;
        COLOR 7
        LOCATE 11, 2: PRINT "Fuel";
        LOCATE 14, 2: PRINT "Acc";
        LOCATE 15, 2: PRINT CHR$(233); " Hrt";
        LOCATE 16, 2: PRINT "Vcen";
        LOCATE 17, 2: PRINT "Vtan";
        LOCATE 18, 2: PRINT CHR$(233); " Vt";
        LOCATE 19, 2: PRINT "Peri";
        LOCATE 20, 2: PRINT "Apo ";
        IF telTRACK = 1 THEN LOCATE 21, 59: PRINT "Track Dev          km"; : LOCATE 22, 59: PRINT "Speed Dev         m/s";
        COLOR 15
402     IF dte = 1 THEN LOCATE 25, 60: PRINT USING "####"; year; : LOCATE 25, 66: PRINT USING "###"; day; HR; min; sec;
       
       
        'Print optional control information
403     LOCATE 1, 60:
        IF SYSflag = 0 THEN PRINT "               ";
        IF SYSflag = 1 THEN PRINT "HABITAT DOCKING"; : GOTO 410
        IF SYSflag = 2 THEN PRINT "AYSE DOCKING   "; : GOTO 420
        IF SYSflag = 3 THEN PRINT "AYSE DRIVE     "; : GOTO 430
        IF SYSflag = 4 THEN PRINT "HABITAT DRIVE  "; : GOTO 440
        IF SYSflag = 5 THEN PRINT "               "; : GOTO 450
        RETURN

410     LOCATE 2, 60: COLOR 12 - (AG * 2):   PRINT "Inertial Dampers"; : COLOR 15
        LOCATE 3, 60: COLOR 12 - (AG * 2):   PRINT "Artificial Grav."; : COLOR 15
        LOCATE 4, 60: COLOR 12 - (DH * 2):   PRINT "Docking Hatch"; : COLOR 15
        LOCATE 5, 60: COLOR 12 - (DHL * 2):  PRINT "Dck Hatch Lock"; : COLOR 15
        LOCATE 6, 60: COLOR 12 - (DHLA * 2): PRINT "Dck Htch Lck ARM"; : COLOR 15
        LOCATE 7, 60: COLOR 12 - (AYSEtr * 2):  PRINT "Thermal Roll    "; : COLOR 15
        LOCATE 8, 60: PRINT "                ";
        RETURN

420     LOCATE 2, 60: COLOR 12 - (AYSEad * 2):  PRINT "Auto Docking    "; : COLOR 15
        LOCATE 3, 60: COLOR 12 - (AYSEdl * 2):   PRINT "Docking Latch   "; : COLOR 15
        LOCATE 4, 60: COLOR 12 - (AYSEdla * 2):   PRINT "Dck Latch ARMED "; : COLOR 15
421     LOCATE 5, 60: COLOR 11 - AYSErefuel: PRINT "AYSE Refuel     ";
        LOCATE 6, 60: COLOR 11 - AYSEventfuel: PRINT "AYSE Vent Fuel  ";
        LOCATE 7, 60: PRINT "                ";
        LOCATE 8, 60: PRINT "                ";
        RETURN


430     COLOR 10
        LOCATE 2, 60: COLOR 9 + TTC: PRINT "TTC             ";
        LOCATE 3, 60: COLOR 9 + GPD: PRINT "GPDs            ";
        LOCATE 4, 60: COLOR 9 + Agen: PRINT "Generator Voltge";
        LOCATE 5, 60: COLOR 9 + Acon1: PRINT "React Confnmt 1 ";
        LOCATE 6, 60: COLOR 9 + Acon2: PRINT "React Confnmt 2 ";
        LOCATE 7, 60: COLOR 9 + Abr: PRINT "System Breakers ";
        LOCATE 8, 60: COLOR 9 + AT: PRINT "System Temp    ";
        COLOR 15
        RETURN

440     COLOR 9 + Fcon1: LOCATE 2, 60: PRINT "React Confnmt 1 ";
        COLOR 9 + Fcon2: LOCATE 3, 60: PRINT "React Confnmt 2 ";
        COLOR 9 + ion: LOCATE 4, 60: PRINT "Ion Voltage     ";
        COLOR 9 + acc: LOCATE 5, 60: PRINT "Accel Voltage   ";
        COLOR 9 + Hbr: LOCATE 6, 60: PRINT "System Breakers ";
        COLOR 9 + HT: LOCATE 7, 60: PRINT "System Temp     ";
        LOCATE 8, 60: PRINT "                ";
        COLOR 15
        RETURN



450     SYSflag = 0
451     LOCATE 2, 60: PRINT "                ";
        LOCATE 3, 60: PRINT "                ";
        LOCATE 4, 60: PRINT "                ";
        LOCATE 5, 60: PRINT "                ";
        LOCATE 6, 60: PRINT "                ";
        LOCATE 7, 60: PRINT "                ";
        LOCATE 8, 60: PRINT "                ";
        RETURN


460     ON Aflag + 1 GOTO 461, 462, 463
461     IF cen - 1 = 36 AND MODULEflag = 0 THEN cen = 36
        IF cen - 1 < 0 THEN cen = 1
        cen = cen - 1
        CLS
        cenXoff = 0
        cenYoff = 0
        GOTO 464

462     IF targ - 1 = 28 THEN targ = 28
        IF targ - 1 = 36 AND MODULEflag = 0 THEN targ = 36
        IF targ - 1 < 0 THEN targ = 1
        targ = targ - 1
        GOTO 464

463     IF ref - 1 = 28 THEN ref = 28
        IF ref - 1 = 36 AND MODULEflag = 0 THEN ref = 36
        IF ref - 1 < 0 THEN ref = 1
        ref = ref - 1
       
        
464     GOSUB 400
        RETURN


465     ON Aflag + 1 GOTO 466, 467, 468
466     IF cen + 1 = 36 AND MODULEflag = 0 THEN cen = 36
        IF cen + 1 > 37 THEN cen = 36
        cen = cen + 1
        CLS
        cenXoff = 0
        cenYoff = 0
        GOTO 469

467     IF targ + 1 = 28 THEN targ = 28
        IF targ + 1 = 36 AND MODULEflag = 0 THEN targ = 36
        IF targ + 1 > 37 + ufo1 + ufo2 THEN targ = 36 + ufo1 + ufo2
        targ = targ + 1
        GOTO 469

468     IF ref + 1 = 28 THEN ref = 28
        IF ref + 1 = 36 AND MODULEflag = 0 THEN ref = 36
        IF ref + 1 > 37 THEN ref = 36
        ref = ref + 1
       
469     GOSUB 400
        RETURN




        'SUBROUTINE Optional control input
500     ON SYSflag GOTO 510, 520, 530, 540
        RETURN
510     IF z$ = CHR$(0) + "A" THEN AG = 1 - AG: COLOR 12 - (AG * 2): LOCATE 2, 60: PRINT "Inertial Dampers"; : LOCATE 3, 60: PRINT "Artificial Grav."; : COLOR 15
        IF z$ = CHR$(0) + "B" AND DHL = 0 THEN DH = 1 - DH: LOCATE 4, 60: COLOR 12 - (DH * 2): PRINT "Docking Hatch"; : COLOR 15
        IF z$ = CHR$(0) + "C" AND DHLA = 1 THEN DHL = 1 - DHL: LOCATE 5, 60: COLOR 12 - (DHL * 2): PRINT "Dck Hatch Lock"; : COLOR 15
        IF z$ = CHR$(0) + "D" THEN DHLA = 1 - DHLA: LOCATE 6, 60: COLOR 12 - (DHLA * 2): PRINT "Dck Htch Lck ARM"; : COLOR 15
        IF z$ = CHR$(0) + CHR$(133) THEN AYSEtr = 1 - AYSEtr: LOCATE 7, 60: COLOR 12 - (AYSEtr * 2): PRINT "Thermal Roll    "; : COLOR 15
        RETURN

520     IF z$ = CHR$(0) + "A" THEN ts = .25
        IF z$ = CHR$(0) + "A" AND AYSEadFLAG = 1 THEN 521
        IF z$ = CHR$(0) + "A" AND AYSEdl = 0 THEN IF AYSEad = 0 THEN AYSEad = 1: AYSEadFLAG = 1 ELSE AYSEad = 0: AYSEadFLAG = 0
        IF z$ = CHR$(0) + "B" AND AYSEdla = 1 AND AYSEad > 1.5 THEN AYSEdl = 1 - AYSEdl: AYSEadFLAG = 0: LOCATE 3, 60: COLOR 12 - (AYSEdl * 2): PRINT "Docking Latch   "; : COLOR 15
        IF z$ = CHR$(0) + "B" AND AYSEdla = 1 AND AYSEad = 0 THEN GOSUB 3200: GOSUB 400
        IF z$ = CHR$(0) + "C" THEN AYSEdla = 1 - AYSEdla: LOCATE 4, 60: COLOR 12 - (AYSEdla * 2): PRINT "Dck Latch ARMED "; : COLOR 15
        IF AYSEdl = 1 AND AYSEad > 1.5 THEN AYSE = 150 ELSE AYSE = 0
        IF z$ = CHR$(0) + CHR$(133) THEN AYSEventfuel = 1 - AYSEventfuel: AYSErefuel = 0: GOSUB 421
        IF z$ = CHR$(0) + "D" THEN AYSErefuel = 1 - AYSErefuel: AYSEventfuel = 0: GOSUB 421
521     RETURN

530     IF z$ = CHR$(0) + "A" THEN TTC = 2 * (1 - SGN(TTC)): LOCATE 2, 60: COLOR 9 + TTC: PRINT "TTC             ";
        IF z$ = CHR$(0) + "B" THEN GPD = 2 * (1 - SGN(GPD)): LOCATE 3, 60: COLOR 9 + GPD: PRINT "GPDs            ";
        IF z$ = CHR$(0) + "C" THEN Agen = 2 * (1 - SGN(Agen)): LOCATE 4, 60: COLOR 9 + Agen: PRINT "Generator Voltge";
        IF z$ = CHR$(0) + "D" THEN Acon1 = 2 * (1 - SGN(Acon1)): LOCATE 5, 60: COLOR 9 + Acon1: PRINT "React Confnmt 1 ";
        IF z$ = CHR$(0) + CHR$(133) THEN Acon2 = 2 * (1 - SGN(Acon2)): LOCATE 6, 60: COLOR 9 + Acon2: PRINT "React Confnmt 2 ";
        IF z$ = CHR$(0) + CHR$(134) THEN Abr = 2 * (1 - SGN(Abr)): LOCATE 7, 60: COLOR 9 + Abr: PRINT "System Breakers ";
        COLOR 15
        RETURN

540     IF z$ = CHR$(0) + "A" THEN Fcon1 = 2 * (1 - SGN(Fcon1)): COLOR 9 + Fcon1: LOCATE 2, 60: PRINT "React Confnmt 1 ";
        IF z$ = CHR$(0) + "B" THEN Fcon2 = 2 * (1 - SGN(Fcon2)): COLOR 9 + Fcon2: LOCATE 3, 60: PRINT "React Confnmt 2 ";
        IF z$ = CHR$(0) + "C" THEN ion = 2 * (1 - SGN(ion)): COLOR 9 + ion: LOCATE 4, 60: PRINT "Ion Voltage     ";
        IF z$ = CHR$(0) + "D" THEN acc = 2 * (1 - SGN(acc)): COLOR 9 + acc: LOCATE 5, 60: PRINT "Accel Voltage   ";
        IF z$ = CHR$(0) + CHR$(133) THEN Hbr = 2 * (1 - SGN(Hbr)): COLOR 9 + Hbr: LOCATE 6, 60: PRINT "System Breakers ";
        COLOR 15
        RETURN



        'SUBROUTINE Restore date from file
700     LOCATE 10, 60: PRINT "Load File: "; : INPUT ; "", filename$
        IF filename$ = "" THEN 703
704     OPEN "R", #1, filename$
        IF LOF(1) > 1 THEN 701
        CLOSE #1
        LOCATE 11, 60: PRINT filename$; " is empty";
702     LOCATE 10, 60: PRINT "                  ";
        GOTO 700
701     CLOSE #1
        OPEN "I", #1, filename$
        INPUT #1, z$
        IF z$ <> "ORBIT5S" THEN LOCATE 11, 60: PRINT filename$; " is unusable"; : CLOSE #1: GOTO 702
        INPUT #1, eng, vflag, Aflag, Sflag, SYSflag
        INPUT #1, mag, Sangle, cen, targ, ref
        INPUT #1, trail, tr, dte, ts, Eflag
        INPUT #1, year, day, HR, min, sec
        INPUT #1, AYSEad, AYSEdl, AYSEdla, AYSEtr, AYSEadFLAG, AYSE
        INPUT #1, zz, MODULEflag, AYSEdist, OCESSdist, explosion, explosion1
        INPUT #1, Px(0, 3), Py(0, 3), Vx(0), Vy(0), P(0, 1), P(0, 2)
        Px(0, 3) = 0
        Py(0, 3) = 0
        P(0, 1) = 0
        FOR i = 1 TO 37
         INPUT #1, Px(i, 3), Py(i, 3), Vx(i), Vy(i), P(i, 1), P(i, 2)
        NEXT i
        FOR i = 1 TO 2
         FOR j = 1 TO 6
          INPUT #1, z
         NEXT j
        NEXT i
        INPUT #1, fuel, AYSEfuel
        Px(37, 3) = 4446370.8284487# + Px(3, 3): Py(37, 3) = 4446370.8284487# + Py(3, 3): Vx(37) = Vx(3): Vy(37) = Vy(3)
        Px(38, 3) = 0: Py(38, 3) = 0: Vx(38) = 0: Vy(38) = 0: P(38, 1) = 0: P(38, 2) = 0
        Px(39, 3) = 0: Py(39, 3) = 0: Vx(39) = 0: Vy(39) = 0: P(39, 1) = 0: P(39, 2) = 0
        tttt = TIMER + ts
        ufo1 = 0
        ufo2 = 0
        cenXoff = 0
        cenYoff = 0
        cenX = Px(cen, 3)
        cenY = Py(cen, 3)
703     CLS
        CLOSE #1
        explosion = 0
        explosion1 = 0
        GOSUB 400
        RETURN



        'SUBROUTINE save data to file
600     LOCATE 9, 60: PRINT "8 charaters a-z 0-9";
        LOCATE 10, 60: PRINT "Save File: "; : INPUT ; "", filename$
        IF filename$ = "" THEN CLS : GOSUB 400: RETURN
        OPEN "R", #1, filename$
        IF LOF(1) < 1 THEN 601
        LOCATE 11, 60: PRINT "File exists";
        LOCATE 12, 60: PRINT "overwrite? "; : INPUT ; "", z$
        IF UCASE$(LEFT$(z$, 1)) = "Y" THEN 601
        FOR i = 9 TO 12
         LOCATE i, 60: PRINT "                  ";
        NEXT i
        CLOSE #1
        GOTO 600
601     CLOSE #1
        OPEN "O", #1, filename$
        PRINT #1, "ORBIT5S"
        PRINT #1, eng; ","; vflag; ","; Aflag; ","; Sflag; ","; SYSflag
        PRINT #1, mag; ","; Sangle; ","; cen; ","; targ; ","; ref
        PRINT #1, trail; ","; tr; ","; dte; ","; ts; ","; Eflag
        PRINT #1, year; ","; day; ","; HR; ","; min; ","; sec
        PRINT #1, AYSEad; ","; AYSEdl; ","; AYSEdla; ","; AYSEtr; ","; AYSEadFLAG; ","; AYSE
        PRINT #1, Ztel(9); ","; MODULEflag; ","; AYSEdist; ","; OCESSdist; ","; explosion; ","; explosion1
        PRINT #1, Ztel(1); ","; Ztel(2); ","; Vx(0); ","; Vy(0); ","; P(0, 1); ","; P(0, 2)
        FOR i = 1 TO 39
         PRINT #1, Px(i, 3); ","; Py(i, 3); ","; Vx(i); ","; Vy(i); ","; P(i, 1); ","; P(i, 2)
        NEXT i
        PRINT #1, fuel; ","; AYSEfuel
        CLOSE #1
620     CLS
        GOSUB 400
        RETURN


        'SUBROUTINE: Timed telemetry retrieval
800     OPEN "I", #1, Zpath$ + "OSbackup"
        IF EOF(1) THEN 803
        INPUT #1, z$
        IF z$ <> "ORBIT5S" THEN 803
805     FOR i = 1 TO 32
         IF EOF(1) THEN 803
         INPUT #1, Ztel(i)
        NEXT i
801     eng = Ztel(1)
        'vflag = Ztel(2)
        'Aflag = Ztel(3)
        Sflag = Ztel(4)
        'SYSflag = Ztel(5)
        'mag = Ztel(6)
        Sangle = Ztel(7)
        'cen = Ztel(8)
        targ = Ztel(9)
        ref = Ztel(10)
        'trail = Ztel(11)
        'tr = Ztel(12)
        dte = Ztel(13)
        ts = Ztel(14)
        'Eflag = Ztel(15)
        year = Ztel(16)
        day = Ztel(17)
        HR = Ztel(18)
        min = Ztel(19)
        sec = Ztel(20)
        AYSEad = Ztel(21)
        AYSEdl = Ztel(22)
        AYSEdla = Ztel(23)
        AYSEtr = Ztel(24)
        AYSEadFLAG = Ztel(25)
        AYSE = Ztel(26)
        MODULEflag = Ztel(28)
        COLOR 15
        FOR i = 0 TO 39
         FOR j = 1 TO 6
          IF EOF(1) THEN 803
          INPUT #1, Ztel(j)
         NEXT j
         Px(i, 3) = Ztel(1)
         Py(i, 3) = Ztel(2)
         Vx(i) = Ztel(3)
         Vy(i) = Ztel(4)
         P(i, 1) = Ztel(5)
         P(i, 2) = Ztel(6)
        NEXT i
        IF EOF(1) THEN 803 ELSE INPUT #1, fuel
        IF EOF(1) THEN 803 ELSE INPUT #1, AYSEfuel
        Px(37, 3) = 4446370.8284487# + Px(3, 3): Py(37, 3) = 4446370.8284487# + Py(3, 3): Vx(37) = Vx(3): Vy(37) = Vy(3)
        ufo1 = 0
        ufo2 = 0
        IF Px(39, 3) <> 0 AND Py(39, 3) <> 0 THEN ufo2 = 1
        IF Px(38, 3) <> 0 AND Py(38, 3) <> 0 THEN ufo1 = 1
804     GOSUB 400
        CLOSE #1
        RETURN
803     CLOSE #1
        tttt = TIMER + .8
        GOTO 800


        'Confirm end program
900     RUN "orbit5ss.exe"
        END

        'Name and author
910     LOCATE 2, 60: PRINT "OCESS Orbit 5 R  ";
        LOCATE 3, 60: PRINT CHR$(74); CHR$(97); CHR$(109); CHR$(101); CHR$(115); " "; CHR$(77); CHR$(97); CHR$(103); CHR$(119); CHR$(111); CHR$(111); CHR$(100);
        RETURN

        'Orbit Projection
3000    P(ref, 6) = Px(ref, 3)
        P(ref, 7) = Py(ref, 3)
        P(28, 6) = Px(28, 3)
        P(28, 7) = Py(28, 3)
        P(28, 8) = Vx(28) - Vx(ref)
        P(28, 9) = Vy(28) - Vy(ref)
        PROJx = Px(ref, 3) + (P(28, 6) - P(ref, 6))
        PROJy = Py(ref, 3) + (P(28, 7) - P(ref, 7))
        PSET (300 + (PROJx - cenX) * mag / AU, 220 + (PROJy - cenY) * mag * 1 / AU), 15
       
        PROJmin = 10000000000000#
        PROJmax = 0
        PROJflag2 = 0
        PROJflag1 = 0
        IF z$ = "o" THEN stp = 60 ELSE stp = 10
        IF z$ = "f" THEN projlim = 3000 ELSE projlim = 60
        FOR jj = 1 TO projlim
        FOR j = 1 TO stp
        P(28, 10) = 0
        P(28, 11) = 0
        
        difx = P(28, 6) - P(ref, 6)
        dify = P(28, 7) - P(ref, 7)
        R = SQR((dify ^ 2) + (difx ^ 2))
        a = G * P(ref, 4) / (R ^ 2)
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        IF PROJflag1 = 0 THEN PROJangle = angle: PROJflag1 = 1
        P(28, 10) = P(28, 10) + (a * SIN(angle))
        P(28, 11) = P(28, 11) + (a * COS(angle))
        IF R < PROJmin THEN PROJmin = R
        IF R > PROJmax THEN PROJmax = R
        IF ABS(angle - PROJangle) > .2 THEN PROJflag2 = 1
        IF ABS(angle - PROJangle) < .05 AND PROJflag2 = 1 THEN 3001
        IF R > PROJmin AND R < P(ref, 5) THEN 3001
        
        P(28, 8) = P(28, 8) + (P(28, 10) * 2)
        P(28, 9) = P(28, 9) + (P(28, 11) * 2)
        P(28, 6) = P(28, 6) + (P(28, 8) * 2)
        P(28, 7) = P(28, 7) + (P(28, 9) * 2)
   
        NEXT j
        PROJx = Px(ref, 3) + (P(28, 6) - P(ref, 6))
        PROJy = Py(ref, 3) + (P(28, 7) - P(ref, 7))
        IF ABS(300 + (PROJx - cenX) * mag / AU) > 10000 THEN 3003
        IF ABS(220 + (PROJy - cenY) * mag / AU) > 10000 THEN 3003
        LINE -(300 + (PROJx - cenX) * mag / AU, 220 + (PROJy - cenY) * mag / AU), 15
3003    NEXT jj
3001    LOCATE 19, 7
        IF ABS(PROJmin) > 89999999 THEN PRINT USING "##^^^^"; PROJmin / 1000;  ELSE PRINT USING "######"; (PROJmin - P(ref, 5)) / 1000;
        LOCATE 20, 7
        IF ABS(PROJmax) > 89999999 THEN PRINT USING "##^^^^"; PROJmax / 1000;  ELSE PRINT USING "######"; (PROJmax - P(ref, 5)) / 1000;
        RETURN


        'Restore orbital altitude of ISS after large time step
3100    difx = Px(3, 3) - Px(35, 3)
        dify = Py(3, 3) - Py(35, 3)
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Px(35, 3) = Px(3, 3) + ((P(3, 5) + 365000) * SIN(angle))
        Py(35, 3) = Py(3, 3) + ((P(3, 5) + 365000) * COS(angle))
        Vx(35) = Vx(3) + (SIN(angle + 1.570796) * SQR(G * P(3, 4) / (P(3, 5) + 365000)))
        Vy(35) = Vy(3) + (COS(angle + 1.570796) * SQR(G * P(3, 4) / (P(3, 5) + 365000)))
        RETURN

3200    IF CONflag = 0 THEN 3299
        IF MODULEflag = 0 THEN 3210
        difx = Px(28, 3) - Px(36, 3)
        dify = Py(28, 3) - Py(36, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF R > 80 THEN 3299
        IF targ = 36 THEN targ = CONtarg
        IF ref = 36 THEN ref = CONtarg
        IF cen = 36 THEN cen = 28
        MODULEflag = 0
        CLS
        GOSUB 400
        GOTO 3299

3210    Px(36, 3) = Px(28, 3)
        Py(36, 3) = Py(28, 3)
        P(36, 1) = Px(36, 3) - Px(CONtarg, 3)
        P(36, 2) = Py(36, 3) - Py(CONtarg, 3)
        MODULEflag = CONtarg
        Vx(36) = Vx(MODULEflag)
        Vy(36) = Vy(MODULEflag)

3299    RETURN


        'Display Recorded Track
6000    LOCATE 10, 60: PRINT "Load File: "; : INPUT ; "", GUIDO$
        CLS
        GOSUB 400
6005    IF GUIDO$ = "" THEN 6003
        OPEN "R", #2, GUIDO$
        IF LOF(2) > 1 THEN CLOSE #2: GOTO 6006
        CLOSE #2
        GOTO 6003
6006    OPEN "I", #2, GUIDO$
        IF EOF(2) THEN 6002
        INPUT #2, teleX
        IF EOF(2) THEN 6002
        INPUT #2, teleY
        IF EOF(2) THEN 6002
        INPUT #2, teleV
        Xold = 300 + ((Px(ref, 3) + teleX) - cenX) * mag / AU
        Yold = 220 + ((Py(ref, 3) + teleY) - cenY) * mag * 1 / AU
6001    IF EOF(2) THEN 6002
        INPUT #2, teleX
        IF EOF(2) THEN 6002
        INPUT #2, teleY
        IF EOF(2) THEN 6002
        INPUT #2, teleV
        Xnew = 300 + ((Px(ref, 3) + teleX) - cenX) * mag / AU
        Ynew = 220 + ((Py(ref, 3) + teleY) - cenY) * mag * 1 / AU
        IF Xold > 10000 THEN 6004
        IF Yold > 10000 THEN 6004
        IF Xnew > 10000 THEN 6004
        IF Ynew > 10000 THEN 6004
        LINE (Xold, Yold)-(Xnew, Ynew), 10
6004    Xold = Xnew
        Yold = Ynew
        GOTO 6001
6002    CLOSE #2
        OPEN "I", #2, GUIDO$
        INPUT #2, telX1
        INPUT #2, telY1
        INPUT #2, telV1
        INPUT #2, telX2
        INPUT #2, telY2
        INPUT #2, telV2
        telTRACK = 1
        LOCATE 21, 59: PRINT "Track Dev          km";
        LOCATE 22, 59: PRINT "Speed Dev         m/s";
6003    RETURN
    
        'Calculate distance from track
6050    x = Px(28, 3) - Px(ref, 3)
        y = Py(28, 3) - Py(ref, 3)
6053    u = ((x - telX1) * (telX2 - telX1)) + ((y - telY1) * (telY2 - telY1))
        DEL = ((telX1 - telX2) ^ 2) + ((telY1 - telY2) ^ 2)
        IF DEL = 0 THEN 6060
        u = u / DEL
        DEL = SQR(DEL)
        x1 = telX1 + (u * (telX2 - telX1))
        y1 = telY1 + (u * (telY2 - telY1))
        del1 = SQR(((telX1 - x1) ^ 2) + ((telY1 - y1) ^ 2))
        del2 = SQR(((telX2 - x1) ^ 2) + ((telY2 - y1) ^ 2))
        'IF del1 < del2 AND del2 > DEL THEN x1 = telX1: y1 = telY1: GOTO 6051
        IF del1 > del2 AND del1 > DEL THEN 6060
        delV = telV2 - telV1
        delV = telV1 + (delV * del1 / DEL)
        delV = Vrefhab - delV
        IF delV < 1000000 THEN LOCATE 22, 69: PRINT USING "#######"; delV;
      
6051    TELdist = SQR(((x - x1) ^ 2) + ((y - y1) ^ 2))
        IF TELdist < 1000000 THEN LOCATE 21, 69: PRINT USING "####.###"; TELdist / 1000;
6052    RETURN

6060    telX1 = telX2
        telY1 = telY2
        telV1 = telV2
        IF EOF(2) THEN 6070
        INPUT #2, telX2
        IF EOF(2) THEN 6070
        INPUT #2, telY2
        IF EOF(2) THEN 6070
        INPUT #2, telV2
        GOTO 6053

6070    CLOSE #2
        telTRACK = 0
        LOCATE 21, 59: PRINT "                     ";
        LOCATE 22, 59: PRINT "                     ";
        RETURN
       

        'Start track recording
6100    IF TELEflag = 1 THEN 6110
        TELEflag = 1
        telTRACK = 0
        CLOSE #2
        TELEbk = TIMER
6101    LOCATE 9, 60: PRINT "8 charaters a-z 0-9";
        LOCATE 10, 60: PRINT "Save File: "; : INPUT ; "", filename$
        IF filename$ = "" THEN TELEflag = 0: GOTO 6120
        OPEN "R", #2, filename$
        IF LOF(2) < 1 THEN CLOSE #2: GOTO 6105
        CLOSE #2
        LOCATE 11, 60: PRINT "File exists";
        LOCATE 12, 60: PRINT "overwrite? "; : INPUT ; "", z$
        IF UCASE$(LEFT$(z$, 1)) = "Y" THEN 6105
        FOR i = 9 TO 12
         LOCATE i, 60: PRINT "                  ";
        NEXT i
        GOTO 6101
6105    OPEN "O", #2, filename$
        CLS : GOSUB 400
        GOTO 6120
6110    TELEflag = 0
        CLOSE #2
6120    RETURN


9000    IF ERL = 91 THEN CLOSE #1: CLS : PRINT "'stars' file is missing or incomplete"
        IF ERL = 801 THEN CLOSE #1: LOCATE 2, 35: PRINT "Telemetry failed"; : RESUME 804
        IF ERL = 98 AND ERR = 76 THEN CLS : LOCATE 15, 5: PRINT "'"; Zpath$; "' Path not found"; : RESUME 97
        IF ERL = 98 AND ERR = 53 THEN CLS : LOCATE 15, 5: PRINT "Backup file not found in '"; Zpath$; "'"; : RESUME 97
        LOCATE 1, 30: PRINT "unknown error: "; ERR; ERL
        z$ = INPUT$(1)
        END

