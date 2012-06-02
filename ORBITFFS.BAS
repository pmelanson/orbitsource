1111    'SCREEN 12
        'PALETTE 8, 19 + (19 * 256) + (19 * 65536)
        CLS
        DEFDBL A-Z
        DIM P(39, 11), Px(39, 5), Py(39, 5), Vx(39), Vy(39), B(1, 250), Ztel(30), Znme$(39)
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
       
        'X-position  Y-position  X-velocity  Y-velocity  X-acceleration  Y-acceleration
        RESTORE 1000
        READ year, day, HR, min, sec
        FOR i = 1 TO 35
         READ Px(i, 3), Py(i, 3), Vx(i), Vy(i), P(i, 1), P(i, 2)
        NEXT i
        RESTORE 4000
        FOR i = 0 TO 39
         READ Znme$(i)
        NEXT i
        Px(37, 3) = 4446370.8284487# + Px(3, 3): Py(37, 3) = 4446370.8284487# + Py(3, 3): Vx(37) = Vx(3): Vy(37) = Vy(3)
        LOCATE 5, 5
        PRINT "ORBIT v. 5O"
        PRINT
        INPUT "    Do you wish to reload the last timed backup: ", z$
        IF UCASE$(LEFT$(z$, 1)) = "Y" THEN filename$ = "ORbackup": GOSUB 704
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
        'IF dispFLAG = 0 THEN LINE (30, 120)-(30 + (20 * SIN(Atarg)), 120 + (20 * COS(Atarg))), 0
        IF SQR(((Px(28, 3) - cenX) * mag / AU) ^ 2 + ((Py(28, 3) - cenY) * mag * 1 / AU) ^ 2) > 400 THEN 131
        'IF vflag = 1 THEN LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (30 * SIN(Atarg)) + (Px(28, 3) - cenX) * mag / AU, 220 + (30 * COS(Atarg)) + (Py(28, 3) - cenY) * mag * 1 / AU), 0
        
131     CONflag = 0
        'Calculate gravitational acceleration for each object pair
        FOR i = 1 TO 162 + (ufo1 * 37) + (ufo2 * 37)
         IF B(1, i) = 32 AND AYSE = 150 THEN 106
         difx = Px(B(1, i), 3) - Px(B(0, i), 3)
         dify = Py(B(1, i), 3) - Py(B(0, i), 3)
         R = SQR((dify ^ 2) + (difx ^ 2))
         IF R < .01 THEN 106
         A = G * P(B(0, i), 4) / (R ^ 2)
         IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
         IF dify > 0 THEN angle = angle + 3.1415926535#
         IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
         P(B(1, i), 1) = P(B(1, i), 1) + (A * SIN(angle))
         P(B(1, i), 2) = P(B(1, i), 2) + (A * COS(angle))
         IF B(1, i) = 32 AND B(0, 1) <> 28 AND R <= P(B(0, i), 5) + P(32, 5) THEN CONflag2 = 1: CONflag3 = B(0, i)
         IF B(1, i) = 28 AND B(0, 1) <> 32 AND R <= P(B(0, i), 5) + P(28, 5) THEN CONflag = 1: CONtarg = B(0, i): Dcon = R: Acon = angle: CONacc = A
         IF B(1, i) = 39 AND R <= P(B(0, i), 5) + P(39, 5) THEN explCENTER = 39: GOSUB 6000
         IF B(1, i) = 38 AND R <= P(B(0, i), 5) + P(38, 5) THEN explCENTER = 38: GOSUB 6000
         IF B(0, i) = ref AND B(1, i) = 28 THEN Vref = SQR(G * P(B(0, i), 4) / R): Aref = angle
         IF B(0, i) = targ AND B(1, i) = 28 THEN Atarg = angle: Dtarg = R: Acctarg = A
         IF i = 156 AND R < 2100000 AND eng + warp = 0 THEN sednaFLAG = 1
106     NEXT i
       
        'calculate angle to target object if object = 35
        difx = Px(28, 3) - Px(35, 3)
        dify = Py(28, 3) - Py(35, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        IF R <= P(35, 5) + P(28, 5) AND CONtarg < 38 THEN CONflag = 1: Dcon = R: CONtarg = 35: Acon = angle: CONacc = 0
        IF targ = 35 THEN Atarg = angle: Dtarg = R: Acctarg = 0
        IF ref = 35 THEN Aref = angle

102     FOR i = 36 TO 37
         difx = Px(28, 3) - Px(i, 3)
         dify = Py(28, 3) - Py(i, 3)
         R = SQR((dify ^ 2) + (difx ^ 2))
         IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
         IF dify > 0 THEN angle = angle + 3.1415926535#
         IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
         IF targ = i THEN Atarg = angle: Dtarg = R: Acctarg = 0
         IF ref = i THEN Aref = angle
        NEXT i

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
        'IF dispFLAG = 0 THEN LINE (30, 120)-(30 + (5 * SIN(Sangle)), 120 + (5 * COS(Sangle))), 0
        'IF dispFLAG = 0 THEN LINE (30, 120)-(30 + (10 * SIN(Vvangle)), 120 + (10 * COS(Vvangle))), 0
        IF SQR(((Px(28, 3) - cenX) * mag / AU) ^ 2 + ((Py(28, 3) - cenY) * mag * 1 / AU) ^ 2) > 400 THEN 132
        'IF vflag = 1 THEN LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (20 * SIN(Vvangle)) + (Px(28, 3) - cenX) * mag / AU, 220 + (20 * COS(Vvangle)) + (Py(28, 3) - cenY) * mag * 1 / AU), 0
        
    
        'Update object velocities and erase old positions
132     FOR i = 37 + ufo1 + ufo2 TO 0 STEP -1
         IF i = 28 THEN GOSUB 301
         IF i = 38 THEN GOSUB 7200
         Vx(i) = Vx(i) + (P(i, 1) * ts)
         Vy(i) = Vy(i) + (P(i, 2) * ts)
117      IF i = 36 AND MODULEflag = 0 THEN 108
         IF SQR(((Px(i, 3) - cenX) * mag / AU) ^ 2 + ((Py(i, 3) - cenY) * mag * 1 / AU) ^ 2) - (P(i, 5) * mag / AU) > 400 THEN 108
         IF cen * tr > 0 THEN 108
         IF mag * P(i, 5) / AU > 3200 THEN 118
         IF mag * P(i, 5) / AU < 1.1 THEN GOTO 108 'PSET (300 + (Px(i, 3) - cenX) * mag / AU, 220 + (Py(i, 3) - cenY) * mag * 1 / AU), 8 * trail:
         IF i = 28 THEN 128
         IF i = 35 THEN 138
         IF i = 37 THEN 148
         'CIRCLE (300 + (Px(i, 3) - cenX) * mag / AU, 220 + (Py(i, 3) - cenY) * mag * 1 / AU), mag * P(i, 5) / AU, 8 * trail: GOTO 108

118      difx = Px(i, 3) - cenX
         dify = Py(i, 3) - cenY
         R = SQR((dify ^ 2) + (difx ^ 2)) - P(i, 5)
         IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
         IF dify > 0 THEN angle = angle + 3.1415926535#
         IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
         dist = (SQR((cenX - Px(i, 3)) ^ 2 + (cenY - Py(i, 3)) ^ 2) - P(i, 5)) * mag / AU
         csz = COS(angle - 1.5707964#)
         snz = SIN(angle - 1.5707964#)
         'LINE (300 - (dist * csz) - (300 * snz), 220 + (dist * snz) - (300 * csz))-(300 - (dist * csz) + (300 * snz), 220 + (dist * snz) + (300 * csz)), 8 * trail
         GOTO 108

128      'CIRCLE (300 + (Px(i, 3) - cenX) * mag / AU, 220 + (Py(i, 3) - cenY) * mag * 1 / AU), mag * P(i, 5) / AU, 8 * trail
         'CIRCLE (300 + (Px(i, 3) - cenX - (P(i, 5) * .8 * SIN(oldSA))) * mag / AU, 220 + (Py(i, 3) - cenY - (P(i, 5) * .8 * COS(oldSA))) * mag * 1 / AU), mag * P(i, 5) * .2 / AU, 8 * trail
         'CIRCLE (300 + (Px(i, 3) - cenX - (P(i, 5) * 1.2 * SIN(oldSA + .84))) * mag / AU, 220 + (Py(i, 3) - cenY - (P(i, 5) * 1.2 * COS(oldSA + .84))) * mag * 1 / AU), mag * P(i, 5) * .2 / AU, 8 * trail
         'CIRCLE (300 + (Px(i, 3) - cenX - (P(i, 5) * 1.2 * SIN(oldSA - .84))) * mag / AU, 220 + (Py(i, 3) - cenY - (P(i, 5) * 1.2 * COS(oldSA - .84))) * mag * 1 / AU), mag * P(i, 5) * .2 / AU, 8 * trail
         GOTO 108

138      'LINE (300 + (Px(i, 3) - cenX - 450) * mag / AU, 220 + (Py(i, 3) - cenY - 150) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 450) * mag / AU, 220 + (Py(i, 3) - cenY + 150) * mag * 1 / AU), 8 * trail, B
         'LINE (300 + (Px(i, 3) - cenX + 500) * mag / AU, 220 + (Py(i, 3) - cenY + 25) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 450) * mag / AU, 220 + (Py(i, 3) - cenY - 25) * mag * 1 / AU), 8 * trail, B
         'LINE (300 + (Px(i, 3) - cenX - 500) * mag / AU, 220 + (Py(i, 3) - cenY + 25) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX - 450) * mag / AU, 220 + (Py(i, 3) - cenY - 25) * mag * 1 / AU), 8 * trail, B
         'LINE (300 + (Px(i, 3) - cenX - 350) * mag / AU, 220 + (Py(i, 3) - cenY + 200) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 350) * mag / AU, 220 + (Py(i, 3) - cenY + 355) * mag * 1 / AU), 8 * trail, B
         'LINE (300 + (Px(i, 3) - cenX - 350) * mag / AU, 220 + (Py(i, 3) - cenY - 200) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 350) * mag / AU, 220 + (Py(i, 3) - cenY - 355) * mag * 1 / AU), 8 * trail, B
         'LINE (300 + (Px(i, 3) - cenX - 50) * mag / AU, 220 + (Py(i, 3) - cenY + 200) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 50) * mag / AU, 220 + (Py(i, 3) - cenY + 150) * mag * 1 / AU), 8 * trail, B
         'LINE (300 + (Px(i, 3) - cenX - 50) * mag / AU, 220 + (Py(i, 3) - cenY - 200) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 50) * mag / AU, 220 + (Py(i, 3) - cenY - 150) * mag * 1 / AU), 8 * trail, B
         GOTO 108

148      px1 = ((Px(37, 3) + 97.5305) - cenX) * mag / AU: py1 = ((Py(37, 3) + 69.2462) - cenY) * mag / AU
         px2 = ((Px(37, 3) + 69.2462) - cenX) * mag / AU: py2 = ((Py(37, 3) + 97.5305) - cenY) * mag / AU
         px3 = ((Px(37, 3) + 21.2132) - cenX) * mag / AU: py3 = ((Py(37, 3) - 21.2132) - cenY) * mag / AU
         px4 = ((Px(37, 3) - 21.2132) - cenX) * mag / AU: py4 = ((Py(37, 3) + 21.2132) - cenY) * mag / AU
         px5 = ((Px(37, 3) - 55.1041) - cenX) * mag / AU: py5 = ((Py(37, 3) - 111.6726) - cenY) * mag / AU
         px6 = ((Px(37, 3) - 111.6726) - cenX) * mag / AU: py6 = ((Py(37, 3) - 55.1041) - cenY) * mag / AU
         'LINE (300 + px1, 220 + py1)-(300 + px5, 220 + py5), 8 * trail
         'LINE (300 + px5, 220 + py5)-(300 + px6, 220 + py6), 8 * trail
         'LINE (300 + px6, 220 + py6)-(300 + px2, 220 + py2), 8 * trail
         'LINE (300 + px2, 220 + py2)-(300 + px1, 220 + py1), 8 * trail
         'LINE (300 + px1, 220 + py1)-(300 + px4, 220 + py4), 8 * trail
         'LINE (300 + px4, 220 + py4)-(300 + px5, 220 + py5), 8 * trail
         'LINE (300 + px6, 220 + py6)-(300 + px3, 220 + py3), 8 * trail
         'LINE (300 + px3, 220 + py3)-(300 + px2, 220 + py2), 8 * trail
        
108     NEXT i
       
        

        'Detect contact with an object
        explFLAG1 = 0
        IF CONflag = 0 THEN 112
        IF CONtarg > 37 THEN ufo2 = 0: explFLAG1 = 1: eng = 0: targ = ref: GOTO 112
        IF ((Dcon - P(CONtarg, 5) - P(28, 5)) <= 0) AND ((Sflag < 3) OR (eng < CONacc * 1.1)) THEN Vx(28) = Vx(CONtarg): Vy(28) = Vy(CONtarg)
        IF ((Dcon - P(CONtarg, 5) - P(28, 5)) > -.5) THEN 112
        eng = 0
        Px(28, 3) = Px(CONtarg, 3) + ((P(CONtarg, 5) + P(28, 5) - .1) * SIN(Acon + 3.1415926#))
        Py(28, 3) = Py(CONtarg, 3) + ((P(CONtarg, 5) + P(28, 5) - .1) * COS(Acon + 3.1415926#))
        explFLAG1 = 1
        
       

        'Docked with AYSE drive module
112     explFLAG2 = 0
        IF AYSE = 150 OR AYSEadFLAG = 1 THEN Vx(32) = Vx(28): Vy(32) = Vy(28)
        IF CONflag2 = 1 AND CONflag4 = 0 THEN CONflag4 = 1: explFLAG2 = 1
        IF CONflag2 = 1 AND CONflag3 < 38 THEN Vx(32) = Vx(CONflag3): Vy(32) = Vy(CONflag3)

       
        'Update object positions
        FOR i = 0 TO 37 + ufo1 + ufo2 '35
         Px(i, 3) = Px(i, 3) + (Vx(i) * ts)
         Py(i, 3) = Py(i, 3) + (Vy(i) * ts)
        NEXT i
        Px(28, 3) = Px(28, 3) + (warpX * ts)
        Py(28, 3) = Py(28, 3) + (warpY * ts)
        IF AYSE = 150 THEN Px(32, 3) = Px(32, 3) + (warpX * ts)
        IF AYSE = 150 THEN Py(32, 3) = Py(32, 3) + (warpY * ts)
        IF ts > 10 THEN GOSUB 3100
        IF MODULEflag > 0 THEN Px(36, 3) = P(36, 1) + Px(MODULEflag, 3): Py(36, 3) = P(36, 2) + Py(MODULEflag, 3): Vx(36) = Vx(MODULEflag): Vy(36) = Vy(MODULEflag)
        Px(37, 3) = 4446370.8284487# + Px(3, 3): Py(37, 3) = 4446370.8284487# + Py(3, 3): Vx(37) = Vx(3): Vy(37) = Vy(3)
       
        'Record new center position
        cenX = Px(cen, 3) + cenXoff
        cenY = Py(cen, 3) + cenYoff
      
        'Dock with AYSE drive module
        IF AYSEad = 0 AND targ <> 32 THEN 111
        difx = Px(28, 3) - Px(32, 3)
        dify = Py(28, 3) - Py(32, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        IF targ = 32 THEN Atarg = angle: Dtarg = R: Acctarg = 0
        IF AYSEad = 0 THEN 111
        IF eng > 0 AND AYSE = 0 AND AYSEadFLAG = 0 THEN AYSEad = 0: GOTO 111
        IF TTC * GPD * Agen * Abr = 0 THEN 111
        IF R > 1000000 THEN AYSEadFLAG = 0: AYSEad = 0: GOTO 111
        IF AYSEadFLAG = 0 THEN 111
        CONflag2 = 0
        CONflag4 = 0
        Px(32, 3) = Px(28, 3) + (R * .2475 * SIN(angle) / ts)
        Py(32, 3) = Py(28, 3) + (R * .2475 * COS(angle) / ts)
        IF R < 250 THEN AYSEadFLAG = 0: AYSEad = 2
        

       
        'Repaint objects to the screen
111     FOR i = 37 + ufo1 + ufo2 TO 0 STEP -1
         IF i = 36 AND MODULEflag = 0 THEN 109
         IF SQR(((Px(i, 3) - cenX) * mag / AU) ^ 2 + ((Py(i, 3) - cenY) * mag * 1 / AU) ^ 2) - (P(i, 5) * mag / AU) > 400 THEN 109
         IF mag * P(i, 5) / AU > 3200 THEN 119
         pld = 0
         IF i = 28 THEN pld = 2 * ABS(SGN(eng))
         IF mag * P(i, 5) / AU < 1.1 THEN GOTO 109   'PSET (300 + (Px(i, 3) - cenX) * mag / AU, 220 + (Py(i, 3) - cenY) * mag * 1 / AU), P(i, 0) + pld: GOTO 109
         IF i = 28 THEN 129
         IF i = 35 THEN 139
         IF i = 37 THEN 149
         'CIRCLE (300 + (Px(i, 3) - cenX) * mag / AU, 220 + (Py(i, 3) - cenY) * mag * 1 / AU), mag * P(i, 5) / AU, P(i, 0) + pld: GOTO 109

119      difx = Px(i, 3) - cenX
         dify = Py(i, 3) - cenY
         R = SQR((dify ^ 2) + (difx ^ 2)) - P(i, 5)
         IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
         IF dify > 0 THEN angle = angle + 3.1415926535#
         IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
         dist = (SQR((cenX - Px(i, 3)) ^ 2 + (cenY - Py(i, 3)) ^ 2) - P(i, 5)) * mag / AU
         csz = COS(angle - 1.5707964#)
         snz = SIN(angle - 1.5707964#)
         'LINE (300 - (dist * csz) - (300 * snz), 220 + (dist * snz) - (300 * csz))-(300 - (dist * csz) + (300 * snz), 220 + (dist * snz) + (300 * csz)), P(i, 0)
         GOTO 109

129      'CIRCLE (300 + (Px(i, 3) - cenX) * mag / AU, 220 + (Py(i, 3) - cenY) * mag * 1 / AU), mag * P(i, 5) / AU, P(i, 0) + pld
         'CIRCLE (300 + (Px(i, 3) - cenX - (P(i, 5) * .8 * SIN(Sangle))) * mag / AU, 220 + (Py(i, 3) - cenY - (P(i, 5) * .8 * COS(Sangle))) * mag * 1 / AU), mag * P(i, 5) * .2 / AU, P(i, 0) + pld
         'CIRCLE (300 + (Px(i, 3) - cenX - (P(i, 5) * 1.2 * SIN(Sangle + .84))) * mag / AU, 220 + (Py(i, 3) - cenY - (P(i, 5) * 1.2 * COS(Sangle + .84))) * mag * 1 / AU), mag * P(i, 5) * .2 / AU, P(i, 0) + pld
         'CIRCLE (300 + (Px(i, 3) - cenX - (P(i, 5) * 1.2 * SIN(Sangle - .84))) * mag / AU, 220 + (Py(i, 3) - cenY - (P(i, 5) * 1.2 * COS(Sangle - .84))) * mag * 1 / AU), mag * P(i, 5) * .2 / AU, P(i, 0) + pld
         GOTO 109
        
139      'LINE (300 + (Px(i, 3) - cenX - 450) * mag / AU, 220 + (Py(i, 3) - cenY - 150) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 450) * mag / AU, 220 + (Py(i, 3) - cenY + 150) * mag * 1 / AU), P(i, 0), B
         'LINE (300 + (Px(i, 3) - cenX + 500) * mag / AU, 220 + (Py(i, 3) - cenY + 25) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 450) * mag / AU, 220 + (Py(i, 3) - cenY - 25) * mag * 1 / AU), P(i, 0), B
         'LINE (300 + (Px(i, 3) - cenX - 500) * mag / AU, 220 + (Py(i, 3) - cenY + 25) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX - 450) * mag / AU, 220 + (Py(i, 3) - cenY - 25) * mag * 1 / AU), P(i, 0), B
         'LINE (300 + (Px(i, 3) - cenX - 350) * mag / AU, 220 + (Py(i, 3) - cenY + 200) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 350) * mag / AU, 220 + (Py(i, 3) - cenY + 355) * mag * 1 / AU), P(i, 0), B
         'LINE (300 + (Px(i, 3) - cenX - 350) * mag / AU, 220 + (Py(i, 3) - cenY - 200) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 350) * mag / AU, 220 + (Py(i, 3) - cenY - 355) * mag * 1 / AU), P(i, 0), B
         'LINE (300 + (Px(i, 3) - cenX - 50) * mag / AU, 220 + (Py(i, 3) - cenY + 200) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 50) * mag / AU, 220 + (Py(i, 3) - cenY + 150) * mag * 1 / AU), P(i, 0), B
         'LINE (300 + (Px(i, 3) - cenX - 50) * mag / AU, 220 + (Py(i, 3) - cenY - 200) * mag * 1 / AU)-(300 + (Px(i, 3) - cenX + 50) * mag / AU, 220 + (Py(i, 3) - cenY - 150) * mag * 1 / AU), P(i, 0), B
         GOTO 109

149      px1 = ((Px(37, 3) + 97.5305) - cenX) * mag / AU: py1 = ((Py(37, 3) + 69.2462) - cenY) * mag / AU
         px2 = ((Px(37, 3) + 69.2462) - cenX) * mag / AU: py2 = ((Py(37, 3) + 97.5305) - cenY) * mag / AU
         px3 = ((Px(37, 3) + 21.2132) - cenX) * mag / AU: py3 = ((Py(37, 3) - 21.2132) - cenY) * mag / AU
         px4 = ((Px(37, 3) - 21.2132) - cenX) * mag / AU: py4 = ((Py(37, 3) + 21.2132) - cenY) * mag / AU
         px5 = ((Px(37, 3) - 55.1041) - cenX) * mag / AU: py5 = ((Py(37, 3) - 111.6726) - cenY) * mag / AU
         px6 = ((Px(37, 3) - 111.6726) - cenX) * mag / AU: py6 = ((Py(37, 3) - 55.1041) - cenY) * mag / AU
         'LINE (300 + px1, 220 + py1)-(300 + px5, 220 + py5), 12
         'LINE (300 + px5, 220 + py5)-(300 + px6, 220 + py6), 12
         'LINE (300 + px6, 220 + py6)-(300 + px2, 220 + py2), 12
         'LINE (300 + px2, 220 + py2)-(300 + px1, 220 + py1), 12
         'LINE (300 + px1, 220 + py1)-(300 + px4, 220 + py4), 12
         'LINE (300 + px4, 220 + py4)-(300 + px5, 220 + py5), 12
         'LINE (300 + px6, 220 + py6)-(300 + px3, 220 + py3), 12
         'LINE (300 + px3, 220 + py3)-(300 + px2, 220 + py2), 12

109     NEXT i
        oldSA = Sangle
      

        'Re-paint target vector
        'IF dispFLAG = 0 THEN LINE (30, 120)-(30 + (20 * SIN(Atarg)), 120 + (20 * COS(Atarg))), 8
       
       
        'Repaint velocity and orientation vectors
        VVx = Vx(targ) - Vx(28)
        VVy = Vy(targ) - Vy(28)
        IF VVy = 0 THEN IF VVx < 0 THEN Vvangle = .5 * 3.1415926535# ELSE Vvangle = 1.5 * 3.1415926535# ELSE Vvangle = ATN(VVx / VVy)
        IF VVy > 0 THEN Vvangle = Vvangle + 3.1415926535#
        IF Vvangle < 0 THEN Vvangle = Vvangle + 6.283185
        'IF dispFLAG = 0 THEN LINE (30, 120)-(30 + (10 * SIN(Vvangle)), 120 + (10 * COS(Vvangle))), 12
        'IF dispFLAG = 0 THEN LINE (30, 120)-(30 + (5 * SIN(Sangle)), 120 + (5 * COS(Sangle))), 10
        'IF dispFLAG = 0 THEN PSET (30, 120), 1
        IF SQR(((Px(28, 3) - cenX) * mag / AU) ^ 2 + ((Py(28, 3) - cenY) * mag * 1 / AU) ^ 2) > 400 THEN 133
        'IF vflag = 1 THEN LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (30 * SIN(Atarg)) + (Px(28, 3) - cenX) * mag / AU, 220 + (30 * COS(Atarg)) + (Py(28, 3) - cenY) * mag * 1 / AU), 8
        'IF vflag = 1 THEN LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (20 * SIN(Vvangle)) + (Px(28, 3) - cenX) * mag / AU, 220 + (20 * COS(Vvangle)) + (Py(28, 3) - cenY) * mag * 1 / AU), 12
133     VangleDIFF = Atarg - Vvangle
       
       
        'Cause explosion
        IF explFLAG1 = 1 THEN explCENTER = 28: GOSUB 6000
        IF explFLAG2 = 1 THEN explCENTER = 32: GOSUB 6000


        'Update simulation time
        ktmr = ktmr + 1
        sec = sec + ts
        IF sec > 60 THEN min = min + 1: sec = sec - 60
        IF dte = 1 AND ts < 60 THEN LOCATE 25, 72: PRINT USING "###"; min; sec;
        IF min = 60 THEN HR = HR + 1: min = 0: IF dte = 1 AND ts < 3600 THEN LOCATE 25, 69: PRINT USING "###"; HR; min;
        IF HR = 24 THEN day = day + 1: HR = 0: IF dte = 1 THEN LOCATE 25, 66: PRINT USING "###"; day; HR;
        dayNUM = 365
        IF INT(year / 4) * 4 = year THEN dayNUM = 366
        IF INT(year / 100) * 100 = year THEN dayNUM = 365
        IF INT(year / 400) * 400 = year THEN dayNUM = 366
        IF day = dayNUM + 1 THEN year = year + 1: day = 1: IF dte = 1 THEN LOCATE 25, 60: PRINT USING "####"; year; : LOCATE 25, 66: PRINT USING "###"; day;
       
       
        'Print Simulation data
        IF dispFLAG = 1 THEN 113
        IF ref = 33 AND sednaFLAG = 0 THEN Vref = 0
        LOCATE 2, 12
        'IF Vref > 9999999 THEN PRINT USING "##.####^^^^"; Vref ELSE PRINT USING "########.##"; Vref;
        'LOCATE 2, 10: PRINT USING "########.##"; SQR(Vx(28) ^ 2 + Vy(28) ^ 2);
        Vrefhab = SQR((Vx(28) - Vx(ref)) ^ 2 + (Vy(28) - Vy(ref)) ^ 2)
        LOCATE 3, 12
        'IF Vrefhab > 9999999 THEN PRINT USING "##.####^^^^"; Vrefhab;  ELSE PRINT USING "########.##"; Vrefhab;
        Vreftarg = SQR((Vx(targ) - Vx(ref)) ^ 2 + (Vy(targ) - Vy(ref)) ^ 2)
        LOCATE 4, 12
        'IF Vreftarg > 9999999 THEN PRINT USING "##.####^^^^"; Vreftarg;  ELSE PRINT USING "########.##"; Vreftarg;
        IF COS(VangleDIFF) <> 0 AND Dtarg - P(targ, 5) <> 0 THEN Acctarg = Acctarg + ((((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2) / (2 * (Dtarg - P(targ, 5)))) * COS(VangleDIFF))
        LOCATE 14, 7
        'IF ABS(Acctarg) > 9999 THEN PRINT USING "##.##^^^^"; Acctarg;  ELSE PRINT USING "######.##"; Acctarg;
       
        LOCATE 13, 2
        'IF Dtarg > 9.9E+11 THEN PRINT USING "##.########^^^^"; (Dtarg - P(targ, 5) - P(28, 5)) / 1000;  ELSE PRINT USING "###,###,###.###"; (Dtarg - P(targ, 5) - P(28, 5)) / 1000;
        'LOCATE 15, 9: PRINT USING "####.##"; Atargref * RAD;
       
        IF PROJflag = 0 THEN 113
        Vcen = SQR(((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2)) * -1 * COS(VangleDIFF)
        Vtan = SQR(((Vx(28) - Vx(targ)) ^ 2 + (Vy(28) - Vy(targ)) ^ 2)) * ABS(SIN(VangleDIFF))
        'LOCATE 16, 7
        'IF ABS(Vcen) > 99999 THEN PRINT USING "##.##^^^^"; Vcen;  ELSE PRINT USING "######.##"; Vcen;
        'LOCATE 17, 7
        'IF ABS(Vtan) > 99999 THEN PRINT USING "##.##^^^^"; Vtan;  ELSE PRINT USING "######.##"; Vtan;

       
113     IF SYSflag = 2 THEN LOCATE 2, 60: 'COLOR 12 - AYSEad: PRINT "Auto Docking    "; : COLOR 15
       
        'Timed back-up
        'IF bkt - TIMER > 120 THEN bkt = TIMER
        'IF bkt + 5 < TIMER THEN bkt = TIMER: GOSUB 800
        'IF ufo2 = 1 THEN Px(39, 1) = Px(39, 1) - 1
        'IF ufo2 = 1 AND Px(39, 1) < 1 THEN explCENTER = 39: GOSUB 6000
       

        'Control input
103     z$ = INKEY$
        IF z$ = "" THEN 105
        IF z$ = "q" THEN GOSUB 900
        'IF z$ = "`" THEN dispFLAG = 1 - dispFLAG: CLS : IF dispFLAG = 0 THEN GOSUB 400
        'IF z$ = CHR$(27) THEN GOSUB 910
        'IF z$ = "w" THEN warp = (1 - warp) * SGN(AYSE) * SGN(Acon1 + Acon2): GOSUB 400
        'IF z$ = " " THEN cen = targ: cenXoff = Px(28, 3) - Px(cen, 3): cenYoff = Py(28, 3) - Py(cen, 3)
        'IF z$ = CHR$(9) THEN Aflag = Aflag + 1: IF Aflag = 3 THEN Aflag = 0: GOSUB 400 ELSE GOSUB 400
        'IF z$ = CHR$(0) + ";" THEN Sflag = 1: GOSUB 400
        'IF z$ = CHR$(0) + "<" THEN Sflag = 0: GOSUB 400
        'IF z$ = CHR$(0) + "=" THEN Sflag = 4: GOSUB 400
        'IF z$ = CHR$(0) + ">" THEN Sflag = 2: GOSUB 400
        'IF z$ = CHR$(0) + "?" THEN Sflag = 3: GOSUB 400
        'IF z$ = CHR$(0) + "@" THEN SYSflag = SYSflag + 1: GOSUB 400
        'IF z$ = "+" AND mag < 130000000000# THEN mag = mag / .75: CLS : GOSUB 400
        'IF z$ = "-" AND mag > 6.8E-11 THEN mag = mag * .75: CLS : GOSUB 400
        'IF z$ = CHR$(0) + "I" THEN LINE (30, 120)-(30 + (5 * SIN(Sangle)), 120 + (5 * COS(Sangle))), 0: Sangle = Sangle - (1 / RAD)
        'IF z$ = CHR$(0) + "G" THEN LINE (30, 120)-(30 + (5 * SIN(Sangle)), 120 + (5 * COS(Sangle))), 0: Sangle = Sangle + (1 / RAD)
       
        'IF z$ = "[" THEN GOSUB 460
        'IF z$ = "]" THEN GOSUB 465
        'IF z$ < "0" OR z$ > "U" THEN 110
        'z = ASC(z$) - 48
        'IF z = 36 AND MODULEflag = 0 THEN 110
        'IF Aflag = 0 THEN cen = z: CLS : cenXoff = 0: cenYoff = 0: GOSUB 400
        'IF z = 28 THEN 110
        'IF Aflag = 1 THEN targ = z: GOSUB 400
        'IF Aflag = 2 THEN ref = z: GOSUB 400
        
      
110     'IF z$ = CHR$(0) + "S" AND eng < 50 + AYSE THEN eng = eng + .1: GOSUB 400
        'IF z$ = CHR$(0) + "R" AND eng > -1 * (50 + (AYSE * TTC * GPD * Agen * Abr)) THEN eng = eng - .1: GOSUB 400
        'IF z$ = CHR$(0) + "Q" AND eng < 50 + AYSE THEN eng = eng + 1: GOSUB 400
        'IF z$ = CHR$(0) + "O" AND eng > -1 * (50 + (AYSE * TTC * GPD * Agen * Abr)) THEN eng = eng - 1: GOSUB 400
        'IF z$ = "\" THEN eng = eng * -1: GOSUB 400
        'IF z$ = CHR$(13) THEN eng = 50 + (AYSE * TTC * GPD * Agen * Abr): GOSUB 400
        'IF z$ = CHR$(8) THEN eng = 0: GOSUB 400
        'IF z$ = CHR$(0) + "H" THEN vern = .1: vernA = 0
        'IF z$ = CHR$(0) + "K" THEN vern = .1: vernA = 90
        'IF z$ = CHR$(0) + "M" THEN vern = .1: vernA = -90
        'IF z$ = CHR$(0) + "P" THEN vern = .1: vernA = 180
       
        'IF z$ <> "v" THEN 107
        'vflag = 1 - vflag
        'LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (20 * SIN(Vvangle)) + (Px(28, 3) - cenX) * mag / AU, 220 + (20 * COS(Vvangle)) + (Py(28, 3) - cenY) * mag * 1 / AU), 0
        'LINE (300 + (Px(28, 3) - cenX) * mag / AU, 220 + (Py(28, 3) - cenY) * mag * 1 / AU)-(300 + (30 * SIN(Atarg)) + (Px(28, 3) - cenX) * mag / AU, 220 + (30 * COS(Atarg)) + (Py(28, 3) - cenY) * mag * 1 / AU), 0
107     'IF z$ = "t" THEN trail = 1 - trail: CLS : GOSUB 400
        'IF z$ = "u" THEN tr = 1 - tr: CLS : GOSUB 400
        IF z$ = "d" THEN dte = 1 - dte: LOCATE 25, 60: PRINT USING "####"; year; : LOCATE 25, 66: PRINT USING "###"; day; HR; min; sec; : GOSUB 400
        'IF z$ = "p" THEN PROJflag = 1 - PROJflag: GOSUB 400
        'IF z$ = "o" THEN GOSUB 3000
        'IF z$ = "n" THEN GOSUB 3000
        'IF z$ = "c" THEN CLS : GOSUB 400
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
105     'IF SYSflag > 0 AND LEN(z$) > 1 THEN GOSUB 500
        IF z$ = "s" THEN GOSUB 600
        IF z$ = "r" THEN GOSUB 700
        'IF z$ = "z" THEN GOSUB 799
        IF tttt - TIMER > ts * 10 THEN tttt = TIMER + (ts / 2)
        IF ts < .3 AND TIMER - tttt < ts THEN 103
        GOTO 100


        'SUBROUTINE Automatic space craft orientation calculations
301     IF Ztel(1) = 1 THEN Sflag = 1
        IF eng > ((50 * ion * acc * Hbr) + (AYSE * TTC * GPD * Agen * Abr)) * (1 - Ztel(2)) THEN eng = ((50 * ion * acc * Hbr) + (AYSE * TTC * GPD * Agen * Abr)) * (1 - Ztel(2)): 'LOCATE 5, 15: COLOR 15: PRINT USING "####.#"; eng;
        'COLOR 15 - (3 * SGN(Ztel(2)))
        IF AG < 1 AND ABS(eng) > 60 THEN eng = SGN(eng) * 60 ': LOCATE 5, 2: PRINT "Engine"; : LOCATE 5, 16: PRINT USING "####.#"; eng;
        IF Abr <> 1 AND ABS(eng) > 50 THEN eng = SGN(eng) * 50 ': : LOCATE 5, 2: PRINT "Engine"; : LOCATE 5, 16: PRINT USING "####.#"; eng;
        'COLOR 15
        IF Sflag = 1 THEN 302
        trg = ref
        IF Sflag = 2 THEN trg = targ
        difx = Px(i, 3) - Px(trg, 3)
        dify = Py(i, 3) - Py(trg, 3)
        IF dify = 0 THEN IF difx < 0 THEN Sangle = .5 * 3.1415926535# ELSE Sangle = 1.5 * 3.1415926535# ELSE Sangle = ATN(difx / dify)
        IF dify > 0 THEN Sangle = Sangle + 3.1415926535#
        IF Sflag = 0 THEN Sangle = Sangle - (90 / RAD)
        IF Sflag = 4 THEN Sangle = Sangle + (90 / RAD)
        IF Sflag = 3 THEN Sangle = Sangle - (180 / RAD)
302     P(i, 1) = P(i, 1) + (eng * SIN(Sangle))
        P(i, 2) = P(i, 2) + (eng * COS(Sangle))
        P(i, 1) = P(i, 1) + vern * SIN(Sangle + (vernA / RAD))
        P(i, 2) = P(i, 2) + vern * COS(Sangle + (vernA / RAD))
        vern = 0
        warpX = 3E+08 * warp * SIN(Sangle)
        warpY = 3E+08 * warp * COS(Sangle)
        RETURN


        'SUBROUTINE print control variable names to screen
400     IF mag < .1 THEN GOSUB 8000
        IF ts < .125 THEN ts = .125
        IF ts > 60 THEN ts = 60
        IF ufo2 = 1 THEN ts = .25
        'COLOR 8
        'LOCATE 1, 1: PRINT CHR$(201); STRING$(9, 205); CHR$(209); STRING$(11, 205); CHR$(187);
        'LOCATE 2, 1: PRINT CHR$(186); : LOCATE 2, 11: PRINT CHR$(179); : LOCATE 2, 23: PRINT CHR$(186);
        'LOCATE 3, 1: PRINT CHR$(186); : LOCATE 3, 11: PRINT CHR$(179); : LOCATE 3, 23: PRINT CHR$(186);
        'LOCATE 4, 1: PRINT CHR$(186); : LOCATE 4, 11: PRINT CHR$(179); : LOCATE 4, 23: PRINT CHR$(186);
        'LOCATE 5, 1: PRINT CHR$(186); : LOCATE 5, 11: PRINT CHR$(179); : LOCATE 5, 23: PRINT CHR$(186);
        'LOCATE 6, 1: PRINT CHR$(204); STRING$(9, 205); CHR$(207); STRING$(5, 205); CHR$(203); STRING$(5, 205); CHR$(188);
        'COLOR 15
        IF Ztel(1) = 1 THEN Sflag = 1
        IF eng > ((50 * ion * acc * Hbr) + (AYSE * TTC * GPD * Agen * Abr)) * (1 - Ztel(2)) THEN eng = ((50 * ion * acc * Hbr) + (AYSE * TTC * GPD * Agen * Abr)) * (1 - Ztel(2))
        'IF master > 0 THEN COLOR 12: LOCATE 1, 35: PRINT "MASTER ALARM"; : COLOR 15
        'LOCATE 2, 2: PRINT "ref Vo";
        'LOCATE 2, 2: PRINT "V hab ";
        'LOCATE 3, 2: PRINT "V hab-ref";
        'LOCATE 4, 2: PRINT "Vtarg-ref";
        'COLOR 15 - (3 * SGN(Ztel(2)))
        'LOCATE 5, 2: PRINT "Engine"; : LOCATE 5, 16: COLOR 15: PRINT USING "####.#"; eng;
        'LOCATE 9, 11: IF warp = 1 THEN PRINT "WARP";  ELSE PRINT "    ";
        'COLOR 15 - (3 * Ztel(1))
        'LOCATE 25, 2: PRINT "NAVmode";
        'COLOR 15
        'LOCATE 25, 11
        'IF Sflag = 0 THEN PRINT "ccw prog "; : GOTO 401
        'IF Sflag = 4 THEN PRINT "ccw retro"; : GOTO 401
        'IF Sflag = 1 THEN PRINT "manual   "; : GOTO 401
        'IF Sflag = 2 THEN PRINT "app targ "; : GOTO 401
        'IF Sflag = 3 THEN PRINT "deprt ref";
401     'COLOR 8
        'LOCATE 21, 1: PRINT CHR$(204); STRING$(7, 205); CHR$(209); STRING$(7, 205); CHR$(202); CHR$(205); CHR$(205); CHR$(187);
        'LOCATE 22, 1: PRINT CHR$(186); : LOCATE 22, 9: PRINT CHR$(179); : LOCATE 22, 20: PRINT CHR$(186);
        'LOCATE 23, 1: PRINT CHR$(186); : LOCATE 23, 9: PRINT CHR$(179); : LOCATE 23, 20: PRINT CHR$(186);
        'LOCATE 24, 1: PRINT CHR$(186); : LOCATE 24, 9: PRINT CHR$(179); : LOCATE 24, 20: PRINT CHR$(186);
        'LOCATE 25, 1: PRINT CHR$(186); : LOCATE 25, 9: PRINT CHR$(179); : LOCATE 25, 20: PRINT CHR$(186);
        'LOCATE 26, 1: PRINT CHR$(200); STRING$(7, 205); CHR$(207); STRING$(10, 205); CHR$(188);
        'LOCATE 10, 1: PRINT CHR$(204); STRING$(15, 205); CHR$(185);
        'LOCATE 11, 1: PRINT CHR$(186); : LOCATE 11, 6: PRINT CHR$(179); : LOCATE 11, 17: PRINT CHR$(186);
        'LOCATE 12, 1: PRINT CHR$(199); STRING$(15, 196); CHR$(182);
        'LOCATE 13, 1: PRINT CHR$(186); : LOCATE 13, 17: PRINT CHR$(186);
        'LOCATE 14, 1: PRINT CHR$(186); : LOCATE 14, 17: PRINT CHR$(186);
        'LOCATE 15, 1: PRINT CHR$(186); : LOCATE 15, 17: PRINT CHR$(186);
        'LOCATE 16, 1: PRINT CHR$(186); : LOCATE 16, 17: PRINT CHR$(186);
        'LOCATE 17, 1: PRINT CHR$(186); : LOCATE 17, 17: PRINT CHR$(186);
        'LOCATE 18, 1: PRINT CHR$(204); STRING$(15, 205); CHR$(185);
        'LOCATE 19, 1: PRINT CHR$(186); : LOCATE 19, 17: PRINT CHR$(186);
        'LOCATE 20, 1: PRINT CHR$(186); : LOCATE 20, 17: PRINT CHR$(186);
        'LOCATE 7, 1: PRINT CHR$(186); : LOCATE 7, 17: PRINT CHR$(186);
        'LOCATE 8, 1: PRINT CHR$(186); : LOCATE 8, 17: PRINT CHR$(186);
        'LOCATE 9, 1: PRINT CHR$(186); : LOCATE 9, 17: PRINT CHR$(186);
        'IF dte = 1 THEN LOCATE 24, 59: PRINT CHR$(201); STRING$(18, 205); CHR$(187);
        'IF dte = 1 THEN LOCATE 25, 59: PRINT CHR$(186); : LOCATE 25, 78: PRINT CHR$(186);
        'IF dte = 1 THEN LOCATE 26, 59: PRINT CHR$(200); STRING$(18, 205); CHR$(188);
        'COLOR 15
        'IF Aflag = 0 THEN COLOR 10 ELSE COLOR 15
        'LOCATE 22, 2: PRINT "center "; : COLOR 15: LOCATE 22, 11: PRINT Znme$(cen);
        'IF Aflag = 1 THEN COLOR 10 ELSE COLOR 15
        'LOCATE 23, 2: PRINT "target "; : COLOR 15: LOCATE 23, 11: PRINT Znme$(targ);
        'IF Aflag = 2 THEN COLOR 10 ELSE COLOR 15
        'LOCATE 24, 2: PRINT "ref    "; : COLOR 15: LOCATE 24, 11: PRINT Znme$(ref);
        LOCATE 8, 10: PRINT USING "##.###"; ts;
        'LOCATE 11, 2: PRINT "TARGET DATA";
        'LOCATE 14, 2: PRINT "Acc";
        'LOCATE 15, 2: PRINT CHR$(233);
        'LOCATE 16, 2: PRINT "Vcen";
        'LOCATE 17, 2: PRINT "Vtan";
        'LOCATE 19, 2: PRINT "Peri";
        'LOCATE 20, 2: PRINT "Apo ";
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
        LOCATE 7, 60: PRINT "                ";
        LOCATE 8, 60: PRINT "                ";
        RETURN

420     LOCATE 2, 60: COLOR 12 - (AYSEad * 2):  PRINT "Auto Docking    "; : COLOR 15
        LOCATE 3, 60: COLOR 12 - (AYSEdl * 2):   PRINT "Docking Latch   "; : COLOR 15
        LOCATE 4, 60: COLOR 12 - (AYSEdla * 2):   PRINT "Dck Latch ARMED "; : COLOR 15
        LOCATE 5, 60: COLOR 12 - (AYSEtr * 2):  PRINT "Thermal Roll    "; : COLOR 15
        LOCATE 6, 60: PRINT "                ";
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
        RETURN

520     IF z$ = CHR$(0) + "A" AND AYSEadFLAG = 1 THEN 521
        IF z$ = CHR$(0) + "A" AND AYSEdl = 0 THEN IF AYSEad = 0 THEN AYSEad = 1: AYSEadFLAG = 1 ELSE AYSEad = 0: AYSEadFLAG = 0
        IF z$ = CHR$(0) + "B" AND AYSEdla = 1 AND AYSEad > 1.5 THEN AYSEdl = 1 - AYSEdl: AYSEadFLAG = 0: LOCATE 3, 60: COLOR 12 - (AYSEdl * 2): PRINT "Docking Latch   "; : COLOR 15
         IF z$ = CHR$(0) + "B" AND AYSEdla = 1 AND AYSEad = 0 THEN GOSUB 3200: GOSUB 400
        IF z$ = CHR$(0) + "C" THEN AYSEdla = 1 - AYSEdla: LOCATE 4, 60: COLOR 12 - (AYSEdla * 2): PRINT "Dck Latch ARMED "; : COLOR 15
        IF AYSEdl = 1 AND AYSEad > 1.5 THEN AYSE = 150 ELSE AYSE = 0
521     IF z$ = CHR$(0) + "D" THEN AYSEtr = 1 - AYSEtr: LOCATE 5, 60: COLOR 12 - (AYSEtr * 2): PRINT "Thermal Roll    "; : COLOR 15
        RETURN

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



        'SUBROUTINE save data to file
600     LOCATE 9, 60: PRINT "8 charaters a-z 0-9";
        LOCATE 10, 60: PRINT "Save File: "; : INPUT ; "", filename$
        IF filename$ = "" THEN 620
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
        PRINT #1, master; ","; MODULEflag; ","; AYSEdist; ","; OCESSdist; ","; explosion; ","; explosion1
        PRINT #1, Ztel(1); ","; Ztel(2); ","; Vx(0); ","; Vy(0); ","; P(0, 1); ","; P(0, 2)
        FOR i = 1 TO 39
         PRINT #1, Px(i, 3); ","; Py(i, 3); ","; Vx(i); ","; Vy(i); ","; P(i, 1); ","; P(i, 2)
        NEXT i
        PRINT #1, fuel; ","; AYSEfuel
        CLOSE #1
620     CLS
        GOSUB 400
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
        INPUT #1, master, MODULEflag, AYSEdist, OCESSdist, explosion, explosion1
        INPUT #1, Px(0, 3), Py(0, 3), Vx(0), Vy(0), P(0, 1), P(0, 2)
        Px(0, 3) = 0
        Py(0, 3) = 0
        FOR i = 1 TO 37
         INPUT #1, Px(i, 3), Py(i, 3), Vx(i), Vy(i), P(i, 1), P(i, 2)
        NEXT i
        FOR i = 1 TO 12
         INPUT #1, z1
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


        'Confirm end program
900     LOCATE 10, 60: INPUT ; "End Program "; z$
        IF UCASE$(z$) = "Y" THEN END
        LOCATE 10, 60: PRINT "                   ";
        RETURN

        'Name and author
910     GOSUB 451
        IF authflag = 1 THEN authflag = 0: RETURN
        LOCATE 2, 60: PRINT "Orbit v.5 bld. O   ";
        LOCATE 3, 60: PRINT "       by          "
        RESTORE 920
        LOCATE 4, 60: PRINT "  ";
        FOR i = 1 TO 5: READ z: PRINT CHR$(z - 11); : NEXT i
        PRINT " ";
        FOR i = 1 TO 7: READ z: PRINT CHR$(z - 11); : NEXT i
        LOCATE 5, 60: PRINT "  for the use of"
        LOCATE 6, 60: PRINT "    the OCESS";
        authflag = 1
        RETURN
920     DATA    85,108,120,112,126
        DATA    88,108,114,130,122,122,111

1000    DATA 2004 , 109 , 1 , 21 , 4.6363525390625
        DATA  5678310129.078904# ,-56686216636.08996# ,-46807.43066018598# ,-14388.09972143655# ,-4.07602660226272D-03  , 4.069049172706571D-02
        DATA -587021040.9336134# ,-107487514357.6818# ,-35193.26082630821# , 203.8286299863534# , 6.273603016240423D-05 , 1.148708903955742D-02
        DATA -60812715434.98506# ,-133925930692.8104# ,-27509.49221664917# , 12706.8541293456#  , 2.53667719213413D-03  , 5.585839797291429D-03
        DATA  231779436006.8387# ,-68535755967.71564# ,-4814.341334215045# ,-22212.2289739346#  ,-2.17871825651253D-03  , 6.439848813334939D-04
        DATA  177610430215.7743# ,-792042104119.012#  ,-12127.2750402101#  ,-3014.915851091514# ,-4.405513680413558D-05 , 1.965715792411807D-04
        DATA  1323421553085.668# ,-320361096119.0074# ,-2098.016402006654# ,-9865.294106258678# ,-6.965331905523109D-05 , 1.681137809307842D-05
        DATA -1343188406299.591# , 2681415765727.077# , 5769.302633928196# , 3003.965380420755# , 6.612634126677743D-06 ,-1.320358811582477D-05
        DATA -3272944898528.763# , 3087939999886.421# , 3749.053844330883# , 3938.341047755119# , 4.773514765424081D-06 ,-4.502997735266399D-06
        DATA -4557963539942.463# ,-753182540713.8163# ,-1676.726598021215# , 5694.539658770696# , 7.949327642024766D-06 ,-5.06025331499927D-07
        DATA -60654632365.97327# ,-133577697947.0275# ,-26579.39806053491# , 12284.52768595019# , 5.206616560788601D-03 , 4.960330145573113D-03
        DATA  231770081146.6454# ,-68535454740.72126# ,-4759.185909400531# ,-20071.1926729495#  , .4865958277722187#    ,-1.503828907776151D-02
        DATA  231801710607.1298# ,-68527647932.21262# ,-4359.912549236539# ,-23471.21883580028# ,-7.382166329357438D-02 ,-2.543668911331626D-02
        DATA  177188754031.5883# ,-792040491770.4265# ,-12076.39889890975# , 14331.49989007556# , .7129849141966721#    ,-2.515088529592792D-03
        DATA  177594465426.3817# ,-792713257418.0233# ,-25862.62735576577# ,-2699.402831854644# , 6.642817996497237D-03 , .2814270738571981#
        DATA  176834543801.0591# ,-791304576485.4255# ,-4632.244620741876# , 4872.364987092844# , 8.014747308286678D-02 ,-7.602914160846631D-02
        DATA  177240086305.232#  ,-790198278576.8878# ,-4078.727393337891# ,-1397.786940330671# , 7.014984234180018D-03 ,-3.494804410618806D-02
        DATA  1323711952024.421# ,-320308766992.5206# ,-61.86226888510446# ,-21016.71589292913# ,-.4292352946460722#    ,-7.732622216573852D-02
        DATA  1323050403902.87#  ,-320429889882.2803# ,-3919.708869488942# ,-3.414178924931288# , .2619479146670379#    , 4.858629835264849D-02
        DATA  1323199634110.595# ,-319883360130.4954# , 5603.312707660197# ,-6285.923515614376# , 5.757895673432819D-02 ,-.124083493344816#
        DATA  1324265171925.324# ,-321247046164.5846# ,-6128.968548223604# ,-13705.89452332595# ,-1.756478833019726D-02 , 1.838997449834441D-02
        DATA  1326401343446.097# ,-318409631888.9748# ,-303.813187120205#  ,-12592.82171603941# ,-2.572942424319465D-03 ,-1.62294735872919D-03
        DATA -1343354301694.183# , 2681319737138.294# , 3011.79407087964#  , 7764.901731883898# , .1365949986236487#    , 7.905364455309262D-02
        DATA -1342925208875.048# , 2681368865845.182# , 4956.707717720317# ,-1581.531862570569# ,-.0798691391270067#    , 1.421936817305784D-02
        DATA -1342794528241.828# , 2681608926206.688# , 7370.556554050535# ,-260.184655171311#  ,-2.704679748092501D-02 ,-1.328050799166438D-02
        DATA -1342674502698.396# , 2681698985066.239# , 7286.498593318408# , 251.2243264797397# ,-1.474357258296337D-02 ,-8.142273213855545D-03
        DATA -3272803764100.605# , 3088070061364.723# , 7810.595328905451# ,-454.0164830147976# ,-.1372083407472857     ,-.1264562661542762#
        DATA -4557822885553.744# ,-753300926119.9912# ,-1714.597250155068# , 5642.024480180919# ,-1.047960846396903D-05 , 1.499908002995975D-05
        DATA -60806523031.3544#  ,-133928389295.9249# ,-30367.86554339824# , 5510.365947638687# ,-8.35438924122654#     , 3.317955759579462#
        DATA -435631423796.0132# , 81438474181.01878# , 3220.703623279241# , 16764.74625843465# , 6.643265818463352D-04 ,-1.242706842471708D-04
        DATA -435631511413.3691# , 81438452823.23042# , 3218.635059889318# , 16773.08689844408# , 1.461464792290308D-03 , 7.008177376321788D-05
        DATA -260215152600.6913# , 192284293225.437#  , 11736.91043281825# , 17299.34635369699# , 1.019717003694402D-03 ,-7.535801991333865D-04
        DATA -60806724461.25755# ,-133932463063.6329# ,-32448.10153203641# , 8161.252242073971# ,-3.432020480684894#    , 3.747650066250596#
        DATA -10516044721994#    , 8735044471654#     , 3719.18524296#     , 1934.83515507#     , 0#                    , 0#
        DATA -6299212552347.643# ,-769366754889.2715# ,-544.3973258384688# , 4539.541216346208# , 0#                    , 0#
        DATA -60812715434.98506# ,-133919277692.8104# ,-19764.83151341109# , 12706.85656052217# , 0#                    , 0#

3000    P(ref, 6) = Px(ref, 3)
        P(ref, 7) = Py(ref, 3)
        P(28, 6) = Px(28, 3)
        P(28, 7) = Py(28, 3)
        P(28, 8) = Vx(28) - Vx(ref)
        P(28, 9) = Vy(28) - Vy(ref)
        PROJx = Px(ref, 3) + (P(28, 6) - P(ref, 6))
        PROJy = Py(ref, 3) + (P(28, 7) - P(ref, 7))
        'Line (300 + (PROJx - cenX) * mag / AU, 220 + (PROJy - cenY) * mag * 1 / AU), 15
       
        PROJmin = 10000000000000#
        PROJmax = 0
        PROJflag2 = 0
        PROJflag1 = 0
        IF z$ = "o" THEN stp = 60 ELSE stp = 10
        FOR jj = 1 TO 60
        FOR j = 1 TO stp
        P(28, 10) = 0
        P(28, 11) = 0
        
        difx = P(28, 6) - P(ref, 6)
        dify = P(28, 7) - P(ref, 7)
        R = SQR((dify ^ 2) + (difx ^ 2))
        A = G * P(ref, 4) / (R ^ 2)
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        IF PROJflag1 = 0 THEN PROJangle = angle: PROJflag1 = 1
        P(28, 10) = P(28, 10) + (A * SIN(angle))
        P(28, 11) = P(28, 11) + (A * COS(angle))
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
        'LINE -(300 + (PROJx - cenX) * mag / AU, 220 + (PROJy - cenY) * mag / AU), 15
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



4000    DATA "Sun     "
        DATA "Mercury "
        DATA "Venus   "
        DATA "Earth   "
        DATA "Mars    "
        DATA "Jupiter "
        DATA "Saturn  "
        DATA "Uranus  "
        DATA "Neptune "
        DATA "Pluto   "
        DATA "Moon    "
        DATA "Phobos  "
        DATA "Deimos  "
        DATA "Io      "
        DATA "Europa  "
        DATA "Ganymede"
        DATA "Callisto"
        DATA "Tethys  "
        DATA "Dione   "
        DATA "Rhea    "
        DATA "Titan   "
        DATA "Iapatus "
        DATA "Ariel   "
        DATA "Umbriel "
        DATA "Titania "
        DATA "Oberon  "
        DATA "Triton  "
        DATA "Charon  "
        DATA "Habitat "
        DATA "Ida     "
        DATA "Dactyl  "
        DATA "Vesta   "
        DATA "AYSE    "
        DATA "Sedna   "
        DATA "Quaoar  "
        DATA "ISS     "
        DATA "Module  "
        DATA "OCESS   "
        DATA "UFO1    "
        DATA "UFO2    "


6000    Xexpl = 300 + (Px(explCENTER, 3) - cenX) * mag / AU
        Yexpl = 220 + (Py(explCENTER, 3) - cenY) * mag / AU
        PLAY "ML L25 GD MB"
        IF ABS(Xexpl) > 1000 OR ABS(Yexpl) > 1000 THEN 6001
        FOR Xj = 0 TO 14
         FOR Xi = 1 TO (49 - (2 * Xj))
          explANGLE = RND * 2 * 3.1415926535#
          Xexpl1 = Xexpl + (SIN(explANGLE) * Xj * 2)
          Yexpl1 = Yexpl + (COS(explANGLE) * Xj * 2)
          n = INT(RND * 5)
          IF n = 0 THEN cl = 6
          IF n = 1 THEN cl = 12
          IF n = 2 THEN cl = 7
          IF n = 3 THEN cl = 14
          IF n = 4 THEN cl = 15
          PRESET (Xexpl1, Yexpl1), cl
         NEXT Xi
         FOR Xi = 1 TO 100000: NEXT Xi
        NEXT Xj
        FOR Xj = 1 TO 56
         'CIRCLE (Xexpl, Yexpl), Xj / 2, 0
         'LINE (Xexpl - Xj / 3, Yexpl - Xj / 3)-(Xexpl + Xj / 3, Yexpl + Xj / 3), 0, BF
        NEXT Xj
6001    LOCATE 1, 35
        IF explCENTER = 39 THEN ufo2 = 0: Px(39, 3) = 0: Py(39, 3) = 0
        IF explCENTER = 38 THEN ufo1 = 0: Px(38, 3) = 0: Py(38, 3) = 0
        IF explCENTER = 28 OR explCENTER = 32 THEN master = 1: ts = .25: COLOR 12: PRINT "MASTER ALARM";
        IF explCENTER = 28 THEN explosion = 12: Ztel(2) = 0: Ztel(1) = 0: ion = 0: acc = 0: Hbr = 0: LOCATE 25, 2: PRINT "NAVmode": LOCATE 25, 11: PRINT "manual   ";
        IF explCENTER = 32 THEN explosion1 = 12: TTC = 0: GPD = 0: Agen = 0: Abr = 0
        COLOR 15
        'LOCATE 8, 10: PRINT USING "##.###"; ts;
        RETURN



7000    R = Ztel(24) + P(Ztel(23), 5)
        A = G * P(Ztel(23), 4) / (R ^ 2)
        angle = Ztel(25)
        V = SQR(G * P(Ztel(23), 4) / R)
        ZDangle = 0
        IF Ztel(26) = 1 THEN ZDangle = -90
        IF Ztel(26) = 0 THEN ZDangle = 90
        IF Ztel(26) = 2 THEN ZDangle = 180
        Vx(38) = V * SIN(angle + (ZDangle / RAD))
        Vy(38) = V * COS(angle + (ZDangle / RAD))
        Vx(38) = Vx(38) + Vx(Ztel(23))
        Vy(38) = Vy(38) + Vy(Ztel(23))
        Px(38, 3) = Px(Ztel(23), 3) + (R * SIN(angle))
        Py(38, 3) = Py(Ztel(23), 3) + (R * COS(angle))
        ufo1 = 1
        z$ = ""
        RETURN

7100    Px(39, 3) = Px(38, 3)
        Py(39, 3) = Py(38, 3)
        difx = Px(39, 3) - Px(Ztel(28), 3)
        dify = Py(39, 3) - Py(Ztel(28), 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Vt = R / 10000
        IF Vt > 1000 THEN 7110
        Vt = CINT(Vt)
        V = R / Vt
        Vx(39) = Vx(Ztel(28)) + (V * SIN(angle))
        Vy(39) = Vy(Ztel(28)) + (V * COS(angle))
        ufo2 = 1
        Px(39, 1) = 4000
7110    RETURN


7200    difx = Px(38, 3) - Px(Ztel(23), 3)
        dify = Py(38, 3) - Py(Ztel(23), 3)
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        IF Ztel(26) = 0 THEN angle = angle - (90 / RAD)
        IF Ztel(26) = 1 THEN angle = angle + (90 / RAD)
        IF Ztel(26) = 2 THEN angle = angle + (180 / RAD)
        Vx(38) = Vx(38) + (Ztel(27) * ts * SIN(angle))
        Vy(38) = Vy(38) + (Ztel(27) * ts * COS(angle))
        RETURN

8000    FOR i = 1 TO 3021
         IF ABS(300 + (Pz(i, 1) - cenX) * mag / AU) > 1000 THEN 8001
         IF ABS(220 + (Pz(i, 2) - cenY) * mag / AU) > 1000 THEN 8001
         'PSET (300 + (Pz(i, 1) - cenX) * mag / AU, 220 + (Pz(i, 2) - cenY) * mag * 1 / AU), Pz(i, 0)
8001    NEXT i
        RETURN

