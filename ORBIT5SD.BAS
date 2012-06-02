        ON ERROR GOTO 3000
        DEFDBL A-Z
        CLS
        LOCATE 5, 5
        PRINT "Telemetry Display Utility for ORBIT v. 5R"
        INPUT ; "    path to main program: ", Zpath$
        IF UCASE$(Zpath$) = "QUIT" THEN END
       
        SCREEN 12
        DIM A(10, 20), P(40, 5), Ztel(50), Px(40, 3), Py(40, 3), ccc(256), nme$(34)
        nme$(0) = "Sun     "
        nme$(1) = "Mercury "
        nme$(2) = "Venus   "
        nme$(3) = "Earth   "
        nme$(4) = "Mars    "
        nme$(5) = "Jupiter "
        nme$(6) = "Saturn  "
        nme$(7) = "Uranus  "
        nme$(8) = "Neptune "
        nme$(9) = "Pluto   "
        nme$(10) = "Moon    "
        nme$(11) = "Phobos  "
        nme$(12) = "Deimos  "
        nme$(13) = "Io      "
        nme$(14) = "Europa  "
        nme$(15) = "Ganymede"
        nme$(16) = "Callisto"
        nme$(17) = "Tethys  "
        nme$(18) = "Dione   "
        nme$(19) = "Rhea    "
        nme$(20) = "Titan   "
        nme$(21) = "Iapatus "
        nme$(22) = "Ariel   "
        nme$(23) = "Umbriel "
        nme$(24) = "Titania "
        nme$(25) = "Oberon  "
        nme$(26) = "Triton  "
        nme$(27) = "Charon  "
        nme$(28) = "Habitat "
        nme$(29) = "Ida     "
        nme$(30) = "Borrelly"
        nme$(31) = "Vesta   "
        nme$(32) = "AYSE    "
        nme$(33) = "Sedna   "
        nme$(34) = "Quaoar  "

        A(2, 0) = 0
        A(2, 1) = 1
        A(2, 2) = 2
        A(2, 3) = 9
        A(2, 4) = 2
        A(2, 5) = 4
        A(2, 6) = 3
        A(2, 7) = 15
        A(2, 8) = 6
        A(2, 9) = 4
        A(2, 10) = 14
        A(2, 11) = 13
        A(2, 12) = 5
        A(2, 13) = 10
        A(2, 14) = 15
        A(2, 15) = 8
        A(1, 0) = 0
        A(1, 1) = 1
        A(1, 2) = 2
        A(1, 3) = 9
        A(1, 4) = 2
        A(1, 5) = 3    '4
        A(1, 6) = 15   '3
        A(1, 7) = 15
        A(1, 8) = 14
        A(1, 9) = 10
        A(1, 10) = 13
        A(1, 11) = 3  '4
        A(1, 12) = 5  '5
        A(1, 13) = 6
        A(1, 14) = 7
        A(1, 15) = 8
        A(3, 0) = 0
        A(3, 1) = 5
        A(3, 2) = 0
        A(3, 3) = 3
        A(3, 4) = 8
        A(3, 5) = 9
        A(3, 6) = 0
        A(3, 7) = 0
        A(3, 8) = 11
        A(3, 9) = 2
        A(3, 10) = 1
        A(3, 11) = 10
        A(3, 12) = 4
        A(3, 13) = 6
        A(3, 14) = 0
        A(3, 15) = 15
        A(4, 0) = 0
        A(4, 1) = 1
        A(4, 2) = 8
        A(4, 3) = 9
        A(4, 4) = 3
        A(4, 5) = 5
        A(4, 6) = 4
        A(4, 7) = 6
        A(4, 8) = 12
        A(4, 9) = 13
        A(4, 10) = 11
        A(4, 11) = 2
        A(4, 12) = 10
        A(4, 13) = 14
        A(4, 14) = 7
        A(4, 15) = 15
        A(5, 0) = 8
        A(5, 1) = 1
        A(5, 2) = 8
        A(5, 3) = 3
        A(5, 4) = 9
        A(5, 5) = 3
        A(5, 6) = 8
        A(5, 7) = 7
        A(5, 8) = 8
        A(5, 9) = 9
        A(5, 10) = 3
        A(5, 11) = 11
        A(5, 12) = 1
        A(5, 13) = 9
        A(5, 14) = 3
        A(5, 15) = 3
        A(6, 0) = 8
        A(6, 1) = 6
        A(6, 2) = 15
        A(6, 3) = 7
        A(6, 4) = 6
        A(6, 5) = 14
        A(6, 6) = 6
        A(6, 7) = 7
        A(6, 8) = 7
        A(6, 9) = 14
        A(6, 10) = 7
        A(6, 11) = 7
        A(6, 12) = 14
        A(6, 13) = 12
        A(6, 14) = 14
        A(6, 15) = 15
        A(7, 0) = 7
        A(7, 1) = 3
        A(7, 2) = 2
        A(7, 3) = 3
        A(7, 4) = 10
        A(7, 5) = 3
        A(7, 6) = 10
        A(7, 7) = 7
        A(7, 8) = 11
        A(7, 9) = 11
        A(7, 10) = 10
        A(7, 11) = 11
        A(7, 12) = 11
        A(7, 13) = 3
        A(7, 14) = 11
        A(7, 15) = 15



        P(0, 5) = 6.96E+08
        P(1, 5) = 4878000 / 2
        P(2, 5) = 12104000 / 2
        P(3, 5) = 12576000 / 2
        P(4, 5) = 6787000 / 2
        P(5, 5) = 143800000 / 2
        P(6, 5) = 120660000 / 2
        P(7, 5) = 52290000 / 2
        P(8, 5) = 49500000 / 2
        P(9, 5) = 3000000 / 2
        P(10, 5) = 3476000 / 2
        P(11, 5) = 11000
        P(12, 5) = 6200
        P(13, 5) = 1820000
        P(14, 5) = 1500000
        P(15, 5) = 2640000
        P(16, 5) = 2500000
        P(17, 5) = 530000
        P(18, 5) = 560000
        P(19, 5) = 765000
        P(20, 5) = 2560000
        P(21, 5) = 730000
        P(22, 5) = 665000
        P(23, 5) = 555000
        P(24, 5) = 800000
        P(25, 5) = 815000
        P(26, 5) = 1700000
        P(27, 5) = 1300000
        P(28, 5) = 80
        P(29, 5) = 40000 / 2
        P(30, 5) = 1400 / 2
        P(31, 5) = 960000 / 2
        P(32, 5) = 500
        P(33, 5) = 1800000
        P(34, 5) = 1287000
        P(35, 5) = 500
        P(36, 5) = 70
        P(38, 5) = 500
        P(39, 5) = 10
        OPEN "I", #3, "MST.DAT"
        INPUT #3, MST#
        CLOSE #3
        Dref = 3: clr = 1
        


90      IF Dref = 28 THEN Dref = 3
        IF Dref = 32 THEN Dref = 3
        'clr = clr + 1: IF clr = 5 THEN clr = 1
        filename$ = ""
        IF Dref = 3 THEN clr = 1: filename$ = "mggd.bmp"
        IF Dref = 4 THEN clr = 2: filename$ = "mgstopo3.bmp"
        IF Dref = 2 THEN clr = 3: filename$ = "venusS.bmp"
        IF Dref = 10 THEN clr = 4: filename$ = "moon.bmp"
        IF Dref = 5 THEN clr = 4: filename$ = "jupiter.bmp"
        IF Dref = 20 THEN clr = 4: filename$ = "titan.bmp"
        IF Dref = 13 THEN clr = 4: filename$ = "io.bmp"
        IF Dref = 14 THEN clr = 4: filename$ = "europa.bmp"
        IF Dref = 15 THEN clr = 4: filename$ = "ganymede.bmp"
        IF Dref = 16 THEN clr = 4: filename$ = "callisto.bmp"
        IF Dref = 6 THEN clr = 6: filename$ = "saturn.bmp"
        IF Dref = 8 THEN clr = 5: filename$ = "neptune.bmp"
        IF Dref = 1 THEN clr = 4: filename$ = "mercury.bmp"
        IF Dref = 9 THEN clr = 4: filename$ = "pluto.bmp"
        IF Dref = 7 THEN clr = 7: filename$ = "uranus.bmp"
        IF Dref = 11 THEN clr = 4: filename$ = "phobos.bmp"
        IF Dref = 12 THEN clr = 4: filename$ = "deimos.bmp"
        IF Dref = 17 THEN clr = 4: filename$ = "tethys.bmp"
        IF Dref = 18 THEN clr = 4: filename$ = "dione.bmp"
        IF Dref = 19 THEN clr = 4: filename$ = "rhea.bmp"
        IF Dref = 21 THEN clr = 4: filename$ = "iapetus.bmp"
        IF Dref = 27 THEN clr = 4: filename$ = "charon.bmp"
        IF Dref = 26 THEN clr = 4: filename$ = "triton.bmp"
        IF Dref = 30 THEN clr = 4: filename$ = "borrelly.bmp"

91      IF filename$ = "" THEN LINE (0, 0)-(639, 319), 0, BF: GOTO 99
        OPEN "R", #1, filename$, 1
        FIELD #1, 1 AS A$

        k = 1079
        FOR x = 0 TO 319
         FOR y = 0 TO 639
          GET #1, k
          i = ASC(A$)
          'ccc(i) = ccc(i) + 1
          PSET (y, 319 - x), A(clr, i)
          k = k + 1
         NEXT y
        NEXT x
        'FOR i = 0 TO 256
        ' IF ccc(i) > 0 THEN PRINT i, ccc(i)
        'NEXT i
        'END
        CLOSE #1

99      x = 0
        y = 80 * SIN((x - 10) / 101.859164#)
        PSET (x, 160 - y), 15
        FOR x = 0 TO 640
         y = 80 * SIN((x - 10) / 101.859164#)
         LINE (x - 1, 159 - y)-(x + 1, 161 - y), 15, BF
        NEXT x

100     'IF clr = 1 THEN Dref = 3
        'IF clr = 2 THEN Dref = 4
        'IF clr = 3 THEN Dref = 2
        'IF clr = 4 THEN Dref = 10
        FOR x = 0 TO 640 STEP 53.333
         LINE (x, 0)-(x, 319), 7
        NEXT x
        FOR y = 0 TO 319 STEP 53.333
         LINE (0, y)-(640, y), 7
        NEXT y
        LINE (0, 319)-(640, 319), 7

200     GOSUB 800
        LOCATE 21, 64: COLOR 9: PRINT "TIME: ";
        LOCATE 22, 64
        COLOR 11: PRINT USING "####"; year;
        PRINT ":"; : PRINT USING "###"; day;
        PRINT ":"; : PRINT USING "##"; hr;
        PRINT ":"; : PRINT USING "##"; min;
        PRINT ":"; : PRINT USING "##"; sec;
        MET# = (year * 31536000#) + (day * 86400#) + (hr * 3600#) + (min * 60#) + sec
        IF MET# > MST# THEN MET# = MET# - MST#: METsgn = 1 ELSE MET# = MST# - MET#: METsgn = -1
        METyear# = INT(MET# / 31536000#)
        METday# = MET# - (METyear# * 31536000#)
        METday# = INT(METday# / 86400#)
        METhr# = MET# - (METyear# * 31536000#) - (METday# * 86400#)
        METhr# = INT(METhr# / 3600#)
        METmin# = MET# - (METyear# * 31536000#) - (METday# * 86400#) - (METhr# * 3600#)
        METmin# = INT(METmin# / 60#)
        METsec# = MET# - (METyear# * 31536000#) - (METday# * 86400#) - (METhr# * 3600#) - (METmin# * 60#)
        LOCATE 23, 64: COLOR 9: PRINT "MET:  ";
        COLOR 13 + METsgn
        LOCATE 24, 63
        IF METsgn = -1 THEN PRINT "-";  ELSE PRINT " ";
        LOCATE 24, 64
        PRINT USING "####"; METyear#;
        PRINT ":"; : PRINT USING "###"; METday#;
        PRINT ":"; : PRINT USING "##"; METhr#;
        PRINT ":"; : PRINT USING "##"; METmin#;
        PRINT ":"; : PRINT USING "##"; METsec#;
               
        MSTyear# = CINT(MST# / 31536000#)
        MSTday# = MST# - (MSTyear# * 31536000#)
        MSTday# = CINT(MSTday# / 86400#)
        MSThr# = MST# - (MSTyear# * 31536000#) - (MSTday# * 86400#)
        MSThr# = CINT(MSThr# / 3600#)
        MSTmin# = MST# - (MSTyear# * 31536000#) - (MSTday# * 86400#) - (MSThr# * 3600#)
        MSTmin# = CINT(MSTmin# / 60#)
        MSTsec# = MST# - (MSTyear# * 31536000#) - (MSTday# * 86400#) - (MSThr# * 3600#) - (MSTmin# * 60#)
        LOCATE 25, 64: COLOR 9: PRINT "MST:  ";
        LOCATE 26, 64
        COLOR 11: PRINT USING "####"; MSTyear#;
        PRINT ":"; : PRINT USING "###"; MSTday#;
        PRINT ":"; : PRINT USING "##"; MSThr#;
        PRINT ":"; : PRINT USING "##"; MSTmin#;
        PRINT ":"; : PRINT USING "##"; MSTsec#;
        




        GOSUB 300
        x1 = 640 * (Ahab + 59.25 + 180) / 360
        IF x1 > 640 THEN x1 = x1 - 640
        y1 = 80 * SIN((x1 - 10) / 101.859164#)
        IF Dhab > 10000000 THEN y1 = y1 * 10000000 / Dhab
        LINE (x1 - 1, 159 - y1)-(x1 + 1, 161 - y1), 12, BF
        PSET (x1, 160 - y1), 14
        LOCATE 21, 1: COLOR 9: PRINT "LONGITUDE:  "; : COLOR 11: PRINT USING "####.#"; ABS((x1 * 57.29578 / 101.859164#) - 180); : COLOR 9: IF x1 < 320 THEN PRINT "W";  ELSE PRINT "E";
        LOCATE 22, 1: COLOR 9: PRINT "LATITUDE:   "; : COLOR 11: PRINT USING "####.#"; (ABS(y1) * 57.29578 / 101.859164#); : COLOR 9: IF y1 < 0 THEN PRINT "S";  ELSE PRINT "N";
        LOCATE 23, 1: COLOR 9: PRINT "OCESS ANGLE:"; : COLOR 11: PRINT USING "####.#"; (Ahab); : COLOR 9: PRINT CHR$(248);
        LOCATE 24, 1: COLOR 9: PRINT "ALTITUDE:"; : COLOR 11:
        IF (Dhab - P(28, 5) - P(Dref, 5)) > 9999999900# THEN PRINT USING "#######.#"; (Dhab - P(28, 5) - P(Dref, 5)) / 1000000; : COLOR 9: PRINT "Mm";  ELSE PRINT USING "#######.#"; (Dhab - P(28, 5) - P(Dref, 5)) / 1000; : COLOR 9: PRINT "km";
        LOCATE 25, 1: COLOR 9: PRINT "ENGINE:     "; : COLOR 11: PRINT USING "####.#"; eng; : COLOR 9: PRINT "m/s"; CHR$(253);
        LOCATE 26, 1: COLOR 9: PRINT "ORIENTATION:"; : COLOR 11
          IF Sflag = 0 THEN PRINT " ccw prog "; : GOTO 201
          IF Sflag = 4 THEN PRINT " ccw retro"; : GOTO 201
          IF Sflag = 1 THEN PRINT " manual   "; : GOTO 201
          IF Sflag = 2 THEN PRINT " app targ "; : GOTO 201
          IF Sflag = 3 THEN PRINT " dep ref  ";
        

201     x2 = 640 * (Aayse + 59.25 + 180) / 360
        IF x2 > 640 THEN x2 = x2 - 640
        y2 = 80 * SIN((x2 - 10) / 101.859164#)
        IF Dayse > 10000000 THEN y2 = y2 * 10000000 / Dayse
        IF Dayse < 100000000 THEN LINE (x2 - 1, 159 - y2)-(x2 + 1, 161 - y2), 6, BF
       
        x3 = 640 * (Aiss + 59.25 + 180) / 360
        IF x3 > 640 THEN x3 = x3 - 640
        y3 = 80 * SIN((x3 - 10) / 101.859164#)
        IF Diss > 10000000 THEN y3 = y3 * 10000000 / Diss
        IF Diss < 100000000 THEN LINE (x3 - 1, 159 - y3)-(x3 + 1, 161 - y3), 4, BF
        
        IF Dufo1 > 10000000 THEN 221
        x4 = 640 * (Aufo1 + 59.25 + 180) / 360
        IF x4 > 640 THEN x4 = x4 - 640
        y4 = 80 * SIN((x4 - 10) / 101.859164#)
        IF Dufo1 > 10000000 THEN y4 = y4 * 10000000 / Dufo1
        IF Dufo1 < 100000000 THEN LINE (x4 - 1, 159 - y4)-(x4 + 1, 161 - y4), 13, BF

221     x6 = 640 * (Aocess + 59.25 + 180) / 360
        IF x6 > 640 THEN x6 = x6 - 640
        y6 = 80 * SIN((x6 - 10) / 101.859164#)
        IF Docess > 10000000 THEN y6 = y6 * 10000000 / Docess
        IF Docess < 100000000 THEN LINE (x6 - 2, 158 - y6)-(x6 + 2, 162 - y6), 10, B

230     IF MODULEflag = 0 THEN 240
        x5 = 640 * (Amod + 59.25 + 180) / 360
        IF x5 > 640 THEN x5 = x5 - 640
        y5 = 80 * SIN((x5 - 10) / 101.859164#)
        IF Dmod > 10000000 THEN y5 = y5 * 10000000 / Dmod
        IF Dmod < 100000000 THEN LINE (x5 - 1, 159 - y5)-(x5 + 1, 161 - y5), 2, BF

240     LOCATE 21, 25: COLOR 6: PRINT "AYSE:";
        COLOR 11
        IF (Dhabayse - P(28, 5) - P(32, 5)) > 999999000# THEN PRINT USING "#######"; (Dhabayse - P(28, 5) - P(32, 5)) / 1000000; : COLOR 9: PRINT "Mm";  ELSE PRINT USING "#######"; (Dhabayse - P(28, 5) - P(32, 5)) / 1000; : COLOR 9: PRINT "km";
        LOCATE 21, 40: COLOR 11
        IF (Dayse - P(32, 5) - P(Dref, 5)) > 999999000# THEN PRINT USING "#######"; (Dayse - P(32, 5) - P(Dref, 5)) / 1000000; : COLOR 9: PRINT "Mm alt";  ELSE PRINT USING "#######"; (Dayse - P(32, 5) - P(Dref, 5)) / 1000; : COLOR 9: PRINT "km alt"; _

        delA = ABS(Ahab - Aayse)
        IF delA > 180 THEN delA = 360 - delA
        COLOR 11
        LOCATE 21, 53
        PRINT USING "####.#"; delA;
        COLOR 9: PRINT CHR$(248);
       
        LOCATE 22, 25
        COLOR 4: PRINT "ISS: ";
        COLOR 11
        IF (Dhabiss - P(28, 5) - P(35, 5)) > 9999999000# THEN PRINT USING "#######"; (Dhabiss - P(28, 5) - P(35, 5)) / 1000000; : COLOR 9: PRINT "Mm";  ELSE PRINT USING "#######"; (Dhabiss - P(28, 5) - P(35, 5)) / 1000; : COLOR 9: PRINT "km";
        LOCATE 22, 40: COLOR 11
        IF (Diss - P(35, 5) - P(Dref, 5)) > 9999999000# THEN PRINT USING "#######"; (Diss - P(35, 5) - P(Dref, 5)) / 1000000; : COLOR 9: PRINT "Mm alt";  ELSE PRINT USING "#######"; (Diss - P(35, 5) - P(Dref, 5)) / 1000; : COLOR 9: PRINT "km alt";
        delA = ABS(Ahab - Aiss)
        IF delA > 180 THEN delA = 360 - delA
        COLOR 11
        LOCATE 22, 53
        PRINT USING "####.#"; delA;
        COLOR 9: PRINT CHR$(248);


205     LOCATE 23, 25: COLOR 9: PRINT "TARG:";
        COLOR 11
        IF (Dhabtarg - P(28, 5) - P(targ, 5)) > 9999999000# THEN PRINT USING "#######"; (Dhabtarg - P(28, 5) - P(targ, 5)) / 1000000; : COLOR 9: PRINT "Mm";  ELSE PRINT USING "#######"; (Dhabtarg - P(28, 5) - P(targ, 5)) / 1000; : COLOR 9: PRINT  _
"km";
        LOCATE 23, 40: COLOR 11
        IF targ = Dref THEN PRINT SPACE$(20): GOTO 204
        IF (Dtarg - P(targ, 5) - P(Dref, 5)) > 9999999000# THEN PRINT USING "#######"; (Dtarg - P(targ, 5) - P(Dref, 5)) / 1000000; : COLOR 9: PRINT "Mm alt";  ELSE PRINT USING "#######"; (Dtarg - P(targ, 5) - P(Dref, 5)) / 1000; : COLOR 9: PRINT  _
"km alt";
        delA = ABS(Ahab - Atarg)
        IF delA > 180 THEN delA = 360 - delA
        COLOR 11
        LOCATE 23, 53
        PRINT USING "####.#"; delA;
        COLOR 9: PRINT CHR$(248);

  
204     LOCATE 26, 25
        IF Dufo1 > 100000000 THEN PRINT SPACE$(35); : GOTO 208
        COLOR 13: PRINT "UFO: ";
        COLOR 11
        IF (Dhabufo1 - P(38, 5) - P(28, 5)) > 999999000# THEN PRINT USING "#######"; (Dhabufo1 - P(28, 5) - P(38, 5)) / 1000000; : COLOR 9: PRINT "Mm";  ELSE PRINT USING "#######"; (Dhabufo1 - P(28, 5) - P(38, 5)) / 1000; : COLOR 9: PRINT "km";
        LOCATE 26, 40: COLOR 11
        IF (Dufo1 - P(28, 5) - P(Dref, 5)) > 999999000# THEN PRINT USING "#######"; (Dufo1 - P(38, 5) - P(Dref, 5)) / 1000000; : COLOR 9: PRINT "Mm alt";  ELSE PRINT USING "#######"; (Dufo1 - P(38, 5) - P(Dref, 5)) / 1000; : COLOR 9: PRINT "km alt"; _

        delA = ABS(Ahab - Aufo1)
        IF delA > 180 THEN delA = 360 - delA
        COLOR 11
        LOCATE 26, 53
        PRINT USING "####.#"; delA;
        COLOR 9: PRINT CHR$(248);


208     LOCATE 25, 25
        COLOR 2: PRINT "MOD: ";
        COLOR 11
        IF (Dhabmod - P(28, 5) - P(36, 5)) > 999999000# THEN PRINT USING "#######"; (Dhabmod - P(28, 5) - P(36, 5)) / 1000000; : COLOR 9: PRINT "Mm";  ELSE PRINT USING "#######"; (Dhabmod - P(28, 5) - P(36, 5)) / 1000; : COLOR 9: PRINT "km";
        delA = ABS(Ahab - Amod)
        IF delA > 180 THEN delA = 360 - delA
        COLOR 11
        LOCATE 25, 53
        PRINT USING "####.#"; delA;
        COLOR 9: PRINT CHR$(248);


207     LOCATE 24, 25
        'IF Dhabocess > 100000000 THEN PRINT SPACE$(35); : GOTO 203
        COLOR 10: PRINT "PAD: ";
        COLOR 11
        IF (Dhabocess - P(37, 5) - P(28, 5)) > 999999000# THEN PRINT USING "#######"; (Dhabocess - P(28, 5) - P(37, 5)) / 1000000; : COLOR 9: PRINT "Mm";  ELSE PRINT USING "#######"; (Dhabocess - P(28, 5) - P(37, 5)) / 1000; : COLOR 9: PRINT "km";
        delA = ABS(Ahab - Aocess)
        IF delA > 180 THEN delA = 360 - delA
        COLOR 11
        LOCATE 24, 53
        PRINT USING "####.#"; delA;
        COLOR 9: PRINT CHR$(248);
203     COLOR 12: LOCATE 26, 25: PRINT nme$(Dref);
        COLOR 7


209     tt = TIMER
        ttt = TIMER
210     z$ = INKEY$
        IF z$ = "Q" THEN END
        IF z$ = CHR$(27) THEN END
        'IF z$ = CHR$(9) THEN 90
        IF z$ = "s" THEN z$ = ""
        IF z$ = "w" THEN z$ = ""
        IF z$ > "/" AND z$ < ":" THEN Dref = VAL(z$): GOTO 90
        IF z$ >= "a" AND z$ < "z" THEN Dref = ASC(z$) - 87: GOTO 90
        IF UCASE$(z$) = "C" THEN 91
        IF z$ = CHR$(0) + CHR$(134) THEN GOSUB 700
        IF TIMER - ttt > .5 THEN st1 = 1 - st1: ttt = TIMER
        IF Dayse < 100000000 THEN LINE (x2 - 1, 159 - y2)-(x2 + 1, 161 - y2), (6 * st1) + (15 * (1 - st1)), BF
        IF Diss < 100000000 THEN LINE (x3 - 1, 159 - y3)-(x3 + 1, 161 - y3), (4 * st1) + (15 * (1 - st1)), BF
        IF Dufo1 < 100000000 THEN LINE (x4 - 1, 159 - y4)-(x4 + 1, 161 - y4), (13 * st1) + (15 * (1 - st1)), BF
        IF Docess < 100000000 THEN LINE (x6 - 2, 158 - y6)-(x6 + 2, 162 - y6), (0 * st1) + (15 * (1 - st1)), B
        IF Dmod < 100000000 THEN LINE (x5 - 1, 159 - y5)-(x5 + 1, 161 - y5), (2 * st1) + (15 * (1 - st1)), BF
        LINE (x1 - 1, 159 - y1)-(x1 + 1, 161 - y1), (12 * st1) + (15 * (1 - st1)), BF
        IF TIMER - tt < 5 THEN 210
       
        LINE (x1 - 1, 159 - y1)-(x1 + 1, 161 - y1), 15, BF
        IF Dayse < 100000000 THEN LINE (x2 - 1, 159 - y2)-(x2 + 1, 161 - y2), 15, BF
        IF Diss < 100000000 THEN LINE (x3 - 1, 159 - y3)-(x3 + 1, 161 - y3), 15, BF
        IF Dufo1 < 100000000 THEN LINE (x4 - 1, 159 - y4)-(x4 + 1, 161 - y4), 15, BF
        IF Dmod < 100000000 THEN LINE (x5 - 1, 159 - y5)-(x5 + 1, 161 - y5), 15, BF
        GOTO 200

       
       
        'SUBROUTINE: Calculate orbit parameters
300     difx = Px(28, 3) - Px(Dref, 3)
        dify = Py(28, 3) - Py(Dref, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Dhab = R
        Ahab = angle * 57.29578
       
        
        difx = Px(32, 3) - Px(Dref, 3)
        dify = Py(32, 3) - Py(Dref, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Dayse = R
        Aayse = angle * 57.29578
        difx = Px(32, 3) - Px(28, 3)
        dify = Py(32, 3) - Py(28, 3)
        Dhabayse = SQR((dify ^ 2) + (difx ^ 2))

      
       
        difx = Px(35, 3) - Px(Dref, 3)
        dify = Py(35, 3) - Py(Dref, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Aiss = angle * 57.29578
        Diss = R
        difx = Px(35, 3) - Px(28, 3)
        dify = Py(35, 3) - Py(28, 3)
        Dhabiss = SQR((dify ^ 2) + (difx ^ 2))


        difx = Px(targ, 3) - Px(Dref, 3)
        dify = Py(targ, 3) - Py(Dref, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Atarg = angle * 57.29578
        Dtarg = R
        IF targ = Dref THEN Dtarg = 2 * (P(targ, 5))
        difx = Px(targ, 3) - Px(28, 3)
        dify = Py(targ, 3) - Py(28, 3)
        Dhabtarg = SQR((dify ^ 2) + (difx ^ 2))

       
       
        IF Px(38, 3) = 0 THEN Dufo1 = 9999999999#: GOTO 350
        difx = Px(38, 3) - Px(Dref, 3)
        dify = Py(38, 3) - Py(Dref, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Dufo1 = R
        Aufo1 = angle * 57.29578
        difx = Px(38, 3) - Px(28, 3)
        dify = Py(38, 3) - Py(28, 3)
        Dhabufo1 = SQR((dify ^ 2) + (difx ^ 2))

350     IF Px(39, 3) = 0 THEN Dufo2 = 9999999999#: GOTO 360
        difx = Px(39, 3) - Px(Dref, 3)
        dify = Py(39, 3) - Py(Dref, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Dufo2 = R
        Aufo2 = angle * 57.29578
        difx = Px(39, 3) - Px(28, 3)
        dify = Py(39, 3) - Py(28, 3)
        Dhabufo2 = SQR((dify ^ 2) + (difx ^ 2))

360     IF MODULEflag = 0 THEN Dmod = 9999999999999#: GOTO 370
        difx = Px(36, 3) - Px(Dref, 3)
        dify = Py(36, 3) - Py(Dref, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Dmod = R
        Amod = angle * 57.29578
        difx = Px(28, 3) - Px(36, 3)
        dify = Py(28, 3) - Py(36, 3)
        Dhabmod = SQR((dify ^ 2) + (difx ^ 2))


370     difx = Px(37, 3) - Px(Dref, 3)
        dify = Py(37, 3) - Py(Dref, 3)
        R = SQR((dify ^ 2) + (difx ^ 2))
        IF dify = 0 THEN IF difx < 0 THEN angle = .5 * 3.1415926535# ELSE angle = 1.5 * 3.1415926535# ELSE angle = ATN(difx / dify)
        IF dify > 0 THEN angle = angle + 3.1415926535#
        IF difx > 0 AND dify < 0 THEN angle = angle + 6.283185307#
        Docess = R
        Aocess = angle * 57.29578
        difx = Px(37, 3) - Px(28, 3)
        dify = Py(37, 3) - Py(28, 3)
        Dhabocess = SQR((dify ^ 2) + (difx ^ 2))

        RETURN

700     LOCATE 25, 64: PRINT "year:         ";
        LOCATE 25, 70: INPUT ; "", z$
        MSTy = VAL(z$)
        LOCATE 25, 64: PRINT "day:          ";
        LOCATE 25, 69: INPUT ; "", z$
        MSTd = VAL(z$)
        LOCATE 25, 64: PRINT "hour:         ";
        LOCATE 25, 70: INPUT ; "", z$
        MSTh = VAL(z$)
        LOCATE 25, 64: PRINT "min:          ";
        LOCATE 25, 69: INPUT ; "", z$
        MSTm = VAL(z$)
        LOCATE 25, 64: PRINT "sec:          ";
        LOCATE 25, 69: INPUT ; "", z$
        MSTs = VAL(z$)
        LOCATE 25, 64: PRINT "              ";
        MST# = (MSTy * 365# * 24# * 3600#) + (MSTd * 24# * 3600#) + (MSTh * 3600#) + (MSTm * 60#) + MSTs
        OPEN "O", #3, "MST.DAT"
        PRINT #3, MST#
        CLOSE #3
        RETURN

        'SUBROUTINE: Timed telemetry retrieval
800     OPEN "I", #2, Zpath$ + "OSbackup"
801     IF EOF(2) THEN 803
        INPUT #2, z$
        IF z$ <> "ORBIT5S" THEN 803
        FOR i = 1 TO 32
         IF EOF(2) THEN 803
         INPUT #2, Ztel(i)
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
        'ID = Ztel(21)
        'AG = Ztel(22)
        'DH = Ztel(23)
        'DHL = Ztel(24)
        'DHLA = Ztel(25)
        AYSEad = Ztel(21)
        AYSEdl = Ztel(22)
        AYSEdla = Ztel(38)
        AYSEtr = Ztel(24)
        AYSEadFLAG = Ztel(25)
        AYSE = Ztel(26)
        'TTC = Ztel(32)
        'GPD = Ztel(33)
        'Agen = Ztel(34)
        'Acon1 = Ztel(35)
        'Acon2 = Ztel(36)
        'Abr = Ztel(37)
        'Fcon1 = Ztel(38)
        'Fcon2 = Ztel(39)
        'ion = Ztel(40)
        'acc = Ztel(41)
        'Hbr = Ztel(42)
        'HT = Ztel(43)
        'AT = Ztel(44)
        'RADshield = Ztel(45)
        MODULEflag = Ztel(28)
        Px(0, 3) = 0
        Py(0, 3) = 0
        FOR i = 0 TO 39
         FOR j = 1 TO 6
          IF EOF(2) THEN 803
          INPUT #2, Ztel(j)
         NEXT j
         IF i = 0 THEN NAVfailure = Ztel(1) ELSE Px(i, 3) = Ztel(1)
         IF i = 0 THEN ENGfailure = Ztel(2) ELSE Py(i, 3) = Ztel(2)
         P(i, 1) = Ztel(5)
         P(i, 2) = Ztel(6)
        NEXT i
        Px(37, 3) = 4446370.8284487# + Px(3, 3): Py(37, 3) = 4446370.8284487# + Py(3, 3)
802     CLOSE #2
        RETURN
803     CLOSE #2
        GOTO 800

3000    IF ERL = 800 THEN RESUME 802
        IF ERL = 801 THEN RESUME 802
        LOCATE 24, 58: PRINT ERL, ERR
        z$ = INPUT$(1)
        END

