        G = 6.673E-11
        DIM P(34, 5)
        'Radius P(i,5) and Mass P(i,4)
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
        P(28, 5) = 100
        P(29, 5) = 40000 / 2:    P(29, 4) = 1E+17
        P(30, 5) = 1400 / 2:     P(30, 4) = 4.31E+12
        P(31, 5) = 960000 / 2:   P(31, 4) = 8.7E+20
        P(32, 5) = 500:          P(31, 4) = 1000000
        P(33, 5) = 1800000:      P(33, 4) = 1255 * 4 * 3.14159265# * P(33, 5) * P(33, 5) * P(33, 5) / 3
        P(34, 5) = 1287000:      P(34, 4) = 1955 * 4 * 3.14159265# * P(34, 5) * P(34, 5) * P(34, 5) / 3
        P = 3

1       CLS
        COLOR 12
        LOCATE 1, 27: PRINT "Transfer Orbit Calculator";
        COLOR 9
        LOCATE 15, 1: PRINT "Choices for center of Current Orbit";
        COLOR 11
        ln = 16
        lnCOUNT = 1
        RESTORE 4000
        FOR i = 0 TO 34
         READ object$
         IF i = 28 OR i = 32 THEN 3
         LOCATE ln, (1 + (lnCOUNT - 1) * 15)
         COLOR 10
         PRINT USING "##"; i;
         COLOR 11
         PRINT " "; object$;
         lnCOUNT = lnCOUNT + 1: IF lnCOUNT > 4 THEN lnCOUNT = 1: ln = ln + 1
3       NEXT i
        LOCATE 25, 1: PRINT "Enter "; : COLOR 10: PRINT "q"; : COLOR 11: PRINT " to exit";
        

        LOCATE 3, 1
        COLOR 11
        PRINT "Current orbit ["; : COLOR 10: PRINT USING "##"; P; : COLOR 11: PRINT "]:  ";
        COLOR 10
        INPUT "", z$
        COLOR 11
        IF UCASE$(z$) = "Q" THEN END
        IF z$ = "" THEN 2
        P = VAL(z$)
        IF P < 0 OR P > 34 THEN P = 3: COLOR 12: PRINT "Not a valid choice"; : COLOR 11: z$ = INPUT$(1): GOTO 1
        IF P = 28 THEN P = 3: COLOR 12: PRINT "Not a valid choice"; : COLOR 11: z$ = INPUT$(1): GOTO 1
2       M = P(P, 4)
        R = P(P, 5)
        COLOR 11: LOCATE 4, 30: PRINT "(km)"; : LOCATE 4, 1: : PRINT "starting altitude:   "; : COLOR 10: INPUT ; "", z$
        IF UCASE$(z$) = "Q" THEN END
        r1 = VAL(z$)
        r1 = (r1 * 1000) + R
        COLOR 11: LOCATE 5, 30: PRINT "(km)"; : LOCATE 5, 1: : PRINT "ending altitude:     "; : COLOR 10: INPUT ; "", z$
        IF UCASE$(z$) = "Q" THEN END
        r2 = VAL(z$)
        r2 = (r2 * 1000) + R
        COLOR 11: LOCATE 6, 30: PRINT "(m/s/s)"; : LOCATE 6, 1: : PRINT "engine acceleration: "; : COLOR 10: INPUT ; "", z$
        IF UCASE$(z$) = "Q" THEN END
        A = VAL(z$)
        COLOR 11
        IF r1 > r2 THEN A = -1 * ABS(A) ELSE A = ABS(A)
        IF ABS(r2 - r1) < 1 THEN COLOR 12: PRINT "Not a valid altitude change"; : COLOR 11: z$ = INPUT$(1): GOTO 1
        PRINT
        PRINT
        delV = SQR(G * M / r1) * (SQR((2 * r2 / r1) / ((r2 / r1) + 1)) - 1)
        IF ABS(A) > 1 THEN Tburn = delV / A
        PRINT "delta-V:"; TAB(28); : COLOR 10: PRINT USING "###,###,###.##"; delV; : COLOR 11: PRINT " m/s/s"
        IF ABS(A) > 1 THEN PRINT "Engine thrust time:"; TAB(28); : COLOR 10: PRINT USING "###,###,###.##"; SQR(G * M / r1) * (SQR((2 * r2 / r1) / ((r2 / r1) + 1)) - 1) / A; : COLOR 11: PRINT " seconds"
        T = 3.14159265# * SQR(((r1 + r2) ^ 3) / (8 * G * P(P, 4)))
        PRINT "Transfer Orbit T-half: "; TAB(28); : COLOR 10: PRINT USING "###,###,###.##"; T; : COLOR 11: PRINT " seconds"
        R2T = 6.283185 * SQR((r2 ^ 3) / (G * P(P, 4)))
        angle = ABS(360 * (T / R2T) - 180)
        IF r1 < r2 THEN word$ = "Trail " ELSE word$ = "Lead "
        PRINT word$; "the target object by"; TAB(28); : COLOR 10: PRINT USING "###,###,###.##"; angle; : COLOR 11: PRINT " degrees"
        Vtan = (3.1415926# * (2 * r2)) / R2T
        PRINT "Orbital Speed of Target:"; TAB(28); : COLOR 10: PRINT USING "###,###,###.##"; Vtan; : COLOR 11: PRINT " m/s"
        z$ = INPUT$(1)
        GOTO 1


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



