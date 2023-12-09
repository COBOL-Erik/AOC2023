       identification division.
       program-id. AOC2A.

       environment division.
       configuration section.
      *special-names. decimal-point is comma.
       repository. function all intrinsic.
       input-output section.
       file-control.
           select INFIL assign to 'input.txt'
           organization is line sequential
           file status is INPUT-FS.

       data division.
       file section.
       FD  INFIL.
       01  INDATA            pic X(222).

       working-storage section.
       01 A-ARB.
          05 INPUT-FS        pic XX.
          05 AAA pic X(222).
          05 A-DUMMY pic X.
          05 A-GAME-NR       pic S9(8) comp.
          05 A-GAME-NR-R     pic 999.
          05 TWO-DIGITS.
             10 DIG1 pic 9.
             10 DIG2 pic 9.
          05 A-IX pic S9(4) comp.
          05 CNTL pic S9(4) comp.
          05 A-ACC pic S9(8) comp value 0.
          05 A-ACC-RED pic -Z(7)9.
          05 A-UC pic X(4). *> Should be too short!
             88 green value ' green'.
             88 blue  value ' blue'.
             88 red   value ' red'.
          05 A-COLOR-CNT-X.
             10 A-COLOR-CNT pic 99.

       01 V-VAXLAR.
          05 FILLER pic X   value ' '.
             88 V-INIT      value ' '.
             88 V-INPUT-EOF value 'E'.
          05 FOUND-COLOR pic X    value 'I'.
             88 FOUND-COLOR-INIT  value 'I'.
             88 FOUND-COLOR-NO    value 'N'.
             88 FOUND-COLOR-YES   value 'Y'.
          05 GAME-POSSIBLE pic X  value 'Y'.
             88 GP-YES value 'Y'.
             88 GP-NO  value 'N'.

       procedure division.
       A-MAIN section.
           display 'AOC2A' 
           open input INFIL
           if INPUT-FS not = '00'
              display INPUT-FS 
              goback
           end-if
           read INFIL at end set V-INPUT-EOF to true end-read
           move ZERO to A-GAME-NR 
           perform until V-INPUT-EOF 
              add 1 to A-GAME-NR 
      *       display trim(INDATA)  
              set GP-YES to TRUE
              set FOUND-COLOR-INIT to TRUE
              set green to TRUE 
              move -1 to A-IX
              perform COUNT-COLORS until FOUND-COLOR-NO or GP-NO 
              set FOUND-COLOR-INIT to true
              set blue to TRUE 
              move -1 to A-IX
              perform COUNT-COLORS until FOUND-COLOR-NO or GP-NO
              set FOUND-COLOR-INIT to true
              set red to TRUE 
              move -1 to A-IX
              perform COUNT-COLORS until FOUND-COLOR-NO or GP-NO
              if GP-YES add A-GAME-NR to A-ACC end-if
              read INFIL at end set V-INPUT-EOF to true end-read
           end-perform
           move A-ACC to A-ACC-RED 
           display A-ACC-RED 
           close INFIL
           goback
           .


       COUNT-COLORS section.
           add 1 to A-IX
           move trim(INDATA) to AAA
           perform until AAA(A-IX:4) = A-UC
                      or A-IX > length of trim(INDATA)
              add 1 to A-IX
           end-perform
           if A-IX > length of trim(INDATA)
              set FOUND-COLOR-NO to TRUE 
              exit section
           end-if
           move ZERO to CNTL
           perform until reverse(trim(AAA(1:A-IX)))(CNTL:1) = ' '
              add 1 to CNTL
           end-perform
           subtract 1 from CNTL
           move reverse(trim(AAA(1:A-IX)))(1:CNTL)
             to A-COLOR-CNT
           if CNTL = 2  
              move reverse(A-COLOR-CNT) to A-COLOR-CNT
           end-if
           evaluate TRUE
           when A-UC = ' gre' and A-COLOR-CNT > 13
              set GP-NO to TRUE
           when A-UC = ' blu' and A-COLOR-CNT > 14
              set GP-NO to TRUE
           when A-UC = ' red' and A-COLOR-CNT > 12
              set GP-NO to TRUE
           end-evaluate
           .
