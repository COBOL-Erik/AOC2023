       identification division.
       program-id. AOC1A.

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
       01  INDATA            PIC X(128).

       working-storage section.
       01 A-ARB.
          05 INPUT-FS        PIC XX.
          05 TWO-DIGITS.
             10 DIG1 pic 9.
             10 DIG2 pic 9.
          05 THE-NUMBER redefines TWO-DIGITS pic 99.
          05 A-IX pic S9(4) comp.
          05 A-ACC pic S9(8) comp.
          05 A-ACC-RED pic -Z(7)9.

       01 V-VAXLAR.
          05 FILLER pic X   value ' '.
             88 V-INIT      value ' '.
             88 V-INPUT-EOF value 'E'.

       procedure division.
       A-MAIN section.
           display 'AOC1A' 
           open input INFIL
           if INPUT-FS not = '00'
              display INPUT-FS 
              goback
           end-if
           read INFIL at end set V-INPUT-EOF to true end-read
           perform until V-INPUT-EOF 
      *       get first number
              perform varying A-IX from 1 by 1
                      until INDATA(A-IX:1) is numeric
                 continue *> So compiler does not complain
              end-perform
              move INDATA(A-IX:1) to DIG1 
      *       get second number
              perform varying A-IX from length of trim(INDATA) by -1
                      until INDATA(A-IX:1) is numeric
                 continue *> So compiler does not complain
              end-perform
              move INDATA(A-IX:1) to DIG2
              add THE-NUMBER to A-ACC   
              read INFIL at end set V-INPUT-EOF to true end-read
           end-perform
           move A-ACC to A-ACC-RED 
           display A-ACC-RED 
           close INFIL
           goback
           .
