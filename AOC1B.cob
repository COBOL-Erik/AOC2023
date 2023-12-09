       Identification division.
       Function-id. foundNumberWord.
       Data division.
       Linkage section.
       01 FUNDATA pic X(128).
       01 FUNIX   pic S9(4) comp.
       01 RES     pic 9.
       Procedure division using FUNDATA FUNIX returning RES.
           move ZERO to RES
           evaluate TRUE
           when FUNDATA(FUNIX:3) = 'one'      compute RES = 1
           when FUNDATA(FUNIX:3) = 'two'      compute RES = 2
           when FUNDATA(FUNIX:5) = 'three'    compute RES = 3
           when FUNDATA(FUNIX:4) = 'four'     compute RES = 4
           when FUNDATA(FUNIX:4) = 'five'     compute RES = 5
           when FUNDATA(FUNIX:3) = 'six'      compute RES = 6
           when FUNDATA(FUNIX:5) = 'seven'    compute RES = 7
           when FUNDATA(FUNIX:5) = 'eight'    compute RES = 8
           when FUNDATA(FUNIX:4) = 'nine'     compute RES = 9
           end-evaluate
           goback.
       End function foundNumberWord.
       
       identification division.
       program-id. AOC1B.
       environment division.
       configuration section.
      *special-names. decimal-point is comma.
       repository. function all intrinsic
                   function foundNumberWord
                   .
       input-output section.
       file-control.
           select INFIL assign to 'input.txt'
           organization is line sequential
           file status is INPUT-FS.

       data division.
       file section.
       FD  INFIL.
       01  INDATA            pic X(128).

       working-storage section.
       01 INDAT              pic X(128).
       01 A-ARB.
          05 INPUT-FS        pic XX.
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
           display 'AOC1B' 

           open input INFIL
           if INPUT-FS not = '00'
              display INPUT-FS 
              goback
           end-if
           read INFIL into INDAT at end set V-INPUT-EOF to true end-read
           perform until V-INPUT-EOF 
      *       get first number
              perform varying A-IX from 1 by 1
                      until INDATA(A-IX:1) is numeric
                 if 0 < foundNumberWord(INDAT A-IX) exit perform end-if
              end-perform
              if INDATA(A-IX:1) is numeric
                 move INDATA(A-IX:1) to DIG1
              else
                 move foundNumberWord(INDAT A-IX) to DIG1
              end-if
      *       get second number
              perform varying A-IX from length of trim(INDATA) by -1
                      until INDATA(A-IX:1) is numeric
                 if 0 < foundNumberWord(INDAT A-IX) exit perform end-if
              end-perform
              if INDATA(A-IX:1) is numeric
                 move INDATA(A-IX:1) to DIG2
              else
                 move foundNumberWord(INDAT A-IX) to DIG2
              end-if
              add THE-NUMBER to A-ACC   
              read INFIL into INDAT
              at end set V-INPUT-EOF to true end-read
           end-perform
           move A-ACC to A-ACC-RED 
           display A-ACC-RED 
           close INFIL
           goback
           .

           end program AOC1B.


       