       identification division.
       program-id. Employee Entry-Search.
       author. Castillo, Beverly May S.
       date-written. August 6, 2018.
       date-compiled.

       environment division.
       configuration section.
       source-computer. ibm-pc.
       object-computer. ibm-pc.

       input-output section.
       file-control.
           select emp-file.
                  assign to disk
                  organization is indexed
                  access mode is dynamic
                  record key is em-id
                  file status is emp-stat.

       data division.
       file section.
       fd emp-file
          label records are standard
          value of file-id is 'empdb.dat'.

       01 emp-record.
          02 em-id         pic 9(5).    
          02 emp-lname     pic a(15).
          02 emp-fname     pic a(15).
          02 emp-minit     pic a.
          02 emp-bdate     pic x(10).
          02 emp-age       pic x(2).
          02 emp-addr      pic a(20).
          02 emp-pcode     pic 9(4).
          02 emp-telno     pic 9(7).
          02 emp-ctcno     pic 9(11).
          02 emp-dept      pic a(15).
          02 emp-rate      pic 9(6)V99.
          02 emp-erate     pic zzz,z99.99.
          02 emp-hdate     pic x(10).
          02 emp-SSS       pic 9(10).
          02 emp-phlth     pic 9(12).
          02 emp-pibig     pic 9(12).
          02 emp-tin       pic 9(12).
          02 emp-cvstat    pic a(10).
          02 emp-dpndts    pic 9(2).
          02 emp-wrkhrs    pic 9(3)V9.
          02 emp-ewrkhrs   pic z99.9.

          02 emp-recstat   pic 9.

       working-storage section.
       01 ws-id             pic 9(5).    
       01 ws-lname          pic a(15).
       01 ws-fname          pic a(15).
       01 ws-minit          pic a.
       01 ws-bdate          pic x(10).
       01 ws-age            pic x(2).
       01 ws-addr           pic a(20).
       01 ws-pcode          pic 9(4).
       01 ws-telno          pic 9(7).
       01 ws-ctcno          pic 9(11).
       01 ws-dept           pic a(15).
       01 ws-rate           pic 9(6)V99.
       01 ws-erate          pic zzz,z99.99.
       01 ws-hdate          pic x(10).
       01 ws-sss            pic 9(10).
       01 ws-phlth          pic 9(12).
       01 ws-pibig          pic 9(12).
       01 ws-tin            pic 9(12).
       01 ws-cvstat         pic a(10).
       01 ws-dpndts         pic 9(2).
       01 ws-opt            pic a.
       01 ws-total          pic 9(6)V99.
       01 ws-wrkhrs         pic 9(3)V9.
       01 ws-ewrkhrs        pic z99.9.

       77 record-found     pic x.
       77 emp-num-field    pic z(5).
       01 emp-stat         pic xx.
       01 norec-choice     pic x.
       01 save             pic a.
       01 choice           pic 9.

       screen section.
       01 clear-active-screen.
           05 blank screen.
 
        01 empid-scr.
           02 line 6 column 25 value
           "ÕÍ Search Employee Record... Í¸".
           02 line 7 column 25 value
           "³                             ³".
           02 line 8 column 25 value
           "³ Enter EMP ID :              ³".
           02 line 9 column 25 value
           "³                             ³".
           02 line 10 column 25 value
           "ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾".

        01 createrec-scr.
           02 line 2 column 7 value
           "ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -    "ÍÍÍÍÍÍ¸".
           02 line 3 column 7 value
           "³                     Castillo-Orofeo Enterprises
      -    "      ³".
           02 line 4 column 7 value
           "³                        Cebu City, Philippines
      -    "      ³".
           02 line 5 column 7 value
           "³
      -    "      ³".
           02 line 6 column 7 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄ´".
           02 line 7 column 7 value
           "³ Lastname       ³ Firstname      ³   MI   ³  Employee ID
      -    "      ³".
           02 line 8 column 7 value
           "³                ³                ³        ³
      -    "      ³".
           02 line 9 column 7 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄ´".
           02 line 10 column 7 value
           "³ Address   :                     ³ Postal Code  :
      -    "      ³".
           02 line 11 column 7 value
           "³ Tel No    :                     ³ Mobile No    :
      -    "      ³".
           02 line 12 column 7 value
           "³ Birthdate :                     ³ Age          :
      -    "      ³".
           02 line 13 column 7 value
           "³ Civil Stat:                     ³ Dependents   :
      -    "      ³".
           02 line 14 column 7 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄ´".
           02 line 15 column 7 value
           "³ Department:                     ³ SSS No       :
      -    "      ³".
           02 line 16 column 7 value
           "³ Rate/hr   :                     ³ PhilHealth No:
      -    "      ³".
           02 line 17 column 7 value
           "³ Wrkhrs/day:                     ³ Pag-Ibig No  :
      -    "      ³".
           02 line 18 column 7 value
           "³ Hire Date :                     ³ TIN No       :
      -    "      ³".
           02 line 19 column 7 value
           "ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -    "ÍÍÍÍÍÍ¾".

        01 search-norec-scr.
           02 line 6 column 19 value
           "ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸".
           02 line 7 column 19 value
           "³     !!! SORRY, NO RECORD FOUND        ³".
           02 line 8 column 19 value
           "³                                       ³".
           02 line 9 column 19 value
           "³    SEARCH another record (y/n)?       ³".
           02 line 10 column 19 value
           "³                                       ³".
           02 line 11 column 19 value
           "ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾".

                   01 clr-scr.
           02 line 1 column 1 value
           "
      -    "                    ".
           02 line 2 column 1 value
           "
      -    "                    ".
           02 line 3 column 1 value
           "
      -    "                    ".
           02 line 4 column 1 value
           "
      -    "                    ".
           02 line 5 column 1 value
           "
      -    "                    ".
           02 line 6 column 1 value
           "
      -    "                    ".
           02 line 7 column 1 value
           "
      -    "                    ".
           02 line 8 column 1 value
           "
      -    "                    ".
           02 line 9 column 1 value
           "
      -    "                    ".
           02 line 10 column 1 value
           "
      -    "                    ".
           02 line 11 column 1 value
           "
      -    "                    ".
           02 line 12 column 1 value
           "
      -    "                    ".
           02 line 13 column 1 value
           "
      -    "                    ".
           02 line 14 column 1 value
           "
      -    "                    ".
           02 line 15 column 1 value
           "
      -    "                    ".
           02 line 16 column 1 value
           "
      -    "                    ".
           02 line 17 column 1 value
           "
      -    "                    ".
           02 line 18 column 1 value
           "
      -    "                    ".
           02 line 19 column 1 value
           "
      -    "                    ".
           02 line 20 column 1 value
           "
      -    "                    ".
           02 line 21 column 1 value
           "
      -    "                    ".
           02 line 22 column 1 value
           "
      -    "                    ".
           02 line 23 column 1 value
           "
      -    "                    ".
           02 line 24 column 1 value
           "
      -    "                    ".
           02 line 25 column 1 value
           "
      -    "                    ".

       procedure division.
       main.
           move 1 to choice.
           open i-o emp-file.
           if emp-stat not = '00'
              open output emp-file
              close emp-file
              open i-o emp-file.

           perform search-choice until choice > 1.
           
           close emp-file.
           perform program-done.
           
       program-done.
           stop run.

     **--------searchig records in emp-record
       search-choice.
            display clr-scr.
            display empid-scr.
            perform get-employee-record.
            perform search-inquire-records.

       search-inquire-records.
           go to get-employee-record.  

       get-employee-record.
           perform init-employee-record.
           perform enter-empid.
           move "n" to record-found.
               perform find-employee-rec.

       init-employee-record.
           move spaces to emp-record.
           move zeroes to em-id.

       find-employee-rec.
           perform read-emprec.
           if record-found = "n"
           display clear-active-screen
           display search-norec-scr 
           accept (9, 53) norec-choice
           perform search-norec-option
           else if record-found = "y"
           display clear-active-screen
           display createrec-scr
           perform display-emp-fields
           display (5, 9) "Record Found!"
           perform go-again.

       search-norec-option.
           if (norec-choice = "y") or (norec-choice = "Y")
           display clr-scr
           display empid-scr
           accept (8, 42) ws-id
           move ws-id to em-id
           perform read-emprec
           display createrec-scr
           perform display-emp-fields
           display (5, 9) "Record Found.."
           else if (norec-choice = "n") or (norec-choice = "N")
           display clr-scr
           display (10, 15) 
           "Thank you for using this cobol program. Bye!"
           perform program-done.

      *---- reading files in emp-record
       enter-empid.
            accept (8, 42) ws-id.
            move ws-id to em-id.
           
       read-emprec.
           move "y" to record-found.
           read emp-file record
              invalid key
                 move "n" to record-found.

     **-----ask the user to search another record
       go-again.
           display (23, 10)
              "**Press ENTER to search again / 1 to Exit...".
           accept (23, 55) choice.
           display clr-scr.
           display empid-scr.
           perform goagain-opt.

       goagain-opt.
           if choice = 1
              display clr-scr
              display clear-active-screen
              display (10, 15) 
              "Thank you for using this cobol program. Bye!"
              perform program-done.

     **-----display records in emp-file
       display-emp-fields.
           display (8, 53) em-id.
           display (8, 9) emp-lname.
           display (8, 26) emp-fname.
           display (8, 45) emp-minit.
           display (10, 21) emp-addr.
           display (10, 58) emp-pcode.
           display (11, 21) emp-telno.
           display (11, 58) emp-ctcno.
           display (12, 21) emp-bdate.
           display (12, 58) emp-age.
           display (13, 21) emp-cvstat.
           display (13, 58) emp-dpndts.
           display (15, 21) emp-dept.
           display (16, 21) emp-erate.
           display (17, 21) emp-ewrkhrs.
           display (18, 21) emp-hdate.
           display (15, 58) emp-sss.
           display (16, 58) emp-phlth.
           display (17, 58) emp-pibig.
           display (18, 58) emp-tin.




