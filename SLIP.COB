       identification division.
       program-id. Payslip Computaion.
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
          
          02 emp-month                 pic a(5).
          02 emp-yr                    pic 9(4).
          02 emp-date                  pic a(10).
          02 emp-wrkingdays            pic 9(3)V9.
          02 emp-ewrkingdays           pic z99.9.
          02 emp-regot                 pic 9(3)V9.
          02 emp-eregot                pic zz9.9.
          02 emp-regotamnt             pic 9(6)V99. 
          02 emp-eregotamnt            pic zzz,z99.99.  
          02 emp-holot                 pic 9(3)V9.
          02 emp-eholot                pic zz9.9.
          02 emp-holotamnt             pic 9(6)V99.
          02 emp-eholotamnt            pic zzz,z99.99.
          02 emp-bonusamnt             pic 9(6)V99.
          02 emp-ebonusamnt            pic zzz,z99.99.
          02 emp-regamnt               pic 9(6)V99.
          02 emp-eregamnt              pic zzz,z99.99.
          02 emp-holamnt               pic 9(6)V99.
          02 emp-eholamnt              pic zzz,z99.99.
          02 emp-dailyrate             pic 9(6)V99.
          02 emp-edailyrate            pic zzz,z99.99.
          02 emp-monthrate             pic 9(6)V99.
          02 emp-emonthrate            pic zzz,z99.99.
          02 emp-dpndts                pic 9(2).
          02 emp-tardiness             pic 9(3)V9.
          02 emp-etardiness            pic zz9.9.
          02 emp-absences              pic 9(3)V9.
          02 emp-eabsences             pic zz9.9.
          02 emp-absentvalue           pic 9(3)V9.
          02 emp-tax                   pic 9(6)V99.
          02 emp-etax                  pic zzz,z99.99.
          02 emp-sssamnt               pic 9(6)V99.
          02 emp-esssamnt              pic zzz,z99.99.
          02 emp-phlthamnt             pic 9(6)V99.
          02 emp-ephlthamnt            pic zzz,z99.99.
          02 emp-pibigamnt             pic 9(6)V99.
          02 emp-epibigamnt            pic zzz,z99.99.
          02 emp-rdotamnt              pic 9(6)V99.
          02 emp-erdotamnt             pic zzz,z99.99.
          02 emp-spnonwrkingamnt       pic 9(6)V99.
          02 emp-espnonwrkingamnt      pic zzz,z99.99.
          02 emp-spnonwrkingrdotamnt   pic 9(6)V99.
          02 emp-espnonwrkingrdotamnt  pic zzz,z99.99.
          02 emp-regholrdotamnt        pic 9(6)V99.
          02 emp-eregholrdotamnt       pic zzz,z99.99.
          02 emp-rdot                  pic 9(3)V9.
          02 emp-erdot                 pic zz9.9.
          02 emp-spnonwrking           pic 9(3)V9.
          02 emp-espnonwrking          pic zz9.9.
          02 emp-spnonwrkingrdot       pic 9(3)V9.
          02 emp-espnonwrkingrdot      pic zz9.9.
          02 emp-regholrdot            pic 9(3)V9.
          02 emp-eregholrdot           pic zz9.9.
          02 emp-totalearn             pic 9(6)V99.
          02 emp-etotalearn            pic zzz,z99.99.
          02 emp-subtotal              pic 9(6)V99.   
          02 emp-esubtotal             pic zzz,z99.99.     
          02 emp-totaldeduct           pic 9(6)V99.
          02 emp-etotaldeduct          pic zzz,z99.99.
          02 emp-taxincome             pic 9(6)V99.
          02 emp-etaxincome            pic zzz,z99.99.
          02 emp-absencesamnt          pic 9(6)V99.
          02 emp-eabsencesamnt         pic zzz,z99.99.
          02 emp-tardinessamnt         pic 9(6)V99.
          02 emp-etardinessamnt        pic zzz,z99.99.
          02 emp-totaldayswrked        pic 9(3)V9.
          02 emp-etotaldayswrked       pic zz9.9.
          02 emp-totalot               pic 9(3)V9.
          02 emp-etotalot              pic zz9.9.
          02 emp-totald                pic 9(3)V9.
          02 emp-etotald               pic zz9.9.
          02 emp-takehomepay           pic 9(6)V99.
          02 emp-etakehomepay          pic zzz,z99.99.
          02 emp-totalearntakehome     pic 9(6)V99.
          02 emp-etotalearntakehome    pic zzz,z99.99.
          
          02 emp-recstat   pic 9.

       working-storage section.
       01 ws-id             pic 9(5).    
       01 ws-lname          pic a(150).
       01 ws-month          pic a(5).
       01 ws-rate           pic 9(6)V99.
       01 ws-erate          pic zzz,z99.99.
       01 ws-wrkhrs         pic 9(3)V9.
       01 ws-ewrkhrs        pic z99.9.
      
       01 ws-yr                    pic 9(4).
       01 ws-date                  pic a(10).
       01 ws-wrkingdays            pic 9(3)V9.
       01 ws-ewrkingdays           pic z99.9.
       01 ws-regot                 pic 9(3)V9.
       01 ws-eregot                pic z99.9.
       01 ws-eregotamnt            pic zzz,z99.99.
       01 ws-regotamnt             pic 9(6)V99.
       01 ws-holot                 pic 9(3)V9.
       01 ws-eholot                pic z99.9.
       01 ws-holotamnt             pic 9(6)V99.
       01 ws-eholotamnt            pic zzz,z99.99.
       01 ws-bonusamnt             pic 9(6)V99.
       01 ws-ebonusamnt            pic zzz,z99.99.
       01 ws-regamnt               pic 9(6)V99.
       01 ws-eregamnt              pic zzz,z99.99.
       01 ws-holamnt               pic 9(6)V99.
       01 ws-eholamnt              pic zzz,z99.99.
       01 ws-dailyrate             pic 9(6)V99.
       01 ws-edailyrate            pic zzz,z99.99.
       01 ws-monthrate             pic 9(6)V99.
       01 ws-emonthrate            pic zzz,z99.99.
       01 ws-dpndts                pic 9(2).
       01 ws-tardiness             pic 9(3)V9.
       01 ws-etardiness            pic z99.9.
       01 ws-absences              pic 9(3)V9.
       01 ws-eabsences             pic z99.9.
       01 ws-tax                   pic 9(6)V99.
       01 ws-etax                  pic zzz,z99.99.
       01 ws-takehomepay           pic 9(6)V99.
       01 ws-etakehomepay          pic zzz,z99.99.
       01 ws-sssamnt               pic 9(6)V99.
       01 ws-esssamnt              pic zzz,z99.99.
       01 ws-phlthamnt             pic 9(6)V99.
       01 ws-ephlthamnt            pic zzz,z99.99.
       01 ws-pibigamnt             pic 9(6)V99.
       01 ws-epibigamnt            pic zzz,z99.99.
       01 ws-rdotamnt              pic 9(6)V99.
       01 ws-erdotamnt             pic zzz,z99.99.
       01 ws-spnonwrkingamnt       pic 9(6)V99.
       01 ws-espnonwrkingamnt      pic zzz,z99.99.
       01 ws-spnonwrkingrdotamnt   pic 9(6)V99.
       01 ws-espnonwrkingrdotamnt  pic zzz,z99.99.
       01 ws-regholrdotamnt        pic 9(6)V99.
       01 ws-eregholrdotamnt       pic zzz,z99.99.
       01 ws-rdot                  pic 9(3)V9.
       01 ws-erdot                 pic z99.9.
       01 ws-spnonwrking           pic 9(3)V9.
       01 ws-espnonwrking          pic z99.9.
       01 ws-spnonwrkingrdot       pic 9(3)V9.
       01 ws-espnonwrkingrdot      pic z99.9.
       01 ws-regholrdot            pic 9(3)V9.
       01 ws-eregholrdot           pic z99.9.
       01 ws-totalearn             pic 9(6)V99.
       01 ws-etotalearn            pic zzz,z99.99.
       01 ws-totaldeduct           pic 9(6)V99.
       01 ws-etotaldeduct          pic zzz,z99.99.
       01 ws-taxincome             pic 9(6)V99.
       01 ws-etaxincome            pic zzz,z99.99.
       01 ws-subtotal              pic 9(6)V99.
       01 ws-esubtotal             pic zzz,z99.99.
       01 ws-absencesamnt          pic 9(6)V99.
       01 ws-eabsencesamnt         pic zzz,z99.99.
       01 ws-absentvalue           pic 9(3)V9.
       01 ws-tardinessamnt         pic 9(6)V99.
       01 ws-etardinessamnt        pic zzz,z99.99.
       01 ws-totaldayswrked        pic 9(3)V9.
       01 ws-etotaldayswrked       pic z99.9.       
       01 ws-totalot               pic 9(3)V9.
       01 ws-totald                pic 9(3)V9.
       01 ws-totalearntakehome     pic 9(6)V99.
       01 ws-etotalearntakehome    pic zzz,z99.99.

       77 record-found     pic x.
       77 emp-num-field    pic z(5).
       01 emp-stat         pic xx.
       01 re               pic 9.
       01 norec-choice     pic x.
       01 men-choice       pic x.
       01 emprec-choice    pic x.
       01 rec-counter      pic 9(3).
       01 save             pic a.
       01 save-update      pic a.
       01 choice           pic 9.
       01 summary-choice   pic x.
       01 cont-earn        pic x.
       01 view             pic x.

       screen section.
       01 clear-active-screen.
           05 blank screen.

        01 empid-scr.
           02 line 6 column 25 value
           "ÕÍ Compute Payslip... ÍÍÍÍÍÍÍÍÍ¸".
           02 line 7 column 25 value
           "³                              ³".
           02 line 8 column 25 value
           "³ Enter EMP ID        :        ³".
           02 line 9 column 25 value
           "³ Enter Payroll Month :        ³".
           02 line 10 column 25 value
           "³ Enter Payroll Year  :        ³".
           02 line 11 column 25 value
           "³                              ³".
           02 line 12 column 25 value
           "ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾".

        01 basicpay-scr.
           02 line 3 column 25 value
           "ÕÍ Computing Basic Pay... ÍÍÍÍÍÍÍÍ¸".
           02 line 4 column 25 value
           "³                                 ³".
           02 line 5 column 25 value
           "³ EMP ID           :              ³".
           02 line 6 column 25 value
           "³ Enter date today : mm/dd/yyyy   ³".
           02 line 7 column 25 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
           02 line 8 column 25 value
           "³ Rate/hr          :              ³".
           02 line 9 column 25 value
           "³ Wrking hrs/day   :              ³".
           02 line 10 column 25 value
           "³ Wrking days/month:              ³".
           02 line 11 column 25 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
           02 line 12 column 25 value
           "³ MONTHLY BASIC PAY: P            ³".
           02 line 13 column 25 value
           "ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾".

        01 earnings-screen.
           02 line 3 column 12 value
           "ÕÍ Computing Earnings... ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸".
           02 line 4 column 12 value
           "³                                                 ³".
           02 line 5 column 12 value
           "³ EMP ID      :                                   ³".
           02 line 6 column 12 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
           02 line 7 column 12 value
           "³ M. Basic Pay                     :              ³".
           02 line 8 column 12 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
           02 line 9 column 12 value
           "³ OVERTIME (hrs)                                  ³".
           02 line 10 column 12 value
           "³ Regular Day                      :              ³".
           02 line 11 column 12 value
           "³ Rest Day                         :              ³".
           02 line 12 column 12 value
           "³ Special non-wrking day           :              ³".
           02 line 13 column 12 value
           "³ Special non-wrking day & Rest Day:              ³".
           02 line 14 column 12 value
           "³ Regular Holiday                  :              ³".
           02 line 15 column 12 value
           "³ Regular Holiday & Rest Day       :              ³".
           02 line 16 column 12 value
           "³ Bonus                            :              ³".
           02 line 17 column 12 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
           02 line 18 column 12 value
           "³ GROSS EARNINGS                   : P            ³".
           02 line 19 column 12 value
           "ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾".

        01 deductions-screen.
           02 line 3 column 20 value
           "ÕÍ Computing Deductions... ÍÍÍÍÍÍÍÍ¸".
           02 line 4 column 20 value
           "³                                  ³".
           02 line 5 column 20 value
           "³ EMP ID      :                    ³".
           02 line 6 column 20 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
           02 line 7 column 20 value
           "³ Tardiness (hrs )  :              ³".
           02 line 8 column 20 value
           "³ Absences  (days)  :              ³".
           02 line 9 column 20 value
           "³                                  ³".
           02 line 10 column 20 value
           "³ BENEFITS                         ³".
           02 line 11 column 20 value
           "³ SSS               :              ³".
           02 line 12 column 20 value
           "³ PhilHealth        :              ³".
           02 line 13 column 20 value
           "³ Pag-ibig          :              ³".
           02 line 14 column 20 value
           "³                                  ³".
           02 line 15 column 20 value
           "³ Tax               :              ³".
           02 line 16 column 20 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
           02 line 17 column 20 value
           "³ TOTAL DEDUCTIONS  : P            ³".
           02 line 18 column 20 value
           "ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾".

        01 summary-scr.
           02 line 3 column 20 value
           "ÕÍ Summary ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸".
           02 line 4 column 20 value
           "³                                  ³".
           02 line 5 column 20 value
           "³ GROSS EARNINGS    :              ³".
           02 line 6 column 20 value
           "³ TOTAL DEDUCTIONS  :              ³".
           02 line 7 column 20 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
           02 line 8 column 20 value
           "³ TAXABLE INCOME    : P            ³".
           02 line 9 column 20 value
           "ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾".

        01 payslip-scr.
           02 line 2 column 2 value
           "ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ ** Castillo-Orofeo Enterprises ** ÍÍÍÍÍ
      -    "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸".
           02 line 3 column 2 value
           "³
      -    "                 ³".
           02 line 4 column 2 value
           "³               Payslip for the month of
      -    "                 ³".
           02 line 5 column 2 value
           "³                                                    Date
      -    "   :             ³".
           02 line 6 column 2 value
           "³ EMP ID  :                 Tot. Wrking days:        W. hrs/
      -    "day:             ³".
           02 line 7 column 2 value
           "³ Lastname:                 Tot. Days Worked:        Rate/hr
      -    "   :             ³".
           02 line 8 column 2 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ´".
           02 line 9 column 2 value
           "³ EARNINGS           ³ HRS ³  AMNT    ³ DEDUCTIONS
      -    "³ HRS ³  AMNT    ³".
           02 line 10 column 2 value
           "³ Basic Pay          ³     ³          ³ Tardiness (hrs)
      -    "³     ³          ³".
           02 line 11 column 2 value
           "³ Reg. OT            ³     ³          ³ Absences (days)
      -    "³     ³          ³".
           02 line 12 column 2 value
           "³ RD OT              ³     ³          ³
      -    "³     ³          ³".
           02 line 13 column 2 value
           "³ Sp. non-wrking     ³     ³          ³ SSS
      -    "³     ³          ³".
           02 line 14 column 2 value
           "³ Sp. non-rking RD   ³     ³          ³ PhilHealth
      -    "³     ³          ³".
           02 line 15 column 2 value
           "³ Reg. Holiday       ³     ³          ³ Pag-ibig
      -    "³     ³          ³".
           02 line 16 column 2 value
           "³ Reg. Holiday RD    ³     ³          ³
      -    "³     ³          ³".
           02 line 17 column 2 value
           "³ Bonus              ³     ³          ³ Tax
      -    "³     ³          ³".
           02 line 18 column 2 value
           "³ Takehome Pay       ³     ³          ³
      -    "³     ³          ³".
           02 line 19 column 2 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÁÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´".
           02 line 20 column 2 value
           "³ (A) GROSS EARNINGS       ³          ³ (B) TOT. DEDUCTIONS
      -    "      ³          ³".
           02 line 21 column 2 value
           "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -    "ÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´".
           02 line 22 column 2 value
           "³ (A-B) TAXABLE INCOME   P
      -    "                 ³".
           02 line 23 column 2 value
           "ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -    "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾".

        01 clr-scr.
           02 line 1 column 1 value
           "
      -    "                     ".
           02 line 2 column 1 value
           "
      -    "                     ".
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
      -    "                   ".
           02 line 16 column 1 value
           "
      -    "                   ".
           02 line 17 column 1 value
           "
      -    "                   ".
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

           perform compute-choice until choice > 1.
           
           close emp-file.
           perform program-done.
           
       program-done.
           stop run.

      *----- searching Records in Employee File

       compute-choice.
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

       enter-empid.
            accept (8, 49) ws-id.
            accept (9, 49) ws-month.
            accept (10, 49) ws-yr.
            move ws-id to em-id.

       find-employee-rec.
           perform read-emprec.
           if record-found = "n"
           display clear-active-screen
           display (11, 15) "No record found." 
           display (12, 15) 
                       "Do you want to compute another record (y/n)?" 
           accept (12, 60) norec-choice
           perform search-norec-option
           else if record-found = "y"
           display clear-active-screen
           display basicpay-scr.
           perform par-accept-compute.
           perform go-again.

       search-norec-option.
           if (norec-choice = "y") or (norec-choice = "Y")
           display clr-scr
           display empid-scr
           accept (8, 49) ws-id
           move ws-id to em-id
           perform read-emprec
           else if (norec-choice = "n") or (norec-choice = "N")
           display clr-scr
           display (10, 15) 
           "Thank you for using this cobol program. Bye!"
           perform program-done.
           
       read-emprec.
           move "y" to record-found.
           read emp-file record
              invalid key
                 move "n" to record-found.

       go-again.
           display (24, 5)
              "**Press ENTER to compute another record / 1 to Exit...".
           accept (24, 61) choice.
           display clr-scr.
           display empid-scr.
           perform goagain-opt.

       goagain-opt.
           if choice = 1
           display clear-active-screen
           display (10, 15) 
           "Thank you for using this cobol program. Bye!"
           perform program-done.

      *-------- accepting records to compute monthly rate

       par-accept-compute.
           compute ws-id = em-id
           display (5, 46)  ws-id
           display (8, 46) emp-erate
           display (9, 46) emp-ewrkhrs
           accept  (6, 46) ws-date
           accept  (10, 46) ws-wrkingdays
           perform par-compute-rate
           perform display-basic
           perform continue-earnings.
           
       par-compute-rate.
           compute ws-dailyrate = emp-rate * emp-wrkhrs.
           compute ws-monthrate = ws-dailyrate * ws-wrkingdays.

       display-basic.
           move ws-monthrate to ws-emonthrate.
           display (12, 48) ws-emonthrate.

       continue-earnings.
           display (15, 25) "Press ENTER to continue.."
           accept (15, 51) cont-earn.
                display clr-scr.
                display earnings-screen.
                perform par-accept-earnings.

       par-accept-earnings.
           display (5, 28) ws-id.
           move ws-monthrate to ws-emonthrate.
           display (7, 51) ws-emonthrate.
           accept (10, 49)  ws-regot.
           accept (11, 49)  ws-rdot.
           accept (12, 49) ws-spnonwrking.
           accept (13, 49) ws-spnonwrkingrdot.
           accept (14, 49) ws-holot.
           accept (15, 49) ws-regholrdot.
           accept (16, 49) ws-bonusamnt.
           perform compute-earnings. 
           perform par-compute-deduct.

       compute-earnings.
           perform par-compute-rate.
           perform compute-overtime.
           perform get-total-earnings.
           perform get-taxrate.
           perform display-totalearn.

      *------ compute earnings
       compute-overtime.
           compute ws-regamnt = (emp-rate * ws-regot) * 1.25.
           compute ws-rdotamnt  = (emp-rate * ws-rdot) * 1.69.
           compute ws-spnonwrkingamnt = 
                         (emp-rate * ws-spnonwrking) * 1.69.
           compute ws-spnonwrkingrdotamnt = 
                         (emp-rate * ws-spnonwrkingrdot) * 1.95.
           compute ws-holamnt = (emp-rate * ws-holot) * 2.60.
           compute ws-regholrdotamnt = 
                         (emp-rate * ws-regholrdot) * 3.38.
       get-total-earnings.
           compute ws-totalearn = ws-monthrate + 
                                  ws-regamnt + 
                                  ws-rdotamnt +
                                  ws-spnonwrkingamnt +  
                                  ws-spnonwrkingrdotamnt +
                                  ws-holamnt + 
                                  ws-regholrdotamnt +
                                  ws-bonusamnt.

       get-totalearntakehome.
           compute ws-totalearntakehome = ws-totalearn + ws-takehomepay.
           move ws-totalearntakehome to ws-etotalearntakehome.

       display-totalearn.
           move ws-totalearn to ws-etotalearn.
           display (18, 51) ws-etotalearn.

      *------ compute deductions
       par-compute-deduct.
           display (21, 20) "Press ENTER to continue..."
           accept (21, 47) cont-earn.
                display clr-scr.
                display deductions-screen.
                perform accept-deduction.

       accept-deduction.
            display (5, 36) ws-id.
            accept (7, 42) ws-tardiness.
            accept (8, 42) ws-absences.
            perform get-sss.
            perform get-pagibig.
            perform get-philhealth.
            perform get-taxrate.
            perform get-total-deductions.
            perform display-benefits.
            perform display-totaldeduct.
      *      perform view-summary.
            perform save-compute.

      *------compute tardiness, absences and other deductions
       get-total-deductions.
           compute ws-tardinessamnt = emp-rate * ws-tardiness.
           compute ws-absentvalue = ws-absences * emp-wrkhrs.
           compute ws-absencesamnt  = emp-rate * ws-absentvalue.
      *    compute ws-absencesamnt  = emp-rate * ws-absences.
           compute ws-subtotal = ws-tardinessamnt + 
                                 ws-absencesamnt +
                                 ws-sssamnt +
                                 ws-phlthamnt +
                                 ws-pibigamnt +
                                 ws-tax.                          

       display-benefits.
           move ws-sssamnt to ws-esssamnt.
           move ws-phlthamnt to ws-ephlthamnt.
           move ws-pibigamnt to ws-epibigamnt.
           move ws-tax to ws-etax.
           display (11, 44) ws-esssamnt.
           display (12, 44) ws-ephlthamnt.
           display (13, 44) ws-epibigamnt.
           display (15, 44) ws-etax.

       display-totaldeduct.
           move ws-subtotal to ws-esubtotal.
           display (17, 44) ws-esubtotal.

       get-sss.
            if ws-monthrate > 0.00 and
                ws-monthrate < 1250.00
                move 36.30 to ws-sssamnt
            else if ws-monthrate > 1249.99 and
                ws-monthrate < 1750.00
                move 54.50 to ws-sssamnt
            else if ws-monthrate > 1749.99 and
                ws-monthrate < 2250.00
                move 72.70 to ws-sssamnt
            else if ws-monthrate > 2249.99 and
                ws-monthrate < 2750.00
                move 90.80 to ws-sssamnt
            else if ws-monthrate > 2749.99 and
                ws-monthrate < 3250.00
                move 109.00 to ws-sssamnt
            else if ws-monthrate > 3249.99 and
                ws-monthrate < 3750.00
                move 127.20 to ws-sssamnt
            else if ws-monthrate > 3749.99 and
                ws-monthrate < 4250.00
                move 145.30 to ws-sssamnt
            else if ws-monthrate > 4249.99 and
                ws-monthrate < 4750.00
                move 163.50 to ws-sssamnt
            else if ws-monthrate > 4749.99 and
                ws-monthrate < 5250.00
                move 181.70 to ws-sssamnt
            else if ws-monthrate > 5249.99 and
                ws-monthrate < 5750.00
                move 199.80 to ws-sssamnt
            else if ws-monthrate > 5749.99 and
                ws-monthrate < 6250.00
                move 218.00 to ws-sssamnt
            else if ws-monthrate > 6249.99 and
                ws-monthrate < 6750.00
                move 236.20 to ws-sssamnt
            else if ws-monthrate > 6749.99 and
                ws-monthrate < 7250.00
                move 254.30 to ws-sssamnt
            else if ws-monthrate > 7249.99 and
                ws-monthrate < 7750.00
                move 272.50 to ws-sssamnt
            else if ws-monthrate > 7749.99 and
                ws-monthrate < 8250.00
                move 290.70 to ws-sssamnt
            else if ws-monthrate > 8249.99 and
                ws-monthrate < 8750.00
                move 308.80 to ws-sssamnt
            else if ws-monthrate > 8749.99 and
                ws-monthrate < 9250.00
                move 327.00 to ws-sssamnt
            else if ws-monthrate > 9249.99 and
                 ws-monthrate < 9750.00
                 move 345.20 to ws-sssamnt
            else if ws-monthrate > 9749.99 and
                ws-monthrate < 10250.00
                move 363.30  to ws-sssamnt
            else if ws-monthrate > 10249.99 and
                ws-monthrate < 10750.00
                move 381.50 to ws-sssamnt
            else  if ws-monthrate > 10749.99 and
                ws-monthrate < 11250.00
                move 399.70 to ws-sssamnt
            else if ws-monthrate > 11249.99 and
                ws-monthrate < 11750.00
                move 417.80 to ws-sssamnt
            else if ws-monthrate > 11749.99 and
                ws-monthrate < 12250.00
                move 436.00  to ws-sssamnt
            else if ws-monthrate > 12249.99 and
                ws-monthrate < 12750.00
                move 454.20  to ws-sssamnt
            else if ws-monthrate > 12749.99 and
                ws-monthrate < 13250.00
                move 472.30  to ws-sssamnt
            else if ws-monthrate > 13249.99 and
                ws-monthrate < 13750.00
                move 490.00  to ws-sssamnt
            else if ws-monthrate > 13749.99 and
                 ws-monthrate < 14250.00
                 move 508.70  to ws-sssamnt
            else if ws-monthrate > 14249.99 and
                ws-monthrate < 14750.00
                move 526.80  to ws-sssamnt
            else if ws-monthrate > 14749.99 and
                ws-monthrate < 15250.00
                move 545.00  to ws-sssamnt
            else if ws-monthrate > 15249.99 and
                ws-monthrate < 15750.00
                move 563.20  to ws-sssamnt
             else if ws-monthrate > 15749.99
                move 581.30  to ws-sssamnt.    

       get-philhealth.
             if ws-monthrate > 00.00  and
                ws-monthrate < 10000.00
                move 137.50 to ws-phlthamnt    
            else  if ws-monthrate > 9999.00 and
                ws-monthrate < 40000.00
                compute ws-phlthamnt  = (ws-monthrate * 0.0275) / 2
            else  if ws-monthrate > 39999.99
                compute ws-phlthamnt  = (ws-monthrate * 0.0275) / 2. 

       get-pagibig.
             if ws-monthrate > 00.00  and
                 ws-monthrate < 1500.00
                 compute ws-pibigamnt = ws-monthrate * 0.01
             else if ws-monthrate >  1500.00 and
                 ws-monthrate < 5000.00
                 compute ws-pibigamnt = ws-monthrate * 0.02
             else if ws-monthrate > 4999.99
                 move 100.00 to ws-pibigamnt.

       get-taxincome.
           compute ws-taxincome = (ws-totalearn + ws-takehomepay) - 
                                  ws-subtotal.
           move ws-taxincome to ws-etaxincome.

      * display-taxincome.
      *     move ws-taxincome to ws-etaxincome.
      *     move ws-takehomepay to ws-etakehomepay.
      *     display (9, 43) ws-etaxincome.

       get-taxrate.
            if ws-monthrate > 0.00 and
                 ws-monthrate < 10000.00
                 move 0 to ws-tax
                 move 620.09 to ws-takehomepay
            else if ws-monthrate > 14999.99 and
                 ws-monthrate < 20000.00
                 move 0 to ws-tax
                 move 1541.83 to ws-takehomepay
            else if ws-monthrate > 19999.99 and
                 ws-monthrate < 25000.00
                 move 0 to ws-tax
                 move 2683.84 to ws-takehomepay
            else if ws-monthrate > 24999.99 and
                 ws-monthrate < 30000.00
                 move 634.57 to ws-tax
                 move 3283.65 to ws-takehomepay
            else if ws-monthrate > 29999.99 and
                 ws-monthrate < 35000.00
                 move 1624.57 to ws-tax
                 move 3728.96 to ws-takehomepay
            else if ws-monthrate > 34999.99 and
                 ws-monthrate < 40000.00
                 move 2655.72 to ws-tax
                move 4197.81 to ws-takehomepay
            else if ws-monthrate > 39000.99 and
                ws-monthrate < 45000.00
                move 3886.97 to ws-tax
                move 4444.06 to ws-takehomepay
            else if ws-monthrate > 44999.99 and
                 ws-monthrate < 50000.00
                 move 5136.97 to ws-tax
                 move 4694.06 to ws-takehomepay
            else if ws-monthrate > 49999.99 and
                 ws-monthrate < 55000.00
                 move 6386.97 to ws-tax
                move 5005.01 to ws-takehomepay
            else if ws-monthrate > 54999.99 and
                 ws-monthrate < 60000.00
                 move 7636.97 to ws-tax
                 move 5355.01 to ws-takehomepay
            else if ws-monthrate > 59999.99 and
                 ws-monthrate < 65000.00
                 move 8886.97 to ws-tax
                 move 5705.01 to ws-takehomepay
            else if ws-monthrate > 64999.99 and
                 ws-monthrate < 70000.00
                move 10136.97 to ws-tax
                move 6055.01 to ws-takehomepay
            else if ws-monthrate > 69999.99 and
                 ws-monthrate < 75000.00
                 move 11497.69 to ws-tax
                 move 6294.29 to ws-takehomepay
            else if ws-monthrate > 74999.99 and
                 ws-monthrate < 80000.00
                 move 12997.69 to ws-tax
                 move 6394.29 to ws-takehomepay
            else if ws-monthrate > 79999.99 and
                 ws-monthrate < 85000.00
                 move 14497.69 to ws-tax
                move 6494.29 to ws-takehomepay
            else if ws-monthrate > 84999.99 and
                 ws-monthrate < 90000.00
                 move 15997.69 to ws-tax
                 move 6674 to ws-takehomepay
            else if ws-monthrate > 89999.99 and
                 ws-monthrate < 95000.00
                 move 17497.69 to ws-tax
                 move 6907.63 to ws-takehomepay
            else if ws-monthrate > 94999.99 and
                 ws-monthrate < 100000.00
                 move 19122.69 to ws-tax
                 move 7015.6 to ws-takehomepay.
           
       display-summary-fields.
           display (6, 43) ws-etotalearn.
           display (7, 43) ws-esubtotal.

       view-summaryincome.
           perform get-total-earnings.
           perform get-total-deductions.
           perform get-taxincome.
      *     perform display-taxincome.
           perform get-totalearntakehome.
           perform compute-totalwrkingdays.

      *----------- after viewing the pasylip summary

       compute-totalwrkingdays.
           perform compute-total-ot.
           perform compute-total-d.
           compute ws-totaldayswrked = 
                   (ws-wrkingdays * emp-wrkhrs + ws-totalot 
                   - ws-totald) / emp-wrkhrs.
           move ws-totaldayswrked to ws-etotaldayswrked.

       compute-total-ot.
           compute ws-totalot = ws-regot + ws-rdot +
                                ws-spnonwrking +
                                ws-spnonwrkingrdot +
                                ws-holot + ws-regholrdot.

       compute-total-d.
           compute ws-totald = ws-tardiness + 
                               (ws-absences * emp-wrkhrs).

       save-compute.
           display (19, 21)
             "Save this computation (y/n)?"
           accept (19, 50) save.
            if (save = "y") or (save = "Y")
               perform view-summaryincome.
               perform move-wsdata-to-paydata.
               display (22, 21) "Press Enter to view payroll...".
               accept (22, 53) view.
               display payslip-scr.
               perform display-payslip-fields.
            if (save = "n") or (save = "N")
               display (13, 21)
                "Computation not saved.".
            perform go-again.
               
       move-wsdata-to-paydata.
            move ws-id to em-id.                     
            move ws-month to emp-month.                 
            move ws-yr to emp-yr.                    
            move ws-wrkingdays to emp-ewrkingdays.      
            move ws-totaldayswrked to emp-etotaldayswrked.      
            move ws-regot to emp-eregot.   
            move ws-regamnt to emp-eregamnt.  
            move ws-rdot to emp-erdot.                  
            move ws-rdotamnt to emp-erdotamnt.    
            move ws-spnonwrking to emp-espnonwrking.           
            move ws-spnonwrkingamnt to emp-espnonwrkingamnt.  
            move ws-spnonwrkingrdot to emp-espnonwrkingrdot.
            move ws-spnonwrkingrdotamnt to emp-espnonwrkingrdotamnt.   
            move ws-holot to emp-eholot.     
            move ws-holamnt to emp-eholamnt. 
            move ws-regholrdot to emp-eregholrdot.   
            move ws-regholrdotamnt to emp-eregholrdotamnt.        
            move ws-bonusamnt to emp-ebonusamnt.            
            move ws-date to emp-date.                 
            move ws-dailyrate to emp-edailyrate.             
            move ws-monthrate to emp-emonthrate             
            move ws-tardiness to emp-etardiness.
            move ws-tardinessamnt to emp-etardinessamnt.             
            move ws-absences to emp-eabsences.          
            move ws-absencesamnt to emp-eabsencesamnt.    
            move ws-sssamnt to emp-esssamnt.               
            move ws-phlthamnt to emp-ephlthamnt.             
            move ws-pibigamnt to emp-epibigamnt.          
            move ws-tax to emp-etax.   
            move ws-totalearn to emp-etotalearn.   
            move ws-totalearntakehome to emp-etotalearntakehome.                       
            move ws-subtotal to emp-esubtotal.                   
            move ws-taxincome to emp-etaxincome.
            move ws-totaldayswrked to emp-etotaldayswrked.
            move ws-takehomepay to emp-etakehomepay.
            move 1 to emp-recstat.
            write emp-record.
            display (20, 21) "Payslip computation has been saved.".

       display-payslip-fields.
            display (4, 43)  emp-month.
            display (4, 49)  emp-yr.
            display (5, 67)  emp-date.
            display (6, 14)  em-id.
            display (6, 47)  emp-ewrkingdays.
            display (6, 67)  emp-ewrkhrs.
            display (7, 14)  emp-lname.
            display (7, 47)  emp-etotaldayswrked.
            display (7, 67)  emp-erate.
            display (10, 30) emp-emonthrate.
            display (11, 24) emp-eregot.
            display (11, 30) emp-eregamnt.
            display (12, 24) emp-erdot.
            display (12, 30) emp-erdotamnt.
            display (13, 24) emp-espnonwrking.
            display (13, 30) emp-espnonwrkingamnt.
            display (14, 24) emp-espnonwrkingrdot.
            display (14, 30) emp-espnonwrkingrdotamnt.
            display (15, 24) emp-eholot.
            display (15, 30) emp-eholamnt.
            display (16, 24) emp-eregholrdot.
            display (16, 30) emp-eregholrdotamnt.
            display (17, 30) emp-ebonusamnt.
            display (18, 30) emp-etakehomepay.
            display (10, 63) emp-etardiness.
            display (10, 69) emp-etardinessamnt.
            display (11, 63) emp-eabsences.
            display (11, 69) emp-eabsencesamnt.
            display (13, 69) emp-esssamnt.
            display (14, 69) emp-ephlthamnt.
            display (15, 69) emp-epibigamnt.
            display (17, 69) emp-etax.
            display (20, 30) emp-etotalearntakehome.
            display (20, 69) emp-esubtotal.
            display (22, 30) emp-etaxincome.        






