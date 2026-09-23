020000 ID DIVISION.                                                             
030000 PROGRAM-ID.     W5530200.                                                
040000 AUTHOR.         SARASWATHY S.                                            
050000 DATE-WRITTEN.   21/11/03.                                                
060000 DATE-COMPILED.                                                           
070000                                                                          
080000*                                                                         
090000*    FUNCTION:                                                            
100000*        THE PROGRAM FIXES THE RIGHT LAYOUT AND CHECKS INDATA             
110000*        FOR W553D2-RTN ON                                                
120000*        PARTS PRICE UPDATES.                                             
130000*                                                                         
140000*                                                                         
150000*    ABENDCODES:                                                          
160000*        U0016 -  . . . .                                                 
170000*        U1000 -  . . . .                                                 
180000*                                                                         
190000                                                                          
200000     SKIP3                                                                
210000 ENVIRONMENT DIVISION.                                                    
220000     SKIP2                                                                
230000 INPUT-OUTPUT SECTION.                                                    
240000                                                                          
250000 FILE-CONTROL.                                                            
260100     SKIP2                                                                
260200*          --- PARTS PRICE UPDATE FILE FROM WEB-SITE                      
260300     SELECT GCPFIL                     ASSIGN TO W55302D1.                
260400     SKIP2                                                                
260500*          --- ADJUSTED PRICE FILE TO W553D2                              
261000     SELECT UTFILE                     ASSIGN TO W55302D2.                
280000     EJECT                                                                
280100*          --- ERROR FILE                                                 
280200     SELECT W55302F                    ASSIGN TO W55302D3.                
280300     EJECT                                                                
290000 DATA DIVISION.                                                           
300000     SKIP3                                                                
310000 FILE SECTION.                                                            
320100     SKIP3                                                                
320200 FD  GCPFIL                                                               
320300     RECORDING       F                                                    
320400     BLOCK CONTAINS  0.                                                   
320410 01  CSV-RECORD            PIC X(67).                                     
320420     SKIP3                                                                
320430                                                                          
320500     SKIP3                                                                
320600 FD  UTFILE                                                               
320700     RECORDING       F                                                    
320800     BLOCK CONTAINS  0.                                                   
320900                                                                          
321000*01  POST -COPY W55302 -PRE  UT-  -L.                                     
321010     SKIP2                                                                
321011                                                                          
321020 FD  W55302F                                                              
321030     RECORDING       V                                                    
321040     BLOCK CONTAINS  0.                                                   
321060 01  W55302-001                  PIC X(250).                              
321070     SKIP2                                                                
321080                                                                          
340000 WORKING-STORAGE SECTION.                                                 
350000                                                                          
360000 77  IDPGM                       PIC X(8)    VALUE 'W5530200'.            
360100 77  JA                          PIC X       VALUE 'J'.                   
360200 77  NEJ                         PIC X       VALUE 'N'.                   
400100                                                                          
400200 77  DOTS                        PIC X       VALUE '.'.                   
400300 77  COLONS                      PIC X       VALUE ':'.                   
400400 77  COMMAS                      PIC X       VALUE ','.                   
400500 77  DOUBLE-SPACE                PIC XX      VALUE '  '.                  
400600 77  POS                         PIC S9(4)   VALUE ZERO COMP-3.           
400700 77  INTEGERS                    PIC S9(4)   VALUE ZERO COMP-3.           
400710 77  DECIMALS                    PIC S9(4)   VALUE ZERO COMP-3.           
       77  NEGATIVES                   PIC S9(4)   VALUE ZERO COMP-3.           
400800 77  IND                         PIC S9(4)   VALUE ZERO COMP-3.           
400900 77  INDATA-RAKN                 PIC S9(4)   VALUE ZERO COMP-3.           
401000                                                                          
401210 77  WS-INT                      PIC X(8) VALUE SPACE.                    
401220 77  WS-DEC                      PIC X(6) VALUE SPACE.                    
401220 77  WS-INT-NUM                  PIC S9(7).                               
401220 77  WS-DEC-NUM                  PIC S9(6).                               
401300                                                                          
401400 77  wS-NUMVAL-RESULT            PIC S9(4)   VALUE ZERO COMP-3.           
401510 77  WS-COUNTER1                 PIC S9(3)   VALUE ZERO  COMP-3.          
401520                                                                          
401600 77  GCPFIL-EOF-SW               PIC X       VALUE 'N'.                   
401700     88  END-OF-GCPFIL                       VALUE 'J'.                   
401800     88  NOT-END-OF-GCPFIL                   VALUE 'N'.                   
401900                                                                          
402000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
402100     88  INDATA-OK                           VALUE 'J'.                   
402200     88  INDATA-FEL                          VALUE 'N'.                   
402300                                                                          
402400     EJECT                                                                
402500 01  DAGENS-DATUM                PIC X(8)    VALUE ZERO.                  
402600 01  FILLER REDEFINES DAGENS-DATUM.                                       
402700     03  DAGENS-DATUM-AAR        PIC 9(4).                                
402800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
402900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
403000     EJECT                                                                
403100 01  DYNAMISKA-SUBPROGRAM.                                                
403200*                                                                         
403300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
403400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
403500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
403600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
403610     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
403700     EJECT                                                                
403800*01 -COPY WDECAREA                                                        
403900     EJECT                                                                
404000     SKIP2                                                                
404100*01 -COPY WDATAREA                                                        
404200     EJECT                                                                
404300     SKIP2                                                                
404400*    --- PARAMETRAR TILL ABEND                                            
404500                                                                          
404600 77  RKOD-ABEND                  PIC S9(4) BINARY VALUE +0.               
404700 77  RKOD-SKAPA-FELMAIL          PIC S9(4) BINARY VALUE +7.               
404800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4) BINARY VALUE +16.              
404900 77  RKOD-ABEND-MED-DUMP         PIC S9(4) BINARY VALUE +1000.            
405000     SKIP2                                                                
405100 01  FELTEXT.                                                             
405200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
405300     03  FELTEXT-STR             PIC X(100)  VALUE SPACE.                 
405400     EJECT                                                                
405410 01  W001-DAP.                                                            
405420     03  FILLER                  PIC X(165)  VALUE SPACE.                 
405500*    --- PARAMETRAR TILL POSTSUM                                          
405600*                                                                         
405700*01  -COPY W0005   -PRE  POSTSUM-                                         
405800     EJECT                                                                
405900 01  IN-AREA-START               PIC X(24)   VALUE                        
406000                                 'IN-AREA-START  '.                       
406100 01  IN-AREA                     PIC X(255).                              
406200                                                                          
406300 01  IN-GCP-AREA.                                                         
406400     03 IN-GCP-KDPRURSP          PIC X.                                   
406500     03 IN-GCP-IDARTNR           PIC 9(9).                                
406600     03 IN-GCP-IDLEVNR           PIC X(5).                                
406900     03 IN-GCP-PRARTBEL          PIC X(15).                               
407000     03 IN-GCP-KDANTENH          PIC X.                                   
407100     03 IN-GCP-KDFPKPRI          PIC X.                                   
407200     03 IN-GCP-TIPRLIST          PIC 9(8).                                
407300     03 IN-GCP-KDVALISO          PIC X(3).                                
407400     03 IN-GCP-IDUSER            PIC X(8).                                
407500     03 IN-GCP-IDDC              PIC x(2).                                
407600                                                                          
409500     EJECT                                                                
409601 01  TEST-GCP-AREA.                                                       
409701     03 TEST-KDPRURSP        PIC X(15).                                   
409801     03 TEST-IDARTNR         PIC X(15).                                   
409901     03 TEST-IDLEVNR         PIC X(15).                                   
410001     03 TEST-PRARTBEL        PIC X(15).                                   
410002     03 TEST-PRARTBEL-N REDEFINES TEST-PRARTBEL                           
410003                             PIC 9(8)V9(5).                               
410101     03 TEST-KDANTENH        PIC X(15).                                   
410201     03 TEST-KDFPKPRI        PIC X(15).                                   
410301     03 TEST-TIPRLIST        PIC X(15).                                   
410401     03 TEST-KDVALISO        PIC X(15).                                   
410501     03 TEST-IDUSER          PIC X(15).                                   
410601     03 TEST-IDDC            PIC X(15).                                   
410701                                                                          
410702 01  FELL-HEADER.                                                         
410703     03  FILLER                  PIC X(8)  VALUE 'KDPRURSP'.              
410704     03  FILLER                  PIC X     VALUE ';'.                     
410705     03  FILLER                  PIC X(7)  VALUE 'PART NO'.               
410706     03  FILLER                  PIC X     VALUE ';'.                     
410707     03  FILLER                  PIC X(9)  VALUE 'ERR DESCR'.             
410708     03  FILLER                  PIC X     VALUE ';'.                     
410709     03  FILLER                  PIC X(8)  VALUE 'SUPPLIER'.              
410710     03  FILLER                  PIC X     VALUE ';'.                     
410711     03  FILLER                  PIC X(5)  VALUE 'PRICE'.                 
410712     03  FILLER                  PIC X     VALUE ';'.                     
410713     03  FILLER                  PIC X(8)  VALUE 'CURRENCY'.              
410714     03  FILLER                  PIC X     VALUE ';'.                     
410715     03  FILLER                  PIC X(8)  VALUE 'REG DATE'.              
410716     03  FILLER                  PIC X     VALUE ';'.                     
410717     03  FILLER                  PIC X(8)  VALUE 'KDANTENH'.              
410718     03  FILLER                  PIC X     VALUE ';'.                     
410719     03  FILLER                  PIC X(8)  VALUE 'KDFPKPRI'.              
410720     03  FILLER                  PIC X     VALUE ';'.                     
410721     03  FILLER                  PIC X(6)  VALUE 'IDUSER'.                
410722     03  FILLER                  PIC X     VALUE ';'.                     
410723     EJECT                                                                
410748                                                                          
410749 01  FELL-DETAIL.                                                         
410750     03  FELL-KDPRURSP           PIC X.                                   
410751     03  FILLER                  PIC X     VALUE ';'.                     
410752     03  FELL-IDARTNR            PIC X(9).                                
410753     03  FILLER                  PIC X     VALUE ';'.                     
410754     03  FELL-DESCR              PIC X(39) VALUE SPACE.                   
410755     03  FILLER                  PIC X     VALUE ';'.                     
410756     03  FELL-LEVNR              PIC X(5).                                
410757     03  FILLER                  PIC X     VALUE ';'.                     
410758     03  FELL-PRARTBEL           PIC X(15).                               
410759     03  FILLER                  PIC X     VALUE ';'.                     
410760     03  FELL-KDVALISO           PIC X(3).                                
410761     03  FILLER                  PIC X     VALUE ';'.                     
410762     03  FELL-TIPRLIST           PIC 9(8).                                
410763     03  FILLER                  PIC X     VALUE ';'.                     
410764     03  FELL-KDFPKPRI           PIC X.                                   
410770     03  FILLER                  PIC X     VALUE ';'.                     
410780     03  FELL-KDANTENH           PIC X.                                   
410790     03  FILLER                  PIC X     VALUE ';'.                     
410800     03  FELL-IDUSER             PIC X(8).                                
410801     03  FILLER                  PIC X     VALUE ';'.                     
410900     EJECT                                                                
411102*01  AREA -COPY W55302     -PRE UT-                                       
411200                                                                          
411300     EJECT                                                                
412000     EJECT                                                                
420000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
430000 01  FILLER REDEFINES TODAYS-DATE.                                        
440000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
450000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
460000     03  TODAYS-DATE-DAY         PIC 9(2).                                
470000     EJECT                                                                
530000*    --- PARAMETERS TO ABEND                                              
540000                                                                          
550000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
560000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
570000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
580000     SKIP2                                                                
590000 01  ERROR-TEXT.                                                          
600000     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
610000     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
620100     EJECT                                                                
640100     EJECT                                                                
640200 01  IN-AREA-START               PIC X(24)   VALUE                        
640300                                 'IN-AREA-START  '.                       
640400     SKIP2                                                                
640500     EJECT                                                                
667000 PROCEDURE DIVISION.                                                      
667100 MAIN SECTION.                                                            
667200     SKIP2                                                                
667300     PERFORM A-INIT                                                       
667400     PERFORM S01-LAES-GCPFIL                                              
667500     PERFORM UNTIL END-OF-GCPFIL                                          
667700         IF INDATA-RAKN = +1                                              
667801*          --- Skip first entry (HEADINGS)                                
667900           PERFORM S01-LAES-GCPFIL                                        
668000         ELSE                                                             
668100           PERFORM B-KOLLA-INDATA                                         
668200           IF INDATA-OK                                                   
668301             PERFORM C-SKAPA-UTFILE                                       
668401             PERFORM S11-SKRIV-UTFILE                                     
668600           END-IF                                                         
668610           PERFORM S01-LAES-GCPFIL                                        
668700         END-IF                                                           
668800     END-PERFORM                                                          
668900                                                                          
669000                                                                          
669100     MOVE 'S' TO POSTSUM-OPKOD                                            
669200     CALL POSTSUM USING POSTSUM-PARM                                      
669300                                                                          
670300     PERFORM Z-FINIT                                                      
670400     GOBACK                                                               
670500     .                                                                    
670600     EJECT                                                                
670700 A-INIT SECTION.                                                          
670800     OPEN INPUT  GCPFIL                                                   
670901     OPEN OUTPUT UTFILE                                                   
670902     OPEN OUTPUT W55302F                                                  
671000     SKIP2                                                                
671100     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
671200                                                                          
671300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
671400                                                                          
671500     MOVE ZERO TO INDATA-RAKN                                             
671600                                                                          
671701     INITIALIZE UT-AREA                                                   
671900     .                                                                    
672000     EJECT                                                                
672010                                                                          
672100 B-KOLLA-INDATA  SECTION.                                                 
672300     MOVE JA   TO INDATA-SW                                               
672400     MOVE +1   TO IND                                                     
672500     UNSTRING IN-AREA DELIMITED BY ';' OR '  ' INTO                       
672600                                  IN-GCP-KDPRURSP                         
672700                                  IN-GCP-IDARTNR                          
672800                                  IN-GCP-IDLEVNR                          
672900                                  IN-GCP-PRARTBEL                         
673000                                  IN-GCP-KDANTENH                         
673100                                  IN-GCP-KDFPKPRI                         
673200                                  IN-GCP-TIPRLIST                         
673300                                  IN-GCP-KDVALISO                         
673400                                  IN-GCP-IDUSER                           
673500                                  IN-GCP-IDDC                             
673701     UNSTRING IN-AREA DELIMITED BY ';' OR '  ' INTO                       
673801                                  TEST-KDPRURSP                           
673901                                  TEST-IDARTNR                            
674001                                  TEST-IDLEVNR                            
674101                                  TEST-PRARTBEL                           
674201                                  TEST-KDANTENH                           
674301                                  TEST-KDFPKPRI                           
674401                                  TEST-TIPRLIST                           
674501                                  TEST-KDVALISO                           
674601                                  TEST-IDUSER                             
674701                                  TEST-IDDC                               
674801                                                                          
674900                                                                          
675000*    --- ARTIKELNUMRET                                                    
675100     MOVE ZERO TO POS                                                     
675200     INSPECT TEST-IDARTNR TALLYING POS FOR CHARACTERS BEFORE              
675300                                            INITIAL '  '                  
675400     IF POS > +9                                                          
675500       MOVE NEJ TO INDATA-SW                                              
675600       MOVE 'IN-IDARTNR > 9 TKN :' TO FELTEXT-STR(IND:20)                 
675700       ADD +20 TO IND                                                     
675800       MOVE 'Partno. longer than 9 characters ' TO FELL-DESCR             
675810       PERFORM S05-SKAPA-FELRAD                                           
675900     ELSE                                                                 
676000       IF TEST-IDARTNR(1:POS) NOT NUMERIC                                 
676100         MOVE NEJ TO INDATA-SW                                            
676200         MOVE 'IN-IDARTNR NOT NUM :' TO FELTEXT-STR(IND:20)               
676300         ADD +20 TO IND                                                   
676400         MOVE 'Partno.is not numeric ' TO FELL-DESCR                      
676410         PERFORM S05-SKAPA-FELRAD                                         
676500       ELSE                                                               
676600         CONTINUE                                                         
676700       END-IF                                                             
676800     END-IF                                                               
676900                                                                          
677000*    --- BESTÄLLNINGSPRISET                                               
677100*    --- Kollar att priset har rätt format. Först ersätts alla            
677200*    --- varianter av decimaltecken för att minimera felorsaker.          
677300                                                                          
677400     MOVE ZERO TO POS                                                     
677401                  DECIMALS                                                
677402     MOVE ZEROES                   TO WS-NUMVAL-RESULT                    
677403     COMPUTE WS-NUMVAL-RESULT = FUNCTION TEST-NUMVAL (                    
677404                                         TEST-PRARTBEL)                   
677424                                                                          
677425     IF WS-NUMVAL-RESULT = 0                                              
677426       INSPECT TEST-PRARTBEL TALLYING POS FOR CHARACTERS                  
677427                             BEFORE INITIAL DOUBLE-SPACE                  
677428       UNSTRING TEST-PRARTBEL DELIMITED BY '.'                            
677429                               INTO WS-INT                                
677430                                    WS-DEC                                
677440                                                                          
677445*    IF INDATA-OK                                                         
677446*      CONTINUE                                                           
677447*    ELSE                                                                 
677448       IF POS > +13                                                       
677450         MOVE NEJ TO INDATA-SW                                            
677451         MOVE 'PRARTBEL > 12 TKN :' TO FELTEXT-STR(IND:18)                
677452         ADD +18 TO IND                                                   
677453         MOVE 'Price is over Max.length FOR PartNo' TO FELL-DESCR         
677454         PERFORM S05-SKAPA-FELRAD                                         
677455       ELSE                                                               
677457         IF POS > +0                                                      
677458           MOVE ZERO TO INTEGERS                                          
677459                        DECIMALS                                          
                              NEGATIVES                                         
677460           INSPECT WS-INT TALLYING INTEGERS FOR CHARACTERS                
677461              BEFORE INITIAL SPACE                                        
677462           INSPECT WS-DEC TALLYING DECIMALS FOR CHARACTERS                
677463              BEFORE INITIAL SPACE                                        
                 INSPECT WS-INT TALLYING NEGATIVES FOR LEADING '-'              
                 MOVE FUNCTION TRIM(WS-INT) TO WS-INT-NUM                       
                 MOVE FUNCTION TRIM(WS-DEC) TO WS-DEC-NUM                       
677464           IF ((WS-INT-NUM = +0 AND WS-DEC-NUM = +0)                      
677465           OR NEGATIVES > +0)                                             
                 OR TEST-PRARTBEL = +0                                          
                   MOVE 'PRARTBEL < 0 TKN :' TO FELTEXT-STR(IND:18)             
677466             MOVE NEJ TO INDATA-SW                                        
677467             MOVE 'Price should not be zero'  TO FELL-DESCR               
677468             PERFORM S05-SKAPA-FELRAD                                     
677469           ELSE                                                           
677471             IF INTEGERS > +0                                             
677472               IF WS-INT (1:INTEGERS) NOT NUMERIC                         
677473               MOVE NEJ TO INDATA-SW                                      
677474                 MOVE 'PRARTBEL NOT NUM :' TO FELTEXT-STR(IND:18)         
677475                 ADD +18 TO IND                                           
677476                 MOVE 'Price Integer part is not numeric'                 
677477                         TO FELL-DESCR                                    
677478                 PERFORM S05-SKAPA-FELRAD                                 
677479               ELSE                                                       
677480                 IF DECIMALS > +0                                         
677481                 AND WS-DEC (1:DECIMALS) NOT NUMERIC                      
677482                   MOVE NEJ TO INDATA-SW                                  
677483                  MOVE 'PRARTBEL NOT NUM :' TO FELTEXT-STR(IND:18)        
677484                   ADD +18 TO IND                                         
677485                   MOVE 'Price decimal part is not numeric '              
677486                        TO FELL-DESCR                                     
677487                   PERFORM S05-SKAPA-FELRAD                               
677488                 ELSE                                                     
677489                   IF INTEGERS > 7 OR DECIMALS > 5                        
677490                     MOVE NEJ TO INDATA-SW                                
677491                    MOVE 'PRARTBEL NOT OK:' TO FELTEXT-STR(IND:16)        
677492                     ADD +16 TO IND                                       
677493                     IF INTEGERS > 7                                      
677494                       MOVE 'Price Integers > 7 ' TO FELL-DESCR           
677495                     ELSE                                                 
677496                       MOVE 'Price Decimals > 5 ' TO FELL-DESCR           
677497                     END-IF                                               
677498                     PERFORM S05-SKAPA-FELRAD                             
677499                   END-IF                                                 
677500                 END-IF                                                   
677501               END-IF                                                     
677502             ELSE                                                         
677503*              --- Priset har inga heltal                                 
677504               MOVE NEJ TO INDATA-SW                                      
677505               MOVE 'PRARTBEL HAS NO INTEGERS:'                           
677506                                     TO FELTEXT-STR(IND:25)               
677507               ADD +25 TO IND                                             
677508               MOVE 'Price has no integers ' TO FELL-DESCR                
677509               PERFORM S05-SKAPA-FELRAD                                   
677510             END-IF                                                       
677511           END-IF                                                         
677512         END-IF                                                           
677513       END-IF                                                             
677514     ELSE                                                                 
677515*          --- Pris saknas                                                
677516       IF TEST-PRARTBEL  = SPACE                                          
677517           MOVE NEJ TO INDATA-SW                                          
677518           MOVE 'PRARTBEL MISSING :' TO FELTEXT-STR(IND:18)               
677519           ADD +18 TO IND                                                 
677520           MOVE 'No price declared ' TO FELL-DESCR                        
677521           PERFORM S05-SKAPA-FELRAD                                       
677522       ELSE                                                               
677523         MOVE NEJ TO INDATA-SW                                            
677524         MOVE 'PRARTBEL INVALID :' TO FELTEXT-STR(IND:18)                 
677525         ADD +18 TO IND                                                   
677526         MOVE 'Invalid price' TO FELL-DESCR                               
677527         PERFORM S05-SKAPA-FELRAD                                         
677528       END-IF                                                             
677529     END-IF                                                               
677530*    END-IF                                                               
677600                                                                          
684400***-------------------------                                              
684500                                                                          
684600*    --- PRISLISTE-DATUM                                                  
684700     MOVE ZERO TO POS                                                     
684800     INSPECT TEST-TIPRLIST TALLYING POS FOR CHARACTERS BEFORE             
684900                                            INITIAL '  '                  
685000     IF POS NOT = +8                                                      
685100       MOVE NEJ TO INDATA-SW                                              
685200       MOVE 'TIPRLIST EJ 8 TKN :' TO FELTEXT-STR(IND:19)                  
685300       ADD +19 TO IND                                                     
685400       MOVE 'Date has not 8 digits YYYYMMDD ' TO FELL-DESCR               
685500       PERFORM S05-SKAPA-FELRAD                                           
685600     ELSE                                                                 
685700       IF TEST-TIPRLIST(1:POS) NOT NUMERIC                                
685800         MOVE NEJ TO INDATA-SW                                            
685900         MOVE 'TIPRLIST EJ NUM :' TO FELTEXT-STR(IND:17)                  
686000         ADD +17 TO IND                                                   
686100         MOVE 'Date is not numeric ' TO FELL-DESCR                        
686200         PERFORM S05-SKAPA-FELRAD                                         
686300       ELSE                                                               
686400         IF IN-GCP-TIPRLIST(1:2) NOT = 20                                 
686500           MOVE NEJ TO INDATA-SW                                          
686600           MOVE 'TIPRLIST FEL FORM :' TO FELTEXT-STR(IND:19)              
686700           ADD +19 TO IND                                                 
686800           MOVE 'Year must be specified with 4 digits '                   
686900                TO FELL-DESCR                                             
686910           PERFORM S05-SKAPA-FELRAD                                       
687000         ELSE                                                             
687100            MOVE IN-GCP-TIPRLIST TO DAT-I-TIDATUM                         
687200            MOVE 'AAMMDD'        TO DAT-KDDATFORM                         
687300            CALL WDATKONV USING DAT-KDDATFORM                             
687400                                DAT-I-TIDATUM                             
687500                                DAT-O-TIDATUM                             
687600                                DAT-KDSVAR                                
687700            IF DAT-KDSVAR-OK                                              
687800              IF IN-GCP-TIPRLIST >= DAGENS-DATUM                          
687900                CONTINUE                                                  
688000              ELSE                                                        
688100                MOVE NEJ TO INDATA-SW                                     
688200                MOVE 'TIPRLIST < DAGENS:' TO FELTEXT-STR(IND:19)          
688300                ADD +19 TO IND                                            
688400                MOVE 'Date is lesser than todays date '                   
688500                     TO FELL-DESCR                                        
688510                PERFORM S05-SKAPA-FELRAD                                  
688600              END-IF                                                      
688700            ELSE                                                          
688800              MOVE NEJ TO INDATA-SW                                       
688900              MOVE 'TIPRLIST EJ DATUM :' TO FELTEXT-STR(IND:19)           
689000              ADD +19 TO IND                                              
689100              MOVE 'Not a valid date format ' TO FELL-DESCR               
689200              PERFORM S05-SKAPA-FELRAD                                    
689300            END-IF                                                        
689400         END-IF                                                           
689500       END-IF                                                             
689600     END-IF                                                               
689700     .                                                                    
689800     EJECT                                                                
689900                                                                          
689901 C-SKAPA-UTFILE  SECTION.                                                 
690100*    -- SKAPA POST TILL W5531200, I W553D2                                
690200*    -- SOM SKAPAR MIDDAR TILL 5111                                       
690300*    -- Indatat är bl.a. kontrollerat mot WDATKONV resp. WDECEDIT         
690400*                                                                         
690502     MOVE '985'             TO UT-IDPTYP                                  
690600                                                                          
690702     MOVE IN-GCP-IDARTNR    TO UT-IDARTNR                                 
690802                                                                          
690902     MOVE IN-GCP-IDLEVNR    TO UT-IDLEVNR                                 
691100                                                                          
691200*    -- Beställningspriset skall anges i öre !!                           
691300     INSPECT TEST-PRARTBEL REPLACING ALL COMMAS BY DOTS                   
691310     COMPUTE TEST-PRARTBEL-N = FUNCTION NUMVAL(TEST-PRARTBEL)             
691400     MULTIPLY TEST-PRARTBEL-N BY 1                                        
691402                        GIVING UT-PRARTBEL                                
691500                                                                          
691602     MOVE IN-GCP-KDANTENH   TO UT-KDANTENH                                
691700                                                                          
691802     MOVE IN-GCP-KDFPKPRI   TO UT-KDFPKPRI                                
691900                                                                          
692002     MOVE DAT-TIAAMMDD      TO UT-TIPRLIST                                
692100                                                                          
692202     MOVE IN-GCP-KDVALISO   TO UT-KDVALISO                                
692300                                                                          
692402     MOVE IN-GCP-IDUSER     TO UT-IDUSER                                  
692502                                                                          
692602*    MOVE IN-GCP-IDDC       TO UT-IDDC                                    
692700     .                                                                    
692800     EJECT                                                                
692810                                                                          
692900 Z-FINIT SECTION.                                                         
693000     CLOSE GCPFIL                                                         
693101           UTFILE                                                         
693102           W55302F                                                        
693200     SKIP2                                                                
693300     .                                                                    
693400     EJECT                                                                
693410                                                                          
693500 S01-LAES-GCPFIL  SECTION.                                                
693600     READ GCPFIL INTO IN-AREA                                             
693700     AT END                                                               
693800        MOVE HIGH-VALUE TO IN-AREA                                        
693900        SET END-OF-GCPFIL TO TRUE                                         
694000                                                                          
694100     NOT AT END                                                           
694200        MOVE 'GCPFIL'   TO POSTSUM-FDNAMN                                 
694300        MOVE 'W55302D1' TO POSTSUM-DDNAMN2                                
694400        MOVE SPACE      TO POSTSUM-TRANSTYP                               
694500        CALL POSTSUM USING POSTSUM-PARM                                   
694600                                                                          
694700        ADD +1 TO INDATA-RAKN                                             
694800     END-READ                                                             
694900     .                                                                    
695000     EJECT                                                                
695100                                                                          
695101 S11-SKRIV-UTFILE SECTION.                                                
695301     WRITE UT-POST FROM UT-AREA                                           
695400                                                                          
695500     MOVE 'UT '         TO POSTSUM-TRANSTYP                               
695601     MOVE 'W55302'      TO POSTSUM-FDNAMN                                 
695700     MOVE 'W55302D2'    TO POSTSUM-DDNAMN2                                
695800     CALL POSTSUM USING POSTSUM-PARM                                      
695900     .                                                                    
696000     EJECT                                                                
696001                                                                          
696002 S05-SKAPA-FELRAD SECTION.                                                
696003     MOVE IN-GCP-KDPRURSP          TO FELL-KDPRURSP                       
696005     MOVE TEST-IDARTNR           TO FELL-IDARTNR                          
696008     MOVE IN-GCP-IDLEVNR           TO FELL-LEVNR                          
696009     MOVE IN-GCP-PRARTBEL          TO FELL-PRARTBEL                       
696010     MOVE IN-GCP-KDVALISO          TO FELL-KDVALISO                       
696011     MOVE IN-GCP-TIPRLIST          TO FELL-TIPRLIST                       
696012     MOVE IN-GCP-KDANTENH          TO FELL-KDANTENH                       
696013     MOVE IN-GCP-KDFPKPRI          TO FELL-KDFPKPRI                       
696014     MOVE IN-GCP-IDUSER            TO FELL-IDUSER                         
696015                                                                          
696020     PERFORM S06-SKRIV-ERRFILE                                            
696021     .                                                                    
696022     EJECT                                                                
696023                                                                          
696024 S06-SKRIV-ERRFILE     SECTION.                                           
696025     IF WS-COUNTER1 = ZERO                                                
696026       MOVE  ' ¤DAPW55302-001' TO W001-DAP                                
696027       WRITE W55302-001   FROM W001-DAP                                   
696028       MOVE  ' ¤DAPW553'       TO W001-DAP                                
696029       WRITE W55302-001   FROM W001-DAP                                   
696030       WRITE W55302-001   FROM FELL-HEADER                                
696031     END-IF                                                               
696032     WRITE W55302-001   FROM FELL-DETAIL                                  
696033     ADD +1 TO WS-COUNTER1                                                
696034     .                                                                    
696035     SKIP3                                                                
