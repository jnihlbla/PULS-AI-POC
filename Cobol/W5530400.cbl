010000 ID DIVISION.                                                             
020001 PROGRAM-ID.     W5530400.                                                
030001 AUTHOR.         BARSHARANI BISHOYE.                                      
040001 DATE-WRITTEN.   13/10/2022.                                              
050001 DATE-COMPILED.                                                           
060001                                                                          
070001*                                                                         
080001*    FUNCTION:                                                            
090001*        THE PROGRAM FIXES THE RIGHT LAYOUT AND CHECKS INDATA             
100001*        FOR W553D2-RTN ON                                                
110001*        PARTS PRICE UPDATES.                                             
120001*                                                                         
130001*                                                                         
140001*    ABENDCODES:                                                          
150001*        U0016 -  . . . .                                                 
160001*        U1000 -  . . . .                                                 
170001*                                                                         
180001                                                                          
190001     SKIP3                                                                
200001 ENVIRONMENT DIVISION.                                                    
210001     SKIP2                                                                
220001 INPUT-OUTPUT SECTION.                                                    
230001                                                                          
240001 FILE-CONTROL.                                                            
250001     SKIP2                                                                
260001*          --- PARTS PRICE UPDATE FILE FROM WEB-SITE                      
260101     SELECT INFILE                     ASSIGN TO W55304D1.                
260201     SKIP2                                                                
260301*          --- ADJUSTED PRICE FILE TO W553D2                              
260401     SELECT UTFILE                     ASSIGN TO W55304D2.                
260501     EJECT                                                                
260601*          --- ERROR FILE                                                 
260701     SELECT W55304F                    ASSIGN TO W55304D3.                
260801     EJECT                                                                
260901 DATA DIVISION.                                                           
261001     SKIP3                                                                
262001 FILE SECTION.                                                            
263001     SKIP3                                                                
264001 FD  INFILE                                                               
265001     RECORDING       F                                                    
266001     BLOCK CONTAINS  0.                                                   
267001 01  CSV-RECORD            PIC X(150).                                    
268001     SKIP3                                                                
269001                                                                          
270001     SKIP3                                                                
280001 FD  UTFILE                                                               
290001     RECORDING       F                                                    
300001     BLOCK CONTAINS  0.                                                   
300002 01  UT-POST               PIC X(250).                                    
310001                                                                          
320101     SKIP2                                                                
320201                                                                          
320301 FD  W55304F                                                              
320401     RECORDING       V                                                    
320501     BLOCK CONTAINS  0.                                                   
320601 01  W55304-001                  PIC X(250).                              
320701     SKIP2                                                                
320801                                                                          
320901 WORKING-STORAGE SECTION.                                                 
321001                                                                          
322001 77  IDPGM                       PIC X(8)    VALUE 'W5530400'.            
323001 77  JA                          PIC X       VALUE 'J'.                   
324001 77  NEJ                         PIC X       VALUE 'N'.                   
325001                                                                          
326001 77  DOTS                        PIC X       VALUE '.'.                   
327001 77  COLONS                      PIC X       VALUE ':'.                   
328001 77  COMMAS                      PIC X       VALUE ','.                   
329001 77  DOUBLE-SPACE                PIC XX      VALUE '  '.                  
330001 77  WS-NUMVAL-RESULT            PIC S9(4)   VALUE ZERO COMP-3.           
330002 77  WS-NUMVAL-RESULT1           PIC S9(4)   VALUE ZERO COMP-3.           
330003 77  WS-NUMVAL-RESULT2           PIC S9(4)   VALUE ZERO COMP-3.           
330004 77  WS-NUMVAL-RESULT3           PIC S9(4)   VALUE ZERO COMP-3.           
330005 77  POS                         PIC S9(4)   VALUE ZERO COMP-3.           
340001 77  INTEGERS                    PIC S9(4)   VALUE ZERO COMP-3.           
340002 77  DECIMALS                    PIC S9(4)   VALUE ZERO COMP-3.           
340003 77  INTEGER1                    PIC S9(4)   VALUE ZERO COMP-3.           
340004 77  DECIMAL1                    PIC S9(4)   VALUE ZERO COMP-3.           
340005 77  INTEGER2                    PIC S9(4)   VALUE ZERO COMP-3.           
340006 77  DECIMAL2                    PIC S9(4)   VALUE ZERO COMP-3.           
340007 77  INTEGER3                    PIC S9(4)   VALUE ZERO COMP-3.           
340008 77  DECIMAL3                    PIC S9(4)   VALUE ZERO COMP-3.           
350001 77  IND                         PIC S9(4)   VALUE ZERO COMP-3.           
360001 77  INDATA-RAKN                 PIC S9(4)   VALUE ZERO COMP-3.           
370001                                                                          
380003 77  WS-PRDIRLON                 PIC S9(8).                               
380004 77  WS-INT                      PIC X(5) VALUE SPACE.                    
380005 77  WS-DEC                      PIC X(4) VALUE SPACE.                    
380006 77  WS-INT1                     PIC X(7) VALUE SPACE.                    
380007 77  WS-DEC1                     PIC X(4) VALUE SPACE.                    
380008 77  WS-INT2                     PIC X(5) VALUE SPACE.                    
380009 77  WS-DEC2                     PIC X(4) VALUE SPACE.                    
380010 77  WS-INT3                     PIC X(8) VALUE SPACE.                    
380020 77  WS-DEC3                     PIC X(3) VALUE SPACE.                    
380030 77  WS-PRICE                    PIC 9(8) VALUE ZERO.                     
400001                                                                          
401001 77  WS-PRARTSTD-NUM             PIC S9(7)V9(2) VALUE ZERO.               
401101                                                                          
401201 77  WS-COUNTER1                 PIC S9(3)   VALUE ZERO  COMP-3.          
401301                                                                          
401401 77  INFILE-EOF-SW               PIC X       VALUE 'N'.                   
401501     88  END-OF-INFILE                       VALUE 'J'.                   
401601     88  NOT-END-OF-INFILE                   VALUE 'N'.                   
401701                                                                          
401801 77  INDATA-SW                   PIC X       VALUE 'J'.                   
401901     88  INDATA-OK                           VALUE 'J'.                   
402001     88  INDATA-FEL                          VALUE 'N'.                   
402101                                                                          
402201     EJECT                                                                
402301 01  DAGENS-DATUM                PIC X(8)    VALUE ZERO.                  
402401 01  FILLER REDEFINES DAGENS-DATUM.                                       
402501     03  DAGENS-DATUM-AAR        PIC 9(4).                                
402601     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
402701     03  DAGENS-DATUM-DAG        PIC 9(2).                                
402801     EJECT                                                                
402901 01  DYNAMISKA-SUBPROGRAM.                                                
403001*                                                                         
403101     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
403201     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
403301     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
403401     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
403501     EJECT                                                                
403601*01 -COPY WDECAREA                                                        
403701     EJECT                                                                
403801     SKIP2                                                                
403901*01 -COPY WDATAREA                                                        
404001     EJECT                                                                
404101     SKIP2                                                                
404201*    --- PARAMETRAR TILL ABEND                                            
404301                                                                          
404401 77  RKOD-ABEND                  PIC S9(4) BINARY VALUE +0.               
404501 77  RKOD-SKAPA-FELMAIL          PIC S9(4) BINARY VALUE +7.               
404601 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4) BINARY VALUE +16.              
404701 77  RKOD-ABEND-MED-DUMP         PIC S9(4) BINARY VALUE +1000.            
404801     SKIP2                                                                
404901 01  FELTEXT.                                                             
405001     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
405101     03  FELTEXT-STR             PIC X(100)  VALUE SPACE.                 
405201     EJECT                                                                
405301 01  W001-DAP.                                                            
405401     03  FILLER                  PIC X(165)  VALUE SPACE.                 
405501*    --- PARAMETRAR TILL POSTSUM                                          
405601*                                                                         
405701*01  -COPY W0005   -PRE  POSTSUM-                                         
405801     EJECT                                                                
405901 01  IN-AREA-START               PIC X(24)   VALUE                        
406001                                 'IN-AREA-START  '.                       
406101 01  IN-AREA                     PIC X(150).                              
406201                                                                          
406301 01  FILLER REDEFINES IN-AREA.                                            
406501     03 IN-IDARTNR           PIC 9(9).                                    
406701     03 IN-KDPRIBEH          PIC X.                                       
406705     03 IN-PRDIRLON          PIC X(8).                                    
406708     03 IN-PRDMTRL           PIC X(10).                                   
406720     03 IN-PROVRPAL          PIC X(8).                                    
406750     03 IN-PRARTSTD          PIC X(11).                                   
406902     03 IN-IDUSER            PIC X(8).                                    
407401                                                                          
407501     EJECT                                                                
407601 01  TEST-GCP-AREA           PIC X(250).                                  
407602                                                                          
407603 01  FILLER REDEFINES TEST-GCP-AREA.                                      
407701     03 TEST-IDARTNR           PIC X(15).                                 
408001     03 TEST-KDPRIBEH          PIC X(10).                                 
408005     03 TEST-PRDIRLON          PIC X(8).                                  
408006     03 TEST-PRDIRLON-N  REDEFINES TEST-PRDIRLON                          
408007                             PIC 9(4)V9(3).                               
408008     03 TEST-PRDMTRL           PIC X(10).                                 
408009     03 TEST-PRDMTRL-N   REDEFINES TEST-PRDMTRL                           
408010                             PIC 9(6)V9(3).                               
408020     03 TEST-PROVRPAL          PIC X(8).                                  
408030     03 TEST-PROVRPAL-N  REDEFINES TEST-PROVRPAL                          
408040                             PIC 9(4)V9(3).                               
408050     03 TEST-PRARTSTD          PIC X(11).                                 
408060     03 TEST-PRARTSTD-N  REDEFINES TEST-PRARTSTD                          
408070                             PIC 9(7)V9(2).                               
410002     03 TEST-IDUSER            PIC X(15).                                 
410501                                                                          
410502     EJECT                                                                
410503 01  UT-AREA-START               PIC X(24)   VALUE                        
410504                                             'UT-AREA-START'.             
410505 01  UT-AREA                 PIC X(250).                                  
410506                                                                          
410507 01  FILLER REDEFINES UT-AREA.                                            
410508     03 UT-IDARTNR             PIC 9(9).                                  
410509     03 UT-KDPRIBEH            PIC X(1).                                  
410513     03 UT-PRDIRLON            PIC S9(4)V9(3) COMP-3.                     
410514     03 UT-PRDMTRL             PIC S9(6)V9(3) COMP-3.                     
410515     03 UT-PROVRPAL            PIC S9(4)V9(3) COMP-3.                     
410516     03 UT-PRARTSTD            PIC S9(7)V9(2) COMP-3.                     
410517     03 UT-IDUSER              PIC X(8).                                  
410518                                                                          
410520     EJECT                                                                
410530                                                                          
410601 01  FELL-HEADER.                                                         
410701     03  FILLER            PIC X(7)  VALUE 'PART NO'.                     
410801     03  FILLER            PIC X     VALUE ';'.                           
410802     03  FILLER            PIC X(11) VALUE 'DESCRIPTION'.                 
410803     03  FILLER            PIC X     VALUE ';'.                           
411301     03  FILLER            PIC X(18) VALUE 'PRICE COMMAND CODE'.          
411401     03  FILLER            PIC X     VALUE ';'.                           
411408     03  FILLER            PIC X(13) VALUE 'DIRECT SALARY'.               
411409     03  FILLER            PIC X     VALUE ';'.                           
411410     03  FILLER            PIC X(15) VALUE 'DIRECT MATERIAL'.             
411411     03  FILLER            PIC X     VALUE ';'.                           
411412     03  FILLER            PIC X(15) VALUE 'DIRECT OVERHEAD'.             
411420     03  FILLER            PIC X     VALUE ';'.                           
411701     03  FILLER            PIC X(15) VALUE 'PURCHASED PRICE'.             
411801     03  FILLER            PIC X     VALUE ';'.                           
411802     03  FILLER            PIC X(7)  VALUE 'USER ID'.                     
411803     03  FILLER            PIC X     VALUE ';'.                           
412701     EJECT                                                                
412801                                                                          
412901 01  FELL-DETAIL.                                                         
413201     03  FELL-IDARTNR            PIC X(9).                                
413301     03  FILLER                  PIC X     VALUE ';'.                     
413302     03  FELL-DESCR              PIC X(50) VALUE SPACE.                   
413303     03  FILLER                  PIC X     VALUE ';'.                     
413801     03  FELL-KDPRIBEH           PIC X.                                   
413901     03  FILLER                  PIC X     VALUE ';'.                     
413908     03  FELL-PRDIRLON           PIC X(8).                                
414305     03  FILLER                  PIC X     VALUE ';'.                     
414306     03  FELL-PRDMTRL            PIC X(10).                               
414307     03  FILLER                  PIC X     VALUE ';'.                     
414308     03  FELL-PROVRPAL           PIC X(8).                                
414309     03  FILLER                  PIC X     VALUE ';'.                     
414310     03  FELL-PRARTSTD           PIC X(11).                               
414311     03  FILLER                  PIC X     VALUE ';'.                     
414312     03  FELL-IDUSER             PIC X(8).                                
414313     03  FILLER                  PIC X     VALUE ';'.                     
415000     EJECT                                                                
415501 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
415601 01  FILLER REDEFINES TODAYS-DATE.                                        
415701     03  TODAYS-DATE-YEAR        PIC 9(2).                                
415801     03  TODAYS-DATE-MONTH       PIC 9(2).                                
415901     03  TODAYS-DATE-DAY         PIC 9(2).                                
416001     EJECT                                                                
417001*    --- PARAMETERS TO ABEND                                              
418001                                                                          
419001 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
420001 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
430001 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
440001     SKIP2                                                                
450001 01  ERROR-TEXT.                                                          
460001     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
470001     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
480001     EJECT                                                                
480002                                                                          
540001 PROCEDURE DIVISION.                                                      
550001 MAIN SECTION.                                                            
560001     SKIP2                                                                
570001     PERFORM A-INIT                                                       
580001     PERFORM S01-LAES-INFILE                                              
590001     PERFORM UNTIL END-OF-INFILE                                          
600001         IF INDATA-RAKN = +1                                              
610001*          --- Skip first entry (HEADINGS)                                
620001           PERFORM S01-LAES-INFILE                                        
630001         ELSE                                                             
640001           PERFORM B-KOLLA-INDATA                                         
650001           IF INDATA-OK                                                   
660001             PERFORM C-SKAPA-UTFILE                                       
661001             PERFORM S11-SKRIV-UTFILE                                     
662001           END-IF                                                         
663001           PERFORM S01-LAES-INFILE                                        
664001         END-IF                                                           
665001     END-PERFORM                                                          
666001                                                                          
667001                                                                          
668001     MOVE 'S' TO POSTSUM-OPKOD                                            
669001     CALL POSTSUM USING POSTSUM-PARM                                      
670001                                                                          
670101     PERFORM Z-FINIT                                                      
670201     GOBACK                                                               
670301     .                                                                    
670401     EJECT                                                                
670501 A-INIT SECTION.                                                          
670601     OPEN INPUT  INFILE                                                   
670701     OPEN OUTPUT UTFILE                                                   
670801     OPEN OUTPUT W55304F                                                  
670901     SKIP2                                                                
671001     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
671101                                                                          
671201     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
671301                                                                          
671401     MOVE ZERO TO INDATA-RAKN                                             
671501                                                                          
671601     INITIALIZE UT-AREA                                                   
671701     .                                                                    
671801     EJECT                                                                
671901                                                                          
672001 B-KOLLA-INDATA  SECTION.                                                 
672101     MOVE JA   TO INDATA-SW                                               
672201     MOVE +1   TO IND                                                     
672301     UNSTRING CSV-RECORD  DELIMITED BY ';' OR '  ' INTO                   
672401                                  IN-IDARTNR                              
672701                                  IN-KDPRIBEH                             
672705                                  IN-PRDIRLON                             
672706                                  IN-PRDMTRL                              
672707                                  IN-PROVRPAL                             
672901                                  IN-PRARTSTD                             
672902                                  IN-IDUSER                               
673401                                                                          
673501     UNSTRING CSV-RECORD DELIMITED BY ';' OR '  ' INTO                    
673601                                  TEST-IDARTNR                            
673901                                  TEST-KDPRIBEH                           
673905                                  TEST-PRDIRLON                           
673906                                  TEST-PRDMTRL                            
673907                                  TEST-PROVRPAL                           
674101                                  TEST-PRARTSTD                           
674102                                  TEST-IDUSER                             
674601                                                                          
674701                                                                          
674801*    --- PART NO                                                          
674901     MOVE ZERO TO POS                                                     
675001     INSPECT TEST-IDARTNR TALLYING POS FOR CHARACTERS BEFORE              
675101                                            INITIAL  '  '                 
675201     IF POS > +9                                                          
675301       MOVE NEJ TO INDATA-SW                                              
675401       MOVE 'IN-IDARTNR > 9 TKN :' TO FELTEXT-STR(IND:20)                 
675501       ADD +20 TO IND                                                     
675601       MOVE 'Partno. longer than 9 characters ' TO FELL-DESCR             
675701       PERFORM S05-SKAPA-FELRAD                                           
675801     ELSE                                                                 
675901       IF TEST-IDARTNR(1:POS) NOT NUMERIC                                 
676001         MOVE NEJ TO INDATA-SW                                            
676101         MOVE 'IN-IDARTNR not num :' TO FELTEXT-STR(IND:20)               
676201         ADD +20 TO IND                                                   
676301         MOVE 'Partno.is not numeric ' TO FELL-DESCR                      
676401         PERFORM S05-SKAPA-FELRAD                                         
676501       ELSE                                                               
676601         CONTINUE                                                         
676701       END-IF                                                             
676801     END-IF                                                               
676901                                                                          
676902*    --- PRICE COMMAND CODE                                               
676903     MOVE ZERO TO POS                                                     
676904     INSPECT TEST-KDPRIBEH TALLYING POS FOR CHARACTERS BEFORE             
676905                                            INITIAL  '  '                 
676906     IF POS > +1                                                          
676907       MOVE NEJ TO INDATA-SW                                              
676908       MOVE 'IN-KDPRIBEH > 1 TKN :' TO FELTEXT-STR(IND:21)                
676909       ADD +20 TO IND                                                     
676910       MOVE 'Command code longer than 1 characters ' TO FELL-DESCR        
676920       PERFORM S05-SKAPA-FELRAD                                           
676921     ELSE                                                                 
676923       IF TEST-KDPRIBEH(1:POS) NUMERIC                                    
676924         MOVE NEJ TO INDATA-SW                                            
676925         MOVE 'IN-KDPRIBEH not alpha :' TO FELTEXT-STR(IND:23)            
676926         ADD +20 TO IND                                                   
676927         MOVE 'Command code is invalid ' TO FELL-DESCR                    
676928         PERFORM S05-SKAPA-FELRAD                                         
676929       ELSE                                                               
676930         CONTINUE                                                         
676931       END-IF                                                             
676932     END-IF                                                               
676933                                                                          
676940*    --- DIRECT SALARY                                                    
677220                                                                          
677221     MOVE ZERO TO POS                                                     
677222                  DECIMALS                                                
677223     MOVE SPACES TO WS-INT                                                
677224                    WS-DEC                                                
677225                                                                          
677226     MOVE ZEROES                   TO WS-NUMVAL-RESULT                    
677227     COMPUTE WS-NUMVAL-RESULT = FUNCTION TEST-NUMVAL (                    
677228                                         TEST-PRDIRLON)                   
677230                                                                          
677231     IF WS-NUMVAL-RESULT = 0                                              
677232       INSPECT TEST-PRDIRLON TALLYING POS FOR CHARACTERS                  
677233                           BEFORE INITIAL DOUBLE-SPACE                    
677236                                                                          
677238       UNSTRING TEST-PRDIRLON DELIMITED BY '.'                            
677239                               INTO WS-INT                                
677240                                    WS-DEC                                
677253       IF POS > +8                                                        
677254         MOVE NEJ TO INDATA-SW                                            
677255         MOVE 'PRDIRLON > 8 TKN :' TO FELTEXT-STR(IND:18)                 
677256         ADD +18 TO IND                                                   
677257         MOVE 'Direct salary over Max.length FOR PartNo'                  
677258                                   TO FELL-DESCR                          
677259         PERFORM S05-SKAPA-FELRAD                                         
677260       ELSE                                                               
677261         IF POS > +0                                                      
677262           MOVE ZERO TO INTEGERS                                          
677263                        DECIMALS                                          
677264           INSPECT WS-INT TALLYING INTEGERS FOR CHARACTERS                
677265              BEFORE INITIAL SPACE                                        
677266           INSPECT WS-DEC TALLYING DECIMALS FOR CHARACTERS                
677267              BEFORE INITIAL SPACE                                        
677271           IF INTEGERS > +0                                               
677272             IF WS-INT (1:INTEGERS) NOT NUMERIC                           
677273               MOVE NEJ TO INDATA-SW                                      
677274               MOVE 'PRDIRLON not num :' TO FELTEXT-STR(IND:18)           
677275               ADD +18 TO IND                                             
677276               MOVE 'Direct salary Integer part is not numeric'           
677277                       TO FELL-DESCR                                      
677278               PERFORM S05-SKAPA-FELRAD                                   
677279             ELSE                                                         
677280               IF DECIMALS > +0                                           
677281               AND WS-DEC (1:DECIMALS) NOT NUMERIC                        
677282                 MOVE NEJ TO INDATA-SW                                    
677283                 MOVE 'PRDIRLON NOT NUM :' TO FELTEXT-STR(IND:18)         
677284                 ADD +18 TO IND                                           
677285                 MOVE 'Direct salary decimal part is not numeric'         
677286                      TO FELL-DESCR                                       
677287                 PERFORM S05-SKAPA-FELRAD                                 
677288               ELSE                                                       
677289                 IF INTEGERS > 4 OR  DECIMALS > 3                         
677290                   MOVE NEJ TO INDATA-SW                                  
677291                   MOVE 'PRDIRLON NOT OK:' TO FELTEXT-STR(IND:16)         
677292                   ADD +16 TO IND                                         
677300                   IF INTEGERS > 4                                        
677310                     MOVE 'Direct salary Integers > 4 '                   
677311                                           TO FELL-DESCR                  
677312                   ELSE                                                   
677314                     MOVE 'Direct salary Decimals > 3 '                   
677315                                           TO FELL-DESCR                  
677318                   END-IF                                                 
677319                   PERFORM S05-SKAPA-FELRAD                               
677320                 END-IF                                                   
677321               END-IF                                                     
677329             END-IF                                                       
677330           ELSE                                                           
677331*            --- Direct salary has no integers                            
677332             MOVE NEJ TO INDATA-SW                                        
677333             MOVE 'PRDIRLON HAS NO INTEGERS:'                             
677334                                   TO FELTEXT-STR(IND:25)                 
677335             ADD +25 TO IND                                               
677336             MOVE 'Direct salary has no integers ' TO FELL-DESCR          
677337             PERFORM S05-SKAPA-FELRAD                                     
677338           END-IF                                                         
677339         END-IF                                                           
677340       END-IF                                                             
677341     ELSE                                                                 
677342       IF TEST-PRDIRLON = SPACES                                          
677343*          --- Direct salary not found                                    
677344         MOVE NEJ TO INDATA-SW                                            
677345         MOVE 'PRDIRLON MISSING :' TO FELTEXT-STR(IND:18)                 
677346         ADD +18 TO IND                                                   
677347         MOVE 'No direct salary declared ' TO FELL-DESCR                  
677348         PERFORM S05-SKAPA-FELRAD                                         
677349       ELSE                                                               
677350*          --- Direct salary is invalid                                   
677360         MOVE NEJ TO INDATA-SW                                            
677361         MOVE 'PRDIRLON INVALID :' TO FELTEXT-STR(IND:18)                 
677362         ADD +18 TO IND                                                   
677363         MOVE 'Invalid direct salary ' TO FELL-DESCR                      
677364         PERFORM S05-SKAPA-FELRAD                                         
677365       END-IF                                                             
677366     END-IF                                                               
677367                                                                          
677368*    --- DIRECT MATERIAL                                                  
677370                                                                          
677421                                                                          
677422     MOVE ZERO TO POS                                                     
677423                  DECIMAL1                                                
677424     MOVE SPACES TO WS-INT1                                               
677425                    WS-DEC1                                               
677426     MOVE ZEROES                   TO WS-NUMVAL-RESULT1                   
677427     COMPUTE WS-NUMVAL-RESULT1 = FUNCTION TEST-NUMVAL (                   
677428                                         TEST-PRDMTRL)                    
677429     IF WS-NUMVAL-RESULT1 = 0                                             
677430       INSPECT TEST-PRDMTRL TALLYING POS FOR CHARACTERS                   
677431                           BEFORE INITIAL DOUBLE-SPACE                    
677433                                                                          
677434       UNSTRING TEST-PRDMTRL DELIMITED BY '.'                             
677435                             INTO WS-INT1                                 
677436                                  WS-DEC1                                 
677445                                                                          
677446       IF POS > +10                                                       
677447         MOVE NEJ TO INDATA-SW                                            
677448         MOVE 'PRDMTRL > 10 TKN :' TO FELTEXT-STR(IND:18)                 
677449         ADD +18 TO IND                                                   
677450         MOVE 'Direct material over Max.length FOR PartNo'                
677451                                   TO FELL-DESCR                          
677452         PERFORM S05-SKAPA-FELRAD                                         
677453       ELSE                                                               
677454         IF POS > +0                                                      
677455           MOVE ZERO TO INTEGER1                                          
677456                        DECIMAL1                                          
677457           INSPECT WS-INT1 TALLYING INTEGER1 FOR CHARACTERS               
677458              BEFORE INITIAL SPACE                                        
677459           INSPECT WS-DEC1 TALLYING DECIMAL1 FOR CHARACTERS               
677460              BEFORE INITIAL SPACE                                        
677461           IF INTEGER1 > +0                                               
677462             IF WS-INT1 (1:INTEGER1) NOT NUMERIC                          
677463               MOVE NEJ TO INDATA-SW                                      
677464               MOVE 'PRDMTRL not num :' TO FELTEXT-STR(IND:17)            
677465               ADD +18 TO IND                                             
677466               MOVE 'Direct material Integer part is not numeric'         
677467                       TO FELL-DESCR                                      
677468               PERFORM S05-SKAPA-FELRAD                                   
677469             ELSE                                                         
677470               IF DECIMAL1 > +0                                           
677471               AND WS-DEC1(1:DECIMAL1) NOT NUMERIC                        
677472                 MOVE NEJ TO INDATA-SW                                    
677473                 MOVE 'PRDMTRL NOT NUM :' TO FELTEXT-STR(IND:17)          
677474                 ADD +18 TO IND                                           
677475                 MOVE 'Direct material decimal part not numeric'          
677476                      TO FELL-DESCR                                       
677477                 PERFORM S05-SKAPA-FELRAD                                 
677478               ELSE                                                       
677479                 IF INTEGER1 > 6 OR  DECIMAL1 > 3                         
677480                   MOVE NEJ TO INDATA-SW                                  
677481                   MOVE 'PRDMTRL NOT OK:' TO FELTEXT-STR(IND:15)          
677482                   ADD +16 TO IND                                         
677483                   IF INTEGER1 > 6                                        
677484                     MOVE 'Direct material Integers > 6 '                 
677485                                           TO FELL-DESCR                  
677486                   ELSE                                                   
677487                     MOVE 'Direct material Decimals > 3 '                 
677488                                           TO FELL-DESCR                  
677491                   END-IF                                                 
677492                   PERFORM S05-SKAPA-FELRAD                               
677493                 END-IF                                                   
677494               END-IF                                                     
677502             END-IF                                                       
677503           ELSE                                                           
677504*            --- Direct material has no integers                          
677505             MOVE NEJ TO INDATA-SW                                        
677506             MOVE 'PRDMTRL HAS NO INTEGERS:'                              
677507                                   TO FELTEXT-STR(IND:24)                 
677508             ADD +25 TO IND                                               
677509             MOVE 'Direct material has no integers ' TO FELL-DESCR        
677510             PERFORM S05-SKAPA-FELRAD                                     
677511           END-IF                                                         
677513         END-IF                                                           
677514       END-IF                                                             
677515     ELSE                                                                 
677516       IF TEST-PRDMTRL = SPACES                                           
677518*          --- Direct material not found                                  
677519         MOVE NEJ TO INDATA-SW                                            
677520         MOVE 'PRDMTRL MISSING :' TO FELTEXT-STR(IND:17)                  
677521         ADD +18 TO IND                                                   
677522         MOVE 'No direct material declared ' TO FELL-DESCR                
677523         PERFORM S05-SKAPA-FELRAD                                         
677524       ELSE                                                               
677525*        --- Direct material is invalid                                   
677526         MOVE NEJ TO INDATA-SW                                            
677527         MOVE 'PRDMTRL INVALID :' TO FELTEXT-STR(IND:18)                  
677528         ADD +18 TO IND                                                   
677529         MOVE 'Invalid direct material ' TO FELL-DESCR                    
677530         PERFORM S05-SKAPA-FELRAD                                         
677531       END-IF                                                             
677532     END-IF                                                               
677533                                                                          
677534*    --- Direct overhead                                                  
677578                                                                          
677579     MOVE ZERO TO POS                                                     
677580                  DECIMAL2                                                
677581     MOVE SPACES TO WS-INT2                                               
677582                    WS-DEC2                                               
677583     MOVE ZEROES                   TO WS-NUMVAL-RESULT2                   
677584     COMPUTE WS-NUMVAL-RESULT2 = FUNCTION TEST-NUMVAL (                   
677585                                         TEST-PROVRPAL)                   
677586     IF WS-NUMVAL-RESULT2 = 0                                             
677587        INSPECT TEST-PROVRPAL TALLYING POS FOR CHARACTERS                 
677588                            BEFORE INITIAL DOUBLE-SPACE                   
677590                                                                          
677591        UNSTRING TEST-PROVRPAL DELIMITED BY '.'                           
677592                             INTO WS-INT2                                 
677593                                  WS-DEC2                                 
677594                                                                          
677603       IF POS > +8                                                        
677604         MOVE NEJ TO INDATA-SW                                            
677605         MOVE 'PROVRPAL > 8 TKN :' TO FELTEXT-STR(IND:18)                 
677606         ADD +18 TO IND                                                   
677607         MOVE 'Direct overhead over Max.length FOR PartNo'                
677608                                   TO FELL-DESCR                          
677609         PERFORM S05-SKAPA-FELRAD                                         
677610       ELSE                                                               
677611         IF POS > +0                                                      
677612           MOVE ZERO TO INTEGER2                                          
677613                        DECIMAL2                                          
677614           INSPECT WS-INT2 TALLYING INTEGER2 FOR CHARACTERS               
677615              BEFORE INITIAL SPACE                                        
677616           INSPECT WS-DEC2 TALLYING DECIMAL2 FOR CHARACTERS               
677617              BEFORE INITIAL SPACE                                        
677618           IF INTEGER2 > +0                                               
677619             IF WS-INT2 (1:INTEGER2) NOT NUMERIC                          
677620               MOVE NEJ TO INDATA-SW                                      
677621               MOVE 'PROVRPAL not num :' TO FELTEXT-STR(IND:18)           
677622               ADD +18 TO IND                                             
677623               MOVE 'Direct overhead Integer part is not numeric'         
677624                       TO FELL-DESCR                                      
677625               PERFORM S05-SKAPA-FELRAD                                   
677626             ELSE                                                         
677627               IF DECIMAL2 > +0                                           
677628               AND WS-DEC2 (1:DECIMAL2) NOT NUMERIC                       
677629                 MOVE NEJ TO INDATA-SW                                    
677630                 MOVE 'PROVRPAL NOT NUM :' TO FELTEXT-STR(IND:18)         
677631                 ADD +18 TO IND                                           
677632                 MOVE 'Direct overhea decimal part is not numeric'        
677633                      TO FELL-DESCR                                       
677634                 PERFORM S05-SKAPA-FELRAD                                 
677635               ELSE                                                       
677636                 IF INTEGER2 > 4 OR  DECIMAL2 > 3                         
677637                   MOVE NEJ TO INDATA-SW                                  
677638                   MOVE 'PROVRPAL NOT OK:' TO FELTEXT-STR(IND:16)         
677639                   ADD +16 TO IND                                         
677640                   IF INTEGER2 > 4                                        
677641                     MOVE 'Direct overhead Integers > 4 '                 
677642                                           TO FELL-DESCR                  
677643                   ELSE                                                   
677644                     MOVE 'Direct overhead Decimals > 3 '                 
677645                                           TO FELL-DESCR                  
677646                   END-IF                                                 
677647                   PERFORM S05-SKAPA-FELRAD                               
677648                 END-IF                                                   
677649               END-IF                                                     
677650             END-IF                                                       
677651           ELSE                                                           
677652*            --- Direct overhead has no integers                          
677653             MOVE NEJ TO INDATA-SW                                        
677654             MOVE 'PROVRPAL HAS NO INTEGERS:'                             
677655                                   TO FELTEXT-STR(IND:25)                 
677656             ADD +25 TO IND                                               
677657             MOVE 'Direct overhead has no integers ' TO FELL-DESCR        
677658             PERFORM S05-SKAPA-FELRAD                                     
677659           END-IF                                                         
677660         END-IF                                                           
677661       END-IF                                                             
677662     ELSE                                                                 
677663       IF TEST-PROVRPAL = SPACES                                          
677664*        --- Direct overhead not found                                    
677665         MOVE NEJ TO INDATA-SW                                            
677666         MOVE 'PROVRPAL MISSING :' TO FELTEXT-STR(IND:18)                 
677667         ADD +18 TO IND                                                   
677668         MOVE 'No direct overhead declared ' TO FELL-DESCR                
677669         PERFORM S05-SKAPA-FELRAD                                         
677670       ELSE                                                               
677671*        --- Direct overhead is invalid                                   
677672         MOVE NEJ TO INDATA-SW                                            
677673         MOVE 'PROVRPAL INVALID :' TO FELTEXT-STR(IND:18)                 
677674         ADD +18 TO IND                                                   
677675         MOVE 'Invalid direct overhead' TO FELL-DESCR                     
677676         PERFORM S05-SKAPA-FELRAD                                         
677677       END-IF                                                             
677678     END-IF                                                               
677679                                                                          
677680*    --- PURCHASED PRICE                                                  
677681                                                                          
684401                                                                          
684402     MOVE ZERO TO POS                                                     
684403                  DECIMAL3                                                
684404     MOVE SPACES TO WS-INT3                                               
684405                    WS-DEC3                                               
684406     MOVE ZEROES                   TO WS-NUMVAL-RESULT3                   
684407     COMPUTE WS-NUMVAL-RESULT3 = FUNCTION TEST-NUMVAL (                   
684408                                         TEST-PRARTSTD)                   
684409     IF WS-NUMVAL-RESULT3 = 0                                             
684410        INSPECT TEST-PRARTSTD TALLYING POS FOR CHARACTERS                 
684411                           BEFORE INITIAL DOUBLE-SPACE                    
684413                                                                          
684414        UNSTRING TEST-PRARTSTD DELIMITED BY '.'                           
684415                             INTO WS-INT3                                 
684416                                  WS-DEC3                                 
684417                                                                          
684425       IF POS > +9                                                        
684426         MOVE NEJ TO INDATA-SW                                            
684427         MOVE 'PRARTSTD > 9 TKN :' TO FELTEXT-STR(IND:18)                 
684428         ADD +18 TO IND                                                   
684429         MOVE 'Purchased price over Max.length FOR PartNo'                
684430                                   TO FELL-DESCR                          
684431         PERFORM S05-SKAPA-FELRAD                                         
684432       ELSE                                                               
684433         IF POS > +0                                                      
684434           MOVE ZERO TO INTEGER3                                          
684435                        DECIMAL3                                          
684436           INSPECT WS-INT3 TALLYING INTEGER3 FOR CHARACTERS               
684437              BEFORE INITIAL SPACE                                        
684438           INSPECT WS-DEC3 TALLYING DECIMAL3 FOR CHARACTERS               
684439              BEFORE INITIAL SPACE                                        
684440           IF TEST-PRARTSTD <= ZERO                                       
684441*            MOVE 'PRARTSTD < 0 TKN :' TO FELTEXT-STR(IND:18)             
684442*            MOVE NEJ TO INDATA-SW                                        
684443*            MOVE 'Purchased price should not be zero'                    
684444*                                      TO FELL-DESCR                      
684445*            PERFORM S05-SKAPA-FELRAD                                     
684446             CONTINUE                                                     
684447           ELSE                                                           
684448             IF INTEGER3 > +0                                             
684449               IF WS-INT3 (1:INTEGER3) NOT NUMERIC                        
684451                 MOVE NEJ TO INDATA-SW                                    
684452                 MOVE 'PRARTSTD not num :' TO FELTEXT-STR(IND:18)         
684453                 ADD +18 TO IND                                           
684454                 MOVE 'Purchased price Integer is not numeric'            
684455                         TO FELL-DESCR                                    
684456                 PERFORM S05-SKAPA-FELRAD                                 
684457               ELSE                                                       
684458                 IF DECIMAL3 > +0                                         
684459                 AND WS-DEC3 (1:DECIMAL3) NOT NUMERIC                     
684460                   MOVE NEJ TO INDATA-SW                                  
684461                   MOVE 'PRARTSTD NOT NUM :'                              
684462                                           TO FELTEXT-STR(IND:18)         
684463                   ADD +18 TO IND                                         
684464                   MOVE 'Purchased price decimal part not numeric'        
684465                        TO FELL-DESCR                                     
684481                   PERFORM S05-SKAPA-FELRAD                               
684482                 ELSE                                                     
684483                   IF INTEGER3 > 7 OR DECIMAL3 > 2                        
684484                     MOVE NEJ TO INDATA-SW                                
684485                     MOVE 'PRARTSTD NOT OK:'                              
684486                                           TO FELTEXT-STR(IND:16)         
684487                     ADD +16 TO IND                                       
684488                     IF INTEGER3 > 7                                      
684489                       MOVE 'Purchased price Integers > 7 '               
684490                                             TO FELL-DESCR                
684491                     ELSE                                                 
684492                       MOVE 'Purchased price Decimals > 2 '               
684493                                             TO FELL-DESCR                
684494                     END-IF                                               
684495                     PERFORM S05-SKAPA-FELRAD                             
684496                   END-IF                                                 
684497                 END-IF                                                   
684498               END-IF                                                     
684499             ELSE                                                         
684500*              --- Purchased price has no integers                        
684501               MOVE NEJ TO INDATA-SW                                      
684502               MOVE 'PRARTSTD HAS NO INTEGERS:'                           
684503                                     TO FELTEXT-STR(IND:25)               
684504               ADD +25 TO IND                                             
684505               MOVE 'Purchased price has no integers'                     
684506                                     TO FELL-DESCR                        
684507               PERFORM S05-SKAPA-FELRAD                                   
684508             END-IF                                                       
684509           END-IF                                                         
684510         END-IF                                                           
684511       END-IF                                                             
684512     ELSE                                                                 
684513       IF TEST-PRARTSTD = SPACES                                          
684514*        --- Purchased price not found                                    
684515         MOVE NEJ TO INDATA-SW                                            
684516         MOVE 'PRARTSTD MISSING :' TO FELTEXT-STR(IND:18)                 
684517         ADD +18 TO IND                                                   
684518         MOVE 'No purchased price declared ' TO FELL-DESCR                
684519         PERFORM S05-SKAPA-FELRAD                                         
684520       ELSE                                                               
684521*        --- purchased price is invalid                                   
684522         MOVE NEJ TO INDATA-SW                                            
684523         MOVE 'PRARTSTD INVALID :' TO FELTEXT-STR(IND:18)                 
684524         ADD +18 TO IND                                                   
684525         MOVE 'Invalid purchased price ' TO FELL-DESCR                    
684526         PERFORM S05-SKAPA-FELRAD                                         
684527       END-IF                                                             
684528     END-IF                                                               
684529                                                                          
684530*    --- USER ID                                                          
684531     MOVE ZERO TO POS                                                     
684532     INSPECT TEST-IDUSER TALLYING POS FOR CHARACTERS BEFORE               
684533                                            INITIAL  '  '                 
684534     IF POS > +8                                                          
684535       MOVE NEJ TO INDATA-SW                                              
684536       MOVE 'IN-IDUSER > 8 TKN :' TO FELTEXT-STR(IND:19)                  
684537       ADD +20 TO IND                                                     
684538       MOVE 'USER ID longer than 8 characters ' TO FELL-DESCR             
684539       PERFORM S05-SKAPA-FELRAD                                           
684540     ELSE                                                                 
684541       IF TEST-IDUSER = SPACE                                             
684542         MOVE NEJ TO INDATA-SW                                            
684543         MOVE 'IN-IDUSER INVALID :' TO FELTEXT-STR(IND:19)                
684544         ADD +20 TO IND                                                   
684545         MOVE 'User id is empty ' TO FELL-DESCR                           
684546         PERFORM S05-SKAPA-FELRAD                                         
684547       ELSE                                                               
684548         CONTINUE                                                         
684549       END-IF                                                             
684550     END-IF                                                               
684560                                                                          
684600     .                                                                    
690300     EJECT                                                                
690401 C-SKAPA-UTFILE  SECTION.                                                 
691000                                                                          
691101     MOVE IN-IDARTNR    TO UT-IDARTNR                                     
691102     MOVE IN-KDPRIBEH   TO UT-KDPRIBEH                                    
691106     INSPECT TEST-PRDIRLON REPLACING ALL COMMAS BY DOTS                   
691107     COMPUTE TEST-PRDIRLON-N = FUNCTION NUMVAL(TEST-PRDIRLON)             
691108     MULTIPLY TEST-PRDIRLON-N BY 1                                        
691109                        GIVING UT-PRDIRLON                                
691112     INSPECT TEST-PRDMTRL REPLACING ALL COMMAS BY DOTS                    
691113     COMPUTE TEST-PRDMTRL-N = FUNCTION NUMVAL(TEST-PRDMTRL)               
691114     MULTIPLY TEST-PRDMTRL-N BY 1                                         
691115                        GIVING UT-PRDMTRL                                 
691117     INSPECT TEST-PROVRPAL REPLACING ALL COMMAS BY DOTS                   
691118     COMPUTE TEST-PROVRPAL-N = FUNCTION NUMVAL(TEST-PROVRPAL)             
691119     MULTIPLY TEST-PROVRPAL-N BY 1                                        
691120                        GIVING UT-PROVRPAL                                
691140     INSPECT TEST-PRARTSTD REPLACING ALL COMMAS BY DOTS                   
691150     COMPUTE TEST-PRARTSTD-N = FUNCTION NUMVAL(TEST-PRARTSTD)             
691160     MULTIPLY TEST-PRARTSTD-N BY 1                                        
691170                        GIVING UT-PRARTSTD                                
692701     MOVE IN-IDUSER     TO UT-IDUSER                                      
692801                                                                          
693000     .                                                                    
693100     EJECT                                                                
693201                                                                          
693300 Z-FINIT SECTION.                                                         
693400     CLOSE INFILE                                                         
693501           UTFILE                                                         
693601           W55304F                                                        
693700     SKIP2                                                                
693800     .                                                                    
693900     EJECT                                                                
694001                                                                          
694100 S01-LAES-INFILE  SECTION.                                                
694200     READ INFILE INTO IN-AREA                                             
694300     AT END                                                               
694400        MOVE HIGH-VALUE TO IN-AREA                                        
694500        SET END-OF-INFILE TO TRUE                                         
694600                                                                          
694700     NOT AT END                                                           
694800        MOVE 'INFILE'   TO POSTSUM-FDNAMN                                 
694901        MOVE 'W55304D1' TO POSTSUM-DDNAMN2                                
695000        MOVE SPACE      TO POSTSUM-TRANSTYP                               
695100        CALL POSTSUM USING POSTSUM-PARM                                   
695200                                                                          
695300        ADD +1 TO INDATA-RAKN                                             
695400     END-READ                                                             
695500     .                                                                    
695600     EJECT                                                                
695700                                                                          
695801 S11-SKRIV-UTFILE SECTION.                                                
695901     WRITE UT-POST FROM UT-AREA                                           
696000                                                                          
696100     MOVE 'UT '         TO POSTSUM-TRANSTYP                               
696201     MOVE 'W55304'      TO POSTSUM-FDNAMN                                 
696301     MOVE 'W55304D2'    TO POSTSUM-DDNAMN2                                
696400     CALL POSTSUM USING POSTSUM-PARM                                      
696500     .                                                                    
696600     EJECT                                                                
696701                                                                          
696801 S05-SKAPA-FELRAD SECTION.                                                
696901     MOVE IN-IDARTNR           TO FELL-IDARTNR                            
696904     MOVE IN-KDPRIBEH          TO FELL-KDPRIBEH                           
696908     MOVE IN-PRDIRLON          TO FELL-PRDIRLON                           
696911     MOVE IN-PRDMTRL           TO FELL-PRDMTRL                            
696920     MOVE IN-PROVRPAL          TO FELL-PROVRPAL                           
697101     MOVE IN-PRARTSTD          TO FELL-PRARTSTD                           
697601     MOVE IN-IDUSER            TO FELL-IDUSER                             
697701                                                                          
697801     PERFORM S06-SKRIV-ERRFILE                                            
697901     .                                                                    
698001     EJECT                                                                
698101                                                                          
698201 S06-SKRIV-ERRFILE     SECTION.                                           
698301     IF WS-COUNTER1 = ZERO                                                
698401       MOVE  ' ¤DAPW55304-001' TO W001-DAP                                
698501       WRITE W55304-001   FROM W001-DAP                                   
698601       MOVE  ' ¤DAPW553'       TO W001-DAP                                
698701       WRITE W55304-001   FROM W001-DAP                                   
698801       WRITE W55304-001   FROM FELL-HEADER                                
698901     END-IF                                                               
699001     WRITE W55304-001   FROM FELL-DETAIL                                  
699101     ADD +1 TO WS-COUNTER1                                                
699201     .                                                                    
699301     SKIP3                                                                
