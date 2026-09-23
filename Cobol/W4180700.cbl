000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W4180700.                                                
000301 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000401 DATE-WRITTEN.   94/10/30.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*        BYTER UT KDCLAGER MOT IDDC GENOM LÄSNINGAR MOT WDB5              
000901*                                                                         
001001*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
001101*        PROGRAMMET LÄSER      WDB6                                       
001201*        PROGRAMMET LÄSER      WDK7                                       
001301*        PROGRAMMET LÄSER      WDK6                                       
001401*                                                                         
001501*    ABENDKODER:                                                          
001601*        U0016 -  . . . .                                                 
001701*        U1000 -  . . . .                                                 
001801*                                                                         
001901* CHANGE LOG:  E'TRACKER  2913019     20051201                            
002001*                         5823276     20071114                            
002101*                         5838822     20071107                            
002201*                                                                         
002301*                                                                         
002401                                                                          
002501     SKIP3                                                                
002601 ENVIRONMENT DIVISION.                                                    
002701     SKIP2                                                                
002801 INPUT-OUTPUT SECTION.                                                    
002901                                                                          
003001 FILE-CONTROL.                                                            
003101     SKIP2                                                                
003201*          --- INFIL                                                      
003301     SELECT INFIL                      ASSIGN TO W41807D1.                
003401     SKIP2                                                                
003501*          --- UTFIL                                                      
003601     SELECT W41807                     ASSIGN TO W41807D2.                
003701     EJECT                                                                
003801 DATA DIVISION.                                                           
003901     SKIP2                                                                
004001 FILE SECTION.                                                            
004101     SKIP3                                                                
004201 FD  INFIL                                                                
004301     RECORDING       V                                                    
004401     BLOCK CONTAINS  0.                                                   
004501                                                                          
004601 01  W41809C-POST.                                                        
004701*  03  -COPY WZ01REQU    -L.                                              
004801*  03  -COPY W418001C    -L.                                              
004901*01  W41809B-POST    -COPY W418001B    -L.                                
005001*01  W41803-POST     -COPY W41803      -L.                                
005101*01  W4758P-POST     -COPY W418REF     -L.                                
005201     SKIP3                                                                
005301 FD  W41807                                                               
005401     RECORDING       V                                                    
005501     BLOCK CONTAINS  0.                                                   
005601                                                                          
005701*01  W41807-POST    -COPY W41807      -L.                                 
005801     EJECT                                                                
005901 WORKING-STORAGE SECTION.                                                 
006001                                                                          
006101*    -- CHECKED BY WY2000                                                 
006201     SKIP3                                                                
006301 77  IDPGM                       PIC X(8)    VALUE 'W4180700'.            
006401 77  JA                          PIC X       VALUE 'J'.                   
006501 77  NEJ                         PIC X       VALUE 'N'.                   
006601 77  WC-KDANMORS                 PIC X(8)    VALUE 'KDANMORS'.            
006701 77  WC-IDFKNGRP                 PIC X(8)    VALUE 'IDFKNGRP'.            
006801 77  WC-IDARTNR                  PIC X(8)    VALUE 'IDARTNR '.            
006901 77  WC-IDDC-EXCP                PIC X(8)    VALUE 'IDDC    '.            
007001                                                                          
007101 01  WS-IDORDNR                  PIC 9(7).                                
007201 01  WS-IDORDNR7 REDEFINES WS-IDORDNR.                                    
007301     03                          PIC 9(2).                                
007401     03  WS-IDORDNR5             PIC 9(5).                                
007501                                                                          
007601*    --- PARAMETRAR TILL ABEND                                            
007701 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007801 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007901     SKIP2                                                                
008001                                                                          
008101 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
008201     88  END-OF-INFIL                        VALUE 'J'.                   
008301     EJECT                                                                
008401                                                                          
008501 77  GODK-KOD-SW                 PIC X.                                   
008601     88  GODK-KOD                            VALUE 'J'.                   
008701     88  EJ-GODK-KOD                         VALUE 'N'.                   
008801     EJECT                                                                
008901                                                                          
009001 77  GODK-ARTIKEL-SW             PIC X.                                   
009101     88  GODK-ARTIKEL                        VALUE 'J'.                   
009201     88  EJ-GODK-ARTIKEL                     VALUE 'N'.                   
009301     EJECT                                                                
009401                                                                          
009501 77  GODK-IDFKNGRP-SW            PIC X.                                   
009601     88  GODK-IDFKNGRP                       VALUE 'J'.                   
009701     88  EJ-GODK-IDFKNGRP                    VALUE 'N'.                   
009801     EJECT                                                                
009901                                                                          
010001 77  GODK-DC-ARTIKEL-SW          PIC X.                                   
010101     88  GODK-DC-ARTIKEL                     VALUE 'J'.                   
010201     88  EJ-GODK-DC-ARTIKEL                  VALUE 'N'.                   
010301     EJECT                                                                
010401                                                                          
010501 77  GODK-DC-LEV-SW              PIC X.                                   
010601     88  GODK-DC-LEV                         VALUE 'J'.                   
010701     88  EJ-GODK-DC-LEV                      VALUE 'N'.                   
010801     EJECT                                                                
010901                                                                          
011001 77  WS-KDANMORS                 PIC X(2)   VALUE SPACE.                  
011101 77  WS-IDARTNR                  PIC S9(9)  VALUE ZERO COMP-3.            
011201 77  WS-IDDC-LEV                 PIC X(2)   VALUE SPACE.                  
011403                                                                          
011503 01  TEST-IDDISTR-LDC            PIC S9(5) COMP-3.                        
011603     EJECT                                                                
011703                                                                          
011803 01  TEST-IDKUNDNR-LDC           PIC S9(7) COMP-3.                        
011903     EJECT                                                                
012003                                                                          
012103 01  TEST-IDDISTR                PIC S9(5) COMP-3.                        
012203*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
012303     EJECT                                                                
012403                                                                          
012503                                                                          
012603*      --- VALID IDDC CODES                                               
012703*                                                                         
012803*01    -COPY WWDCKONS                                                     
012903*01    -COPY WWDC99                                                       
013003       EJECT                                                              
013103                                                                          
013203 01  DYNAMISKA-SUBPROGRAM.                                                
013303*                                                                         
013403     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013503     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013603     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013703     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013803     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
013903     SKIP2                                                                
014003                                                                          
014103 01  FELTEXT.                                                             
014203     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014303     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014403     EJECT                                                                
014503*    --- PARAMETRAR TILL POSTSUM                                          
014603*                                                                         
014703*01  -COPY W0005   -PRE  POSTSUM-                                         
014803     EJECT                                                                
014903*    ---  LÄNKAREA TILL W418OKOD                                          
015003 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
015103                                                                          
015203*01  -COPY W418OKOD           -PRE OKOD-.                                 
015303     EJECT                                                                
015403 01  IN-AREA-START               PIC X(24)   VALUE                        
015503                                 'IN-AREA-START  '.                       
015603 01  IN-AREA.                                                             
015703    03 IN-IDPTYP                 PIC X(3).                                
015803    03                           PIC X(1000).                             
015903                                                                          
016003 01  SPAR-AREA                   PIC X(24)   VALUE                        
016103                                 'SPAR-AREA      '.                       
016203*01  -COPY W41803   -PRE  IN03-                                           
016303     EJECT                                                                
016403*01  -COPY W418001B -PRE  IN09B-                                          
016503     EJECT                                                                
016603 01  IN09C-AREA.                                                          
016703*    03  FILLER -COPY WZ01REQU -PRE  IN09C-                               
016803*    03  FILLER -COPY W418001C -PRE  IN09C-                               
016903     EJECT                                                                
017003*01  -COPY W418REF  -PRE  INREF-.                                         
017103     EJECT                                                                
017203                                                                          
017303 01  UT-AREA-START               PIC X(24)   VALUE                        
017403                                 'UT-AREA-START  '.                       
017503*01  -COPY W41807 -PRE  UT-                                               
017603     EJECT                                                                
017703*01  -COPY WWIDFTG                                                        
017803     EJECT                                                                
017903*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018003*                                                                         
018103     EJECT                                                                
018203 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018303     SKIP3                                                                
018403 01  NYCKLAR-TILL-DLI.                                                    
018503                                                                          
018603    03 W-IDGMT-X.                                                         
018703       05 W-IDDISTR              PIC S9(5)    COMP-3.                     
018803       05 W-IDKUNDNR             PIC S9(7)    COMP-3.                     
018903                                                                          
019003    03 W-IDGMT-MIN-X.                                                     
019103       05 W-IDDISTR-WDB2-MIN     PIC S9(5)    COMP-3.                     
019203       05 W-IDKUNDNR-WDB2-MIN    PIC S9(7)    COMP-3.                     
019303                                                                          
019403    03 W-IDGMT-MAX-X.                                                     
019503       05 W-IDDISTR-WDB2-MAX     PIC S9(5)    COMP-3.                     
019603       05 W-IDKUNDNR-WDB2-MAX    PIC S9(7)    COMP-3.                     
019703                                                                          
019803    03  W-IDDC-B6-X.                                                      
019903      05  W-IDDC-B6              PIC X(2)     VALUE SPACE.                
020003                                                                          
020103    03  W-WDB611KY-X.                                                     
021001      05  W-URV-TEELMT            PIC X(16)  VALUE SPACE.                 
021101      05  W-URV-FILLER            PIC X(20)  VALUE SPACE.                 
021201      05  W-URV-IDARTNR-EXCP-FILLER REDEFINES W-URV-FILLER.               
021301        07  W-URV-IDARTNR-EXCP    PIC 9(9).                               
021401        07  FILLER                PIC X(11).                              
021501      05  W-URV-IDFKNGRP-EXCP-FILLER REDEFINES W-URV-FILLER.              
021601        07  W-URV-IDFKNGRP-EXCP   PIC 9(4).                               
021701        07  FILLER                PIC X(16).                              
021801      05  W-URV-KDANMORS-RET-FILLER REDEFINES W-URV-FILLER.               
021901        07  W-URV-KDANMORS-RET    PIC X(2).                               
022001        07  FILLER                PIC X(18).                              
022101       05  W-URV-IDDC-EXCP-FILLER REDEFINES W-URV-FILLER.                 
022201         07  W-URV-IDDC-EXCP       PIC X(2).                              
022301         07  FILLER                PIC X(18).                             
022401                                                                          
022501    03  W-IDDC-K7-X.                                                      
022601      05  W-IDDC-K7              PIC X(2)     VALUE SPACE.                
022701                                                                          
022801    03 W-IDARTNR-X.                                                       
022901       05 W-IDARTNR              PIC S9(9)    COMP-3 VALUE ZERO.          
023001                                                                          
023101     03 W-IDARTNR-K6-X.                                                   
023201       05  W-IDARTNR-K6          PIC S9(9)    COMP-3 VALUE ZERO.          
023301                                                                          
023401     SKIP2                                                                
023501*    --- STATUS-KOD FRÅN IMS                                              
023601 01  STATUS-WS                   PIC XX.                                  
023701     88  SEGMENT-FINNS                       VALUE '  '.                  
023801     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023901     SKIP2                                                                
024001 01  GODK-STATUSKODER.                                                    
024101     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024201     SKIP3                                                                
024301 01  SSA1                        PIC X(64).                               
024401 01  SSA2                        PIC X(64).                               
024501     EJECT                                                                
024601*    --- IMS FUNKTIONSKODER                                               
024701*01  -COPY W0003                                                          
024801     EJECT                                                                
024901*    ---  DLI INPUT-OUTPUT AREA                                           
025001 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDB201'.        
025101     SKIP3                                                                
025201 01  DLI-IO-WDB201.                                                       
025301*  03  -COPY WDB201                                                       
025401     EJECT                                                                
025501 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDB601'.          
025601 01  DLI-IO-WDB601.                                                       
025701*    03  -COPY WDB601                                                     
025801     EJECT                                                                
025901 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDB611'.          
026001 01  DLI-IO-WDB611.                                                       
026101*    03  -COPY WDB611                                                     
026201     EJECT                                                                
026301 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDK711'.          
026401 01  DLI-IO-WDK711.                                                       
026501*    03  -COPY WDK711                                                     
026601     EJECT                                                                
026701 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDK601'.          
026801 01  DLI-IO-WDK601.                                                       
026901*    03  -COPY WDK601                                                     
027001                                                                          
027101     EJECT                                                                
027201 LINKAGE SECTION.                                                         
027301                                                                          
027401     EJECT                                                                
027501*01  -COPY W0008  -PRE GMTA-                                              
027601     05  FILLER                  PIC X.                                   
027701     EJECT                                                                
027801*01  -COPY W0008  -PRE WDB1-                                              
027901     05  FILLER                  PIC X.                                   
028001     EJECT                                                                
028101*01  -COPY W0008  -PRE WDB6-                                              
028201     05  FILLER                  PIC X.                                   
028301     EJECT                                                                
028401*01  -COPY W0008  -PRE WDK7-                                              
028501     05  FILLER                  PIC X.                                   
028601     EJECT                                                                
028701*01  -COPY W0008  -PRE WDK6-                                              
028801     05  FILLER                  PIC X.                                   
028901     EJECT                                                                
029001 PROCEDURE DIVISION  USING GMTA-PCB WDB1-PCB WDB6-PCB WDK7-PCB            
029101                           WDK6-PCB.                                      
029201 MAIN SECTION.                                                            
029301     ENTRY 'DLITCBL' USING GMTA-PCB WDB1-PCB WDB6-PCB WDK7-PCB            
029401                           WDK6-PCB.                                      
029501                                                                          
029601                                                                          
029701     PERFORM A-INIT                                                       
029801     PERFORM S01-LAES-INFIL                                               
029901     PERFORM UNTIL END-OF-INFIL                                           
030001                                                                          
030101       PERFORM B-BEHANDLA-POSTER                                          
030201                                                                          
030301       PERFORM S01-LAES-INFIL                                             
030401     END-PERFORM                                                          
030501                                                                          
030601                                                                          
030701     PERFORM Z-FINIT                                                      
030801                                                                          
030901     MOVE ZERO TO RETURN-CODE                                             
031001     GOBACK                                                               
031101     .                                                                    
031201     EJECT                                                                
031301 A-INIT SECTION.                                                          
031401                                                                          
031501     OPEN INPUT  INFIL                                                    
031601                                                                          
031701     OPEN OUTPUT W41807                                                   
031801                                                                          
031901     MOVE IDPGM                    TO POSTSUM-PROGNAMN                    
032001                                                                          
032101     MOVE LOW-VALUE                TO W-IDGMT-MIN-X                       
032201                                      W-IDGMT-X                           
032301     MOVE HIGH-VALUE               TO IN03-W41803                         
032401                                      UT-W41807-CTX                       
032501                                      W-IDGMT-MAX-X                       
032601     .                                                                    
032701     EJECT                                                                
032801 B-BEHANDLA-POSTER SECTION.                                               
032901                                                                          
033001     EVALUATE IN-IDPTYP                                                   
033101        WHEN 'BAT'                                                        
033201           MOVE IN-AREA            TO IN03-W41803                         
033301           PERFORM BA-FLYTTA-POST                                         
033401        WHEN '01B'                                                        
033501           MOVE IN-AREA            TO IN09B-W418001B                      
033601           PERFORM BC-FLYTTA-POST                                         
033701        WHEN 'REF'                                                        
033801        WHEN 'FAK'                                                        
033901           MOVE IN-AREA            TO INREF-W418REF                       
034001           PERFORM BD-FLYTTA-POST                                         
034101        WHEN '001'                                                        
034201           MOVE IN-AREA            TO IN09C-AREA                          
034301           PERFORM BE-FLYTTA-POST                                         
034401                                                                          
034501        WHEN OTHER                                                        
034601           MOVE 'FEL POSTTYP ' TO FELTEXT-STR                             
034801           PERFORM S99-ABEND                                              
034901     END-EVALUATE                                                         
035001                                                                          
035101     .                                                                    
035201     EJECT                                                                
035301 BA-FLYTTA-POST SECTION.                                                  
035401                                                                          
035501     IF IN03-IDDC = '01'                                                  
035601        MOVE WC-CDC-SE              TO UT-IDDC                            
035701        MOVE WC-CDC-SE              TO UT-IDDC-RET                        
036503        MOVE WC-IDFTG-PV            TO WS-IDFTG                           
036703     ELSE                                                                 
036803        MOVE IN03-IDDISTR           TO W-IDDISTR-WDB2-MIN                 
036903                                       W-IDDISTR-WDB2-MAX                 
037003        MOVE IN03-IDKUNDNR          TO W-IDKUNDNR-WDB2-MIN                
037103        PERFORM IMS-GET-GMTA01-FOERSTA                                    
037203                                                                          
037303        IF SEGMENT-FINNS                                                  
037403           MOVE GMT-IDDC-DAY (1)   TO UT-IDDC                             
037503           MOVE GMT-IDDC-RET       TO UT-IDDC-RET                         
037603           MOVE GMT-IDFTG          TO WS-IDFTG                            
037703        ELSE                                                              
037803           MOVE '99'                TO UT-IDDC                            
037903                                       UT-IDDC-RET                        
037904           MOVE WC-IDFTG-PV         TO WS-IDFTG                           
038003        END-IF                                                            
038103     END-IF                                                               
038203                                                                          
038303     MOVE IN03-IDPTYP               TO UT-IDPTYP                          
038403     MOVE IN03-IDDISTR              TO UT-IDDISTR                         
038503     MOVE IN03-IDKUNDNR             TO UT-IDKUNDNR                        
038603     MOVE IN03-IDRAPPNR             TO UT-IDRAPPNR                        
038703     MOVE IN03-IDARTNR              TO UT-IDARTNR                         
038803     MOVE IN03-IDRADNR              TO UT-IDRADNR                         
038903     MOVE ZERO                      TO UT-IDLOPNRM                        
039003     MOVE IN03-IDDC                 TO WS-IDDC                            
039103     IF IN03-KDANMORS = '72'   OR '52'   OR '53' OR                       
039203        GOOD-DDC               OR                                         
039303       ((IN03-KDANMORS = '70'  OR                                         
039403                         '92'  OR '20'   OR '21') AND                     
039503        (WS-IDFTG      = '57'  AND                                        
039603         IN03-IDDC NOT = '61'  AND '6A' AND '62'))                        
039703        MOVE JA                     TO UT-FLAUTKRE                        
039803     ELSE                                                                 
039903        MOVE NEJ                    TO UT-FLAUTKRE                        
040003     END-IF                                                               
040103     MOVE IN03-FLDIRLEV             TO UT-FLDIRLEV                        
040203     MOVE IN03-IDFAKT               TO UT-IDFAKT                          
040303     MOVE +0                        TO UT-IDFAKT-LOC                      
040403*    MOVE 57                        TO UT-IDFTG                           
040503     MOVE WC-IDFTG-PV               TO UT-IDFTG                           
040603     MOVE IN03-IDKOLLI              TO UT-IDKOLLI                         
040703     MOVE IN03-IDORDNR              TO UT-IDORDNR                         
040803     MOVE IN03-KDANMORS             TO UT-KDANMORS                        
040903     MOVE IN03-KDEMBLEV             TO UT-KDEMBLEV                        
041003     MOVE IN03-KDFAKTYP             TO UT-KDFAKTYP                        
041103     MOVE IN03-KDFRAKT              TO UT-KDFRAKT                         
041203     MOVE IN03-KVLEVANM             TO UT-KVLEVANM                        
041303     MOVE IN03-PRARTBTO             TO UT-PRARTBTO                        
041403     MOVE IN03-PRARTBTO-LOC         TO UT-PRARTBTO-LOC                    
041503     MOVE +0                        TO UT-PRARTBTO-LOCINV                 
041603     MOVE +0                        TO UT-PRFRAKT                         
041703     MOVE IN03-TIFAKT               TO UT-TIFAKT                          
041803     MOVE +0                        TO UT-TIFAKT-LOC                      
041903     MOVE IN03-TILEVANM             TO UT-TILEVANM                        
042003     MOVE IN03-TEANMNOT-REG (1)     TO UT-TEANMNOT-REG (1)                
042103     MOVE IN03-TEANMNOT-REG (2)     TO UT-TEANMNOT-REG (2)                
042203     MOVE IN03-TEANMNOT-REG (3)     TO UT-TEANMNOT-REG (3)                
042303     MOVE SPACE                     TO UT-KDVAT                           
042403     MOVE IN03-KDVALISO             TO UT-KDVALISO                        
042503     MOVE SPACE                     TO UT-BEART-VIPS                      
042603     MOVE +0                        TO UT-PRARTSTD                        
042703     MOVE +0                        TO UT-PRARTSJK                        
042803                                                                          
042903     PERFORM S11-SKRIV-W41807                                             
043003     .                                                                    
043103     EJECT                                                                
043203 BC-FLYTTA-POST SECTION.                                                  
043303                                                                          
043403     MOVE IN09B-IDDISTR             TO W-IDDISTR                          
043503                                       TEST-IDDISTR-LDC                   
043603                                       TEST-IDDISTR                       
043703     MOVE IN09B-IDKUNDNR            TO W-IDKUNDNR                         
043803                                       TEST-IDKUNDNR-LDC                  
043903                                                                          
044003     PERFORM IMS-GET-GMTA01-UNIK                                          
044103                                                                          
044203     IF SEGMENT-FINNS                                                     
044303       MOVE IN09B-KDANMORS  TO OKOD-KDANMORS                              
044403       CALL W418OKOD USING OKOD-W418OKOD                                  
044503                                                                          
044603       IF (GMT-FLLDCKND = JA OR GMT-FLRETUR = JA) AND                     
044703          OKOD-FL-RETILL = JA                                             
044803                                                                          
044903         IF GMT-IDDC-RET72(1) = SPACE                                     
045003           MOVE GMT-IDDC-RET        TO UT-IDDC-RET                        
045103         ELSE                                                             
045203           IF GMT-IDDC-RET72(1) = GMT-IDDC-RET                            
045303             MOVE GMT-IDDC-RET      TO UT-IDDC-RET                        
045403           ELSE                                                           
045503             MOVE IN09B-KDANMORS    TO WS-KDANMORS                        
045603             MOVE IN09B-IDARTNR     TO WS-IDARTNR                         
045703             MOVE IN09B-IDDC        TO WS-IDDC-LEV                        
045803             PERFORM S02-KOLLA-RETUR-DC                                   
045903           END-IF                                                         
046003         END-IF                                                           
046103       ELSE                                                               
046203          MOVE GMT-IDDC-RET         TO UT-IDDC-RET                        
046303       END-IF                                                             
046403     ELSE                                                                 
046503        MOVE '99'                   TO UT-IDDC-RET                        
046603     END-IF                                                               
046703                                                                          
046803     MOVE IN09B-IDPTYP              TO UT-IDPTYP                          
046903     MOVE IN09B-IDDISTR             TO UT-IDDISTR                         
047003     MOVE IN09B-IDKUNDNR            TO UT-IDKUNDNR                        
047103     MOVE IN09B-IDRAPPNR            TO UT-IDRAPPNR                        
047203     MOVE IN09B-IDARTNR             TO UT-IDARTNR                         
047303     MOVE IN09B-IDRADNR             TO UT-IDRADNR                         
047403     MOVE ZERO                      TO UT-IDLOPNRM                        
047503     MOVE IN09B-IDDC                TO WS-IDDC                            
047603     IF IN09B-KDANMORS = '72'   OR '52'  OR '53' OR                       
047703        GOOD-DDC                OR                                        
047803       ((IN09B-KDANMORS = '70'  OR                                        
047903                          '92'  OR '20' OR '21') AND                      
048003        (IN09B-IDFTG    = '57'  AND                                       
048103        (IN09B-IDDC NOT = '61'  AND '6A' AND '62')))                      
048203        MOVE JA                     TO UT-FLAUTKRE                        
048303     ELSE                                                                 
048403        MOVE NEJ                    TO UT-FLAUTKRE                        
048503     END-IF                                                               
048603     MOVE IN09B-FLDIRLEV            TO UT-FLDIRLEV                        
048703     MOVE IN09B-IDFAKT              TO UT-IDFAKT                          
048803     MOVE IN09B-IDFAKT-LOC          TO UT-IDFAKT-LOC                      
048903     MOVE IN09B-IDFTG               TO UT-IDFTG                           
049003     MOVE IN09B-IDKOLLI             TO UT-IDKOLLI                         
049103     MOVE IN09B-IDORDNR7            TO WS-IDORDNR                         
049203     MOVE WS-IDORDNR5               TO UT-IDORDNR                         
049303     MOVE IN09B-IDDC                TO UT-IDDC                            
049403     MOVE IN09B-KDANMORS            TO UT-KDANMORS                        
049503     MOVE IN09B-KDEMBLEV            TO UT-KDEMBLEV                        
049603     MOVE IN09B-KDFAKTYP            TO UT-KDFAKTYP                        
049703     MOVE IN09B-KDFRAKT             TO UT-KDFRAKT                         
049803     MOVE IN09B-KVLEVANM            TO UT-KVLEVANM                        
049903                                                                          
050003*VIPS HAR BARA 2 ST PRISFÄLT/ PRARTBTO=SB-PRIS (PULSPRIS) SOM             
050103*ÄR I SEK FÖR "GAMLA" TRANSAR OCH LOKAL VALUTA/ SEK FÖR NYA "01C"         
050203*OCH PRARTBTO-LOC = ÅF-PRIS (LOKAL VALUTA)                                
050303     MOVE IN09B-PRARTBTO            TO UT-PRARTBTO                        
050403     MOVE +0                        TO UT-PRARTBTO-LOC                    
050503     MOVE IN09B-PRARTBTO-LOC        TO UT-PRARTBTO-LOCINV                 
050603                                                                          
050703     MOVE IN09B-PRFRAKT             TO UT-PRFRAKT                         
050803     MOVE IN09B-TIFAKT              TO UT-TIFAKT                          
050903     MOVE IN09B-TIFAKT-LOC          TO UT-TIFAKT-LOC                      
051003     MOVE IN09B-TILEVANM            TO UT-TILEVANM                        
051103     MOVE IN09B-TEANMNOT-REG (1)    TO UT-TEANMNOT-REG (1)                
051203     MOVE IN09B-TEANMNOT-REG (2)    TO UT-TEANMNOT-REG (2)                
051303     MOVE IN09B-TEANMNOT-REG (3)    TO UT-TEANMNOT-REG (3)                
051403     MOVE SPACE                     TO UT-KDVAT                           
051503     MOVE SPACE                     TO UT-KDVALISO                        
051603     MOVE SPACE                     TO UT-BEART-VIPS                      
051703     MOVE +0                        TO UT-PRARTSTD                        
051803     MOVE +0                        TO UT-PRARTSJK                        
051903                                                                          
052003     PERFORM S11-SKRIV-W41807                                             
052103     .                                                                    
052203     EJECT                                                                
052303 BD-FLYTTA-POST SECTION.                                                  
052403                                                                          
052503     MOVE INREF-IDDISTR             TO W-IDDISTR                          
052603                                       TEST-IDDISTR                       
052703     MOVE INREF-IDKUNDNR            TO W-IDKUNDNR                         
052803     PERFORM IMS-GET-GMTA01-UNIK                                          
052903                                                                          
053003     IF SEGMENT-FINNS                                                     
053103        MOVE GMT-IDDC-RET           TO UT-IDDC-RET                        
053203     ELSE                                                                 
053303        MOVE '99'                   TO UT-IDDC-RET                        
053403     END-IF                                                               
053503                                                                          
053603     MOVE INREF-IDPTYP              TO UT-IDPTYP                          
053703     MOVE INREF-IDDISTR             TO UT-IDDISTR                         
053803     MOVE INREF-IDKUNDNR            TO UT-IDKUNDNR                        
053903     MOVE INREF-IDRAPPNR            TO UT-IDRAPPNR                        
054003     MOVE INREF-IDARTNR             TO UT-IDARTNR                         
054103     MOVE INREF-IDRADNR             TO UT-IDRADNR                         
054203     MOVE INREF-IDLOPNRM            TO UT-IDLOPNRM                        
054303     MOVE INREF-FLAUTKRE            TO UT-FLAUTKRE                        
054403     MOVE INREF-FLDIRLEV            TO UT-FLDIRLEV                        
054503     MOVE INREF-IDFAKT              TO UT-IDFAKT                          
054603     MOVE +0                        TO UT-IDFAKT-LOC                      
054703     MOVE INREF-IDFTG               TO UT-IDFTG                           
054803     MOVE INREF-IDKOLLI             TO UT-IDKOLLI                         
054903     MOVE INREF-IDORDNR5            TO UT-IDORDNR                         
055003     MOVE INREF-IDDC                TO UT-IDDC                            
055103     MOVE INREF-KDANMORS            TO UT-KDANMORS                        
055203     MOVE INREF-KDEMBLEV            TO UT-KDEMBLEV                        
055303     MOVE INREF-KDFAKTYP            TO UT-KDFAKTYP                        
055403     MOVE INREF-KDFRAKT             TO UT-KDFRAKT                         
055503     MOVE INREF-KVLEVANM            TO UT-KVLEVANM                        
055603                                                                          
055703     IF DIST79-DEALER-PRICE OR                                            
055705        DIST79-ECOM-PRICE                                                 
055803       MOVE INREF-PRARTBTO-LOC      TO UT-PRARTBTO-LOC                    
055903       MOVE +0                      TO UT-PRARTBTO                        
056003       MOVE +0                      TO UT-PRARTBTO-LOCINV                 
056103       MOVE INREF-KDVALISO          TO UT-KDVALISO                        
056203     ELSE                                                                 
056303       MOVE INREF-PRARTBTO          TO UT-PRARTBTO                        
056403       MOVE +0                      TO UT-PRARTBTO-LOC                    
056503       MOVE +0                      TO UT-PRARTBTO-LOCINV                 
056603       MOVE SPACE                   TO UT-KDVALISO                        
056703     END-IF                                                               
056803                                                                          
056903     MOVE +0                        TO UT-PRFRAKT                         
057003     MOVE INREF-TIFAKT              TO UT-TIFAKT                          
057103     MOVE +0                        TO UT-TIFAKT-LOC                      
057203     MOVE INREF-TILEVANM            TO UT-TILEVANM                        
057303     MOVE SPACE                     TO UT-TEANMNOT-REG (1)                
057403     MOVE SPACE                     TO UT-TEANMNOT-REG (2)                
057503     MOVE SPACE                     TO UT-TEANMNOT-REG (3)                
057603     MOVE SPACE                     TO UT-KDVAT                           
057703     MOVE SPACE                     TO UT-BEART-VIPS                      
057803     MOVE +0                        TO UT-PRARTSTD                        
057903     MOVE +0                        TO UT-PRARTSJK                        
058003                                                                          
058103     PERFORM S11-SKRIV-W41807                                             
058203     .                                                                    
058303     EJECT                                                                
058403 BE-FLYTTA-POST SECTION.                                                  
058503                                                                          
058603     MOVE IN09C-IDDISTR             TO W-IDDISTR                          
058703                                       TEST-IDDISTR-LDC                   
058803                                       TEST-IDDISTR                       
058903     MOVE IN09C-IDKUNDNR            TO W-IDKUNDNR                         
059003                                       TEST-IDKUNDNR-LDC                  
059103                                                                          
059203     PERFORM IMS-GET-GMTA01-UNIK                                          
059303                                                                          
059403     IF SEGMENT-FINNS                                                     
059503       MOVE IN09C-KDANMORS  TO OKOD-KDANMORS                              
059603       CALL W418OKOD USING OKOD-W418OKOD                                  
059703                                                                          
059803       IF (GMT-FLLDCKND = JA OR GMT-FLRETUR = JA) AND                     
059903          OKOD-FL-RETILL = JA                                             
060003                                                                          
060103         IF GMT-IDDC-RET72(1) = SPACE                                     
060203           MOVE GMT-IDDC-RET        TO UT-IDDC-RET                        
060303         ELSE                                                             
060403           IF GMT-IDDC-RET72(1) = GMT-IDDC-RET                            
060503             MOVE GMT-IDDC-RET      TO UT-IDDC-RET                        
060603           ELSE                                                           
060703             MOVE IN09C-KDANMORS    TO WS-KDANMORS                        
060803             MOVE IN09C-IDARTNR     TO WS-IDARTNR                         
060903             MOVE IN09C-IDDC        TO WS-IDDC-LEV                        
061003             PERFORM S02-KOLLA-RETUR-DC                                   
061103           END-IF                                                         
061203         END-IF                                                           
061303       ELSE                                                               
061403          MOVE GMT-IDDC-RET         TO UT-IDDC-RET                        
061503       END-IF                                                             
061603     ELSE                                                                 
061703        MOVE '99'                   TO UT-IDDC-RET                        
061803     END-IF                                                               
061903                                                                          
062003     MOVE IN09C-IDPTYP              TO UT-IDPTYP                          
062103     MOVE IN09C-IDDISTR             TO UT-IDDISTR                         
062203     MOVE IN09C-IDKUNDNR            TO UT-IDKUNDNR                        
062303     MOVE IN09C-IDRAPPNR            TO UT-IDRAPPNR                        
062403     MOVE IN09C-IDARTNR             TO UT-IDARTNR                         
062503     MOVE IN09C-IDRADNR             TO UT-IDRADNR                         
062603     MOVE ZERO                      TO UT-IDLOPNRM                        
062703     MOVE IN09C-IDDC                TO WS-IDDC                            
062803     IF IN09C-KDANMORS = '72'   OR  '52'  OR '53' OR                      
062903        GOOD-DDC                OR                                        
063003       ((IN09C-KDANMORS = '70'  OR                                        
063103                          '92'  OR '20' OR '21') AND                      
063203        (IN09C-IDFTG    = '57'  AND                                       
063303        (IN09C-IDDC NOT = '61'  AND '6A' AND '62')))                      
063403        MOVE JA                     TO UT-FLAUTKRE                        
063503     ELSE                                                                 
063603        MOVE NEJ                    TO UT-FLAUTKRE                        
063703     END-IF                                                               
063803     MOVE IN09C-FLDIRLEV            TO UT-FLDIRLEV                        
063903     MOVE IN09C-IDFTG               TO UT-IDFTG                           
064003     MOVE IN09C-IDKOLLI             TO UT-IDKOLLI                         
064103     MOVE IN09C-IDORDNR7            TO WS-IDORDNR                         
064203     MOVE WS-IDORDNR5               TO UT-IDORDNR                         
064303     MOVE IN09C-IDDC                TO UT-IDDC                            
064403     MOVE IN09C-KDANMORS            TO UT-KDANMORS                        
064503     MOVE IN09C-KDEMBLEV            TO UT-KDEMBLEV                        
064603     MOVE IN09C-KDFAKTYP            TO UT-KDFAKTYP                        
064703     MOVE IN09C-KDFRAKT             TO UT-KDFRAKT                         
064803     MOVE IN09C-KVLEVANM            TO UT-KVLEVANM                        
064903                                                                          
065003     IF DIST79-DEALER-PRICE OR                                            
065005        DIST79-ECOM-PRICE                                                 
065103       MOVE +0                        TO UT-PRARTBTO                      
065203       MOVE IN09C-PRARTBTO-LOC        TO UT-PRARTBTO-LOC                  
065303                                         UT-PRARTBTO-LOCINV               
065403       MOVE +0                        TO UT-PRFRAKT                       
065503     ELSE                                                                 
065603       MOVE IN09C-PRARTBTO            TO UT-PRARTBTO                      
065703       MOVE +0                        TO UT-PRARTBTO-LOC                  
065803       MOVE IN09C-PRARTBTO-LOC        TO UT-PRARTBTO-LOCINV               
065903       MOVE IN09C-PRFRAKT             TO UT-PRFRAKT                       
066003     END-IF                                                               
066103                                                                          
066203     MOVE IN09C-IDFAKT                TO UT-IDFAKT                        
066303     MOVE IN09C-IDFAKT-LOC            TO UT-IDFAKT-LOC                    
066403     MOVE IN09C-TIFAKT                TO UT-TIFAKT                        
066503     MOVE IN09C-TIFAKT-LOC            TO UT-TIFAKT-LOC                    
066603                                                                          
066703     MOVE IN09C-TILEVANM            TO UT-TILEVANM                        
066803     MOVE IN09C-TEANMNOT-REG (1)    TO UT-TEANMNOT-REG (1)                
066903     MOVE IN09C-TEANMNOT-REG (2)    TO UT-TEANMNOT-REG (2)                
067003     MOVE IN09C-TEANMNOT-REG (3)    TO UT-TEANMNOT-REG (3)                
067103     MOVE IN09C-KDVAT               TO UT-KDVAT                           
067203     MOVE IN09C-KDVALISO            TO UT-KDVALISO                        
067303     MOVE IN09C-BEART-VIPS          TO UT-BEART-VIPS                      
067403     MOVE IN09C-PRARTSTD            TO UT-PRARTSTD                        
067503     MOVE IN09C-PRARTSJK            TO UT-PRARTSJK                        
067603                                                                          
067703     PERFORM S11-SKRIV-W41807                                             
067803     .                                                                    
067903     EJECT                                                                
068003 Z-FINIT SECTION.                                                         
068103     CLOSE INFIL                                                          
068203           W41807                                                         
068303     SKIP2                                                                
068403     MOVE 'S' TO POSTSUM-OPKOD                                            
068503     CALL POSTSUM USING POSTSUM-PARM                                      
068603     .                                                                    
068703     EJECT                                                                
068803 S01-LAES-INFIL   SECTION.                                                
068903     READ INFIL INTO IN-AREA                                              
069003     AT END                                                               
069103        SET END-OF-INFIL TO TRUE                                          
069203                                                                          
069303     NOT AT END                                                           
069403        MOVE 'INFIL' TO POSTSUM-FDNAMN                                    
069503        MOVE 'W41807D1' TO POSTSUM-DDNAMN2                                
069603        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
069703        CALL POSTSUM USING POSTSUM-PARM                                   
069803     END-READ                                                             
069903     .                                                                    
070003     EJECT                                                                
070103 S02-KOLLA-RETUR-DC  SECTION.                                             
070203                                                                          
070303     MOVE NEJ                       TO  GODK-KOD-SW                       
070403     MOVE JA                        TO  GODK-ARTIKEL-SW                   
070503                                        GODK-IDFKNGRP-SW                  
070603                                        GODK-DC-ARTIKEL-SW                
070703                                        GODK-DC-LEV-SW                    
070803                                                                          
070903     MOVE GMT-IDDC-RET72(1)   TO W-IDDC-B6                                
071003                                 W-IDDC-K7                                
071103     PERFORM IMS-GU-WDB601                                                
071203     IF SEGMENT-SAKNAS                                                    
071303       MOVE GMT-IDDC-RET      TO UT-IDDC-RET                              
071403     ELSE                                                                 
071503       IF DCS-FLARTDC = JA                                                
071603*- KOLLA OM ARTIKELN FINNS PÅ DC'T. KRAV FÖR ATT TA EMOT RETUR.           
071703         MOVE WS-IDARTNR      TO W-IDARTNR                                
071803         PERFORM IMS-GU-WDK711                                            
071903         IF SEGMENT-SAKNAS                                                
072003           MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                               
072103         END-IF                                                           
072203       END-IF                                                             
072303                                                                          
072403       IF EJ-GODK-DC-ARTIKEL                                              
072503         CONTINUE                                                         
072603       ELSE                                                               
072703         MOVE SPACE           TO W-URV-TEELMT                             
072803         MOVE WC-IDARTNR      TO W-URV-TEELMT                             
072903         MOVE SPACE           TO W-URV-FILLER                             
073003         MOVE WS-IDARTNR      TO W-URV-IDARTNR-EXCP                       
073103                                                                          
073203         PERFORM IMS-GNP-WDB611-FIRST                                     
073303         IF SEGMENT-FINNS                                                 
073403           MOVE NEJ           TO GODK-ARTIKEL-SW                          
073503         ELSE                                                             
073603           MOVE WS-IDARTNR    TO W-IDARTNR-K6                             
073703           PERFORM IMS-GU-WDK601                                          
073803           IF SEGMENT-SAKNAS                                              
073903             MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                             
074003           ELSE                                                           
074103             MOVE SPACE           TO W-URV-TEELMT                         
074203             MOVE WC-IDFKNGRP     TO W-URV-TEELMT                         
074303             MOVE SPACE           TO W-URV-FILLER                         
074403             MOVE ART-IDFKNGRP    TO W-URV-IDFKNGRP-EXCP                  
074503                                                                          
074603             PERFORM IMS-GNP-WDB611-FIRST                                 
074703             IF SEGMENT-FINNS                                             
074803               MOVE NEJ           TO GODK-IDFKNGRP-SW                     
074903             ELSE                                                         
075003               MOVE SPACE           TO W-URV-TEELMT                       
075103               MOVE WC-IDDC-EXCP    TO W-URV-TEELMT                       
075203               MOVE SPACE           TO W-URV-FILLER                       
075303               MOVE WS-IDDC-LEV     TO W-URV-IDDC-EXCP                    
075403                                                                          
075503               PERFORM IMS-GNP-WDB611-FIRST                               
075603               IF SEGMENT-FINNS                                           
075703                 MOVE NEJ           TO GODK-DC-LEV-SW                     
075803               ELSE                                                       
075903                 MOVE SPACE           TO W-URV-TEELMT                     
076003                 MOVE WC-KDANMORS     TO W-URV-TEELMT                     
076103                 MOVE SPACE           TO W-URV-FILLER                     
076203                 MOVE WS-KDANMORS     TO W-URV-KDANMORS-RET               
076303                                                                          
076403                 PERFORM IMS-GNP-WDB611-FIRST                             
076503                 IF SEGMENT-FINNS                                         
076603                   MOVE JA            TO GODK-KOD-SW                      
076703                 END-IF                                                   
076803               END-IF                                                     
076903             END-IF                                                       
077003           END-IF                                                         
077103         END-IF                                                           
077203       END-IF                                                             
077303                                                                          
077403       IF EJ-GODK-DC-ARTIKEL OR                                           
077503          EJ-GODK-ARTIKEL OR                                              
077603          EJ-GODK-IDFKNGRP OR                                             
077703          EJ-GODK-DC-LEV OR                                               
077803          EJ-GODK-KOD                                                     
077903                                                                          
078003          IF GMT-IDDC-RET72(2) = SPACE                                    
078103            MOVE GMT-IDDC-RET72(3)  TO UT-IDDC-RET                        
078203            IF UT-IDDC-RET = SPACE                                        
078303              MOVE GMT-IDDC-RET     TO UT-IDDC-RET                        
078403            END-IF                                                        
078503          ELSE                                                            
078603            PERFORM S03-KOLLA-RETUR-DC-2                                  
078703          END-IF                                                          
078803       ELSE                                                               
078903         MOVE GMT-IDDC-RET72(1)     TO UT-IDDC-RET                        
079003       END-IF                                                             
079103     END-IF                                                               
079203     .                                                                    
079303     EJECT                                                                
079403 S03-KOLLA-RETUR-DC-2  SECTION.                                           
079503                                                                          
079603     MOVE NEJ                       TO  GODK-KOD-SW                       
079703     MOVE JA                        TO  GODK-ARTIKEL-SW                   
079803                                        GODK-IDFKNGRP-SW                  
079903                                        GODK-DC-ARTIKEL-SW                
080003                                        GODK-DC-LEV-SW                    
080103                                                                          
080203     MOVE GMT-IDDC-RET72(2)   TO W-IDDC-B6                                
080303                                 W-IDDC-K7                                
080403     PERFORM IMS-GU-WDB601                                                
080503     IF SEGMENT-SAKNAS                                                    
080603       MOVE GMT-IDDC-RET      TO UT-IDDC-RET                              
080703     ELSE                                                                 
080803       IF DCS-FLARTDC = JA                                                
080903*- KOLLA OM ARTIKELN FINNS PÅ DC'T. KRAV FÖR ATT TA EMOT RETUR.           
081003         MOVE WS-IDARTNR      TO W-IDARTNR                                
081103         PERFORM IMS-GU-WDK711                                            
081203         IF SEGMENT-SAKNAS                                                
081303           MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                               
081403         END-IF                                                           
081503       END-IF                                                             
081603                                                                          
081703       IF EJ-GODK-DC-ARTIKEL                                              
081803         CONTINUE                                                         
081903       ELSE                                                               
082003         MOVE SPACE           TO W-URV-TEELMT                             
082103         MOVE WC-IDARTNR      TO W-URV-TEELMT                             
082203         MOVE SPACE           TO W-URV-FILLER                             
082303         MOVE WS-IDARTNR      TO W-URV-IDARTNR-EXCP                       
082403                                                                          
082503         PERFORM IMS-GNP-WDB611-FIRST                                     
082603         IF SEGMENT-FINNS                                                 
082703           MOVE NEJ           TO GODK-ARTIKEL-SW                          
082803         ELSE                                                             
082903           MOVE WS-IDARTNR    TO W-IDARTNR-K6                             
083003           PERFORM IMS-GU-WDK601                                          
083103           IF SEGMENT-SAKNAS                                              
083203             MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                             
083303           ELSE                                                           
083403             MOVE SPACE           TO W-URV-TEELMT                         
083503             MOVE WC-IDFKNGRP     TO W-URV-TEELMT                         
083603             MOVE SPACE           TO W-URV-FILLER                         
083703             MOVE ART-IDFKNGRP    TO W-URV-IDFKNGRP-EXCP                  
083803                                                                          
083903             PERFORM IMS-GNP-WDB611-FIRST                                 
084003             IF SEGMENT-FINNS                                             
084103               MOVE NEJ           TO GODK-IDFKNGRP-SW                     
084203             ELSE                                                         
084303               MOVE SPACE            TO W-URV-TEELMT                      
084403               MOVE WC-IDDC-EXCP     TO W-URV-TEELMT                      
084503               MOVE SPACE            TO W-URV-FILLER                      
084603               MOVE WS-IDDC-LEV      TO W-URV-IDDC-EXCP                   
084703                                                                          
084803               PERFORM IMS-GNP-WDB611-FIRST                               
084903               IF SEGMENT-FINNS                                           
085003                 MOVE NEJ            TO GODK-DC-LEV-SW                    
085103               ELSE                                                       
085203                 MOVE SPACE           TO W-URV-TEELMT                     
085303                 MOVE WC-KDANMORS     TO W-URV-TEELMT                     
085403                 MOVE SPACE           TO W-URV-FILLER                     
085503                 MOVE WS-KDANMORS     TO W-URV-KDANMORS-RET               
085603                                                                          
085703                 PERFORM IMS-GNP-WDB611-FIRST                             
085803                 IF SEGMENT-FINNS                                         
085903                   MOVE JA            TO GODK-KOD-SW                      
086003                 END-IF                                                   
086103               END-IF                                                     
086203             END-IF                                                       
086303           END-IF                                                         
086403         END-IF                                                           
086503       END-IF                                                             
086603                                                                          
086703       IF EJ-GODK-DC-ARTIKEL OR                                           
086803          EJ-GODK-ARTIKEL OR                                              
086903          EJ-GODK-IDFKNGRP OR                                             
087003          EJ-GODK-DC-LEV OR                                               
087103          EJ-GODK-KOD                                                     
087203                                                                          
087303         MOVE GMT-IDDC-RET72(3)   TO UT-IDDC-RET                          
087403         IF UT-IDDC-RET = SPACE                                           
087503           MOVE GMT-IDDC-RET      TO UT-IDDC-RET                          
087603         END-IF                                                           
087703       ELSE                                                               
087803         MOVE GMT-IDDC-RET72(2)   TO UT-IDDC-RET                          
087903       END-IF                                                             
088003     END-IF                                                               
088103     .                                                                    
088203     EJECT                                                                
089001                                                                          
089101 S11-SKRIV-W41807 SECTION.                                                
089201                                                                          
089301     WRITE W41807-POST  FROM UT-W41807-CTX                                
089401                                                                          
089501     MOVE UT-IDPTYP  TO POSTSUM-TRANSTYP                                  
089601     MOVE 'W41807'   TO POSTSUM-FDNAMN                                    
089701     MOVE 'W41807D2' TO POSTSUM-DDNAMN2                                   
089801     CALL POSTSUM    USING POSTSUM-PARM                                   
089901     .                                                                    
090001     EJECT                                                                
090101 S99-ABEND SECTION.                                                       
090201                                                                          
090301     SKIP2                                                                
090401     MOVE 'S' TO POSTSUM-OPKOD                                            
090501     CALL POSTSUM USING POSTSUM-PARM                                      
090601     CALL ABEND USING RKOD-ABEND-MED-DUMP                                 
090701     .                                                                    
090801     EJECT                                                                
090901* --- IMS SEKTIONER ---                                                   
091001 IMS-GET-GMTA01-UNIK SECTION.                                             
091101                                                                          
091201     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
091301            DELIMITED BY SIZE INTO SSA1                                   
091401                                                                          
091501     MOVE '  GE' TO GODK-STATUSKODER                                      
091601     CALL CBLTDLI USING                                                   
091701           GU GMTA-PCB DLI-IO-WDB201 SSA1                                 
091801     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
091901     PERFORM IMS-STATUSKONTROLL                                           
092001     .                                                                    
092101     EJECT                                                                
092201 IMS-GET-GMTA01-FOERSTA         SECTION.                                  
092301                                                                          
092401     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
092501                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
092601            DELIMITED BY SIZE INTO SSA1                                   
092701                                                                          
092801     MOVE '  GE' TO GODK-STATUSKODER                                      
092901     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WDB201 SSA1                    
093001     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
093101     PERFORM IMS-STATUSKONTROLL                                           
093201     .                                                                    
093301     EJECT                                                                
093401 IMS-GU-WDB601    SECTION.                                                
093501                                                                          
093601     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
093701          DELIMITED BY SIZE INTO SSA1                                     
093801     MOVE '  GE' TO GODK-STATUSKODER                                      
093901     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
094001     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
095001     PERFORM IMS-STATUSKONTROLL                                           
095101     .                                                                    
095201     EJECT                                                                
095301 IMS-GNP-WDB611-FIRST    SECTION.                                         
095401                                                                          
095501     STRING 'WDB611  *F(WDB611KY =' W-WDB611KY-X ')'                      
095601          DELIMITED BY SIZE INTO SSA1                                     
095701     MOVE '  GE' TO GODK-STATUSKODER                                      
095801     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB611 SSA1                   
095901     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
096001     PERFORM IMS-STATUSKONTROLL                                           
096101     .                                                                    
096201     EJECT                                                                
096301 IMS-GU-WDK711                 SECTION.                                   
096401                                                                          
096501     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
096601     DELIMITED BY SIZE INTO SSA1                                          
096701     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
096801     DELIMITED BY SIZE INTO SSA2                                          
096901     MOVE '  GE' TO GODK-STATUSKODER                                      
097001     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
097101     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
097201     PERFORM IMS-STATUSKONTROLL                                           
097301     .                                                                    
097401     EJECT                                                                
097501 IMS-GU-WDK601    SECTION.                                                
097601                                                                          
097701     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
097801          DELIMITED BY SIZE INTO SSA1                                     
097901     MOVE '  GE'           TO GODK-STATUSKODER                            
098001     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
098101     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
098201     PERFORM IMS-STATUSKONTROLL                                           
098301     .                                                                    
098401     EJECT                                                                
098501 IMS-STATUSKONTROLL SECTION.                                              
098601                                                                          
098701     SET STATUS-IX TO 1                                                   
098801     SEARCH GODK-STATUS                                                   
098901       AT END                                                             
099001         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT-STR                     
099101         DISPLAY FELTEXT                                                  
099201         CALL FELLOG                                                      
099301       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
099401         CONTINUE                                                         
099501     END-SEARCH                                                           
099601     .                                                                    
