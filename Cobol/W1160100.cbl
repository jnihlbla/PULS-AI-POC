000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1160100.                                                
000300 AUTHOR.         ANDERSSON BERT.                                          
000400 DATE-WRITTEN.   09/12/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        GENERATE A FILE WITH PARTS FOR BUY BACKS NOT ALLOWED.            
001000*                                                                         
001100*        FOR EACH RECORD ON INFILE W011.W01172 (DAILY PARTS FILE,         
001200*        LAGERBANDET) WRITE TO OUTFILE W11604.                            
001210*        0 = EJ RETURNERBAR TILL CDC OCH 1 = RETURNERBAR TILL CDC.        
001220*                                                                         
001300*        INFILES W11607, W01172 AND W23321.                               
001400*        OUTFILE W01164                                                   
001500*                                                                         
001600*    ABENDCODES:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- W11607 BUY BACKS NOT ALLOWED                               
002900     SELECT W11607                     ASSIGN TO W11601D1.                
003000     SKIP2                                                                
003100*          --- W23321 - AVROPSFIL                                         
003200     SELECT W23321                     ASSIGN TO W11601D2.                
003300     SKIP2                                                                
003400*          --- W01172 - LAGERBANDET                                       
003500     SELECT W01172                     ASSIGN TO W11601D3.                
003600     SKIP2                                                                
003700*          --- W11604 - OUTFILE                                           
003800     SELECT W11604                     ASSIGN TO W11601D4.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W01172                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  -COPY W011100      -L.                                               
004900     SKIP3                                                                
005000 FD  W23321                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  -COPY W233AVR      -L.                                               
005500     SKIP3                                                                
005600 FD  W11607                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  -COPY W11607      -L.                                                
006100     SKIP3                                                                
006200 FD  W11604                                                               
006300     RECORDING       F                                                    
006400     BLOCK CONTAINS  0.                                                   
006500                                                                          
006600*01  RECORD -COPY W11604 -PRE  OUT-  -L.                                  
006700     EJECT                                                                
006800 WORKING-STORAGE SECTION.                                                 
006900                                                                          
007000 77  IDPGM                       PIC X(8)    VALUE 'W1160100'.            
007100                                                                          
007200 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
007300 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
007400                                                                          
007500 77  FILLER                      PIC X(08)   VALUE 'CURRENT:'.            
007600 77  WS-CURRENT-SECTION          PIC X(32)   VALUE SPACE.                 
007700                                                                          
007800 77  JA                          PIC X       VALUE 'J'.                   
007900 77  NEJ                         PIC X       VALUE 'N'.                   
008350                                                                          
008400 77  FILLER                      PIC X(08)   VALUE 'TAB-IX::'.            
008500 77  IDLEVNR-TAB-MAX             PIC S9(9)   VALUE 50    COMP-3.          
008600 77  IDFKNGRP-TAB-MAX            PIC S9(9)   VALUE 50    COMP-3.          
008700 77  KDFARLIG-TAB-MAX            PIC S9(9)   VALUE 50    COMP-3.          
008800 77  KDPRODSL-TAB-MAX            PIC S9(9)   VALUE 50    COMP-3.          
008900 77  KDSORT-TAB-MAX              PIC S9(9)   VALUE 50    COMP-3.          
009000                                                                          
009300 77  FILLER                      PIC X(08)   VALUE 'SWITCH::'.            
009400 77  W01172-EOF-SW               PIC X       VALUE 'N'.                   
009500     88  END-OF-W01172                       VALUE 'J'.                   
009600                                                                          
009700 77  W11607-EOF-SW               PIC X       VALUE 'N'.                   
009800     88  END-OF-W11607                       VALUE 'J'.                   
009900                                                                          
010000 77  W23321-EOF-SW               PIC X       VALUE 'N'.                   
010100     88  END-OF-W23321                       VALUE 'J'.                   
010200                                                                          
010300 77  MATCH-IN-ANMORS-TAB-SW      PIC X       VALUE 'N'.                   
010400     88  MATCH-IN-ANMORS-TAB                 VALUE 'J'.                   
010500                                                                          
010600 77  MATCH-MOT-W23321-SW         PIC X       VALUE 'N'.                   
010700     88  MATCH-MOT-W23321                    VALUE 'J'.                   
010701                                                                          
010702 77  EJ-RETUR-TILL-CDC-SW         PIC X      VALUE 'J'.                   
010710     88  EJ-RETUR-TILL-CDC                   VALUE 'N'.                   
010800                                                                          
010900 77  MATCH-MOT-W11607-SW         PIC X       VALUE 'N'.                   
011000     88  MATCH-MOT-W11607                    VALUE 'J'.                   
011100                                                                          
011110 77  ARTIKEL-FINNS-PA-W23321-SW  PIC X       VALUE 'N'.                   
011120     88  ARTIKEL-FINNS-PA-W23321             VALUE 'J'.                   
011200                                                                          
011300 77  FILLER                      PIC X(16) VALUE 'TODAYS-DATE:'.          
011400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
011500 01  FILLER REDEFINES TODAYS-DATE.                                        
011600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
011700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
011800     03  TODAYS-DATE-DAY         PIC 9(2).                                
011900                                                                          
012000*****                                                                     
012100 01  WS-YYMMDDHHMM.                                                       
012200     03 WS-YYMMDD                PIC  9(6).                               
012300     03 WS-TIME                  PIC  9(4).                               
012400                                                                          
013000 01  WS-TIAAVV-GRP               PIC  9(4).                               
013100                                                                          
013200 01  WS-TIAAVV-GRP-NUM           PIC S9(5) COMP-3 VALUE +0.               
013300                                                                          
013400 01  WS-CONSTANT-4-WEEKS         PIC S9(5) COMP-3 VALUE +4.               
013500                                                                          
013600 01  WS-HHMMSSTH                 PIC  9(8).                               
013700 01  FILLER REDEFINES WS-HHMMSSTH.                                        
013800       03  WS-HHMM               PIC 9(4).                                
013900       03  WS-SSTH               PIC 9(4).                                
014000*TABELLER-------------------------------                                  
014100*---------------------------------------                                  
014200*IDLEVNR-TAB                                                              
014300 77  FILLER                      PIC X(16) VALUE 'IDLEVNR-TAB:'.          
014400 01  IDLEVNR-TAB-IX              PIC S9(5) COMP-3 VALUE +0.               
014500 01  TAB-IDLEVN.                                                          
014600     03  TAB-IDLEVNR-50          OCCURS 50.                               
014700         05 TAB-IDLEVNR          PIC X(5).                                
014800                                                                          
014900*IDFKNGRP-TAB                                                             
015000 77  FILLER                      PIC X(16) VALUE 'IDFKNGRP-TAB:'.         
015100 01  IDFKNGRP-TAB-IX             PIC S9(5) COMP-3 VALUE +0.               
015200 01  TAB-IDFKNGR.                                                         
015300     03  TAB-IDFKNGRP-50         OCCURS 50.                               
015400         05 TAB-IDFKNGRP         PIC 9(4).                                
015500                                                                          
015600*KDPRODSL-TAB                                                             
015700 77  FILLER                      PIC X(16) VALUE 'KDPRODSL-TAB:'.         
015800 01  KDPRODSL-TAB-IX             PIC S9(5) COMP-3 VALUE +0.               
015900 01  TAB-KDPRODS.                                                         
016000     03  TAB-KDPRODSL-50         OCCURS 50.                               
016100         05 TAB-KDPRODSL         PIC 9(2).                                
016200                                                                          
016300*KDFARLIG-TAB                                                             
016400 77  FILLER                      PIC X(16) VALUE 'KDFARLIG-TAB:'.         
016500 01  KDFARLIG-TAB-IX             PIC S9(5) COMP-3 VALUE +0.               
016600 01  TAB-KDFARLI.                                                         
016700     03  TAB-IDLEVNR-50          OCCURS 50.                               
016800         05 TAB-KDFARLIG         PIC 9(1).                                
016900                                                                          
017000*KDSORT-TAB                                                               
017100 77  FILLER                      PIC X(16) VALUE 'KDSORT-TAB:'.           
017200 01  KDSORT-TAB-IX               PIC S9(5) COMP-3 VALUE +0.               
017300 01  TAB-KDSOR.                                                           
017400     03  TAB-KDSORT-50           OCCURS 50.                               
017500         05 TAB-KDSORT           PIC X(2).                                
017600                                                                          
017700*END-OF-TABELLER                                                          
017800*************************************                                     
017900 01  GENERAL-SUBPROGRAMS.                                                 
018000*                                                                         
018100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
018200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
018300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
018400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018500     SKIP2                                                                
018600*    --- PARAMETERS TO ABEND                                              
018700                                                                          
018800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
018900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
019000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
019100     SKIP2                                                                
019200*    --- PARAMETERS TO DATKORT                                            
019300*                                                                         
019400 01  PROGRAM-NAME                PIC X(6)    VALUE 'W11601'.              
019500     SKIP2                                                                
019600 01  DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.              
019700     SKIP2                                                                
019800*01  -COPY WDATKORT                                                       
019900     SKIP2                                                                
020000*    --- PARAMETRAR TILL POSTSUM                                          
020100*                                                                         
020200*01  -COPY W0005   -PRE  POSTSUM-                                         
020300     SKIP2                                                                
020400 77  FILLER                      PIC X(8)    VALUE 'WDATAREA'.            
020500*01  -COPY WDATAREA                                                       
020600     SKIP2                                                                
020700*******************                                                       
020800 01  LAGER-AREA-START      PIC X(24)   VALUE 'LAGER-AREA-START  '.        
020900     SKIP2                                                                
021000*01  AREA     -COPY W011100     -PRE LAGER-W01172-                        
021100     SKIP2                                                                
021200**********************                                                    
021300 01  IN-AREA-W11607        PIC X(24)   VALUE 'IN-AREA-W11607 '.           
021400     SKIP2                                                                
021500                                                                          
021600*01  AREA     -COPY W11607     -PRE IN-W11607-                            
021700     SKIP2                                                                
021800**********************                                                    
021900 01  IN-AREA-W23321        PIC X(24)   VALUE 'IN-AREA-W23321 '.           
022000     SKIP2                                                                
022100                                                                          
022200*01  AREA     -COPY W233AVR    -PRE IN-W23321-                            
022300     SKIP2                                                                
022400**********************                                                    
022500 01  OUT-AREA-W11604       PIC X(24)   VALUE 'OUT-AREA-START'.            
022600     SKIP2                                                                
022700*01  AREA       -COPY W11604     -PRE OUT-                                
022800     SKIP2                                                                
022900 PROCEDURE DIVISION.                                                      
023000 MAIN SECTION.                                                            
023100     SKIP2                                                                
023200                                                                          
023300     PERFORM A-INIT                                                       
023400     PERFORM S03-READ-W11607                                              
023500     PERFORM B-FLYTTA-TILL-TABELL                                         
023600                                                                          
023700     PERFORM S01-READ-W01172                                              
023710     PERFORM S02-READ-W23321                                              
023720                                                                          
023800     PERFORM UNTIL END-OF-W01172                                          
023900                                                                          
024000       PERFORM C-KONTROLL-FLYTTA-TILL-UTAREA                              
024100       PERFORM S11-WRITE-W11604                                           
024200                                                                          
024300       PERFORM S01-READ-W01172                                            
024400     END-PERFORM                                                          
024500                                                                          
024600                                                                          
024700     PERFORM Z-FINIT                                                      
024800                                                                          
024900     MOVE ZERO TO RETURN-CODE                                             
025000     GOBACK                                                               
025100     .                                                                    
025200     EJECT                                                                
025300 A-INIT SECTION.                                                          
025400                                                                          
025500     OPEN INPUT  W11607                                                   
025600                 W23321                                                   
025700                 W01172                                                   
025800                                                                          
025900     OPEN OUTPUT W11604                                                   
026000     SKIP2                                                                
026100     CALL DATKORT USING PROGRAM-NAME DATECARD-ID DATUMKORT                
026200     MOVE D-AAR     TO  TODAYS-DATE-YEAR                                  
026300     MOVE D-MAANAD  TO  TODAYS-DATE-MONTH                                 
026400     MOVE D-DAG     TO  TODAYS-DATE-DAY                                   
026500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026600*                                                                         
026700*    ACCEPT WS-YYMMDD      FROM DATE                                      
026800*    ACCEPT WS-HHMMSSTH    FROM TIME                                      
026900*    MOVE WS-HHMM          TO WS-TIME                                     
027000                                                                          
027100**   OMVANDLA DAGENS-DATUM TILL ÅR, VECKA                                 
027200     MOVE TODAYS-DATE         TO DAT-I-TIDATUM                            
027300     MOVE 'AAMMDD'            TO DAT-KDDATFORM                            
027400                                                                          
027600     MOVE NEJ                      TO W11607-EOF-SW                       
027700     MOVE NEJ                      TO W23321-EOF-SW                       
027800     MOVE NEJ                      TO W01172-EOF-SW                       
027900                                                                          
028000     CALL WDATKONV USING   DAT-KDDATFORM,                                 
028100                           DAT-I-TIDATUM,                                 
028200                           DAT-O-TIDATUM,                                 
028300                           DAT-KDSVAR                                     
028400                                                                          
028500     IF DAT-KDSVAR-OK                                                     
028600       MOVE DAT-TIAAVV-GRP      TO WS-TIAAVV-GRP                          
028700       MOVE WS-TIAAVV-GRP       TO WS-TIAAVV-GRP-NUM                      
028800     ELSE                                                                 
028900       MOVE 'FEL FRÅN WDATKONV I A-SECTION'                               
029000       TO ERROR-TEXT                                                      
029100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
029200     END-IF                                                               
029300                                                                          
029400*NOLLSTÄLL IDFKNGRP-TAB                                                   
029500     MOVE +00001                   TO IDFKNGRP-TAB-IX                     
029600     PERFORM UNTIL IDFKNGRP-TAB-IX > IDFKNGRP-TAB-MAX                     
029700         MOVE 0000            TO TAB-IDFKNGRP (IDFKNGRP-TAB-IX)           
029800         ADD +1                    TO IDFKNGRP-TAB-IX                     
029900     END-PERFORM                                                          
030000                                                                          
030100*NOLLSTÄLL KDPRODSL-TAB                                                   
030200     MOVE +00001                   TO KDPRODSL-TAB-IX                     
030300     PERFORM UNTIL KDPRODSL-TAB-IX > KDPRODSL-TAB-MAX                     
030400         MOVE 00                TO TAB-KDPRODSL(KDPRODSL-TAB-IX)          
030500         ADD +1                    TO KDPRODSL-TAB-IX                     
030600     END-PERFORM                                                          
030700                                                                          
030800*NOLLSTÄLL KDFARLIG-TAB                                                   
030900     MOVE +00001                   TO KDFARLIG-TAB-IX                     
031000     PERFORM UNTIL KDFARLIG-TAB-IX > KDFARLIG-TAB-MAX                     
031100         MOVE 0                 TO TAB-KDFARLIG(KDFARLIG-TAB-IX)          
031200         ADD +1                    TO KDFARLIG-TAB-IX                     
031300     END-PERFORM                                                          
031400                                                                          
031500*NOLLSTÄLL IDLEVNR-TAB                                                    
031600     MOVE +00001                   TO IDLEVNR-TAB-IX                      
031700     PERFORM UNTIL IDLEVNR-TAB-IX > IDLEVNR-TAB-MAX                       
031800         MOVE SPACE             TO TAB-IDLEVNR (IDLEVNR-TAB-IX)           
031900         ADD +1                    TO IDLEVNR-TAB-IX                      
032000     END-PERFORM                                                          
032100                                                                          
032200*NOLLSTÄLL KDSORT-TAB                                                     
032300     MOVE +00001                   TO KDSORT-TAB-IX                       
032400     PERFORM UNTIL KDSORT-TAB-IX > KDSORT-TAB-MAX                         
032500         MOVE SPACE                TO TAB-KDSORT  (KDSORT-TAB-IX)         
032600         ADD +1                    TO KDSORT-TAB-IX                       
032700     END-PERFORM                                                          
032800                                                                          
032900     .                                                                    
033000     EJECT                                                                
033100                                                                          
033200 B-FLYTTA-TILL-TABELL  SECTION.                                           
033300     MOVE 'B-FLYTTA-TILL-TABELL'   TO WS-CURRENT-SECTION                  
033400                                                                          
033500     MOVE +00001                   TO IDLEVNR-TAB-IX                      
033600     MOVE +00001                   TO IDFKNGRP-TAB-IX                     
033700     MOVE +00001                   TO KDPRODSL-TAB-IX                     
033800     MOVE +00001                   TO KDFARLIG-TAB-IX                     
033900     MOVE +00001                   TO KDSORT-TAB-IX                       
034000                                                                          
034210     PERFORM UNTIL END-OF-W11607                                          
034211                OR IN-W11607-IDARTNR > +000000000                         
034300                                                                          
034400       IF IN-W11607-IDARTNR = +000000000                                  
034500                                                                          
034600         PERFORM BA-FYLL-TABELL                                           
034700       END-IF                                                             
034800                                                                          
034900       PERFORM S03-READ-W11607                                            
035000     END-PERFORM                                                          
035100                                                                          
035200     .                                                                    
035300     EJECT                                                                
035400                                                                          
035500 BA-FYLL-TABELL    SECTION.                                               
035600     MOVE 'B-FLYTTA-TILL-TABELL'   TO WS-CURRENT-SECTION                  
035700                                                                          
035800     IF IDLEVNR-TAB-IX > IDLEVNR-TAB-MAX                                  
035900       MOVE 'IDLEVNR TABMAX NOT ENOUGH '   TO ERROR-TEXT                  
036000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036100     END-IF                                                               
036200     IF IN-W11607-IDLEVNR > SPACE                                         
036300       MOVE IN-W11607-IDLEVNR    TO TAB-IDLEVNR (IDLEVNR-TAB-IX)          
036500       ADD  +00001               TO IDLEVNR-TAB-IX                        
036600     END-IF                                                               
036700                                                                          
036800     IF IDFKNGRP-TAB-IX > IDFKNGRP-TAB-MAX                                
036900       MOVE 'IDFKNGRP TABMAX NOT ENOUGH '   TO ERROR-TEXT                 
037000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037100     END-IF                                                               
037200     IF IN-W11607-IDFKNGRP > ZERO                                         
037300       MOVE IN-W11607-IDFKNGRP   TO TAB-IDFKNGRP(IDFKNGRP-TAB-IX)         
037500       ADD  +00001               TO IDFKNGRP-TAB-IX                       
037600     END-IF                                                               
037700                                                                          
037800     IF KDPRODSL-TAB-IX > KDPRODSL-TAB-MAX                                
037900       MOVE 'KDPRODSL TABMAX NOT ENOUGH '   TO ERROR-TEXT                 
038000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
038100     END-IF                                                               
038200     IF IN-W11607-KDPRODSL > ZERO                                         
038300       MOVE IN-W11607-KDPRODSL   TO TAB-KDPRODSL(KDPRODSL-TAB-IX)         
038500       ADD  +00001               TO KDPRODSL-TAB-IX                       
038600     END-IF                                                               
038700                                                                          
038800     IF KDFARLIG-TAB-IX > KDFARLIG-TAB-MAX                                
038900       MOVE 'KDFARLIG TABMAX NOT ENOUGH '   TO ERROR-TEXT                 
039000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039100     END-IF                                                               
039200     IF IN-W11607-KDFARLIG > ZERO                                         
039300       MOVE IN-W11607-KDFARLIG   TO TAB-KDFARLIG(KDFARLIG-TAB-IX)         
039500       ADD  +00001               TO KDFARLIG-TAB-IX                       
039600     END-IF                                                               
039700                                                                          
039800     IF KDSORT-TAB-IX > KDSORT-TAB-MAX                                    
039900       MOVE 'KDFARLIG TABMAX NOT ENOUGH '   TO ERROR-TEXT                 
040000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
040100     END-IF                                                               
040200     IF IN-W11607-KDSORT > SPACE                                          
040300       MOVE IN-W11607-KDSORT     TO TAB-KDSORT  (KDSORT-TAB-IX)           
040500       ADD  +00001               TO KDSORT-TAB-IX                         
040600     END-IF                                                               
040700                                                                          
040800                                                                          
040900     .                                                                    
041000     SKIP2                                                                
041100 C-KONTROLL-FLYTTA-TILL-UTAREA   SECTION.                                 
041200     MOVE 'C-KONTROLL-FLYTTA-TILL-UTAREA' TO WS-CURRENT-SECTION           
041300                                                                          
041400     IF LAGER-W01172-PRARTSTD = ZERO                                      
041500     OR LAGER-W01172-FLLSRDEL = 'N'                                       
041600     OR LAGER-W01172-KDERS = +001 OR +002 OR +003 OR +004 OR              
041700                     +005 OR +006 OR +007 OR +008                         
041800     OR LAGER-W01172-KDERS > +009                                         
041810*KDERS SOMETHING ELSE THEN 0 OR 9.                                        
041900                                                                          
042000       MOVE LAGER-W01172-IDARTNR   TO OUT-IDARTNR                         
042001       MOVE 0                      TO OUT-KDBBCL                          
042200     ELSE                                                                 
042300                                                                          
042400       PERFORM CA-KONTROLL-MATCH-ANMORS-TAB                               
042500       IF MATCH-IN-ANMORS-TAB                                             
042600         MOVE LAGER-W01172-IDARTNR TO OUT-IDARTNR                         
042700         MOVE 0                    TO OUT-KDBBCL                          
042800         MOVE NEJ                  TO MATCH-IN-ANMORS-TAB-SW              
042900       ELSE                                                               
043000                                                                          
043100         IF END-OF-W23321                                                 
043110           MOVE NEJ                TO EJ-RETUR-TILL-CDC-SW                
043120         ELSE                                                             
043200           PERFORM CB-KONTROLL-OM-MATCH-W23321                            
043300         END-IF                                                           
043400                                                                          
043410         IF EJ-RETUR-TILL-CDC                                             
043500           MOVE LAGER-W01172-IDARTNR  TO OUT-IDARTNR                      
043600           MOVE 0                     TO OUT-KDBBCL                       
043802           MOVE NEJ                   TO EJ-RETUR-TILL-CDC-SW             
043900         ELSE                                                             
044000                                                                          
044100           IF NOT END-OF-W11607                                           
044200             PERFORM CC-KONTROLL-OM-MATCH-W11607                          
044300           END-IF                                                         
044400           IF MATCH-MOT-W11607                                            
044500             MOVE LAGER-W01172-IDARTNR TO OUT-IDARTNR                     
044600             MOVE  0                   TO OUT-KDBBCL                      
044800             MOVE NEJ              TO MATCH-MOT-W11607-SW                 
044900           ELSE                                                           
045000             MOVE LAGER-W01172-IDARTNR TO OUT-IDARTNR                     
045100             MOVE  1                   TO OUT-KDBBCL                      
045200*      DISPLAY 'RETURNERBAR-TILL-C1=' LAGER-W01172-IDARTNR                
045300           END-IF                                                         
045400         END-IF                                                           
045500       END-IF                                                             
045600     END-IF                                                               
046800     .                                                                    
046900     SKIP2                                                                
047000                                                                          
047100 CA-KONTROLL-MATCH-ANMORS-TAB  SECTION.                                   
047200*   KONTROLL-OM-MATCH-MOT-ANMORS-TAB                                      
047300     MOVE 'CA-KONTROLL-OM-MATCH'   TO WS-CURRENT-SECTION                  
047400     MOVE NEJ                    TO MATCH-IN-ANMORS-TAB-SW                
047500                                                                          
047600*KONTROLL AV LAGER-W01172-IDLEVNR                                         
047700                                                                          
047800     MOVE +00001                 TO IDLEVNR-TAB-IX                        
047900     PERFORM UNTIL IDLEVNR-TAB-IX > IDLEVNR-TAB-MAX                       
048000       IF    LAGER-W01172-IDLEVNR = TAB-IDLEVNR (IDLEVNR-TAB-IX)          
048100       AND LAGER-W01172-IDLEVNR > '     '                                 
048200                                                                          
048300         MOVE JA                 TO MATCH-IN-ANMORS-TAB-SW                
048500       END-IF                                                             
048600       ADD +1                    TO IDLEVNR-TAB-IX                        
048700     END-PERFORM                                                          
048800                                                                          
048900*KONTROLL AV LAGER-W01172-IDFKNGRP                                        
049000     IF MATCH-IN-ANMORS-TAB-SW = NEJ                                      
049100                                                                          
049200       MOVE +00001               TO IDFKNGRP-TAB-IX                       
049300       PERFORM UNTIL IDFKNGRP-TAB-IX > IDFKNGRP-TAB-MAX                   
049400         IF LAGER-W01172-IDFKNGRP =                                       
049500            TAB-IDFKNGRP (IDFKNGRP-TAB-IX)                                
049600         AND LAGER-W01172-IDFKNGRP > 00000                                
049700                                                                          
049800           MOVE JA               TO MATCH-IN-ANMORS-TAB-SW                
050000         END-IF                                                           
050100         ADD +1                  TO IDFKNGRP-TAB-IX                       
050200       END-PERFORM                                                        
050300     END-IF                                                               
050400                                                                          
050500*KONTROLL AV LAGER-W01172-KDPRODSL                                        
050600     IF MATCH-IN-ANMORS-TAB-SW = NEJ                                      
050700                                                                          
050800       MOVE +00001               TO KDPRODSL-TAB-IX                       
050900       PERFORM UNTIL KDPRODSL-TAB-IX > KDPRODSL-TAB-MAX                   
051000         IF LAGER-W01172-KDPRODSL =                                       
051100            TAB-KDPRODSL (KDPRODSL-TAB-IX)                                
051200         AND LAGER-W01172-KDPRODSL > 000                                  
051300                                                                          
051400           MOVE JA               TO MATCH-IN-ANMORS-TAB-SW                
051600         END-IF                                                           
051700         ADD +1                  TO KDPRODSL-TAB-IX                       
051800       END-PERFORM                                                        
051900     END-IF                                                               
052000                                                                          
052100*KONTROLL AV LAGER-W01172-KDFARLIG                                        
052200     IF MATCH-IN-ANMORS-TAB-SW = NEJ                                      
052300                                                                          
052400       MOVE +00001               TO KDFARLIG-TAB-IX                       
052500       PERFORM UNTIL KDFARLIG-TAB-IX > KDFARLIG-TAB-MAX                   
052600         IF LAGER-W01172-KDFARLIG =                                       
052700            TAB-KDFARLIG (KDFARLIG-TAB-IX)                                
052800         AND LAGER-W01172-KDFARLIG > 0                                    
052900                                                                          
053000           MOVE JA               TO MATCH-IN-ANMORS-TAB-SW                
053200         END-IF                                                           
053300         ADD +1                  TO KDFARLIG-TAB-IX                       
053400       END-PERFORM                                                        
053500     END-IF                                                               
053600                                                                          
053700*KONTROLL AV LAGER-W01172-KDSORT                                          
053800     IF MATCH-IN-ANMORS-TAB-SW = NEJ                                      
053900                                                                          
054000       MOVE +00001               TO KDSORT-TAB-IX                         
054100       PERFORM UNTIL KDSORT-TAB-IX > KDSORT-TAB-MAX                       
054200         IF LAGER-W01172-KDSORT = TAB-KDSORT (KDSORT-TAB-IX)              
054300         AND LAGER-W01172-KDSORT > '  '                                   
054400                                                                          
054500           MOVE JA               TO MATCH-IN-ANMORS-TAB-SW                
054700         END-IF                                                           
054800         ADD +1                  TO KDSORT-TAB-IX                         
054900       END-PERFORM                                                        
055000     END-IF                                                               
055100     .                                                                    
055200     SKIP2                                                                
055300                                                                          
055400 CB-KONTROLL-OM-MATCH-W23321      SECTION.                                
055500* KONTROLL-OM-MATCH-MOT-W23321                                            
055600     MOVE 'CB-KONTROLL-OM-MATCH'   TO WS-CURRENT-SECTION                  
055700                                                                          
055810     MOVE NEJ                      TO EJ-RETUR-TILL-CDC-SW                
055820     MOVE NEJ                      TO ARTIKEL-FINNS-PA-W23321-SW          
055900                                                                          
055960     IF IN-W23321-IDARTNR < LAGER-W01172-IDARTNR                          
056000       PERFORM S02-READ-W23321                                            
056010     END-IF                                                               
056100                                                                          
056200     PERFORM UNTIL END-OF-W23321                                          
056300                OR IN-W23321-IDARTNR > LAGER-W01172-IDARTNR               
056400                OR EJ-RETUR-TILL-CDC-SW = JA                              
056500                                                                          
056600       PERFORM CBA-KONTROLL-AV-SUBORDER                                   
056700                                                                          
056800       PERFORM S02-READ-W23321                                            
057000     END-PERFORM                                                          
057001                                                                          
057002     IF ARTIKEL-FINNS-PA-W23321                                           
057003       CONTINUE                                                           
057004     ELSE                                                                 
057006       MOVE NEJ                    TO EJ-RETUR-TILL-CDC-SW                
057040     END-IF                                                               
057100                                                                          
057200     .                                                                    
057300     SKIP2                                                                
057400 CBA-KONTROLL-AV-SUBORDER             SECTION.                            
057500     MOVE 'CBA-KONTROLL-AV-SUBORDER' TO WS-CURRENT-SECTION                
057600*KONTROLL OM DET FINNS NÅGON ORDER INOM FRYSTIDEN                         
057700*DVS OM (TIAVROP-INL > (DAGENS VECKA + KVVECKOR + 4 VECKOR))              
057800                                                                          
057900     IF LAGER-W01172-IDARTNR = IN-W23321-IDARTNR                          
058000                                                                          
058010       MOVE JA               TO ARTIKEL-FINNS-PA-W23321-SW                
058300       IF IN-W23321-TIAVROP-INL >                                         
058400         (WS-TIAAVV-GRP-NUM +                                             
058500          LAGER-W01172-KVVECKOR-FT + WS-CONSTANT-4-WEEKS)                 
058600                                                                          
058710         MOVE JA                   TO EJ-RETUR-TILL-CDC-SW                
058810       END-IF                                                             
058900     END-IF                                                               
059000                                                                          
060600     .                                                                    
060700     SKIP2                                                                
060800                                                                          
060900 CC-KONTROLL-OM-MATCH-W11607      SECTION.                                
061000     MOVE 'CC-KONTROLL-OM-MATCH'   TO WS-CURRENT-SECTION                  
061100                                                                          
061200     MOVE NEJ                      TO MATCH-MOT-W11607-SW                 
061300     MOVE NEJ                      TO W11607-EOF-SW                       
061400                                                                          
061500     IF LAGER-W01172-IDARTNR > IN-W11607-IDARTNR                          
061600       PERFORM S03-READ-W11607                                            
061700     END-IF                                                               
061800                                                                          
061900     PERFORM UNTIL END-OF-W11607                                          
062000                OR IN-W11607-IDARTNR > LAGER-W01172-IDARTNR               
062100                OR MATCH-MOT-W11607                                       
062300                                                                          
062400       IF LAGER-W01172-IDARTNR = IN-W11607-IDARTNR                        
062500                                                                          
062600         MOVE JA                   TO MATCH-MOT-W11607-SW                 
063100                                                                          
063200       END-IF                                                             
063300                                                                          
063400       PERFORM S03-READ-W11607                                            
063500     END-PERFORM                                                          
063600                                                                          
063700     .                                                                    
063800     SKIP2                                                                
063900                                                                          
064000 Z-FINIT SECTION.                                                         
064100     MOVE 'Z-FINIT             '   TO WS-CURRENT-SECTION                  
064200                                                                          
064300     CLOSE W01172                                                         
064400           W23321                                                         
064500           W11607                                                         
064600           W11604                                                         
064700     SKIP2                                                                
064800     MOVE 'S' TO POSTSUM-OPKOD                                            
064900     CALL POSTSUM USING POSTSUM-PARM                                      
065000     .                                                                    
065100     EJECT                                                                
065200                                                                          
065300 S01-READ-W01172  SECTION.                                                
065400     MOVE 'S01-READ-W01172     '   TO WS-CURRENT-SECTION                  
065500                                                                          
065600     READ W01172 INTO LAGER-W01172-AREA                                   
065700     AT END                                                               
065800        MOVE HIGH-VALUE TO LAGER-W01172-AREA                              
065900*       SET END-OF-W01172 TO TRUE                                         
066000        MOVE JA           TO W01172-EOF-SW                                
066100                                                                          
066200     NOT AT END                                                           
066300        MOVE 'W01172' TO POSTSUM-FDNAMN                                   
066400        MOVE 'W11601D3' TO POSTSUM-DDNAMN2                                
066500*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
066600        MOVE SPACE            TO POSTSUM-TRANSTYP                         
066700        CALL POSTSUM USING POSTSUM-PARM                                   
066800     END-READ                                                             
066900     .                                                                    
067000     EJECT                                                                
067100                                                                          
067200 S02-READ-W23321  SECTION.                                                
067300     MOVE 'S02-READ-W23321     '   TO WS-CURRENT-SECTION                  
067400                                                                          
067500     READ W23321 INTO IN-W23321-AREA                                      
067600     AT END                                                               
067700        MOVE HIGH-VALUE TO IN-W23321-AREA                                 
067800*       SET END-OF-W23321 TO TRUE                                         
067900        MOVE JA                   TO W23321-EOF-SW                        
068000                                                                          
068100     NOT AT END                                                           
068200        MOVE 'W23321' TO POSTSUM-FDNAMN                                   
068300        MOVE 'W11601D2' TO POSTSUM-DDNAMN2                                
068400*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
068500        MOVE SPACE           TO POSTSUM-TRANSTYP                          
068600        CALL POSTSUM USING POSTSUM-PARM                                   
068700     END-READ                                                             
068800     .                                                                    
068900     EJECT                                                                
069000                                                                          
069100 S03-READ-W11607  SECTION.                                                
069200     MOVE 'S03-READ-W11607     '   TO WS-CURRENT-SECTION                  
069300                                                                          
069400     READ W11607 INTO IN-W11607-AREA                                      
069500                                                                          
069600     AT END                                                               
069700        MOVE HIGH-VALUE TO IN-W11607-AREA                                 
069800        SET END-OF-W11607 TO TRUE                                         
069900        MOVE JA                   TO W11607-EOF-SW                        
070000                                                                          
070100     NOT AT END                                                           
070300                                                                          
070400        MOVE 'W11607' TO POSTSUM-FDNAMN                                   
070500        MOVE 'W11601D1' TO POSTSUM-DDNAMN2                                
070600*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
070700        MOVE SPACE            TO POSTSUM-TRANSTYP                         
070800        CALL POSTSUM USING POSTSUM-PARM                                   
070900     END-READ                                                             
071000     .                                                                    
071100     EJECT                                                                
071200                                                                          
071300 S11-WRITE-W11604 SECTION.                                                
071400     MOVE 'S11-WRITE-W11604    '   TO WS-CURRENT-SECTION                  
071500                                                                          
071600     WRITE OUT-RECORD FROM OUT-AREA                                       
071700                                                                          
071800*    MOVE OUT-EDPTYP TO POSTSUM-TRANSTYP                                  
071900     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
072000     MOVE 'W11604' TO POSTSUM-FDNAMN                                      
072100     MOVE 'W11601D4' TO POSTSUM-DDNAMN2                                   
072200     CALL POSTSUM USING POSTSUM-PARM                                      
072300     .                                                                    
072400     EJECT                                                                
072500                                                                          
072600 S99-ABEND SECTION.                                                       
072700                                                                          
072800     SKIP2                                                                
072900     MOVE 'S' TO POSTSUM-OPKOD                                            
073000     CALL POSTSUM USING POSTSUM-PARM                                      
073100     CALL ABEND USING RKOD-ABEND                                          
073200     .                                                                    
