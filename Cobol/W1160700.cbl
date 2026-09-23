000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1160700.                                                
000300 AUTHOR.         ANDERSSON BERT.                                          
000400 DATE-WRITTEN.   09/12/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        BUY BACKS NOT ALLOWED                                            
000900*                                                                         
001000*        THE PROGRAM READS     WDA8                                       
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- W11607-WDA8-SB                                             
002500     SELECT W11607                     ASSIGN TO W11607D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W11607                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  RECORD -COPY W11607 -PRE    W11607-  -L.                             
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W1160700'.            
004000                                                                          
004100 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
004200 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004300                                                                          
004400 77  FILLER                      PIC X(08)   VALUE 'CURRENT:'.            
004500 77  WS-CURRENT-SECTION          PIC X(32)   VALUE SPACE.                 
004600                                                                          
004700 77  FILLER                      PIC X(08)   VALUE 'CURR-IMS'.            
004800 77  WS-CURRENT-IMS-SECTION      PIC X(32)   VALUE SPACE.                 
004900                                                                          
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  KDANMORS-INDX               PIC S9(9)   VALUE +0.                    
005400                                                                          
005500 77  KDANMORS-MAX                PIC S9(9)   VALUE 14  COMP-3.            
005600                                                                          
005700                                                                          
005800 77  FILLER                      PIC X(08)   VALUE 'NYCKL-SW'.            
005900 77  FLYTTA-NASTA-WDA811-SW      PIC X       VALUE 'J'.                   
006000     88  FLYTTA-NASTA-WDA811                 VALUE 'J'.                   
006100                                                                          
006200     EJECT                                                                
006300 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006400 01  FILLER REDEFINES TODAYS-DATE.                                        
006500     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006600     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006700     03  TODAYS-DATE-DAY         PIC 9(2).                                
006800     EJECT                                                                
006900 01  GENERAL-SUBPROGRAMS.                                                 
007000*                                                                         
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007600     SKIP2                                                                
007700*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007800                                                                          
007900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008200     SKIP2                                                                
008300*    --- PARAMETERS FOR SUBPROGRAM DATKORT                                
008400*                                                                         
008500 01  PROGRAM-NAME                PIC X(6)    VALUE 'W11607'.              
008600     SKIP2                                                                
008700*    --- PARAMETRAR TILL POSTSUM                                          
008800*                                                                         
008900*01  -COPY W0005   -PRE  POSTSUM-                                         
009000     EJECT                                                                
009100*01  -COPY WDATAREA                                                       
009200     EJECT                                                                
009300 01  WDA8-AREA-START             PIC X(24)   VALUE                        
009400                                 'WDA8-AREA-START  '.                     
009500     SKIP2                                                                
009600                                                                          
009700*01  AREA -COPY W11607       -PRE W11607-                                 
009800     EJECT                                                                
009900*    --- AREAS FOR IMS-SECTIONS                                           
010000*                                                                         
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010300     SKIP3                                                                
010400 01  KEYS-FOR-DLI.                                                        
010500     03  W-WDA801-X.                                                      
010600         05  W-BESORTRT          PIC X(20)   VALUE SPACE.                 
010700     03  W-WDA811-X.                                                      
010800         05  W-TEELMT            PIC X(16)   VALUE SPACE.                 
010900         05  W-IDELMT            PIC X(16)   VALUE SPACE.                 
011000     SKIP2                                                                
011100*    --- STATUS-KOD FRÅN IMS                                              
011200 01  STATUS-WS                   PIC XX.                                  
011300     88  SEGMENT-FOUND                       VALUE '  '.                  
011400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011410     88  END-OF-DATA                         VALUE 'GB'.                  
011500     SKIP2                                                                
011600 01  GOOD-STATUSCODES.                                                    
011700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011800     SKIP3                                                                
011900 01  SSA1                        PIC X(64).                               
012000 01  SSA2                        PIC X(64).                               
012100     EJECT                                                                
012200*    --- IMS FUNCTION CODES                                               
012300*01  -COPY W0003                                                          
012400     EJECT                                                                
012500*    ---  DLI INPUT-OUTPUT AREA                                           
012600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA8'.                        
012700 01  DLI-IO-AREA.                                                         
012800     03  IO-AREA             PIC X(096).                                  
012900     SKIP3                                                                
013000*    03  WDA801    -COPY WDA801    -RED IO-AREA                           
013100     SKIP3                                                                
013200*    03  WDA811    -COPY WDA811    -RED IO-AREA                           
013300     SKIP3                                                                
013400 LINKAGE SECTION.                                                         
013500                                                                          
013600                                                                          
013700*01  -COPY W0008  -PRE WDA8-                                              
013800     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING WDA8-PCB.                                      
014100 MAIN SECTION.                                                            
014200     ENTRY 'DLITCBL' USING WDA8-PCB.                                      
014300                                                                          
014400     PERFORM A-INIT                                                       
014500                                                                          
014600     PERFORM IMS-GET-WDA8                                                 
014700     PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATA                         
014800                                                                          
014900       EVALUATE WDA8-SEG-NAME-FB                                          
015000         WHEN 'WDA801'                                                    
015100           PERFORM B-CONTROL-WDA801                                       
015200                                                                          
015300         WHEN 'WDA811'                                                    
015400           PERFORM C-MOVE-WDA811                                          
015500       END-EVALUATE                                                       
015600                                                                          
015700       PERFORM IMS-GET-WDA8                                               
015800     END-PERFORM                                                          
015900     PERFORM Z-FINIT                                                      
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600     MOVE 'A-INIT         '   TO WS-CURRENT-SECTION                       
016700                                                                          
016800     OPEN OUTPUT W11607                                                   
016900                                                                          
017000     .                                                                    
017100     EJECT                                                                
017200                                                                          
017300 B-CONTROL-WDA801 SECTION.                                                
017400     MOVE 'B-CONTROL-WDA801  ' TO WS-CURRENT-SECTION                      
017500                                                                          
017600     MOVE NEJ            TO FLYTTA-NASTA-WDA811-SW                        
017700     MOVE +000000001     TO KDANMORS-INDX                                 
017800                                                                          
017900     PERFORM UNTIL KDANMORS-INDX > KDANMORS-MAX                           
018000                                                                          
018100       IF (SORT-KDANMORS(KDANMORS-INDX) = '98'                            
018200       AND SORT-KDRETBEH(KDANMORS-INDX) = 'S')                            
018300                                                                          
018400         MOVE JA             TO FLYTTA-NASTA-WDA811-SW                    
018500       END-IF                                                             
018600       ADD +1 TO KDANMORS-INDX                                            
018700     END-PERFORM                                                          
018800     .                                                                    
018900     EJECT                                                                
019000 C-MOVE-WDA811    SECTION.                                                
019100     MOVE 'C-MOVE-WDA811  '      TO WS-CURRENT-SECTION                    
019200                                                                          
019210     PERFORM CA-NOLLSTALL-AREA                                            
019220                                                                          
019300     IF FLYTTA-NASTA-WDA811                                               
019400                                                                          
019500        EVALUATE URET-TEELMT                                              
019600          WHEN 'IDARTNR'                                                  
019700            MOVE URET-IDARTNR    TO W11607-IDARTNR                        
019800          WHEN 'IDLEVNR'                                                  
019900            MOVE URET-IDLEVNR    TO W11607-IDLEVNR                        
020000          WHEN 'IDFKNGRP'                                                 
020100            MOVE URET-IDFKNGRP   TO W11607-IDFKNGRP                       
020200          WHEN 'KDFARLIG'                                                 
020300            MOVE URET-KDFARLIG   TO W11607-KDFARLIG                       
020400          WHEN 'KDSORT'                                                   
020500            MOVE URET-KDSORT     TO W11607-KDSORT                         
020600          WHEN 'KDPRODSL'                                                 
020700            MOVE URET-KDPRODSL   TO W11607-KDPRODSL                       
020800        END-EVALUATE                                                      
020900                                                                          
021000        PERFORM S11-WRITE-W11607                                          
021100                                                                          
021200     END-IF                                                               
021300     .                                                                    
021400     EJECT                                                                
021410 CA-NOLLSTALL-AREA          SECTION.                                      
021420                                                                          
021421     MOVE ZERO                   TO W11607-IDARTNR                        
021425     MOVE ZERO                   TO W11607-IDFKNGRP                       
021427     MOVE ZERO                   TO W11607-KDFARLIG                       
021428     MOVE ZERO                   TO W11607-KDPRODSL                       
021429     MOVE SPACE                  TO W11607-IDLEVNR                        
021430     MOVE SPACE                  TO W11607-KDSORT                         
021440     .                                                                    
021450     SKIP2                                                                
021500 Z-FINIT SECTION.                                                         
021600     MOVE 'Z-FINIT        '   TO WS-CURRENT-SECTION                       
021700                                                                          
021800     CLOSE W11607                                                         
021900     SKIP2                                                                
022000     MOVE 'S' TO POSTSUM-OPKOD                                            
022100     CALL POSTSUM USING POSTSUM-PARM                                      
022200     .                                                                    
022300     EJECT                                                                
022400 S11-WRITE-W11607 SECTION.                                                
022500     MOVE 'S11-WRITE-W11607   '   TO WS-CURRENT-SECTION                   
022600                                                                          
022700     WRITE W11607-RECORD FROM W11607-AREA                                 
022800                                                                          
022900*    MOVE WDA8-IDPTYP TO POSTSUM-TRANSTYP                                 
023000     MOVE 'W11607' TO POSTSUM-FDNAMN                                      
023100     MOVE 'W11607D1' TO POSTSUM-DDNAMN2                                   
023200     CALL POSTSUM USING POSTSUM-PARM                                      
023300     .                                                                    
023400     EJECT                                                                
023500 S99-ABEND SECTION.                                                       
023600     MOVE 'S99-ABEND          '   TO WS-CURRENT-SECTION                   
023700                                                                          
023800     SKIP2                                                                
023900     MOVE 'S' TO POSTSUM-OPKOD                                            
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100     CALL ABEND USING RKOD-ABEND                                          
024200     .                                                                    
024300     EJECT                                                                
024400* --- IMS SECTIONS  ---                                                   
024500                                                                          
024600                                                                          
024700 IMS-GET-WDA8   SECTION.                                                  
024800     MOVE 'IMS-GET-WDA8 '   TO WS-CURRENT-IMS-SECTION                     
024900                                                                          
025000     CALL CBLTDLI USING GN WDA8-PCB DLI-IO-AREA                           
025100     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
025200     MOVE '  GAGKGB'    TO GOOD-STATUSCODES                               
025300     PERFORM IMS-STATUSCHECK                                              
025400     .                                                                    
025500     EJECT                                                                
025600 IMS-STATUSCHECK SECTION.                                                 
025700                                                                          
025800     SET STATUS-IX TO 1                                                   
025900     SEARCH GOOD-STATUS                                                   
026000       AT END                                                             
026100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
026200           DELIMITED BY SIZE INTO ERROR-TEXT                              
026300         DISPLAY ERROR-TEXT                                               
026400         CALL FELLOG                                                      
026500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
026600         CONTINUE                                                         
026700     END-SEARCH                                                           
026800     .                                                                    
