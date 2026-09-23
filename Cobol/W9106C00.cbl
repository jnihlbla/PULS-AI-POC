000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9106C00.                                                
000300 AUTHOR.         KIHLBERG STEFAN.                                         
000400 DATE-WRITTEN.   13/12/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        FECHES IDSTATNR FROM PARTS MASTER AND ADD IT TO FILE TO          
000900*        LOGENT                                                           
001000*                                                                         
001100*        THE PROGRAM READS     WDK6                                       
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- PARTS WITH CHANGED SUPPLIER                                
002600     SELECT W91061                     ASSIGN TO W9106CD1.                
002700     SKIP2                                                                
002800*          --- PARTS WITH CHANGES SUPPLIER, IDSTATNR ADDED                
002900     SELECT W9106C                     ASSIGN TO W9106CD2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W91061                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY A7290B01      -L.                                              
004000     SKIP3                                                                
004100 FD  W9106C                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  RECORD -COPY A7290B01 -PRE  OUT-  -L.                                
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W9106C00'.            
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  W91061-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W91061                       VALUE 'J'.                   
005500     EJECT                                                                
005600 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES TODAYS-DATE.                                        
005800     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005900     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006000     03  TODAYS-DATE-DAY         PIC 9(2).                                
006100     EJECT                                                                
006200 01  GENERAL-SUBPROGRAMS.                                                 
006300*                                                                         
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
006900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007000     SKIP2                                                                
007100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007200                                                                          
007300*                                                                         
007400                                                                          
007500*01  -COPY W009CIA                                                        
007600     EJECT                                                                
007700*01  -COPY WDECAREA                                                       
007800 01  FILLER                      PIC X(16)   VALUE 'DECAREA    '.         
007900     EJECT                                                                
008000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008300     SKIP2                                                                
008400 01  ERROR-TEXT.                                                          
008500     03  FILLER                  PIC X(8)    VALUE 'ERRORTXT'.            
008600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL POSTSUM                                          
008900*                                                                         
009000*01  -COPY W0005   -PRE  POSTSUM-                                         
009100     EJECT                                                                
009200 01  IN-AREA-START               PIC X(24)   VALUE                        
009300                                 'IN-AREA-START  '.                       
009400     SKIP2                                                                
009500                                                                          
009600*01  AREA -COPY A7290B01     -PRE IN-                                     
009700     EJECT                                                                
009800 01  OUT-AREA-START              PIC X(24)   VALUE                        
009900                                 'OUT-AREA-START  '.                      
010000     SKIP2                                                                
010100                                                                          
010200*01  AREA -COPY A7290B01     -PRE OUT-                                    
010300     EJECT                                                                
010400*    --- AREAS FOR IMS-SECTIONS                                           
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  KEYS-FOR-DLI.                                                        
011000     03  W-IDARTNR-X.                                                     
011100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011200     03  W-KDSEGKEY-X.                                                    
011300         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
011400     SKIP2                                                                
011500*    --- STATUS-KOD FRÅN IMS                                              
011600 01  STATUS-WS                   PIC XX.                                  
011700     88  SEGMENT-FOUND                       VALUE '  '.                  
011800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012000     SKIP2                                                                
012100 01  GOOD-STATUSCODES.                                                    
012200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012300     SKIP3                                                                
012400 01  SSA1                        PIC X(64).                               
012500 01  SSA2                        PIC X(64).                               
012600     EJECT                                                                
012700*    --- IMS FUNCTION CODES                                               
012800*01  -COPY W0003                                                          
012900     EJECT                                                                
013000*    ---  DLI INPUT-OUTPUT AREA                                           
013100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
013200 01  DLI-IO-WDK601.                                                       
013300*    03  -COPY WDK601                                                     
013400     EJECT                                                                
013500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
013600 01  DLI-IO-WDK611.                                                       
013700*    03  -COPY WDK611                                                     
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000                                                                          
014100                                                                          
014200*01  -COPY W0008  -PRE WDK6-                                              
014300     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014500 PROCEDURE DIVISION  USING WDK6-PCB.                                      
014600 MAIN SECTION.                                                            
014700     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
014800                                                                          
014900                                                                          
015000     PERFORM A-INIT                                                       
015100                                                                          
015200     PERFORM S01-READ-W91061                                              
015300     PERFORM UNTIL END-OF-W91061                                          
015310       MOVE IN-AREA       TO OUT-AREA                                     
015400       IF IN-POSTTYP NOT = 11                                             
015500         MOVE IN-ARTIKELNR         TO DEC-IDFRIDATA                       
015600         MOVE 9                    TO DEC-KVHELTAL                        
015700         MOVE 0                    TO DEC-KVDECIMAL                       
015800                                                                          
015900          CALL WDECEDIT USING DEC-WDECAREA                                
016000                                                                          
016100         IF DEC-KDSVAR-OK                                                 
016200           MOVE DEC-IDEDITDATA   TO W-IDARTNR                             
016300                                                                          
016400           PERFORM IMS-GET-WDK601                                         
016410           IF SEGMENT-FOUND                                               
016500             IF ART-KDERS-UTG > ZERO                                      
016600               CONTINUE                                                   
016700             ELSE                                                         
016800               PERFORM IMS-GET-WDK611                                     
016900               IF SEGMENT-FOUND                                           
017000                 MOVE 'VO'             TO CIA-IDARTPRE-IN                 
017100                 MOVE CLAG-IDSTATNR(3) TO CIA-IDARTBET-IN                 
017200                 CALL W009CIA USING CIA-W009CIA                           
017300                 MOVE CIA-IDARTBET-UT   TO OUT-STATNR                     
017400               END-IF                                                     
017500             END-IF                                                       
017510           END-IF                                                         
017600         END-IF                                                           
017610       END-IF                                                             
017620       PERFORM S11-WRITE-W9106C                                           
017700       PERFORM S01-READ-W91061                                            
017800     END-PERFORM                                                          
017900                                                                          
018000                                                                          
018100     PERFORM Z-FINIT                                                      
018200                                                                          
018300     MOVE ZERO TO RETURN-CODE                                             
018400     GOBACK                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 A-INIT SECTION.                                                          
018800                                                                          
018900     OPEN INPUT  W91061                                                   
019000                                                                          
019100     OPEN OUTPUT W9106C                                                   
019200                                                                          
019300     ACCEPT TODAYS-DATE  FROM DATE                                        
019400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019500     .                                                                    
019600     EJECT                                                                
019700 Z-FINIT SECTION.                                                         
019800     CLOSE W91061                                                         
019900           W9106C                                                         
020000     SKIP2                                                                
020100     MOVE 'S' TO POSTSUM-OPKOD                                            
020200     CALL POSTSUM USING POSTSUM-PARM                                      
020300     .                                                                    
020400     EJECT                                                                
020500 S01-READ-W91061  SECTION.                                                
020600     READ W91061 INTO IN-AREA                                             
020700     AT END                                                               
020800        MOVE HIGH-VALUE TO IN-AREA                                        
020900        SET END-OF-W91061 TO TRUE                                         
021000                                                                          
021100     NOT AT END                                                           
021200        MOVE 'W91061' TO POSTSUM-FDNAMN                                   
021300        MOVE 'W9106CD1' TO POSTSUM-DDNAMN2                                
021400*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
021500        CALL POSTSUM USING POSTSUM-PARM                                   
021600     END-READ                                                             
021700     .                                                                    
021800     EJECT                                                                
021900 S11-WRITE-W9106C SECTION.                                                
022000                                                                          
022100     WRITE OUT-RECORD FROM OUT-AREA                                       
022200                                                                          
022300     MOVE 'W9106C' TO POSTSUM-FDNAMN                                      
022400     MOVE 'W9106CD2' TO POSTSUM-DDNAMN2                                   
022500     CALL POSTSUM USING POSTSUM-PARM                                      
022600     .                                                                    
022700     EJECT                                                                
022800 S99-ABEND SECTION.                                                       
022900                                                                          
023000     SKIP2                                                                
023100     MOVE 'S' TO POSTSUM-OPKOD                                            
023200     CALL POSTSUM USING POSTSUM-PARM                                      
023300     CALL ABEND USING RKOD-ABEND                                          
023400     .                                                                    
023500     EJECT                                                                
023600* --- IMS SECTIONS  ---                                                   
023700                                                                          
023800     EJECT                                                                
023900 IMS-GET-WDK601 SECTION.                                                  
024000                                                                          
024100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
024200          DELIMITED BY SIZE INTO SSA1                                     
024300     MOVE '  GE' TO GOOD-STATUSCODES                                      
024400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
024500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
024600     PERFORM IMS-STATUSCHECK                                              
024700     .                                                                    
024800     EJECT                                                                
024900 IMS-GET-WDK611 SECTION.                                                  
025000                                                                          
025100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
025200          DELIMITED BY SIZE INTO SSA1                                     
025300     MOVE '  GE' TO GOOD-STATUSCODES                                      
025400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
025500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025600     PERFORM IMS-STATUSCHECK                                              
025700     .                                                                    
025800     EJECT                                                                
025900 IMS-STATUSCHECK SECTION.                                                 
026000                                                                          
026100     SET STATUS-IX TO 1                                                   
026200     SEARCH GOOD-STATUS                                                   
026300       AT END                                                             
026400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
026500           DELIMITED BY SIZE INTO ERROR-TEXT                              
026600         DISPLAY ERROR-TEXT                                               
026700         CALL FELLOG                                                      
026800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
026900         CONTINUE                                                         
027000     END-SEARCH                                                           
027100     .                                                                    
