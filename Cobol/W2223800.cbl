000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2223800.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   15/12/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        UPDATE FORECAST                                                  
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDK6                                       
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- W22237                                                     
002200     SELECT W2223701                   ASSIGN TO W22238D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W2223701                                                             
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W2223701    -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W2223800'.            
003700 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
003800 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
003900 77  PLAN-IX                     PIC S9(09)  VALUE +0  COMP SYNC.         
004000 77  PLAN-IX-MAX                 PIC S9(09)  VALUE +12 COMP SYNC.         
004010 77  SW-RESEASON-NEW             PIC X       VALUE 'N'.                   
004040                                                                          
004100 01  CHKP-VAR.                                                            
004200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004700     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004800 77  YES                         PIC X       VALUE 'J'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000     SKIP2                                                                
005100 01  ERROR-TEXT.                                                          
005200     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005400                                                                          
005500 77  W2223701-EOF-SW             PIC X       VALUE 'N'.                   
005600     88  END-OF-W2223701                     VALUE 'Y'.                   
005700     EJECT                                                                
005800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES TODAYS-DATE.                                        
006000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006200     03  TODAYS-DATE-DAY         PIC 9(2).                                
006300     EJECT                                                                
006400 01  GENERAL-SUBPROGRAMS.                                                 
006500*                                                                         
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006900     EJECT                                                                
007000*    --- PARAMETRAR TILL POSTSUM                                          
007100*                                                                         
007200*01  -COPY W0005   -PRE  POSTSUM-                                         
007300     EJECT                                                                
007400 01  IN-AREA-START               PIC X(24)   VALUE                        
007500                                             'IN-AREA-START'.             
007600*01  AREA -COPY W2223701   -PRE IN-                                       
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  KEYS-TILL-DLI.                                                       
008200     03  W-IDDCREF-X.                                                     
008300         05  W-IDDC-REF          PIC X(02)   VALUE SPACE.                 
008400     03  W-IDARTNR-X.                                                     
008500         05  W-IDARTNR           PIC S9(09) COMP-3 VALUE +0.              
008600     03  W-KDSEGKEY-X.                                                    
008700         05  W-KDSEGKEY          PIC X(01)   VALUE '1'.                   
008800     SKIP2                                                                
008900*    --- STATUS-KOD FRÅN IMS                                              
009000 01  STATUS-WS                   PIC XX.                                  
009100     88  SEGMENT-FOUND                       VALUE '  '.                  
009200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009400     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
009500     88  IMS-NOT-OK                          VALUE 'XD'.                  
009600     SKIP2                                                                
009700 01  GOOD-STATUSCODES.                                                    
009800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP3                                                                
010000 01  SSA1                        PIC X(132).                              
010100 01  SSA2                        PIC X(132).                              
010200 01  SSA3                        PIC X(132).                              
010300     EJECT                                                                
010400*    --- IMS FUNCTION CODES                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010700*    ---  DLI INPUT-OUTPUT AREA                                           
010800                                                                          
010900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK629 '.          
011000 01  DLI-IO-WDK629.                                                       
011100*    03  -COPY WDK629                                                     
011200     EJECT                                                                
011300 LINKAGE SECTION.                                                         
011400                                                                          
011500*01  -COPY W0009   -PRE MSG-                                              
011600                                                                          
011700*01  -COPY W0008  -PRE WDK6-                                              
011800     05  FILLER                  PIC X.                                   
011900     EJECT                                                                
012000 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
012100 MAIN SECTION.                                                            
012200     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
012300                                                                          
012400     PERFORM A-INIT                                                       
012500                                                                          
012600     PERFORM S01-READ-W2223701                                            
012700     PERFORM UNTIL END-OF-W2223701                                        
012710       MOVE 'N'                 TO SW-RESEASON-NEW                        
012800       MOVE IN-IDARTNR          TO W-IDARTNR                              
012900       MOVE IN-IDDC-REF         TO W-IDDC-REF                             
013000       PERFORM IMS-GHU-WDK629                                             
013100       IF SEGMENT-FOUND                                                   
013200         MOVE +0                TO PLAN-IX                                
013300         PERFORM UNTIL PLAN-IX = PLAN-IX-MAX                              
013310           ADD +1               TO PLAN-IX                                
013311                                                                          
013320           IF IN-RESEASON-PLAN (PLAN-IX) NOT =                            
013330                                   CREF-RESEASON-PLAN(PLAN-IX)            
013340             MOVE 'J'           TO SW-RESEASON-NEW                        
013350           END-IF                                                         
013360                                                                          
013400           MOVE IN-RESEASON-PLAN (PLAN-IX)                                
013500                                TO CREF-RESEASON-PLAN(PLAN-IX)            
013700         END-PERFORM                                                      
013710         IF (IN-KVPB-PLAN  > CREF-KVPB-PLAN) OR                           
013711            (SW-RESEASON-NEW = 'J')                                       
013716           IF CREF-FLREFNYO = 'J'                                         
013717             MOVE 'N'           TO CREF-FLREFNYO                          
013718           END-IF                                                         
013720         END-IF                                                           
013800         MOVE IN-KVPB-PLAN      TO CREF-KVPB-PLAN                         
013900         PERFORM IMS-REPL-WDK629                                          
014000         IF CHKP-ANT > CHKP-MAX                                           
014100           PERFORM X-TAKE-CHECKPOINT                                      
014200         END-IF                                                           
014300       END-IF                                                             
014400                                                                          
014500       PERFORM S01-READ-W2223701                                          
014600     END-PERFORM                                                          
014700                                                                          
014800                                                                          
014900     PERFORM Z-FINIT                                                      
015000                                                                          
015100     MOVE ZERO TO RETURN-CODE                                             
015200     GOBACK                                                               
015300     .                                                                    
015400     EJECT                                                                
015500 A-INIT SECTION.                                                          
015600     MOVE 'A-INIT                  ' TO CURRENT-SECTION                   
015700                                                                          
015800     PERFORM IMS-RESTART                                                  
015900                                                                          
016000     OPEN INPUT W2223701                                                  
016100                                                                          
016200                                                                          
016300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016400     .                                                                    
016500     EJECT                                                                
016600 Z-FINIT SECTION.                                                         
016700     MOVE 'Z-FINIT                 ' TO CURRENT-SECTION                   
016800                                                                          
016900                                                                          
017000     CLOSE W2223701                                                       
017100                                                                          
017200     MOVE 'S' TO POSTSUM-OPKOD                                            
017300     CALL POSTSUM USING POSTSUM-PARM                                      
017400     .                                                                    
017500     EJECT                                                                
017600 S01-READ-W2223701 SECTION.                                               
017700     MOVE 'S01-READ-W2223701       ' TO CURRENT-SECTION                   
017800                                                                          
017900     READ W2223701 INTO IN-AREA                                           
018000     AT END                                                               
018100        SET END-OF-W2223701 TO TRUE                                       
018200                                                                          
018300     NOT AT END                                                           
018400        MOVE 'W22232'   TO POSTSUM-FDNAMN                                 
018500        MOVE 'W22238D1' TO POSTSUM-DDNAMN2                                
018600        MOVE SPACE      TO POSTSUM-TRANSTYP                               
018700        CALL POSTSUM USING POSTSUM-PARM                                   
018800                                                                          
018900     END-READ                                                             
019000     .                                                                    
019100     EJECT                                                                
019200 X-TAKE-CHECKPOINT   SECTION.                                             
019300     MOVE 'X-TAKE-CHECKPOINT       ' TO CURRENT-SECTION                   
019400                                                                          
019500* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
019600* --- SAVE DATABASE KEYS IF NECESSARY                                     
019700     PERFORM IMS-CHECKPOINT                                               
019800     MOVE ZERO TO CHKP-ANT                                                
019900* --- REREAD DATABASE IF NECESSARY                                        
020000     .                                                                    
020100     EJECT                                                                
020200* --- IMS SECTIONS  ---                                                   
020300                                                                          
020400 IMS-GHU-WDK629 SECTION.                                                  
020500     MOVE 'IMS-GHU-WDK629          ' TO DBS-SECTION                       
020600                                                                          
021210     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
021220          DELIMITED BY SIZE INTO SSA1                                     
021230     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
021240          DELIMITED BY SIZE INTO SSA2                                     
021250     STRING 'WDK629  (IDDCREF  =' W-IDDCREF-X ')'                         
021260          DELIMITED BY SIZE INTO SSA3                                     
021300     MOVE '  GE' TO GOOD-STATUSCODES                                      
021400     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3         
021500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
021600     PERFORM IMS-STATUSCHECK                                              
021700     .                                                                    
021800                                                                          
021900 IMS-REPL-WDK629 SECTION.                                                 
022000     MOVE 'IMS-REPL-WDK629         ' TO DBS-SECTION                       
022100                                                                          
022200     MOVE '  ' TO GOOD-STATUSCODES                                        
022300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
022400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
022500     PERFORM IMS-STATUSCHECK                                              
022600     ADD +1                TO CHKP-ANT                                    
022700     .                                                                    
022800     EJECT                                                                
022900 IMS-RESTART SECTION.                                                     
023000     SKIP2                                                                
023100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
023200     MOVE '  ' TO GOOD-STATUSCODES                                        
023300     CALL CBLTDLI USING XRST MSG-PCB                                      
023400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
023500                        CHKP-AREA-LENGTH CHKP-AREA                        
023600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023700     PERFORM IMS-STATUSCHECK                                              
023800     .                                                                    
023900     SKIP3                                                                
024000 IMS-CHECKPOINT SECTION.                                                  
024100     SKIP2                                                                
024200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024300     MOVE '  XD' TO GOOD-STATUSCODES                                      
024400     CALL CBLTDLI USING CHKP MSG-PCB                                      
024500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024600                        CHKP-AREA-LENGTH CHKP-AREA                        
024700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024800     PERFORM IMS-STATUSCHECK                                              
024900                                                                          
025000     IF IMS-NOT-OK                                                        
025100       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
025200                                       TO ERROR-TEXT-STR                  
025300       DISPLAY ERROR-TEXT                                                 
025400       CALL FELLOG                                                        
025500     END-IF                                                               
025600     .                                                                    
025700     EJECT                                                                
025800 IMS-STATUSCHECK SECTION.                                                 
025900     SKIP2                                                                
026000     SET STATUS-IX TO 1                                                   
026100     SEARCH GOOD-STATUS                                                   
026200       AT END                                                             
026300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
026400           DELIMITED BY SIZE INTO ERROR-TEXT                              
026500         DISPLAY ERROR-TEXT                                               
026600         CALL FELLOG                                                      
026700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
026800         CONTINUE                                                         
026900     END-SEARCH                                                           
027000     .                                                                    
