000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5710500.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   11/11/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        ADD BUFFER INFORMATION FROM WDD8                                 
001000*                                                                         
001100*        THE PROGRAM READS     WDD8                                       
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- WDJ701 INFO                                                
002200     SELECT W571D1                     ASSIGN TO W57105D1.                
002300     SKIP2                                                                
002400*          --- BUFFER INFO                                                
002500     SELECT W571D2                     ASSIGN TO W57105D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W571D1                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  -COPY WDJ701      -L.                                                
003600     SKIP3                                                                
003700 FD  W571D2                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  RECORD -COPY W57105 -PRE  W571D2-  -L.                               
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W5710500'.            
004600 77  YES                         PIC X       VALUE 'J'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
004800 77  WS-KVLS-ACS                 PIC S9(7)      COMP-3 VALUE ZERO.        
004900     SKIP2                                                                
005000 01  ERROR-TEXT.                                                          
005100     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005200     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005300                                                                          
005400 77  W571D1-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W571D1                       VALUE 'Y'.                   
005600     EJECT                                                                
005700 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES TODAYS-DATE.                                        
005900     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006100     03  TODAYS-DATE-DAY         PIC 9(2).                                
006200     EJECT                                                                
006300 01  GENERAL-SUBPROGRAMS.                                                 
006400*                                                                         
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     EJECT                                                                
006900*    --- PARAMETRAR TILL POSTSUM                                          
007000*                                                                         
007100*01  -COPY W0005   -PRE  POSTSUM-                                         
007200     EJECT                                                                
007300 01  W571D1-AREA-START           PIC X(24)   VALUE                        
007400                                             'W571D1-AREA-START'.         
007500     SKIP2                                                                
007600                                                                          
007700*01  AREA -COPY WDJ701     -PRE W571D1-                                   
007800     EJECT                                                                
007900 01  W571D2-AREA-START           PIC X(24)   VALUE                        
008000                                             'W571D2-AREA-START'.         
008100     SKIP2                                                                
008200                                                                          
008300*01  AREA -COPY W57105     -PRE W571D2-                                   
008400*                                                                         
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008700     SKIP3                                                                
008800 01  KEYS-TILL-DLI.                                                       
008900     03  W-WDD801-IDARTNR-X.                                              
009000         05  W-WDD801-IDARTNR    PIC S9(9)   VALUE ZERO   COMP-3.         
009100     03  W-WDD811-IDDC-X.                                                 
009200         05  W-WDD811-IDDC       PIC X(2)    VALUE SPACE.                 
009300     SKIP2                                                                
009400*    --- STATUS-KOD FRÅN IMS                                              
009500 01  STATUS-WS                   PIC XX.                                  
009600     88  SEGMENT-FOUND                       VALUE '  '.                  
009700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009900     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
010000     88  IMS-NOT-OK                          VALUE 'XD'.                  
010100     SKIP2                                                                
010200 01  GOOD-STATUSCODES.                                                    
010300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(64).                               
010600 01  SSA2                        PIC X(64).                               
010700     EJECT                                                                
010800*    --- IMS FUNCTION CODES                                               
010900*01  -COPY W0003                                                          
011000     EJECT                                                                
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200                                                                          
011300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD801'.                      
011400 01  DLI-IO-WDD801.                                                       
011500*    03  -COPY WDD801                                                     
011600     EJECT                                                                
011700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD811'.                      
011800 01  DLI-IO-WDD811.                                                       
011900*    03  -COPY WDD811                                                     
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200                                                                          
012300*01  -COPY W0009   -PRE MSG-                                              
012400                                                                          
012500*01  -COPY W0008  -PRE WDD8-                                              
012600     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012800 PROCEDURE DIVISION  USING MSG-PCB WDD8-PCB.                              
012900 MAIN SECTION.                                                            
013000     ENTRY 'DLITCBL' USING MSG-PCB WDD8-PCB.                              
013100                                                                          
013200     SKIP2                                                                
013300     PERFORM A-INIT                                                       
013400     PERFORM S01-READ-W571D1                                              
013500     PERFORM UNTIL END-OF-W571D1                                          
013600       PERFORM B-GET-BUFFER-INFO                                          
013700       PERFORM S01-READ-W571D1                                            
013800     END-PERFORM                                                          
013900                                                                          
014000     PERFORM Z-FINIT                                                      
014100                                                                          
014200     MOVE ZERO TO RETURN-CODE                                             
014300     GOBACK                                                               
014400     .                                                                    
014500     EJECT                                                                
014600 A-INIT SECTION.                                                          
014700     SKIP2                                                                
014800                                                                          
014900     OPEN INPUT W571D1                                                    
015000                                                                          
015100     OPEN OUTPUT W571D2                                                   
015200                                                                          
015300                                                                          
015400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015500     .                                                                    
015600     EJECT                                                                
015700 Z-FINIT SECTION.                                                         
015800                                                                          
015900                                                                          
016000     CLOSE W571D1                                                         
016100                                                                          
016200           W571D2                                                         
016300                                                                          
016400     MOVE 'S' TO POSTSUM-OPKOD                                            
016500     CALL POSTSUM USING POSTSUM-PARM                                      
016600     .                                                                    
016700     EJECT                                                                
016800 B-GET-BUFFER-INFO SECTION.                                               
016900                                                                          
017000     IF W571D1-ACS-ADLAGOMR NOT = LOW-VALUES                              
017100       MOVE W571D1-ACS-IDARTNR       TO W-WDD801-IDARTNR                  
017200       MOVE ZERO                     TO WS-KVLS-ACS                       
017300       MOVE W571D1-AREA              TO W571D2-AREA                       
017400       MOVE ZERO                     TO W571D2-KVLS-BUFFERT               
017500                                                                          
017600       PERFORM IMS-GU-WDD801                                              
017700                                                                          
017800       IF SEGMENT-FOUND                                                   
017900         MOVE W571D1-ACS-IDDC   TO W-WDD811-IDDC                          
018000         PERFORM IMS-GNP-WDD811                                           
018100         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                  
018200           ADD SALDO-KVBUFF-F   TO WS-KVLS-ACS                            
018300           ADD SALDO-KVBUFF-OF  TO WS-KVLS-ACS                            
018400           PERFORM IMS-GNP-WDD811                                         
018500         END-PERFORM                                                      
018600       END-IF                                                             
018700                                                                          
018800       MOVE WS-KVLS-ACS         TO W571D2-KVLS-BUFFERT                    
018900       PERFORM S11-WRITE-W571D2                                           
018901     ELSE                                                                 
018902        MOVE LOW-VALUES         TO W571D2-AREA                            
018903        MOVE W571D1-ACS-IDDC    TO W571D2-IDDC                            
018904        PERFORM S11-WRITE-W571D2                                          
018910     END-IF                                                               
019000     .                                                                    
019100 S01-READ-W571D1  SECTION.                                                
019200     SKIP2                                                                
019300     READ W571D1 INTO W571D1-AREA                                         
019400     AT END                                                               
019500        SET END-OF-W571D1  TO TRUE                                        
019600                                                                          
019700     NOT AT END                                                           
019800        MOVE 'W571D1'      TO POSTSUM-FDNAMN                              
019900        MOVE 'W57105D1'    TO POSTSUM-DDNAMN2                             
020000        CALL POSTSUM USING POSTSUM-PARM                                   
020100     END-READ                                                             
020200     .                                                                    
020300     EJECT                                                                
020400 S11-WRITE-W571D2 SECTION.                                                
020500     SKIP2                                                                
020600     WRITE W571D2-RECORD FROM W571D2-AREA                                 
020700                                                                          
020800     MOVE 'W571D2 ' TO POSTSUM-FDNAMN                                     
020900     MOVE 'W57105D2' TO POSTSUM-DDNAMN2                                   
021000     CALL POSTSUM USING POSTSUM-PARM                                      
021100     .                                                                    
021200     EJECT                                                                
021300* --- IMS SECTIONS  ---                                                   
021400                                                                          
021500     EJECT                                                                
021600 IMS-GU-WDD801 SECTION.                                                   
021700                                                                          
021800     STRING 'WDD801  (IDARTNR  =' W-WDD801-IDARTNR-X ')'                  
021900          DELIMITED BY SIZE INTO SSA1                                     
022000     MOVE '  GE' TO GOOD-STATUSCODES                                      
022100     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD801 SSA1                    
022200     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
022300     PERFORM IMS-STATUSCHECK                                              
022400     .                                                                    
022500     EJECT                                                                
022600 IMS-GNP-WDD811 SECTION.                                                  
022700                                                                          
022800     STRING 'WDD811  (IDDC     =' W-WDD811-IDDC-X ')'                     
022900          DELIMITED BY SIZE INTO SSA1                                     
023000     MOVE '  GE' TO GOOD-STATUSCODES                                      
023100     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
023200     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
023300     PERFORM IMS-STATUSCHECK                                              
023400     .                                                                    
023500     EJECT                                                                
023600 IMS-STATUSCHECK SECTION.                                                 
023700     SKIP2                                                                
023800     SET STATUS-IX TO 1                                                   
023900     SEARCH GOOD-STATUS                                                   
024000       AT END                                                             
024100         STRING ' INVALID STATUS CODE FROM IMS:' STATUS-WS                
024200           DELIMITED BY SIZE INTO ERROR-TEXT                              
024300         DISPLAY ERROR-TEXT                                               
024400         CALL FELLOG                                                      
024500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
024600         CONTINUE                                                         
024700     END-SEARCH                                                           
024800     .                                                                    
