000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2223300.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   15/12/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        UPDATE ASSETS IN CDC                                             
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
002100*          --- W22232                                                     
002200     SELECT W22232                     ASSIGN TO W22233D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W22232                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W22232      -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W2223300'.            
003700 01  CHKP-VAR.                                                            
003800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004300     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600     SKIP2                                                                
004700 01  ERROR-TEXT.                                                          
004800     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
004900     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005000                                                                          
005100 77  W22232-EOF-SW               PIC X       VALUE 'N'.                   
005200     88  END-OF-W22232                       VALUE 'Y'.                   
005300     EJECT                                                                
005400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005500 01  FILLER REDEFINES TODAYS-DATE.                                        
005600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005800     03  TODAYS-DATE-DAY         PIC 9(2).                                
005900     EJECT                                                                
006000 01  GENERAL-SUBPROGRAMS.                                                 
006100*                                                                         
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006500     EJECT                                                                
006600*    --- PARAMETRAR TILL POSTSUM                                          
006700*                                                                         
006800*01  -COPY W0005   -PRE  POSTSUM-                                         
006900     EJECT                                                                
007000 01  IN-AREA-START               PIC X(24)   VALUE                        
007100                                             'IN-AREA-START'.             
007200*01  AREA -COPY W22232     -PRE IN-                                       
007300*                                                                         
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007600     SKIP3                                                                
007700 01  KEYS-TILL-DLI.                                                       
007800     03  W-IDARTNR-X.                                                     
007900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007910     03  W-KDSEGKEY-X.                                                    
007920         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008000     SKIP2                                                                
008100*    --- STATUS-KOD FRÅN IMS                                              
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FOUND                       VALUE '  '.                  
008400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008600     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
008700     88  IMS-NOT-OK                          VALUE 'XD'.                  
008800     SKIP2                                                                
008900 01  GOOD-STATUSCODES.                                                    
009000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNCTION CODES                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009900                                                                          
010000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK611 '.          
010100 01  DLI-IO-WDK611.                                                       
010200*    03  -COPY WDK611                                                     
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010600*01  -COPY W0009   -PRE MSG-                                              
010700                                                                          
010800*01  -COPY W0008  -PRE WDK6-                                              
010900     05  FILLER                  PIC X.                                   
011000     EJECT                                                                
011100 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
011200 MAIN SECTION.                                                            
011300     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
011400                                                                          
011500     SKIP2                                                                
011600     PERFORM A-INIT                                                       
011700     PERFORM S01-READ-W22232                                              
011800     PERFORM UNTIL END-OF-W22232                                          
011900       MOVE IN-IDARTNR            TO W-IDARTNR                            
012000       PERFORM IMS-GHU-WDK611                                             
012100       IF SEGMENT-FOUND                                                   
012200          MOVE IN-KVAVROP-TOT  TO CLAG-KVAVROP-TOT                        
012300          MOVE IN-KVREFOVL-TOT TO CLAG-KVREFOVL-TOT                       
012400          MOVE IN-KVRETUR-TOT  TO CLAG-KVRETUR-TOT                        
012500          MOVE IN-KVTILLG-TOT  TO CLAG-KVTILLG-TOT                        
012600          PERFORM IMS-REPL-WDK611                                         
012700          IF CHKP-ANT > CHKP-MAX                                          
012800            PERFORM X-TAKE-CHECKPOINT                                     
012900          END-IF                                                          
013000       END-IF                                                             
013100                                                                          
013200       PERFORM S01-READ-W22232                                            
013300     END-PERFORM                                                          
013400                                                                          
013500                                                                          
013600     PERFORM Z-FINIT                                                      
013700                                                                          
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
014300     SKIP2                                                                
014400                                                                          
014500     PERFORM IMS-RESTART                                                  
014600                                                                          
014700     OPEN INPUT W22232                                                    
014800                                                                          
014900                                                                          
015000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015100     .                                                                    
015200     EJECT                                                                
015300 Z-FINIT SECTION.                                                         
015400                                                                          
015500                                                                          
015600     CLOSE W22232                                                         
015700     SKIP2                                                                
015800     MOVE 'S' TO POSTSUM-OPKOD                                            
015900     CALL POSTSUM USING POSTSUM-PARM                                      
016000     .                                                                    
016100     EJECT                                                                
016200 S01-READ-W22232  SECTION.                                                
016300     SKIP2                                                                
016400     READ W22232 INTO IN-AREA                                             
016500     AT END                                                               
016600        SET END-OF-W22232 TO TRUE                                         
016700                                                                          
016800     NOT AT END                                                           
016900        MOVE 'W22232'   TO POSTSUM-FDNAMN                                 
017000        MOVE 'W22233D1' TO POSTSUM-DDNAMN2                                
017100        MOVE SPACE      TO POSTSUM-TRANSTYP                               
017200        CALL POSTSUM USING POSTSUM-PARM                                   
017300                                                                          
017500     END-READ                                                             
017600     .                                                                    
017700     EJECT                                                                
017800 X-TAKE-CHECKPOINT   SECTION.                                             
017900                                                                          
018000* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
018100* --- SAVE DATABASE KEYS IF NECESSARY                                     
018200     PERFORM IMS-CHECKPOINT                                               
018300     MOVE ZERO TO CHKP-ANT                                                
018400* --- REREAD DATABASE IF NECESSARY                                        
018500     .                                                                    
018600     EJECT                                                                
018700* --- IMS SECTIONS  ---                                                   
018800                                                                          
018900 IMS-GHU-WDK611 SECTION.                                                  
019000                                                                          
019100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X  ')'                        
019200          DELIMITED BY SIZE INTO SSA1                                     
019210     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
019400          DELIMITED BY SIZE INTO SSA2                                     
019500     MOVE '  GE' TO GOOD-STATUSCODES                                      
019600     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
019700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
019800     PERFORM IMS-STATUSCHECK                                              
019900     .                                                                    
020000                                                                          
020100 IMS-REPL-WDK611 SECTION.                                                 
020200                                                                          
020300     MOVE '  ' TO GOOD-STATUSCODES                                        
020400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
020500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
020600     PERFORM IMS-STATUSCHECK                                              
020610     ADD +1                TO CHKP-ANT                                    
020700     .                                                                    
020900     EJECT                                                                
021000 IMS-RESTART SECTION.                                                     
021100     SKIP2                                                                
021200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021300     MOVE '  ' TO GOOD-STATUSCODES                                        
021400     CALL CBLTDLI USING XRST MSG-PCB                                      
021500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021600                        CHKP-AREA-LENGTH CHKP-AREA                        
021700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021800     PERFORM IMS-STATUSCHECK                                              
021900     .                                                                    
022000     SKIP3                                                                
022100 IMS-CHECKPOINT SECTION.                                                  
022200     SKIP2                                                                
022300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
022400     MOVE '  XD' TO GOOD-STATUSCODES                                      
022500     CALL CBLTDLI USING CHKP MSG-PCB                                      
022600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022700                        CHKP-AREA-LENGTH CHKP-AREA                        
022800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022900     PERFORM IMS-STATUSCHECK                                              
023000                                                                          
023100     IF IMS-NOT-OK                                                        
023200       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
023400                                       TO ERROR-TEXT-STR                  
023410       DISPLAY ERROR-TEXT                                                 
023500       CALL FELLOG                                                        
023600     END-IF                                                               
023700     .                                                                    
023800     EJECT                                                                
023900 IMS-STATUSCHECK SECTION.                                                 
024000     SKIP2                                                                
024100     SET STATUS-IX TO 1                                                   
024200     SEARCH GOOD-STATUS                                                   
024300       AT END                                                             
024400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
024500           DELIMITED BY SIZE INTO ERROR-TEXT                              
024600         DISPLAY ERROR-TEXT                                               
024700         CALL FELLOG                                                      
024800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
024900         CONTINUE                                                         
025000     END-SEARCH                                                           
025100     .                                                                    
