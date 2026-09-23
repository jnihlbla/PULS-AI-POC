000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4730300.                                                
000300 AUTHOR.         OLGRENER LASSI.                                          
000400 DATE-WRITTEN.   25/01/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        CLEANUP WDG202 (EVENT 4321)                                      
001000*                                                                         
001100     SKIP3                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP2                                                                
001400 INPUT-OUTPUT SECTION.                                                    
001500                                                                          
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800*          --- CLEANUP 4321/WDG202                                        
001900     SELECT W47303                     ASSIGN TO W47303D1.                
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200     SKIP3                                                                
002300 FILE SECTION.                                                            
002400     SKIP3                                                                
002500 FD  W47303                                                               
002600     RECORDING       F                                                    
002700     BLOCK CONTAINS  0.                                                   
002800                                                                          
002900*01  -COPY W47303      -L.                                                
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300 77  IDPGM                       PIC X(8)    VALUE 'W4730300'.            
003400 01  CHKP-VAR.                                                            
003500     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003600     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
003700     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
003800     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
003900     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004000     03 CHKP-MAX                 PIC S9(3)   VALUE +900 COMP-3.           
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300     SKIP2                                                                
004400 01  ERROR-TEXT.                                                          
004500     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
004600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
004700                                                                          
004800 77  W47303-EOF-SW               PIC X       VALUE 'N'.                   
004900     88  END-OF-W47303                       VALUE 'Y'.                   
005000     EJECT                                                                
005100 01  GENERAL-SUBPROGRAMS.                                                 
005200*                                                                         
005300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005600     EJECT                                                                
005700*    --- PARAMETRAR TILL POSTSUM                                          
005800*                                                                         
005900*01  -COPY W0005   -PRE  POSTSUM-                                         
006000     EJECT                                                                
006100 01  IN-AREA-START               PIC X(24)   VALUE 'IN-AREA'.             
006200*01  AREA -COPY W47303     -PRE IN-                                       
006300*                                                                         
006400     EJECT                                                                
006500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006600     SKIP3                                                                
006700 01  KEYS-TILL-DLI.                                                       
006800     03  W-WDGXKEY-X.                                                     
006900         05  W-IDHTYP            PIC X(4)    VALUE '4321'.                
007000         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
007100                                                                          
007200     03  W-IDPRODNR-X.                                                    
007300         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
007400     03  W-IDKOLLI-X.                                                     
007500         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
007600     SKIP2                                                                
007700*    --- STATUS-KOD FRÅN IMS                                              
007800 01  STATUS-WS                   PIC XX.                                  
007900     88  SEGMENT-FOUND                       VALUE '  '.                  
008000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008200     88  IMS-NOT-OK                          VALUE 'XD'.                  
008300     SKIP2                                                                
008400 01  GOOD-STATUSCODES.                                                    
008500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008600     SKIP3                                                                
008700 01  SSA1                        PIC X(64).                               
008800     EJECT                                                                
008900*    --- IMS FUNCTION CODES                                               
009000*01  -COPY W0003                                                          
009100     EJECT                                                                
009200*    ---  DLI INPUT-OUTPUT AREA                                           
009300                                                                          
009800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4322'.                    
009900 01  DLI-IO-WDGX4322.                                                     
010000*    03  -COPY WDGX4322                                                   
010100     EJECT                                                                
010200 LINKAGE SECTION.                                                         
010300                                                                          
010400*01  -COPY W0009   -PRE MSG-                                              
010500                                                                          
010600*01  -COPY W0008  -PRE 4321-                                              
010700     05  FILLER                  PIC X.                                   
010800     EJECT                                                                
010900 PROCEDURE DIVISION  USING MSG-PCB 4321-PCB.                              
011000 MAIN SECTION.                                                            
011100                                                                          
011200     PERFORM A-INIT                                                       
011300                                                                          
011400     PERFORM S01-READ-W47303                                              
011500     PERFORM UNTIL END-OF-W47303                                          
011600       IF CHKP-ANT > CHKP-MAX                                             
011700         PERFORM IMS-CHECKPOINT                                           
011800         MOVE ZERO TO CHKP-ANT                                            
011900       END-IF                                                             
012000                                                                          
012200       MOVE IN-IDPRODNR TO W-IDPRODNR                                     
012210       MOVE IN-IDKOLLI  TO W-IDKOLLI                                      
012300       PERFORM IMS-GHU-WDGX4322                                           
012400       IF SEGMENT-FOUND                                                   
012410         PERFORM IMS-DLET-WDGX4322                                        
012420         ADD +1           TO CHKP-ANT                                     
012500       ELSE                                                               
012600         DISPLAY 'NO 4322. DLET BY PACKN? ' W-IDPRODNR '/'                
012700                                            W-IDKOLLI                     
012800       END-IF                                                             
012901                                                                          
013200       PERFORM S01-READ-W47303                                            
013300     END-PERFORM                                                          
013400                                                                          
013500     PERFORM Z-FINIT                                                      
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014200     SKIP2                                                                
014300                                                                          
014400     OPEN INPUT W47303                                                    
014500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014510     PERFORM IMS-RESTART                                                  
014600     .                                                                    
014700     EJECT                                                                
014800 Z-FINIT SECTION.                                                         
014900                                                                          
015000                                                                          
015100     CLOSE W47303                                                         
015200     SKIP2                                                                
015300     MOVE 'S' TO POSTSUM-OPKOD                                            
015400     CALL POSTSUM USING POSTSUM-PARM                                      
015500     .                                                                    
015600     EJECT                                                                
015700 S01-READ-W47303  SECTION.                                                
015800     SKIP2                                                                
015900     READ W47303 INTO IN-AREA                                             
016000     AT END                                                               
016100        SET END-OF-W47303 TO TRUE                                         
016200                                                                          
016300     NOT AT END                                                           
016400        MOVE 'W47303' TO POSTSUM-FDNAMN                                   
016500        MOVE 'W47303D1' TO POSTSUM-DDNAMN2                                
016600        MOVE 'DLET'    TO POSTSUM-TRANSTYP                                
016700        CALL POSTSUM USING POSTSUM-PARM                                   
016800     END-READ                                                             
016900     .                                                                    
017000     EJECT                                                                
017100* --- IMS SECTIONS  ---                                                   
017200                                                                          
017300     EJECT                                                                
018400 IMS-GHU-WDGX4322 SECTION.                                                
018500                                                                          
018510     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
018520            DELIMITED BY SIZE INTO SSA1                                   
018600     STRING 'WDG202  (IDPRODNR =' W-IDPRODNR-X                            
018700                    '&IDKOLLI  =' W-IDKOLLI-X ')'                         
018800          DELIMITED BY SIZE INTO SSA1                                     
018900     MOVE 'GE  ' TO GOOD-STATUSCODES                                      
019000     CALL CBLTDLI USING GHU 4321-PCB DLI-IO-WDGX4322 SSA1                 
019100     MOVE 4321-STATUS-CODE TO STATUS-WS                                   
019200     PERFORM IMS-STATUSCHECK                                              
019300     .                                                                    
019400     SKIP3                                                                
019500 IMS-DLET-WDGX4322 SECTION.                                               
019600                                                                          
019700     MOVE '  ' TO GOOD-STATUSCODES                                        
019800     CALL CBLTDLI USING DLET 4321-PCB DLI-IO-WDGX4322                     
019900     MOVE 4321-STATUS-CODE TO STATUS-WS                                   
020000     PERFORM IMS-STATUSCHECK                                              
020100     .                                                                    
020200     EJECT                                                                
020300 IMS-RESTART SECTION.                                                     
020400     SKIP2                                                                
020500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020600     MOVE '  ' TO GOOD-STATUSCODES                                        
020700     CALL CBLTDLI USING XRST MSG-PCB                                      
020800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020900                        CHKP-AREA-LENGTH CHKP-AREA                        
021000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021100     PERFORM IMS-STATUSCHECK                                              
021200     .                                                                    
021300     SKIP3                                                                
021400 IMS-CHECKPOINT SECTION.                                                  
021500     SKIP2                                                                
021600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021700     MOVE '  XD' TO GOOD-STATUSCODES                                      
021800     CALL CBLTDLI USING CHKP MSG-PCB                                      
021900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022000                        CHKP-AREA-LENGTH CHKP-AREA                        
022100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022200     PERFORM IMS-STATUSCHECK                                              
022300                                                                          
022400     IF IMS-NOT-OK                                                        
022500       MOVE 'IMS CONTROL REGION NOT ACCESSIBLE' TO ERROR-TEXT-STR         
022600       DISPLAY ERROR-TEXT                                                 
022700       CALL FELLOG                                                        
022800     END-IF                                                               
022900     .                                                                    
023000 IMS-STATUSCHECK SECTION.                                                 
023100     SKIP2                                                                
023200     SET STATUS-IX TO 1                                                   
023300     SEARCH GOOD-STATUS                                                   
023400       AT END                                                             
023500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023600           DELIMITED BY SIZE INTO ERROR-TEXT                              
023700         DISPLAY ERROR-TEXT                                               
023800         CALL FELLOG                                                      
023900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
024000         CONTINUE                                                         
024100     END-SEARCH                                                           
024200     .                                                                    
