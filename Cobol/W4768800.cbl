000100************************************                                      
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4768800.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   24/07/09.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNCTION:                                                            
001000*        READ FILE WITH INVOICES TO BE DELETED FROM WDL5                  
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- FILE WITH POST THAT SHOULD BE DELETED                      
002100     SELECT W47687                     ASSIGN TO W47688D1.                
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W47687                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000                                                                          
003100*01  -COPY W47687      -L.                                                
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500 77  IDPGM                       PIC X(8)    VALUE 'W4768800'.            
003600 01  CHKP-VAR.                                                            
003700     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003800     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
003900     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004000     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004100     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004200     03 CHKP-MAX                 PIC S9(3)   VALUE +400 COMP-3.           
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004500     SKIP2                                                                
004600 01  ERROR-TEXT.                                                          
004700     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
004800     03  ERROR-TEXT-S            PIC X(70)   VALUE SPACE.                 
004900                                                                          
005000 77  W47687-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W47687                       VALUE 'Y'.                   
005200     EJECT                                                                
005300 01  GENERAL-SUBPROGRAMS.                                                 
005400*                                                                         
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005800     EJECT                                                                
005900*    --- PARAMETRAR TILL POSTSUM                                          
006000*                                                                         
006100*01  -COPY W0005   -PRE  POSTSUM-                                         
006200     EJECT                                                                
006300 01  IN-AREA-START               PIC X(24)   VALUE                        
006400                                             'IN-AREA-START'.             
006500     SKIP2                                                                
006600                                                                          
006700*01  AREA -COPY W47687     -PRE IN-                                       
006800*                                                                         
006900     EJECT                                                                
007000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007100     SKIP3                                                                
007200 01  KEYS-TILL-DLI.                                                       
007300                                                                          
007400     03 W-IDFAKT-X.                                                       
007500       05 W-IDFAKT               PIC S9(7)    COMP-3.                     
007600     SKIP2                                                                
007700*    --- STATUS-KOD FRÅN IMS                                              
007800 01  STATUS-WS                   PIC XX.                                  
007900     88  SEGMENT-FOUND                       VALUE '  '.                  
008000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008200     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
008300     88  IMS-NOT-OK                          VALUE 'XD'.                  
008400     SKIP2                                                                
008500 01  GOOD-STATUSCODES.                                                    
008600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008700     SKIP3                                                                
008800 01  SSA1                        PIC X(64).                               
008900 01  SSA2                        PIC X(64).                               
009000     EJECT                                                                
009100*    --- IMS FUNCTION CODES                                               
009200*01  -COPY W0003                                                          
009300     EJECT                                                                
009400*    ---  DLI INPUT-OUTPUT AREA                                           
009500                                                                          
009600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL501'.                      
009700 01  DLI-IO-WDL501.                                                       
009800*    03  -COPY WDL501                                                     
009900                                                                          
010000     EJECT                                                                
010100 LINKAGE SECTION.                                                         
010200                                                                          
010300*01  -COPY W0009   -PRE MSG-                                              
010400                                                                          
010500*01  -COPY W0008  -PRE WDL5-                                              
010600     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010800 PROCEDURE DIVISION  USING MSG-PCB WDL5-PCB.                              
010900 MAIN SECTION.                                                            
011000     ENTRY 'DLITCBL' USING MSG-PCB WDL5-PCB.                              
011100                                                                          
011200     PERFORM A-INIT                                                       
011300     PERFORM S01-READ-W47687                                              
011400     PERFORM UNTIL END-OF-W47687                                          
011500       IF CHKP-ANT > CHKP-MAX                                             
011600         PERFORM IMS-CHECKPOINT                                           
011700         MOVE ZERO TO CHKP-ANT                                            
011800       END-IF                                                             
011900                                                                          
012000       MOVE IN-IDFAKT   TO W-IDFAKT                                       
012100       PERFORM IMS-GHU-WDL501                                             
012200       IF SEGMENT-FOUND                                                   
012201         PERFORM IMS-DLET-WDL501                                          
012202         ADD 1 TO CHKP-ANT                                                
012210       ELSE                                                               
012220         DISPLAY 'FAKT= ' IN-IDFAKT ' REMOVED IN OTHER SHIPM'             
012300       END-IF                                                             
012600                                                                          
012700       PERFORM S01-READ-W47687                                            
012800     END-PERFORM                                                          
012900                                                                          
013000     PERFORM Z-FINIT                                                      
013100                                                                          
013200     MOVE ZERO TO RETURN-CODE                                             
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013700     SKIP2                                                                
013800                                                                          
013900     PERFORM IMS-RESTART                                                  
014000                                                                          
014100     OPEN INPUT W47687                                                    
014200                                                                          
014300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014400     .                                                                    
014500     EJECT                                                                
014600 Z-FINIT SECTION.                                                         
014700                                                                          
014800     CLOSE W47687                                                         
014900     SKIP2                                                                
015000     MOVE 'S' TO POSTSUM-OPKOD                                            
015100     CALL POSTSUM USING POSTSUM-PARM                                      
015200     .                                                                    
015300     EJECT                                                                
015400 S01-READ-W47687  SECTION.                                                
015500     SKIP2                                                                
015600     READ W47687 INTO IN-AREA                                             
015700     AT END                                                               
015800        MOVE HIGH-VALUE TO IN-AREA                                        
015900        SET END-OF-W47687 TO TRUE                                         
016000                                                                          
016100     NOT AT END                                                           
016200        MOVE 'W47687' TO POSTSUM-FDNAMN                                   
016300        MOVE 'W47688D1' TO POSTSUM-DDNAMN2                                
016400        MOVE SPACE TO POSTSUM-TRANSTYP                                    
016500        CALL POSTSUM USING POSTSUM-PARM                                   
016600     END-READ                                                             
016700     .                                                                    
016800     EJECT                                                                
016900* --- IMS SECTIONS  ---                                                   
017000                                                                          
017100     EJECT                                                                
017200 IMS-GHU-WDL501 SECTION.                                                  
017300                                                                          
017400     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
017500          DELIMITED BY SIZE INTO SSA1                                     
017600     MOVE 'GE  ' TO GOOD-STATUSCODES                                      
017700     CALL CBLTDLI USING GHU WDL5-PCB DLI-IO-WDL501 SSA1                   
017800     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
017900     PERFORM IMS-STATUSCHECK                                              
018000     .                                                                    
018100     EJECT                                                                
018200 IMS-DLET-WDL501 SECTION.                                                 
018300                                                                          
018400     MOVE '  ' TO GOOD-STATUSCODES                                        
018500     CALL CBLTDLI USING DLET WDL5-PCB DLI-IO-WDL501                       
018600     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
018700     PERFORM IMS-STATUSCHECK                                              
018800     .                                                                    
018900     EJECT                                                                
019000 IMS-RESTART SECTION.                                                     
019100     SKIP2                                                                
019200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019300     MOVE '  ' TO GOOD-STATUSCODES                                        
019400     CALL CBLTDLI USING XRST MSG-PCB                                      
019500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019600                        CHKP-AREA-LENGTH CHKP-AREA                        
019700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019800     PERFORM IMS-STATUSCHECK                                              
019900     .                                                                    
020000     SKIP3                                                                
020100 IMS-CHECKPOINT SECTION.                                                  
020200     SKIP2                                                                
020300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020400     MOVE '  XD' TO GOOD-STATUSCODES                                      
020500     CALL CBLTDLI USING CHKP MSG-PCB                                      
020600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020700                        CHKP-AREA-LENGTH CHKP-AREA                        
020800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020900     PERFORM IMS-STATUSCHECK                                              
021000                                                                          
021100     IF IMS-NOT-OK                                                        
021200       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO ERROR-TEXT-S        
021300       DISPLAY ERROR-TEXT                                                 
021400       CALL FELLOG                                                        
021500     END-IF                                                               
021600     .                                                                    
021700     EJECT                                                                
021800 IMS-STATUSCHECK SECTION.                                                 
021900     SKIP2                                                                
022000     SET STATUS-IX TO 1                                                   
022100     SEARCH GOOD-STATUS                                                   
022200       AT END                                                             
022300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022400           DELIMITED BY SIZE INTO ERROR-TEXT                              
022500         DISPLAY ERROR-TEXT                                               
022600         CALL FELLOG                                                      
022700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022800         CONTINUE                                                         
022900     END-SEARCH                                                           
023000     .                                                                    
