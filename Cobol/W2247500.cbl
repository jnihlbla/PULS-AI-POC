000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2247500.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   16/12/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        CLEAR STOP DATE FOR ALARM 223 ON WDK711                          
001000*                                                                         
001100*        THE PROGRAM UPDATE    WDK711                                     
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- UTDRAG UR WDK7                                             
002200     SELECT W01184                     ASSIGN TO W22475D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W01184                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W01184      -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W2247500'.            
003610 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
003620 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
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
005100 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
005200     88  END-OF-W01184                       VALUE 'Y'.                   
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
007200     SKIP2                                                                
007300                                                                          
007400*01  AREA -COPY W01184     -PRE IN-                                       
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  KEYS-TILL-DLI.                                                       
008000     03  W-IDARTNR-X.                                                     
008100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008200                                                                          
008210     03  W-IDDC-X.                                                        
008220         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008500                                                                          
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FOUND                       VALUE '  '.                  
008900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009100     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
009200     88  IMS-NOT-OK                          VALUE 'XD'.                  
009300     SKIP2                                                                
009400 01  GOOD-STATUSCODES.                                                    
009500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009600     SKIP3                                                                
009700 01  SSA1                        PIC X(64).                               
009800 01  SSA2                        PIC X(64).                               
009900     EJECT                                                                
010000*    --- IMS FUNCTION CODES                                               
010100*01  -COPY W0003                                                          
010200     EJECT                                                                
010300*    ---  DLI INPUT-OUTPUT AREA                                           
010400                                                                          
010500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
010600 01  DLI-IO-WDK711.                                                       
010700*    03  -COPY WDK711                                                     
010800                                                                          
010900     EJECT                                                                
011000 LINKAGE SECTION.                                                         
011100                                                                          
011200*01  -COPY W0009   -PRE MSG-                                              
011300                                                                          
011400*01  -COPY W0008  -PRE WDK7-                                              
011500     05  FILLER                  PIC X.                                   
011600     EJECT                                                                
011700 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB.                              
011800 MAIN SECTION.                                                            
011900     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB.                              
012000                                                                          
012100     PERFORM A-INIT                                                       
012200     PERFORM S01-READ-W01184                                              
012300     PERFORM UNTIL END-OF-W01184                                          
012310                                                                          
012400       IF CHKP-ANT > CHKP-MAX                                             
012500         PERFORM X-TAKE-CHECKPOINT                                        
012600       END-IF                                                             
012700                                                                          
012800       IF IN-SLAG-TISTODAT-LARM > +0                                      
012810      AND IN-SLAG-TISTODAT-LARM < TODAYS-DATE                             
012900          MOVE IN-SLAG-IDARTNR TO W-IDARTNR                               
012910          MOVE IN-SLAG-IDDC    TO W-IDDC                                  
013000          PERFORM IMS-GHU-WDK711                                          
013100          IF SEGMENT-FOUND                                                
013200             MOVE +0           TO SLAG-TISTODAT-LARM                      
013300             PERFORM IMS-REPL-WDK711                                      
013400          END-IF                                                          
013500       END-IF                                                             
013600                                                                          
013700       PERFORM S01-READ-W01184                                            
013800     END-PERFORM                                                          
013900                                                                          
014000                                                                          
014100     PERFORM Z-FINIT                                                      
014200                                                                          
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500     .                                                                    
014600     EJECT                                                                
014700 A-INIT SECTION.                                                          
014800     SKIP2                                                                
014900                                                                          
015000     PERFORM IMS-RESTART                                                  
015100                                                                          
015200     OPEN INPUT W01184                                                    
015300                                                                          
015400     ACCEPT TODAYS-DATE FROM DATE                                         
015500                                                                          
015600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015700     .                                                                    
015800     EJECT                                                                
015900 Z-FINIT SECTION.                                                         
016000                                                                          
016100                                                                          
016200     CLOSE W01184                                                         
016300     SKIP2                                                                
016400     MOVE 'S' TO POSTSUM-OPKOD                                            
016500     CALL POSTSUM USING POSTSUM-PARM                                      
016600     .                                                                    
016700     EJECT                                                                
016800 S01-READ-W01184  SECTION.                                                
016900     SKIP2                                                                
017000     READ W01184        INTO IN-AREA                                      
017100     AT END                                                               
017300        SET END-OF-W01184 TO TRUE                                         
017400                                                                          
017500     NOT AT END                                                           
017600        MOVE 'W01184'     TO POSTSUM-FDNAMN                               
017700        MOVE 'W22475D1'   TO POSTSUM-DDNAMN2                              
017800        MOVE SPACE        TO POSTSUM-TRANSTYP                             
017900        CALL POSTSUM   USING POSTSUM-PARM                                 
018000                                                                          
018200     END-READ                                                             
018300     .                                                                    
018400     EJECT                                                                
019510 IMS-GHU-WDK711 SECTION.                                                  
019520     MOVE 'IMS-GHU-WDK711  ' TO DBS-SECTION                               
019530                                                                          
019550     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
019560          DELIMITED BY SIZE INTO SSA1                                     
019570     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
019580          DELIMITED BY SIZE INTO SSA2                                     
019590     MOVE '  '                TO GOOD-STATUSCODES                         
019591     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
019592     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
019593     PERFORM IMS-STATUSCHECK                                              
019594     .                                                                    
019600                                                                          
019700 IMS-REPL-WDK711 SECTION.                                                 
019710     MOVE 'IMS-REPL-WDK711 ' TO DBS-SECTION                               
019800                                                                          
019900     MOVE '  '             TO GOOD-STATUSCODES                            
020000     CALL CBLTDLI  USING REPL WDK7-PCB DLI-IO-WDK711                      
020100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
020200     PERFORM IMS-STATUSCHECK                                              
020300     ADD +2 TO CHKP-ANT                                                   
020400     .                                                                    
020500     EJECT                                                                
020600 X-TAKE-CHECKPOINT   SECTION.                                             
020700                                                                          
020800* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
020900* --- SAVE DATABASE KEYS IF NECESSARY                                     
021000     PERFORM IMS-CHECKPOINT                                               
021100     MOVE ZERO TO CHKP-ANT                                                
021200* --- REREAD DATABASE IF NECESSARY                                        
021300     .                                                                    
021400     EJECT                                                                
021500* --- IMS SECTIONS  ---                                                   
021600                                                                          
021700     EJECT                                                                
021800 IMS-RESTART SECTION.                                                     
021900     SKIP2                                                                
022000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
022100     MOVE '  ' TO GOOD-STATUSCODES                                        
022200     CALL CBLTDLI USING XRST MSG-PCB                                      
022300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022400                        CHKP-AREA-LENGTH CHKP-AREA                        
022500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022600     PERFORM IMS-STATUSCHECK                                              
022700     .                                                                    
022800     SKIP3                                                                
022900 IMS-CHECKPOINT SECTION.                                                  
023000     SKIP2                                                                
023100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
023200     MOVE '  XD' TO GOOD-STATUSCODES                                      
023300     CALL CBLTDLI USING CHKP MSG-PCB                                      
023400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
023500                        CHKP-AREA-LENGTH CHKP-AREA                        
023600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023700     PERFORM IMS-STATUSCHECK                                              
023800                                                                          
023900     IF IMS-NOT-OK                                                        
024000       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
024100                                    TO ERROR-TEXT-STR                     
024200       DISPLAY ERROR-TEXT                                                 
024300       CALL FELLOG                                                        
024400     END-IF                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 IMS-STATUSCHECK SECTION.                                                 
024800     SKIP2                                                                
024900     SET STATUS-IX TO 1                                                   
025000     SEARCH GOOD-STATUS                                                   
025100       AT END                                                             
025200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
025300           DELIMITED BY SIZE INTO ERROR-TEXT                              
025400         DISPLAY ERROR-TEXT                                               
025500         CALL FELLOG                                                      
025600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
025700         CONTINUE                                                         
025800     END-SEARCH                                                           
025900     .                                                                    
