000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4054800.                                                
000300 AUTHOR.         THOMAS LARSSON                                           
000400 DATE-WRITTEN.   NOVEMBER 2021                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        API      UPDATE FLAG FROM MIC                                    
000900*                                                                         
001000*                                                                         
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W40548U                                             
001400*        MID:         W405481I1                                           
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W405481O1                                           
001800*                                                                         
001900*    STORY 2065580 ADD FIELDS TO W40538                                   
002000*                                                                         
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300 DATA DIVISION.                                                           
002400                                                                          
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700 77    IDPGM                     PIC X(8)    VALUE 'W4054800'.            
002800 77    CURRENT-SECTION           PIC X(16)   VALUE SPACE.                 
002900 77    CURRENT-IMS-SECTION       PIC X(16)   VALUE SPACE.                 
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77    ERROR-TEXT                PIC X(80) VALUE SPACE.                   
003200 77    FELTEXT                   PIC X(80) VALUE SPACE.                   
003300 77    KDRC-DISPLAY              PIC Z(5).                                
003400 77    RKOD-ABEND-NO-DUMP        PIC S9(4)   COMP VALUE +16.              
003500 77    RKOD-ABEND-WITH-DUMP      PIC S9(4)   COMP VALUE +1000.            
003600                                                                          
003700 77    JAA                       PIC X       VALUE 'J'.                   
003800 77    YES                       PIC X       VALUE 'Y'.                   
003900 77    NOO                       PIC X       VALUE 'N'.                   
004000 77    WS-IDFAKT                 PIC 9(7)    VALUE ZERO.                  
004100 77    WS-FLKLAR                 PIC X(1)    VALUE SPACE.                 
004200                                                                          
004300 77    OK-SW                     PIC X       VALUE 'Y'.                   
004400   88  EVERYTHING-OK                         VALUE 'Y'.                   
004500   88  SOMETHING-WRONG                       VALUE 'N'.                   
004600                                                                          
004700 77    STATUS-U-P-SW             PIC X       VALUE 'N'.                   
004800   88  ORDER-IN-STATUS-U-P                   VALUE 'Y'.                   
004900                                                                          
005000                                                                          
005100*      --- VALID IDDC CODES                                               
005200*                                                                         
005300*01   --COPY WWDC99                                                       
005400*01   --COPY WWDCKONS                                                     
005500                                                                          
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700   03  FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
005800   03  ABEND                     PIC X(8)   VALUE 'ABEND   '.             
005900   03  WZ01SUB                   PIC X(8)   VALUE 'WZ01SUB '.             
006000   03  CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
006100   03  WDATKONV                  PIC X(8)   VALUE 'WDATKONV'.             
006200   03  W005INIT                  PIC X(8)   VALUE 'W005INIT'.             
006300   03  WZ01SEND                  PIC X(8)   VALUE 'WZ01SEND'.             
006400*                                                                         
006500 01  FILLER                      PIC X(16)  VALUE 'SUB-CONTROL'.          
006600                                                                          
006700*01  -COPY WZ01SUB                                                        
006800*                                                                         
006900*                                                                         
007000*    --- PARAMETERS FOR WDATKONV                                          
007100*01  -COPY WDATAREA                                                       
007200                                                                          
007300 01 FILLER                       PIC X(8) VALUE 'W005INIT'.               
007400*   -COPY WMSGINIT                                                        
007500                                                                          
007600                                                                          
007700*    --- AREAS FOR SUB MODULES                                            
007800                                                                          
007900 01  FILLER                    PIC X(16) VALUE 'MSG-IO-AREA     '.        
008000*01  -COPY WMSGAREA                                                       
008100                                                                          
008200                                                                          
008300                                                                          
008400******************************************************************        
008500*                                                                         
008600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008900                                                                          
009000 01  REQU-AREA.                                                           
009100*    03  -COPY WZ01REQ2                                                   
009200*    03  -COPY W40548I1                                                   
009300                                                                          
009400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009500                                                                          
009600 01  RESP-AREA.                                                           
009700*    03  -COPY WZ01RESP                                                   
009800*    03  -COPY W40548O1                                                   
009900                                                                          
010000 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
010100 01  SEND-AREA.                                                           
010200*    03  -COPY WZ01SEND                                                   
010300     EJECT                                                                
010400 01  SEND-RAD.                                                            
010500   03  MAIL-RAD                PIC X(80)  VALUE SPACE.                    
010600                                                                          
010700                                                                          
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900 01  DLI-KEYS.                                                            
011000                                                                          
011100     03  W-WDM701KY-MIN-X.                                                
011200         05  W-IDFAKT-MIN        PIC S9(7)   VALUE ZERO COMP-3.           
011300         05  W-IDORDNR7-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
011400         05  W-IDKOLLI-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
011500         05  W-IDPRODNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
011600                                                                          
011700     03  W-WDM701KY-MAX-X.                                                
011800         05  W-IDFAKT-MAX        PIC S9(7)   VALUE ZERO COMP-3.           
011900         05  FILLER              PIC X(11)   VALUE HIGH-VALUE.            
012000                                                                          
012100                                                                          
012200 01  MESSAGE-CODES.                                                       
012300     03  SYS-ERROR               PIC X(3)    VALUE '099'.                 
012400     03  MICFLAG-UPDATED         PIC X(3)    VALUE '200'.                 
012500     03  BAD-REQUEST             PIC X(3)    VALUE '400'.                 
012600     03  NOT-FOUND               PIC X(3)    VALUE '404'.                 
012700     03  ALREADY-DELETED         PIC X(3)    VALUE '410'.                 
012800     03  DELETE-NOT-ALLOWED      PIC X(3)    VALUE '405'.                 
012900                                                                          
013000                                                                          
013100 01  STATUS-WS                   PIC XX.                                  
013200     88  SEGMENT-FOUND                       VALUE '  '.                  
013300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
013400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013500     88  END-OF-DATA                         VALUE 'GB'.                  
013600                                                                          
013700 01  GOOD-STATUSCODES.                                                    
013800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013900                                                                          
014000 01  ALL-SSA.                                                             
014100     03 SSA1                     PIC X(144).                              
014200     03 SSA2                     PIC X(120).                              
014300                                                                          
014400     EJECT                                                                
014500*    --- IMS FUNCTION CODES                                               
014600*01  -COPY W0003                                                          
014700                                                                          
014800     EJECT                                                                
014900 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDM701'.          
015000 01  DLI-IO-WDM701.                                                       
015100*    03  -COPY WDM701                                                     
015200                                                                          
015300                                                                          
015400                                                                          
015500 LINKAGE SECTION.                                                         
015600                                                                          
015700*01  -COPY W0009     -PRE MSG-                                            
015800                                                                          
015900*01  -COPY W0008     -PRE WDM7-                                           
016000     05  FILLER                  PIC X.                                   
016100                                                                          
016200                                                                          
016300                                                                          
016400 PROCEDURE DIVISION  USING MSG-PCB  WDM7-PCB.                             
016500                                                                          
016600 MAIN SECTION.                                                            
016700     ENTRY 'DLITCBL' USING MSG-PCB  WDM7-PCB.                             
016800                                                                          
016900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
017000     IF SUB-KDRC = 0                                                      
017100       IF REQU-KDPGMACT = 'E'                                             
017200                                                                          
017300         PERFORM A-INIT-SAVE-INPUT                                        
017400         PERFORM B-CHECK-INPUT                                            
017500                                                                          
017600         IF EVERYTHING-OK                                                 
017700           PERFORM C-UPDATE-FLAG-MIC                                      
017800         END-IF                                                           
017900       ELSE                                                               
018000         MOVE SYS-ERROR TO RESP-IDMSG-ERROR                               
018100       END-IF                                                             
018200                                                                          
018300       PERFORM S02-RETURN-RESPONSE                                        
018400     END-IF                                                               
018500                                                                          
018600     MOVE ZERO                         TO RETURN-CODE                     
018700                                                                          
018800     GOBACK                                                               
018900     .                                                                    
019000                                                                          
019100                                                                          
019200 A-INIT-SAVE-INPUT SECTION.                                               
019300     MOVE 'A-INIT-SAVE-INPUT' TO CURRENT-SECTION                          
019400                                                                          
019500     MOVE 001                        TO RESP-IDMSGVER                     
019600     MOVE SPACE                      TO RESP-IDMSG-ERROR                  
019700                                        RESP-IDMSG-INFO                   
019800                                        RESP-IDELMT-ERROR                 
019900     MOVE SPACE                      TO RESP-IDMFSINF                     
020000                                        RESP-TEMFSINF                     
020100     .                                                                    
020200                                                                          
020300                                                                          
020400 B-CHECK-INPUT SECTION.                                                   
020500     MOVE 'B-CHECK-INPUT  ' TO CURRENT-SECTION                            
020600                                                                          
020700     MOVE LOW-VALUE          TO W-WDM701KY-MIN-X                          
020800*    MOVE HIGH-VALUE         TO W-WDM701KY-MAX-X                          
020900                                                                          
021000     IF  REQU-IDFAKT   = ZERO  OR                                         
021100        REQU-IDFAKT      NOT NUMERIC  OR                                  
021200        REQU-FLKLAR      NOT = YES                                        
021300       MOVE NOO                  TO OK-SW                                 
021400       MOVE BAD-REQUEST          TO RESP-IDMFSINF                         
021500       MOVE 'BAD REQUEST  '      TO RESP-TEMFSINF                         
021600     ELSE                                                                 
021700       MOVE REQU-IDFAKT          TO W-IDFAKT-MIN                          
021800                                    W-IDFAKT-MAX                          
021900     END-IF                                                               
022000     .                                                                    
022100     EJECT                                                                
022200                                                                          
022300 C-UPDATE-FLAG-MIC     SECTION.                                           
022400     MOVE 'C-UPDATE-FLAG-MIC ' TO CURRENT-SECTION                         
022500                                                                          
022600     PERFORM IMS-GHN-WDM701                                               
022700     IF SEGMENT-FOUND                                                     
022800       PERFORM UNTIL END-OF-DATA OR SEGMENT-MISSING                       
022900         MOVE REQU-FLKLAR        TO HUV-FLKLAR                            
023000                                                                          
023100         PERFORM IMS-REPL-WDM701                                          
023200                                                                          
023300         PERFORM IMS-GHN-WDM701                                           
023400       END-PERFORM                                                        
023500                                                                          
023600       PERFORM G-CREATE-RESPONSE-UPDATED                                  
023700     ELSE                                                                 
023800        MOVE NOO                 TO OK-SW                                 
023900        MOVE NOT-FOUND           TO RESP-IDMFSINF                         
024000        STRING 'INVOICE NOT FOUND'                                        
024100        DELIMITED BY SIZE INTO RESP-TEMFSINF                              
024200     END-IF                                                               
024300     .                                                                    
024400                                                                          
024500 G-CREATE-RESPONSE-UPDATED SECTION.                                       
024600     MOVE 'G-CREATE-RESPONSE ' TO CURRENT-SECTION                         
024700                                                                          
024800     MOVE MICFLAG-UPDATED    TO RESP-IDMFSINF                             
024900     MOVE 'MIC FLAG UPDATED' TO RESP-TEMFSINF                             
025000     .                                                                    
025100                                                                          
025200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
025300                                                                          
025400     MOVE 'GETARG'               TO SUB-KDFUNC                            
025500     MOVE 'CARPARTS.FLS.EXPORTSHIPMENTRELEASE' TO SUB-ADDISPABS           
025600     MOVE SPACE TO REQU-AREA                                              
025700     MOVE LENGTH OF REQU-AREA         TO SUB-KVDLEN                       
025800                                                                          
025900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
026000                                                                          
026100     IF SUB-KDRC > 0                                                      
026200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
026300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
026400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
026500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
026600     END-IF                                                               
026700     .                                                                    
026800     SKIP3                                                                
026900 S02-RETURN-RESPONSE SECTION.                                             
027000                                                                          
027100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
027200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
027300                                                                          
027400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
027500                                                                          
027600     IF SUB-KDRC > 0                                                      
027700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
027800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
027900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
028000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
028100     END-IF                                                               
028200     .                                                                    
028300                                                                          
028400 IMS-GHN-WDM701   SECTION.                                                
028500                                                                          
028600     MOVE 'IMS-GHN-WDM701'     TO CURRENT-IMS-SECTION                     
028700                                                                          
028800     STRING 'WDM701  (WDM701KY=>' W-WDM701KY-MIN-X                        
028900                    '&WDM701KY=<' W-WDM701KY-MAX-X ')'                    
029000          DELIMITED BY SIZE INTO SSA1                                     
029100                                                                          
029200     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
029300     CALL CBLTDLI USING GHN WDM7-PCB DLI-IO-WDM701 SSA1                   
029400     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
029500                                                                          
029600     PERFORM IMS-STATUSCHECK                                              
029700     .                                                                    
029800     EJECT                                                                
029900 IMS-REPL-WDM701 SECTION.                                                 
030000                                                                          
030100     MOVE 'IMS-REPL-WDM701      '  TO CURRENT-IMS-SECTION                 
030200                                                                          
030300     MOVE '  ' TO GOOD-STATUSCODES                                        
030400     CALL CBLTDLI USING REPL WDM7-PCB DLI-IO-WDM701                       
030500     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
030600     PERFORM IMS-STATUSCHECK                                              
030700     .                                                                    
030800     EJECT                                                                
030900                                                                          
031000 IMS-STATUSCHECK SECTION.                                                 
031100                                                                          
031200     SET STATUS-IX                 TO 1                                   
031300     SEARCH GOOD-STATUS                                                   
031400       AT END                                                             
031500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
031600         DELIMITED BY SIZE INTO ERROR-TEXT                                
031700         CALL FELLOG                                                      
031800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
031900         CONTINUE                                                         
032000     END-SEARCH                                                           
032100     .                                                                    
