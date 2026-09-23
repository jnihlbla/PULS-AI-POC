000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF026800.                                                
000300 AUTHOR.         LUNDH BERNT.                                             
000400 DATE-WRITTEN.   02/03/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:                                                                
000800*        CARPARTS.BILLIT.RECCOUNTRYMAINTENANCE                            
000900*    FUNCTION:                                                            
001000*        READ/UPDATE/INSERT/DELETE RECEIVING COUNTRY TABLE T01RECO        
001100*        DEPENDING ON REQUESTED PROGRAMS ACTION CODE (KDPGMACT)           
001200*        KDPGMACT = 'S' READ                                              
001300*        KDPGMACT = 'U' UPDATE                                            
001400*        KDPGMACT = 'I' INSERT                                            
001500*        KDPGMACT = 'D' DELETE                                            
001600*                                                                         
001700*        THE PROGRAM READS   TABLE T01LSEL                                
001800*        THE PROGRAM READS   TABLE T01INRE                                
001810*        THE PROGRAM READS   TABLE T01COCO                                
001900*        THE PROGRAM UPDATES TABLE T01RECO                                
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: WF0268U                                             
002300*        REQUEST:     WF0268I1                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        RESPONSE:    WF0268O1                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'WF026800'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004300 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600*    --- CONSTANT WORK FIELDS                                             
004700 77  YES                         PIC X       VALUE 'Y'.                   
004800 77  NOO                         PIC X       VALUE 'N'.                   
004900 77  WS-ADRESS                   PIC X(50)                                
005000                    VALUE 'CARPARTS.BILLIT.RECCOUNTRYMAINTENANCE'.        
005100 77  WS-CURRENT                  PIC S9(3)   VALUE +001    COMP-3.        
005200 77  WS-COMING                   PIC S9(3)   VALUE +002    COMP-3.        
005300 77  WS-ACTIVE                   PIC X(8)    VALUE '00000000'.            
005310 77  WS-DATE-FORMAT              PIC X(8)    VALUE 'YYYYMMDD'.            
005400                                                                          
005500 77  KEYS-SW                     PIC X       VALUE SPACE.                 
005600     88  KEYS-OK                             VALUE 'Y'.                   
005700     88  KEYS-WRONG                          VALUE 'N'.                   
005800                                                                          
005900 77  ACTION-CODE-SW              PIC X       VALUE SPACE.                 
006000     88  ACT-CODE-VALID                 VALUE 'S', 'U', 'I', 'D'.         
006100     88  ACT-CODE-SEARCH                     VALUE 'S'.                   
006200     88  ACT-CODE-UPDATE                     VALUE 'U'.                   
006300     88  ACT-CODE-INSERT                     VALUE 'I'.                   
006400     88  ACT-CODE-DELETE                     VALUE 'D'.                   
006500                                                                          
006600*    --- OTHER MAPPING-FIELDS THAN COPYTEXT WF0268O1                      
006700 01  MAP-KDSTATUS                PIC S9(3)   VALUE ZERO COMP-3.           
006800 01  MAP-DAREGDAT                PIC X(8)    VALUE SPACE.                 
006900 01  MAP-DAUPPDAT                PIC X(8)    VALUE SPACE.                 
007000 01  MAP-DADELDAT                PIC X(8)    VALUE SPACE.                 
007100                                                                          
007200*    --- WORK-FIELDS                                                      
007300 01  WS-FLCONTROL                PIC X       VALUE SPACE.                 
007400 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
007500 01  WS-SUDOCLIM                 PIC S9(11)  COMP-3 VALUE ZERO.           
007510 01  WS-SUDOCLIM-2               PIC 9(11)   VALUE ZERO.                  
007600                                                                          
007700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007800 01  GENERAL-SUBPROGRAMS.                                                 
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008010     03  WZ20DATE                PIC X(8)    VALUE 'WZ20DATE'.            
008100     SKIP3                                                                
008200                                                                          
008300*    --- PARAMETERS TO ABEND                                              
008400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008700 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
008800                                                                          
008900 01  MESSAGE-CODES.                                                       
009000     03  ERROR-CODES.                                                     
009100         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
009200         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
009300         05  ERR-DELETE-NOT-ALLOWED  PIC X(3)    VALUE '009'.             
009400         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
009410         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
009500         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
009510         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
009600         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
009700         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
009800         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
009810         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
009900     03  INFO-CODES.                                                      
010000         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
010100         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
010200         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
010300         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
010400*                                                                         
010500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
010600     SKIP3                                                                
010700 01  -COPY WZ01SUB                                                        
010710     EJECT                                                                
010720 01  FILLER                      PIC X(16)   VALUE 'DATE-CONTROL'.        
010721     SKIP3                                                                
010730 01  -COPY WZ20DATE                                                       
010740     EJECT                                                                
010900*                                                                         
011000 01  FILLER                      PIC X(16)   VALUE 'MAPPING-AREA'.        
011100     SKIP3                                                                
011200 01  -COPY WF0268O1  -PRE MAP-                                            
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011500     SKIP3                                                                
011600 01  REQU-AREA.                                                           
011700*    03  -COPY WZ01REQU                                                   
011800*    03  -COPY WF0268I1                                                   
011900     EJECT                                                                
012000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012100     SKIP3                                                                
012200 01  RESP-AREA.                                                           
012300*    03  -COPY WZ01RESP                                                   
012400*    03  -COPY WF0268O1                                                   
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
012700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
012800                                                                          
012900 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
013000 01  DB2-WS.                                                              
013100     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
013200         88  CURSOR-OK                       VALUE 000.                   
013300         88  LINES-FOUND                     VALUE 000.                   
013400         88  LINES-MISSING                   VALUE 100.                   
013410         88  NULLVALUE-INTO-HOST             VALUE 305.                   
013500         88  RESOURCE-WRONG                  VALUE 904.                   
013600     03  GOOD-SQLCODECODES.                                               
013700         05  GOOD-SQLCODE OCCURS 5                                        
013800             INDEXED BY SQLCODE-IX PIC 9(3).                              
013900                                                                          
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
014300*01  -COPY T01LSEL -PRE T01LSEL-                                          
014400     EJECT                                                                
014500 01  FILLER                      PIC X(16)   VALUE 'T01RECO-AREA'.        
014700*01  -COPY T01RECO -PRE T01RECO-                                          
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'T01INRE-AREA'.        
015100*01  -COPY T01INRE -PRE T01INRE-                                          
015200     EJECT                                                                
015210 01  FILLER                      PIC X(16)   VALUE 'T01COCO-AREA'.        
015220*01  -COPY T01COCO -PRE T01COCO-                                          
015230     EJECT                                                                
015300     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
015400     EJECT                                                                
015500     EXEC SQL INCLUDE T01RECO END-EXEC.                                   
015600     EJECT                                                                
015700     EXEC SQL INCLUDE T01INRE END-EXEC.                                   
015800     EJECT                                                                
015810     EXEC SQL INCLUDE T01COCO END-EXEC.                                   
015820     EJECT                                                                
015900 LINKAGE SECTION.                                                         
016000     EJECT                                                                
016100 PROCEDURE DIVISION.                                                      
016200 MAIN SECTION.                                                            
016300                                                                          
016400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
016500     IF SUB-KDRC = 0                                                      
016600       PERFORM A-INIT                                                     
016700       PERFORM B-CHECK-KEYS                                               
016800       IF KEYS-OK                                                         
016900         PERFORM D-VALIDATE-REQUEST                                       
017000       END-IF                                                             
017001       IF KEYS-OK                                                         
017002         PERFORM F-READ-SHOW-INFO                                         
017003       END-IF                                                             
017010       IF KEYS-WRONG                                                      
017020         PERFORM S04-MOVE-MISSING-TO-RESPOND                              
017030       END-IF                                                             
017100       PERFORM S02-RETURN-RESPONSE                                        
017200     END-IF                                                               
017300                                                                          
017400     MOVE ZERO TO RETURN-CODE                                             
017500     GOBACK                                                               
017600     .                                                                    
017700 A-INIT SECTION.                                                          
017800                                                                          
017900     INITIALIZE GOOD-SQLCODECODES                                         
018000     MOVE ALL '+' TO RESP-AREA                                            
018010     MOVE SPACE TO RESP-IDMSG-ERROR                                       
018020     MOVE SPACE TO RESP-IDMSG-INFO                                        
018030     MOVE SPACE TO RESP-IDELMT-ERROR                                      
018100     INITIALIZE MAP-RESP-WF0268O1                                         
018200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
018300     .                                                                    
018400*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
018500 B-CHECK-KEYS SECTION.                                                    
018600                                                                          
018700     MOVE YES TO KEYS-SW                                                  
018800     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
018900                                                                          
019000     IF REQU-KDSTATUS-KEY NUMERIC                                         
019001     AND REQU-IDMSGVER NUMERIC                                            
019010       IF REQU-IDLEGSEL-KEY > SPACE                                       
019100       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
019200       AND REQU-IDLANDX3-KEY > SPACE                                      
019300       AND REQU-IDLANDX3-KEY NOT = ALL '+'                                
019400       AND ACT-CODE-VALID                                                 
019500       AND (REQU-KDSTATUS-KEY = WS-CURRENT OR WS-COMING)                  
019600         CONTINUE                                                         
020200       ELSE                                                               
020300         MOVE NOO TO KEYS-SW                                              
020400       END-IF                                                             
020410     ELSE                                                                 
020420       MOVE NOO TO KEYS-SW                                                
020430     END-IF                                                               
020440                                                                          
020450     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
020460       MOVE NOO TO KEYS-SW                                                
020470     END-IF                                                               
020500                                                                          
020600     IF KEYS-WRONG                                                        
020700       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
020701       IF REQU-IDMSGVER NUMERIC                                           
020702         CONTINUE                                                         
020703       ELSE                                                               
020704         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
020705         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
020706       END-IF                                                             
020710       IF ACT-CODE-VALID                                                  
020720         CONTINUE                                                         
020730       ELSE                                                               
020740         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
020750         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
020760       END-IF                                                             
020770       IF REQU-IDUSER = SPACE OR = ALL '+'                                
020780         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
020790         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
020791       END-IF                                                             
020800     END-IF                                                               
020810     IF KEYS-OK                                                           
020820       PERFORM DB2-SELECT-T01COCO-TAB                                     
020830       IF LINES-FOUND                                                     
020840         CONTINUE                                                         
020850       ELSE                                                               
020860         MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                       
020870         MOVE 'IDLANDX3'        TO RESP-IDELMT-ERROR                      
020880         MOVE NOO TO KEYS-SW                                              
020890       END-IF                                                             
020891     END-IF                                                               
020892     IF KEYS-OK                                                           
020893       PERFORM DB2-SELECT-T01LSEL-TAB                                     
020894       IF LINES-FOUND                                                     
020895         CONTINUE                                                         
020896       ELSE                                                               
020897         MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                       
020898         MOVE 'IDLEGSEL'        TO RESP-IDELMT-ERROR                      
020899         MOVE NOO TO KEYS-SW                                              
020900       END-IF                                                             
020901     END-IF                                                               
020910     .                                                                    
020920 D-VALIDATE-REQUEST SECTION.                                              
020921     IF REQU-KDPGMACT = 'U' OR = 'I'                                      
020922       IF REQU-SUDOCLIM NUMERIC                                           
020923         MOVE REQU-SUDOCLIM       TO WS-SUDOCLIM                          
020924       ELSE                                                               
020925         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
020926         MOVE 'SUDOCLIM'          TO RESP-IDELMT-ERROR                    
020927         MOVE NOO TO KEYS-SW                                              
020928       END-IF                                                             
020929     END-IF                                                               
020991     .                                                                    
021000*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
021100 F-READ-SHOW-INFO SECTION.                                                
021200                                                                          
021300     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
021400     MOVE REQU-IDLANDX3-KEY  TO RESP-IDLANDX3-KEY                         
021500     MOVE REQU-KDSTATUS-KEY  TO RESP-KDSTATUS-KEY                         
021600     MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                           
021700                                                                          
021800     PERFORM FA-READ-BASICDATA                                            
021900     .                                                                    
022000*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
022100 FA-READ-BASICDATA SECTION.                                               
022200                                                                          
022300     IF ACT-CODE-SEARCH                                                   
022400       PERFORM FAA-SEARCH-T01RECO                                         
022500     ELSE                                                                 
022600       IF ACT-CODE-UPDATE                                                 
022700         PERFORM FAB-UPDATE-T01RECO                                       
022800       ELSE                                                               
022900         IF ACT-CODE-INSERT                                               
023000           PERFORM FAC-INSERT-T01RECO                                     
023100         ELSE                                                             
023200           IF ACT-CODE-DELETE                                             
023300             PERFORM FAD-DELETE-T01RECO                                   
023400           END-IF                                                         
023500         END-IF                                                           
023600       END-IF                                                             
023700     END-IF                                                               
023800     .                                                                    
023900*** - SEARCH FOR RIGHT RECEIVING COUNTRY AND MARK CURRENT LINE            
024000*** - IF COMING LINE EXIST.                                               
024100 FAA-SEARCH-T01RECO SECTION.                                              
024200                                                                          
024300     MOVE NOO TO WS-FLCONTROL                                             
024400                                                                          
024500     PERFORM DB2-DCL-OPN-T01RECO-CRS-1                                    
024600     PERFORM DB2-FETCH-T01RECO-CRS-1                                      
024700                                                                          
024800     IF LINES-FOUND                                                       
024900       PERFORM UNTIL LINES-MISSING                                        
025000         IF REQU-KDSTATUS-KEY = WS-CURRENT                                
025100           IF MAP-KDSTATUS = WS-CURRENT                                   
025200             PERFORM S03-MOVE-TO-RESPOND                                  
025300             MOVE YES TO WS-FLCONTROL                                     
025400           ELSE                                                           
025500             IF MAP-KDSTATUS = WS-COMING                                  
025600               IF WS-FLCONTROL = YES                                      
025700                 MOVE YES TO RESP-FLCOMING                                
025800                 MOVE NOO TO WS-FLCONTROL                                 
025900               END-IF                                                     
026000             END-IF                                                       
026100           END-IF                                                         
026200         ELSE                                                             
026300           IF REQU-KDSTATUS-KEY = WS-COMING                               
026400             IF MAP-KDSTATUS = WS-COMING                                  
026410               PERFORM S03-MOVE-TO-RESPOND                                
026420               MOVE SPACE   TO RESP-IDMSG-ERROR                           
026600             ELSE                                                         
026610               MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                 
026620               MOVE 'IDLANDX3'        TO RESP-IDELMT-ERROR                
026640               PERFORM S04-MOVE-MISSING-TO-RESPOND                        
027000             END-IF                                                       
027100           END-IF                                                         
027200         END-IF                                                           
027300         PERFORM DB2-FETCH-T01RECO-CRS-1                                  
027400       END-PERFORM                                                        
027500     ELSE                                                                 
027510       MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                         
027520       MOVE 'IDLANDX3'        TO RESP-IDELMT-ERROR                        
027530       MOVE NOO TO KEYS-SW                                                
027700     END-IF                                                               
027800                                                                          
027900     PERFORM DB2-CLOSE-T01RECO-CRS-1                                      
028000     .                                                                    
028100*** - CHECK IF UPDATE IS ON CURRENT OR COMING LINE                        
028200 FAB-UPDATE-T01RECO SECTION.                                              
028300                                                                          
028400     PERFORM FABA-CHECK-UPDATE-DATA                                       
028500                                                                          
028600     IF RESP-IDMSG-ERROR = SPACE                                          
028700       IF REQU-KDSTATUS-KEY = WS-CURRENT                                  
028800         PERFORM FABB-UPD-CURRENT                                         
028900       ELSE                                                               
029000         PERFORM FABC-UPD-COMING                                          
029100       END-IF                                                             
029200     END-IF                                                               
029300     .                                                                    
029400*** - VALIDATE REQUESTED FIELDS FOR UPDATE ON RECEIVING COUNTRY           
029500 FABA-CHECK-UPDATE-DATA SECTION.                                          
029600                                                                          
030500     IF RESP-IDMSG-ERROR = SPACE                                          
030600        IF REQU-IDSPRAK = 'EN'                                            
030800          CONTINUE                                                        
030900        ELSE                                                              
031000          MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                      
031100          MOVE 'IDSPRAK'         TO RESP-IDELMT-ERROR                     
031200        END-IF                                                            
031300     END-IF                                                               
031400                                                                          
031500     IF RESP-IDMSG-ERROR = SPACE                                          
031600        IF REQU-BERESPRA-1 > SPACE                                        
031700        AND REQU-BERESPRA-1 NOT = ALL '+'                                 
031800          CONTINUE                                                        
031900        ELSE                                                              
032000          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
032100          MOVE 'BERESPRA-1' TO RESP-IDELMT-ERROR                          
032200        END-IF                                                            
032300     END-IF                                                               
032310                                                                          
032320     IF RESP-IDMSG-ERROR = SPACE                                          
032330       IF REQU-BERESPRA-2 = ALL '+'                                       
032340         MOVE SPACE TO REQU-BERESPRA-2                                    
032350       END-IF                                                             
032360     END-IF                                                               
032400                                                                          
032500     IF RESP-IDMSG-ERROR = SPACE                                          
032600        IF REQU-ADRESP-STREET > SPACE                                     
032700        AND REQU-ADRESP-STREET NOT = ALL '+'                              
032800          CONTINUE                                                        
032900        ELSE                                                              
033000          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
033100          MOVE 'ADRESP-STREET' TO RESP-IDELMT-ERROR                       
033200        END-IF                                                            
033300     END-IF                                                               
033400                                                                          
033500     IF RESP-IDMSG-ERROR = SPACE                                          
033510       IF REQU-ADRESP-BOX > SPACE                                         
033520       AND REQU-ADRESP-BOX NOT = ALL '+'                                  
033530         CONTINUE                                                         
033540       ELSE                                                               
033550         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
033560         MOVE 'ADRESP-BOX' TO RESP-IDELMT-ERROR                           
033570       END-IF                                                             
033900     END-IF                                                               
034000                                                                          
034100     IF RESP-IDMSG-ERROR = SPACE                                          
034200        IF REQU-ADRESP-CITY > SPACE                                       
034300        AND REQU-ADRESP-CITY NOT = ALL '+'                                
034400          CONTINUE                                                        
034500        ELSE                                                              
034600          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
034700          MOVE 'ADRESP-CITY' TO RESP-IDELMT-ERROR                         
034800        END-IF                                                            
034900     END-IF                                                               
036000                                                                          
036100     IF RESP-IDMSG-ERROR = SPACE                                          
036200        IF REQU-IDTFN > SPACE                                             
036300        AND REQU-IDTFN NOT = ALL '+'                                      
036400          CONTINUE                                                        
036500        ELSE                                                              
036600          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
036700          MOVE 'IDTFN' TO RESP-IDELMT-ERROR                               
036800        END-IF                                                            
036900     END-IF                                                               
037000                                                                          
037100     IF RESP-IDMSG-ERROR = SPACE                                          
037200        IF REQU-IDTFX > SPACE                                             
037300        AND REQU-IDTFX NOT = ALL '+'                                      
037400          CONTINUE                                                        
037500        ELSE                                                              
037600          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
037700          MOVE 'IDTFX' TO RESP-IDELMT-ERROR                               
037800        END-IF                                                            
037900     END-IF                                                               
038000                                                                          
038100     IF RESP-IDMSG-ERROR = SPACE                                          
038200        IF REQU-IDMAIL > SPACE                                            
038300        AND REQU-IDMAIL NOT = ALL '+'                                     
038400          CONTINUE                                                        
038500        ELSE                                                              
038600          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
038700          MOVE 'IDMAIL' TO RESP-IDELMT-ERROR                              
038800        END-IF                                                            
038900     END-IF                                                               
039000                                                                          
039100     IF RESP-IDMSG-ERROR = SPACE                                          
039200        IF REQU-BECONT > SPACE                                            
039300        AND REQU-BECONT NOT = ALL '+'                                     
039400          CONTINUE                                                        
039500        ELSE                                                              
039600          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
039700          MOVE 'BECONT' TO RESP-IDELMT-ERROR                              
039800        END-IF                                                            
039900     END-IF                                                               
040000                                                                          
040100     IF RESP-IDMSG-ERROR = SPACE                                          
040200        IF REQU-IDVAT > SPACE                                             
040300        AND REQU-IDVAT NOT = ALL '+'                                      
040400          CONTINUE                                                        
040500        ELSE                                                              
040600          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
040700          MOVE 'IDVAT' TO RESP-IDELMT-ERROR                               
040800        END-IF                                                            
040900     END-IF                                                               
040910                                                                          
040911     IF RESP-IDMSG-ERROR = SPACE                                          
040912        IF REQU-IDVAT-AGENT > SPACE                                       
040913        AND REQU-IDVAT-AGENT NOT = ALL '+'                                
040914          CONTINUE                                                        
040915        ELSE                                                              
040916          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
040917          MOVE 'IDVAT' TO RESP-IDELMT-ERROR                               
040918        END-IF                                                            
040919     END-IF                                                               
040920                                                                          
040921     IF RESP-IDMSG-ERROR = SPACE                                          
040930       IF REQU-IDBG = ALL '+'                                             
040940         MOVE SPACE TO REQU-IDBG                                          
040950       END-IF                                                             
040960     END-IF                                                               
040970                                                                          
040980     IF RESP-IDMSG-ERROR = SPACE                                          
040990       IF REQU-IDPG = ALL '+'                                             
040991         MOVE SPACE TO REQU-IDPG                                          
040992       END-IF                                                             
040993     END-IF                                                               
041000                                                                          
042100     IF RESP-IDMSG-ERROR = SPACE                                          
042200       IF REQU-KDSTATUS-KEY = WS-CURRENT                                  
042300         MOVE WS-CURRENT-DATE TO MAP-DAUPPDAT                             
042400       ELSE                                                               
042500         IF REQU-DAUPPDAT NUMERIC                                         
042600           IF REQU-DAUPPDAT > WS-CURRENT-DATE                             
042700             MOVE REQU-DAUPPDAT TO MAP-DAUPPDAT                           
042710                                   DATE-TIDATE                            
042720             MOVE WS-DATE-FORMAT TO DATE-KDDATFMT                         
042730             CALL WZ20DATE USING DATE-WZ20DATE                            
042740             IF DATE-KDRC > ZERO                                          
042750               MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                 
042760               MOVE 'DAUPPDAT'        TO RESP-IDELMT-ERROR                
042770             END-IF                                                       
042800           ELSE                                                           
042900             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
042910             MOVE 'DAUPPDAT' TO RESP-IDELMT-ERROR                         
043000           END-IF                                                         
043100         ELSE                                                             
043200           MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
043300           MOVE 'DAUPPDAT' TO RESP-IDELMT-ERROR                           
043400         END-IF                                                           
043500       END-IF                                                             
043600     END-IF                                                               
043700     .                                                                    
043800*** - UPDATE CURRENT LINE ON T01RECO.                                     
043900 FABB-UPD-CURRENT SECTION.                                                
044000                                                                          
044100     PERFORM DB2-SELECT-T01RECO-TAB-CURR                                  
044200                                                                          
044300     IF LINES-FOUND                                                       
044310       MOVE REQU-SUDOCLIM TO WS-SUDOCLIM                                  
044400       PERFORM DB2-UPDATE-T01RECO-TAB-CURR                                
044500       PERFORM S03-MOVE-TO-RESPOND                                        
044600       IF REQU-FLCOMING = YES                                             
044700         MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                  
044800       ELSE                                                               
044900         MOVE INF-UPDATE-OK TO RESP-IDMSG-INFO                            
045000       END-IF                                                             
045100     ELSE                                                                 
045110       MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                         
045120       MOVE 'IDLANDX3'        TO RESP-IDELMT-ERROR                        
045130       MOVE NOO TO KEYS-SW                                                
045300     END-IF                                                               
045400     .                                                                    
045500*** - UPDATE COMING LINE ON T01RECO. IF NOO COMING LINE EXIST BUT         
045600***   COMING LINE IS CHOOSED FOR UPDATE, PROGRAM WILL INSERT ONE          
045700***   COMING LINE WITH DATA FROM CURRENT LINE BUT USER WILL SEE           
045800***   THIS AS AN UPDATE.                                                  
045900 FABC-UPD-COMING SECTION.                                                 
046000                                                                          
046100     PERFORM DB2-SELECT-T01RECO-TAB-COM                                   
046200                                                                          
046300     IF LINES-FOUND                                                       
046310       MOVE REQU-SUDOCLIM TO WS-SUDOCLIM                                  
046400       PERFORM DB2-UPDATE-T01RECO-TAB-COM                                 
046500       PERFORM S03-MOVE-TO-RESPOND                                        
046600       MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                    
046700     ELSE                                                                 
046800       PERFORM DB2-SELECT-T01RECO-TAB-CURR                                
046900       IF LINES-FOUND                                                     
047000         MOVE REQU-DAUPPDAT TO MAP-DAUPPDAT                               
047100         MOVE WS-ACTIVE TO MAP-DADELDAT                                   
047110         MOVE REQU-SUDOCLIM TO WS-SUDOCLIM                                
047200         PERFORM DB2-INSERT-T01RECO-TAB-COM                               
047300         PERFORM S03-MOVE-TO-RESPOND                                      
047400         MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                  
047500       ELSE                                                               
047510         MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                       
047520         MOVE 'IDLANDX3'        TO RESP-IDELMT-ERROR                      
047530         MOVE NOO TO KEYS-SW                                              
047700       END-IF                                                             
047800     END-IF                                                               
047900     .                                                                    
048000*** - INSERT NEW CURRENT LINE. COMING LINE COULD NOT BE INSERTED.         
048100*** - IF CURRENT LINE EXIST WITH DELETE DATE, DELETE CURRENT LINE         
048200***   PHYSICAL AND INSERT NEW CURRENT LINE.                               
048300 FAC-INSERT-T01RECO SECTION.                                              
048400                                                                          
048500     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
048600       PERFORM DB2-SELECT-T01RECO-TAB-CURR-2                              
048700       IF LINES-FOUND                                                     
048800         IF MAP-DADELDAT = WS-ACTIVE                                      
048901           MOVE ERR-ALREADY-EXIST TO RESP-IDMSG-ERROR                     
048910           MOVE 'IDLANDX3' TO RESP-IDELMT-ERROR                           
049000         ELSE                                                             
049100           PERFORM FACA-CHECK-INSERT-DATA                                 
049200           IF RESP-IDMSG-ERROR = SPACE                                    
049210             MOVE REQU-SUDOCLIM TO WS-SUDOCLIM                            
049300             PERFORM DB2-DELETE-T01RECO-TAB-CURR                          
049400             PERFORM DB2-INSERT-T01RECO-TAB-CURR                          
049500             PERFORM S03-MOVE-TO-RESPOND                                  
049600             MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                        
049900           END-IF                                                         
050000         END-IF                                                           
050100       ELSE                                                               
050200         PERFORM FACA-CHECK-INSERT-DATA                                   
050300         IF RESP-IDMSG-ERROR = SPACE                                      
050310           MOVE REQU-SUDOCLIM TO WS-SUDOCLIM                              
050400           PERFORM DB2-INSERT-T01RECO-TAB-CURR                            
050500           PERFORM S03-MOVE-TO-RESPOND                                    
050600           MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                          
050700         END-IF                                                           
050800       END-IF                                                             
050900     ELSE                                                                 
051000       MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
051100     END-IF                                                               
051200     .                                                                    
051300*** - VALIDATE REQUESTED FIELDS FOR INSERT ON RECEIVING COUNTRY           
051400 FACA-CHECK-INSERT-DATA SECTION.                                          
051500                                                                          
052400     IF RESP-IDMSG-ERROR = SPACE                                          
052500        IF REQU-IDSPRAK = 'EN'                                            
052700          CONTINUE                                                        
052800        ELSE                                                              
052900          MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                      
053000          MOVE 'IDSPRAK'         TO RESP-IDELMT-ERROR                     
053100        END-IF                                                            
053200     END-IF                                                               
053300                                                                          
053400     IF RESP-IDMSG-ERROR = SPACE                                          
053500        IF REQU-BERESPRA-1 > SPACE                                        
053600        AND REQU-BERESPRA-1 NOT = ALL '+'                                 
053700          CONTINUE                                                        
053800        ELSE                                                              
053900          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
054000          MOVE 'BERESPRA-1' TO RESP-IDELMT-ERROR                          
054100        END-IF                                                            
054200     END-IF                                                               
054210                                                                          
054220     IF RESP-IDMSG-ERROR = SPACE                                          
054230       IF REQU-BERESPRA-2 = ALL '+'                                       
054240         MOVE SPACE TO REQU-BERESPRA-2                                    
054250       END-IF                                                             
054260     END-IF                                                               
054300                                                                          
054400     IF RESP-IDMSG-ERROR = SPACE                                          
054500        IF REQU-ADRESP-STREET > SPACE                                     
054600        AND REQU-ADRESP-STREET NOT = ALL '+'                              
054700          CONTINUE                                                        
054800        ELSE                                                              
054900          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
055000          MOVE 'ADRESP-STREET' TO RESP-IDELMT-ERROR                       
055100        END-IF                                                            
055200     END-IF                                                               
055300                                                                          
055400     IF RESP-IDMSG-ERROR = SPACE                                          
055410       IF REQU-ADRESP-BOX > SPACE                                         
055420       AND REQU-ADRESP-BOX NOT = ALL '+'                                  
055430         CONTINUE                                                         
055440       ELSE                                                               
055450         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
055460         MOVE 'ADRESP-BOX' TO RESP-IDELMT-ERROR                           
055470       END-IF                                                             
055800     END-IF                                                               
055900                                                                          
056000     IF RESP-IDMSG-ERROR = SPACE                                          
056100        IF REQU-ADRESP-CITY > SPACE                                       
056200        AND REQU-ADRESP-CITY NOT = ALL '+'                                
056300          CONTINUE                                                        
056400        ELSE                                                              
056500          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
056600          MOVE 'ADRESP-CITY' TO RESP-IDELMT-ERROR                         
056700        END-IF                                                            
056800     END-IF                                                               
056900                                                                          
058000     IF RESP-IDMSG-ERROR = SPACE                                          
058100        IF REQU-IDTFN > SPACE                                             
058200        AND REQU-IDTFN NOT = ALL '+'                                      
058300          CONTINUE                                                        
058400        ELSE                                                              
058500          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
058600          MOVE 'IDTFN' TO RESP-IDELMT-ERROR                               
058700        END-IF                                                            
058800     END-IF                                                               
058900                                                                          
059000     IF RESP-IDMSG-ERROR = SPACE                                          
059100        IF REQU-IDTFX > SPACE                                             
059200        AND REQU-IDTFX NOT = ALL '+'                                      
059300          CONTINUE                                                        
059400        ELSE                                                              
059500          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
059600          MOVE 'IDTFX' TO RESP-IDELMT-ERROR                               
059700        END-IF                                                            
059800     END-IF                                                               
059900                                                                          
060000     IF RESP-IDMSG-ERROR = SPACE                                          
060100        IF REQU-IDMAIL > SPACE                                            
060200        AND REQU-IDMAIL NOT = ALL '+'                                     
060300          CONTINUE                                                        
060400        ELSE                                                              
060500          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
060600          MOVE 'IDMAIL' TO RESP-IDELMT-ERROR                              
060700        END-IF                                                            
060800     END-IF                                                               
060900                                                                          
061000     IF RESP-IDMSG-ERROR = SPACE                                          
061100        IF REQU-BECONT > SPACE                                            
061200        AND REQU-BECONT NOT = ALL '+'                                     
061300          CONTINUE                                                        
061400        ELSE                                                              
061500          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
061600          MOVE 'BECONT' TO RESP-IDELMT-ERROR                              
061700        END-IF                                                            
061800     END-IF                                                               
061900                                                                          
062000     IF RESP-IDMSG-ERROR = SPACE                                          
062100        IF REQU-IDVAT > SPACE                                             
062200        AND REQU-IDVAT NOT = ALL '+'                                      
062300          CONTINUE                                                        
062400        ELSE                                                              
062500          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
062600          MOVE 'IDVAT' TO RESP-IDELMT-ERROR                               
062700        END-IF                                                            
062800     END-IF                                                               
062801                                                                          
062802     IF RESP-IDMSG-ERROR = SPACE                                          
062803        IF REQU-IDVAT-AGENT > SPACE                                       
062804        AND REQU-IDVAT-AGENT NOT = ALL '+'                                
062805          CONTINUE                                                        
062806        ELSE                                                              
062807          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
062808          MOVE 'IDVAT' TO RESP-IDELMT-ERROR                               
062809        END-IF                                                            
062810     END-IF                                                               
062811                                                                          
062812     IF RESP-IDMSG-ERROR = SPACE                                          
062820       IF REQU-IDBG = ALL '+'                                             
062830         MOVE SPACE TO REQU-IDBG                                          
062840       END-IF                                                             
062850     END-IF                                                               
062860                                                                          
062870     IF RESP-IDMSG-ERROR = SPACE                                          
062880       IF REQU-IDPG = ALL '+'                                             
062890         MOVE SPACE TO REQU-IDPG                                          
062891       END-IF                                                             
062892     END-IF                                                               
062900                                                                          
064000     IF RESP-IDMSG-ERROR = SPACE                                          
064100       MOVE WS-CURRENT-DATE TO MAP-DAREGDAT                               
064200       MOVE WS-ACTIVE       TO MAP-DAUPPDAT                               
064300       MOVE WS-ACTIVE       TO MAP-DADELDAT                               
064500     END-IF                                                               
064600     .                                                                    
064700*** - DELETE ON CURRENT LINE = UPDATE IN TABLE T01RECO WITH               
064800***   CURRENT DATE AS DELETE DATE.                                        
064900*** - DELETE ON COMING LINE IS A PHYSICAL DELETE FROM TABLE               
065000***   T01RECO.                                                            
065100 FAD-DELETE-T01RECO SECTION.                                              
065200                                                                          
065300     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
065400       PERFORM DB2-SELECT-T01INRE-TAB                                     
065500       IF LINES-FOUND                                                     
065600         MOVE ERR-DELETE-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
065800       ELSE                                                               
065900         PERFORM DB2-SELECT-T01RECO-TAB-CURR                              
066000         IF LINES-FOUND                                                   
066010           MOVE REQU-SUDOCLIM TO WS-SUDOCLIM                              
066100           PERFORM DB2-UPDATE-T01RECO-TAB-DEL                             
066200           PERFORM S03-MOVE-TO-RESPOND                                    
066400           PERFORM DB2-SELECT-T01RECO-TAB-COM                             
066500           IF LINES-FOUND                                                 
066600             PERFORM DB2-DELETE-T01RECO-TAB-COM                           
066700           END-IF                                                         
066800         ELSE                                                             
066810           MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                     
066820           MOVE 'IDLANDX3'        TO RESP-IDELMT-ERROR                    
066830           MOVE NOO TO KEYS-SW                                            
067000         END-IF                                                           
067100       END-IF                                                             
067200     ELSE                                                                 
067300       PERFORM DB2-SELECT-T01RECO-TAB-COM                                 
067400       IF LINES-FOUND                                                     
067500         PERFORM DB2-DELETE-T01RECO-TAB-COM                               
067600         PERFORM S03-MOVE-TO-RESPOND                                      
067610         MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                            
067700       ELSE                                                               
067710         MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                       
067720         MOVE 'IDLANDX3'        TO RESP-IDELMT-ERROR                      
067730         MOVE NOO TO KEYS-SW                                              
067900       END-IF                                                             
068000     END-IF                                                               
068100     .                                                                    
068200*    --- DISPATCHER SECTIONS                                              
068300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
068400                                                                          
068500     MOVE 'GETARG'                   TO SUB-KDFUNC                        
068600     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
068700     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
068800                                                                          
068900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
069000                                                                          
069100     IF SUB-KDRC > 0                                                      
069200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
069300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
069400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
069500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
069600     END-IF                                                               
069700     .                                                                    
069800 S02-RETURN-RESPONSE SECTION.                                             
069900                                                                          
070000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
070100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
070200                                                                          
070300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
070400                                                                          
070500     IF SUB-KDRC > 0                                                      
070600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
070700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
070800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
070900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
071000     END-IF                                                               
071100     .                                                                    
071200*    --- MOVE TO OUTPUT SECTIONS                                          
071300 S03-MOVE-TO-RESPOND SECTION.                                             
071400                                                                          
071500     IF ACT-CODE-SEARCH                                                   
071600       MOVE MAP-RESP-BELAND        TO RESP-BELAND                         
071700       MOVE MAP-RESP-IDSPRAK       TO RESP-IDSPRAK                        
071800       MOVE MAP-RESP-BERESPRA-1    TO RESP-BERESPRA-1                     
071810       MOVE MAP-RESP-BERESPRA-2    TO RESP-BERESPRA-2                     
071900       MOVE MAP-RESP-ADRESP-STREET TO RESP-ADRESP-STREET                  
072000       MOVE MAP-RESP-ADRESP-BOX    TO RESP-ADRESP-BOX                     
072100       MOVE MAP-RESP-ADRESP-CITY   TO RESP-ADRESP-CITY                    
072200       MOVE MAP-RESP-ADRESP-PCODE  TO RESP-ADRESP-PCODE                   
072300       MOVE MAP-RESP-IDTFN         TO RESP-IDTFN                          
072400       MOVE MAP-RESP-IDTFX         TO RESP-IDTFX                          
072500       MOVE MAP-RESP-IDMAIL        TO RESP-IDMAIL                         
072600       MOVE MAP-RESP-BECONT        TO RESP-BECONT                         
072700       MOVE MAP-RESP-IDVAT         TO RESP-IDVAT                          
072701       MOVE MAP-RESP-IDVAT-AGENT   TO RESP-IDVAT-AGENT                    
072710       MOVE MAP-RESP-IDBG          TO RESP-IDBG                           
072720       MOVE MAP-RESP-IDPG          TO RESP-IDPG                           
072730       MOVE WS-SUDOCLIM            TO WS-SUDOCLIM-2                       
072740       MOVE WS-SUDOCLIM-2          TO RESP-SUDOCLIM                       
072800       MOVE MAP-DAREGDAT           TO RESP-DAREGDAT                       
072900       MOVE MAP-DAUPPDAT           TO RESP-DAUPPDAT                       
073000       MOVE MAP-DADELDAT           TO RESP-DADELDAT                       
073100       MOVE MAP-RESP-IDUSER        TO RESP-IDUSER                         
073200       MOVE NOO                    TO RESP-FLCOMING                       
073300     ELSE                                                                 
073400       IF ACT-CODE-UPDATE                                                 
073500         MOVE MAP-RESP-BELAND    TO RESP-BELAND                           
073600         MOVE REQU-IDSPRAK       TO RESP-IDSPRAK                          
073700         MOVE REQU-BERESPRA-1    TO RESP-BERESPRA-1                       
073710         MOVE REQU-BERESPRA-2    TO RESP-BERESPRA-2                       
073800         MOVE REQU-ADRESP-STREET TO RESP-ADRESP-STREET                    
073900         MOVE REQU-ADRESP-BOX    TO RESP-ADRESP-BOX                       
074000         MOVE REQU-ADRESP-CITY   TO RESP-ADRESP-CITY                      
074100         MOVE REQU-ADRESP-PCODE  TO RESP-ADRESP-PCODE                     
074200         MOVE REQU-IDTFN         TO RESP-IDTFN                            
074300         MOVE REQU-IDTFX         TO RESP-IDTFX                            
074400         MOVE REQU-IDMAIL        TO RESP-IDMAIL                           
074500         MOVE REQU-BECONT        TO RESP-BECONT                           
074600         MOVE REQU-IDVAT         TO RESP-IDVAT                            
074601         MOVE REQU-IDVAT-AGENT   TO RESP-IDVAT-AGENT                      
074610         MOVE REQU-IDBG          TO RESP-IDBG                             
074620         MOVE REQU-IDPG          TO RESP-IDPG                             
074630         MOVE REQU-SUDOCLIM      TO RESP-SUDOCLIM                         
074700         MOVE MAP-DAREGDAT       TO RESP-DAREGDAT                         
074800         MOVE MAP-DAUPPDAT       TO RESP-DAUPPDAT                         
074900         MOVE MAP-DADELDAT       TO RESP-DADELDAT                         
075000         MOVE REQU-IDUSER        TO RESP-IDUSER                           
075100         IF REQU-KDSTATUS-KEY = WS-CURRENT                                
075200           MOVE REQU-FLCOMING TO RESP-FLCOMING                            
075300         ELSE                                                             
075400           MOVE REQU-FLCOMING TO RESP-FLCOMING                            
075410* DONT SHOW DAREGDAT WHEN UPDATING ON COMING VERSION                      
075420           MOVE ZERO TO RESP-DAREGDAT                                     
075500         END-IF                                                           
075600       ELSE                                                               
075700         IF ACT-CODE-INSERT                                               
075800           MOVE MAP-RESP-BELAND    TO RESP-BELAND                         
075900           MOVE REQU-IDSPRAK       TO RESP-IDSPRAK                        
076000           MOVE REQU-BERESPRA-1    TO RESP-BERESPRA-1                     
076010           MOVE REQU-BERESPRA-2    TO RESP-BERESPRA-2                     
076100           MOVE REQU-ADRESP-STREET TO RESP-ADRESP-STREET                  
076200           MOVE REQU-ADRESP-BOX    TO RESP-ADRESP-BOX                     
076300           MOVE REQU-ADRESP-CITY   TO RESP-ADRESP-CITY                    
076400           MOVE REQU-ADRESP-PCODE  TO RESP-ADRESP-PCODE                   
076500           MOVE REQU-IDTFN         TO RESP-IDTFN                          
076600           MOVE REQU-IDTFX         TO RESP-IDTFX                          
076700           MOVE REQU-IDMAIL        TO RESP-IDMAIL                         
076800           MOVE REQU-BECONT        TO RESP-BECONT                         
076900           MOVE REQU-IDVAT         TO RESP-IDVAT                          
076901           MOVE REQU-IDVAT-AGENT   TO RESP-IDVAT-AGENT                    
076910           MOVE REQU-IDBG          TO RESP-IDBG                           
076920           MOVE REQU-IDPG          TO RESP-IDPG                           
076930           MOVE REQU-SUDOCLIM      TO RESP-SUDOCLIM                       
077000           MOVE MAP-DAREGDAT       TO RESP-DAREGDAT                       
077100           MOVE MAP-DAUPPDAT       TO RESP-DAUPPDAT                       
077200           MOVE MAP-DADELDAT       TO RESP-DADELDAT                       
077300           MOVE REQU-IDUSER        TO RESP-IDUSER                         
077400           MOVE NOO                TO RESP-FLCOMING                       
077500         ELSE                                                             
077600           IF ACT-CODE-DELETE                                             
077700             MOVE SPACE           TO RESP-BELAND                          
077800                                     RESP-IDSPRAK                         
077900                                     RESP-BERESPRA-1                      
077910                                     RESP-BERESPRA-2                      
078000                                     RESP-ADRESP-STREET                   
078100                                     RESP-ADRESP-BOX                      
078200                                     RESP-ADRESP-CITY                     
078300                                     RESP-ADRESP-PCODE                    
078400                                     RESP-IDTFN                           
078500                                     RESP-IDTFX                           
078600                                     RESP-IDMAIL                          
078700                                     RESP-BECONT                          
078800                                     RESP-IDVAT                           
078801                                     RESP-IDVAT-AGENT                     
078810                                     RESP-IDBG                            
078820                                     RESP-IDPG                            
078900             MOVE ZERO            TO RESP-SUDOCLIM                        
078910                                     RESP-DAREGDAT                        
079000                                     RESP-DAUPPDAT                        
079100             MOVE WS-CURRENT-DATE TO RESP-DADELDAT                        
079200             MOVE REQU-IDUSER     TO RESP-IDUSER                          
079300             MOVE NOO             TO RESP-FLCOMING                        
079400           END-IF                                                         
079500         END-IF                                                           
079600       END-IF                                                             
079700     END-IF                                                               
079800     .                                                                    
079801                                                                          
079802     EJECT                                                                
079810 S04-MOVE-MISSING-TO-RESPOND SECTION.                                     
079820     MOVE SPACE             TO RESP-FLCOMING                              
079830                               RESP-BELAND                                
079840                               RESP-IDSPRAK                               
079841                               RESP-BERESPRA-1                            
079842                               RESP-BERESPRA-2                            
079843                               RESP-ADRESP-STREET                         
079844                               RESP-ADRESP-BOX                            
079845                               RESP-ADRESP-CITY                           
079846                               RESP-ADRESP-PCODE                          
079847                               RESP-IDTFN                                 
079848                               RESP-IDTFX                                 
079849                               RESP-IDMAIL                                
079850                               RESP-IDBG                                  
079851                               RESP-IDPG                                  
079852                               RESP-IDVAT                                 
079853                               RESP-IDVAT-AGENT                           
079854                               RESP-BECONT                                
079860                               RESP-IDUSER                                
079870     MOVE ZERO              TO RESP-SUDOCLIM                              
079871                               RESP-DAREGDAT                              
079880                               RESP-DAUPPDAT                              
079890                               RESP-DADELDAT                              
079891     .                                                                    
079892     EJECT                                                                
079893                                                                          
079900*    --- DB2 SECTIONS                                                     
080000 DB2-SELECT-T01LSEL-TAB   SECTION.                                        
080100                                                                          
080200     MOVE 000100 TO GOOD-SQLCODECODES                                     
080300                                                                          
080400     EXEC SQL                                                             
080500        SELECT  BELEGRAD_1                                                
080600                                                                          
080700        INTO   :T01LSEL-BELEGRAD-1                                        
080800                                                                          
080900        FROM    T01LSEL                                                   
081000                                                                          
081100        WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                             
081200            AND KDSTATUS = :WS-CURRENT                                    
081300     END-EXEC                                                             
081400                                                                          
081500     MOVE SQLCODE TO SQLCODE-WS                                           
081600     PERFORM DB2-STATUS-CHECK                                             
081700     .                                                                    
081710 DB2-SELECT-T01COCO-TAB   SECTION.                                        
081720                                                                          
081730     MOVE 000100 TO GOOD-SQLCODECODES                                     
081740                                                                          
081750     EXEC SQL                                                             
081760        SELECT  BELAND                                                    
081770                                                                          
081780        INTO   :MAP-RESP-BELAND                                           
081790                                                                          
081791        FROM    T01COCO                                                   
081792                                                                          
081793        WHERE   IDLANDX3 = :REQU-IDLANDX3-KEY                             
081794     END-EXEC                                                             
081795                                                                          
081796     MOVE SQLCODE TO SQLCODE-WS                                           
081797     PERFORM DB2-STATUS-CHECK                                             
081798     .                                                                    
081800 DB2-SELECT-T01INRE-TAB   SECTION.                                        
081900                                                                          
082000     MOVE 000100305 TO GOOD-SQLCODECODES                                  
082100                                                                          
082200     EXEC SQL                                                             
082300        SELECT MAX(IDLANDX3_REC)                                          
082400                                                                          
082500        INTO   :T01INRE-IDLANDX3-REC                                      
082600                                                                          
082700        FROM    T01INRE                                                   
082800                                                                          
082900        WHERE IDLEGSEL      = :REQU-IDLEGSEL-KEY                          
083000        AND   IDLANDX3_SEND = :REQU-IDLANDX3-KEY                          
083100        AND   DADELDAT      = :WS-ACTIVE                                  
083200     END-EXEC                                                             
083300                                                                          
083400     MOVE SQLCODE TO SQLCODE-WS                                           
083500     PERFORM DB2-STATUS-CHECK                                             
083600     .                                                                    
083700* * * * * * * * * * * * - CURSOR-1 - * * * * * * * * * * * * * * *        
083800 DB2-DCL-OPN-T01RECO-CRS-1 SECTION.                                       
083900                                                                          
084000     MOVE 000100 TO GOOD-SQLCODECODES                                     
084100                                                                          
084200     EXEC SQL                                                             
084300         DECLARE T01RECO-CRS-1 CURSOR WITH HOLD FOR                       
084400                                                                          
084500           SELECT  KDSTATUS                                               
084700                 , IDSPRAK                                                
084800                 , BERESPRA_1                                             
084810                 , BERESPRA_2                                             
084900                 , ADRESP_STREET                                          
085000                 , ADRESP_BOX                                             
085100                 , ADRESP_CITY                                            
085200                 , ADRESP_PCODE                                           
085300                 , IDTFN                                                  
085400                 , IDTFX                                                  
085500                 , IDMAIL                                                 
085600                 , BECONT                                                 
085700                 , IDVAT                                                  
085701                 , IDVAT_AGENT                                            
085710                 , IDBG                                                   
085720                 , IDPG                                                   
085730                 , SUDOCLIM                                               
085800                 , DAREGDAT                                               
085900                 , DAUPPDAT                                               
086000                 , DADELDAT                                               
086100                 , IDUSER                                                 
086200                                                                          
086300           FROM     T01RECO                                               
086400                                                                          
086500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
086600                AND IDLANDX3 = :REQU-IDLANDX3-KEY                         
086700                AND DADELDAT = :WS-ACTIVE                                 
086800                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
086900                                                                          
087000           ORDER BY IDLEGSEL                                              
087100                  , IDLANDX3                                              
087200                  , KDSTATUS                                              
087300     END-EXEC                                                             
087400                                                                          
087500     MOVE 000100 TO GOOD-SQLCODECODES                                     
087600                                                                          
087700     EXEC SQL                                                             
087800        OPEN T01RECO-CRS-1                                                
087900     END-EXEC                                                             
088000                                                                          
088100     MOVE SQLCODE TO SQLCODE-WS                                           
088200     PERFORM DB2-STATUS-CHECK                                             
088300     .                                                                    
088400 DB2-FETCH-T01RECO-CRS-1 SECTION.                                         
088500                                                                          
088600     MOVE 000100  TO GOOD-SQLCODECODES                                    
088700                                                                          
088800     EXEC SQL                                                             
088900         FETCH T01RECO-CRS-1                                              
089000                                                                          
089100         INTO :MAP-KDSTATUS                                               
089300            , :MAP-RESP-IDSPRAK                                           
089400            , :MAP-RESP-BERESPRA-1                                        
089410            , :MAP-RESP-BERESPRA-2                                        
089500            , :MAP-RESP-ADRESP-STREET                                     
089600            , :MAP-RESP-ADRESP-BOX                                        
089700            , :MAP-RESP-ADRESP-CITY                                       
089800            , :MAP-RESP-ADRESP-PCODE                                      
089900            , :MAP-RESP-IDTFN                                             
090000            , :MAP-RESP-IDTFX                                             
090100            , :MAP-RESP-IDMAIL                                            
090200            , :MAP-RESP-BECONT                                            
090300            , :MAP-RESP-IDVAT                                             
090301            , :MAP-RESP-IDVAT-AGENT                                       
090310            , :MAP-RESP-IDBG                                              
090320            , :MAP-RESP-IDPG                                              
090330            , :WS-SUDOCLIM                                                
090400            , :MAP-DAREGDAT                                               
090500            , :MAP-DAUPPDAT                                               
090600            , :MAP-DADELDAT                                               
090700            , :MAP-RESP-IDUSER                                            
090800     END-EXEC                                                             
090900                                                                          
091000     MOVE SQLCODE TO SQLCODE-WS                                           
091100     PERFORM DB2-STATUS-CHECK                                             
091200     .                                                                    
091300 DB2-CLOSE-T01RECO-CRS-1  SECTION.                                        
091400                                                                          
091500     EXEC SQL                                                             
091600        CLOSE T01RECO-CRS-1                                               
091700     END-EXEC                                                             
091800     .                                                                    
091900 DB2-SELECT-T01RECO-TAB-CURR SECTION.                                     
092000                                                                          
092100     MOVE 000100  TO GOOD-SQLCODECODES                                    
092200                                                                          
092300     EXEC SQL                                                             
092400         SELECT DAREGDAT                                                  
092500              , DADELDAT                                                  
092600                                                                          
092700         INTO  :MAP-DAREGDAT                                              
092800             , :MAP-DADELDAT                                              
092900                                                                          
093000         FROM  T01RECO                                                    
093100                                                                          
093200         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
093300         AND   IDLANDX3 = :REQU-IDLANDX3-KEY                              
093400         AND   DADELDAT = :WS-ACTIVE                                      
093500         AND   KDSTATUS = :WS-CURRENT                                     
093600     END-EXEC                                                             
093700                                                                          
093800     MOVE SQLCODE TO SQLCODE-WS                                           
093900     PERFORM DB2-STATUS-CHECK                                             
094000     .                                                                    
094100 DB2-UPDATE-T01RECO-TAB-CURR SECTION.                                     
094200                                                                          
094300     MOVE 000     TO GOOD-SQLCODECODES                                    
094400                                                                          
094500     EXEC SQL                                                             
094600        UPDATE T01RECO                                                    
094700           SET                                                            
094800                 IDSPRAK       = :REQU-IDSPRAK                            
094900               , BERESPRA_1    = :REQU-BERESPRA-1                         
094910               , BERESPRA_2    = :REQU-BERESPRA-2                         
095000               , ADRESP_STREET = :REQU-ADRESP-STREET                      
095100               , ADRESP_BOX    = :REQU-ADRESP-BOX                         
095200               , ADRESP_CITY   = :REQU-ADRESP-CITY                        
095300               , ADRESP_PCODE  = :REQU-ADRESP-PCODE                       
095400               , IDTFN         = :REQU-IDTFN                              
095500               , IDTFX         = :REQU-IDTFX                              
095600               , IDMAIL        = :REQU-IDMAIL                             
095700               , BECONT        = :REQU-BECONT                             
095800               , IDVAT         = :REQU-IDVAT                              
095801               , IDVAT_AGENT   = :REQU-IDVAT-AGENT                        
095810               , IDBG          = :REQU-IDBG                               
095820               , IDPG          = :REQU-IDPG                               
095830               , SUDOCLIM      = :WS-SUDOCLIM                             
095900               , DAUPPDAT      = :MAP-DAUPPDAT                            
096000               , IDUSER        = :REQU-IDUSER                             
096100                                                                          
096200         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
096300         AND     IDLANDX3 = :REQU-IDLANDX3-KEY                            
096400         AND     DADELDAT = :WS-ACTIVE                                    
096500         AND     KDSTATUS = :WS-CURRENT                                   
096600     END-EXEC                                                             
096700                                                                          
096800     MOVE SQLCODE TO SQLCODE-WS                                           
096900     PERFORM DB2-STATUS-CHECK                                             
097000     .                                                                    
097100 DB2-UPDATE-T01RECO-TAB-DEL SECTION.                                      
097200                                                                          
097300     MOVE 000     TO GOOD-SQLCODECODES                                    
097400                                                                          
097500     EXEC SQL                                                             
097600        UPDATE T01RECO                                                    
097700           SET   DADELDAT = :WS-CURRENT-DATE                              
097800                                                                          
097900         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
098000         AND     IDLANDX3 = :REQU-IDLANDX3-KEY                            
098100         AND     DADELDAT = :WS-ACTIVE                                    
098200         AND     KDSTATUS = :WS-CURRENT                                   
098300     END-EXEC                                                             
098400                                                                          
098500     MOVE SQLCODE TO SQLCODE-WS                                           
098600     PERFORM DB2-STATUS-CHECK                                             
098700     .                                                                    
098800 DB2-SELECT-T01RECO-TAB-COM SECTION.                                      
098900                                                                          
099000     MOVE 000100  TO GOOD-SQLCODECODES                                    
099100                                                                          
099200     EXEC SQL                                                             
099300         SELECT DAREGDAT                                                  
099400              , DADELDAT                                                  
099500                                                                          
099600         INTO  :MAP-DAREGDAT                                              
099700             , :MAP-DADELDAT                                              
099800                                                                          
099900         FROM  T01RECO                                                    
100000                                                                          
100100         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
100200         AND   IDLANDX3 = :REQU-IDLANDX3-KEY                              
100300         AND   KDSTATUS = :WS-COMING                                      
100400     END-EXEC                                                             
100500                                                                          
100600     MOVE SQLCODE TO SQLCODE-WS                                           
100700     PERFORM DB2-STATUS-CHECK                                             
100800     .                                                                    
100900 DB2-UPDATE-T01RECO-TAB-COM  SECTION.                                     
101000                                                                          
101100     MOVE 000     TO GOOD-SQLCODECODES                                    
101200                                                                          
101300     EXEC SQL                                                             
101400        UPDATE T01RECO                                                    
101500           SET                                                            
101600                 IDSPRAK       = :REQU-IDSPRAK                            
101700               , BERESPRA_1    = :REQU-BERESPRA-1                         
101710               , BERESPRA_2    = :REQU-BERESPRA-2                         
101800               , ADRESP_STREET = :REQU-ADRESP-STREET                      
101900               , ADRESP_BOX    = :REQU-ADRESP-BOX                         
102000               , ADRESP_CITY   = :REQU-ADRESP-CITY                        
102100               , ADRESP_PCODE  = :REQU-ADRESP-PCODE                       
102200               , IDTFN         = :REQU-IDTFN                              
102300               , IDTFX         = :REQU-IDTFX                              
102400               , IDMAIL        = :REQU-IDMAIL                             
102500               , BECONT        = :REQU-BECONT                             
102600               , IDVAT         = :REQU-IDVAT                              
102601               , IDVAT_AGENT   = :REQU-IDVAT-AGENT                        
102610               , IDBG          = :REQU-IDBG                               
102620               , IDPG          = :REQU-IDPG                               
102630               , SUDOCLIM      = :WS-SUDOCLIM                             
102700               , DAUPPDAT      = :MAP-DAUPPDAT                            
102800               , IDUSER        = :REQU-IDUSER                             
102900                                                                          
103000         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
103100         AND     IDLANDX3 = :REQU-IDLANDX3-KEY                            
103200         AND     DADELDAT = :WS-ACTIVE                                    
103300         AND     KDSTATUS = :WS-COMING                                    
103400     END-EXEC                                                             
103500                                                                          
103600     MOVE SQLCODE TO SQLCODE-WS                                           
103700     PERFORM DB2-STATUS-CHECK                                             
103800     .                                                                    
103900 DB2-SELECT-T01RECO-TAB-CURR-2 SECTION.                                   
104000                                                                          
104100     MOVE 000100  TO GOOD-SQLCODECODES                                    
104200                                                                          
104300     EXEC SQL                                                             
104400         SELECT DADELDAT                                                  
104500                                                                          
104600         INTO  :MAP-DADELDAT                                              
104700                                                                          
104800         FROM  T01RECO                                                    
104900                                                                          
105000         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
105100         AND   IDLANDX3 = :REQU-IDLANDX3-KEY                              
105200         AND   KDSTATUS = :WS-CURRENT                                     
105300     END-EXEC                                                             
105400                                                                          
105500     MOVE SQLCODE TO SQLCODE-WS                                           
105600     PERFORM DB2-STATUS-CHECK                                             
105700     .                                                                    
105800 DB2-INSERT-T01RECO-TAB-CURR  SECTION.                                    
105900                                                                          
106000     MOVE 000   TO GOOD-SQLCODECODES                                      
106100                                                                          
106200     EXEC SQL                                                             
106300         INSERT INTO T01RECO                                              
106400            (IDLEGSEL,IDLANDX3,KDSTATUS,IDSPRAK,BERESPRA_1                
106500            ,BERESPRA_2,ADRESP_STREET,ADRESP_BOX,ADRESP_CITY              
106600            ,ADRESP_PCODE,IDTFN,IDTFX,IDMAIL,BECONT                       
106610            ,IDVAT,IDVAT_AGENT,IDBG                                       
106700            ,IDPG,SUDOCLIM,DAREGDAT,DAUPPDAT,DADELDAT,IDUSER)             
106800         VALUES                                                           
106900            (:REQU-IDLEGSEL-KEY,:REQU-IDLANDX3-KEY,:WS-CURRENT            
107000            ,:REQU-IDSPRAK,:REQU-BERESPRA-1                               
107100            ,:REQU-BERESPRA-2,:REQU-ADRESP-STREET,:REQU-ADRESP-BOX        
107200            ,:REQU-ADRESP-CITY,:REQU-ADRESP-PCODE,:REQU-IDTFN             
107300            ,:REQU-IDTFX,:REQU-IDMAIL,:REQU-BECONT                        
107301            ,:REQU-IDVAT,:REQU-IDVAT-AGENT                                
107310            ,:REQU-IDBG,:REQU-IDPG,:WS-SUDOCLIM                           
107320                                  ,:MAP-DAREGDAT,:MAP-DAUPPDAT            
107400            ,:MAP-DADELDAT,:REQU-IDUSER)                                  
107600     END-EXEC                                                             
107700                                                                          
107800     MOVE SQLCODE TO SQLCODE-WS                                           
107900     PERFORM DB2-STATUS-CHECK                                             
108000     .                                                                    
108100 DB2-INSERT-T01RECO-TAB-COM SECTION.                                      
108200                                                                          
108300     MOVE 000   TO GOOD-SQLCODECODES                                      
108400                                                                          
108500     EXEC SQL                                                             
108600         INSERT INTO T01RECO                                              
108700            (IDLEGSEL,IDLANDX3,KDSTATUS,IDSPRAK,BERESPRA_1                
108800            ,BERESPRA_2,ADRESP_STREET,ADRESP_BOX,ADRESP_CITY              
108900            ,ADRESP_PCODE,IDTFN,IDTFX,IDMAIL,BECONT                       
108910            ,IDVAT,IDVAT_AGENT,IDBG                                       
109000            ,IDPG,SUDOCLIM,DAREGDAT,DAUPPDAT,DADELDAT,IDUSER)             
109100         VALUES                                                           
109200            (:REQU-IDLEGSEL-KEY,:REQU-IDLANDX3-KEY,:WS-COMING             
109300            ,:REQU-IDSPRAK,:REQU-BERESPRA-1                               
109400            ,:REQU-BERESPRA-2,:REQU-ADRESP-STREET,:REQU-ADRESP-BOX        
109500            ,:REQU-ADRESP-CITY,:REQU-ADRESP-PCODE,:REQU-IDTFN             
109600            ,:REQU-IDTFX,:REQU-IDMAIL,:REQU-BECONT                        
109610            ,:REQU-IDVAT,:REQU-IDVAT-AGENT                                
109700            ,:REQU-IDBG,:REQU-IDPG,:WS-SUDOCLIM                           
109701            ,:MAP-DAREGDAT,:MAP-DAUPPDAT                                  
109710            ,:MAP-DADELDAT,:REQU-IDUSER)                                  
109900     END-EXEC                                                             
110000                                                                          
110100     MOVE SQLCODE TO SQLCODE-WS                                           
110200     PERFORM DB2-STATUS-CHECK                                             
110300     .                                                                    
110400 DB2-DELETE-T01RECO-TAB-CURR SECTION.                                     
110500                                                                          
110600     MOVE 000   TO GOOD-SQLCODECODES                                      
110700                                                                          
110800     EXEC SQL                                                             
110900         DELETE FROM T01RECO                                              
111000                                                                          
111100         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
111200            AND IDLANDX3 = :REQU-IDLANDX3-KEY                             
111300            AND KDSTATUS = :WS-CURRENT                                    
111400            AND DADELDAT > :WS-ACTIVE                                     
111500     END-EXEC                                                             
111600                                                                          
111700     MOVE SQLCODE TO SQLCODE-WS                                           
111800     PERFORM DB2-STATUS-CHECK                                             
111900     .                                                                    
112000 DB2-DELETE-T01RECO-TAB-COM SECTION.                                      
112100                                                                          
112200     MOVE 000   TO GOOD-SQLCODECODES                                      
112300                                                                          
112400     EXEC SQL                                                             
112500         DELETE FROM T01RECO                                              
112600                                                                          
112700         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
112800            AND IDLANDX3 = :REQU-IDLANDX3-KEY                             
112900            AND KDSTATUS = :WS-COMING                                     
113000     END-EXEC                                                             
113100                                                                          
113200     MOVE SQLCODE TO SQLCODE-WS                                           
113300     PERFORM DB2-STATUS-CHECK                                             
113400     .                                                                    
113500 DB2-STATUS-CHECK  SECTION.                                               
113600                                                                          
113700     SET SQLCODE-IX TO 1                                                  
113800     SEARCH GOOD-SQLCODE                                                  
113900       AT END                                                             
114000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
114100          DELIMITED BY SIZE INTO ERROR-TEXT                               
114200          CALL ABEND USING RKOD-ABEND-DB2                                 
114300       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
114400     END-SEARCH                                                           
114500     .                                                                    
