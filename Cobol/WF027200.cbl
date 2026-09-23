000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF027200.                                                
000300 AUTHOR.         LUNDH BERNT.                                             
000400 DATE-WRITTEN.   02/04/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000710*    NAME:                                                                
000800*        CARPARTS.BILLIT.INTRULEMAINTENANCE                               
000900*    FUNCTION:                                                            
001000*        READ/UPDATE/INSERT/DELETE INTERSTATE RULES TABLE T01INRE         
001100*        DEPENDING ON REQUESTED PROGRAMS ACTION CODE (KDPGMACT)           
001200*        KDPGMACT = 'S' READ                                              
001300*        KDPGMACT = 'U' UPDATE                                            
001400*        KDPGMACT = 'I' INSERT                                            
001500*        KDPGMACT = 'D' DELETE                                            
001600*                                                                         
001700*        THE PROGRAM READS   TABLE T01LSEL                                
001900*        THE PROGRAM READS   TABLE T01COCO                                
001901*        THE PROGRAM READS   TABLE T01SECO                                
001902*        THE PROGRAM READS   TABLE T01RECO                                
001910*        THE PROGRAM UPDATES TABLE T01INRE                                
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: WF0272U                                             
002300*        REQUEST:     WF0272I1                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        RESPONSE:    WF0272O1                                            
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
004000 77  IDPGM                       PIC X(08)   VALUE 'WF027200'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004300 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600*    --- CONSTANT WORK FIELDS                                             
004700 77  JA                          PIC X       VALUE 'J'.                   
004710 77  YES                         PIC X       VALUE 'Y'.                   
004800 77  NOO                         PIC X       VALUE 'N'.                   
004900 77  WS-ADRESS                   PIC X(50)                                
005000                    VALUE 'CARPARTS.BILLIT.INTRULEMAINTENANCE'.           
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
006600*    --- OTHER MAPPING-FIELDS THAN COPYTEXT WF0272O1                      
006700 01  MAP-KDSTATUS                PIC S9(3)   VALUE ZERO COMP-3.           
006800 01  MAP-DAREGDAT                PIC X(8)    VALUE SPACE.                 
006900 01  MAP-DAUPPDAT                PIC X(8)    VALUE SPACE.                 
007000 01  MAP-DADELDAT                PIC X(8)    VALUE SPACE.                 
007100                                                                          
007200*    --- WORK-FIELDS                                                      
007300 01  WS-FLCONTROL                PIC X       VALUE SPACE.                 
007400 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
007500 01  WS-IDLANDX3-KEY             PIC X(3).                                
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
009600         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
009610         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
009700         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
009800         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
009810         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
009900     03  INFO-CODES.                                                      
010000         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
010100         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
010200         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
010300         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
010310         05  INF-NO-DATA-JOINED      PIC X(3)    VALUE '102'.             
010400*                                                                         
010500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
010600     SKIP3                                                                
010700 01  -COPY WZ01SUB                                                        
010701     EJECT                                                                
010710 01  FILLER                      PIC X(16)   VALUE 'DATE-CONTROL'.        
010720     SKIP3                                                                
010730 01  -COPY WZ20DATE                                                       
010740     EJECT                                                                
010900*                                                                         
011000 01  FILLER                      PIC X(16)   VALUE 'MAPPING-AREA'.        
011100     SKIP3                                                                
011200 01  -COPY WF0272O1  -PRE MAP-                                            
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011500     SKIP3                                                                
011600 01  REQU-AREA.                                                           
011700*    03  -COPY WZ01REQU                                                   
011800*    03  -COPY WF0272I1                                                   
011900     EJECT                                                                
012000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012100     SKIP3                                                                
012200 01  RESP-AREA.                                                           
012300*    03  -COPY WZ01RESP                                                   
012400*    03  -COPY WF0272O1                                                   
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
013500         88  RESOURCE-WRONG                  VALUE 904.                   
013600     03  GOOD-SQLCODECODES.                                               
013700         05  GOOD-SQLCODE OCCURS 5                                        
013800             INDEXED BY SQLCODE-IX PIC 9(3).                              
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
014300*01  -COPY T01LSEL -PRE T01LSEL-                                          
014400     EJECT                                                                
014410 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
014420*01  -COPY T01COCO -PRE T01COCO-                                          
014430     EJECT                                                                
014500 01  FILLER                      PIC X(16)   VALUE 'T01SECO-AREA'.        
014700*01  -COPY T01SECO -PRE T01SECO-                                          
014710     EJECT                                                                
014720 01  FILLER                      PIC X(16)   VALUE 'T01RECO-AREA'.        
014740*01  -COPY T01RECO -PRE T01RECO-                                          
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'T01INRE-AREA'.        
015100*01  -COPY T01INRE -PRE T01INRE-                                          
015200     EJECT                                                                
015300     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
015400     EJECT                                                                
015410     EXEC SQL INCLUDE T01COCO END-EXEC.                                   
015420     EJECT                                                                
015500     EXEC SQL INCLUDE T01SECO END-EXEC.                                   
015510     EJECT                                                                
015520     EXEC SQL INCLUDE T01RECO END-EXEC.                                   
015600     EJECT                                                                
015700     EXEC SQL INCLUDE T01INRE END-EXEC.                                   
015800     EJECT                                                                
015900 LINKAGE SECTION.                                                         
016000     EJECT                                                                
016010                                                                          
016100 PROCEDURE DIVISION.                                                      
016200 MAIN SECTION.                                                            
016400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
016500     IF SUB-KDRC = 0                                                      
016600       PERFORM A-INIT                                                     
016700       PERFORM B-CHECK-KEYS                                               
016800       IF KEYS-OK                                                         
016900         PERFORM F-READ-SHOW-INFO                                         
017000       END-IF                                                             
017010       IF KEYS-WRONG                                                      
017020         PERFORM S05-MOVE-MISSING-TO-RESPOND                              
017030       END-IF                                                             
017100       PERFORM S02-RETURN-RESPONSE                                        
017200     END-IF                                                               
017300                                                                          
017400     MOVE ZERO TO RETURN-CODE                                             
017500     GOBACK                                                               
017600     .                                                                    
017800                                                                          
017810 A-INIT SECTION.                                                          
017900     INITIALIZE GOOD-SQLCODECODES                                         
018000     MOVE ALL '+' TO RESP-AREA                                            
018010     MOVE SPACE TO RESP-IDMSG-ERROR                                       
018020     MOVE SPACE TO RESP-IDMSG-INFO                                        
018030     MOVE SPACE TO RESP-IDELMT-ERROR                                      
018100     INITIALIZE MAP-RESP-WF0272O1                                         
018200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
018300     .                                                                    
018310                                                                          
018400*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
018500 B-CHECK-KEYS SECTION.                                                    
018700     MOVE YES TO KEYS-SW                                                  
018800     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
018900                                                                          
019010     IF REQU-IDLEGSEL-KEY NOT = SPACE OR NOT = ALL '+'                    
019200     AND REQU-IDLANDX3-SEND-KEY NOT = SPACE OR NOT = ALL '+'              
019310     AND REQU-IDLANDX3-REC-KEY NOT = SPACE OR NOT = ALL '+'               
019400     AND ACT-CODE-VALID                                                   
019410     AND REQU-IDMSGVER NUMERIC                                            
019500     AND (REQU-KDSTATUS-KEY = WS-CURRENT OR WS-COMING)                    
019600       CONTINUE                                                           
020200     ELSE                                                                 
020420       MOVE NOO TO KEYS-SW                                                
020430     END-IF                                                               
020440                                                                          
020450     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
020460       MOVE NOO TO KEYS-SW                                                
020470     END-IF                                                               
020500                                                                          
020600     IF KEYS-WRONG                                                        
020700       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
020710       IF REQU-IDMSGVER NUMERIC                                           
020720         CONTINUE                                                         
020730       ELSE                                                               
020740         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
020750         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
020760       END-IF                                                             
020761       IF ACT-CODE-VALID                                                  
020770         CONTINUE                                                         
020780       ELSE                                                               
020790         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
020791         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
020792       END-IF                                                             
020793       IF REQU-IDUSER = SPACE OR = ALL '+'                                
020794         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
020795         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
020796       END-IF                                                             
020797     END-IF                                                               
020798     IF KEYS-OK                                                           
020799       PERFORM DB2-SELECT-T01LSEL-TAB                                     
020800       IF LINES-FOUND                                                     
020801         CONTINUE                                                         
020802       ELSE                                                               
020803         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
020804         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
020805         MOVE NOO TO KEYS-SW                                              
020806       END-IF                                                             
020807     END-IF                                                               
020810     IF KEYS-OK                                                           
020811       MOVE REQU-IDLANDX3-SEND-KEY  TO WS-IDLANDX3-KEY                    
020812       PERFORM DB2-SELECT-T01COCO-TAB                                     
020813       IF LINES-FOUND                                                     
020814         PERFORM DB2-SELECT-T01SECO-TAB                                   
020820         IF LINES-FOUND                                                   
020821           CONTINUE                                                       
020822         ELSE                                                             
020823           MOVE NOT-FOUND       TO RESP-IDMSG-ERROR                       
020824           MOVE 'IDLANDX3-SEND' TO RESP-IDELMT-ERROR                      
020825           MOVE NOO TO KEYS-SW                                            
020826         END-IF                                                           
020827       ELSE                                                               
020828         MOVE NOT-FOUND       TO RESP-IDMSG-ERROR                         
020829         MOVE 'IDLANDX3-SEND' TO RESP-IDELMT-ERROR                        
020830         MOVE NOO TO KEYS-SW                                              
020831       END-IF                                                             
020832     END-IF                                                               
020833     IF KEYS-OK                                                           
020834       MOVE REQU-IDLANDX3-REC-KEY  TO WS-IDLANDX3-KEY                     
020835       PERFORM DB2-SELECT-T01COCO-TAB                                     
020836       IF LINES-FOUND                                                     
020837         PERFORM DB2-SELECT-T01RECO-TAB                                   
020838         IF LINES-FOUND                                                   
020839           CONTINUE                                                       
020840         ELSE                                                             
020841           MOVE NOT-FOUND      TO RESP-IDMSG-ERROR                        
020842           MOVE 'IDLANDX3-REC' TO RESP-IDELMT-ERROR                       
020843           MOVE NOO TO KEYS-SW                                            
020844         END-IF                                                           
020845       ELSE                                                               
020846         MOVE NOT-FOUND      TO RESP-IDMSG-ERROR                          
020847         MOVE 'IDLANDX3-REC' TO RESP-IDELMT-ERROR                         
020848         MOVE NOO TO KEYS-SW                                              
020849       END-IF                                                             
020850     END-IF                                                               
020920     .                                                                    
020930                                                                          
021000*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
021100 F-READ-SHOW-INFO SECTION.                                                
021300     MOVE REQU-IDLEGSEL-KEY      TO RESP-IDLEGSEL-KEY                     
021400     MOVE REQU-IDLANDX3-SEND-KEY TO RESP-IDLANDX3-SEND-KEY                
021410     MOVE REQU-IDLANDX3-REC-KEY  TO RESP-IDLANDX3-REC-KEY                 
021500     MOVE REQU-KDSTATUS-KEY      TO RESP-KDSTATUS-KEY                     
021600     MOVE T01LSEL-BELEGRAD-1     TO RESP-BELEGRAD-1                       
021700                                                                          
021800     PERFORM FA-READ-BASICDATA                                            
021900     .                                                                    
021910                                                                          
022000*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
022100 FA-READ-BASICDATA SECTION.                                               
022300     IF ACT-CODE-SEARCH                                                   
022400       PERFORM FAA-SEARCH-T01INRE                                         
022500     ELSE                                                                 
022600       IF ACT-CODE-UPDATE                                                 
022700         PERFORM FAB-UPDATE-T01INRE                                       
022800       ELSE                                                               
022900         IF ACT-CODE-INSERT                                               
023000           PERFORM FAC-INSERT-T01INRE                                     
023100         ELSE                                                             
023200           IF ACT-CODE-DELETE                                             
023300             PERFORM FAD-DELETE-T01INRE                                   
023400           END-IF                                                         
023500         END-IF                                                           
023600       END-IF                                                             
023700     END-IF                                                               
023800     .                                                                    
023810                                                                          
023900*** - SEARCH FOR RIGHT CONNECTION BETWEEN SENDING/RECEIVING               
023910***   COUNTRY AND MARK CURRENT LINE IF COMING LINE EXIST.                 
024100 FAA-SEARCH-T01INRE SECTION.                                              
024300     MOVE NOO TO WS-FLCONTROL                                             
024400                                                                          
024500     PERFORM DB2-DCL-OPN-T01INRE-CRS-1                                    
024600     PERFORM DB2-FETCH-T01INRE-CRS-1                                      
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
026401               MOVE SPACE   TO RESP-IDMSG-ERROR                           
026410               PERFORM S03-MOVE-TO-RESPOND                                
026600             ELSE                                                         
026700               MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR               
026800               PERFORM S05-MOVE-MISSING-TO-RESPOND                        
027000             END-IF                                                       
027100           END-IF                                                         
027200         END-IF                                                           
027300         PERFORM DB2-FETCH-T01INRE-CRS-1                                  
027400       END-PERFORM                                                        
027500     ELSE                                                                 
027510       PERFORM S03-MOVE-TO-RESPOND                                        
027600       MOVE INF-NO-DATA-JOINED TO RESP-IDMSG-INFO                         
027700     END-IF                                                               
027800                                                                          
027900     PERFORM DB2-CLOSE-T01INRE-CRS-1                                      
028000     .                                                                    
028010                                                                          
028100*** - CHECK IF UPDATE IS ON CURRENT OR COMING LINE                        
028200 FAB-UPDATE-T01INRE SECTION.                                              
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
029310                                                                          
029400*** - VALIDATE REQUESTED FIELDS FOR UPDATE ON INTERSTATE RULE             
029500 FABA-CHECK-UPDATE-DATA SECTION.                                          
029700     IF REQU-FLEXPORT = YES OR NOO                                        
029900       IF REQU-FLEXPORT = YES                                             
029901         MOVE JA TO REQU-FLEXPORT                                         
029910       END-IF                                                             
030000     ELSE                                                                 
030100       MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                         
030200       MOVE 'FLEXPORT' TO RESP-IDELMT-ERROR                               
030300     END-IF                                                               
030400                                                                          
030500     IF RESP-IDMSG-ERROR = SPACE                                          
030600       IF REQU-FLVAT = YES OR NOO                                         
030610         IF REQU-FLVAT = YES                                              
030700           MOVE JA TO REQU-FLVAT                                          
030710         END-IF                                                           
030800       ELSE                                                               
030900         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
031000         MOVE 'FLVAT' TO RESP-IDELMT-ERROR                                
031100       END-IF                                                             
031300     END-IF                                                               
031310                                                                          
031311     IF RESP-IDMSG-ERROR = SPACE                                          
031312       IF REQU-FLVAT-PRIV = YES OR NOO                                    
031313         IF REQU-FLVAT-PRIV = YES                                         
031314           MOVE JA TO REQU-FLVAT-PRIV                                     
031315         END-IF                                                           
031316       ELSE                                                               
031317         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
031318         MOVE 'FLVAT-PRIV' TO RESP-IDELMT-ERROR                           
031319       END-IF                                                             
031320     END-IF                                                               
031321                                                                          
031322     IF RESP-IDMSG-ERROR = SPACE                                          
031330       IF REQU-FLVATREP = YES OR NOO                                      
031340         IF REQU-FLVATREP = YES                                           
031341           MOVE JA TO REQU-FLVATREP                                       
031350         END-IF                                                           
031360       ELSE                                                               
031370         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
031380         MOVE 'FLVATREP' TO RESP-IDELMT-ERROR                             
031390       END-IF                                                             
031391     END-IF                                                               
031392                                                                          
031393     IF RESP-IDMSG-ERROR = SPACE                                          
031394       IF REQU-BETEXT-1 = ALL '+'                                         
031395         MOVE SPACE TO REQU-BETEXT-1                                      
031396       END-IF                                                             
031397     END-IF                                                               
031403                                                                          
031404     IF RESP-IDMSG-ERROR = SPACE                                          
031405       IF REQU-BETEXT-2 = ALL '+'                                         
031406         MOVE SPACE TO REQU-BETEXT-2                                      
031407       END-IF                                                             
031408     END-IF                                                               
031409                                                                          
031410     IF RESP-IDMSG-ERROR = SPACE                                          
031411       IF REQU-BETEXT-3 = ALL '+'                                         
031412         MOVE SPACE TO REQU-BETEXT-3                                      
031413       END-IF                                                             
031414     END-IF                                                               
031415                                                                          
031416     IF RESP-IDMSG-ERROR = SPACE                                          
031417       IF REQU-BETEXT-4 = ALL '+'                                         
031418         MOVE SPACE TO REQU-BETEXT-4                                      
031419       END-IF                                                             
031420     END-IF                                                               
031421                                                                          
031422     IF RESP-IDMSG-ERROR = SPACE                                          
031423       IF REQU-BETEXT-5 = ALL '+'                                         
031424         MOVE SPACE TO REQU-BETEXT-5                                      
031425       END-IF                                                             
031426     END-IF                                                               
031427                                                                          
031428     IF RESP-IDMSG-ERROR = SPACE                                          
031429       IF REQU-BETEXT-6 = ALL '+'                                         
031430         MOVE SPACE TO REQU-BETEXT-6                                      
031431       END-IF                                                             
031432     END-IF                                                               
031433                                                                          
031434     IF RESP-IDMSG-ERROR = SPACE                                          
031435       IF REQU-BETEXT-7 = ALL '+'                                         
031436         MOVE SPACE TO REQU-BETEXT-7                                      
031437       END-IF                                                             
031438     END-IF                                                               
031439                                                                          
031440     IF RESP-IDMSG-ERROR = SPACE                                          
031441       IF REQU-BETEXT-8 = ALL '+'                                         
031442         MOVE SPACE TO REQU-BETEXT-8                                      
031443       END-IF                                                             
031444     END-IF                                                               
031445                                                                          
031446     IF RESP-IDMSG-ERROR = SPACE                                          
031447       IF REQU-BETEXT-9 = ALL '+'                                         
031448         MOVE SPACE TO REQU-BETEXT-9                                      
031449       END-IF                                                             
031450     END-IF                                                               
031451                                                                          
031452     IF RESP-IDMSG-ERROR = SPACE                                          
031453       IF REQU-BETEXT-10 = ALL '+'                                        
031454         MOVE SPACE TO REQU-BETEXT-10                                     
031455       END-IF                                                             
031456     END-IF                                                               
031457                                                                          
031458     IF RESP-IDMSG-ERROR = SPACE                                          
031459       IF REQU-BETEXT-11 = ALL '+'                                        
031460         MOVE SPACE TO REQU-BETEXT-11                                     
031461       END-IF                                                             
031462     END-IF                                                               
031463                                                                          
031464     IF RESP-IDMSG-ERROR = SPACE                                          
031465       IF REQU-BETEXT-12 = ALL '+'                                        
031466         MOVE SPACE TO REQU-BETEXT-12                                     
031467       END-IF                                                             
031468     END-IF                                                               
031469                                                                          
031470     IF RESP-IDMSG-ERROR = SPACE                                          
031471       IF REQU-BETEXT-13 = ALL '+'                                        
031472         MOVE SPACE TO REQU-BETEXT-13                                     
031473       END-IF                                                             
031474     END-IF                                                               
031475                                                                          
031476     IF RESP-IDMSG-ERROR = SPACE                                          
031477       IF REQU-BETEXT-14 = ALL '+'                                        
031478         MOVE SPACE TO REQU-BETEXT-14                                     
031479       END-IF                                                             
031480     END-IF                                                               
031481                                                                          
031482     IF RESP-IDMSG-ERROR = SPACE                                          
031483       IF REQU-BETEXT-15 = ALL '+'                                        
031484         MOVE SPACE TO REQU-BETEXT-15                                     
031485       END-IF                                                             
031486     END-IF                                                               
031487                                                                          
031488     IF RESP-IDMSG-ERROR = SPACE                                          
031489       IF REQU-BETEXT-16 = ALL '+'                                        
031490         MOVE SPACE TO REQU-BETEXT-16                                     
031491       END-IF                                                             
031492     END-IF                                                               
031493                                                                          
031494     IF RESP-IDMSG-ERROR = SPACE                                          
031495       IF REQU-BETEXT-17 = ALL '+'                                        
031496         MOVE SPACE TO REQU-BETEXT-17                                     
031497       END-IF                                                             
031498     END-IF                                                               
031499                                                                          
031500     IF RESP-IDMSG-ERROR = SPACE                                          
031501       IF REQU-BETEXT-18 = ALL '+'                                        
031502         MOVE SPACE TO REQU-BETEXT-18                                     
031503       END-IF                                                             
031504     END-IF                                                               
031505                                                                          
031506     IF RESP-IDMSG-ERROR = SPACE                                          
031507       IF REQU-BETEXT-19 = ALL '+'                                        
031508         MOVE SPACE TO REQU-BETEXT-19                                     
031509       END-IF                                                             
031510     END-IF                                                               
031511                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-20 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-20                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-21 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-21                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-22 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-22                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-23 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-23                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-24 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-24                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-25 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-25                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-26 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-26                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-27 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-27                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-28 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-28                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-29 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-29                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-30 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-30                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-31 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-31                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-32 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-32                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-33 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-33                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-34 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-34                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-35 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-35                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-36 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-36                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-37 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-37                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-38 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-38                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-39 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-39                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031512     IF RESP-IDMSG-ERROR = SPACE                                          
031513       IF REQU-BETEXT-40 = ALL '+'                                        
031514         MOVE SPACE TO REQU-BETEXT-40                                     
031515       END-IF                                                             
031516     END-IF                                                               
031517                                                                          
031518     IF RESP-IDMSG-ERROR = SPACE                                          
031519       IF REQU-FLINTREP = YES OR NOO                                      
031520         IF REQU-FLINTREP = YES                                           
031521           IF REQU-IDLANDX3-SEND-KEY = REQU-IDLANDX3-REC-KEY              
031522             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
031523             MOVE 'FLINTREP' TO RESP-IDELMT-ERROR                         
031524           ELSE                                                           
031525             MOVE JA TO REQU-FLINTREP                                     
031526           END-IF                                                         
031527         END-IF                                                           
031528       ELSE                                                               
031529         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
031530         MOVE 'FLINTREP' TO RESP-IDELMT-ERROR                             
031531       END-IF                                                             
031532     END-IF                                                               
031533                                                                          
031534     IF RESP-IDMSG-ERROR = SPACE                                          
031535       IF REQU-FLCUSREP = YES OR NOO                                      
031536         IF REQU-FLCUSREP = YES                                           
031537           IF REQU-IDLANDX3-SEND-KEY = REQU-IDLANDX3-REC-KEY              
031538             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
031539             MOVE 'FLCUSREP' TO RESP-IDELMT-ERROR                         
031540           ELSE                                                           
031541             MOVE JA TO REQU-FLCUSREP                                     
031542           END-IF                                                         
031543         END-IF                                                           
031544       ELSE                                                               
031545         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
031546         MOVE 'FLCUSREP' TO RESP-IDELMT-ERROR                             
031547       END-IF                                                             
031548     END-IF                                                               
031549                                                                          
031550     IF RESP-IDMSG-ERROR = SPACE                                          
031600        IF REQU-KDVAT > SPACE                                             
031700        AND REQU-KDVAT NOT = ALL '+'                                      
031800          CONTINUE                                                        
031900        ELSE                                                              
032000          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
032100          MOVE 'KDVAT' TO RESP-IDELMT-ERROR                               
032200        END-IF                                                            
032300     END-IF                                                               
042000                                                                          
042010     IF RESP-IDMSG-ERROR = SPACE                                          
042020        IF REQU-KDVAT-SERV > SPACE                                        
042030        AND REQU-KDVAT-SERV NOT = ALL '+'                                 
042040          CONTINUE                                                        
042050        ELSE                                                              
042060          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
042070          MOVE 'KDVAT-SERV' TO RESP-IDELMT-ERROR                          
042080        END-IF                                                            
042090     END-IF                                                               
042091                                                                          
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
042760               MOVE 'DAUPPDAT' TO RESP-IDELMT-ERROR                       
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
043710                                                                          
043800*** - UPDATE CURRENT LINE ON T01INRE.                                     
043900 FABB-UPD-CURRENT SECTION.                                                
044100     PERFORM DB2-SELECT-T01INRE-TAB-CURR                                  
044200                                                                          
044300     IF LINES-FOUND                                                       
044400        PERFORM DB2-UPDATE-T01INRE-TAB-CURR                               
044500        PERFORM S04-MOVE-TO-RESPOND                                       
044600        IF REQU-FLCOMING = YES                                            
044700           MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                
044800        ELSE                                                              
044900           MOVE INF-UPDATE-OK TO RESP-IDMSG-INFO                          
045000        END-IF                                                            
045100     ELSE                                                                 
045200        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
045300     END-IF                                                               
045400     .                                                                    
045410                                                                          
045500*** - UPDATE COMING LINE ON T01INRE. IF NOO COMING LINE EXIST BUT         
045600***   COMING LINE IS CHOOSED FOR UPDATE, PROGRAM WILL INSERT ONE          
045700***   COMING LINE WITH DATA FROM CURRENT LINE BUT USER WILL SEE           
045800***   THIS AS AN UPDATE.                                                  
045900 FABC-UPD-COMING SECTION.                                                 
046100     PERFORM DB2-SELECT-T01INRE-TAB-COM                                   
046200                                                                          
046300     IF LINES-FOUND                                                       
046400       PERFORM DB2-UPDATE-T01INRE-TAB-COM                                 
046500       PERFORM S04-MOVE-TO-RESPOND                                        
046600       MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                    
046700     ELSE                                                                 
046800       PERFORM DB2-SELECT-T01INRE-TAB-CURR                                
046900       IF LINES-FOUND                                                     
047200         PERFORM DB2-INSERT-T01INRE-TAB-COM                               
047300         PERFORM S04-MOVE-TO-RESPOND                                      
047400         MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                  
047500       ELSE                                                               
047600         MOVE ERR-UPDATE-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
047700       END-IF                                                             
047800     END-IF                                                               
047900     .                                                                    
047910                                                                          
048000*** - INSERT NEW CURRENT LINE. COMING LINE COULD NOT BE INSERTED.         
048100*** - IF CURRENT LINE EXIST WITH DELETE DATE, DELETE CURRENT LINE         
048200***   PHYSICAL AND INSERT NEW CURRENT LINE.                               
048300 FAC-INSERT-T01INRE SECTION.                                              
048500     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
048600       PERFORM DB2-SELECT-T01INRE-TAB-CURR-2                              
048700       IF LINES-FOUND                                                     
048800         IF MAP-DADELDAT = WS-ACTIVE                                      
048910           MOVE ERR-ALREADY-EXIST TO RESP-IDMSG-ERROR                     
048920           MOVE 'RELATION' TO RESP-IDELMT-ERROR                           
049000         ELSE                                                             
049100           PERFORM FACA-CHECK-INSERT-DATA                                 
049200           IF RESP-IDMSG-ERROR = SPACE                                    
049300             PERFORM DB2-DELETE-T01INRE-TAB-CURR                          
049400             PERFORM DB2-INSERT-T01INRE-TAB-CURR                          
049500             PERFORM S04-MOVE-TO-RESPOND                                  
049600             MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                        
049900           END-IF                                                         
050000         END-IF                                                           
050100       ELSE                                                               
050200         PERFORM FACA-CHECK-INSERT-DATA                                   
050300         IF RESP-IDMSG-ERROR = SPACE                                      
050400           PERFORM DB2-INSERT-T01INRE-TAB-CURR                            
050500           PERFORM S04-MOVE-TO-RESPOND                                    
050600           MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                          
050700         END-IF                                                           
050800       END-IF                                                             
050900     ELSE                                                                 
051000       MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
051100     END-IF                                                               
051200     .                                                                    
051210                                                                          
051300*** - VALIDATE REQUESTED FIELDS FOR INSERT ON RECEIVING COUNTRY           
051400 FACA-CHECK-INSERT-DATA SECTION.                                          
051520     IF REQU-FLEXPORT = YES OR NOO                                        
051530       IF REQU-FLEXPORT = YES                                             
051531         MOVE JA TO REQU-FLEXPORT                                         
051540       END-IF                                                             
051550     ELSE                                                                 
051560       MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                         
051570       MOVE 'FLEXPORT' TO RESP-IDELMT-ERROR                               
051580     END-IF                                                               
051590                                                                          
051591     IF RESP-IDMSG-ERROR = SPACE                                          
051592       IF REQU-FLVAT = YES OR NOO                                         
051593         IF REQU-FLVAT = YES                                              
051594           MOVE JA TO REQU-FLVAT                                          
051595         END-IF                                                           
051596       ELSE                                                               
051597         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
051598         MOVE 'FLVAT' TO RESP-IDELMT-ERROR                                
051599       END-IF                                                             
051600     END-IF                                                               
051601                                                                          
051602     IF RESP-IDMSG-ERROR = SPACE                                          
051603       IF REQU-FLVAT-PRIV = YES OR NOO                                    
051604         IF REQU-FLVAT-PRIV = YES                                         
051605           MOVE JA TO REQU-FLVAT-PRIV                                     
051606         END-IF                                                           
051607       ELSE                                                               
051608         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
051609         MOVE 'FLVAT-PRIV' TO RESP-IDELMT-ERROR                           
051610       END-IF                                                             
051611     END-IF                                                               
051612                                                                          
051613     IF RESP-IDMSG-ERROR = SPACE                                          
051614       IF REQU-FLVATREP = YES OR NOO                                      
051615         IF REQU-FLVATREP = YES                                           
051616           MOVE JA TO REQU-FLVATREP                                       
051617         END-IF                                                           
051618       ELSE                                                               
051619         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
051620         MOVE 'FLVATREP' TO RESP-IDELMT-ERROR                             
051621       END-IF                                                             
051622     END-IF                                                               
051623                                                                          
051624     IF RESP-IDMSG-ERROR = SPACE                                          
051625       IF REQU-FLINTREP = YES OR NOO                                      
051626         IF REQU-FLINTREP = YES                                           
051627           IF REQU-IDLANDX3-SEND-KEY = REQU-IDLANDX3-REC-KEY              
051628             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
051629             MOVE 'FLINTREP' TO RESP-IDELMT-ERROR                         
051630           ELSE                                                           
051631             MOVE JA TO REQU-FLINTREP                                     
051632           END-IF                                                         
051633         END-IF                                                           
051634       ELSE                                                               
051635         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
051636         MOVE 'FLINTREP' TO RESP-IDELMT-ERROR                             
051637       END-IF                                                             
051638     END-IF                                                               
051639                                                                          
051640     IF RESP-IDMSG-ERROR = SPACE                                          
051641       IF REQU-FLCUSREP = YES OR NOO                                      
051642         IF REQU-FLCUSREP = YES                                           
051643           IF REQU-IDLANDX3-SEND-KEY = REQU-IDLANDX3-REC-KEY              
051644             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
051645             MOVE 'FLCUSREP' TO RESP-IDELMT-ERROR                         
051646           ELSE                                                           
051647             MOVE JA TO REQU-FLCUSREP                                     
051648           END-IF                                                         
051649         END-IF                                                           
051650       ELSE                                                               
051651         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
051652         MOVE 'FLCUSREP' TO RESP-IDELMT-ERROR                             
051653       END-IF                                                             
051654     END-IF                                                               
051655                                                                          
051656     IF RESP-IDMSG-ERROR = SPACE                                          
051657       IF REQU-BETEXT-1 = ALL '+'                                         
051658         MOVE SPACE TO REQU-BETEXT-1                                      
051659       END-IF                                                             
051660     END-IF                                                               
051661                                                                          
051662     IF RESP-IDMSG-ERROR = SPACE                                          
051663       IF REQU-BETEXT-2 = ALL '+'                                         
051664         MOVE SPACE TO REQU-BETEXT-2                                      
051665       END-IF                                                             
051666     END-IF                                                               
051667                                                                          
051668     IF RESP-IDMSG-ERROR = SPACE                                          
051669       IF REQU-BETEXT-3 = ALL '+'                                         
051670         MOVE SPACE TO REQU-BETEXT-3                                      
051671       END-IF                                                             
051672     END-IF                                                               
051673                                                                          
051674     IF RESP-IDMSG-ERROR = SPACE                                          
051675       IF REQU-BETEXT-4 = ALL '+'                                         
051676         MOVE SPACE TO REQU-BETEXT-4                                      
051677       END-IF                                                             
051678     END-IF                                                               
051680                                                                          
051681     IF RESP-IDMSG-ERROR = SPACE                                          
051682       IF REQU-BETEXT-5 = ALL '+'                                         
051683         MOVE SPACE TO REQU-BETEXT-5                                      
051684       END-IF                                                             
051685     END-IF                                                               
051686                                                                          
051687     IF RESP-IDMSG-ERROR = SPACE                                          
051688       IF REQU-BETEXT-6 = ALL '+'                                         
051689         MOVE SPACE TO REQU-BETEXT-6                                      
051690       END-IF                                                             
051691     END-IF                                                               
051692                                                                          
051693     IF RESP-IDMSG-ERROR = SPACE                                          
051694       IF REQU-BETEXT-7 = ALL '+'                                         
051695         MOVE SPACE TO REQU-BETEXT-7                                      
051696       END-IF                                                             
051697     END-IF                                                               
051698                                                                          
051699     IF RESP-IDMSG-ERROR = SPACE                                          
051700       IF REQU-BETEXT-8 = ALL '+'                                         
051701         MOVE SPACE TO REQU-BETEXT-8                                      
051702       END-IF                                                             
051703     END-IF                                                               
051704                                                                          
051705     IF RESP-IDMSG-ERROR = SPACE                                          
051706       IF REQU-BETEXT-9 = ALL '+'                                         
051707         MOVE SPACE TO REQU-BETEXT-9                                      
051708       END-IF                                                             
051709     END-IF                                                               
051710                                                                          
051711     IF RESP-IDMSG-ERROR = SPACE                                          
051712       IF REQU-BETEXT-10 = ALL '+'                                        
051713         MOVE SPACE TO REQU-BETEXT-10                                     
051714       END-IF                                                             
051715     END-IF                                                               
051716                                                                          
051717     IF RESP-IDMSG-ERROR = SPACE                                          
051718       IF REQU-BETEXT-11 = ALL '+'                                        
051719         MOVE SPACE TO REQU-BETEXT-11                                     
051720       END-IF                                                             
051721     END-IF                                                               
051722                                                                          
051723     IF RESP-IDMSG-ERROR = SPACE                                          
051724       IF REQU-BETEXT-12 = ALL '+'                                        
051725         MOVE SPACE TO REQU-BETEXT-12                                     
051726       END-IF                                                             
051727     END-IF                                                               
051728                                                                          
051729     IF RESP-IDMSG-ERROR = SPACE                                          
051730       IF REQU-BETEXT-13 = ALL '+'                                        
051731         MOVE SPACE TO REQU-BETEXT-13                                     
051732       END-IF                                                             
051733     END-IF                                                               
051734                                                                          
051735     IF RESP-IDMSG-ERROR = SPACE                                          
051736       IF REQU-BETEXT-14 = ALL '+'                                        
051737         MOVE SPACE TO REQU-BETEXT-14                                     
051738       END-IF                                                             
051739     END-IF                                                               
051740                                                                          
051741     IF RESP-IDMSG-ERROR = SPACE                                          
051742       IF REQU-BETEXT-15 = ALL '+'                                        
051743         MOVE SPACE TO REQU-BETEXT-15                                     
051744       END-IF                                                             
051745     END-IF                                                               
051746                                                                          
051747     IF RESP-IDMSG-ERROR = SPACE                                          
051748       IF REQU-BETEXT-16 = ALL '+'                                        
051749         MOVE SPACE TO REQU-BETEXT-16                                     
051750       END-IF                                                             
051751     END-IF                                                               
051752                                                                          
051753     IF RESP-IDMSG-ERROR = SPACE                                          
051754       IF REQU-BETEXT-17 = ALL '+'                                        
051755         MOVE SPACE TO REQU-BETEXT-17                                     
051756       END-IF                                                             
051757     END-IF                                                               
051758                                                                          
051759     IF RESP-IDMSG-ERROR = SPACE                                          
051760       IF REQU-BETEXT-18 = ALL '+'                                        
051761         MOVE SPACE TO REQU-BETEXT-18                                     
051762       END-IF                                                             
051763     END-IF                                                               
051764                                                                          
051765     IF RESP-IDMSG-ERROR = SPACE                                          
051766       IF REQU-BETEXT-19 = ALL '+'                                        
051767         MOVE SPACE TO REQU-BETEXT-19                                     
051768       END-IF                                                             
051769     END-IF                                                               
051770                                                                          
051771     IF RESP-IDMSG-ERROR = SPACE                                          
051772       IF REQU-BETEXT-20 = ALL '+'                                        
051773         MOVE SPACE TO REQU-BETEXT-20                                     
051774       END-IF                                                             
051775     END-IF                                                               
051776                                                                          
051656     IF RESP-IDMSG-ERROR = SPACE                                          
051657       IF REQU-BETEXT-21 = ALL '+'                                        
051658         MOVE SPACE TO REQU-BETEXT-21                                     
051659       END-IF                                                             
051660     END-IF                                                               
051661                                                                          
051662     IF RESP-IDMSG-ERROR = SPACE                                          
051663       IF REQU-BETEXT-22 = ALL '+'                                        
051664         MOVE SPACE TO REQU-BETEXT-22                                     
051665       END-IF                                                             
051666     END-IF                                                               
051667                                                                          
051668     IF RESP-IDMSG-ERROR = SPACE                                          
051669       IF REQU-BETEXT-23 = ALL '+'                                        
051670         MOVE SPACE TO REQU-BETEXT-23                                     
051671       END-IF                                                             
051672     END-IF                                                               
051673                                                                          
051674     IF RESP-IDMSG-ERROR = SPACE                                          
051675       IF REQU-BETEXT-24 = ALL '+'                                        
051676         MOVE SPACE TO REQU-BETEXT-24                                     
051677       END-IF                                                             
051678     END-IF                                                               
051680                                                                          
051681     IF RESP-IDMSG-ERROR = SPACE                                          
051682       IF REQU-BETEXT-25 = ALL '+'                                        
051683         MOVE SPACE TO REQU-BETEXT-25                                     
051684       END-IF                                                             
051685     END-IF                                                               
051686                                                                          
051687     IF RESP-IDMSG-ERROR = SPACE                                          
051688       IF REQU-BETEXT-26 = ALL '+'                                        
051689         MOVE SPACE TO REQU-BETEXT-26                                     
051690       END-IF                                                             
051691     END-IF                                                               
051692                                                                          
051693     IF RESP-IDMSG-ERROR = SPACE                                          
051694       IF REQU-BETEXT-27 = ALL '+'                                        
051695         MOVE SPACE TO REQU-BETEXT-27                                     
051696       END-IF                                                             
051697     END-IF                                                               
051698                                                                          
051699     IF RESP-IDMSG-ERROR = SPACE                                          
051700       IF REQU-BETEXT-28 = ALL '+'                                        
051701         MOVE SPACE TO REQU-BETEXT-28                                     
051702       END-IF                                                             
051703     END-IF                                                               
051704                                                                          
051705     IF RESP-IDMSG-ERROR = SPACE                                          
051706       IF REQU-BETEXT-29 = ALL '+'                                        
051707         MOVE SPACE TO REQU-BETEXT-29                                     
051708       END-IF                                                             
051709     END-IF                                                               
051710                                                                          
051711     IF RESP-IDMSG-ERROR = SPACE                                          
051712       IF REQU-BETEXT-30 = ALL '+'                                        
051713         MOVE SPACE TO REQU-BETEXT-30                                     
051714       END-IF                                                             
051715     END-IF                                                               
051716                                                                          
051717     IF RESP-IDMSG-ERROR = SPACE                                          
051718       IF REQU-BETEXT-31 = ALL '+'                                        
051719         MOVE SPACE TO REQU-BETEXT-31                                     
051720       END-IF                                                             
051721     END-IF                                                               
051722                                                                          
051723     IF RESP-IDMSG-ERROR = SPACE                                          
051724       IF REQU-BETEXT-32 = ALL '+'                                        
051725         MOVE SPACE TO REQU-BETEXT-32                                     
051726       END-IF                                                             
051727     END-IF                                                               
051728                                                                          
051729     IF RESP-IDMSG-ERROR = SPACE                                          
051730       IF REQU-BETEXT-33 = ALL '+'                                        
051731         MOVE SPACE TO REQU-BETEXT-33                                     
051732       END-IF                                                             
051733     END-IF                                                               
051734                                                                          
051735     IF RESP-IDMSG-ERROR = SPACE                                          
051736       IF REQU-BETEXT-34 = ALL '+'                                        
051737         MOVE SPACE TO REQU-BETEXT-34                                     
051738       END-IF                                                             
051739     END-IF                                                               
051740                                                                          
051741     IF RESP-IDMSG-ERROR = SPACE                                          
051742       IF REQU-BETEXT-35 = ALL '+'                                        
051743         MOVE SPACE TO REQU-BETEXT-35                                     
051744       END-IF                                                             
051745     END-IF                                                               
051746                                                                          
051747     IF RESP-IDMSG-ERROR = SPACE                                          
051748       IF REQU-BETEXT-36 = ALL '+'                                        
051749         MOVE SPACE TO REQU-BETEXT-36                                     
051750       END-IF                                                             
051751     END-IF                                                               
051752                                                                          
051753     IF RESP-IDMSG-ERROR = SPACE                                          
051754       IF REQU-BETEXT-37 = ALL '+'                                        
051755         MOVE SPACE TO REQU-BETEXT-37                                     
051756       END-IF                                                             
051757     END-IF                                                               
051758                                                                          
051759     IF RESP-IDMSG-ERROR = SPACE                                          
051760       IF REQU-BETEXT-38 = ALL '+'                                        
051761         MOVE SPACE TO REQU-BETEXT-38                                     
051762       END-IF                                                             
051763     END-IF                                                               
051764                                                                          
051765     IF RESP-IDMSG-ERROR = SPACE                                          
051766       IF REQU-BETEXT-39 = ALL '+'                                        
051767         MOVE SPACE TO REQU-BETEXT-39                                     
051768       END-IF                                                             
051769     END-IF                                                               
051770                                                                          
051771     IF RESP-IDMSG-ERROR = SPACE                                          
051772       IF REQU-BETEXT-40 = ALL '+'                                        
051773         MOVE SPACE TO REQU-BETEXT-40                                     
051774       END-IF                                                             
051775     END-IF                                                               
051776                                                                          
051777     IF RESP-IDMSG-ERROR = SPACE                                          
051778        IF REQU-KDVAT > SPACE                                             
051779        AND REQU-KDVAT NOT = ALL '+'                                      
051780          CONTINUE                                                        
051781        ELSE                                                              
051782          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
051783          MOVE 'KDVAT' TO RESP-IDELMT-ERROR                               
051784        END-IF                                                            
051785     END-IF                                                               
051786                                                                          
051787     IF RESP-IDMSG-ERROR = SPACE                                          
051788        IF REQU-KDVAT-SERV > SPACE                                        
051789        AND REQU-KDVAT-SERV NOT = ALL '+'                                 
051790          CONTINUE                                                        
051791        ELSE                                                              
051800          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
051900          MOVE 'KDVAT-SERV' TO RESP-IDELMT-ERROR                          
052000        END-IF                                                            
052100     END-IF                                                               
052200                                                                          
064000     IF RESP-IDMSG-ERROR = SPACE                                          
064100       MOVE WS-CURRENT-DATE TO MAP-DAREGDAT                               
064200       MOVE WS-ACTIVE       TO MAP-DAUPPDAT                               
064300       MOVE WS-ACTIVE       TO MAP-DADELDAT                               
064500     END-IF                                                               
064600     .                                                                    
064610                                                                          
064700*** - DELETE ON CURRENT LINE = UPDATE IN TABLE T01INRE WITH               
064800***   CURRENT DATE AS DELETE DATE.                                        
064900*** - DELETE ON COMING LINE IS A PHYSICAL DELETE FROM TABLE               
065000***   T01INRE.                                                            
065100 FAD-DELETE-T01INRE SECTION.                                              
065300     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
065900       PERFORM DB2-SELECT-T01INRE-TAB-CURR                                
066000       IF LINES-FOUND                                                     
066100         PERFORM DB2-UPDATE-T01INRE-TAB-DEL                               
066200         PERFORM S04-MOVE-TO-RESPOND                                      
066300         MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                            
066400         PERFORM DB2-SELECT-T01INRE-TAB-COM                               
066500         IF LINES-FOUND                                                   
066600           PERFORM DB2-DELETE-T01INRE-TAB-COM                             
066700         END-IF                                                           
066800       ELSE                                                               
066900         MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
067000       END-IF                                                             
067200     ELSE                                                                 
067300       PERFORM DB2-SELECT-T01INRE-TAB-COM                                 
067400       IF LINES-FOUND                                                     
067500         PERFORM DB2-DELETE-T01INRE-TAB-COM                               
067600         PERFORM S04-MOVE-TO-RESPOND                                      
067610         MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                            
067700       ELSE                                                               
067800         MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
067900       END-IF                                                             
068000     END-IF                                                               
068100     .                                                                    
068110                                                                          
068200*    --- DISPATCHER SECTIONS                                              
068300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
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
069710                                                                          
069800 S02-RETURN-RESPONSE SECTION.                                             
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
071110                                                                          
071200*    --- MOVE TO OUTPUT SECTIONS                                          
071300 S03-MOVE-TO-RESPOND SECTION.                                             
071500     IF ACT-CODE-SEARCH                                                   
071600       MOVE MAP-RESP-FLEXPORT TO RESP-FLEXPORT                            
071610       IF RESP-FLEXPORT = JA                                              
071620         MOVE YES TO RESP-FLEXPORT                                        
071630       END-IF                                                             
071700       MOVE MAP-RESP-FLVAT    TO RESP-FLVAT                               
071710       IF RESP-FLVAT = JA                                                 
071720         MOVE YES TO RESP-FLVAT                                           
071730       END-IF                                                             
071740       MOVE MAP-RESP-FLVAT-PRIV TO RESP-FLVAT-PRIV                        
071750       IF RESP-FLVAT-PRIV = JA                                            
071760         MOVE YES TO RESP-FLVAT-PRIV                                      
071770       END-IF                                                             
071800       MOVE MAP-RESP-FLVATREP TO RESP-FLVATREP                            
071801       IF RESP-FLVATREP = JA                                              
071802         MOVE YES TO RESP-FLVATREP                                        
071803       END-IF                                                             
071810       MOVE MAP-RESP-FLINTREP TO RESP-FLINTREP                            
071820       IF RESP-FLINTREP = JA                                              
071830         MOVE YES TO RESP-FLINTREP                                        
071840       END-IF                                                             
071850       MOVE MAP-RESP-FLCUSREP TO RESP-FLCUSREP                            
071860       IF RESP-FLCUSREP = JA                                              
071870         MOVE YES TO RESP-FLCUSREP                                        
071880       END-IF                                                             
071900       MOVE MAP-RESP-KDVAT      TO RESP-KDVAT                             
072000       MOVE MAP-RESP-KDVAT-SERV TO RESP-KDVAT-SERV                        
072800       MOVE MAP-DAREGDAT        TO RESP-DAREGDAT                          
072900       MOVE MAP-DAUPPDAT        TO RESP-DAUPPDAT                          
073000       MOVE MAP-DADELDAT        TO RESP-DADELDAT                          
073100       MOVE MAP-RESP-IDUSER     TO RESP-IDUSER                            
073200       MOVE NOO                 TO RESP-FLCOMING                          
073201       MOVE MAP-RESP-BETEXT-1   TO RESP-BETEXT-1                          
073202       MOVE MAP-RESP-BETEXT-2   TO RESP-BETEXT-2                          
073203       MOVE MAP-RESP-BETEXT-3   TO RESP-BETEXT-3                          
073204       MOVE MAP-RESP-BETEXT-4   TO RESP-BETEXT-4                          
073205       MOVE MAP-RESP-BETEXT-5   TO RESP-BETEXT-5                          
073206       MOVE MAP-RESP-BETEXT-6   TO RESP-BETEXT-6                          
073207       MOVE MAP-RESP-BETEXT-7   TO RESP-BETEXT-7                          
073208       MOVE MAP-RESP-BETEXT-8   TO RESP-BETEXT-8                          
073209       MOVE MAP-RESP-BETEXT-9   TO RESP-BETEXT-9                          
073210       MOVE MAP-RESP-BETEXT-10  TO RESP-BETEXT-10                         
073211       MOVE MAP-RESP-BETEXT-11  TO RESP-BETEXT-11                         
073212       MOVE MAP-RESP-BETEXT-12  TO RESP-BETEXT-12                         
073213       MOVE MAP-RESP-BETEXT-13  TO RESP-BETEXT-13                         
073214       MOVE MAP-RESP-BETEXT-14  TO RESP-BETEXT-14                         
073215       MOVE MAP-RESP-BETEXT-15  TO RESP-BETEXT-15                         
073216       MOVE MAP-RESP-BETEXT-16  TO RESP-BETEXT-16                         
073217       MOVE MAP-RESP-BETEXT-17  TO RESP-BETEXT-17                         
073218       MOVE MAP-RESP-BETEXT-18  TO RESP-BETEXT-18                         
073219       MOVE MAP-RESP-BETEXT-19  TO RESP-BETEXT-19                         
073220       MOVE MAP-RESP-BETEXT-20  TO RESP-BETEXT-20                         
073220       MOVE MAP-RESP-BETEXT-21  TO RESP-BETEXT-21                         
073220       MOVE MAP-RESP-BETEXT-22  TO RESP-BETEXT-22                         
073220       MOVE MAP-RESP-BETEXT-23  TO RESP-BETEXT-23                         
073220       MOVE MAP-RESP-BETEXT-24  TO RESP-BETEXT-24                         
073220       MOVE MAP-RESP-BETEXT-25  TO RESP-BETEXT-25                         
073220       MOVE MAP-RESP-BETEXT-26  TO RESP-BETEXT-26                         
073220       MOVE MAP-RESP-BETEXT-27  TO RESP-BETEXT-27                         
073220       MOVE MAP-RESP-BETEXT-28  TO RESP-BETEXT-28                         
073220       MOVE MAP-RESP-BETEXT-29  TO RESP-BETEXT-29                         
073220       MOVE MAP-RESP-BETEXT-30  TO RESP-BETEXT-30                         
073220       MOVE MAP-RESP-BETEXT-31  TO RESP-BETEXT-31                         
073220       MOVE MAP-RESP-BETEXT-32  TO RESP-BETEXT-32                         
073220       MOVE MAP-RESP-BETEXT-33  TO RESP-BETEXT-33                         
073220       MOVE MAP-RESP-BETEXT-34  TO RESP-BETEXT-34                         
073220       MOVE MAP-RESP-BETEXT-35  TO RESP-BETEXT-35                         
073220       MOVE MAP-RESP-BETEXT-36  TO RESP-BETEXT-36                         
073220       MOVE MAP-RESP-BETEXT-37  TO RESP-BETEXT-37                         
073220       MOVE MAP-RESP-BETEXT-38  TO RESP-BETEXT-38                         
073220       MOVE MAP-RESP-BETEXT-39  TO RESP-BETEXT-39                         
073220       MOVE MAP-RESP-BETEXT-40  TO RESP-BETEXT-40                         
073230     END-IF                                                               
073312     .                                                                    
073314                                                                          
073320 S04-MOVE-TO-RESPOND SECTION.                                             
073400     IF ACT-CODE-UPDATE                                                   
073500       MOVE REQU-FLEXPORT        TO RESP-FLEXPORT                         
073510       IF RESP-FLEXPORT = JA                                              
073520         MOVE YES TO RESP-FLEXPORT                                        
073530       END-IF                                                             
073600       MOVE REQU-FLVAT           TO RESP-FLVAT                            
073601       IF RESP-FLVAT = JA                                                 
073602         MOVE YES TO RESP-FLVAT                                           
073603       END-IF                                                             
073604       MOVE REQU-FLVAT-PRIV      TO RESP-FLVAT-PRIV                       
073605       IF RESP-FLVAT-PRIV = JA                                            
073606         MOVE YES TO RESP-FLVAT-PRIV                                      
073607       END-IF                                                             
073610       MOVE REQU-FLVATREP        TO RESP-FLVATREP                         
073620       IF RESP-FLVATREP = JA                                              
073630         MOVE YES TO RESP-FLVATREP                                        
073640       END-IF                                                             
073700       MOVE REQU-FLINTREP        TO RESP-FLINTREP                         
073701       IF RESP-FLINTREP = JA                                              
073702         MOVE YES TO RESP-FLINTREP                                        
073703       END-IF                                                             
073704       MOVE REQU-FLCUSREP        TO RESP-FLCUSREP                         
073705       IF RESP-FLCUSREP = JA                                              
073706         MOVE YES TO RESP-FLCUSREP                                        
073707       END-IF                                                             
073710       MOVE REQU-KDVAT           TO RESP-KDVAT                            
073720       MOVE REQU-KDVAT-SERV      TO RESP-KDVAT-SERV                       
074700       MOVE MAP-DAREGDAT         TO RESP-DAREGDAT                         
074800       MOVE MAP-DAUPPDAT         TO RESP-DAUPPDAT                         
074900       MOVE MAP-DADELDAT         TO RESP-DADELDAT                         
075000       MOVE REQU-IDUSER          TO RESP-IDUSER                           
075010       MOVE REQU-BETEXT-1        TO RESP-BETEXT-1                         
075020       MOVE REQU-BETEXT-2        TO RESP-BETEXT-2                         
075030       MOVE REQU-BETEXT-3        TO RESP-BETEXT-3                         
075040       MOVE REQU-BETEXT-4        TO RESP-BETEXT-4                         
075050       MOVE REQU-BETEXT-5        TO RESP-BETEXT-5                         
075060       MOVE REQU-BETEXT-6        TO RESP-BETEXT-6                         
075070       MOVE REQU-BETEXT-7        TO RESP-BETEXT-7                         
075080       MOVE REQU-BETEXT-8        TO RESP-BETEXT-8                         
075090       MOVE REQU-BETEXT-9        TO RESP-BETEXT-9                         
075091       MOVE REQU-BETEXT-10       TO RESP-BETEXT-10                        
075092       MOVE REQU-BETEXT-11       TO RESP-BETEXT-11                        
075093       MOVE REQU-BETEXT-12       TO RESP-BETEXT-12                        
075094       MOVE REQU-BETEXT-13       TO RESP-BETEXT-13                        
075095       MOVE REQU-BETEXT-14       TO RESP-BETEXT-14                        
075096       MOVE REQU-BETEXT-15       TO RESP-BETEXT-15                        
075097       MOVE REQU-BETEXT-16       TO RESP-BETEXT-16                        
075098       MOVE REQU-BETEXT-17       TO RESP-BETEXT-17                        
075099       MOVE REQU-BETEXT-18       TO RESP-BETEXT-18                        
075100       MOVE REQU-BETEXT-19       TO RESP-BETEXT-19                        
075101       MOVE REQU-BETEXT-20       TO RESP-BETEXT-20                        
075101       MOVE REQU-BETEXT-21       TO RESP-BETEXT-21                        
075101       MOVE REQU-BETEXT-22       TO RESP-BETEXT-22                        
075101       MOVE REQU-BETEXT-23       TO RESP-BETEXT-23                        
075101       MOVE REQU-BETEXT-24       TO RESP-BETEXT-24                        
075101       MOVE REQU-BETEXT-25       TO RESP-BETEXT-25                        
075101       MOVE REQU-BETEXT-26       TO RESP-BETEXT-26                        
075101       MOVE REQU-BETEXT-27       TO RESP-BETEXT-27                        
075101       MOVE REQU-BETEXT-28       TO RESP-BETEXT-28                        
075101       MOVE REQU-BETEXT-29       TO RESP-BETEXT-29                        
075101       MOVE REQU-BETEXT-30       TO RESP-BETEXT-30                        
075101       MOVE REQU-BETEXT-31       TO RESP-BETEXT-31                        
075101       MOVE REQU-BETEXT-32       TO RESP-BETEXT-32                        
075101       MOVE REQU-BETEXT-33       TO RESP-BETEXT-33                        
075101       MOVE REQU-BETEXT-34       TO RESP-BETEXT-34                        
075101       MOVE REQU-BETEXT-35       TO RESP-BETEXT-35                        
075101       MOVE REQU-BETEXT-36       TO RESP-BETEXT-36                        
075101       MOVE REQU-BETEXT-37       TO RESP-BETEXT-37                        
075101       MOVE REQU-BETEXT-38       TO RESP-BETEXT-38                        
075101       MOVE REQU-BETEXT-39       TO RESP-BETEXT-39                        
075101       MOVE REQU-BETEXT-40       TO RESP-BETEXT-40                        
075110       IF REQU-KDSTATUS-KEY = WS-CURRENT                                  
075200         MOVE REQU-FLCOMING TO RESP-FLCOMING                              
075300       ELSE                                                               
075400         MOVE REQU-FLCOMING TO RESP-FLCOMING                              
075410* DONT SHOW DAREGDAT WHEN UPDATING ON COMING VERSION                      
075420         MOVE ZERO TO RESP-DAREGDAT                                       
075500       END-IF                                                             
075600     ELSE                                                                 
075700       IF ACT-CODE-INSERT                                                 
075800         MOVE REQU-FLEXPORT TO RESP-FLEXPORT                              
075810         IF RESP-FLEXPORT = JA                                            
075820           MOVE YES TO RESP-FLEXPORT                                      
075830         END-IF                                                           
075900         MOVE REQU-FLVAT      TO RESP-FLVAT                               
075910         IF RESP-FLVAT = JA                                               
075920           MOVE YES TO RESP-FLVAT                                         
075930         END-IF                                                           
075940         MOVE REQU-FLVAT-PRIV TO RESP-FLVAT-PRIV                          
075950         IF RESP-FLVAT-PRIV = JA                                          
075960           MOVE YES TO RESP-FLVAT-PRIV                                    
075970         END-IF                                                           
076000         MOVE REQU-FLVATREP TO RESP-FLVATREP                              
076001         IF RESP-FLVATREP = JA                                            
076002           MOVE YES TO RESP-FLVATREP                                      
076003         END-IF                                                           
076010         MOVE REQU-FLINTREP TO RESP-FLINTREP                              
076020         IF RESP-FLINTREP = JA                                            
076030           MOVE YES TO RESP-FLINTREP                                      
076040         END-IF                                                           
076050         MOVE REQU-FLCUSREP TO RESP-FLCUSREP                              
076060         IF RESP-FLCUSREP = JA                                            
076070           MOVE YES TO RESP-FLCUSREP                                      
076080         END-IF                                                           
076100         MOVE REQU-KDVAT      TO RESP-KDVAT                               
076200         MOVE REQU-KDVAT-SERV TO RESP-KDVAT-SERV                          
077000         MOVE MAP-DAREGDAT    TO RESP-DAREGDAT                            
077100         MOVE MAP-DAUPPDAT    TO RESP-DAUPPDAT                            
077200         MOVE MAP-DADELDAT    TO RESP-DADELDAT                            
077300         MOVE REQU-IDUSER     TO RESP-IDUSER                              
077310         MOVE REQU-BETEXT-1        TO RESP-BETEXT-1                       
077320         MOVE REQU-BETEXT-2        TO RESP-BETEXT-2                       
077330         MOVE REQU-BETEXT-3        TO RESP-BETEXT-3                       
077340         MOVE REQU-BETEXT-4        TO RESP-BETEXT-4                       
077350         MOVE REQU-BETEXT-5        TO RESP-BETEXT-5                       
077360         MOVE REQU-BETEXT-6        TO RESP-BETEXT-6                       
077370         MOVE REQU-BETEXT-7        TO RESP-BETEXT-7                       
077380         MOVE REQU-BETEXT-8        TO RESP-BETEXT-8                       
077390         MOVE REQU-BETEXT-9        TO RESP-BETEXT-9                       
077391         MOVE REQU-BETEXT-10       TO RESP-BETEXT-10                      
077392         MOVE REQU-BETEXT-11       TO RESP-BETEXT-11                      
077393         MOVE REQU-BETEXT-12       TO RESP-BETEXT-12                      
077394         MOVE REQU-BETEXT-13       TO RESP-BETEXT-13                      
077395         MOVE REQU-BETEXT-14       TO RESP-BETEXT-14                      
077396         MOVE REQU-BETEXT-15       TO RESP-BETEXT-15                      
077397         MOVE REQU-BETEXT-16       TO RESP-BETEXT-16                      
077398         MOVE REQU-BETEXT-17       TO RESP-BETEXT-17                      
077399         MOVE REQU-BETEXT-18       TO RESP-BETEXT-18                      
077400         MOVE REQU-BETEXT-19       TO RESP-BETEXT-19                      
077401         MOVE REQU-BETEXT-20       TO RESP-BETEXT-20                      
077401         MOVE REQU-BETEXT-21       TO RESP-BETEXT-21                      
077401         MOVE REQU-BETEXT-22       TO RESP-BETEXT-22                      
077401         MOVE REQU-BETEXT-23       TO RESP-BETEXT-23                      
077401         MOVE REQU-BETEXT-24       TO RESP-BETEXT-24                      
077401         MOVE REQU-BETEXT-25       TO RESP-BETEXT-25                      
077401         MOVE REQU-BETEXT-26       TO RESP-BETEXT-26                      
077401         MOVE REQU-BETEXT-27       TO RESP-BETEXT-27                      
077401         MOVE REQU-BETEXT-28       TO RESP-BETEXT-28                      
077401         MOVE REQU-BETEXT-29       TO RESP-BETEXT-29                      
077401         MOVE REQU-BETEXT-30       TO RESP-BETEXT-30                      
077401         MOVE REQU-BETEXT-31       TO RESP-BETEXT-31                      
077401         MOVE REQU-BETEXT-32       TO RESP-BETEXT-32                      
077401         MOVE REQU-BETEXT-33       TO RESP-BETEXT-33                      
077401         MOVE REQU-BETEXT-34       TO RESP-BETEXT-34                      
077401         MOVE REQU-BETEXT-35       TO RESP-BETEXT-35                      
077401         MOVE REQU-BETEXT-36       TO RESP-BETEXT-36                      
077401         MOVE REQU-BETEXT-37       TO RESP-BETEXT-37                      
077401         MOVE REQU-BETEXT-38       TO RESP-BETEXT-38                      
077401         MOVE REQU-BETEXT-39       TO RESP-BETEXT-39                      
077401         MOVE REQU-BETEXT-40       TO RESP-BETEXT-40                      
077410         MOVE NOO             TO RESP-FLCOMING                            
077500       ELSE                                                               
077600         IF ACT-CODE-DELETE                                               
077700           MOVE SPACE             TO RESP-FLEXPORT                        
077800           MOVE SPACE             TO RESP-FLVAT                           
077810           MOVE SPACE             TO RESP-FLVAT-PRIV                      
077900           MOVE SPACE             TO RESP-FLVATREP                        
078000           MOVE SPACE             TO RESP-FLINTREP                        
078010           MOVE SPACE             TO RESP-FLCUSREP                        
078100           MOVE SPACE             TO RESP-KDVAT                           
078200           MOVE SPACE             TO RESP-KDVAT-SERV                      
078900           MOVE ZERO              TO RESP-DAREGDAT                        
079000           MOVE ZERO              TO RESP-DAUPPDAT                        
079100           MOVE WS-CURRENT-DATE   TO RESP-DADELDAT                        
079200           MOVE REQU-IDUSER       TO RESP-IDUSER                          
079210           MOVE SPACE             TO RESP-BETEXT-1                        
079220           MOVE SPACE             TO RESP-BETEXT-2                        
079230           MOVE SPACE             TO RESP-BETEXT-3                        
079240           MOVE SPACE             TO RESP-BETEXT-4                        
079250           MOVE SPACE             TO RESP-BETEXT-5                        
079260           MOVE SPACE             TO RESP-BETEXT-6                        
079270           MOVE SPACE             TO RESP-BETEXT-7                        
079280           MOVE SPACE             TO RESP-BETEXT-8                        
079290           MOVE SPACE             TO RESP-BETEXT-9                        
079291           MOVE SPACE             TO RESP-BETEXT-10                       
079292           MOVE SPACE             TO RESP-BETEXT-11                       
079293           MOVE SPACE             TO RESP-BETEXT-12                       
079294           MOVE SPACE             TO RESP-BETEXT-13                       
079295           MOVE SPACE             TO RESP-BETEXT-14                       
079296           MOVE SPACE             TO RESP-BETEXT-15                       
079297           MOVE SPACE             TO RESP-BETEXT-16                       
079298           MOVE SPACE             TO RESP-BETEXT-17                       
079299           MOVE SPACE             TO RESP-BETEXT-18                       
079300           MOVE SPACE             TO RESP-BETEXT-19                       
079301           MOVE SPACE             TO RESP-BETEXT-20                       
079301           MOVE SPACE             TO RESP-BETEXT-21                       
079301           MOVE SPACE             TO RESP-BETEXT-22                       
079301           MOVE SPACE             TO RESP-BETEXT-23                       
079301           MOVE SPACE             TO RESP-BETEXT-24                       
079301           MOVE SPACE             TO RESP-BETEXT-25                       
079301           MOVE SPACE             TO RESP-BETEXT-26                       
079301           MOVE SPACE             TO RESP-BETEXT-27                       
079301           MOVE SPACE             TO RESP-BETEXT-28                       
079301           MOVE SPACE             TO RESP-BETEXT-29                       
079301           MOVE SPACE             TO RESP-BETEXT-30                       
079301           MOVE SPACE             TO RESP-BETEXT-31                       
079301           MOVE SPACE             TO RESP-BETEXT-32                       
079301           MOVE SPACE             TO RESP-BETEXT-33                       
079301           MOVE SPACE             TO RESP-BETEXT-34                       
079301           MOVE SPACE             TO RESP-BETEXT-35                       
079301           MOVE SPACE             TO RESP-BETEXT-36                       
079301           MOVE SPACE             TO RESP-BETEXT-37                       
079301           MOVE SPACE             TO RESP-BETEXT-38                       
079301           MOVE SPACE             TO RESP-BETEXT-39                       
079301           MOVE SPACE             TO RESP-BETEXT-40                       
079310           MOVE NOO               TO RESP-FLCOMING                        
079400         END-IF                                                           
079500       END-IF                                                             
079600     END-IF                                                               
079800     .                                                                    
079801                                                                          
079810 S05-MOVE-MISSING-TO-RESPOND SECTION.                                     
079820     MOVE SPACE             TO RESP-FLCOMING                              
079821                               RESP-FLEXPORT                              
079822                               RESP-FLVAT                                 
079823                               RESP-FLVAT-PRIV                            
079826                               RESP-FLVATREP                              
079827                               RESP-FLINTREP                              
079828                               RESP-FLCUSREP                              
079829                               RESP-KDVAT                                 
079830                               RESP-KDVAT-SERV                            
079831                               RESP-IDUSER                                
079832                               RESP-BETEXT-1                              
079833                               RESP-BETEXT-2                              
079834                               RESP-BETEXT-3                              
079835                               RESP-BETEXT-4                              
079836                               RESP-BETEXT-5                              
079837                               RESP-BETEXT-6                              
079838                               RESP-BETEXT-7                              
079839                               RESP-BETEXT-8                              
079840                               RESP-BETEXT-9                              
079841                               RESP-BETEXT-10                             
079842                               RESP-BETEXT-11                             
079843                               RESP-BETEXT-12                             
079844                               RESP-BETEXT-13                             
079845                               RESP-BETEXT-14                             
079846                               RESP-BETEXT-15                             
079847                               RESP-BETEXT-16                             
079848                               RESP-BETEXT-17                             
079849                               RESP-BETEXT-18                             
079850                               RESP-BETEXT-19                             
079851                               RESP-BETEXT-20                             
079851                               RESP-BETEXT-21                             
079851                               RESP-BETEXT-22                             
079851                               RESP-BETEXT-23                             
079851                               RESP-BETEXT-24                             
079851                               RESP-BETEXT-25                             
079851                               RESP-BETEXT-26                             
079851                               RESP-BETEXT-27                             
079851                               RESP-BETEXT-28                             
079851                               RESP-BETEXT-29                             
079851                               RESP-BETEXT-30                             
079851                               RESP-BETEXT-31                             
079851                               RESP-BETEXT-32                             
079851                               RESP-BETEXT-33                             
079851                               RESP-BETEXT-34                             
079851                               RESP-BETEXT-35                             
079851                               RESP-BETEXT-36                             
079851                               RESP-BETEXT-37                             
079851                               RESP-BETEXT-38                             
079851                               RESP-BETEXT-39                             
079851                               RESP-BETEXT-40                             
079852     MOVE ZERO              TO RESP-DAREGDAT                              
079853                               RESP-DAUPPDAT                              
079860                               RESP-DADELDAT                              
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
081701                                                                          
081710 DB2-SELECT-T01COCO-TAB   SECTION.                                        
081730     MOVE 000100 TO GOOD-SQLCODECODES                                     
081740                                                                          
081750     EXEC SQL                                                             
081760        SELECT  IDLANDX3                                                  
081770                                                                          
081780        INTO   :T01COCO-IDLANDX3                                          
081790                                                                          
081791        FROM    T01COCO                                                   
081792                                                                          
081793        WHERE   IDLANDX3 = :WS-IDLANDX3-KEY                               
081795     END-EXEC                                                             
081796                                                                          
081797     MOVE SQLCODE TO SQLCODE-WS                                           
081798     PERFORM DB2-STATUS-CHECK                                             
081799     .                                                                    
081900                                                                          
081910 DB2-SELECT-T01SECO-TAB   SECTION.                                        
082000     MOVE 000100 TO GOOD-SQLCODECODES                                     
082100                                                                          
082200     EXEC SQL                                                             
082300        SELECT IDLANDX3                                                   
082400                                                                          
082500        INTO  :T01SECO-IDLANDX3                                           
082600                                                                          
082700        FROM   T01SECO                                                    
082800                                                                          
082900        WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                              
083000        AND    IDLANDX3 = :REQU-IDLANDX3-SEND-KEY                         
083100        AND    KDSTATUS = :WS-CURRENT                                     
083110        AND    DADELDAT = :WS-ACTIVE                                      
083200     END-EXEC                                                             
083300                                                                          
083400     MOVE SQLCODE TO SQLCODE-WS                                           
083500     PERFORM DB2-STATUS-CHECK                                             
083600     .                                                                    
083620                                                                          
083621 DB2-SELECT-T01RECO-TAB   SECTION.                                        
083630     MOVE 000100 TO GOOD-SQLCODECODES                                     
083640                                                                          
083650     EXEC SQL                                                             
083660        SELECT IDLANDX3                                                   
083670                                                                          
083680        INTO  :T01RECO-IDLANDX3                                           
083690                                                                          
083691        FROM   T01RECO                                                    
083692                                                                          
083693        WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                              
083694        AND    IDLANDX3 = :REQU-IDLANDX3-REC-KEY                          
083696        AND    KDSTATUS = :WS-CURRENT                                     
083697        AND    DADELDAT = :WS-ACTIVE                                      
083698     END-EXEC                                                             
083699                                                                          
083700     MOVE SQLCODE TO SQLCODE-WS                                           
083701     PERFORM DB2-STATUS-CHECK                                             
083702     .                                                                    
083703                                                                          
083710* * * * * * * * * * * * - CURSOR-1 - * * * * * * * * * * * * * * *        
083800 DB2-DCL-OPN-T01INRE-CRS-1 SECTION.                                       
084000     MOVE 000100 TO GOOD-SQLCODECODES                                     
084100                                                                          
084200     EXEC SQL                                                             
084300         DECLARE T01INRE-CRS-1 CURSOR WITH HOLD FOR                       
084400                                                                          
084500           SELECT  KDSTATUS                                               
084600                 , FLEXPORT                                               
084800                 , FLVAT                                                  
084801                 , FLVAT_PRIV                                             
084810                 , FLVATREP                                               
084900                 , FLINTREP                                               
085000                 , KDVAT                                                  
085100                 , KDVAT_SERV                                             
085800                 , DAREGDAT                                               
085900                 , DAUPPDAT                                               
086000                 , DADELDAT                                               
086100                 , IDUSER                                                 
086110                 , FLCUSREP                                               
086120                 , BETEXT_1                                               
086130                 , BETEXT_2                                               
086140                 , BETEXT_3                                               
086150                 , BETEXT_4                                               
086160                 , BETEXT_5                                               
086170                 , BETEXT_6                                               
086180                 , BETEXT_7                                               
086190                 , BETEXT_8                                               
086191                 , BETEXT_9                                               
086192                 , BETEXT_10                                              
086193                 , BETEXT_11                                              
086194                 , BETEXT_12                                              
086195                 , BETEXT_13                                              
086196                 , BETEXT_14                                              
086197                 , BETEXT_15                                              
086198                 , BETEXT_16                                              
086199                 , BETEXT_17                                              
086200                 , BETEXT_18                                              
086201                 , BETEXT_19                                              
086202                 , BETEXT_20                                              
086202                 , BETEXT_21                                              
086202                 , BETEXT_22                                              
086202                 , BETEXT_23                                              
086202                 , BETEXT_24                                              
086202                 , BETEXT_25                                              
086202                 , BETEXT_26                                              
086202                 , BETEXT_27                                              
086202                 , BETEXT_28                                              
086202                 , BETEXT_29                                              
086202                 , BETEXT_30                                              
086202                 , BETEXT_31                                              
086202                 , BETEXT_32                                              
086202                 , BETEXT_33                                              
086202                 , BETEXT_34                                              
086202                 , BETEXT_35                                              
086202                 , BETEXT_36                                              
086202                 , BETEXT_37                                              
086202                 , BETEXT_38                                              
086202                 , BETEXT_39                                              
086202                 , BETEXT_40                                              
086210                                                                          
086300           FROM     T01INRE                                               
086400                                                                          
086500           WHERE IDLEGSEL      = :REQU-IDLEGSEL-KEY                       
086600           AND   IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                  
086610           AND   IDLANDX3_REC  = :REQU-IDLANDX3-REC-KEY                   
086800           AND   KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING              
086810           AND   DADELDAT      = :WS-ACTIVE                               
086900                                                                          
087300     END-EXEC                                                             
087400                                                                          
087500     MOVE 000100 TO GOOD-SQLCODECODES                                     
087600                                                                          
087700     EXEC SQL                                                             
087800        OPEN T01INRE-CRS-1                                                
087900     END-EXEC                                                             
088000                                                                          
088100     MOVE SQLCODE TO SQLCODE-WS                                           
088200     PERFORM DB2-STATUS-CHECK                                             
088300     .                                                                    
088500                                                                          
088510 DB2-FETCH-T01INRE-CRS-1 SECTION.                                         
088600     MOVE 000100  TO GOOD-SQLCODECODES                                    
088700                                                                          
088800     EXEC SQL                                                             
088900         FETCH T01INRE-CRS-1                                              
089000                                                                          
089100         INTO :MAP-KDSTATUS                                               
089200            , :MAP-RESP-FLEXPORT                                          
089300            , :MAP-RESP-FLVAT                                             
089310            , :MAP-RESP-FLVAT-PRIV                                        
089400            , :MAP-RESP-FLVATREP                                          
089410            , :MAP-RESP-FLINTREP                                          
089500            , :MAP-RESP-KDVAT                                             
089600            , :MAP-RESP-KDVAT-SERV                                        
090400            , :MAP-DAREGDAT                                               
090500            , :MAP-DAUPPDAT                                               
090600            , :MAP-DADELDAT                                               
090700            , :MAP-RESP-IDUSER                                            
090710            , :MAP-RESP-FLCUSREP                                          
090720            , :MAP-RESP-BETEXT-1                                          
090730            , :MAP-RESP-BETEXT-2                                          
090740            , :MAP-RESP-BETEXT-3                                          
090750            , :MAP-RESP-BETEXT-4                                          
090760            , :MAP-RESP-BETEXT-5                                          
090770            , :MAP-RESP-BETEXT-6                                          
090780            , :MAP-RESP-BETEXT-7                                          
090790            , :MAP-RESP-BETEXT-8                                          
090791            , :MAP-RESP-BETEXT-9                                          
090792            , :MAP-RESP-BETEXT-10                                         
090793            , :MAP-RESP-BETEXT-11                                         
090794            , :MAP-RESP-BETEXT-12                                         
090795            , :MAP-RESP-BETEXT-13                                         
090796            , :MAP-RESP-BETEXT-14                                         
090797            , :MAP-RESP-BETEXT-15                                         
090798            , :MAP-RESP-BETEXT-16                                         
090799            , :MAP-RESP-BETEXT-17                                         
090800            , :MAP-RESP-BETEXT-18                                         
090801            , :MAP-RESP-BETEXT-19                                         
090802            , :MAP-RESP-BETEXT-20                                         
090802            , :MAP-RESP-BETEXT-21                                         
090802            , :MAP-RESP-BETEXT-22                                         
090802            , :MAP-RESP-BETEXT-23                                         
090802            , :MAP-RESP-BETEXT-24                                         
090802            , :MAP-RESP-BETEXT-25                                         
090802            , :MAP-RESP-BETEXT-26                                         
090802            , :MAP-RESP-BETEXT-27                                         
090802            , :MAP-RESP-BETEXT-28                                         
090802            , :MAP-RESP-BETEXT-29                                         
090802            , :MAP-RESP-BETEXT-30                                         
090802            , :MAP-RESP-BETEXT-31                                         
090802            , :MAP-RESP-BETEXT-32                                         
090802            , :MAP-RESP-BETEXT-33                                         
090802            , :MAP-RESP-BETEXT-34                                         
090802            , :MAP-RESP-BETEXT-35                                         
090802            , :MAP-RESP-BETEXT-36                                         
090802            , :MAP-RESP-BETEXT-37                                         
090802            , :MAP-RESP-BETEXT-38                                         
090802            , :MAP-RESP-BETEXT-39                                         
090802            , :MAP-RESP-BETEXT-40                                         
090810     END-EXEC                                                             
090900                                                                          
091000     MOVE SQLCODE TO SQLCODE-WS                                           
091100     PERFORM DB2-STATUS-CHECK                                             
091200     .                                                                    
091400                                                                          
091410 DB2-CLOSE-T01INRE-CRS-1  SECTION.                                        
091500     EXEC SQL                                                             
091600        CLOSE T01INRE-CRS-1                                               
091700     END-EXEC                                                             
091800     .                                                                    
091810                                                                          
091900 DB2-SELECT-T01INRE-TAB-CURR SECTION.                                     
092100     MOVE 000100  TO GOOD-SQLCODECODES                                    
092200                                                                          
092300     EXEC SQL                                                             
092400         SELECT DAREGDAT                                                  
092410             ,  DADELDAT                                                  
092500                                                                          
092600         INTO  :MAP-DAREGDAT                                              
092610             , :MAP-DADELDAT                                              
092700                                                                          
092800         FROM  T01INRE                                                    
092900                                                                          
093000         WHERE IDLEGSEL      = :REQU-IDLEGSEL-KEY                         
093100         AND   IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                    
093110         AND   IDLANDX3_REC  = :REQU-IDLANDX3-REC-KEY                     
093200         AND   KDSTATUS      = :WS-CURRENT                                
093210         AND   DADELDAT      = :WS-ACTIVE                                 
093300     END-EXEC                                                             
093400                                                                          
093500     MOVE SQLCODE TO SQLCODE-WS                                           
093600     PERFORM DB2-STATUS-CHECK                                             
093700     .                                                                    
094200                                                                          
094210 DB2-UPDATE-T01INRE-TAB-CURR SECTION.                                     
094300     MOVE 000     TO GOOD-SQLCODECODES                                    
094400                                                                          
094500     EXEC SQL                                                             
094600        UPDATE T01INRE                                                    
094700           SET FLEXPORT   = :REQU-FLEXPORT                                
094800             , FLVAT      = :REQU-FLVAT                                   
094810             , FLVAT_PRIV = :REQU-FLVAT-PRIV                              
094900             , FLVATREP   = :REQU-FLVATREP                                
094910             , FLINTREP   = :REQU-FLINTREP                                
095000             , KDVAT      = :REQU-KDVAT                                   
095100             , KDVAT_SERV = :REQU-KDVAT-SERV                              
095900             , DAUPPDAT   = :MAP-DAUPPDAT                                 
096000             , IDUSER     = :REQU-IDUSER                                  
096010             , FLCUSREP   = :REQU-FLCUSREP                                
096020             , BETEXT_1   = :REQU-BETEXT-1                                
096030             , BETEXT_2   = :REQU-BETEXT-2                                
096040             , BETEXT_3   = :REQU-BETEXT-3                                
096050             , BETEXT_4   = :REQU-BETEXT-4                                
096060             , BETEXT_5   = :REQU-BETEXT-5                                
096070             , BETEXT_6   = :REQU-BETEXT-6                                
096080             , BETEXT_7   = :REQU-BETEXT-7                                
096090             , BETEXT_8   = :REQU-BETEXT-8                                
096091             , BETEXT_9   = :REQU-BETEXT-9                                
096092             , BETEXT_10  = :REQU-BETEXT-10                               
096093             , BETEXT_11  = :REQU-BETEXT-11                               
096094             , BETEXT_12  = :REQU-BETEXT-12                               
096095             , BETEXT_13  = :REQU-BETEXT-13                               
096096             , BETEXT_14  = :REQU-BETEXT-14                               
096097             , BETEXT_15  = :REQU-BETEXT-15                               
096098             , BETEXT_16  = :REQU-BETEXT-16                               
096099             , BETEXT_17  = :REQU-BETEXT-17                               
096100             , BETEXT_18  = :REQU-BETEXT-18                               
096101             , BETEXT_19  = :REQU-BETEXT-19                               
096102             , BETEXT_20  = :REQU-BETEXT-20                               
096102             , BETEXT_21  = :REQU-BETEXT-21                               
096102             , BETEXT_22  = :REQU-BETEXT-22                               
096102             , BETEXT_23  = :REQU-BETEXT-23                               
096102             , BETEXT_24  = :REQU-BETEXT-24                               
096102             , BETEXT_25  = :REQU-BETEXT-25                               
096102             , BETEXT_26  = :REQU-BETEXT-26                               
096102             , BETEXT_27  = :REQU-BETEXT-27                               
096102             , BETEXT_28  = :REQU-BETEXT-28                               
096102             , BETEXT_29  = :REQU-BETEXT-29                               
096102             , BETEXT_30  = :REQU-BETEXT-30                               
096102             , BETEXT_31  = :REQU-BETEXT-31                               
096102             , BETEXT_32  = :REQU-BETEXT-32                               
096102             , BETEXT_33  = :REQU-BETEXT-33                               
096102             , BETEXT_34  = :REQU-BETEXT-34                               
096102             , BETEXT_35  = :REQU-BETEXT-35                               
096102             , BETEXT_36  = :REQU-BETEXT-36                               
096102             , BETEXT_37  = :REQU-BETEXT-37                               
096102             , BETEXT_38  = :REQU-BETEXT-38                               
096102             , BETEXT_39  = :REQU-BETEXT-39                               
096102             , BETEXT_40  = :REQU-BETEXT-40                               
096103                                                                          
096110         WHERE IDLEGSEL      = :REQU-IDLEGSEL-KEY                         
096120         AND   IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                    
096130         AND   IDLANDX3_REC  = :REQU-IDLANDX3-REC-KEY                     
096140         AND   KDSTATUS      = :WS-CURRENT                                
096150         AND   DADELDAT      = :WS-ACTIVE                                 
096600     END-EXEC                                                             
096700                                                                          
096800     MOVE SQLCODE TO SQLCODE-WS                                           
096900     PERFORM DB2-STATUS-CHECK                                             
097000     .                                                                    
097200                                                                          
097210 DB2-UPDATE-T01INRE-TAB-DEL SECTION.                                      
097300     MOVE 000     TO GOOD-SQLCODECODES                                    
097400                                                                          
097500     EXEC SQL                                                             
097600        UPDATE T01INRE                                                    
097700           SET   DADELDAT = :WS-CURRENT-DATE                              
097800                                                                          
097900         WHERE   IDLEGSEL      = :REQU-IDLEGSEL-KEY                       
098000         AND     IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                  
098010         AND     IDLANDX3_REC  = :REQU-IDLANDX3-REC-KEY                   
098020         AND     KDSTATUS      = :WS-CURRENT                              
098100         AND     DADELDAT      = :WS-ACTIVE                               
098300     END-EXEC                                                             
098400                                                                          
098500     MOVE SQLCODE TO SQLCODE-WS                                           
098600     PERFORM DB2-STATUS-CHECK                                             
098700     .                                                                    
098900                                                                          
098910 DB2-SELECT-T01INRE-TAB-COM SECTION.                                      
099000     MOVE 000100  TO GOOD-SQLCODECODES                                    
099100                                                                          
099200     EXEC SQL                                                             
099300         SELECT DAREGDAT                                                  
099400              , DADELDAT                                                  
099500                                                                          
099600         INTO  :MAP-DAREGDAT                                              
099700             , :MAP-DADELDAT                                              
099800                                                                          
099900         FROM  T01INRE                                                    
100000                                                                          
100100         WHERE IDLEGSEL      = :REQU-IDLEGSEL-KEY                         
100200         AND   IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                    
100210         AND   IDLANDX3_REC  = :REQU-IDLANDX3-REC-KEY                     
100300         AND   KDSTATUS      = :WS-COMING                                 
100400     END-EXEC                                                             
100500                                                                          
100600     MOVE SQLCODE TO SQLCODE-WS                                           
100700     PERFORM DB2-STATUS-CHECK                                             
100800     .                                                                    
101000                                                                          
101010 DB2-UPDATE-T01INRE-TAB-COM  SECTION.                                     
101100     MOVE 000     TO GOOD-SQLCODECODES                                    
101200                                                                          
101300     EXEC SQL                                                             
101400        UPDATE T01INRE                                                    
101500           SET   FLEXPORT      = :REQU-FLEXPORT                           
101600               , FLVAT         = :REQU-FLVAT                              
101700               , FLVAT_PRIV    = :REQU-FLVAT-PRIV                         
101701               , FLVATREP      = :REQU-FLVATREP                           
101710               , FLINTREP      = :REQU-FLINTREP                           
101800               , KDVAT         = :REQU-KDVAT                              
101900               , KDVAT_SERV    = :REQU-KDVAT-SERV                         
102700               , DAUPPDAT      = :MAP-DAUPPDAT                            
102800               , IDUSER        = :REQU-IDUSER                             
102810               , FLCUSREP      = :REQU-FLCUSREP                           
102820               , BETEXT_1      = :REQU-BETEXT-1                           
102830               , BETEXT_2      = :REQU-BETEXT-2                           
102840               , BETEXT_3      = :REQU-BETEXT-3                           
102850               , BETEXT_4      = :REQU-BETEXT-4                           
102860               , BETEXT_5      = :REQU-BETEXT-5                           
102870               , BETEXT_6      = :REQU-BETEXT-6                           
102880               , BETEXT_7      = :REQU-BETEXT-7                           
102890               , BETEXT_8      = :REQU-BETEXT-8                           
102891               , BETEXT_9      = :REQU-BETEXT-9                           
102892               , BETEXT_10     = :REQU-BETEXT-10                          
102893               , BETEXT_11     = :REQU-BETEXT-11                          
102894               , BETEXT_12     = :REQU-BETEXT-12                          
102895               , BETEXT_13     = :REQU-BETEXT-13                          
102896               , BETEXT_14     = :REQU-BETEXT-14                          
102897               , BETEXT_15     = :REQU-BETEXT-15                          
102898               , BETEXT_16     = :REQU-BETEXT-16                          
102899               , BETEXT_17     = :REQU-BETEXT-17                          
102900               , BETEXT_18     = :REQU-BETEXT-18                          
102901               , BETEXT_19     = :REQU-BETEXT-19                          
102902               , BETEXT_20     = :REQU-BETEXT-20                          
102902               , BETEXT_21     = :REQU-BETEXT-21                          
102902               , BETEXT_22     = :REQU-BETEXT-22                          
102902               , BETEXT_23     = :REQU-BETEXT-23                          
102902               , BETEXT_24     = :REQU-BETEXT-24                          
102902               , BETEXT_25     = :REQU-BETEXT-25                          
102902               , BETEXT_26     = :REQU-BETEXT-26                          
102902               , BETEXT_27     = :REQU-BETEXT-27                          
102902               , BETEXT_28     = :REQU-BETEXT-28                          
102902               , BETEXT_29     = :REQU-BETEXT-29                          
102902               , BETEXT_30     = :REQU-BETEXT-30                          
102902               , BETEXT_31     = :REQU-BETEXT-31                          
102902               , BETEXT_32     = :REQU-BETEXT-32                          
102902               , BETEXT_33     = :REQU-BETEXT-33                          
102902               , BETEXT_34     = :REQU-BETEXT-34                          
102902               , BETEXT_35     = :REQU-BETEXT-35                          
102902               , BETEXT_36     = :REQU-BETEXT-36                          
102902               , BETEXT_37     = :REQU-BETEXT-37                          
102902               , BETEXT_38     = :REQU-BETEXT-38                          
102902               , BETEXT_39     = :REQU-BETEXT-39                          
102902               , BETEXT_40     = :REQU-BETEXT-40                          
102910                                                                          
103000         WHERE   IDLEGSEL      = :REQU-IDLEGSEL-KEY                       
103100         AND     IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                  
103110         AND     IDLANDX3_REC  = :REQU-IDLANDX3-REC-KEY                   
103300         AND     KDSTATUS      = :WS-COMING                               
103400     END-EXEC                                                             
103500                                                                          
103600     MOVE SQLCODE TO SQLCODE-WS                                           
103700     PERFORM DB2-STATUS-CHECK                                             
103800     .                                                                    
105720                                                                          
105721 DB2-SELECT-T01INRE-TAB-CURR-2 SECTION.                                   
105730     MOVE 000100  TO GOOD-SQLCODECODES                                    
105740                                                                          
105750     EXEC SQL                                                             
105760         SELECT DADELDAT                                                  
105770                                                                          
105780         INTO  :MAP-DADELDAT                                              
105790                                                                          
105791         FROM  T01INRE                                                    
105792                                                                          
105793         WHERE IDLEGSEL      = :REQU-IDLEGSEL-KEY                         
105794         AND   IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                    
105795         AND   IDLANDX3_REC  = :REQU-IDLANDX3-REC-KEY                     
105797         AND   KDSTATUS      = :WS-CURRENT                                
105798     END-EXEC                                                             
105799                                                                          
105800     MOVE SQLCODE TO SQLCODE-WS                                           
105801     PERFORM DB2-STATUS-CHECK                                             
105802     .                                                                    
105900                                                                          
105910 DB2-INSERT-T01INRE-TAB-CURR  SECTION.                                    
106000     MOVE 000   TO GOOD-SQLCODECODES                                      
106100                                                                          
106200     EXEC SQL                                                             
106300         INSERT INTO T01INRE                                              
106400            (IDLEGSEL                                                     
106410            ,IDLANDX3_SEND                                                
106420            ,IDLANDX3_REC                                                 
106430            ,KDSTATUS                                                     
106500            ,FLEXPORT                                                     
106600            ,FLVAT                                                        
106610            ,FLVAT_PRIV                                                   
106620            ,FLVATREP                                                     
106630            ,FLINTREP                                                     
106640            ,KDVAT                                                        
106650            ,KDVAT_SERV                                                   
106700            ,DAREGDAT                                                     
106710            ,DAUPPDAT                                                     
106720            ,DADELDAT                                                     
106730            ,IDUSER                                                       
106740            ,FLCUSREP                                                     
106750            ,BETEXT_1                                                     
106760            ,BETEXT_2                                                     
106770            ,BETEXT_3                                                     
106780            ,BETEXT_4                                                     
106790            ,BETEXT_5                                                     
106791            ,BETEXT_6                                                     
106792            ,BETEXT_7                                                     
106793            ,BETEXT_8                                                     
106794            ,BETEXT_9                                                     
106795            ,BETEXT_10                                                    
106796            ,BETEXT_11                                                    
106797            ,BETEXT_12                                                    
106798            ,BETEXT_13                                                    
106799            ,BETEXT_14                                                    
106800            ,BETEXT_15                                                    
106801            ,BETEXT_16                                                    
106802            ,BETEXT_17                                                    
106803            ,BETEXT_18                                                    
106804            ,BETEXT_19                                                    
106804            ,BETEXT_20                                                    
106804            ,BETEXT_21                                                    
106804            ,BETEXT_22                                                    
106804            ,BETEXT_23                                                    
106804            ,BETEXT_24                                                    
106804            ,BETEXT_25                                                    
106804            ,BETEXT_26                                                    
106804            ,BETEXT_27                                                    
106804            ,BETEXT_28                                                    
106804            ,BETEXT_29                                                    
106804            ,BETEXT_30                                                    
106804            ,BETEXT_31                                                    
106804            ,BETEXT_32                                                    
106804            ,BETEXT_33                                                    
106804            ,BETEXT_34                                                    
106804            ,BETEXT_35                                                    
106804            ,BETEXT_36                                                    
106804            ,BETEXT_37                                                    
106804            ,BETEXT_38                                                    
106804            ,BETEXT_39                                                    
106805            ,BETEXT_40)                                                   
106810         VALUES                                                           
106900            (:REQU-IDLEGSEL-KEY                                           
106910            ,:REQU-IDLANDX3-SEND-KEY                                      
107000            ,:REQU-IDLANDX3-REC-KEY                                       
107010            ,:WS-CURRENT                                                  
107020            ,:REQU-FLEXPORT                                               
107100            ,:REQU-FLVAT                                                  
107200            ,:REQU-FLVAT-PRIV                                             
107400            ,:REQU-FLVATREP                                               
107500            ,:REQU-FLINTREP                                               
107510            ,:REQU-KDVAT                                                  
107520            ,:REQU-KDVAT-SERV                                             
107600            ,:MAP-DAREGDAT                                                
107610            ,:MAP-DAUPPDAT                                                
107620            ,:MAP-DADELDAT                                                
107700            ,:REQU-IDUSER                                                 
107701            ,:REQU-FLCUSREP                                               
107702            ,:REQU-BETEXT-1                                               
107703            ,:REQU-BETEXT-2                                               
107704            ,:REQU-BETEXT-3                                               
107705            ,:REQU-BETEXT-4                                               
107706            ,:REQU-BETEXT-5                                               
107707            ,:REQU-BETEXT-6                                               
107708            ,:REQU-BETEXT-7                                               
107709            ,:REQU-BETEXT-8                                               
107710            ,:REQU-BETEXT-9                                               
107711            ,:REQU-BETEXT-10                                              
107712            ,:REQU-BETEXT-11                                              
107713            ,:REQU-BETEXT-12                                              
107714            ,:REQU-BETEXT-13                                              
107715            ,:REQU-BETEXT-14                                              
107716            ,:REQU-BETEXT-15                                              
107717            ,:REQU-BETEXT-16                                              
107718            ,:REQU-BETEXT-17                                              
107719            ,:REQU-BETEXT-18                                              
107720            ,:REQU-BETEXT-19                                              
107720            ,:REQU-BETEXT-20                                              
107720            ,:REQU-BETEXT-21                                              
107720            ,:REQU-BETEXT-22                                              
107720            ,:REQU-BETEXT-23                                              
107720            ,:REQU-BETEXT-24                                              
107720            ,:REQU-BETEXT-25                                              
107720            ,:REQU-BETEXT-26                                              
107720            ,:REQU-BETEXT-27                                              
107720            ,:REQU-BETEXT-28                                              
107720            ,:REQU-BETEXT-29                                              
107720            ,:REQU-BETEXT-30                                              
107720            ,:REQU-BETEXT-31                                              
107720            ,:REQU-BETEXT-32                                              
107720            ,:REQU-BETEXT-33                                              
107720            ,:REQU-BETEXT-34                                              
107720            ,:REQU-BETEXT-35                                              
107720            ,:REQU-BETEXT-36                                              
107720            ,:REQU-BETEXT-37                                              
107720            ,:REQU-BETEXT-38                                              
107720            ,:REQU-BETEXT-39                                              
107721            ,:REQU-BETEXT-40)                                             
107722     END-EXEC                                                             
107730                                                                          
107800     MOVE SQLCODE TO SQLCODE-WS                                           
107900     PERFORM DB2-STATUS-CHECK                                             
108000     .                                                                    
108010                                                                          
108100 DB2-INSERT-T01INRE-TAB-COM SECTION.                                      
108300     MOVE 000   TO GOOD-SQLCODECODES                                      
108400                                                                          
108500     EXEC SQL                                                             
108600         INSERT INTO T01INRE                                              
108700            (IDLEGSEL                                                     
108710            ,IDLANDX3_SEND                                                
108720            ,IDLANDX3_REC                                                 
108730            ,KDSTATUS                                                     
108800            ,FLEXPORT                                                     
108900            ,FLVAT                                                        
108910            ,FLVAT_PRIV                                                   
108920            ,FLVATREP                                                     
108930            ,FLINTREP                                                     
108940            ,KDVAT                                                        
108950            ,KDVAT_SERV                                                   
109000            ,DAREGDAT                                                     
109010            ,DAUPPDAT                                                     
109020            ,DADELDAT                                                     
109030            ,IDUSER                                                       
109040            ,FLCUSREP                                                     
109050            ,BETEXT_1                                                     
109060            ,BETEXT_2                                                     
109070            ,BETEXT_3                                                     
109080            ,BETEXT_4                                                     
109090            ,BETEXT_5                                                     
109091            ,BETEXT_6                                                     
109092            ,BETEXT_7                                                     
109093            ,BETEXT_8                                                     
109094            ,BETEXT_9                                                     
109095            ,BETEXT_10                                                    
109096            ,BETEXT_11                                                    
109097            ,BETEXT_12                                                    
109098            ,BETEXT_13                                                    
109099            ,BETEXT_14                                                    
109100            ,BETEXT_15                                                    
109101            ,BETEXT_16                                                    
109102            ,BETEXT_17                                                    
109103            ,BETEXT_18                                                    
109104            ,BETEXT_19                                                    
109104            ,BETEXT_20                                                    
109104            ,BETEXT_21                                                    
109104            ,BETEXT_22                                                    
109104            ,BETEXT_23                                                    
109104            ,BETEXT_24                                                    
109104            ,BETEXT_25                                                    
109104            ,BETEXT_26                                                    
109104            ,BETEXT_27                                                    
109104            ,BETEXT_28                                                    
109104            ,BETEXT_29                                                    
109104            ,BETEXT_30                                                    
109104            ,BETEXT_31                                                    
109104            ,BETEXT_32                                                    
109104            ,BETEXT_33                                                    
109104            ,BETEXT_34                                                    
109104            ,BETEXT_35                                                    
109104            ,BETEXT_36                                                    
109104            ,BETEXT_37                                                    
109104            ,BETEXT_38                                                    
109104            ,BETEXT_39                                                    
109105            ,BETEXT_40)                                                   
109110         VALUES                                                           
109200            (:REQU-IDLEGSEL-KEY                                           
109210            ,:REQU-IDLANDX3-SEND-KEY                                      
109300            ,:REQU-IDLANDX3-REC-KEY                                       
109310            ,:WS-COMING                                                   
109400            ,:REQU-FLEXPORT                                               
109410            ,:REQU-FLVAT                                                  
109420            ,:REQU-FLVAT-PRIV                                             
109430            ,:REQU-FLVATREP                                               
109500            ,:REQU-FLINTREP                                               
109600            ,:REQU-KDVAT                                                  
109610            ,:REQU-KDVAT-SERV                                             
109700            ,:MAP-DAREGDAT                                                
109710            ,:MAP-DAUPPDAT                                                
109720            ,:MAP-DADELDAT                                                
109730            ,:REQU-IDUSER                                                 
109740            ,:REQU-FLCUSREP                                               
109750            ,:REQU-BETEXT-1                                               
109760            ,:REQU-BETEXT-2                                               
109770            ,:REQU-BETEXT-3                                               
109780            ,:REQU-BETEXT-4                                               
109790            ,:REQU-BETEXT-5                                               
109800            ,:REQU-BETEXT-6                                               
109810            ,:REQU-BETEXT-7                                               
109820            ,:REQU-BETEXT-8                                               
109830            ,:REQU-BETEXT-9                                               
109840            ,:REQU-BETEXT-10                                              
109850            ,:REQU-BETEXT-11                                              
109860            ,:REQU-BETEXT-12                                              
109870            ,:REQU-BETEXT-13                                              
109880            ,:REQU-BETEXT-14                                              
109890            ,:REQU-BETEXT-15                                              
109891            ,:REQU-BETEXT-16                                              
109892            ,:REQU-BETEXT-17                                              
109893            ,:REQU-BETEXT-18                                              
109894            ,:REQU-BETEXT-19                                              
109894            ,:REQU-BETEXT-20                                              
109894            ,:REQU-BETEXT-21                                              
109894            ,:REQU-BETEXT-22                                              
109894            ,:REQU-BETEXT-23                                              
109894            ,:REQU-BETEXT-24                                              
109894            ,:REQU-BETEXT-25                                              
109894            ,:REQU-BETEXT-26                                              
109894            ,:REQU-BETEXT-27                                              
109894            ,:REQU-BETEXT-28                                              
109894            ,:REQU-BETEXT-29                                              
109894            ,:REQU-BETEXT-30                                              
109894            ,:REQU-BETEXT-31                                              
109894            ,:REQU-BETEXT-32                                              
109894            ,:REQU-BETEXT-33                                              
109894            ,:REQU-BETEXT-34                                              
109894            ,:REQU-BETEXT-35                                              
109894            ,:REQU-BETEXT-36                                              
109894            ,:REQU-BETEXT-37                                              
109894            ,:REQU-BETEXT-38                                              
109894            ,:REQU-BETEXT-39                                              
109895            ,:REQU-BETEXT-40)                                             
109900     END-EXEC                                                             
110000                                                                          
110100     MOVE SQLCODE TO SQLCODE-WS                                           
110200     PERFORM DB2-STATUS-CHECK                                             
110300     .                                                                    
110500                                                                          
110510 DB2-DELETE-T01INRE-TAB-CURR SECTION.                                     
110600     MOVE 000   TO GOOD-SQLCODECODES                                      
110700                                                                          
110800     EXEC SQL                                                             
110900         DELETE FROM T01INRE                                              
111000                                                                          
111100         WHERE  IDLEGSEL      = :REQU-IDLEGSEL-KEY                        
111200            AND IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                   
111201            AND IDLANDX3_REC  = :REQU-IDLANDX3-REC-KEY                    
111300            AND KDSTATUS      = :WS-CURRENT                               
111400            AND DADELDAT      > :WS-ACTIVE                                
111500     END-EXEC                                                             
111600                                                                          
111700     MOVE SQLCODE TO SQLCODE-WS                                           
111800     PERFORM DB2-STATUS-CHECK                                             
111900     .                                                                    
112100                                                                          
112110 DB2-DELETE-T01INRE-TAB-COM SECTION.                                      
112200     MOVE 000   TO GOOD-SQLCODECODES                                      
112300                                                                          
112400     EXEC SQL                                                             
112500         DELETE FROM T01INRE                                              
112600                                                                          
112700         WHERE  IDLEGSEL      = :REQU-IDLEGSEL-KEY                        
112800            AND IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                   
112801            AND IDLANDX3_REC  = :REQU-IDLANDX3-REC-KEY                    
112900            AND KDSTATUS      = :WS-COMING                                
113000     END-EXEC                                                             
113100                                                                          
113200     MOVE SQLCODE TO SQLCODE-WS                                           
113300     PERFORM DB2-STATUS-CHECK                                             
113400     .                                                                    
113600                                                                          
113610 DB2-STATUS-CHECK  SECTION.                                               
113700     SET SQLCODE-IX TO 1                                                  
113800     SEARCH GOOD-SQLCODE                                                  
113900       AT END                                                             
114000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
114100          DELIMITED BY SIZE INTO ERROR-TEXT                               
114200          CALL ABEND USING RKOD-ABEND-DB2                                 
114300       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
114400     END-SEARCH                                                           
114500     .                                                                    
