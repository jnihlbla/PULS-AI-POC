000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5029400.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   11/10/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.PULS.ACSCOUNTUPDATE                             
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        INVENTORY COUNT UPDATE                                           
001100*                                                                         
001200*        THE PROGRAM UPDATES   WDJ7                                       
001300*        THE PROGRAM READS     WDR2                                       
001400*        THE PROGRAM READS     WDB6                                       
001500*        THE PROGRAM READS     WDD3                                       
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: W50294T                                             
001900*        REQUEST:     W50294I1                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        RESPONSE:    W50294O1                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W5029400'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004000 77  ERROR-TEXT1                 PIC X(80) VALUE SPACE.                   
004100 77  ERROR-TEXT2                 PIC X(80) VALUE SPACE.                   
004200 77  ERROR-TEXT3                 PIC X(80) VALUE SPACE.                   
004300 77  ERROR-TEXT4                 PIC X(80) VALUE SPACE.                   
004400 77  ERROR-TEXT5                 PIC X(80) VALUE SPACE.                   
004500 77  KDRC-DISPLAY                PIC Z(5).                                
004600 77  WS-RESP-AREA-LENGTH         PIC 9(5) COMP.                           
004700 77  WS-KVRADER                  PIC 9(5) VALUE 0.                        
004800 77  WS-KVRADER-NUM              PIC 9(5) VALUE 0.                        
004900 77  WS-CNTR1                    PIC 9(5) VALUE 0.                        
005000 77  WS-REC-UPD-CNT              PIC 9(5) VALUE 0.                        
005100 77  WS-IDFRIDATA                PIC X(25) VALUE SPACE.                   
005200 77  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
005300 77  WS-CURRENT-TIME             PIC 9(6)    VALUE ZERO.                  
005400 77  WS-DEV-CNT-UPPER            PIC 9(8) VALUE 0.                        
005500 77  WS-DEV-CNT-LOWER            PIC 9(8) VALUE 0.                        
005600 77  WS-KVCOUNT                  PIC 9(9) VALUE 0.                        
005700 77  WS-AVG-COST                 PIC 9(8)V99 VALUE 0.                     
005800 77  WS-AVG-COST-UPPER           PIC 9(8)V99 VALUE 0.                     
005900 77  WS-AVG-COST-LOWER           PIC 9(8)V99 VALUE 0.                     
006000 77  TBL-INDX                    PIC 9(3)    VALUE 0.                     
006100 77  TBL-INDX-MAX                PIC 9(3)    VALUE 100.                   
006200 77  WS-ADDRESS                  PIC X(31)   VALUE                        
006300               'CARPARTS.PULS.ACSCOUNTUPDATE'.                            
006400 77  UPPER-ALPHA                 PIC X(29)                                
006500                            VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ'.        
006600 77  LOWER-ALPHA                 PIC X(29)                                
006700                            VALUE 'abcdefghijklmnopqrstuvwxyzåäö'.        
006800                                                                          
006900 77  WS-IDSKYLT-CHINESE          PIC X(3)    VALUE 'RCN'.                 
007000 77  WS-IDSKYLT-ENGLISH          PIC X(3)    VALUE 'GB '.                 
007100                                                                          
007200 77  YES                         PIC X       VALUE 'J'.                   
007300 77  NOO                         PIC X       VALUE 'N'.                   
007400                                                                          
007500                                                                          
007600 77  KEYS-SW                     PIC X       VALUE 'J'.                   
007700     88  KEYS-OK                             VALUE 'J'.                   
007800     88  KEYS-WRONG                          VALUE 'N'.                   
007900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008000     88  INDATA-OK                           VALUE 'J'.                   
008100     88  INDATA-NOT-OK                       VALUE 'N'.                   
008200     EJECT                                                                
008300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008400 01  GENERAL-SUBPROGRAMS.                                                 
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
009000     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
009100     SKIP3                                                                
009200*    --- PARAMETERS TO ABEND                                              
009300                                                                          
009400     SKIP3                                                                
009500     EJECT                                                                
009600 01  MESSAGE-CODES.                                                       
009700     03  ERROR-CODES.                                                     
009800         05  ERR-UPDATE-NOT-OK       PIC X(3)    VALUE '004'.             
009900         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
010000         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
010100         05  ERR-NO-DATA             PIC X(3)    VALUE '014'.             
010200         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
010300         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
010400         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
010500         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
010600         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
010700         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
010800         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
010900         05  ERR-MUST-NOT-BE-CHANGED PIC X(3)    VALUE '031'.             
011000         05  ERR-MUST-NOT-BE-ENTERED PIC X(3)    VALUE '033'.             
011100         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
011200         05  ERR-INRE-ALREADY-EXISTS PIC X(3)    VALUE '104'.             
011300         05  ERR-SHLD-NOT-BE-ZERO    PIC X(3)    VALUE '126'.             
011400         05  ERR-ZERO-NOT-ALLOWED    PIC X(3)    VALUE '168'.             
011500         05  ERR-INV-INFO-NOT-FOUND  PIC X(3)    VALUE '221'.             
011600         05  ERR-INV-NOT-COMPLETED   PIC X(3)    VALUE '222'.             
011700         05  ERR-ACS-NOT-ALLOWED     PIC X(3)    VALUE '331'.             
011800         05  ERR-INV-NOT-STARTED     PIC X(3)    VALUE '332'.             
011900     03  INFO-CODES.                                                      
012000         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
012100         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
012200         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
012300         05  INF-PROCESS-STARTED     PIC X(3)    VALUE '015'.             
012400         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
012500         05  INF-INV-INFO-MISSING    PIC X(3)    VALUE '220'.             
012600*                                                                         
012700*01  -COPY WDECAREA                                                       
012800                                                                          
012900 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
013000*01  -COPY WTRAUTF8                                                       
013100                                                                          
013200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
013300     SKIP3                                                                
013400*01  -COPY WZ01SUB                                                        
013500     EJECT                                                                
013600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
013700     SKIP3                                                                
013800 01  REQU-AREA.                                                           
013900*    03  -COPY WZ01REQU                                                   
014000*    03  -COPY W50294I1                                                   
014100     EJECT                                                                
014200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
014300     SKIP3                                                                
014400 01  RESP-AREA.                                                           
014500*    03  -COPY WZ01RESP                                                   
014600*    03  -COPY W50294O1                                                   
014700     EJECT                                                                
014800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
014900*                                                                         
015000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015100     SKIP3                                                                
015200 01  KEYS-FOR-DLI.                                                        
015300     03  W-IDDC-B6-X.                                                     
015400         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
015500     03  W-WDGXKEY-5104-X.                                                
015600         05 W-IDHTYP-5103        PIC X(4)    VALUE '5103'.                
015700         05 W-LOWVALUE           PIC X(26)   VALUE LOW-VALUE.             
015800     03  W-IDDC-5104-X.                                                   
015900         05  W-IDDC-5104         PIC X(2)    VALUE SPACE.                 
016000     03  W-WDJ7D1SEQ.                                                     
016100         05  W-IDDC-D1           PIC X(2).                                
016200         05  W-IDACSNR-P         PIC S9(7) COMP-3.                        
016300     03  W-WDJ7E1SEQ.                                                     
016400         05  W-IDDC-E1           PIC X(2).                                
016500         05  W-IDACSNR-R         PIC S9(7) COMP-3.                        
016600     03  W-WDJ7F1SEQ.                                                     
016700         05  W-IDDC-F1           PIC X(2).                                
016800         05  W-IDACSNR-T         PIC S9(7) COMP-3.                        
016900     03  W-WDJ701KY-X.                                                    
017000         05  W-WDJ7-IDDC         PIC X(2) VALUE SPACE.                    
017100         05  W-WDJ7-IDARTNR      PIC S9(9) COMP-3 VALUE 0.                
017200     03  W-IDARTNR-X.                                                     
017300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO   COMP-3.         
017400     03  W-IDSKYLT-X.                                                     
017500         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
017600     SKIP2                                                                
017700*    --- STATUS-KOD FRÅN IMS                                              
017800 01  STATUS-WS                   PIC XX.                                  
017900     88  SEGMENT-FOUND                       VALUE '  '.                  
018000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
018100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018200     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
018300     SKIP2                                                                
018400 01  GOOD-STATUSCODES.                                                    
018500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018600     SKIP3                                                                
018700 01  SSA1                        PIC X(64).                               
018800 01  SSA2                        PIC X(64).                               
018900     EJECT                                                                
019000*    --- IMS FUNCTION CODES                                               
019100*01  -COPY W0003                                                          
019200     EJECT                                                                
019300                                                                          
019400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ701'.                      
019500 01  DLI-IO-WDJ701.                                                       
019600*    03  -COPY WDJ701                                                     
019700                                                                          
019800 01  FILLER         PIC X(18) VALUE 'DLI-IO-WDJ701-DSEQ'.                 
019900 01  DLI-IO-WDJ701-DSEQ.                                                  
020000*    03  -COPY WDJ7D1                                                     
020100 01  FILLER         PIC X(18) VALUE 'DLI-IO-WDJ701-ESEQ'.                 
020200 01  DLI-IO-WDJ701-ESEQ.                                                  
020300*    03  -COPY WDJ7E1                                                     
020400 01  FILLER         PIC X(18) VALUE 'DLI-IO-WDJ701-FSEQ'.                 
020500 01  DLI-IO-WDJ701-FSEQ.                                                  
020600*    03  -COPY WDJ7F1                                                     
020700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5104'.                    
020800 01  DLI-IO-WDGX5104.                                                     
020900*    03  -COPY WDGX5104                                                   
021000                                                                          
021100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
021200 01  DLI-IO-WDB601.                                                       
021300*    03  -COPY WDB601                                                     
021400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
021500 01  DLI-IO-WDD311.                                                       
021600*    03  -COPY WDD311                                                     
021700     EJECT                                                                
021800 LINKAGE SECTION.                                                         
021900*01  -COPY W0009  -PRE MSG-                                               
022000     05  FILLER                  PIC X.                                   
022100                                                                          
022200*01  -COPY W0008  -PRE WDJ7-                                              
022300     05  FILLER                  PIC X.                                   
022400*01  -COPY W0008  -PRE WDJ7D-                                             
022500     05  FILLER                  PIC X.                                   
022600*01  -COPY W0008  -PRE WDJ7E-                                             
022700     05  FILLER                  PIC X.                                   
022800*01  -COPY W0008  -PRE WDJ7F-                                             
022900     05  FILLER                  PIC X.                                   
023000                                                                          
023100*01  -COPY W0008  -PRE 5104-                                              
023200     05  FILLER                  PIC X.                                   
023300                                                                          
023400*01  -COPY W0008  -PRE WDB6-                                              
023500     05  FILLER                  PIC X.                                   
023600                                                                          
023700*01  -COPY W0008  -PRE WDD3-                                              
023800     05  FILLER                  PIC X.                                   
023900     EJECT                                                                
024000 PROCEDURE DIVISION  USING MSG-PCB WDJ7D-PCB WDJ7E-PCB WDJ7F-PCB          
024100                           WDJ7-PCB 5104-PCB WDB6-PCB  WDD3-PCB.          
024200 MAIN SECTION.                                                            
024300     ENTRY 'DLITCBL' USING MSG-PCB WDJ7D-PCB WDJ7E-PCB WDJ7F-PCB          
024400                           WDJ7-PCB 5104-PCB WDB6-PCB  WDD3-PCB.          
024500                                                                          
024600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
024700     IF SUB-KDRC = 0                                                      
024800       PERFORM A-INIT                                                     
024900       PERFORM B-CHECK-KEYS                                               
025000       IF KEYS-OK                                                         
025100         PERFORM F-READ-SHOW-INFO                                         
025200       END-IF                                                             
025300       PERFORM S02-RETURN-RESPONSE                                        
025400     END-IF                                                               
025500                                                                          
025600     MOVE ZERO TO RETURN-CODE                                             
025700     GOBACK                                                               
025800     .                                                                    
025900     EJECT                                                                
026000 A-INIT SECTION.                                                          
026100                                                                          
026200     MOVE ALL '+'                     TO RESP-AREA                        
026300     MOVE SPACE                       TO RESP-IDMSG-ERROR                 
026400                                         RESP-IDMSG-INFO                  
026500                                         RESP-IDELMT-ERROR                
026600     MOVE 0                           TO RESP-KVRADER                     
026700     MOVE '001'                       TO RESP-IDMSGVER                    
026800     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
026900     MOVE FUNCTION CURRENT-DATE (9:6) TO WS-CURRENT-TIME                  
027000     .                                                                    
027100     EJECT                                                                
027200 B-CHECK-KEYS SECTION.                                                    
027300                                                                          
027400     MOVE YES TO KEYS-SW                                                  
027500                                                                          
027600     IF REQU-IDDC = ALL '+'                                               
027700        MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                      
027800        MOVE 'IDDC'              TO RESP-IDELMT-ERROR                     
027900        MOVE NOO                 TO KEYS-SW                               
028000     END-IF                                                               
028100                                                                          
028200     IF KEYS-OK                                                           
028300        MOVE REQU-IDDC          TO W-IDDC-B6                              
028400        PERFORM IMS-GU-WDB601                                             
028500        IF SEGMENT-FOUND                                                  
028600           IF DCS-FLINVACS = YES                                          
028700              CONTINUE                                                    
028800           ELSE                                                           
028900              MOVE ERR-ACS-NOT-ALLOWED                                    
029000                                TO RESP-IDMSG-ERROR                       
029100              MOVE NOO          TO KEYS-SW                                
029200           END-IF                                                         
029300        ELSE                                                              
029400           MOVE NOT-FOUND       TO RESP-IDMSG-ERROR                       
029500           MOVE 'IDDC'          TO RESP-IDELMT-ERROR                      
029600           MOVE NOO             TO KEYS-SW                                
029700        END-IF                                                            
029800     END-IF                                                               
029900                                                                          
030000     IF KEYS-OK                                                           
030100       IF REQU-KDPGMACT = 'S' OR 'E'                                      
030200          CONTINUE                                                        
030300       ELSE                                                               
030400          MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                     
030500          MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                    
030600          MOVE NOO                TO KEYS-SW                              
030700       END-IF                                                             
030800     END-IF                                                               
030900     .                                                                    
031000     EJECT                                                                
031100 F-READ-SHOW-INFO SECTION.                                                
031200                                                                          
031300     MOVE REQU-IDDC            TO W-IDDC-5104                             
031400     PERFORM IMS-GU-WDGX5104                                              
031500     IF SEGMENT-FOUND                                                     
031600        IF 5104-DASTADAT > 5104-DASTODAT                                  
031700           PERFORM FA-READ-BASICDATA                                      
031800        ELSE                                                              
031900           MOVE ERR-INV-NOT-STARTED TO RESP-IDMSG-ERROR                   
032000        END-IF                                                            
032100     ELSE                                                                 
032200        MOVE ERR-INV-INFO-NOT-FOUND TO RESP-IDMSG-ERROR                   
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 FA-READ-BASICDATA SECTION.                                               
032700                                                                          
032800     PERFORM FAA-CHECK-INPUT                                              
032900     IF INDATA-OK                                                         
033000        EVALUATE TRUE                                                     
033100           WHEN REQU-KDPGMACT = 'S'                                       
033200             PERFORM FAB-GET-DATA                                         
033300                                                                          
033400           WHEN REQU-KDPGMACT = 'E'                                       
033500             INITIALIZE RESP-AREA                                         
033600             PERFORM FAD-EXECUTE-PROCESS VARYING TBL-INDX                 
033700             FROM 1 BY 1 UNTIL TBL-INDX > REQU-KVRADER                    
033800             PERFORM FAE-REFRESH-DATA                                     
033900        END-EVALUATE                                                      
034000     END-IF                                                               
034100     .                                                                    
034200     EJECT                                                                
034300 FAA-CHECK-INPUT SECTION.                                                 
034400                                                                          
034500     MOVE YES                     TO INDATA-SW                            
034600     IF REQU-IDACSNR = ALL '+'                                            
034700        MOVE ERR-MUST-BE-ENTERED  TO RESP-IDMSG-ERROR                     
034800        MOVE 'IDACSNR'            TO RESP-IDELMT-ERROR                    
034900        MOVE NOO                  TO INDATA-SW                            
035000     END-IF                                                               
035100                                                                          
035200     IF INDATA-OK                                                         
035300        INSPECT REQU-IDACSNR TALLYING WS-CNTR1                            
035400           FOR ALL '-' '+'                                                
035500        IF WS-CNTR1 > 0                                                   
035600           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
035700           MOVE 'IDACSNR'         TO RESP-IDELMT-ERROR                    
035800           MOVE NOO               TO INDATA-SW                            
035900        END-IF                                                            
036000     END-IF                                                               
036100                                                                          
036200     IF INDATA-OK                                                         
036300         MOVE REQU-IDACSNR         TO WS-IDFRIDATA                        
036400         MOVE WS-IDFRIDATA         TO DEC-IDFRIDATA                       
036500         MOVE 6                    TO DEC-KVHELTAL                        
036600         MOVE 0                    TO DEC-KVDECIMAL                       
036700                                                                          
036800         CALL WDECEDIT USING DEC-WDECAREA                                 
036900         IF DEC-KDSVAR-OK                                                 
037000            CONTINUE                                                      
037100         ELSE                                                             
037200            MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                  
037300            MOVE 'IDACSNR'           TO RESP-IDELMT-ERROR                 
037400            MOVE NOO                 TO INDATA-SW                         
037500         END-IF                                                           
037600     END-IF                                                               
037700                                                                          
037800     IF INDATA-OK                                                         
037900       EVALUATE TRUE                                                      
038000        WHEN REQU-IDACSNR >= 100000 AND REQU-IDACSNR <= 199999            
038100            MOVE REQU-IDACSNR TO W-IDACSNR-P                              
038200            MOVE REQU-IDDC    TO W-IDDC-D1                                
038300            PERFORM IMS-GN-WDJ701-DSEQ                                    
038400        WHEN REQU-IDACSNR >= 200000 AND REQU-IDACSNR <= 299999            
038500            MOVE REQU-IDACSNR TO W-IDACSNR-R                              
038600            MOVE REQU-IDDC    TO W-IDDC-E1                                
038700            PERFORM IMS-GN-WDJ701-ESEQ                                    
038800        WHEN REQU-IDACSNR >= 300000 AND REQU-IDACSNR <= 399999            
038900            MOVE REQU-IDACSNR TO W-IDACSNR-T                              
039000            MOVE REQU-IDDC    TO W-IDDC-F1                                
039100            PERFORM IMS-GN-WDJ701-FSEQ                                    
039200        WHEN OTHER                                                        
039300            MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                    
039400            MOVE 'IDACSNR'         TO RESP-IDELMT-ERROR                   
039500            MOVE NOO               TO INDATA-SW                           
039600       END-EVALUATE                                                       
039700       IF SEGMENT-FOUND                                                   
039800          CONTINUE                                                        
039900       ELSE                                                               
040000            MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                    
040100            MOVE 'IDACSNR'         TO RESP-IDELMT-ERROR                   
040200            MOVE NOO               TO INDATA-SW                           
040300       END-IF                                                             
040400     END-IF                                                               
040500                                                                          
040600     IF INDATA-OK                                                         
040700         IF REQU-IDCOUNTER-REG = ALL '+' OR SPACES                        
040800            MOVE ERR-MUST-BE-ENTERED  TO RESP-IDMSG-ERROR                 
040900            MOVE 'IDCOUNTER-REG'      TO RESP-IDELMT-ERROR                
041000            MOVE NOO                  TO INDATA-SW                        
041100         ELSE                                                             
041200            INSPECT REQU-IDCOUNTER-REG                                    
041300            CONVERTING LOWER-ALPHA    TO UPPER-ALPHA                      
041400         END-IF                                                           
041500     END-IF                                                               
041600                                                                          
041700     IF INDATA-OK                                                         
041800       IF REQU-KDPGMACT = 'E'                                             
041900         IF REQU-KVRADER < ZERO                                           
042000            MOVE ERR-MUST-BE-ENTERED  TO RESP-IDMSG-ERROR                 
042100            MOVE 'KVRADER'            TO RESP-IDELMT-ERROR                
042200            MOVE NOO                  TO INDATA-SW                        
042300         END-IF                                                           
042400       END-IF                                                             
042500     END-IF                                                               
042600     .                                                                    
042700 FAB-GET-DATA SECTION.                                                    
042800* DATA HAS BEEN FETCHED IN A PREVIOUS GN CALL, SO JUST MOVE THAT          
042900* ROW TO THE SCREEN.                                                      
043000     MOVE SPACES TO STATUS-WS                                             
043100     EVALUATE TRUE                                                        
043200       WHEN REQU-IDACSNR >= 100000 AND REQU-IDACSNR <= 199999             
043300         IF ACS-IDCOUNTER-P-REG = SPACE                                   
043400            ADD  +1    TO TBL-INDX                                        
043500            ADD  +1    TO WS-KVRADER                                      
043600            PERFORM FABA-MOVE-TO-SCREEN                                   
043700            PERFORM FABB-FETCH-NEXT-WDJ7                                  
043800         ELSE                                                             
043900            PERFORM FABB-FETCH-NEXT-WDJ7                                  
044000         END-IF                                                           
044100       WHEN REQU-IDACSNR >= 200000 AND REQU-IDACSNR <= 299999             
044200         IF ACS-IDCOUNTER-R-REG = SPACE                                   
044300            ADD  +1    TO TBL-INDX                                        
044400            ADD  +1    TO WS-KVRADER                                      
044500            PERFORM FABA-MOVE-TO-SCREEN                                   
044600            PERFORM FABB-FETCH-NEXT-WDJ7                                  
044700         ELSE                                                             
044800            PERFORM FABB-FETCH-NEXT-WDJ7                                  
044900         END-IF                                                           
045000       WHEN REQU-IDACSNR >= 300000 AND REQU-IDACSNR <= 399999             
045100         IF ACS-IDCOUNTER-T-REG = SPACE                                   
045200            ADD  +1    TO TBL-INDX                                        
045300            ADD  +1    TO WS-KVRADER                                      
045400            PERFORM FABA-MOVE-TO-SCREEN                                   
045500            PERFORM FABB-FETCH-NEXT-WDJ7                                  
045600         ELSE                                                             
045700            PERFORM FABB-FETCH-NEXT-WDJ7                                  
045800         END-IF                                                           
045900     END-EVALUATE                                                         
046000                                                                          
046100     MOVE WS-KVRADER                TO RESP-KVRADER                       
046200     IF WS-KVRADER = 0 AND REQU-KDPGMACT NOT = 'E'                        
046300        MOVE ERR-INV-INFO-NOT-FOUND TO RESP-IDMSG-ERROR                   
046400        MOVE NOO                    TO INDATA-SW                          
046500     END-IF                                                               
046600     .                                                                    
046700 FABA-MOVE-TO-SCREEN SECTION.                                             
046800     MOVE ACS-ADLAGOMR      TO RESP-ADLAGOMR-LINE(TBL-INDX)               
046900     MOVE ACS-ADGANG        TO RESP-ADGANG-LINE(TBL-INDX)                 
047000     MOVE ACS-ADPLATS       TO RESP-ADPLATS-LINE(TBL-INDX)                
047100     MOVE ACS-IDARTNR       TO RESP-IDARTNR-LINE(TBL-INDX)                
047200     MOVE ACS-BEART         TO RESP-BEART-LINE(TBL-INDX)                  
047300                                                                          
047400*    WE NEED SOME ADJUSTMENT FOR CHINESE NAME                             
047500*    BEFORE DISPLAY OF BEART                                              
047600                                                                          
047700     MOVE ACS-IDARTNR           TO W-IDARTNR                              
047800     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
047900     IF DCS-UNICODE-IDSKYLT                                               
048000        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
048100     ELSE                                                                 
048200        MOVE '278 '             TO TRAUTF8-KDCP                           
048300     END-IF                                                               
048400     PERFORM IMS-GU-WDD311                                                
048500     IF SEGMENT-FOUND                                                     
048600        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
048700     ELSE                                                                 
048800        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
048900        MOVE '278 '             TO TRAUTF8-KDCP                           
049000     END-IF                                                               
049100     IF TRAUTF8-TECONV-FROM = SPACES                                      
049200      MOVE 'GB'  TO W-IDSKYLT                                             
049300      MOVE '278' TO TRAUTF8-KDCP                                          
049400      PERFORM IMS-GU-WDD311                                               
049500      MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
049600     END-IF                                                               
049700     MOVE 25                    TO TRAUTF8-KVMAXTL                        
049800     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
049900     MOVE TRAUTF8-TECONV-TO     TO RESP-BEART-LINE(TBL-INDX)              
050000                                                                          
050100     MOVE 5104-FLBLINDCO    TO RESP-FLBLINDCO                             
050200* IF THE BLIND COUNT INDICATOR IS NOT SET, SHOW THE STOCK BALANCE         
050300* ON THE SCREEN                                                           
050400     IF 5104-FLBLINDCO = 'N'                                              
050500        IF ACS-KVLS < 0                                                   
050600          MOVE ACS-KVLS       TO RESP-KVLS-LINE(TBL-INDX)                 
050700          MOVE '-'            TO RESP-KVLS-LINE(TBL-INDX)(8:1)            
050800        ELSE                                                              
050900          MOVE ACS-KVLS       TO RESP-KVLS-LINE(TBL-INDX)                 
051000        END-IF                                                            
051100        INSPECT RESP-KVLS-LINE(TBL-INDX) REPLACING LEADING                
051200        ZEROS BY SPACES                                                   
051300        IF RESP-KVLS-LINE(TBL-INDX) = ALL SPACES                          
051400           MOVE 0             TO RESP-KVLS-LINE(TBL-INDX)                 
051500        END-IF                                                            
051600     ELSE                                                                 
051700        MOVE SPACES           TO RESP-KVLS-LINE(TBL-INDX)                 
051800     END-IF                                                               
051900                                                                          
052000     IF REQU-IDACSNR >= 100000 AND REQU-IDACSNR <= 199999                 
052100        IF ACS-KVPCOUNT = 0 AND ACS-FLKLAR = 'J'                          
052200         MOVE 0              TO RESP-KVCOUNT-LINE(TBL-INDX)               
052300        ELSE                                                              
052400         MOVE SPACES         TO RESP-KVCOUNT-LINE(TBL-INDX)               
052500        END-IF                                                            
052600     END-IF                                                               
052700                                                                          
052800     IF REQU-IDACSNR >= 200000 AND REQU-IDACSNR <= 299999                 
052900        IF ACS-KVRCOUNT = 0 AND ACS-FLKLAR = 'J'                          
053000         MOVE 0              TO RESP-KVCOUNT-LINE(TBL-INDX)               
053100        ELSE                                                              
053200         MOVE SPACES         TO RESP-KVCOUNT-LINE(TBL-INDX)               
053300        END-IF                                                            
053400        IF ACS-KVRCOUNT NOT = 0                                           
053500           MOVE ACS-KVRCOUNT TO RESP-KVCOUNT-LINE(TBL-INDX)               
053600           INSPECT RESP-KVCOUNT-LINE(TBL-INDX) REPLACING                  
053700           LEADING ZEROS BY SPACE                                         
053800           IF RESP-KVCOUNT-LINE(TBL-INDX) = ALL SPACES                    
053900              MOVE 0         TO RESP-KVCOUNT-LINE(TBL-INDX)               
054000           END-IF                                                         
054100        END-IF                                                            
054200     END-IF                                                               
054300                                                                          
054400     IF REQU-IDACSNR >= 300000 AND REQU-IDACSNR <= 399999                 
054500        IF ACS-KVTCOUNT = 0 AND ACS-FLKLAR = 'J'                          
054600         MOVE 0              TO RESP-KVCOUNT-LINE(TBL-INDX)               
054700        ELSE                                                              
054800         MOVE SPACES         TO RESP-KVCOUNT-LINE(TBL-INDX)               
054900        END-IF                                                            
055000        IF ACS-KVTCOUNT NOT = 0                                           
055100           MOVE ACS-KVTCOUNT TO RESP-KVCOUNT-LINE(TBL-INDX)               
055200           INSPECT RESP-KVCOUNT-LINE(TBL-INDX) REPLACING                  
055300           LEADING ZEROS BY SPACE                                         
055400           IF RESP-KVCOUNT-LINE(TBL-INDX) = ALL SPACES                    
055500              MOVE 0         TO RESP-KVCOUNT-LINE(TBL-INDX)               
055600           END-IF                                                         
055700        END-IF                                                            
055800     END-IF                                                               
055900     .                                                                    
056000                                                                          
056100 FABB-FETCH-NEXT-WDJ7 SECTION.                                            
056200* FETCH THE NEXT ROWS FROM THE DATABASE                                   
056300     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
056400                   OR TBL-INDX = TBL-INDX-MAX                             
056500       EVALUATE TRUE                                                      
056600        WHEN REQU-IDACSNR >= 100000 AND REQU-IDACSNR <= 199999            
056700          PERFORM IMS-GN-WDJ701-DSEQ                                      
056800          IF SEGMENT-FOUND                                                
056900            IF ACS-IDCOUNTER-P-REG = SPACE                                
057000               ADD  +1    TO WS-KVRADER                                   
057100               ADD  +1    TO TBL-INDX                                     
057200               PERFORM FABA-MOVE-TO-SCREEN                                
057300            END-IF                                                        
057400          END-IF                                                          
057500        WHEN REQU-IDACSNR >= 200000 AND REQU-IDACSNR <= 299999            
057600          PERFORM IMS-GN-WDJ701-ESEQ                                      
057700          IF SEGMENT-FOUND                                                
057800            IF ACS-IDCOUNTER-R-REG = SPACE                                
057900               ADD  +1    TO WS-KVRADER                                   
058000               ADD  +1    TO TBL-INDX                                     
058100               PERFORM FABA-MOVE-TO-SCREEN                                
058200            END-IF                                                        
058300          END-IF                                                          
058400        WHEN REQU-IDACSNR >= 300000 AND REQU-IDACSNR <= 399999            
058500          PERFORM IMS-GN-WDJ701-FSEQ                                      
058600          IF SEGMENT-FOUND                                                
058700            IF ACS-IDCOUNTER-T-REG = SPACE                                
058800               ADD  +1    TO WS-KVRADER                                   
058900               ADD  +1    TO TBL-INDX                                     
059000               PERFORM FABA-MOVE-TO-SCREEN                                
059100            END-IF                                                        
059200          END-IF                                                          
059300       END-EVALUATE                                                       
059400     END-PERFORM                                                          
059500     .                                                                    
059600                                                                          
059700 FAD-EXECUTE-PROCESS SECTION.                                             
059800     MOVE YES                   TO INDATA-SW                              
059900     IF REQU-IDARTNR(TBL-INDX) = ALL '+'                                  
060000     AND REQU-KVRADER = ALL '+'                                           
060100       MOVE ALL '+'             TO RESP-AREA                              
060200       MOVE ERR-NO-DATA         TO RESP-IDMSG-ERROR                       
060300       MOVE NOO                 TO INDATA-SW                              
060400       MOVE ALL '+'             TO REQU-AREA                              
060500     ELSE                                                                 
060600       MOVE REQU-IDDC              TO W-WDJ7-IDDC                         
060700       IF REQU-IDARTNR(TBL-INDX) = ALL '+'                                
060800          MOVE ZERO TO W-WDJ7-IDARTNR                                     
060900       ELSE                                                               
061000          MOVE REQU-IDARTNR(TBL-INDX) TO W-WDJ7-IDARTNR                   
061100       END-IF                                                             
061200       PERFORM IMS-GHU-WDJ701                                             
061300       IF SEGMENT-FOUND                                                   
061400         EVALUATE TRUE                                                    
061500         WHEN REQU-IDACSNR >= 100000 AND REQU-IDACSNR <= 199999           
061600           IF ACS-IDCOUNTER-P-REG = SPACES AND ACS-FLKLAR = 'N'           
061700             IF REQU-KDACS = 'T' AND 5104-FLBLINDCO = 'N'                 
061800              IF ACS-KVLS < 0                                             
061900                MOVE ERR-INVALID-FIELD      TO RESP-IDMSG-ERROR           
062000                MOVE 'KVCOUNT'              TO RESP-IDELMT-ERROR          
062100                MOVE NOO                    TO INDATA-SW                  
062200              ELSE                                                        
062300                MOVE ACS-KVLS           TO ACS-KVPCOUNT                   
062400                MOVE YES                TO ACS-FLKLAR                     
062500                MOVE REQU-IDCOUNTER-REG TO ACS-IDCOUNTER-P-REG            
062600                MOVE WS-CURRENT-DATE    TO ACS-TIREGDAT-PCOUNT-REG        
062700                MOVE WS-CURRENT-TIME    TO ACS-TIREGTID-PCOUNT-REG        
062800                PERFORM IMS-REPL-WDJ701                                   
062900                MOVE INF-UPDATE-OK      TO RESP-IDMSG-INFO                
063000              END-IF                                                      
063100             ELSE                                                         
063200              IF REQU-KVCOUNT(TBL-INDX) NOT = ALL '+'                     
063300                 PERFORM FADA-CHECK-KVCOUNT                               
063400                 IF INDATA-OK                                             
063500                   MOVE REQU-KVCOUNT(TBL-INDX) TO ACS-KVPCOUNT            
063600                   PERFORM FADB-CHECK-ROUND1-PARM                         
063700                 END-IF                                                   
063800              ELSE                                                        
063900                IF ACS-KVLS < 0                                           
064000                   MOVE ERR-INVALID-FIELD   TO RESP-IDMSG-ERROR           
064100                   MOVE 'KVCOUNT'           TO RESP-IDELMT-ERROR          
064200                   MOVE NOO                 TO INDATA-SW                  
064300                END-IF                                                    
064400              END-IF                                                      
064500             END-IF                                                       
064600           END-IF                                                         
064700                                                                          
064800         WHEN REQU-IDACSNR >= 200000 AND REQU-IDACSNR <= 299999           
064900           IF ACS-IDCOUNTER-P-REG NOT = SPACES                            
065000           AND ACS-IDCOUNTER-R-REG = SPACES AND ACS-FLKLAR = 'N'          
065100             IF REQU-KDACS = 'T' AND 5104-FLBLINDCO = 'N'                 
065200              IF ACS-KVLS < 0                                             
065300                MOVE ERR-INVALID-FIELD      TO RESP-IDMSG-ERROR           
065400                MOVE 'KVCOUNT'              TO RESP-IDELMT-ERROR          
065500                MOVE NOO                    TO INDATA-SW                  
065600              ELSE                                                        
065700                MOVE ACS-KVLS           TO ACS-KVRCOUNT                   
065800                MOVE YES                TO ACS-FLKLAR                     
065900                MOVE REQU-IDCOUNTER-REG TO ACS-IDCOUNTER-R-REG            
066000                MOVE WS-CURRENT-DATE    TO ACS-TIREGDAT-RCOUNT-REG        
066100                MOVE WS-CURRENT-TIME    TO ACS-TIREGTID-RCOUNT-REG        
066200                PERFORM IMS-REPL-WDJ701                                   
066300                MOVE INF-UPDATE-OK      TO RESP-IDMSG-INFO                
066400              END-IF                                                      
066500             ELSE                                                         
066600              IF REQU-KVCOUNT(TBL-INDX) NOT = ALL '+'                     
066700                 PERFORM FADA-CHECK-KVCOUNT                               
066800                 IF INDATA-OK                                             
066900                   MOVE REQU-KVCOUNT(TBL-INDX) TO ACS-KVRCOUNT            
067000                   PERFORM FADC-CHECK-ROUND2-PARM                         
067100                 END-IF                                                   
067200              ELSE                                                        
067300                IF ACS-KVLS < 0 AND REQU-KVRADER = 1                      
067400                   MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR           
067500                   MOVE 'KVCOUNT'          TO RESP-IDELMT-ERROR           
067600                   MOVE NOO                 TO INDATA-SW                  
067700                END-IF                                                    
067800              END-IF                                                      
067900             END-IF                                                       
068000           END-IF                                                         
068100                                                                          
068200         WHEN REQU-IDACSNR >= 300000 AND REQU-IDACSNR <= 399999           
068300           IF ACS-IDCOUNTER-P-REG NOT = SPACES                            
068400           AND ACS-IDCOUNTER-R-REG NOT = SPACES                           
068500           AND ACS-IDCOUNTER-T-REG = SPACES                               
068600           AND ACS-FLKLAR = 'N'                                           
068700             IF REQU-KDACS = 'T' AND 5104-FLBLINDCO = 'N'                 
068800              IF ACS-KVLS < 0                                             
068900                MOVE ERR-INVALID-FIELD      TO RESP-IDMSG-ERROR           
069000                MOVE 'KVCOUNT'              TO RESP-IDELMT-ERROR          
069100                MOVE NOO                    TO INDATA-SW                  
069200              ELSE                                                        
069300                MOVE ACS-KVLS           TO ACS-KVTCOUNT                   
069400                MOVE YES                TO ACS-FLKLAR                     
069500                MOVE REQU-IDCOUNTER-REG TO ACS-IDCOUNTER-T-REG            
069600                MOVE WS-CURRENT-DATE    TO ACS-TIREGDAT-TCOUNT-REG        
069700                MOVE WS-CURRENT-TIME    TO ACS-TIREGTID-TCOUNT-REG        
069800                PERFORM IMS-REPL-WDJ701                                   
069900                MOVE INF-UPDATE-OK      TO RESP-IDMSG-INFO                
070000              END-IF                                                      
070100             ELSE                                                         
070200              IF REQU-KVCOUNT(TBL-INDX) NOT = ALL '+'                     
070300                 PERFORM FADA-CHECK-KVCOUNT                               
070400                 IF INDATA-OK                                             
070500                   MOVE REQU-KVCOUNT(TBL-INDX) TO ACS-KVTCOUNT            
070600                   MOVE YES                TO ACS-FLKLAR                  
070700                   MOVE REQU-IDCOUNTER-REG TO ACS-IDCOUNTER-T-REG         
070800                   MOVE WS-CURRENT-DATE    TO                             
070900                                           ACS-TIREGDAT-TCOUNT-REG        
071000                   MOVE WS-CURRENT-TIME    TO                             
071100                                           ACS-TIREGTID-TCOUNT-REG        
071200                   PERFORM IMS-REPL-WDJ701                                
071300                   MOVE INF-UPDATE-OK      TO RESP-IDMSG-INFO             
071400                 END-IF                                                   
071500              ELSE                                                        
071600                IF ACS-KVLS < 0 AND REQU-KVRADER = 1                      
071700                   MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR           
071800                   MOVE 'KVCOUNT'           TO RESP-IDELMT-ERROR          
071900                   MOVE NOO                 TO INDATA-SW                  
072000                END-IF                                                    
072100              END-IF                                                      
072200             END-IF                                                       
072300           END-IF                                                         
072400         END-EVALUATE                                                     
072500       ELSE                                                               
072600          MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                      
072700          MOVE 'IDACSNR'         TO RESP-IDELMT-ERROR                     
072800          MOVE NOO               TO INDATA-SW                             
072900       END-IF                                                             
073000     END-IF                                                               
073100     .                                                                    
073200                                                                          
073300 FADA-CHECK-KVCOUNT SECTION.                                              
073400     MOVE 0 TO WS-CNTR1                                                   
073500     INSPECT REQU-KVCOUNT(TBL-INDX) TALLYING WS-CNTR1                     
073600     FOR ALL '-' '+'                                                      
073700     IF WS-CNTR1 > 0                                                      
073800        MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                        
073900        MOVE 'KVCOUNT'         TO RESP-IDELMT-ERROR                       
074000        MOVE NOO               TO INDATA-SW                               
074100     END-IF                                                               
074200     IF INDATA-OK                                                         
074300       INSPECT REQU-KVCOUNT(TBL-INDX) REPLACING ALL SPACES                
074400       BY ZEROS                                                           
074500       IF REQU-KVCOUNT(TBL-INDX) NOT NUMERIC                              
074600          MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                    
074700          MOVE 'KVCOUNT'         TO RESP-IDELMT-ERROR                     
074800          MOVE NOO               TO INDATA-SW                             
074900       END-IF                                                             
075000     END-IF                                                               
075100     .                                                                    
075200                                                                          
075300 FADB-CHECK-ROUND1-PARM SECTION.                                          
075400     MOVE 0  TO WS-DEV-CNT-UPPER                                          
075500                WS-DEV-CNT-LOWER                                          
075600                WS-AVG-COST-UPPER                                         
075700                WS-AVG-COST-LOWER                                         
075800                WS-AVG-COST                                               
075900                WS-KVCOUNT                                                
076000                                                                          
076100                                                                          
076200     COMPUTE WS-DEV-CNT-UPPER = ACS-KVLS +                                
076300                                (ACS-KVLS * 5104-REQTYDEV-1)              
076400     COMPUTE WS-DEV-CNT-LOWER = ACS-KVLS -                                
076500                                (ACS-KVLS * 5104-REQTYDEV-1)              
076600     MOVE REQU-KVCOUNT(TBL-INDX) TO WS-KVCOUNT                            
076700     IF WS-KVCOUNT    >= WS-DEV-CNT-LOWER                                 
076800     AND WS-KVCOUNT   <= WS-DEV-CNT-UPPER                                 
076900       COMPUTE WS-AVG-COST-UPPER =                                        
077000               (ACS-PRAVCOST * ACS-KVLS) + 5104-PRAVCOST-DEV1             
077100       COMPUTE WS-AVG-COST-LOWER =                                        
077200               (ACS-PRAVCOST * ACS-KVLS) - 5104-PRAVCOST-DEV1             
077300       COMPUTE WS-AVG-COST = ACS-PRAVCOST * WS-KVCOUNT                    
077400       IF WS-AVG-COST  >= WS-AVG-COST-LOWER                               
077500       AND WS-AVG-COST <= WS-AVG-COST-UPPER                               
077600          MOVE YES  TO ACS-FLKLAR                                         
077700       ELSE                                                               
077800          MOVE NOO  TO ACS-FLKLAR                                         
077900       END-IF                                                             
078000     ELSE                                                                 
078100        MOVE NOO  TO ACS-FLKLAR                                           
078200     END-IF                                                               
078300                                                                          
078400     MOVE REQU-IDCOUNTER-REG TO ACS-IDCOUNTER-P-REG                       
078500     MOVE WS-CURRENT-DATE    TO ACS-TIREGDAT-PCOUNT-REG                   
078600     MOVE WS-CURRENT-TIME    TO ACS-TIREGTID-PCOUNT-REG                   
078700     PERFORM IMS-REPL-WDJ701                                              
078800     MOVE INF-UPDATE-OK      TO RESP-IDMSG-INFO                           
078900     .                                                                    
079000 FADC-CHECK-ROUND2-PARM SECTION.                                          
079100     MOVE 0  TO WS-DEV-CNT-UPPER                                          
079200                WS-DEV-CNT-LOWER                                          
079300                WS-AVG-COST-UPPER                                         
079400                WS-AVG-COST-LOWER                                         
079500                WS-AVG-COST                                               
079600                WS-KVCOUNT                                                
079700     COMPUTE WS-DEV-CNT-UPPER = ACS-KVLS +                                
079800                                (ACS-KVLS * 5104-REQTYDEV-2)              
079900     COMPUTE WS-DEV-CNT-LOWER = ACS-KVLS -                                
080000                                (ACS-KVLS * 5104-REQTYDEV-2)              
080100     MOVE REQU-KVCOUNT(TBL-INDX) TO WS-KVCOUNT                            
080200     IF WS-KVCOUNT    >= WS-DEV-CNT-LOWER                                 
080300     AND WS-KVCOUNT   <= WS-DEV-CNT-UPPER                                 
080400       COMPUTE WS-AVG-COST-UPPER =                                        
080500               (ACS-PRAVCOST * ACS-KVLS) + 5104-PRAVCOST-DEV2             
080600       COMPUTE WS-AVG-COST-LOWER =                                        
080700               (ACS-PRAVCOST * ACS-KVLS) - 5104-PRAVCOST-DEV2             
080800       COMPUTE WS-AVG-COST = ACS-PRAVCOST * WS-KVCOUNT                    
080900       IF WS-AVG-COST  >= WS-AVG-COST-LOWER                               
081000       AND WS-AVG-COST <= WS-AVG-COST-UPPER                               
081100          MOVE YES  TO ACS-FLKLAR                                         
081200       ELSE                                                               
081300          MOVE NOO  TO ACS-FLKLAR                                         
081400       END-IF                                                             
081500     ELSE                                                                 
081600        MOVE NOO  TO ACS-FLKLAR                                           
081700     END-IF                                                               
081800     MOVE REQU-IDCOUNTER-REG TO ACS-IDCOUNTER-R-REG                       
081900     MOVE WS-CURRENT-DATE    TO ACS-TIREGDAT-RCOUNT-REG                   
082000     MOVE WS-CURRENT-TIME    TO ACS-TIREGTID-RCOUNT-REG                   
082100     PERFORM IMS-REPL-WDJ701                                              
082200     MOVE INF-UPDATE-OK      TO RESP-IDMSG-INFO                           
082300     .                                                                    
082400 FAE-REFRESH-DATA SECTION.                                                
082500     MOVE 0      TO WS-KVRADER                                            
082600                    TBL-INDX                                              
082700     MOVE SPACES TO STATUS-WS                                             
082800     EVALUATE TRUE                                                        
082900      WHEN REQU-IDACSNR >= 100000 AND REQU-IDACSNR <= 199999              
083000          MOVE REQU-IDACSNR TO W-IDACSNR-P                                
083100          MOVE REQU-IDDC    TO W-IDDC-D1                                  
083200          PERFORM IMS-GU-WDJ701-DSEQ                                      
083300      WHEN REQU-IDACSNR >= 200000 AND REQU-IDACSNR <= 299999              
083400          MOVE REQU-IDACSNR TO W-IDACSNR-R                                
083500          MOVE REQU-IDDC    TO W-IDDC-E1                                  
083600          PERFORM IMS-GU-WDJ701-ESEQ                                      
083700      WHEN REQU-IDACSNR >= 300000 AND REQU-IDACSNR <= 399999              
083800          MOVE REQU-IDACSNR TO W-IDACSNR-T                                
083900          MOVE REQU-IDDC    TO W-IDDC-F1                                  
084000          PERFORM IMS-GU-WDJ701-FSEQ                                      
084100     END-EVALUATE                                                         
084200     IF SEGMENT-FOUND                                                     
084300       PERFORM FAB-GET-DATA                                               
084400     ELSE                                                                 
084500       MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                         
084600       MOVE 'IDACSNR'         TO RESP-IDELMT-ERROR                        
084700       MOVE NOO               TO INDATA-SW                                
084800     END-IF                                                               
084900     .                                                                    
085000                                                                          
085100*    --- DISPATCHER SECTIONS                                              
085200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
085300                                                                          
085400     MOVE 'GETARG'               TO SUB-KDFUNC                            
085500     MOVE WS-ADDRESS             TO SUB-ADDISPABS                         
085600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
085700                                                                          
085800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
085900                                                                          
086000     IF SUB-KDRC > 0                                                      
086100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
086200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
086300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
086400       DISPLAY ERROR-TEXT                                                 
086500       CALL FELLOG                                                        
086600     END-IF                                                               
086700     .                                                                    
086800     SKIP3                                                                
086900 S02-RETURN-RESPONSE SECTION.                                             
087000                                                                          
087100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
087200                                                                          
087300     MOVE RESP-KVRADER               TO WS-KVRADER-NUM                    
087400     COMPUTE WS-RESP-AREA-LENGTH =                                        
087500       LENGTH OF RESP-WZ01RESP + LENGTH OF RESP-W50294O1 -                
087600       ((100 - WS-KVRADER-NUM) * LENGTH OF RESP-TABELLRAD)                
087700     MOVE WS-RESP-AREA-LENGTH        TO SUB-KVDLEN                        
087800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
087900                                                                          
088000     IF SUB-KDRC > 0                                                      
088100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
088200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
088300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
088400       DISPLAY ERROR-TEXT                                                 
088500       CALL FELLOG                                                        
088600     END-IF                                                               
088700     .                                                                    
088800     EJECT                                                                
088900     SKIP3                                                                
089000     SKIP3                                                                
089100 IMS-GU-WDJ701-DSEQ SECTION.                                              
089200                                                                          
089300     STRING 'WDJ701  (WDJ7DSEQ =' W-WDJ7D1SEQ  ')'                        
089400          DELIMITED BY SIZE INTO SSA1                                     
089500     MOVE '  GE' TO GOOD-STATUSCODES                                      
089600     CALL CBLTDLI USING GU WDJ7D-PCB DLI-IO-WDJ701 SSA1                   
089700     MOVE WDJ7D-STATUS-CODE TO STATUS-WS                                  
089800     PERFORM IMS-STATUSCHECK                                              
089900     .                                                                    
090000 IMS-GU-WDJ701-ESEQ SECTION.                                              
090100                                                                          
090200     STRING 'WDJ701  (WDJ7ESEQ =' W-WDJ7E1SEQ  ')'                        
090300          DELIMITED BY SIZE INTO SSA1                                     
090400     MOVE '  GE' TO GOOD-STATUSCODES                                      
090500     CALL CBLTDLI USING GU WDJ7E-PCB DLI-IO-WDJ701 SSA1                   
090600     MOVE WDJ7E-STATUS-CODE TO STATUS-WS                                  
090700     PERFORM IMS-STATUSCHECK                                              
090800     .                                                                    
090900 IMS-GU-WDJ701-FSEQ SECTION.                                              
091000                                                                          
091100     STRING 'WDJ701  (WDJ7FSEQ =' W-WDJ7F1SEQ  ')'                        
091200          DELIMITED BY SIZE INTO SSA1                                     
091300     MOVE '  GE' TO GOOD-STATUSCODES                                      
091400     CALL CBLTDLI USING GU WDJ7F-PCB DLI-IO-WDJ701 SSA1                   
091500     MOVE WDJ7F-STATUS-CODE TO STATUS-WS                                  
091600     PERFORM IMS-STATUSCHECK                                              
091700     .                                                                    
091800 IMS-GN-WDJ701-DSEQ SECTION.                                              
091900                                                                          
092000     STRING 'WDJ701  (WDJ7DSEQ =' W-WDJ7D1SEQ  ')'                        
092100          DELIMITED BY SIZE INTO SSA1                                     
092200     MOVE '  GE' TO GOOD-STATUSCODES                                      
092300     CALL CBLTDLI USING GN WDJ7D-PCB DLI-IO-WDJ701 SSA1                   
092400     MOVE WDJ7D-STATUS-CODE TO STATUS-WS                                  
092500     PERFORM IMS-STATUSCHECK                                              
092600     .                                                                    
092700 IMS-GN-WDJ701-ESEQ SECTION.                                              
092800                                                                          
092900     STRING 'WDJ701  (WDJ7ESEQ =' W-WDJ7E1SEQ  ')'                        
093000          DELIMITED BY SIZE INTO SSA1                                     
093100     MOVE '  GE' TO GOOD-STATUSCODES                                      
093200     CALL CBLTDLI USING GN WDJ7E-PCB DLI-IO-WDJ701 SSA1                   
093300     MOVE WDJ7E-STATUS-CODE TO STATUS-WS                                  
093400     PERFORM IMS-STATUSCHECK                                              
093500     .                                                                    
093600     SKIP3                                                                
093700 IMS-GN-WDJ701-FSEQ SECTION.                                              
093800                                                                          
093900     STRING 'WDJ701  (WDJ7FSEQ =' W-WDJ7F1SEQ  ')'                        
094000          DELIMITED BY SIZE INTO SSA1                                     
094100     MOVE '  GE' TO GOOD-STATUSCODES                                      
094200     CALL CBLTDLI USING GN WDJ7F-PCB DLI-IO-WDJ701 SSA1                   
094300     MOVE WDJ7F-STATUS-CODE TO STATUS-WS                                  
094400     PERFORM IMS-STATUSCHECK                                              
094500     .                                                                    
094600 IMS-GHU-WDJ701 SECTION.                                                  
094700                                                                          
094800     STRING 'WDJ701  (WDJ701KY =' W-WDJ701KY-X  ')'                       
094900          DELIMITED BY SIZE INTO SSA1                                     
095000     MOVE '  GE' TO GOOD-STATUSCODES                                      
095100     CALL CBLTDLI USING GHU WDJ7-PCB DLI-IO-WDJ701 SSA1                   
095200     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
095300     PERFORM IMS-STATUSCHECK                                              
095400     .                                                                    
095500     SKIP3                                                                
095600 IMS-REPL-WDJ701 SECTION.                                                 
095700                                                                          
095800     MOVE '  ' TO GOOD-STATUSCODES                                        
095900     CALL CBLTDLI USING REPL WDJ7-PCB DLI-IO-WDJ701                       
096000     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
096100     PERFORM IMS-STATUSCHECK                                              
096200     .                                                                    
096300     EJECT                                                                
096400 IMS-GU-WDGX5104 SECTION.                                                 
096500                                                                          
096600     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-5104-X ')'                    
096700          DELIMITED BY SIZE INTO SSA1                                     
096800     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
096900          DELIMITED BY SIZE INTO SSA2                                     
097000     MOVE '  GE' TO GOOD-STATUSCODES                                      
097100     CALL CBLTDLI USING GU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2             
097200     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
097300     PERFORM IMS-STATUSCHECK                                              
097400     .                                                                    
097500     EJECT                                                                
097600 IMS-GU-WDB601 SECTION.                                                   
097700                                                                          
097800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
097900          DELIMITED BY SIZE INTO SSA1                                     
098000     MOVE '  GE' TO GOOD-STATUSCODES                                      
098100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
098200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
098300     PERFORM IMS-STATUSCHECK                                              
098400     .                                                                    
098500     EJECT                                                                
098600 IMS-GU-WDD311 SECTION.                                                   
098700                                                                          
098800     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
098900             DELIMITED BY SIZE INTO SSA1                                  
099000     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
099100             DELIMITED BY SIZE INTO SSA2                                  
099200     MOVE '  GE'                 TO GOOD-STATUSCODES                      
099300     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
099400     MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
099500     PERFORM IMS-STATUSCHECK                                              
099600     .                                                                    
099700     EJECT                                                                
099800 IMS-STATUSCHECK SECTION.                                                 
099900     SKIP2                                                                
100000     SET STATUS-IX TO 1                                                   
100100     SEARCH GOOD-STATUS                                                   
100200       AT END                                                             
100300         STRING 'INVALID STATUS CODE FROM IMS: ' STATUS-WS                
100400           DELIMITED BY SIZE INTO ERROR-TEXT                              
100500         DISPLAY ERROR-TEXT                                               
100600         CALL FELLOG                                                      
100700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
100800         CONTINUE                                                         
100900     END-SEARCH                                                           
101000     .                                                                    
