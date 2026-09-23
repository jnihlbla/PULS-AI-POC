000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5029200.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   11/10/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.PULS.ACSCOUNTSELECTION                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        INVENTORY COUNT SELECTION                                        
001100*                                                                         
001200*        THE PROGRAM UPDATES   WDJ7                                       
001300*        THE PROGRAM READS     WDR2                                       
001400*        THE PROGRAM READS     WDD3                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W50292T                                             
001800*        REQUEST:     W50292I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    W50292O1                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W5029200'.            
003600 77  WS-TEMP-NUM                 PIC 9(3)    VALUE 0.                     
003700 77  WS-TEMP-ALPHA               PIC X(3)    VALUE SPACE.                 
003800 77  WS-CNTR                     PIC 9(3)    VALUE 0.                     
003900 77  WS-CNTR1                    PIC 9(3)    VALUE 0.                     
004000 77  SAVE-TBL-INDX               PIC 9(3)    VALUE 0.                     
004100 77  TBL-INDX                    PIC 9(3)    VALUE 0.                     
004200 77  TBL-INDX-MAX                PIC 9(3)    VALUE 100.                   
004300 77  WS-IDFRIDATA                PIC X(25).                               
004400 77  WS-KVRADER-TOT              PIC 9(5) VALUE 0.                        
004500 77  WS-KVRADER                  PIC 9(5) VALUE 0.                        
004600 77  WS-ADDRESS                  PIC X(31)   VALUE                        
004700               'CARPARTS.PULS.ACSCOUNTSELECTION'.                         
004800 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004900                                                                          
005000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005100 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005200 77  KDRC-DISPLAY                PIC Z(5).                                
005300 77  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
005400 77  WS-CURRENT-TIME             PIC 9(6)    VALUE ZERO.                  
005500                                                                          
005600 77  WS-IDSKYLT-CHINESE          PIC X(3)    VALUE 'RCN'.                 
005700 77  WS-IDSKYLT-ENGLISH          PIC X(3)    VALUE 'GB '.                 
005800                                                                          
005900 77  YES                         PIC X       VALUE 'J'.                   
006000 77  NOO                         PIC X       VALUE 'N'.                   
006100                                                                          
006200                                                                          
006300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006400     88  KEYS-OK                             VALUE 'J'.                   
006500     88  KEYS-WRONG                          VALUE 'N'.                   
006600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006700     88  INDATA-OK                           VALUE 'J'.                   
006800     88  INDATA-NOT-OK                       VALUE 'N'.                   
006900 77  WS-EXIT-SW                  PIC X       VALUE 'N'.                   
007000     88  WS-EXIT                             VALUE 'J'.                   
007100 77  UPPER-ALPHA                 PIC X(29)                                
007200                            VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ'.        
007300 77  LOWER-ALPHA                 PIC X(29)                                
007400                            VALUE 'abcdefghijklmnopqrstuvwxyzåäö'.        
007500     EJECT                                                                
007600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007700 01  GENERAL-SUBPROGRAMS.                                                 
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008400*COMPOPT DB2BIND=YES            -- REMOVE IF PGM USES DB2 DIRECTLY        
008500     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008600     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
008700     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
008800     SKIP3                                                                
008900 01  MESSAGE-CODES.                                                       
009000     03  ERROR-CODES.                                                     
009100         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
009200         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
009300         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
009400         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
009500         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
009600         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
009700         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
009800         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
009900         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
010000         05  ERR-MUST-NOT-BE-ENTERED PIC X(3)    VALUE '033'.             
010100         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
010200         05  ERR-INRE-ALREADY-EXISTS PIC X(3)    VALUE '104'.             
010300         05  ERR-SHLD-NOT-BE-ZERO    PIC X(3)    VALUE '126'.             
010400         05  ERR-ZERO-NOT-ALLOWED    PIC X(3)    VALUE '168'.             
010500         05  ERR-INV-INFO-NOT-FOUND  PIC X(3)    VALUE '221'.             
010600         05  ERR-INV-NOT-COMPLETED   PIC X(3)    VALUE '222'.             
010700         05  ERR-ACS-NOT-ALLOWED     PIC X(3)    VALUE '331'.             
010800         05  ERR-INV-NOT-STARTED     PIC X(3)    VALUE '332'.             
010900     03  INFO-CODES.                                                      
011000         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
011100         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
011200         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
011300         05  INF-PROCESS-STARTED     PIC X(3)    VALUE '015'.             
011400         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
011500         05  INF-INV-INFO-MISSING    PIC X(3)    VALUE '220'.             
011600 01  WS-SAVE-LINE-AREA.                                                   
011700     03 WS-SAVE-LINE-TBL OCCURS 100 TIMES.                                
011800        05 WS-LINE-ADLAGOMR     PIC X(2)         VALUE SPACE.             
011900        05 WS-LINE-ADGANG       PIC X(2)         VALUE SPACE.             
012000        05 WS-LINE-ADPLATS      PIC X(5)         VALUE SPACE.             
012100        05 WS-LINE-IDARTNR      PIC S9(9) COMP-3.                         
012200        05 WS-LINE-ADBUFFOMR    PIC X(2)         VALUE SPACE.             
012300        05 WS-LINE-ADBUFFGANG   PIC X(2)         VALUE SPACE.             
012400        05 WS-LINE-ADBUFFPL     PIC X(5)         VALUE SPACE.             
012500        05 WS-LINE-BEART        PIC X(25)        VALUE SPACE.             
012600        05 WS-LINE-KVBUFF       PIC S9(7) COMP-3 VALUE 0.                 
012700        05 WS-LINE-KVLS         PIC S9(7) VALUE 0.                        
012800     EJECT                                                                
013300*01  -COPY WDECAREA                                                       
013400                                                                          
013500 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
013600*01  -COPY WTRAUTF8                                                       
013700     EJECT                                                                
013800*01  -COPY WL01TIDZ                                                       
013900*                                                                         
014000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
014100     SKIP3                                                                
014200*01  -COPY WZ01SUB                                                        
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
014500     SKIP3                                                                
014600 01  REQU-AREA.                                                           
014700*    03  -COPY WZ01REQU                                                   
014800*    03  -COPY W50292I1                                                   
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
015100     SKIP3                                                                
015200 01  RESP-AREA.                                                           
015300*    03  -COPY WZ01RESP                                                   
015400*    03  -COPY W50292O1                                                   
015500     EJECT                                                                
015600 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
015700     SKIP3                                                                
015800*01  -COPY WZ01SEND                                                       
015900     EJECT                                                                
016000 01  HEADER-AREA                 PIC X(24)    VALUE 'HEADER-AREA'.        
016100 01  DP-HDR-AREA.                                                         
016200*   03  -COPY WZ01REQU -PRE HDR-                                          
016300*   03  -COPY WZ04HDR                                                     
016400                                                                          
016500 01  HDR-AREA.                                                            
016600*   03  -COPY W5029201                                                    
016700*                                                                         
016800 01  LINE-AREA                   PIC X(24)    VALUE 'LINE-AREA'.          
016900 01  DOC-LINE-AREA.                                                       
017000*    03 -COPY W5029202                                                    
017100     EJECT                                                                
017200*    --- WORK-AREAS FOR IMS-SECTIONS                                      
017300*                                                                         
017400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017500     SKIP3                                                                
017600 01  KEYS-FOR-DLI.                                                        
017700     03  W-IDDC-B6-X.                                                     
017800         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
017900     03  W-WDGXKEY-5104-X.                                                
018000          05 W-IDHTYP-5103       PIC X(4)    VALUE '5103'.                
018100          05 W-LOWVALUE          PIC X(26)   VALUE LOW-VALUE.             
018200     03  W-IDDC-5104-X.                                                   
018300         05  W-IDDC-5104         PIC X(2)    VALUE SPACE.                 
018400     03  W-WDGXKEY-5106-X.                                                
018500          05 W-IDHTYP-5105       PIC X(4)    VALUE '5105'.                
018600          05 W-IDDC-5106         PIC X(2)    VALUE SPACE.                 
018700          05 W-LOWVALUE          PIC X(24)   VALUE LOW-VALUE.             
018800     03  W-5106-X.                                                        
018900         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
019000     03  W-WDD801-IDARTNR-X.                                              
019100         05  W-WDD801-IDARTNR    PIC S9(9)   VALUE ZERO   COMP-3.         
019200     03  W-WDD811-IDDC-X.                                                 
019300         05  W-WDD811-IDDC       PIC X(2)    VALUE SPACE.                 
019400     03  W-WDJ7ASEQ-MIN-X.                                                
019500         05  W-IDDC-MIN          PIC X(2).                                
019600         05  W-ADLAGOMR-MIN      PIC 9(2).                                
019700         05  W-ADGANG-MIN        PIC 9(2).                                
019800         05  W-ADPLATS-MIN       PIC 9(5).                                
019900     03  W-WDJ7ASEQ-MAX-X.                                                
020000         05  W-IDDC-MAX          PIC X(2).                                
020100         05  W-ADLAGOMR-MAX      PIC 9(2).                                
020200         05  W-ADGANG-MAX        PIC 9(2).                                
020300         05  W-ADPLATS-MAX       PIC 9(5).                                
020400     03  W-WDJ701-X.                                                      
020500         05  W-WDJ701-IDDC       PIC X(2)    VALUE SPACE.                 
020600         05  W-WDJ701-IDARTNR    PIC S9(9)   VALUE ZERO   COMP-3.         
020700     03  W-IDARTNR-X.                                                     
020800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO   COMP-3.         
020900     03  W-IDSKYLT-X.                                                     
021000         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
021100     SKIP2                                                                
021200*    --- STATUS-KOD FRÅN IMS                                              
021300 01  STATUS-WS                   PIC XX.                                  
021400     88  SEGMENT-FOUND                       VALUE '  '.                  
021500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
021600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
021700     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
021800     SKIP2                                                                
021900 01  GOOD-STATUSCODES.                                                    
022000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022100     SKIP3                                                                
022200 01  SSA1                        PIC X(64).                               
022300 01  SSA2                        PIC X(64).                               
022400     EJECT                                                                
022500*    --- IMS FUNCTION CODES                                               
022600*01  -COPY W0003                                                          
022700     EJECT                                                                
022800                                                                          
022900*    ---  DLI INPUT-OUTPUT AREA                                           
023000                                                                          
023100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
023200 01  DLI-IO-WDB601.                                                       
023300*    03  -COPY WDB601                                                     
023400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5104'.                    
023500 01  DLI-IO-WDGX5104.                                                     
023600*    03  -COPY WDGX5104                                                   
023700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5106'.                    
023800 01  DLI-IO-WDGX5106.                                                     
023900*    03  -COPY WDGX5106                                                   
024000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ701'.                      
024100 01  DLI-IO-WDJ701.                                                       
024200*    03  -COPY WDJ701                                                     
024300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD811'.                      
024400 01  DLI-IO-WDD811.                                                       
024500*    03  -COPY WDD811   -PRE WDD8-                                        
024600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
024700 01  DLI-IO-WDD311.                                                       
024800*    03  -COPY WDD311                                                     
024900                                                                          
025000     EJECT                                                                
025100 LINKAGE SECTION.                                                         
025200*01  -COPY W0009   -PRE MSG-                                              
025300*01  -COPY W0009   -PRE DAP-                                              
025400                                                                          
025500*01  -COPY W0008  -PRE WDB6-                                              
025600     05  FILLER                  PIC X.                                   
025700                                                                          
025800*01  -COPY W0008  -PRE 5104-                                              
025900     05  FILLER                  PIC X.                                   
026000                                                                          
026100*01  -COPY W0008  -PRE 5106-                                              
026200     05  FILLER                  PIC X.                                   
026300                                                                          
026400*01  -COPY W0008  -PRE WDJ7-                                              
026500     05  FILLER                  PIC X.                                   
026600*01  -COPY W0008  -PRE WDJ7A-                                             
026700     05  FILLER                  PIC X.                                   
026800*01  -COPY W0008  -PRE WDD8-                                              
026900     05  FILLER                  PIC X.                                   
027000*01  -COPY W0008  -PRE WDD3-                                              
027100     05  FILLER                  PIC X.                                   
027200     EJECT                                                                
027300 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB 5104-PCB 5106-PCB              
027400               WDJ7-PCB WDJ7A-PCB WDB6-PCB WDD8-PCB WDD3-PCB.             
027500 MAIN SECTION.                                                            
027600     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB 5104-PCB 5106-PCB              
027700               WDJ7-PCB WDJ7A-PCB WDB6-PCB WDD8-PCB WDD3-PCB.             
027800                                                                          
027900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
028000     IF SUB-KDRC = 0                                                      
028100       PERFORM A-INIT                                                     
028200       PERFORM B-CHECK-KEYS                                               
028300       IF KEYS-OK                                                         
028400         PERFORM F-READ-SHOW-INFO                                         
028500       END-IF                                                             
028600       IF KEYS-WRONG OR INDATA-NOT-OK OR REQU-KDPGMACT = 'S'              
028700          PERFORM S02-RETURN-RESPONSE                                     
028800       END-IF                                                             
028900     END-IF                                                               
029000                                                                          
029100     MOVE ZERO TO RETURN-CODE                                             
029200                                                                          
029300     GOBACK                                                               
029400     .                                                                    
029500     EJECT                                                                
029600 A-INIT SECTION.                                                          
029700                                                                          
029800     MOVE ALL '+'                     TO RESP-AREA                        
029900     MOVE SPACE                       TO RESP-IDMSG-ERROR                 
030000                                         RESP-IDMSG-INFO                  
030100                                         RESP-IDELMT-ERROR                
030200     MOVE ZEROS                       TO RESP-KVRADER-TOT                 
030300                                         RESP-KVRADER                     
030400     MOVE '001'                       TO RESP-IDMSGVER                    
030500     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
030600     MOVE FUNCTION CURRENT-DATE (9:6) TO WS-CURRENT-TIME                  
030700     .                                                                    
030800     EJECT                                                                
030900 B-CHECK-KEYS SECTION.                                                    
031000                                                                          
031100     MOVE YES TO KEYS-SW                                                  
031200                                                                          
031300     IF REQU-KDPGMACT = 'S' OR 'P'                                        
031400        CONTINUE                                                          
031500     ELSE                                                                 
031600        MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                       
031700        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
031800        MOVE NOO                TO KEYS-SW                                
031900     END-IF                                                               
032000                                                                          
032100     IF REQU-IDDC = ALL '+'                                               
032200        MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                      
032300        MOVE 'IDDC'              TO RESP-IDELMT-ERROR                     
032400        MOVE NOO                 TO KEYS-SW                               
032500     END-IF                                                               
032600                                                                          
032700     IF KEYS-OK                                                           
032800        MOVE REQU-IDDC          TO W-IDDC-B6                              
032900        PERFORM IMS-GU-WDB601                                             
033000        IF SEGMENT-FOUND                                                  
033100           IF DCS-FLINVACS = YES                                          
033200              CONTINUE                                                    
033300           ELSE                                                           
033400              MOVE ERR-ACS-NOT-ALLOWED                                    
033500                                TO RESP-IDMSG-ERROR                       
033600              MOVE NOO          TO KEYS-SW                                
033700           END-IF                                                         
033800        ELSE                                                              
033900           MOVE NOT-FOUND       TO RESP-IDMSG-ERROR                       
034000           MOVE 'IDDC'          TO RESP-IDELMT-ERROR                      
034100           MOVE NOO             TO KEYS-SW                                
034200        END-IF                                                            
034300     END-IF                                                               
034400                                                                          
034500     .                                                                    
034600     EJECT                                                                
034700 F-READ-SHOW-INFO SECTION.                                                
034800                                                                          
034900* GET LOCAL DATE AND TIME                                                 
035000     MOVE '011'                TO MSGI-KDCALL                             
035100     MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                           
035100     MOVE DCS-IDDC             TO MSGI-IDDC                               
035200     MOVE WS-CURRENT-DATE(3:6) TO MSGI-TILOKDAT                           
035300     MOVE WS-CURRENT-TIME(1:4) TO MSGI-TILOKTID                           
035400     CALL WL01TIDZ   USING      MSGI-WL01TIDZ                             
035500                                                                          
035600     STRING WS-CURRENT-DATE(1:2)                                          
035700            MSGI-TILOKDAT DELIMITED BY SIZE                               
035800                             INTO DP-HEAD-TIDATETIME(1:8)                 
035900                                                                          
036000     STRING MSGI-TILOKTID WS-CURRENT-TIME(5:2)                            
036100            DELIMITED BY SIZE INTO DP-HEAD-TIDATETIME(9:6)                
036200*                                                                         
036300     MOVE REQU-IDDC            TO W-IDDC-5104                             
036400     PERFORM IMS-GU-WDGX5104                                              
036500     IF SEGMENT-FOUND                                                     
036600        IF 5104-DASTADAT > 5104-DASTODAT                                  
036700           PERFORM FA-READ-BASICDATA                                      
036800        ELSE                                                              
036900           MOVE ERR-INV-NOT-STARTED TO RESP-IDMSG-ERROR                   
037000           MOVE NOO              TO INDATA-SW                             
037100        END-IF                                                            
037200     ELSE                                                                 
037300        MOVE ERR-INV-INFO-NOT-FOUND TO RESP-IDMSG-ERROR                   
037400        MOVE NOO                    TO INDATA-SW                          
037500     END-IF                                                               
037600     .                                                                    
037700     EJECT                                                                
037800 FA-READ-BASICDATA SECTION.                                               
037900                                                                          
038000     PERFORM FAA-CHECK-INPUT                                              
038100     IF INDATA-OK                                                         
038200        EVALUATE TRUE                                                     
038300           WHEN REQU-KDPGMACT = 'S'                                       
038400             PERFORM FAB-GET-DATA                                         
038500                                                                          
038600           WHEN REQU-KDPGMACT = 'P'                                       
038700             PERFORM FAB-GET-DATA                                         
038800             IF INDATA-OK                                                 
038900               PERFORM FAC-UPDATE-WDGX5106                                
039000               PERFORM FAD-CREATE-HEADER                                  
039100               IF INDATA-OK                                               
039200                 MOVE +1 TO SAVE-TBL-INDX                                 
039300                 IF REQU-KVART-INVPRINT > WS-KVRADER                      
039400                    MOVE WS-KVRADER TO REQU-KVART-INVPRINT                
039500                 END-IF                                                   
039600                 PERFORM UNTIL SAVE-TBL-INDX > REQU-KVART-INVPRINT        
039700                            OR WS-EXIT                                    
039800                   PERFORM FAE-UPDATE-WDJ701                              
039900                   IF NOT WS-EXIT                                         
040000                      PERFORM FAF-CREATE-LINE                             
040100                      ADD +1 TO SAVE-TBL-INDX                             
040200                   END-IF                                                 
040300                 END-PERFORM                                              
040400                 PERFORM S04-SEND-CLOSE                                   
040500               END-IF                                                     
040600             END-IF                                                       
040700        END-EVALUATE                                                      
040800        IF INDATA-OK                                                      
040900           MOVE WS-KVRADER-TOT       TO RESP-KVRADER-TOT                  
041000           MOVE WS-KVRADER           TO RESP-KVRADER                      
041100        END-IF                                                            
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 FAA-CHECK-INPUT SECTION.                                                 
041600                                                                          
041700     MOVE YES                     TO INDATA-SW                            
041800                                                                          
041900     IF REQU-ADLAGOMR-FOM NOT = ALL '+'                                   
042000        INSPECT REQU-ADLAGOMR-FOM TALLYING WS-CNTR1                       
042100        FOR ALL '-' '+'                                                   
042200        IF WS-CNTR1 > 0                                                   
042300           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
042400           MOVE 'ADLAGOMR-FOM'    TO RESP-IDELMT-ERROR                    
042500           MOVE NOO               TO INDATA-SW                            
042600        END-IF                                                            
042700     END-IF                                                               
042800                                                                          
042900     IF INDATA-OK                                                         
043000       IF REQU-ADLAGOMR-TOM NOT = ALL '+'                                 
043100          MOVE 0                    TO WS-CNTR1                           
043200          INSPECT REQU-ADLAGOMR-TOM TALLYING WS-CNTR1                     
043300          FOR ALL '-' '+'                                                 
043400          IF WS-CNTR1 > 0                                                 
043500             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
043600             MOVE 'ADLAGOMR-TOM'    TO RESP-IDELMT-ERROR                  
043700             MOVE NOO               TO INDATA-SW                          
043800          END-IF                                                          
043900       END-IF                                                             
044000     END-IF                                                               
044100                                                                          
044200     IF INDATA-OK                                                         
044300       IF REQU-ADGANG-FOM NOT = ALL '+'                                   
044400          MOVE 0                    TO WS-CNTR1                           
044500          INSPECT REQU-ADGANG-FOM   TALLYING WS-CNTR1                     
044600          FOR ALL '-' '+'                                                 
044700          IF WS-CNTR1 > 0                                                 
044800             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
044900             MOVE 'ADGANG-FOM'      TO RESP-IDELMT-ERROR                  
045000             MOVE NOO               TO INDATA-SW                          
045100          END-IF                                                          
045200       END-IF                                                             
045300     END-IF                                                               
045400                                                                          
045500     IF INDATA-OK                                                         
045600       IF REQU-ADGANG-TOM NOT = ALL '+'                                   
045700          MOVE 0                    TO WS-CNTR1                           
045800          INSPECT REQU-ADGANG-TOM   TALLYING WS-CNTR1                     
045900          FOR ALL '-' '+'                                                 
046000          IF WS-CNTR1 > 0                                                 
046100             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
046200             MOVE 'ADGANG-TOM'      TO RESP-IDELMT-ERROR                  
046300             MOVE NOO               TO INDATA-SW                          
046400          END-IF                                                          
046500       END-IF                                                             
046600     END-IF                                                               
046700                                                                          
046800     IF INDATA-OK                                                         
046900       IF REQU-ADPLATS NOT = ALL '+'                                      
047000          MOVE 0                    TO WS-CNTR1                           
047100          INSPECT REQU-ADPLATS   TALLYING WS-CNTR1                        
047200          FOR ALL '-' '+'                                                 
047300          IF WS-CNTR1 > 0                                                 
047400             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
047500             MOVE 'ADPLATS'         TO RESP-IDELMT-ERROR                  
047600             MOVE NOO               TO INDATA-SW                          
047700          END-IF                                                          
047800       END-IF                                                             
047900     END-IF                                                               
048000                                                                          
048100     IF INDATA-OK                                                         
048200       IF REQU-ADLAGOMR-FOM NOT = ALL '+'                                 
048300          MOVE REQU-ADLAGOMR-FOM    TO WS-IDFRIDATA                       
048400          PERFORM FAAA-TRANSFORM-NUMERIC-DATA                             
048500          IF DEC-KDSVAR-OK                                                
048600             CONTINUE                                                     
048700          ELSE                                                            
048800             MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                 
048900             MOVE 'ADLAGOMR-FOM'      TO RESP-IDELMT-ERROR                
049000             MOVE NOO                 TO INDATA-SW                        
049100          END-IF                                                          
049200       END-IF                                                             
049300     END-IF                                                               
049400                                                                          
049500     IF INDATA-OK                                                         
049600        IF REQU-ADLAGOMR-TOM NOT = ALL '+'                                
049700        AND REQU-ADLAGOMR-FOM = 0                                         
049800           MOVE ERR-SHLD-NOT-BE-ZERO TO RESP-IDMSG-ERROR                  
049900           MOVE 'ADLAGOMR-FOM'       TO RESP-IDELMT-ERROR                 
050000           MOVE NOO                  TO INDATA-SW                         
050100        ELSE                                                              
050200           MOVE REQU-ADLAGOMR-TOM    TO WS-IDFRIDATA                      
050300           PERFORM FAAA-TRANSFORM-NUMERIC-DATA                            
050400           IF DEC-KDSVAR-OK                                               
050500              CONTINUE                                                    
050600           ELSE                                                           
050700              MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                
050800              MOVE 'ADLAGOMR-TOM'      TO RESP-IDELMT-ERROR               
050900              MOVE NOO                 TO INDATA-SW                       
051000           END-IF                                                         
051100        END-IF                                                            
051200     END-IF                                                               
051300                                                                          
051400     IF INDATA-OK                                                         
051500       EVALUATE TRUE                                                      
051600         WHEN REQU-ADGANG-FOM NOT = ALL '+'                               
051700         AND REQU-ADLAGOMR-FOM = 0                                        
051800           MOVE ERR-SHLD-NOT-BE-ZERO TO RESP-IDMSG-ERROR                  
051900           MOVE 'ADLAGOMR-FOM'       TO RESP-IDELMT-ERROR                 
052000           MOVE NOO                  TO INDATA-SW                         
052100         WHEN REQU-ADGANG-FOM NOT = ALL '+'                               
052200         AND REQU-ADLAGOMR-FOM > 0                                        
052300         AND REQU-ADLAGOMR-TOM > 0                                        
052400         AND REQU-ADLAGOMR-TOM NOT = REQU-ADLAGOMR-FOM                    
052500           MOVE ERR-MUST-NOT-BE-ENTERED TO RESP-IDMSG-ERROR               
052600           MOVE 'ADLAGOMR-TOM'          TO RESP-IDELMT-ERROR              
052700           MOVE NOO                     TO INDATA-SW                      
052800         WHEN REQU-ADGANG-FOM NOT = ALL '+'                               
052900           MOVE REQU-ADGANG-FOM         TO WS-IDFRIDATA                   
053000           PERFORM FAAA-TRANSFORM-NUMERIC-DATA                            
053100           IF DEC-KDSVAR-OK                                               
053200              CONTINUE                                                    
053300           ELSE                                                           
053400              MOVE ERR-MUST-BE-NUMERIC  TO RESP-IDMSG-ERROR               
053500              MOVE 'ADGANG-FOM'         TO RESP-IDELMT-ERROR              
053600              MOVE NOO                  TO INDATA-SW                      
053700           END-IF                                                         
053800       END-EVALUATE                                                       
053900     END-IF                                                               
054000                                                                          
054100     IF INDATA-OK                                                         
054200       EVALUATE TRUE                                                      
054300         WHEN REQU-ADGANG-TOM NOT = ALL '+'                               
054400         AND REQU-ADLAGOMR-FOM = 0                                        
054500           MOVE ERR-SHLD-NOT-BE-ZERO TO RESP-IDMSG-ERROR                  
054600           MOVE 'ADLAGOMR-FOM'       TO RESP-IDELMT-ERROR                 
054700           MOVE NOO                  TO INDATA-SW                         
054800         WHEN REQU-ADGANG-TOM  NOT = ALL '+'                              
054900         AND REQU-ADLAGOMR-FOM > 0                                        
055000         AND REQU-ADLAGOMR-TOM > 0                                        
055100         AND REQU-ADLAGOMR-TOM NOT = REQU-ADLAGOMR-FOM                    
055200           MOVE ERR-MUST-NOT-BE-ENTERED TO RESP-IDMSG-ERROR               
055300           MOVE 'ADLAGOMR-TOM'          TO RESP-IDELMT-ERROR              
055400           MOVE NOO                     TO INDATA-SW                      
055500         WHEN REQU-ADGANG-TOM  NOT = ALL '+'                              
055600         AND REQU-ADLAGOMR-FOM > 0                                        
055700         AND REQU-ADGANG-FOM = 0                                          
055800           MOVE ERR-SHLD-NOT-BE-ZERO    TO RESP-IDMSG-ERROR               
055900           MOVE 'ADGANG-FOM'            TO RESP-IDELMT-ERROR              
056000           MOVE NOO                     TO INDATA-SW                      
056100         WHEN REQU-ADGANG-TOM  NOT = ALL '+'                              
056200           MOVE REQU-ADGANG-TOM         TO WS-IDFRIDATA                   
056300           PERFORM FAAA-TRANSFORM-NUMERIC-DATA                            
056400           IF DEC-KDSVAR-OK                                               
056500              CONTINUE                                                    
056600           ELSE                                                           
056700              MOVE ERR-MUST-BE-NUMERIC  TO RESP-IDMSG-ERROR               
056800              MOVE 'ADGANG-TOM'         TO RESP-IDELMT-ERROR              
056900              MOVE NOO                  TO INDATA-SW                      
057000           END-IF                                                         
057100       END-EVALUATE                                                       
057200     END-IF                                                               
057300                                                                          
057400     IF INDATA-OK                                                         
057500       EVALUATE TRUE                                                      
057600         WHEN REQU-ADPLATS NOT = ALL '+'                                  
057700         AND REQU-ADLAGOMR-FOM = 0                                        
057800           MOVE ERR-SHLD-NOT-BE-ZERO TO RESP-IDMSG-ERROR                  
057900           MOVE 'ADLAGOMR-FOM'       TO RESP-IDELMT-ERROR                 
058000           MOVE NOO                  TO INDATA-SW                         
058100         WHEN REQU-ADPLATS NOT = ALL '+'                                  
058200         AND REQU-ADLAGOMR-FOM > 0                                        
058300         AND REQU-ADLAGOMR-TOM > 0                                        
058400         AND REQU-ADLAGOMR-TOM NOT = REQU-ADLAGOMR-FOM                    
058500           MOVE ERR-MUST-NOT-BE-ENTERED TO RESP-IDMSG-ERROR               
058600           MOVE 'ADLAGOMR-TOM'       TO RESP-IDELMT-ERROR                 
058700           MOVE NOO                  TO INDATA-SW                         
058800         WHEN REQU-ADPLATS NOT = ALL '+'                                  
058900         AND REQU-ADLAGOMR-FOM > 0                                        
059000         AND REQU-ADGANG-FOM = 0                                          
059100           MOVE ERR-SHLD-NOT-BE-ZERO TO RESP-IDMSG-ERROR                  
059200           MOVE 'ADGANG-FOM'         TO RESP-IDELMT-ERROR                 
059300           MOVE NOO                  TO INDATA-SW                         
059400         WHEN REQU-ADPLATS NOT = ALL '+'                                  
059500         AND REQU-ADLAGOMR-FOM > 0                                        
059600         AND REQU-ADGANG-FOM > 0                                          
059700         AND REQU-ADGANG-TOM > 0                                          
059800         AND REQU-ADGANG-TOM NOT = REQU-ADGANG-FOM                        
059900           MOVE ERR-MUST-NOT-BE-ENTERED TO RESP-IDMSG-ERROR               
060000           MOVE 'ADGANG-TOM'         TO RESP-IDELMT-ERROR                 
060100           MOVE NOO                  TO INDATA-SW                         
060200         WHEN REQU-ADPLATS    NOT = ALL '+'                               
060300           MOVE REQU-ADPLATS         TO WS-IDFRIDATA                      
060400           MOVE WS-IDFRIDATA         TO DEC-IDFRIDATA                     
060500           MOVE 5                    TO DEC-KVHELTAL                      
060600           MOVE 0                    TO DEC-KVDECIMAL                     
060700                                                                          
060800           CALL WDECEDIT USING DEC-WDECAREA                               
060900           IF DEC-KDSVAR-OK                                               
061000              CONTINUE                                                    
061100           ELSE                                                           
061200              MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                
061300              MOVE 'ADPLATS'           TO RESP-IDELMT-ERROR               
061400              MOVE NOO                 TO INDATA-SW                       
061500           END-IF                                                         
061600       END-EVALUATE                                                       
061700     END-IF                                                               
061800                                                                          
061900     IF INDATA-OK                                                         
062000        IF REQU-KDBUFFER NOT = 'Y' AND 'N' AND 'A'                        
062100           MOVE ERR-INVALID-FIELD    TO RESP-IDMSG-ERROR                  
062200           MOVE 'KDBUFFER'           TO RESP-IDELMT-ERROR                 
062300           MOVE NOO                  TO INDATA-SW                         
062400        END-IF                                                            
062500     END-IF                                                               
062600                                                                          
062700     IF INDATA-OK                                                         
062800       IF REQU-KDPGMACT = 'P'                                             
062900         IF REQU-KVART-INVPRINT = ALL '+'                                 
063000            MOVE ERR-MUST-BE-ENTERED  TO RESP-IDMSG-ERROR                 
063100            MOVE 'KVART-INVPRINT'     TO RESP-IDELMT-ERROR                
063200            MOVE NOO                  TO INDATA-SW                        
063300         END-IF                                                           
063400       END-IF                                                             
063500     END-IF                                                               
063600                                                                          
063700     IF INDATA-OK                                                         
063800      IF REQU-KDPGMACT = 'P'                                              
063900        MOVE 0                      TO WS-CNTR1                           
064000        INSPECT REQU-KVART-INVPRINT TALLYING WS-CNTR1                     
064100        FOR ALL '-' '+'                                                   
064200        IF WS-CNTR1 > 0                                                   
064300           MOVE ERR-INVALID-FIELD   TO RESP-IDMSG-ERROR                   
064400           MOVE 'KVART-INVPRINT'    TO RESP-IDELMT-ERROR                  
064500           MOVE NOO                 TO INDATA-SW                          
064600        END-IF                                                            
064700      END-IF                                                              
064800     END-IF                                                               
064900                                                                          
065000     IF INDATA-OK                                                         
065100      IF REQU-KDPGMACT = 'P'                                              
065200         MOVE REQU-KVART-INVPRINT  TO WS-IDFRIDATA                        
065300         MOVE WS-IDFRIDATA         TO DEC-IDFRIDATA                       
065400         MOVE 3                    TO DEC-KVHELTAL                        
065500         MOVE 0                    TO DEC-KVDECIMAL                       
065600                                                                          
065700         CALL WDECEDIT USING DEC-WDECAREA                                 
065800         IF DEC-KDSVAR-OK                                                 
065900            CONTINUE                                                      
066000         ELSE                                                             
066100            MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                  
066200            MOVE 'KVART-INVPRINT'    TO RESP-IDELMT-ERROR                 
066300            MOVE NOO                 TO INDATA-SW                         
066400         END-IF                                                           
066500      END-IF                                                              
066600     END-IF                                                               
066700                                                                          
066800     IF INDATA-OK                                                         
066900      IF REQU-KDPGMACT = 'P'                                              
067000         IF REQU-KVART-INVPRINT >= 1                                      
067100         AND REQU-KVART-INVPRINT <= 100                                   
067200            CONTINUE                                                      
067300         ELSE                                                             
067400            MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                    
067500            MOVE 'KVART-INVPRINT'  TO RESP-IDELMT-ERROR                   
067600            MOVE NOO               TO INDATA-SW                           
067700         END-IF                                                           
067800      END-IF                                                              
067900     END-IF                                                               
068000                                                                          
068100     IF INDATA-OK                                                         
068200       EVALUATE TRUE                                                      
068300        WHEN REQU-KDPGMACT = 'P'                                          
068400          IF REQU-IDPRTOMG NOT = '1' AND '2' AND '3'                      
068500            MOVE ERR-INVALID-FIELD    TO RESP-IDMSG-ERROR                 
068600            MOVE 'IDPRTOMG'           TO RESP-IDELMT-ERROR                
068700            MOVE NOO                  TO INDATA-SW                        
068800          END-IF                                                          
068900        WHEN REQU-KDPGMACT = 'S'                                          
069000          IF REQU-IDPRTOMG NOT = '1' AND '2' AND '3'                      
069100            MOVE '0'                  TO REQU-IDPRTOMG                    
069200          END-IF                                                          
069300       END-EVALUATE                                                       
069400     END-IF                                                               
069500                                                                          
069600     IF INDATA-OK                                                         
069700       IF REQU-KDPGMACT = 'P'                                             
069800         IF REQU-IDCOUNTER = ALL '+'                                      
069900            MOVE ERR-MUST-BE-ENTERED  TO RESP-IDMSG-ERROR                 
070000            MOVE 'IDCOUNTER'          TO RESP-IDELMT-ERROR                
070100            MOVE NOO                  TO INDATA-SW                        
070200         ELSE                                                             
070300            INSPECT REQU-IDCOUNTER                                        
070400            CONVERTING LOWER-ALPHA    TO UPPER-ALPHA                      
070500         END-IF                                                           
070600       END-IF                                                             
070700     END-IF                                                               
070800     IF INDATA-OK                                                         
070900       IF REQU-ADGANG-FOM = ALL '+'                                       
071000         MOVE 0 TO REQU-ADGANG-FOM                                        
071100       END-IF                                                             
071200       IF REQU-ADGANG-TOM = ALL '+'                                       
071300         MOVE 0 TO REQU-ADGANG-TOM                                        
071400       END-IF                                                             
071500       IF REQU-ADPLATS = ALL '+'                                          
071600         MOVE 0 TO REQU-ADPLATS                                           
071700       END-IF                                                             
071800     END-IF                                                               
071900     .                                                                    
072000     EJECT                                                                
072100*-----------------------------------------------------------------        
072200* TRANSFORM THE NUMERIC VALUES RECEIVED FROM THE WEB                      
072300*-----------------------------------------------------------------        
072400 FAAA-TRANSFORM-NUMERIC-DATA SECTION.                                     
072500     MOVE WS-IDFRIDATA  TO DEC-IDFRIDATA                                  
072600     MOVE 2             TO DEC-KVHELTAL                                   
072700     MOVE 0             TO DEC-KVDECIMAL                                  
072800                                                                          
072900     CALL WDECEDIT USING DEC-WDECAREA                                     
073000     .                                                                    
073100 FAB-GET-DATA SECTION.                                                    
073200                                                                          
073300     PERFORM FABA-SETUP-SSA                                               
073400     IF INDATA-OK                                                         
073500        IF REQU-KDBUFFER = 'Y' OR 'N' OR 'A'                              
073600           PERFORM FABB-GET-WDJ7-WDD8-LOCATION                            
073700        END-IF                                                            
073800     END-IF                                                               
073900     .                                                                    
074000 FABA-SETUP-SSA SECTION.                                                  
074100                                                                          
074200     MOVE REQU-IDDC  TO W-IDDC-MIN                                        
074300                        W-IDDC-MAX                                        
074400                                                                          
074500     IF REQU-ADLAGOMR-FOM = 0                                             
074600        MOVE 0     TO W-ADLAGOMR-MIN                                      
074700                      W-ADLAGOMR-MAX                                      
074800        MOVE 0     TO W-ADGANG-MIN                                        
074900        MOVE 99    TO W-ADGANG-MAX                                        
075000        MOVE 0     TO W-ADPLATS-MIN                                       
075100        MOVE 99999 TO W-ADPLATS-MAX                                       
075200     END-IF                                                               
075300                                                                          
075400     IF REQU-ADLAGOMR-FOM > 0                                             
075500        MOVE REQU-ADLAGOMR-FOM TO W-ADLAGOMR-MIN                          
075600                                  W-ADLAGOMR-MAX                          
075700        MOVE 0                 TO W-ADGANG-MIN                            
075800        MOVE 99                TO W-ADGANG-MAX                            
075900        MOVE 0                 TO W-ADPLATS-MIN                           
076000        MOVE 99999             TO W-ADPLATS-MAX                           
076100     END-IF                                                               
076200                                                                          
076300     IF REQU-ADLAGOMR-FOM > 0 AND REQU-ADLAGOMR-TOM > 0                   
076400        MOVE REQU-ADLAGOMR-FOM    TO W-ADLAGOMR-MIN                       
076500        IF REQU-ADLAGOMR-TOM >= REQU-ADLAGOMR-FOM                         
076600          MOVE REQU-ADLAGOMR-TOM  TO W-ADLAGOMR-MAX                       
076700        ELSE                                                              
076800           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
076900           MOVE 'ADLAGOMR-TOM'    TO RESP-IDELMT-ERROR                    
077000           MOVE NOO               TO INDATA-SW                            
077100        END-IF                                                            
077200        MOVE 0                    TO W-ADGANG-MIN                         
077300        MOVE 99                   TO W-ADGANG-MAX                         
077400        MOVE 0                    TO W-ADPLATS-MIN                        
077500        MOVE 99999                TO W-ADPLATS-MAX                        
077600     END-IF                                                               
077700                                                                          
077800     IF REQU-ADLAGOMR-FOM > 0                                             
077900     AND REQU-ADGANG-FOM > 0                                              
078000        MOVE REQU-ADLAGOMR-FOM   TO W-ADLAGOMR-MIN                        
078100                                    W-ADLAGOMR-MAX                        
078200        MOVE REQU-ADGANG-FOM     TO W-ADGANG-MIN                          
078300                                    W-ADGANG-MAX                          
078400        MOVE 0                   TO W-ADPLATS-MIN                         
078500        MOVE 99999               TO W-ADPLATS-MAX                         
078600     END-IF                                                               
078700                                                                          
078800     IF REQU-ADLAGOMR-FOM > 0                                             
078900     AND REQU-ADGANG-FOM > 0                                              
079000     AND REQU-ADGANG-TOM > 0                                              
079100        MOVE REQU-ADLAGOMR-FOM    TO W-ADLAGOMR-MIN                       
079200                                     W-ADLAGOMR-MAX                       
079300        MOVE REQU-ADGANG-FOM      TO W-ADGANG-MIN                         
079400        IF REQU-ADGANG-TOM >= REQU-ADGANG-FOM                             
079500           MOVE REQU-ADGANG-TOM   TO W-ADGANG-MAX                         
079600        ELSE                                                              
079700           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
079800           MOVE 'ADGANG-TOM'      TO RESP-IDELMT-ERROR                    
079900           MOVE NOO               TO INDATA-SW                            
080000        END-IF                                                            
080100        MOVE 0                    TO W-ADPLATS-MIN                        
080200        MOVE 99999                TO W-ADPLATS-MAX                        
080300     END-IF                                                               
080400                                                                          
080500     IF REQU-ADLAGOMR-FOM > 0                                             
080600     AND REQU-ADGANG-FOM > 0                                              
080700     AND REQU-ADPLATS > 0                                                 
080800        MOVE REQU-ADLAGOMR-FOM    TO W-ADLAGOMR-MIN                       
080900                                     W-ADLAGOMR-MAX                       
081000        MOVE REQU-ADGANG-FOM      TO W-ADGANG-MIN                         
081100                                     W-ADGANG-MAX                         
081200        MOVE REQU-ADPLATS         TO W-ADPLATS-MIN                        
081300                                     W-ADPLATS-MAX                        
081400     END-IF                                                               
081500     .                                                                    
081600     EJECT                                                                
081700 FABB-GET-WDJ7-WDD8-LOCATION SECTION.                                     
081800     MOVE +1 TO TBL-INDX                                                  
081900                SAVE-TBL-INDX                                             
082000     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
082100       PERFORM IMS-GN-WDJ7A1                                              
082200       IF SEGMENT-FOUND                                                   
082300          PERFORM FABBA-CALC-NUM-OF-PARTS                                 
082400          IF TBL-INDX <= TBL-INDX-MAX                                     
082500            EVALUATE TRUE                                                 
082600             WHEN REQU-IDPRTOMG = 0                                       
082700              IF ACS-IDCOUNTER-P = SPACE                                  
082800                 PERFORM FABBA-GET-BUFFER-LOC                             
082900              ELSE IF ACS-FLKLAR = 'N'                                    
083000                    AND ACS-IDCOUNTER-R = SPACE                           
083100                    AND ACS-IDCOUNTER-P NOT = SPACE                       
083200                    AND ACS-IDCOUNTER-P-REG NOT = SPACE                   
083300                        PERFORM FABBA-GET-BUFFER-LOC                      
083400                    ELSE IF ACS-FLKLAR = 'N'                              
083500                          AND ACS-IDCOUNTER-T = SPACE                     
083600                          AND ACS-IDCOUNTER-P NOT = SPACE                 
083700                          AND ACS-IDCOUNTER-P-REG NOT = SPACE             
083800                          AND ACS-IDCOUNTER-R NOT = SPACE                 
083900                          AND ACS-IDCOUNTER-R-REG NOT = SPACE             
084000                              PERFORM FABBA-GET-BUFFER-LOC                
084100                          END-IF                                          
084200                    END-IF                                                
084300              END-IF                                                      
084400             WHEN REQU-IDPRTOMG = 1                                       
084500              IF ACS-IDCOUNTER-P = SPACE                                  
084600                PERFORM FABBA-GET-BUFFER-LOC                              
084700              END-IF                                                      
084800             WHEN REQU-IDPRTOMG = 2                                       
084900              IF ACS-FLKLAR = 'N'                                         
085000              AND ACS-IDCOUNTER-R = SPACE                                 
085100              AND ACS-IDCOUNTER-P NOT = SPACE                             
085200              AND ACS-IDCOUNTER-P-REG NOT = SPACE                         
085300                PERFORM FABBA-GET-BUFFER-LOC                              
085400              END-IF                                                      
085500             WHEN REQU-IDPRTOMG = 3                                       
085600              IF ACS-FLKLAR = 'N'                                         
085700              AND ACS-IDCOUNTER-T = SPACE                                 
085800              AND ACS-IDCOUNTER-P NOT = SPACE                             
085900              AND ACS-IDCOUNTER-P-REG NOT = SPACE                         
086000              AND ACS-IDCOUNTER-R NOT = SPACE                             
086100              AND ACS-IDCOUNTER-R-REG NOT = SPACE                         
086200                PERFORM FABBA-GET-BUFFER-LOC                              
086300              END-IF                                                      
086400            END-EVALUATE                                                  
086500          END-IF                                                          
086600       ELSE                                                               
086700          IF WS-KVRADER = 0                                               
086800             MOVE ERR-INV-INFO-NOT-FOUND TO RESP-IDMSG-ERROR              
086900             MOVE NOO                    TO INDATA-SW                     
087000          END-IF                                                          
087100       END-IF                                                             
087200     END-PERFORM                                                          
087300     .                                                                    
087400     EJECT                                                                
087500 FABBA-CALC-NUM-OF-PARTS SECTION.                                         
087600                                                                          
087700     EVALUATE TRUE                                                        
087800      WHEN REQU-IDPRTOMG = 0                                              
087900       IF ACS-IDCOUNTER-P = SPACE                                         
088000          ADD +1  TO WS-KVRADER-TOT                                       
088100       ELSE IF ACS-FLKLAR = 'N'                                           
088200             AND ACS-IDCOUNTER-R = SPACE                                  
088300             AND ACS-IDCOUNTER-P NOT = SPACE                              
088400             AND ACS-IDCOUNTER-P-REG NOT = SPACE                          
088500                 ADD +1  TO WS-KVRADER-TOT                                
088600             ELSE IF ACS-FLKLAR = 'N'                                     
088700                   AND ACS-IDCOUNTER-T = SPACE                            
088800                   AND ACS-IDCOUNTER-P NOT = SPACE                        
088900                   AND ACS-IDCOUNTER-P-REG NOT = SPACE                    
089000                   AND ACS-IDCOUNTER-R NOT = SPACE                        
089100                   AND ACS-IDCOUNTER-R-REG NOT = SPACE                    
089200                       ADD +1  TO WS-KVRADER-TOT                          
089300                   END-IF                                                 
089400             END-IF                                                       
089500       END-IF                                                             
089600      WHEN REQU-IDPRTOMG = 1                                              
089700       IF ACS-IDCOUNTER-P = SPACE                                         
089800         ADD +1  TO WS-KVRADER-TOT                                        
089900       END-IF                                                             
090000      WHEN REQU-IDPRTOMG = 2                                              
090100       IF ACS-FLKLAR = 'N'                                                
090200       AND ACS-IDCOUNTER-R = SPACE                                        
090300       AND ACS-IDCOUNTER-P NOT = SPACE                                    
090400       AND ACS-IDCOUNTER-P-REG NOT = SPACE                                
090500         ADD +1  TO WS-KVRADER-TOT                                        
090600       END-IF                                                             
090700      WHEN REQU-IDPRTOMG = 3                                              
090800       IF ACS-FLKLAR = 'N'                                                
090900       AND ACS-IDCOUNTER-T = SPACE                                        
091000       AND ACS-IDCOUNTER-P NOT = SPACE                                    
091100       AND ACS-IDCOUNTER-P-REG NOT = SPACE                                
091200       AND ACS-IDCOUNTER-R NOT = SPACE                                    
091300       AND ACS-IDCOUNTER-R-REG NOT = SPACE                                
091400         ADD +1  TO WS-KVRADER-TOT                                        
091500       END-IF                                                             
091600     END-EVALUATE                                                         
091700     .                                                                    
091800                                                                          
091900 FABBA-GET-BUFFER-LOC SECTION.                                            
092000                                                                          
092100     IF REQU-KDBUFFER = 'Y' OR 'A'                                        
092200      MOVE ACS-IDARTNR  TO W-WDD801-IDARTNR                               
092300      MOVE ACS-IDDC     TO W-WDD811-IDDC                                  
092400      PERFORM IMS-GU-WDD811                                               
092500      IF SEGMENT-FOUND                                                    
092600         ADD +1  TO WS-KVRADER                                            
092700         PERFORM FABBB-MOVE-WDJ7-FIELDS                                   
092800         MOVE WDD8-SALDO-ADBUFFOMR TO WS-TEMP-NUM                         
092900         PERFORM FABBBA-FORMAT-NUMERIC-DATA                               
093000         MOVE WS-TEMP-ALPHA        TO                                     
093100                            RESP-ADBUFFOMR-LINE(TBL-INDX)                 
093200                            WS-LINE-ADBUFFOMR(SAVE-TBL-INDX)              
093300                                                                          
093400         MOVE WDD8-SALDO-ADBUFFGANG TO WS-TEMP-NUM                        
093500         PERFORM FABBBA-FORMAT-NUMERIC-DATA                               
093600         MOVE WS-TEMP-ALPHA         TO                                    
093700                            RESP-ADBUFFGANG-LINE(TBL-INDX)                
093800                            WS-LINE-ADBUFFGANG(SAVE-TBL-INDX)             
093900                                                                          
094000         MOVE WDD8-SALDO-ADBUFFPL   TO                                    
094100                            RESP-ADBUFFPL-LINE(TBL-INDX)                  
094200                            WS-LINE-ADBUFFPL(SAVE-TBL-INDX)               
094300         COMPUTE WS-LINE-KVBUFF(SAVE-TBL-INDX) =                          
094400                 WDD8-SALDO-KVBUFF-F + WDD8-SALDO-KVBUFF-OF               
094500                                                                          
094600         MOVE SPACES    TO STATUS-WS                                      
094700         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                  
094800         OR TBL-INDX = TBL-INDX-MAX                                       
094900           PERFORM IMS-GN-WDD811                                          
095000           IF SEGMENT-FOUND                                               
095100              ADD +1 TO TBL-INDX                                          
095200                        SAVE-TBL-INDX                                     
095300                        WS-KVRADER                                        
095400              MOVE WDD8-SALDO-ADBUFFOMR TO WS-TEMP-NUM                    
095500              PERFORM FABBBA-FORMAT-NUMERIC-DATA                          
095600              MOVE WS-TEMP-ALPHA        TO                                
095700                            RESP-ADBUFFOMR-LINE(TBL-INDX)                 
095800                            WS-LINE-ADBUFFOMR(SAVE-TBL-INDX)              
095900                                                                          
096000              MOVE WDD8-SALDO-ADBUFFGANG TO WS-TEMP-NUM                   
096100              PERFORM FABBBA-FORMAT-NUMERIC-DATA                          
096200              MOVE WS-TEMP-ALPHA        TO                                
096300                                 RESP-ADBUFFGANG-LINE(TBL-INDX)           
096400                                 WS-LINE-ADBUFFGANG(SAVE-TBL-INDX)        
096500                                                                          
096600              MOVE WDD8-SALDO-ADBUFFPL   TO                               
096700                                 RESP-ADBUFFPL-LINE(TBL-INDX)             
096800                                 WS-LINE-ADBUFFPL(SAVE-TBL-INDX)          
096900              COMPUTE WS-LINE-KVBUFF(SAVE-TBL-INDX) =                     
097000                      WDD8-SALDO-KVBUFF-F + WDD8-SALDO-KVBUFF-OF          
097100              MOVE ZEROS    TO RESP-IDARTNR-LINE(TBL-INDX)                
097200                               WS-LINE-IDARTNR(SAVE-TBL-INDX)             
097300                               WS-LINE-KVLS(SAVE-TBL-INDX)                
097400              MOVE SPACES   TO RESP-ADLAGOMR-LINE(TBL-INDX)               
097500                               RESP-ADGANG-LINE(TBL-INDX)                 
097600                               RESP-ADPLATS-LINE(TBL-INDX)                
097700                               WS-LINE-ADLAGOMR(SAVE-TBL-INDX)            
097800                               WS-LINE-ADGANG(SAVE-TBL-INDX)              
097900                               WS-LINE-ADPLATS(SAVE-TBL-INDX)             
098000              MOVE ALL X'20' TO                                           
098100                               RESP-BEART-LINE(TBL-INDX)                  
098200                               WS-LINE-BEART(SAVE-TBL-INDX)               
098300           END-IF                                                         
098400         END-PERFORM                                                      
098500         ADD +1 TO TBL-INDX                                               
098600                   SAVE-TBL-INDX                                          
098700                                                                          
098800      ELSE                                                                
098900        IF REQU-KDBUFFER = 'A'                                            
099000          PERFORM FABBB-MOVE-WDJ7-FIELDS                                  
099100          ADD +1 TO TBL-INDX                                              
099200                    SAVE-TBL-INDX                                         
099300                    WS-KVRADER                                            
099400        END-IF                                                            
099500      END-IF                                                              
099600     END-IF                                                               
099700     IF REQU-KDBUFFER = 'N'                                               
099800        PERFORM FABBB-MOVE-WDJ7-FIELDS                                    
099900        ADD +1 TO TBL-INDX                                                
100000                  SAVE-TBL-INDX                                           
100100                  WS-KVRADER                                              
100200     END-IF                                                               
100300     MOVE SPACES   TO STATUS-WS                                           
100400     .                                                                    
100500     EJECT                                                                
100600 FABBB-MOVE-WDJ7-FIELDS SECTION.                                          
100700                                                                          
100800     MOVE ACS-ADLAGOMR    TO RESP-ADLAGOMR-LINE(TBL-INDX)                 
100900                               WS-LINE-ADLAGOMR(SAVE-TBL-INDX)            
101000     MOVE ACS-ADGANG      TO RESP-ADGANG-LINE(TBL-INDX)                   
101100                               WS-LINE-ADGANG(SAVE-TBL-INDX)              
101200     MOVE ACS-ADPLATS     TO RESP-ADPLATS-LINE(TBL-INDX)                  
101300                               WS-LINE-ADPLATS(SAVE-TBL-INDX)             
101400     MOVE ACS-IDARTNR     TO RESP-IDARTNR-LINE(TBL-INDX)                  
101500                               WS-LINE-IDARTNR(SAVE-TBL-INDX)             
101600     MOVE ACS-BEART       TO RESP-BEART-LINE(TBL-INDX)                    
101700                               WS-LINE-BEART(SAVE-TBL-INDX)               
101800                                                                          
101900*    WE NEED SOME ADJUSTMENT FOR CHINESE NAME                             
102000*    BEFORE SENDING TO resp-area/ws-line                                  
102100                                                                          
102200     MOVE ACS-IDARTNR           TO W-IDARTNR                              
102300     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
102400     IF DCS-UNICODE-IDSKYLT                                               
102500        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
102600     ELSE                                                                 
102700        MOVE '278 '             TO TRAUTF8-KDCP                           
102800     END-IF                                                               
102900     PERFORM IMS-GU-WDD311                                                
103000     IF SEGMENT-FOUND                                                     
103100        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
103200     ELSE                                                                 
103300        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
103400        MOVE '278 '             TO TRAUTF8-KDCP                           
103500     END-IF                                                               
103600     IF TRAUTF8-TECONV-FROM = SPACES                                      
103700      MOVE 'GB'  TO W-IDSKYLT                                             
103800      MOVE '278' TO TRAUTF8-KDCP                                          
103900      PERFORM IMS-GU-WDD311                                               
104000      MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
104100     END-IF                                                               
104200     MOVE 25                    TO TRAUTF8-KVMAXTL                        
104300     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
104400     MOVE TRAUTF8-TECONV-TO     TO RESP-BEART-LINE(TBL-INDX)              
104500                                   WS-LINE-BEART(SAVE-TBL-INDX)           
104600                                                                          
104700     MOVE ACS-KVLS        TO WS-LINE-KVLS(SAVE-TBL-INDX)                  
104800     .                                                                    
104900     EJECT                                                                
105000*-----------------------------------------------------------------        
105100* FORMAT THE DATA                                                         
105200*-----------------------------------------------------------------        
105300 FABBBA-FORMAT-NUMERIC-DATA SECTION.                                      
105400     MOVE ZEROS TO WS-CNTR                                                
105500     INSPECT WS-TEMP-NUM TALLYING WS-CNTR                                 
105600     FOR LEADING ZEROS                                                    
105700     EVALUATE TRUE                                                        
105800      WHEN WS-CNTR = 0                                                    
105900         MOVE WS-TEMP-NUM      TO WS-TEMP-ALPHA                           
106000      WHEN WS-CNTR = 1                                                    
106100         MOVE WS-TEMP-NUM(2:2) TO WS-TEMP-ALPHA                           
106200      WHEN WS-CNTR = 2                                                    
106300         MOVE WS-TEMP-NUM(3:1) TO WS-TEMP-ALPHA                           
106400      WHEN OTHER                                                          
106500         MOVE ZEROS            TO WS-TEMP-ALPHA                           
106600     END-EVALUATE                                                         
106700     .                                                                    
106800 FAC-UPDATE-WDGX5106 SECTION.                                             
106900     MOVE REQU-IDDC    TO W-IDDC-5106                                     
107000     PERFORM IMS-GHU-WDGX5106                                             
107100     IF SEGMENT-FOUND                                                     
107200        EVALUATE TRUE                                                     
107300         WHEN REQU-IDPRTOMG = 1                                           
107400           COMPUTE 5106-IDACSNR-PAKT = 5106-IDACSNR-PAKT + 1              
107500           IF 5106-IDACSNR-PAKT = 5106-IDACSNR-PMAX                       
107600              MOVE 5106-IDACSNR-PMIN TO 5106-IDACSNR-PAKT                 
107700           END-IF                                                         
107800           MOVE 5106-IDACSNR-PAKT    TO DP-HEAD-IDACSNR                   
107900         WHEN REQU-IDPRTOMG = 2                                           
108000           COMPUTE 5106-IDACSNR-RAKT = 5106-IDACSNR-RAKT + 1              
108100           IF 5106-IDACSNR-RAKT = 5106-IDACSNR-RMAX                       
108200              MOVE 5106-IDACSNR-RMIN TO 5106-IDACSNR-RAKT                 
108300           END-IF                                                         
108400           MOVE 5106-IDACSNR-RAKT    TO DP-HEAD-IDACSNR                   
108500         WHEN REQU-IDPRTOMG = 3                                           
108600           COMPUTE 5106-IDACSNR-TAKT = 5106-IDACSNR-TAKT + 1              
108700           IF 5106-IDACSNR-TAKT = 5106-IDACSNR-TMAX                       
108800              MOVE 5106-IDACSNR-TMIN TO 5106-IDACSNR-TAKT                 
108900           END-IF                                                         
109000           MOVE 5106-IDACSNR-TAKT    TO DP-HEAD-IDACSNR                   
109100        END-EVALUATE                                                      
109200        PERFORM IMS-REPL-WDGX5106                                         
109300     ELSE                                                                 
109400        MOVE NOT-FOUND        TO RESP-IDMSG-INFO                          
109500        MOVE 'IDDC'           TO RESP-IDELMT-ERROR                        
109600        MOVE NOO              TO INDATA-SW                                
109700     END-IF                                                               
109800     .                                                                    
109900*-----------------------------------------------------------------        
110000* CREATE HEADER OF THE INVENTORY COUNT SELECTION LIST                     
110100*-----------------------------------------------------------------        
110200 FAD-CREATE-HEADER SECTION.                                               
110300     MOVE 1                          TO HDR-REQU-IDMSGVER                 
110400     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
110500     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
110600                                                                          
110700     MOVE SPACE                      TO HDR-IDOUTREC                      
110800     MOVE REQU-IDDC                  TO HDR-IDOUTREC                      
110900                                        DP-HEAD-IDDC                      
111000     MOVE 'W50292-001'               TO HDR-IDOUTTYPE                     
111100     MOVE WS-CURRENT-DATE            TO HDR-IDLIST                        
111200     MOVE '1         '               TO DP-HEAD-IDAFPRCD                  
111300                                                                          
111400     IF REQU-ADLAGOMR-FOM NOT = ALL '+'                                   
111500        MOVE REQU-ADLAGOMR-FOM       TO DP-HEAD-ADLAGOMR-FOM              
111600     ELSE                                                                 
111700        MOVE ZEROS                   TO DP-HEAD-ADLAGOMR-FOM              
111800     END-IF                                                               
111900     IF REQU-ADLAGOMR-TOM NOT = ALL '+'                                   
112000        MOVE REQU-ADLAGOMR-TOM       TO DP-HEAD-ADLAGOMR-TOM              
112100     ELSE                                                                 
112200        MOVE ZEROS                   TO DP-HEAD-ADLAGOMR-TOM              
112300     END-IF                                                               
112400     IF REQU-ADGANG-FOM   NOT = ALL '+'                                   
112500        MOVE REQU-ADGANG-FOM         TO DP-HEAD-ADGANG-FOM                
112600     ELSE                                                                 
112700        MOVE ZEROS                   TO DP-HEAD-ADGANG-FOM                
112800     END-IF                                                               
112900     IF REQU-ADGANG-TOM   NOT = ALL '+'                                   
113000        MOVE REQU-ADGANG-TOM         TO DP-HEAD-ADGANG-TOM                
113100     ELSE                                                                 
113200        MOVE ZEROS                   TO DP-HEAD-ADGANG-TOM                
113300     END-IF                                                               
113400     IF REQU-ADPLATS   NOT = ALL '+'                                      
113500        MOVE REQU-ADPLATS            TO DP-HEAD-ADPLATS                   
113600     ELSE                                                                 
113700        MOVE ZEROS                   TO DP-HEAD-ADPLATS                   
113800     END-IF                                                               
113900     MOVE REQU-IDCOUNTER             TO DP-HEAD-IDCOUNTER                 
114000                                                                          
114100     PERFORM S04-SEND-OPEN                                                
114200     MOVE SEND-IDCOM                 TO WZ04-SEND-IDCOM                   
114300*HDR                                                                      
114400     PERFORM S05-PUT-DP-HEADER                                            
114500     PERFORM S05-PUT-HEADER                                               
114600     .                                                                    
114700     EJECT                                                                
114800 FAE-UPDATE-WDJ701 SECTION.                                               
114900     MOVE NOO   TO WS-EXIT-SW                                             
115000                                                                          
115100     MOVE REQU-IDDC                        TO W-WDJ701-IDDC               
115200     MOVE WS-LINE-IDARTNR(SAVE-TBL-INDX)   TO W-WDJ701-IDARTNR            
115300     IF W-WDJ701-IDARTNR > 0                                              
115400       PERFORM IMS-GHU-WDJ701                                             
115500       IF SEGMENT-FOUND                                                   
115600         EVALUATE TRUE                                                    
115700          WHEN REQU-IDPRTOMG = 1                                          
115800           MOVE REQU-IDCOUNTER       TO ACS-IDCOUNTER-P                   
115900           MOVE 5106-IDACSNR-PAKT    TO ACS-IDACSNR-P                     
116000           MOVE WS-CURRENT-DATE      TO ACS-TIREGDAT-PCOUNT               
116100           MOVE WS-CURRENT-TIME      TO ACS-TIREGTID-PCOUNT               
116200          WHEN REQU-IDPRTOMG = 2                                          
116300           MOVE REQU-IDCOUNTER       TO ACS-IDCOUNTER-R                   
116400           MOVE 5106-IDACSNR-RAKT    TO ACS-IDACSNR-R                     
116500           MOVE WS-CURRENT-DATE      TO ACS-TIREGDAT-RCOUNT               
116600           MOVE WS-CURRENT-TIME      TO ACS-TIREGTID-RCOUNT               
116700          WHEN REQU-IDPRTOMG = 3                                          
116800           MOVE REQU-IDCOUNTER       TO ACS-IDCOUNTER-T                   
116900           MOVE 5106-IDACSNR-TAKT    TO ACS-IDACSNR-T                     
117000           MOVE WS-CURRENT-DATE      TO ACS-TIREGDAT-TCOUNT               
117100           MOVE WS-CURRENT-TIME      TO ACS-TIREGTID-TCOUNT               
117200         END-EVALUATE                                                     
117300         PERFORM IMS-REPL-WDJ701                                          
117400       ELSE                                                               
117500        MOVE NOT-FOUND        TO RESP-IDMSG-INFO                          
117600        MOVE 'IDDC'           TO RESP-IDELMT-ERROR                        
117700        MOVE NOO              TO INDATA-SW                                
117800        SET WS-EXIT           TO TRUE                                     
117900       END-IF                                                             
118000     END-IF                                                               
118100     .                                                                    
118200     EJECT                                                                
118300*-----------------------------------------------------------------        
118400* CREATE LINES  OF THE INVENTORY COUNT SELECTION LIST                     
118500*-----------------------------------------------------------------        
118600 FAF-CREATE-LINE SECTION.                                                 
118700     MOVE '2         '                    TO DP-LINE-IDAFPRCD             
118800     MOVE WS-LINE-ADLAGOMR(SAVE-TBL-INDX) TO DP-LINE-ADLAGOMR             
118900     MOVE WS-LINE-ADGANG(SAVE-TBL-INDX)   TO DP-LINE-ADGANG               
119000     MOVE WS-LINE-ADPLATS(SAVE-TBL-INDX)  TO DP-LINE-ADPLATS              
119100     MOVE WS-LINE-IDARTNR(SAVE-TBL-INDX)  TO DP-LINE-IDARTNR              
119200     MOVE WS-LINE-BEART(SAVE-TBL-INDX)    TO DP-LINE-BEART                
119300                                                                          
119400*    IF 5104-FLBLINDCO = 'N'                                              
119500       MOVE WS-LINE-ADBUFFOMR(SAVE-TBL-INDX) TO DP-LINE-ADBUFFOMR         
119600       MOVE WS-LINE-ADBUFFGANG(SAVE-TBL-INDX) TO                          
119700                                            DP-LINE-ADBUFFGANG            
119800       MOVE WS-LINE-ADBUFFPL(SAVE-TBL-INDX) TO DP-LINE-ADBUFFPL           
119900*    ELSE                                                                 
120000*      MOVE SPACES                        TO DP-LINE-ADBUFFOMR            
120100*                                            DP-LINE-ADBUFFGANG           
120200*                                            DP-LINE-ADBUFFPL             
120300*    END-IF                                                               
120400     IF 5104-FLBLINDCO = 'N'                                              
120500       IF WS-LINE-KVBUFF(SAVE-TBL-INDX) < 0                               
120600         MOVE WS-LINE-KVBUFF(SAVE-TBL-INDX) TO DP-LINE-KVBUFF             
120700         MOVE '-'                           TO DP-LINE-KVBUFF(7:1)        
120800       ELSE                                                               
120900         MOVE WS-LINE-KVBUFF(SAVE-TBL-INDX) TO DP-LINE-KVBUFF             
121000       END-IF                                                             
121100       INSPECT DP-LINE-KVBUFF REPLACING LEADING ZEROS BY SPACES           
121200       IF DP-LINE-KVBUFF = ALL SPACES                                     
121300          MOVE 0                            TO DP-LINE-KVBUFF             
121400       END-IF                                                             
121500     ELSE                                                                 
121600         MOVE SPACES                        TO DP-LINE-KVBUFF             
121700     END-IF                                                               
121800     IF 5104-FLBLINDCO = 'N'                                              
121900       IF WS-LINE-KVLS(SAVE-TBL-INDX) < 0                                 
122000         MOVE WS-LINE-KVLS(SAVE-TBL-INDX)   TO DP-LINE-KVLS               
122100         MOVE '-'                           TO DP-LINE-KVLS(8:1)          
122200       ELSE                                                               
122300         MOVE WS-LINE-KVLS(SAVE-TBL-INDX)   TO DP-LINE-KVLS               
122400       END-IF                                                             
122500       INSPECT DP-LINE-KVLS REPLACING LEADING ZEROS BY SPACES             
122600       IF DP-LINE-KVLS = ALL SPACES                                       
122700          MOVE 0                           TO DP-LINE-KVLS                
122800       END-IF                                                             
122900     ELSE                                                                 
123000       MOVE SPACES                         TO DP-LINE-KVLS                
123100     END-IF                                                               
123200     PERFORM S06-PUT-REPORT-LINE                                          
123300     .                                                                    
123400*    --- DISPATCHER SECTIONS                                              
123500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
123600                                                                          
123700     MOVE 'GETARG'               TO SUB-KDFUNC                            
123800     MOVE WS-ADDRESS             TO SUB-ADDISPABS                         
123900     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
124000                                                                          
124100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
124200                                                                          
124300     IF SUB-KDRC > 0                                                      
124400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
124500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
124600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
124700       DISPLAY ERROR-TEXT                                                 
124800       CALL FELLOG                                                        
124900     END-IF                                                               
125000     .                                                                    
125100     SKIP3                                                                
125200 S02-RETURN-RESPONSE SECTION.                                             
125300                                                                          
125400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
125500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
125600                                                                          
125700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
125800                                                                          
125900     IF SUB-KDRC > 0                                                      
126000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
126100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
126200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
126300       DISPLAY ERROR-TEXT                                                 
126400       CALL FELLOG                                                        
126500     END-IF                                                               
126600     .                                                                    
126700     EJECT                                                                
126800 S04-SEND-OPEN SECTION.                                                   
126900                                                                          
127000     MOVE 'OPEN'                     TO SEND-KDFUNC                       
127100     MOVE 'CARPARTS.DAP.DISTRDOC'    TO SEND-ADDISPABS                    
127200     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
127300                                                                          
127400     IF SEND-KDRC > 0                                                     
127500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
127600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
127700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
127800       DISPLAY ERROR-TEXT                                                 
127900       CALL FELLOG                                                        
128000     END-IF                                                               
128100     .                                                                    
128200     SKIP3                                                                
128300 S04-SEND-CLOSE SECTION.                                                  
128400                                                                          
128500     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
128600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
128700                                                                          
128800     IF SEND-KDRC > 0                                                     
128900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
129000       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
129100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
129200       DISPLAY ERROR-TEXT                                                 
129300       CALL FELLOG                                                        
129400     END-IF                                                               
129500     .                                                                    
129600     EJECT                                                                
129700 S05-PUT-DP-HEADER SECTION.                                               
129800                                                                          
129900     MOVE 'PUT'                           TO SEND-KDFUNC                  
130000     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
130100     MOVE LENGTH OF DP-HDR-AREA           TO SEND-KVDLEN                  
130200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
130300                         SEND-KVDLEN                                      
130400                         DP-HDR-AREA                                      
130500     IF SEND-KDRC > ZERO                                                  
130600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
130700       STRING 'WZ01SEND PUT-DP-HDR ERROR RC=' KDRC-DISPLAY                
130800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
130900       DISPLAY ERROR-TEXT                                                 
131000       CALL FELLOG                                                        
131100     END-IF                                                               
131200     .                                                                    
131300     EJECT                                                                
131400 S05-PUT-HEADER SECTION.                                                  
131500                                                                          
131600     MOVE 'PUT'                           TO SEND-KDFUNC                  
131700     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
131800     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
131900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
132000                         SEND-KVDLEN                                      
132100                         HDR-AREA                                         
132200     IF SEND-KDRC > ZERO                                                  
132300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
132400       STRING 'WZ01SEND PUT-HDR ERROR RC=' KDRC-DISPLAY                   
132500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
132600       DISPLAY ERROR-TEXT                                                 
132700       CALL FELLOG                                                        
132800     END-IF                                                               
132900     .                                                                    
133000     EJECT                                                                
133100 S06-PUT-REPORT-LINE    SECTION.                                          
133200                                                                          
133300     MOVE 'PUT'                           TO SEND-KDFUNC                  
133400     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
133500     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
133600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
133700                         SEND-KVDLEN                                      
133800                         DOC-LINE-AREA                                    
133900     IF SEND-KDRC > ZERO                                                  
134000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
134100       STRING 'WZ01SEND PUT-LINE ERROR RC=' KDRC-DISPLAY                  
134200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
134300       DISPLAY ERROR-TEXT                                                 
134400       CALL FELLOG                                                        
134500     END-IF                                                               
134600     .                                                                    
134700     SKIP3                                                                
134800 IMS-GU-WDB601 SECTION.                                                   
134900                                                                          
135000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
135100          DELIMITED BY SIZE INTO SSA1                                     
135200     MOVE '  GE' TO GOOD-STATUSCODES                                      
135300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
135400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
135500     PERFORM IMS-STATUSCHECK                                              
135600     .                                                                    
135700     EJECT                                                                
135800 IMS-GU-WDGX5104 SECTION.                                                 
135900                                                                          
136000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-5104-X ')'                    
136100          DELIMITED BY SIZE INTO SSA1                                     
136200     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
136300          DELIMITED BY SIZE INTO SSA2                                     
136400     MOVE '  GE' TO GOOD-STATUSCODES                                      
136500     CALL CBLTDLI USING GU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2             
136600     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
136700     PERFORM IMS-STATUSCHECK                                              
136800     .                                                                    
136900 IMS-GHU-WDGX5106 SECTION.                                                
137000                                                                          
137100     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-5106-X ')'                    
137200          DELIMITED BY SIZE INTO SSA1                                     
137300     STRING 'WDGX5106(KDSEGKEY =' W-5106-X ')'                            
137400          DELIMITED BY SIZE INTO SSA2                                     
137500     MOVE '  GE' TO GOOD-STATUSCODES                                      
137600     CALL CBLTDLI USING GHU 5106-PCB DLI-IO-WDGX5106 SSA1 SSA2            
137700     MOVE 5106-STATUS-CODE TO STATUS-WS                                   
137800     PERFORM IMS-STATUSCHECK                                              
137900     .                                                                    
138000     EJECT                                                                
138100 IMS-GHU-WDJ701 SECTION.                                                  
138200                                                                          
138300     STRING  'WDJ701  (WDJ701KY =' W-WDJ701-X ')'                         
138400          DELIMITED BY SIZE INTO SSA1                                     
138500     MOVE '  GE' TO GOOD-STATUSCODES                                      
138600     CALL CBLTDLI USING GHU WDJ7-PCB DLI-IO-WDJ701 SSA1                   
138700     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
138800     PERFORM IMS-STATUSCHECK                                              
138900     .                                                                    
139000     SKIP3                                                                
139100 IMS-GN-WDJ7A1    SECTION.                                                
139200                                                                          
139300     STRING 'WDJ701  (WDJ7ASEQ>=' W-WDJ7ASEQ-MIN-X                        
139400                    '&WDJ7ASEQ<=' W-WDJ7ASEQ-MAX-X  ')'                   
139500          DELIMITED BY SIZE INTO SSA1                                     
139600     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
139700     CALL CBLTDLI USING GN WDJ7A-PCB DLI-IO-WDJ701 SSA1                   
139800     MOVE WDJ7A-STATUS-CODE TO STATUS-WS                                  
139900     PERFORM IMS-STATUSCHECK                                              
140000     .                                                                    
140100 IMS-GU-WDD811 SECTION.                                                   
140200     STRING 'WDD801  (IDARTNR  =' W-WDD801-IDARTNR-X ')'                  
140300          DELIMITED BY SIZE INTO SSA1                                     
140400     STRING 'WDD811  (IDDC     =' W-WDD811-IDDC-X ')'                     
140500          DELIMITED BY SIZE INTO SSA2                                     
140600     MOVE '  GE'           TO GOOD-STATUSCODES                            
140700     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD811 SSA1 SSA2               
140800     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
140900     PERFORM IMS-STATUSCHECK                                              
141000     .                                                                    
141100     EJECT                                                                
141200 IMS-GN-WDD811 SECTION.                                                   
141300     STRING 'WDD801  (IDARTNR  =' W-WDD801-IDARTNR-X ')'                  
141400          DELIMITED BY SIZE INTO SSA1                                     
141500     STRING 'WDD811  (IDDC     =' W-WDD811-IDDC-X ')'                     
141600          DELIMITED BY SIZE INTO SSA2                                     
141700     MOVE '  GEGB'         TO GOOD-STATUSCODES                            
141800     CALL CBLTDLI USING GN WDD8-PCB DLI-IO-WDD811 SSA1 SSA2               
141900     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
142000     PERFORM IMS-STATUSCHECK                                              
142100     .                                                                    
142200 IMS-REPL-WDJ701 SECTION.                                                 
142300                                                                          
142400     MOVE '  ' TO GOOD-STATUSCODES                                        
142500     CALL CBLTDLI USING REPL WDJ7-PCB DLI-IO-WDJ701                       
142600     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
142700     PERFORM IMS-STATUSCHECK                                              
142800     .                                                                    
142900     EJECT                                                                
143000 IMS-REPL-WDGX5106 SECTION.                                               
143100                                                                          
143200     MOVE '  '                TO GOOD-STATUSCODES                         
143300     CALL CBLTDLI USING REPL 5106-PCB DLI-IO-WDGX5106                     
143400     MOVE 5106-STATUS-CODE    TO STATUS-WS                                
143500     PERFORM IMS-STATUSCHECK                                              
143600     .                                                                    
143700 IMS-GU-WDD311 SECTION.                                                   
143800                                                                          
143900     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
144000             DELIMITED BY SIZE INTO SSA1                                  
144100     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
144200             DELIMITED BY SIZE INTO SSA2                                  
144300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
144400     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
144500     MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
144600     PERFORM IMS-STATUSCHECK                                              
144700     .                                                                    
144800 IMS-STATUSCHECK SECTION.                                                 
144900     SKIP2                                                                
145000     SET STATUS-IX TO 1                                                   
145100     SEARCH GOOD-STATUS                                                   
145200       AT END                                                             
145300         STRING 'INVALID STATUS CODE FROM IMS: ' STATUS-WS                
145400           DELIMITED BY SIZE INTO ERROR-TEXT                              
145500         DISPLAY ERROR-TEXT                                               
145600         CALL FELLOG                                                      
145700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
145800         CONTINUE                                                         
145900     END-SEARCH                                                           
146000     .                                                                    
