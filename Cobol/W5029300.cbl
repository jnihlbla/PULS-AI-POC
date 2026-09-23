000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5029300.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   11/11/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.PULS.ACSFOLLOWUPSELECTION                       
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        INVENTORY FOLLOW UP SELECTION                                    
001100*                                                                         
001200*        THE PROGRAM READS     WDJ7                                       
001300*        THE PROGRAM READS     WDR2                                       
001400*        THE PROGRAM READS     WDB6                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W50293T                                             
001800*        REQUEST:     W50293I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    W50293O1                                            
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 FILE SECTION.                                                            
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W5029300'.            
003300 77  WS-IDFRIDATA                PIC X(25).                               
003400 77  WS-RESP-AREA-LENGTH         PIC 9(5) COMP.                           
003500 77  WS-KVART-INVPRINT-LINE      PIC 9(7) VALUE 0.                        
003600 77  WS-KVART-INVPRINT-LINE-NEW  PIC 9(7) VALUE 0.                        
003700 77  WS-KVART-INVLEFT-LINE       PIC 9(7) VALUE 0.                        
003800 77  WS-KVART-INVTOT-LINE        PIC 9(7) VALUE 0.                        
003900 77  WS-ADDRESS                  PIC X(34)   VALUE                        
004000               'CARPARTS.PULS.ACSFOLLOWUPSELECTION'.                      
004100 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004200 77  TBL-INDX                    PIC 9(3)    VALUE 0.                     
004300 77  TBL-MAX                     PIC 9(3)    VALUE 100.                   
004400 77  WS-KVRADER                  PIC 9(5) VALUE 0.                        
004500 77  WS-KVRADER-NUM              PIC 9(5) VALUE 0.                        
004600 77  WS-CNTR1                    PIC 9(5) VALUE 0.                        
004700 77  WS-SAVE-IDACSNR-P           PIC S9(7) COMP-3 VALUE 0.                
004800 77  WS-SAVE-IDACSNR-R           PIC S9(7) COMP-3 VALUE 0.                
004900 77  WS-SAVE-IDACSNR-T           PIC S9(7) COMP-3 VALUE 0.                
005000 77  WS-SAVE-ADLAGOMR            PIC 9(2) VALUE 0.                        
005100 77  WS-SAVE-ADGANG              PIC S9(2) VALUE 0.                       
005200                                                                          
005300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005500 77  KDRC-DISPLAY                PIC Z(5).                                
005600 77  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
005700 77  WS-CURRENT-TIME             PIC 9(6)    VALUE ZERO.                  
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
006900 77  WS-PRINT-SW                 PIC X       VALUE 'N'.                   
007000     88  WS-PRINT                            VALUE 'Y'.                   
007100     EJECT                                                                
007200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007300 01  GENERAL-SUBPROGRAMS.                                                 
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008100     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
008200*01  -COPY WDECAREA                                                       
008300     EJECT                                                                
008400*01  -COPY WL01TIDZ                                                       
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008700*01  -COPY WZ01SUB                                                        
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009000 01  REQU-AREA.                                                           
009100*    03  -COPY WZ01REQU                                                   
009200*    03  -COPY W50293I1                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009500 01  RESP-AREA.                                                           
009600*    03  -COPY WZ01RESP                                                   
009700*    03  -COPY W50293O1                                                   
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
010000*01  -COPY WZ01SEND                                                       
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
010300 01  HEADER-AREA                 PIC X(24)    VALUE 'HEADER-AREA'.        
010400 01  DP-HDR-AREA.                                                         
010500*   03  -COPY WZ01REQU -PRE HDR-                                          
010600*   03  -COPY WZ04HDR                                                     
010700                                                                          
010800 01  HDR-AREA.                                                            
010900*   03  -COPY W5029301                                                    
011000*                                                                         
011100 01  LINE-AREA                   PIC X(24)    VALUE 'LINE-AREA'.          
011200 01  DOC-LINE-AREA.                                                       
011300*    03 -COPY W5029302                                                    
011400     EJECT                                                                
011500 01  MESSAGE-CODES.                                                       
011600     03  ERROR-CODES.                                                     
011700         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
011800         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
011900         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
012000         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
012100         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
012200         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
012300         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
012400         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
012500         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
012600         05  ERR-MUST-NOT-BE-ENTERED PIC X(3)    VALUE '033'.             
012700         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
012800         05  ERR-INRE-ALREADY-EXISTS PIC X(3)    VALUE '104'.             
012900         05  ERR-SHLD-NOT-BE-ZERO    PIC X(3)    VALUE '126'.             
013000         05  ERR-ZERO-NOT-ALLOWED    PIC X(3)    VALUE '168'.             
013100         05  ERR-INV-INFO-NOT-FOUND  PIC X(3)    VALUE '221'.             
013200         05  ERR-INV-NOT-COMPLETED   PIC X(3)    VALUE '222'.             
013300         05  ERR-ACS-NOT-ALLOWED     PIC X(3)    VALUE '331'.             
013310         05  ERR-INV-NOT-STARTED     PIC X(3)    VALUE '332'.             
013400     03  INFO-CODES.                                                      
013500         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
013600         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
013700         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
013800         05  INF-PROCESS-STARTED     PIC X(3)    VALUE '015'.             
013900         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
014000         05  INF-INV-INFO-MISSING    PIC X(3)    VALUE '220'.             
014200*    --- WORK-AREAS FOR IMS-SECTIONS                                      
014300*                                                                         
014400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014500 01  KEYS-FOR-DLI.                                                        
014600     03  W-IDDC-B6-X.                                                     
014700         05  W-IDDC-B6            PIC X(2)    VALUE SPACE.                
014800     03  W-WDGXKEY-5104-X.                                                
014900          05 W-IDHTYP             PIC X(4)    VALUE '5103'.               
015000          05 W-LOWVALUE           PIC X(26)   VALUE LOW-VALUE.            
015100     03  W-IDDC-5104-X.                                                   
015200         05  W-IDDC-5104          PIC X(2)    VALUE SPACE.                
015300     03  W-WDJ701-MIN-X.                                                  
015400         05  W-WDJ701-IDDC-MIN    PIC X(2)    VALUE SPACE.                
015500         05  W-WDJ701-IDARTNR-MIN PIC S9(9) COMP-3                        
015600                                              VALUE 0.                    
015700     03  W-WDJ701-MAX-X.                                                  
015800         05  W-WDJ701-IDDC-MAX    PIC X(2)    VALUE SPACE.                
015900         05  W-WDJ701-IDARTNR-MAX PIC S9(9) COMP-3                        
016000                                              VALUE 999999999.            
016100     03  W-WDJ7ASEQ-MIN-X.                                                
016200         05  W-WDJ7A1-IDDC-MIN     PIC X(2) VALUE SPACE.                  
016300         05  W-WDJ7A1-ADLAGOMR-MIN PIC 9(2) VALUE 0.                      
016400         05  W-WDJ7A1-ADGANG-MIN   PIC 9(2) VALUE 0.                      
016500         05  W-WDJ7A1-ADPLATS-MIN  PIC 9(5) VALUE 0.                      
016600     03  W-WDJ7ASEQ-MAX-X.                                                
016700         05  W-WDJ7A1-IDDC-MAX     PIC X(2) VALUE SPACE.                  
016800         05  W-WDJ7A1-ADLAGOMR-MAX PIC 9(2) VALUE 99.                     
016900         05  W-WDJ7A1-ADGANG-MAX   PIC 9(2) VALUE 99.                     
017000         05  W-WDJ7A1-ADPLATS-MAX  PIC 9(5) VALUE 99999.                  
017100     03  W-WDJ7D1SEQ-MIN-X.                                               
017200         05  W-IDDC-D1-MIN       PIC X(2) VALUE SPACE.                    
017300         05  W-IDACSNR-P-MIN     PIC S9(7) COMP-3 VALUE 100000.           
017400     03  W-WDJ7D1SEQ-MAX-X.                                               
017500         05  W-IDDC-D1-MAX       PIC X(2) VALUE SPACE.                    
017600         05  W-IDACSNR-P-MAX     PIC S9(7) COMP-3 VALUE 199999.           
017700     03  W-WDJ7E1SEQ-MIN-X.                                               
017800         05  W-IDDC-E1-MIN       PIC X(2) VALUE SPACE.                    
017900         05  W-IDACSNR-R-MIN     PIC S9(7) COMP-3 VALUE 200000.           
018000     03  W-WDJ7E1SEQ-MAX-X.                                               
018100         05  W-IDDC-E1-MAX       PIC X(2) VALUE SPACE.                    
018200         05  W-IDACSNR-R-MAX     PIC S9(7) COMP-3 VALUE 299999.           
018300     03  W-WDJ7F1SEQ-MIN-X.                                               
018400         05  W-IDDC-F1-MIN       PIC X(2) VALUE SPACE.                    
018500         05  W-IDACSNR-T-MIN     PIC S9(7) COMP-3 VALUE 300000.           
018600     03  W-WDJ7F1SEQ-MAX-X.                                               
018700         05  W-IDDC-F1-MAX       PIC X(2) VALUE SPACE.                    
018800         05  W-IDACSNR-T-MAX     PIC S9(7) COMP-3 VALUE 399999.           
018900*    --- STATUS-KOD FRÅN IMS                                              
019000 01  STATUS-WS                   PIC XX.                                  
019100     88  SEGMENT-FOUND                       VALUE '  '.                  
019200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
019300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
019400     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
019500 01  GOOD-STATUSCODES.                                                    
019600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019700 01  SSA1                        PIC X(64).                               
019800 01  SSA2                        PIC X(64).                               
019900     EJECT                                                                
020000*    --- IMS FUNCTION CODES                                               
020100*01  -COPY W0003                                                          
020200     EJECT                                                                
020300                                                                          
020400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ701'.                      
020500 01  DLI-IO-WDJ701.                                                       
020600*    03  -COPY WDJ701                                                     
020700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ7A1'.                      
020800 01  DLI-IO-WDJ7A1.                                                       
020900*    03  -COPY WDJ7A1                                                     
021000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5104'.                    
021100 01  DLI-IO-WDGX5104.                                                     
021200*    03  -COPY WDGX5104                                                   
021300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
021400 01  DLI-IO-WDB601.                                                       
021500*    03  -COPY WDB601                                                     
021600     EJECT                                                                
021700 LINKAGE SECTION.                                                         
021800*01  -COPY W0009   -PRE MSG-                                              
021900*01  -COPY W0009   -PRE DAP-                                              
022000                                                                          
022100*01  -COPY W0008  -PRE WDJ7-                                              
022200     05  FILLER                  PIC X.                                   
022300                                                                          
022400*01  -COPY W0008  -PRE WDJ7A-                                             
022500     05  FILLER                  PIC X.                                   
022600                                                                          
022700*01  -COPY W0008  -PRE WDJ7D-                                             
022800     05  FILLER                  PIC X.                                   
022900                                                                          
023000*01  -COPY W0008  -PRE WDJ7E-                                             
023100     05  FILLER                  PIC X.                                   
023200                                                                          
023300*01  -COPY W0008  -PRE WDJ7F-                                             
023400     05  FILLER                  PIC X.                                   
023500                                                                          
023600*01  -COPY W0008  -PRE 5104-                                              
023700     05  FILLER                  PIC X.                                   
023800                                                                          
023900*01  -COPY W0008  -PRE WDB6-                                              
024000     05  FILLER                  PIC X.                                   
024100     EJECT                                                                
024200 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDJ7-PCB WDJ7A-PCB             
024300             WDJ7D-PCB WDJ7E-PCB WDJ7F-PCB 5104-PCB WDB6-PCB.             
024400                                                                          
024500 MAIN SECTION.                                                            
024600     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDJ7-PCB WDJ7A-PCB             
024700             WDJ7D-PCB WDJ7E-PCB WDJ7F-PCB 5104-PCB WDB6-PCB.             
024800                                                                          
024900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
025000     IF SUB-KDRC = 0                                                      
025100       PERFORM A-INIT                                                     
025200       PERFORM B-CHECK-KEYS                                               
025300       IF KEYS-OK                                                         
025400         PERFORM F-READ-SHOW-INFO                                         
025500       END-IF                                                             
025600       PERFORM S02-RETURN-RESPONSE                                        
025700     END-IF                                                               
025800                                                                          
025900     MOVE ZERO TO RETURN-CODE                                             
026000     GOBACK                                                               
026100     .                                                                    
026200     EJECT                                                                
026300 A-INIT SECTION.                                                          
026400                                                                          
026500     MOVE ALL '+'                     TO RESP-AREA                        
026600     MOVE SPACE                       TO RESP-IDMSG-ERROR                 
026700                                         RESP-IDMSG-INFO                  
026800                                         RESP-IDELMT-ERROR                
026900     MOVE ZEROS                       TO RESP-KVRADER                     
027000     MOVE '001'                       TO RESP-IDMSGVER                    
027100     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
027200     MOVE FUNCTION CURRENT-DATE (9:6) TO WS-CURRENT-TIME                  
027300     .                                                                    
027400     EJECT                                                                
027500 B-CHECK-KEYS SECTION.                                                    
027600                                                                          
027700     MOVE YES TO KEYS-SW                                                  
027800                                                                          
027900     IF REQU-IDDC = ALL '+'                                               
028000        MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                      
028100        MOVE 'IDDC'              TO RESP-IDELMT-ERROR                     
028200        MOVE NOO                 TO KEYS-SW                               
028300     END-IF                                                               
028400                                                                          
028500     IF KEYS-OK                                                           
028600        MOVE REQU-IDDC          TO W-IDDC-B6                              
028700        PERFORM IMS-GU-WDB601                                             
028800        IF SEGMENT-FOUND                                                  
028900           IF DCS-FLINVACS = YES                                          
029000              CONTINUE                                                    
029100           ELSE                                                           
029200              MOVE ERR-ACS-NOT-ALLOWED                                    
029300                                TO RESP-IDMSG-ERROR                       
029400              MOVE NOO          TO KEYS-SW                                
029500           END-IF                                                         
029600        ELSE                                                              
029700           MOVE NOT-FOUND       TO RESP-IDMSG-ERROR                       
029800           MOVE 'IDDC'          TO RESP-IDELMT-ERROR                      
029900           MOVE NOO             TO KEYS-SW                                
030000        END-IF                                                            
030100     END-IF                                                               
030200                                                                          
030300     IF KEYS-OK                                                           
030400       IF REQU-KDPGMACT = 'S' OR 'P'                                      
030500          CONTINUE                                                        
030600       ELSE                                                               
030700          MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                     
030800          MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                    
030900          MOVE NOO                TO KEYS-SW                              
031000       END-IF                                                             
031100     END-IF                                                               
031200                                                                          
031300     .                                                                    
031400     EJECT                                                                
031500 F-READ-SHOW-INFO SECTION.                                                
031600                                                                          
031700* GET LOCAL DATE AND TIME                                                 
031800     MOVE '011'                TO MSGI-KDCALL                             
031900     MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                           
031900     MOVE DCS-IDDC             TO MSGI-IDDC                               
032000     MOVE WS-CURRENT-DATE(3:6) TO MSGI-TILOKDAT                           
032100     MOVE WS-CURRENT-TIME(1:4) TO MSGI-TILOKTID                           
032200     CALL WL01TIDZ   USING      MSGI-WL01TIDZ                             
032300                                                                          
032400     STRING WS-CURRENT-DATE(1:2)                                          
032500            MSGI-TILOKDAT DELIMITED BY SIZE                               
032600                             INTO DP-HEAD-TIDATETIME(1:8)                 
032700                                                                          
032800     STRING MSGI-TILOKTID WS-CURRENT-TIME(5:2)                            
032900            DELIMITED BY SIZE INTO DP-HEAD-TIDATETIME(9:6)                
033000*                                                                         
033100     MOVE REQU-IDDC            TO W-IDDC-5104                             
033200     PERFORM IMS-GU-WDGX5104                                              
033300     IF SEGMENT-FOUND                                                     
033400        IF 5104-DASTADAT > 5104-DASTODAT                                  
033500           PERFORM FA-READ-BASICDATA                                      
033600        ELSE                                                              
033700           MOVE ERR-INV-NOT-STARTED TO RESP-IDMSG-ERROR                   
033800           MOVE NOO                 TO INDATA-SW                          
033900        END-IF                                                            
034000     ELSE                                                                 
034100        MOVE ERR-INV-INFO-NOT-FOUND TO RESP-IDMSG-ERROR                   
034300        MOVE NOO                    TO INDATA-SW                          
034400     END-IF                                                               
034500     .                                                                    
034600     EJECT                                                                
034700 FA-READ-BASICDATA SECTION.                                               
034800                                                                          
034900     PERFORM FAA-CHECK-INPUT                                              
035000     IF INDATA-OK                                                         
035100        EVALUATE TRUE                                                     
035200           WHEN REQU-KDPGMACT = 'S' AND REQU-KDCMDVAL = 'A'               
035300             PERFORM FAB-PROCESS-SUMMARY                                  
035400                                                                          
035500           WHEN REQU-KDPGMACT = 'S' AND REQU-KDCMDVAL = 'B'               
035600             PERFORM FAC-PROCESS-PRINTED-PARTS                            
035700                                                                          
035800           WHEN REQU-KDPGMACT = 'S' AND REQU-KDCMDVAL = 'C'               
035900             PERFORM FAD-PROCESS-REMAINING-PARTS                          
036000                                                                          
036100           WHEN REQU-KDPGMACT = 'S' AND REQU-KDCMDVAL = 'D'               
036200             PERFORM FAE-VIEW-OPEN-LIST                                   
036300                                                                          
036400           WHEN REQU-KDPGMACT = 'P'                                       
036500            IF REQU-KDCMDVAL = 'D'                                        
036600              PERFORM FAF-PRINT-LIST                                      
036700            ELSE                                                          
036800              MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                  
036900              MOVE 'KDCMDVAL'        TO RESP-IDELMT-ERROR                 
037000              MOVE NOO               TO INDATA-SW                         
037100            END-IF                                                        
037200        END-EVALUATE                                                      
037300     END-IF                                                               
037400     .                                                                    
037500 FAA-CHECK-INPUT SECTION.                                                 
037600     MOVE YES                     TO INDATA-SW                            
037700                                                                          
037800     IF REQU-ADLAGOMR NOT = ALL '+'                                       
037900        INSPECT REQU-ADLAGOMR TALLYING WS-CNTR1                           
038000        FOR ALL '-' '+'                                                   
038100        IF WS-CNTR1 > 0                                                   
038200           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
038300           MOVE 'ADLAGOMR'        TO RESP-IDELMT-ERROR                    
038400           MOVE NOO               TO INDATA-SW                            
038500        END-IF                                                            
038600     END-IF                                                               
038700                                                                          
038800     IF INDATA-OK                                                         
038900       IF REQU-ADGANG-FOM NOT = ALL '+'                                   
039000          MOVE 0                    TO WS-CNTR1                           
039100          INSPECT REQU-ADGANG-FOM   TALLYING WS-CNTR1                     
039200          FOR ALL '-' '+'                                                 
039300          IF WS-CNTR1 > 0                                                 
039400             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
039500             MOVE 'ADGANG-FOM'      TO RESP-IDELMT-ERROR                  
039600             MOVE NOO               TO INDATA-SW                          
039700          END-IF                                                          
039800       END-IF                                                             
039900     END-IF                                                               
040000                                                                          
040100     IF INDATA-OK                                                         
040200       IF REQU-ADGANG-TOM NOT = ALL '+'                                   
040300          MOVE 0                    TO WS-CNTR1                           
040400          INSPECT REQU-ADGANG-TOM   TALLYING WS-CNTR1                     
040500          FOR ALL '-' '+'                                                 
040600          IF WS-CNTR1 > 0                                                 
040700             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
040800             MOVE 'ADGANG-TOM'      TO RESP-IDELMT-ERROR                  
040900             MOVE NOO               TO INDATA-SW                          
041000          END-IF                                                          
041100       END-IF                                                             
041200     END-IF                                                               
041300                                                                          
041400     IF INDATA-OK                                                         
041500       IF REQU-ADLAGOMR = ALL '+' AND                                     
041600          REQU-ADGANG-FOM NOT = ALL '+'                                   
041700          MOVE ERR-MUST-NOT-BE-ENTERED TO RESP-IDMSG-ERROR                
041800          MOVE 'ADGANG-FOM'            TO RESP-IDELMT-ERROR               
041900          MOVE NOO                     TO INDATA-SW                       
042000       END-IF                                                             
042100     END-IF                                                               
042200                                                                          
042300     IF INDATA-OK                                                         
042400       IF REQU-ADLAGOMR NOT = ALL '+'                                     
042500          MOVE REQU-ADLAGOMR        TO WS-IDFRIDATA                       
042600          PERFORM FAAA-TRANSFORM-NUMERIC-DATA                             
042700          IF DEC-KDSVAR-OK                                                
042800             CONTINUE                                                     
042900          ELSE                                                            
043000             MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                 
043100             MOVE 'ADLAGOMR'          TO RESP-IDELMT-ERROR                
043200             MOVE NOO                 TO INDATA-SW                        
043300          END-IF                                                          
043400       END-IF                                                             
043500     END-IF                                                               
043600                                                                          
043700     IF INDATA-OK                                                         
043800       EVALUATE TRUE                                                      
043900         WHEN REQU-ADGANG-FOM NOT = ALL '+'                               
044000         AND REQU-ADLAGOMR = 0                                            
044100           MOVE ERR-SHLD-NOT-BE-ZERO TO RESP-IDMSG-ERROR                  
044200           MOVE 'ADLAGOMR'           TO RESP-IDELMT-ERROR                 
044300           MOVE NOO                  TO INDATA-SW                         
044400         WHEN REQU-ADGANG-FOM NOT = ALL '+'                               
044500           MOVE REQU-ADGANG-FOM         TO WS-IDFRIDATA                   
044600           PERFORM FAAA-TRANSFORM-NUMERIC-DATA                            
044700           IF DEC-KDSVAR-OK                                               
044800              CONTINUE                                                    
044900           ELSE                                                           
045000              MOVE ERR-MUST-BE-NUMERIC  TO RESP-IDMSG-ERROR               
045100              MOVE 'ADGANG-FOM'         TO RESP-IDELMT-ERROR              
045200              MOVE NOO                  TO INDATA-SW                      
045300           END-IF                                                         
045400       END-EVALUATE                                                       
045500     END-IF                                                               
045600                                                                          
045700     IF INDATA-OK                                                         
045800       EVALUATE TRUE                                                      
045900         WHEN REQU-ADGANG-TOM NOT = ALL '+'                               
046000         AND REQU-ADLAGOMR = 0                                            
046100           MOVE ERR-SHLD-NOT-BE-ZERO TO RESP-IDMSG-ERROR                  
046200           MOVE 'ADLAGOMR'           TO RESP-IDELMT-ERROR                 
046300           MOVE NOO                  TO INDATA-SW                         
046400         WHEN REQU-ADGANG-TOM  NOT = ALL '+'                              
046500         AND REQU-ADLAGOMR > 0                                            
046600         AND REQU-ADGANG-FOM = 0                                          
046700           MOVE ERR-SHLD-NOT-BE-ZERO    TO RESP-IDMSG-ERROR               
046800           MOVE 'ADGANG-FOM'            TO RESP-IDELMT-ERROR              
046900           MOVE NOO                     TO INDATA-SW                      
047000         WHEN REQU-ADGANG-TOM  NOT = ALL '+'                              
047100           MOVE REQU-ADGANG-TOM         TO WS-IDFRIDATA                   
047200           PERFORM FAAA-TRANSFORM-NUMERIC-DATA                            
047300           IF DEC-KDSVAR-OK                                               
047400              CONTINUE                                                    
047500           ELSE                                                           
047600              MOVE ERR-MUST-BE-NUMERIC  TO RESP-IDMSG-ERROR               
047700              MOVE 'ADGANG-TOM'         TO RESP-IDELMT-ERROR              
047800              MOVE NOO                  TO INDATA-SW                      
047900           END-IF                                                         
048000         WHEN REQU-ADGANG-TOM  NOT = ALL '+'                              
048100         AND REQU-ADGANG-TOM < REQU-ADGANG-FOM                            
048200           MOVE ERR-INVALID-FIELD    TO RESP-IDMSG-ERROR                  
048300           MOVE 'ADGANG-TOM'         TO RESP-IDELMT-ERROR                 
048400           MOVE NOO                  TO INDATA-SW                         
048500       END-EVALUATE                                                       
048600     END-IF                                                               
048700                                                                          
048800     IF INDATA-OK                                                         
048900       IF REQU-KDCMDVAL = 'A' OR 'B' OR 'C' OR 'D'                        
049000          CONTINUE                                                        
049100       ELSE                                                               
049200          MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                     
049300          MOVE 'KDCMDVAL'         TO RESP-IDELMT-ERROR                    
049400          MOVE NOO                TO INDATA-SW                            
049500       END-IF                                                             
049600     END-IF                                                               
049700                                                                          
049800     IF INDATA-OK                                                         
049900       IF REQU-KDCMDVAL NOT = 'A'                                         
050000         IF REQU-IDPRTOMG = '1' OR '2' OR '3' OR '0'                      
050100           CONTINUE                                                       
050200         ELSE                                                             
050300           MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                    
050400           MOVE 'IDPRTOMG'         TO RESP-IDELMT-ERROR                   
050500           MOVE NOO                TO INDATA-SW                           
050600         END-IF                                                           
050700       END-IF                                                             
050800     END-IF                                                               
050900     .                                                                    
051000                                                                          
051100*-----------------------------------------------------------------        
051200* TRANSFORM THE NUMERIC VALUES RECEIVED FROM THE WEB                      
051300*-----------------------------------------------------------------        
051400 FAAA-TRANSFORM-NUMERIC-DATA SECTION.                                     
051500     MOVE WS-IDFRIDATA  TO DEC-IDFRIDATA                                  
051600     MOVE 2             TO DEC-KVHELTAL                                   
051700     MOVE 0             TO DEC-KVDECIMAL                                  
051800                                                                          
051900     CALL WDECEDIT USING DEC-WDECAREA                                     
052000     .                                                                    
052100 FAB-PROCESS-SUMMARY SECTION.                                             
052200     MOVE +1  TO RESP-KVRADER                                             
052300                                                                          
052400     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
052500       MOVE REQU-IDDC    TO W-WDJ701-IDDC-MIN                             
052600                            W-WDJ701-IDDC-MAX                             
052700       PERFORM IMS-GN-WDJ701                                              
052800       IF SEGMENT-FOUND                                                   
052900         IF ACS-FLKLAR = 'N'                                              
053000           IF (ACS-IDCOUNTER-P > SPACE                                    
053100           AND ACS-IDCOUNTER-P-REG = SPACE)                               
053200           OR (ACS-IDCOUNTER-R > SPACE                                    
053300           AND ACS-IDCOUNTER-R-REG = SPACE)                               
053400           OR (ACS-IDCOUNTER-T > SPACE                                    
053500           AND ACS-IDCOUNTER-T-REG = SPACE)                               
053600              ADD +1 TO WS-KVART-INVPRINT-LINE                            
053700           END-IF                                                         
053800           IF  ACS-IDCOUNTER-P = SPACE                                    
053900           OR  (ACS-IDCOUNTER-R  = SPACE                                  
054000                AND ACS-IDCOUNTER-P > SPACE                               
054100                AND ACS-IDCOUNTER-P-REG > SPACE)                          
054200           OR  (ACS-IDCOUNTER-T  = SPACE                                  
054300                AND ACS-IDCOUNTER-P > SPACE                               
054400                AND ACS-IDCOUNTER-P-REG > SPACE                           
054500                AND ACS-IDCOUNTER-R > SPACE                               
054600                AND ACS-IDCOUNTER-R-REG > SPACE)                          
054700             ADD +1 TO WS-KVART-INVLEFT-LINE                              
054800           END-IF                                                         
054900         END-IF                                                           
055000       END-IF                                                             
055100     END-PERFORM                                                          
055200                                                                          
055300     COMPUTE WS-KVART-INVTOT-LINE = WS-KVART-INVPRINT-LINE +              
055400                                    WS-KVART-INVLEFT-LINE                 
055500                                                                          
055600     MOVE WS-KVART-INVTOT-LINE   TO RESP-KVART-INVTOT-LINE(1)             
055700     MOVE WS-KVART-INVPRINT-LINE TO                                       
055800                                  RESP-KVART-INVPRINT-LINE(1)             
055900     MOVE WS-KVART-INVLEFT-LINE  TO RESP-KVART-INVLEFT-LINE(1)            
056000     .                                                                    
056100 FAC-PROCESS-PRINTED-PARTS SECTION.                                       
056200     EVALUATE TRUE                                                        
056300       WHEN REQU-ADLAGOMR = ALL '+'                                       
056400          PERFORM FACA-GET-PRINTED-PARTS-AREA0                            
056500                                                                          
056600       WHEN (REQU-ADLAGOMR NOT = ALL '+'                                  
056700       AND  (REQU-ADGANG-FOM NOT = ALL '+'                                
056800       AND   REQU-ADGANG-TOM NOT = ALL '+'))                              
056900          MOVE REQU-ADGANG-FOM TO W-WDJ7A1-ADGANG-MIN                     
057000          MOVE REQU-ADGANG-TOM TO W-WDJ7A1-ADGANG-MAX                     
057100          PERFORM FACB-GET-PRINTED-PARTS                                  
057200                                                                          
057300       WHEN (REQU-ADLAGOMR NOT = ALL '+'                                  
057400       AND  REQU-ADGANG-FOM NOT = ALL '+')                                
057500          MOVE REQU-ADGANG-FOM TO W-WDJ7A1-ADGANG-MIN                     
057600                                  W-WDJ7A1-ADGANG-MAX                     
057700          PERFORM FACB-GET-PRINTED-PARTS                                  
057800                                                                          
057900       WHEN REQU-ADLAGOMR NOT = ALL '+'                                   
058000          PERFORM FACB-GET-PRINTED-PARTS                                  
058100                                                                          
058200     END-EVALUATE                                                         
058300     .                                                                    
058400                                                                          
058500 FACA-GET-PRINTED-PARTS-AREA0 SECTION.                                    
058600     MOVE +1            TO RESP-KVRADER                                   
058700     MOVE  0            TO WS-KVART-INVPRINT-LINE                         
058800     MOVE REQU-IDDC     TO W-WDJ7A1-IDDC-MIN                              
058900                           W-WDJ7A1-IDDC-MAX                              
059000     MOVE  0            TO W-WDJ7A1-ADLAGOMR-MIN                          
059100     MOVE 99            TO W-WDJ7A1-ADLAGOMR-MAX                          
059200     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
059300         PERFORM IMS-GN-WDJ7A1                                            
059400         IF SEGMENT-FOUND                                                 
059500            IF ACS-FLKLAR = 'N'                                           
059600             EVALUATE TRUE                                                
059700              WHEN REQU-IDPRTOMG = '0'                                    
059800                IF (ACS-IDCOUNTER-P > SPACE                               
059900                AND ACS-IDCOUNTER-P-REG = SPACE)                          
060000                OR (ACS-IDCOUNTER-R > SPACE                               
060100                AND ACS-IDCOUNTER-R-REG = SPACE)                          
060200                OR (ACS-IDCOUNTER-T > SPACE                               
060300                AND ACS-IDCOUNTER-T-REG = SPACE)                          
060400                  ADD +1 TO WS-KVART-INVPRINT-LINE                        
060500                END-IF                                                    
060600              WHEN REQU-IDPRTOMG = '1'                                    
060700                IF ACS-IDCOUNTER-P > SPACE                                
060800                AND ACS-IDCOUNTER-P-REG = SPACE                           
060900                  ADD +1 TO WS-KVART-INVPRINT-LINE                        
061000                END-IF                                                    
061100              WHEN REQU-IDPRTOMG = '2'                                    
061200                IF ACS-IDCOUNTER-R > SPACE                                
061300                AND ACS-IDCOUNTER-R-REG = SPACE                           
061400                  ADD +1 TO WS-KVART-INVPRINT-LINE                        
061500                END-IF                                                    
061600              WHEN REQU-IDPRTOMG = '3'                                    
061700                IF ACS-IDCOUNTER-T > SPACE                                
061800                AND ACS-IDCOUNTER-T-REG = SPACE                           
061900                  ADD +1 TO WS-KVART-INVPRINT-LINE                        
062000                END-IF                                                    
062100             END-EVALUATE                                                 
062200            END-IF                                                        
062300         END-IF                                                           
062400     END-PERFORM                                                          
062500     MOVE SPACES                 TO RESP-ADLAGOMR-LINE(1)                 
062600                                    RESP-ADGANG-LINE(1)                   
062700     MOVE REQU-IDPRTOMG          TO RESP-IDPRTOMG-LINE(1)                 
062800     MOVE WS-KVART-INVPRINT-LINE TO RESP-KVART-INVPRINT-LINE(1)           
062900     IF WS-KVART-INVPRINT-LINE = 0                                        
063000        MOVE 0                      TO RESP-KVRADER                       
063100        MOVE ERR-INV-INFO-NOT-FOUND TO RESP-IDMSG-ERROR                   
063200     END-IF                                                               
063300     .                                                                    
063400                                                                          
063500 FACB-GET-PRINTED-PARTS SECTION.                                          
063600     MOVE  0            TO WS-KVART-INVPRINT-LINE                         
063700                           WS-KVRADER                                     
063800                           WS-SAVE-ADGANG                                 
063900     MOVE REQU-IDDC     TO W-WDJ7A1-IDDC-MIN                              
064000                           W-WDJ7A1-IDDC-MAX                              
064100     MOVE REQU-ADLAGOMR TO W-WDJ7A1-ADLAGOMR-MIN                          
064200                           W-WDJ7A1-ADLAGOMR-MAX                          
064300     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
064400             OR TBL-INDX = TBL-MAX                                        
064500       PERFORM IMS-GN-WDJ7A1                                              
064600       IF SEGMENT-FOUND                                                   
064700         IF ACS-FLKLAR = 'N'                                              
064800           IF ACS-ADGANG = 0 AND WS-KVART-INVPRINT-LINE = 0               
064900              MOVE -1   TO WS-SAVE-ADGANG                                 
065000           END-IF                                                         
065100           EVALUATE TRUE                                                  
065200             WHEN REQU-IDPRTOMG = '0'                                     
065300               IF (ACS-IDCOUNTER-P > SPACE                                
065400               AND ACS-IDCOUNTER-P-REG = SPACE)                           
065500               OR (ACS-IDCOUNTER-R > SPACE                                
065600               AND ACS-IDCOUNTER-R-REG = SPACE)                           
065700               OR (ACS-IDCOUNTER-T > SPACE                                
065800               AND ACS-IDCOUNTER-T-REG = SPACE)                           
065900                 PERFORM FACBA-MOVE-DATA                                  
066000               END-IF                                                     
066100             WHEN REQU-IDPRTOMG = '1'                                     
066200               IF ACS-IDCOUNTER-P > SPACE                                 
066300               AND ACS-IDCOUNTER-P-REG = SPACE                            
066400                 PERFORM FACBA-MOVE-DATA                                  
066500               END-IF                                                     
066600             WHEN REQU-IDPRTOMG = '2'                                     
066700               IF ACS-IDCOUNTER-R > SPACE                                 
066800               AND ACS-IDCOUNTER-R-REG = SPACE                            
066900                 PERFORM FACBA-MOVE-DATA                                  
067000               END-IF                                                     
067100             WHEN REQU-IDPRTOMG = '3'                                     
067200               IF ACS-IDCOUNTER-T > SPACE                                 
067300               AND ACS-IDCOUNTER-T-REG = SPACE                            
067400                 PERFORM FACBA-MOVE-DATA                                  
067500               END-IF                                                     
067600            END-EVALUATE                                                  
067700          END-IF                                                          
067800       END-IF                                                             
067900     END-PERFORM                                                          
068000     IF WS-KVART-INVPRINT-LINE = 0                                        
068100        MOVE 0                    TO WS-KVRADER                           
068200        MOVE ERR-INV-INFO-NOT-FOUND TO RESP-IDMSG-ERROR                   
068300     END-IF                                                               
068400     MOVE WS-KVRADER TO RESP-KVRADER                                      
068500     .                                                                    
068600 FACBA-MOVE-DATA SECTION.                                                 
068700                                                                          
068800      IF ACS-ADGANG = WS-SAVE-ADGANG                                      
068900        ADD +1             TO WS-KVART-INVPRINT-LINE                      
069000        MOVE ACS-ADLAGOMR  TO RESP-ADLAGOMR-LINE(TBL-INDX)                
069100        MOVE ACS-ADGANG    TO RESP-ADGANG-LINE(TBL-INDX)                  
069200        MOVE REQU-IDPRTOMG TO RESP-IDPRTOMG-LINE(TBL-INDX)                
069300        MOVE WS-KVART-INVPRINT-LINE                                       
069400                           TO RESP-KVART-INVPRINT-LINE(TBL-INDX)          
069500                                                                          
069600      ELSE                                                                
069700        MOVE ACS-ADGANG    TO WS-SAVE-ADGANG                              
069800        ADD +1             TO WS-KVRADER                                  
069900        MOVE +1            TO WS-KVART-INVPRINT-LINE                      
070000        ADD +1             TO TBL-INDX                                    
070100        MOVE ACS-ADLAGOMR  TO RESP-ADLAGOMR-LINE(TBL-INDX)                
070200        MOVE ACS-ADGANG    TO RESP-ADGANG-LINE(TBL-INDX)                  
070300        MOVE REQU-IDPRTOMG TO RESP-IDPRTOMG-LINE(TBL-INDX)                
070400        MOVE WS-KVART-INVPRINT-LINE                                       
070500                           TO RESP-KVART-INVPRINT-LINE(TBL-INDX)          
070600      END-IF                                                              
070700      .                                                                   
070800 FAD-PROCESS-REMAINING-PARTS SECTION.                                     
070900     EVALUATE TRUE                                                        
071000       WHEN REQU-ADLAGOMR = ALL '+'                                       
071100          PERFORM FADA-GET-REMAIN-PARTS-AREA0                             
071200                                                                          
071300       WHEN (REQU-ADLAGOMR NOT = ALL '+'                                  
071400       AND  (REQU-ADGANG-FOM NOT = ALL '+'                                
071500       AND   REQU-ADGANG-TOM NOT = ALL '+'))                              
071600          MOVE REQU-ADGANG-FOM TO W-WDJ7A1-ADGANG-MIN                     
071700          MOVE REQU-ADGANG-TOM TO W-WDJ7A1-ADGANG-MAX                     
071800          PERFORM FADB-GET-REMAIN-PARTS                                   
071900                                                                          
072000       WHEN (REQU-ADLAGOMR NOT = ALL '+'                                  
072100       AND  REQU-ADGANG-FOM NOT = ALL '+')                                
072200          MOVE REQU-ADGANG-FOM TO W-WDJ7A1-ADGANG-MIN                     
072300                                  W-WDJ7A1-ADGANG-MAX                     
072400          PERFORM FADB-GET-REMAIN-PARTS                                   
072500                                                                          
072600       WHEN REQU-ADLAGOMR NOT = ALL '+'                                   
072700          PERFORM FADB-GET-REMAIN-PARTS                                   
072800                                                                          
072900     END-EVALUATE                                                         
073000     .                                                                    
073100                                                                          
073200 FADA-GET-REMAIN-PARTS-AREA0 SECTION.                                     
073300     MOVE +1            TO RESP-KVRADER                                   
073400     MOVE  0            TO WS-KVART-INVLEFT-LINE                          
073500     MOVE REQU-IDDC     TO W-WDJ7A1-IDDC-MIN                              
073600                           W-WDJ7A1-IDDC-MAX                              
073700     MOVE 0             TO W-WDJ7A1-ADLAGOMR-MIN                          
073800     MOVE 99            TO W-WDJ7A1-ADLAGOMR-MAX                          
073900     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
074000       PERFORM IMS-GN-WDJ7A1                                              
074100       IF SEGMENT-FOUND                                                   
074200         IF ACS-FLKLAR = 'N'                                              
074300           EVALUATE TRUE                                                  
074400             WHEN REQU-IDPRTOMG = '0'                                     
074500               IF ACS-IDCOUNTER-P = SPACE                                 
074600               OR (ACS-IDCOUNTER-R = SPACE                                
074700                   AND ACS-IDCOUNTER-P > SPACE                            
074800                   AND ACS-IDCOUNTER-P-REG > SPACE)                       
074900               OR (ACS-IDCOUNTER-T = SPACE                                
075000                   AND ACS-IDCOUNTER-P > SPACE                            
075100                   AND ACS-IDCOUNTER-P-REG > SPACE                        
075200                   AND ACS-IDCOUNTER-R > SPACE                            
075300                   AND ACS-IDCOUNTER-R-REG > SPACE)                       
075400                  ADD +1 TO WS-KVART-INVLEFT-LINE                         
075500               END-IF                                                     
075600             WHEN REQU-IDPRTOMG = '1'                                     
075700               IF ACS-IDCOUNTER-P = SPACE                                 
075800                  ADD +1 TO WS-KVART-INVLEFT-LINE                         
075900               END-IF                                                     
076000             WHEN REQU-IDPRTOMG = '2'                                     
076100               IF ACS-IDCOUNTER-R = SPACE                                 
076200               AND ACS-IDCOUNTER-P > SPACE                                
076300               AND ACS-IDCOUNTER-P-REG > SPACE                            
076400                  ADD +1 TO WS-KVART-INVLEFT-LINE                         
076500               END-IF                                                     
076600             WHEN REQU-IDPRTOMG = '3'                                     
076700               IF ACS-IDCOUNTER-T = SPACE                                 
076800               AND ACS-IDCOUNTER-P > SPACE                                
076900               AND ACS-IDCOUNTER-P-REG > SPACE                            
077000               AND ACS-IDCOUNTER-R > SPACE                                
077100                AND ACS-IDCOUNTER-R-REG > SPACE                           
077200                 ADD +1 TO WS-KVART-INVLEFT-LINE                          
077300               END-IF                                                     
077400           END-EVALUATE                                                   
077500         END-IF                                                           
077600       END-IF                                                             
077700     END-PERFORM                                                          
077800     MOVE SPACES                 TO RESP-ADLAGOMR-LINE(1)                 
077900                                    RESP-ADGANG-LINE(1)                   
078000     MOVE REQU-IDPRTOMG          TO RESP-IDPRTOMG-LINE(1)                 
078100     MOVE WS-KVART-INVLEFT-LINE  TO RESP-KVART-INVLEFT-LINE(1)            
078200     IF WS-KVART-INVLEFT-LINE = 0                                         
078300        MOVE 0                      TO RESP-KVRADER                       
078400        MOVE ERR-INV-INFO-NOT-FOUND TO RESP-IDMSG-ERROR                   
078500     END-IF                                                               
078600     .                                                                    
078700                                                                          
078800 FADB-GET-REMAIN-PARTS SECTION.                                           
078900     MOVE  0            TO WS-KVART-INVLEFT-LINE                          
079000                           WS-KVRADER                                     
079100                           WS-SAVE-ADGANG                                 
079200     MOVE REQU-IDDC     TO W-WDJ7A1-IDDC-MIN                              
079300                           W-WDJ7A1-IDDC-MAX                              
079400     MOVE REQU-ADLAGOMR TO W-WDJ7A1-ADLAGOMR-MIN                          
079500                           W-WDJ7A1-ADLAGOMR-MAX                          
079600     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
079700             OR TBL-INDX = TBL-MAX                                        
079800       PERFORM IMS-GN-WDJ7A1                                              
079900         IF SEGMENT-FOUND                                                 
080000           IF ACS-FLKLAR = 'N'                                            
080100             IF ACS-ADGANG = 0 AND WS-KVART-INVLEFT-LINE = 0              
080200                MOVE -1   TO WS-SAVE-ADGANG                               
080300             END-IF                                                       
080400             EVALUATE TRUE                                                
080500              WHEN REQU-IDPRTOMG = '0'                                    
080600               IF ACS-IDCOUNTER-P = SPACE                                 
080700               OR (ACS-IDCOUNTER-R = SPACE                                
080800                   AND ACS-IDCOUNTER-P > SPACE                            
080900                   AND ACS-IDCOUNTER-P-REG > SPACE)                       
081000               OR (ACS-IDCOUNTER-T = SPACE                                
081100                   AND ACS-IDCOUNTER-P > SPACE                            
081200                   AND ACS-IDCOUNTER-P-REG > SPACE                        
081300                   AND ACS-IDCOUNTER-R > SPACE                            
081400                   AND ACS-IDCOUNTER-R-REG > SPACE)                       
081500                  PERFORM FADBA-MOVE-DATA                                 
081600               END-IF                                                     
081700              WHEN REQU-IDPRTOMG = '1'                                    
081800                IF ACS-IDCOUNTER-P = SPACE                                
081900                  PERFORM FADBA-MOVE-DATA                                 
082000                END-IF                                                    
082100              WHEN REQU-IDPRTOMG = '2'                                    
082200                IF ACS-IDCOUNTER-R = SPACE                                
082300                AND ACS-IDCOUNTER-P > SPACE                               
082400                AND ACS-IDCOUNTER-P-REG > SPACE                           
082500                  PERFORM FADBA-MOVE-DATA                                 
082600                END-IF                                                    
082700              WHEN REQU-IDPRTOMG = '3'                                    
082800               IF ACS-IDCOUNTER-T = SPACE                                 
082900               AND ACS-IDCOUNTER-P > SPACE                                
083000               AND ACS-IDCOUNTER-P-REG > SPACE                            
083100               AND ACS-IDCOUNTER-R > SPACE                                
083200               AND ACS-IDCOUNTER-R-REG > SPACE                            
083300                  PERFORM FADBA-MOVE-DATA                                 
083400               END-IF                                                     
083500            END-EVALUATE                                                  
083600          END-IF                                                          
083700       END-IF                                                             
083800     END-PERFORM                                                          
083900     IF WS-KVART-INVLEFT-LINE = 0                                         
084000        MOVE 0                    TO WS-KVRADER                           
084100        MOVE ERR-INV-INFO-NOT-FOUND TO RESP-IDMSG-ERROR                   
084200     END-IF                                                               
084300     MOVE WS-KVRADER TO RESP-KVRADER                                      
084400     .                                                                    
084500 FADBA-MOVE-DATA SECTION.                                                 
084600     IF ACS-ADGANG = WS-SAVE-ADGANG                                       
084700        ADD +1             TO WS-KVART-INVLEFT-LINE                       
084800        MOVE ACS-ADLAGOMR  TO RESP-ADLAGOMR-LINE(TBL-INDX)                
084900        MOVE ACS-ADGANG    TO RESP-ADGANG-LINE(TBL-INDX)                  
085000        MOVE REQU-IDPRTOMG TO RESP-IDPRTOMG-LINE(TBL-INDX)                
085100        MOVE WS-KVART-INVLEFT-LINE                                        
085200                           TO RESP-KVART-INVLEFT-LINE(TBL-INDX)           
085300                                                                          
085400     ELSE                                                                 
085500        MOVE ACS-ADGANG    TO WS-SAVE-ADGANG                              
085600        ADD +1             TO WS-KVRADER                                  
085700        MOVE +1            TO WS-KVART-INVLEFT-LINE                       
085800        ADD +1             TO TBL-INDX                                    
085900        MOVE ACS-ADLAGOMR  TO RESP-ADLAGOMR-LINE(TBL-INDX)                
086000        MOVE ACS-ADGANG    TO RESP-ADGANG-LINE(TBL-INDX)                  
086100        MOVE REQU-IDPRTOMG TO RESP-IDPRTOMG-LINE(TBL-INDX)                
086200        MOVE WS-KVART-INVLEFT-LINE                                        
086300                           TO RESP-KVART-INVLEFT-LINE(TBL-INDX)           
086400     END-IF                                                               
086500     .                                                                    
086600 FAE-VIEW-OPEN-LIST SECTION.                                              
086700     MOVE 0 TO WS-KVRADER                                                 
086800               TBL-INDX                                                   
086900               WS-SAVE-IDACSNR-P                                          
087000               WS-SAVE-IDACSNR-R                                          
087100               WS-SAVE-IDACSNR-T                                          
087200               WS-SAVE-ADLAGOMR                                           
087300                                                                          
087400     MOVE REQU-IDDC TO W-IDDC-D1-MIN                                      
087500                       W-IDDC-D1-MAX                                      
087600                       W-IDDC-E1-MIN                                      
087700                       W-IDDC-E1-MAX                                      
087800                       W-IDDC-F1-MIN                                      
087900                       W-IDDC-F1-MAX                                      
088000     EVALUATE TRUE                                                        
088100       WHEN REQU-IDPRTOMG = '0'                                           
088200         PERFORM FAEA-CHECK-ROUND-1                                       
088300         UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                          
088310         OR TBL-INDX = TBL-MAX                                            
088400         MOVE SPACES TO STATUS-WS                                         
088500                                                                          
088600         PERFORM FAEB-CHECK-ROUND-2                                       
088700         UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                          
088800         OR TBL-INDX = TBL-MAX                                            
088900         MOVE SPACES TO STATUS-WS                                         
088910                                                                          
089000         PERFORM FAEC-CHECK-ROUND-3                                       
089100         UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                          
089110         OR TBL-INDX = TBL-MAX                                            
089200       WHEN REQU-IDPRTOMG = '1'                                           
089300         PERFORM FAEA-CHECK-ROUND-1                                       
089400         UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                          
089410         OR TBL-INDX = TBL-MAX                                            
089500       WHEN REQU-IDPRTOMG = '2'                                           
089600         PERFORM FAEB-CHECK-ROUND-2                                       
089700         UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                          
089710         OR TBL-INDX = TBL-MAX                                            
089800       WHEN REQU-IDPRTOMG = '3'                                           
089900         PERFORM FAEC-CHECK-ROUND-3                                       
090000         UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                          
090010         OR TBL-INDX = TBL-MAX                                            
090100     END-EVALUATE                                                         
090200     MOVE WS-KVRADER         TO RESP-KVRADER                              
090300     IF WS-KVRADER = 0                                                    
090400        MOVE ERR-INV-INFO-NOT-FOUND TO RESP-IDMSG-ERROR                   
090600     END-IF                                                               
090700     .                                                                    
090800 FAEA-CHECK-ROUND-1 SECTION.                                              
090900     PERFORM IMS-GN-WDJ701-DSEQ                                           
091000     IF SEGMENT-FOUND                                                     
091100       IF ACS-IDCOUNTER-P-REG = SPACE                                     
091200        EVALUATE TRUE                                                     
091300         WHEN ACS-IDACSNR-P = WS-SAVE-IDACSNR-P                           
091400         AND ACS-ADLAGOMR = WS-SAVE-ADLAGOMR                              
091500           MOVE NOO             TO WS-PRINT-SW                            
091600                                                                          
091700         WHEN OTHER                                                       
091800           SET WS-PRINT         TO TRUE                                   
091900           MOVE ACS-IDACSNR-P   TO WS-SAVE-IDACSNR-P                      
092000           MOVE ACS-ADLAGOMR    TO WS-SAVE-ADLAGOMR                       
092100           ADD +1               TO WS-KVRADER                             
092200           ADD +1               TO TBL-INDX                               
092300           MOVE ACS-IDACSNR-P   TO                                        
092400                             RESP-IDACSNR-LINE(TBL-INDX)                  
092500           MOVE ACS-IDCOUNTER-P TO                                        
092600                             RESP-IDCOUNTER-LINE(TBL-INDX)                
092700           MOVE ACS-ADLAGOMR    TO                                        
092800                             RESP-ADLAGOMR-LINE(TBL-INDX)                 
092900           MOVE REQU-IDPRTOMG   TO                                        
093000                             RESP-IDPRTOMG-LINE(TBL-INDX)                 
093100        END-EVALUATE                                                      
093200       ELSE                                                               
093300        MOVE NOO                TO WS-PRINT-SW                            
093400       END-IF                                                             
093500     END-IF                                                               
093600     .                                                                    
093700                                                                          
093800 FAEB-CHECK-ROUND-2 SECTION.                                              
093900     PERFORM IMS-GN-WDJ701-ESEQ                                           
094000     IF SEGMENT-FOUND                                                     
094100       IF ACS-IDCOUNTER-R-REG = SPACE                                     
094200        EVALUATE TRUE                                                     
094300         WHEN ACS-IDACSNR-R = WS-SAVE-IDACSNR-R                           
094400         AND ACS-ADLAGOMR = WS-SAVE-ADLAGOMR                              
094500           MOVE NOO             TO WS-PRINT-SW                            
094600                                                                          
094700         WHEN OTHER                                                       
094800           SET WS-PRINT     TO TRUE                                       
094900           MOVE ACS-IDACSNR-R   TO WS-SAVE-IDACSNR-R                      
095000           MOVE ACS-ADLAGOMR    TO WS-SAVE-ADLAGOMR                       
095100           ADD +1               TO WS-KVRADER                             
095200           ADD +1               TO TBL-INDX                               
095300           MOVE ACS-IDACSNR-R   TO                                        
095400                             RESP-IDACSNR-LINE(TBL-INDX)                  
095500           MOVE ACS-IDCOUNTER-R TO                                        
095600                             RESP-IDCOUNTER-LINE(TBL-INDX)                
095700           MOVE ACS-ADLAGOMR    TO                                        
095800                             RESP-ADLAGOMR-LINE(TBL-INDX)                 
095900           MOVE REQU-IDPRTOMG   TO                                        
096000                             RESP-IDPRTOMG-LINE(TBL-INDX)                 
096100        END-EVALUATE                                                      
096200       ELSE                                                               
096300        MOVE NOO                TO WS-PRINT-SW                            
096400       END-IF                                                             
096500     END-IF                                                               
096600     .                                                                    
096700 FAEC-CHECK-ROUND-3 SECTION.                                              
096800     PERFORM IMS-GN-WDJ701-FSEQ                                           
096900     IF SEGMENT-FOUND                                                     
097000       IF ACS-IDCOUNTER-T-REG = SPACE                                     
097100         EVALUATE TRUE                                                    
097200          WHEN ACS-IDACSNR-T = WS-SAVE-IDACSNR-T                          
097300          AND ACS-ADLAGOMR = WS-SAVE-ADLAGOMR                             
097400            MOVE NOO             TO WS-PRINT-SW                           
097500                                                                          
097600          WHEN OTHER                                                      
097700            SET WS-PRINT         TO TRUE                                  
097800            MOVE ACS-IDACSNR-T   TO WS-SAVE-IDACSNR-T                     
097900            MOVE ACS-ADLAGOMR    TO WS-SAVE-ADLAGOMR                      
098000            ADD +1               TO WS-KVRADER                            
098100            ADD +1               TO TBL-INDX                              
098200            MOVE ACS-IDACSNR-T   TO                                       
098300                              RESP-IDACSNR-LINE(TBL-INDX)                 
098400            MOVE ACS-IDCOUNTER-T TO                                       
098500                              RESP-IDCOUNTER-LINE(TBL-INDX)               
098600            MOVE ACS-ADLAGOMR    TO                                       
098700                              RESP-ADLAGOMR-LINE(TBL-INDX)                
098800            MOVE REQU-IDPRTOMG   TO                                       
098900                              RESP-IDPRTOMG-LINE(TBL-INDX)                
099000         END-EVALUATE                                                     
099100       ELSE                                                               
099200         MOVE NOO                TO WS-PRINT-SW                           
099300       END-IF                                                             
099400     END-IF                                                               
099500     .                                                                    
099600 FAF-PRINT-LIST SECTION.                                                  
099700                                                                          
099800     MOVE 0 TO WS-KVRADER                                                 
099900               TBL-INDX                                                   
100000               WS-SAVE-IDACSNR-P                                          
100100               WS-SAVE-IDACSNR-R                                          
100200               WS-SAVE-IDACSNR-T                                          
100300               WS-SAVE-ADLAGOMR                                           
100400                                                                          
100500     MOVE REQU-IDDC TO W-IDDC-D1-MIN                                      
100600                       W-IDDC-D1-MAX                                      
100700                       W-IDDC-E1-MIN                                      
100800                       W-IDDC-E1-MAX                                      
100900                       W-IDDC-F1-MIN                                      
101000                       W-IDDC-F1-MAX                                      
101100     PERFORM FAFA-CREATE-HEADER                                           
101200                                                                          
101300     EVALUATE TRUE                                                        
101400       WHEN REQU-IDPRTOMG = '0'                                           
101500         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                  
101600           PERFORM FAEA-CHECK-ROUND-1                                     
101700           IF SEGMENT-FOUND AND WS-PRINT                                  
101800             MOVE '2         '    TO DP-LINE-IDAFPRCD                     
101900             MOVE ACS-IDACSNR-P   TO DP-LINE-IDACSNR                      
102000             MOVE ACS-IDCOUNTER-P TO DP-LINE-IDCOUNTER                    
102100             MOVE ACS-ADLAGOMR    TO DP-LINE-ADLAGOMR                     
102200             MOVE REQU-IDPRTOMG   TO DP-LINE-IDPRTOMG                     
102300             PERFORM S05-PUT-REPORT-LINE                                  
102400           END-IF                                                         
102500         END-PERFORM                                                      
102600         MOVE SPACES              TO STATUS-WS                            
102700         MOVE NOO                 TO WS-PRINT-SW                          
102800         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                  
102900           PERFORM FAEB-CHECK-ROUND-2                                     
103000           IF SEGMENT-FOUND AND WS-PRINT                                  
103100             MOVE '2         '    TO DP-LINE-IDAFPRCD                     
103200             MOVE ACS-IDACSNR-R   TO DP-LINE-IDACSNR                      
103300             MOVE ACS-IDCOUNTER-R TO DP-LINE-IDCOUNTER                    
103400             MOVE ACS-ADLAGOMR    TO DP-LINE-ADLAGOMR                     
103500             MOVE REQU-IDPRTOMG   TO DP-LINE-IDPRTOMG                     
103600             PERFORM S05-PUT-REPORT-LINE                                  
103700           END-IF                                                         
103800         END-PERFORM                                                      
103900         MOVE SPACES              TO STATUS-WS                            
104000         MOVE NOO                 TO WS-PRINT-SW                          
104100         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                  
104200           PERFORM FAEC-CHECK-ROUND-3                                     
104300           IF SEGMENT-FOUND AND WS-PRINT                                  
104400             MOVE '2         '    TO DP-LINE-IDAFPRCD                     
104500             MOVE ACS-IDACSNR-T   TO DP-LINE-IDACSNR                      
104600             MOVE ACS-IDCOUNTER-T TO DP-LINE-IDCOUNTER                    
104700             MOVE ACS-ADLAGOMR    TO DP-LINE-ADLAGOMR                     
104800             MOVE REQU-IDPRTOMG   TO DP-LINE-IDPRTOMG                     
104900             PERFORM S05-PUT-REPORT-LINE                                  
105000           END-IF                                                         
105100         END-PERFORM                                                      
105200       WHEN REQU-IDPRTOMG = '1'                                           
105300         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                  
105400           PERFORM FAEA-CHECK-ROUND-1                                     
105500           IF SEGMENT-FOUND AND WS-PRINT                                  
105600             MOVE '2         '    TO DP-LINE-IDAFPRCD                     
105700             MOVE ACS-IDACSNR-P   TO DP-LINE-IDACSNR                      
105800             MOVE ACS-IDCOUNTER-P TO DP-LINE-IDCOUNTER                    
105900             MOVE ACS-ADLAGOMR    TO DP-LINE-ADLAGOMR                     
106000             MOVE REQU-IDPRTOMG   TO DP-LINE-IDPRTOMG                     
106100             PERFORM S05-PUT-REPORT-LINE                                  
106200           END-IF                                                         
106300         END-PERFORM                                                      
106400       WHEN REQU-IDPRTOMG = '2'                                           
106500         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                  
106600           PERFORM FAEB-CHECK-ROUND-2                                     
106700           IF SEGMENT-FOUND AND WS-PRINT                                  
106800             MOVE '2         '    TO DP-LINE-IDAFPRCD                     
106900             MOVE ACS-IDACSNR-R   TO DP-LINE-IDACSNR                      
107000             MOVE ACS-IDCOUNTER-R TO DP-LINE-IDCOUNTER                    
107100             MOVE ACS-ADLAGOMR    TO DP-LINE-ADLAGOMR                     
107200             MOVE REQU-IDPRTOMG   TO DP-LINE-IDPRTOMG                     
107300             PERFORM S05-PUT-REPORT-LINE                                  
107400           END-IF                                                         
107500         END-PERFORM                                                      
107600       WHEN REQU-IDPRTOMG = '3'                                           
107700         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                  
107800           PERFORM FAEC-CHECK-ROUND-3                                     
107900           IF SEGMENT-FOUND AND WS-PRINT                                  
108000             MOVE '2         '    TO DP-LINE-IDAFPRCD                     
108100             MOVE ACS-IDACSNR-T   TO DP-LINE-IDACSNR                      
108200             MOVE ACS-IDCOUNTER-T TO DP-LINE-IDCOUNTER                    
108300             MOVE ACS-ADLAGOMR    TO DP-LINE-ADLAGOMR                     
108400             MOVE REQU-IDPRTOMG   TO DP-LINE-IDPRTOMG                     
108500             PERFORM S05-PUT-REPORT-LINE                                  
108600           END-IF                                                         
108700         END-PERFORM                                                      
108800     END-EVALUATE                                                         
108900     PERFORM S04-SEND-CLOSE                                               
109000                                                                          
109100     MOVE WS-KVRADER          TO RESP-KVRADER                             
109200     MOVE INF-PROCESS-STARTED TO RESP-IDMSG-INFO                          
109300     .                                                                    
109400                                                                          
109500 FAFA-CREATE-HEADER SECTION.                                              
109600     MOVE 1                          TO HDR-REQU-IDMSGVER                 
109700     MOVE 'P'                        TO HDR-REQU-KDPGMACT                 
109800     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
109900                                                                          
110000     MOVE SPACE                      TO HDR-IDOUTREC                      
110100     MOVE REQU-IDDC                  TO HDR-IDOUTREC                      
110200                                        DP-HEAD-IDDC                      
110300     MOVE 'W50293-001'               TO HDR-IDOUTTYPE                     
110400     MOVE WS-CURRENT-DATE            TO HDR-IDLIST                        
110500     MOVE '1         '               TO DP-HEAD-IDAFPRCD                  
110600                                                                          
110700                                                                          
110800     PERFORM S04-SEND-OPEN                                                
110900     MOVE SEND-IDCOM                 TO WZ04-SEND-IDCOM                   
111000*HDR                                                                      
111100     PERFORM S05-PUT-DP-HEADER                                            
111200     PERFORM S05-PUT-HEADER                                               
111300     .                                                                    
111400     EJECT                                                                
111500*    --- DISPATCHER SECTIONS                                              
111600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
111700                                                                          
111800     MOVE 'GETARG'               TO SUB-KDFUNC                            
111900     MOVE WS-ADDRESS             TO SUB-ADDISPABS                         
112000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
112100                                                                          
112200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
112300                                                                          
112400     IF SUB-KDRC > 0                                                      
112500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
112600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
112700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
112800       DISPLAY ERROR-TEXT                                                 
112900       CALL FELLOG                                                        
113000     END-IF                                                               
113100     .                                                                    
113200 S02-RETURN-RESPONSE SECTION.                                             
113300                                                                          
113400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
113500     MOVE RESP-KVRADER               TO WS-KVRADER-NUM                    
113600     COMPUTE WS-RESP-AREA-LENGTH =                                        
113700       LENGTH OF RESP-WZ01RESP + LENGTH OF RESP-W50293O1 -                
113800       ((100 - WS-KVRADER-NUM) * LENGTH OF RESP-TABELLRAD)                
113900     MOVE WS-RESP-AREA-LENGTH        TO SUB-KVDLEN                        
114000                                                                          
114100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
114200                                                                          
114300     IF SUB-KDRC > 0                                                      
114400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
114500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
114600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
114700       DISPLAY ERROR-TEXT                                                 
114800       CALL FELLOG                                                        
114900     END-IF                                                               
115000     .                                                                    
115100     EJECT                                                                
115200 S04-SEND-OPEN SECTION.                                                   
115300                                                                          
115400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
115500     MOVE 'CARPARTS.DAP.DISTRDOC'    TO SEND-ADDISPABS                    
115600     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
115700                                                                          
115800     IF SEND-KDRC > 0                                                     
115900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
116000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
116100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
116200       DISPLAY ERROR-TEXT                                                 
116300       CALL FELLOG                                                        
116400     END-IF                                                               
116500     .                                                                    
116600 S04-SEND-CLOSE SECTION.                                                  
116700                                                                          
116800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
116900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
117000                                                                          
117100     IF SEND-KDRC > 0                                                     
117200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
117300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
117400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
117500       DISPLAY ERROR-TEXT                                                 
117600       CALL FELLOG                                                        
117700     END-IF                                                               
117800     .                                                                    
117900 S05-PUT-DP-HEADER SECTION.                                               
118000                                                                          
118100     MOVE 'PUT'                           TO SEND-KDFUNC                  
118200     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
118300     MOVE LENGTH OF DP-HDR-AREA           TO SEND-KVDLEN                  
118400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
118500                         SEND-KVDLEN                                      
118600                         DP-HDR-AREA                                      
118700     IF SEND-KDRC > ZERO                                                  
118800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
118900       STRING 'WZ01SEND PUT-DP-HDR ERROR RC=' KDRC-DISPLAY                
119000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
119100       DISPLAY ERROR-TEXT                                                 
119200       CALL FELLOG                                                        
119300     END-IF                                                               
119400     .                                                                    
119500     EJECT                                                                
119600 S05-PUT-HEADER SECTION.                                                  
119700                                                                          
119800     MOVE 'PUT'                           TO SEND-KDFUNC                  
119900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
120000     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
120100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
120200                         SEND-KVDLEN                                      
120300                         HDR-AREA                                         
120400     IF SEND-KDRC > ZERO                                                  
120500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
120600       STRING 'WZ01SEND PUT-HDR ERROR RC=' KDRC-DISPLAY                   
120700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
120800       DISPLAY ERROR-TEXT                                                 
120900       CALL FELLOG                                                        
121000     END-IF                                                               
121100     .                                                                    
121200     EJECT                                                                
121300 S05-PUT-REPORT-LINE    SECTION.                                          
121400                                                                          
121500     MOVE 'PUT'                           TO SEND-KDFUNC                  
121600     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
121700     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
121800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
121900                         SEND-KVDLEN                                      
122000                         DOC-LINE-AREA                                    
122100     IF SEND-KDRC > ZERO                                                  
122200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
122300       STRING 'WZ01SEND PUT-LINE ERROR RC=' KDRC-DISPLAY                  
122400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
122500       DISPLAY ERROR-TEXT                                                 
122600       CALL FELLOG                                                        
122700     END-IF                                                               
122800     .                                                                    
122900 IMS-GN-WDJ701 SECTION.                                                   
123000                                                                          
123100     STRING 'WDJ701  (WDJ701KY>=' W-WDJ701-MIN-X                          
123200                    '&WDJ701KY<=' W-WDJ701-MAX-X  ')'                     
123300          DELIMITED BY SIZE INTO SSA1                                     
123400     MOVE '  GE' TO GOOD-STATUSCODES                                      
123500     CALL CBLTDLI USING GN WDJ7-PCB DLI-IO-WDJ701 SSA1                    
123600     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
123700     PERFORM IMS-STATUSCHECK                                              
123800     .                                                                    
123900 IMS-GN-WDJ7A1 SECTION.                                                   
124000                                                                          
124100     STRING 'WDJ701  (WDJ7ASEQ>=' W-WDJ7ASEQ-MIN-X                        
124200                    '&WDJ7ASEQ<=' W-WDJ7ASEQ-MAX-X  ')'                   
124300          DELIMITED BY SIZE INTO SSA1                                     
124400     MOVE '  GE' TO GOOD-STATUSCODES                                      
124500     CALL CBLTDLI USING GN WDJ7A-PCB DLI-IO-WDJ701 SSA1                   
124600     MOVE WDJ7A-STATUS-CODE TO STATUS-WS                                  
124700     PERFORM IMS-STATUSCHECK                                              
124800     .                                                                    
124900     EJECT                                                                
125000 IMS-GN-WDJ701-DSEQ SECTION.                                              
125100                                                                          
125200     STRING 'WDJ701  (WDJ7DSEQ>=' W-WDJ7D1SEQ-MIN-X                       
125300                    '&WDJ7DSEQ<=' W-WDJ7D1SEQ-MAX-X  ')'                  
125400          DELIMITED BY SIZE INTO SSA1                                     
125500     MOVE '  GE' TO GOOD-STATUSCODES                                      
125600     CALL CBLTDLI USING GN WDJ7D-PCB DLI-IO-WDJ701 SSA1                   
125700     MOVE WDJ7D-STATUS-CODE TO STATUS-WS                                  
125800     PERFORM IMS-STATUSCHECK                                              
125900     .                                                                    
126000 IMS-GN-WDJ701-ESEQ SECTION.                                              
126100                                                                          
126200     STRING 'WDJ701  (WDJ7ESEQ>=' W-WDJ7E1SEQ-MIN-X                       
126300                    '&WDJ7ESEQ<=' W-WDJ7E1SEQ-MAX-X  ')'                  
126400          DELIMITED BY SIZE INTO SSA1                                     
126500     MOVE '  GE' TO GOOD-STATUSCODES                                      
126600     CALL CBLTDLI USING GN WDJ7E-PCB DLI-IO-WDJ701 SSA1                   
126700     MOVE WDJ7E-STATUS-CODE TO STATUS-WS                                  
126800     PERFORM IMS-STATUSCHECK                                              
126900     .                                                                    
127000 IMS-GN-WDJ701-FSEQ SECTION.                                              
127100                                                                          
127200     STRING 'WDJ701  (WDJ7FSEQ>=' W-WDJ7F1SEQ-MIN-X                       
127300                    '&WDJ7FSEQ<=' W-WDJ7F1SEQ-MAX-X  ')'                  
127400          DELIMITED BY SIZE INTO SSA1                                     
127500     MOVE '  GE' TO GOOD-STATUSCODES                                      
127600     CALL CBLTDLI USING GN WDJ7F-PCB DLI-IO-WDJ701 SSA1                   
127700     MOVE WDJ7F-STATUS-CODE TO STATUS-WS                                  
127800     PERFORM IMS-STATUSCHECK                                              
127900     .                                                                    
128000 IMS-GU-WDGX5104 SECTION.                                                 
128100                                                                          
128200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-5104-X ')'                    
128300          DELIMITED BY SIZE INTO SSA1                                     
128400     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
128500          DELIMITED BY SIZE INTO SSA2                                     
128600     MOVE '  GE' TO GOOD-STATUSCODES                                      
128700     CALL CBLTDLI USING GU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2             
128800     MOVE 5104-STATUS-CODE TO STATUS-WS                                   
128900     PERFORM IMS-STATUSCHECK                                              
129000     .                                                                    
129100     EJECT                                                                
129200 IMS-GU-WDB601 SECTION.                                                   
129300                                                                          
129400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
129500          DELIMITED BY SIZE INTO SSA1                                     
129600     MOVE '  GE' TO GOOD-STATUSCODES                                      
129700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
129800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
129900     PERFORM IMS-STATUSCHECK                                              
130000     .                                                                    
130100     EJECT                                                                
130200 IMS-STATUSCHECK SECTION.                                                 
130300     SET STATUS-IX TO 1                                                   
130400     SEARCH GOOD-STATUS                                                   
130500       AT END                                                             
130600         STRING 'INVALID STATUS CODE FROM IMS: ' STATUS-WS                
130700           DELIMITED BY SIZE INTO ERROR-TEXT                              
130800         DISPLAY ERROR-TEXT                                               
130900         CALL FELLOG                                                      
131000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
131100         CONTINUE                                                         
131200     END-SEARCH                                                           
131300     .                                                                    
