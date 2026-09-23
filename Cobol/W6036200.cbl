000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6036200.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   14/11/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       PART LABEL TECHNICAL INFORMATION.                        
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS PROGRAM IS CALLED THRU A WEB SCREEN. THIS IS USED TO        
001100*        ADD TECHNICAL INFO TO CHINESE LABELS.                            
001200*                                                                         
001300*        THE PROGRAM UPDATES   WDT4                                       
001400*        THE PROGRAM READS     WDD3                                       
001500*        THE PROGRAM READS     WDK7                                       
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: W60362T / W60362U                                   
001900*        REQUEST:     W60362I1                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        RESPONSE:    W60362O1                                            
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
003600 77  IDPGM                       PIC X(08)       VALUE 'W6036200'.        
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT                  PIC X(80)       VALUE SPACE.             
004000 77  KDRC-DISPLAY                PIC Z(5).                                
004100                                                                          
004200 77  YES                         PIC X           VALUE 'J'.               
004300 77  NOO                         PIC X           VALUE 'N'.               
004400 77  WS-DC-71                    PIC X(2)        VALUE '71'.              
004500 77  WS-IDSKYLT-CN               PIC X(3)        VALUE 'RCN'.             
004600 77  WS-CP-UTF8                  PIC X(4)        VALUE 'UTF8'.            
004700 77  WS-CP-278                   PIC X(3)        VALUE '278'.             
004710 77  INDX                        PIC S9(4) COMP  VALUE ZERO.              
004720 77  WS-BEART                    PIC X(25)       VALUE SPACE.             
004730 77  INDX-MAX                    PIC S9(4) COMP  VALUE 999.               
004740 77  MAX-POS                     PIC S9(3) COMP  VALUE 180.               
004750 77  MAX-TRAEBCD-POS             PIC S9(3) COMP  VALUE 700.               
004751 77  WTALLY                      PIC S9(3) COMP  VALUE ZERO.              
004752 77  WS-KVRADER                  PIC S9(5) COMP  VALUE ZERO.              
004760 01  EBCDIC-SPACE.                                                        
004770     03 FILLER                   PIC X(180)      VALUE SPACE.             
004780 01  UNICODE-SPACE.                                                       
004790     03 FILLER                   PIC X(180)      VALUE ALL X'20'.         
004800                                                                          
004900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005000     88  KEYS-OK                             VALUE 'J'.                   
005100     88  KEYS-WRONG                          VALUE 'N'.                   
005200     EJECT                                                                
005300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005400     88  INDATA-OK                           VALUE 'J'.                   
005500     EJECT                                                                
005900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006000 01  GENERAL-SUBPROGRAMS.                                                 
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006110     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
006400     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
006410     03  WTRAEBCD                PIC X(8)    VALUE 'WTRAEBCD'.            
006500     SKIP3                                                                
006600*    --- PARAMETERS TO ABEND                                              
006700                                                                          
006800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007100     SKIP3                                                                
007200 01  MESSAGE-CODES.                                                       
007300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007400     03  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.                 
007500     03  UPDATE-NOT-ALLOWED      PIC X(3)    VALUE '007'.                 
007604*                                                                         
007610     03  INF-UPDATE-OK           PIC X(3)    VALUE '001'.                 
007620     03  INF-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.                 
007700     EJECT                                                                
007800*                                                                         
007900 01  FILLER                      PIC X(16)  VALUE 'WTRAUTF8-AREA'.        
008000*01  -COPY WTRAUTF8                                                       
008100     EJECT                                                                
008110*                                                                         
008120 01  FILLER                      PIC X(16)  VALUE 'WTRAEBCD-AREA'.        
008130*01  -COPY WTRAEBCD                                                       
008140     EJECT                                                                
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008400     SKIP3                                                                
008500*01  -COPY WZ01SUB                                                        
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008800     SKIP3                                                                
008900 01  REQU-AREA.                                                           
009000*    03  -COPY WZ01REQU                                                   
009100*    03  -COPY W60362I1                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009400     SKIP3                                                                
009500 01  RESP-AREA.                                                           
009600*    03  -COPY WZ01RESP                                                   
009700*    03  -COPY W60362O1                                                   
009800     EJECT                                                                
009900*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200     SKIP3                                                                
010300 01  KEYS-FOR-DLI.                                                        
010400     03  W-IDARTNR-X.                                                     
010500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010600     03  W-IDDC-X.                                                        
010700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010800     03  W-IDSKYLT-X.                                                     
010900         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
011000     03  W-IDARTNR-MIN-X.                                                 
011100         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
011200     03  W-IDARTNR-MAX-X.                                                 
011300         05  W-IDARTNR-MAX       PIC S9(9)   VALUE 999999999              
011400                                                   COMP-3.                
011500     03  W-IDTEKINF-MIN-X.                                                
011600         05  W-IDTEKINF-MIN      PIC X(180)  VALUE LOW-VALUE.             
011610     03  W-IDTEKINF-MAX-X.                                                
011620         05  W-IDTEKINF-MAX      PIC X(180)  VALUE HIGH-VALUE.            
           03  W-IDDC-B6-X.                                                     
               05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
011700     SKIP2                                                                
011710 01  STA-FL-WDT4              PIC X.                                      
011720     88  SEGM-FOUND-WDT4                  VALUE 'J'.                      
011730     88  SEGM-MISSING-WDT4                VALUE 'N'.                      
011750     SKIP2                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FOUND                       VALUE '  '.                  
012200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012300     88  END-OF-DATABASE                     VALUE 'GB'.                  
012400     SKIP2                                                                
012500 01  GOOD-STATUSCODES.                                                    
012600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012700     SKIP3                                                                
012800 01  SSA1                        PIC X(640).                              
012900 01  SSA2                        PIC X(64).                               
013000     EJECT                                                                
013100*    --- IMS FUNCTION CODES                                               
013200*01  -COPY W0003                                                          
013300     EJECT                                                                
013400                                                                          
013500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT401'.                      
013600 01  DLI-IO-WDT401.                                                       
013700*    03  -COPY WDT401                                                     
013800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
013900 01  DLI-IO-WDD311.                                                       
014000*    03  -COPY WDD311                                                     
014100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
014200 01  DLI-IO-WDK711.                                                       
014300*    03  -COPY WDK711                                                     
       01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
       01  DLI-IO-AREA-B601.                                                    
      *    03  -COPY WDB601                                                     
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600*01  -COPY W0009  -PRE MSG-                                               
014700                                                                          
014800*01  -COPY W0008  -PRE WDT4-                                              
014900     05  FILLER                  PIC X.                                   
015000                                                                          
015100*01  -COPY W0008  -PRE WDD3-                                              
015200     05  FILLER                  PIC X.                                   
015300                                                                          
015400*01  -COPY W0008  -PRE WDK7-                                              
015500     05  FILLER                  PIC X.                                   
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
015600     EJECT                                                                
015700 PROCEDURE DIVISION  USING MSG-PCB WDT4-PCB WDD3-PCB                      
015800                           WDK7-PCB WDB6-PCB.                             
015900 MAIN SECTION.                                                            
016000     ENTRY 'DLITCBL' USING MSG-PCB WDT4-PCB WDD3-PCB                      
016100                           WDK7-PCB WDB6-PCB.                             
016200                                                                          
016300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
016400     IF SUB-KDRC = 0                                                      
016500       PERFORM A-INIT                                                     
016600       PERFORM B-CHECK-KEYS                                               
016700       IF KEYS-OK                                                         
016710         IF REQU-KDPGMACT = 'E'                                           
016720           PERFORM G-UPDATE-DATA                                          
016721         ELSE                                                             
016722           MOVE EBCDIC-SPACE   TO RESP-IDARTNR-UPD                        
016723           MOVE UNICODE-SPACE  TO RESP-IDTEKINF-UPD                       
016730         END-IF                                                           
016800         PERFORM F-READ-SHOW-INFO                                         
016900       END-IF                                                             
017000       PERFORM S02-RETURN-RESPONSE                                        
017100     END-IF                                                               
017200                                                                          
017300     MOVE ZERO TO RETURN-CODE                                             
017400     GOBACK                                                               
017500     .                                                                    
017600     EJECT                                                                
017700 A-INIT SECTION.                                                          
017800                                                                          
017900     MOVE ALL '+'                TO RESP-AREA                             
017910     MOVE ALL X'2B'              TO RESP-IDTEKINF-UPD                     
018000     MOVE ZERO                   TO RESP-KVRADER                          
018010                                    WS-KVRADER                            
018100     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
018200                                    RESP-IDMSG-INFO                       
018300                                    RESP-IDELMT-ERROR                     
018400     MOVE '001'                  TO RESP-IDMSGVER                         
018410                                                                          
018500     MOVE REQU-IDTEKINF-KEY TO TRAEBCD-TECONV-FROM                        
018510     PERFORM S05-CALL-WTRAEBCD                                            
018520     MOVE TRAEBCD-TECONV-TO TO REQU-IDTEKINF-KEY                          
018521                                                                          
018522     MOVE REQU-IDTEKINF-UPD TO TRAEBCD-TECONV-FROM                        
018523     PERFORM S05-CALL-WTRAEBCD                                            
018524     MOVE TRAEBCD-TECONV-TO TO REQU-IDTEKINF-UPD                          
018525                                                                          
018526     MOVE 1 TO INDX                                                       
018527     PERFORM UNTIL INDX > REQU-KVRADER                                    
018528        MOVE REQU-IDTEKINF-LINE(INDX) TO TRAEBCD-TECONV-FROM              
018529        PERFORM S05-CALL-WTRAEBCD                                         
018530        MOVE TRAEBCD-TECONV-TO TO REQU-IDTEKINF-LINE(INDX)                
018531        ADD 1 TO INDX                                                     
018540     END-PERFORM                                                          
           MOVE REQU-IDDC-KEY TO W-IDDC-B6                                      
           PERFORM IMS-GU-WDB601                                                
018600     .                                                                    
018700     EJECT                                                                
018800 B-CHECK-KEYS SECTION.                                                    
018900                                                                          
019000     MOVE YES                    TO KEYS-SW                               
019800                                                                          
019900     IF REQU-IDARTNR-KEY = ALL '+'                                        
020000        CONTINUE                                                          
020100     ELSE                                                                 
020200        INSPECT REQU-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO          
020300        IF REQU-IDARTNR-KEY > ZERO                                        
020400        AND REQU-IDARTNR-KEY IS NUMERIC                                   
020500           MOVE REQU-IDARTNR-KEY  TO W-IDARTNR                            
020600        ELSE                                                              
020700           MOVE NOO               TO KEYS-SW                              
020800           MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                    
020810           MOVE ERR-WRONG-KEY     TO RESP-IDMSG-ERROR                     
020900        END-IF                                                            
021000     END-IF                                                               
021500     .                                                                    
021600     EJECT                                                                
022900 F-READ-SHOW-INFO SECTION.                                                
023000                                                                          
024400     IF REQU-IDARTNR-KEY NOT = ALL '+'                                    
024500        MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                
024600        PERFORM FA-GET-DATA-WITH-IDARTNR                                  
024700     ELSE                                                                 
024800        IF REQU-IDTEKINF-KEY(1:50) NOT = ALL X'2B'                        
024801           MOVE 1 TO INDX                                                 
024802                                                                          
024811           PERFORM UNTIL (REQU-IDTEKINF-KEY(INDX:1) = X'20') OR           
024820                          INDX > MAX-POS                                  
024900              MOVE REQU-IDTEKINF-KEY(INDX:1) TO                           
024901                                          W-IDTEKINF-MIN(INDX:1)          
024902                                          W-IDTEKINF-MAX(INDX:1)          
024905              ADD 1 TO INDX                                               
024910           END-PERFORM                                                    
025000           PERFORM FB-GET-DATA-WITH-TEKINF                                
025100        ELSE                                                              
025200           IF REQU-FLVISA-KEY = YES                                       
025300              PERFORM FC-READ-FULL-DB                                     
025600           END-IF                                                         
025700        END-IF                                                            
025800     END-IF                                                               
025810                                                                          
025910     IF RESP-IDMSG-ERROR = SPACE                                          
026000       IF WS-KVRADER = 0                                                  
026100          MOVE INF-LINES-NOT-FOUND  TO RESP-IDMSG-ERROR                   
026200       END-IF                                                             
026650     END-IF                                                               
026651     IF WS-KVRADER > INDX-MAX                                             
026652        MOVE INDX-MAX TO RESP-KVRADER                                     
026653     ELSE                                                                 
026660        MOVE WS-KVRADER TO RESP-KVRADER                                   
026670     END-IF                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 FA-GET-DATA-WITH-IDARTNR SECTION.                                        
027100     PERFORM IMS-GU-WDT401-ART                                            
027200     IF SEGMENT-FOUND                                                     
027300**      PERFORM FD-GET-DESCR                                              
027400**      MOVE +1                TO INDX                                    
027500**                                WS-KVRADER                              
027600**      PERFORM FE-MOVE-INFO                                              
027610        PERFORM FC-READ-FULL-DB-ART                                       
027700     ELSE                                                                 
027800        MOVE ZERO              TO WS-KVRADER                              
027900     END-IF                                                               
028000     .                                                                    
028100     EJECT                                                                
028200 FB-GET-DATA-WITH-TEKINF SECTION.                                         
028300                                                                          
028310     MOVE ZERO TO INDX                                                    
028320     MOVE 'J' TO STA-FL-WDT4                                              
028400     PERFORM UNTIL SEGM-MISSING-WDT4                                      
028500             OR END-OF-DATABASE                                           
028510             OR INDX > INDX-MAX                                           
028600        PERFORM IMS-GN-WDT401-TEKINF                                      
028700        IF SEGMENT-FOUND                                                  
028800           MOVE TEKI-IDARTNR      TO W-IDARTNR                            
028900           PERFORM FD-GET-DESCR                                           
029000           ADD +1                 TO INDX                                 
029100           IF INDX NOT > INDX-MAX                                         
029200              PERFORM FE-MOVE-INFO                                        
029300           END-IF                                                         
029310        ELSE                                                              
029320           IF SEGMENT-MISSING                                             
029330              MOVE 'N' TO STA-FL-WDT4                                     
029340           END-IF                                                         
029350        END-IF                                                            
029400     END-PERFORM                                                          
029410     MOVE INDX                    TO WS-KVRADER                           
029500     .                                                                    
029600     EJECT                                                                
029700 FC-READ-FULL-DB SECTION.                                                 
029800                                                                          
029801     MOVE 0 TO INDX                                                       
029900     PERFORM UNTIL END-OF-DATABASE OR SEGMENT-MISSING                     
030000                                   OR INDX > INDX-MAX                     
030100        IF SEGMENT-FOUND                                                  
                 IF TEKI-IDARTNR IS NUMERIC                                     
030200            MOVE TEKI-IDARTNR   TO W-IDARTNR                              
                 ELSE                                                           
                  MOVE ZERO TO W-IDARTNR                                        
                 END-IF                                                         
030300           PERFORM FD-GET-DESCR                                           
030400           ADD +1              TO INDX                                    
030500           IF INDX NOT > INDX-MAX                                         
030600              PERFORM FE-MOVE-INFO                                        
030700           END-IF                                                         
030701        END-IF                                                            
030710        PERFORM IMS-GN-WDT401                                             
030800     END-PERFORM                                                          
030810     MOVE INDX                 TO WS-KVRADER                              
030900     .                                                                    
031000     EJECT                                                                
031010 FC-READ-FULL-DB-ART SECTION.                                             
031020                                                                          
031030     MOVE 0 TO INDX                                                       
031040**   PERFORM IMS-GU-WDT401-ART                                            
031050     PERFORM UNTIL END-OF-DATABASE OR SEGMENT-MISSING                     
031060                                   OR INDX > INDX-MAX                     
031070        IF SEGMENT-FOUND                                                  
                 IF TEKI-IDARTNR IS NUMERIC                                     
031080            MOVE TEKI-IDARTNR   TO W-IDARTNR                              
                 ELSE                                                           
                  MOVE 0 TO W-IDARTNR                                           
                 END-IF                                                         
031090           PERFORM FD-GET-DESCR                                           
031091           ADD +1              TO INDX                                    
031092           IF INDX NOT > INDX-MAX                                         
031093              PERFORM FE-MOVE-INFO                                        
031094           END-IF                                                         
031095        END-IF                                                            
031096        PERFORM IMS-GN-WDT401-ART                                         
031097     END-PERFORM                                                          
031098     MOVE INDX                 TO WS-KVRADER                              
031099     .                                                                    
031100     EJECT                                                                
031110 FD-GET-DESCR SECTION.                                                    
031200                                                                          
049301     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
049302     IF DCS-UNICODE-IDSKYLT                                               
049303       MOVE 'UTF8'             TO TRAUTF8-KDCP                            
049304     ELSE                                                                 
049305       MOVE '278 '             TO TRAUTF8-KDCP                            
049306     END-IF                                                               
031500                                                                          
031600     PERFORM IMS-GU-WDD311                                                
031700     IF SEGMENT-FOUND                                                     
031800        MOVE TEXT-BEART      TO TRAUTF8-TECONV-FROM                       
031900     ELSE                                                                 
032000        MOVE SPACE           TO TRAUTF8-TECONV-FROM                       
032100        MOVE WS-CP-278       TO TRAUTF8-KDCP                              
032200     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
032300* -- STRIP SPACE OR CONVERT TO UNICODE                                    
032400     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
032800     .                                                                    
032900     EJECT                                                                
033000 FE-MOVE-INFO SECTION.                                                    
033100                                                                          
033200     MOVE SPACE             TO RESP-KDCMD-LINE(INDX)                      
033300     MOVE TEKI-IDARTNR      TO RESP-IDARTNR-LINE(INDX)                    
033400     MOVE TRAUTF8-TECONV-TO TO RESP-BEART-LINE(INDX)                      
033500     MOVE TEKI-IDTEKINF     TO RESP-IDTEKINF-LINE(INDX)                   
033600     .                                                                    
033700     EJECT                                                                
033800 G-UPDATE-DATA SECTION.                                                   
033801                                                                          
033820     IF REQU-IDARTNR-UPD = ALL '+' OR                                     
033821     REQU-IDTEKINF-UPD(1:50) = ALL X'2B'                                  
033831        IF REQU-IDARTNR-UPD NOT = ALL '+'                                 
033832        OR REQU-IDTEKINF-UPD(1:50) NOT = ALL X'2B'                        
033834           MOVE UPDATE-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
033835           MOVE NOO                TO INDATA-SW                           
033836        END-IF                                                            
033850     ELSE                                                                 
033851        IF REQU-IDARTNR-UPD NOT = ALL '+'                                 
033860          PERFORM GA-CHK-INPUT                                            
033870          IF INDATA-OK                                                    
033880            PERFORM GB-UPDATE-WDT4                                        
033890          END-IF                                                          
033891        END-IF                                                            
033892     END-IF                                                               
033900                                                                          
033910     IF INDATA-OK                                                         
034000       PERFORM VARYING INDX FROM 1 BY 1                                   
034100       UNTIL INDX > REQU-KVRADER                                          
034200         EVALUATE TRUE                                                    
034300           WHEN REQU-KDCMD-LINE(INDX) = 'D'                               
034400              PERFORM GC-DELETE-DATA                                      
034500           WHEN REQU-KDCMD-LINE(INDX) = 'C'                               
034600              MOVE REQU-IDARTNR-LINE(INDX)  TO RESP-IDARTNR-UPD           
034700              MOVE REQU-IDTEKINF-LINE(INDX) TO RESP-IDTEKINF-UPD          
035000         END-EVALUATE                                                     
035100       END-PERFORM                                                        
035101     END-IF                                                               
035200     .                                                                    
035300     EJECT                                                                
036300 GA-CHK-INPUT SECTION.                                                    
036400                                                                          
037200     MOVE REQU-IDARTNR-UPD     TO W-IDARTNR                               
037300     MOVE WS-DC-71             TO W-IDDC                                  
037400     PERFORM IMS-GU-WDK711                                                
037500     IF SEGMENT-FOUND                                                     
037600        CONTINUE                                                          
037700     ELSE                                                                 
037800        MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                       
037810        MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                        
037900        MOVE NOO               TO INDATA-SW                               
038000     END-IF                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 GB-UPDATE-WDT4 SECTION.                                                  
038500                                                                          
038600     MOVE REQU-IDARTNR-UPD    TO W-IDARTNR                                
038700     PERFORM IMS-GHU-WDT401                                               
038701                                                                          
038710     MOVE REQU-IDARTNR-UPD    TO TEKI-IDARTNR                             
038720     MOVE REQU-IDTEKINF-UPD   TO TEKI-IDTEKINF                            
038730                                                                          
038800     IF SEGMENT-FOUND                                                     
039000        PERFORM IMS-REPL-WDT401                                           
039100     ELSE                                                                 
039400        PERFORM IMS-ISRT-WDT401                                           
039500     END-IF                                                               
039501                                                                          
039502     MOVE INF-UPDATE-OK       TO RESP-IDMSG-INFO                          
039510     MOVE EBCDIC-SPACE        TO RESP-IDARTNR-UPD                         
039520     MOVE UNICODE-SPACE       TO RESP-IDTEKINF-UPD                        
039600     .                                                                    
039700     EJECT                                                                
039710 GC-DELETE-DATA SECTION.                                                  
039720                                                                          
039730     MOVE REQU-IDARTNR-LINE(INDX)  TO W-IDARTNR                           
039740     PERFORM IMS-GHU-WDT401                                               
039750     IF SEGMENT-FOUND                                                     
039760        PERFORM IMS-DLET-WDT401                                           
039770        MOVE INF-UPDATE-OK         TO RESP-IDMSG-INFO                     
039780     END-IF                                                               
039790     .                                                                    
039791     EJECT                                                                
039800*    --- DISPATCHER SECTIONS                                              
039900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
040000                                                                          
040100     MOVE 'GETARG'                       TO SUB-KDFUNC                    
040200     MOVE 'CARPARTS.NDC.CHINESETECHINFO' TO SUB-ADDISPABS                 
040300     MOVE LENGTH OF REQU-AREA            TO SUB-KVDLEN                    
040400                                                                          
040500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
040600                                                                          
040700     IF SUB-KDRC > 0                                                      
040800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
040900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
041000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
041100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
041200     END-IF                                                               
041300     .                                                                    
041400     SKIP3                                                                
041500 S02-RETURN-RESPONSE SECTION.                                             
041600                                                                          
041700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
041800     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
041900                                                                          
042000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
042100                                                                          
042200     IF SUB-KDRC > 0                                                      
042300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
042400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
042500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
042600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
042910 S05-CALL-WTRAEBCD SECTION.                                               
042920                                                                          
042950     MOVE 'UTF8'   TO TRAEBCD-KDCP                                        
042990     CALL WTRAEBCD USING TRAEBCD-AREA                                     
042991     MOVE ZERO TO WTALLY                                                  
042992     INSPECT FUNCTION REVERSE(TRAEBCD-TECONV-TO)                          
042993       TALLYING WTALLY FOR LEADING SPACE                                  
042994     IF WTALLY > ZERO                                                     
042995       INSPECT TRAEBCD-TECONV-TO(MAX-TRAEBCD-POS + 1 - WTALLY:)           
042996         CONVERTING SPACE TO X'20'                                        
042997     END-IF                                                               
042998     .                                                                    
042999     EJECT                                                                
043000 IMS-GU-WDT401-ART SECTION.                                               
043100                                                                          
043200     STRING 'WDT401  (IDARTNR  =' W-IDARTNR-X ')'                         
043300          DELIMITED BY SIZE INTO SSA1                                     
043400     MOVE '  GE' TO GOOD-STATUSCODES                                      
043500     CALL CBLTDLI USING GU WDT4-PCB DLI-IO-WDT401 SSA1                    
043600     MOVE WDT4-STATUS-CODE TO STATUS-WS                                   
043700     PERFORM IMS-STATUSCHECK                                              
043800     .                                                                    
043900     SKIP3                                                                
043910 IMS-GN-WDT401-ART SECTION.                                               
043920                                                                          
043930     STRING 'WDT401  (IDARTNR >=' W-IDARTNR-X ')'                         
043940          DELIMITED BY SIZE INTO SSA1                                     
043950     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
043960     CALL CBLTDLI USING GN WDT4-PCB DLI-IO-WDT401 SSA1                    
043970     MOVE WDT4-STATUS-CODE TO STATUS-WS                                   
043980     PERFORM IMS-STATUSCHECK                                              
043990     .                                                                    
043991     SKIP3                                                                
044000 IMS-GN-WDT401-TEKINF SECTION.                                            
044100                                                                          
044200     STRING 'WDT401  (IDARTNR >=' W-IDARTNR-MIN-X                         
044300                     '&IDARTNR <=' W-IDARTNR-MAX-X                        
044400                     '&IDTEKINF>=' W-IDTEKINF-MIN-X                       
044410                     '&IDTEKINF<=' W-IDTEKINF-MAX-X ')'                   
044500          DELIMITED BY SIZE INTO SSA1                                     
044600     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
044700     CALL CBLTDLI USING GN WDT4-PCB DLI-IO-WDT401 SSA1                    
044800     MOVE WDT4-STATUS-CODE TO STATUS-WS                                   
044900     PERFORM IMS-STATUSCHECK                                              
045000     .                                                                    
045100 IMS-GU-WDT401 SECTION.                                                   
045200                                                                          
045400     MOVE '  GEGB'         TO GOOD-STATUSCODES                            
045500     CALL CBLTDLI       USING GU WDT4-PCB DLI-IO-WDT401                   
045600     MOVE WDT4-STATUS-CODE TO STATUS-WS                                   
045700     PERFORM IMS-STATUSCHECK                                              
045800     .                                                                    
045810 IMS-GN-WDT401 SECTION.                                                   
045820                                                                          
045830     MOVE '  GEGB'         TO GOOD-STATUSCODES                            
045840     CALL CBLTDLI       USING GN WDT4-PCB DLI-IO-WDT401                   
045850     MOVE WDT4-STATUS-CODE TO STATUS-WS                                   
045860     PERFORM IMS-STATUSCHECK                                              
045870     .                                                                    
045900 IMS-GHU-WDT401 SECTION.                                                  
046000                                                                          
046100     STRING 'WDT401  (IDARTNR  =' W-IDARTNR-X ')'                         
046200          DELIMITED BY SIZE INTO SSA1                                     
046300     MOVE '  GE' TO GOOD-STATUSCODES                                      
046400     CALL CBLTDLI USING GHU WDT4-PCB DLI-IO-WDT401 SSA1                   
046500     MOVE WDT4-STATUS-CODE TO STATUS-WS                                   
046600     PERFORM IMS-STATUSCHECK                                              
046700     .                                                                    
046800     SKIP3                                                                
046900 IMS-DLET-WDT401 SECTION.                                                 
047000                                                                          
047100     MOVE '  ' TO GOOD-STATUSCODES                                        
047200     CALL CBLTDLI USING DLET WDT4-PCB DLI-IO-WDT401                       
047300     MOVE WDT4-STATUS-CODE TO STATUS-WS                                   
047400     PERFORM IMS-STATUSCHECK                                              
047500     .                                                                    
047600     EJECT                                                                
047700 IMS-REPL-WDT401 SECTION.                                                 
047800                                                                          
047900     MOVE '  ' TO GOOD-STATUSCODES                                        
048000     CALL CBLTDLI USING REPL WDT4-PCB DLI-IO-WDT401                       
048100     MOVE WDT4-STATUS-CODE TO STATUS-WS                                   
048200     PERFORM IMS-STATUSCHECK                                              
048300     .                                                                    
048400     EJECT                                                                
048500 IMS-ISRT-WDT401 SECTION.                                                 
048600                                                                          
048700     STRING 'WDT401   ' DELIMITED BY SIZE INTO SSA1                       
048800     MOVE '  ' TO GOOD-STATUSCODES                                        
048900     CALL CBLTDLI USING ISRT WDT4-PCB DLI-IO-WDT401 SSA1                  
049000     MOVE WDT4-STATUS-CODE TO STATUS-WS                                   
049100     PERFORM IMS-STATUSCHECK                                              
049200     .                                                                    
049300     SKIP3                                                                
049400 IMS-GU-WDD311 SECTION.                                                   
049500                                                                          
049600     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
049700          DELIMITED BY SIZE INTO SSA1                                     
049800     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
049900          DELIMITED BY SIZE INTO SSA2                                     
050000     MOVE '  GE' TO GOOD-STATUSCODES                                      
050100     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
050200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
050300     PERFORM IMS-STATUSCHECK                                              
050400     .                                                                    
050500     EJECT                                                                
050600 IMS-GU-WDK711 SECTION.                                                   
050700                                                                          
050800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
050900          DELIMITED BY SIZE INTO SSA1                                     
051000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
051100          DELIMITED BY SIZE INTO SSA2                                     
051200     MOVE '  GE' TO GOOD-STATUSCODES                                      
051300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
051400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
051500     PERFORM IMS-STATUSCHECK                                              
051600     .                                                                    
051700     EJECT                                                                
       IMS-GU-WDB601 SECTION.                                                   
                                                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  ' TO GOOD-STATUSCODES                                        
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
           SKIP3                                                                
051800 IMS-STATUSCHECK SECTION.                                                 
051900     SKIP2                                                                
052000     SET STATUS-IX TO 1                                                   
052100     SEARCH GOOD-STATUS                                                   
052200       AT END                                                             
052300         STRING 'INVALID STATUS CODE FROM IMS: ' STATUS-WS                
052400           DELIMITED BY SIZE INTO ERROR-TEXT                              
052500         DISPLAY ERROR-TEXT                                               
052600         CALL FELLOG                                                      
052700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
052800         CONTINUE                                                         
052900     END-SEARCH                                                           
053000     .                                                                    
