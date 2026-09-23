000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4072800.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   07/03/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAMN:       CARPARTS.BUYBACK.REPORTS                                 
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER WDA4 BUY BACK BASEN                                        
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDA4                                       
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W40728T                                             
001600*        REQUEST:     W40728I1                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        RESPONSE:    W40728O1                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W4072800'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  INDX                        PIC 9(3)    VALUE ZERO.                  
004000 77  MAX-INDX                    PIC 9(3)    VALUE 500.                   
004100 77  RAD-INDX                    PIC 9(3)    VALUE 500.                   
004200 77  WS-KVRADER                  PIC 9(3)    VALUE 0.                     
004300 77  KDRC-DISPLAY                PIC Z(5).                                
004400 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004500 77  WS-RUB-AREA                 PIC X(100)  VALUE SPACE.                 
004600 77  WS-RAD-AREA                 PIC X(100)  VALUE SPACE.                 
004700 77  WS-IMS-SECTION              PIC X(80)   VALUE SPACE.                 
004800 77  WS-SECTION                  PIC X(80)   VALUE SPACE.                 
004900 77  WS-FLMATCH                  PIC X       VALUE SPACE.                 
005000 77  WS-FLPRGRNS                 PIC X       VALUE SPACE.                 
005100 77  WS-SUM-ART                  PIC 9(6)    VALUE ZERO.                  
005200 77  WS-SUM-BELOPP               PIC 9(9)V99 VALUE ZERO.                  
005300                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005700                                                                          
005800 77  LAES-HUVUD-SW               PIC X       VALUE 'N'.                   
005900     88  LAES-HUVUD                          VALUE 'J'.                   
006000                                                                          
006100 77  LAES-RADER-SW               PIC X       VALUE 'N'.                   
006200     88  LAES-RADER                          VALUE 'J'.                   
006300                                                                          
006400 77  OK-SW                       PIC X       VALUE 'N'.                   
006500     88  ALLT-OK                             VALUE 'J'.                   
006600     88  INTE-OK                             VALUE 'N'.                   
006700                                                                          
006800 77  KDCMD-SW                    PIC X       VALUE 'N'.                   
006900     88  KDCMD-FINNS                         VALUE 'J'.                   
007000     88  KDCMD-SAKNAS                        VALUE 'N'.                   
007100                                                                          
007200 01  DAGENS-DATUM                PIC 9(6).                                
007300 01  DAGENS-KLOCKA               PIC 9(9).                                
007400     EJECT                                                                
007500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007600 01  GENERELLA-SUBPROGRAM.                                                
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200                                                                          
008300     SKIP2                                                                
008400 01  BMP-PARAMETRAR.                                                      
008500   03  SKICKA-IDDISTR            PIC  X(5).                               
008600   03  SKICKA-IDRAPPNR           PIC  X(7).                               
008700     SKIP2                                                                
008800 01  W-PROG-TO-PROG-SW.                                                   
008900*03  -COPY WMSGSOP                                                        
009000     SKIP2                                                                
009100     03  W-WDA401KY-X.                                                    
009200         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
009300         05  W-IDRAPPNR          PIC 9(7)    VALUE ZERO.                  
009400     03    WDA401KY-MIN-X.                                                
009500         05  W-IDDISTR-MIN       PIC S9(5)   VALUE +0   COMP-3.           
009600         05  W-IDRAPPNR-MIN      PIC 9(7)    VALUE ZERO.                  
009700     03    WDA401KY-MAX-X.                                                
009800         05  W-IDDISTR-MAX       PIC S9(5)   VALUE +0   COMP-3.           
009900         05  W-IDRAPPNR-MAX      PIC 9(7)    VALUE 9999999.               
010000     03    WDA401KY-DEL-X.                                                
010100         05  W-IDDISTR-DEL       PIC S9(5)   VALUE +0   COMP-3.           
010200         05  W-IDRAPPNR-DEL      PIC 9(7)    VALUE 9999999.               
010300     03  W-IDARTNR-X.                                                     
010400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010500     03  W-IDDISTR-X.                                                     
010600         05  W-IDDISTR-UNIK      PIC S9(5)   VALUE +0   COMP-3.           
010700     03  W-WDA411KY-X.                                                    
010800         05  W-FLMATCH-UNIK      PIC X       VALUE SPACE.                 
010900         05  W-FLPRGRNS-UNIK     PIC X       VALUE SPACE.                 
011000         05  W-IDARTNR-UNIK      PIC S9(9)   VALUE ZERO COMP-3.           
011100*    --- PARAMETRAR TILL ABEND                                            
011200                                                                          
011300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011600     SKIP3                                                                
011700 01  MESSAGE-CODES.                                                       
011800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
011900     EJECT                                                                
012000*                                                                         
012100 01  FILLER                      PIC X(16)   VALUE 'HEADERS'.             
012200 01  TOM-RAD.                                                             
012300   03  FILLER                       PIC X(20)  VALUE SPACE.               
012400                                                                          
012500 01  STJARN-RAD.                                                          
012600   03  FILLER                       PIC X(86)  VALUE ALL '*'.             
012700                                                                          
012800 01  RUB-MATCH-PRIS-1.                                                    
012900   03  FILLER                       PIC X(20)  VALUE SPACE.               
013000   03  FILLER                       PIC X(26)  VALUE                      
013100       'VOLVO CAR CORP. AFTERSALES'.                                      
013200   03  FILLER                       PIC X(34)  VALUE SPACE.               
013300                                                                          
013400 01  RUB-MATCH-PRIS-2.                                                    
013500   03  FILLER                       PIC X(7)   VALUE                      
013600       ' DATE: '.                                                         
013700   03  MATCH-TIREGDAT               PIC 9(6).                             
013800   03  FILLER                       PIC X(2)   VALUE SPACE.               
013900   03  FILLER                       PIC X(11)  VALUE                      
014000       'DISTR NO : '.                                                     
014100   03  MATCH-IDDISTR                PIC 9(4).                             
014200   03  FILLER                       PIC X(2)   VALUE SPACE.               
014300   03  FILLER                       PIC X(13)  VALUE                      
014400       'CUSTOMER NO: '.                                                   
014500   03  MATCH-IDKUNDNR               PIC Z(6)9(1).                         
014600   03  FILLER                       PIC X(2)   VALUE SPACE.               
014700   03  FILLER                       PIC X(5)   VALUE                      
014800       'ID : '.                                                           
014900   03  MATCH-IDRAPPNR               PIC Z9(7).                            
015000                                                                          
015100 01  RUB-MATCH-PRIS.                                                      
015200   03  FILLER                       PIC X(24)  VALUE                      
015300       ' MATCHED AND VALUE OVER '.                                        
015400   03  MATCH-SUMINVD-RUB1           PIC 9(3).                             
015500   03  FILLER                       PIC X(2) VALUE SPACE.                 
015600   03  MATCH-KDVALISO-RUB1          PIC X(3).                             
015700                                                                          
015800 01  RUB-MATCH.                                                           
015900   03  FILLER                       PIC X(24)  VALUE                      
016000       ' MATCHED AND LESS THEN  '.                                        
016100   03  MATCH-SUMINVD-RUB2           PIC 9(3).                             
016200   03  FILLER                       PIC X(2) VALUE SPACE.                 
016300   03  MATCH-KDVALISO-RUB2          PIC X(3).                             
016400                                                                          
016500                                                                          
016600 01  RUB-OMATCH-PRIS.                                                     
016700   03  FILLER                        PIC X(28)  VALUE                     
016800       ' NOT MATCHED AND VALUE OVER '.                                    
016900   03  OMATCH-SUMINVD-RUB1           PIC 9(3).                            
017000   03  FILLER                        PIC X(2) VALUE SPACE.                
017100   03  OMATCH-KDVALISO-RUB1          PIC X(3).                            
017200                                                                          
017300 01  RUB-OMATCH.                                                          
017400   03  FILLER                        PIC X(28)  VALUE                     
017500       ' NOT MATCHED AND LESS THEN  '.                                    
017600   03  OMATCH-SUMINVD-RUB2           PIC 9(3).                            
017700   03  FILLER                        PIC X(2) VALUE SPACE.                
017800   03  OMATCH-KDVALISO-RUB2          PIC X(3).                            
017900                                                                          
018000                                                                          
018100 01  RUB-MATCH-PRIS-3.                                                    
018200   03  FILLER                       PIC X(10)  VALUE                      
018300       ' PART NO  '.                                                      
018400   03 FILLER                        PIC X(2)   VALUE SPACE.               
018500   03  FILLER                       PIC X(14)  VALUE                      
018600       'DESCRIPTION   '.                                                  
018700   03 FILLER                        PIC X(2)   VALUE SPACE.               
018800   03 FILLER                        PIC X(13)  VALUE SPACE.               
018900   03  FILLER                       PIC X(10)  VALUE                      
019000       'QTY REQ   '.                                                      
019100   03 FILLER                        PIC X(2)   VALUE SPACE.               
019200   03  FILLER                       PIC X(10)  VALUE                      
019300       'UNIT PRICE'.                                                      
019400   03 FILLER                        PIC X(2)   VALUE SPACE.               
019500   03  FILLER                       PIC X(11)  VALUE                      
019600       'TOTAL PRICE'.                                                     
019700   03 FILLER                        PIC X(2)   VALUE SPACE.               
019800   03  FILLER                       PIC X(8)   VALUE                      
019900       'CURRENCY'.                                                        
020000   03 FILLER                        PIC X(2)   VALUE SPACE.               
020100                                                                          
020200 01  RUB-MATCH-PRIS-RAD.                                                  
020300   03  MATCH-IDARTNR                PIC Z(9).                             
020400   03  FILLER                       PIC X(3)   VALUE SPACE.               
020500   03  MATCH-BEART                  PIC X(25).                            
020600   03  FILLER                       PIC X(2)   VALUE SPACE.               
020700   03  MATCH-KVANTAL                PIC Z(9).                             
020800   03  FILLER                       PIC X(2)   VALUE SPACE.               
020900   03  FILLER                       PIC X(3)   VALUE SPACE.               
021000   03  MATCH-PRARTNTO-NEW           PIC Z(7).99.                          
021100   03  FILLER                       PIC X(3)   VALUE SPACE.               
021200   03  MATCH-PRARTNTO-TOT           PIC Z(7).99.                          
021300   03  FILLER                       PIC X(7)   VALUE SPACE.               
021400   03  MATCH-KDVALISO               PIC X(3).                             
021500   03  FILLER                       PIC X(2)   VALUE SPACE.               
021600                                                                          
021700 01  SUMMA-RAD.                                                           
021800   03  FILLER                       PIC X(22) VALUE SPACE.                
021900   03  FILLER                       PIC X(17) VALUE                       
022000       'NUMBER OF PARTS: '.                                               
022100   03  MATCH-SUM-ART                PIC Z(9).                             
022200   03  FILLER                       PIC X(5) VALUE SPACE.                 
022300   03  FILLER                       PIC X(10) VALUE                       
022400       'SUM VALUE:'.                                                      
022500   03  FILLER                       PIC X(3) VALUE SPACE.                 
022600   03  MATCH-SUM-BELOPP             PIC Z(7).99.                          
022700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
022800     SKIP3                                                                
022900*    --- AREOR FÖR KOMMUNIKATION                                          
023000*01  -COPY WZ01SUB                                                        
023100     EJECT                                                                
023200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
023300     SKIP3                                                                
023400 01  REQU-AREA.                                                           
023500*    03  -COPY WZ01REQU                                                   
023600*    03  -COPY W40728I1                                                   
023700     EJECT                                                                
023800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
023900     SKIP3                                                                
024000 01  RESP-AREA.                                                           
024100*    03  -COPY WZ01RESP                                                   
024200*    03  -COPY W40728O1                                                   
024300     EJECT                                                                
024400 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
024500*01  -COPY WZ01SEND                                                       
024600     EJECT                                                                
024700 01  UT-AREA-START               PIC X(24)   VALUE                        
024800                                 'UT-AREA-START '.                        
024900 01  HDR-AREA.                                                            
025000*    03  -COPY WZ01REQU -PRE HDR-                                         
025100*    03  -COPY WZ04HDR                                                    
025200     EJECT                                                                
025300                                                                          
025400     EJECT                                                                
025500                                                                          
025600*    --- STATUS-KOD FRÅN IMS                                              
025700 01  STATUS-WS                   PIC XX.                                  
025800     88  STATUS-OK                           VALUE '  '.                  
025900     88  SEGMENT-FINNS                       VALUE '  '.                  
026000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026100     88  BASEN-SLUT                          VALUE 'GB'.                  
026200     SKIP2                                                                
026300 01  GODK-STATUSKODER.                                                    
026400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026500     SKIP3                                                                
026600 01  SSA1                        PIC X(128).                              
026700 01  SSA2                        PIC X(128).                              
026800     EJECT                                                                
026900*    --- IMS FUNKTIONSKODER                                               
027000*01  -COPY W0003                                                          
027100     EJECT                                                                
027200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA401'.                      
027300 01  DLI-IO-WDA401.                                                       
027400*    03  -COPY WDA401                                                     
027500     EJECT                                                                
027600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA411'.                      
027700 01  DLI-IO-WDA411.                                                       
027800*    03  -COPY WDA411                                                     
027900     EJECT                                                                
028000 LINKAGE SECTION.                                                         
028100                                                                          
028200*01  -COPY W0009  -PRE MSG-                                               
028300     EJECT                                                                
028400*01  -COPY W0009   -PRE ALT-                                              
028500 01  DISTRDOC-PCB                PIC X.                                   
028600     EJECT                                                                
028700*01  -COPY W0008  -PRE WDA4-                                              
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB DISTRDOC-PCB WDA4-PCB.         
029100 MAIN SECTION.                                                            
029200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB DISTRDOC-PCB WDA4-PCB.         
029300                                                                          
029400     PERFORM S01-HAEMTA-ANROPSDATA                                        
029500     IF SUB-KDRC = 0                                                      
029600       PERFORM A-INIT                                                     
029700       PERFORM B-KOLLA-NYCKLAR                                            
029800       IF NYCKLAR-OK                                                      
029900         IF REQU-KDPGMACT = 'S' OR 'N'                                    
030000           PERFORM F-LAES-VISA-INFO                                       
030100         ELSE                                                             
030200           IF REQU-KDPGMACT = 'E'                                         
030300             MOVE JA TO OK-SW                                             
030400             PERFORM G-KOLLA-KDCMD                                        
030500             IF KDCMD-FINNS                                               
030600               PERFORM IMS-GET-WDA401                                     
030700               IF SEGMENT-FINNS                                           
030800                 PERFORM IMS-DLET-WDA401                                  
030900                 MOVE '003'          TO RESP-IDMSG-INFO                   
031000               END-IF                                                     
031100             ELSE                                                         
031200               IF REQU-FLPRINT-IN  = 'J' AND                              
031300                  REQU-FLPERMIT-IN = 'N'                                  
031400                   PERFORM D-PRINT-LISTA                                  
031500               ELSE                                                       
031600                 IF REQU-FLPERMIT-IN = 'J' AND                            
031700                    REQU-FLPRINT-IN  = 'N'                                
031800                      PERFORM E-SKAPA-TILLSTAND                           
031900                 ELSE                                                     
032000                   IF REQU-FLPERMIT-IN = 'J' AND                          
032100                      REQU-FLPRINT-IN  = 'J'                              
032200                     MOVE '042'           TO RESP-IDMSG-ERROR             
032300                     MOVE NEJ TO OK-SW                                    
032400                   END-IF                                                 
032500                   IF REQU-FLPERMIT-IN = 'N' AND                          
032600                      REQU-FLPRINT-IN  = 'N'                              
032700                     MOVE '014'           TO RESP-IDMSG-ERROR             
032800                     MOVE NEJ TO OK-SW                                    
032900                   END-IF                                                 
033000                 END-IF                                                   
033100               END-IF                                                     
033200             END-IF                                                       
033300                                                                          
033400             IF ALLT-OK                                                   
033500*              MOVE ALL '+' TO RESP-W40728O1                              
033600               IF RESP-IDMSG-INFO = SPACE                                 
033700                 MOVE '001'   TO RESP-IDMSG-INFO                          
033800               END-IF                                                     
033900               MOVE SPACE TO RESP-FLPRINT-IN                              
034000               MOVE SPACE TO RESP-FLPERMIT-IN                             
034100             ELSE                                                         
034200               MOVE ALL '+' TO RESP-W40728O1                              
034300             END-IF                                                       
034400           END-IF                                                         
034500         END-IF                                                           
034600       END-IF                                                             
034700       IF ALLT-OK AND REQU-KDPGMACT = 'E'                                 
034800         PERFORM F-LAES-VISA-INFO                                         
034900       END-IF                                                             
035000       PERFORM S02-RETURNERA-SVAR                                         
035100     END-IF                                                               
035200                                                                          
035300                                                                          
035400*    PERFORM Z-FINIT                                                      
035500     MOVE ZERO TO RETURN-CODE                                             
035600     GOBACK                                                               
035700     .                                                                    
035800     EJECT                                                                
035900 A-INIT SECTION.                                                          
036000                                                                          
036100     MOVE ALL '+' TO RESP-AREA                                            
036200     MOVE ZERO    TO RESP-KVRADER                                         
036300     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
036400                     RESP-IDMSG-INFO                                      
036500                     RESP-IDELMT-ERROR                                    
036600     MOVE '001'   TO RESP-IDMSGVER                                        
036700     IF REQU-KDPGMACT = 'S'                                               
036800       MOVE SPACE TO RESP-W40728O1                                        
036900     END-IF                                                               
037000     MOVE LOW-VALUE  TO WDA401KY-MIN-X                                    
037100     MOVE HIGH-VALUE TO WDA401KY-MAX-X                                    
037200     ACCEPT DAGENS-KLOCKA FROM TIME                                       
037300     ACCEPT DAGENS-DATUM  FROM DATE                                       
037400*    DISPLAY 'W4072800'                                                   
037500*    DISPLAY 'TID : ' DAGENS-KLOCKA                                       
037600     .                                                                    
037700     EJECT                                                                
037800 B-KOLLA-NYCKLAR SECTION.                                                 
037900                                                                          
038000     MOVE JA TO NYCKLAR-SW                                                
038100     IF REQU-IDDISTR-KEY NOT = ALL '+'                                    
038200       IF REQU-IDDISTR-KEY NUMERIC                                        
038300         MOVE REQU-IDDISTR-KEY TO RESP-IDDISTR-KEY                        
038400                                  W-IDDISTR                               
038500*                                 W-IDDISTR-MIN                           
038600*                                 W-IDDISTR-MAX                           
038700       ELSE                                                               
038800         MOVE NEJ TO NYCKLAR-SW                                           
038900       END-IF                                                             
039000     END-IF                                                               
039100                                                                          
039200     IF REQU-IDRAPPNR-KEY NOT = ALL '+'                                   
039300      IF REQU-IDRAPPNR-KEY NUMERIC                                        
039400        MOVE REQU-IDRAPPNR-KEY TO RESP-IDRAPPNR-KEY                       
039500                                  W-IDRAPPNR                              
039600*                                 W-IDRAPPNR-MIN                          
039700*                                 W-IDRAPPNR-MAX                          
039800      ELSE                                                                
039900        MOVE NEJ TO NYCKLAR-SW                                            
040000      END-IF                                                              
040100     END-IF                                                               
040200     IF REQU-FLMATCH-KEY = 'J' OR 'N'                                     
040300       MOVE REQU-FLMATCH-KEY   TO RESP-FLMATCH-KEY                        
040400                                                                          
040500     ELSE                                                                 
040600        MOVE NEJ                TO NYCKLAR-SW                             
040700        MOVE 'N'                TO RESP-FLMATCH-KEY                       
040800     END-IF                                                               
040900     IF REQU-FLPRGRNS-KEY = 'J' OR 'N'                                    
041000       MOVE REQU-FLPRGRNS-KEY  TO RESP-FLPRGRNS-KEY                       
041100     ELSE                                                                 
041200        MOVE NEJ                TO NYCKLAR-SW                             
041300        MOVE 'N'                TO RESP-FLPRGRNS-KEY                      
041400     END-IF                                                               
041500     IF NYCKLAR-OK                                                        
041600       IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDRAPPNR-KEY NUMERIC          
041700         MOVE JA TO LAES-RADER-SW                                         
041800       ELSE                                                               
041900         MOVE JA TO LAES-HUVUD-SW                                         
042000       END-IF                                                             
042100     END-IF                                                               
042200                                                                          
042300     IF NYCKLAR-FEL                                                       
042400       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
042500     END-IF                                                               
042600     .                                                                    
042700     EJECT                                                                
042800 D-PRINT-LISTA    SECTION.                                                
042900     MOVE 'D-PRINT           ' TO WS-SECTION                              
043000                                                                          
043100     MOVE JA                  TO OK-SW                                    
043200     MOVE REQU-IDDISTR-KEY    TO W-IDDISTR                                
043300     MOVE REQU-IDRAPPNR-KEY   TO W-IDRAPPNR                               
043400     MOVE 'SKA GET WDA401    ' TO WS-SECTION                              
043500     PERFORM IMS-GET-WDA401                                               
043600     IF SEGMENT-FINNS AND BUYB-FLPRINT = 'N'                              
043700       MOVE 'SKA S90-SEND-OPEN ' TO WS-SECTION                            
043800       PERFORM  S90-SEND-OPEN                                             
043900       MOVE SEND-IDCOM         TO WZ04-SEND-IDCOM                         
044000       MOVE 001                TO HDR-REQU-IDMSGVER                       
044100       MOVE SPACE              TO HDR-REQU-KDPGMACT                       
044200       MOVE REQU-IDUSER        TO HDR-REQU-IDUSER                         
044300                                                                          
044400       MOVE SPACE               TO HDR-IDOUTREC                           
044500       MOVE 'BUYBACK'           TO HDR-IDOUTTYPE                          
044600       MOVE REQU-IDUSER         TO HDR-IDOUTREC(1:7)                      
044700       MOVE DAGENS-KLOCKA       TO HDR-IDLIST                             
044800                                                                          
044900       MOVE 'S90-PUT-HEADER    ' TO WS-SECTION                            
045000                                                                          
045100       PERFORM  S90-PUT-HEADER                                            
045200                                                                          
045300       MOVE 'HAR GJORT PUT HEADER' TO WS-SECTION                          
045400                                                                          
045500       MOVE RUB-MATCH-PRIS-1     TO WS-RUB-AREA                           
045600                                                                          
045700       MOVE 'SKA GÖRA S90 PUT DOC HEAD ' TO WS-SECTION                    
045800                                                                          
045900       PERFORM  S90-PUT-DOC-HEAD                                          
046000       MOVE 'HAR GJORT PUT DOC-HEAD' TO WS-SECTION                        
046100       MOVE BUYB-IDDISTR         TO MATCH-IDDISTR                         
046200       MOVE BUYB-IDRAPPNR        TO MATCH-IDRAPPNR                        
046300       MOVE BUYB-IDKUNDNR        TO MATCH-IDKUNDNR                        
046400       MOVE BUYB-TIREGDAT        TO MATCH-TIREGDAT                        
046500       MOVE BUYB-SUMINVD         TO MATCH-SUMINVD-RUB1                    
046600                                    MATCH-SUMINVD-RUB2                    
046700                                    OMATCH-SUMINVD-RUB1                   
046800                                    OMATCH-SUMINVD-RUB2                   
046900       MOVE RUB-MATCH-PRIS-2     TO WS-RUB-AREA                           
047000       MOVE 'SKA GÖRA S90 PUT DOC HEAD2' TO WS-SECTION                    
047100       PERFORM  S90-PUT-DOC-HEAD                                          
047200                                                                          
047300       MOVE 'SKA GÖRA S90 PUT DOC HEAD3' TO WS-SECTION                    
047400       PERFORM  IMS-GN-WDA411                                             
047500       IF SEGMENT-FINNS                                                   
047600         MOVE BART-KDVALISO    TO MATCH-KDVALISO-RUB1                     
047700                                  MATCH-KDVALISO-RUB2                     
047800                                  OMATCH-KDVALISO-RUB1                    
047900                                  OMATCH-KDVALISO-RUB2                    
048000         IF BART-FLMATCH = 'J' AND BART-FLPRGRNS = 'J'                    
048100           MOVE RUB-MATCH-PRIS      TO WS-RUB-AREA                        
048200         END-IF                                                           
048300         IF BART-FLMATCH = 'J' AND BART-FLPRGRNS = 'N'                    
048400           MOVE RUB-MATCH           TO WS-RUB-AREA                        
048500         END-IF                                                           
048600         IF BART-FLMATCH = 'N' AND BART-FLPRGRNS = 'J'                    
048700           MOVE RUB-OMATCH-PRIS     TO WS-RUB-AREA                        
048800         END-IF                                                           
048900         IF BART-FLMATCH = 'N' AND BART-FLPRGRNS = 'N'                    
049000           MOVE RUB-OMATCH          TO WS-RUB-AREA                        
049100         END-IF                                                           
049200         MOVE BART-FLMATCH          TO WS-FLMATCH                         
049300         MOVE BART-FLPRGRNS         TO WS-FLPRGRNS                        
049400         PERFORM S90-PUT-DOC-HEAD                                         
049500         MOVE RUB-MATCH-PRIS-3      TO WS-RUB-AREA                        
049600         PERFORM  S90-PUT-DOC-HEAD                                        
049700       END-IF                                                             
049800       IF SEGMENT-FINNS                                                   
049900         PERFORM UNTIL SEGMENT-SAKNAS                                     
050000           MOVE BART-IDARTNR       TO MATCH-IDARTNR                       
050100           MOVE BART-BEART         TO MATCH-BEART                         
050200           MOVE BART-KVANTAL       TO MATCH-KVANTAL                       
050300           MOVE BART-PRARTNTO-NEW  TO MATCH-PRARTNTO-NEW                  
050400           MOVE BART-PRARTNTO-TOT  TO MATCH-PRARTNTO-TOT                  
050500           MOVE BART-KDVALISO      TO MATCH-KDVALISO                      
050600           MOVE RUB-MATCH-PRIS-RAD TO WS-RAD-AREA                         
050700           ADD +1 TO WS-SUM-ART                                           
050800           COMPUTE WS-SUM-BELOPP =                                        
050900                 WS-SUM-BELOPP + BART-PRARTNTO-TOT                        
051000                                                                          
051100           MOVE 'SKA GÖRA S90 PUT DOC LINE ' TO WS-SECTION                
051200           PERFORM S90-PUT-DOC-LINE                                       
051300                                                                          
051400           PERFORM  IMS-GN-WDA411                                         
051500           IF SEGMENT-FINNS                                               
051600             IF WS-FLMATCH NOT = BART-FLMATCH OR                          
051700                WS-FLPRGRNS NOT = BART-FLPRGRNS                           
051800               PERFORM DA-NYTT-DAP-HUVUD                                  
051900             END-IF                                                       
052000           END-IF                                                         
052100         END-PERFORM                                                      
052200                                                                          
052300         MOVE TOM-RAD              TO WS-RUB-AREA                         
052400         PERFORM  S90-PUT-DOC-HEAD                                        
052500                                                                          
052600         MOVE WS-SUM-ART           TO MATCH-SUM-ART                       
052700         MOVE WS-SUM-BELOPP        TO MATCH-SUM-BELOPP                    
052800         MOVE SUMMA-RAD            TO WS-RUB-AREA                         
052900         PERFORM  S90-PUT-DOC-HEAD                                        
053000                                                                          
053100         MOVE 'SKA GÖRA CLOSE      ' TO WS-SECTION                        
053200         PERFORM  S90-SEND-CLOSE                                          
053300         MOVE ZERO TO WZ04-SEND-IDCOM                                     
053400                                                                          
053500****   UPPDATERA WDA401 MED JA PÅ FLPRINT                                 
053600         PERFORM IMS-GET-WDA401                                           
053700         IF SEGMENT-FINNS                                                 
053800           MOVE JA          TO BUYB-FLPRINT                               
053900           PERFORM IMS-REPL-WDA401                                        
054000           MOVE '101'       TO RESP-IDMSG-INFO                            
054100         END-IF                                                           
054200       ELSE                                                               
054300         MOVE NEJ TO OK-SW                                                
054400         MOVE '027'         TO RESP-IDMSG-ERROR                           
054500         MOVE 'IDARTNR'     TO RESP-IDELMT-ERROR                          
054600       END-IF                                                             
054700     ELSE                                                                 
054800       MOVE NEJ TO OK-SW                                                  
054900       IF SEGMENT-FINNS                                                   
055000         IF BUYB-FLPRINT = 'J'                                            
055100           MOVE '007'       TO RESP-IDMSG-ERROR                           
055200         END-IF                                                           
055300       ELSE                                                               
055400         MOVE '027'         TO RESP-IDMSG-ERROR                           
055500         MOVE 'IDRAPPNR'    TO RESP-IDELMT-ERROR                          
055600       END-IF                                                             
055700     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
056000 DA-NYTT-DAP-HUVUD SECTION.                                               
056100*** SUMMA POSTER IFRÅN MATCH SKALL LÄGGAS UT OCKSÅ                        
056200     MOVE TOM-RAD              TO WS-RUB-AREA                             
056300     PERFORM  S90-PUT-DOC-HEAD                                            
056400                                                                          
056500     MOVE WS-SUM-ART           TO MATCH-SUM-ART                           
056600     MOVE WS-SUM-BELOPP        TO MATCH-SUM-BELOPP                        
056700     MOVE SUMMA-RAD            TO WS-RUB-AREA                             
056800     PERFORM  S90-PUT-DOC-HEAD                                            
056900                                                                          
057000     MOVE ZERO                 TO WS-SUM-ART                              
057100     MOVE ZERO                 TO WS-SUM-BELOPP                           
057200                                                                          
057300     MOVE TOM-RAD              TO WS-RUB-AREA                             
057400     PERFORM  S90-PUT-DOC-HEAD                                            
057500     MOVE STJARN-RAD           TO WS-RUB-AREA                             
057600     PERFORM  S90-PUT-DOC-HEAD                                            
057700     MOVE TOM-RAD              TO WS-RUB-AREA                             
057800     PERFORM  S90-PUT-DOC-HEAD                                            
057900                                                                          
058000     MOVE RUB-MATCH-PRIS-1     TO WS-RUB-AREA                             
058100                                                                          
058200     PERFORM  S90-PUT-DOC-HEAD                                            
058300                                                                          
058400     MOVE RUB-MATCH-PRIS-2     TO WS-RUB-AREA                             
058500     PERFORM  S90-PUT-DOC-HEAD                                            
058600                                                                          
058700     IF BART-FLMATCH = 'J' AND BART-FLPRGRNS = 'J'                        
058800       MOVE RUB-MATCH-PRIS      TO WS-RUB-AREA                            
058900     END-IF                                                               
059000     IF BART-FLMATCH = 'J' AND BART-FLPRGRNS = 'N'                        
059100       MOVE RUB-MATCH           TO WS-RUB-AREA                            
059200     END-IF                                                               
059300     IF BART-FLMATCH = 'N' AND BART-FLPRGRNS = 'J'                        
059400       MOVE RUB-OMATCH-PRIS     TO WS-RUB-AREA                            
059500     END-IF                                                               
059600     IF BART-FLMATCH = 'N' AND BART-FLPRGRNS = 'N'                        
059700       MOVE RUB-OMATCH          TO WS-RUB-AREA                            
059800     END-IF                                                               
059900     MOVE BART-FLMATCH          TO WS-FLMATCH                             
060000     MOVE BART-FLPRGRNS         TO WS-FLPRGRNS                            
060100                                                                          
060200     PERFORM S90-PUT-DOC-HEAD                                             
060300                                                                          
060400     MOVE RUB-MATCH-PRIS-3     TO WS-RUB-AREA                             
060500     PERFORM S90-PUT-DOC-HEAD                                             
060600                                                                          
060700     .                                                                    
060800     EJECT                                                                
060900 E-SKAPA-TILLSTAND SECTION.                                               
061000                                                                          
061100*                                                                         
061200*  FLYTTA DATA TILL WMSGSOP / STARTA RUTIN W418B4                         
061300*                                                                         
061400     PERFORM IMS-GET-WDA401                                               
061500     IF SEGMENT-FINNS                                                     
061600       IF BUYB-FLPERMIT = 'N'                                             
061700         PERFORM IMS-GN-WDA411                                            
061800         IF BART-FLMATCH = 'J'                                            
061900           MOVE W-IDDISTR         TO SKICKA-IDDISTR                       
062000           MOVE W-IDRAPPNR        TO SKICKA-IDRAPPNR                      
062100                                                                          
062200           MOVE '0606'        TO MSGSOP-IDTRANS                           
062300           MOVE '1'           TO MSGSOP-KDMFSFOR                          
062400           MOVE 'W418B4'      TO MSGSOP-IDPROCESS                         
062500           MOVE 'A'           TO MSGSOP-KDSOPFUNK                         
062600                                                                          
062700           STRING 'IDDISTR(' SKICKA-IDDISTR ')                            
062800-               'IDRAPPNR(' SKICKA-IDRAPPNR ')'                           
062900                DELIMITED BY SIZE INTO MSGSOP-TESYMBV                     
063000           PERFORM IMS-INSERT-ALTMSG                                      
063100                                                                          
063200           MOVE '102'          TO RESP-IDMSG-INFO                         
063300           PERFORM IMS-GET-WDA401                                         
063400           IF SEGMENT-FINNS                                               
063500             MOVE 'J'          TO BUYB-FLPERMIT                           
063600             PERFORM IMS-REPL-WDA401                                      
063700           END-IF                                                         
063800         ELSE                                                             
063900           MOVE NEJ TO OK-SW                                              
064000           MOVE '007'          TO RESP-IDMSG-ERROR                        
064100         END-IF                                                           
064200       ELSE                                                               
064300         MOVE NEJ TO OK-SW                                                
064400         MOVE '007'          TO RESP-IDMSG-ERROR                          
064500       END-IF                                                             
064600     ELSE                                                                 
064700       MOVE NEJ TO OK-SW                                                  
064800       MOVE '007'          TO RESP-IDMSG-ERROR                            
064900       MOVE 'IDRAPPNR'     TO RESP-IDELMT-ERROR                           
065000     END-IF                                                               
065100     .                                                                    
065200     EJECT                                                                
065300 F-LAES-VISA-INFO SECTION.                                                
065400     MOVE ZERO TO  INDX                                                   
065500     PERFORM FA-LAES-GRUNDDATA                                            
065600                                                                          
065700     IF SEGMENT-SAKNAS                                                    
065800       MOVE '027' TO RESP-IDMSG-ERROR                                     
065900     ELSE                                                                 
066000       ADD +1 TO INDX                                                     
066100       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX OR                 
066200          BASEN-SLUT                                                      
066300         IF LAES-HUVUD                                                    
066400           MOVE BUYB-IDRAPPNR    TO RESP-IDRAPPNR-RAD  (INDX)             
066500           MOVE BUYB-IDKUNDNR    TO RESP-IDKUNDNR-RAD  (INDX)             
066600           MOVE BUYB-TIREGDAT    TO RESP-TIREGDAT-RAD  (INDX)             
066700           IF BUYB-FLPRINT = 'J'                                          
066800             MOVE 'Y'            TO RESP-FLPRINT-RAD   (INDX)             
066900           ELSE                                                           
067000             MOVE BUYB-FLPRINT   TO RESP-FLPRINT-RAD   (INDX)             
067100           END-IF                                                         
067200           IF BUYB-FLPERMIT = 'J'                                         
067300             MOVE 'Y'            TO RESP-FLPERMIT-RAD  (INDX)             
067400           ELSE                                                           
067500             MOVE BUYB-FLPERMIT  TO RESP-FLPERMIT-RAD  (INDX)             
067600           END-IF                                                         
067700           MOVE INDX TO WS-KVRADER                                        
067800           ADD +1 TO INDX                                                 
067900           PERFORM IMS-GET-WDA401-HUV                                     
068000         END-IF                                                           
068100         IF LAES-RADER                                                    
068200          IF REQU-FLMATCH-KEY  = BART-FLMATCH AND                         
068300             REQU-FLPRGRNS-KEY = BART-FLPRGRNS                            
068400           MOVE BART-IDARTNR       TO RESP-IDARTNR-RAD   (INDX)           
068500           MOVE BART-BEART         TO RESP-BEART-RAD     (INDX)           
068600           MOVE BART-KVANTAL       TO RESP-KVANTAL-RAD   (INDX)           
068700           MOVE BART-PRARTNTO-TOT  TO RESP-PRARTNTO-TOT-RAD(INDX)         
068800           MOVE BART-KDVALISO      TO RESP-KDVALISO-RAD  (INDX)           
068900           MOVE BART-PRARTNTO-NEW  TO RESP-PRARTNTO-NEW-RAD(INDX)         
069000           MOVE INDX TO WS-KVRADER                                        
069100           ADD +1 TO INDX                                                 
069200          END-IF                                                          
069300          PERFORM IMS-GN-WDA411                                           
069400         END-IF                                                           
069500       END-PERFORM                                                        
069600       MOVE WS-KVRADER TO RESP-KVRADER                                    
069700       IF INDX > MAX-INDX                                                 
069800         IF LAES-RADER                                                    
069900           PERFORM IMS-GN-WDA411                                          
070000           MOVE BART-IDARTNR       TO RESP-IDARTNR-NEXT                   
070100           MOVE 500                TO RESP-KVRADER                        
070200           MOVE '011'              TO RESP-IDMSG-INFO                     
070300         END-IF                                                           
070400       ELSE                                                               
070500         MOVE ZERO                 TO RESP-IDARTNR-NEXT                   
070600       END-IF                                                             
070700       IF WS-KVRADER NOT = ZERO                                           
070800         CONTINUE                                                         
070900       ELSE                                                               
071000         MOVE '027' TO RESP-IDMSG-ERROR                                   
071100       END-IF                                                             
071200     END-IF                                                               
071300     .                                                                    
071400     EJECT                                                                
071500 FA-LAES-GRUNDDATA SECTION.                                               
071600     IF LAES-HUVUD                                                        
071700       MOVE REQU-IDDISTR-KEY   TO W-IDDISTR-UNIK                          
071800       PERFORM IMS-GHU-WDA401-FIRSTHUV                                    
071900*      PERFORM IMS-GET-WDA401-HUV                                         
072000     ELSE                                                                 
072100       MOVE REQU-IDDISTR-KEY   TO W-IDDISTR                               
072200       MOVE REQU-IDRAPPNR-KEY  TO W-IDRAPPNR                              
072300       PERFORM IMS-GET-WDA401                                             
072400       IF SEGMENT-FINNS                                                   
072500         MOVE BUYB-IDKUNDNR    TO RESP-IDKUNDNR                           
072600         IF BUYB-FLPRINT = 'J'                                            
072700           MOVE 'Y'            TO RESP-FLPRINT-BAS                        
072800         ELSE                                                             
072900           MOVE BUYB-FLPRINT   TO RESP-FLPRINT-BAS                        
073000         END-IF                                                           
073100         IF BUYB-FLPERMIT = 'J'                                           
073200           MOVE 'Y'            TO RESP-FLPERMIT-BAS                       
073300         ELSE                                                             
073400           MOVE BUYB-FLPERMIT  TO RESP-FLPERMIT-BAS                       
073500         END-IF                                                           
073600         MOVE BUYB-SUMINVD     TO RESP-SUMINVD                            
073700         MOVE BUYB-REFOBNET    TO RESP-REFOBNET                           
073800         IF REQU-KDPGMACT = 'N'                                           
073900           MOVE REQU-FLMATCH-KEY   TO W-FLMATCH-UNIK                      
074000           MOVE REQU-FLPRGRNS-KEY  TO W-FLPRGRNS-UNIK                     
074100           MOVE REQU-IDARTNR-NEXT  TO W-IDARTNR-UNIK                      
074200           PERFORM IMS-GET-WDA411                                         
074300         ELSE                                                             
074400           PERFORM IMS-GN-WDA411                                          
074500         END-IF                                                           
074600       END-IF                                                             
074700     END-IF                                                               
074800     .                                                                    
074900     EJECT                                                                
075000 G-KOLLA-KDCMD SECTION.                                                   
075100     MOVE +1   TO INDX                                                    
075200     MOVE NEJ TO KDCMD-SW                                                 
075300     IF REQU-IDDISTR-KEY NOT = ALL '+'                                    
075400       IF REQU-IDRAPPNR-KEY  = ALL '+'                                    
075500         IF REQU-KVRADER = ALL '+'                                        
075510           MOVE ZERO                    TO RAD-INDX                       
075511         ELSE                                                             
075520           MOVE REQU-KVRADER            TO RAD-INDX                       
075530         END-IF                                                           
075600         PERFORM UNTIL INDX > RAD-INDX OR KDCMD-FINNS                     
075700           IF REQU-KDCMD (INDX) = 'J'                                     
075800             MOVE JA TO KDCMD-SW                                          
075900             MOVE REQU-IDRAPPNR (INDX)  TO W-IDRAPPNR                     
076000             MOVE REQU-IDDISTR-KEY      TO W-IDDISTR                      
076100           END-IF                                                         
076200           ADD +1 TO INDX                                                 
076300         END-PERFORM                                                      
076400       END-IF                                                             
076500     END-IF                                                               
076600     .                                                                    
076700     EJECT                                                                
076800*    --- DISPATCHER-SEKTIONER                                             
076900 S01-HAEMTA-ANROPSDATA SECTION.                                           
077000                                                                          
077100     MOVE 'GETARG'               TO SUB-KDFUNC                            
077200     MOVE 'CARPARTS.BUYBACK.REPORTS'         TO SUB-ADDISPABS             
077300     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
077400                                                                          
077500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
077600                                                                          
077700     IF SUB-KDRC > 0                                                      
077800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
077900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
078000       DELIMITED BY SIZE INTO FELTEXT                                     
078100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
078200     END-IF                                                               
078300     .                                                                    
078400     SKIP3                                                                
078500 S02-RETURNERA-SVAR SECTION.                                              
078600                                                                          
078700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
078800     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
078900                                                                          
079000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
079100                                                                          
079200     IF SUB-KDRC > 0                                                      
079300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
079400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
079500       DELIMITED BY SIZE INTO FELTEXT                                     
079600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
079700     END-IF                                                               
079800     .                                                                    
079900     EJECT                                                                
080000*                                                                         
080100 S90-SEND-OPEN SECTION.                                                   
080200     MOVE 'S90-SEND-OPEN     ' TO WS-IMS-SECTION                          
080300*    DISPLAY 'S90-SEND-OPEN     '                                         
080400     MOVE 'CARPARTS.DAP.DISTRDOCWEB'      TO SEND-ADDISPABS               
080500     MOVE 'OPEN'                          TO SEND-KDFUNC                  
080600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
080700                         SEND-OPEN-AREA                                   
080800     IF SEND-KDRC > ZERO                                                  
080900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
081000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
081100       DELIMITED BY SIZE INTO FELTEXT                                     
081200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
081300     END-IF                                                               
081400     .                                                                    
081500                                                                          
081600 S90-PUT-HEADER SECTION.                                                  
081700     MOVE 'S90-PUT-HEADER    ' TO WS-IMS-SECTION                          
081800*    DISPLAY 'S90-PUT-HEADER    '                                         
081900*    DISPLAY 'HEAD AREA         ' HDR-AREA                                
082000     MOVE 'PUT'                           TO SEND-KDFUNC                  
082100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
082200     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
082300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
082400                         SEND-KVDLEN                                      
082500                         HDR-AREA                                         
082600     IF SEND-KDRC > ZERO                                                  
082700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
082800       STRING 'WZ01SEND PUT HEAD ERROR RC=' KDRC-DISPLAY                  
082900       DELIMITED BY SIZE INTO FELTEXT                                     
083000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
083100     END-IF                                                               
083200     .                                                                    
083300                                                                          
083400 S90-PUT-DOC-HEAD SECTION.                                                
083500     MOVE 'S90-PUT-DOC-HEAD  ' TO WS-IMS-SECTION                          
083600*    DISPLAY 'S90-PUT-DOC-HEAD  '                                         
083700*    DISPLAY 'DOC HEAD          ' WS-RUB-AREA                             
083800     MOVE 'PUT'                           TO SEND-KDFUNC                  
083900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
084000     MOVE LENGTH OF WS-RUB-AREA           TO SEND-KVDLEN                  
084100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
084200                         SEND-KVDLEN                                      
084300                         WS-RUB-AREA                                      
084400     IF SEND-KDRC > ZERO                                                  
084500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
084600       STRING 'WZ01SEND PUT DOCHEAD ERROR RC=' KDRC-DISPLAY               
084700       DELIMITED BY SIZE INTO FELTEXT                                     
084800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
084900     END-IF                                                               
085000     .                                                                    
085100                                                                          
085200 S90-PUT-DOC-LINE SECTION.                                                
085300     MOVE 'S90-PUT-DOC-LINE  ' TO WS-IMS-SECTION                          
085400*    DISPLAY 'S90-PUT-DOC-LINE  '                                         
085500*    DISPLAY 'DOC LINE          ' WS-RAD-AREA                             
085600     MOVE 'PUT'                           TO SEND-KDFUNC                  
085700     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
085800     MOVE LENGTH OF WS-RAD-AREA           TO SEND-KVDLEN                  
085900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
086000                         SEND-KVDLEN                                      
086100                         WS-RAD-AREA                                      
086200     IF SEND-KDRC > ZERO                                                  
086300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
086400       STRING 'WZ01SEND PUT DOCLINE ERROR RC=' KDRC-DISPLAY               
086500       DELIMITED BY SIZE INTO FELTEXT                                     
086600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
086700     END-IF                                                               
086800     .                                                                    
086900                                                                          
087000 S90-SEND-CLOSE SECTION.                                                  
087100     MOVE 'S90-SEND-CLOSE    ' TO WS-IMS-SECTION                          
087200*    DISPLAY 'S90-SEND-CLOSE    '                                         
087300     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
087400     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
087500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
087600     .                                                                    
087700     EJECT                                                                
087800                                                                          
087900 IMS-INSERT-ALTMSG SECTION.                                               
088000     MOVE '  ' TO GODK-STATUSKODER                                        
088100     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
088200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
088300     PERFORM IMS-STATUSKONTROLL                                           
088400     .                                                                    
088500     EJECT                                                                
088600 IMS-GET-WDA401 SECTION.                                                  
088700     MOVE 'IMS-GET-WDA401    ' TO WS-IMS-SECTION                          
088800*    DISPLAY 'IMS-GET-WDA401    '                                         
088900     STRING 'WDA401  (WDA401KY =' W-WDA401KY-X ')'                        
089000          DELIMITED BY SIZE INTO SSA1                                     
089100     MOVE '  GE' TO GODK-STATUSKODER                                      
089200     CALL CBLTDLI USING GHU WDA4-PCB DLI-IO-WDA401 SSA1                   
089300     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
089400     PERFORM IMS-STATUSKONTROLL                                           
089500     .                                                                    
089600     SKIP3                                                                
089700 IMS-GET-WDA401-HUV SECTION.                                              
089800     MOVE 'IMS-GET-WDA401-HUV' TO WS-IMS-SECTION                          
089900*    DISPLAY 'IMS-GET-WDA401-HUV'                                         
090000     STRING 'WDA401  (IDDISTR  =' W-IDDISTR-X ')'                         
090100                                                                          
090200          DELIMITED BY SIZE INTO SSA1                                     
090300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
090400     CALL CBLTDLI USING GN  WDA4-PCB DLI-IO-WDA401 SSA1                   
090500     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
090600     PERFORM IMS-STATUSKONTROLL                                           
090700**   CALL ABEND                                                           
090800     .                                                                    
090900     SKIP3                                                                
091000 IMS-GHU-WDA401-FIRSTHUV SECTION.                                         
091100     MOVE 'IMS-GET-WDA401-HUV' TO WS-IMS-SECTION                          
091200*    DISPLAY 'IMS-GET-WDA401-FIRSTHUV'                                    
091300     STRING 'WDA401  *F(IDDISTR  =' W-IDDISTR-X ')'                       
091400                                                                          
091500          DELIMITED BY SIZE INTO SSA1                                     
091600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
091700     CALL CBLTDLI USING GHU WDA4-PCB DLI-IO-WDA401 SSA1                   
091800     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
091900     PERFORM IMS-STATUSKONTROLL                                           
092000**   CALL ABEND                                                           
092100     .                                                                    
092200     SKIP3                                                                
092300*                                                                         
092400*IMS-ISRT-WDA401 SECTION.                                                 
092500*                                                                         
092600*    MOVE 'WDA401 ' TO SSA1                                               
092700*    MOVE '  II' TO GODK-STATUSKODER                                      
092800*    CALL CBLTDLI USING ISRT WDA4-PCB DLI-IO-WDA401 SSA1                  
092900*    MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
093000*    PERFORM IMS-STATUSKONTROLL                                           
093100*    .                                                                    
093200*    SKIP3                                                                
093300 IMS-REPL-WDA401 SECTION.                                                 
093400*    DISPLAY 'IMS-REPL-WDA401'                                            
093500     MOVE 'IMS-REPL WDA4 ' TO WS-IMS-SECTION                              
093600     MOVE '  ' TO GODK-STATUSKODER                                        
093700     CALL CBLTDLI USING REPL WDA4-PCB DLI-IO-WDA401                       
093800     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
093900     PERFORM IMS-STATUSKONTROLL                                           
094000     .                                                                    
094100     SKIP3                                                                
094200 IMS-DLET-WDA401 SECTION.                                                 
094300*    DISPLAY 'IMS-DLET-WDA401'                                            
094400     MOVE 'IMS-DLET-WDA4 ' TO WS-IMS-SECTION                              
094500     MOVE '  ' TO GODK-STATUSKODER                                        
094600     CALL CBLTDLI USING DLET WDA4-PCB DLI-IO-WDA401                       
094700     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
094800     PERFORM IMS-STATUSKONTROLL                                           
094900     .                                                                    
095000     EJECT                                                                
095100 IMS-GN-WDA411 SECTION.                                                   
095200     MOVE 'IMS-GN-WDA411 ' TO WS-IMS-SECTION                              
095300*    DISPLAY 'IMS-GN-WDA411 '                                             
095400     MOVE 'WDA411   ' TO SSA1                                             
095500     MOVE '  GE' TO GODK-STATUSKODER                                      
095600     CALL CBLTDLI USING GHNP WDA4-PCB DLI-IO-WDA411 SSA1                  
095700     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
095800     PERFORM IMS-STATUSKONTROLL                                           
095900     .                                                                    
096000     SKIP3                                                                
096100*                                                                         
096200 IMS-GET-WDA411 SECTION.                                                  
096300     MOVE 'IMS-GET-WDA411 ' TO WS-IMS-SECTION                             
096400*    DISPLAY 'IMS-GET-WDA411 '                                            
096500     STRING 'WDA411  (WDA411KY =' W-WDA411KY-X ')'                        
096600     DELIMITED BY SIZE INTO SSA1                                          
096700     MOVE '  GE' TO GODK-STATUSKODER                                      
096800     CALL CBLTDLI USING GHNP WDA4-PCB DLI-IO-WDA411 SSA1                  
096900     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
097000     PERFORM IMS-STATUSKONTROLL                                           
097100     .                                                                    
097200     SKIP3                                                                
097300*                                                                         
097400*IMS-ISRT-WDA411 SECTION.                                                 
097500*                                                                         
097600*    STRING 'WDA401  (IDARTNR  =' W-IDARTNR-X ')'                         
097700*         DELIMITED BY SIZE INTO SSA1                                     
097800*    MOVE 'WDA411 ' TO SSA2                                               
097900*    MOVE '  II' TO GODK-STATUSKODER                                      
098000*    CALL CBLTDLI USING ISRT WDA4-PCB DLI-IO-WDA411 SSA1 SSA2             
098100*    MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
098200*    PERFORM IMS-STATUSKONTROLL                                           
098300*    .                                                                    
098400*    SKIP3                                                                
098500 IMS-REPL-WDA411 SECTION.                                                 
098600*    DISPLAY 'IMS-REPL-WDA411'                                            
098700     MOVE '  ' TO GODK-STATUSKODER                                        
098800     CALL CBLTDLI USING REPL WDA4-PCB DLI-IO-WDA411                       
098900     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
099000     PERFORM IMS-STATUSKONTROLL                                           
099100     .                                                                    
099200     SKIP3                                                                
099300 IMS-DLET-WDA411 SECTION.                                                 
099400*    DISPLAY 'IMS-DLET-WDA411'                                            
099500     MOVE '  ' TO GODK-STATUSKODER                                        
099600     CALL CBLTDLI USING DLET WDA4-PCB DLI-IO-WDA411                       
099700     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
099800     PERFORM IMS-STATUSKONTROLL                                           
099900     .                                                                    
100000     EJECT                                                                
100100 IMS-STATUSKONTROLL SECTION.                                              
100200     SET STATUS-IX TO 1                                                   
100300     SEARCH GODK-STATUS                                                   
100400       AT END                                                             
100500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
100600         DELIMITED BY SIZE INTO FELTEXT                                   
100700         CALL FELLOG                                                      
100800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
100900         CONTINUE                                                         
101000     END-SEARCH                                                           
101100     .                                                                    
101200     EJECT                                                                
