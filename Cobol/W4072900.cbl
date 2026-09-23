000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4072900.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   07/03/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAMN:       CARPARTS.BUYBACK.NEWPRICE                                
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER WDA4 BUY BACK BASEN                                        
001100*        MAN KAN ÄNDRA PÅ MIN VÄRDET ELLER FOB NET                        
001200*        OCG PROGRAMMET ÄNDRAR PÅ BASEN MED NYA VÄRDEN                    
001300*                                                                         
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W40729T                                             
001700*        REQUEST:     W40729I1                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        RESPONSE:    W40729O1                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W4072900'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 77  INDX                        PIC 9(3)    VALUE ZERO.                  
004100 77  MAX-INDX                    PIC 9(3)    VALUE 500.                   
004200 77  RAD-INDX                    PIC 9(3)    VALUE 500.                   
004300 77  WS-KVRADER                  PIC 9(3)    VALUE 0.                     
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004600 77  WS-RUB-AREA                 PIC X(100)  VALUE SPACE.                 
004700 77  WS-RAD-AREA                 PIC X(100)  VALUE SPACE.                 
004800 77  WS-IMS-SECTION              PIC X(80)   VALUE SPACE.                 
004900 77  WS-SECTION                  PIC X(80)   VALUE SPACE.                 
005000 77  WS-FLMATCH                  PIC X       VALUE SPACE.                 
005100 77  WS-FLPRGRNS                 PIC X       VALUE SPACE.                 
005200 77  WS-SUMINVD                  PIC 999     VALUE ZERO.                  
005300 77  WS-REFOBNET                 PIC 999     VALUE ZERO.                  
005400 77  WS-PRARTNTO-NEW          PIC S9(7)V9(2) COMP-3.                      
005500 77  WS-PRARTNTO-TOT          PIC S9(7)V9(2) COMP-3.                      
005600 77  WS-IDARTNR               PIC  9(8).                                  
005700                                                                          
005800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005900     88  NYCKLAR-OK                          VALUE 'J'.                   
006000     88  NYCKLAR-FEL                         VALUE 'N'.                   
006100                                                                          
006200 77  LAES-HUVUD-SW               PIC X       VALUE 'N'.                   
006300     88  LAES-HUVUD                          VALUE 'J'.                   
006400                                                                          
006500 77  LAES-RADER-SW               PIC X       VALUE 'N'.                   
006600     88  LAES-RADER                          VALUE 'J'.                   
006700                                                                          
006800 77  OK-SW                       PIC X       VALUE 'N'.                   
006900     88  ALLT-OK                             VALUE 'J'.                   
007000     88  INTE-OK                             VALUE 'N'.                   
007100                                                                          
007200 77  KDCMD-SW                    PIC X       VALUE 'N'.                   
007300     88  KDCMD-FINNS                         VALUE 'J'.                   
007400     88  KDCMD-SAKNAS                        VALUE 'N'.                   
007500                                                                          
007600 77  NYTT-PRIS-SW                PIC X       VALUE 'N'.                   
007700     88  GAMMALT-PRIS                        VALUE 'N'.                   
007800     88  NYTT-PRIS                           VALUE 'J'.                   
007900                                                                          
008000 01  DAGENS-DATUM                PIC 9(6).                                
008100 01  DAGENS-KLOCKA               PIC 9(9).                                
008200     EJECT                                                                
008300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008400 01  GENERELLA-SUBPROGRAM.                                                
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009000                                                                          
009100     SKIP2                                                                
009200     03  W-WDA401KY-X.                                                    
009300         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
009400         05  W-IDRAPPNR          PIC 9(7)    VALUE ZERO.                  
009500     03    WDA401KY-MIN-X.                                                
009600         05  W-IDDISTR-MIN       PIC S9(5)   VALUE +0   COMP-3.           
009700         05  W-IDRAPPNR-MIN      PIC 9(7)    VALUE ZERO.                  
009800     03    WDA401KY-MAX-X.                                                
009900         05  W-IDDISTR-MAX       PIC S9(5)   VALUE +0   COMP-3.           
010000         05  W-IDRAPPNR-MAX      PIC 9(7)    VALUE 9999999.               
010100     03    WDA401KY-DEL-X.                                                
010200         05  W-IDDISTR-DEL       PIC S9(5)   VALUE +0   COMP-3.           
010300         05  W-IDRAPPNR-DEL      PIC 9(7)    VALUE 9999999.               
010400     03  W-IDARTNR-X.                                                     
010500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010600     03  W-WDA411KY-X.                                                    
010700         05  W-FLMATCH-UNIK      PIC X       VALUE SPACE.                 
010800         05  W-FLPRGRNS-UNIK     PIC X       VALUE SPACE.                 
010900         05  W-IDARTNR-UNIK      PIC S9(9)   VALUE ZERO COMP-3.           
011000*    --- PARAMETRAR TILL ABEND                                            
011100                                                                          
011200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011500     SKIP3                                                                
011600 01  MESSAGE-CODES.                                                       
011700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
011800     EJECT                                                                
011900*                                                                         
012000                                                                          
012100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
012200     SKIP3                                                                
012300*    --- AREOR FÖR KOMMUNIKATION                                          
012400*01  -COPY WZ01SUB                                                        
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012700     SKIP3                                                                
012800 01  REQU-AREA.                                                           
012900*    03  -COPY WZ01REQU                                                   
013000*    03  -COPY W40729I1                                                   
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
013300     SKIP3                                                                
013400 01  RESP-AREA.                                                           
013500*    03  -COPY WZ01RESP                                                   
013600*    03  -COPY W40729O1                                                   
013700     EJECT                                                                
013800 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
013900*01  -COPY WZ01SEND                                                       
014000     EJECT                                                                
014100 01  UT-AREA-START               PIC X(24)   VALUE                        
014200                                 'UT-AREA-START '.                        
014300 01  HDR-AREA.                                                            
014400*    03  -COPY WZ01REQU -PRE HDR-                                         
014500*    03  -COPY WZ04HDR                                                    
014600     EJECT                                                                
014700                                                                          
014800     EJECT                                                                
014900                                                                          
015000*    --- STATUS-KOD FRÅN IMS                                              
015100 01  STATUS-WS                   PIC XX.                                  
015200     88  STATUS-OK                           VALUE '  '.                  
015300     88  SEGMENT-FINNS                       VALUE '  '.                  
015400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015500     88  BASEN-SLUT                          VALUE 'GB'.                  
015600     SKIP2                                                                
015700 01  GODK-STATUSKODER.                                                    
015800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015900     SKIP3                                                                
016000 01  SSA1                        PIC X(128).                              
016100 01  SSA2                        PIC X(128).                              
016200     EJECT                                                                
016300*    --- IMS FUNKTIONSKODER                                               
016400*01  -COPY W0003                                                          
016500     EJECT                                                                
016600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA401'.                      
016700 01  DLI-IO-WDA401.                                                       
016800*    03  -COPY WDA401                                                     
016900     EJECT                                                                
017000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA411'.                      
017100 01  DLI-IO-WDA411.                                                       
017200*    03  -COPY WDA411                                                     
017300     EJECT                                                                
017400 LINKAGE SECTION.                                                         
017500                                                                          
017600*01  -COPY W0009  -PRE MSG-                                               
017700     EJECT                                                                
017800*01  -COPY W0008  -PRE WDA4-                                              
017900     05  FILLER                  PIC X.                                   
018000*01  -COPY W0008  -PRE WDA4X-                                             
018100     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018300 PROCEDURE DIVISION  USING MSG-PCB  WDA4-PCB WDA4X-PCB.                   
018400 MAIN SECTION.                                                            
018500     ENTRY 'DLITCBL' USING MSG-PCB  WDA4-PCB WDA4X-PCB.                   
018600                                                                          
018700     PERFORM S01-HAEMTA-ANROPSDATA                                        
018800     IF SUB-KDRC = 0                                                      
018900       PERFORM A-INIT                                                     
019000       PERFORM B-KOLLA-NYCKLAR                                            
019100       IF NYCKLAR-OK                                                      
019200         IF REQU-KDPGMACT = 'S' OR 'N'                                    
019300           PERFORM F-LAES-VISA-INFO                                       
019400         ELSE                                                             
019500           IF REQU-KDPGMACT = 'E'                                         
019600             PERFORM G-KOLLA-INPUT                                        
019700             IF ALLT-OK                                                   
019800               PERFORM H-UPPDATERA-WDA4                                   
019900             END-IF                                                       
020000           END-IF                                                         
020100*******   CALL ABEND                                                      
020200         END-IF                                                           
020300       END-IF                                                             
020400       IF ALLT-OK AND REQU-KDPGMACT = 'E'                                 
020500         MOVE SPACE TO RESP-W40729O1                                      
020600         MOVE REQU-IDDISTR-KEY    TO RESP-IDDISTR-KEY                     
020700         MOVE REQU-IDRAPPNR-KEY   TO RESP-IDRAPPNR-KEY                    
020800         PERFORM F-LAES-VISA-INFO                                         
020900         MOVE '001' TO RESP-IDMSG-INFO                                    
021000       END-IF                                                             
021100       PERFORM S02-RETURNERA-SVAR                                         
021200     END-IF                                                               
021300                                                                          
021400                                                                          
021500*    PERFORM Z-FINIT                                                      
021600     MOVE ZERO TO RETURN-CODE                                             
021700     GOBACK                                                               
021800     .                                                                    
021900     EJECT                                                                
022000 A-INIT SECTION.                                                          
022100                                                                          
022200     MOVE ALL '+' TO RESP-AREA                                            
022300     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
022400                     RESP-IDMSG-INFO                                      
022500                     RESP-IDELMT-ERROR                                    
022600     MOVE '001'   TO RESP-IDMSGVER                                        
022700     IF REQU-KDPGMACT = 'S'                                               
022800       MOVE SPACE TO RESP-W40729O1                                        
022900       MOVE ZERO    TO RESP-KVRADER                                       
023000     END-IF                                                               
023100     MOVE LOW-VALUE  TO WDA401KY-MIN-X                                    
023200     MOVE HIGH-VALUE TO WDA401KY-MAX-X                                    
023300     ACCEPT DAGENS-KLOCKA FROM TIME                                       
023400     ACCEPT DAGENS-DATUM  FROM DATE                                       
023500*    DISPLAY 'W4072900 ' DAGENS-DATUM ' ' DAGENS-KLOCKA                   
023600     .                                                                    
023700     EJECT                                                                
023800 B-KOLLA-NYCKLAR SECTION.                                                 
023900                                                                          
024000     MOVE JA TO NYCKLAR-SW                                                
024100     IF REQU-IDDISTR-KEY NOT = ALL '+'                                    
024200       IF REQU-IDDISTR-KEY NUMERIC                                        
024300         MOVE REQU-IDDISTR-KEY TO RESP-IDDISTR-KEY                        
024400                                  W-IDDISTR                               
024500*                                 W-IDDISTR-MIN                           
024600*                                 W-IDDISTR-MAX                           
024700       ELSE                                                               
024800         MOVE NEJ TO NYCKLAR-SW                                           
024900       END-IF                                                             
025000     END-IF                                                               
025100                                                                          
025200     IF REQU-IDRAPPNR-KEY NOT = ALL '+'                                   
025300      IF REQU-IDRAPPNR-KEY NUMERIC                                        
025400        MOVE REQU-IDRAPPNR-KEY TO RESP-IDRAPPNR-KEY                       
025500                                  W-IDRAPPNR                              
025600*                                 W-IDRAPPNR-MIN                          
025700*                                 W-IDRAPPNR-MAX                          
025800      ELSE                                                                
025900        MOVE NEJ TO NYCKLAR-SW                                            
026000      END-IF                                                              
026100     END-IF                                                               
026200                                                                          
026300     IF REQU-IDRAPPNR-KEY  = ALL '+' AND                                  
026400        REQU-IDDISTR-KEY   = ALL '+'                                      
026500        MOVE NEJ TO NYCKLAR-SW                                            
026600     END-IF                                                               
026700                                                                          
026800     IF NYCKLAR-OK                                                        
026900       IF REQU-IDDISTR-KEY NUMERIC AND REQU-IDRAPPNR-KEY NUMERIC          
027000         MOVE JA TO LAES-RADER-SW                                         
027100       ELSE                                                               
027200         MOVE JA TO LAES-HUVUD-SW                                         
027300       END-IF                                                             
027400     END-IF                                                               
027500                                                                          
027600     IF NYCKLAR-FEL                                                       
027700       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
027800     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 F-LAES-VISA-INFO SECTION.                                                
028200     MOVE 'F-LAES '   TO      WS-SECTION                                  
028300     MOVE ZERO TO  INDX                                                   
028400     PERFORM FA-LAES-GRUNDDATA                                            
028500                                                                          
028600     IF SEGMENT-SAKNAS                                                    
028700       MOVE '027' TO RESP-IDMSG-ERROR                                     
028800     ELSE                                                                 
028900       ADD +1 TO INDX                                                     
029000       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
029100         IF LAES-RADER                                                    
029200           MOVE BART-IDARTNR       TO RESP-IDARTNR-RAD   (INDX)           
029300           MOVE BART-BEART         TO RESP-BEART-RAD     (INDX)           
029400           MOVE BART-KVANTAL       TO RESP-KVANTAL-RAD   (INDX)           
029500           MOVE BART-PRARTNTO-TOT  TO RESP-PRARTNTO-TOT-RAD(INDX)         
029600           MOVE BART-KDVALISO      TO RESP-KDVALISO-RAD  (INDX)           
029700           MOVE BART-PRARTNTO-NEW  TO RESP-PRARTNTO-NEW-RAD(INDX)         
029800           IF BART-FLMATCH = 'J'                                          
029900             MOVE 'Y'              TO RESP-FLMATCH-RAD(INDX)              
030000           ELSE                                                           
030100             MOVE BART-FLMATCH     TO RESP-FLMATCH-RAD(INDX)              
030200           END-IF                                                         
030300           IF BART-FLPRGRNS = 'J'                                         
030400             MOVE 'Y'              TO RESP-FLPRGRNS-RAD(INDX)             
030500           ELSE                                                           
030600             MOVE BART-FLPRGRNS    TO RESP-FLPRGRNS-RAD(INDX)             
030700           END-IF                                                         
030800           MOVE INDX TO WS-KVRADER                                        
030900           ADD +1 TO INDX                                                 
031000          PERFORM IMS-GHNP-WDA411                                         
031100         END-IF                                                           
031200       END-PERFORM                                                        
031300       MOVE WS-KVRADER TO RESP-KVRADER                                    
031400       IF INDX > MAX-INDX                                                 
031500         IF LAES-RADER                                                    
031600           IF SEGMENT-FINNS                                               
031700             MOVE BART-IDARTNR       TO RESP-IDARTNR-NEXT                 
031800             MOVE BART-FLMATCH       TO RESP-FLMATCH-NEXT                 
031900             MOVE BART-FLPRGRNS      TO RESP-FLPRGRNS-NEXT                
032000             MOVE 500                TO RESP-KVRADER                      
032100             MOVE '011'              TO RESP-IDMSG-INFO                   
032200           END-IF                                                         
032300         END-IF                                                           
032400       END-IF                                                             
032500       IF WS-KVRADER NOT = ZERO                                           
032600         CONTINUE                                                         
032700       ELSE                                                               
032800         MOVE '027' TO RESP-IDMSG-ERROR                                   
032900       END-IF                                                             
033000     END-IF                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 FA-LAES-GRUNDDATA SECTION.                                               
033400     MOVE 'FA-LAES'  TO       WS-SECTION                                  
033500     MOVE REQU-IDDISTR-KEY   TO W-IDDISTR                                 
033600     MOVE REQU-IDRAPPNR-KEY  TO W-IDRAPPNR                                
033700     PERFORM IMS-GET-WDA401                                               
033800     IF SEGMENT-FINNS                                                     
033900       MOVE BUYB-IDKUNDNR    TO RESP-IDKUNDNR-BAS                         
034000       IF BUYB-FLPRINT = 'J'                                              
034100         MOVE 'Y'            TO RESP-FLPRINT-BAS                          
034200       ELSE                                                               
034300         MOVE BUYB-FLPRINT   TO RESP-FLPRINT-BAS                          
034400       END-IF                                                             
034500       IF BUYB-FLPERMIT = 'J'                                             
034600         MOVE 'Y'            TO RESP-FLPERMIT-BAS                         
034700       ELSE                                                               
034800         MOVE BUYB-FLPERMIT  TO RESP-FLPERMIT-BAS                         
034900       END-IF                                                             
035000                                                                          
035100       MOVE BUYB-SUMINVD     TO RESP-SUMINVD-BAS                          
035200       MOVE BUYB-REFOBNET    TO RESP-REFOBNET-BAS                         
035300       IF REQU-KDPGMACT = 'N'                                             
035400         MOVE REQU-IDARTNR-NEXT     TO W-IDARTNR-UNIK                     
035500         MOVE REQU-FLMATCH-NEXT     TO W-FLMATCH-UNIK                     
035600         MOVE REQU-FLPRGRNS-NEXT    TO W-FLPRGRNS-UNIK                    
035700         PERFORM IMS-GET-WDA411                                           
035800       ELSE                                                               
035900         PERFORM IMS-GHNP-WDA411                                          
036000       END-IF                                                             
036100     END-IF                                                               
036200     .                                                                    
036300     EJECT                                                                
036400 G-KOLLA-INPUT SECTION.                                                   
036500     MOVE 'G-KOLLA'  TO       WS-SECTION                                  
036600     MOVE JA TO OK-SW                                                     
036700     IF REQU-SUMINVD-IN NOT = ALL '+'                                     
036800       IF REQU-SUMINVD-IN NUMERIC                                         
036900         MOVE REQU-SUMINVD-IN  TO WS-SUMINVD                              
037000*        DISPLAY ' NYTT MIN PRICE ' WS-SUMINVD                            
037100       ELSE                                                               
037200         MOVE NEJ TO OK-SW                                                
037300         MOVE '024'            TO RESP-IDMSG-ERROR                        
037400         MOVE 'SUMINVD '       TO RESP-IDELMT-ERROR                       
037500       END-IF                                                             
037600     END-IF                                                               
037700                                                                          
037800     IF REQU-REFOBNET-IN NOT = ALL '+'                                    
037900       IF REQU-REFOBNET-IN NUMERIC                                        
038000         MOVE REQU-REFOBNET-IN TO WS-REFOBNET                             
038100*        DISPLAY ' NYTT REFOBNET ' WS-REFOBNET                            
038200       ELSE                                                               
038300         MOVE NEJ TO OK-SW                                                
038400         MOVE '024'            TO RESP-IDMSG-ERROR                        
038500         MOVE 'REFOBNET'       TO RESP-IDELMT-ERROR                       
038600       END-IF                                                             
038700     END-IF                                                               
038800     IF REQU-SUMINVD-IN = ALL '+' AND                                     
038900        REQU-REFOBNET-IN = ALL '+'                                        
039000       MOVE ALL '+'            TO RESP-W40729O1                           
039100       MOVE '014'              TO RESP-IDMSG-ERROR                        
039200       MOVE NEJ TO OK-SW                                                  
039300     ELSE                                                                 
039400       MOVE REQU-IDDISTR-KEY   TO W-IDDISTR                               
039500       MOVE REQU-IDRAPPNR-KEY  TO W-IDRAPPNR                              
039600       PERFORM IMS-GET-WDA401                                             
039700       IF REQU-SUMINVD-IN NOT = ALL '+'                                   
039800         IF REQU-SUMINVD-IN = BUYB-SUMINVD                                
039900           MOVE '031'          TO RESP-IDMSG-ERROR                        
040000           MOVE 'SUMINVD'      TO RESP-IDELMT-ERROR                       
040100           MOVE NEJ TO OK-SW                                              
040200         END-IF                                                           
040300       END-IF                                                             
040400       IF REQU-REFOBNET-IN NOT = ALL '+'                                  
040500         IF REQU-REFOBNET-IN = BUYB-REFOBNET                              
040600           MOVE '031'          TO RESP-IDMSG-ERROR                        
040700           MOVE 'REFOBNET'     TO RESP-IDELMT-ERROR                       
040800           MOVE NEJ TO OK-SW                                              
040900         END-IF                                                           
041000         IF REQU-REFOBNET-IN > 100                                        
041100           MOVE '023'          TO RESP-IDMSG-ERROR                        
041200           MOVE 'REFOBNET'     TO RESP-IDELMT-ERROR                       
041300           MOVE NEJ TO OK-SW                                              
041400         END-IF                                                           
041500       END-IF                                                             
041600     END-IF                                                               
041700     .                                                                    
041800     EJECT                                                                
041900 H-UPPDATERA-WDA4 SECTION.                                                
042000     MOVE 'H-UPPDATERA'  TO   WS-SECTION                                  
042100     MOVE NEJ TO NYTT-PRIS-SW                                             
042200     MOVE JA  TO OK-SW                                                    
042300     MOVE REQU-IDDISTR-KEY   TO W-IDDISTR                                 
042400     MOVE REQU-IDRAPPNR-KEY  TO W-IDRAPPNR                                
042500     PERFORM IMS-GET-WDA401                                               
042600     IF SEGMENT-FINNS                                                     
042700       IF WS-REFOBNET > ZERO                                              
042800         MOVE JA TO NYTT-PRIS-SW                                          
042900         MOVE WS-REFOBNET    TO BUYB-REFOBNET                             
043000         MOVE 'N'            TO BUYB-FLPRINT                              
043100       ELSE                                                               
043200         MOVE BUYB-REFOBNET  TO WS-REFOBNET                               
043300       END-IF                                                             
043400       IF WS-SUMINVD > ZERO                                               
043500         MOVE WS-SUMINVD     TO BUYB-SUMINVD                              
043600         MOVE 'N'            TO BUYB-FLPRINT                              
043700       ELSE                                                               
043800         MOVE BUYB-SUMINVD   TO WS-SUMINVD                                
043900       END-IF                                                             
044000       PERFORM IMS-REPL-WDA401                                            
044100     ELSE                                                                 
044200       MOVE NEJ TO OK-SW                                                  
044300       MOVE '024'            TO RESP-IDMSG-ERROR                          
044400       MOVE 'IDRAPPNR'       TO RESP-IDELMT-ERROR                         
044500     END-IF                                                               
044600                                                                          
044700*    PERFORM IMS-GET-WDA401                                               
044800*    IF SEGMENT-FINNS                                                     
044900     IF ALLT-OK                                                           
045000       PERFORM IMS-GHNP-WDA411                                            
045100*    END-IF                                                               
045200       IF SEGMENT-FINNS                                                   
045300         PERFORM UNTIL SEGMENT-SAKNAS                                     
045400          IF NYTT-PRIS                                                    
045500            IF BART-PRARTNTO > ZERO                                       
045600              COMPUTE WS-PRARTNTO-NEW = BART-PRARTNTO *                   
045700                                       (WS-REFOBNET / 100)                
045800            ELSE                                                          
045900             COMPUTE WS-PRARTNTO-NEW = BART-PRARTNTO-LOC *                
046000                                       (WS-REFOBNET / 100)                
046100             MOVE WS-PRARTNTO-NEW     TO BART-PRARTNTO-NEW                
046200            END-IF                                                        
046300                                                                          
046400            COMPUTE WS-PRARTNTO-TOT   = WS-PRARTNTO-NEW *                 
046500                                        BART-KVANTAL                      
046600                                                                          
046700            MOVE WS-PRARTNTO-TOT      TO BART-PRARTNTO-TOT                
046800            MOVE WS-PRARTNTO-NEW      TO BART-PRARTNTO-NEW                
046900          END-IF                                                          
047000                                                                          
047100          IF BART-PRARTNTO-TOT > WS-SUMINVD                               
047200            MOVE 'J'                 TO WS-FLPRGRNS                       
047300          ELSE                                                            
047400            MOVE 'N'                 TO WS-FLPRGRNS                       
047500          END-IF                                                          
047600          IF WS-FLPRGRNS NOT = BART-FLPRGRNS                              
047700            PERFORM IMS-DLET-WDA411                                       
047800            MOVE WS-FLPRGRNS         TO BART-FLPRGRNS                     
047900            PERFORM IMS-ISRT-WDA411                                       
048000                                                                          
048100            MOVE BART-FLMATCH        TO W-FLMATCH-UNIK                    
048200            MOVE BART-FLPRGRNS       TO W-FLPRGRNS-UNIK                   
048300            MOVE BART-IDARTNR        TO W-IDARTNR-UNIK                    
048400          ELSE                                                            
048500            IF NYTT-PRIS                                                  
048600              PERFORM IMS-REPL-WDA411                                     
048700            END-IF                                                        
048800          END-IF                                                          
048900          PERFORM IMS-GHNP-WDA411                                         
049000                                                                          
049100         END-PERFORM                                                      
049200       ELSE                                                               
049300         MOVE NEJ TO OK-SW                                                
049400         MOVE '027'            TO RESP-IDMSG-ERROR                        
049500*        MOVE 'ROWS'           TO RESP-IDELMT-ERROR                       
049600       END-IF                                                             
049700     END-IF                                                               
049800     .                                                                    
049900     EJECT                                                                
050000*    --- DISPATCHER-SEKTIONER                                             
050100 S01-HAEMTA-ANROPSDATA SECTION.                                           
050200     MOVE 'S01-HAEMTA '   TO      WS-IMS-SECTION                          
050300     MOVE 'GETARG'               TO SUB-KDFUNC                            
050400     MOVE 'CARPARTS.BUYBACK.NEWPRICE'        TO SUB-ADDISPABS             
050500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
050600                                                                          
050700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
050800                                                                          
050900     IF SUB-KDRC > 0                                                      
051000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
051100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
051200       DELIMITED BY SIZE INTO FELTEXT                                     
051300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
051400     END-IF                                                               
051500     .                                                                    
051600     SKIP3                                                                
051700 S02-RETURNERA-SVAR SECTION.                                              
051800     MOVE 'S02-RETUR  '  TO       WS-IMS-SECTION                          
051900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
052000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
052100                                                                          
052200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
052300                                                                          
052400     IF SUB-KDRC > 0                                                      
052500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
052600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
052700       DELIMITED BY SIZE INTO FELTEXT                                     
052800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200*                                                                         
053300                                                                          
053400 IMS-GET-WDA401 SECTION.                                                  
053500     MOVE 'IMS-GET-WDA401    ' TO WS-IMS-SECTION                          
053600*    DISPLAY 'IMS-GET-WDA401    '                                         
053700     STRING 'WDA401  (WDA401KY =' W-WDA401KY-X ')'                        
053800          DELIMITED BY SIZE INTO SSA1                                     
053900     MOVE '  GE' TO GODK-STATUSKODER                                      
054000     CALL CBLTDLI USING GHU WDA4-PCB DLI-IO-WDA401 SSA1                   
054100     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
054200     PERFORM IMS-STATUSKONTROLL                                           
054300     .                                                                    
054400     SKIP3                                                                
054500 IMS-GET-WDA401-HUV SECTION.                                              
054600     MOVE 'IMS-GET-WDA401-HUV' TO WS-IMS-SECTION                          
054700     STRING 'WDA401  (WDA401KY=>' WDA401KY-MIN-X                          
054800                    '&WDA401KY=<' WDA401KY-MAX-X ')'                      
054900          DELIMITED BY SIZE INTO SSA1                                     
055000     MOVE '  GE' TO GODK-STATUSKODER                                      
055100     CALL CBLTDLI USING GN  WDA4-PCB DLI-IO-WDA401 SSA1                   
055200     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
055300     PERFORM IMS-STATUSKONTROLL                                           
055400**   CALL ABEND                                                           
055500     .                                                                    
055600     SKIP3                                                                
055700*                                                                         
055800*IMS-ISRT-WDA401 SECTION.                                                 
055900*                                                                         
056000*    MOVE 'WDA401 ' TO SSA1                                               
056100*    MOVE '  II' TO GODK-STATUSKODER                                      
056200*    CALL CBLTDLI USING ISRT WDA4-PCB DLI-IO-WDA401 SSA1                  
056300*    MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
056400*    PERFORM IMS-STATUSKONTROLL                                           
056500*    .                                                                    
056600*    SKIP3                                                                
056700 IMS-REPL-WDA401 SECTION.                                                 
056800     MOVE 'IMS-REPL WDA4 ' TO WS-IMS-SECTION                              
056900     MOVE '  ' TO GODK-STATUSKODER                                        
057000     CALL CBLTDLI USING REPL WDA4-PCB DLI-IO-WDA401                       
057100     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
057200     PERFORM IMS-STATUSKONTROLL                                           
057300     .                                                                    
057400     SKIP3                                                                
057500 IMS-DLET-WDA401 SECTION.                                                 
057600     MOVE 'IMS-DLET-WDA4 ' TO WS-IMS-SECTION                              
057700     MOVE '  ' TO GODK-STATUSKODER                                        
057800     CALL CBLTDLI USING DLET WDA4-PCB DLI-IO-WDA401                       
057900     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
058000     PERFORM IMS-STATUSKONTROLL                                           
058100     .                                                                    
058200     EJECT                                                                
058300 IMS-GHNP-WDA411 SECTION.                                                 
058400     MOVE 'IMS-GHNP-WDA411'  TO WS-IMS-SECTION                            
058500*    DISPLAY 'IMS-GHNP-WDA411'                                            
058600     MOVE 'WDA411   ' TO SSA1                                             
058700     MOVE '  GE' TO GODK-STATUSKODER                                      
058800     CALL CBLTDLI USING GHNP WDA4-PCB DLI-IO-WDA411 SSA1                  
058900     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
059000     PERFORM IMS-STATUSKONTROLL                                           
059100     .                                                                    
059200     SKIP3                                                                
059300*                                                                         
059400 IMS-GET-WDA411 SECTION.                                                  
059500     MOVE 'IMS-GET-WDA411 ' TO WS-IMS-SECTION                             
059600                                                                          
059700     STRING 'WDA411  (WDA411KY =' W-WDA411KY-X ')'                        
059800     DELIMITED BY SIZE INTO SSA1                                          
059900     MOVE '  GE' TO GODK-STATUSKODER                                      
060000     CALL CBLTDLI USING GHNP WDA4-PCB DLI-IO-WDA411 SSA1                  
060100     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
060200     PERFORM IMS-STATUSKONTROLL                                           
060300     .                                                                    
060400     SKIP3                                                                
060500*                                                                         
060600 IMS-ISRT-WDA411 SECTION.                                                 
060700     MOVE 'IMS-ISRT-WDA411 ' TO WS-IMS-SECTION                            
060800     STRING 'WDA401  (WDA401KY =' W-WDA401KY-X ')'                        
060900          DELIMITED BY SIZE INTO SSA1                                     
061000     MOVE 'WDA411 ' TO SSA2                                               
061100     MOVE '  ' TO GODK-STATUSKODER                                        
061200     CALL CBLTDLI USING ISRT WDA4X-PCB DLI-IO-WDA411 SSA1 SSA2            
061300     MOVE WDA4X-STATUS-CODE TO STATUS-WS                                  
061400     PERFORM IMS-STATUSKONTROLL                                           
061500*    MOVE BART-IDARTNR TO WS-IDARTNR                                      
061600*    DISPLAY ' IMS-ISRT-WDA411 ' WS-IDARTNR                               
061700     .                                                                    
061800     SKIP3                                                                
061900 IMS-REPL-WDA411 SECTION.                                                 
062000     MOVE 'IMS-REPL WDA411 ' TO WS-IMS-SECTION                            
062100     MOVE '  ' TO GODK-STATUSKODER                                        
062200     CALL CBLTDLI USING REPL WDA4-PCB DLI-IO-WDA411                       
062300     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
062400     PERFORM IMS-STATUSKONTROLL                                           
062500*    MOVE BART-IDARTNR TO WS-IDARTNR                                      
062600*    DISPLAY ' IMS-REPL-WDA411 ' WS-IDARTNR                               
062700     .                                                                    
062800     SKIP3                                                                
062900 IMS-DLET-WDA411 SECTION.                                                 
063000     MOVE 'IMS-DLET-WDA411 ' TO WS-IMS-SECTION                            
063100*    MOVE BART-IDARTNR TO WS-IDARTNR                                      
063200*    DISPLAY ' IMS-DLET-WDA411 ' WS-IDARTNR                               
063300                                                                          
063400     MOVE '  ' TO GODK-STATUSKODER                                        
063500     CALL CBLTDLI USING DLET WDA4-PCB DLI-IO-WDA411                       
063600     MOVE WDA4-STATUS-CODE TO STATUS-WS                                   
063700     PERFORM IMS-STATUSKONTROLL                                           
063800     .                                                                    
063900     EJECT                                                                
064000 IMS-STATUSKONTROLL SECTION.                                              
064100     SET STATUS-IX TO 1                                                   
064200     SEARCH GODK-STATUS                                                   
064300       AT END                                                             
064400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
064500         DELIMITED BY SIZE INTO FELTEXT                                   
064600         CALL FELLOG                                                      
064700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
064800         CONTINUE                                                         
064900     END-SEARCH                                                           
065000     .                                                                    
065100     EJECT                                                                
