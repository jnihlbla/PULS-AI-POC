000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL013900.                                                
000400 AUTHOR.         GÖRAN KJELLSON   GUIDE                                   
000500 DATE-WRITTEN.   JULI 2005                                                
000600 DATE-COMPILED.                                                           
000700*    NAME:       'CARPARTS.LDC.DISCREPANCYQUERYPART'                      
000800*                                                                         
000810*                                                                         
000900*        WL013900 PROGRAM IS A REPLICA OF W4072400 PROGRAM                
001000*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001100*                                                                         
001200*                                                                         
001300*    FUNKTION:                                                            
001400*        VISAR INFO OM EN ARTIKELS FÖREKOMST PÅ                           
001500*        LEVERANSANMÄRKNINGSREGISTRET WDA2.                               
001600*                                                                         
001700*        PROGRAMMET LÄSER      WLKREE (WDA2)                              
001800*        PROGRAMMET LÄSER      WLKREI (WDA2)                              
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: WL0139T                                             
002200*        MID:         WL0139I1                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         WL0139O1                                            
002600*                                                                         
002700*    E'TRACKER: 5277475  DATED 2007-07-05 RÄTTA FEL                       
002710*    E'TRACKER: 10296404 DATED 2017-01-26 RETURNS FROM CA TO US           
002720*    E'TRACKER: 10302968 DATED 2017-07-03 GENERIC SOLUTION IDFTG          
002800*                                                                         
002900*                                                                         
003000                                                                          
003100                                                                          
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600*    -COPY WY2000W1                                                       
003700     SKIP3                                                                
003800 77  IDPGM                       PIC X(08)  VALUE 'WL013900'.             
003900 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
004000 77  JA                          PIC X      VALUE 'J'.                    
004100 77  YES                         PIC X      VALUE 'Y'.                    
004200 77  NEJ                         PIC X      VALUE 'N'.                    
004300 77  FEL                         PIC X      VALUE 'F'.                    
004400 77  WS-EXECUTE                  PIC X      VALUE 'E'.                    
004500 77  WS-RETURN-PERMISSION        PIC X      VALUE 'R'.                    
004600 77  WS-DISCREPANCY-LINE         PIC X      VALUE 'D'.                    
004700 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
004800 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
004900 77  KDRC-DISPLAY                PIC Z(5).                                
005000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005200 77  INDX                        PIC S9(4)  VALUE +0    COMP-3.           
005300 77  ANTAL-FLCMD                 PIC  9(4)  VALUE 0.                      
005400 77  REQU-FLCMD-INDX             PIC S9(4)  VALUE +0    COMP-3.           
005500 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP-3.           
005600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP-3.           
005700 77  WS-KVRADER                  PIC S9(3)  VALUE +0    COMP-3.           
005800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005900 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
006000 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
006100 77  W-IDDISTR-SEC               PIC 9(4).                                
006200 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
006300 77  WS-KDANMORS                 PIC X(2)    VALUE SPACE.                 
006400 77  WS-KDKREBEH                 PIC X(3)    VALUE SPACE.                 
006500 77  WS-IDRADNR                  PIC 9(4)    VALUE ZERO.                  
006600                                                                          
006700 77  WS-IDELMT-ERROR             PIC X(16).                               
006800 77  WS-IDMSG-ERROR              PIC X(03).                               
006900 77  WS-IDMSG-INFO               PIC X(03).                               
007000                                                                          
007100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007200     88  NYCKLAR-OK                          VALUE 'J'.                   
007300     88  NYCKLAR-FEL                         VALUE 'N'.                   
007400                                                                          
007500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007600     88  INDATA-OK                           VALUE 'J'.                   
007700     88  INDATA-FEL                          VALUE 'N'.                   
007800                                                                          
007900 77  P-TO-P-SW                   PIC X       VALUE 'N'.                   
008000     88  HOPP-OK                             VALUE 'J'.                   
008100     88  HOPP-FEL                            VALUE 'F'.                   
008200     88  EJ-BILD-BYTE                        VALUE 'N'.                   
008300                                                                          
008400     EJECT                                                                
008500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008600 01  GENERELLA-SUBPROGRAM.                                                
008700     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
009100     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
009200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
009300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009400     EJECT                                                                
009500*    ---  LÄNKAREA TILL W418OKOD                                          
009600 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
009700                                                                          
009800*01 -COPY W418OKOD           -PRE OKOD-.                                  
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
010100 01  FILLER                      PIC X(16)   VALUE 'W006PRT '.            
010200                                                                          
010300*01  -COPY W006PRT                                                        
010400     EJECT                                                                
010500                                                                          
010600*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
010700*   -COPY WSECAREA                                                        
010800     EJECT                                                                
010900                                                                          
011000*    --- AREOR FÖR WEBKOMMUNIKATION                                       
011100*                                                                         
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011400*01  -COPY WZ01SUB                                                        
011500                                                                          
011600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011700 01  REQU-AREA.                                                           
011800*    03  -COPY WZ01REQU                                                   
011900*    03  -COPY WL0139I1                                                   
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012200     SKIP3                                                                
012300 01  RESP-AREA.                                                           
012400*    03  -COPY WZ01RESP                                                   
012500*    03  -COPY WL0139O1                                                   
012600                                                                          
012700                                                                          
012800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012900                                                                          
013000 01  NYCKLAR-TILL-DLI.                                                    
013100     03  W-IDLEVANM-X.                                                    
013200         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
013300         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
013400         05  W-IDRAPPNR          PIC  9(7)   VALUE ZERO.                  
013500                                                                          
013600     03  W-WDA211KY-X.                                                    
013700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013800         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
013900                                                                          
014000     03  W-WDA2D1KY-MIN-X.                                                
014100         05  W-IDFTG-MIN         PIC  9(2)   VALUE ZERO.                  
014200         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
014300         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
014400         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
014500         05  W-IDRAPPNR-MIN      PIC  9(7)   VALUE ZERO.                  
014600         05  W-IDRADNR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
014700                                                                          
014800     03  W-WDA2D1KY-MAX-X.                                                
014900*        05  W-IDFTG-MAX         PIC  9(2)   VALUE ZERO.                  
015000         05  W-IDFTG-MAX         PIC  9(2)   VALUE 99.                    
015100         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
015200         05  W-IDDISTR-MAX       PIC S9(5)   VALUE 99999 COMP-3.          
015300         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE 9999999 COMP-3.        
015400         05  FILLER              PIC X(10)   VALUE HIGH-VALUE.            
015500                                                                          
015600     03  W-KDANMORS-X.                                                    
015700         05  W-KDANMORS          PIC X(2).                                
015800     EJECT                                                                
015900*    --- STATUS-KOD FRÅN IMS                                              
016000 01  STATUS-WS                   PIC XX.                                  
016100     88  SEGMENT-FINNS                       VALUE '  '.                  
016200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016400     88  BASEN-SLUT                          VALUE 'GB'.                  
016500                                                                          
016600 01  GODK-STATUSKODER.                                                    
016700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016800                                                                          
016900 01  SSA1                        PIC X(128).                              
017000 01  SSA2                        PIC X(64).                               
017100     EJECT                                                                
017200*    --- IMS FUNKTIONSKODER                                               
017300*01  -COPY W0003                                                          
017400     EJECT                                                                
017500*    ---  DLI INPUT-OUTPUT AREA                                           
017600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017700                                                                          
017800 01  FILLER                      PIC X(16)   VALUE 'WDA201-AREA'.         
017900 01  DLI-IO-AREA-WDA201.                                                  
018000*    03  -COPY WDA201                                                     
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)   VALUE 'WDA211-AREA'.         
018300 01  DLI-IO-AREA-WDA211.                                                  
018400*    03  -COPY WDA211                                                     
018500     EJECT                                                                
018600 01  FILLER                      PIC X(16)   VALUE 'WDA2D1-AREA'.         
018700 01  DLI-IO-AREA-WDA2D1.                                                  
018800*    03  -COPY WDA2D1                                                     
018900     EJECT                                                                
019000 LINKAGE SECTION.                                                         
019100                                                                          
019200*01  -COPY W0009   -PRE MSG-                                              
019300     EJECT                                                                
019400*01  -COPY W0008  -PRE KREE-                                              
019500     05  FILLER                  PIC X.                                   
019600     EJECT                                                                
019700*01  -COPY W0008  -PRE KREI-                                              
019800     05  FILLER                  PIC X.                                   
019900     EJECT                                                                
020000 PROCEDURE DIVISION  USING MSG-PCB                                        
020100                           KREE-PCB                                       
020200                           KREI-PCB.                                      
020300     ENTRY 'DLITCBL' USING MSG-PCB                                        
020400                           KREE-PCB                                       
020500                           KREI-PCB.                                      
020600                                                                          
020700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
020800     IF SUB-KDRC = 0                                                      
020900        PERFORM A-INIT                                                    
021000        PERFORM B-KOLLA-NYCKLAR                                           
021100        IF NYCKLAR-OK                                                     
021200           IF REQU-KDPGMACT = WS-EXECUTE                                  
021300              PERFORM C-LAES-VISA-INFO                                    
021400           END-IF                                                         
021500                                                                          
021600           IF REQU-KDPGMACT = WS-DISCREPANCY-LINE                         
021700              PERFORM F-KONTROLL-FLCMD                                    
021800           END-IF                                                         
021900                                                                          
022000           IF REQU-KDPGMACT = WS-RETURN-PERMISSION                        
022100              PERFORM F-KONTROLL-FLCMD                                    
022200           END-IF                                                         
022300        END-IF                                                            
022400                                                                          
022500        MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                          
022600        MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                         
022700        MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                        
022800        IF WS-IDMSG-ERROR NOT = SPACE                                     
022900          MOVE ALL '+' TO RESP-WL0139O1                                   
023000          MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                       
023100          MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                      
023200          MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                        
023300          MOVE 001              TO RESP-IDMSGVER                          
023400                                                                          
023500          IF REQU-KVRADER  NUMERIC                                        
023600            MOVE REQU-KVRADER   TO RESP-KVRADER                           
023700          ELSE                                                            
023800            MOVE ZERO           TO RESP-KVRADER                           
023900          END-IF                                                          
024000        END-IF                                                            
024100                                                                          
024200        PERFORM S02-RETURN-RESPONSE                                       
024300     END-IF                                                               
024400                                                                          
024500     MOVE ZERO TO RETURN-CODE                                             
024600     GOBACK                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 A-INIT                         SECTION.                                  
025000     MOVE 'A-INIT' TO CURR-SECTION                                        
025100                                                                          
025200     MOVE ALL '+'   TO RESP-AREA                                          
025300     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
025400                       RESP-IDMSG-INFO                                    
025500                       RESP-IDELMT-ERROR                                  
025600     MOVE 001       TO RESP-IDMSGVER                                      
025700     MOVE ZERO      TO RESP-KVRADER                                       
025800                                                                          
025900     SET INDATA-OK TO TRUE                                                
026000                                                                          
026100     .                                                                    
026200     EJECT                                                                
026300 B-KOLLA-NYCKLAR                SECTION.                                  
026400     MOVE 'B-KOLLA-NYCKLAR' TO CURR-SECTION                               
026500                                                                          
026600     MOVE JA                    TO NYCKLAR-SW                             
026700                                                                          
026800*    MOVE REQU-IDFTG-KEY      TO W-IDFTG-MIN                              
026900*                                W-IDFTG-MAX                              
026901                                                                          
026902*    FIX TO MAKE IT POSSIBLE FOR A SPECIFIC USER TO HANDLE                
026903*    RETURNS FROM CA (FTG=54) TO US (DC=44, FTG=53)                       
026904*    IF REQU-IDUSER = 'PHCA4G1'                                           
026905*       AND REQU-IDDC-KEY3 = '44'                                         
026906*      MOVE '54'           TO REQU-IDFTG-KEY                              
026907*    END-IF                                                               
026908*    END FIX                                                              
026910                                                                          
027000     IF REQU-IDFTG-KEY NOT NUMERIC                                        
027100       MOVE NEJ             TO NYCKLAR-SW                                 
027200       MOVE 'IDFTG'         TO RESP-IDELMT-ERROR                          
027300       MOVE '023'           TO RESP-IDMSG-ERROR                           
027400     ELSE                                                                 
027500       MOVE REQU-IDFTG-KEY  TO W-IDFTG-MIN                                
027600                               W-IDFTG-MAX                                
027700     END-IF                                                               
027800     IF REQU-IDARTNR-KEY3 NUMERIC                                         
027900       MOVE REQU-IDARTNR-KEY3   TO W-IDARTNR-MIN                          
028000                                   W-IDARTNR-MAX                          
028100                                                                          
028200     ELSE                                                                 
028300       MOVE NEJ              TO NYCKLAR-SW                                
028400       MOVE '023'        TO RESP-IDMSG-ERROR                              
028500*      IS INVALID ***                                                     
028600       MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                             
028700     END-IF                                                               
028800                                                                          
028900     MOVE REQU-IDDISTR-KEY3   TO WS-IDDISTR                               
029000     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
029100                                                                          
029200     IF WS-IDDISTR NUMERIC                                                
029300       IF WS-IDDISTR > ZERO                                               
029400          MOVE WS-IDDISTR        TO W-IDDISTR-MIN                         
029500                                    W-IDDISTR-MAX                         
029600       ELSE                                                               
029700          MOVE ZERO              TO W-IDDISTR-MIN                         
029800                                    W-IDDISTR-MAX                         
029900       END-IF                                                             
030000     ELSE                                                                 
030100       IF WS-IDDISTR NOT NUMERIC                                          
030200          MOVE NEJ              TO NYCKLAR-SW                             
030300          MOVE '023'        TO RESP-IDMSG-ERROR                           
030400*         IS INVALID ***                                                  
030500          MOVE 'IDDISTR'    TO RESP-IDELMT-ERROR                          
030600       END-IF                                                             
030700     END-IF                                                               
030800                                                                          
030900*    -- KONTROLL AV IDKUNDNR                                              
031000                                                                          
031100     MOVE REQU-IDKUNDNR-KEY3   TO WS-IDKUNDNR                             
031200     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
031300                                                                          
031400     IF WS-IDKUNDNR NUMERIC AND                                           
031500        WS-IDKUNDNR > ZERO AND                                            
031600        W-IDDISTR-MIN > ZERO                                              
031700           MOVE WS-IDKUNDNR     TO W-IDKUNDNR-MIN                         
031800                                   W-IDKUNDNR-MAX                         
031900     ELSE                                                                 
032000        IF WS-IDKUNDNR NOT = ALL '+'                                      
032100           MOVE NEJ             TO NYCKLAR-SW                             
032200           MOVE '023'        TO RESP-IDMSG-ERROR                          
032300*          IS INVALID ***                                                 
032400           MOVE 'IDKUNDNR'   TO RESP-IDELMT-ERROR                         
032500        END-IF                                                            
032600     END-IF                                                               
032700                                                                          
032800*    -- KONTROLL AV KDANMORS                                              
032900                                                                          
033000     MOVE REQU-KDANMORS-KEY3   TO WS-KDANMORS                             
033100     INSPECT WS-KDANMORS REPLACING LEADING SPACE BY ZERO                  
033200                                                                          
033300     IF WS-KDANMORS NUMERIC                                               
033400       MOVE WS-KDANMORS        TO W-KDANMORS                              
033500     ELSE                                                                 
033600       IF WS-KDANMORS NOT = ALL '+'                                       
033700         MOVE NEJ              TO NYCKLAR-SW                              
033800         MOVE '023'            TO RESP-IDMSG-ERROR                        
033900         MOVE 'KDANMORS'       TO RESP-IDELMT-ERROR                       
034000       END-IF                                                             
034100     END-IF                                                               
034200                                                                          
034300     IF NYCKLAR-OK                                                        
034400       MOVE REQU-IDDC-KEY3        TO RESP-IDDC-KEY3                       
034500       IF NOT REQU-IDARTNR-KEY3 = ALL '+'                                 
034600          MOVE REQU-IDARTNR-KEY3  TO RESP-IDARTNR-KEY3                    
034700       END-IF                                                             
034800       IF NOT REQU-IDDISTR-KEY3 = ALL '+'                                 
034900          MOVE REQU-IDDISTR-KEY3  TO RESP-IDDISTR-KEY3                    
035000       END-IF                                                             
035100       IF NOT REQU-IDKUNDNR-KEY3 = ALL '+'                                
035200          MOVE REQU-IDKUNDNR-KEY3 TO RESP-IDKUNDNR-KEY3                   
035300       END-IF                                                             
035400       IF NOT REQU-KDANMORS-KEY3 = ALL '+'                                
035500          MOVE REQU-KDANMORS-KEY3 TO RESP-KDANMORS-KEY3                   
035600       END-IF                                                             
035700                                                                          
035800       IF REQU-KDPGMACT = WS-DISCREPANCY-LINE  OR                         
035900          REQU-KDPGMACT = WS-RETURN-PERMISSION                            
036000         IF REQU-KVRADER = ALL '+' OR                                     
036100            REQU-KVRADER = ZERO                                           
036200           MOVE NEJ              TO NYCKLAR-SW                            
036300           MOVE '023'            TO RESP-IDMSG-ERROR                      
036400           MOVE 'CMD'            TO RESP-IDELMT-ERROR                     
036500         END-IF                                                           
036600       END-IF                                                             
036700     END-IF                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 C-LAES-VISA-INFO               SECTION.                                  
037100     MOVE 'C-LAES-VISA-INFO' TO CURR-SECTION                              
037200                                                                          
037300     MOVE ZERO TO WS-KVRADER                                              
037400     PERFORM CA-LAES-RADDATA                                              
037500                                                                          
037600     IF SEGMENT-FINNS                                                     
037700                                                                          
037800        MOVE +1   TO INDX                                                 
037900        PERFORM UNTIL INDX > MAX-INDX OR                                  
038000                      SEGMENT-SAKNAS                                      
038100                                                                          
038200           PERFORM CB-RED-RESP-RAD                                        
038300           PERFORM CA-LAES-RADDATA                                        
038400        END-PERFORM                                                       
038500        MOVE WS-KVRADER  TO RESP-KVRADER                                  
038600                                                                          
038700*** DET FINNS INGA RADER FÖR PÅLOGGAT DC.                                 
038800        IF WS-KVRADER = 0                                                 
038900          MOVE '027'     TO RESP-IDMSG-ERROR                              
039000        END-IF                                                            
039100                                                                          
039200     ELSE                                                                 
039300        MOVE '027'       TO RESP-IDMSG-ERROR                              
039400        SET INDATA-FEL TO TRUE                                            
039500     END-IF                                                               
039600     .                                                                    
039700     EJECT                                                                
039800 CA-LAES-RADDATA                SECTION.                                  
039900     MOVE 'CA-LAES-RADDATA ' TO CURR-SECTION                              
040000                                                                          
040100     IF W-KDANMORS NUMERIC                                                
040200       PERFORM IMS-03-GET-KREI-KOD                                        
040300       PERFORM CAA-KOLLA-BEHORIGHET                                       
040400     ELSE                                                                 
040500       PERFORM IMS-04-GET-KREI                                            
040600       PERFORM CAB-KOLLA-BEHORIGHET                                       
040700     END-IF                                                               
040800                                                                          
040900     IF SEGMENT-FINNS                                                     
041000       MOVE SEQD-IDLEVANM       TO W-IDLEVANM-X                           
041100       PERFORM IMS-01-GU-KREE01                                           
041200                                                                          
041300       MOVE SEQD-IDARTNR        TO W-IDARTNR                              
041400       MOVE SEQD-IDRADNR        TO W-IDRADNR                              
041500       PERFORM IMS-02-GNP-KREE11                                          
041600     END-IF                                                               
041700     .                                                                    
041800     EJECT                                                                
041900 CAA-KOLLA-BEHORIGHET SECTION.                                            
042000     MOVE 'CAA-KOLLA-BEHORIGHET' TO CURR-SECTION                          
042100                                                                          
042200     MOVE 'F'                     TO SEC-KDSVAR                           
042300     PERFORM UNTIL SEC-KDSVAR NOT = 'F'    OR                             
042400                   SEGMENT-SAKNAS          OR                             
042500                   BASEN-SLUT                                             
042600        MOVE REQU-IDUSER          TO SEC-IDUSER                           
042700        MOVE 'L139'               TO SEC-IDTRANS                          
042800        MOVE SEQD-IDDISTR         TO W-IDDISTR-SEC                        
042900        MOVE W-IDDISTR-SEC        TO SEC-IDKEY                            
043000                                                                          
043100        CALL WSECURIT USING SEC-IDUSER                                    
043200                            SEC-IDTRANS                                   
043300                            SEC-IDKEY                                     
043400                            SEC-KDSVAR                                    
043500                                                                          
043600        IF SEC-KDSVAR = 'F'                                               
043700           PERFORM IMS-03-GET-KREI-KOD                                    
043800        END-IF                                                            
043900     END-PERFORM                                                          
044000     .                                                                    
044100     EJECT                                                                
044200 CAB-KOLLA-BEHORIGHET SECTION.                                            
044300     MOVE 'CAB-KOLLA-BEHORIGHET' TO CURR-SECTION                          
044400                                                                          
044500     MOVE 'F'                     TO SEC-KDSVAR                           
044600     PERFORM UNTIL SEC-KDSVAR NOT = 'F'    OR                             
044700                   SEGMENT-SAKNAS          OR                             
044800                   BASEN-SLUT                                             
044900        MOVE REQU-IDUSER          TO SEC-IDUSER                           
045000        MOVE 'L139'               TO SEC-IDTRANS                          
045100        MOVE SEQD-IDDISTR         TO W-IDDISTR-SEC                        
045200        MOVE W-IDDISTR-SEC        TO SEC-IDKEY                            
045300                                                                          
045400        CALL WSECURIT USING SEC-IDUSER                                    
045500                            SEC-IDTRANS                                   
045600                            SEC-IDKEY                                     
045700                            SEC-KDSVAR                                    
045800                                                                          
045900        IF SEC-KDSVAR = 'F'                                               
046000           PERFORM IMS-04-GET-KREI                                        
046100        END-IF                                                            
046200     END-PERFORM                                                          
046300     .                                                                    
046400     EJECT                                                                
046500 CB-RED-RESP-RAD                SECTION.                                  
046600     MOVE 'CB-RED-RESP-RAD     ' TO CURR-SECTION                          
046700                                                                          
046800     IF REQU-FL-IDDC = 'J'                                                
046900       PERFORM CBA-FLYTTA-RADDATA                                         
047000       ADD 1 TO INDX                                                      
047100       ADD 1 TO WS-KVRADER                                                
047200     ELSE                                                                 
047300       IF LEV-IDDC = REQU-IDDC-KEY3                                       
047400          PERFORM CBA-FLYTTA-RADDATA                                      
047500          ADD 1 TO INDX                                                   
047600          ADD 1 TO WS-KVRADER                                             
047700       END-IF                                                             
047800     END-IF                                                               
047900     .                                                                    
048000     EJECT                                                                
048100 CBA-FLYTTA-RADDATA             SECTION.                                  
048200     MOVE 'CBA-FLYTTA-RADDATA  ' TO CURR-SECTION                          
048300                                                                          
048400       MOVE ANM-IDDISTR         TO RESP-IDDISTR      (INDX)               
048500       MOVE ANM-IDKUNDNR        TO RESP-IDKUNDNR     (INDX)               
048600       MOVE ANM-IDRAPPNR        TO RESP-IDRAPPNR     (INDX)               
048700       MOVE ANM-DALEVANM(3:6)   TO RESP-TILEVANM     (INDX)               
048800       MOVE LEV-KDANMORS        TO RESP-KDANMORS     (INDX)               
048900                                   OKOD-KDANMORS                          
049000       IF LEV-KDKREBEH = 'J'                                              
049100         MOVE 'Y'               TO RESP-KDKREBEH     (INDX)               
049200       ELSE                                                               
049300         IF LEV-KDKREBEH = 'ANN' OR 'DEL'                                 
049400           MOVE 'CAN'             TO RESP-KDKREBEH     (INDX)             
049500         ELSE                                                             
049600           MOVE LEV-KDKREBEH      TO RESP-KDKREBEH     (INDX)             
049700         END-IF                                                           
049800       END-IF                                                             
049900                                                                          
050000       MOVE W-IDRADNR           TO RESP-IDRADNR      (INDX)               
050100       MOVE LEV-IDDC            TO RESP-IDDC         (INDX)               
050200                                                                          
050300       CALL W418OKOD USING OKOD-W418OKOD                                  
050400       IF (OKOD-FL-RETILL = 'J') OR                                       
050500          (OKOD-FL-INTERNUPPACKNING = 'J')                                
050600         IF ANM-DARETILL > 0                                              
050700           MOVE ANM-DARETILL(3:6) TO TMP1-YYMMDD                          
050800           MOVE ANM-DARETANK(3:6) TO TMP2-YYMMDD                          
050900           MOVE LEV-TIINLINL      TO TMP3-YYMMDD                          
051000           PERFORM WY2000Q1                                               
051100                                                                          
051200           IF TMP1-YYMMDD >= TMP2-YYMMDD AND                              
051300              TMP1-YYMMDD >= TMP3-YYMMDD                                  
051400                                                                          
051500             MOVE 'RP '             TO RESP-DATUM-TEXT   (INDX)           
051600             MOVE ANM-DARETILL(3:6) TO RESP-TIRETILL     (INDX)           
051700           END-IF                                                         
051800                                                                          
051900           IF TMP2-YYMMDD >= TMP1-YYMMDD AND                              
052000              TMP2-YYMMDD >= TMP3-YYMMDD                                  
052100              MOVE 'REC'             TO RESP-DATUM-TEXT   (INDX)          
052200              MOVE ANM-DARETANK(3:6) TO RESP-TIRETILL     (INDX)          
052300           END-IF                                                         
052400                                                                          
052500           IF TMP3-YYMMDD >= TMP1-YYMMDD AND                              
052600              TMP3-YYMMDD >= TMP2-YYMMDD                                  
052700              MOVE 'BIN'        TO RESP-DATUM-TEXT   (INDX)               
052800              MOVE LEV-TIINLINL TO RESP-TIRETILL     (INDX)               
052900           END-IF                                                         
053000         END-IF                                                           
053100       END-IF                                                             
053200                                                                          
053300       MOVE LEV-KVRETINL        TO RESP-KVRETINL     (INDX)               
053400       MOVE LEV-KVRETINL-SKR    TO RESP-KVRETINL-SKR (INDX)               
053500       IF LEV-FLTEXT             = JA                                     
053600          MOVE YES              TO RESP-FLTEXT       (INDX)               
053700       ELSE                                                               
053800         MOVE LEV-FLTEXT        TO RESP-FLTEXT       (INDX)               
053900       END-IF                                                             
054000     .                                                                    
054100     EJECT                                                                
054200                                                                          
054300 F-KONTROLL-FLCMD    SECTION.                                             
054400     MOVE 'F-KONTROLL-FLCMD    ' TO CURR-SECTION                          
054500                                                                          
054600     MOVE +1 TO INDX                                                      
054700     MOVE  0 TO ANTAL-FLCMD                                               
054800     PERFORM UNTIL INDX > REQU-KVRADER                                    
054900       IF REQU-FLCMD         (INDX) NOT = ALL '+'                         
055000                                                                          
055100         IF REQU-FLCMD       (INDX) = 'J' OR 'N'                          
055110                                                                          
055120           IF ( NOT REQU-IDDISTR (INDX)  = ALL '+' ) AND                  
055130              ( NOT REQU-IDKUNDNR(INDX)  = ALL '+' ) AND                  
055140              ( NOT REQU-IDRAPPNR(INDX)  = ALL '+' ) AND                  
055150              ( NOT REQU-IDRADNR (INDX)  = ALL '+' )                      
055160                                                                          
055200                MOVE REQU-FLCMD(INDX)    TO RESP-FLCMD(INDX)              
055300                MOVE REQU-IDDISTR(INDX)  TO RESP-IDDISTR(INDX)            
055400                MOVE REQU-IDKUNDNR(INDX) TO RESP-IDKUNDNR(INDX)           
055500                MOVE REQU-IDRAPPNR(INDX) TO RESP-IDRAPPNR(INDX)           
055600                MOVE REQU-IDRADNR(INDX)  TO RESP-IDRADNR(INDX)            
055700                                                                          
055800                IF REQU-FLCMD       (INDX) = 'J'                          
055900                   ADD +1         TO ANTAL-FLCMD                          
056000                END-IF                                                    
056010           ELSE                                                           
056020               MOVE NEJ         TO NYCKLAR-SW                             
056030               MOVE '023'       TO RESP-IDMSG-ERROR                       
056040*              IS INVALID ***                                             
056050               MOVE 'DATA '     TO RESP-IDELMT-ERROR                      
056060           END-IF                                                         
056100         ELSE                                                             
056200           MOVE NEJ         TO NYCKLAR-SW                                 
056300           MOVE '023'       TO RESP-IDMSG-ERROR                           
056400*          IS INVALID ***                                                 
056500           MOVE 'CMD'       TO RESP-IDELMT-ERROR                          
056600         END-IF                                                           
056700       ELSE                                                               
056800         MOVE '+'                   TO RESP-FLCMD(INDX)                   
056900       END-IF                                                             
057000       ADD 1 TO INDX                                                      
057100     END-PERFORM                                                          
057200                                                                          
057300     MOVE REQU-KVRADER  TO RESP-KVRADER                                   
057400                                                                          
057500     IF ANTAL-FLCMD = 0                                                   
057600       MOVE NEJ         TO NYCKLAR-SW                                     
057700       MOVE '023'       TO RESP-IDMSG-ERROR                               
057800       MOVE 'CMD'       TO RESP-IDELMT-ERROR                              
057900     END-IF                                                               
058000     IF ANTAL-FLCMD > 1                                                   
058100       MOVE NEJ             TO NYCKLAR-SW                                 
058200       MOVE '023'           TO RESP-IDMSG-ERROR                           
058300*      IS INVALID ***                                                     
058400       MOVE 'CMD'           TO RESP-IDELMT-ERROR                          
058500     END-IF                                                               
058600     .                                                                    
058700     EJECT                                                                
058800*    --- DISPATCHER SECTIONS                                              
058900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
059000     MOVE 'S01-FETCH-REQUEST-ARGUMENT' TO CURR-SECTION                    
059100                                                                          
059200     MOVE 'GETARG'               TO SUB-KDFUNC                            
059300     MOVE 'CARPARTS.LDC.DISCREPANCYQUERYPART' TO SUB-ADDISPABS            
059400     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
059500                                                                          
059600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
059700                                                                          
059800     IF SUB-KDRC > 0                                                      
059900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
060000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
060100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
060200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
060300     END-IF                                                               
060400     .                                                                    
060500                                                                          
060600 S02-RETURN-RESPONSE SECTION.                                             
060700     MOVE 'S02-RETURN-RESPONSE  ' TO CURR-SECTION                         
060800                                                                          
060900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
061000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
061100                                                                          
061200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
061300                                                                          
061400     IF SUB-KDRC > 0                                                      
061500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
061600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
061700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
061800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
061900     END-IF                                                               
062000     .                                                                    
062100                                                                          
062200* --- IMS SEKTIONER ---                                                   
062300                                                                          
062400 IMS-01-GU-KREE01               SECTION.                                  
062500     MOVE 'IMS-01'     TO CURR-IMS-SECTION                                
062600                                                                          
062700     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
062800          DELIMITED BY SIZE INTO SSA1                                     
062900     MOVE '    ' TO GODK-STATUSKODER                                      
063000     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA-WDA201 SSA1               
063100     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
063200     PERFORM IMS-STATUSKONTROLL                                           
063300     .                                                                    
063400                                                                          
063500 IMS-02-GNP-KREE11              SECTION.                                  
063600     MOVE 'IMS-02'     TO CURR-IMS-SECTION                                
063700                                                                          
063800     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
063900          DELIMITED BY SIZE INTO SSA1                                     
064000     MOVE '    ' TO GODK-STATUSKODER                                      
064100     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA-WDA211 SSA1              
064200     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
064300     PERFORM IMS-STATUSKONTROLL                                           
064400     .                                                                    
064500     EJECT                                                                
064600 IMS-03-GET-KREI-KOD            SECTION.                                  
064700     MOVE 'IMS-03'     TO CURR-IMS-SECTION                                
064800                                                                          
064900     STRING 'WLKREI01(WDA2D1KY>=' W-WDA2D1KY-MIN-X                        
065000                    '&WDA2D1KY<=' W-WDA2D1KY-MAX-X                        
065100                    '&KDANMORS =' W-KDANMORS-X ')'                        
065200          DELIMITED BY SIZE INTO SSA1                                     
065300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
065400     CALL CBLTDLI USING GN KREI-PCB DLI-IO-AREA-WDA2D1 SSA1               
065500     MOVE KREI-STATUS-CODE TO STATUS-WS                                   
065600     PERFORM IMS-STATUSKONTROLL                                           
065700     .                                                                    
065800                                                                          
065900 IMS-04-GET-KREI                SECTION.                                  
066000     MOVE 'IMS-04'     TO CURR-IMS-SECTION                                
066100                                                                          
066200     STRING 'WLKREI01(WDA2D1KY>=' W-WDA2D1KY-MIN-X                        
066300                    '&WDA2D1KY<=' W-WDA2D1KY-MAX-X ')'                    
066400          DELIMITED BY SIZE INTO SSA1                                     
066500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
066600     CALL CBLTDLI USING GN KREI-PCB DLI-IO-AREA-WDA2D1 SSA1               
066700     MOVE KREI-STATUS-CODE TO STATUS-WS                                   
066800     PERFORM IMS-STATUSKONTROLL                                           
066900     .                                                                    
067000                                                                          
067100 IMS-STATUSKONTROLL             SECTION.                                  
067200                                                                          
067300     SET STATUS-IX TO 1                                                   
067400     SEARCH GODK-STATUS                                                   
067500       AT END                                                             
067600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
067700         DELIMITED BY SIZE INTO FELTEXT                                   
067800         CALL FELLOG                                                      
067900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
068000         CONTINUE                                                         
068100     END-SEARCH                                                           
068200     .                                                                    
068300     EJECT                                                                
069000*    -COPY WY2000Q1                                                       
