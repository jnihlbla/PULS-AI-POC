000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WL015400.                                                
000400*AUTHOR.         TAPAS KUMAR GHOSH.                                       
000500*DATE-WRITTEN.   2004/08/27.                                              
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*        WL015400 PROGRAM IS A REPLICA OF W4079400 PROGRAM                
001000*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001100*                                                                         
001200*                                                                         
001300*    FUNKTION:                                                            
001400*        BAKGRUNDS MPP SOM SKRIVER UT RETURTILLSTÅND.                     
001500*        STARTAS AV W40735, W40736 OCH W40737                             
001600*                                                                         
001700*        PROGRAMMET          LÄSER      WLKREE (WDA2)                     
001800*        PROGRAMMET          LÄSER      WLRETA (WDA3)                     
001900*        PROGRAMMET          LÄSER      WLARTC (WDK6)                     
002000*        PROGRAMMET          LÄSER              WDK7                      
002100*        PROGRAMMET          LÄSER      WLBENA (WDD3)                     
002200*        PROGRAMMET          LÄSER              WDD5                      
002300*                                                                         
002400*    CHANGES:                                                             
002500*    2011-09-26 SS(SHILPA) E'TRACKER 10143271 CHINA WAREHOUSE             
002600*                                             PROJECT 1                   
002700*    INDATA.                                                              
002800*        TRANSAKTION: WL0154T                                             
002900*        REQUEST:     WZ01REQU                                            
003000*        REQUEST:     WL0154I1                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*        RESPONSE:    WZ01RESP                                            
003400*                     WZ04HDR                                             
003500*                     WL01541                                             
003600*                     WL01542                                             
003700*                     WL01543                                             
003800                                                                          
003900     SKIP3                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'WL015400'.            
004700                                                                          
004800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005000 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
005100 77  KDRC-DISPLAY                PIC Z(5).                                
005200 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
005300                                                                          
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  YES                         PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  WS-IDSKYLT-CN               PIC X(3)    VALUE 'RCN'.                 
005800 77  WS-IDSKYLT-GB               PIC X(3)    VALUE 'GB '.                 
005900 77  WS-CP-UTF8                  PIC X(4)    VALUE 'UTF8'.                
006000 77  WS-CP-EBCDIC                PIC X(3)    VALUE '278'.                 
006100                                                                          
006200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006300     88  NYCKLAR-OK                          VALUE 'J'.                   
006400     88  NYCKLAR-FEL                         VALUE 'N'.                   
006500                                                                          
006600 77  INDX                        PIC S9(4)   VALUE ZERO COMP SYNC.        
006700 77  MAX-INDX                    PIC S9(4)   VALUE +10 COMP SYNC.         
006800 77  W-KVLEVANM-KVAR             PIC S9(7)   VALUE 0   COMP-3.            
006900 77  W-FLFLER-KOLLI              PIC  X(1)   VALUE 'N'.                   
007000 77  W-REKSIFFR                  PIC S9(1)   VALUE ZERO COMP-3.           
007100 77  W-ADLAGOMR                  PIC S9(3)   VALUE ZERO COMP-3.           
007200 77  W-ADGANG                    PIC S9(3)   VALUE ZERO COMP-3.           
007300 77  W-ADPLATS                   PIC S9(5)   VALUE ZERO COMP-3.           
007400 77  W-KDERS                     PIC S9(3)   VALUE ZERO COMP-3.           
007500 77  W-BEART                     PIC X(25)   VALUE SPACE.                 
007600 77  W-KDSORT                    PIC X(2)    VALUE SPACE.                 
007700 77  W-KDFGPRIO                  PIC 9(3)    VALUE ZERO.                  
007800 77  DATE-INTERVAL-SW            PIC X       VALUE 'J'.                   
007900     88  W-DATE-INTERVAL-OK                  VALUE 'J'.                   
008000     88  W-DATE-INTERVAL-EJ                  VALUE 'N'.                   
008100                                                                          
008200 01  WS-DAREGDAT                 PIC 9(8)    VALUE ZERO.                  
008300 01  FILLER REDEFINES WS-DAREGDAT.                                        
008400     03  WS-SEKEL-D              PIC 9(2).                                
008500     03  WS-AAMMDD               PIC 9(6).                                
008600                                                                          
008700 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
008800 01  FILLER REDEFINES DAGENS-DATUM.                                       
008900     03  DAGENS-DATUM-SEKEL      PIC 9(2).                                
009000     03  DAGENS-DATUM-AAMMDD     PIC 9(6).                                
009100                                                                          
009200*    --- PARAMETERS TO ABEND                                              
009300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009600                                                                          
009700 01  WS-YYMMDDHHMM.                                                       
009800     03 WS-YYMMDD                PIC  9(6).                               
009900     03 WS-HHMM                  PIC  9(4).                               
010000                                                                          
010100                                                                          
010200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010300 01  GENERELLA-SUBPROGRAM.                                                
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010700     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
010800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
011000     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
011100     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
011200     EJECT                                                                
011300*    ---  LÄNKAREA TILL W418OKOD                                          
011400 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
011500                                                                          
011600*01 -COPY W418OKOD           -PRE OKOD-.                                  
011700     SKIP3                                                                
011800* VARIABLER TILL SUBPROGRAM W006PRR1                                      
011900*01  -COPY W006PRAR                                                       
012000     SKIP2                                                                
012100     EJECT                                                                
012200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012300*                                                                         
012400 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
012500*   -COPY WZ01SEND                                                        
012600     EJECT                                                                
012700 01  FILLER                      PIC X(16)  VALUE 'WZ01SUB '.             
012800*   -COPY WZ01SUB                                                         
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
013100                                                                          
013200 01  REQU-AREA.                                                           
013300*    03 -COPY WZ01REQU                                                    
013400*    03 -COPY WL0154I1                                                    
013500                                                                          
013600 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
013700                                                                          
013800 01  RESP-AREA.                                                           
013900*    03 -COPY WZ01RESP                                                    
014000                                                                          
014100 01  FILLER                      PIC X(16)  VALUE 'HDR-AREA'.             
014200                                                                          
014300 01  HDR-AREA.                                                            
014400*    03 -COPY WZ01REQU   -PRE HDR-                                        
014500*    03 -COPY WZ04HDR                                                     
014600                                                                          
014700 01  FILLER                 PIC X(16)  VALUE 'RESP-AREA-HEAD'.            
014800 01  RESP-AREA-HEAD.                                                      
014900*    03 -COPY WL01541                                                     
015000                                                                          
015100 01  FILLER                 PIC X(16)  VALUE 'RESP-AREA-LINE'.            
015200 01  RESP-AREA-LINE.                                                      
015300*    03 -COPY WL01542                                                     
015400                                                                          
015500 01  FILLER                 PIC X(16)  VALUE 'RESP-AREA-TEXT'.            
015600 01  RESP-AREA-TEXT.                                                      
015700*    03 -COPY WL01543                                                     
015800     EJECT                                                                
015900 01  FILLER                 PIC X(16)  VALUE 'WTRAUTF8-AREA'.             
016000*01  -COPY WTRAUTF8                                                       
016100                                                                          
016200     EJECT                                                                
016300*    --- PARAMETRAR TILL WDAGKONV                                         
016400*01  -COPY WDAGAREA                                                       
016500     EJECT                                                                
016600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016700*                                                                         
016800     EJECT                                                                
016900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017000     SKIP3                                                                
017100 01  NYCKLAR-TILL-DLI.                                                    
017200                                                                          
017300     03  W-IDLEVANM-X.                                                    
017400         05  W-IDDISTR-ANM       PIC S9(5)   COMP-3 VALUE ZERO.           
017500         05  W-IDKUNDNR-ANM      PIC S9(7)   COMP-3 VALUE ZERO.           
017600         05  W-IDRAPPNR-ANM      PIC  X(7)          VALUE ZERO.           
017700                                                                          
017800     03  W-WDA3F1KY-MIN-X.                                                
017900         05  W-IDDC-X.                                                    
018000           07  W-IDDC-RET-MIN    PIC  X(2)          VALUE SPACE.          
018100         05  W-IDDISTR-RET-MIN   PIC S9(5)   COMP-3 VALUE ZERO.           
018200         05  W-IDKUNDNR-RET-MIN  PIC S9(7)   COMP-3 VALUE ZERO.           
018300         05  W-IDRAPPNR-RET-MIN  PIC  X(7)          VALUE ZERO.           
018400         05  W-IDRT-RET-MIN      PIC  X(3)          VALUE SPACE.          
018500         05  W-IDRTLOP-RET-MIN   PIC  9(3)          VALUE ZERO.           
018600         05  W-IDKOLLI-RET-MIN   PIC S9(5)   COMP-3 VALUE ZERO.           
018700         05  W-DAREGDAT-RET-MIN  PIC  9(8)          VALUE ZERO.           
018800         05  W-TIKLOCK-RET-MIN   PIC S9(9)   COMP-3 VALUE ZERO.           
018900                                                                          
019000     03  W-WDA3F1KY-MAX-X.                                                
019100         05  W-IDDC-RET-MAX      PIC  X(2)          VALUE SPACE.          
019200         05  W-IDDISTR-RET-MAX   PIC S9(5)   COMP-3 VALUE ZERO.           
019300         05  W-IDKUNDNR-RET-MAX  PIC S9(7)   COMP-3 VALUE ZERO.           
019400         05  W-IDRAPPNR-RET-MAX  PIC  X(7)          VALUE ZERO.           
019500         05  W-IDRT-RET-MAX      PIC  X(3)          VALUE SPACE.          
019600         05  W-IDRTLOP-RET-MAX   PIC  9(3)          VALUE ZERO.           
019700         05  W-IDKOLLI-RET-MAX   PIC S9(5)   COMP-3 VALUE ZERO.           
019800         05  W-DAREGDAT-RET-MAX  PIC  9(8)          VALUE ZERO.           
019900         05  W-TIKLOCK-RET-MAX   PIC S9(9)   COMP-3 VALUE ZERO.           
020000                                                                          
020100     03  W-WDA211KY-X.                                                    
020200       04 W-IDARTNR-X.                                                    
020300         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
020400       04 W-IDRADNR-X.                                                    
020500         05  W-IDRADNR           PIC S9(5)   COMP-3 VALUE ZERO.           
020600                                                                          
020700     03  W-IDSKYLT-X.                                                     
020800         05  W-IDSKYLT           PIC  X(3).                               
020900                                                                          
021000     03  W-IDLAND-X.                                                      
021100         05  W-IDLAND            PIC  X(2).                               
021200                                                                          
021300     03  W-KDKVAINF-X.                                                    
021400         05  W-KDKVAINF          PIC  X(1)   VALUE 'R'.                   
021500                                                                          
021600     03  W-IDDC-B6-X.                                                     
021700         05  W-IDDC-B6           PIC  X(2).                               
021800                                                                          
021900     SKIP2                                                                
022000*    --- STATUS-KOD FRÅN IMS                                              
022100 01  STATUS-WS                   PIC XX.                                  
022200     88  SEGMENT-FINNS                       VALUE '  '.                  
022300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
022600     SKIP2                                                                
022700 01  GODK-STATUSKODER.                                                    
022800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022900     SKIP3                                                                
023000 01  SSA1                        PIC X(128).                              
023100 01  SSA2                        PIC X(64).                               
023200 01  SSA3                        PIC X(64).                               
023300     EJECT                                                                
023400*    --- IMS FUNKTIONSKODER                                               
023500*01  -COPY W0003                                                          
023600     EJECT                                                                
023700*    ---  DLI INPUT-OUTPUT AREA                                           
023800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
023900     SKIP3                                                                
024000 01  DLI-IO-AREA1.                                                        
024100     03  IO-AREA1                PIC X(500)  VALUE SPACE.                 
024200     SKIP3                                                                
024300     03  WLKREE01 REDEFINES IO-AREA1.                                     
024400*        05  -COPY WDA201                                                 
024500     EJECT                                                                
024600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
024700     SKIP3                                                                
024800 01  DLI-IO-AREA2.                                                        
024900     03  IO-AREA2                PIC X(500)  VALUE SPACE.                 
025000     SKIP3                                                                
025100     03  WLKREE11 REDEFINES IO-AREA2.                                     
025200*        05  -COPY WDA211                                                 
025300     EJECT                                                                
025400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
025500     SKIP3                                                                
025600 01  DLI-IO-AREA3.                                                        
025700     03  IO-AREA3                PIC X(1200)  VALUE SPACE.                
025800     SKIP3                                                                
025900     03  WLRETG01 REDEFINES IO-AREA3.                                     
026000*        05  -COPY WDA3F1                                                 
026100     EJECT                                                                
026200     03  WLARTC01 REDEFINES IO-AREA3.                                     
026300*        05  -COPY WDK601                                                 
026400     EJECT                                                                
026500     03  WLARTC11 REDEFINES IO-AREA3.                                     
026600*        05  -COPY WDK611                                                 
026700     EJECT                                                                
026800     EJECT                                                                
026900     03  WLBENA11 REDEFINES IO-AREA3.                                     
027000*        05  -COPY WDD311                                                 
027100     EJECT                                                                
027200     03  W6KVAH11 REDEFINES IO-AREA3.                                     
027300*        05  -COPY W6D211                                                 
027400     EJECT                                                                
027500     03  WLKREE21 REDEFINES IO-AREA3.                                     
027600*        05  -COPY WDA221                                                 
027700 01  FILLER                      PIC X(16) VALUE 'WDK711 AREA'.           
027800 01  DLI-IO-WDK711.                                                       
027900*    03  -COPY WDK711                                                     
028000     EJECT                                                                
028100 01  FILLER                      PIC X(16) VALUE 'WDK712 AREA'.           
028200 01  DLI-IO-WDK712.                                                       
028300*    03  -COPY WDK712                                                     
028400     EJECT                                                                
028500 01  FILLER                      PIC X(16) VALUE 'WDB601 AREA'.           
028600 01  DLI-IO-AREA-B601.                                                    
028700*    03  -COPY WDB601                                                     
028800     EJECT                                                                
028900 01  FILLER               PIC X(16)   VALUE 'WDD501 AREA'.                
029000 01   DLI-IO-AREA-D501.                                                   
029100*     03  -COPY WDD501                                                    
029200                                                                          
029300                                                                          
029400 LINKAGE SECTION.                                                         
029500                                                                          
029600 01  MSG-PCB                     PIC X.                                   
029700*01  -COPY W0008   -PRE KREE-                                             
029800     05  FILLER                  PIC X.                                   
029900                                                                          
030000 01  DISTRDOC-PCB                PIC X.                                   
030100     EJECT                                                                
030200*01  -COPY W0008   -PRE RETG-                                             
030300     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500*01  -COPY W0008   -PRE ARTC-                                             
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800*01  -COPY W0008   -PRE WDK7-                                             
030900     05  FILLER                  PIC X.                                   
031000     EJECT                                                                
031100*01  -COPY W0008   -PRE BENA-                                             
031200     05  FILLER                  PIC X.                                   
031300     EJECT                                                                
031400*01  -COPY W0008   -PRE KVAH-                                             
031500     05  FILLER                  PIC X.                                   
031600     EJECT                                                                
031700*01  -COPY W0008   -PRE WDB6-                                             
031800     05  FILLER                  PIC X.                                   
031900     EJECT                                                                
032000*01  -COPY W0008   -PRE WDD5-                                             
032100     05  FILLER                  PIC X.                                   
032200     EJECT                                                                
032300 PROCEDURE DIVISION  USING MSG-PCB  DISTRDOC-PCB                          
032400                           KREE-PCB RETG-PCB ARTC-PCB WDK7-PCB            
032500                           BENA-PCB KVAH-PCB WDB6-PCB                     
032600                           WDD5-PCB.                                      
032700     ENTRY 'DLITCBL' USING MSG-PCB  DISTRDOC-PCB                          
032800                           KREE-PCB RETG-PCB ARTC-PCB WDK7-PCB            
032900                           BENA-PCB KVAH-PCB WDB6-PCB                     
033000                           WDD5-PCB.                                      
033100                                                                          
033200     PERFORM S04-FETCH-REQUEST-ARGUMENT                                   
033300     IF SUB-KDRC = 0                                                      
033400         PERFORM A-INIT                                                   
033500         PERFORM B-SKAPA-RT-UTSKRIFT                                      
033600     END-IF                                                               
033700                                                                          
033800     MOVE ZERO TO RETURN-CODE                                             
033900     GOBACK                                                               
034000     .                                                                    
034100     EJECT                                                                
034200 A-INIT SECTION.                                                          
034300                                                                          
034400                                                                          
034500     MOVE SPACE                TO RESP-AREA                               
034600                                  HDR-AREA                                
034700                                  RESP-AREA-HEAD                          
034800                                  RESP-AREA-LINE                          
034900                                  RESP-AREA-TEXT                          
035000     MOVE 'N'                  TO DATE-INTERVAL-SW                        
035100                                                                          
035200*    -- BEART SKA VARA SPACE I UNICODE                                    
035300     MOVE ALL X'20'            TO L154-BEART                              
035400                                                                          
035500     MOVE LOW-VALUE              TO W-WDA3F1KY-MIN-X                      
035600     MOVE HIGH-VALUE             TO W-WDA3F1KY-MAX-X                      
035700                                                                          
035800     MOVE FUNCTION CURRENT-DATE(1:8)                                      
035900                                 TO DAGENS-DATUM                          
036000     .                                                                    
036100     EJECT                                                                
036200 B-SKAPA-RT-UTSKRIFT    SECTION.                                          
036300                                                                          
036400     IF WZ04-SEND-IDCOM = ZERO                                            
036500        PERFORM S05-SEND-OPEN                                             
036600        MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                
036700     END-IF                                                               
036800                                                                          
036900     ACCEPT WS-YYMMDD      FROM DATE                                      
037000     ACCEPT WS-HHMM        FROM TIME                                      
037100                                                                          
037200     MOVE 001                        TO HDR-REQU-IDMSGVER                 
037300     MOVE 'RETURN-PERMIT'            TO HDR-IDOUTTYPE                     
037400     MOVE REQU-IDDC-REP              TO HDR-IDOUTREC(1:2)                 
037500     MOVE REQU-IDUSER IN REQU-AREA   TO HDR-IDOUTREC(3:8)                 
037600     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
037700*HDR                                                                      
037800     PERFORM S05-PUT-HEADER                                               
037900                                                                          
038000     MOVE +1                   TO INDX                                    
038100     PERFORM UNTIL INDX  > MAX-INDX                                       
038200        IF REQU-IDDISTR-REP(INDX) NOT = ALL '+'                           
038300           PERFORM BA-BEHANDLA-RT                                         
038400        END-IF                                                            
038500        ADD +1                TO INDX                                     
038600     END-PERFORM                                                          
038700                                                                          
038800     IF WZ04-SEND-IDCOM > ZERO                                            
038900        PERFORM S05-SEND-CLOSE                                            
039000        MOVE ZERO TO WZ04-SEND-IDCOM                                      
039100     END-IF                                                               
039200                                                                          
039300     .                                                                    
039400     EJECT                                                                
039500 BA-BEHANDLA-RT     SECTION.                                              
039600                                                                          
039700     MOVE LOW-VALUE            TO W-WDA3F1KY-MIN-X                        
039800     MOVE HIGH-VALUE           TO W-WDA3F1KY-MAX-X                        
039900                                                                          
040000     INSPECT REQU-IDDISTR-REP(INDX)                                       
040100                                REPLACING LEADING SPACE BY ZERO           
040200     INSPECT REQU-IDKUNDNR-REP(INDX)                                      
040300                                REPLACING LEADING SPACE BY ZERO           
040400     INSPECT REQU-IDRAPPNR-REP(INDX)                                      
040500                                REPLACING LEADING SPACE BY ZERO           
040600                                                                          
040700     MOVE REQU-IDDISTR-REP(INDX)     TO W-IDDISTR-ANM                     
040800                                        W-IDDISTR-RET-MIN                 
040900                                        W-IDDISTR-RET-MAX                 
041000     MOVE REQU-IDKUNDNR-REP(INDX)    TO W-IDKUNDNR-ANM                    
041100                                        W-IDKUNDNR-RET-MIN                
041200                                        W-IDKUNDNR-RET-MAX                
041300     MOVE REQU-IDRAPPNR-REP(INDX)    TO W-IDRAPPNR-ANM                    
041400                                        W-IDRAPPNR-RET-MIN                
041500                                        W-IDRAPPNR-RET-MAX                
041600                                                                          
041700     PERFORM IMS-GU-WLKREE01                                              
041800                                                                          
041900     IF SEGMENT-FINNS                                                     
042000                                                                          
042100        MOVE REQU-IDDC-REP            TO W-IDDC-RET-MIN                   
042200                                         W-IDDC-RET-MAX                   
042300        PERFORM BAA-KOLLA-OM-FLERA-KOLLIN                                 
042400        PERFORM S01-SKAPA-HUVUD                                           
042500                                                                          
042600*DOCUMENT HEADER                                                          
042700        PERFORM S05-PUT-REPORT-HEAD                                       
042800                                                                          
042900        PERFORM S03-LAES-WLKREE11                                         
043000                                                                          
043100*DOCUMENT REPORT-LINE & TEXT-LINE IN THIS LOOP                            
043200                                                                          
043300        PERFORM UNTIL SEGMENT-SAKNAS                                      
043400            PERFORM BAB-SKAPA-RAD                                         
043500            PERFORM S03-LAES-WLKREE11                                     
043600        END-PERFORM                                                       
043700     END-IF                                                               
043800     .                                                                    
043900     EJECT                                                                
044000 BAA-KOLLA-OM-FLERA-KOLLIN       SECTION.                                 
044100                                                                          
044200     PERFORM IMS-GU-WLRETG01                                              
044300     IF SEGMENT-FINNS                                                     
044400        PERFORM IMS-GN-WLRETG01                                           
044500        IF SEGMENT-FINNS                                                  
044600           MOVE JA               TO W-FLFLER-KOLLI                        
044700        ELSE                                                              
044800           MOVE NEJ              TO W-FLFLER-KOLLI                        
044900        END-IF                                                            
045000     ELSE                                                                 
045100        MOVE NEJ                 TO W-FLFLER-KOLLI                        
045200     END-IF                                                               
045300                                                                          
045400     .                                                                    
045500     EJECT                                                                
045600 BAB-SKAPA-RAD       SECTION.                                             
045700                                                                          
045800     PERFORM BABA-HAEMTA-ART-INFO                                         
045900                                                                          
046000     MOVE '2'                  TO L154-IDAFPRCD-2                         
046100     MOVE LEV-IDDC             TO L154-IDDC                               
046200     MOVE LEV-IDORDNR7         TO L154-IDORDNR7                           
046300     MOVE LEV-IDARTNR          TO L154-IDARTNR                            
046400     MOVE W-REKSIFFR           TO L154-REKSIFFR                           
046500     MOVE W-BEART              TO L154-BEART                              
046600     MOVE LEV-KVLEVANM-BEKR    TO L154-KVLEVANM-BEKR                      
046700     COMPUTE W-KVLEVANM-KVAR   =  LEV-KVLEVANM-BEKR -                     
046800                                  LEV-KVRETINL -                          
046900                                  LEV-KVAVV-KVANT -                       
047000                                  LEV-KVRETINL-SKR -                      
047100                                  LEV-KVAVV-KVAL -                        
047200                                  LEV-KVANTAL-ILI                         
047300     MOVE W-KVLEVANM-KVAR      TO L154-KVLEVANM-KVAR                      
047400                                                                          
047500     MOVE LEV-KDANMORS         TO L154-KDANMORS                           
047600                                                                          
047700                                                                          
047800       MOVE W-ADLAGOMR         TO L154-ADLAGOMR                           
047900       MOVE W-ADGANG           TO L154-ADGANG                             
048000       MOVE W-ADPLATS          TO L154-ADPLATS                            
048100                                                                          
048200                                                                          
048300     MOVE W-KDERS              TO L154-KDERS                              
048400     MOVE W-KDSORT             TO L154-KDSORT                             
048500     MOVE W-KDFGPRIO           TO L154-KDFGPRIO                           
048600                                                                          
048700     MOVE LEV-IDRADNR          TO W-IDRADNR                               
048800                                                                          
048900     PERFORM IMS-GNP-WLKREE21                                             
049000                                                                          
049100                                                                          
049200* DOCUMENT DETAIL-LINE                                                    
049300     PERFORM S05-PUT-REPORT-LINE                                          
049400                                                                          
049500     MOVE '3'                  TO L154-IDAFPRCD-3                         
049600                                                                          
049700     IF SEGMENT-FINNS                                                     
049800       IF TXT-TEANMNOT-REG(1) = SPACE AND                                 
049900          TXT-TEANMNOT-REG(2) = SPACE AND                                 
050000          TXT-TEANMNOT-REG(3) = SPACE AND                                 
050100          TXT-TEANMNOT-ADM(1) = SPACE AND                                 
050200          TXT-TEANMNOT-ADM(2) = SPACE AND                                 
050300          TXT-TEANMNOT-ADM(3) = SPACE                                     
050400         CONTINUE                                                         
050500       ELSE                                                               
050600         IF TXT-TEANMNOT-REG(1) NOT  = SPACE                              
050700           MOVE 'REG'                TO L154-TYP                          
050800           MOVE TXT-TEANMNOT-REG(1)  TO L154-TEXT                         
050900           INSPECT L154-TEXT    REPLACING ALL '¤' BY 'U'                  
051000                                                                          
051100* DOCUMENT TEXT-LINE                                                      
051200           PERFORM S05-PUT-REPORT-TEXT                                    
051300         END-IF                                                           
051400         IF TXT-TEANMNOT-REG(2) NOT  = SPACE                              
051500           MOVE 'REG'                TO L154-TYP                          
051600           MOVE TXT-TEANMNOT-REG(2)  TO L154-TEXT                         
051700           INSPECT L154-TEXT    REPLACING ALL '¤' BY 'U'                  
051800                                                                          
051900* DOCUMENT TEXT-LINE                                                      
052000           PERFORM S05-PUT-REPORT-TEXT                                    
052100         END-IF                                                           
052200         IF TXT-TEANMNOT-REG(3) NOT  = SPACE                              
052300           MOVE 'REG'                TO L154-TYP                          
052400           MOVE TXT-TEANMNOT-REG(3)  TO L154-TEXT                         
052500           INSPECT L154-TEXT    REPLACING ALL '¤' BY 'U'                  
052600                                                                          
052700* DOCUMENT TEXT-LINE                                                      
052800           PERFORM S05-PUT-REPORT-TEXT                                    
052900         END-IF                                                           
053000         IF TXT-TEANMNOT-ADM(1) NOT  = SPACE                              
053100           MOVE 'ADM'                TO L154-TYP                          
053200           MOVE TXT-TEANMNOT-ADM(1)  TO L154-TEXT                         
053300           INSPECT L154-TEXT    REPLACING ALL '¤' BY 'U'                  
053400                                                                          
053500* DOCUMENT TEXT-LINE                                                      
053600           PERFORM S05-PUT-REPORT-TEXT                                    
053700         END-IF                                                           
053800         IF TXT-TEANMNOT-ADM(2) NOT  = SPACE                              
053900           MOVE 'ADM'                TO L154-TYP                          
054000           MOVE TXT-TEANMNOT-ADM(2)  TO L154-TEXT                         
054100           INSPECT L154-TEXT    REPLACING ALL '¤' BY 'U'                  
054200                                                                          
054300* DOCUMENT TEXT-LINE                                                      
054400           PERFORM S05-PUT-REPORT-TEXT                                    
054500         END-IF                                                           
054600         IF TXT-TEANMNOT-ADM(3) NOT  = SPACE                              
054700           MOVE 'ADM'                TO L154-TYP                          
054800           MOVE TXT-TEANMNOT-ADM(3)  TO L154-TEXT                         
054900           INSPECT L154-TEXT    REPLACING ALL '¤' BY 'U'                  
055000                                                                          
055100* DOCUMENT TEXT-LINE                                                      
055200           PERFORM S05-PUT-REPORT-TEXT                                    
055300         END-IF                                                           
055400       END-IF                                                             
055500     END-IF                                                               
055600     PERFORM IMS-GU-W6KVAH11                                              
055700*** NY KOD                                                                
055800     IF SEGMENT-FINNS                                                     
055900*     IF LEV-KDANMORS NOT = '72'                                          
056000      PERFORM S02-DATE-INTERVAL                                           
056100      IF W-DATE-INTERVAL-OK                                               
056200       IF INFO-TEKVAINF-EXT(1) NOT = SPACE                                
056300           MOVE 'QUA'            TO L154-TYP                              
056400         MOVE INFO-TEKVAINF-EXT(1) TO L154-TEXT                           
056500         INSPECT L154-TEXT     REPLACING ALL '#' BY ' '                   
056600                                                                          
056700* DOCUMENT TEXT-LINE                                                      
056800           PERFORM S05-PUT-REPORT-TEXT                                    
056900       END-IF                                                             
057000       IF INFO-TEKVAINF-EXT(2) NOT = SPACE                                
057100           MOVE 'QUA'            TO L154-TYP                              
057200         MOVE INFO-TEKVAINF-EXT(2) TO L154-TEXT                           
057300         INSPECT L154-TEXT     REPLACING ALL '#' BY ' '                   
057400                                                                          
057500* DOCUMENT TEXT-LINE                                                      
057600           PERFORM S05-PUT-REPORT-TEXT                                    
057700       END-IF                                                             
057800       IF INFO-TEKVAINF-EXT(3) NOT = SPACE                                
057900           MOVE 'QUA'            TO L154-TYP                              
058000         MOVE INFO-TEKVAINF-EXT(3) TO L154-TEXT                           
058100         INSPECT L154-TEXT     REPLACING ALL '#' BY ' '                   
058200                                                                          
058300* DOCUMENT TEXT-LINE                                                      
058400           PERFORM S05-PUT-REPORT-TEXT                                    
058500       END-IF                                                             
058600       IF INFO-TEKVAINF-EXT(4) NOT = SPACE                                
058700           MOVE 'QUA'            TO L154-TYP                              
058800         MOVE INFO-TEKVAINF-EXT(4) TO L154-TEXT                           
058900         INSPECT L154-TEXT     REPLACING ALL '#' BY ' '                   
059000                                                                          
059100* DOCUMENT TEXT-LINE                                                      
059200           PERFORM S05-PUT-REPORT-TEXT                                    
059300       END-IF                                                             
059400       IF INFO-TEKVAINF-EXT(5) NOT = SPACE                                
059500           MOVE 'QUA'            TO L154-TYP                              
059600         MOVE INFO-TEKVAINF-EXT(5) TO L154-TEXT                           
059700         INSPECT L154-TEXT     REPLACING ALL '#' BY ' '                   
059800                                                                          
059900* DOCUMENT TEXT-LINE                                                      
060000           PERFORM S05-PUT-REPORT-TEXT                                    
060100       END-IF                                                             
060200       IF INFO-TEKVAINF-EXT(6) NOT = SPACE                                
060300           MOVE 'QUA'            TO L154-TYP                              
060400         MOVE INFO-TEKVAINF-EXT(6) TO L154-TEXT                           
060500         INSPECT L154-TEXT     REPLACING ALL '#' BY ' '                   
060600                                                                          
060700* DOCUMENT TEXT-LINE                                                      
060800           PERFORM S05-PUT-REPORT-TEXT                                    
060900       END-IF                                                             
061000       IF INFO-TEKVAINF-EXT(7) NOT = SPACE                                
061100           MOVE 'QUA'            TO L154-TYP                              
061200         MOVE INFO-TEKVAINF-EXT(7) TO L154-TEXT                           
061300         INSPECT L154-TEXT     REPLACING ALL '#' BY ' '                   
061400                                                                          
061500* DOCUMENT TEXT-LINE                                                      
061600           PERFORM S05-PUT-REPORT-TEXT                                    
061700       END-IF                                                             
061800      END-IF                                                              
061900     END-IF                                                               
062000     .                                                                    
062100     EJECT                                                                
062200                                                                          
062300 BABA-HAEMTA-ART-INFO             SECTION.                                
062400                                                                          
062500     MOVE LEV-IDARTNR          TO W-IDARTNR                               
062600     MOVE LEV-IDDC-RET         TO W-IDDC-X                                
062700                                  W-IDDC-B6                               
062800                                                                          
062900     PERFORM IMS-GU-WDB601                                                
063000                                                                          
063100****  MAN SKALL VISA LAGERPLATSEN FÖR DET MOTTAGANDE DC'T.                
063200     IF DCS-NDC-CN OR DCS-NDC-PF OR DCS-SDC OR DCS-NDC-NA OR              
063300        DCS-NDC-OTHERS OR DCS-NDC-SA                                      
063400                                                                          
063500       PERFORM IMS-GU-WLARTC01                                            
063600       IF SEGMENT-FINNS                                                   
063700          MOVE ART-REKSIFFR      TO W-REKSIFFR                            
063800          MOVE ART-KDSORT        TO W-KDSORT                              
063900          PERFORM IMS-GNP-WLARTC11                                        
064000          IF SEGMENT-FINNS                                                
064100            MOVE CLAG-KDERS     TO W-KDERS                                
064200            MOVE CLAG-KDARTURS  TO L154-KDARTURS                          
064300          ELSE                                                            
064400            MOVE ZERO           TO W-KDERS                                
064500            MOVE SPACE          TO L154-KDARTURS                          
064600          END-IF                                                          
064700       ELSE                                                               
064800          MOVE ZERO              TO W-REKSIFFR                            
064900          MOVE SPACE             TO W-KDSORT                              
065000       END-IF                                                             
065100       PERFORM IMS-GU-WDK711                                              
065200       IF SEGMENT-FINNS                                                   
065300          MOVE SLAG-ADLAGOMR  TO W-ADLAGOMR                               
065400          MOVE SLAG-ADGANG    TO W-ADGANG                                 
065500          MOVE SLAG-ADPLATS   TO W-ADPLATS                                
065600       ELSE                                                               
065700          MOVE ZERO           TO W-ADLAGOMR                               
065800                                 W-ADGANG                                 
065900                                 W-ADPLATS                                
066000                                 W-KDERS                                  
066100       END-IF                                                             
066200       MOVE DCS-IDLANDX2      TO W-IDLAND                                 
066300       PERFORM IMS-GU-WDK712                                              
066400       IF SEGMENT-FINNS                                                   
066500          MOVE LART-KDARTURS  TO L154-KDARTURS                            
066600       ELSE                                                               
066700          MOVE SPACE          TO L154-KDARTURS                            
066800       END-IF                                                             
066900     ELSE                                                                 
067000       PERFORM IMS-GU-WLARTC01                                            
067100       IF SEGMENT-FINNS                                                   
067200          MOVE ART-REKSIFFR      TO W-REKSIFFR                            
067300          MOVE ART-KDSORT        TO W-KDSORT                              
067400          PERFORM IMS-GNP-WLARTC11                                        
067500          IF SEGMENT-FINNS                                                
067600             MOVE CLAG-ADLAGOMR  TO W-ADLAGOMR                            
067700             MOVE CLAG-ADGANG    TO W-ADGANG                              
067800             MOVE CLAG-ADPLATS   TO W-ADPLATS                             
067900             MOVE CLAG-KDERS     TO W-KDERS                               
068000             MOVE CLAG-KDARTURS  TO L154-KDARTURS                         
068100          ELSE                                                            
068200             MOVE ZERO           TO W-ADLAGOMR                            
068300                                    W-ADGANG                              
068400                                    W-ADPLATS                             
068500                                    W-KDERS                               
068600             MOVE SPACE          TO L154-KDARTURS                         
068700          END-IF                                                          
068800       ELSE                                                               
068900          MOVE ZERO              TO W-ADLAGOMR                            
069000                                    W-ADGANG                              
069100                                    W-ADPLATS                             
069200                                    W-KDERS                               
069300          MOVE SPACE             TO L154-KDARTURS                         
069400          MOVE SPACE             TO W-KDSORT                              
069500                                                                          
069600       END-IF                                                             
069700     END-IF                                                               
069800                                                                          
069900     PERFORM IMS-GU-WDD501                                                
070000     IF SEGMENT-FINNS                                                     
070100       MOVE ART-KDFGPRIO       TO W-KDFGPRIO                              
070200     ELSE                                                                 
070300       MOVE ZERO               TO W-KDFGPRIO                              
070400     END-IF                                                               
070500                                                                          
070600     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
070700     IF DCS-UNICODE-IDSKYLT                                               
070800        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
070900     ELSE                                                                 
071000        MOVE '278 '             TO TRAUTF8-KDCP                           
071100     END-IF                                                               
071200                                                                          
071300     PERFORM IMS-GU-WLBENA11                                              
071400     IF SEGMENT-FINNS                                                     
071500      MOVE TEXT-BEART        TO TRAUTF8-TECONV-FROM                       
071600     ELSE                                                                 
071700      MOVE SPACE TO TRAUTF8-TECONV-FROM                                   
071800     END-IF                                                               
071900     IF TRAUTF8-TECONV-FROM = SPACES                                      
072000      MOVE 'GB'  TO W-IDSKYLT                                             
072100      MOVE '278' TO TRAUTF8-KDCP                                          
072200      PERFORM IMS-GU-WLBENA11                                             
072300      MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
072400     END-IF                                                               
072500                                                                          
072600*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
072700     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
072800                                                                          
072900*    -- MOVE CONVERTED DESCRIPTION TO OUTPUT WORK FIELD                   
073000     MOVE TRAUTF8-TECONV-TO    TO W-BEART                                 
073100     .                                                                    
073200                                                                          
073300     EJECT                                                                
073400 S01-SKAPA-HUVUD     SECTION.                                             
073500                                                                          
073600                                                                          
073700     MOVE '1'                     TO L154-IDAFPRCD-1                      
073800     MOVE ANM-IDDISTR             TO L154-IDDISTR                         
073900     MOVE ANM-IDKUNDNR            TO L154-IDKUNDNR                        
074000     MOVE ANM-IDRAPPNR            TO L154-IDRAPPNR                        
074100     MOVE W-FLFLER-KOLLI          TO L154-FLFLER-KOLLI                    
074200                                                                          
074300     .                                                                    
074400     EJECT                                                                
074500                                                                          
074600 S02-DATE-INTERVAL SECTION.                                               
074700                                                                          
074800      COMPUTE WS-DAREGDAT = 99999999 - INFO-DAREGDAT-9KOMPL               
074900      MOVE WS-SEKEL-D               TO DAG-TISEKEL-FOM                    
075000      MOVE WS-AAMMDD                TO DAG-TIAAMMDD-FOM                   
075100      MOVE DAGENS-DATUM-SEKEL       TO DAG-TISEKEL-TOM                    
075200      MOVE DAGENS-DATUM-AAMMDD      TO DAG-TIAAMMDD-TOM                   
075300      MOVE '001'                    TO DAG-KDCALL                         
075400      CALL WDAGKONV USING DAG-KDCALL, DAG-DATUM-AREA,                     
075500                          DAG-KDSVAR                                      
075600      IF DAG-KDSVAR = SPACE                                               
075700         IF DAG-KVKALDAG < 00090                                          
075800            SET W-DATE-INTERVAL-OK  TO TRUE                               
075900         ELSE                                                             
076000            SET W-DATE-INTERVAL-EJ  TO TRUE                               
076100         END-IF                                                           
076200      END-IF                                                              
076300     .                                                                    
076400     EJECT                                                                
076500                                                                          
076600 S03-LAES-WLKREE11        SECTION.                                        
076700                                                                          
076800     MOVE NEJ                    TO OKOD-FL-RETILL                        
076900                                    OKOD-FL-INTERNUPPACKNING              
077000     PERFORM IMS-GNP-WLKREE11                                             
077100     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
077200                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
077300        IF LEV-KDKREBEH(1:1) = 'Y'   OR                                   
077400           LEV-KDKREBEH(1:1) = 'J'   OR                                   
077500           LEV-KDKREBEH(1:1) = 'C'   OR                                   
077600           LEV-KDKREBEH      = 'D01' OR                                   
077700           LEV-KDKREBEH      = 'D02' OR                                   
077800           LEV-KDKREBEH      = 'D03'                                      
077900           MOVE LEV-KDANMORS TO OKOD-KDANMORS                             
078000*--ANROPA KONTROLL AV ORSAKSKODER                                         
078100           CALL W418OKOD USING OKOD-W418OKOD                              
078200        END-IF                                                            
078300        IF OKOD-FL-RETILL = 'J' OR                                        
078400           OKOD-FL-INTERNUPPACKNING = 'J'                                 
078500           CONTINUE                                                       
078600        ELSE                                                              
078700          PERFORM IMS-GNP-WLKREE11                                        
078800        END-IF                                                            
078900     END-PERFORM                                                          
079000                                                                          
079100     .                                                                    
079200     EJECT                                                                
079300* DISPATCHER-SEKTIONER                                                    
079400     SKIP3                                                                
079500 S04-FETCH-REQUEST-ARGUMENT SECTION.                                      
079600                                                                          
079700     MOVE 'GETARG'                        TO SUB-KDFUNC                   
079800     MOVE 'CARPARTS.LDC.PRRETPERMITBG'    TO SUB-ADDISPABS                
079900     MOVE LENGTH OF REQU-AREA             TO SUB-KVDLEN                   
080000                                                                          
080100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
080200                                                                          
080300     IF SUB-KDRC > 0                                                      
080400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
080500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
080600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
080700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
080800     END-IF                                                               
080900     .                                                                    
081000     SKIP3                                                                
081100     SKIP3                                                                
081200 S05-SEND-OPEN SECTION.                                                   
081300                                                                          
081400     MOVE 'CARPARTS.DAP.DISTRDOCWEB'      TO SEND-ADDISPABS               
081500     MOVE 'OPEN'                          TO SEND-KDFUNC                  
081600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
081700                         SEND-OPEN-AREA                                   
081800     IF SEND-KDRC > ZERO                                                  
081900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
082000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
082100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
082200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
082300     END-IF                                                               
082400     .                                                                    
082500     SKIP3                                                                
082600 S05-PUT-HEADER SECTION.                                                  
082700                                                                          
082800     MOVE 'PUT'                           TO SEND-KDFUNC                  
082900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
083000     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
083100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
083200                         SEND-KVDLEN                                      
083300                         HDR-AREA                                         
083400     IF SEND-KDRC > ZERO                                                  
083500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
083600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
083700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
083800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
083900     END-IF                                                               
084000     .                                                                    
084100     EJECT                                                                
084200 S05-PUT-REPORT-HEAD    SECTION.                                          
084300                                                                          
084400     MOVE 'PUT'                           TO SEND-KDFUNC                  
084500     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
084600     MOVE LENGTH OF RESP-AREA-HEAD        TO SEND-KVDLEN                  
084700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
084800                         SEND-KVDLEN                                      
084900                         RESP-AREA-HEAD                                   
085000     IF SEND-KDRC > ZERO                                                  
085100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
085200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
085300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
085400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
085500     END-IF                                                               
085600     .                                                                    
085700     SKIP3                                                                
085800 S05-PUT-REPORT-LINE    SECTION.                                          
085900                                                                          
086000     MOVE 'PUT'                           TO SEND-KDFUNC                  
086100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
086200     MOVE LENGTH OF RESP-AREA-LINE        TO SEND-KVDLEN                  
086300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
086400                         SEND-KVDLEN                                      
086500                         RESP-AREA-LINE                                   
086600     IF SEND-KDRC > ZERO                                                  
086700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
086800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
086900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
087000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
087100     END-IF                                                               
087200     .                                                                    
087300     SKIP3                                                                
087400 S05-PUT-REPORT-TEXT SECTION.                                             
087500                                                                          
087600     MOVE 'PUT'                           TO SEND-KDFUNC                  
087700     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
087800     MOVE LENGTH OF RESP-AREA-TEXT        TO SEND-KVDLEN                  
087900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
088000                         SEND-KVDLEN                                      
088100                         RESP-AREA-TEXT                                   
088200     IF SEND-KDRC > ZERO                                                  
088300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
088400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
088500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
088600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
088700     END-IF                                                               
088800     .                                                                    
088900     SKIP3                                                                
089000 S05-SEND-CLOSE SECTION.                                                  
089100                                                                          
089200     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
089300     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
089400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
089500     .                                                                    
089600     EJECT                                                                
089700* --- IMS SEKTIONER ---                                                   
089800     SKIP3                                                                
089900 IMS-GU-WLKREE01    SECTION.                                              
090000                                                                          
090100     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
090200          DELIMITED BY SIZE INTO SSA1                                     
090300     MOVE '  GE' TO GODK-STATUSKODER                                      
090400     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA1 SSA1                     
090500     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
090600     PERFORM IMS-STATUSKONTROLL                                           
090700     .                                                                    
090800     SKIP2                                                                
090900 IMS-GNP-WLKREE11    SECTION.                                             
091000                                                                          
091100     MOVE 'WLKREE11'        TO SSA1                                       
091200     MOVE '  GE' TO GODK-STATUSKODER                                      
091300     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA2 SSA1                    
091400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
091500     PERFORM IMS-STATUSKONTROLL                                           
091600     .                                                                    
091700     SKIP2                                                                
091800 IMS-GNP-WLKREE21    SECTION.                                             
091900                                                                          
092000     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
092100          DELIMITED BY SIZE INTO SSA1                                     
092200     MOVE 'WLKREE21'        TO SSA2                                       
092300     MOVE '  GE' TO GODK-STATUSKODER                                      
092400     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA3 SSA1 SSA2               
092500     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
092600     PERFORM IMS-STATUSKONTROLL                                           
092700     .                                                                    
092800     SKIP2                                                                
092900 IMS-GU-WLRETG01    SECTION.                                              
093000                                                                          
093100     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
093200                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
093300          DELIMITED BY SIZE INTO SSA1                                     
093400     MOVE '  GE' TO GODK-STATUSKODER                                      
093500     CALL CBLTDLI USING GU RETG-PCB DLI-IO-AREA3 SSA1                     
093600     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
093700     PERFORM IMS-STATUSKONTROLL                                           
093800     .                                                                    
093900     SKIP2                                                                
094000 IMS-GN-WLRETG01    SECTION.                                              
094100                                                                          
094200     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
094300                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
094400          DELIMITED BY SIZE INTO SSA1                                     
094500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
094600     CALL CBLTDLI USING GN RETG-PCB DLI-IO-AREA3 SSA1                     
094700     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
094800     PERFORM IMS-STATUSKONTROLL                                           
094900     .                                                                    
095000     SKIP2                                                                
095100 IMS-GU-WLARTC01     SECTION.                                             
095200                                                                          
095300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
095400          DELIMITED BY SIZE INTO SSA1                                     
095500     MOVE '  GE' TO GODK-STATUSKODER                                      
095600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA3 SSA1                     
095700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
095800     PERFORM IMS-STATUSKONTROLL                                           
095900     .                                                                    
096000                                                                          
096100 IMS-GNP-WLARTC11     SECTION.                                            
096200                                                                          
096300     MOVE 'WLARTC11'   TO  SSA1                                           
096400     MOVE '  GE' TO GODK-STATUSKODER                                      
096500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA3 SSA1                    
096600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
096700     PERFORM IMS-STATUSKONTROLL                                           
096800     .                                                                    
096900                                                                          
097000 IMS-GU-WDK711       SECTION.                                             
097100                                                                          
097200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
097300          DELIMITED BY SIZE INTO SSA1                                     
097400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
097500          DELIMITED BY SIZE INTO SSA2                                     
097600     MOVE '  GE' TO GODK-STATUSKODER                                      
097700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
097800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
097900     PERFORM IMS-STATUSKONTROLL                                           
098000     .                                                                    
098100                                                                          
098200 IMS-GU-WDK712       SECTION.                                             
098300                                                                          
098400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
098500          DELIMITED BY SIZE INTO SSA1                                     
098600     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
098700          DELIMITED BY SIZE INTO SSA2                                     
098800     MOVE '  GE' TO GODK-STATUSKODER                                      
098900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
099000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
099100     PERFORM IMS-STATUSKONTROLL                                           
099200     .                                                                    
099300                                                                          
099400 IMS-GU-WLBENA11     SECTION.                                             
099500                                                                          
099600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
099700            DELIMITED BY SIZE INTO SSA1                                   
099800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
099900            DELIMITED BY SIZE INTO SSA2                                   
100000     MOVE '  ' TO GODK-STATUSKODER                                        
100100     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA3 SSA1 SSA2               
100200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
100300     PERFORM IMS-STATUSKONTROLL                                           
100400     .                                                                    
100500     EJECT                                                                
100600 IMS-GU-W6KVAH11     SECTION.                                             
100700                                                                          
100800     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
100900            DELIMITED BY SIZE INTO SSA1                                   
101000     STRING 'W6KVAH11(KDKVAINF =' W-KDKVAINF-X ')'                        
101100            DELIMITED BY SIZE INTO SSA2                                   
101200     MOVE '  GE' TO GODK-STATUSKODER                                      
101300     CALL CBLTDLI USING GU  KVAH-PCB DLI-IO-AREA3 SSA1 SSA2               
101400     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
101500     PERFORM IMS-STATUSKONTROLL                                           
101600     .                                                                    
101700     EJECT                                                                
101800 IMS-GU-WDB601    SECTION.                                                
101900                                                                          
102000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
102100     DELIMITED BY SIZE INTO SSA1                                          
102200     MOVE '    ' TO GODK-STATUSKODER                                      
102300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
102400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
102500     PERFORM IMS-STATUSKONTROLL                                           
102600     .                                                                    
102700     EJECT                                                                
102800 IMS-GU-WDD501 SECTION.                                                   
102900     STRING 'WDD501  (IDARTNR  =' W-IDARTNR-X ')'                         
103000          DELIMITED BY SIZE INTO SSA1                                     
103100     MOVE '  GE' TO GODK-STATUSKODER                                      
103200     CALL CBLTDLI USING GU WDD5-PCB DLI-IO-AREA-D501 SSA1                 
103300     MOVE WDD5-STATUS-CODE TO STATUS-WS                                   
103400     PERFORM IMS-STATUSKONTROLL                                           
103500     .                                                                    
103600 IMS-STATUSKONTROLL SECTION.                                              
103700                                                                          
103800     SET STATUS-IX TO 1                                                   
103900     SEARCH GODK-STATUS                                                   
104000       AT END                                                             
104100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
104200         DELIMITED BY SIZE INTO FELTEXT                                   
104300         CALL FELLOG                                                      
104400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
104500         CONTINUE                                                         
104600     END-SEARCH                                                           
104700     .                                                                    
