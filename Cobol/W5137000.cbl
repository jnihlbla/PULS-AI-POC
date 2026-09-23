000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5137000.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   05/06/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER IN FIL W51370 OCH SKAPAR DAP POSTER                        
000900*        LÄSER IN FIL W51372 OCH SKAPAR DAP POSTER MANAGEMENT             
001000*                                                                         
001100*    ETRACKER: 8218038 CORRECTION OF PERIOD                               
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- FIL MED ACCUMULATED ADJUSTMENTS                            
002100     SELECT W51370                     ASSIGN TO W51370D1.                
002200     SKIP2                                                                
002300*          --- FIL MED ACCUMULATED ADJUSTMENTS MANAGEMENT                 
002400     SELECT W51372                     ASSIGN TO W51370D2.                
002500     SKIP2                                                                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W51370                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  -COPY W513701      -L.                                               
003600     SKIP3                                                                
003700                                                                          
003800 FD  W51372                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W513702      -L.                                               
004300     SKIP3                                                                
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W5137000'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  WS-ADRESS                   PIC X(50)                                
005100                   VALUE 'CARPARTS.DAP.DISTRDOC'.                         
005200 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
005300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005400 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
005500 77  KDRC-DISPLAY                PIC Z(5).                                
005600 77  INDX                        PIC S9(9) COMP-3.                        
005700 77  ANTAL-POSTER                PIC S9(9) COMP-3.                        
005800 77  W-ADCITY                    PIC X(20).                               
005900 77  W-IDLANDX2                  PIC X(2).                                
006000 77  W-TIAARP                    PIC 9(4).                                
006100 77  W-IDDC                      PIC X(2).                                
006200 77  W-IDDC-TOT                  PIC X(2).                                
006300 77  WS-IDFTG                    PIC 9(2)      VALUE ZERO.                
006400 77  WDCS-KDMFUP                 PIC X(2)    VALUE SPACE.                 
006500                                                                          
006600 77  SUM-KVJUSTKV-TOT            PIC 9(8)      VALUE ZERO.                
006700 77  SUM-KVJUSTKV-NEG            PIC 9(8)      VALUE ZERO.                
006800 77  SUM-SUARTSTD-NEG            PIC 9(8)V9(2) VALUE ZERO.                
006900 77  SUM-KVJUSTKV-POS            PIC 9(8)      VALUE ZERO.                
007000 77  SUM-SUARTSTD-POS            PIC 9(8)V9(2) VALUE ZERO.                
007100 77  SUM-KVJUSTKV-ZERO           PIC 9(8)      VALUE ZERO.                
007200                                                                          
007300 01  WS-YYMMDDHHMM.                                                       
007400     03 WS-YYMMDD                PIC  9(6).                               
007500     03 WS-TIME                  PIC  9(4).                               
007600                                                                          
007700 01  WS-HHMMSSTH                 PIC  9(8).                               
007800 01  FILLER REDEFINES WS-HHMMSSTH.                                        
007900       03  WS-HHMM               PIC 9(4).                                
008000       03  WS-SSTH               PIC 9(4).                                
008100 01  WS-IDDC-WEB                 PIC X(2) VALUE SPACE.                    
008200*---------------------------------------                                  
008300                                                                          
008400 01  W-IDDC-B6-X.                                                         
008500     03 W-IDDC-B6        PIC X(2).                                        
008600                                                                          
008700     EJECT                                                                
008800*---------------------------------------                                  
008900                                                                          
009000     SKIP2                                                                
009100 01  FELTEXT.                                                             
009200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009400                                                                          
009500 77  W51370-EOF-SW               PIC X       VALUE 'N'.                   
009600     88  END-OF-W51370                       VALUE 'J'.                   
009700     EJECT                                                                
009800 77  W51372-EOF-SW               PIC X       VALUE 'N'.                   
009900     88  END-OF-W51372                       VALUE 'J'.                   
010000     EJECT                                                                
010100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010200 01  FILLER REDEFINES DAGENS-DATUM.                                       
010300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
010400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010600     EJECT                                                                
010700 01  TABENTRY-PARM.                                                       
010800     03  STEGLAANGD              PIC S9(9) COMP.                          
010900     03  ANTAL                   PIC S9(9) COMP.                          
011000     03  NYCKELLAANGD            PIC S9(9) COMP.                          
011100 01  FILLER                      PIC X(16) VALUE 'SORT-TABELL'.           
011200 01  SORT-TABELL.                                                         
011300     03  TAB-RAD OCCURS 500.                                              
011400        05  TAB-SORT-BEGREPP1.                                            
011500            07 TAB-KDMFUP        PIC X(2).                                
011600            07 TAB-IDDC          PIC X(2).                                
011700            07 TAB-KDJUSTYP      PIC 9.                                   
011800        05  TAB-IDAFPRCD         PIC X(10).                               
011900        05  TAB-TIAARP           PIC 9(4).                                
012000        05  TAB-KVJUSTKV-TOT     PIC 9(7).                                
012100        05  TAB-SUARTSTD-TOT     PIC 9(8)V9(2).                           
012200        05  TAB-KVJUSTKV-NEG     PIC 9(7).                                
012300        05  TAB-SUARTSTD-NEG     PIC 9(8)V9(2).                           
012400        05  TAB-KVJUSTKV-POS     PIC 9(7).                                
012500        05  TAB-SUARTSTD-POS     PIC 9(8)V9(2).                           
012600        05  TAB-KVJUSTKV-ZERO    PIC 9(7).                                
012700        05  TAB-IDLANDX2         PIC X(2).                                
012800        05  TAB-ADCITY           PIC X(20).                               
012900                                                                          
013000 01  DYNAMISKA-SUBPROGRAM.                                                
013100*                                                                         
013200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
013700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013800     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
013900     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
014000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
014100     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
014200     EJECT                                                                
014300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
014400                                                                          
014500*01  -COPY WDATKORT                                                       
014600*01  -COPY WDATAREA                                                       
014700*    --- PARAMETRAR TILL WL10WBDC                                         
014800*01  -COPY WL10WBDC                                                       
014900     EJECT                                                                
015000     EJECT                                                                
015100*    --- PARAMETERS TO ABEND                                              
015200                                                                          
015300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
015500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
015600                                                                          
015700*    --- PARAMETRAR TILL POSTSUM                                          
015800*                                                                         
015900*01  -COPY W0005   -PRE  POSTSUM-                                         
016000     EJECT                                                                
016100 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
016200*01  -COPY WZ01SEND                                                       
016300     EJECT                                                                
016400 01  IN-AREA-START               PIC X(24)   VALUE                        
016500                                             'IN-AREA-START'.             
016600     SKIP2                                                                
016700                                                                          
016800*01  AREA -COPY W513701     -PRE IN-                                      
016900     EJECT                                                                
017000 01  IN-AREA2-START               PIC X(24)   VALUE                       
017100                                             'IN-AREA2-START'.            
017200     SKIP2                                                                
017300                                                                          
017400*01  AREA -COPY W513702     -PRE IN2-                                     
017500     EJECT                                                                
017600 01  UT-AREA-START               PIC X(24)   VALUE                        
017700                                             'UT-AREA-START'.             
017800     SKIP2                                                                
017900 01  HDR-AREA.                                                            
018000*   03  -COPY WZ01REQU -PRE HDR-                                          
018100*   03  -COPY WZ04HDR                                                     
018200*                                                                         
018300 01  HDR-AREA2.                                                           
018400*   03  -COPY WZ01REQU -PRE HDR2-                                         
018500*   03  -COPY WZ04HDR  -PRE HDR2-                                         
018600*                                                                         
018700 01  LINE-AREA                   PIC X(24)    VALUE 'LINE-AREA'.          
018800 01  DOC-LINE-AREA.                                                       
018900*    03 -COPY W513701 -PRE LINE-                                          
019000     EJECT                                                                
019100     SKIP3                                                                
019200 01  LINE-AREA2                  PIC X(24)    VALUE 'LINE-AREA2'.         
019300 01  DOC-LINE-AREA2.                                                      
019400*    03 -COPY W513703 -PRE LINE2-                                         
019500     EJECT                                                                
019600     SKIP3                                                                
019700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
019800 01   DLI-IO-AREA-B601.                                                   
019900*     03  -COPY WDB601                                                    
020000                                                                          
020100                                                                          
020200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020300*    --- STATUS-KOD FRÅN IMS                                              
020400 01  STATUS-WS                   PIC XX.                                  
020500     88  SEGMENT-FINNS                       VALUE '  '.                  
020600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
020900     88  IMS-EJ-OK                           VALUE 'XD'.                  
021000     SKIP2                                                                
021100 01  GODK-STATUSKODER.                                                    
021200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021300     SKIP3                                                                
021400 01  SSA1                        PIC X(64).                               
021500 01  SSA2                        PIC X(64).                               
021600     EJECT                                                                
021700*    --- IMS FUNKTIONSKODER                                               
021800*01  -COPY W0003                                                          
021900     EJECT                                                                
022000                                                                          
022100 LINKAGE SECTION.                                                         
022200*01  -COPY W0009   -PRE MSG-                                              
022300     EJECT                                                                
022400 01  DAP-PCB              PIC X.                                          
022500     EJECT                                                                
022600*01  -COPY W0008 -PRE WDB6-                                               
022700     05  FILLER           PIC X.                                          
022800                                                                          
022900 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB.                      
023000 MAIN SECTION.                                                            
023100     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB.                      
023200                                                                          
023300     SKIP2                                                                
023400     PERFORM A-INIT                                                       
023500     PERFORM S01-LAES-W51370                                              
023600     IF NOT END-OF-W51370                                                 
023700       MOVE ZERO TO WS-IDDC-WEB                                           
023800       PERFORM UNTIL END-OF-W51370                                        
023900         MOVE IN-IDDC TO W-IDDC-B6                                        
024000         PERFORM IMS-GU-WDB601                                            
024100         IF DCS-FLWEBDC = JA                                              
024200           PERFORM B-WEB-LISTA                                            
024300         END-IF                                                           
024400         PERFORM S01-LAES-W51370                                          
024500       END-PERFORM                                                        
024600       PERFORM S05-SEND-CLOSE                                             
024700     END-IF                                                               
024800                                                                          
024900* LISTA 2 MANAGEMENT                                                      
025000     MOVE +0 TO INDX                                                      
025100     PERFORM S01-LAES-W51372                                              
025200     IF NOT END-OF-W51372                                                 
025300       PERFORM UNTIL END-OF-W51372                                        
025400         MOVE IN2-IDDC TO W-IDDC-B6                                       
025500         PERFORM IMS-GU-WDB601                                            
025600         IF DCS-FLWEBDC = JA                                              
025700           PERFORM C-FYLL-TABELL                                          
025800         END-IF                                                           
025900         PERFORM S01-LAES-W51372                                          
026000       END-PERFORM                                                        
026100     END-IF                                                               
026200     IF INDX > +0                                                         
026300       PERFORM CA-SORT-TABELL                                             
026400       PERFORM D-WEB-LISTA                                                
026500     END-IF                                                               
026600                                                                          
026700     PERFORM S05-SEND-CLOSE                                               
026800     PERFORM Z-FINIT                                                      
026900     MOVE ZERO TO RETURN-CODE                                             
027000     GOBACK                                                               
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400 A-INIT SECTION.                                                          
027500     OPEN INPUT W51370                                                    
027600     OPEN INPUT W51372                                                    
027700                                                                          
027800     ACCEPT WS-YYMMDD      FROM DATE                                      
027900     ACCEPT WS-HHMMSSTH    FROM TIME                                      
028000     MOVE WS-HHMM          TO WS-TIME                                     
028100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028200                                                                          
028300     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
028400     MOVE D-AAR          TO DAGENS-DATUM-AAR                              
028500     MOVE D-MAANAD       TO DAGENS-DATUM-MAANAD                           
028600     MOVE D-DAG          TO DAGENS-DATUM-DAG                              
028700     MOVE DAGENS-DATUM   TO DAT-I-TIDATUM                                 
028800                                                                          
028900     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
029000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
029100                         DAT-O-TIDATUM DAT-KDSVAR                         
029200                                                                          
029300     IF DAT-KDSVAR-OK                                                     
029400       MOVE DAT-TIAARP-GRP    TO W-TIAARP                                 
029500     END-IF                                                               
029600     INITIALIZE SORT-TABELL                                               
029700     .                                                                    
029800     EJECT                                                                
029900                                                                          
030000 B-WEB-LISTA  SECTION.                                                    
030100     IF IN-IDDC NOT = WS-IDDC-WEB                                         
030200       MOVE IN-IDDC TO WS-IDDC-WEB                                        
030300       IF SEND-IDCOM > ZERO                                               
030400         PERFORM S05-SEND-CLOSE                                           
030500       END-IF                                                             
030600       PERFORM BA-SKAPA-HEADER                                            
030700       PERFORM BC-SKAPA-LINE                                              
030800     ELSE                                                                 
030900       PERFORM BC-SKAPA-LINE                                              
031000     END-IF                                                               
031100     .                                                                    
031200     SKIP3                                                                
031300                                                                          
031400 BA-SKAPA-HEADER SECTION.                                                 
031500     MOVE 1                          TO HDR-REQU-IDMSGVER                 
031600     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
031700     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
031800                                                                          
031900     MOVE SPACE                      TO HDR-IDOUTREC                      
032000     MOVE 'ACCUMULATED-ADJ'          TO HDR-IDOUTTYPE                     
032100     MOVE IN-IDDC                    TO HDR-IDOUTREC(1:2)                 
032200     MOVE 'W51370'                   TO HDR-IDOUTREC(3:8)                 
032300     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
032400                                                                          
032500     PERFORM S05-SEND-OPEN                                                
032600     MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                   
032700*HDR                                                                      
032800     PERFORM S05-PUT-HEADER                                               
032900     .                                                                    
033000     SKIP3                                                                
033100                                                                          
033200 BC-SKAPA-LINE SECTION.                                                   
033300     MOVE '1'                        TO LINE-IDAFPRCD                     
033400     MOVE IN-IDDC                    TO LINE-IDDC                         
033500     MOVE IN-TIRP                    TO LINE-TIRP                         
033600     MOVE IN-KDJUSTYP                TO LINE-KDJUSTYP                     
033700     MOVE IN-KVJUSTKV-TOT            TO LINE-KVJUSTKV-TOT                 
033800     MOVE IN-KVJUSTKV-NEG            TO LINE-KVJUSTKV-NEG                 
033900     MOVE IN-KVJUSTKV-POS            TO LINE-KVJUSTKV-POS                 
034000     MOVE IN-KVJUSTKV-ZERO           TO LINE-KVJUSTKV-ZERO                
034100     PERFORM S05-PUT-REPORT-LINE                                          
034200     .                                                                    
034300     EJECT                                                                
034400                                                                          
034500 C-FYLL-TABELL SECTION.                                                   
034600     ADD +1 TO INDX                                                       
034700     MOVE DCS-ADGMT-PADR(11:20) TO TAB-ADCITY  (INDX)                     
034800     MOVE DCS-IDLANDX2          TO TAB-IDLANDX2(INDX)                     
034900                                                                          
035000     MOVE IN2-IDDC    TO WBDC-IDDC                                        
035100     CALL WL10WBDC USING WBDC-AREA                                        
035200     MOVE WBDC-KDMFUP TO TAB-KDMFUP(INDX)                                 
035300                                                                          
035400     MOVE IN2-IDAFPRCD          TO TAB-IDAFPRCD     (INDX)                
035500     MOVE IN2-IDDC              TO TAB-IDDC         (INDX)                
035600     MOVE W-TIAARP              TO TAB-TIAARP       (INDX)                
035700     MOVE IN2-KDJUSTYP          TO TAB-KDJUSTYP     (INDX)                
035800     MOVE IN2-KVJUSTKV-TOT      TO TAB-KVJUSTKV-TOT (INDX)                
035900     MOVE IN2-SUARTSTD-TOT      TO TAB-SUARTSTD-TOT (INDX)                
036000     MOVE IN2-KVJUSTKV-NEG      TO TAB-KVJUSTKV-NEG (INDX)                
036100     MOVE IN2-SUARTSTD-NEG      TO TAB-SUARTSTD-NEG (INDX)                
036200     MOVE IN2-KVJUSTKV-POS      TO TAB-KVJUSTKV-POS (INDX)                
036300     MOVE IN2-SUARTSTD-POS      TO TAB-SUARTSTD-POS (INDX)                
036400     MOVE IN2-KVJUSTKV-ZERO     TO TAB-KVJUSTKV-ZERO(INDX)                
036500     .                                                                    
036600     EJECT                                                                
036700                                                                          
036800 CA-SORT-TABELL SECTION.                                                  
036900     MOVE +99   TO STEGLAANGD                                             
037000     MOVE INDX  TO ANTAL                                                  
037100     MOVE +5    TO NYCKELLAANGD                                           
037200                                                                          
037300     CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                      
037400          TAB-SORT-BEGREPP1(1) NYCKELLAANGD                               
037500     .                                                                    
037600     EJECT                                                                
037700                                                                          
037800 D-WEB-LISTA  SECTION.                                                    
037900     MOVE INDX TO ANTAL-POSTER                                            
038000     MOVE +1 TO INDX                                                      
038100     MOVE TAB-IDLANDX2 (INDX) TO W-IDLANDX2                               
038200     MOVE TAB-IDDC     (INDX) TO W-IDDC                                   
038300                                 W-IDDC-TOT                               
038400                                 W-IDDC-B6                                
038500     MOVE TAB-KDMFUP (INDX) TO WDCS-KDMFUP                                
038600                                                                          
038700     PERFORM DA-SKAPA-HEADER                                              
038800                                                                          
038900     PERFORM UNTIL INDX > ANTAL-POSTER                                    
039000       IF TAB-KDMFUP (INDX) NOT = WDCS-KDMFUP                             
039100         PERFORM DD-SKAPA-TOT-POST                                        
039200         PERFORM S05-SEND-CLOSE                                           
039300         MOVE TAB-KDMFUP (INDX)   TO WDCS-KDMFUP                          
039400         PERFORM DA-SKAPA-HEADER                                          
039500         PERFORM DC-SKAPA-LINE                                            
039600         MOVE TAB-IDLANDX2 (INDX) TO W-IDLANDX2                           
039700         MOVE TAB-IDDC     (INDX) TO W-IDDC                               
039800                                     W-IDDC-TOT                           
039900       ELSE                                                               
040000         IF TAB-IDDC(INDX) NOT = W-IDDC-TOT                               
040100           PERFORM DD-SKAPA-TOT-POST                                      
040200           MOVE TAB-IDDC     (INDX) TO W-IDDC-TOT                         
040300         END-IF                                                           
040400         IF W-IDLANDX2 NOT = TAB-IDLANDX2 (INDX)                          
040500           MOVE TAB-IDLANDX2 (INDX) TO W-IDLANDX2                         
040600         END-IF                                                           
040700         PERFORM DC-SKAPA-LINE                                            
040800**       ADD +1 TO INDX                                                   
040900       END-IF                                                             
041000       ADD +1 TO INDX                                                     
041100     END-PERFORM                                                          
041200                                                                          
041300     PERFORM DD-SKAPA-TOT-POST                                            
041400     .                                                                    
041500     SKIP3                                                                
041600                                                                          
041700 DA-SKAPA-HEADER SECTION.                                                 
041800     MOVE 1                          TO HDR2-REQU-IDMSGVER                
041900     MOVE 'E'                        TO HDR2-REQU-KDPGMACT                
042000     MOVE IDPGM                      TO HDR2-REQU-IDUSER                  
042100                                                                          
042200     MOVE SPACE                      TO HDR2-HDR-IDOUTREC                 
042300     MOVE 'MAN-ACC-ADJ'              TO HDR2-HDR-IDOUTTYPE                
042400     MOVE WDCS-KDMFUP                TO HDR2-HDR-IDOUTREC(1:2)            
042500     MOVE WS-YYMMDDHHMM              TO HDR2-HDR-IDLIST                   
042600                                                                          
042700     PERFORM S05-SEND-OPEN                                                
042800     MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                   
042900*HDR                                                                      
043000     PERFORM S05-PUT-HEADER2                                              
043100     .                                                                    
043200     SKIP3                                                                
043300                                                                          
043400 DC-SKAPA-LINE SECTION.                                                   
043500     MOVE '1'                         TO LINE2-IDAFPRCD                   
043600     MOVE TAB-IDDC         (INDX)     TO LINE2-IDDC                       
043700     MOVE TAB-TIAARP       (INDX)     TO LINE2-TIAARP                     
043800     MOVE TAB-KDJUSTYP     (INDX)     TO LINE2-KDJUSTYP                   
043900     MOVE TAB-KVJUSTKV-TOT (INDX)     TO LINE2-KVJUSTKV-TOT               
044000     MOVE TAB-KVJUSTKV-NEG (INDX)     TO LINE2-KVJUSTKV-NEG               
044100     MOVE TAB-KVJUSTKV-POS (INDX)     TO LINE2-KVJUSTKV-POS               
044200     MOVE TAB-SUARTSTD-TOT (INDX)     TO LINE2-SUARTSTD-TOT               
044300     MOVE TAB-SUARTSTD-NEG (INDX)     TO LINE2-SUARTSTD-NEG               
044400     MOVE TAB-SUARTSTD-POS (INDX)     TO LINE2-SUARTSTD-POS               
044500     MOVE TAB-KVJUSTKV-ZERO(INDX)     TO LINE2-KVJUSTKV-ZERO              
044600     MOVE TAB-IDLANDX2     (INDX)     TO LINE2-IDLANDX2                   
044700     MOVE TAB-ADCITY       (INDX)     TO LINE2-ADCITY                     
044800     IF INDX > +1                                                         
044900       IF W-IDDC     = TAB-IDDC    (INDX)                                 
045000         MOVE SPACE                     TO LINE2-IDLANDX2                 
045100                                           LINE2-ADCITY                   
045200                                           LINE2-IDDC                     
045300       ELSE                                                               
045400         MOVE TAB-IDDC    (INDX)        TO W-IDDC                         
045500       END-IF                                                             
045600     END-IF                                                               
045700                                                                          
045800     PERFORM S05-PUT-REPORT-LINE2                                         
045900                                                                          
046000     PERFORM DCA-SUMMERA-POSTER                                           
046100     .                                                                    
046200     EJECT                                                                
046300                                                                          
046400 DCA-SUMMERA-POSTER SECTION.                                              
046500     COMPUTE SUM-KVJUSTKV-TOT  = SUM-KVJUSTKV-TOT  +                      
046600        TAB-KVJUSTKV-TOT (INDX)                                           
046700     COMPUTE SUM-KVJUSTKV-NEG  = SUM-KVJUSTKV-NEG  +                      
046800        TAB-KVJUSTKV-NEG (INDX)                                           
046900     COMPUTE SUM-KVJUSTKV-POS  = SUM-KVJUSTKV-POS  +                      
047000        TAB-KVJUSTKV-POS  (INDX)                                          
047100     COMPUTE SUM-KVJUSTKV-ZERO = SUM-KVJUSTKV-ZERO +                      
047200        TAB-KVJUSTKV-ZERO  (INDX)                                         
047300     COMPUTE SUM-SUARTSTD-NEG  = SUM-SUARTSTD-NEG  +                      
047400        TAB-SUARTSTD-NEG (INDX)                                           
047500     COMPUTE SUM-SUARTSTD-POS  = SUM-SUARTSTD-POS  +                      
047600        TAB-SUARTSTD-POS  (INDX)                                          
047700     .                                                                    
047800     EJECT                                                                
047900                                                                          
048000 DD-SKAPA-TOT-POST SECTION.                                               
048100     MOVE 'TOT'                       TO LINE2-IDAFPRCD                   
048200     MOVE SPACE                       TO LINE2-IDDC                       
048300     MOVE W-TIAARP                    TO LINE2-TIAARP                     
048400     MOVE ZERO                        TO LINE2-KDJUSTYP                   
048500     MOVE SUM-KVJUSTKV-TOT            TO LINE2-KVJUSTKV-TOT               
048600     MOVE SUM-KVJUSTKV-NEG            TO LINE2-KVJUSTKV-NEG               
048700     MOVE SUM-KVJUSTKV-POS            TO LINE2-KVJUSTKV-POS               
048800     MOVE ZERO                        TO LINE2-SUARTSTD-TOT               
048900     MOVE SUM-SUARTSTD-NEG            TO LINE2-SUARTSTD-NEG               
049000     MOVE SUM-SUARTSTD-POS            TO LINE2-SUARTSTD-POS               
049100     MOVE SUM-KVJUSTKV-ZERO           TO LINE2-KVJUSTKV-ZERO              
049200     MOVE SPACE                       TO LINE2-IDLANDX2                   
049300     MOVE SPACE                       TO LINE2-ADCITY                     
049400                                                                          
049500     PERFORM S05-PUT-REPORT-LINE2                                         
049600                                                                          
049700     MOVE ALL '+' TO DOC-LINE-AREA2                                       
049800     MOVE '1'                         TO LINE2-IDAFPRCD                   
049900     PERFORM S05-PUT-REPORT-LINE2                                         
050000                                                                          
050100     MOVE ZERO TO SUM-KVJUSTKV-TOT                                        
050200                  SUM-KVJUSTKV-NEG                                        
050300                  SUM-KVJUSTKV-POS                                        
050400                  SUM-SUARTSTD-NEG                                        
050500                  SUM-SUARTSTD-POS                                        
050600                  SUM-KVJUSTKV-ZERO                                       
050700     .                                                                    
050800     EJECT                                                                
050900                                                                          
051000 Z-FINIT SECTION.                                                         
051100     CLOSE W51370                                                         
051200     CLOSE W51372                                                         
051300                                                                          
051400     MOVE 'S' TO POSTSUM-OPKOD                                            
051500     CALL POSTSUM USING POSTSUM-PARM                                      
051600     .                                                                    
051700     EJECT                                                                
051800                                                                          
051900 S01-LAES-W51370  SECTION.                                                
052000     READ W51370 INTO IN-AREA                                             
052100     AT END                                                               
052200        SET END-OF-W51370 TO TRUE                                         
052300                                                                          
052400     NOT AT END                                                           
052500        MOVE 'W51370' TO POSTSUM-FDNAMN                                   
052600        MOVE 'W51370D1' TO POSTSUM-DDNAMN2                                
052700        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
052800        CALL POSTSUM USING POSTSUM-PARM                                   
052900     END-READ                                                             
053000     .                                                                    
053100     EJECT                                                                
053200                                                                          
053300 S01-LAES-W51372  SECTION.                                                
053400     READ W51372 INTO IN2-AREA                                            
053500     AT END                                                               
053600        SET END-OF-W51372 TO TRUE                                         
053700                                                                          
053800     NOT AT END                                                           
053900        MOVE 'W51372' TO POSTSUM-FDNAMN                                   
054000        MOVE 'W51370D2' TO POSTSUM-DDNAMN2                                
054100        MOVE 'IN2-'     TO POSTSUM-TRANSTYP                               
054200        CALL POSTSUM USING POSTSUM-PARM                                   
054300     END-READ                                                             
054400     .                                                                    
054500     EJECT                                                                
054600                                                                          
054700 S05-SEND-OPEN SECTION.                                                   
054800     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
054900     MOVE 'OPEN'                          TO SEND-KDFUNC                  
055000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
055100                         SEND-OPEN-AREA                                   
055200     IF SEND-KDRC > ZERO                                                  
055300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
055400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
055500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
055600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
055700     END-IF                                                               
055800     .                                                                    
055900     SKIP3                                                                
056000                                                                          
056100 S05-PUT-HEADER SECTION.                                                  
056200     MOVE 'PUT'                           TO SEND-KDFUNC                  
056300     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
056400     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
056500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
056600                         SEND-KVDLEN                                      
056700                         HDR-AREA                                         
056800     IF SEND-KDRC > ZERO                                                  
056900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
057000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
057100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
057200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
057300     END-IF                                                               
057400     .                                                                    
057500     EJECT                                                                
057600                                                                          
057700 S05-PUT-HEADER2 SECTION.                                                 
057800     MOVE 'PUT'                           TO SEND-KDFUNC                  
057900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
058000     MOVE LENGTH OF HDR-AREA2             TO SEND-KVDLEN                  
058100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
058200                         SEND-KVDLEN                                      
058300                         HDR-AREA2                                        
058400     IF SEND-KDRC > ZERO                                                  
058500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
058600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
058700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
058800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
058900     END-IF                                                               
059000     .                                                                    
059100     EJECT                                                                
059200                                                                          
059300 S05-PUT-REPORT-LINE    SECTION.                                          
059400     MOVE 'PUT'                           TO SEND-KDFUNC                  
059500     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
059600     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
059700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
059800                         SEND-KVDLEN                                      
059900                         DOC-LINE-AREA                                    
060000     IF SEND-KDRC > ZERO                                                  
060100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
060200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
060300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
060400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
060500     END-IF                                                               
060600     .                                                                    
060700     SKIP3                                                                
060800                                                                          
060900 S05-PUT-REPORT-LINE2   SECTION.                                          
061000     MOVE 'PUT'                           TO SEND-KDFUNC                  
061100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
061200     MOVE LENGTH OF DOC-LINE-AREA2        TO SEND-KVDLEN                  
061300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
061400                         SEND-KVDLEN                                      
061500                         DOC-LINE-AREA2                                   
061600     IF SEND-KDRC > ZERO                                                  
061700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
061800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
061900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
062000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
062100     END-IF                                                               
062200                                                                          
062300     .                                                                    
062400     SKIP3                                                                
062500                                                                          
062600 S05-SEND-CLOSE SECTION.                                                  
062700     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
062800     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
062900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
063000                                                                          
063100     IF SEND-KDRC > ZERO                                                  
063200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
063300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
063400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
063500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
063600     END-IF                                                               
063700     .                                                                    
063800                                                                          
063900 IMS-GU-WDB601    SECTION.                                                
064000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
064100          DELIMITED BY SIZE INTO SSA1                                     
064200     MOVE '  GE' TO GODK-STATUSKODER                                      
064300     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
064400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
064500     PERFORM IMS-STATUSKONTROLL                                           
064600     .                                                                    
064700     EJECT                                                                
064800                                                                          
064900 IMS-STATUSKONTROLL SECTION.                                              
065000     SET STATUS-IX TO 1                                                   
065100     SEARCH GODK-STATUS AT END CALL FELLOG                                
065200        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
065300        CONTINUE                                                          
065400     END-SEARCH                                                           
065500     .                                                                    
