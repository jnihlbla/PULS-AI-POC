000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5134700.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   05/06/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER IN FIL W51347 OCH SKAPAR DAP POSTER                        
001000*                                                                         
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- FIL MED NEGATIVA LAGERSALDO POSTER                         
002100     SELECT W51347                     ASSIGN TO W51347D1.                
002200     SKIP2                                                                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W51347                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W513471      -L.                                               
003300     SKIP3                                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W5134700'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  YES                         PIC X       VALUE 'Y'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  WS-ADRESS                   PIC X(50)                                
004200                   VALUE 'CARPARTS.DAP.DISTRDOC'.                         
004300 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004600 77  KDRC-DISPLAY                PIC Z(5).                                
004700 77  WS-FLOPEN                   PIC X     VALUE 'N'.                     
004800 77  INDX                        PIC S9(9) COMP-3 VALUE ZERO.             
004900 77  ANTAL-POSTER                PIC S9(9) COMP-3.                        
005000                                                                          
005100 77  W-KDMFUP                    PIC X(2).                                
005200 77  W-ADCITY                    PIC X(20).                               
005300 77  W-IDLANDX2                  PIC X(2).                                
005400                                                                          
005500 77  WS-IDSKYLT-CHINESE          PIC X(3)  VALUE 'RCN'.                   
005600 77  WS-IDSKYLT-ENGLISH          PIC X(3)  VALUE 'GB '.                   
005700                                                                          
005800                                                                          
005900 01  WS-YYMMDDHHMM.                                                       
006000     03 WS-YYMMDD                PIC  9(6).                               
006100     03 WS-TIME                  PIC  9(4).                               
006200                                                                          
006300 01  WS-HHMMSSTH                 PIC  9(8).                               
006400 01  FILLER REDEFINES WS-HHMMSSTH.                                        
006500       03  WS-HHMM               PIC 9(4).                                
006600       03  WS-SSTH               PIC 9(4).                                
006700 01  WS-IDDC-WEB                 PIC X(2) VALUE SPACE.                    
006800                                                                          
006900     SKIP2                                                                
007000 01  FELTEXT.                                                             
007100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007300                                                                          
007400 77  W51347-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W51347                       VALUE 'J'.                   
007600     EJECT                                                                
007700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007800 01  FILLER REDEFINES DAGENS-DATUM.                                       
007900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008200     EJECT                                                                
008300 01  DYNAMISKA-SUBPROGRAM.                                                
008400*                                                                         
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009000     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
009100     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
009200     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
009300     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
009400     EJECT                                                                
009500*    --- PARAMETERS TO ABEND                                              
009600                                                                          
009700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010000                                                                          
010100*    --- PARAMETRAR TILL POSTSUM                                          
010200*                                                                         
010300*01  -COPY W0005   -PRE  POSTSUM-                                         
010400                                                                          
010500 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
010600*01  -COPY WTRAUTF8                                                       
010700                                                                          
010800     EJECT                                                                
010900*                                                                         
011000 01  FILLER                      PIC X(16) VALUE 'WL10WBDC-AREA'.         
011100*01  -COPY WL10WBDC                                                       
011200                                                                          
011300     EJECT                                                                
011400 01  TABENTRY-PARM.                                                       
011500     03  STEGLAANGD              PIC S9(9) COMP.                          
011600     03  ANTAL                   PIC S9(9) COMP.                          
011700     03  NYCKELLAANGD            PIC S9(9) COMP.                          
011800 01  FILLER                      PIC X(16) VALUE 'SORT-TABELL'.           
011900 01  SORT-TABELL.                                                         
012000     03  TAB-RAD OCCURS 1000.                                             
012100        05  TAB-SORT-BEGREPP1.                                            
012200            07 TAB-KDMFUP        PIC X(2).                                
012300            07 TAB-IDLANDX2      PIC X(2).                                
012400            07 TAB-IDDC          PIC X(2).                                
012500        05  TAB-IDARTNR          PIC 9(8).                                
012600        05  TAB-BEART            PIC X(25).                               
012700        05  TAB-ADCITY           PIC X(20).                               
012800        05  TAB-KVAKS            PIC S9(9) COMP-3.                        
012900        05  TAB-KVEFRS           PIC S9(9) COMP-3.                        
013000        05  TAB-KVLS             PIC S9(9) COMP-3.                        
013100        05  TAB-DAAAVV           PIC 9(4).                                
013200                                                                          
013300     EJECT                                                                
013400 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
013500*01  -COPY WZ01SEND                                                       
013600     EJECT                                                                
013700*--------------------------------------- NYCKLAR TILL BASERNA             
013800                                                                          
013900 01  W-IDDC-B6-X.                                                         
014000     03 W-IDDC-B6        PIC X(2).                                        
014100                                                                          
014200 01  W-IDDC-X.                                                            
014300     03 W-IDDC           PIC X(2).                                        
014400                                                                          
014500 01  W-IDARTNR-X.                                                         
014600     03  W-IDARTNR       PIC S9(9)   COMP-3.                              
014700 01  W-IDSKYLT-X.                                                         
014800     03  W-IDSKYLT       PIC X(3)    VALUE SPACE.                         
014900                                                                          
015000     EJECT                                                                
015100 01  IN-AREA-START               PIC X(24)   VALUE                        
015200                                             'IN-AREA-START'.             
015300     SKIP2                                                                
015400                                                                          
015500*01  AREA -COPY W513471     -PRE IN-                                      
015600     EJECT                                                                
015700 01  UT-AREA-START               PIC X(24)   VALUE                        
015800                                             'UT-AREA-START'.             
015900     SKIP2                                                                
016000 01  HDR-AREA.                                                            
016100*   03  -COPY WZ01REQU -PRE HDR-                                          
016200*   03  -COPY WZ04HDR                                                     
016300*                                                                         
016400 01  LINE-AREA                   PIC X(24)    VALUE 'LINE-AREA'.          
016500 01  DOC-LINE-AREA.                                                       
016600*    03 -COPY W513472 -PRE LINE-                                          
016700     EJECT                                                                
016800 01  LINE-AREA2                  PIC X(24)    VALUE 'LINE-AREA2'.         
016900 01  DOC-LINE-AREA2.                                                      
017000*    03 -COPY W513473 -PRE LINE2-                                         
017100     EJECT                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300     SKIP3                                                                
017400*    --- STATUS-KOD FRÅN IMS                                              
017500 01  STATUS-WS                   PIC XX.                                  
017600     88  SEGMENT-FINNS                       VALUE '  '.                  
017700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018000     88  IMS-EJ-OK                           VALUE 'XD'.                  
018100     SKIP2                                                                
018200 01  GODK-STATUSKODER.                                                    
018300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018400     SKIP3                                                                
018500 01  SSA1                        PIC X(64).                               
018600 01  SSA2                        PIC X(64).                               
018700     EJECT                                                                
018800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
018900 01   DLI-IO-AREA-B601.                                                   
019000*     03  -COPY WDB601                                                    
019100                                                                          
019200 01  FILLER               PIC X(16)   VALUE 'WDD311 AREA'.                
019300 01  DLI-IO-WDD311.                                                       
019400*    03  -COPY WDD311                                                     
019500                                                                          
019600                                                                          
019700*    --- IMS FUNKTIONSKODER                                               
019800*01  -COPY W0003                                                          
019900     EJECT                                                                
020000                                                                          
020100 LINKAGE SECTION.                                                         
020200*01  -COPY W0009   -PRE MSG-                                              
020300     EJECT                                                                
020400 01  DAP-PCB              PIC X.                                          
020500     EJECT                                                                
020600*01  -COPY W0008 -PRE WDB6-                                               
020700     05  FILLER           PIC X.                                          
020800     EJECT                                                                
020900*01  -COPY W0008 -PRE WDD3-                                               
021000     05  FILLER           PIC X.                                          
021100                                                                          
021200                                                                          
021300 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB WDD3-PCB.             
021400 MAIN SECTION.                                                            
021500     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB WDD3-PCB.             
021600                                                                          
021700     SKIP2                                                                
021800     PERFORM A-INIT                                                       
021900     PERFORM S01-LAES-W51347                                              
022000     PERFORM UNTIL END-OF-W51347                                          
022100                                                                          
022200       IF IN-IDDC NOT = WS-IDDC-WEB                                       
022300*        -- FETCH INFO FRO NEW DC                                         
022400         MOVE IN-IDDC TO W-IDDC-B6                                        
022500                         WS-IDDC-WEB                                      
022600         PERFORM IMS-GU-WDB601                                            
022700         PERFORM F-COMPUTE-KDMFUP                                         
022800*        -- CLOSE POSSIBLE REPORT FOR PREVIOUS DC                         
022900         IF WS-FLOPEN = JA                                                
023000           PERFORM S05-SEND-CLOSE                                         
023100           MOVE NEJ TO WS-FLOPEN                                          
023200         END-IF                                                           
023300       END-IF                                                             
023400                                                                          
023500       IF DCS-FLWEBDC = JA OR YES                                         
023600*        MOVE JA   TO WS-FLWEBDC                                          
023700         IF WS-FLOPEN = NEJ                                               
023800*          -- OPEN REPORT FOR NEW DC AND SEND HEADER                      
023900           PERFORM BA-SKAPA-HEADER                                        
024000           MOVE JA TO WS-FLOPEN                                           
024100         END-IF                                                           
024200         PERFORM BC-SKAPA-LINE                                            
024300*        -- ALSO SAVE DATA FOR MANAGEMENT REPORT                          
024310         IF WBDC-KDMFUP = SPACE                                           
024320           CONTINUE                                                       
024330         ELSE                                                             
024400           PERFORM C-FYLL-TABELL                                          
024500         END-IF                                                           
024510       END-IF                                                             
024600                                                                          
024700       PERFORM S01-LAES-W51347                                            
024800     END-PERFORM                                                          
024900                                                                          
025000     IF WS-FLOPEN = 'J'                                                   
025100       PERFORM S05-SEND-CLOSE                                             
025200     END-IF                                                               
025300                                                                          
025400     IF INDX > +0                                                         
025500*      -- DATA FOR MANAGMENT REPORT EXISTS                                
025600       PERFORM D-SORT-TABELL                                              
025700       PERFORM E-SKAPA-MAN-REPORT                                         
025800     END-IF                                                               
025900                                                                          
026000     PERFORM Z-FINIT                                                      
026100     MOVE ZERO TO RETURN-CODE                                             
026200     GOBACK                                                               
026300     .                                                                    
026400     EJECT                                                                
026500 A-INIT SECTION.                                                          
026600     SKIP2                                                                
026700                                                                          
026800     OPEN INPUT W51347                                                    
026900                                                                          
027000     ACCEPT WS-YYMMDD      FROM DATE                                      
027100     ACCEPT WS-HHMMSSTH    FROM TIME                                      
027200     MOVE WS-HHMM          TO WS-TIME                                     
027300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027400                                                                          
027500     MOVE ZERO TO INDX                                                    
027600     .                                                                    
027700     EJECT                                                                
027800 B-WEB-LISTA  SECTION.                                                    
027900     IF IN-IDDC NOT = WS-IDDC-WEB                                         
028000       MOVE IN-IDDC TO WS-IDDC-WEB                                        
028100       PERFORM S05-SEND-CLOSE                                             
028200       PERFORM BA-SKAPA-HEADER                                            
028300       PERFORM BC-SKAPA-LINE                                              
028400     ELSE                                                                 
028500       PERFORM BC-SKAPA-LINE                                              
028600     END-IF                                                               
028700     .                                                                    
028800     SKIP3                                                                
028900 BA-SKAPA-HEADER SECTION.                                                 
029000     MOVE 1                          TO HDR-REQU-IDMSGVER                 
029100     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
029200     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
029300                                                                          
029400     MOVE SPACE                      TO HDR-IDOUTREC                      
029500     MOVE 'PARTS-NEG-BAL'            TO HDR-IDOUTTYPE                     
029600     MOVE IN-IDDC                    TO HDR-IDOUTREC(1:2)                 
029700     MOVE 'W51347'                   TO HDR-IDOUTREC(3:8)                 
029800     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
029900                                                                          
030000***  IF WZ04-SEND-IDCOM = ZERO                                            
030100       PERFORM S05-SEND-OPEN                                              
030200       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
030300***  END-IF                                                               
030400*HDR                                                                      
030500     PERFORM S05-PUT-HEADER                                               
030600     .                                                                    
030700     SKIP3                                                                
030800 BC-SKAPA-LINE SECTION.                                                   
030900     MOVE '1'                        TO LINE-IDAFPRCD                     
031000     MOVE IN-IDDC                    TO LINE-IDDC                         
031100     MOVE IN-IDARTNR                 TO LINE-IDARTNR                      
031200     MOVE IN-BEART                   TO LINE-BEART                        
031300                                                                          
031400*    WE NEED SOME ADJUSTMENT FOR CHINESE NAME                             
031500*    BEFORE DISPLAY OF LINE-BEART                                         
031600                                                                          
031700     MOVE IN-IDARTNR                 TO W-IDARTNR                         
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
032500     PERFORM IMS-GU-WDD311                                                
032600     IF SEGMENT-FINNS                                                     
032700        MOVE TEXT-BEART              TO TRAUTF8-TECONV-FROM               
032800     ELSE                                                                 
032900        MOVE SPACE                   TO TRAUTF8-TECONV-FROM               
033000        MOVE '278 '                  TO TRAUTF8-KDCP                      
033100     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
033200     MOVE 25                    TO TRAUTF8-KVMAXTL                        
033300     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
033400     MOVE TRAUTF8-TECONV-TO     TO LINE-BEART                             
033500                                                                          
033600     MOVE IN-KVAKS                   TO LINE-KVAKS                        
033700     MOVE IN-KVEFRS                  TO LINE-KVEFRS                       
033800     MOVE IN-KVLS                    TO LINE-KVLS                         
033900     MOVE IN-DAAAVV                  TO LINE-DAAAVV                       
034000     PERFORM S05-PUT-REPORT-LINE                                          
034100     .                                                                    
034200     EJECT                                                                
034300 C-FYLL-TABELL  SECTION.                                                  
034310                                                                          
034500     ADD +1 TO INDX                                                       
034600                                                                          
034700     MOVE WBDC-KDMFUP          TO TAB-KDMFUP            (INDX)            
034800     MOVE IN-IDDC              TO TAB-IDDC              (INDX)            
034900                                                                          
035000     MOVE IN-DAAAVV            TO TAB-DAAAVV            (INDX)            
035100     MOVE LINE-BEART           TO TAB-BEART             (INDX)            
035200                                                                          
035300     MOVE IN-KVAKS             TO TAB-KVAKS             (INDX)            
035400     MOVE IN-KVEFRS            TO TAB-KVEFRS            (INDX)            
035500     MOVE IN-KVLS              TO TAB-KVLS              (INDX)            
035600     MOVE IN-IDARTNR           TO TAB-IDARTNR           (INDX)            
035700     MOVE DCS-ADGMT-PADR(11:20) TO TAB-ADCITY           (INDX)            
035800     MOVE DCS-IDLANDX2         TO TAB-IDLANDX2          (INDX)            
035900     .                                                                    
036000     EJECT                                                                
036100 D-SORT-TABELL SECTION.                                                   
036200     MOVE +78   TO STEGLAANGD                                             
036300     MOVE INDX  TO ANTAL                                                  
036400     MOVE +6    TO NYCKELLAANGD                                           
036500                                                                          
036600     CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                      
036700     TAB-SORT-BEGREPP1(1) NYCKELLAANGD                                    
036800     .                                                                    
036900     EJECT                                                                
037000 E-SKAPA-MAN-REPORT   SECTION.                                            
037100     MOVE INDX  TO ANTAL-POSTER                                           
037200     MOVE +1    TO INDX                                                   
037300                                                                          
037400     MOVE TAB-KDMFUP   (INDX) TO W-KDMFUP                                 
037500     MOVE TAB-IDLANDX2 (INDX) TO W-IDLANDX2                               
037600     MOVE TAB-IDDC     (INDX) TO W-IDDC                                   
037700                                                                          
037800     PERFORM EA-SKAPA-HEADER                                              
037900                                                                          
038000     PERFORM UNTIL INDX > ANTAL-POSTER                                    
038100       IF TAB-IDLANDX2 (INDX) NOT = W-IDLANDX2                            
038200         MOVE SPACE                      TO DOC-LINE-AREA2                
038300*        WE NEED SOME ADJUSTMENT FOR WEB-REPORTS                          
038400*        SPACE IN BEART WILL BE ALL 'N' IF NOT                            
038500         MOVE ALL X'20'                  TO LINE2-BEART                   
038600                                                                          
038700*                                                                         
038800         MOVE '1'                        TO LINE2-IDAFPRCD                
038900         PERFORM S05-PUT-REPORT-LINE2                                     
039000         MOVE TAB-IDLANDX2 (INDX) TO W-IDLANDX2                           
039100       END-IF                                                             
039200                                                                          
039300       MOVE '1'                        TO LINE2-IDAFPRCD                  
039400       MOVE TAB-IDDC            (INDX) TO LINE2-IDDC                      
039500                                          W-IDDC-B6                       
039600       MOVE TAB-DAAAVV          (INDX) TO LINE2-DAAAVV                    
039700       MOVE TAB-BEART           (INDX) TO LINE2-BEART                     
039800                                                                          
039900       MOVE TAB-KVAKS           (INDX) TO LINE2-KVAKS                     
040000       MOVE TAB-KVEFRS          (INDX) TO LINE2-KVEFRS                    
040100       MOVE TAB-KVLS            (INDX) TO LINE2-KVLS                      
040200       MOVE TAB-IDARTNR         (INDX) TO LINE2-IDARTNR                   
040300       MOVE TAB-ADCITY          (INDX) TO LINE2-ADCITY                    
040400       MOVE TAB-IDLANDX2        (INDX) TO LINE2-IDLANDX2                  
040500*                                                                         
040600       IF TAB-KDMFUP (INDX) NOT = W-KDMFUP                                
040700          MOVE TAB-KDMFUP (INDX)   TO W-KDMFUP                            
040800          PERFORM S05-SEND-CLOSE                                          
040900          PERFORM EA-SKAPA-HEADER                                         
041000       END-IF                                                             
041100*                                                                         
041200       IF TAB-IDDC (INDX) = W-IDDC AND INDX > 1                           
041300         MOVE SPACE                    TO LINE2-ADCITY                    
041400         MOVE SPACE                    TO LINE2-IDDC                      
041500         MOVE SPACE                    TO LINE2-IDLANDX2                  
041600                                                                          
041700       ELSE                                                               
041800         MOVE TAB-IDDC (INDX)          TO W-IDDC                          
041900       END-IF                                                             
042000*                                                                         
042100       PERFORM S05-PUT-REPORT-LINE2                                       
042200       ADD +1 TO INDX                                                     
042300     END-PERFORM                                                          
042400     PERFORM S05-SEND-CLOSE                                               
042500                                                                          
042600     .                                                                    
042700     EJECT                                                                
042800 EA-SKAPA-HEADER SECTION.                                                 
042900     MOVE 1                          TO HDR-REQU-IDMSGVER                 
043000     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
043100     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
043200                                                                          
043300     MOVE SPACE                      TO HDR-IDOUTREC                      
043400     MOVE 'MAN-NEG-BAL'              TO HDR-IDOUTTYPE                     
043500     MOVE W-KDMFUP                   TO HDR-IDOUTREC(1:2)                 
043600     MOVE 'W51347'                   TO HDR-IDOUTREC(3:8)                 
043700     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
043800                                                                          
043900     PERFORM S05-SEND-OPEN                                                
044000     MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                   
044100*HDR                                                                      
044200     PERFORM S05-PUT-HEADER                                               
044300     .                                                                    
044400     EJECT                                                                
044500 F-COMPUTE-KDMFUP SECTION.                                                
044600                                                                          
044700*    -- COMPUTE WHICH MANANGEMENT-FOLLOW-UP GROUP THE DC                  
044800*    -- BELONGS TO.                                                       
044900     MOVE IN-IDDC     TO WBDC-IDDC                                        
045000     CALL WL10WBDC USING WBDC-AREA                                        
045100     .                                                                    
045200     EJECT                                                                
045300 Z-FINIT SECTION.                                                         
045400                                                                          
045500     CLOSE W51347                                                         
045600                                                                          
045700     MOVE 'S' TO POSTSUM-OPKOD                                            
045800     CALL POSTSUM USING POSTSUM-PARM                                      
045900     .                                                                    
046000     EJECT                                                                
046100 S01-LAES-W51347  SECTION.                                                
046200     SKIP2                                                                
046300     READ W51347 INTO IN-AREA                                             
046400     AT END                                                               
046500        SET END-OF-W51347 TO TRUE                                         
046600                                                                          
046700     NOT AT END                                                           
046800        MOVE 'W51347' TO POSTSUM-FDNAMN                                   
046900        MOVE 'W51347D1' TO POSTSUM-DDNAMN2                                
047000        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
047100        CALL POSTSUM USING POSTSUM-PARM                                   
047200     END-READ                                                             
047300     .                                                                    
047400     EJECT                                                                
047500 S05-SEND-OPEN SECTION.                                                   
047600     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
047700     MOVE 'OPEN'                          TO SEND-KDFUNC                  
047800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
047900                         SEND-OPEN-AREA                                   
048000     IF SEND-KDRC > ZERO                                                  
048100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
048200       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
048300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
048400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
048500     END-IF                                                               
048600                                                                          
048700     .                                                                    
048800     SKIP3                                                                
048900 S05-PUT-HEADER SECTION.                                                  
049000                                                                          
049100     MOVE 'PUT'                           TO SEND-KDFUNC                  
049200     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
049300     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
049400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
049500                         SEND-KVDLEN                                      
049600                         HDR-AREA                                         
049700                                                                          
049800     IF SEND-KDRC > ZERO                                                  
049900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
050000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
050100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
050200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600 S05-PUT-REPORT-LINE    SECTION.                                          
050700                                                                          
050800     MOVE 'PUT'                           TO SEND-KDFUNC                  
050900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
051000     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
051100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
051200                         SEND-KVDLEN                                      
051300                         DOC-LINE-AREA                                    
051400                                                                          
051500     IF SEND-KDRC > ZERO                                                  
051600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
051700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
051800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
051900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
052000     END-IF                                                               
052100     MOVE 'W51347' TO POSTSUM-FDNAMN                                      
052200     MOVE 'DAP1' TO POSTSUM-DDNAMN2                                       
052300     MOVE 'LINE1'     TO POSTSUM-TRANSTYP                                 
052400     CALL POSTSUM USING POSTSUM-PARM                                      
052500     .                                                                    
052600     SKIP3                                                                
052700 S05-PUT-REPORT-LINE2   SECTION.                                          
052800                                                                          
052900     MOVE 'PUT'                           TO SEND-KDFUNC                  
053000     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
053100     MOVE LENGTH OF DOC-LINE-AREA2        TO SEND-KVDLEN                  
053200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
053300                         SEND-KVDLEN                                      
053400                         DOC-LINE-AREA2                                   
053500     IF SEND-KDRC > ZERO                                                  
053600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
053700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
053800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
053900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
054000     END-IF                                                               
054100        MOVE 'W51347' TO POSTSUM-FDNAMN                                   
054200        MOVE 'DAP2' TO POSTSUM-DDNAMN2                                    
054300        MOVE 'LINE2'     TO POSTSUM-TRANSTYP                              
054400        CALL POSTSUM USING POSTSUM-PARM                                   
054500*                                                                         
054600     .                                                                    
054700     SKIP3                                                                
054800 S05-SEND-CLOSE SECTION.                                                  
054900                                                                          
055000     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
055100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
055200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
055300                                                                          
055400                                                                          
055500     IF SEND-KDRC > ZERO                                                  
055600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
055700       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
055800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
055900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
056000     END-IF                                                               
056100     .                                                                    
056200 IMS-GU-WDB601    SECTION.                                                
056300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
056400          DELIMITED BY SIZE INTO SSA1                                     
056500     MOVE '  GE' TO GODK-STATUSKODER                                      
056600     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
056700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
056800     PERFORM IMS-STATUSKONTROLL                                           
056900     .                                                                    
057000     EJECT                                                                
057100 IMS-GU-WDD311 SECTION.                                                   
057200                                                                          
057300     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
057400             DELIMITED BY SIZE INTO SSA1                                  
057500     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
057600             DELIMITED BY SIZE INTO SSA2                                  
057700     MOVE '  GE'                 TO GODK-STATUSKODER                      
057800     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
057900     MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
058000     PERFORM IMS-STATUSKONTROLL                                           
058100     .                                                                    
058200     EJECT                                                                
058300 IMS-STATUSKONTROLL SECTION.                                              
058400                                                                          
058500     SET STATUS-IX TO 1                                                   
058600     SEARCH GODK-STATUS AT END CALL FELLOG                                
058700        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
058800        CONTINUE                                                          
058900     END-SEARCH                                                           
059000     .                                                                    
