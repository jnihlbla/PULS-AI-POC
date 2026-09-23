000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5139400.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   05/06/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER IN FIL W51394 OCH SKAPAR DAP POSTER                        
001000*        SELECTIONS REGISTER POSTER                                       
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- FIL MED SELECTIONS REGISTER POSTER                         
002100     SELECT W51394                     ASSIGN TO W51394D1.                
002200                                                                          
002300*          --- FIL MED SELECTIONS REGISTER POSTER OCH ANTAL               
002400*          --- INVENTERADE POSTER PER DC                                  
002500     SELECT W51396                     ASSIGN TO W51394D2.                
002600     SKIP2                                                                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W51394                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  -COPY W513951      -L.                                               
003700     SKIP3                                                                
003800 FD  W51396                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W51396       -L.                                               
004300     SKIP3                                                                
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W5139400'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  YES                         PIC X       VALUE 'Y'.                   
005100 77  WS-ADRESS                   PIC X(50)                                
005200                   VALUE 'CARPARTS.DAP.DISTRDOC'.                         
005300 77  INDX                        PIC S9(9)   VALUE ZERO.                  
005400 77  ANTAL-POSTER                PIC S9(9)   VALUE ZERO.                  
005500 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
005600 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005700 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005800 77  KDRC-DISPLAY                PIC Z(5).                                
005900 77  WS-FLWEBDC                  PIC X     VALUE 'N'.                     
006000 77  WS-FLOPEN                   PIC X     VALUE 'N'.                     
006100                                                                          
006200 01  WS-YYMMDDHHMM.                                                       
006300     03 WS-YYMMDD                PIC  9(6).                               
006400     03 WS-TIME                  PIC  9(4).                               
006500                                                                          
006600 01  WS-HHMMSSTH.                                                         
006700     03 WS-HHMM                  PIC  9(4).                               
006800     03 WS-SSTH                  PIC  9(4).                               
006900                                                                          
007000 01  WS-KDMFUP                   PIC X(2)   VALUE SPACE.                  
007100 01  WS-IDDC-WEB                 PIC X(2)   VALUE SPACE.                  
007200 01  WS-IDLANDX2                 PIC X(2)   VALUE SPACE.                  
007300*---------------------------------------                                  
007400                                                                          
007500 01  W-IDDC-B6-X.                                                         
007600     03 W-IDDC-B6        PIC X(2).                                        
007700                                                                          
007800     EJECT                                                                
007900*---------------------------------------                                  
008000                                                                          
008100     SKIP2                                                                
008200 01  FELTEXT.                                                             
008300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008500                                                                          
008600 77  W51394-EOF-SW               PIC X       VALUE 'N'.                   
008700     88  END-OF-W51394                       VALUE 'J'.                   
008800                                                                          
008900 77  W51396-EOF-SW               PIC X       VALUE 'N'.                   
009000     88  END-OF-W51396                       VALUE 'J'.                   
009100     EJECT                                                                
009200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009300 01  FILLER REDEFINES DAGENS-DATUM.                                       
009400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16) VALUE 'TAB VARDE'.             
009900 01  TABENTRY-PARM.                                                       
010000     03  STEGLAANGD              PIC S9(9) COMP.                          
010100     03  ANTAL                   PIC S9(9) COMP.                          
010200     03  NYCKELLAANGD            PIC S9(9) COMP.                          
010300 01  FILLER                      PIC X(16) VALUE 'SORT-TABELL'.           
010400 01  SORT-TABELL.                                                         
010500     03  TAB-RAD OCCURS 500.                                              
010600        05  TAB-SORT-BEGREPP1.                                            
010700            07 TAB-KDMFUP        PIC X(2).                                
010800            07 TAB-IDLANDX2      PIC X(2).                                
010900            07 TAB-IDDC          PIC X(2).                                
011000        05  TAB-ADCITY           PIC X(20).                               
011100        05  TAB-SUART-REM        PIC 9(5).                                
011200        05  TAB-SUART-TOT        PIC 9(5).                                
011300        05  TAB-SUART-INV        PIC 9(5).                                
011400        05  TAB-TIAAVV           PIC 9(4).                                
011500                                                                          
011600 01  DYNAMISKA-SUBPROGRAM.                                                
011700*                                                                         
011800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
012200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012300     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
012400     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
012500     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
012600     EJECT                                                                
012700*    --- PARAMETERS TO ABEND                                              
012800                                                                          
012900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013200                                                                          
013300*    --- PARAMETRAR TILL POSTSUM                                          
013400*                                                                         
013500*01  -COPY W0005   -PRE  POSTSUM-                                         
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL WL10WBDC                                         
013800*01  -COPY WL10WBDC                                                       
013900     EJECT                                                                
014000                                                                          
014100 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
014200*01  -COPY WZ01SEND                                                       
014300     EJECT                                                                
014400 01  IN-AREA-START               PIC X(24)   VALUE                        
014500                                             'IN-AREA-START'.             
014600     SKIP2                                                                
014700                                                                          
014800*01  AREA -COPY W513951     -PRE IN-                                      
014900     EJECT                                                                
015000                                                                          
015100 01  IN-AREA2-START               PIC X(24)   VALUE                       
015200                                             'IN-AREA2-START'.            
015300     SKIP2                                                                
015400                                                                          
015500*01  AREA -COPY W51396     -PRE IN2-                                      
015600     EJECT                                                                
015700 01  UT-AREA-START               PIC X(24)   VALUE                        
015800                                             'UT-AREA-START'.             
015900     SKIP2                                                                
016000 01  HDR-AREA.                                                            
016100*   03  -COPY WZ01REQU -PRE HDR-                                          
016200*   03  -COPY WZ04HDR                                                     
016300*                                                                         
016400 01  HDR-AREA2.                                                           
016500*   03  -COPY WZ01REQU -PRE HDR2-                                         
016600*   03  -COPY WZ04HDR  -PRE HDR2-                                         
016700*                                                                         
016800                                                                          
016900                                                                          
017000 01  LINE-AREA                   PIC X(24)    VALUE 'LINE-AREA'.          
017100 01  DOC-LINE-AREA.                                                       
017200*    03 -COPY W513951 -PRE LINE-                                          
017300     EJECT                                                                
017400                                                                          
017500 01  LINE-AREA2                  PIC X(24)    VALUE 'LINE-AREA2'.         
017600 01  DOC-LINE-AREA2.                                                      
017700*    03 -COPY W513961 -PRE LINE2-                                         
017800     EJECT                                                                
017900                                                                          
018000                                                                          
018100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
018200 01   DLI-IO-AREA-B601.                                                   
018300*     03  -COPY WDB601                                                    
018400                                                                          
018500                                                                          
018600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018700     SKIP3                                                                
018800*    --- STATUS-KOD FRÅN IMS                                              
018900 01  STATUS-WS                   PIC XX.                                  
019000     88  SEGMENT-FINNS                       VALUE '  '.                  
019100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
019400     88  IMS-EJ-OK                           VALUE 'XD'.                  
019500     SKIP2                                                                
019600 01  GODK-STATUSKODER.                                                    
019700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019800     SKIP3                                                                
019900 01  SSA1                        PIC X(64).                               
020000 01  SSA2                        PIC X(64).                               
020100     EJECT                                                                
020200*    --- IMS FUNKTIONSKODER                                               
020300*01  -COPY W0003                                                          
020400     EJECT                                                                
020500                                                                          
020600 LINKAGE SECTION.                                                         
020700*01  -COPY W0009   -PRE MSG-                                              
020800     EJECT                                                                
020900 01  DAP-PCB              PIC X.                                          
021000     EJECT                                                                
021100*01  -COPY W0008 -PRE WDB6-                                               
021200     05  FILLER           PIC X.                                          
021300                                                                          
021400 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB.                      
021500 MAIN SECTION.                                                            
021600     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB.                      
021700                                                                          
021800     SKIP2                                                                
021900     PERFORM A-INIT                                                       
022000     PERFORM S01-LAES-W51394                                              
022100** LÄS FRAM TILL FÖRSTA LDC POSTEN                                        
022200       PERFORM UNTIL WS-FLWEBDC = 'J'  OR END-OF-W51394                   
022300         MOVE IN-IDDC TO W-IDDC-B6                                        
022400         PERFORM IMS-GU-WDB601                                            
022500         IF DCS-FLWEBDC = JA OR YES                                       
022600           PERFORM BA-SKAPA-HEADER                                        
022700           PERFORM BC-SKAPA-LINE                                          
022800           MOVE IN-IDDC TO WS-IDDC-WEB                                    
022900           MOVE 'J'     TO WS-FLWEBDC                                     
023000           MOVE 'J'     TO WS-FLOPEN                                      
023100         ELSE                                                             
023200           PERFORM S01-LAES-W51394                                        
023300*          MOVE IN-IDDC TO W-IDDC-B6                                      
023400*          PERFORM IMS-GU-WDB601                                          
023500         END-IF                                                           
023600       END-PERFORM                                                        
023700****                                                                      
023800     IF NOT END-OF-W51394                                                 
023900       PERFORM S01-LAES-W51394                                            
024000       IF NOT END-OF-W51394                                               
024100         MOVE IN-IDDC    TO W-IDDC-B6                                     
024200         PERFORM UNTIL END-OF-W51394                                      
024300           IF IN-IDDC NOT  = WS-IDDC-WEB                                  
024400             PERFORM IMS-GU-WDB601                                        
024500           END-IF                                                         
024600           IF DCS-FLWEBDC = JA OR YES                                     
024700             PERFORM B-WEB-LISTA                                          
024800           END-IF                                                         
024900           PERFORM S01-LAES-W51394                                        
025000           MOVE IN-IDDC TO W-IDDC-B6                                      
025100         END-PERFORM                                                      
025200         PERFORM S05-SEND-CLOSE                                           
025300         MOVE 'N' TO WS-FLOPEN                                            
025400       END-IF                                                             
025500     END-IF                                                               
025600     IF WS-FLOPEN = 'J'                                                   
025700       PERFORM S05-SEND-CLOSE                                             
025800     END-IF                                                               
025900                                                                          
026000* NEXT INFILE                                                             
026100                                                                          
026200     MOVE 'N' TO WS-FLOPEN                                                
026300     PERFORM S01-LAES-W51396                                              
026400     IF NOT END-OF-W51396                                                 
026500       MOVE 'J'     TO WS-FLOPEN                                          
026600     END-IF                                                               
026700                                                                          
026800     MOVE +0 TO INDX                                                      
026900     PERFORM UNTIL END-OF-W51396                                          
027000       IF IN2-IDDC NOT  = W-IDDC-B6                                       
027100         MOVE IN2-IDDC      TO W-IDDC-B6                                  
027200         PERFORM IMS-GU-WDB601                                            
027300       END-IF                                                             
027400                                                                          
027500       IF DCS-FLWEBDC = JA OR YES                                         
027600         ADD +1 TO INDX                                                   
027700         MOVE DCS-IDLANDX2          TO TAB-IDLANDX2(INDX)                 
027800         MOVE DCS-ADGMT-PADR(11:20) TO TAB-ADCITY  (INDX)                 
027900                                                                          
028000         MOVE IN2-IDDC  TO WBDC-IDDC                                      
028100         CALL WL10WBDC USING WBDC-AREA                                    
028200         MOVE WBDC-KDMFUP TO TAB-KDMFUP(INDX)                             
028300                                                                          
028400         MOVE IN2-TIAAVV          TO TAB-TIAAVV    (INDX)                 
028500         MOVE IN2-IDDC            TO TAB-IDDC      (INDX)                 
028600         MOVE IN2-SUART-REM       TO TAB-SUART-REM (INDX)                 
028700         MOVE IN2-SUART-TOT       TO TAB-SUART-TOT (INDX)                 
028800         MOVE IN2-SUART-INV       TO TAB-SUART-INV (INDX)                 
028900       END-IF                                                             
029000                                                                          
029100       PERFORM S01-LAES-W51396                                            
029200     END-PERFORM                                                          
029300                                                                          
029400     MOVE INDX TO ANTAL-POSTER                                            
029500     IF INDX > ZERO                                                       
029600       PERFORM C-SORT-TABELL                                              
029700       PERFORM D-SKAPA-LINE                                               
029800*      IF WS-FLOPEN = 'J'                                                 
029900       PERFORM S05-SEND-CLOSE                                             
030000     END-IF                                                               
030100     PERFORM Z-FINIT                                                      
030200     MOVE ZERO TO RETURN-CODE                                             
030300     GOBACK                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 A-INIT SECTION.                                                          
030700     SKIP2                                                                
030800                                                                          
030900     OPEN INPUT W51394                                                    
031000     OPEN INPUT W51396                                                    
031100                                                                          
031200     ACCEPT WS-YYMMDD      FROM DATE                                      
031300     ACCEPT WS-HHMMSSTH    FROM TIME                                      
031400     MOVE   WS-HHMM      TO WS-TIME                                       
031500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
031600     INITIALIZE SORT-TABELL                                               
031700     .                                                                    
031800     EJECT                                                                
031900 B-WEB-LISTA  SECTION.                                                    
032000     IF IN-IDDC NOT = WS-IDDC-WEB                                         
032100       MOVE IN-IDDC TO WS-IDDC-WEB                                        
032200       PERFORM S05-SEND-CLOSE                                             
032300       PERFORM BA-SKAPA-HEADER                                            
032400       PERFORM BC-SKAPA-LINE                                              
032500     ELSE                                                                 
032600       PERFORM BC-SKAPA-LINE                                              
032700     END-IF                                                               
032800     .                                                                    
032900     SKIP3                                                                
033000 BA-SKAPA-HEADER SECTION.                                                 
033100******IF WZ04-SEND-IDCOM = ZERO                                           
033200       PERFORM S05-SEND-OPEN                                              
033300       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
033400******END-IF                                                              
033500                                                                          
033600     MOVE 1                          TO HDR-REQU-IDMSGVER                 
033700     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
033800     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
033900                                                                          
034000     MOVE SPACE                      TO HDR-IDOUTREC                      
034100     MOVE 'SELECTION-REG'            TO HDR-IDOUTTYPE                     
034200     MOVE IN-IDDC                    TO HDR-IDOUTREC(1:2)                 
034300     MOVE 'W51394'                   TO HDR-IDOUTREC(3:8)                 
034400     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
034500*HDR                                                                      
034600     PERFORM S05-PUT-HEADER                                               
034700     .                                                                    
034800     SKIP3                                                                
034900 BC-SKAPA-LINE SECTION.                                                   
035000     MOVE '1'                        TO LINE-IDAFPRCD                     
035100     MOVE IN-IDDC                    TO LINE-IDDC                         
035200     MOVE IN-ADLAGOMR                TO LINE-ADLAGOMR                     
035300     MOVE IN-SUART-REM               TO LINE-SUART-REM                    
035400     MOVE IN-SUART-TOT               TO LINE-SUART-TOT                    
035500     MOVE IN-DAAAVV                  TO LINE-DAAAVV                       
035600     PERFORM S05-PUT-REPORT-LINE                                          
035700     .                                                                    
035800     EJECT                                                                
035900                                                                          
036000 C-SORT-TABELL SECTION.                                                   
036100                                                                          
036200     MOVE +45   TO STEGLAANGD                                             
036300     MOVE INDX  TO ANTAL                                                  
036400     MOVE +6    TO NYCKELLAANGD                                           
036500                                                                          
036600     CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                      
036700     TAB-SORT-BEGREPP1(1) NYCKELLAANGD                                    
036800     .                                                                    
036900                                                                          
037000     EJECT                                                                
037100 D-SKAPA-LINE SECTION.                                                    
037200                                                                          
037300     MOVE +1 TO INDX                                                      
037400                                                                          
037500     PERFORM UNTIL INDX >  ANTAL-POSTER                                   
037600       IF TAB-KDMFUP (INDX) NOT = WS-KDMFUP                               
037700*        -- NY MANAGMENT FOLLOW-UP GROUP (= NY LISTA)                     
037800         IF INDX > 1                                                      
037900*          -- STÄNG FÖREGÅENDE LISTA                                      
038000           PERFORM S05-SEND-CLOSE                                         
038100         END-IF                                                           
038200                                                                          
038300*        -- ÖPPNA NY LISTA OCH SKRIV DAP-HEADER                           
038400         PERFORM S05-SEND-OPEN                                            
038500         MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                               
038600         MOVE TAB-KDMFUP (INDX)   TO WS-KDMFUP                            
038700*    -- SPARA LAND FÖR ATT UNDVIKA EN ONÖDIG INLEDANDE BLANK RAD          
038800         MOVE TAB-IDLANDX2 (INDX) TO WS-IDLANDX2                          
038900         PERFORM DA-SKAPA-HEADER                                          
039000       END-IF                                                             
039100                                                                          
039200       IF TAB-IDLANDX2 (INDX) NOT = WS-IDLANDX2                           
039300*        -- EN BLANK RAD EFTER FÖREGÅENDE LAND                            
039400         MOVE ALL '+'                   TO DOC-LINE-AREA2                 
039500         MOVE '2'                       TO LINE2-IDAFPRCD                 
039600         PERFORM S05-PUT-REPORT-LINE2                                     
039700         MOVE TAB-IDLANDX2 (INDX)       TO WS-IDLANDX2                    
039800       END-IF                                                             
039900                                                                          
040000*      -- EN VANLIG DETALJRAD                                             
040100       MOVE '1'                         TO LINE2-IDAFPRCD                 
040200       MOVE TAB-IDDC      (INDX)        TO LINE2-IDDC                     
040300       MOVE TAB-ADCITY    (INDX)        TO LINE2-ADCITY                   
040400       MOVE TAB-IDLANDX2  (INDX)        TO LINE2-IDLANDX2                 
040500       MOVE TAB-SUART-REM (INDX)        TO LINE2-SUART-REM                
040600       MOVE TAB-SUART-TOT (INDX)        TO LINE2-SUART-TOT                
040700       MOVE TAB-SUART-INV (INDX)        TO LINE2-SUART-INV                
040800       MOVE TAB-TIAAVV    (INDX)        TO LINE2-TIAAVV                   
040900       PERFORM S05-PUT-REPORT-LINE2                                       
041000                                                                          
041100       ADD +1 TO INDX                                                     
041200     END-PERFORM                                                          
041300     .                                                                    
041400                                                                          
041500                                                                          
041600 DA-SKAPA-HEADER SECTION.                                                 
041700                                                                          
041800     MOVE 1                          TO HDR2-REQU-IDMSGVER                
041900     MOVE 'E'                        TO HDR2-REQU-KDPGMACT                
042000     MOVE IDPGM                      TO HDR2-REQU-IDUSER                  
042100                                                                          
042200     MOVE 'MAN-SELECT-REG'           TO HDR2-HDR-IDOUTTYPE                
042300                                                                          
042400     MOVE SPACE                      TO HDR2-HDR-IDOUTREC                 
042500     MOVE WS-KDMFUP                  TO HDR2-HDR-IDOUTREC(1:2)            
042600     MOVE 'W51394'                   TO HDR2-HDR-IDOUTREC(3:6)            
042700                                                                          
042800     MOVE WS-YYMMDDHHMM              TO HDR2-HDR-IDLIST                   
042900*HDR                                                                      
043000     PERFORM S05-PUT-HEADER2                                              
043100     .                                                                    
043200                                                                          
043300     EJECT                                                                
043400 Z-FINIT SECTION.                                                         
043500                                                                          
043600                                                                          
043700     CLOSE W51394                                                         
043800     CLOSE W51396                                                         
043900                                                                          
044000     SKIP2                                                                
044100     MOVE 'S' TO POSTSUM-OPKOD                                            
044200     CALL POSTSUM USING POSTSUM-PARM                                      
044300     .                                                                    
044400     EJECT                                                                
044500 S01-LAES-W51394  SECTION.                                                
044600     SKIP2                                                                
044700     READ W51394 INTO IN-AREA                                             
044800     AT END                                                               
044900****    MOVE HIGH-VALUE TO IN-ID                                          
045000        SET END-OF-W51394 TO TRUE                                         
045100                                                                          
045200     NOT AT END                                                           
045300        MOVE 'W51394' TO POSTSUM-FDNAMN                                   
045400        MOVE 'W51394D1' TO POSTSUM-DDNAMN2                                
045500        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
045600        CALL POSTSUM USING POSTSUM-PARM                                   
045700     END-READ                                                             
045800     .                                                                    
045900     EJECT                                                                
046000 S01-LAES-W51396  SECTION.                                                
046100     SKIP2                                                                
046200     READ W51396 INTO IN2-AREA                                            
046300     AT END                                                               
046400****    MOVE HIGH-VALUE TO IN-ID                                          
046500        SET END-OF-W51396 TO TRUE                                         
046600                                                                          
046700     NOT AT END                                                           
046800        MOVE 'W51396' TO POSTSUM-FDNAMN                                   
046900        MOVE 'W51394D2' TO POSTSUM-DDNAMN2                                
047000        MOVE 'IN2-'     TO POSTSUM-TRANSTYP                               
047100        CALL POSTSUM USING POSTSUM-PARM                                   
047200     END-READ                                                             
047300     .                                                                    
047400     EJECT                                                                
047500 S05-SEND-OPEN SECTION.                                                   
047600                                                                          
047700     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
047800     MOVE 'OPEN'                          TO SEND-KDFUNC                  
047900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
048000                         SEND-OPEN-AREA                                   
048100     IF SEND-KDRC > ZERO                                                  
048200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
048300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
048400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
048500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
048600     END-IF                                                               
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
049700     IF SEND-KDRC > ZERO                                                  
049800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
049900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
050000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
050100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
050200     END-IF                                                               
050300     .                                                                    
050400     EJECT                                                                
050500 S05-PUT-HEADER2 SECTION.                                                 
050600                                                                          
050700     MOVE 'PUT'                           TO SEND-KDFUNC                  
050800     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
050900     MOVE LENGTH OF HDR-AREA2             TO SEND-KVDLEN                  
051000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
051100                         SEND-KVDLEN                                      
051200                         HDR-AREA2                                        
051300     IF SEND-KDRC > ZERO                                                  
051400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
051500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
051600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
051700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
051800     END-IF                                                               
051900     .                                                                    
052000     EJECT                                                                
052100 S05-PUT-REPORT-LINE    SECTION.                                          
052200                                                                          
052300     MOVE 'PUT'                           TO SEND-KDFUNC                  
052400     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
052500     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
052600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
052700                         SEND-KVDLEN                                      
052800                         DOC-LINE-AREA                                    
052900     IF SEND-KDRC > ZERO                                                  
053000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
053100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
053200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
053300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
053400     END-IF                                                               
053500     .                                                                    
053600     SKIP3                                                                
053700 S05-PUT-REPORT-LINE2   SECTION.                                          
053800                                                                          
053900     MOVE 'PUT'                           TO SEND-KDFUNC                  
054000     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
054100     MOVE LENGTH OF DOC-LINE-AREA2        TO SEND-KVDLEN                  
054200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
054300                         SEND-KVDLEN                                      
054400                         DOC-LINE-AREA2                                   
054500     IF SEND-KDRC > ZERO                                                  
054600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
054700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
054800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
054900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
055000     END-IF                                                               
055100     .                                                                    
055200     SKIP3                                                                
055300 S05-SEND-CLOSE SECTION.                                                  
055400                                                                          
055500     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
055600     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
055700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
055800     .                                                                    
055900 IMS-GU-WDB601    SECTION.                                                
056000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
056100          DELIMITED BY SIZE INTO SSA1                                     
056200     MOVE '  GE' TO GODK-STATUSKODER                                      
056300     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
056400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
056500     PERFORM IMS-STATUSKONTROLL                                           
056600     .                                                                    
056700     EJECT                                                                
056800 IMS-STATUSKONTROLL SECTION.                                              
056900                                                                          
057000     SET STATUS-IX TO 1                                                   
057100     SEARCH GODK-STATUS AT END CALL FELLOG                                
057200        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
057300        CONTINUE                                                          
057400     END-SEARCH                                                           
057500     .                                                                    
