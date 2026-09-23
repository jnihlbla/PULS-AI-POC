000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5135600.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   05/06/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER IN FIL W51355 OCH SKAPAR DAP POSTER                        
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
002000*          --- FIL MED JUSTERINGAR > 5000 KR                              
002100     SELECT W51355                     ASSIGN TO W51356D1.                
002200     SKIP2                                                                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W51355                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W513561      -L.                                               
003300     SKIP3                                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W5135600'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  YES                         PIC X       VALUE 'Y'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  WS-ADRESS                   PIC X(50)                                
004200                   VALUE 'CARPARTS.DAP.DISTRDOC'.                         
004300 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004600 77  KDRC-DISPLAY                PIC Z(5).                                
004700 77  WS-FLWEBDC                  PIC X     VALUE 'N'.                     
004800 77  WS-FLOPEN                   PIC X     VALUE 'N'.                     
004900 77  W-IDLANDX2                  PIC X(2).                                
005000 77  INDX                        PIC S9(9) COMP-3.                        
005100 77  ANTAL-POSTER                PIC S9(9) COMP-3.                        
005200 77  W-ADCITY                    PIC X(20).                               
005300 77  WS-IDDC                     PIC X(2).                                
005310 77  WDCS-KDMFUP                 PIC X(2)    VALUE SPACE.                 
005400                                                                          
005410 77  WS-IDSKYLT-CHINESE          PIC X(3)  VALUE 'RCN'.                   
005420 77  WS-IDSKYLT-ENGLISH          PIC X(3)  VALUE 'GB '.                   
005430                                                                          
005500 01  WS-YYMMDDHHMM.                                                       
005600     03 WS-YYMMDD                PIC  9(6).                               
005700     03 WS-TIME                  PIC  9(4).                               
005800                                                                          
005900 01  WS-HHMMSSTH.                                                         
006000     03 WS-HHMM                  PIC  9(4).                               
006100     03 WS-SSTH                  PIC  9(4).                               
006200                                                                          
006300                                                                          
006400 01  WS-IDDC-WEB                 PIC X(2) VALUE SPACE.                    
006500                                                                          
006600     SKIP2                                                                
006700*--------------------------------------- NYCKLAR TILL BASERNA             
006800                                                                          
006900 01  W-IDDC-B6-X.                                                         
007000     03 W-IDDC-B6        PIC X(2).                                        
007100                                                                          
007200 01  W-IDDC-X.                                                            
007300     03  W-IDDC          PIC X(2)    VALUE SPACE.                         
007400     SKIP3                                                                
007500 01  W-KDSEGKEY-X.                                                        
007600     03  W-KDSEGKEY      PIC X(1)    VALUE '1'.                           
007700                                                                          
007800 01  W-IDARTNR-X.                                                         
007900     03  W-IDARTNR       PIC S9(9)   COMP-3.                              
007910 01  W-IDSKYLT-X.                                                         
007920     03  W-IDSKYLT       PIC X(3)    VALUE SPACE.                         
008000     SKIP3                                                                
008100     EJECT                                                                
008200*---------------------------------------                                  
008300 01  FELTEXT.                                                             
008400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008600                                                                          
008700 77  W51355-EOF-SW               PIC X       VALUE 'N'.                   
008800     88  END-OF-W51355                       VALUE 'J'.                   
008900     EJECT                                                                
009000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009100 01  FILLER REDEFINES DAGENS-DATUM.                                       
009200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009500     EJECT                                                                
009600 01  DYNAMISKA-SUBPROGRAM.                                                
009700*                                                                         
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
010200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010300     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
010400     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
010410     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
010420     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
010500     EJECT                                                                
010600* ----- TABELL ------                                                     
010700 01  TABENTRY-PARM.                                                       
010800     03  STEGLAANGD              PIC S9(9) COMP.                          
010900     03  ANTAL                   PIC S9(9) COMP.                          
011000     03  NYCKELLAANGD            PIC S9(9) COMP.                          
011100 01  FILLER                      PIC X(16) VALUE 'SORT-TABELL'.           
011200 01  SORT-TABELL.                                                         
011300     03  TAB-RAD OCCURS 500.                                              
011400        05  TAB-SORT-BEGREPP1.                                            
011500            07 TAB-KDMFUP        PIC X(2).                                
011510            07 TAB-IDLANDX2      PIC X(2).                                
011600            07 TAB-IDDC          PIC X(2).                                
011700        05  TAB-TIAAVV           PIC 9(4).                                
011800        05  TAB-IDARTNR          PIC 9(8).                                
011900        05  TAB-BEART            PIC X(25).                               
012000        05  TAB-TIINVDAT         PIC 9(5).                                
012100        05  TAB-ADLAGOMR         PIC 9(2).                                
012200        05  TAB-KVJUSTKV         PIC S9(8).                               
012300        05  TAB-KDPRODSL         PIC 9(2).                                
012400        05  TAB-ADCITY           PIC X(20).                               
012500                                                                          
012600*    --- PARAMETERS TO ABEND                                              
012700                                                                          
012800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013100                                                                          
013200*    --- PARAMETRAR TILL POSTSUM                                          
013300*                                                                         
013400*01  -COPY W0005   -PRE  POSTSUM-                                         
013410                                                                          
013420 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
013430*01  -COPY WTRAUTF8                                                       
013440                                                                          
013450*    --- PARAMETRAR TILL WL10WBDC                                         
013460*01  -COPY WL10WBDC                                                       
013470     EJECT                                                                
013500     EJECT                                                                
013600 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
013700*01  -COPY WZ01SEND                                                       
013800     EJECT                                                                
013900 01  IN-AREA-START               PIC X(24)   VALUE                        
014000                                             'IN-AREA-START'.             
014100*01  AREA -COPY W513561     -PRE IN-                                      
014200     EJECT                                                                
014300     SKIP2                                                                
014400                                                                          
014500 01  UT-HDR-START               PIC X(24)   VALUE                         
014600                                             'UT-HDR-START'.              
014700     SKIP2                                                                
014800 01  HDR-AREA.                                                            
014900*   03  -COPY WZ01REQU -PRE HDR-                                          
015000*   03  -COPY WZ04HDR                                                     
015100*                                                                         
015200 01  HDR-AREA2.                                                           
015300*   03  -COPY WZ01REQU -PRE HDR2-                                         
015400*   03  -COPY WZ04HDR  -PRE HDR2-                                         
015500*                                                                         
015600 01  LINE-AREA                   PIC X(24)    VALUE 'LINE-AREA'.          
015700 01  DOC-LINE-AREA.                                                       
015800*    03 -COPY W513562 -PRE LINE-                                          
015900     EJECT                                                                
016000 01  LINE-AREA2                  PIC X(24)    VALUE 'LINE-AREA2'.         
016100 01  DOC-LINE-AREA2.                                                      
016200*    03 -COPY W513563 -PRE LINE2-                                         
016300     EJECT                                                                
016400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016500     SKIP3                                                                
016600*    --- STATUS-KOD FRÅN IMS                                              
016700 01  STATUS-WS                   PIC XX.                                  
016800     88  SEGMENT-FINNS                       VALUE '  '.                  
016900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017200     88  IMS-EJ-OK                           VALUE 'XD'.                  
017300     SKIP2                                                                
017400 01  GODK-STATUSKODER.                                                    
017500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017600     SKIP3                                                                
017700 01  SSA1                        PIC X(64).                               
017800 01  SSA2                        PIC X(64).                               
017900     EJECT                                                                
018000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
018100 01   DLI-IO-AREA-B601.                                                   
018200*     03  -COPY WDB601                                                    
018300                                                                          
018400 01  FILLER               PIC X(16)   VALUE 'WDK601 AREA'.                
018500 01   DLI-IO-WDK601.                                                      
018600*     03  -COPY WDK601                                                    
018700                                                                          
018800 01  FILLER               PIC X(16)   VALUE 'WDK611 AREA'.                
018900 01   DLI-IO-WDK611.                                                      
019000*     03  -COPY WDK611                                                    
019100                                                                          
019200                                                                          
019300 01  FILLER               PIC X(16)   VALUE 'WDK701 AREA'.                
019400 01   DLI-IO-WDK701.                                                      
019500*     03  -COPY WDK701                                                    
019600                                                                          
019700 01  FILLER               PIC X(16)   VALUE 'WDK711 AREA'.                
019800 01   DLI-IO-WDK711.                                                      
019900*     03  -COPY WDK711                                                    
020000                                                                          
020010 01  FILLER               PIC X(16)   VALUE 'WDD311 AREA'.                
020020 01  DLI-IO-WDD311.                                                       
020030*    03  -COPY WDD311                                                     
020040                                                                          
020100*    --- IMS FUNKTIONSKODER                                               
020200*01  -COPY W0003                                                          
020300     EJECT                                                                
020400                                                                          
020500 LINKAGE SECTION.                                                         
020600*01  -COPY W0009   -PRE MSG-                                              
020700     EJECT                                                                
020800 01  DAP-PCB              PIC X.                                          
020900     EJECT                                                                
021000*01  -COPY W0008 -PRE WDB6-                                               
021100     05  FILLER           PIC X.                                          
021200                                                                          
021300*01  -COPY W0008 -PRE WDK6-                                               
021400     05  FILLER           PIC X.                                          
021500                                                                          
021600*01  -COPY W0008 -PRE WDK7-                                               
021700     05  FILLER           PIC X.                                          
021800                                                                          
021810*01  -COPY W0008 -PRE WDD3-                                               
021820     05  FILLER           PIC X.                                          
021830                                                                          
021900 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB                       
022000                           WDK6-PCB WDK7-PCB WDD3-PCB.                    
022100 MAIN SECTION.                                                            
022200     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB                       
022300                           WDK6-PCB WDK7-PCB WDD3-PCB.                    
022400                                                                          
022500     SKIP2                                                                
022600     PERFORM A-INIT                                                       
022700     PERFORM S01-LAES-W51355                                              
022800     IF NOT END-OF-W51355                                                 
022900** LÄS FRAM TILL FÖRSTA LDC POSTEN                                        
023000       PERFORM UNTIL WS-FLWEBDC = 'J'  OR END-OF-W51355                   
023100         MOVE IN-IDDC TO W-IDDC-B6                                        
023200         PERFORM IMS-GU-WDB601                                            
023300         IF DCS-FLWEBDC = JA OR YES                                       
023400           PERFORM BA-SKAPA-HEADER                                        
023500           PERFORM BC-SKAPA-LINE                                          
023600           MOVE IN-IDDC TO WS-IDDC-WEB                                    
023700           MOVE 'J'     TO WS-FLWEBDC                                     
023800           MOVE 'J'     TO WS-FLOPEN                                      
023900         ELSE                                                             
024000           PERFORM S01-LAES-W51355                                        
024100           MOVE IN-IDDC TO W-IDDC-B6                                      
024200           PERFORM IMS-GU-WDB601                                          
024300         END-IF                                                           
024400       END-PERFORM                                                        
024500****                                                                      
024600       IF NOT END-OF-W51355                                               
024700         PERFORM S01-LAES-W51355                                          
024800         IF NOT END-OF-W51355                                             
024900           MOVE IN-IDDC    TO W-IDDC-B6                                   
025000           PERFORM UNTIL END-OF-W51355                                    
025100             PERFORM IMS-GU-WDB601                                        
025200             IF DCS-FLWEBDC = JA OR YES                                   
025300               PERFORM B-WEB-LISTA                                        
025400             END-IF                                                       
025500             PERFORM S01-LAES-W51355                                      
025600             MOVE IN-IDDC TO W-IDDC-B6                                    
025700           END-PERFORM                                                    
025800           PERFORM S05-SEND-CLOSE                                         
025900           MOVE 'N' TO WS-FLOPEN                                          
026000         END-IF                                                           
026100       END-IF                                                             
026200     END-IF                                                               
026300     IF WS-FLOPEN = 'J'                                                   
026400       PERFORM S05-SEND-CLOSE                                             
026500     END-IF                                                               
026600                                                                          
026700     CLOSE W51355                                                         
026800                                                                          
026900     OPEN INPUT W51355                                                    
027000     MOVE NEJ TO W51355-EOF-SW                                            
027100     MOVE NEJ TO WS-FLWEBDC                                               
027200                                                                          
027300* FIXA TILL NYA MANAGEMNET LISTA PÅ SAMMA INFIL                           
027400* ALLA LDC PÅ SAMMA DOKUMENT                                              
027500                                                                          
027600     PERFORM S01-LAES-W51355                                              
027700     IF NOT END-OF-W51355                                                 
027800       MOVE +0 TO INDX                                                    
027900** LÄS FRAM TILL FÖRSTA LDC POSTEN                                        
028000       PERFORM UNTIL WS-FLWEBDC = 'J'  OR END-OF-W51355                   
028100         MOVE IN-IDDC TO W-IDDC-B6                                        
028200         PERFORM IMS-GU-WDB601                                            
028300         IF DCS-FLWEBDC = JA OR YES                                       
028400           MOVE 'J' TO WS-FLWEBDC                                         
028500           PERFORM C-FYLL-TABELL                                          
028600         ELSE                                                             
028700           PERFORM S01-LAES-W51355                                        
028800           MOVE IN-IDDC TO W-IDDC-B6                                      
028900           PERFORM IMS-GU-WDB601                                          
029000         END-IF                                                           
029100       END-PERFORM                                                        
029200****                                                                      
029300       IF NOT END-OF-W51355                                               
029400         PERFORM S01-LAES-W51355                                          
029500         IF NOT END-OF-W51355                                             
029600           MOVE IN-IDDC    TO W-IDDC-B6                                   
029700           PERFORM UNTIL END-OF-W51355                                    
029800             PERFORM IMS-GU-WDB601                                        
029900             IF DCS-FLWEBDC = JA OR YES                                   
030000               PERFORM C-FYLL-TABELL                                      
030100             END-IF                                                       
030200             PERFORM S01-LAES-W51355                                      
030300             MOVE IN-IDDC TO W-IDDC-B6                                    
030400           END-PERFORM                                                    
030500         END-IF                                                           
030600       END-IF                                                             
030700     END-IF                                                               
030800     IF INDX > +0                                                         
030900       PERFORM D-SORT-TABELL                                              
031000       PERFORM E-SKAPA-DAP                                                
031100     END-IF                                                               
031200                                                                          
031300     IF WS-FLOPEN = 'J'                                                   
031400       PERFORM S05-SEND-CLOSE                                             
031500     END-IF                                                               
031600                                                                          
031700     PERFORM Z-FINIT                                                      
031800     MOVE ZERO TO RETURN-CODE                                             
031900     GOBACK                                                               
032000     .                                                                    
032100     EJECT                                                                
032200 A-INIT SECTION.                                                          
032300     SKIP2                                                                
032400                                                                          
032500     OPEN INPUT W51355                                                    
032600                                                                          
032700     ACCEPT WS-YYMMDD      FROM DATE                                      
032800     ACCEPT WS-HHMMSSTH    FROM TIME                                      
032900     MOVE   WS-HHMM      TO WS-TIME                                       
033000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
033100     .                                                                    
033200     EJECT                                                                
033300 B-WEB-LISTA  SECTION.                                                    
033400     IF IN-IDDC NOT = WS-IDDC-WEB                                         
033500       MOVE IN-IDDC TO WS-IDDC-WEB                                        
033600       IF DCS-FLWEBDC = JA OR YES                                         
033700         PERFORM S05-SEND-CLOSE                                           
033800         PERFORM BA-SKAPA-HEADER                                          
033900         PERFORM BC-SKAPA-LINE                                            
034000       END-IF                                                             
034100     ELSE                                                                 
034200       PERFORM BC-SKAPA-LINE                                              
034300     END-IF                                                               
034400     .                                                                    
034500     SKIP3                                                                
034510                                                                          
034600 BA-SKAPA-HEADER SECTION.                                                 
034800     PERFORM S05-SEND-OPEN                                                
034900     MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                   
035100                                                                          
035200     MOVE 001                        TO HDR-REQU-IDMSGVER                 
035300     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
035400     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
035500                                                                          
035600     MOVE SPACE                      TO HDR-IDOUTREC                      
035700     MOVE 'ADJ-5000'                 TO HDR-IDOUTTYPE                     
035800     MOVE IN-IDDC                    TO HDR-IDOUTREC(1:2)                 
035900     MOVE 'W51356'                   TO HDR-IDOUTREC(3:8)                 
036000     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
036100*HDR                                                                      
036200     PERFORM S05-PUT-HEADER                                               
036300     .                                                                    
036400     SKIP3                                                                
036410                                                                          
036500 BC-SKAPA-LINE SECTION.                                                   
036600     MOVE '1'                        TO LINE-IDAFPRCD                     
036700     MOVE IN-IDDC                    TO LINE-IDDC                         
036800     MOVE IN-IDARTNR                 TO LINE-IDARTNR                      
036900     MOVE IN-BEART                   TO LINE-BEART                        
036910                                                                          
036920*    WE NEED SOME ADJUSTMENT FOR CHINESE NAME                             
036930*    BEFORE DISPLAY OF LINE-BEART                                         
036940                                                                          
036950     MOVE IN-IDARTNR            TO W-IDARTNR                              
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
036994     PERFORM IMS-GU-WDD311                                                
036995     IF SEGMENT-FINNS                                                     
036996        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
036997     ELSE                                                                 
036998        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
036999        MOVE '278 '             TO TRAUTF8-KDCP                           
037000     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
037001     MOVE 25                    TO TRAUTF8-KVMAXTL                        
037002     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
037003     MOVE TRAUTF8-TECONV-TO     TO LINE-BEART                             
037004                                                                          
037010     MOVE IN-TIAAVV                  TO LINE-TIAAVV                       
037100     MOVE IN-TIINVDAT                TO LINE-TIINVDAT                     
037200     MOVE IN-ADLAGOMR                TO LINE-ADLAGOMR                     
037300     MOVE IN-KVJUSTKV                TO LINE-KVJUSTKV                     
037400     MOVE IN-KDPRODSL                TO LINE-KDPRODSL                     
037500     PERFORM S05-PUT-REPORT-LINE                                          
037600     .                                                                    
037700     EJECT                                                                
037800                                                                          
039500 C-FYLL-TABELL  SECTION.                                                  
039600     ADD +1 TO INDX                                                       
039610     MOVE IN-IDDC TO WBDC-IDDC                                            
039620     CALL WL10WBDC USING WBDC-AREA                                        
039630     IF WBDC-FLWEBDC = JA                                                 
039640       MOVE WBDC-KDMFUP TO WDCS-KDMFUP                                    
039650     ELSE                                                                 
039660       MOVE SPACE       TO WDCS-KDMFUP                                    
039670     END-IF                                                               
039700     MOVE WDCS-KDMFUP          TO TAB-KDMFUP            (INDX)            
039710     MOVE IN-IDDC              TO TAB-IDDC              (INDX)            
039800     MOVE IN-TIAAVV            TO TAB-TIAAVV            (INDX)            
039900     MOVE IN-IDARTNR           TO TAB-IDARTNR           (INDX)            
040000     MOVE IN-BEART             TO TAB-BEART             (INDX)            
040010                                                                          
040020*    WE NEED SOME ADJUSTMENT FOR CHINESE NAME                             
040030*    BEFORE DISPLAY OF TAB-BEART                                          
040040                                                                          
040050     MOVE IN-IDARTNR                 TO W-IDARTNR                         
040061     IF DCS-CHINA                                                         
040070        MOVE WS-IDSKYLT-CHINESE      TO W-IDSKYLT                         
040080        MOVE 'UTF8'                  TO TRAUTF8-KDCP                      
040090     ELSE                                                                 
040091        MOVE WS-IDSKYLT-ENGLISH      TO W-IDSKYLT                         
040092        MOVE '278 '                  TO TRAUTF8-KDCP                      
040093     END-IF                                                               
040094     PERFORM IMS-GU-WDD311                                                
040095     IF SEGMENT-FINNS                                                     
040096        MOVE TEXT-BEART              TO TRAUTF8-TECONV-FROM               
040097     ELSE                                                                 
040098        MOVE SPACE                   TO TRAUTF8-TECONV-FROM               
040099        MOVE '278 '                  TO TRAUTF8-KDCP                      
040100     END-IF                                                               
040101     MOVE 25                         TO TRAUTF8-KVMAXTL                   
040102     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
040103     MOVE TRAUTF8-TECONV-TO          TO TAB-BEART       (INDX)            
040104                                                                          
040110     MOVE IN-TIINVDAT          TO TAB-TIINVDAT          (INDX)            
040200     MOVE IN-ADLAGOMR          TO TAB-ADLAGOMR          (INDX)            
040300     MOVE IN-KVJUSTKV          TO TAB-KVJUSTKV          (INDX)            
040400     MOVE IN-KDPRODSL          TO TAB-KDPRODSL          (INDX)            
040500     MOVE DCS-IDLANDX2         TO TAB-IDLANDX2          (INDX)            
040600     MOVE DCS-ADGMT-PADR(11:20) TO TAB-ADCITY           (INDX)            
040700     .                                                                    
040800     EJECT                                                                
040810                                                                          
040900 D-SORT-TABELL SECTION.                                                   
041000       MOVE +80   TO STEGLAANGD                                           
041100       MOVE INDX  TO ANTAL                                                
041200       MOVE +6    TO NYCKELLAANGD                                         
041300                                                                          
041400       CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                    
041500       TAB-SORT-BEGREPP1(1) NYCKELLAANGD                                  
041600     .                                                                    
041700     EJECT                                                                
041710                                                                          
041800 E-SKAPA-DAP SECTION.                                                     
041900     MOVE INDX TO ANTAL-POSTER                                            
042000     MOVE +1 TO INDX                                                      
042100     MOVE TAB-KDMFUP   (INDX) TO WDCS-KDMFUP                              
042110     MOVE TAB-IDLANDX2 (INDX) TO W-IDLANDX2                               
042200     MOVE TAB-IDDC     (INDX) TO WS-IDDC                                  
042210                                                                          
042300     PERFORM EA-SKAPA-HEADER                                              
042400     PERFORM UNTIL INDX > ANTAL-POSTER                                    
042500       IF TAB-IDLANDX2 (INDX) NOT = W-IDLANDX2                            
042600         MOVE ALL '+'                  TO DOC-LINE-AREA2                  
042700         MOVE '1'                      TO LINE2-IDAFPRCD                  
042800         PERFORM S05-PUT-REPORT-LINE2                                     
042900         MOVE TAB-IDLANDX2 (INDX) TO W-IDLANDX2                           
043000       END-IF                                                             
043100                                                                          
043200       PERFORM EB-SKAPA-LINE                                              
043500       ADD +1 TO INDX                                                     
043600     END-PERFORM                                                          
043700     .                                                                    
043800     EJECT                                                                
043900                                                                          
044000 EA-SKAPA-HEADER SECTION.                                                 
044100     MOVE 'J'     TO WS-FLOPEN                                            
044200***  IF WZ04-SEND-IDCOM = ZERO                                            
044300       PERFORM S05-SEND-OPEN                                              
044400       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
044500***  END-IF                                                               
044600                                                                          
044700     MOVE 001                        TO HDR2-REQU-IDMSGVER                
044800     MOVE 'E'                        TO HDR2-REQU-KDPGMACT                
044900     MOVE IDPGM                      TO HDR2-REQU-IDUSER                  
045000                                                                          
045100     MOVE SPACE                      TO HDR2-HDR-IDOUTREC                 
045200     MOVE 'MAN-ADJ-5000'             TO HDR2-HDR-IDOUTTYPE                
045230     MOVE WDCS-KDMFUP                TO HDR2-HDR-IDOUTREC(1:2)            
045500     MOVE WS-YYMMDDHHMM              TO HDR2-HDR-IDLIST                   
045600*HDR                                                                      
045700     PERFORM S05-PUT-HEADER2                                              
045800     .                                                                    
045900     SKIP3                                                                
045910                                                                          
046000 EB-SKAPA-LINE SECTION.                                                   
046200     MOVE '1'                        TO LINE2-IDAFPRCD                    
046300     MOVE TAB-IDDC     (INDX)        TO LINE2-IDDC                        
046400                                        W-IDDC                            
046410                                        W-IDDC-B6                         
046500     MOVE TAB-IDARTNR  (INDX)        TO LINE2-IDARTNR                     
046600                                        W-IDARTNR                         
046700     MOVE TAB-BEART    (INDX)        TO LINE2-BEART                       
046804                                                                          
046810     MOVE TAB-TIAAVV   (INDX)        TO LINE2-TIAAVV                      
046900     MOVE TAB-TIINVDAT (INDX)        TO LINE2-TIINVDAT                    
047000     MOVE TAB-ADLAGOMR (INDX)        TO LINE2-ADLAGOMR                    
047100     MOVE TAB-KVJUSTKV (INDX)        TO LINE2-KVJUSTKV                    
047200     MOVE TAB-KDPRODSL (INDX)        TO LINE2-KDPRODSL                    
047300     MOVE TAB-IDLANDX2 (INDX)        TO LINE2-IDLANDX2                    
047400     MOVE TAB-ADCITY   (INDX)        TO LINE2-ADCITY                      
047500                                                                          
047600     PERFORM IMS-GU-WDK601                                                
047610     IF SEGMENT-FINNS                                                     
047611        PERFORM IMS-GNP-WDK611                                            
047612        COMPUTE LINE2-SUARTSTD = TAB-KVJUSTKV (INDX)                      
047613                                 * CLAG-PRARTSTD                          
047614     ELSE                                                                 
047615        MOVE 0                      TO LINE2-SUARTSTD                     
047616     END-IF                                                               
047617                                                                          
047634     IF TAB-KDMFUP (INDX) NOT = WDCS-KDMFUP                               
047635        MOVE TAB-KDMFUP (INDX)   TO WDCS-KDMFUP                           
047636        PERFORM S05-SEND-CLOSE                                            
047637        PERFORM EA-SKAPA-HEADER                                           
047638     END-IF                                                               
047639                                                                          
047900     PERFORM IMS-GU-WDK711                                                
048000     MOVE SLAG-ADLAGOMR              TO LINE2-ADLAGOMR                    
048100                                                                          
048200     IF TAB-IDDC    (INDX)  = WS-IDDC AND INDX > 1                        
048300       MOVE SPACE                    TO LINE2-ADCITY                      
048400       MOVE SPACE                    TO LINE2-IDDC                        
048500       MOVE SPACE                    TO LINE2-IDLANDX2                    
048600                                                                          
048700     ELSE                                                                 
048800       MOVE TAB-IDDC (INDX)          TO WS-IDDC                           
048900     END-IF                                                               
049000                                                                          
049100                                                                          
049200     PERFORM S05-PUT-REPORT-LINE2                                         
049300     .                                                                    
049400     EJECT                                                                
049410                                                                          
049500 Z-FINIT SECTION.                                                         
049700     CLOSE W51355                                                         
049800                                                                          
049900     SKIP2                                                                
050000     MOVE 'S' TO POSTSUM-OPKOD                                            
050100     CALL POSTSUM USING POSTSUM-PARM                                      
050200     .                                                                    
050300     EJECT                                                                
050310                                                                          
050400 S01-LAES-W51355  SECTION.                                                
050500     SKIP2                                                                
050600     READ W51355 INTO IN-AREA                                             
050700                                                                          
050800     AT END                                                               
050900****    MOVE HIGH-VALUE TO IN-ID                                          
051000        SET END-OF-W51355 TO TRUE                                         
051100                                                                          
051200     NOT AT END                                                           
051300        MOVE 'W51355' TO POSTSUM-FDNAMN                                   
051400        MOVE 'W51356D1' TO POSTSUM-DDNAMN2                                
051500        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
051600        CALL POSTSUM USING POSTSUM-PARM                                   
051700     END-READ                                                             
051800     .                                                                    
051900     EJECT                                                                
051910                                                                          
052000 S05-SEND-OPEN SECTION.                                                   
052200     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
052300     MOVE 'OPEN'                          TO SEND-KDFUNC                  
052400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
052500                         SEND-OPEN-AREA                                   
052600     IF SEND-KDRC > ZERO                                                  
052700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
052800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
052900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
053000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
053100     END-IF                                                               
053200     .                                                                    
053300     SKIP3                                                                
053310                                                                          
053400 S05-PUT-HEADER SECTION.                                                  
053600     MOVE 'PUT'                           TO SEND-KDFUNC                  
053700     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
053800     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
053900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
054000                         SEND-KVDLEN                                      
054100                         HDR-AREA                                         
054200     IF SEND-KDRC > ZERO                                                  
054300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
054400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
054500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
054600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
054700     END-IF                                                               
054800     .                                                                    
054900     EJECT                                                                
054910                                                                          
055000 S05-PUT-HEADER2 SECTION.                                                 
055200     MOVE 'PUT'                           TO SEND-KDFUNC                  
055300     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
055400     MOVE LENGTH OF HDR-AREA2             TO SEND-KVDLEN                  
055500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
055600                         SEND-KVDLEN                                      
055700                         HDR-AREA2                                        
055800     IF SEND-KDRC > ZERO                                                  
055900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
056000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
056100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
056200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056510                                                                          
056600 S05-PUT-REPORT-LINE    SECTION.                                          
056800     MOVE 'PUT'                           TO SEND-KDFUNC                  
056900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
057000     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
057100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
057200                         SEND-KVDLEN                                      
057300                         DOC-LINE-AREA                                    
057400     IF SEND-KDRC > ZERO                                                  
057500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
057600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
057700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
057800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
057900     END-IF                                                               
058000     .                                                                    
058100     SKIP3                                                                
058110                                                                          
058200 S05-PUT-REPORT-LINE2   SECTION.                                          
058300     MOVE 'PUT'                           TO SEND-KDFUNC                  
058400     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
058500     MOVE LENGTH OF DOC-LINE-AREA2        TO SEND-KVDLEN                  
058600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
058700                         SEND-KVDLEN                                      
058800                         DOC-LINE-AREA2                                   
058900     IF SEND-KDRC > ZERO                                                  
059000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
059100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
059200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
059300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
059400     END-IF                                                               
059500     .                                                                    
059600     SKIP3                                                                
059610                                                                          
059700 S05-SEND-CLOSE SECTION.                                                  
059800     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
059900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
060000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
060100     .                                                                    
060110                                                                          
060200 IMS-GU-WDB601    SECTION.                                                
060300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
060400          DELIMITED BY SIZE INTO SSA1                                     
060500     MOVE '  GE' TO GODK-STATUSKODER                                      
060600     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
060700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
060800     PERFORM IMS-STATUSKONTROLL                                           
061100     .                                                                    
061200     EJECT                                                                
061210                                                                          
061300 IMS-GU-WDK711 SECTION.                                                   
061400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
061500     DELIMITED BY SIZE INTO SSA1                                          
061600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
061700     DELIMITED BY SIZE INTO SSA2                                          
061800     MOVE '  GE' TO GODK-STATUSKODER                                      
061900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711   SSA1 SSA2             
062000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
062100     PERFORM IMS-STATUSKONTROLL                                           
062200     .                                                                    
062300     SKIP2                                                                
062310                                                                          
062400 IMS-GU-WDK601 SECTION.                                                   
062600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
062700     DELIMITED BY SIZE INTO SSA1                                          
062800     MOVE '  GE' TO GODK-STATUSKODER                                      
062900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601   SSA1                  
063000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
063100     PERFORM IMS-STATUSKONTROLL                                           
063200     .                                                                    
063300     SKIP3                                                                
063310                                                                          
063400 IMS-GNP-WDK611 SECTION.                                                  
063500     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
063600     DELIMITED BY SIZE INTO SSA1                                          
063700     MOVE '  GE' TO GODK-STATUSKODER                                      
063800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611   SSA1                 
063900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
064000     PERFORM IMS-STATUSKONTROLL                                           
064100     .                                                                    
064200     SKIP3                                                                
064201                                                                          
064210 IMS-GU-WDD311 SECTION.                                                   
064230     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
064240             DELIMITED BY SIZE INTO SSA1                                  
064250     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
064260             DELIMITED BY SIZE INTO SSA2                                  
064270     MOVE '  GE'                 TO GODK-STATUSKODER                      
064280     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
064290     MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
064291     PERFORM IMS-STATUSKONTROLL                                           
064292     .                                                                    
064293     SKIP3                                                                
064294                                                                          
064300 IMS-STATUSKONTROLL SECTION.                                              
064500     SET STATUS-IX TO 1                                                   
064600     SEARCH GODK-STATUS AT END CALL FELLOG                                
064700        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
064800        CONTINUE                                                          
064900     END-SEARCH                                                           
065000     .                                                                    
