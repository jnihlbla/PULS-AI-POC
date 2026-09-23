000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4182000.                                                
000300 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000400 DATE-WRITTEN.   95/06/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄGGER UPP LEVANM. VIA DISPATCHEN.                               
000900*        LÄGGER UPP BAYBACK VIA DISPATCHEN.                               
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDP8                                       
001200*                                                                         
001300* CHANGE LOG:                                                             
001400*        E-TRACKER 8635407  20091021 RETURN CODES MATRIX                  
001500*        E-TRACKER 10143271 2011-11-22 CHINA WAREHOUSE PROJECT-1          
001600*                                                                         
001700*                                                                         
001800                                                                          
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500                                                                          
002600*          --- KONTROLLERADE LEV.ANM.POSTER                               
002700     SELECT W41810                     ASSIGN TO W41820D1.                
002800     SELECT W41813                     ASSIGN TO W41820D2.                
002900     SELECT W01521                     ASSIGN TO W41820D3.                
003000                                                                          
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300                                                                          
003400 FILE SECTION.                                                            
003500                                                                          
003600 FD  W41810                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W41810      -L.                                                
004100                                                                          
004200 FD  W41813                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY W41810      -L.                                                
004700                                                                          
004800 FD  W01521                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  UT-W01521 -COPY WMSGKOM     -L.                                      
005300                                                                          
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800                                                                          
005900 77  IDPGM                       PIC X(8)   VALUE 'W4182000'.             
006000 77  JA                          PIC X      VALUE 'J'.                    
006100 77  NEJ                         PIC X      VALUE 'N'.                    
006200 77  END-FIL1                    PIC X      VALUE 'N'.                    
006300 77  NY-BUNT                     PIC X      VALUE 'J'.                    
006400 77  MATRIX-Q                    PIC X      VALUE 'N'.                    
006500 77  IX                          PIC S9(9)  VALUE ZERO COMP SYNC.         
006600 77  MAX-IX                      PIC S9(9)  VALUE +4  COMP SYNC.          
006700 77  WS-KVRADER                  PIC  9(5)  VALUE ZERO.                   
006800 77  WS-ANTAL-ISRT               PIC S9(7)  VALUE +0 COMP-3.              
006900 77  WS-ANTAL-ISRT-TXT           PIC S9(7)  VALUE +0 COMP-3.              
007000 77  DAGENS-DATUM                PIC  9(6)  VALUE ZERO.                   
007100 77  DAGENS-TID                  PIC  9(8)  VALUE ZERO.                   
007200 77  HELP-IDDISTR                PIC  9(4)  VALUE ZERO.                   
007300 77  HELP-IDKUNDNR               PIC  9(6)  VALUE ZERO.                   
007400 77  HELP-IDARTNR                PIC  9(9)  VALUE ZERO.                   
007500 77  HELP-IDRADNR                PIC  9(4)  VALUE ZERO.                   
007600 77  HELP-IDKONTO                PIC  9(10) VALUE ZERO.                   
007700 77  HELP-IDFAKT                 PIC  9(7)  VALUE ZERO.                   
007800 77  HELP-IDFAKT-LOC             PIC  9(7)  VALUE ZERO.                   
007900 77  HELP-IDKOLLI                PIC  9(5)  VALUE ZERO.                   
008000 77  HELP-KDFRAKT                PIC  9(2)  VALUE ZERO.                   
008100 77  HELP-IDFTG                  PIC  9(2)  VALUE ZERO.                   
008200 77  HELP-KDORDKL                PIC  9(1)  VALUE ZERO.                   
008300 77  HELP-KVLEVANM               PIC  9(6)  VALUE ZERO.                   
008400 77  HELP-TIFAKT                 PIC  9(6)  VALUE ZERO.                   
008500 77  HELP-TIFAKT-LOC             PIC  9(6)  VALUE ZERO.                   
008600 77  HELP-TILEVANM               PIC  9(6)  VALUE ZERO.                   
008700 77  HELP-IDLOPNRM               PIC  9(8)  VALUE ZERO.                   
008800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
008900                                                                          
009000 01 WS-4723-TABELL.                                                       
009100    03 WS-4723-GRP OCCURS 7.                                              
009200       05 WS-SPAR-IDDISTR        PIC 9(4).                                
009300       05 WS-SPAR-IDKUNDNR       PIC 9(6).                                
009400       05 WS-SPAR-IDRAPPNR       PIC 9(7).                                
009500       05 WS-SPAR-IDARTNR        PIC 9(9).                                
009600       05 WS-SPAR-IDRADNR        PIC 9(4).                                
009700       05 WS-SPAR-TEANMNOT-GRP.                                           
009800          07 WS-SPAR-TEANMNOT-REG OCCURS 3 PIC X(70).                     
009900                                                                          
010000 01  WS-PRARTBTO.                                                         
010100    03 WS-PRARTBTO-ALFA          PIC X(10).                               
010200 01  FILLER REDEFINES WS-PRARTBTO.                                        
010300   03 WS-PRARTBTO-NUM            PIC  9(7).9(2).                          
010400                                                                          
010500 01  WS-PRARTBTO-LOC.                                                     
010600    03 WS-PRARTBTO-ALFA-LOC      PIC X(10).                               
010700 01  FILLER REDEFINES WS-PRARTBTO-LOC.                                    
010800   03 WS-PRARTBTO-NUM-LOC        PIC  9(7).9(2).                          
010900                                                                          
011000 01  WS-PRARTBTO-LOCINV.                                                  
011100    03 WS-PRARTBTO-ALFA-LOCINV   PIC X(10).                               
011200 01  FILLER REDEFINES WS-PRARTBTO-LOCINV.                                 
011300   03 WS-PRARTBTO-NUM-LOCINV        PIC  9(7).9(2).                       
011400                                                                          
011500 01  WS-PRFRAKT.                                                          
011600    03 WS-PRFRAKT-ALFA           PIC X(10).                               
011700 01  FILLER REDEFINES WS-PRFRAKT.                                         
011800   03 WS-PRFRAKT-NUM             PIC  9(7).9(2).                          
011900                                                                          
012000 01  WS-PRARTSTD.                                                         
012100    03 WS-PRARTSTD-ALFA          PIC X(10).                               
012200 01  FILLER REDEFINES WS-PRARTSTD.                                        
012300   03 WS-PRARTSTD-NUM            PIC  9(7).9(2).                          
012400                                                                          
012500 01  WS-PRARTSJK.                                                         
012600    03 WS-PRARTSJK-ALFA          PIC X(10).                               
012700 01  FILLER REDEFINES WS-PRARTSJK.                                        
012800   03 WS-PRARTSJK-NUM            PIC  9(7).9(2).                          
012900                                                                          
013000*                                                                         
013100 01  CHKP-VAR.                                                            
013200   03 CHKP-MSG-IO-AREA-LENGTH    PIC S9(9)   VALUE +32 COMP SYNC.         
013300   03 CHKP-MSG-IO-AREA           PIC X(32)   VALUE SPACE.                 
013400   03 CHKP-AREA-LENGTH           PIC S9(9)   VALUE +32 COMP SYNC.         
013500   03 CHKP-AREA                  PIC X(32)   VALUE SPACE.                 
013600   03 CHKP-ANT                   PIC S9(3)   VALUE +0.                    
013700   03 CHKP-MAX                   PIC S9(3)   VALUE +100.                  
013800                                                                          
013900 01  FELTEXT.                                                             
014000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014200                                                                          
014300 77  W41810-EOF-SW               PIC X       VALUE 'N'.                   
014400     88  END-OF-W41810                       VALUE 'J'.                   
014500                                                                          
014600 77  LEVANM-SW                   PIC X       VALUE 'N'.                   
014700     88  NY-LEVANM                           VALUE 'J'.                   
014800                                                                          
014900 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
015000     88  FIRST-TIME                          VALUE 'J'.                   
015100                                                                          
015200     EJECT                                                                
015300 01  TEST-IDDISTR           PIC  9(5) COMP-3 VALUE ZERO.                  
015400                                                                          
015500*01  FILLER  -COPY WWDIST35    -RED TEST-IDDISTR.                         
015600                                                                          
015700     EJECT                                                                
015800 01  DYNAMISKA-SUBPROGRAM.                                                
015900                                                                          
016000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
016500     EJECT                                                                
016600 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
016700*01  -COPY WMSGKOM                                                        
016800     EJECT                                                                
016900*    --- PARAMETRAR TILL POSTSUM                                          
017000*                                                                         
017100*01  -COPY W0005   -PRE  POSTSUM-                                         
017200     EJECT                                                                
017300*    --- IMS FUNKTIONSKODER                                               
017400*01  -COPY W0003                                                          
017500     EJECT                                                                
017600 01  IN-AREA-START               PIC X(24)   VALUE                        
017700                                             'IN-AREA-START'.             
017800     SKIP2                                                                
017900                                                                          
018000*01  AREA -COPY W41810     -PRE IN-                                       
018100*                                                                         
018200     EJECT                                                                
018300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018400     SKIP3                                                                
018500 01  SPAR-AREA-1-START             PIC X(24)   VALUE                      
018600                                            'SPAR-AREA-1-START'.          
018700*                                                                         
018800*01  AREA -COPY W41810     -PRE SPAR-                                     
018900*                                                                         
019000                                                                          
019100*    --- STATUS-KOD FRÅN IMS                                              
019200 01  STATUS-WS                   PIC XX.                                  
019300     88  IMS-EJ-OK                           VALUE 'XD'.                  
019400     SKIP2                                                                
019500 01  GODK-STATUSKODER.                                                    
019600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019700     EJECT                                                                
019800*                                                                         
019900*    --- AREOR FÖR W006KOM SUBMODUL                                       
020000*                                                                         
020100 01  FILLER                      PIC X(16) VALUE 'KOM-IO-AREA'.           
020200                                                                          
020300 01  4791-MSG-IO-AREA.                                                    
020400     03  4791-LL                 PIC S9(4) VALUE +0 COMP SYNC.            
020500     03  4791-Z1                 PIC X.                                   
020600     03  4791-Z2                 PIC X.                                   
020700     03  4791-TRANSKOD           PIC X(8)  VALUE 'W4T791X '.              
020800     03  4791-IDTRANS            PIC X(4)  VALUE '4791'.                  
020900     03  4791-SPRAK              PIC X     VALUE '1'.                     
021000     03  FILLER.                                                          
021100*       05 -COPY W4I79101                                                 
021200     EJECT                                                                
021300 01  4723-MSG-IO-AREA.                                                    
021400     03  4723-LL                 PIC S9(4) VALUE +0 COMP SYNC.            
021500     03  4723-Z1                 PIC X.                                   
021600     03  4723-Z2                 PIC X.                                   
021700     03  4723-TRANSKOD           PIC X(8)  VALUE 'W4T723U '.              
021800     03  4723-IDTRANS            PIC X(4)  VALUE '4723'.                  
021900     03  4723-SPRAK              PIC X     VALUE '1'.                     
022000     03  FILLER.                                                          
022100*       05 -COPY W4I72301                                                 
022200     EJECT                                                                
022300*                                                                         
022400 LINKAGE SECTION.                                                         
022500                                                                          
022600*01  -COPY W0009   -PRE MSG-                                              
022700     EJECT                                                                
022800*01  -COPY W0008  -PRE DISP-                                              
022900     05  FILLER                  PIC X.                                   
023000                                                                          
023100*01  -COPY W0008  -PRE KOMA-                                              
023200     05  FILLER                  PIC X.                                   
023300                                                                          
023400     EJECT                                                                
023500 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB KOMA-PCB.                     
023600 MAIN SECTION.                                                            
023700     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB KOMA-PCB.                     
023800                                                                          
023900     PERFORM A-INIT                                                       
024000                                                                          
024100     PERFORM S01-LAES-W41810                                              
024200     PERFORM UNTIL END-OF-W41810                                          
024300       IF CHKP-ANT > CHKP-MAX                                             
024400         PERFORM X-TAG-CHECKPOINT                                         
024500       END-IF                                                             
024600       PERFORM B-LAES-EN-LEVANM                                           
024700       PERFORM C-BEHANDLA-LEVANM                                          
024800       PERFORM D-BEHANDLA-TEXTEN                                          
024900     END-PERFORM                                                          
025000                                                                          
025100     PERFORM Z-FINIT                                                      
025200     DISPLAY 'ANTAL ISRT MOT DISPATCHEN     = ' WS-ANTAL-ISRT             
025300     DISPLAY 'ANTAL TXT-ISRT MOT DISPATCHEN = ' WS-ANTAL-ISRT-TXT         
025400                                                                          
025500     MOVE ZERO TO RETURN-CODE                                             
025600     GOBACK                                                               
025700     .                                                                    
025800                                                                          
025900     EJECT                                                                
026000 A-INIT SECTION.                                                          
026100                                                                          
026200     PERFORM IMS-RESTART                                                  
026300                                                                          
026400     OPEN INPUT  W41810                                                   
026500                 W41813                                                   
026600     OPEN OUTPUT W01521                                                   
026700                                                                          
026800     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
026900     MOVE +0                   TO WS-ANTAL-ISRT                           
027000     MOVE +0                   TO WS-ANTAL-ISRT-TXT                       
027100                                                                          
027200     COMPUTE 4723-LL        = LENGTH OF MID-W4I72301 + 17                 
027300                                                                          
027400     ACCEPT DAGENS-DATUM       FROM DATE                                  
027500     ACCEPT DAGENS-TID         FROM TIME                                  
027600     .                                                                    
027700                                                                          
027800     EJECT                                                                
027900 B-LAES-EN-LEVANM SECTION.                                                
028000     MOVE '** B-LAES-EN-LEVANM'                                           
028100                               TO FELTEXT-STR                             
028200     MOVE NEJ                  TO LEVANM-SW                               
028300     MOVE JA                   TO FIRST-TIME-SW                           
028400     MOVE +1                   TO IX                                      
028500     MOVE ZERO                 TO WS-KVRADER                              
028600     MOVE SPACE                TO MID-W4I79101                            
028700     MOVE SPACE                TO MID-W4I72301                            
028800     MOVE SPACE                TO WS-4723-TABELL                          
028900     PERFORM UNTIL NY-LEVANM   OR END-OF-W41810 OR IX > MAX-IX            
029000        MOVE IN-W41810         TO SPAR-W41810                             
029100        PERFORM S01-LAES-W41810                                           
029200        IF NOT END-OF-W41810                                              
029300           IF IN-IDDISTR    = SPAR-IDDISTR  AND                           
029400              IN-IDKUNDNR   = SPAR-IDKUNDNR AND                           
029500              IN-IDRAPPNR   = SPAR-IDRAPPNR                               
029600              CONTINUE                                                    
029700           ELSE                                                           
029800              MOVE JA          TO LEVANM-SW                               
029900           END-IF                                                         
030000                                                                          
030100           IF FIRST-TIME                                                  
030200              PERFORM BA-FLYTTA-HUVUD-DATA                                
030300              PERFORM BB-FLYTTA-RAD-DATA                                  
030400              MOVE NEJ         TO FIRST-TIME-SW                           
030500           ELSE                                                           
030600              PERFORM BB-FLYTTA-RAD-DATA                                  
030700           END-IF                                                         
030800           ADD +1              TO IX                                      
030900        ELSE                                                              
031000           IF FIRST-TIME                                                  
031100              PERFORM BA-FLYTTA-HUVUD-DATA                                
031200              PERFORM BB-FLYTTA-RAD-DATA                                  
031300              MOVE NEJ         TO FIRST-TIME-SW                           
031400           ELSE                                                           
031500              PERFORM BB-FLYTTA-RAD-DATA                                  
031600              MOVE NEJ         TO FIRST-TIME-SW                           
031700           END-IF                                                         
031800        END-IF                                                            
031900     END-PERFORM                                                          
032000                                                                          
032100     MOVE WS-KVRADER                   TO MID-KVRADER                     
032200     .                                                                    
032300                                                                          
032400     EJECT                                                                
032500 BA-FLYTTA-HUVUD-DATA SECTION.                                            
032600                                                                          
032700     MOVE NEJ     TO MATRIX-Q                                             
032800                                                                          
032900     MOVE SPAR-IDDISTR                 TO TEST-IDDISTR                    
033000     IF (SPAR-FLAUTKRE = JA AND DIST35-REFILL-NA)     OR                  
033100        (SPAR-FLAUTKRE = JA AND DIST35-REFILL-CN)     OR                  
033120        (SPAR-FLAUTKRE = JA AND DIST35-CDC-IN-REFILL) OR                  
033121        (SPAR-FLAUTKRE = JA AND DIST35-CDC-KR-REFILL) OR                  
033122        (SPAR-FLAUTKRE = JA AND DIST35-CDC-TR-REFILL) OR                  
033123        (SPAR-FLAUTKRE = JA AND DIST35-CDC-BR-REFILL) OR                  
033124        (SPAR-FLAUTKRE = JA AND DIST35-CDC-ZA-REFILL) OR                  
033125        (SPAR-FLAUTKRE = JA AND DIST35-CDC-MX-REFILL) OR                  
033126        (SPAR-FLAUTKRE = JA AND DIST35-CDC-AE-REFILL) OR                  
033127        (SPAR-FLAUTKRE = JA AND DIST35-CDC-MY-REFILL) OR                  
033128        (SPAR-FLAUTKRE = JA AND DIST35-CDC-TH-REFILL) OR                  
033129        (SPAR-FLAUTKRE = JA AND DIST35-CDC-TW-REFILL) OR                  
033130        (SPAR-FLAUTKRE = JA AND DIST35-NONVCC-NONVCC-REFILL) OR           
033130        (SPAR-FLAUTKRE = JA AND DIST35-NONVCC-NONVCC-TRANSFER)            
033200        MOVE 'REF '                    TO MID-IDSYSTEM                    
033300        MOVE '3'                       TO MID-KDLEVANM                    
033400     ELSE                                                                 
033500       IF SPAR-IDPTYP = 'B72'                                             
033600         MOVE 'B72'                    TO MID-IDSYSTEM                    
033700         MOVE '3'                      TO MID-KDLEVANM                    
033800       ELSE                                                               
033900         IF SPAR-IDPTYP = 'FAK'                                           
034000           MOVE 'FAK'                  TO MID-IDSYSTEM                    
034100           MOVE '3'                    TO MID-KDLEVANM                    
034200         ELSE                                                             
034300           MOVE 'BAT '                 TO MID-IDSYSTEM                    
034400           MOVE '1'                    TO MID-KDLEVANM                    
034500         END-IF                                                           
034600       END-IF                                                             
034700     END-IF                                                               
034800     MOVE SPAR-IDDISTR                 TO HELP-IDDISTR                    
034900     MOVE HELP-IDDISTR                 TO MID-IDDISTR                     
035000     MOVE SPAR-IDKUNDNR                TO HELP-IDKUNDNR                   
035100     MOVE HELP-IDKUNDNR                TO MID-IDKUNDNR                    
035200     MOVE SPAR-IDRAPPNR                TO MID-IDRAPPNR                    
035300     MOVE 'J'                          TO MID-LEVANM-KLAR                 
035400     MOVE SPAR-TILEVANM                TO HELP-TILEVANM                   
035500     MOVE HELP-TILEVANM                TO MID-TILEVANM                    
035600     MOVE SPACE                        TO MID-PRFRAKT                     
035700     MOVE SPACE                        TO MID-RELANDCO                    
035800     MOVE SPACE                        TO MID-PRLEGKST                    
035900     MOVE SPACE                        TO MID-REEMBHNT                    
036000     MOVE SPAR-KDVALISO                TO MID-KDVALISO                    
036100     .                                                                    
036200                                                                          
036300     EJECT                                                                
036400 BB-FLYTTA-RAD-DATA SECTION.                                              
036500                                                                          
036600     IF SPAR-FLDIRLEV = '0'                                               
036700        MOVE NEJ                       TO MID-FLDIRLEV       (IX)         
036800     ELSE                                                                 
036900        MOVE SPAR-FLDIRLEV             TO MID-FLDIRLEV       (IX)         
037000     END-IF                                                               
037100     MOVE SPAR-FLANLYSF                TO MID-FLANLYSF       (IX)         
037200     MOVE SPAR-FLAUTKRE                TO MID-FLAUTKRE       (IX)         
037300     MOVE SPAR-IDANALYS                TO MID-IDANALYS       (IX)         
037400     MOVE SPAR-IDKONTO                 TO HELP-IDKONTO                    
037500     MOVE HELP-IDKONTO                 TO MID-IDKONTO        (IX)         
037600     MOVE SPAR-IDKST                   TO MID-IDKST          (IX)         
037700     MOVE SPAR-IDARTNR                 TO HELP-IDARTNR                    
037800     MOVE HELP-IDARTNR                 TO MID-IDARTNR        (IX)         
037900     MOVE SPAR-IDDC                    TO MID-IDDC           (IX)         
038000     MOVE SPAR-IDDC-RET                TO MID-IDDC-RET       (IX)         
038100     MOVE SPAR-IDFAKT                  TO HELP-IDFAKT                     
038200     MOVE HELP-IDFAKT                  TO MID-IDFAKT         (IX)         
038300     MOVE SPAR-IDFAKT-LOC              TO HELP-IDFAKT-LOC                 
038400     MOVE HELP-IDFAKT-LOC              TO MID-IDFAKT-LOC     (IX)         
038500     MOVE SPAR-IDFTG                   TO HELP-IDFTG                      
038600     MOVE HELP-IDFTG                   TO MID-IDFTG          (IX)         
038700     MOVE SPAR-IDKOLLI                 TO HELP-IDKOLLI                    
038800     MOVE HELP-IDKOLLI                 TO MID-IDKOLLI        (IX)         
038900     MOVE SPACE                        TO MID-IDKUNDRF       (IX)         
039000     MOVE SPAR-IDORDNR                 TO MID-IDORDNR7       (IX)         
039100     MOVE SPAR-IDLOPNRM                TO HELP-IDLOPNRM                   
039200     MOVE HELP-IDLOPNRM                TO MID-IDLOPNRM       (IX)         
039300     MOVE SPAR-IDRADNR                 TO HELP-IDRADNR                    
039400     MOVE HELP-IDRADNR                 TO MID-IDRADNR        (IX)         
039500     MOVE SPAR-KDANMORS                TO MID-KDANMORS       (IX)         
039600     MOVE SPAR-KDEMBLEV                TO MID-KDEMBLEV       (IX)         
039700     MOVE SPAR-KDFAKTYP                TO MID-KDFAKTYP       (IX)         
039800     MOVE SPAR-KDFRAKT                 TO HELP-KDFRAKT                    
039900     MOVE HELP-KDFRAKT                 TO MID-KDFRAKT        (IX)         
040000     MOVE SPAR-KDKREBEH                TO MID-KDKREBEH       (IX)         
040100     MOVE SPAR-KVLEVANM                TO HELP-KVLEVANM                   
040200     MOVE HELP-KVLEVANM                TO MID-KVLEVANM       (IX)         
040300     MOVE SPAR-PRARTBTO                TO WS-PRARTBTO-NUM                 
040400     MOVE WS-PRARTBTO-ALFA             TO MID-PRARTBTO       (IX)         
040500     MOVE SPAR-PRARTBTO-LOC            TO WS-PRARTBTO-NUM-LOC             
040600     MOVE WS-PRARTBTO-ALFA-LOC         TO MID-PRARTBTO-LOC   (IX)         
040700     MOVE SPAR-PRARTBTO-LOCINV         TO WS-PRARTBTO-NUM-LOCINV          
040800     MOVE WS-PRARTBTO-ALFA-LOCINV      TO MID-PRARTBTO-LOCINV(IX)         
040900     MOVE SPAR-PRFRAKT                 TO WS-PRFRAKT-NUM                  
041000     MOVE WS-PRFRAKT-ALFA              TO MID-PRFRAKT-RAD    (IX)         
041100     MOVE SPAR-TIFAKT                  TO HELP-TIFAKT                     
041200     MOVE HELP-TIFAKT                  TO MID-TIFAKT         (IX)         
041300     MOVE SPAR-TIFAKT-LOC              TO HELP-TIFAKT-LOC                 
041400     MOVE HELP-TIFAKT-LOC              TO MID-TIFAKT-LOC     (IX)         
041500     MOVE SPAR-TILEVANM                TO HELP-TILEVANM                   
041600     MOVE HELP-TILEVANM                TO MID-TILEVANM-RAD   (IX)         
041700     MOVE SPAR-IDUSER-PACK             TO MID-IDUSER-PACK    (IX)         
041800     MOVE SPAR-KDORDKL                 TO HELP-KDORDKL                    
041900     MOVE HELP-KDORDKL                 TO MID-KDORDKL        (IX)         
042000     MOVE SPAR-FLPRQUES                TO MID-FLPRQUES       (IX)         
042100     MOVE SPAR-KDVAT                   TO MID-KDVAT          (IX)         
042200     MOVE SPAR-BEART-VIPS              TO MID-BEART-VIPS     (IX)         
042300     MOVE SPAR-PRARTSTD                TO WS-PRARTSTD-NUM                 
042400     MOVE WS-PRARTSTD-ALFA             TO MID-PRARTSTD       (IX)         
042500     MOVE SPAR-PRARTSJK                TO WS-PRARTSJK-NUM                 
042600     MOVE WS-PRARTSJK-ALFA             TO MID-PRARTSJK       (IX)         
042700                                                                          
042800     IF SPAR-KDKREBEH = 'Q  ' OR 'P  '                                    
042900        MOVE JA      TO MATRIX-Q                                          
043000        IF MID-KDLEVANM = '3'                                             
043100           MOVE '1'                    TO MID-KDLEVANM                    
043200        END-IF                                                            
043300     ELSE                                                                 
043400       IF SPAR-KDANMORS = '96' OR '98'                                    
043500         IF MATRIX-Q = JA                                                 
043600           CONTINUE                                                       
043700         ELSE                                                             
043800           IF MID-KDLEVANM = '1'                                          
043900              MOVE '3'                 TO MID-KDLEVANM                    
044000           END-IF                                                         
044100         END-IF                                                           
044200       END-IF                                                             
044300     END-IF                                                               
044400                                                                          
044500     IF SPAR-TEANMNOT-REG (1) = SPACE                                     
044600      AND SPAR-TEANMNOT-REG (2) = SPACE                                   
044700      AND SPAR-TEANMNOT-REG (3) = SPACE                                   
044800      CONTINUE                                                            
044900     ELSE                                                                 
045000        MOVE HELP-IDDISTR              TO WS-SPAR-IDDISTR  (IX)           
045100        MOVE HELP-IDKUNDNR             TO WS-SPAR-IDKUNDNR (IX)           
045200        MOVE SPAR-IDRAPPNR             TO WS-SPAR-IDRAPPNR (IX)           
045300        MOVE HELP-IDARTNR              TO WS-SPAR-IDARTNR  (IX)           
045400        MOVE HELP-IDRADNR              TO WS-SPAR-IDRADNR  (IX)           
045500                                                                          
045600        MOVE SPAR-TEANMNOT-REG (1)     TO                                 
045700                                      WS-SPAR-TEANMNOT-REG (IX, 1)        
045800        MOVE SPAR-TEANMNOT-REG (2)     TO                                 
045900                                      WS-SPAR-TEANMNOT-REG (IX, 2)        
046000        MOVE SPAR-TEANMNOT-REG (3)     TO                                 
046100                                      WS-SPAR-TEANMNOT-REG (IX, 3)        
046200     END-IF                                                               
046300                                                                          
046400     ADD 1                             TO WS-KVRADER                      
046500     .                                                                    
046600                                                                          
046700     EJECT                                                                
046800 C-BEHANDLA-LEVANM SECTION.                                               
046900                                                                          
047000     PERFORM CA-SKAPA-BUNTHUVUD                                           
047100     PERFORM CB-UPPDAT-DISP-MED-RAD                                       
047200     .                                                                    
047300                                                                          
047400     EJECT                                                                
047500 CA-SKAPA-BUNTHUVUD SECTION.                                              
047600                                                                          
047700     MOVE +54                             TO MSG-KOM-KVLL                 
047800     MOVE LOW-VALUE                       TO MSG-KOM-KDZ1                 
047900     MOVE LOW-VALUE                       TO MSG-KOM-KDZ2                 
048000     MOVE SPACE                           TO MSG-KOM-KDTRANS              
048100     MOVE 'W4I79101'                      TO MSG-KOM-IDCPYTXT             
048200     MOVE 'KREDIT  '                      TO MSG-KOM-IDSNDNOD             
048300     MOVE 'W4182000'                      TO MSG-KOM-IDSNDJOB             
048400     MOVE DAGENS-DATUM                    TO MSG-KOM-TIREGDAT             
048500     MOVE DAGENS-TID                      TO MSG-KOM-TIKLOCK              
048600     MOVE SPACE                           TO MSG-KOM-IDMFSMED             
048700                                             MSG-KOM-KDSVAR               
048800     IF NY-BUNT = JA                                                      
048900       WRITE UT-W01521 FROM MSG-KOM-WMSGKOM                               
049000       MOVE 'W01521'     TO POSTSUM-FDNAMN                                
049100       MOVE 'W41820D3'   TO POSTSUM-DDNAMN2                               
049200       MOVE 'UT'         TO POSTSUM-TRANSTYP                              
049300       CALL POSTSUM USING POSTSUM-PARM                                    
049400       MOVE NEJ TO NY-BUNT                                                
049500     END-IF                                                               
049600     .                                                                    
049700                                                                          
049800     EJECT                                                                
049900                                                                          
050000 CB-UPPDAT-DISP-MED-RAD SECTION.                                          
050100*4791-LL = ((ANTAL-RADER * RAD-LÄNGDEN) + ÖVRIGT DATA) + 17 FÖR           
050200*          P-TO-P-SW                                                      
050300*                                                                         
050400     COMPUTE 4791-LL        = ((MID-KVRADER * 221) + 77) + 17             
050500                                                                          
050600     CALL W006KOM USING MSG-PCB                                           
050700                        DISP-PCB                                          
050800                        KOMA-PCB                                          
050900                        MSG-KOM-WMSGKOM                                   
051000                        4791-MSG-IO-AREA                                  
051100                                                                          
051200     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
051300*       FELAKTIG UPPDATERING AV LEVERANSANM PÅ                            
051400*       KOMMUNIKATIONS DB                                                 
051500        MOVE                                                              
051600        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
051700                                     TO FELTEXT                           
051800        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
051900     END-IF                                                               
052000                                                                          
052100     ADD +1                           TO WS-ANTAL-ISRT                    
052200     .                                                                    
052300                                                                          
052400     EJECT                                                                
052500 D-BEHANDLA-TEXTEN SECTION.                                               
052600                                                                          
052700     MOVE +1                         TO IX                                
052800     PERFORM UNTIL IX > MAX-IX                                            
052900        IF WS-SPAR-IDDISTR (IX) > ZERO                                    
053000           MOVE WS-SPAR-IDDISTR (IX) TO MID-IDDISTR-IN                    
053100           MOVE WS-SPAR-IDKUNDNR(IX) TO MID-IDKUNDNR-IN                   
053200           MOVE WS-SPAR-IDRAPPNR(IX) TO MID-IDRAPPNR-IN                   
053300           MOVE WS-SPAR-IDARTNR (IX) TO MID-IDARTNR-IN                    
053400           MOVE WS-SPAR-IDRADNR (IX) TO MID-IDRADNR-IN                    
053500           MOVE WS-SPAR-TEANMNOT-REG (IX, 1)                              
053600                                     TO MID-TEANMNOT-REG (1)              
053700           MOVE WS-SPAR-TEANMNOT-REG (IX, 2)                              
053800                                     TO MID-TEANMNOT-REG (2)              
053900           MOVE WS-SPAR-TEANMNOT-REG (IX, 3)                              
054000                                     TO MID-TEANMNOT-REG (3)              
054100           PERFORM DA-UPPDAT-DISP-MED-TEXT                                
054200        END-IF                                                            
054300        ADD +1                       TO IX                                
054400     END-PERFORM                                                          
054500     .                                                                    
054600                                                                          
054700     EJECT                                                                
054800 DA-UPPDAT-DISP-MED-TEXT SECTION.                                         
054900                                                                          
055000     CALL W006KOM USING MSG-PCB                                           
055100                        DISP-PCB                                          
055200                        KOMA-PCB                                          
055300                        MSG-KOM-WMSGKOM                                   
055400                        4723-MSG-IO-AREA                                  
055500                                                                          
055600     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
055700*       FELAKTIG UPPDATERING AV LEVERANSANM PÅ                            
055800*       KOMMUNIKATIONS DB                                                 
055900        MOVE                                                              
056000        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
056100                                     TO FELTEXT                           
056200        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
056300     END-IF                                                               
056400                                                                          
056500     ADD +1                           TO WS-ANTAL-ISRT-TXT                
056600     .                                                                    
056700                                                                          
056800     EJECT                                                                
056900 Z-FINIT SECTION.                                                         
057000                                                                          
057100     CLOSE W41810                                                         
057200           W41813                                                         
057300           W01521                                                         
057400                                                                          
057500     MOVE 'S' TO POSTSUM-OPKOD                                            
057600     CALL POSTSUM USING POSTSUM-PARM                                      
057700     .                                                                    
057800                                                                          
057900     EJECT                                                                
058000 S01-LAES-W41810  SECTION.                                                
058100                                                                          
058200     IF END-FIL1 = NEJ                                                    
058300       READ W41810 INTO IN-AREA                                           
058400         AT END                                                           
058500           MOVE JA TO END-FIL1                                            
058600           ADD  10 TO DAGENS-TID                                          
058700           MOVE JA TO NY-BUNT                                             
058800         NOT AT END                                                       
058900           MOVE 'W41810' TO POSTSUM-FDNAMN                                
059000           MOVE 'W41820D1' TO POSTSUM-DDNAMN2                             
059100           MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                             
059200           CALL POSTSUM USING POSTSUM-PARM                                
059300       END-READ                                                           
059400     END-IF                                                               
059500                                                                          
059600     IF END-FIL1 = JA                                                     
059700       READ W41813 INTO IN-AREA                                           
059800         AT END                                                           
059900           SET END-OF-W41810 TO TRUE                                      
060000         NOT AT END                                                       
060100           MOVE 'W41813' TO POSTSUM-FDNAMN                                
060200           MOVE 'W41820D2' TO POSTSUM-DDNAMN2                             
060300           MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                             
060400           CALL POSTSUM USING POSTSUM-PARM                                
060500       END-READ                                                           
060600     END-IF                                                               
060700     .                                                                    
060800                                                                          
060900     EJECT                                                                
061000 X-TAG-CHECKPOINT   SECTION.                                              
061100                                                                          
061200     PERFORM IMS-CHECKPOINT                                               
061300     MOVE ZERO TO CHKP-ANT                                                
061400     .                                                                    
061500                                                                          
061600     EJECT                                                                
061700* --- IMS SEKTIONER ---                                                   
061800                                                                          
061900 IMS-RESTART SECTION.                                                     
062000                                                                          
062100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
062200     MOVE '  ' TO GODK-STATUSKODER                                        
062300     CALL CBLTDLI USING XRST MSG-PCB                                      
062400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
062500                        CHKP-AREA-LENGTH CHKP-AREA                        
062600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062700     PERFORM IMS-STATUSKONTROLL                                           
062800     .                                                                    
062900                                                                          
063000     EJECT                                                                
063100 IMS-CHECKPOINT SECTION.                                                  
063200                                                                          
063300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
063400     MOVE '  XD' TO GODK-STATUSKODER                                      
063500     CALL CBLTDLI USING CHKP MSG-PCB                                      
063600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
063700                        CHKP-AREA-LENGTH CHKP-AREA                        
063800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063900     PERFORM IMS-STATUSKONTROLL                                           
064000                                                                          
064100     IF IMS-EJ-OK                                                         
064200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
064300       DISPLAY FELTEXT                                                    
064400       CALL FELLOG                                                        
064500     END-IF                                                               
064600     .                                                                    
064700                                                                          
064800     EJECT                                                                
064900 IMS-STATUSKONTROLL SECTION.                                              
065000                                                                          
065100     SET STATUS-IX TO 1                                                   
065200     SEARCH GODK-STATUS                                                   
065300       AT END                                                             
065400         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
065500         DISPLAY FELTEXT                                                  
065600         CALL FELLOG                                                      
065700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
065800         CONTINUE                                                         
065900     END-SEARCH                                                           
066000     .                                                                    
