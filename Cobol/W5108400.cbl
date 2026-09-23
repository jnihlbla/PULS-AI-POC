000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5108400.                                                
000300 AUTHOR.         ANDERS HENRIKSSON                                        
000400 DATE-WRITTEN.   20090415.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*      SKICKAR KONVERTERA TRANSAR FRÅN A432GSBD TILL W4325MR              
001000*      LAYOUT TO SEND TO SAP MM                                           
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- LEVA1-TRANSAR                                              
002500     SELECT W51080                     ASSIGN TO W51084D1.                
002600     SKIP2                                                                
002700*                                                                         
002800     SELECT W51084                     ASSIGN TO W51084D2.                
002900     EJECT                                                                
003000*                                                                         
003100     SELECT W51085                     ASSIGN TO W51084D3.                
003200     EJECT                                                                
003300*                                                                         
003400     SELECT W51084A                    ASSIGN TO W51084D4.                
003500     EJECT                                                                
003600*                                                                         
003700     SELECT W51084B                    ASSIGN TO W51084D5.                
003800     EJECT                                                                
003900*                                                                         
004000     SELECT W51084UC                   ASSIGN TO W51084D6.                
004100     EJECT                                                                
004200*                                                                         
004300     SELECT W51084VC                   ASSIGN TO W51084D7.                
004400     EJECT                                                                
004500*                                                                         
004600 DATA DIVISION.                                                           
004700     SKIP3                                                                
004800 FILE SECTION.                                                            
004900     SKIP3                                                                
005000 FD  W51080                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  IN-POST -COPY A432GSDB     -L.                                       
005500     SKIP3                                                                
005600 FD  W51084                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900     SKIP2                                                                
006000*01  UT-POST -COPY A4325MR      -L.                                       
006100     SKIP2                                                                
006200 FD  W51085                                                               
006300     RECORDING       V                                                    
006400     BLOCK CONTAINS  0.                                                   
006500     SKIP2                                                                
006600 01  UT2-POST       PIC X(228).                                           
006700     SKIP2                                                                
006800 FD  W51084A                                                              
006900     RECORDING       F                                                    
007000     BLOCK CONTAINS  0.                                                   
007100     SKIP2                                                                
007200*01  UT3-POST -COPY A4325MR      -L.                                      
007300     SKIP2                                                                
007400 FD  W51084B                                                              
007500     RECORDING       F                                                    
007600     BLOCK CONTAINS  0.                                                   
007700     SKIP2                                                                
007800*01  UT4-POST -COPY A4325MR      -L.                                      
007900     SKIP2                                                                
008000 FD  W51084UC                                                             
008100     RECORDING       F                                                    
008200     BLOCK CONTAINS  0.                                                   
008300     SKIP2                                                                
008400*01  UT5-POST -COPY A4325UC      -L.                                      
008500     SKIP2                                                                
008600 FD  W51084VC                                                             
008700     RECORDING       F                                                    
008800     BLOCK CONTAINS  0.                                                   
008900     SKIP2                                                                
009000*01  UT6-POST -COPY A4325VC      -L.                                      
009100     SKIP2                                                                
009200                                                                          
009300 WORKING-STORAGE SECTION.                                                 
009400                                                                          
009500 77  IDPGM                       PIC X(8)    VALUE 'W5108400'.            
009600 77  JA                          PIC X       VALUE 'J'.                   
009700 77  NEJ                         PIC X       VALUE 'N'.                   
009800                                                                          
009900 77  W51080-EOF-SW               PIC X       VALUE 'N'.                   
010000     88  END-OF-W51080                       VALUE 'J'.                   
010100                                                                          
010200 77  W51085-HEADER-SW            PIC X       VALUE 'N'.                   
010300     88  HEADER-OK                           VALUE 'J'.                   
010400                                                                          
010500 77  W51084UC-HEADER-SW          PIC X       VALUE 'N'.                   
010600     88  HEADER-UC-OK                        VALUE 'J'.                   
010700                                                                          
010800 77  W51084VC-HEADER-SW          PIC X       VALUE 'N'.                   
010900     88  HEADER-VC-OK                        VALUE 'J'.                   
011000                                                                          
011100 01  WS-IN-PRARTBEL-PR            PIC S9(8)V9(3) COMP-3.                  
011200                                                                          
011300 01  KONV-AREOR.                                                          
011400     03  W-TULLKURS-NUM           PIC  9(3)V9(2).                         
011500     03  FILLER  REDEFINES W-TULLKURS-NUM.                                
011600         05  W-TULLKURS-ALF       PIC X(5).                               
011700                                                                          
011800     03  W-TULLFAKT-NUM           PIC  9(1)V9(4).                         
011900     03  FILLER  REDEFINES W-TULLFAKT-NUM.                                
012000         05  W-TULLFAKT-ALF       PIC X(5).                               
012100                                                                          
012200     03  W-BEL-NUM                PIC  9(9)V9(2).                         
012300     03  FILLER  REDEFINES W-BEL-NUM.                                     
012400         05  W-BEL-ALF            PIC X(11).                              
012500                                                                          
012600     03  W-PRIS-NUM               PIC  9(8)V9(3).                         
012700     03  FILLER  REDEFINES W-PRIS-NUM.                                    
012800         05  W-PRIS-ALF            PIC X(11).                             
012900                                                                          
013000     03  W-LEVNR-NUM              PIC 9(5).                               
013100     03  FILLER  REDEFINES W-LEVNR-NUM.                                   
013200         05  W-LEVNR-ALF          PIC X(5).                               
013300                                                                          
013400     03  W-FTAG                   PIC 9(2).                               
013500     03  W-FTAG-ALF REDEFINES W-FTAG PIC X(2).                            
013600     03  W-PRODKOD                PIC 9(4).                               
013700     03  W-PRODKOD-ALF REDEFINES W-PRODKOD  PIC X(4).                     
013800     03  W-SYSTKOD                PIC 9(2).                               
013900     03  W-SYSTKOD-ALF REDEFINES W-SYSTKOD  PIC X(2).                     
014000                                                                          
014100 01  WS-DAP-LINE.                                                         
014200     03  WS-ARTNR                PIC X(10)   VALUE  SPACES.               
014300     03  FIL-1                   PIC X(01)   VALUE  ';'.                  
014400     03  WS-LEVNR                PIC X(06)   VALUE  SPACES.               
014500     03  FIL-2                   PIC X(01)   VALUE  ';'.                  
014600     03  WS-DATUM-AVS            PIC X(08)   VALUE  SPACES.               
014700     03  FIL-3                   PIC X(01)   VALUE  ';'.                  
014800     03  WS-ANTAL                PIC X(07)   VALUE  SPACES.               
014900     03  FIL-4                   PIC X(01)   VALUE  ';'.                  
015000     03  WS-BEL-STD              PIC Z(08)9.9(2) VALUE ZERO.              
015100     03  FIL-5                   PIC X(01)   VALUE  ';'.                  
015200     03  WS-PRIS-BEST            PIC Z(08)9.9(2) VALUE ZERO.              
015300     03  FIL-6                   PIC X(01)   VALUE  ';'.                  
015400     03  WS-URSPPRIS-BEST        PIC Z(07)9.9(2) VALUE ZERO.              
015500     03  FIL-7                   PIC X(01)   VALUE  ';'.                  
015600     03  WS-TIPPAT               PIC X(01)   VALUE  SPACES.               
015700     03  FIL-8                   PIC X(01)   VALUE  ';'.                  
015800     03  WS-VAL                  PIC X(03)   VALUE  SPACES.               
015900     03  FIL-9                   PIC X(01)   VALUE  ';'.                  
016000     03  WS-MRNR                 PIC X(09)   VALUE  SPACES.               
016100     03  FIL-10                  PIC X(01)   VALUE  ';'.                  
016200     03  WS-PACKNR               PIC X(06)   VALUE  SPACES.               
016300     03  FIL-11                  PIC X(01)   VALUE  ';'.                  
016400     03  WS-FTG                  PIC X(02)   VALUE  SPACES.               
016500     03  FIL-12                  PIC X(01)   VALUE  ';'.                  
016600                                                                          
016700 01  TEXT-AREA.                                                           
016800     03  HEAD-LINE               PIC X(650).                              
016900                                                                          
017000 01  DYNAMISKA-SUBPROGRAM.                                                
017100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
017200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
017300     SKIP2                                                                
017400                                                                          
017500*    --- PARAMETRAR TILL ABEND                                            
017600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
017700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
017800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
017900     SKIP2                                                                
018000                                                                          
018100*    --- PARAMETRAR TILL POSTSUM                                          
018200*01  -COPY W0005   -PRE  POSTSUM-                                         
018300     EJECT                                                                
018400                                                                          
018500 01  IN-AREA-START               PIC X(24)   VALUE                        
018600                                 'IN-AREA-START  '.                       
018700     SKIP2                                                                
018800*01  AREA -COPY A432GSDB    -PRE IN-                                      
018900     EJECT                                                                
019000                                                                          
019100 01  UT-AREA-START               PIC X(24)   VALUE                        
019200                                 'UT-AREA-START  '.                       
019300*01  AREA -COPY A4325MR     -PRE UT-                                      
019400     EJECT                                                                
019500                                                                          
019600 01  UT2-AREA-START              PIC X(24)   VALUE                        
019700                                 'UT2-AREA-START  '.                      
019800 01  UT2-AREA                    PIC X(228).                              
019900     EJECT                                                                
020000                                                                          
020100 01  UT3-AREA-START              PIC X(24)   VALUE                        
020200                                 'UT3-AREA-START  '.                      
020300*01  AREA -COPY A4325MR     -PRE UT3-                                     
020400     EJECT                                                                
020500                                                                          
020600 01  UT4-AREA-START              PIC X(24)   VALUE                        
020700                                 'UT4-AREA-START  '.                      
020800*01  AREA -COPY A4325MR     -PRE UT4-                                     
020900     EJECT                                                                
021000                                                                          
021100 01  UT5-AREA-START              PIC X(24)   VALUE                        
021200                                 'UT5-AREA-START  '.                      
021300*01  AREA -COPY A4325UC     -PRE UT5-                                     
021400     EJECT                                                                
021500                                                                          
021600 01  UT6-AREA-START              PIC X(24)   VALUE                        
021700                                 'UT6-AREA-START  '.                      
021800*01  AREA -COPY A4325VC     -PRE UT6-                                     
021900     EJECT                                                                
022000                                                                          
022100                                                                          
022200 PROCEDURE DIVISION.                                                      
022300 MAIN SECTION.                                                            
022400     SKIP2                                                                
022500                                                                          
022600     PERFORM A-INIT                                                       
022700                                                                          
022800     PERFORM B-READ                                                       
022900                                                                          
023000     PERFORM Z-FINIT                                                      
023100                                                                          
023200     MOVE ZERO TO RETURN-CODE                                             
023300     GOBACK                                                               
023400                                                                          
023500     .                                                                    
023600     EJECT                                                                
023700                                                                          
023800 A-INIT SECTION.                                                          
023900     OPEN INPUT  W51080                                                   
024000          OUTPUT W51084                                                   
024100          OUTPUT W51085                                                   
024200          OUTPUT W51084A                                                  
024300          OUTPUT W51084B                                                  
024400          OUTPUT W51084UC                                                 
024500          OUTPUT W51084VC                                                 
024600     .                                                                    
024700     EJECT                                                                
024800                                                                          
024900 B-READ        SECTION.                                                   
025000     PERFORM S01-LAES-W51080                                              
025100     PERFORM UNTIL END-OF-W51080                                          
025200       IF IN-FTAG = 53                                                    
025300         PERFORM C-KONVERTERA                                             
025400       END-IF                                                             
025500       IF IN-FTAG = 57                                                    
025600         PERFORM D-KONVERTERA                                             
025700       END-IF                                                             
025800       IF IN-FTAG = 60                                                    
025900         PERFORM E-KONVERTERA                                             
026000       END-IF                                                             
026100       PERFORM S01-LAES-W51080                                            
026200     END-PERFORM                                                          
026300     .                                                                    
026400     EJECT                                                                
026500                                                                          
026600 C-KONVERTERA         SECTION.                                            
026700                                                                          
026800     IF W51084UC-HEADER-SW        = NEJ                                   
026900        PERFORM UC-DAP-HEADER-LINE                                        
027000     END-IF                                                               
027100                                                                          
027200     MOVE '501'                 TO UT3-PTYP                               
027300                                   UT5-IDPTYP                             
027400     MOVE IN-MRNR               TO UT3-MRNR                               
027500                                   WS-MRNR                                
027600                                   UT5-IDLOPNRM                           
027700     MOVE IN-ARTNR              TO UT3-ARTNR                              
027800                                   WS-ARTNR                               
027900                                   UT5-IDARTNR                            
028000     MOVE IN-GSDB               TO UT3-LEVNR                              
028100                                   WS-LEVNR                               
028200                                   UT5-IDLEVNR                            
028300     MOVE IN-BESTPREF           TO UT3-BESTPREF                           
028400                                   UT5-BESTPREF                           
028500     MOVE IN-BESTLOPNR          TO UT3-BESTLOPNR                          
028600                                   UT5-BESTLOPNR                          
028700     MOVE IN-BESTSUFF           TO UT3-BESTSUFF                           
028800                                   UT5-BESTSUFF                           
028900     MOVE IN-PACKNR             TO UT3-PACKNR                             
029000                                   WS-PACKNR                              
029100                                   UT5-PACKNR                             
029200     MOVE IN-DATUM-AVS          TO UT3-DATUM-AVS                          
029300                                   WS-DATUM-AVS                           
029400                                   UT5-DATUM-AVS                          
029500     MOVE IN-ANTAL              TO UT3-ANTAL                              
029600                                   WS-ANTAL                               
029700                                   UT5-KVAVIS                             
029800     MOVE IN-SORT1              TO UT3-SORT1                              
029900                                   UT5-KDSORT                             
030000     MOVE IN-KDVALISO           TO UT3-VAL                                
030100                                   WS-VAL                                 
030200                                   UT5-KDVALISO                           
030300                                                                          
030400     MOVE IN-PRIS-BEST          TO W-BEL-NUM                              
030500                                   UT5-PRARTBES                           
030600     MOVE W-BEL-ALF             TO UT3-PRIS-BEST-X                        
030700     MOVE W-BEL-NUM             TO WS-PRIS-BEST                           
030800                                                                          
030900     MOVE IN-URSPPRIS-BEST    TO WS-IN-PRARTBEL-PR                        
031000     IF UT3-VAL = 'GBP'                                                   
031100       IF WS-IN-PRARTBEL-PR > ZERO                                        
031200         COMPUTE WS-IN-PRARTBEL-PR = IN-URSPPRIS-BEST / 1000              
031300       ELSE                                                               
031400         MOVE ZERO TO WS-IN-PRARTBEL-PR                                   
031500       END-IF                                                             
031600     ELSE                                                                 
031700       COMPUTE WS-IN-PRARTBEL-PR  = IN-URSPPRIS-BEST / 100                
031800     END-IF                                                               
031900                                                                          
032000     MOVE WS-IN-PRARTBEL-PR     TO W-PRIS-NUM                             
032100                                   UT5-PRARTBEL-PR                        
032200     MOVE W-PRIS-ALF            TO UT3-URSPPRIS-BEST-X                    
032300     COMPUTE WS-URSPPRIS-BEST ROUNDED = W-PRIS-NUM * 1                    
032400                                                                          
032500     MOVE IN-ENHET-PRIS         TO UT3-ENHET-PRIS                         
032600                                   UT5-KDANTENH                           
032700     MOVE IN-TIPPAT             TO UT3-TIPPAT                             
032800                                   WS-TIPPAT                              
032900                                   UT5-TIPPAT                             
033000                                                                          
033100     IF IN-BEL-STD < ZERO                                                 
033200       MOVE '-'                 TO UT3-KREDIT                             
033300                                   UT5-IDTECKEN                           
033400     ELSE                                                                 
033500       MOVE '+'                 TO UT3-KREDIT                             
033600                                   UT5-IDTECKEN                           
033700     END-IF                                                               
033800                                                                          
033900     IF IN-BEL-STD < ZERO                                                 
034000        COMPUTE W-BEL-NUM = 0 - IN-BEL-STD                                
034100     ELSE                                                                 
034200        MOVE IN-BEL-STD         TO W-BEL-NUM                              
034300     END-IF                                                               
034400     MOVE W-BEL-ALF             TO UT3-BEL-STD-X                          
034500     MOVE W-BEL-NUM             TO WS-BEL-STD                             
034600     MOVE IN-BEL-STD            TO UT5-PRARTSTD                           
034700                                                                          
034800     IF IN-BEL-BEST < ZERO                                                
034900        COMPUTE W-BEL-NUM = 0 - IN-BEL-BEST                               
035000     ELSE                                                                 
035100        MOVE IN-BEL-BEST        TO W-BEL-NUM                              
035200     END-IF                                                               
035300     MOVE IN-BEL-BEST           TO UT5-BEL-BEST                           
035400     MOVE W-BEL-ALF             TO UT3-BEL-BEST-X                         
035500                                                                          
035600     MOVE IN-HKTO               TO UT3-HKTO                               
035700     MOVE IN-UKTO               TO UT3-UKTO                               
035800                                                                          
035900     STRING UT3-HKTO      DELIMITED BY SIZE                               
036000            UT3-UKTO(1:2) DELIMITED BY SIZE                               
036100            INTO UT5-HKTO                                                 
036200     END-STRING                                                           
036300                                                                          
036400     MOVE IN-UKTO(3:2)          TO UT5-UKTO                               
036500     MOVE IN-FTAG               TO W-FTAG                                 
036600     MOVE W-FTAG-ALF            TO UT3-FTAG                               
036700                                   WS-FTG                                 
036800                                   UT5-IDFTG                              
036900                                                                          
037000     MOVE IN-PRODKOD            TO W-PRODKOD                              
037100                                   UT5-PRODKOD                            
037200     MOVE W-PRODKOD-ALF         TO UT3-PRODKOD                            
037300     MOVE IN-SYSTKOD            TO W-SYSTKOD                              
037400     MOVE W-SYSTKOD-ALF         TO UT3-SYSTKOD                            
037500                                   UT5-SYSTKOD                            
037600                                                                          
037700     MOVE IN-TULLKURS           TO W-TULLKURS-NUM                         
037800                                   UT5-TULLKURS                           
037900     MOVE W-TULLKURS-ALF        TO UT3-TULLKURS-X                         
038000                                                                          
038100     MOVE IN-TULLFAKT           TO W-TULLFAKT-NUM                         
038200     MOVE W-TULLFAKT-ALF        TO UT3-TULLFAKT-X                         
038300     MOVE W-TULLFAKT-NUM        TO UT5-TULLFAKT                           
038400                                                                          
038500     MOVE IN-RATTKOD            TO UT3-RATTKOD                            
038600                                   UT5-KDINLAVV                           
038700     MOVE IN-PRCTR              TO UT3-PRCTR                              
038800                                   UT5-IDPRCTR                            
038900     MOVE IN-COSTCTR-FREIGHT    TO UT3-COSTCTR-FREIGHT                    
039000                                   UT5-COSTCTR-FREIGHT                    
039100     MOVE IN-FLAGGA1F           TO UT3-FLAGGA1F                           
039200                                   UT5-FLAGGA1F                           
039300     MOVE IN-COSTCTR-PRICE-DIFF TO UT3-COSTCTR-PRICE-DIFF                 
039400                                   UT5-COSTCTR-PRICE-DIFF                 
039500     MOVE IN-FLAGGA2D           TO UT3-FLAGGA2D                           
039600                                   UT5-FLAGGA2D                           
039700     MOVE IN-COSTCTR-EXCH-DIFF  TO UT3-COSTCTR-EXCH-DIFF                  
039800                                   UT5-COSTCTR-EXCH-DIFF                  
039900     MOVE IN-FLAGGA3VD          TO UT3-FLAGGA3VD                          
040000                                   UT5-FLAGGA3VD                          
040100     MOVE IN-GODSMOT            TO UT3-GODSMOT                            
040200                                   UT5-IDDISTR                            
040300     MOVE IN-LOPNRSEKVFROM1     TO UT3-LOPNRFR1                           
040400                                   UT5-LOPNRFR1                           
040500     MOVE IN-LOPNRSEKVTOM1      TO UT3-LOPNRTM1                           
040600                                   UT5-LOPNRTM1                           
040700     MOVE IN-LOPNRSEKVFROM2     TO UT3-LOPNRFR2                           
040800                                   UT5-LOPNRFR2                           
040900     MOVE IN-LOPNRSEKVTOM2      TO UT3-LOPNRTM2                           
041000                                   UT5-LOPNRTM2                           
041100     MOVE IN-AVVIKELSE          TO UT3-AVVIKELSE                          
041200                                   UT5-FLAVVINL                           
041300     MOVE IN-PACKNRTOT          TO UT3-PACKNRTOT                          
041400                                   UT5-PACKNRTOT                          
041500                                                                          
041501**** INTERNAL SUPPLIERS IN US THAT SHOULD NOT BE SENT TO SAP MM           
041510     IF IN-GSDB = 'CHA03'                                                 
041520     OR IN-GSDB = '1496A'                                                 
041530       CONTINUE                                                           
041540     ELSE                                                                 
041600       PERFORM S04-SKRIV-LISTA                                            
041700       PERFORM S06-SKRIV-LISTA                                            
041800                                                                          
041900       IF W51085-HEADER-SW      = NEJ                                     
042000          PERFORM CA-DAP-HEADER-LINE                                      
042100          PERFORM CB-DAP-DETAIL-LINE                                      
042200       ELSE                                                               
042300          PERFORM CB-DAP-DETAIL-LINE                                      
042400       END-IF                                                             
042410     END-IF                                                               
042500     .                                                                    
042600     EJECT                                                                
042700                                                                          
042800 D-KONVERTERA         SECTION.                                            
042900                                                                          
043000     IF W51084VC-HEADER-SW        = NEJ                                   
043100        PERFORM VC-DAP-HEADER-LINE                                        
043200     END-IF                                                               
043300                                                                          
043400     MOVE '501'                 TO UT-PTYP                                
043500                                   UT6-IDPTYP                             
043600     MOVE IN-MRNR               TO UT-MRNR                                
043700                                   WS-MRNR                                
043800                                   UT6-IDLOPNRM                           
043900     MOVE IN-ARTNR              TO UT-ARTNR                               
044000                                   WS-ARTNR                               
044100                                   UT6-IDARTNR                            
044200     MOVE IN-GSDB               TO UT-LEVNR                               
044300                                   WS-LEVNR                               
044400                                   UT6-IDLEVNR                            
044500     MOVE IN-BESTPREF           TO UT-BESTPREF                            
044600                                   UT6-BESTPREF                           
044700     MOVE IN-BESTLOPNR          TO UT-BESTLOPNR                           
044800                                   UT6-BESTLOPNR                          
044900     MOVE IN-BESTSUFF           TO UT-BESTSUFF                            
045000                                   UT6-BESTSUFF                           
045100     MOVE IN-PACKNR             TO UT-PACKNR                              
045200                                   WS-PACKNR                              
045300                                   UT6-PACKNR                             
045400     MOVE IN-DATUM-AVS          TO UT-DATUM-AVS                           
045500                                   WS-DATUM-AVS                           
045600                                   UT6-DATUM-AVS                          
045700     MOVE IN-ANTAL              TO UT-ANTAL                               
045800                                   WS-ANTAL                               
045900                                   UT6-KVAVIS                             
046000     MOVE IN-SORT1              TO UT-SORT1                               
046100                                   UT6-KDSORT                             
046200     MOVE IN-KDVALISO           TO UT-VAL                                 
046300                                   WS-VAL                                 
046400                                   UT6-KDVALISO                           
046500                                                                          
046600     MOVE IN-PRIS-BEST          TO W-BEL-NUM                              
046700                                   UT6-PRARTBES                           
046800     MOVE W-BEL-ALF             TO UT-PRIS-BEST-X                         
046900     MOVE W-BEL-NUM             TO WS-PRIS-BEST                           
047000                                                                          
047100     MOVE IN-URSPPRIS-BEST    TO WS-IN-PRARTBEL-PR                        
047200     IF UT-VAL = 'GBP'                                                    
047300       IF WS-IN-PRARTBEL-PR > ZERO                                        
047400         COMPUTE WS-IN-PRARTBEL-PR = IN-URSPPRIS-BEST / 1000              
047500       ELSE                                                               
047600         MOVE ZERO TO WS-IN-PRARTBEL-PR                                   
047700       END-IF                                                             
047800     ELSE                                                                 
047900       COMPUTE WS-IN-PRARTBEL-PR  = IN-URSPPRIS-BEST / 100                
048000     END-IF                                                               
048100                                                                          
048200     MOVE WS-IN-PRARTBEL-PR     TO W-PRIS-NUM                             
048300                                   UT6-PRARTBEL-PR                        
048400     MOVE W-PRIS-ALF            TO UT-URSPPRIS-BEST-X                     
048500     COMPUTE WS-URSPPRIS-BEST ROUNDED = W-PRIS-NUM * 1                    
048600                                                                          
048700     MOVE IN-ENHET-PRIS         TO UT-ENHET-PRIS                          
048800                                   UT6-KDANTENH                           
048900     MOVE IN-TIPPAT             TO UT-TIPPAT                              
049000                                   WS-TIPPAT                              
049100                                   UT6-TIPPAT                             
049200                                                                          
049300     IF IN-BEL-STD < ZERO                                                 
049400       MOVE '-'                 TO UT-KREDIT                              
049500                                   UT6-IDTECKEN                           
049600     ELSE                                                                 
049700       MOVE '+'                 TO UT-KREDIT                              
049800                                   UT6-IDTECKEN                           
049900     END-IF                                                               
050000                                                                          
050100     IF IN-BEL-STD < ZERO                                                 
050200        COMPUTE W-BEL-NUM = 0 - IN-BEL-STD                                
050300     ELSE                                                                 
050400        MOVE IN-BEL-STD         TO W-BEL-NUM                              
050500     END-IF                                                               
050600     MOVE W-BEL-ALF             TO UT-BEL-STD-X                           
050700     MOVE W-BEL-NUM             TO WS-BEL-STD                             
050800     MOVE IN-BEL-STD            TO UT6-PRARTSTD                           
050900                                                                          
051000     IF IN-BEL-BEST < ZERO                                                
051100        COMPUTE W-BEL-NUM = 0 - IN-BEL-BEST                               
051200     ELSE                                                                 
051300        MOVE IN-BEL-BEST        TO W-BEL-NUM                              
051400     END-IF                                                               
051500     MOVE IN-BEL-BEST           TO UT6-BEL-BEST                           
051600     MOVE W-BEL-ALF             TO UT-BEL-BEST-X                          
051700                                                                          
051800     MOVE IN-HKTO               TO UT-HKTO                                
051900                                   UT6-HKTO                               
052000     MOVE IN-UKTO               TO UT-UKTO                                
052100     MOVE IN-UKTO(1:2)          TO UT6-UKTO-DD                            
052200     MOVE IN-UKTO(3:2)          TO UT6-UKTO-DC                            
052300                                                                          
052400     MOVE IN-FTAG               TO W-FTAG                                 
052500     MOVE W-FTAG-ALF            TO UT-FTAG                                
052600                                   WS-FTG                                 
052700                                   UT6-IDFTG                              
052800                                                                          
052900     MOVE IN-PRODKOD            TO W-PRODKOD                              
053000                                   UT6-PRODKOD                            
053100     MOVE W-PRODKOD-ALF         TO UT-PRODKOD                             
053200     MOVE IN-SYSTKOD            TO W-SYSTKOD                              
053300     MOVE W-SYSTKOD-ALF         TO UT-SYSTKOD                             
053400                                   UT6-SYSTKOD                            
053500                                                                          
053600     MOVE IN-TULLKURS           TO W-TULLKURS-NUM                         
053700     MOVE W-TULLKURS-NUM        TO UT6-TULLKURS                           
053800     MOVE W-TULLKURS-ALF        TO UT-TULLKURS-X                          
053900                                                                          
054000     MOVE IN-TULLFAKT           TO W-TULLFAKT-NUM                         
054100     MOVE W-TULLFAKT-ALF        TO UT-TULLFAKT-X                          
054200     MOVE W-TULLFAKT-NUM        TO UT6-TULLFAKT                           
054300                                                                          
054400     MOVE IN-RATTKOD            TO UT-RATTKOD                             
054500                                   UT6-KDINLAVV                           
054600     MOVE IN-PRCTR              TO UT-PRCTR                               
054700                                   UT6-IDPRCTR                            
054800     MOVE IN-COSTCTR-FREIGHT    TO UT-COSTCTR-FREIGHT                     
054900                                   UT6-COSTCTR-FREIGHT                    
055000     MOVE IN-FLAGGA1F           TO UT-FLAGGA1F                            
055100                                   UT6-FLAGGA1F                           
055200     MOVE IN-COSTCTR-PRICE-DIFF TO UT-COSTCTR-PRICE-DIFF                  
055300                                   UT6-COSTCTR-PRICE-DIFF                 
055400     MOVE IN-FLAGGA2D           TO UT-FLAGGA2D                            
055500                                   UT6-FLAGGA2D                           
055600     MOVE IN-COSTCTR-EXCH-DIFF  TO UT-COSTCTR-EXCH-DIFF                   
055700                                   UT6-COSTCTR-EXCH-DIFF                  
055800     MOVE IN-FLAGGA3VD          TO UT-FLAGGA3VD                           
055900                                   UT6-FLAGGA3VD                          
056000     MOVE IN-GODSMOT            TO UT-GODSMOT                             
056100                                   UT6-IDDISTR                            
056200     MOVE IN-LOPNRSEKVFROM1     TO UT-LOPNRFR1                            
056300                                   UT6-LOPNRFR1                           
056400     MOVE IN-LOPNRSEKVTOM1      TO UT-LOPNRTM1                            
056500                                   UT6-LOPNRTM1                           
056600     MOVE IN-LOPNRSEKVFROM2     TO UT-LOPNRFR2                            
056700                                   UT6-LOPNRFR2                           
056800     MOVE IN-LOPNRSEKVTOM2      TO UT-LOPNRTM2                            
056900                                   UT6-LOPNRTM2                           
057000     MOVE IN-AVVIKELSE          TO UT-AVVIKELSE                           
057100                                   UT6-FLAVVINL                           
057200     MOVE IN-PACKNRTOT          TO UT-PACKNRTOT                           
057300                                   UT6-PACKNRTOT                          
057400                                                                          
057500     PERFORM S02-SKRIV-LISTA                                              
057600     PERFORM S07-SKRIV-LISTA                                              
057700                                                                          
057800     IF W51085-HEADER-SW        = NEJ                                     
057900        PERFORM CA-DAP-HEADER-LINE                                        
058000        PERFORM CB-DAP-DETAIL-LINE                                        
058100     ELSE                                                                 
058200        PERFORM CB-DAP-DETAIL-LINE                                        
058300     END-IF                                                               
058400     .                                                                    
058500     EJECT                                                                
058600                                                                          
058700 E-KONVERTERA         SECTION.                                            
058800                                                                          
058900     IF W51084UC-HEADER-SW        = NEJ                                   
059000        PERFORM UC-DAP-HEADER-LINE                                        
059100     END-IF                                                               
059200                                                                          
059300     MOVE '501'                 TO UT4-PTYP                               
059400                                   UT5-IDPTYP                             
059500     MOVE IN-MRNR               TO UT4-MRNR                               
059600                                   WS-MRNR                                
059700                                   UT5-IDLOPNRM                           
059800     MOVE IN-ARTNR              TO UT4-ARTNR                              
059900                                   WS-ARTNR                               
060000                                   UT5-IDARTNR                            
060100     MOVE IN-GSDB               TO UT4-LEVNR                              
060200                                   WS-LEVNR                               
060300                                   UT5-IDLEVNR                            
060400     MOVE IN-BESTPREF           TO UT4-BESTPREF                           
060500                                   UT5-BESTPREF                           
060600     MOVE IN-BESTLOPNR          TO UT4-BESTLOPNR                          
060700                                   UT5-BESTLOPNR                          
060800     MOVE IN-BESTSUFF           TO UT4-BESTSUFF                           
060900                                   UT5-BESTSUFF                           
061000     MOVE IN-PACKNR             TO UT4-PACKNR                             
061100                                   WS-PACKNR                              
061200                                   UT5-PACKNR                             
061300     MOVE IN-DATUM-AVS          TO UT4-DATUM-AVS                          
061400                                   WS-DATUM-AVS                           
061500                                   UT5-DATUM-AVS                          
061600     MOVE IN-ANTAL              TO UT4-ANTAL                              
061700                                   WS-ANTAL                               
061800                                   UT5-KVAVIS                             
061900     MOVE IN-SORT1              TO UT4-SORT1                              
062000                                   UT5-KDSORT                             
062100     MOVE IN-KDVALISO           TO UT4-VAL                                
062200                                   WS-VAL                                 
062300                                   UT5-KDVALISO                           
062400                                                                          
062500     MOVE IN-PRIS-BEST          TO W-BEL-NUM                              
062600                                   UT5-PRARTBES                           
062700     MOVE W-BEL-ALF             TO UT4-PRIS-BEST-X                        
062800     MOVE W-BEL-NUM             TO WS-PRIS-BEST                           
062900                                                                          
063000     MOVE IN-URSPPRIS-BEST    TO WS-IN-PRARTBEL-PR                        
063100     IF UT4-VAL = 'GBP'                                                   
063200       IF WS-IN-PRARTBEL-PR > ZERO                                        
063300         COMPUTE WS-IN-PRARTBEL-PR = IN-URSPPRIS-BEST / 1000              
063400       ELSE                                                               
063500         MOVE ZERO TO WS-IN-PRARTBEL-PR                                   
063600       END-IF                                                             
063700     ELSE                                                                 
063800       COMPUTE WS-IN-PRARTBEL-PR  = IN-URSPPRIS-BEST / 100                
063900     END-IF                                                               
064000                                                                          
064100     MOVE WS-IN-PRARTBEL-PR     TO W-PRIS-NUM                             
064200                                   UT5-PRARTBEL-PR                        
064300     MOVE W-PRIS-ALF            TO UT4-URSPPRIS-BEST-X                    
064400     COMPUTE WS-URSPPRIS-BEST ROUNDED = W-PRIS-NUM * 1                    
064500                                                                          
064600     MOVE IN-ENHET-PRIS         TO UT4-ENHET-PRIS                         
064700                                   UT5-KDANTENH                           
064800     MOVE IN-TIPPAT             TO UT4-TIPPAT                             
064900                                   WS-TIPPAT                              
065000                                   UT5-TIPPAT                             
065100                                                                          
065200     IF IN-BEL-STD < ZERO                                                 
065300       MOVE '-'                 TO UT4-KREDIT                             
065400                                   UT5-IDTECKEN                           
065500     ELSE                                                                 
065600       MOVE '+'                 TO UT4-KREDIT                             
065700                                   UT5-IDTECKEN                           
065800     END-IF                                                               
065900                                                                          
066000     IF IN-BEL-STD < ZERO                                                 
066100        COMPUTE W-BEL-NUM = 0 - IN-BEL-STD                                
066200     ELSE                                                                 
066300        MOVE IN-BEL-STD         TO W-BEL-NUM                              
066400     END-IF                                                               
066500     MOVE W-BEL-ALF             TO UT4-BEL-STD-X                          
066600     MOVE W-BEL-NUM             TO WS-BEL-STD                             
066700     MOVE IN-BEL-STD            TO UT5-PRARTSTD                           
066800                                                                          
066900     IF IN-BEL-BEST < ZERO                                                
067000        COMPUTE W-BEL-NUM = 0 - IN-BEL-BEST                               
067100     ELSE                                                                 
067200        MOVE IN-BEL-BEST        TO W-BEL-NUM                              
067300     END-IF                                                               
067400     MOVE IN-BEL-BEST           TO UT5-BEL-BEST                           
067500     MOVE W-BEL-ALF             TO UT4-BEL-BEST-X                         
067600                                                                          
067700     MOVE IN-HKTO               TO UT4-HKTO                               
067800     MOVE IN-UKTO               TO UT4-UKTO                               
067900     STRING UT4-HKTO      DELIMITED BY SIZE                               
068000            UT4-UKTO(1:2) DELIMITED BY SIZE                               
068100            INTO UT5-HKTO                                                 
068200     END-STRING                                                           
068300                                                                          
068400     MOVE IN-UKTO(3:2)          TO UT5-UKTO                               
068500     MOVE IN-FTAG               TO W-FTAG                                 
068600     MOVE W-FTAG-ALF            TO UT4-FTAG                               
068700                                   WS-FTG                                 
068800                                   UT5-IDFTG                              
068900                                                                          
069000     MOVE IN-PRODKOD            TO W-PRODKOD                              
069100                                   UT5-PRODKOD                            
069200     MOVE W-PRODKOD-ALF         TO UT4-PRODKOD                            
069300     MOVE IN-SYSTKOD            TO W-SYSTKOD                              
069400     MOVE W-SYSTKOD-ALF         TO UT4-SYSTKOD                            
069500                                   UT5-SYSTKOD                            
069600                                                                          
069700     MOVE IN-TULLKURS           TO W-TULLKURS-NUM                         
069800                                   UT5-TULLKURS                           
069900     MOVE W-TULLKURS-ALF        TO UT4-TULLKURS-X                         
070000                                                                          
070100     MOVE IN-TULLFAKT           TO W-TULLFAKT-NUM                         
070200     MOVE W-TULLFAKT-ALF        TO UT4-TULLFAKT-X                         
070300     MOVE W-TULLFAKT-NUM        TO UT5-TULLFAKT                           
070400                                                                          
070500     MOVE IN-RATTKOD            TO UT4-RATTKOD                            
070600                                   UT5-KDINLAVV                           
070700     MOVE IN-PRCTR              TO UT4-PRCTR                              
070800                                   UT5-IDPRCTR                            
070900     MOVE IN-COSTCTR-FREIGHT    TO UT4-COSTCTR-FREIGHT                    
071000                                   UT5-COSTCTR-FREIGHT                    
071100     MOVE IN-FLAGGA1F           TO UT4-FLAGGA1F                           
071200                                   UT5-FLAGGA1F                           
071300     MOVE IN-COSTCTR-PRICE-DIFF TO UT4-COSTCTR-PRICE-DIFF                 
071400                                   UT5-COSTCTR-PRICE-DIFF                 
071500     MOVE IN-FLAGGA2D           TO UT4-FLAGGA2D                           
071600                                   UT5-FLAGGA2D                           
071700     MOVE IN-COSTCTR-EXCH-DIFF  TO UT4-COSTCTR-EXCH-DIFF                  
071800                                   UT5-COSTCTR-EXCH-DIFF                  
071900     MOVE IN-FLAGGA3VD          TO UT4-FLAGGA3VD                          
072000                                   UT5-FLAGGA3VD                          
072100     MOVE IN-GODSMOT            TO UT4-GODSMOT                            
072200                                   UT5-IDDISTR                            
072300     MOVE IN-LOPNRSEKVFROM1     TO UT4-LOPNRFR1                           
072400                                   UT5-LOPNRFR1                           
072500     MOVE IN-LOPNRSEKVTOM1      TO UT4-LOPNRTM1                           
072600                                   UT5-LOPNRTM1                           
072700     MOVE IN-LOPNRSEKVFROM2     TO UT4-LOPNRFR2                           
072800                                   UT5-LOPNRFR2                           
072900     MOVE IN-LOPNRSEKVTOM2      TO UT4-LOPNRTM2                           
073000                                   UT5-LOPNRTM2                           
073100     MOVE IN-AVVIKELSE          TO UT4-AVVIKELSE                          
073200                                   UT5-FLAVVINL                           
073300     MOVE IN-PACKNRTOT          TO UT4-PACKNRTOT                          
073400                                   UT5-PACKNRTOT                          
073500                                                                          
073600     PERFORM S05-SKRIV-LISTA                                              
073700     PERFORM S06-SKRIV-LISTA                                              
073800                                                                          
073900     IF W51085-HEADER-SW        = NEJ                                     
074000        PERFORM CA-DAP-HEADER-LINE                                        
074100        PERFORM CB-DAP-DETAIL-LINE                                        
074200     ELSE                                                                 
074300        PERFORM CB-DAP-DETAIL-LINE                                        
074400     END-IF                                                               
074500     .                                                                    
074600     EJECT                                                                
074700                                                                          
074800 CA-DAP-HEADER-LINE   SECTION.                                            
074900     MOVE SPACE TO UT2-AREA                                               
075000     SET HEADER-OK              TO TRUE                                   
075100                                                                          
075200     STRING 'PART NO           ' ';'                                      
075300       'SUPPLIER          '      ';'                                      
075400       'DATE              '      ';'                                      
075500       'QUANTITY          '      ';'                                      
075600       'SUM STANDARD PRICE'      ';'                                      
075700       'BEST PRICE        '      ';'                                      
075800       'BEST PRICE LOCAL  '      ';'                                      
075900       'TIPPED PRICE      '      ';'                                      
076000       'CURRENCY          '      ';'                                      
076100       'MRNR              '      ';'                                      
076200       'PACKING NO        '      ';'                                      
076300       'COMPANY CODE      '      ';'                                      
076400     DELIMITED BY SIZE INTO UT2-AREA                                      
076500*                                                                         
076600     PERFORM S03-SKRIV-DAP-LISTA                                          
076700     .                                                                    
076800     EJECT                                                                
076900                                                                          
077000 CB-DAP-DETAIL-LINE   SECTION.                                            
077100     MOVE WS-DAP-LINE           TO UT2-AREA                               
077200     PERFORM S03-SKRIV-DAP-LISTA                                          
077300*                                                                         
077400     MOVE SPACE                 TO UT2-AREA                               
077500     .                                                                    
077600     EJECT                                                                
077700                                                                          
077800 UC-DAP-HEADER-LINE   SECTION.                                            
077900     INITIALIZE    TEXT-AREA                                              
078000     SET HEADER-UC-OK           TO TRUE                                   
078100                                                                          
078200     STRING 'RECORD_TYPE'          ';'                                    
078300       'MR_NUMBER'                 ';'                                    
078400       'PART_NUMBER'               ';'                                    
078500       'SUPPLIER_NUMBER'           ';'                                    
078600       'SERIAL_NUMBER_PREFIX'      ';'                                    
078700       'SERIAL_NUMBER'             ';'                                    
078800       'SERIAL_NUMBER_SUFFIX'      ';'                                    
078900       'ADVICE_NOTE_NUMBER'        ';'                                    
079000       'ADVICE_NOTE_DATE'          ';'                                    
079100       'ADVICE_QUANTITY'           ';'                                    
079200       'SORT_TYPE'                 ';'                                    
079300       'ORDER_PRICE'               ';'                                    
079400       'AGREEMENT_PRICE'           ';'                                    
079500       'UNIT_CODE'                 ';'                                    
079600       'ESTIMATE_PRICE_CODE'       ';'                                    
079700       'ORDER_PRICE_TOTAL'         ';'                                    
079800       'AGREEMENT_PRICE_TOTAL'     ';'                                    
079900       'ACCOUNT_NUMBER'            ';'                                    
080000       'DISTRUBUTION_CENTER'       ';'                                    
080100       'COMPANY_CODE'              ';'                                    
080200       'PRODUCT_GROUP'             ';'                                    
080300       'SYSTEM_CODE'               ';'                                    
080400       'CURRENCY'                  ';'                                    
080500       'CURRENCY_RATE'             ';'                                    
080600       'INFRIEGHT_FACTOR'          ';'                                    
080700       'DEVIATION_TYPE'            ';'                                    
080800       'PROFIT_CENTER'             ';'                                    
080900       'ORDER_NUMBER_FREIGHT'      ';'                                    
081000       'FLAG_1F'                   ';'                                    
081100       'ORDER_NUMBER_PRICE'        ';'                                    
081200       'FLAG_2D'                   ';'                                    
081300       'ORDER_NUMBER_EXCHANGE'     ';'                                    
081400       'FLAG_3VD'                  ';'                                    
081500       'DISTRICT_NUMBER'           ';'                                    
081600       'LOOP_NUMBER_FR1'           ';'                                    
081700       'LOOP_NUMBER_TM1'           ';'                                    
081800       'LOOP_NUMBER_FR2'           ';'                                    
081900       'LOOP_NUMBER_TM2'           ';'                                    
082000       'DEVIATION_INBOUND'         ';'                                    
082100       'TOTAL_ADVICE_NOTE'         ';'                                    
082200       'SIGN'                                                             
082300     DELIMITED BY SIZE INTO HEAD-LINE                                     
082400*                                                                         
082600     WRITE UT5-POST FROM HEAD-LINE                                        
082700     .                                                                    
082800     EJECT                                                                
082900                                                                          
083000 VC-DAP-HEADER-LINE   SECTION.                                            
083100     INITIALIZE   TEXT-AREA                                               
083200     SET HEADER-VC-OK           TO TRUE                                   
083300                                                                          
083400     STRING 'RECORD_TYPE'          ';'                                    
083500       'MR_NUMBER'                 ';'                                    
083600       'PART_NUMBER'               ';'                                    
083700       'SUPPLIER_NUMBER'           ';'                                    
083900       'SERIAL_NUMBER_PREFIX'      ';'                                    
083910       'SERIAL_NUMBER'             ';'                                    
084000       'SERIAL_NUMBER_SUFFIX'      ';'                                    
084100       'ADVICE_NOTE_NUMBER'        ';'                                    
084200       'ADVICE_NOTE_DATE'          ';'                                    
084300       'ADVICE_QUANTITY'           ';'                                    
084400       'SORT_TYPE'                 ';'                                    
084500       'ORDER_PRICE'               ';'                                    
084600       'AGREEMENT_PRICE'           ';'                                    
084700       'UNIT_CODE'                 ';'                                    
084800       'ESTIMATE_PRICE_CODE'       ';'                                    
084900       'STANDARD_PRICE'            ';'                                    
085000       'SUPPLIER_PRICE_TOTAL'      ';'                                    
085100       'ACCOUNT_NUMBER'            ';'                                    
085110       'DIRECT_DELIVERY'           ';'                                    
085200       'DISTRUBUTION_CENTER'       ';'                                    
085300       'COMPANY_CODE'              ';'                                    
085400       'PRODUCT_GROUP'             ';'                                    
085500       'SYSTEM_CODE'               ';'                                    
085600       'CURRENCY'                  ';'                                    
085700       'CURRENCY_RATE'             ';'                                    
085800       'INFRIEGHT_FACTOR'          ';'                                    
085900       'DEVIATION_TYPE'            ';'                                    
086000       'PROFIT_CENTER'             ';'                                    
086100       'ORDER_NUMBER_FREIGHT'      ';'                                    
086200       'FLAG_1F'                   ';'                                    
086300       'ORDER_NUMBER_PRICE'        ';'                                    
086400       'FLAG_2D'                   ';'                                    
086500       'ORDER_NUMBER_EXCHANGE'     ';'                                    
086600       'FLAG_3VD'                  ';'                                    
086700       'DISTRICT_NUMBER'           ';'                                    
086800       'LOOP_NUMBER_FR1'           ';'                                    
086900       'LOOP_NUMBER_TM1'           ';'                                    
087000       'LOOP_NUMBER_FR2'           ';'                                    
087100       'LOOP_NUMBER_TM2'           ';'                                    
087200       'DEVIATION_INBOUND'         ';'                                    
087300       'TOTAL_ADVICE_NOTE'         ';'                                    
087400       'SIGN'                                                             
087500     DELIMITED BY SIZE INTO HEAD-LINE                                     
087600*                                                                         
087800     WRITE UT6-POST FROM HEAD-LINE                                        
087900     .                                                                    
088000     EJECT                                                                
088100                                                                          
088200                                                                          
088300 Z-FINIT SECTION.                                                         
088400     CLOSE W51080                                                         
088500           W51084                                                         
088600           W51085                                                         
088700           W51084A                                                        
088800           W51084B                                                        
088900           W51084UC                                                       
089000           W51084VC                                                       
089100     SKIP2                                                                
089200     MOVE 'S' TO POSTSUM-OPKOD                                            
089300     CALL POSTSUM USING POSTSUM-PARM                                      
089400     .                                                                    
089500     EJECT                                                                
089600                                                                          
089700 S01-LAES-W51080  SECTION.                                                
089800     READ W51080 INTO IN-AREA                                             
089900     AT END                                                               
090000        MOVE HIGH-VALUE TO IN-AREA                                        
090100        SET END-OF-W51080 TO TRUE                                         
090200                                                                          
090300     NOT AT END                                                           
090400        MOVE 'W51080' TO POSTSUM-FDNAMN                                   
090500        MOVE 'W51084D1' TO POSTSUM-DDNAMN2                                
090600        MOVE SPACE     TO POSTSUM-TRANSTYP                                
090700        CALL POSTSUM USING POSTSUM-PARM                                   
090800     END-READ                                                             
090900     .                                                                    
091000     EJECT                                                                
091100                                                                          
091200 S02-SKRIV-LISTA  SECTION.                                                
091300     WRITE UT-POST  FROM UT-AREA                                          
091400     .                                                                    
091500     EJECT                                                                
091600                                                                          
091700 S03-SKRIV-DAP-LISTA  SECTION.                                            
091800     WRITE UT2-POST  FROM UT2-AREA                                        
091900     .                                                                    
092000     EJECT                                                                
092100                                                                          
092200 S04-SKRIV-LISTA  SECTION.                                                
092300     WRITE UT3-POST  FROM UT3-AREA                                        
092400     .                                                                    
092500     EJECT                                                                
092600                                                                          
092700 S05-SKRIV-LISTA  SECTION.                                                
092800     WRITE UT4-POST  FROM UT4-AREA                                        
092900     .                                                                    
093000     EJECT                                                                
093100                                                                          
093200 S06-SKRIV-LISTA  SECTION.                                                
093300     WRITE UT5-POST  FROM UT5-AREA                                        
093400     .                                                                    
093500     EJECT                                                                
093600                                                                          
093700 S07-SKRIV-LISTA  SECTION.                                                
093800     WRITE UT6-POST  FROM UT6-AREA                                        
093900     .                                                                    
094000     EJECT                                                                
094100                                                                          
094200 S99-ABEND SECTION.                                                       
094300     SKIP2                                                                
094400     MOVE 'S' TO POSTSUM-OPKOD                                            
094500     CALL POSTSUM USING POSTSUM-PARM                                      
094600     CALL ABEND USING RKOD-ABEND                                          
094700     .                                                                    
