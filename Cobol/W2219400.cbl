000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2219400.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   15/08/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CREATE A ALERT ON SCREEN 2171 WHEN TOTAL DEMAND WITHIN           
000900*        ANY WEEK DURING CURRENT WEEK + LEADTIME FOR THE MAIN             
001000*        SUPPLIER IS LARGER THAN THE TOTAL ASSETS ON THE DC UP            
001100*        UNTIL THAT WEEK.                                                 
001200*                                                                         
001300*                                                                         
001400*    ABENDCODES:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- PART FROM ALERT FILE - DELETE                              
002700     SELECT W22197                     ASSIGN TO W22194D1.                
002800*          --- PART FILE WDK6                                             
002900     SELECT W01160                     ASSIGN TO W22194D2.                
003000*          --- OUTPUT FILE TO UPDATE WDGX2223/24                          
003100     SELECT W2219401                   ASSIGN TO W22194D3.                
003200*          --- OUTPUT FILE TO DELETE WDGX2223/24                          
003300     SELECT W2219402                   ASSIGN TO W22194D4.                
003400*          --- OUTPUT FILE - KONTROLL FILE INTERNT                        
003500     SELECT W2219403                   ASSIGN TO W22194D5.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W22197                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  RECORD -COPY W01160   -PRE  IN1- -L.                                 
004600     EJECT                                                                
004700 FD  W01160                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  RECORD -COPY W01160   -PRE  IN- -L.                         0        
005200     EJECT                                                                
005300 FD  W2219401                                                             
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  RECORD -COPY W2219401 -PRE  OUT1- -L.                                
005800     EJECT                                                                
005900 FD  W2219402                                                             
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  RECORD -COPY W2219401 -PRE  OUT2- -L.                                
006400     EJECT                                                                
006500 FD  W2219403                                                             
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS  0.                                                   
006800                                                                          
006900*01  RECORD -COPY W2219402 -PRE  OUT3- -L.                                
007000     EJECT                                                                
007100 WORKING-STORAGE SECTION.                                                 
007200*    -- CHECKED BY WY2000                                                 
007300     SKIP3                                                                
007400*    -COPY WY2000W3                                                       
007500     SKIP3                                                                
007600                                                                          
007700 77  IDPGM                       PIC X(8)    VALUE 'W2219400'.            
007800 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
007900 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
008000 77  JA                          PIC X       VALUE 'J'.                   
008100 77  YES                         PIC X       VALUE 'Y'.                   
008200 77  NOO                         PIC X       VALUE 'N'.                   
008300 77  W222-IX                     PIC S9(4)   COMP SYNC VALUE ZERO.        
008400 77  TAB-IX                      PIC S9(4)   COMP SYNC VALUE ZERO.        
008500 77  TAB-IX-MAX                  PIC S9(4)   COMP SYNC VALUE +156.        
008600 77  TAB-IX-MAX-30               PIC S9(4)   COMP SYNC VALUE +30.         
008700                                                                          
008800 77  W-TILLG-BEHOV               PIC S9(07)V9(02) VALUE +0.               
008900 77  W-SLAG-KVPB-REF             PIC 9(9)V9(2) VALUE ZERO COMP-3.         
009000 77  W-KVPB-SEP                  PIC 9(6)V9(2) VALUE ZERO COMP-3.         
009100 77  W-KVAVROP-OLD               PIC S9(07)    VALUE ZERO COMP-3.         
009200 77  W-KVDAGAR-KVAR              PIC 9(3)      VALUE ZERO COMP-3.         
009300 77  W-VECKO-SEP-BEHOV          PIC S9(7)V9(2) VALUE ZERO COMP-3.         
009400 77  W-DAG-SEP-BEHOV            PIC S9(7)V9(2) VALUE ZERO COMP-3.         
009500 77  W-TIME                      PIC 9(8)    VALUE ZERO.                  
009600 77  W-TIFINLV-AAVV              PIC S9(5)               COMP-3.          
009700 77  W-FAKTOR                    PIC S9(1)V9(3)          COMP-3.          
009800 77  W-DELETE-ALERT              PIC X(1)    VALUE SPACE.                 
009900 77  W-CREATE-ALERT              PIC X(1)    VALUE SPACE.                 
010000 77  W-CREATE-ALERT-FIRST-TIME   PIC X(1)    VALUE SPACE.                 
010100 77  W-GET-WDD924                PIC X(1)    VALUE 'N'.                   
010200 77  W-TILEVBSK-DISP-YYWW-LAST   PIC 9(04)   VALUE ZERO.                  
010300 77  W-CLAG-KVVECKOR-LT          PIC S9(3)   COMP-3 VALUE +0.             
010400 77  W-TIBEHOV-FIRST             PIC 9(04)   VALUE ZERO.                  
010500                                                                          
010600 77  SEP-SATS-TPO-LEV-SDC-NDC    PIC X(2)    VALUE '19'.                  
010700                                                                          
010800 01  W-YYWWD.                                                             
010900     03  W-YYWWD-NUM             PIC 9(05).                               
011000     03  W-YYWWD REDEFINES W-YYWWD-NUM.                                   
011100         05 W-YYWW               PIC 9(04).                               
011200         05 W-TODAYS-DAGNR       PIC 9(01).                               
011300                                                                          
011400 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
011500     88  END-OF-W01160                       VALUE 'J'.                   
011600                                                                          
011700 77  W22197-EOF-SW               PIC X       VALUE 'N'.                   
011800     88  END-OF-W22197                       VALUE 'J'.                   
011900                                                                          
012000 01  TAB-BEHOV-X.                                                         
012100    05  TAB-BEHOV OCCURS 156.                                             
012200        10 TAB-YYWW              PIC 9(04).                               
012300        10 FILLER REDEFINES TAB-YYWW.                                     
012400           15 TAB-YYWW-YY        PIC 9(02).                               
012500           15 TAB-YYWW-WW        PIC 9(02).                               
012600        10 TAB-KVBEHOV           PIC S9(7)V9(2) COMP-3.                   
012700        10 TAB-KVAVROP           PIC S9(07).                              
012800        10 TAB-SUDISPV           PIC S9(09)V9(2) COMP-3.                  
012900        10 TAB-FLLARM            PIC X(01).                               
013000                                                                          
013100 01  W-KVDISP                    PIC S9(09) VALUE +0.                     
013200 01  W-KVTILLG                   PIC S9(09)V9(02) VALUE +0.               
013300 01  W-KVAVIS-KVRAPP             PIC S9(09) VALUE +0.                     
013400 01  W-TOT-KVRAPP                PIC S9(09) VALUE +0.                     
013500     EJECT                                                                
013600*    --- COPYTEXT FÖR ATT KUNNA UR DISTR FÅ MOTTAGANDE IDDC               
013700*01  -COPY WWDIST35                                                       
013800     EJECT                                                                
013900*    -COPY WWDC99                                                         
014000     EJECT                                                                
014100*01  -COPY WWDCKONS                                                       
014200     EJECT                                                                
014300*01  -COPY WWPRODSL                                                       
014400     EJECT                                                                
014500 01  WS-DAGENS-AAAAMMDD          PIC 9(8).                                
014600                                                                          
014700 01  TODAYS-DATE                 PIC 9(06).                               
014800                                                                          
014900 01  TODAYS-DATE-YYWWD.                                                   
015000     03  TODAYS-YEAR-WEEK        PIC 9(04).                               
015100     03  FILLER REDEFINES TODAYS-YEAR-WEEK.                               
015200         05  TODAYS-YEAR         PIC 9(2).                                
015300         05  TODAYS-WEEK         PIC 9(2).                                
015400     03  TODAYS-DAGNR            PIC 9(1).                                
015500     EJECT                                                                
015600 01  GENERAL-SUBPROGRAMS.                                                 
015700*                                                                         
015800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
015900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
016200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016300     03  W22222                  PIC X(8)    VALUE 'W22222'.              
016400     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
016500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016600     EJECT                                                                
016700*01  AREA    -COPY W222L222   -PRE W222-.                                 
016800     EJECT                                                                
016900*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
017000                                                                          
017100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
017200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
017300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
017400     SKIP2                                                                
017500 01  ERROR-TEXT.                                                          
017600     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
017700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
017800     EJECT                                                                
017900*    --- PARAMETRAR TILL DATKORT                                          
018000*                                                                         
018100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22194'.              
018200     SKIP2                                                                
018300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
018400     SKIP2                                                                
018500*01  -COPY WDATKORT                                                       
018600     EJECT                                                                
018700*01  -COPY WDATAREA                                                       
018800     EJECT                                                                
018900 01  W01160-AREA-START           PIC X(24)   VALUE                        
019000                                 'W01160-AREA-START  '.                   
019100                                                                          
019200*01  AREA -COPY W01160     -PRE IN-                                       
019300     EJECT                                                                
019400                                                                          
019500*    --- PARAMETRAR TILL POSTSUM                                          
019600*                                                                         
019700*01  -COPY W0005   -PRE  POSTSUM-                                         
019800     EJECT                                                                
019900* VARIABLES TO SUBPROGRAM W009VADD                                        
020000 01  W009-TIBEHOV-START          PIC S9(5)  COMP-3.                       
020100 01  W009-ANTAL-VECKOR           PIC S9(3)  COMP-3.                       
020200     EJECT                                                                
020300                                                                          
020400 01  WORK-AREA-START           PIC X(24)  VALUE 'WORK-AREA-START'.        
020500*01  -COPY W2219401     -PRE WORK-                                        
020600     EJECT                                                                
020700                                                                          
020800 01  OUT3-AREA-START           PIC X(24)  VALUE 'OUT3-AREA-START'.        
020900*01  -COPY W2219402     -PRE OUT3-                                        
021000     EJECT                                                                
021100                                                                          
021200*    --- AREAS FOR IMS-SECTIONS                                           
021300*                                                                         
021400     EJECT                                                                
021500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021600                                                                          
021700 01  KEYS-FOR-DLI.                                                        
021800     03  W-IDARTNR-X.                                                     
021900         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
022000                                                                          
022100     03  W-IDDC-K7-X.                                                     
022200         05  W-IDDC-K7            PIC X(2)   VALUE SPACE.                 
022300                                                                          
022400     03  W-IDLEVNR-X.                                                     
022500         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
022600                                                                          
022700     03  W-WDD901KY-X.                                                    
022800         05  W-IDARTNR-D9         PIC S9(9)  VALUE ZERO COMP-3.           
022900         05  W-IDDC-D9            PIC X(2)   VALUE SPACE.                 
023000                                                                          
023100     03  W-WDGX2231-X.                                                    
023200         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
023300         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
023400                                                                          
023500     03  W-WDGX2232-X.                                                    
023600         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
023700         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
023800                                                                          
023900      03 W-DAINLEV-X.                                                     
024000         05  W-DAINLEV           PIC 9(16).                               
024100                                                                          
024200      03  W-IDPTYP-X.                                                     
024300         05  W-IDPTYP            PIC X(3)   VALUE '310'.                  
024400                                                                          
024500      03 W-KDAVROP-X.                                                     
024600         05  W-KDAVROP           PIC S9(1)   COMP-3 VALUE +2.             
024700                                                                          
024800     SKIP2                                                                
024900*    --- STATUS-KOD FRÅN IMS                                              
025000 01  STATUS-WS                   PIC XX.                                  
025100     88  SEGMENT-FOUND                       VALUE '  '.                  
025200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
025300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
025400     88  SEGMENT-END                         VALUE 'GB'.                  
025500     SKIP2                                                                
025600 01  GOOD-STATUSCODES.                                                    
025700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025800     SKIP3                                                                
025900 01  SSA1                        PIC X(64).                               
026000 01  SSA2                        PIC X(64).                               
026100 01  SSA3                        PIC X(64).                               
026200     EJECT                                                                
026300*    --- IMS FUNCTION CODES                                               
026400*01  -COPY W0003                                                          
026500     EJECT                                                                
026600*    ---  DLI INPUT-OUTPUT AREA                                           
026700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2232'.                    
026800 01  DLI-IO-WDGX2232.                                                     
026900*    03  -COPY WDGX2232                                                   
027000     EJECT                                                                
027100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
027200 01  DLI-IO-WDK701.                                                       
027300*    03  -COPY WDK701                                                     
027400     EJECT                                                                
027500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
027600 01  DLI-IO-WDK711.                                                       
027700*    03  -COPY WDK711                                                     
027800     EJECT                                                                
027900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
028000 01  DLI-IO-WDD901.                                                       
028100*    03  -COPY WDD901 -PRE WDD901-                                        
028200     EJECT                                                                
028300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
028400 01  DLI-IO-WDD905.                                                       
028500*    03  -COPY WDD905 -PRE WDD905-                                        
028600     EJECT                                                                
028700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
028800 01  DLI-IO-WDD924.                                                       
028900*    03  -COPY WDD924 -PRE WDD924-                                        
029000     EJECT                                                                
029100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL201'.                      
029200 01  DLI-IO-WDL201.                                                       
029300*    03  -COPY WDL201 -PRE L201-                                          
029400                                                                          
029500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL211'.                      
029600 01  DLI-IO-WDL211.                                                       
029700*    03  -COPY WDL211                                                     
029800                                                                          
029900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL221'.                      
030000 01  DLI-IO-WDL221.                                                       
030100*    03  -COPY WDL221                                                     
030200                                                                          
030300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL231'.                      
030400 01  DLI-IO-WDL231.                                                       
030500*    03  -COPY WDL231                                                     
030600     EJECT                                                                
030700                                                                          
030800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
030900 01  DLI-IO-WDK901.                                                       
031000*    03  -COPY WDK901                                                     
031100     EJECT                                                                
031200 LINKAGE SECTION.                                                         
031300                                                                          
031400*01  -COPY W0008  -PRE WDD9-                                              
031500     05  FILLER                  PIC X.                                   
031600     EJECT                                                                
031700*01  -COPY W0008  -PRE WDR2-                                              
031800     05  FILLER                  PIC X.                                   
031900     EJECT                                                                
032000*01  -COPY W0008  -PRE WDL2-                                              
032100     05  FILLER                  PIC X.                                   
032200     EJECT                                                                
032300*01  -COPY W0008  -PRE WDK7-                                              
032400     05  FILLER                  PIC X.                                   
032500     EJECT                                                                
032600*01  -COPY W0008  -PRE WDK9-                                              
032700     05  FILLER                  PIC X.                                   
032800     EJECT                                                                
032900 01  W222-WDK6-PCB               PIC X.                                   
033000 01  W222-WDK7-PCB               PIC X.                                   
033100 01  W222-ARTM-PCB               PIC X.                                   
033200 01  W222-2501-PCB               PIC X.                                   
033300 01  W222-WDB6R-PCB              PIC X.                                   
033400 01  W222-WDK7R-PCB              PIC X.                                   
033500 01  W222-WDB6-PCB               PIC X.                                   
033600 01  W222-WDD7-PCB               PIC X.                                   
033700 01  W222-WDK7E-PCB              PIC X.                                   
033800 01  W222-UTIL-WDK6-PCB          PIC X.                                   
033900 01  W222-UTIL-WDK7-PCB          PIC X.                                   
034000 01  W222-UTIL-WDB6-PCB          PIC X.                                   
034100 01  W222-UTUP-WDK7-PCB          PIC X.                                   
034200 01  W222-UTUP-WDB6-PCB          PIC X.                                   
034300 01  W222-UTUP-UTIL-WDK6-PCB     PIC X.                                   
034400 01  W222-UTUP-UTIL-WDK7-PCB     PIC X.                                   
034500 01  W222-UTUP-UTIL-WDB6-PCB     PIC X.                                   
034600     EJECT                                                                
034700 PROCEDURE DIVISION  USING WDD9-PCB WDR2-PCB WDL2-PCB                     
034800                           WDK7-PCB WDK9-PCB                              
034900                           W222-WDK6-PCB  W222-WDK7-PCB                   
035000                           W222-ARTM-PCB  W222-2501-PCB                   
035100                           W222-WDB6R-PCB W222-WDK7R-PCB                  
035200                           W222-WDB6-PCB  W222-WDD7-PCB                   
035300                           W222-WDK7E-PCB                                 
035400                           W222-UTIL-WDK6-PCB                             
035500                           W222-UTIL-WDK7-PCB                             
035600                           W222-UTIL-WDB6-PCB                             
035700                           W222-UTUP-WDK7-PCB                             
035800                           W222-UTUP-WDB6-PCB                             
035900                           W222-UTUP-UTIL-WDK6-PCB                        
036000                           W222-UTUP-UTIL-WDK7-PCB                        
036100                           W222-UTUP-UTIL-WDB6-PCB                        
036200                           .                                              
036300 MAIN SECTION.                                                            
036400     ENTRY 'DLITCBL' USING WDD9-PCB WDR2-PCB WDL2-PCB                     
036500                           WDK7-PCB WDK9-PCB                              
036600                           W222-WDK6-PCB  W222-WDK7-PCB                   
036700                           W222-ARTM-PCB  W222-2501-PCB                   
036800                           W222-WDB6R-PCB W222-WDK7R-PCB                  
036900                           W222-WDB6-PCB  W222-WDD7-PCB                   
037000                           W222-WDK7E-PCB                                 
037100                           W222-UTIL-WDK6-PCB                             
037200                           W222-UTIL-WDK7-PCB                             
037300                           W222-UTIL-WDB6-PCB                             
037400                           W222-UTUP-WDK7-PCB                             
037500                           W222-UTUP-WDB6-PCB                             
037600                           W222-UTUP-UTIL-WDK6-PCB                        
037700                           W222-UTUP-UTIL-WDK7-PCB                        
037800                           W222-UTUP-UTIL-WDB6-PCB                        
037900                           .                                              
038000                                                                          
038100     PERFORM A-INIT                                                       
038200                                                                          
038300* DELETE ALERT                                                            
038400     PERFORM S02-INIT-TAB-BEHOV                                           
038500     MOVE +0               TO W-KVTILLG                                   
038600                                                                          
038700     PERFORM S01-LAES-W22197                                              
038800     PERFORM UNTIL END-OF-W22197                                          
038900        PERFORM C-CREATE-ALERT                                            
039000                                                                          
039100        MOVE YES           TO W-DELETE-ALERT                              
039200        PERFORM D-WRITE-W22194                                            
039300                                                                          
039400        PERFORM S02-INIT-TAB-BEHOV                                        
039500        MOVE +0            TO W-KVTILLG                                   
039600                                                                          
039700        PERFORM S01-LAES-W22197                                           
039800     END-PERFORM                                                          
039900                                                                          
040000* INSERT/REPLACE ALERT                                                    
040100     PERFORM S02-INIT-TAB-BEHOV                                           
040200     MOVE +0               TO W-KVTILLG                                   
040300                                                                          
040400     PERFORM S01-LAES-W01160                                              
040500     PERFORM UNTIL END-OF-W01160                                          
040600       MOVE IN-CLAG-KDPRODSL       TO TEST-KDPRODSL                       
040700       IF  IN-CLAG-KDSORT    = 'SW'                                       
040800       OR  KDPRODSL-LOCAL                                                 
040900       OR  IN-CLAG-KDERS     > +06                                        
041000       OR  IN-CLAG-KDERS-UTG > +0                                         
041100       OR  IN-CLAG-KDUART    = 'S'                                        
041200       OR  IN-CLAG-REDIRLEV  = 1.0                                        
041300       OR (IN-CLAG-IDLEVNR   = '9998' OR '8261')                          
041400       OR  IN-CLAG-TISTODAT-LARM > TODAYS-DATE                            
041500       OR  IN-CLAG-TISTODAT-LARM = 999999                                 
041600          CONTINUE                                                        
041700       ELSE                                                               
041800          IF IN-CLAG-IDDC-REF = SPACE                                     
041900             PERFORM B-CHECK-PB-TOT                                       
042000             IF W-CREATE-ALERT = YES                                      
042100                PERFORM C-CREATE-ALERT                                    
042200                                                                          
042300                MOVE NOO           TO W-DELETE-ALERT                      
042400                PERFORM D-WRITE-W22194                                    
042500                                                                          
042600                PERFORM S02-INIT-TAB-BEHOV                                
042700                MOVE +0            TO W-KVTILLG                           
042800             END-IF                                                       
042900          END-IF                                                          
043000       END-IF                                                             
043100       PERFORM S01-LAES-W01160                                            
043200     END-PERFORM                                                          
043300                                                                          
043400     PERFORM Z-FINIT                                                      
043500                                                                          
043600     MOVE ZERO TO RETURN-CODE                                             
043700     GOBACK                                                               
043800     .                                                                    
043900     EJECT                                                                
044000 A-INIT SECTION.                                                          
044100                                                                          
044200     OPEN INPUT  W01160                                                   
044300                 W22197                                                   
044400          OUTPUT W2219401                                                 
044500                 W2219402                                                 
044600                 W2219403                                                 
044700                                                                          
044800     ACCEPT TODAYS-DATE FROM DATE                                         
044900                                                                          
045000     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
045100                                                                          
045200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
045300     MOVE D-AAR            TO TODAYS-YEAR                                 
045400     MOVE D-VECKA          TO TODAYS-WEEK                                 
045500     MOVE D-DAGNR          TO TODAYS-DAGNR                                
045600                                                                          
045700     MOVE IDPGM            TO POSTSUM-PROGNAMN                            
045800     .                                                                    
045900     EJECT                                                                
046000 B-CHECK-PB-TOT SECTION.                                                  
046100     MOVE 'B-CHECK-PB-TOT          ' TO CURRENT-SECTION                   
046200                                                                          
046300     MOVE YES                         TO W-CREATE-ALERT                   
046400     MOVE +0                          TO W-SLAG-KVPB-REF                  
046500     IF IN-CLAG-DAPBPLAN > WS-DAGENS-AAAAMMDD                             
046600        IF IN-CLAG-KVPB-PLAN < 5.0                                        
046700           MOVE NOO                   TO W-CREATE-ALERT                   
046800        END-IF                                                            
046900     ELSE                                                                 
047000        MOVE IN-CLAG-IDARTNR          TO W-IDARTNR                        
047100        PERFORM IMS-GU-WDK701                                             
047200        IF SEGMENT-FOUND                                                  
047300           PERFORM IMS-GNP-WDK711                                         
047400           PERFORM UNTIL SEGMENT-MISSING                                  
047500           MOVE SLAG-IDDC             TO WS-IDDC                          
047600              IF SLAG-IDDC-REF = WC-CDC-SE                                
047700                 ADD SLAG-KVPB-REF    TO W-SLAG-KVPB-REF                  
047800              END-IF                                                      
047900              PERFORM IMS-GNP-WDK711                                      
048000           END-PERFORM                                                    
048100        END-IF                                                            
048200        IF (IN-CLAG-KVPB-SEP  +                                           
048300            IN-CLAG-KVPB-SATS +                                           
048400            W-SLAG-KVPB-REF)  < 5.0                                       
048500            MOVE NOO                  TO W-CREATE-ALERT                   
048600        END-IF                                                            
048700     END-IF                                                               
048800     .                                                                    
048900     EJECT                                                                
049000 C-CREATE-ALERT SECTION.                                                  
049100     MOVE 'C-CREATE-ALERT          ' TO CURRENT-SECTION                   
049200                                                                          
049300     COMPUTE W-CLAG-KVVECKOR-LT = IN-CLAG-KVVECKOR-LT + 1                 
049400     PERFORM CA-CALL-W22222                                               
049500     PERFORM CB-TILLG                                                     
049600     .                                                                    
049700     EJECT                                                                
049800 CA-CALL-W22222 SECTION.                                                  
049900     MOVE 'CA-CALL-W22222         ' TO CURRENT-SECTION                    
050000                                                                          
050100     MOVE TODAYS-YEAR-WEEK           TO W009-TIBEHOV-START                
050200     MOVE 1                          TO W009-ANTAL-VECKOR                 
050300     CALL W009VADD USING W009-TIBEHOV-START W009-ANTAL-VECKOR             
050400     MOVE W009-TIBEHOV-START         TO W222-TIBEHOV-START                
050500     MOVE IN-CLAG-IDARTNR            TO W222-IDARTNR                      
050600     MOVE SPACE                      TO W222-IDDC                         
050700     MOVE TODAYS-YEAR-WEEK           TO W222-TIAAVV-AKTUELL               
050800     MOVE TODAYS-DAGNR               TO W222-TID-AKTUELL                  
050900     MOVE W-CLAG-KVVECKOR-LT         TO W222-KVVECKOR-BEHOV               
051000     MOVE SEP-SATS-TPO-LEV-SDC-NDC   TO W222-KDBEHOV                      
051100     MOVE NOO                        TO W222-FLINKLDIRLEV                 
051200                                                                          
051300     CALL W22222 USING W222-AREA W222-WDK6-PCB  W222-WDK7-PCB             
051400                                 W222-ARTM-PCB  W222-2501-PCB             
051500                                 W222-WDB6R-PCB W222-WDK7R-PCB            
051600                                 W222-WDB6-PCB  W222-WDD7-PCB             
051700                                 W222-WDK7E-PCB                           
051800                                 W222-UTIL-WDK6-PCB                       
051900                                 W222-UTIL-WDK7-PCB                       
052000                                 W222-UTIL-WDB6-PCB                       
052100                                 W222-UTUP-WDK7-PCB                       
052200                                 W222-UTUP-WDB6-PCB                       
052300                                 W222-UTUP-UTIL-WDK6-PCB                  
052400                                 W222-UTUP-UTIL-WDK7-PCB                  
052500                                 W222-UTUP-UTIL-WDB6-PCB                  
052600                                                                          
052700     IF W222-ANROP-FEL                                                    
052800        PERFORM S04-NOLLA-W22222                                          
052900     END-IF                                                               
053000                                                                          
053100     MOVE +1                         TO TAB-IX                            
053200     MOVE TODAYS-YEAR-WEEK           TO TAB-YYWW   (TAB-IX)               
053300                                                                          
053400     PERFORM CAA-SEP-BEHOV-INNEV-VECKA                                    
053500     COMPUTE TAB-KVBEHOV(TAB-IX) ROUNDED =                                
053600             W-KVPB-SEP + W222-KVBEHOV-DESSUTOM                           
053700                                                                          
053800     MOVE +0                         TO W222-IX                           
053900     PERFORM UNTIL TAB-IX = TAB-IX-MAX                                    
054000       ADD +1                        TO TAB-IX                            
054100                                        W222-IX                           
054200       MOVE TAB-YYWW-YY(TAB-IX - 1)  TO TAB-YYWW-YY(TAB-IX)               
054300       COMPUTE TAB-YYWW-WW(TAB-IX) =                                      
054400               TAB-YYWW-WW(TAB-IX - 1) + 1                                
054500       IF TAB-YYWW-WW(TAB-IX) > +52                                       
054600          MOVE 'AAVV  '              TO DAT-KDDATFORM                     
054700          MOVE TAB-YYWW(TAB-IX)      TO DAT-I-TIDATUM                     
054800          CALL WDATKONV USING DAT-KDDATFORM                               
054900               DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR                     
055000          IF DAT-KDSVAR-FEL                                               
055100             COMPUTE TAB-YYWW-YY(TAB-IX) =                                
055200                     TAB-YYWW-YY(TAB-IX - 1) + 1                          
055300             MOVE 1                  TO TAB-YYWW-WW(TAB-IX)               
055400          END-IF                                                          
055500       END-IF                                                             
055600                                                                          
055700       MOVE W222-KVBEHOV-VECKA (W222-IX)                                  
055800                                     TO TAB-KVBEHOV(TAB-IX)               
055900     END-PERFORM                                                          
056000     .                                                                    
056100     EJECT                                                                
056200 CAA-SEP-BEHOV-INNEV-VECKA SECTION.                                       
056300     MOVE 'CAA-SEP-BEHOV-INNEV-VECKA  ' TO CURRENT-SECTION                
056400                                                                          
056500     ACCEPT W-TIME             FROM TIME                                  
056600     COMPUTE W-VECKO-SEP-BEHOV ROUNDED  =                                 
056700                               IN-CLAG-KVPB-SEP / 4.33                    
056800     COMPUTE W-DAG-SEP-BEHOV   ROUNDED  =  W-VECKO-SEP-BEHOV / 5          
056900     DIVIDE IN-CLAG-TIFINLV BY 10 GIVING W-TIFINLV-AAVV                   
057000     MOVE TODAYS-YEAR-WEEK     TO TMP1-YYWW                               
057100     MOVE W-TIFINLV-AAVV       TO TMP2-YYWW                               
057200     PERFORM WY2000P3                                                     
057300     MOVE 'AAVVD '             TO DAT-KDDATFORM                           
057400     MOVE TAB-YYWW(TAB-IX)     TO W-YYWW                                  
057500     MOVE TODAYS-DAGNR         TO W-TODAYS-DAGNR                          
057600     MOVE W-YYWWD-NUM          TO DAT-I-TIDATUM                           
057700     CALL WDATKONV USING DAT-KDDATFORM                                    
057800                   DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR                 
057900     IF DAT-KDSVAR-FEL                                                    
058000         MOVE ZERO              TO W-KVPB-SEP                             
058100     ELSE                                                                 
058200        IF  DAT-TID = 6 OR                                                
058300            DAT-TID = 7 OR                                                
058400           (DAT-TID = 5 AND W-TIME(1:4) > 1700)                           
058500        OR  TMP1-YYWW < TMP2-YYWW                                         
058600            MOVE ZERO           TO W-KVPB-SEP                             
058700        ELSE                                                              
058800            COMPUTE W-KVDAGAR-KVAR =  5 - DAT-TID                         
058900            IF W-TIME(1:4)         <= 1700                                
059000               ADD +1           TO W-KVDAGAR-KVAR                         
059100            END-IF                                                        
059200                                                                          
059300            MOVE +1             TO W-FAKTOR                               
059400            SUBTRACT IN-CLAG-REDIRLEV FROM W-FAKTOR                       
059500                                                                          
059600            COMPUTE W-KVPB-SEP = W-DAG-SEP-BEHOV *                        
059700                                 W-KVDAGAR-KVAR  *                        
059800                                 W-FAKTOR                                 
059900        END-IF                                                            
060000     END-IF                                                               
060100     .                                                                    
060200     EJECT                                                                
060300 CB-TILLG SECTION.                                                        
060400     MOVE 'CB-TILLG                   ' TO CURRENT-SECTION                
060500                                                                          
060600     COMPUTE W-KVTILLG = IN-CLAG-KVLS                                     
060700                       + IN-CLAG-KVAKS-CDC                                
060800                       - IN-CLAG-KVROS                                    
060900                       - IN-CLAG-KVRESS                                   
061000                       - IN-CLAG-KVSPARR-KVAL                             
061100                                                                          
061200     PERFORM CBA-ADD-WDK9-TO-TILLG                                        
061300                                                                          
061400     MOVE W-KVTILLG             TO W-KVDISP                               
061500                                                                          
061600     PERFORM CBB-ADD-WDL2-TO-TILLG                                        
061700                                                                          
061800     PERFORM CBC-ADD-WDD9-TO-TAB-AVROP                                    
061900     .                                                                    
062000     EJECT                                                                
062100 CBA-ADD-WDK9-TO-TILLG SECTION.                                           
062200     MOVE 'CBA-ADD-WDK9-TO-TILLG' TO CURRENT-SECTION                      
062300                                                                          
062400     MOVE IN-CLAG-IDARTNR TO W-IDARTNR                                    
062500     PERFORM IMS-GU-WDK901                                                
062600     IF SEGMENT-FOUND                                                     
062700        COMPUTE W-KVTILLG = W-KVTILLG      -                              
062800                            ART-KVOKS-BULK -                              
062900                            ART-KVOKS-DAG  -                              
063000                            ART-KVOKS-VOR                                 
063100     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400 CBB-ADD-WDL2-TO-TILLG SECTION.                                           
063500     MOVE 'CBB-ADD-WDL2-TO-TILLG' TO CURRENT-SECTION                      
063600                                                                          
063700     MOVE +0 TO W-TOT-KVRAPP                                              
063800                W-KVAVIS-KVRAPP                                           
063900                                                                          
064000     MOVE IN-CLAG-IDARTNR       TO W-IDARTNR                              
064100     PERFORM IMS-GU-WDL201                                                
064200     IF SEGMENT-FOUND                                                     
064300       PERFORM IMS-GNP-WDL211                                             
064400       PERFORM UNTIL SEGMENT-MISSING                                      
064500         MOVE INL-DAINLEV       TO W-DAINLEV                              
064600         PERFORM IMS-GNP-WDL221                                           
064700         IF SEGMENT-FOUND                                                 
064800           IF MOT-IDDC   = WC-CDC-SE                                      
064900          AND MOT-KDRT   = 7                                              
065000              MOVE ZERO              TO TALLY                             
065100              INSPECT MOT-IDLEVNR TALLYING TALLY                          
065200                      FOR CHARACTERS BEFORE INITIAL SPACE                 
065300              IF TALLY = ZERO                                             
065400                 MOVE ZERO           TO DIST35-IDDISTR                    
065500              ELSE                                                        
065600                 MOVE MOT-IDLEVNR(1:TALLY) TO DIST35-IDDISTR              
065700              END-IF                                                      
065800*--- QUALITY RETURN SKALL EXKLUDERAS.BB RETURN INKLUDERAS.                
065900              IF DIST35-NA-CDC-BB-RETURN                                  
066000              OR DIST35-CN-CDC-RETUR                                      
066100              OR DIST35-IN-CDC-RETUR                                      
066200              OR DIST35-KR-CDC-RETUR                                      
066300              OR DIST35-AE-CDC-RETUR                                      
066400              OR DIST35-TH-CDC-RETUR                                      
066500              OR DIST35-TW-CDC-RETUR                                      
066600              OR DIST35-MY-CDC-RETUR                                      
066700              OR DIST35-RU-CDC-RETUR                                      
066800                 CONTINUE                                                 
066900              ELSE                                                        
067000                 MOVE +0           TO W-TOT-KVRAPP                        
067100                 PERFORM IMS-GNP-WDL231                                   
067200                 PERFORM UNTIL SEGMENT-MISSING                            
067300                   ADD DEL-KVRAPP  TO W-TOT-KVRAPP                        
067400                   PERFORM IMS-GNP-WDL231                                 
067500                 END-PERFORM                                              
067600                                                                          
067700                 COMPUTE W-KVAVIS-KVRAPP =                                
067800                         W-KVAVIS-KVRAPP +                                
067900                        (MOT-KVAVIS - W-TOT-KVRAPP)                       
068000              END-IF                                                      
068100           END-IF                                                         
068200         END-IF                                                           
068300         PERFORM IMS-GNP-WDL211                                           
068400       END-PERFORM                                                        
068500     END-IF                                                               
068600     COMPUTE W-KVTILLG = W-KVTILLG - W-KVAVIS-KVRAPP                      
068700     .                                                                    
068800     EJECT                                                                
068900 CBC-ADD-WDD9-TO-TAB-AVROP SECTION.                                       
069000     MOVE 'CBC-ADD-WDD9-TO-TAB-AVROP' TO CURRENT-SECTION                  
069100                                                                          
069200     MOVE +0                               TO TAB-IX                      
069300     MOVE +0                               TO W-KVAVROP-OLD               
069400                                                                          
069500     MOVE IN-CLAG-IDARTNR                  TO W-IDARTNR-D9                
069600     MOVE WC-CDC-SE                        TO W-IDDC-D9                   
069700     PERFORM IMS-GU-WDD901                                                
069800     IF SEGMENT-FOUND                                                     
069900        PERFORM IMS-GNP-WDD905                                            
070000        PERFORM UNTIL SEGMENT-MISSING                                     
070100          IF WDD905-KVAVROP > ZERO                                        
070200             MOVE 'AAMMDD'                 TO DAT-KDDATFORM               
070300             MOVE WDD905-TIAVRDAT-DISP     TO DAT-I-TIDATUM               
070400                                                                          
070500             CALL WDATKONV USING DAT-KDDATFORM                            
070600                  DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR                  
070700             IF DAT-KDSVAR-OK                                             
070800                                                                          
070900                IF DAT-TIAAVV-GRP < TAB-YYWW (01)                         
071000                   ADD WDD905-KVAVROP      TO W-KVTILLG                   
071100                                              W-KVAVROP-OLD               
071200                END-IF                                                    
071300                                                                          
071400                MOVE +0                    TO TAB-IX                      
071500                PERFORM UNTIL W-CLAG-KVVECKOR-LT = TAB-IX OR              
071600                                           TAB-IX = TAB-IX-MAX            
071700                  ADD +1                   TO TAB-IX                      
071800                  IF DAT-TIAAVV-GRP = TAB-YYWW (TAB-IX)                   
071900                     ADD WDD905-KVAVROP    TO TAB-KVAVROP(TAB-IX)         
072000                  ELSE                                                    
072100                     IF DAT-TIAAVV-GRP >                                  
072200                        TAB-YYWW (W-CLAG-KVVECKOR-LT)                     
072300                        MOVE TAB-IX-MAX    TO TAB-IX                      
072400                     END-IF                                               
072500                  END-IF                                                  
072600                END-PERFORM                                               
072700             END-IF                                                       
072800          END-IF                                                          
072900          PERFORM IMS-GNP-WDD905                                          
073000        END-PERFORM                                                       
073100     END-IF                                                               
073200     .                                                                    
073300     EJECT                                                                
073400 D-WRITE-W22194 SECTION.                                                  
073500     MOVE 'D-WRITE-W22194         ' TO CURRENT-SECTION                    
073600                                                                          
073700     MOVE NOO                        TO W-GET-WDD924                      
073800     MOVE YES                        TO W-CREATE-ALERT-FIRST-TIME         
073900     MOVE +0                         TO TAB-IX                            
074000     MOVE ZERO                       TO W-TILEVBSK-DISP-YYWW-LAST         
074100     MOVE ZERO                       TO W-TIBEHOV-FIRST                   
074200     PERFORM UNTIL W-CLAG-KVVECKOR-LT = TAB-IX                            
074300        ADD  +1                      TO TAB-IX                            
074400        COMPUTE W-KVTILLG = W-KVTILLG + TAB-KVAVROP(TAB-IX)               
074500        IF TAB-IX > +1                                                    
074600           SUBTRACT TAB-KVBEHOV(TAB-IX - 1) FROM W-KVTILLG                
074700        END-IF                                                            
074800                                                                          
074900        MOVE NOO                     TO TAB-FLLARM (TAB-IX)               
075000        IF TAB-KVBEHOV(TAB-IX) > W-KVTILLG                                
075100           MOVE YES                  TO TAB-FLLARM (TAB-IX)               
075200           IF W-GET-WDD924 = NOO                                          
075300              PERFORM DA-GET-WDD924-LAST                                  
075400              MOVE YES               TO W-GET-WDD924                      
075500           END-IF                                                         
075600           IF W-TILEVBSK-DISP-YYWW-LAST = TAB-YYWW (TAB-IX)               
075700           OR W-TILEVBSK-DISP-YYWW-LAST > TAB-YYWW (TAB-IX)               
075800              MOVE NOO               TO TAB-FLLARM (TAB-IX)               
075900           ELSE                                                           
076000              COMPUTE W-TILLG-BEHOV =                                     
076100                      W-KVTILLG - TAB-KVBEHOV(TAB-IX)                     
076200              IF W-TILLG-BEHOV > -0.99 AND W-TILLG-BEHOV < 0.00           
076300                 MOVE NOO            TO TAB-FLLARM (TAB-IX)               
076400              END-IF                                                      
076500           END-IF                                                         
076600                                                                          
076700           IF TAB-FLLARM (TAB-IX)       = YES                             
076800          AND W-CREATE-ALERT-FIRST-TIME = YES                             
076900              MOVE TAB-YYWW (TAB-IX) TO W-TIBEHOV-FIRST                   
077000              MOVE NOO              TO W-CREATE-ALERT-FIRST-TIME          
077100           END-IF                                                         
077200                                                                          
077300        END-IF                                                            
077400        MOVE W-KVTILLG               TO TAB-SUDISPV(TAB-IX)               
077500     END-PERFORM                                                          
077600                                                                          
077700     MOVE NOO                        TO W-CREATE-ALERT                    
077800     MOVE +0                         TO TAB-IX                            
077900     PERFORM UNTIL W-CLAG-KVVECKOR-LT = TAB-IX                            
078000                OR W-CREATE-ALERT     = YES                               
078100       ADD +1                        TO TAB-IX                            
078200       IF TAB-FLLARM (TAB-IX) = YES                                       
078300          MOVE YES                   TO W-CREATE-ALERT                    
078400       END-IF                                                             
078500     END-PERFORM                                                          
078600                                                                          
078700     IF W-CREATE-ALERT = YES                                              
078800        IF W-DELETE-ALERT = NOO                                           
078900           PERFORM S11-MOVE-TO-WORK-AREA                                  
079000           PERFORM S11A-WRITE-W2219401                                    
079100        END-IF                                                            
079200     ELSE                                                                 
079300        IF W-DELETE-ALERT = YES                                           
079400           PERFORM S11-MOVE-TO-WORK-AREA                                  
079500           PERFORM S11B-WRITE-W2219402                                    
079600        END-IF                                                            
079700     END-IF                                                               
079800                                                                          
079900     IF W-DELETE-ALERT = NOO                                              
080000        PERFORM DB-WRITE-W2219403                                         
080100     END-IF                                                               
080200     .                                                                    
080300     EJECT                                                                
080400 DA-GET-WDD924-LAST SECTION.                                              
080500     MOVE 'DA-GET-WDD924-LAST '      TO CURRENT-SECTION                   
080600                                                                          
080700     MOVE ZERO                       TO W-TILEVBSK-DISP-YYWW-LAST         
080800     MOVE IN-CLAG-IDARTNR            TO W-IDARTNR                         
080900     PERFORM IMS-GU-WDD901                                                
081000     IF SEGMENT-FOUND                                                     
081100       MOVE IN-CLAG-IDLEVNR          TO W-IDLEVNR                         
081200       PERFORM IMS-GNP-WDD924-LAST                                        
081300       IF SEGMENT-FOUND                                                   
081400          MOVE 'AAMMDD'              TO DAT-KDDATFORM                     
081500          MOVE WDD924-LEV-TILEVBSK-DISP                                   
081600                                     TO DAT-I-TIDATUM                     
081700          CALL WDATKONV USING DAT-KDDATFORM                               
081800                        DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR            
081900          IF DAT-KDSVAR-OK                                                
082000             MOVE DAT-TIAAVV-GRP     TO W-TILEVBSK-DISP-YYWW-LAST         
082100          END-IF                                                          
082200       END-IF                                                             
082300     END-IF                                                               
082400     .                                                                    
082500     EJECT                                                                
082600 DB-WRITE-W2219403 SECTION.                                               
082700     MOVE 'DB-WRITE-W2219403  '      TO CURRENT-SECTION                   
082800                                                                          
082900     MOVE IN-CLAG-IDANSK            TO OUT3-IDANSK                        
083000     MOVE 223                       TO OUT3-KDLARM                        
083100     MOVE IN-CLAG-IDARTNR           TO OUT3-IDARTNR                       
083200     MOVE WC-CDC-SE                 TO OUT3-IDDC                          
083300     MOVE IN-CLAG-IDLEVNR           TO OUT3-IDLEVNR                       
083400     MOVE W-KVDISP                  TO OUT3-KVDISP                        
083500     MOVE W-KVAVIS-KVRAPP           TO OUT3-KVAVIS                        
083600     MOVE W-KVAVROP-OLD             TO OUT3-KVAVROP-OLD                   
083700     MOVE W-TILEVBSK-DISP-YYWW-LAST TO OUT3-TILEVBSK-DISP-LAST            
083800     MOVE W-CREATE-ALERT            TO OUT3-FLLARM-TOT                    
083900                                                                          
084000     MOVE +1                        TO TAB-IX                             
084100     PERFORM UNTIL TAB-IX > TAB-IX-MAX-30                                 
084200       MOVE TAB-YYWW       (TAB-IX) TO OUT3-TIAAVV  (TAB-IX)              
084300       MOVE TAB-KVBEHOV    (TAB-IX) TO OUT3-KVBEHOV (TAB-IX)              
084400       MOVE TAB-KVAVROP    (TAB-IX) TO OUT3-KVAVROP (TAB-IX)              
084500       MOVE TAB-SUDISPV    (TAB-IX) TO OUT3-SUDISPV (TAB-IX)              
084600       MOVE TAB-FLLARM     (TAB-IX) TO OUT3-FLLARM  (TAB-IX)              
084700       ADD +1                       TO TAB-IX                             
084800     END-PERFORM                                                          
084900                                                                          
085000     PERFORM S11C-WRITE-W2219403                                          
085100     .                                                                    
085200     EJECT                                                                
085300*------------------------------                                           
085400 Z-FINIT SECTION.                                                         
085500     CLOSE W01160                                                         
085600           W22197                                                         
085700           W2219401                                                       
085800           W2219402                                                       
085900           W2219403                                                       
086000     SKIP2                                                                
086100     MOVE 'S' TO POSTSUM-OPKOD                                            
086200     CALL POSTSUM USING POSTSUM-PARM                                      
086300     .                                                                    
086400     EJECT                                                                
086500 S01-LAES-W01160  SECTION.                                                
086600     MOVE 'S01-LAES-W01160        ' TO CURRENT-SECTION                    
086700                                                                          
086800     READ W01160 INTO IN-AREA                                             
086900     AT END                                                               
087000        SET END-OF-W01160 TO TRUE                                         
087100                                                                          
087200     NOT AT END                                                           
087300        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
087400        MOVE 'W22194D2' TO POSTSUM-DDNAMN2                                
087500        CALL POSTSUM USING POSTSUM-PARM                                   
087600     END-READ                                                             
087700     .                                                                    
087800     EJECT                                                                
087900                                                                          
088000 S01-LAES-W22197  SECTION.                                                
088100     MOVE 'S01-LAES-W22197        ' TO CURRENT-SECTION                    
088200                                                                          
088300     READ W22197 INTO IN-AREA                                             
088400     AT END                                                               
088500        SET END-OF-W22197 TO TRUE                                         
088600                                                                          
088700     NOT AT END                                                           
088800        MOVE 'W22197' TO POSTSUM-FDNAMN                                   
088900        MOVE 'W22194D1' TO POSTSUM-DDNAMN2                                
089000        CALL POSTSUM USING POSTSUM-PARM                                   
089100     END-READ                                                             
089200     .                                                                    
089300     EJECT                                                                
089400                                                                          
089500 S02-INIT-TAB-BEHOV SECTION.                                              
089600     MOVE 'S02-INIT-TAB-BEHOV     ' TO CURRENT-SECTION                    
089700                                                                          
089800     MOVE +1         TO TAB-IX                                            
089900     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
090000       MOVE ZERO     TO TAB-YYWW    (TAB-IX)                              
090100       MOVE +0       TO TAB-KVBEHOV (TAB-IX)                              
090200                        TAB-KVAVROP (TAB-IX)                              
090300                        TAB-SUDISPV (TAB-IX)                              
090400       MOVE SPACE    TO TAB-FLLARM  (TAB-IX)                              
090500       ADD +1        TO TAB-IX                                            
090600     END-PERFORM                                                          
090700     .                                                                    
090800     EJECT                                                                
090900 S04-NOLLA-W22222 SECTION.                                                
091000     MOVE 'S04-NOLLA-W22222       ' TO CURRENT-SECTION                    
091100                                                                          
091200     MOVE +0                        TO W222-KVBEHOV-SUMMA                 
091300                                       W222-KVBEHOV-DESSUTOM              
091400                                       W222-TIBEHOV-FIRST                 
091500                                                                          
091600     MOVE +1                        TO TAB-IX                             
091700     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
091800       MOVE +0                      TO W222-KVBEHOV-VECKA(TAB-IX)         
091900       ADD +1                       TO TAB-IX                             
092000     END-PERFORM                                                          
092100     .                                                                    
092200     EJECT                                                                
092300 S11-MOVE-TO-WORK-AREA SECTION.                                           
092400     MOVE 'S11-MOVE-TO-WORK-AREA   ' TO CURRENT-SECTION                   
092500                                                                          
092600     MOVE IN-CLAG-IDANSK             TO W-IDANSK-2232                     
092700     PERFORM IMS-GU-WDR220                                                
092800     IF SEGMENT-FOUND                                                     
092900        MOVE 2232-IDANSK-LARM        TO WORK-IDANSK                       
093000     ELSE                                                                 
093100        MOVE ZERO                    TO WORK-IDANSK                       
093200     END-IF                                                               
093300     MOVE 223                        TO WORK-KDLARM                       
093400     MOVE IN-CLAG-IDARTNR            TO WORK-IDARTNR                      
093500     MOVE WC-CDC-SE                  TO WORK-IDDC                         
093600     MOVE JA                         TO WORK-FLNYLARM                     
093700     MOVE ZERO                       TO WORK-IDDISTR                      
093800     MOVE ZERO                       TO WORK-IDKUNDNR                     
093900     MOVE '0000000   '               TO WORK-IDKUNDRF                     
094000     MOVE 1                          TO WORK-IDLOPNR                      
094100     MOVE 'W221'                     TO WORK-IDTRANS                      
094200     MOVE SPACE                      TO WORK-KDMFSFOR                     
094300     MOVE ZERO                       TO WORK-IDKR                         
094400     MOVE IN-CLAG-IDLEVNR            TO WORK-IDLEVNR                      
094500     MOVE W-TIBEHOV-FIRST            TO WORK-TIBEHOV-FIRST                
094600     .                                                                    
094700     EJECT                                                                
094800 S11A-WRITE-W2219401 SECTION.                                             
094900     MOVE 'S11A-WRITE-W2219401     ' TO CURRENT-SECTION                   
095000                                                                          
095100     WRITE OUT1-RECORD FROM WORK-W2219401                                 
095200                                                                          
095300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
095400     MOVE 'W2219401' TO POSTSUM-FDNAMN                                    
095500     MOVE 'W22194D3' TO POSTSUM-DDNAMN2                                   
095600     CALL POSTSUM USING POSTSUM-PARM                                      
095700     .                                                                    
095800     EJECT                                                                
095900 S11B-WRITE-W2219402 SECTION.                                             
096000     MOVE 'S11B-WRITE-W2219402     ' TO CURRENT-SECTION                   
096100                                                                          
096200     WRITE OUT2-RECORD FROM WORK-W2219401                                 
096300                                                                          
096400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
096500     MOVE 'W2219401' TO POSTSUM-FDNAMN                                    
096600     MOVE 'W22194D4' TO POSTSUM-DDNAMN2                                   
096700     CALL POSTSUM USING POSTSUM-PARM                                      
096800     .                                                                    
096900     EJECT                                                                
097000 S11C-WRITE-W2219403 SECTION.                                             
097100     MOVE 'S11C-WRITE-W2219403     ' TO CURRENT-SECTION                   
097200                                                                          
097300     WRITE OUT3-RECORD FROM OUT3-W2219402                                 
097400                                                                          
097500     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
097600     MOVE 'W2219403' TO POSTSUM-FDNAMN                                    
097700     MOVE 'W22194D5' TO POSTSUM-DDNAMN2                                   
097800     CALL POSTSUM USING POSTSUM-PARM                                      
097900     .                                                                    
098000     EJECT                                                                
098100 S99-ABEND SECTION.                                                       
098200                                                                          
098300     SKIP2                                                                
098400     MOVE 'S' TO POSTSUM-OPKOD                                            
098500     CALL POSTSUM USING POSTSUM-PARM                                      
098600     CALL ABEND USING RKOD-ABEND                                          
098700     .                                                                    
098800     EJECT                                                                
098900* --- IMS SECTIONS  ---                                                   
099000                                                                          
099100 IMS-GU-WDK701 SECTION.                                                   
099200     MOVE 'IMS-GU-WDK701   '    TO DBS-SECTION                            
099300                                                                          
099400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
099500          DELIMITED BY SIZE   INTO SSA1                                   
099600     MOVE '  GE' TO GOOD-STATUSCODES                                      
099700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
099800     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
099900     PERFORM IMS-STATUSCHECK                                              
100000     .                                                                    
100100                                                                          
100200 IMS-GNP-WDK711 SECTION.                                                  
100300     MOVE 'IMS-GNP-WDK711  '  TO DBS-SECTION                              
100400                                                                          
100500     MOVE 'WDK711 '        TO SSA1                                        
100600     MOVE '  GE'           TO GOOD-STATUSCODES                            
100700     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
100800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
100900     PERFORM IMS-STATUSCHECK                                              
101000     .                                                                    
101100                                                                          
101200 IMS-GU-WDD901 SECTION.                                                   
101300     MOVE 'IMS-GU-WDD901 '   TO DBS-SECTION                               
101400                                                                          
101500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
101600          DELIMITED BY SIZE INTO SSA1                                     
101700     MOVE '  GE' TO GOOD-STATUSCODES                                      
101800     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
101900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
102000     PERFORM IMS-STATUSCHECK                                              
102100     .                                                                    
102200                                                                          
102300 IMS-GNP-WDD905 SECTION.                                                  
102400     MOVE 'IMS-GNP-WDD905 '   TO DBS-SECTION                              
102500                                                                          
102600     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
102700          DELIMITED BY SIZE   INTO SSA1                                   
102800     MOVE '  GE' TO GOOD-STATUSCODES                                      
102900     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
103000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
103100     PERFORM IMS-STATUSCHECK                                              
103200     .                                                                    
103300                                                                          
103400 IMS-GNP-WDD924-LAST SECTION.                                             
103500     MOVE 'IMS-GNP-WDD924-LAST   ' TO DBS-SECTION                         
103600                                                                          
103700     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
103800          DELIMITED BY SIZE INTO SSA1                                     
103900     MOVE 'WDD924  *L ' TO SSA2                                           
104000     MOVE '  GE' TO GOOD-STATUSCODES                                      
104100     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1 SSA2              
104200                                                                          
104300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
104400     PERFORM IMS-STATUSCHECK                                              
104500     .                                                                    
104600     EJECT                                                                
104700 IMS-GU-WDR220 SECTION.                                                   
104800     MOVE 'IMS-GU-WDR220 '  TO DBS-SECTION                                
104900                                                                          
105000     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
105100          DELIMITED BY SIZE INTO SSA1                                     
105200     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
105300          DELIMITED BY SIZE INTO SSA2                                     
105400     MOVE '  GE' TO GOOD-STATUSCODES                                      
105500     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2232 SSA1 SSA2             
105600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
105700     PERFORM IMS-STATUSCHECK                                              
105800     .                                                                    
105900     SKIP2                                                                
106000 IMS-GU-WDL201 SECTION.                                                   
106100     MOVE 'IMS-GU-WDL201 '  TO DBS-SECTION                                
106200                                                                          
106300     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
106400          DELIMITED BY SIZE INTO SSA1                                     
106500     MOVE '  GE'           TO GOOD-STATUSCODES                            
106600     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
106700     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
106800     PERFORM IMS-STATUSCHECK                                              
106900     .                                                                    
107000                                                                          
107100 IMS-GNP-WDL211 SECTION.                                                  
107200     MOVE 'IMS-GNP-WDL211'  TO DBS-SECTION                                
107300                                                                          
107400     MOVE 'WDL211'         TO SSA1                                        
107500     MOVE '  GE'           TO GOOD-STATUSCODES                            
107600     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL211 SSA1                   
107700     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
107800     PERFORM IMS-STATUSCHECK                                              
107900     .                                                                    
108000                                                                          
108100 IMS-GNP-WDL221 SECTION.                                                  
108200     MOVE 'IMS-GNP-WDL221'  TO DBS-SECTION                                
108300                                                                          
108400     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
108500             DELIMITED BY SIZE INTO SSA1                                  
108600     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
108700             DELIMITED BY SIZE INTO SSA2                                  
108800     MOVE '  GE' TO GOOD-STATUSCODES                                      
108900     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1 SSA2              
109000     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
109100     PERFORM IMS-STATUSCHECK                                              
109200     .                                                                    
109300                                                                          
109400 IMS-GNP-WDL231 SECTION.                                                  
109500     MOVE 'IMS-GNP-WDL231'  TO DBS-SECTION                                
109600                                                                          
109700     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
109800             DELIMITED BY SIZE INTO SSA1                                  
109900     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
110000             DELIMITED BY SIZE INTO SSA2                                  
110100     MOVE 'WDL231   ' TO SSA3                                             
110200     MOVE '  GE' TO GOOD-STATUSCODES                                      
110300     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL231 SSA1 SSA2 SSA3         
110400     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
110500     PERFORM IMS-STATUSCHECK                                              
110600     .                                                                    
110700                                                                          
110800 IMS-GU-WDK901                           SECTION.                         
110900                                                                          
111000     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
111100          DELIMITED BY SIZE INTO SSA1                                     
111200     MOVE '  GE' TO GOOD-STATUSCODES                                      
111300     CALL CBLTDLI USING GHU WDK9-PCB DLI-IO-WDK901 SSA1                   
111400     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
111500     PERFORM IMS-STATUSCHECK                                              
111600     .                                                                    
111700     SKIP2                                                                
111800 IMS-STATUSCHECK SECTION.                                                 
111900                                                                          
112000     SET STATUS-IX TO 1                                                   
112100     SEARCH GOOD-STATUS                                                   
112200       AT END                                                             
112300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
112400           DELIMITED BY SIZE INTO ERROR-TEXT                              
112500         DISPLAY ERROR-TEXT                                               
112600         CALL FELLOG                                                      
112700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
112800         CONTINUE                                                         
112900     END-SEARCH                                                           
113000     .                                                                    
113100     EJECT                                                                
113200*    -COPY WY2000P3                                                       
