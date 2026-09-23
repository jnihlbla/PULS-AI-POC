000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL010800.                                                
000300 AUTHOR.         BERT ANDERSSON.                                          
000400 DATE-WRITTEN.   MAJ 2005.                                                
000500                                                                          
000600                                                                          
000700     REMARKS.                                                             
000800* WL010800 PROGRAM IS A REPLICA OF W6011700 PROGRAM                       
000900* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
001000*                                                                         
001100*    NAMN:       CARPARTS.LDC.R34REGISTRATION                             
001200*                                                                         
001300*    FUNKTION:                                                            
001400*        STANSBILD FÖR UPPLÄGGNING AV R34-TRANSAR. PROGRAMMET             
001500*        SKICKAR TRANSAR TILL PROGRAM W611C.                              
001600*                                                                         
001700*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001800*        PROGRAMMET LÄSER              WDK7                               
001900*                                                                         
002000*        PROGRAMMET ANROPAR    W411SAP FÖR KONTROLL AV                    
002100*                              KONTERINGSINFO, WLSAPC (WDH3)              
002200*        PROGRAMMET ANROPAR    WDB6 FÖR KONTROLL AV DC                    
002300*                              HÄMTAR KONTO, ANALYS OCH KOSTSTÄLLE        
002400*    INDATA.                                                              
002500*        TRANSAKTION: WL0108                                              
002600*        MID:         WL0108I1                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         WLO108O1                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600     SKIP2                                                                
003700 77  IDPGM                       PIC X(8)    VALUE 'WL010800'.            
003800 77  FILLER                      PIC X(08)   VALUE 'AAAAAAAA'.            
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
004200 77  HOPP-TILL-0167              PIC X(1)    VALUE 'N'.                   
004300 77  PGM-POS                     PIC X(32)   VALUE SPACE.                 
004400 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
004500 77  INDX                        PIC S9(9)   VALUE +0 COMP-3.             
004600 77  INDX-TAB                    PIC S9(9)   VALUE +0 COMP-3.             
004700 77  MAX-TAB-INDX                PIC S9(9)   VALUE +0 COMP-3.             
004800 77  IX                          PIC 9(2)    VALUE ZERO.                  
004900 77  IX2                         PIC 9(2)    VALUE ZERO.                  
005000 77  MAX-INDX-100                PIC S9(9)   VALUE +100 COMP-3.           
005100 77  611C-IX                     PIC S9(4)   VALUE +0 COMP SYNC.          
005200 77  611C-MAX                    PIC S9(4)   VALUE +22 COMP SYNC.         
005300 77  WS-IDTTYP                   PIC X(3)    VALUE SPACE.                 
005400 77  WS-IDLAND-SPR               PIC X(2)    VALUE SPACE.                 
005500                                                                          
005600 77  WS-IDDISTR                  PIC S9(5)   VALUE ZERO COMP-3.           
005700 77  WS-IDKUNDNR                 PIC S9(7)   VALUE ZERO COMP-3.           
005800                                                                          
005900 77  WS-FLTRACK                  PIC X(1)    VALUE 'N'.                   
006000 77  WS-VALD-KVAVIS              PIC 9(6)    VALUE ZERO.                  
006100 77  WS-TEMP-KVAVIS              PIC 9(6)    VALUE ZERO.                  
006200 77  WS-TEMP-KVAVIS-SHOW         PIC Z(4)9.                               
006300 77  WS-IDARTNR-FL               PIC 9(9) COMP-3.                         
006400 77  WS-TEMP-IDARTNR             PIC 9(7).                                
006500 77  WS-NOT-FIRST-FL             PIC X(1)    VALUE ' '.                   
006600                                                                          
006700 77  IDTRACK-QTY-SW              PIC X       VALUE 'N'.                   
006800     88  IDTRACK-QTY-DONE                    VALUE 'J'.                   
006900     88  IDTRACK-QTY-NOT-DONE                VALUE 'N'.                   
007000                                                                          
007100*77  WS-IDFTG  -- IN COPYTEXT WWIDFTG                                     
007200 77  WS-IDFTG-B6                 PIC 9(2)    VALUE ZERO.                  
007300                                                                          
007400 77  WS-IDKST                    PIC X(10)   VALUE SPACE.                 
007500                                                                          
007600 77  WS-IDKONTO                  PIC 9(10)   VALUE ZERO.                  
007700                                                                          
007800 77  WS-IDANALYS                 PIC X(12)   VALUE SPACE.                 
007900 77  WS-IDLEVNR                  PIC X(4)    VALUE SPACE.                 
008000 77  WS-KDRT                     PIC 9(2)    VALUE ZERO.                  
008100                                                                          
008200 77  WS-VKART-K7                 PIC 9(7)    VALUE ZERO.                  
008300 77  WS-VLARTNTO-K7              PIC 9(8)V9(1) VALUE ZERO.                
008400 77  WS-KDARTURS-K7              PIC X(2) VALUE SPACE.                    
008500                                                                          
008600*--  VÄRDEN FÖR KDBEH                                                     
008700 77  PACKING-ADJUSTMENT          PIC X(1)   VALUE 'P'.                    
008800 77  MIX-STOCK                   PIC X(1)   VALUE 'M'.                    
008900 77  SCRAP                       PIC X(1)   VALUE 'S'.                    
009000 77  CORE-SCRAP                  PIC X(1)   VALUE 'O'.                    
009100 77  LOCAL-REMAN                 PIC X(1)   VALUE 'L'.                    
009200 77  EXCHANGE-CONV               PIC X(1)   VALUE 'C'.                    
009300                                                                          
009400* -- HÅRDKODAT VÄRDE SOM BORDE HA FUNNITS PÅ WDB6.                        
009500* -- DET GÄLLER ALLA FÖREKOMMANDE VARIANTER.                              
009600 77  DCS-IDLEVNR-ALLA            PIC X(5)  VALUE  '0    '.                
009700*77  DCS-KDRT-ALLA               PIC 9(2)  VALUE  6.                      
009800                                                                          
009900* -- TEMPORÄRT HÅRDKODADE VÄRDEN FÖR AUSTRALIEN                           
010000*    -- CORE SCRAPPING                                                    
010100 77  DCS-IDDISTR-OSKROT          PIC 9(4)  VALUE  82.                     
010200 77  DCS-IDKUNDNR-OSKROT         PIC 9(6)  VALUE  0.                      
010300 77  DCS-IDKONTO-OSKROT          PIC 9(10) VALUE  482311.                 
010400 77  DCS-IDANALYS-OSKROT         PIC X(12) VALUE  '158600000316'.         
010500 77  DCS-IDKST-OSKROT            PIC X(10) VALUE  SPACE.                  
010600                                                                          
010700*    -- LOCAL REMANUFACUTER                                               
010800*    -- DISTRIKT BORDE MATAS IN MEN ISTÄLLET ANVÄNDS DEFAULT.             
010900*    -- OM KONTO ÄNDRAS FÖR KINA SÅ SKALL PROGRAM 6011C00                 
011000*    -- OCKSÅ ÄNDRAS.                                                     
011100 77  DCS-IDDISTR-LRENOV          PIC 9(4)  VALUE  82.                     
011200 77  DCS-IDKUNDNR-LRENOV         PIC 9(6)  VALUE  0.                      
011300 77  DCS-IDKONTO-LRENOV          PIC 9(10) VALUE  483105.                 
011400 77  DCS-IDANALYS-LRENOV         PIC X(12) VALUE  '158600000810'.         
011500 77  DCS-IDKST-LRENOV            PIC X(10) VALUE  SPACE.                  
011600                                                                          
011700 77  DCS-IDANALYS-LRENOV-CN      PIC X(12) VALUE  '449600000059'.         
011800 77  DCS-IDKST-LRENOV-CN         PIC X(10) VALUE  '30400'.                
011900                                                                          
012000*    -- EXCHANGE CONVERSION                                               
012100 77  DCS-IDDISTR-BKONV           PIC 9(4)  VALUE  82.                     
012200 77  DCS-IDKUNDNR-BKONV          PIC 9(6)  VALUE  0.                      
012300 77  DCS-IDKONTO-BKONV           PIC 9(10) VALUE  483104.                 
012400 77  DCS-IDANALYS-BKONV          PIC X(12) VALUE  '158600000798'.         
012500 77  DCS-IDKST-BKONV             PIC X(10) VALUE  SPACE.                  
012600                                                                          
012700* -- TEMPORÄRT HÅRDKODADE VÄRDEN FÖR KINA                                 
012800*    -- CORE SCRAPPING                                                    
012900 77  DCS-IDDISTR-OSKROT-CN       PIC 9(4)  VALUE  82.                     
013000 77  DCS-IDKUNDNR-OSKROT-CN      PIC 9(6)  VALUE  0.                      
013100 77  DCS-IDKONTO-OSKROT-CN       PIC 9(10) VALUE  482301.                 
013200 77  DCS-IDANALYS-OSKROT-CN      PIC X(12) VALUE  '449600000056'.         
013300 77  DCS-IDKST-OSKROT-CN         PIC X(10) VALUE  SPACE.                  
013400                                                                          
013500* -- TEMPORÄRT HÅRDKODADE VÄRDEN FÖR KOREA                                
013600*    -- CORE SCRAPPING                                                    
013700 77  DCS-IDDISTR-OSKROT-KR       PIC 9(4)  VALUE  82.                     
013800 77  DCS-IDKUNDNR-OSKROT-KR      PIC 9(6)  VALUE  0.                      
013900 77  DCS-IDKONTO-OSKROT-KR       PIC 9(10) VALUE  482301.                 
014000 77  DCS-IDANALYS-OSKROT-KR      PIC X(12) VALUE  SPACE.                  
014100 77  DCS-IDKST-OSKROT-KR         PIC X(10) VALUE  'HC30000'.              
014200                                                                          
014300 77  WDK7-SAKNAS-SW              PIC X       VALUE 'N'.                   
014400     88  WDK7-SAKNAS                         VALUE 'J'.                   
014500                                                                          
014600 77  ALLT-SW                     PIC X       VALUE 'J'.                   
014700     88  ALLT-OK                             VALUE 'J'.                   
014800                                                                          
014900  01  WS-SORT-TAB.                                                        
015000     03  WS-SOR-DATA  OCCURS 100 TIMES.                                   
015100         05  TAB-IDARTNR        PIC S9(9)   VALUE +0 COMP-3.              
015200         05  TAB-KVAVIS         PIC 9(6)    VALUE ZERO.                   
015300                                                                          
015400  01  WS-NEW-TAB.                                                         
015500     03  WS-NEW-DATA  OCCURS 100 TIMES INDEXED BY INDX-NEW-TAB.           
015600         05  NEW-TAB-IDARTNR    PIC S9(9)   VALUE +0 COMP-3.              
015700         05  NEW-TAB-KVAVIS     PIC 9(6)    VALUE ZERO.                   
015800         05  NEW-TAB-FLAG       PIC X(1)    VALUE SPACE.                  
015900         05  NEW-TAB-KVAV       PIC 9(6)    VALUE ZERO.                   
016000                                                                          
016100 01  WS-AVIS.                                                             
016200     03 WS-KVAVIS OCCURS 100     PIC S9(8)   VALUE +0 COMP-3.             
       01  WS-KDPRODSL.                                                         
           03 WS-KDPRODSL-FDIGIT       PIC 9(1).                                
           03 WS-KDPRODSL-LDIGIT       PIC 9(1).                                
016300 01  WS-LOPNR-R34.                                                        
016400     03  WS-VV                   PIC 9(2)  VALUE ZERO.                    
016500     03  WS-D                    PIC 9(1)  VALUE ZERO.                    
016600     03  WS-LOPNR-ZERO           PIC 9(5)  VALUE ZERO.                    
016700 01  LNG-P-TO-P-PREFIX           PIC S9(4)   VALUE +17 COMP SYNC.         
016800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
016900 01  FILLER REDEFINES DAGENS-DATUM.                                       
017000     03  DAGENS-AA               PIC 9(2).                                
017100     03  DAGENS-MM               PIC 9(2).                                
017200     03  DAGENS-DD               PIC 9(2).                                
017300                                                                          
017400 01  WS-IDDC-LOCAL.                                                       
017500     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
017600     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
017700     03  FILLER                  PIC X(1)   VALUE SPACE.                  
017800                                                                          
017900 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
018000                                                                          
018100 01  TEST-DATUM                PIC 9(6)    VALUE ZERO.                    
018200 01  FILLER REDEFINES TEST-DATUM.                                         
018300     03  TEST-AA                 PIC 9(2).                                
018400     03  TEST-MM                 PIC 9(2).                                
018500     03  TEST-DD                 PIC 9(2).                                
018600                                                                          
018700 01  TMP1-YYMMDD               PIC 9(6)    VALUE ZERO.                    
018800 01  FILLER REDEFINES TMP1-YYMMDD.                                        
018900     03  TMP1-YY                 PIC 9(2).                                
019000     03  TMP1-MM                 PIC 9(2).                                
019100     03  TMP1-DD                 PIC 9(2).                                
019200                                                                          
019300 01  TMP2-YYMMDD               PIC 9(6)    VALUE ZERO.                    
019400 01  FILLER REDEFINES TMP2-YYMMDD.                                        
019500     03  TMP2-YY                 PIC 9(2).                                
019600     03  TMP2-MM                 PIC 9(2).                                
019700     03  TMP2-DD                 PIC 9(2).                                
019800                                                                          
019900 01  TEST-IDARTNR                PIC 9(9) COMP-3.                         
020000                                                                          
020100 01  BUMPER-IDARTNR REDEFINES TEST-IDARTNR PIC S9(9) COMP-3.              
020200       88  BUMPER            VALUE  3207235                               
020300                                    3207236                               
020400                                    3296330                               
020500                                    3296873                               
020600                                    3342319                               
020700                                    3342320                               
020800                                    3342321                               
020900                                    3342322                               
021000                                    3343633                               
021100                                    3343634                               
021200                                    3433204                               
021300                                    3433205                               
021400                                    3434364                               
021500                                    3434368                               
021600                                    3434675                               
021700                                    3445266                               
021800                                    3445278                               
021900                                    3472189                               
022000                                    3472190                               
022100                                    3472267.                              
022200                                                                          
022300                                                                          
022400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
022500                                                                          
022600                                                                          
022700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
022800                                                                          
022900                                                                          
023000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
023100     88  INDATA-OK                           VALUE 'J'.                   
023200     88  INDATA-FEL                          VALUE 'N'.                   
023300                                                                          
023400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
023500     88  EGEN-MID                            VALUE '6117'.                
023600     88  GODK-MID                            VALUE '6111' '6112'          
023700                                                   '6113' '6114'          
023800                                                   '6115' '6116'          
023900                                                   '6117' '6118'          
024000                                                   '6119'.                
024100     88  HELP-MID                            VALUE '0551'.                
024200     EJECT                                                                
024300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
024400 01  GENERELLA-SUBPROGRAM.                                                
024500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
024600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
024700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
024800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
024900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
025000     03  W411SAP                 PIC X(8)    VALUE 'W411SAP'.             
025100     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
025200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
025300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
025400     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
025500     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
025600     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
025700*                                                                         
025800*    --- PARAMETERS TO ABEND                                              
025900                                                                          
026000 77  FILLER                      PIC X(08)   VALUE 'ABENDARE'.            
026100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +33.              
026200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
026300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
026400     SKIP2                                                                
026500*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
026600*01 -COPY WDATAREA                                                        
026700     SKIP2                                                                
026800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
026900*01 -COPY WMEDAREA                                                        
027000     SKIP3                                                                
027100*01  -COPY WL01TIDZ                                                       
027200*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
027300*01 -COPY W005WDK7                                                        
027400     EJECT                                                                
027500*01  -COPY WWIDFTG                                                        
027600     EJECT                                                                
027700*                                                                         
027800 01  MESSAGE-CODES.                                                       
027900     03  ERR-WRONG-KEY           PIC X(3)     VALUE '022'.                
028000     03  ERR-CORR-FIELDS         PIC X(3)     VALUE '023'.                
028100     03  ERR-MAX-DECL-IDTRACK    PIC X(3)     VALUE '423'.                
028200     03  ERR-ARTIKEL-SAKNAS      PIC X(3)     VALUE '017'.                
028300     03  INF-UPDATE-DONE         PIC X(3)     VALUE '001'.                
028400     03  ERR-PART-NO-IS-OBSOLETE PIC X(3)     VALUE '309'.                
028500     03  ERR-PRIS-SAKNAS         PIC X(3)     VALUE '301'.                
028600     03  ERR-EXEC-BUTTON-NOT-PRES    PIC X(3) VALUE '003'.                
028700     03  ERR-PLACE-MISSING       PIC X(3)     VALUE '764'.                
028800     03  ERR-WEIGHT-MISSING      PIC X(3)     VALUE '792'.                
028900     03  ERR-VOLUME-MISSING      PIC X(3)     VALUE '793'.                
029000     03  ERR-ORIGIN-MISSING      PIC X(3)     VALUE '794'.                
029100     03  ERR-IDELMT-MISSING      PIC X(3)     VALUE '041'.                
029200     03  ERR-PRICE-MISSING       PIC X(3)     VALUE '260'.                
029300     03  ERR-ORDER-FINISHED      PIC X(3)     VALUE '306'.                
029400     03  ERR-PART-WEIGHT-IS-MISSING  PIC X(3) VALUE '310'.                
029500     03  ERR-PART-VOLUME-MISSING     PIC X(3) VALUE '311'.                
029600     03  ERR-PART-ORIGIN-MISSING     PIC X(3) VALUE '312'.                
029700     03  ERR-PART-ADDRESS-MISSING    PIC X(3) VALUE '313'.                
029800     03  ERR-PART-SVS-ADDRESS-MISSING PIC X(3) VALUE '314'.               
029900     03  ERR-OBEHORIG            PIC X(3)    VALUE '324'.                 
030000     03  ERR-ORDER-FINNS         PIC X(3)    VALUE '030'.                 
030100     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
030200     03  ERR-FIELD-IS-INVALID    PIC X(3)    VALUE '023'.                 
030300     03  ERR-INVALID-KEY-FIELDS  PIC X(3)    VALUE '022'.                 
030400     SKIP2                                                                
030500*01  -COPY WDECAREA                                                       
030600     SKIP2                                                                
030700 01  FILLER                      PIC X(16)   VALUE 'W411SAP '.            
030800     SKIP3                                                                
030900*01 -COPY W411SAP                                                         
031000     SKIP3                                                                
031100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
031200*                                                                         
031300 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
031400     SKIP3                                                                
031500*01  -COPY WZ01SUB                                                        
031600     EJECT                                                                
031700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
031800     SKIP3                                                                
031900 01  REQU-AREA.                                                           
032000*    03  -COPY WZ01REQU                                                   
032100*    03  -COPY WL0108I1                                                   
032200     EJECT                                                                
032300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
032400     SKIP3                                                                
032500 01  RESP-AREA.                                                           
032600*    03  -COPY WZ01RESP                                                   
032700*    03  -COPY WL0108O1                                                   
032800     SKIP3                                                                
032900***********************************************************               
033000 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
033100     SKIP3                                                                
033200 01  KOM-MSG-IO-AREA.                                                     
033300*03  -COPY WMSGKOM                                                        
033400     EJECT                                                                
033500 01  FILLER -COPY WMSGSNUF -PRE SNUF-                                     
033600     SKIP3                                                                
033700 01  MID611C-START               PIC X(16) VALUE                          
033800                                      'MID611C-START'.                    
033900 01  611C-MSG-IO-AREA.                                                    
034000     03  611C-LL                 PIC S9(4) VALUE +0 COMP SYNC.            
034100     03  611C-Z1                 PIC X     VALUE LOW-VALUE.               
034200     03  611C-Z2                 PIC X     VALUE LOW-VALUE.               
034300     03  611C-TRANSKOD           PIC X(8)  VALUE 'W6T11CX '.              
034400     03  611C-IDTRANS            PIC X(4)  VALUE '611C'.                  
034500     03  611C-KDMFSFOR           PIC X.                                   
034600     03  FILLER.                                                          
034700*       05 -COPY W6I11C01    -PRE MOD611C-                                
034800     SKIP3                                                                
034900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
035000*                                                                         
035100     EJECT                                                                
035200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
035300     SKIP3                                                                
035400 01  NYCKLAR-TILL-DLI.                                                    
035500     03  W-IDARTNR-X.                                                     
035600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
035700     03  W-IDARTNR-WDK7-X.                                                
035800         05  W-IDARTNR-WDK7      PIC S9(9)   VALUE ZERO COMP-3.           
035900     03  W-IDDC-X.                                                        
036000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
036100     03  W-KDSEGKEY-X.                                                    
036200         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
036300     03  W-WDG6KEY-X.                                                     
036400         05  W-WDG6KEY           PIC X(18)    VALUE SPACE.                
036500     03  W-KDSEGKEY-X.                                                    
036600         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
036700     03  W-IDDC-B6-X.                                                     
036800         05 W-IDDC-B6            PIC X(2).                                
036900     03  W-IDLAND-X.                                                      
037000         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
037100     03  W-DAINLEV-X.                                                     
037200         05  W-DAINLEV           PIC 9(16).                               
037300                                                                          
037400     SKIP2                                                                
037500*    --- STATUS-KOD FRÅN IMS                                              
037600 01  STATUS-WS                   PIC XX.                                  
037700     88  SEGMENT-FINNS                       VALUE '  '.                  
037800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
037900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
038000     SKIP2                                                                
038100 01  GODK-STATUSKODER.                                                    
038200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
038300     SKIP3                                                                
038400 01  SSA1                        PIC X(64).                               
038500 01  SSA2                        PIC X(64).                               
038600 01  SSA3                        PIC X(128).                              
038700     EJECT                                                                
038800*01  -COPY WWDCKONS                                                       
038900     EJECT                                                                
039000*01  -COPY WWDC99                                                         
039100     EJECT                                                                
039200*    --- IMS FUNKTIONSKODER                                               
039300*01  -COPY W0003                                                          
039400     EJECT                                                                
039500*    ---  DLI INPUT-OUTPUT AREA                                           
039600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
039700     SKIP3                                                                
039800 01  DLI-IO-AREA.                                                         
039900     03  IO-AREA                 PIC X(1000)  VALUE SPACE.                
040000     SKIP3                                                                
040100     03  WLARTC01 REDEFINES IO-AREA.                                      
040200*        05  -COPY WDK601                                                 
040300     SKIP3                                                                
040400     03  WLARTC11 REDEFINES IO-AREA.                                      
040500*        05  -COPY WDK611                                                 
040600     EJECT                                                                
040700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
040800     SKIP3                                                                
040900 01  DLI-IO-AREA2.                                                        
041000     03  IO-AREA2                PIC X(600)  VALUE SPACE.                 
041100     SKIP3                                                                
041200 01  FILLER                     PIC X(16) VALUE 'AREA-WDK701'.            
041300 01  DLI-IO-AREA-WDK701.                                                  
041400*    03  -COPY WDK701                                                     
041500     EJECT                                                                
041600                                                                          
041700 01  FILLER                   PIC X(16) VALUE 'AREA-WDK711'.              
041800 01  DLI-IO-AREA-WDK711.                                                  
041900*    03  -COPY WDK711                                                     
042000     EJECT                                                                
042100                                                                          
042200 01  FILLER                   PIC X(16) VALUE 'AREA-WDK712'.              
042300 01  DLI-IO-AREA-WDK712.                                                  
042400*    03  -COPY WDK712                                                     
042500     EJECT                                                                
042600                                                                          
042700 01  FILLER                   PIC X(16) VALUE 'AREA-WDK728'.              
042800 01  DLI-IO-AREA-WDK728.                                                  
042900*    03  -COPY WDK728                                                     
043000     EJECT                                                                
043100                                                                          
043200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
043300 01   DLI-IO-AREA-B601.                                                   
043400*     03  -COPY WDB601                                                    
043500     EJECT                                                                
043600                                                                          
043700 LINKAGE SECTION.                                                         
043800                                                                          
043900*01  -COPY W0009  -PRE MSG-                                               
044000*01  -COPY W0009  -PRE DISP-                                              
044100*01  -COPY W0009  -PRE 611C-                                              
044200     EJECT                                                                
044300*01  -COPY W0008  -PRE ARTC-                                              
044400     05  FILLER                  PIC X.                                   
044500     EJECT                                                                
044600*01  -COPY W0008  -PRE WDK7-                                              
044700     05  FILLER                  PIC X.                                   
044800     EJECT                                                                
044900*01  -COPY W0008  -PRE WDB6-                                              
045000     05  FILLER                  PIC X.                                   
045100     EJECT                                                                
045200*01  -COPY W0008  -PRE SAPC-                                              
045300     05  FILLER                  PIC X.                                   
045400     EJECT                                                                
045500                                                                          
045600 PROCEDURE DIVISION  USING MSG-PCB  611C-PCB                              
045700                     ARTC-PCB WDK7-PCB WDB6-PCB                           
045800                     SAPC-PCB.                                            
045900                                                                          
046000                                                                          
046100     ENTRY 'DLITCBL' USING MSG-PCB  611C-PCB                              
046200                           ARTC-PCB WDK7-PCB WDB6-PCB                     
046300                           SAPC-PCB.                                      
046400                                                                          
046500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
046600     IF SUB-KDRC = 0                                                      
046700       PERFORM A-INIT                                                     
046800                                                                          
046900       IF REQU-KDPGMACT = 'S'                                             
047000         CONTINUE                                                         
047100       ELSE                                                               
047200         IF REQU-KDPGMACT = 'E'                                           
047300            PERFORM G-KOLLA-INPUT                                         
047400                                                                          
047500            IF INDATA-OK                                                  
047600               PERFORM H-SKAPA-SKICKA-TRANS                               
047700               MOVE ALL SPACE       TO RESP-WL0108O1                      
047800               MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                    
047900            END-IF                                                        
048000                                                                          
048100         ELSE                                                             
048200            MOVE ERR-EXEC-BUTTON-NOT-PRES TO RESP-IDMSG-ERROR             
048300            PERFORM S01-VISA-DEFAULT                                      
048400         END-IF                                                           
048500       END-IF                                                             
048600                                                                          
048700       IF RESP-IDELMT-ERROR = 'IDKONTO'                                   
048800         MOVE WS-IDKONTO     TO RESP-IDELMT-ERROR                         
048900       END-IF                                                             
049000       PERFORM S02-RETURN-RESPONSE                                        
049100     END-IF                                                               
049200                                                                          
049300     MOVE ZERO TO RETURN-CODE                                             
049400     GOBACK                                                               
049500     .                                                                    
049600     EJECT                                                                
049700 A-INIT SECTION.                                                          
049800     MOVE 'STA A-INIT        ' TO PGM-POS                                 
049900                                                                          
050000     MOVE ALL '+'              TO RESP-WL0108O1                           
050100     MOVE 001                  TO RESP-IDMSGVER                           
050200     MOVE SPACE                TO RESP-IDMSG-ERROR                        
050300                                  RESP-IDMSG-INFO                         
050400                                  RESP-IDELMT-ERROR                       
050500     MOVE REQU-IDDC-KEY TO W-IDDC                                         
050600                           RESP-IDDC-KEY                                  
050700                                                                          
050800     ACCEPT DAGENS-DATUM FROM DATE                                        
050900     ACCEPT DAGENS-TID   FROM TIME                                        
051000                                                                          
051100     MOVE W-IDDC               TO W-IDDC-B6                               
051200     PERFORM IMS-GU-WDB601                                                
051300     IF DCS-FLTRACK = 'J'                                                 
051400        MOVE 'J'  TO WS-FLTRACK                                           
051500     ELSE                                                                 
051600        MOVE 'N'  TO WS-FLTRACK                                           
051700     END-IF                                                               
051800                                                                          
051900     MOVE DCS-IDFTG            TO WS-IDFTG-B6                             
052000                                  WS-IDFTG                                
052100     MOVE DCS-IDLANDX2         TO W-IDLAND                                
052200     .                                                                    
052300     EJECT                                                                
052400 G-KOLLA-INPUT SECTION.                                                   
052500     MOVE 'STA G-KOLLA-INPUT'  TO PGM-POS                                 
052600     SKIP2                                                                
052700     PERFORM GA-KOLLA-BEH-RT-OCH-WDB6-DFLT                                
052800     IF INDATA-OK                                                         
052900       PERFORM GB-KOLLA-RADER                                             
053000     END-IF                                                               
053100     IF INDATA-OK                                                         
053200       IF DCS-IDFTG = WC-IDFTG-US OR WC-IDFTG-PV                          
053300         PERFORM GC-FORMELLA-KONTROLLER-EGEN                              
053400       ELSE                                                               
053500         PERFORM GC-FORMELLA-KONTROLLER-OVR                               
053600       END-IF                                                             
053700     END-IF                                                               
053800     MOVE 'END G-KOLLA-INPUT'  TO PGM-POS                                 
053900     .                                                                    
054000     EJECT                                                                
054100 GA-KOLLA-BEH-RT-OCH-WDB6-DFLT SECTION.                                   
054200* -- SÄTT VÄRDEN BEROENDE PÅ VILKEN RADIO-BUTTON SOM VALDES               
054300* -- OCH SIMULERA ATT VÄRDENA MATATS IN MANUELLT                          
054400     MOVE DCS-IDLEVNR-ALLA            TO WS-IDLEVNR                       
054500                                                                          
054600     EVALUATE REQU-KDBEH-KEY                                              
054700                                                                          
054800     WHEN PACKING-ADJUSTMENT                                              
054900                                                                          
055000         MOVE DCS-IDDISTR-JUST        TO WS-IDDISTR                       
055100         MOVE DCS-IDKUNDNR-JUST       TO WS-IDKUNDNR                      
055200         MOVE DCS-IDKONTO-JUST        TO WS-IDKONTO                       
055300         MOVE DCS-IDANALYS-JUST       TO WS-IDANALYS                      
055400         MOVE DCS-IDKST-JUST          TO WS-IDKST                         
055500                                                                          
055600     WHEN MIX-STOCK                                                       
055700         MOVE DCS-IDDISTR-MIX         TO WS-IDDISTR                       
055800         MOVE DCS-IDKUNDNR-MIX        TO WS-IDKUNDNR                      
055900         MOVE DCS-IDKONTO-MIX         TO WS-IDKONTO                       
056000         MOVE DCS-IDANALYS-MIX        TO WS-IDANALYS                      
056100         MOVE DCS-IDKST-MIX           TO WS-IDKST                         
056200                                                                          
056300     WHEN SCRAP                                                           
056400*--- NÄR DET ÄR INTERNSKROT OCH KINA DC SKALL MAN ANVÄNDA DISTRIKT        
056500*--- SCR-R (8497) ISTÄLLET FÖR SCR-Q. SUSSI 2012-04-03                    
056600         IF DCS-IDLANDX2 = 'CN'                                           
056700           MOVE DCS-IDDISTR-RSKROT    TO WS-IDDISTR                       
056800           MOVE DCS-IDKUNDNR-RSKROT   TO WS-IDKUNDNR                      
056900         ELSE                                                             
057000           MOVE DCS-IDDISTR-QSKROT    TO WS-IDDISTR                       
057100           MOVE DCS-IDKUNDNR-QSKROT   TO WS-IDKUNDNR                      
057200         END-IF                                                           
057300         MOVE DCS-IDKONTO-SKROT       TO WS-IDKONTO                       
057400         MOVE DCS-IDANALYS-SKROT      TO WS-IDANALYS                      
057500         MOVE DCS-IDKST-SKROT         TO WS-IDKST                         
057600                                                                          
057700     WHEN CORE-SCRAP                                                      
057800         IF DCS-IDLANDX2 = 'CN'                                           
057900**** from working storage                                                 
058000            MOVE DCS-IDDISTR-OSKROT-CN   TO WS-IDDISTR                    
058100            MOVE DCS-IDKUNDNR-OSKROT-CN  TO WS-IDKUNDNR                   
058200            MOVE DCS-IDKONTO-OSKROT-CN   TO WS-IDKONTO                    
058300            MOVE DCS-IDANALYS-OSKROT-CN  TO WS-IDANALYS                   
058400            MOVE DCS-IDKST-OSKROT-CN     TO WS-IDKST                      
058500         ELSE                                                             
058600            IF DCS-IDLANDX2 = 'KR'                                        
058700**** from working storage                                                 
058800               MOVE DCS-IDDISTR-OSKROT-KR   TO WS-IDDISTR                 
058900               MOVE DCS-IDKUNDNR-OSKROT-KR  TO WS-IDKUNDNR                
059000               MOVE DCS-IDKONTO-OSKROT-KR   TO WS-IDKONTO                 
059100               MOVE DCS-IDANALYS-OSKROT-KR  TO WS-IDANALYS                
059200               MOVE DCS-IDKST-OSKROT-KR     TO WS-IDKST                   
059300            ELSE                                                          
059400              IF DCS-IDLANDX2 = 'MY' OR 'TH' OR 'TW'                      
059500**** from working storage                                                 
059600                MOVE DCS-IDDISTR-OSKROT     TO WS-IDDISTR                 
059700                MOVE DCS-IDKUNDNR-OSKROT    TO WS-IDKUNDNR                
059800**** from working storage                                                 
059900**** FROM WDB6                                                            
060000                MOVE DCS-IDKONTO-SKROT      TO WS-IDKONTO                 
060100                MOVE DCS-IDANALYS-SKROT     TO WS-IDANALYS                
060200                MOVE DCS-IDKST-SKROT        TO WS-IDKST                   
060300**** FROM WDB6                                                            
060400              ELSE                                                        
060500**** from working storage                                                 
060600                MOVE DCS-IDDISTR-OSKROT     TO WS-IDDISTR                 
060700                MOVE DCS-IDKUNDNR-OSKROT    TO WS-IDKUNDNR                
060800                MOVE DCS-IDKONTO-OSKROT     TO WS-IDKONTO                 
060900                MOVE DCS-IDANALYS-OSKROT    TO WS-IDANALYS                
061000                MOVE DCS-IDKST-OSKROT       TO WS-IDKST                   
061100                  END-IF                                                  
061200            END-IF                                                        
061300         END-IF                                                           
061400                                                                          
061500     WHEN LOCAL-REMAN                                                     
061600         MOVE DCS-IDDISTR-LRENOV      TO WS-IDDISTR                       
061700         MOVE DCS-IDKUNDNR-LRENOV     TO WS-IDKUNDNR                      
061800         MOVE DCS-IDKONTO-LRENOV      TO WS-IDKONTO                       
061900         IF DCS-IDLANDX2 = 'CN'                                           
062000            MOVE DCS-IDANALYS-LRENOV-CN TO WS-IDANALYS                    
062100            MOVE DCS-IDKST-LRENOV-CN    TO WS-IDKST                       
062200         ELSE                                                             
062300            MOVE DCS-IDANALYS-LRENOV    TO WS-IDANALYS                    
062400            MOVE DCS-IDKST-LRENOV       TO WS-IDKST                       
062500         END-IF                                                           
062600                                                                          
062700     WHEN EXCHANGE-CONV                                                   
062800         MOVE DCS-IDDISTR-BKONV       TO WS-IDDISTR                       
062900         MOVE DCS-IDKUNDNR-BKONV      TO WS-IDKUNDNR                      
063000         MOVE DCS-IDKONTO-BKONV       TO WS-IDKONTO                       
063100         MOVE DCS-IDANALYS-BKONV      TO WS-IDANALYS                      
063200         MOVE DCS-IDKST-BKONV         TO WS-IDKST                         
063300                                                                          
063400     WHEN OTHER                                                           
063500        MOVE NEJ                      TO ALLT-SW                          
063600        MOVE NEJ                      TO INDATA-SW                        
063700        MOVE ERR-FIELD-IS-INVALID     TO RESP-IDMSG-ERROR                 
063800        MOVE 'KDBEHADJ'               TO RESP-IDELMT-ERROR                
063900                                                                          
064000     END-EVALUATE                                                         
064100                                                                          
064200     IF ALLT-OK                                                           
064300       IF REQU-IDFTG = ALL '+'                                            
064400         MOVE DCS-IDFTG                TO WS-IDFTG                        
064500       ELSE                                                               
064600         IF REQU-IDFTG   NUMERIC                                          
064700           MOVE REQU-IDFTG             TO WS-IDFTG                        
064800         ELSE                                                             
064900           MOVE NEJ                    TO ALLT-SW                         
065000           MOVE ERR-INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                
065100           MOVE 'IDFTG'                TO RESP-IDELMT-ERROR               
065200         END-IF                                                           
065300       END-IF                                                             
065400                                                                          
065500       IF REQU-IDKONTO NOT = ALL '+'                                      
065600         MOVE ZERO TO TALLY                                               
065700         INSPECT REQU-IDKONTO TALLYING TALLY                              
065800                         FOR CHARACTERS BEFORE INITIAL SPACE              
065900         MOVE REQU-IDKONTO(1:TALLY) TO WS-IDKONTO                         
066000       END-IF                                                             
066100                                                                          
066200       IF REQU-IDKST NOT = ALL '+'                                        
066300         MOVE REQU-IDKST TO WS-IDKST                                      
066400       END-IF                                                             
066500                                                                          
066600       IF REQU-IDANALYS NOT = ALL '+'                                     
066700         MOVE REQU-IDANALYS            TO WS-IDANALYS                     
066800         IF WS-IDANALYS = ZERO OR SPACE                                   
066900           MOVE NEJ                    TO ALLT-SW                         
067000           MOVE NEJ                    TO INDATA-SW                       
067100           MOVE ERR-INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                
067200           MOVE 'IDANALYS'             TO RESP-IDELMT-ERROR               
067300         END-IF                                                           
067400       END-IF                                                             
067500                                                                          
067600       IF REQU-KDRT = ALL '+'                                             
067700                                                                          
067800          IF REQU-KDBEH-KEY = PACKING-ADJUSTMENT OR                       
067900                              MIX-STOCK OR                                
068000                              SCRAP                                       
068100             IF REQU-KDBEH-KEY = PACKING-ADJUSTMENT                       
068200                MOVE DCS-KDRT-JUST       TO WS-KDRT                       
068300             END-IF                                                       
068400             IF REQU-KDBEH-KEY = MIX-STOCK                                
068500                MOVE DCS-KDRT-MIX        TO WS-KDRT                       
068600             END-IF                                                       
068700             IF REQU-KDBEH-KEY = SCRAP                                    
068800                MOVE DCS-KDRT-SKROT      TO WS-KDRT                       
068900             END-IF                                                       
069000          ELSE                                                            
069100             IF DCS-NDC-NA                                                
069200                MOVE 80                  TO WS-KDRT                       
069300             ELSE                                                         
069400                MOVE 6                   TO WS-KDRT                       
069500             END-IF                                                       
069600          END-IF                                                          
069700       ELSE                                                               
069800          IF DCS-NDC-NA                                                   
069900            IF  REQU-KDRT = '40' OR '41' OR '42' OR '43'                  
070000                       OR '44' OR '45' OR '80'                            
070100              MOVE REQU-KDRT              TO WS-KDRT                      
070200            ELSE                                                          
070300              MOVE NEJ                    TO ALLT-SW                      
070400              MOVE NEJ                    TO INDATA-SW                    
070500              MOVE ERR-INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR             
070600              MOVE 'KDRT'                 TO RESP-IDELMT-ERROR            
070700            END-IF                                                        
070800          ELSE                                                            
070900            MOVE REQU-KDRT                TO WS-KDRT                      
071000          END-IF                                                          
071100       END-IF                                                             
071200     END-IF                                                               
071300     .                                                                    
071400     EJECT                                                                
071500 GB-KOLLA-RADER SECTION.                                                  
071600     MOVE 'STA GA-KOLLA-RAD'   TO PGM-POS                                 
071700                                                                          
071800     MOVE +1 TO INDX INDX-TAB                                             
071900     MOVE JA        TO INDATA-SW                                          
072000     PERFORM UNTIL INDX > MAX-INDX-100                                    
072100       IF REQU-IDARTNR (INDX)       = ALL '+'                             
072200       AND REQU-IDAVINR (INDX)      = ALL '+'                             
072300       AND REQU-KVAVIS (INDX)       = ALL '+'                             
072400           IF INDX = +1                                                   
072500             IF REQU-KDPGMACT = 'E' AND INDX = +1                         
072600               MOVE NEJ TO INDATA-SW                                      
072700               MOVE '014'             TO RESP-IDMSG-ERROR                 
072800             END-IF                                                       
072900             MOVE NEJ TO INDATA-SW                                        
073000           END-IF                                                         
073100           MOVE +999 TO INDX                                              
073200       ELSE                                                               
073300        IF REQU-IDARTNR (INDX) NOT = ALL '+'                              
               MOVE REQU-IDARTNR(INDX)  TO W-IDARTNR                            
073400         IF REQU-IDAVINR (INDX) = ALL '+'                                 
073500             MOVE 'IDAVINR'        TO RESP-IDELMT-ERROR                   
073600             MOVE ERR-CORR-FIELDS  TO RESP-IDMSG-ERROR                    
073700                                      RESP-IDMSG-ERROR-LINE(INDX)         
073800             MOVE NEJ TO INDATA-SW                                        
073900         ELSE                                                             
074000             IF REQU-IDAVINR (INDX) NOT NUMERIC OR                        
074100                REQU-IDAVINR (INDX) = ZERO                                
074200                MOVE 'IDAVINR'       TO RESP-IDELMT-ERROR                 
074300                MOVE ERR-CORR-FIELDS TO RESP-IDMSG-ERROR                  
074400                                      RESP-IDMSG-ERROR-LINE(INDX)         
074500                MOVE NEJ TO INDATA-SW                                     
074600             END-IF                                                       
074700         END-IF                                                           
074800         IF REQU-KVAVIS (INDX) NOT = ALL '+'                              
074900            MOVE REQU-KVAVIS (INDX) TO DEC-IDFRIDATA                      
075000            MOVE 7                 TO DEC-KVHELTAL                        
075100            MOVE 0                 TO DEC-KVDECIMAL                       
075200                                                                          
075300            CALL WDECEDIT USING DEC-WDECAREA                              
075400                                                                          
075500            IF DEC-KDSVAR-OK                                              
075600               IF DEC-IDEDITDATA = ZERO                                   
075700                  MOVE 'KVANTAL'    TO RESP-IDELMT-ERROR                  
075800                  MOVE ERR-CORR-FIELDS TO RESP-IDMSG-ERROR                
075900                                      RESP-IDMSG-ERROR-LINE(INDX)         
076000                  MOVE NEJ TO INDATA-SW                                   
076100               ELSE                                                       
076200                  MOVE DEC-IDEDITDATA TO WS-KVAVIS (INDX)                 
076300               END-IF                                                     
076400            ELSE                                                          
076500              MOVE 'KVANTAL'        TO RESP-IDELMT-ERROR                  
076600              MOVE ERR-CORR-FIELDS TO RESP-IDMSG-ERROR                    
076700                                      RESP-IDMSG-ERROR-LINE(INDX)         
076800              MOVE NEJ         TO INDATA-SW                               
076900            END-IF                                                        
077000         ELSE                                                             
077100            MOVE 'KVANTAL'          TO RESP-IDELMT-ERROR                  
077200            MOVE ERR-CORR-FIELDS TO RESP-IDMSG-ERROR                      
077300                                      RESP-IDMSG-ERROR-LINE(INDX)         
077400            MOVE NEJ         TO INDATA-SW                                 
077500         END-IF                                                           
077600         IF REQU-IDARTNR-FROM (INDX) NOT = ALL '+'                        
077700           IF REQU-IDARTNR-FROM (INDX) NOT NUMERIC                        
077800             MOVE 'IDARTNR-FROM'  TO RESP-IDELMT-ERROR                    
077900             MOVE ERR-CORR-FIELDS TO RESP-IDMSG-ERROR                     
078000                                    RESP-IDMSG-ERROR-LINE(INDX)           
078100             MOVE NEJ TO INDATA-SW                                        
078200             MOVE REQU-IDARTNR-FROM (INDX)                                
078300               TO RESP-IDARTNR-FROM (INDX)                                
078400           END-IF                                                         
078500         ELSE                                                             
078600           IF REQU-IDARTNR-FROM (INDX)  = ALL '+'                         
078700             MOVE ZERO TO REQU-IDARTNR-FROM (INDX)                        
078800           END-IF                                                         
078900         END-IF                                                           
079000                                                                          
079100        END-IF                                                            
079200                                                                          
079300       END-IF                                                             
079400       ADD +1       TO INDX                                               
079500     END-PERFORM                                                          
079600     PERFORM IMS-GU-ARTC01                                                
           MOVE ART-KDPRODSL TO WS-KDPRODSL                                     
079700     IF WS-FLTRACK='J'                                                    
079800*       MOVE INDX-TAB TO MAX-TAB-INDX                                     
079900      IF WS-KDPRODSL-FDIGIT = 9                                           
             CONTINUE                                                           
            ELSE                                                                
080000       IF INDATA-OK                                                       
080100          MOVE +1 TO INDX                                                 
080200          MOVE JA        TO INDATA-SW                                     
080300          PERFORM UNTIL INDX > MAX-INDX-100                               
080400            IF REQU-IDARTNR(INDX) NOT = ALL '+'                           
080500              MOVE REQU-IDDC-KEY       TO W-IDDC                          
080600              MOVE REQU-IDARTNR(INDX)  TO W-IDARTNR                       
080700              PERFORM IMS-GU-WDK711                                       
080800              IF SEGMENT-SAKNAS                                           
080900                 MOVE NEJ TO INDATA-SW                                    
081000                 MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR             
081100                 MOVE ERR-IDELMT-MISSING TO RESP-IDMSG-ERROR              
081200                                  RESP-IDMSG-ERROR-LINE(INDX)             
081300              END-IF                                                      
081400            END-IF                                                        
081500          ADD +1       TO INDX                                            
081600          END-PERFORM                                                     
081700       END-IF                                                             
081800                                                                          
081900       IF INDATA-OK                                                       
082000          MOVE +1 TO INDX INDX-TAB                                        
082100          PERFORM UNTIL INDX > MAX-INDX-100                               
082200             IF REQU-IDARTNR (INDX) NOT = ALL '+'                         
082300                MOVE WS-KVAVIS (INDX)    TO TAB-KVAVIS(INDX-TAB)          
082400                MOVE REQU-IDARTNR (INDX) TO TAB-IDARTNR(INDX-TAB)         
082500                ADD +1       TO INDX INDX-TAB                             
082600             ELSE                                                         
082700                ADD +1       TO INDX                                      
082800             END-IF                                                       
082900          END-PERFORM                                                     
083000                                                                          
083100          SORT WS-SOR-DATA DESCENDING TAB-IDARTNR TAB-KVAVIS              
083200                                                                          
083300        MOVE +1 TO INDX-TAB                                               
083400        SET INDX-NEW-TAB TO 1                                             
083500                                                                          
083600        MOVE TAB-IDARTNR (INDX-TAB) TO                                    
083700                                    NEW-TAB-IDARTNR(INDX-NEW-TAB)         
083800        MOVE TAB-KVAVIS  (INDX-TAB) TO                                    
083900                             NEW-TAB-KVAVIS(INDX-NEW-TAB)                 
084000        ADD +1 TO INDX-TAB                                                
084100        PERFORM UNTIL INDX-TAB > MAX-INDX-100                             
084200         IF TAB-IDARTNR (INDX-TAB) =                                      
084300                             NEW-TAB-IDARTNR(INDX-NEW-TAB)                
084400           ADD TAB-KVAVIS (INDX-TAB) TO                                   
084500                             NEW-TAB-KVAVIS(INDX-NEW-TAB)                 
084600         ELSE                                                             
084700            PERFORM GBA-VALIDATE-WDK728                                   
084800              IF IDTRACK-QTY-DONE                                         
084900                 MOVE 'J' TO NEW-TAB-FLAG(INDX-NEW-TAB)                   
085000                 MOVE WS-VALD-KVAVIS TO                                   
085100                             NEW-TAB-KVAV(INDX-NEW-TAB)                   
085200              ELSE                                                        
085300                 MOVE 'N' TO NEW-TAB-FLAG(INDX-NEW-TAB)                   
085400                 MOVE WS-TEMP-KVAVIS TO                                   
085500                             NEW-TAB-KVAV(INDX-NEW-TAB)                   
085600              END-IF                                                      
085700                                                                          
085800              SET INDX-NEW-TAB UP BY 1                                    
085900              MOVE TAB-IDARTNR (INDX-TAB) TO                              
086000                   NEW-TAB-IDARTNR(INDX-NEW-TAB)                          
086100              MOVE TAB-KVAVIS (INDX-TAB) TO                               
086200                   NEW-TAB-KVAVIS(INDX-NEW-TAB)                           
086300                                                                          
086400         END-IF                                                           
086500         ADD +1 TO INDX-TAB                                               
086600        END-PERFORM                                                       
086700                                                                          
086800                                                                          
086900        MOVE +1 TO INDX                                                   
087000        SET INDX-NEW-TAB TO 1                                             
087100        MOVE JA        TO INDATA-SW                                       
087200        MOVE 'N' TO WS-NOT-FIRST-FL                                       
087300        PERFORM UNTIL INDX > MAX-INDX-100                                 
087400          SET INDX-NEW-TAB TO 1                                           
087500          IF REQU-IDARTNR (INDX) NOT = ALL'+'                             
087600          SEARCH WS-NEW-DATA                                              
087700            AT END CONTINUE                                               
087800           WHEN NEW-TAB-IDARTNR(INDX-NEW-TAB) =                           
087900                REQU-IDARTNR (INDX)                                       
088000              IF NEW-TAB-FLAG(INDX-NEW-TAB) = 'N'                         
088100                 IF WS-NOT-FIRST-FL = 'N'                                 
088200                    MOVE NEW-TAB-IDARTNR(INDX-NEW-TAB)                    
088300                                TO WS-TEMP-IDARTNR                        
088400                    MOVE SPACES TO RESP-IDELMT-ERROR                      
088500                    MOVE NEW-TAB-KVAV(INDX-NEW-TAB) TO                    
088600                                 WS-TEMP-KVAVIS-SHOW                      
088700                     STRING WS-TEMP-KVAVIS-SHOW                           
088800                     ' for marked '                                       
088900                     DELIMITED BY SIZE INTO RESP-IDELMT-ERROR             
089000                     MOVE '423'           TO RESP-IDMSG-ERROR             
089100                                   RESP-IDMSG-ERROR-LINE(INDX)            
089200                     MOVE NEJ             TO INDATA-SW                    
089300                     MOVE REQU-IDARTNR (INDX) TO WS-IDARTNR-FL            
089400                     MOVE 'J' TO WS-NOT-FIRST-FL                          
089500                 ELSE                                                     
089600                    IF REQU-IDARTNR (INDX) = WS-IDARTNR-FL                
089700                       MOVE '423'           TO RESP-IDMSG-ERROR           
089800                                    RESP-IDMSG-ERROR-LINE(INDX)           
089900                    ELSE                                                  
090000                       CONTINUE                                           
090100                    END-IF                                                
090200                                                                          
090300                 END-IF                                                   
090400              END-IF                                                      
090500          END-SEARCH                                                      
090600          END-IF                                                          
090700          ADD +1 TO INDX                                                  
090800        END-PERFORM                                                       
090900                                                                          
091000       END-IF                                                             
            END-IF                                                              
091100     END-IF                                                               
091200* KOLLA KONTO ANALYS KOST *                                               
091300     IF INDATA-OK                                                         
091400                                                                          
091500       IF DCS-NDC-NA                                                      
091600         CONTINUE                                                         
091700       ELSE                                                               
091800         MOVE WS-IDKONTO   TO SAP-IDKONTO                                 
091900         MOVE WS-IDKST     TO SAP-IDKST                                   
092000         MOVE WS-IDANALYS  TO SAP-IDANALYS                                
092100*                                                                         
092200*        IF IDFTG-PV                                                      
092300*          MOVE 'SEPV'           TO SAP-KDTRADP                           
092400*        ELSE                                                             
092500*          IF IDFTG-CN                                                    
092600*            MOVE 'CN05'         TO SAP-KDTRADP                           
092700*          ELSE                                                           
092800*            IF IDFTG-IN                                                  
092900*              MOVE 'IN07'       TO SAP-KDTRADP                           
093000*            ELSE                                                         
093100*              IF IDFTG-KR                                                
093200*                MOVE 'KR01'     TO SAP-KDTRADP                           
093300*              ELSE                                                       
093400*                MOVE 'SEPV'     TO SAP-KDTRADP                           
093500*              END-IF                                                     
093600*            END-IF                                                       
093700*          END-IF                                                         
093800*        END-IF                                                           
093900         IF IDFTG-NON-VCC                                                 
094000           PERFORM IMS-GU-WDB601-FTG                                      
094100           IF SEGMENT-FINNS                                               
094200             MOVE DCS-KDTRADP    TO SAP-KDTRADP                           
094300           END-IF                                                         
094400         ELSE                                                             
094500           MOVE 'SEPV'           TO SAP-KDTRADP                           
094600         END-IF                                                           
094700         MOVE ZERO               TO SAP-IDDISTR                           
094800         MOVE SPACE              TO SAP-KDFAKTYP                          
094900         MOVE ZERO               TO SAP-IDFTG                             
095000         MOVE SPACE              TO SAP-IDPROFIT                          
095100         MOVE +2                 TO SAP-KDCALL                            
095200                                                                          
095300         CALL W411SAP USING SAP-W411SAP SAPC-PCB                          
095400                                                                          
095500         IF SAP-BEFEL NOT = SPACE                                         
095600           MOVE NEJ                TO INDATA-SW                           
095700           IF SAP-IDKONTO-OK = NEJ                                        
095800             MOVE 'IDKONTO'       TO RESP-IDELMT-ERROR                    
095900             MOVE ERR-CORR-FIELDS TO RESP-IDMSG-ERROR                     
096000                                                                          
096100           ELSE                                                           
096200             IF SAP-IDANALYS-OK = NEJ                                     
096300               IF SAP-IDANALYS NOT = SPACE                                
096400                 MOVE 'IDANALYS'  TO RESP-IDELMT-ERROR                    
096500                 MOVE ERR-CORR-FIELDS TO RESP-IDMSG-ERROR                 
096600                                                                          
096700               END-IF                                                     
096800             ELSE                                                         
096900               IF SAP-IDKST-OK = NEJ                                      
097000                 IF SAP-IDKST NOT = SPACE                                 
097100                   MOVE 'IDKST' TO RESP-IDELMT-ERROR                      
097200                   MOVE ERR-CORR-FIELDS TO RESP-IDMSG-ERROR               
097300                                                                          
097400                 END-IF                                                   
097500               END-IF                                                     
097600             END-IF                                                       
097700             IF SAP-IDANALYS = SPACE AND                                  
097800                SAP-IDKST  = SPACE                                        
097900               MOVE '315'     TO RESP-IDMSG-ERROR                         
098000             END-IF                                                       
098100                                                                          
098200             IF SAP-IDFTG-OK = NEJ                                        
098300               MOVE 'IDFTG' TO RESP-IDELMT-ERROR                          
098400               MOVE ERR-CORR-FIELDS TO RESP-IDMSG-ERROR                   
098500             END-IF                                                       
098600                                                                          
098700           END-IF                                                         
098800         END-IF                                                           
098900       END-IF                                                             
099000     END-IF                                                               
099100                                                                          
099200     COMPUTE RESP-KVRADER = INDX - 1                                      
099300     END-COMPUTE                                                          
099400     MOVE 'END GA-KOLLA-RAD'   TO PGM-POS                                 
099500     .                                                                    
099600     EJECT                                                                
099700 GBA-VALIDATE-WDK728  SECTION.                                            
099800     MOVE ZERO                TO WS-VALD-KVAVIS                           
099900                                 WS-TEMP-KVAVIS                           
100000     MOVE 'N'                 TO IDTRACK-QTY-SW                           
100100     MOVE REQU-IDDC-KEY       TO W-IDDC                                   
100200     MOVE NEW-TAB-KVAVIS(INDX-NEW-TAB)                                    
100300                              TO WS-VALD-KVAVIS                           
100400     MOVE NEW-TAB-IDARTNR(INDX-NEW-TAB)                                   
100500                              TO W-IDARTNR                                
100600     PERFORM IMS-GU-WDK711                                                
100700     IF SEGMENT-SAKNAS                                                    
100800        MOVE NEJ TO INDATA-SW                                             
100900        MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                      
101000        MOVE ERR-IDELMT-MISSING TO RESP-IDMSG-ERROR                       
101100                      RESP-IDMSG-ERROR-LINE(INDX-NEW-TAB)                 
101200     ELSE                                                                 
101300        MOVE 9999999999999999    TO W-DAINLEV                             
101400        PERFORM IMS-GHNP-WDK728-LAST                                      
101500        PERFORM UNTIL SEGMENT-SAKNAS OR IDTRACK-QTY-DONE                  
101600          IF TRCK-KVTRACK-KVAR < TRCK-KVANTMOT                            
101700             COMPUTE WS-TEMP-KVAVIS = WS-TEMP-KVAVIS +                    
101800             (TRCK-KVANTMOT - TRCK-KVTRACK-KVAR)                          
101900             IF WS-TEMP-KVAVIS >= WS-VALD-KVAVIS                          
102000                MOVE 'J' TO IDTRACK-QTY-SW                                
102100             END-IF                                                       
102200          END-IF                                                          
102300          IF IDTRACK-QTY-NOT-DONE                                         
102400             PERFORM IMS-GU-WDK711                                        
102500             MOVE  TRCK-DAINLEV TO W-DAINLEV                              
102600             PERFORM IMS-GHNP-WDK728-LAST                                 
102700          END-IF                                                          
102800        END-PERFORM                                                       
102900     END-IF                                                               
103000     .                                                                    
103100     EJECT                                                                
103200 GC-FORMELLA-KONTROLLER-EGEN SECTION.                                     
103300     MOVE 'STA GC-FORMELLA E'    TO PGM-POS                               
103400     MOVE +1 TO INDX                                                      
103500     MOVE +1 TO INDX-TAB                                                  
103600     PERFORM UNTIL INDX > MAX-INDX-100                                    
103700     OR INDATA-FEL                                                        
103800     OR (REQU-IDARTNR (INDX)     = ALL '+' AND                            
103900         REQU-IDAVINR (INDX)     = ALL '+' AND                            
104000         REQU-KVAVIS (INDX)       = ALL '+')                              
104100                                                                          
104200         MOVE REQU-IDARTNR (INDX) TO W-IDARTNR                            
104300                              W-IDARTNR-WDK7                              
104400         MOVE NEJ TO WDK7-SAKNAS-SW                                       
104500                                                                          
104600         PERFORM IMS-GU-WDK711                                            
104700         IF SEGMENT-SAKNAS                                                
104800            PERFORM IMS-GU-ARTC01                                         
104900            IF SEGMENT-SAKNAS                                             
105000              MOVE NEJ TO INDATA-SW                                       
105100              MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                
105200              MOVE ERR-IDELMT-MISSING TO RESP-IDMSG-ERROR                 
105300                                       RESP-IDMSG-ERROR-LINE(INDX)        
105400            ELSE                                                          
105500              PERFORM IMS-GNP-ARTC11                                      
105600              IF SEGMENT-SAKNAS                                           
105700                MOVE NEJ TO INDATA-SW                                     
105800                MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR              
105900                MOVE ERR-IDELMT-MISSING TO RESP-IDMSG-ERROR               
106000                                       RESP-IDMSG-ERROR-LINE(INDX)        
106100              ELSE                                                        
106200*** ARTIKEL FINNS PÅ WDK6 MEN INTE PÅ WDK7 ****                           
106300*** LÄGGA UPP NY ART PÅ WDK7 ***                                          
106400                PERFORM IMS-GU-WDK711                                     
106500                IF SEGMENT-SAKNAS                                         
106600                  MOVE JA  TO WDK7-SAKNAS-SW                              
106700                END-IF                                                    
106800              END-IF                                                      
106900            END-IF                                                        
107000          END-IF                                                          
107100                                                                          
107200          IF INDATA-OK                                                    
107300             MOVE REQU-IDARTNR(INDX) TO W-IDARTNR                         
107400             PERFORM IMS-GU-ARTC01                                        
107500             IF SEGMENT-SAKNAS                                            
107600               MOVE 'IDARTNR'       TO RESP-IDELMT-ERROR                  
107700               MOVE ERR-IDELMT-MISSING TO RESP-IDMSG-ERROR                
107800                                      RESP-IDMSG-ERROR-LINE(INDX)         
107900               MOVE NEJ    TO INDATA-SW                                   
108000             ELSE                                                         
108100                                                                          
108200               IF ART-KDERS-UTG > 20                                      
108300                 MOVE 'IDARTNR'     TO RESP-IDELMT-ERROR                  
108400                 MOVE ERR-PART-NO-IS-OBSOLETE TO RESP-IDMSG-ERROR         
108500                                      RESP-IDMSG-ERROR-LINE(INDX)         
108600                 MOVE NEJ    TO INDATA-SW                                 
108700               ELSE                                                       
108800                 PERFORM IMS-GNP-ARTC11                                   
108900                 IF SEGMENT-SAKNAS                                        
109000                   MOVE 'IDARTNR'   TO RESP-IDELMT-ERROR                  
109100                   MOVE ERR-IDELMT-MISSING TO RESP-IDMSG-ERROR            
109200                                      RESP-IDMSG-ERROR-LINE(INDX)         
109300                   MOVE NEJ    TO INDATA-SW                               
109400                 ELSE                                                     
109500                   IF CLAG-PRARTSTD = +0                                  
109600                     MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                  
109700                     MOVE ERR-PRICE-MISSING TO RESP-IDMSG-ERROR           
109800                                      RESP-IDMSG-ERROR-LINE(INDX)         
109900                     MOVE NEJ    TO INDATA-SW                             
110000                   ELSE                                                   
110100                     PERFORM GCD-KOLLA-VIKT-VOLYM-URSPR                   
110200                   END-IF                                                 
110300                 END-IF                                                   
110400               END-IF                                                     
110500             END-IF                                                       
110600         END-IF                                                           
110700         IF INDATA-OK AND WDK7-SAKNAS                                     
110800           MOVE ALL '+'       TO WDK7-W005WDK7                            
110900           MOVE 'WDK711'      TO WDK7-IDSEGM                              
111000           MOVE W-IDARTNR     TO WDK7-IDARTNR-KFB                         
111100           MOVE REQU-IDDC-KEY TO WDK7-IDDC-KFB                            
111200                                 WDK7-IDDC                                
111300                                 WS-IDDC                                  
111400           IF NDC-US-BAT                                                  
111500              MOVE NEJ        TO WDK7-FLREFILL                            
111600           END-IF                                                         
111700           CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                     
111800                                             ARTC-PCB WDK7-PCB            
111900         END-IF                                                           
112000         ADD +1 TO INDX                                                   
112100     END-PERFORM                                                          
112200     MOVE 'END GC-FORMELLA E'  TO PGM-POS                                 
112300     .                                                                    
112400     EJECT                                                                
112500 GC-FORMELLA-KONTROLLER-OVR SECTION.                                      
112600     MOVE 'STA GC-FORMELLA O'  TO PGM-POS                                 
112700     SKIP2                                                                
112800                                                                          
112900     MOVE +1 TO INDX                                                      
113000     PERFORM UNTIL INDX > MAX-INDX-100                                    
113100     OR INDATA-FEL                                                        
113200     OR (REQU-IDARTNR (INDX)     = ALL '+' AND                            
113300         REQU-IDAVINR (INDX)     = ALL '+' AND                            
113400         REQU-KVAVIS (INDX)       = ALL '+')                              
113500                                                                          
113600         MOVE REQU-IDARTNR (INDX) TO W-IDARTNR                            
113700                                    W-IDARTNR-WDK7                        
113800                                                                          
113900         PERFORM IMS-GU-WDK711                                            
114000         IF SEGMENT-SAKNAS                                                
114100            MOVE NEJ TO INDATA-SW                                         
114200            MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                  
114300            MOVE ERR-IDELMT-MISSING TO RESP-IDMSG-ERROR                   
114400                                      RESP-IDMSG-ERROR-LINE(INDX)         
114500         ELSE                                                             
114600           IF SLAG-PRAVCOST = +0                                          
114700              MOVE NEJ TO INDATA-SW                                       
114800              MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                 
114900              MOVE ERR-PRICE-MISSING TO RESP-IDMSG-ERROR                  
115000                                       RESP-IDMSG-ERROR-LINE(INDX)        
115100           END-IF                                                         
115200         END-IF                                                           
115300                                                                          
115400         IF INDATA-OK                                                     
115500           MOVE REQU-IDARTNR(INDX) TO W-IDARTNR                           
115600           PERFORM IMS-GU-ARTC01                                          
115700           IF SEGMENT-SAKNAS                                              
115800             MOVE 'IDARTNR'       TO RESP-IDELMT-ERROR                    
115900             MOVE ERR-IDELMT-MISSING TO RESP-IDMSG-ERROR                  
116000                                    RESP-IDMSG-ERROR-LINE(INDX)           
116100             MOVE NEJ    TO INDATA-SW                                     
116200           ELSE                                                           
116300                                                                          
116400             IF ART-KDERS-UTG > 20                                        
116500               MOVE 'IDARTNR'     TO RESP-IDELMT-ERROR                    
116600               MOVE ERR-PART-NO-IS-OBSOLETE TO RESP-IDMSG-ERROR           
116700                                    RESP-IDMSG-ERROR-LINE(INDX)           
116800               MOVE NEJ    TO INDATA-SW                                   
116900             ELSE                                                         
117000               PERFORM IMS-GNP-ARTC11                                     
117100               IF SEGMENT-SAKNAS                                          
117200                 MOVE 'IDARTNR'   TO RESP-IDELMT-ERROR                    
117300                 MOVE ERR-IDELMT-MISSING TO RESP-IDMSG-ERROR              
117400                                    RESP-IDMSG-ERROR-LINE(INDX)           
117500                 MOVE NEJ    TO INDATA-SW                                 
117600               ELSE                                                       
117700                 IF CLAG-PRARTSTD = +0                                    
117800                   MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                    
117900                   MOVE ERR-PRICE-MISSING TO RESP-IDMSG-ERROR             
118000                                    RESP-IDMSG-ERROR-LINE(INDX)           
118100                   MOVE NEJ    TO INDATA-SW                               
118200                 ELSE                                                     
118300                   PERFORM GCD-KOLLA-VIKT-VOLYM-URSPR                     
118400                 END-IF                                                   
118500               END-IF                                                     
118600             END-IF                                                       
118700           END-IF                                                         
118800         END-IF                                                           
118900                                                                          
119000         ADD +1 TO INDX                                                   
119100     END-PERFORM                                                          
119200     MOVE 'END GC-FORMELLA O'  TO PGM-POS                                 
119300     .                                                                    
119400     EJECT                                                                
119500 GCD-KOLLA-VIKT-VOLYM-URSPR SECTION.                                      
119600     MOVE 'STA GCD-KOLLA-V'    TO PGM-POS                                 
119700                                                                          
119800     PERFORM IMS-GU-WDK712                                                
119900     IF SEGMENT-FINNS                                                     
120000       MOVE LART-VKART     TO WS-VKART-K7                                 
120100       MOVE LART-VLARTNTO  TO WS-VLARTNTO-K7                              
120200       MOVE LART-KDARTURS  TO WS-KDARTURS-K7                              
120300     ELSE                                                                 
120400       MOVE ZERO           TO WS-VKART-K7                                 
120500                              WS-VLARTNTO-K7                              
120600       MOVE SPACE          TO WS-KDARTURS-K7                              
120700     END-IF                                                               
120800                                                                          
120900     IF CLAG-VKART  = ZERO AND                                            
121000        WS-VKART-K7 = ZERO                                                
121100       MOVE 'VKART'                    TO RESP-IDELMT-ERROR               
121200       MOVE ERR-PART-WEIGHT-IS-MISSING TO RESP-IDMSG-ERROR                
121300                                      RESP-IDMSG-ERROR-LINE(INDX)         
121400       MOVE NEJ TO INDATA-SW                                              
121500     ELSE                                                                 
121600       IF CLAG-VLARTNTO  = ZERO AND                                       
121700          WS-VLARTNTO-K7 = ZERO                                           
121800         MOVE 'VKARTNTO'              TO RESP-IDELMT-ERROR                
121900         MOVE ERR-PART-VOLUME-MISSING TO RESP-IDMSG-ERROR                 
122000                                      RESP-IDMSG-ERROR-LINE(INDX)         
122100         MOVE NEJ TO INDATA-SW                                            
122200       ELSE                                                               
122300         IF CLAG-KDARTURS  = SPACE AND                                    
122400            WS-KDARTURS-K7 = SPACE                                        
122500           MOVE 'KDARTURS'              TO RESP-IDELMT-ERROR              
122600           MOVE ERR-PART-ORIGIN-MISSING TO RESP-IDMSG-ERROR               
122700                                      RESP-IDMSG-ERROR-LINE(INDX)         
122800           MOVE NEJ TO INDATA-SW                                          
122900         END-IF                                                           
123000       END-IF                                                             
123100     END-IF                                                               
123200     MOVE 'END GCD-KOLLA-V'    TO PGM-POS                                 
123300     .                                                                    
123400     EJECT                                                                
123500 H-SKAPA-SKICKA-TRANS SECTION.                                            
123600     MOVE 'STA H-SKAPA-SKI'    TO PGM-POS                                 
123700                                                                          
123800     MOVE ALL '+' TO MOD611C-MID-W6I11C01                                 
123900     MOVE +1 TO INDX                                                      
124000     PERFORM UNTIL INDX > MAX-INDX-100                                    
124100     OR (REQU-IDARTNR (INDX)     = ALL '+' AND                            
124200         REQU-IDAVINR (INDX)     = ALL '+' AND                            
124300         REQU-KVAVIS (INDX)       = ALL '+')                              
124400                                                                          
124500         ADD +1         TO 611C-IX                                        
124600         MOVE W-IDDC    TO MOD611C-MID-IDDC                               
124700                                                                          
124800         MOVE WS-IDLEVNR   TO                                             
124900                          MOD611C-MID-IDLEVNR (611C-IX)                   
125000         MOVE REQU-IDAVINR (INDX) TO                                      
125100                          MOD611C-MID-IDAVINR (611C-IX)                   
125200         MOVE DAGENS-DATUM TO                                             
125300                          MOD611C-MID-TIAVIDAT (611C-IX)                  
125400         MOVE REQU-IDARTNR (INDX) TO                                      
125500                          MOD611C-MID-IDARTNR (611C-IX)                   
125600         MOVE WS-KVAVIS (INDX)   TO                                       
125700                          MOD611C-MID-KVAVIS (611C-IX)                    
125800         MOVE WS-KDRT          TO MOD611C-MID-KDRT (611C-IX)              
125900                                                                          
126000******* HÄMTADE VÄRDEN ELLER INMATADE VÄRDEN ******                       
126100         MOVE WS-IDKONTO     TO MOD611C-MID-IDKONTO (611C-IX)             
126200         MOVE WS-IDANALYS    TO MOD611C-MID-IDANALYS(611C-IX)             
126300         MOVE WS-IDKST       TO MOD611C-MID-IDKST(611C-IX)                
126400                                                                          
126500******** ADAPT DATE AND TIME FOR TIMEZONES                                
126600         MOVE '011'                TO MSGI-KDCALL                         
126700         MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                       
126700         MOVE DCS-IDDC             TO MSGI-IDDC                           
126800         MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                       
126900         MOVE DAGENS-TID           TO MSGI-TILOKTID                       
127000         CALL WL01TIDZ USING          MSGI-WL01TIDZ                       
127100           MOVE MSGI-TILOKDAT(1:6) TO DAGENS-DATUM                        
127200           MOVE MSGI-TILOKTID(1:4) TO DAGENS-TID(1:4)                     
127300********                                                                  
127400         IF REQU-IDARTNR-FROM (INDX) = ZERO                               
127500             MOVE DAGENS-DATUM  TO DAT-I-TIDATUM                          
127600             MOVE 'AAMMDD'      TO DAT-KDDATFORM                          
127700             CALL WDATKONV USING   DAT-KDDATFORM                          
127800                                   DAT-I-TIDATUM                          
127900                                   DAT-O-TIDATUM                          
128000                                   DAT-KDSVAR                             
128100             IF DAT-KDSVAR-OK                                             
128200                 MOVE DAT-TIVV  TO WS-VV                                  
128300                 MOVE DAT-TID   TO WS-D                                   
128400             ELSE                                                         
128500                 MOVE ZERO TO WS-VV                                       
128600                              WS-D                                        
128700             END-IF                                                       
128800                 MOVE WS-LOPNR-R34 TO                                     
128900                               MOD611C-MID-IDARTNR-FROM (INDX)            
129000         ELSE                                                             
129100             MOVE REQU-IDARTNR-FROM (INDX) TO                             
129200                          MOD611C-MID-IDARTNR-FROM (611C-IX)              
129300         END-IF                                                           
129400         IF 611C-IX = 611C-MAX                                            
129500            PERFORM S04-STARTA-R34-TRANS                                  
129600            MOVE +0 TO 611C-IX                                            
129700         END-IF                                                           
129800        ADD +1 TO INDX                                                    
129900     END-PERFORM                                                          
130000                                                                          
130100                                                                          
130200     IF 611C-IX > +0                                                      
130300     AND 611C-IX < 611C-MAX                                               
130400         PERFORM S04-STARTA-R34-TRANS                                     
130500     END-IF                                                               
130600     MOVE 'END H-SKAPA-SKI'    TO PGM-POS                                 
130700     .                                                                    
130800     EJECT                                                                
130900 S01-VISA-DEFAULT SECTION.                                                
131000     MOVE 'STA S01-VISA-DE'    TO PGM-POS                                 
131100     SKIP2                                                                
131200     MOVE +1 TO INDX                                                      
131300     MOVE 'END S01-VISA-DE'    TO PGM-POS                                 
131400     .                                                                    
131500     EJECT                                                                
131600 S04-STARTA-R34-TRANS  SECTION.                                           
131700     MOVE 'STA S04-STARTA'     TO PGM-POS                                 
131800                                                                          
131900                                                                          
132000     MOVE IDPGM              TO MOD611C-MID-IDPGM                         
132100     MOVE 611C-IX            TO MOD611C-MID-KVPOST                        
132200     IF REQU-KDPGMACT = 'E'                                               
132300       MOVE 'J'              TO MOD611C-MID-FLSVS                         
132400     ELSE                                                                 
132500       MOVE 'N'              TO MOD611C-MID-FLSVS                         
132600     END-IF                                                               
132700     COMPUTE 611C-LL = LENGTH OF 611C-MSG-IO-AREA                         
132800                                                                          
132900     PERFORM IMS-ISRT-MSG-ALT-611C                                        
133000                                                                          
133100     MOVE +0                   TO 611C-IX                                 
133200     MOVE 'END S04-STARTA'     TO PGM-POS                                 
133300     .                                                                    
133400     EJECT                                                                
133500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
133600     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
133700                                                                          
133800     MOVE 'GETARG'               TO SUB-KDFUNC                            
133900     MOVE 'CARPARTS.LDC.R34REGISTRATION'   TO SUB-ADDISPABS               
134000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
134100                                                                          
134200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
134300                                                                          
134400     IF SUB-KDRC > 0                                                      
134500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
134600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
134700       DELIMITED BY SIZE INTO FELTEXT                                     
134800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
134900     END-IF                                                               
135000     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
135100     .                                                                    
135200     SKIP3                                                                
135300 S02-RETURN-RESPONSE SECTION.                                             
135400     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
135500                                                                          
135600     MOVE 'RETURN'                   TO SUB-KDFUNC                        
135700     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
135800                                                                          
135900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
136000                                                                          
136100     IF SUB-KDRC > 0                                                      
136200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
136300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
136400       DELIMITED BY SIZE INTO FELTEXT                                     
136500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
136600     END-IF                                                               
136700     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
136800     .                                                                    
136900     EJECT                                                                
137000                                                                          
137100* --- IMS SEKTIONER ---                                                   
137200     SKIP3                                                                
137300 IMS-ISRT-MSG-ALT-611C SECTION.                                           
137400     MOVE 'IMS-ISRT-MSG-ALT-611CSE        ' TO PGM-POS                    
137500     SKIP2                                                                
137600     MOVE SPACE TO GODK-STATUSKODER                                       
137700     CALL CBLTDLI   USING  ISRT                                           
137800                           611C-PCB                                       
137900                           611C-MSG-IO-AREA                               
138000     MOVE 611C-STATUS-CODE TO STATUS-WS                                   
138100     PERFORM IMS-STATUSKONTROLL                                           
138200     .                                                                    
138300     EJECT                                                                
138400 IMS-GU-ARTC01   SECTION.                                                 
138500     MOVE 'IMS-GU-ARTC01                  ' TO PGM-POS                    
138600                                                                          
138700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
138800          DELIMITED BY SIZE INTO SSA1                                     
138900     MOVE '  GE' TO GODK-STATUSKODER                                      
139000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
139100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
139200     PERFORM IMS-STATUSKONTROLL                                           
139300     .                                                                    
139400     EJECT                                                                
139500 IMS-GNP-ARTC11   SECTION.                                                
139600     MOVE 'IMS-GNP-ARTC01                ' TO PGM-POS                     
139700                                                                          
139800     MOVE 'WLARTC11 ' TO SSA1                                             
139900     MOVE '  GE' TO GODK-STATUSKODER                                      
140000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
140100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
140200     PERFORM IMS-STATUSKONTROLL                                           
140300     .                                                                    
140400     EJECT                                                                
140500 IMS-GU-WDK711   SECTION.                                                 
140600     MOVE 'IMS-GU-WDK711                 ' TO PGM-POS                     
140700                                                                          
140800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
140900          DELIMITED BY SIZE INTO SSA1                                     
141000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
141100          DELIMITED BY SIZE INTO SSA2                                     
141200     MOVE '  GE' TO GODK-STATUSKODER                                      
141300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
141400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
141500     PERFORM IMS-STATUSKONTROLL                                           
141600     .                                                                    
141700     EJECT                                                                
141800 IMS-GU-WDK712   SECTION.                                                 
141900     MOVE 'IMS-GU-WDK712                 ' TO PGM-POS                     
142000                                                                          
142100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
142200          DELIMITED BY SIZE INTO SSA1                                     
142300     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
142400          DELIMITED BY SIZE INTO SSA2                                     
142500     MOVE '  GE' TO GODK-STATUSKODER                                      
142600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
142700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
142800     PERFORM IMS-STATUSKONTROLL                                           
142900     .                                                                    
143000     EJECT                                                                
143100 IMS-GHNP-WDK728-LAST SECTION.                                            
143200                                                                          
143300     STRING 'WDK728  *L(DAINLEV < ' W-DAINLEV-X ')'                       
143400          DELIMITED BY SIZE INTO SSA1                                     
143500     MOVE '  GE' TO GODK-STATUSKODER                                      
143600     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-AREA-WDK728 SSA1             
143700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
143800     PERFORM IMS-STATUSKONTROLL                                           
143900     .                                                                    
144000     EJECT                                                                
144100 IMS-GU-WDB601    SECTION.                                                
144200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
144300          DELIMITED BY SIZE INTO SSA1                                     
144400     MOVE '  ' TO GODK-STATUSKODER                                        
144500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
144600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
144700     PERFORM IMS-STATUSKONTROLL                                           
144800     .                                                                    
144900     SKIP3                                                                
145000 IMS-GU-WDB601-FTG SECTION.                                               
145100     STRING 'WDB601  (IDFTG    =' WS-IDFTG-B6 ')'                         
145200            DELIMITED BY SIZE INTO SSA1                                   
145300     MOVE '  GE'                 TO GODK-STATUSKODER                      
145400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
145500     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
145600     PERFORM IMS-STATUSKONTROLL                                           
145700     .                                                                    
145800     SKIP3                                                                
145900                                                                          
146000 IMS-STATUSKONTROLL SECTION.                                              
146100     SET STATUS-IX TO 1                                                   
146200     SEARCH GODK-STATUS                                                   
146300       AT END                                                             
146400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
146500         DELIMITED BY SIZE INTO FELTEXT                                   
146600         CALL FELLOG                                                      
146700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
146800         CONTINUE                                                         
146900     END-SEARCH                                                           
147000     .                                                                    
