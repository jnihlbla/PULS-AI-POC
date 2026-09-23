000101*                                                                         
000201******************************************************************        
000301*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0102      *        
000401******************************************************************        
000501 ID DIVISION.                                                             
000601 PROGRAM-ID.     W6030200.                                                
000701 AUTHOR.         BO LINDAHL, FRONTEC.                                     
000801 DATE-WRITTEN.   94/12/21.                                                
000901 DATE-COMPILED.                                                           
001001                                                                          
001101*    FUNKTION: (TILLÄGG )                                                 
001201*        KOMPLETTERAT AV SUSANNE OLSSON, 1997                             
001301*        GENERELL PROGRAMKOD FÖR UPPLÄGG AV SALDOLOGG                     
001401*        I DATABAS WDL9/WLLOGA                                            
001501*                                                                         
001601*        ÄNDRAT FÖR ATT KLARA LOCAL CURRENCY I NORDAMERIKA                
001701*        AV SUSANNE OLSSON , OKTOBER 2004.                                
001801*                                                                         
001901*                                                                         
002001*    FUNKTION:                                                            
002101*        NDC/SDC GODSMOTTAGNING                                           
002201*                                                                         
002301*        PROGRAMMET LÄSER      WLINLD (WDL6)                              
002401*                              WLGMTB (WDB3)                              
002501*                              WLARTC (WDK6)                              
002601*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
002701*                              WLINLC (WDL6)                              
002801*                              WLFILB (WDR8)                              
002901*                              WLKOMA (WDP8)                              
003001*                              WL6301 (WDR5)                              
003101*                              WL6305 (WDR5)                              
003201*                                                                         
003301*    INDATA.                                                              
003401*        TRANSAKTION: W6T302                                              
003501*        MID:         W6I30201                                            
003601*                                                                         
003701*    UTDATA.                                                              
003801*        MOD:         W6O30201                                            
003901                                                                          
004001     SKIP3                                                                
004101 ENVIRONMENT DIVISION.                                                    
004201     EJECT                                                                
004301 DATA DIVISION.                                                           
004401 WORKING-STORAGE SECTION.                                                 
004501                                                                          
004601*    -- CHECKED BY WY2000                                                 
004701 77  IDPGM                       PIC X(08)   VALUE 'W6030200'.            
004801                                                                          
004901*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005001 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005101                                                                          
005201 77  JA                          PIC X       VALUE 'J'.                   
005301 77  NEJ                         PIC X       VALUE 'N'.                   
005401 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005501 77  SPAR-WS-IDDC                PIC X(2)    VALUE SPACE.                 
005601 77  SEND-WS-IDDC                PIC X(2)    VALUE SPACE.                 
005701 77  FEL-IDDC                    PIC X       VALUE SPACE.                 
005801                                                                          
005901*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
006001 77  WS-LOGG-AAAAMMDD            PIC 9(8)    VALUE ZERO.                  
006101 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
006201 77  WS-SAP-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
006301 77  WS-SAP-TTMMSSTH             PIC 9(8)    VALUE ZERO.                  
006401 77  W-TID                       PIC 9(8)    VALUE ZERO.                  
006501 77  NOLL-RAKNARE                PIC S9(5)   VALUE ZERO COMP-3.           
006601                                                                          
006701*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006801 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006901 77  INDX-2                      PIC S9(4)  VALUE +0    COMP SYNC.        
007001 77  INDX-3                      PIC S9(4)  VALUE +0    COMP SYNC.        
007101 77  BIN-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
007201 77  WS-INDX                     PIC S9(4)  VALUE +0    COMP SYNC.        
007301 77  PER-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
007401 77  ORAD-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
007501 77  ORAD-IX-MAX                 PIC S9(4)  VALUE +5    COMP SYNC.        
007601 77  URV-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
007701 77  URV-IX-MAX                  PIC S9(4)  VALUE +12   COMP SYNC.        
007801 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
007901 77  4341-LAENGD                 PIC S9(4)  VALUE +85   COMP SYNC.        
008001 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
008101 77  WS-KDSORT                   PIC X(2)   VALUE SPACE.                  
008201 77  WS-IDUSER-003               PIC X(5)   VALUE SPACE.                  
008301 77  WS-IDLTERM                  PIC X(8)   VALUE SPACE.                  
008401 77  WS-PRAVCOST                 PIC S9(7)V9(2)                           
008501                                            VALUE ZERO COMP-3.            
008600                                                                          
008700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008800     88  INDATA-OK                           VALUE 'J'.                   
008900     88  INDATA-FEL                          VALUE 'N'.                   
009000                                                                          
009100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009200     88  NYCKLAR-OK                          VALUE 'J'.                   
009300     88  NYCKLAR-FEL                         VALUE 'N'.                   
009400                                                                          
009500 77  HOPP-SW                     PIC X       VALUE 'N'.                   
009600     88  HOPP                                VALUE 'J'.                   
009700     88  EJ-HOPP                             VALUE 'N'.                   
009800                                                                          
009900 77  HOPP-UPDATE-SW              PIC X       VALUE 'N'.                   
010000     88  HOPP-UPDATE                         VALUE 'J'.                   
010100     88  EJ-HOPP-UPDATE                      VALUE 'N'.                   
010200                                                                          
010301 77  AKTUELLT-LAND               PIC X       VALUE 'N'.                   
010401     88  AKTUELLT-LAND-USA                   VALUE 'J'.                   
010501     88  AKTUELLT-EJ-USA                     VALUE 'N'.                   
010601                                                                          
010700 77  WS-PROD                     PIC X       VALUE 'N'.                   
010800     88 PRODKOD-SAKNAS                       VALUE 'N'.                   
010900     88 PRODKOD-FINNS                        VALUE 'J'.                   
011000                                                                          
011100 77  PRINTNING-SW                PIC X       VALUE 'N'.                   
011200     88  PRINTNING-BEGARD                    VALUE 'J'.                   
011300                                                                          
011400 77  TRANS-OHUVUD-RET-SKAPAD-SW  PIC X       VALUE 'N'.                   
011500     88  TRANS-OHUVUD-RET-SKAPAD             VALUE 'J'.                   
011600                                                                          
011700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011800     88  EGEN-MID                            VALUE '6302'.                
011900     88  GODK-MID                            VALUE '6302'.                
012000     88  HOPP-MID                            VALUE '6301'.                
012100     88  HELP-MID                            VALUE '0551'.                
012200     88  LAS-MID                             VALUE '630C'.                
012300                                                                          
012400 01  TEST-IDDISTR                PIC S9(5) COMP-3.                        
012500*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
012600     EJECT                                                                
012700                                                                          
012800*    --- GENERELLA ARBETSAREAOR.                                          
012900 01  W.                                                                   
013000     05  W-KVANT-UPD         PIC S9(3).                                   
013100     05  W-DATUM-X.                                                       
013200         10  W-DATUM         PIC 9(6).                                    
013300                                                                          
013400     05  W-TIME-X.                                                        
013500         10  W-TIME-TT       PIC 9(2).                                    
013600         10  FILLER          PIC 9(2).                                    
013700         10  W-TIME-SS       PIC 9(2).                                    
013800         10  FILLER          PIC 9(2).                                    
013900     05  W-TIME-N            REDEFINES W-TIME-X                           
014000                             PIC 9(8).                                    
014100     03  AKTUELL-TID.                                                     
014200         05  AKTUELL-TTMM    PIC 9(4).                                    
014300         05  FILLER          PIC 9(4).                                    
014400                                                                          
014500     03  WS-IDDC-KOLL.                                                    
014600         05  FILLER                  PIC X(5) VALUE 'WIDDC'.              
014700         05  WS-IDDC-TID             PIC X(2) VALUE SPACE.                
014800         05  FILLER                  PIC X    VALUE SPACE.                
014900                                                                          
015000                                                                          
015100     05  W-IDORDNR-X.                                                     
015200         10  FILLER          PIC 9(2)  VALUE ZERO.                        
015300         10  W-IDORDNR-VV    PIC 9(2).                                    
015400         10  W-IDORDNR-D     PIC 9(1).                                    
015500         10  W-IDORDNR-SS    PIC 9(2).                                    
015600                                                                          
015700     05  W-KVAVIS-6-X.                                                    
015800         10  W-KVAVIS-6      PIC 9(6).                                    
015900                                                                          
016000     05  W-ANTAL-LASN        PIC S9(7)            COMP-3.                 
016100     05  W-KVAVIS            PIC S9(7)            COMP-3.                 
016200     05  W-KVNYART           PIC S9(7)            COMP-3.                 
016300     05  W-KVRADER           PIC S9(7)            COMP-3.                 
016400     05  WS-RAKNARE          PIC S9(7)            COMP-3.                 
016500     05  W-KVPRIOART         PIC S9(7)            COMP-3.                 
016600     05  W-CMD               PIC X(3).                                    
016700     05  W-SPAR-IDKUNDRF     PIC X(10).                                   
016800     05  W-SPAR-IDKUNDNR     PIC S9(7)            COMP-3.                 
016900     05  W-SPAR-IDKOLLI      PIC S9(5)            COMP-3.                 
017000     05  WS-SAP-IDDISTR      PIC 9(5)   VALUE ZERO.                       
017100     05  WS-SAP-IDKUNDNR     PIC 9(7)   VALUE ZERO.                       
017200     05  WS-SAP-IDFAKT       PIC 9(7)   VALUE ZERO.                       
017300     05  WS-SAP-X-IDFAKT     PIC X(7)   VALUE ZERO.                       
017400     05  W-IDKUNDNR-RETUR    PIC 9(6).                                    
017500     05  W-IDDISTR-RETUR     PIC 9(4).                                    
017600     05  W-IDKUNDNR-REFILL   PIC 9(6).                                    
017700     05  W-IDDISTR-REFILL    PIC 9(4).                                    
017800     05  W-IDSEKVNR          PIC S9(3)  VALUE 0   COMP-3.                 
017900     05  W-IDSEKVNR-A03      PIC S9(3)  VALUE 0   COMP-3.                 
018000     05  W-IDSEKVNR-SAP      PIC S9(3)  VALUE 0   COMP-3.                 
018100     05  W-TIKLOCK           PIC S9(9)  VALUE 0   COMP-3.                 
018200     05  W-TEMFSINF          PIC X(40)  VALUE SPACE.                      
018300     05  W-KDFRAKT           PIC S9(3)  VALUE ZERO COMP-3.                
018400     05  WS-KVTILLGANG       PIC S9(7)V9(1)       VALUE ZERO.             
018500     05  WS-KVBEHOV          PIC S9(7)V9(1)       VALUE ZERO.             
018600     05  WS-DIFF             PIC S9(7)V9(1)       VALUE ZERO.             
018700     05  WS-BAATORDER        PIC X                VALUE SPACE.            
018800     05  WS-FLYGORDER        PIC X                VALUE SPACE.            
018900     05  WS-FAKT-INFO-DLET   PIC X                VALUE SPACE.            
019000     05  WS-IDDC-SEND        PIC X(2)             VALUE SPACE.            
019100     05  WS-A03-SKAPAD       PIC X(2)             VALUE SPACE.            
019200     05  WS-MARKUP           PIC 9V9(2)           VALUE ZERO.             
019300     05  WS-KVBINART         PIC 9(3)             VALUE ZERO.             
019400     05  WS-KVBINART-MAX     PIC S9(3)   VALUE +25 COMP-3.                
019501     05  WS-KDPRODSL         PIC S9(3)   VALUE ZERO COMP-3.               
019600     05  SW-ADINLOMR         PIC X       VALUE SPACE.                     
019700     05  SW-IDUSER           PIC X       VALUE SPACE.                     
019800     05  SPAR-FLINLREP       PIC X       VALUE SPACE.                     
019900                                                                          
020000 01  KONTROLL-SIFFRA.                                                     
020100     03  REK-IDARTNR             PIC 9(9)    VALUE 0.                     
020200     03  REK-LNGD                PIC 9(1)    VALUE 9.                     
020300     03  REK-REKSIFFR            PIC 9(1)    VALUE 0.                     
020400                                                                          
020500 01  WS-SEKEL-KOLL               PIC 9(6).                                
020600 01  FILLER REDEFINES WS-SEKEL-KOLL.                                      
020700     03  WS-SEKEL                PIC 9(1).                                
020800     03  FILLER                  PIC 9(5).                                
020900                                                                          
021000 01  WS-SEKEL-EKOA03.                                                     
021100     03  WS-EKOA03-SS            PIC 9(2).                                
021200     03  WS-EKOA03-AAMMDD        PIC 9(6).                                
021300 01  WS-AAAAMMDD REDEFINES WS-SEKEL-EKOA03 PIC 9(8).                      
021400                                                                          
021500 01  WS-SEKEL-TEST               PIC 9(6).                                
021600 01  FILLER REDEFINES WS-SEKEL-TEST.                                      
021700     03  WS-SEKEL2               PIC 9(1).                                
021800     03  FILLER                  PIC 9(5).                                
021900                                                                          
022000 01  WS-SEKEL-AVVIK.                                                      
022100     03  WS-AVVIK-SS             PIC 9(2).                                
022200     03  WS-AVVIK-AAMMDD         PIC 9(6).                                
022300 01  WS-AVVIK-AAAAMMDD REDEFINES WS-SEKEL-AVVIK PIC 9(8).                 
022400                                                                          
022500       EJECT                                                              
022600*    --- SPAR-AREA FÖR LEVERANS ANMÄRKNING                                
022700 01  SPAR-AREA-6306.                                                      
022800*    03 -COPY WDGX6306              -PRE SPAR-                            
022900 01  SPAR-AREA-6308.                                                      
023000*    03 -COPY WDGX6308              -PRE SPAR-                            
023100     EJECT                                                                
023200*    --- VALID DC                                                         
023300*01 -COPY WWDCKONS                                                        
023400     EJECT                                                                
023500*    --- DISTRIKT OMVANDLINGS TABELL                                      
023600*01 -COPY WWDIST35                                                        
023700     EJECT                                                                
023800*    --- REMARKUP FAKTOR                                                  
023900*01 -COPY WWMARKUP                                                        
024000     EJECT                                                                
024100*    --- EKONOMITRANS                                                     
024200*01 -COPY W510A03               -PRE EKOTRA03-                            
024300     EJECT                                                                
024400*    --- AVVIKELSE TRANS                                                  
024500*01 -COPY W61244            -PRE FILC-                                    
024600     EJECT                                                                
024700*01 -COPY W61247            -PRE FILC2-                                   
024800     EJECT                                                                
024900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
025000 01  GENERELLA-SUBPROGRAM.                                                
025100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
025200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
025300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
025400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
025500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
025600     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
025700     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
025800     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
025900     EJECT                                                                
026000*01 -COPY W006PRT                                                         
026100     EJECT                                                                
026200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
026300*01 -COPY WMSGINIT                                                        
026400     EJECT                                                                
026500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
026600*01 -COPY WMEDAREA                                                        
026700     SKIP3                                                                
026800 01  MESSAGE-CODES.                                                       
026900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
027000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
027100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
027200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
027300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
027400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
027500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
027600     03  TOM-RAD                 PIC X(3)    VALUE '080'.                 
027700     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
027800     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
027900     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
028000     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
028100     03  AREA-MISSING            PIC X(3)    VALUE '705'.                 
028200     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
028300     EJECT                                                                
028400 01  MESSAGE-TEXT2.                                                       
028500     03  UPDATING-NOT-ALLOWED-WRONG-DC                                    
028600                                 PIC X(31)   VALUE                        
028700         'UPDATING NOT ALLOWED - WRONG DC'.                               
028800 01  MESSAGE-TEXT3.                                                       
028900     03  NOTHING-PRINTED-WRONG-DC                                         
029000                                 PIC X(31)   VALUE                        
029100         'NOTHING PRINTED - WRONG DC'.                                    
029200     EJECT                                                                
029300 01  PROG-TO-PROG-SW.                                                     
029400*    03  -COPY WMSGSOP                                                    
029500     EJECT                                                                
029600 01  WS-BC-PARAMETRAR.                                                    
029700     03  WS-BC.                                                           
029800         05  BC-URV-IDFAKT       PIC 9(7)  VALUE ZERO.                    
029900         05  BC-URV-IDKUNDRF     PIC X(10) VALUE SPACE.                   
030000         05  BC-URV-IDKUNDNR     PIC 9(7)  VALUE ZERO.                    
030100         05  BC-URV-IDKOLLI      PIC 9(5)  VALUE ZERO.                    
030200     EJECT                                                                
030300 01  URV-AREA-START              PIC X(24)   VALUE                        
030400                                 'URV-AREA-START '.                       
030500 01  WS-PARAMETRAR.                                                       
030600     03 WS-URVAL.                                                         
030700        05 WS-URVAL-RAD OCCURS 12.                                        
030800         07  URV-CMD             PIC X(3)  VALUE SPACE.                   
030900         07  URV-IDFAKT          PIC 9(7)  VALUE ZERO.                    
031000         07  URV-IDKOLLI         PIC 9(5)  VALUE ZERO.                    
031100         07  URV-IDKUNDRF        PIC X(10) VALUE SPACE.                   
031200         07  URV-IDKUNDNR        PIC 9(7)  VALUE ZERO.                    
031300         07  URV-KDMATT          PIC X     VALUE SPACE.                   
031400     03 WS-PRINTER.                                                       
031500         05  URV-IDPRINTER       PIC X(8)  VALUE SPACE.                   
031600     EJECT                                                                
031700 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
031800     SKIP3                                                                
031900 01  DAT-IO-AREA.                                                         
032000*    03  -COPY WDATAREA                                                   
032100     EJECT                                                                
032200                                                                          
032300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
032400*                                                                         
032500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
032600     SKIP3                                                                
032700*01  MID -COPY W6I30201                                                   
032800     EJECT                                                                
032900 01  FILLER                      PIC X(16)   VALUE                        
033000                                 '6303-MID-AREA'.                         
033100     SKIP3                                                                
033200*01  MID -COPY W6I30301          -PRE 6303-                               
033300     EJECT                                                                
033400 01  FILLER                      PIC X(16)   VALUE                        
033500                                 '4341-MID-AREA'.                         
033600     SKIP3                                                                
033700*01  MID -COPY W4I34101          -PRE 4341-                               
033800     EJECT                                                                
033900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
034000     SKIP3                                                                
034100*01  -COPY WMSGAREA                                                       
034200     EJECT                                                                
034300     03  MOD REDEFINES MSG-AREA.                                          
034400*      05  -COPY W6O30201                                                 
034500     EJECT                                                                
034600*    --- AREOR FÖR W006KOM SUBMODUL                                       
034700*                                                                         
034800 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
034900*01  -COPY WMSGKOM                                                        
035000     EJECT                                                                
035100 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
035200 01  KOM-IO-AREA.                                                         
035300   03  KOM-AREA                     PIC X(2457) VALUE SPACE.              
035400   03  OHUV     REDEFINES KOM-AREA.                                       
035500*    05      -COPY W4I25101   -PRE OHUV-                                  
035600     EJECT                                                                
035700   03  ORAD     REDEFINES KOM-AREA.                                       
035800*    05      -COPY W4I25201   -PRE ORAD-                                  
035900     EJECT                                                                
036000 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
036100 01  P-TO-P-AREA.                                                         
036200     03  P-TO-P-LL               PIC S9(4)            COMP SYNC.          
036300     03  P-TO-P-Z1               PIC  X(1)   VALUE LOW-VALUE.             
036400     03  P-TO-P-Z2               PIC  X(1)   VALUE LOW-VALUE.             
036500     03  P-TO-P-TRANSKOD         PIC  X(7).                               
036600     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
036700     03  P-TO-P-FROM-MID         PIC  X(4).                               
036800     03  P-TO-P-KDMFSFOR         PIC  X(1).                               
036900     03  P-TO-P-DATA             PIC  X(1000).                            
037000     EJECT                                                                
037100 01  FILLER                      PIC X(16)  VALUE 'P-TO-P-AREA-2'.        
037200 01  P-TO-P-AREA-2.                                                       
037300     03  P-TO-P-LL-2             PIC S9(4)            COMP SYNC.          
037400     03  P-TO-P-Z1-2             PIC  X(1)   VALUE LOW-VALUE.             
037500     03  P-TO-P-Z2-2             PIC  X(1)   VALUE LOW-VALUE.             
037600     03  P-TO-P-TRANSKOD-2       PIC  X(7).                               
037700     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
037800     03  P-TO-P-FROM-MID-2       PIC  X(4).                               
037900     03  P-TO-P-KDMFSFOR-2       PIC  X(1).                               
038000*    03  W6O30201  -COPY W6I30201  -PRE PROGSW-                           
038100*    03  W6O30201  -COPY W6O30201  -PRE PROGSW-                           
038200     EJECT                                                                
038300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
038400     SKIP3                                                                
038500*01  -COPY WMFSAREA                                                       
038600     EJECT                                                                
038700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
038800*                                                                         
038900     EJECT                                                                
039000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
039100     SKIP3                                                                
039200 01  NYCKLAR-TILL-DLI.                                                    
039300     03  W-IDARTNR-X.                                                     
039400         05  W-IDARTNR           PIC S9(9)              COMP-3.           
039500                                                                          
039600     03  W-KDSEGKEY-X.                                                    
039700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
039800                                                                          
039900     03  W-TIREGDAT-X.                                                    
040000         05  W-TIREGDAT          PIC S9(7)              COMP-3.           
040100                                                                          
040200     03  W-DAINLEV-X.                                                     
040300         05  W-DAINLEV           PIC 9(16).                               
040400                                                                          
040500     03  W-IDDISTR-X.                                                     
040600         05  W-IDDISTR           PIC S9(5)              COMP-3.           
040700                                                                          
040800     03  W-IDDC                  PIC X(2).                                
040900                                                                          
041000     03  W-IDDC-B6-X.                                                     
041100         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
041200                                                                          
041300     03  W-IDPTYP                PIC X(3).                                
041400     03  W-IDKUNDRF-IDORDNR.                                              
041500         10  W-IDORDNR           PIC 9(5).                                
041600         10  FILLER              PIC X(5).                                
041700                                                                          
041800     03  W-WDL6ASEQ-MIN.                                                  
041900         05  W-SEQA-IDFAKT-MIN    PIC S9(7)             COMP-3.           
042000         05  W-SEQA-IDKUNDRF-MIN  PIC X(10).                              
042100         05  W-SEQA-IDKUNDNR-MIN  PIC S9(7)             COMP-3.           
042200         05  W-SEQA-IDKOLLI-MIN   PIC S9(5)             COMP-3.           
042300                                                                          
042400     03  W-WDL6ASEQ-MAX.                                                  
042500         05  W-SEQA-IDFAKT-MAX    PIC S9(7)             COMP-3.           
042600         05  W-SEQA-IDKUNDRF-MAX  PIC X(10).                              
042700         05  W-SEQA-IDKUNDNR-MAX  PIC S9(7)             COMP-3.           
042800         05  W-SEQA-IDKOLLI-MAX   PIC S9(5)             COMP-3.           
042900                                                                          
043000     03  W-WDL6A1KY-MIN.                                                  
043100         05  W-IDFAKT-MIN         PIC S9(7)             COMP-3.           
043200         05  W-IDKUNDRF-MIN       PIC  X(10).                             
043300         05  W-IDKUNDNR-MIN       PIC S9(7)             COMP-3.           
043400         05  W-IDKOLLI-MIN        PIC S9(5)             COMP-3.           
043500         05  FILLER               PIC X(21).                              
043600                                                                          
043700     03  W-WDL6A1KY-MAX.                                                  
043800         05  W-IDFAKT-MAX        PIC S9(7)             COMP-3.            
043900         05  W-IDKUNDRF-MAX       PIC  X(10).                             
044000         05  W-IDKUNDNR-MAX       PIC S9(7)             COMP-3.           
044100         05  W-IDKOLLI-MAX        PIC S9(5)             COMP-3.           
044200         05  FILLER               PIC X(21).                              
044300                                                                          
044400     03  W-IDFAKT-X.                                                      
044500         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
044600                                                                          
044700     03  W-IDKUNDRF              PIC X(10)   VALUE SPACE.                 
044800                                                                          
044900     03  W-IDKUNDNR-X.                                                    
045000         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
045100                                                                          
045200     03  W-IDKOLLI-X.                                                     
045300         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
045400                                                                          
045500     03  W-IDLBBET               PIC X(12)   VALUE SPACE.                 
045600                                                                          
045700     03  W-6301KEY-X.                                                     
045800         05  W-6301-IDHTYP      PIC X(4)     VALUE '6301'.                
045900         05  W-6301-IDDC        PIC X(2).                                 
046000         05  FILLER             PIC X(24)    VALUE LOW-VALUE.             
046100                                                                          
046200     03  W-6305KEY-X.                                                     
046300         05  W-6305-IDHTYP      PIC X(4)     VALUE '6305'.                
046400         05  FILLER             PIC X(2)     VALUE LOW-VALUE.             
046500         05  FILLER             PIC X(24)    VALUE LOW-VALUE.             
046600                                                                          
046700     03  W-WDB301KY-X.                                                    
046800         05  W-IDDC-WDB3         PIC X(2)    VALUE SPACE.                 
046900         05  W-IDDISTR-WDB3      PIC S9(5)   VALUE ZERO COMP-3.           
047000         05  W-IDKUNDNR-WDB3     PIC S9(7)   VALUE ZERO COMP-3.           
047100                                                                          
047200     03  W-WDB301KY-DEF-X.                                                
047300         05  W-IDDC-WDB3-DEF     PIC X(2)    VALUE SPACE.                 
047400         05  W-IDDISTR-WDB3-DEF  PIC S9(5)   VALUE ZERO COMP-3.           
047500         05  W-IDKUNDNR-WDB3-DEF PIC S9(7) VALUE +9999999 COMP-3.         
047600                                                                          
047700*--------W6G1                                                             
047800     03  W-W6GXKEY-6005-X.                                                
047900         05  W-IDHTYP-6005       PIC X(4)    VALUE '6005'.                
048000         05  W-IDDC-6005         PIC X(2)    VALUE '11'.                  
048100         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
048200                                                                          
048300     03  W-W6GXKEY-6006-X.                                                
048400         05  W-ADINLOMR-6006     PIC X(4)    VALUE SPACE.                 
048500         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
048600     EJECT                                                                
048700*    --- STATUS-KOD FRÅN IMS                                              
048800 01  STATUS-WS                   PIC XX.                                  
048900     88  SEGMENT-FINNS                       VALUE '  '.                  
049000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
049100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
049200     SKIP2                                                                
049300 01  GODK-STATUSKODER.                                                    
049400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
049500     SKIP3                                                                
049600 01  SSA1                        PIC X(160).                              
049700 01  SSA2                        PIC X(128).                              
049800 01  SSA3                        PIC X(128).                              
049900     EJECT                                                                
050000*    --- IMS FUNKTIONSKODER                                               
050100*01  -COPY W0003                                                          
050200     EJECT                                                                
050300*    ---  DLI INPUT-OUTPUT AREA                                           
050400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
050500     SKIP3                                                                
050600 01  DLI-IO-AREA.                                                         
050700     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
050800     03  WLARTS01 REDEFINES IO-AREA.                                      
050900*        05  -COPY WDK701                                                 
051000     EJECT                                                                
051100     03  WLARTS11 REDEFINES IO-AREA.                                      
051200*        05  -COPY WDK711                                                 
051300     EJECT                                                                
051400     03  WLFILB01 REDEFINES IO-AREA.                                      
051500*        05  -COPY WDR801                                                 
051601*        07  -COPY W510EKHA  -PRE R8- -RED FIL-WDR801-DATA                
051700     EJECT                                                                
051800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
051900     SKIP3                                                                
052000 01  DLI-IO-AREA1.                                                        
052100     03  IO-AREA1                PIC X(300)  VALUE SPACE.                 
052200     03  WLINLD01 REDEFINES IO-AREA1.                                     
052300*        05  -COPY WDL6A1                                                 
052400     EJECT                                                                
052500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
052600     SKIP3                                                                
052700 01  DLI-IO-AREA2.                                                        
052800     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
052900     03  WLINLC11 REDEFINES IO-AREA2.                                     
053000*        05  -COPY WDL611                                                 
053100     EJECT                                                                
053200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
053300     SKIP3                                                                
053400 01  DLI-IO-AREA3.                                                        
053500     03  IO-AREA3                PIC X(300)  VALUE SPACE.                 
053600     03  WLGMTB01 REDEFINES IO-AREA3.                                     
053700*        05  -COPY WDB301                                                 
053800     EJECT                                                                
053900 01  DLI-IO-AREA4.                                                        
054000     03  IO-AREA4                PIC X(900)  VALUE SPACE.                 
054100     03  WLARTC01 REDEFINES IO-AREA4.                                     
054200*        05  -COPY WDK601                                                 
054300     EJECT                                                                
054400     03  WLARTC11 REDEFINES IO-AREA4.                                     
054500*        05  -COPY WDK611                                                 
054600     EJECT                                                                
054700 01  DLI-IO-AREA5.                                                        
054800     03  IO-AREA5                PIC X(550)  VALUE SPACE.                 
054900     03  WL630511 REDEFINES IO-AREA5.                                     
055000*        05  -COPY WDGX6306                                               
055100     EJECT                                                                
055200     03  WL630521 REDEFINES IO-AREA5.                                     
055300*        05  -COPY WDGX6308                                               
055400     EJECT                                                                
055500 01  DLI-IO-AREA6.                                                        
055600     03  IO-AREA6                PIC X(300)  VALUE SPACE.                 
055700     SKIP3                                                                
055800     03  WL630101 REDEFINES IO-AREA6.                                     
055900*        05  -COPY WDGX6301                                               
056000     EJECT                                                                
056100     03  WL630111 REDEFINES IO-AREA6.                                     
056200*        05  -COPY WDGX6302                                               
056300     EJECT                                                                
056400 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLLOGA01'.               
056500 01  DLI-IO-WLLOGA01.                                                     
056600*    03  WLLOGA01  -COPY WDL901                                           
056700     EJECT                                                                
056800 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLSAPA01'.               
056900 01  DLI-IO-WLSAPA01.                                                     
057000*    03  WLSAPA01  -COPY WDR901                                           
057100*    07  -COPY W510EKHA  -RED FIL-WDR901-DATA                             
057200     EJECT                                                                
057300 01  DLI-IO-AREA-FILC.                                                    
057400     03  IO-AREA-FILC            PIC X(300)  VALUE SPACE.                 
057500     SKIP3                                                                
057600     03  WLFILC01 REDEFINES IO-AREA-FILC.                                 
057700*        05  -COPY WDR301       -PRE FILC-                                
057800     EJECT                                                                
057900 01  DLI-IO-AREA-FILC2.                                                   
058000     03  IO-AREA-FILC2           PIC X(300)  VALUE SPACE.                 
058100     SKIP3                                                                
058200     03  WLFILC01 REDEFINES IO-AREA-FILC2.                                
058300*        05  -COPY WDR301       -PRE FILC2-                               
058400     EJECT                                                                
058500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
058600 01  DLI-IO-AREA-B601.                                                    
058700*    03  -COPY WDB601                                                     
058800     EJECT                                                                
058900 01  FILLER               PIC X(16)   VALUE 'WDB601 SPAR'.                
059000 01  DLI-IO-AREA-B601-SPAR.                                               
059100*    03  -COPY WDB601 -PRE SPAR-                                          
059200     EJECT                                                                
059300 01  FILLER               PIC X(16)   VALUE 'WDB601 SEND'.                
059400 01  DLI-IO-AREA-B601-SEND.                                               
059500*    03  -COPY WDB601 -PRE SEND-                                          
059600     EJECT                                                                
059700 01  FILLER               PIC X(16)   VALUE 'DLI-IO-W6G130'.              
059800 01  DLI-IO-AREA-W6G130.                                                  
059900*    03  -COPY W6GX6006                                                   
060000     EJECT                                                                
060100 LINKAGE SECTION.                                                         
060200                                                                          
060300*01  -COPY W0009   -PRE MSG-                                              
060400     EJECT                                                                
060500*01  -COPY W0009   -PRE ALT1-                                             
060600     EJECT                                                                
060700*01  -COPY W0009   -PRE ALT3-                                             
060800     EJECT                                                                
060900*01  -COPY W0009   -PRE ALT-                                              
061000     EJECT                                                                
061100*01  -COPY W0009   -PRE ALT4-                                             
061200     EJECT                                                                
061300*01  -COPY W0008   -PRE USEA-                                             
061400     05  FILLER                  PIC X.                                   
061500     EJECT                                                                
061600*01  -COPY W0008  -PRE GX63-                                              
061700     05  FILLER                  PIC X.                                   
061800     EJECT                                                                
061900*01  -COPY W0008  -PRE INLC-                                              
062000     05  FILLER                  PIC X.                                   
062100     EJECT                                                                
062200*01  -COPY W0008  -PRE INLC1-                                             
062300     05  FILLER                  PIC X.                                   
062400     EJECT                                                                
062500*01  -COPY W0008  -PRE INLD-                                              
062600     05  FILLER                  PIC X.                                   
062700     EJECT                                                                
062800*01  -COPY W0008  -PRE ARTS-                                              
062900     05  FILLER                  PIC X.                                   
063000     EJECT                                                                
063100*01  -COPY W0008  -PRE KOMA-                                              
063200     05  FILLER                  PIC X.                                   
063300     EJECT                                                                
063400*01  -COPY W0008  -PRE FILB-                                              
063500     05  FILLER                  PIC X.                                   
063600     EJECT                                                                
063700*01  -COPY W0008  -PRE KNDB-                                              
063800     05  FILLER                  PIC X.                                   
063900     EJECT                                                                
064000*01  -COPY W0008  -PRE ARTC-                                              
064100     05  FILLER                  PIC X.                                   
064200     EJECT                                                                
064300*01  -COPY W0008  -PRE GX65-                                              
064400     05  FILLER                  PIC X.                                   
064500     EJECT                                                                
064600*01  -COPY W0008  -PRE WLLOGA-                                            
064700     05  FILLER                  PIC X.                                   
064800     EJECT                                                                
064900*01  -COPY W0008  -PRE SAPA-                                              
065000     05  FILLER                  PIC X.                                   
065100     EJECT                                                                
065200*01  -COPY W0008  -PRE FILC-                                              
065300     05  FILLER                  PIC X.                                   
065400     EJECT                                                                
065500*01  -COPY W0008  -PRE WDB6-                                              
065600     05  FILLER                  PIC X.                                   
065700     EJECT                                                                
065800*01  -COPY W0008  -PRE W6G1-                                              
065900     05  FILLER                  PIC X.                                   
066000     EJECT                                                                
066100 PROCEDURE DIVISION  USING MSG-PCB  ALT1-PCB ALT3-PCB                     
066200                           ALT-PCB  ALT4-PCB                              
066300                           USEA-PCB GX63-PCB INLC-PCB INLC1-PCB           
066400                           INLD-PCB ARTS-PCB KOMA-PCB FILB-PCB            
066500                           KNDB-PCB ARTC-PCB GX65-PCB WLLOGA-PCB          
066600                           SAPA-PCB FILC-PCB WDB6-PCB W6G1-PCB.           
066700 MAIN SECTION.                                                            
066800     ENTRY 'DLITCBL' USING MSG-PCB  ALT1-PCB ALT3-PCB                     
066900                           ALT-PCB  ALT4-PCB                              
067000                           USEA-PCB GX63-PCB INLC-PCB INLC1-PCB           
067100                           INLD-PCB ARTS-PCB KOMA-PCB FILB-PCB            
067200                           KNDB-PCB ARTC-PCB GX65-PCB WLLOGA-PCB          
067300                           SAPA-PCB FILC-PCB WDB6-PCB W6G1-PCB.           
067400                                                                          
067500     PERFORM IMS-GET-MSG                                                  
067600                                                                          
067700     IF SEGMENT-FINNS                                                     
067800        PERFORM A-INIT                                                    
067900        PERFORM B-KOLLA-NYCKLAR                                           
068000                                                                          
068100        IF NYCKLAR-OK                                                     
068200                                                                          
068300           IF MFS-UPDATE                                                  
068400              PERFORM G-KOLLA-INPUT                                       
068500                                                                          
068600              IF INDATA-OK                                                
068700                 PERFORM H-UPPDATERA                                      
068800              END-IF                                                      
068900                                                                          
069000           ELSE                                                           
069100              IF MFS-FIRST                                                
069200                 PERFORM C-FOERSTA-SIDA                                   
069300                                                                          
069400              ELSE                                                        
069500                 IF MFS-NEXT                                              
069600                    PERFORM D-NAESTA-SIDA                                 
069700                                                                          
069800                 ELSE                                                     
069900                    PERFORM E-SAMMA-SIDA                                  
070000                                                                          
070100                 END-IF                                                   
070200              END-IF                                                      
070300           END-IF                                                         
070400                                                                          
070500           IF  EJ-HOPP                                                    
070600              PERFORM F-LAES-VISA-INFO                                    
070700              IF MFS-IDTRANS = '630C'                                     
070800                 MOVE INF-UPDATE-DONE TO MED-IDMFSINF                     
070900                 CALL WMEDKONV USING MED-WMEDAREA                         
071000                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
071100              END-IF                                                      
071200           END-IF                                                         
071300        END-IF                                                            
071400                                                                          
071500        IF HOPP                                                           
071600           COMPUTE P-TO-P-LL =  LNG-P-TO-P-PREFIX +                       
071700                                LENGTH OF 6303-MID-W6I30301               
071800                                                                          
071900           MOVE LOW-VALUE      TO P-TO-P-Z1                               
072000           MOVE LOW-VALUE      TO P-TO-P-Z2                               
072100           MOVE 'W6T303 '      TO P-TO-P-TRANSKOD                         
072200           MOVE '6302'         TO P-TO-P-FROM-MID                         
072300           MOVE MFS-KDMFSFOR   TO P-TO-P-KDMFSFOR                         
072400                                                                          
072500           PERFORM I-EDIT-MID-W6I30301                                    
072600           MOVE 6303-MID-W6I30301   TO P-TO-P-DATA                        
072700           PERFORM IMS-ISRT-ALT1-PCB-6303                                 
072800        ELSE                                                              
072900           IF HOPP-UPDATE                                                 
073000              COMPUTE P-TO-P-LL-2 =  LNG-P-TO-P-PREFIX +                  
073100                                     LENGTH OF MID-W6I30201 +             
073200                                     LENGTH OF MOD-W6O30201               
073300                                                                          
073400              MOVE LOW-VALUE      TO P-TO-P-Z1-2                          
073500              MOVE LOW-VALUE      TO P-TO-P-Z2-2                          
073600              MOVE 'W6T303X'      TO P-TO-P-TRANSKOD-2                    
073700              MOVE '6302'         TO P-TO-P-FROM-MID-2                    
073800              MOVE MFS-KDMFSFOR   TO P-TO-P-KDMFSFOR-2                    
073900              MOVE MOD-W6O30201 TO PROGSW-MOD-W6O30201                    
074000                                                                          
074100              PERFORM IMS-ISRT-ALT4-PCB-6303                              
074200           ELSE                                                           
074300              COMPUTE MSG-KVLL = LENGTH OF MOD-W6O30201 + 4               
074400              PERFORM IMS-INSERT-MSG                                      
074500           END-IF                                                         
074600        END-IF                                                            
074700     END-IF                                                               
074800                                                                          
074900     MOVE ZERO TO RETURN-CODE                                             
075000     GOBACK                                                               
075100     .                                                                    
075200     EJECT                                                                
075300 A-INIT SECTION.                                                          
075400                                                                          
075500     IF MSG-DUBBLA-TRANSKODER                                             
075600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I30201                 
075700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
075800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
075900     ELSE                                                                 
076000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I30201                  
076100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
076200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
076300     END-IF                                                               
076400                                                                          
076500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
076600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
076700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
076800                                                                          
076900     MOVE LOW-VALUE TO MSG-AREA                                           
077000     MOVE 'W6O302N1' TO MFS-IDMOD                                         
077100     MOVE '6302' TO MOD-IDTRANS                                           
077200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
077300                                                                          
077400     IF EGEN-MID OR HELP-MID                                              
077500        CONTINUE                                                          
077600     ELSE                                                                 
077700        MOVE SPACE TO MFS-KDTRTYP                                         
077800        MOVE '7'   TO MFS-IDPFK                                           
077900     END-IF                                                               
078000                                                                          
078100     ACCEPT W-DATUM-X     FROM DATE                                       
078200     MOVE W-DATUM(3:2)    TO PER-IX                                       
078300     ACCEPT W-TIME-X      FROM TIME                                       
078400     MOVE W-TIME-N        TO W-TIKLOCK                                    
078500                                                                          
078600     MOVE LOW-VALUE       TO W-WDL6ASEQ-MIN                               
078700     MOVE HIGH-VALUE      TO W-WDL6ASEQ-MAX                               
078800     MOVE ZERO            TO WS-KVBINART                                  
078900                                                                          
079000     MOVE 'W6030200'      TO FILC-FIL-IDPGM                               
079100                             FILC2-FIL-IDPGM                              
079200     MOVE W-DATUM         TO FILC-FIL-TIREGDAT                            
079300                             FILC2-FIL-TIREGDAT                           
079400     MOVE 'W61244  '      TO FILC-FIL-IDCPYTXT                            
079500     MOVE 'W61247  '      TO FILC2-FIL-IDCPYTXT                           
079600     MOVE ZERO            TO FILC-FIL-TIKLOCK                             
079700                             FILC2-FIL-TIKLOCK                            
079800     .                                                                    
079900     EJECT                                                                
080000 B-KOLLA-NYCKLAR SECTION.                                                 
080100                                                                          
080200     MOVE ALL '+' TO MSGI-WMSGINIT                                        
080300     MOVE '001' TO MSGI-KDCALL                                            
080400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
080500     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
080600     MOVE '6302'                 TO MSGI-IDTRANS                          
080700     IF  HELP-MID OR EGEN-MID OR HOPP-MID                                 
080800       IF MID-IDFAKT-IN NUMERIC                                           
080900          IF MID-IDFAKT-IN > ZERO                                         
081000             MOVE MID-IDFAKT-IN TO MSGI-IDFAKT                            
081100          END-IF                                                          
081200       END-IF                                                             
081300     END-IF                                                               
081400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
081500     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
081600     MOVE JA TO NYCKLAR-SW                                                
081700                                                                          
081800*    -- KONTROLL AV IDFAKT                                                
081900     MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-IN                                
082000                                                                          
082100     IF MID-IDFAKT-IN NOT = ALL '+'                                       
082200       MOVE '7'           TO MFS-IDPFK                                    
082300       MOVE SPACE         TO MFS-KDTRTYP                                  
082400       MOVE +1 TO INDX                                                    
082500       PERFORM UNTIL INDX > MAX-INDX                                      
082600         MOVE ALL '+' TO MID-ADINLOMR(INDX)                               
082700         ADD +1 TO INDX                                                   
082800       END-PERFORM                                                        
082900     END-IF                                                               
083000                                                                          
083100     IF EGEN-MID                                                          
083200        CONTINUE                                                          
083300     ELSE                                                                 
083400       MOVE +1 TO INDX                                                    
083500       PERFORM UNTIL INDX > MAX-INDX                                      
083600         MOVE ALL '+' TO MID-ADINLOMR(INDX)                               
083700         ADD +1 TO INDX                                                   
083800       END-PERFORM                                                        
083900     END-IF                                                               
084000                                                                          
084100     IF MSGI-IDFAKT       NUMERIC                                         
084200        MOVE MSGI-IDFAKT  TO W-IDFAKT                                     
084300     ELSE                                                                 
084400       MOVE NEJ           TO NYCKLAR-SW                                   
084500     END-IF                                                               
084600                                                                          
084701     IF GODK-MID                                                          
084800     OR NYCKLAR-OK                                                        
084900        MOVE MSGI-IDFAKT       TO MOD-IDFAKT-UT                           
085000        INSPECT MOD-IDFAKT-UT REPLACING LEADING ZERO BY SPACE             
085100     ELSE                                                                 
085200        MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-UT                             
085300     END-IF                                                               
085400                                                                          
085500     IF NYCKLAR-FEL                                                       
085600        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
085700        CALL WMEDKONV USING MED-WMEDAREA                                  
085800                                                                          
085900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
086000        PERFORM MFS-RENSA-FAELT-IN                                        
086100        PERFORM MFS-RENSA-FAELT-UT                                        
086200     END-IF                                                               
086300     .                                                                    
086400     EJECT                                                                
086500 C-FOERSTA-SIDA SECTION.                                                  
086600                                                                          
086700     MOVE INF-FIRST-PAGE  TO MED-IDMFSINF                                 
086800     CALL WMEDKONV USING MED-WMEDAREA                                     
086900                                                                          
087000     MOVE MED-MFSINF      TO MOD-TEMFSFEL                                 
087100                                                                          
087200*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
087300     MOVE SPACE           TO W-IDKUNDRF                                   
087400     MOVE ZERO            TO W-IDKUNDNR                                   
087500                             W-IDKOLLI                                    
087600     PERFORM MFS-RENSA-FAELT-IN                                           
087700     MOVE +1 TO INDX                                                      
087800     PERFORM UNTIL INDX > MAX-INDX                                        
087900       MOVE ALL '+' TO MID-ADINLOMR(INDX)                                 
088000       ADD +1 TO INDX                                                     
088100     END-PERFORM                                                          
088200     .                                                                    
088300     EJECT                                                                
088400 D-NAESTA-SIDA SECTION.                                                   
088500                                                                          
088600     MOVE MID-IDKUNDRF-NEXT TO W-IDKUNDRF                                 
088700                               W-SEQA-IDKUNDRF-MIN                        
088800                                                                          
088900     INSPECT MID-IDKUNDNR-NEXT  REPLACING LEADING SPACE BY ZERO           
089000     IF  MID-IDKUNDNR-NEXT  NUMERIC                                       
089100         MOVE MID-IDKUNDNR-NEXT                                           
089200                          TO W-IDKUNDNR                                   
089300                             W-SEQA-IDKUNDNR-MIN                          
089400     ELSE                                                                 
089500         MOVE ZERO        TO W-IDKUNDNR                                   
089600     END-IF                                                               
089700                                                                          
089800     INSPECT MID-IDKOLLI-NEXT REPLACING LEADING SPACE BY ZERO             
089900     IF  MID-IDKOLLI-NEXT  NUMERIC                                        
090000         MOVE MID-IDKOLLI-NEXT                                            
090100                          TO W-IDKOLLI                                    
090200                             W-SEQA-IDKOLLI-MIN                           
090300     ELSE                                                                 
090400         MOVE ZERO        TO W-IDKOLLI                                    
090500     END-IF                                                               
090600                                                                          
090700     PERFORM MFS-RENSA-FAELT-IN                                           
090800     MOVE +1 TO INDX                                                      
090900     PERFORM UNTIL INDX > MAX-INDX                                        
091000       MOVE ALL '+' TO MID-ADINLOMR(INDX)                                 
091100       ADD +1 TO INDX                                                     
091200     END-PERFORM                                                          
091300                                                                          
091400     IF  W-IDKUNDRF       = SPACE                                         
091500     AND W-IDKUNDNR       = ZERO                                          
091600     AND W-IDKOLLI        = ZERO                                          
091700         MOVE INF-SISTA-SIDAN TO MED-IDMFSINF                             
091800         CALL WMEDKONV USING MED-WMEDAREA                                 
091900                                                                          
092000         MOVE MED-MFSINF      TO MOD-TEMFSFEL                             
092100         MOVE MID-IDKUNDRF-ENTER TO W-IDKUNDRF                            
092200                                    W-SEQA-IDKUNDRF-MIN                   
092300                                                                          
092400         INSPECT MID-IDKUNDNR-ENTER                                       
092500                               REPLACING LEADING SPACE BY ZERO            
092600         IF  MID-IDKUNDNR-ENTER NUMERIC                                   
092700             MOVE MID-IDKUNDNR-ENTER                                      
092800                          TO W-IDKUNDNR                                   
092900                             W-SEQA-IDKUNDNR-MIN                          
093000         ELSE                                                             
093100             MOVE ZERO    TO W-IDKUNDNR                                   
093200         END-IF                                                           
093300                                                                          
093400         INSPECT MID-IDKOLLI-ENTER                                        
093500                                 REPLACING LEADING SPACE BY ZERO          
093600         IF  MID-IDKOLLI-ENTER NUMERIC                                    
093700             MOVE MID-IDKOLLI-ENTER                                       
093800                          TO W-IDKOLLI                                    
093900                             W-SEQA-IDKOLLI-MIN                           
094000         ELSE                                                             
094100             MOVE ZERO    TO W-IDKOLLI                                    
094200         END-IF                                                           
094300     END-IF                                                               
094400     .                                                                    
094500     EJECT                                                                
094600 E-SAMMA-SIDA SECTION.                                                    
094700                                                                          
094800     MOVE NEJ TO PRINTNING-SW                                             
094900              SW-ADINLOMR                                                 
095000              SW-IDUSER                                                   
095100                                                                          
095200     IF EGEN-MID                                                          
095300     OR HELP-MID                                                          
095400        MOVE MID-IDKUNDRF-ENTER TO W-IDKUNDRF                             
095500                                   W-SEQA-IDKUNDRF-MIN                    
095600                                                                          
095700        INSPECT MID-IDKUNDNR-ENTER                                        
095800                               REPLACING LEADING SPACE BY ZERO            
095900        IF  MID-IDKUNDNR-ENTER NUMERIC                                    
096000            MOVE MID-IDKUNDNR-ENTER                                       
096100                          TO W-IDKUNDNR                                   
096200                             W-SEQA-IDKUNDNR-MIN                          
096300        ELSE                                                              
096400            MOVE ZERO        TO W-IDKUNDNR                                
096500        END-IF                                                            
096600                                                                          
096700        INSPECT MID-IDKOLLI-ENTER                                         
096800                                 REPLACING LEADING SPACE BY ZERO          
096900        IF  MID-IDKOLLI-ENTER NUMERIC                                     
097000            MOVE MID-IDKOLLI-ENTER                                        
097100                          TO W-IDKOLLI                                    
097200                             W-SEQA-IDKOLLI-MIN                           
097300        ELSE                                                              
097400            MOVE ZERO     TO W-IDKOLLI                                    
097500        END-IF                                                            
097600                                                                          
097700        IF  MID-CMD-IN (1) = ALL '+'                                      
097800        AND MID-CMD-IN (2) = ALL '+'                                      
097900        AND MID-CMD-IN (3) = ALL '+'                                      
098000        AND MID-CMD-IN (4) = ALL '+'                                      
098100        AND MID-CMD-IN (5) = ALL '+'                                      
098200        AND MID-CMD-IN (6) = ALL '+'                                      
098300        AND MID-CMD-IN (7) = ALL '+'                                      
098400        AND MID-CMD-IN (8) = ALL '+'                                      
098500        AND MID-CMD-IN (9) = ALL '+'                                      
098600        AND MID-CMD-IN (10) = ALL '+'                                     
098700        AND MID-CMD-IN (11) = ALL '+'                                     
098800        AND MID-CMD-IN (12) = ALL '+'                                     
098900        AND MID-ADINLOMR(1) = ALL '+'                                     
099000        AND MID-ADINLOMR(2) = ALL '+'                                     
099100        AND MID-ADINLOMR(3) = ALL '+'                                     
099200        AND MID-ADINLOMR(4) = ALL '+'                                     
099300        AND MID-ADINLOMR(5) = ALL '+'                                     
099400        AND MID-ADINLOMR(6) = ALL '+'                                     
099500        AND MID-ADINLOMR(7) = ALL '+'                                     
099600        AND MID-ADINLOMR(8) = ALL '+'                                     
099700        AND MID-ADINLOMR(9) = ALL '+'                                     
099800        AND MID-ADINLOMR(10) = ALL '+'                                    
099900        AND MID-ADINLOMR(11) = ALL '+'                                    
100000        AND MID-ADINLOMR(12) = ALL '+'                                    
100100        AND MID-IDUSER-003   = ALL '+'                                    
100200        AND MID-ADINLOMR-PRT = ALL '+'                                    
100300           PERFORM MFS-RENSA-FAELT-IN                                     
100400                                                                          
100500        ELSE                                                              
100600           PERFORM EA-KOLLA-CMD                                           
100700                                                                          
100800           IF  INDATA-OK                                                  
100900               IF  W-CMD  = 'MIS'                                         
101000               OR  W-CMD  = 'RET'                                         
101100               OR  W-CMD  = 'LOS'                                         
101200               OR  W-CMD  = 'DCR'                                         
101300               OR  W-CMD  = 'CAN'                                         
101400               OR  W-CMD  = 'LOC'                                         
101500               OR  W-CMD  = 'BIN'                                         
101600               OR  SW-ADINLOMR = 'J'                                      
101700               OR  SW-IDUSER = 'J'                                        
101800                   MOVE INF-PRESS-PF11 TO MED-IDMFSINF                    
101900                   CALL WMEDKONV USING MED-WMEDAREA                       
102000                   MOVE MED-MFSINF TO MOD-TEMFSFEL                        
102100                   PERFORM EB-MID-INDATA-TILL-MOD                         
102200               ELSE                                                       
102300                   IF  W-CMD  = 'S  '                                     
102400                   OR  W-CMD  = 'X  '                                     
102500                       MOVE JA TO HOPP-SW                                 
102600                   ELSE                                                   
102700                       IF  W-CMD  = 'PR ' OR 'PRL'                        
102800                           MOVE +1  TO INDX                               
102900                                       URV-IX                             
103000                           PERFORM UNTIL INDX > MAX-INDX                  
103100                               IF  MID-CMD-IN (INDX) = 'PR '              
103200                               OR 'PRL'                                   
103300                                   PERFORM EC-SKAPA-PRINTER-TRANS         
103400                                   ADD +1 TO URV-IX                       
103500                               END-IF                                     
103600                               ADD +1 TO INDX                             
103700******************* 990204                                                
103800                           END-PERFORM                                    
103900                           PERFORM EF-STARTA-PRINTER-TRANS                
104000                           PERFORM MFS-RENSA-FAELT-IN                     
104100                       ELSE                                               
104200                           IF W-CMD  = 'BC '                              
104300                              MOVE +1  TO INDX                            
104400                              PERFORM UNTIL INDX > MAX-INDX               
104500                                  IF  MID-CMD-IN (INDX) = 'BC '           
104600                                      PERFORM ED-SKAPA-BC-TRANS           
104700                                  END-IF                                  
104800                                  ADD +1 TO INDX                          
104900                              END-PERFORM                                 
105000******************* 990204                                                
105100                              PERFORM MFS-RENSA-FAELT-IN                  
105200                           END-IF                                         
105300                       END-IF                                             
105400                   END-IF                                                 
105500               END-IF                                                     
105600           END-IF                                                         
105700        END-IF                                                            
105800     ELSE                                                                 
105900        PERFORM MFS-RENSA-FAELT-IN                                        
106000     END-IF                                                               
106100     .                                                                    
106200     EJECT                                                                
106300 EA-KOLLA-CMD SECTION.                                                    
106400******************************************************************        
106500*  ENTER CMD = S      HOPP TILL 6303                                      
106600*              X                                                          
106700*              PR/PRL STARTAR W612S4 BINNING LIST                         
106800*              BC     STARTAR W612S3                                      
106900*  PF11        MIS SAKNAT KOLLI                                           
107000*              RET RETUR                                                  
107100*              LOS LOST KOLLI                                             
107200*              DCR SKADAT KOLLI                                           
107300*              LOC UPPDATERING LOCATION                                   
107400*              BIN TRANS TILL 6303                                        
107500******************************************************************        
107600                                                                          
107700     MOVE JA              TO INDATA-SW                                    
107800     MOVE SPACE           TO W-CMD                                        
107900     MOVE +1              TO INDX                                         
108000                                                                          
108100                                                                          
108200     PERFORM UNTIL INDX > MAX-INDX                                        
108300         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-CMD-ATTR (INDX)                
108400                                                                          
108500            IF  MID-CMD-IN (INDX) NOT = ALL '+'                           
108600            AND MID-CMD-IN (INDX) NOT = 'S  '                             
108700            AND MID-CMD-IN (INDX) NOT = 'X  '                             
108800            AND MID-CMD-IN (INDX) NOT = 'PR '                             
108900            AND MID-CMD-IN (INDX) NOT = 'PRL'                             
109000            AND MID-CMD-IN (INDX) NOT = 'MIS'                             
109100            AND MID-CMD-IN (INDX) NOT = 'RET'                             
109200            AND MID-CMD-IN (INDX) NOT = 'LOS'                             
109300            AND MID-CMD-IN (INDX) NOT = 'DCR'                             
109400            AND MID-CMD-IN (INDX) NOT = 'CAN'                             
109500            AND MID-CMD-IN (INDX) NOT = 'BC '                             
109600            AND MID-CMD-IN (INDX) NOT = 'LOC'                             
109700            AND MID-CMD-IN (INDX) NOT = 'BIN'                             
109800            AND MID-CMD-IN (INDX) NOT = SPACE                             
109900              MOVE NEJ     TO INDATA-SW                                   
110000              MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)              
110100            END-IF                                                        
110200                                                                          
110300            IF  MID-CMD-IN (INDX) = 'S  '                                 
110400            OR  MID-CMD-IN (INDX) = 'X  '                                 
110500            OR  MID-CMD-IN (INDX) = 'PR '                                 
110600            OR  MID-CMD-IN (INDX) = 'PRL'                                 
110700            OR  MID-CMD-IN (INDX) = 'MIS'                                 
110800            OR  MID-CMD-IN (INDX) = 'RET'                                 
110900            OR  MID-CMD-IN (INDX) = 'LOS'                                 
111000            OR  MID-CMD-IN (INDX) = 'DCR'                                 
111100            OR  MID-CMD-IN (INDX) = 'CAN'                                 
111200            OR  MID-CMD-IN (INDX) = 'BC '                                 
111300            OR  MID-CMD-IN (INDX) = 'LOC'                                 
111400            OR  MID-CMD-IN (INDX) = 'BIN'                                 
111500                IF  MID-IDKUNDRF (INDX) = SPACE                           
111600                AND MID-IDKUNDNR (INDX) = ZERO                            
111700                AND MID-IDKOLLI  (INDX) = ZERO                            
111800                   MOVE NEJ TO INDATA-SW                                  
111900                   MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)         
112000                END-IF                                                    
112100            END-IF                                                        
112200                                                                          
112300            IF  MID-CMD-IN (INDX) = 'S  '                                 
112400            OR  MID-CMD-IN (INDX) = 'X  '                                 
112500                IF W-CMD          = SPACE                                 
112600                   MOVE MID-CMD-IN (INDX) TO W-CMD                        
112700                ELSE                                                      
112800                   MOVE NEJ TO INDATA-SW                                  
112900                   MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)         
113000                END-IF                                                    
113100            END-IF                                                        
113200                                                                          
113300            IF  MID-CMD-IN (INDX) = 'MIS'                                 
113400            OR  MID-CMD-IN (INDX) = 'RET'                                 
113500            OR  MID-CMD-IN (INDX) = 'LOS'                                 
113600            OR  MID-CMD-IN (INDX) = 'DCR'                                 
113700            OR  MID-CMD-IN (INDX) = 'CAN'                                 
113800            OR  MID-CMD-IN (INDX) = 'LOC'                                 
113900            OR  MID-CMD-IN (INDX) = 'BIN'                                 
114000                IF  W-CMD         = SPACE                                 
114100                    MOVE MID-CMD-IN (INDX) TO W-CMD                       
114200                ELSE                                                      
114300                    IF  W-CMD         = 'S  '                             
114400                    OR  W-CMD         = 'X  '                             
114500                    OR  W-CMD         = 'PR '                             
114600                    OR  W-CMD         = 'PRL'                             
114700                    OR  W-CMD         = 'BC '                             
114800                        MOVE NEJ     TO INDATA-SW                         
114900                        MOVE MFS-ALFA-FAELT-FEL                           
115000                                     TO MOD-CMD-ATTR (INDX)               
115100                    END-IF                                                
115200                END-IF                                                    
115300            END-IF                                                        
115400                                                                          
115500            IF  MID-CMD-IN (INDX) = 'PR ' OR 'PRL'                        
115600                IF  W-CMD         = SPACE                                 
115700                    MOVE MID-CMD-IN (INDX) TO W-CMD                       
115800                ELSE                                                      
115900                    IF  W-CMD         = 'S  '                             
116000                    OR  W-CMD         = 'X  '                             
116100                    OR  W-CMD         = 'MIS'                             
116200                    OR  W-CMD         = 'RET'                             
116300                    OR  W-CMD         = 'LOS'                             
116400                    OR  W-CMD         = 'DCR'                             
116500                    OR  W-CMD         = 'CAN'                             
116600                    OR  W-CMD         = 'BC '                             
116700                    OR  W-CMD         = 'LOC'                             
116800                    OR  W-CMD         = 'BIN'                             
116900                        MOVE NEJ     TO INDATA-SW                         
117000                        MOVE MFS-ALFA-FAELT-FEL                           
117100                                     TO MOD-CMD-ATTR (INDX)               
117200                    END-IF                                                
117300                END-IF                                                    
117400            END-IF                                                        
117500                                                                          
117600            IF  MID-CMD-IN (INDX) = 'BC '                                 
117700                IF  W-CMD         = SPACE                                 
117800                    MOVE MID-CMD-IN (INDX) TO W-CMD                       
117900                ELSE                                                      
118000                    IF  W-CMD         = 'S  '                             
118100                    OR  W-CMD         = 'X  '                             
118200                    OR  W-CMD         = 'MIS'                             
118300                    OR  W-CMD         = 'RET'                             
118400                    OR  W-CMD         = 'LOS'                             
118500                    OR  W-CMD         = 'DCR'                             
118600                    OR  W-CMD         = 'CAN'                             
118700                    OR  W-CMD         = 'PR '                             
118800                    OR  W-CMD         = 'PRL'                             
118900                    OR  W-CMD         = 'LOC'                             
119000                    OR  W-CMD         = 'BIN'                             
119100                        MOVE NEJ     TO INDATA-SW                         
119200                        MOVE MFS-ALFA-FAELT-FEL                           
119300                                     TO MOD-CMD-ATTR (INDX)               
119400                    END-IF                                                
119500                END-IF                                                    
119600            END-IF                                                        
119700                                                                          
119800            IF MID-ADINLOMR(INDX) NOT = ALL '+'                           
119900               MOVE JA TO SW-ADINLOMR                                     
120000               MOVE MFS-ALFA-FAELT-RAETT                                  
120100                                    TO MOD-ADINLOMR-ATTR(INDX)            
120200            END-IF                                                        
120300                                                                          
120400         ADD 1            TO INDX                                         
120500     END-PERFORM                                                          
120600                                                                          
120700     IF MID-IDUSER-003 NOT = ALL '+'                                      
120800        MOVE JA TO SW-IDUSER                                              
120900        MOVE MFS-ALFA-FAELT-RAETT                                         
121000                 TO MOD-IDUSER-ATTR                                       
121100     END-IF                                                               
121200                                                                          
121300     IF MID-ADINLOMR-PRT NOT = ALL '+'                                    
121400        MOVE MFS-ALFA-FAELT-RAETT                                         
121500                                 TO MOD-ADINLOMR-PRT-ATTR                 
121600     END-IF                                                               
121700                                                                          
121800     IF W-CMD = 'PR ' OR 'PRL'                                            
121900        IF MID-ADINLOMR-PRT = ALL '+' OR SPACE                            
122000           CONTINUE                                                       
122100        ELSE                                                              
122200           PERFORM EAA-CHECK-ADINLOMR-PRT                                 
122300        END-IF                                                            
122400     END-IF                                                               
122500                                                                          
122600     IF  INDATA-FEL                                                       
122700         IF MED-IDMFSFEL = SPACE                                          
122800            MOVE ERR-CORR-HILITE-FLDS                                     
122900                          TO MED-IDMFSFEL                                 
123000         END-IF                                                           
123100         CALL WMEDKONV USING MED-WMEDAREA                                 
123200                                                                          
123300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
123400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
123500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
123600     END-IF                                                               
123700                                                                          
123800     IF MSGI-IDDC = MID-IDDC-SPAR                                         
123900        CONTINUE                                                          
124000     ELSE                                                                 
124100        IF W-CMD = 'PR ' OR 'BC ' OR 'PRL'                                
124200           MOVE NOTHING-PRINTED-WRONG-DC TO MOD-TEMFSFEL                  
124300           MOVE NEJ TO INDATA-SW                                          
124400        END-IF                                                            
124500     END-IF                                                               
124600                                                                          
124700     .                                                                    
124800     EJECT                                                                
124900 EAA-CHECK-ADINLOMR-PRT SECTION.                                          
125000                                                                          
125100     IF DCS-IDDC NOT = MID-IDDC-SPAR                                      
125200        MOVE MID-IDDC-SPAR       TO W-IDDC-B6                             
125300        PERFORM IMS-GU-WDB601                                             
125400     END-IF                                                               
125500                                                                          
125600     IF DCS-CDC                                                           
125700       MOVE SPACE                TO PRT-IDPRTLST                          
125800       MOVE '6M'                 TO PRT-IDPRTLST (1:2)                    
125900       MOVE MID-ADINLOMR-PRT     TO PRT-IDPRTLST (3:4)                    
126000       MOVE 1                    TO PRT-KDCALL                            
126100       CALL W006PRT           USING PRT-W006PRT                           
126200       IF PRT-KDSVAR = 'F'                                                
126300         MOVE SPACE              TO PRT-IDPRTLST                          
126400         MOVE '6L'               TO PRT-IDPRTLST (1:2)                    
126500         MOVE MID-ADINLOMR-PRT   TO PRT-IDPRTLST (3:4)                    
126600         MOVE 1                  TO PRT-KDCALL                            
126700         CALL W006PRT         USING PRT-W006PRT                           
126800         IF PRT-KDSVAR = 'F'                                              
126900           MOVE NEJ              TO INDATA-SW                             
127000           MOVE ERR-WRONG-PRINTER                                         
127100                                 TO MED-IDMFSFEL                          
127200           MOVE MFS-ALFA-FAELT-FEL                                        
127300                                 TO MOD-ADINLOMR-PRT-ATTR                 
127400         ELSE                                                             
127500           MOVE PRT-IDLTERM      TO WS-IDLTERM                            
127600           MOVE MFS-ALFA-FAELT-RAETT                                      
127700                                 TO MOD-ADINLOMR-PRT-ATTR                 
127800         END-IF                                                           
127900       ELSE                                                               
128000         MOVE PRT-IDLTERM        TO WS-IDLTERM                            
128100         MOVE MFS-ALFA-FAELT-RAETT                                        
128200                                 TO MOD-ADINLOMR-PRT-ATTR                 
128300       END-IF                                                             
128400     ELSE                                                                 
128500       MOVE NEJ                  TO INDATA-SW                             
128600       MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADINLOMR-PRT-ATTR                 
128700       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
128800     END-IF                                                               
128900     .                                                                    
129000     EJECT                                                                
129100 EB-MID-INDATA-TILL-MOD SECTION.                                          
129200                                                                          
129300* * * * * FÖR VARJE MID-FÄLT                                              
129400* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
129500* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
129600* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
129700                                                                          
129800     IF MID-IDUSER-003 = ALL '+'                                          
129900        MOVE MFS-RENSA-FAELT TO MOD-IDUSER-003                            
130000     ELSE                                                                 
130100        MOVE MID-IDUSER-003 TO MOD-IDUSER-003                             
130200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDUSER-ATTR                     
130300     END-IF                                                               
130400                                                                          
130500     IF MID-ADINLOMR-PRT NOT = ALL '+'                                    
130600       MOVE MID-ADINLOMR-PRT     TO MOD-ADINLOMR-PRT                      
130700       MOVE MFS-ADD-LAES-IN-FAELT                                         
130800                                 TO MOD-ADINLOMR-PRT-ATTR                 
130900     ELSE                                                                 
131000       MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR-PRT                      
131100     END-IF                                                               
131200                                                                          
131300     MOVE +1              TO INDX                                         
131400     PERFORM UNTIL INDX > MAX-INDX                                        
131500         IF  MID-CMD-IN (INDX) = ALL '+'                                  
131600             MOVE MFS-RENSA-FAELT TO MOD-CMD-IN (INDX)                    
131700         ELSE                                                             
131800             MOVE MID-CMD-IN (INDX) TO MOD-CMD-IN (INDX)                  
131900             MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR (INDX)            
132000         END-IF                                                           
132100         IF  MID-ADINLOMR (INDX) = ALL '+'                                
132200             MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR(INDX)                   
132300         ELSE                                                             
132400             MOVE MID-ADINLOMR(INDX) TO MOD-ADINLOMR(INDX)                
132500             MOVE MFS-ADD-LAES-IN-FAELT                                   
132600                            TO MOD-ADINLOMR-ATTR (INDX)                   
132700         END-IF                                                           
132800         ADD +1           TO INDX                                         
132900     END-PERFORM                                                          
133000     .                                                                    
133100     EJECT                                                                
133200 EC-SKAPA-PRINTER-TRANS SECTION.                                          
133300                                                                          
133400     MOVE W-IDFAKT             TO URV-IDFAKT(URV-IX)                      
133500     INSPECT MID-IDKUNDRF(INDX) REPLACING LEADING SPACE BY ZERO           
133600     MOVE MID-IDKUNDRF(INDX)   TO URV-IDKUNDRF(URV-IX)                    
133700     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
133800     MOVE ZERO                 TO URV-IDKUNDNR(URV-IX)                    
133900     MOVE MID-IDKUNDNR(INDX)   TO URV-IDKUNDNR(URV-IX)                    
134000     MOVE ZERO                 TO URV-IDKOLLI(URV-IX)                     
134100     INSPECT MID-IDKOLLI(INDX) REPLACING LEADING SPACE BY ZERO            
134200     MOVE MID-IDKOLLI(INDX)    TO URV-IDKOLLI(URV-IX)                     
134300     MOVE MID-CMD-IN(INDX)     TO URV-CMD(URV-IX)                         
134400     MOVE MSGI-KDMATT          TO URV-KDMATT(URV-IX)                      
134500                                                                          
134600     MOVE MID-IDDC-SPAR   TO SPAR-WS-IDDC                                 
134700                             W-IDDC-B6                                    
134800     PERFORM IMS-GU-WDB601                                                
134900     IF WS-IDLTERM = SPACE                                                
135000       MOVE DCS-IDPRTLST-INLA    TO PRT-IDPRTLST                          
135100       MOVE 1                    TO PRT-KDCALL                            
135200       CALL W006PRT           USING PRT-W006PRT                           
135300       MOVE PRT-IDLTERM          TO URV-IDPRINTER                         
135400     ELSE                                                                 
135500       MOVE WS-IDLTERM           TO URV-IDPRINTER                         
135600     END-IF                                                               
135700                                                                          
135800     EJECT                                                                
135900**************************                                                
136000     .                                                                    
136100     EJECT                                                                
136200 ED-SKAPA-BC-TRANS SECTION.                                               
136300                                                                          
136400     MOVE W-IDFAKT             TO BC-URV-IDFAKT                           
136500     INSPECT MID-IDKUNDRF(INDX) REPLACING LEADING SPACE BY ZERO           
136600     MOVE MID-IDKUNDRF(INDX)   TO BC-URV-IDKUNDRF                         
136700     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
136800     MOVE ZERO                 TO BC-URV-IDKUNDNR                         
136900     MOVE MID-IDKUNDNR(INDX)   TO BC-URV-IDKUNDNR                         
137000     INSPECT MID-IDKOLLI(INDX) REPLACING LEADING SPACE BY ZERO            
137100     MOVE MID-IDKOLLI(INDX)    TO BC-URV-IDKOLLI                          
137200                                                                          
137300     MOVE '6302'   TO MSGSOP-IDTRANS                                      
137400     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
137500     MOVE 'W612S3' TO MSGSOP-IDPROCESS                                    
137600     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
137700                                                                          
137800     STRING 'URVAL(' WS-BC ')'                                            
137900             DELIMITED BY SIZE INTO MSGSOP-TESYMBV                        
138000                                                                          
138100     PERFORM IMS-INSERT-ALTMSG                                            
138200     MOVE INF-PRINT-BEGAERD TO MED-IDMFSINF                               
138300     CALL WMEDKONV USING MED-WMEDAREA                                     
138400     MOVE MED-TEMFSINF      TO MOD-TEMFSFEL                               
138500     .                                                                    
138600     EJECT                                                                
138700 EF-STARTA-PRINTER-TRANS SECTION.                                         
138800                                                                          
138900     MOVE '6302'   TO MSGSOP-IDTRANS                                      
139000     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
139100     MOVE 'W612S4' TO MSGSOP-IDPROCESS                                    
139200     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
139300                                                                          
139400     MOVE SPACE    TO MSGSOP-TESYMBV                                      
139500                                                                          
139600     STRING 'U1(' WS-URVAL-RAD (1) ') '                                   
139700            'U2(' WS-URVAL-RAD (2) ') '                                   
139800            'U3(' WS-URVAL-RAD (3) ') '                                   
139900            'U4(' WS-URVAL-RAD (4) ') '                                   
140000            'U5(' WS-URVAL-RAD (5) ') '                                   
140100            'U6(' WS-URVAL-RAD (6) ') '                                   
140200            'U7(' WS-URVAL-RAD (7) ') '                                   
140300            'U8(' WS-URVAL-RAD (8) ') '                                   
140400            'U9(' WS-URVAL-RAD (9) ') '                                   
140500            'U10(' WS-URVAL-RAD (10) ') '                                 
140600            'U11(' WS-URVAL-RAD (11) ') '                                 
140700            'U12(' WS-URVAL-RAD (12) ') '                                 
140800            'PRT(' WS-PRINTER ') '                                        
140900              DELIMITED BY SIZE INTO MSGSOP-TESYMBV                       
141000                                                                          
141100     PERFORM IMS-INSERT-ALTMSG                                            
141200     MOVE INF-PRINT-BEGAERD TO MED-IDMFSINF                               
141300     CALL WMEDKONV USING MED-WMEDAREA                                     
141400     MOVE MED-TEMFSINF      TO MOD-TEMFSFEL                               
141500     .                                                                    
141600     EJECT                                                                
141700 F-LAES-VISA-INFO SECTION.                                                
141800                                                                          
141900******* TEST                                                              
142000     IF MFS-UPDATE                                                        
142100        MOVE MID-IDKUNDRF-ENTER TO W-IDKUNDRF                             
142200                                   W-SEQA-IDKUNDRF-MIN                    
142300                                                                          
142400        INSPECT MID-IDKUNDNR-ENTER                                        
142500                             REPLACING LEADING SPACE BY ZERO              
142600        IF  MID-IDKUNDNR-ENTER NUMERIC                                    
142700            MOVE MID-IDKUNDNR-ENTER                                       
142800                             TO W-IDKUNDNR                                
142900                                W-SEQA-IDKUNDNR-MIN                       
143000        ELSE                                                              
143100            MOVE ZERO        TO W-IDKUNDNR                                
143200        END-IF                                                            
143300                                                                          
143400        INSPECT MID-IDKOLLI-ENTER                                         
143500                                REPLACING LEADING SPACE BY ZERO           
143600        IF  MID-IDKOLLI-ENTER NUMERIC                                     
143700            MOVE MID-IDKOLLI-ENTER                                        
143800                            TO W-IDKOLLI                                  
143900                               W-SEQA-IDKOLLI-MIN                         
144000        ELSE                                                              
144100              MOVE ZERO     TO W-IDKOLLI                                  
144200        END-IF                                                            
144300     END-IF                                                               
144400     MOVE HIGH-VALUE      TO W-WDL6ASEQ-MAX                               
144500********** TEST                                                           
144600                                                                          
144700     MOVE +1              TO INDX                                         
144800     MOVE W-IDFAKT        TO W-SEQA-IDFAKT-MIN                            
144900                             W-SEQA-IDFAKT-MAX                            
145000                                                                          
145100     PERFORM IMS-GU-INLC-WLINLC11-F                                       
145200     IF  SEGMENT-FINNS                                                    
145300         MOVE INL-IDKUNDRF   TO MOD-IDKUNDRF-ENTER                        
145400         MOVE INL-IDKUNDNR   TO MOD-IDKUNDNR-ENTER                        
145500         MOVE INL-IDKOLLI    TO MOD-IDKOLLI-ENTER                         
145600         MOVE INL-IDDC       TO MOD-IDDC-SPAR                             
145700                                WS-IDDC                                   
145800     ELSE                                                                 
145900         MOVE INF-URVAL-SAKNAS TO MED-IDMFSFEL                            
146000         CALL WMEDKONV USING MED-WMEDAREA                                 
146100         MOVE MED-MFSFEL    TO MOD-TEMFSFEL                               
146200         MOVE SPACE         TO MOD-IDKUNDRF-ENTER                         
146300                               MOD-IDDC-SPAR                              
146400         MOVE ZERO          TO MOD-IDKUNDNR-ENTER                         
146500                               MOD-IDKOLLI-ENTER                          
146600     END-IF                                                               
146700                                                                          
146800     PERFORM UNTIL INDX > MAX-INDX                                        
146900                                                                          
147000         IF  SEGMENT-FINNS                                                
147100             MOVE INL-IDKUNDRF TO MOD-IDKUNDRF (INDX)                     
147200                                  W-SPAR-IDKUNDRF                         
147300             INSPECT MOD-IDKUNDRF(INDX) REPLACING LEADING                 
147400                          ZERO BY SPACE                                   
147500                                                                          
147600             MOVE INL-IDKUNDNR TO MOD-IDKUNDNR (INDX)                     
147700                                  W-SPAR-IDKUNDNR                         
147800             MOVE INL-IDKOLLI  TO MOD-IDKOLLI  (INDX)                     
147900                                  W-SPAR-IDKOLLI                          
148000             MOVE INL-KDKOLLI  TO MOD-KDKOLLI  (INDX)                     
148100                                                                          
148200             IF WS-IDDC NOT = DCS-IDDC                                    
148300                MOVE WS-IDDC TO W-IDDC-B6                                 
148400                PERFORM IMS-GU-WDB601                                     
148500             END-IF                                                       
148600             IF DCS-NDC-PF OR DCS-CDC OR DCS-NDC-OTHERS                   
148700***BODIL     IF MID-IDDC-SPAR = '61' OR '62'                              
148800                IF MID-ADINLOMR(INDX) = ALL '+'                           
148900                OR (INDATA-SW = JA AND MFS-UPDATE)                        
149000                   MOVE INL-ADINLOMR TO MOD-ADINLOMR(INDX)                
149100                   MOVE MFS-FORMATETS-ATTR                                
149200                                     TO MOD-ADINLOMR-ATTR(INDX)           
149300                END-IF                                                    
149400             ELSE                                                         
149500                MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR(INDX)                
149600             END-IF                                                       
149700                                                                          
149800             MOVE MFS-RENSA-FAELT  TO MOD-TEINFO (INDX)                   
149900             IF INL-FLSKAKOL = 'J'                                        
150000                MOVE 'DCR    ' TO MOD-TEINFO (INDX)                       
150100             END-IF                                                       
150200             IF INL-IDPTYP     =  'R30' AND                               
150300                INL-TIINLMOT   >   ZERO                                   
150400                MOVE 'MISSING' TO MOD-TEINFO (INDX)                       
150500             END-IF                                                       
150600                                                                          
150700             MOVE ZERO TO W-KVRADER                                       
150800                          W-KVNYART                                       
150900                          W-KVPRIOART                                     
151000                          W-ANTAL-LASN                                    
151100                                                                          
151200             PERFORM UNTIL SEGMENT-SAKNAS                                 
151300                      OR  W-SPAR-IDKUNDRF NOT = INL-IDKUNDRF              
151400                      OR  W-SPAR-IDKUNDNR NOT = INL-IDKUNDNR              
151500                      OR  W-SPAR-IDKOLLI  NOT = INL-IDKOLLI               
151600                 ADD 1      TO W-KVRADER                                  
151700                                                                          
151800                 IF INL-FLPRIO = 'J' OR 'Y'                               
151900                    ADD +1  TO W-KVPRIOART                                
152000                 END-IF                                                   
152100                 PERFORM S01-LAS-FRAM-ARTIKEL                             
152200                 ADD +1 TO W-ANTAL-LASN                                   
152300                 IF INL-ADLAGOMR = ZERO                                   
152400                    IF DCS-CDC                                            
152500                       PERFORM IMS-GU-WDK611                              
152600                       IF CLAG-ADLAGOMR = ZERO                            
152700                          ADD +1 TO W-KVNYART                             
152800                       END-IF                                             
152900                    ELSE                                                  
153000                       PERFORM IMS-GU-ARTS11                              
153100                       IF SLAG-ADLAGOMR = ZERO                            
153200                          ADD +1 TO W-KVNYART                             
153300                       END-IF                                             
153400                    END-IF                                                
153500                 END-IF                                                   
153600                 PERFORM IMS-GN-INLC-WLINLC11                             
153700             END-PERFORM                                                  
153800                                                                          
153900             MOVE W-KVRADER   TO MOD-IDARTNR-KOLLI (INDX)                 
154000             MOVE W-KVNYART   TO MOD-IDARTNR-NEW   (INDX)                 
154100             MOVE W-KVPRIOART TO MOD-IDARTNR-PRIO  (INDX)                 
154200                                                                          
154300         ELSE                                                             
154400             MOVE MFS-STAENG-FAELT TO MOD-CMD-ATTR     (INDX)             
154500                                      MOD-ADINLOMR-ATTR(INDX)             
154600             MOVE MFS-RENSA-FAELT  TO MOD-CMD-IN       (INDX)             
154700                                      MOD-IDKUNDRF     (INDX)             
154800                                      MOD-IDKUNDNR     (INDX)             
154900                                      MOD-IDKOLLI      (INDX)             
155000                                      MOD-KDKOLLI      (INDX)             
155100                                      MOD-IDARTNR-KOLLI(INDX)             
155200                                      MOD-IDARTNR-NEW  (INDX)             
155300                                      MOD-IDARTNR-PRIO (INDX)             
155400                                      MOD-TEINFO       (INDX)             
155500                                      MOD-ADINLOMR     (INDX)             
155600         END-IF                                                           
155700                                                                          
155800         ADD 1            TO INDX                                         
155900     END-PERFORM                                                          
156000                                                                          
156100     IF  SEGMENT-FINNS                                                    
156200         MOVE INL-IDKUNDRF TO MOD-IDKUNDRF-NEXT                           
156300         MOVE INL-IDKUNDNR TO MOD-IDKUNDNR-NEXT                           
156400         MOVE INL-IDKOLLI  TO MOD-IDKOLLI-NEXT                            
156500         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
156600         CALL WMEDKONV USING MED-WMEDAREA                                 
156700                                                                          
156800         IF  W-TEMFSINF       > SPACE                                     
156900            MOVE W-TEMFSINF   TO MOD-TEMFSINF                             
157000         ELSE                                                             
157100            MOVE MED-TEMFSINF TO MOD-TEMFSINF                             
157200         END-IF                                                           
157300                                                                          
157400     ELSE                                                                 
157500         IF  W-TEMFSINF     > SPACE                                       
157600             MOVE W-TEMFSINF  TO MOD-TEMFSINF                             
157700         END-IF                                                           
157800                                                                          
157900         MOVE SPACE         TO MOD-IDKUNDRF-NEXT                          
158000         MOVE ZERO          TO MOD-IDKUNDNR-NEXT                          
158100                               MOD-IDKOLLI-NEXT                           
158200     END-IF                                                               
158300     .                                                                    
158400     EJECT                                                                
158500 G-KOLLA-INPUT SECTION.                                                   
158600                                                                          
158700     IF MID-IDDC-SPAR = SPACE                                             
158800        MOVE NEJ TO INDATA-SW                                             
158900     ELSE                                                                 
159000        MOVE JA TO INDATA-SW                                              
159100        MOVE MID-IDDC-SPAR TO WS-IDDC                                     
159200                              W-IDDC-B6                                   
159300        PERFORM IMS-GU-WDB601-SPAR                                        
159401                                                                          
159501        IF SPAR-DCS-USA                                                   
159601          SET AKTUELLT-LAND-USA TO TRUE                                   
159701        ELSE                                                              
159801          SET AKTUELLT-EJ-USA   TO TRUE                                   
159901        END-IF                                                            
160000        MOVE SPAR-DCS-FLINLREP TO SPAR-FLINLREP                           
160100                                                                          
160200        IF  MID-CMD-IN (1) = ALL '+'                                      
160300        AND MID-CMD-IN (2) = ALL '+'                                      
160400        AND MID-CMD-IN (3) = ALL '+'                                      
160500        AND MID-CMD-IN (4) = ALL '+'                                      
160600        AND MID-CMD-IN (5) = ALL '+'                                      
160700        AND MID-CMD-IN (6) = ALL '+'                                      
160800        AND MID-CMD-IN (7) = ALL '+'                                      
160900        AND MID-CMD-IN (8) = ALL '+'                                      
161000        AND MID-CMD-IN (9) = ALL '+'                                      
161100        AND MID-CMD-IN (10) = ALL '+'                                     
161200        AND MID-CMD-IN (11) = ALL '+'                                     
161300        AND MID-CMD-IN (12) = ALL '+'                                     
161400        AND MID-ADINLOMR(1) = ALL '+'                                     
161500        AND MID-ADINLOMR(2) = ALL '+'                                     
161600        AND MID-ADINLOMR(3) = ALL '+'                                     
161700        AND MID-ADINLOMR(4) = ALL '+'                                     
161800        AND MID-ADINLOMR(5) = ALL '+'                                     
161900        AND MID-ADINLOMR(6) = ALL '+'                                     
162000        AND MID-ADINLOMR(7) = ALL '+'                                     
162100        AND MID-ADINLOMR(8) = ALL '+'                                     
162200        AND MID-ADINLOMR(9) = ALL '+'                                     
162300        AND MID-ADINLOMR(10) = ALL '+'                                    
162400        AND MID-ADINLOMR(11) = ALL '+'                                    
162500        AND MID-ADINLOMR(12) = ALL '+'                                    
162600           MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                      
162700           MOVE NEJ TO INDATA-SW                                          
162800           IF MID-IDUSER-003  = ALL '+' OR SPACE                          
162900              CONTINUE                                                    
163000           ELSE                                                           
163100              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-ATTR                  
163200           END-IF                                                         
163300           IF MID-ADINLOMR-PRT = ALL '+' OR SPACE                         
163400              CONTINUE                                                    
163500           ELSE                                                           
163600              MOVE MFS-ALFA-FAELT-FEL                                     
163700                                 TO MOD-ADINLOMR-PRT-ATTR                 
163800           END-IF                                                         
163900        ELSE                                                              
164000                                                                          
164100           MOVE +1              TO INDX                                   
164200           PERFORM UNTIL INDX   > MAX-INDX                                
164300                                                                          
164400               IF  MID-CMD-IN (INDX)  NOT = ALL '+'                       
164500               AND MID-CMD-IN (INDX)  NOT = SPACE                         
164600                                                                          
164700                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR(INDX)        
164800                                                                          
164900                   IF  MID-CMD-IN (INDX) NOT = 'LOS'                      
165000                   AND MID-CMD-IN (INDX) NOT = 'MIS'                      
165100                   AND MID-CMD-IN (INDX) NOT = 'RET'                      
165200                   AND MID-CMD-IN (INDX) NOT = 'DCR'                      
165300                   AND MID-CMD-IN (INDX) NOT = 'CAN'                      
165400                   AND MID-CMD-IN (INDX) NOT = 'LOC'                      
165500                   AND MID-CMD-IN (INDX) NOT = 'BIN'                      
165600                       MOVE MFS-ALFA-FAELT-FEL                            
165700                                TO MOD-CMD-ATTR (INDX)                    
165800                       MOVE ERR-CORR-HILITE-FLDS                          
165900                                TO MED-IDMFSFEL                           
166000                       MOVE NEJ TO INDATA-SW                              
166100                   ELSE                                                   
166200                      IF MID-CMD-IN(INDX) = 'RET'                         
166300                      AND (SPAR-DCS-NDC-NA OR SPAR-DCS-NDC-PF OR          
166310                           SPAR-DCS-NDC-OTHERS)                           
166400                         MOVE MFS-ALFA-FAELT-FEL                          
166500                                      TO MOD-CMD-ATTR (INDX)              
166600                         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL        
166700                         MOVE NEJ TO INDATA-SW                            
166800                      ELSE                                                
166900                         IF MID-CMD-IN(INDX) = 'LOC'                      
167000                            IF SPAR-DCS-SDC                               
167100                            OR SPAR-DCS-NDC-NA                            
167200                               MOVE MFS-ALFA-FAELT-FEL                    
167300                                         TO MOD-CMD-ATTR (INDX)           
167400                               MOVE ERR-CORR-HILITE-FLDS                  
167500                                    TO MED-IDMFSFEL                       
167600                               MOVE NEJ TO INDATA-SW                      
167700                            ELSE                                          
167800                               PERFORM GA-KONTROLL-AV-PTYP                
167900                            END-IF                                        
168000                         ELSE                                             
168100                            IF MID-CMD-IN(INDX) = 'BIN'                   
168200                               PERFORM GB-KOLLA-BIN                       
168300                            ELSE                                          
168400                               PERFORM GA-KONTROLL-AV-PTYP                
168500                            END-IF                                        
168600                         END-IF                                           
168700                      END-IF                                              
168800                   END-IF                                                 
168900               ELSE                                                       
169000                   MOVE MFS-RENSA-FAELT TO MOD-CMD-ATTR (INDX)            
169100               END-IF                                                     
169200                                                                          
169300               IF MID-ADINLOMR(INDX) = ALL '+' OR SPACE                   
169400                  MOVE MFS-ALFA-FAELT-RAETT                               
169500                                 TO MOD-ADINLOMR-ATTR(INDX)               
169600                  IF MID-CMD-IN(INDX) = 'LOC'                             
169700                     MOVE MFS-ALFA-FAELT-FEL                              
169800                                      TO MOD-CMD-ATTR (INDX)              
169900                     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL            
170000                     MOVE NEJ TO INDATA-SW                                
170100                  END-IF                                                  
170200               ELSE                                                       
170300                  MOVE MFS-ALFA-FAELT-RAETT                               
170400                                 TO MOD-ADINLOMR-ATTR(INDX)               
170500                  IF SPAR-DCS-CDC                                         
170600                     MOVE MID-ADINLOMR (INDX)                             
170700                                 TO W-ADINLOMR-6006                       
170800                     PERFORM IMS-GU-W6G130                                
170900                     IF SEGMENT-SAKNAS                                    
171000                        MOVE NEJ TO INDATA-SW                             
171100                        MOVE MFS-ALFA-FAELT-FEL                           
171200                                 TO MOD-ADINLOMR-ATTR(INDX)               
171300                        MOVE UPDATING-NOT-ALLOWED                         
171400                                 TO MED-IDMFSFEL                          
171500                     END-IF                                               
171600                  END-IF                                                  
171700                  IF MID-CMD-IN(INDX) = 'LOC'                             
171800                     IF SPAR-DCS-NDC-PF OR SPAR-DCS-CDC OR                
171810                        SPAR-DCS-NDC-OTHERS                               
171900                        CONTINUE                                          
172000                     ELSE                                                 
172100                        MOVE MFS-ALFA-FAELT-FEL                           
172200                                 TO MOD-CMD-ATTR (INDX)                   
172300                        MOVE ERR-CORR-HILITE-FLDS                         
172400                                 TO MED-IDMFSFEL                          
172500                        MOVE NEJ TO INDATA-SW                             
172600                     END-IF                                               
172700                  ELSE                                                    
172800                     MOVE MFS-ALFA-FAELT-FEL                              
172900                                 TO MOD-CMD-ATTR (INDX)                   
173000                     MOVE ERR-CORR-HILITE-FLDS                            
173100                                 TO MED-IDMFSFEL                          
173200                     MOVE NEJ    TO INDATA-SW                             
173300                  END-IF                                                  
173400               END-IF                                                     
173500                                                                          
173600               IF MID-IDUSER-003 = ALL '+' OR SPACE                       
173700                  IF MID-CMD-IN(INDX) = 'BIN' OR 'LOS' OR 'MIS'           
173800                                         OR 'RET'                         
173900                     IF SPAR-DCS-FLBINNUT = JA                            
174000                        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDUSER-ATTR        
174100                        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL         
174200                        MOVE NEJ TO INDATA-SW                             
174300                     ELSE                                                 
174400                        MOVE SPACE TO WS-IDUSER-003                       
174500                     END-IF                                               
174600                  ELSE                                                    
174700                     MOVE SPACE TO WS-IDUSER-003                          
174800                  END-IF                                                  
174900               ELSE                                                       
175000                  MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDUSER-ATTR            
175100                  MOVE MID-IDUSER-003 TO WS-IDUSER-003                    
175200               END-IF                                                     
175300                                                                          
175400               ADD 1 TO INDX                                              
175500            END-PERFORM                                                   
175600        END-IF                                                            
175700     END-IF                                                               
175800                                                                          
175900     IF INDATA-FEL                                                        
176000        CALL WMEDKONV USING MED-WMEDAREA                                  
176100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
176200        PERFORM MFS-ROER-EJ-FAELT-UT                                      
176300        PERFORM MFS-ROER-EJ-FAELT-IN                                      
176400     END-IF                                                               
176500                                                                          
176600     IF MID-IDDC-SPAR = MSGI-IDDC                                         
176700        CONTINUE                                                          
176800     ELSE                                                                 
176900        MOVE UPDATING-NOT-ALLOWED-WRONG-DC TO MOD-TEMFSFEL                
177000        MOVE NEJ TO INDATA-SW                                             
177100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
177200        PERFORM MFS-ROER-EJ-FAELT-IN                                      
177300     END-IF                                                               
177400     .                                                                    
177500     EJECT                                                                
177600 GA-KONTROLL-AV-PTYP SECTION.                                             
177700                                                                          
177800     MOVE LOW-VALUE           TO W-WDL6ASEQ-MIN                           
177900     MOVE HIGH-VALUE          TO W-WDL6ASEQ-MAX                           
178000     MOVE W-IDFAKT            TO W-SEQA-IDFAKT-MIN                        
178100                                 W-SEQA-IDFAKT-MAX                        
178200     INSPECT MID-IDKUNDRF(INDX) REPLACING LEADING SPACE BY ZERO           
178300     MOVE MID-IDKUNDRF (INDX) TO W-SEQA-IDKUNDRF-MIN                      
178400                                 W-SEQA-IDKUNDRF-MAX                      
178500     MOVE MID-IDKUNDNR (INDX) TO W-SEQA-IDKUNDNR-MIN                      
178600                                 W-SEQA-IDKUNDNR-MAX                      
178700     MOVE MID-IDKOLLI  (INDX) TO W-SEQA-IDKOLLI-MIN                       
178800                                 W-SEQA-IDKOLLI-MAX                       
178900                                                                          
179000     PERFORM IMS-GN-INLC-WLINLC11                                         
179100                                                                          
179200     PERFORM UNTIL SEGMENT-SAKNAS                                         
179300                                                                          
179400         IF  MID-CMD-IN (INDX) = 'LOS'                                    
179500         AND INL-IDPTYP       NOT = 'R30'                                 
179600             MOVE MFS-ALFA-FAELT-FEL                                      
179700                             TO MOD-CMD-ATTR (INDX)                       
179800             MOVE ERR-CORR-HILITE-FLDS                                    
179900                             TO MED-IDMFSFEL                              
180000             MOVE NEJ         TO INDATA-SW                                
180100         END-IF                                                           
180200*********                                                                 
180300         IF  MID-CMD-IN (INDX) = 'LOS'                                    
180400         AND INL-TIINLMOT     NOT > ZERO                                  
180500             MOVE MFS-ALFA-FAELT-FEL                                      
180600                             TO MOD-CMD-ATTR (INDX)                       
180700             MOVE ERR-CORR-HILITE-FLDS                                    
180800                             TO MED-IDMFSFEL                              
180900             MOVE NEJ         TO INDATA-SW                                
181000         END-IF                                                           
181100*********                                                                 
181200                                                                          
181300         IF (MID-CMD-IN (INDX) = 'MIS'                                    
181400         OR  MID-CMD-IN (INDX) = 'RET'                                    
181500         OR  MID-CMD-IN (INDX) = 'DCR'                                    
181600         OR  MID-CMD-IN (INDX) = 'BIN'                                    
181700         OR  MID-CMD-IN (INDX) = 'CAN')                                   
181800         AND INL-IDPTYP       NOT = '310'                                 
181900             MOVE MFS-ALFA-FAELT-FEL                                      
182000                             TO MOD-CMD-ATTR (INDX)                       
182100             MOVE ERR-CORR-HILITE-FLDS                                    
182200                             TO MED-IDMFSFEL                              
182300             MOVE NEJ        TO INDATA-SW                                 
182401         ELSE                                                             
182501             IF MID-CMD-IN (INDX) = 'RET'                                 
182601               MOVE INL-IDDISTR TO DIST35-IDDISTR                         
182701               IF DIST35-RETUR                                            
182801                 MOVE MFS-ALFA-FAELT-FEL                                  
182901                             TO MOD-CMD-ATTR (INDX)                       
183001                 MOVE ERR-CORR-HILITE-FLDS                                
183101                             TO MED-IDMFSFEL                              
183201                 MOVE NEJ    TO INDATA-SW                                 
183301               END-IF                                                     
183401             END-IF                                                       
183500         END-IF                                                           
183600                                                                          
183700         PERFORM IMS-GN-INLC-WLINLC11                                     
183800     END-PERFORM                                                          
183900     .                                                                    
184000     EJECT                                                                
184100 GB-KOLLA-BIN SECTION.                                                    
184200                                                                          
184300     PERFORM S02-SKAPA-WDL6A1KY                                           
184400     PERFORM IMS-GU-WLINLD01                                              
184500                                                                          
184600     IF SEGMENT-SAKNAS                                                    
184700        MOVE MFS-ALFA-FAELT-FEL                                           
184800                       TO MOD-CMD-ATTR (INDX)                             
184900        MOVE ERR-CORR-HILITE-FLDS                                         
185000                       TO MED-IDMFSFEL                                    
185100        MOVE NEJ       TO INDATA-SW                                       
185200     END-IF                                                               
185300                                                                          
185400     PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                           
185500                                                                          
185600         IF SEQA-IDPTYP NOT = '310'                                       
185700             MOVE MFS-ALFA-FAELT-FEL                                      
185800                             TO MOD-CMD-ATTR (INDX)                       
185900             MOVE ERR-CORR-HILITE-FLDS                                    
186000                             TO MED-IDMFSFEL                              
186100             MOVE NEJ        TO INDATA-SW                                 
186200         ELSE                                                             
186300            MOVE SEQA-IDARTNR   TO W-IDARTNR                              
186400            MOVE SEQA-IDDC      TO W-IDDC                                 
186500            IF SPAR-DCS-CDC                                               
186600               PERFORM IMS-GHU-WDK611                                     
186700            ELSE                                                          
186800               PERFORM IMS-GHU-ARTS11                                     
186900            END-IF                                                        
187000                                                                          
187100            ADD +1 TO WS-KVBINART                                         
187200                                                                          
187300            IF WS-KVBINART > WS-KVBINART-MAX                              
187400                MOVE NEJ       TO INDATA-SW                               
187500                MOVE MFS-ALFA-FAELT-FEL                                   
187600                              TO MOD-CMD-ATTR(INDX)                       
187700                MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                 
187800            END-IF                                                        
187900                                                                          
188000            IF SPAR-DCS-CDC                                               
188100               IF CLAG-ADLAGOMR = ZERO                                    
188200                  MOVE NEJ       TO INDATA-SW                             
188300                  MOVE MFS-ALFA-FAELT-FEL                                 
188400                              TO MOD-CMD-ATTR(INDX)                       
188500                  MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL               
188600                  MOVE AREA-MISSING TO MED-IDMFSINF                       
188700                  CALL WMEDKONV USING MED-WMEDAREA                        
188800                  MOVE MED-MFSINF TO W-TEMFSINF                           
188900               END-IF                                                     
189000            ELSE                                                          
189100               IF SLAG-ADLAGOMR  = ZERO                                   
189200                  MOVE NEJ       TO INDATA-SW                             
189300                  MOVE MFS-ALFA-FAELT-FEL                                 
189400                              TO MOD-CMD-ATTR(INDX)                       
189500                  MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL               
189600                  MOVE AREA-MISSING TO MED-IDMFSINF                       
189700                  CALL WMEDKONV USING MED-WMEDAREA                        
189800                  MOVE MED-MFSINF TO W-TEMFSINF                           
189900               END-IF                                                     
190000            END-IF                                                        
190100         END-IF                                                           
190200                                                                          
190300         PERFORM IMS-GN-WLINLD01                                          
190400     END-PERFORM                                                          
190500     .                                                                    
190600     EJECT                                                                
190700 H-UPPDATERA SECTION.                                                     
190800                                                                          
190900     MOVE +1 TO BIN-IX                                                    
191000     PERFORM UNTIL BIN-IX > MAX-INDX                                      
191100        MOVE ALL '+' TO PROGSW-MID-INPUT(BIN-IX)                          
191200        ADD +1 TO BIN-IX                                                  
191300     END-PERFORM                                                          
191400                                                                          
191500     MOVE +1 TO INDX                                                      
191600                BIN-IX                                                    
191700     IF WS-IDDC NOT = DCS-IDDC                                            
191800        MOVE WS-IDDC TO W-IDDC-B6                                         
191900        PERFORM IMS-GU-WDB601                                             
192000     END-IF                                                               
192100     PERFORM UNTIL INDX > MAX-INDX                                        
192200         IF MID-CMD-IN (INDX) = 'MIS'                                     
192300             PERFORM HA-MIS-UPDATERING                                    
192400         ELSE                                                             
192500             IF MID-CMD-IN (INDX) = 'RET'                                 
192600                 PERFORM HB-RET-UPDATERING                                
192700             ELSE                                                         
192800                 IF MID-CMD-IN (INDX) = 'LOS'                             
192900                    IF DCS-SDC OR DCS-NDC-PF OR DCS-CDC OR                
192910                       DCS-NDC-OTHERS                                     
193000                       PERFORM HC-LOS-UPDATERING                          
193100                    ELSE                                                  
193200                       PERFORM HD-LOS-UPDATERING                          
193300                    END-IF                                                
193400                 ELSE                                                     
193500                    IF MID-CMD-IN (INDX) = 'DCR'                          
193600                       PERFORM HE-DCR-UPDATERING                          
193700                    ELSE                                                  
193800                       IF MID-CMD-IN (INDX) = 'CAN'                       
193900                          PERFORM HF-CAN-UPDATERING                       
194000                       ELSE                                               
194100                          IF MID-CMD-IN (INDX) = 'LOC'                    
194200                             PERFORM HG-LOC-UPDATERING                    
194300                          ELSE                                            
194400                             IF MID-CMD-IN (INDX) = 'BIN'                 
194500                                PERFORM HH-BIN-UPDATERING                 
194600                             END-IF                                       
194700                          END-IF                                          
194800                       END-IF                                             
194900                    END-IF                                                
195000                 END-IF                                                   
195100             END-IF                                                       
195200         END-IF                                                           
195300         ADD 1 TO INDX                                                    
195400     END-PERFORM                                                          
195500                                                                          
195600     IF  TRANS-OHUVUD-RET-SKAPAD                                          
195700         MOVE 'J' TO ORAD-MID-FLSLUT                                      
195800         PERFORM X010-CALL-W006KOM                                        
195900     END-IF                                                               
196000                                                                          
196100     IF HOPP-UPDATE                                                       
196200        CONTINUE                                                          
196300     ELSE                                                                 
196400        MOVE INF-UPDATE-DONE TO MED-IDMFSINF                              
196500        CALL WMEDKONV USING MED-WMEDAREA                                  
196600        MOVE MED-MFSINF TO W-TEMFSINF                                     
196700        PERFORM MFS-RENSA-FAELT-IN                                        
196800     END-IF                                                               
196900     .                                                                    
197000     EJECT                                                                
197100 HA-MIS-UPDATERING SECTION.                                               
197200                                                                          
197300     PERFORM S02-SKAPA-WDL6A1KY                                           
197400     PERFORM IMS-GU-WLINLD01                                              
197500                                                                          
197600     PERFORM UNTIL SEGMENT-SAKNAS                                         
197700                                                                          
197800         MOVE SEQA-IDARTNR   TO W-IDARTNR                                 
197900         MOVE SEQA-DAINLEV   TO W-DAINLEV                                 
198000         PERFORM IMS-GHU-INLC1-WLINLC11                                   
198100                                                                          
198200         MOVE INL-IDDC       TO W-IDDC                                    
198300         MOVE INL-KVAVIS     TO W-KVAVIS                                  
198400                                                                          
198500         MOVE 'R30'          TO INL-IDPTYP                                
198700         IF MID-IDUSER-003 = ALL '+' OR SPACE                             
198800            CONTINUE                                                      
198900         ELSE                                                             
199000            MOVE WS-IDUSER-003 TO INL-IDUSER-003                          
199100         END-IF                                                           
199200         PERFORM IMS-REPL-INLC1-WLINLC11                                  
199300                                                                          
199400         IF DCS-CDC                                                       
199500           PERFORM IMS-GHU-WDK611                                         
199600           ADD W-KVAVIS          TO CLAG-KVAKS-PAV                        
199700           SUBTRACT W-KVAVIS   FROM CLAG-KVAKS-CDC                        
199800           PERFORM IMS-REPL-WDK611                                        
199900         ELSE                                                             
200000           PERFORM IMS-GHU-ARTS11                                         
200100           ADD W-KVAVIS          TO SLAG-KVAKS-PAV                        
200200           SUBTRACT W-KVAVIS   FROM SLAG-KVAKS-SDC                        
200300           PERFORM IMS-REPL-ARTS11                                        
200400         END-IF                                                           
200500                                                                          
200600         MOVE '-'     TO LOGG-IDTECKEN-KVAKS                              
200700         MOVE '+'     TO LOGG-IDTECKEN-KVAKS-PAV                          
200800         MOVE ' '     TO LOGG-IDTECKEN-KVLS                               
200900         PERFORM S06-SKAPA-SALDOLOGG                                      
201000                                                                          
201100         PERFORM IMS-GN-WLINLD01                                          
201200     END-PERFORM                                                          
201300                                                                          
201400     MOVE MID-IDDC-SPAR TO W-6301-IDDC                                    
201500     PERFORM IMS-GHU-WL630111                                             
201600     IF SEGMENT-FINNS                                                     
201700        MOVE 'M' TO 6302-KDTRPSTA                                         
201800        PERFORM IMS-REPL-WL630111                                         
201900     END-IF                                                               
202000     .                                                                    
202100     EJECT                                                                
202200 HB-RET-UPDATERING SECTION.                                               
202300                                                                          
202400     PERFORM S02-SKAPA-WDL6A1KY                                           
202500     PERFORM IMS-GU-WLINLD01                                              
202600     MOVE ZERO TO WS-RAKNARE                                              
202700                                                                          
202800     IF  SEGMENT-FINNS                                                    
202900         IF  NOT TRANS-OHUVUD-RET-SKAPAD                                  
203000             PERFORM HBA-SKAPA-TRANS-ORDERHUVUD                           
203100             PERFORM HBB-SKAPA-HUVUD-ORDERRADER                           
203200             MOVE 1           TO ORAD-IX                                  
203300         END-IF                                                           
203400                                                                          
203500         PERFORM UNTIL SEGMENT-SAKNAS                                     
203600             PERFORM UNTIL SEGMENT-SAKNAS                                 
203700                        OR ORAD-IX > ORAD-IX-MAX                          
203800                                                                          
203900                 MOVE SEQA-IDARTNR   TO W-IDARTNR                         
204000                 MOVE SEQA-DAINLEV   TO W-DAINLEV                         
204100                 PERFORM IMS-GHU-INLC1-WLINLC11                           
204200                 MOVE INL-IDDC   TO W-IDDC                                
204300                 MOVE INL-KVAVIS TO W-KVAVIS                              
204400                                    INL-KVANTMOT                          
204500                                                                          
204600                 MOVE 'R32'       TO INL-IDPTYP                           
204700                 IF MID-IDUSER-003 = ALL '+' OR SPACE                     
204800                    CONTINUE                                              
204900                 ELSE                                                     
205000                    MOVE WS-IDUSER-003 TO INL-IDUSER-003                  
205100                 END-IF                                                   
205200                 ADD +1           TO WS-RAKNARE                           
205300                 ACCEPT W-DATUM-X FROM DATE                               
205400                 MOVE W-DATUM       TO INL-TIINLINL                       
205500                 PERFORM IMS-REPL-INLC1-WLINLC11                          
205600                                                                          
205700                 IF DCS-CDC                                               
205800                   PERFORM IMS-GHU-WDK611                                 
205900                   ADD W-KVAVIS  TO CLAG-KVLS                             
206000                   SUBTRACT W-KVAVIS                                      
206100                               FROM CLAG-KVAKS-CDC                        
206200                   PERFORM IMS-REPL-WDK611                                
206300                 ELSE                                                     
206400                   PERFORM IMS-GHU-ARTS11                                 
206500                   ADD W-KVAVIS  TO SLAG-KVLS                             
206600                   SUBTRACT W-KVAVIS                                      
206700                               FROM SLAG-KVAKS-SDC                        
206800                   PERFORM IMS-REPL-ARTS11                                
206900                 END-IF                                                   
207000                                                                          
207100                 MOVE '-'    TO LOGG-IDTECKEN-KVAKS                       
207200                 MOVE ' '    TO LOGG-IDTECKEN-KVAKS-PAV                   
207300                 MOVE '+'    TO LOGG-IDTECKEN-KVLS                        
207400                 PERFORM S06-SKAPA-SALDOLOGG                              
207500                                                                          
207600                 PERFORM HBC-EDIT-TRANS-ORDERRADER                        
207700                                                                          
207800                 ADD +1   TO ORAD-IX                                      
207900                 PERFORM IMS-GN-WLINLD01                                  
208000                                                                          
208100             END-PERFORM                                                  
208200                                                                          
208300             IF  ORAD-IX  > ORAD-IX-MAX                                   
208400             AND SEGMENT-FINNS                                            
208500                 PERFORM X010-CALL-W006KOM                                
208600                                                                          
208700                 MOVE +1  TO ORAD-IX                                      
208800                 PERFORM UNTIL ORAD-IX > ORAD-IX-MAX                      
208900                     MOVE SPACE TO ORAD-MID-RADER (ORAD-IX)               
209000                     ADD +1     TO ORAD-IX                                
209100                 END-PERFORM                                              
209200                 MOVE +1  TO ORAD-IX                                      
209300             END-IF                                                       
209400         END-PERFORM                                                      
209500                                                                          
209600         PERFORM X020-UPPDATERA-WL630111                                  
209700     END-IF                                                               
209800     .                                                                    
209900     EJECT                                                                
210000 HBA-SKAPA-TRANS-ORDERHUVUD SECTION.                                      
210100                                                                          
210200     MOVE JA              TO TRANS-OHUVUD-RET-SKAPAD-SW                   
210300                                                                          
210400     MOVE SPACE           TO MSG-KOM-WMSGKOM                              
210500     MOVE +54             TO MSG-KOM-KVLL                                 
210600     MOVE LOW-VALUE       TO MSG-KOM-KDZ1                                 
210700     MOVE LOW-VALUE       TO MSG-KOM-KDZ2                                 
210800     MOVE SPACE           TO MSG-KOM-KDTRANS                              
210900     MOVE 'W4I25101'      TO MSG-KOM-IDCPYTXT                             
211000     MOVE 'SDC-RET '      TO MSG-KOM-IDSNDNOD                             
211100     MOVE 'W6030200'      TO MSG-KOM-IDSNDJOB                             
211200                                                                          
211300     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
211400     ACCEPT MSG-KOM-TIKLOCK FROM TIME                                     
211500                                                                          
211600     MOVE SPACE           TO MSG-KOM-IDMFSMED                             
211700                             MSG-KOM-KDSVAR                               
211800                                                                          
211900     PERFORM HBAA-DISTRIKT-RETUR                                          
212000                                                                          
212100     MOVE SPACE           TO KOM-AREA                                     
212200                                                                          
212300     COMPUTE P-TO-P-LL =  LNG-P-TO-P-PREFIX +                             
212400                          LENGTH OF OHUV-MID-W4I25101                     
212500                                                                          
212600     MOVE LOW-VALUE              TO P-TO-P-Z1                             
212700     MOVE LOW-VALUE              TO P-TO-P-Z2                             
212800     MOVE 'W4T251X'              TO P-TO-P-TRANSKOD                       
212900     MOVE '4251'                 TO P-TO-P-FROM-MID                       
213000     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
213100                                                                          
213200     MOVE 'W603'                 TO OHUV-MID-IDSYSTEM                     
213300     MOVE W-IDDISTR-RETUR        TO OHUV-MID-IDDISTR                      
213400     MOVE W-IDKUNDNR-RETUR       TO OHUV-MID-IDKUNDNR                     
213500                                                                          
213600     ACCEPT W-DATUM-X FROM DATE                                           
213700     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
213800     MOVE W-DATUM        TO DAT-I-TIDATUM                                 
213900     CALL WDATKONV    USING DAT-KDDATFORM                                 
214000                            DAT-I-TIDATUM                                 
214100                            DAT-O-TIDATUM                                 
214200                            DAT-KDSVAR                                    
214300                                                                          
214400     IF  DAT-KDSVAR   NOT = SPACE                                         
214500         MOVE ' FELAKTIG RETURKOD FRÅN WDATKONV I HBA-'                   
214600                          TO FELTEXT                                      
214700         CALL FELLOG                                                      
214800     END-IF                                                               
214900                                                                          
215000     MOVE DAT-TIVV        TO W-IDORDNR-VV                                 
215100     MOVE DAT-TID         TO W-IDORDNR-D                                  
215200     ACCEPT W-TIME-X      FROM TIME                                       
215300                                                                          
215400     MOVE W-TIME-SS       TO W-IDORDNR-SS                                 
215500                                                                          
215600     MOVE W-IDORDNR-X     TO OHUV-MID-IDORDNR                             
215700     MOVE '1'             TO OHUV-MID-KDORDKL                             
215800                                                                          
215900     MOVE SPACE           TO OHUV-MID-KDFRAKT                             
216000                             OHUV-MID-TIRFS                               
216100     MOVE 'DAMAGED'       TO OHUV-MID-BEKUNDRF                            
216200     MOVE SPACE           TO OHUV-MID-KDFAKTYP                            
216300     MOVE NEJ             TO OHUV-MID-FLRESTN                             
216400     MOVE SPACE           TO OHUV-MID-KDTPOTYP                            
216500                             OHUV-MID-TITPO                               
216600                             OHUV-MID-BELAGINS                            
216700                             OHUV-MID-BEGMT                               
216800                             OHUV-MID-ADGMT-GATA                          
216900                             OHUV-MID-ADGMT-PADR                          
217000                             OHUV-MID-KDROPACK                            
217100                             OHUV-MID-IDKONTO                             
217200                             OHUV-MID-IDKST                               
217300                             OHUV-MID-BEVARREF                            
217400                             OHUV-MID-KDTULLVE                            
217500                             OHUV-MID-KDNOTES                             
217600     MOVE JA              TO OHUV-MID-FLAUTFAK                            
217700     MOVE JA              TO OHUV-MID-FLAUTPAC                            
217800     MOVE NEJ             TO OHUV-MID-FLEMBORD                            
217900     MOVE NEJ             TO OHUV-MID-FLOVRLEV                            
218000     MOVE SPACE           TO OHUV-MID-IDKAMPRF                            
218100                             OHUV-MID-IDFTG                               
218200                             OHUV-MID-ADBET                               
218300                             OHUV-MID-BEBET                               
218400                             OHUV-MID-IDSKYLT                             
218500                             OHUV-MID-FLLSBOK                             
218600                             OHUV-MID-IDANALYS                            
218700     MOVE MID-IDDC-SPAR   TO OHUV-MID-IDDC                                
218800     MOVE ZERO            TO OHUV-MID-IDDEPT                              
218900     MOVE SPACE           TO OHUV-MID-KDORDTYP-LDC                        
219000     MOVE ZERO            TO OHUV-MID-TIREPDAT                            
219100     MOVE NEJ             TO OHUV-MID-FLFORBI                             
219200                             OHUV-MID-FLORDTIL                            
219300     MOVE ZERO            TO OHUV-MID-IDGROSS                             
219400     MOVE SPACE           TO OHUV-MID-IDBILREG                            
219500                             OHUV-MID-IDVIN                               
219600                             OHUV-MID-IDCISNR                             
219700                                                                          
219800     PERFORM X010-CALL-W006KOM                                            
219900     .                                                                    
220000     EJECT                                                                
220100 HBAA-DISTRIKT-RETUR SECTION.                                             
220200                                                                          
220300     MOVE MID-IDDC-SPAR      TO W-IDDC-B6                                 
220400     PERFORM IMS-GU-WDB601                                                
220500     MOVE DCS-IDDISTR-RETUR  TO W-IDDISTR-RETUR                           
220600     MOVE DCS-IDKUNDNR-RETUR TO W-IDKUNDNR-RETUR                          
220700     .                                                                    
220800     EJECT                                                                
220900 HBB-SKAPA-HUVUD-ORDERRADER SECTION.                                      
221000                                                                          
221100     COMPUTE P-TO-P-LL =  LNG-P-TO-P-PREFIX +                             
221200                          LENGTH OF ORAD-MID-W4I25201                     
221300                                                                          
221400     MOVE LOW-VALUE       TO P-TO-P-Z1                                    
221500     MOVE LOW-VALUE       TO P-TO-P-Z2                                    
221600     MOVE 'W4T252X'       TO P-TO-P-TRANSKOD                              
221700     MOVE '4252'          TO P-TO-P-FROM-MID                              
221800     MOVE MFS-KDMFSFOR    TO P-TO-P-KDMFSFOR                              
221900                                                                          
222000     MOVE SPACE           TO KOM-AREA                                     
222100     MOVE 'W603'           TO ORAD-MID-IDSYSTEM                           
222200     MOVE W-IDDISTR-RETUR  TO ORAD-MID-IDDISTR                            
222300     MOVE W-IDKUNDNR-RETUR TO ORAD-MID-IDKUNDNR                           
222400     MOVE W-IDORDNR-X      TO ORAD-MID-IDORDNR                            
222500     MOVE 'DAMAGED'        TO ORAD-MID-BEVOLREF                           
222600     MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                        
222700     MOVE 'N'              TO ORAD-MID-FLSLUT                             
222800     .                                                                    
222900     EJECT                                                                
223000 HBC-EDIT-TRANS-ORDERRADER SECTION.                                       
223100                                                                          
223200     MOVE W-IDARTNR       TO ORAD-MID-IDARTNR   (ORAD-IX)                 
223300                             REK-IDARTNR                                  
223400     MOVE 9               TO REK-LNGD                                     
223500     MOVE 0               TO REK-REKSIFFR                                 
223600                                                                          
223700     CALL W009KSIF        USING REK-IDARTNR                               
223800                                REK-LNGD                                  
223900                                REK-REKSIFFR                              
224000                                                                          
224100     MOVE REK-REKSIFFR    TO ORAD-MID-REKSIFFR  (ORAD-IX)                 
224200     MOVE W-KVAVIS        TO W-KVAVIS-6                                   
224300     MOVE W-KVAVIS-6-X    TO ORAD-MID-KVBEART   (ORAD-IX)                 
224400     MOVE SPACE           TO ORAD-MID-PRARTNTO  (ORAD-IX)                 
224500                             ORAD-MID-TITPO     (ORAD-IX)                 
224600                             ORAD-MID-FLRESTN   (ORAD-IX)                 
224700                             ORAD-MID-KDKVBRYT  (ORAD-IX)                 
224800                             ORAD-MID-FLINVEST  (ORAD-IX)                 
224900     MOVE ZERO            TO ORAD-MID-KDVRINFO  (ORAD-IX)                 
225000     MOVE SPACE           TO ORAD-MID-IDKONTO   (ORAD-IX)                 
225100                             ORAD-MID-IDKST     (ORAD-IX)                 
225200                             ORAD-MID-BERADREF  (ORAD-IX)                 
225300                             ORAD-MID-KDDSP     (ORAD-IX)                 
225400                             ORAD-MID-IDBIL     (ORAD-IX)                 
225500     MOVE NEJ             TO ORAD-MID-FLSLATT   (ORAD-IX)                 
225600                             ORAD-MID-FLDIRLEV  (ORAD-IX)                 
225700     MOVE SPACE        TO ORAD-MID-PRARTNTO-LOC (ORAD-IX)                 
225800     MOVE SPACE        TO ORAD-MID-PRARTBTO-LOC (ORAD-IX)                 
225900     MOVE SPACE        TO     ORAD-MID-KDVALISO (ORAD-IX)                 
226000     MOVE SPACE        TO     ORAD-MID-KDVAT    (ORAD-IX)                 
226100     MOVE 0            TO     ORAD-MID-RERAB    (ORAD-IX)                 
226200     MOVE SPACE        TO     ORAD-MID-KDRAB    (ORAD-IX)                 
226300     MOVE SPACE        TO ORAD-MID-BEART-VIPS   (ORAD-IX)                 
226400     MOVE ZERO         TO ORAD-MID-ADLAGOMR-CD  (ORAD-IX)                 
226500                          ORAD-MID-ADGANG-CD    (ORAD-IX)                 
226600                          ORAD-MID-ADPLATS-CD   (ORAD-IX)                 
226700     MOVE SPACE        TO ORAD-MID-IDKUNDRF-WIP (ORAD-IX)                 
226800     .                                                                    
226900     EJECT                                                                
227000 HC-LOS-UPDATERING SECTION.                                               
227100                                                                          
227200     PERFORM S02-SKAPA-WDL6A1KY                                           
227300     PERFORM IMS-GU-WLINLD01                                              
227400     MOVE ZERO TO WS-RAKNARE                                              
227500                                                                          
227600     MOVE MID-IDDC-SPAR TO W-6301-IDDC                                    
227700     PERFORM IMS-GHU-WL630111                                             
227800     MOVE 6302-IDDC-SEND TO WS-IDDC-SEND                                  
227900                                                                          
228000     PERFORM UNTIL SEGMENT-SAKNAS                                         
228100         MOVE SEQA-IDARTNR   TO W-IDARTNR                                 
228201                                SPAR-6308-IDARTNR                         
228300         MOVE SEQA-DAINLEV   TO W-DAINLEV                                 
228400         PERFORM IMS-GHU-INLC1-WLINLC11                                   
228500         MOVE INL-KVAVIS     TO W-KVAVIS                                  
228600         MOVE INL-IDDC       TO W-IDDC                                    
228700         MOVE INL-IDDISTR    TO WS-SAP-IDDISTR                            
228800         MOVE INL-IDKUNDNR   TO WS-SAP-IDKUNDNR                           
228900                                                                          
229000         MOVE 'R32'          TO INL-IDPTYP                                
229100         IF MID-IDUSER-003 = ALL '+' OR SPACE                             
229200            CONTINUE                                                      
229300         ELSE                                                             
229400            MOVE WS-IDUSER-003 TO INL-IDUSER-003                          
229500         END-IF                                                           
229600         ADD +1              TO WS-RAKNARE                                
229700         PERFORM IMS-REPL-INLC1-WLINLC11                                  
229800                                                                          
229900         IF DCS-CDC                                                       
230000           PERFORM IMS-GHU-WDK611                                         
230100           SUBTRACT W-KVAVIS     FROM CLAG-KVAKS-PAV                      
230200           PERFORM IMS-REPL-WDK611                                        
230300         ELSE                                                             
230400           PERFORM IMS-GHU-ARTS11                                         
230500           SUBTRACT W-KVAVIS     FROM SLAG-KVAKS-PAV                      
230600           PERFORM IMS-REPL-ARTS11                                        
230700         END-IF                                                           
230800                                                                          
230900         MOVE ' '    TO LOGG-IDTECKEN-KVAKS                               
231000         MOVE '-'    TO LOGG-IDTECKEN-KVAKS-PAV                           
231100         MOVE ' '    TO LOGG-IDTECKEN-KVLS                                
231200         PERFORM S06-SKAPA-SALDOLOGG                                      
231300                                                                          
231400         PERFORM IMS-GET-WLARTC01                                         
231500         MOVE ART-KDPRODSL  TO EKH-KDPRODSL                               
231601                               R8-EKH-KDPRODSL                            
231701                               WS-KDPRODSL                                
231800         MOVE ART-KDSORT    TO WS-KDSORT                                  
231900         PERFORM IMS-GET-WLARTC11                                         
232000         MOVE CLAG-PRARTSTD TO EKH-PRARTSTD                               
232100         PERFORM S07-SKAPA-SAP-TRANS                                      
232200         MOVE WS-IDDC-SEND TO SEND-WS-IDDC                                
232300         IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                              
232400            MOVE SEND-WS-IDDC TO W-IDDC-B6                                
232500            PERFORM IMS-GU-WDB601-SEND                                    
232600         END-IF                                                           
232701         IF SEND-DCS-DDC                                                  
232800           PERFORM S08-SKAPA-AVVIK-TRANS                                  
232900         END-IF                                                           
233000                                                                          
233100         MOVE DCS-FLINLREP TO FILC2-FLINLREP                              
233200         PERFORM S09-SKAPA-LDC-TRANS                                      
233300                                                                          
233401         IF DCS-CDC                                                       
233501           IF SEND-DCS-NDC-CN OR SEND-DCS-USA                             
233601*-REFILL FRÅN EXPORT TILL CDC.UT PÅ RAPPORT W41841-001.                   
233701             MOVE 6302-IDFAKT      TO SPAR-6306-IDFAKT                    
233801                                      SPAR-6308-IDRAPPNR                  
233901             MOVE 'N'              TO SPAR-6306-FLDIRLEV                  
234001             MOVE ZERO             TO SPAR-6308-IDRADNR                   
234101                                      SPAR-6308-KDEMBLEV                  
234201             ACCEPT W-DATUM-X FROM DATE                                   
234301             MOVE W-DATUM           TO SPAR-6308-TILEVANM                 
234401             MOVE 6302-TIFAKT       TO SPAR-6306-TIFAKT                   
234501             MOVE MID-IDDC-SPAR     TO SPAR-6306-IDDC-REC                 
234601             MOVE 6302-IDDC-SEND    TO SPAR-6306-IDDC-SEND                
234701             MOVE 6302-IDDC-LEV     TO SPAR-6306-IDDC-LEV                 
234801             MOVE INL-IDDISTR       TO SPAR-6308-IDDISTR                  
234901             MOVE INL-IDKUNDNR      TO SPAR-6308-IDKUNDNR                 
235001             MOVE INL-KDFRAKT       TO SPAR-6308-KDFRAKT                  
235101             MOVE INL-IDKUNDRF      TO SPAR-6308-IDKUNDRF                 
235201                                                                          
235301             IF DIST79-DEALER-PRICE                                       
235401               MOVE INL-PRARTNTO    TO SPAR-6308-PRARTBTO-LOC             
235501               MOVE ZERO            TO SPAR-6308-PRARTBTO                 
235601               MOVE INL-KDVALISO    TO SPAR-6308-KDVALISO                 
235701             ELSE                                                         
235801               MOVE INL-PRARTNTO    TO SPAR-6308-PRARTBTO                 
235901               MOVE ZERO            TO SPAR-6308-PRARTBTO-LOC             
236001               MOVE 6302-IDDISTR    TO DIST35-IDDISTR                     
236101               IF DIST35-NDCCN-CDC-REFILL                                 
236201                  MOVE 'CNY'        TO SPAR-6308-KDVALISO                 
236301               ELSE                                                       
236401                 IF DIST35-NDCUS-CDC-REFILL                               
236501                   MOVE 'USD'       TO SPAR-6308-KDVALISO                 
236601                 ELSE                                                     
236701                   MOVE 'SEK'       TO SPAR-6308-KDVALISO                 
236801                 END-IF                                                   
236901               END-IF                                                     
237001             END-IF                                                       
237101                                                                          
237201             MOVE INL-IDKOLLI       TO SPAR-6308-IDKOLLI                  
237301             MOVE INL-KVAVIS        TO SPAR-6308-KVLEVANM                 
237401             MOVE '60'              TO SPAR-6308-KDANMORS                 
237501                                                                          
237601             PERFORM S98-SKAPA-LEVANM-TRANS                               
237701           END-IF                                                         
237801         END-IF                                                           
237900                                                                          
238000         PERFORM IMS-GN-WLINLD01                                          
238100     END-PERFORM                                                          
238200                                                                          
238300     PERFORM X020-UPPDATERA-WL630111                                      
238401     IF WS-FAKT-INFO-DLET = JA                                            
238501        PERFORM HDD-FAKTURA-SLUTBEHANDLAD                                 
238601     END-IF                                                               
238700     .                                                                    
238800     EJECT                                                                
238900 HD-LOS-UPDATERING SECTION.                                               
239000                                                                          
239100     MOVE NEJ TO WS-A03-SKAPAD                                            
239200     MOVE ZERO TO WS-RAKNARE                                              
239300                                                                          
239400     MOVE MID-IDDC-SPAR TO W-6301-IDDC                                    
239500     PERFORM IMS-GHU-WL630111                                             
239600     MOVE 6302-IDDC-SEND TO WS-IDDC-SEND                                  
239700                            SEND-WS-IDDC                                  
239801     MOVE 6302-IDDISTR   TO DIST35-IDDISTR                                
239900                                                                          
240000     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
240100        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
240200        PERFORM IMS-GU-WDB601-SEND                                        
240300     END-IF                                                               
240400                                                                          
240500     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
240600        PERFORM HDA-LOS-CDC-NDC                                           
240700     ELSE                                                                 
240800        MOVE MID-IDDC-SPAR TO SPAR-WS-IDDC                                
240900        IF SPAR-WS-IDDC NOT = SPAR-DCS-IDDC                               
241000           MOVE SPAR-WS-IDDC TO W-IDDC-B6                                 
241100           PERFORM IMS-GU-WDB601-SPAR                                     
241200        END-IF                                                            
241300        IF SEND-DCS-NDC-NA AND SEND-DCS-USA AND                           
241400           SPAR-DCS-NDC-NA AND SPAR-DCS-USA                               
241500           PERFORM HDB-LOS-USA-USA                                        
241600        ELSE                                                              
241700           IF (SEND-DCS-NDC-NA AND SEND-DCS-CANADA AND                    
241800               SPAR-DCS-NDC-NA AND SPAR-DCS-USA)                          
241900           OR (SEND-DCS-NDC-NA AND SEND-DCS-USA    AND                    
242000               SPAR-DCS-NDC-NA AND SPAR-DCS-CANADA)                       
242100              PERFORM HDC-LOS-USA-CANADA                                  
242200           END-IF                                                         
242300        END-IF                                                            
242400     END-IF                                                               
242500                                                                          
242600     PERFORM X020-UPPDATERA-WL630111                                      
242700     IF WS-FAKT-INFO-DLET = JA                                            
242800        PERFORM HDD-FAKTURA-SLUTBEHANDLAD                                 
242900     END-IF                                                               
243000     IF WS-A03-SKAPAD = JA                                                
243100        IF WS-FAKT-INFO-DLET = 'J'                                        
243200           MOVE 'Y' TO EKOTRA03-FLSLUT                                    
243300        END-IF                                                            
243400        PERFORM S041-SKRIV-EKOTRANS-A03                                   
243500     END-IF                                                               
243600     .                                                                    
243700     EJECT                                                                
243800 HDA-LOS-CDC-NDC SECTION.                                                 
243900                                                                          
244000     PERFORM S02-SKAPA-WDL6A1KY                                           
244100     PERFORM IMS-GU-WLINLD01                                              
244200                                                                          
244300     PERFORM UNTIL SEGMENT-SAKNAS                                         
244400        IF WS-A03-SKAPAD = JA                                             
244500           PERFORM S041-SKRIV-EKOTRANS-A03                                
244600        END-IF                                                            
244700                                                                          
244800        MOVE SEQA-IDARTNR      TO W-IDARTNR                               
244900                                  SPAR-6308-IDARTNR                       
245000                                  EKOTRA03-IDARTNR                        
245100        MOVE SEQA-DAINLEV      TO W-DAINLEV                               
245200        PERFORM IMS-GHU-INLC1-WLINLC11                                    
245300                                                                          
245400        MOVE INL-KVAVIS        TO W-KVAVIS                                
245500        MOVE INL-IDDC          TO W-IDDC                                  
245600        MOVE INL-KDFRAKT       TO W-KDFRAKT                               
245700                                                                          
245800        MOVE 6302-IDFAKT       TO SPAR-6306-IDFAKT                        
245900                                  EKOTRA03-IDFAKT                         
246000        MOVE 'N'               TO SPAR-6306-FLDIRLEV                      
246100        MOVE ZERO              TO SPAR-6308-IDRAPPNR                      
246200                                  SPAR-6308-IDRADNR                       
246300                                  SPAR-6308-KDEMBLEV                      
246400        ACCEPT W-DATUM-X FROM DATE                                        
246500        MOVE W-DATUM           TO SPAR-6308-TILEVANM                      
246600        MOVE 6302-TIFAKT       TO SPAR-6306-TIFAKT                        
246700        MOVE 6302-TIFAKT       TO WS-SEKEL-KOLL                           
246800                                  WS-EKOA03-AAMMDD                        
246900        IF WS-SEKEL = 9                                                   
247000           MOVE 19             TO WS-EKOA03-SS                            
247100        ELSE                                                              
247200           MOVE 20             TO WS-EKOA03-SS                            
247300        END-IF                                                            
247400        MOVE WS-AAAAMMDD       TO EKOTRA03-DAFAKT                         
247500        MOVE MID-IDDC-SPAR     TO SPAR-6306-IDDC-REC                      
247600                                  EKOTRA03-IDDC-REC                       
247700        MOVE WC-CDC-SE         TO EKOTRA03-IDDC-SEND                      
247800        MOVE 6302-IDDC-SEND    TO SPAR-6306-IDDC-SEND                     
247901        MOVE 6302-IDDC-LEV     TO SPAR-6306-IDDC-LEV                      
248000        MOVE INL-IDDISTR       TO SPAR-6308-IDDISTR                       
248100                                  EKOTRA03-IDDISTR                        
248200                                  WS-SAP-IDDISTR                          
248300                                  W-IDDISTR-WDB3                          
248400                                  W-IDDISTR-WDB3-DEF                      
248500        MOVE INL-IDKUNDNR      TO SPAR-6308-IDKUNDNR                      
248600                                  EKOTRA03-IDKUNDNR                       
248700                                  WS-SAP-IDKUNDNR                         
248800        MOVE INL-KDFRAKT       TO SPAR-6308-KDFRAKT                       
248900        MOVE INL-IDKUNDRF      TO SPAR-6308-IDKUNDRF                      
249000                                                                          
249100**- ÄNDRING 2004-10 FÖR ATT KLARA LOCAL-CURRENCY NA.INL-PRARTNTO          
249200**- KOMMER FRÅN BILL-IT OCH ÄR I LOCAL VALUTA.                            
249300        IF DIST79-DEALER-PRICE                                            
249400          MOVE INL-PRARTNTO    TO SPAR-6308-PRARTBTO-LOC                  
249500                                  EKOTRA03-PRARTNTO                       
249600          MOVE ZERO            TO SPAR-6308-PRARTBTO                      
249700          MOVE INL-KDVALISO    TO SPAR-6308-KDVALISO                      
249800                                  EKOTRA03-KDVALISO                       
249900        ELSE                                                              
250000          MOVE INL-PRARTNTO    TO SPAR-6308-PRARTBTO                      
250100                                  EKOTRA03-PRARTNTO                       
250200          MOVE ZERO            TO SPAR-6308-PRARTBTO-LOC                  
250300          MOVE 'SEK'           TO SPAR-6308-KDVALISO                      
250400                                  EKOTRA03-KDVALISO                       
250500        END-IF                                                            
250600                                                                          
250700        MOVE INL-IDKOLLI       TO SPAR-6308-IDKOLLI                       
250800                                  EKOTRA03-IDKOLLI                        
250900        MOVE ZERO              TO EKOTRA03-IDORDNR7                       
251000        MOVE INL-IDORDNR5      TO EKOTRA03-IDORDNR7                       
251100        MOVE ZERO              TO EKOTRA03-DAINLINL                       
251200        MOVE INL-KVAVIS        TO EKOTRA03-KVLEVART                       
251300                                  SPAR-6308-KVLEVANM                      
251400        MOVE 'SEK'             TO EKOTRA03-KDVALISO                       
251500        MOVE INL-PRKURS        TO EKOTRA03-PRKURS                         
251600***** SAP                         EKH-PRKURS                              
251700        MOVE '60'              TO SPAR-6308-KDANMORS                      
251800                                  EKOTRA03-KDANMORS                       
251900                                                                          
252000        PERFORM IMS-GET-WLARTC01                                          
252100        MOVE ART-KDPRODSL      TO EKOTRA03-KDPRODSL                       
252200                                  EKH-KDPRODSL                            
252301                                  R8-EKH-KDPRODSL                         
252401                                  WS-KDPRODSL                             
252500        MOVE ART-KDSORT        TO WS-KDSORT                               
252600        PERFORM IMS-GET-WLARTC11                                          
252700        MOVE CLAG-KDPSLLOC     TO EKOTRA03-KDPSLLOC                       
252800        MOVE CLAG-PRARTSTD     TO EKH-PRARTSTD                            
252900        PERFORM S05-SOEK-REMARKUP                                         
253000        MOVE WS-MARKUP         TO EKOTRA03-REMARKUP                       
253100        MOVE SPACE             TO EKOTRA03-FLSLUT                         
253200        MOVE ZERO              TO EKOTRA03-KVANTMOT                       
253300                                  EKOTRA03-KVSKROT                        
253400                                                                          
253500        MOVE 'R32'         TO INL-IDPTYP                                  
253600        IF MID-IDUSER-003 = ALL '+' OR SPACE                              
253700           CONTINUE                                                       
253800        ELSE                                                              
253900           MOVE WS-IDUSER-003 TO INL-IDUSER-003                           
254000        END-IF                                                            
254100        MOVE ZERO          TO INL-TIINLINL                                
254200                              INL-KVANTMOT                                
254300        ADD +1 TO WS-RAKNARE                                              
254400        PERFORM IMS-REPL-INLC1-WLINLC11                                   
254500                                                                          
254600        PERFORM HDAB-KOLLA-FRAKTKOD                                       
254700*       IF WS-FLYGORDER = NEJ ÄNDRING 970512                              
254800           PERFORM S98-SKAPA-LEVANM-TRANS                                 
254900*       END-IF                                                            
255000                                                                          
255100                                                                          
255200        PERFORM IMS-GHU-ARTS11                                            
255300        SUBTRACT W-KVAVIS      FROM SLAG-KVAKS-PAV                        
255400        MOVE SLAG-KVLS           TO EKOTRA03-KVLS-OLD                     
255500        MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST-OLD                 
255600        MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST                     
255701                                    WS-PRAVCOST                           
255800                                                                          
255900        PERFORM IMS-REPL-ARTS11                                           
256000                                                                          
256100        MOVE ' '    TO LOGG-IDTECKEN-KVAKS                                
256200        MOVE '-'    TO LOGG-IDTECKEN-KVAKS-PAV                            
256300        MOVE ' '    TO LOGG-IDTECKEN-KVLS                                 
256400        PERFORM S06-SKAPA-SALDOLOGG                                       
256501                                                                          
256601*** FOR US BOUNCE FLOW                                                    
256701        IF AKTUELLT-LAND-USA                                              
256801        AND DIST35-NDCCN-NDCUS-REFILL                                     
256901          PERFORM S07A-SKAPA-SAP-TRANS                                    
257001        ELSE                                                              
257101          PERFORM S04-SKAPA-EKOTRANS-A03                                  
257201        END-IF                                                            
257300                                                                          
257400        MOVE SPAR-FLINLREP TO FILC2-FLINLREP                              
257500        PERFORM S09-SKAPA-LDC-TRANS                                       
257600                                                                          
257700        PERFORM IMS-GN-WLINLD01                                           
257800     END-PERFORM                                                          
257900     .                                                                    
258000     EJECT                                                                
258100 HDAB-KOLLA-FRAKTKOD SECTION.                                             
258200                                                                          
258300     MOVE NEJ TO WS-FLYGORDER                                             
258400                 WS-BAATORDER                                             
258500                                                                          
258600     MOVE 6302-IDDC-SEND      TO W-IDDC-WDB3                              
258700                                 W-IDDC-WDB3-DEF                          
258800     MOVE 6302-IDKUNDNR       TO W-IDKUNDNR-WDB3                          
258900                                                                          
259000     PERFORM IMS-GU-WDB301                                                
259100     IF SEGMENT-FINNS                                                     
259200        IF W-KDFRAKT = DC-KDGENFRA-VOR                                    
259300           MOVE JA TO WS-FLYGORDER                                        
259400        ELSE                                                              
259500           IF W-KDFRAKT = DC-KDGENFRA-MO                                  
259600              MOVE JA TO WS-BAATORDER                                     
259700           END-IF                                                         
259800        END-IF                                                            
259900     END-IF                                                               
260000     .                                                                    
260100     EJECT                                                                
260200 HDB-LOS-USA-USA SECTION.                                                 
260300                                                                          
260400     PERFORM S02-SKAPA-WDL6A1KY                                           
260500     PERFORM IMS-GU-WLINLD01                                              
260600                                                                          
260700     PERFORM UNTIL SEGMENT-SAKNAS                                         
260800        IF WS-A03-SKAPAD = JA                                             
260900           PERFORM S041-SKRIV-EKOTRANS-A03                                
261000        END-IF                                                            
261100        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
261200                             EKOTRA03-IDARTNR                             
261300                                                                          
261400        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
261500        PERFORM IMS-GHU-INLC1-WLINLC11                                    
261600                                                                          
261700        MOVE INL-KVAVIS        TO W-KVAVIS                                
261800        MOVE INL-IDDC          TO W-IDDC                                  
261900        MOVE ZERO              TO EKOTRA03-IDORDNR7                       
262000        MOVE INL-IDORDNR5      TO EKOTRA03-IDORDNR7                       
262100        MOVE INL-PRARTNTO      TO EKOTRA03-PRARTNTO                       
262200        MOVE INL-IDKOLLI       TO EKOTRA03-IDKOLLI                        
262300        MOVE INL-KVAVIS        TO EKOTRA03-KVLEVART                       
262400        MOVE 'USD'             TO EKOTRA03-KDVALISO                       
262500        MOVE INL-PRKURS        TO EKOTRA03-PRKURS                         
262600        MOVE 6302-IDDC-SEND    TO EKOTRA03-IDDC-SEND                      
262700        MOVE INL-IDDISTR       TO EKOTRA03-IDDISTR                        
262800        MOVE INL-IDKUNDNR      TO EKOTRA03-IDKUNDNR                       
262900        MOVE 6302-TIFAKT       TO WS-SEKEL-KOLL                           
263000                                  WS-EKOA03-AAMMDD                        
263100        IF WS-SEKEL = 9                                                   
263200           MOVE 19             TO WS-EKOA03-SS                            
263300        ELSE                                                              
263400           MOVE 20             TO WS-EKOA03-SS                            
263500        END-IF                                                            
263600        MOVE WS-AAAAMMDD       TO EKOTRA03-DAFAKT                         
263700        MOVE 6302-IDFAKT       TO EKOTRA03-IDFAKT                         
263800        MOVE MID-IDDC-SPAR     TO EKOTRA03-IDDC-REC                       
263900        MOVE '60'              TO EKOTRA03-KDANMORS                       
264000        MOVE ZERO              TO EKOTRA03-DAINLINL                       
264100                                                                          
264200        MOVE 'R32'             TO INL-IDPTYP                              
264300        IF MID-IDUSER-003 = ALL '+' OR SPACE                              
264400           CONTINUE                                                       
264500        ELSE                                                              
264600           MOVE WS-IDUSER-003  TO INL-IDUSER-003                          
264700        END-IF                                                            
264800        MOVE ZERO          TO INL-TIINLINL                                
264900                              INL-KVANTMOT                                
265000        ADD +1     TO WS-RAKNARE                                          
265100                                                                          
265200        PERFORM IMS-REPL-INLC1-WLINLC11                                   
265300                                                                          
265400        PERFORM IMS-GHU-ARTS11                                            
265500        SUBTRACT W-KVAVIS      FROM SLAG-KVAKS-PAV                        
265600        MOVE SLAG-KVLS           TO EKOTRA03-KVLS-OLD                     
265700        MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST-OLD                 
265800        MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST                     
265900                                                                          
266000        PERFORM IMS-REPL-ARTS11                                           
266100                                                                          
266200        MOVE ' '    TO LOGG-IDTECKEN-KVAKS                                
266300        MOVE '-'    TO LOGG-IDTECKEN-KVAKS-PAV                            
266400        MOVE ' '    TO LOGG-IDTECKEN-KVLS                                 
266500        PERFORM S06-SKAPA-SALDOLOGG                                       
266600                                                                          
266700        PERFORM IMS-GET-WLARTC01                                          
266800        MOVE ART-KDPRODSL      TO EKOTRA03-KDPRODSL                       
266900        PERFORM IMS-GET-WLARTC11                                          
267000        MOVE CLAG-KDPSLLOC     TO EKOTRA03-KDPSLLOC                       
267100        MOVE 1                 TO EKOTRA03-REMARKUP                       
267200                                                                          
267300        MOVE SPACE             TO EKOTRA03-FLSLUT                         
267400        MOVE ZERO              TO EKOTRA03-KVANTMOT                       
267500                                  EKOTRA03-KVSKROT                        
267600        PERFORM S04-SKAPA-EKOTRANS-A03                                    
267700        MOVE SPAR-FLINLREP TO FILC2-FLINLREP                              
267800        PERFORM S09-SKAPA-LDC-TRANS                                       
267900                                                                          
268000        PERFORM IMS-GN-WLINLD01                                           
268100     END-PERFORM                                                          
268200     .                                                                    
268300     EJECT                                                                
268400 HDC-LOS-USA-CANADA SECTION.                                              
268500                                                                          
268600     PERFORM S02-SKAPA-WDL6A1KY                                           
268700     PERFORM IMS-GU-WLINLD01                                              
268800                                                                          
268900     PERFORM UNTIL SEGMENT-SAKNAS                                         
269000        IF WS-A03-SKAPAD = JA                                             
269100           PERFORM S041-SKRIV-EKOTRANS-A03                                
269200        END-IF                                                            
269300                                                                          
269400        MOVE SEQA-IDARTNR   TO W-IDARTNR                                  
269500        MOVE SEQA-IDARTNR   TO SPAR-6308-IDARTNR                          
269600                               EKOTRA03-IDARTNR                           
269700                                                                          
269800        MOVE SEQA-DAINLEV   TO W-DAINLEV                                  
269900        PERFORM IMS-GHU-INLC1-WLINLC11                                    
270000        MOVE INL-KVAVIS     TO W-KVAVIS                                   
270100        MOVE INL-IDDC       TO W-IDDC                                     
270200                                                                          
270300        MOVE 'R32'          TO INL-IDPTYP                                 
270400        IF MID-IDUSER-003 = ALL '+' OR SPACE                              
270500           CONTINUE                                                       
270600        ELSE                                                              
270700           MOVE WS-IDUSER-003 TO INL-IDUSER-003                           
270800        END-IF                                                            
270900        MOVE ZERO           TO INL-TIINLINL                               
271000                               INL-KVANTMOT                               
271100        ADD +1              TO WS-RAKNARE                                 
271200                                                                          
271300        MOVE 6302-TIFAKT    TO SPAR-6306-TIFAKT                           
271400        MOVE 6302-TIFAKT    TO WS-SEKEL-KOLL                              
271500                               WS-EKOA03-AAMMDD                           
271600        IF WS-SEKEL = 9                                                   
271700           MOVE 19          TO WS-EKOA03-SS                               
271800        ELSE                                                              
271900           MOVE 20          TO WS-EKOA03-SS                               
272000        END-IF                                                            
272100                                                                          
272200        MOVE WS-AAAAMMDD    TO EKOTRA03-DAFAKT                            
272300        ACCEPT W-DATUM-X FROM DATE                                        
272400        MOVE W-DATUM        TO SPAR-6308-TILEVANM                         
272500        MOVE INL-IDKUNDNR   TO EKOTRA03-IDKUNDNR                          
272600                               SPAR-6308-IDKUNDNR                         
272700        MOVE 6302-IDFAKT    TO EKOTRA03-IDFAKT                            
272800                               SPAR-6306-IDFAKT                           
272900        MOVE MID-IDDC-SPAR  TO SPAR-6306-IDDC-REC                         
273000                               EKOTRA03-IDDC-REC                          
273100        MOVE 'N'            TO SPAR-6306-FLDIRLEV                         
273200        MOVE ZERO           TO SPAR-6308-IDRAPPNR                         
273300                               SPAR-6308-IDRADNR                          
273400                               SPAR-6308-KDEMBLEV                         
273500        MOVE INL-IDDISTR    TO SPAR-6308-IDDISTR                          
273600                               EKOTRA03-IDDISTR                           
273700        MOVE INL-IDKUNDRF   TO SPAR-6308-IDKUNDRF                         
273800        MOVE INL-KDFRAKT    TO SPAR-6308-KDFRAKT                          
273900        MOVE ZERO           TO EKOTRA03-IDORDNR7                          
274000        MOVE INL-IDORDNR5   TO EKOTRA03-IDORDNR7                          
274100        MOVE INL-KVAVIS     TO EKOTRA03-KVLEVART                          
274200                               SPAR-6308-KVLEVANM                         
274300        MOVE INL-IDKOLLI    TO SPAR-6308-IDKOLLI                          
274400                               EKOTRA03-IDKOLLI                           
274500                                                                          
274600**- ÄNDRING 2004-10 FÖR ATT KLARA LOCAL-CURRENCY NA RÖR EJ                
274700**- NA-TRANSFER-DISTRIKTEN EFTERSOM DESSA HAR INL-PRARTNTO=               
274800**- PRAVCOST I USD/CAD. SE PGM W4752A00.                                  
274900        MOVE INL-PRARTNTO   TO EKOTRA03-PRARTNTO                          
275000                               SPAR-6308-PRARTBTO                         
275100        MOVE ZERO           TO SPAR-6308-PRARTBTO-LOC                     
275200        MOVE ZERO           TO EKOTRA03-DAINLINL                          
275300        MOVE 6302-IDDC-SEND TO SEND-WS-IDDC                               
275400        IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                               
275500           MOVE SEND-WS-IDDC TO W-IDDC-B6                                 
275600           PERFORM IMS-GU-WDB601-SEND                                     
275700        END-IF                                                            
275800        IF SEND-DCS-NDC-NA AND SEND-DCS-CANADA                            
275900           MOVE 'CAD'       TO EKOTRA03-KDVALISO                          
276000                               SPAR-6308-KDVALISO                         
276100        ELSE                                                              
276200           MOVE 'USD'       TO EKOTRA03-KDVALISO                          
276300                               SPAR-6308-KDVALISO                         
276400        END-IF                                                            
276500        MOVE 6302-IDDC-SEND TO EKOTRA03-IDDC-SEND                         
276600        MOVE 6302-IDDC-SEND TO SPAR-6306-IDDC-SEND                        
276701        MOVE 6302-IDDC-LEV  TO SPAR-6306-IDDC-LEV                         
276800        MOVE INL-PRKURS     TO EKOTRA03-PRKURS                            
276900        MOVE '60'           TO EKOTRA03-KDANMORS                          
277000        MOVE '00'           TO SPAR-6308-KDANMORS                         
277100                                                                          
277200        PERFORM IMS-REPL-INLC1-WLINLC11                                   
277300                                                                          
277400        PERFORM S98-SKAPA-LEVANM-TRANS                                    
277500                                                                          
277600        PERFORM IMS-GHU-ARTS11                                            
277700        SUBTRACT W-KVAVIS      FROM SLAG-KVAKS-PAV                        
277800        MOVE SLAG-KVLS           TO EKOTRA03-KVLS-OLD                     
277900        MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST-OLD                 
278000        MOVE SLAG-PRAVCOST       TO EKOTRA03-PRAVCOST                     
278100                                                                          
278200        PERFORM IMS-REPL-ARTS11                                           
278300                                                                          
278400        MOVE ' '    TO LOGG-IDTECKEN-KVAKS                                
278500        MOVE '-'    TO LOGG-IDTECKEN-KVAKS-PAV                            
278600        MOVE ' '    TO LOGG-IDTECKEN-KVLS                                 
278700        PERFORM S06-SKAPA-SALDOLOGG                                       
278800                                                                          
278900        PERFORM IMS-GET-WLARTC01                                          
279000        MOVE ART-KDPRODSL      TO EKOTRA03-KDPRODSL                       
279100        PERFORM IMS-GET-WLARTC11                                          
279200        MOVE CLAG-KDPSLLOC     TO EKOTRA03-KDPSLLOC                       
279300        PERFORM S05-SOEK-REMARKUP                                         
279400        MOVE WS-MARKUP         TO EKOTRA03-REMARKUP                       
279500        MOVE SPACE             TO EKOTRA03-FLSLUT                         
279600        MOVE ZERO              TO EKOTRA03-KVANTMOT                       
279700                                  EKOTRA03-KVSKROT                        
279800                                                                          
279900        PERFORM S04-SKAPA-EKOTRANS-A03                                    
280000        MOVE SPAR-FLINLREP TO FILC2-FLINLREP                              
280100        PERFORM S09-SKAPA-LDC-TRANS                                       
280200                                                                          
280300        PERFORM IMS-GN-WLINLD01                                           
280400     END-PERFORM                                                          
280500     .                                                                    
280600     EJECT                                                                
280700 HDD-FAKTURA-SLUTBEHANDLAD SECTION.                                       
280800                                                                          
280900     PERFORM IMS-GHU-WL630511                                             
281000     IF SEGMENT-FINNS                                                     
281100        MOVE JA TO 6306-FLKLAR                                            
281200        PERFORM IMS-REPL-WL630511                                         
281300     END-IF                                                               
281400     .                                                                    
281500     EJECT                                                                
281600 HE-DCR-UPDATERING SECTION.                                               
281700                                                                          
281800     PERFORM S02-SKAPA-WDL6A1KY                                           
281900     PERFORM IMS-GU-WLINLD01                                              
282000                                                                          
282100     PERFORM UNTIL SEGMENT-SAKNAS                                         
282200                                                                          
282300        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
282400        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
282500                                                                          
282600        PERFORM IMS-GHU-INLC1-WLINLC11                                    
282700        IF SEGMENT-FINNS                                                  
282800           IF MID-IDUSER-003 = ALL '+' OR SPACE                           
282900              CONTINUE                                                    
283000           ELSE                                                           
283100              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
283200           END-IF                                                         
283300           MOVE 'J' TO INL-FLSKAKOL                                       
283400           PERFORM IMS-REPL-INLC1-WLINLC11                                
283500        END-IF                                                            
283600        PERFORM IMS-GN-WLINLD01                                           
283700     END-PERFORM                                                          
283800     .                                                                    
283900     EJECT                                                                
284000 HF-CAN-UPDATERING SECTION.                                               
284100                                                                          
284200     PERFORM S02-SKAPA-WDL6A1KY                                           
284300     PERFORM IMS-GU-WLINLD01                                              
284400                                                                          
284500     PERFORM UNTIL SEGMENT-SAKNAS                                         
284600                                                                          
284700        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
284800        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
284900                                                                          
285000        PERFORM IMS-GHU-INLC1-WLINLC11                                    
285100        IF SEGMENT-FINNS                                                  
285200           IF MID-IDUSER-003 = ALL '+' OR SPACE                           
285300              CONTINUE                                                    
285400           ELSE                                                           
285500              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
285600           END-IF                                                         
285700           MOVE 'N' TO INL-FLSKAKOL                                       
285800           PERFORM IMS-REPL-INLC1-WLINLC11                                
285900        END-IF                                                            
286000        PERFORM IMS-GN-WLINLD01                                           
286100     END-PERFORM                                                          
286200     .                                                                    
286300     EJECT                                                                
286400 HG-LOC-UPDATERING SECTION.                                               
286500                                                                          
286600     PERFORM S02-SKAPA-WDL6A1KY                                           
286700     PERFORM IMS-GU-WLINLD01                                              
286800                                                                          
286900     PERFORM UNTIL SEGMENT-SAKNAS                                         
287000                                                                          
287100        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
287200        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
287300                                                                          
287400        PERFORM IMS-GHU-INLC1-WLINLC11                                    
287500        IF SEGMENT-FINNS                                                  
287600           IF MID-IDUSER-003 = ALL '+' OR SPACE                           
287700              CONTINUE                                                    
287800           ELSE                                                           
287900              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
288000           END-IF                                                         
288100           MOVE MID-ADINLOMR(INDX) TO INL-ADINLOMR                        
288200           PERFORM IMS-REPL-INLC1-WLINLC11                                
288300        END-IF                                                            
288400        PERFORM IMS-GN-WLINLD01                                           
288500     END-PERFORM                                                          
288600     .                                                                    
288700     EJECT                                                                
288800 HH-BIN-UPDATERING SECTION.                                               
288900******************************************************************        
289000* CMD = BIN STARTAR TRANS W6T303X                                         
289100******************************************************************        
289200                                                                          
289300     MOVE JA              TO HOPP-UPDATE-SW                               
289400                                                                          
289500     IF MID-CMD-IN (INDX) = 'BIN'                                         
289600        MOVE W-IDFAKT            TO PROGSW-MID-IDFAKT-IN                  
289700        MOVE MID-IDDC-SPAR       TO PROGSW-MID-IDDC-SPAR                  
289800        IF MID-IDUSER-003 = ALL '+' OR SPACE                              
289900           MOVE SPACE            TO PROGSW-MID-IDUSER-003                 
290000        ELSE                                                              
290100           MOVE MID-IDUSER-003   TO PROGSW-MID-IDUSER-003                 
290200        END-IF                                                            
290300                                                                          
290400        INSPECT PROGSW-MID-IDFAKT-IN                                      
290500             REPLACING LEADING SPACE BY ZERO                              
290600        MOVE MID-IDKUNDRF (INDX) TO PROGSW-MID-IDKUNDRF(BIN-IX)           
290700        INSPECT PROGSW-MID-IDKUNDRF(BIN-IX)                               
290800             REPLACING LEADING SPACE BY ZERO                              
290900        MOVE MID-IDKUNDNR (INDX) TO PROGSW-MID-IDKUNDNR(BIN-IX)           
291000        INSPECT PROGSW-MID-IDKUNDNR(BIN-IX)                               
291100             REPLACING LEADING SPACE BY ZERO                              
291200        MOVE MID-IDKOLLI(INDX)   TO PROGSW-MID-IDKOLLI(BIN-IX)            
291300        INSPECT PROGSW-MID-IDKOLLI(BIN-IX)                                
291400             REPLACING LEADING SPACE BY ZERO                              
291500                                                                          
291600        ADD +1 TO BIN-IX                                                  
291700     END-IF                                                               
291800     .                                                                    
291900     EJECT                                                                
292000 I-EDIT-MID-W6I30301      SECTION.                                        
292100                                                                          
292200     MOVE LOW-VALUE       TO 6303-MID-W6I30301                            
292300                                                                          
292400     MOVE ZERO            TO 6303-MID-IDARTNR-NEXT                        
292500                             6303-MID-IDARTNR-ENTER                       
292600                                                                          
292700     MOVE +1              TO INDX                                         
292800                                                                          
292900     PERFORM UNTIL INDX > MAX-INDX                                        
293000         IF  MID-CMD-IN (INDX) = 'S  '                                    
293100         OR  MID-CMD-IN (INDX) = 'X  '                                    
293200             MOVE W-IDFAKT            TO 6303-MID-IDFAKT-IN               
293300             INSPECT 6303-MID-IDFAKT-IN                                   
293400             REPLACING LEADING SPACE BY ZERO                              
293500             MOVE MID-IDKUNDRF (INDX) TO 6303-MID-IDKUNDRF-IN             
293600             INSPECT 6303-MID-IDKUNDRF-IN                                 
293700             REPLACING LEADING SPACE BY ZERO                              
293800             MOVE MID-IDKUNDNR (INDX) TO 6303-MID-IDKUNDNR-IN             
293900             INSPECT 6303-MID-IDKUNDNR-IN                                 
294000             REPLACING LEADING SPACE BY ZERO                              
294100             MOVE MID-IDKOLLI  (INDX) TO 6303-MID-IDKOLLI-IN              
294200             INSPECT 6303-MID-IDKOLLI-IN                                  
294300             REPLACING LEADING SPACE BY ZERO                              
294400             MOVE 'GB '               TO 6303-MID-IDSPRAK-IN              
294500             MOVE MID-IDDC-SPAR       TO 6303-MID-IDDC-IN                 
294600         END-IF                                                           
294700                                                                          
294800         ADD 1            TO INDX                                         
294900     END-PERFORM                                                          
295000                                                                          
295100     .                                                                    
295200     EJECT                                                                
295300 S01-LAS-FRAM-ARTIKEL    SECTION.                                         
295400                                                                          
295500      MOVE LOW-VALUE                  TO W-WDL6A1KY-MIN                   
295600      MOVE HIGH-VALUE                 TO W-WDL6A1KY-MAX                   
295700      MOVE INL-IDFAKT                 TO W-IDFAKT-MIN                     
295800                                         W-IDFAKT-MAX                     
295900      MOVE INL-IDKUNDRF               TO W-IDKUNDRF-MIN                   
296000                                         W-IDKUNDRF-MAX                   
296100      MOVE INL-IDKUNDNR               TO W-IDKUNDNR-MIN                   
296200                                         W-IDKUNDNR-MAX                   
296300      MOVE INL-IDKOLLI                TO W-IDKOLLI-MIN                    
296400                                         W-IDKOLLI-MAX                    
296500      IF W-ANTAL-LASN = ZERO                                              
296600         PERFORM IMS-GU-WLINLD01                                          
296700      ELSE                                                                
296800         PERFORM IMS-GN-WLINLD01                                          
296900      END-IF                                                              
297000                                                                          
297100      MOVE SEQA-IDARTNR           TO W-IDARTNR                            
297200      MOVE INL-IDDC               TO W-IDDC                               
297300     .                                                                    
297400     EJECT                                                                
297500 S02-SKAPA-WDL6A1KY SECTION.                                              
297600                                                                          
297700     MOVE LOW-VALUE                  TO W-WDL6A1KY-MIN                    
297800     MOVE HIGH-VALUE                 TO W-WDL6A1KY-MAX                    
297900     MOVE W-IDFAKT                   TO W-IDFAKT-MIN                      
298000                                        W-IDFAKT-MAX                      
298100     INSPECT MID-IDKUNDRF(INDX) REPLACING LEADING SPACE BY ZERO           
298200     MOVE MID-IDKUNDRF(INDX)         TO W-IDKUNDRF-MIN                    
298300                                        W-IDKUNDRF-MAX                    
298400     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
298500     MOVE MID-IDKUNDNR(INDX)          TO W-IDKUNDNR-MIN                   
298600                                         W-IDKUNDNR-MAX                   
298700     INSPECT MID-IDKOLLI(INDX) REPLACING LEADING SPACE BY ZERO            
298800     MOVE MID-IDKOLLI(INDX)          TO W-IDKOLLI-MIN                     
298900                                        W-IDKOLLI-MAX                     
299000     .                                                                    
299100     EJECT                                                                
299200 S03-RAKNA-ARTIKLAR SECTION.                                              
299300                                                                          
299400     MOVE LOW-VALUE                  TO W-WDL6A1KY-MIN                    
299500     MOVE HIGH-VALUE                 TO W-WDL6A1KY-MAX                    
299600     MOVE W-IDFAKT                   TO W-IDFAKT-MIN                      
299700                                        W-IDFAKT-MAX                      
299800     INSPECT MID-IDKUNDRF(INDX) REPLACING LEADING SPACE BY ZERO           
299900     MOVE MID-IDKUNDRF(INDX)         TO W-IDKUNDRF-MIN                    
300000                                        W-IDKUNDRF-MAX                    
300100     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
300200     MOVE MID-IDKUNDNR(INDX)          TO W-IDKUNDNR-MIN                   
300300                                         W-IDKUNDNR-MAX                   
300400     INSPECT MID-IDKOLLI(INDX) REPLACING LEADING SPACE BY ZERO            
300500     MOVE MID-IDKOLLI(INDX)          TO W-IDKOLLI-MIN                     
300600                                        W-IDKOLLI-MAX                     
300700                                                                          
300800     MOVE ZERO TO W-KVRADER                                               
300900     PERFORM IMS-GU-WLINLD01                                              
301000     PERFORM  UNTIL SEGMENT-SAKNAS                                        
301100        ADD +1 TO W-KVRADER                                               
301200        PERFORM IMS-GN-WLINLD01                                           
301300     END-PERFORM                                                          
301400     .                                                                    
301500     EJECT                                                                
301600 S04-SKAPA-EKOTRANS-A03 SECTION.                                          
301700                                                                          
301800     MOVE WS-IDDC-SEND          TO SEND-WS-IDDC                           
301900     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
302000        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
302100        PERFORM IMS-GU-WDB601-SEND                                        
302200     END-IF                                                               
302300     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
302400        MOVE 6302-IDDISTR       TO DIST35-IDDISTR                         
302500        IF DIST35-REFILL-NA                                               
302600           MOVE 'T10'           TO EKOTRA03-KDEKOHT                       
302700           IF DIST35-CDC-NDC41-REFILL                                     
302900           OR DIST35-CDC-NDC43-REFILL                                     
303001           OR DIST35-CDC-NDC44-REFILL                                     
303101           OR DIST35-CDC-NDC45-REFILL                                     
303201           OR DIST35-CDC-NDC46-REFILL                                     
303202           OR DIST35-CDC-NDC47-REFILL                                     
303300              MOVE 53           TO EKOTRA03-IDFTG                         
303400           ELSE                                                           
303500              MOVE 54           TO EKOTRA03-IDFTG                         
303600           END-IF                                                         
303700        ELSE                                                              
303800           IF DIST35-REFILL-NA-JAP                                        
303900              MOVE 'I20'        TO EKOTRA03-KDEKOHT                       
304000              IF DIST35-JAP-NDC41-REFILL                                  
304200              OR DIST35-JAP-NDC43-REFILL                                  
304301              OR DIST35-JAP-NDC44-REFILL                                  
304400                 MOVE 53         TO EKOTRA03-IDFTG                        
304500              ELSE                                                        
304600                 MOVE 54         TO EKOTRA03-IDFTG                        
304700              END-IF                                                      
304800           END-IF                                                         
304900        END-IF                                                            
305000     ELSE                                                                 
305100        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
305200           AND (DCS-NDC-NA AND DCS-CANADA)                                
305300             MOVE 'T50'      TO EKOTRA03-KDEKOHT                          
305400             MOVE 54          TO EKOTRA03-IDFTG                           
305500        ELSE                                                              
305600           IF (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                       
305700           AND (DCS-NDC-NA AND DCS-USA)                                   
305800             MOVE 'T40'      TO EKOTRA03-KDEKOHT                          
305900             MOVE 53          TO EKOTRA03-IDFTG                           
306000           ELSE                                                           
306100              IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                       
306200              AND (DCS-NDC-NA AND DCS-USA)                                
306300                  MOVE 'T30' TO EKOTRA03-KDEKOHT                          
306400                  MOVE 53    TO EKOTRA03-IDFTG                            
306500              END-IF                                                      
306600           END-IF                                                         
306700        END-IF                                                            
306800     END-IF                                                               
306900                                                                          
307000     MOVE 'A03'           TO EKOTRA03-IDPTYP                              
307100     MOVE JA              TO WS-A03-SKAPAD                                
307200     .                                                                    
307300     EJECT                                                                
307400 S041-SKRIV-EKOTRANS-A03 SECTION.                                         
307500                                                                          
307600     MOVE 'W6030200'        TO FIL-IDPGM IN FIL-WDR801                    
307700     MOVE W-DATUM           TO FIL-TIREGDAT                               
307800     ADD +1                 TO W-TIKLOCK                                  
307900     MOVE W-TIKLOCK         TO FIL-TIKLOCK IN FIL-WDR801                  
308000     ADD +1                 TO W-IDSEKVNR-A03                             
308100     MOVE W-IDSEKVNR-A03    TO FIL-IDSEKVNR IN FIL-WDR801                 
308200     MOVE 'W510'            TO FIL-CT-IDSYSTEM IN FIL-WDR801              
308300     MOVE 'A03'             TO FIL-CT-IDPTYP IN FIL-WDR801                
308400     MOVE ' '               TO FIL-CT-IDVTYP IN FIL-WDR801                
308500     MOVE EKOTRA03-W510A03  TO FIL-WDR801-DATA                            
308600                                                                          
308700     PERFORM IMS-ISRT-FILB01                                              
308800                                                                          
308900     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
309000        ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                              
309100        PERFORM IMS-ISRT-FILB01                                           
309200     END-PERFORM                                                          
309300     MOVE NEJ               TO WS-A03-SKAPAD                              
309400     .                                                                    
309500     EJECT                                                                
309600 S05-SOEK-REMARKUP SECTION.                                               
309700                                                                          
309800     MOVE 1   TO WS-INDX                                                  
309900                 WS-MARKUP                                                
310000     MOVE NEJ TO WS-PROD                                                  
310100                                                                          
310200     PERFORM UNTIL PRODKOD-FINNS OR WS-INDX > MARKUP-TAB-MAX              
310300       IF MARKUP-LPC (WS-INDX) = CLAG-KDPSLLOC                            
310400         MOVE JA               TO WS-PROD                                 
310500       ELSE                                                               
310600         ADD 1                 TO WS-INDX                                 
310700       END-IF                                                             
310800     END-PERFORM                                                          
310900                                                                          
311000     IF PRODKOD-FINNS                                                     
311100       MOVE EKOTRA03-IDDC-REC      TO SPAR-WS-IDDC                        
311200       IF SPAR-WS-IDDC NOT = SPAR-DCS-IDDC                                
311300          MOVE SPAR-WS-IDDC TO W-IDDC-B6                                  
311400          PERFORM IMS-GU-WDB601-SPAR                                      
311500       END-IF                                                             
311600       IF SPAR-DCS-NDC-NA AND SPAR-DCS-USA                                
311700         MOVE MARKUP-FAKTOR-USA (WS-INDX)                                 
311800                               TO WS-MARKUP                               
311900       ELSE                                                               
312000         MOVE MARKUP-FAKTOR-CAN (WS-INDX)                                 
312100                               TO WS-MARKUP                               
312200       END-IF                                                             
312300     END-IF                                                               
312400     .                                                                    
312500     EJECT                                                                
312600 S06-SKAPA-SALDOLOGG SECTION.                                             
312700                                                                          
312800     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
312900                                                                          
313000     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-LOGG-AAAAMMDD                 
313100     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
313200                                   - WS-LOGG-AAAAMMDD                     
313300     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
313400     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
313500                                   - WS-TTMMSSTH                          
313600     MOVE 9                        TO LOGG-IDSEKVNR                       
313700     MOVE W-IDDC                   TO LOGG-IDDC                           
313800     MOVE 'INBO'                   TO LOGG-IDHUVTYP                       
313900     MOVE 'R32'                    TO LOGG-IDSUBTYP                       
314000     MOVE 'W6030200'               TO LOGG-IDPGM                          
314100     MOVE '6302'                   TO LOGG-IDTRANS                        
314200     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
314300     MOVE SPACE                    TO LOGG-REF                            
314400     MOVE W-IDFAKT                 TO LOGG-IDFAKT                         
314500     MOVE W-KVAVIS                 TO LOGG-KVART-SALDO                    
314900     MOVE ' '                      TO LOGG-IDTECKEN-KVEFRS                
315000     IF DCS-CDC                                                           
315100       COMPUTE LOGG-KVAKS          =  CLAG-KVAKS-CDC                      
315200                                   +  CLAG-KVAKS-T                        
315300       MOVE CLAG-KVAKS-PAV         TO LOGG-KVAKS-PAV                      
315400       MOVE CLAG-KVEFRS            TO LOGG-KVEFRS                         
315500       MOVE CLAG-KVLS              TO LOGG-KVLS                           
315600     ELSE                                                                 
315700       MOVE SLAG-KVAKS-SDC         TO LOGG-KVAKS                          
315800       MOVE SLAG-KVAKS-PAV         TO LOGG-KVAKS-PAV                      
315900       MOVE SLAG-KVEFRS            TO LOGG-KVEFRS                         
316000       MOVE SLAG-KVLS              TO LOGG-KVLS                           
316100     END-IF                                                               
316200     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
316400                                                                          
316500     PERFORM IMS-ISRT-WDL901                                              
316600     IF SEGMENT-FINNS-REDAN                                               
316700       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
316800         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
316900         PERFORM IMS-ISRT-WDL901                                          
317000       END-PERFORM                                                        
317100     END-IF                                                               
317200     .                                                                    
317300     EJECT                                                                
317400 S07-SKAPA-SAP-TRANS SECTION.                                             
317500******************************************************************        
317600* ÄT SAP  UPPDAT-TRANS WDR9 SKAPAS VID IDDC-SEND = CDC                    
317700* 980420  BÅDE FÖR SDC OCH NDC. TRANSEN SKA ERSÄTTA 'AVV'-TRANS           
317800*         (WDR8) FÖR SDC OCH TILLKOMMER FÖR NDC. 'AVV'-TRANS              
317900*         LIGGER KVAR TILLS VIDARE (WDR8).                                
318000******************************************************************        
318100                                                                          
318200     MOVE 'W6030200'                  TO FIL-IDPGM IN FIL-WDR901          
318300     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
318400     MOVE WS-SAP-AAAAMMDD             TO FIL-DAREGDAT                     
318500                                         EKH-DAVERDAT                     
318600     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
318700     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR901        
318800     ADD +1                           TO W-IDSEKVNR-SAP                   
318900     MOVE 'W510EKHA'                 TO FIL-IDCPYTXT IN FIL-WDR901        
319000     MOVE MSG-SIGNON-USERID          TO FIL-IDUSER IN FIL-WDR901          
319100     MOVE W-IDSEKVNR-SAP             TO FIL-IDSEKVNR IN FIL-WDR901        
319201     MOVE INL-IDDISTR                TO DIST35-IDDISTR                    
319301     MOVE SPACE                      TO EKH-KDTRADP                       
319401     IF DIST35-RETUR                                                      
319501         MOVE '502'                  TO EKH-KDEKHHT                       
319601         MOVE '501'                  TO EKH-KDEKSHT                       
319701         MOVE INL-PRARTNTO           TO EKH-PRARTNTO                      
319801         MOVE ZERO                   TO EKH-PRARTNTO                      
319901         MOVE 'SEPV'                 TO EKH-KDTRADP                       
320001     ELSE                                                                 
320101       IF DCS-CDC                                                         
320201         MOVE '102'                  TO EKH-KDEKHHT                       
320301         MOVE '122'                  TO EKH-KDEKSHT                       
320401         MOVE INL-PRARTNTO           TO EKH-PRARTNTO                      
320501       ELSE                                                               
320601         MOVE '503'                  TO EKH-KDEKHHT                       
320701         MOVE '501'                  TO EKH-KDEKSHT                       
320801       END-IF                                                             
320901       MOVE ZERO                      TO EKH-PRARTNTO                     
321001     END-IF                                                               
321101     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
321201     MOVE WS-IDDC-SEND                TO SEND-WS-IDDC                     
321301     MOVE WS-IDDC-SEND                TO EKH-IDDC-SEND                    
321401     MOVE MID-IDDC-SPAR               TO EKH-IDDC-REC                     
321501     MOVE WS-SAP-IDDISTR              TO EKH-IDDISTR                      
321601     MOVE WS-SAP-IDKUNDNR             TO EKH-IDKUNDNR                     
321701*******************************                                           
321801     MOVE ZERO TO NOLL-RAKNARE                                            
321901     MOVE W-IDFAKT                   TO WS-SAP-IDFAKT                     
322001     MOVE WS-SAP-IDFAKT              TO WS-SAP-X-IDFAKT                   
322101     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
322201          FOR LEADING ZERO                                                
322301     ADD +1 TO NOLL-RAKNARE                                               
322401     UNSTRING WS-SAP-X-IDFAKT      INTO EKH-IDVERGL                       
322501          WITH POINTER NOLL-RAKNARE                                       
322601     MOVE ZERO                        TO EKH-KDPSLLOC                     
322701                                         EKH-PRARTSJK                     
322801                                         EKH-PRHEMTAG                     
322901                                         EKH-PRINK                        
323001                                         EKH-PRDIRLON                     
323101                                         EKH-PRDMTRL                      
323201                                         EKH-PROVRPAL                     
323301                                         EKH-SUBEL                        
323401     MOVE W-IDARTNR                   TO EKH-IDARTNR                      
323501     MOVE SPACE                       TO EKH-FLLSBOK                      
323601     MOVE 'SEK'                       TO EKH-KDVALISO                     
323701********** EV ÄNDRING FÖR PRKURS                                          
323801     MOVE 1.00                        TO EKH-PRKURS                       
323901**********                                                                
324001     COMPUTE EKH-KVANTAL = W-KVAVIS * -1                                  
324101     MOVE '6302'                      TO EKH-IDTRANS                      
324201     MOVE ZERO                        TO EKH-BEVAT                        
324301                                         EKH-IDANALYS                     
324401                                         EKH-IDKONTO                      
324501                                         EKH-KDANMORS                     
324601                                         EKH-SUVAT                        
324701                                         EKH-KDFRAKT                      
324801                                         EKH-PRLANDCO                     
324901                                         EKH-DAAVIDAT                     
325001                                         EKH-IDAVINR                      
325101                                         EKH-KDAVVTYP                     
325201                                         EKH-KDRT                         
325301                                         EKH-KVANTMOT                     
325401                                         EKH-KVAVIS                       
325501     MOVE WS-KDSORT                  TO  EKH-KDSORT                       
325601     MOVE SPACE                      TO  EKH-IDLEVNR                      
325701                                         EKH-FLDCET                       
325801                                         EKH-IDKST                        
325901                                         EKH-IDKUNDRF                     
326001                                         EKH-IDFAKT-EXP                   
326101                                                                          
326201     PERFORM IMS-ISRT-WLSAPA01                                            
326301                                                                          
326401     PERFORM UNTIL SEGMENT-FINNS                                          
326501         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                             
326601         PERFORM IMS-ISRT-WLSAPA01                                        
326701     END-PERFORM                                                          
326801     .                                                                    
326901     EJECT                                                                
327001 S07A-SKAPA-SAP-TRANS SECTION.                                            
327101***** MAPPING OF SAP TRANSACTIONS TO USA                                  
327201                                                                          
327301     MOVE 'W6030200'                  TO FIL-IDPGM IN FIL-WDR801          
327401     ACCEPT FIL-TIREGDAT IN FIL-WDR801 FROM DATE                          
327501     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
327601                                         R8-EKH-DAVERDAT                  
327701     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
327801     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR801        
327901     ADD +1                           TO W-IDSEKVNR-SAP                   
328001     MOVE 'W561EKHA'                TO FIL-IDCPYTXT IN FIL-WDR801         
328101     MOVE W-IDSEKVNR-SAP            TO FIL-IDSEKVNR IN FIL-WDR801         
328201     MOVE '102'                       TO R8-EKH-KDEKHHT                   
328301     MOVE '132'                       TO R8-EKH-KDEKSHT                   
328401     MOVE WS-PRAVCOST                 TO R8-EKH-PRARTSTD                  
328501     MOVE 'DET  '                     TO R8-EKH-KDEKNIVA                  
328601     MOVE WS-IDDC-SEND                TO SEND-WS-IDDC                     
328701                                         R8-EKH-IDDC-SEND                 
328801     MOVE MID-IDDC-SPAR               TO R8-EKH-IDDC-REC                  
328901     MOVE WS-SAP-IDDISTR              TO R8-EKH-IDDISTR                   
329001     MOVE WS-SAP-IDKUNDNR             TO R8-EKH-IDKUNDNR                  
329101*******************************                                           
329201     MOVE ZERO                        TO NOLL-RAKNARE                     
329301     MOVE W-IDFAKT                    TO WS-SAP-IDFAKT                    
329401     MOVE WS-SAP-IDFAKT               TO WS-SAP-X-IDFAKT                  
329501     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
329601          FOR LEADING ZERO                                                
329701     ADD +1                           TO NOLL-RAKNARE                     
329801     UNSTRING WS-SAP-X-IDFAKT       INTO R8-EKH-IDVERGL                   
329901          WITH POINTER NOLL-RAKNARE                                       
330001     MOVE ZERO                        TO R8-EKH-KDPSLLOC                  
330101     MOVE INL-PRARTNTO                TO R8-EKH-PRARTNTO                  
330201     MOVE ZERO                        TO R8-EKH-PRARTSJK                  
330301                                         R8-EKH-PRHEMTAG                  
330401                                         R8-EKH-PRINK                     
330501                                         R8-EKH-PRDIRLON                  
330601                                         R8-EKH-PRDMTRL                   
330701                                         R8-EKH-PROVRPAL                  
330801                                         R8-EKH-SUBEL                     
330901     MOVE W-IDARTNR                   TO R8-EKH-IDARTNR                   
331001     MOVE SPACE                       TO R8-EKH-FLLSBOK                   
331101     MOVE 'USD'                       TO R8-EKH-KDVALISO                  
331201********** EV ÄNDRING FÖR PRKURS                                          
331301     MOVE 1.00                        TO R8-EKH-PRKURS                    
331401**********                                                                
331501     COMPUTE R8-EKH-KVANTAL = W-KVAVIS * -1                               
331601     MOVE '6302'                      TO R8-EKH-IDTRANS                   
331701     MOVE ZERO                        TO R8-EKH-BEVAT                     
331801                                         R8-EKH-IDANALYS                  
331901                                         R8-EKH-IDKONTO                   
332001                                         R8-EKH-KDANMORS                  
332101                                         R8-EKH-SUVAT                     
332201                                         R8-EKH-KDFRAKT                   
332301                                         R8-EKH-PRLANDCO                  
332401                                         R8-EKH-DAAVIDAT                  
332501                                         R8-EKH-IDAVINR                   
332601                                         R8-EKH-KDAVVTYP                  
332701                                         R8-EKH-KDRT                      
332801                                         R8-EKH-KVANTMOT                  
332901                                         R8-EKH-KVAVIS                    
333001     MOVE WS-KDSORT                  TO  R8-EKH-KDSORT                    
333101     MOVE SPACE                      TO  R8-EKH-IDLEVNR                   
333201                                         R8-EKH-IDKST                     
333301     MOVE SPACE                      TO  R8-EKH-FLDCET                    
333401     MOVE SPACE                      TO  R8-EKH-IDKUNDRF                  
333501     MOVE SPACE                      TO  R8-EKH-IDFAKT-EXP                
333601     MOVE 'US01'                     TO  R8-EKH-KDTRADP                   
333701     MOVE SPACE                      TO  R8-EKH-IDKUNDRF                  
333801     MOVE WS-KDPRODSL                TO  R8-EKH-KDPRODSL                  
333901     PERFORM IMS-ISRT-FILB01                                              
334001                                                                          
334101     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
334201        ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                              
334301        PERFORM IMS-ISRT-FILB01                                           
334401     END-PERFORM                                                          
334501     .                                                                    
334601     EJECT                                                                
334701 S08-SKAPA-AVVIK-TRANS SECTION.                                           
334801                                                                          
334901     MOVE INL-KVAVIS      TO FILC-KVANTAL                                 
335001     MOVE 'LOST'          TO FILC-AVVIKELSETYP                            
335101     MOVE 6302-TIFAKT     TO WS-SEKEL-TEST                                
335201                             WS-AVVIK-AAMMDD                              
335301     IF WS-SEKEL2 = 9                                                     
335401        MOVE 19           TO WS-AVVIK-SS                                  
335501     ELSE                                                                 
335601        MOVE 20           TO WS-AVVIK-SS                                  
335701     END-IF                                                               
335801     MOVE WS-AVVIK-AAAAMMDD   TO FILC-DAFAKT                              
335901     MOVE W-IDARTNR           TO FILC-IDARTNR                             
336001     MOVE WS-IDDC-SEND        TO FILC-IDDC-SEND                           
336101     MOVE MID-IDDC-SPAR       TO FILC-IDDC-REC                            
336201     MOVE W-IDFAKT            TO FILC-IDFAKT                              
336301     INSPECT MID-IDKUNDRF(INDX) REPLACING LEADING SPACE BY ZERO           
336401     MOVE MID-IDKUNDRF(INDX)  TO FILC-IDKUNDRF                            
336501     MOVE ZERO                TO FILC-PRARTBES-PR                         
336601     MOVE 6302-IDLEVNR        TO FILC-IDLEVNR                             
336701     MOVE FILC-W61244         TO FILC-FIL-WDR301-DATA                     
336801     ACCEPT W-TID FROM TIME                                               
336901     IF W-TID = FILC-FIL-TIKLOCK                                          
337001        ADD +1                TO FILC-FIL-IDSEKVNR                        
337101     ELSE                                                                 
337201        MOVE W-TID            TO FILC-FIL-TIKLOCK                         
337301        MOVE +1               TO FILC-FIL-IDSEKVNR                        
337401     END-IF                                                               
337501     PERFORM IMS-ISRT-WLFILC                                              
337601     .                                                                    
337701     EJECT                                                                
337801 S09-SKAPA-LDC-TRANS SECTION.                                             
337901                                                                          
338001     MOVE INL-KVAVIS      TO FILC2-KVANTAL                                
338101                             FILC2-KVAVIS                                 
338201     MOVE 'LOST'          TO FILC2-AVVIKELSETYP                           
338301     MOVE 6302-TIFAKT     TO WS-SEKEL-TEST                                
338401                             WS-AVVIK-AAMMDD                              
338501     IF WS-SEKEL2 = 9                                                     
338601        MOVE 19           TO WS-AVVIK-SS                                  
338701     ELSE                                                                 
338801        MOVE 20           TO WS-AVVIK-SS                                  
338901     END-IF                                                               
339001     MOVE WS-AVVIK-AAAAMMDD   TO FILC2-DAFAKT                             
339101     MOVE W-IDARTNR           TO FILC2-IDARTNR                            
339201     IF INL-IDDC-LEV NOT = SPACE                                          
339301        MOVE INL-IDDC-LEV     TO FILC2-IDDC-SEND                          
339401     ELSE                                                                 
339501        MOVE WS-IDDC-SEND     TO FILC2-IDDC-SEND                          
339601     END-IF                                                               
339701     MOVE MID-IDDC-SPAR       TO FILC2-IDDC-REC                           
339801     MOVE W-IDFAKT            TO FILC2-IDFAKT                             
339901     INSPECT MID-IDKUNDRF(INDX) REPLACING LEADING SPACE BY ZERO           
340001     MOVE MID-IDKUNDRF(INDX)  TO FILC2-IDKUNDRF                           
340101     MOVE ZERO                TO FILC2-IDKUNDNR                           
340201     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
340301     MOVE MID-IDKUNDNR(INDX)  TO FILC2-IDKUNDNR                           
340401     INSPECT MID-IDKOLLI(INDX)  REPLACING LEADING SPACE BY ZERO           
340501     MOVE MID-IDKOLLI(INDX)   TO FILC2-IDKOLLI                            
340601     MOVE ZERO                TO FILC2-PRARTSTD                           
340701     MOVE 1                   TO FILC2-KDSORT1                            
340801     MOVE W-DATUM             TO FILC2-DAREGDAT                           
340901     ADD 20000000             TO FILC2-DAREGDAT                           
341001     IF MID-IDUSER-003 = ALL '+'                                          
341101        MOVE SPACE            TO FILC2-IDUSER                             
341201     ELSE                                                                 
341301        MOVE MID-IDUSER-003   TO FILC2-IDUSER                             
341401     END-IF                                                               
341501     MOVE FILC2-W61247        TO FILC2-FIL-WDR301-DATA                    
341601     ACCEPT W-TID FROM TIME                                               
341701     IF W-TID = FILC2-FIL-TIKLOCK                                         
341801        ADD +1                TO FILC2-FIL-IDSEKVNR                       
341901     ELSE                                                                 
342001        MOVE W-TID            TO FILC2-FIL-TIKLOCK                        
342101        MOVE +1               TO FILC2-FIL-IDSEKVNR                       
342201     END-IF                                                               
342301     PERFORM IMS-ISRT-WLFILC2                                             
342401     .                                                                    
342501     EJECT                                                                
342601                                                                          
342701 X010-CALL-W006KOM        SECTION.                                        
342801                                                                          
342901     MOVE KOM-AREA        TO P-TO-P-DATA                                  
343001     CALL W006KOM         USING MSG-PCB                                   
343101                                ALT3-PCB                                  
343201                                KOMA-PCB                                  
343301                                MSG-KOM-WMSGKOM                           
343401                                P-TO-P-AREA                               
343501                                                                          
343601     .                                                                    
343701     EJECT                                                                
343801 X020-UPPDATERA-WL630111 SECTION.                                         
343901                                                                          
344001     MOVE NEJ TO WS-FAKT-INFO-DLET                                        
344101                                                                          
344201     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
344301     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
344401     MOVE W-IDFAKT        TO W-IDFAKT-MIN                                 
344501                             W-IDFAKT-MAX                                 
344601     MOVE '310'           TO W-IDPTYP                                     
344701     PERFORM IMS-GU-WLINLD01-FIRST-310                                    
344801                                                                          
344901     IF  SEGMENT-SAKNAS                                                   
345001         MOVE 'R30'            TO W-IDPTYP                                
345101         PERFORM IMS-GU-WLINLD01-FIRST-310                                
345201         IF SEGMENT-SAKNAS                                                
345301            MOVE MID-IDDC-SPAR TO W-6301-IDDC                             
345401            PERFORM IMS-GHU-WL630111                                      
345501            PERFORM IMS-DLET-WL630111                                     
345601            MOVE JA TO WS-FAKT-INFO-DLET                                  
345701         ELSE                                                             
345801            PERFORM S03-RAKNA-ARTIKLAR                                    
345901            MOVE MID-IDDC-SPAR TO W-6301-IDDC                             
346001            PERFORM IMS-GHU-WL630111                                      
346101            ADD +1             TO 6302-KVKOLLI-MOT                        
346201            ADD W-KVRADER      TO 6302-KVRADER-MOT                        
346301            IF MID-CMD-IN(INDX) = 'LOS'                                   
346401               MOVE 'L'        TO 6302-KDTRPSTA                           
346501               ADD WS-RAKNARE  TO 6302-KVRADER-MOT                        
346601            END-IF                                                        
346701            IF MID-CMD-IN(INDX) = 'RET'                                   
346801               ADD WS-RAKNARE  TO 6302-KVRADER-MOT                        
346901            END-IF                                                        
347001            PERFORM IMS-REPL-WL630111                                     
347101         END-IF                                                           
347201     ELSE                                                                 
347301         PERFORM S03-RAKNA-ARTIKLAR                                       
347401         MOVE MID-IDDC-SPAR    TO W-6301-IDDC                             
347501         PERFORM IMS-GHU-WL630111                                         
347601         ADD +1                TO 6302-KVKOLLI-MOT                        
347701         ADD W-KVRADER         TO 6302-KVRADER-MOT                        
347801         IF MID-CMD-IN(INDX) = 'LOS'                                      
347901            MOVE 'L'        TO 6302-KDTRPSTA                              
348001            ADD WS-RAKNARE  TO 6302-KVRADER-MOT                           
348101         END-IF                                                           
348201         IF MID-CMD-IN(INDX) = 'RET'                                      
348301            ADD WS-RAKNARE  TO 6302-KVRADER-MOT                           
348401         END-IF                                                           
348501         PERFORM IMS-REPL-WL630111                                        
348601     END-IF                                                               
348701     MOVE ZERO TO WS-RAKNARE                                              
348801     .                                                                    
348901     EJECT                                                                
349001 S98-SKAPA-LEVANM-TRANS SECTION.                                          
349101                                                                          
349201     PERFORM IMS-GHU-WL630501                                             
349301     IF SEGMENT-FINNS                                                     
349401        PERFORM IMS-GHU-WL630511                                          
349501        IF SEGMENT-FINNS                                                  
349601           MOVE SPAR-AREA-6308  TO 6308-WDGX6308                          
349701           PERFORM IMS-ISRT-WL630521                                      
349801***** TILLAGT AV MÅNS FÖR ATT KLARA TVÅ RADER MED SAMMA ARTNR             
349901           PERFORM UNTIL SEGMENT-FINNS                                    
350001             ADD 1 TO 6308-IDRADNR                                        
350101             PERFORM IMS-ISRT-WL630521                                    
350201           END-PERFORM                                                    
350301        ELSE                                                              
350401           MOVE SPAR-AREA-6306  TO 6306-WDGX6306                          
350501           MOVE NEJ             TO 6306-FLKLAR                            
350601           PERFORM IMS-ISRT-WL630511                                      
350701           MOVE SPAR-AREA-6308  TO 6308-WDGX6308                          
350801           PERFORM IMS-ISRT-WL630521                                      
350901***** TILLAGT AV MÅNS FÖR ATT KLARA TVÅ RADER MED SAMMA ARTNR             
351001           PERFORM UNTIL SEGMENT-FINNS                                    
351101             ADD 1 TO 6308-IDRADNR                                        
351201             PERFORM IMS-ISRT-WL630521                                    
351301           END-PERFORM                                                    
351401        END-IF                                                            
351501     END-IF                                                               
351601     .                                                                    
351701     EJECT                                                                
351801 MFS-RENSA-FAELT-UT SECTION.                                              
351901                                                                          
352001*    --- ALLA UTDATA-FÄLT                                                 
352101*    --- INKL. BLÄDDRINGSNYCKLAR                                          
352201     MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-UT                                
352301                             MOD-IDKUNDRF-ENTER                           
352401                             MOD-IDKUNDRF-NEXT                            
352501                             MOD-IDKUNDNR-ENTER                           
352601                             MOD-IDKUNDNR-NEXT                            
352701                             MOD-IDKOLLI-ENTER                            
352801                             MOD-IDKOLLI-NEXT                             
352901                             MOD-IDDC-SPAR                                
353001     .                                                                    
353101     EJECT                                                                
353201 MFS-RENSA-FAELT-IN SECTION.                                              
353301                                                                          
353401*    --- ALLA INDATA-FÄLT                                                 
353501     MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-IN                                
353601                             MOD-IDUSER-003                               
353701                             MOD-ADINLOMR-PRT                             
353801     .                                                                    
353901     EJECT                                                                
354001 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
354101                                                                          
354201*    --- ALLA UTDATA-FÄLT                                                 
354301*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
354401     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFAKT-UT                              
354501                               MOD-IDKUNDRF-ENTER                         
354601                               MOD-IDKUNDRF-NEXT                          
354701                               MOD-IDKUNDNR-ENTER                         
354801                               MOD-IDKUNDNR-NEXT                          
354901                               MOD-IDKOLLI-ENTER                          
355001                               MOD-IDKOLLI-NEXT                           
355101                               MOD-IDDC-SPAR                              
355201     MOVE +1 TO INDX                                                      
355301     PERFORM UNTIL INDX > MAX-INDX                                        
355401       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
355501       ADD +1 TO INDX                                                     
355601     END-PERFORM                                                          
355701     .                                                                    
355801     EJECT                                                                
355901 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
356001                                                                          
356101*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
356201     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-IN     (INDX)                      
356301                               MOD-IDKUNDRF     (INDX)                    
356401                               MOD-IDKUNDNR     (INDX)                    
356501                               MOD-IDKOLLI      (INDX)                    
356601                               MOD-KDKOLLI      (INDX)                    
356701                               MOD-IDARTNR-KOLLI(INDX)                    
356801                               MOD-IDARTNR-NEW  (INDX)                    
356901                               MOD-IDARTNR-PRIO (INDX)                    
357001                               MOD-TEINFO       (INDX)                    
357101                               MOD-ADINLOMR     (INDX)                    
357201     .                                                                    
357301     EJECT                                                                
357401 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
357501                                                                          
357601*    --- ALLA INDATA-FÄLT                                                 
357701     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFAKT-IN                              
357801                               MOD-IDUSER-003                             
357901                               MOD-ADINLOMR-PRT                           
358001     MOVE +1 TO INDX                                                      
358101     PERFORM UNTIL INDX > MAX-INDX                                        
358201       IF MID-ADINLOMR(INDX) NOT = ALL '+'                                
358301          MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR(INDX)                    
358401       END-IF                                                             
358501       ADD +1 TO INDX                                                     
358601     END-PERFORM                                                          
358701     .                                                                    
358801     EJECT                                                                
358901* --- IMS SEKTIONER ---                                                   
359001     SKIP3                                                                
359101 IMS-GET-MSG SECTION.                                                     
359201     MOVE '  QC' TO GODK-STATUSKODER                                      
359301     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
359401     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
359501     PERFORM IMS-STATUSKONTROLL                                           
359601     .                                                                    
359701     SKIP2                                                                
359801 IMS-INSERT-MSG SECTION.                                                  
359901     IF SWEDISH-TEXT                                                      
360001        IF MSGI-IDLAND-SPR NOT = 'GB'                                     
360101           MOVE '0' TO MFS-KDHUVOMR                                       
360201        END-IF                                                            
360301     END-IF                                                               
360401     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
360501     MOVE SPACE TO GODK-STATUSKODER                                       
360601     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
360701     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
360801     PERFORM IMS-STATUSKONTROLL                                           
360901     .                                                                    
361001     SKIP2                                                                
361101 IMS-INSERT-ALTMSG SECTION.                                               
361201     MOVE SPACE TO GODK-STATUSKODER                                       
361301     CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
361401     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
361501     PERFORM IMS-STATUSKONTROLL                                           
361601     .                                                                    
361701     EJECT                                                                
361801 IMS-ISRT-ALT1-PCB-6303 SECTION.                                          
361901                                                                          
362001     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
362101     MOVE SPACE TO GODK-STATUSKODER                                       
362201     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-AREA                         
362301                                                                          
362401     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
362501     PERFORM IMS-STATUSKONTROLL                                           
362601     .                                                                    
362701     SKIP3                                                                
362801 IMS-ISRT-ALT4-PCB-6303 SECTION.                                          
362901                                                                          
363001     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
363101     MOVE SPACE TO GODK-STATUSKODER                                       
363201     CALL CBLTDLI USING ISRT ALT4-PCB P-TO-P-AREA-2                       
363301                                                                          
363401     MOVE ALT4-STATUS-CODE TO STATUS-WS                                   
363501     PERFORM IMS-STATUSKONTROLL                                           
363601     .                                                                    
363701     EJECT                                                                
363801 IMS-GHU-WL630111 SECTION.                                                
363901                                                                          
364001     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
364101          DELIMITED BY SIZE INTO SSA1                                     
364201     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
364301          DELIMITED BY SIZE INTO SSA2                                     
364401     MOVE SPACE           TO GODK-STATUSKODER                             
364501     CALL CBLTDLI USING GHU GX63-PCB DLI-IO-AREA6 SSA1 SSA2               
364601                                                                          
364701     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
364801     PERFORM IMS-STATUSKONTROLL                                           
364901     .                                                                    
365001     SKIP3                                                                
365101 IMS-REPL-WL630111 SECTION.                                               
365201                                                                          
365301     MOVE SPACE           TO GODK-STATUSKODER                             
365401     CALL CBLTDLI USING REPL GX63-PCB DLI-IO-AREA6                        
365501                                                                          
365601     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
365701     PERFORM IMS-STATUSKONTROLL                                           
365801     .                                                                    
365901     SKIP3                                                                
366001 IMS-DLET-WL630111 SECTION.                                               
366101                                                                          
366201     MOVE SPACE           TO GODK-STATUSKODER                             
366301     CALL CBLTDLI USING DLET GX63-PCB DLI-IO-AREA6                        
366401                                                                          
366501     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
366601     PERFORM IMS-STATUSKONTROLL                                           
366701     .                                                                    
366801     SKIP2                                                                
366901 IMS-GHU-WL630501 SECTION.                                                
367001     STRING 'WL630501(WDGXKEY = ' W-6305KEY-X ')'                         
367101          DELIMITED BY SIZE INTO SSA1                                     
367201     MOVE 'GE  ' TO GODK-STATUSKODER                                      
367301     CALL CBLTDLI USING GHU GX65-PCB DLI-IO-AREA5 SSA1                    
367401     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
367501     PERFORM IMS-STATUSKONTROLL                                           
367601     .                                                                    
367701     SKIP2                                                                
367801 IMS-GHU-WL630511 SECTION.                                                
367901     STRING 'WL630501(WDGXKEY = ' W-6305KEY-X ')'                         
368001          DELIMITED BY SIZE INTO SSA1                                     
368101     STRING 'WL630511(IDFAKT  = ' W-IDFAKT-X  ')'                         
368201          DELIMITED BY SIZE INTO SSA2                                     
368301     MOVE 'GE  ' TO GODK-STATUSKODER                                      
368401     CALL CBLTDLI USING GHU GX65-PCB DLI-IO-AREA5 SSA1 SSA2               
368501     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
368601     PERFORM IMS-STATUSKONTROLL                                           
368701     .                                                                    
368801     EJECT                                                                
368901 IMS-ISRT-WL630511 SECTION.                                               
369001     STRING 'WL630501(WDGXKEY = ' W-6305KEY-X  ')'                        
369101          DELIMITED BY SIZE INTO SSA1                                     
369201     MOVE 'WL630511 ' TO SSA2                                             
369301     MOVE SPACE TO GODK-STATUSKODER                                       
369401     CALL CBLTDLI USING ISRT GX65-PCB DLI-IO-AREA5 SSA1 SSA2              
369501     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
369601     PERFORM IMS-STATUSKONTROLL                                           
369701     .                                                                    
369801     SKIP2                                                                
369901 IMS-ISRT-WL630521 SECTION.                                               
370001     STRING 'WL630501(WDGXKEY = ' W-6305KEY-X   ')'                       
370101          DELIMITED BY SIZE INTO SSA1                                     
370201     STRING 'WL630511(IDFAKT  = ' W-IDFAKT-X     ')'                      
370301          DELIMITED BY SIZE INTO SSA2                                     
370401     MOVE 'WL630521 ' TO SSA3                                             
370501     MOVE '  II' TO GODK-STATUSKODER                                      
370601     CALL CBLTDLI USING ISRT GX65-PCB DLI-IO-AREA5 SSA1 SSA2              
370701                                                   SSA3                   
370801     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
370901     PERFORM IMS-STATUSKONTROLL                                           
371001     .                                                                    
371101     SKIP3                                                                
371201 IMS-REPL-WL630511 SECTION.                                               
371301     MOVE SPACE TO GODK-STATUSKODER                                       
371401     CALL CBLTDLI USING REPL GX65-PCB DLI-IO-AREA5                        
371501     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
371601     PERFORM IMS-STATUSKONTROLL                                           
371701     .                                                                    
371801     EJECT                                                                
371901 IMS-GU-WLINLD01 SECTION.                                                 
372001                                                                          
372101     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
372201                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
372301          DELIMITED BY SIZE INTO SSA1                                     
372401     MOVE '  GE' TO GODK-STATUSKODER                                      
372501     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA1 SSA1                     
372601                                                                          
372701     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
372801     PERFORM IMS-STATUSKONTROLL                                           
372901     .                                                                    
373001     SKIP3                                                                
373101 IMS-GN-WLINLD01 SECTION.                                                 
373201                                                                          
373301     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
373401                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
373501          DELIMITED BY SIZE INTO SSA1                                     
373601     MOVE '  GE' TO GODK-STATUSKODER                                      
373701     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA1 SSA1                     
373801                                                                          
373901     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
374001     PERFORM IMS-STATUSKONTROLL                                           
374101     .                                                                    
374201     EJECT                                                                
374301 IMS-GU-WLINLD01-FIRST-310 SECTION.                                       
374401                                                                          
374501     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
374601                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
374701                    '&IDPTYP  = ' W-IDPTYP ')'                            
374801          DELIMITED BY SIZE INTO SSA1                                     
374901     MOVE '  GE' TO GODK-STATUSKODER                                      
375001     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA1 SSA1                     
375101                                                                          
375201     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
375301     PERFORM IMS-STATUSKONTROLL                                           
375401     .                                                                    
375501     SKIP3                                                                
375601 IMS-GU-INLC-WLINLC11-F SECTION.                                          
375701                                                                          
375801     STRING 'WLINLC11(WDL6ASEQ=>' W-WDL6ASEQ-MIN                          
375901                    '&WDL6ASEQ=<' W-WDL6ASEQ-MAX ')'                      
376001          DELIMITED BY SIZE INTO SSA1                                     
376101     MOVE '  GE' TO GODK-STATUSKODER                                      
376201     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA2 SSA1                     
376301                                                                          
376401     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
376501     PERFORM IMS-STATUSKONTROLL                                           
376601     .                                                                    
376701     EJECT                                                                
376801 IMS-GN-INLC-WLINLC11 SECTION.                                            
376901                                                                          
377001     STRING 'WLINLC11(WDL6ASEQ=>' W-WDL6ASEQ-MIN                          
377101                    '&WDL6ASEQ=<' W-WDL6ASEQ-MAX ')'                      
377201          DELIMITED BY SIZE INTO SSA1                                     
377301     MOVE '  GE' TO GODK-STATUSKODER                                      
377401     CALL CBLTDLI USING GN INLC-PCB DLI-IO-AREA2 SSA1                     
377501                                                                          
377601     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
377701     PERFORM IMS-STATUSKONTROLL                                           
377801     .                                                                    
377901     SKIP3                                                                
378001 IMS-GHU-INLC1-WLINLC11 SECTION.                                          
378101                                                                          
378201     STRING 'WLINLC01(IDARTNR = ' W-IDARTNR-X ')'                         
378301          DELIMITED BY SIZE INTO SSA1                                     
378401     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
378501          DELIMITED BY SIZE INTO SSA2                                     
378601     MOVE '  ' TO GODK-STATUSKODER                                        
378701     CALL CBLTDLI USING GHU INLC1-PCB DLI-IO-AREA2 SSA1 SSA2              
378801                                                                          
378901     MOVE INLC1-STATUS-CODE TO STATUS-WS                                  
379001     PERFORM IMS-STATUSKONTROLL                                           
379101     .                                                                    
379201     SKIP3                                                                
379301 IMS-REPL-INLC1-WLINLC11 SECTION.                                         
379401                                                                          
379501     MOVE '  ' TO GODK-STATUSKODER                                        
379601     CALL CBLTDLI USING REPL INLC1-PCB DLI-IO-AREA2                       
379701                                                                          
379801     MOVE INLC1-STATUS-CODE TO STATUS-WS                                  
379901     PERFORM IMS-STATUSKONTROLL                                           
380001     .                                                                    
380101     EJECT                                                                
380201 IMS-GU-ARTS11   SECTION.                                                 
380301                                                                          
380401     STRING 'WLARTS01(IDARTNR = ' W-IDARTNR-X ')'                         
380501          DELIMITED BY SIZE INTO SSA1                                     
380601     STRING 'WLARTS11(IDDC    = ' W-IDDC  ')'                             
380701          DELIMITED BY SIZE INTO SSA2                                     
380801     MOVE SPACE  TO GODK-STATUSKODER                                      
380901     CALL CBLTDLI USING GU  ARTS-PCB DLI-IO-AREA SSA1 SSA2                
381001     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
381101     PERFORM IMS-STATUSKONTROLL                                           
381201     .                                                                    
381301     SKIP3                                                                
381401 IMS-GHU-ARTS11   SECTION.                                                
381501                                                                          
381601     STRING 'WLARTS01(IDARTNR = ' W-IDARTNR-X ')'                         
381701          DELIMITED BY SIZE INTO SSA1                                     
381801     STRING 'WLARTS11(IDDC    = ' W-IDDC  ')'                             
381901          DELIMITED BY SIZE INTO SSA2                                     
382001     MOVE SPACE  TO GODK-STATUSKODER                                      
382101     CALL CBLTDLI USING GHU  ARTS-PCB DLI-IO-AREA SSA1 SSA2               
382201     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
382301     PERFORM IMS-STATUSKONTROLL                                           
382401     .                                                                    
382501     EJECT                                                                
382601 IMS-REPL-ARTS11 SECTION.                                                 
382701                                                                          
382801     MOVE '  ' TO GODK-STATUSKODER                                        
382901     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA                         
383001                                                                          
383101     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
383201     PERFORM IMS-STATUSKONTROLL                                           
383301     .                                                                    
383401     SKIP3                                                                
383501 IMS-ISRT-FILB01 SECTION.                                                 
383601                                                                          
383701     MOVE '  '            TO GODK-STATUSKODER                             
383801     MOVE   'WLFILB01 '   TO SSA1                                         
383901     CALL CBLTDLI USING ISRT FILB-PCB DLI-IO-AREA SSA1                    
384001     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
384101     PERFORM IMS-STATUSKONTROLL                                           
384201     .                                                                    
384301     EJECT                                                                
384401 IMS-GET-WLARTC01 SECTION.                                                
384501                                                                          
384601     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
384701          DELIMITED BY SIZE INTO SSA1                                     
384801     MOVE SPACE  TO GODK-STATUSKODER                                      
384901     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA4 SSA1                    
385001     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
385101     PERFORM IMS-STATUSKONTROLL                                           
385201     .                                                                    
385301     SKIP3                                                                
385401 IMS-GET-WLARTC11 SECTION.                                                
385501                                                                          
385601     MOVE 'WLARTC11 ' TO SSA1                                             
385701     MOVE SPACE  TO GODK-STATUSKODER                                      
385801     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA4 SSA1                    
385901     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
386001     PERFORM IMS-STATUSKONTROLL                                           
386101     .                                                                    
386201     SKIP2                                                                
386301 IMS-GU-WDK611   SECTION.                                                 
386401                                                                          
386501     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
386601          DELIMITED BY SIZE INTO SSA1                                     
386701     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
386801          DELIMITED BY SIZE INTO SSA2                                     
386901     MOVE '  ' TO GODK-STATUSKODER                                        
387001     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4 SSA1 SSA2                
387101     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
387201     PERFORM IMS-STATUSKONTROLL                                           
387301     .                                                                    
387401     SKIP2                                                                
387501 IMS-GHU-WDK611   SECTION.                                                
387601                                                                          
387701     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
387801          DELIMITED BY SIZE INTO SSA1                                     
387901     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
388001          DELIMITED BY SIZE INTO SSA2                                     
388101     MOVE '  ' TO GODK-STATUSKODER                                        
388201     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA4 SSA1 SSA2               
388301     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
388401     PERFORM IMS-STATUSKONTROLL                                           
388501     .                                                                    
388601     SKIP2                                                                
388701 IMS-REPL-WDK611 SECTION.                                                 
388801                                                                          
388901     MOVE '  ' TO GODK-STATUSKODER                                        
389001     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA4                        
389101                                                                          
389201     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
389301     PERFORM IMS-STATUSKONTROLL                                           
389401     .                                                                    
389501     SKIP2                                                                
389601 IMS-GU-WDB301 SECTION.                                                   
389701                                                                          
389801     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
389901                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
390001          DELIMITED BY SIZE INTO SSA1                                     
390101     MOVE '  GE' TO GODK-STATUSKODER                                      
390201     CALL CBLTDLI USING GHU KNDB-PCB DLI-IO-AREA3 SSA1                    
390301     MOVE KNDB-STATUS-CODE TO STATUS-WS                                   
390401     PERFORM IMS-STATUSKONTROLL                                           
390501     .                                                                    
390601     EJECT                                                                
390701 IMS-ISRT-WDL901 SECTION.                                                 
390801                                                                          
390901     MOVE 'WLLOGA01 ' TO SSA1                                             
391001     MOVE '  II' TO GODK-STATUSKODER                                      
391101     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
391201     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
391301     PERFORM IMS-STATUSKONTROLL                                           
391401     .                                                                    
391501     SKIP3                                                                
391601 IMS-ISRT-WLSAPA01 SECTION.                                               
391701     MOVE 'WLSAPA01 ' TO SSA1                                             
391801     MOVE '  II' TO GODK-STATUSKODER                                      
391901     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
392001     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
392101     PERFORM IMS-STATUSKONTROLL                                           
392201     .                                                                    
392301     SKIP3                                                                
392401 IMS-ISRT-WLFILC SECTION.                                                 
392501     STRING 'WLFILC01    '                                                
392601          DELIMITED BY SIZE INTO SSA1                                     
392701     MOVE '   ' TO GODK-STATUSKODER                                       
392801     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC SSA1               
392901     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
393001     PERFORM IMS-STATUSKONTROLL                                           
393101     .                                                                    
393201     EJECT                                                                
393301 IMS-ISRT-WLFILC2 SECTION.                                                
393401     STRING 'WLFILC01    '                                                
393501          DELIMITED BY SIZE INTO SSA1                                     
393601     MOVE '   ' TO GODK-STATUSKODER                                       
393701     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC2 SSA1              
393801     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
393901     PERFORM IMS-STATUSKONTROLL                                           
394001     .                                                                    
394101     SKIP3                                                                
394201 IMS-GU-WDB601    SECTION.                                                
394301     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
394401          DELIMITED BY SIZE INTO SSA1                                     
394501     MOVE '  ' TO GODK-STATUSKODER                                        
394601     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
394701     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
394801     PERFORM IMS-STATUSKONTROLL                                           
394901     .                                                                    
395001                                                                          
395101 IMS-GU-WDB601-SPAR SECTION.                                              
395201     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
395301          DELIMITED BY SIZE INTO SSA1                                     
395401     MOVE '  ' TO GODK-STATUSKODER                                        
395501     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SPAR SSA1            
395601     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
395701     PERFORM IMS-STATUSKONTROLL                                           
395801     .                                                                    
395901                                                                          
396001 IMS-GU-WDB601-SEND SECTION.                                              
396101     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
396201          DELIMITED BY SIZE INTO SSA1                                     
396301     MOVE '  ' TO GODK-STATUSKODER                                        
396401     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SEND SSA1            
396501     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
396601     PERFORM IMS-STATUSKONTROLL                                           
396701     .                                                                    
396801     EJECT                                                                
396901 IMS-GU-W6G130 SECTION.                                                   
397001                                                                          
397101     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
397201          DELIMITED BY SIZE INTO SSA1                                     
397301     STRING 'W6G130  (W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
397401          DELIMITED BY SIZE INTO SSA2                                     
397501     MOVE '  GE' TO GODK-STATUSKODER                                      
397601     CALL CBLTDLI USING GU W6G1-PCB DLI-IO-AREA-W6G130 SSA1 SSA2          
397701     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
397801     PERFORM IMS-STATUSKONTROLL                                           
397901     .                                                                    
398001     SKIP3                                                                
398101 IMS-STATUSKONTROLL SECTION.                                              
398201     SET STATUS-IX TO 1                                                   
398301     SEARCH GODK-STATUS                                                   
398401       AT END                                                             
398501         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
398601         DELIMITED BY SIZE INTO FELTEXT                                   
398701         CALL FELLOG                                                      
398801       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
398901         CONTINUE                                                         
399001     END-SEARCH                                                           
400000     .                                                                    
