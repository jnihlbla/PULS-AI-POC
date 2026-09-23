000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL010200.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   03/11/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:CARPARTS.LDC.WL0102                                             
000800*    WEB-LDC: WL010200 PROGRAM IS A REPLICA OF W6030200 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100* HÄR FINNS ÄNDRINGAR FÖR E-TRACKER 4780773                               
001200* OBS TA INTE BORT DETTA PROGRAM OBS                                      
001300*        LDC GODSMOTTAGNING                                               
001400*                                                                         
001500*        PROGRAMMET LÄSER      WLINLD (WDL6)                              
001600*                              WLGMTB (WDB3)                              
001700*                              WLARTC (WDK6)                              
001800*        PROGRAMMET UPPDATERAR         WDK7                               
001900*                              WLINLC (WDL6)                              
002000*                              WLFILB (WDR8)                              
002100*                              WLKOMA (WDP8)                              
002200*                              WL6301 (WDR5)                              
002300*                              WL6305 (WDR5)                              
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: WL0102U                                             
002700*                     WL0103X                                             
002800*        REQUEST:     WZ01REQ2                                            
002900*                     WL0102I1                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        RESPONSE:    WZ01RES2                                            
003300*                     WL0102O1                                            
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'WL010200'.            
004100 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300 77  WS-RESP-AREA                PIC S9(5)   VALUE ZERO COMP-3.           
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  YES                         PIC X       VALUE 'Y'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
004800 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
004900 77  WS-KVROS                    PIC S9(7)   VALUE ZERO COMP-3.           
005000 77  SW-REC-UPPD                 PIC X       VALUE SPACE.                 
005100 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
005200 77  W-KVANT-UPPD                PIC 9(3)    VALUE ZERO.                  
005300 77  SPAR-KDTRPSTA               PIC X(3)    VALUE SPACE.                 
005400 77  WS-BIN                      PIC X(3)    VALUE SPACE.                 
005500 77  WS-REC                      PIC X(3)    VALUE SPACE.                 
005600 77  INDX-REC                    PIC 9(3)    VALUE ZERO.                  
005700 77  INDX-BIN                    PIC 9(3)    VALUE ZERO.                  
005800 77  INDX-LINE                   PIC 9(3)    VALUE ZERO.                  
005900 77  WS-IDORDER                  PIC 9(7)  VALUE ZERO.                    
006000 77  WS-IDKOLLI                  PIC 9(5)  VALUE ZERO.                    
006100                                                                          
006200 77  SEND-WS-IDDC                PIC X(2)    VALUE SPACE.                 
006300 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
006400 77  WS-IDDC-SPAR                PIC X(2)    VALUE SPACE.                 
006500 77  DC-CROSS-FLAG               PIC X       VALUE 'N'.                   
006600                                                                          
006700 77  INDX-DISPLAY                PIC 999     VALUE ZERO.                  
006800 77  WS-KVANT                    PIC S9(5)   VALUE ZERO COMP-3.           
006900 77  WS-ADRESS                   PIC X(50)                                
007000       VALUE 'CARPARTS.LDC.DCUNLOADING'.                                  
007100 77  WS-ADRESS-WL0103            PIC X(50)                                
007200       VALUE 'CARPARTS.LDC.DCBINNING'.                                    
007300 77  WS-ADRESS-DP                PIC X(50)                                
007400       VALUE 'CARPARTS.DAP.DISTRDOC'.                                     
007500 77  WS-ADDRESS-WHSTOCKA         PIC X(50)                                
007600       VALUE 'CARPARTS.PULS.WHSTOCKADJ'.                                  
007700 77  WS-ADDRESS-MQASYNC          PIC X(50)                                
007800       VALUE 'CARPARTS.PULS.MQASYNC'.                                     
007900 77  SW-REQU-RAD-IFYLLD          PIC X.                                   
008000 77  WS-LOGG-AAAAMMDD            PIC 9(8)    VALUE ZERO.                  
008100 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
008200 77  WS-SAP-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
008300 77  WS-SAP-TTMMSSTH             PIC 9(8)    VALUE ZERO.                  
008400 77  W-TID                       PIC 9(8)    VALUE ZERO.                  
008500 77  NOLL-RAKNARE                PIC S9(5)   VALUE ZERO COMP-3.           
008600 77  LNG-P-TO-P-PREFIX           PIC S9(4)   VALUE +17  COMP SYNC.        
008700 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
008800 77  WS-INDX                     PIC S9(4)   VALUE +0   COMP SYNC.        
008900 77  BIN-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
009000 77  ORAD-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
009100 77  ORAD-IX-MAX                 PIC S9(4)  VALUE +5    COMP SYNC.        
009200 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
009300 77  WS-KDSORT                   PIC X(2)   VALUE SPACE.                  
009400 77  WS-IDUSER-003               PIC X(5)   VALUE SPACE.                  
009500 77  WS-ANTAL-PRINT              PIC S9(5)    VALUE ZERO.                 
009600 77  WS-SECTION                  PIC X(30)   VALUE 'MAIN'.                
009700 77  WS-IMS-SECTION              PIC X(8)    VALUE SPACE.                 
009800 77  WS-TOT-ART                  PIC S9(5)   VALUE ZERO COMP-3.           
009900 77  WS-SUM-VKART                PIC S9(11)  VALUE ZERO COMP-3.           
010000 77  WS-FLTRACK                  PIC X(1)    VALUE 'N'.                   
010100 77  WS-TOT-VKART                PIC S9(11)V9(2)                          
010200                                             VALUE ZERO COMP-3.           
010300 77  WS-SUM-VLARTNTO             PIC S9(11)  VALUE ZERO COMP-3.           
010400 77  WS-TOT-VLARTNTO             PIC S9(11)V9(2)                          
010500                                             VALUE ZERO COMP-3.           
010600 77  WS-VKART                    PIC S9(11)V9(2)                          
010700                                             VALUE ZERO COMP-3.           
010800 77  WS-VLARTNTO                 PIC S9(11)V9(2)                          
010900                                             VALUE ZERO COMP-3.           
011000 77  WS-PRAVCOST                 PIC S9(7)V9(2)                           
011100                                             VALUE ZERO COMP-3.           
011200 77  WS-PRARTSTD                 PIC S9(7)V9(2)                           
011300                                             VALUE ZERO COMP-3.           
011400 77  WS-PRARTNTO                 PIC S9(7)V9(2)                           
011500                                             VALUE ZERO COMP-3.           
011600 77  WS-DABERANK                 PIC 9(6)    VALUE ZERO.                  
011700 77  ETA-IDKUNDNR                PIC 9(7)    VALUE ZERO.                  
011800 77  ETA-IDKUNDNR-SORD           PIC 9(7)    VALUE ZERO.                  
011900 77  ETA-IDKUNDNR-SBPS           PIC 9(7)    VALUE ZERO.                  
012000                                                                          
012100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
012200     88  INDATA-OK                           VALUE 'J'.                   
012300     88  INDATA-FEL                          VALUE 'N'.                   
012400 77  KEYS-SW                   PIC X      VALUE 'J'.                      
012500     88  KEYS-OK                          VALUE 'J'.                      
012600     88  KEYS-WRONG                       VALUE 'N'.                      
012700                                                                          
012800 77  DCA-RECEIVED              PIC X      VALUE 'J'.                      
012900     88  DCA-RECEIVED-OK                  VALUE 'J'.                      
013000     88  DCA-NOT-RECEIVED                 VALUE 'N'.                      
013100                                                                          
013200 77  IDTRACK-SW                  PIC X     VALUE 'J'.                     
013300     88  IDTRACK-OK                        VALUE 'J'.                     
013400     88  IDTRACK-FEL                       VALUE 'N'.                     
013500                                                                          
013600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013700     88  NYCKLAR-OK                          VALUE 'J'.                   
013800     88  NYCKLAR-FEL                         VALUE 'N'.                   
013900                                                                          
014000 77  IDUSER-SW                   PIC X       VALUE 'J'.                   
014100     88  IDUSER-OK                           VALUE 'J'.                   
014200     88  IDUSER-FEL                          VALUE 'N'.                   
014300                                                                          
014400 77  WS-PROD                     PIC X       VALUE 'N'.                   
014500     88 PRODKOD-SAKNAS                       VALUE 'N'.                   
014600     88 PRODKOD-FINNS                        VALUE 'J'.                   
014700                                                                          
014800 77  HOPP-UPDATE-SW              PIC X       VALUE 'N'.                   
014900     88  HOPP-UPDATE                         VALUE 'J'.                   
015000     88  EJ-HOPP-UPDATE                      VALUE 'N'.                   
015100                                                                          
015200 77  PRINT-SW                    PIC X       VALUE 'N'.                   
015300     88  PRINT                               VALUE 'J'.                   
015400     88  EJ-PRINT                            VALUE 'N'.                   
015500                                                                          
015600 77  TRANS-OHUVUD-RET-SKAPAD-SW  PIC X       VALUE 'N'.                   
015700     88  TRANS-OHUVUD-RET-SKAPAD             VALUE 'J'.                   
015800                                                                          
015900 77  ATERHOPP-SW                 PIC X       VALUE 'N'.                   
016000     88  ATERHOPP                            VALUE 'J'.                   
016100     88  EJ-ATERHOPP                         VALUE 'N'.                   
016200                                                                          
016300 77  INLEV-SW                    PIC X       VALUE 'J'.                   
016400     88  INLEV-JA                            VALUE 'J'.                   
016500     88  INLEV-NEJ                           VALUE 'N'.                   
016600                                                                          
016700 77  AKTUELLT-LAND               PIC X       VALUE 'N'.                   
016800     88  AKTUELLT-LAND-USA                   VALUE 'J'.                   
016900     88  AKTUELLT-EJ-USA                     VALUE 'N'.                   
017000                                                                          
017100 77  AKTUELLT-LAND-OTHER         PIC X       VALUE 'N'.                   
017200     88  AKTUELLT-LAND-NA                    VALUE 'J'.                   
017300     88  AKTUELLT-EJ-NA                      VALUE 'N'.                   
017400                                                                          
017500 01  WS-KDMATT                   PIC X.                                   
017600     88 US-MEASUREMENT           VALUE 'U'.                               
017700     88 SIS-MEASUREMENT          VALUE 'S'.                               
017800                                                                          
017900 77  WS-WRITE-IDTRACK            PIC X       VALUE 'N'.                   
018000     88  WRITE-IDTRACK                       VALUE 'J'.                   
018100     88  NOT-WRITE-IDTRACK                   VALUE 'N'.                   
018200                                                                          
018300 77  ETA-UPD-SW                  PIC X       VALUE 'J'.                   
018400     88  ETA-UPD-OK                          VALUE 'J'.                   
018500     88  ETA-UPD-NOT-OK                      VALUE 'N'.                   
018600                                                                          
018700 01  WS-IDKUNDRF-GRP.                                                     
018800     03  WS-IDKUNDRF             PIC X(10).                               
018900     03  FILLER REDEFINES WS-IDKUNDRF.                                    
019000         05  WS-IDORDNR5         PIC 9(5).                                
019100         05  WS-IDORDNR5-FILLER  PIC X(5).                                
019200     03  FILLER REDEFINES WS-IDKUNDRF.                                    
019300         05  WS-IDORDNR7         PIC 9(7).                                
019400         05  FILLER              PIC X(3).                                
019500 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
019600 77  W-IDARTNR-EDIT-X            PIC Z(9).                                
019700 77  FIRST-REC-TRANS-SW          PIC X       VALUE 'N'.                   
019800     88  NOT-FIRST-REC-TRANS                 VALUE 'N'.                   
019900     88  FIRST-REC-TRANS                     VALUE 'J'.                   
020000                                                                          
020100 01  WS-IDKUNDRF-IDORDNR5        PIC 9(5).                                
020200                                                                          
020300 01  TEST-IDDISTR                PIC S9(5) COMP-3.                        
020400*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
020500     EJECT                                                                
020600                                                                          
020700 01  W.                                                                   
020800     05  W-KVANT-UPD             PIC S9(3).                               
020900     05  W-DATUM-X.                                                       
021000         10  W-DATUM             PIC 9(6).                                
021100     05  W-DATUM-Y.                                                       
021200         10  W-DATUM-LOCAL       PIC 9(6).                                
021300     05  W-TIME-X.                                                        
021400         10  W-TIME-TT           PIC 9(2).                                
021500         10  FILLER              PIC 9(2).                                
021600         10  W-TIME-SS           PIC 9(2).                                
021700         10  FILLER              PIC 9(2).                                
021800     05  W-TIME-N REDEFINES W-TIME-X PIC 9(8).                            
021900                                                                          
022000     03  AKTUELL-TID.                                                     
022100         05  AKTUELL-TTMM        PIC 9(4).                                
022200         05  FILLER              PIC 9(4).                                
022300                                                                          
022400     03  AKTUELL-TID-X.                                                   
022500         05  AKTUELL-TTMM-LOC    PIC 9(4).                                
022600         05  FILLER              PIC 9(4).                                
022700                                                                          
022800     03  WS-IDDC-KOLL.                                                    
022900         05  FILLER              PIC X(5)     VALUE 'WIDDC'.              
023000         05  WS-IDDC-TID         PIC X(2)     VALUE SPACE.                
023100         05  FILLER              PIC X        VALUE SPACE.                
023200                                                                          
023300     05  W-IDORDNR-X.                                                     
023400         10  FILLER              PIC 9(2)     VALUE ZERO.                 
023500         10  W-IDORDNR-VV        PIC 9(2).                                
023600         10  W-IDORDNR-D         PIC 9(1).                                
023700         10  W-IDORDNR-SS        PIC 9(2).                                
023800                                                                          
023900     05  W-KVAVIS-6-X.                                                    
024000         10  W-KVAVIS-6          PIC 9(6).                                
024100                                                                          
024200     05  W-ANTAL-LASN            PIC S9(7) COMP-3.                        
024300     05  W-KVAVIS                PIC S9(7) COMP-3.                        
024400     05  W-KVNYART               PIC S9(7) COMP-3.                        
024500     05  W-KVRADER               PIC S9(7) COMP-3.                        
024600     05  WS-RAKNARE              PIC S9(7) COMP-3.                        
024700     05  W-KVPRIOART             PIC S9(7) COMP-3.                        
024800     05  W-CMD                   PIC X(3).                                
024900     05  W-SPAR-IDKUNDRF         PIC X(10).                               
025000     05  W-SPAR-IDKUNDNR         PIC S9(7) COMP-3.                        
025100     05  W-SPAR-IDKOLLI          PIC S9(5) COMP-3.                        
025200     05  WS-SAP-IDDISTR          PIC 9(5)  VALUE ZERO.                    
025300     05  WS-SAP-IDKUNDNR         PIC 9(7)  VALUE ZERO.                    
025400     05  WS-SAP-IDFAKT           PIC 9(7)  VALUE ZERO.                    
025500     05  WS-SAP-X-IDFAKT         PIC X(7)  VALUE ZERO.                    
025600     05  W-IDKUNDNR-RETUR        PIC 9(6).                                
025700     05  W-IDDISTR-RETUR         PIC 9(4).                                
025800     05  W-IDKUNDNR-REFILL       PIC 9(6).                                
025900     05  W-IDDISTR-REFILL        PIC 9(4).                                
026000     05  W-IDSEKVNR-SAP          PIC S9(3) VALUE 0   COMP-3.              
026100     05  W-IDSEKVNR-A03      PIC S9(3)  VALUE 0   COMP-3.                 
026200     05  W-TIKLOCK               PIC S9(9) VALUE 0   COMP-3.              
026300     05  W-KDFRAKT               PIC S9(3) VALUE ZERO COMP-3.             
026400     05  WS-KVTILLGANG           PIC S9(7)V9(1) VALUE ZERO.               
026500     05  WS-KVBEHOV              PIC S9(7)V9(1) VALUE ZERO.               
026600     05  WS-FAKT-INFO-DLET       PIC X          VALUE SPACE.              
026700     05  WS-IDDC-SEND            PIC X(2)       VALUE SPACE.              
026800     05  WS-A03-SKAPAD           PIC X(2)       VALUE SPACE.              
026900     05  WS-KVBINART             PIC 9(3)       VALUE ZERO.               
027000     05  WS-KVBINART-MAX         PIC S9(3)      VALUE +25 COMP-3.         
027100     05  WS-BAATORDER            PIC X          VALUE SPACE.              
027200     05  WS-FLYGORDER            PIC X          VALUE SPACE.              
027300     05  SPAR-FLINLREP           PIC X          VALUE SPACE.              
027400     05  WS-MARKUP               PIC 9V9(2)     VALUE ZERO.               
027500     05  WS-CDC-11               PIC X(2)       VALUE '11'.               
027600                                                                          
027700 01  WS-URVAL.                                                            
027800     07  URV-CMD                 PIC X(3)  VALUE SPACE.                   
027900     07  URV-IDDC                PIC X(2)  VALUE SPACE.                   
028000     07  URV-IDDISTR             PIC 9(5)  VALUE ZERO.                    
028100     07  URV-IDFAKT              PIC 9(7)  VALUE ZERO.                    
028200     07  URV-IDKOLLI             PIC 9(5)  VALUE ZERO.                    
028300     07  URV-IDKUNDRF            PIC X(10) VALUE SPACE.                   
028400     07  URV-IDKUNDNR            PIC 9(7)  VALUE ZERO.                    
028500     07  URV-KDMATT              PIC X     VALUE SPACE.                   
028600     07  URV-IDUSER              PIC X(8)  VALUE SPACE.                   
028700                                                                          
028800 77  TAB-IX                      PIC S9(5)   VALUE ZERO COMP-3.           
028900 77  TAB-IX-MAX                  PIC S9(5)   VALUE 1100 COMP-3.           
029000 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
029100 77  MAX-IX                      PIC S9(3)   VALUE +7   COMP-3.           
029200 01  TABENTRY-PARM.                                                       
029300     03  STEGLAANGD              PIC S9(9) COMP.                          
029400     03  ANTAL                   PIC S9(9) COMP.                          
029500     03  NYCKELLAANGD            PIC S9(9) COMP.                          
029600 01  SORT-TABELL.                                                         
029700     03  TAB-RAD OCCURS 1100.                                             
029800        05  TAB-SORT1-BEGREPP.                                            
029900            07  TAB-IDARTNR     PIC S9(9) COMP-3.                         
030000        05  TAB-SORT2-BEGREPP.                                            
030100            07  TAB-ADLAGOMR    PIC S9(3) COMP-3.                         
030200            07  TAB-ADGANG      PIC S9(3) COMP-3.                         
030300            07  TAB-ADPLATS     PIC S9(5) COMP-3.                         
030400        05  TAB-BEART           PIC X(100).                               
030500        05  TAB-FLKDFARLIG      PIC X.                                    
030600        05  TAB-FLBACKORDER     PIC X.                                    
030700        05  TAB-FLPRIO          PIC X.                                    
030800        05  TAB-KDARTURS        PIC X(2).                                 
030900        05  TAB-KVAVIS          PIC S9(7) COMP-3.                         
031000                                                                          
031100 01  KONTROLL-SIFFRA.                                                     
031200     03  REK-IDARTNR             PIC 9(9)       VALUE 0.                  
031300     03  REK-LNGD                PIC 9(1)       VALUE 9.                  
031400     03  REK-REKSIFFR            PIC 9(1)       VALUE 0.                  
031500                                                                          
031600 01  WS-DAREGDAT                 PIC 9(8)    VALUE ZERO.                  
031700 01  FILLER REDEFINES WS-DAREGDAT.                                        
031800     03  WS-SEKEL-D              PIC 9(2).                                
031900     03  WS-AAMMDD               PIC 9(6).                                
032000                                                                          
032100 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
032200 01  FILLER REDEFINES DAGENS-DATUM.                                       
032300     03  DAGENS-DATUM-SEKEL      PIC 9(2).                                
032400     03  DAGENS-DATUM-AAMMDD     PIC 9(6).                                
032500                                                                          
032600 01  WS-SEKEL-KOLL               PIC 9(6).                                
032700 01  FILLER REDEFINES WS-SEKEL-KOLL.                                      
032800     03  WS-SEKEL                PIC 9(1).                                
032900     03  FILLER                  PIC 9(5).                                
033000                                                                          
033100 01  WS-SEKEL-EKOA03.                                                     
033200     03  WS-EKOA03-SS            PIC 9(2).                                
033300     03  WS-EKOA03-AAMMDD        PIC 9(6).                                
033400 01  WS-AAAAMMDD REDEFINES WS-SEKEL-EKOA03 PIC 9(8).                      
033500                                                                          
033600 01  WS-SEKEL-TEST               PIC 9(6).                                
033700 01  FILLER REDEFINES WS-SEKEL-TEST.                                      
033800     03  WS-SEKEL2               PIC 9(1).                                
033900     03  FILLER                  PIC 9(5).                                
034000                                                                          
034100 01  WS-SEKEL-AVVIK.                                                      
034200     03  WS-AVVIK-SS             PIC 9(2).                                
034300     03  WS-AVVIK-AAMMDD         PIC 9(6).                                
034400 01  WS-AVVIK-AAAAMMDD REDEFINES WS-SEKEL-AVVIK PIC 9(8).                 
034500                                                                          
034600*    --- SPAR-AREA FÖR LEVERANS ANMÄRKNING                                
034700 01  SPAR-AREA-6306.                                                      
034800*    03 -COPY WDGX6306              -PRE SPAR-                            
034900 01  SPAR-AREA-6308.                                                      
035000*    03 -COPY WDGX6308              -PRE SPAR-                            
035100     EJECT                                                                
035200*    --- DISTRIKT OMVANDLINGS TABELL                                      
035300*01 -COPY WWDIST35                                                        
035400     EJECT                                                                
035500*    --- REMARKUP FAKTOR                                                  
035600*01 -COPY WWMARKUP                                                        
035700     EJECT                                                                
035800*    --- EKONOMITRANS                                                     
035900*01 -COPY W510A03               -PRE EKOTRA03-                            
036000     EJECT                                                                
036100*    --- AVVIKELSE TRANS                                                  
036200*01 -COPY W61244                -PRE FILC-                                
036300     EJECT                                                                
036400*01 -COPY W61247                -PRE FILC2-                               
036500     EJECT                                                                
036600*    --- PARAMETRAR TILL SUBPROGRAM W612LABL                              
036700*01    -COPY W612LABL PRE LABL-                                           
036800       EJECT                                                              
036900*    --- PARAMETRAR TILL SUBPROGRAM W612KLBL                              
037000*01    -COPY W612KLBL PRE KLBL-                                           
037100       EJECT                                                              
037200                                                                          
037300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
037400 01  GENERELLA-SUBPROGRAM.                                                
037500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
037600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
037700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
037800     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
037900     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
038000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
038100     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
038200     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
038300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
038400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
038500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
038600     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
038700     03  W612LABL                PIC X(8)    VALUE 'W612LABL'.            
038800     03  W612KLBL                PIC X(8)    VALUE 'W612KLBL'.            
038900     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
039000     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
039100     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
039200     03  W602CROS                PIC X(8)    VALUE 'W602CROS'.            
039300     EJECT                                                                
039400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
039500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
039600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
039700     EJECT                                                                
039800 01  MESSAGE-CODES.                                                       
039900     05  NO-DATA-ENTERED         PIC X(3)   VALUE '014'.                  
040000     03  ERR-UNAUTHORIZED        PIC X(3)   VALUE '00A'.                  
040100     03  UPDATE-DONE             PIC X(3)   VALUE '001'.                  
040200     03  INVALID-KEY-FIELDS      PIC X(3)   VALUE '022'.                  
040300     03  IS-INVALID              PIC X(3)   VALUE '023'.                  
040400     03  TOO-MANY-LINES          PIC X(3)   VALUE '028'.                  
040500     03  SYSTEM-ERROR            PIC X(3)   VALUE '099'.                  
040600     03  KEYS-ARE-MISSING        PIC X(3)   VALUE '041'.                  
040700     03  PRINTING-REQUESTED      PIC X(3)   VALUE '015'.                  
040800     03  LINES-NOT-FOUND         PIC X(3)   VALUE '027'.                  
040900     03  MUST-ENTER-EMP-ID       PIC X(3)   VALUE '026'.                  
041000     03  TRACKING-ID-MISSING     PIC X(3)   VALUE '422'.                  
041100     03  CASE-NOT-FOUND          PIC X(3)   VALUE '287'.                  
041200     03  RECEIVED-AT-WRONGDC-SENDTO PIC X(3)   VALUE '427'.               
041300     03  UPDATE-DONE-NEXT-DCCROS PIC X(3)   VALUE '428'.                  
041400     03  UPDATE-DONE-NEXT-TRPTNR PIC X(3)   VALUE '429'.                  
041500     EJECT                                                                
041600*      --- VALID IDDC CODES                                               
041700*                                                                         
041800*01  -COPY WWDC99                                                         
041900     EJECT                                                                
042000 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
042100     SKIP3                                                                
042200 01  DAT-IO-AREA.                                                         
042300*    03  -COPY WDATAREA                                                   
042400     EJECT                                                                
042500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
042600     SKIP3                                                                
042700*01  -COPY WZ01SUB                                                        
042800     EJECT                                                                
042900 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
043000*01  -COPY WMSGCONV                                                       
043100     EJECT                                                                
043200 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
043300*01  -COPY WZ01AUTH                                                       
043400     EJECT                                                                
043500 01  FILLER                      PIC X(16)   VALUE 'W218ETA '.            
043600*01 -COPY W218LETA              -PRE LETA-                                
043700     EJECT                                                                
043800 01  FILLER                      PIC X(16)   VALUE 'W602CROS'.            
043900*01  -COPY W602CROS                                                       
044000     EJECT                                                                
044100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
044200 01  REQU-AREA.                                                           
044300*    03  -COPY WZ01REQ2                                                   
044400*    03  -COPY WL0102I1                                                   
044500     03  REQU-TAB.                                                        
044600         05  REQU-FLATERHOPP     PIC X(1).                                
044700         05  REQU-KDCMD          PIC X(1).                                
044800         05  REQU-LOS-RAD OCCURS 500.                                     
044900             07 REQU-FLLOS       PIC X(1).                                
045000                                                                          
045100     EJECT                                                                
045200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
045300     SKIP3                                                                
045400 01  RESP-AREA.                                                           
045500*    03  -COPY WZ01RES2                                                   
045600*    03  -COPY WL0102O1                                                   
045700     EJECT                                                                
045800 01  FILLER                      PIC X(16)   VALUE 'MSG PROP'.            
045900*01  -COPY WZ04PROP                                                       
046000     EJECT                                                                
046100*    NOTAFISCAL                                                           
046200 01  NOTF-AREA.                                                           
046300*    03  -COPY W611NOTF                                                   
046400     EJECT                                                                
046500 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
046600 01  -COPY WZ01SEND                                                       
046700     EJECT                                                                
046800 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
046900 01  SEND-AREA.                                                           
047000*    03  -COPY WZ01REQ2 -PRE SEND-                                        
047100*    03  -COPY WL0102I1 -PRE SEND-                                        
047200     03  SEND-REQU-TAB.                                                   
047300         05 SEND-REQU-FLATERHOPP PIC X(1).                                
047400         05 SEND-REQU-KDCMD      PIC X(1).                                
047500         05 SEND-REQU-LOS-RAD OCCURS 500.                                 
047600             07 SEND-REQU-FLLOS  PIC X(1).                                
047700     EJECT                                                                
047800 01  FILLER                      PIC X(16) VALUE 'WL01TIDZ-AREA'.         
047900*01  -COPY WL01TIDZ                                                       
048000 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
048100*01  -COPY WTRAUTF8                                                       
048200 01  FILLER                      PIC X(16)   VALUE 'WWOMVAND '.           
048300*   -COPY WWOMVAND                                                        
048400     SKIP3                                                                
048500                                                                          
048600 77  WS-IDSKYLT-CHINESE          PIC X(3)  VALUE 'RCN'.                   
048700 77  WS-IDSKYLT-ENGLISH          PIC X(3)  VALUE 'GB '.                   
048800     EJECT                                                                
048900 01  UT-AREA-START               PIC X(24)   VALUE                        
049000                                 'UT-AREA-START '.                        
049100 01  HDR-AREA.                                                            
049200*    03  -COPY WZ01REQU  -PRE HDR-                                        
049300*    03  -COPY WZ04HDR                                                    
049400     EJECT                                                                
049500 01  DOC-HEAD-AREA.                                                       
049600*    03  -COPY WL10041   -PRE UT1-                                        
049700     EJECT                                                                
049800 01  DOC-LINE-AREA.                                                       
049900*    03  -COPY WL10042   -PRE UT2-                                        
050000     EJECT                                                                
050100 01  DOC-TOT-AREA.                                                        
050200*    03  -COPY WL10043   -PRE UT3-                                        
050300 01  DOC-LINE2-AREA.                                                      
050400*    03  -COPY WL10044   -PRE UT4-                                        
050500     EJECT                                                                
050600 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
050700*01  -COPY WMSGKOM                                                        
050800     EJECT                                                                
050900 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
051000 01  KOM-IO-AREA.                                                         
051100   03  KOM-AREA                  PIC X(2457) VALUE SPACE.                 
051200   03  OHUV     REDEFINES KOM-AREA.                                       
051300*    05      -COPY W4I25101   -PRE OHUV-                                  
051400     EJECT                                                                
051500   03  ORAD     REDEFINES KOM-AREA.                                       
051600*    05      -COPY W4I25201   -PRE ORAD-                                  
051700     EJECT                                                                
051800 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
051900 01  P-TO-P-AREA.                                                         
052000     03  P-TO-P-LL               PIC S9(4)            COMP SYNC.          
052100     03  P-TO-P-Z1               PIC  X(1)   VALUE LOW-VALUE.             
052200     03  P-TO-P-Z2               PIC  X(1)   VALUE LOW-VALUE.             
052300     03  P-TO-P-TRANSKOD         PIC  X(7).                               
052400     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
052500     03  P-TO-P-FROM-MID         PIC  X(4).                               
052600     03  P-TO-P-KDMFSFOR         PIC  X(1).                               
052700     03  P-TO-P-DATA             PIC  X(1000).                            
052800     EJECT                                                                
052900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
053000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
053100 01  NYCKLAR-TILL-DLI.                                                    
053200     03  W-IDARTNR-X.                                                     
053300         05  W-IDARTNR           PIC S9(9) COMP-3.                        
053400     03  W-IDSKYLT-X.                                                     
053500         05  W-IDSKYLT           PIC X(3)   VALUE 'GB '.                  
053600     03  W-IDLAND-X.                                                      
053700         05  W-IDLAND            PIC X(2)   VALUE SPACE.                  
053800                                                                          
053900     03  W-TIREGDAT-X.                                                    
054000         05  W-TIREGDAT          PIC S9(7) COMP-3.                        
054100                                                                          
054200     03  W-DAINLEV-X.                                                     
054300         05  W-DAINLEV           PIC 9(16).                               
054400                                                                          
054500     03  W-IDDISTR-X.                                                     
054600         05  W-IDDISTR           PIC S9(5) COMP-3.                        
054700                                                                          
054800     03  W-IDDC                  PIC X(2).                                
054900     03  W-IDDC-B6-X.                                                     
055000         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
055100     03  W-IDPTYP                PIC X(3).                                
055200     03  W-IDKUNDRF-IDORDNR.                                              
055300         10  W-IDORDNR           PIC 9(5).                                
055400         10  FILLER              PIC X(5).                                
055500                                                                          
055600     03  W-WDL6ASEQ-MIN.                                                  
055700         05  W-SEQA-IDFAKT-MIN    PIC S9(7) COMP-3.                       
055800         05  W-SEQA-IDKUNDRF-MIN  PIC X(10).                              
055900         05  W-SEQA-IDKUNDNR-MIN  PIC S9(7) COMP-3.                       
056000         05  W-SEQA-IDKOLLI-MIN   PIC S9(5) COMP-3.                       
056100                                                                          
056200     03  W-WDL6ASEQ-MAX.                                                  
056300         05  W-SEQA-IDFAKT-MAX    PIC S9(7) COMP-3.                       
056400         05  W-SEQA-IDKUNDRF-MAX  PIC X(10).                              
056500         05  W-SEQA-IDKUNDNR-MAX  PIC S9(7) COMP-3.                       
056600         05  W-SEQA-IDKOLLI-MAX   PIC S9(5) COMP-3.                       
056700                                                                          
056800     03  W-WDL6A1KY-MIN.                                                  
056900         05  W-IDFAKT-MIN         PIC S9(7) COMP-3.                       
057000         05  W-IDKUNDRF-MIN       PIC X(10).                              
057100         05  W-IDKUNDNR-MIN       PIC S9(7) COMP-3.                       
057200         05  W-IDKOLLI-MIN        PIC S9(5) COMP-3.                       
057300         05  FILLER               PIC X(21).                              
057400                                                                          
057500     03  W-WDL6A1KY-MAX.                                                  
057600         05  W-IDFAKT-MAX         PIC S9(7) COMP-3.                       
057700         05  W-IDKUNDRF-MAX       PIC  X(10).                             
057800         05  W-IDKUNDNR-MAX       PIC S9(7) COMP-3.                       
057900         05  W-IDKOLLI-MAX        PIC S9(5) COMP-3.                       
058000         05  FILLER               PIC X(21).                              
058101                                                                          
058201     03  W-WDL6CSEQ-MIN.                                                  
058301         05  W-SEQC-IDFAKT-MIN    PIC S9(7) COMP-3.                       
058401         05  W-SEQC-IDDC-MIN      PIC X(2).                               
058701                                                                          
058801     03  W-WDL6CSEQ-MAX.                                                  
058901         05  W-SEQC-IDFAKT-MAX    PIC S9(7) COMP-3.                       
059001         05  W-SEQC-IDDC-MAX      PIC X(2).                               
060301                                                                          
060401     03  W-IDFAKT-X.                                                      
060501         05  W-IDFAKT            PIC S9(7)  VALUE ZERO COMP-3.            
060601                                                                          
060701     03  W-IDKUNDRF              PIC X(10)  VALUE SPACE.                  
060801                                                                          
060901     03  W-IDKUNDNR-X.                                                    
061001         05  W-IDKUNDNR          PIC S9(7)  VALUE ZERO COMP-3.            
061101                                                                          
061201     03  W-IDKOLLI-X.                                                     
061301         05  W-IDKOLLI           PIC S9(5)  VALUE ZERO COMP-3.            
061401                                                                          
061501     03  W-IDLBBET               PIC X(12)  VALUE SPACE.                  
061601                                                                          
061701     03  W-6301KEY-X.                                                     
061801         05  W-6301-IDHTYP      PIC X(4)    VALUE '6301'.                 
061901         05  W-6301-IDDC        PIC X(2).                                 
062001         05  FILLER             PIC X(24)   VALUE LOW-VALUE.              
062101                                                                          
062201     03  W-6305KEY-X.                                                     
062301         05  W-6305-IDHTYP      PIC X(4)    VALUE '6305'.                 
062401         05  FILLER             PIC X(2)    VALUE LOW-VALUE.              
062501         05  FILLER             PIC X(24)   VALUE LOW-VALUE.              
062601                                                                          
062701     03  W-WDB301KY-X.                                                    
062801         05  W-IDDC-WDB3         PIC X(2)   VALUE SPACE.                  
062901         05  W-IDDISTR-WDB3      PIC S9(5)  VALUE ZERO COMP-3.            
063001         05  W-IDKUNDNR-WDB3     PIC S9(7)  VALUE ZERO COMP-3.            
063101                                                                          
063201     03  W-WDB301KY-DEF-X.                                                
063301         05  W-IDDC-WDB3-DEF     PIC X(2)   VALUE SPACE.                  
063401         05  W-IDDISTR-WDB3-DEF  PIC S9(5)  VALUE ZERO COMP-3.            
063501         05  W-IDKUNDNR-WDB3-DEF PIC S9(7) VALUE +9999999 COMP-3.         
063601     03  W-W6D211KY-X.                                                    
063701         05  W-DAREGDAT-9KOMPL   PIC 9(8)   VALUE 79000001.               
063801         05  W-TIKLOCK-9KOMPL    PIC S9(9)  VALUE ZERO COMP-3.            
063901                                                                          
064001     03  W-WDL6D1KY-MIN.                                                  
064101         05  W-SEQD-IDDC-MIN     PIC X(2).                                
064201         05  W-SEQD-IDDISTR-MIN  PIC S9(5) COMP-3.                        
064301         05  W-SEQD-IDKUNDNR-MIN PIC S9(7) COMP-3.                        
064401         05  W-SEQD-IDKUNDRF-MIN PIC X(10).                               
064501         05  W-SEQD-IDKOLLI-MIN  PIC S9(5) COMP-3.                        
064601         05  W-SEQD-IDARTNR-MIN  PIC S9(9) COMP-3.                        
064701         05  W-SEQD-DAINLEV-MIN  PIC 9(16).                               
064801                                                                          
064901     03  W-WDL6D1KY-MAX.                                                  
065001         05  W-SEQD-IDDC-MAX     PIC X(2).                                
065101         05  W-SEQD-IDDISTR-MAX  PIC S9(5) COMP-3.                        
065201         05  W-SEQD-IDKUNDNR-MAX PIC S9(7) COMP-3.                        
065301         05  W-SEQD-IDKUNDRF-MAX PIC X(10).                               
065401         05  W-SEQD-IDKOLLI-MAX  PIC S9(5) COMP-3.                        
065501         05  W-SEQD-IDARTNR-MAX  PIC S9(9) COMP-3.                        
065601         05  W-SEQD-DAINLEV-MAX  PIC 9(16).                               
065701                                                                          
065801*--------W6G1                                                             
065901     03  W-W6GXKEY-6005-X.                                                
066001         05  W-IDHTYP-6005       PIC X(4)    VALUE '6005'.                
066101         05  W-IDDC-6005         PIC X(2)    VALUE '11'.                  
066201         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
066301                                                                          
066401     03  W-W6GXKEY-6006-X.                                                
066501         05  W-ADINLOMR-6006     PIC X(4)    VALUE SPACE.                 
066601         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
066701                                                                          
066801     EJECT                                                                
066901*    --- STATUS-KOD FRÅN IMS                                              
067001 01  STATUS-WS                   PIC XX.                                  
067101     88  SEGMENT-FINNS                       VALUE '  '.                  
067201     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
067301     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
067401     88  BASEN-SLUT                          VALUE 'GB'.                  
067501     SKIP2                                                                
067601 01  GODK-STATUSKODER.                                                    
067701     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
067801     SKIP3                                                                
067901 01  SSA1                        PIC X(160).                              
068001 01  SSA2                        PIC X(128).                              
068101 01  SSA3                        PIC X(128).                              
068201     EJECT                                                                
068301*    --- IMS FUNKTIONSKODER                                               
068401*01  -COPY W0003                                                          
068501     EJECT                                                                
068601*    ---  DLI INPUT-OUTPUT AREA                                           
068701 01  FILLER         PIC X(24) VALUE 'DLI-IO- WDK711'.                     
068801 01  DLI-IO-WDK711.                                                       
068901*    03  -COPY WDK711                                                     
069001     EJECT                                                                
069101 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
069201 01  DLI-IO-WDK712.                                                       
069301*    03  -COPY WDK712                                                     
069401     EJECT                                                                
069501 01  FILLER         PIC X(24) VALUE 'DLI-IO-W6KVAH'.                      
069601 01  DLI-IO-W6KVAH.                                                       
069701*    03  -COPY W6D211                                                     
069801     EJECT                                                                
069901 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
070001     SKIP3                                                                
070101 01  DLI-IO-AREA1.                                                        
070201     03  IO-AREA1                PIC X(300)  VALUE SPACE.                 
070301     03  WLINLD01 REDEFINES IO-AREA1.                                     
070401*        05  -COPY WDL6A1                                                 
070501 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
070601     SKIP3                                                                
070701 01  DLI-IO-AREA3.                                                        
070801     03  IO-AREA3                PIC X(300)  VALUE SPACE.                 
070901     03  WLGMTB01 REDEFINES IO-AREA3.                                     
071001*        05  -COPY WDB301                                                 
071101     EJECT                                                                
071201 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC'.                      
071301 01  DLI-IO-WLARTC01.                                                     
071401*    03  -COPY WDK601                                                     
071501     EJECT                                                                
071601 01  DLI-IO-WLARTC11.                                                     
071701*    03  -COPY WDK611                                                     
071801     EJECT                                                                
071901 01  DLI-IO-AREA5.                                                        
072001     03  IO-AREA5                PIC X(550)  VALUE SPACE.                 
072101     03  WL630511 REDEFINES IO-AREA5.                                     
072201*        05  -COPY WDGX6306                                               
072301     EJECT                                                                
072401     03  WL630521 REDEFINES IO-AREA5.                                     
072501*        05  -COPY WDGX6308                                               
072601     EJECT                                                                
072701 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WLLOGA01'.          
072801 01  DLI-IO-WLLOGA01.                                                     
072901*    03  WLLOGA01  -COPY WDL901                                           
073001     EJECT                                                                
073101                                                                          
073201 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-HELPAREA'.          
073301 01  DLI-IO-W510WKHA.                                                     
073401*    07  -COPY W510EKHA                                                   
073501                                                                          
073601                                                                          
073701 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WLSAPA01'.          
073801 01  DLI-IO-WLSAPA01.                                                     
073901*    03  WLSAPA01  -COPY WDR901                                           
074001*    07  -COPY W510EKHA  -PRE R9- -RED FIL-WDR901-DATA                    
074101     EJECT                                                                
074201 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WLFILB01'.          
074301 01  DLI-IO-WLFILB01.                                                     
074401*    03  WLFILB01  -COPY WDR801                                           
074501*    07  -COPY W510EKHA  -PRE R8- -RED FIL-WDR801-DATA                    
074601     EJECT                                                                
074701 01  DLI-IO-AREA-FILC.                                                    
074801     03  IO-AREA-FILC        PIC X(300)  VALUE SPACE.                     
074901     SKIP3                                                                
075001     03  WLFILC01 REDEFINES IO-AREA-FILC.                                 
075101*        05  -COPY WDR301       -PRE FILC-                                
075201     EJECT                                                                
075301 01  DLI-IO-AREA-FILC2.                                                   
075401     03  IO-AREA-FILC2       PIC X(300)  VALUE SPACE.                     
075501     SKIP3                                                                
075601     03  WLFILC01 REDEFINES IO-AREA-FILC2.                                
075701*        05  -COPY WDR301       -PRE FILC2-                               
075801     EJECT                                                                
075901 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
076001 01   DLI-IO-AREA-B601.                                                   
076101*     03  -COPY WDB601                                                    
076201 01  FILLER               PIC X(16)   VALUE 'WDB601 SPAR'.                
076301 01   DLI-IO-AREA-B601-SPAR.                                              
076401*     03  -COPY WDB601 -PRE SPAR-                                         
076501     EJECT                                                                
076601 01  FILLER               PIC X(16)   VALUE 'WDB601 SEND'.                
076701 01   DLI-IO-AREA-B601-SEND.                                              
076801*     03  -COPY WDB601 -PRE SEND-                                         
076901     EJECT                                                                
077001 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLGX63'.                      
077101 01  DLI-IO-WLGX63.                                                       
077201*    03  -COPY WDGX6302                                                   
077301     EJECT                                                                
077401 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC11'.                    
077501 01  DLI-IO-WLINLC.                                                       
077601*    03  -COPY WDL611                                                     
077602     EJECT                                                                
077603 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC01'.                    
077604 01  DLI-IO-WLINLC01.                                                     
077605*    03  -COPY WDL601 -PRE L601-                                          
077701     EJECT                                                                
077801 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL623'.                      
077901 01  DLI-IO-WDL623.                                                       
078001*    03  -COPY WDL623                                                     
078101     EJECT                                                                
078201 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
078301 01  DLI-IO-WDD311.                                                       
078401*    03  -COPY WDD311                                                     
078501     EJECT                                                                
078601 01  FILLER               PIC X(16)   VALUE 'WDB601 RECV'.                
078701 01  DLI-IO-AREA-B601-REC.                                                
078801*     03  -COPY WDB601 -PRE REC-                                          
078901 01  DLI-IO-L6D1.                                                         
079001*    03  -COPY WDL6D1                                                     
079101     EJECT                                                                
079201 01  FILLER               PIC X(16)   VALUE 'DLI-IO-W6G130'.              
079301 01  DLI-IO-AREA-W6G130.                                                  
079401*    03  -COPY W6GX6006                                                   
079501     EJECT                                                                
079601                                                                          
079701 LINKAGE SECTION.                                                         
079801                                                                          
079901*01  -COPY W0009   -PRE MSG-                                              
080001 01  MQASYNC-PCB                 PIC X.                                   
080101 01  DISTRDOC-PCB                PIC X.                                   
080201     EJECT                                                                
080301*01  -COPY W0009   -PRE ALT3-                                             
080401     EJECT                                                                
080501*01  -COPY W0009   -PRE HOPP-                                             
080601     EJECT                                                                
080701*01  -COPY W0009   -PRE WL0102-                                           
080801     EJECT                                                                
080901 01  ATAB-PCB                    PIC X.                                   
081001     EJECT                                                                
081101*01  -COPY W0008  -PRE GX63-                                              
081201     05  FILLER                  PIC X.                                   
081301     EJECT                                                                
081401*01  -COPY W0008  -PRE INLC-                                              
081501     05  FILLER                  PIC X.                                   
081601     EJECT                                                                
081701*01  -COPY W0008  -PRE INLC1-                                             
081801     05  FILLER                  PIC X.                                   
081901     EJECT                                                                
082001*01  -COPY W0008  -PRE INLC2-                                             
082101     05  FILLER                  PIC X.                                   
082201     EJECT                                                                
082301*01  -COPY W0008  -PRE INLD-                                              
082401     05  FILLER                  PIC X.                                   
082501     EJECT                                                                
082601*01  -COPY W0008  -PRE WDK7-                                              
082701     05  FILLER                  PIC X.                                   
082801     EJECT                                                                
082901*01  -COPY W0008  -PRE KOMA-                                              
083001     05  FILLER                  PIC X.                                   
083101     EJECT                                                                
083201*01  -COPY W0008  -PRE FILB-                                              
083301     05  FILLER                  PIC X.                                   
083401     EJECT                                                                
083501*01  -COPY W0008  -PRE KNDB-                                              
083601     05  FILLER                  PIC X.                                   
083701     EJECT                                                                
083801*01  -COPY W0008  -PRE ARTC-                                              
083901     05  FILLER                  PIC X.                                   
084001     EJECT                                                                
084101*01  -COPY W0008  -PRE GX65-                                              
084201     05  FILLER                  PIC X.                                   
084301     EJECT                                                                
084401*01  -COPY W0008  -PRE WLLOGA-                                            
084501     05  FILLER                  PIC X.                                   
084601     EJECT                                                                
084701*01  -COPY W0008  -PRE SAPA-                                              
084801     05  FILLER                  PIC X.                                   
084901     EJECT                                                                
085001*01  -COPY W0008  -PRE FILC-                                              
085101     05  FILLER                  PIC X.                                   
085201     EJECT                                                                
085301*01  -COPY W0008  -PRE WDB6-                                              
085401     05  FILLER                  PIC X.                                   
085501     EJECT                                                                
085601*01  -COPY W0008  -PRE WDD3-                                              
085701     05  FILLER                  PIC X.                                   
085801     EJECT                                                                
085901*01  -COPY W0008  -PRE INLD2-                                             
086001     05  FILLER                  PIC X.                                   
086101     EJECT                                                                
086201*01  -COPY W0008  -PRE WDK6-                                              
086301     05  FILLER                  PIC X.                                   
086401*01  -COPY W0008  -PRE KVAH-                                              
086501     05  FILLER                  PIC X.                                   
086601*01  -COPY W0008  -PRE WDT4-                                              
086701     05  FILLER                  PIC X.                                   
086801*01  -COPY W0008  -PRE WDL6-                                              
086901     05  FILLER                  PIC X.                                   
087001*01  -COPY W0008  -PRE WDL6D-                                             
087101     05  FILLER                  PIC X.                                   
087201 01  LABL-WDB6-PCB               PIC X.                                   
087301*01  -COPY W0008  -PRE W6G1-                                              
087401     05  FILLER                  PIC X.                                   
087501 01  W218-WDK6-PCB               PIC X.                                   
087601 01  W218-WDK7-PCB               PIC X.                                   
087701 01  W218-WDL6-PCB               PIC X.                                   
087801 01  W218-WDF1-PCB               PIC X.                                   
087901 01  W218-WDB6-PCB               PIC X.                                   
088001 01  W218-WDD9-PCB               PIC X.                                   
088101 01  CROS-WDE6-PCB               PIC X.                                   
088201 01  CROS-WDE4-PCB               PIC X.                                   
088301 01  CROS-WDE41-PCB              PIC X.                                   
088401 01  CROS-WDE1-PCB               PIC X.                                   
088501 01  CROS-WDE6F-PCB              PIC X.                                   
088601 01  CROS-WDB6-PCB               PIC X.                                   
088701 01  PLATS-DM-PCB                PIC X.                                   
088801 01  PLATS-DN-PCB                PIC X.                                   
088901 01  PLATS-DP-PCB                PIC X.                                   
089001 01  PLATS-DO-PCB                PIC X.                                   
089101 01  PLATS-WDE6C-PCB             PIC X.                                   
089201 01  PLATS-GMTC-PCB              PIC X.                                   
089301 01  PLATS-WDB6-PCB              PIC X.                                   
089401     EJECT                                                                
089501 PROCEDURE DIVISION  USING MSG-PCB  MQASYNC-PCB DISTRDOC-PCB              
089601                           ALT3-PCB HOPP-PCB WL0102-PCB ATAB-PCB          
089701                           GX63-PCB INLC-PCB INLC1-PCB INLC2-PCB          
089801                           INLD-PCB WDK7-PCB KOMA-PCB FILB-PCB            
089901                           KNDB-PCB ARTC-PCB GX65-PCB WLLOGA-PCB          
090001                           SAPA-PCB FILC-PCB WDB6-PCB                     
090101                           WDD3-PCB INLD2-PCB WDK6-PCB KVAH-PCB           
090201                           WDT4-PCB WDL6-PCB WDL6D-PCB                    
090301                           LABL-WDB6-PCB W6G1-PCB                         
090401                           W218-WDK6-PCB W218-WDK7-PCB                    
090501                           W218-WDL6-PCB W218-WDF1-PCB                    
090601                           W218-WDB6-PCB W218-WDD9-PCB                    
090701                           CROS-WDE6-PCB CROS-WDE4-PCB                    
090801                           CROS-WDE41-PCB                                 
090901                           CROS-WDE1-PCB CROS-WDE6F-PCB                   
091001                           CROS-WDB6-PCB                                  
091101                           PLATS-DM-PCB  PLATS-DN-PCB                     
091201                           PLATS-DP-PCB  PLATS-DO-PCB                     
091301                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
091401                           PLATS-WDB6-PCB.                                
091501                                                                          
091601 MAIN SECTION.                                                            
091701     ENTRY 'DLITCBL' USING MSG-PCB  MQASYNC-PCB DISTRDOC-PCB              
091801                           ALT3-PCB HOPP-PCB WL0102-PCB ATAB-PCB          
091901                           GX63-PCB INLC-PCB INLC1-PCB INLC2-PCB          
092001                           INLD-PCB WDK7-PCB KOMA-PCB FILB-PCB            
092101                           KNDB-PCB ARTC-PCB GX65-PCB WLLOGA-PCB          
092201                           SAPA-PCB FILC-PCB WDB6-PCB                     
092301                           WDD3-PCB INLD2-PCB WDK6-PCB KVAH-PCB           
092401                           WDT4-PCB WDL6-PCB WDL6D-PCB                    
092501                           LABL-WDB6-PCB W6G1-PCB                         
092601                           W218-WDK6-PCB W218-WDK7-PCB                    
092701                           W218-WDL6-PCB W218-WDF1-PCB                    
092801                           W218-WDB6-PCB W218-WDD9-PCB                    
092901                           CROS-WDE6-PCB CROS-WDE4-PCB                    
093001                           CROS-WDE41-PCB                                 
093101                           CROS-WDE1-PCB CROS-WDE6F-PCB                   
093201                           CROS-WDB6-PCB                                  
093301                           PLATS-DM-PCB  PLATS-DN-PCB                     
093401                           PLATS-DP-PCB  PLATS-DO-PCB                     
093501                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
093601                           PLATS-WDB6-PCB.                                
093701                                                                          
093801     PERFORM S10-HAEMTA-ANROPSDATA                                        
093901     IF SUB-KDRC = 0                                                      
094001       PERFORM A-INIT                                                     
094101       IF NYCKLAR-OK                                                      
094201        IF DC-CROSS-FLAG = 'N'                                            
094301         PERFORM B-KOLLA-NYCKLAR                                          
094401        END-IF                                                            
094501         IF NYCKLAR-OK                                                    
094601           IF REQU-KDPGMACT = 'E'                                         
094701             IF DC-CROSS-FLAG = 'N'                                       
094801              PERFORM G-KOLLA-INPUT                                       
094901             END-IF                                                       
095001             IF INDATA-OK AND IDUSER-OK                                   
095101                PERFORM H-UPPDATERA                                       
095201             END-IF                                                       
095301           END-IF                                                         
095401           IF (EJ-HOPP-UPDATE) AND (EJ-ATERHOPP)                          
095501              IF REC-DCS-KDTRADP = 'BR12' AND                             
095601                 REQU-KDPGMACT = 'E'                                      
095701                 PERFORM I-TRIGGER-BR-TRANS                               
095801              END-IF                                                      
095901              PERFORM F-LAES-VISA-INFO                                    
096001              IF REQU-KDPGMACT = 'X'                                      
096101                 MOVE UPDATE-DONE TO RESP-IDMSG-INFO                      
096201              END-IF                                                      
096301              IF EJ-PRINT                                                 
096401                 IF SUB-KDTRANS(1:6) = 'WLA102'                           
096501                   PERFORM S11-MSG-CONV                                   
096601                 END-IF                                                   
096701                 PERFORM S11-RETURNERA-SVAR                               
096801              ELSE                                                        
096901                 PERFORM S95-SEND-CLOSE                                   
097001              END-IF                                                      
097101           ELSE                                                           
097201              IF HOPP-UPDATE                                              
097301                 MOVE ZERO  TO SEND-REQU-IDREQVER                         
097401                 MOVE SPACE TO SEND-REQU-IDUSER                           
097501                 MOVE 'X'   TO SEND-REQU-KDPGMACT                         
097601                 PERFORM S12-SEND-OPEN                                    
097701                 PERFORM S13-SEND-MESSAGE                                 
097801                 PERFORM S14-SEND-CLOSE                                   
097901              ELSE                                                        
098001                 IF ATERHOPP                                              
098101                    MOVE YES           TO REQU-FLATERHOPP                 
098201                    MOVE REQU-WL0102I1 TO SEND-REQU-WL0102I1              
098301                    MOVE REQU-TAB      TO SEND-REQU-TAB                   
098401                    MOVE ZERO  TO SEND-REQU-IDREQVER                      
098501                    MOVE SPACE TO SEND-REQU-IDUSER                        
098601                    MOVE 'E'   TO SEND-REQU-KDPGMACT                      
098701                    PERFORM K-OMSTART-EGEN-TRANS                          
098801                 END-IF                                                   
098901              END-IF                                                      
099001           END-IF                                                         
099101         ELSE                                                             
099201           IF SUB-KDTRANS(1:6) = 'WLA102'                                 
099301             PERFORM S11-MSG-CONV                                         
099401           END-IF                                                         
099501           PERFORM S11-RETURNERA-SVAR                                     
099601         END-IF                                                           
099701       ELSE                                                               
099801         IF SUB-KDTRANS(1:6) = 'WLA102'                                   
099901           PERFORM S11-MSG-CONV                                           
100001         END-IF                                                           
100101         PERFORM S11-RETURNERA-SVAR                                       
100201       END-IF                                                             
100301     END-IF                                                               
100401     MOVE ZERO TO RETURN-CODE                                             
100501     GOBACK                                                               
100601     .                                                                    
100701     EJECT                                                                
100801 A-INIT SECTION.                                                          
100901     MOVE 'A-INIT                        ' TO WS-SECTION                  
101001                                                                          
101101     MOVE ALL '+' TO RESP-AREA                                            
101201     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
101301                     RESP-IDMSG-INFO                                      
101401                     RESP-IDELMT-ERROR                                    
101501     MOVE ZERO    TO RESP-KVRADER                                         
101601     MOVE '001'   TO RESP-IDRESVER                                        
101701     MOVE NEJ     TO HOPP-UPDATE-SW                                       
101801                     SW-REC-UPPD                                          
101901     MOVE JA                     TO NYCKLAR-SW                            
102001                                                                          
102101     IF SUB-KDTRANS(1:6) = 'WLA102'                                       
102201       MOVE 001                  TO AUTH-KDCALL                           
102301       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
102401                                    REQU-WZ01REQ2                         
102501       IF AUTH-KDRC > 0                                                   
102601         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
102701         MOVE NOO                TO KEYS-SW                               
102801       END-IF                                                             
102901       MOVE FUNCTION UPPER-CASE (REQU-IDUSER-003) TO                      
103001                                 REQU-IDUSER-003                          
103101       MOVE FUNCTION UPPER-CASE (REQU-IDUSER) TO                          
103201                                 REQU-IDUSER                              
103301       MOVE FUNCTION UPPER-CASE (REQU-KDMATT) TO                          
103401                                 REQU-KDMATT                              
103501       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY)   TO                      
103601                                 REQU-IDDC-KEY                            
103701       MOVE FUNCTION UPPER-CASE (REQU-KDPGMACT)   TO                      
103801                                 REQU-KDPGMACT                            
103901       MOVE +1 TO INDX                                                    
104001       MOVE REQU-KVRADER         TO MAX-INDX                              
104101                                    602CROS-KVRADER                       
104201       PERFORM UNTIL INDX > MAX-INDX                                      
104301        MOVE FUNCTION UPPER-CASE (REQU-CMD-IN(INDX)) TO                   
104401                                  REQU-CMD-IN(INDX)                       
104501**      INPUT FROM HANDHELD IS CURRENTLY BASED ON THE                     
104601**      CASE ID (DIST + CUST + ORDER + CASE NO), WHERE                    
104701**      ORDER IS A 7 DIGIT NUMBER. HOWEVER WDL6 HAS A                     
104801**      5 DIGIT ORDER NUMBER.                                             
104901        MOVE REQU-IDKUNDRF (INDX)                                         
105001                                 TO WS-IDKUNDRF-GRP                       
105101        IF WS-IDORDNR5-FILLER = SPACES                                    
105201          MOVE WS-IDORDNR5       TO WS-IDKUNDRF-IDORDNR5                  
105301        ELSE                                                              
105401          MOVE WS-IDORDNR7       TO WS-IDKUNDRF-IDORDNR5                  
105501          MOVE WS-IDKUNDRF-IDORDNR5                                       
105601                                 TO REQU-IDKUNDRF (INDX)                  
105701        END-IF                                                            
105801        PERFORM AA-FETCH-IDFAKT                                           
105901        ADD +1 TO INDX                                                    
106001       END-PERFORM                                                        
106101                                                                          
106201     END-IF                                                               
106301                                                                          
106401     INITIALIZE LABL-W612LABL                                             
106501     INITIALIZE KLBL-W612KLBL                                             
106601     ACCEPT W-DATUM-X  FROM DATE                                          
106701     ACCEPT W-TIME-X   FROM TIME                                          
106801     MOVE W-TIME-N     TO W-TIKLOCK                                       
106901     ACCEPT DAGENS-TID FROM TIME                                          
107001     MOVE DAGENS-TID   TO AKTUELL-TID                                     
107101                          AKTUELL-TID-X                                   
107201     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
107301                                                                          
107401     MOVE LOW-VALUE    TO W-WDL6ASEQ-MIN                                  
107501     MOVE HIGH-VALUE   TO W-WDL6ASEQ-MAX                                  
107601     MOVE ZERO         TO WS-KVBINART                                     
107701                                                                          
107801     MOVE 'WL010200'   TO FILC-FIL-IDPGM                                  
107901                          FILC2-FIL-IDPGM                                 
108001     MOVE W-DATUM      TO FILC-FIL-TIREGDAT                               
108101                          FILC2-FIL-TIREGDAT                              
108201     MOVE 'W61244  '   TO FILC-FIL-IDCPYTXT                               
108301     MOVE 'W61247  '   TO FILC2-FIL-IDCPYTXT                              
108401     MOVE ZERO         TO FILC-FIL-TIKLOCK                                
108501                          FILC2-FIL-TIKLOCK                               
108601                                                                          
108701     INITIALIZE SORT-TABELL                                               
108801     MOVE ZERO         TO W-KVANT-UPPD                                    
108901     IF REQU-FLATERHOPP = YES                                             
109001       CONTINUE                                                           
109101     ELSE                                                                 
109201       MOVE SPACE      TO REQU-TAB                                        
109301     END-IF                                                               
109401     .                                                                    
109501     EJECT                                                                
109601                                                                          
109701 AA-FETCH-IDFAKT SECTION.                                                 
109801     MOVE LOW-VALUE                  TO W-WDL6D1KY-MIN                    
109901     MOVE HIGH-VALUE                 TO W-WDL6D1KY-MAX                    
110001     MOVE REQU-IDKUNDRF(INDX)        TO W-SEQD-IDKUNDRF-MIN               
110101                                        W-SEQD-IDKUNDRF-MAX               
110201     MOVE WS-IDKUNDRF-IDORDNR5  TO    602CROS-IDORDNR5(INDX)              
110301     MOVE REQU-IDKUNDNR(INDX)        TO W-SEQD-IDKUNDNR-MIN               
110401                                        W-SEQD-IDKUNDNR-MAX               
110501                                        602CROS-IDKUNDNR (INDX)           
110601     MOVE REQU-IDKOLLI(INDX)         TO W-SEQD-IDKOLLI-MIN                
110701                                        W-SEQD-IDKOLLI-MAX                
110801                                        602CROS-IDKOLLI (INDX)            
110901     MOVE REQU-IDDC-KEY              TO W-SEQD-IDDC-MIN                   
111001                                        W-SEQD-IDDC-MAX                   
111101                                        602CROS-IDDC-KEY                  
111201     MOVE REQU-IDDISTR (INDX)        TO W-SEQD-IDDISTR-MIN                
111301                                        W-SEQD-IDDISTR-MAX                
111401                                        602CROS-IDDISTR (INDX)            
111501     PERFORM IMS-GU-WDL6D1                                                
111601     IF SEGMENT-FINNS                                                     
111701       MOVE SEQD-IDFAKT TO REQU-IDFAKT-KEY                                
111801                           W-IDFAKT                                       
111901     ELSE                                                                 
112001       MOVE NEJ                      TO NYCKLAR-SW                        
112101       MOVE CASE-NOT-FOUND           TO RESP-IDMSG-ERROR                  
112201       MOVE JA                       TO DC-CROSS-FLAG                     
112301     END-IF                                                               
112401     IF DC-CROSS-FLAG = 'J'                                               
112501      MOVE JA                       TO NYCKLAR-SW                         
112601      MOVE ZERO                       TO 602CROS-IDPRODNR (INDX)          
112701      MOVE 'S'                        TO 602CROS-KDPGMACT                 
112801      MOVE ZERO                       TO 602CROS-IDMSG-INFO               
112901      MOVE 001                        TO 602CROS-KDCALL                   
113001      MOVE REQU-CMD-IN(INDX)          TO 602CROS-KDCMDVAL(INDX)           
113101      IF REQU-CMD-IN(INDX) = 'REC'                                        
113201       CALL W602CROS USING 602CROS-W602CROS                               
113301                           CROS-WDE6-PCB CROS-WDE4-PCB                    
113401                           CROS-WDE41-PCB                                 
113501                           CROS-WDE1-PCB CROS-WDE6F-PCB                   
113601                           CROS-WDB6-PCB                                  
113701                           PLATS-DM-PCB  PLATS-DN-PCB                     
113801                           PLATS-DP-PCB  PLATS-DO-PCB                     
113901                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
114001                           PLATS-WDB6-PCB                                 
114101       IF 602CROS-KDSVAR = ' '                                            
114201        MOVE SPACE   TO RESP-IDMSG-ERROR                                  
114301                        RESP-IDMSG-INFO                                   
114401                        RESP-IDELMT-ERROR                                 
114501        IF 602CROS-IDMSG-INFO > ZERO                                      
114601         MOVE 602CROS-IDMSG-INFO TO RESP-IDMSG-INFO                       
114701         MOVE NEJ                      TO NYCKLAR-SW                      
114801        END-IF                                                            
114901        MOVE JA TO DC-CROSS-FLAG                                          
115001       ELSE                                                               
115101        MOVE NEJ TO DC-CROSS-FLAG                                         
115201        MOVE NEJ                      TO NYCKLAR-SW                       
115301       END-IF                                                             
115401      END-IF                                                              
115501     END-IF                                                               
115601     .                                                                    
115701     EJECT                                                                
115801                                                                          
115901 B-KOLLA-NYCKLAR SECTION.                                                 
116001     MOVE 'B-KOLLA-NYCKLAR               ' TO WS-SECTION                  
116101                                                                          
116201     MOVE REQU-IDDC-KEY TO SPAR-IDDC                                      
116301                                                                          
116401***  KONTROLL AV REQU-KDPGMACT                                            
116501     IF REQU-KDPGMACT = 'S' OR 'X' OR 'E'                                 
116601        CONTINUE                                                          
116701     ELSE                                                                 
116801        MOVE 'KDPGMACT' TO RESP-IDELMT-ERROR                              
116901        MOVE NEJ TO NYCKLAR-SW                                            
117001     END-IF                                                               
117101                                                                          
117201***  KONTROLL AV REQU-IDFAKT                                              
117301     IF REQU-IDFAKT-KEY = ALL '+'                                         
117401        MOVE 'IDFAKT' TO RESP-IDELMT-ERROR                                
117501        MOVE NEJ TO NYCKLAR-SW                                            
117601     ELSE                                                                 
117701        INSPECT REQU-IDFAKT-KEY REPLACING LEADING SPACE BY ZERO           
117801        IF REQU-IDFAKT-KEY NUMERIC                                        
117901        AND REQU-IDFAKT-KEY > ZERO                                        
118001           MOVE REQU-IDFAKT-KEY TO  W-IDFAKT                              
118101        ELSE                                                              
118201           MOVE 'IDFAKT' TO RESP-IDELMT-ERROR                             
118301           MOVE NEJ TO NYCKLAR-SW                                         
118401        END-IF                                                            
118501     END-IF                                                               
118601                                                                          
118701***  KONTROLL IDDC                                                        
118801     MOVE REQU-IDDC-KEY TO W-IDDC                                         
118901     MOVE W-IDDC        TO WS-IDDC-SPAR                                   
119001                           W-IDDC-B6                                      
119101                           WS-IDDC                                        
119201     PERFORM IMS-22-GU-WDB601                                             
119301     IF DCS-KDDC = SPACE OR DCS-DDC                                       
119401        MOVE 'IDDC' TO RESP-IDELMT-ERROR                                  
119501        MOVE NEJ TO NYCKLAR-SW                                            
119601     ELSE                                                                 
119701       MOVE DCS-IDDISTR-RETUR  TO W-IDDISTR-RETUR                         
119801       MOVE DCS-IDKUNDNR-RETUR TO W-IDKUNDNR-RETUR                        
119901     END-IF                                                               
120001                                                                          
120101     PERFORM IMS-22-GU-WDB601-REC                                         
120201     IF REC-DCS-CANADA OR REC-DCS-USA                                     
120301        SET AKTUELLT-LAND-NA    TO TRUE                                   
120401        IF REC-DCS-USA                                                    
120501          SET AKTUELLT-LAND-USA TO TRUE                                   
120601        ELSE                                                              
120701          SET AKTUELLT-EJ-USA   TO TRUE                                   
120801        END-IF                                                            
120901     ELSE                                                                 
121001        SET AKTUELLT-EJ-NA      TO TRUE                                   
121101     END-IF                                                               
121201     IF REC-DCS-FLTRACK = 'J'                                             
121301        MOVE 'J'  TO WS-FLTRACK                                           
121401     ELSE                                                                 
121501        MOVE 'N'  TO WS-FLTRACK                                           
121601     END-IF                                                               
121701                                                                          
121801     IF REQU-IDFAKT-KEY = ALL '+'                                         
121901        MOVE ZERO TO RESP-IDFAKT-KEY                                      
122001     ELSE                                                                 
122101        MOVE REQU-IDFAKT-KEY TO RESP-IDFAKT-KEY                           
122201     END-IF                                                               
122301     INSPECT RESP-IDFAKT-KEY REPLACING LEADING ZERO  BY SPACE             
122401     MOVE REQU-IDDC-KEY   TO RESP-IDDC-KEY                                
122501                                                                          
122601     MOVE REQU-KDMATT       TO URV-KDMATT                                 
122701     MOVE REQU-KDMATT       TO WS-KDMATT                                  
122801                                                                          
122901     IF NYCKLAR-FEL                                                       
123001        IF RESP-IDELMT-ERROR = 'KDPGMACT'                                 
123101           MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                          
123201        ELSE                                                              
123301           MOVE INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                    
123401        END-IF                                                            
123501     END-IF                                                               
123601     .                                                                    
123701     EJECT                                                                
123801 F-LAES-VISA-INFO SECTION.                                                
123901     MOVE 'F-LAES-VISA-INFO              ' TO WS-SECTION                  
124001                                                                          
124101     MOVE 500      TO MAX-INDX                                            
124201     MOVE ZERO     TO WS-KVANT                                            
124301     MOVE +1       TO INDX                                                
124401     MOVE LOW-VALUE    TO W-WDL6ASEQ-MIN                                  
124501     MOVE HIGH-VALUE   TO W-WDL6ASEQ-MAX                                  
124601     MOVE W-IDFAKT TO W-SEQA-IDFAKT-MIN                                   
124701                      W-SEQA-IDFAKT-MAX                                   
124801                                                                          
124901     PERFORM IMS-07-GU-INLC-WLINLC11-F                                    
125001     IF SEGMENT-SAKNAS                                                    
125101        MOVE LINES-NOT-FOUND TO RESP-IDMSG-ERROR                          
125201     ELSE                                                                 
125301        MOVE ALL '+' TO RESP-IDUSER-003                                   
125401        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
125501                      INDX > MAX-INDX                                     
125601           ADD 1 TO WS-KVANT                                              
125701           MOVE INL-IDKUNDRF TO W-SPAR-IDKUNDRF                           
125801           MOVE INL-IDKUNDNR TO W-SPAR-IDKUNDNR                           
125901           MOVE INL-IDKOLLI  TO W-SPAR-IDKOLLI                            
126001           IF INDATA-OK                                                   
126101              MOVE INL-ADINLOMR        TO RESP-ADINLOMR(INDX)             
126201           ELSE                                                           
126301              MOVE REQU-ADINLOMR(INDX) TO RESP-ADINLOMR(INDX)             
126401           END-IF                                                         
126501           IF INDX <= MAX-INDX                                            
126601               MOVE INL-IDKUNDRF TO RESP-IDKUNDRF(INDX)                   
126701                                    W-SPAR-IDKUNDRF                       
126801               MOVE INL-IDKUNDNR TO RESP-IDKUNDNR(INDX)                   
126901                                    W-SPAR-IDKUNDNR                       
127001               MOVE INL-IDKOLLI  TO RESP-IDKOLLI(INDX)                    
127101                                    W-SPAR-IDKOLLI                        
127201               MOVE INL-KDKOLLI  TO RESP-KDKOLLI(INDX)                    
127301               MOVE SPACE        TO RESP-TEINFO (INDX)                    
127401               IF INL-FLSKAKOL = 'J'                                      
127501                  MOVE 'DCA    ' TO RESP-TEINFO (INDX)                    
127601               END-IF                                                     
127701               IF INL-IDPTYP     =  'R30' AND                             
127801                  INL-TIINLMOT   >   ZERO                                 
127901                  MOVE 'MISSING' TO RESP-TEINFO (INDX)                    
128001               END-IF                                                     
128101            END-IF                                                        
128201***** 0220                                                                
128301            IF REQU-CMD-IN(INDX) = ALL '+' OR SPACE OR                    
128401            LOW-VALUE                                                     
128501                MOVE SPACE TO RESP-CMD-IN(INDX)                           
128601                              RESP-IDMSG-ERROR-LINE(INDX)                 
128701            END-IF                                                        
128801            IF REQU-KDPGMACT = 'S' OR (REQU-KDPGMACT = 'E'                
128901               AND INDATA-OK)                                             
129001               MOVE ' '        TO RESP-IDMSG-ERROR-LINE(INDX)             
129101               MOVE ' '        TO RESP-CMD-IN(INDX)                       
129201            END-IF                                                        
129301***** 0220                                                                
129401                                                                          
129501            MOVE ZERO TO W-KVRADER                                        
129601                         W-KVNYART                                        
129701                         W-KVPRIOART                                      
129801                         W-ANTAL-LASN                                     
129901                                                                          
130001            PERFORM UNTIL SEGMENT-SAKNAS                                  
130101                 OR  W-SPAR-IDKUNDRF NOT = INL-IDKUNDRF                   
130201                 OR  W-SPAR-IDKUNDNR NOT = INL-IDKUNDNR                   
130301                 OR  W-SPAR-IDKOLLI  NOT = INL-IDKOLLI                    
130401                                                                          
130501                 ADD 1 TO W-KVRADER                                       
130601                 IF INL-FLPRIO = 'J' OR 'Y'                               
130701                    ADD +1  TO W-KVPRIOART                                
130801                 END-IF                                                   
130901                 PERFORM S01-LAS-FRAM-ARTIKEL                             
131001                 ADD +1 TO W-ANTAL-LASN                                   
131101                 IF INL-ADLAGOMR = ZERO                                   
131201                  IF REC-DCS-CDC                                          
131301                     PERFORM IMS-GU-WDK611                                
131401                     IF CLAG-ADLAGOMR = ZERO                              
131501                        ADD +1 TO W-KVNYART                               
131601                     END-IF                                               
131701                  ELSE                                                    
131801                    PERFORM IMS-11-GU-WDK711                              
131901                    IF SLAG-ADLAGOMR = ZERO                               
132001                       ADD +1 TO W-KVNYART                                
132101                    END-IF                                                
132201                  END-IF                                                  
132301                 END-IF                                                   
132401                 PERFORM IMS-08-GN-INLC-WLINLC11                          
132501            END-PERFORM                                                   
132601                                                                          
132701            IF INDX <= MAX-INDX                                           
132801               MOVE W-KVRADER   TO RESP-IDARTNR-KOLLI(INDX)               
132901               MOVE W-KVNYART   TO RESP-IDARTNR-NEW(INDX)                 
133001               MOVE W-KVPRIOART TO RESP-IDARTNR-PRIO(INDX)                
133101               ADD 1 TO INDX                                              
133201            END-IF                                                        
133301        END-PERFORM                                                       
133401        MOVE WS-KVANT TO RESP-KVRADER                                     
133501        IF WS-KVANT > 500                                                 
133601           MOVE 500 TO RESP-KVRADER                                       
133701           MOVE TOO-MANY-LINES TO RESP-IDMSG-ERROR                        
133801        END-IF                                                            
133901     END-IF                                                               
134001     .                                                                    
134101     EJECT                                                                
134201 G-KOLLA-INPUT SECTION.                                                   
134301     MOVE 'G-KOLLA-INPUT                 ' TO WS-SECTION                  
134401                                                                          
134501     MOVE NEJ TO SW-REQU-RAD-IFYLLD                                       
134601                                                                          
134701     IF REQU-KVRADER NUMERIC                                              
134801     AND REQU-KVRADER > ZERO                                              
134901        MOVE REQU-KVRADER TO MAX-INDX                                     
135001     END-IF                                                               
135101                                                                          
135201     MOVE +1 TO INDX                                                      
135301     PERFORM UNTIL INDX > MAX-INDX                                        
135401        MOVE SPACE TO RESP-IDMSG-ERROR-LINE(INDX)                         
135501        ADD +1 TO INDX                                                    
135601     END-PERFORM                                                          
135701                                                                          
135801     MOVE +1 TO INDX                                                      
135901     PERFORM UNTIL INDX > MAX-INDX                                        
136001        IF REQU-CMD-IN (INDX) = ALL '+'                                   
136101           CONTINUE                                                       
136201        ELSE                                                              
136301           MOVE JA TO SW-REQU-RAD-IFYLLD                                  
136401           MOVE MAX-INDX TO INDX                                          
136501        END-IF                                                            
136601        ADD +1 TO INDX                                                    
136701     END-PERFORM                                                          
136801                                                                          
136901     IF SW-REQU-RAD-IFYLLD = NEJ                                          
137001        MOVE NEJ TO INDATA-SW                                             
137101     ELSE                                                                 
137201        PERFORM GC-KOLLA-CMD                                              
137301        IF INDATA-OK                                                      
137401           MOVE +1 TO INDX                                                
137501           PERFORM UNTIL INDX > MAX-INDX OR                               
137601                         REQU-IDKUNDNR(INDX) NOT NUMERIC                  
137701              IF REQU-CMD-IN(INDX) = 'BIN' OR 'HAC'                       
137801                 PERFORM GB-KOLLA-BIN                                     
137901              ELSE                                                        
138001                 PERFORM GA-KONTROLL-AV-PTYP                              
138101              END-IF                                                      
138201              ADD +1 TO INDX                                              
138301           END-PERFORM                                                    
138401        END-IF                                                            
138501     END-IF                                                               
138601                                                                          
138701     MOVE +1 TO INDX                                                      
138801     IF REQU-IDUSER-003 = ALL '+' OR SPACE                                
138901       PERFORM UNTIL INDX > MAX-INDX                                      
139001       OR IDUSER-SW = NEJ                                                 
139101         IF REQU-CMD-IN(INDX) = 'BIN'                                     
139201         OR REQU-CMD-IN(INDX) = 'LOS'                                     
139301         OR REQU-CMD-IN(INDX) = 'MIS'                                     
139401         OR REQU-CMD-IN(INDX) = 'RET'                                     
139501         OR REQU-CMD-IN(INDX) = 'HAC'                                     
139601           IF DCS-FLBINNUT = JA                                           
139701              MOVE NEJ         TO IDUSER-SW                               
139801           ELSE                                                           
139901              MOVE SPACE       TO WS-IDUSER-003                           
140001              MOVE JA          TO IDUSER-SW                               
140101           END-IF                                                         
140201         ELSE                                                             
140301           MOVE SPACE          TO WS-IDUSER-003                           
140401           MOVE JA             TO IDUSER-SW                               
140501         END-IF                                                           
140601         ADD +1                TO INDX                                    
140701       END-PERFORM                                                        
140801     ELSE                                                                 
140901        MOVE REQU-IDUSER-003   TO WS-IDUSER-003                           
141001        MOVE JA                TO IDUSER-SW                               
141101     END-IF                                                               
141201                                                                          
141301     IF SW-REQU-RAD-IFYLLD = NEJ                                          
141401        MOVE NO-DATA-ENTERED TO RESP-IDMSG-ERROR                          
141501     ELSE                                                                 
141601       IF IDTRACK-FEL                                                     
141701          MOVE TRACKING-ID-MISSING TO RESP-IDMSG-ERROR                    
141801       ELSE                                                               
141901          IF INDATA-FEL                                                   
142001             MOVE IS-INVALID TO RESP-IDMSG-ERROR                          
142101          END-IF                                                          
142201       END-IF                                                             
142301     END-IF                                                               
142401                                                                          
142501     IF IDUSER-FEL                                                        
142601        MOVE 'IDANSTNR'         TO RESP-IDELMT-ERROR                      
142701        MOVE MUST-ENTER-EMP-ID  TO RESP-IDMSG-ERROR                       
142801     END-IF                                                               
142901     .                                                                    
143001     EJECT                                                                
143101 GA-KONTROLL-AV-PTYP SECTION.                                             
143201     MOVE 'GA-KONTROLL-AV-PTYP           ' TO WS-SECTION                  
143301                                                                          
143401     MOVE LOW-VALUE           TO W-WDL6ASEQ-MIN                           
143501     MOVE HIGH-VALUE          TO W-WDL6ASEQ-MAX                           
143601     MOVE W-IDFAKT            TO W-SEQA-IDFAKT-MIN                        
143701                                 W-SEQA-IDFAKT-MAX                        
143801     INSPECT REQU-IDKUNDRF(INDX) REPLACING LEADING SPACE BY ZERO          
143901     MOVE REQU-IDKUNDRF (INDX) TO W-SEQA-IDKUNDRF-MIN                     
144001                                 W-SEQA-IDKUNDRF-MAX                      
144101     MOVE REQU-IDKUNDNR (INDX) TO W-SEQA-IDKUNDNR-MIN                     
144201                                 W-SEQA-IDKUNDNR-MAX                      
144301     MOVE REQU-IDKOLLI (INDX) TO W-SEQA-IDKOLLI-MIN                       
144401                                 W-SEQA-IDKOLLI-MAX                       
144501                                                                          
144601     PERFORM IMS-08-GN-INLC-WLINLC11                                      
144701                                                                          
144801     PERFORM UNTIL SEGMENT-SAKNAS                                         
144901                                                                          
145001         IF  REQU-CMD-IN (INDX) = 'LOS'                                   
145101         AND INL-IDPTYP       NOT = 'R30'                                 
145201            MOVE NEJ TO INDATA-SW                                         
145301            MOVE INDX TO INDX-DISPLAY                                     
145401            MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                     
145501            STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE                  
145601                  INTO RESP-IDELMT-ERROR                                  
145701         END-IF                                                           
145801                                                                          
145901         IF  REQU-CMD-IN (INDX) = 'LOS'                                   
146001         AND INL-TIINLMOT  NOT > ZERO                                     
146101            MOVE NEJ TO INDATA-SW                                         
146201            MOVE INDX TO INDX-DISPLAY                                     
146301            MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                     
146401            STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE                  
146501                  INTO RESP-IDELMT-ERROR                                  
146601         END-IF                                                           
146701                                                                          
146801         IF (REQU-CMD-IN (INDX) = 'MIS'                                   
146901         OR  REQU-CMD-IN (INDX) = 'RET'                                   
147001         OR  REQU-CMD-IN (INDX) = 'DCA'                                   
147101         OR  REQU-CMD-IN (INDX) = 'BIN'                                   
147201         OR  REQU-CMD-IN (INDX) = 'HAC'                                   
147301         OR  REQU-CMD-IN (INDX) = 'CAN'                                   
147401         OR  REQU-CMD-IN (INDX) = 'LOC')                                  
147501         AND INL-IDPTYP       NOT = '310'                                 
147601          IF SUB-KDTRANS(1:6) = 'WLA102'                                  
147701          AND REQU-CMD-IN (INDX) = 'DCA'                                  
147801            MOVE NEJ TO DCA-RECEIVED                                      
147901          ELSE                                                            
148001            MOVE NEJ TO INDATA-SW                                         
148101            MOVE INDX TO INDX-DISPLAY                                     
148201            MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                     
148301            STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE                  
148401                  INTO RESP-IDELMT-ERROR                                  
148501          END-IF                                                          
148601         END-IF                                                           
148701         PERFORM IMS-08-GN-INLC-WLINLC11                                  
148801     END-PERFORM                                                          
148901     .                                                                    
149001     EJECT                                                                
149101 GB-KOLLA-BIN SECTION.                                                    
149201**** THIS PARA USED FOR BOTH BIN AND HAC                                  
149301     MOVE 'GB-KOLLA-BIN                  ' TO WS-SECTION                  
149401                                                                          
149501     PERFORM S02-SKAPA-WDL6A1KY                                           
149601     PERFORM IMS-04-GU-WLINLD01                                           
149701                                                                          
149801     IF SEGMENT-SAKNAS                                                    
149901        MOVE NEJ TO INDATA-SW                                             
150001        MOVE INDX TO INDX-DISPLAY                                         
150101        MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                         
150201        STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE                      
150301                  INTO RESP-IDELMT-ERROR                                  
150401     END-IF                                                               
150501                                                                          
150601     PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                           
150701                                                                          
150801         IF SEQA-IDPTYP NOT = '310'                                       
150901            MOVE NEJ TO INDATA-SW                                         
151001            MOVE INDX TO INDX-DISPLAY                                     
151101            MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                     
151201            STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE                  
151301                   INTO RESP-IDELMT-ERROR                                 
151401         ELSE                                                             
151501            MOVE SEQA-IDARTNR   TO W-IDARTNR                              
151601            MOVE SEQA-IDDC      TO W-IDDC                                 
151701            MOVE SEQA-DAINLEV   TO W-DAINLEV                              
151801            IF WS-FLTRACK = 'J'                                           
151901              PERFORM IMS-GU-WDL623                                       
152001              IF SEGMENT-SAKNAS                                           
152101                 MOVE NEJ TO INDATA-SW                                    
152201                             IDTRACK-SW                                   
152301                 MOVE INDX TO INDX-DISPLAY                                
152401                 MOVE 'BIN' TO RESP-IDMSG-ERROR-LINE(INDX)                
152501                 MOVE TRACKING-ID-MISSING TO RESP-IDMSG-ERROR             
152601              END-IF                                                      
152701            END-IF                                                        
152801            IF REC-DCS-CDC                                                
152901              PERFORM IMS-GU-WDK611                                       
153001              ADD +1 TO WS-KVBINART                                       
153101                                                                          
153201              IF WS-KVBINART > WS-KVBINART-MAX                            
153301                 MOVE NEJ TO INDATA-SW                                    
153401                 MOVE INDX TO INDX-DISPLAY                                
153501                 MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                
153601                 STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE             
153701                      INTO RESP-IDELMT-ERROR                              
153801              END-IF                                                      
153901                                                                          
154001              IF CLAG-ADLAGOMR = ZERO                                     
154101                 MOVE NEJ TO INDATA-SW                                    
154201                 MOVE INDX TO INDX-DISPLAY                                
154301                 MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                
154401                 STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE             
154501                      INTO RESP-IDELMT-ERROR                              
154601              END-IF                                                      
154701            ELSE                                                          
154801              PERFORM IMS-12-GHU-WDK711                                   
154901              ADD +1 TO WS-KVBINART                                       
155001                                                                          
155101              IF WS-KVBINART > WS-KVBINART-MAX                            
155201                 MOVE NEJ TO INDATA-SW                                    
155301                 MOVE INDX TO INDX-DISPLAY                                
155401                 MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                
155501                 STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE             
155601                      INTO RESP-IDELMT-ERROR                              
155701              END-IF                                                      
155801                                                                          
155901              IF SLAG-ADLAGOMR = ZERO                                     
156001                 MOVE NEJ TO INDATA-SW                                    
156101                 MOVE INDX TO INDX-DISPLAY                                
156201                 MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                
156301                 STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE             
156401                      INTO RESP-IDELMT-ERROR                              
156501              END-IF                                                      
156601            END-IF                                                        
156701         END-IF                                                           
156801                                                                          
156901         PERFORM IMS-05-GN-WLINLD01                                       
157001     END-PERFORM                                                          
157101     .                                                                    
157201     EJECT                                                                
157301 GC-KOLLA-CMD SECTION.                                                    
157401     MOVE 'GC-KOLLA-CMD                  ' TO WS-SECTION                  
157501                                                                          
157601******************************************************************        
157701*  CMD         PR  STARTAR WL10S2 BINNING LIST SORT PART NO               
157801*              PRL STARTAR WL10S2 BINNING LIST SORT LOCATION              
157901*              LBL PRINT CHINESE LABELS                                   
158001*              MIS SAKNAT KOLLI                                           
158101*              RET RETUR                                                  
158201*              LOS LOST KOLLI                                             
158301*              DCA SKADAT KOLLI                                           
158401*              CAN SKADAT KOLLI                                           
158501*              BIN TRANS TILL 6303                                        
158601*              HAC TRANS TILL 6303                                        
158701*              REC MOTTAGNING KOLLI                                       
158801*              LOC UPPDATERING AV UNL AREA PÅ KOLLI NIVÅ                  
158901******************************************************************        
159001                                                                          
159101     MOVE JA    TO INDATA-SW                                              
159201     MOVE SPACE TO W-CMD                                                  
159301                   WS-REC                                                 
159401                   WS-BIN                                                 
159501     MOVE ZERO TO  INDX-BIN                                               
159601                   INDX-REC                                               
159701     MOVE +1    TO INDX                                                   
159801                                                                          
159901     PERFORM UNTIL INDX > MAX-INDX                                        
160001        IF REQU-CMD-IN(INDX) NOT = ALL '+'                                
160101           MOVE REQU-CMD-IN(INDX) TO RESP-CMD-IN(INDX)                    
160201        ELSE                                                              
160301           MOVE SPACE             TO RESP-CMD-IN(INDX)                    
160401        END-IF                                                            
160501        IF REQU-CMD-IN (INDX) NOT = ALL '+'                               
160601        AND REQU-CMD-IN (INDX) NOT = 'PR '                                
160701        AND REQU-CMD-IN (INDX) NOT = 'PRL'                                
160801        AND REQU-CMD-IN (INDX) NOT = 'LBL'                                
160901        AND REQU-CMD-IN (INDX) NOT = 'MIS'                                
161001        AND REQU-CMD-IN (INDX) NOT = 'RET'                                
161101        AND REQU-CMD-IN (INDX) NOT = 'LOS'                                
161201        AND REQU-CMD-IN (INDX) NOT = 'DCA'                                
161301        AND REQU-CMD-IN (INDX) NOT = 'CAN'                                
161401        AND REQU-CMD-IN (INDX) NOT = 'BIN'                                
161501        AND REQU-CMD-IN (INDX) NOT = 'HAC'                                
161601        AND REQU-CMD-IN (INDX) NOT = 'REC'                                
161701        AND REQU-CMD-IN (INDX) NOT = 'LOC'                                
161801        AND REQU-CMD-IN (INDX) NOT = SPACE                                
161901           MOVE NEJ TO INDATA-SW                                          
162001           MOVE INDX TO INDX-DISPLAY                                      
162101           MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                      
162201           STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE                   
162301                    INTO RESP-IDELMT-ERROR                                
162401        ELSE                                                              
162501           IF  REQU-CMD-IN(INDX) = 'RET'                                  
162601           AND (SPAR-DCS-NDC-NA)                                          
162701               MOVE NEJ TO INDATA-SW                                      
162801               MOVE INDX TO INDX-DISPLAY                                  
162901               MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                  
163001               STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE               
163101                             INTO RESP-IDELMT-ERROR                       
163201           END-IF                                                         
163202           IF  ((REQU-CMD-IN(INDX) = 'MIS' OR 'LOS')                      
163203           AND (NDC-BR))                                                  
163204               MOVE NEJ TO INDATA-SW                                      
163205               MOVE INDX TO INDX-DISPLAY                                  
163206               MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                  
163207               STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE               
163208                             INTO RESP-IDELMT-ERROR                       
163209           END-IF                                                         
163301        END-IF                                                            
163401                                                                          
163501        IF REQU-CMD-IN (INDX) = 'PR '                                     
163601        OR REQU-CMD-IN (INDX) = 'PRL'                                     
163701        OR REQU-CMD-IN (INDX) = 'LBL'                                     
163801        OR REQU-CMD-IN (INDX) = 'MIS'                                     
163901        OR REQU-CMD-IN (INDX) = 'RET'                                     
164001        OR REQU-CMD-IN (INDX) = 'LOS'                                     
164101        OR REQU-CMD-IN (INDX) = 'DCA'                                     
164201        OR REQU-CMD-IN (INDX) = 'CAN'                                     
164301        OR REQU-CMD-IN (INDX) = 'BIN'                                     
164401        OR REQU-CMD-IN (INDX) = 'HAC'                                     
164501        OR REQU-CMD-IN (INDX) = 'REC'                                     
164601        OR REQU-CMD-IN (INDX) = 'LOC'                                     
164701           IF REQU-IDKUNDRF (INDX) = SPACE                                
164801           AND REQU-IDKUNDNR (INDX) = ZERO                                
164901           AND REQU-IDKOLLI (INDX) = ZERO                                 
165001              MOVE NEJ TO INDATA-SW                                       
165101              MOVE INDX TO INDX-DISPLAY                                   
165201              MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                   
165301              STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE                
165401                      INTO RESP-IDELMT-ERROR                              
165501           END-IF                                                         
165601        END-IF                                                            
165701                                                                          
165801        IF REQU-CMD-IN (INDX) = 'MIS'                                     
165901        OR REQU-CMD-IN (INDX) = 'RET'                                     
166001        OR REQU-CMD-IN (INDX) = 'LOS'                                     
166101        OR REQU-CMD-IN (INDX) = 'DCA'                                     
166201        OR REQU-CMD-IN (INDX) = 'CAN'                                     
166301        OR REQU-CMD-IN (INDX) = 'BIN'                                     
166401        OR REQU-CMD-IN (INDX) = 'HAC'                                     
166501        OR REQU-CMD-IN (INDX) = 'REC'                                     
166601        OR REQU-CMD-IN (INDX) = 'LOC'                                     
166701          IF W-CMD = SPACE                                                
166801             MOVE REQU-CMD-IN(INDX) TO W-CMD                              
166901          ELSE                                                            
167001             IF W-CMD = 'PR ' OR  'PRL' OR 'LBL'                          
167101                MOVE NEJ TO INDATA-SW                                     
167201                MOVE INDX TO INDX-DISPLAY                                 
167301                MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                 
167401                STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE              
167501                       INTO RESP-IDELMT-ERROR                             
167601             END-IF                                                       
167701          END-IF                                                          
167801        END-IF                                                            
167901                                                                          
168001        IF REQU-CMD-IN(INDX) = 'PR '                                      
168101           IF W-CMD  = SPACE                                              
168201              MOVE REQU-CMD-IN (INDX) TO W-CMD                            
168301           ELSE                                                           
168401              IF W-CMD = 'MIS'                                            
168501              OR W-CMD = 'RET'                                            
168601              OR W-CMD = 'LOS'                                            
168701              OR W-CMD = 'DCA'                                            
168801              OR W-CMD = 'CAN'                                            
168901              OR W-CMD = 'BIN'                                            
169001              OR W-CMD = 'HAC'                                            
169101              OR W-CMD = 'PRL'                                            
169201              OR W-CMD = 'LBL'                                            
169301              OR W-CMD = 'REC'                                            
169401              OR W-CMD = 'LOC'                                            
169501                 MOVE NEJ TO INDATA-SW                                    
169601                 MOVE INDX TO INDX-DISPLAY                                
169701                 MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                
169801                 STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE             
169901                        INTO RESP-IDELMT-ERROR                            
170001              END-IF                                                      
170101           END-IF                                                         
170201         END-IF                                                           
170301        IF REQU-CMD-IN(INDX) = 'PRL'                                      
170401           IF W-CMD  = SPACE                                              
170501              MOVE REQU-CMD-IN (INDX) TO W-CMD                            
170601           ELSE                                                           
170701              IF W-CMD = 'MIS'                                            
170801              OR W-CMD = 'RET'                                            
170901              OR W-CMD = 'LOS'                                            
171001              OR W-CMD = 'DCA'                                            
171101              OR W-CMD = 'CAN'                                            
171201              OR W-CMD = 'BIN'                                            
171301              OR W-CMD = 'HAC'                                            
171401              OR W-CMD = 'PR '                                            
171501              OR W-CMD = 'LBL'                                            
171601              OR W-CMD = 'REC'                                            
171701              OR W-CMD = 'LOC'                                            
171801                 MOVE NEJ TO INDATA-SW                                    
171901                 MOVE INDX TO INDX-DISPLAY                                
172001                 MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                
172101                 STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE             
172201                        INTO RESP-IDELMT-ERROR                            
172301              END-IF                                                      
172401           END-IF                                                         
172501         END-IF                                                           
172601                                                                          
172701        IF REQU-CMD-IN(INDX) = 'LBL'                                      
172801           IF W-CMD  = SPACE                                              
172901              MOVE REQU-CMD-IN (INDX) TO W-CMD                            
173001           ELSE                                                           
173101              IF W-CMD = 'MIS'                                            
173201              OR W-CMD = 'RET'                                            
173301              OR W-CMD = 'LOS'                                            
173401              OR W-CMD = 'DCA'                                            
173501              OR W-CMD = 'CAN'                                            
173601              OR W-CMD = 'BIN'                                            
173701              OR W-CMD = 'HAC'                                            
173801              OR W-CMD = 'PR '                                            
173901              OR W-CMD = 'PRL'                                            
174001              OR W-CMD = 'REC'                                            
174101              OR W-CMD = 'LOC'                                            
174201                 MOVE NEJ TO INDATA-SW                                    
174301                 MOVE INDX TO INDX-DISPLAY                                
174401                 MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)                
174501                 STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE             
174601                        INTO RESP-IDELMT-ERROR                            
174701              END-IF                                                      
174801           END-IF                                                         
174901         END-IF                                                           
175001                                                                          
175101         IF REQU-CMD-IN (INDX) = 'REC'                                    
175201            IF WS-FLTRACK = 'J'                                           
175301               PERFORM S02-SKAPA-WDL6A1KY                                 
175401               PERFORM IMS-04-GU-WLINLD01                                 
175501               IF SEGMENT-SAKNAS                                          
175601                  MOVE NEJ TO INDATA-SW                                   
175701                  MOVE INDX TO INDX-DISPLAY                               
175801                  MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX)               
175901                  STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE            
176001                            INTO RESP-IDELMT-ERROR                        
176101               ELSE                                                       
176201                  MOVE SEQA-IDARTNR   TO W-IDARTNR                        
176301                  MOVE SEQA-IDDC      TO W-IDDC                           
176401                  MOVE SEQA-DAINLEV   TO W-DAINLEV                        
176501                  PERFORM IMS-GU-WDL623                                   
176601                  IF SEGMENT-SAKNAS                                       
176701                     MOVE NEJ TO INDATA-SW                                
176801                                 IDTRACK-SW                               
176901                     MOVE INDX TO INDX-DISPLAY                            
177001                     MOVE 'REC' TO RESP-IDMSG-ERROR-LINE(INDX)            
177101                     MOVE TRACKING-ID-MISSING                             
177201                                TO RESP-IDMSG-ERROR                       
177301                  END-IF                                                  
177401               END-IF                                                     
177501            END-IF                                                        
177601         END-IF                                                           
177701                                                                          
177801         IF REQU-CMD-IN (INDX) = 'LOC'                                    
177901           IF REQU-ADINLOMR(INDX) = ALL '+'                               
178001             MOVE NEJ TO INDATA-SW                                        
178101             MOVE INDX TO INDX-DISPLAY                                    
178201             MOVE 'UNL' TO RESP-IDMSG-ERROR-LINE(INDX)                    
178301             STRING 'ADINLOMR-UL*' INDX-DISPLAY DELIMITED BY SIZE         
178401                        INTO RESP-IDELMT-ERROR                            
178501           ELSE                                                           
178601             IF REC-DCS-CDC                                               
178701               MOVE REQU-ADINLOMR(INDX)                                   
178801                           TO W-ADINLOMR-6006                             
178901               PERFORM IMS-GU-W6G130                                      
179001               IF SEGMENT-SAKNAS                                          
179101                 MOVE NEJ TO INDATA-SW                                    
179201                 MOVE INDX TO INDX-DISPLAY                                
179301                 STRING 'ADINLOMR-UL*' INDX-DISPLAY                       
179401                   DELIMITED BY SIZE INTO RESP-IDELMT-ERROR               
179501                 MOVE 'UNL' TO  RESP-IDMSG-ERROR-LINE(INDX)               
179601               END-IF                                                     
179701             END-IF                                                       
179801           END-IF                                                         
179901         END-IF                                                           
180001                                                                          
180101         IF REQU-CMD-IN(INDX) = 'BIN'                                     
180201            MOVE 'BIN' TO WS-BIN                                          
180301            MOVE INDX TO INDX-BIN                                         
180401         END-IF                                                           
180501                                                                          
180601** CHECK IF HAC WORKS AS SAME LIKE BIN TO CALL WL0103                     
180701         IF REQU-CMD-IN(INDX) = 'HAC'                                     
180801            MOVE 'HAC' TO WS-BIN                                          
180901            MOVE INDX TO INDX-BIN                                         
181001         END-IF                                                           
181101                                                                          
181201         IF REQU-CMD-IN(INDX) = 'REC'                                     
181301            MOVE 'REC' TO WS-REC                                          
181401            MOVE INDX TO INDX-REC                                         
181501         END-IF                                                           
181601                                                                          
181701         ADD 1            TO INDX                                         
181801     END-PERFORM                                                          
181901                                                                          
182001     IF WS-REC = SPACE                                                    
182101        CONTINUE                                                          
182201     ELSE                                                                 
182301        IF WS-BIN = SPACE                                                 
182401           CONTINUE                                                       
182501        ELSE                                                              
182601           MOVE NEJ TO INDATA-SW                                          
182701           MOVE INDX-BIN TO INDX-DISPLAY                                  
182801           MOVE 'CMD' TO RESP-IDMSG-ERROR-LINE(INDX-BIN)                  
182901           STRING 'CMD*' INDX-DISPLAY DELIMITED BY SIZE                   
183001                        INTO RESP-IDELMT-ERROR                            
183101        END-IF                                                            
183201     END-IF                                                               
183301                                                                          
183401     IF IDTRACK-FEL                                                       
183501        MOVE TRACKING-ID-MISSING TO RESP-IDMSG-ERROR                      
183601     ELSE                                                                 
183701        IF INDATA-FEL                                                     
183801           MOVE IS-INVALID TO RESP-IDMSG-ERROR                            
183901        END-IF                                                            
184001     END-IF                                                               
184101     .                                                                    
184201     EJECT                                                                
184301 H-UPPDATERA SECTION.                                                     
184401     MOVE 'H-UPPDATERA                   ' TO WS-SECTION                  
184501     MOVE ZERO TO W-KVANT-UPPD                                            
184601     MOVE NEJ TO ATERHOPP-SW                                              
184701                                                                          
184801     MOVE +1 TO BIN-IX                                                    
184901     PERFORM UNTIL BIN-IX > MAX-INDX                                      
185001        MOVE ALL '+' TO SEND-REQU-CMD-IN(BIN-IX)                          
185101        ADD +1 TO BIN-IX                                                  
185201     END-PERFORM                                                          
185301                                                                          
185401     MOVE +1 TO INDX                                                      
185501                BIN-IX                                                    
185601     PERFORM UNTIL INDX > MAX-INDX                                        
185701         IF REQU-CMD-IN (INDX) = 'MIS'                                    
185801             PERFORM HA-MIS-UPDATERING                                    
185901             MOVE ALL '+' TO REQU-CMD-IN (INDX)                           
186001         ELSE                                                             
186101             IF REQU-CMD-IN (INDX) = 'RET'                                
186201                 PERFORM HB-RET-UPDATERING                                
186301                 MOVE ALL '+' TO REQU-CMD-IN (INDX)                       
186401             ELSE                                                         
186501                 IF REQU-CMD-IN (INDX) = 'LOS'                            
186601                    IF REC-DCS-LAND-NON-VCC-OWNED                         
186701                    OR AKTUELLT-LAND-NA                                   
186801**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
186901**** REST OF FLOWS TO US IS IN LAB.                                       
187001**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
187101**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
187201                    OR (REC-DCS-USA                                       
187301                    AND DIST35-NDCCN-NDCUS-REFILL)                        
187401                      PERFORM HD-LOS-UPDATERING                           
187501                    ELSE                                                  
187601                      PERFORM HC-LOS-UPDATERING                           
187701                    END-IF                                                
187801                    MOVE ALL '+' TO REQU-CMD-IN (INDX)                    
187901                 ELSE                                                     
188001                    IF REQU-CMD-IN (INDX) = 'DCA'                         
188101* IF RECEIVED DCA COMMAND FROM APP AND THE CASE IS NOT RECEIVED           
188201                       IF DCA-NOT-RECEIVED                                
188301                        MOVE NEJ TO ETA-UPD-SW                            
188401                        MOVE WS-IDDC-SPAR TO W-6301-IDDC                  
188501                        PERFORM IMS-GU-WL630111                           
188601                        PERFORM HI-REC-UPDATERING                         
188701                        PERFORM HIB-KOLLA-IDFAKT                          
188801*                       IF ATERHOPP-SW = NEJ                              
188901*                         MOVE WS-IDDC-SPAR TO W-6301-IDDC                
189001*                         PERFORM IMS-01-GHU-WL630111                     
189101*                         IF SEGMENT-FINNS                                
189201*                           MOVE SPAR-KDTRPSTA TO                         
189301*                                6302-KDTRPSTA                            
189401*                           PERFORM IMS-02-REPL-WL630111                  
189501*                         END-IF                                          
189601*                       END-IF                                            
189701                       END-IF                                             
189801                       PERFORM HE-DCA-UPDATERING                          
189901                       MOVE ALL '+' TO REQU-CMD-IN (INDX)                 
190001                    ELSE                                                  
190101                       IF REQU-CMD-IN (INDX) = 'CAN'                      
190201                          PERFORM HF-CAN-UPDATERING                       
190301                          MOVE ALL '+' TO REQU-CMD-IN (INDX)              
190401                       ELSE                                               
190501                          IF REQU-CMD-IN (INDX) = 'BIN' OR 'HAC'          
190601                             PERFORM HH-BIN-UPDATERING                    
190701                             MOVE ALL '+' TO REQU-CMD-IN(INDX)            
190801                          ELSE                                            
190901                            IF REQU-CMD-IN (INDX) = 'REC'                 
191001                             IF DC-CROSS-FLAG = 'J'                       
191101                              PERFORM HM-REC-DCCROS                       
191201                             ELSE                                         
191301                               MOVE NEJ TO ETA-UPD-SW                     
191401                               MOVE WS-IDDC-SPAR TO W-6301-IDDC           
191501                               PERFORM IMS-GU-WL630111                    
191601                               IF 6302-KDTRPSTA NOT = 'R' AND             
191701                                  6302-KDTRPSTA NOT = 'M' AND             
191801                                  6302-KDTRPSTA NOT = 'L'                 
191901                                  PERFORM S18-CALL-W218ETA                
192001                               END-IF                                     
192101                               PERFORM HI-REC-UPDATERING                  
192201                               PERFORM HIB-KOLLA-IDFAKT                   
192301                             END-IF                                       
192401                             IF ATERHOPP-SW = NEJ                         
192501                                MOVE ALL '+' TO                           
192601                                     REQU-CMD-IN (INDX)                   
192701*                                 MOVE WS-IDDC-SPAR TO W-6301-IDDC        
192801*                                 PERFORM IMS-01-GHU-WL630111             
192901*                                 IF SEGMENT-FINNS                        
193001*                                   MOVE SPAR-KDTRPSTA TO                 
193101*                                       6302-KDTRPSTA                     
193201*                                   PERFORM IMS-02-REPL-WL630111          
193301*                                 END-IF                                  
193401                             END-IF                                       
193501                            ELSE                                          
193601                               IF REQU-CMD-IN (INDX) = 'LOC'              
193701                                 PERFORM HL-LOC-UPPDATERING               
193801                                 MOVE ALL '+' TO REQU-CMD-IN(INDX)        
193901                               END-IF                                     
194001                            END-IF                                        
194101                          END-IF                                          
194201                       END-IF                                             
194301                    END-IF                                                
194401                 END-IF                                                   
194501             END-IF                                                       
194601         END-IF                                                           
194701         ADD 1 TO INDX                                                    
194801     END-PERFORM                                                          
194901                                                                          
195001     IF  TRANS-OHUVUD-RET-SKAPAD                                          
195101         MOVE 'J' TO ORAD-MID-FLSLUT                                      
195201         PERFORM X010-CALL-W006KOM                                        
195301     END-IF                                                               
195401                                                                          
195501     IF W-CMD = 'PR ' OR 'PRL'                                            
195601        MOVE JA                  TO PRINT-SW                              
195701        PERFORM S90-SEND-OPEN                                             
195801        PERFORM S96-SKAPA-HEADER                                          
195901                                                                          
196001        MOVE +1 TO INDX                                                   
196101        PERFORM UNTIL INDX > MAX-INDX                                     
196201           IF REQU-CMD-IN(INDX) = 'PR ' OR 'PRL'                          
196301              PERFORM HG-SKAPA-DP-RAPPORT                                 
196401           END-IF                                                         
196501           ADD +1 TO INDX                                                 
196601        END-PERFORM                                                       
196701     ELSE                                                                 
196801        IF W-CMD = 'LBL' AND REQU-IDDC-KEY(1:1) = '7'                     
196901          MOVE JA                  TO PRINT-SW                            
197001          MOVE +1 TO INDX                                                 
197101          MOVE 1 TO INDX-LINE                                             
197201          PERFORM UNTIL INDX > MAX-INDX                                   
197301             IF REQU-CMD-IN(INDX) = 'LBL'                                 
197401                PERFORM HJ-CREATE-CHINESE-LABELS                          
197501             END-IF                                                       
197601             ADD +1 TO INDX                                               
197701          END-PERFORM                                                     
197801*         CALL W612LABL TO CREATE CHINESE LABLES                          
197901          CALL W612LABL   USING LABL-W612LABL                             
198001                                REQU-WZ01REQ2                             
198101                                DISTRDOC-PCB                              
198201                                WDD3-PCB                                  
198301                                WDT4-PCB                                  
198401                                LABL-WDB6-PCB                             
198501        ELSE                                                              
198601          IF W-CMD = 'LBL'                                                
198701             MOVE JA                  TO PRINT-SW                         
198801             MOVE +1 TO INDX                                              
198901             MOVE +1 TO INDX-LINE                                         
199001             PERFORM UNTIL INDX > MAX-INDX                                
199101                IF REQU-CMD-IN(INDX) = 'LBL'                              
199201                   PERFORM HK-CREATE-KOREAN-LABELS                        
199301                END-IF                                                    
199401                ADD +1 TO INDX                                            
199501             END-PERFORM                                                  
199601*         CALL W612KLBL TO CREATE KOREAN LABLES                           
199701          CALL W612KLBL   USING KLBL-W612KLBL                             
199801                                REQU-WZ01REQ2                             
199901                                DISTRDOC-PCB                              
200001                                WDD3-PCB                                  
200101                                WDT4-PCB                                  
200201          END-IF                                                          
200301        END-IF                                                            
200401     END-IF                                                               
200501                                                                          
200601     IF HOPP-UPDATE                                                       
200701        CONTINUE                                                          
200801     ELSE                                                                 
200901        IF W-CMD = 'PR ' OR 'PRL' OR 'LBL'                                
201001          IF REC-DCS-CDC                                                  
201101            CONTINUE                                                      
201201          ELSE                                                            
201301            MOVE PRINTING-REQUESTED TO RESP-IDMSG-INFO                    
201401          END-IF                                                          
201501        ELSE                                                              
201601          IF DC-CROSS-FLAG = 'N'                                          
201701           MOVE UPDATE-DONE        TO RESP-IDMSG-INFO                     
201801          END-IF                                                          
201901        END-IF                                                            
202001     END-IF                                                               
202101     .                                                                    
202201     EJECT                                                                
202301 HA-MIS-UPDATERING SECTION.                                               
202401     MOVE 'HA-MIS-UPDATERING             ' TO WS-SECTION                  
202501                                                                          
202601     PERFORM S02-SKAPA-WDL6A1KY                                           
202701     PERFORM IMS-04-GU-WLINLD01                                           
202801                                                                          
202901     PERFORM UNTIL SEGMENT-SAKNAS                                         
203001                                                                          
203101         MOVE SEQA-IDARTNR   TO W-IDARTNR                                 
203201         MOVE SEQA-DAINLEV   TO W-DAINLEV                                 
203301         PERFORM IMS-09-GHU-INLC1-WLINLC11                                
203401                                                                          
203501         MOVE INL-IDDC       TO W-IDDC                                    
203601         MOVE INL-KVAVIS     TO W-KVAVIS                                  
203701                                                                          
203801         MOVE 'R30'          TO INL-IDPTYP                                
203901         MOVE WS-IDUSER-003  TO INL-IDUSER-003                            
204001         PERFORM IMS-10-REPL-INLC1-WLINLC11                               
204101                                                                          
204201         IF REC-DCS-CDC                                                   
204301           PERFORM IMS-GHU-WDK611                                         
204401           ADD W-KVAVIS          TO CLAG-KVAKS-PAV                        
204501           SUBTRACT W-KVAVIS   FROM CLAG-KVAKS-CDC                        
204601           PERFORM IMS-REPL-WDK611                                        
204701         ELSE                                                             
204801           PERFORM IMS-12-GHU-WDK711                                      
204901           ADD W-KVAVIS        TO SLAG-KVAKS-PAV                          
205001           SUBTRACT W-KVAVIS   FROM SLAG-KVAKS-SDC                        
205101           PERFORM IMS-13-REPL-WDK711                                     
205201         END-IF                                                           
205301                                                                          
205401         MOVE '-'     TO LOGG-IDTECKEN-KVAKS                              
205501         MOVE '+'     TO LOGG-IDTECKEN-KVAKS-PAV                          
205601         MOVE ' '     TO LOGG-IDTECKEN-KVLS                               
205701         PERFORM S06-SKAPA-SALDOLOGG                                      
205801                                                                          
205901         PERFORM IMS-05-GN-WLINLD01                                       
206001     END-PERFORM                                                          
206101                                                                          
206201     MOVE WS-IDDC-SPAR TO W-6301-IDDC                                     
206301     PERFORM IMS-01-GHU-WL630111                                          
206401     IF SEGMENT-FINNS                                                     
206501        MOVE 'M' TO 6302-KDTRPSTA                                         
206601        PERFORM IMS-02-REPL-WL630111                                      
206701     END-IF                                                               
206801     .                                                                    
206901     EJECT                                                                
207001 HB-RET-UPDATERING SECTION.                                               
207101     MOVE 'HB-RET-UPDATERING             ' TO WS-SECTION                  
207201                                                                          
207301     PERFORM S02-SKAPA-WDL6A1KY                                           
207401     PERFORM IMS-04-GU-WLINLD01                                           
207501     MOVE ZERO TO WS-RAKNARE                                              
207601                                                                          
207701     IF  SEGMENT-FINNS                                                    
207801         IF  NOT TRANS-OHUVUD-RET-SKAPAD                                  
207901             PERFORM HBA-SKAPA-TRANS-ORDERHUVUD                           
208001             PERFORM HBB-SKAPA-HUVUD-ORDERRADER                           
208101             MOVE 1           TO ORAD-IX                                  
208201         END-IF                                                           
208301                                                                          
208401         PERFORM UNTIL SEGMENT-SAKNAS                                     
208501             PERFORM UNTIL SEGMENT-SAKNAS                                 
208601                        OR ORAD-IX > ORAD-IX-MAX                          
208701                                                                          
208801                 MOVE SEQA-IDARTNR   TO W-IDARTNR                         
208901                 MOVE SEQA-DAINLEV   TO W-DAINLEV                         
209001                 PERFORM IMS-09-GHU-INLC1-WLINLC11                        
209101                 MOVE INL-IDDC   TO W-IDDC                                
209201                 MOVE INL-KVAVIS TO W-KVAVIS                              
209301                                    INL-KVANTMOT                          
209401                                                                          
209501                 MOVE 'R32'       TO INL-IDPTYP                           
209601                 MOVE WS-IDUSER-003 TO INL-IDUSER-003                     
209701                 ADD +1           TO WS-RAKNARE                           
209801                 ACCEPT W-DATUM-X FROM DATE                               
209901                 MOVE W-DATUM       TO INL-TIINLINL                       
210001                 PERFORM IMS-10-REPL-INLC1-WLINLC11                       
210101                 IF REC-DCS-CDC                                           
210201                   PERFORM IMS-GHU-WDK611                                 
210301                   ADD W-KVAVIS  TO CLAG-KVLS                             
210401                   SUBTRACT W-KVAVIS                                      
210501                               FROM CLAG-KVAKS-CDC                        
210601                   PERFORM IMS-REPL-WDK611                                
210701                 ELSE                                                     
210801                   PERFORM IMS-12-GHU-WDK711                              
210901                   ADD W-KVAVIS  TO SLAG-KVLS                             
211001                   SUBTRACT W-KVAVIS FROM SLAG-KVAKS-SDC                  
211101                   PERFORM IMS-13-REPL-WDK711                             
211201                 END-IF                                                   
211301                 MOVE '-'    TO LOGG-IDTECKEN-KVAKS                       
211401                 MOVE ' '    TO LOGG-IDTECKEN-KVAKS-PAV                   
211501                 MOVE '+'    TO LOGG-IDTECKEN-KVLS                        
211601                 PERFORM S06-SKAPA-SALDOLOGG                              
211701                                                                          
211801                 PERFORM HBC-EDIT-TRANS-ORDERRADER                        
211901                                                                          
212001                 ADD +1   TO ORAD-IX                                      
212101                 PERFORM IMS-05-GN-WLINLD01                               
212201                                                                          
212301             END-PERFORM                                                  
212401                                                                          
212501             IF  ORAD-IX  > ORAD-IX-MAX                                   
212601             AND SEGMENT-FINNS                                            
212701                 PERFORM X010-CALL-W006KOM                                
212801                                                                          
212901                 MOVE +1  TO ORAD-IX                                      
213001                 PERFORM UNTIL ORAD-IX > ORAD-IX-MAX                      
213101                     MOVE SPACE TO ORAD-MID-RADER (ORAD-IX)               
213201                     ADD +1     TO ORAD-IX                                
213301                 END-PERFORM                                              
213401                 MOVE +1  TO ORAD-IX                                      
213501             END-IF                                                       
213601         END-PERFORM                                                      
213701                                                                          
213801         PERFORM X020-UPPDATERA-WL630111                                  
213901     END-IF                                                               
214001     .                                                                    
214101     EJECT                                                                
214201 HBA-SKAPA-TRANS-ORDERHUVUD SECTION.                                      
214301     MOVE 'HBA-SKAPA-TRANS-ORDERHUVUD    ' TO WS-SECTION                  
214401                                                                          
214501     MOVE JA              TO TRANS-OHUVUD-RET-SKAPAD-SW                   
214601                                                                          
214701     MOVE SPACE           TO MSG-KOM-WMSGKOM                              
214801     MOVE +54             TO MSG-KOM-KVLL                                 
214901     MOVE LOW-VALUE       TO MSG-KOM-KDZ1                                 
215001     MOVE LOW-VALUE       TO MSG-KOM-KDZ2                                 
215101     MOVE SPACE           TO MSG-KOM-KDTRANS                              
215201     MOVE 'W4I25101'      TO MSG-KOM-IDCPYTXT                             
215301     MOVE 'SDC-RET '      TO MSG-KOM-IDSNDNOD                             
215401     MOVE 'WL010200'      TO MSG-KOM-IDSNDJOB                             
215501                                                                          
215601     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
215701     ACCEPT MSG-KOM-TIKLOCK FROM TIME                                     
215801                                                                          
215901     MOVE SPACE           TO MSG-KOM-IDMFSMED                             
216001                             MSG-KOM-KDSVAR                               
216101                                                                          
216201     MOVE SPACE           TO KOM-AREA                                     
216301                                                                          
216401     COMPUTE P-TO-P-LL =  LNG-P-TO-P-PREFIX +                             
216501                          LENGTH OF OHUV-MID-W4I25101                     
216601                                                                          
216701     MOVE LOW-VALUE              TO P-TO-P-Z1                             
216801     MOVE LOW-VALUE              TO P-TO-P-Z2                             
216901     MOVE 'W4T251X'              TO P-TO-P-TRANSKOD                       
217001     MOVE '4251'                 TO P-TO-P-FROM-MID                       
217101     MOVE '2'                    TO P-TO-P-KDMFSFOR                       
217201                                                                          
217301     MOVE 'WL01'                 TO OHUV-MID-IDSYSTEM                     
217401     MOVE W-IDDISTR-RETUR        TO OHUV-MID-IDDISTR                      
217501     MOVE W-IDKUNDNR-RETUR       TO OHUV-MID-IDKUNDNR                     
217601                                                                          
217701     ACCEPT W-DATUM-X FROM DATE                                           
217801     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
217901     MOVE W-DATUM        TO DAT-I-TIDATUM                                 
218001     CALL WDATKONV    USING DAT-KDDATFORM                                 
218101                            DAT-I-TIDATUM                                 
218201                            DAT-O-TIDATUM                                 
218301                            DAT-KDSVAR                                    
218401                                                                          
218501     IF  DAT-KDSVAR   NOT = SPACE                                         
218601         MOVE ' FELAKTIG RETURKOD FRÅN WDATKONV I HBA-'                   
218701                          TO FELTEXT                                      
218801         CALL FELLOG                                                      
218901     END-IF                                                               
219001                                                                          
219101     MOVE DAT-TIVV        TO W-IDORDNR-VV                                 
219201     MOVE DAT-TID         TO W-IDORDNR-D                                  
219301     ACCEPT W-TIME-X      FROM TIME                                       
219401                                                                          
219501     MOVE W-TIME-SS       TO W-IDORDNR-SS                                 
219601                                                                          
219701     MOVE W-IDORDNR-X     TO OHUV-MID-IDORDNR                             
219801     MOVE '1'             TO OHUV-MID-KDORDKL                             
219901                                                                          
220001     MOVE SPACE           TO OHUV-MID-KDFRAKT                             
220101                             OHUV-MID-TIRFS                               
220201     MOVE 'DAMAGED'       TO OHUV-MID-BEKUNDRF                            
220301     MOVE SPACE           TO OHUV-MID-KDFAKTYP                            
220401     MOVE NEJ             TO OHUV-MID-FLRESTN                             
220501     MOVE SPACE           TO OHUV-MID-KDTPOTYP                            
220601                             OHUV-MID-TITPO                               
220701                             OHUV-MID-BELAGINS                            
220801                             OHUV-MID-BEGMT                               
220901                             OHUV-MID-ADGMT-GATA                          
221001                             OHUV-MID-ADGMT-PADR                          
221101                             OHUV-MID-KDROPACK                            
221201                             OHUV-MID-IDKONTO                             
221301                             OHUV-MID-IDKST                               
221401                             OHUV-MID-BEVARREF                            
221501                             OHUV-MID-KDTULLVE                            
221601                             OHUV-MID-KDNOTES                             
221701     MOVE JA              TO OHUV-MID-FLAUTFAK                            
221801     MOVE JA              TO OHUV-MID-FLAUTPAC                            
221901     MOVE NEJ             TO OHUV-MID-FLEMBORD                            
222001     MOVE NEJ             TO OHUV-MID-FLOVRLEV                            
222101     MOVE SPACE           TO OHUV-MID-IDKAMPRF                            
222201                             OHUV-MID-IDFTG                               
222301                             OHUV-MID-ADBET                               
222401                             OHUV-MID-BEBET                               
222501                             OHUV-MID-IDSKYLT                             
222601                             OHUV-MID-FLLSBOK                             
222701                             OHUV-MID-IDANALYS                            
222801                             OHUV-MID-IDBILREG                            
222901                             OHUV-MID-IDVIN                               
223001                             OHUV-MID-IDCISNR                             
223101     MOVE WS-IDDC-SPAR    TO OHUV-MID-IDDC                                
223201     MOVE ZERO            TO OHUV-MID-IDDEPT                              
223301     MOVE SPACE           TO OHUV-MID-KDORDTYP-LDC                        
223401     MOVE ZERO            TO OHUV-MID-TIREPDAT                            
223501     MOVE NEJ             TO OHUV-MID-FLFORBI                             
223601                             OHUV-MID-FLORDTIL                            
223701     MOVE ZERO            TO OHUV-MID-IDGROSS                             
223801                                                                          
223901     PERFORM X010-CALL-W006KOM                                            
224001     .                                                                    
224101     EJECT                                                                
224201 HBB-SKAPA-HUVUD-ORDERRADER SECTION.                                      
224301     MOVE 'HBB-SKAPA-HUVUD-ORDERRADER    ' TO WS-SECTION                  
224401                                                                          
224501     COMPUTE P-TO-P-LL =  LNG-P-TO-P-PREFIX +                             
224601                          LENGTH OF ORAD-MID-W4I25201                     
224701                                                                          
224801     MOVE LOW-VALUE       TO P-TO-P-Z1                                    
224901     MOVE LOW-VALUE       TO P-TO-P-Z2                                    
225001     MOVE 'W4T252X'       TO P-TO-P-TRANSKOD                              
225101     MOVE '4252'          TO P-TO-P-FROM-MID                              
225201     MOVE '2'             TO P-TO-P-KDMFSFOR                              
225301                                                                          
225401     MOVE SPACE           TO KOM-AREA                                     
225501     MOVE 'WL01'           TO ORAD-MID-IDSYSTEM                           
225601     MOVE W-IDDISTR-RETUR  TO ORAD-MID-IDDISTR                            
225701     MOVE W-IDKUNDNR-RETUR TO ORAD-MID-IDKUNDNR                           
225801     MOVE W-IDORDNR-X      TO ORAD-MID-IDORDNR                            
225901     MOVE 'DAMAGED'        TO ORAD-MID-BEVOLREF                           
226001     MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                        
226101     MOVE 'N'              TO ORAD-MID-FLSLUT                             
226201     .                                                                    
226301     EJECT                                                                
226401 HBC-EDIT-TRANS-ORDERRADER SECTION.                                       
226501     MOVE 'HBC-EDIT-TRANS-ORDERRADER     ' TO WS-SECTION                  
226601                                                                          
226701     MOVE W-IDARTNR       TO ORAD-MID-IDARTNR   (ORAD-IX)                 
226801                             REK-IDARTNR                                  
226901     MOVE 9               TO REK-LNGD                                     
227001     MOVE 0               TO REK-REKSIFFR                                 
227101                                                                          
227201     CALL W009KSIF        USING REK-IDARTNR                               
227301                                REK-LNGD                                  
227401                                REK-REKSIFFR                              
227501                                                                          
227601     MOVE REK-REKSIFFR    TO ORAD-MID-REKSIFFR  (ORAD-IX)                 
227701     MOVE W-KVAVIS        TO W-KVAVIS-6                                   
227801     MOVE W-KVAVIS-6-X    TO ORAD-MID-KVBEART   (ORAD-IX)                 
227901     MOVE SPACE           TO ORAD-MID-PRARTNTO  (ORAD-IX)                 
228001                             ORAD-MID-TITPO     (ORAD-IX)                 
228101                             ORAD-MID-FLRESTN   (ORAD-IX)                 
228201                             ORAD-MID-KDKVBRYT  (ORAD-IX)                 
228301                             ORAD-MID-FLINVEST  (ORAD-IX)                 
228401     MOVE ZERO            TO ORAD-MID-KDVRINFO  (ORAD-IX)                 
228501     MOVE SPACE           TO ORAD-MID-IDKONTO   (ORAD-IX)                 
228601                             ORAD-MID-IDKST     (ORAD-IX)                 
228701                             ORAD-MID-BERADREF  (ORAD-IX)                 
228801                             ORAD-MID-KDDSP     (ORAD-IX)                 
228901                             ORAD-MID-IDBIL     (ORAD-IX)                 
229001     MOVE NEJ             TO ORAD-MID-FLSLATT   (ORAD-IX)                 
229101                             ORAD-MID-FLDIRLEV  (ORAD-IX)                 
229201     MOVE SPACE        TO ORAD-MID-PRARTNTO-LOC (ORAD-IX)                 
229301     MOVE SPACE        TO ORAD-MID-PRARTBTO-LOC (ORAD-IX)                 
229401     MOVE SPACE        TO     ORAD-MID-KDVALISO (ORAD-IX)                 
229501     MOVE SPACE        TO     ORAD-MID-KDVAT    (ORAD-IX)                 
229601     MOVE 0            TO     ORAD-MID-RERAB    (ORAD-IX)                 
229701     MOVE SPACE        TO     ORAD-MID-KDRAB    (ORAD-IX)                 
229801     MOVE SPACE        TO ORAD-MID-BEART-VIPS   (ORAD-IX)                 
229901     MOVE ZERO         TO ORAD-MID-ADLAGOMR-CD  (ORAD-IX)                 
230001                          ORAD-MID-ADGANG-CD    (ORAD-IX)                 
230101                          ORAD-MID-ADPLATS-CD   (ORAD-IX)                 
230201     MOVE SPACE        TO ORAD-MID-IDKUNDRF-WIP (ORAD-IX)                 
230301     .                                                                    
230401     EJECT                                                                
230501 HC-LOS-UPDATERING SECTION.                                               
230601     MOVE 'HC-LOS-UPDATERING             ' TO WS-SECTION                  
230701                                                                          
230801     PERFORM S02-SKAPA-WDL6A1KY                                           
230901     PERFORM IMS-04-GU-WLINLD01                                           
231001     MOVE ZERO TO WS-RAKNARE                                              
231101                                                                          
231201     MOVE WS-IDDC-SPAR TO W-6301-IDDC                                     
231301     PERFORM IMS-01-GHU-WL630111                                          
231401     MOVE 6302-IDDC-SEND TO WS-IDDC-SEND                                  
231402     MOVE 6302-IDDISTR TO DIST35-IDDISTR                                  
231501                                                                          
231601     PERFORM UNTIL SEGMENT-SAKNAS                                         
231701         MOVE SEQA-IDARTNR   TO W-IDARTNR                                 
231801         MOVE SEQA-DAINLEV   TO W-DAINLEV                                 
231901         PERFORM IMS-09-GHU-INLC1-WLINLC11                                
232001         MOVE INL-KVAVIS     TO W-KVAVIS                                  
232101         MOVE INL-IDDC       TO W-IDDC                                    
232201         MOVE INL-IDDISTR    TO WS-SAP-IDDISTR                            
232301         MOVE INL-IDKUNDNR   TO WS-SAP-IDKUNDNR                           
232401                                                                          
232501         MOVE 'R32'          TO INL-IDPTYP                                
232601         MOVE WS-IDUSER-003  TO INL-IDUSER-003                            
232701         ADD +1              TO WS-RAKNARE                                
232801         PERFORM IMS-10-REPL-INLC1-WLINLC11                               
232901         IF REC-DCS-CDC                                                   
233001           PERFORM IMS-GHU-WDK611                                         
233101           SUBTRACT W-KVAVIS     FROM CLAG-KVAKS-PAV                      
233201           PERFORM IMS-REPL-WDK611                                        
233301         ELSE                                                             
233401           PERFORM IMS-12-GHU-WDK711                                      
233501           SUBTRACT W-KVAVIS FROM SLAG-KVAKS-PAV                          
233601           PERFORM IMS-13-REPL-WDK711                                     
233701         END-IF                                                           
233801         MOVE ' '    TO LOGG-IDTECKEN-KVAKS                               
233901         MOVE '-'    TO LOGG-IDTECKEN-KVAKS-PAV                           
234001         MOVE ' '    TO LOGG-IDTECKEN-KVLS                                
234101         PERFORM S06-SKAPA-SALDOLOGG                                      
234201                                                                          
234301         PERFORM IMS-15-GET-WLARTC01                                      
234401         MOVE ART-KDPRODSL  TO EKH-KDPRODSL                               
234501         MOVE ART-KDSORT    TO WS-KDSORT                                  
234601         PERFORM IMS-16-GET-WLARTC11                                      
234701         MOVE CLAG-PRARTSTD TO EKH-PRARTSTD                               
234801         PERFORM S07C-SKAPA-SAP-TRANS                                     
234901         MOVE WS-IDDC-SEND TO SEND-WS-IDDC                                
235001         IF DCS-IDDC NOT = SEND-WS-IDDC                                   
235101            MOVE SEND-WS-IDDC TO W-IDDC-B6                                
235201            PERFORM IMS-22-GU-WDB601                                      
235301         END-IF                                                           
235401         IF DCS-DDC                                                       
235501           PERFORM S08-SKAPA-AVVIK-TRANS                                  
235601         END-IF                                                           
235701                                                                          
235801****     IF DCS-CDC                                                       
235901           PERFORM S09-SKAPA-LDC-TRANS                                    
236001****     END-IF                                                           
236101                                                                          
236201         PERFORM IMS-05-GN-WLINLD01                                       
236301     END-PERFORM                                                          
236401                                                                          
236501     PERFORM X020-UPPDATERA-WL630111                                      
236601     IF WS-FAKT-INFO-DLET = JA                                            
236701        PERFORM HDD-FAKTURA-SLUTBEHANDLAD                                 
236801     END-IF                                                               
236901     .                                                                    
237001     EJECT                                                                
237101 HD-LOS-UPDATERING SECTION.                                               
237201                                                                          
237301     MOVE 'HD-LOS-UPDATERING             ' TO WS-SECTION                  
237401                                                                          
237501     MOVE NEJ TO WS-A03-SKAPAD                                            
237601     MOVE ZERO TO WS-RAKNARE                                              
237701                                                                          
237801     MOVE WS-IDDC-SPAR  TO W-6301-IDDC                                    
237901     PERFORM IMS-GHU-WL630111                                             
238001     MOVE 6302-IDDC-SEND TO WS-IDDC-SEND                                  
238101                            SEND-WS-IDDC                                  
238201     MOVE 6302-IDDISTR   TO WS-SAP-IDDISTR                                
238301     MOVE 6302-IDKUNDNR  TO WS-SAP-IDKUNDNR                               
238401     MOVE 6302-IDDISTR   TO DIST35-IDDISTR                                
238501                                                                          
238601     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
238701        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
238801        PERFORM IMS-GU-WDB601-SEND                                        
238901     END-IF                                                               
239001                                                                          
239101     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
239201       IF REC-DCS-LAND-NON-VCC-OWNED                                      
239301**** FOR NONVCC BOUNCE FLOW                                               
239401**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
239501**** REST OF FLOWS TO US IS IN LAB.                                       
239601**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
239701**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
239801       OR (REC-DCS-USA AND DIST35-NDCCN-NDCUS-REFILL)                     
239901          PERFORM HDA-LOS-CDC-NDC                                         
240001       ELSE                                                               
240101         IF AKTUELLT-LAND-NA                                              
240201            PERFORM HDC-LOS-CDC-NDC-NA                                    
240301         END-IF                                                           
240401       END-IF                                                             
240501     ELSE                                                                 
240601        IF WS-IDDC-SPAR NOT = SPAR-DCS-IDDC                               
240701           MOVE WS-IDDC-SPAR TO W-IDDC-B6                                 
240801           PERFORM IMS-GU-WDB601-SPAR                                     
240901        END-IF                                                            
241001        IF REC-DCS-LAND-NON-VCC-OWNED                                     
241101           PERFORM HDB-LOS-VCC-NONVCC                                     
241201        ELSE                                                              
241301          IF AKTUELLT-LAND-NA                                             
241401             IF SEND-DCS-NDC-NA AND SEND-DCS-USA AND                      
241501                SPAR-DCS-NDC-NA AND SPAR-DCS-USA                          
241601                PERFORM HDE-LOS-USA-USA                                   
241701             ELSE                                                         
241801               IF (SEND-DCS-NDC-NA AND SEND-DCS-CANADA AND                
241901                   SPAR-DCS-NDC-NA AND SPAR-DCS-USA)                      
242001               OR (SEND-DCS-NDC-NA AND SEND-DCS-USA    AND                
242101                   SPAR-DCS-NDC-NA AND SPAR-DCS-CANADA)                   
242201                   PERFORM HDF-LOS-USA-CANADA                             
242301               END-IF                                                     
242401             END-IF                                                       
242501          END-IF                                                          
242601        END-IF                                                            
242701     END-IF                                                               
242801                                                                          
242901     PERFORM X020-UPPDATERA-WL630111                                      
243001     IF WS-FAKT-INFO-DLET = JA                                            
243101        PERFORM HDD-FAKTURA-SLUTBEHANDLAD                                 
243201     END-IF                                                               
243301     IF WS-A03-SKAPAD = JA                                                
243401        IF WS-FAKT-INFO-DLET = 'J'                                        
243501           MOVE 'Y' TO EKOTRA03-FLSLUT                                    
243601        END-IF                                                            
243701        PERFORM S041-SKRIV-EKOTRANS-A03                                   
243801     END-IF                                                               
243901     .                                                                    
244001     EJECT                                                                
244101 HDA-LOS-CDC-NDC SECTION.                                                 
244201                                                                          
244301     MOVE 'HDA-LOS-CDC-NDC                ' TO WS-SECTION                 
244401                                                                          
244501* SENDING CDC                                                             
244601     PERFORM S02-SKAPA-WDL6A1KY                                           
244701     PERFORM IMS-GU-WLINLD01                                              
244801                                                                          
244901     PERFORM UNTIL SEGMENT-SAKNAS                                         
245001        MOVE SEQA-IDARTNR      TO W-IDARTNR                               
245101                                  SPAR-6308-IDARTNR                       
245201        MOVE SEQA-DAINLEV      TO W-DAINLEV                               
245301        PERFORM IMS-GHU-INLC1-WLINLC11                                    
245401                                                                          
245501        MOVE INL-KVAVIS        TO W-KVAVIS                                
245601        MOVE INL-IDDC          TO W-IDDC                                  
245701        MOVE INL-KDFRAKT       TO W-KDFRAKT                               
245801                                                                          
245901        MOVE 6302-IDFAKT       TO SPAR-6306-IDFAKT                        
246001        MOVE 'N'               TO SPAR-6306-FLDIRLEV                      
246101        MOVE ZERO              TO SPAR-6308-IDRAPPNR                      
246201                                  SPAR-6308-IDRADNR                       
246301                                  SPAR-6308-KDEMBLEV                      
246401        ACCEPT W-DATUM-X FROM DATE                                        
246501        MOVE W-DATUM           TO SPAR-6308-TILEVANM                      
246601        MOVE 6302-TIFAKT       TO SPAR-6306-TIFAKT                        
246701        MOVE 6302-TIFAKT       TO WS-SEKEL-KOLL                           
246801        MOVE  WS-IDDC-SPAR     TO SPAR-6306-IDDC-REC                      
246901        MOVE 6302-IDDC-SEND    TO SPAR-6306-IDDC-SEND                     
247001        MOVE 6302-IDDC-LEV     TO SPAR-6306-IDDC-LEV                      
247101        MOVE INL-IDDISTR       TO SPAR-6308-IDDISTR                       
247201                                  WS-SAP-IDDISTR                          
247301                                  W-IDDISTR-WDB3                          
247401                                  W-IDDISTR-WDB3-DEF                      
247501        MOVE INL-IDKUNDNR      TO SPAR-6308-IDKUNDNR                      
247601                                  WS-SAP-IDKUNDNR                         
247701        MOVE INL-KDFRAKT       TO SPAR-6308-KDFRAKT                       
247801        MOVE INL-IDKUNDRF      TO SPAR-6308-IDKUNDRF                      
247901                                                                          
248001**- ÄNDRING 2004-10 FÖR ATT KLARA LOCAL-CURRENCY NA.INL-PRARTNTO          
248101**- KOMMER FRÅN BILL-IT OCH ÄR I LOCAL VALUTA.                            
248201        IF DIST79-DEALER-PRICE                                            
248301          MOVE INL-PRARTNTO    TO SPAR-6308-PRARTBTO-LOC                  
248401          MOVE ZERO            TO SPAR-6308-PRARTBTO                      
248501          MOVE INL-KDVALISO    TO SPAR-6308-KDVALISO                      
248601        ELSE                                                              
248701          MOVE INL-PRARTNTO    TO SPAR-6308-PRARTBTO                      
248801          MOVE ZERO            TO SPAR-6308-PRARTBTO-LOC                  
248901          MOVE 'SEK'           TO SPAR-6308-KDVALISO                      
249001        END-IF                                                            
249101                                                                          
249201        MOVE INL-IDKOLLI       TO SPAR-6308-IDKOLLI                       
249301        MOVE INL-KVAVIS        TO SPAR-6308-KVLEVANM                      
249401        MOVE '60'              TO SPAR-6308-KDANMORS                      
249501                                                                          
249601        PERFORM IMS-GET-WLARTC01                                          
249701        MOVE ART-KDPRODSL      TO R8-EKH-KDPRODSL                         
249801        MOVE ART-KDSORT        TO WS-KDSORT                               
249901        PERFORM IMS-GET-WLARTC11                                          
250001        PERFORM S05-SOEK-REMARKUP                                         
250101                                                                          
250201        MOVE 'R32'         TO INL-IDPTYP                                  
250301        MOVE WS-IDUSER-003 TO INL-IDUSER-003                              
250401        MOVE ZERO          TO INL-TIINLINL                                
250501                              INL-KVANTMOT                                
250601        ADD +1 TO WS-RAKNARE                                              
250701        PERFORM IMS-REPL-INLC1-WLINLC11                                   
250801                                                                          
250901        PERFORM HDAB-KOLLA-FRAKTKOD                                       
251001                                                                          
251101        PERFORM S98-SKAPA-LEVANM-TRANS                                    
251201                                                                          
251301        PERFORM IMS-12-GHU-WDK711                                         
251401        SUBTRACT W-KVAVIS FROM SLAG-KVAKS-PAV                             
251501        MOVE SLAG-PRAVCOST TO WS-PRAVCOST                                 
251601                                                                          
251701        PERFORM IMS-13-REPL-WDK711                                        
251801                                                                          
251901        MOVE ' '    TO LOGG-IDTECKEN-KVAKS                                
252001        MOVE '-'    TO LOGG-IDTECKEN-KVAKS-PAV                            
252101        MOVE ' '    TO LOGG-IDTECKEN-KVLS                                 
252201        PERFORM S06-SKAPA-SALDOLOGG                                       
252301                                                                          
252401        IF REC-DCS-KDTRADP = 'BR12'                                       
252501          MOVE 'L' TO REQU-KDCMD                                          
252601          MOVE YES TO REQU-FLLOS(INDX)                                    
252701        ELSE                                                              
252801          PERFORM S07A-SKAPA-SAP-TRANS                                    
252901        END-IF                                                            
253001        MOVE SPAR-FLINLREP TO FILC2-FLINLREP                              
253101        PERFORM S09-SKAPA-LDC-TRANS                                       
253201        PERFORM IMS-GN-WLINLD01                                           
253301     END-PERFORM                                                          
253401     .                                                                    
253501     EJECT                                                                
253601 HDAB-KOLLA-FRAKTKOD SECTION.                                             
253701                                                                          
253801     MOVE NEJ TO WS-FLYGORDER                                             
253901                 WS-BAATORDER                                             
254001                                                                          
254101     MOVE 6302-IDDC-SEND      TO W-IDDC-WDB3                              
254201                                 W-IDDC-WDB3-DEF                          
254301     MOVE 6302-IDKUNDNR       TO W-IDKUNDNR-WDB3                          
254401                                                                          
254501     PERFORM IMS-GU-WDB301                                                
254601     IF SEGMENT-FINNS                                                     
254701        IF W-KDFRAKT = DC-KDGENFRA-VOR                                    
254801           MOVE JA TO WS-FLYGORDER                                        
254901        ELSE                                                              
255001           IF W-KDFRAKT = DC-KDGENFRA-MO                                  
255101              MOVE JA TO WS-BAATORDER                                     
255201           END-IF                                                         
255301        END-IF                                                            
255401     END-IF                                                               
255501     .                                                                    
255601     EJECT                                                                
255701 HDB-LOS-VCC-NONVCC SECTION.                                              
255801                                                                          
255901     PERFORM S02-SKAPA-WDL6A1KY                                           
256001     PERFORM IMS-GU-WLINLD01                                              
256101                                                                          
256201     PERFORM UNTIL SEGMENT-SAKNAS                                         
256301        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
256401                                                                          
256501        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
256601        PERFORM IMS-GHU-INLC1-WLINLC11                                    
256701                                                                          
256801        MOVE INL-KVAVIS    TO W-KVAVIS                                    
256901        MOVE INL-IDDC      TO W-IDDC                                      
257001                                                                          
257101        MOVE 'R32'         TO INL-IDPTYP                                  
257201        MOVE WS-IDUSER-003 TO INL-IDUSER-003                              
257301        MOVE ZERO          TO INL-TIINLINL                                
257401                              INL-KVANTMOT                                
257501        ADD +1     TO WS-RAKNARE                                          
257601                                                                          
257701        PERFORM IMS-REPL-INLC1-WLINLC11                                   
257801                                                                          
257901        PERFORM IMS-12-GHU-WDK711                                         
258001        SUBTRACT W-KVAVIS FROM SLAG-KVAKS-PAV                             
258101        MOVE SLAG-PRAVCOST TO WS-PRAVCOST                                 
258201        PERFORM IMS-13-REPL-WDK711                                        
258301                                                                          
258401        MOVE ' '    TO LOGG-IDTECKEN-KVAKS                                
258501        MOVE '-'    TO LOGG-IDTECKEN-KVAKS-PAV                            
258601        MOVE ' '    TO LOGG-IDTECKEN-KVLS                                 
258701        PERFORM S06-SKAPA-SALDOLOGG                                       
258801                                                                          
258901        PERFORM IMS-GET-WLARTC01                                          
259001        MOVE ART-KDPRODSL TO R8-EKH-KDPRODSL                              
259101        PERFORM IMS-GET-WLARTC11                                          
259201        IF REC-DCS-KDTRADP = 'BR12'                                       
259301          MOVE 'L' TO REQU-KDCMD                                          
259401          MOVE YES TO REQU-FLLOS(INDX)                                    
259501        ELSE                                                              
259601          PERFORM S07A-SKAPA-SAP-TRANS                                    
259701        END-IF                                                            
259801        MOVE SPAR-FLINLREP TO FILC2-FLINLREP                              
259901        PERFORM S09-SKAPA-LDC-TRANS                                       
260001                                                                          
260101        PERFORM IMS-GN-WLINLD01                                           
260201     END-PERFORM                                                          
260301     .                                                                    
260401     EJECT                                                                
260501 HDC-LOS-CDC-NDC-NA SECTION.                                              
260601                                                                          
260701     MOVE 'HDC-LOS-CDC-NDC-NA             ' TO WS-SECTION                 
260801                                                                          
260901* SENDING CDC                                                             
261001     PERFORM S02-SKAPA-WDL6A1KY                                           
261101     PERFORM IMS-GU-WLINLD01                                              
261201                                                                          
261301     PERFORM UNTIL SEGMENT-SAKNAS                                         
261401        IF WS-A03-SKAPAD = JA                                             
261501           PERFORM S041-SKRIV-EKOTRANS-A03                                
261601        END-IF                                                            
261701                                                                          
261801        MOVE SEQA-IDARTNR      TO W-IDARTNR                               
261901                                  SPAR-6308-IDARTNR                       
262001                                  EKOTRA03-IDARTNR                        
262101        MOVE SEQA-DAINLEV      TO W-DAINLEV                               
262201        PERFORM IMS-GHU-INLC1-WLINLC11                                    
262301                                                                          
262401        MOVE INL-KVAVIS        TO W-KVAVIS                                
262501        MOVE INL-IDDC          TO W-IDDC                                  
262601        MOVE INL-KDFRAKT       TO W-KDFRAKT                               
262701                                                                          
262801        MOVE 6302-IDFAKT       TO SPAR-6306-IDFAKT                        
262901                                  EKOTRA03-IDFAKT                         
263001        MOVE 'N'               TO SPAR-6306-FLDIRLEV                      
263101        MOVE ZERO              TO SPAR-6308-IDRAPPNR                      
263201                                  SPAR-6308-IDRADNR                       
263301                                  SPAR-6308-KDEMBLEV                      
263401        ACCEPT W-DATUM-X FROM DATE                                        
263501        MOVE W-DATUM           TO SPAR-6308-TILEVANM                      
263601        MOVE 6302-TIFAKT       TO SPAR-6306-TIFAKT                        
263701        MOVE 6302-TIFAKT       TO WS-SEKEL-KOLL                           
263801                                  WS-EKOA03-AAMMDD                        
263901        IF WS-SEKEL = 9                                                   
264001           MOVE 19             TO WS-EKOA03-SS                            
264101        ELSE                                                              
264201           MOVE 20             TO WS-EKOA03-SS                            
264301        END-IF                                                            
264401        MOVE WS-AAAAMMDD       TO EKOTRA03-DAFAKT                         
264501        MOVE WS-IDDC-SPAR      TO SPAR-6306-IDDC-REC                      
264601                                  EKOTRA03-IDDC-REC                       
264701        MOVE WS-CDC-11         TO EKOTRA03-IDDC-SEND                      
264801        MOVE 6302-IDDC-SEND    TO SPAR-6306-IDDC-SEND                     
264901        MOVE 6302-IDDC-LEV     TO SPAR-6306-IDDC-LEV                      
265001        MOVE INL-IDDISTR       TO SPAR-6308-IDDISTR                       
265101                                  EKOTRA03-IDDISTR                        
265201                                  WS-SAP-IDDISTR                          
265301                                  W-IDDISTR-WDB3                          
265401                                  W-IDDISTR-WDB3-DEF                      
265501        MOVE INL-IDKUNDNR      TO SPAR-6308-IDKUNDNR                      
265601                                  EKOTRA03-IDKUNDNR                       
265701                                  WS-SAP-IDKUNDNR                         
265801        MOVE INL-KDFRAKT       TO SPAR-6308-KDFRAKT                       
265901        MOVE INL-IDKUNDRF      TO SPAR-6308-IDKUNDRF                      
266001                                                                          
266101**- ÄNDRING 2004-10 FÖR ATT KLARA LOCAL-CURRENCY NA.INL-PRARTNTO          
266201**- KOMMER FRÅN BILL-IT OCH ÄR I LOCAL VALUTA.                            
266301        IF DIST79-DEALER-PRICE                                            
266401          MOVE INL-PRARTNTO    TO SPAR-6308-PRARTBTO-LOC                  
266501                                  EKOTRA03-PRARTNTO                       
266601          MOVE ZERO            TO SPAR-6308-PRARTBTO                      
266701          MOVE INL-KDVALISO    TO SPAR-6308-KDVALISO                      
266801                                  EKOTRA03-KDVALISO                       
266901        ELSE                                                              
267001          MOVE INL-PRARTNTO    TO SPAR-6308-PRARTBTO                      
267101                                  EKOTRA03-PRARTNTO                       
267201          MOVE ZERO            TO SPAR-6308-PRARTBTO-LOC                  
267301          MOVE 'SEK'           TO SPAR-6308-KDVALISO                      
267401                                  EKOTRA03-KDVALISO                       
267501        END-IF                                                            
267601                                                                          
267701        MOVE INL-IDKOLLI       TO SPAR-6308-IDKOLLI                       
267801                                  EKOTRA03-IDKOLLI                        
267901        MOVE ZERO              TO EKOTRA03-IDORDNR7                       
268001        MOVE INL-IDORDNR5      TO EKOTRA03-IDORDNR7                       
268101        MOVE ZERO              TO EKOTRA03-DAINLINL                       
268201        MOVE INL-KVAVIS        TO EKOTRA03-KVLEVART                       
268301                                  SPAR-6308-KVLEVANM                      
268401        MOVE 'SEK'             TO EKOTRA03-KDVALISO                       
268501        MOVE INL-PRKURS        TO EKOTRA03-PRKURS                         
268601        MOVE '60'              TO SPAR-6308-KDANMORS                      
268701                                  EKOTRA03-KDANMORS                       
268801                                                                          
268901        PERFORM IMS-GET-WLARTC01                                          
269001        MOVE ART-KDPRODSL      TO EKOTRA03-KDPRODSL                       
269101                                  R8-EKH-KDPRODSL                         
269201        MOVE ART-KDSORT        TO WS-KDSORT                               
269301        PERFORM IMS-GET-WLARTC11                                          
269401        MOVE CLAG-KDPSLLOC     TO EKOTRA03-KDPSLLOC                       
269501        PERFORM S05-SOEK-REMARKUP                                         
269601        MOVE WS-MARKUP         TO EKOTRA03-REMARKUP                       
269701        MOVE SPACE             TO EKOTRA03-FLSLUT                         
269801        MOVE ZERO              TO EKOTRA03-KVANTMOT                       
269901                                  EKOTRA03-KVSKROT                        
270001                                                                          
270101        MOVE 'R32'         TO INL-IDPTYP                                  
270201        MOVE WS-IDUSER-003 TO INL-IDUSER-003                              
270301        MOVE ZERO          TO INL-TIINLINL                                
270401                              INL-KVANTMOT                                
270501        ADD +1 TO WS-RAKNARE                                              
270601        PERFORM IMS-REPL-INLC1-WLINLC11                                   
270701                                                                          
270801        PERFORM HDAB-KOLLA-FRAKTKOD                                       
270901                                                                          
271001        PERFORM S98-SKAPA-LEVANM-TRANS                                    
271101                                                                          
271201        PERFORM IMS-12-GHU-WDK711                                         
271301        SUBTRACT W-KVAVIS FROM SLAG-KVAKS-PAV                             
271401        MOVE SLAG-KVLS     TO EKOTRA03-KVLS-OLD                           
271501        MOVE SLAG-PRAVCOST TO WS-PRAVCOST                                 
271601                              EKOTRA03-PRAVCOST-OLD                       
271701                              EKOTRA03-PRAVCOST                           
271801                                                                          
271901        PERFORM IMS-13-REPL-WDK711                                        
272001                                                                          
272101        MOVE ' '    TO LOGG-IDTECKEN-KVAKS                                
272201        MOVE '-'    TO LOGG-IDTECKEN-KVAKS-PAV                            
272301        MOVE ' '    TO LOGG-IDTECKEN-KVLS                                 
272401        PERFORM S06-SKAPA-SALDOLOGG                                       
272501                                                                          
272601        PERFORM S04-SKAPA-EKOTRANS-A03                                    
272701        MOVE SPAR-FLINLREP TO FILC2-FLINLREP                              
272801        PERFORM S09-SKAPA-LDC-TRANS                                       
272901        PERFORM IMS-GN-WLINLD01                                           
273001     END-PERFORM                                                          
273101     .                                                                    
273201     EJECT                                                                
273301 HDD-FAKTURA-SLUTBEHANDLAD SECTION.                                       
273401                                                                          
273501     PERFORM IMS-GHU-WL630511                                             
273601     IF SEGMENT-FINNS                                                     
273701        MOVE JA TO 6306-FLKLAR                                            
273801        PERFORM IMS-REPL-WL630511                                         
273901     END-IF                                                               
274001     .                                                                    
274101     EJECT                                                                
274201 HDE-LOS-USA-USA SECTION.                                                 
274301                                                                          
274401     PERFORM S02-SKAPA-WDL6A1KY                                           
274501     PERFORM IMS-GU-WLINLD01                                              
274601                                                                          
274701     PERFORM UNTIL SEGMENT-SAKNAS                                         
274801        IF WS-A03-SKAPAD = JA                                             
274901           PERFORM S041-SKRIV-EKOTRANS-A03                                
275001        END-IF                                                            
275101        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
275201                             EKOTRA03-IDARTNR                             
275301                                                                          
275401        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
275501        PERFORM IMS-GHU-INLC1-WLINLC11                                    
275601                                                                          
275701        MOVE INL-KVAVIS        TO W-KVAVIS                                
275801        MOVE INL-IDDC          TO W-IDDC                                  
275901        MOVE ZERO              TO EKOTRA03-IDORDNR7                       
276001        MOVE INL-IDORDNR5      TO EKOTRA03-IDORDNR7                       
276101        MOVE INL-PRARTNTO      TO EKOTRA03-PRARTNTO                       
276201        MOVE INL-IDKOLLI       TO EKOTRA03-IDKOLLI                        
276301        MOVE INL-KVAVIS        TO EKOTRA03-KVLEVART                       
276401        MOVE 'USD'             TO EKOTRA03-KDVALISO                       
276501        MOVE INL-PRKURS        TO EKOTRA03-PRKURS                         
276601        MOVE 6302-IDDC-SEND    TO EKOTRA03-IDDC-SEND                      
276701        MOVE INL-IDDISTR       TO EKOTRA03-IDDISTR                        
276801        MOVE INL-IDKUNDNR      TO EKOTRA03-IDKUNDNR                       
276901        MOVE 6302-TIFAKT       TO WS-SEKEL-KOLL                           
277001                                  WS-EKOA03-AAMMDD                        
277101        IF WS-SEKEL = 9                                                   
277201           MOVE 19             TO WS-EKOA03-SS                            
277301        ELSE                                                              
277401           MOVE 20             TO WS-EKOA03-SS                            
277501        END-IF                                                            
277601        MOVE WS-AAAAMMDD       TO EKOTRA03-DAFAKT                         
277701        MOVE 6302-IDFAKT       TO EKOTRA03-IDFAKT                         
277801        MOVE WS-IDDC-SPAR      TO EKOTRA03-IDDC-REC                       
277901        MOVE '60'              TO EKOTRA03-KDANMORS                       
278001        MOVE ZERO              TO EKOTRA03-DAINLINL                       
278101                                                                          
278201        MOVE 'R32'             TO INL-IDPTYP                              
278301        MOVE WS-IDUSER-003     TO INL-IDUSER-003                          
278401        MOVE ZERO          TO INL-TIINLINL                                
278501                              INL-KVANTMOT                                
278601        ADD +1     TO WS-RAKNARE                                          
278701                                                                          
278801        PERFORM IMS-REPL-INLC1-WLINLC11                                   
278901                                                                          
279001        PERFORM IMS-12-GHU-WDK711                                         
279101        SUBTRACT W-KVAVIS FROM SLAG-KVAKS-PAV                             
279201        MOVE SLAG-KVLS         TO EKOTRA03-KVLS-OLD                       
279301        MOVE SLAG-PRAVCOST     TO EKOTRA03-PRAVCOST-OLD                   
279401        MOVE SLAG-PRAVCOST     TO EKOTRA03-PRAVCOST                       
279501                                                                          
279601        PERFORM IMS-13-REPL-WDK711                                        
279701                                                                          
279801        MOVE ' '    TO LOGG-IDTECKEN-KVAKS                                
279901        MOVE '-'    TO LOGG-IDTECKEN-KVAKS-PAV                            
280001        MOVE ' '    TO LOGG-IDTECKEN-KVLS                                 
280101        PERFORM S06-SKAPA-SALDOLOGG                                       
280201                                                                          
280301        PERFORM IMS-GET-WLARTC01                                          
280401        MOVE ART-KDPRODSL      TO EKOTRA03-KDPRODSL                       
280501        PERFORM IMS-GET-WLARTC11                                          
280601        MOVE CLAG-KDPSLLOC     TO EKOTRA03-KDPSLLOC                       
280701        MOVE 1                 TO EKOTRA03-REMARKUP                       
280801                                                                          
280901        MOVE SPACE             TO EKOTRA03-FLSLUT                         
281001        MOVE ZERO              TO EKOTRA03-KVANTMOT                       
281101                                  EKOTRA03-KVSKROT                        
281201        PERFORM S04-SKAPA-EKOTRANS-A03                                    
281301        MOVE SPAR-FLINLREP TO FILC2-FLINLREP                              
281401        PERFORM S09-SKAPA-LDC-TRANS                                       
281501                                                                          
281601        PERFORM IMS-GN-WLINLD01                                           
281701     END-PERFORM                                                          
281801     .                                                                    
281901     EJECT                                                                
282001 HDF-LOS-USA-CANADA SECTION.                                              
282101                                                                          
282201     PERFORM S02-SKAPA-WDL6A1KY                                           
282301     PERFORM IMS-GU-WLINLD01                                              
282401                                                                          
282501     PERFORM UNTIL SEGMENT-SAKNAS                                         
282601        IF WS-A03-SKAPAD = JA                                             
282701           PERFORM S041-SKRIV-EKOTRANS-A03                                
282801        END-IF                                                            
282901                                                                          
283001        MOVE SEQA-IDARTNR   TO W-IDARTNR                                  
283101        MOVE SEQA-IDARTNR   TO SPAR-6308-IDARTNR                          
283201                               EKOTRA03-IDARTNR                           
283301                                                                          
283401        MOVE SEQA-DAINLEV   TO W-DAINLEV                                  
283501        PERFORM IMS-GHU-INLC1-WLINLC11                                    
283601        MOVE INL-KVAVIS     TO W-KVAVIS                                   
283701        MOVE INL-IDDC       TO W-IDDC                                     
283801                                                                          
283901        MOVE 'R32'          TO INL-IDPTYP                                 
284001        MOVE WS-IDUSER-003  TO INL-IDUSER-003                             
284101        MOVE ZERO           TO INL-TIINLINL                               
284201                               INL-KVANTMOT                               
284301        ADD +1              TO WS-RAKNARE                                 
284401                                                                          
284501        MOVE 6302-TIFAKT    TO SPAR-6306-TIFAKT                           
284601        MOVE 6302-TIFAKT    TO WS-SEKEL-KOLL                              
284701                               WS-EKOA03-AAMMDD                           
284801        IF WS-SEKEL = 9                                                   
284901           MOVE 19          TO WS-EKOA03-SS                               
285001        ELSE                                                              
285101           MOVE 20          TO WS-EKOA03-SS                               
285201        END-IF                                                            
285301                                                                          
285401        MOVE WS-AAAAMMDD    TO EKOTRA03-DAFAKT                            
285501        ACCEPT W-DATUM-X FROM DATE                                        
285601        MOVE W-DATUM        TO SPAR-6308-TILEVANM                         
285701        MOVE INL-IDKUNDNR   TO EKOTRA03-IDKUNDNR                          
285801                               SPAR-6308-IDKUNDNR                         
285901        MOVE 6302-IDFAKT    TO EKOTRA03-IDFAKT                            
286001                               SPAR-6306-IDFAKT                           
286101        MOVE WS-IDDC-SPAR   TO SPAR-6306-IDDC-REC                         
286201                               EKOTRA03-IDDC-REC                          
286301        MOVE 'N'            TO SPAR-6306-FLDIRLEV                         
286401        MOVE ZERO           TO SPAR-6308-IDRAPPNR                         
286501                               SPAR-6308-IDRADNR                          
286601                               SPAR-6308-KDEMBLEV                         
286701        MOVE INL-IDDISTR    TO SPAR-6308-IDDISTR                          
286801                               EKOTRA03-IDDISTR                           
286901        MOVE INL-IDKUNDRF   TO SPAR-6308-IDKUNDRF                         
287001        MOVE INL-KDFRAKT    TO SPAR-6308-KDFRAKT                          
287101        MOVE ZERO           TO EKOTRA03-IDORDNR7                          
287201        MOVE INL-IDORDNR5   TO EKOTRA03-IDORDNR7                          
287301        MOVE INL-KVAVIS     TO EKOTRA03-KVLEVART                          
287401                               SPAR-6308-KVLEVANM                         
287501        MOVE INL-IDKOLLI    TO SPAR-6308-IDKOLLI                          
287601                               EKOTRA03-IDKOLLI                           
287701                                                                          
287801**- ÄNDRING 2004-10 FÖR ATT KLARA LOCAL-CURRENCY NA RÖR EJ                
287901**- NA-TRANSFER-DISTRIKTEN EFTERSOM DESSA HAR INL-PRARTNTO=               
288001**- PRAVCOST I USD/CAD. SE PGM W4752A00.                                  
288101        MOVE INL-PRARTNTO   TO EKOTRA03-PRARTNTO                          
288201                               SPAR-6308-PRARTBTO                         
288301        MOVE ZERO           TO SPAR-6308-PRARTBTO-LOC                     
288401        MOVE ZERO           TO EKOTRA03-DAINLINL                          
288501        MOVE 6302-IDDC-SEND TO SEND-WS-IDDC                               
288601        IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                               
288701           MOVE SEND-WS-IDDC TO W-IDDC-B6                                 
288801           PERFORM IMS-GU-WDB601-SEND                                     
288901        END-IF                                                            
289001        IF SEND-DCS-NDC-NA AND SEND-DCS-CANADA                            
289101           MOVE 'CAD'       TO EKOTRA03-KDVALISO                          
289201                               SPAR-6308-KDVALISO                         
289301        ELSE                                                              
289401           MOVE 'USD'       TO EKOTRA03-KDVALISO                          
289501                               SPAR-6308-KDVALISO                         
289601        END-IF                                                            
289701        MOVE 6302-IDDC-SEND TO EKOTRA03-IDDC-SEND                         
289801        MOVE 6302-IDDC-SEND TO SPAR-6306-IDDC-SEND                        
289901        MOVE 6302-IDDC-LEV  TO SPAR-6306-IDDC-LEV                         
290001        MOVE INL-PRKURS     TO EKOTRA03-PRKURS                            
290101        MOVE '60'           TO EKOTRA03-KDANMORS                          
290201        MOVE '00'           TO SPAR-6308-KDANMORS                         
290301                                                                          
290401        PERFORM IMS-REPL-INLC1-WLINLC11                                   
290501                                                                          
290601        PERFORM S98-SKAPA-LEVANM-TRANS                                    
290701                                                                          
290801        PERFORM IMS-12-GHU-WDK711                                         
290901        SUBTRACT W-KVAVIS FROM SLAG-KVAKS-PAV                             
291001        MOVE SLAG-KVLS         TO EKOTRA03-KVLS-OLD                       
291101        MOVE SLAG-PRAVCOST     TO EKOTRA03-PRAVCOST-OLD                   
291201        MOVE SLAG-PRAVCOST     TO EKOTRA03-PRAVCOST                       
291301                                                                          
291401        PERFORM IMS-13-REPL-WDK711                                        
291501                                                                          
291601        MOVE ' '    TO LOGG-IDTECKEN-KVAKS                                
291701        MOVE '-'    TO LOGG-IDTECKEN-KVAKS-PAV                            
291801        MOVE ' '    TO LOGG-IDTECKEN-KVLS                                 
291901        PERFORM S06-SKAPA-SALDOLOGG                                       
292001                                                                          
292101        PERFORM IMS-GET-WLARTC01                                          
292201        MOVE ART-KDPRODSL      TO EKOTRA03-KDPRODSL                       
292301        PERFORM IMS-GET-WLARTC11                                          
292401        MOVE CLAG-KDPSLLOC     TO EKOTRA03-KDPSLLOC                       
292501        PERFORM S05-SOEK-REMARKUP                                         
292601        MOVE WS-MARKUP         TO EKOTRA03-REMARKUP                       
292701        MOVE SPACE             TO EKOTRA03-FLSLUT                         
292801        MOVE ZERO              TO EKOTRA03-KVANTMOT                       
292901                                  EKOTRA03-KVSKROT                        
293001                                                                          
293101        PERFORM S04-SKAPA-EKOTRANS-A03                                    
293201        MOVE SPAR-FLINLREP TO FILC2-FLINLREP                              
293301        PERFORM S09-SKAPA-LDC-TRANS                                       
293401                                                                          
293501        PERFORM IMS-GN-WLINLD01                                           
293601     END-PERFORM                                                          
293701     .                                                                    
293801     EJECT                                                                
293901 HE-DCA-UPDATERING SECTION.                                               
294001     MOVE 'HE-DCA-UPDATERING             ' TO WS-SECTION                  
294101                                                                          
294201     PERFORM S02-SKAPA-WDL6A1KY                                           
294301     PERFORM IMS-04-GU-WLINLD01                                           
294401                                                                          
294501     PERFORM UNTIL SEGMENT-SAKNAS                                         
294601                                                                          
294701        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
294801        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
294901                                                                          
295001        PERFORM IMS-09-GHU-INLC1-WLINLC11                                 
295101        IF SEGMENT-FINNS                                                  
295201           IF REQU-IDUSER-003 = ALL '+' OR SPACE                          
295301              CONTINUE                                                    
295401           ELSE                                                           
295501              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
295601           END-IF                                                         
295701           MOVE 'J' TO INL-FLSKAKOL                                       
295801           PERFORM IMS-10-REPL-INLC1-WLINLC11                             
295901        END-IF                                                            
296001        PERFORM IMS-05-GN-WLINLD01                                        
296101     END-PERFORM                                                          
296201     .                                                                    
296301     EJECT                                                                
296401 HF-CAN-UPDATERING SECTION.                                               
296501     MOVE 'HF-CAN-UPDATERING             ' TO WS-SECTION                  
296601                                                                          
296701     PERFORM S02-SKAPA-WDL6A1KY                                           
296801     PERFORM IMS-04-GU-WLINLD01                                           
296901                                                                          
297001     PERFORM UNTIL SEGMENT-SAKNAS                                         
297101                                                                          
297201        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
297301        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
297401                                                                          
297501        PERFORM IMS-09-GHU-INLC1-WLINLC11                                 
297601        IF SEGMENT-FINNS                                                  
297701           IF REQU-IDUSER-003 = ALL '+' OR SPACE                          
297801              CONTINUE                                                    
297901           ELSE                                                           
298001              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
298101           END-IF                                                         
298201           MOVE 'N' TO INL-FLSKAKOL                                       
298301           PERFORM IMS-10-REPL-INLC1-WLINLC11                             
298401        END-IF                                                            
298501        PERFORM IMS-05-GN-WLINLD01                                        
298601     END-PERFORM                                                          
298701     .                                                                    
298801     EJECT                                                                
298901 HH-BIN-UPDATERING SECTION.                                               
299001     MOVE 'HH-BIN-UPDATERING             ' TO WS-SECTION                  
299101                                                                          
299201******************************************************************        
299301* CMD = BIN STARTAR TRANS W6T303X                                         
299401******************************************************************        
299501                                                                          
299601     MOVE JA              TO HOPP-UPDATE-SW                               
299701                                                                          
299801     IF REQU-CMD-IN (INDX) = 'BIN' OR 'HAC'                               
299901        MOVE W-IDFAKT              TO SEND-REQU-IDFAKT-KEY                
300001        MOVE WS-IDDC-SPAR          TO SEND-REQU-IDDC-KEY                  
300101        MOVE WS-IDUSER-003         TO SEND-REQU-IDUSER-003                
300201        IF REQU-CMD-IN (INDX) = 'HAC'                                     
300301           MOVE 'HAC'              TO SEND-REQU-CMD-IN(BIN-IX)            
300401        END-IF                                                            
300501        INSPECT SEND-REQU-IDFAKT-KEY                                      
300601             REPLACING LEADING SPACE BY ZERO                              
300701        MOVE REQU-IDKUNDRF(INDX)   TO SEND-REQU-IDKUNDRF(BIN-IX)          
300801        INSPECT REQU-IDKUNDNR(INDX)                                       
300901                            REPLACING LEADING SPACE BY ZERO               
301001        MOVE REQU-IDKUNDNR (INDX)  TO SEND-REQU-IDKUNDNR(BIN-IX)          
301101        INSPECT REQU-IDKOLLI(INDX)                                        
301201                            REPLACING LEADING SPACE BY ZERO               
301301        MOVE REQU-IDKOLLI(INDX)    TO SEND-REQU-IDKOLLI(BIN-IX)           
301401                                                                          
301501        MOVE BIN-IX                TO SEND-REQU-KVRADER                   
301601        ADD +1 TO BIN-IX                                                  
301701     END-IF                                                               
301801     .                                                                    
301901     EJECT                                                                
302001 HG-SKAPA-DP-RAPPORT    SECTION.                                          
302101     MOVE 'HG-SKAPA-DP-RA' TO WS-SECTION                                  
302201                                                                          
302301     INSPECT REQU-IDKOLLI(INDX)    REPLACING LEADING SPACE BY ZERO        
302401     INSPECT REQU-IDKUNDRF(INDX)   REPLACING LEADING SPACE BY ZERO        
302501     INSPECT REQU-IDKUNDNR(INDX)   REPLACING LEADING SPACE BY ZERO        
302601                                                                          
302701     MOVE W-CMD                 TO URV-CMD                                
302801     MOVE REQU-IDDC-KEY         TO URV-IDDC                               
302901     MOVE W-IDFAKT              TO URV-IDFAKT                             
303001     MOVE REQU-IDKOLLI(INDX)    TO URV-IDKOLLI                            
303101     MOVE REQU-IDKUNDRF(INDX)   TO URV-IDKUNDRF                           
303201     MOVE REQU-IDKUNDNR(INDX)   TO URV-IDKUNDNR                           
303301                                                                          
303401     IF URV-KDMATT = 'U'                                                  
303501       MOVE 'U'                 TO URV-KDMATT                             
303601     ELSE                                                                 
303701       MOVE 'S'                 TO URV-KDMATT                             
303801     END-IF                                                               
303901                                                                          
304001     MOVE REQU-IDUSER           TO URV-IDUSER                             
304101                                                                          
304201     MOVE URV-IDDC TO W-6301-IDDC                                         
304301     PERFORM IMS-23-GET-WL630111                                          
304401     MOVE 6302-IDDISTR          TO URV-IDDISTR                            
304501                                                                          
304601     IF URV-IDFAKT   NUMERIC AND                                          
304701        URV-IDKUNDNR NUMERIC AND                                          
304801        URV-IDKOLLI  NUMERIC AND                                          
304901        URV-IDFAKT   NOT = ZERO                                           
305001                                                                          
305101        MOVE LOW-VALUE           TO W-WDL6A1KY-MIN                        
305201        MOVE HIGH-VALUE          TO W-WDL6A1KY-MAX                        
305301        MOVE URV-IDFAKT          TO W-IDFAKT-MIN                          
305401                                    W-IDFAKT-MAX                          
305501                                                                          
305601*       MOVE +1 TO INDX                                                   
305701        MOVE URV-IDKUNDRF        TO W-IDKUNDRF-MIN                        
305801                                    W-IDKUNDRF-MAX                        
305901        MOVE URV-IDKUNDNR        TO W-IDKUNDNR-MIN                        
306001                                    W-IDKUNDNR-MAX                        
306101        MOVE URV-IDKOLLI         TO W-IDKOLLI-MIN                         
306201                                    W-IDKOLLI-MAX                         
306301        PERFORM IMS-04-GU-WLINLD01                                        
306401        IF SEGMENT-FINNS                                                  
306501                                                                          
306601           PERFORM HGA-SKAPA-POSTTYP-01                                   
306701                                                                          
306801           MOVE ZERO TO WS-TOT-ART                                        
306901                        WS-TOT-VKART                                      
307001                        WS-TOT-VLARTNTO                                   
307101           INITIALIZE SORT-TABELL                                         
307201           MOVE +1 TO TAB-IX                                              
307301           PERFORM UNTIL SEGMENT-SAKNAS                                   
307401              ADD +1 TO WS-TOT-ART                                        
307501              PERFORM HGB-LAS-TILL-TABELL                                 
307601              ADD +1 TO TAB-IX                                            
307701              PERFORM IMS-05-GN-WLINLD01                                  
307801           END-PERFORM                                                    
307901           PERFORM HGC-SKAPA-POSTTYP-02-03                                
308001        END-IF                                                            
308101     END-IF                                                               
308201     .                                                                    
308301     EJECT                                                                
308401                                                                          
308501 HGA-SKAPA-POSTTYP-01    SECTION.                                         
308601     MOVE 'HGA-SKAPA-01  ' TO WS-SECTION                                  
308701                                                                          
308801     MOVE '1'            TO UT1-IDAFPRCD                                  
308901     MOVE URV-IDDC       TO UT1-IDDC                                      
309001     MOVE URV-IDUSER     TO UT1-IDUSER                                    
309101     MOVE URV-IDFAKT     TO UT1-IDLIST                                    
309201     MOVE URV-IDDISTR    TO UT1-IDDISTR                                   
309301     MOVE URV-IDKUNDRF   TO UT1-IDKUNDRF                                  
309401     MOVE URV-IDKUNDNR   TO UT1-IDKUNDNR                                  
309501     MOVE URV-IDKOLLI    TO UT1-IDKOLLI                                   
309601                                                                          
309701     PERFORM S92-PUT-DOC-HEAD                                             
309801     .                                                                    
309901     EJECT                                                                
310001                                                                          
310101 HGB-LAS-TILL-TABELL     SECTION.                                         
310201     MOVE 'HGB-LAS-TILL-TABELL' TO WS-SECTION                             
310301                                                                          
310401     IF TAB-IX > TAB-IX-MAX                                               
310501        MOVE 'ARBETSTABELL FULL' TO FELTEXT                               
310601        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
310701     END-IF                                                               
310801                                                                          
310901     MOVE SEQA-IDARTNR         TO W-IDARTNR                               
311001     MOVE SEQA-IDDC            TO W-IDDC                                  
311101                                                                          
311201     IF REC-DCS-CDC                                                       
311301       PERFORM IMS-17-GET-ARTC11                                          
311401       MOVE CLAG-KVROS         TO WS-KVROS                                
311501       IF WS-KVROS >= INL-KVAVIS                                          
311601          MOVE 'Y'             TO TAB-FLBACKORDER(TAB-IX)                 
311701       ELSE                                                               
311801          MOVE 'N'             TO TAB-FLBACKORDER(TAB-IX)                 
311901       END-IF                                                             
312001                                                                          
312101       IF CLAG-ADART = ZERO                                               
312201          MOVE ZERO            TO TAB-ADLAGOMR   (TAB-IX)                 
312301                                  TAB-ADGANG     (TAB-IX)                 
312401                                  TAB-ADPLATS    (TAB-IX)                 
312501       ELSE                                                               
312601          MOVE CLAG-ADLAGOMR   TO TAB-ADLAGOMR   (TAB-IX)                 
312701          MOVE CLAG-ADGANG     TO TAB-ADGANG     (TAB-IX)                 
312801          MOVE CLAG-ADPLATS    TO TAB-ADPLATS    (TAB-IX)                 
312901       END-IF                                                             
313001                                                                          
313101       MOVE CLAG-KDARTURS      TO TAB-KDARTURS   (TAB-IX)                 
313201     ELSE                                                                 
313301       PERFORM IMS-11-GU-WDK711                                           
313401       COMPUTE WS-KVROS = SLAG-KVROS-DAG + SLAG-KVROS-BULK                
313501       IF WS-KVROS >= INL-KVAVIS                                          
313601          MOVE 'Y'             TO TAB-FLBACKORDER(TAB-IX)                 
313701       ELSE                                                               
313801          MOVE 'N'             TO TAB-FLBACKORDER(TAB-IX)                 
313901       END-IF                                                             
314001                                                                          
314101       IF SLAG-ADART = ZERO                                               
314201          MOVE ZERO            TO TAB-ADLAGOMR   (TAB-IX)                 
314301                                  TAB-ADGANG     (TAB-IX)                 
314401                                  TAB-ADPLATS    (TAB-IX)                 
314501       ELSE                                                               
314601          MOVE SLAG-ADLAGOMR   TO TAB-ADLAGOMR   (TAB-IX)                 
314701          MOVE SLAG-ADGANG     TO TAB-ADGANG     (TAB-IX)                 
314801          MOVE SLAG-ADPLATS    TO TAB-ADPLATS    (TAB-IX)                 
314901       END-IF                                                             
315001                                                                          
315101       IF SLAG-IDLEVNR = '1441 '                                          
315201       OR SLAG-IDLEVNR = SPACE                                            
315301       OR SLAG-IDLEVNR = 'BP2TW'                                          
315401          MOVE SPACE           TO TAB-KDARTURS   (TAB-IX)                 
315501       ELSE                                                               
315601          PERFORM IMS-14-GU-WDK712                                        
315701          IF SEGMENT-FINNS                                                
315801             MOVE LART-KDARTURS TO TAB-KDARTURS  (TAB-IX)                 
315901          ELSE                                                            
316001            MOVE SPACE          TO TAB-KDARTURS   (TAB-IX)                
316101          END-IF                                                          
316201       END-IF                                                             
316301       PERFORM IMS-17-GET-ARTC11                                          
316401     END-IF                                                               
316501                                                                          
316601     IF CLAG-KDFARLIG = 4 OR CLAG-KDFARLIG = 7                            
316701        MOVE 'Y'             TO TAB-FLKDFARLIG (TAB-IX)                   
316801     ELSE                                                                 
316901        MOVE 'N'             TO TAB-FLKDFARLIG (TAB-IX)                   
317001     END-IF                                                               
317101                                                                          
317201     IF TAB-KDARTURS (TAB-IX) = SPACE                                     
317301        MOVE CLAG-KDARTURS   TO TAB-KDARTURS   (TAB-IX)                   
317401     END-IF                                                               
317501                                                                          
317601     MOVE SEQA-IDARTNR       TO TAB-IDARTNR    (TAB-IX)                   
317701     MOVE SEQA-DAINLEV       TO W-DAINLEV                                 
317801     PERFORM IMS-09-GHU-INLC1-WLINLC11                                    
317901     MOVE INL-FLPRIO         TO TAB-FLPRIO     (TAB-IX)                   
318001     MOVE INL-KVAVIS         TO TAB-KVAVIS     (TAB-IX)                   
318101                                                                          
318201     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
318301     IF DCS-UNICODE-IDSKYLT                                               
318401        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
318501     ELSE                                                                 
318601        MOVE '278 '             TO TRAUTF8-KDCP                           
318701     END-IF                                                               
318801     PERFORM IMS-24-GET-WDD311                                            
318901     IF SEGMENT-FINNS                                                     
319001      MOVE TEXT-BEARTEXT      TO TRAUTF8-TECONV-FROM                      
319101     ELSE                                                                 
319201      MOVE SPACES TO TRAUTF8-TECONV-FROM                                  
319301     END-IF                                                               
319401     IF TRAUTF8-TECONV-FROM = SPACES                                      
319501      MOVE 'GB'  TO W-IDSKYLT                                             
319601      MOVE '278' TO TRAUTF8-KDCP                                          
319701      PERFORM IMS-24-GET-WDD311                                           
319801      MOVE TEXT-BEARTEXT TO TRAUTF8-TECONV-FROM                           
319901     END-IF                                                               
320001     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
320101     MOVE TRAUTF8-TECONV-TO  TO TAB-BEART (TAB-IX)                        
320201                                                                          
320301     MOVE ZERO               TO WS-VKART                                  
320401                                WS-VLARTNTO                               
320501     COMPUTE WS-VKART    = INL-KVAVIS * CLAG-VKART                        
320601     ADD WS-VKART            TO WS-TOT-VKART                              
320701     COMPUTE WS-VLARTNTO = INL-KVAVIS * CLAG-VLARTNTO                     
320801     ADD WS-VLARTNTO         TO WS-TOT-VLARTNTO                           
320901     .                                                                    
321001     EJECT                                                                
321101                                                                          
321201 HGC-SKAPA-POSTTYP-02-03 SECTION.                                         
321301     MOVE 'HGC-SKAPA-02-03' TO WS-SECTION                                 
321401                                                                          
321501     MOVE WS-TOT-ART TO ANTAL                                             
321601     IF WS-TOT-ART > 1                                                    
321701        MOVE +121    TO STEGLAANGD                                        
321801        IF URV-CMD   = 'PR '                                              
321901          MOVE +5    TO NYCKELLAANGD                                      
322001                                                                          
322101          CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                 
322201          TAB-SORT1-BEGREPP(1) NYCKELLAANGD                               
322301        ELSE                                                              
322401          MOVE +7    TO NYCKELLAANGD                                      
322501                                                                          
322601          CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                 
322701          TAB-SORT2-BEGREPP(1) NYCKELLAANGD                               
322801        END-IF                                                            
322901     END-IF                                                               
323001                                                                          
323101     MOVE +1 TO TAB-IX                                                    
323201                                                                          
323301     PERFORM UNTIL TAB-IX > ANTAL                                         
323401        MOVE '2'                     TO UT2-IDAFPRCD                      
323501        IF TAB-FLPRIO(TAB-IX) = 'J' OR 'Y'                                
323601           MOVE 'Y'                  TO UT2-FLPRIO                        
323701        ELSE                                                              
323801           MOVE SPACE                TO UT2-FLPRIO                        
323901        END-IF                                                            
324001        MOVE TAB-KVAVIS(TAB-IX)      TO UT2-KVAVIS                        
324101        MOVE TAB-IDARTNR(TAB-IX)     TO UT2-IDARTNR                       
324201        IF TAB-FLBACKORDER(TAB-IX) = 'J' OR 'Y'                           
324301           MOVE 'Y'                  TO UT2-FLBACKORDER                   
324401        ELSE                                                              
324501           MOVE SPACE                TO UT2-FLBACKORDER                   
324601        END-IF                                                            
324701        IF TAB-ADLAGOMR(TAB-IX) = ZERO                                    
324801           MOVE 'Y'                  TO UT2-FLNEW                         
324901        ELSE                                                              
325001           MOVE SPACE                TO UT2-FLNEW                         
325101        END-IF                                                            
325201        MOVE TAB-ADLAGOMR(TAB-IX) TO UT2-ADLAGOMR                         
325301        MOVE TAB-ADGANG(TAB-IX)   TO UT2-ADGANG                           
325401        MOVE TAB-ADPLATS(TAB-IX)  TO UT2-ADPLATS                          
325501                                                                          
325601        MOVE TAB-KDARTURS(TAB-IX)    TO UT2-KDARTURS                      
325701        MOVE TAB-FLKDFARLIG(TAB-IX)  TO UT2-FLKDFARLIG                    
325801        MOVE TAB-BEART(TAB-IX)       TO UT2-BEART                         
325901        MOVE SPACE                   TO UT2-FLTEKVALNOT                   
326001                                                                          
326101        PERFORM S93-PUT-DOC-LINE                                          
326201                                                                          
326301        ADD +1 TO TAB-IX                                                  
326401                                                                          
326501     END-PERFORM                                                          
326601                                                                          
326701     MOVE '3'          TO UT3-IDAFPRCD                                    
326801     MOVE WS-TOT-ART   TO UT3-TOT-ART                                     
326901     COMPUTE WS-SUM-VKART ROUNDED = WS-TOT-VKART / 1000                   
327001     END-COMPUTE                                                          
327101     COMPUTE WS-SUM-VLARTNTO ROUNDED                                      
327201        = WS-TOT-VLARTNTO / 1000000                                       
327301     END-COMPUTE                                                          
327401                                                                          
327501     IF WS-SUM-VKART = ZERO                                               
327601       IF US-MEASUREMENT                                                  
327701         COMPUTE UT3-TOT-VKART = 1 * CONV-GR-TO-LB                        
327801         MOVE 'LB        ' TO UT3-KDSORT-VIKT                             
327901       ELSE                                                               
328001         MOVE 1            TO UT3-TOT-VKART                               
328101         MOVE 'KG        ' TO UT3-KDSORT-VIKT                             
328201       END-IF                                                             
328301     ELSE                                                                 
328401       IF US-MEASUREMENT                                                  
328501         COMPUTE UT3-TOT-VKART = WS-SUM-VKART * CONV-GR-TO-LB             
328601         END-COMPUTE                                                      
328701         MOVE 'LB        ' TO UT3-KDSORT-VIKT                             
328801       ELSE                                                               
328901         MOVE WS-SUM-VKART TO UT3-TOT-VKART                               
329001         MOVE 'KG        ' TO UT3-KDSORT-VIKT                             
329101       END-IF                                                             
329201     END-IF                                                               
329301                                                                          
329401     IF WS-SUM-VLARTNTO = ZERO                                            
329501       IF US-MEASUREMENT                                                  
329601         COMPUTE UT3-TOT-VLARTNTO = 1 * CONV-M3-TO-FT3                    
329701         MOVE 'FT3     '      TO UT3-KDSORT-VOLYM                         
329801       ELSE                                                               
329901         MOVE 1               TO UT3-TOT-VLARTNTO                         
330001         MOVE 'M3      '      TO UT3-KDSORT-VOLYM                         
330101       END-IF                                                             
330201     ELSE                                                                 
330301       IF US-MEASUREMENT                                                  
330401         COMPUTE UT3-TOT-VLARTNTO =                                       
330501                 WS-SUM-VLARTNTO * CONV-M3-TO-FT3                         
330601         END-COMPUTE                                                      
330701         MOVE 'FT3     '      TO UT3-KDSORT-VOLYM                         
330801       ELSE                                                               
330901         MOVE WS-SUM-VLARTNTO TO UT3-TOT-VLARTNTO                         
331001         MOVE 'M3      '      TO UT3-KDSORT-VOLYM                         
331101       END-IF                                                             
331201     END-IF                                                               
331301                                                                          
331401     PERFORM S94-PUT-DOC-TOT                                              
331501     PERFORM HGCA-SKAPA-POSTTYP-04                                        
331601     .                                                                    
331701     EJECT                                                                
331801                                                                          
331901 HGCA-SKAPA-POSTTYP-04 SECTION.                                           
332001     MOVE 'HGCA-SKAPA-04' TO WS-SECTION                                   
332101                                                                          
332201     MOVE '4'                        TO UT4-IDAFPRCD                      
332301     MOVE +1 TO TAB-IX                                                    
332401                                                                          
332501     PERFORM UNTIL TAB-IX > ANTAL                                         
332601        MOVE TAB-IDARTNR(TAB-IX)     TO UT4-IDARTNR                       
332701                                        W-IDARTNR                         
332801                                                                          
332901        IF REC-DCS-CDC                                                    
333001          MOVE ZERO                  TO UT4-IDPERSON-BUY                  
333101        ELSE                                                              
333201          PERFORM IMS-11-GU-WDK711                                        
333301          MOVE SLAG-IDPERSON-BUY     TO UT4-IDPERSON-BUY                  
333401        END-IF                                                            
333501                                                                          
333601        MOVE TAB-BEART(TAB-IX)       TO UT4-BEART                         
333701                                                                          
333801        PERFORM IMS-GET-KVAH11                                            
333901        IF SEGMENT-FINNS                                                  
334001          IF INFO-DAREGDAT-9KOMPL > INL-DAINLEV(1:8)                      
334101**SHIPMENT IS DONE AT LEAST ONE DAY AFTER THE QUALITY REMARK              
334201             CONTINUE                                                     
334301          ELSE                                                            
334401             COMPUTE UT4-DAREGDAT =                                       
334501                     99999999 - INFO-DAREGDAT-9KOMPL                      
334601                                                                          
334701             MOVE +1 TO IX                                                
334801             PERFORM UNTIL IX > MAX-IX                                    
334901                MOVE INFO-TEKVAINF-EXT(IX)                                
335001                               TO UT4-INFO-TEKVAINF-EXT(IX)               
335101                INSPECT UT4-INFO-TEKVAINF-EXT(IX) REPLACING               
335201                      ALL '#' BY SPACE                                    
335301                INSPECT UT4-INFO-TEKVAINF-EXT(IX) REPLACING               
335401                      ALL 'Å' BY 'A'                                      
335501                INSPECT UT4-INFO-TEKVAINF-EXT(IX) REPLACING               
335601                      ALL 'Ä' BY 'A'                                      
335701                INSPECT UT4-INFO-TEKVAINF-EXT(IX) REPLACING               
335801                      ALL 'Ö' BY 'O'                                      
335901                ADD +1 TO IX                                              
336001             END-PERFORM                                                  
336101             PERFORM S97-PUT-DOC-LINE2                                    
336201          END-IF                                                          
336301        END-IF                                                            
336401        ADD +1 TO TAB-IX                                                  
336501                                                                          
336601     END-PERFORM                                                          
336701     .                                                                    
336801     EJECT                                                                
336901                                                                          
337001 HI-REC-UPDATERING SECTION.                                               
337101     MOVE 6302-IDDC-SEND TO WS-IDDC-SEND                                  
337201                            SEND-WS-IDDC                                  
337301     MOVE 6302-IDDISTR TO DIST35-IDDISTR                                  
337401                                                                          
337501     IF 6302-KDTRPSTA = ' ' OR 'I' OR 'A' OR 'C'                          
337601      PERFORM HIC-LOCAL-DATE-TIME                                         
337701      MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                           
337801      MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                           
337901      MOVE W-IDFAKT           TO W-IDFAKT-MIN                             
338001                                 W-IDFAKT-MAX                             
338101      MOVE SPACE              TO W-IDKUNDRF-MIN                           
338201      MOVE ZERO               TO W-IDKUNDNR-MIN                           
338301                                 W-IDKOLLI-MIN                            
338401      MOVE '9999999999'       TO W-IDKUNDRF-MAX                           
338501      MOVE 9999999            TO W-IDKUNDNR-MAX                           
338601      MOVE 99999              TO W-IDKOLLI-MAX                            
338701      MOVE 'R30'              TO W-IDPTYP                                 
338801      PERFORM IMS-06-GU-WLINLD01-FIRST-310                                
338901      PERFORM UNTIL SEGMENT-SAKNAS                                        
339001                                                                          
339101         MOVE SEQA-IDARTNR TO W-IDARTNR                                   
339201         MOVE SEQA-DAINLEV TO W-DAINLEV                                   
339301         MOVE NEJ          TO INLEV-SW                                    
339401         PERFORM IMS-GU-INLC1-WLINLC11                                    
339501         IF INL-TIINLMOT > ZERO                                           
339601           MOVE JA TO INLEV-SW                                            
339701         END-IF                                                           
339801         MOVE INL-IDDISTR              TO WS-SAP-IDDISTR                  
339901         MOVE INL-IDKUNDNR             TO WS-SAP-IDKUNDNR                 
340001         MOVE INL-PRARTNTO             TO WS-PRARTNTO                     
340101         MOVE INL-KVAVIS               TO W-KVAVIS                        
340201         MOVE 6302-IDDC-SEND TO WS-IDDC-SEND                              
340301                                WS-IDDC                                   
340401         IF REC-DCS-CDC                                                   
340501           PERFORM IMS-GU-WDK611                                          
340601         ELSE                                                             
340701           PERFORM IMS-11-GU-WDK711                                       
340801         END-IF                                                           
340901         IF SEGMENT-FINNS                                                 
341001            IF REC-DCS-CDC                                                
341101              MOVE CLAG-PRARTSTD TO WS-PRARTSTD                           
341201            ELSE                                                          
341301              MOVE SLAG-PRAVCOST TO WS-PRAVCOST                           
341401            END-IF                                                        
341501                                                                          
341601            MOVE WS-IDDC-SPAR  TO W-6301-IDDC                             
341701            PERFORM IMS-GHU-WL630111                                      
341801            MOVE 6302-IDDC-SEND TO WS-IDDC-SEND                           
341901                                   SEND-WS-IDDC                           
342001                                                                          
342101            IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                           
342201               MOVE SEND-WS-IDDC TO W-IDDC-B6                             
342301               PERFORM IMS-GU-WDB601-SEND                                 
342401            END-IF                                                        
342501*                                                                         
342601           IF ((SEND-DCS-CDC OR SEND-DCS-DDC)                             
342701           AND  REC-DCS-LAND-NON-VCC-OWNED)                               
342801           OR ((DIST35-VCC-NONVCC-REFILL OR                               
342901                DIST35-VCC-NONVCC-TRANSFER)                               
343001           AND  REC-DCS-LAND-NON-VCC-OWNED)                               
343101**** SO FAR IT'S ONLY THE BOUNCE FLOW TO US.                              
343201**** REST OF FLOWS TO US IS IN LAB.                                       
343301**** FOR BOUNCE FLOW TO US, NEED TO ADD THE LEVEL                         
343401**** DIST35-NONVCC-NDCUS-REFILL IN WWDIST35                               
343501           OR  (SEND-DCS-CDC                                              
343601           AND (REC-DCS-USA AND DIST35-NDCCN-NDCUS-REFILL))               
343701              PERFORM IMS-15-GET-WLARTC01                                 
343801              MOVE ART-KDPRODSL TO R8-EKH-KDPRODSL                        
343901**** WHEN FOUND A MISS CASE A BOOKING SHOULD NOT BE DONE                  
344001              IF 6302-KDTRPSTA = 'M'                                      
344101                CONTINUE                                                  
344201              ELSE                                                        
344301                IF REC-DCS-KDTRADP = 'BR12'                               
344401                  MOVE 'R' TO REQU-KDCMD                                  
344501                ELSE                                                      
344601                  PERFORM S07B-SKAPA-SAP-TRANS                            
344701                END-IF                                                    
344801              END-IF                                                      
344901           ELSE                                                           
345001             IF DIST35-NONVCC-VCC-REFILL OR                               
345101                DIST35-NONVCC-VCC-TRANSFER                                
345201                PERFORM IMS-15-GET-WLARTC01                               
345301                MOVE ART-KDPRODSL  TO EKH-KDPRODSL                        
345401                MOVE ART-KDSORT    TO WS-KDSORT                           
345501                PERFORM IMS-16-GET-WLARTC11                               
345601                MOVE CLAG-PRARTSTD TO WS-PRARTSTD                         
345701                PERFORM S07D-SKAPA-SAP-TRANS                              
345801             ELSE                                                         
345901                IF (SEND-DCS-CHINA OR SEND-DCS-USA)                       
346001                AND REC-DCS-CDC                                           
346101                AND INLEV-NEJ                                             
346201                  PERFORM IMS-15-GET-WLARTC01                             
346301                  MOVE ART-KDPRODSL TO EKH-KDPRODSL                       
346401                  MOVE ART-KDSORT   TO WS-KDSORT                          
346501                  PERFORM S07E-SKAPA-SAP-TRANS                            
346601                END-IF                                                    
346701             END-IF                                                       
346801           END-IF                                                         
346901          END-IF                                                          
347001         PERFORM IMS-25-GN-WDL6A1                                         
347101      END-PERFORM                                                         
347201     END-IF                                                               
347301                                                                          
347401     PERFORM S02-SKAPA-WDL6A1KY                                           
347501     MOVE 'R30' TO W-IDPTYP                                               
347601     PERFORM IMS-26-GU-WDL6A1                                             
347701                                                                          
347801     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
347901                                                                          
348001        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
348101        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
348201        MOVE SEQA-IDDC    TO W-IDDC                                       
348301                                                                          
348401        PERFORM IMS-09-GHU-INLC1-WLINLC11                                 
348501                                                                          
348601        MOVE NEJ          TO INLEV-SW                                     
348701        IF INL-TIINLMOT > ZERO                                            
348801          MOVE JA TO INLEV-SW                                             
348901        END-IF                                                            
349001        MOVE INL-PRARTNTO             TO WS-PRARTNTO                      
349101        MOVE INL-IDDISTR              TO WS-SAP-IDDISTR                   
349201        MOVE INL-IDKUNDNR             TO WS-SAP-IDKUNDNR                  
349301                                                                          
349401        MOVE INL-KVAVIS               TO W-KVAVIS                         
349501        MOVE '310'                    TO INL-IDPTYP                       
349601                                                                          
349701        MOVE W-IDDC                   TO W-IDDC-B6                        
349801        PERFORM HIC-LOCAL-DATE-TIME                                       
349901                                                                          
350001        MOVE W-DATUM-LOCAL            TO INL-TIINLMOT                     
350101        MOVE AKTUELL-TTMM-LOC         TO INL-TIINLMTI                     
350201        IF ETA-UPD-OK                                                     
350301          MOVE LETA-TIAAMMDD-SVAR     TO INL-TIBERANK                     
350401        END-IF                                                            
350501        PERFORM IMS-10-REPL-INLC1-WLINLC11                                
350601                                                                          
350701        IF REC-DCS-CDC                                                    
350801          PERFORM IMS-GHU-WDK611                                          
350901        ELSE                                                              
351001          PERFORM IMS-12-GHU-WDK711                                       
351101        END-IF                                                            
351201        IF SEGMENT-FINNS                                                  
351301           IF REC-DCS-CDC                                                 
351401             ADD W-KVAVIS TO CLAG-KVAKS-CDC                               
351501             MOVE CLAG-PRARTSTD TO WS-PRARTSTD                            
351601             SUBTRACT W-KVAVIS FROM CLAG-KVAKS-PAV                        
351701             PERFORM IMS-REPL-WDK611                                      
351801           ELSE                                                           
351901             ADD W-KVAVIS TO SLAG-KVAKS-SDC                               
352001             MOVE SLAG-PRAVCOST TO WS-PRAVCOST                            
352101             SUBTRACT W-KVAVIS FROM SLAG-KVAKS-PAV                        
352201             PERFORM IMS-13-REPL-WDK711                                   
352301           END-IF                                                         
352401           PERFORM HIA-SKAPA-SALDOLOGG                                    
352501*                                                                         
352601           MOVE WS-IDDC-SPAR  TO W-6301-IDDC                              
352701           PERFORM IMS-GHU-WL630111                                       
352801           MOVE 6302-IDDC-SEND TO WS-IDDC-SEND                            
352901                                  SEND-WS-IDDC                            
353001        END-IF                                                            
353101                                                                          
353201        PERFORM IMS-25-GN-WDL6A1                                          
353301     END-PERFORM                                                          
353401     .                                                                    
353501     EJECT                                                                
353601 HIA-SKAPA-SALDOLOGG SECTION.                                             
353701                                                                          
353801     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
353901     MOVE 9                         TO LOGG-IDSEKVNR                      
354001     MOVE W-IDDC                    TO LOGG-IDDC                          
354101     MOVE 'INBO'                    TO LOGG-IDHUVTYP                      
354201     MOVE '310'                     TO LOGG-IDSUBTYP                      
354301     MOVE 'WL010200'                TO LOGG-IDPGM                         
354401     MOVE 'WL02'                    TO LOGG-IDTRANS                       
354501     MOVE MSG-SIGNON-USERID         TO LOGG-IDUSER                        
354601     MOVE SPACE                     TO LOGG-REF                           
354701     MOVE W-IDFAKT                  TO LOGG-IDFAKT                        
354801     MOVE SPACE                     TO LOGG-IDLBBET                       
354901     MOVE ZERO                      TO LOGG-TIFAKT                        
355001     MOVE '+'                       TO LOGG-IDTECKEN-KVAKS                
355101     MOVE '-'                       TO LOGG-IDTECKEN-KVAKS-PAV            
355201     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
355301     MOVE SPACE                     TO LOGG-IDTECKEN-KVLS                 
355401     MOVE W-KVAVIS                  TO LOGG-KVART-SALDO                   
355501     IF REC-DCS-CDC                                                       
355601       COMPUTE LOGG-KVAKS           =  CLAG-KVAKS-CDC                     
355701                                    +  CLAG-KVAKS-T                       
355801       MOVE CLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                     
355901       MOVE CLAG-KVEFRS             TO LOGG-KVEFRS                        
356001       MOVE CLAG-KVLS               TO LOGG-KVLS                          
356101     ELSE                                                                 
356201       MOVE SLAG-KVAKS-SDC          TO LOGG-KVAKS                         
356301       MOVE SLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                     
356401       MOVE SLAG-KVEFRS             TO LOGG-KVEFRS                        
356501       MOVE SLAG-KVLS               TO LOGG-KVLS                          
356601     END-IF                                                               
356701     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
356801                                                                          
356901     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-LOGG-AAAAMMDD                 
357001     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
357101                                   - WS-LOGG-AAAAMMDD                     
357201     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
357301     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
357401                                   - WS-TTMMSSTH                          
357501                                                                          
357601     PERFORM IMS-18-ISRT-WDL901                                           
357701     IF SEGMENT-FINNS-REDAN                                               
357801       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
357901         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
358001         PERFORM IMS-18-ISRT-WDL901                                       
358101       END-PERFORM                                                        
358201     END-IF                                                               
358301     .                                                                    
358401     EJECT                                                                
358501 HIB-KOLLA-IDFAKT SECTION.                                                
358601                                                                          
358701     MOVE 'REC' TO SPAR-KDTRPSTA                                          
358801                                                                          
358901     MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                            
359001     MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                            
359101     MOVE W-IDFAKT           TO W-IDFAKT-MIN                              
359201                                W-IDFAKT-MAX                              
359301     MOVE SPACE              TO W-IDKUNDRF-MIN                            
359401     MOVE ZERO               TO W-IDKUNDNR-MIN                            
359501                                W-IDKOLLI-MIN                             
359601     MOVE '9999999999'       TO W-SEQA-IDKUNDRF-MAX                       
359701     MOVE 9999999            TO W-SEQA-IDKUNDNR-MAX                       
359801     MOVE 99999              TO W-SEQA-IDKOLLI-MAX                        
359901     MOVE 'R30'              TO W-IDPTYP                                  
360001                                                                          
360101     PERFORM IMS-27-GU-WDL6A1                                             
360201     PERFORM UNTIL (SEGMENT-SAKNAS) OR (W-KVANT-UPPD > 5)                 
360301        OR (BASEN-SLUT)                                                   
360401                                                                          
360501        IF SEGMENT-FINNS                                                  
360601           MOVE SEQA-IDARTNR TO W-IDARTNR                                 
360701           MOVE SEQA-DAINLEV TO W-DAINLEV                                 
360801           PERFORM IMS-09-GHU-INLC1-WLINLC11                              
360901                                                                          
361001           MOVE INL-IDDC   TO W-IDDC                                      
361101           MOVE INL-KVAVIS TO W-KVAVIS                                    
361201                                                                          
361301           IF INL-TIINLMOT = ZERO                                         
361401              IF REQU-IDUSER-003 = ALL '+' OR SPACE                       
361501                 CONTINUE                                                 
361601              ELSE                                                        
361701                 MOVE WS-IDUSER-003 TO INL-IDUSER-003                     
361801              END-IF                                                      
361901                                                                          
362001              MOVE W-IDDC           TO W-IDDC-B6                          
362101              PERFORM HIC-LOCAL-DATE-TIME                                 
362201                                                                          
362301              MOVE W-DATUM-LOCAL    TO INL-TIINLMOT                       
362401              MOVE AKTUELL-TTMM-LOC TO INL-TIINLMTI                       
362501              IF ETA-UPD-OK                                               
362601                MOVE LETA-TIAAMMDD-SVAR TO INL-TIBERANK                   
362701              END-IF                                                      
362801              PERFORM IMS-10-REPL-INLC1-WLINLC11                          
362901              MOVE 'MIS' TO SPAR-KDTRPSTA                                 
363001                                                                          
363101              ADD +1 TO W-KVANT-UPPD                                      
363201           ELSE                                                           
363301              MOVE 'MIS' TO SPAR-KDTRPSTA                                 
363401           END-IF                                                         
363501        END-IF                                                            
363601                                                                          
363701        PERFORM IMS-28-GN-WDL6A1                                          
363801     END-PERFORM                                                          
363901                                                                          
364001     IF SEGMENT-FINNS                                                     
364101        MOVE JA TO ATERHOPP-SW                                            
364201     ELSE                                                                 
364301        MOVE NEJ TO ATERHOPP-SW                                           
364401     END-IF                                                               
364501                                                                          
364601     MOVE 6302-DABERANK(3:6) TO WS-DABERANK                               
364701     IF ETA-UPD-OK AND                                                    
364801       (LETA-TIAAMMDD-SVAR NOT = WS-DABERANK)                             
364901       MOVE WS-IDDC-SPAR TO W-6301-IDDC                                   
365001       PERFORM IMS-01-GHU-WL630111                                        
365101       IF SEGMENT-FINNS                                                   
365201         PERFORM IMS-03-DLET-WL630111                                     
365301         MOVE SPAR-KDTRPSTA TO 6302-KDTRPSTA                              
365401         MOVE LETA-TIAAMMDD-SVAR TO 6302-DABERANK                         
365501         MOVE 20 TO 6302-DABERANK (1:2)                                   
365601         MOVE NEJ   TO 6302-FLMANETA                                      
365701         MOVE SPACE TO 6302-IDUSER-MANETA                                 
365801         PERFORM IMS-02-ISRT-WL630111                                     
365901       END-IF                                                             
366001     ELSE                                                                 
366101       MOVE WS-IDDC-SPAR TO W-6301-IDDC                                   
366201       PERFORM IMS-01-GHU-WL630111                                        
366301       IF SEGMENT-FINNS                                                   
366401         MOVE SPAR-KDTRPSTA TO 6302-KDTRPSTA                              
366501         PERFORM IMS-02-REPL-WL630111                                     
366601       END-IF                                                             
366701     END-IF                                                               
366801     .                                                                    
366901     EJECT                                                                
367001 HIC-LOCAL-DATE-TIME SECTION.                                             
367101                                                                          
367201******** ADAPT DATE AND TIME FOR TIMEZONES                                
367301     PERFORM IMS-22-GU-WDB601                                             
367401                                                                          
367501     MOVE '011'                TO MSGI-KDCALL                             
367601     MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                           
367701     MOVE DCS-IDDC             TO MSGI-IDDC                               
367801     MOVE DAGENS-DATUM-AAMMDD  TO MSGI-TILOKDAT                           
367901     MOVE DAGENS-TID           TO MSGI-TILOKTID                           
368001     CALL WL01TIDZ USING          MSGI-WL01TIDZ                           
368101       MOVE MSGI-TILOKDAT(1:6) TO W-DATUM-Y                               
368201       MOVE MSGI-TILOKTID(1:4) TO AKTUELL-TID-X(1:4)                      
368301     .                                                                    
368401     EJECT                                                                
368501 HJ-CREATE-CHINESE-LABELS SECTION.                                        
368601     PERFORM S02-SKAPA-WDL6A1KY                                           
368701     PERFORM IMS-GU-WLINLD01                                              
368801**   IF SEGMENT-FINNS                                                     
368901     PERFORM UNTIL SEGMENT-SAKNAS                                         
369001       MOVE SEQA-IDARTNR   TO W-IDARTNR                                   
369101                              LABL-IDARTNR (INDX-LINE)                    
369201       MOVE SEQA-IDDC      TO W-IDDC                                      
369301                              LABL-IDDC                                   
369401       MOVE SEQA-DAINLEV   TO W-DAINLEV                                   
369501       PERFORM IMS-09-GHU-INLC1-WLINLC11                                  
369601       IF SEGMENT-FINNS                                                   
369701          IF INL-KVAVIS > 100                                             
369801            MOVE 1            TO LABL-KVANTAL(INDX-LINE)                  
369901          ELSE                                                            
370001            MOVE INL-KVAVIS   TO LABL-KVANTAL(INDX-LINE)                  
370101          END-IF                                                          
370201          PERFORM IMS-GU-WDK611                                           
370301          MOVE CLAG-KDARTURS  TO LABL-KDARTURS (INDX-LINE)                
370401       END-IF                                                             
370501       PERFORM IMS-GN-WLINLD01                                            
370601       ADD 1 TO INDX-LINE                                                 
370701     END-PERFORM                                                          
370801**   END-IF                                                               
370901     .                                                                    
371001     EJECT                                                                
371101                                                                          
371201 HK-CREATE-KOREAN-LABELS SECTION.                                         
371301     PERFORM S02-SKAPA-WDL6A1KY                                           
371401     PERFORM IMS-GU-WLINLD01                                              
371501**   IF SEGMENT-FINNS                                                     
371601     MOVE REQU-IDDC-KEY TO KLBL-IDDC                                      
371701     IF INDX-LINE > 500                                                   
371801        MOVE +1 TO INDX-LINE                                              
371901     END-IF                                                               
372001                                                                          
372101     PERFORM UNTIL SEGMENT-SAKNAS                                         
372201       MOVE SEQA-IDARTNR   TO W-IDARTNR                                   
372301                              KLBL-IDARTNR (INDX-LINE)                    
372401       MOVE SEQA-IDDC      TO W-IDDC                                      
372501       MOVE SEQA-DAINLEV   TO W-DAINLEV                                   
372601       PERFORM IMS-09-GHU-INLC1-WLINLC11                                  
372701       IF SEGMENT-FINNS                                                   
372801*         MOVE 1             TO KLBL-KVANTAL(INDX-LINE)                   
372901          MOVE INL-KVAVIS    TO KLBL-KVAVIS(INDX-LINE)                    
373001          MOVE ZERO          TO KLBL-IDORDNR7(INDX-LINE)                  
373101          MOVE INL-IDORDNR5  TO KLBL-IDORDNR7(INDX-LINE)                  
373201          MOVE INL-IDKOLLI   TO KLBL-IDKOLLI(INDX-LINE)                   
373301          PERFORM IMS-GET-WLARTC01                                        
373401*         MOVE ART-KDSORT    TO KLBL-KDSORT(INDX-LINE)                    
373501          IF NDC-KR                                                       
373601           IF ART-IDFKNGRP = 5222 OR 8841 OR 8842 OR 3521 OR              
373701                             3531 OR 3532 OR 8417 OR 1912 OR              
373801                             8441 OR 8431 OR 8121 OR 8433 OR              
373901                             8445 OR 8443 OR 5115 OR 5125 OR              
374001                             7713 OR 7703 OR 8369 OR 3551 OR              
374101                             3567 OR 3514 OR 8361                         
374201             MOVE '***'      TO KLBL-KRKC(INDX-LINE)                      
374301           ELSE                                                           
374401             MOVE '   '      TO KLBL-KRKC(INDX-LINE)                      
374501           END-IF                                                         
374601          ELSE                                                            
374701             MOVE '   '      TO KLBL-KRKC(INDX-LINE)                      
374801          END-IF                                                          
374901          IF REC-DCS-CDC                                                  
375001            PERFORM IMS-GET-WLARTC11                                      
375101            MOVE CLAG-ADLAGOMR  TO KLBL-ADLAGOMR(INDX-LINE)               
375201            MOVE CLAG-ADGANG    TO KLBL-ADGANG(INDX-LINE)                 
375301            MOVE CLAG-ADPLATS   TO KLBL-ADPLATS(INDX-LINE)                
375401            MOVE CLAG-KVROS     TO KLBL-KVOKS(INDX-LINE)                  
375501          ELSE                                                            
375601            PERFORM IMS-11-GU-WDK711                                      
375701            MOVE SLAG-ADLAGOMR  TO KLBL-ADLAGOMR(INDX-LINE)               
375801            MOVE SLAG-ADGANG    TO KLBL-ADGANG(INDX-LINE)                 
375901            MOVE SLAG-ADPLATS   TO KLBL-ADPLATS(INDX-LINE)                
376001            COMPUTE KLBL-KVOKS(INDX-LINE) = SLAG-KVROS-BULK               
376101                                        + SLAG-KVROS-DAG                  
376201          END-IF                                                          
376301       END-IF                                                             
376401       PERFORM IMS-GN-WLINLD01                                            
376501       ADD 1 TO INDX-LINE                                                 
376601     END-PERFORM                                                          
376701**   END-IF                                                               
376801     .                                                                    
376901     EJECT                                                                
377001                                                                          
377101 HL-LOC-UPPDATERING SECTION.                                              
377201     PERFORM S02-SKAPA-WDL6A1KY                                           
377301     PERFORM IMS-GU-WLINLD01                                              
377401                                                                          
377501     PERFORM UNTIL SEGMENT-SAKNAS                                         
377601                                                                          
377701        MOVE SEQA-IDARTNR TO W-IDARTNR                                    
377801        MOVE SEQA-DAINLEV TO W-DAINLEV                                    
377901                                                                          
378001        PERFORM IMS-GHU-INLC1-WLINLC11                                    
378101        IF SEGMENT-FINNS                                                  
378201           IF REQU-IDUSER-003 = ALL '+' OR SPACE                          
378301              CONTINUE                                                    
378401           ELSE                                                           
378501              MOVE WS-IDUSER-003 TO INL-IDUSER-003                        
378601           END-IF                                                         
378701           MOVE REQU-ADINLOMR(INDX) TO INL-ADINLOMR                       
378801           PERFORM IMS-REPL-INLC1-WLINLC11                                
378901        END-IF                                                            
379001        PERFORM IMS-GN-WLINLD01                                           
379101     END-PERFORM                                                          
379201     .                                                                    
379301 HM-REC-DCCROS SECTION.                                                   
379401                                                                          
379501     MOVE REQU-KDPGMACT TO 602CROS-KDPGMACT                               
379601     CALL W602CROS USING 602CROS-W602CROS                                 
379701                         CROS-WDE6-PCB CROS-WDE4-PCB                      
379801                         CROS-WDE41-PCB                                   
379901                         CROS-WDE1-PCB CROS-WDE6F-PCB                     
380001                         CROS-WDB6-PCB                                    
380101                         PLATS-DM-PCB  PLATS-DN-PCB                       
380201                         PLATS-DP-PCB  PLATS-DO-PCB                       
380301                         PLATS-WDE6C-PCB PLATS-GMTC-PCB                   
380401                         PLATS-WDB6-PCB                                   
380501     IF 602CROS-KDSVAR = ' '                                              
380601      MOVE UPDATE-DONE TO RESP-IDMSG-INFO                                 
380701      IF 602CROS-IDMSG-INFO > ZERO                                        
380801       MOVE 602CROS-IDMSG-INFO TO RESP-IDMSG-INFO                         
380901*      MOVE 602CROS-NEXT-TRPTNR-CROSS TO RESP-IDELMT-ERROR                
381001       MOVE 602CROS-IDTRPTNR-CROSS (INDX) TO RESP-IDELMT-ERROR            
381101                                             MSG-CONV-IDELMT              
381201      END-IF                                                              
381301     END-IF                                                               
381401     .                                                                    
381501     EJECT                                                                
381601 I-TRIGGER-BR-TRANS SECTION.                                              
381701     IF REQU-KDCMD = 'R'                                                  
381801       MOVE WS-IDDC-SPAR TO W-6301-IDDC                                   
381901       PERFORM IMS-GU-WL630111                                            
382001       MOVE 6302-IDDC-SEND TO WS-IDDC-SEND                                
382101                              SEND-WS-IDDC                                
382201       MOVE 6302-IDDISTR   TO DIST35-IDDISTR                              
382301       MOVE W-IDDC         TO W-IDDC-B6                                   
382401       PERFORM HIC-LOCAL-DATE-TIME                                        
382501       IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                
382601          MOVE SEND-WS-IDDC TO W-IDDC-B6                                  
382701          PERFORM IMS-GU-WDB601-SEND                                      
382801       END-IF                                                             
382901                                                                          
383001       MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                          
383101       MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                          
383201       MOVE W-IDFAKT           TO W-IDFAKT-MIN                            
383301                                  W-IDFAKT-MAX                            
383401       MOVE SPACE              TO W-IDKUNDRF-MIN                          
383501       MOVE ZERO               TO W-IDKUNDNR-MIN                          
383601                                  W-IDKOLLI-MIN                           
383701       MOVE '9999999999'       TO W-IDKUNDRF-MAX                          
383801       MOVE 9999999            TO W-IDKUNDNR-MAX                          
383901       MOVE 99999              TO W-IDKOLLI-MAX                           
384001       MOVE 'R30'              TO W-IDPTYP                                
384101       PERFORM IMS-06-GU-WLINLD01-FIRST-310                               
384201       PERFORM UNTIL SEGMENT-SAKNAS                                       
384301         MOVE SEQA-IDARTNR TO W-IDARTNR                                   
384401         MOVE SEQA-DAINLEV TO W-DAINLEV                                   
384501         PERFORM IMS-GU-INLC1-WLINLC11                                    
385002         MOVE INL-IDDISTR              TO WS-SAP-IDDISTR                  
385102         MOVE INL-IDKUNDNR             TO WS-SAP-IDKUNDNR                 
385202         MOVE INL-PRARTNTO             TO WS-PRARTNTO                     
385302         MOVE INL-KVAVIS               TO W-KVAVIS                        
385402         MOVE INL-IDORDNR5             TO WS-IDORDER                      
385502         MOVE INL-IDKOLLI              TO WS-IDKOLLI                      
385602         PERFORM IMS-15-GET-WLARTC01                                      
385702         MOVE ART-KDPRODSL TO R8-EKH-KDPRODSL                             
385802         MOVE ART-KDSORT   TO WS-KDSORT                                   
385902         PERFORM IMS-11-GU-WDK711                                         
386002         MOVE SLAG-PRAVCOST TO WS-PRAVCOST                                
386102         PERFORM S07B-SKAPA-SAP-TRANS                                     
386301         PERFORM IMS-25-GN-WDL6A1                                         
386401       END-PERFORM                                                        
386501                                                                          
386601       MOVE LOW-VALUE          TO W-WDL6A1KY-MIN                          
386701       MOVE HIGH-VALUE         TO W-WDL6A1KY-MAX                          
386801       MOVE W-IDFAKT           TO W-IDFAKT-MIN                            
386901                                  W-IDFAKT-MAX                            
387001       MOVE SPACE              TO W-IDKUNDRF-MIN                          
387101       MOVE ZERO               TO W-IDKUNDNR-MIN                          
387201                                  W-IDKOLLI-MIN                           
387301       MOVE '9999999999'       TO W-IDKUNDRF-MAX                          
387401       MOVE 9999999            TO W-IDKUNDNR-MAX                          
387501       MOVE 99999              TO W-IDKOLLI-MAX                           
387601       MOVE '310'              TO W-IDPTYP                                
387701       PERFORM IMS-06-GU-WLINLD01-FIRST-310                               
387801       PERFORM UNTIL SEGMENT-SAKNAS                                       
387901         MOVE SEQA-IDARTNR TO W-IDARTNR                                   
388001         MOVE SEQA-DAINLEV TO W-DAINLEV                                   
388101         PERFORM IMS-GU-INLC1-WLINLC11                                    
388201         MOVE INL-IDDISTR              TO WS-SAP-IDDISTR                  
388301         MOVE INL-IDKUNDNR             TO WS-SAP-IDKUNDNR                 
388401         MOVE INL-PRARTNTO             TO WS-PRARTNTO                     
388501         MOVE INL-KVAVIS               TO W-KVAVIS                        
388601         MOVE INL-IDORDNR5             TO WS-IDORDER                      
388701         MOVE INL-IDKOLLI              TO WS-IDKOLLI                      
388801         PERFORM IMS-15-GET-WLARTC01                                      
388901         MOVE ART-KDPRODSL TO R8-EKH-KDPRODSL                             
389001         MOVE ART-KDSORT   TO WS-KDSORT                                   
389101         PERFORM IMS-11-GU-WDK711                                         
389201         MOVE SLAG-PRAVCOST TO WS-PRAVCOST                                
389301         PERFORM S07B-SKAPA-SAP-TRANS                                     
389401         PERFORM IMS-25-GN-WDL6A1                                         
389501       END-PERFORM                                                        
389601       IF REC-DCS-KDTRADP = 'BR12' AND FIRST-REC-TRANS                    
389701         PERFORM S23-SEND-CLOSE                                           
389801       END-IF                                                             
389901     END-IF                                                               
390001                                                                          
390101     IF REQU-KDCMD = 'L'                                                  
390201       MOVE 6302-IDDC-SEND TO WS-IDDC-SEND                                
390301                              SEND-WS-IDDC                                
390401       MOVE 6302-IDDISTR   TO DIST35-IDDISTR                              
390501       MOVE W-IDDC         TO W-IDDC-B6                                   
390601       PERFORM HIC-LOCAL-DATE-TIME                                        
390701       IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                
390801          MOVE SEND-WS-IDDC TO W-IDDC-B6                                  
390901          PERFORM IMS-GU-WDB601-SEND                                      
391001       END-IF                                                             
391101       MOVE +1        TO INDX                                             
391201       PERFORM UNTIL INDX  > MAX-INDX                                     
391301         IF REQU-FLLOS(INDX) = YES                                        
391401           PERFORM S19-SKAPA-WDL6CSEQ                                     
391501           PERFORM IMS-29-GU-INLC2-WLINLC11-F                             
391601           PERFORM UNTIL SEGMENT-SAKNAS                                   
391702             IF W-IDFAKT = INL-IDFAKT AND                                 
391802                REQU-IDKUNDRF(INDX) = INL-IDKUNDRF AND                    
391902                REQU-IDKUNDNR(INDX) = INL-IDKUNDNR AND                    
392002                REQU-IDKOLLI(INDX) = INL-IDKOLLI                          
392003               PERFORM IMS-31-GNP-INLC2-WLINLC01                          
392004               MOVE L601-ART-IDARTNR  TO W-IDARTNR                        
392102               MOVE INL-KVAVIS        TO W-KVAVIS                         
392202               MOVE INL-IDDC          TO W-IDDC                           
392302               MOVE INL-KDFRAKT       TO W-KDFRAKT                        
392402               MOVE INL-IDDISTR       TO WS-SAP-IDDISTR                   
392502               MOVE INL-IDKUNDNR      TO WS-SAP-IDKUNDNR                  
392602               MOVE INL-PRARTNTO      TO WS-PRARTNTO                      
392702               MOVE INL-IDORDNR5      TO WS-IDORDER                       
392802               MOVE INL-IDKOLLI       TO WS-IDKOLLI                       
392902               PERFORM IMS-GET-WLARTC01                                   
393002               MOVE ART-KDPRODSL      TO R8-EKH-KDPRODSL                  
393102               MOVE ART-KDSORT        TO WS-KDSORT                        
393202               PERFORM IMS-11-GU-WDK711                                   
393302               MOVE SLAG-PRAVCOST TO  WS-PRAVCOST                         
393402               PERFORM S07A-SKAPA-SAP-TRANS                               
393502             END-IF                                                       
393601             PERFORM IMS-30-GN-INLC2-WLINLC11                             
393701           END-PERFORM                                                    
393801         END-IF                                                           
393901         ADD +1 TO INDX                                                   
394001       END-PERFORM                                                        
394101       IF REC-DCS-KDTRADP = 'BR12' AND FIRST-REC-TRANS                    
394201         PERFORM S23-SEND-CLOSE                                           
394301       END-IF                                                             
394401     END-IF                                                               
394501     .                                                                    
394601     EJECT                                                                
394701 K-OMSTART-EGEN-TRANS SECTION.                                            
394801                                                                          
394901     PERFORM S15-SEND-OPEN                                                
395001     PERFORM S16-SEND-PUT                                                 
395101     PERFORM S17-SEND-CLOSE                                               
395201     .                                                                    
395301     EJECT                                                                
395401 S01-LAS-FRAM-ARTIKEL    SECTION.                                         
395501                                                                          
395601      MOVE LOW-VALUE                  TO W-WDL6A1KY-MIN                   
395701      MOVE HIGH-VALUE                 TO W-WDL6A1KY-MAX                   
395801      MOVE INL-IDFAKT                 TO W-IDFAKT-MIN                     
395901                                         W-IDFAKT-MAX                     
396001      MOVE INL-IDKUNDRF               TO W-IDKUNDRF-MIN                   
396101                                         W-IDKUNDRF-MAX                   
396201      MOVE INL-IDKUNDNR               TO W-IDKUNDNR-MIN                   
396301                                         W-IDKUNDNR-MAX                   
396401      MOVE INL-IDKOLLI                TO W-IDKOLLI-MIN                    
396501                                         W-IDKOLLI-MAX                    
396601      IF W-ANTAL-LASN = ZERO                                              
396701         PERFORM IMS-04-GU-WLINLD01                                       
396801      ELSE                                                                
396901         PERFORM IMS-05-GN-WLINLD01                                       
397001      END-IF                                                              
397101                                                                          
397201      MOVE SEQA-IDARTNR           TO W-IDARTNR                            
397301      MOVE INL-IDDC               TO W-IDDC                               
397401     .                                                                    
397501     EJECT                                                                
397601 S02-SKAPA-WDL6A1KY SECTION.                                              
397701                                                                          
397801     MOVE LOW-VALUE                  TO W-WDL6A1KY-MIN                    
397901     MOVE HIGH-VALUE                 TO W-WDL6A1KY-MAX                    
398001     MOVE W-IDFAKT                   TO W-IDFAKT-MIN                      
398101                                        W-IDFAKT-MAX                      
398201     INSPECT REQU-IDKUNDRF(INDX) REPLACING LEADING SPACE BY ZERO          
398301     MOVE REQU-IDKUNDRF(INDX)        TO W-IDKUNDRF-MIN                    
398401                                        W-IDKUNDRF-MAX                    
398501     INSPECT REQU-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO          
398601     MOVE REQU-IDKUNDNR(INDX)         TO W-IDKUNDNR-MIN                   
398701                                         W-IDKUNDNR-MAX                   
398801     INSPECT REQU-IDKOLLI(INDX) REPLACING LEADING SPACE BY ZERO           
398901     MOVE REQU-IDKOLLI(INDX)         TO W-IDKOLLI-MIN                     
399001                                        W-IDKOLLI-MAX                     
399101     .                                                                    
399201     EJECT                                                                
399301 S03-RAKNA-ARTIKLAR SECTION.                                              
399401                                                                          
399501     MOVE LOW-VALUE                  TO W-WDL6A1KY-MIN                    
399601     MOVE HIGH-VALUE                 TO W-WDL6A1KY-MAX                    
399701     MOVE W-IDFAKT                   TO W-IDFAKT-MIN                      
399801                                        W-IDFAKT-MAX                      
399901     INSPECT REQU-IDKUNDRF(INDX) REPLACING LEADING SPACE BY ZERO          
400001     MOVE REQU-IDKUNDRF(INDX)        TO W-IDKUNDRF-MIN                    
400101                                        W-IDKUNDRF-MAX                    
400201     INSPECT REQU-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO          
400301     MOVE REQU-IDKUNDNR(INDX)         TO W-IDKUNDNR-MIN                   
400401                                         W-IDKUNDNR-MAX                   
400501     INSPECT REQU-IDKOLLI(INDX) REPLACING LEADING SPACE BY ZERO           
400601     MOVE REQU-IDKOLLI(INDX)         TO W-IDKOLLI-MIN                     
400701                                        W-IDKOLLI-MAX                     
400801                                                                          
400901     MOVE ZERO TO W-KVRADER                                               
401001     PERFORM IMS-04-GU-WLINLD01                                           
401101     PERFORM  UNTIL SEGMENT-SAKNAS                                        
401201        ADD +1 TO W-KVRADER                                               
401301        PERFORM IMS-05-GN-WLINLD01                                        
401401     END-PERFORM                                                          
401501     .                                                                    
401601     EJECT                                                                
401701 S04-SKAPA-EKOTRANS-A03 SECTION.                                          
401801                                                                          
401901     MOVE WS-IDDC-SEND          TO SEND-WS-IDDC                           
402001     IF SEND-WS-IDDC NOT = SEND-DCS-IDDC                                  
402101        MOVE SEND-WS-IDDC TO W-IDDC-B6                                    
402201        PERFORM IMS-GU-WDB601-SEND                                        
402301     END-IF                                                               
402401     IF SEND-DCS-CDC OR SEND-DCS-DDC                                      
402501        MOVE 6302-IDDISTR       TO DIST35-IDDISTR                         
402601        IF DIST35-REFILL-NA                                               
402701           MOVE 'T10'           TO EKOTRA03-KDEKOHT                       
402801           IF DIST35-CDC-NDC51-REFILL                                     
402901              MOVE 54           TO EKOTRA03-IDFTG                         
403001           ELSE                                                           
403101              MOVE 53           TO EKOTRA03-IDFTG                         
403201           END-IF                                                         
403301        ELSE                                                              
403401           IF DIST35-REFILL-NA-JAP                                        
403501              MOVE 'I20'        TO EKOTRA03-KDEKOHT                       
403601              IF DIST35-JAP-NDC41-REFILL                                  
403701              OR DIST35-JAP-NDC43-REFILL                                  
403801              OR DIST35-JAP-NDC44-REFILL                                  
403901                 MOVE 53         TO EKOTRA03-IDFTG                        
404001              ELSE                                                        
404101                 MOVE 54         TO EKOTRA03-IDFTG                        
404201              END-IF                                                      
404301           END-IF                                                         
404401        END-IF                                                            
404501     ELSE                                                                 
404601        IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                             
404701           AND (DCS-NDC-NA AND DCS-CANADA)                                
404801             MOVE 'T50'      TO EKOTRA03-KDEKOHT                          
404901             MOVE 54          TO EKOTRA03-IDFTG                           
405001        ELSE                                                              
405101           IF (SEND-DCS-NDC-NA AND SEND-DCS-CANADA)                       
405201           AND (DCS-NDC-NA AND DCS-USA)                                   
405301             MOVE 'T40'      TO EKOTRA03-KDEKOHT                          
405401             MOVE 53          TO EKOTRA03-IDFTG                           
405501           ELSE                                                           
405601              IF (SEND-DCS-NDC-NA AND SEND-DCS-USA)                       
405701              AND (DCS-NDC-NA AND DCS-USA)                                
405801                  MOVE 'T30' TO EKOTRA03-KDEKOHT                          
405901                  MOVE 53    TO EKOTRA03-IDFTG                            
406001              END-IF                                                      
406101           END-IF                                                         
406201        END-IF                                                            
406301     END-IF                                                               
406401                                                                          
406501     MOVE 'A03'           TO EKOTRA03-IDPTYP                              
406601     MOVE JA              TO WS-A03-SKAPAD                                
406701     .                                                                    
406801     EJECT                                                                
406901 S041-SKRIV-EKOTRANS-A03 SECTION.                                         
407001                                                                          
407101     MOVE 'WL010200'        TO FIL-IDPGM IN FIL-WDR801                    
407201     MOVE W-DATUM           TO FIL-TIREGDAT                               
407301     ADD +1                 TO W-TIKLOCK                                  
407401     MOVE W-TIKLOCK         TO FIL-TIKLOCK IN FIL-WDR801                  
407501     ADD +1                 TO W-IDSEKVNR-A03                             
407601     MOVE W-IDSEKVNR-A03    TO FIL-IDSEKVNR IN FIL-WDR801                 
407701     MOVE 'W510'            TO FIL-CT-IDSYSTEM IN FIL-WDR801              
407801     MOVE 'A03'             TO FIL-CT-IDPTYP IN FIL-WDR801                
407901     MOVE ' '               TO FIL-CT-IDVTYP IN FIL-WDR801                
408001     MOVE EKOTRA03-W510A03  TO FIL-WDR801-DATA                            
408101                                                                          
408201     PERFORM IMS-ISRT-FILB01                                              
408301                                                                          
408401     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
408501        ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                              
408601        PERFORM IMS-ISRT-FILB01                                           
408701     END-PERFORM                                                          
408801     MOVE NEJ               TO WS-A03-SKAPAD                              
408901     .                                                                    
409001     EJECT                                                                
409101 S05-SOEK-REMARKUP SECTION.                                               
409201                                                                          
409301     MOVE 1   TO WS-INDX                                                  
409401                 WS-MARKUP                                                
409501     MOVE NEJ TO WS-PROD                                                  
409601                                                                          
409701     PERFORM UNTIL PRODKOD-FINNS OR WS-INDX > MARKUP-TAB-MAX              
409801       IF MARKUP-LPC (WS-INDX) = CLAG-KDPSLLOC                            
409901         MOVE JA               TO WS-PROD                                 
410001       ELSE                                                               
410101         ADD 1                 TO WS-INDX                                 
410201       END-IF                                                             
410301     END-PERFORM                                                          
410401                                                                          
410501     IF PRODKOD-FINNS                                                     
410601       IF SPAR-IDDC NOT = SPAR-DCS-IDDC                                   
410701          MOVE SPAR-IDDC TO W-IDDC-B6                                     
410801          PERFORM IMS-GU-WDB601-SPAR                                      
410901       END-IF                                                             
411001       IF SPAR-DCS-NDC-NA AND SPAR-DCS-USA                                
411101         MOVE MARKUP-FAKTOR-USA (WS-INDX)                                 
411201                               TO WS-MARKUP                               
411301       ELSE                                                               
411401         MOVE MARKUP-FAKTOR-CAN (WS-INDX)                                 
411501                               TO WS-MARKUP                               
411601       END-IF                                                             
411701     END-IF                                                               
411801     .                                                                    
411901     EJECT                                                                
412001 S06-SKAPA-SALDOLOGG SECTION.                                             
412101                                                                          
412201     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
412301                                                                          
412401     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-LOGG-AAAAMMDD                 
412501     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
412601                                   - WS-LOGG-AAAAMMDD                     
412701     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
412801     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
412901                                   - WS-TTMMSSTH                          
413001     MOVE 9                        TO LOGG-IDSEKVNR                       
413101     MOVE W-IDDC                   TO LOGG-IDDC                           
413201     MOVE 'INBO'                   TO LOGG-IDHUVTYP                       
413301     MOVE 'R32'                    TO LOGG-IDSUBTYP                       
413401     MOVE 'WL010200'               TO LOGG-IDPGM                          
413501     MOVE 'L102'                   TO LOGG-IDTRANS                        
413601     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
413701     MOVE SPACE                    TO LOGG-REF                            
413801     MOVE W-IDFAKT                 TO LOGG-IDFAKT                         
413901     MOVE W-KVAVIS                 TO LOGG-KVART-SALDO                    
414001     MOVE ' '                      TO LOGG-IDTECKEN-KVEFRS                
414101     IF REC-DCS-CDC                                                       
414201       COMPUTE LOGG-KVAKS          =  CLAG-KVAKS-CDC                      
414301                                   +  CLAG-KVAKS-T                        
414401       MOVE CLAG-KVAKS-PAV         TO LOGG-KVAKS-PAV                      
414501       MOVE CLAG-KVEFRS            TO LOGG-KVEFRS                         
414601       MOVE CLAG-KVLS              TO LOGG-KVLS                           
414701     ELSE                                                                 
414801       MOVE SLAG-KVAKS-SDC           TO LOGG-KVAKS                        
414901       MOVE SLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                    
415001       MOVE SLAG-KVEFRS              TO LOGG-KVEFRS                       
415101       MOVE SLAG-KVLS                TO LOGG-KVLS                         
415201     END-IF                                                               
415301     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
415401                                                                          
415501     PERFORM IMS-18-ISRT-WDL901                                           
415601     IF SEGMENT-FINNS-REDAN                                               
415701       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
415801         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
415901         PERFORM IMS-18-ISRT-WDL901                                       
416001       END-PERFORM                                                        
416101     END-IF                                                               
416201     .                                                                    
416301     EJECT                                                                
416401 S07A-SKAPA-SAP-TRANS SECTION.                                            
416501***** MAPPING OF SAP TRANSACTIONS                                         
416601                                                                          
416701     MOVE 'WL010200'                  TO FIL-IDPGM IN FIL-WDR801          
416801     ACCEPT FIL-TIREGDAT IN FIL-WDR801 FROM DATE                          
416901     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
417001                                         R8-EKH-DAVERDAT                  
417101     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
417201     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR801        
417301     ADD +1                           TO W-IDSEKVNR-SAP                   
417401     MOVE W-IDSEKVNR-SAP             TO FIL-IDSEKVNR IN FIL-WDR801        
417501     IF (REC-DCS-LAND-NON-VCC-OWNED AND                                   
417601        (SEND-DCS-CDC OR SEND-DCS-DDC))                                   
417701     OR (REC-DCS-LAND-NON-VCC-OWNED AND                                   
417801        (DIST35-VCC-NONVCC-REFILL OR                                      
417901         DIST35-VCC-NONVCC-TRANSFER))                                     
418001     OR (AKTUELLT-LAND-USA   AND SEND-DCS-CDC)                            
418101       MOVE '102'                     TO R8-EKH-KDEKHHT                   
418201**** FOR THE BOUNCE FLOW                                                  
418301       IF DIST35-NONVCC-NONVCC-REFILL OR                                  
418401          DIST35-NONVCC-NONVCC-TRANSFER                                   
418501         MOVE '132'                   TO R8-EKH-KDEKSHT                   
418601       ELSE                                                               
418701         MOVE '122'                   TO R8-EKH-KDEKSHT                   
418801       END-IF                                                             
418901       MOVE WS-PRAVCOST               TO R8-EKH-PRARTSTD                  
419001     ELSE                                                                 
419101**** WITHIN A COUNTRY                                                     
419201       MOVE '503'                     TO R8-EKH-KDEKHHT                   
419301       MOVE '501'                     TO R8-EKH-KDEKSHT                   
419401       MOVE WS-IDDC-SEND TO W-IDDC                                        
419501       PERFORM IMS-12-GHU-WDK711                                          
419601       MOVE SLAG-PRAVCOST TO WS-PRAVCOST                                  
419701       MOVE WS-PRAVCOST               TO R8-EKH-PRARTSTD                  
419801     END-IF                                                               
419901     MOVE 'DET  '                     TO R8-EKH-KDEKNIVA                  
420001     MOVE WS-IDDC-SEND                TO SEND-WS-IDDC                     
420101     MOVE WS-IDDC-SEND                TO R8-EKH-IDDC-SEND                 
420201     MOVE WS-IDDC-SPAR                TO R8-EKH-IDDC-REC                  
420301     MOVE WS-SAP-IDDISTR              TO R8-EKH-IDDISTR                   
420401     MOVE WS-SAP-IDKUNDNR             TO R8-EKH-IDKUNDNR                  
420501*******************************                                           
420601     MOVE ZERO TO NOLL-RAKNARE                                            
420701     MOVE W-IDFAKT                   TO WS-SAP-IDFAKT                     
420801     MOVE WS-SAP-IDFAKT              TO WS-SAP-X-IDFAKT                   
420901     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
421001          FOR LEADING ZERO                                                
421101     ADD +1 TO NOLL-RAKNARE                                               
421201     UNSTRING WS-SAP-X-IDFAKT      INTO R8-EKH-IDVERGL                    
421301          WITH POINTER NOLL-RAKNARE                                       
421401     MOVE ZERO                        TO R8-EKH-KDPSLLOC                  
421501     MOVE INL-PRARTNTO                TO R8-EKH-PRARTNTO                  
421601     MOVE ZERO                        TO R8-EKH-PRARTSJK                  
421701                                         R8-EKH-PRHEMTAG                  
421801                                         R8-EKH-PRINK                     
421901                                         R8-EKH-PRDIRLON                  
422001                                         R8-EKH-PRDMTRL                   
422101                                         R8-EKH-PROVRPAL                  
422201                                         R8-EKH-SUBEL                     
422301     MOVE W-IDARTNR                   TO R8-EKH-IDARTNR                   
422401     MOVE SPACE                       TO R8-EKH-FLLSBOK                   
422501     MOVE 1.00                        TO R8-EKH-PRKURS                    
422601********** EV ÄNDRING FÖR PRKURS                                          
422701**********                                                                
422801     COMPUTE R8-EKH-KVANTAL = W-KVAVIS * -1                               
422901     MOVE 'L102'                      TO R8-EKH-IDTRANS                   
423001     MOVE ZERO                        TO R8-EKH-BEVAT                     
423101                                         R8-EKH-IDANALYS                  
423201                                         R8-EKH-IDKONTO                   
423301                                         R8-EKH-KDANMORS                  
423401                                         R8-EKH-SUVAT                     
423501                                         R8-EKH-KDFRAKT                   
423601                                         R8-EKH-PRLANDCO                  
423701                                         R8-EKH-DAAVIDAT                  
423801                                         R8-EKH-IDAVINR                   
423901                                         R8-EKH-KDAVVTYP                  
424001                                         R8-EKH-KDRT                      
424101                                         R8-EKH-KVANTMOT                  
424201                                         R8-EKH-KVAVIS                    
424301     MOVE WS-KDSORT                  TO  R8-EKH-KDSORT                    
424401     MOVE SPACE                      TO  R8-EKH-IDLEVNR                   
424501                                         R8-EKH-IDKST                     
424601     MOVE SPACE                      TO  R8-EKH-FLDCET                    
424701     MOVE SPACE                      TO  R8-EKH-IDKUNDRF                  
424801     MOVE SPACE                      TO  R8-EKH-IDFAKT-EXP                
424901     MOVE SPACE                      TO  R8-EKH-IDKUNDRF                  
425001     MOVE SPACE                      TO  R8-EKH-IDFAKT-EXP                
425101     MOVE REC-DCS-KDVALISO           TO  R8-EKH-KDVALISO                  
425201     MOVE REC-DCS-KDTRADP            TO  R8-EKH-KDTRADP                   
425301     IF REC-DCS-LAND-NON-VCC-OWNED                                        
425401     AND (SEND-DCS-CDC OR SEND-DCS-DDC)                                   
425501     OR (REC-DCS-LAND-NON-VCC-OWNED                                       
425601     AND (DIST35-VCC-NONVCC-REFILL OR                                     
425701          DIST35-VCC-NONVCC-TRANSFER))                                    
425801       IF REC-DCS-CHINA                                                   
425901            MOVE 'W570'    TO FIL-IDCPYTXT IN FIL-WDR801(1:4)             
426001       ELSE                                                               
426101         IF REC-DCS-INDIA                                                 
426201            MOVE 'W515'    TO FIL-IDCPYTXT IN FIL-WDR801(1:4)             
426301         ELSE                                                             
426401          MOVE REC-DCS-KDTRADP                                            
426501                           TO FIL-IDCPYTXT IN FIL-WDR801(1:4)             
426601         END-IF                                                           
426701       END-IF                                                             
426801       MOVE 'EKHA'         TO FIL-IDCPYTXT IN FIL-WDR801(5:4)             
426901     END-IF                                                               
427001     IF AKTUELLT-LAND-USA                                                 
427101     AND SEND-DCS-CDC                                                     
427201       MOVE 'W561EKHA'               TO FIL-IDCPYTXT IN FIL-WDR801        
427301     END-IF                                                               
427401                                                                          
427501     PERFORM IMS-ISRT-FILB01                                              
427601                                                                          
427701     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
427801        ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                              
427901        PERFORM IMS-ISRT-FILB01                                           
428001     END-PERFORM                                                          
428101     IF REC-DCS-KDTRADP = 'BR12'                                          
428201        IF NOT-FIRST-REC-TRANS                                            
428301            MOVE ZERO           TO NOTF-IDSEKVNR                          
428401            PERFORM S20-SEND-OPEN                                         
428501            MOVE SEND-IDCOM     TO WZ04-SEND-IDCOM                        
428601            PERFORM S21-SEND-PUT-PROP                                     
428701            MOVE JA             TO FIRST-REC-TRANS-SW                     
428801        END-IF                                                            
428901        MOVE R8-EKH-KDEKHHT     TO NOTF-KDEKHHT                           
429001        MOVE R8-EKH-KDEKSHT     TO NOTF-KDEKSHT                           
429101        MOVE R8-EKH-DAVERDAT    TO NOTF-DAVERDAT                          
429201        MOVE AKTUELL-TID-X(1:6) TO NOTF-TIREGTID                          
429301        MOVE R8-EKH-IDVERGL     TO NOTF-IDVERGL                           
429401        MOVE R8-EKH-IDDC-SEND   TO NOTF-IDDC                              
429501        MOVE WS-SAP-IDFAKT      TO NOTF-IDFAKT                            
429601        MOVE R8-EKH-IDKUNDNR    TO NOTF-IDKUNDNR                          
429701        MOVE WS-IDORDER         TO NOTF-IDORDER                           
429801        MOVE WS-IDKOLLI         TO NOTF-IDKOLLI                           
429901        MOVE R8-EKH-KVANTAL     TO NOTF-KVANTAL                           
430001        MOVE R8-EKH-IDARTNR     TO W-IDARTNR-EDIT-X                       
430101        MOVE FUNCTION TRIM(W-IDARTNR-EDIT-X LEADING)                      
430201                                TO NOTF-IDARTNR20                         
430301        ADD +1                  TO NOTF-IDSEKVNR                          
430401        PERFORM S22-SEND-PUT                                              
430501     END-IF                                                               
430601     .                                                                    
430701     EJECT                                                                
430801 S07B-SKAPA-SAP-TRANS SECTION.                                            
430901***** MAPPING OF SAP TRANSACTIONS                                         
431001                                                                          
431101     MOVE 'WL010200'                  TO FIL-IDPGM IN FIL-WDR801          
431201     ACCEPT FIL-TIREGDAT IN FIL-WDR801 FROM DATE                          
431301     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
431401     MOVE WS-SAP-AAAAMMDD             TO R8-EKH-DAVERDAT                  
431501     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
431601     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR801        
431701     ADD +1                           TO W-IDSEKVNR-SAP                   
431801     MOVE W-IDSEKVNR-SAP             TO FIL-IDSEKVNR IN FIL-WDR801        
431901     IF SEND-DCS-CDC OR SEND-DCS-DDC OR                                   
432001        DIST35-VCC-NONVCC-REFILL OR                                       
432101        DIST35-VCC-NONVCC-TRANSFER                                        
432201       MOVE '102'                     TO R8-EKH-KDEKHHT                   
432301**** IF IT'S A BOUNCE FLOW                                                
432401       IF DIST35-NONVCC-NONVCC-REFILL OR                                  
432501          DIST35-NONVCC-NONVCC-TRANSFER                                   
432601         MOVE '131'                   TO R8-EKH-KDEKSHT                   
432701       ELSE                                                               
432801         MOVE '121'                   TO R8-EKH-KDEKSHT                   
432901       END-IF                                                             
433001     ELSE                                                                 
433101       MOVE '503'                     TO R8-EKH-KDEKHHT                   
433201       MOVE '501'                     TO R8-EKH-KDEKSHT                   
433301     END-IF                                                               
433401     MOVE 'DET  '                     TO R8-EKH-KDEKNIVA                  
433501     MOVE WS-IDDC-SEND                TO SEND-WS-IDDC                     
433601     MOVE WS-IDDC-SEND                TO R8-EKH-IDDC-SEND                 
433701     MOVE WS-IDDC-SPAR                TO R8-EKH-IDDC-REC                  
433801     MOVE WS-SAP-IDDISTR              TO R8-EKH-IDDISTR                   
433901     MOVE WS-SAP-IDKUNDNR             TO R8-EKH-IDKUNDNR                  
434001*******************************                                           
434101     MOVE ZERO TO NOLL-RAKNARE                                            
434201     MOVE W-IDFAKT                   TO WS-SAP-IDFAKT                     
434301     MOVE WS-SAP-IDFAKT              TO WS-SAP-X-IDFAKT                   
434401     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
434501          FOR LEADING ZERO                                                
434601     ADD +1 TO NOLL-RAKNARE                                               
434701     UNSTRING WS-SAP-X-IDFAKT      INTO R8-EKH-IDVERGL                    
434801          WITH POINTER NOLL-RAKNARE                                       
434901     MOVE ZERO                        TO R8-EKH-KDPSLLOC                  
435001     MOVE INL-PRARTNTO                TO R8-EKH-PRARTNTO                  
435101     MOVE WS-PRAVCOST                 TO R8-EKH-PRARTSTD                  
435201     MOVE ZERO                        TO R8-EKH-PRARTSJK                  
435301                                         R8-EKH-PRHEMTAG                  
435401                                         R8-EKH-PRINK                     
435501                                         R8-EKH-PRDIRLON                  
435601                                         R8-EKH-PRDMTRL                   
435701                                         R8-EKH-PROVRPAL                  
435801                                         R8-EKH-SUBEL                     
435901     MOVE W-IDARTNR                   TO R8-EKH-IDARTNR                   
436001     MOVE SPACE                       TO R8-EKH-FLLSBOK                   
436101     MOVE 1.00                        TO R8-EKH-PRKURS                    
436201********** EV ÄNDRING FÖR PRKURS                                          
436301**********                                                                
436401     MOVE W-KVAVIS                    TO R8-EKH-KVANTAL                   
436501     MOVE 'L102'                      TO R8-EKH-IDTRANS                   
436601     MOVE INL-KDFRAKT                 TO R8-EKH-KDFRAKT                   
436701     MOVE ZERO                        TO R8-EKH-BEVAT                     
436801                                         R8-EKH-IDANALYS                  
436901                                         R8-EKH-IDKONTO                   
437001                                         R8-EKH-KDANMORS                  
437101                                         R8-EKH-SUVAT                     
437201                                         R8-EKH-PRLANDCO                  
437301                                         R8-EKH-DAAVIDAT                  
437401                                         R8-EKH-IDAVINR                   
437501                                         R8-EKH-KDAVVTYP                  
437601                                         R8-EKH-KDRT                      
437701                                         R8-EKH-KVANTMOT                  
437801                                         R8-EKH-KVAVIS                    
437901     MOVE WS-KDSORT                  TO  R8-EKH-KDSORT                    
438001     MOVE SPACE                      TO  R8-EKH-IDLEVNR                   
438101                                         R8-EKH-IDKST                     
438201     MOVE SPACE                      TO  R8-EKH-FLDCET                    
438301     MOVE SPACE                      TO  R8-EKH-IDKUNDRF                  
438401     MOVE SPACE                      TO  R8-EKH-IDFAKT-EXP                
438501     MOVE SPACE                      TO  R8-EKH-IDKUNDRF                  
438601     MOVE SPACE                      TO  R8-EKH-IDFAKT-EXP                
438701     MOVE DCS-KDVALISO               TO  R8-EKH-KDVALISO                  
438801     MOVE DCS-KDTRADP                TO  R8-EKH-KDTRADP                   
438901     IF REC-DCS-LAND-NON-VCC-OWNED                                        
439001       IF REC-DCS-CHINA                                                   
439101         MOVE 'W570'     TO FIL-IDCPYTXT IN FIL-WDR801(1:4)               
439201       ELSE                                                               
439301         IF REC-DCS-INDIA                                                 
439401           MOVE 'W515'   TO FIL-IDCPYTXT IN FIL-WDR801(1:4)               
439501         ELSE                                                             
439601           MOVE REC-DCS-KDTRADP                                           
439701                         TO FIL-IDCPYTXT IN FIL-WDR801(1:4)               
439801         END-IF                                                           
439901       END-IF                                                             
440001     ELSE                                                                 
440101       IF REC-DCS-USA                                                     
440201          MOVE 'W561'    TO FIL-IDCPYTXT IN FIL-WDR801(1:4)               
440301       END-IF                                                             
440401     END-IF                                                               
440501     MOVE 'EKHA'         TO FIL-IDCPYTXT IN FIL-WDR801(5:4)               
440601                                                                          
440701     PERFORM IMS-ISRT-FILB01                                              
440801                                                                          
440901     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
441001        ADD +1 TO FIL-IDSEKVNR IN FIL-WDR801                              
441101        PERFORM IMS-ISRT-FILB01                                           
441201     END-PERFORM                                                          
441301     IF REC-DCS-KDTRADP = 'BR12'                                          
441401        IF NOT-FIRST-REC-TRANS                                            
441501            MOVE ZERO           TO NOTF-IDSEKVNR                          
441601            PERFORM S20-SEND-OPEN                                         
441701            MOVE SEND-IDCOM     TO WZ04-SEND-IDCOM                        
441801            PERFORM S21-SEND-PUT-PROP                                     
441901            MOVE JA             TO FIRST-REC-TRANS-SW                     
442001        END-IF                                                            
442101        MOVE R8-EKH-KDEKHHT     TO NOTF-KDEKHHT                           
442201        MOVE R8-EKH-KDEKSHT     TO NOTF-KDEKSHT                           
442301        MOVE R8-EKH-DAVERDAT    TO NOTF-DAVERDAT                          
442401        MOVE AKTUELL-TID-X(1:6) TO NOTF-TIREGTID                          
442501        MOVE R8-EKH-IDVERGL     TO NOTF-IDVERGL                           
442601        MOVE R8-EKH-IDDC-SEND   TO NOTF-IDDC                              
442701        MOVE WS-SAP-IDFAKT      TO NOTF-IDFAKT                            
442801        MOVE R8-EKH-IDKUNDNR    TO NOTF-IDKUNDNR                          
442901        MOVE WS-IDORDER         TO NOTF-IDORDER                           
443001        MOVE WS-IDKOLLI         TO NOTF-IDKOLLI                           
443101        MOVE R8-EKH-KVANTAL     TO NOTF-KVANTAL                           
443201        MOVE R8-EKH-IDARTNR     TO W-IDARTNR-EDIT-X                       
443301        MOVE FUNCTION TRIM(W-IDARTNR-EDIT-X LEADING)                      
443401                                TO NOTF-IDARTNR20                         
443501        ADD +1                  TO NOTF-IDSEKVNR                          
443601        PERFORM S22-SEND-PUT                                              
443701     END-IF                                                               
443801     .                                                                    
443901     EJECT                                                                
444001 S07C-SKAPA-SAP-TRANS SECTION.                                            
444101***** MAPPING OF SAP TRANSACTIONS TO VCCS                                 
444201                                                                          
444301     MOVE 'WL010200'                  TO FIL-IDPGM IN FIL-WDR901          
444401     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
444501     MOVE WS-SAP-AAAAMMDD             TO FIL-DAREGDAT                     
444601                                         EKH-DAVERDAT                     
444701     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
444801     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR901        
444901     ADD +1                           TO W-IDSEKVNR-SAP                   
445001     MOVE 'W510EKHA'                 TO FIL-IDCPYTXT IN FIL-WDR901        
445101     MOVE MSG-SIGNON-USERID          TO FIL-IDUSER IN FIL-WDR901          
445201     MOVE W-IDSEKVNR-SAP             TO FIL-IDSEKVNR IN FIL-WDR901        
445301     IF DIST35-NONVCC-VCC-REFILL OR                                       
445401        DIST35-NONVCC-VCC-TRANSFER OR                                     
445402        DIST35-NONVCC-CDC-REFILL                                          
445501       MOVE '102'                     TO EKH-KDEKHHT                      
445601       MOVE '122'                     TO EKH-KDEKSHT                      
445701     ELSE                                                                 
445801       MOVE '503'                     TO EKH-KDEKHHT                      
445901       MOVE '501'                     TO EKH-KDEKSHT                      
446001     END-IF                                                               
446101     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
446201     MOVE WS-IDDC-SEND                TO SEND-WS-IDDC                     
446301     MOVE WS-IDDC-SEND                TO EKH-IDDC-SEND                    
446401     MOVE WS-IDDC-SPAR                TO EKH-IDDC-REC                     
446501     MOVE WS-SAP-IDDISTR              TO EKH-IDDISTR                      
446601     MOVE WS-SAP-IDKUNDNR             TO EKH-IDKUNDNR                     
446701*******************************                                           
446801     MOVE ZERO TO NOLL-RAKNARE                                            
446901     MOVE W-IDFAKT                   TO WS-SAP-IDFAKT                     
447001     MOVE WS-SAP-IDFAKT              TO WS-SAP-X-IDFAKT                   
447101     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
447201          FOR LEADING ZERO                                                
447301     ADD +1 TO NOLL-RAKNARE                                               
447401     UNSTRING WS-SAP-X-IDFAKT      INTO EKH-IDVERGL                       
447501          WITH POINTER NOLL-RAKNARE                                       
447601     MOVE ZERO                        TO EKH-KDPSLLOC                     
447701                                         EKH-PRARTNTO                     
447801                                         EKH-PRARTSJK                     
447901                                         EKH-PRHEMTAG                     
448001                                         EKH-PRINK                        
448101                                         EKH-PRDIRLON                     
448201                                         EKH-PRDMTRL                      
448301                                         EKH-PROVRPAL                     
448401                                         EKH-SUBEL                        
448501     MOVE W-IDARTNR                   TO EKH-IDARTNR                      
448601     MOVE SPACE                       TO EKH-FLLSBOK                      
448701     MOVE 'SEK'                       TO EKH-KDVALISO                     
448801********** EV ÄNDRING FÖR PRKURS                                          
448901     MOVE 1.00                        TO EKH-PRKURS                       
449001**********                                                                
449101     COMPUTE EKH-KVANTAL = W-KVAVIS * -1                                  
449201     MOVE 'L102'                      TO EKH-IDTRANS                      
449301     MOVE ZERO                        TO EKH-BEVAT                        
449401                                         EKH-IDANALYS                     
449501                                         EKH-IDKONTO                      
449601                                         EKH-KDANMORS                     
449701                                         EKH-SUVAT                        
449801                                         EKH-KDFRAKT                      
449901                                         EKH-PRLANDCO                     
450001                                         EKH-DAAVIDAT                     
450101                                         EKH-IDAVINR                      
450201                                         EKH-KDAVVTYP                     
450301                                         EKH-KDRT                         
450401                                         EKH-KVANTMOT                     
450501                                         EKH-KVAVIS                       
450601     MOVE WS-KDSORT                  TO  EKH-KDSORT                       
450701     MOVE SPACE                      TO  EKH-IDLEVNR                      
450801                                         EKH-IDKST                        
450901     MOVE SPACE                      TO  EKH-FLDCET                       
451001     MOVE SPACE                      TO  EKH-IDKUNDRF                     
451101     MOVE 'SEPV'                     TO  EKH-KDTRADP                      
451201     MOVE SPACE                      TO  EKH-IDKUNDRF                     
451301     MOVE SPACE                      TO  EKH-IDFAKT-EXP                   
451401                                                                          
451501     MOVE EKH-W510EKHA               TO  R9-EKH-W510EKHA                  
451601                                                                          
451701     PERFORM IMS-19-ISRT-WLSAPA01                                         
451801                                                                          
451901     PERFORM UNTIL SEGMENT-FINNS                                          
452001         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                             
452101         PERFORM IMS-19-ISRT-WLSAPA01                                     
452201     END-PERFORM                                                          
452301     .                                                                    
452401     EJECT                                                                
452501                                                                          
452601 S07D-SKAPA-SAP-TRANS SECTION.                                            
452701***** MAPPING OF SAP TRANSACTIONS TO VCCS                                 
452801                                                                          
452901     MOVE 'WL010200'                  TO FIL-IDPGM IN FIL-WDR901          
453001     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
453101     MOVE WS-SAP-AAAAMMDD             TO FIL-DAREGDAT                     
453201                                         EKH-DAVERDAT                     
453301     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
453401     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR901        
453501     ADD +1                           TO W-IDSEKVNR-SAP                   
453601     MOVE 'W510EKHA'                 TO FIL-IDCPYTXT IN FIL-WDR901        
453701     MOVE MSG-SIGNON-USERID          TO FIL-IDUSER IN FIL-WDR901          
453801     MOVE W-IDSEKVNR-SAP             TO FIL-IDSEKVNR IN FIL-WDR901        
453901     IF DIST35-NONVCC-VCC-REFILL OR                                       
454001        DIST35-NONVCC-VCC-TRANSFER                                        
454101       MOVE '102'                     TO EKH-KDEKHHT                      
454201       MOVE '121'                     TO EKH-KDEKSHT                      
454301     ELSE                                                                 
454401       MOVE '503'                     TO EKH-KDEKHHT                      
454501       MOVE '501'                     TO EKH-KDEKSHT                      
454601     END-IF                                                               
454701     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
454801     MOVE WS-IDDC-SEND                TO SEND-WS-IDDC                     
454901     MOVE WS-IDDC-SEND                TO EKH-IDDC-SEND                    
455001     MOVE WS-IDDC-SPAR                TO EKH-IDDC-REC                     
455101     MOVE WS-SAP-IDDISTR              TO EKH-IDDISTR                      
455201     MOVE WS-SAP-IDKUNDNR             TO EKH-IDKUNDNR                     
455301     MOVE WS-PRARTNTO                 TO EKH-PRARTNTO                     
455401     MOVE WS-PRARTSTD                 TO EKH-PRARTSTD                     
455501*******************************                                           
455601     MOVE ZERO TO NOLL-RAKNARE                                            
455701     MOVE W-IDFAKT                   TO WS-SAP-IDFAKT                     
455801     MOVE WS-SAP-IDFAKT              TO WS-SAP-X-IDFAKT                   
455901     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
456001          FOR LEADING ZERO                                                
456101     ADD +1 TO NOLL-RAKNARE                                               
456201     UNSTRING WS-SAP-X-IDFAKT      INTO EKH-IDVERGL                       
456301          WITH POINTER NOLL-RAKNARE                                       
456401     MOVE ZERO                        TO EKH-KDPSLLOC                     
456501                                         EKH-PRARTSJK                     
456601                                         EKH-PRHEMTAG                     
456701                                         EKH-PRINK                        
456801                                         EKH-PRDIRLON                     
456901                                         EKH-PRDMTRL                      
457001                                         EKH-PROVRPAL                     
457101                                         EKH-SUBEL                        
457201     MOVE W-IDARTNR                   TO EKH-IDARTNR                      
457301     MOVE SPACE                       TO EKH-FLLSBOK                      
457401     MOVE 'SEK'                       TO EKH-KDVALISO                     
457501********** EV ÄNDRING FÖR PRKURS                                          
457601     MOVE 1.00                        TO EKH-PRKURS                       
457701**********                                                                
457801     MOVE W-KVAVIS                    TO EKH-KVANTAL                      
457901     MOVE 'L102'                      TO EKH-IDTRANS                      
458001     MOVE ZERO                        TO EKH-BEVAT                        
458101                                         EKH-IDANALYS                     
458201                                         EKH-IDKONTO                      
458301                                         EKH-KDANMORS                     
458401                                         EKH-SUVAT                        
458501                                         EKH-KDFRAKT                      
458601                                         EKH-PRLANDCO                     
458701                                         EKH-DAAVIDAT                     
458801                                         EKH-IDAVINR                      
458901                                         EKH-KDAVVTYP                     
459001                                         EKH-KDRT                         
459101                                         EKH-KVANTMOT                     
459201                                         EKH-KVAVIS                       
459301     MOVE WS-KDSORT                  TO  EKH-KDSORT                       
459401     MOVE SPACE                      TO  EKH-IDLEVNR                      
459501                                         EKH-IDKST                        
459601     MOVE SPACE                      TO  EKH-FLDCET                       
459701     MOVE SPACE                      TO  EKH-IDKUNDRF                     
459801     MOVE 'SEPV'                     TO  EKH-KDTRADP                      
459901     MOVE SPACE                      TO  EKH-IDKUNDRF                     
460001     MOVE SPACE                      TO  EKH-IDFAKT-EXP                   
460101                                                                          
460201     MOVE EKH-W510EKHA               TO  R9-EKH-W510EKHA                  
460301                                                                          
460401     PERFORM IMS-19-ISRT-WLSAPA01                                         
460501                                                                          
460601     PERFORM UNTIL SEGMENT-FINNS                                          
460701         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                             
460801         PERFORM IMS-19-ISRT-WLSAPA01                                     
460901     END-PERFORM                                                          
461001     .                                                                    
461101     EJECT                                                                
461201                                                                          
461301 S07E-SKAPA-SAP-TRANS SECTION.                                            
461401***** MAPPING OF SAP TRANSACTIONS TO CDC                                  
461501                                                                          
461601     MOVE 'WL010200'                  TO FIL-IDPGM IN FIL-WDR901          
461701     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-SAP-AAAAMMDD                  
461801     MOVE WS-SAP-AAAAMMDD             TO FIL-DAREGDAT                     
461901                                         EKH-DAVERDAT                     
462001     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-SAP-TTMMSSTH                  
462101     MOVE WS-SAP-TTMMSSTH             TO FIL-TIKLOCK IN FIL-WDR901        
462201     ADD +1                           TO W-IDSEKVNR-SAP                   
462301     MOVE 'W510EKHA'                 TO FIL-IDCPYTXT IN FIL-WDR901        
462401     MOVE MSG-SIGNON-USERID          TO FIL-IDUSER IN FIL-WDR901          
462501     MOVE W-IDSEKVNR-SAP             TO FIL-IDSEKVNR IN FIL-WDR901        
462601     MOVE '102'                       TO EKH-KDEKHHT                      
462701     MOVE '121'                       TO EKH-KDEKSHT                      
462801     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
462901     MOVE WS-IDDC-SEND                TO SEND-WS-IDDC                     
463001     MOVE WS-IDDC-SEND                TO EKH-IDDC-SEND                    
463101     MOVE WS-IDDC-SPAR                TO EKH-IDDC-REC                     
463201     MOVE WS-SAP-IDDISTR              TO EKH-IDDISTR                      
463301     MOVE WS-SAP-IDKUNDNR             TO EKH-IDKUNDNR                     
463401*******************************                                           
463501     MOVE ZERO TO NOLL-RAKNARE                                            
463601     MOVE W-IDFAKT                   TO WS-SAP-IDFAKT                     
463701     MOVE WS-SAP-IDFAKT              TO WS-SAP-X-IDFAKT                   
463801     INSPECT WS-SAP-X-IDFAKT TALLYING NOLL-RAKNARE                        
463901          FOR LEADING ZERO                                                
464001     ADD +1 TO NOLL-RAKNARE                                               
464101     UNSTRING WS-SAP-X-IDFAKT      INTO EKH-IDVERGL                       
464201          WITH POINTER NOLL-RAKNARE                                       
464301     MOVE ZERO                        TO EKH-KDPSLLOC                     
464401     MOVE WS-PRARTNTO                 TO EKH-PRARTNTO                     
464501     MOVE WS-PRARTSTD                 TO EKH-PRARTSTD                     
464601     MOVE ZERO                        TO EKH-PRARTSJK                     
464701                                         EKH-PRHEMTAG                     
464801                                         EKH-PRINK                        
464901                                         EKH-PRDIRLON                     
465001                                         EKH-PRDMTRL                      
465101                                         EKH-PROVRPAL                     
465201                                         EKH-SUBEL                        
465301     MOVE W-IDARTNR                   TO EKH-IDARTNR                      
465401     MOVE SPACE                       TO EKH-FLLSBOK                      
465501     MOVE 'SEK'                       TO EKH-KDVALISO                     
465601********** EV ÄNDRING FÖR PRKURS                                          
465701     MOVE 1.00                        TO EKH-PRKURS                       
465801**********                                                                
465901     MOVE W-KVAVIS                    TO EKH-KVANTAL                      
466001     MOVE 'L102'                      TO EKH-IDTRANS                      
466101     MOVE ZERO                        TO EKH-BEVAT                        
466201                                         EKH-IDANALYS                     
466301                                         EKH-IDKONTO                      
466401                                         EKH-KDANMORS                     
466501                                         EKH-SUVAT                        
466601                                         EKH-KDFRAKT                      
466701                                         EKH-PRLANDCO                     
466801                                         EKH-DAAVIDAT                     
466901                                         EKH-IDAVINR                      
467001                                         EKH-KDAVVTYP                     
467101                                         EKH-KDRT                         
467201                                         EKH-KVANTMOT                     
467301                                         EKH-KVAVIS                       
467401                                         EKH-IDORDNR5                     
467501     MOVE WS-KDSORT                  TO  EKH-KDSORT                       
467601     MOVE SPACE                      TO  EKH-IDLEVNR                      
467701                                         EKH-IDKST                        
467801     MOVE SPACE                      TO  EKH-FLDCET                       
467901     MOVE SPACE                      TO  EKH-IDKUNDRF                     
468001     MOVE 'SEPV'                     TO  EKH-KDTRADP                      
468101     MOVE SPACE                      TO  EKH-IDKUNDRF                     
468201     MOVE SPACE                      TO  EKH-IDFAKT-EXP                   
468301                                                                          
468401     MOVE EKH-W510EKHA               TO  R9-EKH-W510EKHA                  
468501                                                                          
468601     PERFORM IMS-19-ISRT-WLSAPA01                                         
468701                                                                          
468801     PERFORM UNTIL SEGMENT-FINNS                                          
468901         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                             
469001         PERFORM IMS-19-ISRT-WLSAPA01                                     
469101     END-PERFORM                                                          
469201     .                                                                    
469301     EJECT                                                                
469401 S08-SKAPA-AVVIK-TRANS SECTION.                                           
469501                                                                          
469601     MOVE INL-KVAVIS      TO FILC-KVANTAL                                 
469701     MOVE 'LOST'          TO FILC-AVVIKELSETYP                            
469801     MOVE 6302-TIFAKT     TO WS-SEKEL-TEST                                
469901                             WS-AVVIK-AAMMDD                              
470001     IF WS-SEKEL2 = 9                                                     
470101        MOVE 19           TO WS-AVVIK-SS                                  
470201     ELSE                                                                 
470301        MOVE 20           TO WS-AVVIK-SS                                  
470401     END-IF                                                               
470501     MOVE WS-AVVIK-AAAAMMDD   TO FILC-DAFAKT                              
470601     MOVE W-IDARTNR           TO FILC-IDARTNR                             
470701     MOVE WS-IDDC-SEND        TO FILC-IDDC-SEND                           
470801     MOVE WS-IDDC-SPAR        TO FILC-IDDC-REC                            
470901     MOVE W-IDFAKT            TO FILC-IDFAKT                              
471001     INSPECT REQU-IDKUNDRF(INDX) REPLACING LEADING SPACE BY ZERO          
471101     MOVE REQU-IDKUNDRF(INDX) TO FILC-IDKUNDRF                            
471201     MOVE ZERO                TO FILC-PRARTBES-PR                         
471301     MOVE 6302-IDLEVNR        TO FILC-IDLEVNR                             
471401     MOVE FILC-W61244         TO FILC-FIL-WDR301-DATA                     
471501     ACCEPT W-TID FROM TIME                                               
471601     IF W-TID = FILC-FIL-TIKLOCK                                          
471701        ADD +1                TO FILC-FIL-IDSEKVNR                        
471801     ELSE                                                                 
471901        MOVE W-TID            TO FILC-FIL-TIKLOCK                         
472001        MOVE +1               TO FILC-FIL-IDSEKVNR                        
472101     END-IF                                                               
472201     PERFORM IMS-20-ISRT-WLFILC                                           
472301     .                                                                    
472401     EJECT                                                                
472501 S09-SKAPA-LDC-TRANS SECTION.                                             
472601                                                                          
472701     MOVE 'J'             TO FILC2-FLINLREP                               
472801     MOVE INL-KVAVIS      TO FILC2-KVANTAL                                
472901                             FILC2-KVAVIS                                 
473001     MOVE 'LOST'          TO FILC2-AVVIKELSETYP                           
473101     MOVE 6302-TIFAKT     TO WS-SEKEL-TEST                                
473201                             WS-AVVIK-AAMMDD                              
473301     IF WS-SEKEL2 = 9                                                     
473401        MOVE 19           TO WS-AVVIK-SS                                  
473501     ELSE                                                                 
473601        MOVE 20           TO WS-AVVIK-SS                                  
473701     END-IF                                                               
473801     MOVE WS-AVVIK-AAAAMMDD   TO FILC2-DAFAKT                             
473901     MOVE W-IDARTNR           TO FILC2-IDARTNR                            
474001     IF INL-IDDC-LEV NOT = SPACE                                          
474101        MOVE INL-IDDC-LEV     TO FILC2-IDDC-SEND                          
474201     ELSE                                                                 
474301        MOVE WS-IDDC-SEND     TO FILC2-IDDC-SEND                          
474401     END-IF                                                               
474501     MOVE WS-IDDC-SPAR        TO FILC2-IDDC-REC                           
474601     MOVE W-IDFAKT            TO FILC2-IDFAKT                             
474701     INSPECT REQU-IDKUNDRF(INDX) REPLACING LEADING SPACE BY ZERO          
474801     MOVE REQU-IDKUNDRF(INDX) TO FILC2-IDKUNDRF                           
474901     MOVE ZERO                TO FILC2-IDKUNDNR                           
475001     INSPECT REQU-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO          
475101     MOVE REQU-IDKUNDNR(INDX) TO FILC2-IDKUNDNR                           
475201     INSPECT REQU-IDKOLLI(INDX) REPLACING LEADING SPACE BY ZERO           
475301     MOVE REQU-IDKOLLI(INDX)  TO FILC2-IDKOLLI                            
475401     MOVE ZERO                TO FILC2-PRARTSTD                           
475501     MOVE 1                   TO FILC2-KDSORT1                            
475601     MOVE DAGENS-DATUM        TO FILC2-DAREGDAT                           
475701     IF REQU-IDUSER-003 = ALL '+'                                         
475801        MOVE SPACE            TO FILC2-IDUSER                             
475901     ELSE                                                                 
476001        MOVE REQU-IDUSER-003  TO FILC2-IDUSER                             
476101     END-IF                                                               
476201     MOVE FILC2-W61247        TO FILC2-FIL-WDR301-DATA                    
476301     ACCEPT W-TID FROM TIME                                               
476401     IF W-TID = FILC2-FIL-TIKLOCK                                         
476501        ADD +1                TO FILC2-FIL-IDSEKVNR                       
476601     ELSE                                                                 
476701        MOVE W-TID            TO FILC2-FIL-TIKLOCK                        
476801        MOVE +1               TO FILC2-FIL-IDSEKVNR                       
476901     END-IF                                                               
477001     PERFORM IMS-21-ISRT-WLFILC2                                          
477101     .                                                                    
477201     EJECT                                                                
477301 S10-HAEMTA-ANROPSDATA SECTION.                                           
477401                                                                          
477501     MOVE 'GETARG'            TO SUB-KDFUNC                               
477601     MOVE WS-ADRESS           TO SUB-ADDISPABS                            
477701     MOVE LENGTH OF REQU-AREA TO SUB-KVDLEN                               
477801                                                                          
477901     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
478001                                                                          
478101     IF SUB-KDRC > 0                                                      
478201       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
478301       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
478401       DELIMITED BY SIZE INTO FELTEXT                                     
478501       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
478601     END-IF                                                               
478701     .                                                                    
478801     EJECT                                                                
478901 S11-RETURNERA-SVAR SECTION.                                              
479001                                                                          
479101     COMPUTE WS-RESP-AREA = LENGTH OF RESP-AREA                           
479201       - LENGTH OF RESP-TABELLRAD * (500 - WS-KVANT)                      
479301                                                                          
479401     MOVE 'RETURN'                   TO SUB-KDFUNC                        
479501     MOVE WS-RESP-AREA               TO SUB-KVDLEN                        
479601                                                                          
479701     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
479801                                                                          
479901     IF SUB-KDRC > 0                                                      
480001       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
480101       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
480201       DELIMITED BY SIZE INTO FELTEXT                                     
480301       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
480401     END-IF                                                               
480501     .                                                                    
480601     EJECT                                                                
480701 S11-MSG-CONV SECTION.                                                    
480801     MOVE SPACES                  TO RESP-MESSAGES (1)                    
480901                                     RESP-MESSAGES (2)                    
481001     MOVE 1                       TO MSG-IX                               
481101*    REQUEST OK                                                           
481201     MOVE 200                     TO RESP-KDSTATUS-API                    
481301     IF RESP-IDMSG-INFO > SPACE                                           
481401       MOVE SPACES                TO MSG-CONV-AREA                        
481501       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
481601       IF DC-CROSS-FLAG = 'J'                                             
481701        MOVE RESP-IDELMT-ERROR TO MSG-CONV-IDELMT                         
481801       END-IF                                                             
481901       CALL WMSGCONV           USING MSG-CONV-AREA                        
482001       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
482101       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
482201       ADD 1                      TO MSG-IX                               
482301     END-IF                                                               
482401     IF RESP-IDMSG-ERROR > SPACE                                          
482501*      BAD REQUEST                                                        
482601       MOVE 400                   TO RESP-KDSTATUS-API                    
482701       MOVE SPACES                TO MSG-CONV-AREA                        
482801       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
482901       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
483001       CALL WMSGCONV           USING MSG-CONV-AREA                        
483101       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
483201       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
483301     END-IF                                                               
483401     .                                                                    
483501 S12-SEND-OPEN SECTION.                                                   
483601     MOVE 'OPEN'                        TO SEND-KDFUNC                    
483701     MOVE WS-ADRESS-WL0103              TO SEND-ADDISPABS                 
483801     CALL WZ01SEND USING SEND-CONTROL-AREA                                
483901                         SEND-OPEN-AREA                                   
484001     IF SEND-KDRC > 0                                                     
484101       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
484201       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
484301       DELIMITED BY SIZE INTO FELTEXT                                     
484401       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
484501     END-IF                                                               
484601     .                                                                    
484701     EJECT                                                                
484801 S13-SEND-MESSAGE SECTION.                                                
484901     MOVE 'PUT'                           TO SEND-KDFUNC                  
485001     MOVE LENGTH OF SEND-AREA             TO SEND-KVDLEN                  
485101     CALL WZ01SEND USING SEND-CONTROL-AREA                                
485201                         SEND-KVDLEN                                      
485301                         SEND-AREA                                        
485401     IF SEND-KDRC > 0                                                     
485501       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
485601       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
485701       DELIMITED BY SIZE INTO FELTEXT                                     
485801       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
485901     END-IF                                                               
486001     .                                                                    
486101     EJECT                                                                
486201 S14-SEND-CLOSE SECTION.                                                  
486301     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
486401     CALL WZ01SEND USING SEND-CONTROL-AREA                                
486501                                                                          
486601     IF SEND-KDRC > 0                                                     
486701       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
486801       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
486901       DELIMITED BY SIZE INTO FELTEXT                                     
487001       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
487101     END-IF                                                               
487201     .                                                                    
487301     EJECT                                                                
487401 S15-SEND-OPEN SECTION.                                                   
487501                                                                          
487601     MOVE WS-ADRESS                  TO SEND-ADDISPABS                    
487701     MOVE 'OPEN'                     TO SEND-KDFUNC                       
487801     CALL WZ01SEND USING SEND-CONTROL-AREA                                
487901                         SEND-OPEN-AREA                                   
488001     IF SEND-KDRC > ZERO                                                  
488101       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
488201       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
488301       DELIMITED BY SIZE INTO FELTEXT                                     
488401       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
488501     END-IF                                                               
488601     .                                                                    
488701     EJECT                                                                
488801 S16-SEND-PUT SECTION.                                                    
488901                                                                          
489001     MOVE 'PUT'                            TO SEND-KDFUNC                 
489101     MOVE LENGTH OF SEND-AREA              TO SEND-KVDLEN                 
489201     CALL WZ01SEND USING SEND-CONTROL-AREA                                
489301                         SEND-KVDLEN                                      
489401                         SEND-AREA                                        
489501     IF SEND-KDRC > 1                                                     
489601       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
489701       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
489801       DELIMITED BY SIZE INTO FELTEXT                                     
489901       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
490001     END-IF                                                               
490101     .                                                                    
490201     EJECT                                                                
490301 S17-SEND-CLOSE SECTION.                                                  
490401                                                                          
490501     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
490601     CALL WZ01SEND USING SEND-CONTROL-AREA                                
490701                                                                          
490801     IF SEND-KDRC > 0                                                     
490901       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
491001       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
491101       DELIMITED BY SIZE INTO FELTEXT                                     
491201       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
491301     END-IF                                                               
491401     .                                                                    
491501     EJECT                                                                
491601 S18-CALL-W218ETA SECTION.                                                
491701                                                                          
491801                                                                          
491901     IF REC-DCS-LAND-NON-VCC-OWNED OR REC-DCS-NDC-NA                      
492001     OR REC-DCS-AUSTRALIA OR REC-DCS-JAPAN OR REC-DCS-CDC                 
492101       MOVE W-IDDC           TO W-IDDC-B6                                 
492201       PERFORM HIC-LOCAL-DATE-TIME                                        
492301       MOVE 6302-IDKUNDNR TO ETA-IDKUNDNR                                 
492401       IF REC-DCS-CDC                                                     
492501         IF ETA-IDKUNDNR(6:2) = 01 OR 02 OR 17                            
492601           MOVE '613' TO LETA-KDCALL                                      
492701         ELSE                                                             
492801           MOVE '608' TO LETA-KDCALL                                      
492901         END-IF                                                           
493001       ELSE                                                               
493101         MOVE REC-DCS-IDKUNDNR-SORD TO ETA-IDKUNDNR-SORD                  
493201         MOVE REC-DCS-IDKUNDNR-SBPS TO ETA-IDKUNDNR-SBPS                  
493301         IF ETA-IDKUNDNR(6:2) = (ETA-IDKUNDNR-SORD(6:2) OR                
493401                                 ETA-IDKUNDNR-SBPS(6:2))                  
493501           MOVE '613' TO LETA-KDCALL                                      
493601         ELSE                                                             
493701           MOVE '608' TO LETA-KDCALL                                      
493801         END-IF                                                           
493901       END-IF                                                             
494001       IF 6302-IDDC-LEV NOT = SPACE                                       
494101         MOVE 6302-IDDC-LEV  TO LETA-IDDC-SEND                            
494201       ELSE                                                               
494301         MOVE 6302-IDDC-SEND TO LETA-IDDC-SEND                            
494401       END-IF                                                             
494501       MOVE WS-IDDC-SPAR   TO LETA-IDDC-REC                               
494601       MOVE ZERO           TO LETA-KDFRAKT                                
494701       MOVE W-DATUM-LOCAL  TO LETA-TIAAMMDD-ANROP                         
494801       MOVE 20             TO LETA-TISEKEL-ANROP                          
494901       CALL W218ETA USING LETA-W218LETA                                   
495001                           W218-WDK6-PCB                                  
495101                           W218-WDK7-PCB                                  
495201                           W218-WDL6-PCB                                  
495301                           W218-WDF1-PCB                                  
495401                           W218-WDB6-PCB                                  
495501                           W218-WDD9-PCB                                  
495601                                                                          
495701       IF LETA-SVAR-OK = 'F' OR 'N'                                       
495801          MOVE 'FEL RETURKOD FRÅN ETA' TO FELTEXT                         
495901          DISPLAY FELTEXT                                                 
496001          CALL FELLOG                                                     
496101       END-IF                                                             
496201       MOVE JA             TO ETA-UPD-SW                                  
496301     END-IF                                                               
496401     .                                                                    
496501     EJECT                                                                
496601 S19-SKAPA-WDL6CSEQ SECTION.                                              
496701                                                                          
496801     MOVE LOW-VALUE                  TO W-WDL6CSEQ-MIN                    
496901     MOVE HIGH-VALUE                 TO W-WDL6CSEQ-MAX                    
497001     MOVE W-IDFAKT                   TO W-SEQC-IDFAKT-MIN                 
497101                                        W-SEQC-IDFAKT-MAX                 
497201     MOVE W-IDDC                     TO W-SEQC-IDDC-MIN                   
497301                                        W-SEQC-IDDC-MAX                   
497401     .                                                                    
497501     EJECT                                                                
497601 S20-SEND-OPEN SECTION.                                                   
497701     MOVE 'OPEN'                        TO SEND-KDFUNC                    
497801     MOVE WS-ADDRESS-MQASYNC            TO SEND-ADDISPABS                 
497901     CALL WZ01SEND USING SEND-CONTROL-AREA                                
498001                         SEND-OPEN-AREA                                   
498101     IF SEND-KDRC > 0                                                     
498201       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
498301       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
498401       DELIMITED BY SIZE INTO FELTEXT                                     
498501       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
498601     END-IF                                                               
499001     .                                                                    
502201     EJECT                                                                
502301 S21-SEND-PUT-PROP SECTION.                                               
502401                                                                          
502501     SET PROP-IX                 TO +1                                    
502601*    MANDATORY PROPERTY THAT SPECIFIES THE ACTUAL DESTINATION             
502701     MOVE 'ADDRESS'              TO PROP-IDPROPTYPE  (PROP-IX)            
502801     MOVE 'ADDISPABS'            TO PROP-IDPROPNAME  (PROP-IX)            
502901     MOVE WS-ADDRESS-WHSTOCKA    TO PROP-BEPROPVALUE (PROP-IX)            
503001                                                                          
503101     SET PROP-IX              UP BY +1                                    
503201*    OPTIONAL MQ MESSAGE PROPERTIES. CAN BE CASE-SENSITIVE                
503301     MOVE 'MQMPROP'              TO PROP-IDPROPTYPE  (PROP-IX)            
503401     MOVE 'CountryCode'          TO PROP-IDPROPNAME  (PROP-IX)            
503501     MOVE 'BR'                   TO PROP-BEPROPVALUE (PROP-IX)            
503601                                                                          
503701*    SET THE NUMBER OF PROPERTIES (KVANTAL) SO CORRECT LENGTH             
503801*    IS CALCULATED.                                                       
503901     SET PROP-KVANTAL            TO PROP-IX                               
504001                                                                          
504101     MOVE 'PUT'                            TO SEND-KDFUNC                 
504201     MOVE LENGTH OF PROP-WZ04PROP          TO SEND-KVDLEN                 
504301     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
504401     CALL WZ01SEND USING SEND-CONTROL-AREA                                
504501                         SEND-KVDLEN                                      
504601                         PROP-WZ04PROP                                    
504701                                                                          
504801     IF SEND-KDRC > 1                                                     
504901       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
505001       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
505101       DELIMITED BY SIZE INTO FELTEXT                                     
505201       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
505301     END-IF                                                               
505401     .                                                                    
505501     EJECT                                                                
505601 S22-SEND-PUT SECTION.                                                    
505701                                                                          
505801     MOVE 'PUT'                            TO SEND-KDFUNC                 
505901     MOVE LENGTH OF NOTF-AREA              TO SEND-KVDLEN                 
506001     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
506101     CALL WZ01SEND USING SEND-CONTROL-AREA                                
506201                         SEND-KVDLEN                                      
506301                         NOTF-AREA                                        
506401     IF SEND-KDRC > 1                                                     
506501       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
506601       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
506701       DELIMITED BY SIZE INTO FELTEXT                                     
506801       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
506901     END-IF                                                               
507001     .                                                                    
507101     EJECT                                                                
507201 S23-SEND-CLOSE SECTION.                                                  
507301     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
507401     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
507501     CALL WZ01SEND USING SEND-CONTROL-AREA                                
507601                                                                          
507701     IF SEND-KDRC > 0                                                     
507801       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
507901       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
508001       DELIMITED BY SIZE INTO FELTEXT                                     
508101       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
508201     END-IF                                                               
508301     .                                                                    
508401     EJECT                                                                
508501 S90-SEND-OPEN SECTION.                                                   
508601                                                                          
508701     MOVE WS-ADRESS-DP                    TO SEND-ADDISPABS               
508801     MOVE 'OPEN'                          TO SEND-KDFUNC                  
508901     CALL WZ01SEND USING SEND-CONTROL-AREA                                
509001                         SEND-OPEN-AREA                                   
509101     IF SEND-KDRC > ZERO                                                  
509201       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
509301       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
509401       DELIMITED BY SIZE INTO FELTEXT                                     
509501       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
509601     END-IF                                                               
509701     .                                                                    
509801     SKIP3                                                                
509901 S91-PUT-HEADER SECTION.                                                  
510001                                                                          
510101     MOVE 'PUT'                           TO SEND-KDFUNC                  
510201     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
510301     CALL WZ01SEND USING SEND-CONTROL-AREA                                
510401                         SEND-KVDLEN                                      
510501                         HDR-AREA                                         
510601     IF SEND-KDRC > ZERO                                                  
510701       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
510801       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
510901       DELIMITED BY SIZE INTO FELTEXT                                     
511001       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
511101     END-IF                                                               
511201     .                                                                    
511301     EJECT                                                                
511401 S92-PUT-DOC-HEAD SECTION.                                                
511501                                                                          
511601     MOVE 'PUT'                           TO SEND-KDFUNC                  
511701     MOVE LENGTH OF DOC-HEAD-AREA         TO SEND-KVDLEN                  
511801     CALL WZ01SEND USING SEND-CONTROL-AREA                                
511901                         SEND-KVDLEN                                      
512001                         DOC-HEAD-AREA                                    
512101     IF SEND-KDRC > ZERO                                                  
512201       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
512301       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
512401       DELIMITED BY SIZE INTO FELTEXT                                     
512501       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
512601     END-IF                                                               
512701     .                                                                    
512801     SKIP3                                                                
512901 S93-PUT-DOC-LINE SECTION.                                                
513001                                                                          
513101     MOVE 'PUT'                           TO SEND-KDFUNC                  
513201     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
513301     CALL WZ01SEND USING SEND-CONTROL-AREA                                
513401                         SEND-KVDLEN                                      
513501                         DOC-LINE-AREA                                    
513601     IF SEND-KDRC > ZERO                                                  
513701       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
513801       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
513901       DELIMITED BY SIZE INTO FELTEXT                                     
514001       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
514101     END-IF                                                               
514201     .                                                                    
514301     EJECT                                                                
514401 S94-PUT-DOC-TOT SECTION.                                                 
514501                                                                          
514601     MOVE 'PUT'                           TO SEND-KDFUNC                  
514701     MOVE LENGTH OF DOC-TOT-AREA          TO SEND-KVDLEN                  
514801     CALL WZ01SEND USING SEND-CONTROL-AREA                                
514901                         SEND-KVDLEN                                      
515001                         DOC-TOT-AREA                                     
515101     IF SEND-KDRC > ZERO                                                  
515201       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
515301       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
515401       DELIMITED BY SIZE INTO FELTEXT                                     
515501       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
515601     END-IF                                                               
515701     .                                                                    
515801     EJECT                                                                
515901 S95-SEND-CLOSE SECTION.                                                  
516001                                                                          
516101     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
516201     CALL WZ01SEND USING SEND-CONTROL-AREA                                
516301     .                                                                    
516401     EJECT                                                                
516501 S96-SKAPA-HEADER        SECTION.                                         
516601     MOVE 'S96-SKAPA-HEAD' TO WS-SECTION                                  
516701                                                                          
516801     MOVE SPACE          TO HDR-AREA                                      
516901     MOVE 1              TO HDR-REQU-IDMSGVER                             
517001     MOVE 'R'            TO HDR-REQU-KDPGMACT                             
517101     MOVE REQU-IDUSER    TO HDR-REQU-IDUSER                               
517201                                                                          
517301     MOVE 'BINNING-LIST' TO HDR-IDOUTTYPE                                 
517401     MOVE REQU-IDDC-KEY  TO HDR-IDOUTREC(1:2)                             
517501     MOVE REQU-IDUSER    TO HDR-IDOUTREC(3:8)                             
517601     MOVE W-IDFAKT       TO HDR-IDLIST                                    
517701                                                                          
517801     PERFORM S91-PUT-HEADER                                               
517901     .                                                                    
518001     EJECT                                                                
518101 S97-PUT-DOC-LINE2 SECTION.                                               
518201                                                                          
518301     MOVE 'PUT'                           TO SEND-KDFUNC                  
518401     MOVE LENGTH OF DOC-LINE2-AREA        TO SEND-KVDLEN                  
518501     CALL WZ01SEND USING SEND-CONTROL-AREA                                
518601                         SEND-KVDLEN                                      
518701                         DOC-LINE2-AREA                                   
518801     IF SEND-KDRC > ZERO                                                  
518901       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
519001       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
519101       DELIMITED BY SIZE INTO FELTEXT                                     
519201       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
519301     END-IF                                                               
519401     .                                                                    
519501     EJECT                                                                
519601 S98-SKAPA-LEVANM-TRANS SECTION.                                          
519701                                                                          
519801     PERFORM IMS-GHU-WL630501                                             
519901     IF SEGMENT-FINNS                                                     
520001        PERFORM IMS-GHU-WL630511                                          
520101        IF SEGMENT-FINNS                                                  
520201           MOVE SPAR-AREA-6308  TO 6308-WDGX6308                          
520301           PERFORM IMS-ISRT-WL630521                                      
520401***** TILLAGT AV MÅNS FÖR ATT KLARA TVÅ RADER MED SAMMA ARTNR             
520501           PERFORM UNTIL SEGMENT-FINNS                                    
520601             ADD 1 TO 6308-IDRADNR                                        
520701             PERFORM IMS-ISRT-WL630521                                    
520801           END-PERFORM                                                    
520901        ELSE                                                              
521001           MOVE SPAR-AREA-6306  TO 6306-WDGX6306                          
521101           MOVE NEJ             TO 6306-FLKLAR                            
521201           PERFORM IMS-ISRT-WL630511                                      
521301           MOVE SPAR-AREA-6308  TO 6308-WDGX6308                          
521401           PERFORM IMS-ISRT-WL630521                                      
521501***** TILLAGT AV MÅNS FÖR ATT KLARA TVÅ RADER MED SAMMA ARTNR             
521601           PERFORM UNTIL SEGMENT-FINNS                                    
521701             ADD 1 TO 6308-IDRADNR                                        
521801             PERFORM IMS-ISRT-WL630521                                    
521901           END-PERFORM                                                    
522001        END-IF                                                            
522101     END-IF                                                               
522201     .                                                                    
522301     EJECT                                                                
522401 X010-CALL-W006KOM        SECTION.                                        
522501                                                                          
522601     MOVE KOM-AREA        TO P-TO-P-DATA                                  
522701     CALL W006KOM         USING MSG-PCB                                   
522801                                ALT3-PCB                                  
522901                                KOMA-PCB                                  
523001                                MSG-KOM-WMSGKOM                           
523101                                P-TO-P-AREA                               
523201                                                                          
523301     .                                                                    
523401     EJECT                                                                
523501 X020-UPPDATERA-WL630111 SECTION.                                         
523601                                                                          
523701     MOVE NEJ TO WS-FAKT-INFO-DLET                                        
523801                                                                          
523901     MOVE LOW-VALUE       TO W-WDL6A1KY-MIN                               
524001     MOVE HIGH-VALUE      TO W-WDL6A1KY-MAX                               
524101     MOVE W-IDFAKT        TO W-IDFAKT-MIN                                 
524201                             W-IDFAKT-MAX                                 
524301     MOVE '310'           TO W-IDPTYP                                     
524401     PERFORM IMS-06-GU-WLINLD01-FIRST-310                                 
524501                                                                          
524601     IF  SEGMENT-SAKNAS                                                   
524701         MOVE 'R30'            TO W-IDPTYP                                
524801         PERFORM IMS-06-GU-WLINLD01-FIRST-310                             
524901         IF SEGMENT-SAKNAS                                                
525001            MOVE WS-IDDC-SPAR TO W-6301-IDDC                              
525101            PERFORM IMS-01-GHU-WL630111                                   
525201            PERFORM IMS-03-DLET-WL630111                                  
525301            MOVE JA TO WS-FAKT-INFO-DLET                                  
525401         ELSE                                                             
525501            PERFORM S03-RAKNA-ARTIKLAR                                    
525601            MOVE WS-IDDC-SPAR TO W-6301-IDDC                              
525701            PERFORM IMS-01-GHU-WL630111                                   
525801            ADD +1             TO 6302-KVKOLLI-MOT                        
525901            ADD W-KVRADER      TO 6302-KVRADER-MOT                        
526001            IF REQU-CMD-IN(INDX) = 'LOS'                                  
526101               MOVE 'L'        TO 6302-KDTRPSTA                           
526201               ADD WS-RAKNARE  TO 6302-KVRADER-MOT                        
526301            END-IF                                                        
526401            IF REQU-CMD-IN(INDX) = 'RET'                                  
526501               ADD WS-RAKNARE  TO 6302-KVRADER-MOT                        
526601            END-IF                                                        
526701            PERFORM IMS-02-REPL-WL630111                                  
526801         END-IF                                                           
526901     ELSE                                                                 
527001         PERFORM S03-RAKNA-ARTIKLAR                                       
527101         MOVE WS-IDDC-SPAR    TO W-6301-IDDC                              
527201         PERFORM IMS-01-GHU-WL630111                                      
527301         ADD +1                TO 6302-KVKOLLI-MOT                        
527401         ADD W-KVRADER         TO 6302-KVRADER-MOT                        
527501         IF REQU-CMD-IN(INDX) = 'LOS'                                     
527601            MOVE 'L'        TO 6302-KDTRPSTA                              
527701            ADD WS-RAKNARE  TO 6302-KVRADER-MOT                           
527801         END-IF                                                           
527901         IF REQU-CMD-IN(INDX) = 'RET'                                     
528001            ADD WS-RAKNARE  TO 6302-KVRADER-MOT                           
528101         END-IF                                                           
528201         PERFORM IMS-02-REPL-WL630111                                     
528301     END-IF                                                               
528401     MOVE ZERO TO WS-RAKNARE                                              
528501     .                                                                    
528601     EJECT                                                                
528701 IMS-GET-KVAH11 SECTION.                                                  
528801     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
528901             DELIMITED BY SIZE INTO SSA1                                  
529001     STRING 'W6KVAH11(W6D211KY >' W-W6D211KY-X ')'                        
529101             DELIMITED BY SIZE INTO SSA2                                  
529201     MOVE '  GE' TO GODK-STATUSKODER                                      
529301     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-W6KVAH SSA1 SSA2               
529401     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
529501     PERFORM IMS-STATUSKONTROLL                                           
529601     .                                                                    
529701     SKIP3                                                                
529801 IMS-GU-WDK611 SECTION.                                                   
529901     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
530001          DELIMITED BY SIZE INTO SSA1                                     
530101     MOVE 'WDK611  ' TO SSA2                                              
530201     MOVE '  ' TO GODK-STATUSKODER                                        
530301     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WLARTC11 SSA1 SSA2             
530401     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
530501     PERFORM IMS-STATUSKONTROLL                                           
530601     .                                                                    
530701     SKIP3                                                                
530801 IMS-GHU-WDK611   SECTION.                                                
530901                                                                          
531001     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
531101          DELIMITED BY SIZE INTO SSA1                                     
531201     MOVE 'WDK611  ' TO SSA2                                              
531301     MOVE '  ' TO GODK-STATUSKODER                                        
531401     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WLARTC11 SSA1 SSA2            
531501     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
531601     PERFORM IMS-STATUSKONTROLL                                           
531701     .                                                                    
531801     SKIP2                                                                
531901 IMS-REPL-WDK611 SECTION.                                                 
532001                                                                          
532101     MOVE '  ' TO GODK-STATUSKODER                                        
532201     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WLARTC11                     
532301                                                                          
532401     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
532501     PERFORM IMS-STATUSKONTROLL                                           
532601     .                                                                    
532701     SKIP2                                                                
532801 IMS-GU-WDL6D1   SECTION.                                                 
532901     STRING 'WDL6D1  (WDL6D1KY=>' W-WDL6D1KY-MIN                          
533001                     '&WDL6D1KY=<' W-WDL6D1KY-MAX ')'                     
533101     DELIMITED BY SIZE INTO SSA1                                          
533201     MOVE '  GE' TO GODK-STATUSKODER                                      
533301     CALL CBLTDLI USING GU WDL6D-PCB DLI-IO-L6D1 SSA1                     
533401     MOVE WDL6D-STATUS-CODE TO STATUS-WS                                  
533501     PERFORM IMS-STATUSKONTROLL                                           
533601     .                                                                    
533701     SKIP3                                                                
533801                                                                          
533901 IMS-01-GHU-WL630111 SECTION.                                             
534001     MOVE 'IMS-01' TO WS-IMS-SECTION                                      
534101                                                                          
534201     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
534301          DELIMITED BY SIZE INTO SSA1                                     
534401     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
534501          DELIMITED BY SIZE INTO SSA2                                     
534601     MOVE SPACE           TO GODK-STATUSKODER                             
534701     CALL CBLTDLI USING GHU GX63-PCB DLI-IO-WLGX63 SSA1 SSA2              
534801     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
534901     PERFORM IMS-STATUSKONTROLL                                           
535001     .                                                                    
535101     SKIP3                                                                
535201 IMS-GU-WL630111 SECTION.                                                 
535301                                                                          
535401     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
535501          DELIMITED BY SIZE INTO SSA1                                     
535601     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
535701          DELIMITED BY SIZE INTO SSA2                                     
535801     MOVE SPACE           TO GODK-STATUSKODER                             
535901     CALL CBLTDLI USING GU GX63-PCB DLI-IO-WLGX63 SSA1 SSA2               
536001     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
536101     PERFORM IMS-STATUSKONTROLL                                           
536201     .                                                                    
536301     SKIP3                                                                
536401 IMS-02-REPL-WL630111 SECTION.                                            
536501     MOVE 'IMS-02' TO WS-IMS-SECTION                                      
536601                                                                          
536701     MOVE SPACE           TO GODK-STATUSKODER                             
536801     CALL CBLTDLI USING REPL GX63-PCB DLI-IO-WLGX63                       
536901     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
537001     PERFORM IMS-STATUSKONTROLL                                           
537101     .                                                                    
537201     EJECT                                                                
537301 IMS-02-ISRT-WL630111 SECTION.                                            
537401     MOVE 'IMS-02B' TO WS-IMS-SECTION                                     
537501                                                                          
537601     MOVE 'WL630111 ' TO SSA1                                             
537701     MOVE '  II' TO GODK-STATUSKODER                                      
537801     CALL CBLTDLI USING ISRT GX63-PCB DLI-IO-WLGX63 SSA1                  
537901     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
538001     PERFORM IMS-STATUSKONTROLL                                           
538101     .                                                                    
538201     SKIP3                                                                
538301 IMS-03-DLET-WL630111 SECTION.                                            
538401     MOVE 'IMS-03' TO WS-IMS-SECTION                                      
538501                                                                          
538601     MOVE SPACE           TO GODK-STATUSKODER                             
538701     CALL CBLTDLI USING DLET GX63-PCB DLI-IO-WLGX63                       
538801                                                                          
538901     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
539001     PERFORM IMS-STATUSKONTROLL                                           
539101     .                                                                    
539201     EJECT                                                                
539301 IMS-04-GU-WLINLD01 SECTION.                                              
539401     MOVE 'IMS-04' TO WS-IMS-SECTION                                      
539501                                                                          
539601     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
539701                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
539801          DELIMITED BY SIZE INTO SSA1                                     
539901     MOVE '  GE' TO GODK-STATUSKODER                                      
540001     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA1 SSA1                     
540101                                                                          
540201     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
540301     PERFORM IMS-STATUSKONTROLL                                           
540401     .                                                                    
540501     SKIP3                                                                
540601 IMS-05-GN-WLINLD01 SECTION.                                              
540701     MOVE 'IMS-05' TO WS-IMS-SECTION                                      
540801                                                                          
540901     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
541001                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
541101          DELIMITED BY SIZE INTO SSA1                                     
541201     MOVE '  GE' TO GODK-STATUSKODER                                      
541301     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA1 SSA1                     
541401                                                                          
541501     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
541601     PERFORM IMS-STATUSKONTROLL                                           
541701     .                                                                    
541801     EJECT                                                                
541901 IMS-06-GU-WLINLD01-FIRST-310 SECTION.                                    
542001     MOVE 'IMS-06' TO WS-IMS-SECTION                                      
542101                                                                          
542201     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
542301                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
542401                    '&IDPTYP  = ' W-IDPTYP ')'                            
542501          DELIMITED BY SIZE INTO SSA1                                     
542601     MOVE '  GE' TO GODK-STATUSKODER                                      
542701     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA1 SSA1                     
542801     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
542901     PERFORM IMS-STATUSKONTROLL                                           
543001     .                                                                    
543101     SKIP3                                                                
543201 IMS-07-GU-INLC-WLINLC11-F SECTION.                                       
543301     MOVE 'IMS-07' TO WS-IMS-SECTION                                      
543401                                                                          
543501     STRING 'WLINLC11(WDL6ASEQ=>' W-WDL6ASEQ-MIN                          
543601                    '&WDL6ASEQ=<' W-WDL6ASEQ-MAX ')'                      
543701          DELIMITED BY SIZE INTO SSA1                                     
543801     MOVE '  GE' TO GODK-STATUSKODER                                      
543901     CALL CBLTDLI USING GU INLC-PCB DLI-IO-WLINLC SSA1                    
544001     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
544101     PERFORM IMS-STATUSKONTROLL                                           
544201     .                                                                    
544301     SKIP3                                                                
544401 IMS-08-GN-INLC-WLINLC11 SECTION.                                         
544501     MOVE 'IMS-08' TO WS-IMS-SECTION                                      
544601                                                                          
544701     STRING 'WLINLC11(WDL6ASEQ=>' W-WDL6ASEQ-MIN                          
544801                    '&WDL6ASEQ=<' W-WDL6ASEQ-MAX ')'                      
544901          DELIMITED BY SIZE INTO SSA1                                     
545001     MOVE '  GE' TO GODK-STATUSKODER                                      
545101     CALL CBLTDLI USING GN INLC-PCB DLI-IO-WLINLC SSA1                    
545201                                                                          
545301     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
545401     PERFORM IMS-STATUSKONTROLL                                           
545501     .                                                                    
545601     EJECT                                                                
545701 IMS-09-GHU-INLC1-WLINLC11 SECTION.                                       
545801     MOVE 'IMS-09' TO WS-IMS-SECTION                                      
545901                                                                          
546001     STRING 'WLINLC01(IDARTNR = ' W-IDARTNR-X ')'                         
546101          DELIMITED BY SIZE INTO SSA1                                     
546201     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
546301          DELIMITED BY SIZE INTO SSA2                                     
546401     MOVE '  ' TO GODK-STATUSKODER                                        
546501     CALL CBLTDLI USING GHU INLC1-PCB DLI-IO-WLINLC SSA1 SSA2             
546601                                                                          
546701     MOVE INLC1-STATUS-CODE TO STATUS-WS                                  
546801     PERFORM IMS-STATUSKONTROLL                                           
546901     .                                                                    
547001     SKIP3                                                                
547101 IMS-GU-INLC1-WLINLC11 SECTION.                                           
547201                                                                          
547301     STRING 'WLINLC01(IDARTNR = ' W-IDARTNR-X ')'                         
547401          DELIMITED BY SIZE INTO SSA1                                     
547501     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
547601          DELIMITED BY SIZE INTO SSA2                                     
547701     MOVE '  ' TO GODK-STATUSKODER                                        
547801     CALL CBLTDLI USING GU INLC1-PCB DLI-IO-WLINLC SSA1 SSA2              
547901                                                                          
548001     MOVE INLC1-STATUS-CODE TO STATUS-WS                                  
548101     PERFORM IMS-STATUSKONTROLL                                           
548201     .                                                                    
548301     SKIP3                                                                
548401 IMS-10-REPL-INLC1-WLINLC11 SECTION.                                      
548501     MOVE 'IMS-10' TO WS-IMS-SECTION                                      
548601                                                                          
548701     MOVE '  ' TO GODK-STATUSKODER                                        
548801     CALL CBLTDLI USING REPL INLC1-PCB DLI-IO-WLINLC                      
548901                                                                          
549001     MOVE INLC1-STATUS-CODE TO STATUS-WS                                  
549101     PERFORM IMS-STATUSKONTROLL                                           
549201     .                                                                    
549301     SKIP3                                                                
549401 IMS-11-GU-WDK711 SECTION.                                                
549501     MOVE 'IMS-11' TO WS-IMS-SECTION                                      
549601                                                                          
549701     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
549801          DELIMITED BY SIZE INTO SSA1                                     
549901     STRING 'WDK711  (IDDC    = ' W-IDDC  ')'                             
550001          DELIMITED BY SIZE INTO SSA2                                     
550101     MOVE SPACE  TO GODK-STATUSKODER                                      
550201     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
550301     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
550401     PERFORM IMS-STATUSKONTROLL                                           
550501     .                                                                    
550601     EJECT                                                                
550701 IMS-12-GHU-WDK711 SECTION.                                               
550801     MOVE 'IMS-12' TO WS-IMS-SECTION                                      
550901                                                                          
551001     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
551101          DELIMITED BY SIZE INTO SSA1                                     
551201     STRING 'WDK711  (IDDC    = ' W-IDDC  ')'                             
551301          DELIMITED BY SIZE INTO SSA2                                     
551401     MOVE SPACE  TO GODK-STATUSKODER                                      
551501     CALL CBLTDLI USING GHU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2             
551601     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
551701     PERFORM IMS-STATUSKONTROLL                                           
551801     .                                                                    
551901     SKIP3                                                                
552001 IMS-13-REPL-WDK711 SECTION.                                              
552101     MOVE 'IMS-13' TO WS-IMS-SECTION                                      
552201                                                                          
552301     MOVE '  ' TO GODK-STATUSKODER                                        
552401     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
552501                                                                          
552601     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
552701     PERFORM IMS-STATUSKONTROLL                                           
552801     .                                                                    
552901     SKIP3                                                                
553001 IMS-14-GU-WDK712 SECTION.                                                
553101     MOVE 'IMS-14' TO WS-IMS-SECTION                                      
553201                                                                          
553301     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
553401          DELIMITED BY SIZE INTO SSA1                                     
553501     STRING 'WDK712  (IDLAND  = ' W-IDLAND-X ')'                          
553601          DELIMITED BY SIZE INTO SSA2                                     
553701     MOVE '  GE' TO GODK-STATUSKODER                                      
553801     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
553901     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
554001     PERFORM IMS-STATUSKONTROLL                                           
554101     .                                                                    
554201     EJECT                                                                
554301 IMS-15-GET-WLARTC01 SECTION.                                             
554401     MOVE 'IMS-15' TO WS-IMS-SECTION                                      
554501                                                                          
554601     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
554701          DELIMITED BY SIZE INTO SSA1                                     
554801     MOVE SPACE  TO GODK-STATUSKODER                                      
554901     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-WLARTC01 SSA1                 
555001     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
555101     PERFORM IMS-STATUSKONTROLL                                           
555201     .                                                                    
555301     SKIP3                                                                
555401 IMS-16-GET-WLARTC11 SECTION.                                             
555501     MOVE 'IMS-16' TO WS-IMS-SECTION                                      
555601                                                                          
555701     MOVE 'WLARTC11 ' TO SSA1                                             
555801     MOVE SPACE  TO GODK-STATUSKODER                                      
555901     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
556001     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
556101     PERFORM IMS-STATUSKONTROLL                                           
556201     .                                                                    
556301     EJECT                                                                
556401 IMS-17-GET-ARTC11 SECTION.                                               
556501     MOVE 'IMS-17' TO WS-IMS-SECTION                                      
556601                                                                          
556701     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
556801          DELIMITED BY SIZE INTO SSA1                                     
556901     MOVE 'WLARTC11 ' TO SSA2                                             
557001     MOVE '  GE' TO GODK-STATUSKODER                                      
557101     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2             
557201     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
557301     PERFORM IMS-STATUSKONTROLL                                           
557401     .                                                                    
557501     SKIP3                                                                
557601 IMS-18-ISRT-WDL901 SECTION.                                              
557701     MOVE 'IMS-18' TO WS-IMS-SECTION                                      
557801                                                                          
557901     MOVE 'WLLOGA01 ' TO SSA1                                             
558001     MOVE '  II' TO GODK-STATUSKODER                                      
558101     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
558201     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
558301     PERFORM IMS-STATUSKONTROLL                                           
558401     .                                                                    
558501     SKIP3                                                                
558601 IMS-19-ISRT-WLSAPA01 SECTION.                                            
558701     MOVE 'IMS-19' TO WS-IMS-SECTION                                      
558801                                                                          
558901     MOVE 'WLSAPA01 ' TO SSA1                                             
559001     MOVE '  II' TO GODK-STATUSKODER                                      
559101     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
559201     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
559301     PERFORM IMS-STATUSKONTROLL                                           
559401     .                                                                    
559501     SKIP3                                                                
559601 IMS-20-ISRT-WLFILC SECTION.                                              
559701     MOVE 'IMS-20' TO WS-IMS-SECTION                                      
559801                                                                          
559901     STRING 'WLFILC01    '                                                
560001          DELIMITED BY SIZE INTO SSA1                                     
560101     MOVE '   ' TO GODK-STATUSKODER                                       
560201     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC SSA1               
560301     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
560401     PERFORM IMS-STATUSKONTROLL                                           
560501     .                                                                    
560601     EJECT                                                                
560701 IMS-21-ISRT-WLFILC2 SECTION.                                             
560801     MOVE 'IMS-21' TO WS-IMS-SECTION                                      
560901                                                                          
561001     STRING 'WLFILC01    '                                                
561101          DELIMITED BY SIZE INTO SSA1                                     
561201     MOVE '   ' TO GODK-STATUSKODER                                       
561301     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC2 SSA1              
561401     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
561501     PERFORM IMS-STATUSKONTROLL                                           
561601     .                                                                    
561701     EJECT                                                                
561801                                                                          
561901 IMS-22-GU-WDB601 SECTION.                                                
562001     MOVE 'IMS-22' TO WS-IMS-SECTION                                      
562101                                                                          
562201     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
562301          DELIMITED BY SIZE INTO SSA1                                     
562401     MOVE '  GE' TO GODK-STATUSKODER                                      
562501     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
562601     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
562701     PERFORM IMS-STATUSKONTROLL                                           
562801     IF SEGMENT-SAKNAS                                                    
562901        MOVE SPACE TO DCS-KDDC                                            
563001                      DCS-IDLANDX2                                        
563101     END-IF                                                               
563201     .                                                                    
563301     EJECT                                                                
563401 IMS-22-GU-WDB601-REC SECTION.                                            
563501     MOVE 'IMS-22' TO WS-IMS-SECTION                                      
563601                                                                          
563701     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
563801          DELIMITED BY SIZE INTO SSA1                                     
563901     MOVE '  GE' TO GODK-STATUSKODER                                      
564001     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-REC SSA1             
564101     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
564201     PERFORM IMS-STATUSKONTROLL                                           
564301     .                                                                    
564401     EJECT                                                                
564501 IMS-23-GET-WL630111 SECTION.                                             
564601     MOVE 'IMS-23' TO WS-IMS-SECTION                                      
564701                                                                          
564801     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
564901          DELIMITED BY SIZE INTO SSA1                                     
565001     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
565101          DELIMITED BY SIZE INTO SSA2                                     
565201     MOVE SPACE TO GODK-STATUSKODER                                       
565301     CALL CBLTDLI USING GU GX63-PCB DLI-IO-WLGX63 SSA1 SSA2               
565401     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
565501     PERFORM IMS-STATUSKONTROLL                                           
565601     .                                                                    
565701     EJECT                                                                
565801 IMS-24-GET-WDD311 SECTION.                                               
565901     MOVE 'IMS-24' TO WS-IMS-SECTION                                      
566001                                                                          
566101     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
566201             DELIMITED BY SIZE INTO SSA1                                  
566301     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
566401              DELIMITED BY SIZE INTO SSA2                                 
566501     MOVE '  GE' TO GODK-STATUSKODER                                      
566601     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
566701     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
566801     PERFORM IMS-STATUSKONTROLL                                           
566901     .                                                                    
567001     EJECT                                                                
567101 IMS-25-GN-WDL6A1 SECTION.                                                
567201     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
567301                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
567401                    '&IDPTYP  = ' W-IDPTYP ')'                            
567501          DELIMITED BY SIZE INTO SSA1                                     
567601     MOVE '  GEGB' TO GODK-STATUSKODER                                    
567701     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA1 SSA1                     
567801     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
567901     PERFORM IMS-STATUSKONTROLL                                           
568001     .                                                                    
568101     SKIP3                                                                
568201 IMS-26-GU-WDL6A1  SECTION.                                               
568301     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
568401                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
568501                    '&IDPTYP  = ' W-IDPTYP ')'                            
568601          DELIMITED BY SIZE INTO SSA1                                     
568701     MOVE '  GE' TO GODK-STATUSKODER                                      
568801     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA1 SSA1                     
568901     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
569001     PERFORM IMS-STATUSKONTROLL                                           
569101     .                                                                    
569201     EJECT                                                                
569301 IMS-27-GU-WDL6A1  SECTION.                                               
569401     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
569501                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
569601                    '&IDPTYP  = ' W-IDPTYP ')'                            
569701          DELIMITED BY SIZE INTO SSA1                                     
569801     MOVE '  GE' TO GODK-STATUSKODER                                      
569901     CALL CBLTDLI USING GU INLD2-PCB DLI-IO-AREA1 SSA1                    
570001     MOVE INLD2-STATUS-CODE TO STATUS-WS                                  
570101     PERFORM IMS-STATUSKONTROLL                                           
570201     .                                                                    
570301     EJECT                                                                
570401 IMS-28-GN-WDL6A1 SECTION.                                                
570501     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
570601                    '&WDL6A1KY=<' W-WDL6A1KY-MAX                          
570701                    '&IDPTYP  = ' W-IDPTYP ')'                            
570801          DELIMITED BY SIZE INTO SSA1                                     
570901     MOVE '  GEGB' TO GODK-STATUSKODER                                    
571001     CALL CBLTDLI USING GN INLD2-PCB DLI-IO-AREA1 SSA1                    
571101     MOVE INLD2-STATUS-CODE TO STATUS-WS                                  
571201     PERFORM IMS-STATUSKONTROLL                                           
571301     .                                                                    
571401     SKIP3                                                                
571501 IMS-29-GU-INLC2-WLINLC11-F SECTION.                                      
571601     MOVE 'IMS-29' TO WS-IMS-SECTION                                      
571701                                                                          
571801     STRING 'WLINLC11(WDL6CSEQ=>' W-WDL6CSEQ-MIN                          
571901                    '&WDL6CSEQ=<' W-WDL6CSEQ-MAX ')'                      
572001          DELIMITED BY SIZE INTO SSA1                                     
572101     MOVE '  GE' TO GODK-STATUSKODER                                      
572201     CALL CBLTDLI USING GU INLC2-PCB DLI-IO-WLINLC SSA1                   
572301     MOVE INLC2-STATUS-CODE TO STATUS-WS                                  
572401     PERFORM IMS-STATUSKONTROLL                                           
572501     .                                                                    
572601     SKIP3                                                                
572701 IMS-30-GN-INLC2-WLINLC11 SECTION.                                        
572801     MOVE 'IMS-30' TO WS-IMS-SECTION                                      
572901                                                                          
573001     STRING 'WLINLC11(WDL6CSEQ=>' W-WDL6CSEQ-MIN                          
573101                    '&WDL6CSEQ=<' W-WDL6CSEQ-MAX ')'                      
573201          DELIMITED BY SIZE INTO SSA1                                     
573301     MOVE '  GE' TO GODK-STATUSKODER                                      
573401     CALL CBLTDLI USING GN INLC2-PCB DLI-IO-WLINLC SSA1                   
573501                                                                          
573601     MOVE INLC2-STATUS-CODE TO STATUS-WS                                  
573701     PERFORM IMS-STATUSKONTROLL                                           
573801     .                                                                    
573802     SKIP3                                                                
573803 IMS-31-GNP-INLC2-WLINLC01 SECTION.                                       
573804     MOVE 'IMS-31' TO WS-IMS-SECTION                                      
573805                                                                          
573806     MOVE 'WLINLC01 ' TO SSA1                                             
573810     MOVE '  ' TO GODK-STATUSKODER                                        
573811     CALL CBLTDLI USING GNP INLC2-PCB DLI-IO-WLINLC01 SSA1                
573820                                                                          
573830     MOVE INLC2-STATUS-CODE TO STATUS-WS                                  
573840     PERFORM IMS-STATUSKONTROLL                                           
573850     .                                                                    
573901     SKIP3                                                                
574001 IMS-ISRT-FILB01 SECTION.                                                 
574101     MOVE 'IMS-ISRT-FILB' TO WS-IMS-SECTION                               
574201                                                                          
574301     MOVE 'WLFILB01 ' TO SSA1                                             
574401     MOVE '  II' TO GODK-STATUSKODER                                      
574501     CALL CBLTDLI USING ISRT FILB-PCB WLFILB01 SSA1                       
574601     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
574701     PERFORM IMS-STATUSKONTROLL                                           
574801     .                                                                    
574901     EJECT                                                                
575001 IMS-GHU-WL630111 SECTION.                                                
575101                                                                          
575201     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
575301          DELIMITED BY SIZE INTO SSA1                                     
575401     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
575501          DELIMITED BY SIZE INTO SSA2                                     
575601     MOVE SPACE           TO GODK-STATUSKODER                             
575701     CALL CBLTDLI USING GHU GX63-PCB DLI-IO-WLGX63 SSA1 SSA2              
575801                                                                          
575901     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
576001     PERFORM IMS-STATUSKONTROLL                                           
576101     .                                                                    
576201     SKIP3                                                                
576301 IMS-GU-WDB301 SECTION.                                                   
576401                                                                          
576501     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
576601                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
576701          DELIMITED BY SIZE INTO SSA1                                     
576801     MOVE '  GE' TO GODK-STATUSKODER                                      
576901     CALL CBLTDLI USING GHU KNDB-PCB DLI-IO-AREA3 SSA1                    
577001     MOVE KNDB-STATUS-CODE TO STATUS-WS                                   
577101     PERFORM IMS-STATUSKONTROLL                                           
577201     .                                                                    
577301     EJECT                                                                
577401 IMS-GHU-WL630501 SECTION.                                                
577501     STRING 'WL630501(WDGXKEY = ' W-6305KEY-X ')'                         
577601          DELIMITED BY SIZE INTO SSA1                                     
577701     MOVE 'GE  ' TO GODK-STATUSKODER                                      
577801     CALL CBLTDLI USING GHU GX65-PCB DLI-IO-AREA5 SSA1                    
577901     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
578001     PERFORM IMS-STATUSKONTROLL                                           
578101     .                                                                    
578201     SKIP2                                                                
578301 IMS-GHU-WL630511 SECTION.                                                
578401     STRING 'WL630501(WDGXKEY = ' W-6305KEY-X ')'                         
578501          DELIMITED BY SIZE INTO SSA1                                     
578601     STRING 'WL630511(IDFAKT  = ' W-IDFAKT-X  ')'                         
578701          DELIMITED BY SIZE INTO SSA2                                     
578801     MOVE 'GE  ' TO GODK-STATUSKODER                                      
578901     CALL CBLTDLI USING GHU GX65-PCB DLI-IO-AREA5 SSA1 SSA2               
579001     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
579101     PERFORM IMS-STATUSKONTROLL                                           
579201     .                                                                    
579301     EJECT                                                                
579401 IMS-ISRT-WL630511 SECTION.                                               
579501     STRING 'WL630501(WDGXKEY = ' W-6305KEY-X  ')'                        
579601          DELIMITED BY SIZE INTO SSA1                                     
579701     MOVE 'WL630511 ' TO SSA2                                             
579801     MOVE SPACE TO GODK-STATUSKODER                                       
579901     CALL CBLTDLI USING ISRT GX65-PCB DLI-IO-AREA5 SSA1 SSA2              
580001     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
580101     PERFORM IMS-STATUSKONTROLL                                           
580201     .                                                                    
580301     SKIP2                                                                
580401 IMS-GHU-INLC1-WLINLC11 SECTION.                                          
580501                                                                          
580601     STRING 'WLINLC01(IDARTNR = ' W-IDARTNR-X ')'                         
580701          DELIMITED BY SIZE INTO SSA1                                     
580801     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
580901          DELIMITED BY SIZE INTO SSA2                                     
581001     MOVE '  ' TO GODK-STATUSKODER                                        
581101     CALL CBLTDLI USING GHU INLC1-PCB DLI-IO-WLINLC SSA1 SSA2             
581201                                                                          
581301     MOVE INLC1-STATUS-CODE TO STATUS-WS                                  
581401     PERFORM IMS-STATUSKONTROLL                                           
581501     .                                                                    
581601     SKIP3                                                                
581701 IMS-REPL-INLC1-WLINLC11 SECTION.                                         
581801                                                                          
581901     MOVE '  ' TO GODK-STATUSKODER                                        
582001     CALL CBLTDLI USING REPL INLC1-PCB DLI-IO-WLINLC                      
582101                                                                          
582201     MOVE INLC1-STATUS-CODE TO STATUS-WS                                  
582301     PERFORM IMS-STATUSKONTROLL                                           
582401     .                                                                    
582501     EJECT                                                                
582601 IMS-GU-WDB601-SEND SECTION.                                              
582701     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
582801          DELIMITED BY SIZE INTO SSA1                                     
582901     MOVE '  ' TO GODK-STATUSKODER                                        
583001     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SEND SSA1            
583101     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
583201     PERFORM IMS-STATUSKONTROLL                                           
583301     .                                                                    
583401     EJECT                                                                
583501 IMS-GET-WLARTC01 SECTION.                                                
583601                                                                          
583701     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
583801          DELIMITED BY SIZE INTO SSA1                                     
583901     MOVE SPACE  TO GODK-STATUSKODER                                      
584001     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-WLARTC01   SSA1               
584101     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
584201     PERFORM IMS-STATUSKONTROLL                                           
584301     .                                                                    
584401     SKIP3                                                                
584501 IMS-GET-WLARTC11 SECTION.                                                
584601                                                                          
584701     MOVE 'WLARTC11 ' TO SSA1                                             
584801     MOVE SPACE  TO GODK-STATUSKODER                                      
584901     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
585001     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
585101     PERFORM IMS-STATUSKONTROLL                                           
585201     .                                                                    
585301     SKIP2                                                                
585401 IMS-GU-WLINLD01 SECTION.                                                 
585501                                                                          
585601     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
585701                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
585801          DELIMITED BY SIZE INTO SSA1                                     
585901     MOVE '  GE' TO GODK-STATUSKODER                                      
586001     CALL CBLTDLI USING GU INLD-PCB DLI-IO-AREA1 SSA1                     
586101                                                                          
586201     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
586301     PERFORM IMS-STATUSKONTROLL                                           
586401     .                                                                    
586501     SKIP3                                                                
586601 IMS-GN-WLINLD01 SECTION.                                                 
586701                                                                          
586801     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
586901                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
587001          DELIMITED BY SIZE INTO SSA1                                     
587101     MOVE '  GE' TO GODK-STATUSKODER                                      
587201     CALL CBLTDLI USING GN INLD-PCB DLI-IO-AREA1 SSA1                     
587301                                                                          
587401     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
587501     PERFORM IMS-STATUSKONTROLL                                           
587601     .                                                                    
587701     EJECT                                                                
587801 IMS-GU-WDL623    SECTION.                                                
587901     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
588001          DELIMITED BY SIZE INTO SSA1                                     
588101     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
588201          DELIMITED BY SIZE INTO SSA2                                     
588301     MOVE 'WDL623 ' TO SSA3                                               
588401     MOVE '  GE' TO GODK-STATUSKODER                                      
588501     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL623                         
588601                             SSA1 SSA2 SSA3                               
588701     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
588801     PERFORM IMS-STATUSKONTROLL                                           
588901     .                                                                    
589001     EJECT                                                                
589101 IMS-GHU-WDL623    SECTION.                                               
589201     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
589301          DELIMITED BY SIZE INTO SSA1                                     
589401     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
589501          DELIMITED BY SIZE INTO SSA2                                     
589601     MOVE 'WDL623 ' TO SSA3                                               
589701     MOVE '  GE' TO GODK-STATUSKODER                                      
589801     CALL CBLTDLI USING GHU WDL6-PCB DLI-IO-WDL623                        
589901                             SSA1 SSA2 SSA3                               
590001     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
590101     PERFORM IMS-STATUSKONTROLL                                           
590201     .                                                                    
590301     EJECT                                                                
590401 IMS-ISRT-WDL623    SECTION.                                              
590501     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
590601          DELIMITED BY SIZE INTO SSA1                                     
590701     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
590801          DELIMITED BY SIZE INTO SSA2                                     
590901     MOVE 'WDL623 ' TO SSA3                                               
591001     MOVE '  II' TO GODK-STATUSKODER                                      
591101     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-WDL623                       
591201                             SSA1 SSA2 SSA3                               
591301     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
591401     PERFORM IMS-STATUSKONTROLL                                           
591501     .                                                                    
591601     EJECT                                                                
591701 IMS-REPL-WDL623    SECTION.                                              
591801     MOVE 'WDL623   ' TO SSA1                                             
591901     MOVE '  ' TO GODK-STATUSKODER                                        
592001     CALL CBLTDLI USING REPL WDL6-PCB DLI-IO-WDL623 SSA1                  
592101     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
592201     PERFORM IMS-STATUSKONTROLL                                           
592301     .                                                                    
592401     EJECT                                                                
592501 IMS-GU-WDB601-SPAR SECTION.                                              
592601     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
592701          DELIMITED BY SIZE INTO SSA1                                     
592801     MOVE '  ' TO GODK-STATUSKODER                                        
592901     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SPAR SSA1            
593001     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
593101     PERFORM IMS-STATUSKONTROLL                                           
593201     .                                                                    
593301                                                                          
593401 IMS-ISRT-WL630521 SECTION.                                               
593501     STRING 'WL630501(WDGXKEY = ' W-6305KEY-X   ')'                       
593601          DELIMITED BY SIZE INTO SSA1                                     
593701     STRING 'WL630511(IDFAKT  = ' W-IDFAKT-X     ')'                      
593801          DELIMITED BY SIZE INTO SSA2                                     
593901     MOVE 'WL630521 ' TO SSA3                                             
594001     MOVE '  II' TO GODK-STATUSKODER                                      
594101     CALL CBLTDLI USING ISRT GX65-PCB DLI-IO-AREA5 SSA1 SSA2              
594201                                                   SSA3                   
594301     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
594401     PERFORM IMS-STATUSKONTROLL                                           
594501     .                                                                    
594601     SKIP3                                                                
594701 IMS-REPL-WL630511 SECTION.                                               
594801     MOVE SPACE TO GODK-STATUSKODER                                       
594901     CALL CBLTDLI USING REPL GX65-PCB DLI-IO-AREA5                        
595001     MOVE GX65-STATUS-CODE TO STATUS-WS                                   
595101     PERFORM IMS-STATUSKONTROLL                                           
595201     .                                                                    
595301     EJECT                                                                
595401 IMS-GU-W6G130 SECTION.                                                   
595501                                                                          
595601     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
595701          DELIMITED BY SIZE INTO SSA1                                     
595801     STRING 'W6G130  (W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
595901          DELIMITED BY SIZE INTO SSA2                                     
596001     MOVE '  GE' TO GODK-STATUSKODER                                      
596101     CALL CBLTDLI USING GU W6G1-PCB DLI-IO-AREA-W6G130 SSA1 SSA2          
596201     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
596301     PERFORM IMS-STATUSKONTROLL                                           
596401     .                                                                    
596501     EJECT                                                                
596601 IMS-STATUSKONTROLL SECTION.                                              
596701     SET STATUS-IX TO 1                                                   
596801     SEARCH GODK-STATUS                                                   
596901       AT END                                                             
597001         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
597101         DELIMITED BY SIZE INTO FELTEXT                                   
597201         CALL FELLOG                                                      
597301       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
597401         CONTINUE                                                         
598001     END-SEARCH                                                           
600000     .                                                                    
