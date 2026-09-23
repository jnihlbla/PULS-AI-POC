000100                                                                          
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0130      *        
000400******************************************************************        
000500                                                                          
000600 ID DIVISION.                                                             
000700 PROGRAM-ID.     W4035200.                                                
000800 AUTHOR.         ROGER OLSSON.                                            
000900 DATE-WRITTEN.   90/04/09.                                                
001000 DATE-COMPILED.                                                           
001100                                                                          
001200*                                                                         
001300*    FUNKTION.                                                            
001400*        VISAR ORDERDELAR SOM TILLHÖR VISS PRC-KANAL.                     
001500*        ORDERDELAR SOM MARKERAS MED 'X' BILDAR EN PLOCKSATS.             
001600*                                                                         
001700*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001800*        PROGRAMMET UPPDATERAR WLORQA (WDQ3)                              
001900*                              WLXXKQ (WDR4)                              
002000*        PROGRAMMET LÄSER      WLXXKH (WDR1)                              
002100*        PROGRAMMET LÄSER      WDB2                                       
002200*                                                                         
002300*    E-TRACKER:7898645 ADDITION OF NEW FIELDS TO WDGX4002                 
002400*                      4002-IDMSG3IV 4002-IDSNO3IV 4002-ADDISPXTRA        
002500*                                                                         
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W4T352                                              
002900*        MID:         W4I35201                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W4O35201                                            
003300*                                                                         
003400*    JUNI-AUGUSTI -04   GÖRAN KJELLSON  GUIDE                             
003500*    CONSOLIDATE ORDERS                                                   
003600*    FUNKTION FÖR ATT ENBART VISA ORDER MED SAMMA DISTRIKT/KUNDNR         
003700*    DESSUTOM EFFEKTIVISERAS EN DEL IMS-LÄSNINGAR DÄR SEKUNDÄRA           
003800*    NYCKLAR ANVÄNDS                                                      
003900*                                                                         
003910*    2019-09-19:  ADDED SCROLLING (PF6) -  LOVISH KUMAR                   
003920*                                                                         
004000                                                                          
004100     SKIP3                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700*    -- CHECKED BY WY2000                                                 
004800     SKIP3                                                                
004900 77  IDPGM                       PIC X(08)   VALUE 'W4035200'.            
005000 77  FELTEXT                     PIC X(32)   VALUE SPACE.                 
005100                                                                          
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  YES                         PIC X       VALUE 'Y'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500 77  EXPAND                      PIC X       VALUE 'E'.                   
005600                                                                          
005700 77  PGMPOS                      PIC X(16)   VALUE SPACE.                 
005800                                                                          
005900 77  IX1                         PIC S9(9)  VALUE +0    COMP SYNC.        
006000 77  IX2                         PIC S9(9)  VALUE +0    COMP SYNC.        
006100 77  MAX-IX                      PIC S9(9)  VALUE +11   COMP SYNC.        
006200 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006300 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1472 COMP SYNC.        
006400 77  MAX-VARDE                   PIC  9(11) VALUE 99999999999.            
006500 77  MAX-ANTAL-ORDERDELAR        PIC S9(3)  VALUE 99    COMP-3.           
006600 77  MAX-PLKSATS-RADER           PIC S9(4)  VALUE 1500  COMP-3.           
006700 77  WS-IDTIDZON                 PIC  X(2).                               
006800 77  WS-KDMATT                   PIC X(1).                                
006900     88 US-MATT                  VALUE 'U'.                               
007000                                                                          
007200 01      WS-ORDDEL.                                                       
007300   03    WS-ODEL-IDORDER         PIC S9(7) COMP-3.                        
007400   03    WS-ODEL-IDDC            PIC X(2).                                
007500   03    WS-ODEL-IDPRODNR        PIC S9(7) COMP-3.                        
007600   03    WS-ODEL-IDPLKLST        PIC S9(3) COMP-3.                        
007700                                                                          
007800 01      WS-TAB-ORDERDELAR.                                               
007900   03    WS-TAB-ORDDEL           OCCURS 99.                               
008000     05  WS-TAB-IDORDER          PIC S9(7) COMP-3.                        
008100     05  WS-TAB-IDDC             PIC X(2).                                
008200     05  WS-TAB-IDPRODNR         PIC S9(7) COMP-3.                        
008300     05  WS-TAB-IDPLKLST         PIC S9(3) COMP-3.                        
008400                                                                          
008500 01      WS-TABELLRAD.                                                    
008600   03    WS-LST                  PIC X(8).                                
008700   03    FILLER                  PIC X(1).                                
008800   03    WS-P-TID                PIC ZZ9.99.                              
008900   03    FILLER                  PIC X(1).                                
009000   03    WS-RFS                  PIC X(8).                                
009100   03    WS-RADER                PIC Z(4)9.                               
009200   03    WS-VIKT                 PIC Z(5)9.9.                             
009300   03    WS-VOLYM                PIC Z(3)9.999.                           
009400   03    FILLER                  PIC X(1).                                
009500   03    WS-TRP                  PIC X(5).                                
009600   03    WS-PLOCK                PIC ZZ9.                                 
009700   03    WS-4002-KDPRT-PU        PIC X(3).                                
009800   03    WS-4002-KDPRT-PLE       PIC X(3).                                
009900                                                                          
010000 01  FILLER                      PIC X(16)  VALUE 'EXPAND-KUND'.          
010100 01  W-ANTAL-MOD-RADER           PIC S9(9)  VALUE +0    COMP SYNC.        
010200 01  EXP-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
010300 01  EXPAND-KUND-TAB.                                                     
010400     03  EXPAND-KUND  OCCURS 12.                                          
010500         05  EXP-IDDISTR         PIC X(4).                                
010600         05  EXP-IDKUNDNR        PIC X(6).                                
010700         05  EXP-IDDISTR-NUM     PIC S9(5) COMP-3.                        
010800         05  EXP-IDKUNDNR-NUM    PIC S9(7) COMP-3.                        
010900         05  EXP-FLER            PIC X.                                   
011000                                                                          
011100 01      WS-KLOCKAN.                                                      
011200   03    WS-TIHHMMSS             PIC 9(6).                                
011300   03    FILLER                  PIC X(2).                                
011400 01      FILLER REDEFINES WS-KLOCKAN.                                     
011500   03    WS-TIHHMM               PIC 9(4).                                
011600   03    FILLER                  PIC X(4).                                
011700                                                                          
011800 01      WS-KLOCKAN-LOK.                                                  
011900   03    WS-TIHHMMSS-LOK         PIC 9(6).                                
012000   03    FILLER                  PIC X(2).                                
012100 01      FILLER REDEFINES WS-KLOCKAN-LOK.                                 
012200   03    WS-TIHHMM-LOK           PIC 9(4).                                
012300   03    FILLER                  PIC X(4).                                
012400                                                                          
012500 01      WS-LST-TMP1             PIC S9(11) COMP-3.                       
012600 01      WS-LST-TMP2             PIC S9(11) COMP-3.                       
012700 01      WS-DALSTORD             PIC 9(12).                               
012800 01      WS-DALST                PIC 9(12).                               
012900 01      FILLER REDEFINES WS-DALST.                                       
013000   03    WS-TI-SEKEL             PIC 9(2).                                
013100   03    WS-TI-DATUM             PIC 9(6).                                
013200   03    WS-TI-HHMM              PIC 9(4).                                
013300                                                                          
013400 01      WS-TIRFS                PIC 9(11).                               
013500 01      FILLER REDEFINES WS-TIRFS.                                       
013600   03    FILLER                  PIC X(1).                                
013700   03    WS-RFS-DATE             PIC 9(6).                                
013800   03    WS-RFS-TIME             PIC X(4).                                
013900                                                                          
014000 01      WS-LST-RFS              PIC 9(12).                               
014100 01      FILLER REDEFINES WS-LST-RFS.                                     
014200   03    FILLER                  PIC 9(4).                                
014300   03    WS-LST-RFS-TAB          PIC 9(8).                                
014400                                                                          
014500 77      WS-ANTPLK               PIC S9(3)      COMP-3 VALUE 0.           
014700 77      WS-ANTPLK-ENTER         PIC S9(3)      COMP-3 VALUE 0.           
015000 77      WS-PRC-KVORDER          PIC S9(7)      COMP-3 VALUE 0.           
015200 77      WS-PRC-KVRADER          PIC S9(5)      COMP-3 VALUE 0.           
015400 77      WS-KVORDER-ENTER        PIC S9(7)      COMP-3 VALUE 0.           
015600 77      WS-KVORDER              PIC S9(7)      COMP-3 VALUE 0.           
015800 77      WS-KVRADER-ENTER        PIC S9(5)      COMP-3 VALUE 0.           
016000 77      WS-KVRADER              PIC S9(5)      COMP-3 VALUE 0.           
016100 77      WS-PLKSATS-KVRADER      PIC S9(5)      COMP-3 VALUE 0.           
016300 77      WS-DATUM                PIC 9(6).                                
016400 77      WS-DATUM-LOK            PIC 9(6).                                
016500*                                                                         
016600 01      WS-ORQA-ODEL-DARFS.                                              
016700   03    WS-ODEL-SEKEL           PIC 9(02).                               
016800   03    WS-ODEL-YYMMDD          PIC 9(06).                               
016900   03    WS-ODEL-HHMM            PIC 9(04).                               
017000                                                                          
017100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
017200 01      FILLER                  PIC X(10)      VALUE 'ARBETSF'.          
017300*                                                                         
017400*      --- VALID IDDC CODES                                               
017500*                                                                         
017600*01    -COPY WWDC99                                                       
017700       EJECT                                                              
017800 01      WS-IDPRC.                                                        
017900   03    WS-IDPRCBAS             PIC X(03).                               
018000   03    WS-IDPRCVAR             PIC X(01).                               
018100                                                                          
018200 01      WS-IDTRP.                                                        
018300   03    WS-IDTRPLOS             PIC X(03).                               
018400   03    WS-IDTRPVAR             PIC X(02).                               
018500                                                                          
018600 01      WS-IDUSER               PIC X(08).                               
018800 01      WS-IDBORD               PIC X(03).                               
018900                                                                          
019000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
019100     88  INDATA-OK                           VALUE 'J'.                   
019200     88  INDATA-FEL                          VALUE 'N'.                   
019300                                                                          
019400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
019500     88  NYCKLAR-OK                          VALUE 'J'.                   
019600     88  NYCKLAR-FEL                         VALUE 'N'.                   
019700                                                                          
019800 77  PRC-FEL-SW                  PIC X       VALUE 'J'.                   
019900     88  PRC-FEL                             VALUE 'N'.                   
020000                                                                          
020100 77  KANAL-FEL-SW                PIC X       VALUE 'J'.                   
020200     88  KANAL-FEL                           VALUE 'N'.                   
020300                                                                          
020400 77  BORD-FEL-SW                 PIC X       VALUE 'J'.                   
020500     88  BORD-FEL                            VALUE 'N'.                   
020600                                                                          
020700 77  USER-FEL-SW                 PIC X       VALUE 'J'.                   
020800     88  USER-OK                             VALUE 'J'.                   
020900     88  USER-FEL                            VALUE 'N'.                   
021000                                                                          
021100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
021200     88  ALLT-OK                             VALUE 'J'.                   
021300                                                                          
021400 77  LAES-SW                     PIC X.                                   
021500     88  LAES-OK                             VALUE 'J'.                   
021600                                                                          
021700 77  LAES-WAY                    PIC X.                                   
021800     88  PRC                                 VALUE '1'.                   
021900     88  TRANSPORT                           VALUE '2'.                   
022000                                                                          
022100 77  PLOCK-SW                    PIC X.                                   
022200     88  PLOCKSATS-FINNS                     VALUE 'J'.                   
022300     88  PLOCKSATS-SAKNAS                    VALUE 'N'.                   
022400                                                                          
022500 77  PLOCKSATS-SW                PIC X.                                   
022600     88  PLOCKSATS-OK                        VALUE 'J'.                   
022700     88  PLOCKSATS-FEL                       VALUE 'N'.                   
022800                                                                          
022900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
023000     88  EGEN-MID                            VALUE '4352'.                
023100     88  GODK-MID                            VALUE '4352'.                
023200                                                                          
023300 01  WS-IDPRTLST.                                                         
023400     03 WS-SYSTDEL               PIC X(1).                                
023500     03 WS-LISTTYP               PIC X(2).                                
023600     03 WS-KDPRT                 PIC X(3).                                
023700     03 FILLER                   PIC X(2)    VALUE SPACE.                 
023800     EJECT                                                                
023900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
024000 01  GENERELLA-SUBPROGRAM.                                                
024100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
024200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
024300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
024400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
024500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
024600     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
024700     EJECT                                                                
024800 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND'.             
024900*    --- PARAMETRAR WWOMVAND                                              
025000*01 -COPY WWOMVAND                                                        
025100     EJECT                                                                
025200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
025300*01 -COPY WMSGINIT                                                        
025400     EJECT                                                                
025500*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
025600                                                                          
025700*   -COPY W006PRT                                                         
025800     EJECT                                                                
025900*                                                                         
026000 01  MEDDELANDE.                                                          
026100   03  FEL1.                                                              
026200     05 FILLER                   PIC X(40)                                
026300          VALUE 'BORDSNUMMER SAKNAS '.                                    
026400     05 FILLER                   PIC X(40)                                
026500          VALUE 'TABLENUMBER MISSING '.                                   
026600   03  FILLER REDEFINES FEL1.                                             
026700     05  FEL-1                   PIC X(40)   OCCURS 2.                    
026800   03  FEL2.                                                              
026900     05 FILLER                   PIC X(40)                                
027000          VALUE 'OTILLÅTEN PRC      '.                                    
027100     05 FILLER                   PIC X(40)                                
027200          VALUE 'PRC NOT ALLOWED     '.                                   
027300   03  FILLER REDEFINES FEL2.                                             
027400     05  FEL-2                   PIC X(40)   OCCURS 2.                    
027500                                                                          
027600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
027700*   -COPY WMEDAREA                                                        
027800     EJECT                                                                
027900 01  FILLER                      PIC X(16)   VALUE 'WORKAREA'.            
028000*01 -COPY WORKAREA                                                        
028100     EJECT                                                                
028200 01    FILLER                    PIC X(17)                                
028300                                 VALUE 'PARAMETER FOR S07'.               
028400 77  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
028500 77  TEST-IDKUNDNR               PIC 9(7)    COMP-3.                      
028600 77  WS-TEST-IDDISTR             PIC 9(5).                                
028700 77  WS-TEST-IDKUNDNR            PIC 9(7).                                
028800 77  TEST-IDDISTR-PREV           PIC 9(5)    COMP-3.                      
028900 77  TEST-IDKUNDNR-PREV          PIC 9(7)    COMP-3.                      
029000                                                                          
029100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
029200*                                                                         
029300 01  FILLER                      PIC X(16)  VALUE 'SAVE-AREA  '.          
029400 01  SAVE-AREA.                                                           
029500     03 SAVE-IDTRANS               PIC X(4)   VALUE SPACE.                
029600     03 PGNO                       PIC 9(2)   VALUE 01.                   
029601     03 FIRST-SW                   PIC X      VALUE 'J'.                  
029602     03 LAST-SW                    PIC X      VALUE 'N'.                  
029603     03 EXTEND-SW                  PIC X      VALUE 'N'.                  
029610     03  SPAR-AREA.                                                       
029620         05 SPAR-IDDISTR-X.                                               
029630            07 SPAR-IDDISTR        PIC S9(5)  COMP-3.                     
029640         05 SPAR-IDKUNDNR-X.                                              
029650            07 SPAR-IDKUNDNR       PIC S9(7)  COMP-3.                     
029700     03 SAVE-AREA-ENTER OCCURS 20.                                        
029800        05 SAVE-ODEL-IDPRODNR      PIC S9(7)  COMP-3 VALUE ZERO.          
029900        05 SAVE-ODEL-IDPLKLST      PIC S9(3)  COMP-3 VALUE ZERO.          
030000        05 SAVE-KVORDER-ENTER      PIC S9(7)  COMP-3 VALUE ZERO.          
030100        05 SAVE-KVRADER-ENTER      PIC S9(5)  COMP-3 VALUE ZERO.          
030200        05 SAVE-IDPLKLST-ENTER     PIC S9(3)  COMP-3 VALUE ZERO.          
030600        05 SAVE-TIAAMMDD-ENTER     PIC S9(6)  COMP-3 VALUE ZERO.          
030700        05 SAVE-TIHHMM-ENTER       PIC S9(5)  COMP-3 VALUE ZERO.          
030800        05 SAVE-TIRFS-ENTER        PIC S9(10) COMP-3 VALUE ZERO.          
030900        05 SAVE-TILST-O-ENTER      PIC S9(10) COMP-3 VALUE ZERO.          
031000        05 SAVE-TIUTSKR-ENTER      PIC S9(6)  COMP-3 VALUE ZERO.          
031100        05 SAVE-TIUTSTID-ENTER     PIC S9(6)  COMP-3 VALUE ZERO.          
031110        05 SAVE-IDTRP-ENTER.                                              
031120           07 SAVE-IDTRPLOS        PIC X(3)  VALUE SPACE.                 
031130           07 SAVE-IDTRPVAR        PIC X(2)  VALUE SPACE.                 
031800*                                                                         
031900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
032000*                                                                         
032100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
032200     SKIP3                                                                
032300*01  MID -COPY W4I35201                                                   
032400     EJECT                                                                
032500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
032600     SKIP3                                                                
032700*01  -COPY WMSGAREA                                                       
032800     EJECT                                                                
032900*    03  MOD -COPY W4O35201   -RED MSG-AREA.                              
033000     EJECT                                                                
033100 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
033200 01  P-TO-P-SW.                                                           
033300     03  PTOP-LL                 PIC S9(4)   VALUE 28 COMP SYNC.          
033400     03  PTOP-Z1                 PIC  X(1)   VALUE LOW-VALUE.             
033500     03  PTOP-Z2                 PIC  X(1)   VALUE LOW-VALUE.             
033600     03  PTOP-TRANSKOD           PIC  X(7)   VALUE 'W4T375U'.             
033700     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
033800     03  FILLER                  PIC  X(4)   VALUE '4352'.                
033900     03  PTOP-KDMFSFOR           PIC  X(1).                               
034000     03  -COPY W4I37501  -PRE PTOP-                                       
034100                                                                          
034200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
034300     SKIP3                                                                
034400*01  -COPY WMFSAREA                                                       
034500     EJECT                                                                
034600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
034700*                                                                         
034800 01  FILLER                      PIC X(16)   VALUE 'DLI-  '.              
034900     SKIP3                                                                
035000 01  NYCKLAR-TILL-DLI.                                                    
035100                                                                          
035200*----> DIREKTNYCKEL TILL ORDERHUVUD.                                      
035300                                                                          
035400     03  W-IDORDER-X.                                                     
035500         05  W-IDORDER               PIC S9(7)  COMP-3.                   
035600                                                                          
035700*----> DIREKTNYCKEL TILL ORDERDEL.                                        
035800                                                                          
035900     03  W-WDQ301KY-X.                                                    
036000         05  W-Q301KY-IDORDER        PIC S9(7)  COMP-3.                   
036100         05  W-Q301KY-IDDC           PIC X(2).                            
036200         05  W-Q301KY-IDPRODNR       PIC S9(7)  COMP-3.                   
036300         05  W-Q301KY-IDPLKLST       PIC S9(3)  COMP-3.                   
036400                                                                          
036500*----> SÖKFÄLT TILL ORDERDEL.                                             
036600                                                                          
036700     03  W-IDPRODNR-SOEK-X.                                               
036800         05  W-IDPRODNR-SOEK         PIC S9(7)  COMP-3.                   
036900     03  W-IDPLKLST-SOEK-X.                                               
037000         05  W-IDPLKLST-SOEK         PIC S9(3)  COMP-3.                   
037100                                                                          
037200*----> SEKUNDÄR INDEX TRANSPORT-ID TILL ORDERDEL.                         
037300                                                                          
037400     03  W-WDQ3A1-MIN-X.                                                  
037500         05  W-Q3A1-MIN-IDDC         PIC X(2).                            
037600         05  W-Q3A1-MIN-IDTRP        PIC X(5).                            
037700         05  W-Q3A1-MIN-DATRPAVT.                                         
037800          07 W-Q3A1-MIN-DATRPAVD     PIC 9(8).                            
037900          07 W-Q3A1-MIN-TIHHMM       PIC S9(5)  COMP-3.                   
038000         05  W-Q3A1-MIN-DARFS        PIC 9(12).                           
038100         05  W-Q3A1-MIN-DALSTORD     PIC 9(12).                           
038200         05  W-Q3A1-MIN-IDPRC.                                            
038300          07 W-Q3A1-MIN-IDPRCBAS     PIC X(3).                            
038400          07 W-Q3A1-MIN-IDPRCVAR     PIC X(1).                            
038500         05  FILLER                  PIC X(10).                           
038600                                                                          
038700     03  W-WDQ3A1-MAX-X.                                                  
038800         05  W-Q3A1-MAX-IDDC         PIC X(2).                            
038900         05  W-Q3A1-MAX-IDTRP        PIC X(5).                            
039000         05  W-Q3A1-MAX-DATRPAVT.                                         
039100          07 W-Q3A1-MAX-DATRPAVD     PIC 9(8).                            
039200          07 W-Q3A1-MAX-TIHHMM       PIC S9(5)  COMP-3.                   
039300         05  W-Q3A1-MAX-DARFS        PIC 9(12).                           
039400         05  W-Q3A1-MAX-DALSTORD     PIC 9(12).                           
039500         05  W-Q3A1-MAX-IDPRC.                                            
039600          07 W-Q3A1-MAX-IDPRCBAS     PIC X(3).                            
039700          07 W-Q3A1-MAX-IDPRCVAR     PIC X(1).                            
039800         05  FILLER                  PIC X(10).                           
039900                                                                          
040000*----> SEKUNDÄR INDEX PRC-KANAL TILL ORDERDEL.                            
040100                                                                          
040200     03  W-WDQ3B1-MIN-X.                                                  
040300         05  W-Q3B1-MIN-IDDC         PIC X(2).                            
040400         05  W-Q3B1-MIN-IDPRCBAS     PIC X(3).                            
040500         05  W-Q3B1-MIN-DAUTSKR      PIC 9(8).                            
040600         05  W-Q3B1-MIN-TIUTSTID     PIC S9(7)  COMP-3.                   
040700         05  W-Q3B1-MIN-DARFS        PIC 9(12).                           
040800         05  W-Q3B1-MIN-DALSTORD     PIC 9(12).                           
040900         05  FILLER                  PIC X.                               
041000         05  FILLER                  PIC X(10).                           
041100                                                                          
041200     03      W-Q3B1-MIN-IDPRCVAR     PIC X(1).                            
041300                                                                          
041400     03  W-WDQ3B1-MAX-X.                                                  
041500         05  W-Q3B1-MAX-IDDC         PIC X(2).                            
041600         05  W-Q3B1-MAX-IDPRCBAS     PIC X(3).                            
041700         05  W-Q3B1-MAX-DAUTSKR      PIC 9(8).                            
041800         05  W-Q3B1-MAX-TIUTSTID     PIC S9(7)  COMP-3.                   
041900         05  W-Q3B1-MAX-DARFS        PIC 9(12).                           
042000         05  W-Q3B1-MAX-DALSTORD     PIC 9(12).                           
042100         05  FILLER                  PIC X.                               
042200         05  FILLER                  PIC X(10).                           
042300                                                                          
042400     03      W-Q3B1-MAX-IDPRCVAR     PIC X(1).                            
042500                                                                          
042600*----> SEKUNDÄR INDEX PRODNUMMER, PLOCKLISTNR  ORDERDEL.                  
042700                                                                          
042800     03  W-WDQ3DSEQ-X.                                                    
042900         05  W-Q3DSEQ-IDPRODNR       PIC S9(7)  COMP-3.                   
043000         05  W-Q3DSEQ-IDPLKLST       PIC S9(3)  COMP-3.                   
043100                                                                          
043200*----> PRC-KANALEN.                                                       
043300                                                                          
043400     03  W-4447-IDHTYP-X.                                                 
043500         05  W-4447-IDHTYP       PIC  X(04) VALUE '4447'.                 
043600         05  W-4447-IDDC         PIC  X(02).                              
043700         05  W-4447-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
043800                                                                          
043900     03  W-4448-IDPRC-X.                                                  
044000         05  W-4448-IDPRC        PIC  X(04).                              
044100         05  W-4448-LOW-VALUE    PIC  X(01) VALUE LOW-VALUE.              
044200                                                                          
044300*----> PLOCKSATSENS LÖPNR INOM PRCGRUPP.                                  
044400                                                                          
044500     03  W-4461-IDHTYP-X.                                                 
044600         05  W-4461-IDHTYP       PIC  X(04) VALUE '4461'.                 
044700         05  W-4461-IDDC         PIC  X(02).                              
044800         05  W-4461-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
044900                                                                          
045000     03  W-4462-KDPRCGRP-X       PIC  X(05).                              
045100                                                                          
045200*----> PLOCKSATS.                                                         
045300                                                                          
045400     03  W-4001-IDHTYP-X.                                                 
045500         05  W-4001-IDHTYP       PIC  X(04) VALUE '4001'.                 
045600         05  W-4001-IDPRODNR     PIC  9(07).                              
045700         05  W-4001-IDPLKLST     PIC  9(03).                              
045800         05  W-LOW-VALUE         PIC  X(16) VALUE LOW-VALUE.              
045900                                                                          
046000*----> PLOCKSATS/PLOCKARE                                                 
046100                                                                          
046200     03  W-4011-IDHTYP-X.                                                 
046300         05  W-4011-IDHTYP       PIC  X(04) VALUE '4011'.                 
046400         05  W-4011-IDDC         PIC  X(02).                              
046500         05  W-4011-IDUSER       PIC  X(08).                              
046600         05  W-LOW-VALUE         PIC  X(16) VALUE LOW-VALUE.              
046700     03  W-IDGMT-X.                                                       
046800         05  W-WDB2-IDDISTR      PIC S9(5)   VALUE ZERO COMP-3.           
046900         05  W-WDB2-IDKUNDNR     PIC S9(7)   VALUE ZERO COMP-3.           
047000                                                                          
047100     03  W-IDDC-B6-X.                                                     
047200         05 W-IDDC-B6                  PIC X(2).                          
047300*                                                                         
047400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
047500*    --- STATUS-KOD FRÅN IMS                                              
047600 01  WDB2-STATUS-WS              PIC XX.                                  
047700     88  WDB2-SEGMENT-FINNS                  VALUE '  '.                  
047800 01  STATUS-WS                   PIC XX.                                  
047900     88  SEGMENT-FINNS                       VALUE '  '.                  
048000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
048100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
048200     88  END-OF-DATA                         VALUE 'GB'.                  
048300     SKIP2                                                                
048400 01  GODK-STATUSKODER.                                                    
048500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
048600     SKIP3                                                                
048700 01  SSA1                        PIC X(400).                              
048800 01  SSA2                        PIC X(64).                               
048900     EJECT                                                                
049000*    --- IMS FUNKTIONSKODER                                               
049100*01  -COPY W0003                                                          
049200     EJECT                                                                
049300*    ---  DLI INPUT-OUTPUT AREA                                           
049400 01  FILLER                      PIC X(16)   VALUE 'IO-AREA1'.            
049500                                                                          
049600 01  DLI-IO-AREA1.                                                        
049700     03  IO-AREA1                PIC X(600)  VALUE SPACE.                 
049800                                                                          
049900     03  WLXXKH11 REDEFINES IO-AREA1.                                     
050000*        05  -COPY WDGX4448   -PRE XXKH-                                  
050100     EJECT                                                                
050200     03  WLXXKO01 REDEFINES IO-AREA1.                                     
050300*        05  -COPY WDGX4461   -PRE XXKO-                                  
050400     EJECT                                                                
050500     03  WLXXKO11 REDEFINES IO-AREA1.                                     
050600*        05  -COPY WDGX4462   -PRE XXKO-                                  
050700     EJECT                                                                
050800     03  WL400101 REDEFINES IO-AREA1.                                     
050900*        05  -COPY WDGX4001   -PRE 4001-                                  
051000     EJECT                                                                
051100     03  WL401101 REDEFINES IO-AREA1.                                     
051200*        05  -COPY WDGX4011   -PRE 4011-                                  
051300     EJECT                                                                
051400 01  FILLER                      PIC X(16)   VALUE 'IO-AREA2'.            
051500                                                                          
051600 01  DLI-IO-AREA2.                                                        
051700     03  WLORQA01.                                                        
051800*        05  -COPY WDQ301     -PRE ORQA-                                  
051900     EJECT                                                                
052000 01  FILLER                      PIC X(16)   VALUE 'IO-AREA3'.            
052100                                                                          
052200 01  DLI-IO-AREA3.                                                        
052300     03  WL400111.                                                        
052400*        05  -COPY WDGX4002                                               
052500     EJECT                                                                
052600 01  FILLER                      PIC X(16)   VALUE 'IO-AREA4'.            
052700                                                                          
052800 01  DLI-IO-AREA4.                                                        
052900     03  WL401111.                                                        
053000*        05  -COPY WDGX4012                                               
053100     EJECT                                                                
053200 01  FILLER                      PIC X(16)   VALUE 'IO-AREA5'.            
053300                                                                          
053400 01  DLI-IO-AREA5.                                                        
053500     03  WLORQI01.                                                        
053600*        05  -COPY WDQ201     -PRE ORQI-                                  
053700     EJECT                                                                
053800 01  FILLER                      PIC X(16)   VALUE                        
053900                                                'IO AREA - WDB2'.         
054000 01  DLI-IO-WDB2.                                                         
054100     03  DLI-IO-WDB201.                                                   
054200*        05  -COPY WDB201                                                 
054300     EJECT                                                                
054400 01  FILLER                      PIC X(16)   VALUE                        
054500                                                'IO AREA - Q3A1'.         
054600 01  DLI-IO-Q3A1.                                                         
054700     03  DLI-IO-WDQ3A1.                                                   
054800*        05  -COPY WDQ3A1   -PRE Q3A1-                                    
054900     EJECT                                                                
055000 01  FILLER                      PIC X(16)   VALUE                        
055100                                                'IO AREA - Q3B1'.         
055200 01  DLI-IO-Q3B1.                                                         
055300     03  DLI-IO-WDQ3B1.                                                   
055400*        05  -COPY WDQ3B1   -PRE Q3B1-                                    
055500                                                                          
055600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
055700 01   DLI-IO-AREA-B601.                                                   
055800*     03  -COPY WDB601                                                    
055900     EJECT                                                                
056000 LINKAGE SECTION.                                                         
056100                                                                          
056200*01  -COPY W0009      -PRE MSG-                                           
056300     EJECT                                                                
056400*01  -COPY W0009      -PRE ALT-                                           
056500     EJECT                                                                
056600*01  -COPY W0008      -PRE USEA-                                          
056700     05  FILLER                  PIC X.                                   
056800     EJECT                                                                
056900*01  -COPY W0008      -PRE ORQ-                                           
057000     05  FILLER                  PIC X.                                   
057100     EJECT                                                                
057200*01  -COPY W0008      -PRE Q3A1-                                          
057300     05  FILLER                  PIC X.                                   
057400     EJECT                                                                
057500*01  -COPY W0008      -PRE Q3B1-                                          
057600     05  FILLER                  PIC X.                                   
057700     EJECT                                                                
057800*01  -COPY W0008      -PRE ORQD-                                          
057900     05  FILLER                  PIC X.                                   
058000     EJECT                                                                
058100*01  -COPY W0008      -PRE XXKH-                                          
058200     05  FILLER                  PIC X.                                   
058300     EJECT                                                                
058400*01  -COPY W0008      -PRE 4001-                                          
058500     05  FILLER                  PIC X.                                   
058600     EJECT                                                                
058700*01  -COPY W0008      -PRE XXKO-                                          
058800     05  FILLER                  PIC X.                                   
058900     EJECT                                                                
059000*01  -COPY W0008      -PRE ORQI-                                          
059100     05  FILLER                  PIC X.                                   
059200     EJECT                                                                
059300*01  -COPY W0008      -PRE 4011-                                          
059400     05  FILLER                  PIC X.                                   
059500     EJECT                                                                
059600*01  -COPY W0008      -PRE WDB2-                                          
059700     05  FILLER                  PIC X.                                   
059800     EJECT                                                                
059900*01  -COPY W0008      -PRE WDB6-                                          
060000     05  FILLER                  PIC X.                                   
060100     EJECT                                                                
060200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB ORQ-PCB               
060300                           Q3A1-PCB Q3B1-PCB ORQD-PCB XXKH-PCB            
060800                           4001-PCB XXKO-PCB ORQI-PCB 4011-PCB            
061400                           WDB2-PCB WDB6-PCB.                             
061600                                                                          
061700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB ORQ-PCB               
062700                           Q3A1-PCB Q3B1-PCB ORQD-PCB XXKH-PCB            
062800                           4001-PCB XXKO-PCB ORQI-PCB 4011-PCB            
062900                           WDB2-PCB WDB6-PCB.                             
063100                                                                          
063200     PERFORM IMS-GU-MSG                                                   
063300     IF SEGMENT-FINNS                                                     
063400        PERFORM A-INIT                                                    
063500                                                                          
063600        PERFORM B-KOLLA-NYCKLAR                                           
063700        IF NYCKLAR-OK                                                     
063800           IF MFS-UPDATE                                                  
063900              PERFORM F-LAES-PLOCKSATS-PLOCKARE                           
064000              PERFORM H-UPPDATERA                                         
064100              IF INDATA-OK                                                
064200                 IF MID-FLKLAR = NEJ                                      
064300                    PERFORM E-SAMMA-SIDA                                  
064400                 ELSE                                                     
064500                    PERFORM C-FOERSTA-SIDA                                
064600                 END-IF                                                   
064700              END-IF                                                      
064800           ELSE                                                           
064900              IF MFS-FIRST                                                
065000                 PERFORM C-FOERSTA-SIDA                                   
065100              ELSE                                                        
065200                 IF MFS-NEXT                                              
065300                    PERFORM D-NAESTA-SIDA                                 
065400                 ELSE                                                     
065500                    IF MFS-PREVIOUS                                       
065700                       PERFORM I-PREV-SIDA                                
065800                    ELSE                                                  
065900                       IF MFS-SPLIT                                       
066000                         PERFORM G-EXPANDERA-SIDA                         
066100                       ELSE                                               
066200                         PERFORM E-SAMMA-SIDA                             
066300                       END-IF                                             
066400                    END-IF                                                
066500                 END-IF                                                   
066600              END-IF                                                      
066700           END-IF                                                         
066800        END-IF                                                            
066900        PERFORM S10-SAVE-KEYS                                             
068100     END-IF                                                               
068200                                                                          
068300     MOVE ZERO TO RETURN-CODE                                             
068400     GOBACK                                                               
068500     .                                                                    
068600     EJECT                                                                
068700 A-INIT SECTION.                                                          
068800                                                                          
068900     IF MSG-DUBBLA-TRANSKODER                                             
069000        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I35201                
069100        MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                 
069200        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
069300     ELSE                                                                 
069400        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I35201                 
069500        MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                 
069600        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
069700     END-IF                                                               
069800                                                                          
069900     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
070000     MOVE MSG-IDPFK TO MFS-IDPFK                                          
070100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
070200                                                                          
070300     MOVE LOW-VALUE TO MSG-AREA                                           
070400     MOVE 'W4O352N1' TO MFS-IDMOD                                         
070500     MOVE '4352' TO MOD-IDTRANS                                           
070600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
070700     MOVE SPACE TO MED-IDMFSFEL                                           
070800                                                                          
070900     IF NOT EGEN-MID                                                      
071000        MOVE SPACE TO MFS-KDTRTYP                                         
071100        MOVE '7' TO MFS-IDPFK                                             
071200     END-IF                                                               
071300                                                                          
071400     MOVE NEJ          TO PTOP-MID-FLSVAR                                 
071500     MOVE MFS-KDMFSFOR TO PTOP-KDMFSFOR                                   
071600                                                                          
071700     ACCEPT WS-DATUM   FROM DATE                                          
071800     ACCEPT WS-KLOCKAN FROM TIME                                          
071900     PERFORM AA-FLYTTA-DOLDA-FAELT                                        
072000     MOVE NEJ       TO MOD-FLKLAR                                         
072100     MOVE ZERO                  TO TEST-IDKUNDNR-PREV                     
072200                                   TEST-IDDISTR-PREV                      
072300                                   TEST-IDKUNDNR                          
072400                                   TEST-IDDISTR                           
072500                                                                          
072600     MOVE +1 TO EXP-IX                                                    
072700     PERFORM UNTIL EXP-IX > 12                                            
072800        MOVE SPACE TO EXP-IDDISTR(EXP-IX)                                 
072900        MOVE SPACE TO EXP-IDKUNDNR(EXP-IX)                                
073000        MOVE ZERO  TO EXP-IDDISTR-NUM(EXP-IX)                             
073100        MOVE ZERO  TO EXP-IDKUNDNR-NUM(EXP-IX)                            
073200        MOVE SPACE TO EXP-FLER(EXP-IX)                                    
073300        ADD +1 TO EXP-IX                                                  
073400     END-PERFORM                                                          
073500     .                                                                    
073600     EJECT                                                                
073700 AA-FLYTTA-DOLDA-FAELT SECTION.                                           
073800                                                                          
075900     IF MID-KVORDER-NEXT NUMERIC                                          
076000        MOVE MID-KVORDER-NEXT  TO WS-KVORDER                              
076100     ELSE                                                                 
076200        MOVE 0                 TO WS-KVORDER                              
076300     END-IF                                                               
076400                                                                          
076500     IF MID-KVRADER-NEXT NUMERIC                                          
076600        MOVE MID-KVRADER-NEXT  TO WS-KVRADER                              
076700     ELSE                                                                 
076800        MOVE 0                 TO WS-KVRADER                              
076900     END-IF                                                               
077000                                                                          
077100     IF MID-IDPLKLST-NEXT NUMERIC                                         
077200        AND                                                               
077300        MID-IDPLKLST-NEXT > 0                                             
077400        MOVE MID-IDPLKLST-NEXT  TO WS-ANTPLK                              
079515     END-IF                                                               
079516                                                                          
079600     MOVE MID-IDPRC-SPAR  TO W-4448-IDPRC                                 
079700     .                                                                    
079800     EJECT                                                                
079900 B-KOLLA-NYCKLAR SECTION.                                                 
080000                                                                          
080100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
080200     MOVE '001'             TO MSGI-KDCALL                                
080300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
080400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
080500     MOVE '4352'            TO MSGI-IDTRANS                               
080600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
080700     MOVE MSGI-IDTIDZON     TO WS-IDTIDZON                                
080800     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
080900     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
081000     IF SAVE-IDTRANS NOT = '4352'                                         
081100        INITIALIZE SAVE-AREA                                              
081110        MOVE 01   TO PGNO                                                 
081200     END-IF                                                               
081300                                                                          
081400     IF MSGI-IDLAND-SPR = 'GB'                                            
081500        MOVE +2 TO SPRAK-IX                                               
081600        MOVE 'GB ' TO MED-IDSKYLT                                         
081700     ELSE                                                                 
081800        MOVE +1 TO SPRAK-IX                                               
081900        MOVE 'S  ' TO MED-IDSKYLT                                         
082000     END-IF                                                               
082100                                                                          
082200     MOVE JA TO NYCKLAR-SW                                                
082300                                                                          
082400     MOVE LOW-VALUE  TO W-WDQ3B1-MIN-X                                    
082500                        W-Q3B1-MIN-IDPRCVAR                               
082600                        W-WDQ3A1-MIN-X                                    
082700                        W-Q3A1-MIN-IDPRC                                  
082800                                                                          
082900     MOVE HIGH-VALUE TO W-WDQ3B1-MAX-X                                    
083000                        W-Q3B1-MAX-IDPRCVAR                               
083100                        W-WDQ3A1-MAX-X                                    
083200                        W-Q3A1-MAX-IDPRC                                  
083300                                                                          
083400     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
083500                             MOD-IDPRC-IN                                 
083600                             MOD-IDTRP-IN                                 
083700                             MOD-IDUSER-IN                                
083800                             MOD-IDBORD-IN                                
083900                                                                          
084000     PERFORM BD-FLYTTA-IDDC                                               
084100                                                                          
084200     MOVE '011'                TO MSGI-KDCALL                             
084300     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
084400     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
084500     MOVE WS-DATUM             TO MSGI-TILOKDAT                           
084600     MOVE WS-TIHHMMSS(1:4)     TO MSGI-TILOKTID                           
084700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
084800     MOVE MSGI-TILOKDAT        TO WS-DATUM-LOK                            
084900     MOVE WS-TIHHMMSS          TO WS-TIHHMMSS-LOK                         
085000     MOVE MSGI-TILOKTID        TO WS-TIHHMMSS-LOK(1:4)                    
085100                                                                          
085200     MOVE WS-TIHHMM-LOK TO WS-TI-HHMM                                     
085300     MOVE WS-DATUM-LOK  TO WS-TI-DATUM                                    
085400     IF WS-DATUM-LOK < 500000                                             
085500       MOVE 20          TO WS-TI-SEKEL                                    
085600     ELSE                                                                 
085700       MOVE 19          TO WS-TI-SEKEL                                    
085800     END-IF                                                               
085900     MOVE WS-DALST      TO WS-DALSTORD                                    
086000                                                                          
086100     PERFORM BA-KOLLA-PRC                                                 
086200     PERFORM BB-KOLLA-TRANSPORT                                           
086300     PERFORM BC-KOLLA-USER                                                
086400                                                                          
086500     IF MID-IDBORD-IN = ALL '+'                                           
086600        MOVE MID-IDBORD-UT TO WS-IDBORD                                   
086700     ELSE                                                                 
086800        MOVE MID-IDBORD-IN TO WS-IDBORD                                   
086900     END-IF                                                               
087000                                                                          
087100     IF NOT GODK-MID                                                      
087200        MOVE NEJ          TO NYCKLAR-SW                                   
087300     END-IF                                                               
087400                                                                          
087500     MOVE WS-IDDC         TO MOD-IDDC-UT                                  
087600                                                                          
087700     IF GODK-MID OR NYCKLAR-OK                                            
087800        MOVE WS-IDPRC   TO MOD-IDPRC-UT                                   
087900        MOVE WS-IDTRP   TO MOD-IDTRP-UT                                   
088000        MOVE WS-IDUSER  TO MOD-IDUSER-UT                                  
088100        MOVE WS-IDBORD  TO MOD-IDBORD-UT                                  
088200        INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE               
088300        INSPECT MOD-IDPRC-UT REPLACING LEADING ZERO BY SPACE              
088400        INSPECT MOD-IDTRP-UT REPLACING LEADING ZERO BY SPACE              
088500        INSPECT MOD-IDUSER-UT REPLACING LEADING ZERO BY SPACE             
088600        IF MFS-UPDATE                                                     
088700          IF CDC-SE AND                                                   
088800              (WS-IDBORD NOT > SPACE OR WS-IDBORD = ALL '+')              
088900             MOVE NEJ          TO NYCKLAR-SW                              
089000                                  BORD-FEL-SW                             
089100             MOVE MFS-RENSA-FAELT TO MOD-IDBORD-UT                        
089200             PERFORM MFS-ROR-EJ-FAELT-IN                                  
089300             PERFORM MFS-ROR-EJ-FAELT-UT                                  
089400             PERFORM MFS-LAS-IN-IGEN                                      
089500          ELSE                                                            
089600             MOVE WS-IDBORD  TO MOD-IDBORD-UT                             
089700          END-IF                                                          
089800        END-IF                                                            
089900     ELSE                                                                 
090000        MOVE MFS-RENSA-FAELT TO MOD-IDPRC-UT                              
090100                                MOD-IDTRP-UT                              
090200                                MOD-IDUSER-UT                             
090300                                MOD-IDBORD-UT                             
090400     END-IF                                                               
090500                                                                          
090600     IF NYCKLAR-FEL                                                       
090700        IF GODK-MID                                                       
090800          IF PRC-FEL                                                      
090900            IF KANAL-FEL                                                  
091000             MOVE FEL-2 (SPRAK-IX)   TO MOD-TEMFSFEL                      
091100            ELSE                                                          
091200*324  PRC9998 FÅR INTE SKRIVASUT MANUELLT                                 
091300             MOVE '324' TO MED-IDMFSFEL                                   
091400             PERFORM S05-FEL-MEDDELANDE                                   
091500            END-IF                                                        
091600          ELSE                                                            
091700*401 FEL NYCKEL                                                           
091800             MOVE '401' TO MED-IDMFSFEL                                   
091900             PERFORM S05-FEL-MEDDELANDE                                   
092000          END-IF                                                          
092100          IF MFS-UPDATE                                                   
092200             IF BORD-FEL AND USER-OK                                      
092300*  BORDSNUMMER SAKNAS                                                     
092400                MOVE FEL-1 (SPRAK-IX)   TO MOD-TEMFSFEL                   
092500             END-IF                                                       
092600          END-IF                                                          
092700          IF NOT MFS-UPDATE                                               
092800             PERFORM MFS-RENSA-FAELT-IN                                   
092900             PERFORM MFS-RENSA-FAELT-UT                                   
093000          END-IF                                                          
093100        END-IF                                                            
093200     ELSE                                                                 
093300*    NYCKLAR OK                                                           
093400*    KOLLA OM NYCKLAR ÄNDRATS,                                            
093500*    I SÅ FALL BLANKAS SPAR-AREA SÅ INTE EXPANDERAD BILD VISAS            
093600                                                                          
093700        IF NOT MID-IDPRC-IN  = ALL '+' OR                                 
093800           NOT MID-IDUSER-IN = ALL '+' OR                                 
093900           NOT MID-IDBORD-IN = ALL '+' OR                                 
094000           NOT MID-IDTRP-IN  = ALL '+'                                    
094100                                                                          
094200           MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                         
094300           MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                   
094310           MOVE NEJ                TO EXTEND-SW                           
094400           MOVE SPACE              TO SPAR-AREA                           
094410           MOVE '4352'             TO MSGI-IDTRANS                        
094420           MOVE '4352'             TO SAVE-IDTRANS                        
094500           MOVE '002'              TO MSGI-KDCALL                         
094600           MOVE SAVE-AREA          TO MSGI-SPAR-AREA                      
094700           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
094800        END-IF                                                            
094900     END-IF                                                               
095000     .                                                                    
095100     EJECT                                                                
095200 BD-FLYTTA-IDDC       SECTION.                                            
095300                                                                          
095400     MOVE MSGI-IDDC                 TO WS-IDDC                            
095500                                                                          
095600     IF WS-IDDC > SPACE                                                   
095700       MOVE WS-IDDC                 TO W-Q3A1-MIN-IDDC                    
095800                                       W-Q3A1-MAX-IDDC                    
095900                                       W-Q3B1-MIN-IDDC                    
096000                                       W-Q3B1-MAX-IDDC                    
096100                                                                          
096200     ELSE                                                                 
096300       MOVE NEJ                     TO NYCKLAR-SW                         
096400     END-IF                                                               
096500     .                                                                    
096600     EJECT                                                                
096700 BA-KOLLA-PRC SECTION.                                                    
096800                                                                          
096900     IF MID-IDPRC-IN = ALL '+'                                            
097000        MOVE MID-IDPRC-UT TO WS-IDPRC                                     
097100     ELSE                                                                 
097200        MOVE MID-IDPRC-IN TO WS-IDPRC                                     
097300        MOVE '7'          TO MFS-IDPFK                                    
097400        MOVE SPACE        TO MFS-KDTRTYP                                  
097500     END-IF                                                               
097600                                                                          
097700     INSPECT WS-IDPRC REPLACING LEADING SPACE BY ZERO                     
097800                                                                          
097900     IF MSGI-IDUSER = 'PCSSE01'                                           
098000**PCSSE01 FÅR BARA SKRIVA UT ORDER FRÅN PRC 420S/                         
098100**PCSSE01 FÅR BARA SKRIVA UT ORDER FRÅN PRC 4200/10/20/30/35              
098200**PCSSE01 FÅR BARA SKRIVA UT ORDER FRÅN PRC 4300/10/15/20/30/35           
098300       IF WS-IDPRC = '420S' OR '4200' OR '4210' OR '4220' OR              
098400                     '4230' OR '4235' OR '4299' OR                        
098500                     '4300' OR '4310' OR '4315' OR '4320' OR              
098600                     '4330' OR '4335' OR '4399'                           
098700         CONTINUE                                                         
098800       ELSE                                                               
098900         MOVE NEJ           TO NYCKLAR-SW                                 
099000                               PRC-FEL-SW                                 
099100                               KANAL-FEL-SW                               
099200       END-IF                                                             
099300     END-IF                                                               
099400                                                                          
099500     IF WS-IDPRCBAS NUMERIC AND WS-IDPRCBAS > ZERO                        
099600*PRC 9998 = DIREKTLEV UTSKRIFT. SKALL SKRIVAS UT AV PGM 4695.             
099700       IF NDC                                                             
099800       AND WS-IDPRCBAS = '999' AND WS-IDPRCVAR = '8'                      
099900          MOVE NEJ        TO NYCKLAR-SW                                   
100000                             PRC-FEL-SW                                   
100100       ELSE                                                               
100200          MOVE WS-IDPRCBAS TO W-Q3B1-MIN-IDPRCBAS                         
100300                              W-Q3B1-MAX-IDPRCBAS                         
100400                              W-Q3A1-MIN-IDPRCBAS                         
100500                              W-Q3A1-MAX-IDPRCBAS                         
100600          MOVE 1          TO LAES-WAY                                     
100700       END-IF                                                             
100800     ELSE                                                                 
100900        MOVE NEJ          TO NYCKLAR-SW                                   
101000     END-IF                                                               
101100                                                                          
101200     IF WS-IDPRCVAR NOT = SPACE                                           
101300        MOVE WS-IDPRCVAR  TO W-Q3B1-MIN-IDPRCVAR                          
101400                             W-Q3B1-MAX-IDPRCVAR                          
101500                             W-Q3A1-MIN-IDPRCVAR                          
101600                             W-Q3A1-MAX-IDPRCVAR                          
101700     END-IF                                                               
101800     .                                                                    
101900     EJECT                                                                
102000 BB-KOLLA-TRANSPORT SECTION.                                              
102100                                                                          
102200     IF MID-IDTRP-IN = ALL '+'                                            
102300        MOVE MID-IDTRP-UT TO WS-IDTRP                                     
102400     ELSE                                                                 
102500        MOVE MID-IDTRP-IN TO WS-IDTRP                                     
102600        MOVE '7'          TO MFS-IDPFK                                    
102700        MOVE SPACE        TO MFS-KDTRTYP                                  
102800     END-IF                                                               
102900                                                                          
103000     INSPECT WS-IDTRP REPLACING LEADING SPACE BY ZERO                     
103100                                                                          
103200     IF WS-IDTRP NUMERIC                                                  
103300        IF WS-IDTRP > ZERO                                                
103400          IF WS-IDPRCBAS = '999' AND WS-IDPRCVAR = '8'                    
103500            MOVE NEJ      TO NYCKLAR-SW                                   
103600                             PRC-FEL-SW                                   
103700          ELSE                                                            
103800             MOVE WS-IDTRP TO W-Q3A1-MIN-IDTRP                            
103900                               W-Q3A1-MAX-IDTRP                           
104000             MOVE 2       TO LAES-WAY                                     
104100          END-IF                                                          
104200        END-IF                                                            
104300     ELSE                                                                 
104400        MOVE NEJ          TO NYCKLAR-SW                                   
104500     END-IF                                                               
104600     .                                                                    
104700     EJECT                                                                
104800 BC-KOLLA-USER SECTION.                                                   
104900                                                                          
105000     MOVE JA              TO USER-FEL-SW                                  
105100     IF MID-IDUSER-IN = ALL '+'                                           
105200        MOVE MID-IDUSER-UT TO WS-IDUSER                                   
105300     ELSE                                                                 
105400        MOVE MID-IDUSER-IN TO WS-IDUSER                                   
105500     END-IF                                                               
105600                                                                          
105700     INSPECT WS-IDUSER REPLACING LEADING SPACE BY ZERO                    
105800                                                                          
105900     IF WS-IDUSER NOT NUMERIC                                             
106000        MOVE NEJ          TO NYCKLAR-SW                                   
106100     END-IF                                                               
106200                                                                          
106300     IF WS-IDUSER (1:3) NOT = ZERO                                        
106400        MOVE NEJ          TO NYCKLAR-SW                                   
106500     END-IF                                                               
106600                                                                          
106700     IF MFS-UPDATE                                                        
106800        IF WS-IDUSER NOT > ZERO                                           
106900           PERFORM MFS-ROR-EJ-FAELT-IN                                    
107000           PERFORM MFS-ROR-EJ-FAELT-UT                                    
107100           PERFORM MFS-LAS-IN-IGEN                                        
107200           MOVE NEJ TO NYCKLAR-SW                                         
107300                       USER-FEL-SW                                        
107400        END-IF                                                            
107500     END-IF                                                               
107600     .                                                                    
107700     EJECT                                                                
107800 C-FOERSTA-SIDA SECTION.                                                  
107900                                                                          
108000     MOVE 1 TO PGNO                                                       
108100     MOVE 1 TO WS-ANTPLK                                                  
108110     MOVE JA   TO FIRST-SW                                                
108120     MOVE NEJ  TO LAST-SW                                                 
108200     MOVE 0 TO WS-KVORDER-ENTER                                           
108300               WS-KVRADER-ENTER                                           
108600               WS-KVORDER                                                 
108700               WS-KVRADER                                                 
108800                                                                          
108900     PERFORM S03-LAES-NASTA-ORDERDEL                                      
109000     IF SEGMENT-FINNS                                                     
109100        MOVE ORQA-ODEL-IDPRC  TO W-4448-IDPRC                             
109200        PERFORM S01-LAES-PRC-KANAL                                        
109300        IF SEGMENT-FINNS                                                  
109400*006 DETTA ÄR FÖRSTA SIDAN                                                
109500           MOVE '006' TO MED-IDMFSFEL                                     
109600           PERFORM S05-FEL-MEDDELANDE                                     
109700           PERFORM S04-FYLL-BILD                                          
109800        ELSE                                                              
109900*023 TABELL SAKNAS                                                        
110000           MOVE '023' TO MED-IDMFSFEL                                     
110100           PERFORM S05-FEL-MEDDELANDE                                     
110200           PERFORM MFS-RENSA-FAELT-UT                                     
110300        END-IF                                                            
110400     ELSE                                                                 
110500        IF (MID-FLKLAR NOT = JA ) OR                                      
110600           (MID-FLKLAR NOT = YES)                                         
110700*036 ORDERDELAR SAKNAS                                                    
110800           MOVE '036' TO MED-IDMFSINF                                     
110900           PERFORM S06-INFO-MEDDELANDE                                    
111000        END-IF                                                            
111100        PERFORM MFS-RENSA-FAELT-UT                                        
111200     END-IF                                                               
111300     .                                                                    
111400     EJECT                                                                
111500 D-NAESTA-SIDA SECTION.                                                   
111600                                                                          
111601     IF SAVE-IDTRANS = '4352'                                             
111602       IF  LAST-SW = JA                                                   
111604         CONTINUE                                                         
111605       ELSE                                                               
111607         IF PGNO = 20                                                     
111609           PERFORM VARYING PGNO FROM 1 BY 1                               
111610           UNTIL PGNO = 20                                                
111611            MOVE SAVE-AREA-ENTER(PGNO + 1) TO                             
111612                                   SAVE-AREA-ENTER(PGNO)                  
111637           END-PERFORM                                                    
111638           MOVE NEJ TO FIRST-SW                                           
111639         ELSE                                                             
111640           COMPUTE PGNO = PGNO + 1                                        
111641         END-IF                                                           
111642       END-IF                                                             
113660     END-IF                                                               
113661                                                                          
113662     PERFORM DA-SKAPA-NYCKEL-NEXT                                         
113663     PERFORM S09-GET-WDQ3B1-A1                                            
117200     .                                                                    
117300     EJECT                                                                
117400 DA-SKAPA-NYCKEL-NEXT SECTION.                                            
117500                                                                          
117600        IF MID-IDPRODNR-KEY (12) NUMERIC                                  
117700           MOVE MID-IDPRODNR-KEY (12) TO W-IDPRODNR-SOEK                  
117800        ELSE                                                              
117900           MOVE ZERO                  TO W-IDPRODNR-SOEK                  
118000        END-IF                                                            
118100        IF MID-IDPLKLST-KEY (12) NUMERIC                                  
118200           MOVE MID-IDPLKLST-KEY (12) TO W-IDPLKLST-SOEK                  
118300        ELSE                                                              
118400           MOVE ZERO                  TO W-IDPLKLST-SOEK                  
118500        END-IF                                                            
118600        MOVE WS-IDDC               TO W-Q3A1-MIN-IDDC                     
118700                                      W-Q3B1-MIN-IDDC                     
118800        MOVE MID-IDTRP-NEXT        TO W-Q3A1-MIN-IDTRP                    
118900        MOVE MID-TIAAMMDD-NEXT     TO W-Q3A1-MIN-DATRPAVD                 
119000        IF MID-TIAAMMDD-NEXT NOT = ZERO                                   
119100          IF MID-TIAAMMDD-NEXT < 500000                                   
119200            MOVE 20                TO W-Q3A1-MIN-DATRPAVD (1:2)           
119300          ELSE                                                            
119400            IF MID-TIAAMMDD-NEXT < 999999                                 
119500              MOVE 19              TO W-Q3A1-MIN-DATRPAVD (1:2)           
119600            ELSE                                                          
119700              MOVE 99999999        TO W-Q3A1-MIN-DATRPAVD                 
119800            END-IF                                                        
119900          END-IF                                                          
120000        END-IF                                                            
120100        MOVE MID-TIHHMM-NEXT       TO W-Q3A1-MIN-TIHHMM                   
120200        MOVE MID-TIRFS-NEXT        TO W-Q3A1-MIN-DARFS                    
120300                                      W-Q3B1-MIN-DARFS                    
120400        IF MID-TIRFS-NEXT NOT = ZERO                                      
120500          IF MID-TIRFS-NEXT < 5000000000                                  
120600            MOVE 20                TO W-Q3A1-MIN-DARFS (1:2)              
120700                                      W-Q3B1-MIN-DARFS (1:2)              
120800          ELSE                                                            
120900            IF MID-TIRFS-NEXT < 9999999999                                
121000              MOVE 19              TO W-Q3A1-MIN-DARFS (1:2)              
121100                                      W-Q3B1-MIN-DARFS (1:2)              
121200            ELSE                                                          
121300              MOVE 999999999999    TO W-Q3A1-MIN-DARFS                    
121400                                      W-Q3B1-MIN-DARFS                    
121500            END-IF                                                        
121600          END-IF                                                          
121700        END-IF                                                            
121800                                                                          
121900        MOVE MID-TILST-O-NEXT      TO W-Q3A1-MIN-DALSTORD                 
122000                                      W-Q3B1-MIN-DALSTORD                 
122100        IF MID-TILST-O-NEXT NOT = ZERO                                    
122200          IF MID-TILST-O-NEXT < 5000000000                                
122300            MOVE 20                TO W-Q3A1-MIN-DALSTORD (1:2)           
122400                                      W-Q3B1-MIN-DALSTORD (1:2)           
122500          ELSE                                                            
122600            IF MID-TILST-O-NEXT < 9999999999                              
122700              MOVE 19              TO W-Q3A1-MIN-DALSTORD (1:2)           
122800                                      W-Q3B1-MIN-DALSTORD (1:2)           
122900            ELSE                                                          
123000              MOVE 999999999999    TO W-Q3A1-MIN-DALSTORD                 
123100                                      W-Q3B1-MIN-DALSTORD                 
123200            END-IF                                                        
123300          END-IF                                                          
123400        END-IF                                                            
123500                                                                          
123600        MOVE MID-TIUTSKR-NEXT      TO W-Q3B1-MIN-DAUTSKR                  
123700        IF MID-TIUTSKR-NEXT NOT = ZERO                                    
123800          IF MID-TIUTSKR-NEXT < 500000                                    
123900            MOVE 20                TO W-Q3B1-MIN-DAUTSKR (1:2)            
124000          ELSE                                                            
124100            IF MID-TIUTSKR-NEXT < 999999                                  
124200              MOVE 19              TO W-Q3B1-MIN-DAUTSKR (1:2)            
124300            ELSE                                                          
124400              MOVE 99999999        TO W-Q3B1-MIN-DAUTSKR                  
124500            END-IF                                                        
124600          END-IF                                                          
124700        END-IF                                                            
124800        MOVE MID-TIUTSTID-NEXT     TO W-Q3B1-MIN-TIUTSTID                 
124900     .                                                                    
125000     EJECT                                                                
125100 E-SAMMA-SIDA SECTION.                                                    
125200                                                                          
125300     IF MFS-QUERY                                                         
125400        PERFORM EB-KOLLA-OM-UPPDATERING                                   
125500        IF INDATA-FEL                                                     
125600*003 TRYCK PF11 VID UPPDATERING                                           
125700           MOVE '003' TO MED-IDMFSFEL                                     
125800           PERFORM S05-FEL-MEDDELANDE                                     
125900           PERFORM MFS-ROR-EJ-FAELT-IN                                    
126000           PERFORM MFS-ROR-EJ-FAELT-UT                                    
126100           PERFORM MFS-LAS-IN-IGEN                                        
126200        END-IF                                                            
126300     END-IF                                                               
126400                                                                          
126500     IF INDATA-OK                                                         
126501        PERFORM EA-SKAPA-NYCKEL-ENTER                                     
126502        MOVE WS-ANTPLK-ENTER  TO WS-ANTPLK                                
126503        MOVE WS-KVORDER-ENTER TO WS-KVORDER                               
126504        MOVE WS-KVRADER-ENTER TO WS-KVRADER                               
126505        PERFORM S09-GET-WDQ3B1-A1                                         
132300     END-IF                                                               
132400     .                                                                    
132500     EJECT                                                                
132600 EA-SKAPA-NYCKEL-ENTER SECTION.                                           
132601                                                                          
132602     IF SAVE-KVORDER-ENTER(PGNO) NUMERIC                                  
132603        MOVE SAVE-KVORDER-ENTER(PGNO) TO WS-KVORDER-ENTER                 
132604     ELSE                                                                 
132605        MOVE 0                        TO WS-KVORDER-ENTER                 
132606     END-IF                                                               
132607                                                                          
132608     IF SAVE-KVRADER-ENTER(PGNO) NUMERIC                                  
132609        MOVE SAVE-KVRADER-ENTER(PGNO)  TO WS-KVRADER-ENTER                
132610     ELSE                                                                 
132620        MOVE 0                         TO WS-KVRADER-ENTER                
132630     END-IF                                                               
132640                                                                          
132650     IF SAVE-IDPLKLST-ENTER(PGNO) NUMERIC                                 
132660        AND                                                               
132670        SAVE-IDPLKLST-ENTER(PGNO)  >  0                                   
132680        MOVE SAVE-IDPLKLST-ENTER(PGNO) TO WS-ANTPLK-ENTER                 
132690     END-IF                                                               
132700                                                                          
132800        IF SAVE-ODEL-IDPRODNR (PGNO)    NUMERIC                           
132900           MOVE SAVE-ODEL-IDPRODNR (PGNO) TO W-IDPRODNR-SOEK              
133000        ELSE                                                              
133100           MOVE ZERO                 TO W-IDPRODNR-SOEK                   
133200        END-IF                                                            
133201        IF SAVE-ODEL-IDPLKLST (PGNO) NUMERIC                              
133202           MOVE SAVE-ODEL-IDPLKLST (PGNO) TO W-IDPLKLST-SOEK              
133500        ELSE                                                              
133600           MOVE ZERO                 TO W-IDPLKLST-SOEK                   
133700        END-IF                                                            
133800        MOVE WS-IDDC             TO W-Q3A1-MIN-IDDC                       
133900                                    W-Q3B1-MIN-IDDC                       
133910        MOVE SAVE-IDTRP-ENTER(PGNO)    TO W-Q3A1-MIN-IDTRP                
133920        MOVE SAVE-TIAAMMDD-ENTER(PGNO) TO W-Q3A1-MIN-DATRPAVD             
133930        IF SAVE-TIAAMMDD-ENTER(PGNO) NOT = ZERO                           
133940          IF SAVE-TIAAMMDD-ENTER(PGNO)  < 500000                          
133950            MOVE 20                TO W-Q3A1-MIN-DATRPAVD (1:2)           
134500          ELSE                                                            
134501            IF SAVE-TIAAMMDD-ENTER(PGNO)  < 999999                        
134502              MOVE 19              TO W-Q3A1-MIN-DATRPAVD (1:2)           
134800            ELSE                                                          
134900              MOVE 99999999        TO W-Q3A1-MIN-DATRPAVD                 
134901            END-IF                                                        
134902          END-IF                                                          
134903        END-IF                                                            
135300                                                                          
135310        MOVE SAVE-TIHHMM-ENTER(PGNO) TO W-Q3A1-MIN-TIHHMM                 
135320        MOVE SAVE-TIRFS-ENTER(PGNO)  TO W-Q3A1-MIN-DARFS                  
135330                                        W-Q3B1-MIN-DARFS                  
135340        IF SAVE-TIRFS-ENTER(PGNO)   NOT = ZERO                            
135350          IF SAVE-TIRFS-ENTER(PGNO) < 5000000000                          
135360            MOVE 20                TO W-Q3A1-MIN-DARFS (1:2)              
135370                                      W-Q3B1-MIN-DARFS (1:2)              
136200          ELSE                                                            
136201            IF SAVE-TIRFS-ENTER(PGNO) < 9999999999                        
136202              MOVE 19              TO W-Q3A1-MIN-DARFS (1:2)              
136203                                      W-Q3B1-MIN-DARFS (1:2)              
136600            ELSE                                                          
136601              MOVE 999999999999    TO W-Q3A1-MIN-DARFS                    
136602                                      W-Q3B1-MIN-DARFS                    
136801            END-IF                                                        
136802          END-IF                                                          
136803        END-IF                                                            
137200                                                                          
137210        MOVE SAVE-TILST-O-ENTER(PGNO)  TO W-Q3A1-MIN-DALSTORD             
137220                                          W-Q3B1-MIN-DALSTORD             
137230        IF SAVE-TILST-O-ENTER(PGNO)    NOT = ZERO                         
137240          IF SAVE-TILST-O-ENTER(PGNO)  < 5000000000                       
137250            MOVE 20                TO W-Q3A1-MIN-DALSTORD (1:2)           
137260                                      W-Q3B1-MIN-DALSTORD (1:2)           
137900          ELSE                                                            
137901            IF SAVE-TILST-O-ENTER(PGNO)  < 9999999999                     
137902              MOVE 19              TO W-Q3A1-MIN-DALSTORD (1:2)           
137903                                      W-Q3B1-MIN-DALSTORD (1:2)           
138300            ELSE                                                          
138301              MOVE 999999999999    TO W-Q3A1-MIN-DALSTORD                 
138302                                      W-Q3B1-MIN-DALSTORD                 
138501            END-IF                                                        
138502          END-IF                                                          
138503        END-IF                                                            
138900                                                                          
138910        MOVE SAVE-TIUTSKR-ENTER(PGNO)    TO W-Q3B1-MIN-DAUTSKR            
138920        IF SAVE-TIUTSKR-ENTER(PGNO)   NOT = ZERO                          
138930          IF SAVE-TIUTSKR-ENTER(PGNO)    < 500000                         
138940            MOVE 20                TO W-Q3B1-MIN-DAUTSKR (1:2)            
139400          ELSE                                                            
139401            IF SAVE-TIUTSKR-ENTER(PGNO)  < 999999                         
139402              MOVE 19              TO W-Q3B1-MIN-DAUTSKR (1:2)            
139700            ELSE                                                          
139800              MOVE 99999999        TO W-Q3B1-MIN-DAUTSKR                  
139801            END-IF                                                        
139802          END-IF                                                          
139803        END-IF                                                            
139804        MOVE SAVE-TIUTSTID-ENTER(PGNO)  TO W-Q3B1-MIN-TIUTSTID            
140400     .                                                                    
140500     EJECT                                                                
140600 EB-KOLLA-OM-UPPDATERING SECTION.                                         
140700                                                                          
140800     MOVE 1 TO IX1                                                        
140900     PERFORM UNTIL IX1 > MAX-IX                                           
141000        IF MID-FLORDDEL (IX1) NOT = SPACE                                 
141100           MOVE NEJ TO INDATA-SW                                          
141200        END-IF                                                            
141300        ADD 1 TO IX1                                                      
141400     END-PERFORM                                                          
141500                                                                          
141600     IF MID-FLKLAR NOT = NEJ                                              
141700        MOVE NEJ TO INDATA-SW                                             
141800     END-IF                                                               
141900     .                                                                    
142000     EJECT                                                                
142100 F-LAES-PLOCKSATS-PLOCKARE SECTION.                                       
142200                                                                          
142300     MOVE NEJ TO PLOCK-SW                                                 
142400                                                                          
142500     MOVE WS-IDDC       TO W-4011-IDDC                                    
142600     MOVE WS-IDUSER     TO W-4011-IDUSER                                  
142700                                                                          
142800     PERFORM IMS-GHU-4011-WL401111                                        
142900     IF SEGMENT-FINNS                                                     
143000        MOVE JA         TO PLOCK-SW                                       
143100     END-IF                                                               
143200                                                                          
143300     IF PLOCKSATS-SAKNAS                                                  
143400        MOVE 1          TO 4012-KDSEGKEY                                  
143500        MOVE ZERO       TO 4012-IXHEL                                     
143600        MOVE 1 TO IX1                                                     
143700        PERFORM UNTIL IX1 > MAX-ANTAL-ORDERDELAR                          
143800           MOVE LOW-VALUE TO 4012-ORDDEL (IX1)                            
143900           ADD 1 TO IX1                                                   
144000        END-PERFORM                                                       
144100     END-IF                                                               
144200     .                                                                    
144300     EJECT                                                                
144400 G-EXPANDERA-SIDA SECTION.                                                
144500*                                                                         
144600*  PROCEDUREN HANTERAR PF9-TRYCKNING VILKET MÅSTE INITIERA                
144700*  EXPANDERING AV SPECIELL KUND                                           
144800*  NÄR DISTR/KUND FINNS I SPARAREAN RÄCKER DETTA FÖR ATT VETA             
144900*  ATT VI HANTERAR GIVET DISTRIKT/KUND                                    
145000*  GENOM ATT ANVÄNDA BEFINTLIGA SECTIONER (C-, D- OCH E-)                 
145100*  KAN MAN BLÄDDRA MED PF8 ÄVEN PÅ EXPANDERADE SIDOR UTAN                 
145200*  ALLT FÖR MYCKET NYKODNING                                              
145300*                                                                         
145400     IF EXTEND-SW = NEJ                                                   
145410        MOVE SPACE  TO SPAR-AREA                                          
145420     END-IF                                                               
145430                                                                          
145500     IF SPAR-AREA = SPACE                                                 
145600*    VISA FÖRSTA EXPANDERADE SIDAN                                        
145700        PERFORM GA-SPARA-FORSTA-MARKERADE-KUND                            
145800        IF SPAR-AREA = SPACE                                              
145900*       INGEN RAD MARKERAD, VISA SAMMA SIDA                               
146000           PERFORM E-SAMMA-SIDA                                           
146100        ELSE                                                              
146200           PERFORM C-FOERSTA-SIDA                                         
146800        END-IF                                                            
146900     ELSE                                                                 
147000*    VISA NÄSTA  EXPANDERADE SIDA                                         
147100        PERFORM D-NAESTA-SIDA                                             
147200     END-IF                                                               
147300     .                                                                    
147400     EJECT                                                                
147500 GA-SPARA-FORSTA-MARKERADE-KUND SECTION.                                  
147600                                                                          
147700     MOVE 1 TO IX1                                                        
147800     PERFORM UNTIL IX1 > MAX-IX                                           
147900                OR SPAR-IDDISTR-X > SPACE                                 
148000                                                                          
148100        IF MID-FLORDDEL(IX1) = EXPAND                                     
148200           IF MID-IDPRODNR-KEY (IX1) NUMERIC   AND                        
148300              MID-IDPLKLST-KEY (IX1) NUMERIC                              
148400              MOVE MID-IDPRODNR-KEY (IX1) TO W-Q3DSEQ-IDPRODNR            
148500              MOVE MID-IDPLKLST-KEY (IX1) TO W-Q3DSEQ-IDPLKLST            
148600              PERFORM IMS-GU-WDQ3DSEQ-WLORQA01                            
148700              IF SEGMENT-FINNS                                            
148800                IF ORQA-ODEL-IDDC = WS-IDDC                               
148900                  CONTINUE                                                
149000                ELSE                                                      
149100                  PERFORM IMS-GN-WDQ3DSEQ-WLORQA01                        
149200*TILLÄGG PGA AV ATT DET FINNS "LÖSA" WDQ301 DVS ORENSADE WDQ301           
149300*SOM HAR SAMMA PRODNR SOM EN NY ORDER.                                    
149400*RESNINGSFEL? ELLER PGA FELUPPDATERING AV WDQ301 TIDIGARE?                
149500                END-IF                                                    
149600              END-IF                                                      
149700              IF SEGMENT-FINNS                                            
149800                 MOVE JA                 TO EXTEND-SW                     
149900                 MOVE ORQA-ODEL-IDDISTR  TO SPAR-IDDISTR                  
150000                 MOVE ORQA-ODEL-IDKUNDNR TO SPAR-IDKUNDNR                 
150100              END-IF                                                      
150200           END-IF                                                         
150300        END-IF                                                            
150400        ADD 1 TO IX1                                                      
150500     END-PERFORM                                                          
150600     .                                                                    
150700     EJECT                                                                
150800 H-UPPDATERA SECTION.                                                     
150900                                                                          
151000     MOVE JA TO INDATA-SW                                                 
151100                PLOCKSATS-SW                                              
151200                                                                          
151300     PERFORM HD-KOLLA-PRINTER                                             
151400     PERFORM HA-KOLLA-SPARA-ORDERDEL                                      
151500                                                                          
151600     IF 4012-IXHEL > MAX-ANTAL-ORDERDELAR                                 
151700        MOVE NEJ TO PLOCKSATS-SW                                          
151800        IF PLOCKSATS-FINNS                                                
151900           PERFORM IMS-GHU-4011-WL401101                                  
152000           PERFORM IMS-DLET-4011-WL401101                                 
152100        END-IF                                                            
152200     ELSE                                                                 
152300        IF INDATA-OK                                                      
152400           IF MID-FLKLAR = JA OR YES                                      
152500              PERFORM HB-UPPDAT-ORDERDEL                                  
152600              IF PLOCKSATS-OK                                             
152700                 PERFORM HC-UPPDAT-PLOCKSATS-REG                          
152800              END-IF                                                      
152900           ELSE                                                           
153000              PERFORM HC-UPPDAT-PLOCKSATS-REG                             
153100           END-IF                                                         
153200        END-IF                                                            
153300     END-IF                                                               
153400                                                                          
153500     IF INDATA-FEL                                                        
153600*001 KORRIGERA UPPLYSTA FÄLT                                              
153700        IF MED-IDMFSFEL = SPACE                                           
153800          MOVE '001' TO MED-IDMFSFEL                                      
153900        END-IF                                                            
154000        PERFORM S05-FEL-MEDDELANDE                                        
154100        PERFORM MFS-ROR-EJ-FAELT-IN                                       
154200        PERFORM MFS-ROR-EJ-FAELT-UT                                       
154300     ELSE                                                                 
154400        IF PLOCKSATS-FEL                                                  
154500*FELMED: 173 PLOCKSATS > 500 RADER, BÖRJA OM                              
154600           MOVE NEJ TO INDATA-SW                                          
154700           MOVE '173' TO MED-IDMFSFEL                                     
154800           PERFORM S05-FEL-MEDDELANDE                                     
154900           PERFORM MFS-RENSA-FAELT-IN                                     
155000           PERFORM MFS-RENSA-FAELT-UT                                     
155100        END-IF                                                            
155200     END-IF                                                               
155300     .                                                                    
155400     EJECT                                                                
155500 HA-KOLLA-SPARA-ORDERDEL SECTION.                                         
155600                                                                          
155700     IF MID-FLKLAR = JA OR YES OR NEJ                                     
155800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKLAR-ATTR                      
155900     ELSE                                                                 
156000        MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKLAR-ATTR                      
156100        MOVE NEJ                  TO INDATA-SW                            
156200     END-IF                                                               
156300                                                                          
156400     MOVE 1 TO IX1                                                        
156500     PERFORM UNTIL IX1 > MAX-IX                                           
156600        IF MID-FLORDDEL (IX1) = SPACE OR 'X'                              
156700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLORDDEL-ATTR (IX1)           
156800        ELSE                                                              
156900           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLORDDEL-ATTR (IX1)           
157000           MOVE NEJ                  TO INDATA-SW                         
157100        END-IF                                                            
157200        ADD 1 TO IX1                                                      
157300     END-PERFORM                                                          
157400                                                                          
157500     IF INDATA-OK                                                         
157600        MOVE 1 TO IX1                                                     
157700        PERFORM UNTIL IX1 > MAX-IX                                        
157800           IF MID-FLORDDEL (IX1) = 'X'                                    
157900              PERFORM HAA-LAGRA-ORDERDEL                                  
158000           END-IF                                                         
158100           MOVE MFS-RENSA-FAELT TO MOD-FLORDDEL      (IX1)                
158200                                   MOD-IDPRODNR-KEY  (IX1)                
158300                                   MOD-IDPLKLST-KEY  (IX1)                
158400           ADD 1 TO IX1                                                   
158500        END-PERFORM                                                       
158600     END-IF                                                               
158700     .                                                                    
158800     EJECT                                                                
158900 HAA-LAGRA-ORDERDEL SECTION.                                              
159000                                                                          
159100     IF MID-IDPRODNR-KEY (IX1) NUMERIC   AND                              
159200        MID-IDPLKLST-KEY (IX1) NUMERIC                                    
159300        MOVE MID-IDPRODNR-KEY (IX1) TO W-Q3DSEQ-IDPRODNR                  
159400        MOVE MID-IDPLKLST-KEY (IX1) TO W-Q3DSEQ-IDPLKLST                  
159500        PERFORM IMS-GU-WDQ3DSEQ-WLORQA01                                  
159600        IF SEGMENT-FINNS                                                  
159700          IF ORQA-ODEL-IDDC = WS-IDDC                                     
159800            CONTINUE                                                      
159900          ELSE                                                            
160000            PERFORM IMS-GN-WDQ3DSEQ-WLORQA01                              
160100*TILLÄGG PGA AV ATT DET FINNS "LÖSA" WDQ301 DVS ORENSADE WDQ301           
160200*SOM HAR SAMMA PRODNR SOM EN NY ORDER.                                    
160300*RESNINGSFEL? ELLER PGA FELUPPDATERING AV WDQ301 TIDIGARE?                
160400          END-IF                                                          
160500        END-IF                                                            
160600        IF SEGMENT-FINNS                                                  
160700          IF ORQA-ODEL-IDDC = WS-IDDC                                     
160800            MOVE ORQA-ODEL-IDORDER TO WS-ODEL-IDORDER                     
160900            MOVE ORQA-ODEL-IDDC    TO WS-ODEL-IDDC                        
161000            MOVE ORQA-ODEL-IDPRODNR TO WS-ODEL-IDPRODNR                   
161100            MOVE ORQA-ODEL-IDPLKLST TO WS-ODEL-IDPLKLST                   
161200            MOVE 1 TO IX2                                                 
161300            PERFORM UNTIL IX2 > 4012-IXHEL                                
161400                       OR IX2 > 99                                        
161500               IF WS-ORDDEL = 4012-ORDDEL (IX2)                           
161600                  MOVE LOW-VALUE TO WS-ORDDEL                             
161700                  MOVE 99 TO IX2                                          
161800               END-IF                                                     
161900               ADD 1 TO IX2                                               
162000            END-PERFORM                                                   
162100            IF WS-ORDDEL NOT = LOW-VALUE                                  
162200               ADD 1         TO 4012-IXHEL                                
162300               IF 4012-IXHEL <= MAX-ANTAL-ORDERDELAR                      
162400                  MOVE WS-ORDDEL TO 4012-ORDDEL (4012-IXHEL)              
162500               END-IF                                                     
162600            END-IF                                                        
162700          END-IF                                                          
162800        END-IF                                                            
162900     END-IF                                                               
163000     .                                                                    
163100 EJECT                                                                    
163200 HB-UPPDAT-ORDERDEL SECTION.                                              
163300                                                                          
163400     IF 4012-IXHEL > 0                                                    
163500        MOVE LOW-VALUE TO WS-TAB-ORDERDELAR                               
163600        MOVE 1 TO IX1                                                     
163700        MOVE 0 TO IX2                                                     
163800        PERFORM UNTIL IX1 > 4012-IXHEL                                    
163900           MOVE 4012-IDORDER  (IX1) TO W-Q301KY-IDORDER                   
164000           MOVE 4012-IDDC     (IX1) TO W-Q301KY-IDDC                      
164100           MOVE 4012-IDPRODNR (IX1) TO W-Q301KY-IDPRODNR                  
164200           MOVE 4012-IDPLKLST (IX1) TO W-Q301KY-IDPLKLST                  
164300           PERFORM IMS-GHU-ORQ-WLORQA01                                   
164400           IF SEGMENT-FINNS AND ORQA-ODEL-KDODELSTA = 'R'                 
164500              MOVE ORQA-ODEL-IDORDER TO W-IDORDER                         
164600              PERFORM IMS-GHU-ORQI-WLORQI01                               
164700              IF SEGMENT-FINNS                                            
164800                 IF ORQI-OHUV-FLKLAR = JA                                 
164900                    ADD ORQA-ODEL-KVRADER TO WS-PLKSATS-KVRADER           
165000                    ADD 1                 TO IX2                          
165100                    MOVE W-WDQ301KY-X     TO WS-TAB-ORDDEL (IX2)          
165200                    MOVE WS-IDUSER        TO ORQA-ODEL-IDUSER             
165300                    MOVE WS-IDBORD        TO ORQA-ODEL-IDBORD             
165400                    MOVE WS-DATUM-LOK     TO ORQA-ODEL-DAUTSKR            
165500                    IF WS-DATUM-LOK NOT = ZERO                            
165600                      IF WS-DATUM-LOK < 500000                            
165700                        MOVE 20        TO ORQA-ODEL-DAUTSKR (1:2)         
165800                      ELSE                                                
165900                        IF WS-DATUM-LOK < 999999                          
166000                          MOVE 19      TO ORQA-ODEL-DAUTSKR (1:2)         
166100                        ELSE                                              
166200                          MOVE 99999999 TO ORQA-ODEL-DAUTSKR              
166300                        END-IF                                            
166400                      END-IF                                              
166500                    END-IF                                                
166600                    MOVE WS-TIHHMMSS-LOK  TO ORQA-ODEL-TIUTSTID           
166700                    MOVE 'U'              TO ORQA-ODEL-KDODELSTA          
166800                    PERFORM IMS-REPL-ORQ-WLORQA01                         
166900                 END-IF                                                   
167000              END-IF                                                      
167100           END-IF                                                         
167200           ADD 1 TO IX1                                                   
167300        END-PERFORM                                                       
167400        IF WS-PLKSATS-KVRADER > MAX-PLKSATS-RADER                         
167500           MOVE NEJ TO PLOCKSATS-SW                                       
167600           PERFORM HBC-ATERSTALL-ORDERDELAR                               
167700           IF PLOCKSATS-FINNS                                             
167800              PERFORM IMS-GHU-4011-WL401101                               
167900              PERFORM IMS-DLET-4011-WL401101                              
168000           END-IF                                                         
168100        ELSE                                                              
168200           IF IX2 > 0                                                     
168300              IF IX2 = 4012-IXHEL                                         
168400*037 PLOCKSATS KÖAD FÖR UTSKRIFT                                          
168500                 MOVE '037' TO MED-IDMFSINF                               
168600                 PERFORM S06-INFO-MEDDELANDE                              
168700              ELSE                                                        
168800*038 EJ KOMPLETT PLOCKSATS KÖAD FÖR UTSKRIFT                              
168900                 MOVE '038' TO MED-IDMFSINF                               
169000                 PERFORM S06-INFO-MEDDELANDE                              
169100              END-IF                                                      
169200              PERFORM HBA-FLYTTA-TAB-ORDERDELAR                           
169300              PERFORM HBB-HAMTA-PLOCKSATSNR                               
169400           END-IF                                                         
169500        END-IF                                                            
169600     ELSE                                                                 
169700*011 PF11 OCH INGET INMATAT                                               
169800        MOVE '011' TO MED-IDMFSFEL                                        
169900        PERFORM S05-FEL-MEDDELANDE                                        
170000        PERFORM MFS-ROR-EJ-FAELT-IN                                       
170100        PERFORM MFS-ROR-EJ-FAELT-UT                                       
170200     END-IF                                                               
170300     .                                                                    
170400     EJECT                                                                
170500 HBA-FLYTTA-TAB-ORDERDELAR SECTION.                                       
170600                                                                          
170700     MOVE 1         TO 4002-KDSEGKEY                                      
170800     MOVE NEJ       TO 4002-FLORDKNY                                      
170900                       4002-FLORDSPL                                      
171000     MOVE WS-IDBORD TO 4002-IDBORD                                        
171100     MOVE WS-IDUSER TO 4002-IDUSER                                        
171200                                                                          
171300     INITIALIZE        4002-DEAL-PR-SUM                                   
171400     MOVE ZERO      TO 4002-IXHEL                                         
171500                       4002-KVORDSPL                                      
171600                       4002-KVRADER                                       
171700                       4002-SUORDV                                        
171800                       4002-VKORDNTO                                      
171900                       4002-VLORDNTO                                      
172000     MOVE WS-4002-KDPRT-PLE                                               
172100                    TO 4002-KDPRT-PLE                                     
172200     MOVE WS-4002-KDPRT-PU                                                
172300                    TO 4002-KDPRT-PU                                      
172400                                                                          
172500     MOVE SPACE     TO 4002-KDSORT                                        
172600                       4002-IDMSG3IV                                      
172700                       4002-IDSNO3IV                                      
172800                       4002-ADDISPXTRA                                    
172900                                                                          
173000     MOVE 1 TO IX1                                                        
173100     PERFORM UNTIL IX1 > MAX-ANTAL-ORDERDELAR                             
173200        MOVE WS-TAB-ORDDEL (IX1) TO 4002-ORDDEL (IX1)                     
173300        ADD 1 TO IX1                                                      
173400     END-PERFORM                                                          
173500     .                                                                    
173600     EJECT                                                                
173700 HBB-HAMTA-PLOCKSATSNR SECTION.                                           
173800                                                                          
173900     PERFORM S01-LAES-PRC-KANAL                                           
174000     MOVE WS-IDDC  TO W-4461-IDDC                                         
174100     PERFORM IMS-GHU-XXKO-WLXXKO11                                        
174200     IF SEGMENT-FINNS                                                     
174300        IF XXKO-4462-IDLOPNR-PL = 999                                     
174400           MOVE WS-DATUM-LOK TO XXKO-4462-TIDATUM                         
174500           MOVE 1            TO XXKO-4462-IDLOPNR-PL                      
174600        ELSE                                                              
174700           ADD  1            TO XXKO-4462-IDLOPNR-PL                      
174800        END-IF                                                            
174900        MOVE XXKO-4462-IDLOPNR-PL TO 4002-IDLOPNR-PL                      
175000        PERFORM IMS-REPL-XXKO-WLXXKO11                                    
175100     ELSE                                                                 
175200        MOVE 1                    TO 4002-IDLOPNR-PL                      
175300     END-IF                                                               
175400     .                                                                    
175500     EJECT                                                                
175600 HBC-ATERSTALL-ORDERDELAR SECTION.                                        
175700                                                                          
175800     MOVE 1 TO IX1                                                        
175900                                                                          
176000     PERFORM UNTIL WS-TAB-ORDDEL (IX1) = LOW-VALUE                        
176100        MOVE WS-TAB-ORDDEL (IX1) TO W-WDQ301KY-X                          
176200        PERFORM IMS-GHU-ORQ-WLORQA01                                      
176300        IF SEGMENT-FINNS                                                  
176400           MOVE ZERO  TO ORQA-ODEL-DAUTSKR                                
176500                         ORQA-ODEL-TIUTSTID                               
176600           MOVE SPACE TO ORQA-ODEL-IDUSER                                 
176700                         ORQA-ODEL-IDBORD                                 
176800           MOVE 'R'   TO ORQA-ODEL-KDODELSTA                              
176900           PERFORM IMS-REPL-ORQ-WLORQA01                                  
177000        END-IF                                                            
177100        ADD 1 TO IX1                                                      
177200     END-PERFORM                                                          
177300     .                                                                    
177400     EJECT                                                                
177500 HC-UPPDAT-PLOCKSATS-REG SECTION.                                         
177600***********************************************************               
177700* OM MID-FLKLAR = JA  INNEBÄR DET ATT MAN HAR TRYCKT PF11 *               
177800* OCH AVSLUTAT EN PLOCKSATS. ENDAST I DETTA FALL SKICKAS  *               
177900* EN TRANS TILL NÄSTA PGM.                                *               
178000***********************************************************               
178100                                                                          
178200     IF MID-FLKLAR = JA OR YES                                            
178300        IF 4002-ORDDEL (1) NOT = LOW-VALUE                                
178400           MOVE 4002-IDPRODNR (1) TO W-4001-IDPRODNR                      
178500                                     PTOP-MID-IDPRODNR                    
178600           MOVE 4002-IDPLKLST (1) TO W-4001-IDPLKLST                      
178700                                     PTOP-MID-IDPLKLST                    
178800           MOVE W-4001-IDHTYP-X   TO WL400101                             
178900           PERFORM IMS-ISRT-4001-WL400101                                 
179000           PERFORM IMS-ISRT-4001-WL400111                                 
179100           PERFORM IMS-ISRT-MSG-ALT                                       
179200        END-IF                                                            
179300        IF PLOCKSATS-FINNS                                                
179400           PERFORM IMS-GHU-4011-WL401101                                  
179500           PERFORM IMS-DLET-4011-WL401101                                 
179600        END-IF                                                            
179700     ELSE                                                                 
179800        IF PLOCKSATS-FINNS                                                
179900           PERFORM IMS-REPL-4011-WL401111                                 
180000        ELSE                                                              
180100           IF 4012-IXHEL > 0                                              
180200              MOVE W-4011-IDHTYP-X TO WL401101                            
180300              PERFORM IMS-ISRT-4011-WL401101                              
180400              PERFORM IMS-ISRT-4011-WL401111                              
180500           END-IF                                                         
180600        END-IF                                                            
180700     END-IF                                                               
180800     .                                                                    
180900     EJECT                                                                
181000 HD-KOLLA-PRINTER        SECTION.                                         
181100                                                                          
181200     MOVE SPACE                              TO WS-4002-KDPRT-PU          
181300                                                WS-4002-KDPRT-PLE         
181400     IF MID-KDPRT-PU = LOW-VALUE OR                                       
181500        MID-KDPRT-PU = SPACE                                              
181600        MOVE ALL '+' TO MID-KDPRT-PU                                      
181700     END-IF                                                               
181800                                                                          
181900     IF MID-KDPRT-PLE = LOW-VALUE OR                                      
182000        MID-KDPRT-PLE = SPACE                                             
182100        MOVE ALL '+' TO MID-KDPRT-PLE                                     
182200     END-IF                                                               
182300                                                                          
182400*    ENDAST CDC FÅR ANGE PRINTER                                          
182500                                                                          
182600     IF NOT CDC-SE AND                                                    
182700        (MID-KDPRT-PU NOT = ALL '+' OR                                    
182800        MID-KDPRT-PLE NOT = ALL '+')                                      
182900        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PU-ATTR                      
183000        MOVE MID-KDPRT-PU                     TO MOD-KDPRT-PU             
183100        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PLE-ATTR                     
183200        MOVE MID-KDPRT-PLE                    TO MOD-KDPRT-PLE            
183300        MOVE '808' TO MED-IDMFSFEL                                        
183400        PERFORM S05-FEL-MEDDELANDE                                        
183500        MOVE NEJ                              TO INDATA-SW                
183600     END-IF                                                               
183700                                                                          
183800*    ANGES PRINTER MÅSTE BÅDA VAR IFYLLDA                                 
183900     IF INDATA-OK AND                                                     
184000        ((MID-KDPRT-PU NOT = ALL '+' AND                                  
184100        MID-KDPRT-PLE = ALL '+')                                          
184200        OR                                                                
184300        (MID-KDPRT-PU = ALL '+' AND                                       
184400        MID-KDPRT-PLE NOT = ALL '+'))                                     
184500        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PU-ATTR                      
184600        MOVE MID-KDPRT-PU                     TO MOD-KDPRT-PU             
184700        MOVE '809' TO MED-IDMFSFEL                                        
184800        PERFORM S05-FEL-MEDDELANDE                                        
184900        MOVE NEJ                              TO INDATA-SW                
185000     END-IF                                                               
185100                                                                          
185200     IF INDATA-OK AND                                                     
185300        MID-KDPRT-PU NOT = ALL '+'                                        
185400        MOVE '4'                           TO WS-SYSTDEL                  
185500        MOVE 'PU'                          TO WS-LISTTYP                  
185600        MOVE MID-KDPRT-PU TO WS-KDPRT                                     
185700                                                                          
185800        MOVE 1                             TO PRT-KDCALL                  
185900        MOVE WS-IDPRTLST                   TO PRT-IDPRTLST                
186000        CALL W006PRT                   USING PRT-W006PRT                  
186100                                                                          
186200        IF PRT-IDLTERM = 'SAKNAS  '                                       
186300           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PU-ATTR                   
186400           MOVE MID-KDPRT-PU               TO MOD-KDPRT-PU                
186500           MOVE '772' TO MED-IDMFSFEL                                     
186600           PERFORM S05-FEL-MEDDELANDE                                     
186700           MOVE NEJ                    TO INDATA-SW                       
186800        ELSE                                                              
186900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRT-PU-ATTR                 
187000           MOVE MID-KDPRT-PU                TO MOD-KDPRT-PU               
187100                                               WS-4002-KDPRT-PU           
187200        END-IF                                                            
187300     END-IF                                                               
187400                                                                          
187500     IF INDATA-OK AND                                                     
187600        MID-KDPRT-PLE NOT = ALL '+'                                       
187700        MOVE '4'                            TO WS-SYSTDEL                 
187800        MOVE 'PE'                           TO WS-LISTTYP                 
187900        MOVE MID-KDPRT-PLE TO WS-KDPRT                                    
188000                                                                          
188100        MOVE 1                              TO PRT-KDCALL                 
188200        MOVE WS-IDPRTLST                    TO PRT-IDPRTLST               
188300        CALL W006PRT                   USING PRT-W006PRT                  
188400                                                                          
188500        IF PRT-IDLTERM = 'SAKNAS  '                                       
188600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRT-PLE-ATTR                  
188700           MOVE MID-KDPRT-PLE      TO MOD-KDPRT-PLE                       
188800           MOVE '772' TO MED-IDMFSFEL                                     
188900           PERFORM S05-FEL-MEDDELANDE                                     
189000           MOVE NEJ                    TO INDATA-SW                       
189100        ELSE                                                              
189200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRT-PLE-ATTR                
189300           MOVE MID-KDPRT-PLE        TO MOD-KDPRT-PLE                     
189400                                        WS-4002-KDPRT-PLE                 
189500        END-IF                                                            
189600     END-IF                                                               
189700                                                                          
189701     .                                                                    
189702     EJECT                                                                
190000 I-PREV-SIDA  SECTION.                                                    
190100                                                                          
190200     IF SAVE-IDTRANS = '4352'                                             
190300       IF PGNO > 1                                                        
190400         COMPUTE PGNO = PGNO - 1                                          
190500         MOVE NEJ TO LAST-SW                                              
190600       END-IF                                                             
190810       IF PGNO = 1                                                        
190820         IF FIRST-SW = JA                                                 
190830           MOVE '006' TO MED-IDMFSFEL                                     
190850         ELSE                                                             
190860           MOVE '368' TO MED-IDMFSFEL                                     
190880         END-IF                                                           
190881         PERFORM S05-FEL-MEDDELANDE                                       
190890       END-IF                                                             
190900     END-IF                                                               
190901     PERFORM IA-SKAPA-NYCKEL-PREV                                         
190902     MOVE WS-ANTPLK-ENTER  TO WS-ANTPLK                                   
190903     MOVE WS-KVORDER-ENTER TO WS-KVORDER                                  
190904     MOVE WS-KVRADER-ENTER TO WS-KVRADER                                  
190905     PERFORM S09-GET-WDQ3B1-A1                                            
196300     .                                                                    
196400 IA-SKAPA-NYCKEL-PREV SECTION.                                            
196410     IF SAVE-KVORDER-ENTER(PGNO) NUMERIC                                  
196420        MOVE SAVE-KVORDER-ENTER(PGNO) TO WS-KVORDER-ENTER                 
196430     ELSE                                                                 
196440        MOVE 0                        TO WS-KVORDER-ENTER                 
196450     END-IF                                                               
196460                                                                          
196470     IF SAVE-KVRADER-ENTER(PGNO) NUMERIC                                  
196480        MOVE SAVE-KVRADER-ENTER(PGNO)  TO WS-KVRADER-ENTER                
196490     ELSE                                                                 
196491        MOVE 0                         TO WS-KVRADER-ENTER                
196492     END-IF                                                               
196493                                                                          
196494     IF SAVE-IDPLKLST-ENTER(PGNO) NUMERIC                                 
196495        AND                                                               
196496        SAVE-IDPLKLST-ENTER(PGNO)  >  0                                   
196497        MOVE SAVE-IDPLKLST-ENTER(PGNO) TO WS-ANTPLK-ENTER                 
196498     END-IF                                                               
196600        IF SAVE-ODEL-IDPRODNR (PGNO) NUMERIC                              
196700           MOVE SAVE-ODEL-IDPRODNR (PGNO) TO W-IDPRODNR-SOEK              
196800        ELSE                                                              
196900           MOVE ZERO                  TO W-IDPRODNR-SOEK                  
197000        END-IF                                                            
197100        IF SAVE-ODEL-IDPLKLST (PGNO) NUMERIC                              
197200           MOVE SAVE-ODEL-IDPLKLST (PGNO) TO W-IDPLKLST-SOEK              
197300        ELSE                                                              
197400           MOVE ZERO                  TO W-IDPLKLST-SOEK                  
197500        END-IF                                                            
197600        MOVE WS-IDDC               TO W-Q3A1-MIN-IDDC                     
197700                                      W-Q3B1-MIN-IDDC                     
197800        MOVE SAVE-IDTRP-ENTER(PGNO)    TO W-Q3A1-MIN-IDTRP                
197900        MOVE SAVE-TIAAMMDD-ENTER(PGNO) TO W-Q3A1-MIN-DATRPAVD             
198000        IF SAVE-TIAAMMDD-ENTER(PGNO) NOT = ZERO                           
198100          IF SAVE-TIAAMMDD-ENTER(PGNO)  < 500000                          
198200            MOVE 20                TO W-Q3A1-MIN-DATRPAVD (1:2)           
198300          ELSE                                                            
198400            IF SAVE-TIAAMMDD-ENTER(PGNO)  < 999999                        
198500              MOVE 19              TO W-Q3A1-MIN-DATRPAVD (1:2)           
198600            ELSE                                                          
198700              MOVE 99999999        TO W-Q3A1-MIN-DATRPAVD                 
198800            END-IF                                                        
198900          END-IF                                                          
199000        END-IF                                                            
199100        MOVE SAVE-TIHHMM-ENTER(PGNO) TO W-Q3A1-MIN-TIHHMM                 
199200        MOVE SAVE-TIRFS-ENTER(PGNO)  TO W-Q3A1-MIN-DARFS                  
199300                                        W-Q3B1-MIN-DARFS                  
199400        IF SAVE-TIRFS-ENTER(PGNO)   NOT = ZERO                            
199500          IF SAVE-TIRFS-ENTER(PGNO) < 5000000000                          
199600            MOVE 20                TO W-Q3A1-MIN-DARFS (1:2)              
199700                                      W-Q3B1-MIN-DARFS (1:2)              
199800          ELSE                                                            
199900            IF SAVE-TIRFS-ENTER(PGNO) < 9999999999                        
200000              MOVE 19              TO W-Q3A1-MIN-DARFS (1:2)              
200100                                      W-Q3B1-MIN-DARFS (1:2)              
200200            ELSE                                                          
200300              MOVE 999999999999    TO W-Q3A1-MIN-DARFS                    
200400                                      W-Q3B1-MIN-DARFS                    
200500            END-IF                                                        
200600          END-IF                                                          
200700        END-IF                                                            
200800                                                                          
200900        MOVE SAVE-TILST-O-ENTER(PGNO)  TO W-Q3A1-MIN-DALSTORD             
201000                                          W-Q3B1-MIN-DALSTORD             
201100        IF SAVE-TILST-O-ENTER(PGNO)    NOT = ZERO                         
201200          IF SAVE-TILST-O-ENTER(PGNO)  < 5000000000                       
201300            MOVE 20                TO W-Q3A1-MIN-DALSTORD (1:2)           
201400                                      W-Q3B1-MIN-DALSTORD (1:2)           
201500          ELSE                                                            
201600            IF SAVE-TILST-O-ENTER(PGNO)  < 9999999999                     
201700              MOVE 19              TO W-Q3A1-MIN-DALSTORD (1:2)           
201800                                      W-Q3B1-MIN-DALSTORD (1:2)           
201900            ELSE                                                          
202000              MOVE 999999999999    TO W-Q3A1-MIN-DALSTORD                 
202100                                      W-Q3B1-MIN-DALSTORD                 
202200            END-IF                                                        
202300          END-IF                                                          
202400        END-IF                                                            
202500                                                                          
202600        MOVE SAVE-TIUTSKR-ENTER(PGNO)    TO W-Q3B1-MIN-DAUTSKR            
202700        IF SAVE-TIUTSKR-ENTER(PGNO)   NOT = ZERO                          
202800          IF SAVE-TIUTSKR-ENTER(PGNO)    < 500000                         
202900            MOVE 20                TO W-Q3B1-MIN-DAUTSKR (1:2)            
203000          ELSE                                                            
203100            IF SAVE-TIUTSKR-ENTER(PGNO)  < 999999                         
203200              MOVE 19              TO W-Q3B1-MIN-DAUTSKR (1:2)            
203300            ELSE                                                          
203400              MOVE 99999999        TO W-Q3B1-MIN-DAUTSKR                  
203500            END-IF                                                        
203600          END-IF                                                          
203700        END-IF                                                            
203800        MOVE SAVE-TIUTSTID-ENTER(PGNO)  TO W-Q3B1-MIN-TIUTSTID            
204301     .                                                                    
204302     EJECT                                                                
204600 S01-LAES-PRC-KANAL SECTION.                                              
204700                                                                          
204800     MOVE WS-IDDC         TO W-4447-IDDC                                  
204900                                                                          
205000     PERFORM IMS-GU-XXKH-WLXXKH11                                         
205100                                                                          
205200     IF SEGMENT-FINNS                                                     
205300        MOVE XXKH-4448-KVORDER  TO WS-PRC-KVORDER                         
205400        MOVE XXKH-4448-KVRADER  TO WS-PRC-KVRADER                         
205500        MOVE XXKH-4448-KDPRCGRP TO W-4462-KDPRCGRP-X                      
205600     END-IF                                                               
205700     .                                                                    
205800     EJECT                                                                
205900 S02-FLYTTA-TILL-BILD SECTION.                                            
206000     MOVE 'STA S02-FLYTT   '        TO PGMPOS                             
206200     MOVE WS-IDDC TO W-IDDC-B6                                            
206300     PERFORM IMS-GU-WDB601                                                
206400                                                                          
206500     MOVE ORQA-ODEL-DARFS           TO WS-ORQA-ODEL-DARFS                 
206600     MOVE ORQA-ODEL-IDKUNDNR        TO TEST-IDKUNDNR                      
206700     MOVE ORQA-ODEL-IDKUNDNR        TO WS-TEST-IDKUNDNR                   
206800     MOVE ORQA-ODEL-IDDISTR         TO TEST-IDDISTR                       
206900     MOVE ORQA-ODEL-IDDISTR         TO WS-TEST-IDDISTR                    
207000     MOVE 'S02-FLYTTA-POS4 '        TO PGMPOS                             
207100     IF  TEST-IDKUNDNR-PREV NOT =  TEST-IDKUNDNR  OR                      
207200         TEST-IDDISTR-PREV  NOT =  TEST-IDDISTR                           
207300         MOVE 'S02-FLYTTA-POS5 '    TO PGMPOS                             
207400         MOVE TEST-IDKUNDNR         TO TEST-IDKUNDNR-PREV                 
207500         MOVE TEST-IDDISTR          TO TEST-IDDISTR-PREV                  
207600         PERFORM S07-DIST-KUND-LDC                                        
207700     END-IF                                                               
207800                                                                          
207900     IF ((MSGI-IDUSER (1:4)  = 'PHUS') AND                                
208000        (MSGI-KDARBTYP-SEC-4352 (1:2) NOT = 'WS'))                        
208100     OR ((MSGI-IDUSER (1:4)  = 'PHCA') AND                                
208200        (MSGI-KDARBTYP-SEC-4352 (1:2) NOT = 'WS'))                        
208300*    DÖLJ VIKT/VOLYM                                                      
208400       MOVE 'S02-FLYTTA POS3 '      TO PGMPOS                             
208500       MOVE SPACE                   TO MOD-FLORDDEL (IX1)                 
208600                                       MOD-FLIDUSER (IX1)                 
208700                                       WS-TABELLRAD                       
208800       MOVE ORQA-ODEL-IDPRCVAR      TO MOD-IDPRCVAR-RAD (IX1)             
208900       IF ORQA-ODEL-IDUSER NOT = SPACE                                    
209000          IF ORQA-ODEL-IDUSER NOT = WS-IDUSER                             
209100             MOVE '*'          TO MOD-FLIDUSER (IX1)                      
209200             MOVE MFS-STAENG-FAELT                                        
209300                               TO MOD-FLORDDEL-ATTR (IX1)                 
209400          END-IF                                                          
209500       END-IF                                                             
209600       MOVE ORQA-ODEL-IDDISTR  TO MOD-IDDISTR  (IX1)                      
209700       MOVE ORQA-ODEL-IDKUNDNR TO MOD-IDKUNDNR (IX1)                      
209800       MOVE ORQA-ODEL-IDKUNDRF (3:5)                                      
209900                               TO MOD-IDORDNR5 (IX1)                      
210000       INSPECT MOD-IDORDNR5 (IX1) REPLACING LEADING ZERO BY SPACE         
210100                                                                          
210200       MOVE ORQA-ODEL-DALSTORD TO WS-LST-RFS                              
210300       MOVE WS-LST-RFS-TAB     TO WS-LST                                  
210400                                                                          
210500       MOVE ORQA-ODEL-SUPTID   TO WS-P-TID                                
210600                                                                          
210700       MOVE ORQA-ODEL-DARFS    TO WS-LST-RFS                              
210800       MOVE WS-LST-RFS-TAB     TO WS-RFS                                  
210900                                                                          
211000       MOVE ORQA-ODEL-KVRADER  TO WS-RADER                                
211100       MOVE ORQA-ODEL-IDTRP    TO WS-TRP                                  
211200       MOVE WS-ANTPLK          TO WS-PLOCK                                
211300       IF IX1 = 1                                                         
211400          MOVE WS-ANTPLK          TO WS-ANTPLK-ENTER                      
211500          MOVE WS-KVORDER         TO WS-KVORDER-ENTER                     
211600          MOVE WS-KVRADER         TO WS-KVRADER-ENTER                     
211700          MOVE ORQA-ODEL-IDPRODNR TO SAVE-ODEL-IDPRODNR (PGNO)            
211800          MOVE ORQA-ODEL-IDPLKLST TO SAVE-ODEL-IDPLKLST (PGNO)            
211900       END-IF                                                             
212000       ADD 1                   TO WS-KVORDER                              
212100       ADD ORQA-ODEL-KVRADER   TO WS-KVRADER                              
212200       IF WS-KVORDER >= WS-PRC-KVORDER                                    
212300          OR                                                              
212400          WS-KVRADER > WS-PRC-KVRADER                                     
212500          MOVE 0               TO WS-KVORDER                              
212600                                    WS-KVRADER                            
212700          ADD 1                TO WS-ANTPLK                               
212800       END-IF                                                             
212900       MOVE WS-TABELLRAD       TO MOD-BEODEINF (IX1)                      
213000                                                                          
213100       IF ORQA-ODEL-DALSTORD < WS-DALSTORD                                
213200          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLIDUSER-ATTR (IX1)           
213300                                        MOD-IDPRCVAR-ATTR (IX1)           
213400                                        MOD-IDDISTR-ATTR (IX1)            
213500                                        MOD-IDKUNDNR-ATTR (IX1)           
213600                                        MOD-IDORDNR5-ATTR (IX1)           
213700                                        MOD-BEODEINF-ATTR (IX1)           
213800       END-IF                                                             
213900       MOVE ORQA-ODEL-IDPRODNR TO MOD-IDPRODNR-KEY (IX1)                  
214000       MOVE ORQA-ODEL-IDPLKLST TO MOD-IDPLKLST-KEY (IX1)                  
214100     ELSE                                                                 
214200       MOVE 'S02-FLYTTA-POS6 '  TO PGMPOS                                 
214300       IF (DCS-CDC OR DCS-CDC-TR OR DCS-SDC) AND                          
214400           GMT-FLLDCKND = JA               AND                            
214500           WS-ODEL-YYMMDD > WS-DATUM       AND                            
214600           ORQI-OHUV-IDSYSTEM = 'LDC '                                    
214700                                                                          
214800         MOVE 'S02-FLYTTA-POS1 '    TO PGMPOS                             
214900         MOVE 001                   TO WORK-KDCALL                        
215000         MOVE '11'                  TO WORK-IDDC                          
215100         MOVE WS-DATUM              TO WORK-TIAAMMDD-FOM                  
215200         MOVE WS-ODEL-YYMMDD        TO WORK-TIAAMMDD-TOM                  
215300         CALL WORKDAY               USING WORK-KDCALL                     
215400                                             WORK-DATE-AREA               
215500                                             WORK-KDSVAR                  
215600         MOVE 'S02-FLYTTA-POS7 '    TO PGMPOS                             
215700         IF WORK-KDSVAR-OK                                                
215800           CONTINUE                                                       
215900         ELSE                                                             
216000           MOVE 'FEL FRÅN WORKDAY I S02-SECTION.' TO FELTEXT              
216100           CALL FELLOG                                                    
216200         END-IF                                                           
216300       ELSE                                                               
216400         MOVE 000                   TO WORK-KVWORKD                       
216500       END-IF                                                             
216600       MOVE 'S02-FLYTTA-POS8 '      TO PGMPOS                             
216800       IF DCS-SDC                                                         
216900       AND GMT-FLLDCKND = JA                                              
217000       AND WORK-KVWORKD > GMT-KVDAGAR-SDC + 1                             
217100       AND ORQI-OHUV-IDSYSTEM = 'LDC '                                    
217200                                                                          
217300       OR DCS-CDC                                                         
217400       AND GMT-FLLDCKND = JA                                              
217500       AND WORK-KVWORKD > GMT-KVDAGAR-CDC + 1                             
217600       AND ORQI-OHUV-IDSYSTEM = 'LDC '                                    
217700         MOVE 'S02-FLYTTA POS2 '    TO PGMPOS                             
217800         COMPUTE IX1 = IX1 - 1                                            
217900       ELSE                                                               
218000         MOVE 'S02-FLYTTA POS3 '    TO PGMPOS                             
218100         MOVE SPACE                 TO MOD-FLORDDEL (IX1)                 
218200                                         MOD-FLIDUSER (IX1)               
218300                                         WS-TABELLRAD                     
218400         MOVE ORQA-ODEL-IDPRCVAR    TO MOD-IDPRCVAR-RAD (IX1)             
218500         IF ORQA-ODEL-IDUSER NOT = SPACE                                  
218600            IF ORQA-ODEL-IDUSER NOT = WS-IDUSER                           
218700               MOVE '*'        TO MOD-FLIDUSER (IX1)                      
218800               MOVE MFS-STAENG-FAELT                                      
218900                                 TO MOD-FLORDDEL-ATTR (IX1)               
219000            END-IF                                                        
219100         END-IF                                                           
219200         MOVE ORQA-ODEL-IDDISTR TO MOD-IDDISTR (IX1)                      
219300         MOVE ORQA-ODEL-IDKUNDNR TO MOD-IDKUNDNR (IX1)                    
219400         MOVE ORQA-ODEL-IDKUNDRF (3:5)                                    
219500                                 TO MOD-IDORDNR5 (IX1)                    
219600        INSPECT MOD-IDORDNR5 (IX1) REPLACING LEADING ZERO BY SPACE        
219700                                                                          
219800         MOVE ORQA-ODEL-DALSTORD TO WS-LST-RFS                            
219900         MOVE WS-LST-RFS-TAB   TO WS-LST                                  
220000                                                                          
220100         MOVE ORQA-ODEL-SUPTID TO WS-P-TID                                
220200                                                                          
220300         MOVE ORQA-ODEL-DARFS  TO WS-LST-RFS                              
220400         MOVE WS-LST-RFS-TAB   TO WS-RFS                                  
220500                                                                          
220600         MOVE ORQA-ODEL-KVRADER TO WS-RADER                               
220700         IF US-MATT                                                       
220800            COMPUTE WS-VIKT = ORQA-ODEL-VKORDNTO                          
220900                            * CONV-KG-TO-LB                               
221000            COMPUTE WS-VOLYM = ORQA-ODEL-VLORDNTO                         
221100                             * CONV-M3-TO-FT3                             
221200         ELSE                                                             
221300           MOVE ORQA-ODEL-VKORDNTO TO WS-VIKT                             
221400           MOVE ORQA-ODEL-VLORDNTO TO WS-VOLYM                            
221500         END-IF                                                           
221600         MOVE ORQA-ODEL-IDTRP  TO WS-TRP                                  
221700         MOVE WS-ANTPLK        TO WS-PLOCK                                
221800         IF IX1 = 1                                                       
221900            MOVE WS-ANTPLK          TO WS-ANTPLK-ENTER                    
222000            MOVE WS-KVORDER         TO WS-KVORDER-ENTER                   
222100            MOVE WS-KVRADER         TO WS-KVRADER-ENTER                   
222200            MOVE ORQA-ODEL-IDPRODNR TO SAVE-ODEL-IDPRODNR (PGNO)          
222300            MOVE ORQA-ODEL-IDPLKLST TO SAVE-ODEL-IDPLKLST (PGNO)          
222400         END-IF                                                           
222500         ADD 1                 TO WS-KVORDER                              
222600         ADD ORQA-ODEL-KVRADER TO WS-KVRADER                              
222700         IF WS-KVORDER >= WS-PRC-KVORDER                                  
222800            OR                                                            
222900            WS-KVRADER > WS-PRC-KVRADER                                   
223000            MOVE 0             TO WS-KVORDER                              
223100                                      WS-KVRADER                          
223200            ADD 1              TO WS-ANTPLK                               
223300         END-IF                                                           
223400         MOVE WS-TABELLRAD     TO MOD-BEODEINF (IX1)                      
223600         IF ORQA-ODEL-DALSTORD < WS-DALSTORD                              
223700            MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLIDUSER-ATTR (IX1)         
223800                                          MOD-IDPRCVAR-ATTR (IX1)         
223900                                          MOD-IDDISTR-ATTR (IX1)          
224000                                          MOD-IDKUNDNR-ATTR (IX1)         
224100                                          MOD-IDORDNR5-ATTR (IX1)         
224200                                          MOD-BEODEINF-ATTR (IX1)         
224300         END-IF                                                           
224400         MOVE ORQA-ODEL-IDPRODNR TO MOD-IDPRODNR-KEY (IX1)                
224500         MOVE ORQA-ODEL-IDPLKLST TO MOD-IDPLKLST-KEY (IX1)                
224600       END-IF                                                             
224700     END-IF                                                               
224800     .                                                                    
224900     EJECT                                                                
225000 S03-LAES-NASTA-ORDERDEL SECTION.                                         
225100                                                                          
225200     MOVE NEJ TO LAES-SW                                                  
225300                                                                          
225400     PERFORM UNTIL LAES-OK                                                
225500        IF PRC                                                            
225600*          PERFORM IMS-GN-WDQ3BSEQ-WLORQA01                               
225700           IF EXTEND-SW = NEJ                                             
225800              PERFORM IMS-GN-WDQ3B1                                       
225900           ELSE                                                           
226000              PERFORM IMS-GN-WDQ3B1-DISTR-KUND                            
226100           END-IF                                                         
226200           IF SEGMENT-FINNS                                               
226300              MOVE Q3B1-SEQB-IDORDER  TO W-Q301KY-IDORDER                 
226400              MOVE Q3B1-SEQB-IDDC     TO W-Q301KY-IDDC                    
226500              MOVE Q3B1-SEQB-IDPRODNR TO W-Q301KY-IDPRODNR                
226600              MOVE Q3B1-SEQB-IDPLKLST TO W-Q301KY-IDPLKLST                
226700              PERFORM IMS-GU-ORQ-WLORQA01                                 
226800           END-IF                                                         
226900        ELSE                                                              
227000*          PERFORM IMS-GN-WDQ3ASEQ-WLORQA01                               
227100           IF EXTEND-SW = NEJ                                             
227200              PERFORM IMS-GN-WDQ3A1                                       
227300           ELSE                                                           
227400              PERFORM IMS-GN-WDQ3A1-DISTR-KUND                            
227500           END-IF                                                         
227600           IF SEGMENT-FINNS                                               
227700              MOVE Q3A1-SEQA-IDORDER  TO W-Q301KY-IDORDER                 
227800              MOVE Q3A1-SEQA-IDDC     TO W-Q301KY-IDDC                    
227900              MOVE Q3A1-SEQA-IDPRODNR TO W-Q301KY-IDPRODNR                
228000              MOVE Q3A1-SEQA-IDPLKLST TO W-Q301KY-IDPLKLST                
228100              PERFORM IMS-GU-ORQ-WLORQA01                                 
228200           END-IF                                                         
228300        END-IF                                                            
228400        IF SEGMENT-SAKNAS OR END-OF-DATA                                  
228500           MOVE JA TO LAES-SW                                             
228600        END-IF                                                            
228700        IF SEGMENT-FINNS                                                  
228800           MOVE ORQA-ODEL-IDORDER TO W-IDORDER                            
228900           PERFORM IMS-GHU-ORQI-WLORQI01                                  
229000           IF SEGMENT-FINNS                                               
229100              IF ORQI-OHUV-FLKLAR = JA                                    
229200                 MOVE JA TO LAES-SW                                       
229300              END-IF                                                      
229400           END-IF                                                         
229500        END-IF                                                            
229600     END-PERFORM                                                          
229700     .                                                                    
229800     EJECT                                                                
229900 S04-FYLL-BILD SECTION.                                                   
230100     MOVE ORQA-ODEL-IDTRP           TO MOD-IDTRP-ENTER                    
230200                                       MOD-IDTRP-NEXT                     
230300                                       SAVE-IDTRP-ENTER(PGNO)             
230400     MOVE ORQA-ODEL-DATRPAVD (3:6)  TO MOD-TIAAMMDD-ENTER                 
230500                                       SAVE-TIAAMMDD-ENTER(PGNO)          
230600     MOVE ORQA-ODEL-TIHHMM          TO MOD-TIHHMM-ENTER                   
230700                                       SAVE-TIHHMM-ENTER(PGNO)            
230800     MOVE ORQA-ODEL-DARFS (3:10)    TO MOD-TIRFS-ENTER                    
230900                                       SAVE-TIRFS-ENTER(PGNO)             
231000     MOVE ORQA-ODEL-DAUTSKR (3:6)   TO MOD-TIUTSKR-ENTER                  
231100                                       SAVE-TIUTSKR-ENTER(PGNO)           
231200     MOVE ORQA-ODEL-DALSTORD (3:10) TO MOD-TILST-O-ENTER                  
231300                                       SAVE-TILST-O-ENTER(PGNO)           
231400     MOVE ORQA-ODEL-TIUTSTID        TO MOD-TIUTSTID-ENTER                 
231500                                       SAVE-TIUTSTID-ENTER(PGNO)          
231700     MOVE 1    TO IX1                                                     
231800     MOVE ZERO TO W-ANTAL-MOD-RADER                                       
231900     PERFORM UNTIL IX1 > MAX-IX                                           
232000       IF SEGMENT-FINNS                                                   
232100          PERFORM S02-FLYTTA-TILL-BILD                                    
232200          PERFORM S03-LAES-NASTA-ORDERDEL                                 
232300          MOVE IX1 TO W-ANTAL-MOD-RADER                                   
232400       ELSE                                                               
232500          MOVE MFS-RENSA-FAELT TO MOD-IDPRCVAR-RAD (IX1)                  
232600                                  MOD-FLIDUSER     (IX1)                  
232700                                  MOD-IDDISTR      (IX1)                  
232800                                  MOD-IDKUNDNR     (IX1)                  
232900                                  MOD-IDORDNR5     (IX1)                  
233000                                  MOD-BEODEINF     (IX1)                  
233100       END-IF                                                             
233200         ADD 1 TO IX1                                                     
233300     END-PERFORM                                                          
233400                                                                          
233500     IF SEGMENT-FINNS AND IX1 < 14                                        
233600        MOVE ORQA-ODEL-IDTRP           TO MOD-IDTRP-NEXT                  
233700        MOVE ORQA-ODEL-DATRPAVD (3:6)  TO MOD-TIAAMMDD-NEXT               
233800        MOVE ORQA-ODEL-TIHHMM          TO MOD-TIHHMM-NEXT                 
233900        MOVE ORQA-ODEL-DARFS (3:10)    TO MOD-TIRFS-NEXT                  
234000        MOVE ORQA-ODEL-DAUTSKR (3:6)   TO MOD-TIUTSKR-NEXT                
234100        MOVE ORQA-ODEL-DALSTORD (3:10) TO MOD-TILST-O-NEXT                
234200        MOVE ORQA-ODEL-TIUTSTID        TO MOD-TIUTSTID-NEXT               
234300        MOVE ORQA-ODEL-IDPRODNR        TO MOD-IDPRODNR-KEY (12)           
234400        MOVE ORQA-ODEL-IDPLKLST        TO MOD-IDPLKLST-KEY (12)           
234500        IF MID-FLKLAR = NEJ                                               
234600*105 MER INFORMATION FINNS, TRYCK PF8                                     
234700           MOVE '105'               TO MED-IDMFSINF                       
234800           PERFORM S06-INFO-MEDDELANDE                                    
234900        END-IF                                                            
235000     ELSE                                                                 
235100        MOVE MAX-VARDE           TO MOD-TIAAMMDD-NEXT                     
235200                                    MOD-TIHHMM-NEXT                       
235300                                    MOD-TIRFS-NEXT                        
235400                                    MOD-TIUTSKR-NEXT                      
235500                                    MOD-TILST-O-NEXT                      
235600                                    MOD-TIUTSTID-NEXT                     
235700                                    MOD-IDPRODNR-KEY (12)                 
235800                                    MOD-IDPLKLST-KEY (12)                 
235900        IF MID-FLKLAR = NEJ                                               
236000*106 DETTA ÄR SISTA SIDAN                                                 
236100             MOVE '106'             TO MED-IDMFSINF                       
236200             PERFORM S06-INFO-MEDDELANDE                                  
236300           MOVE MFS-RENSA-FAELT     TO MOD-TEMFSFEL                       
236400        END-IF                                                            
236500     END-IF                                                               
236600                                                                          
236700     PERFORM S08-KOLLA-FLER-ORDER-PA-KUND                                 
236701                                                                          
236800     .                                                                    
236900     EJECT                                                                
237000 S05-FEL-MEDDELANDE  SECTION.                                             
237100                                                                          
237200     CALL WMEDKONV USING MED-WMEDAREA                                     
237300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
237400     .                                                                    
237500                                                                          
237600                                                                          
237700 S06-INFO-MEDDELANDE  SECTION.                                            
237800                                                                          
237900     CALL WMEDKONV USING MED-WMEDAREA                                     
238000     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
238100     .                                                                    
238200     SKIP2                                                                
238300 S07-DIST-KUND-LDC SECTION.                                               
238400                                                                          
238500     MOVE  TEST-IDDISTR       TO  W-WDB2-IDDISTR                          
238600     MOVE  TEST-IDKUNDNR      TO  W-WDB2-IDKUNDNR                         
238700                                                                          
238800     PERFORM IMS-GU-WDB201                                                
238900                                                                          
239000     IF SEGMENT-SAKNAS                                                    
239100        MOVE NEJ              TO  GMT-FLLDCKND                            
239200     END-IF                                                               
239300                                                                          
239400     .                                                                    
239500     SKIP2                                                                
239600 S08-KOLLA-FLER-ORDER-PA-KUND   SECTION.                                  
239700                                                                          
239800*  KOLLA PÅ SAMMA SIDA                                                    
239900     MOVE 1 TO IX1                                                        
240000     PERFORM UNTIL IX1 > W-ANTAL-MOD-RADER                                
240100*    PERFORM UNTIL IX1 > MAX-IX                                           
240200*               OR (MOD-IDDISTR(IX1) = SPACE OR ZERO)                     
240300*               OR MOD-IDDISTR(IX1) = EXP-IDDISTR(EXP-IX)                 
240400                                                                          
240500        MOVE 1 TO EXP-IX                                                  
240600        PERFORM UNTIL EXP-IDDISTR(EXP-IX) = SPACE                         
240700                  OR (EXP-IDDISTR(EXP-IX) = MOD-IDDISTR(IX1)              
240800                  AND EXP-IDKUNDNR(EXP-IX) = MOD-IDKUNDNR(IX1))           
240900           ADD +1 TO EXP-IX                                               
241000        END-PERFORM                                                       
241100                                                                          
241200        IF MOD-IDDISTR(IX1)  = EXP-IDDISTR(EXP-IX) AND                    
241300           MOD-IDKUNDNR(IX1) = EXP-IDKUNDNR(EXP-IX)                       
241400           MOVE '*' TO EXP-FLER(EXP-IX)                                   
241500        ELSE                                                              
241600           MOVE MOD-IDDISTR(IX1)  TO EXP-IDDISTR(EXP-IX)                  
241700           MOVE MOD-IDKUNDNR(IX1) TO EXP-IDKUNDNR(EXP-IX)                 
241800           MOVE MOD-IDDISTR(IX1)  TO EXP-IDDISTR-NUM(EXP-IX)              
241900           MOVE MOD-IDKUNDNR(IX1) TO EXP-IDKUNDNR-NUM(EXP-IX)             
242000        END-IF                                                            
242100        ADD +1 TO IX1                                                     
242200     END-PERFORM                                                          
242300                                                                          
242400*  KOLLA PÅ KOMMANDE SIDOR                                                
242500     MOVE 1 TO EXP-IX                                                     
242600     PERFORM UNTIL EXP-IX > MAX-IX                                        
242700                OR (EXP-IDDISTR(EXP-IX) = SPACE OR ZERO)                  
242800                                                                          
242900        IF EXP-FLER(EXP-IX) = SPACE                                       
243000           MOVE EXP-IDDISTR-NUM(EXP-IX)  TO SPAR-IDDISTR                  
243100           MOVE EXP-IDKUNDNR-NUM(EXP-IX) TO SPAR-IDKUNDNR                 
243200           IF PRC                                                         
243300              PERFORM IMS-GU-WDQ3B1-DISTR-KUND                            
243400              PERFORM IMS-GN-WDQ3B1-DISTR-KUND                            
243500              IF SEGMENT-FINNS                                            
243600                 MOVE '*' TO EXP-FLER(EXP-IX)                             
243700              END-IF                                                      
243800           ELSE                                                           
243900              PERFORM IMS-GU-WDQ3A1-DISTR-KUND                            
244000              PERFORM IMS-GN-WDQ3A1-DISTR-KUND                            
244100              IF SEGMENT-FINNS                                            
244200                 MOVE '*' TO EXP-FLER(EXP-IX)                             
244300              END-IF                                                      
244400           END-IF                                                         
244500        END-IF                                                            
244600                                                                          
244700        ADD +1 TO EXP-IX                                                  
244800     END-PERFORM                                                          
244900                                                                          
245000*  MARKERA KUNDER MED FLERA ORDER I MODEN                                 
245100     MOVE 1 TO EXP-IX                                                     
245200     PERFORM UNTIL EXP-IX > MAX-IX                                        
245300                OR (EXP-IDDISTR(EXP-IX) = SPACE OR ZERO)                  
245400                                                                          
245500        IF EXP-FLER(EXP-IX) = '*'                                         
245600           MOVE +1 TO IX1                                                 
245700           PERFORM UNTIL IX1 > MAX-IX                                     
245800                      OR (MOD-IDDISTR(IX1)  = EXP-IDDISTR(EXP-IX)         
245900                    AND  MOD-IDKUNDNR(IX1) = EXP-IDKUNDNR(EXP-IX))        
246000              ADD +1 TO IX1                                               
246100           END-PERFORM                                                    
246200        END-IF                                                            
246300        IF IX1 > MAX-IX                                                   
246400          CONTINUE                                                        
246500        ELSE                                                              
246600          IF MOD-IDDISTR(IX1)  = EXP-IDDISTR(EXP-IX) AND                  
246700             MOD-IDKUNDNR(IX1) = EXP-IDKUNDNR(EXP-IX)                     
246800                                                                          
246900             MOVE '*'  TO MOD-FLFLER(IX1)                                 
247000          END-IF                                                          
247100        END-IF                                                            
247200        ADD +1 TO EXP-IX                                                  
247300     END-PERFORM                                                          
247400     .                                                                    
247500     EJECT                                                                
247501 S09-GET-WDQ3B1-A1 SECTION.                                               
247502                                                                          
247503     IF PRC                                                               
247504        PERFORM IMS-GU-WDQ3B1                                             
247505        IF SEGMENT-FINNS                                                  
247506           MOVE Q3B1-SEQB-IDORDER  TO W-Q301KY-IDORDER                    
247507           MOVE Q3B1-SEQB-IDDC     TO W-Q301KY-IDDC                       
247508           MOVE Q3B1-SEQB-IDPRODNR TO W-Q301KY-IDPRODNR                   
247509           MOVE Q3B1-SEQB-IDPLKLST TO W-Q301KY-IDPLKLST                   
247510           PERFORM IMS-GU-ORQ-WLORQA01                                    
247511        END-IF                                                            
247512     ELSE                                                                 
247513        PERFORM IMS-GU-WDQ3A1                                             
247514        IF SEGMENT-FINNS                                                  
247515           MOVE Q3A1-SEQA-IDORDER  TO W-Q301KY-IDORDER                    
247516           MOVE Q3A1-SEQA-IDDC     TO W-Q301KY-IDDC                       
247517           MOVE Q3A1-SEQA-IDPRODNR TO W-Q301KY-IDPRODNR                   
247518           MOVE Q3A1-SEQA-IDPLKLST TO W-Q301KY-IDPLKLST                   
247519           PERFORM IMS-GU-ORQ-WLORQA01                                    
247520        END-IF                                                            
247521     END-IF                                                               
247522                                                                          
247523     IF SEGMENT-FINNS                                                     
247524        PERFORM S01-LAES-PRC-KANAL                                        
247525        IF SEGMENT-FINNS                                                  
247526           PERFORM S04-FYLL-BILD                                          
247527        ELSE                                                              
247528*023 TABELL SAKNAS                                                        
247529           MOVE '023' TO MED-IDMFSFEL                                     
247530           PERFORM S05-FEL-MEDDELANDE                                     
247531           PERFORM MFS-RENSA-FAELT-UT                                     
247532        END-IF                                                            
247533     ELSE                                                                 
247534*115 SISTA SIDAN REDAN VISAD                                              
247535        MOVE JA TO LAST-SW                                                
247536        MOVE '115' TO MED-IDMFSFEL                                        
247537        PERFORM S05-FEL-MEDDELANDE                                        
247538        PERFORM MFS-RENSA-FAELT-UT                                        
247539        MOVE MAX-VARDE           TO MOD-TIAAMMDD-NEXT                     
247540                                    MOD-TIHHMM-NEXT                       
247541                                    MOD-TIRFS-NEXT                        
247542                                    MOD-TIUTSKR-NEXT                      
247543                                    MOD-TILST-O-NEXT                      
247544                                    MOD-TIUTSTID-NEXT                     
247545                                    SAVE-TIAAMMDD-ENTER(PGNO)             
247546                                    SAVE-TIHHMM-ENTER(PGNO)               
247547                                    SAVE-TIRFS-ENTER(PGNO)                
247548                                    SAVE-TIUTSKR-ENTER(PGNO)              
247549                                    SAVE-TILST-O-ENTER(PGNO)              
247550                                    SAVE-TIUTSTID-ENTER(PGNO)             
247551     END-IF                                                               
247552     .                                                                    
247553     EJECT                                                                
247554 S10-SAVE-KEYS  SECTION.                                                  
247555                                                                          
247556     MOVE WS-KVORDER-ENTER         TO SAVE-KVORDER-ENTER(PGNO)            
247557     MOVE WS-KVRADER-ENTER         TO SAVE-KVRADER-ENTER(PGNO)            
247558     MOVE WS-ANTPLK-ENTER          TO SAVE-IDPLKLST-ENTER(PGNO)           
247559     MOVE MSG-SIGNON-USERID        TO MSGI-IDUSER                         
247560     MOVE MSG-LTERM-NAME           TO MSGI-IDLTERM-USER                   
247561     MOVE '4352'                   TO MSGI-IDTRANS                        
247562     MOVE '4352'                   TO SAVE-IDTRANS                        
247563     MOVE '002'                    TO MSGI-KDCALL                         
247564     MOVE SAVE-AREA                TO MSGI-SPAR-AREA                      
247565     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
247566                                                                          
247567     MOVE WS-KVORDER               TO MOD-KVORDER-NEXT                    
247568     MOVE WS-KVRADER               TO MOD-KVRADER-NEXT                    
247569     MOVE WS-ANTPLK                TO MOD-IDPLKLST-NEXT                   
247570     MOVE W-4448-IDPRC             TO MOD-IDPRC-SPAR                      
247571     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
247572     PERFORM IMS-ISRT-MSG                                                 
247573     .                                                                    
247580     EJECT                                                                
247610 MFS-RENSA-FAELT-UT SECTION.                                              
247620                                                                          
247800*    --- ALLA UTDATA-FÄLT                                                 
247900                                                                          
248000     MOVE MFS-RENSA-FAELT   TO MOD-IDTRP-NEXT                             
248701                               MOD-TIAAMMDD-NEXT                          
248702                               MOD-TIHHMM-NEXT                            
248703                               MOD-TIRFS-NEXT                             
248704                               MOD-TIUTSKR-NEXT                           
248705                               MOD-TILST-O-NEXT                           
248706                               MOD-TIUTSTID-NEXT                          
248707                               MOD-KDPRT-PU                               
248800                               MOD-KDPRT-PLE                              
249600                                                                          
249601                                                                          
249602                                                                          
249603                                                                          
249604                                                                          
249605                                                                          
249606                                                                          
249607                                                                          
249700     MOVE 1 TO IX1                                                        
249800     PERFORM UNTIL IX1 > MAX-IX                                           
249900        MOVE MFS-RENSA-FAELT TO MOD-IDPRCVAR-RAD (IX1)                    
250000                                MOD-FLIDUSER     (IX1)                    
250100                                MOD-IDDISTR      (IX1)                    
250200                                MOD-IDKUNDNR     (IX1)                    
250300                                MOD-IDORDNR5     (IX1)                    
250400                                MOD-BEODEINF     (IX1)                    
250500                                MOD-IDPRODNR-KEY (IX1)                    
250600                                MOD-IDPLKLST-KEY (IX1)                    
250700        ADD 1 TO IX1                                                      
250800     END-PERFORM                                                          
250900     .                                                                    
251000     SKIP2                                                                
251100 MFS-RENSA-FAELT-IN SECTION.                                              
251200                                                                          
251300*    --- ALLA INDATA-FÄLT                                                 
251400                                                                          
251500     MOVE MFS-RENSA-FAELT TO   MOD-KDPRT-PU                               
251600                               MOD-KDPRT-PLE                              
251700     MOVE 1 TO IX1                                                        
251800     PERFORM UNTIL IX1 > MAX-IX                                           
251900        MOVE MFS-RENSA-FAELT TO MOD-FLORDDEL  (IX1)                       
252000        ADD 1 TO IX1                                                      
252100     END-PERFORM                                                          
252200     .                                                                    
252300     EJECT                                                                
252400 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
252500                                                                          
252600*    --- ALLA UTDATA-FÄLT                                                 
252700                                                                          
252800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDTRP-NEXT                             
253501                               MOD-TIAAMMDD-NEXT                          
253502                               MOD-TIHHMM-NEXT                            
253503                               MOD-TIRFS-NEXT                             
253504                               MOD-TIUTSKR-NEXT                           
253505                               MOD-TILST-O-NEXT                           
253506                               MOD-TIUTSTID-NEXT                          
253507                               MOD-KDPRT-PU                               
253600                               MOD-KDPRT-PLE                              
254500     MOVE 1 TO IX1                                                        
254600     PERFORM UNTIL IX1 > MAX-IX                                           
254700        MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRCVAR-RAD (IX1)                  
254800                                  MOD-IDDISTR      (IX1)                  
254900                                  MOD-IDKUNDNR     (IX1)                  
255000                                  MOD-IDORDNR5     (IX1)                  
255100                                  MOD-BEODEINF     (IX1)                  
255200                                  MOD-IDPRODNR-KEY (IX1)                  
255300                                  MOD-IDPLKLST-KEY (IX1)                  
255400        ADD 1 TO IX1                                                      
255500     END-PERFORM                                                          
255600     .                                                                    
255700     SKIP2                                                                
255800 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
255900                                                                          
256000*    --- ALLA INDATA-FÄLT                                                 
256100                                                                          
256200     MOVE MFS-ROER-EJ-FAELT TO MOD-FLKLAR                                 
256300                               MOD-KDPRT-PU                               
256400                               MOD-KDPRT-PLE                              
256500                                                                          
256600     MOVE 1 TO IX1                                                        
256700     PERFORM UNTIL IX1 > MAX-IX                                           
256800        MOVE MFS-ROER-EJ-FAELT TO MOD-FLORDDEL  (IX1)                     
256900        ADD 1 TO IX1                                                      
257000     END-PERFORM                                                          
257100     .                                                                    
257200     EJECT                                                                
257300 MFS-LAS-IN-IGEN SECTION.                                                 
257400                                                                          
257500*    --- ALLA INDATA-FÄLT                                                 
257600                                                                          
257700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKLAR-ATTR                        
257800                                   MOD-KDPRT-PU-ATTR                      
257900                                   MOD-KDPRT-PLE-ATTR                     
258000                                                                          
258100     MOVE 1 TO IX1                                                        
258200     PERFORM UNTIL IX1 > MAX-IX                                           
258300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLORDDEL-ATTR  (IX1)            
258400                                      MOD-IDPRCVAR-ATTR  (IX1)            
258500                                      MOD-FLIDUSER-ATTR  (IX1)            
258600                                      MOD-IDDISTR-ATTR   (IX1)            
258700                                      MOD-IDKUNDNR-ATTR  (IX1)            
258800                                      MOD-IDORDNR5-ATTR  (IX1)            
258900                                      MOD-BEODEINF-ATTR  (IX1)            
259000        ADD 1 TO IX1                                                      
259100     END-PERFORM                                                          
259200     .                                                                    
259300     EJECT                                                                
259400* --- IMS SEKTIONER ---                                                   
259500     SKIP3                                                                
259600 IMS-GU-MSG SECTION.                                                      
259700                                                                          
259800     MOVE '  QC' TO GODK-STATUSKODER                                      
259900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
260000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
260100     PERFORM IMS-STATUSKONTROLL                                           
260200     .                                                                    
260300     SKIP3                                                                
260400 IMS-ISRT-MSG SECTION.                                                    
260500                                                                          
260600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
260700        MOVE '0' TO MFS-KDHUVOMR                                          
260800     END-IF                                                               
260900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
261000     MOVE SPACE TO GODK-STATUSKODER                                       
261100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
261200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
261300     PERFORM IMS-STATUSKONTROLL                                           
261400     .                                                                    
261500     SKIP3                                                                
261600 IMS-ISRT-MSG-ALT SECTION.                                                
261700                                                                          
261800     MOVE SPACE TO GODK-STATUSKODER                                       
261900     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
262000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
262100     PERFORM IMS-STATUSKONTROLL                                           
262200     .                                                                    
262300     EJECT                                                                
262400 IMS-GHU-ORQI-WLORQI01 SECTION.                                           
262500                                                                          
262600     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
262700          DELIMITED BY SIZE INTO SSA1                                     
262800     MOVE '  GE' TO GODK-STATUSKODER                                      
262900     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA5 SSA1                    
263000     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
263100     PERFORM IMS-STATUSKONTROLL                                           
263200     .                                                                    
263300     EJECT                                                                
263400 IMS-GU-WDQ3DSEQ-WLORQA01 SECTION.                                        
263500                                                                          
263600     STRING 'WLORQA01(WDQ3DSEQ =' W-WDQ3DSEQ-X ')'                        
263700          DELIMITED BY SIZE INTO SSA1                                     
263800     MOVE '  GE' TO GODK-STATUSKODER                                      
263900     CALL CBLTDLI USING GU ORQD-PCB DLI-IO-AREA2 SSA1                     
264000     MOVE ORQD-STATUS-CODE TO STATUS-WS                                   
264100     PERFORM IMS-STATUSKONTROLL                                           
264200     .                                                                    
264300                                                                          
264400 IMS-GN-WDQ3DSEQ-WLORQA01 SECTION.                                        
264500                                                                          
264600     STRING 'WLORQA01(WDQ3DSEQ =' W-WDQ3DSEQ-X ')'                        
264700          DELIMITED BY SIZE INTO SSA1                                     
264800     MOVE '  GE' TO GODK-STATUSKODER                                      
264900     CALL CBLTDLI USING GN ORQD-PCB DLI-IO-AREA2 SSA1                     
265000     MOVE ORQD-STATUS-CODE TO STATUS-WS                                   
265100     PERFORM IMS-STATUSKONTROLL                                           
265200     .                                                                    
265300                                                                          
265400                                                                          
265500 IMS-GHU-ORQ-WLORQA01 SECTION.                                            
265600                                                                          
265700     STRING 'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                        
265800          DELIMITED BY SIZE INTO SSA1                                     
265900     MOVE '  GE' TO GODK-STATUSKODER                                      
266000     CALL CBLTDLI USING GHU ORQ-PCB DLI-IO-AREA2 SSA1                     
266100     MOVE ORQ-STATUS-CODE TO STATUS-WS                                    
266200     PERFORM IMS-STATUSKONTROLL                                           
266300     .                                                                    
266400                                                                          
266500 IMS-GU-ORQ-WLORQA01 SECTION.                                             
266600                                                                          
266700     STRING 'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                        
266800          DELIMITED BY SIZE INTO SSA1                                     
266900     MOVE '  ' TO GODK-STATUSKODER                                        
267000     CALL CBLTDLI USING GU ORQ-PCB DLI-IO-AREA2 SSA1                      
267100     MOVE ORQ-STATUS-CODE TO STATUS-WS                                    
267200     PERFORM IMS-STATUSKONTROLL                                           
267300     .                                                                    
267400                                                                          
267500                                                                          
267600 IMS-GN-WDQ3B1 SECTION.                                                   
267700                                                                          
267800     STRING 'WDQ3B1  (WDQ3B1KY>=' W-WDQ3B1-MIN-X                          
267900                    '&WDQ3B1KY<=' W-WDQ3B1-MAX-X                          
268000                    '&IDPRCVAR>=' W-Q3B1-MIN-IDPRCVAR                     
268100                    '&IDPRCVAR<=' W-Q3B1-MAX-IDPRCVAR ')'                 
268200          DELIMITED BY SIZE INTO SSA1                                     
268300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
268400     CALL CBLTDLI USING GN Q3B1-PCB DLI-IO-Q3B1 SSA1                      
268500     MOVE Q3B1-STATUS-CODE TO STATUS-WS                                   
268600     PERFORM IMS-STATUSKONTROLL                                           
268700     .                                                                    
268800                                                                          
268900 IMS-GN-WDQ3B1-DISTR-KUND SECTION.                                        
269000                                                                          
269100     STRING 'WDQ3B1  (WDQ3B1KY>=' W-WDQ3B1-MIN-X                          
269200                    '&WDQ3B1KY<=' W-WDQ3B1-MAX-X                          
269300                    '&IDDISTR  =' SPAR-IDDISTR-X                          
269400                    '&IDKUNDNR =' SPAR-IDKUNDNR-X                         
269500                    '&IDPRCVAR>=' W-Q3B1-MIN-IDPRCVAR                     
269600                    '&IDPRCVAR<=' W-Q3B1-MAX-IDPRCVAR ')'                 
269700          DELIMITED BY SIZE INTO SSA1                                     
269800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
269900     CALL CBLTDLI USING GN Q3B1-PCB DLI-IO-Q3B1 SSA1                      
270000     MOVE Q3B1-STATUS-CODE TO STATUS-WS                                   
270100     PERFORM IMS-STATUSKONTROLL                                           
270200     .                                                                    
270300                                                                          
270400*IMS-GN-WDQ3BSEQ-WLORQA01 SECTION.                                        
270500*                                                                         
270600*    STRING 'WLORQA01(WDQ3BSEQ>=' W-WDQ3BSEQ-MIN-X                        
270700*                   '&WDQ3BSEQ<=' W-WDQ3BSEQ-MAX-X                        
270800*                   '&IDPRCVAR>=' W-Q3BSEQ-MIN-IDPRCVAR                   
270900*                   '&IDPRCVAR<=' W-Q3BSEQ-MAX-IDPRCVAR ')'               
271000*         DELIMITED BY SIZE INTO SSA1                                     
271100*    MOVE '  GBGE' TO GODK-STATUSKODER                                    
271200*    CALL CBLTDLI USING GN ORQB-PCB DLI-IO-AREA2 SSA1                     
271300*    MOVE ORQB-STATUS-CODE TO STATUS-WS                                   
271400*    PERFORM IMS-STATUSKONTROLL                                           
271500*    .                                                                    
271600                                                                          
271700 IMS-GU-WDQ3B1 SECTION.                                                   
271800                                                                          
271900     STRING 'WDQ3B1  (WDQ3B1KY>=' W-WDQ3B1-MIN-X                          
272000                    '&WDQ3B1KY<=' W-WDQ3B1-MAX-X                          
272100                    '&IDPRCVAR>=' W-Q3B1-MIN-IDPRCVAR                     
272200                    '&IDPRCVAR<=' W-Q3B1-MAX-IDPRCVAR                     
272300                    '&IDPRODNR= ' W-IDPRODNR-SOEK-X                       
272400                    '&IDPLKLST= ' W-IDPLKLST-SOEK-X ')'                   
272500          DELIMITED BY SIZE INTO SSA1                                     
272600     MOVE '  GE' TO GODK-STATUSKODER                                      
272700     CALL CBLTDLI USING GU Q3B1-PCB DLI-IO-Q3B1 SSA1                      
272800     MOVE Q3B1-STATUS-CODE TO STATUS-WS                                   
272900     PERFORM IMS-STATUSKONTROLL                                           
273000     .                                                                    
273100                                                                          
273200 IMS-GU-WDQ3B1-DISTR-KUND SECTION.                                        
273300                                                                          
273400     STRING 'WDQ3B1  (WDQ3B1KY>=' W-WDQ3B1-MIN-X                          
273500                    '&WDQ3B1KY<=' W-WDQ3B1-MAX-X                          
273600                    '&IDDISTR  =' SPAR-IDDISTR-X                          
273700                    '&IDKUNDNR =' SPAR-IDKUNDNR-X ')'                     
273800*    STRING 'WDQ3B1  (WDQ3B1KY>=' W-WDQ3B1-MIN-X                          
273900*                   '&WDQ3B1KY<=' W-WDQ3B1-MAX-X                          
274000*                   '&IDDISTR  =' SPAR-IDDISTR-X                          
274100*                   '&IDKUNDNR =' SPAR-IDKUNDNR-X                         
274200*                   '&IDPRCVAR>=' W-Q3B1-MIN-IDPRCVAR                     
274300*                   '&IDPRCVAR<=' W-Q3B1-MAX-IDPRCVAR                     
274400*                   '&IDPRODNR= ' W-IDPRODNR-SOEK-X                       
274500*                   '&IDPLKLST= ' W-IDPLKLST-SOEK-X ')'                   
274600          DELIMITED BY SIZE INTO SSA1                                     
274700     MOVE '  GE' TO GODK-STATUSKODER                                      
274800     CALL CBLTDLI USING GU Q3B1-PCB DLI-IO-Q3B1 SSA1                      
274900     MOVE Q3B1-STATUS-CODE TO STATUS-WS                                   
275000     PERFORM IMS-STATUSKONTROLL                                           
275100     .                                                                    
275200                                                                          
275300*IMS-GU-WDQ3BSEQ-WLORQA01 SECTION.                                        
275400*                                                                         
275500*    STRING 'WLORQA01(WDQ3BSEQ>=' W-WDQ3BSEQ-MIN-X                        
275600*                   '&WDQ3BSEQ<=' W-WDQ3BSEQ-MAX-X                        
275700*                   '&IDPRCVAR>=' W-Q3BSEQ-MIN-IDPRCVAR                   
275800*                   '&IDPRCVAR<=' W-Q3BSEQ-MAX-IDPRCVAR                   
275900*                   '&IDPRODNR= ' W-IDPRODNR-SOEK-X                       
276000*                   '&IDPLKLST= ' W-IDPLKLST-SOEK-X ')'                   
276100*         DELIMITED BY SIZE INTO SSA1                                     
276200*    MOVE '  GE' TO GODK-STATUSKODER                                      
276300*    CALL CBLTDLI USING GU ORQB-PCB DLI-IO-AREA2 SSA1                     
276400*    MOVE ORQB-STATUS-CODE TO STATUS-WS                                   
276500*    PERFORM IMS-STATUSKONTROLL                                           
276600*    .                                                                    
276700                                                                          
276800 IMS-GN-WDQ3A1 SECTION.                                                   
276900                                                                          
277000     STRING 'WDQ3A1  (WDQ3A1KY>=' W-WDQ3A1-MIN-X                          
277100                    '&WDQ3A1KY<=' W-WDQ3A1-MAX-X                          
277200                    '&IDPRC   >=' W-Q3A1-MIN-IDPRC                        
277300                    '&IDPRC   <=' W-Q3A1-MAX-IDPRC ')'                    
277400          DELIMITED BY SIZE INTO SSA1                                     
277500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
277600     CALL CBLTDLI USING GN Q3A1-PCB DLI-IO-Q3A1 SSA1                      
277700     MOVE Q3A1-STATUS-CODE TO STATUS-WS                                   
277800     PERFORM IMS-STATUSKONTROLL                                           
277900     .                                                                    
278000                                                                          
278100 IMS-GN-WDQ3A1-DISTR-KUND SECTION.                                        
278200                                                                          
278300     STRING 'WDQ3A1  (WDQ3A1KY>=' W-WDQ3A1-MIN-X                          
278400                    '&WDQ3A1KY<=' W-WDQ3A1-MAX-X                          
278500                    '&IDDISTR  =' SPAR-IDDISTR-X                          
278600                    '&IDKUNDNR =' SPAR-IDKUNDNR-X                         
278700                    '&IDPRC   >=' W-Q3A1-MIN-IDPRC                        
278800                    '&IDPRC   <=' W-Q3A1-MAX-IDPRC ')'                    
278900          DELIMITED BY SIZE INTO SSA1                                     
279000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
279100     CALL CBLTDLI USING GN Q3A1-PCB DLI-IO-Q3A1 SSA1                      
279200     MOVE Q3A1-STATUS-CODE TO STATUS-WS                                   
279300     PERFORM IMS-STATUSKONTROLL                                           
279400     .                                                                    
279500                                                                          
279600*IMS-GN-WDQ3ASEQ-WLORQA01 SECTION.                                        
279700*                                                                         
279800*    STRING 'WLORQA01(WDQ3ASEQ>=' W-WDQ3ASEQ-MIN-X                        
279900*                   '&WDQ3ASEQ<=' W-WDQ3ASEQ-MAX-X                        
280000*                   '&IDPRC   >=' W-Q3ASEQ-MIN-IDPRC                      
280100*                   '&IDPRC   <=' W-Q3ASEQ-MAX-IDPRC ')'                  
280200*         DELIMITED BY SIZE INTO SSA1                                     
280300*    MOVE '  GBGE' TO GODK-STATUSKODER                                    
280400*    CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA2 SSA1                     
280500*    MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
280600*    PERFORM IMS-STATUSKONTROLL                                           
280700*    .                                                                    
280800                                                                          
280900 IMS-GU-WDQ3A1 SECTION.                                                   
281000                                                                          
281100     STRING 'WDQ3A1  (WDQ3A1KY>=' W-WDQ3A1-MIN-X                          
281200                    '&WDQ3A1KY<=' W-WDQ3A1-MAX-X                          
281300                    '&IDPRC   >=' W-Q3A1-MIN-IDPRC                        
281400                    '&IDPRC   <=' W-Q3A1-MAX-IDPRC                        
281500                    '&IDPRODNR= ' W-IDPRODNR-SOEK-X                       
281600                    '&IDPLKLST= ' W-IDPLKLST-SOEK-X ')'                   
281700          DELIMITED BY SIZE INTO SSA1                                     
281800     MOVE '  GE' TO GODK-STATUSKODER                                      
281900     CALL CBLTDLI USING GU Q3A1-PCB DLI-IO-Q3A1 SSA1                      
282000     MOVE Q3A1-STATUS-CODE TO STATUS-WS                                   
282100     PERFORM IMS-STATUSKONTROLL                                           
282200     .                                                                    
282300                                                                          
282400 IMS-GU-WDQ3A1-DISTR-KUND SECTION.                                        
282500                                                                          
282600     STRING 'WDQ3A1  (WDQ3A1KY>=' W-WDQ3A1-MIN-X                          
282700                    '&WDQ3A1KY<=' W-WDQ3A1-MAX-X                          
282800                    '&IDDISTR  =' SPAR-IDDISTR-X                          
282900                    '&IDKUNDNR =' SPAR-IDKUNDNR-X ')'                     
283000*    STRING 'WDQ3A1  (WDQ3A1KY>=' W-WDQ3A1-MIN-X                          
283100*                   '&WDQ3A1KY<=' W-WDQ3A1-MAX-X                          
283200*                   '&IDDISTR  =' SPAR-IDDISTR-X                          
283300*                   '&IDKUNDNR =' SPAR-IDKUNDNR-X                         
283400*                   '&IDPRC   >=' W-Q3A1-MIN-IDPRC                        
283500*                   '&IDPRC   <=' W-Q3A1-MAX-IDPRC                        
283600*                   '&IDPRODNR= ' W-IDPRODNR-SOEK-X                       
283700*                   '&IDPLKLST= ' W-IDPLKLST-SOEK-X ')'                   
283800          DELIMITED BY SIZE INTO SSA1                                     
283900     MOVE '  GE' TO GODK-STATUSKODER                                      
284000     CALL CBLTDLI USING GU Q3A1-PCB DLI-IO-Q3A1 SSA1                      
284100     MOVE Q3A1-STATUS-CODE TO STATUS-WS                                   
284200     PERFORM IMS-STATUSKONTROLL                                           
284300     .                                                                    
284400                                                                          
284500*IMS-GU-WDQ3ASEQ-WLORQA01 SECTION.                                        
284600*                                                                         
284700*    STRING 'WLORQA01(WDQ3ASEQ>=' W-WDQ3ASEQ-MIN-X                        
284800*                   '&WDQ3ASEQ<=' W-WDQ3ASEQ-MAX-X                        
284900*                   '&IDPRC   >=' W-Q3ASEQ-MIN-IDPRC                      
285000*                   '&IDPRC   <=' W-Q3ASEQ-MAX-IDPRC                      
285100*                   '&IDPRODNR= ' W-IDPRODNR-SOEK-X                       
285200*                   '&IDPLKLST= ' W-IDPLKLST-SOEK-X ')'                   
285300*         DELIMITED BY SIZE INTO SSA1                                     
285400*    MOVE '  GE' TO GODK-STATUSKODER                                      
285500*    CALL CBLTDLI USING GU ORQA-PCB DLI-IO-Q3A1 SSA1                      
285600*    MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
285700*    PERFORM IMS-STATUSKONTROLL                                           
285800*    .                                                                    
285900                                                                          
286000                                                                          
286100 IMS-REPL-ORQ-WLORQA01 SECTION.                                           
286200                                                                          
286300     MOVE '  ' TO GODK-STATUSKODER                                        
286400     CALL CBLTDLI USING REPL ORQ-PCB DLI-IO-AREA2                         
286500     MOVE ORQ-STATUS-CODE TO STATUS-WS                                    
286600     PERFORM IMS-STATUSKONTROLL                                           
286700     .                                                                    
286800     EJECT                                                                
286900 IMS-GU-XXKH-WLXXKH11 SECTION.                                            
287000                                                                          
287100     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
287200          DELIMITED BY SIZE INTO SSA1                                     
287300     STRING 'WLXXKH11(WDGXKEY  =' W-4448-IDPRC-X ')'                      
287400          DELIMITED BY SIZE INTO SSA2                                     
287500     MOVE '  GE' TO GODK-STATUSKODER                                      
287600     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA1 SSA1 SSA2                
287700     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
287800     PERFORM IMS-STATUSKONTROLL                                           
287900     .                                                                    
288000     EJECT                                                                
288100 IMS-GHU-XXKO-WLXXKO11 SECTION.                                           
288200                                                                          
288300     STRING 'WLXXKO01(WDGXKEY  =' W-4461-IDHTYP-X ')'                     
288400          DELIMITED BY SIZE INTO SSA1                                     
288500     STRING 'WLXXKO11(WDGXKEY  =' W-4462-KDPRCGRP-X ')'                   
288600          DELIMITED BY SIZE INTO SSA2                                     
288700     MOVE '  GE' TO GODK-STATUSKODER                                      
288800     CALL CBLTDLI USING GHU XXKO-PCB DLI-IO-AREA1 SSA1 SSA2               
288900     MOVE XXKO-STATUS-CODE TO STATUS-WS                                   
289000     PERFORM IMS-STATUSKONTROLL                                           
289100     .                                                                    
289200                                                                          
289300                                                                          
289400 IMS-REPL-XXKO-WLXXKO11 SECTION.                                          
289500                                                                          
289600     MOVE '    ' TO GODK-STATUSKODER                                      
289700     CALL CBLTDLI USING REPL XXKO-PCB DLI-IO-AREA1                        
289800     MOVE XXKO-STATUS-CODE TO STATUS-WS                                   
289900     PERFORM IMS-STATUSKONTROLL                                           
290000     .                                                                    
290100     EJECT                                                                
290200 IMS-GHU-4011-WL401101 SECTION.                                           
290300                                                                          
290400     STRING 'WL401101(WDGXKEY  =' W-4011-IDHTYP-X ')'                     
290500          DELIMITED BY SIZE INTO SSA1                                     
290600     MOVE '    ' TO GODK-STATUSKODER                                      
290700     CALL CBLTDLI USING GHU 4011-PCB DLI-IO-AREA1 SSA1                    
290800     MOVE 4011-STATUS-CODE TO STATUS-WS                                   
290900     PERFORM IMS-STATUSKONTROLL                                           
291000     .                                                                    
291100                                                                          
291200 IMS-GHU-4011-WL401111 SECTION.                                           
291300                                                                          
291400     STRING 'WL401101(WDGXKEY  =' W-4011-IDHTYP-X ')'                     
291500          DELIMITED BY SIZE INTO SSA1                                     
291600     MOVE 'WL401111 ' TO SSA2                                             
291700     MOVE '  GE' TO GODK-STATUSKODER                                      
291800     CALL CBLTDLI USING GHU 4011-PCB DLI-IO-AREA4 SSA1 SSA2               
291900     MOVE 4011-STATUS-CODE TO STATUS-WS                                   
292000     PERFORM IMS-STATUSKONTROLL                                           
292100     .                                                                    
292200                                                                          
292300 IMS-REPL-4011-WL401111 SECTION.                                          
292400                                                                          
292500     MOVE '    ' TO GODK-STATUSKODER                                      
292600     CALL CBLTDLI USING REPL 4011-PCB DLI-IO-AREA4                        
292700     MOVE 4011-STATUS-CODE TO STATUS-WS                                   
292800     PERFORM IMS-STATUSKONTROLL                                           
292900     .                                                                    
293000                                                                          
293100 IMS-DLET-4011-WL401101 SECTION.                                          
293200                                                                          
293300     MOVE '    ' TO GODK-STATUSKODER                                      
293400     CALL CBLTDLI USING DLET 4011-PCB DLI-IO-AREA1                        
293500     MOVE 4011-STATUS-CODE TO STATUS-WS                                   
293600     PERFORM IMS-STATUSKONTROLL                                           
293700     .                                                                    
293800                                                                          
293900 IMS-ISRT-4011-WL401101 SECTION.                                          
294000                                                                          
294100     MOVE 'WL401101 ' TO SSA1                                             
294200     MOVE '    ' TO GODK-STATUSKODER                                      
294300     CALL CBLTDLI USING ISRT 4011-PCB DLI-IO-AREA1 SSA1                   
294400     MOVE 4011-STATUS-CODE TO STATUS-WS                                   
294500     PERFORM IMS-STATUSKONTROLL                                           
294600     .                                                                    
294700                                                                          
294800 IMS-ISRT-4011-WL401111 SECTION.                                          
294900                                                                          
295000     STRING 'WL401101(WDGXKEY  =' W-4011-IDHTYP-X ')'                     
295100          DELIMITED BY SIZE INTO SSA1                                     
295200     MOVE 'WL401111 ' TO SSA2                                             
295300     MOVE '    ' TO GODK-STATUSKODER                                      
295400     CALL CBLTDLI USING ISRT 4011-PCB DLI-IO-AREA4 SSA1 SSA2              
295500     MOVE 4011-STATUS-CODE TO STATUS-WS                                   
295600     PERFORM IMS-STATUSKONTROLL                                           
295700     .                                                                    
295800     EJECT                                                                
295900 IMS-ISRT-4001-WL400101 SECTION.                                          
296000                                                                          
296100     MOVE 'WL400101 ' TO SSA1                                             
296200     MOVE '    ' TO GODK-STATUSKODER                                      
296300     CALL CBLTDLI USING ISRT 4001-PCB DLI-IO-AREA1 SSA1                   
296400     MOVE 4001-STATUS-CODE TO STATUS-WS                                   
296500     PERFORM IMS-STATUSKONTROLL                                           
296600     .                                                                    
296700                                                                          
296800 IMS-ISRT-4001-WL400111 SECTION.                                          
296900                                                                          
297000     STRING 'WL400101(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
297100          DELIMITED BY SIZE INTO SSA1                                     
297200     MOVE 'WL400111 ' TO SSA2                                             
297300     MOVE '    ' TO GODK-STATUSKODER                                      
297400     CALL CBLTDLI USING ISRT 4001-PCB DLI-IO-AREA3 SSA1 SSA2              
297500     MOVE 4001-STATUS-CODE TO STATUS-WS                                   
297600     PERFORM IMS-STATUSKONTROLL                                           
297700     .                                                                    
297800     EJECT                                                                
297900 IMS-GU-WDB201      SECTION.                                              
298000                                                                          
298100     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
298200          DELIMITED BY SIZE INTO SSA1                                     
298300     MOVE '  GE' TO GODK-STATUSKODER                                      
298400     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
298500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
298600     PERFORM IMS-STATUSKONTROLL                                           
298700     .                                                                    
298800                                                                          
298900 IMS-GU-WDB601    SECTION.                                                
299000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
299100          DELIMITED BY SIZE INTO SSA1                                     
299200     MOVE '  GE' TO GODK-STATUSKODER                                      
299300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
299400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
299500     PERFORM IMS-STATUSKONTROLL                                           
299600     .                                                                    
299700     EJECT                                                                
299800                                                                          
299900 IMS-STATUSKONTROLL SECTION.                                              
300000                                                                          
300100     SET STATUS-IX TO 1                                                   
300200     SEARCH GODK-STATUS                                                   
300300       AT END CALL FELLOG                                                 
300400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
300500     END-SEARCH                                                           
300600     .                                                                    
300700     EJECT                                                                
