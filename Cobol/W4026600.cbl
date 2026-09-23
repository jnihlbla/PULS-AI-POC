000100 PROCESS DYNAM                                                            
000200*COMPOPT DB2BIND=YES                                                      
000300 ID DIVISION.                                                             
000400     SKIP2                                                                
000500 PROGRAM-ID.     W4026600.                                                
000600 AUTHOR.         CAMELIA OLGRENER.                                        
000700 DATE-WRITTEN.   91/04/05.                                                
000800                                                                          
000900     REMARKS.                                                             
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        MPP PROGRAM SOM:                                                 
001300*        - GÖR ÄNDRINGAR AV KOMPLETTERINGSINFO I ETT BEFINTLIGT           
001400*          PROFORMAHUVUD                                                  
001500*                                                                         
001600*        - STARTAR UTSKRIFT AV PROFORMADOKUMENT                           
001700*                                                                         
001800*        - STARTAR ORDERRELEASE AV PROFORMA                               
001900*                                                                         
002000*        PROGRAMMET LÄSER OCH UPPDATERAR :                                
002100*                           - WLPROC (WDE8) PROFORMA HUVUD                
002200*                                                                         
002300*        PROGRAMMET LÄSER : - WLPROD (WDE9) PROFORMA RADER                
002400*                           - WLARTC (WDK6) ARTIKEL REG. KDFARLIG         
002500*                           - WL4735 (WDR1) LEVERANSVILLKORTEXTER         
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W4T266  W4T266U                                     
002900*        MID:         W4I26601                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W4O26601                                            
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003900*    -COPY WY2000W1                                                       
004000     SKIP3                                                                
004100 77  IDPGM                       PIC X(08)   VALUE 'W4026600'.            
004200                                                                          
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  YES                         PIC X       VALUE 'Y'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 77  UPPDAT-TID                  PIC 9(8)    VALUE ZERO.                  
005200 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005300 77  WS-INDEX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +314  COMP SYNC.        
005500                                                                          
005600*    -ARBETSETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005700 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
005800 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
005900 77  WS-IDORDNR7                 PIC X(7)    VALUE SPACE.                 
006000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
006100 77  WS-ENGANGS-KUND             PIC 9(6)    VALUE 999999.                
006200 01  WS-IDKUNDRF-RED.                                                     
006300     03 WS-IDKUNDRF-1-7          PIC X(7)    VALUE SPACE.                 
006400     03 WS-IDKUNDRF-8-10         PIC X(3)    VALUE SPACE.                 
006500                                                                          
006600 01  WS-IDOUTREC-GRP.                                                     
006700     03 WS-IDOUTREC-IDPRT        PIC X(3)    VALUE SPACE.                 
006800     03 WS-IDOUTREC-IDDISTR      PIC X(9)    VALUE ZERO.                  
006900                                                                          
007000 77  WS-IDKUNDNR-NY              PIC X(6)    VALUE SPACE.                 
007100                                                                          
007200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007300     88  NYCKLAR-OK                          VALUE 'J'.                   
007400     88  NYCKLAR-FEL                         VALUE 'N'.                   
007500                                                                          
007600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007700     88  INDATA-OK                           VALUE 'J'.                   
007800     88  INDATA-FEL                          VALUE 'N'.                   
007900                                                                          
008000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
008100     88  ALLT-OK                             VALUE 'J'.                   
008200     88  ALLT-FEL                            VALUE 'N'.                   
008300                                                                          
008400 77  FARLIG-SW                   PIC X       VALUE 'J'.                   
008500     88  KDFARLIG-FINNS                      VALUE 'J'.                   
008600     88  KDFARLIG-FINNS-EJ                   VALUE 'N'.                   
008700                                                                          
008800 77  FLAGGA-TIGILTIG             PIC X       VALUE 'J'.                   
008900     88  TIGILTIG-OK                         VALUE 'J'.                   
009000                                                                          
009100 77  FLAGGA-TIFORDAT             PIC X       VALUE 'J'.                   
009200     88  TIFORDAT-OK                         VALUE 'J'.                   
009300                                                                          
009400 77  OTILL-UPPDAT-SW             PIC X       VALUE 'J'.                   
009500     88  OTILL-UPPDAT                        VALUE 'N'.                   
009600                                                                          
009700 77  UPPDAT-SW                   PIC X       VALUE 'N'.                   
009800     88  UPPDATERING-OK                      VALUE 'J'.                   
009900     88  UPPDATERING-EJ                      VALUE 'N'.                   
010000                                                                          
010100 77  UTSKRIFT-SW                 PIC X       VALUE 'N'.                   
010200     88  UTSKRIFT-OK                         VALUE 'J'.                   
010300     88  UTSKRIFT-EJ                         VALUE 'N'.                   
010400                                                                          
010500 77  RELEASE-SW                  PIC X       VALUE 'N'.                   
010600     88  RELEASE-START                       VALUE 'J'.                   
010700     88  RELEASE-EJ-START                    VALUE 'N'.                   
010800                                                                          
010900 77  OBKR-SW                     PIC X       VALUE 'N'.                   
011000     88  OBKR-OK                             VALUE 'J'.                   
011100     88  OBKR-FEL                            VALUE 'N'.                   
011200                                                                          
011300 77  PRELPR-SW                   PIC X       VALUE 'N'.                   
011400     88  PRELPR-FINNS                        VALUE 'J'.                   
011500     88  PRELPR-EJ-FINNS                     VALUE 'N'.                   
011600                                                                          
011700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011800     88  EGEN-MID                            VALUE '4266'.                
011900     88  GODK-MID                            VALUE '4262' '4263'          
012000                                                   '4264' '4265'          
012100                                                   '4266' '4267'          
012200                                                   '4268' '4269'.         
012300*   --- VALID IDDC CODES                                                  
012400*                                                                         
012500*01  -COPY WWDCKONS                                                       
012600                                                                          
012700     EJECT                                                                
012800*    --- SPAR AREA                                                        
012900 01  SPAR-AREA.                                                           
013000     03  SPAR-SUFKTBEL           PIC S9(9)V9(2) VALUE ZERO COMP-3.        
013100     03  SPAR-VKORDBTO           PIC S9(6)V9(1) VALUE ZERO COMP-3.        
013200     03  SPAR-VLORDBTO           PIC S9(4)V9(3) VALUE ZERO COMP-3.        
013300     03  SPAR-KDLEVVIL           PIC S9         VALUE ZERO COMP-3.        
013400     03  SPAR-TIGILTIG           PIC S9(7)      VALUE ZERO COMP-3.        
013500     03  SPAR-TIFORDAT           PIC S9(7)      VALUE ZERO COMP-3.        
013600     EJECT                                                                
013700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013800 01  GENERELLA-SUBPROGRAM.                                                
013900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014300     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
014400     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
014500     03  WZ04CRUL                PIC X(8)    VALUE 'WZ04CRUL'.            
014600     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
014700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014800*                                                                         
014900 01  GEMENSAMMA-SUBPROGRAM.                                               
015000     03  W411KREG                PIC X(8)    VALUE 'W411KREG'.            
015100     EJECT                                                                
015200 01  FILLER                      PIC X(8)    VALUE 'W411KREG'.            
015300*   -COPY W411KREG                                                        
015400     EJECT                                                                
015500*   -COPY W006PRT                                                         
015600     EJECT                                                                
015700 01  FILLER                      PIC X(8)    VALUE 'WZ04CRUL'.            
015800     EJECT                                                                
015900*   -COPY WZ04CRUL                                                        
016000     EJECT                                                                
016100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
016200*   -COPY WMEDAREA                                                        
016300     EJECT                                                                
016400 01  FELM-CODES.                                                          
016500     03  FILLER                  PIC X(16)   VALUE 'FELM AREA'.           
016600     03  FELM-KOR-UPPLYSTA-FAELT PIC X(3)    VALUE '001'.                 
016700     03  FELM-OTILL-UPPDATERING  PIC X(3)    VALUE '007'.                 
016800     03  FELM-FINNS-EJ           PIC X(3)    VALUE '010'.                 
016900     03  FELM-PF11-O-EJ-INDATA   PIC X(3)    VALUE '011'.                 
017000     03  FELM-RADER-SAKNAS       PIC X(3)    VALUE '029'.                 
017100     03  FELM-ORDERN-ANNULLERAD  PIC X(3)    VALUE '052'.                 
017200     03  FELM-ORDER-EJ-AVSLUT    PIC X(3)    VALUE '053'.                 
017300     03  FELM-MER-FUNK-VALD      PIC X(3)    VALUE '097'.                 
017400     03  FELM-MARK-FINNS-EJ      PIC X(3)    VALUE '108'.                 
017500     03  FELM-FEL-NYCKEL         PIC X(3)    VALUE '401'.                 
017600     03  FELM-OBEHOERIG          PIC X(3)    VALUE '405'.                 
017700     03  FELM-DIST-KUND-SAKNAS   PIC X(3)    VALUE '063'.                 
017800     03  FELM-FELAKTIG-PRINTER   PIC X(3)    VALUE '772'.                 
017900     03  FELM-BET-STOPPAD        PIC X(3)    VALUE '213'.                 
018000     03  FELM-DEST-SAKNAS        PIC X(3)    VALUE '222'.                 
018100     SKIP3                                                                
018200 01  MESSAGE-CODES.                                                       
018300     03  FILLER                  PIC X(16)   VALUE 'INFO AREA'.           
018400     03  INFO-TRYCK-PF11         PIC X(3)    VALUE '003'.                 
018500     03  INFO-UPPDAT-GJORD       PIC X(3)    VALUE '101'.                 
018600     03  INFO-KOAD-FOR-UTSKRIFT  PIC X(3)    VALUE '118'.                 
018700     03  INFO-BEKR-PRELPRIS      PIC X(3)    VALUE '198'.                 
018800     EJECT                                                                
018900*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
019000*01 -COPY WDATAREA                                                        
019100     EJECT                                                                
019200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
019300*01 -COPY WMSGINIT                                                        
019400     EJECT                                                                
019500*01  -COPY WDECAREA                                                       
019600     EJECT                                                                
019700*01  -COPY WSECAREA                                                       
019800     EJECT                                                                
019900*01  -COPY W475CONS -PRE CONS-                                            
020000     EJECT                                                                
020100 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
020200 01  FILLER REDEFINES TEST-IDDISTR.                                       
020300*    03  -COPY WWDIST03.                                                  
020310 01  FILLER REDEFINES TEST-IDDISTR.                                       
020320*    03  -COPY WWDIST11.                                                  
020400 01  FILLER REDEFINES TEST-IDDISTR.                                       
020500*    ----DIST79-DEALER-PRICE----                                          
020600*    03  -COPY WWDIST79                                                   
020700     EJECT                                                                
020800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020900*                                                                         
021000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021100     SKIP3                                                                
021200*01  MID -COPY W4I26601                                                   
021300     EJECT                                                                
021400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021500     SKIP3                                                                
021600*01  -COPY WMSGAREA                                                       
021700     EJECT                                                                
021800     03  MOD REDEFINES MSG-AREA.                                          
021900*      05  -COPY W4O26601                                                 
022000     EJECT                                                                
022100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022200     SKIP3                                                                
022300*01  -COPY WMFSAREA                                                       
022400     EJECT                                                                
022500                                                                          
022600*------------------HOPP TIL 4295 (PROFORMA UTSKRIFT)                      
022700                                                                          
022800 01  P-TO-P-SW1.                                                          
022900     03  PTOP1-LL                PIC S9(4)   VALUE +92 COMP SYNC.         
023000     03  PTOP1-Z1                PIC X       VALUE LOW-VALUE.             
023100     03  PTOP1-Z2                PIC X       VALUE LOW-VALUE.             
023200     03  PTOP1-TRANSKOD          PIC X(7)    VALUE 'W4T295X'.             
023300     03  FILLER                  PIC X       VALUE SPACE.                 
023400     03  PTOP1-IDTRANS           PIC X(4)    VALUE '4266'.                
023500     03  PTOP1-KDMFSFOR          PIC X.                                   
023600*    03  MID -COPY W4I29501   -PRE PTOP1-.                                
023700     EJECT                                                                
023800*------------------HOPP TIL 4269 (PROFORMA RELEASE)                       
023900 01  P-TO-P-SW2.                                                          
024000     03  PTOP2-LL                PIC S9(4)   VALUE +40  COMP SYNC.        
024100     03  PTOP2-Z1                PIC X       VALUE LOW-VALUE.             
024200     03  PTOP2-Z2                PIC X       VALUE LOW-VALUE.             
024300     03  PTOP2-TRANSKOD          PIC X(7)    VALUE 'W4T269X'.             
024400     03  FILLER                  PIC X       VALUE SPACE.                 
024500     03  PTOP2-IDTRANS           PIC X(4)    VALUE '4266'.                
024600     03  PTOP2-KDMFSFOR          PIC X.                                   
024700     03  PTOP2-IDDISTR           PIC X(4).                                
024800     03  PTOP2-IDKUNDNR          PIC X(6).                                
024900     03  PTOP2-IDKUNDRF          PIC X(10).                               
025000     03  PTOP2-IDPRT             PIC X(3).                                
025100     EJECT                                                                
025200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025400     SKIP3                                                                
025500*    --- STATUS-KOD FRÅN IMS                                              
025600 01  STATUS-WS                   PIC XX.                                  
025700     88  SEGMENT-FINNS                       VALUE '  '.                  
025800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026000     88  BASEN-SLUT                          VALUE 'GB'.                  
026100     SKIP2                                                                
026200 01  GODK-STATUSKODER.                                                    
026300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026400     SKIP3                                                                
026500 01  NYCKLAR-TILL-DLI.                                                    
026600*--------------------WDB1                                                 
026700     03  W-WDB101KY-X.                                                    
026800         05  W-IDPARTNR          PIC X(9)         VALUE SPACE.            
026900         05  W-IDFTG             PIC 9(2)         VALUE ZERO.             
027000*--------------------WDB2                                                 
027100     03  W-IDGMT-X.                                                       
027200         05  W-IDDISTR           PIC S9(5) COMP-3 VALUE ZERO.             
027300         05  W-IDKUNDNR          PIC S9(7) COMP-3 VALUE ZERO.             
027400*--------------------WDE8                                                 
027500     03  W-WDE801KY-X.                                                    
027600         05  W-PHUV-IDDISTR      PIC S9(5)    VALUE ZERO COMP-3.          
027700         05  W-PHUV-IDKUNDNR     PIC S9(7)    VALUE ZERO COMP-3.          
027800         05  W-PHUV-IDKUNDRF.                                             
027900            07 W-PHUV-IDORDNR7   PIC 9(7)     VALUE ZERO.                 
028000            07 FILLER            PIC X(3)     VALUE SPACE.                
028100                                                                          
028200*--------------------WDE9                                                 
028300     03  W-WDE901KY-MIN-X.                                                
028400         05  W-PRAD-IDORDER-MIN  PIC S9(7)    VALUE ZERO COMP-3.          
028500         05  W-PRAD-IDARTNR-MIN  PIC S9(9)    VALUE ZERO COMP-3.          
028600         05  W-PRAD-IDLOPNR-MIN  PIC S9(3)    VALUE ZERO COMP-3.          
028700                                                                          
028800     03  W-WDE901KY-MAX-X.                                                
028900         05  W-PRAD-IDORDER-MAX  PIC S9(7)    VALUE ZERO COMP-3.          
029000         05  W-PRAD-IDARTNR-MAX  PIC S9(9)    VALUE +999999999            
029100                                                         COMP-3.          
029200         05  W-PRAD-IDLOPNR-MAX  PIC S9(3)    VALUE +999 COMP-3.          
029300                                                                          
029400*--------------------WDK6                                                 
029500     03  W-IDARTNR-X.                                                     
029600         05  W-WDK6-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.           
029700                                                                          
029800     03  W-KDSEGKEY-X.                                                    
029900         05  W-WDK6-KDSEGKEY     PIC X(1)    VALUE '1'.                   
030000                                                                          
030100*--------------------WDR1                                                 
030200     03  W-WDGX473E-X.                                                    
030300         05  W-473E-IDHTYP       PIC X(4)    VALUE '4735'.                
030400         05  W-473E-KDLEVVIL     PIC S9(3)   VALUE ZERO COMP-3.           
030500         05  W-473E-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
030600                                                                          
030700*--------------------WDQ1                                                 
030800     03  W-WDQ101KY-MIN-X.                                                
030900         05  W-WDQ101-MIN-IDORDER PIC S9(7)  COMP-3.                      
031000         05  FILLER               PIC X(13).                              
031100                                                                          
031200     03  W-WDQ101KY-MAX-X.                                                
031300         05  W-WDQ101-MAX-IDORDER PIC S9(7)  COMP-3.                      
031400         05  FILLER               PIC X(13).                              
031500                                                                          
031600     03  W-WDQ101-IDSYSTEM        PIC X(4)   VALUE 'PROF'.                
031700                                                                          
031800     SKIP3                                                                
031900 01  SSA1                        PIC X(94).                               
032000 01  SSA2                        PIC X(94).                               
032100 01  SSA3                        PIC X(94).                               
032200     EJECT                                                                
032300*    --- IMS FUNKTIONSKODER                                               
032400*01  -COPY W0003                                                          
032500     EJECT                                                                
032600*    ---  DLI INPUT-OUTPUT AREA                                           
032700                                                                          
032800*---------------IO-AREA FÖR BARA LÄSNING                                  
032900 01  DLI-IO-AREA.                                                         
033000     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
033100     SKIP3                                                                
033200 01  FILLER                      PIC X(16)   VALUE                        
033300                                              'IO-WDE801'.                
033400 01  DLI-IO-AREA-WDE801.                                                  
033500     03  WLPROC01.                                                        
033600*        05  -COPY WDE801                                                 
033700     EJECT                                                                
033800 01  FILLER                      PIC X(16)   VALUE 'IO-WDB101'.           
033900 01  DLI-IO-AREA-WDB101.                                                  
034000     03  WDB101.                                                          
034100*        05  -COPY WDB101                                                 
034200     EJECT                                                                
034300 01  FILLER                      PIC X(16)   VALUE 'IO-WDB201'.           
034400 01  DLI-IO-AREA-WDB201.                                                  
034500     03  WDB201.                                                          
034600*        05  -COPY WDB201                                                 
034700     EJECT                                                                
034800 01  FILLER                      PIC X(16)   VALUE 'IO-WDE901'.           
034900 01  DLI-IO-AREA-WDE901.                                                  
035000     03  WLPROD01.                                                        
035100*        05  -COPY WDE901                                                 
035200     EJECT                                                                
035300 01  FILLER                      PIC X(16)   VALUE                        
035400                                              'IO-WDK601'.                
035500     EJECT                                                                
035600 01  DLI-IO-AREA-WDK601.                                                  
035700     03  WLARTC01.                                                        
035800*        05  -COPY WDK601    -PRE WDK6-                                   
035900     EJECT                                                                
036000 01  FILLER                      PIC X(16)   VALUE                        
036100                                              'IO-WDK611'.                
036200 01  DLI-IO-AREA-WDK611.                                                  
036300     03  WLARTC11.                                                        
036400*        05  -COPY WDK611    -PRE WDK6-                                   
036500     EJECT                                                                
036600 01  FILLER                      PIC X(16)   VALUE                        
036700                                              'IO-WDR1-HTYP473E'.         
036800 01  DLI-IO-AREA-WDGX473E.                                                
036900     03  WL473501.                                                        
037000*        05  -COPY WDGX473E                                               
037100     EJECT                                                                
037200 01  FILLER                      PIC X(16)   VALUE                        
037300                                              'IO-WDR1-HTYP4735'.         
037400 01  DLI-IO-AREA-WDGX4735.                                                
037500     03  WL473511.                                                        
037600*        05  -COPY WDGX4735                                               
037700     EJECT                                                                
037800 01  FILLER                      PIC X(16)   VALUE                        
037900                                              'IO-WDQ101'.                
038000 01  DLI-IO-AREA-WDQ101.                                                  
038100     03  WLORQM01.                                                        
038200*        05  -COPY WDQ101                                                 
038300     EJECT                                                                
038400 LINKAGE SECTION.                                                         
038500                                                                          
038600*01  -COPY W0009      -PRE MSG-                                           
038700     EJECT                                                                
038800*01  -COPY W0009      -PRE ALT1-                                          
038900     EJECT                                                                
039000*01  -COPY W0009      -PRE ALT2-                                          
039100     EJECT                                                                
039200*01  -COPY W0008      -PRE USEA-                                          
039300     05  FILLER                  PIC X.                                   
039400     EJECT                                                                
039500*01  -COPY W0008      -PRE PROC-                                          
039600     05  FILLER                  PIC X.                                   
039700     EJECT                                                                
039800*01  -COPY W0008      -PRE PROD-                                          
039900     05  FILLER                  PIC X.                                   
040000     EJECT                                                                
040100*01  -COPY W0008      -PRE ARTC-                                          
040200     05  FILLER                  PIC X.                                   
040300     EJECT                                                                
040400*01  -COPY W0008      -PRE 4735-                                          
040500     05  FILLER                  PIC X.                                   
040600     EJECT                                                                
040700*01  -COPY W0008      -PRE ORQM-                                          
040800     05  FILLER                  PIC X.                                   
040900     EJECT                                                                
041000*01  -COPY W0008      -PRE WDB1-                                          
041100     05  FILLER                  PIC X.                                   
041200     EJECT                                                                
041300*01  -COPY W0008      -PRE WDB2-                                          
041400     05  FILLER                  PIC X.                                   
041500     EJECT                                                                
041600 01  KREG-GMTA-PCB               PIC X.                                   
041700                                                                          
041800 01  KREG-GMTB-PCB               PIC X.                                   
041900                                                                          
042000 01  KREG-GMTC-PCB               PIC X.                                   
042100                                                                          
042200 01  KREG-BETC-PCB               PIC X.                                   
042300     EJECT                                                                
042400 PROCEDURE DIVISION  USING MSG-PCB   ALT1-PCB ALT2-PCB USEA-PCB           
042500                           PROC-PCB  PROD-PCB ARTC-PCB                    
042600                           4735-PCB  ORQM-PCB WDB1-PCB WDB2-PCB           
042700                           KREG-GMTA-PCB                                  
042800                           KREG-GMTB-PCB                                  
042900                           KREG-GMTC-PCB                                  
043000                           KREG-BETC-PCB.                                 
043100 MAIN SECTION.                                                            
043200     ENTRY 'DLITCBL' USING MSG-PCB   ALT1-PCB ALT2-PCB USEA-PCB           
043300                           PROC-PCB  PROD-PCB ARTC-PCB                    
043400                           4735-PCB  ORQM-PCB WDB1-PCB WDB2-PCB           
043500                           KREG-GMTA-PCB                                  
043600                           KREG-GMTB-PCB                                  
043700                           KREG-GMTC-PCB                                  
043800                           KREG-BETC-PCB.                                 
043900     PERFORM IMS-GET-MSG                                                  
044000     IF SEGMENT-FINNS                                                     
044100       PERFORM A-INIT                                                     
044200       PERFORM B-KOLLA-NYCKLAR                                            
044300       IF NYCKLAR-OK                                                      
044400         PERFORM IMS-GHU-PROC-WDE801-PHUV                                 
044500                                                                          
044600         IF SEGMENT-FINNS                                                 
044700           PERFORM C-KOLLA-OM-RAETT-PHUV                                  
044800                                                                          
044900           IF ALLT-OK                                                     
045000             IF MFS-UPDATE                                                
045100               PERFORM E-UPPDATERA                                        
045200             ELSE                                                         
045300               PERFORM F-BEHANDLA-ENTER                                   
045400             END-IF                                                       
045500                                                                          
045600             IF PRELPR-EJ-FINNS                                           
045700               PERFORM G-LAES-VISA-BILD                                   
045800             END-IF                                                       
045900           END-IF                                                         
046000         ELSE                                                             
046100           MOVE FELM-FINNS-EJ TO MED-IDMFSFEL                             
046200           CALL WMEDKONV USING MED-WMEDAREA                               
046300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
046400         END-IF                                                           
046500       END-IF                                                             
046600                                                                          
046700       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
046800       PERFORM H-INSERT-MESAGE                                            
046900     END-IF                                                               
047000     MOVE ZERO TO RETURN-CODE                                             
047100     GOBACK                                                               
047200     .                                                                    
047300     EJECT                                                                
047400 A-INIT SECTION.                                                          
047500                                                                          
047600     IF MSG-DUBBLA-TRANSKODER                                             
047700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I26601                 
047800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
047900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
048000     ELSE                                                                 
048100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I26601                  
048200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
048300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
048400     END-IF                                                               
048500                                                                          
048600     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
048700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
048800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
048900                                                                          
049000     MOVE LOW-VALUE TO MSG-AREA                                           
049100     MOVE 'W4O26601' TO MFS-IDMOD                                         
049200     MOVE '4266' TO MOD-IDTRANS                                           
049300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
049400                                                                          
049500     ACCEPT DAGENS-DATUM FROM DATE                                        
049600     ACCEPT UPPDAT-TID   FROM TIME                                        
049700                                                                          
049800     IF NOT EGEN-MID                                                      
049900       MOVE SPACE TO MFS-KDTRTYP                                          
050000       MOVE ' ' TO MFS-IDPFK                                              
050100       PERFORM MFS-RENSA-FAELT-IN                                         
050200       MOVE MFS-RENSA-FAELT TO MID-IDDISTR-IN                             
050300                               MID-IDKUNDNR-IN                            
050400                               MID-IDORDNR7-IN                            
050500                               MID-IDARTNR-IN                             
050600                               MID-IDDISTR-UT                             
050700                               MID-IDKUNDNR-UT                            
050800                               MID-IDORDNR7-UT                            
050900                               MID-IDARTNR-UT                             
051000     END-IF                                                               
051100                                                                          
051200     IF ENGLISH-TEXT                                                      
051300       MOVE +2 TO SPRAK-IX                                                
051400       MOVE 'GB ' TO MED-IDSKYLT                                          
051500     ELSE                                                                 
051600       MOVE +1 TO SPRAK-IX                                                
051700       MOVE 'S  ' TO MED-IDSKYLT                                          
051800     END-IF                                                               
051900                                                                          
052000     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O26601 + 4                  
052100     .                                                                    
052200     EJECT                                                                
052300 B-KOLLA-NYCKLAR SECTION.                                                 
052400                                                                          
052500     MOVE JA TO NYCKLAR-SW                                                
052600                                                                          
052700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
052800     MOVE '001'             TO MSGI-KDCALL                                
052900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
053000     MOVE '4266'            TO MSGI-IDTRANS                               
053100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
053200     IF MFS-IDTRANS = '4266'                                              
053300        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
053400        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
053500                                                                          
053600        MOVE MID-IDORDNR7-IN TO WS-IDKUNDRF-1-7                           
053700        IF WS-IDKUNDRF-1-7 = ALL '+'                                      
053800          MOVE '+++'         TO WS-IDKUNDRF-8-10                          
053900        ELSE                                                              
054000          MOVE SPACE         TO WS-IDKUNDRF-8-10                          
054100        END-IF                                                            
054200        MOVE WS-IDKUNDRF-RED TO MSGI-IDKUNDRF                             
054300        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
054400     END-IF                                                               
054500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
054600     PERFORM BA-KOLLA-DISTRIKT                                            
054700     PERFORM BB-KOLLA-KUNDNR                                              
054800     PERFORM BC-KOLLA-ORDER                                               
054900                                                                          
055000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
055100                                                                          
055200                                                                          
055300     IF NYCKLAR-FEL                                                       
055400          MOVE FELM-FEL-NYCKEL TO MED-IDMFSFEL                            
055500          CALL WMEDKONV USING MED-WMEDAREA                                
055600          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
055700          PERFORM MFS-RENSA-FAELT-IN                                      
055800          PERFORM MFS-RENSA-FAELT-UT                                      
055900                                                                          
056000     END-IF                                                               
056100     .                                                                    
056200     EJECT                                                                
056300 BA-KOLLA-DISTRIKT SECTION.                                               
056400                                                                          
056500     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
056600                                                                          
056700     MOVE MSGI-IDDISTR   TO WS-IDDISTR                                    
056800     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
056900                                                                          
057000     IF MID-IDDISTR-IN = ALL '+'                                          
057100       CONTINUE                                                           
057200     ELSE                                                                 
057300       MOVE ' '         TO MFS-IDPFK                                      
057400       MOVE SPACE       TO MFS-KDTRTYP                                    
057500     END-IF                                                               
057600                                                                          
057700     MOVE WS-IDDISTR TO MOD-IDDISTR-UT                                    
057800     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
057900                                                                          
058000     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
058100       MOVE WS-IDDISTR TO W-PHUV-IDDISTR                                  
058110       MOVE WS-IDDISTR        TO DIST79-IDDISTR                           
058120       IF DIST79-DEALER-PRICE                                             
058130         IF ENGLISH-TEXT                                                  
058140           MOVE 'DEALERPRICE' TO MOD-TEDDI                                
058150         ELSE                                                             
058160           MOVE '    ÅF PRIS' TO MOD-TEDDI                                
058170         END-IF                                                           
058180       ELSE                                                               
058190         MOVE SPACE           TO MOD-TEDDI                                
058191       END-IF                                                             
058200     ELSE                                                                 
058300       MOVE NEJ TO NYCKLAR-SW                                             
058400     END-IF                                                               
058500                                                                          
059600     .                                                                    
059700     EJECT                                                                
059800 BB-KOLLA-KUNDNR SECTION.                                                 
059900                                                                          
060000     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
060100                                                                          
060200     MOVE MSGI-IDKUNDNR    TO WS-IDKUNDNR                                 
060300     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
060400                                                                          
060500     IF MID-IDKUNDNR-IN = ALL '+'                                         
060600       CONTINUE                                                           
060700     ELSE                                                                 
060800       MOVE ' '         TO MFS-IDPFK                                      
060900       MOVE SPACE       TO MFS-KDTRTYP                                    
061000     END-IF                                                               
061100                                                                          
061200     MOVE WS-IDKUNDNR TO MOD-IDKUNDNR-UT                                  
061300     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
061400     IF MOD-IDKUNDNR-UT = SPACE                                           
061500       MOVE '     0' TO MOD-IDKUNDNR-UT                                   
061600     END-IF                                                               
061700                                                                          
061800     IF WS-IDKUNDNR NUMERIC                                               
061900       MOVE WS-IDKUNDNR TO W-PHUV-IDKUNDNR                                
062000     ELSE                                                                 
062100       MOVE NEJ TO NYCKLAR-SW                                             
062200     END-IF                                                               
062300     .                                                                    
062400     EJECT                                                                
062500 BC-KOLLA-ORDER SECTION.                                                  
062600                                                                          
062700     MOVE MFS-RENSA-FAELT TO MOD-IDORDNR7-IN                              
062800                                                                          
062900     MOVE MSGI-IDKUNDRF    TO WS-IDORDNR7                                 
063000     INSPECT WS-IDORDNR7 REPLACING LEADING SPACE BY ZERO                  
063100                                                                          
063200     IF MID-IDORDNR7-IN = ALL '+'                                         
063300       CONTINUE                                                           
063400     ELSE                                                                 
063500       MOVE ' '         TO MFS-IDPFK                                      
063600       MOVE SPACE       TO MFS-KDTRTYP                                    
063700     END-IF                                                               
063800                                                                          
063900     MOVE WS-IDORDNR7 TO MOD-IDORDNR7-UT                                  
064000     INSPECT MOD-IDORDNR7-UT REPLACING LEADING ZERO BY SPACE              
064100                                                                          
064200     IF WS-IDORDNR7 NUMERIC                                               
064300       MOVE WS-IDORDNR7 TO W-PHUV-IDORDNR7                                
064400     ELSE                                                                 
064500       MOVE NEJ TO NYCKLAR-SW                                             
064600     END-IF                                                               
064700     .                                                                    
064800     EJECT                                                                
064900 C-KOLLA-OM-RAETT-PHUV SECTION.                                           
065000                                                                          
065100     MOVE JA TO ALLT-SW                                                   
065200                                                                          
065300     IF PHUV-FLBORT    = 'N'                                              
065400                                                                          
065500       IF MFS-UPDATE                                                      
065600                                                                          
065700         IF MID-IDPRT-UTSKRIFT NOT = ALL '+'                              
065800           PERFORM S01-KOLLA-BEHOERIGHET                                  
065900           PERFORM S02-KOLLA-OM-PRAD-FINNS                                
066000                                                                          
066100         ELSE                                                             
066200           MOVE PHUV-TIFORDAT   TO TMP1-YYMMDD                            
066300           MOVE DAGENS-DATUM    TO TMP2-YYMMDD                            
066400           PERFORM WY2000P1                                               
066500           IF TMP1-YYMMDD > TMP2-YYMMDD AND                               
066600              PHUV-TIORDDAT = ZERO                                        
066700             PERFORM S01-KOLLA-BEHOERIGHET                                
066800             PERFORM S02-KOLLA-OM-PRAD-FINNS                              
066900                                                                          
067000           ELSE                                                           
067100             PERFORM S03-VISA-OTILL-UPPDATERING                           
067200           END-IF                                                         
067300         END-IF                                                           
067400                                                                          
067500       ELSE                                                               
067600         PERFORM S01-KOLLA-BEHOERIGHET                                    
067700         PERFORM S07-KOLL-OM-PRAD-FARLIG-FINNS                            
067800       END-IF                                                             
067900                                                                          
068000     ELSE                                                                 
068100       MOVE FELM-ORDERN-ANNULLERAD TO MED-IDMFSFEL                        
068200       CALL WMEDKONV USING MED-WMEDAREA                                   
068300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
068400       MOVE NEJ TO ALLT-SW                                                
068500     END-IF                                                               
068600     .                                                                    
068700     EJECT                                                                
068800 E-UPPDATERA SECTION.                                                     
068900                                                                          
069000     IF MID-INPUT NOT        = ALL '+' AND                                
069100        MID-IDPRT-UTSKRIFT   = ALL '+' AND                                
069200        MID-IDPRT-RELEASE    = ALL '+' AND                                
069300        MID-FLAGGA-PRELPRIS  = ALL '+' AND                                
069400        MID-IDKUNDNR-NY      = ALL '+'                                    
069500                                                                          
069600       PERFORM EA-UPPDATERA-PHUVUD                                        
069700     ELSE                                                                 
069800       IF  MID-INPUT              = ALL '+'  AND                          
069900           MID-IDPRT-UTSKRIFT NOT = ALL '+'  AND                          
070000           MID-IDPRT-RELEASE      = ALL '+'  AND                          
070100           MID-IDKUNDNR-NY        = ALL '+'                               
070200                                                                          
070300         PERFORM EB-STARTA-PROFORMA-UTSKRIFT                              
070400       ELSE                                                               
070500         IF  MID-INPUT             = ALL '+' AND                          
070600             MID-IDPRT-UTSKRIFT    = ALL '+' AND                          
070700             MID-IDPRT-RELEASE NOT = ALL '+' AND                          
070800             MID-IDKUNDNR-NY       = ALL '+'                              
070900                                                                          
071000           PERFORM EC-STARTA-PROFORMA-RELEASE                             
071100         ELSE                                                             
071200           IF MID-INPUT            = ALL '+' AND                          
071300              MID-IDPRT-UTSKRIFT   = ALL '+' AND                          
071400              MID-IDPRT-RELEASE    = ALL '+' AND                          
071500              MID-FLAGGA-PRELPRIS  = ALL '+' AND                          
071600              MID-IDKUNDNR-NY NOT  = ALL '+'                              
071700                                                                          
071800             PERFORM ED-BEHANDLA-NYTT-KUNDNR                              
071900           ELSE                                                           
072000             IF (MID-FLAGGA-PRELPRIS = 'J' OR 'Y') AND                    
072100                 MID-IDPRT-UTSKRIFT NOT = ALL '+'                         
072200               PERFORM EB-STARTA-PROFORMA-UTSKRIFT                        
072300             ELSE                                                         
072400               IF (MID-FLAGGA-PRELPRIS = 'J' OR 'Y') AND                  
072500                  MID-IDPRT-RELEASE  NOT = ALL '+'                        
072600                 PERFORM EC-STARTA-PROFORMA-RELEASE                       
072700               ELSE                                                       
072800                 PERFORM EE-VISA-FEL                                      
072900               END-IF                                                     
073000             END-IF                                                       
073100           END-IF                                                         
073200         END-IF                                                           
073300       END-IF                                                             
073400     END-IF                                                               
073500     .                                                                    
073600     EJECT                                                                
073700 EA-UPPDATERA-PHUVUD SECTION.                                             
073800                                                                          
073900     IF NOT OTILL-UPPDAT                                                  
074000       PERFORM EAA-KOLLA-INPUT                                            
074100                                                                          
074200       IF INDATA-FEL                                                      
074300         MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                     
074400         CALL WMEDKONV USING MED-WMEDAREA                                 
074500         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
074600         PERFORM MFS-ROER-EJ-FAELT-IN                                     
074700         MOVE NEJ TO UPPDAT-SW                                            
074800                                                                          
074900       ELSE                                                               
075000         PERFORM EAB-AENDRA-I-DB                                          
075100         MOVE JA TO UPPDAT-SW                                             
075200       END-IF                                                             
075300     END-IF                                                               
075400     .                                                                    
075500     EJECT                                                                
075600 EAA-KOLLA-INPUT SECTION.                                                 
075700                                                                          
075800     MOVE JA TO INDATA-SW                                                 
075900                                                                          
076000     IF MID-VKORDBTO NOT = ALL '+'                                        
076100       MOVE MID-VKORDBTO           TO DEC-IDFRIDATA                       
076200       MOVE +6                     TO DEC-KVHELTAL                        
076300       MOVE +1                     TO DEC-KVDECIMAL                       
076400       CALL WDECEDIT USING DEC-WDECAREA                                   
076500       IF DEC-KDSVAR-OK                                                   
076600         MOVE DEC-IDEDITDATA       TO SPAR-VKORDBTO                       
076700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-VKORDBTO-ATTR                   
076800       ELSE                                                               
076900         MOVE NEJ                  TO INDATA-SW                           
077000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-VKORDBTO-ATTR                   
077100       END-IF                                                             
077200     END-IF                                                               
077300                                                                          
077400     IF MID-VLORDBTO NOT = ALL '+'                                        
077500       MOVE MID-VLORDBTO           TO DEC-IDFRIDATA                       
077600       MOVE +4                     TO DEC-KVHELTAL                        
077700       MOVE +3                     TO DEC-KVDECIMAL                       
077800       CALL WDECEDIT USING DEC-WDECAREA                                   
077900       IF DEC-KDSVAR-OK                                                   
078000         MOVE DEC-IDEDITDATA       TO SPAR-VLORDBTO                       
078100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-VLORDBTO-ATTR                   
078200       ELSE                                                               
078300         MOVE NEJ                  TO INDATA-SW                           
078400         MOVE MFS-ALFA-FAELT-FEL   TO MOD-VLORDBTO-ATTR                   
078500       END-IF                                                             
078600     END-IF                                                               
078700                                                                          
078800     IF MID-KDLEVVIL NOT = ALL '+'                                        
078900       IF MID-KDLEVVIL NUMERIC                                            
079000         PERFORM EAAB-KOLLA-OM-RAETT-KDLEVVIL                             
079100       ELSE                                                               
079200         MOVE NEJ                  TO INDATA-SW                           
079300         MOVE MFS-NUM-FAELT-FEL    TO MOD-KDLEVVIL-ATTR                   
079400       END-IF                                                             
079500     END-IF                                                               
079600                                                                          
079700     IF MID-TIGILTIG NOT = ALL '+'                                        
079800       MOVE NEJ                    TO FLAGGA-TIGILTIG                     
079900       IF MID-TIGILTIG NUMERIC                                            
080000         MOVE MID-TIGILTIG         TO DAT-I-TIDATUM                       
080100         MOVE 'AAMMDD'             TO DAT-KDDATFORM                       
080200         CALL WDATKONV USING DAT-KDDATFORM,                               
080300                             DAT-I-TIDATUM,                               
080400                             DAT-O-TIDATUM,                               
080500                             DAT-KDSVAR                                   
080600         MOVE MID-TIGILTIG   TO TMP1-YYMMDD                               
080700         MOVE DAGENS-DATUM   TO TMP2-YYMMDD                               
080800         PERFORM WY2000P1                                                 
080900         IF DAT-KDSVAR-OK AND                                             
081000            TMP1-YYMMDD  NOT < TMP2-YYMMDD                                
081100           MOVE JA                  TO FLAGGA-TIGILTIG                    
081200           MOVE MFS-NUM-FAELT-RAETT TO MOD-TIGILTIG-ATTR                  
081300         ELSE                                                             
081400           MOVE NEJ                 TO INDATA-SW                          
081500           MOVE MFS-NUM-FAELT-FEL   TO MOD-TIGILTIG-ATTR                  
081600         END-IF                                                           
081700       ELSE                                                               
081800         MOVE NEJ                   TO INDATA-SW                          
081900         MOVE MFS-NUM-FAELT-FEL     TO MOD-TIGILTIG-ATTR                  
082000       END-IF                                                             
082100     END-IF                                                               
082200                                                                          
082300     IF MID-TIFORDAT NOT = ALL '+'                                        
082400       MOVE NEJ                     TO FLAGGA-TIFORDAT                    
082500       IF MID-TIFORDAT NUMERIC                                            
082600         MOVE MID-TIFORDAT          TO DAT-I-TIDATUM                      
082700         MOVE 'AAMMDD'              TO DAT-KDDATFORM                      
082800         CALL WDATKONV USING DAT-KDDATFORM,                               
082900                             DAT-I-TIDATUM,                               
083000                             DAT-O-TIDATUM,                               
083100                             DAT-KDSVAR                                   
083200         MOVE MID-TIFORDAT   TO TMP1-YYMMDD                               
083300         MOVE DAGENS-DATUM   TO TMP2-YYMMDD                               
083400         PERFORM WY2000P1                                                 
083500         IF DAT-KDSVAR-OK AND                                             
083600            TMP1-YYMMDD  NOT < TMP2-YYMMDD                                
083700           MOVE JA                  TO FLAGGA-TIFORDAT                    
083800           MOVE MFS-NUM-FAELT-RAETT TO MOD-TIFORDAT-ATTR                  
083900         ELSE                                                             
084000           MOVE NEJ                 TO INDATA-SW                          
084100           MOVE MFS-NUM-FAELT-FEL   TO MOD-TIFORDAT-ATTR                  
084200         END-IF                                                           
084300       ELSE                                                               
084400         MOVE NEJ                   TO INDATA-SW                          
084500         MOVE MFS-NUM-FAELT-FEL     TO MOD-TIFORDAT-ATTR                  
084600       END-IF                                                             
084700     END-IF                                                               
084800                                                                          
084900     PERFORM EAAC-KOLLA-OM-RAETT-DATUM                                    
085000     .                                                                    
085100     EJECT                                                                
085200 EAAB-KOLLA-OM-RAETT-KDLEVVIL SECTION.                                    
085300                                                                          
085400     MOVE MID-KDLEVVIL          TO W-473E-KDLEVVIL                        
085500                                                                          
085600     PERFORM IMS-GU-4735-WDR101-473E                                      
085700                                                                          
085800     IF SEGMENT-FINNS                                                     
085900       MOVE MID-KDLEVVIL        TO SPAR-KDLEVVIL                          
086000       MOVE MFS-NUM-FAELT-RAETT TO MOD-KDLEVVIL-ATTR                      
086100     ELSE                                                                 
086200       MOVE NEJ                 TO INDATA-SW                              
086300       MOVE MFS-NUM-FAELT-FEL   TO MOD-KDLEVVIL-ATTR                      
086400     END-IF                                                               
086500     .                                                                    
086600     EJECT                                                                
086700 EAAC-KOLLA-OM-RAETT-DATUM SECTION.                                       
086800                                                                          
086900     IF MID-TIGILTIG NOT = ALL '+' AND                                    
087000        MID-TIFORDAT NOT = ALL '+' AND                                    
087100        TIGILTIG-OK                AND                                    
087200        TIFORDAT-OK                                                       
087300                                                                          
087400       MOVE MID-TIGILTIG   TO TMP1-YYMMDD                                 
087500       MOVE MID-TIFORDAT   TO TMP2-YYMMDD                                 
087600       PERFORM WY2000P1                                                   
087700       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
087800         MOVE NEJ TO INDATA-SW                                            
087900         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIGILTIG-ATTR                    
088000       ELSE                                                               
088100         MOVE MID-TIGILTIG        TO SPAR-TIGILTIG                        
088200         MOVE MID-TIFORDAT        TO SPAR-TIFORDAT                        
088300         MOVE MFS-NUM-FAELT-RAETT TO MOD-TIGILTIG-ATTR                    
088400         MOVE MFS-NUM-FAELT-RAETT TO MOD-TIFORDAT-ATTR                    
088500       END-IF                                                             
088600                                                                          
088700     ELSE                                                                 
088800       IF MID-TIGILTIG NOT = ALL '+' AND                                  
088900          TIGILTIG-OK                                                     
089000                                                                          
089100         MOVE MID-TIGILTIG    TO TMP1-YYMMDD                              
089200         MOVE PHUV-TIFORDAT   TO TMP2-YYMMDD                              
089300         PERFORM WY2000P1                                                 
089400         IF TMP1-YYMMDD > TMP2-YYMMDD                                     
089500           MOVE NEJ TO INDATA-SW                                          
089600           MOVE MFS-NUM-FAELT-FEL   TO MOD-TIGILTIG-ATTR                  
089700         ELSE                                                             
089800           MOVE MID-TIGILTIG        TO SPAR-TIGILTIG                      
089900           MOVE MFS-NUM-FAELT-RAETT TO MOD-TIGILTIG-ATTR                  
090000         END-IF                                                           
090100                                                                          
090200       ELSE                                                               
090300         IF MID-TIFORDAT NOT = ALL '+' AND                                
090400            TIFORDAT-OK                                                   
090500                                                                          
090600           MOVE MID-TIFORDAT    TO TMP1-YYMMDD                            
090700           MOVE PHUV-TIGILTIG   TO TMP2-YYMMDD                            
090800           PERFORM WY2000P1                                               
090900           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
091000             MOVE NEJ TO INDATA-SW                                        
091100             MOVE MFS-NUM-FAELT-FEL   TO MOD-TIFORDAT-ATTR                
091200           ELSE                                                           
091300             MOVE MID-TIFORDAT        TO SPAR-TIFORDAT                    
091400             MOVE MFS-NUM-FAELT-RAETT TO MOD-TIFORDAT-ATTR                
091500           END-IF                                                         
091600         END-IF                                                           
091700       END-IF                                                             
091800     END-IF                                                               
091900     .                                                                    
092000     EJECT                                                                
092100 EAB-AENDRA-I-DB SECTION.                                                 
092200                                                                          
092300     IF MID-VKORDBTO NOT = ALL '+'                                        
092400       MOVE SPAR-VKORDBTO         TO PHUV-VKORDBTO                        
092500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VKORDBTO-ATTR                    
092600     END-IF                                                               
092700                                                                          
092800     IF MID-VLORDBTO NOT = ALL '+'                                        
092900       MOVE SPAR-VLORDBTO         TO PHUV-VLORDBTO                        
093000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VLORDBTO-ATTR                    
093100     END-IF                                                               
093200                                                                          
093300     IF MID-KDLEVVIL NOT = ALL '+'                                        
093400       MOVE SPAR-KDLEVVIL         TO PHUV-KDLEVVIL                        
093500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDLEVVIL-ATTR                    
093600     END-IF                                                               
093700                                                                          
093800     IF MID-TIGILTIG NOT = ALL '+'                                        
093900       MOVE SPAR-TIGILTIG         TO PHUV-TIGILTIG                        
094000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIGILTIG-ATTR                    
094100     END-IF                                                               
094200                                                                          
094300     IF MID-TIFORDAT NOT = ALL '+'                                        
094400       MOVE SPAR-TIFORDAT         TO PHUV-TIFORDAT                        
094500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIFORDAT-ATTR                    
094600     END-IF                                                               
094700                                                                          
094800     MOVE DAGENS-DATUM            TO PHUV-TIUPPDAT                        
094900     MOVE UPPDAT-TID              TO PHUV-TIUPPTID                        
095000                                                                          
095100     PERFORM IMS-REPL-PROC-WDE801                                         
095200     .                                                                    
095300     EJECT                                                                
095400 EB-STARTA-PROFORMA-UTSKRIFT SECTION.                                     
095500                                                                          
095600     IF PHUV-BELOSORT NOT = SPACE                                         
095700                                                                          
095800       MOVE NEJ TO PRELPR-SW                                              
095900                                                                          
096000       IF DIST79-DEALER-PRICE AND                                         
096100          PHUV-SUORDV-LOCPREL > 0                                         
096200          AND MID-FLAGGA-PRELPRIS = ALL '+' OR 'N'                        
096300                                                                          
096400           MOVE MFS-ADD-SAETT-CURSOR TO MOD-FLAGGA-PRELPRIS-ATTR          
096500           MOVE INFO-BEKR-PRELPRIS TO MED-IDMFSFEL                        
096600           PERFORM MFS-ROER-EJ-FAELT-IN                                   
096700           PERFORM MFS-ROER-EJ-FAELT-UT                                   
096800           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-UTSKRIFT-ATTR          
096900           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-RELEASE-ATTR           
097000           CALL WMEDKONV USING MED-WMEDAREA                               
097100           MOVE MED-MFSFEL         TO MOD-TEMFSINF                        
097200           MOVE NEJ                TO UTSKRIFT-SW                         
097300           MOVE JA                 TO PRELPR-SW                           
097400       ELSE                                                               
097500                                                                          
097600           MOVE 'PROFORMA'           TO CRUL-IDOUTTYPE                    
097700           MOVE MID-IDPRT-UTSKRIFT   TO WS-IDOUTREC-IDPRT                 
097800           MOVE PHUV-IDDISTR         TO WS-IDOUTREC-IDDISTR               
097900           MOVE WS-IDOUTREC-GRP      TO CRUL-IDOUTREC                     
098000                                                                          
098100           CALL WZ04CRUL  USING CRUL-WZ04CRUL                             
098200                                                                          
098300           IF CRUL-KDRC       = ZERO                                      
098400             PERFORM S06-KOLLA-ORDERBEKR                                  
098500             IF OBKR-OK                                                   
098600               PERFORM EBA-UTSKRIFT-AV-PROFORMA                           
098700             ELSE                                                         
098800               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRT-UTSKRIFT-ATTR         
098900               MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPRT-UTSKRIFT              
099000               MOVE FELM-ORDER-EJ-AVSLUT TO MED-IDMFSFEL                  
099100               CALL WMEDKONV USING MED-WMEDAREA                           
099200               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
099300             END-IF                                                       
099400                                                                          
099500           ELSE                                                           
099600             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRT-UTSKRIFT-ATTR           
099700             MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPRT-UTSKRIFT                
099800             MOVE FELM-FELAKTIG-PRINTER TO MED-IDMFSFEL                   
099900             CALL WMEDKONV USING MED-WMEDAREA                             
100000             MOVE MED-MFSFEL         TO MOD-TEMFSFEL                      
100100             MOVE NEJ                TO UTSKRIFT-SW                       
100200           END-IF                                                         
100300       END-IF                                                             
100400     ELSE                                                                 
100500       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPRT-UTSKRIFT-ATTR               
100600       MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPRT-UTSKRIFT                    
100700       MOVE FELM-DEST-SAKNAS     TO MED-IDMFSFEL                          
100800       CALL WMEDKONV USING MED-WMEDAREA                                   
100900       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
101000       MOVE NEJ                  TO UTSKRIFT-SW                           
101100     END-IF                                                               
101200     .                                                                    
101300     EJECT                                                                
101400 EBA-UTSKRIFT-AV-PROFORMA SECTION.                                        
101500                                                                          
101600     MOVE MFS-KDMFSFOR           TO PTOP1-KDMFSFOR                        
101700     MOVE PHUV-IDDISTR           TO PTOP1-MID-IDDISTR                     
101800     MOVE PHUV-IDKUNDNR          TO PTOP1-MID-IDKUNDNR                    
101900     MOVE PHUV-IDKUNDRF          TO PTOP1-MID-IDKUNDRF                    
102000     MOVE ZERO                   TO PTOP1-MID-IDSID                       
102100     MOVE 'PROF'                 TO PTOP1-MID-IDSYSTEM                    
102200     MOVE MID-IDPRT-UTSKRIFT TO PTOP1-MID-IDPRT                           
102300                                                                          
102400     MOVE ZERO                   TO PTOP1-MID-SUORDV                      
102500                                                                          
102600     MOVE PHUV-TIUPPDAT          TO PTOP1-MID-TIUPPDAT                    
102700     MOVE PHUV-TIUPPTID          TO PTOP1-MID-TIUPPTID                    
102800     MOVE LOW-VALUE              TO PTOP1-MID-NYCKEL-GRP                  
102900                                                                          
103000     MOVE JA                     TO UTSKRIFT-SW                           
103100     PERFORM IMS-ISRT-MSG-ALT1-4295                                       
103200     .                                                                    
103300     EJECT                                                                
103400 EC-STARTA-PROFORMA-RELEASE SECTION.                                      
103500                                                                          
103600     IF NOT OTILL-UPPDAT                                                  
103700                                                                          
103800       MOVE NEJ TO PRELPR-SW                                              
103900                                                                          
104000       IF DIST79-DEALER-PRICE AND                                         
104100          PHUV-SUORDV-LOCPREL > 0                                         
104200          AND MID-FLAGGA-PRELPRIS = ALL '+' OR 'N'                        
104300                                                                          
104400           MOVE MFS-ADD-SAETT-CURSOR TO MOD-FLAGGA-PRELPRIS-ATTR          
104500           MOVE INFO-BEKR-PRELPRIS TO MED-IDMFSFEL                        
104600           PERFORM MFS-ROER-EJ-FAELT-IN                                   
104700           PERFORM MFS-ROER-EJ-FAELT-UT                                   
104800           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-UTSKRIFT-ATTR          
104900           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-RELEASE-ATTR           
105000           CALL WMEDKONV USING MED-WMEDAREA                               
105100           MOVE MED-MFSFEL         TO MOD-TEMFSINF                        
105200           MOVE NEJ                TO UTSKRIFT-SW                         
105300           MOVE JA                 TO PRELPR-SW                           
105400       ELSE                                                               
105500         IF ((WS-IDKUNDNR NOT = WS-ENGANGS-KUND) AND                      
105600            (PHUV-KDPROTYP NOT = 'F' ))                                   
105700                                                                          
105800****** FIX START ATT RELAEASA PROFORMA                                    
105900*          OR                                                             
106000*          PHUV-IDDISTR = +2679 AND                                       
106100*          PHUV-IDKUNDNR = +999999 AND                                    
106200*          PHUV-IDORDER  = +7570271                                       
106300****** FIX SLUT                                                           
106400           MOVE WS-IDKUNDNR TO KREG-IDKUNDNR                              
106500           PERFORM EDE-KOLLA-MOT-W411KREG                                 
106600           IF KREG-KDKREDSP = '0' OR ' '                                  
106700* ' ' TILLFÄLLIGT GODKÄNT TL 020917                                       
106800             IF PHUV-BELOSORT NOT = SPACE                                 
106900                                                                          
107000               MOVE PHUV-TIFORDAT   TO TMP1-YYMMDD                        
107100               MOVE DAGENS-DATUM    TO TMP2-YYMMDD                        
107200               PERFORM WY2000P1                                           
107300               IF TMP1-YYMMDD > TMP2-YYMMDD                               
107400                                                                          
107500                 MOVE 'PROFORMA'        TO CRUL-IDOUTTYPE                 
107600                 MOVE MID-IDPRT-RELEASE TO WS-IDOUTREC-IDPRT              
107700                 MOVE PHUV-IDDISTR      TO WS-IDOUTREC-IDDISTR            
107800                 MOVE WS-IDOUTREC-GRP   TO CRUL-IDOUTREC                  
107900                                                                          
108000                 CALL WZ04CRUL USING CRUL-WZ04CRUL                        
108100                                                                          
108200                 IF CRUL-KDRC = ZERO                                      
108300                                                                          
108400                   PERFORM S06-KOLLA-ORDERBEKR                            
108500                   IF OBKR-OK                                             
108600                     PERFORM ECB-RELEASE-AV-PROFORMA                      
108700                   ELSE                                                   
108800                     MOVE MFS-ALFA-FAELT-FEL TO                           
108900                          MOD-IDPRT-RELEASE-ATTR                          
109000                     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPRT-RELEASE         
109100                     MOVE FELM-ORDER-EJ-AVSLUT TO MED-IDMFSFEL            
109200                     CALL WMEDKONV USING MED-WMEDAREA                     
109300                     MOVE MED-MFSFEL TO MOD-TEMFSFEL                      
109400                   END-IF                                                 
109500                 ELSE                                                     
109600                   MOVE MFS-ALFA-FAELT-FEL TO                             
109700                                        MOD-IDPRT-RELEASE-ATTR            
109800                   MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPRT-RELEASE           
109900                   MOVE FELM-FELAKTIG-PRINTER TO MED-IDMFSFEL             
110000                   CALL WMEDKONV USING MED-WMEDAREA                       
110100                   MOVE MED-MFSFEL TO MOD-TEMFSFEL                        
110200                 END-IF                                                   
110300                                                                          
110400               ELSE                                                       
110500                 MOVE MFS-ALFA-FAELT-FEL TO MOD-TIFORDAT-ATTR             
110600                 MOVE MFS-ROER-EJ-FAELT  TO MOD-TIFORDAT                  
110700                 PERFORM S05-VISA-INPUTFEL                                
110800               END-IF                                                     
110900                                                                          
111000             ELSE                                                         
111100               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRT-RELEASE-ATTR          
111200               MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPRT-RELEASE               
111300               MOVE FELM-DEST-SAKNAS   TO MED-IDMFSFEL                    
111400               CALL WMEDKONV USING MED-WMEDAREA                           
111500               MOVE MED-MFSFEL         TO MOD-TEMFSFEL                    
111600             END-IF                                                       
111700           ELSE                                                           
111800             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRT-RELEASE-ATTR            
111900             MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPRT-RELEASE                 
112000             MOVE FELM-BET-STOPPAD TO MED-IDMFSFEL                        
112100             CALL WMEDKONV USING MED-WMEDAREA                             
112200             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
112300           END-IF                                                         
112400         ELSE                                                             
112500           PERFORM S05-VISA-INPUTFEL                                      
112600         END-IF                                                           
112700       END-IF                                                             
112800     END-IF                                                               
112900     .                                                                    
113000     EJECT                                                                
113100 ECB-RELEASE-AV-PROFORMA SECTION.                                         
113200     MOVE MFS-KDMFSFOR      TO PTOP2-KDMFSFOR                             
113300     MOVE WS-IDDISTR        TO PTOP2-IDDISTR                              
113400     MOVE WS-IDKUNDNR       TO PTOP2-IDKUNDNR                             
113500     MOVE PHUV-IDKUNDRF     TO PTOP2-IDKUNDRF                             
113600     MOVE MID-IDPRT-RELEASE TO PTOP2-IDPRT                                
113700                                                                          
113800     MOVE JA                TO RELEASE-SW                                 
113900     PERFORM IMS-ISRT-MSG-ALT2-4269                                       
114000     .                                                                    
114100     EJECT                                                                
114200 ED-BEHANDLA-NYTT-KUNDNR SECTION.                                         
114300                                                                          
114400     IF NOT OTILL-UPPDAT                                                  
114500       IF WS-IDKUNDNR = WS-ENGANGS-KUND                                   
114600                                                                          
114700         IF MID-IDKUNDNR-NY NUMERIC                                       
114800                                                                          
114900           MOVE MID-IDKUNDNR-NY TO KREG-IDKUNDNR                          
115000           PERFORM EDE-KOLLA-MOT-W411KREG                                 
115100                                                                          
115200           IF ALLT-OK                                                     
115300                                                                          
115400             PERFORM EDC-KOLLA-IDKUNDNR-NY                                
115500                                                                          
115600             PERFORM EDD-KOLLA-FAKTURA-TYPER                              
115700                                                                          
115800             IF ALLT-OK                                                   
115900               PERFORM EDF-BYTA-TILL-NYTT-KUNDNR                          
116000             ELSE                                                         
116100               MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-NY-ATTR             
116200               MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR-NY                  
116300               MOVE FELM-DIST-KUND-SAKNAS TO MED-IDMFSFEL                 
116400               CALL WMEDKONV USING MED-WMEDAREA                           
116500               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
116600             END-IF                                                       
116700           ELSE                                                           
116800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-NY-ATTR               
116900             MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR-NY                    
117000             MOVE FELM-DIST-KUND-SAKNAS TO MED-IDMFSFEL                   
117100             CALL WMEDKONV USING MED-WMEDAREA                             
117200             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
117300           END-IF                                                         
117400         ELSE                                                             
117500           MOVE NEJ TO ALLT-SW                                            
117600         END-IF                                                           
117700       ELSE                                                               
117800         MOVE NEJ TO ALLT-SW                                              
117900       END-IF                                                             
118000                                                                          
118100       IF ALLT-OK                                                         
118200         CONTINUE                                                         
118300       ELSE                                                               
118400         IF MED-IDMFSFEL NOT = FELM-DIST-KUND-SAKNAS                      
118500            MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-NY-ATTR                
118600            MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR-NY                     
118700            MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                  
118800            CALL WMEDKONV USING MED-WMEDAREA                              
118900            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
119000         END-IF                                                           
119100       END-IF                                                             
119200     END-IF                                                               
119300     .                                                                    
119400     EJECT                                                                
119500 EDC-KOLLA-IDKUNDNR-NY SECTION.                                           
119600                                                                          
119700     MOVE NEJ TO ALLT-SW                                                  
119800                                                                          
119900     IF MID-IDKUNDNR-NY = WS-ENGANGS-KUND                                 
120000       IF PHUV-KDFAKTYP = 'G'                                             
120100         MOVE JA TO ALLT-SW                                               
120200       END-IF                                                             
120300     END-IF                                                               
120400                                                                          
120500     .                                                                    
120600     EJECT                                                                
120700 EDD-KOLLA-FAKTURA-TYPER SECTION.                                         
120800                                                                          
120900     MOVE NEJ TO ALLT-SW                                                  
121000                                                                          
121100     IF PHUV-KDFAKTYP = 'R'                                               
121200       IF KREG-FLOKFAK-R = JA OR YES                                      
121300         MOVE JA TO ALLT-SW                                               
121400       END-IF                                                             
121500                                                                          
121600     ELSE                                                                 
121700       IF PHUV-KDFAKTYP = 'G'                                             
121800         IF KREG-FLOKFAK-G = JA OR YES                                    
121900           MOVE JA TO ALLT-SW                                             
122000         END-IF                                                           
122100       END-IF                                                             
122200     END-IF                                                               
122300     .                                                                    
122400     EJECT                                                                
122500 EDE-KOLLA-MOT-W411KREG SECTION.                                          
122600                                                                          
122700     MOVE WS-IDDISTR           TO KREG-IDDISTR                            
122800     MOVE 'PROF'               TO KREG-IDSYSTEM                           
122810     IF DIST11-PROFORMA-DUBAI                                             
122820        CONTINUE                                                          
122830     ELSE                                                                 
122900        MOVE WC-CDC-SE         TO KREG-IDDC-TVS                           
122910     END-IF                                                               
123000     MOVE PHUV-KDFRAKT         TO KREG-KDFRAKT-IN                         
123100     MOVE PHUV-KDORDKL         TO KREG-KDORDKL                            
123200     MOVE SPACE                TO KREG-KDFAKTYP-IN                        
123300     MOVE NEJ                  TO KREG-FLVORKO                            
123400                                  KREG-FLVORFK                            
123500                                                                          
123600     CALL W411KREG USING KREG-W411KREG KREG-GMTA-PCB                      
123700                                       KREG-GMTB-PCB                      
123800                                       KREG-GMTC-PCB                      
123900                                       KREG-BETC-PCB                      
124000                                                                          
124100     PERFORM EDEA-KOLLA-FEL-KREG                                          
124200     .                                                                    
124300     EJECT                                                                
124400 EDEA-KOLLA-FEL-KREG SECTION.                                             
124500                                                                          
124600     IF KREG-IDDISTR-OK = NEJ                                             
124700        MOVE NEJ               TO ALLT-SW                                 
124800     END-IF                                                               
124900                                                                          
125000     IF KREG-IDKUNDNR-OK = NEJ                                            
125100        MOVE NEJ               TO ALLT-SW                                 
125200     END-IF                                                               
125300                                                                          
125400     IF KREG-KDFRAKT-OK = NEJ                                             
125500        MOVE NEJ               TO ALLT-SW                                 
125600     END-IF                                                               
125700     .                                                                    
125800     EJECT                                                                
125900 EDF-BYTA-TILL-NYTT-KUNDNR SECTION.                                       
126000                                                                          
126100     PERFORM IMS-DLET-PROC-WDE801                                         
126200                                                                          
126300     MOVE MID-IDKUNDNR-NY TO PHUV-IDKUNDNR                                
126400     PERFORM IMS-ISRT-PROC-WDE801                                         
126500                                                                          
126600     MOVE MID-IDKUNDNR-NY TO WS-IDKUNDNR-NY                               
126700     MOVE WS-IDKUNDNR-NY  TO MOD-IDKUNDNR-UT                              
126800     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
126900                                                                          
127000     IF MOD-IDKUNDNR-UT = SPACE                                           
127100       MOVE '     0' TO MOD-IDKUNDNR-UT                                   
127200     END-IF                                                               
127300                                                                          
127400     MOVE JA TO UPPDAT-SW                                                 
127500     .                                                                    
127600     EJECT                                                                
127700 EE-VISA-FEL SECTION.                                                     
127800                                                                          
127900     IF MID-INPUT          = ALL '+' AND                                  
128000        MID-IDPRT-UTSKRIFT = ALL '+' AND                                  
128100        MID-IDPRT-RELEASE  = ALL '+' AND                                  
128200        MID-IDKUNDNR-NY    = ALL '+'                                      
128300       MOVE FELM-PF11-O-EJ-INDATA TO MED-IDMFSFEL                         
128400                                                                          
128500     ELSE                                                                 
128600       PERFORM EEA-LYS-UPP-FEL-FAELT                                      
128700                                                                          
128800       MOVE FELM-MER-FUNK-VALD    TO MED-IDMFSFEL                         
128900                                                                          
129000     END-IF                                                               
129100                                                                          
129200     CALL WMEDKONV USING MED-WMEDAREA                                     
129300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
129400     .                                                                    
129500     EJECT                                                                
129600 EEA-LYS-UPP-FEL-FAELT SECTION.                                           
129700                                                                          
129800     MOVE NEJ TO INDATA-SW                                                
129900                                                                          
130000     IF MID-VKORDBTO NOT = ALL '+'                                        
130100       MOVE MFS-ALFA-FAELT-FEL  TO MOD-VKORDBTO-ATTR                      
130200       MOVE MFS-ROER-EJ-FAELT   TO MOD-VKORDBTO                           
130300     END-IF                                                               
130400                                                                          
130500     IF MID-VLORDBTO NOT = ALL '+'                                        
130600       MOVE MFS-ALFA-FAELT-FEL  TO MOD-VLORDBTO-ATTR                      
130700       MOVE MFS-ROER-EJ-FAELT   TO MOD-VLORDBTO                           
130800     END-IF                                                               
130900                                                                          
131000     IF MID-KDLEVVIL NOT = ALL '+'                                        
131100       MOVE MFS-NUM-FAELT-FEL   TO MOD-KDLEVVIL-ATTR                      
131200       MOVE MFS-ROER-EJ-FAELT   TO MOD-KDLEVVIL                           
131300     END-IF                                                               
131400                                                                          
131500     IF MID-TIGILTIG NOT = ALL '+'                                        
131600       MOVE MFS-NUM-FAELT-FEL   TO MOD-TIGILTIG-ATTR                      
131700       MOVE MFS-ROER-EJ-FAELT   TO MOD-TIGILTIG                           
131800     END-IF                                                               
131900                                                                          
132000     IF MID-TIFORDAT NOT = ALL '+'                                        
132100       MOVE MFS-NUM-FAELT-FEL   TO MOD-TIFORDAT-ATTR                      
132200       MOVE MFS-ROER-EJ-FAELT   TO MOD-TIFORDAT                           
132300     END-IF                                                               
132400                                                                          
132500     IF MID-IDPRT-UTSKRIFT NOT = ALL '+'                                  
132600       MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDPRT-UTSKRIFT-ATTR                
132700       MOVE MFS-ROER-EJ-FAELT   TO MOD-IDPRT-UTSKRIFT                     
132800     END-IF                                                               
132900                                                                          
133000     IF MID-IDPRT-RELEASE NOT = ALL '+'                                   
133100       MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDPRT-RELEASE-ATTR                 
133200       MOVE MFS-ROER-EJ-FAELT   TO MOD-IDPRT-RELEASE                      
133300     END-IF                                                               
133400                                                                          
133500     IF MID-IDKUNDNR-NY NOT = ALL '+'                                     
133600       MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-NY-ATTR                   
133700       MOVE MFS-ROER-EJ-FAELT   TO MOD-IDKUNDNR-NY                        
133800     END-IF                                                               
133900     .                                                                    
134000     EJECT                                                                
134100 F-BEHANDLA-ENTER SECTION.                                                
134200                                                                          
134300     IF MID-IDDISTR-IN  = ALL '+' AND                                     
134400        MID-IDKUNDNR-IN = ALL '+' AND                                     
134500        MID-IDORDNR7-IN = ALL '+' AND                                     
134600        MID-IDARTNR-IN  = ALL '+'                                         
134700                                                                          
134800       IF EGEN-MID AND                                                    
134900          (MID-INPUT NOT          = ALL '+' OR                            
135000           MID-IDPRT-UTSKRIFT NOT = ALL '+' OR                            
135100           MID-IDPRT-RELEASE NOT  = ALL '+' OR                            
135200           MID-IDKUNDNR-NY NOT    = ALL '+')                              
135300                                                                          
135400         MOVE INFO-TRYCK-PF11 TO MED-IDMFSFEL                             
135500         CALL WMEDKONV USING MED-WMEDAREA                                 
135600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
135700         MOVE NEJ TO INDATA-SW                                            
135800                                                                          
135900         PERFORM FA-LAES-IN-IGEN-INDATA                                   
136000         PERFORM FB-ROER-EJ-FAELT                                         
136100       ELSE                                                               
136200         PERFORM MFS-RENSA-FAELT-IN                                       
136300       END-IF                                                             
136400                                                                          
136500     ELSE                                                                 
136600       PERFORM MFS-RENSA-FAELT-IN                                         
136700     END-IF                                                               
136800     .                                                                    
136900     EJECT                                                                
137000 FA-LAES-IN-IGEN-INDATA SECTION.                                          
137100                                                                          
137200     IF MID-VKORDBTO NOT = ALL '+'                                        
137300       MOVE MFS-ROER-EJ-FAELT     TO MOD-VKORDBTO                         
137400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VKORDBTO-ATTR                    
137500     END-IF                                                               
137600                                                                          
137700     IF MID-VLORDBTO NOT = ALL '+'                                        
137800       MOVE MFS-ROER-EJ-FAELT     TO MOD-VLORDBTO                         
137900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VLORDBTO-ATTR                    
138000     END-IF                                                               
138100                                                                          
138200     IF MID-KDLEVVIL NOT = ALL '+'                                        
138300       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDLEVVIL                         
138400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDLEVVIL-ATTR                    
138500     END-IF                                                               
138600                                                                          
138700     IF MID-TIGILTIG NOT = ALL '+'                                        
138800       MOVE MFS-ROER-EJ-FAELT     TO MOD-TIGILTIG                         
138900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIGILTIG-ATTR                    
139000     END-IF                                                               
139100                                                                          
139200     IF MID-TIFORDAT NOT = ALL '+'                                        
139300       MOVE MFS-ROER-EJ-FAELT     TO MOD-TIFORDAT                         
139400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIFORDAT-ATTR                    
139500     END-IF                                                               
139600     .                                                                    
139700     EJECT                                                                
139800 FB-ROER-EJ-FAELT SECTION.                                                
139900                                                                          
140000     IF MID-IDPRT-UTSKRIFT NOT = ALL '+'                                  
140100       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDPRT-UTSKRIFT                   
140200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-UTSKRIFT-ATTR              
140300     END-IF                                                               
140400                                                                          
140500     IF MID-IDPRT-RELEASE NOT = ALL '+'                                   
140600       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDPRT-RELEASE                    
140700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-RELEASE-ATTR               
140800     END-IF                                                               
140900                                                                          
141000     IF MID-IDKUNDNR-NY NOT = ALL '+'                                     
141100       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDKUNDNR-NY                      
141200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKUNDNR-NY-ATTR                 
141300     END-IF                                                               
141400     .                                                                    
141500     EJECT                                                                
141600 G-LAES-VISA-BILD SECTION.                                                
141700                                                                          
141800     PERFORM IMS-GHU-PROC-WDE801-PHUV                                     
141900                                                                          
142000     MOVE PHUV-VKORDNTO    TO MOD-VKORDNTO                                
142100                                                                          
142200     IF DIST79-DEALER-PRICE                                               
142300       COMPUTE SPAR-SUFKTBEL = PHUV-SUORDV-LOC +                          
142400                               PHUV-SUORDV-LOCPREL                        
142500       MOVE PHUV-IDDISTR     TO TEST-IDDISTR                              
142600       IF DIST03-SVERIGE AND                                              
142700          PHUV-KDMOMSIN = +1                                              
142800          COMPUTE SPAR-SUFKTBEL = SPAR-SUFKTBEL  *                        
142900                                 (1              +                        
143000                                 (CONS-REMOMS    /                        
143100                                  100))                                   
143200       END-IF                                                             
143300       IF PHUV-SUORDV-LOCPREL > 0                                         
143400         MOVE '*'                 TO MOD-TEASTRIX                         
143500       ELSE                                                               
143600         MOVE ' '                 TO MOD-TEASTRIX                         
143700       END-IF                                                             
143800     ELSE                                                                 
143900       COMPUTE SPAR-SUFKTBEL = PHUV-SUORDV     +                          
144000                               PHUV-PRFRAKT    +                          
144100                               PHUV-PREMBHNT   +                          
144200                               PHUV-PRFOERS    +                          
144300                               PHUV-PRLEGKST   -                          
144400                               PHUV-PRAVDRAG                              
144500       MOVE PHUV-IDDISTR     TO TEST-IDDISTR                              
144600       IF DIST03-SVERIGE AND                                              
144700          PHUV-KDMOMSIN = +1                                              
144800          COMPUTE SPAR-SUFKTBEL = SPAR-SUFKTBEL  *                        
144900                                 (1              +                        
145000                                 (CONS-REMOMS    /                        
145100                                  100))                                   
145200       END-IF                                                             
145300       MOVE ' '                 TO MOD-TEASTRIX                           
145400     END-IF                                                               
145500     MOVE SPAR-SUFKTBEL    TO MOD-SUFKTBEL                                
145600                                                                          
145700     IF PHUV-KVMOTOR > ZERO                                               
145800       MOVE 'J'            TO MOD-PRODSL-KVMOTOR                          
145900     ELSE                                                                 
146000       MOVE 'N'            TO MOD-PRODSL-KVMOTOR                          
146100     END-IF                                                               
146200                                                                          
146300     IF PHUV-KVKAROSS > ZERO                                              
146400       MOVE 'J'            TO MOD-PRODSL-KVKAROSS                         
146500     ELSE                                                                 
146600       MOVE 'N'            TO MOD-PRODSL-KVKAROSS                         
146700     END-IF                                                               
146800                                                                          
146900     IF KDFARLIG-FINNS                                                    
147000       MOVE 'J'            TO MOD-KDFARLIG                                
147100     ELSE                                                                 
147200       MOVE 'N'            TO MOD-KDFARLIG                                
147300     END-IF                                                               
147400                                                                          
147500     MOVE MFS-STAENG-FAELT TO MOD-FLAGGA-PRELPRIS-ATTR                    
147600     MOVE MFS-RENSA-FAELT  TO MOD-FLAGGA-PRELPRIS                         
147700                                                                          
147800     PERFORM GA-FLYTTA-TILL-UPPDAT-FAELT                                  
147900     PERFORM GB-HAEMTA-BETVILLKOR                                         
148000     PERFORM GC-HAEMTA-LEVVILLKOR-TEXT                                    
148100                                                                          
148200     PERFORM GD-KOLLA-OM-UPPDAT-GJORD                                     
148300     .                                                                    
148400     EJECT                                                                
148500 GA-FLYTTA-TILL-UPPDAT-FAELT SECTION.                                     
148600                                                                          
148700     IF INDATA-FEL                                                        
148800                                                                          
148900       IF MID-VKORDBTO = ALL '+'                                          
149000         MOVE PHUV-VKORDBTO TO MOD-VKORDBTO                               
149100       END-IF                                                             
149200                                                                          
149300       IF MID-VLORDBTO = ALL '+'                                          
149400         MOVE PHUV-VLORDBTO TO MOD-VLORDBTO                               
149500       END-IF                                                             
149600                                                                          
149700       IF MID-KDLEVVIL = ALL '+'                                          
149800         MOVE PHUV-KDLEVVIL TO MOD-KDLEVVIL                               
149900       END-IF                                                             
150000                                                                          
150100       IF MID-TIGILTIG = ALL '+'                                          
150200         MOVE PHUV-TIGILTIG TO MOD-TIGILTIG                               
150300       END-IF                                                             
150400                                                                          
150500       IF MID-TIFORDAT = ALL '+'                                          
150600         MOVE PHUV-TIFORDAT TO MOD-TIFORDAT                               
150700       END-IF                                                             
150800     ELSE                                                                 
150900                                                                          
151000       MOVE PHUV-VKORDBTO   TO MOD-VKORDBTO                               
151100       MOVE PHUV-VLORDBTO   TO MOD-VLORDBTO                               
151200                                                                          
151300       MOVE PHUV-KDLEVVIL   TO MOD-KDLEVVIL                               
151400                                                                          
151500       MOVE PHUV-TIGILTIG   TO MOD-TIGILTIG                               
151600                                                                          
151700       MOVE PHUV-TIFORDAT   TO MOD-TIFORDAT                               
151800     END-IF                                                               
151900     .                                                                    
152000     EJECT                                                                
152100 GB-HAEMTA-BETVILLKOR SECTION.                                            
152200                                                                          
152300     MOVE SPACE              TO MOD-KDBETVIL                              
152400                                MOD-BEBETVIL                              
152500     MOVE WS-IDDISTR         TO W-IDDISTR                                 
152600     MOVE WS-IDKUNDNR        TO W-IDKUNDNR                                
152700     PERFORM IMS-GU-WDB201                                                
152800                                                                          
152900     IF SEGMENT-FINNS                                                     
153000       MOVE GMT-IDPARTNR     TO W-IDPARTNR                                
153100       MOVE GMT-IDFTG        TO W-IDFTG                                   
153200                                                                          
153300       PERFORM IMS-GU-WDB101                                              
153400       IF SEGMENT-FINNS                                                   
153500         MOVE BET-KDBETVIL   TO MOD-KDBETVIL                              
153600         MOVE BET-BEBETVIL   TO MOD-BEBETVIL                              
153700       END-IF                                                             
153800     END-IF                                                               
153900     .                                                                    
154000     EJECT                                                                
154100 GC-HAEMTA-LEVVILLKOR-TEXT SECTION.                                       
154200                                                                          
154300     IF PHUV-KDLEVVIL > ZERO                                              
154400       MOVE PHUV-KDLEVVIL TO W-473E-KDLEVVIL                              
154500       PERFORM IMS-GU-4735-WDR150-4735                                    
154600       IF SEGMENT-FINNS                                                   
154700         MOVE LEVVIL-BELEVVIL (SPRAK-IX) TO MOD-BELEVVIL                  
154800       ELSE                                                               
154900         MOVE SPACE                      TO MOD-BELEVVIL                  
155000       END-IF                                                             
155100                                                                          
155200     ELSE                                                                 
155300       INSPECT MOD-KDLEVVIL REPLACING LEADING ZERO BY SPACE               
155400       IF PHUV-PREMBHNT > ZERO AND                                        
155500          PHUV-PRFRAKT  = ZERO AND                                        
155600          PHUV-PRFOERS  = ZERO AND                                        
155700          PHUV-PRLEGKST = ZERO AND                                        
155800          PHUV-PRAVDRAG = ZERO                                            
155900           MOVE 'FCA GÖTEBORG'           TO MOD-BELEVVIL                  
156000                                                                          
156100       ELSE                                                               
156200         IF PHUV-PRFRAKT = ZERO                                           
156300             MOVE 'FCA             (INCOTERMS 2010)' TO                   
156400                                  MOD-BELEVVIL                            
156500                                                                          
156600         ELSE                                                             
156700           IF PHUV-PRFOERS = ZERO                                         
156800             MOVE 'CPT             (INCOTERMS 2010)' TO                   
156900                                  MOD-BELEVVIL                            
157000           ELSE                                                           
157100             MOVE 'CIP             (INCOTERMS 2010)' TO                   
157200                                  MOD-BELEVVIL                            
157300           END-IF                                                         
157400         END-IF                                                           
157500       END-IF                                                             
157600     END-IF                                                               
157700     .                                                                    
157800     EJECT                                                                
157900 GD-KOLLA-OM-UPPDAT-GJORD SECTION.                                        
158000                                                                          
158100     IF UPPDATERING-OK                                                    
158200       MOVE INFO-UPPDAT-GJORD TO MED-IDMFSINF                             
158300       PERFORM S04-CALL-WMEDKONV                                          
158400                                                                          
158500     ELSE                                                                 
158600       IF UTSKRIFT-OK                                                     
158700         MOVE INFO-KOAD-FOR-UTSKRIFT TO MED-IDMFSINF                      
158800         PERFORM S04-CALL-WMEDKONV                                        
158900                                                                          
159000       ELSE                                                               
159100         IF RELEASE-START                                                 
159200           MOVE MFS-RENSA-FAELT TO MOD-IDPRT-UTSKRIFT                     
159300                                   MOD-IDPRT-RELEASE                      
159400                                   MOD-IDKUNDNR-IN                        
159500         END-IF                                                           
159600       END-IF                                                             
159700     END-IF                                                               
159800     .                                                                    
159900     EJECT                                                                
160000 H-INSERT-MESAGE SECTION.                                                 
160100                                                                          
160200     IF RELEASE-START                                                     
160300       CONTINUE                                                           
160400***    PROFORMA RELEASE-PGM (4269) GÖR MSG-INSERT                         
160500     ELSE                                                                 
160600       PERFORM IMS-INSERT-MSG                                             
160700     END-IF                                                               
160800     .                                                                    
160900     EJECT                                                                
161000 S01-KOLLA-BEHOERIGHET SECTION.                                           
161100                                                                          
161200     IF PHUV-KDPROTYP = 'L'                                               
161300                                                                          
161400       IF PHUV-IDUSER NOT = MSG-SIGNON-USERID                             
161500                                                                          
161600         MOVE NEJ TO ALLT-SW                                              
161700         MOVE FELM-OBEHOERIG TO MED-IDMFSFEL                              
161800         CALL WMEDKONV USING MED-WMEDAREA                                 
161900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
162000                                                                          
162100       END-IF                                                             
162200     END-IF                                                               
162300     .                                                                    
162400     EJECT                                                                
162500 S02-KOLLA-OM-PRAD-FINNS SECTION.                                         
162600                                                                          
162700     IF ALLT-OK                                                           
162800                                                                          
162900       MOVE PHUV-IDORDER TO W-PRAD-IDORDER-MIN                            
163000                            W-PRAD-IDORDER-MAX                            
163100       PERFORM IMS-GU-PROD-WDE901-PRAD                                    
163200                                                                          
163300       IF SEGMENT-FINNS                                                   
163400         CONTINUE                                                         
163500                                                                          
163600       ELSE                                                               
163700         MOVE NEJ TO ALLT-SW                                              
163800         MOVE FELM-RADER-SAKNAS TO MED-IDMFSFEL                           
163900         CALL WMEDKONV USING MED-WMEDAREA                                 
164000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
164100       END-IF                                                             
164200                                                                          
164300     END-IF                                                               
164400     .                                                                    
164500     EJECT                                                                
164600 S03-VISA-OTILL-UPPDATERING SECTION.                                      
164700                                                                          
164800     MOVE NEJ TO OTILL-UPPDAT-SW                                          
164900     MOVE FELM-OTILL-UPPDATERING TO MED-IDMFSFEL                          
165000     CALL WMEDKONV USING MED-WMEDAREA                                     
165100     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
165200     .                                                                    
165300     SKIP3                                                                
165400 S04-CALL-WMEDKONV SECTION.                                               
165500                                                                          
165600     CALL WMEDKONV USING MED-WMEDAREA                                     
165700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
165800     MOVE MFS-RENSA-FAELT TO MOD-IDPRT-UTSKRIFT                           
165900                             MOD-IDPRT-RELEASE                            
166000                             MOD-IDKUNDNR-NY                              
166100                             MOD-FLAGGA-PRELPRIS                          
166200     .                                                                    
166300     SKIP3                                                                
166400 S05-VISA-INPUTFEL SECTION.                                               
166500                                                                          
166600     MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDPRT-RELEASE-ATTR               
166700     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDPRT-RELEASE                    
166800     MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                         
166900     CALL WMEDKONV USING MED-WMEDAREA                                     
167000     MOVE MED-MFSFEL              TO MOD-TEMFSFEL                         
167100     .                                                                    
167200     EJECT                                                                
167300 S06-KOLLA-ORDERBEKR SECTION.                                             
167400                                                                          
167500     MOVE JA                TO OBKR-SW                                    
167600     MOVE LOW-VALUE         TO W-WDQ101KY-MIN-X                           
167700     MOVE HIGH-VALUE        TO W-WDQ101KY-MAX-X                           
167800     MOVE PHUV-IDORDER      TO W-WDQ101-MIN-IDORDER                       
167900                               W-WDQ101-MAX-IDORDER                       
168000                                                                          
168100     PERFORM IMS-GU-ORQM-WDQ101                                           
168200                                                                          
168300     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
168400                   BASEN-SLUT      OR                                     
168500                   OBKR-FEL                                               
168600        IF OBKR-FLOBOK = NEJ                                              
168700           MOVE NEJ TO OBKR-SW                                            
168800        ELSE                                                              
168900           PERFORM IMS-GN-ORQM-WDQ101                                     
169000        END-IF                                                            
169100     END-PERFORM                                                          
169200     .                                                                    
169300     EJECT                                                                
169400 S07-KOLL-OM-PRAD-FARLIG-FINNS SECTION.                                   
169500                                                                          
169600     IF ALLT-OK                                                           
169700       MOVE NEJ TO FARLIG-SW                                              
169800                                                                          
169900       MOVE PHUV-IDORDER TO W-PRAD-IDORDER-MIN                            
170000                            W-PRAD-IDORDER-MAX                            
170100       PERFORM IMS-GU-PROD-WDE901-PRAD                                    
170200                                                                          
170300       IF SEGMENT-FINNS                                                   
170400         PERFORM UNTIL SEGMENT-SAKNAS OR                                  
170500           KDFARLIG-FINNS                                                 
170600                                                                          
170700           MOVE PRAD-IDARTNR TO W-WDK6-IDARTNR                            
170800           PERFORM IMS-GU-WDK611                                          
170900           IF SEGMENT-FINNS                                               
171000             IF WDK6-CLAG-KDFARLIG = 4                                    
171100             OR WDK6-CLAG-KDFARLIG = 7                                    
171200               MOVE JA TO FARLIG-SW                                       
171300             END-IF                                                       
171400           END-IF                                                         
171500           PERFORM IMS-GN-PROD-WDE901-PRAD                                
171600         END-PERFORM                                                      
171700                                                                          
171800       ELSE                                                               
171900         MOVE NEJ TO ALLT-SW                                              
172000         MOVE FELM-RADER-SAKNAS TO MED-IDMFSFEL                           
172100         CALL WMEDKONV USING MED-WMEDAREA                                 
172200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
172300       END-IF                                                             
172400                                                                          
172500     END-IF                                                               
172600     .                                                                    
172700     EJECT                                                                
172800 MFS-RENSA-FAELT-UT SECTION.                                              
172900                                                                          
173000*    --- ALLA UTDATA-FÄLT                                                 
173100     MOVE MFS-RENSA-FAELT TO MOD-VKORDNTO                                 
173200                             MOD-SUFKTBEL                                 
173300                             MOD-PRODSL-KVMOTOR                           
173400                             MOD-PRODSL-KVKAROSS                          
173500                             MOD-KDFARLIG                                 
173600                             MOD-VKORDBTO                                 
173700                             MOD-VLORDBTO                                 
173800                             MOD-KDLEVVIL                                 
173900                             MOD-BELEVVIL                                 
174000                             MOD-TIGILTIG                                 
174100                             MOD-TIFORDAT                                 
174200                             MOD-IDPRT-UTSKRIFT                           
174300                             MOD-IDPRT-RELEASE                            
174400                             MOD-IDKUNDNR-NY                              
174500                             MOD-FLAGGA-PRELPRIS                          
174600     .                                                                    
174700     SKIP3                                                                
174800 MFS-RENSA-FAELT-IN SECTION.                                              
174900                                                                          
175000*    --- ALLA INDATA-FÄLT                                                 
175100     MOVE MFS-RENSA-FAELT TO MOD-VKORDBTO                                 
175200                             MOD-VLORDBTO                                 
175300                             MOD-KDLEVVIL                                 
175400                             MOD-TIGILTIG                                 
175500                             MOD-TIFORDAT                                 
175600                             MOD-IDPRT-UTSKRIFT                           
175700                             MOD-IDPRT-RELEASE                            
175800                             MOD-IDKUNDNR-NY                              
175900                             MOD-FLAGGA-PRELPRIS                          
176000     .                                                                    
176100     EJECT                                                                
176200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
176300                                                                          
176400*    --- ALLA INDATA-FÄLT                                                 
176500     MOVE MFS-ROER-EJ-FAELT TO MOD-VKORDBTO                               
176600                               MOD-VLORDBTO                               
176700                               MOD-KDLEVVIL                               
176800                               MOD-TIGILTIG                               
176900                               MOD-TIFORDAT                               
177000                               MOD-IDPRT-UTSKRIFT                         
177100                               MOD-IDPRT-RELEASE                          
177200                               MOD-IDKUNDNR-NY                            
177300                               MOD-FLAGGA-PRELPRIS                        
177400     .                                                                    
177500 MFS-ROER-EJ-FAELT-UT SECTION.                                            
177600                                                                          
177700*    --- ALLA UTDATA-FÄLT                                                 
177800     MOVE MFS-ROER-EJ-FAELT TO MOD-VKORDNTO                               
177900                               MOD-SUFKTBEL                               
178000                               MOD-PRODSL-KVMOTOR                         
178100                               MOD-PRODSL-KVKAROSS                        
178200                               MOD-KDFARLIG                               
178300                               MOD-VKORDBTO                               
178400                               MOD-VLORDBTO                               
178500                               MOD-KDBETVIL                               
178600                               MOD-BEBETVIL                               
178700                               MOD-KDLEVVIL                               
178800                               MOD-BELEVVIL                               
178900                               MOD-TIGILTIG                               
179000                               MOD-TIFORDAT                               
179100                               MOD-IDPRT-UTSKRIFT                         
179200                               MOD-IDPRT-RELEASE                          
179300                               MOD-IDKUNDNR-NY                            
179400                               MOD-FLAGGA-PRELPRIS                        
179500     .                                                                    
179600     EJECT                                                                
179700* --- IMS SEKTIONER ---                                                   
179800     SKIP3                                                                
179900 IMS-GET-MSG SECTION.                                                     
180000                                                                          
180100     MOVE '  QC' TO GODK-STATUSKODER                                      
180200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
180300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
180400     PERFORM IMS-STATUSKONTROLL                                           
180500     .                                                                    
180600     SKIP3                                                                
180700 IMS-INSERT-MSG SECTION.                                                  
180800                                                                          
180900     IF ENGLISH-TEXT                                                      
181000       MOVE 'N' TO MFS-KDHUVOMR                                           
181100     END-IF                                                               
181200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
181300     MOVE SPACE TO GODK-STATUSKODER                                       
181400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
181500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
181600     PERFORM IMS-STATUSKONTROLL                                           
181700     .                                                                    
181800     EJECT                                                                
181900 IMS-ISRT-MSG-ALT1-4295 SECTION.                                          
182000                                                                          
182100     MOVE SPACE TO GODK-STATUSKODER                                       
182200     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
182300     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
182400     PERFORM IMS-STATUSKONTROLL                                           
182500     .                                                                    
182600     SKIP3                                                                
182700 IMS-ISRT-MSG-ALT2-4269 SECTION.                                          
182800                                                                          
182900     MOVE SPACE TO GODK-STATUSKODER                                       
183000     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW2                          
183100     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
183200     PERFORM IMS-STATUSKONTROLL                                           
183300     .                                                                    
183400     EJECT                                                                
183500 IMS-GHU-PROC-WDE801-PHUV SECTION.                                        
183600                                                                          
183700     STRING 'WLPROC01(WDE801KY =' W-WDE801KY-X ')'                        
183800          DELIMITED BY SIZE INTO SSA1                                     
183900     MOVE '  GE' TO GODK-STATUSKODER                                      
184000     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-AREA-WDE801 SSA1              
184100     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
184200     PERFORM IMS-STATUSKONTROLL                                           
184300     .                                                                    
184400     SKIP3                                                                
184500 IMS-GU-PROD-WDE901-PRAD SECTION.                                         
184600                                                                          
184700     STRING 'WLPROD01(WDE901KY=>' W-WDE901KY-MIN-X                        
184800                    '&WDE901KY=<' W-WDE901KY-MAX-X ')'                    
184900          DELIMITED BY SIZE INTO SSA1                                     
185000     MOVE '  GE' TO GODK-STATUSKODER                                      
185100     CALL CBLTDLI USING GU PROD-PCB DLI-IO-AREA-WDE901 SSA1               
185200     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
185300     PERFORM IMS-STATUSKONTROLL                                           
185400     .                                                                    
185500     SKIP3                                                                
185600 IMS-GN-PROD-WDE901-PRAD SECTION.                                         
185700                                                                          
185800     STRING 'WLPROD01(WDE901KY=>' W-WDE901KY-MIN-X                        
185900                    '&WDE901KY=<' W-WDE901KY-MAX-X ')'                    
186000          DELIMITED BY SIZE INTO SSA1                                     
186100     MOVE '  GE' TO GODK-STATUSKODER                                      
186200     CALL CBLTDLI USING GN PROD-PCB DLI-IO-AREA-WDE901 SSA1               
186300     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
186400     PERFORM IMS-STATUSKONTROLL                                           
186500     .                                                                    
186600     EJECT                                                                
186700 IMS-GU-WDK611      SECTION.                                              
186800                                                                          
186900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
187000          DELIMITED BY SIZE INTO SSA1                                     
187100     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
187200          DELIMITED BY SIZE INTO SSA2                                     
187300     MOVE '  GE' TO GODK-STATUSKODER                                      
187400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
187500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
187600     PERFORM IMS-STATUSKONTROLL                                           
187700     .                                                                    
187800     SKIP3                                                                
187900 IMS-GU-4735-WDR101-473E SECTION.                                         
188000                                                                          
188100     STRING 'WL473501(WDGXKEY  =' W-WDGX473E-X ')'                        
188200          DELIMITED BY SIZE INTO SSA1                                     
188300     MOVE '  GE' TO GODK-STATUSKODER                                      
188400     CALL CBLTDLI USING GU 4735-PCB DLI-IO-AREA-WDGX473E SSA1             
188500     MOVE 4735-STATUS-CODE TO STATUS-WS                                   
188600     PERFORM IMS-STATUSKONTROLL                                           
188700     .                                                                    
188800     EJECT                                                                
188900 IMS-GU-4735-WDR150-4735 SECTION.                                         
189000                                                                          
189100     STRING 'WL473501(WDGXKEY  =' W-WDGX473E-X ')'                        
189200          DELIMITED BY SIZE INTO SSA1                                     
189300     MOVE 'WL473511 ' TO SSA2                                             
189400     MOVE '  GE' TO GODK-STATUSKODER                                      
189500     CALL CBLTDLI USING GU 4735-PCB DLI-IO-AREA-WDGX4735                  
189600                                    SSA1 SSA2                             
189700     MOVE 4735-STATUS-CODE TO STATUS-WS                                   
189800     PERFORM IMS-STATUSKONTROLL                                           
189900     .                                                                    
190000     SKIP3                                                                
190100 IMS-REPL-PROC-WDE801 SECTION.                                            
190200                                                                          
190300     MOVE '    ' TO GODK-STATUSKODER                                      
190400     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-AREA-WDE801                  
190500     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
190600     PERFORM IMS-STATUSKONTROLL                                           
190700     .                                                                    
190800     EJECT                                                                
190900 IMS-DLET-PROC-WDE801 SECTION.                                            
191000                                                                          
191100     MOVE '    ' TO GODK-STATUSKODER                                      
191200     CALL CBLTDLI USING DLET PROC-PCB DLI-IO-AREA-WDE801                  
191300     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
191400     PERFORM IMS-STATUSKONTROLL                                           
191500     .                                                                    
191600     SKIP3                                                                
191700 IMS-ISRT-PROC-WDE801 SECTION.                                            
191800                                                                          
191900     MOVE 'WLPROC01 ' TO SSA1                                             
192000     MOVE '    ' TO GODK-STATUSKODER                                      
192100     CALL CBLTDLI USING ISRT PROC-PCB DLI-IO-AREA-WDE801 SSA1             
192200     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
192300     PERFORM IMS-STATUSKONTROLL                                           
192400     .                                                                    
192500     EJECT                                                                
192600 IMS-GU-ORQM-WDQ101 SECTION.                                              
192700     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
192800                    '&WDQ101KY<=' W-WDQ101KY-MAX-X                        
192900                    '&IDSYSTEM =' W-WDQ101-IDSYSTEM ')'                   
193000          DELIMITED BY SIZE INTO SSA1                                     
193100     MOVE '  GE' TO GODK-STATUSKODER                                      
193200     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA-WDQ101 SSA1               
193300     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
193400     PERFORM IMS-STATUSKONTROLL                                           
193500     .                                                                    
193600     EJECT                                                                
193700 IMS-GN-ORQM-WDQ101 SECTION.                                              
193800     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
193900                    '&WDQ101KY<=' W-WDQ101KY-MAX-X                        
194000                    '&IDSYSTEM =' W-WDQ101-IDSYSTEM ')'                   
194100          DELIMITED BY SIZE INTO SSA1                                     
194200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
194300     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-AREA-WDQ101 SSA1               
194400     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
194500     PERFORM IMS-STATUSKONTROLL                                           
194600     .                                                                    
194700     EJECT                                                                
194800 IMS-GU-WDB201 SECTION.                                                   
194900                                                                          
195000     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
195100          DELIMITED BY SIZE INTO SSA1                                     
195200     MOVE '  GE' TO GODK-STATUSKODER                                      
195300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
195400     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
195500     PERFORM IMS-STATUSKONTROLL                                           
195600     .                                                                    
195700     EJECT                                                                
195800 IMS-GU-WDB101 SECTION.                                                   
195900                                                                          
196000     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
196100          DELIMITED BY SIZE INTO SSA1                                     
196200     MOVE '  GE' TO GODK-STATUSKODER                                      
196300     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
196400     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
196500     PERFORM IMS-STATUSKONTROLL                                           
196600     .                                                                    
196700     EJECT                                                                
196800 IMS-STATUSKONTROLL SECTION.                                              
196900                                                                          
197000     SET STATUS-IX TO 1                                                   
197100     SEARCH GODK-STATUS                                                   
197200       AT END                                                             
197300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
197400         DELIMITED BY SIZE INTO FELTEXT                                   
197500         CALL FELLOG                                                      
197600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
197700     END-SEARCH                                                           
197800     .                                                                    
197900     EJECT                                                                
198000*    -COPY WY2000P1                                                       
