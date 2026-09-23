000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4026300.                                                
000400 AUTHOR.         ANNELIE ENGLUND                                          
000500 DATE-WRITTEN.   APRIL -91.                                               
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR SVARSBILD TILL PROFORMAREGISTRERING          
001100*        4261/4262.                                                       
001200*        VISAR AVVIKELSER.                                                
001300*        UPPDATERING AV GODKÄNDA RADER. FLOBOK BLIR = 'J'.                
001400*                                                                         
001500*                                                                         
001600*        EFTER AVSLUTAD BEHANDLING SKER UTHOPP TILL ORDERHUVUD            
001700*        4261.                                                            
001800*                                                                         
001900*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
002000*        PROGRAMMET UPPDATERAR WLORQM (WDQ1)  ORDERBEKR.BAS               
002100*        PROGRAMMET UPPDATERAR WLPROC (WDE8)  PROFORMAHUVUD               
002200*        PROGRAMMET UPPDATERAR WLPROD (WDE9)  PROFORMARAD                 
002300*        PROGRAMMET LÄSER      WLARTM (WDK9)  ARTIKELREGISTER             
002400*        PROGRAMMET LÄSER      WLBENA (WDD3)  BENÄMNINGSREGISTER          
002500*        PROGRAMMET UPPDATERAR WLXXBU (WDR5)  LARMKÖ                      
002600*        PROGRAMMET LÄSER      WL4437 (WDR1)  ARBETS-KALENDER             
002700*        PROGRAMMET LÄSER      WLSATB (WDJ1)  SATSREGISTER                
002800*        PROGRAMMET LÄSER      WLXXBV (WDR2)  TIDSBEKRÄFTELSE             
002900*        PROGRAMMET LÄSER      WLXXBX (WDR2)  ÖVERSÄTTN ANSK-LARM         
003000*                                                                         
003100*    INDATA.                                                              
003200*        TRANSAKTION: W4T263                                              
003300*                     W4T263U                                             
003400*                     W4T263V                                             
003500*                                                                         
003600*        MID:         W4I26301                                            
003700*                                                                         
003800*    UTDATA.                                                              
003900*        MOD:         W4O26301                                            
004000*                                                                         
004100*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
004200     EJECT                                                                
004300 ENVIRONMENT DIVISION.                                                    
004400                                                                          
004500 DATA DIVISION.                                                           
004600 WORKING-STORAGE SECTION.                                                 
004700*    -COPY WY2000W1                                                       
004800     SKIP3                                                                
004900 77  IDPGM                       PIC X(08)   VALUE 'W4026300'.            
005000 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
005100 77  YES                         PIC X(1)   VALUE 'Y'.                    
005200 77  HOPP                        PIC X(1)   VALUE 'N'.                    
005300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1026 COMP SYNC.        
005500 77  4261-MOD-LAENGD             PIC S9(4)  VALUE +44   COMP SYNC.        
005600 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
005700 77  WS-RADER                    PIC 9(2)   VALUE ZERO.                   
005800 77  4262-MID-IX                 PIC S9(9)   COMP SYNC VALUE ZERO.        
005900 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
006000 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
006100 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +13.         
006200 77  WS-INDEX-MOD                PIC S9(9)   COMP SYNC VALUE ZERO.        
006300 77  WS-INDEX-MOD-MAX            PIC S9(9)   COMP SYNC VALUE +13.         
006400 77  WS-INDEX-SATS               PIC S9(9)   COMP SYNC VALUE ZERO.        
006500 77  WS-INDEX-SATS-MAX           PIC S9(9)   COMP SYNC VALUE +5.          
006600 77  SPAR-PRAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
006700 77  SPAR-IDARTNR-TILLK          PIC S9(9)   VALUE +0  COMP-3.            
006800 77  SPAR-KVBEART-TILLK          PIC S9(7)   VALUE +0  COMP-3.            
006900 77  SPAR-REKSIFFR-TILLK         PIC S9(1)   VALUE +0  COMP-3.            
007000 77  SPAR-DIERS-KVOT          PIC S9(4)V9(3) VALUE +0  COMP-3.            
007100 77  SPAR-IDARTNR-40             PIC S9(9)   VALUE +0  COMP-3.            
007200 77  SPAR-IDLOPNR-40             PIC S9(3)   VALUE +0  COMP-3.            
007300 77  SPAR-IDSEKVNR-40            PIC S9(3)   VALUE +0  COMP-3.            
007400 77  WS-IDDISTR                  PIC X(4).                                
007500 77  WS-IDKUNDNR                 PIC X(6).                                
007600 77  WS-IDORDNR                  PIC X(7).                                
007700 77  WS-TIFORDAT                 PIC 9(9)    VALUE ZERO.                  
007800 77  WS-SUORDV                   PIC S9(9)V9(2) VALUE +0  COMP-3.         
007900 77  WS-SUORDV-LOC               PIC S9(9)V9(2) VALUE +0  COMP-3.         
008000 77  WS-SUORDV-LOCPREL           PIC S9(9)V9(2) VALUE +0  COMP-3.         
008100 77  WS-VLORDBTO                 PIC S9(4)V9(3) VALUE +0  COMP-3.         
008200 77  WS-VKORDNTO                 PIC S9(6)V9(1) VALUE +0  COMP-3.         
008300 77  WS-SUORDV-RAKN              PIC S9(9)V9(2) VALUE +0  COMP-3.         
008400 77  WS-SUORDV-RAKN-LOC         PIC S9(9)V9(2) VALUE +0  COMP-3.          
008500 77  WS-SUORDV-RAKN-LOCPREL      PIC S9(9)V9(2) VALUE +0  COMP-3.         
008600 77  WS-SA-VLARTNTO              PIC S9(8)V9(1) VALUE +0  COMP-3.         
008700 77  WS-SA-VLARTNTO-RAKN         PIC S9(9)V9(1) VALUE +0  COMP-3.         
008800 77  WS-SA-VKART                 PIC S9(9)      VALUE +0  COMP-3.         
008900 77  WS-SA-VKART-RAKN            PIC S9(9)      VALUE +0  COMP-3.         
009000 77  WS-KVMOTOR                  PIC S9(3)   VALUE +0.                    
009100 77  WS-KVKAROSS                 PIC S9(3)   VALUE +0.                    
009200     EJECT                                                                
009300                                                                          
009400 77  STARTAD-AV-DISPATCHEN-SW    PIC X       VALUE 'N'.                   
009500     88  STARTAD-AV-DISPATCHEN               VALUE 'J'.                   
009600 77  ALLT-SW                     PIC X       VALUE 'J'.                   
009700     88  ALLT-OK                             VALUE 'J'.                   
009800 77  NYCKEL-SW                   PIC X       VALUE 'J'.                   
009900     88  NYCKEL-OK                           VALUE 'J'.                   
010000 77  AKT-SIDA-SW                 PIC X       VALUE 'N'.                   
010100     88  AKT-SIDA                            VALUE 'J'.                   
010200 77  AVSLUTA-SW                  PIC X       VALUE 'N'.                   
010300     88  AVSLUTA                             VALUE 'J'.                   
010400 77  START-4262-SW               PIC X       VALUE 'N'.                   
010500     88  START-4262                          VALUE 'J'.                   
010600 77  STATUS-OBKR-SW              PIC X(2)    VALUE 'GE'.                  
010700     88  OBKR-SEGMENT-FINNS                  VALUE '  '.                  
010800 77  NEXT-SATS-SW                PIC X       VALUE 'N'.                   
010900     88  NEXT-SATS                           VALUE 'J'.                   
011000 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
011100     88  OMSTART                             VALUE 'J'.                   
011200                                                                          
011300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011400     88  EGEN-MID                            VALUE '4263'.                
011500     88  GODK-MID                            VALUE '4262' '4263'          
011600                                                   '4264' '4265'          
011700                                                   '4266' '4267'          
011800                                                   '4268'.                
011900*    --- VALID IDDC CODES                                                 
012000*                                                                         
012100*01  -COPY WWDCKONS                                                       
012200*                                                                         
012300*    ----DISTR-DEALER-PRICE----                                           
012400*01  -COPY  WWDIST79                                                      
012500*                                                                         
012600 01  W-TISENBEK.                                                          
012700     03  W-TISENBEK-DAG          PIC 9(6).                                
012800     03  W-TISENBEK-KL           PIC 9(6).                                
012900                                                                          
013000 01  W-TISENBEK-KL-UPPD.                                                  
013100     03  W-TISENBEK-KL-HH        PIC 9(2)    VALUE ZERO.                  
013200     03  W-TISENBEK-KL-MM        PIC 9(2)    VALUE ZERO.                  
013300     03  W-TISENBEK-KL-SS        PIC 9(2)    VALUE ZERO.                  
013400                                                                          
013500 01  DAGENS-TID                  PIC 9(8)   VALUE ZERO.                   
013600 01  FILLER REDEFINES DAGENS-TID.                                         
013700     03 DAGENS-HHMMSS            PIC 9(6).                                
013800     03 FILLER                   PIC 9(2).                                
013900     EJECT                                                                
014000                                                                          
014100 01  WS-ALFA-6.                                                           
014200     03  WS-NUM-6                PIC 9(6).                                
014300 01  WS-ALFA-9.                                                           
014400     03  WS-NUM-9                PIC 9(9).                                
014500                                                                          
014600 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
014700 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
014800     03  WS-19-TAL-9KOMPL        PIC 9(2).                                
014900     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
015000     EJECT                                                                
015100 01  WS-AKTUELL-MID-RAD.                                                  
015200     03  WS-AKT-KDORDBEK         PIC 9(2).                                
015300     03  WS-AKT-KDBEHX           PIC X(1).                                
015400     03  WS-AKT-IDARTNR          PIC 9(9).                                
015500     03  WS-AKT-FILLER           PIC X(1).                                
015600     03  WS-AKT-REKSIFFR         PIC 9(1).                                
015700     03  WS-AKT-KEYS.                                                     
015800         05 WS-AKT-IDLOPNR       PIC 9(3).                                
015900         05 WS-AKT-IDSEKVNR      PIC 9(3).                                
016000         05 WS-AKT-IDARTNR-URS   PIC 9(9).                                
016100                                                                          
016200 01  WS-IDARTNR-SATS-TAB.                                                 
016300     05 WS-IDARTNR-SATS          PIC X(9)    OCCURS 5.                    
016301                                                                          
016310 01  WS-IDARTNR-REKSIFFR.                                                 
016320     03  WS-IDARTNR              PIC 9(9).                                
016330     03  FILLER                  PIC X(1)   VALUE '-'.                    
016340     03  WS-REKSIFFR             PIC 9(1).                                
016350                                                                          
016400                                                                          
016500 01  WS-KEYS-SPAR.                                                        
016600     03 WS-IDLOPNR-SPAR          PIC 9(3).                                
016700     03 WS-IDSEKVNR-SPAR         PIC 9(3).                                
016800     03 WS-IDARTNR-URS-SPAR      PIC 9(9).                                
016900     03 WS-IDARTNR-SPAR          PIC 9(9).                                
017000     EJECT                                                                
017100                                                                          
017200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017300 01  GENERELLA-SUBPROGRAM.                                                
017400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
017500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017800*                                                                         
017900*                                                                         
018000 01  GEMENSAMMA-SUBPROGRAM.                                               
018100     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
018200*        LÄSNING ARTIKELREGISTER                                          
018300     03  W411TIME                PIC X(8)    VALUE 'W411TIME'.            
018400*        TIDBERÄKNING                                                     
018500     EJECT                                                                
018600*   -COPY W402W001                                                        
018700     EJECT                                                                
018800*    --- PARAMETRAR TILL SUBPROGRAM ABEND                                 
018900                                                                          
019000 01  RKOD-ABEND                  PIC S9(4) VALUE +33 COMP SYNC.           
019100                                                                          
019200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
019300 01  MESSAGE-CODES.                                                       
019400     03  MED-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
019500     03  MED-FLER-SIDOR          PIC X(3)    VALUE '105'.                 
019600     03  MED-UPPDAT-PF           PIC X(3)    VALUE '144'.                 
019700     03  MED-EJ-FLER-RADER       PIC X(3)    VALUE '056'.                 
019800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
019900     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
020000     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
020100     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '057'.                 
020200     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '409'.                 
020300     03  ERR-ORDER-ANNULLERAD    PIC X(3)    VALUE '052'.                 
020400     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
020500     SKIP2                                                                
020600*   -COPY WMEDAREA                                                        
020700     EJECT                                                                
020800*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
020900 01 FILLER                       PIC X(8)    VALUE 'W411AREG'.            
021000*   -COPY W411AREG                                                        
021100     EJECT                                                                
021200 01 FILLER                       PIC X(8)    VALUE 'W411TIME'.            
021300*   -COPY W411TIME                                                        
021400     EJECT                                                                
021500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
021600*                                                                         
021700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021800     SKIP3                                                                
021900*01  MID -COPY W4I26301                                                   
022000     EJECT                                                                
022100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
022200*01  -COPY WMSGAREA                                                       
022300     EJECT                                                                
022400*    03  MOD -COPY W4O26301   -RED MSG-AREA.                              
022500     EJECT                                                                
022600 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
022700     SKIP3                                                                
022800 01  KOM-IO-AREA.                                                         
022900*03  -COPY WMSGKOM                                                        
023000     EJECT                                                                
023100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
023200     SKIP3                                                                
023300*01  -COPY WMFSAREA                                                       
023400     EJECT                                                                
023500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023700 01  NYCKLAR-TILL-DLI.                                                    
023800     03  W-WDQ101-KEY-UNIK.                                               
023900         05  W-Q1-IDORDER-UNIK   PIC S9(7)   VALUE ZERO COMP-3.           
024000         05  W-Q1-IDARTNR-UNIK   PIC S9(9)   VALUE ZERO COMP-3.           
024100         05  W-Q1-IDLOPNR-UNIK   PIC S9(3)   VALUE ZERO COMP-3.           
024200         05  W-Q1-IDSEKVNR-UNIK  PIC S9(3)   VALUE ZERO COMP-3.           
024300         05  W-Q1-IDDC-UNIK      PIC X(2)    VALUE '11'.                  
024400         05  W-Q1-KDORDBEK-UNIK  PIC 9(2)    VALUE ZERO.                  
024500                                                                          
024600     03  W-WDQ101-KEY-MIN.                                                
024700         05  W-Q1-IDORDER-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
024800         05  W-Q1-IDARTNR-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
024900         05  W-Q1-IDLOPNR-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
025000         05  W-Q1-IDSEKVNR-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
025100         05  W-Q1-IDDC-MIN       PIC  X(2)   VALUE SPACE.                 
025200         05  W-Q1-KDORDBEK-MIN   PIC 9(2)    VALUE ZERO.                  
025300                                                                          
025400     03  W-WDQ101-KEY-MAX.                                                
025500         05  W-Q1-IDORDER-MAX    PIC S9(7) VALUE 9999999 COMP-3.          
025600         05  W-Q1-IDARTNR-MAX    PIC S9(9) VALUE 999999999 COMP-3.        
025700         05  W-Q1-IDLOPNR-MAX    PIC S9(3) VALUE 999  COMP-3.             
025800         05  W-Q1-IDSEKVNR-MAX   PIC S9(3) VALUE 999  COMP-3.             
025900         05  W-Q1-IDDC-MAX       PIC  X(2) VALUE '11'.                    
026000         05  W-Q1-KDORDBEK-MAX   PIC 9(2)  VALUE 99.                      
026100                                                                          
026200     03  W-WDQ101KY-MIN-X2.                                               
026300         05  W-IDORDER-Q1-MIN2   PIC S9(7)   VALUE ZERO COMP-3.           
026400         05  W-IDARTNR-Q1-MIN2   PIC S9(9)   VALUE ZERO COMP-3.           
026500         05  W-IDLOPNR-Q1-MIN2   PIC S9(3)   VALUE ZERO COMP-3.           
026600         05  FILLER              PIC X(06)   VALUE LOW-VALUE.             
026700                                                                          
026800     03  W-WDQ101KY-MAX-X2.                                               
026900         05  W-IDORDER-Q1-MAX2   PIC S9(7)   VALUE ZERO COMP-3.           
027000         05  W-IDARTNR-Q1-MAX2   PIC S9(9)   VALUE ZERO COMP-3.           
027100         05  W-IDLOPNR-Q1-MAX2   PIC S9(3)   VALUE ZERO COMP-3.           
027200         05  FILLER              PIC X(06)   VALUE HIGH-VALUE.            
027300                                                                          
027400     03  W-WDJ1CSEQ-X.                                                    
027500         05  W-J1-IDLEVNR        PIC X(5)    VALUE SPACE.                 
027600         05  FILLER              PIC X(30)   VALUE SPACE.                 
027700         05  W-J1-IDARTNR        PIC S9(9)   VALUE +0  COMP-3.            
027800                                                                          
027900     03  W-IDLEVNR-X             PIC X(5)    VALUE '1002 '.               
028000                                                                          
028100     03  W-WDE801KY-X.                                                    
028200         05  W-IDDISTR-KUND-X.                                            
028300           07  W-IDDISTR-X.                                               
028400             09  W-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.           
028500           07  W-IDKUNDNR-X.                                              
028600             09  W-IDKUNDNR      PIC S9(7)   VALUE ZERO COMP-3.           
028700         05  W-IDKUNDRF-X.                                                
028800             07  W-IDKUNDRF.                                              
028900                09  W-IDORDNR    PIC 9(7)    VALUE ZERO.                  
029000                09  FILLER       PIC X(3)    VALUE SPACE.                 
029100                                                                          
029200     03  W-IDARTNR-X.                                                     
029300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
029400                                                                          
029500     03  W-IDSKYLT-X.                                                     
029600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
029700                                                                          
029800     03  W-WDGX2225-X.                                                    
029900         05  W-IDHTYP-2225       PIC X(4)     VALUE '2225'.               
030000         05  W-VALFRI-2225       PIC X(26)    VALUE LOW-VALUE.            
030100                                                                          
030200     03  W-KDSEGKEY-X.                                                    
030300         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
030400                                                                          
030500     03  W-WDGX2223-X.                                                    
030600         05  W-IDHTYP-2223       PIC X(4)     VALUE '2223'.               
030700         05  W-IDANSK-2223       PIC S9(3)    VALUE ZERO COMP-3.          
030800         05  W-VALFRI-2225       PIC X(24)    VALUE LOW-VALUE.            
030900                                                                          
031000     03  W-WDGX2224-X.                                                    
031100         05  W-TISENBEK-DAG-2224 PIC S9(7)    VALUE ZERO COMP-3.          
031200         05  W-TISENBEK-KL-2224  PIC S9(7)    VALUE ZERO COMP-3.          
031300         05  W-KDLARM-2224       PIC S9(3)    VALUE ZERO COMP-3.          
031400                                                                          
031500     03  W-WDGX2231-X.                                                    
031600         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
031700         05  W-VALFRI-2231       PIC X(26)    VALUE LOW-VALUE.            
031800                                                                          
031900     03  W-WDGX2232-X.                                                    
032000         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
032100         05  W-LOW-VALUE-2232    PIC X(3)     VALUE LOW-VALUE.            
032200                                                                          
032300     03  W-WDGXKEY-4541-X.                                                
032400         05  W-IDHTYP            PIC X(04)   VALUE '4541'.                
032500         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
032600                                                                          
032700     03  W-WDGXKEY-X.                                                     
032800         05  W-WDGXKEY           PIC X(30)   VALUE SPACE.                 
032900                                                                          
033000     EJECT                                                                
033100*    --- STATUS-KOD FRÅN IMS                                              
033200 01  STATUS-WS                   PIC XX.                                  
033300     88  SEGMENT-FINNS                       VALUE '  '.                  
033400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
033500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
033600     88  BASEN-SLUT                          VALUE 'GB'.                  
033700     SKIP2                                                                
033800 01  GODK-STATUSKODER.                                                    
033900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034000     SKIP3                                                                
034100 01  SSA1                        PIC X(160).                              
034200 01  SSA2                        PIC X(96).                               
034300 01  SSA3                        PIC X(64).                               
034400                                                                          
034500     EJECT                                                                
034600*    --- IMS FUNKTIONSKODER                                               
034700*01  -COPY W0003                                                          
034800     EJECT                                                                
034900*    ---  DLI INPUT-OUTPUT AREA                                           
035000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
035100     SKIP3                                                                
035200 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
035300 01  DLI-IO-AREA-ORQM.                                                    
035400     03  WLORQM01.                                                        
035500*        05  -COPY WDQ101                                                 
035600     EJECT                                                                
035700 01  FILLER                      PIC X(16)   VALUE 'WDE801-AREA'.         
035800 01  DLI-IO-AREA-PROC.                                                    
035900     03  WLPROC01.                                                        
036000*        05  -COPY WDE801                                                 
036100     EJECT                                                                
036200 01  FILLER                      PIC X(16)   VALUE 'WDE901-AREA'.         
036300 01  DLI-IO-AREA-PROD.                                                    
036400     03  WLPROD01.                                                        
036500*        05  -COPY WDE901                                                 
036600     EJECT                                                                
036700 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
036800 01  DLI-IO-AREA-ARTM.                                                    
036900     03  WLARTM01.                                                        
037000*        05  -COPY WDK901                                                 
037100     EJECT                                                                
037200 01  FILLER                      PIC X(16)   VALUE 'WDD311-AREA'.         
037300 01  DLI-IO-AREA-BENA.                                                    
037400     03  WLBENA11.                                                        
037500*        05  -COPY WDD311                                                 
037600     EJECT                                                                
037700 01  FILLER                      PIC X(16)   VALUE 'WDJ1  -AREA'.         
037800 01  DLI-IO-AREA-SATB.                                                    
037900     03  WLSATB11.                                                        
038000*        05  -COPY WDJ111                                                 
038100     03  WLSATB01.                                                        
038200*        05  -COPY WDJ101                                                 
038300     EJECT                                                                
038400 01  FILLER                      PIC X(16)   VALUE 'WDR501-AREA'.         
038500 01  DLI-IO-AREA-2223.                                                    
038600     03  WLXXBU01.                                                        
038700*        05  -COPY WDGX2223   -PRE XXBU-                                  
038800     EJECT                                                                
038900 01  FILLER                      PIC X(16)   VALUE 'WDR550-AREA'.         
039000 01  DLI-IO-AREA-2224.                                                    
039100     03  WLXXBU11.                                                        
039200*        05  -COPY WDGX2224   -PRE XXBU-                                  
039300     EJECT                                                                
039400 01  FILLER                      PIC X(16)   VALUE 'WDR210-AREA'.         
039500 01  DLI-IO-AREA-2226.                                                    
039600     03  WLXXBV11.                                                        
039700*        05  -COPY WDGX2226   -PRE XXBV-                                  
039800     EJECT                                                                
039900 01  FILLER                      PIC X(16)   VALUE 'WDR220-AREA'.         
040000 01  DLI-IO-AREA-2232.                                                    
040100     03  WLXXBX11.                                                        
040200*        05  -COPY WDGX2232   -PRE XXBX-                                  
040300     EJECT                                                                
040400 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW'.                
040500 01  4263-MSG-IO-AREA.                                                    
040600     03  4263-LL               PIC S9(4)  VALUE +85  COMP SYNC.           
040700     03  4263-Z1               PIC X      VALUE LOW-VALUE.                
040800     03  4263-Z2               PIC X      VALUE LOW-VALUE.                
040900     03  4263-TRANSKOD         PIC X(8)   VALUE 'W4T263V '.               
041000     03  4263-IDTRANS          PIC X(4)   VALUE '4263'.                   
041100     03  4263-SPRAK            PIC X.                                     
041200     03  4263-IDDISTR-IN       PIC X(4).                                  
041300     03  4263-IDKUNDNR-IN      PIC X(6).                                  
041400     03  4263-IDORDNR-IN       PIC X(7).                                  
041500     03  4263-IDARTNR-IN       PIC X(9).                                  
041600     03  4263-IDDISTR-UT       PIC X(4).                                  
041700     03  4263-IDKUNDNR-UT      PIC X(6).                                  
041800     03  4263-IDORDNR-UT       PIC X(7).                                  
041900     03  4263-KDORDKL-UT       PIC X      VALUE SPACE.                    
042000     03  FILLER                PIC X(24)  VALUE ZERO.                     
042100                                                                          
042200     EJECT                                                                
042300 01  FILLER                  PIC X(16)  VALUE '4262-MSG-IO-AREA'.         
042400 01  4262-MSG-IO-AREA.                                                    
042500     03  4262-LL               PIC S9(4)  VALUE +821 COMP SYNC.           
042600     03  4262-Z1               PIC X.                                     
042700     03  4262-Z2               PIC X.                                     
042800     03  4262-TRANSKOD         PIC X(8)   VALUE 'W4T262U '.               
042900     03  4262-IDTRANS          PIC X(4)   VALUE '4263'.                   
043000     03  4262-SPRAK            PIC X.                                     
043100     03  -COPY W4I26201  -PRE 4262-                                       
043200     EJECT                                                                
043300 LINKAGE SECTION.                                                         
043400                                                                          
043500*01  -COPY W0009      -PRE MSG-                                           
043600     EJECT                                                                
043700*01  -COPY W0009      -PRE DISP-                                          
043800     EJECT                                                                
043900*01  -COPY W0009      -PRE 4262-                                          
044000     EJECT                                                                
044100*01  -COPY W0009      -PRE 4263-                                          
044200     EJECT                                                                
044300*01  -COPY W0008      -PRE SATB-                                          
044400     05  FILLER                  PIC X.                                   
044500     EJECT                                                                
044600*01  -COPY W0008      -PRE ARTM-                                          
044700     05  FILLER                  PIC X.                                   
044800     EJECT                                                                
044900*01  -COPY W0008      -PRE BENA-                                          
045000     05  FILLER                  PIC X.                                   
045100     EJECT                                                                
045200*01  -COPY W0008      -PRE PROC-                                          
045300     05  FILLER                  PIC X.                                   
045400     EJECT                                                                
045500*01  -COPY W0008      -PRE PROD-                                          
045600     05  FILLER                  PIC X.                                   
045700     EJECT                                                                
045800*01  -COPY W0008      -PRE ORQM-                                          
045900     05  FILLER                  PIC X.                                   
046000     EJECT                                                                
046100*01  -COPY W0008      -PRE XXBU-                                          
046200     05  FILLER                  PIC X.                                   
046300     EJECT                                                                
046400*01  -COPY W0008      -PRE XXBV-                                          
046500     05  FILLER                  PIC X.                                   
046600     EJECT                                                                
046700*01  -COPY W0008      -PRE XXBX-                                          
046800     05  FILLER                  PIC X.                                   
046900     EJECT                                                                
047000                                                                          
047100 01  AREG-WDK6-PCB               PIC X.                                   
047200 01  AREG-WDK7-PCB               PIC X.                                   
047300                                                                          
047400 01  TIME-4437-PCB               PIC X.                                   
047500                                                                          
047600     EJECT                                                                
047700                                                                          
047800 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB 4262-PCB 4263-PCB             
047900        SATB-PCB ARTM-PCB BENA-PCB PROC-PCB PROD-PCB ORQM-PCB             
048000        XXBU-PCB XXBV-PCB XXBX-PCB                                        
048100        AREG-WDK6-PCB                                                     
048200        AREG-WDK7-PCB                                                     
048300        TIME-4437-PCB.                                                    
048400                                                                          
048500     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB 4262-PCB 4263-PCB             
048600        SATB-PCB ARTM-PCB BENA-PCB PROC-PCB PROD-PCB ORQM-PCB             
048700        XXBU-PCB XXBV-PCB XXBX-PCB                                        
048800        AREG-WDK6-PCB                                                     
048900        AREG-WDK7-PCB                                                     
049000        TIME-4437-PCB.                                                    
049100                                                                          
049200     EJECT                                                                
049300     PERFORM IMS-GET-MSG                                                  
049400     IF SEGMENT-FINNS                                                     
049500       PERFORM IMS-GN-MSG                                                 
049600       IF SEGMENT-FINNS                                                   
049700          MOVE JA TO STARTAD-AV-DISPATCHEN-SW                             
049800       END-IF                                                             
049900       PERFORM A-INIT                                                     
050000       PERFORM B-KOLLA-NYCKLAR                                            
050100       IF NYCKEL-OK                                                       
050200         IF MFS-NEXT                                                      
050300           PERFORM C-NAESTA-SIDA                                          
050400         ELSE                                                             
050500           PERFORM D-FOERSTA-SIDA                                         
050600         END-IF                                                           
050700         IF ALLT-OK                                                       
050800           PERFORM F-KOLLA-ATT-ORDER-FINNS                                
050900           IF ALLT-OK                                                     
051000             IF ((MFS-KDTRTYP = 'U' OR 'V') OR MFS-ENTER)  AND            
051100                (NOT STARTAD-AV-DISPATCHEN)                               
051200               PERFORM G-KONTROLLERA-BILDEN                               
051300             END-IF                                                       
051400             IF ALLT-OK                                                   
051500               PERFORM H-BEHANDLA-RADER                                   
051600               PERFORM I-UPPDATERA-PHUV                                   
051700               IF AVSLUTA                                                 
051800                 PERFORM M-HOPPA-TILL-ORDERHUVUD-4261                     
051900               END-IF                                                     
052000             END-IF                                                       
052100           END-IF                                                         
052200         END-IF                                                           
052300       END-IF                                                             
052400       IF HOPP = NEJ                                                      
052500         PERFORM Z-FINIT                                                  
052600       END-IF                                                             
052700     END-IF                                                               
052800     MOVE +0 TO RETURN-CODE                                               
052900     GOBACK                                                               
053000     .                                                                    
053100     EJECT                                                                
053200 A-INIT SECTION.                                                          
053300                                                                          
053400     MOVE SPACE                TO MED-IDMFSFEL                            
053500                                  MED-IDMFSINF                            
053600     MOVE NEJ                  TO OMSTART-SW                              
053700     MOVE JA                   TO ALLT-SW                                 
053800                                  NYCKEL-SW                               
053900     ACCEPT DAGENS-DATUM     FROM DATE                                    
054000     ACCEPT DAGENS-TID       FROM TIME                                    
054100                                                                          
054200     IF MSG-DUBBLA-TRANSKODER                                             
054300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I26301                 
054400       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
054500       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
054600     ELSE                                                                 
054700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I26301                  
054800       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
054900       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
055000     END-IF                                                               
055100     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
055200     MOVE MSG-IDPFK            TO MFS-IDPFK                               
055300     MOVE MFS-IDTRANS          TO W-IDTRANS                               
055400     MOVE LOW-VALUE            TO MSG-AREA                                
055500     MOVE 'W4O26301'           TO MFS-IDMOD                               
055600     MOVE '4263'               TO MOD-IDTRANS                             
055700     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
055800                                                                          
055900     IF NOT EGEN-MID                                                      
056000       IF W-IDTRANS = '4262' AND MFS-UPD-V                                
056100         CONTINUE                                                         
056200       ELSE                                                               
056300         MOVE SPACE              TO MFS-KDTRTYP                           
056400         MOVE '7'                TO MFS-IDPFK                             
056500       END-IF                                                             
056600     END-IF                                                               
056700     EJECT                                                                
056800     IF ENGLISH-TEXT                                                      
056900       MOVE +2                 TO SPRAK-IX                                
057000       MOVE 'GB '              TO MED-IDSKYLT                             
057100     ELSE                                                                 
057200       MOVE +1                 TO SPRAK-IX                                
057300       MOVE 'S  '              TO MED-IDSKYLT                             
057400     END-IF                                                               
057500                                                                          
057600     .                                                                    
057700     EJECT                                                                
057800 B-KOLLA-NYCKLAR SECTION.                                                 
057900                                                                          
058000     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
058100                                  MOD-IDKUNDNR-IN                         
058200                                  MOD-IDORDNR-IN                          
058300                                  MOD-IDARTNR-IN                          
058400     IF MID-IDDISTR-IN = ALL '+'                                          
058500       MOVE MID-IDDISTR-UT    TO WS-IDDISTR                               
058600       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
058700     ELSE                                                                 
058800       MOVE MID-IDDISTR-IN    TO WS-IDDISTR                               
058900       MOVE '7'               TO MFS-IDPFK                                
059000       MOVE SPACE             TO MFS-KDTRTYP                              
059100     END-IF                                                               
059200                                                                          
059300     IF WS-IDDISTR NUMERIC  AND  WS-IDDISTR > ZERO                        
059400       MOVE WS-IDDISTR        TO W-IDDISTR                                
059500     ELSE                                                                 
059600       MOVE NEJ               TO NYCKEL-SW                                
059610       MOVE ZERO              TO W-IDDISTR                                
059700     END-IF                                                               
059800                                                                          
059900     MOVE W-IDDISTR          TO DIST79-IDDISTR                            
060000     IF DIST79-DEALER-PRICE                                               
060100       IF ENGLISH-TEXT                                                    
060200         MOVE 'DEALERPRICE'   TO MOD-TEDDI                                
060300       ELSE                                                               
060400         MOVE '    ÅF PRIS'   TO MOD-TEDDI                                
060500       END-IF                                                             
060600     ELSE                                                                 
061400        MOVE SPACE             TO MOD-TEDDI                               
061600     END-IF                                                               
061700                                                                          
061800     IF MID-IDKUNDNR-IN = ALL '+'                                         
061900       MOVE MID-IDKUNDNR-UT   TO WS-IDKUNDNR                              
062000       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
062100     ELSE                                                                 
062200       MOVE MID-IDKUNDNR-IN   TO WS-IDKUNDNR                              
062300       MOVE '7'               TO MFS-IDPFK                                
062400       MOVE SPACE             TO MFS-KDTRTYP                              
062500     END-IF                                                               
062600                                                                          
062700     IF WS-IDKUNDNR NUMERIC                                               
062800       MOVE WS-IDKUNDNR       TO W-IDKUNDNR                               
062900     ELSE                                                                 
063000       MOVE NEJ               TO NYCKEL-SW                                
063100     END-IF                                                               
063200     IF MID-IDORDNR-IN = ALL '+'                                          
063300       MOVE MID-IDORDNR-UT    TO WS-IDORDNR                               
063400       INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                 
063500     ELSE                                                                 
063600       MOVE MID-IDORDNR-IN    TO WS-IDORDNR                               
063700       MOVE '7'               TO MFS-IDPFK                                
063800       MOVE SPACE             TO MFS-KDTRTYP                              
063900     END-IF                                                               
064000     IF WS-IDORDNR NUMERIC  AND WS-IDORDNR > ZERO                         
064100       MOVE WS-IDORDNR        TO W-IDORDNR                                
064200     ELSE                                                                 
064300       MOVE NEJ               TO NYCKEL-SW                                
064400     END-IF                                                               
064500                                                                          
064600     IF GODK-MID OR NYCKEL-OK                                             
064700       MOVE WS-IDKUNDNR          TO MOD-IDKUNDNR-UT                       
064800       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
064900       IF WS-IDKUNDNR = ZERO                                              
065000         MOVE '     0'          TO MOD-IDKUNDNR-UT                        
065100       END-IF                                                             
065200       MOVE WS-IDDISTR           TO MOD-IDDISTR-UT                        
065300       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
065400       MOVE WS-IDORDNR           TO MOD-IDORDNR-UT                        
065500       INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE             
065600     ELSE                                                                 
065700       MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-UT                           
065800                                 MOD-IDKUNDNR-UT                          
065900                                 MOD-IDORDNR-UT                           
066000     END-IF                                                               
066100                                                                          
066200     IF NOT NYCKEL-OK                                                     
066300       MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                             
066400       PERFORM MFS-RENSA-ALLA-FAELT                                       
066500     END-IF                                                               
066600     .                                                                    
066700     EJECT                                                                
066800 C-NAESTA-SIDA SECTION.                                                   
066900                                                                          
067000     IF MID-IDARTNR-NEXT NUMERIC AND                                      
067010        MID-IDARTNR-NEXT > ZERO                                           
067100       MOVE MID-IDARTNR-NEXT  TO W-Q1-IDARTNR-MIN                         
067200       MOVE MID-IDLOPNR-NEXT  TO W-Q1-IDLOPNR-MIN                         
067300       MOVE MID-IDSEKVNR-NEXT TO W-Q1-IDSEKVNR-MIN                        
067400       MOVE WC-CDC-SE         TO W-Q1-IDDC-MIN                            
067500       MOVE MID-KDORDBEK-NEXT TO W-Q1-KDORDBEK-MIN                        
067600     ELSE                                                                 
067700       MOVE MED-EJ-FLER-RADER TO MED-IDMFSFEL                             
067800       MOVE NEJ               TO ALLT-SW                                  
067900     END-IF                                                               
068000     .                                                                    
068100     EJECT                                                                
068200 D-FOERSTA-SIDA SECTION.                                                  
068300                                                                          
068400     MOVE ZERO                 TO W-Q1-IDARTNR-MIN                        
068500                                  W-Q1-IDLOPNR-MIN                        
068600                                  W-Q1-IDSEKVNR-MIN                       
068700                                  W-Q1-KDORDBEK-MIN                       
068800     MOVE WC-CDC-SE            TO W-Q1-IDDC-MIN                           
068900                                                                          
068910     IF MID-IDARTNR-NEXT NUMERIC AND                                      
068920        MID-IDARTNR-NEXT > ZERO                                           
069100       MOVE MID-IDARTNR-NEXT  TO MOD-IDARTNR-NEXT                         
069200       MOVE MID-IDLOPNR-NEXT  TO MOD-IDLOPNR-NEXT                         
069300       MOVE MID-IDSEKVNR-NEXT TO MOD-IDSEKVNR-NEXT                        
069400       MOVE MID-KDORDBEK-NEXT TO MOD-KDORDBEK-NEXT                        
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 F-KOLLA-ATT-ORDER-FINNS SECTION.                                         
069900                                                                          
070000     PERFORM IMS-GHU-PROC-WDE801                                          
070100     IF SEGMENT-FINNS                                                     
070200       IF PHUV-FLBORT = JA                                                
070300         MOVE ERR-ORDER-ANNULLERAD TO MED-IDMFSFEL                        
070400         MOVE NEJ            TO ALLT-SW                                   
070500         MOVE NEJ            TO NYCKEL-SW                                 
070600         PERFORM MFS-RENSA-ALLA-FAELT                                     
070700       ELSE                                                               
070800         PERFORM FA-KOLLA-DATUM                                           
070900         IF ALLT-OK                                                       
071000           IF PHUV-IDUSER NOT = MSG-SIGNON-USERID AND                     
071100             PHUV-KDPROTYP = 'L'                                          
071200             MOVE ERR-OBEHORIG TO MED-IDMFSFEL                            
071300             MOVE NEJ          TO ALLT-SW                                 
071400             MOVE NEJ          TO NYCKEL-SW                               
071500           ELSE                                                           
071600             MOVE PHUV-KDORDKL  TO MOD-KDORDKL-UT                         
071700             MOVE PHUV-KDFRAKT  TO MOD-KDFRAKT-UT                         
071800             MOVE PHUV-KDPROTYP TO MOD-KDPROTYP-UT                        
071900             MOVE PHUV-IDORDER TO W-Q1-IDORDER-UNIK                       
072000                                  W-Q1-IDORDER-MIN                        
072100                                  W-Q1-IDORDER-MAX                        
072200             PERFORM FB-KOLLA-ATT-ORDERBEK-FINNS                          
072300           END-IF                                                         
072400         END-IF                                                           
072500       END-IF                                                             
072600     ELSE                                                                 
072700       MOVE ERR-ORDER-SAKNAS  TO MED-IDMFSFEL                             
072800       MOVE NEJ               TO ALLT-SW                                  
072900       MOVE NEJ               TO NYCKEL-SW                                
073000       PERFORM MFS-RENSA-ALLA-FAELT                                       
073100     END-IF                                                               
073200     .                                                                    
073300     EJECT                                                                
073400 FA-KOLLA-DATUM SECTION.                                                  
073500                                                                          
073600     MOVE PHUV-TIFORDAT TO WS-TIFORDAT                                    
073700     MOVE WS-TIFORDAT  TO TMP1-YYMMDD                                     
073800     MOVE DAGENS-DATUM TO TMP2-YYMMDD                                     
073900     PERFORM WY2000P1                                                     
074000     IF TMP1-YYMMDD <= TMP2-YYMMDD OR PHUV-TIORDDAT NOT = ZERO            
074100       MOVE ERR-ORDER-AVSLUTAD TO MED-IDMFSFEL                            
074200       MOVE NEJ TO ALLT-SW                                                
074300     END-IF                                                               
074400                                                                          
074500     .                                                                    
074600     EJECT                                                                
074700 FB-KOLLA-ATT-ORDERBEK-FINNS SECTION.                                     
074800                                                                          
074900     PERFORM IMS-GHU-ORQM-WDQ101-MIN-MAX                                  
075000     IF SEGMENT-SAKNAS AND W-IDTRANS NOT = '4262'                         
075100       MOVE ERR-ORDER-AVSLUTAD TO MED-IDMFSFEL                            
075200       MOVE NEJ                TO ALLT-SW                                 
075300       MOVE NEJ                TO NYCKEL-SW                               
075400       PERFORM MFS-RENSA-ALLA-FAELT                                       
075500     END-IF                                                               
075600                                                                          
075700     .                                                                    
075800     EJECT                                                                
075900 G-KONTROLLERA-BILDEN SECTION.                                            
076000                                                                          
076100     MOVE JA                   TO ALLT-SW                                 
076200                                                                          
076300     PERFORM GA-KONTROLLERA-KDBEHX                                        
076400     IF ALLT-OK                                                           
076500       PERFORM GB-KONTROLLERA-SAMBAND                                     
076600       IF ALLT-OK                                                         
076700         PERFORM GC-JUSTERA-KDBEHX                                        
076800       END-IF                                                             
076900     END-IF                                                               
077000     .                                                                    
077100     EJECT                                                                
077200 GA-KONTROLLERA-KDBEHX SECTION.                                           
077300                                                                          
077400     MOVE +1                   TO WS-INDEX-MID                            
077500     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
077600                      MID-KDORDBEK(WS-INDEX-MID) = ZERO                   
077700       IF MID-KDBEHX(WS-INDEX-MID) = ALL '+'                              
077800          MOVE SPACE           TO MID-KDBEHX(WS-INDEX-MID)                
077900       END-IF                                                             
078000       MOVE MID-RAD(WS-INDEX-MID) TO WS-AKTUELL-MID-RAD                   
078100       IF WS-AKT-KDBEHX = 'B' OR 'D' OR 'A'                               
078200                              OR '1' OR '2' OR ' '                        
078300         EVALUATE WS-AKT-KDBEHX                                           
078400           WHEN 'A'                                                       
078500             IF WS-AKT-KDORDBEK = 61                                      
078600               CONTINUE                                                   
078700             ELSE                                                         
078800               MOVE MFS-ALFA-FAELT-FEL                                    
078900                              TO MOD-KDBEHX-ATTR(WS-INDEX-MID)            
079000               MOVE NEJ TO ALLT-SW                                        
079100             END-IF                                                       
079200           WHEN 'B'                                                       
079300             IF WS-AKT-KDORDBEK = 21 OR 51  OR 52  OR 53  OR              
079400                54  OR 55  OR 57  OR 58  OR 59  OR 66  OR                 
079500                67  OR 72  OR 73  OR 74  OR 75  OR                        
079600                76  OR 80  OR 81  OR 82  OR 85  OR  98                    
079700                              OR                                          
079800                ((WS-AKT-KDORDBEK = 41 OR 61)                             
079900                      AND (WS-AKT-IDARTNR = WS-AKT-IDARTNR-URS))          
080000               CONTINUE                                                   
080100             ELSE                                                         
080200               MOVE MFS-ALFA-FAELT-FEL                                    
080300                              TO MOD-KDBEHX-ATTR(WS-INDEX-MID)            
080400               MOVE NEJ TO ALLT-SW                                        
080500             END-IF                                                       
080600           WHEN 'D'                                                       
080700             IF WS-AKT-KDORDBEK = 15  OR 16  OR 41 OR 43                  
080800                     OR 44  OR 70  OR 95                                  
080900               CONTINUE                                                   
081000             ELSE                                                         
081100               MOVE MFS-ALFA-FAELT-FEL                                    
081200                           TO MOD-KDBEHX-ATTR(WS-INDEX-MID)               
081300               MOVE NEJ TO ALLT-SW                                        
081400             END-IF                                                       
081500           WHEN '1'                                                       
081600             IF WS-AKT-KDORDBEK = 43  OR 44                               
081700               CONTINUE                                                   
081800             ELSE                                                         
081900               MOVE MFS-ALFA-FAELT-FEL                                    
082000                             TO MOD-KDBEHX-ATTR(WS-INDEX-MID)             
082100               MOVE NEJ   TO ALLT-SW                                      
082200             END-IF                                                       
082300           WHEN '2'                                                       
082400             IF WS-AKT-KDORDBEK = 43  OR 44                               
082500               CONTINUE                                                   
082600             ELSE                                                         
082700               MOVE MFS-ALFA-FAELT-FEL                                    
082800                             TO MOD-KDBEHX-ATTR(WS-INDEX-MID)             
082900               MOVE NEJ   TO ALLT-SW                                      
083000             END-IF                                                       
083100           WHEN OTHER                                                     
083200             IF WS-AKT-KDORDBEK = 10  OR 15  OR 16  OR                    
083300                26  OR                                                    
083400                43  OR 44  OR 70  OR 74  OR 95  OR                        
083500                99  OR 41  OR 61  OR 92                                   
083600                CONTINUE                                                  
083700             ELSE                                                         
083800                MOVE MFS-ALFA-FAELT-FEL                                   
083900                            TO MOD-KDBEHX-ATTR(WS-INDEX-MID)              
084000                MOVE NEJ   TO ALLT-SW                                     
084100             END-IF                                                       
084200         END-EVALUATE                                                     
084300       ELSE                                                               
084400         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBEHX-ATTR(WS-INDEX-MID)         
084500         MOVE NEJ            TO ALLT-SW                                   
084600       END-IF                                                             
084700       ADD +1                 TO WS-INDEX-MID                             
084800     END-PERFORM                                                          
084900     IF NOT ALLT-OK                                                       
085000       MOVE ERR-UPPLYSTA-FEL  TO MED-IDMFSFEL                             
085100     END-IF                                                               
085200     .                                                                    
085300     EJECT                                                                
085400 GB-KONTROLLERA-SAMBAND SECTION.                                          
085500                                                                          
085600     MOVE +1                   TO WS-INDEX-MID                            
085700     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
085800                   MID-KDBEHX(WS-INDEX-MID) = '+'                         
085900       MOVE MID-RAD(WS-INDEX-MID) TO WS-AKTUELL-MID-RAD                   
086000       IF WS-AKT-KDBEHX = 'D'                                             
086100         PERFORM S13-HITTA-FORSTA-I-GRUPPEN                               
086200         ADD +1              TO WS-INDEX-MID                              
086300         IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                           
086400           MOVE MID-RAD(WS-INDEX-MID) TO WS-AKTUELL-MID-RAD               
086500         END-IF                                                           
086600         PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR                 
086700                  WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR                 
086800                  WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR                 
086900                  WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR            
087000           IF WS-AKT-KDBEHX = '1' OR '2'                                  
087100             MOVE NEJ      TO ALLT-SW                                     
087200             MOVE MFS-ALFA-FAELT-FEL                                      
087300                               TO MOD-KDBEHX-ATTR(WS-INDEX-MID)           
087400             MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                        
087500           END-IF                                                         
087600           ADD +1           TO WS-INDEX-MID                               
087700           IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                         
087800             MOVE MID-RAD(WS-INDEX-MID) TO WS-AKTUELL-MID-RAD             
087900           END-IF                                                         
088000         END-PERFORM                                                      
088100       ELSE                                                               
088200         ADD +1              TO WS-INDEX-MID                              
088300       END-IF                                                             
088400     END-PERFORM                                                          
088500     .                                                                    
088600     EJECT                                                                
088700 GC-JUSTERA-KDBEHX SECTION.                                               
088800                                                                          
088900*    JUSTERINGEN GÖRS FÖR ATT VARJE RAD SENARE I PROGRAMMET               
089000*    SKALL KUNNA BEHANDLAS VAR FÖR SIG.                                   
089100*    BORTTAG AV ORDERBEKRÄFTELSER SKULLE ANNARS BEHÖVA GÖRAS              
089200*    I MÅNGA SEKTIONER. PÅ DETTA SÄTT KOMMER ALLA BORTTAG                 
089300*    ATT GÖRAS I HEA-ANNULLERA-RAD.                                       
089400                                                                          
089500     MOVE +1                   TO WS-INDEX-MID                            
089600     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX   OR                   
089700                   MID-KDBEHX(WS-INDEX-MID) = '+'                         
089800       MOVE MID-RAD(WS-INDEX-MID) TO WS-AKTUELL-MID-RAD                   
089900       IF WS-AKT-KDBEHX = '1' OR '2'                                      
090000         PERFORM S13-HITTA-FORSTA-I-GRUPPEN                               
090100         PERFORM GCA-D-MARKERA-1-2-RADER                                  
090200       ELSE                                                               
090300         IF WS-AKT-KDBEHX = 'D'                                           
090400           PERFORM S13-HITTA-FORSTA-I-GRUPPEN                             
090500           PERFORM GCB-D-MARKERA-RADER                                    
090600         ELSE                                                             
090700           ADD +1        TO WS-INDEX-MID                                  
090800         END-IF                                                           
090900       END-IF                                                             
091000     END-PERFORM                                                          
091100     .                                                                    
091200     EJECT                                                                
091300 GCA-D-MARKERA-1-2-RADER SECTION.                                         
091400                                                                          
091500     ADD +1                    TO WS-INDEX-MID                            
091600     IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                               
091700       MOVE MID-RAD(WS-INDEX-MID)                                         
091800                               TO WS-AKTUELL-MID-RAD                      
091900     END-IF                                                               
092000     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR                     
092100              WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR                     
092200              WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR                     
092300          WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR                    
092400                                                                          
092500       IF WS-AKT-KDBEHX = ' '                                             
092600         IF WS-AKT-KDORDBEK = 41                                          
092700           CONTINUE                                                       
092800         ELSE                                                             
092900           MOVE 'D'         TO MID-KDBEHX(WS-INDEX-MID)                   
093000         END-IF                                                           
093100       END-IF                                                             
093200       ADD +1                 TO WS-INDEX-MID                             
093300       IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                             
093400         MOVE MID-RAD(WS-INDEX-MID)                                       
093500                               TO WS-AKTUELL-MID-RAD                      
093600       END-IF                                                             
093700     END-PERFORM                                                          
093800     .                                                                    
093900     EJECT                                                                
094000 GCB-D-MARKERA-RADER SECTION.                                             
094100                                                                          
094200     ADD +1                    TO WS-INDEX-MID                            
094300     IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                               
094400       MOVE MID-RAD(WS-INDEX-MID)                                         
094500                               TO WS-AKTUELL-MID-RAD                      
094600     END-IF                                                               
094700     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX OR                     
094800              WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR                     
094900              WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR                     
095000          WS-AKT-IDARTNR-URS NOT = WS-IDARTNR-URS-SPAR                    
095100                                                                          
095200       IF WS-AKT-KDBEHX = ' '                                             
095300         MOVE 'D'            TO MID-KDBEHX(WS-INDEX-MID)                  
095400       END-IF                                                             
095500       ADD +1                 TO WS-INDEX-MID                             
095600       IF WS-INDEX-MID NOT > WS-INDEX-MID-MAX                             
095700         MOVE MID-RAD(WS-INDEX-MID)                                       
095800                               TO WS-AKTUELL-MID-RAD                      
095900       END-IF                                                             
096000     END-PERFORM                                                          
096100     .                                                                    
096200     EJECT                                                                
096300 H-BEHANDLA-RADER SECTION.                                                
096400                                                                          
096500     IF MFS-FIRST  OR  MFS-NEXT                                           
096600       PERFORM HB-LAS-IN-13-RADER                                         
096700        IF  WS-INDEX-MOD = +1  AND (W-IDTRANS = '4262')                   
096800           MOVE JA             TO AVSLUTA-SW                              
096900        END-IF                                                            
097000     ELSE                                                                 
097100       IF MFS-UPDATE  OR  MFS-QUERY                                       
097200         PERFORM HD-UPPDATERA-OBEH-RADER                                  
097300         IF NOT STARTAD-AV-DISPATCHEN                                     
097400           PERFORM HE-UPPDATERA-AKT-SIDA                                  
097500           IF START-4262                                                  
097600             MOVE 'U'         TO 4262-MID-KDTRTYP                         
097700             PERFORM S03-STARTA-RADBEHANDLINGEN                           
097800           ELSE                                                           
097900             PERFORM HB-LAS-IN-13-RADER                                   
098000             IF  WS-INDEX-MOD = +1                                        
098100                MOVE JA      TO AVSLUTA-SW                                
098200             END-IF                                                       
098300           END-IF                                                         
098400         END-IF                                                           
098500       ELSE                                                               
098600         IF MFS-UPD-V                                                     
098700            PERFORM HE-UPPDATERA-AKT-SIDA                                 
098800            PERFORM HG-UPPDATERA-RESTERANDE-RADER                         
098900         END-IF                                                           
099000       END-IF                                                             
099100     END-IF                                                               
099200     .                                                                    
099300     EJECT                                                                
099400 HB-LAS-IN-13-RADER SECTION.                                              
099500                                                                          
099600     PERFORM MFS-RENSA-ALLA-FAELT                                         
099700     MOVE +0                   TO WS-IDARTNR-SPAR                         
099800                                  WS-IDLOPNR-SPAR                         
099900                                                                          
100000     PERFORM HBA-VISA-OBKR-OCH-SATS-RADER                                 
100100                                                                          
100200     PERFORM HBB-FIXA-BLADDRINGS-VARDEN                                   
100300                                                                          
100400     IF WS-INDEX-MOD > WS-INDEX-MOD-MAX AND                               
100500           OBKR-SEGMENT-FINNS   AND                                       
100600          (OBKR-KDORDBEK = 41 OR 61)                                      
100700                                                                          
100800       PERFORM HBC-KONTROLLERA-SIDSLUT                                    
100900     END-IF                                                               
101000                                                                          
101100     IF MED-IDMFSINF NOT = SPACE                                          
101200       CALL WMEDKONV USING MED-WMEDAREA                                   
101300       MOVE MED-MFSINF        TO MOD-TEMFSINF                             
101400     END-IF                                                               
101500     .                                                                    
101600     EJECT                                                                
101700 HBA-VISA-OBKR-OCH-SATS-RADER SECTION.                                    
101800                                                                          
101900     MOVE +1                   TO WS-INDEX-MOD                            
102000                                                                          
102100     PERFORM IMS-GHU-ORQM-WDQ101-MIN-MAX                                  
102200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
102300              OR   WS-INDEX-MOD > WS-INDEX-MOD-MAX                        
102400                                                                          
102500       PERFORM HBAA-REDIGERA-ORDERBEKR-RAD                                
102600                                                                          
102700       IF OBKR-KDORDBEK = 57                                              
102800         PERFORM HBAB-VISA-SATS-ARTIKLAR                                  
102900       END-IF                                                             
103000       MOVE OBKR-IDARTNR   TO WS-IDARTNR-SPAR                             
103100       MOVE OBKR-IDLOPNR   TO WS-IDLOPNR-SPAR                             
103200                                                                          
103300       ADD +1              TO WS-INDEX-MOD                                
103400                                                                          
103500       IF NOT NEXT-SATS                                                   
103600         PERFORM IMS-GHN-ORQM-WDQ101-MIN-MAX                              
103700*--------SPARA STATUSKODEN FRÅN ORDERBEKRÄFTELSE LÄSNINGEN                
103800         MOVE STATUS-WS      TO STATUS-OBKR-SW                            
103900       END-IF                                                             
104000     END-PERFORM                                                          
104100     .                                                                    
104200     EJECT                                                                
104300 HBAA-REDIGERA-ORDERBEKR-RAD SECTION.                                     
104400                                                                          
104500     MOVE OBKR-KDORDBEK        TO MOD-KDORDBEK(WS-INDEX-MOD)              
104600     IF OBKR-KDORDBEK = 41 OR 61                                          
104700        MOVE '*'               TO MOD-ASTERIX(WS-INDEX-MOD)               
104800     END-IF                                                               
104900                                                                          
105000     MOVE SPACE                TO MOD-KDBEHX(WS-INDEX-MOD)                
105100     IF OBKR-KDORDBEK = 21 OR 51                                          
105200                     OR 52 OR 53 OR 54 OR 55 OR 57 OR 58 OR 59            
105300                     OR 66 OR 67 OR 72 OR 73 OR 74 OR 75 OR 76            
105400                     OR 80 OR 81 OR 82 OR 85 OR 98                        
105500       MOVE 'B'              TO MOD-KDBEHX(WS-INDEX-MOD)                  
105600       MOVE MFS-STAENG-FAELT TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)             
105700     END-IF                                                               
105800     IF OBKR-KDORDBEK = 41 OR 61                                          
105900       IF OBKR-IDARTNR = WS-IDARTNR-SPAR AND                              
106000          OBKR-IDLOPNR = WS-IDLOPNR-SPAR                                  
106100         MOVE SPACE          TO MOD-KDBEHX(WS-INDEX-MOD)                  
106200         MOVE MFS-STAENG-FAELT-OSYNLIGT                                   
106300                               TO MOD-KDORDBEK-ATTR(WS-INDEX-MOD)         
106400       ELSE                                                               
106500         MOVE 'B'              TO MOD-KDBEHX(WS-INDEX-MOD)                
106600         MOVE MFS-STAENG-FAELT TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)           
106700       END-IF                                                             
106800     END-IF                                                               
106900     IF (OBKR-KDORDBEK = 61 )  AND                                        
107000                         OBKR-IDARTNR-TILLK > +0                          
107100       MOVE 'A'               TO MOD-KDBEHX(WS-INDEX-MOD)                 
107200     END-IF                                                               
107300     MOVE OBKR-IDARTNR         TO MOD-IDARTNR-1--9(WS-INDEX-MOD)          
107400     MOVE '-'                  TO MOD-STRAEK(WS-INDEX-MOD)                
107500     MOVE OBKR-REKSIFFR        TO MOD-REKSIFFR(WS-INDEX-MOD)              
107600                                                                          
107700     PERFORM S22-HAMTA-BENAMNING                                          
107800                                                                          
107900     IF OBKR-KDORDBEK = 10 OR 15 OR 16 OR 43 OR 44 OR 70 OR               
108000                        74 OR 95                                          
108100       MOVE OBKR-KVBEART-Q    TO MOD-KVBEART(WS-INDEX-MOD)                
108200     ELSE                                                                 
108300       IF OBKR-KDORDBEK = 80                                              
108400         MOVE OBKR-KVANNANT  TO MOD-KVBEART(WS-INDEX-MOD)                 
108500       ELSE                                                               
108600         IF OBKR-KDORDBEK = 92 OR 99                                      
108700           MOVE OBKR-KVPRERO TO MOD-KVBEART(WS-INDEX-MOD)                 
108800         ELSE                                                             
108900           MOVE OBKR-KVBEART TO MOD-KVBEART(WS-INDEX-MOD)                 
109000         END-IF                                                           
109100       END-IF                                                             
109200     END-IF                                                               
109300                                                                          
109400     IF OBKR-KDORDBEK = 43 OR 44                                          
109500       MOVE OBKR-KVQPACK      TO MOD-KVQPACK(WS-INDEX-MOD)                
109600     ELSE                                                                 
109700       MOVE +0                TO MOD-KVQPACK(WS-INDEX-MOD)                
109800     END-IF                                                               
109900                                                                          
110000     IF OBKR-KDORDBEK = 15 OR 16 OR 21                                    
110100                     OR 43 OR 44 OR 52 OR 53 OR 54                        
110200                     OR 55 OR 57 OR 70 OR 72 OR 73 OR 74                  
110300                     OR 75 OR 76 OR 80 OR 95 OR 99 OR 58 OR 92            
110400                     OR 66                                                
110500       IF OBKR-IDARTNR-TILLK > 0                                          
110600         MOVE OBKR-IDARTNR-TILLK                                          
110700                             TO MOD-IDARTNR-1--9(WS-INDEX-MOD)            
110800         MOVE '-'            TO MOD-STRAEK(WS-INDEX-MOD)                  
110900         MOVE OBKR-REKSIFFR-TILLK                                         
111000                             TO MOD-REKSIFFR(WS-INDEX-MOD)                
111100       END-IF                                                             
111200     ELSE                                                                 
111300       IF OBKR-KDORDBEK = 41 OR 61                                        
111400*------ ERSATT ARTIKEL                                                    
111500                                                                          
111600         PERFORM HBAAA-FIXA-ERSATNING-RAD                                 
111700       END-IF                                                             
111800     END-IF                                                               
111900                                                                          
112000     MOVE OBKR-IDLOPNR         TO WS-AKT-IDLOPNR                          
112100     MOVE OBKR-IDSEKVNR        TO WS-AKT-IDSEKVNR                         
112200     MOVE OBKR-IDARTNR         TO WS-AKT-IDARTNR-URS                      
112300                                                                          
112400     MOVE WS-AKT-KEYS          TO MOD-NYCKLAR(WS-INDEX-MOD)               
112500     .                                                                    
112600     EJECT                                                                
112700 HBAAA-FIXA-ERSATNING-RAD SECTION.                                        
112800                                                                          
112900     IF (OBKR-IDARTNR NOT = WS-IDARTNR-SPAR)     OR                       
113000           (OBKR-IDARTNR = WS-IDARTNR-SPAR  AND                           
113100              OBKR-IDLOPNR NOT = WS-IDLOPNR-SPAR)                         
113200       PERFORM S22-HAMTA-BENAMNING                                        
113300       MOVE MFS-STAENG-FAELT-OSYNLIGT                                     
113400                    TO  MOD-KVBEART-ATTR(WS-INDEX-MOD)                    
113500                        MOD-KVQPACK-ATTR(WS-INDEX-MOD)                    
113600     ELSE                                                                 
113700       IF OBKR-IDARTNR-TILLK = +0                                         
113800*------ TILLKOMMANDE TEXT                                                 
113900         MOVE MFS-RENSA-FAELT                                             
114000                         TO MOD-IDARTNR(WS-INDEX-MOD)                     
114100         MOVE OBKR-BEERS TO MOD-BEART(WS-INDEX-MOD)                       
114200         MOVE MFS-STAENG-FAELT-OSYNLIGT                                   
114300                      TO MOD-KDBEHX-ATTR(WS-INDEX-MOD)                    
114400                         MOD-KVBEART-ATTR(WS-INDEX-MOD)                   
114500                         MOD-KVQPACK-ATTR(WS-INDEX-MOD)                   
114600       ELSE                                                               
114700*-------TILLKOMMANDE ARTIKEL                                              
114800         MOVE OBKR-IDARTNR-TILLK                                          
114900                         TO MOD-IDARTNR-1--9(WS-INDEX-MOD)                
115000         MOVE '-'      TO MOD-STRAEK(WS-INDEX-MOD)                        
115100         MOVE OBKR-REKSIFFR-TILLK                                         
115200                       TO MOD-REKSIFFR(WS-INDEX-MOD)                      
115300         PERFORM S22-HAMTA-BENAMNING                                      
115400         MOVE OBKR-KVBEART-TILLK                                          
115500                       TO MOD-KVBEART(WS-INDEX-MOD)                       
115600         MOVE OBKR-DIERS-KVOT                                             
115700                       TO MOD-KVQPACK(WS-INDEX-MOD)                       
115800       END-IF                                                             
115900     END-IF                                                               
116000     .                                                                    
116100     EJECT                                                                
116200 HBAB-VISA-SATS-ARTIKLAR SECTION.                                         
116300                                                                          
116400     MOVE NEJ                  TO NEXT-SATS-SW                            
116500                                                                          
116600     MOVE +1                   TO WS-INDEX-SATS                           
116700     PERFORM UNTIL WS-INDEX-SATS > WS-INDEX-SATS-MAX                      
116800       MOVE SPACE             TO WS-IDARTNR-SATS(WS-INDEX-SATS)           
116900       ADD +1                 TO WS-INDEX-SATS                            
117000     END-PERFORM                                                          
117100                                                                          
117200     MOVE +1                   TO WS-INDEX-SATS                           
117300                                  WS-INDEX                                
117400     MOVE OBKR-IDARTNR         TO W-J1-IDARTNR                            
117500                                                                          
117600     PERFORM IMS-GU-SATB-WDJ111-01                                        
117700     PERFORM UNTIL SEGMENT-SAKNAS                                         
117800             OR    WS-INDEX-SATS > WS-INDEX-SATS-MAX                      
117900             OR    WS-INDEX  > +10                                        
118000                                                                          
118100       MOVE RAD-TISTADAT   TO TMP1-YYMMDD                                 
118200       MOVE RAD-TISTODAT   TO TMP2-YYMMDD                                 
118300       MOVE DAGENS-DATUM   TO TMP3-YYMMDD                                 
118400       PERFORM WY2000Q1                                                   
118500       IF STR-IDARTNR < +100000000 AND STR-TIBORT = +0 AND                
118600           TMP1-YYMMDD <= TMP3-YYMMDD       AND                           
118700           TMP2-YYMMDD >= TMP3-YYMMDD                                     
118800                                                                          
118900         IF WS-INDEX-MOD = WS-INDEX-MOD-MAX                               
119000           MOVE JA             TO NEXT-SATS-SW                            
119100           MOVE +6             TO WS-INDEX-SATS-MAX                       
119200         ELSE                                                             
119300            MOVE STR-IDARTNR TO WS-NUM-9                                  
119400            MOVE WS-ALFA-9   TO WS-IDARTNR-SATS(WS-INDEX-SATS)            
119500            ADD +1           TO WS-INDEX-SATS                             
119600         END-IF                                                           
119700       END-IF                                                             
119800       IF WS-INDEX-SATS NOT > WS-INDEX-SATS-MAX                           
119900         PERFORM IMS-GN-SATB-WDJ111-01                                    
120000         ADD  +1             TO WS-INDEX                                  
120100       END-IF                                                             
120200     END-PERFORM                                                          
120300     IF WS-INDEX-SATS NOT > WS-INDEX-SATS-MAX                             
120400                                                                          
120500       COMPUTE WS-INDEX = WS-INDEX-MOD + WS-INDEX-SATS - 1                
120600       IF WS-INDEX NOT > WS-INDEX-MOD-MAX                                 
120700                                                                          
120800         MOVE +1       TO WS-INDEX-SATS                                   
120900         PERFORM UNTIL WS-INDEX-SATS > WS-INDEX-SATS-MAX                  
121000               OR WS-IDARTNR-SATS(WS-INDEX-SATS) = SPACE                  
121100                                                                          
121200           ADD +1     TO WS-INDEX-MOD                                     
121300           PERFORM HBABA-REDIGERA-SATS-RAD                                
121400           ADD +1     TO WS-INDEX-SATS                                    
121500         END-PERFORM                                                      
121600       ELSE                                                               
121700         MOVE JA       TO NEXT-SATS-SW                                    
121800       END-IF                                                             
121900     END-IF                                                               
122000     IF NEXT-SATS                                                         
122100*-----OM EJ ALLA SATS-ART FÅR PLATS PÅ SIDAN RADERAS RAD MED              
122200*-----ORDBEK = 57 OCH DEN SPARAS FÖR NÄSTA SIDA(I HBB-SECTIONEN)          
122300       MOVE MFS-RENSA-FAELT   TO MOD-KDORDBEK(WS-INDEX-MOD)               
122400                                 MOD-KDBEHX(WS-INDEX-MOD)                 
122500                                 MOD-IDARTNR(WS-INDEX-MOD)                
122600                                 MOD-BEART(WS-INDEX-MOD)                  
122700                                 MOD-KVBEART(WS-INDEX-MOD)                
122800                                 MOD-KVQPACK(WS-INDEX-MOD)                
122900       MOVE MED-FLER-SIDOR    TO MED-IDMFSINF                             
123000       MOVE MED-UPPDAT-PF     TO MED-IDMFSFEL                             
123100     END-IF                                                               
123200     .                                                                    
123300     EJECT                                                                
123400                                                                          
123500 HBABA-REDIGERA-SATS-RAD SECTION.                                         
123600                                                                          
123700     MOVE OBKR-KDORDBEK        TO MOD-KDORDBEK(WS-INDEX-MOD)              
123800     MOVE MFS-STAENG-FAELT-OSYNLIGT                                       
123900                               TO MOD-KDORDBEK-ATTR(WS-INDEX-MOD)         
124000                                                                          
124100     MOVE SPACE                TO MOD-ASTERIX(WS-INDEX-MOD)               
124200     MOVE 'B'                  TO MOD-KDBEHX(WS-INDEX-MOD)                
124300     MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR(WS-INDEX-MOD)               
124400                                                                          
124500     MOVE WS-IDARTNR-SATS(WS-INDEX-SATS)                                  
124600                               TO MOD-BEART(WS-INDEX-MOD)                 
124700     INSPECT MOD-BEART(WS-INDEX-MOD) REPLACING LEADING ZERO BY            
124800                                                         SPACE            
124900     MOVE MFS-STAENG-FAELT-OSYNLIGT                                       
125000                          TO  MOD-KVBEART-ATTR(WS-INDEX-MOD)              
125100                              MOD-KVQPACK-ATTR(WS-INDEX-MOD)              
125200                                                                          
125300     MOVE WS-AKT-KEYS          TO MOD-NYCKLAR(WS-INDEX-MOD)               
125400     .                                                                    
125500     EJECT                                                                
125600                                                                          
125700 HBB-FIXA-BLADDRINGS-VARDEN SECTION.                                      
125800                                                                          
125900     IF OBKR-SEGMENT-FINNS                                                
126000       MOVE OBKR-IDARTNR      TO MOD-IDARTNR-NEXT                         
126100       MOVE OBKR-IDLOPNR      TO MOD-IDLOPNR-NEXT                         
126200       MOVE OBKR-IDSEKVNR     TO MOD-IDSEKVNR-NEXT                        
126300       MOVE OBKR-KDORDBEK     TO MOD-KDORDBEK-NEXT                        
126400                                                                          
126500       MOVE MED-FLER-SIDOR    TO MED-IDMFSINF                             
126600       MOVE MED-UPPDAT-PF     TO MED-IDMFSFEL                             
126700     ELSE                                                                 
126800       MOVE ZERO              TO MOD-IDARTNR-NEXT                         
126900                                 MOD-IDLOPNR-NEXT                         
127000                                 MOD-IDSEKVNR-NEXT                        
127100                                 MOD-KDORDBEK-NEXT                        
127200     END-IF                                                               
127300                                                                          
127400     .                                                                    
127500     EJECT                                                                
127600                                                                          
127700 HBC-KONTROLLERA-SIDSLUT SECTION.                                         
127800                                                                          
127900     MOVE WS-INDEX-MOD-MAX     TO WS-INDEX-MOD                            
128000     MOVE MOD-NYCKLAR(WS-INDEX-MOD)                                       
128100                               TO WS-AKT-KEYS                             
128200     IF OBKR-IDARTNR = WS-AKT-IDARTNR-URS AND                             
128300       OBKR-IDLOPNR = WS-AKT-IDLOPNR                                      
128400                                                                          
128500       PERFORM UNTIL WS-INDEX-MOD = +1 OR                                 
128600        (OBKR-IDARTNR NOT = WS-AKT-IDARTNR-URS OR                         
128700         OBKR-IDLOPNR NOT = WS-AKT-IDLOPNR)                               
128800                                                                          
128900         SUBTRACT 1 FROM WS-INDEX-MOD                                     
129000         MOVE MOD-NYCKLAR(WS-INDEX-MOD) TO WS-AKT-KEYS                    
129100       END-PERFORM                                                        
129200                                                                          
129300       ADD +1 TO WS-INDEX-MOD                                             
129400                                                                          
129500*---- BLÄDDRINGSVÄRDENA MÅSTE JUSTERAS OM NÄR VI BACKAR RADER             
129600                                                                          
129700       MOVE MOD-NYCKLAR(WS-INDEX-MOD)                                     
129800                              TO WS-AKT-KEYS                              
129900       MOVE WS-AKT-IDARTNR-URS TO MOD-IDARTNR-NEXT                        
130000       MOVE WS-AKT-IDLOPNR    TO MOD-IDLOPNR-NEXT                         
130100       MOVE WS-AKT-IDSEKVNR   TO MOD-IDSEKVNR-NEXT                        
130200       MOVE MOD-KDORDBEK(WS-INDEX-MOD)                                    
130300                              TO MOD-KDORDBEK-NEXT                        
130400       MOVE WS-INDEX-MOD      TO WS-INDEX                                 
130500       PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                          
130600         MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(WS-INDEX)                   
130700                                 MOD-ASTERIX(WS-INDEX)                    
130800                                 MOD-KDBEHX(WS-INDEX)                     
130900                                 MOD-IDARTNR(WS-INDEX)                    
131000                                 MOD-BEART(WS-INDEX)                      
131100                                 MOD-KVBEART(WS-INDEX)                    
131200                                 MOD-KVQPACK(WS-INDEX)                    
131300                                 MOD-NYCKLAR(WS-INDEX)                    
131400         ADD +1               TO WS-INDEX                                 
131500       END-PERFORM                                                        
131600     ELSE                                                                 
131700       ADD +1                 TO WS-INDEX-MOD                             
131800     END-IF                                                               
131900                                                                          
132000     MOVE MED-FLER-SIDOR       TO MED-IDMFSINF                            
132100     MOVE MED-UPPDAT-PF        TO MED-IDMFSFEL                            
132200     .                                                                    
132300     EJECT                                                                
132400                                                                          
132500 HD-UPPDATERA-OBEH-RADER SECTION.                                         
132600                                                                          
132700     MOVE NEJ                  TO AKT-SIDA-SW                             
132800                                                                          
132900     IF STARTAD-AV-DISPATCHEN                                             
133000       MOVE HIGH-VALUE           TO W-WDQ101-KEY-MAX                      
133100       MOVE W-Q1-IDORDER-UNIK    TO W-Q1-IDORDER-MAX                      
133200     ELSE                                                                 
133300       MOVE MID-RAD(1)           TO WS-AKTUELL-MID-RAD                    
133400       MOVE WS-AKT-IDARTNR-URS   TO W-Q1-IDARTNR-MAX                      
133500       MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-MAX                      
133600       MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-MAX                     
133700       MOVE WC-CDC-SE            TO W-Q1-IDDC-MAX                         
133800       MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-MAX                     
133900     END-IF                                                               
134000     PERFORM IMS-GHU-ORQM-WDQ101-FOERE                                    
134100     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
134200       PERFORM S02-GODKANN-RAD                                            
134300       PERFORM IMS-GHN-ORQM-WDQ101-FOERE                                  
134400     END-PERFORM                                                          
134500                                                                          
134600     MOVE ALL '9'              TO W-Q1-IDARTNR-MAX                        
134700                                  W-Q1-IDLOPNR-MAX                        
134800                                  W-Q1-IDSEKVNR-MAX                       
134900                                  W-Q1-KDORDBEK-MAX                       
135000     MOVE WC-CDC-SE            TO W-Q1-IDDC-MAX                           
135100     .                                                                    
135200     EJECT                                                                
135300 HE-UPPDATERA-AKT-SIDA SECTION.                                           
135400                                                                          
135500     MOVE JA                   TO AKT-SIDA-SW                             
135600     MOVE +1 TO WS-INDEX-MID                                              
135700     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
135800                   MID-KDORDBEK(WS-INDEX-MID) = ZERO                      
135900       MOVE MID-RAD(WS-INDEX-MID)                                         
136000                             TO WS-AKTUELL-MID-RAD                        
136100       MOVE WS-AKT-IDARTNR-URS   TO W-Q1-IDARTNR-UNIK                     
136200       MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-UNIK                     
136300       MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-UNIK                    
136400       MOVE WC-CDC-SE            TO W-Q1-IDDC-UNIK                        
136500       MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-UNIK                    
136600                                                                          
136700       PERFORM IMS-GHU-ORQM-WDQ101-UNIK                                   
136800       IF SEGMENT-FINNS                                                   
136900         IF MID-IDARTNR(WS-INDEX-MID) = ZERO                              
137000           MOVE JA       TO OBKR-FLOBOK                                   
137100           PERFORM IMS-REPL-ORQM-WDQ101                                   
137200         ELSE                                                             
137300           IF WS-AKT-KDBEHX = SPACE                                       
137400             PERFORM S02-GODKANN-RAD                                      
137500           ELSE                                                           
137600             IF WS-AKT-KDBEHX = 'A'                                       
137700               MOVE JA       TO OBKR-FLOBOK                               
137800               PERFORM IMS-REPL-ORQM-WDQ101                               
137900             ELSE                                                         
138000               IF WS-AKT-KDBEHX = 'D'                                     
138100                 PERFORM HEA-ANNULLERA-RAD                                
138200               ELSE                                                       
138300                 IF WS-AKT-KDBEHX = '1' OR '2'                            
138400                   PERFORM HEA-ANNULLERA-RAD                              
138500                   IF WS-AKT-KDBEHX = '1'                                 
138600                     MOVE +1       TO OBKR-KDKVBRYT                       
138700                   ELSE                                                   
138800                     MOVE +2       TO OBKR-KDKVBRYT                       
138900                   END-IF                                                 
139000                   MOVE +0         TO OBKR-KVPREAVB                       
139100                                      OBKR-KVPRERO                        
139200                   PERFORM S01-SKRIV-MID-TILL-4262                        
139300                 ELSE                                                     
139400                   MOVE JA TO OBKR-FLOBOK                                 
139500                   PERFORM IMS-REPL-ORQM-WDQ101                           
139600                 END-IF                                                   
139700               END-IF                                                     
139800             END-IF                                                       
139900           END-IF                                                         
140000         END-IF                                                           
140100       END-IF                                                             
140200       ADD +1                  TO WS-INDEX-MID                            
140300     END-PERFORM                                                          
140400     .                                                                    
140500     EJECT                                                                
140600                                                                          
140700 HEA-ANNULLERA-RAD SECTION.                                               
140800                                                                          
140900     IF OBKR-KVPREAVB > +0 AND PHUV-KDPROTYP = 'O' OR 'L'                 
141000       IF OBKR-IDARTNR-TILLK > +0                                         
141100         MOVE OBKR-IDARTNR-TILLK TO W-IDARTNR                             
141200       ELSE                                                               
141300         MOVE OBKR-IDARTNR       TO W-IDARTNR                             
141400       END-IF                                                             
141500       PERFORM IMS-GHU-ARTM-WDK901                                        
141600                                                                          
141700       COMPUTE ART-KVOFFERT =                                             
141800               ART-KVOFFERT - OBKR-KVBEART-Q                              
141900                                                                          
142000       PERFORM IMS-REPL-ARTM-WDK901                                       
142100     END-IF                                                               
142200     PERFORM IMS-DLET-ORQM-WDQ101                                         
142300     .                                                                    
142400     EJECT                                                                
142500 HG-UPPDATERA-RESTERANDE-RADER SECTION.                                   
142600                                                                          
142700     MOVE NEJ                  TO AKT-SIDA-SW                             
142800*--- BEHANDLA RESTERANDE OBKR PÅ ORDERN                                   
142900                                                                          
143000     PERFORM IMS-GHU-ORQM-WDQ101-MIN-MAX                                  
143100     MOVE +1                   TO WS-RADER                                
143200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR WS-RADER > +13         
143300                                                                          
143400        PERFORM S02-GODKANN-RAD                                           
143500        ADD  +1               TO WS-RADER                                 
143600                                                                          
143700        PERFORM IMS-GHN-ORQM-WDQ101-MIN-MAX                               
143800     END-PERFORM                                                          
143900                                                                          
144000     IF START-4262                                                        
144100        MOVE 'V'              TO 4262-MID-KDTRTYP                         
144200        PERFORM S03-STARTA-RADBEHANDLINGEN                                
144300     ELSE                                                                 
144400        IF WS-RADER = 14 AND SEGMENT-FINNS                                
144500           PERFORM HGA-OMSKEDULERA                                        
144600        ELSE                                                              
144700           MOVE JA             TO AVSLUTA-SW                              
144800        END-IF                                                            
144900     END-IF                                                               
145000     .                                                                    
145100     EJECT                                                                
145200 HGA-OMSKEDULERA SECTION.                                                 
145300                                                                          
145400     MOVE MFS-KDMFSFOR         TO 4263-SPRAK                              
145500     MOVE ALL '+'              TO 4263-IDDISTR-IN                         
145600                                  4263-IDKUNDNR-IN                        
145700                                  4263-IDORDNR-IN                         
145800                                  4263-IDARTNR-IN                         
145900     MOVE WS-IDDISTR           TO 4263-IDDISTR-UT                         
146000     MOVE WS-IDKUNDNR          TO 4263-IDKUNDNR-UT                        
146100     MOVE WS-IDORDNR           TO 4263-IDORDNR-UT                         
146200                                                                          
146300     PERFORM IMS-INSERT-4263-MSG                                          
146400     MOVE JA                   TO HOPP                                    
146500     .                                                                    
146600     EJECT                                                                
146700 I-UPPDATERA-PHUV SECTION.                                                
146800                                                                          
146900     ACCEPT PHUV-TIUPPDAT FROM DATE                                       
147000     ACCEPT PHUV-TIUPPTID FROM TIME                                       
147100     IF DIST79-DEALER-PRICE                                               
147200        ADD WS-SUORDV-RAKN-LOC     TO PHUV-SUORDV-LOC                     
147300        ADD WS-SUORDV-RAKN-LOCPREL TO PHUV-SUORDV-LOCPREL                 
147400     ELSE                                                                 
147500        ADD WS-SUORDV-RAKN         TO PHUV-SUORDV                         
147600     END-IF                                                               
147700     COMPUTE WS-VLORDBTO ROUNDED = WS-SA-VLARTNTO-RAKN / 1000000          
147800     ADD WS-VLORDBTO TO PHUV-VLORDBTO                                     
147900     COMPUTE WS-VKORDNTO ROUNDED = WS-SA-VKART-RAKN / 1000                
148000     ADD WS-VKORDNTO TO PHUV-VKORDNTO                                     
148100     ADD WS-KVMOTOR  TO PHUV-KVMOTOR                                      
148200     ADD WS-KVKAROSS TO PHUV-KVKAROSS                                     
148300     IF DIST79-DEALER-PRICE                                               
148400       COMPUTE PHUV-PREMBHNT ROUNDED =                                    
148500                     (((PHUV-SUORDV-LOC + PHUV-SUORDV-LOCPREL -           
148600                        PHUV-PRAVDRAG) *                                  
148700                        PHUV-REEMBHNT) / 100)                             
148800     ELSE                                                                 
148900       COMPUTE PHUV-PREMBHNT ROUNDED =                                    
149000                        (((PHUV-SUORDV - PHUV-PRAVDRAG) *                 
149100                           PHUV-REEMBHNT) / 100)                          
149200     END-IF                                                               
149300     PERFORM IMS-REPL-PROC-WDE801                                         
149400     .                                                                    
149500     EJECT                                                                
149600 M-HOPPA-TILL-ORDERHUVUD-4261 SECTION.                                    
149700                                                                          
149800     IF NOT STARTAD-AV-DISPATCHEN                                         
149900       MOVE 'W4O26101'           TO MFS-IDMOD                             
150000                                                                          
150100       MOVE '4261'               TO MOD-IDTRANS                           
150200                                                                          
150300       MOVE ERR-ORDER-AVSLUTAD   TO MED-IDMFSFEL                          
150400       CALL WMEDKONV USING MED-WMEDAREA                                   
150500       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
150600                                                                          
150700       MOVE 4261-MOD-LAENGD      TO MSG-KVLL                              
150800       PERFORM IMS-INSERT-MSG                                             
150900       MOVE JA                   TO HOPP                                  
151000     END-IF                                                               
151100     .                                                                    
151200     EJECT                                                                
151300 Z-FINIT SECTION.                                                         
151400                                                                          
151500     IF MED-IDMFSFEL NOT = SPACE OR MED-IDMFSINF NOT = SPACE              
151600         CALL WMEDKONV USING MED-WMEDAREA                                 
151700         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
151800         MOVE MED-MFSINF       TO MOD-TEMFSINF                            
151900     END-IF                                                               
152000     IF NOT ALLT-OK AND NYCKEL-OK                                         
152100        PERFORM MFS-ROER-EJ-BILD                                          
152200     END-IF                                                               
152300                                                                          
152400     IF STARTAD-AV-DISPATCHEN                                             
152500        MOVE MED-IDMFSFEL             TO MSG-KOM-IDMFSMED                 
152600        MOVE '4'                      TO MSG-KOM-KDSVAR                   
152700                                                                          
152800        IF MSG-KOM-IDMFSMED = SPACE  OR  ERR-ORDER-AVSLUTAD               
152900           MOVE OK-BEHANDLAD      TO MSG-KOM-IDMFSMED                     
153000        END-IF                                                            
153100                                                                          
153200        PERFORM IMS-INSERT-DISP-MSG                                       
153300     ELSE                                                                 
153400        MOVE MAX-MOD-LAENGD       TO MSG-KVLL                             
153500        PERFORM IMS-INSERT-MSG                                            
153600     END-IF                                                               
153700     .                                                                    
153800     EJECT                                                                
153900 S01-SKRIV-MID-TILL-4262 SECTION.                                         
154000                                                                          
154100     MOVE JA                   TO START-4262-SW                           
154200     ADD +1                    TO 4262-MID-IX                             
154201                                                                          
154210     IF OBKR-IDARTNR-TILLK > +0                                           
154220       MOVE OBKR-IDARTNR-TILLK  TO WS-IDARTNR                             
154230       MOVE OBKR-REKSIFFR-TILLK TO WS-REKSIFFR                            
154240     ELSE                                                                 
154250       MOVE OBKR-IDARTNR       TO WS-IDARTNR                              
154260       MOVE OBKR-REKSIFFR      TO WS-REKSIFFR                             
154270     END-IF                                                               
154280     MOVE WS-IDARTNR-REKSIFFR  TO                                         
154290                               4262-MID-IDARTNR-006(4262-MID-IX)          
154300                                                                          
154400     IF OBKR-KVBEART-TILLK > +0                                           
154500       MOVE OBKR-KVBEART-TILLK TO WS-NUM-6                                
154600       MOVE WS-ALFA-6          TO 4262-MID-KVBEART(4262-MID-IX)           
154700     ELSE                                                                 
154800       IF OBKR-KVPREAVB > +0  OR  OBKR-KVPRERO > +0                       
154900          COMPUTE WS-NUM-6 = OBKR-KVPREAVB + OBKR-KVPRERO                 
155000          MOVE WS-ALFA-6       TO 4262-MID-KVBEART(4262-MID-IX)           
155100       ELSE                                                               
155200          MOVE OBKR-KVBEART    TO WS-NUM-6                                
155300          MOVE WS-ALFA-6       TO 4262-MID-KVBEART(4262-MID-IX)           
155400       END-IF                                                             
155500     END-IF                                                               
155600                                                                          
155700     MOVE ALL '+'              TO 4262-MID-PRARTNTO(4262-MID-IX)          
155800     MOVE OBKR-KDKVBRYT        TO 4262-MID-KDKVBRYT(4262-MID-IX)          
155900     MOVE OBKR-FLINVEST        TO 4262-MID-FLINVEST(4262-MID-IX)          
156000     MOVE OBKR-BERADREF        TO 4262-MID-BERADREF(4262-MID-IX)          
156100     .                                                                    
156200     EJECT                                                                
156300                                                                          
156400 S02-GODKANN-RAD SECTION.                                                 
156500                                                                          
156600     MOVE JA                   TO OBKR-FLOBOK                             
156700     PERFORM IMS-REPL-ORQM-WDQ101                                         
156800                                                                          
156900     IF OBKR-KVPREAVB > +0                                                
157000       PERFORM S14-LAS-ARTIKELREG                                         
157100       PERFORM S12-BYGG-UPP-ORDERRAD                                      
157200       PERFORM S10-SKRIV-PRAD                                             
157300     END-IF                                                               
157400                                                                          
157500     IF (OBKR-KDORDBEK =  61  ) AND AKT-SIDA                              
157600       PERFORM S01-SKRIV-MID-TILL-4262                                    
157700       PERFORM S02B-SKRIV-ORDERBEKR-40                                    
157800     END-IF                                                               
157900     .                                                                    
158000     EJECT                                                                
158100 S02B-SKRIV-ORDERBEKR-40 SECTION.                                         
158200                                                                          
158300     MOVE OBKR-KVBEART-TILLK   TO SPAR-KVBEART-TILLK                      
158400     MOVE OBKR-IDARTNR-TILLK   TO SPAR-IDARTNR-TILLK                      
158500     MOVE OBKR-REKSIFFR-TILLK  TO SPAR-REKSIFFR-TILLK                     
158600     MOVE OBKR-DIERS-KVOT      TO SPAR-DIERS-KVOT                         
158700                                                                          
158800                                                                          
158900     IF OBKR-IDARTNR NOT = SPAR-IDARTNR-40  OR                            
159000         (OBKR-IDARTNR = SPAR-IDARTNR-40   AND                            
159100          OBKR-IDLOPNR NOT = SPAR-IDLOPNR-40)                             
159200                                                                          
159300       PERFORM S02BA-SKRIV-KOD40-ERSATT-ART                               
159400       COMPUTE SPAR-IDSEKVNR-40 = OBKR-IDSEKVNR + 1                       
159500     END-IF                                                               
159600                                                                          
159700     MOVE SPAR-KVBEART-TILLK   TO OBKR-KVBEART-TILLK                      
159800     MOVE SPAR-IDARTNR-TILLK   TO OBKR-IDARTNR-TILLK                      
159900     MOVE SPAR-REKSIFFR-TILLK  TO OBKR-REKSIFFR-TILLK                     
160000     MOVE SPAR-DIERS-KVOT      TO OBKR-DIERS-KVOT                         
160100                                                                          
160200     MOVE JA                   TO OBKR-FLOBOK                             
160300     MOVE 40                   TO OBKR-KDORDBEK                           
160400     MOVE IDPGM                TO OBKR-IDPGM                              
160500     MOVE SPAR-IDSEKVNR-40     TO OBKR-IDSEKVNR                           
160600                                                                          
160700     PERFORM IMS-ISRT-ORQM-WDQ101                                         
160800     ADD +1                 TO OBKR-IDSEKVNR                              
160900                               SPAR-IDSEKVNR-40                           
161000     MOVE OBKR-IDARTNR         TO SPAR-IDARTNR-40                         
161100     MOVE OBKR-IDLOPNR         TO SPAR-IDLOPNR-40                         
161200     .                                                                    
161300     EJECT                                                                
161400 S02BA-SKRIV-KOD40-ERSATT-ART SECTION.                                    
161500                                                                          
161600     MOVE OBKR-IDORDER      TO W-IDORDER-Q1-MIN2                          
161700                               W-IDORDER-Q1-MAX2                          
161800     MOVE OBKR-IDARTNR      TO W-IDARTNR                                  
161900                               W-IDARTNR-Q1-MIN2                          
162000                               W-IDARTNR-Q1-MAX2                          
162100     MOVE OBKR-IDLOPNR      TO W-IDLOPNR-Q1-MIN2                          
162200                               W-IDLOPNR-Q1-MAX2                          
162300                                                                          
162400     PERFORM IMS-GU-ORQM-WDQ101                                           
162500     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
162600                                                                          
162700        PERFORM IMS-GN-ORQM-WDQ101                                        
162800     END-PERFORM                                                          
162900                                                                          
163000     MOVE +0                TO OBKR-IDARTNR-TILLK                         
163100                               OBKR-KVBEART-TILLK                         
163200                               OBKR-REKSIFFR-TILLK                        
163300                               OBKR-DIERS-KVOT                            
163400     MOVE JA                TO OBKR-FLOBOK                                
163500     MOVE 40                TO OBKR-KDORDBEK                              
163600     MOVE IDPGM             TO OBKR-IDPGM                                 
163700     ADD +1                 TO OBKR-IDSEKVNR                              
163800                                                                          
163900     PERFORM IMS-ISRT-ORQM-WDQ101                                         
164000     .                                                                    
164100     EJECT                                                                
164200                                                                          
164300 S03-STARTA-RADBEHANDLINGEN SECTION.                                      
164400                                                                          
164500     MOVE MFS-KDMFSFOR         TO 4262-SPRAK                              
164600                                                                          
164700     MOVE ALL '+'              TO 4262-MID-IDDISTR-IN                     
164800                                  4262-MID-IDKUNDNR-IN                    
164900                                  4262-MID-IDORDNR-IN                     
165000                                  4262-MID-IDARTNR-IN                     
165100     MOVE WS-IDDISTR           TO 4262-MID-IDDISTR-UT                     
165200     MOVE WS-IDKUNDNR          TO 4262-MID-IDKUNDNR-UT                    
165300     MOVE WS-IDORDNR           TO 4262-MID-IDORDNR-UT                     
165400                                                                          
165500     PERFORM IMS-INSERT-4262-MSG                                          
165600     MOVE JA                   TO HOPP                                    
165700     .                                                                    
165800     EJECT                                                                
165900                                                                          
166000 S10-SKRIV-PRAD SECTION.                                                  
166100                                                                          
166200     PERFORM IMS-ISRT-PROD-WDE901                                         
166300     PERFORM UNTIL SEGMENT-FINNS                                          
166400       ADD +1                    TO PRAD-IDLOPNR                          
166500       PERFORM IMS-ISRT-PROD-WDE901                                       
166600     END-PERFORM                                                          
166700                                                                          
166800     PERFORM S10A-ADDERA-TILL-PHUV                                        
166900     IF PRAD-KDTPOTYP = +5                                                
167000       PERFORM S10B-LAGG-UPP-LARM                                         
167100     END-IF                                                               
167200                                                                          
167300     .                                                                    
167400     EJECT                                                                
167500 S10A-ADDERA-TILL-PHUV SECTION.                                           
167600                                                                          
167700     IF AREG-IDFKNGRP = 2101 OR 2102                                      
167800       ADD +1 TO WS-KVMOTOR                                               
167900     ELSE                                                                 
168000       IF AREG-IDFKNGRP = 8001 OR 8002                                    
168100         ADD +1 TO WS-KVKAROSS                                            
168200       END-IF                                                             
168300     END-IF                                                               
168400                                                                          
168500     IF DIST79-DEALER-PRICE                                               
168600       COMPUTE WS-SUORDV-LOC = PRAD-PRARTNTO-LOC *                        
168700                               PRAD-KVBEART-Q                             
168800       ADD WS-SUORDV-LOC TO WS-SUORDV-RAKN-LOC                            
168900       COMPUTE WS-SUORDV-LOCPREL = PRAD-PRARTNTO-LOCPREL *                
169000                               PRAD-KVBEART-Q                             
169100       ADD WS-SUORDV-LOCPREL TO WS-SUORDV-RAKN-LOCPREL                    
169200     ELSE                                                                 
169300       COMPUTE WS-SUORDV = PRAD-PRARTNTO * PRAD-KVBEART-Q                 
169400       ADD WS-SUORDV TO WS-SUORDV-RAKN                                    
169500     END-IF                                                               
169600     COMPUTE WS-SA-VLARTNTO = (PRAD-VLARTNTO * PRAD-KVBEART-Q)            
169700     ADD WS-SA-VLARTNTO TO WS-SA-VLARTNTO-RAKN                            
169800     COMPUTE WS-SA-VKART = (PRAD-VKART * PRAD-KVBEART-Q)                  
169900     ADD WS-SA-VKART TO WS-SA-VKART-RAKN                                  
170000                                                                          
170100     .                                                                    
170200     EJECT                                                                
170300 S10B-LAGG-UPP-LARM SECTION.                                              
170400                                                                          
170500     PERFORM S10BA-BERAKNA-BEKRTIDPKT                                     
170600                                                                          
170700     MOVE AREG-IDANSK TO W-IDANSK-2232                                    
170800     PERFORM IMS-GU-XXBX-WDR220                                           
170900     IF SEGMENT-FINNS                                                     
171000       MOVE XXBX-2232-IDANSK-LARM TO W-IDANSK-2223                        
171100     ELSE                                                                 
171200       MOVE ZERO TO W-IDANSK-2223                                         
171300     END-IF                                                               
171400     MOVE '2223'        TO XXBU-2223-IDHTYP                               
171500     MOVE W-IDANSK-2223 TO XXBU-2223-IDANSK                               
171600     MOVE LOW-VALUE     TO XXBU-2223-LOW-VALUE                            
171700     PERFORM IMS-ISRT-XXBU-WDR501                                         
171800     PERFORM IMS-GHU-XXBU-WDR501                                          
171900     MOVE W-TISENBEK-DAG TO W-TISENBEK-DAG-2224                           
172000     MOVE W-TISENBEK-KL  TO W-TISENBEK-KL-2224                            
172100     MOVE 150            TO W-KDLARM-2224                                 
172200     PERFORM IMS-GNP-XXBU-WDR550                                          
172300     IF SEGMENT-FINNS                                                     
172400       PERFORM UNTIL SEGMENT-SAKNAS                                       
172500         ADD 1 TO W-TISENBEK-KL-2224                                      
172600         PERFORM S10BB-KOLLA-TIDEN                                        
172700         PERFORM IMS-GNP-XXBU-WDR550                                      
172800       END-PERFORM                                                        
172900     END-IF                                                               
173000     MOVE W-TISENBEK-DAG-2224 TO XXBU-2224-TISENBEK-DAG                   
173100     MOVE W-TISENBEK-KL-2224  TO XXBU-2224-TISENBEK-KL                    
173200     MOVE 150                 TO XXBU-2224-KDLARM                         
173300     MOVE PRAD-IDARTNR        TO XXBU-2224-IDARTNR                        
173310     MOVE WC-CDC-SE           TO XXBU-2224-IDDC                           
173400     MOVE JA                  TO XXBU-2224-FLNYLARM                       
173500     MOVE PHUV-IDDISTR        TO XXBU-2224-IDDISTR                        
173600     MOVE PHUV-IDKUNDNR       TO XXBU-2224-IDKUNDNR                       
173700     MOVE PHUV-IDKUNDRF       TO XXBU-2224-IDKUNDRF                       
173800     MOVE PRAD-IDLOPNR        TO XXBU-2224-IDLOPNR                        
173900     MOVE DAGENS-DATUM        TO XXBU-2224-TIREGDAT                       
174100     MOVE SPACE               TO XXBU-2224-IDTRANS                        
174200                                 XXBU-2224-KDMFSFOR                       
174300     MOVE ZERO                TO XXBU-2224-IDKR                           
174310     MOVE SPACE               TO XXBU-2224-IDLEVNR                        
174400                                                                          
174500     PERFORM IMS-ISRT-XXBU-WDR550                                         
174600                                                                          
174700     .                                                                    
174800     EJECT                                                                
174900 S10BA-BERAKNA-BEKRTIDPKT SECTION.                                        
175000                                                                          
175100     MOVE '1' TO W-KDSEGKEY                                               
175200     PERFORM IMS-GU-XXBV-WDR210                                           
175300     MOVE XXBV-2226-KVARBTIM(PHUV-KDORDKL , PRAD-KDTPOTYP)                
175400     TO TIME-TIARB                                                        
175500     MOVE WC-CDC-SE     TO TIME-IDDC                                      
175600     MOVE DAGENS-DATUM  TO TIME-START-TIAAMMDD                            
175700     MOVE DAGENS-HHMMSS TO TIME-START-TIHHMMSS                            
175800     MOVE 022           TO TIME-KDCALL                                    
175900     MOVE ZERO          TO TIME-STOPDAT                                   
176000                                                                          
176100     CALL W411TIME USING TIME-W411TIME TIME-4437-PCB.                     
176200                                                                          
176300     IF TIME-KDSVAR-OK                                                    
176400       MOVE TIME-STOP-TIAAMMDD TO W-TISENBEK-DAG                          
176500       MOVE TIME-STOP-TIHHMMSS TO W-TISENBEK-KL                           
176600     ELSE                                                                 
176700       MOVE 'FEL FRÅN PROGRAM W40263 I SECTION S10BA' TO FELTEXT          
176800       CALL ABEND USING RKOD-ABEND                                        
176900     END-IF                                                               
177000                                                                          
177100     .                                                                    
177200     EJECT                                                                
177300 S10BB-KOLLA-TIDEN SECTION.                                               
177400                                                                          
177500     MOVE W-TISENBEK-KL-2224 TO W-TISENBEK-KL-UPPD                        
177600     IF W-TISENBEK-KL-SS > 59                                             
177700       ADD 1 TO W-TISENBEK-KL-MM                                          
177800       MOVE ZERO TO W-TISENBEK-KL-SS                                      
177900       MOVE W-TISENBEK-KL-UPPD TO W-TISENBEK-KL-2224                      
178000       IF W-TISENBEK-KL-MM > 59                                           
178100         ADD 1 TO W-TISENBEK-KL-HH                                        
178200         MOVE ZERO TO W-TISENBEK-KL-MM                                    
178300         MOVE W-TISENBEK-KL-UPPD TO W-TISENBEK-KL-2224                    
178400       END-IF                                                             
178500     END-IF                                                               
178600                                                                          
178700     .                                                                    
178800     EJECT                                                                
178900 S12-BYGG-UPP-ORDERRAD SECTION.                                           
179000                                                                          
179100     MOVE OBKR-IDORDER         TO PRAD-IDORDER                            
179200     MOVE AREG-IDARTNR         TO PRAD-IDARTNR                            
179300     MOVE +1                   TO PRAD-IDLOPNR                            
179400     MOVE SPACE                TO PRAD-BEART                              
179500     MOVE OBKR-BERADREF        TO PRAD-BERADREF                           
179600     MOVE OBKR-FLINVEST        TO PRAD-FLINVEST                           
179700     MOVE OBKR-FLPRTILL        TO PRAD-FLPRTILL                           
179800     MOVE OBKR-FLRESTN         TO PRAD-FLRESTN                            
179900     MOVE OBKR-IDLEVNR         TO PRAD-IDLEVNR                            
180000     MOVE +0                   TO PRAD-IDSPECEMB                          
180100     MOVE AREG-KDARTURS        TO PRAD-KDARTURS                           
180200     MOVE OBKR-IDSYSTEM        TO PRAD-IDSYSTEM                           
180300     MOVE OBKR-KDKVBRYT        TO PRAD-KDKVBRYT                           
180400     MOVE PHUV-KDORDING        TO PRAD-KDORDING                           
180500     MOVE AREG-KDPRODSL        TO PRAD-KDPRODSL                           
180600     MOVE OBKR-KDPRTYP         TO PRAD-KDPRTYP                            
180700     MOVE AREG-KDSPEEMB        TO PRAD-KDSPEEMB                           
180800     MOVE OBKR-KDTPOTYP        TO PRAD-KDTPOTYP                           
180900     MOVE +0                   TO PRAD-KVVECKOR-TPO5                      
181000     MOVE OBKR-KVBEART         TO PRAD-KVBEART                            
181100     MOVE OBKR-KVBEART-Q       TO PRAD-KVBEART-Q                          
181200     MOVE OBKR-PRARTNTO        TO PRAD-PRARTNTO                           
181300     MOVE OBKR-DEAL-PR-LINE    TO PRAD-DEAL-PR-LINE                       
181400     MOVE OBKR-PRBPRIS         TO PRAD-PRBPRIS                            
181500     MOVE AREG-REKSIFFR        TO PRAD-REKSIFFR                           
181600     MOVE OBKR-TIPRIS          TO PRAD-TIPRIS                             
181700     MOVE DAGENS-DATUM         TO PRAD-TIREGDAT                           
181800     MOVE DAGENS-HHMMSS        TO PRAD-TIREGTID                           
181900     MOVE AREG-VKART           TO PRAD-VKART                              
182000     MOVE AREG-VLARTNTO        TO PRAD-VLARTNTO                           
182100     .                                                                    
182200     EJECT                                                                
182300                                                                          
182400 S13-HITTA-FORSTA-I-GRUPPEN SECTION.                                      
182500                                                                          
182600     MOVE WS-AKT-IDARTNR       TO WS-IDARTNR-SPAR                         
182700     MOVE WS-AKT-IDLOPNR       TO WS-IDLOPNR-SPAR                         
182800     MOVE WS-AKT-IDARTNR-URS   TO WS-IDARTNR-URS-SPAR                     
182900                                                                          
183000     SUBTRACT 1 FROM WS-INDEX-MID                                         
183100     IF WS-INDEX-MID NOT = +0                                             
183200       MOVE MID-RAD(WS-INDEX-MID)                                         
183300                               TO WS-AKTUELL-MID-RAD                      
183400       PERFORM UNTIL WS-INDEX-MID = +0 OR                                 
183500                     WS-AKT-IDARTNR NOT = WS-IDARTNR-SPAR OR              
183600                     WS-AKT-IDLOPNR NOT = WS-IDLOPNR-SPAR OR              
183700                     WS-AKT-IDARTNR-URS NOT =                             
183800                     WS-IDARTNR-URS-SPAR                                  
183900         SUBTRACT 1        FROM WS-INDEX-MID                              
184000         IF WS-INDEX-MID NOT = +0                                         
184100           MOVE MID-RAD(WS-INDEX-MID)                                     
184200                              TO WS-AKTUELL-MID-RAD                       
184300         END-IF                                                           
184400       END-PERFORM                                                        
184500     END-IF                                                               
184600     .                                                                    
184700     EJECT                                                                
184800                                                                          
184900 S14-LAS-ARTIKELREG SECTION.                                              
185000                                                                          
185100     IF ALLT-OK                                                           
185200       IF OBKR-IDARTNR-TILLK > +0                                         
185300         MOVE OBKR-IDARTNR-TILLK TO AREG-IDARTNR                          
185400       ELSE                                                               
185500         MOVE OBKR-IDARTNR       TO AREG-IDARTNR                          
185600       END-IF                                                             
185700                                                                          
185800       CALL W411AREG USING AREG-W411AREG                                  
185900                           AREG-WDK6-PCB                                  
186000                           AREG-WDK7-PCB                                  
186100     END-IF                                                               
186200     .                                                                    
186300     EJECT                                                                
186400                                                                          
186500 S22-HAMTA-BENAMNING SECTION.                                             
186600                                                                          
186700     IF OBKR-IDARTNR-TILLK > +0                                           
186800       MOVE OBKR-IDARTNR-TILLK                                            
186900                              TO W-IDARTNR                                
187000     ELSE                                                                 
187100       MOVE OBKR-IDARTNR      TO W-IDARTNR                                
187200     END-IF                                                               
187300                                                                          
187400     MOVE PHUV-IDSKYLT         TO W-IDSKYLT                               
187500                                                                          
187600     PERFORM IMS-GU-BENA-WDD311                                           
187700     IF SEGMENT-FINNS                                                     
187800       MOVE TEXT-BEART        TO MOD-BEART(WS-INDEX-MOD)                  
187900     ELSE                                                                 
188000       MOVE SPACE             TO MOD-BEART(WS-INDEX-MOD)                  
188100     END-IF                                                               
188200     .                                                                    
188300     EJECT                                                                
188400                                                                          
188500                                                                          
188600 MFS-ROER-EJ-BILD SECTION.                                                
188700                                                                          
188800     MOVE MFS-ROER-EJ-FAELT    TO MOD-KDORDKL-UT                          
188900                                  MOD-KDFRAKT-UT                          
189000                                  MOD-KDPROTYP-UT                         
189100                                                                          
189200     MOVE +1                   TO WS-INDEX                                
189300     PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                            
189400                                                                          
189500        PERFORM MFS-SAETT-ATTRIBUT                                        
189600                                                                          
189700        MOVE MFS-ROER-EJ-FAELT TO MOD-KDORDBEK(WS-INDEX)                  
189800                                  MOD-ASTERIX(WS-INDEX)                   
189900                                  MOD-KDBEHX(WS-INDEX)                    
190000                                  MOD-IDARTNR(WS-INDEX)                   
190100                                  MOD-BEART(WS-INDEX)                     
190200                                  MOD-KVBEART(WS-INDEX)                   
190300                                  MOD-KVQPACK(WS-INDEX)                   
190400                                  MOD-NYCKLAR(WS-INDEX)                   
190500        ADD +1                 TO WS-INDEX                                
190600     END-PERFORM                                                          
190700     .                                                                    
190800     EJECT                                                                
190900 MFS-SAETT-ATTRIBUT SECTION.                                              
191000                                                                          
191100     MOVE MID-RAD(WS-INDEX)    TO WS-AKTUELL-MID-RAD                      
191200                                                                          
191300     IF WS-AKT-KDBEHX = 'B'                                               
191400       IF (WS-AKT-KDORDBEK = 41 OR 61) AND                                
191500                     (WS-AKT-IDARTNR NOT = WS-AKT-IDARTNR-URS)            
191600          MOVE MFS-STAENG-FAELT-OSYNLIGT                                  
191700                             TO MOD-KDORDBEK-ATTR(WS-INDEX)               
191800       ELSE                                                               
191900         IF WS-AKT-KDORDBEK = 21  OR 51  OR                               
192000                              41  OR 61  OR                               
192100                              52  OR 53  OR 54  OR 55  OR                 
192200                              57  OR 58  OR 59  OR 66  OR                 
192300                              67  OR 72  OR 73  OR 74  OR                 
192400                              75  OR 76  OR 80  OR 81  OR                 
192500                              82  OR 85  OR 98                            
192600           MOVE MFS-STAENG-FAELT TO MOD-KDBEHX-ATTR(WS-INDEX)             
192700         END-IF                                                           
192800                                                                          
192900         IF WS-AKT-KDORDBEK = 41 OR 61                                    
193000           MOVE MFS-STAENG-FAELT-OSYNLIGT TO                              
193100                                MOD-KVBEART-ATTR(WS-INDEX)                
193200                                MOD-KVQPACK-ATTR(WS-INDEX)                
193300         END-IF                                                           
193400         IF WS-AKT-KDORDBEK = 57  AND  WS-AKT-IDARTNR = ZERO              
193500            MOVE MFS-STAENG-FAELT-OSYNLIGT                                
193600                             TO MOD-KDORDBEK-ATTR(WS-INDEX)               
193700                                MOD-KVBEART-ATTR(WS-INDEX)                
193800                                MOD-KVQPACK-ATTR(WS-INDEX)                
193900         END-IF                                                           
194000       END-IF                                                             
194100     ELSE                                                                 
194110       IF WS-AKT-KDORDBEK = ALL '+'                                       
194120          CONTINUE                                                        
194130       ELSE                                                               
194200         IF WS-AKT-KDORDBEK = 41 OR 61                                    
194300           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
194400                             TO MOD-KDORDBEK-ATTR(WS-INDEX)               
194500           IF (WS-AKT-KDORDBEK = 61 ) AND                                 
194600                    WS-AKT-IDARTNR = ZERO                                 
194700             MOVE MFS-STAENG-FAELT-OSYNLIGT                               
194800                             TO MOD-KDBEHX-ATTR(WS-INDEX)                 
194900                                MOD-KVBEART-ATTR(WS-INDEX)                
195000                                MOD-KVQPACK-ATTR(WS-INDEX)                
195100           END-IF                                                         
195200         END-IF                                                           
195210       END-IF                                                             
195300     END-IF                                                               
195400     .                                                                    
195500     EJECT                                                                
195600                                                                          
195700 MFS-RENSA-MOD-RADER SECTION.                                             
195800                                                                          
195900     MOVE +1                 TO WS-INDEX                                  
196000     PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                            
196100        MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(WS-INDEX)                    
196200                                MOD-ASTERIX(WS-INDEX)                     
196300                                MOD-KDBEHX(WS-INDEX)                      
196400                                MOD-IDARTNR(WS-INDEX)                     
196500                                MOD-BEART(WS-INDEX)                       
196600                                MOD-KVBEART(WS-INDEX)                     
196700                                MOD-KVQPACK(WS-INDEX)                     
196800                                MOD-NYCKLAR(WS-INDEX)                     
196900        ADD 1                TO WS-INDEX                                  
197000     END-PERFORM                                                          
197100     .                                                                    
197200                                                                          
197300 MFS-RENSA-ALLA-FAELT SECTION.                                            
197400                                                                          
197500                                                                          
197600     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-NEXT                          
197700                                MOD-IDLOPNR-NEXT                          
197800                                MOD-IDSEKVNR-NEXT                         
197900                                MOD-KDORDBEK-NEXT                         
198000                                MOD-KDBEHX-NEXT                           
198100                                                                          
198200     PERFORM MFS-RENSA-MOD-RADER                                          
198300     .                                                                    
198400     EJECT                                                                
198500* --- IMS SEKTIONER ---                                                   
198600                                                                          
198700 IMS-GET-MSG SECTION.                                                     
198800                                                                          
198900     MOVE '  QC' TO GODK-STATUSKODER                                      
199000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
199100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
199200     PERFORM IMS-STATUSKONTROLL                                           
199300     .                                                                    
199400     SKIP3                                                                
199500 IMS-GN-MSG SECTION.                                                      
199600                                                                          
199700     MOVE '  QD'   TO GODK-STATUSKODER                                    
199800     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
199900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
200000     PERFORM IMS-STATUSKONTROLL                                           
200100     .                                                                    
200200     EJECT                                                                
200300 IMS-INSERT-DISP-MSG SECTION.                                             
200400                                                                          
200500     MOVE '  '  TO GODK-STATUSKODER                                       
200600     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
200700     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
200800     PERFORM IMS-STATUSKONTROLL                                           
200900     .                                                                    
201000     SKIP3                                                                
201100 IMS-INSERT-MSG SECTION.                                                  
201200                                                                          
201300     IF ENGLISH-TEXT                                                      
201400       MOVE 'N' TO MFS-KDHUVOMR                                           
201500     END-IF                                                               
201600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
201700     MOVE SPACE TO GODK-STATUSKODER                                       
201800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
201900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
202000     PERFORM IMS-STATUSKONTROLL                                           
202100     .                                                                    
202200     EJECT                                                                
202300 IMS-INSERT-4262-MSG SECTION.                                             
202400                                                                          
202500     IF ENGLISH-TEXT                                                      
202600       MOVE 'N' TO MFS-KDHUVOMR                                           
202700     END-IF                                                               
202800     MOVE LOW-VALUE TO 4262-Z1 4262-Z2                                    
202900     MOVE SPACE TO GODK-STATUSKODER                                       
203000     CALL CBLTDLI USING ISRT 4262-PCB 4262-MSG-IO-AREA                    
203100     MOVE 4262-STATUS-CODE TO STATUS-WS                                   
203200     PERFORM IMS-STATUSKONTROLL                                           
203300     .                                                                    
203400     SKIP2                                                                
203500 IMS-INSERT-4263-MSG SECTION.                                             
203600                                                                          
203700     IF ENGLISH-TEXT                                                      
203800       MOVE 'N' TO MFS-KDHUVOMR                                           
203900     END-IF                                                               
204000     MOVE SPACE TO GODK-STATUSKODER                                       
204100     CALL CBLTDLI USING ISRT 4263-PCB 4263-MSG-IO-AREA                    
204200     MOVE 4263-STATUS-CODE TO STATUS-WS                                   
204300     PERFORM IMS-STATUSKONTROLL                                           
204400     .                                                                    
204500     EJECT                                                                
204600 IMS-GHU-PROC-WDE801 SECTION.                                             
204700                                                                          
204800     STRING 'WLPROC01(WDE801KY =' W-WDE801KY-X ')'                        
204900          DELIMITED BY SIZE INTO SSA1                                     
205000     MOVE '  GE'               TO GODK-STATUSKODER                        
205100     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-AREA-PROC SSA1                
205200     MOVE PROC-STATUS-CODE     TO STATUS-WS                               
205300     PERFORM IMS-STATUSKONTROLL                                           
205400     .                                                                    
205500     SKIP3                                                                
205600 IMS-REPL-PROC-WDE801 SECTION.                                            
205700                                                                          
205800     MOVE '    '             TO GODK-STATUSKODER                          
205900     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-AREA-PROC                    
206000     MOVE PROC-STATUS-CODE     TO STATUS-WS                               
206100     PERFORM IMS-STATUSKONTROLL                                           
206200     .                                                                    
206300     EJECT                                                                
206400 IMS-ISRT-PROD-WDE901 SECTION.                                            
206500                                                                          
206600     MOVE 'WLPROD01'         TO SSA1                                      
206700     MOVE '  II'             TO GODK-STATUSKODER                          
206800     CALL CBLTDLI USING ISRT PROD-PCB DLI-IO-AREA-PROD SSA1               
206900     MOVE PROD-STATUS-CODE     TO STATUS-WS                               
207000     PERFORM IMS-STATUSKONTROLL                                           
207100     .                                                                    
207200     EJECT                                                                
207300 IMS-GHU-ORQM-WDQ101-FOERE SECTION.                                       
207400                                                                          
207500     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN                        
207600                    '&WDQ101KY <' W-WDQ101-KEY-MAX                        
207700                    '&FLOBOK   =' NEJ ')'                                 
207800          DELIMITED BY SIZE INTO SSA1                                     
207900     MOVE '  GE'               TO GODK-STATUSKODER                        
208000     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
208100     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
208200     PERFORM IMS-STATUSKONTROLL                                           
208300     .                                                                    
208400     SKIP2                                                                
208500 IMS-GHN-ORQM-WDQ101-FOERE SECTION.                                       
208600                                                                          
208700     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN                        
208800                    '&WDQ101KY <' W-WDQ101-KEY-MAX                        
208900                    '&FLOBOK   =' NEJ ')'                                 
209000          DELIMITED BY SIZE INTO SSA1                                     
209100     MOVE '  GEGB'             TO GODK-STATUSKODER                        
209200     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
209300     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
209400     PERFORM IMS-STATUSKONTROLL                                           
209500     .                                                                    
209600     SKIP2                                                                
209700 IMS-GHU-ORQM-WDQ101-MIN-MAX SECTION.                                     
209800                                                                          
209900     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
210000                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
210100                    '&FLOBOK   =' NEJ ')'                                 
210200          DELIMITED BY SIZE INTO SSA1                                     
210300     MOVE '  GE'               TO GODK-STATUSKODER                        
210400     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
210500     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
210600     PERFORM IMS-STATUSKONTROLL                                           
210700     .                                                                    
210800     EJECT                                                                
210900 IMS-GHN-ORQM-WDQ101-MIN-MAX SECTION.                                     
211000                                                                          
211100     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
211200                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
211300                    '&FLOBOK   =' NEJ ')'                                 
211400          DELIMITED BY SIZE INTO SSA1                                     
211500     MOVE '  GEGB'             TO GODK-STATUSKODER                        
211600     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
211700     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
211800     PERFORM IMS-STATUSKONTROLL                                           
211900     .                                                                    
212000     SKIP2                                                                
212100 IMS-GHU-ORQM-WDQ101-UNIK SECTION.                                        
212200                                                                          
212300     STRING 'WLORQM01(WDQ101KY =' W-WDQ101-KEY-UNIK                       
212400                    '&FLOBOK   =' NEJ ')'                                 
212500          DELIMITED BY SIZE  INTO SSA1                                    
212600     MOVE '  GE'               TO GODK-STATUSKODER                        
212700     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
212800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
212900     PERFORM IMS-STATUSKONTROLL                                           
213000     .                                                                    
213100     SKIP2                                                                
213200 IMS-REPL-ORQM-WDQ101 SECTION.                                            
213300                                                                          
213400     MOVE '    '               TO GODK-STATUSKODER                        
213500     CALL CBLTDLI USING REPL ORQM-PCB DLI-IO-AREA-ORQM                    
213600     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
213700     PERFORM IMS-STATUSKONTROLL                                           
213800     .                                                                    
213900     EJECT                                                                
214000 IMS-ISRT-ORQM-WDQ101 SECTION.                                            
214100                                                                          
214200     MOVE 'WLORQM01 '          TO SSA1                                    
214300     MOVE '  '                 TO GODK-STATUSKODER                        
214400     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-ORQM SSA1               
214500     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
214600     PERFORM IMS-STATUSKONTROLL                                           
214700     .                                                                    
214800     SKIP3                                                                
214900 IMS-DLET-ORQM-WDQ101 SECTION.                                            
215000                                                                          
215100     MOVE '    '               TO GODK-STATUSKODER                        
215200     CALL CBLTDLI USING DLET ORQM-PCB DLI-IO-AREA-ORQM                    
215300     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
215400     PERFORM IMS-STATUSKONTROLL                                           
215500     .                                                                    
215600     EJECT                                                                
215700 IMS-GU-ORQM-WDQ101 SECTION.                                              
215800                                                                          
215900     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X2                       
216000                    '&WDQ101KY <' W-WDQ101KY-MAX-X2 ')'                   
216100          DELIMITED BY SIZE INTO SSA1                                     
216200     MOVE '  '             TO GODK-STATUSKODER                            
216300     MOVE '    '           TO GODK-STATUSKODER                            
216400     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-AREA-ORQM SSA1               
216500     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
216600     PERFORM IMS-STATUSKONTROLL                                           
216700     .                                                                    
216800     SKIP2                                                                
216900 IMS-GN-ORQM-WDQ101 SECTION.                                              
217000                                                                          
217100     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X2                       
217200                    '&WDQ101KY <' W-WDQ101KY-MAX-X2 ')'                   
217300          DELIMITED BY SIZE INTO SSA1                                     
217400     MOVE '  GEGB'             TO GODK-STATUSKODER                        
217500     CALL CBLTDLI USING GN   ORQM-PCB DLI-IO-AREA-ORQM SSA1               
217600     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
217700     PERFORM IMS-STATUSKONTROLL                                           
217800     .                                                                    
217900     EJECT                                                                
218000 IMS-GU-SATB-WDJ111-01 SECTION.                                           
218100                                                                          
218200     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
218300          DELIMITED BY SIZE INTO SSA1                                     
218400     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
218500          DELIMITED BY SIZE INTO SSA2                                     
218600     MOVE '  GE'               TO GODK-STATUSKODER                        
218700     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA-SATB SSA1 SSA2            
218800     MOVE SATB-STATUS-CODE     TO STATUS-WS                               
218900     PERFORM IMS-STATUSKONTROLL                                           
219000     .                                                                    
219100     SKIP2                                                                
219200 IMS-GN-SATB-WDJ111-01 SECTION.                                           
219300                                                                          
219400     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
219500          DELIMITED BY SIZE INTO SSA1                                     
219600     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
219700          DELIMITED BY SIZE INTO SSA2                                     
219800     MOVE '  GE'               TO GODK-STATUSKODER                        
219900     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA-SATB SSA1 SSA2            
220000     MOVE SATB-STATUS-CODE     TO STATUS-WS                               
220100     PERFORM IMS-STATUSKONTROLL                                           
220200     .                                                                    
220300     EJECT                                                                
220400 IMS-GHU-ARTM-WDK901 SECTION.                                             
220500                                                                          
220600     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
220700          DELIMITED BY SIZE  INTO SSA1                                    
220800     MOVE '    '               TO GODK-STATUSKODER                        
220900     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ARTM SSA1                
221000     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
221100     PERFORM IMS-STATUSKONTROLL                                           
221200     .                                                                    
221300     SKIP2                                                                
221400 IMS-REPL-ARTM-WDK901 SECTION.                                            
221500                                                                          
221600     MOVE '    '               TO GODK-STATUSKODER                        
221700     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ARTM                    
221800     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
221900     PERFORM IMS-STATUSKONTROLL                                           
222000     .                                                                    
222100     EJECT                                                                
222200 IMS-GU-BENA-WDD311 SECTION.                                              
222300                                                                          
222400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
222500          DELIMITED BY SIZE INTO SSA1                                     
222600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
222700          DELIMITED BY SIZE INTO SSA2                                     
222800     MOVE '  GE'               TO GODK-STATUSKODER                        
222900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA SSA1 SSA2            
223000     MOVE BENA-STATUS-CODE     TO STATUS-WS                               
223100     PERFORM IMS-STATUSKONTROLL                                           
223200     .                                                                    
223300     SKIP2                                                                
223400 IMS-GU-XXBV-WDR210 SECTION.                                              
223500                                                                          
223600     STRING 'WLXXBV01(WDGXKEY  =' W-WDGX2225-X ')'                        
223700          DELIMITED BY SIZE INTO SSA1                                     
223800     STRING 'WLXXBV11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
223900          DELIMITED BY SIZE INTO SSA2                                     
224000     MOVE '  ' TO GODK-STATUSKODER                                        
224100     CALL CBLTDLI USING GU XXBV-PCB DLI-IO-AREA-2226 SSA1 SSA2            
224200     MOVE XXBV-STATUS-CODE TO STATUS-WS                                   
224300     PERFORM IMS-STATUSKONTROLL                                           
224400     .                                                                    
224500     EJECT                                                                
224600 IMS-GU-XXBX-WDR220 SECTION.                                              
224700                                                                          
224800     STRING 'WLXXBX01(WDGXKEY  =' W-WDGX2231-X ')'                        
224900          DELIMITED BY SIZE INTO SSA1                                     
225000     STRING 'WLXXBX11(WDGXKEY  =' W-WDGX2232-X ')'                        
225100          DELIMITED BY SIZE INTO SSA2                                     
225200     MOVE '  GE' TO GODK-STATUSKODER                                      
225300     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA-2232 SSA1 SSA2            
225400     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
225500     PERFORM IMS-STATUSKONTROLL                                           
225600     .                                                                    
225700     SKIP2                                                                
225800 IMS-ISRT-XXBU-WDR501 SECTION.                                            
225900                                                                          
226000     MOVE 'WLXXBU01 ' TO SSA1                                             
226100     MOVE '  II' TO GODK-STATUSKODER                                      
226200     CALL CBLTDLI USING ISRT XXBU-PCB DLI-IO-AREA-2223 SSA1               
226300     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
226400     PERFORM IMS-STATUSKONTROLL                                           
226500     .                                                                    
226600     SKIP2                                                                
226700 IMS-GHU-XXBU-WDR501 SECTION.                                             
226800                                                                          
226900     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
227000          DELIMITED BY SIZE INTO SSA1                                     
227100     MOVE '  ' TO GODK-STATUSKODER                                        
227200     CALL CBLTDLI USING GHU XXBU-PCB DLI-IO-AREA-2223 SSA1                
227300     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
227400     PERFORM IMS-STATUSKONTROLL                                           
227500     .                                                                    
227600     EJECT                                                                
227700 IMS-GNP-XXBU-WDR550 SECTION.                                             
227800                                                                          
227900     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
228000          DELIMITED BY SIZE INTO SSA1                                     
228100     STRING 'WLXXBU11(WDGXKEY  =' W-WDGX2224-X ')'                        
228200          DELIMITED BY SIZE INTO SSA2                                     
228300     MOVE '  GE' TO GODK-STATUSKODER                                      
228400     CALL CBLTDLI USING GNP XXBU-PCB DLI-IO-AREA-2224 SSA1 SSA2           
228500     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
228600     PERFORM IMS-STATUSKONTROLL                                           
228700     .                                                                    
228800     SKIP2                                                                
228900 IMS-ISRT-XXBU-WDR550 SECTION.                                            
229000                                                                          
229100     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
229200          DELIMITED BY SIZE INTO SSA1                                     
229300     STRING 'WLXXBU11 '                                                   
229400          DELIMITED BY SIZE INTO SSA2                                     
229500     MOVE '  II' TO GODK-STATUSKODER                                      
229600     CALL CBLTDLI USING ISRT XXBU-PCB DLI-IO-AREA-2224 SSA1 SSA2          
229700     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
229800     PERFORM IMS-STATUSKONTROLL                                           
229900     .                                                                    
230000     SKIP3                                                                
230100 IMS-STATUSKONTROLL SECTION.                                              
230200                                                                          
230300     SET STATUS-IX TO 1                                                   
230400     SEARCH GODK-STATUS                                                   
230500       AT END CALL FELLOG                                                 
230600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
230700     END-SEARCH                                                           
230800     .                                                                    
230900     EJECT                                                                
231000*    -COPY WY2000P1                                                       
231100*    -COPY WY2000Q1                                                       
