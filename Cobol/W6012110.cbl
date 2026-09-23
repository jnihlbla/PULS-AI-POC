000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6012110.                                                
000400*AUTHOR.         RAHUL REDDY.                                             
000500*DATE-WRITTEN.   12/07/23.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ALLMÄN BESKRIVNING:                                              
001100*        PROGRAMMET ÄR EN MPP SOM ANVÄNDS VID LOSSNING AV EN              
001200*        BIL.                                                             
001300*        DEN INNEHÅLLER INFORMATION OM VAD SOM FINNS PÅ                   
001400*        BILEN SAMT VART DET SKALL.                                       
001500*        ALLTEFTERSOM LOSSAREN LASTAR AV PARTIER FRÅN BILEN               
001600*        REGISTRERAR PERSONEN DET PÅ BILDEN, SAMT OM EV. FEL              
001700*        UPPTÄCKS.                                                        
001800*        DET KAN EX.VIS VARA FEL EMBALLAGE PÅ GODSET,                     
001900*        ELLER SÅ SAKNAS KANSKE PARTIER.                                  
002000*                                                                         
002100*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
002200*                              W6LASA (W6G2)                              
002300*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W6T121                                              
002700*        MID:         W6I12101                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W6O12101                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500                                                                          
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W6012110'.            
004100 77  WS-IDRADNR                  PIC S9(5)   COMP-3.                      
004200                                                                          
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  YES                         PIC X       VALUE 'Y'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 01  ALL-SPACE.                                                           
005000     03 FILLER                   PIC X(70)   VALUE SPACE.                 
005100 01  ALL-PLUS.                                                            
005200     03 FILLER                   PIC X(70)   VALUE ALL '+'.               
005300                                                                          
005400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005500 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
005600 77  IX-X                        PIC S9(4)  VALUE +0    COMP SYNC.        
005700 77  TIX                         PIC S9(4)  VALUE +0    COMP SYNC.        
005800 77  MAX-TIX                     PIC S9(4)  VALUE +0    COMP SYNC.        
005900 77  T91-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
006000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006100                                                                          
006200*    --- RÄKNARE                                                          
006300 77  FELRAKN                     PIC S9(2)  VALUE +0    COMP SYNC.        
006400 77  SAK-RAKN                    PIC S9(2)  VALUE +0    COMP SYNC.        
006500 77  COUNTER                     PIC S9(3)  VALUE +0    COMP SYNC.        
006600 77  WS-COUNT                    PIC S9(3)  VALUE +0    COMP SYNC.        
006700                                                                          
006800                                                                          
006900***********************************                                       
007000*    --- INTERN-TABELLER                                                  
007100***********************************                                       
007200 01  WT-TAB.                                                              
007300                                                                          
007400     03  WT-TABELL OCCURS 501.                                            
007500         05  WT-IDLEVNR           PIC X(5)  VALUE SPACE.                  
007600         05  WT-IDARTNR           PIC 9(9)  VALUE ZERO.                   
007700         05  WT-KVAVIS-TOT        PIC 9(7)  VALUE ZERO.                   
007800         05  WT-KVKOLLI           PIC 9(4)  VALUE ZERO.                   
007900         05  WT-KVAVIS            PIC 9(7)  VALUE ZERO.                   
008000         05  WT-KVAVIS-PRIO       PIC 9(7)  VALUE ZERO.                   
008100         05  WT-KVAVIS-KIT        PIC 9(7)  VALUE ZERO.                   
008200         05  WT-TIAVIDAT          PIC 9(7)  VALUE ZERO.                   
008300         05  WT-BEFT              PIC 9(2)  VALUE ZERO.                   
008400                                                                          
008500         05  WT-IDFS              PIC X(8)         VALUE SPACE.           
008600         05  WT-IDARTNR-LYS       PIC X(1)         VALUE SPACE.           
008700         05  WT-KDLAGEMB          PIC X(4)         VALUE SPACE.           
008800         05  WT-ADINLOMR-NXT      PIC X(4)         VALUE SPACE.           
008900         05  WT-ADTRDEST-KIT      PIC X(3)         VALUE SPACE.           
009000                                                                          
009100 01  WT-TAB-LYS.                                                          
009200                                                                          
009300     03  WT-TABELL-LYS OCCURS 500.                                        
009400         05  WT-LYS-ATGKOD        PIC X(3)  VALUE SPACE.                  
009500         05  WT-LYS-IDLEVNR       PIC X(5)  VALUE SPACE.                  
009600         05  WT-LYS-IDARTNR       PIC 9(9)  VALUE ZERO.                   
009700         05  WT-LYS-IDFS          PIC X(8)  VALUE SPACE.                  
009800                                                                          
009900***********************************                                       
010000*        ARBETSFÄLT *                                                     
010100***********************************                                       
010200 01  WS-AREA.                                                             
010300*                                                                         
010400*REDIGERING (NUM-ALFA, ETC.)                                              
010500     03  W-IDLOPNRM               PIC 9(9).                               
010600     03  W-IDLOPNRM-N REDEFINES W-IDLOPNRM.                               
010700         05  FILLER               PIC 9(1).                               
010800         05  W-RED-IDLOPNRM       PIC 9(8).                               
010900                                                                          
011000     03  WS-IDLEVNR               PIC X(5).                               
011100     03  WS-IDLEVNR-X REDEFINES WS-IDLEVNR.                               
011200         05  FILLER               PIC X(5).                               
011300                                                                          
011400     03  WS-IDARTNR-N             PIC 9(9).                               
011500     03  WS-IDARTNR-X REDEFINES WS-IDARTNR-N.                             
011600         05  FILLER               PIC X(1).                               
011700         05  WS-RED-IDARTNR       PIC X(8).                               
011800                                                                          
011900     03  WS-KVKOLLI               PIC 9(4).                               
012000     03  WS-KVKOLLI-X REDEFINES WS-KVKOLLI.                               
012100         05  FILLER               PIC X(4).                               
012200                                                                          
012300     03  WS-KVAVIS-N              PIC 9(7).                               
012400     03  WS-KVAVIS-X REDEFINES WS-KVAVIS-N.                               
012500         05  FILLER               PIC X(1).                               
012600         05  WS-RED-KVAVIS        PIC X(6).                               
012700                                                                          
012800     03  WS-KVAVIS-PRIO-N         PIC 9(7).                               
012900     03  WS-KVAVIS-PRIO-X REDEFINES WS-KVAVIS-PRIO-N.                     
013000         05  FILLER               PIC X(1).                               
013100         05  WS-RED-KVAVIS-PRIO   PIC X(6).                               
013200                                                                          
013300     03  WS-KVAVIS-KIT-N          PIC 9(7).                               
013400     03  WS-KVAVIS-KIT-X REDEFINES WS-KVAVIS-KIT-N.                       
013500         05  FILLER               PIC X(1).                               
013600         05  WS-RED-KVAVIS-KIT    PIC X(6).                               
013700                                                                          
013800     03  WS-KVAVIS-TOT-N          PIC 9(7).                               
013900     03  WS-KVAVIS-TOT-X REDEFINES WS-KVAVIS-TOT-N.                       
014000         05  FILLER               PIC X(1).                               
014100         05  WS-RED-KVAVIS-TOT    PIC X(6).                               
014200                                                                          
014300*ACKAR                                                                    
014400     03  W-ACK-FLKLAR             PIC S9(3)  COMP-3 VALUE ZERO.           
014500     03  W-ACK-KVKOLLI            PIC S9(7)  COMP-3 VALUE ZERO.           
014600     03  W-ACK-SEK-PRIM           PIC S9(7)  COMP-3 VALUE ZERO.           
014700                                                                          
014800*NOTERING                                                                 
014900     03  WS-TELOSSN.                                                      
015000         05  WS-TELOSSN1          PIC X(39).                              
015100         05  WS-TELOSSN2          PIC X(66).                              
015200                                                                          
015300*PRINTER                                                                  
015400     03  WS-IDPRT.                                                        
015500         05  WS-IDPRT1            PIC X(2).                               
015600         05  WS-IDPRT2            PIC X(4).                               
015700         05  WS-IDPRT3            PIC X(2).                               
015800                                                                          
015900*PLAA                                                                     
016000     03  SPAR-ADLAGOMR-N          PIC 9(3).                               
016100     03  SPAR-ADLAGOMR REDEFINES SPAR-ADLAGOMR-N.                         
016200         05  FILLER               PIC X(1).                               
016300         05  SPAR-RED-ADLAGOMR    PIC X(2).                               
016400                                                                          
016500     03  SPAR-KDINLOMR            PIC X(4).                               
016600*WT-MOD                                                                   
016700     03  SPAR-KVAVIS-TOT          PIC 9(7)            VALUE ZERO.         
016800     03  SPAR-KDLAGEMB            PIC X(4)            VALUE SPACE.        
016900     03  SPAR-KDFARLIG            PIC S9(1) COMP-3    VALUE ZERO.         
017000     03  SPAR-FLKVAKAR            PIC X(1)            VALUE SPACE.        
017100     03  SPAR-BEFT                PIC 9(2)            VALUE ZERO.         
017200     03  SPAR-ADTRDEST-KIT        PIC X(4)            VALUE SPACE.        
017300                                                                          
017400*JMF - FÖR ATT KUNNA LÄGGA UT FÄLT OSYNLIGT TILL MODEN                    
017500     03  SPAR-IDARTNR             PIC X(8)            VALUE SPACE.        
017600     03  SPAR-IDLEVNR             PIC X(5)            VALUE SPACE.        
017700     03  SPAR-IDFS                PIC X(12)           VALUE SPACE.        
017800                                                                          
017900*JMF - FÖR ATT KUNNA KOLLA BRYTNING PÅ PARTI                              
018000     03  SPAR-P-IDARTNR           PIC 9(9)            VALUE ZERO.         
018100     03  SPAR-P-IDLEVNR           PIC X(5)            VALUE SPACE.        
018200     03  SPAR-P-IDFS              PIC X(12)           VALUE SPACE.        
018300     03  SPAR-P-TIAVIDAT          PIC 9(6)            VALUE ZERO.         
018400                                                                          
018500*T91                                                                      
018600     03  SPAR-ADINLOMR-NXT-OLD    PIC X(4) VALUE SPACE.                   
018700     03  SPAR-ADINLOMR-OLD        PIC X(4) VALUE SPACE.                   
018800     03  SPAR-KDINLSTA-OLD        PIC X(3)            VALUE SPACE.        
018900     03  SPAR-IDLOPNRM            PIC S9(9)   COMP-3  VALUE ZERO.         
019000     03  SPAR-PRARTSTD            PIC 9(7)V9(2)       VALUE ZERO.         
019100*                                                                         
019200     03  W-PTOP1-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
019300*    03  W-PTOP2-OCC-LL          PIC S9(4)    COMP-3 VALUE ZERO.          
019400                                                                          
019500*    --- ARBETSFÄLT FÖR SWITCHAR                                          
019600                                                                          
019700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
019800     88  INDATA-OK                           VALUE 'J'.                   
019900     88  INDATA-FEL                          VALUE 'N'.                   
020000                                                                          
020100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
020200     88  NYCKLAR-OK                          VALUE 'J'.                   
020300     88  NYCKLAR-FEL                         VALUE 'N'.                   
020400                                                                          
020500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
020600     88  ALLT-OK                             VALUE 'J'.                   
020700     88  ALLT-NEJ                            VALUE 'N'.                   
020800                                                                          
020900 77  PLACERINGS-SW               PIC X       VALUE 'N'.                   
021000     88  PLAC-AENDRING                       VALUE 'J'.                   
021100                                                                          
021200 77  SAMMA-SIDA-SW               PIC X       VALUE 'N'.                   
021300     88  SAMMA-SIDA-JA                       VALUE 'J'.                   
021400     88  SAMMA-SIDA-NEJ                      VALUE 'N'.                   
021500                                                                          
021600 77  SEK-PRIM-SW                 PIC X       VALUE 'N'.                   
021700     88  SEK-PRIM-JA                         VALUE 'J'.                   
021800     88  SEK-PRIM-NEJ                        VALUE 'N'.                   
021900                                                                          
022000 77  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
022100     88  ARTIKEL-FINNS                       VALUE 'J'.                   
022200     88  ARTIKEL-SAKNAS                      VALUE 'N'.                   
022300                                                                          
022400 77  KARANTAEN-SW                PIC X       VALUE 'N'.                   
022500     88  KARANTAEN                           VALUE 'J'.                   
022600                                                                          
022700 77  TOT-FLKLAR-SW               PIC X       VALUE 'N'.                   
022800     88  TOT-FLKLAR-JA                       VALUE 'J'.                   
022900     88  TOT-FLKLAR-NEJ                      VALUE 'N'.                   
023000                                                                          
023100 77  FOERSTA-T91-SW              PIC X       VALUE 'N'.                   
023200     88  FOERSTA-T91-JA                      VALUE 'J'.                   
023300     88  FOERSTA-T91-NEJ                     VALUE 'N'.                   
023400                                                                          
023500 77  SAKNAT-SW                   PIC X       VALUE 'N'.                   
023600     88  SAKNAT-PARTI                        VALUE 'J'.                   
023700                                                                          
023800 77  LASTB-SW                    PIC X       VALUE 'N'.                   
023900     88  LASTB-BORTTAGEN                     VALUE 'J'.                   
024000                                                                          
024100 77  T91-RAD-SW                  PIC X       VALUE 'N'.                   
024200     88  T91-RAD-JA                          VALUE 'J'.                   
024300     88  T91-RAD-NEJ                         VALUE 'N'.                   
024400                                                                          
024500 77  T93-RAD-SW                  PIC X       VALUE 'N'.                   
024600     88  T93-RAD-JA                          VALUE 'J'.                   
024700     88  T93-RAD-NEJ                         VALUE 'N'.                   
024800                                                                          
024900 77  TRANS91-SW                  PIC X       VALUE 'N'.                   
025000     88  TRANS91-JA                          VALUE 'J'.                   
025100     88  TRANS91-NEJ                         VALUE 'N'.                   
025200                                                                          
025300 77  TRANS93-SW                  PIC X       VALUE 'N'.                   
025400     88  TRANS93-JA                          VALUE 'J'.                   
025500     88  TRANS93-NEJ                         VALUE 'N'.                   
025600                                                                          
025700*----------------------------------------------------------------*        
025800*   NKLTYP1=LASTBÄRARE,    NKLTYP2=LASTBÄRARE-ARTNR                       
025900*   NKLTYP3=LASTBÄRARE-ARTNR-LEVNR-FS                                     
026000*----------------------------------------------------------------*        
026100 77  NKLTYP-SW                  PIC X.                                    
026200     88  NKLTYP1                            VALUE '1'.                    
026300     88  NKLTYP2                            VALUE '2'.                    
026400     88  NKLTYP3                            VALUE '3'.                    
026500                                                                          
026600                                                                          
026700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
026800 01  GENERELLA-SUBPROGRAM.                                                
026900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
027000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
027100     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
027200     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
027300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
027400     03  W6012111                PIC X(8)    VALUE 'W6012111'.            
027500     03  W6012112                PIC X(8)    VALUE 'W6012112'.            
027600     EJECT                                                                
027700                                                                          
027800*01 -COPY WMSGINIT                                                        
027900     SKIP3                                                                
028000 01  MESSAGE-CODES.                                                       
028100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
028200     03  ERR-CONFLICT            PIC X(3)    VALUE '046'.                 
028300     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
028400     03  INF-PRESS-PF4           PIC X(3)    VALUE '379'.                 
028500     03  ERR-UPDATE-NOT-VALID    PIC X(3)    VALUE '007'.                 
028600     03  ERR-SAKN-I-REG          PIC X(3)    VALUE '025'.                 
028700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
028800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.                 
028900     03  INF-LAST-PAGE           PIC X(3)    VALUE '012'.                 
029000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
029100     03  INF-FARLIGT-GODS        PIC X(3)    VALUE '362'.                 
029200     03  INF-KARANTAEN           PIC X(3)    VALUE '355'.                 
029300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
029400     03  ERR-CODE-NOT-VALID      PIC X(3)    VALUE '023'.                 
029500     03  ERR-PRINT-SAKN          PIC X(3)    VALUE '347'.                 
029600     03  ERR-WRONG-PLACE         PIC X(3)    VALUE '385'.                 
029700     03  ERR-PLACE-MISSING       PIC X(3)    VALUE '041'.                 
029800     EJECT                                                                
029900                                                                          
030000*    --- AREOR FÖR BAKGRUNDS MPP:ER / BMP:ER                              
030100*    --- COPYTEXTER FÖR W006PRT, W611STYR, W6012111,W6012112              
030200*    ---                         SP1-6110-W6GX6110                        
030300*    ---                         SP2-6110-W6GX6110                        
030400*    ---                         W60191, W60193                           
030500*                                                                         
030600*01  -COPY W006PRT                                                        
030700     EJECT                                                                
030800                                                                          
030900*01  -COPY W611STYR                                                       
031000     EJECT                                                                
031100                                                                          
031200*01  -COPY W6012111     -PRE W6012111-                                    
031300     EJECT                                                                
031400                                                                          
031500*01  -COPY W6012112     -PRE W6012112-                                    
031600     EJECT                                                                
031700                                                                          
031800*01  -COPY W6GX6110       -PRE SP1-                                       
031900     EJECT                                                                
032000                                                                          
032100*01  -COPY W6GX6110       -PRE SP2-                                       
032200     EJECT                                                                
032300                                                                          
032400 01  P-TO-P-T91.                                                          
032500*----TILL W60191                                                          
032600     03  PTOP1-LL              PIC S9(4)   COMP SYNC.                     
032700     03  PTOP1-Z1              PIC X(1)    VALUE LOW-VALUE.               
032800     03  PTOP1-Z2              PIC X(1)    VALUE LOW-VALUE.               
032900     03  PTOP1-TRANSKOD        PIC X(7)    VALUE 'W6T191X'.               
033000     03  FILLER                PIC X(1)    VALUE SPACE.                   
033100     03  PTOP1-IDTRANS         PIC X(4)    VALUE '6121'.                  
033200     03  PTOP1-KDMFSFOR        PIC X(1).                                  
033300*    03  MID -COPY W6I19101       -PRE T91-                               
033400     EJECT                                                                
033500*                                                                         
033600 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
033700     SKIP3                                                                
033800*01  -COPY WMSGKOM                                                        
033900     EJECT                                                                
034000 01  FILLER                      PIC X(16)  VALUE 'MSG/KOM-AREA'.         
034100     SKIP3                                                                
034200*01  -COPY WMSGAREA   -PRE  K                                             
034300     EJECT                                                                
034400*    05  MOD -COPY W6I19301 -PRE T93-   -RED KMSG-MID-OUT.                
034500     EJECT                                                                
034600                                                                          
034700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
034800*                                                                         
034900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
035000     SKIP3                                                                
035100*01  -COPY WMFSAREA                                                       
035200     EJECT                                                                
035300                                                                          
035400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
035500*                                                                         
035600     EJECT                                                                
035700                                                                          
035800*                                                                         
035900*    --- DLI- NYCKLAR TILL IMS-SEKTIONERNA                                
036000*                                                                         
036100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
036200     SKIP3                                                                
036300 01  NYCKLAR-TILL-DLI.                                                    
036400*--------FYSISK NKL TILL INLA                                             
036500     03  W-W6D101KY-X.                                                    
036600         05  W-IDDC          PIC X(2)     VALUE SPACE.                    
036700         05  W-IDLEVNR       PIC  X(5)    VALUE SPACE.                    
036800         05  W-IDFS          PIC X(8)     VALUE SPACE.                    
036900         05  W-TIAVIDAT      PIC S9(7)    VALUE ZERO COMP-3.              
037000                                                                          
037100     03  W-IDRADNR-INL-X.                                                 
037200         05  W-IDRADNR-INL       PIC S9(5)  VALUE ZERO COMP-3.            
037300                                                                          
037400     03  W-IDRADNR-X.                                                     
037500         05  W-IDRADNR           PIC S9(5)   COMP-3.                      
037600                                                                          
037700*--------ALT NKL TILL INLA                                                
037800     03  W-IDARTNR-X.                                                     
037900         05  W-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
038000                                                                          
038100     03  W-IDRADNR-ALT-X.                                                 
038200         05  W-IDRADNR-ALT       PIC S9(5)   COMP-3.                      
038300                                                                          
038400*--------FYSISK NKL TILL LASA                                             
038500     03  WL-W6GX01KY-X.                                                   
038600         05  WL-IDHTYP           PIC X(4)    VALUE SPACE.                 
038700         05  WL-IDDC             PIC X(2)    VALUE SPACE.                 
038800         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
038900                                                                          
039000     03  WL-W6GX11KY-X.                                                   
039100         05  WL-IDLBBET          PIC X(12)   VALUE SPACE.                 
039200         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
039300                                                                          
039400     03  WL-W6GX21KY-X.                                                   
039500         05  WL-IDLEVNR        PIC  X(5)   VALUE SPACE.                   
039600         05  WL-IDFS           PIC X(8).                                  
039700         05  WL-TIAVIDAT       PIC S9(7)   COMP-3.                        
039800         05  WL-IDARTNR        PIC S9(9)   COMP-3.                        
039900         05  WL-KDSORT1        PIC S9      COMP-3.                        
040000                                                                          
040100*--------SÖK NKL 1 TILL LASA-G121                                         
040200     03  WL-W6GX21KY-SOEK1.                                               
040300         05  WLS1-IDARTNR        PIC S9(9)   COMP-3.                      
040400                                                                          
040500*--------SÖK NKL 2 TILL LASA-G121                                         
040600     03  WLS2-IDLEVNR-X.                                                  
040700         05  WLS2-IDLEVNR        PIC  X(5)   VALUE SPACE.                 
040800                                                                          
040900     03  WLS2-IDFS-X.                                                     
041000         05  WLS2-IDFS           PIC X(8).                                
041100                                                                          
041200     03  WLS2-IDARTNR-X.                                                  
041300         05  WLS2-IDARTNR        PIC S9(9)   COMP-3.                      
041400                                                                          
041500     03  WLS2-TIAVIDAT-X.                                                 
041600         05  WLS2-TIAVIDAT       PIC S9(7)   COMP-3.                      
041700                                                                          
041800*--------SÖK NKL 3 TILL LASA-G121                                         
041900     03  WLS3-IDARTNR-X.                                                  
042000         05  WLS3-IDARTNR        PIC S9(9)   COMP-3.                      
042100                                                                          
042200*--------FYSISK NKL TILL PLAA                                             
042300     03  W-W6GX01KY-X.                                                    
042400         05  WGX-IDHTYP          PIC X(4)    VALUE SPACE.                 
042500         05  WGX-IDDC            PIC X(2)    VALUE SPACE.                 
042600         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
042700                                                                          
042800     03  W-W6GX11KY-X.                                                    
042900         05  W-ADINLOMR          PIC X(4)    VALUE SPACE.                 
043000         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
043100                                                                          
043200     03  W-IDDC-X.                                                        
043300         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
043400     03  W-IDARTNR-K7-X.                                                  
043500         05  W-IDARTNR-K7        PIC S9(9)   VALUE ZERO COMP-3.           
043600     SKIP2                                                                
043700                                                                          
043800*    --- STATUS-KOD FRÅN IMS                                              
043900 01  STATUS-WS                   PIC XX.                                  
044000     88  SEGMENT-FINNS                       VALUE '  '.                  
044100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
044200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
044300                                                                          
044400 01  SAVE-STATUS-WS              PIC XX.                                  
044500     88  SAVE-SEGMENT-FINNS                  VALUE '  '.                  
044600     88  SAVE-SEGMENT-SAKNAS                 VALUE 'GE'.                  
044700     SKIP2                                                                
044800                                                                          
044900 01  GODK-STATUSKODER.                                                    
045000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
045100     SKIP3                                                                
045200                                                                          
045300 01  SSA1                        PIC X(90).                               
045400 01  SSA2                        PIC X(64).                               
045500 01  SSA3                        PIC X(64).                               
045600     EJECT                                                                
045700                                                                          
045800*    --- IMS FUNKTIONSKODER                                               
045900*01  -COPY W0003                                                          
046000     EJECT                                                                
046100                                                                          
046200*    ---  DLI INPUT-OUTPUT AREA                                           
046300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
046400     SKIP3                                                                
046500                                                                          
046600 01  DLI-IO-AREA-1.                                                       
046700     03  IO-AREA-1               PIC X(150)  VALUE SPACE.                 
046800     SKIP3                                                                
046900                                                                          
047000     03  W6INLA01 REDEFINES IO-AREA-1.                                    
047100*        05  -COPY W6D101                                                 
047200     EJECT                                                                
047300                                                                          
047400     03  W6INLA11 REDEFINES IO-AREA-1.                                    
047500*        05  -COPY W6D111                                                 
047600     EJECT                                                                
047700                                                                          
047800     03  W6INLA21 REDEFINES IO-AREA-1.                                    
047900*        05  -COPY W6D121                                                 
048000     EJECT                                                                
048100                                                                          
048200 01  DLI-IO-AREA-2A.                                                      
048300     03  IO-AREA-2A              PIC X(150)  VALUE SPACE.                 
048400     SKIP3                                                                
048500                                                                          
048600     03  W6LASA01 REDEFINES IO-AREA-2A.                                   
048700*        05  -COPY W6GX01      -PRE A-                                    
048800     EJECT                                                                
048900                                                                          
049000     03  W6LASA11 REDEFINES IO-AREA-2A.                                   
049100*        05  -COPY W6GX6108    -PRE A-                                    
049200     EJECT                                                                
049300                                                                          
049400     03  W6LASA21 REDEFINES IO-AREA-2A.                                   
049500*        05  -COPY W6GX6110    -PRE A-                                    
049600     EJECT                                                                
049700                                                                          
049800 01  DLI-IO-AREA-2B.                                                      
049900     03  IO-AREA-2B              PIC X(150)  VALUE SPACE.                 
050000     SKIP3                                                                
050100                                                                          
050200     03  W6LASA01 REDEFINES IO-AREA-2B.                                   
050300*        05  -COPY W6GX01      -PRE B-                                    
050400     EJECT                                                                
050500                                                                          
050600     03  W6LASA11 REDEFINES IO-AREA-2B.                                   
050700*        05  -COPY W6GX6108    -PRE B-                                    
050800     EJECT                                                                
050900                                                                          
051000     03  W6LASA21 REDEFINES IO-AREA-2B.                                   
051100*        05  -COPY W6GX6110    -PRE B-                                    
051200     EJECT                                                                
051300                                                                          
051400 01  DLI-IO-AREA-3.                                                       
051500     03  IO-AREA-3               PIC X(150)  VALUE SPACE.                 
051600     SKIP3                                                                
051700                                                                          
051800     03  W6PLAA01 REDEFINES IO-AREA-3.                                    
051900*        05  -COPY W6GX01                                                 
052000     EJECT                                                                
052100                                                                          
052200     03  W6PLAA11 REDEFINES IO-AREA-3.                                    
052300*        05  -COPY W6GX6006                                               
052400     EJECT                                                                
052500                                                                          
052600 01  DLI-IO-AREA-01.                                                      
052700     03  IO-AREA-01              PIC X(150)  VALUE SPACE.                 
052800     SKIP3                                                                
052900                                                                          
053000     03  W6HANA01 REDEFINES IO-AREA-01.                                   
053100*        05  -COPY W6GX01                                                 
053200     EJECT                                                                
053300                                                                          
053400                                                                          
053500 01  DLI-IO-AREA-12.                                                      
053600     03  IO-AREA-12              PIC X(150)  VALUE SPACE.                 
053700     SKIP3                                                                
053800                                                                          
053900     03  W6INLA21 REDEFINES IO-AREA-12.                                   
054000*        05  -COPY W6D121   -PRE  ALT-                                    
054100     EJECT                                                                
054200                                                                          
054300 01    DLI-IO-AREA-711.                                                   
054400*03      -COPY WDK711                                                     
054500                                                                          
054600                                                                          
054700 LINKAGE SECTION.                                                         
054800                                                                          
054900 01  REQU-AREA.                                                           
055000*    03 -COPY WZ01REQU                                                    
055100*    03 -COPY W60121I1                                                    
055200     EJECT                                                                
055300 01  RESP-AREA.                                                           
055400*    03 -COPY WZ01RESP                                                    
055500*    03 -COPY W60121O1                                                    
055600     EJECT                                                                
055700 01  MAX-KVRADER                 PIC S9(4) COMP.                          
055800*01  -COPY W0009   -PRE MSG-                                              
055900     EJECT                                                                
056000                                                                          
056100*01  -COPY W0009   -PRE ALT-                                              
056200     EJECT                                                                
056300                                                                          
056400*01  -COPY W0009   -PRE ALT1-                                             
056500     EJECT                                                                
056600                                                                          
056700*01  -COPY W0009   -PRE DISP-                                             
056800     EJECT                                                                
056900                                                                          
057000*01  -COPY W0008   -PRE USEA-                                             
057100     05  FILLER                  PIC X.                                   
057200     EJECT                                                                
057300                                                                          
057400*01  -COPY W0008   -PRE LISB-                                             
057500     05  FILLER                  PIC X.                                   
057600     EJECT                                                                
057700                                                                          
057800*01  -COPY W0008  -PRE INLA-                                              
057900     05  FILLER                  PIC X.                                   
058000     EJECT                                                                
058100                                                                          
058200*01  -COPY W0008  -PRE INLA-ALT-                                          
058300     05  FILLER                  PIC X.                                   
058400     EJECT                                                                
058500                                                                          
058600*01  -COPY W0008  -PRE LASA-A-                                            
058700     05  FILLER                  PIC X.                                   
058800     EJECT                                                                
058900                                                                          
059000*01  -COPY W0008  -PRE LASA-B-                                            
059100     05  FILLER                  PIC X.                                   
059200     EJECT                                                                
059300                                                                          
059400*01  -COPY W0008  -PRE PLAA-                                              
059500     05  FILLER                  PIC X.                                   
059600     EJECT                                                                
059700                                                                          
059800*01  -COPY W0008  -PRE HANA-                                              
059900     05  FILLER                  PIC X.                                   
060000     EJECT                                                                
060100                                                                          
060200*01  -COPY W0008  -PRE STYR-PLAA-                                         
060300     05  FILLER                  PIC X.                                   
060400     EJECT                                                                
060500                                                                          
060600 01  KOM-KOMA-PCB                PIC X.                                   
060700     EJECT                                                                
060800                                                                          
060900*01  -COPY W0008  -PRE LASA-W6012111-                                     
061000     05  FILLER                  PIC X.                                   
061100     EJECT                                                                
061200                                                                          
061300*01  -COPY W0008  -PRE INLA-W6012111-                                     
061400     05  FILLER                  PIC X.                                   
061500     EJECT                                                                
061600                                                                          
061700*01  -COPY W0008  -PRE LASA-W6012112-                                     
061800     05  FILLER                  PIC X.                                   
061900     EJECT                                                                
062000                                                                          
062100*01  -COPY W0008  -PRE INLA-W6012112-                                     
062200     05  FILLER                  PIC X.                                   
062300     EJECT                                                                
062400                                                                          
062500*01  -COPY W0008  -PRE WDK6-W6012112-                                     
062600     05  FILLER                  PIC X.                                   
062700     EJECT                                                                
062800*01  -COPY W0008  -PRE WDK7-                                              
062900     05  FILLER                  PIC X.                                   
063000     EJECT                                                                
063100                                                                          
063200 PROCEDURE DIVISION  USING                                                
063300                         REQU-AREA  RESP-AREA MAX-KVRADER MSG-PCB         
063400                         ALT-PCB    ALT1-PCB  DISP-PCB    USEA-PCB        
063500                         LISB-PCB   INLA-PCB  INLA-ALT-PCB                
063600                         LASA-A-PCB LASA-B-PCB PLAA-PCB   HANA-PCB        
063700                         STYR-PLAA-PCB        KOM-KOMA-PCB                
063800                         LASA-W6012111-PCB                                
063900                         INLA-W6012111-PCB                                
064000                         LASA-W6012112-PCB                                
064100                         INLA-W6012112-PCB                                
064200                         WDK6-W6012112-PCB WDK7-PCB.                      
064300                                                                          
064400     ENTRY 'DLITCBL' USING                                                
064500                         REQU-AREA  RESP-AREA MAX-KVRADER MSG-PCB         
064600                         ALT-PCB    ALT1-PCB  DISP-PCB    USEA-PCB        
064700                         LISB-PCB   INLA-PCB  INLA-ALT-PCB                
064800                         LASA-A-PCB LASA-B-PCB PLAA-PCB   HANA-PCB        
064900                         STYR-PLAA-PCB        KOM-KOMA-PCB                
065000                         LASA-W6012111-PCB                                
065100                         INLA-W6012111-PCB                                
065200                         LASA-W6012112-PCB                                
065300                         INLA-W6012112-PCB                                
065400                         WDK6-W6012112-PCB WDK7-PCB.                      
065500                                                                          
065600     EJECT                                                                
065700                                                                          
065800*----------------------------------------------------------------*        
065900        PERFORM A-INIT                                                    
066000        PERFORM B-KOLLA-NYCKLAR                                           
066100        IF NYCKLAR-OK                                                     
066200           IF REQU-UPDATE                                                 
066300              PERFORM G-KOLLA-INPUT                                       
066400              IF INDATA-OK                                                
066500                 PERFORM H-UPPDATERA                                      
066600                 IF REQU-FLKLAR-BIL = NEJ                                 
066700                    PERFORM MFS-RENSA-FAELT-UT                            
066800                    PERFORM S11-NKL-SAMMA-SIDA                            
066900                    MOVE JA  TO ALLT-SW                                   
067000                    PERFORM F-LAES-VISA-INFO                              
067100                 ELSE                                                     
067200*-------------------VISA TOM BILD                                         
067300                    MOVE ALL-SPACE       TO                               
067400                                            RESP-IDARTNR-KEY              
067500                                            RESP-IDLEVNR-KEY              
067600                                            RESP-IDFS-KEY                 
067700                                            RESP-IDLBBET-KEY              
067800                                            RESP-IDDC-KEY                 
067900                                            RESP-FLKLAR-TOT-KEY           
068000                    PERFORM MFS-RENSA-FAELT-UT                            
068100                    MOVE NEJ             TO RESP-FLKLAR-BIL               
068200                    MOVE REQU-ADINLOMR-PRT TO RESP-ADINLOMR-PRT           
068300                 END-IF                                                   
068400                 MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                  
068500*             ELSE                                                        
068600*               CALL FELLOG                                               
068700              END-IF                                                      
068800           ELSE                                                           
068900              IF REQU-FIRST                                               
069000                 PERFORM C-FOERSTA-SIDA                                   
069100              ELSE                                                        
069200                 IF REQU-NEXT                                             
069300                    PERFORM D-NAESTA-SIDA                                 
069400                 ELSE                                                     
069500                    IF REQU-PRINT                                         
069600                       PERFORM S61-KOLLA-PRINTER                          
069700                       PERFORM S65-KOLLA-FLKLAR-BIL                       
069800                       PERFORM S66-KOLLA-AETG                             
069900                       PERFORM S67-KOLLA-LASTBARARE                       
070000                       IF INDATA-OK                                       
070100                          PERFORM S80-LISTA-W6012112                      
070200                          MOVE SPACE TO RESP-TELOSSN1                     
070300                          MOVE SPACE TO RESP-TELOSSN2                     
070400                          PERFORM MFS-RENSA-FAELT-UT                      
070500                          PERFORM S11-NKL-SAMMA-SIDA                      
070600                          MOVE JA TO ALLT-SW                              
070700                       ELSE                                               
070800*------------------------- -PRINTER EJ ANGIVEN ELLER                      
070900*                          -FLKLAR-BIL=JA ELLER                           
071000*                          -NGN ÅTGÄRDS-KOD ÄR IFYLLD                     
071100*-------------------------I DETTA FALL HAR MAN                            
071200*                         VALT ATT LÄSA OM IGEN                           
071300*                         FÖR ATT KLARA BILDHANTERINGEN                   
071400*                         MAP STÄNGDA FÄLT ETC.                           
071500                          PERFORM S11-NKL-SAMMA-SIDA                      
071600                          MOVE JA           TO ALLT-SW                    
071700                                               INDATA-SW                  
071800                          MOVE ALL-PLUS          TO                       
071900                               RESP-ADINLOMR-PRT                          
072000                       END-IF                                             
072100                    ELSE                                                  
072200                       PERFORM E-SAMMA-SIDA                               
072300                    END-IF                                                
072400                 END-IF                                                   
072500              END-IF                                                      
072600              IF ALLT-OK                                                  
072700                 PERFORM F-LAES-VISA-INFO                                 
072800              END-IF                                                      
072900           END-IF                                                         
073000        END-IF                                                            
073100        PERFORM S90-BLANKUTF-NUM-FAELT                                    
073200                                                                          
073300     GOBACK                                                               
073400     .                                                                    
073500     EJECT                                                                
073600*----------------------------------------------------------------*        
073700 A-INIT SECTION.                                                          
073800                                                                          
073900     MOVE ALL '+'                TO RESP-W60121O1                         
074000     MOVE 001                    TO RESP-IDMSGVER                         
074100     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
074200                                    RESP-IDMSG-INFO                       
074300                                    RESP-IDELMT-ERROR                     
074400     PERFORM MFS-FORM-ATTR                                                
074500                                                                          
074600     MOVE REQU-KVRADER           TO RESP-KVRADER                          
074700                                                                          
074800     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
074900     ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                    
075000     .                                                                    
075100     EJECT                                                                
075200*----------------------------------------------------------------*        
075300 B-KOLLA-NYCKLAR SECTION.                                                 
075400                                                                          
075500     MOVE JA                 TO NYCKLAR-SW                                
075600                                INDATA-SW                                 
075700     MOVE SPACE              TO NKLTYP-SW                                 
075800     MOVE SPACE              TO RESP-IDARTNR-KEY                          
075900                                RESP-IDLEVNR-KEY                          
076000                                RESP-IDFS-KEY                             
076100                                RESP-IDLBBET-KEY                          
076200                                RESP-FLKLAR-TOT-KEY                       
076300*----NKL-FÄLT-UT                                                          
076400     INSPECT REQU-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO             
076500     INSPECT REQU-IDLOPNRM-KEY REPLACING LEADING SPACE BY ZERO            
076700     IF REQU-IDLBBET-KEY = (ALL '+' OR SPACE) AND                         
076800        REQU-IDARTNR-KEY = (ALL '+' OR ZERO ) AND                         
076900        REQU-IDLEVNR-KEY = (ALL '+' OR SPACE) AND                         
077000        REQU-IDFS-KEY    = (ALL '+' OR SPACE)                             
077100*-FEL - 401                                                               
077200           MOVE NEJ          TO NYCKLAR-SW                                
077300           MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                         
077400           MOVE 'IDLBBET'    TO RESP-IDELMT-ERROR                         
077500           MOVE ZERO         TO RESP-KVRADER                              
077600     ELSE                                                                 
077700*-------NYA NYCKLAR                                                       
077800        PERFORM BA-FORMELLA-KONTR                                         
077900*       MOVE NEJ                   TO REQU-FLKLAR-BIL                     
078000*                                     RESP-FLKLAR-BIL                     
078100     END-IF                                                               
078101     IF REQU-FIRST                                                        
078102*-------NEW KEY                                                           
078104        MOVE NEJ                   TO REQU-FLKLAR-BIL                     
078105                                      RESP-FLKLAR-BIL                     
078106     END-IF                                                               
078200                                                                          
078300     IF REQU-ADINLOMR-PRT NOT = ALL '+' AND                               
078400        REQU-ADINLOMR-PRT NOT = SPACE                                     
078500        MOVE REQU-ADINLOMR-PRT TO RESP-ADINLOMR-PRT                       
078600        MOVE MFS-ADD-LAES-IN-FAELT TO RESP-ADINLOMR-PRT-ATTR              
078700     END-IF                                                               
078800                                                                          
078900     IF REQU-ADINLOMR NOT = ALL '+' AND                                   
079000        REQU-ADINLOMR NOT = SPACE                                         
079100        MOVE REQU-ADINLOMR TO RESP-ADINLOMR                               
079200        MOVE MFS-ADD-LAES-IN-FAELT TO RESP-ADINLOMR-ATTR                  
079300     END-IF                                                               
079400                                                                          
079500     MOVE REQU-IDDC-KEY TO    RESP-IDDC-KEY                               
079600                              W-IDDC                                      
079700                              W-IDDC-K7                                   
079800                              WL-IDDC                                     
079900                              WGX-IDDC                                    
080000                                                                          
080100     IF REQU-TELOSSN1 NOT = SPACE                                         
080200        MOVE REQU-TELOSSN1 TO RESP-TELOSSN1                               
080300     END-IF                                                               
080400     IF REQU-TELOSSN2 NOT = SPACE                                         
080500        MOVE REQU-TELOSSN2 TO RESP-TELOSSN2                               
080600     END-IF                                                               
080700                                                                          
080800     IF REQU-FLKLAR-TOT-KEY NOT = ALL '+' AND                             
080900        REQU-FLKLAR-TOT-KEY NOT = SPACE AND                               
081000        REQU-FLKLAR-TOT-KEY NOT = LOW-VALUE                               
081100        IF (REQU-FLKLAR-TOT-KEY = JA OR                                   
081200            REQU-FLKLAR-TOT-KEY = YES OR                                  
081300            REQU-FLKLAR-TOT-KEY = NEJ)                                    
081400            MOVE REQU-FLKLAR-TOT-KEY  TO RESP-FLKLAR-TOT-KEY              
081500        ELSE                                                              
081600*-FEL - 401                                                               
081700*-------------FEL    FLKLAR INTE J/N                                      
081800            MOVE NEJ               TO NYCKLAR-SW                          
081900            MOVE REQU-FLKLAR-TOT-KEY TO RESP-FLKLAR-TOT-KEY               
082000            MOVE ERR-WRONG-KEY     TO RESP-IDMSG-ERROR                    
082100            MOVE 'FLKLAR-TOT'      TO RESP-IDELMT-ERROR                   
082200            MOVE ZERO              TO RESP-KVRADER                        
082300        END-IF                                                            
082400     ELSE                                                                 
082500        MOVE NEJ                TO RESP-FLKLAR-TOT-KEY                    
082600     END-IF                                                               
082700     IF REQU-FLKLAR-BIL = '+' OR                                          
082800        REQU-FLKLAR-BIL = SPACE OR                                        
082900        REQU-FLKLAR-BIL = LOW-VALUE                                       
083000        MOVE NEJ                   TO RESP-FLKLAR-BIL                     
083100        MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLKLAR-BIL-ATTR                
083200     ELSE                                                                 
083300        MOVE REQU-FLKLAR-BIL       TO RESP-FLKLAR-BIL                     
083400        MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLKLAR-BIL-ATTR                
083500     END-IF                                                               
083600                                                                          
083700     .                                                                    
083800     EJECT                                                                
083900*----------------------------------------------------------------*        
084000 BA-FORMELLA-KONTR SECTION.                                               
084100                                                                          
084200*----LASTBÄRARE                                                           
084300     IF REQU-IDLBBET-KEY NOT = (ALL '+' OR SPACE) AND                     
084400        REQU-IDARTNR-KEY   = (ALL '+' OR ZERO) AND                        
084500        REQU-IDLEVNR-KEY   = (ALL '+' OR SPACE) AND                       
084600        REQU-IDFS-KEY      = (ALL '+' OR SPACE)                           
084700*-------NY NKL - LASTB                                                    
084800        MOVE '1'                   TO NKLTYP-SW                           
084900        MOVE REQU-IDLBBET-KEY      TO RESP-IDLBBET-KEY                    
085000        MOVE SPACE                 TO RESP-IDFS-KEY                       
085100                                      RESP-IDLEVNR-KEY                    
085200        MOVE ZERO                  TO RESP-IDARTNR-KEY                    
085300        PERFORM MFS-RENSA-FAELT-UT                                        
085400     ELSE                                                                 
085500*-------LASTBÄRARE,ARTNR                                                  
085600        IF REQU-IDLBBET-KEY NOT = (ALL '+' OR SPACE) AND                  
085700           REQU-IDARTNR-KEY NOT = (ALL '+' OR ZERO) AND                   
085800           REQU-IDLEVNR-KEY    = (ALL '+' OR SPACE) AND                   
085900           REQU-IDFS-KEY       = (ALL '+' OR SPACE)                       
086000           MOVE '2'               TO NKLTYP-SW                            
086100           PERFORM BAA-KONTR-LASTB-ART                                    
086200           PERFORM MFS-RENSA-FAELT-UT                                     
086300        ELSE                                                              
086400*----------LASTBÄRARE,ARTNR,LEVNR,FS                                      
086500           IF REQU-IDLBBET-KEY NOT = (ALL '+' OR SPACE) AND               
086600              REQU-IDARTNR-KEY NOT = (ALL '+' OR ZERO) AND                
086700              REQU-IDLEVNR-KEY NOT = (ALL '+' OR SPACE) AND               
086800              REQU-IDFS-KEY   NOT = (ALL '+' OR SPACE)                    
086900              MOVE '3'            TO NKLTYP-SW                            
087000              PERFORM BAB-KONTR-LASTB-ART-LEV-FS                          
087100              PERFORM MFS-RENSA-FAELT-UT                                  
087200           ELSE                                                           
087300*-------------NY DEL-NKL-ARTNR,LEVNR,FS                                   
087400              IF REQU-IDLBBET-KEY = (ALL '+' OR SPACE) AND                
087500                 (REQU-IDARTNR-KEY NOT = (ALL '+' OR ZERO) OR             
087600                  REQU-IDLEVNR-KEY NOT = (ALL '+' OR SPACE) OR            
087700                  REQU-IDFS-KEY  NOT = (ALL '+' OR SPACE))                
087800                 MOVE '3'         TO NKLTYP-SW                            
087900                 PERFORM BAC-KONTR-NYA-DELNKL                             
088000                 PERFORM MFS-RENSA-FAELT-UT                               
088100              ELSE                                                        
088200*-FEL - 401                                                               
088300                 MOVE NEJ          TO NYCKLAR-SW                          
088400                 MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                   
088500                 MOVE 'IDLBBET'     TO RESP-IDELMT-ERROR                  
088600                 MOVE ZERO              TO RESP-KVRADER                   
088700                 IF REQU-IDLBBET-KEY = ALL '+'                            
088800                    MOVE SPACE          TO RESP-IDLBBET-KEY               
088900                 ELSE                                                     
089000                    MOVE REQU-IDLBBET-KEY TO RESP-IDLBBET-KEY             
089100                 END-IF                                                   
089200                 IF REQU-IDARTNR-KEY = ALL '+'                            
089300                    MOVE SPACE          TO RESP-IDARTNR-KEY               
089400                 ELSE                                                     
089500                    MOVE REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY             
089600                 END-IF                                                   
089700                 IF REQU-IDLEVNR-KEY = ALL '+'                            
089800                    MOVE SPACE          TO RESP-IDLEVNR-KEY               
089900                 ELSE                                                     
090000                    MOVE REQU-IDLEVNR-KEY TO RESP-IDLEVNR-KEY             
090100                 END-IF                                                   
090200                 IF REQU-IDFS-KEY = ALL '+'                               
090300                    MOVE SPACE        TO RESP-IDFS-KEY                    
090400                 ELSE                                                     
090500                    MOVE REQU-IDFS-KEY TO RESP-IDFS-KEY                   
090600                 END-IF                                                   
090700              END-IF                                                      
090800           END-IF                                                         
090900        END-IF                                                            
091000     END-IF                                                               
091100     .                                                                    
091200     EJECT                                                                
091300*----------------------------------------------------------------*        
091400 BAA-KONTR-LASTB-ART SECTION.                                             
091500                                                                          
091600     IF (REQU-IDARTNR-KEY NOT = ALL '+') AND                              
091700        (REQU-IDARTNR-KEY = ZERO OR                                       
091800         REQU-IDARTNR-KEY NOT NUMERIC)                                    
091900*-FEL - 401                                                               
092000        MOVE NEJ                   TO NYCKLAR-SW                          
092100        MOVE ERR-WRONG-KEY         TO RESP-IDMSG-ERROR                    
092200        MOVE 'IDARTNR'             TO RESP-IDELMT-ERROR                   
092300        MOVE ZERO              TO RESP-KVRADER                            
092400     END-IF                                                               
092500                                                                          
092600     MOVE REQU-IDLBBET-KEY         TO RESP-IDLBBET-KEY                    
092700     MOVE REQU-IDARTNR-KEY         TO RESP-IDARTNR-KEY                    
092800     MOVE SPACE                    TO RESP-IDFS-KEY                       
092900     MOVE SPACE                    TO RESP-IDLEVNR-KEY                    
093000     .                                                                    
093100     EJECT                                                                
093200*----------------------------------------------------------------*        
093300 BAB-KONTR-LASTB-ART-LEV-FS SECTION.                                      
093400                                                                          
093500     IF (REQU-IDARTNR-KEY NOT = ALL '+') AND                              
093600        (REQU-IDARTNR-KEY = ZERO OR                                       
093700         REQU-IDARTNR-KEY NOT NUMERIC)                                    
093800*-FEL - 401                                                               
093900         MOVE NEJ               TO NYCKLAR-SW                             
094000         MOVE ERR-WRONG-KEY     TO RESP-IDMSG-ERROR                       
094100         MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                      
094200         MOVE ZERO              TO RESP-KVRADER                           
094300     END-IF                                                               
094400     MOVE REQU-IDLBBET-KEY        TO RESP-IDLBBET-KEY                     
094500     MOVE REQU-IDARTNR-KEY        TO RESP-IDARTNR-KEY                     
094600     MOVE REQU-IDLEVNR-KEY        TO RESP-IDLEVNR-KEY                     
094700     MOVE REQU-IDFS-KEY           TO RESP-IDFS-KEY                        
094800     .                                                                    
094900     EJECT                                                                
095000*----------------------------------------------------------------*        
095100*--- KONTROLL AV NYA NYCKLAR (EV. FÄRRE I ANTAL)          ------*         
095200*--- TA REDA PÅ OM DET SKA BLI ENL NKLTYP 1 ELLER 2       ------*         
095300*--- ELLER OM 3:AN GÄLLER FORTFARANDE                     ------*         
095400*----------------------------------------------------------------*        
095500 BAC-KONTR-NYA-DELNKL SECTION.                                            
095600                                                                          
095700     IF REQU-IDARTNR-KEY = ALL '+' AND                                    
095800        REQU-IDLEVNR-KEY = ALL '+' AND                                    
095900        REQU-IDFS-KEY NOT = ALL '+'                                       
096000        PERFORM BACA-FS                                                   
096100     ELSE                                                                 
096200        IF REQU-IDARTNR-KEY   = ALL '+' AND                               
096300           REQU-IDLEVNR-KEY NOT = ALL '+' AND                             
096400           REQU-IDFS-KEY      = ALL '+'                                   
096500           PERFORM BACB-LEV                                               
096600        ELSE                                                              
096700           IF REQU-IDARTNR-KEY NOT = ALL '+' AND                          
096800              REQU-IDLEVNR-KEY   = ALL '+' AND                            
096900              REQU-IDFS-KEY      = ALL '+'                                
097000              PERFORM BACC-ART                                            
097100           ELSE                                                           
097200              IF REQU-IDARTNR-KEY NOT = ALL '+' AND                       
097300                 (REQU-IDLEVNR-KEY NOT = ALL '+' OR                       
097400                 REQU-IDFS-KEY  NOT = ALL '+')                            
097500                 PERFORM BACD-KOLLA-ART-LEV-FS                            
097600              ELSE                                                        
097700                 IF REQU-IDARTNR-KEY   = ALL '+' AND                      
097800                    (REQU-IDLEVNR-KEY NOT = ALL '+' OR                    
097900                    REQU-IDFS-KEY NOT = ALL '+')                          
098000                    PERFORM BACE-LEV-FS                                   
098100                 ELSE                                                     
098200*-FEL - 401 - INFORMATION MISSING                                         
098300                    MOVE NEJ         TO NYCKLAR-SW                        
098400                    MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                
098500                    MOVE 'INF'       TO RESP-IDELMT-ERROR                 
098600                    MOVE ZERO              TO RESP-KVRADER                
098700                 END-IF                                                   
098800              END-IF                                                      
098900           END-IF                                                         
099000        END-IF                                                            
099100     END-IF                                                               
099200     .                                                                    
099300     EJECT                                                                
099400*----------------------------------------------------------------*        
099500 BACA-FS       SECTION.                                                   
099600                                                                          
099700*----KOLLA EV. FÖRÄNDRING AV NKLTYP  ------*                              
099800     IF REQU-IDFS-KEY = (SPACE OR LOW-VALUE)                              
099900*-------LASTB-ART ==> NKLTYP 2                                            
100000        MOVE '2'                     TO NKLTYP-SW                         
100100        MOVE REQU-IDLBBET-KEY        TO RESP-IDLBBET-KEY                  
100200        MOVE REQU-IDARTNR-KEY        TO RESP-IDARTNR-KEY                  
100300        MOVE SPACE                   TO RESP-IDLEVNR-KEY                  
100400                                        RESP-IDFS-KEY                     
100500     ELSE                                                                 
100600*-------NY FS                                                             
100700        MOVE REQU-IDLBBET-KEY        TO RESP-IDLBBET-KEY                  
100800        MOVE REQU-IDARTNR-KEY        TO RESP-IDARTNR-KEY                  
100900        MOVE REQU-IDLEVNR-KEY        TO RESP-IDLEVNR-KEY                  
101000        MOVE REQU-IDFS-KEY           TO RESP-IDFS-KEY                     
101100     END-IF                                                               
101200     .                                                                    
101300     EJECT                                                                
101400*----------------------------------------------------------------*        
101500 BACB-LEV       SECTION.                                                  
101600                                                                          
101700     MOVE REQU-IDLBBET-KEY          TO RESP-IDLBBET-KEY                   
101800     MOVE REQU-IDARTNR-KEY          TO RESP-IDARTNR-KEY                   
101900     MOVE REQU-IDLEVNR-KEY          TO RESP-IDLEVNR-KEY                   
102000     MOVE REQU-IDFS-KEY             TO RESP-IDFS-KEY                      
102100     .                                                                    
102200     EJECT                                                                
102300*----------------------------------------------------------------*        
102400 BACC-ART           SECTION.                                              
102500                                                                          
102600     IF REQU-IDARTNR-KEY NOT = ALL '+' AND                                
102700        REQU-IDARTNR-KEY NOT NUMERIC                                      
102800*-FEL - 401                                                               
102900        MOVE NEJ               TO NYCKLAR-SW                              
103000        MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                       
103100        MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                       
103200        MOVE ZERO              TO RESP-KVRADER                            
103300     ELSE                                                                 
103400*-------OK                                                                
103500*-------KOLLA    EV. FÖRÄNDRING AV NKLTYP ------*                         
103600        IF REQU-IDARTNR-KEY = ZERO                                        
103700*----------ENBART    LASTB ==> NKLTYP 1                                   
103800           MOVE '1'                  TO NKLTYP-SW                         
103900           MOVE REQU-IDLBBET-KEY     TO RESP-IDLBBET-KEY                  
104000           MOVE SPACE                TO RESP-IDARTNR-KEY                  
104100                                           RESP-IDLEVNR-KEY               
104200                                           RESP-IDFS-KEY                  
104300        ELSE                                                              
104400           IF REQU-IDLBBET-KEY = ALL '+' AND                              
104500              REQU-IDLBBET-KEY NOT = ALL '+' AND                          
104600              REQU-IDLEVNR-KEY = SPACE AND                                
104700              REQU-IDFS-KEY  = SPACE                                      
104800*-------------NY     ART TILLSAMMANS MED GAMMAL LASTB                     
104900              MOVE '2'               TO NKLTYP-SW                         
105000              MOVE REQU-IDLBBET-KEY  TO RESP-IDLBBET-KEY                  
105100              MOVE REQU-IDARTNR-KEY  TO RESP-IDARTNR-KEY                  
105200              MOVE SPACE             TO RESP-IDLEVNR-KEY                  
105300                                        RESP-IDFS-KEY                     
105400           ELSE                                                           
105500*-------------NY ART                                                      
105600              MOVE REQU-IDLBBET-KEY  TO RESP-IDLBBET-KEY                  
105700              MOVE REQU-IDARTNR-KEY  TO RESP-IDARTNR-KEY                  
105800              MOVE REQU-IDLEVNR-KEY  TO RESP-IDLEVNR-KEY                  
105900              MOVE REQU-IDFS-KEY     TO RESP-IDFS-KEY                     
106000           END-IF                                                         
106100        END-IF                                                            
106200     END-IF                                                               
106300     .                                                                    
106400     EJECT                                                                
106500*----------------------------------------------------------------*        
106600 BACD-KOLLA-ART-LEV-FS  SECTION.                                          
106700                                                                          
106800*----KOLLA    EV. FÖRÄNDRING AV NKLTYP ------*                            
106900                                                                          
107000     IF REQU-IDARTNR-KEY NOT = ALL '+' AND                                
107100        REQU-IDARTNR-KEY NOT NUMERIC                                      
107200*-FEL - 401                                                               
107300        MOVE NEJ                TO NYCKLAR-SW                             
107400        MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                       
107500        MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                       
107600        MOVE ZERO              TO RESP-KVRADER                            
107700     ELSE                                                                 
107800*-------OK                                                                
107900        IF REQU-IDARTNR-KEY = ZERO                                        
108000*----------ENBART       LASTB ==> NKLTYP 1                                
108100           MOVE '1'                  TO NKLTYP-SW                         
108200           MOVE REQU-IDLBBET-KEY     TO RESP-IDLBBET-KEY                  
108300           MOVE SPACE                TO RESP-IDARTNR-KEY                  
108400                                        RESP-IDLEVNR-KEY                  
108500                                        RESP-IDFS-KEY                     
108600        ELSE                                                              
108700*----------NY       ART                                                   
108800           MOVE REQU-IDLBBET-KEY     TO RESP-IDLBBET-KEY                  
108900           MOVE REQU-IDARTNR-KEY     TO RESP-IDARTNR-KEY                  
109000              IF REQU-IDLEVNR-KEY = SPACE AND                             
109100                 REQU-IDFS-KEY = (SPACE OR LOW-VALUE)                     
109200*----------------LASTB-ART       ==> NKLTYP 2                             
109300                 MOVE '2'            TO NKLTYP-SW                         
109400                 MOVE SPACE          TO RESP-IDLEVNR-KEY                  
109500                                              RESP-IDFS-KEY               
109600              ELSE                                                        
109700*----------------LASTB-ARTLEV-FS       ==> NKLTYP 3                       
109800                 IF REQU-IDLEVNR-KEY NOT = ALL '+'                        
109900                    MOVE REQU-IDLEVNR-KEY TO RESP-IDLEVNR-KEY             
110000                 ELSE                                                     
110100                    MOVE REQU-IDLEVNR-KEY TO RESP-IDLEVNR-KEY             
110200                 END-IF                                                   
110300                 IF REQU-IDFS-KEY NOT = ALL '+'                           
110400                    MOVE REQU-IDFS-KEY TO RESP-IDFS-KEY                   
110500                 ELSE                                                     
110600                    MOVE REQU-IDFS-KEY TO RESP-IDFS-KEY                   
110700                 END-IF                                                   
110800              END-IF                                                      
110900        END-IF                                                            
111000     END-IF                                                               
111100     .                                                                    
111200     EJECT                                                                
111300*----------------------------------------------------------------*        
111400 BACE-LEV-FS  SECTION.                                                    
111500                                                                          
111600*----KOLLA    EV. FÖRÄNDRING AV NKLTYP ------*                            
111700                                                                          
111800     MOVE REQU-IDLBBET-KEY           TO RESP-IDLBBET-KEY                  
111900     MOVE REQU-IDARTNR-KEY           TO RESP-IDARTNR-KEY                  
112000        IF REQU-IDLEVNR-KEY = SPACE AND                                   
112100           REQU-IDFS-KEY = (SPACE OR LOW-VALUE)                           
112200*----------LASTB-ART ==> NKLTYP 2                                         
112300           MOVE '2'                  TO NKLTYP-SW                         
112400           MOVE SPACE                TO RESP-IDLEVNR-KEY                  
112500                                        RESP-IDFS-KEY                     
112600        ELSE                                                              
112700*----------LASTB-ART-LEV-FS ==> NKLTYP 3                                  
112800           IF REQU-IDLEVNR-KEY NOT = ALL '+'                              
112900              MOVE REQU-IDLEVNR-KEY TO RESP-IDLEVNR-KEY                   
113000           ELSE                                                           
113100              MOVE REQU-IDLEVNR-KEY TO RESP-IDLEVNR-KEY                   
113200           END-IF                                                         
113300           IF REQU-IDFS-KEY NOT = ALL '+'                                 
113400              MOVE REQU-IDFS-KEY TO RESP-IDFS-KEY                         
113500           ELSE                                                           
113600              MOVE REQU-IDFS-KEY TO RESP-IDFS-KEY                         
113700           END-IF                                                         
113800        END-IF                                                            
113900     .                                                                    
114000     EJECT                                                                
114100*----------------------------------------------------------------*        
114200*----------------------------------------------------------------*        
114300 C-FOERSTA-SIDA SECTION.                                                  
114400                                                                          
114500     MOVE INF-FIRST-PAGE     TO RESP-IDMSG-INFO                           
114600                                                                          
114700*----BLANKA/NOLLA UT BLÄDDRINGSNYCKLAR                                    
114800     PERFORM MFS-RENSA-FAELT-UT                                           
114900                                                                          
115000     PERFORM S12-INIT-NKL                                                 
115100                                                                          
115200     MOVE JA                 TO ALLT-SW                                   
115300     .                                                                    
115400     EJECT                                                                
115500*----------------------------------------------------------------*        
115600 D-NAESTA-SIDA SECTION.                                                   
115700                                                                          
115800     PERFORM MFS-RENSA-FAELT-UT                                           
115900     IF REQU-IDARTNR-START = ZERO AND                                     
116000        REQU-IDLEVNR-START = SPACE AND                                    
116100        REQU-IDFS-START    = SPACE                                        
116200*-------FEL DETTA ÄR SISTA SIDAN                                          
116300*-------I DETTA FALL HAR MAN VALT ATT LÄSA OM IGEN                        
116400*-------FÖR ATT KLARA BILDHANTERINGEN MAP STÄNGDA FÄLT ETC.               
116500        PERFORM S11-NKL-SAMMA-SIDA                                        
116600        MOVE JA              TO ALLT-SW                                   
116700        MOVE INF-LAST-PAGE            TO RESP-IDMSG-INFO                  
116800     ELSE                                                                 
116900        PERFORM S10-NKL-NAESTA-SIDA                                       
117000        MOVE JA              TO ALLT-SW                                   
117100     END-IF                                                               
117200     .                                                                    
117300     EJECT                                                                
117400*----------------------------------------------------------------*        
117500 E-SAMMA-SIDA SECTION.                                                    
117600                                                                          
117700        PERFORM MFS-RENSA-FAELT-UT                                        
117800                                                                          
117900        MOVE ZERO    TO WS-COUNT                                          
118000        PERFORM                                                           
118100        VARYING IX-X FROM 1 BY 1                                          
118200          UNTIL IX-X > REQU-KVRADER                                       
118300          IF REQU-KDCMDVAL-INPUT-LINE (IX-X) = ALL '+'                    
118400            ADD +1   TO WS-COUNT                                          
118500          END-IF                                                          
118600        END-PERFORM                                                       
118700                                                                          
118800        IF WS-COUNT = REQU-KVRADER  AND                                   
118900           REQU-FLKLAR-BIL = NEJ AND                                      
119000           REQU-TELOSSN1 = SPACE AND                                      
119100           REQU-TELOSSN2 = SPACE                                          
119200           PERFORM S11-NKL-SAMMA-SIDA                                     
119300           MOVE JA           TO ALLT-SW                                   
119400        ELSE                                                              
119500*----------ÅTGÄRD IFYLLD/FLKLAR-BIL=J  OCH ENTER TRYCKT                   
119600*----------I DETTA FALL HAR MAN VALT ATT LÄSA OM IGEN                     
119700*----------FÖR ATT KLARA BILDHANTERINGEN MAP STÄNGDA FÄLT ETC.            
119800           PERFORM S11-NKL-SAMMA-SIDA                                     
119900           MOVE JA           TO ALLT-SW                                   
120000           MOVE REQU-TELOSSN1        TO RESP-TELOSSN1                     
120100           MOVE REQU-TELOSSN2        TO RESP-TELOSSN2                     
120200           IF REQU-FLKLAR-BIL = NEJ AND                                   
120300              (REQU-TELOSSN1 NOT = SPACE OR                               
120400              REQU-TELOSSN2 NOT = SPACE)                                  
120500              MOVE INF-PRESS-PF4 TO RESP-IDMSG-ERROR                      
120600           ELSE                                                           
120700              MOVE INF-PRESS-PF11 TO RESP-IDMSG-ERROR                     
120800              PERFORM MFS-ROER-EJ-LAES-IN-AETG                            
120900           END-IF                                                         
121000        END-IF                                                            
121100     .                                                                    
121200     EJECT                                                                
121300*----------------------------------------------------------------*        
121400*--- NKLTYP1                                                  ---*        
121500*--- LÄSNING AV W6LASA                                        ---*        
121600*--- MED ENBART LASTBÄRARE SOM NYCKEL                         ---*        
121700*--- OCH ARTNR SOM START-VÄRDE                                ---*        
121800*----------------------------------------------------------------*        
121900*---                                                          ---*        
122000*--- LÄSNING AV W6LASA                                        ---*        
122100*--- MED LASTBÄRARE SOM NYCKEL                                ---*        
122200*--- OCH ARTNR SOM SÖKNYCKEL                                  ---*        
122300*----------------------------------------------------------------*        
122400*--- NKLTYP3                                                  ---*        
122500*--- LÄSNING AV W6LASA                                        ---*        
122600*--- MED LASTBÄRARE SOM NYCKEL                                ---*        
122700*--- OCH LEVNR, ARTNR OCH FS  SOM SÖKNYCKLAR                  ---*        
122800*----------------------------------------------------------------*        
122900 F-LAES-VISA-INFO SECTION.                                                
123000                                                                          
123100     MOVE ZERO                        TO RESP-KVRADER                     
123200*----LÄS BEROENDE PÅ NKLTYP,NÄSTA SIDA, SAMMA SIDA ETC                    
123300                                                                          
123400*----INITIERA WT-TAB                                                      
123500     PERFORM S25-INIT-WT-TAB                                              
123600                                                                          
123700     PERFORM IMS-GU-LASA-G111                                             
123800                                                                          
123900     IF SEGMENT-FINNS                                                     
124000        MOVE A-6108-ADINLOMR-LPL      TO RESP-ADINLOMR                    
124100        MOVE MFS-ADD-LAES-IN-FAELT    TO RESP-ADINLOMR-ATTR               
124200        PERFORM FA-BEARB-NKLTYPER                                         
124300     ELSE                                                                 
124400*-FEL - 010                                                               
124500*------ INGA LASTBÄRARE SAKNAS FÖR ANG. ART-LEV-FS                        
124600        MOVE NEJ                      TO NYCKLAR-SW                       
124700        MOVE ERR-SAKN-I-REG           TO RESP-IDMSG-ERROR                 
124800*       MOVE 'IDLBBET'                TO RESP-IDELMT-ERROR                
124900        PERFORM MFS-RENSA-FAELT-UT                                        
125000     END-IF                                                               
125100     .                                                                    
125200     EJECT                                                                
125300*----------------------------------------------------------------*        
125400 FA-BEARB-NKLTYPER  SECTION.                                              
125500                                                                          
125600     IF NKLTYP1  AND                                                      
125700       (SAMMA-SIDA-JA OR                                                  
125800        RESP-NEXT = JA)                                                   
125900        PERFORM IMS-GNP-LASA-G121-SOEK2                                   
126000     ELSE                                                                 
126100        IF NKLTYP2 AND                                                    
126200          (SAMMA-SIDA-JA OR                                               
126300           RESP-NEXT = JA)                                                
126400           PERFORM IMS-GNP-LASA-G121-SOEK2                                
126500        ELSE                                                              
126600           PERFORM FAB-LAES-G121                                          
126700        END-IF                                                            
126800     END-IF                                                               
126900     IF SEGMENT-FINNS                                                     
127000        MOVE A-6110-IDARTNR           TO RESP-IDARTNR-START               
127100        MOVE A-6110-IDLEVNR           TO RESP-IDLEVNR-START               
127200        MOVE A-6110-IDFS              TO RESP-IDFS-START                  
127300                                                                          
127400        MOVE +1   TO IX                                                   
127500        MOVE +1   TO TIX                                                  
127600        MOVE ZERO TO MAX-TIX                                              
127700        PERFORM UNTIL IX > MAX-KVRADER                                    
127800           IF SEGMENT-FINNS                                               
127900              PERFORM FAA-KOLL-LASTB                                      
128000              PERFORM FAB-LAES-G121                                       
128100           ELSE                                                           
128200              PERFORM MFS-RENSA-RAD-FAELT-UT                              
128300              ADD 1 TO IX                                                 
128400           END-IF                                                         
128500        END-PERFORM                                                       
128600                                                                          
128700*----FLYTTA WT-RADER TILL MOD-RADER                                       
128800        MOVE STATUS-WS TO SAVE-STATUS-WS                                  
128900        PERFORM S22-FLYTTA-WT-MOD                                         
129000                                                                          
129100        IF SAVE-SEGMENT-FINNS                                             
129200*----------FLER RADER PÅ REG                                              
129300           MOVE JA                    TO RESP-NEXT                        
129400           PERFORM S24-INIT-MOD-NEXT                                      
129500           MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                   
129600        ELSE                                                              
129700*----------INGA FLER RADER PÅ REG - RENSA NKL:AR                          
129800           MOVE SPACE                 TO RESP-IDFS-NEXT                   
129900           MOVE SPACE                 TO RESP-IDLEVNR-NEXT                
130000           MOVE ZERO                  TO RESP-IDARTNR-NEXT                
130100           MOVE NEJ                   TO RESP-NEXT                        
130200           MOVE INF-LAST-PAGE         TO RESP-IDMSG-INFO                  
130300        END-IF                                                            
130400     ELSE                                                                 
130500*-FEL - 010                                                               
130600*-------INGA RADER PÅ REG                                                 
130700        MOVE NEJ                      TO NYCKLAR-SW                       
130800        MOVE ERR-SAKN-I-REG           TO RESP-IDMSG-INFO                  
130900*       MOVE 'IDAVINR'                TO RESP-IDELMT-ERROR                
131000     END-IF                                                               
131100     .                                                                    
131200     EJECT                                                                
131300*----------------------------------------------------------------*        
131400 FAA-KOLL-LASTB   SECTION.                                                
131500                                                                          
131600     IF WT-IDARTNR(TIX) = A-6110-IDARTNR AND                              
131700        WT-IDLEVNR(TIX) = A-6110-IDLEVNR AND                              
131800        WT-IDFS(TIX)     = A-6110-IDFS   AND                              
131900        WT-TIAVIDAT(TIX) = A-6110-TIAVIDAT                                
132000*-------SAMMA ART IGEN                                                    
132100*-------KOLLA OM PARTIET SKA MED UT PÅ BILDEN                             
132200        PERFORM FAAA-KOLLA-PARTI                                          
132300     ELSE                                                                 
132400*-------NY ART                                                            
132500        IF             TIX = 1   AND                                      
132600           WT-IDARTNR(TIX) = ZERO AND                                     
132700           WT-IDLEVNR(TIX) = SPACE AND                                    
132800           WT-IDFS(TIX)    = SPACE                                        
132900           MOVE ZERO TO TIX                                               
133000        END-IF                                                            
133100        PERFORM FAAB-LAES-INLA-D111-D121                                  
133200*-------KOLLA OM PARTIET SKA MED UT PÅ BILDEN                             
133300        PERFORM FAAA-KOLLA-PARTI                                          
133400     END-IF                                                               
133500     .                                                                    
133600     EJECT                                                                
133700*----------------------------------------------------------------*        
133800 FAAA-KOLLA-PARTI         SECTION.                                        
133900                                                                          
134000     IF RESP-FLKLAR-TOT-KEY = JA OR YES                                   
134100        ADD +1 TO IX                                                      
134200                  RESP-KVRADER                                            
134300        ADD +1 TO TIX                                                     
134400        ADD +1 TO MAX-TIX                                                 
134500        PERFORM S21-FLYTTA-LASA-WT                                        
134600        PERFORM S23-FLYTTA-SPAR-INLA11-WT                                 
134700        PERFORM S24-INIT-MOD-NEXT                                         
134800     ELSE                                                                 
134900        IF A-6110-FLKLAR = NEJ                                            
135000           ADD +1 TO IX                                                   
135100                     RESP-KVRADER                                         
135200           ADD +1 TO TIX                                                  
135300           ADD +1 TO MAX-TIX                                              
135400           PERFORM S21-FLYTTA-LASA-WT                                     
135500           PERFORM S23-FLYTTA-SPAR-INLA11-WT                              
135600           PERFORM S24-INIT-MOD-NEXT                                      
135700        END-IF                                                            
135800        IF TIX = ZERO                                                     
135900           ADD +1 TO TIX                                                  
136000        END-IF                                                            
136100     END-IF                                                               
136200     .                                                                    
136300     EJECT                                                                
136400*----------------------------------------------------------------*        
136500 FAAB-LAES-INLA-D111-D121 SECTION.                                        
136600                                                                          
136700*----LÄS INLA-SEGM D111 OCH D121 FÖR BERÄKNA ANTAL KOLLI                  
136800*----OCH KONTROLLERA OM FLAGGA-KLAR SKA SLÅS PÅ                           
136900                                                                          
137000     MOVE JA                       TO ARTIKEL-SW                          
137100     MOVE ZERO                     TO W-ACK-KVKOLLI                       
137200                                      W-ACK-FLKLAR                        
137300                                      W-IDFS                              
137400                                      W-TIAVIDAT                          
137500                                      W-IDARTNR                           
137600     MOVE SPACE                    TO W-IDLEVNR                           
137700     PERFORM UNTIL ARTIKEL-SAKNAS                                         
137800       IF A-6110-IDLEVNR = W-IDLEVNR AND                                  
137900          A-6110-IDFS    = W-IDFS    AND                                  
138000          A-6110-TIAVIDAT = W-TIAVIDAT AND                                
138100          A-6110-IDARTNR  = W-IDARTNR                                     
138200                                                                          
138300          PERFORM IMS-GNP-INLA-D111-ARTNR                                 
138400          IF SEGMENT-SAKNAS                                               
138500            MOVE NEJ  TO ARTIKEL-SW                                       
138600          ELSE                                                            
138700            ADD ART-KVAVIS TO SPAR-KVAVIS-TOT                             
138800            MOVE ART-IDRADNR-INL TO W-IDRADNR-INL                         
138900          END-IF                                                          
139000       ELSE                                                               
139100          MOVE A-6110-IDLEVNR           TO W-IDLEVNR                      
139200          MOVE A-6110-IDFS              TO W-IDFS                         
139300          MOVE A-6110-TIAVIDAT          TO W-TIAVIDAT                     
139400          MOVE A-6110-IDARTNR           TO W-IDARTNR                      
139500                                                                          
139600          PERFORM IMS-GU-INLA-D101                                        
139700          IF SEGMENT-FINNS                                                
139800            PERFORM IMS-GNP-INLA-D111-ARTNR                               
139900            IF SEGMENT-FINNS                                              
140000              PERFORM S20-FLYTTA-INLA11-SPAR                              
140100              MOVE ART-IDRADNR-INL TO W-IDRADNR-INL                       
140200                                                                          
140300            ELSE                                                          
140400              MOVE NEJ  TO ARTIKEL-SW                                     
140500            END-IF                                                        
140600          ELSE                                                            
140700            MOVE NEJ  TO ARTIKEL-SW                                       
140800          END-IF                                                          
140900       END-IF                                                             
141000       IF ARTIKEL-FINNS                                                   
141100         PERFORM FAAC-LAES-INLA-D121                                      
141200       END-IF                                                             
141300     END-PERFORM                                                          
141400     .                                                                    
141500     EJECT                                                                
141600*----------------------------------------------------------------*        
141700 FAAC-LAES-INLA-D121    SECTION.                                          
141800                                                                          
141900     IF SEGMENT-FINNS                                                     
142000        PERFORM IMS-GNP-INLA-D121-RADNR-INL                               
142100                                                                          
142200        PERFORM UNTIL INLA-STATUS-CODE = 'GE'                             
142300           IF SEGMENT-FINNS                                               
142400              IF (RAD-KDINLSTA = 'FPK' OR SPACE) AND                      
142500                 RAD-ADINLOMR NOT = SPACE                                 
142600                 CONTINUE                                                 
142700              ELSE                                                        
142800                 ADD 1 TO W-ACK-FLKLAR                                    
142900              END-IF                                                      
143000              IF RAD-IDOKOLLI > ZERO                                      
143100                 ADD 1 TO W-ACK-KVKOLLI                                   
143200              END-IF                                                      
143300           PERFORM IMS-GNP-INLA-D121-RADNR-INL                            
143400           END-IF                                                         
143500        END-PERFORM                                                       
143600     END-IF                                                               
143700     .                                                                    
143800     EJECT                                                                
143900*----------------------------------------------------------------*        
144000 FAB-LAES-G121    SECTION.                                                
144100                                                                          
144200     IF NKLTYP1                                                           
144300        PERFORM IMS-GNP-LASA-G121-OKVAL                                   
144400     ELSE                                                                 
144500        IF NKLTYP2                                                        
144600           PERFORM IMS-GNP-LASA-G121-SOEK1                                
144700        ELSE                                                              
144800           PERFORM IMS-GNP-LASA-G121-SOEK2                                
144900        END-IF                                                            
145000     END-IF                                                               
145100     .                                                                    
145200     EJECT                                                                
145300*----------------------------------------------------------------*        
145400 G-KOLLA-INPUT SECTION.                                                   
145500                                                                          
145600     MOVE ZERO               TO FELRAKN                                   
145700     MOVE JA                 TO INDATA-SW                                 
145800     PERFORM GC-KOLLA-INPUT                                               
145900                                                                          
146000     IF INDATA-OK                                                         
146100                                                                          
146200       MOVE ZERO    TO WS-COUNT                                           
146300       PERFORM                                                            
146400       VARYING IX-X FROM 1 BY 1                                           
146500         UNTIL IX-X > REQU-KVRADER                                        
146600         IF REQU-KDCMDVAL-INPUT-LINE (IX-X) = ALL '+' OR                  
146700            REQU-KDCMDVAL-INPUT-LINE (IX-X) = SPACE                       
146800           ADD +1   TO WS-COUNT                                           
146900         END-IF                                                           
147000       END-PERFORM                                                        
147100                                                                          
147200        IF WS-COUNT = REQU-KVRADER                                        
147300           PERFORM GA-KOLLA-BILKLAR                                       
147400        ELSE                                                              
147500           MOVE ZERO    TO WS-COUNT                                       
147600           PERFORM                                                        
147700           VARYING IX-X FROM 1 BY 1                                       
147800             UNTIL IX-X > REQU-KVRADER                                    
147900             IF REQU-KDCMDVAL-INPUT-LINE (IX-X) NOT = ALL '+' OR          
148000                REQU-KDCMDVAL-INPUT-LINE (IX-X) NOT = SPACE               
148100               ADD +1   TO WS-COUNT                                       
148200             END-IF                                                       
148300           END-PERFORM                                                    
148400                                                                          
148500           IF WS-COUNT = REQU-KVRADER                                     
148600              IF REQU-FLKLAR-BIL = JA OR YES                              
148700*-FEL - 002                                                               
148800                 MOVE NEJ                TO INDATA-SW                     
148900                 MOVE ERR-CONFLICT       TO RESP-IDMSG-ERROR              
149000                 MOVE MFS-ALFA-FAELT-FEL                                  
149100                                         TO RESP-FLKLAR-BIL-ATTR          
149200              ELSE                                                        
149300                 PERFORM GB-KOLLA-AETGAERDER                              
149400              END-IF                                                      
149500           END-IF                                                         
149600        END-IF                                                            
149700     END-IF                                                               
149800                                                                          
149900     IF INDATA-FEL                                                        
150000        PERFORM MFS-ROER-EJ-LAES-IN-FAELT                                 
150100     END-IF                                                               
150200     .                                                                    
150300     EJECT                                                                
150400*----------------------------------------------------------------*        
150500 GA-KOLLA-BILKLAR    SECTION.                                             
150600                                                                          
150700     IF (REQU-ADINLOMR = ALL '+') OR                                      
150800        (REQU-ADINLOMR = SPACE) OR                                        
150900        (REQU-ADINLOMR = LOW-VALUE)                                       
151000*-FEL - 730                                                               
151100*-------PLACERING OBL                                                     
151200        MOVE NEJ                       TO INDATA-SW                       
151300        MOVE ERR-WRONG-PLACE           TO RESP-IDMSG-ERROR                
151400        MOVE MFS-ALFA-FAELT-FEL        TO RESP-ADINLOMR-ATTR              
151500                                                                          
151600     ELSE                                                                 
151700        MOVE '6005'          TO WGX-IDHTYP                                
151800        MOVE REQU-ADINLOMR    TO W-ADINLOMR                               
151900        PERFORM IMS-GU-PLAA-G111                                          
152000        IF SEGMENT-SAKNAS                                                 
152100*-FEL - 764                                                               
152200*----------PLACERING SAKNAS                                               
152300           MOVE NEJ                    TO INDATA-SW                       
152400           MOVE ERR-PLACE-MISSING      TO RESP-IDMSG-ERROR                
152500           MOVE 'ADDRESS'              TO RESP-IDELMT-ERROR               
152600           MOVE MFS-ALFA-FAELT-FEL     TO RESP-ADINLOMR-ATTR              
152700        ELSE                                                              
152800           MOVE 6006-KDINLOMR          TO SPAR-KDINLOMR                   
152900        END-IF                                                            
153000     END-IF                                                               
153100                                                                          
153200     IF INDATA-OK                                                         
153300        IF REQU-TELOSSN1 NOT = SPACE OR                                   
153400           REQU-TELOSSN2 NOT = SPACE                                      
153500           IF REQU-ADINLOMR-PRT NOT = ALL '+' AND                         
153600              REQU-ADINLOMR-PRT NOT = SPACE AND                           
153700              REQU-ADINLOMR-PRT NOT = LOW-VALUE                           
153800              PERFORM S61-KOLLA-PRINTER                                   
153900           ELSE                                                           
154000*-FEL - 772                                                               
154100*-------------PRINTER-PLACERING OBL                                       
154200              MOVE NEJ                 TO INDATA-SW                       
154300              MOVE ERR-PRINT-SAKN      TO RESP-IDMSG-ERROR                
154400              MOVE MFS-ALFA-FAELT-FEL  TO RESP-ADINLOMR-PRT-ATTR          
154500           END-IF                                                         
154600        ELSE                                                              
154700           MOVE NEJ TO SAKNAT-SW                                          
154800           PERFORM GAA-KOLLA-OM-NGT-SAKNAS                                
154900           IF SAKNAT-PARTI                                                
155000             IF REQU-ADINLOMR = ALL '+'                                   
155100               MOVE NEJ                 TO INDATA-SW                      
155200               MOVE ERR-PRINT-SAKN      TO RESP-IDMSG-ERROR               
155300               MOVE MFS-ALFA-FAELT-FEL  TO RESP-ADINLOMR-PRT-ATTR         
155400             ELSE                                                         
155500               PERFORM S61-KOLLA-PRINTER                                  
155600             END-IF                                                       
155700           END-IF                                                         
155800        END-IF                                                            
155900     END-IF                                                               
156000     .                                                                    
156100     EJECT                                                                
156200*----------------------------------------------------------------*        
156300 GAA-KOLLA-OM-NGT-SAKNAS SECTION.                                         
156400                                                                          
156500     MOVE '6107'                   TO WL-IDHTYP                           
156600     MOVE RESP-IDLBBET-KEY         TO WL-IDLBBET                          
156700     PERFORM IMS-GU-LASA-G111                                             
156800     IF SEGMENT-FINNS                                                     
156900       PERFORM IMS-GNP-LASA-G121-OKVAL                                    
157000       PERFORM UNTIL SEGMENT-SAKNAS OR SAKNAT-PARTI                       
157100         IF A-6110-IDLEVNR NOT = W-IDLEVNR OR                             
157200            A-6110-IDFS    NOT = W-IDFS    OR                             
157300            A-6110-IDARTNR NOT = W-IDARTNR                                
157400           MOVE A-6110-IDLEVNR  TO W-IDLEVNR                              
157500           MOVE A-6110-IDFS     TO W-IDFS                                 
157600           MOVE A-6110-TIAVIDAT TO W-TIAVIDAT                             
157700           MOVE A-6110-IDARTNR  TO W-IDARTNR                              
157800           PERFORM IMS-GU-INLA-D111                                       
157900           IF SEGMENT-FINNS                                               
158000             PERFORM IMS-GNP-INLA-D121-OKVAL                              
158100             PERFORM UNTIL SEGMENT-SAKNAS OR SAKNAT-PARTI                 
158200               IF RAD-KDINLSTA = 'SAK'                                    
158300                 MOVE JA TO SAKNAT-SW                                     
158400               END-IF                                                     
158500               PERFORM IMS-GNP-INLA-D121-OKVAL                            
158600             END-PERFORM                                                  
158700           END-IF                                                         
158800         END-IF                                                           
158900         PERFORM IMS-GNP-LASA-G121-OKVAL                                  
159000       END-PERFORM                                                        
159100     ELSE                                                                 
159200       MOVE NEJ                 TO INDATA-SW                              
159300       MOVE ERR-WRONG-KEY       TO RESP-IDMSG-ERROR                       
159400       MOVE 'IDLBBET'           TO RESP-IDELMT-ERROR                      
159500     END-IF                                                               
159600     .                                                                    
159700     EJECT                                                                
159800*----------------------------------------------------------------*        
159900 GB-KOLLA-AETGAERDER SECTION.                                             
160000                                                                          
160100     MOVE ZERO TO IX                                                      
160200     ADD  1    TO IX                                                      
160300*    PERFORM UNTIL IX > MAX-KVRADER                                       
160400     PERFORM UNTIL IX > REQU-KVRADER                                      
160500        IF REQU-KDCMDVAL-INPUT-LINE(IX) NOT = ALL '+' AND                 
160600           REQU-KDCMDVAL-INPUT-LINE(IX) NOT = SPACE                       
160700                                                                          
160800           IF (REQU-KDCMDVAL-INPUT-LINE(IX) = 'EMB' OR                    
160900                                     'SAK' OR 'PK ' OR                    
161000                                     'TP ' OR 'MIS' OR 'BR ')             
161100              PERFORM GBA-KONTR-VAL-UPD-OK                                
161200              IF INDATA-OK                                                
161300                 MOVE JA     TO INDATA-SW                                 
161400                 MOVE MFS-ALFA-FAELT-RAETT                                
161500                         TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)             
161600              END-IF                                                      
161700           ELSE                                                           
161800*-FEL - 416                                                               
161900              MOVE NEJ               TO INDATA-SW                         
162000              IF FELRAKN = 0                                              
162100                 MOVE ERR-CODE-NOT-VALID TO RESP-IDMSG-ERROR              
162200*                MOVE 'KDCMDVAL'         TO RESP-IDELMT-ERROR             
162300              END-IF                                                      
162400              ADD 1     TO FELRAKN                                        
162500              MOVE MFS-ALFA-FAELT-FEL                                     
162600                      TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                
162700           END-IF                                                         
162800        END-IF                                                            
162900        ADD 1  TO IX                                                      
163000     END-PERFORM                                                          
163100     .                                                                    
163200     EJECT                                                                
163300*----------------------------------------------------------------*        
163400 GBA-KONTR-VAL-UPD-OK SECTION.                                            
163500                                                                          
163600     PERFORM GBAC-KONTR-LASA                                              
163700                                                                          
163800     IF INDATA-OK                                                         
163900        IF REQU-KDCMDVAL-INPUT-LINE(IX) = 'PK ' OR 'BR '                  
164000           PERFORM GBAA-KONTR-VAL-PK                                      
164100        END-IF                                                            
164200        IF REQU-KDCMDVAL-INPUT-LINE(IX) = 'SAK' OR 'MIS'                  
164300           PERFORM GBAB-KONTR-VAL-SAK                                     
164400        END-IF                                                            
164500     END-IF                                                               
164600     .                                                                    
164700     EJECT                                                                
164800*----------------------------------------------------------------*        
164900 GBAA-KONTR-VAL-PK SECTION.                                               
165000                                                                          
165100     IF REQU-ADINLOMR NOT = ALL '+' AND                                   
165200        REQU-ADINLOMR NOT = SPACE AND                                     
165300        REQU-ADINLOMR NOT = LOW-VALUE                                     
165400        MOVE '6005'          TO WGX-IDHTYP                                
165500        MOVE REQU-ADINLOMR    TO W-ADINLOMR                               
165600        PERFORM IMS-GU-PLAA-G111                                          
165700        IF SEGMENT-SAKNAS                                                 
165800*-FEL - 001                                                               
165900*----------PLACERING SAKNAS                                               
166000           MOVE NEJ                     TO INDATA-SW                      
166100           IF FELRAKN = 0                                                 
166200              MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR               
166300           END-IF                                                         
166400           ADD 1                        TO FELRAKN                        
166500           MOVE MFS-ALFA-FAELT-FEL      TO RESP-ADINLOMR-ATTR             
166600*                             RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)           
166700        END-IF                                                            
166800     ELSE                                                                 
166900*-FEL - 001                                                               
167000*-------PLACERING OBL                                                     
167100        MOVE NEJ                        TO INDATA-SW                      
167200        IF FELRAKN = 0                                                    
167300           MOVE ERR-CORR-HILITE-FLDS    TO RESP-IDMSG-ERROR               
167400        END-IF                                                            
167500        ADD 1                           TO FELRAKN                        
167600        MOVE MFS-ALFA-FAELT-FEL         TO RESP-ADINLOMR-ATTR             
167700*                             RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)           
167800     END-IF                                                               
167900     .                                                                    
168000     EJECT                                                                
168100*----------------------------------------------------------------*        
168200 GBAB-KONTR-VAL-SAK SECTION.                                              
168300                                                                          
168400*----SEGMENTEN I INLA MÅSTE LÄSAS FÖR ATT KUNNA KOLLAS                    
168500                                                                          
168600     IF SEGMENT-FINNS                                                     
168700        PERFORM GBABA-LAES-INLA-D111-D121                                 
168800                                                                          
168900        IF SAK-RAKN > ZERO                                                
169000*-FEL - 007                                                               
169100           MOVE NEJ                      TO INDATA-SW                     
169200           IF FELRAKN = 0                                                 
169300              MOVE ERR-UPDATE-NOT-VALID  TO RESP-IDMSG-ERROR              
169400           END-IF                                                         
169500           ADD 1 TO FELRAKN                                               
169600           MOVE MFS-ALFA-FAELT-FEL                                        
169700                   TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                   
169800        END-IF                                                            
169900     END-IF                                                               
170000     .                                                                    
170100     EJECT                                                                
170200*----------------------------------------------------------------*        
170300 GBABA-LAES-INLA-D111-D121 SECTION.                                       
170400                                                                          
170500*----LÄSNING FÖR ATT KOLLA ALLA 21-RADER                                  
170600*----MAP ÅTG-KOD=SAK SAMT FLKLAR                                          
170700*    NKL = FYS , ARTNR ,RADNR > 0                                         
170800                                                                          
170900     MOVE A-6110-IDLEVNR       TO W-IDLEVNR                               
171000     MOVE A-6110-IDFS          TO W-IDFS                                  
171100     MOVE A-6110-TIAVIDAT      TO W-TIAVIDAT                              
171200     MOVE A-6110-IDARTNR       TO W-IDARTNR                               
171300     MOVE ZERO                 TO W-IDRADNR                               
171400                                  SAK-RAKN                                
171500     PERFORM IMS-GU-INLA-D111                                             
171600     PERFORM UNTIL SEGMENT-SAKNAS                                         
171700       PERFORM IMS-GNP-INLA-D121                                          
171800       PERFORM UNTIL SEGMENT-SAKNAS                                       
171900          IF (RAD-KDINLSTA = 'FPK' OR SPACE)                              
172000             CONTINUE                                                     
172100          ELSE                                                            
172200             ADD 1    TO SAK-RAKN                                         
172300          END-IF                                                          
172400          MOVE RAD-IDRADNR          TO W-IDRADNR                          
172500          PERFORM IMS-GNP-INLA-D121                                       
172600       END-PERFORM                                                        
172700       PERFORM IMS-GN-INLA-D111                                           
172800     END-PERFORM                                                          
172900     .                                                                    
173000     EJECT                                                                
173100*----------------------------------------------------------------*        
173200 GBAC-KONTR-LASA  SECTION.                                                
173300                                                                          
173400*----SEGMENTEN    MÅSTE LÄSAS FÖR ATT KUNNA KOLLAS                        
173500     PERFORM GBACA-LAES-LASA                                              
173600                                                                          
173700     IF A-6110-FLKLAR = JA OR SEGMENT-SAKNAS                              
173800*-FEL - 007                                                               
173900        MOVE NEJ                   TO INDATA-SW                           
174000        IF FELRAKN = 0                                                    
174100           MOVE ERR-UPDATE-NOT-VALID  TO RESP-IDMSG-ERROR                 
174200        END-IF                                                            
174300        ADD 1 TO FELRAKN                                                  
174400        MOVE MFS-ALFA-FAELT-FEL                                           
174500                TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                      
174600     END-IF                                                               
174700     .                                                                    
174800     EJECT                                                                
174900*----------------------------------------------------------------*        
175000 GBACA-LAES-LASA           SECTION.                                       
175100                                                                          
175200*----LÄSNING FÖR ATT HÄMTA TIAVIDAT FRÅN LASA                             
175300                                                                          
175400     MOVE '6107'                   TO WL-IDHTYP                           
175500     MOVE RESP-IDLBBET-KEY         TO WL-IDLBBET                          
175600     PERFORM IMS-GU-LASA-G111                                             
175700                                                                          
175800     IF SEGMENT-FINNS                                                     
175900                                                                          
176000        MOVE REQU-IDARTNR-LINE(IX) TO WS-RED-IDARTNR                      
176100        INSPECT WS-IDARTNR-N REPLACING LEADING SPACE BY ZERO              
176200        MOVE WS-IDARTNR-N          TO WLS2-IDARTNR                        
176300                                                                          
176400        MOVE REQU-IDLEVNR-LINE(IX) TO WS-IDLEVNR-X                        
176500        MOVE WS-IDLEVNR            TO WLS2-IDLEVNR                        
176600                                                                          
176700        MOVE REQU-IDFS-LINE(IX)    TO WLS2-IDFS                           
176800                                                                          
176900        PERFORM IMS-GNP-LASA-G121-SOEK2                                   
177000     END-IF                                                               
177100     .                                                                    
177200     EJECT                                                                
177300*----------------------------------------------------------------*        
177400 GC-KOLLA-INPUT  SECTION.                                                 
177500                                                                          
177600     MOVE ZERO    TO WS-COUNT                                             
177700     PERFORM                                                              
177800     VARYING IX-X FROM 1 BY 1                                             
177900       UNTIL IX-X > REQU-KVRADER                                          
178000       IF REQU-KDCMDVAL-INPUT-LINE (IX-X) = ALL '+' OR                    
178100          REQU-KDCMDVAL-INPUT-LINE (IX-X) = SPACE   OR                    
178200          REQU-KDCMDVAL-INPUT-LINE (IX-X) = LOW-VALUE                     
178300         ADD +1   TO WS-COUNT                                             
178400       END-IF                                                             
178500     END-PERFORM                                                          
178600                                                                          
178700     IF WS-COUNT = REQU-KVRADER                                           
178800        IF REQU-FLKLAR-BIL = NEJ                                          
178900           MOVE JA                   TO PLACERINGS-SW                     
179000        END-IF                                                            
179100     END-IF                                                               
179200     IF (REQU-FLKLAR-BIL = JA OR                                          
179300         REQU-FLKLAR-BIL = YES OR                                         
179400         REQU-FLKLAR-BIL = NEJ)                                           
179500*----------OK                                                             
179600        CONTINUE                                                          
179700     ELSE                                                                 
179800*-FEL - 001                                                               
179900*-------FEL FLKLAR EJ J/N                                                 
180000        MOVE NEJ                     TO INDATA-SW                         
180100        MOVE MFS-ALFA-FAELT-FEL      TO RESP-FLKLAR-BIL-ATTR              
180200        MOVE ERR-CORR-HILITE-FLDS    TO RESP-IDMSG-ERROR                  
180300     END-IF                                                               
180400     .                                                                    
180500     EJECT                                                                
180600******************************************************************        
180700*  UPPDATERA REGISTER                                            *        
180800******************************************************************        
180900*----------------------------------------------------------------*        
181000 H-UPPDATERA SECTION.                                                     
181100                                                                          
181200     MOVE ZERO       TO T91-MID-KVPOST                                    
181300     MOVE NEJ        TO TRANS91-SW                                        
181400     MOVE JA         TO ALLT-SW                                           
181500     MOVE JA         TO FOERSTA-T91-SW                                    
181600                                                                          
181700     IF REQU-FLKLAR-BIL = JA OR YES OR                                    
181800        PLAC-AENDRING                                                     
181900        PERFORM HA-LOSSNINGS-INFO-2                                       
182000     ELSE                                                                 
182100        PERFORM HB-LOSSNINGS-INFO-1                                       
182200     END-IF                                                               
182300                                                                          
182400     IF TRANS91-JA                                                        
182500*-------SKICKA TRANS TILL BAKGRUNDS-MPP                                   
182600        PERFORM S50-SKICKA-W60191                                         
182700     END-IF                                                               
182800     .                                                                    
182900     EJECT                                                                
183000*----------------------------------------------------------------*        
183100 HA-LOSSNINGS-INFO-2 SECTION.                                             
183200                                                                          
183300     MOVE '6107'                   TO WL-IDHTYP                           
183400     MOVE RESP-IDLBBET-KEY         TO WL-IDLBBET                          
183500     PERFORM IMS-GHU-LASA-G111                                            
183600                                                                          
183700     MOVE ZERO                     TO SPAR-P-IDARTNR                      
183800                                      SPAR-P-TIAVIDAT                     
183900     MOVE SPACE                    TO SPAR-P-IDFS                         
184000                                      SPAR-P-IDLEVNR                      
184100     MOVE NEJ                      TO SAKNAT-SW                           
184200     IF PLAC-AENDRING                                                     
184300       MOVE REQU-ADINLOMR          TO B-6108-ADINLOMR-LPL                 
184400       PERFORM IMS-REPL-LASA                                              
184500     END-IF                                                               
184600                                                                          
184700     PERFORM IMS-GHNP-LASA-G121-OKVAL                                     
184800*----LÄS ALLA LASA-RADER-21-SEGM                                          
184900     PERFORM UNTIL SEGMENT-SAKNAS                                         
185000        IF SPAR-P-IDARTNR  = B-6110-IDARTNR AND                           
185100           SPAR-P-IDLEVNR  = B-6110-IDLEVNR AND                           
185200           SPAR-P-IDFS     = B-6110-IDFS    AND                           
185300           SPAR-P-TIAVIDAT = B-6110-TIAVIDAT                              
185400*----------SAMMA IGEN - LÄS NÄSTA                                         
185500           CONTINUE                                                       
185600        ELSE                                                              
185700*----------NYTT PARTI                                                     
185800           PERFORM HAA-BEARB-INLA                                         
185900           MOVE B-6110-IDARTNR     TO SPAR-P-IDARTNR                      
186000           MOVE B-6110-IDLEVNR     TO SPAR-P-IDLEVNR                      
186100           MOVE B-6110-IDFS        TO SPAR-P-IDFS                         
186200           MOVE B-6110-TIAVIDAT    TO SPAR-P-TIAVIDAT                     
186300        END-IF                                                            
186400        PERFORM IMS-GHNP-LASA-G121-OKVAL                                  
186500     END-PERFORM                                                          
186600                                                                          
186700     IF REQU-IDMSGVER = '101'                                             
186800*      -- THIS IS ONLY APPLICABLE IN A IMS CLASSIC CALL                   
186900       IF REQU-TELOSSN1 NOT = SPACE OR                                    
187000          REQU-TELOSSN2 NOT = SPACE OR SAKNAT-PARTI                       
187100          PERFORM S70-LISTA-W6012111                                      
187200          MOVE SPACE                   TO RESP-TELOSSN1                   
187300          MOVE SPACE                   TO RESP-TELOSSN2                   
187400       END-IF                                                             
187500     END-IF                                                               
187600                                                                          
187700     MOVE '6107'                   TO WL-IDHTYP                           
187800     MOVE RESP-IDLBBET-KEY         TO WL-IDLBBET                          
187900                                                                          
188000     IF REQU-FLKLAR-BIL = JA OR YES                                       
188100       PERFORM IMS-GHU-LASA-G111                                          
188200       PERFORM IMS-DLET-LASA                                              
188300     END-IF                                                               
188400     MOVE JA TO LASTB-SW                                                  
188500     .                                                                    
188600     EJECT                                                                
188700*----------------------------------------------------------------*        
188800 HAA-BEARB-INLA      SECTION.                                             
188900                                                                          
189000*----LÄS OCH BEARBETA INLA-RADER-21-SEGM                                  
189100     PERFORM S64-LAES-INLA-D111                                           
189200                                                                          
189300     MOVE ART-ADLAGOMR      TO SPAR-ADLAGOMR-N                            
189400     MOVE SPACE             TO SPAR-KDINLSTA-OLD                          
189500                               SPAR-ADINLOMR-OLD                          
189600                               SPAR-ADINLOMR-NXT-OLD                      
189700     MOVE NEJ               TO T91-RAD-SW                                 
189800     PERFORM HAAA-LAES-PLAA                                               
189900     PERFORM UNTIL INLA-STATUS-CODE NOT = SPACE                           
190000        PERFORM IMS-GHNP-INLA-D121                                        
190100        MOVE NEJ                       TO T93-RAD-SW                      
190200        PERFORM UNTIL SEGMENT-SAKNAS                                      
190300           IF RAD-IDILIST      = +0                                       
190400             IF B-6110-FLKLAR = NEJ                                       
190500                MOVE RAD-ADINLOMR     TO SPAR-ADINLOMR-OLD                
190600                MOVE REQU-ADINLOMR    TO RAD-ADINLOMR                     
190700                MOVE RAD-ADINLOMR-NXT TO SPAR-ADINLOMR-NXT-OLD            
190800                MOVE JA               TO T91-RAD-SW                       
190900             END-IF                                                       
191000             IF PLAA-STATUS-CODE = SPACE AND                              
191100                6006-KDLORAPP = 4                                         
191200                IF RAD-IDRADNR = +1                                       
191300                  PERFORM HAAB-LAEGG-IN                                   
191400                ELSE                                                      
191500                  MOVE RAD-KDINLSTA     TO SPAR-KDINLSTA-OLD              
191600                  MOVE 'INL'            TO RAD-KDINLSTA                   
191700                  MOVE RAD-ADINLOMR     TO SPAR-ADINLOMR-OLD              
191800                  MOVE RAD-ADINLOMR-NXT TO SPAR-ADINLOMR-NXT-OLD          
191900                  MOVE SPACE            TO RAD-ADINLOMR                   
192000                                           RAD-ADINLOMR-NXT               
192100                  MOVE JA               TO T91-RAD-SW                     
192200                  MOVE JA               TO T93-RAD-SW                     
192300               END-IF                                                     
192400             END-IF                                                       
192500           ELSE                                                           
192600              IF RAD-KDINLSTA = 'SAK'                                     
192700                MOVE JA TO SAKNAT-SW                                      
192800              END-IF                                                      
192900           END-IF                                                         
193000           IF T91-RAD-JA                                                  
193100              PERFORM IMS-REPL-INLA                                       
193200*-------------SKICKA TRANS TILL BAKGRUNDS-MPP ' W60191'                   
193300              IF T91-MID-KVPOST = 24                                      
193400                 PERFORM S50-SKICKA-W60191                                
193500                 MOVE ZERO          TO T91-MID-KVPOST                     
193600              END-IF                                                      
193700              PERFORM S40-TRANS-W60191                                    
193800           END-IF                                                         
193900*----------LÄS NXT INLA-RADER-21-SEGM                                     
194000           MOVE RAD-IDRADNR           TO W-IDRADNR                        
194100           MOVE NEJ                   TO T91-RAD-SW                       
194200           PERFORM IMS-GHNP-INLA-D121                                     
194300           MOVE SPACE                 TO SPAR-KDINLSTA-OLD                
194400                                         SPAR-ADINLOMR-OLD                
194500                                         SPAR-ADINLOMR-NXT-OLD            
194600        END-PERFORM                                                       
194700        IF T93-RAD-JA                                                     
194800*----------SKICKA TRANS TILL BAKGRUNDS-MPP ' W60193'                      
194900           MOVE SPAR-IDLOPNRM      TO W-IDLOPNRM                          
195000           MOVE W-RED-IDLOPNRM     TO T93-MID-IDLOPNRM                    
195100           MOVE ZERO               TO T93-MID-IDRADNR                     
195200           PERFORM S51-SKICKA-W60193                                      
195300        END-IF                                                            
195400        PERFORM IMS-GN-INLA-D111                                          
195500        IF SEGMENT-FINNS                                                  
195600          MOVE ART-IDLOPNRM TO SPAR-IDLOPNRM                              
195700          MOVE +0 TO W-IDRADNR                                            
195800        END-IF                                                            
195900     END-PERFORM                                                          
196000     .                                                                    
196100     EJECT                                                                
196200*----------------------------------------------------------------*        
196300 HAAA-LAES-PLAA      SECTION.                                             
196400                                                                          
196500     MOVE '6005'             TO WGX-IDHTYP                                
196600     MOVE SPAR-RED-ADLAGOMR  TO W-ADINLOMR                                
196700                                                                          
196800     PERFORM IMS-GU-PLAA-G111                                             
196900     .                                                                    
197000     EJECT                                                                
197100*----------------------------------------------------------------*        
197200 HAAB-LAEGG-IN    SECTION.                                                
197300                                                                          
197400     MOVE +1      TO W-IDRADNR-ALT                                        
197500     PERFORM IMS-GHU-ALT-INLA-D121                                        
197600     PERFORM IMS-DLET-ALT-INLA                                            
197700     IF T91-MID-KVPOST = 24                                               
197800        PERFORM S50-SKICKA-W60191                                         
197900        MOVE ZERO          TO T91-MID-KVPOST                              
198000     END-IF                                                               
198100     PERFORM S41-TRANS-W60191-DLET                                        
198200                                                                          
198300     PERFORM IMS-GU-ALT-INLA-D121-LAST                                    
198400     IF SEGMENT-SAKNAS                                                    
198500       MOVE +2 TO WS-IDRADNR                                              
198600     ELSE                                                                 
198700       COMPUTE WS-IDRADNR = ALT-RAD-IDRADNR + 1                           
198800     END-IF                                                               
198900     MOVE IO-AREA-1 TO IO-AREA-12                                         
199000     MOVE WS-IDRADNR TO ALT-RAD-IDRADNR                                   
199100     MOVE 'INL'      TO ALT-RAD-KDINLSTA                                  
199200     MOVE SPACE      TO ALT-RAD-ADINLOMR-NXT                              
199300     MOVE ZERO       TO ALT-RAD-IDINLVGN                                  
199400                        ALT-RAD-IDILIST                                   
199500                        ALT-RAD-IDILIRAD                                  
199600                        ALT-RAD-TIUPPDAT                                  
199700     PERFORM IMS-ISRT-ALT-INLA-D121                                       
199800     IF T91-MID-KVPOST = 24                                               
199900        PERFORM S50-SKICKA-W60191                                         
200000        MOVE ZERO          TO T91-MID-KVPOST                              
200100     END-IF                                                               
200200     PERFORM S42-TRANS-W60191-ISRT                                        
200300     MOVE JA                  TO T93-RAD-SW                               
200400     MOVE NEJ                 TO T91-RAD-SW                               
200500     .                                                                    
200600     EJECT                                                                
200700*----------------------------------------------------------------*        
200800 HB-LOSSNINGS-INFO-1 SECTION.                                             
200900                                                                          
201000     MOVE ZERO       TO IX                                                
201100     ADD +1          TO IX                                                
201200     PERFORM UNTIL IX > REQU-KVRADER                                      
201300        IF REQU-KDCMDVAL-INPUT-LINE(IX) NOT = ALL '+' AND                 
201400           REQU-KDCMDVAL-INPUT-LINE(IX) NOT = SPACE AND                   
201500           REQU-KDCMDVAL-INPUT-LINE(IX) NOT = LOW-VALUE                   
201600                                                                          
201700           IF REQU-KDCMDVAL-INPUT-LINE(IX) = 'EMB' OR 'TP '               
201800              PERFORM HBA-LAES-LASA-G111                                  
201900              PERFORM HBB-UPD-VAL-EMB                                     
202000           END-IF                                                         
202100           IF REQU-KDCMDVAL-INPUT-LINE(IX) = 'SAK' OR 'MIS'               
202200              PERFORM HBC-UPD-VAL-SAK                                     
202300           END-IF                                                         
202400           IF REQU-KDCMDVAL-INPUT-LINE(IX) = 'PK ' OR 'BR '               
202500              PERFORM HBD-UPD-VAL-PK                                      
202600           END-IF                                                         
202700        END-IF                                                            
202800        MOVE ALL-SPACE            TO RESP-KDCMDVAL-INPUT-LINE(IX)         
202900        MOVE MFS-FORMATETS-ATTR                                           
203000                             TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)         
203100        ADD +1                    TO IX                                   
203200     END-PERFORM                                                          
203300     .                                                                    
203400     EJECT                                                                
203500*----------------------------------------------------------------*        
203600 HBA-LAES-LASA-G111 SECTION.                                              
203700                                                                          
203800     MOVE '6107'                   TO WL-IDHTYP                           
203900     MOVE RESP-IDLBBET-KEY         TO WL-IDLBBET                          
204000     PERFORM IMS-GHU-LASA-G111                                            
204100     .                                                                    
204200     EJECT                                                                
204300*----------------------------------------------------------------*        
204400 HBB-UPD-VAL-EMB SECTION.                                                 
204500                                                                          
204600     PERFORM HBBA-LAES-LASA-G121                                          
204700     PERFORM HBBB-LAES-INLA-D111                                          
204800                                                                          
204900*----INIT STYR                                                            
205000     MOVE W-IDDC               TO STYR-IDDC                               
205100     MOVE ART-IDARTNR          TO STYR-IDARTNR                            
205200     MOVE B-6110-IDLEVNR       TO STYR-IDLEVNR                            
205300     MOVE ART-IDFKNGRP         TO STYR-IDFKNGRP                           
205400     MOVE ART-BEFT             TO STYR-BEFT                               
205500     MOVE ART-IDLOPNRM         TO SPAR-IDLOPNRM                           
205600     MOVE ART-PRARTSTD         TO SPAR-PRARTSTD                           
205700*----SPARA FÖR ATT KUNNA LYSA UPP FÄLTEN                                  
205800     MOVE 'EMB'                TO WT-LYS-ATGKOD(IX)                       
205900     MOVE B-6110-IDLEVNR       TO WT-LYS-IDLEVNR(IX)                      
206000     MOVE B-6110-IDFS          TO WT-LYS-IDFS(IX)                         
206100     MOVE ART-IDARTNR          TO WT-LYS-IDARTNR(IX)                      
206200*----SPARA OCH INITIERA NYA VÄRDEN                                        
206300     MOVE B-6110-IDLEVNR       TO SP1-6110-IDLEVNR                        
206400     MOVE B-6110-IDFS          TO SP1-6110-IDFS                           
206500     MOVE ART-IDARTNR          TO SP1-6110-IDARTNR                        
206600     MOVE 1                    TO SP1-6110-KDSORT1                        
206700     MOVE NEJ                  TO SP1-6110-FLKLAR                         
206800     MOVE ZERO                 TO SP1-6110-KVAVIS                         
206900     MOVE ZERO                 TO SP1-6110-KVAVIS-PRIO                    
207000     MOVE ZERO                 TO SP1-6110-KVAVIS-KIT                     
207100     MOVE B-6110-TIAVIDAT      TO SP1-6110-TIAVIDAT                       
207200     MOVE ZERO                 TO SP1-6110-VLARTNTO                       
207300                                                                          
207400     PERFORM S30-CALL-STYR                                                
207500                                                                          
207600     IF STYR-KDSVAR-OK                                                    
207700        IF ART-BEFT > ZERO                                                
207800           IF STYR-ADINLOMR-FP NOT = SPACE                                
207900*-------------SPARA FB/FP-ADRESSEN                                        
208000*-------------KOLLA ÄVEN SEK+PRIM-BERÄKNING                               
208100              PERFORM HBBE-KOLLA-SEK-PRIM                                 
208200           ELSE                                                           
208300*-------------ALLT TILL FB-ADRESSEN                                       
208400              MOVE STYR-ADINLOMR-FB    TO SP1-6110-ADINLOMR               
208500              MOVE ART-KVAVIS          TO SP1-6110-KVAVIS                 
208600              MOVE ART-KVAVIS-KIT      TO SP1-6110-KVAVIS-KIT             
208700              MOVE ART-KVAVIS-PRIO     TO SP1-6110-KVAVIS-PRIO            
208800           END-IF                                                         
208900        ELSE                                                              
209000*----------ALLT TILL FB-ADRESSEN                                          
209100           MOVE STYR-ADINLOMR-FB       TO SP1-6110-ADINLOMR               
209200           MOVE ART-KVAVIS             TO SP1-6110-KVAVIS                 
209300           MOVE ART-KVAVIS-KIT         TO SP1-6110-KVAVIS-KIT             
209400           MOVE ART-KVAVIS-PRIO        TO SP1-6110-KVAVIS-PRIO            
209500        END-IF                                                            
209600     END-IF                                                               
209700                                                                          
209800*----LÄS OCH TA BORT ALLA GAMLA LASA-RADER                                
209900     PERFORM HBBC-BEARB-LASA                                              
210000*----SEDAN INSERT AV NYTT 21-SEGM                                         
210100     PERFORM HBBD-ISRT-NY-LASA                                            
210200     .                                                                    
210300     EJECT                                                                
210400*----------------------------------------------------------------*        
210500 HBBA-LAES-LASA-G121 SECTION.                                             
210600                                                                          
210700*----LÄS SEGMENT FÖR ATT HÄMTA TIAVIDAT FÖR LÄSNING AV INLA               
210800                                                                          
210900     MOVE REQU-IDARTNR-LINE(IX)    TO WS-RED-IDARTNR                      
211000     INSPECT WS-IDARTNR-N REPLACING LEADING SPACE BY ZERO                 
211100     MOVE WS-IDARTNR-N             TO WLS2-IDARTNR                        
211200                                                                          
211300     MOVE REQU-IDLEVNR-LINE(IX)    TO WS-IDLEVNR-X                        
211400     MOVE WS-IDLEVNR               TO WLS2-IDLEVNR                        
211500                                                                          
211600     MOVE REQU-IDFS-LINE(IX)       TO WLS2-IDFS                           
211700     PERFORM IMS-GHNP-LASA-G121-SOEK2                                     
211800     .                                                                    
211900     EJECT                                                                
212000*----------------------------------------------------------------*        
212100 HBBB-LAES-INLA-D111 SECTION.                                             
212200                                                                          
212300*----LÄS SEGMENT FÖR ATT FÅ PARAMETRAR TILL STYR-                         
212400                                                                          
212500     MOVE B-6110-IDLEVNR       TO W-IDLEVNR                               
212600     MOVE B-6110-IDFS          TO W-IDFS                                  
212700     MOVE B-6110-TIAVIDAT      TO W-TIAVIDAT                              
212800                                  WLS2-TIAVIDAT                           
212900     MOVE B-6110-IDARTNR       TO W-IDARTNR                               
213000     MOVE ZERO                 TO W-IDRADNR                               
213100                                  SAK-RAKN                                
213200                                                                          
213300     PERFORM IMS-GU-INLA-D111                                             
213400     .                                                                    
213500     EJECT                                                                
213600*----------------------------------------------------------------*        
213700 HBBC-BEARB-LASA SECTION.                                                 
213800                                                                          
213900*----LÄS SEGMENT FÖR BORTTAG AV GAMLA OCH TILLÄGG AV NY                   
214000*--- LÄS SAMTLIGA RADER 21-SEGM FÖR PARTIET                               
214100*--- GÖR BORTTAG AV ALLA RADER FÖR PARTIET                                
214200*----1:A GHNP GJORDES I HBBA-                                             
214300                                                                          
214400*----LÄS ALLA LASA-RADER-21-SEGM                                          
214500     PERFORM UNTIL SEGMENT-SAKNAS                                         
214600        PERFORM IMS-DLET-LASA                                             
214700        PERFORM IMS-GHNP-LASA-G121-SOEK2                                  
214800     END-PERFORM                                                          
214900     .                                                                    
215000     EJECT                                                                
215100*----------------------------------------------------------------*        
215200 HBBD-ISRT-NY-LASA SECTION.                                               
215300                                                                          
215400     MOVE SP1-6110-W6GX6110           TO B-6110-W6GX6110                  
215500     PERFORM IMS-ISRT-LASA                                                
215600     IF SEK-PRIM-JA                                                       
215700        MOVE SP2-6110-W6GX6110        TO B-6110-W6GX6110                  
215800        PERFORM IMS-ISRT-LASA                                             
215900     END-IF                                                               
216000     .                                                                    
216100     EJECT                                                                
216200*----------------------------------------------------------------*        
216300 HBBE-KOLLA-SEK-PRIM SECTION.                                             
216400                                                                          
216500     MOVE ZERO                         TO W-ACK-SEK-PRIM                  
216600     COMPUTE W-ACK-SEK-PRIM =                                             
216700             ART-KVKVAPRIM-BER + ART-KVKVASEK-BER                         
216800     IF W-ACK-SEK-PRIM > ZERO                                             
216900*-------1:A RAD TILL LASA                                                 
217000        MOVE +1                        TO SP1-6110-KDSORT1                
217100        MOVE W-ACK-SEK-PRIM            TO SP1-6110-KVAVIS                 
217200        MOVE STYR-ADINLOMR-FB          TO SP1-6110-ADINLOMR               
217300                                                                          
217400*-------2:A RAD TILL LASA                                                 
217500        MOVE SP1-6110-W6GX6110         TO SP2-6110-W6GX6110               
217600        MOVE +2                        TO SP2-6110-KDSORT1                
217700        COMPUTE SP2-6110-KVAVIS = ART-KVAVIS - W-ACK-SEK-PRIM             
217800        MOVE STYR-ADINLOMR-FP          TO SP2-6110-ADINLOMR               
217900        MOVE JA                        TO SEK-PRIM-SW                     
218000        MOVE ZERO                      TO SP1-6110-KVAVIS-PRIO            
218100        MOVE ZERO                      TO SP2-6110-KVAVIS-KIT             
218200     ELSE                                                                 
218300        MOVE NEJ                       TO SEK-PRIM-SW                     
218400        MOVE STYR-ADINLOMR-FP          TO SP1-6110-ADINLOMR               
218500        MOVE ART-KVAVIS                TO SP1-6110-KVAVIS                 
218600        MOVE ART-KVAVIS-KIT            TO SP1-6110-KVAVIS-KIT             
218700        MOVE ART-KVAVIS-PRIO           TO SP1-6110-KVAVIS-PRIO            
218800     END-IF                                                               
218900     .                                                                    
219000     EJECT                                                                
219100*----------------------------------------------------------------*        
219200 HBC-UPD-VAL-SAK SECTION.                                                 
219300                                                                          
219400                                                                          
219500*----LÄS OCH BEARBETA PARTIET PÅ LASA - ALLA RADER                        
219600     PERFORM S63-LAES-LASA-G111                                           
219700                                                                          
219800*----LÄS INLA-RADER-21-SEGM                                               
219900     PERFORM S64-LAES-INLA-D111                                           
220000                                                                          
220100     PERFORM UNTIL SEGMENT-SAKNAS                                         
220200        PERFORM IMS-GHNP-INLA-D121                                        
220300        PERFORM UNTIL SEGMENT-SAKNAS                                      
220400           MOVE RAD-KDINLSTA          TO SPAR-KDINLSTA-OLD                
220500           MOVE 'SAK'                 TO RAD-KDINLSTA                     
220600           MOVE RAD-ADINLOMR          TO SPAR-ADINLOMR-OLD                
220700           MOVE RAD-ADINLOMR-NXT TO SPAR-ADINLOMR-NXT-OLD                 
220800           PERFORM IMS-REPL-INLA                                          
220900*----------SKICKA TRANS TILL BAKGRUNDS-MPP ' W60191'                      
221000           IF T91-MID-KVPOST = 24                                         
221100              PERFORM S50-SKICKA-W60191                                   
221200              MOVE ZERO             TO T91-MID-KVPOST                     
221300           END-IF                                                         
221400           PERFORM S40-TRANS-W60191                                       
221500*----------LÄS NXT INLA-RADER-21-SEGM                                     
221600           MOVE RAD-IDRADNR           TO W-IDRADNR                        
221700           PERFORM IMS-GHNP-INLA-D121                                     
221800        END-PERFORM                                                       
221900        PERFORM IMS-GN-INLA-D111                                          
222000        IF SEGMENT-FINNS                                                  
222100          MOVE ART-IDLOPNRM TO SPAR-IDLOPNRM                              
222200          MOVE +0           TO W-IDRADNR                                  
222300        END-IF                                                            
222400     END-PERFORM                                                          
222500     .                                                                    
222600     EJECT                                                                
222700*----------------------------------------------------------------*        
222800 HBD-UPD-VAL-PK  SECTION.                                                 
222900                                                                          
223000                                                                          
223100*----LÄS OCH BEARBETA PARTIET PÅ LASA - ALLA RADER                        
223200     PERFORM S63-LAES-LASA-G111                                           
223300                                                                          
223400*----LÄS OCH BEARBETA INLA-RADER-21-SEGM                                  
223500     PERFORM S64-LAES-INLA-D111                                           
223600                                                                          
223700     PERFORM UNTIL SEGMENT-SAKNAS                                         
223800        PERFORM IMS-GHNP-INLA-D121                                        
223900        PERFORM UNTIL SEGMENT-SAKNAS                                      
224000           IF (RAD-KDINLSTA = 'FPK' OR SPACE) AND                         
224100              RAD-IDINLVGN = 0                                            
224200              MOVE RAD-KDINLSTA       TO SPAR-KDINLSTA-OLD                
224300              MOVE RAD-ADINLOMR       TO SPAR-ADINLOMR-OLD                
224400              MOVE REQU-ADINLOMR      TO RAD-ADINLOMR                     
224500              MOVE RAD-ADINLOMR-NXT   TO SPAR-ADINLOMR-NXT-OLD            
224600              PERFORM IMS-REPL-INLA                                       
224700*-------------SKICKA TRANS TILL BAKGRUNDS-MPP ' W60191'                   
224800              IF T91-MID-KVPOST = 24                                      
224900                 PERFORM S50-SKICKA-W60191                                
225000                 MOVE ZERO          TO T91-MID-KVPOST                     
225100              END-IF                                                      
225200              PERFORM S40-TRANS-W60191                                    
225300           END-IF                                                         
225400*----------LÄS NXT INLA-RADER-21-SEGM                                     
225500           MOVE RAD-IDRADNR           TO W-IDRADNR                        
225600           PERFORM IMS-GHNP-INLA-D121                                     
225700        END-PERFORM                                                       
225800        PERFORM IMS-GN-INLA-D111                                          
225900        IF SEGMENT-FINNS                                                  
226000          MOVE ART-IDLOPNRM TO SPAR-IDLOPNRM                              
226100          MOVE +0           TO W-IDRADNR                                  
226200        END-IF                                                            
226300     END-PERFORM                                                          
226400     .                                                                    
226500     EJECT                                                                
226600******************************************************************        
226700*    MFS-REDIGERING AV BILDENS FÄLT                              *        
226800******************************************************************        
226900 MFS-FORM-ATTR SECTION.                                                   
227000                                                                          
227100     MOVE +1 TO IX                                                        
227200     PERFORM UNTIL IX > MAX-KVRADER                                       
227300       MOVE MFS-FORMATETS-ATTR TO                                         
227400               RESP-KDCMDVAL-INPUT-LINE-ATTR (IX)                         
227500               RESP-IDLEVNR-LINE-ATTR (IX)                                
227600               RESP-IDFS-LINE-ATTR (IX)                                   
227700               RESP-IDARTNR-LINE-ATTR (IX)                                
227800               RESP-KVAVIS-TOT-LINE-ATTR(IX)                              
227900               RESP-KVKOLLI-LINE-ATTR(IX)                                 
228000               RESP-KDLAGEMB-LINE-ATTR (IX)                               
228100               RESP-BEFT-LINE-ATTR(IX)                                    
228200               RESP-ADINLOMR-NXT-LINE-ATTR(IX)                            
228300               RESP-KVAVIS-LINE-ATTR (IX)                                 
228400               RESP-KVAVIS-PRIO-LINE-ATTR (IX)                            
228500               RESP-KVAVIS-KIT-LINE-ATTR (IX)                             
228600               RESP-ADTRDEST-KIT-LINE-ATTR (IX)                           
228700        ADD +1 TO IX                                                      
228800     END-PERFORM                                                          
228900     .                                                                    
229000                                                                          
229100*----------------------------------------------------------------*        
229200 MFS-RENSA-FAELT-UT SECTION.                                              
229300                                                                          
229400*    --- ALLA UTDATA-FÄLT                                                 
229500*    --- INKL. BLÄDDRINGSNYCKLAR - SPAR-FÄLT                              
229600                                                                          
229700     MOVE ALL-SPACE          TO RESP-TELOSSN1                             
229800                                RESP-TELOSSN2                             
229900                                RESP-ADINLOMR                             
230000                                RESP-IDARTNR-START                        
230100                                RESP-IDLEVNR-START                        
230200                                RESP-IDFS-START                           
230300                                RESP-IDARTNR-NEXT                         
230400                                RESP-IDLEVNR-NEXT                         
230500                                RESP-IDFS-NEXT                            
230600                                RESP-NEXT                                 
230700                                                                          
230800*--- RENSA INDEXERADE RADER                                               
230900                                                                          
231000     MOVE +1 TO IX                                                        
231100     PERFORM UNTIL IX > MAX-KVRADER                                       
231200        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
231300        ADD +1 TO IX                                                      
231400     END-PERFORM                                                          
231500     .                                                                    
231600     SKIP2                                                                
231700     EJECT                                                                
231800*----------------------------------------------------------------*        
231900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
232000                                                                          
232100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
232200                                                                          
232300     MOVE ALL-SPACE          TO RESP-KDCMDVAL-INPUT-LINE(IX)              
232400                                RESP-IDLEVNR-LINE(IX)                     
232500                                RESP-IDFS-LINE(IX)                        
232600                                RESP-IDARTNR-LINE(IX)                     
232700                                RESP-KVAVIS-TOT-LINE(IX)                  
232800                                RESP-KVKOLLI-LINE(IX)                     
232900                                RESP-KDLAGEMB-LINE(IX)                    
233000                                RESP-BEFT-LINE(IX)                        
233100                                RESP-ADINLOMR-NXT-LINE(IX)                
233200                                RESP-KVAVIS-LINE(IX)                      
233300                                RESP-KVAVIS-PRIO-LINE(IX)                 
233400                                RESP-KVAVIS-KIT-LINE(IX)                  
233500                                RESP-ADTRDEST-KIT-LINE(IX)                
233600                                RESP-FLKVROS-LINE(IX)                     
233700                                RESP-FLKVAKAR-LINE(IX)                    
233800                                RESP-KDFARLIG-LINE(IX)                    
233900     .                                                                    
234000     EJECT                                                                
234100     SKIP2                                                                
234200*----------------------------------------------------------------*        
234300 MFS-ROER-EJ-LAES-IN-AETG  SECTION.                                       
234400                                                                          
234500     MOVE +1 TO IX                                                        
234600     PERFORM UNTIL IX > MAX-KVRADER                                       
234700        IF REQU-KDCMDVAL-INPUT-LINE(IX) = ALL '+' OR                      
234800           REQU-KDCMDVAL-INPUT-LINE(IX) = SPACE OR                        
234900           REQU-KDCMDVAL-INPUT-LINE(IX) = LOW-VALUE                       
235000           MOVE ALL-SPACE       TO RESP-KDCMDVAL-INPUT-LINE(IX)           
235100        ELSE                                                              
235200           MOVE ALL-PLUS           TO RESP-KDCMDVAL-INPUT-LINE(IX)        
235300        END-IF                                                            
235400        ADD +1 TO IX                                                      
235500     END-PERFORM                                                          
235600     .                                                                    
235700     EJECT                                                                
235800     SKIP2                                                                
235900*----------------------------------------------------------------*        
236000 MFS-ROER-EJ-LAES-IN-FAELT  SECTION.                                      
236100                                                                          
236200*    --- ALLA UTDATA-FÄLT                                                 
236300*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
236400                                                                          
236500     MOVE ALL-PLUS               TO RESP-FLKLAR-BIL                       
236600                                    RESP-ADINLOMR                         
236700                                    RESP-TELOSSN1                         
236800                                    RESP-TELOSSN2                         
236900                                    RESP-ADINLOMR-PRT                     
237000                                    RESP-IDARTNR-START                    
237100                                    RESP-IDLEVNR-START                    
237200                                    RESP-IDFS-START                       
237300                                    RESP-IDARTNR-NEXT                     
237400                                    RESP-IDLEVNR-NEXT                     
237500                                    RESP-IDFS-NEXT                        
237600                                    RESP-NEXT                             
237700     IF INDATA-OK                                                         
237800        MOVE MFS-ADD-LAES-IN-FAELT TO RESP-ADINLOMR-PRT-ATTR              
237900                                      RESP-FLKLAR-BIL-ATTR                
238000                                      RESP-ADINLOMR-ATTR                  
238100     END-IF                                                               
238200                                                                          
238300*--- INDEXERADE RADER                                                     
238400*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
238500                                                                          
238600     MOVE SPACE                   TO SPAR-IDLEVNR                         
238700                                     SPAR-IDARTNR                         
238800                                     SPAR-IDFS                            
238900     MOVE +1 TO IX                                                        
239000     PERFORM UNTIL IX > MAX-KVRADER                                       
239100      INSPECT REQU-IDARTNR-LINE(IX)                                       
239200                          REPLACING LEADING SPACE BY ZERO                 
239300                                                                          
239400        MOVE ALL-PLUS              TO RESP-KDCMDVAL-INPUT-LINE(IX)        
239500        IF (REQU-IDLEVNR-LINE(IX) = SPAR-IDLEVNR AND                      
239600           REQU-IDFS-LINE(IX) = SPAR-IDFS AND                             
239700           REQU-IDARTNR-LINE(IX) = SPAR-IDARTNR)                          
239800           OR                                                             
239900           (REQU-IDLEVNR-LINE(IX) = SPACE OR                              
240000            REQU-IDFS-LINE(IX) = SPACE OR                                 
240100            REQU-IDARTNR-LINE(IX) = ZERO)                                 
240200           MOVE ALL-SPACE          TO RESP-KDCMDVAL-INPUT-LINE(IX)        
240300           MOVE MFS-STAENG-FAELT                                          
240400                             TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)         
240500        ELSE                                                              
240600           IF RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                           
240700                                           = MFS-ALFA-FAELT-FEL           
240800              CONTINUE                                                    
240900           ELSE                                                           
241000              MOVE MFS-ADD-LAES-IN-FAELT                                  
241100                             TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)         
241200           END-IF                                                         
241300        END-IF                                                            
241400                                                                          
241500        IF REQU-IDLEVNR-LINE(IX) = SPAR-IDLEVNR                           
241600           MOVE REQU-IDLEVNR-LINE(IX)     TO SPAR-IDLEVNR                 
241700           MOVE REQU-IDLEVNR-LINE(IX)     TO RESP-IDLEVNR-LINE(IX)        
241800           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
241900                               TO RESP-IDLEVNR-LINE-ATTR(IX)              
242000        ELSE                                                              
242100           MOVE REQU-IDLEVNR-LINE(IX)     TO SPAR-IDLEVNR                 
242200           MOVE ALL-PLUS                  TO RESP-IDLEVNR-LINE(IX)        
242300           MOVE MFS-ADD-LAES-IN-FAELT                                     
242400                               TO RESP-IDLEVNR-LINE-ATTR(IX)              
242500        END-IF                                                            
242600                                                                          
242700        IF REQU-IDFS-LINE(IX) = SPAR-IDFS                                 
242800           MOVE REQU-IDFS-LINE(IX)        TO SPAR-IDFS                    
242900           MOVE REQU-IDFS-LINE(IX)        TO RESP-IDFS-LINE(IX)           
243000           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
243100                                    TO RESP-IDFS-LINE-ATTR(IX)            
243200        ELSE                                                              
243300           MOVE REQU-IDFS-LINE(IX)        TO SPAR-IDFS                    
243400           MOVE ALL-PLUS                  TO RESP-IDFS-LINE(IX)           
243500           MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDFS-LINE-ATTR(IX)          
243600        END-IF                                                            
243700                                                                          
243800        IF REQU-IDARTNR-LINE(IX) = SPAR-IDARTNR                           
243900           MOVE REQU-IDARTNR-LINE(IX)   TO SPAR-IDARTNR                   
244000           MOVE REQU-IDARTNR-LINE(IX)   TO RESP-IDARTNR-LINE(IX)          
244100           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
244200                                  TO RESP-IDARTNR-LINE-ATTR(IX)           
244300           MOVE ALL-PLUS                TO RESP-KVAVIS-LINE(IX)           
244400                                       RESP-ADINLOMR-NXT-LINE(IX)         
244500                                       RESP-KVAVIS-PRIO-LINE(IX)          
244600                                       RESP-KVAVIS-KIT-LINE(IX)           
244700                                       RESP-ADTRDEST-KIT-LINE(IX)         
244800                                       RESP-FLKVROS-LINE(IX)              
244900                                       RESP-FLKVAKAR-LINE(IX)             
245000                                       RESP-KDFARLIG-LINE(IX)             
245100           MOVE MFS-ADD-LAES-IN-FAELT TO RESP-KVAVIS-LINE-ATTR(IX)        
245200                                  RESP-ADINLOMR-NXT-LINE-ATTR(IX)         
245300                                  RESP-KVAVIS-PRIO-LINE-ATTR(IX)          
245400                                  RESP-KVAVIS-KIT-LINE-ATTR(IX)           
245500                                  RESP-ADTRDEST-KIT-LINE-ATTR(IX)         
245600                                                                          
245700        ELSE                                                              
245800           MOVE REQU-IDARTNR-LINE(IX) TO SPAR-IDARTNR                     
245900           MOVE ALL-PLUS              TO RESP-IDARTNR-LINE(IX)            
246000           MOVE MFS-ADD-LAES-IN-FAELT                                     
246100                             TO RESP-IDARTNR-LINE-ATTR(IX)                
246200                                                                          
246300           MOVE ALL-PLUS              TO RESP-KVAVIS-TOT-LINE(IX)         
246400                                         RESP-KVKOLLI-LINE(IX)            
246500                                    RESP-KDLAGEMB-LINE(IX)                
246600                                    RESP-BEFT-LINE(IX)                    
246700                                    RESP-ADINLOMR-NXT-LINE(IX)            
246800                                    RESP-KVAVIS-LINE(IX)                  
246900                                    RESP-KVAVIS-PRIO-LINE(IX)             
247000                                    RESP-KVAVIS-KIT-LINE(IX)              
247100                                    RESP-ADTRDEST-KIT-LINE(IX)            
247200                                    RESP-FLKVROS-LINE(IX)                 
247300                                    RESP-FLKVAKAR-LINE(IX)                
247400                                    RESP-KDFARLIG-LINE(IX)                
247500           MOVE MFS-ADD-LAES-IN-FAELT                                     
247600                               TO RESP-KVAVIS-TOT-LINE-ATTR(IX)           
247700                                  RESP-KVKOLLI-LINE-ATTR(IX)              
247800                                  RESP-KDLAGEMB-LINE-ATTR(IX)             
247900                                  RESP-BEFT-LINE-ATTR(IX)                 
248000                                  RESP-ADINLOMR-NXT-LINE-ATTR(IX)         
248100                                  RESP-KVAVIS-LINE-ATTR(IX)               
248200                               RESP-KVAVIS-PRIO-LINE-ATTR(IX)             
248300                                  RESP-KVAVIS-KIT-LINE-ATTR(IX)           
248400                                  RESP-ADTRDEST-KIT-LINE-ATTR(IX)         
248500        END-IF                                                            
248600        ADD +1 TO IX                                                      
248700     END-PERFORM                                                          
248800     .                                                                    
248900     EJECT                                                                
249000******************************************************************        
249100*  IMS-SECTIONER                                                 *        
249200******************************************************************        
249300*----------------------------------------------------------------*        
249400******************************************************************        
249500*    ALT1-PCB  (TRANS W60191)                                   *         
249600******************************************************************        
249700*----------------------------------------------------------------*        
249800 IMS-ISRT-MSG-ALT1-6191 SECTION.                                          
249900                                                                          
250000     MOVE SPACE              TO GODK-STATUSKODER                          
250100     CALL CBLTDLI USING      ISRT ALT1-PCB                                
250200                                  P-TO-P-T91                              
250300     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
250400     PERFORM IMS-STATUSKONTROLL                                           
250500     .                                                                    
250600     EJECT                                                                
250700                                                                          
250800******************************************************************        
250900*    ALT1-PCB  (TRANS W60191)                                   *         
251000******************************************************************        
251100*----------------------------------------------------------------*        
251200 IMS-PURG-MSG-ALT1-6191 SECTION.                                          
251300                                                                          
251400     MOVE SPACE              TO GODK-STATUSKODER                          
251500     CALL CBLTDLI USING      PURG ALT1-PCB                                
251600                                  P-TO-P-T91                              
251700     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
251800     PERFORM IMS-STATUSKONTROLL                                           
251900     .                                                                    
252000     EJECT                                                                
252100                                                                          
252200******************************************************************        
252300*    INLA-PCB                                                    *        
252400******************************************************************        
252500*----------------------------------------------------------------*        
252600 IMS-GU-INLA-D101 SECTION.                                                
252700     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
252800             DELIMITED BY SIZE INTO SSA1                                  
252900     MOVE '  GE'            TO GODK-STATUSKODER                           
253000     CALL CBLTDLI USING GU  INLA-PCB                                      
253100                            DLI-IO-AREA-1                                 
253200                            SSA1                                          
253300     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
253400     PERFORM IMS-STATUSKONTROLL                                           
253500     .                                                                    
253600     EJECT                                                                
253700     SKIP3                                                                
253800*----------------------------------------------------------------*        
253900 IMS-GNP-INLA-D111-ARTNR SECTION.                                         
254000     STRING 'W6INLA11(IDARTNR  =' W-IDARTNR-X ')'                         
254100             DELIMITED BY SIZE INTO SSA1                                  
254200     MOVE '  GE'          TO GODK-STATUSKODER                             
254300     CALL CBLTDLI USING GNP INLA-PCB                                      
254400                            DLI-IO-AREA-1                                 
254500                            SSA1                                          
254600     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
254700     PERFORM IMS-STATUSKONTROLL                                           
254800     .                                                                    
254900     EJECT                                                                
255000     SKIP3                                                                
255100*----------------------------------------------------------------*        
255200 IMS-GNP-INLA-D121-RADNR-INL SECTION.                                     
255300     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
255400             DELIMITED BY SIZE INTO SSA1                                  
255500     MOVE 'W6INLA21'   TO SSA2                                            
255600     MOVE '  GE'          TO GODK-STATUSKODER                             
255700     CALL CBLTDLI USING GNP INLA-PCB                                      
255800                            DLI-IO-AREA-1                                 
255900                            SSA1                                          
256000                            SSA2                                          
256100     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
256200     PERFORM IMS-STATUSKONTROLL                                           
256300     .                                                                    
256400     EJECT                                                                
256500     SKIP3                                                                
256600*----------------------------------------------------------------*        
256700 IMS-GU-INLA-D111 SECTION.                                                
256800     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
256900             DELIMITED BY SIZE INTO SSA1                                  
257000     STRING 'W6INLA11(IDARTNR  =' W-IDARTNR-X ')'                         
257100          DELIMITED BY SIZE INTO SSA2                                     
257200     MOVE '  GE'            TO GODK-STATUSKODER                           
257300     CALL CBLTDLI USING GU  INLA-PCB                                      
257400                            DLI-IO-AREA-1                                 
257500                            SSA1                                          
257600                            SSA2                                          
257700     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
257800     PERFORM IMS-STATUSKONTROLL                                           
257900     .                                                                    
258000     EJECT                                                                
258100     SKIP3                                                                
258200*----------------------------------------------------------------*        
258300 IMS-GN-INLA-D111 SECTION.                                                
258400     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
258500             DELIMITED BY SIZE INTO SSA1                                  
258600     STRING 'W6INLA11(IDARTNR  =' W-IDARTNR-X ')'                         
258700          DELIMITED BY SIZE INTO SSA2                                     
258800     MOVE '  GE'            TO GODK-STATUSKODER                           
258900     CALL CBLTDLI USING GN  INLA-PCB                                      
259000                            DLI-IO-AREA-1                                 
259100                            SSA1                                          
259200                            SSA2                                          
259300     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
259400     PERFORM IMS-STATUSKONTROLL                                           
259500     .                                                                    
259600     EJECT                                                                
259700     SKIP3                                                                
259800*----------------------------------------------------------------*        
259900 IMS-GNP-INLA-D121 SECTION.                                               
260000     STRING 'W6INLA11(IDARTNR  =' W-IDARTNR-X ')'                         
260100          DELIMITED BY SIZE INTO SSA1                                     
260200     STRING 'W6INLA21(IDRADNR  >' W-IDRADNR-X ')'                         
260300          DELIMITED BY SIZE INTO SSA2                                     
260400     MOVE '  GE'            TO GODK-STATUSKODER                           
260500     CALL CBLTDLI USING GNP INLA-PCB                                      
260600                            DLI-IO-AREA-1                                 
260700                            SSA1                                          
260800                            SSA2                                          
260900     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
261000     PERFORM IMS-STATUSKONTROLL                                           
261100     .                                                                    
261200     EJECT                                                                
261300     SKIP3                                                                
261400*----------------------------------------------------------------*        
261500 IMS-GNP-INLA-D121-OKVAL SECTION.                                         
261600     STRING 'W6INLA11(IDARTNR  =' W-IDARTNR-X ')'                         
261700          DELIMITED BY SIZE INTO SSA1                                     
261800     MOVE 'W6INLA21 '  TO SSA2                                            
261900     MOVE '  GE'            TO GODK-STATUSKODER                           
262000     CALL CBLTDLI USING GNP INLA-PCB                                      
262100                            DLI-IO-AREA-1                                 
262200                            SSA1                                          
262300                            SSA2                                          
262400     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
262500     PERFORM IMS-STATUSKONTROLL                                           
262600     .                                                                    
262700     EJECT                                                                
262800     SKIP3                                                                
262900******************************************************************        
263000*    IMS-UPPDATERING VIA INLA-PCB                                *        
263100******************************************************************        
263200*----------------------------------------------------------------*        
263300 IMS-GHNP-INLA-D121 SECTION.                                              
263400     STRING 'W6INLA21(IDRADNR  >' W-IDRADNR-X ')'                         
263500          DELIMITED BY SIZE INTO SSA1                                     
263600     MOVE '  GE'            TO GODK-STATUSKODER                           
263700     CALL CBLTDLI USING GHNP INLA-PCB                                     
263800                            DLI-IO-AREA-1                                 
263900                            SSA1                                          
264000     MOVE INLA-STATUS-CODE  TO STATUS-WS                                  
264100     PERFORM IMS-STATUSKONTROLL                                           
264200     .                                                                    
264300     EJECT                                                                
264400     SKIP3                                                                
264500*----------------------------------------------------------------*        
264600 IMS-REPL-INLA SECTION.                                                   
264700                                                                          
264800     MOVE '  '               TO GODK-STATUSKODER                          
264900     CALL CBLTDLI USING REPL INLA-PCB                                     
265000                             DLI-IO-AREA-1                                
265100     MOVE INLA-STATUS-CODE   TO STATUS-WS                                 
265200     PERFORM IMS-STATUSKONTROLL                                           
265300     .                                                                    
265400     EJECT                                                                
265500     SKIP3                                                                
265600*----------------------------------------------------------------*        
265700******************************************************************        
265800*    IMS-UPPDATERING VIA INLA-ALT-PCB                            *        
265900******************************************************************        
266000*----------------------------------------------------------------*        
266100     SKIP3                                                                
266200*----------------------------------------------------------------*        
266300 IMS-GU-ALT-INLA-D121-LAST SECTION.                                       
266400     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
266500             DELIMITED BY SIZE INTO SSA1                                  
266600     STRING 'W6INLA11(IDARTNR  =' W-IDARTNR-X ')'                         
266700          DELIMITED BY SIZE INTO SSA2                                     
266800     MOVE 'W6INLA21*L' TO SSA3                                            
266900     MOVE '  GE'          TO GODK-STATUSKODER                             
267000     CALL CBLTDLI USING GU INLA-ALT-PCB                                   
267100                           DLI-IO-AREA-12                                 
267200                           SSA1                                           
267300                           SSA2                                           
267400                           SSA3                                           
267500     MOVE INLA-ALT-STATUS-CODE  TO STATUS-WS                              
267600     PERFORM IMS-STATUSKONTROLL                                           
267700     .                                                                    
267800     EJECT                                                                
267900     SKIP3                                                                
268000*----------------------------------------------------------------*        
268100 IMS-GHU-ALT-INLA-D121 SECTION.                                           
268200     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
268300             DELIMITED BY SIZE INTO SSA1                                  
268400     STRING 'W6INLA11(IDARTNR  =' W-IDARTNR-X ')'                         
268500          DELIMITED BY SIZE INTO SSA2                                     
268600     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-ALT-X ')'                     
268700          DELIMITED BY SIZE INTO SSA3                                     
268800     MOVE '  '            TO GODK-STATUSKODER                             
268900     CALL CBLTDLI USING GHU INLA-ALT-PCB                                  
269000                            DLI-IO-AREA-12                                
269100                            SSA1                                          
269200                            SSA2                                          
269300                            SSA3                                          
269400     MOVE INLA-ALT-STATUS-CODE  TO STATUS-WS                              
269500     PERFORM IMS-STATUSKONTROLL                                           
269600     .                                                                    
269700     EJECT                                                                
269800     SKIP3                                                                
269900*----------------------------------------------------------------*        
270000 IMS-ISRT-ALT-INLA-D121 SECTION.                                          
270100     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
270200             DELIMITED BY SIZE INTO SSA1                                  
270300     STRING 'W6INLA11(IDARTNR  =' W-IDARTNR-X ')'                         
270400          DELIMITED BY SIZE INTO SSA2                                     
270500     MOVE  'W6INLA21 ' TO SSA3                                            
270600     MOVE '  '            TO GODK-STATUSKODER                             
270700     CALL CBLTDLI USING ISRT INLA-ALT-PCB                                 
270800                            DLI-IO-AREA-12                                
270900                            SSA1                                          
271000                            SSA2                                          
271100                            SSA3                                          
271200     MOVE INLA-ALT-STATUS-CODE  TO STATUS-WS                              
271300     PERFORM IMS-STATUSKONTROLL                                           
271400     .                                                                    
271500     EJECT                                                                
271600     SKIP3                                                                
271700*----------------------------------------------------------------*        
271800 IMS-DLET-ALT-INLA SECTION.                                               
271900                                                                          
272000     MOVE '  '               TO GODK-STATUSKODER                          
272100     CALL CBLTDLI USING DLET INLA-ALT-PCB                                 
272200                             DLI-IO-AREA-12                               
272300     MOVE INLA-ALT-STATUS-CODE   TO STATUS-WS                             
272400     PERFORM IMS-STATUSKONTROLL                                           
272500     .                                                                    
272600     EJECT                                                                
272700     SKIP3                                                                
272800*----------------------------------------------------------------*        
272900******************************************************************        
273000*    LASA-PCB                                                    *        
273100******************************************************************        
273200     SKIP3                                                                
273300*----------------------------------------------------------------*        
273400 IMS-GU-LASA-G111      SECTION.                                           
273500     STRING 'W6LASA01(W6GXKEY  =' WL-W6GX01KY-X ')'                       
273600          DELIMITED BY SIZE INTO SSA1                                     
273700     STRING 'W6LASA11(W6GXKEY  =' WL-W6GX11KY-X ')'                       
273800          DELIMITED BY SIZE INTO SSA2                                     
273900     MOVE '  GE'            TO GODK-STATUSKODER                           
274000     CALL CBLTDLI USING GU  LASA-A-PCB                                    
274100                            DLI-IO-AREA-2A                                
274200                            SSA1                                          
274300                            SSA2                                          
274400     MOVE LASA-A-STATUS-CODE  TO STATUS-WS                                
274500     PERFORM IMS-STATUSKONTROLL                                           
274600     .                                                                    
274700     EJECT                                                                
274800     SKIP3                                                                
274900*----------------------------------------------------------------*        
275000 IMS-GNP-LASA-G121-OKVAL SECTION.                                         
275100     STRING 'W6LASA01(W6GXKEY  =' WL-W6GX01KY-X ')'                       
275200          DELIMITED BY SIZE INTO SSA1                                     
275300     STRING 'W6LASA11(W6GXKEY  =' WL-W6GX11KY-X ')'                       
275400          DELIMITED BY SIZE INTO SSA2                                     
275500     MOVE 'W6LASA21 '            TO SSA3                                  
275600     MOVE '  GE'            TO GODK-STATUSKODER                           
275700     CALL CBLTDLI USING GNP LASA-A-PCB                                    
275800                            DLI-IO-AREA-2A                                
275900                            SSA1                                          
276000                            SSA2                                          
276100                            SSA3                                          
276200     MOVE LASA-A-STATUS-CODE  TO STATUS-WS                                
276300     PERFORM IMS-STATUSKONTROLL                                           
276400     .                                                                    
276500     EJECT                                                                
276600     SKIP3                                                                
276700*----------------------------------------------------------------*        
276800 IMS-GNP-LASA-G121-SOEK1       SECTION.                                   
276900     STRING 'W6LASA01(W6GXKEY  =' WL-W6GX01KY-X ')'                       
277000          DELIMITED BY SIZE INTO SSA1                                     
277100     STRING 'W6LASA11(W6GXKEY  =' WL-W6GX11KY-X ')'                       
277200          DELIMITED BY SIZE INTO SSA2                                     
277300     STRING 'W6LASA21(IDARTNR  =' WL-W6GX21KY-SOEK1 ')'                   
277400             DELIMITED BY SIZE INTO SSA3                                  
277500     MOVE '  GE'            TO GODK-STATUSKODER                           
277600     CALL CBLTDLI USING GNP LASA-A-PCB                                    
277700                            DLI-IO-AREA-2A                                
277800                            SSA1                                          
277900                            SSA2                                          
278000                            SSA3                                          
278100     MOVE LASA-A-STATUS-CODE  TO STATUS-WS                                
278200     PERFORM IMS-STATUSKONTROLL                                           
278300     .                                                                    
278400     EJECT                                                                
278500     SKIP3                                                                
278600*----------------------------------------------------------------*        
278700 IMS-GNP-LASA-G121-SOEK2       SECTION.                                   
278800     STRING 'W6LASA01(W6GXKEY  =' WL-W6GX01KY-X ')'                       
278900          DELIMITED BY SIZE INTO SSA1                                     
279000     STRING 'W6LASA11(W6GXKEY  =' WL-W6GX11KY-X ')'                       
279100          DELIMITED BY SIZE INTO SSA2                                     
279200     STRING 'W6LASA21(IDLEVNR  =' WLS2-IDLEVNR-X                          
279300                    '&IDFS     =' WLS2-IDFS-X                             
279400                    '&IDARTNR  =' WLS2-IDARTNR-X ')'                      
279500             DELIMITED BY SIZE INTO SSA3                                  
279600     MOVE '  GE'            TO GODK-STATUSKODER                           
279700     CALL CBLTDLI USING GNP LASA-A-PCB                                    
279800                            DLI-IO-AREA-2A                                
279900                            SSA1                                          
280000                            SSA2                                          
280100                            SSA3                                          
280200     MOVE LASA-A-STATUS-CODE  TO STATUS-WS                                
280300     PERFORM IMS-STATUSKONTROLL                                           
280400     .                                                                    
280500     EJECT                                                                
280600     SKIP3                                                                
280700*----------------------------------------------------------------*        
280800     SKIP3                                                                
280900******************************************************************        
281000*    IMS-UPPDATERING VIA LASA-PCB                                *        
281100******************************************************************        
281200*----------------------------------------------------------------*        
281300 IMS-GHU-LASA-G111      SECTION.                                          
281400     STRING 'W6LASA01(W6GXKEY  =' WL-W6GX01KY-X ')'                       
281500          DELIMITED BY SIZE INTO SSA1                                     
281600     STRING 'W6LASA11(W6GXKEY  =' WL-W6GX11KY-X ')'                       
281700          DELIMITED BY SIZE INTO SSA2                                     
281800     MOVE '  GE'            TO GODK-STATUSKODER                           
281900     CALL CBLTDLI USING GHU LASA-B-PCB                                    
282000                            DLI-IO-AREA-2B                                
282100                            SSA1                                          
282200                            SSA2                                          
282300     MOVE LASA-B-STATUS-CODE  TO STATUS-WS                                
282400     PERFORM IMS-STATUSKONTROLL                                           
282500     .                                                                    
282600     EJECT                                                                
282700     SKIP3                                                                
282800*----------------------------------------------------------------*        
282900 IMS-GHNP-LASA-G121-OKVAL SECTION.                                        
283000     STRING 'W6LASA01(W6GXKEY  =' WL-W6GX01KY-X ')'                       
283100          DELIMITED BY SIZE INTO SSA1                                     
283200     STRING 'W6LASA11(W6GXKEY  =' WL-W6GX11KY-X ')'                       
283300          DELIMITED BY SIZE INTO SSA2                                     
283400     MOVE 'W6LASA21 '            TO SSA3                                  
283500     MOVE '  GE'            TO GODK-STATUSKODER                           
283600     CALL CBLTDLI USING GHNP LASA-B-PCB                                   
283700                            DLI-IO-AREA-2B                                
283800                            SSA1                                          
283900                            SSA2                                          
284000                            SSA3                                          
284100     MOVE LASA-B-STATUS-CODE  TO STATUS-WS                                
284200     PERFORM IMS-STATUSKONTROLL                                           
284300     .                                                                    
284400     EJECT                                                                
284500     SKIP3                                                                
284600*----------------------------------------------------------------*        
284700     SKIP3                                                                
284800*----------------------------------------------------------------*        
284900 IMS-GHNP-LASA-G121-SOEK2       SECTION.                                  
285000     STRING 'W6LASA01(W6GXKEY  =' WL-W6GX01KY-X ')'                       
285100          DELIMITED BY SIZE INTO SSA1                                     
285200     STRING 'W6LASA11(W6GXKEY  =' WL-W6GX11KY-X ')'                       
285300          DELIMITED BY SIZE INTO SSA2                                     
285400     STRING 'W6LASA21(IDLEVNR  =' WLS2-IDLEVNR-X                          
285500                    '&IDFS     =' WLS2-IDFS-X                             
285600                    '&IDARTNR  =' WLS2-IDARTNR-X ')'                      
285700             DELIMITED BY SIZE INTO SSA3                                  
285800     MOVE '  GE'            TO GODK-STATUSKODER                           
285900     CALL CBLTDLI USING GHNP LASA-B-PCB                                   
286000                            DLI-IO-AREA-2B                                
286100                            SSA1                                          
286200                            SSA2                                          
286300                            SSA3                                          
286400     MOVE LASA-B-STATUS-CODE  TO STATUS-WS                                
286500     PERFORM IMS-STATUSKONTROLL                                           
286600     .                                                                    
286700     EJECT                                                                
286800     SKIP3                                                                
286900*----------------------------------------------------------------*        
287000*----------------------------------------------------------------*        
287100 IMS-REPL-LASA SECTION.                                                   
287200                                                                          
287300     MOVE '  '               TO GODK-STATUSKODER                          
287400     CALL CBLTDLI USING REPL LASA-B-PCB                                   
287500                             DLI-IO-AREA-2B                               
287600     MOVE LASA-B-STATUS-CODE   TO STATUS-WS                               
287700     PERFORM IMS-STATUSKONTROLL                                           
287800     .                                                                    
287900     EJECT                                                                
288000     SKIP3                                                                
288100*----------------------------------------------------------------*        
288200 IMS-ISRT-LASA SECTION.                                                   
288300                                                                          
288400     MOVE 'W6LASA21 '        TO SSA1                                      
288500     MOVE '  '               TO GODK-STATUSKODER                          
288600     CALL CBLTDLI USING ISRT LASA-B-PCB                                   
288700                             DLI-IO-AREA-2B                               
288800                             SSA1                                         
288900     MOVE LASA-B-STATUS-CODE   TO STATUS-WS                               
289000     PERFORM IMS-STATUSKONTROLL                                           
289100     .                                                                    
289200     EJECT                                                                
289300     SKIP3                                                                
289400*----------------------------------------------------------------*        
289500 IMS-DLET-LASA SECTION.                                                   
289600                                                                          
289700     MOVE '  '               TO GODK-STATUSKODER                          
289800     CALL CBLTDLI USING DLET LASA-B-PCB                                   
289900                             DLI-IO-AREA-2B                               
290000     MOVE LASA-B-STATUS-CODE   TO STATUS-WS                               
290100     PERFORM IMS-STATUSKONTROLL                                           
290200     .                                                                    
290300     EJECT                                                                
290400     SKIP3                                                                
290500******************************************************************        
290600*    PLAA-PCB                                                    *        
290700******************************************************************        
290800     SKIP3                                                                
290900*----------------------------------------------------------------*        
291000 IMS-GU-PLAA-G111 SECTION.                                                
291100     STRING 'W6PLAA01(W6GXKEY  =' W-W6GX01KY-X ')'                        
291200          DELIMITED BY SIZE INTO SSA1                                     
291300     STRING 'W6PLAA11(W6GXKEY  =' W-W6GX11KY-X ')'                        
291400          DELIMITED BY SIZE INTO SSA2                                     
291500     MOVE '  GE'            TO GODK-STATUSKODER                           
291600     CALL CBLTDLI USING GU  PLAA-PCB                                      
291700                            DLI-IO-AREA-3                                 
291800                            SSA1                                          
291900                            SSA2                                          
292000     MOVE PLAA-STATUS-CODE  TO STATUS-WS                                  
292100     PERFORM IMS-STATUSKONTROLL                                           
292200     .                                                                    
292300     EJECT                                                                
292400     SKIP3                                                                
292500 IMS-GU-WDK711 SECTION.                                                   
292600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
292700          DELIMITED BY SIZE INTO SSA1                                     
292800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
292900          DELIMITED BY SIZE INTO SSA2                                     
293000     MOVE '  GE' TO GODK-STATUSKODER                                      
293100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-711 SSA1 SSA2             
293200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
293300     PERFORM IMS-STATUSKONTROLL                                           
293400     .                                                                    
293500     EJECT                                                                
293600*----------------------------------------------------------------*        
293700 IMS-STATUSKONTROLL SECTION.                                              
293800                                                                          
293900     SET STATUS-IX TO 1                                                   
294000     SEARCH GODK-STATUS                                                   
294100       AT END                                                             
294200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
294300         DELIMITED BY SIZE INTO FELTEXT                                   
294400         CALL FELLOG                                                      
294500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
294600         CONTINUE                                                         
294700     END-SEARCH                                                           
294800     .                                                                    
294900     EJECT                                                                
295000     SKIP3                                                                
295100******************************************************************        
295200*  INITERA BLÄDDRINGS-NKL:AR                                     *        
295300******************************************************************        
295400*----------------------------------------------------------------*        
295500 S10-NKL-NAESTA-SIDA   SECTION.                                           
295600                                                                          
295700     MOVE '6107'                   TO WL-IDHTYP                           
295800     MOVE RESP-IDLBBET-KEY         TO WL-IDLBBET                          
295900                                                                          
296000     IF NKLTYP1                                                           
296100*-------HÄR BEHÖVS LEVNR + FS FÖR ATT HITTA STARVÄRDE                     
296200        MOVE REQU-IDARTNR-START     TO WLS2-IDARTNR                       
296300                                      RESP-IDARTNR-START                  
296400        MOVE REQU-IDLEVNR-START    TO WLS2-IDLEVNR                        
296500                                      RESP-IDLEVNR-START                  
296600        MOVE REQU-IDFS-START       TO WLS2-IDFS                           
296700                                      RESP-IDFS-START                     
296800     END-IF                                                               
296900                                                                          
297000     IF NKLTYP2                                                           
297100*-------HÄR BEHÖVS LEVNR + FS FÖR ATT HITTA STARVÄRDE                     
297200        MOVE REQU-IDARTNR-START    TO WLS1-IDARTNR                        
297300                                      WLS2-IDARTNR                        
297400                                      RESP-IDARTNR-START                  
297500        MOVE REQU-IDLEVNR-START    TO WLS2-IDLEVNR                        
297600                                      RESP-IDLEVNR-START                  
297700        MOVE REQU-IDFS-START       TO WLS2-IDFS                           
297800                                      RESP-IDFS-START                     
297900     END-IF                                                               
298000                                                                          
298100     IF NKLTYP3                                                           
298200        MOVE REQU-IDARTNR-START    TO WLS2-IDARTNR                        
298300                                      RESP-IDARTNR-START                  
298400        MOVE REQU-IDLEVNR-START    TO WLS2-IDLEVNR                        
298500                                      RESP-IDLEVNR-START                  
298600        MOVE REQU-IDFS-START       TO WLS2-IDFS                           
298700                                      RESP-IDFS-START                     
298800     END-IF                                                               
298900                                                                          
299000     MOVE NEJ                      TO SAMMA-SIDA-SW                       
299100     MOVE REQU-START               TO RESP-NEXT                           
299200                                                                          
299300     .                                                                    
299400     EJECT                                                                
299500     SKIP3                                                                
299600*----------------------------------------------------------------*        
299700 S11-NKL-SAMMA-SIDA   SECTION.                                            
299800                                                                          
299900     MOVE '6107'                   TO WL-IDHTYP                           
300000     MOVE RESP-IDLBBET-KEY         TO WL-IDLBBET                          
300100                                                                          
300200     IF NKLTYP1                                                           
300300*-------HÄR BEHÖVS LEVNR + FS FÖR ATT HITTA STARVÄRDE                     
300400        MOVE REQU-IDARTNR-START    TO WLS2-IDARTNR                        
300500                                      RESP-IDARTNR-START                  
300600        MOVE REQU-IDLEVNR-START    TO WLS2-IDLEVNR                        
300700                                      RESP-IDLEVNR-START                  
300800        MOVE REQU-IDFS-START       TO WLS2-IDFS                           
300900                                      RESP-IDFS-START                     
301000     END-IF                                                               
301100                                                                          
301200     IF NKLTYP2                                                           
301300*-------HÄR BEHÖVS LEVNR + FS FÖR ATT HITTA STARVÄRDE                     
301400        MOVE REQU-IDARTNR-START    TO WLS1-IDARTNR                        
301500                                      WLS2-IDARTNR                        
301600                                      RESP-IDARTNR-START                  
301700        MOVE REQU-IDLEVNR-START    TO WLS2-IDLEVNR                        
301800                                      RESP-IDLEVNR-START                  
301900        MOVE REQU-IDFS-START       TO WLS2-IDFS                           
302000                                      RESP-IDFS-START                     
302100     END-IF                                                               
302200                                                                          
302300     IF NKLTYP3                                                           
302400        MOVE REQU-IDARTNR-START    TO WLS2-IDARTNR                        
302500                                      RESP-IDARTNR-START                  
302600        MOVE REQU-IDLEVNR-START    TO WLS2-IDLEVNR                        
302700                                      RESP-IDLEVNR-START                  
302800        MOVE REQU-IDFS-START       TO WLS2-IDFS                           
302900                                      RESP-IDFS-START                     
303000     END-IF                                                               
303100                                                                          
303200     MOVE JA                       TO SAMMA-SIDA-SW                       
303300                                                                          
303400     .                                                                    
303500     EJECT                                                                
303600     SKIP3                                                                
303700*----------------------------------------------------------------*        
303800 S12-INIT-NKL   SECTION.                                                  
303900                                                                          
304000     MOVE '6107'                   TO WL-IDHTYP                           
304100     MOVE RESP-IDLBBET-KEY         TO WL-IDLBBET                          
304200                                                                          
304300     IF NKLTYP1                                                           
304400        MOVE ZERO                  TO WLS2-IDARTNR                        
304500        MOVE SPACE                 TO WLS2-IDFS                           
304600                                      WLS2-IDLEVNR                        
304700     END-IF                                                               
304800                                                                          
304900     IF NKLTYP2                                                           
305000        MOVE RESP-IDARTNR-KEY      TO WLS1-IDARTNR                        
305100     END-IF                                                               
305200                                                                          
305300     IF NKLTYP3                                                           
305400        MOVE RESP-IDARTNR-KEY      TO WLS2-IDARTNR                        
305500        MOVE RESP-IDLEVNR-KEY      TO WLS2-IDLEVNR                        
305600        MOVE RESP-IDFS-KEY         TO WLS2-IDFS                           
305700     END-IF                                                               
305800                                                                          
305900                                                                          
306000     MOVE SPACE                    TO RESP-IDFS-START                     
306100                                      RESP-IDFS-NEXT                      
306200                                      RESP-IDLEVNR-START                  
306300                                      RESP-IDLEVNR-NEXT                   
306400     MOVE ZERO                     TO RESP-IDARTNR-START                  
306500                                      RESP-IDARTNR-NEXT                   
306600     MOVE NEJ                      TO RESP-NEXT                           
306700     MOVE NEJ                      TO SAMMA-SIDA-SW                       
306800     .                                                                    
306900     EJECT                                                                
307000*----------------------------------------------------------------*        
307100 S20-FLYTTA-INLA11-SPAR      SECTION.                                     
307200                                                                          
307300     MOVE ART-KVAVIS         TO SPAR-KVAVIS-TOT                           
307400     MOVE ART-KDLAGEMB       TO SPAR-KDLAGEMB                             
307500     MOVE ART-KDFARLIG       TO SPAR-KDFARLIG                             
307600     MOVE ART-FLKVAKAR       TO SPAR-FLKVAKAR                             
307700     MOVE ART-BEFT           TO SPAR-BEFT                                 
307800     MOVE ART-ADTRDEST-KIT   TO SPAR-ADTRDEST-KIT                         
307900                                                                          
308000     .                                                                    
308100     EJECT                                                                
308200     SKIP3                                                                
308300*----------------------------------------------------------------*        
308400 S21-FLYTTA-LASA-WT          SECTION.                                     
308500                                                                          
308600     MOVE A-6110-IDLEVNR     TO WT-IDLEVNR(TIX)                           
308700     MOVE A-6110-IDFS        TO WT-IDFS(TIX)                              
308800     MOVE A-6110-IDARTNR     TO WT-IDARTNR(TIX)                           
308900     MOVE W-ACK-KVKOLLI      TO WT-KVKOLLI(TIX)                           
309000     MOVE A-6110-ADINLOMR    TO WT-ADINLOMR-NXT(TIX)                      
309100     MOVE A-6110-KVAVIS      TO WT-KVAVIS(TIX)                            
309200     MOVE A-6110-KVAVIS-PRIO TO WT-KVAVIS-PRIO(TIX)                       
309300     MOVE A-6110-KVAVIS-KIT  TO WT-KVAVIS-KIT(TIX)                        
309400     MOVE A-6110-TIAVIDAT    TO WT-TIAVIDAT(TIX)                          
309500                                                                          
309600     .                                                                    
309700     EJECT                                                                
309800     SKIP3                                                                
309900*----------------------------------------------------------------*        
310000 S22-FLYTTA-WT-MOD       SECTION.                                         
310100                                                                          
310200     MOVE +1 TO IX                                                        
310300     PERFORM UNTIL IX > RESP-KVRADER                                      
310400        MOVE WT-IDLEVNR(IX)              TO WS-IDLEVNR                    
310500        MOVE WS-IDLEVNR-X                TO RESP-IDLEVNR-LINE(IX)         
310600                                                                          
310700        MOVE WT-IDFS(IX)                 TO RESP-IDFS-LINE(IX)            
310800                                                                          
310900        MOVE WT-IDARTNR(IX)              TO WS-IDARTNR-N                  
311000                                            W-IDARTNR-K7                  
311100        MOVE WS-RED-IDARTNR              TO RESP-IDARTNR-LINE(IX)         
311200        IF WT-IDARTNR-LYS(IX) = JA                                        
311300           MOVE MFS-ADD-LYS-UPP-FAELT                                     
311400                                TO RESP-IDARTNR-LINE-ATTR(IX)             
311500           IF KARANTAEN                                                   
311600             MOVE INF-KARANTAEN            TO RESP-IDMSG-ERROR            
311700           ELSE                                                           
311800             MOVE INF-FARLIGT-GODS         TO RESP-IDMSG-ERROR            
311900           END-IF                                                         
312000        END-IF                                                            
312100        IF REQU-IDMSGVER = '001'                                          
312200           PERFORM IMS-GU-WDK711                                          
312300           IF SEGMENT-FINNS                                               
312400             IF SLAG-KVROS-DAG > 0 OR SLAG-KVROS-BULK > 0                 
312500                MOVE YES                 TO RESP-FLKVROS-LINE(IX)         
312600             END-IF                                                       
312700           END-IF                                                         
312800                                                                          
312900           MOVE WT-IDLEVNR(IX)           TO W-IDLEVNR                     
313000           MOVE WT-IDFS(IX)              TO W-IDFS                        
313100           MOVE WT-TIAVIDAT(IX)          TO W-TIAVIDAT                    
313200           MOVE WT-IDARTNR(IX)           TO W-IDARTNR                     
313300           PERFORM IMS-GU-INLA-D101                                       
313400           IF SEGMENT-FINNS                                               
313500             PERFORM IMS-GNP-INLA-D111-ARTNR                              
313600             MOVE ART-FLKVAKAR         TO RESP-FLKVAKAR-LINE(IX)          
313700             IF ART-KDFARLIG = 4 OR 7                                     
313800                MOVE YES               TO RESP-KDFARLIG-LINE(IX)          
313900             ELSE                                                         
314000                MOVE SPACES            TO RESP-KDFARLIG-LINE(IX)          
314100             END-IF                                                       
314200           END-IF                                                         
314300        END-IF                                                            
314400                                                                          
314500        MOVE WT-KVAVIS-TOT(IX)           TO WS-KVAVIS-TOT-N               
314600        MOVE WS-RED-KVAVIS-TOT      TO RESP-KVAVIS-TOT-LINE(IX)           
314700        MOVE WT-KVKOLLI(IX)              TO WS-KVKOLLI                    
314800        MOVE WS-KVKOLLI-X                TO RESP-KVKOLLI-LINE(IX)         
314900        MOVE WT-KDLAGEMB(IX)             TO RESP-KDLAGEMB-LINE(IX)        
315000        MOVE WT-BEFT(IX)                 TO RESP-BEFT-LINE(IX)            
315100        MOVE WT-ADINLOMR-NXT(IX)   TO RESP-ADINLOMR-NXT-LINE(IX)          
315200        MOVE WT-KVAVIS(IX)               TO WS-KVAVIS-N                   
315300        MOVE WS-RED-KVAVIS               TO RESP-KVAVIS-LINE(IX)          
315400        MOVE WT-KVAVIS-PRIO(IX)          TO WS-KVAVIS-PRIO-N              
315500        MOVE WS-RED-KVAVIS-PRIO       TO RESP-KVAVIS-PRIO-LINE(IX)        
315600        MOVE WT-KVAVIS-KIT(IX)           TO WS-KVAVIS-KIT-N               
315700        MOVE WS-RED-KVAVIS-KIT       TO RESP-KVAVIS-KIT-LINE(IX)          
315800        MOVE WT-ADTRDEST-KIT(IX)     TO RESP-ADTRDEST-KIT-LINE(IX)        
315900        PERFORM S22A-KOLLA-LYSA-UPP-FAELT                                 
316000        ADD 1 TO IX                                                       
316100     END-PERFORM                                                          
316200                                                                          
316300     .                                                                    
316400     EJECT                                                                
316500     SKIP3                                                                
316600*----------------------------------------------------------------*        
316700 S22A-KOLLA-LYSA-UPP-FAELT      SECTION.                                  
316800                                                                          
316900     MOVE +1 TO TIX                                                       
317000     PERFORM UNTIL TIX > MAX-KVRADER                                      
317100        IF WT-LYS-ATGKOD(TIX) = 'EMB' AND                                 
317200           WT-LYS-IDLEVNR(TIX) = WT-IDLEVNR(IX) AND                       
317300           WT-LYS-IDARTNR(TIX) = WT-IDARTNR(IX) AND                       
317400           WT-LYS-IDFS(TIX) = WT-IDFS(IX)                                 
317500           MOVE MFS-ADD-LYS-UPP-FAELT                                     
317600                              TO RESP-ADINLOMR-NXT-LINE-ATTR(IX)          
317700                                         RESP-KVAVIS-LINE-ATTR(IX)        
317800                                    RESP-KVAVIS-PRIO-LINE-ATTR(IX)        
317900                                    RESP-KVAVIS-KIT-LINE-ATTR(IX)         
318000        END-IF                                                            
318100        ADD 1 TO TIX                                                      
318200     END-PERFORM                                                          
318300     .                                                                    
318400     EJECT                                                                
318500*----------------------------------------------------------------*        
318600 S23-FLYTTA-SPAR-INLA11-WT   SECTION.                                     
318700                                                                          
318800     MOVE SPAR-KVAVIS-TOT     TO WT-KVAVIS-TOT(TIX)                       
318900     MOVE SPAR-KDLAGEMB       TO WT-KDLAGEMB(TIX)                         
319000     MOVE SPAR-BEFT           TO WT-BEFT(TIX)                             
319100     MOVE SPAR-ADTRDEST-KIT   TO WT-ADTRDEST-KIT(TIX)                     
319200                                                                          
319300     IF SPAR-KDFARLIG = +4                                                
319400     OR SPAR-KDFARLIG = +7                                                
319500*-------INFO-TEXT + LYS UPP ARTNR                                         
319600        MOVE JA                    TO  WT-IDARTNR-LYS(TIX)                
319700     END-IF                                                               
319800     IF SPAR-FLKVAKAR = JA                                                
319900        MOVE JA                    TO WT-IDARTNR-LYS(TIX)                 
320000        MOVE JA                    TO KARANTAEN-SW                        
320100     END-IF                                                               
320200     .                                                                    
320300     EJECT                                                                
320400*----------------------------------------------------------------*        
320500 S24-INIT-MOD-NEXT     SECTION.                                           
320600                                                                          
320700     MOVE A-6110-IDLEVNR           TO RESP-IDLEVNR-NEXT                   
320800     MOVE A-6110-IDARTNR           TO RESP-IDARTNR-NEXT                   
320900     MOVE A-6110-IDFS              TO RESP-IDFS-NEXT                      
321000     .                                                                    
321100     EJECT                                                                
321200*----------------------------------------------------------------*        
321300 S25-INIT-WT-TAB         SECTION.                                         
321400                                                                          
321500     MOVE +1 TO IX                                                        
321600     PERFORM UNTIL IX > MAX-KVRADER                                       
321700        MOVE ZERO                TO WT-IDARTNR(IX)                        
321800                                    WT-KVAVIS-TOT(IX)                     
321900                                    WT-KVKOLLI(IX)                        
322000                                    WT-KVAVIS(IX)                         
322100                                    WT-KVAVIS-PRIO(IX)                    
322200                                    WT-KVAVIS-KIT(IX)                     
322300                                    WT-TIAVIDAT(IX)                       
322400                                    WT-BEFT(IX)                           
322500                                                                          
322600        MOVE SPACE               TO WT-IDFS(IX)                           
322700                                    WT-IDARTNR-LYS(IX)                    
322800                                    WT-KDLAGEMB(IX)                       
322900                                    WT-ADINLOMR-NXT(IX)                   
323000                                    WT-IDLEVNR(IX)                        
323100                                    WT-ADTRDEST-KIT(IX)                   
323200        ADD 1 TO IX                                                       
323300     END-PERFORM                                                          
323400     .                                                                    
323500     EJECT                                                                
323600*----------------------------------------------------------------*        
323700 S30-CALL-STYR SECTION.                                                   
323800                                                                          
323900     CALL W611STYR USING STYR-W611STYR                                    
324000                         HANA-PCB STYR-PLAA-PCB                           
324100     .                                                                    
324200     EJECT                                                                
324300*----------------------------------------------------------------*        
324400 S40-TRANS-W60191      SECTION.                                           
324500                                                                          
324600     IF T91-MID-KVPOST = ZERO                                             
324700        MOVE JA                  TO TRANS91-SW                            
324800        MOVE SPACE               TO T91-MID-W6I19101                      
324900        MOVE +1                  TO T91-MID-KVPOST                        
325000        MOVE +1                  TO T91-IX                                
325100                                                                          
325200        MOVE IDPGM               TO T91-MID-IDPGM                         
325300        MOVE W-IDDC              TO T91-MID-IDDC                          
325400                                                                          
325500        MOVE SPAR-IDLOPNRM       TO T91-MID-IDLOPNRM(1)                   
325600        INSPECT T91-MID-IDLOPNRM(1)                                       
325700                REPLACING LEADING SPACE BY ZERO                           
325800        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(1)                 
325900        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(1)                  
326000        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(1)                
326100        MOVE +0                    TO T91-MID-KVKOLLI (1)                 
326200        MOVE 'N'                   TO T91-MID-FLINLI  (1)                 
326300        MOVE SPAR-ADINLOMR-OLD     TO T91-MID-ADINLOMR-OLD(1)             
326400        MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-NEW(1)             
326500        MOVE SPAR-ADINLOMR-NXT-OLD TO T91-MID-ADINLOMR-NXT-OLD(1)         
326600        MOVE RAD-ADINLOMR-NXT      TO T91-MID-ADINLOMR-NXT-NEW(1)         
326700        MOVE RAD-KVINLART          TO T91-MID-KVINLART-OLD(1)             
326800        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(1)             
326900        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(1)             
327000        MOVE RAD-KDINLSTA          TO T91-MID-KDINLSTA-NEW(1)             
327100     ELSE                                                                 
327200        COMPUTE T91-IX = T91-MID-KVPOST + 1                               
327300        ADD 1                      TO T91-MID-KVPOST                      
327400                                                                          
327500        MOVE SPAR-IDLOPNRM         TO T91-MID-IDLOPNRM(T91-IX)            
327600        INSPECT T91-MID-IDLOPNRM(T91-IX)                                  
327700                REPLACING LEADING SPACE BY ZERO                           
327800        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(T91-IX)            
327900        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(T91-IX)             
328000        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(T91-IX)           
328100        MOVE +0                    TO T91-MID-KVKOLLI  (T91-IX)           
328200        MOVE 'N'                   TO T91-MID-FLINLI   (T91-IX)           
328300        MOVE SPAR-ADINLOMR-OLD     TO T91-MID-ADINLOMR-OLD(T91-IX)        
328400        MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-NEW(T91-IX)        
328500        MOVE SPAR-ADINLOMR-NXT-OLD                                        
328600             TO T91-MID-ADINLOMR-NXT-OLD(T91-IX)                          
328700        MOVE RAD-ADINLOMR-NXT                                             
328800             TO T91-MID-ADINLOMR-NXT-NEW(T91-IX)                          
328900        MOVE RAD-KVINLART          TO T91-MID-KVINLART-OLD(T91-IX)        
329000        MOVE RAD-KVINLART          TO T91-MID-KVINLART-NEW(T91-IX)        
329100        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(T91-IX)        
329200        MOVE RAD-KDINLSTA         TO T91-MID-KDINLSTA-NEW(T91-IX)         
329300     END-IF                                                               
329400     .                                                                    
329500     EJECT                                                                
329600*----------------------------------------------------------------*        
329700 S41-TRANS-W60191-DLET  SECTION.                                          
329800                                                                          
329900     IF T91-MID-KVPOST = ZERO                                             
330000        MOVE JA                  TO TRANS91-SW                            
330100        MOVE SPACE               TO T91-MID-W6I19101                      
330200        MOVE +1                  TO T91-MID-KVPOST                        
330300        MOVE +1                  TO T91-IX                                
330400                                                                          
330500        MOVE IDPGM               TO T91-MID-IDPGM                         
330600        MOVE W-IDDC              TO T91-MID-IDDC                          
330700                                                                          
330800        MOVE SPAR-IDLOPNRM       TO T91-MID-IDLOPNRM(1)                   
330900        INSPECT T91-MID-IDLOPNRM(1)                                       
331000                REPLACING LEADING SPACE BY ZERO                           
331100        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(1)                 
331200        MOVE ALT-RAD-IDRADNR       TO T91-MID-IDRADNR(1)                  
331300        MOVE ALT-RAD-KDINLPRIO     TO T91-MID-KDINLPRIO(1)                
331400        MOVE +0                    TO T91-MID-KVKOLLI  (1)                
331500        MOVE 'J'                   TO T91-MID-FLINLI   (1)                
331600        MOVE ALT-RAD-ADINLOMR      TO T91-MID-ADINLOMR-OLD(1)             
331700        MOVE SPACE                 TO T91-MID-ADINLOMR-NEW(1)             
331800        MOVE ALT-RAD-ADINLOMR      TO T91-MID-ADINLOMR-NXT-OLD(1)         
331900        MOVE SPACE                 TO T91-MID-ADINLOMR-NXT-NEW(1)         
332000        MOVE ALT-RAD-KVINLART      TO T91-MID-KVINLART-OLD(1)             
332100        MOVE ZERO                  TO T91-MID-KVINLART-NEW(1)             
332200        MOVE ALT-RAD-KDINLSTA      TO T91-MID-KDINLSTA-OLD(1)             
332300        MOVE SPACE                 TO T91-MID-KDINLSTA-NEW(1)             
332400     ELSE                                                                 
332500        COMPUTE T91-IX = T91-MID-KVPOST + 1                               
332600        ADD 1                      TO T91-MID-KVPOST                      
332700                                                                          
332800        MOVE SPAR-IDLOPNRM         TO T91-MID-IDLOPNRM(T91-IX)            
332900        INSPECT T91-MID-IDLOPNRM(T91-IX)                                  
333000                REPLACING LEADING SPACE BY ZERO                           
333100        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(T91-IX)            
333200        MOVE ALT-RAD-IDRADNR       TO T91-MID-IDRADNR(T91-IX)             
333300        MOVE ALT-RAD-KDINLPRIO     TO T91-MID-KDINLPRIO(T91-IX)           
333400        MOVE +0                    TO T91-MID-KVKOLLI  (T91-IX)           
333500        MOVE 'N'                   TO T91-MID-FLINLI   (T91-IX)           
333600        MOVE ALT-RAD-ADINLOMR      TO T91-MID-ADINLOMR-OLD(T91-IX)        
333700        MOVE SPACE                 TO T91-MID-ADINLOMR-NEW(T91-IX)        
333800        MOVE ALT-RAD-ADINLOMR-NXT                                         
333900             TO T91-MID-ADINLOMR-NXT-OLD(T91-IX)                          
334000        MOVE SPACE                                                        
334100             TO T91-MID-ADINLOMR-NXT-NEW(T91-IX)                          
334200        MOVE ALT-RAD-KVINLART     TO T91-MID-KVINLART-OLD(T91-IX)         
334300        MOVE ZERO                 TO T91-MID-KVINLART-NEW(T91-IX)         
334400        MOVE ALT-RAD-KDINLSTA     TO T91-MID-KDINLSTA-OLD(T91-IX)         
334500        MOVE SPACE                TO T91-MID-KDINLSTA-NEW(T91-IX)         
334600     END-IF                                                               
334700     .                                                                    
334800     EJECT                                                                
334900*----------------------------------------------------------------*        
335000 S42-TRANS-W60191-ISRT  SECTION.                                          
335100                                                                          
335200     IF T91-MID-KVPOST = ZERO                                             
335300        MOVE JA                  TO TRANS91-SW                            
335400        MOVE SPACE               TO T91-MID-W6I19101                      
335500        MOVE +1                  TO T91-MID-KVPOST                        
335600        MOVE +1                  TO T91-IX                                
335700                                                                          
335800        MOVE IDPGM               TO T91-MID-IDPGM                         
335900        MOVE W-IDDC              TO T91-MID-IDDC                          
336000                                                                          
336100        MOVE SPAR-IDLOPNRM       TO T91-MID-IDLOPNRM(1)                   
336200        INSPECT T91-MID-IDLOPNRM(1)                                       
336300                REPLACING LEADING SPACE BY ZERO                           
336400        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(1)                 
336500        MOVE ALT-RAD-IDRADNR       TO T91-MID-IDRADNR(1)                  
336600        MOVE ALT-RAD-KDINLPRIO     TO T91-MID-KDINLPRIO(1)                
336700        MOVE +0                    TO T91-MID-KVKOLLI  (1)                
336800        MOVE 'N'                   TO T91-MID-FLINLI   (1)                
336900        MOVE SPACE                 TO T91-MID-ADINLOMR-OLD(1)             
337000        MOVE ALT-RAD-ADINLOMR      TO T91-MID-ADINLOMR-NEW(1)             
337100        MOVE SPACE                 TO T91-MID-ADINLOMR-NXT-OLD(1)         
337200        MOVE ALT-RAD-ADINLOMR-NXT  TO T91-MID-ADINLOMR-NXT-NEW(1)         
337300        MOVE ZERO                  TO T91-MID-KVINLART-OLD(1)             
337400        MOVE ALT-RAD-KVINLART      TO T91-MID-KVINLART-NEW(1)             
337500        MOVE SPACE                 TO T91-MID-KDINLSTA-OLD(1)             
337600        MOVE 'INL'                 TO T91-MID-KDINLSTA-NEW(1)             
337700     ELSE                                                                 
337800        COMPUTE T91-IX = T91-MID-KVPOST + 1                               
337900        ADD 1                      TO T91-MID-KVPOST                      
338000                                                                          
338100        MOVE SPAR-IDLOPNRM         TO T91-MID-IDLOPNRM(T91-IX)            
338200        INSPECT T91-MID-IDLOPNRM(T91-IX)                                  
338300                REPLACING LEADING SPACE BY ZERO                           
338400        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(T91-IX)            
338500        MOVE ALT-RAD-IDRADNR       TO T91-MID-IDRADNR(T91-IX)             
338600        MOVE ALT-RAD-KDINLPRIO     TO T91-MID-KDINLPRIO(T91-IX)           
338700        MOVE +0                    TO T91-MID-KVKOLLI  (T91-IX)           
338800        MOVE 'N'                   TO T91-MID-FLINLI   (T91-IX)           
338900        MOVE ALT-RAD-ADINLOMR      TO T91-MID-ADINLOMR-NEW(T91-IX)        
339000        MOVE SPACE                 TO T91-MID-ADINLOMR-OLD(T91-IX)        
339100        MOVE SPACE                                                        
339200             TO T91-MID-ADINLOMR-NXT-OLD(T91-IX)                          
339300        MOVE ALT-RAD-ADINLOMR-NXT                                         
339400             TO T91-MID-ADINLOMR-NXT-NEW(T91-IX)                          
339500        MOVE ALT-RAD-KVINLART     TO T91-MID-KVINLART-NEW(T91-IX)         
339600        MOVE ZERO                 TO T91-MID-KVINLART-OLD(T91-IX)         
339700        MOVE 'INL'                TO T91-MID-KDINLSTA-NEW(T91-IX)         
339800        MOVE SPACE                TO T91-MID-KDINLSTA-OLD(T91-IX)         
339900     END-IF                                                               
340000     .                                                                    
340100     EJECT                                                                
340200*----------------------------------------------------------------*        
340300 S50-SKICKA-W60191 SECTION.                                               
340400                                                                          
340500     COMPUTE W-PTOP1-OCC-LL = T91-MID-KVPOST * 64                         
340600     COMPUTE PTOP1-LL = W-PTOP1-OCC-LL + 35                               
340700                                                                          
340800     MOVE MFS-KDMFSFOR            TO PTOP1-KDMFSFOR                       
340900                                                                          
341000     IF FOERSTA-T91-JA                                                    
341100        PERFORM IMS-ISRT-MSG-ALT1-6191                                    
341200        MOVE NEJ     TO FOERSTA-T91-SW                                    
341300     ELSE                                                                 
341400        PERFORM IMS-PURG-MSG-ALT1-6191                                    
341500     END-IF                                                               
341600     .                                                                    
341700     EJECT                                                                
341800*----------------------------------------------------------------*        
341900 S51-SKICKA-W60193 SECTION.                                               
342000                                                                          
342100     ADD  1                    TO COUNTER                                 
342200     MOVE +53                  TO MSG-KOM-KVLL                            
342300     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
342400     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
342500     MOVE SPACE                TO MSG-KOM-KDTRANS                         
342600     MOVE 'W6I19301'           TO MSG-KOM-IDCPYTXT                        
342700     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
342800     MOVE 'W6012100'           TO MSG-KOM-IDSNDJOB                        
342900     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
343000                                                                          
343100     MOVE +30                  TO KMSG-KVLL                               
343200*----CTEXTLÄNGD + KMSG-AREA                                               
343300     MOVE 'W6T193X '           TO KMSG-KDTRANS-1                          
343400     MOVE '6121'               TO KMSG-IDTRANS-1                          
343500     MOVE MFS-KDMFSFOR         TO KMSG-KDMFSFOR-1                         
343600                                                                          
343700     CALL W006KOM USING MSG-PCB                                           
343800                        DISP-PCB                                          
343900                        KOM-KOMA-PCB                                      
344000                        MSG-KOM-WMSGKOM                                   
344100                        KMSG-IO-AREA                                      
344200     IF COUNTER = 100                                                     
344300       ADD  1 TO MSG-KOM-TIKLOCK                                          
344400       MOVE 0 TO COUNTER                                                  
344500     END-IF                                                               
344600     .                                                                    
344700     EJECT                                                                
344800*----------------------------------------------------------------*        
344900 S61-KOLLA-PRINTER SECTION.                                               
345000                                                                          
345100     MOVE 001                TO PRT-KDCALL                                
345200     MOVE '6M'               TO WS-IDPRT1                                 
345300     MOVE REQU-ADINLOMR-PRT  TO WS-IDPRT2                                 
345400     MOVE SPACE              TO WS-IDPRT3                                 
345500     MOVE WS-IDPRT           TO PRT-IDPRTLST                              
345600     MOVE SPACE              TO PRT-IDLTERM                               
345700                                                                          
345800     CALL W006PRT USING PRT-W006PRT                                       
345900                                                                          
346000     IF PRT-KDSVAR = 'R'                                                  
346100        CONTINUE                                                          
346200     ELSE                                                                 
346300*-FEL - 772                                                               
346400*-------PRINTER-SAKNAS                                                    
346500        MOVE NEJ                       TO INDATA-SW                       
346600        MOVE ERR-PRINT-SAKN            TO RESP-IDMSG-ERROR                
346700        MOVE MFS-ALFA-FAELT-FEL        TO RESP-ADINLOMR-PRT-ATTR          
346800     END-IF                                                               
346900     .                                                                    
347000     EJECT                                                                
347100*----------------------------------------------------------------*        
347200 S63-LAES-LASA-G111    SECTION.                                           
347300                                                                          
347400     MOVE '6107'                   TO WL-IDHTYP                           
347500     MOVE RESP-IDLBBET-KEY         TO WL-IDLBBET                          
347600     PERFORM IMS-GHU-LASA-G111                                            
347700                                                                          
347800     MOVE REQU-IDARTNR-LINE(IX)    TO WS-RED-IDARTNR                      
347900     INSPECT WS-IDARTNR-N REPLACING LEADING SPACE BY ZERO                 
348000     MOVE WS-IDARTNR-N             TO WLS2-IDARTNR                        
348100                                                                          
348200     MOVE REQU-IDLEVNR-LINE(IX)    TO WS-IDLEVNR-X                        
348300     MOVE WS-IDLEVNR               TO WLS2-IDLEVNR                        
348400                                                                          
348500     MOVE REQU-IDFS-LINE(IX)       TO WLS2-IDFS                           
348600     PERFORM  S63A-UPD-LASA-G121                                          
348700     .                                                                    
348800     EJECT                                                                
348900*----------------------------------------------------------------*        
349000 S63A-UPD-LASA-G121    SECTION.                                           
349100                                                                          
349200     PERFORM IMS-GHNP-LASA-G121-SOEK2                                     
349300*----LÄS ALLA LASA-RADER-21-SEGM                                          
349400     PERFORM UNTIL SEGMENT-SAKNAS                                         
349500        MOVE JA                    TO B-6110-FLKLAR                       
349600                                                                          
349700        PERFORM IMS-REPL-LASA                                             
349800        PERFORM IMS-GHNP-LASA-G121-SOEK2                                  
349900     END-PERFORM                                                          
350000     .                                                                    
350100     EJECT                                                                
350200*----------------------------------------------------------------*        
350300 S64-LAES-INLA-D111   SECTION.                                            
350400                                                                          
350500     MOVE B-6110-IDLEVNR       TO W-IDLEVNR                               
350600     MOVE B-6110-IDFS          TO W-IDFS                                  
350700     MOVE B-6110-TIAVIDAT      TO W-TIAVIDAT                              
350800     MOVE B-6110-IDARTNR       TO W-IDARTNR                               
350900     MOVE ZERO                 TO W-IDRADNR                               
351000                                                                          
351100     PERFORM IMS-GU-INLA-D111                                             
351200     IF SEGMENT-FINNS                                                     
351300       MOVE ART-IDLOPNRM         TO SPAR-IDLOPNRM                         
351400       MOVE ART-PRARTSTD         TO SPAR-PRARTSTD                         
351500     END-IF                                                               
351600     .                                                                    
351700     EJECT                                                                
351800*----------------------------------------------------------------*        
351900 S65-KOLLA-FLKLAR-BIL  SECTION.                                           
352000                                                                          
352100     IF REQU-FLKLAR-BIL = JA OR YES                                       
352200*-FEL - 003                                                               
352300*-------DENNA KAN INTE VARA JA OM MAN TRYCKT PF4                          
352400*-------UTAN DÅ SKA MAN TRYCKA PF11 ISTÄLLET                              
352500        MOVE NEJ                       TO INDATA-SW                       
352600        MOVE MFS-ALFA-FAELT-FEL        TO RESP-FLKLAR-BIL-ATTR            
352700        MOVE ALL-PLUS                  TO RESP-FLKLAR-BIL                 
352800        MOVE INF-PRESS-PF11            TO RESP-IDMSG-ERROR                
352900     END-IF                                                               
353000     .                                                                    
353100     EJECT                                                                
353200*----------------------------------------------------------------*        
353300 S66-KOLLA-AETG  SECTION.                                                 
353400                                                                          
353500     MOVE +1   TO IX                                                      
353600     PERFORM UNTIL IX > MAX-KVRADER                                       
353700        IF REQU-KDCMDVAL-INPUT-LINE(IX) NOT = ALL '+' AND                 
353800           REQU-KDCMDVAL-INPUT-LINE(IX) NOT = SPACE AND                   
353900           REQU-KDCMDVAL-INPUT-LINE(IX) NOT = LOW-VALUE                   
354000*-FEL - 003                                                               
354100*----------DESSA KAN INTE VARA IFYLLDA OM MAN TRYCKT PF4                  
354200*----------UTAN DÅ SKA MAN TRYCKA PF11 ISTÄLLET                           
354300           MOVE NEJ                    TO INDATA-SW                       
354400           MOVE MFS-ALFA-FAELT-FEL                                        
354500                      TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                
354600           MOVE ALL-PLUS                                                  
354700                      TO RESP-KDCMDVAL-INPUT-LINE(IX)                     
354800           MOVE INF-PRESS-PF11         TO RESP-IDMSG-ERROR                
354900        END-IF                                                            
355000        ADD 1 TO IX                                                       
355100     END-PERFORM                                                          
355200     .                                                                    
355300     EJECT                                                                
355400*----------------------------------------------------------------*        
355500 S67-KOLLA-LASTBARARE  SECTION.                                           
355600                                                                          
355700     MOVE '6107'                   TO WL-IDHTYP                           
355800     MOVE RESP-IDLBBET-KEY         TO WL-IDLBBET                          
355900     PERFORM IMS-GHU-LASA-G111                                            
356000     IF SEGMENT-SAKNAS                                                    
356100       MOVE NEJ TO INDATA-SW                                              
356200     END-IF                                                               
356300                                                                          
356400     .                                                                    
356500     EJECT                                                                
356600*----------------------------------------------------------------*        
356700 S70-LISTA-W6012111    SECTION.                                           
356800                                                                          
356900     MOVE W-IDDC                 TO W6012111-W601-IDDC                    
357000     MOVE RESP-IDLBBET-KEY       TO W6012111-W601-IDLBBET                 
357100     MOVE REQU-TELOSSN1          TO WS-TELOSSN1                           
357200     MOVE REQU-TELOSSN2          TO WS-TELOSSN2                           
357300     MOVE WS-TELOSSN             TO W6012111-W601-TELOSSN                 
357400     MOVE MSG-SIGNON-USERID      TO W6012111-W601-IDUSER                  
357500     MOVE WS-IDPRT               TO W6012111-W601-IDPRTLST                
357600     MOVE PRT-BEPRTLST           TO RESP-BEPRTLST                         
357700                                                                          
357800        CALL W6012111 USING ALT-PCB                                       
357900                            LISB-PCB                                      
358000                            W6012111-W601-W6012111                        
358100                            LASA-W6012111-PCB                             
358200                            INLA-W6012111-PCB                             
358300     .                                                                    
358400     EJECT                                                                
358500*----------------------------------------------------------------*        
358600 S80-LISTA-W6012112    SECTION.                                           
358700                                                                          
358800     MOVE W-IDDC                 TO W6012112-W601-IDDC                    
358900     MOVE RESP-IDLBBET-KEY       TO W6012112-W601-IDLBBET                 
359000     MOVE WS-IDPRT               TO W6012112-W601-IDPRTLST                
359100     MOVE PRT-BEPRTLST           TO RESP-BEPRTLST                         
359200        CALL W6012112 USING ALT-PCB                                       
359300                            W6012112-W601-W6012112                        
359400                            LASA-W6012112-PCB                             
359500                            INLA-W6012112-PCB                             
359600                            WDK6-W6012112-PCB                             
359700                            HANA-PCB STYR-PLAA-PCB                        
359800     .                                                                    
359900     EJECT                                                                
360000******************************************************************        
360100*  BILD-REDIGERING                                               *        
360200******************************************************************        
360300*----------------------------------------------------------------*        
360400 S90-BLANKUTF-NUM-FAELT SECTION.                                          
360500                                                                          
360600*----LÄS IN RAD-FÄLTEN IGEN FÖR ATT KLARA EN EV UPPDATERING               
360700*----NKL-FÄLT                                                             
360800     INSPECT RESP-IDLOPNRM-KEY REPLACING LEADING ZERO BY SPACE            
360900     INSPECT RESP-IDARTNR-KEY REPLACING LEADING ZERO BY SPACE             
361000*----RAD-FÄLT                                                             
361100*--- INDEXERADE RADER                                                     
361200                                                                          
361300     MOVE SPACE                          TO SPAR-IDFS                     
361400                                            SPAR-IDARTNR                  
361500                                            SPAR-IDLEVNR                  
361600     MOVE +1 TO IX                                                        
361700     PERFORM UNTIL IX > MAX-KVRADER                                       
361800                                                                          
361900      INSPECT RESP-IDARTNR-LINE(IX)                                       
362000              REPLACING LEADING ZERO BY SPACE                             
362100      INSPECT RESP-KVAVIS-TOT-LINE(IX)                                    
362200              REPLACING LEADING ZERO BY SPACE                             
362300      INSPECT RESP-KVKOLLI-LINE(IX)                                       
362400              REPLACING LEADING ZERO BY SPACE                             
362500      INSPECT RESP-KVAVIS-LINE(IX)                                        
362600              REPLACING LEADING ZERO BY SPACE                             
362700      INSPECT RESP-KVAVIS-PRIO-LINE(IX)                                   
362800              REPLACING LEADING ZERO BY SPACE                             
362900      INSPECT RESP-KVAVIS-KIT-LINE(IX)                                    
363000              REPLACING LEADING ZERO BY SPACE                             
363100      INSPECT RESP-BEFT-LINE(IX)                                          
363200              REPLACING LEADING ZERO BY SPACE                             
363300                                                                          
363400        IF INDATA-OK AND NYCKLAR-OK AND ALLT-OK                           
363500           IF (RESP-IDLEVNR-LINE(IX) = SPAR-IDLEVNR AND                   
363600              RESP-IDFS-LINE(IX) = SPAR-IDFS AND                          
363700              RESP-IDARTNR-LINE(IX) = SPAR-IDARTNR)                       
363800              OR                                                          
363900             (RESP-IDLEVNR-LINE(IX) = SPACE OR                            
364000              RESP-IDFS-LINE(IX) = SPACE OR                               
364100              RESP-IDARTNR-LINE(IX) = SPACE)                              
364200              MOVE ALL-SPACE                                              
364300                              TO RESP-KDCMDVAL-INPUT-LINE(IX)             
364400              MOVE MFS-STAENG-FAELT                                       
364500                              TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)        
364600           ELSE                                                           
364700              IF RESP-KDCMDVAL-INPUT-LINE-ATTR(IX) =                      
364800                 MFS-ADD-LAES-IN-FAELT-HI OR MFS-ALFA-FAELT-FEL           
364900                 CONTINUE                                                 
365000              ELSE                                                        
365100                 IF NOT LASTB-BORTTAGEN                                   
365200                   MOVE MFS-OEPPNA-ALFA-FAELT                             
365300                     TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)                 
365400                 END-IF                                                   
365500              END-IF                                                      
365600           END-IF                                                         
365700                                                                          
365800           PERFORM S90A-STAENG-FAELT-INBOERDES                            
365900                                                                          
366000        ELSE                                                              
366100           IF INDATA-FEL OR ALLT-NEJ                                      
366200              CONTINUE                                                    
366300           ELSE                                                           
366400              MOVE ALL-SPACE                                              
366500                              TO RESP-KDCMDVAL-INPUT-LINE(IX)             
366600              MOVE MFS-STAENG-FAELT                                       
366700                            TO RESP-KDCMDVAL-INPUT-LINE-ATTR(IX)          
366800              MOVE MFS-STAENG-FAELT TO RESP-FLKLAR-BIL-ATTR               
366900              MOVE MFS-STAENG-FAELT TO RESP-ADINLOMR-ATTR                 
367000           END-IF                                                         
367100        END-IF                                                            
367200        ADD 1 TO IX                                                       
367300     END-PERFORM                                                          
367400     .                                                                    
367500     EJECT                                                                
367600*----------------------------------------------------------------*        
367700 S90A-STAENG-FAELT-INBOERDES SECTION.                                     
367800                                                                          
367900     IF RESP-IDLEVNR-LINE(IX) = SPAR-IDLEVNR                              
368000        MOVE RESP-IDLEVNR-LINE(IX)        TO SPAR-IDLEVNR                 
368100        MOVE MFS-STAENG-FAELT-OSYNLIGT                                    
368200             TO RESP-IDLEVNR-LINE-ATTR(IX)                                
368300        IF RESP-IDFS-LINE(IX) = SPAR-IDFS                                 
368400           MOVE RESP-IDFS-LINE(IX)        TO SPAR-IDFS                    
368500           MOVE MFS-STAENG-FAELT-OSYNLIGT                                 
368600                TO RESP-IDFS-LINE-ATTR(IX)                                
368700           PERFORM S90AA-ART                                              
368800        ELSE                                                              
368900           MOVE RESP-IDFS-LINE(IX)        TO SPAR-IDFS                    
369000           MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDFS-LINE-ATTR(IX)          
369100           MOVE RESP-IDARTNR-LINE(IX)     TO SPAR-IDARTNR                 
369200           MOVE MFS-ADD-LAES-IN-FAELT                                     
369300                                 TO RESP-IDARTNR-LINE-ATTR(IX)            
369400                                    RESP-KVAVIS-TOT-LINE-ATTR(IX)         
369500                                    RESP-KVKOLLI-LINE-ATTR(IX)            
369600                                    RESP-KDLAGEMB-LINE-ATTR(IX)           
369700                                    RESP-BEFT-LINE-ATTR(IX)               
369800                              RESP-ADTRDEST-KIT-LINE-ATTR(IX)             
369900           IF WT-IDARTNR-LYS(IX) = JA                                     
370000              MOVE MFS-ADD-LAES-IN-FAELT-HI                               
370100                   TO RESP-IDARTNR-LINE-ATTR(IX)                          
370200           END-IF                                                         
370300        END-IF                                                            
370400     ELSE                                                                 
370500        MOVE RESP-IDLEVNR-LINE(IX)        TO SPAR-IDLEVNR                 
370600        MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDLEVNR-LINE-ATTR(IX)          
370700        MOVE RESP-IDFS-LINE(IX)           TO SPAR-IDFS                    
370800        MOVE MFS-ADD-LAES-IN-FAELT    TO RESP-IDFS-LINE-ATTR(IX)          
370900        MOVE RESP-IDARTNR-LINE(IX)        TO SPAR-IDARTNR                 
371000        MOVE MFS-ADD-LAES-IN-FAELT                                        
371100                                TO RESP-IDARTNR-LINE-ATTR(IX)             
371200                                   RESP-KVAVIS-TOT-LINE-ATTR(IX)          
371300                                   RESP-KVKOLLI-LINE-ATTR(IX)             
371400                                   RESP-KDLAGEMB-LINE-ATTR(IX)            
371500                                   RESP-BEFT-LINE-ATTR(IX)                
371600                                   RESP-ADTRDEST-KIT-LINE-ATTR(IX)        
371700        IF WT-IDARTNR-LYS(IX) = JA                                        
371800           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
371900                TO RESP-IDARTNR-LINE-ATTR(IX)                             
372000        END-IF                                                            
372100     END-IF                                                               
372200     .                                                                    
372300     EJECT                                                                
372400*----------------------------------------------------------------*        
372500 S90AA-ART SECTION.                                                       
372600                                                                          
372700     IF RESP-IDARTNR-LINE(IX) = SPAR-IDARTNR                              
372800        MOVE RESP-IDARTNR-LINE(IX) TO SPAR-IDARTNR                        
372900        MOVE MFS-STAENG-FAELT-OSYNLIGT                                    
373000             TO RESP-IDARTNR-LINE-ATTR(IX)                                
373100                RESP-KVAVIS-TOT-LINE-ATTR(IX)                             
373200                RESP-KVKOLLI-LINE-ATTR(IX)                                
373300                RESP-KDLAGEMB-LINE-ATTR(IX)                               
373400                RESP-BEFT-LINE-ATTR(IX)                                   
373500                RESP-ADTRDEST-KIT-LINE-ATTR(IX)                           
373600     ELSE                                                                 
373700        MOVE RESP-IDARTNR-LINE(IX) TO SPAR-IDARTNR                        
373800        MOVE MFS-ADD-LAES-IN-FAELT                                        
373900             TO RESP-IDARTNR-LINE-ATTR(IX)                                
374000                RESP-KVAVIS-TOT-LINE-ATTR(IX)                             
374100                RESP-KVKOLLI-LINE-ATTR(IX)                                
374200                RESP-KDLAGEMB-LINE-ATTR(IX)                               
374300                RESP-BEFT-LINE-ATTR(IX)                                   
374400                RESP-ADTRDEST-KIT-LINE-ATTR(IX)                           
374500        IF WT-IDARTNR-LYS(IX) = JA                                        
374600           MOVE MFS-ADD-LAES-IN-FAELT-HI                                  
374700                TO RESP-IDARTNR-LINE-ATTR(IX)                             
374800        END-IF                                                            
374900     END-IF                                                               
375000     .                                                                    
375100     EJECT                                                                
