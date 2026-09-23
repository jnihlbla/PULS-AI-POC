000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W2013900.                                            
000300*AUTHOR.             STEFAN ANDREASSON.                                   
000400*DATE-WRITTEN.       JUNI 2000.                                           
000500*    SKIP2                                                                
000600*REMARKS.                                                                 
000700*    FUNCTION.                                                            
000800*            TP-PROGRAM FÖR ANSKAFFNING (LEVERANSPLAN)                    
000900*            OMSPECNING PÅ DAGNIVÅ                                        
001000*            (DETTA PGM ÄR KOPIERAT FRÅN W2010300)                        
001100*                                                                         
001200*            PROGRAMMET LÄSER    WDG3 (WDGX2258/WDGX2260)H-TYP 225        
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W2T139     FRÅGEDEL                                 
001600*        MID:         W2I13901                                            
001700*    UTDATA.                                                              
001800*        MOD:         W2O13901                                            
001900*    SUBPROGRAM.                                                          
002000*        WDATKONV                                                         
002100*        FELLOG                                                           
002200*                                                                         
002300*   ÄNDRINGAR:                                                            
002400*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002500*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002600*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002700*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002800*                                                                         
002900*        14-03-04  LAGT TILL LÄSNING MOT WDG3/HTYP 2257 (BILD 2149        
003000*                  BLOCKERADE AVROPSVECKOR FÖR EN SHIP-LEVERANTÖR.        
003100*                  E'TRACKER 8403120.                                     
003200*                                                                         
003300*      2015-09-15  ETRACKER 10209749    (WDD903)                          
003400*                  ÄNDRA 2103/2403 ORSAK EXTRALEVERANSER OCH MERA.        
003500*                                                                         
003600*                                                                         
003700*                                                                         
003800     EJECT                                                                
003900 ENVIRONMENT DIVISION.                                                    
004000     SKIP2                                                                
004100 DATA DIVISION.                                                           
004200     SKIP2                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP3                                                                
004500*    -COPY WY2000W1                                                       
004600     SKIP3                                                                
004700*    -COPY WY2000W3                                                       
004800     SKIP3                                                                
004900*    -COPY WY2000W6                                                       
005000     SKIP3                                                                
005100*    -COPY WY2000W7                                                       
005200     SKIP3                                                                
005300*    -COPY WY2000W9                                                       
005400     SKIP3                                                                
005500     EJECT                                                                
005600 77  FELTEXT                 PIC X(80) VALUE SPACE.                       
005700 77  CURRENT-SECTION         PIC X(30) VALUE SPACE.                       
005800 77  DBS-SECTION             PIC X(30) VALUE SPACE.                       
005900                                                                          
006000 77  IDARTNR-WS              PIC X(9).                                    
006100 77  W-SAVE-IDLEVNR          PIC X(5).                                    
006200 77  IDLEVNR-WS              PIC X(5).                                    
006300 77  WS-IDLEVNR-8            PIC X(8).                                    
006400 77  PERIOD-WS               PIC X(4).                                    
006500 77  KDBEHX-PLAN-WS          PIC X(1).                                    
006600 77  PAH-KDBEHX-PLAN-IN      PIC X(1).                                    
006700 77  PAH-KDBEHX-PLAN-UT      PIC X(1).                                    
006800 77  JA                      PIC X(1)    VALUE 'J'.                       
006900 77  NEJ                     PIC X(1)    VALUE 'N'.                       
007000 77  FEL                     PIC X(1)    VALUE 'F'.                       
007100 77  VIP-ARTIKEL             PIC X(1)    VALUE 'V'.                       
007200 77  STRECK                  PIC X(1)    VALUE '-'.                       
007300 77  ASTERISK                PIC X(1)    VALUE '*'.                       
007400 77  PARENTES                PIC X(1)    VALUE ')'.                       
007500 77  GALLANDE                PIC X(1)    VALUE 'G'.                       
007600 77  FORSLAG                 PIC X(1)    VALUE 'F'.                       
007700 77  AAVV                    PIC X(4)    VALUE 'ÅÅVV'.                    
007800 77  YYWW                    PIC X(4)    VALUE 'YYWW'.                    
007900 77  KOLON                   PIC X(1)    VALUE ':'.                       
008000 77  W-NY-GAMMAL             PIC X(1)    VALUE SPACE.                     
008100 77  W-NY-INDATA             PIC X(1)    VALUE 'N'.                       
008200 77  W-GAMMAL-INDATA         PIC X(1)    VALUE 'G'.                       
008300 77  TRAFF                   PIC X(1)    VALUE SPACE.                     
008400                                                                          
008500 77  MAX-ANT-ORSAKSKODER     PIC S9(9)   VALUE +26   COMP SYNC.           
008600 77  MAX-ANT-PERIODER        PIC S9(9)   VALUE +2    COMP SYNC.           
008700 77  MAX-ANT-VECKOR-I-TAB    PIC S9(9)   VALUE +5    COMP SYNC.           
008800 77  MAX-ANT-PERIODER-I-TAB  PIC S9(9)   VALUE +2    COMP SYNC.           
008900 77  MAX-ANT-GAMLA-AVROP     PIC S9(9)   VALUE +5    COMP SYNC.           
009000 77  MAX-ANT-INDATA-FLT      PIC S9(9)   VALUE +21   COMP SYNC.           
009100 77  MAX-MOD-LENGD           PIC S9(9)   VALUE +1402 COMP SYNC.           
009200 01  WS-DAGENS-AAAAMMDD      PIC 9(8).                                    
009300 01  WS-DAGENS-DATUM         PIC 9(6).                                    
009400                                                                          
009500*01  -COPY WWDCKONS                                                       
009600                                                                          
009700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009800     88  HELP-MID                            VALUE '0551'.                
009900     88  EGEN-MID                            VALUE '2139'.                
010000     88  2103-MID                            VALUE '2103'.                
010100     EJECT                                                                
010200*                                         GENERELLA SUBPROGRAM            
010300 01  GENERELLA-SUBPROGRAM.                                                
010400     03 WDATKONV             PIC X(8)    VALUE 'WDATKONV'.                
010500     03 CBLTDLI              PIC X(8)    VALUE 'CBLTDLI '.                
010600     03 FELLOG               PIC X(8)    VALUE 'FELLOG  '.                
010700     03 W005INIT             PIC X(8)    VALUE 'W005INIT'.                
010800     03 WMEDKONV             PIC X(8)    VALUE 'WMEDKONV'.                
010900     03 W009VADD             PIC X(8)    VALUE 'W009VADD'.                
011000     03 WORKDAY              PIC X(8)    VALUE 'WORKDAY'.                 
011100     EJECT                                                                
011200 01  OLIKA-LEV               PIC X(5).                                    
011300     88 VOLKSWAGEN-LEVNR                 VALUE '6453'                     
011400                                               'Q09EB'.                   
011500     88 LV-LEVNR                         VALUE '14489'                    
011600                                               'DL7YA'.                   
011700     EJECT                                                                
011800*01  -COPY WORKAREA                                                       
011900     EJECT                                                                
012000*                                         NYCKELFÄLT FÖR LÄSNING          
012100*                                          AV DATABASER                   
012200 01  W-WDD901KY-X.                                                        
012300     03  W-IDARTNR-D9        PIC S9(9)    VALUE ZERO COMP-3.              
012400     03  W-IDDC-D9           PIC X(2)     VALUE SPACE.                    
012500 01  W-IDARTNR-X.                                                         
012600     03  W-IDARTNR           PIC S9(9)               COMP-3.              
012700 01  W-IDLEVNR-X.                                                         
012800     03  W-IDLEVNR           PIC X(5).                                    
012900 01  W-WDD905KY-X.                                                        
013000     03  W-DAAVROP-X.                                                     
013100         05  W-DAAVROP       PIC  9(6)    VALUE ZERO.                     
013200         05  FILLER REDEFINES W-DAAVROP.                                  
013300             07  W-DAAVROP-SS    PIC  9(2).                               
013400             07  W-DAAVROP-AAVV  PIC  9(4).                               
013500     03  W-TILEVDAG-X.                                                    
013600         05  W-TILEVDAG      PIC  S9      VALUE ZERO COMP-3.              
013700 01  W-KDCLAGER-X.                                                        
013800     03  W-KDCLAGER          PIC S9(1)              COMP-3.               
013900 01  W-KDAVROP-X.                                                         
014000     03  W-KDAVROP           PIC S9(1)               COMP-3.              
014100 01  W-IDSKYLT-X.                                                         
014200     03  W-IDSKYLT           PIC  X(3)   VALUE 'S  '.                     
014300 01  W-WDGXKEY-2223-X.                                                    
014400     03  W-IDHTYP            PIC X(4)    VALUE '2223'.                    
014500     03  W-IDANSK            PIC S9(3)   VALUE ZERO COMP-3.               
014600     03  FILLER              PIC X(24)   VALUE LOW-VALUE.                 
014700 01  W-FLNYLARM-X.                                                        
014800     03  W-FLNYLARM          PIC X       VALUE 'J'.                       
014900 01  W-WDGXKEY-2231-X.                                                    
015000     03  FILLER              PIC X(4)    VALUE '2231'.                    
015100     03  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
015200 01  W-WDGXKEY-2232-X.                                                    
015300     03  W-IDANSK-L          PIC S9(3)   VALUE ZERO COMP-3.               
015400     03  FILLER              PIC X(3)    VALUE LOW-VALUE.                 
015500 01  W-WDGXKEY-IDLEVNR.                                                   
015600     03  X-IDLEVNR           PIC X(5).                                    
015700 01  W-WDGXKEY-2215-X.                                                    
015800     03  W-WDGX-KEY          PIC X(4)    VALUE '2215'.                    
015900     03  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
016000 01  W-WDGXKEY-ROT.                                                       
016100     03  W-WDGXKEY           PIC X(4)    VALUE SPACE.                     
016200     03  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
016300 01  W-WDGXKEY-PERIOD-ROT.                                                
016400     03  FILLER              PIC X(4)    VALUE '2217'.                    
016500     03  FILLER              PIC X(1)    VALUE 'J'.                       
016600     03  FILLER              PIC X(25)   VALUE LOW-VALUE.                 
016700 01  W-WDGXKEY-2245-X.                                                    
016800     03 W-IDHTYP-2245        PIC X(4)    VALUE '2245'.                    
016900     03 FILLER               PIC X(26)   VALUE LOW-VALUE.                 
017000 01  WDF3.                                                                
017100     03  W-WDF301KY-X.                                                    
017200         05  W-IDLANDX2      PIC X(2)    VALUE SPACE.                     
017300         05  W-DADATUM-HELG  PIC 9(8)    VALUE ZERO.                      
017400         05  FILLER  REDEFINES W-DADATUM-HELG.                            
017500             07  W-DADATUM-HELG-SS       PIC 9(2).                        
017600             07  W-DADATUM-HELG-AAMMDD   PIC 9(6).                        
017700                                                                          
017800*-----                                                                    
017900 01  W-WDGXKEY-2257-X.                                                    
018000     03 W-IDHTYP-2257        PIC X(4)    VALUE '2257'.                    
018100     03 FILLER               PIC X(26)   VALUE LOW-VALUE.                 
018200                                                                          
018300 01  W-WDGXKEY-2258-X.                                                    
018400     03 W-IDLEVNR-SHIP-2258  PIC X(5)    VALUE SPACE.                     
018500                                                                          
018600 01  W-DAAVROP-2260-X.                                                    
018700     03 W-DAAVROP-2260       PIC 9(6)    VALUE ZERO.                      
018800                                                                          
018900 01  W-IDANSK-2260-X.                                                     
019000     03 W-IDANSK-2260        PIC S9(3)   VALUE ZERO COMP-3.               
019100                                                                          
019200 01  W-IDLEVNR-DC-X.                                                      
019300     03  W-IDLEVNR-DC        PIC X(5)    VALUE SPACE.                     
019400*-----                                                                    
019500                                                                          
019600     EJECT                                                                
019700 01  SWITCHAR.                                                            
019800     03  SW-MSG-OK           PIC X(1)    VALUE 'N'.                       
019900     03  SW-FAELT-IFYLLT     PIC X(1)    VALUE 'N'.                       
020000     03  SW-LEVSEGM-FINNS    PIC X(1)    VALUE 'N'.                       
020100     03  SW-HUVUDLEVERANTOER PIC X(1)    VALUE 'N'.                       
020200                                                                          
020300 01  SW-INDATA               PIC X(1).                                    
020400     88  INDATA-OK                       VALUE 'J'.                       
020500     88  INDATA-FEL                      VALUE 'N'.                       
020600                                                                          
020700 01  W-TESTA-INDATA          PIC X(1).                                    
020800     88  W-INDATA-FINNS                  VALUE 'J'.                       
020900                                                                          
021000 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
021100     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
021200     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
021300     SKIP3                                                                
021400*                                          REG-INFO FÖR KONTOLL AV        
021500*                                          UPPDAT.DATA REDIGERING         
021600*                                          AV UPPD. BILD                  
021700 01  W-REGISTER-INFO.                                                     
021800     03  W-MATINFO-KDAVT     PIC S9(3)               COMP-3.              
021900     03  W-MATINFO-KDLPSP    PIC S9(1)               COMP-3.              
022000     03  W-OMSPEC-KVBEST-PL  PIC S9(7)               COMP-3.              
022100     03  W-OMSPEC-TISPECST   PIC S9(7)               COMP-3.              
022200     03  W-LEVNR-KVBR        PIC S9(7)               COMP-3.              
022300     EJECT                                                                
022400 01  ARBETSAREOR.                                                         
022500                                                                          
022600     03  IX                  PIC S9(9)               COMP SYNC.           
022700     03  IX1                 PIC S9(9)               COMP SYNC.           
022800     03  IX2                 PIC S9(9)               COMP SYNC.           
022900     03  IX3                 PIC S9(9)               COMP SYNC.           
023000     03  IX-PLUS-1           PIC S9(9)               COMP SYNC.           
023100     03  IX-TOT              PIC S9(9)               COMP SYNC.           
023200     03  IX-GAM              PIC S9(9)               COMP SYNC.           
023300     03  IX-VECKA            PIC S9(9)               COMP SYNC.           
023400     03  IX-PP               PIC S9(9)               COMP SYNC.           
023500     03  IY                  PIC S9(9)               COMP SYNC.           
023600     03  IY-PERIOD           PIC S9(9)               COMP SYNC.           
023700     03  PIX                 PIC  9(2).                                   
023800     03  IX-AAPP             PIC  9(2).                                   
023900     03  ANT-LEVDAG          PIC  9(2).                                   
024000     SKIP3                                                                
024100     03 WS-TEMFSINF-X.                                                    
024200       10 W-A                    PIC X(9)  VALUE SPACE.                   
024300       10 FILLER                 PIC X     VALUE '/'.                     
024400       10 W-B                    PIC X(3)  VALUE SPACE.                   
024500       10 FILLER                 PIC X     VALUE '/'.                     
024600       10 W-C                    PIC X(3)  VALUE SPACE.                   
024700       10 FILLER                 PIC X     VALUE '/'.                     
024800       10 W-D                    PIC X(3)  VALUE SPACE.                   
024900       10 FILLER                 PIC X     VALUE '/'.                     
025000       10 W-E                    PIC 9(6)  VALUE ZERO.                    
025100       10 FILLER                 PIC X     VALUE '/'.                     
025200       10 W-F                    PIC 9(1)  VALUE ZERO.                    
025300       10 FILLER                 PIC X     VALUE '/'.                     
025400       10 W-G                    PIC 9(6)  VALUE ZERO.                    
025500       10 FILLER                 PIC X     VALUE '/'.                     
025600       10 W-H                    PIC 9(6)  VALUE ZERO.                    
025700       10 FILLER                 PIC X     VALUE '/'.                     
025800       10 W-I                    PIC 9(6)  VALUE ZERO.                    
025900       10 FILLER                 PIC X     VALUE '/'.                     
026000       10 W-J                    PIC X(2)  VALUE 'XX'.                    
026100       10 FILLER                 PIC X     VALUE '/'.                     
026200       10 W-K                    PIC X(3)  VALUE SPACE.                   
026300     03 FILLER               PIC X(16)   VALUE 'W-AVROP-TAB-RAD'.         
026400     03 W-AVROP-TAB-RAD      OCCURS 2.                                    
026500         05  W-PERIOD-TAB-RAD PIC 9(2).                                   
026600         05  W-AVROP-TAB-KOL OCCURS 5.                                    
026700             10  W-KVAVROP-TAB                                            
026800                             PIC S9(7)               COMP-3.              
026900                                                                          
027000     03  W-ORSKAS-TAB        OCCURS 3.                                    
027100         05  W-ORSAK-AENDRAD PIC X.                                       
027200         05  W-ORSAK-TAB-KOD PIC 9(2).                                    
027300     SKIP3                                                                
027400     03  W-KVAVROP-RED       PIC Z(5)9        VALUE ZERO.                 
027500     03  W-KDBEHX-PLAN       PIC X.                                       
027600     03  W-SKILJETECKEN      PIC X.                                       
027700     03  W-KVAVROP-ACC       PIC S9(7)               COMP-3.              
027800     03  W-ANTAL-LEVNR       PIC S9(3)               COMP-3.              
027900     03  W-ANTAL-VECKOR      PIC S9(3)               COMP-3.              
028000                                                                          
028100     03  W-HUVUDLEVNR        PIC X(5).                                    
028200     03  W-TILEVPL           PIC S9(7)               COMP-3.              
028300     03  W-KVBR              PIC S9(7)               COMP-3.              
028400     03  W-KVPB-PLAN         PIC S9(6)V9             COMP-3.              
028500     03  W-KVPB-SATS         PIC S9(6)V9             COMP-3.              
028600     03  W-AAVV-ADD          PIC S9(5)   COMP-3  VALUE ZERO.              
028700                                                                          
028800     03  W-DATUM-AKTUELLT    PIC S9(5)               COMP-3.              
028900                                                                          
029000     03  W-DATUM-AAVV        PIC 9(4).                                    
029100     03  FILLER              REDEFINES W-DATUM-AAVV.                      
029200         05  W-DATUM-AA      PIC 9(2).                                    
029300         05  W-DATUM-VV      PIC S9(2).                                   
029400                                                                          
029500     03  W-PER-PP            PIC 99 VALUE ZERO.                           
029600     03  W-PERIOD-AAPP       PIC 9(4).                                    
029700     03  FILLER              REDEFINES W-PERIOD-AAPP.                     
029800         05  W-PERIOD-AA     PIC 9(2).                                    
029900         05  W-PERIOD-PP     PIC 9(2).                                    
030000     03  W-PERIOD-AAPP-START PIC 9(4).                                    
030100                                                                          
030200     03  W-PER-AA            PIC S9(3)               COMP-3.              
030300     03  W-BER-PERIOD        PIC S9(5)               COMP-3.              
030400     03  W-START-PER-AAPP    PIC  9(4)   VALUE ZERO.                      
030500     03  W-START-PER-AA      PIC  9(2)   VALUE ZERO.                      
030600     03  W-START-PER-PP      PIC  9(2)   VALUE ZERO.                      
030700     03  W-VECKA             PIC  9(2)   VALUE ZERO.                      
030800     03 WS-AAPP              PIC 9(4)    VALUE ZERO.                      
030900     03 FILLER REDEFINES     WS-AAPP.                                     
031000        05 WS-AA             PIC 9(2).                                    
031100        05 WS-PP             PIC 9(2).                                    
031200     03  W-TIOMSPEC          PIC  9(6) VALUE ZERO.                        
031300     03  W-TIOMSPEC-X   REDEFINES W-TIOMSPEC.                             
031400         05  W-TIOMSPEC-1-2  PIC X(2).                                    
031500         05  FILLER          PIC 9(4).                                    
031600                                                                          
031700     03  W-FLJIT             PIC X     VALUE SPACE.                       
031800     03  W-KDLEVPLF          PIC X     VALUE SPACE.                       
031900                                                                          
032000     03  W-KDHF              PIC S9(1) COMP-3  VALUE ZERO.                
032100     03  W-TILPSP            PIC 9(4)  VALUE ZERO.                        
032200                                                                          
032300     03  W-DASPECST          PIC 9(6).                                    
032400     03  FILLER  REDEFINES W-DASPECST.                                    
032500         05  FILLER      PIC 9(2).                                        
032600         05  W-TISPECST  PIC 9(4).                                        
032700     03  W-DAAVROP-AVS       PIC 9(6).                                    
032800     03  FILLER  REDEFINES W-DAAVROP-AVS.                                 
032900         05  FILLER      PIC 9(2).                                        
033000         05  W-TIAVROP-AVS  PIC 9(4).                                     
033100     03  W-TIAAMMDD-AVS      PIC 9(6).                                    
033200     03  WS-IDLANDX2             PIC X(2)    VALUE SPACE.                 
033300                                                                          
033400*    03  WDGX2216    -COPY WDGX2216 -PRE W-.                              
033500                                                                          
033600     EJECT                                                                
033700 01  MESSAGE-CODES.                                                       
033800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
033900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
034000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
034100     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
034200     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
034300     03  FEL-LEVNR               PIC X(3)    VALUE '092'.                 
034400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
034500     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
034600     03  ERR-LEVNR-SAKNAS        PIC X(3)    VALUE '273'.                 
034700     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
034800     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
034900     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
035000     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
035100     03  END-O-I-REG             PIC X(3)    VALUE '308'.                 
035200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
035300     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
035400     03  INF-REFILL-PART         PIC X(3)    VALUE '434'.                 
035500     03  INF-SUPPL-BLOCKED       PIC X(3)    VALUE '438'.                 
035600     EJECT                                                                
035700 01      FELMEDDELANDE.                                                   
035800   03    FEL-1           PIC X(40)                                        
035900                VALUE 'ARTIKELNUMMER EJ NUMERISKT              '.         
036000   03    FEL-2           PIC X(40)                                        
036100                VALUE 'ARTIKEL SAKNAS I DATABAS                '.         
036200   03    FEL-3           PIC X(40)                                        
036300                VALUE 'LEVERANTÖRNUMMER EJ NUMERISKT           '.         
036400   03    FEL-4           PIC X(20)  VALUE 'TYP-KOD FEL         '.         
036500   03    FEL-5           PIC X(20)  VALUE 'PERIOD FELAKTIG     '.         
036600   03    FEL-6           PIC X(20)  VALUE 'ARTIKEL UTGÅNGEN    '.         
036700   03    FEL-7           PIC X(20)  VALUE 'ARTIKEL ERSATT      '.         
036800   03    FEL-9           PIC X(20)  VALUE 'FÖRSLAG SAKNAS      '.         
036900   03    FEL-20          PIC X(32)  VALUE                                 
037000                       'LEVERANTÖR SAKNAS PÅ BILD 2114'.                  
037100   03    FEL-31          PIC X(40)                                        
037200                 VALUE 'UPPDATRING EJ TILLÅTEN. ANV BILD 2103.'.          
037300*  ENGELSK TEXT                                                           
037400   03    FEL-11          PIC X(40)                                        
037500                VALUE 'PART NO.  NOT NUMERIC                   '.         
037600   03    FEL-41          PIC X(40)                                        
037700                VALUE 'FIELD NOT NUMERIC                       '.         
037800   03    FEL-12          PIC X(40)                                        
037900                VALUE 'PART NO.  MISSING IN DATA BASE          '.         
038000   03    FEL-13          PIC X(40)                                        
038100                VALUE 'SUPPLIER NO. NOT NUMERIC                '.         
038200   03    FEL-14          PIC X(20)  VALUE 'TYPE-CODE ERROR     '.         
038300   03    FEL-15          PIC X(20)  VALUE 'PERIOD NOT VALID    '.         
038400   03    FEL-16          PIC X(20)  VALUE 'PART EXPIRED        '.         
038500   03    FEL-17          PIC X(20)  VALUE 'PART REPLACED       '.         
038600   03    FEL-19          PIC X(20)  VALUE 'PROPOSAL MISSING    '.         
038700   03    FEL-21          PIC X(40)                                        
038710                 VALUE 'UPDATE NOT ALLOWED. USE SCREEN 2103.'.            
038900   03    FEL-120         PIC X(32) VALUE                                  
039000                       'SUPPLIER MISSING ON SCREEN 2114'.                 
039100 01      INFMEDDELANDE.                                                   
039200   03    INF-1           PIC X(20)  VALUE 'OMSPEC UTFÖRD       '.         
039300   03    INF-11          PIC X(20)  VALUE 'RESPEC. DONE        '.         
039400     SKIP3                                                                
039500*                                         MEDDELANDEN                     
039600 01  MEDDELANDEN.                                                         
039700     03  MED-1           PIC X(9)   VALUE 'SE LARMKÖ'.                    
039800     03  MED-11          PIC X(26)  VALUE 'INSPECT ALARM CUE'.            
039900     03  MED-2           PIC X(30)                                        
040000                         VALUE 'AVROP PÅ ICKE-SÄNDNINGSDAG'.              
040100     03  MED-12          PIC X(30)                                        
040200                         VALUE 'CALL-OFF ON NON-DELIVERY DAY'.            
040300     03  MED-3           PIC X(30)                                        
040400                         VALUE 'AVROP PÅ LEVERANTÖRS HELGDAG'.            
040500     03  MED-13          PIC X(30)                                        
040600                         VALUE 'CALL-OFF ON SUPPL BANK HOLIDAY'.          
040700                                                                          
040800*                                         KOMMENTAR-TEXTER                
040900 01  KOM-TEXTER.                                                          
041000     03  FILLER              PIC X(26)                                    
041100                         VALUE 'EJ HUVUDLEVERANTÖR        '.              
041200     03  FILLER              PIC X(26)                                    
041300                         VALUE 'ANNAT LEVNR FINNS         '.              
041400     03  FILLER              PIC X(26)                                    
041500                         VALUE ' NR 3 ANV EJ              '.              
041600     03  FILLER              PIC X(26)                                    
041700                         VALUE 'NYTT PB ÅÅVVD             '.              
041800     03  FILLER              PIC X(26)                                    
041900                         VALUE 'PROGNOSVARNING            '.              
042000     03  FILLER              PIC X(26)                                    
042100                         VALUE 'UTREDNINGSSALDO           '.              
042200     03  FILLER              PIC X(26)                                    
042300                         VALUE 'FÖR UPPDATERING TRYCK PF11'.              
042400     03  FILLER              PIC X(26)                                    
042500                         VALUE 'PASSIV ARTIKEL            '.              
042600     03  FILLER              PIC X(26)                                    
042700                         VALUE 'DIREKTLEVERANS > 0        '.              
042800     03  FILLER              PIC X(26)                                    
042900                         VALUE SPACE.                                     
043000*    ENGELSK TEXT                                                         
043100     03  FILLER              PIC X(26)                                    
043200                         VALUE 'NOT MAIN SUPPLIER         '.              
043300     03  FILLER              PIC X(26)                                    
043400                         VALUE 'ANOTHER SUPPLIER EXIST    '.              
043500     03  FILLER              PIC X(26)                                    
043600                         VALUE ' NR 3 ANV EJ              '.              
043700     03  FILLER              PIC X(26)                                    
043800                         VALUE 'NEW  PB YYWWD             '.              
043900     03  FILLER              PIC X(26)                                    
044000                         VALUE 'PROGNOSIS WARNING         '.              
044100     03  FILLER              PIC X(26)                                    
044200                         VALUE 'INVESTIGATION BALANCE     '.              
044300     03  FILLER              PIC X(26)                                    
044400                         VALUE 'TO UPDATE   PRESS PF11    '.              
044500     03  FILLER              PIC X(26)                                    
044600                         VALUE 'PART PASSIVE              '.              
044700     03  FILLER              PIC X(26)                                    
044800                         VALUE 'DIR. DEL. > 0             '.              
044900     03  FILLER              PIC X(26)                                    
045000                         VALUE SPACE.                                     
045100                                                                          
045200 01  KOMMENTAR       REDEFINES KOM-TEXTER.                                
045300     03  KOMMENTAR-TEXT  OCCURS 20                                        
045400                             PIC X(26).                                   
045500     SKIP3                                                                
045600 01  W-MESSAGE-BOTTOM.                                                    
045700     03  W-MESSAGE-BOTTOM-1  PIC X(18)  VALUE SPACE.                      
045800     03  FILLER              PIC X(01)  VALUE SPACE.                      
045900     03  W-MESSAGE-BOTTOM-2  PIC X(18)  VALUE SPACE.                      
046000     03  FILLER              PIC X(01)  VALUE SPACE.                      
046100     03  W-MESSAGE-BOTTOM-3  PIC X(18)  VALUE SPACE.                      
046200     EJECT                                                                
046300*                                                                         
046400*                                                                         
046500 01  PERIODINDELNING.                                                     
046600     03  PERIOD-TAB          OCCURS 2.                                    
046700         05  PER-TISEKEL     PIC  9(2).                                   
046800         05  PER-START-AAVV  PIC  9(4).                                   
046900         05  FILLER REDEFINES PER-START-AAVV.                             
047000           07 PER-START-AA   PIC  9(2).                                   
047100           07 PER-START-VV   PIC  9(2).                                   
047200                                                                          
047300         05  PER-SLUT-AAVV   PIC  9(4).                                   
047400         05  FILLER REDEFINES PER-SLUT-AAVV.                              
047500           07 PER-SLUT-AA    PIC  9(2).                                   
047600           07 PER-SLUT-VV    PIC  9(2).                                   
047700                                                                          
047800         05  PER-ANT-VV      PIC  9(1).                                   
047900                                                                          
048000         05  PER-AAPP        PIC 9(4).                                    
048100         05  FILLER          REDEFINES PER-AAPP.                          
048200             07  PER-AAPP-AA PIC 9(2).                                    
048300             07  PER-AAPP-PP PIC 9(2).                                    
048400*                                                                         
048500 01  WS-AAAAVV               PIC 9(6).                                    
048600 01  FILLER  REDEFINES WS-AAAAVV.                                         
048700     03  WS-TISEKEL          PIC 9(2).                                    
048800     03  WS-AAVV             PIC 9(4).                                    
048900     EJECT                                                                
049000*    -COPY W221FLEV                                                       
049100     EJECT                                                                
049200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
049300*01 -COPY WMEDAREA                                                        
049400     EJECT                                                                
049500*01      -COPY WDATAREA.                                                  
049600     EJECT                                                                
049700*                         ****    PARAMTERAR TILL W005INIT                
049800*01      -COPY WMSGINIT.                                                  
049900     EJECT                                                                
050000 01  TP-WS.                                                               
050100   03    FILLER              PIC X(16)   VALUE '   TP - AREAOR  '.        
050200     SKIP3                                                                
050300*01  MID       -COPY W2I13901  -PRE MID-.                                 
050400     EJECT                                                                
050500*01  -COPY WMSGAREA.                                                      
050600     EJECT                                                                
050700*    03  MOD   -COPY W2O13901  -PRE MOD-  -RED MSG-AREA.                  
050800     EJECT                                                                
050900*01  -COPY WMFSAREA.                                                      
051000     EJECT                                                                
051100 01  FILLER                  PIC X(16)   VALUE '     IMS-WS     '.        
051200                                                                          
051300 01  STATUS-WS               PIC X(2).                                    
051400     88  SEGMENT-FINNS                   VALUE '  '.                      
051500     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
051600                                                                          
051700 01  GODK-STATUSKODER.                                                    
051800     03  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC X(2).              
051900                                                                          
052000 01      SSA1                PIC X(128).                                  
052100 01      SSA2                PIC X(64).                                   
052200 01      SSA3                PIC X(64).                                   
052300     EJECT                                                                
052400*01  -COPY W0003                                                          
052500     EJECT                                                                
052600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA  '.             
052700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD901'.             
052800 01  DLI-IO-WDD901.                                                       
052900*    03  -COPY WDD901                                                     
053000     EJECT                                                                
053100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD902'.             
053200 01  DLI-IO-WDD902.                                                       
053300*    03  -COPY WDD902 -PRE LEVNR-                                         
053400     EJECT                                                                
053500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD904'.             
053600 01  DLI-IO-WDD904.                                                       
053700*    03  -COPY WDD904 -PRE OMSPEC-                                        
053800     EJECT                                                                
053900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD905'.             
054000 01  DLI-IO-WDD905.                                                       
054100*    03  -COPY WDD905 -PRE AVROP-                                         
054200     EJECT                                                                
054300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
054400 01  DLI-IO-WDK601.                                                       
054500*    03  -COPY WDK601                                                     
054600     EJECT                                                                
054700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
054800 01  DLI-IO-WDK611.                                                       
054900*    03  -COPY WDK611                                                     
055000     EJECT                                                                
055100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK623'.             
055200 01  DLI-IO-WDK623.                                                       
055300*    03  -COPY WDK623                                                     
055400     EJECT                                                                
055500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD311'.             
055600 01  DLI-IO-WDD311.                                                       
055700*    03  -COPY WDD311                                                     
055800     EJECT                                                                
055900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDF101'.             
056000 01  DLI-IO-WDF101.                                                       
056100*    03  -COPY WDF101                                                     
056200     EJECT                                                                
056300 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-F106'.         
056400     SKIP3                                                                
056500 01  DLI-IO-AREA-F106.                                                    
056600     SKIP2                                                                
056700*    03  WLLEVA14 -COPY WDF106                                            
056800     EJECT                                                                
056900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-F301'.         
057000     SKIP3                                                                
057100 01  DLI-IO-AREA-F301.                                                    
057200     SKIP2                                                                
057300*    03  -COPY WDF301                                                     
057400     EJECT                                                                
057500 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-F311'.         
057600     SKIP3                                                                
057700 01  DLI-IO-AREA-F311.                                                    
057800     SKIP2                                                                
057900*    03  -COPY WDF311                                                     
058000     EJECT                                                                
058100 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-2216'.         
058200 01  DLI-IO-2216.                                                         
058300*    03  -COPY WDGX2216                                                   
058400     EJECT                                                                
058500 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-2218'.         
058600 01  DLI-IO-2218.                                                         
058700*    03  -COPY WDGX2218                                                   
058800     EJECT                                                                
058900 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-2246'.         
059000 01  DLI-IO-2246.                                                         
059100*    03 -COPY WDGX2246    -PRE XXCZ-                                      
059200     EJECT                                                                
059300 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDGX2258'.          
059400 01  DLI-IO-WDGX2258.                                                     
059500*    03 -COPY WDGX2258                                                    
059600     EJECT                                                                
059700 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDGX2260'.          
059800 01  DLI-IO-WDGX2260.                                                     
059900*    03 -COPY WDGX2260                                                    
060000                                                                          
060100 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDB601'.            
060200 01  DLI-IO-B601.                                                         
060300*  03   -COPY WDB601                                                      
060400     EJECT                                                                
060500 LINKAGE SECTION.                                                         
060600     SKIP3                                                                
060700*01  -COPY W0009 -PRE MSG-                                                
060800     EJECT                                                                
060900*01  -COPY W0008 -PRE USEA-.                                              
061000     05  FILLER              PIC X.                                       
061100     EJECT                                                                
061200*01  -COPY W0008 -PRE WDK6-.                                              
061300     05  FILLER              PIC X.                                       
061400     EJECT                                                                
061500*01  -COPY W0008 -PRE WDD9-.                                              
061600     05  FILLER              PIC X.                                       
061700     EJECT                                                                
061800*01  -COPY W0008 -PRE WDD3-.                                              
061900     05  FILLER              PIC X.                                       
062000     EJECT                                                                
062100*01  -COPY W0008 -PRE WDF1-.                                              
062200     05  FILLER              PIC X.                                       
062300     EJECT                                                                
062400*01  -COPY W0008 -PRE WDF3-.                                              
062500     05  FILLER              PIC X.                                       
062600     EJECT                                                                
062700*01  -COPY W0008 -PRE XXBL-.                                              
062800     05  FILLER              PIC X.                                       
062900     EJECT                                                                
063000*01  -COPY W0008 -PRE XXBK-.                                              
063100     05  FILLER              PIC X.                                       
063200     EJECT                                                                
063300*01  -COPY W0008 -PRE XXCZ-.                                              
063400     05  FILLER              PIC X.                                       
063500     EJECT                                                                
063600*01  -COPY W0008 -PRE 2257-.                                              
063700     05  FILLER              PIC X.                                       
063800     EJECT                                                                
063900*01  -COPY W0008 -PRE WDB6-.                                              
064000     05  FILLER              PIC X.                                       
064100     EJECT                                                                
064200                                                                          
064300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
064400                          WDK6-PCB WDD9-PCB WDD3-PCB WDF1-PCB             
064500                          WDF3-PCB XXBL-PCB XXBK-PCB                      
064600                          XXCZ-PCB 2257-PCB WDB6-PCB.                     
064700     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
064800                          WDK6-PCB WDD9-PCB WDD3-PCB WDF1-PCB             
064900                          WDF3-PCB XXBL-PCB XXBK-PCB                      
065000                          XXCZ-PCB 2257-PCB WDB6-PCB.                     
065100                                                                          
065200     PERFORM IMS-GET-MSG                                                  
065300     IF SEGMENT-FINNS                                                     
065400       PERFORM A-KONTROLL-NYCKLAR-OCH-INIT                                
065500                                                                          
065600       IF INDATA-OK                                                       
065700         PERFORM S1-SECURITY-CHECK-ART-LEV                                
065800         IF PASSED-SECURITY-CHECK                                         
065900                                                                          
066000           IF MFS-UPDATE                                                  
066100             PERFORM G-KOLLA-INPUT                                        
066200             IF INDATA-OK                                                 
066300               PERFORM H-UPPDATERA                                        
066400             END-IF                                                       
066500           ELSE                                                           
066600             IF MFS-FIRST                                                 
066700               PERFORM C-FOERSTA-SIDA                                     
066800             ELSE                                                         
066900               PERFORM E-SAMMA-SIDA                                       
067000             END-IF                                                       
067100           END-IF                                                         
067200           IF INDATA-OK                                                   
067300             PERFORM F-LAES-VISA-INFO                                     
067400           END-IF                                                         
067500                                                                          
067600         END-IF                                                           
067700       END-IF                                                             
067800       PERFORM D-STAENG-EJ-GILTIG-VECKA                                   
067900       MOVE W-MESSAGE-BOTTOM   TO MOD-MESSAGE-BOTTOM                      
068000       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O13901 + 4                      
068100       PERFORM IMS-INSERT-MSG                                             
068200     END-IF                                                               
068300     MOVE ZERO TO RETURN-CODE                                             
068400     GOBACK                                                               
068500     .                                                                    
068600     EJECT                                                                
068700 A-KONTROLL-NYCKLAR-OCH-INIT SECTION.                                     
068800     MOVE 'A-KONTROLL-NYCKLAR-OCH-INIT' TO CURRENT-SECTION                
068900                                                                          
069000*    KONTROLL AV NYCKLAR OCH ATT MID 2139 MOTTAGITS              *        
069100                                                                          
069200     MOVE JA TO SW-INDATA                                                 
069300     MOVE NEJ TO SW-FAELT-IFYLLT                                          
069400                                                                          
069500     IF MSG-DUBBLA-TRANSKODER                                             
069600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I13901                 
069700       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
069800       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
069900     ELSE                                                                 
070000       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W2I13901                   
070100       MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                    
070200       MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                   
070300     END-IF                                                               
070400                                                                          
070500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
070600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
070700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
070800     MOVE ALL '+' TO MSGI-WMSGINIT                                        
070900     MOVE '001'             TO MSGI-KDCALL                                
071000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
071100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
071200     MOVE '2139'            TO MSGI-IDTRANS                               
071300     MOVE MID-KDBEHX-PLAN-IN   TO PAH-KDBEHX-PLAN-IN                      
071400     MOVE MID-KDBEHX-PLAN-UT   TO PAH-KDBEHX-PLAN-UT                      
071500     IF EGEN-MID                                                          
071600       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
071700     ELSE                                                                 
071800       IF  MID-IDARTNR-IN NUMERIC                                         
071900       AND MID-IDARTNR-IN > ZERO                                          
072000         MOVE MID-IDARTNR-IN                                              
072100                            TO MSGI-IDARTNR                               
072200       END-IF                                                             
072300       MOVE ALL '+' TO MID-IDLEVNR-IN                                     
072400*PAH ?                 MID-KDBEHX-PLAN-IN                                 
072500                       MID-PERIOD-IN                                      
072600       MOVE SPACE   TO MID-IDLEVNR-UT                                     
072700*PAH?                  MID-KDBEHX-PLAN-UT                                 
072800                       MID-PERIOD-UT                                      
072900     END-IF                                                               
073000*    MOVE SPACE              TO MSGI-SPAR-AREA                            
073100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
073200                                                                          
073300     MOVE MSGI-IDLAND-SPR    TO MED-IDSKYLT                               
073400                                                                          
073500     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
073600     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
073700                                                                          
073800     IF MID-KDBEHX-PLAN-IN = 'E'                                          
073900        MOVE 'G'               TO MID-KDBEHX-PLAN-IN                      
074000     END-IF                                                               
074100     IF MID-KDBEHX-PLAN-IN = 'P'                                          
074200        MOVE 'F'               TO MID-KDBEHX-PLAN-IN                      
074300     END-IF                                                               
074400     IF MID-KDBEHX-PLAN-UT = 'E'                                          
074500        MOVE 'G'               TO MID-KDBEHX-PLAN-UT                      
074600     END-IF                                                               
074700     IF MID-KDBEHX-PLAN-UT = 'P'                                          
074800        MOVE 'F'               TO MID-KDBEHX-PLAN-UT                      
074900     END-IF                                                               
075000                                                                          
075100     IF MID-KDBEHX-PLAN-IN = ALL '+' OR SPACE                             
075200       MOVE MID-KDBEHX-PLAN-UT TO KDBEHX-PLAN-WS                          
075300     ELSE                                                                 
075400       MOVE MID-KDBEHX-PLAN-IN TO KDBEHX-PLAN-WS                          
075500     END-IF                                                               
075600                                                                          
075700     IF KDBEHX-PLAN-WS NOT = 'G'                                          
075800********MOVE 'F'               TO MID-KDBEHX-PLAN-UT                      
075900        MOVE 'G'               TO MID-KDBEHX-PLAN-UT                      
076000                                  KDBEHX-PLAN-WS                          
076100     END-IF                                                               
076200                                                                          
076300     IF KDBEHX-PLAN-WS = FORSLAG                                          
076400        MOVE 1               TO W-KDAVROP                                 
076500     ELSE                                                                 
076600        MOVE 2               TO W-KDAVROP                                 
076700     END-IF                                                               
076800                                                                          
076900     IF MID-IDLEVNR-IN = ALL '+'                                          
077000       MOVE MID-IDLEVNR-UT   TO IDLEVNR-WS                                
077100     ELSE                                                                 
077200       MOVE MID-IDLEVNR-IN   TO IDLEVNR-WS                                
077300     END-IF                                                               
077400                                                                          
077500     IF MID-PERIOD-IN = ALL '+'                                           
077600       MOVE MID-PERIOD-UT    TO PERIOD-WS                                 
077700       INSPECT PERIOD-WS REPLACING LEADING SPACE BY ZERO                  
077800     ELSE                                                                 
077900       MOVE MID-PERIOD-IN    TO PERIOD-WS                                 
078000     END-IF                                                               
078100                                                                          
078200     MOVE LOW-VALUE TO MOD-W2O13901                                       
078300     MOVE '2139'    TO MOD-IDTRANS                                        
078400     MOVE 'W2O139N1' TO MFS-IDMOD                                         
078500                                                                          
078600     MOVE IDARTNR-WS         TO MOD-IDARTNR-UT                            
078700     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
078800     MOVE IDLEVNR-WS         TO MOD-IDLEVNR-UT                            
078900                                MOD-IDLEVNR-SHIP-UT                       
079000                                                                          
079100     MOVE KDBEHX-PLAN-WS     TO MOD-KDBEHX-PLAN-UT                        
079200                                                                          
079300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
079400                             MOD-IDLEVNR-IN                               
079500                             MOD-KDBEHX-PLAN-IN                           
079600                             MOD-PERIOD-IN                                
079700                             MOD-MESSAGE                                  
079800                             MOD-MESSAGE-BOTTOM                           
079900*FIX TEST                                                                 
080000*    STRING MID-IDARTNR-IN ' ' PAH-KDBEHX-PLAN-IN                         
080100*                          ' ' PAH-KDBEHX-PLAN-UT                         
080200*           ' ' MID-PERIOD-IN                                             
080300*           DELIMITED BY SIZE INTO MOD-MESSAGE                            
080400*FIX TEST                                                                 
080500                                                                          
080600     IF IDARTNR-WS NOT NUMERIC                                            
080800       MOVE FEL-11 TO MOD-MESSAGE                                         
081200       MOVE NEJ TO SW-INDATA                                              
081300     ELSE                                                                 
081400         IF KDBEHX-PLAN-WS NOT = GALLANDE                                 
081500                   AND KDBEHX-PLAN-WS NOT = FORSLAG                       
081600                     AND KDBEHX-PLAN-WS NOT = SPACE                       
081800           MOVE FEL-14 TO MOD-MESSAGE                                     
082200           MOVE NEJ TO SW-INDATA                                          
082300         END-IF                                                           
082400     END-IF                                                               
082500     PERFORM AA-DAGENS-DATUM                                              
082600     IF PERIOD-WS = ZERO                                                  
082700        MOVE W-START-PER-AAPP                                             
082800                             TO PERIOD-WS                                 
082900     END-IF                                                               
082910                                                                          
083000     MOVE PERIOD-WS          TO MOD-PERIOD-UT                             
083100     PERFORM AB-PERIODINDELNING-TAB                                       
083200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
083210                                                                          
083213     MOVE 'AARP'             TO DAT-KDDATFORM                             
083220     MOVE PERIOD-WS          TO DAT-I-TIDATUM                             
083230     CALL WDATKONV USING DAT-KDDATFORM                                    
083231                         DAT-I-TIDATUM                                    
083232                         DAT-O-TIDATUM                                    
083233                         DAT-KDSVAR                                       
083234     IF DAT-KDSVAR-OK                                                     
083241        MOVE 2             TO WORK-KDCALL                                 
083242        MOVE '11'          TO WORK-IDDC                                   
083243        MOVE DAT-TIAAMMDD  TO WORK-TIAAMMDD-FOM                           
083245        MOVE +1            TO WORK-KVWORKD                                
083246        CALL WORKDAY USING  WORK-KDCALL                                   
083247                WORK-DATE-AREA WORK-KDSVAR                                
083250        IF WORK-KDSVAR-FEL                                                
083260           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
083261           CALL WMEDKONV USING MED-WMEDAREA                               
083262           MOVE MED-MFSFEL TO MOD-MESSAGE                                 
083263           MOVE NEJ        TO SW-INDATA                                   
083272        END-IF                                                            
083280     END-IF                                                               
083300     CONTINUE.                                                            
083400     EJECT                                                                
083500 AA-DAGENS-DATUM SECTION.                                                 
083600     MOVE 'AA-DAGENS-DATUM '   TO CURRENT-SECTION                         
083700                                                                          
083800     MOVE 'IDAG  '           TO DAT-KDDATFORM                             
083900                                                                          
084000     CALL WDATKONV USING DAT-KDDATFORM                                    
084100                         DAT-I-TIDATUM                                    
084200                         DAT-O-TIDATUM                                    
084300                         DAT-KDSVAR                                       
084400                                                                          
084500     MOVE DAT-TIAA           TO W-DATUM-AA                                
084600                                W-START-PER-AA                            
084700     MOVE DAT-TIRP           TO W-START-PER-PP                            
084800     MOVE DAT-TIVV           TO W-DATUM-VV                                
084900     MOVE DAT-TIAARP         TO W-PERIOD-AAPP                             
085000                                W-START-PER-AAPP                          
085100                                                                          
085200     MOVE W-DATUM-AAVV       TO W-DATUM-AKTUELLT                          
085300                                                                          
085400     MOVE DAT-TIAAMMDD       TO WS-DAGENS-DATUM                           
085500     .                                                                    
085600     EJECT                                                                
085700 AB-PERIODINDELNING-TAB SECTION.                                          
085800     MOVE 'AB-PERIODINDELNING-TAB '  TO CURRENT-SECTION                   
085900                                                                          
086000     MOVE PERIOD-WS          TO WS-AAPP                                   
086100     MOVE 'AARP'             TO DAT-KDDATFORM                             
086200     MOVE +1                 TO IX-PP                                     
086300                                                                          
086400     PERFORM UNTIL IX-PP      >  2                                        
086500       IF WS-PP = 13                                                      
086600          MOVE +1             TO WS-PP                                    
086700          ADD  +1             TO WS-AA                                    
086800       END-IF                                                             
086900       MOVE WS-AAPP          TO DAT-I-TIDATUM                             
087000       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
087100                             DAT-O-TIDATUM DAT-KDSVAR                     
087200       IF DAT-KDSVAR-OK                                                   
087300          MOVE DAT-TISEKEL   TO PER-TISEKEL (IX-PP)                       
087400          MOVE DAT-TIAA      TO PER-START-AA (IX-PP)                      
087500                                PER-SLUT-AA (IX-PP)                       
087600          MOVE DAT-TIVV      TO PER-START-VV (IX-PP)                      
087700          MOVE DAT-KVVIPER   TO PER-ANT-VV   (IX-PP)                      
087800          MOVE DAT-TIAARP    TO PER-AAPP     (IX-PP)                      
087900          COMPUTE PER-SLUT-VV (IX-PP) =                                   
088000                  PER-START-VV (IX-PP) + DAT-KVVIPER - 1                  
088100          ADD +1             TO WS-PP IX-PP                               
088200       ELSE                                                               
088400          MOVE FEL-15 TO MOD-MESSAGE                                      
088800          MOVE NEJ TO SW-INDATA                                           
088900          MOVE +3         TO IX-PP                                        
089000       END-IF                                                             
089100     END-PERFORM                                                          
089200     MOVE W-START-PER-AAPP   TO TMP1-YYPP                                 
089300     MOVE PER-AAPP (1)       TO TMP2-YYPP                                 
089400     PERFORM WY2000P6                                                     
089500*  * IF TMP2-YYPP < TMP1-YYPP                                             
089600*  *    IF MSGI-IDLAND-SPR = 'GB'                                         
089700*  *       MOVE FEL-15       TO MOD-MESSAGE                               
089800*  *    ELSE                                                              
089900*  *       MOVE FEL-5        TO MOD-MESSAGE                               
090000*  *    END-IF                                                            
090100*  *    MOVE NEJ             TO SW-INDATA                                 
090200*  * END-IF                                                               
090300     .                                                                    
090400     EJECT                                                                
090500 C-FOERSTA-SIDA SECTION.                                                  
090600                                                                          
090700     PERFORM MFS-RENSA-FAELT-IN                                           
090800     .                                                                    
090900     EJECT                                                                
091000 D-STAENG-EJ-GILTIG-VECKA SECTION.                                        
091100     MOVE 'D-STAENG-EJ-GILTIG-VECKA '  TO CURRENT-SECTION                 
091200                                                                          
091300     MOVE 1                  TO IX                                        
091400                                                                          
091500     PERFORM UNTIL IX > 2                                                 
091600                                                                          
091700       IF PER-ANT-VV (IX) < 5                                             
091800          MOVE MFS-STAENG-FAELT-NOMOD                                     
091900                             TO MOD-KVAVROP-DAG-IN-ATTR (IX, 5, 1)        
092000                                MOD-KVAVROP-DAG-IN-ATTR (IX, 5, 2)        
092100                                MOD-KVAVROP-DAG-IN-ATTR (IX, 5, 3)        
092200                                MOD-KVAVROP-DAG-IN-ATTR (IX, 5, 4)        
092300                                MOD-KVAVROP-DAG-IN-ATTR (IX, 5, 5)        
092400          MOVE SPACE         TO MOD-KVAVROP-TAB (IX, 5)                   
092500       END-IF                                                             
092600       ADD 1                 TO IX                                        
092700     END-PERFORM                                                          
092800     .                                                                    
092900     EJECT                                                                
093000 E-SAMMA-SIDA SECTION.                                                    
093100     MOVE 'E-SAMMA-SIDA'  TO CURRENT-SECTION                              
093200                                                                          
093300     IF MID-INPUT = ALL '+'                                               
093400       PERFORM MFS-RENSA-FAELT-IN                                         
093500     ELSE                                                                 
093600       IF EGEN-MID OR HELP-MID                                            
093700         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
093800         CALL WMEDKONV USING MED-WMEDAREA                                 
093900         MOVE MED-MFSINF TO W-MESSAGE-BOTTOM                              
094000                                                                          
094100         PERFORM EA-MID-INDATA-TILL-MOD                                   
094200       ELSE                                                               
094300         PERFORM MFS-RENSA-FAELT-IN                                       
094400       END-IF                                                             
094500     END-IF                                                               
094600     .                                                                    
094700 EA-MID-INDATA-TILL-MOD SECTION.                                          
094800     MOVE 'EA-MID-INDATA-TILL-MOD '  TO CURRENT-SECTION                   
094900* * * * * FÖR VARJE MID-FÄLT                                              
095000* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
095100* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
095200                                                                          
095300                                                                          
095400     MOVE 1 TO IX1                                                        
095500     PERFORM UNTIL IX1 > 2                                                
095600        MOVE 1         TO IX2                                             
095700        PERFORM UNTIL IX2 > 5                                             
095800           MOVE 1      TO IX3                                             
095900           PERFORM UNTIL IX3 > 5                                          
096000              IF MID-KVAVROP-DAG (IX1, IX2, IX3) = ALL '+'                
096100                MOVE MFS-RENSA-FAELT                                      
096200                       TO MOD-KVAVROP-DAG-IN (IX1, IX2, IX3)              
096300              ELSE                                                        
096400                MOVE MFS-ADD-LAES-IN-FAELT                                
096500                       TO MOD-KVAVROP-DAG-IN-ATTR (IX1, IX2, IX3)         
096600                MOVE MFS-ROER-EJ-FAELT                                    
096700                       TO MOD-KVAVROP-DAG-IN (IX1, IX2, IX3)              
096800              END-IF                                                      
096900              ADD 1    TO IX3                                             
097000           END-PERFORM                                                    
097100           ADD 1       TO IX2                                             
097200        END-PERFORM                                                       
097300        ADD 1          TO IX1                                             
097400     END-PERFORM                                                          
097500     .                                                                    
097600     EJECT                                                                
097700 F-LAES-VISA-INFO SECTION.                                                
097800     MOVE 'F-LAES-VISA-INFO '  TO CURRENT-SECTION                         
097900                                                                          
098000     MOVE JA TO SW-MSG-OK                                                 
098100     MOVE IDARTNR-WS TO W-IDARTNR                                         
098200     MOVE IDLEVNR-WS TO W-IDLEVNR                                         
098300     MOVE KDBEHX-PLAN-WS TO W-KDBEHX-PLAN                                 
098400                                                                          
098500     PERFORM IMS-GU-K601                                                  
098600     IF  SEGMENT-FINNS                                                    
098700       IF  ART-KDERS-UTG = ZERO                                           
098800         PERFORM FA-UPPDAT-BILD-FRAN-ARTIKELREG                           
098900         PERFORM FB-UPPDAT-BILD-FRAN-LEVREG                               
099000         IF SW-MSG-OK = JA                                                
099100           IF  W-KDBEHX-PLAN = FORSLAG                                    
099200             MOVE MFS-FORMATETS-ATTR                                      
099300                             TO MOD-IDARTNR-IN-ATTR                       
099400           ELSE                                                           
099500             EVALUATE TRUE                                                
099600             WHEN W-KDBEHX-PLAN = GALLANDE                                
099700               MOVE MFS-OEPPNA-NUM-FAELT                                  
099800                               TO MOD-IDARTNR-IN-ATTR                     
099900             END-EVALUATE                                                 
100000           END-IF                                                         
100100         END-IF                                                           
100200       ELSE                                                               
100300         MOVE NEJ            TO SW-INDATA                                 
100400         IF  ART-KDERS-UTG < 20                                           
100600            MOVE FEL-16 TO MOD-MESSAGE                                    
101000         ELSE                                                             
101200            MOVE FEL-17 TO MOD-MESSAGE                                    
101600         END-IF                                                           
101700         PERFORM IMS-GNP-K611                                             
101800         IF SEGMENT-FINNS                                                 
101900           MOVE CLAG-IDANSK TO W-IDANSK-L                                 
102000         END-IF                                                           
102100       END-IF                                                             
102200     ELSE                                                                 
102400        MOVE FEL-12 TO MOD-MESSAGE                                        
102800     END-IF                                                               
102900     .                                                                    
103000     EJECT                                                                
103100 FA-UPPDAT-BILD-FRAN-ARTIKELREG SECTION.                                  
103200     MOVE 'FA-UPPDAT-BILD-FRAN-ARTIKELREG ' TO CURRENT-SECTION            
103300                                                                          
103400     MOVE ART-IDLEVNR     TO W-HUVUDLEVNR                                 
103500                                                                          
103600     IF  W-IDLEVNR = SPACE                                                
103700       MOVE ART-IDLEVNR TO W-IDLEVNR                                      
103800                           IDLEVNR-WS                                     
103900                           MOD-IDLEVNR-UT                                 
104000     END-IF                                                               
104100     IF  W-IDLEVNR = ART-IDLEVNR                                          
104200       MOVE JA TO SW-HUVUDLEVERANTOER                                     
104300     ELSE                                                                 
104400       MOVE NEJ TO SW-HUVUDLEVERANTOER                                    
104500     END-IF                                                               
104600     MOVE ART-REKSIFFR     TO MOD-REKSIFFR                                
104700     MOVE STRECK           TO MOD-STRECK-1                                
104800*                                                                         
104900     IF W-IDLEVNR = ART-IDLEVNR                                           
105000       PERFORM IMS-GNP-K611                                               
105100       IF SEGMENT-FINNS                                                   
105200         MOVE CLAG-IDLEVNR-SHIP    TO MOD-IDLEVNR-SHIP-UT                 
105300       ELSE                                                               
105400         PERFORM IMS-GNP-K623                                             
105500         MOVE NEJ TO TRAFF                                                
105600         PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF = JA                       
105700           IF W-IDLEVNR = AVT-IDLEVNR-AVT                                 
105800             MOVE AVT-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP-UT                 
105900             MOVE JA TO TRAFF                                             
106000           END-IF                                                         
106100           PERFORM IMS-GNP-K623                                           
106200         END-PERFORM                                                      
106300         IF TRAFF = NEJ                                                   
106400           MOVE W-IDLEVNR          TO MOD-IDLEVNR-SHIP-UT                 
106500         END-IF                                                           
106600       END-IF                                                             
106700     END-IF                                                               
106800*                                                                         
107000     MOVE 'GB'     TO W-IDSKYLT                                           
107400                                                                          
107500     PERFORM IMS-GU-D311                                                  
107600     MOVE TEXT-BEART        TO MOD-BEART-SVE                              
107700                                                                          
107800     MOVE SPACE             TO MOD-AVS-DAG (1)                            
107900                               MOD-AVS-DAG (2)                            
108000                               MOD-AVS-DAG (3)                            
108100                               MOD-AVS-DAG (4)                            
108200                               MOD-AVS-DAG (5)                            
108300                                                                          
108400     MOVE W-IDLEVNR           TO W-SAVE-IDLEVNR                           
108500     PERFORM IMS-GNP-K611                                                 
108600     IF W-IDLEVNR = W-HUVUDLEVNR                                          
108700       MOVE CLAG-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP-UT                      
108800                                 W-IDLEVNR                                
108900     ELSE                                                                 
109000       MOVE SPACE             TO MOD-IDLEVNR-SHIP-UT                      
109100     END-IF                                                               
109200                                                                          
109300     MOVE ZERO        TO ANT-LEVDAG                                       
109400     IF CLAG-TILEVDAG (1) > ZERO                                          
109500        MOVE 'MO'     TO MOD-AVS-DAG (1)                                  
109600        ADD +1        TO ANT-LEVDAG                                       
109700     END-IF                                                               
109800     IF CLAG-TILEVDAG (2) > ZERO                                          
109900        MOVE 'TU'     TO MOD-AVS-DAG (2)                                  
110000        ADD +1        TO ANT-LEVDAG                                       
110100     END-IF                                                               
110200     IF CLAG-TILEVDAG (3) > ZERO                                          
110300        MOVE 'WE'     TO MOD-AVS-DAG (3)                                  
110400        ADD +1        TO ANT-LEVDAG                                       
110500     END-IF                                                               
110600     IF CLAG-TILEVDAG (4) > ZERO                                          
110700        MOVE 'TH'     TO MOD-AVS-DAG (4)                                  
110800        ADD +1        TO ANT-LEVDAG                                       
110900     END-IF                                                               
111000     IF CLAG-TILEVDAG (5) > ZERO                                          
111100        MOVE 'FR'     TO MOD-AVS-DAG (5)                                  
111200        ADD +1        TO ANT-LEVDAG                                       
111300     END-IF                                                               
111400                                                                          
111500     PERFORM IMS-GU-F101                                                  
111600     IF SEGMENT-FINNS                                                     
111700        IF ANT-LEVDAG = ZERO                                              
111800           IF LEV-TILEVDAG (1) > ZERO                                     
111900              MOVE 'MO'     TO MOD-AVS-DAG (1)                            
112000           END-IF                                                         
112100           IF LEV-TILEVDAG (2) > ZERO                                     
112200              MOVE 'TU'     TO MOD-AVS-DAG (2)                            
112300           END-IF                                                         
112400           IF LEV-TILEVDAG (3) > ZERO                                     
112500              MOVE 'WE'     TO MOD-AVS-DAG (3)                            
112600           END-IF                                                         
112700           IF LEV-TILEVDAG (4) > ZERO                                     
112800              MOVE 'TH'     TO MOD-AVS-DAG (4)                            
112900           END-IF                                                         
113000           IF LEV-TILEVDAG (5) > ZERO                                     
113100              MOVE 'FR'     TO MOD-AVS-DAG (5)                            
113200           END-IF                                                         
113300        END-IF                                                            
113400     END-IF                                                               
113500                                                                          
113600     MOVE CLAG-IDANSK       TO W-IDANSK-L                                 
113700     MOVE CLAG-TIOMSPEC     TO W-TIOMSPEC                                 
113800     MOVE CLAG-IDANSK       TO MOD-IDANSK                                 
113900     MOVE CLAG-KVVECKOR-LT  TO MOD-KVVECKOR-LT                            
114000     MOVE CLAG-KVVECKOR-FT  TO MOD-KVVECKOR-FT                            
114100     MOVE CLAG-KVVECKOR-BT  TO MOD-KVVECKOR-BT                            
114200     MOVE CLAG-KDLPSP       TO MOD-KDLPSP                                 
114300                               W-MATINFO-KDLPSP                           
114400     MOVE CLAG-TILPSP       TO W-TILPSP                                   
114500     IF W-TILPSP > ZERO                                                   
114600        MOVE W-TILPSP       TO MOD-TILPSP                                 
114700     ELSE                                                                 
114800        MOVE SPACE          TO MOD-TILPSP                                 
114900     END-IF                                                               
115000                                                                          
115100     IF CLAG-IDDC-REF NOT = SPACE                                         
115200        MOVE INF-REFILL-PART    TO MED-IDMFSFEL                           
115300        CALL WMEDKONV USING MED-WMEDAREA                                  
115400        MOVE MED-MFSFEL         TO MOD-MESSAGE                            
115500     END-IF                                                               
115600                                                                          
115700     .                                                                    
115800     EJECT                                                                
115900                                                                          
116000 FB-UPPDAT-BILD-FRAN-LEVREG SECTION.                                      
116100     MOVE 'FB-UPPDAT-BILD-FRAN-LEVREG' TO CURRENT-SECTION                 
116200                                                                          
116300     MOVE W-SAVE-IDLEVNR     TO W-IDLEVNR                                 
116400     MOVE NEJ  TO SW-LEVSEGM-FINNS                                        
116500     MOVE ZERO TO W-ANTAL-LEVNR                                           
116600     MOVE IDARTNR-WS  TO W-IDARTNR-D9                                     
116700     MOVE WC-CDC-SE   TO W-IDDC-D9                                        
116800     PERFORM IMS-GU-D901                                                  
116900     IF SEGMENT-FINNS                                                     
117000        PERFORM IMS-GNP-D902                                              
117100        PERFORM UNTIL                                                     
117200         NOT ( SEGMENT-FINNS )                                            
117300          ADD 1 TO W-ANTAL-LEVNR                                          
117400                                                                          
117500          IF LEVNR-IDLEVNR = W-IDLEVNR                                    
117600            MOVE JA TO SW-LEVSEGM-FINNS                                   
117700            MOVE LEVNR-KVBR TO W-KVBR                                     
117800                               W-LEVNR-KVBR                               
117900                               MOD-KVBR                                   
118000            MOVE LEVNR-TILEVPL TO W-TILEVPL                               
118100            PERFORM FBA-LAES-AVROP-TILL-MOD                               
118200          END-IF                                                          
118300          PERFORM IMS-GNP-D902                                            
118400        END-PERFORM                                                       
118500     END-IF                                                               
118600     .                                                                    
118700     EJECT                                                                
118800 FBA-LAES-AVROP-TILL-MOD SECTION.                                         
118900     MOVE 'FBA-LAES-AVROP-TILL-MOD ' TO CURRENT-SECTION                   
119000                                                                          
119100*    AVROP LÄSES. AVROPEN MELLANLAGRAS I WS.                     *        
119200                                                                          
119300     PERFORM FBAA-INITIERA-TABELL                                         
119400     PERFORM IMS-GNP-D905                                                 
119500     PERFORM UNTIL                                                        
119600      NOT ( SEGMENT-FINNS )                                               
119700       MOVE AVROP-DAAVROP-AVS    TO W-DAAVROP-AVS                         
119800       MOVE W-TIAVROP-AVS        TO TMP1-YYWW                             
119900       MOVE PER-START-AAVV (1)   TO TMP2-YYWW                             
120000       MOVE PER-SLUT-AAVV (2)    TO TMP3-YYWW                             
120100       PERFORM WY2000Q3                                                   
120200       IF  TMP1-YYWW >= TMP2-YYWW                                         
120300       AND TMP1-YYWW <= TMP3-YYWW                                         
120400       AND AVROP-KDAVROP  =  W-KDAVROP                                    
120500                                                                          
120600          PERFORM S01-NYA-AVROP-TILL-TAB                                  
120700       END-IF                                                             
120800       PERFORM IMS-GNP-D905                                               
120900     END-PERFORM                                                          
121000     PERFORM S06-NYA-AVROP-I-TAB-TILL-MOD                                 
121100     CONTINUE.                                                            
121200     EJECT                                                                
121300 FBAA-INITIERA-TABELL SECTION.                                            
121400     MOVE 'FBAA-INITIERA-TABELL '  TO CURRENT-SECTION                     
121500                                                                          
121600     MOVE PER-AAPP (1)       TO MOD-PERIOD-AARP (1)                       
121700     MOVE PER-AAPP (2)       TO MOD-PERIOD-AARP (2)                       
121800     MOVE 1 TO IY                                                         
121900                                                                          
122000     PERFORM UNTIL                                                        
122100      ( IY > MAX-ANT-PERIODER-I-TAB )                                     
122200       MOVE 1                TO IX                                        
122300       MOVE PER-START-VV (IY)                                             
122400                             TO W-VECKA                                   
122500                                                                          
122600       PERFORM UNTIL W-VECKA > PER-SLUT-VV (IY)                           
122700         MOVE W-VECKA        TO MOD-TIAVROP-AVS-TAB (IY, IX)              
122800         MOVE '-'            TO MOD-SKILJETECKEN-TAB (IY, IX)             
122900         ADD 1               TO W-VECKA                                   
123000                                IX                                        
123100       END-PERFORM                                                        
123200       ADD 1 TO IY                                                        
123300     END-PERFORM                                                          
123400                                                                          
123500     PERFORM S07-NOLLSTAELL-AVROPSTABELLER                                
123600     .                                                                    
123700     EJECT                                                                
123800 G-KOLLA-INPUT SECTION.                                                   
123900     MOVE 'G-KOLLA-INPUT '  TO CURRENT-SECTION                            
124000                                                                          
124100     MOVE JA  TO SW-INDATA                                                
124200     MOVE IDARTNR-WS TO W-IDARTNR                                         
124300     MOVE IDLEVNR-WS TO W-IDLEVNR                                         
124400     MOVE KDBEHX-PLAN-WS TO W-KDBEHX-PLAN                                 
124500                                                                          
124600     PERFORM GA-KOLLA-REFILL-ARTIKEL                                      
124700                                                                          
124800     IF INDATA-OK                                                         
124900       PERFORM GC-KOLLA-ARTIKEL-WDK6                                      
125000     END-IF                                                               
125100                                                                          
125200     IF INDATA-OK                                                         
125300       MOVE IDARTNR-WS  TO W-IDARTNR-D9                                   
125400       MOVE WC-CDC-SE   TO W-IDDC-D9                                      
125500       PERFORM IMS-GU-D901                                                
125600       IF SEGMENT-SAKNAS                                                  
125700          MOVE NEJ             TO SW-INDATA                               
125900          MOVE FEL-12 TO MOD-MESSAGE                                      
126300          PERFORM MFS-ROER-EJ-FAELT-IN                                    
126400          PERFORM MFS-ROER-EJ-FAELT-UT                                    
126500       ELSE                                                               
126600          PERFORM IMS-GNP-D902-KVAL                                       
126700          IF SEGMENT-FINNS                                                
126800            IF LEVNR-IDLEVNR = '1002 '                                    
126900               MOVE NEJ             TO SW-INDATA                          
127100               MOVE FEL-21 TO MOD-MESSAGE                                 
127500               PERFORM MFS-ROER-EJ-FAELT-IN                               
127600               PERFORM MFS-ROER-EJ-FAELT-UT                               
127700            END-IF                                                        
127800          ELSE                                                            
127900            MOVE NEJ               TO SW-INDATA                           
128000            MOVE ERR-LEVNR-SAKNAS  TO MED-IDMFSFEL                        
128100            CALL WMEDKONV USING MED-WMEDAREA                              
128200            MOVE MED-MFSFEL      TO MOD-MESSAGE                           
128300            PERFORM MFS-ROER-EJ-FAELT-IN                                  
128400            PERFORM MFS-ROER-EJ-FAELT-UT                                  
128500          END-IF                                                          
128600       END-IF                                                             
128700                                                                          
128800       IF INDATA-OK                                                       
128900         IF MID-INPUT = ALL '+'                                           
129000           MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                      
129100           CALL WMEDKONV USING MED-WMEDAREA                               
129200           MOVE MED-MFSFEL TO MOD-MESSAGE                                 
129300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
129400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
129500           MOVE NEJ TO SW-INDATA                                          
129600         ELSE                                                             
129700           PERFORM GB-KOLLA-DATAELEMENT                                   
129800           IF INDATA-FEL                                                  
129900             PERFORM MFS-ROER-EJ-FAELT-IN                                 
130000             PERFORM MFS-ROER-EJ-FAELT-UT                                 
130100           END-IF                                                         
130200         END-IF                                                           
130300       END-IF                                                             
130400     END-IF                                                               
130500     .                                                                    
130600     EJECT                                                                
130700 GA-KOLLA-REFILL-ARTIKEL  SECTION.                                        
130800     MOVE 'GA-KOLLA-REFILL-ARTIKEL  '  TO CURRENT-SECTION                 
130900*---------------------------------------------------------                
131000*--- DET SKALL INTE SKAPAS AVROP/LEVPLANER PÅ REFILL DC                   
131100*---------------------------------------------------------                
131200                                                                          
131300     MOVE IDLEVNR-WS   TO W-IDLEVNR-DC                                    
131400     PERFORM IMS-GU-WDB601-LEV                                            
131500     IF SEGMENT-FINNS                                                     
131600       IF DCS-NDC-CN                                                      
131700         MOVE NEJ             TO SW-INDATA                                
131800         MOVE FEL-LEVNR       TO MED-IDMFSFEL                             
131900         CALL WMEDKONV USING MED-WMEDAREA                                 
132000         MOVE MED-MFSFEL      TO MOD-MESSAGE                              
132100         PERFORM MFS-ROER-EJ-FAELT-IN                                     
132200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
132300       END-IF                                                             
132400     END-IF                                                               
132500     .                                                                    
132600     EJECT                                                                
132700 GB-KOLLA-DATAELEMENT SECTION.                                            
132800     MOVE 'GB-KOLLA-DATAELEMENT '  TO CURRENT-SECTION                     
132900                                                                          
133000     MOVE 1 TO IX1                                                        
133100     PERFORM UNTIL IX1 > 2                                                
133200       MOVE 1         TO IX2                                              
133300       PERFORM UNTIL IX2 > 5                                              
133400         MOVE 1      TO IX3                                               
133500         PERFORM UNTIL IX3 > 5                                            
133600           IF MID-KVAVROP-DAG (IX1, IX2, IX3) = ALL '+'                   
133700             MOVE MFS-RENSA-FAELT                                         
133800                     TO MOD-KVAVROP-DAG-IN (IX1, IX2, IX3)                
133900           ELSE                                                           
134000             IF MID-KVAVROP-DAG (IX1, IX2, IX3) NUMERIC                   
134100                MOVE MID-KVAVROP-DAG (IX1, IX2, IX3)                      
134200                     TO MOD-KVAVROP-DAG-IN (IX1, IX2, IX3)                
134300                PERFORM GBB-KONTROLL-LEVINFO                              
134400             ELSE                                                         
134500               MOVE NEJ TO SW-INDATA                                      
134600               MOVE MFS-NUM-FAELT-FEL                                     
134700                    TO MOD-KVAVROP-DAG-IN-ATTR (IX1, IX2, IX3)            
134800               MOVE FEL-41 TO MOD-MESSAGE                                 
134900             END-IF                                                       
135000           END-IF                                                         
135100           ADD 1    TO IX3                                                
135200         END-PERFORM                                                      
135300         ADD 1       TO IX2                                               
135400       END-PERFORM                                                        
135500       ADD 1          TO IX1                                              
135600     END-PERFORM                                                          
135700     .                                                                    
135800     EJECT                                                                
135900 GBB-KONTROLL-LEVINFO    SECTION.                                         
136000     MOVE 'GBB-KONTROLL-LEVINFO         ' TO CURRENT-SECTION              
136100                                                                          
136200     MOVE SPACE           TO DLI-IO-2216                                  
136300     MOVE '2215'          TO W-WDGXKEY                                    
136400     MOVE W-IDLEVNR       TO X-IDLEVNR                                    
136500     PERFORM IMS-GU-2216-SEG                                              
136600     IF SEGMENT-SAKNAS                                                    
136700        MOVE NEJ          TO SW-INDATA                                    
136800        MOVE MFS-NUM-FAELT-FEL                                            
136900                    TO MOD-KVAVROP-DAG-IN-ATTR (IX1, IX2, IX3)            
137100        MOVE FEL-120      TO MOD-MESSAGE                                  
137600     END-IF                                                               
137700     .                                                                    
137800     EJECT                                                                
137900 GC-KOLLA-ARTIKEL-WDK6  SECTION.                                          
138000     MOVE 'GC-KOLLA-ARTIKEL-WDK6  '  TO CURRENT-SECTION                   
138100                                                                          
138200     PERFORM IMS-GU-K601                                                  
138300     IF  SEGMENT-FINNS                                                    
138400       IF  ART-KDERS-UTG = ZERO                                           
138500         PERFORM IMS-GNP-K611                                             
138600         IF SEGMENT-SAKNAS                                                
138700            MOVE NEJ         TO SW-INDATA                                 
138900            MOVE FEL-12 TO MOD-MESSAGE                                    
139300         ELSE                                                             
139400           MOVE CLAG-IDANSK TO W-IDANSK                                   
139500           MOVE CLAG-KDHF   TO W-KDHF                                     
139600         END-IF                                                           
139700       ELSE                                                               
139800         MOVE NEJ            TO SW-INDATA                                 
139900         IF  ART-KDERS-UTG < 20                                           
140100           MOVE FEL-16 TO MOD-MESSAGE                                     
140500         ELSE                                                             
140700           MOVE FEL-17 TO MOD-MESSAGE                                     
141100         END-IF                                                           
141200       END-IF                                                             
141300     ELSE                                                                 
141400       MOVE NEJ              TO SW-INDATA                                 
141600       MOVE FEL-12 TO MOD-MESSAGE                                         
142000     END-IF                                                               
142100                                                                          
142200     IF INDATA-FEL                                                        
142300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
142400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
142500     END-IF                                                               
142600     .                                                                    
142700     EJECT                                                                
142800 H-UPPDATERA SECTION.                                                     
142900     MOVE 'H-UPPDATERA '  TO CURRENT-SECTION                              
143000                                                                          
143100     MOVE NEJ                 TO SW-HUVUDLEVERANTOER                      
143200     MOVE W-IDLEVNR           TO W-SAVE-IDLEVNR                           
143300     PERFORM IMS-GU-K601                                                  
143400     IF W-IDLEVNR = ART-IDLEVNR                                           
143500       MOVE JA                TO SW-HUVUDLEVERANTOER                      
143600       PERFORM IMS-GNP-K611                                               
143700       MOVE CLAG-IDLEVNR-SHIP TO W-IDLEVNR                                
143800                                 W-IDLEVNR-SHIP-2258                      
143900       MOVE CLAG-IDANSK       TO W-IDANSK-2260                            
144000     END-IF                                                               
144100                                                                          
144200     PERFORM IMS-GU-F101                                                  
144300     MOVE W-SAVE-IDLEVNR      TO W-IDLEVNR                                
144400*                          FÖR ATT KUNNA SE AVSÄNDNINGSDAGARNA            
144500     MOVE IDARTNR-WS  TO W-IDARTNR-D9                                     
144600     MOVE WC-CDC-SE   TO W-IDDC-D9                                        
144700     PERFORM IMS-GU-D901                                                  
144800     MOVE 1 TO IX1                                                        
144900     PERFORM UNTIL IX1 > 2                                                
145000        MOVE 1               TO IX2                                       
145100        MOVE PER-TISEKEL (IX1)                                            
145200                             TO W-DAAVROP-SS                              
145300        MOVE PER-START-AAVV (IX1)                                         
145400                             TO W-DAAVROP-AAVV                            
145500        MOVE PER-START-VV (IX1)                                           
145600                             TO W-VECKA                                   
145700        PERFORM UNTIL W-VECKA > PER-SLUT-VV (IX1)                         
145800           MOVE 1            TO IX3                                       
145900           PERFORM UNTIL IX3 > 5                                          
146000              IF MID-KVAVROP-DAG (IX1, IX2, IX3) = ALL '+'                
146100                CONTINUE                                                  
146200              ELSE                                                        
146300                MOVE IX3     TO W-TILEVDAG                                
146400                PERFORM IMS-GHNP-D905                                     
146500                PERFORM UNTIL SEGMENT-SAKNAS                              
146600                OR AVROP-KDAVROP = W-KDAVROP                              
146700                   PERFORM IMS-GHNP-D905                                  
146800                END-PERFORM                                               
146900                IF SEGMENT-FINNS                                          
147000                   IF MID-KVAVROP-DAG (IX1, IX2, IX3) > ZERO              
147100                      MOVE MID-KVAVROP-DAG (IX1, IX2, IX3)                
147200                             TO AVROP-KVAVROP                             
147300                      PERFORM IMS-REPL-D905                               
147310                      PERFORM HB-KOLL-LEV-HELGDAG                         
147400                   ELSE                                                   
147500                      PERFORM IMS-DLET-D905                               
147600                   END-IF                                                 
147700                ELSE                                                      
147800                   IF MID-KVAVROP-DAG (IX1, IX2, IX3) > ZERO              
147900                                                                          
148000                      PERFORM HA-ISRT-AVROP                               
148100                                                                          
148200                   END-IF                                                 
148300                END-IF                                                    
148400              END-IF                                                      
148500              ADD 1          TO IX3                                       
148600           END-PERFORM                                                    
148700           ADD 1             TO IX2                                       
148800                                W-VECKA                                   
148900                                W-DAAVROP-AAVV                            
149000        END-PERFORM                                                       
149100        ADD 1                TO IX1                                       
149200     END-PERFORM                                                          
149300                                                                          
149400     PERFORM S08-GEN-UTSKRIFTSBEGAERAN                                    
149500     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
149600     CALL WMEDKONV USING MED-WMEDAREA                                     
149700     IF W-MESSAGE-BOTTOM = SPACE                                          
149800       MOVE MED-MFSINF TO W-MESSAGE-BOTTOM                                
149900     ELSE                                                                 
150000       MOVE MED-MFSINF TO W-MESSAGE-BOTTOM-3                              
150100     END-IF                                                               
150200     PERFORM MFS-FORM-ATTR                                                
150300     PERFORM MFS-RENSA-FAELT-IN                                           
150400     .                                                                    
150500     EJECT                                                                
150600 HA-ISRT-AVROP SECTION.                                                   
150700     MOVE 'HA-ISRT-AVROP '  TO CURRENT-SECTION                            
150800                                                                          
150900             MOVE W-KDAVROP                                               
151000                    TO AVROP-KDAVROP                                      
151100             MOVE W-DAAVROP                                               
151200                    TO AVROP-DAAVROP-AVS                                  
151300             MOVE W-TILEVDAG                                              
151400                    TO AVROP-TILEVDAG                                     
151500                                                                          
151600             PERFORM HAA-KOLL-LEV-TILEVDAG                                
151700*                    ÄR DET EN GILTIG TILEVDAG ?                          
151800                                                                          
151900*AVS-AAMMDD                                                               
152000             MOVE 'AAVVD'             TO DAT-KDDATFORM                    
152100             COMPUTE DAT-I-TIDATUM =  10 * W-DAAVROP-AAVV +               
152200                                           W-TILEVDAG                     
152300             CALL WDATKONV USING      DAT-KDDATFORM                       
152400                                      DAT-I-TIDATUM                       
152500                                      DAT-O-TIDATUM                       
152600                                      DAT-KDSVAR                          
152700             MOVE DAT-TIAAMMDD        TO W-TIAAMMDD-AVS                   
152800                                         W-DADATUM-HELG-AAMMDD            
152900*   AVROP-TIAVRDAT-INL                                                    
153000             MOVE 2                   TO WORK-KDCALL                      
153100             MOVE '11'                TO WORK-IDDC                        
153200             MOVE W-TIAAMMDD-AVS      TO WORK-TIAAMMDD-FOM                
153300             MOVE CLAG-KVDAGAR-TT     TO WORK-KVWORKD                     
153400             ADD +1                   TO WORK-KVWORKD                     
153500             CALL WORKDAY USING  WORK-KDCALL                              
153600                  WORK-DATE-AREA WORK-KDSVAR                              
153701             IF WORK-KDSVAR-FEL                                           
153710                MOVE 'FELAKTIGT DATUM - WORKDAY' TO FELTEXT               
153720                CALL FELLOG                                               
153730             END-IF                                                       
153740             MOVE WORK-TIAAMMDD-TOM   TO AVROP-TIAVRDAT-INL               
153800*   AVROP-TIAVRDAT-DISP                                                   
153900             MOVE 2                   TO WORK-KDCALL                      
154000             MOVE '11'                TO WORK-IDDC                        
154100             MOVE AVROP-TIAVRDAT-INL  TO WORK-TIAAMMDD-FOM                
154200             MOVE CLAG-KVDAGAR-INLEV  TO WORK-KVWORKD                     
154300             ADD +1                   TO WORK-KVWORKD                     
154400             CALL WORKDAY USING  WORK-KDCALL                              
154500                  WORK-DATE-AREA WORK-KDSVAR                              
154510             IF WORK-KDSVAR-FEL                                           
154520                MOVE 'FELAKTIGT DATUM - WORKDAY' TO FELTEXT               
154530                CALL FELLOG                                               
154540             END-IF                                                       
154600             MOVE WORK-TIAAMMDD-TOM   TO AVROP-TIAVRDAT-DISP              
154700                                                                          
154800             MOVE MID-KVAVROP-DAG (IX1, IX2, IX3)                         
154900                    TO AVROP-KVAVROP                                      
155000                                                                          
155100             MOVE IDARTNR-WS TO W-IDARTNR-D9                              
155200             MOVE WC-CDC-SE  TO W-IDDC-D9                                 
155300             PERFORM IMS-ISRT-D905                                        
155400                                                                          
155500             PERFORM HAB-KOLL-LEV-HELGDAG                                 
155600*                    VAR DET EN HELGDAG ?                                 
155700                                                                          
155800             PERFORM HAC-KOLL-LEV-BLOCKAD                                 
155900*                    VAR DET EN SPÄRRAD AVROPSVECKA FÖR LEV.?             
156000     .                                                                    
156100     EJECT                                                                
156200 HAA-KOLL-LEV-TILEVDAG SECTION.                                           
156300     MOVE 'HAA-KOLL-LEV-TILEVDAG '  TO CURRENT-SECTION                    
156400                                                                          
156500*PAH   (IX1 PERIOD,  IX2 VECKA,   IX3 VECKODAG)                           
156600                                                                          
156700     IF LEV-TILEVDAG (IX3) > ZERO                                         
156800        CONTINUE                                                          
156900     ELSE                                                                 
157000*       VARNING: EJ SÄNDNINGSDAG + HI                                     
157200        MOVE MED-12 TO MOD-MESSAGE                                        
157600        MOVE MFS-ADD-LYS-UPP-FAELT                                        
157700             TO MOD-KVAVROP-DAG-IN-ATTR (IX1, IX2, IX3)                   
157800     END-IF                                                               
157900     .                                                                    
158000     EJECT                                                                
158100 HAB-KOLL-LEV-HELGDAG SECTION.                                            
158200     MOVE 'HAB-KOLL-LEV-HELGDAG ' TO CURRENT-SECTION                      
158300                                                                          
158400*      (IX1 PERIOD,  IX2 VECKA,   IX3 VECKODAG)                           
158500                                                                          
158600     MOVE SPACE              TO WS-IDLANDX2                               
158700     PERFORM IMS-GET-LEVA14-WDF106                                        
158800     IF SEGMENT-FINNS                                                     
158900        MOVE ADR-IDLANDX2    TO WS-IDLANDX2                               
159000     END-IF                                                               
159100     MOVE WS-IDLANDX2       TO W-IDLANDX2                                 
159200     MOVE 20                TO W-DADATUM-HELG-SS                          
159300*****MOVE W-TIAAMMDD-AVS    TO W-DADATUM-HELG-AAMMDD  SE OVAN             
159400     PERFORM IMS-GET-WDF301                                               
159500     IF SEGMENT-FINNS                                                     
159700        MOVE MED-13 TO MOD-MESSAGE                                        
160100        MOVE MFS-ADD-LYS-UPP-FAELT                                        
160200             TO MOD-KVAVROP-DAG-IN-ATTR (IX1, IX2, IX3)                   
160300     END-IF                                                               
160400     .                                                                    
160500     EJECT                                                                
160600 HAC-KOLL-LEV-BLOCKAD SECTION.                                            
160700     MOVE 'HAC-KOLL-LEV-BLOCKAD ' TO CURRENT-SECTION                      
160800                                                                          
160900     IF SW-HUVUDLEVERANTOER = JA                                          
161000       PERFORM IMS-GU-WDGX2258                                            
161100       IF SEGMENT-FINNS                                                   
161200         MOVE AVROP-DAAVROP-AVS   TO W-DAAVROP-2260                       
161300                                                                          
161400         PERFORM IMS-GNP-WDGX2260                                         
161500         IF SEGMENT-FINNS                                                 
161600           MOVE INF-SUPPL-BLOCKED TO MED-IDMFSINF                         
161700           CALL WMEDKONV USING MED-WMEDAREA                               
161800           IF MOD-MESSAGE = SPACE                                         
161900             MOVE MED-MFSINF TO MOD-MESSAGE                               
162000           ELSE                                                           
162100*            IF MSGI-IDLAND-SPR = 'SE'                                    
162200*              MOVE MED-TEMFSINF TO W-MESSAGE-BOTTOM                      
162300*            ELSE                                                         
162400               MOVE MED-MFSINF   TO W-MESSAGE-BOTTOM                      
162500*            END-IF                                                       
162600           END-IF                                                         
162700           MOVE MFS-ADD-LYS-UPP-FAELT                                     
162800                TO MOD-KVAVROP-DAG-IN-ATTR (IX1, IX2, IX3)                
162900         END-IF                                                           
163000       END-IF                                                             
163100     END-IF                                                               
163200                                                                          
163300     .                                                                    
163400     EJECT                                                                
163410 HB-KOLL-LEV-HELGDAG SECTION.                                             
163420     MOVE 'HB-KOLL-LEV-HELGDAG ' TO CURRENT-SECTION                       
163430                                                                          
163495     MOVE SPACE               TO WS-IDLANDX2                              
163496     PERFORM IMS-GET-LEVA14-WDF106                                        
163497     IF SEGMENT-FINNS                                                     
163498        MOVE ADR-IDLANDX2     TO WS-IDLANDX2                              
163499     END-IF                                                               
163503     MOVE WS-IDLANDX2         TO W-IDLANDX2                               
163504     MOVE 20                  TO W-DADATUM-HELG-SS                        
163505                                                                          
163506     MOVE 'AAVVD'             TO DAT-KDDATFORM                            
163507     COMPUTE DAT-I-TIDATUM =  10 * AVROP-DAAVROP-AVS +                    
163508                                   AVROP-TILEVDAG                         
163510     CALL WDATKONV USING      DAT-KDDATFORM                               
163511                              DAT-I-TIDATUM                               
163512                              DAT-O-TIDATUM                               
163513                              DAT-KDSVAR                                  
163514     MOVE DAT-TIAAMMDD        TO W-TIAAMMDD-AVS                           
163515                                 W-DADATUM-HELG-AAMMDD                    
163516                                                                          
163520     PERFORM IMS-GET-WDF301                                               
163521     IF SEGMENT-FINNS                                                     
163524        MOVE MED-13 TO MOD-MESSAGE                                        
163528        MOVE MFS-ADD-LYS-UPP-FAELT                                        
163529             TO MOD-KVAVROP-DAG-IN-ATTR (IX1, IX2, IX3)                   
163530     END-IF                                                               
163531     .                                                                    
163532     EJECT                                                                
163540 S1-SECURITY-CHECK-ART-LEV SECTION.                                       
163600     MOVE 'S1-SECURITY-CHECK-ART-LEV' TO CURRENT-SECTION                  
163700     SKIP2                                                                
163800*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
163900     MOVE IDARTNR-WS TO W-IDARTNR                                         
164000     PERFORM IMS-GU-K601                                                  
164100     IF  SEGMENT-FINNS                                                    
164200       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
164300       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
164400       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
164500*        --- BEHÖRIG USER                                                 
164600         SET PASSED-SECURITY-CHECK TO TRUE                                
164700       ELSE                                                               
164800*        --- OBEHÖRIG USER / USER NOT AUTHORIZED                          
164900         MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                          
165000         CALL WMEDKONV USING MED-WMEDAREA                                 
165100         MOVE MED-TEMFSFEL TO MOD-MESSAGE                                 
165200       END-IF                                                             
165300     ELSE                                                                 
165500       MOVE FEL-12 TO MOD-MESSAGE                                         
165900     END-IF                                                               
166000     .                                                                    
166100     EJECT                                                                
166200 S01-NYA-AVROP-TILL-TAB SECTION.                                          
166300     MOVE 'S01-NYA-AVROP-TILL-TAB ' TO CURRENT-SECTION                    
166400                                                                          
166500     MOVE AVROP-DAAVROP-AVS  TO W-DAAVROP-AVS                             
166600     MOVE W-TIAVROP-AVS      TO TMP1-YYWW                                 
166700     MOVE PER-START-AAVV (1) TO TMP2-YYWW                                 
166800     MOVE PER-SLUT-AAVV (1)  TO TMP3-YYWW                                 
166900     PERFORM WY2000Q3                                                     
167000     IF  TMP1-YYWW >= TMP2-YYWW                                           
167100     AND TMP1-YYWW <= TMP3-YYWW                                           
167200*                                                                         
167300*  PERIOD 1                                                               
167400*                                                                         
167500       MOVE W-TIAVROP-AVS (3:2)                                           
167600                             TO W-VECKA                                   
167700       COMPUTE IX-VECKA = W-VECKA - PER-START-VV (1) + 1                  
167800       MOVE AVROP-KVAVROP                                                 
167900               TO MOD-KVAVROP-DAG-UT (1, IX-VECKA, AVROP-TILEVDAG)        
168000       ADD AVROP-KVAVROP     TO W-KVAVROP-TAB (1, IX-VECKA)               
168100     ELSE                                                                 
168200*                                                                         
168300*  PERIOD 2                                                               
168400*                                                                         
168500       MOVE W-TIAVROP-AVS (3:2)                                           
168600                             TO W-VECKA                                   
168700       COMPUTE IX-VECKA = W-VECKA - PER-START-VV (2) + 1                  
168800       MOVE AVROP-KVAVROP                                                 
168900               TO MOD-KVAVROP-DAG-UT (2, IX-VECKA, AVROP-TILEVDAG)        
169000       ADD AVROP-KVAVROP     TO W-KVAVROP-TAB (2, IX-VECKA)               
169100     END-IF                                                               
169200     .                                                                    
169300     EJECT                                                                
169400 S06-NYA-AVROP-I-TAB-TILL-MOD SECTION.                                    
169500     MOVE 'S06-NYA-AVROP-I-TAB-TILL-MOD'  TO CURRENT-SECTION              
169600                                                                          
169700     MOVE 1 TO IY                                                         
169800     PERFORM UNTIL                                                        
169900      ( IY > MAX-ANT-PERIODER-I-TAB )                                     
170000       MOVE 1 TO IX                                                       
170100       PERFORM UNTIL                                                      
170200        ( IX > 5)                                                         
170300                                                                          
170400           MOVE W-KVAVROP-TAB (IY, IX) TO W-KVAVROP-RED                   
170500                                                                          
170600           MOVE W-KVAVROP-RED  TO MOD-KVAVROP-TAB (IY, IX)                
170700                                                                          
170800         ADD 1 TO IX                                                      
170900       END-PERFORM                                                        
171000       ADD 1 TO IY                                                        
171100     END-PERFORM                                                          
171200     CONTINUE.                                                            
171300     EJECT                                                                
171400 S07-NOLLSTAELL-AVROPSTABELLER SECTION.                                   
171500     MOVE 'S07-NOLLSTAELL-AVROPSTABELLER'  TO CURRENT-SECTION             
171600                                                                          
171700     MOVE 1 TO IY                                                         
171800     PERFORM UNTIL                                                        
171900      ( IY > MAX-ANT-PERIODER-I-TAB )                                     
172000       MOVE 1 TO IX                                                       
172100       PERFORM UNTIL                                                      
172200        ( IX > 5)                                                         
172300         MOVE ZERO TO W-KVAVROP-TAB (IY, IX)                              
172400         ADD 1 TO IX                                                      
172500       END-PERFORM                                                        
172600       ADD 1 TO IY                                                        
172700     END-PERFORM                                                          
172800     CONTINUE.                                                            
172900     EJECT                                                                
173000*                            **                                 **        
173100*                            **                                 **        
173200*                            **   M F S -  SUBRUTINER           **        
173300*                            **                                 **        
173400*                            **                                 **        
173500     SKIP3                                                                
173600                                                                          
173700 S08-GEN-UTSKRIFTSBEGAERAN SECTION.                                       
173800     MOVE 'S08-GEN-UTSKRIFTSBEGAERAN '  TO CURRENT-SECTION                
173900                                                                          
174000     MOVE IDARTNR-WS TO W-IDARTNR-D9                                      
174100     MOVE WC-CDC-SE  TO W-IDDC-D9                                         
174200     PERFORM IMS-GHU-WDD902                                               
174300     IF SEGMENT-FINNS                                                     
174400        MOVE WS-DAGENS-DATUM TO LEVNR-TILEVPL                             
174500        PERFORM IMS-REPL-WDD902                                           
174600     END-IF                                                               
174700*                                                                         
174800     PERFORM IMS-GHU-WDD904                                               
174900     IF SEGMENT-FINNS                                                     
175000       PERFORM IMS-GU-WDK611                                              
175100       IF SEGMENT-FINNS                                                   
175200          IF CLAG-KDERS = +0                                              
175300             IF CLAG-KDLPSP NOT = 5                                       
175400             OR W-IDLEVNR NOT = ART-IDLEVNR                               
175500                PERFORM IMS-DLET-WDD904                                   
175600             END-IF                                                       
175700          END-IF                                                          
175800       END-IF                                                             
175900     END-IF                                                               
176000                                                                          
176100     IF W-KDHF = 0                                                        
176200       MOVE SPACE TO DLI-IO-2216                                          
176300       MOVE '2215' TO W-WDGXKEY                                           
176400       MOVE W-IDLEVNR TO X-IDLEVNR                                        
176500       PERFORM IMS-GU-2216-SEG                                            
176600       IF SEGMENT-FINNS AND 2216-KDEDI NOT = 'T'                          
176700         MOVE 2216-WDGX2216  TO W-WDGX2216                                
176800         MOVE SPACE TO DLI-IO-2218                                        
176900         MOVE '2217' TO W-WDGXKEY                                         
177000         MOVE W-IDARTNR TO 2218-IDARTNR                                   
177100         MOVE W-IDLEVNR TO 2218-IDLEVNR                                   
177200         MOVE W-IDANSK  TO 2218-IDANSK                                    
177300         IF (W-2216-KDVECKOSL NOT = 'P')  AND                             
177400            (2218-IDLEVNR     NOT = 'BP8HB')                              
177600            PERFORM IMS-INSERT-2218-SEG                                   
177700         END-IF                                                           
177800*  FIX                                                                    
177900         IF W-2216-TISEND-PER NOT NUMERIC                                 
178000            MOVE ZERO TO W-2216-TISEND-PER                                
178100         END-IF                                                           
178200*  FIX                                                                    
178300         MOVE W-2216-TISEND-PER   TO TMP1-YYMMDD                          
178400         MOVE W-2216-TISEND-SEN   TO TMP2-YYMMDD                          
178500         PERFORM WY2000P1                                                 
178600         IF W-2216-FLLEVPLP = JA OR                                       
178700            W-2216-FLLEVVB  = JA OR                                       
178800           (W-2216-TISEND-PER > ZERO AND                                  
178900            TMP1-YYMMDD > TMP2-YYMMDD)                                    
179000            PERFORM IMS-INSERT-2218-PERIOD-SEG                            
179100         END-IF                                                           
179200       END-IF                                                             
179300     END-IF                                                               
179400                                                                          
179500     MOVE W-IDLEVNR     TO OLIKA-LEV                                      
179600     IF LV-LEVNR                                                          
179700****** LEVERANTÖR LV                                                      
179800       MOVE LOW-VALUE TO XXCZ-2246-WDGX2246                               
179900       MOVE W-IDARTNR TO XXCZ-2246-IDARTNR                                
180000       MOVE W-IDLEVNR TO XXCZ-2246-IDLEVNR                                
180100       PERFORM IMS-INSERT-XXCZ-2246                                       
180200     END-IF                                                               
180300     .                                                                    
180400     EJECT                                                                
180500                                                                          
180600 MFS-RENSA-FAELT-UT SECTION.                                              
180700                                                                          
180800*    --- ALLA UTDATA-FÄLT                                                 
180900     MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-IN                             
181000                               MOD-KDBEHX-PLAN-IN                         
181100                               MOD-IDLEVNR-IN                             
181200                               MOD-IDARTNR                                
181300                               MOD-BEART-SVE                              
181400                               MOD-AVS-DAG (1)                            
181500                               MOD-AVS-DAG (2)                            
181600                               MOD-AVS-DAG (3)                            
181700                               MOD-AVS-DAG (4)                            
181800                               MOD-AVS-DAG (5)                            
181900                               MOD-IDANSK                                 
182000                               MOD-KVVECKOR-LT                            
182100                               MOD-KVVECKOR-FT                            
182200                               MOD-KVVECKOR-BT                            
182300                               MOD-KDLPSP                                 
182400                               MOD-TILPSP                                 
182500                               MOD-KVBR                                   
182600                                                                          
182700     MOVE 1 TO IX1                                                        
182800     PERFORM UNTIL IX1 > 2                                                
182900        MOVE MFS-RENSA-FAELT                                              
183000                            TO MOD-PERIOD-AARP (IX1)                      
183100        MOVE 1         TO IX2                                             
183200        PERFORM UNTIL IX2 > 5                                             
183300           MOVE MFS-RENSA-FAELT                                           
183400                       TO MOD-KVAVROP-TAB (IX1, IX2)                      
183500                          MOD-SKILJETECKEN-TAB (IX1, IX2)                 
183600                          MOD-TIAVROP-AVS-TAB (IX1, IX2)                  
183700           MOVE 1      TO IX3                                             
183800           PERFORM UNTIL IX3 > 5                                          
183900              MOVE MFS-RENSA-FAELT                                        
184000                       TO MOD-KVAVROP-DAG-UT (IX1, IX2, IX3)              
184100              ADD 1    TO IX3                                             
184200           END-PERFORM                                                    
184300           ADD 1       TO IX2                                             
184400        END-PERFORM                                                       
184500        ADD 1          TO IX1                                             
184600     END-PERFORM                                                          
184700     .                                                                    
184800     SKIP2                                                                
184900 MFS-RENSA-FAELT-IN SECTION.                                              
185000                                                                          
185100*    --- ALLA INDATA-FÄLT                                                 
185200                                                                          
185300     MOVE 1 TO IX1                                                        
185400     PERFORM UNTIL IX1 > 2                                                
185500        MOVE 1         TO IX2                                             
185600        PERFORM UNTIL IX2 > 5                                             
185700           MOVE 1      TO IX3                                             
185800           PERFORM UNTIL IX3 > 5                                          
185900              MOVE MFS-RENSA-FAELT                                        
186000                       TO MOD-KVAVROP-DAG-IN (IX1, IX2, IX3)              
186100              ADD 1    TO IX3                                             
186200           END-PERFORM                                                    
186300           ADD 1       TO IX2                                             
186400        END-PERFORM                                                       
186500        ADD 1          TO IX1                                             
186600     END-PERFORM                                                          
186700     .                                                                    
186800     EJECT                                                                
186900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
187000                                                                          
187100*    --- ALLA UTDATA-FÄLT                                                 
187200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                             
187300                               MOD-KDBEHX-PLAN-IN                         
187400                               MOD-IDLEVNR-IN                             
187500                               MOD-IDARTNR                                
187600                               MOD-BEART-SVE                              
187700                               MOD-AVS-DAG (1)                            
187800                               MOD-AVS-DAG (2)                            
187900                               MOD-AVS-DAG (3)                            
188000                               MOD-AVS-DAG (4)                            
188100                               MOD-AVS-DAG (5)                            
188200                               MOD-IDANSK                                 
188300                               MOD-KVVECKOR-LT                            
188400                               MOD-KVVECKOR-FT                            
188500                               MOD-KVVECKOR-BT                            
188600                               MOD-KDLPSP                                 
188700                               MOD-TILPSP                                 
188800                               MOD-KVBR                                   
188900                                                                          
189000     MOVE 1 TO IX1                                                        
189100     PERFORM UNTIL IX1 > 2                                                
189200        MOVE MFS-ROER-EJ-FAELT                                            
189300                            TO MOD-PERIOD-AARP (IX1)                      
189400        MOVE 1         TO IX2                                             
189500        PERFORM UNTIL IX2 > 5                                             
189600           MOVE MFS-ROER-EJ-FAELT                                         
189700                       TO MOD-KVAVROP-TAB (IX1, IX2)                      
189800                          MOD-SKILJETECKEN-TAB (IX1, IX2)                 
189900                          MOD-TIAVROP-AVS-TAB (IX1, IX2)                  
190000           MOVE 1      TO IX3                                             
190100           PERFORM UNTIL IX3 > 5                                          
190200              MOVE MFS-ROER-EJ-FAELT                                      
190300                       TO MOD-KVAVROP-DAG-UT (IX1, IX2, IX3)              
190400              ADD 1    TO IX3                                             
190500           END-PERFORM                                                    
190600           ADD 1       TO IX2                                             
190700        END-PERFORM                                                       
190800        ADD 1          TO IX1                                             
190900     END-PERFORM                                                          
191000     .                                                                    
191100     SKIP2                                                                
191200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
191300                                                                          
191400*    --- ALLA INDATA-FÄLT                                                 
191500                                                                          
191600     MOVE 1 TO IX1                                                        
191700     PERFORM UNTIL IX1 > 2                                                
191800        MOVE 1         TO IX2                                             
191900        PERFORM UNTIL IX2 > 5                                             
192000           MOVE 1      TO IX3                                             
192100           PERFORM UNTIL IX3 > 5                                          
192200              MOVE MFS-ROER-EJ-FAELT                                      
192300                       TO MOD-KVAVROP-DAG-IN (IX1, IX2, IX3)              
192400              ADD 1    TO IX3                                             
192500           END-PERFORM                                                    
192600           ADD 1       TO IX2                                             
192700        END-PERFORM                                                       
192800        ADD 1          TO IX1                                             
192900     END-PERFORM                                                          
193000     .                                                                    
193100     EJECT                                                                
193200 MFS-FORM-ATTR SECTION.                                                   
193300                                                                          
193400*    --- ALLA INDATA-FÄLT                                                 
193500                                                                          
193600     MOVE 1 TO IX1                                                        
193700     PERFORM UNTIL IX1 > 2                                                
193800        MOVE 1         TO IX2                                             
193900        PERFORM UNTIL IX2 > 5                                             
194000           MOVE 1      TO IX3                                             
194100           PERFORM UNTIL IX3 > 5                                          
194200              MOVE MFS-FORMATETS-ATTR                                     
194300                       TO MOD-KVAVROP-DAG-IN-ATTR (IX1, IX2, IX3)         
194400              ADD 1    TO IX3                                             
194500           END-PERFORM                                                    
194600           ADD 1       TO IX2                                             
194700        END-PERFORM                                                       
194800        ADD 1          TO IX1                                             
194900     END-PERFORM                                                          
195000     .                                                                    
195100     SKIP2                                                                
195200 MFS-LAES-IN-IGEN SECTION.                                                
195300                                                                          
195400*    --- ALLA INDATA-FÄLT                                                 
195500                                                                          
195600     MOVE 1 TO IX1                                                        
195700     PERFORM UNTIL IX1 > 2                                                
195800        MOVE 1         TO IX2                                             
195900        PERFORM UNTIL IX2 > 5                                             
196000           MOVE 1      TO IX3                                             
196100           PERFORM UNTIL IX3 > 5                                          
196200             MOVE MFS-ADD-LAES-IN-FAELT                                   
196300                       TO MOD-KVAVROP-DAG-IN (IX1, IX2, IX3)              
196400              ADD 1    TO IX3                                             
196500           END-PERFORM                                                    
196600           ADD 1       TO IX2                                             
196700        END-PERFORM                                                       
196800        ADD 1          TO IX1                                             
196900     END-PERFORM                                                          
197000     .                                                                    
197100     EJECT                                                                
197200*                            **                                 **        
197300*                            **   I M S -  SUBRUTINER           **        
197400*                            **                                 **        
197500     SKIP3                                                                
197600 IMS-GET-MSG SECTION.                                                     
197700                                                                          
197800     MOVE '  QC' TO GODK-STATUSKODER                                      
197900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
198000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
198100     PERFORM IMS-STATUSKONTROLL                                           
198200     CONTINUE.                                                            
198300     SKIP3                                                                
198400 IMS-INSERT-MSG SECTION.                                                  
198500                                                                          
198700     MOVE 'N' TO MFS-KDHUVOMR                                             
198900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
199000     MOVE SPACE TO GODK-STATUSKODER                                       
199100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
199200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
199300     PERFORM IMS-STATUSKONTROLL                                           
199400     CONTINUE.                                                            
199500     EJECT                                                                
199600 IMS-GU-K601 SECTION.                                                     
199700     MOVE 'IMS-GU-K601 '  TO DBS-SECTION                                  
199800                                                                          
199900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
200000            DELIMITED BY SIZE INTO SSA1                                   
200100     MOVE '  GE' TO GODK-STATUSKODER                                      
200200     CALL CBLTDLI USING GU     WDK6-PCB DLI-IO-WDK601 SSA1                
200300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
200400     PERFORM IMS-STATUSKONTROLL                                           
200500     CONTINUE.                                                            
200600     SKIP3                                                                
200700     EJECT                                                                
200800 IMS-GNP-K611 SECTION.                                                    
200900     MOVE 'IMS-GNP-K611 '  TO DBS-SECTION                                 
201000                                                                          
201100     MOVE 'WDK611   ' TO SSA1                                             
201200     MOVE '  GE' TO GODK-STATUSKODER                                      
201300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
201400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
201500     PERFORM IMS-STATUSKONTROLL                                           
201600     CONTINUE.                                                            
201700     EJECT                                                                
201800 IMS-GNP-K623 SECTION.                                                    
201900     MOVE 'IMS-GNP-K623 ' TO DBS-SECTION                                  
202000                                                                          
202100     MOVE 'WDK611   ' TO SSA1                                             
202200     MOVE 'WDK623   ' TO SSA2                                             
202300     MOVE '  GE' TO GODK-STATUSKODER                                      
202400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK623 SSA1 SSA2              
202500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
202600     PERFORM IMS-STATUSKONTROLL                                           
202700     .                                                                    
202800     SKIP3                                                                
202900 IMS-GU-D901 SECTION.                                                     
203000     MOVE 'IMS-GU-D901 '   TO DBS-SECTION                                 
203100                                                                          
203200     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
203300            DELIMITED BY SIZE INTO SSA1                                   
203400     MOVE '  GE' TO GODK-STATUSKODER                                      
203500     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
203600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
203700     PERFORM IMS-STATUSKONTROLL                                           
203800     CONTINUE.                                                            
203900     SKIP3                                                                
204000 IMS-GNP-D902 SECTION.                                                    
204100     MOVE 'IMS-GNP-D902 '   TO DBS-SECTION                                
204200                                                                          
204300     MOVE 'WDD902   ' TO SSA1                                             
204400     MOVE '  GE' TO GODK-STATUSKODER                                      
204500     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
204600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
204700     PERFORM IMS-STATUSKONTROLL                                           
204800     CONTINUE.                                                            
204900     SKIP3                                                                
205000 IMS-GNP-D902-KVAL SECTION.                                               
205100     MOVE 'IMS-GNP-D902-KVAL '  TO DBS-SECTION                            
205200                                                                          
205300     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
205400            DELIMITED BY SIZE INTO SSA1                                   
205500     MOVE '  GE' TO GODK-STATUSKODER                                      
205600     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
205700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
205800     PERFORM IMS-STATUSKONTROLL                                           
205900     CONTINUE.                                                            
206000     EJECT                                                                
206100 IMS-GNP-D905 SECTION.                                                    
206200     MOVE 'IMS-GNP-D905 '   TO DBS-SECTION                                
206300                                                                          
206400     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
206500            DELIMITED BY SIZE INTO SSA1                                   
206600     MOVE 'WDD905   ' TO SSA2                                             
206700     MOVE '  GE' TO GODK-STATUSKODER                                      
206800     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
206900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
207000     PERFORM IMS-STATUSKONTROLL                                           
207100     .                                                                    
207200     EJECT                                                                
207300 IMS-GHNP-D905 SECTION.                                                   
207400     MOVE 'IMS-GHNP-D905 '  TO DBS-SECTION                                
207500                                                                          
207600     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
207700            DELIMITED BY SIZE INTO SSA1                                   
207800     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X ')'                        
207900            DELIMITED BY SIZE INTO SSA2                                   
208000     MOVE '  GE' TO GODK-STATUSKODER                                      
208100     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2             
208200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
208300     PERFORM IMS-STATUSKONTROLL                                           
208400     CONTINUE.                                                            
208500     EJECT                                                                
208600 IMS-GU-D905-AAAAVV SECTION.                                              
208700     MOVE 'IMS-GU-D905-AAAAVV '  TO DBS-SECTION                           
208800                                                                          
208900     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
209000            DELIMITED BY SIZE INTO SSA1                                   
209100     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
209200            DELIMITED BY SIZE INTO SSA2                                   
209300     STRING 'WDD905  (DAAVROP  =' W-DAAVROP-X ')'                         
209400            DELIMITED BY SIZE INTO SSA3                                   
209500     MOVE '  GE' TO GODK-STATUSKODER                                      
209600     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3          
209700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
209800     PERFORM IMS-STATUSKONTROLL                                           
209900     .                                                                    
210000     EJECT                                                                
210100 IMS-ISRT-D905 SECTION.                                                   
210200     MOVE 'IMS-ISRT-D905 '   TO DBS-SECTION                               
210300                                                                          
210400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
210500            DELIMITED BY SIZE INTO SSA1                                   
210600     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
210700            DELIMITED BY SIZE INTO SSA2                                   
210800     MOVE 'WDD905   ' TO SSA3                                             
210900     MOVE '  ' TO GODK-STATUSKODER                                        
211000     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD905                       
211100                               SSA1 SSA2 SSA3                             
211200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
211300     PERFORM IMS-STATUSKONTROLL                                           
211400     .                                                                    
211500     EJECT                                                                
211600 IMS-REPL-D905 SECTION.                                                   
211700     MOVE 'IMS-REPL-D905 '  TO DBS-SECTION                                
211800                                                                          
211900     MOVE '  ' TO GODK-STATUSKODER                                        
212000     CALL CBLTDLI USING REPL    WDD9-PCB DLI-IO-WDD905                    
212100     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
212200     PERFORM IMS-STATUSKONTROLL                                           
212300     .                                                                    
212400     EJECT                                                                
212500 IMS-DLET-D905 SECTION.                                                   
212600     MOVE 'IMS-DLET-D905 '  TO DBS-SECTION                                
212700                                                                          
212800     MOVE '  ' TO GODK-STATUSKODER                                        
212900     CALL CBLTDLI USING DLET    WDD9-PCB DLI-IO-WDD905                    
213000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
213100     PERFORM IMS-STATUSKONTROLL                                           
213200     .                                                                    
213300     EJECT                                                                
213400                                                                          
213500 IMS-GHU-WDD902  SECTION.                                                 
213600     MOVE 'IMS-GU-WDD902    '  TO DBS-SECTION                             
213700                                                                          
213800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
213900         DELIMITED BY SIZE INTO SSA1                                      
214000     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
214100         DELIMITED BY SIZE INTO SSA2                                      
214200     MOVE '  GE' TO GODK-STATUSKODER                                      
214300     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2              
214400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
214500     PERFORM IMS-STATUSKONTROLL                                           
214600     SKIP3                                                                
214700     .                                                                    
214800                                                                          
214900 IMS-REPL-WDD902 SECTION.                                                 
215000     MOVE 'IMS-REPL-WDD902  '  TO DBS-SECTION                             
215100                                                                          
215200     MOVE '  '   TO GODK-STATUSKODER                                      
215300     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD902                       
215400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
215500     PERFORM IMS-STATUSKONTROLL                                           
215600     .                                                                    
215700                                                                          
215800 IMS-GHU-WDD904 SECTION.                                                  
215900     MOVE 'IMS-GHU-WDD904   '  TO DBS-SECTION                             
216000                                                                          
216100     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
216200         DELIMITED BY SIZE INTO SSA1                                      
216300     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
216400         DELIMITED BY SIZE INTO SSA2                                      
216500     MOVE 'WDD904  ' TO SSA3                                              
216600     MOVE '  GE' TO GODK-STATUSKODER                                      
216700     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3         
216800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
216900     PERFORM IMS-STATUSKONTROLL                                           
217000     .                                                                    
217100                                                                          
217200 IMS-DLET-WDD904 SECTION.                                                 
217300     MOVE 'IMS-DLET-WDD904  '  TO DBS-SECTION                             
217400                                                                          
217500     MOVE '  '   TO GODK-STATUSKODER                                      
217600     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD904                       
217700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
217800     PERFORM IMS-STATUSKONTROLL                                           
217900     .                                                                    
218000                                                                          
218100 IMS-GU-WDK611 SECTION.                                                   
218200     MOVE 'IMS-GU-WDK611    '  TO DBS-SECTION                             
218300                                                                          
218400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
218500         DELIMITED BY SIZE INTO SSA1                                      
218600     MOVE 'WDK611  ' TO SSA2                                              
218700     MOVE '    ' TO GODK-STATUSKODER                                      
218800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
218900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
219000     PERFORM IMS-STATUSKONTROLL                                           
219100     SKIP3                                                                
219200     .                                                                    
219300                                                                          
219400 IMS-GU-D311 SECTION.                                                     
219500     MOVE 'IMS-GU-D311 '  TO DBS-SECTION                                  
219600                                                                          
219700     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
219800            DELIMITED BY SIZE INTO SSA1                                   
219900     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
220000            DELIMITED BY SIZE INTO SSA2                                   
220100     MOVE '  ' TO GODK-STATUSKODER                                        
220200     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
220300     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
220400     PERFORM IMS-STATUSKONTROLL                                           
220500     CONTINUE.                                                            
220600     EJECT                                                                
220700 IMS-GU-F101 SECTION.                                                     
220800     MOVE 'IMS-GU-F101 '  TO DBS-SECTION                                  
220900                                                                          
221000     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
221100          DELIMITED BY SIZE INTO SSA1                                     
221200     MOVE '  GE' TO GODK-STATUSKODER                                      
221300     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
221400     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
221500     PERFORM IMS-STATUSKONTROLL                                           
221600     .                                                                    
221700     SKIP3                                                                
221800 IMS-GET-LEVA14-WDF106 SECTION.                                           
221900     MOVE 'IMS-GET-LEVA14-WDF106 '  TO DBS-SECTION                        
222000                                                                          
222100     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
222200          DELIMITED BY SIZE INTO SSA1                                     
222300     STRING 'WDF106     '                                                 
222400          DELIMITED BY SIZE INTO SSA2                                     
222500     MOVE '  GE' TO GODK-STATUSKODER                                      
222600     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F106 SSA1 SSA2            
222700     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
222800     PERFORM IMS-STATUSKONTROLL                                           
222900     .                                                                    
223000     EJECT                                                                
223100 IMS-GET-WDF301 SECTION.                                                  
223200     MOVE 'IMS-GET-WDF301 '  TO DBS-SECTION                               
223300                                                                          
223400     STRING 'WDF301  (WDF301KY =' W-WDF301KY-X ')'                        
223500          DELIMITED BY SIZE INTO SSA1                                     
223600     MOVE '  GE' TO GODK-STATUSKODER                                      
223700     CALL CBLTDLI USING GU WDF3-PCB DLI-IO-AREA-F301 SSA1                 
223800     MOVE WDF3-STATUS-CODE TO STATUS-WS                                   
223900     PERFORM IMS-STATUSKONTROLL                                           
224000     .                                                                    
224100     SKIP3                                                                
224200*IMS-GET-WDF311 SECTION.                                                  
224300*                                                                         
224400*    STRING 'WDF311  (IDLEVNR  =' W-IDLEVNR-X ')'                         
224500*         DELIMITED BY SIZE INTO SSA1                                     
224600*    MOVE '  GE' TO GODK-STATUSKODER                                      
224700*    CALL CBLTDLI USING GNP WDF3-PCB DLI-IO-AREA-F311 SSA1                
224800*    MOVE WDF3-STATUS-CODE TO STATUS-WS                                   
224900*    PERFORM IMS-STATUSKONTROLL                                           
225000*    .                                                                    
225100     EJECT                                                                
225200 IMS-GU-2216-SEG SECTION.                                                 
225300     MOVE 'IMS-GU-2216-SEG '  TO DBS-SECTION                              
225400                                                                          
225500     STRING 'WLXXBK01(WDGXKEY  =' W-WDGXKEY-ROT ')'                       
225600            DELIMITED BY SIZE INTO SSA1                                   
225700     STRING 'WLXXBK11(IDLEVNR  =' W-WDGXKEY-IDLEVNR ')'                   
225800            DELIMITED BY SIZE INTO SSA2                                   
225900     MOVE '  GE' TO GODK-STATUSKODER                                      
226000     CALL CBLTDLI USING GU XXBK-PCB DLI-IO-2216 SSA1 SSA2                 
226100     MOVE XXBK-STATUS-CODE TO STATUS-WS                                   
226200     PERFORM IMS-STATUSKONTROLL                                           
226300     .                                                                    
226400     SKIP3                                                                
226500 IMS-INSERT-2218-SEG SECTION.                                             
226600     MOVE 'IMS-INSERT-2218-SEG '  TO DBS-SECTION                          
226700                                                                          
226800     STRING 'WLXXBL01(WDGXKEY  =' W-WDGXKEY-ROT ')'                       
226900            DELIMITED BY SIZE INTO SSA1                                   
227000     MOVE 'WLXXBL11 ' TO SSA2                                             
227100     MOVE '  II' TO GODK-STATUSKODER                                      
227200     CALL CBLTDLI USING ISRT XXBL-PCB DLI-IO-2218 SSA1 SSA2               
227300     MOVE XXBL-STATUS-CODE TO STATUS-WS                                   
227400     PERFORM IMS-STATUSKONTROLL                                           
227500     .                                                                    
227600     EJECT                                                                
227700 IMS-INSERT-2218-PERIOD-SEG SECTION.                                      
227800     MOVE 'IMS-INSERT-2218-PERIOD-SEG'  TO DBS-SECTION                    
227900                                                                          
228000     STRING 'WLXXBL01(WDGXKEY  =' W-WDGXKEY-PERIOD-ROT ')'                
228100            DELIMITED BY SIZE INTO SSA1                                   
228200     MOVE 'WLXXBL11 ' TO SSA2                                             
228300     MOVE '  II' TO GODK-STATUSKODER                                      
228400     CALL CBLTDLI USING ISRT XXBL-PCB DLI-IO-2218 SSA1 SSA2               
228500     MOVE XXBL-STATUS-CODE TO STATUS-WS                                   
228600     PERFORM IMS-STATUSKONTROLL                                           
228700     .                                                                    
228800     SKIP3                                                                
228900 IMS-INSERT-XXCZ-2246 SECTION.                                            
229000     MOVE 'IMS-INSERT-XXCZ-2246 '   TO DBS-SECTION                        
229100                                                                          
229200     STRING 'WLXXCZ01(WDGXKEY  =' W-WDGXKEY-2245-X ')'                    
229300            DELIMITED BY SIZE INTO SSA1                                   
229400     MOVE   'WLXXCZ11 ' TO SSA2                                           
229500     MOVE '  II' TO GODK-STATUSKODER                                      
229600     CALL CBLTDLI USING ISRT XXCZ-PCB DLI-IO-2246 SSA1 SSA2               
229700     MOVE XXCZ-STATUS-CODE TO STATUS-WS                                   
229800     PERFORM IMS-STATUSKONTROLL                                           
229900     .                                                                    
230000     SKIP3                                                                
230100 IMS-GU-WDGX2258 SECTION.                                                 
230200     MOVE 'IMS-GU-WDGX2258 '  TO DBS-SECTION                              
230300                                                                          
230400     MOVE SPACES              TO SSA1 SSA2                                
230500     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2257-X ')'                    
230600          DELIMITED BY SIZE INTO SSA1                                     
230700     STRING 'WDGX2258(IDLEVNRS =' W-WDGXKEY-2258-X ')'                    
230800          DELIMITED BY SIZE INTO SSA2                                     
230900     MOVE '  GE'              TO GODK-STATUSKODER                         
231000     CALL CBLTDLI USING GU 2257-PCB DLI-IO-WDGX2258 SSA1 SSA2             
231100     MOVE 2257-STATUS-CODE    TO STATUS-WS                                
231200     PERFORM IMS-STATUSKONTROLL                                           
231300     .                                                                    
231400     SKIP3                                                                
231500 IMS-GNP-WDGX2260 SECTION.                                                
231600     MOVE 'IMS-GNP-WDGX2260 '  TO DBS-SECTION                             
231700                                                                          
231800     MOVE SPACE               TO SSA1                                     
231900     STRING 'WDGX2260(DAAVROPF<=' W-DAAVROP-2260-X                        
232000                    '&DAAVROPT>=' W-DAAVROP-2260-X                        
232100                    '&IDANSKF <=' W-IDANSK-2260-X                         
232200                    '&IDANSKT >=' W-IDANSK-2260-X ')'                     
232300          DELIMITED BY SIZE INTO SSA1                                     
232400     MOVE '  GE'              TO GODK-STATUSKODER                         
232500     CALL CBLTDLI USING GNP 2257-PCB DLI-IO-WDGX2260 SSA1                 
232600     MOVE 2257-STATUS-CODE    TO STATUS-WS                                
232700     PERFORM IMS-STATUSKONTROLL                                           
232800     .                                                                    
232900     EJECT                                                                
233000 IMS-GU-WDB601-LEV SECTION.                                               
233100                                                                          
233200     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNR-DC-X ')'                      
233300          DELIMITED BY SIZE INTO SSA1                                     
233400     MOVE '  GE'              TO GODK-STATUSKODER                         
233500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-B601 SSA1                      
233600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
233700     PERFORM IMS-STATUSKONTROLL                                           
233800     .                                                                    
233900 IMS-STATUSKONTROLL SECTION.                                              
234000                                                                          
234100     SET STATUS-IX TO 1                                                   
234200     SEARCH GODK-STATUS AT END                                            
234300     CALL FELLOG                                                          
234400     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
234500     CONTINUE                                                             
234600     END-SEARCH                                                           
234700     .                                                                    
234800     EJECT                                                                
234900*    -COPY WY2000P1                                                       
235000     EJECT                                                                
235100*    -COPY WY2000P3                                                       
235200     EJECT                                                                
235300*    -COPY WY2000P6                                                       
235400     EJECT                                                                
235500*    -COPY WY2000P9                                                       
235600     EJECT                                                                
235700*    -COPY WY2000Q3                                                       
235800     EJECT                                                                
235900*    -COPY WY2000Q7                                                       
