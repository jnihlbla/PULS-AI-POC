000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6019700.                                                
000300 AUTHOR.         GUNNAR LARSSON IDK.                                      
000400 DATE-WRITTEN.   92/05/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET ÄR EN BAKGRUNDS-MPP SOM                               
000900*        HANTERAR UTSKRIFT AV FÖRBEHANDLINGSRAPPORTER                     
001000*        THIS PROGRAM PRINTS PRE TREATMENT REPORT.                        
001100*                                                                         
001200*        PROGRAMMET LÄSER       W6INLA (W6D1)                             
001300*        PROGRAMMET LÄSER       WLARTC (WDK6)                             
001400*        PROGRAMMET LÄSER       WLARTD (WDD8)                             
001500*        PROGRAMMET LÄSER       WLBENA (WDD3)                             
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W6T197U                                             
001900*        MID:         W6I19701                                            
002000*                                                                         
002100*    UTDATA:                                                              
002200*        UTSKRIVNA FB-RAPPORTER (VIA W006PRC1)                            
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W6019700'.            
003200                                                                          
003300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800                                                                          
003900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004000                                                                          
004100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004200     88  EGEN-MID                            VALUE '6197'.                
004300     88  GODK-MID                            VALUE '6132'                 
004400                                                   '6136'.                
004500                                                                          
004600 77  LASER-SW                    PIC X(1)    VALUE 'N'.                   
004700     88  LASER                               VALUE 'J'.                   
004800     EJECT                                                                
004900 01      FILLER                  PIC X(8)    VALUE 'DATUM***'.            
005000 01      DAGENS-DATUM.                                                    
005100     03  DAGENS-DATUM-AAR        PIC X(2)    VALUE SPACE.                 
005200     03  DAGENS-DATUM-MAANAD     PIC X(2)    VALUE SPACE.                 
005300     03  DAGENS-DATUM-DAG        PIC X(2)    VALUE SPACE.                 
005400*                                                                         
005500     SKIP3                                                                
005600 01      FILLER                  PIC X(8)    VALUE 'WS******'.            
005700 01      WS.                                                              
005800                                                                          
005900*     -- PRINTER-ID                                                       
006000  02     WS-IDPRTLST             PIC X(8)    VALUE SPACE.                 
006100*     -- ANTAL GODKÄNDA BUFFERTADRESSER C1                                
006200  02     WS-ANT-GODK-ADBUFF      PIC S9(3)   VALUE ZERO COMP-3.           
006300*     -- REMAINDER I DIVIDE                                               
006400  02     WS-REMAINDER            PIC S9(5)   VALUE ZERO COMP-3.           
006500*     -- AVISERAT ANTAL FÖRUTOM SATS                                      
006600  02     WS-KVAVIS-EJ-KIT        PIC S9(7)   VALUE ZERO COMP-3.           
006700*     -- FÖR EMBALLAGE-RADER                                              
006800  02     WS-EMB.                                                          
006900   03    WS-EMB-IDARTNR-EMBQ     PIC S9(9)   VALUE ZERO COMP-3.           
007000   03    WS-EMB-KVQPACK          PIC S9(5)   VALUE ZERO COMP-3.           
007100   03    WS-EMB-QTYP             PIC 9(1)    VALUE ZERO.                  
007200*     -- FÖR KLOCKSLAG                                                    
007300  02     WS-TID                  PIC 9(8)    VALUE ZERO.                  
007400                                                                          
007500*     -- SPARAT FRÅN SEGMENT                                              
007600                                                                          
007700  02     WS-ARTC.                                                         
007800                                                                          
007900   03    WS-ARTC01.                                                       
008000    04   WS-ARTC01-IDFTG         PIC 9(2)    VALUE ZERO.                  
008100                                                                          
008200   03    WS-ARTC11.                                                       
008300    04   WS-ARTC11-KDARTURS      PIC  X(2)   VALUE SPACE.                 
008400    04   WS-ARTC11-KDFORP        PIC  9(5)   VALUE ZERO.                  
008500    04   WS-ARTC11-KVQPACK-0     PIC S9(5)   VALUE ZERO COMP-3.           
008600    04   WS-ARTC11-KVQPACK-1     PIC S9(5)   VALUE ZERO COMP-3.           
008700    04   WS-ARTC11-KVQPACK-2     PIC S9(5)   VALUE ZERO COMP-3.           
008800    04   WS-ARTC11-KVQPACK-3     PIC S9(5)   VALUE ZERO COMP-3.           
008900    04   WS-ARTC11-IDARTNR-EMBQ0 PIC S9(9)   VALUE ZERO COMP-3.           
009000    04   WS-ARTC11-IDARTNR-EMBQ1 PIC S9(9)   VALUE ZERO COMP-3.           
009100    04   WS-ARTC11-IDARTNR-EMBQ2 PIC S9(9)   VALUE ZERO COMP-3.           
009200    04   WS-ARTC11-ADLAGOMR      PIC S9(3)   VALUE ZERO COMP-3.           
009300    04   WS-ARTC11-ADGANG        PIC S9(3)   VALUE ZERO COMP-3.           
009400    04   WS-ARTC11-ADPLATS       PIC S9(5)   VALUE ZERO COMP-3.           
009500    04   WS-ARTC11-ADLAGOMR-SVS  PIC S9(3)   VALUE ZERO COMP-3.           
009600    04   WS-ARTC11-ADGANG-SVS    PIC S9(3)   VALUE ZERO COMP-3.           
009700    04   WS-ARTC11-ADPLATS-SVS   PIC S9(5)   VALUE ZERO COMP-3.           
009800    04   WS-ARTC11-FLFSP         PIC  X(1)   VALUE 'N'.                   
009900    04   WS-ARTC11-KVROS         PIC S9(6)   VALUE ZERO COMP-3.           
009910                                                                          
009920   03    WS-WDK712.                                                       
009990    04   WS-WDK712-IDARTNR-EMBQ0 PIC S9(9)   VALUE ZERO COMP-3.           
009991    04   WS-WDK712-IDARTNR-EMBQ1 PIC S9(9)   VALUE ZERO COMP-3.           
009992    04   WS-WDK712-IDARTNR-EMBQ2 PIC S9(9)   VALUE ZERO COMP-3.           
010000                                                                          
010100     EJECT                                                                
010200*     -- SPAR AREOR                                                       
010300 01    SPAR-ADLAGOMR             PIC 9(2) VALUE ZERO.                     
010400 01    SPAR-ADGANG               PIC 9(2) VALUE ZERO.                     
010500 01    SPAR-ADPLATS              PIC 9(5) VALUE ZERO.                     
010600 01    SPAR-IDARTNR-EMBQ         PIC 9(8) VALUE ZERO.                     
010700 01    SPAR-BEART                PIC X(25) VALUE ZERO.                    
010800 01    SPAR-QTYP                 PIC 9(1) VALUE ZERO.                     
010900 01    SPAR-KVQPACK              PIC 9(5) VALUE ZERO.                     
011000                                                                          
011100      EJECT                                                               
011200 01      FILLER                  PIC X(8)    VALUE 'SW-*****'.            
011300 01      SW-SWITCHAR.                                                     
011400                                                                          
011500  02     SW-ARTC11-C1-FINNS      PIC X(1)    VALUE SPACE.                 
011510  02     SW-WDK712-US-CN-FINNS   PIC X(1)    VALUE SPACE.                 
011600  02     SW-EMB-INFO-REDIGERAD   PIC X(1)    VALUE SPACE.                 
011700     SKIP3                                                                
011800 01      FILLER                  PIC X(8)    VALUE 'IX-*****'.            
011900 01      IX-INDEXVARIABLER.                                               
012000*     -- POST INOM MID                                                    
012100  02     IX-POST                 PIC S9(9)   VALUE ZERO COMP SYNC.        
012200*     -- TABEL RAD I TERMO TABELL                                         
012300  02     TERMO-IX                PIC S9(9)   VALUE ZERO COMP SYNC.        
012400  02     LASER-IX                PIC S9(9)   VALUE ZERO COMP SYNC.        
012500     SKIP3                                                                
012600 01      FILLER                  PIC X(8)    VALUE 'K-******'.            
012700 01      K-KONSTANTER.                                                    
012800                                                                          
012900*     -- MAX ANTAL POSTER I MID                                           
013000  02     K-MAX-KVPOST            PIC S9(3)   VALUE +15  COMP-3.           
013100                                                                          
013200*     -- MAX ANTAL REDIGERADE BUFFERTADRESSER C1 PÅ FÖRB.RAPP.            
013300  02     K-MAX-REDIG-ADBUFF      PIC S9(3)   VALUE +3   COMP-3.           
013400                                                                          
013500*     -- MAX ANTAL POSTER I TERMO TABELL                                  
013600  02     FR-TAB-MAX              PIC S9(3)   VALUE +90  COMP-3.           
013700  02     FR-TAB-MAX-NOVA         PIC S9(3)   VALUE +90  COMP-3.           
013800  02     FR-TAB-MAX-VCOM         PIC S9(3)   VALUE +42  COMP-3.           
013900  02     FR-LASER-MAX            PIC S9(3)   VALUE +49  COMP-3.           
014000  02     PR-LASER-MAX            PIC S9(3)   VALUE +46  COMP-3.           
014100     EJECT                                                                
014200*    --- FÖRBEHANDLINGSRAPPORT VIA TERMO SKRIVARE                         
014300 01      FILLER                  PIC X(8)    VALUE 'FR TERMO'.            
014400 01      FR-TAB-VCOM.                                                     
014500     03  FILLER                  PIC X(80)   VALUE                        
014600         '!M "FORB"                                             '.        
014700     03  FILLER.                                                          
014800         05 FR-TAB-IDLOPNRM-VCOM PIC 9(8).                                
014900         05 FILLER               PIC X(72)   VALUE SPACE.                 
015000     03  FILLER.                                                          
015100         05 FR-TAB-KVQPACK-3-VCOM PIC Z(6)9.                              
015200         05 FILLER               PIC X(73)   VALUE SPACE.                 
015300     03  FILLER.                                                          
015400         05 FR-TAB-ADLAGOMR-VCOM PIC 9(2).                                
015500         05 FILLER               PIC X(1)    VALUE SPACE.                 
015600         05 FR-TAB-ADGANG-VCOM   PIC Z(2).                                
015700         05 FILLER               PIC X(1)    VALUE SPACE.                 
015800         05 FR-TAB-ADPLATS-VCOM  PIC Z(5).                                
015900         05 FILLER               PIC X(69)   VALUE SPACE.                 
016000     03  FILLER.                                                          
016100         05 FR-TAB-ADLAGOMR-EMB0-VCOM PIC Z(2).                           
016200         05 FILLER               PIC X(1)    VALUE SPACE.                 
016300         05 FR-TAB-ADGANG-EMB0-VCOM   PIC Z(2).                           
016400         05 FILLER               PIC X(1)    VALUE SPACE.                 
016500         05 FR-TAB-ADPLATS-EMB0-VCOM  PIC Z(5).                           
016600         05 FILLER               PIC X(69)   VALUE SPACE.                 
016700     03  FILLER.                                                          
016800         05 FR-TAB-ADLAGOMR-EMB1-VCOM PIC Z(2).                           
016900         05 FILLER               PIC X(1)    VALUE SPACE.                 
017000         05 FR-TAB-ADGANG-EMB1-VCOM   PIC Z(2).                           
017100         05 FILLER               PIC X(1)    VALUE SPACE.                 
017200         05 FR-TAB-ADPLATS-EMB1-VCOM  PIC Z(5).                           
017300         05 FILLER               PIC X(69)   VALUE SPACE.                 
017400     03  FILLER.                                                          
017500         05 FR-TAB-ADLAGOMR-EMB2-VCOM PIC Z(2).                           
017600         05 FILLER               PIC X(1)    VALUE SPACE.                 
017700         05 FR-TAB-ADGANG-EMB2-VCOM   PIC Z(2).                           
017800         05 FILLER               PIC X(1)    VALUE SPACE.                 
017900         05 FR-TAB-ADPLATS-EMB2-VCOM  PIC Z(5).                           
018000         05 FILLER               PIC X(69)   VALUE SPACE.                 
018100     03  FILLER.                                                          
018200         05 FR-TAB-KDLAGEMB-VCOM PIC X(4)    VALUE SPACE.                 
018300         05 FILLER               PIC X(76)   VALUE SPACE.                 
018400     03  FILLER.                                                          
018500         05 FR-TAB-KDKVAANT-TEXT-VCOM PIC X(3)    VALUE SPACE.            
018600         05 FILLER               PIC X(77)   VALUE SPACE.                 
018700     03  FILLER.                                                          
018800         05 FR-TAB-BEARTURS-VCOM      PIC X(15)   VALUE SPACE.            
018900         05 FILLER               PIC X(65)   VALUE SPACE.                 
019000     03  FILLER.                                                          
019100         05 FR-TAB-IDARTNR-EMBQ0-VCOM PIC Z(8).                           
019200         05 FILLER               PIC X(72)   VALUE SPACE.                 
019300     03  FILLER.                                                          
019400         05 FR-TAB-IDARTNR-EMBQ1-VCOM PIC Z(8).                           
019500         05 FILLER               PIC X(72)   VALUE SPACE.                 
019600     03  FILLER.                                                          
019700         05 FR-TAB-IDARTNR-EMBQ2-VCOM PIC Z(8).                           
019800         05 FILLER               PIC X(72)   VALUE SPACE.                 
019900     03  FILLER.                                                          
020000         05 FR-TAB-BEART-VCOM    PIC X(25)   VALUE SPACE.                 
020100         05 FILLER               PIC X(55)   VALUE SPACE.                 
020200     03  FILLER.                                                          
020300         05 FR-TAB-KVAVIS-VCOM   PIC Z(5)9.                               
020400         05 FILLER               PIC X(74)   VALUE SPACE.                 
020500     03  FILLER.                                                          
020600         05 FR-TAB-VKART-VCOM    PIC Z(6)9.                               
020700         05 FILLER               PIC X(73)   VALUE SPACE.                 
020800     03  FILLER.                                                          
020900         05 FR-TAB-ADBUFFOMR-1-VCOM   PIC Z(1)9.                          
021000         05 FILLER               PIC X(1)    VALUE SPACE.                 
021100         05 FR-TAB-ADBUFFGANG-1-VCOM  PIC Z(1)9.                          
021200         05 FILLER               PIC X(1)    VALUE SPACE.                 
021300         05 FR-TAB-ADBUFFPL-1-VCOM    PIC Z(4)9.                          
021400         05 FILLER               PIC X(69)   VALUE SPACE.                 
021500     03  FILLER.                                                          
021600         05 FR-TAB-ADBUFFOMR-2-VCOM   PIC Z(2).                           
021700         05 FILLER               PIC X(1)    VALUE SPACE.                 
021800         05 FR-TAB-ADBUFFGANG-2-VCOM  PIC Z(2).                           
021900         05 FILLER               PIC X(1)    VALUE SPACE.                 
022000         05 FR-TAB-ADBUFFPL-2-VCOM    PIC Z(5).                           
022100         05 FILLER               PIC X(69)   VALUE SPACE.                 
022200     03  FILLER.                                                          
022300         05 FR-TAB-ADBUFFOMR-3-VCOM   PIC Z(2).                           
022400         05 FILLER               PIC X(1)    VALUE SPACE.                 
022500         05 FR-TAB-ADBUFFGANG-3-VCOM  PIC Z(2).                           
022600         05 FILLER               PIC X(1)    VALUE SPACE.                 
022700         05 FR-TAB-ADBUFFPL-3-VCOM    PIC Z(5).                           
022800         05 FILLER               PIC X(69)   VALUE SPACE.                 
022900     03  FILLER.                                                          
023000         05 FR-TAB-BEART-EMB0-VCOM    PIC X(25)   VALUE SPACE.            
023100         05 FILLER               PIC X(55)   VALUE SPACE.                 
023200     03  FILLER.                                                          
023300         05 FR-TAB-BEART-EMB1-VCOM    PIC X(25)   VALUE SPACE.            
023400         05 FILLER               PIC X(55)   VALUE SPACE.                 
023500     03  FILLER.                                                          
023600         05 FR-TAB-BEART-EMB2-VCOM    PIC X(25)   VALUE SPACE.            
023700         05 FILLER               PIC X(55)   VALUE SPACE.                 
023800     03  FILLER.                                                          
023900         05 FR-TAB-KVAVIS-PRIO-VCOM   PIC Z(5)9.                          
024000         05 FILLER               PIC X(74)   VALUE SPACE.                 
024100     03  FILLER.                                                          
024200         05 FR-TAB-VLARTNTO-VCOM      PIC Z(7)9.9.                        
024300         05 FILLER               PIC X(70)   VALUE SPACE.                 
024400     03  FILLER.                                                          
024500         05 FR-TAB-IDARTNR-VCOM       PIC Z(8).                           
024600         05 FILLER               PIC X(72)   VALUE SPACE.                 
024700     03  FILLER.                                                          
024800         05 FR-TAB-KVAVIS-KIT-VCOM    PIC Z(5)9.                          
024900         05 FILLER                    PIC X(1)    VALUE '/'.              
025000         05 FR-TAB-ADTRDEST-KIT-VCOM  PIC X(3).                           
025100         05 FILLER                    PIC X(70)   VALUE SPACE.            
025200     03  FILLER.                                                          
025300         05 FR-TAB-BEFT-VCOM          PIC Z(2).                           
025400         05 FILLER               PIC X(78)   VALUE SPACE.                 
025500     03  FILLER.                                                          
025600         05 FR-TAB-KDFARLIG-TEXT-L-VCOM PIC X(10) VALUE SPACE.            
025700         05 FILLER               PIC X(70)   VALUE SPACE.                 
025800     03  FILLER.                                                          
025900         05 FR-TAB-KVROS-VCOM PIC Z(06) VALUE ZERO.                       
026000         05 FILLER               PIC X(74)   VALUE SPACE.                 
026100     03  FILLER.                                                          
026200         05 FR-TAB-QTYP0-VCOM         PIC X(1)    VALUE SPACE.            
026300         05 FILLER               PIC X(79)   VALUE SPACE.                 
026400     03  FILLER.                                                          
026500         05 FR-TAB-QTYP1-VCOM         PIC Z(1).                           
026600         05 FILLER               PIC X(79)   VALUE SPACE.                 
026700     03  FILLER.                                                          
026800         05 FR-TAB-QTYP2-VCOM         PIC Z(1).                           
026900         05 FILLER               PIC X(79)   VALUE SPACE.                 
027000     03  FILLER.                                                          
027100         05 FR-TAB-TIAAMMDD-VCOM      PIC 9(6).                           
027200         05 FILLER               PIC X(74)   VALUE SPACE.                 
027300     03  FILLER.                                                          
027400         05 FR-TAB-KDSORT-VCOM   PIC X(2)    VALUE SPACE.                 
027500         05 FILLER               PIC X(78)   VALUE SPACE.                 
027600     03  FILLER.                                                          
027700         05 FR-TAB-KVQPACK0-VCOM PIC Z(5).                                
027800         05 FILLER               PIC X(75)   VALUE SPACE.                 
027900     03  FILLER.                                                          
028000         05 FR-TAB-KVQPACK1-VCOM PIC Z(5).                                
028100         05 FILLER               PIC X(75)   VALUE SPACE.                 
028200     03  FILLER.                                                          
028300         05 FR-TAB-KVQPACK2-VCOM PIC Z(5).                                
028400         05 FILLER               PIC X(75)   VALUE SPACE.                 
028500     03  FILLER.                                                          
028600         05 FR-TAB-HH-VCOM       PIC 9(2).                                
028700         05 FILLER               PIC X(1)    VALUE '.'.                   
028800         05 FR-TAB-MM-VCOM       PIC 9(2).                                
028900         05 FILLER               PIC X(1)    VALUE '.'.                   
029000         05 FR-TAB-SS-VCOM       PIC 9(2).                                
029100         05 FILLER               PIC X(72)   VALUE SPACE.                 
029200     03  FILLER.                                                          
029300         05 FR-TAB-ADINLOMR-VCOM PIC X(4).                                
029400         05 FILLER               PIC X(76)   VALUE SPACE.                 
029500     03  FILLER.                                                          
029600         05 FR-TAB-KDFORP-VCOM   PIC Z(5).                                
029700         05 FILLER               PIC X(75)   VALUE SPACE.                 
029800     03  FILLER                  PIC X(80)   VALUE                        
029900         '!P                                                    '.        
030000     03  FILLER                  PIC X(80)   VALUE                        
030100         '!R                                                    '.        
030200     SKIP3                                                                
030300 01  FILLER REDEFINES FR-TAB-VCOM.                                        
030400     03 FILLER OCCURS 42.                                                 
030500        05 FR-RAD-VCOM           PIC X(80).                               
030600     SKIP3                                                                
030700 01  FR-LIST-RAD                 PIC X(132) VALUE SPACE.                  
030800     EJECT                                                                
030900*    --- FÖRBEHANDLINGSRAPPORT VIA TERMO SKRIVARE                         
031000*    --- FÖRBEHANDLINGSRAPPORT VIA TERMO SKRIVARE                         
031100*    --- FÖRBEHANDLINGSRAPPORT VIA TERMO SKRIVARE                         
031200*    --- FÖRBEHANDLINGSRAPPORT VIA TERMO SKRIVARE                         
031300 01      FILLER                  PIC X(8)    VALUE 'FR TERMO'.            
031400 01      FR-TAB.                                                          
031500     03  FILLER                  PIC X(80)   VALUE                        
031600         '!C                                                   '.         
031700     03  FILLER                  PIC X(80)   VALUE                        
031800         '!C                                                   '.         
031900     03  FILLER                  PIC X(80)   VALUE                        
032000         '!K 125                                               '.         
032100     03  FILLER                  PIC X(80)   VALUE                        
032200         '!F T S 1300 2145 L 2 2 3 "VOLVO"                     '.         
032300     03  FILLER                  PIC X(80)   VALUE                        
032400         '!F T S 1250 2145 L 1 1 3 "Car Aftersales"            '.         
032500     03  FILLER                  PIC X(80)   VALUE                        
032600         '!F T S 1170 2145 L 1 1 3 "Partinr"                   '.         
032700     03  FILLER                  PIC X(80)   VALUE                        
032800         '!F T S  770 2145 L 1 1 3 "Q3-kvant"                  '.         
032900     03  FILLER                  PIC X(80)   VALUE                        
033000         '!F T S  670 2145 L 1 1 3 "Lagerplats"                '.         
033100     03  FILLER                  PIC X(80)   VALUE                        
033200         '!F T S  550 2145 L 2 1 3 "Förpacknings material"     '.         
033300     03  FILLER                  PIC X(80)   VALUE                        
033400         '!F T S  470 2145 L 1 1 3 "Lagerplats"                '.         
033500     03  FILLER                  PIC X(80)   VALUE                        
033600         '!F T S  770 1895 L 1 1 3 "Emballage"                 '.         
033700     03  FILLER                  PIC X(80)   VALUE                        
033800         '!F T S  770 1645 L 1 1 3 "Antalskontroll"            '.         
033900     03  FILLER                  PIC X(80)   VALUE                        
034000         '!F T S  670 1645 L 1 1 3 "Ursprung"                  '.         
034100     03  FILLER                  PIC X(80)   VALUE                        
034200         '!F T S  470 1645 L 1 1 3 "Emb artnr"                 '.         
034300     03  FILLER                  PIC X(80)   VALUE                        
034400         '!F T S 1300 1245 L 2 2 3 "Förbehandlingsrapport"     '.         
034500     03  FILLER                  PIC X(80)   VALUE                        
034600         '!F T S 1170 1245 L 1 1 3 "Benämning"                 '.         
034700     03  FILLER                  PIC X(80)   VALUE                        
034800         '!F T S  970 1245 L 1 1 3 "Avis"                      '.         
034900     03  FILLER                  PIC X(80)   VALUE                        
035000         '!F T S  870 1245 L 1 1 3 "Vikt gram"                 '.         
035100     03  FILLER                  PIC X(80)   VALUE                        
035200         '!F T S  770 1245 L 1 1 3 "Buffertplats"              '.         
035300     03  FILLER                  PIC X(80)   VALUE                        
035400         '!F T S  470 1245 L 1 1 3 "Benämning"                 '.         
035500     03  FILLER                  PIC X(80)   VALUE                        
035600         '!F T S  970  945 L 1 1 3 "Antal prio"                '.         
035700     03  FILLER                  PIC X(80)   VALUE                        
035800         '!F T S  870  945 L 1 1 3 "Volym cm3"                 '.         
035900     03  FILLER                  PIC X(80)   VALUE                        
036000         '!F T S 1170  645 L 1 1 3 "Artnr"                     '.         
036100     03  FILLER                  PIC X(80)   VALUE                        
036200         '!F T S  970  645 L 1 1 3 "Antal till sats"           '.         
036300     03  FILLER                  PIC X(80)   VALUE                        
036400         '!F T S  870  645 L 1 1 3 "Förp.typ"                  '.         
036500     03  FILLER                  PIC X(80)   VALUE                        
036600         '!F T S  770  645 L 1 1 3 "L farligt gods"            '.         
036700     03  FILLER                  PIC X(80)   VALUE                        
036800         '!F T S  670  645 L 1 1 3 "Ant. RO       "            '.         
036900     03  FILLER                  PIC X(80)   VALUE                        
037000         '!F T S  470  645 L 1 1 3 "Q-typ"                     '.         
037100     03  FILLER                  PIC X(80)   VALUE                        
037200         '!F T S  870  345 L 1 1 3 "Enhet"                     '.         
037300     03  FILLER                  PIC X(80)   VALUE                        
037400         '!F T S  470  345 L 1 1 3 "Antal förp"                '.         
037500     03  FILLER                  PIC X(80)   VALUE                        
037600         '!F T S  970  345 L 1 1 3 "Förp.kod"                  '.         
037700     03  FILLER.                                                          
037800         05 FILLER               PIC X(26)   VALUE                        
037900         '!F T S 1120 2145 L 2 1 3 "'.                                    
038000         05 FR-TAB-IDLOPNRM      PIC 9(8).                                
038100         05 FILLER               PIC X(46)   VALUE                        
038200         '"            '.                                                 
038300     03  FILLER.                                                          
038400         05 FILLER               PIC X(26)   VALUE                        
038500         '!F T S  720 2145 L 2 1 3 "'.                                    
038600         05 FR-TAB-KVQPACK-3     PIC Z(6)9.                               
038700         05 FILLER               PIC X(47)   VALUE                        
038800         '"            '.                                                 
038900     03  FILLER.                                                          
039000         05 FILLER               PIC X(26)   VALUE                        
039100         '!F T S  620 2145 L 2 1 3 "'.                                    
039200         05 FR-TAB-ADLAGOMR      PIC 9(2).                                
039300         05 FILLER               PIC X(1)    VALUE SPACE.                 
039400         05 FR-TAB-ADGANG        PIC Z(2).                                
039500         05 FILLER               PIC X(1)    VALUE SPACE.                 
039600         05 FR-TAB-ADPLATS       PIC Z(5).                                
039700         05 FILLER               PIC X(43)   VALUE                        
039800         '"            '.                                                 
039900     03  FILLER.                                                          
040000         05 FILLER               PIC X(26)   VALUE                        
040100         '!F T S  420 2145 L 2 1 3 "'.                                    
040200         05 FR-TAB-ADLAGOMR-EMB0 PIC Z(2).                                
040300         05 FILLER               PIC X(1)    VALUE SPACE.                 
040400         05 FR-TAB-ADGANG-EMB0   PIC Z(2).                                
040500         05 FILLER               PIC X(1)    VALUE SPACE.                 
040600         05 FR-TAB-ADPLATS-EMB0  PIC Z(5).                                
040700         05 FILLER               PIC X(43)   VALUE                        
040800         '"            '.                                                 
040900     03  FILLER.                                                          
041000         05 FILLER               PIC X(26)   VALUE                        
041100         '!F T S  320 2145 L 2 1 3 "'.                                    
041200         05 FR-TAB-ADLAGOMR-EMB1 PIC Z(2).                                
041300         05 FILLER               PIC X(1)    VALUE SPACE.                 
041400         05 FR-TAB-ADGANG-EMB1   PIC Z(2).                                
041500         05 FILLER               PIC X(1)    VALUE SPACE.                 
041600         05 FR-TAB-ADPLATS-EMB1  PIC Z(5).                                
041700         05 FILLER               PIC X(43)   VALUE                        
041800         '"            '.                                                 
041900     03  FILLER.                                                          
042000         05 FILLER               PIC X(26)   VALUE                        
042100         '!F T S  220 2145 L 2 1 3 "'.                                    
042200         05 FR-TAB-ADLAGOMR-EMB2 PIC Z(2).                                
042300         05 FILLER               PIC X(1)    VALUE SPACE.                 
042400         05 FR-TAB-ADGANG-EMB2   PIC Z(2).                                
042500         05 FILLER               PIC X(1)    VALUE SPACE.                 
042600         05 FR-TAB-ADPLATS-EMB2  PIC Z(5).                                
042700         05 FILLER               PIC X(43)   VALUE                        
042800         '"            '.                                                 
042900     03  FILLER.                                                          
043000         05 FILLER               PIC X(26)   VALUE                        
043100         '!F T S  720 1895 L 2 1 3 "'.                                    
043200         05 FR-TAB-KDLAGEMB      PIC X(4)    VALUE SPACE.                 
043300         05 FILLER               PIC X(50)   VALUE                        
043400         '"            '.                                                 
043500     03  FILLER.                                                          
043600         05 FILLER               PIC X(26)   VALUE                        
043700         '!F T S  720 1645 L 2 1 3 "'.                                    
043800         05 FR-TAB-KDKVAANT-TEXT PIC X(3)    VALUE SPACE.                 
043900         05 FILLER               PIC X(51)   VALUE                        
044000         '"            '.                                                 
044100     03  FILLER.                                                          
044200         05 FILLER               PIC X(26)   VALUE                        
044300         '!F T S  620 1645 L 2 1 3 "'.                                    
044400         05 FR-TAB-BEARTURS      PIC X(15)   VALUE SPACE.                 
044500         05 FILLER               PIC X(39)   VALUE                        
044600         '"            '.                                                 
044700     03  FILLER.                                                          
044800         05 FILLER               PIC X(26)   VALUE                        
044900         '!F T S  420 1645 L 2 1 3 "'.                                    
045000         05 FR-TAB-IDARTNR-EMBQ0 PIC Z(8).                                
045100         05 FILLER               PIC X(46)   VALUE                        
045200         '"            '.                                                 
045300     03  FILLER.                                                          
045400         05 FILLER               PIC X(26)   VALUE                        
045500         '!F T S  320 1645 L 2 1 3 "'.                                    
045600         05 FR-TAB-IDARTNR-EMBQ1 PIC Z(8).                                
045700         05 FILLER               PIC X(46)   VALUE                        
045800         '"            '.                                                 
045900     03  FILLER.                                                          
046000         05 FILLER               PIC X(26)   VALUE                        
046100         '!F T S  220 1645 L 2 1 3 "'.                                    
046200         05 FR-TAB-IDARTNR-EMBQ2 PIC Z(8).                                
046300         05 FILLER               PIC X(46)   VALUE                        
046400         '"            '.                                                 
046500     03  FILLER.                                                          
046600         05 FILLER               PIC X(26)   VALUE                        
046700         '!F T S 1120 1245 L 2 1 3 "'.                                    
046800         05 FR-TAB-BEART         PIC X(25)   VALUE SPACE.                 
046900         05 FILLER               PIC X(29)   VALUE                        
047000         '"            '.                                                 
047100     03  FILLER.                                                          
047200         05 FILLER               PIC X(26)   VALUE                        
047300         '!F T S  920 1245 L 2 1 3 "'.                                    
047400         05 FR-TAB-KVAVIS        PIC Z(5)9.                               
047500         05 FILLER               PIC X(48)   VALUE                        
047600         '"            '.                                                 
047700     03  FILLER.                                                          
047800         05 FILLER               PIC X(26)   VALUE                        
047900         '!F T S  820 1245 L 2 1 3 "'.                                    
048000         05 FR-TAB-VKART         PIC Z(6)9.                               
048100         05 FILLER               PIC X(47)   VALUE                        
048200         '"            '.                                                 
048300     03  FILLER.                                                          
048400         05 FILLER               PIC X(26)   VALUE                        
048500         '!F T S  720 1245 L 2 1 3 "'.                                    
048600         05 FR-TAB-ADBUFFOMR-1   PIC Z(1)9.                               
048700         05 FILLER               PIC X(1)    VALUE SPACE.                 
048800         05 FR-TAB-ADBUFFGANG-1  PIC Z(1)9.                               
048900         05 FILLER               PIC X(1)    VALUE SPACE.                 
049000         05 FR-TAB-ADBUFFPL-1    PIC Z(4)9.                               
049100         05 FILLER               PIC X(43)   VALUE                        
049200         '"            '.                                                 
049300     03  FILLER.                                                          
049400         05 FILLER               PIC X(26)   VALUE                        
049500         '!F T S  670 1245 L 2 1 3 "'.                                    
049600         05 FR-TAB-ADBUFFOMR-2   PIC Z(2).                                
049700         05 FILLER               PIC X(1)    VALUE SPACE.                 
049800         05 FR-TAB-ADBUFFGANG-2  PIC Z(2).                                
049900         05 FILLER               PIC X(1)    VALUE SPACE.                 
050000         05 FR-TAB-ADBUFFPL-2    PIC Z(5).                                
050100         05 FILLER               PIC X(43)   VALUE                        
050200         '"            '.                                                 
050300     03  FILLER.                                                          
050400         05 FILLER               PIC X(26)   VALUE                        
050500         '!F T S  620 1245 L 2 1 3 "'.                                    
050600         05 FR-TAB-ADBUFFOMR-3   PIC Z(2).                                
050700         05 FILLER               PIC X(1)    VALUE SPACE.                 
050800         05 FR-TAB-ADBUFFGANG-3  PIC Z(2).                                
050900         05 FILLER               PIC X(1)    VALUE SPACE.                 
051000         05 FR-TAB-ADBUFFPL-3    PIC Z(5).                                
051100         05 FILLER               PIC X(43)   VALUE                        
051200         '"            '.                                                 
051300     03  FILLER.                                                          
051400         05 FILLER               PIC X(26)   VALUE                        
051500         '!F T S  420 1245 L 2 1 3 "'.                                    
051600         05 FR-TAB-BEART-EMB0    PIC X(25)   VALUE SPACE.                 
051700         05 FILLER               PIC X(29)   VALUE                        
051800         '"            '.                                                 
051900     03  FILLER.                                                          
052000         05 FILLER               PIC X(26)   VALUE                        
052100         '!F T S  320 1245 L 2 1 3 "'.                                    
052200         05 FR-TAB-BEART-EMB1    PIC X(25)   VALUE SPACE.                 
052300         05 FILLER               PIC X(29)   VALUE                        
052400         '"            '.                                                 
052500     03  FILLER.                                                          
052600         05 FILLER               PIC X(26)   VALUE                        
052700         '!F T S  220 1245 L 2 1 3 "'.                                    
052800         05 FR-TAB-BEART-EMB2    PIC X(25)   VALUE SPACE.                 
052900         05 FILLER               PIC X(29)   VALUE                        
053000         '"            '.                                                 
053100     03  FILLER.                                                          
053200         05 FILLER               PIC X(26)   VALUE                        
053300         '!F T S  920  945 L 2 1 3 "'.                                    
053400         05 FR-TAB-KVAVIS-PRIO   PIC Z(5)9.                               
053500         05 FILLER               PIC X(48)   VALUE                        
053600         '"            '.                                                 
053700     03  FILLER.                                                          
053800         05 FILLER               PIC X(26)   VALUE                        
053900         '!F T S  820  945 L 2 1 3 "'.                                    
054000         05 FR-TAB-VLARTNTO      PIC Z(7)9.9.                             
054100         05 FILLER               PIC X(44)   VALUE                        
054200         '"            '.                                                 
054300     03  FILLER.                                                          
054400         05 FILLER               PIC X(26)   VALUE                        
054500         '!F T S 1120  645 L 2 1 3 "'.                                    
054600         05 FR-TAB-IDARTNR       PIC Z(8).                                
054700         05 FILLER               PIC X(46)   VALUE                        
054800         '"            '.                                                 
054900     03  FILLER.                                                          
055000         05 FILLER               PIC X(26)   VALUE                        
055100         '!F T S  920  475 L 2 1 3 "'.                                    
055200         05 FR-TAB-ADTRDEST-KIT  PIC X(3).                                
055300         05 FILLER               PIC X(51)   VALUE                        
055400         '"            '.                                                 
055500     03  FILLER.                                                          
055600         05 FILLER               PIC X(26)   VALUE                        
055700         '!F T S  920  645 L 2 1 3 "'.                                    
055800         05 FR-TAB-KVAVIS-KIT    PIC Z(5)9.                               
055900         05 FILLER               PIC X(48)   VALUE                        
056000         '"            '.                                                 
056100     03  FILLER.                                                          
056200         05 FILLER               PIC X(26)   VALUE                        
056300         '!F T S  820  645 L 2 1 3 "'.                                    
056400         05 FR-TAB-BEFT          PIC Z(2).                                
056500         05 FILLER               PIC X(52)   VALUE                        
056600         '"            '.                                                 
056700     03  FILLER.                                                          
056800         05 FILLER               PIC X(26)   VALUE                        
056900         '!F T S  720  645 L 2 1 3 "'.                                    
057000         05 FR-TAB-KDFARLIG-TEXT-L PIC X(10) VALUE SPACE.                 
057100         05 FILLER               PIC X(44)   VALUE                        
057200         '"            '.                                                 
057300     03  FILLER.                                                          
057400         05 FILLER               PIC X(26)   VALUE                        
057500         '!F T S  620  645 L 2 1 3 "'.                                    
057600         05 FR-TAB-KVROS         PIC Z(06) VALUE ZERO.                    
057700         05 FILLER               PIC X(48)   VALUE                        
057800         '"            '.                                                 
057900     03  FILLER.                                                          
058000         05 FILLER               PIC X(26)   VALUE                        
058100         '!F T S  420  645 L 2 1 3 "'.                                    
058200         05 FR-TAB-QTYP0         PIC X(1)    VALUE SPACE.                 
058300         05 FILLER               PIC X(53)   VALUE                        
058400         '"            '.                                                 
058500     03  FILLER.                                                          
058600         05 FILLER               PIC X(26)   VALUE                        
058700         '!F T S  320  645 L 2 1 3 "'.                                    
058800         05 FR-TAB-QTYP1         PIC Z(1).                                
058900         05 FILLER               PIC X(53)   VALUE                        
059000         '"            '.                                                 
059100     03  FILLER.                                                          
059200         05 FILLER               PIC X(26)   VALUE                        
059300         '!F T S  220  645 L 2 1 3 "'.                                    
059400         05 FR-TAB-QTYP2         PIC Z(1).                                
059500         05 FILLER               PIC X(53)   VALUE                        
059600         '"            '.                                                 
059700     03  FILLER.                                                          
059800         05 FILLER               PIC X(26)   VALUE                        
059900         '!F T S   60  500 L 1 1 3 "'.                                    
060000         05 FR-TAB-TIAAMMDD      PIC 9(6).                                
060100         05 FILLER               PIC X(48)   VALUE                        
060200         '"            '.                                                 
060300     03  FILLER.                                                          
060400         05 FILLER               PIC X(26)   VALUE                        
060500         '!F T S  820  345 L 2 1 3 "'.                                    
060600         05 FR-TAB-KDSORT        PIC X(2)    VALUE SPACE.                 
060700         05 FILLER               PIC X(52)   VALUE                        
060800         '"            '.                                                 
060900     03  FILLER.                                                          
061000         05 FILLER               PIC X(26)   VALUE                        
061100         '!F T S  420  345 L 2 1 3 "'.                                    
061200         05 FR-TAB-KVQPACK0      PIC Z(5).                                
061300         05 FILLER               PIC X(49)   VALUE                        
061400         '"            '.                                                 
061500     03  FILLER.                                                          
061600         05 FILLER               PIC X(26)   VALUE                        
061700         '!F T S  320  345 L 2 1 3 "'.                                    
061800         05 FR-TAB-KVQPACK1      PIC Z(5).                                
061900         05 FILLER               PIC X(49)   VALUE                        
062000         '"            '.                                                 
062100     03  FILLER.                                                          
062200         05 FILLER               PIC X(26)   VALUE                        
062300         '!F T S  220  345 L 2 1 3 "'.                                    
062400         05 FR-TAB-KVQPACK2      PIC Z(5).                                
062500         05 FILLER               PIC X(49)   VALUE                        
062600         '"            '.                                                 
062700     03  FILLER.                                                          
062800         05 FILLER               PIC X(26)   VALUE                        
062900         '!F T S   60  345 L 1 1 3 "'.                                    
063000         05 FR-TAB-HH            PIC 9(2).                                
063100         05 FILLER               PIC X(1)    VALUE '.'.                   
063200         05 FR-TAB-MM            PIC 9(2).                                
063300         05 FILLER               PIC X(1)    VALUE '.'.                   
063400         05 FR-TAB-SS            PIC 9(2).                                
063500         05 FILLER               PIC X(46)   VALUE                        
063600         '"            '.                                                 
063700     03  FILLER.                                                          
063800         05 FILLER               PIC X(26)   VALUE                        
063900         '!F T S 1300  445 L 2 2 3 "'.                                    
064000         05 FR-TAB-ADINLOMR      PIC X(4).                                
064100         05 FILLER               PIC X(50)   VALUE                        
064200         '"            '.                                                 
064300     03  FILLER.                                                          
064400         05 FILLER               PIC X(26)   VALUE                        
064501         '!F T S  920  345 L 2 1 3 "'.                                    
064600         05 FR-TAB-KDFORP        PIC Z(5).                                
064700         05 FILLER               PIC X(49)   VALUE                        
064800         '"            '.                                                 
064900     03  FILLER.                                                          
065000         05 FILLER               PIC X(30)   VALUE                        
065100         '!F C S  900 2045 L 130 3 12 "P'.                                
065200         05 FR-TAB-IDLOPNRM-STRK PIC 9(8).                                
065300         05 FILLER               PIC X(42)   VALUE                        
065400         '"            '.                                                 
065500     03  FILLER                  PIC X(80)   VALUE                        
065600         '!F B S 1200 2160 L 5 2010                            '.         
065700     03  FILLER                  PIC X(80)   VALUE                        
065800         '!F B S 1000 1260 L 5 1100                            '.         
065900     03  FILLER                  PIC X(80)   VALUE                        
066000         '!F B S  900 1260 L 5 1100                            '.         
066100     03  FILLER                  PIC X(80)   VALUE                        
066200         '!F B S  800 2160 L 5 2010                            '.         
066300     03  FILLER                  PIC X(80)   VALUE                        
066400         '!F B S  700 2160 L 5  900                            '.         
066500     03  FILLER                  PIC X(80)   VALUE                        
066600         '!F B S  700  660 L 5  510                            '.         
066700     03  FILLER                  PIC X(80)   VALUE                        
066800         '!F B S  600 2160 L 5 2010                            '.         
066900     03  FILLER                  PIC X(80)   VALUE                        
067000         '!F B S   50 2160 L 5 2010                            '.         
067100     03  FILLER                  PIC X(80)   VALUE                        
067200         '!F B S   50 2160 L 1150 5                            '.         
067300     03  FILLER                  PIC X(80)   VALUE                        
067400         '!F B S  700 1910 L  100 5                            '.         
067500     03  FILLER                  PIC X(80)   VALUE                        
067600         '!F B S  600 1660 L  200 5                            '.         
067700     03  FILLER                  PIC X(80)   VALUE                        
067800         '!F B S  600 1260 L  600 5                            '.         
067900     03  FILLER                  PIC X(80)   VALUE                        
068000         '!F B S  800  960 L  200 5                            '.         
068100     03  FILLER                  PIC X(80)   VALUE                        
068200         '!F B S  600  660 L  600 5                            '.         
068300     03  FILLER                  PIC X(80)   VALUE                        
068400         '!F B S  800  360 L  200 5                            '.         
068500     03  FILLER                  PIC X(80)   VALUE                        
068600         '!F B S   50  150 L 1150 5                            '.         
068700     03  FILLER                  PIC X(80)   VALUE                        
068800         '!P                                                   '.         
068900     03  FILLER                  PIC X(80)   VALUE                        
069000         '!R                                                   '.         
069100     SKIP3                                                                
069200 01  FILLER REDEFINES FR-TAB.                                             
069300     03 FILLER OCCURS 90.                                                 
069400        05 FR-RAD                PIC X(80).                               
069500     EJECT                                                                
069600*    --- FÖRBEHANDLINGSRAPPORT VIA  NOVA SKRIVARE                         
069700*    --- FÖRBEHANDLINGSRAPPORT VIA  NOVA SKRIVARE                         
069800*    --- FÖRBEHANDLINGSRAPPORT VIA  NOVA SKRIVARE                         
069900*    --- FÖRBEHANDLINGSRAPPORT VIA  NOVA SKRIVARE                         
070000 01      FILLER                  PIC X(8)    VALUE 'FR NOVA'.             
070100 01      FR-NOVA-TAB.                                                     
070200     03  FILLER                  PIC X(80)   VALUE                        
070300         '!C                                                   '.         
070400     03  FILLER                  PIC X(80)   VALUE                        
070500         '!C                                                   '.         
070600     03  FILLER                  PIC X(80)   VALUE                        
070700         '!F T E 1470   20 L 2 2 3 "VOLVO"                     '.         
070800     03  FILLER                  PIC X(80)   VALUE                        
070900         '!F T E 1410   20 L 1 1 3 "Car Aftersales"            '.         
071000     03  FILLER                  PIC X(80)   VALUE                        
071100         '!F T E 1350   20 L 1 1 3 "Partinr"                   '.         
071200     03  FILLER                  PIC X(80)   VALUE                        
071300         '!F T E  950   20 L 1 1 3 "Q3-kvant"                  '.         
071400     03  FILLER                  PIC X(80)   VALUE                        
071500         '!F T E  840   20 L 1 1 3 "Lagerplats"                '.         
071600     03  FILLER                  PIC X(80)   VALUE                        
071700         '!F T E  720   20 L 2 1 3 "Förpacknings material"     '.         
071800     03  FILLER                  PIC X(80)   VALUE                        
071900         '!F T E  640   20 L 1 1 3 "Lagerplats"                '.         
072000     03  FILLER                  PIC X(80)   VALUE                        
072100         '!F T E  950  270 L 1 1 3 "Emballage"                 '.         
072200     03  FILLER                  PIC X(80)   VALUE                        
072300         '!F T E  950  520 L 1 1 3 "Antalskontroll"            '.         
072400     03  FILLER                  PIC X(80)   VALUE                        
072500         '!F T E  840  520 L 1 1 3 "Ursprung"                  '.         
072600     03  FILLER                  PIC X(80)   VALUE                        
072700         '!F T E  640  520 L 1 1 3 "Emb artnr"                 '.         
072800     03  FILLER                  PIC X(80)   VALUE                        
072900         '!F T E 1470  900 L 2 2 3 "Förbehandlingsrapport"     '.         
073000     03  FILLER                  PIC X(80)   VALUE                        
073100         '!F T E  640  900 L 1 1 3 "Benämning"                 '.         
073200     03  FILLER                  PIC X(80)   VALUE                        
073300         '!F T E 1150  910 L 1 1 3 "Avis"                      '.         
073400     03  FILLER                  PIC X(80)   VALUE                        
073500         '!F T E 1050  910 L 1 1 3 "Vikt gram"                 '.         
073600     03  FILLER                  PIC X(80)   VALUE                        
073700         '!F T E  950  910 L 1 1 3 "Buffertplats"              '.         
073800     03  FILLER                  PIC X(80)   VALUE                        
073900         '!F T E 1350  910 L 1 1 3 "Benämning"                 '.         
074000     03  FILLER                  PIC X(80)   VALUE                        
074100         '!F T E 1150 1210 L 1 1 3 "Antal prio"                '.         
074200     03  FILLER                  PIC X(80)   VALUE                        
074300         '!F T E 1050 1210 L 1 1 3 "Volym cm3"                 '.         
074400     03  FILLER                  PIC X(80)   VALUE                        
074500         '!F T E 1350 1480 L 1 1 3 "Artnr"                     '.         
074600     03  FILLER                  PIC X(80)   VALUE                        
074700         '!F T E 1150 1480 L 1 1 3 "Antal till sats"           '.         
074800     03  FILLER                  PIC X(80)   VALUE                        
074900         '!F T E 1050 1480 L 1 1 3 "Förp.typ"                  '.         
075000     03  FILLER                  PIC X(80)   VALUE                        
075100         '!F T E  950 1490 L 1 1 3 "L farligt gods"            '.         
075200     03  FILLER                  PIC X(80)   VALUE                        
075300         '!F T E  840 1490 L 1 1 3 "Ant. RO       "            '.         
075400     03  FILLER                  PIC X(80)   VALUE                        
075500         '!F T E  640 1480 L 1 1 3 "Q-typ"                     '.         
075600     03  FILLER                  PIC X(80)   VALUE                        
075700         '!F T E 1150 1810 L 1 1 3 "Förp.kod"                  '.         
075800     03  FILLER                  PIC X(80)   VALUE                        
075900         '!F T E 1050 1810 L 1 1 3 "Enhet"                     '.         
076000     03  FILLER                  PIC X(80)   VALUE                        
076100         '!F T E  640 1790 L 1 1 3 "Antal förp"                '.         
076200     03  FILLER.                                                          
076300         05 FILLER               PIC X(26)   VALUE                        
076400         '!F T E 1290   20 L 2 1 3 "'.                                    
076500         05 FR-NOVA-TAB-IDLOPNRM PIC 9(8).                                
076600         05 FILLER               PIC X(46)   VALUE                        
076700         '"            '.                                                 
076800     03  FILLER.                                                          
076900         05 FILLER               PIC X(26)   VALUE                        
077000         '!F T E  890   20 L 2 1 3 "'.                                    
077100         05 FR-NOVA-TAB-KVQPACK-3 PIC Z(6)9.                              
077200         05 FILLER               PIC X(47)   VALUE                        
077300         '"            '.                                                 
077400     03  FILLER.                                                          
077500         05 FILLER               PIC X(26)   VALUE                        
077600         '!F T E  780   20 L 2 1 3 "'.                                    
077700         05 FR-NOVA-TAB-ADLAGOMR PIC 9(2).                                
077800         05 FILLER               PIC X(1)    VALUE SPACE.                 
077900         05 FR-NOVA-TAB-ADGANG PIC Z(2).                                  
078000         05 FILLER               PIC X(1)    VALUE SPACE.                 
078100         05 FR-NOVA-TAB-ADPLATS PIC Z(5).                                 
078200         05 FILLER               PIC X(43)   VALUE                        
078300         '"            '.                                                 
078400     03  FILLER.                                                          
078500         05 FILLER               PIC X(26)   VALUE                        
078600         '!F T E  580   20 L 2 1 3 "'.                                    
078700         05 FR-NOVA-TAB-ADLAGOMR-EMB0 PIC Z(2).                           
078800         05 FILLER               PIC X(1)    VALUE SPACE.                 
078900         05 FR-NOVA-TAB-ADGANG-EMB0 PIC Z(2).                             
079000         05 FILLER               PIC X(1)    VALUE SPACE.                 
079100         05 FR-NOVA-TAB-ADPLATS-EMB0 PIC Z(5).                            
079200         05 FILLER               PIC X(43)   VALUE                        
079300         '"            '.                                                 
079400     03  FILLER.                                                          
079500         05 FILLER               PIC X(26)   VALUE                        
079600         '!F T E  480   20 L 2 1 3 "'.                                    
079700         05 FR-NOVA-TAB-ADLAGOMR-EMB1 PIC Z(2).                           
079800         05 FILLER               PIC X(1)    VALUE SPACE.                 
079900         05 FR-NOVA-TAB-ADGANG-EMB1 PIC Z(2).                             
080000         05 FILLER               PIC X(1)    VALUE SPACE.                 
080100         05 FR-NOVA-TAB-ADPLATS-EMB1 PIC Z(5).                            
080200         05 FILLER               PIC X(43)   VALUE                        
080300         '"            '.                                                 
080400     03  FILLER.                                                          
080500         05 FILLER               PIC X(26)   VALUE                        
080600         '!F T E  380   20 L 2 1 3 "'.                                    
080700         05 FR-NOVA-TAB-ADLAGOMR-EMB2 PIC Z(2).                           
080800         05 FILLER               PIC X(1)    VALUE SPACE.                 
080900         05 FR-NOVA-TAB-ADGANG-EMB2 PIC Z(2).                             
081000         05 FILLER               PIC X(1)    VALUE SPACE.                 
081100         05 FR-NOVA-TAB-ADPLATS-EMB2 PIC Z(5).                            
081200         05 FILLER               PIC X(43)   VALUE                        
081300         '"            '.                                                 
081400     03  FILLER.                                                          
081500         05 FILLER               PIC X(26)   VALUE                        
081600         '!F T E  890  400 L 2 1 3 "'.                                    
081700         05 FR-NOVA-TAB-KDLAGEMB PIC X(4)    VALUE SPACE.                 
081800         05 FILLER               PIC X(50)   VALUE                        
081900         '"            '.                                                 
082000     03  FILLER.                                                          
082100         05 FILLER               PIC X(26)   VALUE                        
082200         '!F T E  890  520 L 2 1 3 "'.                                    
082300         05 FR-NOVA-TAB-KDKVAANT-TEXT PIC X(3) VALUE SPACE.               
082400         05 FILLER               PIC X(51)   VALUE                        
082500         '"            '.                                                 
082600     03  FILLER.                                                          
082700         05 FILLER               PIC X(26)   VALUE                        
082800         '!F T E  790  520 L 2 1 3 "'.                                    
082900         05 FR-NOVA-TAB-BEARTURS PIC X(15)   VALUE SPACE.                 
083000         05 FILLER               PIC X(39)   VALUE                        
083100         '"            '.                                                 
083200     03  FILLER.                                                          
083300         05 FILLER               PIC X(26)   VALUE                        
083400         '!F T E  580  500 L 2 1 3 "'.                                    
083500         05 FR-NOVA-TAB-IDARTNR-EMBQ0 PIC Z(8).                           
083600         05 FILLER               PIC X(46)   VALUE                        
083700         '"            '.                                                 
083800     03  FILLER.                                                          
083900         05 FILLER               PIC X(26)   VALUE                        
084000         '!F T E  480  500 L 2 1 3 "'.                                    
084100         05 FR-NOVA-TAB-IDARTNR-EMBQ1 PIC Z(8).                           
084200         05 FILLER               PIC X(46)   VALUE                        
084300         '"            '.                                                 
084400     03  FILLER.                                                          
084500         05 FILLER               PIC X(26)   VALUE                        
084600         '!F T E  380  500 L 2 1 3 "'.                                    
084700         05 FR-NOVA-TAB-IDARTNR-EMBQ2 PIC Z(8).                           
084800         05 FILLER               PIC X(46)   VALUE                        
084900         '"            '.                                                 
085000     03  FILLER.                                                          
085100         05 FILLER               PIC X(26)   VALUE                        
085200         '!F T E  1300 920 L 2 1 3 "'.                                    
085300         05 FR-NOVA-TAB-BEART    PIC X(25)   VALUE SPACE.                 
085400         05 FILLER               PIC X(29)   VALUE                        
085500         '"            '.                                                 
085600     03  FILLER.                                                          
085700         05 FILLER               PIC X(26)   VALUE                        
085800         '!F T E 1100 1010 L 2 1 3 "'.                                    
085900         05 FR-NOVA-TAB-KVAVIS   PIC Z(5)9.                               
086000         05 FILLER               PIC X(48)   VALUE                        
086100         '"            '.                                                 
086200     03  FILLER.                                                          
086300         05 FILLER               PIC X(26)   VALUE                        
086400         '!F T E 1000  980 L 2 1 3 "'.                                    
086500         05 FR-NOVA-TAB-VKART    PIC Z(6)9.                               
086600         05 FILLER               PIC X(47)   VALUE                        
086700         '"            '.                                                 
086800     03  FILLER.                                                          
086900         05 FILLER               PIC X(26)   VALUE                        
087000         '!F T E  900  930 L 2 1 3 "'.                                    
087100         05 FR-NOVA-TAB-ADBUFFOMR-1 PIC Z(1)9.                            
087200         05 FILLER               PIC X(1)    VALUE SPACE.                 
087300         05 FR-NOVA-TAB-ADBUFFGANG-1 PIC Z(1)9.                           
087400         05 FILLER               PIC X(1)    VALUE SPACE.                 
087500         05 FR-NOVA-TAB-ADBUFFPL-1 PIC Z(4)9.                             
087600         05 FILLER               PIC X(43)   VALUE                        
087700         '"            '.                                                 
087800     03  FILLER.                                                          
087900         05 FILLER               PIC X(26)   VALUE                        
088000         '!F T E  850  930 L 2 1 3 "'.                                    
088100         05 FR-NOVA-TAB-ADBUFFOMR-2 PIC Z(2).                             
088200         05 FILLER               PIC X(1)    VALUE SPACE.                 
088300         05 FR-NOVA-TAB-ADBUFFGANG-2 PIC Z(2).                            
088400         05 FILLER               PIC X(1)    VALUE SPACE.                 
088500         05 FR-NOVA-TAB-ADBUFFPL-2 PIC Z(5).                              
088600         05 FILLER               PIC X(43)   VALUE                        
088700         '"            '.                                                 
088800     03  FILLER.                                                          
088900         05 FILLER               PIC X(26)   VALUE                        
089000         '!F T E  800  930 L 2 1 3 "'.                                    
089100         05 FR-NOVA-TAB-ADBUFFOMR-3 PIC Z(2).                             
089200         05 FILLER               PIC X(1)    VALUE SPACE.                 
089300         05 FR-NOVA-TAB-ADBUFFGANG-3 PIC Z(2).                            
089400         05 FILLER               PIC X(1)    VALUE SPACE.                 
089500         05 FR-NOVA-TAB-ADBUFFPL-3 PIC Z(5).                              
089600         05 FILLER               PIC X(43)   VALUE                        
089700         '"            '.                                                 
089800     03  FILLER.                                                          
089900         05 FILLER               PIC X(26)   VALUE                        
090000         '!F T E  580  900 L 2 1 3 "'.                                    
090100         05 FR-NOVA-TAB-BEART-EMB0 PIC X(25) VALUE SPACE.                 
090200         05 FILLER               PIC X(29)   VALUE                        
090300         '"            '.                                                 
090400     03  FILLER.                                                          
090500         05 FILLER               PIC X(26)   VALUE                        
090600         '!F T E  480  900 L 2 1 3 "'.                                    
090700         05 FR-NOVA-TAB-BEART-EMB1 PIC X(25) VALUE SPACE.                 
090800         05 FILLER               PIC X(29)   VALUE                        
090900         '"            '.                                                 
091000     03  FILLER.                                                          
091100         05 FILLER               PIC X(26)   VALUE                        
091200         '!F T E  380  900 L 2 1 3 "'.                                    
091300         05 FR-NOVA-TAB-BEART-EMB2 PIC X(25) VALUE SPACE.                 
091400         05 FILLER               PIC X(29)   VALUE                        
091500         '"            '.                                                 
091600     03  FILLER.                                                          
091700         05 FILLER               PIC X(26)   VALUE                        
091800         '!F T E 1100 1250 L 2 1 3 "'.                                    
091900         05 FR-NOVA-TAB-KVAVIS-PRIO PIC Z(5)9.                            
092000         05 FILLER               PIC X(48)   VALUE                        
092100         '"            '.                                                 
092200     03  FILLER.                                                          
092300         05 FILLER               PIC X(26)   VALUE                        
092400         '!F T E 1000 1200 L 2 1 3 "'.                                    
092500         05 FR-NOVA-TAB-VLARTNTO PIC Z(7)9.9.                             
092600         05 FILLER               PIC X(44)   VALUE                        
092700         '"            '.                                                 
092800     03  FILLER.                                                          
092900         05 FILLER               PIC X(26)   VALUE                        
093000         '!F T E 1300 1500 L 2 1 3 "'.                                    
093100         05 FR-NOVA-TAB-IDARTNR  PIC Z(8).                                
093200         05 FILLER               PIC X(46)   VALUE                        
093300         '"            '.                                                 
093400     03  FILLER.                                                          
093500         05 FILLER               PIC X(26)   VALUE                        
093600         '!F T E 1100 1650 L 2 1 3 "'.                                    
093700         05 FILLER                   PIC X(1)    VALUE '/'.               
093800         05 FR-NOVA-TAB-ADTRDEST-KIT PIC X(3).                            
093900         05 FILLER               PIC X(50)   VALUE                        
094000         '"            '.                                                 
094100     03  FILLER.                                                          
094200         05 FILLER               PIC X(26)   VALUE                        
094300         '!F T E 1100 1500 L 2 1 3 "'.                                    
094400         05 FR-NOVA-TAB-KVAVIS-KIT PIC Z(5)9.                             
094500         05 FILLER               PIC X(48)   VALUE                        
094600         '"            '.                                                 
094700     03  FILLER.                                                          
094800         05 FILLER               PIC X(26)   VALUE                        
094900         '!F T E 1000 1620 L 2 1 3 "'.                                    
095000         05 FR-NOVA-TAB-BEFT     PIC Z(2).                                
095100         05 FILLER               PIC X(52)   VALUE                        
095200         '"            '.                                                 
095300     03  FILLER.                                                          
095400         05 FILLER               PIC X(26)   VALUE                        
095500         '!F T E  890 1520 L 2 1 3 "'.                                    
095600         05 FR-NOVA-TAB-KDFARLIG-TEXT-L PIC X(10) VALUE SPACE.            
095700         05 FILLER               PIC X(44)   VALUE                        
095800         '"            '.                                                 
095900     03  FILLER.                                                          
096000         05 FILLER               PIC X(26)   VALUE                        
096100         '!F T E  790 1520 L 2 1 3 "'.                                    
096200         05 FR-NOVA-TAB-KVROS    PIC Z(06) VALUE ZERO.                    
096300         05 FILLER               PIC X(48)   VALUE                        
096400         '"            '.                                                 
096500     03  FILLER.                                                          
096600         05 FILLER               PIC X(26)   VALUE                        
096700         '!F T E  580 1490 L 2 1 3 "'.                                    
096800         05 FR-NOVA-TAB-QTYP0    PIC X(1)    VALUE SPACE.                 
096900         05 FILLER               PIC X(53)   VALUE                        
097000         '"            '.                                                 
097100     03  FILLER.                                                          
097200         05 FILLER               PIC X(26)   VALUE                        
097300         '!F T E  480 1490 L 2 1 3 "'.                                    
097400         05 FR-NOVA-TAB-QTYP1    PIC Z(1).                                
097500         05 FILLER               PIC X(53)   VALUE                        
097600         '"            '.                                                 
097700     03  FILLER.                                                          
097800         05 FILLER               PIC X(26)   VALUE                        
097900         '!F T E  380 1490 L 2 1 3 "'.                                    
098000         05 FR-NOVA-TAB-QTYP2    PIC Z(1).                                
098100         05 FILLER               PIC X(53)   VALUE                        
098200         '"            '.                                                 
098300     03  FILLER.                                                          
098400         05 FILLER               PIC X(26)   VALUE                        
098500         '!F T E  250 1600 L 1 1 3 "'.                                    
098600         05 FR-NOVA-TAB-TIAAMMDD PIC 9(6).                                
098700         05 FILLER               PIC X(48)   VALUE                        
098800         '"            '.                                                 
098900     03  FILLER.                                                          
099000         05 FILLER               PIC X(26)   VALUE                        
099100         '!F T E 1000 1850 R 2 1 3 "'.                                    
099200         05 FR-NOVA-TAB-KDSORT   PIC X(2)    VALUE SPACE.                 
099300         05 FILLER               PIC X(52)   VALUE                        
099400         '"            '.                                                 
099500     03  FILLER.                                                          
099600         05 FILLER               PIC X(26)   VALUE                        
099700         '!F T E  580 1830 L 2 1 3 "'.                                    
099800         05 FR-NOVA-TAB-KVQPACK0 PIC Z(5).                                
099900         05 FILLER               PIC X(49)   VALUE                        
100000         '"            '.                                                 
100100     03  FILLER.                                                          
100200         05 FILLER               PIC X(26)   VALUE                        
100300         '!F T E  480 1830 L 2 1 3 "'.                                    
100400         05 FR-NOVA-TAB-KVQPACK1 PIC Z(5).                                
100500         05 FILLER               PIC X(49)   VALUE                        
100600         '"            '.                                                 
100700     03  FILLER.                                                          
100800         05 FILLER               PIC X(26)   VALUE                        
100900         '!F T E  380 1830 L 2 1 3 "'.                                    
101000         05 FR-NOVA-TAB-KVQPACK2 PIC Z(5).                                
101100         05 FILLER               PIC X(49)   VALUE                        
101200         '"            '.                                                 
101300     03  FILLER.                                                          
101400         05 FILLER               PIC X(26)   VALUE                        
101500         '!F T E  250 1750 L 1 1 3 "'.                                    
101600         05 FR-NOVA-TAB-HH       PIC 9(2).                                
101700         05 FILLER               PIC X(1)    VALUE '.'.                   
101800         05 FR-NOVA-TAB-MM       PIC 9(2).                                
101900         05 FILLER               PIC X(1)    VALUE '.'.                   
102000         05 FR-NOVA-TAB-SS       PIC 9(2).                                
102100         05 FILLER               PIC X(46)   VALUE                        
102200         '"            '.                                                 
102300     03  FILLER.                                                          
102400         05 FILLER               PIC X(26)   VALUE                        
102500         '!F T E 1470 1700 L 2 2 3 "'.                                    
102600         05 FR-NOVA-TAB-ADINLOMR PIC X(4).                                
102700         05 FILLER               PIC X(50)   VALUE                        
102800         '"            '.                                                 
102900     03  FILLER.                                                          
103000         05 FILLER               PIC X(26)   VALUE                        
103100         '!F T E 1100 1820 L 2 1 3 "'.                                    
103200         05 FR-NOVA-TAB-KDFORP   PIC Z(5).                                
103300         05 FILLER               PIC X(49)   VALUE                        
103400         '"            '.                                                 
103500     03  FILLER.                                                          
103600         05 FILLER               PIC X(30)   VALUE                        
103700         '!F C E 1060  100 L 130 3 12 "P'.                                
103800         05 FR-NOVA-TAB-IDLOPNRM-STRK PIC 9(8).                           
103900         05 FILLER               PIC X(42)   VALUE                        
104000         '"            '.                                                 
104100     03  FILLER                  PIC X(80)   VALUE                        
104200         '!F B E 1380    1 L 5 1980                            '.         
104300     03  FILLER                  PIC X(80)   VALUE                        
104400         '!F B E 1180  900 L 5 1080                            '.         
104500     03  FILLER                  PIC X(80)   VALUE                        
104600         '!F B E 1080  900 L 5 1080                            '.         
104700     03  FILLER                  PIC X(80)   VALUE                        
104800         '!F B E  980    1 L 5 1980                            '.         
104900     03  FILLER                  PIC X(80)   VALUE                        
105000         '!F B E  870    1 L 5  900                            '.         
105100     03  FILLER                  PIC X(80)   VALUE                        
105200         '!F B E  870 1470 L 5  510                            '.         
105300     03  FILLER                  PIC X(80)   VALUE                        
105400         '!F B E  770    1 L 5 1980                            '.         
105500     03  FILLER                  PIC X(80)   VALUE                        
105600         '!F B E  230    1 L 5 1980                            '.         
105700     03  FILLER                  PIC X(80)   VALUE                        
105800         '!F B E  230 1980 L 1150 5                            '.         
105900     03  FILLER                  PIC X(80)   VALUE                        
106000         '!F B E  870  250 L  110 5                            '.         
106100     03  FILLER                  PIC X(80)   VALUE                        
106200         '!F B E  980 1800 L  200 5                            '.         
106300     03  FILLER                  PIC X(80)   VALUE                        
106400         '!F B E  780 1470 L  600 5                            '.         
106500     03  FILLER                  PIC X(80)   VALUE                        
106600         '!F B E  980 1200 L  200 5                            '.         
106700     03  FILLER                  PIC X(80)   VALUE                        
106800         '!F B E  780  900 L  600 5                            '.         
106900     03  FILLER                  PIC X(80)   VALUE                        
107000         '!F B E  770  500 L  210 5                            '.         
107100     03  FILLER                  PIC X(80)   VALUE                        
107200         '!F B E  230    1 L 1150 5                            '.         
107300     03  FILLER                  PIC X(80)   VALUE                        
107400         '!P                                                   '.         
107500     03  FILLER                  PIC X(80)   VALUE                        
107600         '!R                                                   '.         
107700     SKIP3                                                                
107800 01  FILLER REDEFINES FR-NOVA-TAB.                                        
107900     03 FILLER OCCURS 90.                                                 
108000        05 FR-NOVA-RAD            PIC X(80).                              
108100     EJECT                                                                
108200**** Förbehandlingsrapport via laser                                      
108300**** Förbehandlingsrapport via laser                                      
108400**** Förbehandlingsrapport via laser                                      
108500**** Förbehandlingsrapport via laser                                      
108600**** Förbehandlingsrapport via laser                                      
108700 01      FILLER                  PIC X(16)   VALUE 'FR-LASER-TAB'.        
108800 01      FR-LASER-TAB.                                                    
108900   03    FILLER                  PIC X(80)   VALUE                        
109000         '¤&l0L                                                '.         
109100   03    FILLER                  PIC X(80)   VALUE                        
109200         '¤&f2Y                                                '.         
109300   03    FILLER                  PIC X(80)   VALUE                        
109400         '¤&f4X                                                '.         
109500   03    FILLER                  PIC X(80)   VALUE                        
109600         '&&??%%P                                              '.         
109700   03    FILLER                  PIC X(80)   VALUE                        
109800         '%P                                                   '.         
109900   03    FILLER                  PIC X(80)   VALUE                        
110000         '=211,1,2,3,1,5                                       '.         
110100   03    FILLER.                                                          
110200      05 FILLER                  PIC X(7)    VALUE                        
110300         '=212,"P'.                                                       
110400      05 FR-LASER-IDLOPNRM-STRK  PIC 9(8).                                
110500      05 FILLER                  PIC X(65)   VALUE '"  '.                 
110600   03    FILLER                  PIC X(80)   VALUE                        
110700         '%                                                    '.         
110800   03    FILLER.                                                          
110900      05 FILLER                  PIC X(14)   VALUE                        
111000         '¤*p0005y2125xL'.                                                
111100      05 FR-LASER-ADINLOMR       PIC X(4).                                
111200      05 FILLER                  PIC X(62)   VALUE SPACE.                 
111300   03    FILLER.                                                          
111400      05 FILLER                  PIC X(14)   VALUE                        
111500         '¤*p0221y0005xL'.                                                
111600      05 FR-LASER-IDLOPNRM       PIC Z(8).                                
111700      05 FILLER                  PIC X(58)   VALUE SPACE.                 
111800   03    FILLER.                                                          
111900      05 FILLER                  PIC X(14)   VALUE                        
112000         '¤*p0221y1063xL'.                                                
112100      05 FR-LASER-BEART          PIC X(25).                               
112200      05 FILLER                  PIC X(41)   VALUE SPACE.                 
112300   03    FILLER.                                                          
112400      05 FILLER                  PIC X(14)   VALUE                        
112500         '¤*p0221y1768xL'.                                                
112600      05 FR-LASER-IDARTNR        PIC Z(8).                                
112700      05 FILLER                  PIC X(58)   VALUE SPACE.                 
112800   03    FILLER.                                                          
112900      05 FILLER                  PIC X(14)   VALUE                        
113000         '¤*p0440y1063xL'.                                                
113100      05 FR-LASER-KVAVIS         PIC Z(6).                                
113200      05 FILLER                  PIC X(60)   VALUE SPACE.                 
113300   03    FILLER.                                                          
113400      05 FILLER                  PIC X(14)   VALUE                        
113500         '¤*p0440y1417xL'.                                                
113600      05 FR-LASER-KVAVIS-PRIO    PIC Z(6).                                
113700      05 FILLER                  PIC X(60)   VALUE SPACE.                 
113800   03    FILLER.                                                          
113900      05 FILLER                  PIC X(14)   VALUE                        
114000         '¤*p0440y2125xL'.                                                
114100      05 FR-LASER-KDFORP         PIC Z(5).                                
114200      05 FILLER                  PIC X(61)   VALUE SPACE.                 
114300   03    FILLER.                                                          
114400      05 FILLER                  PIC X(14)   VALUE                        
114500         '¤*p0440y1771xL'.                                                
114600      05 FR-LASER-KVAVIS-KIT     PIC Z(6).                                
114700      05 FILLER                  PIC X(60)   VALUE SPACE.                 
114800   03    FILLER.                                                          
114900      05 FILLER                  PIC X(14)   VALUE                        
115000         '¤*p0558y1063xL'.                                                
115100      05 FR-LASER-VKART          PIC Z(6)9.                               
115200      05 FILLER                  PIC X(59)   VALUE SPACE.                 
115300   03    FILLER.                                                          
115400      05 FILLER                  PIC X(14)   VALUE                        
115500         '¤*p0558y1417xL'.                                                
115600      05 FR-LASER-VLARTNTO       PIC Z(7)9.9.                             
115700      05 FILLER                  PIC X(56)   VALUE SPACE.                 
115800   03    FILLER.                                                          
115900      05 FILLER                  PIC X(14)   VALUE                        
116000         '¤*p0558y1771xL'.                                                
116100      05 FR-LASER-BEFT           PIC Z(2).                                
116200      05 FILLER                  PIC X(64)   VALUE SPACE.                 
116300   03    FILLER.                                                          
116400      05 FILLER                  PIC X(14)   VALUE                        
116500         '¤*p0558y2125xL'.                                                
116600      05 FR-LASER-KDSORT         PIC X(2).                                
116700      05 FILLER                  PIC X(64)   VALUE SPACE.                 
116800   03    FILLER.                                                          
116900      05 FILLER                  PIC X(14)   VALUE                        
117000         '¤*p0666y0005xL'.                                                
117100      05 FR-LASER-KVQPACK-3      PIC Z(6).                                
117200      05 FILLER                  PIC X(60)   VALUE SPACE.                 
117300   03    FILLER.                                                          
117400      05 FILLER                  PIC X(14)   VALUE                        
117500         '¤*p0666y0299xL'.                                                
117600      05 FR-LASER-KDLAGEMB       PIC X(4).                                
117700      05 FILLER                  PIC X(62)   VALUE SPACE.                 
117800   03    FILLER.                                                          
117900      05 FILLER                  PIC X(14)   VALUE                        
118000         '¤*p0666y0593xL'.                                                
118100      05 FR-LASER-KDKVAANT-TEXT  PIC X(3).                                
118200      05 FILLER                  PIC X(63)   VALUE SPACE.                 
118300   03    FILLER.                                                          
118400      05 FILLER                  PIC X(14)   VALUE                        
118500         '¤*p0666y1063xL'.                                                
118600      05 FR-LASER-ADBUFFOMR-1    PIC 9(2).                                
118700      05 FILLER                  PIC X(1)    VALUE SPACE.                 
118800      05 FR-LASER-ADBUFFGANG-1   PIC Z(2).                                
118900      05 FILLER                  PIC X(1)    VALUE SPACE.                 
119000      05 FR-LASER-ADBUFFPL-1     PIC Z(4)9.                               
119100      05 FILLER                  PIC X(55)   VALUE SPACE.                 
119200   03    FILLER.                                                          
119300      05 FILLER                  PIC X(14)   VALUE                        
119400         '¤*p0666y1768xL'.                                                
119500      05 FR-LASER-KDFARLIG-TEXT-L PIC X(10).                              
119600      05 FILLER                  PIC X(56)   VALUE SPACE.                 
119700   03    FILLER.                                                          
119800      05 FILLER                  PIC X(14)   VALUE                        
119900         '¤*p0716y1063xL'.                                                
120000      05 FR-LASER-ADBUFFOMR-2    PIC Z(2).                                
120100      05 FILLER                  PIC X(1)    VALUE SPACE.                 
120200      05 FR-LASER-ADBUFFGANG-2   PIC Z(2).                                
120300      05 FILLER                  PIC X(1)    VALUE SPACE.                 
120400      05 FR-LASER-ADBUFFPL-2     PIC Z(5).                                
120500      05 FILLER                  PIC X(55)   VALUE SPACE.                 
120600   03    FILLER.                                                          
120700      05 FILLER                  PIC X(14)   VALUE                        
120800         '¤*p0776y1063xL'.                                                
120900      05 FR-LASER-ADBUFFOMR-3    PIC Z(2).                                
121000      05 FILLER                  PIC X(1)    VALUE SPACE.                 
121100      05 FR-LASER-ADBUFFGANG-3   PIC Z(2).                                
121200      05 FILLER                  PIC X(1)    VALUE SPACE.                 
121300      05 FR-LASER-ADBUFFPL-3     PIC Z(5).                                
121400      05 FILLER                  PIC X(55)   VALUE SPACE.                 
121500   03    FILLER.                                                          
121600      05 FILLER                  PIC X(14)   VALUE                        
121700         '¤*p0794y0005xL'.                                                
121800      05 FR-LASER-ADLAGOMR       PIC 9(2).                                
121900      05 FILLER                  PIC X(1)    VALUE SPACE.                 
122000      05 FR-LASER-ADGANG         PIC Z(2).                                
122100      05 FILLER                  PIC X(1)    VALUE SPACE.                 
122200      05 FR-LASER-ADPLATS        PIC Z(4)9.                               
122300      05 FILLER                  PIC X(55)   VALUE SPACE.                 
122400   03    FILLER.                                                          
122500      05 FILLER                  PIC X(14)   VALUE                        
122600         '¤*p0794y0593xL'.                                                
122700      05 FR-LASER-BEARTURS       PIC X(15).                               
122800      05 FILLER                  PIC X(51)   VALUE SPACE.                 
122900   03    FILLER.                                                          
123000      05 FILLER                  PIC X(14)   VALUE                        
123100         '¤*p0794y1768xL'.                                                
123200      05 FR-LASER-KVROS          PIC Z(06)   VALUE ZERO.                  
123300      05 FILLER                  PIC X(60)   VALUE SPACE.                 
123400   03    FILLER.                                                          
123500      05 FILLER                  PIC X(14)   VALUE                        
123600         '¤*p1030y0005xL'.                                                
123700      05 FR-LASER-ADLAGOMR-EMB0  PIC Z(2).                                
123800      05 FILLER                  PIC X(1)    VALUE SPACE.                 
123900      05 FR-LASER-ADGANG-EMB0    PIC Z(2).                                
124000      05 FILLER                  PIC X(1)    VALUE SPACE.                 
124100      05 FR-LASER-ADPLATS-EMB0   PIC Z(5).                                
124200      05 FILLER                  PIC X(55)   VALUE SPACE.                 
124300   03    FILLER.                                                          
124400      05 FILLER                  PIC X(14)   VALUE                        
124500         '¤*p1030y0593xL'.                                                
124600      05 FR-LASER-IDARTNR-EMBQ0  PIC Z(8).                                
124700      05 FILLER                  PIC X(58)   VALUE SPACE.                 
124800   03    FILLER.                                                          
124900      05 FILLER                  PIC X(14)   VALUE                        
125000         '¤*p1030y1063xL'.                                                
125100      05 FR-LASER-BEART-EMB0     PIC X(25).                               
125200      05 FILLER                  PIC X(41)   VALUE SPACE.                 
125300   03    FILLER.                                                          
125400      05 FILLER                  PIC X(14)   VALUE                        
125500         '¤*p1030y1768xL'.                                                
125600      05 FR-LASER-QTYP0          PIC X(1).                                
125700      05 FILLER                  PIC X(65)   VALUE SPACE.                 
125800   03    FILLER.                                                          
125900      05 FILLER                  PIC X(14)   VALUE                        
126000         '¤*p1030y2125xL'.                                                
126100      05 FR-LASER-KVQPACK0       PIC Z(6).                                
126200      05 FILLER                  PIC X(60)   VALUE SPACE.                 
126300   03    FILLER.                                                          
126400      05 FILLER                  PIC X(14)   VALUE                        
126500         '¤*p1150y0005xL'.                                                
126600      05 FR-LASER-ADLAGOMR-EMB1  PIC Z(2).                                
126700      05 FILLER                  PIC X(1)    VALUE SPACE.                 
126800      05 FR-LASER-ADGANG-EMB1    PIC Z(2).                                
126900      05 FILLER                  PIC X(1)    VALUE SPACE.                 
127000      05 FR-LASER-ADPLATS-EMB1   PIC Z(5).                                
127100      05 FILLER                  PIC X(55)   VALUE SPACE.                 
127200   03    FILLER.                                                          
127300      05 FILLER                  PIC X(14)   VALUE                        
127400         '¤*p1150y0593xL'.                                                
127500      05 FR-LASER-IDARTNR-EMBQ1  PIC Z(8).                                
127600      05 FILLER                  PIC X(58)   VALUE SPACE.                 
127700   03    FILLER.                                                          
127800      05 FILLER                  PIC X(14)   VALUE                        
127900         '¤*p1150y1063xL'.                                                
128000      05 FR-LASER-BEART-EMB1     PIC X(25).                               
128100      05 FILLER                  PIC X(41)   VALUE SPACE.                 
128200   03    FILLER.                                                          
128300      05 FILLER                  PIC X(14)   VALUE                        
128400         '¤*p1150y1768xL'.                                                
128500      05 FR-LASER-QTYP1          PIC Z(1).                                
128600      05 FILLER                  PIC X(65)   VALUE SPACE.                 
128700   03    FILLER.                                                          
128800      05 FILLER                  PIC X(14)   VALUE                        
128900         '¤*p1150y2125xL'.                                                
129000      05 FR-LASER-KVQPACK1       PIC Z(6).                                
129100      05 FILLER                  PIC X(60)   VALUE SPACE.                 
129200   03    FILLER.                                                          
129300      05 FILLER                  PIC X(14)   VALUE                        
129400         '¤*p1270y0005xL'.                                                
129500      05 FR-LASER-ADLAGOMR-EMB2  PIC Z(2).                                
129600      05 FILLER                  PIC X(1)    VALUE SPACE.                 
129700      05 FR-LASER-ADGANG-EMB2    PIC Z(2).                                
129800      05 FILLER                  PIC X(1)    VALUE SPACE.                 
129900      05 FR-LASER-ADPLATS-EMB2   PIC Z(5).                                
130000      05 FILLER                  PIC X(55)   VALUE SPACE.                 
130100   03    FILLER.                                                          
130200      05 FILLER                  PIC X(14)   VALUE                        
130300         '¤*p1270y0593xL'.                                                
130400      05 FR-LASER-IDARTNR-EMBQ2  PIC Z(8).                                
130500      05 FILLER                  PIC X(58)   VALUE SPACE.                 
130600   03    FILLER.                                                          
130700      05 FILLER                  PIC X(14)   VALUE                        
130800         '¤*p1270y1063xL'.                                                
130900      05 FR-LASER-BEART-EMB2     PIC X(25).                               
131000      05 FILLER                  PIC X(41)   VALUE SPACE.                 
131100   03    FILLER.                                                          
131200      05 FILLER                  PIC X(14)   VALUE                        
131300         '¤*p1270y1768xL'.                                                
131400      05 FR-LASER-QTYP2          PIC Z(1).                                
131500      05 FILLER                  PIC X(65)   VALUE SPACE.                 
131600   03    FILLER.                                                          
131700      05 FILLER                  PIC X(14)   VALUE                        
131800         '¤*p1270y2125xL'.                                                
131900      05 FR-LASER-KVQPACK2       PIC Z(6).                                
132000      05 FILLER                  PIC X(60)   VALUE SPACE.                 
132100   03    FILLER.                                                          
132200      05 FILLER                  PIC X(14)   VALUE                        
132300         '¤*p1410y1651xL'.                                                
132400      05 FR-LASER-TIAAMMDD       PIC 9(6).                                
132500      05 FILLER                  PIC X(60)  VALUE SPACE.                  
132600   03    FILLER.                                                          
132700      05 FILLER                  PIC X(14)   VALUE                        
132800         '¤*p1410y1969xL'.                                                
132900      05 FR-LASER-HH             PIC 9(2).                                
133000      05 FILLER                  PIC X(1)  VALUE '.'.                     
133100      05 FR-LASER-MM             PIC 9(2).                                
133200      05 FILLER                  PIC X(1)  VALUE '.'.                     
133300      05 FR-LASER-SS             PIC 9(2).                                
133400      05 FILLER                  PIC X(58) VALUE SPACE.                   
133500   03    FILLER.                                                          
133600      05 FILLER                  PIC X(80)   VALUE                        
133700         '¤Z                                          '.                  
133800   03    FILLER.                                                          
133900      05 FILLER                  PIC X(80)   VALUE                        
134000         '¤E                                          '.                  
134100 01  FILLER REDEFINES FR-LASER-TAB.                                       
134200   03  FILLER OCCURS  49.                                                 
134300     05  FR-LASER-RAD               PIC X(80).                            
134400     EJECT                                                                
134500**** Förbehandlingsrapport via laser i Born                               
134600 01      FILLER                  PIC X(16)   VALUE 'PR-LASER-TAB'.        
134700 01      PR-LASER-TAB.                                                    
134800   03    FILLER                  PIC X(80)   VALUE                        
134900         '^&l0L                                                '.         
135000   03    FILLER                  PIC X(80)   VALUE                        
135100         '¤R¤ CASS 2; SPO L; EXIT;                             '.         
135200   03    FILLER                  PIC X(80)   VALUE                        
135300         '^&f2Y                                                '.         
135400   03    FILLER                  PIC X(80)   VALUE                        
135500         '^&f4X                                                '.         
135600   03    FILLER.                                                          
135700      05 FILLER                  PIC X(20)   VALUE                        
135800         '^*p0271y0100xl      '.                                          
135900      05 FILLER                  PIC X(11)   VALUE '¤R¤ BARC 19'.         
136000      05 FILLER                  PIC X(03)   VALUE ',Y,'.                 
136100      05 FILLER                  PIC X(01)   VALUE QUOTE.                 
136200      05 FILLER                  PIC X(01)   VALUE 'P'.                   
136300      05 PR-LASER-IDLOPNRM-STRK  PIC 9(8).                                
136400      05 FILLER                  PIC X(01)   VALUE QUOTE.                 
136500      05 FILLER                  PIC X(35)   VALUE ';EXIT;'.              
136600   03    FILLER.                                                          
136700      05 FILLER                  PIC X(14)   VALUE                        
136800         '^*p0005y2125xL'.                                                
136900      05 PR-LASER-ADINLOMR       PIC X(4).                                
137000      05 FILLER                  PIC X(62)   VALUE SPACE.                 
137100   03    FILLER.                                                          
137200      05 FILLER                  PIC X(14)   VALUE                        
137300         '^*p0221y1063xL'.                                                
137400      05 PR-LASER-BEART          PIC X(25).                               
137500      05 FILLER                  PIC X(41)   VALUE SPACE.                 
137600   03    FILLER.                                                          
137700      05 FILLER                  PIC X(14)   VALUE                        
137800         '^*p0221y1768xL'.                                                
137900      05 PR-LASER-IDARTNR        PIC Z(8).                                
138000      05 FILLER                  PIC X(58)   VALUE SPACE.                 
138100   03    FILLER.                                                          
138200      05 FILLER                  PIC X(14)   VALUE                        
138300         '^*p0440y1063xL'.                                                
138400      05 PR-LASER-KVAVIS         PIC Z(6).                                
138500      05 FILLER                  PIC X(60)   VALUE SPACE.                 
138600   03    FILLER.                                                          
138700      05 FILLER                  PIC X(14)   VALUE                        
138800         '^*p0440y1417xL'.                                                
138900      05 PR-LASER-KVAVIS-PRIO    PIC Z(6).                                
139000      05 FILLER                  PIC X(60)   VALUE SPACE.                 
139100   03    FILLER.                                                          
139200      05 FILLER                  PIC X(14)   VALUE                        
139300         '^*p0440y2125xL'.                                                
139400      05 PR-LASER-KDFORP         PIC Z(5).                                
139500      05 FILLER                  PIC X(61)   VALUE SPACE.                 
139600   03    FILLER.                                                          
139700      05 FILLER                  PIC X(14)   VALUE                        
139800         '^*p0440y1771xL'.                                                
139900      05 PR-LASER-KVAVIS-KIT     PIC Z(6).                                
140000      05 FILLER                  PIC X(60)   VALUE SPACE.                 
140100   03    FILLER.                                                          
140200      05 FILLER                  PIC X(14)   VALUE                        
140300         '^*p0558y1063xL'.                                                
140400      05 PR-LASER-VKART          PIC Z(6)9.                               
140500      05 FILLER                  PIC X(59)   VALUE SPACE.                 
140600   03    FILLER.                                                          
140700      05 FILLER                  PIC X(14)   VALUE                        
140800         '^*p0558y1417xL'.                                                
140900      05 PR-LASER-VLARTNTO       PIC Z(7)9.9.                             
141000      05 FILLER                  PIC X(56)   VALUE SPACE.                 
141100   03    FILLER.                                                          
141200      05 FILLER                  PIC X(14)   VALUE                        
141300         '^*p0558y1771xL'.                                                
141400      05 PR-LASER-BEFT           PIC Z(2).                                
141500      05 FILLER                  PIC X(64)   VALUE SPACE.                 
141600   03    FILLER.                                                          
141700      05 FILLER                  PIC X(14)   VALUE                        
141800         '^*p0558y2125xL'.                                                
141900      05 PR-LASER-KDSORT         PIC X(2).                                
142000      05 FILLER                  PIC X(64)   VALUE SPACE.                 
142100   03    FILLER.                                                          
142200      05 FILLER                  PIC X(14)   VALUE                        
142300         '^*p0666y0005xL'.                                                
142400      05 PR-LASER-KVQPACK-3      PIC Z(6).                                
142500      05 FILLER                  PIC X(60)   VALUE SPACE.                 
142600   03    FILLER.                                                          
142700      05 FILLER                  PIC X(14)   VALUE                        
142800         '^*p0666y0299xL'.                                                
142900      05 PR-LASER-KDLAGEMB       PIC X(4).                                
143000      05 FILLER                  PIC X(62)   VALUE SPACE.                 
143100   03    FILLER.                                                          
143200      05 FILLER                  PIC X(14)   VALUE                        
143300         '^*p0666y0593xL'.                                                
143400      05 PR-LASER-KDKVAANT-TEXT  PIC X(3).                                
143500      05 FILLER                  PIC X(63)   VALUE SPACE.                 
143600   03    FILLER.                                                          
143700      05 FILLER                  PIC X(14)   VALUE                        
143800         '^*p0666y1063xL'.                                                
143900      05 PR-LASER-ADBUFFOMR-1    PIC 9(2).                                
144000      05 FILLER                  PIC X(1)    VALUE SPACE.                 
144100      05 PR-LASER-ADBUFFGANG-1   PIC Z(2).                                
144200      05 FILLER                  PIC X(1)    VALUE SPACE.                 
144300      05 PR-LASER-ADBUFFPL-1     PIC Z(4)9.                               
144400      05 FILLER                  PIC X(55)   VALUE SPACE.                 
144500   03    FILLER.                                                          
144600      05 FILLER                  PIC X(14)   VALUE                        
144700         '^*p0666y1768xL'.                                                
144800      05 PR-LASER-KDFARLIG-TEXT-L PIC X(10).                              
144900      05 FILLER                  PIC X(56)   VALUE SPACE.                 
145000   03    FILLER.                                                          
145100      05 FILLER                  PIC X(14)   VALUE                        
145200         '^*p0716y1063xL'.                                                
145300      05 PR-LASER-ADBUFFOMR-2    PIC Z(2).                                
145400      05 FILLER                  PIC X(1)    VALUE SPACE.                 
145500      05 PR-LASER-ADBUFFGANG-2   PIC Z(2).                                
145600      05 FILLER                  PIC X(1)    VALUE SPACE.                 
145700      05 PR-LASER-ADBUFFPL-2     PIC Z(5).                                
145800      05 FILLER                  PIC X(55)   VALUE SPACE.                 
145900   03    FILLER.                                                          
146000      05 FILLER                  PIC X(14)   VALUE                        
146100         '^*p0776y1063xL'.                                                
146200      05 PR-LASER-ADBUFFOMR-3    PIC Z(2).                                
146300      05 FILLER                  PIC X(1)    VALUE SPACE.                 
146400      05 PR-LASER-ADBUFFGANG-3   PIC Z(2).                                
146500      05 FILLER                  PIC X(1)    VALUE SPACE.                 
146600      05 PR-LASER-ADBUFFPL-3     PIC Z(5).                                
146700      05 FILLER                  PIC X(55)   VALUE SPACE.                 
146800   03    FILLER.                                                          
146900      05 FILLER                  PIC X(14)   VALUE                        
147000         '^*p0794y0005xL'.                                                
147100      05 PR-LASER-ADLAGOMR       PIC 9(2).                                
147200      05 FILLER                  PIC X(1)    VALUE SPACE.                 
147300      05 PR-LASER-ADGANG         PIC Z(2).                                
147400      05 FILLER                  PIC X(1)    VALUE SPACE.                 
147500      05 PR-LASER-ADPLATS        PIC Z(4)9.                               
147600      05 FILLER                  PIC X(55)   VALUE SPACE.                 
147700   03    FILLER.                                                          
147800      05 FILLER                  PIC X(14)   VALUE                        
147900         '^*p0794y0593xL'.                                                
148000      05 PR-LASER-BEARTURS       PIC X(15).                               
148100      05 FILLER                  PIC X(51)   VALUE SPACE.                 
148200   03    FILLER.                                                          
148300      05 FILLER                  PIC X(14)   VALUE                        
148400         '^*p0794y1768xL'.                                                
148500      05 PR-LASER-KVROS          PIC Z(06)   VALUE ZERO.                  
148600      05 FILLER                  PIC X(60)   VALUE SPACE.                 
148700   03    FILLER.                                                          
148800      05 FILLER                  PIC X(14)   VALUE                        
148900         '^*p1030y0005xL'.                                                
149000      05 PR-LASER-ADLAGOMR-EMB0  PIC Z(2).                                
149100      05 FILLER                  PIC X(1)    VALUE SPACE.                 
149200      05 PR-LASER-ADGANG-EMB0    PIC Z(2).                                
149300      05 FILLER                  PIC X(1)    VALUE SPACE.                 
149400      05 PR-LASER-ADPLATS-EMB0   PIC Z(5).                                
149500      05 FILLER                  PIC X(55)   VALUE SPACE.                 
149600   03    FILLER.                                                          
149700      05 FILLER                  PIC X(14)   VALUE                        
149800         '^*p1030y0593xL'.                                                
149900      05 PR-LASER-IDARTNR-EMBQ0  PIC Z(8).                                
150000      05 FILLER                  PIC X(58)   VALUE SPACE.                 
150100   03    FILLER.                                                          
150200      05 FILLER                  PIC X(14)   VALUE                        
150300         '^*p1030y1063xL'.                                                
150400      05 PR-LASER-BEART-EMB0     PIC X(25).                               
150500      05 FILLER                  PIC X(41)   VALUE SPACE.                 
150600   03    FILLER.                                                          
150700      05 FILLER                  PIC X(14)   VALUE                        
150800         '^*p1030y1768xL'.                                                
150900      05 PR-LASER-QTYP0          PIC X(1).                                
151000      05 FILLER                  PIC X(65)   VALUE SPACE.                 
151100   03    FILLER.                                                          
151200      05 FILLER                  PIC X(14)   VALUE                        
151300         '^*p1030y2125xL'.                                                
151400      05 PR-LASER-KVQPACK0       PIC Z(6).                                
151500      05 FILLER                  PIC X(60)   VALUE SPACE.                 
151600   03    FILLER.                                                          
151700      05 FILLER                  PIC X(14)   VALUE                        
151800         '^*p1150y0005xL'.                                                
151900      05 PR-LASER-ADLAGOMR-EMB1  PIC Z(2).                                
152000      05 FILLER                  PIC X(1)    VALUE SPACE.                 
152100      05 PR-LASER-ADGANG-EMB1    PIC Z(2).                                
152200      05 FILLER                  PIC X(1)    VALUE SPACE.                 
152300      05 PR-LASER-ADPLATS-EMB1   PIC Z(5).                                
152400      05 FILLER                  PIC X(55)   VALUE SPACE.                 
152500   03    FILLER.                                                          
152600      05 FILLER                  PIC X(14)   VALUE                        
152700         '^*p1150y0593xL'.                                                
152800      05 PR-LASER-IDARTNR-EMBQ1  PIC Z(8).                                
152900      05 FILLER                  PIC X(58)   VALUE SPACE.                 
153000   03    FILLER.                                                          
153100      05 FILLER                  PIC X(14)   VALUE                        
153200         '^*p1150y1063xL'.                                                
153300      05 PR-LASER-BEART-EMB1     PIC X(25).                               
153400      05 FILLER                  PIC X(41)   VALUE SPACE.                 
153500   03    FILLER.                                                          
153600      05 FILLER                  PIC X(14)   VALUE                        
153700         '^*p1150y1768xL'.                                                
153800      05 PR-LASER-QTYP1          PIC Z(1).                                
153900      05 FILLER                  PIC X(65)   VALUE SPACE.                 
154000   03    FILLER.                                                          
154100      05 FILLER                  PIC X(14)   VALUE                        
154200         '^*p1150y2125xL'.                                                
154300      05 PR-LASER-KVQPACK1       PIC Z(6).                                
154400      05 FILLER                  PIC X(60)   VALUE SPACE.                 
154500   03    FILLER.                                                          
154600      05 FILLER                  PIC X(14)   VALUE                        
154700         '^*p1270y0005xL'.                                                
154800      05 PR-LASER-ADLAGOMR-EMB2  PIC Z(2).                                
154900      05 FILLER                  PIC X(1)    VALUE SPACE.                 
155000      05 PR-LASER-ADGANG-EMB2    PIC Z(2).                                
155100      05 FILLER                  PIC X(1)    VALUE SPACE.                 
155200      05 PR-LASER-ADPLATS-EMB2   PIC Z(5).                                
155300      05 FILLER                  PIC X(55)   VALUE SPACE.                 
155400   03    FILLER.                                                          
155500      05 FILLER                  PIC X(14)   VALUE                        
155600         '^*p1270y0593xL'.                                                
155700      05 PR-LASER-IDARTNR-EMBQ2  PIC Z(8).                                
155800      05 FILLER                  PIC X(58)   VALUE SPACE.                 
155900   03    FILLER.                                                          
156000      05 FILLER                  PIC X(14)   VALUE                        
156100         '^*p1270y1063xL'.                                                
156200      05 PR-LASER-BEART-EMB2     PIC X(25).                               
156300      05 FILLER                  PIC X(41)   VALUE SPACE.                 
156400   03    FILLER.                                                          
156500      05 FILLER                  PIC X(14)   VALUE                        
156600         '^*p1270y1768xL'.                                                
156700      05 PR-LASER-QTYP2          PIC Z(1).                                
156800      05 FILLER                  PIC X(65)   VALUE SPACE.                 
156900   03    FILLER.                                                          
157000      05 FILLER                  PIC X(14)   VALUE                        
157100         '^*p1270y2125xL'.                                                
157200      05 PR-LASER-KVQPACK2       PIC Z(6).                                
157300      05 FILLER                  PIC X(60)   VALUE SPACE.                 
157400   03    FILLER.                                                          
157500      05 FILLER                  PIC X(14)   VALUE                        
157600         '^*p1410y1651xL'.                                                
157700      05 PR-LASER-TIAAMMDD       PIC 9(6).                                
157800      05 FILLER                  PIC X(60)  VALUE SPACE.                  
157900   03    FILLER.                                                          
158000      05 FILLER                  PIC X(14)   VALUE                        
158100         '^*p1410y1969xL'.                                                
158200      05 PR-LASER-HH             PIC 9(2).                                
158300      05 FILLER                  PIC X(1)  VALUE '.'.                     
158400      05 PR-LASER-MM             PIC 9(2).                                
158500      05 FILLER                  PIC X(1)  VALUE '.'.                     
158600      05 PR-LASER-SS             PIC 9(2).                                
158700      05 FILLER                  PIC X(58) VALUE SPACE.                   
158800   03    FILLER.                                                          
158900      05 FILLER                  PIC X(80)   VALUE                        
159000         '^Z                                          '.                  
159100   03    FILLER.                                                          
159200      05 FILLER                  PIC X(80)   VALUE                        
159300         '^E                                          '.                  
159400   03    FILLER.                                                          
159500      05 FILLER                  PIC X(80)   VALUE                        
159600         '¤R¤ CASS 1; SPO P; EXIT, E;                 '.                  
159700 01  FILLER REDEFINES PR-LASER-TAB.                                       
159800   03  FILLER OCCURS  46.                                                 
159900     05  PR-LASER-RAD               PIC X(80).                            
160000     EJECT                                                                
160100*      --- VALID IDDC CODES                                               
160200*                                                                         
160300*01    -COPY WWDC99                                                       
160400       EJECT                                                              
160500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
160600 01  GENERELLA-SUBPROGRAM.                                                
160700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
160800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
160900     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
161000     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
161100     03  W006PRC1                PIC X(8)    VALUE 'W006PRC1'.            
161200     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
161300     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
161400     EJECT                                                                
161500*    --- AREA FÖR W006PRAR                                                
161600*                                                                         
161700 01  FILLER                      PIC X(8)    VALUE 'W006PRAR'.            
161800                                                                          
161900*01  -COPY W006PRAR                                                       
162000     EJECT                                                                
162100 01  FILLER                      PIC X(8)    VALUE 'W006PRVC'.            
162200                                                                          
162300*01  -COPY W006PRVC                                                       
162400     EJECT                                                                
162500 01  FILLER                      PIC X(8)    VALUE 'W006PRT '.            
162600                                                                          
162700*01  -COPY W006PRT                                                        
162800     EJECT                                                                
162900*    --- PARAMETRAR TILL SUBPROGRAM W400ARTU                              
163000 01  FILLER                      PIC X(8)    VALUE 'W400ARTU'.            
163100                                                                          
163200*01 -COPY W400ARTU                                                        
163300     EJECT                                                                
163400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
163500*                                                                         
163600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
163700     SKIP3                                                                
163800*01  MID -COPY W6I19701                                                   
163900     EJECT                                                                
164000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
164100     SKIP3                                                                
164200*01  -COPY WMSGAREA                                                       
164300     EJECT                                                                
164400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
164500     SKIP3                                                                
164600*01  -COPY WMFSAREA                                                       
164700     EJECT                                                                
164800 01  FILLER                      PIC X(16)  VALUE 'W611STYR-AREA'.        
164900*01  -COPY W611STYR                                                       
165000     EJECT                                                                
165100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
165200*                                                                         
165300     SKIP2                                                                
165400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
165500     SKIP2                                                                
165600 01  NYCKLAR-TILL-DLI.                                                    
165700     SKIP2                                                                
165800*    -- W6INLA/W6D1                                                       
165900     03  W-W6D101KY-X.                                                    
166000         05  W-W6D101KY-IDDC     PIC  X(2)   VALUE SPACE.                 
166100         05  W-W6D101KY-IDLEVNR  PIC  X(5)   VALUE SPACE.                 
166200         05  W-W6D101KY-IDFS     PIC  X(8)   VALUE SPACE.                 
166300         05  W-W6D101KY-TIAVIDAT PIC S9(7)   VALUE ZERO COMP-3.           
166400                                                                          
166500     03  W-IDRADNR-INL-X.                                                 
166600         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
166700                                                                          
166800*    --  WLARTC/WDK6, WLARTD/WDD8, W6INLA/W6D1                            
166900     03  W-IDARTNR-X.                                                     
167000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
167100                                                                          
167200     03  W-IDDC-X.                                                        
167300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
167400                                                                          
167500     03  W-IDDC-91-X.                                                     
167600         05  W-IDDC-91           PIC X(2)    VALUE '91'.                  
167700                                                                          
167800     03  W-KDSEGKEY-X.                                                    
167900         05  W-KDSEGKEY          PIC  X(1)   VALUE '1'.                   
168000                                                                          
168010     03  W-IDLAND.                                                        
168020         05  W-IDLAND-US-CN      PIC  X(2)   VALUE '  '.                  
168030                                                                          
168100*    -- WLBENA/WDD3                                                       
168200     03  W-WDD3BSEQ-X.                                                    
168300         05  W-WDD3BSEQ-IDARTNR  PIC S9(9)   VALUE ZERO COMP-3.           
168400                                                                          
168500     03  W-IDSKYLT-X.                                                     
168600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
168700     SKIP2                                                                
168800*    --- STATUS-KOD FRÅN IMS                                              
168900 01  STATUS-WS                   PIC XX.                                  
169000     88  SEGMENT-FINNS                       VALUE '  '.                  
169100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
169200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
169300     SKIP2                                                                
169400 01  GODK-STATUSKODER.                                                    
169500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
169600     SKIP3                                                                
169700 01  SSA1                        PIC X(64).                               
169800 01  SSA2                        PIC X(64).                               
169900 01  SSA3                        PIC X(64).                               
170000     EJECT                                                                
170100*    --- IMS FUNKTIONSKODER                                               
170200*01  -COPY W0003                                                          
170300     EJECT                                                                
170400*    ---  DLI INPUT-OUTPUT AREA                                           
170500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
170600     SKIP3                                                                
170700 01  DLI-IO-AREA.                                                         
170800     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
170900     SKIP3                                                                
171000     03  WLARTC01 REDEFINES IO-AREA.                                      
171100*        05  -COPY WDK601  -PRE ARTC01-                                   
171200     EJECT                                                                
171300     03  WLARTC11 REDEFINES IO-AREA.                                      
171400*        05  -COPY WDK611  -PRE ARTC11-                                   
171500     EJECT                                                                
171600     03  WLARTS11 REDEFINES IO-AREA.                                      
171700*        05  -COPY WDK711  -PRE ARTS11-                                   
171800     EJECT                                                                
171900     03  WLARTD01 REDEFINES IO-AREA.                                      
172000*        05  -COPY WDD801  -PRE ARTD-                                     
172100     EJECT                                                                
172200     03  WLARTD11 REDEFINES IO-AREA.                                      
172300*        05  -COPY WDD811  -PRE ARTD-                                     
172400     EJECT                                                                
172500     03  WLBENA11 REDEFINES IO-AREA.                                      
172600*        05  -COPY WDD311  -PRE BENA-                                     
172700     EJECT                                                                
172800*    ---  DLI INPUT-OUTPUT AREA 2                                         
172900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
173000     SKIP3                                                                
173100 01  DLI-IO-AREA2.                                                        
173200     03  IO-AREA2                PIC X(200)  VALUE SPACE.                 
173300     SKIP3                                                                
173400     03  W6INLA11 REDEFINES IO-AREA2.                                     
173500*        05  -COPY W6D111  -PRE INLA-                                     
173600     EJECT                                                                
173601 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK712'.        
173602     SKIP3                                                                
173610 01  DLI-IO-WDK712.                                                       
173650*    03  -COPY WDK712                                                     
173660     EJECT                                                                
173700 LINKAGE SECTION.                                                         
173800*01  -COPY W0009   -PRE MSG-                                              
173900                                                                          
174000*01  -COPY W0009   -PRE ALT-                                              
174100     EJECT                                                                
174200*01  -COPY W0008  -PRE INLA-                                              
174300     05  FILLER                  PIC X.                                   
174400                                                                          
174500*01  -COPY W0008  -PRE ARTC-                                              
174600     05  FILLER                  PIC X.                                   
174700     EJECT                                                                
174800*01  -COPY W0008  -PRE ARTS-                                              
174900     05  FILLER                  PIC X.                                   
175000                                                                          
175100*01  -COPY W0008  -PRE ARTD-                                              
175200     05  FILLER                  PIC X.                                   
175300     EJECT                                                                
175400*01  -COPY W0008  -PRE BENA-                                              
175500     05  FILLER                  PIC X.                                   
175600                                                                          
175700*01  -COPY W0008  -PRE STYR-HANB-                                         
175800     05  FILLER                  PIC X.                                   
175900     EJECT                                                                
176000*01  -COPY W0008  -PRE STYR-PLAA-                                         
176100     05  FILLER                  PIC X.                                   
176200     EJECT                                                                
176210*01  -COPY W0008  -PRE WDK7-                                              
176220     05  FILLER                  PIC X.                                   
176230     EJECT                                                                
176300 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
176400                           INLA-PCB ARTC-PCB ARTS-PCB                     
176500                           ARTD-PCB BENA-PCB                              
176600                           STYR-HANB-PCB STYR-PLAA-PCB                    
176610                           WDK7-PCB.                                      
176700 MAIN SECTION.                                                            
176800     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
176900                           INLA-PCB ARTC-PCB ARTS-PCB                     
177000                           ARTD-PCB BENA-PCB                              
177100                           STYR-HANB-PCB STYR-PLAA-PCB                    
177110                           WDK7-PCB.                                      
177200                                                                          
177300     PERFORM IMS-GET-MSG                                                  
177400     IF SEGMENT-FINNS                                                     
177500       PERFORM A-INIT                                                     
177600       IF EGEN-MID OR GODK-MID                                            
177700                                                                          
177800         MOVE MID-IDPRTLST         TO WS-IDPRTLST                         
177900                                                                          
178000         PERFORM S04-PRT-KONTROLL                                         
178100                                                                          
178200         PERFORM S01-PRT-OPEN                                             
178300                                                                          
178400         MOVE MID-IDDC             TO W-W6D101KY-IDDC                     
178500                                      W-IDDC                              
178600                                      WS-IDDC                             
178700                                                                          
178800         MOVE +1                   TO IX-POST                             
178900                                                                          
179000         PERFORM UNTIL (IX-POST > K-MAX-KVPOST                            
179100                    OR  IX-POST > MID-KVPOST)                             
179200                                                                          
179300           PERFORM B-RED-SIDA                                             
179400           IF LASER                                                       
179500             IF CDC-SE                                                    
179600               MOVE +1 TO LASER-IX                                        
179700               MOVE PRT-NYSIDA-RAD1     TO PRT-RADSKIP                    
179800               PERFORM UNTIL LASER-IX > FR-LASER-MAX                      
179900                 MOVE FR-LASER-RAD(LASER-IX) TO FR-LIST-RAD               
180000                 PERFORM S02-PRT-WRITE                                    
180100                 MOVE PRT-AFTER-1   TO PRT-RADSKIP                        
180200                 ADD +1 TO LASER-IX                                       
180300               END-PERFORM                                                
180400             ELSE                                                         
180500               MOVE +1 TO LASER-IX                                        
180600               MOVE PRT-NYSIDA-RAD1     TO PRT-RADSKIP                    
180700               PERFORM UNTIL LASER-IX > PR-LASER-MAX                      
180800                 MOVE PR-LASER-RAD(LASER-IX) TO FR-LIST-RAD               
180900                 PERFORM S02-PRT-WRITE                                    
181000                 MOVE PRT-AFTER-1   TO PRT-RADSKIP                        
181100                 ADD +1 TO LASER-IX                                       
181200               END-PERFORM                                                
181300             END-IF                                                       
181400           ELSE                                                           
181500             MOVE +1 TO TERMO-IX                                          
181600             MOVE PRT-AFTER-1         TO PRT-RADSKIP                      
181700             IF PRT-BEPRTLST(1:4) = 'VCOM'                                
181710               CONTINUE                                                   
181800*              PERFORM UNTIL TERMO-IX > FR-TAB-MAX-VCOM                   
181900*                MOVE FR-RAD-VCOM(TERMO-IX) TO FR-LIST-RAD                
182000*                PERFORM S02-PRT-WRITE                                    
182100*                ADD +1 TO TERMO-IX                                       
182200*              END-PERFORM                                                
182300             ELSE                                                         
182400               IF PRT-BEPRTLST(1:4) = 'NOVA'                              
182500                 PERFORM UNTIL TERMO-IX > FR-TAB-MAX-NOVA                 
182600                   MOVE FR-NOVA-RAD(TERMO-IX) TO FR-LIST-RAD              
182700                   PERFORM S02-PRT-WRITE                                  
182800                   ADD +1 TO TERMO-IX                                     
182900                 END-PERFORM                                              
183000               ELSE                                                       
183100                 PERFORM UNTIL TERMO-IX > FR-TAB-MAX                      
183200                   MOVE FR-RAD(TERMO-IX) TO FR-LIST-RAD                   
183300                   PERFORM S02-PRT-WRITE                                  
183400                   ADD +1 TO TERMO-IX                                     
183500                 END-PERFORM                                              
183600               END-IF                                                     
183700             END-IF                                                       
183800           END-IF                                                         
183900           ADD +1                  TO IX-POST                             
184000         END-PERFORM                                                      
184100                                                                          
184200         PERFORM S03-PRT-CLOSE                                            
184300       END-IF                                                             
184400     END-IF                                                               
184500                                                                          
184600     MOVE ZERO TO RETURN-CODE                                             
184700     GOBACK                                                               
184800     .                                                                    
184900     EJECT                                                                
185000 A-INIT SECTION.                                                          
185100                                                                          
185200     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I19701                    
185300     MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                               
185400     MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                              
185500                                                                          
185600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
185700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
185800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
185900                                                                          
186000     MOVE LOW-VALUE TO MSG-AREA                                           
186100                                                                          
186200     MOVE MID-IDDC       TO WS-IDDC                                       
186300     IF CDC-SE                                                            
186400       MOVE 'S  ' TO W-IDSKYLT                                            
186500     ELSE                                                                 
186600       MOVE 'GB ' TO W-IDSKYLT                                            
186700     END-IF                                                               
186800                                                                          
186900     ACCEPT DAGENS-DATUM FROM DATE                                        
187000     PERFORM AA-NOLLA-TAB                                                 
187100     .                                                                    
187200     EJECT                                                                
187300 AA-NOLLA-TAB   SECTION.                                                  
187400                                                                          
187500     MOVE ZERO              TO FR-TAB-IDLOPNRM                            
187600                               FR-TAB-KDFORP                              
187700                               FR-TAB-KVQPACK-3                           
187800                               FR-TAB-ADLAGOMR                            
187900                               FR-TAB-ADGANG                              
188000                               FR-TAB-ADPLATS                             
188100                               FR-TAB-ADLAGOMR-EMB0                       
188200                               FR-TAB-ADGANG-EMB0                         
188300                               FR-TAB-ADPLATS-EMB0                        
188400                               FR-TAB-ADLAGOMR-EMB1                       
188500                               FR-TAB-ADGANG-EMB1                         
188600                               FR-TAB-ADPLATS-EMB1                        
188700                               FR-TAB-ADLAGOMR-EMB2                       
188800                               FR-TAB-ADGANG-EMB2                         
188900                               FR-TAB-ADPLATS-EMB2                        
189000                               FR-TAB-IDARTNR-EMBQ0                       
189100                               FR-TAB-IDARTNR-EMBQ1                       
189200                               FR-TAB-IDARTNR-EMBQ2                       
189300                               FR-TAB-KVAVIS                              
189400                               FR-TAB-VKART                               
189500                               FR-TAB-ADBUFFOMR-1                         
189600                               FR-TAB-ADBUFFGANG-1                        
189700                               FR-TAB-ADBUFFPL-1                          
189800                               FR-TAB-ADBUFFOMR-2                         
189900                               FR-TAB-ADBUFFGANG-2                        
190000                               FR-TAB-ADBUFFPL-2                          
190100                               FR-TAB-ADBUFFOMR-3                         
190200                               FR-TAB-ADBUFFGANG-3                        
190300                               FR-TAB-ADBUFFPL-3                          
190400                               FR-TAB-KVAVIS-PRIO                         
190500                               FR-TAB-VLARTNTO                            
190600                               FR-TAB-IDARTNR                             
190700                               FR-TAB-KVAVIS-KIT                          
190800                               FR-TAB-QTYP1                               
190900                               FR-TAB-QTYP2                               
191000                               FR-TAB-TIAAMMDD                            
191100                               FR-TAB-KVQPACK0                            
191200                               FR-TAB-KVQPACK1                            
191300                               FR-TAB-KVQPACK2                            
191400     MOVE ZERO              TO FR-NOVA-TAB-IDLOPNRM                       
191500                               FR-NOVA-TAB-KDFORP                         
191600                               FR-NOVA-TAB-KVQPACK-3                      
191700                               FR-NOVA-TAB-ADLAGOMR                       
191800                               FR-NOVA-TAB-ADGANG                         
191900                               FR-NOVA-TAB-ADPLATS                        
192000                               FR-NOVA-TAB-ADLAGOMR-EMB0                  
192100                               FR-NOVA-TAB-ADGANG-EMB0                    
192200                               FR-NOVA-TAB-ADPLATS-EMB0                   
192300                               FR-NOVA-TAB-ADLAGOMR-EMB1                  
192400                               FR-NOVA-TAB-ADGANG-EMB1                    
192500                               FR-NOVA-TAB-ADPLATS-EMB1                   
192600                               FR-NOVA-TAB-ADLAGOMR-EMB2                  
192700                               FR-NOVA-TAB-ADGANG-EMB2                    
192800                               FR-NOVA-TAB-ADPLATS-EMB2                   
192900                               FR-NOVA-TAB-IDARTNR-EMBQ0                  
193000                               FR-NOVA-TAB-IDARTNR-EMBQ1                  
193100                               FR-NOVA-TAB-IDARTNR-EMBQ2                  
193200                               FR-NOVA-TAB-KVAVIS                         
193300                               FR-NOVA-TAB-VKART                          
193400                               FR-NOVA-TAB-ADBUFFOMR-1                    
193500                               FR-NOVA-TAB-ADBUFFGANG-1                   
193600                               FR-NOVA-TAB-ADBUFFPL-1                     
193700                               FR-NOVA-TAB-ADBUFFOMR-2                    
193800                               FR-NOVA-TAB-ADBUFFGANG-2                   
193900                               FR-NOVA-TAB-ADBUFFPL-2                     
194000                               FR-NOVA-TAB-ADBUFFOMR-3                    
194100                               FR-NOVA-TAB-ADBUFFGANG-3                   
194200                               FR-NOVA-TAB-ADBUFFPL-3                     
194300                               FR-NOVA-TAB-KVAVIS-PRIO                    
194400                               FR-NOVA-TAB-VLARTNTO                       
194500                               FR-NOVA-TAB-IDARTNR                        
194600                               FR-NOVA-TAB-KVAVIS-KIT                     
194700                               FR-NOVA-TAB-QTYP1                          
194800                               FR-NOVA-TAB-QTYP2                          
194900                               FR-NOVA-TAB-TIAAMMDD                       
195000                               FR-NOVA-TAB-KVQPACK0                       
195100                               FR-NOVA-TAB-KVQPACK1                       
195200                               FR-NOVA-TAB-KVQPACK2                       
195300     MOVE ZERO              TO FR-TAB-IDLOPNRM-VCOM                       
195400                               FR-TAB-KDFORP-VCOM                         
195500                               FR-TAB-KVQPACK-3-VCOM                      
195600                               FR-TAB-ADLAGOMR-VCOM                       
195700                               FR-TAB-ADGANG-VCOM                         
195800                               FR-TAB-ADPLATS-VCOM                        
195900                               FR-TAB-ADLAGOMR-EMB0-VCOM                  
196000                               FR-TAB-ADGANG-EMB0-VCOM                    
196100                               FR-TAB-ADPLATS-EMB0-VCOM                   
196200                               FR-TAB-ADLAGOMR-EMB1-VCOM                  
196300                               FR-TAB-ADGANG-EMB1-VCOM                    
196400                               FR-TAB-ADPLATS-EMB1-VCOM                   
196500                               FR-TAB-ADLAGOMR-EMB2-VCOM                  
196600                               FR-TAB-ADGANG-EMB2-VCOM                    
196700                               FR-TAB-ADPLATS-EMB2-VCOM                   
196800                               FR-TAB-IDARTNR-EMBQ0-VCOM                  
196900                               FR-TAB-IDARTNR-EMBQ1-VCOM                  
197000                               FR-TAB-IDARTNR-EMBQ2-VCOM                  
197100                               FR-TAB-KVAVIS-VCOM                         
197200                               FR-TAB-VKART-VCOM                          
197300                               FR-TAB-ADBUFFOMR-1-VCOM                    
197400                               FR-TAB-ADBUFFGANG-1-VCOM                   
197500                               FR-TAB-ADBUFFPL-1-VCOM                     
197600                               FR-TAB-ADBUFFOMR-2-VCOM                    
197700                               FR-TAB-ADBUFFGANG-2-VCOM                   
197800                               FR-TAB-ADBUFFPL-2-VCOM                     
197900                               FR-TAB-ADBUFFOMR-3-VCOM                    
198000                               FR-TAB-ADBUFFGANG-3-VCOM                   
198100                               FR-TAB-ADBUFFPL-3-VCOM                     
198200                               FR-TAB-KVAVIS-PRIO-VCOM                    
198300                               FR-TAB-VLARTNTO-VCOM                       
198400                               FR-TAB-IDARTNR-VCOM                        
198500                               FR-TAB-KVAVIS-KIT-VCOM                     
198600                               FR-TAB-QTYP1-VCOM                          
198700                               FR-TAB-QTYP2-VCOM                          
198800                               FR-TAB-TIAAMMDD-VCOM                       
198900                               FR-TAB-KVQPACK0-VCOM                       
199000                               FR-TAB-KVQPACK1-VCOM                       
199100                               FR-TAB-KVQPACK2-VCOM                       
199200     MOVE ZERO              TO FR-LASER-IDLOPNRM                          
199300                               FR-LASER-KVQPACK-3                         
199400                               FR-LASER-ADLAGOMR                          
199500                               FR-LASER-ADGANG                            
199600                               FR-LASER-ADPLATS                           
199700                               FR-LASER-ADLAGOMR-EMB0                     
199800                               FR-LASER-ADGANG-EMB0                       
199900                               FR-LASER-ADPLATS-EMB0                      
200000                               FR-LASER-ADLAGOMR-EMB1                     
200100                               FR-LASER-ADGANG-EMB1                       
200200                               FR-LASER-ADPLATS-EMB1                      
200300                               FR-LASER-ADLAGOMR-EMB2                     
200400                               FR-LASER-ADGANG-EMB2                       
200500                               FR-LASER-ADPLATS-EMB2                      
200600                               FR-LASER-IDARTNR-EMBQ0                     
200700                               FR-LASER-IDARTNR-EMBQ1                     
200800                               FR-LASER-IDARTNR-EMBQ2                     
200900                               FR-LASER-KVAVIS                            
201000                               FR-LASER-VKART                             
201100                               FR-LASER-ADBUFFOMR-1                       
201200                               FR-LASER-ADBUFFGANG-1                      
201300                               FR-LASER-ADBUFFPL-1                        
201400                               FR-LASER-ADBUFFOMR-2                       
201500                               FR-LASER-ADBUFFGANG-2                      
201600                               FR-LASER-ADBUFFPL-2                        
201700                               FR-LASER-ADBUFFOMR-3                       
201800                               FR-LASER-ADBUFFGANG-3                      
201900                               FR-LASER-ADBUFFPL-3                        
202000                               FR-LASER-KVAVIS-PRIO                       
202100                               FR-LASER-VLARTNTO                          
202200                               FR-LASER-IDARTNR                           
202300                               FR-LASER-KVAVIS-KIT                        
202400                               FR-LASER-KDFORP                            
202500                               FR-LASER-QTYP1                             
202600                               FR-LASER-QTYP2                             
202700                               FR-LASER-TIAAMMDD                          
202800                               FR-LASER-KVQPACK0                          
202900                               FR-LASER-KVQPACK1                          
203000                               FR-LASER-KVQPACK2                          
203100                               FR-LASER-IDLOPNRM-STRK                     
203200                               PR-LASER-KVQPACK-3                         
203300                               PR-LASER-ADLAGOMR                          
203400                               PR-LASER-ADGANG                            
203500                               PR-LASER-ADPLATS                           
203600                               PR-LASER-ADLAGOMR-EMB0                     
203700                               PR-LASER-ADGANG-EMB0                       
203800                               PR-LASER-ADPLATS-EMB0                      
203900                               PR-LASER-ADLAGOMR-EMB1                     
204000                               PR-LASER-ADGANG-EMB1                       
204100                               PR-LASER-ADPLATS-EMB1                      
204200                               PR-LASER-ADLAGOMR-EMB2                     
204300                               PR-LASER-ADGANG-EMB2                       
204400                               PR-LASER-ADPLATS-EMB2                      
204500                               PR-LASER-IDARTNR-EMBQ0                     
204600                               PR-LASER-IDARTNR-EMBQ1                     
204700                               PR-LASER-IDARTNR-EMBQ2                     
204800                               PR-LASER-KVAVIS                            
204900                               PR-LASER-VKART                             
205000                               PR-LASER-ADBUFFOMR-1                       
205100                               PR-LASER-ADBUFFGANG-1                      
205200                               PR-LASER-ADBUFFPL-1                        
205300                               PR-LASER-ADBUFFOMR-2                       
205400                               PR-LASER-ADBUFFGANG-2                      
205500                               PR-LASER-ADBUFFPL-2                        
205600                               PR-LASER-ADBUFFOMR-3                       
205700                               PR-LASER-ADBUFFGANG-3                      
205800                               PR-LASER-ADBUFFPL-3                        
205900                               PR-LASER-KVAVIS-PRIO                       
206000                               PR-LASER-VLARTNTO                          
206100                               PR-LASER-IDARTNR                           
206200                               PR-LASER-KVAVIS-KIT                        
206300                               PR-LASER-KDFORP                            
206400                               PR-LASER-QTYP1                             
206500                               PR-LASER-QTYP2                             
206600                               PR-LASER-TIAAMMDD                          
206700                               PR-LASER-KVQPACK0                          
206800                               PR-LASER-KVQPACK1                          
206900                               PR-LASER-KVQPACK2                          
207000                               PR-LASER-IDLOPNRM-STRK                     
207100                               FR-TAB-KVROS                               
207200                               FR-TAB-KVROS-VCOM                          
207300                               FR-LASER-KVROS                             
207400                               PR-LASER-KVROS                             
207500     MOVE SPACE            TO  FR-TAB-ADINLOMR                            
207600     MOVE SPACE            TO  FR-NOVA-TAB-ADINLOMR                       
207700                               FR-TAB-ADINLOMR-VCOM                       
207800                               FR-LASER-ADINLOMR                          
207900                               PR-LASER-ADINLOMR                          
208000                               FR-TAB-KDFARLIG-TEXT-L                     
208100                               FR-NOVA-TAB-KDFARLIG-TEXT-L                
208200                               FR-TAB-KDFARLIG-TEXT-L-VCOM                
208300                               FR-LASER-KDFARLIG-TEXT-L                   
208400                               PR-LASER-KDFARLIG-TEXT-L                   
208500                               FR-TAB-BEART-EMB0                          
208600                               FR-NOVA-TAB-BEART-EMB0                     
208700                               FR-TAB-BEART-EMB0-VCOM                     
208800                               FR-LASER-BEART-EMB0                        
208900                               PR-LASER-BEART-EMB0                        
209000                               FR-TAB-BEART-EMB1                          
209100                               FR-NOVA-TAB-BEART-EMB1                     
209200                               FR-TAB-BEART-EMB1-VCOM                     
209300                               FR-LASER-BEART-EMB1                        
209400                               PR-LASER-BEART-EMB1                        
209500                               FR-TAB-BEART-EMB2                          
209600                               FR-NOVA-TAB-BEART-EMB2                     
209700                               FR-TAB-BEART-EMB2-VCOM                     
209800                               FR-LASER-BEART-EMB2                        
209900                               PR-LASER-BEART-EMB2                        
210000                               FR-TAB-QTYP0                               
210100                               FR-NOVA-TAB-QTYP0                          
210200                               FR-TAB-QTYP0-VCOM                          
210300                               FR-LASER-QTYP0                             
210400                               PR-LASER-QTYP0                             
210500                               FR-TAB-ADTRDEST-KIT                        
210600                               FR-NOVA-TAB-ADTRDEST-KIT                   
210700                               FR-TAB-ADTRDEST-KIT-VCOM                   
210800     .                                                                    
210900     EJECT                                                                
211000 B-RED-SIDA SECTION.                                                      
211100                                                                          
211200     PERFORM BA-LAES-ART-INFO                                             
211300                                                                          
211400     MOVE PRT-NYSIDA-RAD1        TO PRT-RADSKIP                           
211500                                                                          
211600     MOVE INLA-ART-IDLOPNRM      TO FR-TAB-IDLOPNRM                       
211700     MOVE INLA-ART-IDLOPNRM      TO FR-NOVA-TAB-IDLOPNRM                  
211800     MOVE INLA-ART-IDLOPNRM      TO FR-NOVA-TAB-IDLOPNRM-STRK             
211900                                    FR-TAB-IDLOPNRM-VCOM                  
212000                                    FR-TAB-IDLOPNRM-STRK                  
212100                                    FR-LASER-IDLOPNRM                     
212200                                    FR-LASER-IDLOPNRM-STRK                
212300                                    PR-LASER-IDLOPNRM-STRK                
212400                                                                          
212500     MOVE INLA-ART-BEART         TO FR-TAB-BEART                          
212600     MOVE INLA-ART-BEART         TO FR-NOVA-TAB-BEART                     
212700                                    FR-TAB-BEART-VCOM                     
212800                                    FR-LASER-BEART                        
212900                                    PR-LASER-BEART                        
213000     MOVE INLA-ART-IDARTNR       TO FR-TAB-IDARTNR                        
213100     MOVE INLA-ART-IDARTNR       TO FR-NOVA-TAB-IDARTNR                   
213200                                    FR-TAB-IDARTNR-VCOM                   
213300                                    FR-LASER-IDARTNR                      
213400                                    PR-LASER-IDARTNR                      
213500                                                                          
213600     MOVE INLA-ART-KVAVIS        TO FR-TAB-KVAVIS                         
213700     MOVE INLA-ART-KVAVIS        TO FR-NOVA-TAB-KVAVIS                    
213800                                    FR-TAB-KVAVIS-VCOM                    
213900                                    FR-LASER-KVAVIS                       
214000                                    PR-LASER-KVAVIS                       
214100     MOVE INLA-ART-KVAVIS-KIT    TO FR-TAB-KVAVIS-KIT                     
214200     MOVE INLA-ART-KVAVIS-KIT    TO FR-NOVA-TAB-KVAVIS-KIT                
214300                                    FR-TAB-KVAVIS-KIT-VCOM                
214400                                    FR-LASER-KVAVIS-KIT                   
214500                                    PR-LASER-KVAVIS-KIT                   
214600     MOVE INLA-ART-ADTRDEST-KIT  TO FR-TAB-ADTRDEST-KIT                   
214700     MOVE INLA-ART-ADTRDEST-KIT  TO FR-NOVA-TAB-ADTRDEST-KIT              
214800                                    FR-TAB-ADTRDEST-KIT-VCOM              
214900     MOVE INLA-ART-KVAVIS-PRIO   TO FR-TAB-KVAVIS-PRIO                    
215000     MOVE INLA-ART-KVAVIS-PRIO   TO FR-NOVA-TAB-KVAVIS-PRIO               
215100                                    FR-TAB-KVAVIS-PRIO-VCOM               
215200                                    FR-LASER-KVAVIS-PRIO                  
215300                                    PR-LASER-KVAVIS-PRIO                  
215400                                                                          
215500     MOVE INLA-ART-VKART         TO FR-TAB-VKART                          
215600     MOVE INLA-ART-VKART         TO FR-NOVA-TAB-VKART                     
215700                                    FR-TAB-VKART-VCOM                     
215800                                    FR-LASER-VKART                        
215900                                    PR-LASER-VKART                        
216000     MOVE INLA-ART-VLARTNTO      TO FR-TAB-VLARTNTO                       
216100     MOVE INLA-ART-VLARTNTO      TO FR-NOVA-TAB-VLARTNTO                  
216200                                    FR-TAB-VLARTNTO-VCOM                  
216300                                    FR-LASER-VLARTNTO                     
216400                                    PR-LASER-VLARTNTO                     
216500     MOVE INLA-ART-BEFT          TO FR-TAB-BEFT                           
216600     MOVE INLA-ART-BEFT          TO FR-NOVA-TAB-BEFT                      
216700                                    FR-TAB-BEFT-VCOM                      
216800                                    FR-LASER-BEFT                         
216900                                    PR-LASER-BEFT                         
217000     MOVE INLA-ART-KDSORT        TO FR-TAB-KDSORT                         
217100     MOVE INLA-ART-KDSORT        TO FR-NOVA-TAB-KDSORT                    
217200                                    FR-TAB-KDSORT-VCOM                    
217300                                    FR-LASER-KDSORT                       
217400                                    PR-LASER-KDSORT                       
217500                                                                          
217600     PERFORM BB-RED-ADBUFF                                                
217700                                                                          
217800     PERFORM BC-RED-FARLIG                                                
217900                                                                          
218000     MOVE INLA-ART-KDLAGEMB      TO FR-TAB-KDLAGEMB                       
218100     MOVE INLA-ART-KDLAGEMB      TO FR-NOVA-TAB-KDLAGEMB                  
218200                                    FR-TAB-KDLAGEMB-VCOM                  
218300                                    FR-LASER-KDLAGEMB                     
218400                                    PR-LASER-KDLAGEMB                     
218500     IF INLA-ART-KDKVAANT = +0                                            
218600       MOVE SPACE TO                FR-TAB-KDKVAANT-TEXT                  
218700       MOVE SPACE TO                FR-NOVA-TAB-KDKVAANT-TEXT             
218800                                    FR-TAB-KDKVAANT-TEXT-VCOM             
218900                                    FR-LASER-KDKVAANT-TEXT                
219000                                    PR-LASER-KDKVAANT-TEXT                
219100     ELSE                                                                 
219200       MOVE 'JA ' TO                FR-TAB-KDKVAANT-TEXT                  
219300       MOVE 'JA ' TO                FR-NOVA-TAB-KDKVAANT-TEXT             
219400                                    FR-TAB-KDKVAANT-TEXT-VCOM             
219500                                    FR-LASER-KDKVAANT-TEXT                
219600       MOVE 'YES' TO                PR-LASER-KDKVAANT-TEXT                
219700     END-IF                                                               
219800     MOVE WS-ARTC11-KVQPACK-3    TO FR-TAB-KVQPACK-3                      
219900     MOVE WS-ARTC11-KVQPACK-3    TO FR-NOVA-TAB-KVQPACK-3                 
220000                                    FR-TAB-KVQPACK-3-VCOM                 
220100                                    FR-LASER-KVQPACK-3                    
220200                                    PR-LASER-KVQPACK-3                    
220300                                                                          
220400     IF  WS-ARTC11-KDARTURS NOT = '  '                                    
220500       PERFORM BG-CALL-W400ARTU                                           
220600       MOVE ARTU-BEARTURS-SVE              TO FR-TAB-BEARTURS             
220700       MOVE ARTU-BEARTURS-SVE              TO FR-NOVA-TAB-BEARTURS        
220800                                              FR-TAB-BEARTURS-VCOM        
220900                                              FR-LASER-BEARTURS           
221000       MOVE ARTU-BEARTURS-ENG              TO PR-LASER-BEARTURS           
221100     ELSE                                                                 
221200       MOVE SPACE                TO FR-TAB-BEARTURS                       
221300       MOVE SPACE                TO FR-NOVA-TAB-BEARTURS                  
221400                                    FR-TAB-BEARTURS-VCOM                  
221500                                    FR-LASER-BEARTURS                     
221600                                    PR-LASER-BEARTURS                     
221700     END-IF                                                               
221800     MOVE WS-ARTC11-KDFORP       TO FR-LASER-KDFORP                       
221900                                    PR-LASER-KDFORP                       
222000                                    FR-TAB-KDFORP                         
222100                                    FR-NOVA-TAB-KDFORP                    
222200                                    FR-TAB-KDFORP-VCOM                    
222300                                                                          
222400     IF MID-FLSVS = JA                                                    
222500        MOVE WS-ARTC11-ADLAGOMR-SVS TO FR-TAB-ADLAGOMR                    
222600        MOVE WS-ARTC11-ADLAGOMR-SVS TO FR-NOVA-TAB-ADLAGOMR               
222700                                       FR-TAB-ADLAGOMR-VCOM               
222800                                       FR-LASER-ADLAGOMR                  
222900                                       PR-LASER-ADLAGOMR                  
223000        MOVE WS-ARTC11-ADGANG-SVS   TO FR-TAB-ADGANG                      
223100        MOVE WS-ARTC11-ADGANG-SVS   TO FR-NOVA-TAB-ADGANG                 
223200                                       FR-TAB-ADGANG-VCOM                 
223300                                       FR-LASER-ADGANG                    
223400                                       PR-LASER-ADGANG                    
223500        MOVE WS-ARTC11-ADPLATS-SVS  TO FR-TAB-ADPLATS                     
223600        MOVE WS-ARTC11-ADPLATS-SVS  TO FR-NOVA-TAB-ADPLATS                
223700                                       FR-TAB-ADPLATS-VCOM                
223800                                       FR-LASER-ADPLATS                   
223900                                       PR-LASER-ADPLATS                   
224000     ELSE                                                                 
224100        MOVE INLA-ART-ADLAGOMR      TO FR-TAB-ADLAGOMR                    
224200        MOVE INLA-ART-ADLAGOMR      TO FR-NOVA-TAB-ADLAGOMR               
224300                                       FR-TAB-ADLAGOMR-VCOM               
224400                                       FR-LASER-ADLAGOMR                  
224500                                       PR-LASER-ADLAGOMR                  
224600        MOVE INLA-ART-ADGANG        TO FR-TAB-ADGANG                      
224700        MOVE INLA-ART-ADGANG        TO FR-NOVA-TAB-ADGANG                 
224800                                       FR-TAB-ADGANG-VCOM                 
224900                                       FR-LASER-ADGANG                    
225000                                       PR-LASER-ADGANG                    
225100        MOVE INLA-ART-ADPLATS       TO FR-TAB-ADPLATS                     
225200        MOVE INLA-ART-ADPLATS       TO FR-NOVA-TAB-ADPLATS                
225300                                       FR-TAB-ADPLATS-VCOM                
225400                                       FR-LASER-ADPLATS                   
225500                                       PR-LASER-ADPLATS                   
225600     END-IF                                                               
225700                                                                          
225800     SUBTRACT INLA-ART-KVAVIS-KIT FROM INLA-ART-KVAVIS                    
225900     GIVING WS-KVAVIS-EJ-KIT                                              
226000                                                                          
226010     IF SW-WDK712-US-CN-FINNS = JA                                        
226100       MOVE WS-WDK712-IDARTNR-EMBQ0 TO WS-EMB-IDARTNR-EMBQ                
226101     ELSE                                                                 
226110       MOVE WS-ARTC11-IDARTNR-EMBQ0 TO WS-EMB-IDARTNR-EMBQ                
226120     END-IF                                                               
226200     MOVE WS-ARTC11-KVQPACK-0    TO WS-EMB-KVQPACK                        
226300     MOVE 0                      TO WS-EMB-QTYP                           
226400     PERFORM BD-BEH-EMB-RAD                                               
226500                                                                          
226600     IF WS-EMB-IDARTNR-EMBQ > +0                                          
226700       MOVE SPAR-ADLAGOMR         TO FR-TAB-ADLAGOMR-EMB0                 
226800       MOVE SPAR-ADLAGOMR         TO FR-NOVA-TAB-ADLAGOMR-EMB0            
226900                                     FR-TAB-ADLAGOMR-EMB0-VCOM            
227000                                     FR-LASER-ADLAGOMR-EMB0               
227100                                     PR-LASER-ADLAGOMR-EMB0               
227200       MOVE SPAR-ADGANG           TO FR-TAB-ADGANG-EMB0                   
227300       MOVE SPAR-ADGANG           TO FR-NOVA-TAB-ADGANG-EMB0              
227400                                     FR-TAB-ADGANG-EMB0-VCOM              
227500                                     FR-LASER-ADGANG-EMB0                 
227600                                     PR-LASER-ADGANG-EMB0                 
227700       MOVE SPAR-ADPLATS          TO FR-TAB-ADPLATS-EMB0                  
227800       MOVE SPAR-ADPLATS          TO FR-NOVA-TAB-ADPLATS-EMB0             
227900                                     FR-TAB-ADPLATS-EMB0-VCOM             
228000                                     FR-LASER-ADPLATS-EMB0                
228100                                     PR-LASER-ADPLATS-EMB0                
228200       MOVE SPAR-IDARTNR-EMBQ     TO FR-TAB-IDARTNR-EMBQ0                 
228300       MOVE SPAR-IDARTNR-EMBQ     TO FR-NOVA-TAB-IDARTNR-EMBQ0            
228400                                     FR-TAB-IDARTNR-EMBQ0-VCOM            
228500                                     FR-LASER-IDARTNR-EMBQ0               
228600                                     PR-LASER-IDARTNR-EMBQ0               
228700       MOVE SPAR-BEART            TO FR-TAB-BEART-EMB0                    
228800       MOVE SPAR-BEART            TO FR-NOVA-TAB-BEART-EMB0               
228900                                     FR-TAB-BEART-EMB0-VCOM               
229000                                     FR-LASER-BEART-EMB0                  
229100                                     PR-LASER-BEART-EMB0                  
229200       MOVE SPAR-QTYP             TO FR-TAB-QTYP0                         
229300       MOVE SPAR-QTYP             TO FR-NOVA-TAB-QTYP0                    
229400                                     FR-TAB-QTYP0-VCOM                    
229500                                     FR-LASER-QTYP0                       
229600                                     PR-LASER-QTYP0                       
229700       MOVE SPAR-KVQPACK          TO FR-TAB-KVQPACK0                      
229800       MOVE SPAR-KVQPACK          TO FR-NOVA-TAB-KVQPACK0                 
229900                                     FR-TAB-KVQPACK0-VCOM                 
230000                                     FR-LASER-KVQPACK0                    
230100                                     PR-LASER-KVQPACK0                    
230200     END-IF                                                               
230300                                                                          
230310     IF SW-WDK712-US-CN-FINNS = JA                                        
230400       MOVE WS-WDK712-IDARTNR-EMBQ1 TO WS-EMB-IDARTNR-EMBQ                
230401     ELSE                                                                 
230402       MOVE WS-ARTC11-IDARTNR-EMBQ1 TO WS-EMB-IDARTNR-EMBQ                
230420     END-IF                                                               
230500     MOVE WS-ARTC11-KVQPACK-1    TO WS-EMB-KVQPACK                        
230600     MOVE 1                      TO WS-EMB-QTYP                           
230700     PERFORM BD-BEH-EMB-RAD                                               
230800                                                                          
230900     IF WS-EMB-IDARTNR-EMBQ > +0                                          
231000       MOVE SPAR-ADLAGOMR         TO FR-TAB-ADLAGOMR-EMB1                 
231100       MOVE SPAR-ADLAGOMR         TO FR-NOVA-TAB-ADLAGOMR-EMB1            
231200                                     FR-TAB-ADLAGOMR-EMB1-VCOM            
231300                                     FR-LASER-ADLAGOMR-EMB1               
231400                                     PR-LASER-ADLAGOMR-EMB1               
231500       MOVE SPAR-ADGANG           TO FR-TAB-ADGANG-EMB1                   
231600       MOVE SPAR-ADGANG           TO FR-NOVA-TAB-ADGANG-EMB1              
231700                                     FR-TAB-ADGANG-EMB1-VCOM              
231800                                     FR-LASER-ADGANG-EMB1                 
231900                                     PR-LASER-ADGANG-EMB1                 
232000       MOVE SPAR-ADPLATS          TO FR-TAB-ADPLATS-EMB1                  
232100       MOVE SPAR-ADPLATS          TO FR-NOVA-TAB-ADPLATS-EMB1             
232200                                     FR-TAB-ADPLATS-EMB1-VCOM             
232300                                     FR-LASER-ADPLATS-EMB1                
232400                                     PR-LASER-ADPLATS-EMB1                
232500       MOVE SPAR-IDARTNR-EMBQ     TO FR-TAB-IDARTNR-EMBQ1                 
232600       MOVE SPAR-IDARTNR-EMBQ     TO FR-NOVA-TAB-IDARTNR-EMBQ1            
232700                                     FR-TAB-IDARTNR-EMBQ1-VCOM            
232800                                     FR-LASER-IDARTNR-EMBQ1               
232900                                     PR-LASER-IDARTNR-EMBQ1               
233000       MOVE SPAR-BEART            TO FR-TAB-BEART-EMB1                    
233100       MOVE SPAR-BEART            TO FR-NOVA-TAB-BEART-EMB1               
233200                                     FR-TAB-BEART-EMB1-VCOM               
233300                                     FR-LASER-BEART-EMB1                  
233400                                     PR-LASER-BEART-EMB1                  
233500       MOVE SPAR-QTYP             TO FR-TAB-QTYP1                         
233600       MOVE SPAR-QTYP             TO FR-NOVA-TAB-QTYP1                    
233700                                     FR-TAB-QTYP1-VCOM                    
233800                                     FR-LASER-QTYP1                       
233900                                     PR-LASER-QTYP1                       
234000       MOVE SPAR-KVQPACK          TO FR-TAB-KVQPACK1                      
234100       MOVE SPAR-KVQPACK          TO FR-NOVA-TAB-KVQPACK1                 
234200                                     FR-TAB-KVQPACK1-VCOM                 
234300                                     FR-LASER-KVQPACK1                    
234400                                     PR-LASER-KVQPACK1                    
234500     END-IF                                                               
234600                                                                          
234700     MOVE WS-ARTC11-KVROS         TO FR-TAB-KVROS                         
234800     MOVE WS-ARTC11-KVROS         TO FR-NOVA-TAB-KVROS                    
234900                                     FR-TAB-KVROS-VCOM                    
235000                                     FR-LASER-KVROS                       
235100                                     PR-LASER-KVROS                       
235210     IF SW-WDK712-US-CN-FINNS = JA                                        
235220       MOVE WS-WDK712-IDARTNR-EMBQ2 TO WS-EMB-IDARTNR-EMBQ                
235230     ELSE                                                                 
235240       MOVE WS-ARTC11-IDARTNR-EMBQ2 TO WS-EMB-IDARTNR-EMBQ                
235250     END-IF                                                               
235300     MOVE WS-ARTC11-KVQPACK-2    TO WS-EMB-KVQPACK                        
235400     MOVE 2                      TO WS-EMB-QTYP                           
235500     PERFORM BD-BEH-EMB-RAD                                               
235600                                                                          
235700     IF WS-EMB-IDARTNR-EMBQ > +0                                          
235800       MOVE SPAR-ADLAGOMR         TO FR-TAB-ADLAGOMR-EMB2                 
235900       MOVE SPAR-ADLAGOMR         TO FR-NOVA-TAB-ADLAGOMR-EMB2            
236000                                     FR-TAB-ADLAGOMR-EMB2-VCOM            
236100                                     FR-LASER-ADLAGOMR-EMB2               
236200                                     PR-LASER-ADLAGOMR-EMB2               
236300       MOVE SPAR-ADGANG           TO FR-TAB-ADGANG-EMB2                   
236400       MOVE SPAR-ADGANG           TO FR-NOVA-TAB-ADGANG-EMB2              
236500                                     FR-TAB-ADGANG-EMB2-VCOM              
236600                                     FR-LASER-ADGANG-EMB2                 
236700                                     PR-LASER-ADGANG-EMB2                 
236800       MOVE SPAR-ADPLATS          TO FR-TAB-ADPLATS-EMB2                  
236900       MOVE SPAR-ADPLATS          TO FR-NOVA-TAB-ADPLATS-EMB2             
237000                                     FR-TAB-ADPLATS-EMB2-VCOM             
237100                                     FR-LASER-ADPLATS-EMB2                
237200                                     PR-LASER-ADPLATS-EMB2                
237300       MOVE SPAR-IDARTNR-EMBQ     TO FR-TAB-IDARTNR-EMBQ2                 
237400       MOVE SPAR-IDARTNR-EMBQ     TO FR-NOVA-TAB-IDARTNR-EMBQ2            
237500                                     FR-TAB-IDARTNR-EMBQ2-VCOM            
237600                                     FR-LASER-IDARTNR-EMBQ2               
237700                                     PR-LASER-IDARTNR-EMBQ2               
237800       MOVE SPAR-BEART            TO FR-TAB-BEART-EMB2                    
237900       MOVE SPAR-BEART            TO FR-NOVA-TAB-BEART-EMB2               
238000                                     FR-TAB-BEART-EMB2-VCOM               
238100                                     FR-LASER-BEART-EMB2                  
238200                                     PR-LASER-BEART-EMB2                  
238300       MOVE SPAR-QTYP             TO FR-TAB-QTYP2                         
238400       MOVE SPAR-QTYP             TO FR-NOVA-TAB-QTYP2                    
238500                                     FR-TAB-QTYP2-VCOM                    
238600                                     FR-LASER-QTYP2                       
238700                                     PR-LASER-QTYP2                       
238800       MOVE SPAR-KVQPACK          TO FR-TAB-KVQPACK2                      
238900       MOVE SPAR-KVQPACK          TO FR-NOVA-TAB-KVQPACK2                 
239000                                     FR-TAB-KVQPACK2-VCOM                 
239100                                     FR-LASER-KVQPACK2                    
239200                                     PR-LASER-KVQPACK2                    
239300     END-IF                                                               
239400                                                                          
239500     PERFORM BE-RED-DATUM-RAD                                             
239600                                                                          
239700     PERFORM BF-CALL-W611STYR                                             
239800     .                                                                    
239900     EJECT                                                                
240000 BA-LAES-ART-INFO SECTION.                                                
240100                                                                          
240200*    --- IDDC och          fixat i styrsektionen                          
240300                                                                          
240400     MOVE MID-IDDC               TO W-W6D101KY-IDDC                       
240500     MOVE MID-IDLEVNR  (IX-POST) TO W-W6D101KY-IDLEVNR                    
240600     MOVE MID-IDFS     (IX-POST) TO W-W6D101KY-IDFS                       
240700     MOVE MID-TIAVIDAT (IX-POST) TO W-W6D101KY-TIAVIDAT                   
240800     MOVE MID-IDRADNR-INL(IX-POST) TO W-IDRADNR-INL                       
240900                                                                          
241000     PERFORM IMS-GU-INLA-ART                                              
241100                                                                          
241200     MOVE INLA-ART-IDARTNR       TO W-IDARTNR                             
241300                                                                          
241400     PERFORM IMS-GU-ARTC01                                                
241500     MOVE ARTC01-ART-IDFTG       TO WS-ARTC01-IDFTG                       
241600                                                                          
241700     PERFORM IMS-GNP-ARTC11                                               
241800     MOVE ARTC11-CLAG-KVQPACK-0       TO WS-ARTC11-KVQPACK-0              
241900     MOVE ARTC11-CLAG-KVQPACK-1       TO WS-ARTC11-KVQPACK-1              
242000     MOVE ARTC11-CLAG-KVQPACK-2       TO WS-ARTC11-KVQPACK-2              
242100     MOVE ARTC11-CLAG-KVQPACK-3       TO WS-ARTC11-KVQPACK-3              
242200     MOVE ARTC11-CLAG-KDARTURS        TO WS-ARTC11-KDARTURS               
242300     MOVE ARTC11-CLAG-KDFORP          TO WS-ARTC11-KDFORP                 
242400     MOVE ARTC11-CLAG-IDARTNR-EMBQ0   TO WS-ARTC11-IDARTNR-EMBQ0          
242500     MOVE ARTC11-CLAG-IDARTNR-EMBQ1   TO WS-ARTC11-IDARTNR-EMBQ1          
242600     MOVE ARTC11-CLAG-IDARTNR-EMBQ2   TO WS-ARTC11-IDARTNR-EMBQ2          
242700     MOVE ARTC11-CLAG-ADLAGOMR        TO WS-ARTC11-ADLAGOMR               
242800     MOVE ARTC11-CLAG-ADGANG          TO WS-ARTC11-ADGANG                 
242900     MOVE ARTC11-CLAG-ADPLATS         TO WS-ARTC11-ADPLATS                
243000     MOVE ARTC11-CLAG-ADLAGOMR-SVS    TO WS-ARTC11-ADLAGOMR-SVS           
243100     MOVE ARTC11-CLAG-ADGANG-SVS      TO WS-ARTC11-ADGANG-SVS             
243200     MOVE ARTC11-CLAG-ADPLATS-SVS     TO WS-ARTC11-ADPLATS-SVS            
243300     MOVE ARTC11-CLAG-FLFSP           TO WS-ARTC11-FLFSP                  
243400     MOVE ARTC11-CLAG-KVROS           TO WS-ARTC11-KVROS                  
243500     MOVE JA                   TO SW-ARTC11-C1-FINNS                      
243510                                                                          
243511     IF NDC-US OR NDC-CN                                                  
243512        IF NDC-US                                                         
243513           MOVE 'US' TO W-IDLAND-US-CN                                    
243514        ELSE                                                              
243515           MOVE 'CN' TO W-IDLAND-US-CN                                    
243516        END-IF                                                            
243540                                                                          
243550     PERFORM IMS-GU-WDK712                                                
243551     IF SEGMENT-FINNS                                                     
243552       IF NDC-US OR NDC-CN                                                
243593       MOVE LART-IDARTNR-EMBQ0 TO WS-WDK712-IDARTNR-EMBQ0                 
243594       MOVE LART-IDARTNR-EMBQ1 TO WS-WDK712-IDARTNR-EMBQ1                 
243595       MOVE LART-IDARTNR-EMBQ2 TO WS-WDK712-IDARTNR-EMBQ2                 
243596       IF LART-KVQPACK-3 > 0                                              
243597         MOVE LART-KVQPACK-3 TO WS-ARTC11-KVQPACK-3                       
243600       END-IF                                                             
243601                                                                          
243604       MOVE JA TO SW-WDK712-US-CN-FINNS                                   
243605       END-IF                                                             
243606                                                                          
243617     END-IF                                                               
243618     end-if                                                               
243620     .                                                                    
243700     EJECT                                                                
243800 BB-RED-ADBUFF SECTION.                                                   
243900                                                                          
244000     MOVE ZERO                   TO WS-ANT-GODK-ADBUFF                    
244100                                                                          
244200     PERFORM IMS-GU-ARTD-ART                                              
244300                                                                          
244400     IF  SEGMENT-FINNS                                                    
244500       PERFORM IMS-GNP-ARTD-SALDO                                         
244600                                                                          
244700       PERFORM UNTIL (SEGMENT-SAKNAS                                      
244800                  OR  WS-ANT-GODK-ADBUFF >= K-MAX-REDIG-ADBUFF)           
244900                                                                          
245000         IF  ARTD-SALDO-ADBUFFOMR = WS-ARTC11-ADLAGOMR                    
245100           CONTINUE                                                       
245200         ELSE                                                             
245300           ADD +1                TO WS-ANT-GODK-ADBUFF                    
245400                                                                          
245500           EVALUATE WS-ANT-GODK-ADBUFF                                    
245600            WHEN +1                                                       
245700             MOVE ARTD-SALDO-ADBUFFOMR TO FR-TAB-ADBUFFOMR-1              
245800             MOVE ARTD-SALDO-ADBUFFOMR TO FR-NOVA-TAB-ADBUFFOMR-1         
245900                                          FR-TAB-ADBUFFOMR-1-VCOM         
246000                                          FR-LASER-ADBUFFOMR-1            
246100                                          PR-LASER-ADBUFFOMR-1            
246200             MOVE ARTD-SALDO-ADBUFFGANG TO FR-TAB-ADBUFFGANG-1            
246300           MOVE ARTD-SALDO-ADBUFFGANG TO FR-NOVA-TAB-ADBUFFGANG-1         
246400                                          FR-TAB-ADBUFFGANG-1-VCOM        
246500                                          FR-LASER-ADBUFFGANG-1           
246600                                          PR-LASER-ADBUFFGANG-1           
246700             MOVE ARTD-SALDO-ADBUFFPL TO  FR-TAB-ADBUFFPL-1               
246800             MOVE ARTD-SALDO-ADBUFFPL TO  FR-NOVA-TAB-ADBUFFPL-1          
246900                                          FR-TAB-ADBUFFPL-1-VCOM          
247000                                          FR-LASER-ADBUFFPL-1             
247100                                          PR-LASER-ADBUFFPL-1             
247200            WHEN +2                                                       
247300             MOVE ARTD-SALDO-ADBUFFOMR TO FR-TAB-ADBUFFOMR-2              
247400             MOVE ARTD-SALDO-ADBUFFOMR TO FR-NOVA-TAB-ADBUFFOMR-2         
247500                                          FR-TAB-ADBUFFOMR-2-VCOM         
247600                                          FR-LASER-ADBUFFOMR-2            
247700                                          PR-LASER-ADBUFFOMR-2            
247800             MOVE ARTD-SALDO-ADBUFFGANG TO FR-TAB-ADBUFFGANG-2            
247900           MOVE ARTD-SALDO-ADBUFFGANG TO FR-NOVA-TAB-ADBUFFGANG-2         
248000                                          FR-TAB-ADBUFFGANG-2-VCOM        
248100                                          FR-LASER-ADBUFFGANG-2           
248200                                          PR-LASER-ADBUFFGANG-2           
248300             MOVE ARTD-SALDO-ADBUFFPL TO  FR-TAB-ADBUFFPL-2               
248400             MOVE ARTD-SALDO-ADBUFFPL TO  FR-NOVA-TAB-ADBUFFPL-2          
248500                                          FR-TAB-ADBUFFPL-2-VCOM          
248600                                          FR-LASER-ADBUFFPL-2             
248700                                          PR-LASER-ADBUFFPL-2             
248800            WHEN +3                                                       
248900             MOVE ARTD-SALDO-ADBUFFOMR TO FR-TAB-ADBUFFOMR-3              
249000             MOVE ARTD-SALDO-ADBUFFOMR TO FR-NOVA-TAB-ADBUFFOMR-3         
249100                                          FR-TAB-ADBUFFOMR-3-VCOM         
249200                                          FR-LASER-ADBUFFOMR-3            
249300                                          PR-LASER-ADBUFFOMR-3            
249400             MOVE ARTD-SALDO-ADBUFFGANG TO FR-TAB-ADBUFFGANG-3            
249500          MOVE ARTD-SALDO-ADBUFFGANG TO FR-NOVA-TAB-ADBUFFGANG-3          
249600                                          FR-TAB-ADBUFFGANG-3-VCOM        
249700                                          FR-LASER-ADBUFFGANG-3           
249800                                          PR-LASER-ADBUFFGANG-3           
249900             MOVE ARTD-SALDO-ADBUFFPL TO  FR-TAB-ADBUFFPL-3               
250000             MOVE ARTD-SALDO-ADBUFFPL TO  FR-NOVA-TAB-ADBUFFPL-3          
250100                                          FR-TAB-ADBUFFPL-3-VCOM          
250200                                          FR-LASER-ADBUFFPL-3             
250300                                          PR-LASER-ADBUFFPL-3             
250400            WHEN OTHER                                                    
250500             CONTINUE                                                     
250600           END-EVALUATE                                                   
250700         END-IF                                                           
250800                                                                          
250900         IF  WS-ANT-GODK-ADBUFF < K-MAX-REDIG-ADBUFF                      
251000           PERFORM IMS-GNP-ARTD-SALDO                                     
251100         END-IF                                                           
251200       END-PERFORM                                                        
251300     END-IF                                                               
251400     .                                                                    
251500     EJECT                                                                
251600 BC-RED-FARLIG  SECTION.                                                  
251700                                                                          
251800     EVALUATE INLA-ART-KDFARLIG                                           
251900        WHEN 4                                                            
252000         MOVE INLA-ART-IDDC     TO WS-IDDC                                
252100                                                                          
252200        WHEN 5                                                            
252300           MOVE 'ASBEST'      TO FR-TAB-KDFARLIG-TEXT-L                   
252400           MOVE 'ASBEST'      TO FR-NOVA-TAB-KDFARLIG-TEXT-L              
252500                                 FR-TAB-KDFARLIG-TEXT-L-VCOM              
252600                                 PR-LASER-KDFARLIG-TEXT-L                 
252700        WHEN 6                                                            
252800         MOVE INLA-ART-IDDC     TO WS-IDDC                                
252900         IF CDC-SE                                                        
253000           MOVE 'KEMIKALIER'  TO                                          
253100                                 FR-LASER-KDFARLIG-TEXT-L                 
253200                                 FR-NOVA-TAB-KDFARLIG-TEXT-L              
253300                                 FR-TAB-KDFARLIG-TEXT-L                   
253400         ELSE                                                             
253500           MOVE 'CHEMICALS '  TO                                          
253600                                 PR-LASER-KDFARLIG-TEXT-L                 
253700                                 FR-NOVA-TAB-KDFARLIG-TEXT-L              
253800                                 FR-TAB-KDFARLIG-TEXT-L                   
253900         END-IF                                                           
254000        WHEN 7                                                            
254100         MOVE INLA-ART-IDDC     TO WS-IDDC                                
254200                                                                          
254300     END-EVALUATE                                                         
254400     .                                                                    
254500     EJECT                                                                
254600 BD-BEH-EMB-RAD SECTION.                                                  
254700                                                                          
254800     MOVE NEJ                    TO SW-EMB-INFO-REDIGERAD                 
254900                                                                          
255000     IF  SW-ARTC11-C1-FINNS = JA                                          
255100                                                                          
255200       IF  WS-EMB-IDARTNR-EMBQ > ZERO                                     
255300         MOVE WS-EMB-IDARTNR-EMBQ TO W-IDARTNR                            
255400         PERFORM IMS-GU-ARTC01-EMB                                        
255500                                                                          
255600         IF  SEGMENT-FINNS                                                
255700                                                                          
255800           MOVE WS-EMB-IDARTNR-EMBQ TO W-WDD3BSEQ-IDARTNR                 
255900           PERFORM IMS-GU-BENA-TEXT                                       
256000           IF  SEGMENT-FINNS                                              
256100             MOVE BENA-TEXT-BEART TO SPAR-BEART                           
256200           END-IF                                                         
256300                                                                          
256400           MOVE W-IDDC  TO WS-IDDC                                        
256500           IF CDC-TR                                                      
256600             PERFORM IMS-GU-ARTS11-EMB                                    
256700             IF  SEGMENT-FINNS                                            
256800               MOVE ARTS11-SLAG-ADLAGOMR TO SPAR-ADLAGOMR                 
256900               MOVE ARTS11-SLAG-ADGANG  TO SPAR-ADGANG                    
257000               MOVE ARTS11-SLAG-ADPLATS TO SPAR-ADPLATS                   
257100             END-IF                                                       
257200           ELSE                                                           
257300             PERFORM IMS-GNP-ARTC11-EMB                                   
257400             IF  SEGMENT-FINNS                                            
257500               IF MID-FLSVS = JA                                          
257600                 MOVE WS-ARTC11-ADLAGOMR-SVS TO SPAR-ADLAGOMR             
257700                 MOVE WS-ARTC11-ADGANG-SVS   TO SPAR-ADGANG               
257800                 MOVE WS-ARTC11-ADPLATS-SVS  TO SPAR-ADPLATS              
257900               ELSE                                                       
258000                 MOVE ARTC11-CLAG-ADLAGOMR   TO SPAR-ADLAGOMR             
258100                 MOVE ARTC11-CLAG-ADGANG     TO SPAR-ADGANG               
258200                 MOVE ARTC11-CLAG-ADPLATS    TO SPAR-ADPLATS              
258300               END-IF                                                     
258400             END-IF                                                       
258500           END-IF                                                         
258600           MOVE WS-EMB-IDARTNR-EMBQ TO SPAR-IDARTNR-EMBQ                  
258700                                                                          
258800           PERFORM BDA-REDIG-EMB-KVQPACK                                  
258900           MOVE WS-EMB-KVQPACK TO SPAR-KVQPACK                            
259000                                                                          
259100           MOVE WS-EMB-QTYP    TO SPAR-QTYP                               
259200           MOVE JA             TO SW-EMB-INFO-REDIGERAD                   
259300         END-IF                                                           
259400       END-IF                                                             
259500                                                                          
259600       IF  SW-EMB-INFO-REDIGERAD = NEJ                                    
259700                                                                          
259800         IF  WS-EMB-IDARTNR-EMBQ > ZERO                                   
259900         OR  WS-EMB-KVQPACK    > ZERO                                     
260000                                                                          
260100           IF  WS-EMB-IDARTNR-EMBQ < 100                                  
260200             MOVE WS-EMB-IDARTNR-EMBQ TO SPAR-IDARTNR-EMBQ                
260300                                                                          
260400             PERFORM BDA-REDIG-EMB-KVQPACK                                
260500             MOVE WS-EMB-KVQPACK TO SPAR-KVQPACK                          
260600                                                                          
260700             MOVE WS-EMB-QTYP    TO SPAR-QTYP                             
260800           END-IF                                                         
260900         END-IF                                                           
261000       END-IF                                                             
261100                                                                          
261200     ELSE                                                                 
261300*      -- ARTC11-C1 SAKNAS                                                
261400                                                                          
261500       IF  WS-EMB-KVQPACK    > ZERO                                       
261600                                                                          
261700         PERFORM BDA-REDIG-EMB-KVQPACK                                    
261800         MOVE WS-EMB-KVQPACK TO SPAR-KVQPACK                              
261900                                                                          
262000         MOVE WS-EMB-QTYP    TO SPAR-QTYP                                 
262100       END-IF                                                             
262200     END-IF                                                               
262300     .                                                                    
262400     EJECT                                                                
262500 BDA-REDIG-EMB-KVQPACK SECTION.                                           
262600                                                                          
262700*Ändrade regler för utskrift av KVQPACK, tidigare behandlades             
262800*KVQPACK = ZERO som om den vore +1. Ändrat av Wolfgang Kux 940503.        
262900     IF  WS-EMB-KVQPACK = ZERO                                            
263000       CONTINUE                                                           
263100     ELSE                                                                 
263200       DIVIDE WS-KVAVIS-EJ-KIT BY WS-EMB-KVQPACK                          
263300           GIVING WS-EMB-KVQPACK                                          
263400           REMAINDER WS-REMAINDER                                         
263500                                                                          
263600       IF  WS-REMAINDER > ZERO                                            
263700         ADD +1                    TO WS-EMB-KVQPACK                      
263800       END-IF                                                             
263900     END-IF                                                               
264000     .                                                                    
264100     EJECT                                                                
264200 BE-RED-DATUM-RAD  SECTION.                                               
264300                                                                          
264400     MOVE DAGENS-DATUM           TO FR-TAB-TIAAMMDD                       
264500     MOVE DAGENS-DATUM           TO FR-NOVA-TAB-TIAAMMDD                  
264600                                    FR-TAB-TIAAMMDD-VCOM                  
264700                                    FR-LASER-TIAAMMDD                     
264800                                    PR-LASER-TIAAMMDD                     
264900     ACCEPT WS-TID               FROM TIME                                
265000     MOVE WS-TID (1:2)           TO FR-TAB-HH                             
265100     MOVE WS-TID (1:2)           TO FR-NOVA-TAB-HH                        
265200                                    FR-TAB-HH-VCOM                        
265300                                    FR-LASER-HH                           
265400                                    PR-LASER-HH                           
265500     MOVE WS-TID (3:2)           TO FR-TAB-MM                             
265600     MOVE WS-TID (3:2)           TO FR-NOVA-TAB-MM                        
265700                                    FR-TAB-MM-VCOM                        
265800                                    FR-LASER-MM                           
265900                                    PR-LASER-MM                           
266000     MOVE WS-TID (5:2)           TO FR-TAB-SS                             
266100     MOVE WS-TID (5:2)           TO FR-NOVA-TAB-SS                        
266200                                    FR-TAB-SS-VCOM                        
266300                                    FR-LASER-SS                           
266400                                    PR-LASER-SS                           
266500     .                                                                    
266600     EJECT                                                                
266700 BF-CALL-W611STYR  SECTION.                                               
266800                                                                          
266900     MOVE INLA-ART-IDDC      TO STYR-IDDC                                 
267000     MOVE INLA-ART-IDARTNR   TO STYR-IDARTNR                              
267100     MOVE INLA-ART-IDFKNGRP  TO STYR-IDFKNGRP                             
267200     MOVE MID-IDLEVNR (IX-POST) TO STYR-IDLEVNR                           
267300     MOVE INLA-ART-BEFT      TO STYR-BEFT                                 
267400                                                                          
267500     CALL W611STYR USING STYR-W611STYR STYR-HANB-PCB STYR-PLAA-PCB        
267600     IF STYR-KDSVAR-OK                                                    
267700       MOVE STYR-ADINLOMR-FP TO FR-NOVA-TAB-ADINLOMR                      
267800       MOVE STYR-ADINLOMR-FP TO FR-TAB-ADINLOMR                           
267900                                FR-TAB-ADINLOMR-VCOM                      
268000                                FR-LASER-ADINLOMR                         
268100                                PR-LASER-ADINLOMR                         
268200     END-IF                                                               
268300     .                                                                    
268400     EJECT                                                                
268500 BG-CALL-W400ARTU       SECTION.                                          
268600* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
268700* ÖVERSÄTTER EN ARTIKELS URSPRUNGSKOD TILL KLARTEXT             *         
268800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
268900     SKIP2                                                                
269000                                                                          
269100     MOVE WS-ARTC11-KDARTURS                                              
269200                         TO ARTU-KDARTURS                                 
269300     MOVE ZERO           TO ARTU-IDDISTR                                  
269400     MOVE W-IDDC         TO ARTU-IDDC                                     
269500                                                                          
269600     CALL W400ARTU USING ARTU-W400ARTU                                    
269700                                                                          
269800     .                                                                    
269900     EJECT                                                                
270000 S01-PRT-OPEN  SECTION.                                                   
270100                                                                          
270200     MOVE '001'                 TO PRT-KDCALL                             
270300     MOVE WS-IDPRTLST           TO PRT-IDPRTLST                           
270400     CALL W006PRT USING PRT-W006PRT                                       
270500                                                                          
270600     IF LASER                                                             
270700       CALL W006PRS1 USING         PRT-SPOOL-OVR                          
270800                                   PRT-OPEN                               
270900                                   WS-IDPRTLST                            
271000                                   ALT-PCB                                
271100                                   PRT-FILLER                             
271200                                   PRT-FILLER                             
271300     ELSE                                                                 
271400       IF PRT-BEPRTLST(1:4) = 'VCOM'                                      
271410         CONTINUE                                                         
271500*        MOVE 'W601Z1SE' TO PRT-IDVCOM                                    
271600*        MOVE 'W601FRA ' TO PRT-IDCPYTXT                                  
271700*        MOVE +120       TO PRC1-KVLRECL                                  
271800*        MOVE 'W006ASCI' TO PRC1-IDVCINIT                                 
271900*        MOVE 'W006PRT ' TO PRC1-TEVCOMST                                 
272000*        MOVE SPACE      TO PRC1-DATA                                     
272100                                                                          
272200*        CALL W006PRC1 USING         PRT-VCOM                             
272300*                                    PRT-OPEN                             
272400*                                    WS-IDPRTLST                          
272500*                                    ALT-PCB                              
272600*                                    PRC1-W006PRVC                        
272700       ELSE                                                               
272800         CALL W006PRS1 USING         PRT-SPOOL-OVR                        
272900                                     PRT-OPEN                             
273000                                     WS-IDPRTLST                          
273100                                     ALT-PCB                              
273200                                     PRT-FILLER                           
273300                                     PRT-FILLER                           
273400       END-IF                                                             
273500     END-IF                                                               
273600     .                                                                    
273700     SKIP3                                                                
273800 S02-PRT-WRITE SECTION.                                                   
273900                                                                          
274000     IF LASER                                                             
274100       CALL W006PRS1 USING         PRT-SPOOL-OVR                          
274200                                   PRT-WRITE                              
274300                                   WS-IDPRTLST                            
274400                                   ALT-PCB                                
274500                                   PRT-RADSKIP                            
274600                                   FR-LIST-RAD                            
274700     ELSE                                                                 
274800       IF PRT-BEPRTLST(1:4) = 'VCOM'                                      
274810         CONTINUE                                                         
274900*        MOVE FR-LIST-RAD TO PRC1-DATA                                    
275000*        CALL W006PRC1 USING         PRT-VCOM                             
275100*                                    PRT-WRITE                            
275200*                                    WS-IDPRTLST                          
275300*                                    ALT-PCB                              
275400*                                    PRC1-W006PRVC                        
275500       ELSE                                                               
275600         CALL W006PRS1 USING         PRT-SPOOL-OVR                        
275700                                     PRT-WRITE                            
275800                                     WS-IDPRTLST                          
275900                                     ALT-PCB                              
276000                                     PRT-RADSKIP                          
276100                                     FR-LIST-RAD                          
276200       END-IF                                                             
276300     END-IF                                                               
276400     .                                                                    
276500     SKIP3                                                                
276600 S03-PRT-CLOSE SECTION.                                                   
276700                                                                          
276800     IF LASER                                                             
276900       CALL W006PRS1 USING         PRT-SPOOL-OVR                          
277000                                   PRT-CLOSE                              
277100                                   WS-IDPRTLST                            
277200                                   ALT-PCB                                
277300                                   PRT-FILLER                             
277400                                   PRT-FILLER                             
277500     ELSE                                                                 
277600       IF PRT-BEPRTLST(1:4) = 'VCOM'                                      
277610         CONTINUE                                                         
277700*        CALL W006PRC1 USING         PRT-VCOM                             
277800*                                    PRT-CLOSE                            
277900*                                    WS-IDPRTLST                          
278000*                                    ALT-PCB                              
278100*                                    PRC1-W006PRVC                        
278200       ELSE                                                               
278300         CALL W006PRS1 USING         PRT-SPOOL-OVR                        
278400                                     PRT-CLOSE                            
278500                                     WS-IDPRTLST                          
278600                                     ALT-PCB                              
278700                                     PRT-FILLER                           
278800                                     PRT-FILLER                           
278900       END-IF                                                             
279000     END-IF                                                               
279100     .                                                                    
279200     EJECT                                                                
279300 S04-PRT-KONTROLL SECTION.                                                
279400                                                                          
279500      IF WS-IDPRTLST (1:2) = '6L'                                         
279600        MOVE JA TO LASER-SW                                               
279700      END-IF                                                              
279800      .                                                                   
279900      EJECT                                                               
280000* --- IMS SEKTIONER ---                                                   
280100     SKIP3                                                                
280200 IMS-GET-MSG SECTION.                                                     
280300                                                                          
280400     MOVE '  QC' TO GODK-STATUSKODER                                      
280500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
280600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
280700     PERFORM IMS-STATUSKONTROLL                                           
280800     .                                                                    
280900     EJECT                                                                
281000 IMS-GU-INLA-ART SECTION.                                                 
281100     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
281200          DELIMITED BY SIZE INTO SSA1                                     
281300     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
281400          DELIMITED BY SIZE INTO SSA2                                     
281500     MOVE '  ' TO GODK-STATUSKODER                                        
281600     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA2 SSA1 SSA2                
281700     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
281800     PERFORM IMS-STATUSKONTROLL                                           
281900     .                                                                    
282000     EJECT                                                                
282100 IMS-GU-ARTC01 SECTION.                                                   
282200                                                                          
282300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
282400          DELIMITED BY SIZE INTO SSA1                                     
282500     MOVE '  ' TO GODK-STATUSKODER                                        
282600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
282700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
282800     PERFORM IMS-STATUSKONTROLL                                           
282900     .                                                                    
283000     SKIP3                                                                
283100 IMS-GU-ARTC01-EMB SECTION.                                               
283200                                                                          
283300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
283400          DELIMITED BY SIZE INTO SSA1                                     
283500     MOVE '  GE' TO GODK-STATUSKODER                                      
283600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
283700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
283800     PERFORM IMS-STATUSKONTROLL                                           
283900     .                                                                    
284000     SKIP3                                                                
284100 IMS-GNP-ARTC11 SECTION.                                                  
284200                                                                          
284300     MOVE 'WLARTC11 ' TO SSA1                                             
284400     MOVE '  ' TO GODK-STATUSKODER                                        
284500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
284600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
284700     PERFORM IMS-STATUSKONTROLL                                           
284800     .                                                                    
284900     EJECT                                                                
285000 IMS-GNP-ARTC11-EMB SECTION.                                              
285100                                                                          
285200     MOVE 'WLARTC11 ' TO SSA1                                             
285300     MOVE '  GE' TO GODK-STATUSKODER                                      
285400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
285500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
285600     PERFORM IMS-STATUSKONTROLL                                           
285700     .                                                                    
285800     EJECT                                                                
285900 IMS-GU-ARTD-ART SECTION.                                                 
286000                                                                          
286100     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
286200             DELIMITED BY SIZE INTO SSA1                                  
286300     MOVE '  GE' TO GODK-STATUSKODER                                      
286400     CALL CBLTDLI USING GU ARTD-PCB DLI-IO-AREA SSA1                      
286500     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
286600     PERFORM IMS-STATUSKONTROLL                                           
286700     .                                                                    
286800     SKIP3                                                                
286900 IMS-GNP-ARTD-SALDO SECTION.                                              
287000                                                                          
287100     STRING 'WLARTD11(IDDC     =' W-IDDC-X ')'                            
287200              DELIMITED BY SIZE INTO SSA1                                 
287300     MOVE '  GE' TO GODK-STATUSKODER                                      
287400     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA SSA1                     
287500     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
287600     PERFORM IMS-STATUSKONTROLL                                           
287700     .                                                                    
287800     EJECT                                                                
287900 IMS-GU-ARTS11-EMB SECTION.                                               
288000                                                                          
288100     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
288200             DELIMITED BY SIZE INTO SSA1                                  
288300     STRING 'WLARTS11(IDDC     =' W-IDDC-91-X ')'                         
288400              DELIMITED BY SIZE INTO SSA2                                 
288500     MOVE '  GE' TO GODK-STATUSKODER                                      
288600     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1 SSA2                 
288700     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
288800     PERFORM IMS-STATUSKONTROLL                                           
288900     .                                                                    
289000     EJECT                                                                
289100 IMS-GU-BENA-TEXT SECTION.                                                
289200                                                                          
289300     STRING 'WLBENA01(WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
289400          DELIMITED BY SIZE INTO SSA1                                     
289500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
289600          DELIMITED BY SIZE INTO SSA2                                     
289700     MOVE '  GE' TO GODK-STATUSKODER                                      
289800     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
289900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
290000     PERFORM IMS-STATUSKONTROLL                                           
290100     .                                                                    
290200     EJECT                                                                
290210 IMS-GU-WDK712 SECTION.                                                   
290220     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
290230          DELIMITED BY SIZE INTO SSA1                                     
290240     STRING 'WDK712  (IDLAND   =' W-IDLAND ')'                            
290250          DELIMITED BY SIZE INTO SSA2                                     
290260     MOVE '  GE' TO GODK-STATUSKODER                                      
290270     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
290280     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
290290     PERFORM IMS-STATUSKONTROLL                                           
290291     .                                                                    
290292     EJECT                                                                
290300 IMS-STATUSKONTROLL SECTION.                                              
290400                                                                          
290500     SET STATUS-IX TO 1                                                   
290600     SEARCH GODK-STATUS                                                   
290700       AT END                                                             
290800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
290900         DELIMITED BY SIZE INTO FELTEXT                                   
291000         CALL FELLOG                                                      
291100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
291200         CONTINUE                                                         
291300     END-SEARCH                                                           
291400     .                                                                    
