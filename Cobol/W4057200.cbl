000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W4057200.                                                
000500 AUTHOR.         LENNART LUNDGREN ADB-GRUPPEN.                            
000600 DATE-WRITTEN.   SEPT 1988.                                               
000700                                                                          
000800*   REMARKS.                                                              
000900*                                                                         
001000*    FUNKTION.                                                            
001100*                                                                         
001200*     BESKRIVNING                                                         
001300*                                                                         
001400*     DETTA PROGRAM HANTERAR BILD 4572 RO/TPO 2                           
001500*     PROGRAMMET TILLÅTER LÄSNING, BORTTAG, HÖJNING AV PRIO,              
001600*     FÖRÄNDRING OCH DELNING AV RO RADER MED KDSTARAD < = 2               
001700*     I PROGRAMMET ANVÄNDS FÖLJANDE BASER:                                
001800*                                                                         
001900*     ROREG.                                                              
002000*     KUNDREG.                                                            
002100*     ARTREG.                                                             
002200*     STYRREG.                                                            
002300*     LOGGREG.                                                            
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T572                                              
002700*        MID:         W4I57201                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W4O57201                                            
003100* ETRACK 1290414 INTERVALL FOR DISTRICT AND CUSTOMER                      
003200* ETRACK 7450328 2008-HÖST  VOHF                                          
003300* ETRACK 10254592     2015  DECOMISSION VOHF                              
003400* ETRACK 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2             
003500* STORY 2373879 / REPLACE DIST35- 88 LEVELS OF IN,KR,                     
003600*  AE,CN CDC RETURNS TO DIST35-CDC-RETURNS-NON-VCC                        
003700     EJECT                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900     SKIP2                                                                
004000 DATA DIVISION.                                                           
004100     SKIP2                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300*    -COPY WY2000W3                                                       
004400     SKIP3                                                                
004500 77  IDPGM                     PIC X(8)     VALUE 'W4057200'.             
004600 77  FELTEXT                   PIC X(80)    VALUE SPACE.                  
004700 77  RKOD-ABEND                PIC S9(4)    VALUE +33 COMP SYNC.          
004800 77  JA                        PIC X(1)     VALUE 'J'.                    
004900 77  NEJ                       PIC X(1)     VALUE 'N'.                    
005000 77  W-ENTER                   PIC X(1)     VALUE SPACE.                  
005100 77  W-KVRAD                   PIC 9(6)     VALUE ZERO.                   
005200 77  W-PUNKT                   PIC 9(1)     VALUE ZERO.                   
005300 77  W-ANTAL-1                 PIC 9(6)V    VALUE ZERO.                   
005400 77  W-ANTAL-2                 PIC 9(6)V    VALUE ZERO.                   
005500 77  W-VALKOD                  PIC X(1)     VALUE ' '.                    
005600 77  W-OPPNAD                  PIC X(1)     VALUE 'N'.                    
005700 77  W-NEWKEY                  PIC X(1)     VALUE ' '.                    
005800 77  W-FAELT-IFYLLT            PIC X(1)     VALUE 'N'.                    
005900 77  W-DELNING                 PIC X(1)     VALUE 'N'.                    
006000 77  W-UPDATE                  PIC X(1)     VALUE 'N'.                    
006100 77  KVQPACK-OK                PIC X(1)     VALUE 'J'.                    
006200 77  FRAKT-OK                  PIC X(1)     VALUE 'J'.                    
006300 77  ANTAL-OK                  PIC X(1)     VALUE 'J'.                    
006400 77  KLASS-OK                  PIC X(1)     VALUE 'J'.                    
006500 77  PRIS-OK                   PIC X(1)     VALUE 'J'.                    
006600 77  INPUT-OK                  PIC X(1)     VALUE 'J'.                    
006700 77  VALKOD-OK                 PIC X(1)     VALUE 'J'.                    
006800 77  AENDRA-ENTER              PIC X(1)     VALUE 'J'.                    
006900 77  OPP-FAELT-OK              PIC X(1)     VALUE 'J'.                    
007000 77  IDDISTR-WS                PIC X(4)     VALUE SPACE.                  
007100 77  IDKUNDNR-WS               PIC X(6)     VALUE SPACE.                  
007200 77  IDARTNR-WS                PIC X(9)     VALUE SPACE.                  
007300 77  KDORDKL-WS                PIC X(1)     VALUE SPACE.                  
007400 77  KDPRODSL-WS               PIC X(2)     VALUE SPACE.                  
007500 77  IDLOPNR-WS                PIC X(2)     VALUE SPACE.                  
007600 77  IDORDNR-WS                PIC X(5)     VALUE SPACE.                  
007700 77  KDTPOTYP-WS               PIC X(1)     VALUE SPACE.                  
007800 77  IDDC-WS                   PIC X(2)     VALUE SPACE.                  
007900 77  INDX                      PIC S9(9)    VALUE ZERO  COMP SYNC.        
008000 77  SPRAK-IX                  PIC S9(9)    VALUE ZERO  COMP SYNC.        
008100 77  MAX-LINES                 PIC S9(9)    VALUE +13   COMP SYNC.        
008200                                                                          
008300 77  SPAR-PRARTNTO             PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
008400 77  SPAR-KVART                PIC 9(7).                                  
008500 77  W-KVART                   PIC 9(7).                                  
008600 77  W-IDDISTR                 PIC 9(4)      VALUE ZERO.                  
008700 77  W-IDKUNDNR                PIC 9(6)      VALUE ZERO.                  
008800 77  W-IDARTNR                 PIC 9(9)      VALUE ZERO.                  
008900 77  W-KDORDKL                 PIC 9(1)      VALUE ZERO.                  
009000 77  W-KDPRODSL                PIC 9(3)      VALUE ZERO.                  
009100 77  W-IDORDNR                 PIC 9(5)      VALUE ZERO.                  
009200 77  W-KDTPOTYP                PIC 9(1)      VALUE ZERO.                  
009300 77  W-IDDC                    PIC X(2)      VALUE ZERO.                  
009400 77  KVART-OPP                 PIC 9(7).                                  
009500 77  KDORDKL-OPP               PIC 9(1)      VALUE ZERO.                  
009600 77  PRARTNTO-OPP              PIC S9(7)V9(2) VALUE ZERO.                 
009700 77  KDFRAKT-OPP               PIC 9(2)      VALUE ZERO.                  
009800                                                                          
009900 77  DATUM-MED-ARHUNDR         PIC 9(8)      VALUE ZERO.                  
010000                                                                          
010100 77  W-TITPO-TIAAVV            PIC 9(4)      VALUE ZERO.                  
010200 77  W-DAGENS-DAT-TIAAVV       PIC 9(4)      VALUE ZERO.                  
010300                                                                          
010400 77  WS-IDTRANS                PIC X(4).                                  
010500     88  WS-GODKAEND-BILD      VALUE '4571' '4572' '4573'.                
010600     88  EGEN-MID              VALUE '4572'.                              
010700 01 DB2-LASNING.                                                          
010800     03 FILLER                   PIC X(16)   VALUE                        
010900                                             'WS-DB2-SEKTION'.            
011000     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
011100                                                                          
011200                                                                          
011300 01 NYCKLAR-TP4TRAN.                                                      
011400     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
011500                                                                          
011600                                                                          
011700 77  FRYSTID-SW                PIC X         VALUE 'N'.                   
011800     88  BORTOM-FRYSTID                      VALUE 'J'.                   
011900     88  INOM-FRYSTID                        VALUE 'N'.                   
012000                                                                          
012100 77  TPO4-SW                   PIC X         VALUE 'N'.                   
012200     88  TPO4                                VALUE 'J'.                   
012300                                                                          
012400 01  W-DABEHOV.                                                           
012500     03  W-DABEHOV-SEKEL       PIC 9(2)      VALUE ZERO.                  
012600     03  W-DABEHOV-AAVV        PIC 9(4)      VALUE ZERO.                  
012700                                                                          
012800 01  DEBUGG-GRP.                                                          
012900     03  DEBUGG-FLT            PIC 999       VALUE ZERO.                  
013000     03  FILLER                PIC X         VALUE SPACE.                 
013100     03  DEBUGG-IDKUNDNR       PIC 9(7)      VALUE ZERO.                  
013200     03  FILLER                PIC X         VALUE SPACE.                 
013300     03  DEBUGG-IDARTNR        PIC 9(9)      VALUE ZERO.                  
013400                                                                          
013500     SKIP3                                                                
013600*      --- VALID IDDC CODES                                               
013700                                                                          
013800*01    -COPY WWDCKONS                                                     
013900                                                                          
014000*01    -COPY WWBYT03                                                      
014100       EJECT                                                              
014200                                                                          
014300 01  W-SPAR-RAD.                                                          
014400                                                                          
014500     03  W-SPAR-VALKOD         PIC X        VALUE SPACE.                  
014600     03  W-SPAR-IDKUNDNR       PIC 9(6)     VALUE ZERO.                   
014700     03  W-SPAR-IDARTNR        PIC 9(9)     VALUE ZERO.                   
014800     03  WW-SPAR-KVART         PIC 9(7).                                  
014900     03  W-SPAR-IDORDNR        PIC 9(5)     VALUE ZERO.                   
015000     03  W-SPAR-KDORDKL        PIC 9(1)     VALUE ZERO.                   
015100     03  W-SPAR-KDFRAKT        PIC 9(2)     VALUE ZERO.                   
015200     03  W-SPAR-TIRODAT        PIC 9(6)     VALUE ZERO.                   
015300     03  W-SPAR-PRARTNTO       PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
015400     03  W-SPAR-PRAVCOST       PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
015500     03  W-SPAR-PRARTNTO-LOC   PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
015600     03  W-SPAR-PRARTNTO-LOCPREL PIC S9(7)V9(2) COMP-3 VALUE ZERO.        
015700     03  W-SPAR-KVQPACK-1      PIC 9(6)     VALUE ZERO.                   
015800     03  W-SPAR-KDRAPRIO       PIC 9(3)     VALUE ZERO.                   
015900     03  W-SPAR-IDLOPNR        PIC 9(2)     VALUE ZERO.                   
016000 01  W-SPAR-KVART              PIC 9(7).                                  
016100 01  W-PRARTNTO.                                                          
016200     03  WW-SPAR-PRARTNTO      PIC X(10)    VALUE ZERO.                   
016300                                                                          
016400 01  REDIGERADE-OPPRAD-9.                                                 
016500                                                                          
016600     03 W9-UT-KVART-OPP        PIC Z(6)9.                                 
016700     03 W9-UT-KDORDKL-OPP      PIC 9.                                     
016800     03 W9-UT-KDFRAKT-OPP      PIC Z9.                                    
016900     03 W9-UT-PRARTNTO-OPP     PIC Z(9)9.                                 
017000                                                                          
017100 01  W-IDKUNDRF.                                                          
017200     03  W-IDKUNDRF-1-5        PIC 9(5).                                  
017300     03  FILLER                PIC X(5) VALUE SPACE.                      
017400     EJECT                                                                
017500                                                                          
017600 01  NYCKLAR-TILL-DLI.                                                    
017700                                                                          
017800     03  W-SOK-NYCKLAR-MIN.                                               
017900         05  W-IDARTNR-MIN-X.                                             
018000             07  W-IDARTNR-MIN    PIC S9(9)  COMP-3 VALUE ZERO.           
018100         05  W-IDKUNDRF-MIN.                                              
018200             07  W-IDORDNR-MIN    PIC 9(5)          VALUE ZERO.           
018300             07  FILLER           PIC X(5)          VALUE SPACE.          
018400         05  W-IDLOPNR-MIN-X.                                             
018500             07  W-IDLOPNR-MIN    PIC S9(3)  COMP-3 VALUE ZERO.           
018600         05  W-KDORDKL-MIN-X.                                             
018700             07  W-KDORDKL-MIN    PIC S9(1)  COMP-3 VALUE ZERO.           
018800         05  W-KDPRODSL-MIN-X.                                            
018900             07  W-KDPRODSL-MIN   PIC S9(3)  COMP-3 VALUE ZERO.           
019000         05  W-KDSTARAD-MIN       PIC  X(1)         VALUE SPACE.          
019100         05  W-KDTPOTYP-MIN-X.                                            
019200             07  W-KDTPOTYP-MIN   PIC S9(1)  COMP-3 VALUE ZERO.           
019300         05  W-IDDC-MIN           PIC  X(2)         VALUE SPACE.          
019400                                                                          
019500     03  W-SOK-NYCKLAR-MAX.                                               
019600         05  W-IDARTNR-MAX-X.                                             
019700             07  W-IDARTNR-MAX    PIC S9(9)  COMP-3  VALUE ZERO.          
019800         05  W-IDKUNDRF-MAX.                                              
019900             07  W-IDORDNR-MAX    PIC 9(5)           VALUE ZERO.          
020000             07  FILLER           PIC X(5)           VALUE SPACE.         
020100         05  W-IDLOPNR-MAX-X.                                             
020200             07  W-IDLOPNR-MAX    PIC S9(3)  COMP-3  VALUE ZERO.          
020300         05  W-KDORDKL-MAX-X.                                             
020400             07  W-KDORDKL-MAX    PIC S9(1)  COMP-3  VALUE ZERO.          
020500         05  W-KDPRODSL-MAX-X.                                            
020600             07  W-KDPRODSL-MAX   PIC S9(3)  COMP-3  VALUE ZERO.          
020700         05  W-KDSTARAD-MAX       PIC  X(1)          VALUE '2'.           
020800         05  W-KDTPOTYP-MAX-X.                                            
020900             07  W-KDTPOTYP-MAX   PIC S9(1)  COMP-3  VALUE ZERO.          
021000         05  W-IDDC-MAX           PIC  X(2)          VALUE SPACE.         
021100                                                                          
021200     03  W-IDARTNR-X.                                                     
021300         05  W-IDARTNR-N          PIC S9(9) COMP-3   VALUE ZERO.          
021400                                                                          
021500     03  W-WDK711-IDARTNR-X.                                              
021600         05  W-WDK711-IDARTNR-N   PIC S9(9) COMP-3   VALUE ZERO.          
021700                                                                          
021800     03  W-WDK711-IDDC-X.                                                 
021900         05  W-WDK711-IDDC        PIC X(2)           VALUE ZERO.          
022000                                                                          
022100     03  W-DABEHOV-X.                                                     
022200         05  W-DABEHOV-N          PIC 9(6)  VALUE ZERO.                   
022300                                                                          
022400     03  W-WDGX2223-X.                                                    
022500         05  W-IDHTYP             PIC X(4)           VALUE '2223'.        
022600         05  W-IDANSK-2223        PIC S9(3) COMP-3   VALUE ZERO.          
022700         05  W-LOW-VALUE          PIC X(24) VALUE LOW-VALUE.              
022800                                                                          
022900     03  W-WDGX2224-X.                                                    
023000         05  W-TISENBEK-DAG       PIC S9(7) COMP-3   VALUE ZERO.          
023100         05  W-TISENBEK-KL        PIC S9(7) COMP-3   VALUE ZERO.          
023200         05  W-KDLARM             PIC S9(3) COMP-3   VALUE ZERO.          
023300                                                                          
023400     03  W-WDGX2231-X.                                                    
023500         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
023600         05  W-VALFRI-2231       PIC X(26)    VALUE LOW-VALUE.            
023700                                                                          
023800     03  W-WDGX2232-X.                                                    
023900         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
024000         05  W-LOW-VALUE-2232    PIC X(3)     VALUE LOW-VALUE.            
024100                                                                          
024200     03  W-WDA5D1KY-MIN.                                                  
024300         05  W-IDDISTR-N1-MIN     PIC S9(5) COMP-3   VALUE ZERO.          
024400         05  W-IDKUNDNR-N1-MIN    PIC S9(7) COMP-3   VALUE ZERO.          
024500         05  W-IDARTNR-N1-MIN     PIC S9(9) COMP-3   VALUE ZERO.          
024600         05  W-IDKUNDRF-N1-MIN.                                           
024700             07  W-IDORDNR-N1-MIN PIC  9(5)          VALUE ZERO.          
024800             07  FILLER           PIC  X(5)          VALUE SPACE.         
024900         05  W-IDLOPNR-N1-MIN     PIC S9(3) COMP-3   VALUE ZERO.          
025000                                                                          
025100     03  W-WDA5D1KY-MAX.                                                  
025200         05  W-IDDISTR-N1-MAX     PIC S9(5) COMP-3   VALUE ZERO.          
025300         05  W-IDKUNDNR-N1-MAX    PIC S9(7) COMP-3   VALUE ZERO.          
025400         05  W-IDARTNR-N1-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
025500         05  W-IDKUNDRF-N1-MAX.                                           
025600             07  W-IDORDNR-N1-MAX PIC  9(5)          VALUE ZERO.          
025700             07  FILLER           PIC  X(5)          VALUE SPACE.         
025800         05  W-IDLOPNR-N1-MAX     PIC S9(3) COMP-3   VALUE ZERO.          
025900                                                                          
026000     03  FILLER                   PIC X(16)   VALUE 'WDA501KY'.           
026100     03  W-WDA501KY.                                                      
026200         05  W-IDDISTR-N2         PIC S9(5) COMP-3   VALUE ZERO.          
026300         05  W-IDKUNDNR-N2        PIC S9(7) COMP-3   VALUE ZERO.          
026400         05  W-IDKUNDRF-N2.                                               
026500             07  W-IDORDNR-N2     PIC  9(5)          VALUE ZERO.          
026600             07  FILLER           PIC  X(5)          VALUE SPACE.         
026700         05  W-IDARTNR-N2         PIC S9(9) COMP-3   VALUE ZERO.          
026800         05  W-IDLOPNR-N2         PIC S9(3) COMP-3   VALUE ZERO.          
026900                                                                          
027000     03  W-IDARTNR-ARTC01-X.                                              
027100         05  W-IDARTNR-ARTC01     PIC S9(9) COMP-3   VALUE ZERO.          
027200                                                                          
027300     03  W-WDGX01.                                                        
027400                                                                          
027500         05  W-IDHTYP-N5          PIC X(4)           VALUE '4511'.        
027600         05  W-VALFRI-N5          PIC X(26)          VALUE SPACE.         
027700                                                                          
027800     03  W-KDORDKL-N5-X.                                                  
027900         05  W-KDORDKL-N5         PIC S9  COMP-3     VALUE ZERO.          
028000     03  W-KDTPOTYP-N5-X.                                                 
028100         05  W-KDTPOTYP-N5        PIC S9  COMP-3     VALUE ZERO.          
028200                                                                          
028300     03  W-IDDISTR-FOM-N5-X.                                              
028400         05  W-IDDISTR-FOM-N5     PIC S9(5)  COMP-3  VALUE ZERO.          
028500     03  W-IDDISTR-TOM-N5-X.                                              
028600         05  W-IDDISTR-TOM-N5     PIC S9(5)  COMP-3  VALUE ZERO.          
028700                                                                          
028800     03  W-WDGXKEY-N5-MIN.                                                
028900         05  FILLER               PIC X(10)          VALUE SPACE.         
029000                                                                          
029100     03  W-WDGXKEY-N5-MAX.                                                
029200         05  FILLER               PIC X(10)          VALUE SPACE.         
029300                                                                          
029400     03  W-KDRAPRIO-N5-MIN-X.                                             
029500         05  W-KDRAPRIO-N5-MIN    PIC S9(3) COMP-3   VALUE ZERO.          
029600                                                                          
029700     03  W-KDRAPRIO-N5-MAX-X.                                             
029800         05  W-KDRAPRIO-N5-MAX    PIC S9(3) COMP-3   VALUE ZERO.          
029900                                                                          
030000     03  W-WDB201KY-X.                                                    
030100         05  W-IDDISTR-B201       PIC S9(5)  COMP-3  VALUE ZERO.          
030200         05  W-IDKUNDNR-B201      PIC S9(7)  COMP-3  VALUE ZERO.          
030300                                                                          
030400     03 W-WDB201KY-MIN-X.                                                 
030500        05  W-IDDISTR-B201-MIN  PIC S9(5) COMP-3 VALUE ZERO.              
030600        05  W-IDKUNDNR-B201-MIN PIC S9(7) COMP-3 VALUE ZERO.              
030700                                                                          
030800     03 W-WDB201KY-MAX-X.                                                 
030900        05  W-IDDISTR-B201-MAX  PIC S9(5) COMP-3 VALUE ZERO.              
031000        05  W-IDKUNDNR-B201-MAX PIC S9(7) COMP-3 VALUE ZERO.              
031100                                                                          
031200     03  W-WDB101KY-X.                                                    
031300         05  W-WDB1-IDPARTNR     PIC X(9)   VALUE   SPACE.                
031400         05  W-WDB1-IDFTG        PIC 9(2)   VALUE   ZERO.                 
031500                                                                          
031600     03  W-WDB501KY-X.                                                    
031700         05  W-IDDC-WDB5         PIC X(2)    VALUE SPACE.                 
031800         05  W-KDFRAKT-WDB5      PIC S9(3)   VALUE ZERO COMP-3.           
031900         05  W-IDDISTR-WDB5      PIC S9(5)   VALUE ZERO COMP-3.           
032000         05  W-IDKUNDNR-WDB5     PIC S9(7)   VALUE ZERO COMP-3.           
032100     SKIP2                                                                
032200     03  W-IDGMTREF-X.                                                    
032300         05  W-IDDISTR-N9         PIC S9(5) COMP-3   VALUE ZERO.          
032400         05  W-IDKUNDNR-N9        PIC S9(7) COMP-3   VALUE ZERO.          
032500         05  W-IDKUNDRF-N9        PIC X(10) VALUE SPACE.                  
032600                                                                          
032700     03  W-WDQ101KY-MIN-X.                                                
032800         05  W-IDORDER-MIN-N10    PIC S9(7)   VALUE ZERO COMP-3.          
032900         05  W-IDARTNR-MIN-N10    PIC S9(9)   VALUE ZERO COMP-3.          
033000         05  W-IDLOPNR-MIN-N10    PIC S9(3)   VALUE ZERO COMP-3.          
033100         05  W-IDSEKVNR-MIN-N10   PIC S9(3)   VALUE ZERO COMP-3.          
033200         05  W-IDDC-MIN-N10       PIC  X(2)   VALUE SPACE.                
033300         05  W-KDORDBEK-MIN-N10   PIC  9(2)   VALUE ZERO.                 
033400                                                                          
033500     03  W-WDQ101KY-MAX-X.                                                
033600         05  W-IDORDER-MAX-N10   PIC S9(7) VALUE 9999999   COMP-3.        
033700         05  W-IDARTNR-MAX-N10   PIC S9(9) VALUE 999999999 COMP-3.        
033800         05  W-IDLOPNR-MAX-N10   PIC S9(3) VALUE 999       COMP-3.        
033900         05  W-IDSEKVNR-MAX-N10  PIC S9(3) VALUE 999       COMP-3.        
034000         05  W-IDDC-MAX-N10      PIC  X(2) VALUE '99'.                    
034100         05  W-KDORDBEK-MAX-N10  PIC  9(2) VALUE 99.                      
034200                                                                          
034300     03  W-WDM201-X.                                                      
034400         05  W-KAMP-IDKAMPRF      PIC S9(07)   VALUE ZERO COMP-3.         
034500         05  W-KAMP-IDDC          PIC X(02)    VALUE SPACE.               
034600                                                                          
034700     03  W-WDM211-X.                                                      
034800         05  W-KART-IDARTNR       PIC S9(09)   VALUE ZERO COMP-3.         
034900                                                                          
035000     03  W-WDM221-X.                                                      
035100         05  W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.           
035200         05  W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.           
035300         05  W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.           
035400         05  W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.           
035500                                                                          
035600     03  W-IDDC-B6-X.                                                     
035700         05 W-IDDC-B6            PIC X(2).                                
035800                                                                          
035900     EJECT                                                                
036000 01  GENERELLA-SUBPROGRAM.                                                
036100     03  CBLTDLI                  PIC X(8)      VALUE 'CBLTDLI '.         
036200     03  FELLOG                   PIC X(8)      VALUE 'FELLOG  '.         
036300     03  WSECURIT                 PIC X(8)      VALUE 'WSECURIT'.         
036400     03  WDECEDIT                 PIC X(8)      VALUE 'WDECEDIT'.         
036500     03  WDATKONV                 PIC X(8)      VALUE 'WDATKONV'.         
036600     03  WMEDKONV                 PIC X(8)      VALUE 'WMEDKONV'.         
036700     03  W009VADD                 PIC X(8)      VALUE 'W009VADD'.         
036800     03  ABEND                    PIC X(8)      VALUE 'ABEND   '.         
036900     03  W005INIT                 PIC X(8)      VALUE 'W005INIT'.         
037000     EJECT                                                                
037100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
037200*01 -COPY WMSGINIT                                                        
037300     EJECT                                                                
037400*  03    FILLER -COPY WDECAREA.                                           
037500     EJECT                                                                
037600*  03    FILLER -COPY WSECAREA.                                           
037700     EJECT                                                                
037800*   --- PARAMETRAR TILL SUBPROGRAM WDATKONV                               
037900                                                                          
038000*01    FILLER -COPY WDATAREA                                              
038100     EJECT                                                                
038200*   --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                               
038300                                                                          
038400*01    FILLER -COPY WMEDAREA                                              
038500     SKIP2                                                                
038600 01  MESSAGE-CODES.                                                       
038700     03  ERR-INOM-FRYS            PIC X(3)  VALUE '069'.                  
038800     03  ERR-OTILL-UPP            PIC X(3)  VALUE '007'.                  
038900     EJECT                                                                
039000*   --- PARAMETRAR TILL SUBPROGRAM W009VADD                               
039100                                                                          
039200 01  W009VADD-AREA.                                                       
039300     03  VECKO-DATUM-AAVV         PIC S9(5) COMP-3.                       
039400     03  VECKO-ANTAL              PIC S9(3) COMP-3.                       
039500     EJECT                                                                
039600*  ---PARAMETRAR TILL IDDISTR                                             
039700                                                                          
039800 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
039900*01  FILLER -COPY WWDIST07 -RED TEST-IDDISTR.                             
040000     EJECT                                                                
040100*01  FILLER -COPY WWDIST18 -RED TEST-IDDISTR.                             
040200     EJECT                                                                
040300*01  FILLER -COPY WWDIST19 -RED TEST-IDDISTR.                             
040400     EJECT                                                                
040500*01  FILLER -COPY WWDIST35 -RED TEST-IDDISTR.                             
040600     EJECT                                                                
040700*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
040800     EJECT                                                                
040900 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
041000*   -COPY WWDIST57                                                        
041100     EJECT                                                                
041200*  ---COPYTEXT TILL SUC-TRANS                                             
041300     SKIP2                                                                
041400*01    -COPY WDGZSUC                                                      
041500     EJECT                                                                
041600*  ---COPYTEXT TILL RY9-TRANS                                             
041700     SKIP2                                                                
041800*01    -COPY WDGZRY9                                                      
041900     EJECT                                                                
042000*01    -COPY WDGZRY9S                                                     
042100     EJECT                                                                
042200*01    -COPY WWTEXT01                                                     
042300     EJECT                                                                
042400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
042500                                                                          
042600 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
042700     SKIP3                                                                
042800*01    MID -COPY W4I57201.                                                
042900     EJECT                                                                
043000*01    -COPY WMSGAREA                                                     
043100     EJECT                                                                
043200*  03    MOD -COPY W4O57201  -RED MSG-AREA.                               
043300     EJECT                                                                
043400*01    -COPY WMFSAREA                                                     
043500     EJECT                                                                
043600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
043700                                                                          
043800 01    IMS-WS.                                                            
043900   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
044000     SKIP3                                                                
044100*                        **** STATUS-KOD FRÅN IMS                         
044200   03    STATUS-WS               PIC XX.                                  
044300     88    SEGMENT-FINNS                    VALUE '  '.                   
044400     88    SEGMENT-HAR-LAGTS-TILL           VALUE '  '.                   
044500     88    SEGMENT-FINNS-REDAN              VALUE 'II'.                   
044600     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
044700     88    SEGMENT-SLUT                     VALUE 'GB'.                   
044800     SKIP3                                                                
044900   03    GODK-STATUSKODER.                                                
045000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
045100     SKIP3                                                                
045200 01    SSA1                      PIC X(320).                              
045300     SKIP3                                                                
045400 01    SSA2                      PIC X(320).                              
045500     SKIP3                                                                
045600 01    SSA3                      PIC X(320).                              
045700     EJECT                                                                
045800*                            DB2 FUNKTIONSKODER                           
045900 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
046000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
046100                                                                          
046200 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
046300 01  DB2-WS.                                                              
046400     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
046500         88  CURSOR-OK                       VALUE 000.                   
046600         88  RADER-FINNS                     VALUE 000.                   
046700         88  RADER-SAKNAS                    VALUE 100.                   
046800         88  ATKOMST-FEL                     VALUE 904.                   
046900     03  GODK-SQLCODEKODER.                                               
047000         05  GODK-SQLCODE OCCURS 5                                        
047100             INDEXED BY SQLCODE-IX PIC 9(3).                              
047200 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
047300     EJECT                                                                
047400*                            IMS FUNKTIONSKODER                           
047500*01    -COPY W0003                                                        
047600     EJECT                                                                
047700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA501'.         
047800 01  DLI-IO-WDA501.                                                       
047900*  03  WDA501 -COPY WDA501                                                
048000     EJECT                                                                
048100                                                                          
048200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA5D1'.         
048300 01  DLI-IO-WDA5D1.                                                       
048400*  03  WDA5D1 -COPY WDA5D1                                                
048500     EJECT                                                                
048600                                                                          
048700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
048800 01  DLI-IO-WDK601.                                                       
048900*  03    WDK601 -COPY WDK601                                              
049000                                                                          
049100     EJECT                                                                
049200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
049300 01  DLI-IO-WDK611.                                                       
049400*  03    WDK611 -COPY WDK611                                              
049500                                                                          
049600     EJECT                                                                
049700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
049800 01  DLI-IO-WDK711.                                                       
049900*  03    WDK711 -COPY WDK711                                              
050000     EJECT                                                                
050100                                                                          
050200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-4512  '.         
050300 01  DLI-IO-4512.                                                         
050400*  03    WDGX45 -COPY WDGX4512 -PRE STYR-                                 
050500     EJECT                                                                
050600                                                                          
050700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-ZZAC01'.         
050800 01  DLI-IO-ZZAC01.                                                       
050900*  03    WDGZ01 -COPY WDGZ01 -PRE ZZAC-                                   
051000     EJECT                                                                
051100                                                                          
051200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB501'.         
051300 01  DLI-IO-WDB501.                                                       
051400*  03    WDB501 -COPY WDB501                                              
051500     EJECT                                                                
051600                                                                          
051700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK901'.         
051800 01    DLI-IO-WDK901.                                                     
051900*  03    WDK901 -COPY WDK901                                              
052000     EJECT                                                                
052100                                                                          
052200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK911'.         
052300 01  DLI-IO-WDK911.                                                       
052400*  03    WDK911 -COPY WDK911                                              
052500     EJECT                                                                
052600                                                                          
052700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-2224  '.         
052800 01  DLI-IO-2224.                                                         
052900*  03    WDGX   -COPY WDGX2224 -PRE XXBU-                                 
053000     EJECT                                                                
053100                                                                          
053200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB201'.         
053300 01  DLI-IO-WDB201.                                                       
053400*  03    WDB201 -COPY WDB201                                              
053500     EJECT                                                                
053600                                                                          
053700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB101'.         
053800 01  DLI-IO-WDB101.                                                       
053900*  03    WDB101 -COPY WDB101                                              
054000     EJECT                                                                
054100                                                                          
054200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDQ201'.         
054300 01    DLI-IO-WDQ201.                                                     
054400*  03    WDQ201 -COPY WDQ201 -PRE ORQI-                                   
054500     EJECT                                                                
054600                                                                          
054700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ISRT-Q101'.        
054800 01    DLI-IO-ISRT-Q101.                                                  
054900*  03    WDQ101 -COPY WDQ101 -PRE ORQM-                                   
055000     EJECT                                                                
055100                                                                          
055200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-GU-Q101  '.        
055300 01    DLI-IO-GU-Q101.                                                    
055400*  03    WDQ101 -COPY WDQ101 -PRE ORQM2-                                  
055500     EJECT                                                                
055600                                                                          
055700 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
055800 01  DLI-IO-WDM211.                                                       
055900*    03 -COPY WDM211                                                      
056000     EJECT                                                                
056100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
056200 01  DLI-IO-WDM221.                                                       
056300*    03 -COPY WDM221                                                      
056400     EJECT                                                                
056500                                                                          
056600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-2232  '.           
056700 01    DLI-IO-2232.                                                       
056800*  03    WDGX2232 -COPY WDGX2232 -PRE XXBX-                               
056900     EJECT                                                                
057000                                                                          
057100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
057200 01   DLI-IO-AREA-B601.                                                   
057300*     03  -COPY WDB601                                                    
057400                                                                          
057500*    MSG-AREA FÖR HOPP TILL W20109                                        
057600 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
057700 01  W-PROG-TO-PROG-SW-1.                                                 
057800     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
057900     03  2109-Z1                   PIC X.                                 
058000     03  2109-Z2                   PIC X.                                 
058100     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
058200     03  2109-IDTRANS              PIC X(4)  VALUE '4572'.                
058300     03  2109-KDMFSFOR             PIC X.                                 
058400*    03  -COPY W2I10902    -PRE 2109-                                     
058500     EJECT                                                                
058600 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
058700                                                                          
058800*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
058900     EJECT                                                                
059000     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
059100     EJECT                                                                
059200 LINKAGE SECTION.                                                         
059300*01    -COPY W0009     -PRE MSG-                                          
059400     EJECT                                                                
059500*01    -COPY W0009     -PRE 2109-                                         
059600     EJECT                                                                
059700*01    -COPY W0008     -PRE USEA-                                         
059800     05  FILLER                  PIC X.                                   
059900     EJECT                                                                
060000*01    -COPY W0008     -PRE ORDP-                                         
060100     05  FILLER                  PIC X.                                   
060200     EJECT                                                                
060300*01    -COPY W0008     -PRE ORDT-                                         
060400     05  FILLER                  PIC X.                                   
060500     EJECT                                                                
060600*01    -COPY W0008     -PRE ZZAC-                                         
060700     05  FILLER                  PIC X.                                   
060800     EJECT                                                                
060900*01    -COPY W0008     -PRE ART-                                          
061000     05  FILLER                  PIC X.                                   
061100     EJECT                                                                
061200*01    -COPY W0008     -PRE ARTS-                                         
061300     05  FILLER                  PIC X.                                   
061400     EJECT                                                                
061500*01    -COPY W0008     -PRE STYR-                                         
061600     05  FILLER                  PIC X.                                   
061700     EJECT                                                                
061800*01    -COPY W0008     -PRE GMTC-                                         
061900     05  FILLER                  PIC X.                                   
062000     EJECT                                                                
062100*01    -COPY W0008     -PRE ARTM-                                         
062200     05  FILLER                  PIC X.                                   
062300     EJECT                                                                
062400*01    -COPY W0008     -PRE XXBU-                                         
062500     05  FILLER                  PIC X.                                   
062600     EJECT                                                                
062700*01    -COPY W0008     -PRE WDB2-                                         
062800     05  FILLER                  PIC X.                                   
062900     EJECT                                                                
063000*01    -COPY W0008     -PRE WDB1-                                         
063100     05  FILLER                  PIC X.                                   
063200     EJECT                                                                
063300*01    -COPY W0008     -PRE ORQI-                                         
063400     05  FILLER                  PIC X.                                   
063500     EJECT                                                                
063600*01    -COPY W0008     -PRE ORQM-                                         
063700     05  FILLER                  PIC X.                                   
063800     EJECT                                                                
063900*01    -COPY W0008     -PRE WDM2-                                         
064000     05  FILLER                  PIC X.                                   
064100     EJECT                                                                
064200*01    -COPY W0008     -PRE XXBX-                                         
064300     05  FILLER                  PIC X.                                   
064400     EJECT                                                                
064500*01    -COPY W0008     -PRE WDB6-                                         
064600     05  FILLER                  PIC X.                                   
064700     EJECT                                                                
064800 PROCEDURE DIVISION USING MSG-PCB 2109-PCB                                
064900      USEA-PCB ORDP-PCB ORDT-PCB                                          
065000      ZZAC-PCB ART-PCB ARTS-PCB STYR-PCB GMTC-PCB ARTM-PCB                
065100      XXBU-PCB WDB2-PCB WDB1-PCB ORQI-PCB ORQM-PCB                        
065200      WDM2-PCB XXBX-PCB WDB6-PCB.                                         
065300     ENTRY 'DLITCBL' USING MSG-PCB 2109-PCB                               
065400      USEA-PCB ORDP-PCB ORDT-PCB                                          
065500      ZZAC-PCB ART-PCB ARTS-PCB STYR-PCB GMTC-PCB ARTM-PCB                
065600      XXBU-PCB WDB2-PCB WDB1-PCB ORQI-PCB ORQM-PCB                        
065700      WDM2-PCB XXBX-PCB WDB6-PCB.                                         
065800                                                                          
065900     PERFORM IMS-GU-MSG                                                   
066000     IF SEGMENT-FINNS                                                     
066100       PERFORM A-INIT                                                     
066200       IF INPUT-OK     = JA AND                                           
066300          OPP-FAELT-OK = JA AND                                           
066400          VALKOD-OK    = JA                                               
066500                                                                          
066600         MOVE +1 TO INDX                                                  
066700         EVALUATE TRUE                                                    
066800         WHEN WS-IDTRANS NOT = '4572'                                     
066900           PERFORM B-FLYTTA-GAMLA-NYCKLAR                                 
067000           PERFORM M-LAES                                                 
067100         WHEN MFS-IDPFK = 7                                               
067200           PERFORM B-FLYTTA-GAMLA-NYCKLAR                                 
067300           PERFORM M-LAES                                                 
067400         WHEN MFS-IDPFK = 8                                               
067500           IF MID-IDDISTR-SPAR NOT = '0000'                               
067600             PERFORM C-FLYTTA-SPARADE-NYCKLAR                             
067700             PERFORM Q-LAES-PF8                                           
067800           ELSE                                                           
067900             PERFORM G1-FLYTTA-NYCKLAR                                    
068000             PERFORM N-LAES                                               
068100           END-IF                                                         
068200         WHEN MFS-UPDATE                                                  
068300           PERFORM UNTIL INDX > MID-KVRAD-SPAR                            
068400             EVALUATE TRUE                                                
068500             WHEN MID-VALKOD(INDX) = ' ' AND                              
068600                  MID-KVRAD-SPAR = +1                                     
068700               IF DIST19-SATS                                             
068800                 MOVE TEXT-0422 (SPRAK-IX)                                
068900                             TO MOD-TEMFSFEL                              
069000               ELSE                                                       
069100                 PERFORM P-KOLLA-OM-TPO4                                  
069200                 IF NOT TPO4                                              
069300                   PERFORM D-ANDRA                                        
069400                 END-IF                                                   
069500               END-IF                                                     
069600             WHEN MID-VALKOD(INDX) = 'D'                                  
069700               IF DIST19-SATS                                             
069800                 MOVE TEXT-0422 (SPRAK-IX)                                
069900                           TO MOD-TEMFSFEL                                
070000               ELSE                                                       
070100                 PERFORM O-KOLLA-TPO-FRYSTID                              
070200                 IF BORTOM-FRYSTID OR                                     
070300                    SEC-KDSVAR = ' '                                      
070400                   PERFORM E-DELETE                                       
070500                 END-IF                                                   
070600               END-IF                                                     
070700             WHEN MID-VALKOD(INDX) = 'P'                                  
070800               PERFORM P-KOLLA-OM-TPO4                                    
070900               IF NOT TPO4                                                
071000                 PERFORM O-KOLLA-TPO-FRYSTID                              
071100                 IF BORTOM-FRYSTID OR SEC-KDSVAR = ' '                    
071200                   PERFORM F-PRIO                                         
071300                 END-IF                                                   
071400               END-IF                                                     
071500             END-EVALUATE                                                 
071600             ADD +1 TO INDX                                               
071700           END-PERFORM                                                    
071800           MOVE +1 TO INDX                                                
071900           PERFORM G1-FLYTTA-NYCKLAR                                      
072000           PERFORM N-LAES                                                 
072100           IF W-UPDATE = JA                                               
072200             MOVE TEXT-0404 (SPRAK-IX) TO MOD-TEMFSINF                    
072300           END-IF                                                         
072400         WHEN OTHER                                                       
072500           IF W-VALKOD = ' '                                              
072600             IF MID-OPP-RAD = ALL '+'                                     
072700               PERFORM G2-FLYTTA-NYCKLAR                                  
072800               IF W-NEWKEY = JA                                           
072900                 PERFORM M-LAES                                           
073000               ELSE                                                       
073100                 PERFORM N-LAES                                           
073200               END-IF                                                     
073300             ELSE                                                         
073400               MOVE NEJ TO OPP-FAELT-OK                                   
073500                           AENDRA-ENTER                                   
073600               PERFORM K-FLYTTA-FELTEXT                                   
073700             END-IF                                                       
073800           ELSE                                                           
073900             PERFORM UNTIL INDX > MID-KVRAD-SPAR                          
074000               IF MID-VALKOD(INDX) = 'A'                                  
074100                 IF DIST19-SATS                                           
074200                   MOVE TEXT-0422 (SPRAK-IX)                              
074300                             TO MOD-TEMFSFEL                              
074400                   PERFORM G2-FLYTTA-NYCKLAR                              
074500                   PERFORM N-LAES                                         
074600                 ELSE                                                     
074700                   PERFORM P-KOLLA-OM-TPO4                                
074800                   IF NOT TPO4                                            
074900                     PERFORM O-KOLLA-TPO-FRYSTID                          
075000                     PERFORM H-OPPNA                                      
075100                   ELSE                                                   
075200                     PERFORM G2-FLYTTA-NYCKLAR                            
075300                     PERFORM N-LAES                                       
075400                   END-IF                                                 
075500                 END-IF                                                   
075600               END-IF                                                     
075700               ADD +1 TO INDX                                             
075800             END-PERFORM                                                  
075900             MOVE +1 TO INDX                                              
076000             PERFORM IMS-ISRT-MSG                                         
076100             MOVE NEJ TO W-ENTER                                          
076200           END-IF                                                         
076300         END-EVALUATE                                                     
076400         IF AENDRA-ENTER = JA                                             
076500           PERFORM L-RENSA-OPP-FAELT                                      
076600         END-IF                                                           
076700       ELSE                                                               
076800                                                                          
076900         PERFORM K-FLYTTA-FELTEXT                                         
077000       END-IF                                                             
077100       IF W-ENTER NOT = NEJ                                               
077200*      MOVE DEBUGG-GRP TO MOD-TEMFSINF                                    
077300         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O57201 + 4                    
077400         PERFORM IMS-ISRT-MSG                                             
077500       END-IF                                                             
077600     END-IF                                                               
077700                                                                          
077800     MOVE ZERO TO RETURN-CODE                                             
077900     GOBACK.                                                              
078000     EJECT                                                                
078100 A-INIT SECTION.                                                          
078200                                                                          
078300     MOVE WC-CDC-SE    TO W-IDDC-MIN-N10                                  
078400                                                                          
078500     IF MSG-DUBBLA-TRANSKODER                                             
078600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I57201                 
078700       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
078800       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
078900                                             2109-KDMFSFOR                
079000       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
079100     ELSE                                                                 
079200       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I57201                   
079300       MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                    
079400       MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                   
079500                                           2109-KDMFSFOR                  
079600     END-IF                                                               
079700     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
079800     MOVE MFS-IDTRANS                     TO WS-IDTRANS                   
079900                                                                          
080000     IF NOT EGEN-MID                                                      
080100       MOVE '+++++++'      TO    MID-KVART-OPP                            
080200       MOVE '+'            TO    MID-KDORDKL-OPP                          
080300       MOVE '++'           TO    MID-KDFRAKT-OPP                          
080400       MOVE '++++++++++'   TO    MID-PRARTNTO-OPP                         
080500       MOVE '7'            TO    MFS-IDPFK                                
080600     END-IF                                                               
080700                                                                          
080800     MOVE LOW-VALUE  TO MSG-AREA                                          
080900     MOVE 'W4O572N1' TO MFS-IDMOD                                         
081000     MOVE '4572'     TO MOD-IDTRANS                                       
081100     MOVE ZERO       TO MOD-SPARADE-NYCKLAR                               
081200                        MOD-SPARADE-NYCKLAR-E                             
081300                                                                          
081400     IF MID-IDDISTR-IN          NOT = ALL '+'                             
081500         MOVE '7'   TO MFS-IDPFK                                          
081600     END-IF                                                               
081700                                                                          
081800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
081900     MOVE '001'             TO MSGI-KDCALL                                
082000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
082100     MOVE '4572'            TO MSGI-IDTRANS                               
082200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
082300     IF EGEN-MID                                                          
082400        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
082500        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
082600        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
082700     END-IF                                                               
082800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
082900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
083000       MOVE 'S  '  TO MED-IDSKYLT                                         
083100       MOVE +1     TO SPRAK-IX                                            
083200     ELSE                                                                 
083300       MOVE 'GB '  TO MED-IDSKYLT                                         
083400       MOVE +2     TO SPRAK-IX                                            
083500     END-IF                                                               
083600                                                                          
083700     MOVE MSGI-IDDISTR       TO IDDISTR-WS                                
083800     MOVE MSGI-IDDC          TO W-IDDC-B6                                 
083900     PERFORM IMS-GU-WDB601                                                
084000                                                                          
084100     PERFORM S30-SECURIT                                                  
084200     IF SEC-KDSVAR = ' ' OR                                               
084300        SEC-KDSVAR = '1' OR                                               
084400        SEC-KDSVAR = '2' OR                                               
084500        SEC-KDSVAR = '3' OR                                               
084600        SEC-KDSVAR = '5' OR                                               
084700        SEC-KDSVAR = '6'                                                  
084800                                                                          
084900        MOVE 'J'      TO OPP-FAELT-OK                                     
085000        MOVE 'J'      TO INPUT-OK                                         
085100        MOVE 'J'      TO VALKOD-OK                                        
085200        MOVE 'J'      TO FRAKT-OK                                         
085300        MOVE 'J'      TO ANTAL-OK                                         
085400        MOVE 'J'      TO KLASS-OK                                         
085500        MOVE 'J'      TO PRIS-OK                                          
085600        MOVE 'J'      TO AENDRA-ENTER                                     
085700        MOVE NEJ      TO W-OPPNAD                                         
085800        MOVE SPACE TO W-VALKOD                                            
085900        MOVE SPACE TO W-ENTER                                             
086000        MOVE +13      TO MAX-LINES                                        
086100        MOVE +0       TO W-KVRAD                                          
086200        MOVE +0       TO W-PUNKT                                          
086300                                                                          
086400        PERFORM AC-SPARA-INPUT                                            
086500        PERFORM AB-KOLLA-VALKOD                                           
086600                                                                          
086700        MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                            
086800                                MOD-IDKUNDNR-IN                           
086900                                MOD-IDARTNR-IN                            
087000                                MOD-KDORDKL-IN                            
087100                                MOD-KDPRODSL-IN                           
087200                                MOD-IDORDNR-IN                            
087300                                MOD-KDTPOTYP-IN                           
087400                                MOD-IDDC-IN                               
087500                                MOD-TEDDI                                 
087600                                MOD-TEMFSFEL                              
087700                                MOD-TEMFSINF                              
087800                                MOD-KVART-OPP                             
087900                                MOD-KDORDKL-OPP                           
088000                                MOD-KDFRAKT-OPP                           
088100                                MOD-PRARTNTO-OPP                          
088200        IF MID-SPARADE-NYCKLAR        = ALL '0' AND                       
088300           MID-SPARADE-NYCKLAR-E      = ALL '0'                           
088400          MOVE '7'   TO MFS-IDPFK                                         
088500        END-IF                                                            
088600                                                                          
088700        IF W-IDDISTR             = 0 AND                                  
088800           MID-IDDISTR-SPAR      = 0 AND                                  
088900           MID-IDDISTR-SPAR-E = 0                                         
089000            MOVE NEJ            TO INPUT-OK                               
089100        END-IF                                                            
089200     ELSE                                                                 
089300        MOVE NEJ TO INPUT-OK                                              
089400     END-IF                                                               
089500     .                                                                    
089600     EJECT                                                                
089700 AB-KOLLA-VALKOD SECTION.                                                 
089800                                                                          
089900     MOVE +1 TO INDX                                                      
090000     EVALUATE TRUE                                                        
090100     WHEN MFS-IDPFK = 7                                                   
090200       MOVE 'J' TO VALKOD-OK                                              
090300     WHEN MFS-IDPFK = 8                                                   
090400       MOVE 'J' TO VALKOD-OK                                              
090500     WHEN MFS-UPDATE                                                      
090600       PERFORM UNTIL INDX > MID-KVRAD-SPAR OR                             
090700         VALKOD-OK = NEJ                                                  
090800           EVALUATE TRUE                                                  
090900           WHEN MID-VALKOD(INDX) = 'D'                                    
091000             IF W-VALKOD = 'P' OR                                         
091100                W-VALKOD = 'A' OR                                         
091200                W-OPPNAD = JA                                             
091300               MOVE 'N' TO VALKOD-OK                                      
091400             ELSE                                                         
091500               MOVE 'J' TO VALKOD-OK                                      
091600               MOVE 'D' TO W-VALKOD                                       
091700             END-IF                                                       
091800           WHEN MID-VALKOD(INDX) = 'P'                                    
091900             IF W-VALKOD = 'D' OR                                         
092000                W-VALKOD = 'A' OR                                         
092100                W-OPPNAD = JA                                             
092200               MOVE 'N' TO VALKOD-OK                                      
092300             ELSE                                                         
092400               MOVE 'J' TO VALKOD-OK                                      
092500               MOVE 'P' TO W-VALKOD                                       
092600             END-IF                                                       
092700           WHEN MID-VALKOD(INDX) = ' '                                    
092800             IF MID-KVRAD-SPAR = +1                                       
092900                MOVE +1 TO INDX                                           
093000                MOVE JA TO  W-OPPNAD                                      
093100                PERFORM ABA-FLYTTA-RO-NYCKLAR                             
093200                PERFORM IMS-GU-RO                                         
093300               IF SEGMENT-FINNS                                           
093400                 IF OPP-FAELT-OK = JA                                     
093500                   PERFORM ABB-FRAKT                                      
093600                   PERFORM ABC-KOLLA-ANTAL                                
093700                 END-IF                                                   
093800                 MOVE 'J' TO VALKOD-OK                                    
093900               END-IF                                                     
094000             END-IF                                                       
094100           WHEN OTHER                                                     
094200             MOVE 'N' TO VALKOD-OK                                        
094300           END-EVALUATE                                                   
094400          ADD +1 TO INDX                                                  
094500       END-PERFORM                                                        
094600       IF VALKOD-OK NOT = NEJ                                             
094700         IF W-VALKOD = ' '                                                
094800           IF INDX > 1                                                    
094900             ADD -1 TO INDX                                               
095000           END-IF                                                         
095100           IF MID-KVART(INDX) = SPACE AND                                 
095200              MID-KDORDKL(INDX) = SPACE AND                               
095300              MID-KDFRAKT(INDX) = SPACE AND                               
095400              MID-PRARTNTO(INDX) = SPACE                                  
095500             MOVE NEJ TO VALKOD-OK                                        
095600           END-IF                                                         
095700         END-IF                                                           
095800       END-IF                                                             
095900     WHEN OTHER                                                           
096000       PERFORM UNTIL INDX      > MID-KVRAD-SPAR OR                        
096100                     VALKOD-OK = NEJ                                      
096200         EVALUATE TRUE                                                    
096300         WHEN MID-VALKOD(INDX) = 'A'                                      
096400           IF W-VALKOD              = ' '   AND                           
096500              MID-IDARTNR(INDX) NOT = SPACE                               
096600             MOVE 'J' TO VALKOD-OK                                        
096700             MOVE 'A' TO W-VALKOD                                         
096800           ELSE                                                           
096900             MOVE 'N' TO VALKOD-OK                                        
097000           END-IF                                                         
097100         WHEN MID-VALKOD(INDX) NOT = ' '                                  
097200           MOVE 'N' TO VALKOD-OK                                          
097300         END-EVALUATE                                                     
097400         ADD +1 TO INDX                                                   
097500       END-PERFORM                                                        
097600       IF W-VALKOD = ' '                                                  
097700         MOVE   'J' TO VALKOD-OK                                          
097800       END-IF                                                             
097900     END-EVALUATE                                                         
098000     .                                                                    
098100     EJECT                                                                
098200 ABA-FLYTTA-RO-NYCKLAR SECTION.                                           
098300                                                                          
098400                                                                          
098500     INSPECT MID-IDKUNDNR (INDX) REPLACING LEADING SPACE BY ZERO          
098600     INSPECT MID-IDORDNR  (INDX) REPLACING LEADING SPACE BY ZERO          
098700     INSPECT MID-IDARTNR  (INDX) REPLACING LEADING SPACE BY ZERO          
098800     INSPECT MID-IDLOPNR  (INDX) REPLACING LEADING SPACE BY ZERO          
098900     MOVE    MSGI-IDDISTR        TO        W-IDDISTR-N2                   
099000     IF WS-GODKAEND-BILD                                                  
099100        MOVE MID-IDKUNDNR (INDX) TO        W-IDKUNDNR-N2                  
099200        MOVE MID-IDORDNR  (INDX) TO        W-IDORDNR-N2                   
099300        MOVE MID-IDARTNR  (INDX) TO        W-IDARTNR-N2                   
099400        MOVE MID-IDLOPNR  (INDX) TO        W-IDLOPNR-N2                   
099500     ELSE                                                                 
099600        MOVE ZERO                       TO W-IDKUNDNR-N2                  
099700                                           W-IDORDNR-N2                   
099800                                           W-IDARTNR-N2                   
099900                                           W-IDLOPNR-N2                   
100000     END-IF                                                               
100100     .                                                                    
100200     EJECT                                                                
100300 ABB-FRAKT SECTION.                                                       
100400* I DENNA SEKTION KONTROLLERAS OM FRAKTKODEN FINNS PÅ KUNDREG.            
100500     IF KDFRAKT-OPP > 0                                                   
100600       MOVE RAD-IDDC     TO W-IDDC-WDB5                                   
100700       MOVE KDFRAKT-OPP  TO W-KDFRAKT-WDB5                                
100800       MOVE RAD-IDDISTR  TO W-IDDISTR-WDB5                                
100900       MOVE 9999999      TO W-IDKUNDNR-WDB5                               
101000       PERFORM IMS-GU-WDB501                                              
101100       IF SEGMENT-FINNS                                                   
101200         MOVE JA  TO FRAKT-OK                                             
101300       ELSE                                                               
101400         MOVE NEJ TO OPP-FAELT-OK                                         
101500         MOVE NEJ TO FRAKT-OK                                             
101600       END-IF                                                             
101700     END-IF                                                               
101800     .                                                                    
101900     EJECT                                                                
102000 ABC-KOLLA-ANTAL            SECTION.                                      
102100                                                                          
102200     MOVE KVART-OPP TO W-SPAR-KVART                                       
102300     MOVE W-SPAR-KVART  TO WW-SPAR-KVART                                  
102400     IF WW-SPAR-KVART NOT > RAD-KVART                                     
102500        MOVE RAD-IDARTNR     TO W-IDARTNR-ARTC01                          
102600        PERFORM IMS-GU-ARTC01                                             
102700        IF ART-KDSORT   = 'KG' OR 'L ' OR 'M ' OR                         
102800           RAD-KDKVBRYT = 0                                               
102900           PERFORM IMS-GNP-ARTC11                                         
103000           MOVE +1 TO INDX                                                
103100           MOVE MID-KVART(INDX) TO W-KVART                                
103200           IF CLAG-KVQPACK-1 > +1                                         
103300              COMPUTE W-ANTAL-1 =                                         
103400                      WW-SPAR-KVART / CLAG-KVQPACK-1                      
103500              MULTIPLY W-ANTAL-1 BY CLAG-KVQPACK-1                        
103600                                 GIVING W-ANTAL-2                         
103700              IF W-ANTAL-2 = WW-SPAR-KVART                                
103800                  MOVE JA TO ANTAL-OK                                     
103900              ELSE                                                        
104000                  MOVE NEJ TO KVQPACK-OK                                  
104100                  MOVE NEJ TO OPP-FAELT-OK                                
104200                  MOVE NEJ TO ANTAL-OK                                    
104300              END-IF                                                      
104400           END-IF                                                         
104500        END-IF                                                            
104600     ELSE                                                                 
104700        MOVE NEJ TO ANTAL-OK                                              
104800        MOVE NEJ TO OPP-FAELT-OK                                          
104900     END-IF                                                               
105000     .                                                                    
105100     EJECT                                                                
105200 AC-SPARA-INPUT SECTION.                                                  
105300                                                                          
105400     MOVE JA TO INPUT-OK                                                  
105500                                                                          
105600     IF MID-IDDISTR-IN       NOT = ALL '+'                                
105700         MOVE '7'   TO MFS-IDPFK                                          
105800     END-IF                                                               
105900                                                                          
106000     IF MID-IDKUNDNR-IN = ALL '+'                                         
106100         MOVE MID-IDKUNDNR-UT TO IDKUNDNR-WS                              
106200         INSPECT IDKUNDNR-WS REPLACING LEADING SPACE BY ZERO              
106300     ELSE                                                                 
106400         MOVE MID-IDKUNDNR-IN TO IDKUNDNR-WS                              
106500         MOVE '7'   TO MFS-IDPFK                                          
106600     END-IF                                                               
106700                                                                          
106800     IF MID-IDARTNR-IN = ALL '+'                                          
106900         MOVE MID-IDARTNR-UT TO IDARTNR-WS                                
107000         INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO               
107100     ELSE                                                                 
107200         MOVE MID-IDARTNR-IN TO IDARTNR-WS                                
107300         MOVE '7'   TO MFS-IDPFK                                          
107400     END-IF                                                               
107500                                                                          
107600     IF MID-KDORDKL-IN = ALL '+'                                          
107700         MOVE MID-KDORDKL-UT TO KDORDKL-WS                                
107800         INSPECT KDORDKL-WS REPLACING LEADING SPACE BY ZERO               
107900     ELSE                                                                 
108000         MOVE MID-KDORDKL-IN TO KDORDKL-WS                                
108100         MOVE '7'   TO MFS-IDPFK                                          
108200     END-IF                                                               
108300                                                                          
108400     IF MID-KDPRODSL-IN = ALL '+'                                         
108500         MOVE MID-KDPRODSL-UT TO KDPRODSL-WS                              
108600         INSPECT KDPRODSL-WS REPLACING LEADING SPACE BY ZERO              
108700     ELSE                                                                 
108800         MOVE MID-KDPRODSL-IN TO KDPRODSL-WS                              
108900         MOVE '7'   TO MFS-IDPFK                                          
109000     END-IF                                                               
109100                                                                          
109200     IF MID-IDORDNR-IN = ALL '+'                                          
109300         MOVE MID-IDORDNR-UT TO IDORDNR-WS                                
109400         INSPECT IDORDNR-WS REPLACING LEADING SPACE BY ZERO               
109500     ELSE                                                                 
109600         MOVE MID-IDORDNR-IN TO IDORDNR-WS                                
109700         MOVE '7'   TO MFS-IDPFK                                          
109800     END-IF                                                               
109900                                                                          
110000     IF MID-KDTPOTYP-IN = ALL '+'                                         
110100         MOVE MID-KDTPOTYP-UT TO KDTPOTYP-WS                              
110200         INSPECT KDTPOTYP-WS REPLACING LEADING SPACE BY ZERO              
110300     ELSE                                                                 
110400         MOVE MID-KDTPOTYP-IN TO KDTPOTYP-WS                              
110500         MOVE '7'   TO MFS-IDPFK                                          
110600     END-IF                                                               
110700                                                                          
110800     IF MID-IDDC-IN = ALL '+'                                             
110900         MOVE MID-IDDC-UT TO IDDC-WS                                      
111000         INSPECT IDDC-WS REPLACING LEADING SPACE BY ZERO                  
111100     ELSE                                                                 
111200         MOVE MID-IDDC-IN TO IDDC-WS                                      
111300         MOVE '7'   TO MFS-IDPFK                                          
111400     END-IF                                                               
111500                                                                          
111600     IF WS-GODKAEND-BILD                                                  
111700        CONTINUE                                                          
111800     ELSE                                                                 
111900        MOVE ZERO                       TO IDKUNDNR-WS                    
112000                                           IDARTNR-WS                     
112100                                           KDORDKL-WS                     
112200                                           KDPRODSL-WS                    
112300                                           IDORDNR-WS                     
112400                                           KDTPOTYP-WS                    
112500                                           IDDC-WS                        
112600     END-IF                                                               
112700                                                                          
112800     MOVE MSGI-IDDISTR   TO MOD-IDDISTR-UT                                
112900     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
113000                                                                          
113100     MOVE IDKUNDNR-WS  TO MOD-IDKUNDNR-UT                                 
113200     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
113300                                                                          
113400     MOVE IDARTNR-WS   TO MOD-IDARTNR-UT                                  
113500     INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE              
113600                                                                          
113700     MOVE KDORDKL-WS   TO MOD-KDORDKL-UT                                  
113800     INSPECT MOD-KDORDKL-UT  REPLACING LEADING ZERO BY SPACE              
113900                                                                          
114000     MOVE KDPRODSL-WS  TO MOD-KDPRODSL-UT                                 
114100     INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO BY SPACE              
114200                                                                          
114300     MOVE IDORDNR-WS   TO MOD-IDORDNR-UT                                  
114400     INSPECT MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE              
114500                                                                          
114600     MOVE KDTPOTYP-WS  TO MOD-KDTPOTYP-UT                                 
114700     INSPECT MOD-KDTPOTYP-UT REPLACING LEADING ZERO BY SPACE              
114800                                                                          
114900     MOVE IDDC-WS      TO MOD-IDDC-UT                                     
115000                                                                          
115100     IF DCS-IDDC NOT = IDDC-WS                                            
115200        MOVE IDDC-WS TO W-IDDC-B6                                         
115300        PERFORM IMS-GU-WDB601                                             
115400     END-IF                                                               
115500     IF DCS-KDDC = SPACE OR DCS-CDC-TR                                    
115600       MOVE  MSGI-IDDC  TO MOD-IDDC-UT                                    
115700                           IDDC-WS                                        
115800     END-IF                                                               
115900     INSPECT MOD-IDDC-UT     REPLACING LEADING ZERO BY SPACE              
116000                                                                          
116100     IF MID-PRARTNTO-OPP = ALL '+'                                        
116200        MOVE ZERO          TO SPAR-PRARTNTO                               
116300     ELSE                                                                 
116400         IF MID-PRARTNTO-OPP = ZERO                                       
116500            MOVE ZERO           TO SPAR-PRARTNTO                          
116600            MOVE NEJ TO PRIS-OK                                           
116700            MOVE NEJ TO OPP-FAELT-OK                                      
116800         ELSE                                                             
116900           IF DCS-NDC-NA OR DCS-NDC-CN                                    
117000             MOVE NEJ TO PRIS-OK                                          
117100             MOVE NEJ TO OPP-FAELT-OK                                     
117200           ELSE                                                           
117300             IF MID-PRARTNTO-OPP NOT = ZERO                               
117400                MOVE +7               TO DEC-KVHELTAL                     
117500                MOVE +2               TO DEC-KVDECIMAL                    
117600                MOVE MID-PRARTNTO-OPP TO DEC-IDFRIDATA                    
117700                CALL WDECEDIT USING DEC-WDECAREA                          
117800                IF DEC-KDSVAR-OK                                          
117900                   MOVE DEC-IDEDITDATA TO SPAR-PRARTNTO                   
118000                   MOVE JA TO PRIS-OK                                     
118100                ELSE                                                      
118200                   MOVE NEJ TO PRIS-OK                                    
118300                   MOVE NEJ TO OPP-FAELT-OK                               
118400                END-IF                                                    
118500             END-IF                                                       
118600           END-IF                                                         
118700         END-IF                                                           
118800     END-IF                                                               
118900                                                                          
119000     IF MID-KVART-OPP = ALL '+'                                           
119100         MOVE ZERO                 TO   KVART-OPP                         
119200     ELSE                                                                 
119300         INSPECT MID-KVART-OPP REPLACING ALL SPACE BY ZERO                
119400         IF MID-KVART-OPP = ALL '0'                                       
119500            MOVE NEJ TO ANTAL-OK                                          
119600            MOVE NEJ TO OPP-FAELT-OK                                      
119700         ELSE                                                             
119800            MOVE MID-KVART-OPP     TO   KVART-OPP                         
119900         END-IF                                                           
120000     END-IF                                                               
120100                                                                          
120200     IF MID-KDORDKL-OPP = ALL '+'                                         
120300        MOVE ZERO               TO   KDORDKL-OPP                          
120400     ELSE                                                                 
120500        IF MID-KDORDKL-OPP = ALL '0'                                      
120600           MOVE NEJ TO KLASS-OK                                           
120700           MOVE NEJ TO OPP-FAELT-OK                                       
120800        ELSE                                                              
120900           MOVE MID-KDORDKL-OPP     TO   KDORDKL-OPP                      
121000           IF KDORDKL-OPP > +4                                            
121100              MOVE NEJ TO KLASS-OK                                        
121200              MOVE NEJ TO OPP-FAELT-OK                                    
121300           END-IF                                                         
121400        END-IF                                                            
121500     END-IF                                                               
121600                                                                          
121700     IF MID-KDFRAKT-OPP = ALL '+'                                         
121800        MOVE ZERO             TO   KDFRAKT-OPP                            
121900     ELSE                                                                 
122000        IF MID-KDFRAKT-OPP    = ALL '0'                                   
122100           MOVE NEJ TO FRAKT-OK                                           
122200           MOVE NEJ TO OPP-FAELT-OK                                       
122300        ELSE                                                              
122400           MOVE MID-KDFRAKT-OPP     TO   KDFRAKT-OPP                      
122500        END-IF                                                            
122600     END-IF                                                               
122700                                                                          
122800     IF DCS-IDDC NOT = IDDC-WS                                            
122900        MOVE IDDC-WS     TO W-IDDC-B6                                     
123000        PERFORM IMS-GU-WDB601                                             
123100     END-IF                                                               
123200     IF MSGI-IDDISTR    NOT NUMERIC  OR                                   
123300        IDKUNDNR-WS     NOT NUMERIC  OR                                   
123400        IDARTNR-WS      NOT NUMERIC  OR                                   
123500        KDORDKL-WS      NOT NUMERIC  OR                                   
123600        KDPRODSL-WS     NOT NUMERIC  OR                                   
123700        IDORDNR-WS      NOT NUMERIC  OR                                   
123800        KDTPOTYP-WS     NOT NUMERIC  OR                                   
123900        DCS-KDDC = SPACE OR                                               
124000        DCS-DDC                                                           
124100       MOVE NEJ TO INPUT-OK                                               
124200     END-IF                                                               
124300                                                                          
124400     IF INPUT-OK = JA                                                     
124500        MOVE MSGI-IDDISTR  TO W-IDDISTR                                   
124600                              TEST-IDDISTR                                
124700                                                                          
124800        MOVE IDKUNDNR-WS  TO W-IDKUNDNR                                   
124900        MOVE IDARTNR-WS   TO W-IDARTNR                                    
125000        MOVE KDORDKL-WS   TO W-KDORDKL                                    
125100        MOVE KDPRODSL-WS  TO W-KDPRODSL                                   
125200        MOVE IDORDNR-WS   TO W-IDORDNR                                    
125300        MOVE KDTPOTYP-WS  TO W-KDTPOTYP                                   
125400        MOVE IDDC-WS      TO W-IDDC                                       
125500                                                                          
125600        IF W-IDDISTR NOT > 0 OR                                           
125700           W-KDORDKL     > 5 OR                                           
125800           W-KDTPOTYP    > 7                                              
125900*          W-KDTPOTYP    > 6 OR                                           
126000*          (W-IDDC       NOT = ZERO AND                                   
126100*           NOT NDC-NA)                                                   
126200          MOVE NEJ TO INPUT-OK                                            
126300        END-IF                                                            
126400     END-IF                                                               
126500                                                                          
126600     IF DIST79-DEALER-PRICE                                               
126700       IF ENGLISH-TEXT                                                    
126800         MOVE 'DPR'       TO MOD-TEDDI                                    
126900       ELSE                                                               
127000         MOVE 'ÅFP'       TO MOD-TEDDI                                    
127100       END-IF                                                             
127200     ELSE                                                                 
127300       MOVE SPACES        TO MOD-TEDDI                                    
127400     END-IF                                                               
127500     .                                                                    
127600     EJECT                                                                
127700 B-FLYTTA-GAMLA-NYCKLAR SECTION.                                          
127800                                                                          
127900     MOVE LOW-VALUE           TO W-SOK-NYCKLAR-MIN                        
128000                                 W-WDA5D1KY-MIN                           
128100     MOVE HIGH-VALUE          TO W-SOK-NYCKLAR-MAX                        
128200                                 W-WDA5D1KY-MAX                           
128300     MOVE '2'                 TO W-KDSTARAD-MAX                           
128400                                                                          
128500     MOVE W-IDDISTR          TO W-IDDISTR-N1-MIN                          
128600                                W-IDDISTR-N1-MAX                          
128700                                                                          
128800     IF W-IDKUNDNR       > ZERO                                           
128900        MOVE W-IDKUNDNR       TO W-IDKUNDNR-N1-MIN                        
129000                                 W-IDKUNDNR-N1-MAX                        
129100     END-IF                                                               
129200                                                                          
129300     IF W-IDARTNR       > ZERO                                            
129400       MOVE W-IDARTNR            TO W-IDARTNR-MIN                         
129500                                    W-IDARTNR-MAX                         
129600        IF W-IDKUNDNR     > ZERO                                          
129700           MOVE W-IDARTNR        TO W-IDARTNR-N1-MIN                      
129800                                    W-IDARTNR-N1-MAX                      
129900        END-IF                                                            
130000     END-IF                                                               
130100                                                                          
130200     IF W-IDORDNR       > ZERO                                            
130300       MOVE W-IDORDNR             TO W-IDORDNR-MIN                        
130400                                     W-IDORDNR-MAX                        
130500        IF W-IDKUNDNR     > ZERO AND                                      
130600           W-IDARTNR      > ZERO                                          
130700           MOVE W-IDORDNR          TO W-IDORDNR-N1-MIN                    
130800                                      W-IDORDNR-N1-MAX                    
130900        END-IF                                                            
131000     END-IF                                                               
131100     IF W-KDORDKL      > ZERO                                             
131200       MOVE W-KDORDKL       TO W-KDORDKL-MIN                              
131300                               W-KDORDKL-MAX                              
131400     END-IF                                                               
131500     IF W-KDPRODSL     > ZERO                                             
131600       MOVE W-KDPRODSL      TO W-KDPRODSL-MIN                             
131700                               W-KDPRODSL-MAX                             
131800     END-IF                                                               
131900     IF W-KDTPOTYP     > ZERO                                             
132000       MOVE W-KDTPOTYP      TO W-KDTPOTYP-MIN                             
132100                               W-KDTPOTYP-MAX                             
132200     END-IF                                                               
132300     IF W-IDDC         > ZERO                                             
132400       MOVE W-IDDC          TO W-IDDC-MIN                                 
132500                               W-IDDC-MAX                                 
132600     END-IF                                                               
132700     .                                                                    
132800     EJECT                                                                
132900 C-FLYTTA-SPARADE-NYCKLAR SECTION.                                        
133000* HÄR FLYTTAS DE NYCKLAR SOM LIGGER SPARADE PÅ RAD 4 PÅ BILDEN            
133100     MOVE LOW-VALUE  TO W-WDA5D1KY-MIN                                    
133200                        W-SOK-NYCKLAR-MIN                                 
133300     MOVE HIGH-VALUE TO W-WDA5D1KY-MAX                                    
133400                        W-SOK-NYCKLAR-MAX                                 
133500     MOVE '2'        TO W-KDSTARAD-MAX                                    
133600                                                                          
133700     IF MID-IDDISTR-SPAR  > ZERO                                          
133800       MOVE MID-IDDISTR-SPAR       TO W-IDDISTR-N1-MIN                    
133900                                      W-IDDISTR-N1-MAX                    
134000                                      W-IDDISTR-N2                        
134100     ELSE                                                                 
134200       MOVE W-IDDISTR              TO W-IDDISTR-N1-MIN                    
134300                                      W-IDDISTR-N1-MAX                    
134400     END-IF                                                               
134500                                                                          
134600     IF MID-IDKUNDNR-SPAR  > ZERO                                         
134700        MOVE MID-IDKUNDNR-SPAR  TO W-IDKUNDNR-N1-MIN                      
134800                                   W-IDKUNDNR-N1-MAX                      
134900                                   W-IDKUNDNR-N2                          
135000     END-IF                                                               
135100                                                                          
135200     IF MID-IDARTNR-SPAR  > ZERO                                          
135300       MOVE MID-IDARTNR-SPAR    TO W-IDARTNR-MIN                          
135400                                   W-IDARTNR-N1-MIN                       
135500                                   W-IDARTNR-N2                           
135600     END-IF                                                               
135700                                                                          
135800     IF MID-IDLOPNR-SPAR  > ZERO                                          
135900       MOVE MID-IDLOPNR-SPAR    TO W-IDLOPNR-N2                           
136000                                   W-IDLOPNR-MIN                          
136100     END-IF                                                               
136200                                                                          
136300     IF MID-IDORDNR-SPAR  > ZERO                                          
136400       MOVE MID-IDORDNR-SPAR    TO W-IDKUNDRF-1-5                         
136500       MOVE W-IDKUNDRF          TO W-IDKUNDRF-MIN                         
136600                                   W-IDKUNDRF-N1-MIN                      
136700                                   W-IDKUNDRF-N2                          
136800     END-IF                                                               
136900     .                                                                    
137000     EJECT                                                                
137100 D-ANDRA SECTION.                                                         
137200                                                                          
137300     IF OPP-FAELT-OK = JA                                                 
137400                                                                          
137500        PERFORM IMS-GHU-RO                                                
137600        MOVE RAD-IDLOPNR    TO W-SPAR-IDLOPNR                             
137700*       MOVE RAD-KVART      TO W-SPAR-KVART                               
137800        MOVE RAD-KDORDKL    TO W-SPAR-KDORDKL                             
137900        MOVE RAD-KDFRAKT    TO W-SPAR-KDFRAKT                             
138000        MOVE RAD-KDRAPRIO   TO W-SPAR-KDRAPRIO                            
138100        MOVE RAD-PRARTNTO-LOC     TO W-SPAR-PRARTNTO-LOC                  
138200        MOVE RAD-PRARTNTO-LOCPREL TO W-SPAR-PRARTNTO-LOCPREL              
138300        MOVE RAD-PRARTNTO         TO W-SPAR-PRARTNTO                      
138400        MOVE RAD-PRAVCOST         TO W-SPAR-PRAVCOST                      
138500        MOVE +1 TO INDX                                                   
138600        IF KDFRAKT-OPP NOT = ALL '0'                                      
138700            IF KDFRAKT-OPP NOT = MID-KDFRAKT(INDX)                        
138800               MOVE KDFRAKT-OPP TO RAD-KDFRAKT                            
138900               MOVE JA TO W-FAELT-IFYLLT                                  
139000            END-IF                                                        
139100        END-IF                                                            
139200        IF KDORDKL-OPP NOT = ALL '0'                                      
139300            IF KDORDKL-OPP NOT = MID-KDORDKL(INDX)                        
139400               MOVE JA TO W-FAELT-IFYLLT                                  
139500               PERFORM DB-KLASS                                           
139600               IF RAD-KDSTARAD = 2                                        
139700                 IF KVART-OPP = ALL '0'                                   
139800                   PERFORM DE-HANDLE-ROS                                  
139900                 END-IF                                                   
140000                 PERFORM S02-SKAPA-SUC-TRANS                              
140100               END-IF                                                     
140200            END-IF                                                        
140300        END-IF                                                            
140400        IF SPAR-PRARTNTO > +0                                             
140500            MOVE JA TO W-FAELT-IFYLLT                                     
140600            IF DIST79-DEALER-PRICE OR                                     
140800               DIST79-ECOM-PRICE                                          
140900              MOVE SPAR-PRARTNTO  TO RAD-PRARTNTO-LOC                     
141000              MOVE +0             TO RAD-PRARTNTO-LOCPREL                 
141200              IF DIST79-ECOM-PRICE                                        
141300                 MOVE SPAR-PRARTNTO TO RAD-PRARTBTO-LOC                   
141400              END-IF                                                      
141500            ELSE                                                          
141600              IF DIST07-KINA                                              
141700               OR DIST35-CN-TRANSFER                                      
141800               OR DIST35-CN-NDC-RETURNS                                   
141900               OR DIST35-REFILL-INOM-NONVCC-NDC                           
142000               OR DIST35-NONVCC-REFILL                                    
142010               OR DIST35-NONVCC-NONVCC-TRANSFER                           
142020               OR DIST35-NONVCC-VCC-TRANSFER                              
142100               OR DIST07-INDIEN                                           
142200               OR DIST07-KOREA                                            
142300               OR DIST07-TURKEY                                           
142310               OR DIST07-S-AFRICA                                         
142400               OR DIST07-MALAYSIA                                         
142500               OR DIST07-THAILAND                                         
142600               OR DIST07-TAIWAN                                           
142700               OR DIST07-MEXICO                                           
142800               OR DIST07-BRAZIL                                           
142900               OR DIST35-CDC-RETURNS-NON-VCC                              
143000               OR DIST18-SCRAP-NDC-SC                                     
143100               OR DIST18-SCRAP-NDC-QUAL                                   
143200               OR DIST18-SCRAP-NDC-SC-LOCAL                               
143300                 MOVE SPAR-PRARTNTO TO RAD-PRAVCOST                       
143400              ELSE                                                        
143500                 MOVE SPAR-PRARTNTO TO RAD-PRARTNTO                       
143600              END-IF                                                      
143700            END-IF                                                        
143800        END-IF                                                            
143900        IF KVART-OPP NOT = ALL '0'                                        
144000            IF KVART-OPP NOT = RAD-KVART                                  
144100               IF W-FAELT-IFYLLT = JA                                     
144200                  PERFORM DC-ANTAL                                        
144300                  MOVE JA TO W-DELNING                                    
144400               ELSE                                                       
144500                  PERFORM DD-ANTAL-ENDAST                                 
144600                  PERFORM S05-UPPDATERA-WDQ1                              
144700                  MOVE RAD-KVART TO RY9-KVART                             
144800                  IF RAD-KDSTARAD = 2 OR RAD-KDTPOTYP = 6                 
144900                     MOVE SPAR-KVART  TO RY9-KVART                        
145000                  END-IF                                                  
145100                  PERFORM S03-SKAPA-RY9-TRANS                             
145200                  PERFORM S20-SKRIV-LOGG                                  
145300                                                                          
145400                  IF RAD-KDSTARAD = 2 OR RAD-KDTPOTYP = 6                 
145500                     OR RAD-KDTPOTYP = 7                                  
145600                    PERFORM S04-SKAPA-2109-TRANS                          
145700                  END-IF                                                  
145800               END-IF                                                     
145900            END-IF                                                        
146000        END-IF                                                            
146100                                                                          
146200        IF W-DELNING = JA                                                 
146300           PERFORM IMS-GHU-RO                                             
146400           MOVE W-SPAR-IDLOPNR    TO RAD-IDLOPNR                          
146500           MOVE WW-SPAR-KVART     TO RAD-KVART                            
146600           MOVE W-SPAR-KDORDKL    TO RAD-KDORDKL                          
146700           MOVE W-SPAR-KDFRAKT    TO RAD-KDFRAKT                          
146800           MOVE W-SPAR-KDRAPRIO   TO RAD-KDRAPRIO                         
146900           MOVE W-SPAR-PRARTNTO-LOC     TO RAD-PRARTNTO-LOC               
147000           MOVE W-SPAR-PRARTNTO-LOCPREL TO RAD-PRARTNTO-LOCPREL           
147100           MOVE W-SPAR-PRARTNTO         TO RAD-PRARTNTO                   
147200           MOVE W-SPAR-PRAVCOST         TO RAD-PRAVCOST                   
147300        END-IF                                                            
147400        MOVE JA TO W-UPDATE                                               
147500        PERFORM IMS-REPL-RO                                               
147600     END-IF.                                                              
147700     EJECT                                                                
147800 DB-KLASS SECTION.                                                        
147900* I DENNA SEKTION LÄSES STYRREG. FÖR ATT BESTÄMMA PRIO FÖR DEN NYA        
148000* ORDERKLASSEN.                                                           
148100     MOVE RAD-KDTPOTYP      TO W-KDTPOTYP-N5                              
148200     MOVE RAD-IDDISTR       TO W-IDDISTR-FOM-N5                           
148300     MOVE RAD-IDDISTR       TO W-IDDISTR-TOM-N5                           
148400     MOVE KDORDKL-OPP       TO W-KDORDKL-N5                               
148500     MOVE '4511'            TO W-IDHTYP-N5                                
148600     MOVE LOW-VALUE         TO W-VALFRI-N5                                
148700                                                                          
148800     PERFORM IMS-GU-STYR                                                  
148900     MOVE STYR-4512-KDRAPRIO     TO RAD-KDRAPRIO                          
149000     MOVE KDORDKL-OPP            TO RAD-KDORDKL                           
149100     .                                                                    
149200     EJECT                                                                
149300                                                                          
149400 DC-ANTAL SECTION.                                                        
149500* I DENNA SEKTIONEN BEHANDLAS ANTAL OM YTTERLIGARE FÄLT HAR               
149600* FÖRÄNDRATS.                                                             
149700     MOVE KVART-OPP          TO SPAR-KVART                                
149800     COMPUTE WW-SPAR-KVART = RAD-KVART - SPAR-KVART                       
149900     MOVE SPAR-KVART TO RAD-KVART                                         
150000* OM DC EJ = 11 OCH ÄNDRING FRÅN DAG TILL BULK ELLER VISEVERSA            
150100* MÅSTE SALDONA ÄNDRAS                                                    
150200     IF DCS-IDDC NOT = RAD-IDDC                                           
150300        MOVE RAD-IDDC     TO W-IDDC-B6                                    
150400        PERFORM IMS-GU-WDB601                                             
150500     END-IF                                                               
150600     IF  NOT DCS-CDC                                                      
150700     AND RAD-KDSTARAD NOT = '1'                                           
150800     AND (KDORDKL-OPP > 1 AND MID-KDORDKL (INDX) < 2                      
150900      OR  KDORDKL-OPP < 2 AND MID-KDORDKL (INDX) > 1)                     
151000       MOVE MID-IDARTNR(INDX)  TO W-IDARTNR-ARTC01                        
151100                                  W-WDK711-IDARTNR-N                      
151200       MOVE RAD-IDDC           TO W-WDK711-IDDC                           
151300       PERFORM IMS-GHU-WDK711                                             
151400       IF  KDORDKL-OPP > 1                                                
151500           COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                      
151600                                   + SPAR-KVART                           
151700           COMPUTE SLAG-KVROS-DAG = SLAG-KVROS-DAG                        
151800                                  - SPAR-KVART                            
151900       ELSE                                                               
152000           COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                      
152100                                   - SPAR-KVART                           
152200           COMPUTE SLAG-KVROS-DAG = SLAG-KVROS-DAG                        
152300                                  + SPAR-KVART                            
152400       END-IF                                                             
152500       PERFORM IMS-REPL-WDK711                                            
152600     END-IF                                                               
152700                                                                          
152800     ADD +1 TO RAD-IDLOPNR                                                
152900     PERFORM IMS-ISRT-RO                                                  
153000                                                                          
153100     PERFORM UNTIL SEGMENT-FINNS                                          
153200         ADD +1 TO RAD-IDLOPNR                                            
153300         PERFORM IMS-ISRT-RO                                              
153400     END-PERFORM                                                          
153500     .                                                                    
153600     EJECT                                                                
153700 DD-ANTAL-ENDAST SECTION.                                                 
153800* I DENNA SEKTIONEN BEHANDLAS ANTAL OM ENDAST ANTAL HAR                   
153900* FÖRÄNDRATS.                                                             
154000     MOVE MID-IDARTNR(INDX)  TO W-IDARTNR-ARTC01                          
154100                                W-WDK711-IDARTNR-N                        
154200     MOVE RAD-IDDC           TO W-WDK711-IDDC                             
154300     MOVE KVART-OPP          TO SPAR-KVART                                
154400     COMPUTE SPAR-KVART = RAD-KVART - SPAR-KVART                          
154500     IF RAD-KDSTARAD = '1'                                                
154600       IF RAD-FLTPOBEK = JA                                               
154700         PERFORM S01-UPPDATERA-WDK9                                       
154800       END-IF                                                             
154900     ELSE                                                                 
155000       IF DCS-IDDC NOT = RAD-IDDC                                         
155100          MOVE RAD-IDDC     TO W-IDDC-B6                                  
155200          PERFORM IMS-GU-WDB601                                           
155300       END-IF                                                             
155400       IF  DCS-CDC                                                        
155500           PERFORM IMS-GHU-ARTC11                                         
155600           COMPUTE CLAG-KVROS = CLAG-KVROS - SPAR-KVART                   
155700           PERFORM IMS-REPL-ARTK611                                       
155800           PERFORM DDA-EV-UPPDAT-WDK7-REFILL                              
155900       ELSE                                                               
156000           PERFORM IMS-GHU-WDK711                                         
156100           IF  W-SPAR-KDORDKL > 1                                         
156200               COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                  
156300                                       - SPAR-KVART                       
156400           ELSE                                                           
156500               COMPUTE SLAG-KVROS-DAG = SLAG-KVROS-DAG                    
156600                                      - SPAR-KVART                        
156700           END-IF                                                         
156800           PERFORM IMS-REPL-WDK711                                        
156900           PERFORM DDA-EV-UPPDAT-WDK7-REFILL                              
157000       END-IF                                                             
157100     END-IF                                                               
157200                                                                          
157300     MOVE KVART-OPP       TO RAD-KVART                                    
157400     .                                                                    
157500     EJECT                                                                
157600 DE-HANDLE-ROS SECTION.                                                   
157700                                                                          
157800     IF DCS-IDDC NOT = RAD-IDDC                                           
157900        MOVE RAD-IDDC     TO W-IDDC-B6                                    
158000        PERFORM IMS-GU-WDB601                                             
158100     END-IF                                                               
158200                                                                          
158300     IF  NOT DCS-CDC                                                      
158400     AND (KDORDKL-OPP > 1 AND MID-KDORDKL (INDX) < 2                      
158500      OR  KDORDKL-OPP < 2 AND MID-KDORDKL (INDX) > 1)                     
158600        MOVE MID-IDARTNR(INDX)  TO W-WDK711-IDARTNR-N                     
158700        MOVE RAD-IDDC           TO W-WDK711-IDDC                          
158800        PERFORM IMS-GHU-WDK711                                            
158900        IF  KDORDKL-OPP > 1                                               
159000            COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                     
159100                                    + RAD-KVART                           
159200            COMPUTE SLAG-KVROS-DAG = SLAG-KVROS-DAG                       
159300                                    - RAD-KVART                           
159400        ELSE                                                              
159500                                                                          
159600            COMPUTE SLAG-KVROS-DAG = SLAG-KVROS-DAG                       
159700                                    + RAD-KVART                           
159800            COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                     
159900                                    - RAD-KVART                           
160000        END-IF                                                            
160100        PERFORM IMS-REPL-WDK711                                           
160200     END-IF                                                               
160300     .                                                                    
160400     EJECT                                                                
160500 DDA-EV-UPPDAT-WDK7-REFILL         SECTION.                               
160600                                                                          
160700******************************************************************        
160800*                                                                         
160900*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
161000*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
161100*                                                                         
161200******************************************************************        
161300                                                                          
161400     MOVE RAD-IDDISTR         TO W-TP4TRAN-IDDISTR                        
161500                                                                          
161600     PERFORM DB2-SELECT-TP4TRAN                                           
161700                                                                          
161800     MOVE RAD-IDDISTR       TO TEST-IDDISTR                               
161900     IF  DIST35-REFILL                                                    
162000     OR  DIST35-REFILL-INOM-NDC                                           
162100     OR  DIST35-NONVCC-NONVCC-REFILL                                      
162110     OR  DIST35-NONVCC-NONVCC-TRANSFER                                    
162200     OR  DIST35-NA-TRANSFER                                               
162300     OR  DIST35-NA-NDC-RETURNS                                            
162400     OR  DIST35-PACIFIC-TRANSFER                                          
162500     OR  DIST35-REFILL-INOM-JP                                            
162600     OR  DIST35-CN-TRANSFER                                               
162700     OR  DIST35-NONVCC-VCC-REFILL                                         
162710     OR  DIST35-NONVCC-VCC-TRANSFER                                       
162800     OR  RADER-FINNS                                                      
162900                                                                          
163000       IF RADER-FINNS                                                     
163100         MOVE TP4TRAN-IDDC-REC  TO W-WDK711-IDDC                          
163200       ELSE                                                               
163300         SEARCH ALL DIST57-REFILL-DC                                      
163400            AT END                                                        
163500               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
163600                                TO FELTEXT                                
163700               CALL FELLOG                                                
163800            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR             
163900               MOVE DIST57-REFILL-TO-DC(DIST57-IX)                        
164000                                TO W-WDK711-IDDC                          
164100         END-SEARCH                                                       
164200       END-IF                                                             
164300                                                                          
164400       MOVE RAD-IDARTNR       TO W-WDK711-IDARTNR-N                       
164500       PERFORM IMS-GHU-WDK711                                             
164600       SUBTRACT SPAR-KVART    FROM SLAG-KVBEART                           
164700                                                                          
164800       PERFORM IMS-REPL-WDK711                                            
164900     ELSE                                                                 
165000       IF DIST35-NONVCC-CDC-REFILL                                        
165100                                                                          
165200          SEARCH ALL DIST57-REFILL-DC                                     
165300             AT END                                                       
165400                MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                      
165500                                 TO FELTEXT                               
165600                CALL FELLOG                                               
165700             WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR            
165800                MOVE DIST57-REFILL-TO-DC(DIST57-IX)                       
165900                                 TO W-WDK711-IDDC                         
166000          END-SEARCH                                                      
166100                                                                          
166200          MOVE RAD-IDARTNR     TO W-IDARTNR-ARTC01                        
166300          PERFORM IMS-GHU-ARTC11                                          
166400          SUBTRACT SPAR-KVART  FROM CLAG-KVBEART                          
166500                                                                          
166600          PERFORM IMS-REPL-ARTK611                                        
166700       END-IF                                                             
166800     END-IF                                                               
166900     .                                                                    
167000     EJECT                                                                
167100 E-DELETE SECTION.                                                        
167200* I DENNA SEKTION DELETAS EN RAD PÅ ROREG. OCH DET DELETADE               
167300* ANTALET DRAS FRÅN AKTUELL ARTIKEL PÅ ARTREG.                            
167400     MOVE    MID-IDDISTR-UT     TO        IDDISTR-WS                      
167500     MOVE    MID-IDKUNDNR(INDX) TO        IDKUNDNR-WS                     
167600     MOVE    MID-IDORDNR (INDX) TO        IDORDNR-WS                      
167700     MOVE    MID-IDARTNR (INDX) TO        IDARTNR-WS                      
167800     MOVE    MID-IDLOPNR (INDX) TO        IDLOPNR-WS                      
167900                                                                          
168000     INSPECT IDDISTR-WS         REPLACING LEADING SPACE BY ZERO           
168100     INSPECT IDKUNDNR-WS        REPLACING LEADING SPACE BY ZERO           
168200     INSPECT IDARTNR-WS         REPLACING LEADING SPACE BY ZERO           
168300     INSPECT IDORDNR-WS         REPLACING LEADING SPACE BY ZERO           
168400     INSPECT IDLOPNR-WS         REPLACING LEADING SPACE BY ZERO           
168500                                                                          
168600     MOVE    IDDISTR-WS         TO        W-IDDISTR-N2                    
168700     MOVE    IDKUNDNR-WS        TO        W-IDKUNDNR-N2                   
168800     MOVE    SPACE              TO        W-IDKUNDRF-N2                   
168900     MOVE    IDORDNR-WS         TO        W-IDORDNR-N2                    
169000     MOVE    IDARTNR-WS         TO        W-IDARTNR-N2                    
169100                                          W-IDARTNR-ARTC01                
169200                                          W-WDK711-IDARTNR-N              
169300     MOVE    IDLOPNR-WS         TO        W-IDLOPNR-N2                    
169400                                                                          
169500     MOVE    MID-KVART(INDX)    TO        SPAR-KVART                      
169600                                                                          
169700     PERFORM IMS-GHU-RO                                                   
169800                                                                          
169900     IF SEGMENT-FINNS                                                     
170000       MOVE RAD-IDDC            TO W-WDK711-IDDC                          
170100       IF RAD-KDSTARAD = '1'                                              
170200         IF RAD-FLTPOBEK = JA                                             
170300           IF RAD-KDTPOTYP = +4                                           
170400             PERFORM S06-UPPDATERA-ANTAL-MARKNAD                          
170500           ELSE                                                           
170600             PERFORM S01-UPPDATERA-WDK9                                   
170700           END-IF                                                         
170800         END-IF                                                           
170900       ELSE                                                               
171000         IF DCS-IDDC NOT = RAD-IDDC                                       
171100            MOVE RAD-IDDC     TO W-IDDC-B6                                
171200            PERFORM IMS-GU-WDB601                                         
171300         END-IF                                                           
171400         IF  DCS-CDC                                                      
171500             PERFORM IMS-GHU-ARTC11                                       
171600             COMPUTE CLAG-KVROS = CLAG-KVROS - SPAR-KVART                 
171700             PERFORM IMS-REPL-ARTK611                                     
171800             PERFORM EB-EV-UPPDAT-WDK7-REFILL                             
171900         ELSE                                                             
172000             PERFORM IMS-GHU-WDK711                                       
172100             IF  RAD-KDORDKL > 1                                          
172200                 COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                
172300                                         - SPAR-KVART                     
172400             ELSE                                                         
172500                 COMPUTE SLAG-KVROS-DAG = SLAG-KVROS-DAG                  
172600                                        - SPAR-KVART                      
172700             END-IF                                                       
172800             PERFORM IMS-REPL-WDK711                                      
172900             PERFORM EB-EV-UPPDAT-WDK7-REFILL                             
173000         END-IF                                                           
173100       END-IF                                                             
173200       MOVE JA TO W-UPDATE                                                
173300       IF RAD-KDTPOTYP = 2 OR 6                                           
173400         PERFORM EA-BEHANDLA-LARMKO                                       
173500       END-IF                                                             
173600       PERFORM IMS-DLET-RO                                                
173700       PERFORM S05-UPPDATERA-WDQ1                                         
173800                                                                          
173900       MOVE ZERO TO RY9-KVART                                             
174000       IF RAD-KDTPOTYP = 6 OR RAD-KDSTARAD = 2                            
174100         MOVE SPAR-KVART   TO RY9-KVART                                   
174200       END-IF                                                             
174300       PERFORM S03-SKAPA-RY9-TRANS                                        
174400       PERFORM S20-SKRIV-LOGG                                             
174500                                                                          
174600       IF RAD-KDSTARAD = 2 OR RAD-KDTPOTYP = 6                            
174700          OR RAD-KDTPOTYP = 7                                             
174800         PERFORM S04-SKAPA-2109-TRANS                                     
174900       END-IF                                                             
175000     END-IF                                                               
175100     .                                                                    
175200     EJECT                                                                
175300 EA-BEHANDLA-LARMKO SECTION.                                              
175400                                                                          
175500     MOVE RAD-IDANSK TO W-IDANSK-2232                                     
175600     PERFORM IMS-GU-XXBX-WDR220                                           
175700     IF SEGMENT-FINNS                                                     
175800       MOVE XXBX-2232-IDANSK-LARM TO W-IDANSK-2223                        
175900     ELSE                                                                 
176000       MOVE ZERO TO W-IDANSK-2223                                         
176100     END-IF                                                               
176200     MOVE RAD-DASENDAT (3:6) TO W-TISENBEK-DAG                            
176300     MOVE RAD-TISENBEK-KL  TO W-TISENBEK-KL                               
176400     IF RAD-KDTPOTYP = 2                                                  
176500       MOVE 110 TO W-KDLARM                                               
176600     ELSE                                                                 
176700       MOVE 100 TO W-KDLARM                                               
176800     END-IF                                                               
176900     PERFORM IMS-GHU-XXBU-WDR5                                            
177000     IF SEGMENT-FINNS                                                     
177100       PERFORM IMS-DLET-XXBU-WDR5                                         
177200     END-IF                                                               
177300                                                                          
177400     .                                                                    
177500     EJECT                                                                
177600 EB-EV-UPPDAT-WDK7-REFILL         SECTION.                                
177700                                                                          
177800******************************************************************        
177900*                                                                         
178000*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
178100*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
178200*                                                                         
178300******************************************************************        
178400                                                                          
178500     MOVE IDDISTR-WS      TO W-TP4TRAN-IDDISTR                            
178600                                                                          
178700     PERFORM DB2-SELECT-TP4TRAN                                           
178800                                                                          
178900     MOVE IDDISTR-WS        TO TEST-IDDISTR                               
179000     IF  DIST35-REFILL                                                    
179100     OR  DIST35-REFILL-INOM-NDC                                           
179200     OR  DIST35-NONVCC-NONVCC-REFILL                                      
179210     OR  DIST35-NONVCC-NONVCC-TRANSFER                                    
179300     OR  DIST35-NA-TRANSFER                                               
179400     OR  DIST35-NA-NDC-RETURNS                                            
179500     OR  DIST35-PACIFIC-TRANSFER                                          
179600     OR  DIST35-REFILL-INOM-JP                                            
179700     OR  DIST35-CN-TRANSFER                                               
179800     OR  DIST35-NONVCC-VCC-REFILL                                         
179810     OR  DIST35-NONVCC-VCC-TRANSFER                                       
179900     OR  RADER-FINNS                                                      
180000                                                                          
180100       IF RADER-FINNS                                                     
180200         MOVE TP4TRAN-IDDC-REC  TO W-WDK711-IDDC                          
180300       ELSE                                                               
180400         SEARCH ALL DIST57-REFILL-DC                                      
180500            AT END                                                        
180600               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
180700                                TO FELTEXT                                
180800               CALL FELLOG                                                
180900            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR             
181000               MOVE DIST57-REFILL-TO-DC(DIST57-IX)                        
181100                                TO W-WDK711-IDDC                          
181200         END-SEARCH                                                       
181300       END-IF                                                             
181400                                                                          
181500       MOVE IDARTNR-WS        TO W-WDK711-IDARTNR-N                       
181600       PERFORM IMS-GHU-WDK711                                             
181700       SUBTRACT SPAR-KVART    FROM SLAG-KVBEART                           
181800                                                                          
181900       PERFORM IMS-REPL-WDK711                                            
182000     ELSE                                                                 
182100       IF DIST35-NONVCC-CDC-REFILL                                        
182200                                                                          
182300          SEARCH ALL DIST57-REFILL-DC                                     
182400            AT END                                                        
182500               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
182600                                TO FELTEXT                                
182700               CALL FELLOG                                                
182800            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR             
182900               MOVE DIST57-REFILL-TO-DC(DIST57-IX)                        
183000                                TO W-WDK711-IDDC                          
183100          END-SEARCH                                                      
183200                                                                          
183300          MOVE IDARTNR-WS        TO W-IDARTNR-ARTC01                      
183400          PERFORM IMS-GHU-ARTC11                                          
183500          SUBTRACT SPAR-KVART    FROM CLAG-KVBEART                        
183600                                                                          
183700       PERFORM IMS-REPL-ARTK611                                           
183800       END-IF                                                             
183900     END-IF                                                               
184000     .                                                                    
184100     EJECT                                                                
184200 F-PRIO SECTION.                                                          
184300* I DENNA SECTION ÖKAS PRIO TILL 10 MED HJÄLP AV REPL PÅ RADEN.           
184400* DET LÄGGS ÄVEN UT EN RAD PÅ LOGGEN                                      
184500     MOVE    MID-IDDISTR-SPAR-E   TO        IDDISTR-WS                    
184600     MOVE    MID-IDKUNDNR (INDX)  TO        IDKUNDNR-WS                   
184700     MOVE    MID-IDORDNR  (INDX)  TO        IDORDNR-WS                    
184800     MOVE    MID-IDARTNR  (INDX)  TO        IDARTNR-WS                    
184900     MOVE    MID-IDLOPNR  (INDX)  TO        IDLOPNR-WS                    
185000                                                                          
185100     INSPECT IDDISTR-WS           REPLACING LEADING SPACE BY ZERO         
185200     INSPECT IDKUNDNR-WS          REPLACING LEADING SPACE BY ZERO         
185300     INSPECT IDARTNR-WS           REPLACING LEADING SPACE BY ZERO         
185400     INSPECT IDORDNR-WS           REPLACING LEADING SPACE BY ZERO         
185500     INSPECT IDLOPNR-WS           REPLACING LEADING SPACE BY ZERO         
185600                                                                          
185700     MOVE    IDDISTR-WS           TO        W-IDDISTR-N2                  
185800     MOVE    IDKUNDNR-WS          TO        W-IDKUNDNR-N2                 
185900     MOVE    IDORDNR-WS           TO        W-IDORDNR-N2                  
186000     MOVE    IDARTNR-WS           TO        W-IDARTNR-N2                  
186100     MOVE    IDLOPNR-WS           TO        W-IDLOPNR-N2                  
186200                                                                          
186300     PERFORM IMS-GHU-RO                                                   
186400     MOVE    +10                  TO        RAD-KDRAPRIO                  
186500     PERFORM IMS-REPL-RO                                                  
186600     .                                                                    
186700     EJECT                                                                
186800 G1-FLYTTA-NYCKLAR SECTION.                                               
186900                                                                          
187000     MOVE LOW-VALUE  TO W-WDA5D1KY-MIN                                    
187100                        W-SOK-NYCKLAR-MIN                                 
187200     MOVE HIGH-VALUE TO W-WDA5D1KY-MAX                                    
187300                        W-SOK-NYCKLAR-MAX                                 
187400     MOVE '2'        TO W-KDSTARAD-MAX                                    
187500     MOVE MID-IDDISTR-SPAR-E TO W-IDDISTR-N1-MIN                          
187600                                W-IDDISTR-N1-MAX                          
187700     IF W-IDKUNDNR > ZERO                                                 
187800       MOVE W-IDKUNDNR TO W-IDKUNDNR-N1-MAX                               
187900       IF W-IDARTNR > ZERO                                                
188000         MOVE W-IDARTNR TO W-IDARTNR-N1-MAX                               
188100         IF W-IDORDNR > ZERO                                              
188200           MOVE W-IDKUNDRF TO W-IDKUNDRF-N1-MAX                           
188300         END-IF                                                           
188400       END-IF                                                             
188500     END-IF                                                               
188600                                                                          
188700     IF W-IDARTNR > ZERO                                                  
188800       MOVE W-IDARTNR   TO W-IDARTNR-MAX                                  
188900     END-IF                                                               
189000     IF W-IDORDNR > ZERO                                                  
189100       MOVE W-IDKUNDRF  TO W-IDKUNDRF-MAX                                 
189200     END-IF                                                               
189300                                                                          
189400     IF MID-IDKUNDNR-SPAR-E > ZERO                                        
189500       MOVE MID-IDKUNDNR-SPAR-E TO W-IDKUNDNR-N1-MIN                      
189600       IF MID-IDARTNR-SPAR-E > ZERO                                       
189700         MOVE MID-IDARTNR-SPAR-E TO W-IDARTNR-N1-MIN                      
189800         IF MID-IDORDNR-SPAR-E > ZERO                                     
189900           MOVE MID-IDORDNR-SPAR-E TO W-IDKUNDRF-N1-MIN                   
190000         END-IF                                                           
190100       END-IF                                                             
190200     END-IF                                                               
190300                                                                          
190400     IF MID-IDARTNR-SPAR-E > ZERO                                         
190500       MOVE MID-IDARTNR-SPAR-E TO W-IDARTNR-MIN                           
190600        IF MID-IDARTNR-UT > ZERO                                          
190700          MOVE MID-IDARTNR-SPAR-E TO W-IDARTNR-MAX                        
190800        END-IF                                                            
190900     END-IF                                                               
191000     IF MID-IDORDNR-SPAR-E > ZERO                                         
191100       MOVE MID-IDORDNR-SPAR-E TO W-IDKUNDRF-MIN                          
191200        IF MID-IDORDNR-UT > ZERO                                          
191300          MOVE MID-IDORDNR-SPAR-E TO W-IDKUNDRF-MAX                       
191400        END-IF                                                            
191500     END-IF                                                               
191600     IF MID-KDORDKL-SPAR-E  > ZERO                                        
191700       MOVE MID-KDORDKL-SPAR-E TO W-KDORDKL-MIN                           
191800        IF MID-KDORDKL-UT > ZERO                                          
191900          MOVE MID-KDORDKL-SPAR-E TO W-KDORDKL-MAX                        
192000        END-IF                                                            
192100     END-IF                                                               
192200     IF MID-KDPRODSL-SPAR-E > ZERO                                        
192300       MOVE MID-KDPRODSL-SPAR-E TO W-KDPRODSL-MIN                         
192400        IF MID-KDPRODSL-UT > ZERO                                         
192500          MOVE MID-KDPRODSL-SPAR-E TO W-KDPRODSL-MAX                      
192600        END-IF                                                            
192700     END-IF                                                               
192800     IF MID-KDTPOTYP-SPAR-E > ZERO                                        
192900       MOVE MID-KDTPOTYP-SPAR-E TO W-KDTPOTYP-MIN                         
193000       IF MID-KDTPOTYP-UT > ZERO                                          
193100         MOVE MID-KDTPOTYP-SPAR-E TO W-KDTPOTYP-MAX                       
193200       END-IF                                                             
193300     END-IF                                                               
193400     IF MID-IDDC-SPAR-E > ZERO                                            
193500       MOVE MID-IDDC-SPAR-E TO W-IDDC-MIN                                 
193600       IF MID-IDDC-UT > ZERO                                              
193700         MOVE MID-IDDC-SPAR-E TO W-IDDC-MAX                               
193800       END-IF                                                             
193900     END-IF                                                               
194000     .                                                                    
194100     EJECT                                                                
194200 G2-FLYTTA-NYCKLAR SECTION.                                               
194300                                                                          
194400     MOVE LOW-VALUE  TO W-WDA5D1KY-MIN                                    
194500                        W-SOK-NYCKLAR-MIN                                 
194600     MOVE HIGH-VALUE TO W-WDA5D1KY-MAX                                    
194700                        W-SOK-NYCKLAR-MAX                                 
194800     MOVE '2'        TO W-KDSTARAD-MAX                                    
194900****** KOMMANDE IF-SATS KOLLAR OM DET ÄR FÖRSTA GÅNGEN MAN                
195000****** TRYCKER ENTER, ISÅFALL LÄSER MAN FRÅN INMATNINGSRADEN              
195100****** OM INTE SÅ LÄSER MAN FRÅN SPARADE NYCKLAR RAD 20                   
195200     IF MID-IDDISTR-IN  NOT = ALL '+' OR                                  
195300        MID-IDKUNDNR-IN NOT = ALL '+' OR                                  
195400        MID-IDARTNR-IN  NOT = ALL '+' OR                                  
195500        MID-KDORDKL-IN  NOT = ALL '+' OR                                  
195600        MID-KDPRODSL-IN NOT = ALL '+' OR                                  
195700        MID-IDORDNR-IN  NOT = ALL '+' OR                                  
195800        MID-KDTPOTYP-IN NOT = ALL '+' OR                                  
195900        MID-IDDC-IN     NOT = ALL '+' OR                                  
196000        MID-IDDISTR-SPAR-E  = ALL '0'                                     
196100        MOVE JA              TO W-NEWKEY                                  
196200        MOVE W-IDDISTR       TO W-IDDISTR-N1-MIN                          
196300                                W-IDDISTR-N1-MAX                          
196400       IF MID-IDDISTR-IN NOT = ALL '+'                                    
196500         IF W-IDKUNDNR         > ZERO                                     
196600           MOVE W-IDKUNDNR   TO W-IDKUNDNR-N1-MIN                         
196700                                W-IDKUNDNR-N1-MAX                         
196800         END-IF                                                           
196900         IF W-IDARTNR          > ZERO                                     
197000            MOVE W-IDARTNR    TO W-IDARTNR-MIN                            
197100                                 W-IDARTNR-MAX                            
197200                                 W-IDARTNR-N1-MIN                         
197300                                 W-IDARTNR-N1-MAX                         
197400         END-IF                                                           
197500         IF W-IDORDNR          > ZERO                                     
197600            MOVE W-IDORDNR    TO W-IDKUNDRF-1-5                           
197700            MOVE W-IDKUNDRF   TO W-IDKUNDRF-MIN                           
197800                                 W-IDKUNDRF-MAX                           
197900                                 W-IDKUNDRF-N1-MIN                        
198000                                 W-IDKUNDRF-N1-MAX                        
198100         END-IF                                                           
198200         IF W-IDARTNR          > ZERO                                     
198300            MOVE W-IDARTNR    TO W-IDARTNR-MIN                            
198400                                 W-IDARTNR-MAX                            
198500                                 W-IDARTNR-N1-MIN                         
198600                                 W-IDARTNR-N1-MAX                         
198700         END-IF                                                           
198800         IF W-KDORDKL          > ZERO                                     
198900            MOVE W-KDORDKL    TO W-KDORDKL-MIN                            
199000                                 W-KDORDKL-MAX                            
199100         END-IF                                                           
199200         IF W-KDPRODSL         > ZERO                                     
199300            MOVE W-KDPRODSL   TO W-KDPRODSL-MIN                           
199400                                 W-KDPRODSL-MAX                           
199500         END-IF                                                           
199600         IF W-KDTPOTYP         > ZERO                                     
199700            MOVE W-KDTPOTYP   TO W-KDTPOTYP-MIN                           
199800                                 W-KDTPOTYP-MAX                           
199900         END-IF                                                           
200000         IF W-IDDC             > ZERO                                     
200100            MOVE W-IDDC       TO W-IDDC-MIN                               
200200                                 W-IDDC-MAX                               
200300         END-IF                                                           
200400       ELSE                                                               
200500        IF W-IDKUNDNR         > ZERO                                      
200600           MOVE W-IDKUNDNR   TO W-IDKUNDNR-N1-MIN                         
200700                                W-IDKUNDNR-N1-MAX                         
200800        END-IF                                                            
200900        IF W-IDARTNR          > ZERO                                      
201000           MOVE W-IDARTNR    TO W-IDARTNR-MIN                             
201100                                W-IDARTNR-MAX                             
201200                                W-IDARTNR-N1-MIN                          
201300                                W-IDARTNR-N1-MAX                          
201400        END-IF                                                            
201500        IF W-IDORDNR          > ZERO                                      
201600           MOVE W-IDORDNR    TO W-IDKUNDRF-1-5                            
201700           MOVE W-IDKUNDRF   TO W-IDKUNDRF-MIN                            
201800                                W-IDKUNDRF-MAX                            
201900                                W-IDKUNDRF-N1-MIN                         
202000                                W-IDKUNDRF-N1-MAX                         
202100        END-IF                                                            
202200        IF W-KDORDKL          > ZERO                                      
202300           MOVE W-KDORDKL    TO W-KDORDKL-MIN                             
202400                                W-KDORDKL-MAX                             
202500        END-IF                                                            
202600        IF W-KDPRODSL         > ZERO                                      
202700           MOVE W-KDPRODSL   TO W-KDPRODSL-MIN                            
202800                                W-KDPRODSL-MAX                            
202900        END-IF                                                            
203000        IF W-KDTPOTYP         > ZERO                                      
203100           MOVE W-KDTPOTYP   TO W-KDTPOTYP-MIN                            
203200                                W-KDTPOTYP-MAX                            
203300        END-IF                                                            
203400        IF W-IDDC             > ZERO                                      
203500           MOVE W-IDDC       TO W-IDDC-MIN                                
203600                                W-IDDC-MAX                                
203700        END-IF                                                            
203800       END-IF                                                             
203900     EJECT                                                                
204000*    VID ENTER UTAN NYA NYCKLAR                                           
204100     ELSE                                                                 
204200                                                                          
204300         MOVE NEJ                TO W-NEWKEY                              
204400         MOVE MID-IDDISTR-SPAR-E TO W-IDDISTR-N1-MIN                      
204500                                    W-IDDISTR-N1-MAX                      
204600         IF W-IDKUNDNR         > ZERO                                     
204700            MOVE W-IDKUNDNR TO W-IDKUNDNR-N1-MAX                          
204800            IF W-IDARTNR        > ZERO                                    
204900               MOVE W-IDARTNR TO W-IDARTNR-N1-MAX                         
205000               IF W-IDORDNR        > ZERO                                 
205100                    MOVE W-IDKUNDRF     TO W-IDKUNDRF-N1-MAX              
205200               END-IF                                                     
205300            END-IF                                                        
205400         END-IF                                                           
205500                                                                          
205600         IF W-IDARTNR        > ZERO                                       
205700            MOVE W-IDARTNR   TO W-IDARTNR-MAX                             
205800         END-IF                                                           
205900         IF W-IDORDNR        > ZERO                                       
206000            MOVE W-IDKUNDRF  TO W-IDKUNDRF-MAX                            
206100         END-IF                                                           
206200                                                                          
206300         IF MID-IDKUNDNR-SPAR-E > ZERO                                    
206400            MOVE MID-IDKUNDNR-SPAR-E TO W-IDKUNDNR-N1-MIN                 
206500            IF MID-IDARTNR-SPAR-E > ZERO                                  
206600               MOVE MID-IDARTNR-SPAR-E TO W-IDARTNR-N1-MIN                
206700               IF MID-IDORDNR-SPAR-E > ZERO                               
206800                    MOVE MID-IDORDNR-SPAR-E TO W-IDKUNDRF-N1-MIN          
206900               END-IF                                                     
207000            END-IF                                                        
207100         END-IF                                                           
207200                                                                          
207300         IF MID-IDARTNR-SPAR-E > ZERO                                     
207400           MOVE MID-IDARTNR-SPAR-E   TO W-IDARTNR-MIN                     
207500             IF MID-IDARTNR-UT > ZERO                                     
207600               MOVE MID-IDARTNR-SPAR-E   TO W-IDARTNR-MAX                 
207700             END-IF                                                       
207800         END-IF                                                           
207900         IF MID-IDORDNR-SPAR-E > ZERO                                     
208000           MOVE MID-IDORDNR-SPAR-E    TO W-IDKUNDRF-MIN                   
208100             IF MID-IDORDNR-UT > ZERO                                     
208200               MOVE MID-IDORDNR-SPAR-E    TO W-IDKUNDRF-MAX               
208300             END-IF                                                       
208400         END-IF                                                           
208500         IF MID-KDORDKL-SPAR-E  > ZERO                                    
208600            MOVE MID-KDORDKL-SPAR-E  TO W-KDORDKL-MIN                     
208700             IF MID-KDORDKL-UT > ZERO                                     
208800                MOVE MID-KDORDKL-SPAR-E  TO W-KDORDKL-MAX                 
208900             END-IF                                                       
209000         END-IF                                                           
209100         IF MID-KDPRODSL-SPAR-E > ZERO                                    
209200            MOVE MID-KDPRODSL-SPAR-E TO W-KDPRODSL-MIN                    
209300             IF MID-KDPRODSL-UT > ZERO                                    
209400                MOVE MID-KDPRODSL-SPAR-E TO W-KDPRODSL-MAX                
209500             END-IF                                                       
209600         END-IF                                                           
209700         IF MID-KDTPOTYP-SPAR-E > ZERO                                    
209800            MOVE MID-KDTPOTYP-SPAR-E TO W-KDTPOTYP-MIN                    
209900             IF MID-KDTPOTYP-UT > ZERO                                    
210000                MOVE MID-KDTPOTYP-SPAR-E TO W-KDTPOTYP-MAX                
210100             END-IF                                                       
210200         END-IF                                                           
210300         IF MID-IDDC-SPAR-E > ZERO                                        
210400            MOVE MID-IDDC-SPAR-E TO W-IDDC-MIN                            
210500             IF MID-IDDC-UT > ZERO                                        
210600                MOVE MID-IDDC-SPAR-E TO W-IDDC-MAX                        
210700             END-IF                                                       
210800         END-IF                                                           
210900     END-IF.                                                              
211000     EJECT                                                                
211100 H-OPPNA SECTION.                                                         
211200* I DENNA SEKTION FLYTTAS MARKERAD RAD TILL RAD 1, FÖRÄNDRINGS-           
211300* RADEN ÖPPNAS OCH ÖVRIGA RENSAS                                          
211400     MOVE    MID-IDDISTR-UT       TO        IDDISTR-WS                    
211500     MOVE    MID-IDKUNDNR (INDX)  TO        IDKUNDNR-WS                   
211600     MOVE    MID-IDORDNR  (INDX)  TO        IDORDNR-WS                    
211700     MOVE    MID-IDARTNR  (INDX)  TO        IDARTNR-WS                    
211800     MOVE    MID-IDLOPNR  (INDX)  TO        IDLOPNR-WS                    
211900                                                                          
212000     INSPECT IDDISTR-WS           REPLACING LEADING SPACE BY ZERO         
212100     INSPECT IDKUNDNR-WS          REPLACING LEADING SPACE BY ZERO         
212200     INSPECT IDARTNR-WS           REPLACING LEADING SPACE BY ZERO         
212300     INSPECT IDORDNR-WS           REPLACING LEADING SPACE BY ZERO         
212400     INSPECT IDLOPNR-WS           REPLACING LEADING SPACE BY ZERO         
212500                                                                          
212600     MOVE    IDDISTR-WS           TO        W-IDDISTR-N2                  
212700                                            TEST-IDDISTR                  
212800     MOVE    IDKUNDNR-WS          TO        W-IDKUNDNR-N2                 
212900     MOVE    IDORDNR-WS           TO        W-IDORDNR-N2                  
213000     MOVE    IDARTNR-WS           TO        W-IDARTNR-N2                  
213100     MOVE    IDLOPNR-WS           TO        W-IDLOPNR-N2                  
213200                                                                          
213300     PERFORM IMS-GU-RO                                                    
213400                                                                          
213500                                                                          
213600     IF SEGMENT-FINNS                                                     
213700        IF DIST79-DEALER-PRICE                                            
213900          IF RAD-KDVALISO              > SPACE                            
214000            MOVE RAD-KDVALISO     TO  MOD-KDVALISO                        
214100          ELSE                                                            
214200            MOVE RAD-IDDISTR      TO W-IDDISTR-B201                       
214300                                   W-IDDISTR-B201-MIN                     
214400                                   W-IDDISTR-B201-MAX                     
214500            MOVE RAD-IDKUNDNR     TO W-IDKUNDNR-B201                      
214600            PERFORM S11-HAMTA-KDVALISO-TILL-MOD                           
214700          END-IF                                                          
214800        END-IF                                                            
214900        IF DIST79-ECOM-PRICE                                              
215000          IF RAD-KDVALISO              > SPACE                            
215100            MOVE RAD-KDVALISO     TO  MOD-KDVALISO                        
215200          ELSE                                                            
215300            MOVE SPACE            TO  MOD-KDVALISO                        
215400          END-IF                                                          
215500        END-IF                                                            
215600        PERFORM S07-LAES-ART                                              
215700        MOVE CLAG-KVQPACK-1       TO  MOD-KVQPACK-1(+1)                   
215800        MOVE RAD-IDKUNDNR         TO  MOD-IDKUNDNR(+1)                    
215900        MOVE RAD-IDARTNR          TO  MOD-IDARTNR(+1)                     
216000        MOVE RAD-KVART            TO  MOD-KVART(+1)                       
216100        MOVE RAD-IDKUNDRF         TO  W-IDKUNDRF                          
216200        MOVE W-IDKUNDRF-1-5       TO  MOD-IDORDNR(+1)                     
216300        MOVE RAD-KDORDKL          TO  MOD-KDORDKL(+1)                     
216400        MOVE RAD-KDFRAKT          TO  MOD-KDFRAKT(+1)                     
216500        IF SEC-KDSVAR = '2' OR '6'                                        
216600           CONTINUE                                                       
216700        ELSE                                                              
216800          IF DIST79-DEALER-PRICE OR                                       
217000             DIST79-ECOM-PRICE                                            
217100            IF RAD-PRARTNTO-LOC > +0                                      
217200              MOVE RAD-PRARTNTO-LOC     TO MOD-PRARTNTO(+1)               
217300              MOVE ' '                  TO MOD-TEASTRIX(+1)               
217400            ELSE                                                          
217500              MOVE RAD-PRARTNTO-LOCPREL TO MOD-PRARTNTO(+1)               
217600              MOVE '*'                  TO MOD-TEASTRIX(+1)               
217700            END-IF                                                        
217800          ELSE                                                            
217900            IF RAD-PRAVCOST > 0                                           
218000              MOVE RAD-PRAVCOST TO MOD-PRARTNTO(+1)                       
218100              MOVE RAD-KDVALISO TO MOD-KDVALISO                           
218200            ELSE                                                          
218300              MOVE RAD-PRARTNTO TO MOD-PRARTNTO(+1)                       
218400              MOVE RAD-KDVALISO TO MOD-KDVALISO                           
218500            END-IF                                                        
218600            MOVE ' '           TO MOD-TEASTRIX(+1)                        
218700          END-IF                                                          
218800        END-IF                                                            
218900        MOVE RAD-KDRAPRIO         TO  MOD-KDRAPRIO(+1)                    
219000        MOVE RAD-IDDC             TO  MOD-IDDC(+1)                        
219100        MOVE RAD-TITPO            TO  MOD-TITPO(+1)                       
219200        MOVE RAD-IDLOPNR          TO  MOD-IDLOPNR(+1)                     
219300                                                                          
219400        IF INOM-FRYSTID AND NOT (SEC-KDSVAR = ' ')                        
219500           IF NOT DIST19-SATS                                             
219600              MOVE MFS-OEPPNA-NUM-FAELT TO  MOD-KDFRAKT-OPP-ATTR          
219700              MOVE MFS-OEPPNA-NUM-FAELT TO  MOD-KDORDKL-OPP-ATTR          
219800           ELSE                                                           
219900              MOVE MFS-OEPPNA-NUM-FAELT TO  MOD-KDFRAKT-OPP-ATTR          
220000           END-IF                                                         
220100        ELSE                                                              
220200          MOVE MFS-OEPPNA-NUM-FAELT TO  MOD-KDFRAKT-OPP-ATTR              
220300          IF NOT DIST19-SATS                                              
220400             MOVE MFS-OEPPNA-NUM-FAELT TO  MOD-KVART-OPP-ATTR             
220500             MOVE MFS-OEPPNA-NUM-FAELT TO  MOD-KDORDKL-OPP-ATTR           
220600          END-IF                                                          
220700          IF SEC-KDSVAR  = ' '                                            
220800             MOVE MFS-OEPPNA-NUM-FAELT TO  MOD-PRARTNTO-OPP-ATTR          
220900          END-IF                                                          
221000        END-IF                                                            
221100     ELSE                                                                 
221200        MOVE TEXT-0403 (SPRAK-IX) TO MOD-TEMFSFEL                         
221300        MOVE ' '                  TO MOD-KDVALISO                         
221400     END-IF                                                               
221500     MOVE +1                    TO MOD-KVRAD-SPAR                         
221600                                                                          
221700     MOVE +2 TO INDX                                                      
221800     PERFORM UNTIL INDX > MAX-LINES                                       
221900        MOVE MFS-RENSA-FAELT TO                                           
222000                             MOD-IDKUNDNR(INDX)                           
222100                             MOD-IDARTNR(INDX)                            
222200                             MOD-KVART(INDX)                              
222300                             MOD-IDORDNR(INDX)                            
222400                             MOD-KDORDKL(INDX)                            
222500                             MOD-KDFRAKT(INDX)                            
222600                             MOD-TITPO(INDX)                              
222700                             MOD-PRARTNTO(INDX)                           
222800                             MOD-TEASTRIX(INDX)                           
222900                             MOD-KVQPACK-1(INDX)                          
223000                             MOD-KDRAPRIO(INDX)                           
223100                             MOD-IDDC(INDX)                               
223200                             MOD-IDLOPNR(INDX)                            
223300        ADD +1 TO INDX                                                    
223400     END-PERFORM                                                          
223500     .                                                                    
223600     EJECT                                                                
223700 I-LAES-FLYTTA-TILL-MOD SECTION.                                          
223800                                                                          
223900     MOVE +1 TO INDX                                                      
224000*           ADD  1              TO DEBUGG-FLT                             
224100     PERFORM UNTIL SEGMENT-SAKNAS          OR                             
224200                   SEGMENT-SLUT            OR                             
224300                   INDX           > MAX-LINES                             
224400                                                                          
224500       IF SEGMENT-FINNS                                                   
224600*           ADD  3              TO DEBUGG-FLT                             
224700         PERFORM S10-FLYTTA-TILL-NYCKLAR                                  
224800         PERFORM IMS-GHU-RO                                               
224900         IF SEGMENT-FINNS                                                 
225000           PERFORM S07-LAES-ART                                           
225100*           ADD  9              TO DEBUGG-FLT                             
225200         END-IF                                                           
225300         PERFORM S08-FLYTTA-TILL-MOD                                      
225400         ADD +1 TO W-KVRAD                                                
225500       END-IF                                                             
225600       ADD +1 TO INDX                                                     
225700       PERFORM IMS-GN-INDEX-RO                                            
225800     END-PERFORM                                                          
225900                                                                          
226000     MOVE W-KVRAD        TO MOD-KVRAD-SPAR                                
226100     IF SEGMENT-FINNS                                                     
226200       PERFORM S09-FLYTTA-TILL-SPAR-MOD                                   
226300     END-IF                                                               
226400                                                                          
226500     PERFORM UNTIL INDX > MAX-LINES                                       
226600       MOVE MFS-RENSA-FAELT TO                                            
226700                               MOD-IDKUNDNR(INDX)                         
226800                               MOD-IDARTNR(INDX)                          
226900                               MOD-KVART(INDX)                            
227000                               MOD-IDORDNR(INDX)                          
227100                               MOD-KDORDKL(INDX)                          
227200                               MOD-KDFRAKT(INDX)                          
227300                               MOD-TITPO(INDX)                            
227400                               MOD-PRARTNTO(INDX)                         
227500                               MOD-TEASTRIX(INDX)                         
227600                               MOD-KVQPACK-1(INDX)                        
227700                               MOD-KDRAPRIO(INDX)                         
227800                               MOD-IDDC(INDX)                             
227900                               MOD-IDLOPNR(INDX)                          
228000        ADD +1 TO INDX                                                    
228100     END-PERFORM                                                          
228200                                                                          
228300     .                                                                    
228400     EJECT                                                                
228500 K-FLYTTA-FELTEXT SECTION.                                                
228600                                                                          
228700     IF WS-GODKAEND-BILD                                                  
228800        IF OPP-FAELT-OK = NEJ                                             
228900           MOVE TEXT-0409     (SPRAK-IX) TO MOD-TEMFSFEL                  
229000*          MOVE MID-RAD       (+1)       TO W-SPAR-RAD                    
229100*          MOVE W-SPAR-RAD               TO MOD-RAD       (+1)            
229200           MOVE MID-IDKUNDNR  (+1)       TO MOD-IDKUNDNR  (+1)            
229300           MOVE MID-IDARTNR   (+1)       TO MOD-IDARTNR   (+1)            
229400           MOVE MID-KVART     (+1)       TO MOD-KVART     (+1)            
229500           MOVE MID-IDORDNR   (+1)       TO MOD-IDORDNR   (+1)            
229600           MOVE MID-KDORDKL   (+1)       TO MOD-KDORDKL   (+1)            
229700           MOVE MID-KDFRAKT   (+1)       TO MOD-KDFRAKT   (+1)            
229800           MOVE MID-TITPO     (+1)       TO MOD-TITPO     (+1)            
229900           MOVE MID-PRARTNTO  (+1)       TO WW-SPAR-PRARTNTO              
230000           MOVE W-PRARTNTO               TO MOD-PRARTNTO  (+1)            
230100           MOVE MID-KVQPACK-1 (+1)       TO MOD-KVQPACK-1 (+1)            
230200           MOVE MID-KDRAPRIO  (+1)       TO MOD-KDRAPRIO  (+1)            
230300           MOVE MID-IDDC      (+1)       TO MOD-IDDC      (+1)            
230400           MOVE MID-IDLOPNR   (+1)       TO MOD-IDLOPNR   (+1)            
230500           MOVE +1                       TO MOD-KVRAD-SPAR                
230600           IF MID-KVART-OPP = ALL '+'                                     
230700              MOVE MFS-RENSA-FAELT       TO MOD-KVART-OPP                 
230800           ELSE                                                           
230900              MOVE KVART-OPP             TO W9-UT-KVART-OPP               
231000              MOVE W9-UT-KVART-OPP       TO MOD-KVART-OPP                 
231100           END-IF                                                         
231200           IF MID-KDORDKL-OPP   = ALL '+'                                 
231300              MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-OPP                     
231400           ELSE                                                           
231500              MOVE KDORDKL-OPP           TO W9-UT-KDORDKL-OPP             
231600              MOVE W9-UT-KDORDKL-OPP     TO MOD-KDORDKL-OPP               
231700           END-IF                                                         
231800           IF MID-KDFRAKT-OPP   = ALL '+'                                 
231900              MOVE MFS-RENSA-FAELT TO MOD-KDFRAKT-OPP                     
232000           ELSE                                                           
232100              MOVE KDFRAKT-OPP           TO W9-UT-KDFRAKT-OPP             
232200              MOVE W9-UT-KDFRAKT-OPP     TO MOD-KDFRAKT-OPP               
232300           END-IF                                                         
232400           IF MID-PRARTNTO-OPP   = ALL '+'                                
232500              MOVE MFS-RENSA-FAELT TO MOD-PRARTNTO-OPP                    
232600           ELSE                                                           
232700              MOVE SPAR-PRARTNTO         TO W9-UT-PRARTNTO-OPP            
232800              MOVE W9-UT-PRARTNTO-OPP    TO MOD-PRARTNTO-OPP              
232900           END-IF                                                         
233000           MOVE RAD-IDDISTR             TO MOD-IDDISTR-SPAR-E             
233100           MOVE RAD-IDKUNDNR            TO MOD-IDKUNDNR-SPAR-E            
233200           MOVE RAD-IDARTNR             TO MOD-IDARTNR-SPAR-E             
233300           MOVE RAD-IDKUNDRF            TO W-IDKUNDRF                     
233400           MOVE W-IDKUNDRF-1-5          TO MOD-IDORDNR-SPAR-E             
233500           MOVE RAD-KDORDKL             TO MOD-KDORDKL-SPAR-E             
233600           MOVE RAD-KDPRODSL            TO MOD-KDPRODSL-SPAR-E            
233700           MOVE RAD-KDTPOTYP            TO MOD-KDTPOTYP-SPAR-E            
233800           MOVE RAD-IDDC                TO MOD-IDDC-SPAR-E                
233900*          MOVE MFS-OEPPNA-NUM-FAELT    TO MOD-KVART-OPP-ATTR             
234000*                                          MOD-KDFRAKT-OPP-ATTR           
234100*          IF NOT DIST19-SATS                                             
234200*            MOVE MFS-OEPPNA-NUM-FAELT  TO  MOD-KDORDKL-OPP-ATTR          
234300*          END-IF                                                         
234400*          IF SEC-KDSVAR  = ' '                                           
234500*            MOVE MFS-OEPPNA-NUM-FAELT  TO  MOD-PRARTNTO-OPP-ATTR         
234600*          END-IF                                                         
234700           IF ANTAL-OK = NEJ                                              
234800             MOVE MFS-NUM-FAELT-FEL     TO MOD-KVART-OPP-ATTR             
234900           ELSE                                                           
235000            IF NOT DIST19-SATS                                            
235100              MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVART-OPP-ATTR             
235200            END-IF                                                        
235300           END-IF                                                         
235400           IF KLASS-OK = NEJ                                              
235500               MOVE MFS-NUM-FAELT-FEL TO MOD-KDORDKL-OPP-ATTR             
235600           ELSE                                                           
235700              IF NOT DIST19-SATS                                          
235800                 MOVE MFS-NUM-FAELT-RAETT TO MOD-KDORDKL-OPP-ATTR         
235900              END-IF                                                      
236000           END-IF                                                         
236100           IF FRAKT-OK = NEJ                                              
236200               MOVE MFS-NUM-FAELT-FEL TO MOD-KDFRAKT-OPP-ATTR             
236300           ELSE                                                           
236400               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFRAKT-OPP-ATTR           
236500           END-IF                                                         
236600           IF PRIS-OK = NEJ                                               
236700               MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-OPP-ATTR            
236800           ELSE                                                           
236900              IF SEC-KDSVAR  = ' '                                        
237000                MOVE MFS-NUM-FAELT-RAETT TO MOD-PRARTNTO-OPP-ATTR         
237100              END-IF                                                      
237200           END-IF                                                         
237300           IF KVQPACK-OK = NEJ                                            
237400               MOVE MFS-NUM-FAELT-FEL TO MOD-KVART-OPP-ATTR               
237500               MOVE TEXT-0419 (SPRAK-IX) TO MOD-TEMFSFEL                  
237600           END-IF                                                         
237700        END-IF                                                            
237800        IF VALKOD-OK = NEJ                                                
237900            MOVE TEXT-0416 (SPRAK-IX) TO MOD-TEMFSFEL                     
238000            MOVE +1 TO INDX                                               
238100            PERFORM G1-FLYTTA-NYCKLAR                                     
238200            PERFORM N-LAES                                                
238300        END-IF                                                            
238400        IF INPUT-OK = NEJ                                                 
238500           IF SEC-KDSVAR NOT = ' ' AND                                    
238600              SEC-KDSVAR NOT = '1' AND                                    
238700              SEC-KDSVAR NOT = '2' AND                                    
238800              SEC-KDSVAR NOT = '3' AND                                    
238900              SEC-KDSVAR NOT = '5' AND                                    
239000              SEC-KDSVAR NOT = '6'                                        
239100               MOVE TEXT-0405 (SPRAK-IX) TO MOD-TEMFSFEL                  
239200           ELSE                                                           
239300               MOVE TEXT-0401 (SPRAK-IX) TO MOD-TEMFSFEL                  
239400           END-IF                                                         
239500        END-IF                                                            
239600        IF AENDRA-ENTER = NEJ                                             
239700           MOVE TEXT-0407 (SPRAK-IX) TO MOD-TEMFSFEL                      
239800           MOVE MID-IDDISTR-SPAR-E    TO MOD-IDDISTR-SPAR-E               
239900           MOVE MID-IDKUNDNR-SPAR-E   TO MOD-IDKUNDNR-SPAR-E              
240000           MOVE MID-IDARTNR-SPAR-E    TO MOD-IDARTNR-SPAR-E               
240100           MOVE MID-IDORDNR-SPAR-E    TO MOD-IDORDNR-SPAR-E               
240200           MOVE MID-KDORDKL-SPAR-E    TO MOD-KDORDKL-SPAR-E               
240300           MOVE MID-KDPRODSL-SPAR-E   TO MOD-KDPRODSL-SPAR-E              
240400           MOVE MID-KDTPOTYP-SPAR-E   TO MOD-KDTPOTYP-SPAR-E              
240500           MOVE MID-IDDC-SPAR-E       TO MOD-IDDC-SPAR-E                  
240600        END-IF                                                            
240700                                                                          
240800     END-IF                                                               
240900     .                                                                    
241000     EJECT                                                                
241100 L-RENSA-OPP-FAELT SECTION.                                               
241200                                                                          
241300     MOVE MFS-RENSA-FAELT TO MOD-KVART-OPP                                
241400                             MOD-KDORDKL-OPP                              
241500                             MOD-KDFRAKT-OPP                              
241600                             MOD-PRARTNTO-OPP                             
241700     .                                                                    
241800     EJECT                                                                
241900 M-LAES            SECTION.                                               
242000                                                                          
242100     PERFORM IMS-GN-INDEX-RO                                              
242200     IF SEGMENT-FINNS                                                     
242300                                                                          
242400       IF DIST79-DEALER-PRICE                                             
242600         MOVE SEQD-IDDISTR      TO W-IDDISTR-B201                         
242700                                   W-IDDISTR-B201-MIN                     
242800                                   W-IDDISTR-B201-MAX                     
242900         MOVE SEQD-IDKUNDNR     TO W-IDKUNDNR-B201                        
243000         MOVE SEQD-IDDC         TO W-IDDC-B6                              
243100         PERFORM S11-HAMTA-KDVALISO-TILL-MOD                              
243200       END-IF                                                             
243300       PERFORM I-LAES-FLYTTA-TILL-MOD                                     
243400     ELSE                                                                 
243500         MOVE TEXT-0403 (SPRAK-IX) TO MOD-TEMFSFEL                        
243600         MOVE ' '                  TO MOD-KDVALISO                        
243700     END-IF                                                               
243800     .                                                                    
243900     EJECT                                                                
244000 N-LAES            SECTION.                                               
244100                                                                          
244200     PERFORM IMS-GN-INDEX-RO                                              
244300*           ADD  40             TO DEBUGG-FLT                             
244400     IF SEGMENT-FINNS                                                     
244500         IF DIST79-DEALER-PRICE                                           
244700           MOVE SEQD-IDDISTR      TO W-IDDISTR-B201                       
244800                                     W-IDDISTR-B201-MIN                   
244900                                     W-IDDISTR-B201-MAX                   
245000           MOVE SEQD-IDKUNDNR     TO W-IDKUNDNR-B201                      
245100           MOVE SEQD-IDDC         TO W-IDDC-B6                            
245200           PERFORM S11-HAMTA-KDVALISO-TILL-MOD                            
245300         END-IF                                                           
245400         MOVE +1 TO INDX                                                  
245500                                                                          
245600         PERFORM UNTIL SEGMENT-SAKNAS OR                                  
245700            SEGMENT-SLUT OR                                               
245800            INDX > MAX-LINES                                              
245900            IF SEGMENT-FINNS                                              
246000               PERFORM S10-FLYTTA-TILL-NYCKLAR                            
246100               PERFORM IMS-GHU-RO                                         
246200               IF SEGMENT-FINNS                                           
246300                  PERFORM S07-LAES-ART                                    
246400               END-IF                                                     
246500               PERFORM S08-FLYTTA-TILL-MOD                                
246600               ADD +1 TO W-KVRAD                                          
246700            END-IF                                                        
246800            ADD +1 TO INDX                                                
246900            IF INDX = +2                                                  
247000               PERFORM B-FLYTTA-GAMLA-NYCKLAR                             
247100            END-IF                                                        
247200            PERFORM IMS-GN-INDEX-RO                                       
247300         END-PERFORM                                                      
247400                                                                          
247500         MOVE W-KVRAD        TO MOD-KVRAD-SPAR                            
247600         IF SEGMENT-FINNS                                                 
247700            PERFORM S09-FLYTTA-TILL-SPAR-MOD                              
247800         END-IF                                                           
247900                                                                          
248000         PERFORM UNTIL INDX > MAX-LINES                                   
248100            MOVE MFS-RENSA-FAELT TO                                       
248200                                    MOD-IDKUNDNR(INDX)                    
248300                                    MOD-IDARTNR(INDX)                     
248400                                    MOD-KVART(INDX)                       
248500                                    MOD-IDORDNR(INDX)                     
248600                                    MOD-KDORDKL(INDX)                     
248700                                    MOD-KDFRAKT(INDX)                     
248800                                    MOD-TITPO(INDX)                       
248900                                    MOD-PRARTNTO(INDX)                    
249000                                    MOD-TEASTRIX(INDX)                    
249100                                    MOD-KVQPACK-1(INDX)                   
249200                                    MOD-KDRAPRIO(INDX)                    
249300                                    MOD-IDDC(INDX)                        
249400                                    MOD-IDLOPNR(INDX)                     
249500            ADD +1 TO INDX                                                
249600         END-PERFORM                                                      
249700     ELSE                                                                 
249800         MOVE TEXT-0403 (SPRAK-IX) TO MOD-TEMFSFEL                        
249900         MOVE SPACE                TO MOD-KDVALISO                        
250000     END-IF                                                               
250100     .                                                                    
250200     EJECT                                                                
250300 O-KOLLA-TPO-FRYSTID SECTION.                                             
250400                                                                          
250500     MOVE    MID-IDDISTR-UT      TO        IDDISTR-WS                     
250600     MOVE    MID-IDKUNDNR (INDX) TO        IDKUNDNR-WS                    
250700     MOVE    MID-IDORDNR  (INDX) TO        IDORDNR-WS                     
250800     MOVE    MID-IDARTNR  (INDX) TO        IDARTNR-WS                     
250900     MOVE    MID-IDLOPNR  (INDX) TO        IDLOPNR-WS                     
251000                                                                          
251100     INSPECT IDDISTR-WS          REPLACING LEADING SPACE BY ZERO          
251200     INSPECT IDKUNDNR-WS         REPLACING LEADING SPACE BY ZERO          
251300     INSPECT IDARTNR-WS          REPLACING LEADING SPACE BY ZERO          
251400     INSPECT IDORDNR-WS          REPLACING LEADING SPACE BY ZERO          
251500     INSPECT IDLOPNR-WS          REPLACING LEADING SPACE BY ZERO          
251600                                                                          
251700     MOVE    IDDISTR-WS          TO        W-IDDISTR-N2                   
251800                                           TEST-IDDISTR                   
251900     MOVE    IDKUNDNR-WS         TO        W-IDKUNDNR-N2                  
252000     MOVE    IDORDNR-WS          TO        W-IDORDNR-N2                   
252100     MOVE    IDARTNR-WS          TO        W-IDARTNR-N2                   
252200     MOVE    IDLOPNR-WS          TO        W-IDLOPNR-N2                   
252300                                                                          
252400     PERFORM IMS-GU-RO                                                    
252500     IF SEGMENT-FINNS                                                     
252600         IF RAD-KDTPOTYP > ZERO AND RAD-FLTPOBEK = JA                     
252700                                AND RAD-KDSTARAD = 1                      
252800           MOVE RAD-TITPO    TO DAT-I-TIDATUM                             
252900           MOVE 'AAMMDD'     TO DAT-KDDATFORM                             
253000           CALL WDATKONV USING DAT-KDDATFORM                              
253100                               DAT-I-TIDATUM                              
253200                               DAT-O-TIDATUM                              
253300                               DAT-KDSVAR                                 
253400                                                                          
253500           IF DAT-KDSVAR-OK                                               
253600             MOVE DAT-TIAAVV-GRP TO W-TITPO-TIAAVV                        
253700           ELSE                                                           
253800             MOVE 'FEL FRÅN PROGRAM W4057200 I SECTION O' TO              
253900                                                     FELTEXT              
254000             CALL ABEND USING RKOD-ABEND                                  
254100           END-IF                                                         
254200                                                                          
254300           MOVE ZERO         TO DAT-I-TIDATUM                             
254400           MOVE 'IDAG  '     TO DAT-KDDATFORM                             
254500           CALL WDATKONV USING DAT-KDDATFORM                              
254600                               DAT-I-TIDATUM                              
254700                               DAT-O-TIDATUM                              
254800                               DAT-KDSVAR                                 
254900                                                                          
255000           IF DAT-KDSVAR-OK                                               
255100             MOVE DAT-TIAAVV-GRP TO W-DAGENS-DAT-TIAAVV                   
255200           ELSE                                                           
255300             MOVE 'FEL FRÅN PROGRAM W4057200 I SECTION O' TO              
255400                                                     FELTEXT              
255500             CALL ABEND USING RKOD-ABEND                                  
255600           END-IF                                                         
255700                                                                          
255800           PERFORM S07-LAES-ART                                           
255900           MOVE CLAG-KVFRYSTI TO VECKO-ANTAL                              
256000           MOVE W-DAGENS-DAT-TIAAVV TO VECKO-DATUM-AAVV                   
256100                                                                          
256200           CALL W009VADD USING VECKO-DATUM-AAVV VECKO-ANTAL               
256300                                                                          
256400           MOVE W-TITPO-TIAAVV     TO TMP1-YYWW                           
256500           MOVE VECKO-DATUM-AAVV   TO TMP2-YYWW                           
256600           PERFORM WY2000P3                                               
256700           IF TMP1-YYWW   < TMP2-YYWW                                     
256800             MOVE NEJ TO FRYSTID-SW                                       
256900             IF MID-VALKOD(INDX) = 'A'                                    
257000                CONTINUE                                                  
257100             ELSE                                                         
257200               IF SEC-KDSVAR NOT = ' '                                    
257300                 MOVE MAX-LINES TO INDX                                   
257400                 MOVE ERR-INOM-FRYS TO MED-IDMFSFEL                       
257500                 CALL WMEDKONV USING MED-WMEDAREA                         
257600                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
257700               END-IF                                                     
257800             END-IF                                                       
257900           ELSE                                                           
258000             MOVE JA TO FRYSTID-SW                                        
258100           END-IF                                                         
258200         ELSE                                                             
258300           MOVE JA TO FRYSTID-SW                                          
258400         END-IF                                                           
258500     END-IF                                                               
258600                                                                          
258700     .                                                                    
258800     EJECT                                                                
258900 P-KOLLA-OM-TPO4 SECTION.                                                 
259000                                                                          
259100     MOVE    MID-IDDISTR-UT      TO        IDDISTR-WS                     
259200     MOVE    MID-IDKUNDNR (INDX) TO        IDKUNDNR-WS                    
259300     MOVE    MID-IDORDNR  (INDX) TO        IDORDNR-WS                     
259400     MOVE    MID-IDARTNR  (INDX) TO        IDARTNR-WS                     
259500     MOVE    MID-IDLOPNR  (INDX) TO        IDLOPNR-WS                     
259600                                                                          
259700     INSPECT IDDISTR-WS          REPLACING LEADING SPACE BY ZERO          
259800     INSPECT IDKUNDNR-WS         REPLACING LEADING SPACE BY ZERO          
259900     INSPECT IDARTNR-WS          REPLACING LEADING SPACE BY ZERO          
260000     INSPECT IDORDNR-WS          REPLACING LEADING SPACE BY ZERO          
260100     INSPECT IDLOPNR-WS          REPLACING LEADING SPACE BY ZERO          
260200                                                                          
260300     MOVE    IDDISTR-WS          TO        W-IDDISTR-N2                   
260400                                           TEST-IDDISTR                   
260500     MOVE    IDKUNDNR-WS         TO        W-IDKUNDNR-N2                  
260600     MOVE    IDORDNR-WS          TO        W-IDORDNR-N2                   
260700     MOVE    IDARTNR-WS          TO        W-IDARTNR-N2                   
260800     MOVE    IDLOPNR-WS          TO        W-IDLOPNR-N2                   
260900                                                                          
261000     PERFORM IMS-GU-RO                                                    
261100     IF SEGMENT-FINNS                                                     
261200       IF RAD-KDTPOTYP = 4 AND RAD-KDSTARAD = 1                           
261300         MOVE JA TO TPO4-SW                                               
261400         MOVE MAX-LINES TO INDX                                           
261500         MOVE ERR-OTILL-UPP TO MED-IDMFSFEL                               
261600         CALL WMEDKONV USING MED-WMEDAREA                                 
261700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
261800       END-IF                                                             
261900     END-IF                                                               
262000                                                                          
262100     .                                                                    
262200     EJECT                                                                
262300 Q-LAES-PF8        SECTION.                                               
262400                                                                          
262500     PERFORM IMS-GHU-RO                                                   
262600                                                                          
262700     IF DIST79-DEALER-PRICE                                               
262900       IF RAD-KDVALISO              > SPACE                               
263000         MOVE RAD-KDVALISO          TO  MOD-KDVALISO                      
263100       ELSE                                                               
263200         MOVE RAD-IDDISTR      TO W-IDDISTR-B201                          
263300                                  W-IDDISTR-B201-MIN                      
263400                                  W-IDDISTR-B201-MAX                      
263500         MOVE RAD-IDKUNDNR     TO W-IDKUNDNR-B201                         
263600         MOVE RAD-IDDC         TO W-IDDC-B6                               
263700         PERFORM S11-HAMTA-KDVALISO-TILL-MOD                              
263800       END-IF                                                             
263900     END-IF                                                               
264000     IF DIST79-ECOM-PRICE                                                 
264100       IF RAD-KDVALISO              > SPACE                               
264200         MOVE RAD-KDVALISO          TO  MOD-KDVALISO                      
264300       ELSE                                                               
264400         MOVE SPACE                 TO  MOD-KDVALISO                      
264500       END-IF                                                             
264600     END-IF                                                               
264700     PERFORM S07-LAES-ART                                                 
264800     PERFORM S08-FLYTTA-TILL-MOD                                          
264900     MOVE +1 TO INDX                                                      
265000     PERFORM QA-ANGE-NYCKLAR                                              
265100     PERFORM IMS-GU-INDEX-RO                                              
265200     PERFORM S09-FLYTTA-TILL-SPAR-MOD                                     
265300     MOVE MFS-RENSA-FAELT   TO MOD-TEMFSINF                               
265400                                                                          
265500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
265600                                                  INDX > MAX-LINES        
265700        PERFORM S10-FLYTTA-TILL-NYCKLAR                                   
265800        PERFORM IMS-GHU-RO                                                
265900        PERFORM S07-LAES-ART                                              
266000        PERFORM S08-FLYTTA-TILL-MOD                                       
266100        ADD +1 TO W-KVRAD                                                 
266200        ADD +1 TO INDX                                                    
266300        IF INDX = +2                                                      
266400            PERFORM B-FLYTTA-GAMLA-NYCKLAR                                
266500        END-IF                                                            
266600        PERFORM IMS-GN-INDEX-RO                                           
266700     END-PERFORM                                                          
266800                                                                          
266900     MOVE W-KVRAD        TO MOD-KVRAD-SPAR                                
267000     IF SEGMENT-FINNS                                                     
267100        PERFORM S09-FLYTTA-TILL-SPAR-MOD                                  
267200     END-IF                                                               
267300                                                                          
267400     PERFORM UNTIL INDX > MAX-LINES                                       
267500        MOVE MFS-RENSA-FAELT TO                                           
267600                                MOD-IDKUNDNR(INDX)                        
267700                                MOD-IDARTNR(INDX)                         
267800                                MOD-KVART(INDX)                           
267900                                MOD-IDORDNR(INDX)                         
268000                                MOD-KDORDKL(INDX)                         
268100                                MOD-KDFRAKT(INDX)                         
268200                                MOD-TITPO(INDX)                           
268300                                MOD-PRARTNTO(INDX)                        
268400                                MOD-TEASTRIX(INDX)                        
268500                                MOD-KVQPACK-1(INDX)                       
268600                                MOD-KDRAPRIO(INDX)                        
268700                                MOD-IDDC(INDX)                            
268800                                MOD-IDLOPNR(INDX)                         
268900        ADD +1 TO INDX                                                    
269000     END-PERFORM                                                          
269100     .                                                                    
269200     EJECT                                                                
269300 QA-ANGE-NYCKLAR     SECTION.                                             
269400* HÄR FLYTTAS NYCKLAR TILL GU-LÄSNING AV WDA5D1                           
269500     MOVE LOW-VALUE  TO W-WDA5D1KY-MIN                                    
269600     MOVE HIGH-VALUE TO W-WDA5D1KY-MAX                                    
269700     MOVE '2'        TO W-KDSTARAD-MAX                                    
269800                                                                          
269900     MOVE RAD-IDDISTR            TO W-IDDISTR-N1-MIN                      
270000                                    W-IDDISTR-N1-MAX                      
270100                                                                          
270200     MOVE RAD-IDKUNDNR           TO W-IDKUNDNR-N1-MIN                     
270300                                    W-IDKUNDNR-N1-MAX                     
270400                                                                          
270500     MOVE RAD-IDARTNR            TO W-IDARTNR-N1-MIN                      
270600                                    W-IDARTNR-N1-MAX                      
270700                                                                          
270800     MOVE RAD-IDLOPNR            TO W-IDLOPNR-N1-MIN                      
270900                                    W-IDLOPNR-N1-MAX                      
271000                                                                          
271100     MOVE RAD-IDORDNR5        TO W-IDKUNDRF-1-5                           
271200     MOVE W-IDKUNDRF          TO W-IDKUNDRF-N1-MIN                        
271300                                 W-IDKUNDRF-N1-MAX                        
271400     .                                                                    
271500     EJECT                                                                
271600 S01-UPPDATERA-WDK9 SECTION.                                              
271700                                                                          
271800     MOVE RAD-IDARTNR TO W-IDARTNR-N                                      
271900     PERFORM IMS-GHU-ARTM-WDK901                                          
272000     SUBTRACT SPAR-KVART FROM ART-SUTPO-TOT                               
272100     PERFORM IMS-REPL-ARTM-WDK901                                         
272200     MOVE RAD-TITPO TO DAT-I-TIDATUM                                      
272300     MOVE 'AAMMDD'  TO DAT-KDDATFORM                                      
272400     CALL WDATKONV USING DAT-KDDATFORM                                    
272500                         DAT-I-TIDATUM                                    
272600                         DAT-O-TIDATUM                                    
272700                         DAT-KDSVAR                                       
272800     IF DAT-KDSVAR-OK                                                     
272900       MOVE DAT-TIAAVV-GRP TO W-DABEHOV-AAVV                              
273000       MOVE DAT-TISEKEL    TO W-DABEHOV-SEKEL                             
273100       MOVE W-DABEHOV      TO W-DABEHOV-N                                 
273200     ELSE                                                                 
273300       MOVE 'FEL FRÅN PROGRAM W4057200 I SECTION S01' TO FELTEXT          
273400       CALL ABEND USING RKOD-ABEND                                        
273500     END-IF                                                               
273600                                                                          
273700     PERFORM IMS-GHNP-ARTM-WDK911                                         
273800*  PROD ABEND 000321 - DABEHOV FANNS INTE PÅ K9                           
273900     IF SEGMENT-FINNS                                                     
274000       IF RAD-KDTPOTYP = 1 OR 2                                           
274100         SUBTRACT SPAR-KVART FROM ANT-SUTPO-PB                            
274200       ELSE                                                               
274300         SUBTRACT SPAR-KVART FROM ANT-SUTPO-EJPB                          
274400       END-IF                                                             
274500       IF ANT-SUTPO-PB = ZERO AND ANT-SUTPO-EJPB = ZERO                   
274600         PERFORM IMS-DLET-ARTM-WDK9                                       
274700       ELSE                                                               
274800         PERFORM IMS-REPL-ARTM-WDK911                                     
274900       END-IF                                                             
275000     END-IF                                                               
275100     .                                                                    
275200     EJECT                                                                
275300 S02-SKAPA-SUC-TRANS SECTION.                                             
275400                                                                          
275500     MOVE 'SUC'            TO SUC-IDPTYP                                  
275600     MOVE RAD-IDDISTR      TO SUC-IDDISTR                                 
275700     MOVE RAD-IDKUNDNR     TO SUC-IDKUNDNR                                
275800     MOVE RAD-IDDC         TO SUC-IDDC                                    
275900     MOVE RAD-IDORDNR5     TO SUC-IDORDNR                                 
276000     MOVE RAD-IDARTNR      TO SUC-IDARTNR                                 
276100     MOVE RAD-REKSIFFR     TO SUC-REKSIFFR                                
276200     MOVE RAD-KVART        TO SUC-KVBEART                                 
276300     MOVE RAD-KDSTARAD     TO SUC-KDRO                                    
276400     MOVE W-SPAR-KDORDKL   TO SUC-KDORDER                                 
276500     MOVE RAD-KDORDKL      TO SUC-KDORDER-NY                              
276600     MOVE RAD-KDVRINFO     TO SUC-KDVRINFO                                
276700                                                                          
276800     MOVE SUC-WDGZSUC      TO ZZAC-LOGGPOST                               
276900     MOVE SPACE            TO ZZAC-SORTPOST                               
277000                                                                          
277100     PERFORM S20-SKRIV-LOGG                                               
277200                                                                          
277300     .                                                                    
277400     EJECT                                                                
277500 S03-SKAPA-RY9-TRANS  SECTION.                                            
277600                                                                          
277700     MOVE 'RY9'            TO RY9-IDPTYP                                  
277800     MOVE RAD-BERADREF     TO RY9-BERADREF                                
277900     MOVE RAD-BEVOLREF     TO RY9-BEVOLREF                                
278000     MOVE RAD-FLERS        TO RY9-FLERS                                   
278100     MOVE RAD-IDDISTR      TO W-IDDISTR-B201                              
278200     MOVE RAD-IDKUNDNR     TO W-IDKUNDNR-B201                             
278300     PERFORM IMS-GU-WDB201                                                
278400     MOVE GMT-FLNC         TO RY9-FLNC                                    
278500     MOVE RAD-IDARTNR      TO RY9-IDARTNR                                 
278600     MOVE ZERO             TO RY9-IDDIVORD                                
278700     MOVE RAD-IDKUNDRF     TO RY9-IDKUNDRF                                
278800     MOVE RAD-IDLOPNR      TO RY9-IDLOPNR                                 
278900     MOVE MSG-LTERM-NAME   TO RY9-IDUSER                                  
279000     MOVE RAD-KDFAKTYP     TO RY9-KDFAKTYP                                
279100     MOVE RAD-KDKVBRYT     TO RY9-KDKVBRYT                                
279200     MOVE RAD-KDORDKL      TO RY9-KDORDKL                                 
279300     MOVE RAD-KDDSP        TO RY9-KDDSP                                   
279400     MOVE RAD-KDRAPRIO     TO RY9-KDRAPRIO                                
279500     MOVE RAD-KDSTARAD     TO RY9-KDSTARAD                                
279600     MOVE RAD-KDTPOTYP     TO RY9-KDTPOTYP                                
279700     MOVE SPACE            TO RY9-KDUART                                  
279800     MOVE RAD-KDVRINFO     TO RY9-KDVRINFO                                
279900     IF RAD-KDTPOTYP = 1 AND RAD-IDSYSTEM = 'VR'                          
280000       MOVE 1              TO RY9-KDVRTPO                                 
280100     END-IF                                                               
280200     IF RAD-KDTPOTYP = 1 AND RAD-IDSYSTEM NOT = 'VR'                      
280300       MOVE 2              TO RY9-KDVRTPO                                 
280400     END-IF                                                               
280500     IF RAD-KDTPOTYP NOT = 1                                              
280600       MOVE 0              TO RY9-KDVRTPO                                 
280700     END-IF                                                               
280800     MOVE RAD-PRARTNTO     TO RY9-PRARTNTO                                
280900     MOVE RAD-TIREGDAT     TO RY9-TIREGDAT                                
281000     MOVE RAD-TIRES        TO RY9-TIRES                                   
281100     MOVE RAD-TITPO        TO RY9-TITPO                                   
281200     MOVE RAD-DARODAT (3:6) TO RY9-TIRODAT                                
281300     MOVE GMT-FLVR         TO RY9-FLVR                                    
281400                                                                          
281500     MOVE RY9-WDGZRY9      TO ZZAC-LOGGPOST                               
281600                                                                          
281700     MOVE SPACE            TO RY9S-WDGZRY9S                               
281800     MOVE RAD-IDDISTR      TO RY9S-IDDISTR                                
281900     MOVE RAD-IDKUNDNR     TO RY9S-IDKUNDNR                               
282000     MOVE ORQI-OHUV-IDORDER TO RY9S-IDORDER                               
282100     MOVE RAD-IDDC         TO RY9S-IDDC                                   
282200     MOVE RAD-KDFRAKT      TO RY9S-KDFRAKT                                
282300     MOVE RAD-KDORDKL      TO RY9S-KDORDKL                                
282400     IF RAD-FLTPOBEK = NEJ                                                
282500        MOVE 83            TO RY9S-KDORDBEK                               
282600     ELSE                                                                 
282700        MOVE 85            TO RY9S-KDORDBEK                               
282800     END-IF                                                               
282900                                                                          
283000     MOVE RY9S-WDGZRY9S    TO ZZAC-SORTPOST                               
283100     .                                                                    
283200     EJECT                                                                
283300 S04-SKAPA-2109-TRANS SECTION.                                            
283400                                                                          
283500*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
283600     MOVE RAD-IDARTNR       TO BYT03-IDARTNR                              
283700     IF NOT BYT03-OBJEKT                                                  
283800                                                                          
283900        IF RAD-KDOI NOT = SPACE                                           
284000          MOVE SPACE       TO 2109-MID2-W2I10902                          
284100          MOVE 1           TO 2109-MID2-KVANTART                          
284200          MOVE RAD-IDARTNR TO 2109-MID2-IDARTNR (1)                       
284300          MOVE ORQI-OHUV-IDDC-PRIM                                        
284400                           TO 2109-MID2-IDDC(1)                           
284500          MOVE '-'         TO 2109-MID2-KDTECKEN (1)                      
284600          MOVE RAD-KDOI    TO 2109-MID2-KDOI (1)                          
284700          MOVE SPAR-KVART  TO 2109-MID2-KVOI (1)                          
284800          MOVE RAD-CLEARGROUP                                             
284900                           TO 2109-MID2-CLEARGROUP(1)                     
285000                                                                          
285100          IF RAD-KDTPOTYP >= 1 AND <= 5 AND RAD-KDSTARAD = 1              
285200            IF RAD-FLTPOBEK = NEJ                                         
285300              MOVE RAD-TIREGDAT TO 2109-MID2-TIUPPDAT (1)                 
285400            ELSE                                                          
285500              MOVE RAD-TITPO TO 2109-MID2-TIUPPDAT (1)                    
285600            END-IF                                                        
285700          ELSE                                                            
285800            IF RAD-KDTPOTYP >= 1 AND <= 5                                 
285900              MOVE 'DT'      TO 2109-MID2-KDOI (1)                        
286000              MOVE RAD-TITPO TO 2109-MID2-TIUPPDAT (1)                    
286100            ELSE                                                          
286200*             MOVE RAD-TIREGDAT TO 2109-MID2-TIUPPDAT (1)                 
286300              MOVE ORQI-OHUV-TIREGDAT TO 2109-MID2-TIUPPDAT (1)           
286400            END-IF                                                        
286500          END-IF                                                          
286600                                                                          
286700          COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17           
286800                                                                          
286900          PERFORM IMS-PURG-ALT-MSG-2109                                   
287000        END-IF                                                            
287100     END-IF                                                               
287200                                                                          
287300     .                                                                    
287400     EJECT                                                                
287500 S05-UPPDATERA-WDQ1 SECTION.                                              
287600                                                                          
287700     MOVE RAD-IDDISTR  TO W-IDDISTR-N9                                    
287800     MOVE RAD-IDKUNDNR TO W-IDKUNDNR-N9                                   
287900     MOVE '00'         TO W-IDKUNDRF-N9(1:2)                              
288000     MOVE RAD-IDKUNDRF TO W-IDKUNDRF-N9(3:5)                              
288100     PERFORM IMS-GU-ORQI01-CSEQ                                           
288200                                                                          
288300     MOVE SPACE                     TO DLI-IO-ISRT-Q101                   
288400     MOVE ORQI-OHUV-IDORDER         TO ORQM-OBKR-IDORDER                  
288500     MOVE RAD-IDARTNR               TO ORQM-OBKR-IDARTNR                  
288600     MOVE +1                        TO ORQM-OBKR-IDLOPNR                  
288700                                       ORQM-OBKR-IDSEKVNR                 
288800     MOVE RAD-IDDC                  TO ORQM-OBKR-IDDC                     
288900     MOVE RAD-IDDC-RO               TO ORQM-OBKR-IDDC-RO                  
289000     MOVE 85                        TO ORQM-OBKR-KDORDBEK                 
289100     MOVE SPACE                     TO ORQM-OBKR-BEERS                    
289200     MOVE SPACE                     TO ORQM-OBKR-IDBIL                    
289300     MOVE RAD-BEKUNDRF              TO ORQM-OBKR-BEKUNDRF                 
289400     MOVE RAD-BERADREF              TO ORQM-OBKR-BERADREF                 
289500     MOVE RAD-BEVOLREF              TO ORQM-OBKR-BEVOLREF                 
289600     MOVE RAD-IDKAMPRF              TO ORQM-OBKR-IDKAMPRF                 
289700     MOVE ZERO                      TO ORQM-OBKR-DIERS-KVOT               
289800     MOVE NEJ                       TO ORQM-OBKR-FLAKPLOC                 
289900     MOVE RAD-FLINVEST              TO ORQM-OBKR-FLINVEST                 
290000     MOVE JA                        TO ORQM-OBKR-FLOBOK                   
290100     MOVE NEJ                       TO ORQM-OBKR-FLOBTRAN                 
290200                                       ORQM-OBKR-FLOBPRT                  
290300     MOVE RAD-FLPRTILL              TO ORQM-OBKR-FLPRTILL                 
290400     MOVE JA                        TO ORQM-OBKR-FLRESTN                  
290500     MOVE NEJ                       TO ORQM-OBKR-FLSLATT                  
290600     MOVE RAD-FLERS                 TO ORQM-OBKR-FLTILLK                  
290700     MOVE ZERO                      TO ORQM-OBKR-IDARTNR-TILLK            
290800     MOVE RAD-IDDISTR               TO ORQM-OBKR-IDDISTR                  
290900     MOVE RAD-IDKUNDNR              TO ORQM-OBKR-IDKUNDNR                 
291000     MOVE '00'                      TO ORQM-OBKR-IDKUNDRF(1:2)            
291100     MOVE RAD-IDORDNR5              TO ORQM-OBKR-IDKUNDRF(3:5)            
291200     MOVE '0000000   '              TO ORQM-OBKR-IDKUNDRF-RO              
291300     MOVE RAD-IDLEVNR               TO ORQM-OBKR-IDLEVNR                  
291400     MOVE RAD-IDLOPNR               TO ORQM-OBKR-IDLOPNR-RO               
291500     MOVE IDPGM                     TO ORQM-OBKR-IDPGM                    
291600     MOVE RAD-IDSYSTEM              TO ORQM-OBKR-IDSYSTEM                 
291700     MOVE RAD-KDDSP                 TO ORQM-OBKR-KDDSP                    
291800     MOVE ZERO                      TO ORQM-OBKR-KDERS                    
291900     MOVE RAD-KDKVBRYT              TO ORQM-OBKR-KDKVBRYT                 
292000     MOVE RAD-KDPRTYP               TO ORQM-OBKR-KDPRTYP                  
292100     MOVE RAD-KDTPOTYP              TO ORQM-OBKR-KDTPOTYP                 
292200     MOVE RAD-KDVRINFO              TO ORQM-OBKR-KDVRINFO                 
292300     MOVE SPAR-KVART                TO ORQM-OBKR-KVANNANT                 
292400     MOVE ZERO                      TO ORQM-OBKR-KVAVBART                 
292500     MOVE RAD-KVART                 TO ORQM-OBKR-KVBEART                  
292600                                       ORQM-OBKR-KVBEART-Q                
292700     MOVE ZERO                      TO ORQM-OBKR-KVBEART-TILLK            
292800                                       ORQM-OBKR-KVPREAVB                 
292900                                       ORQM-OBKR-KVPRERO                  
293000                                       ORQM-OBKR-KVQPACK                  
293100                                       ORQM-OBKR-KVRO                     
293200                                       ORQM-OBKR-KVSLATT                  
293300     MOVE RAD-PRARTNTO              TO ORQM-OBKR-PRARTNTO                 
293400     MOVE RAD-DEAL-PR-LINE          TO ORQM-OBKR-DEAL-PR-LINE             
293500     MOVE ZERO                      TO ORQM-OBKR-PRBPRIS                  
293600     MOVE RAD-REKSIFFR              TO ORQM-OBKR-REKSIFFR                 
293700     MOVE ZERO                      TO ORQM-OBKR-REKSIFFR-TILLK           
293800                                       ORQM-OBKR-RERF-RAD                 
293900                                       ORQM-OBKR-TIDISPIN                 
294000     MOVE ORQI-OHUV-TIREGDAT        TO ORQM-OBKR-TIORDREG                 
294100     MOVE ZERO                      TO ORQM-OBKR-TIPRIS                   
294200                                       ORQM-OBKR-TIRODAT                  
294300     ACCEPT ORQM-OBKR-TIREGDAT FROM DATE                                  
294400     ACCEPT ORQM-OBKR-TIREGTID FROM TIME                                  
294500                                                                          
294600     MOVE MSGI-TILOKDAT TO ORQM-OBKR-TIREGDAT                             
294700     MOVE MSGI-TILOKTID TO ORQM-OBKR-TIREGTID                             
294800                                                                          
294900     MOVE FUNCTION CURRENT-DATE (1:8) TO DATUM-MED-ARHUNDR                
295000     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
295100                                     ORQM-OBKR-TITIREGD-9KOMPL            
295200     MOVE RAD-TITPO                 TO ORQM-OBKR-TITPO                    
295300     IF  ORQI-OHUV-TIREGDAT > +500000                                     
295400         ADD +19000000 TO ORQI-OHUV-TIREGDAT GIVING                       
295500                                     DATUM-MED-ARHUNDR                    
295600     ELSE                                                                 
295700         ADD +20000000 TO ORQI-OHUV-TIREGDAT GIVING                       
295800                                     DATUM-MED-ARHUNDR                    
295900     END-IF                                                               
296000     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
296100                                     ORQM-OBKR-TITIORDD-9KOMPL            
296200     MOVE RAD-KDFRAKT               TO ORQM-OBKR-KDFRAKT                  
296300     MOVE ORQI-OHUV-KDORDKL         TO ORQM-OBKR-KDORDKL                  
296400                                                                          
296500     MOVE RAD-KDORDTYP-LDC          TO ORQM-OBKR-KDORDTYP-LDC             
296600     MOVE RAD-TIREPDAT              TO ORQM-OBKR-TIREPDAT                 
296700     MOVE RAD-IDKUNDRF-WIP          TO ORQM-OBKR-IDKUNDRF-WIP             
296800     MOVE ZERO                      TO ORQM-OBKR-TIDLEVDAT                
296900     MOVE RAD-PRAVCOST              TO ORQM-OBKR-PRAVCOST                 
297000     MOVE RAD-KDVALISO              TO ORQM-OBKR-KDVALISO                 
297100                                                                          
297200                                                                          
297300     MOVE ORQI-OHUV-IDORDER      TO   W-IDORDER-MIN-N10                   
297400                                      W-IDORDER-MAX-N10                   
297500     MOVE RAD-IDARTNR            TO   W-IDARTNR-MIN-N10                   
297600                                      W-IDARTNR-MAX-N10                   
297700     MOVE +1                     TO   W-IDLOPNR-MIN-N10                   
297800                                      W-IDLOPNR-MAX-N10                   
297900     MOVE +1                     TO   W-IDSEKVNR-MIN-N10                  
298000                                      W-IDSEKVNR-MAX-N10                  
298100     MOVE RAD-IDDC               TO   W-IDDC-MIN-N10                      
298200                                      W-IDDC-MAX-N10                      
298300     PERFORM IMS-GU-ORQM-WDQ1                                             
298400     IF SEGMENT-FINNS                                                     
298500       PERFORM UNTIL SEGMENT-SAKNAS                                       
298600         ADD +1 TO ORQM-OBKR-IDLOPNR                                      
298700                   W-IDLOPNR-MIN-N10                                      
298800                   W-IDLOPNR-MAX-N10                                      
298900         PERFORM IMS-GU-ORQM-WDQ1                                         
299000       END-PERFORM                                                        
299100       PERFORM IMS-ISRT-ORQM-WDQ1                                         
299200     ELSE                                                                 
299300       PERFORM IMS-ISRT-ORQM-WDQ1                                         
299400     END-IF                                                               
299500     .                                                                    
299600     EJECT                                                                
299700 S06-UPPDATERA-ANTAL-MARKNAD SECTION.                                     
299800                                                                          
299900     MOVE RAD-IDKAMPRF      TO W-KAMP-IDKAMPRF                            
300000     MOVE RAD-IDDC          TO W-KAMP-IDDC                                
300100     MOVE RAD-IDARTNR       TO W-KART-IDARTNR                             
300200                                                                          
300300     PERFORM IMS-GHU-WDM211                                               
300400     IF SEGMENT-FINNS                                                     
300500       SUBTRACT SPAR-KVART FROM KART-KVBEART-KUND                         
300600       SUBTRACT SPAR-KVART FROM KART-KVBEART-TPO4                         
300700       PERFORM IMS-REPL-WDM211                                            
300800                                                                          
300900       MOVE RAD-IDKAMPRF    TO W-KAMP-IDKAMPRF                            
301000       MOVE RAD-IDDC        TO W-KAMP-IDDC                                
301100       MOVE RAD-IDARTNR     TO W-KART-IDARTNR                             
301200       MOVE RAD-IDDISTR     TO W-KMRK-IDDISTR-FOM                         
301300       MOVE RAD-IDDISTR     TO W-KMRK-IDDISTR-TOM                         
301400       MOVE RAD-IDKUNDNR    TO W-KMRK-IDKUNDNR-FOM                        
301500       MOVE RAD-IDKUNDNR    TO W-KMRK-IDKUNDNR-TOM                        
301600                                                                          
301700       PERFORM S20-FINN-INTERVALL                                         
301800                                                                          
301900       PERFORM IMS-GHU-WDM221                                             
302000       IF SEGMENT-FINNS                                                   
302100         SUBTRACT SPAR-KVART FROM KMRK-KVBEART-KUND                       
302200         PERFORM IMS-REPL-WDM221                                          
302300       ELSE                                                               
302400         MOVE ZERO TO W-KMRK-IDKUNDNR-FOM                                 
302500         MOVE ZERO TO W-KMRK-IDKUNDNR-TOM                                 
302600         PERFORM IMS-GHU-WDM221                                           
302700         IF SEGMENT-FINNS                                                 
302800           SUBTRACT SPAR-KVART FROM KMRK-KVBEART-KUND                     
302900           PERFORM IMS-REPL-WDM221                                        
303000         END-IF                                                           
303100       END-IF                                                             
303200     END-IF                                                               
303300                                                                          
303400     .                                                                    
303500     EJECT                                                                
303600 S07-LAES-ART SECTION.                                                    
303700                                                                          
303800     MOVE    RAD-IDARTNR  TO W-IDARTNR-ARTC01                             
303900     PERFORM IMS-GU-ARTC11                                                
304000     .                                                                    
304100     EJECT                                                                
304200 S08-FLYTTA-TILL-MOD SECTION.                                             
304300                                                                          
304400*           ADD  100            TO DEBUGG-FLT                             
304500     MOVE RAD-IDKUNDNR     TO MOD-IDKUNDNR  (INDX)                        
304600     MOVE RAD-IDARTNR      TO MOD-IDARTNR   (INDX)                        
304700*    IF  INDX = 1                                                         
304800*      MOVE RAD-IDKUNDNR   TO DEBUGG-IDKUNDNR                             
304900*      MOVE RAD-IDARTNR    TO DEBUGG-IDARTNR                              
305000*    END-IF                                                               
305100     MOVE RAD-KVART        TO MOD-KVART     (INDX)                        
305200     MOVE RAD-KDORDKL      TO MOD-KDORDKL   (INDX)                        
305300     MOVE RAD-KDFRAKT      TO MOD-KDFRAKT   (INDX)                        
305400     MOVE RAD-TITPO        TO MOD-TITPO     (INDX)                        
305500     IF MOD-TITPO (INDX) = 0                                              
305600       INSPECT MOD-TITPO (INDX) REPLACING LEADING ZERO BY SPACE           
305700     END-IF                                                               
305800     MOVE CLAG-KVQPACK-1   TO MOD-KVQPACK-1 (INDX)                        
305900     MOVE RAD-KDRAPRIO     TO MOD-KDRAPRIO  (INDX)                        
306000     MOVE RAD-IDDC         TO MOD-IDDC      (INDX)                        
306100     MOVE RAD-IDLOPNR      TO MOD-IDLOPNR   (INDX)                        
306200                                                                          
306300     IF SEC-KDSVAR = '2' OR '6'                                           
306400       CONTINUE                                                           
306500     ELSE                                                                 
306600       IF DIST79-DEALER-PRICE OR                                          
306800          DIST79-ECOM-PRICE                                               
306900         IF RAD-PRARTNTO-LOC > +0                                         
307000           MOVE RAD-PRARTNTO-LOC    TO MOD-PRARTNTO  (INDX)               
307100           MOVE ' '                 TO MOD-TEASTRIX  (INDX)               
307200         ELSE                                                             
307300           MOVE RAD-PRARTNTO-LOCPREL TO MOD-PRARTNTO (INDX)               
307400           MOVE '*'                  TO MOD-TEASTRIX (INDX)               
307500         END-IF                                                           
307600       ELSE                                                               
307700         IF RAD-PRAVCOST > 0                                              
307800            MOVE RAD-PRAVCOST TO MOD-PRARTNTO (INDX)                      
307900            MOVE RAD-KDVALISO TO MOD-KDVALISO                             
308000         ELSE                                                             
308100            MOVE RAD-PRARTNTO TO MOD-PRARTNTO (INDX)                      
308200            MOVE RAD-KDVALISO TO MOD-KDVALISO                             
308300         END-IF                                                           
308400         MOVE ' '          TO MOD-TEASTRIX  (INDX)                        
308500       END-IF                                                             
308600     END-IF                                                               
308700                                                                          
308800     MOVE RAD-IDKUNDRF     TO W-IDKUNDRF                                  
308900     MOVE W-IDKUNDRF-1-5   TO MOD-IDORDNR   (INDX)                        
309000                                                                          
309100     IF INDX = 1                                                          
309200       MOVE RAD-IDDISTR    TO MOD-IDDISTR-SPAR-E                          
309300       MOVE RAD-IDKUNDNR   TO MOD-IDKUNDNR-SPAR-E                         
309400       MOVE RAD-IDARTNR    TO MOD-IDARTNR-SPAR-E                          
309500       MOVE RAD-IDKUNDRF   TO W-IDKUNDRF                                  
309600       MOVE W-IDKUNDRF-1-5 TO MOD-IDORDNR-SPAR-E                          
309700       MOVE RAD-KDORDKL  TO MOD-KDORDKL-SPAR-E                            
309800       MOVE RAD-KDPRODSL TO MOD-KDPRODSL-SPAR-E                           
309900       MOVE RAD-KDTPOTYP TO MOD-KDTPOTYP-SPAR-E                           
310000       MOVE RAD-IDDC     TO MOD-IDDC-SPAR-E                               
310100     END-IF                                                               
310200     .                                                                    
310300     EJECT                                                                
310400 S09-FLYTTA-TILL-SPAR-MOD SECTION.                                        
310500                                                                          
310600*           ADD  300            TO DEBUGG-FLT                             
310700     MOVE SEQD-IDDISTR         TO MOD-IDDISTR-SPAR                        
310800     MOVE SEQD-IDKUNDNR        TO MOD-IDKUNDNR-SPAR                       
310900     MOVE SEQD-IDARTNR         TO MOD-IDARTNR-SPAR                        
311000     MOVE SEQD-IDKUNDRF        TO W-IDKUNDRF                              
311100     MOVE W-IDKUNDRF-1-5       TO MOD-IDORDNR-SPAR                        
311200     MOVE SEQD-IDLOPNR         TO MOD-IDLOPNR-SPAR                        
311300     IF SEQD-KDORDKL  > ZERO                                              
311400       MOVE SEQD-KDORDKL       TO MOD-KDORDKL-SPAR                        
311500     ELSE                                                                 
311600       MOVE ZERO               TO MOD-KDORDKL-SPAR                        
311700     END-IF                                                               
311800     IF SEQD-KDPRODSL > ZERO                                              
311900       MOVE SEQD-KDPRODSL      TO MOD-KDPRODSL-SPAR                       
312000     ELSE                                                                 
312100       MOVE ZERO               TO MOD-KDPRODSL-SPAR                       
312200     END-IF                                                               
312300     IF SEQD-KDTPOTYP > ZERO                                              
312400       MOVE SEQD-KDTPOTYP      TO MOD-KDTPOTYP-SPAR                       
312500     ELSE                                                                 
312600       MOVE ZERO               TO MOD-KDTPOTYP-SPAR                       
312700     END-IF                                                               
312800     IF SEQD-IDDC > ZERO                                                  
312900       MOVE SEQD-IDDC          TO MOD-IDDC-SPAR                           
313000     ELSE                                                                 
313100       MOVE ZERO               TO MOD-IDDC-SPAR                           
313200     END-IF                                                               
313300     MOVE TEXT-0402 (SPRAK-IX) TO MOD-TEMFSINF                            
313400     .                                                                    
313500     EJECT                                                                
313600 S10-FLYTTA-TILL-NYCKLAR SECTION.                                         
313700                                                                          
313800     MOVE SEQD-IDDISTR    TO  W-IDDISTR-N2                                
313900     MOVE SEQD-IDKUNDNR   TO  W-IDKUNDNR-N2                               
314000     MOVE SEQD-IDKUNDRF   TO  W-IDKUNDRF-N2                               
314100     MOVE SEQD-IDARTNR    TO  W-IDARTNR-N2                                
314200     MOVE SEQD-IDLOPNR    TO  W-IDLOPNR-N2                                
314300     .                                                                    
314400     EJECT                                                                
314500 S11-HAMTA-KDVALISO-TILL-MOD SECTION.                                     
314600                                                                          
314700     PERFORM IMS-GET-WDB201-UNIK                                          
314800     IF SEGMENT-FINNS                                                     
314900       CONTINUE                                                           
315000     ELSE                                                                 
315100       PERFORM IMS-GET-WDB201                                             
315200     END-IF                                                               
315300                                                                          
315400     MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                           
315500     MOVE GMT-IDFTG          TO W-WDB1-IDFTG                              
315600     PERFORM IMS-GU-WDB101                                                
315700     MOVE BET-KDVALISO     TO MOD-KDVALISO                                
315800     .                                                                    
315900     EJECT                                                                
316000 S20-FINN-INTERVALL SECTION.                                              
316100                                                                          
316200     PERFORM IMS-GU-WDM211                                                
316300     IF SEGMENT-FINNS                                                     
316400       PERFORM IMS-GNP-WDM221                                             
316500       PERFORM UNTIL SEGMENT-SAKNAS                                       
316600         IF  RAD-IDDISTR > KMRK-IDDISTR-TOM                               
316700         OR  RAD-IDDISTR < KMRK-IDDISTR-FOM                               
316800           CONTINUE                                                       
316900         ELSE                                                             
317000           IF  RAD-IDDISTR  = KMRK-IDDISTR-TOM                            
317100           AND RAD-IDKUNDNR > KMRK-IDKUNDNR-TOM                           
317200             CONTINUE                                                     
317300           ELSE                                                           
317400             IF  RAD-IDDISTR  = KMRK-IDDISTR-FOM                          
317500             AND RAD-IDKUNDNR < KMRK-IDKUNDNR-FOM                         
317600               CONTINUE                                                   
317700             ELSE                                                         
317800               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
317900               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
318000               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
318100               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
318200             END-IF                                                       
318300           END-IF                                                         
318400         END-IF                                                           
318500         PERFORM IMS-GNP-WDM221                                           
318600       END-PERFORM                                                        
318700     END-IF                                                               
318800     .                                                                    
318900     EJECT                                                                
319000                                                                          
319100 S20-SKRIV-LOGG SECTION.                                                  
319200                                                                          
319300     ACCEPT ZZAC-TIAAMMDD  FROM DATE                                      
319400     ACCEPT ZZAC-TIKLOCK   FROM TIME                                      
319500     MOVE  +1            TO ZZAC-IDLOGLOP                                 
319600                                                                          
319700     PERFORM IMS-ISRT-LOGG                                                
319800     IF SEGMENT-FINNS-REDAN                                               
319900       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
320000         ADD +1 TO ZZAC-IDLOGLOP                                          
320100         IF ZZAC-IDLOGLOP = 0                                             
320200           ADD 1 TO ZZAC-TIKLOCK                                          
320300         END-IF                                                           
320400         PERFORM IMS-ISRT-LOGG                                            
320500       END-PERFORM                                                        
320600     END-IF                                                               
320700     .                                                                    
320800     EJECT                                                                
320900 S30-SECURIT     SECTION.                                                 
321000                                                                          
321100     MOVE MSG-SIGNON-USERID TO SEC-IDUSER                                 
321200*    MOVE 'R0C3004 '        TO SEC-IDUSER                                 
321300     MOVE '4572'            TO SEC-IDTRANS                                
321400     MOVE IDDISTR-WS        TO SEC-IDKEY                                  
321500                                                                          
321600     CALL WSECURIT USING       SEC-IDUSER                                 
321700                               SEC-IDTRANS                                
321800                               SEC-IDKEY                                  
321900                               SEC-KDSVAR                                 
322000     .                                                                    
322100     EJECT                                                                
322200* IMS SEKTIONER                                                           
322300 IMS-GU-MSG SECTION.                                                      
322400                                                                          
322500     MOVE '  QC' TO GODK-STATUSKODER                                      
322600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
322700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
322800     PERFORM IMS-STATUSKONTROLL                                           
322900     .                                                                    
323000                                                                          
323100 IMS-ISRT-MSG SECTION.                                                    
323200                                                                          
323300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
323400       MOVE '0' TO MFS-KDHUVOMR                                           
323500     END-IF                                                               
323600                                                                          
323700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
323800     MOVE SPACE TO GODK-STATUSKODER                                       
323900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
324000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
324100     PERFORM IMS-STATUSKONTROLL                                           
324200     .                                                                    
324300     SKIP2                                                                
324400 IMS-PURG-ALT-MSG-2109 SECTION.                                           
324500     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
324600     MOVE SPACE TO GODK-STATUSKODER                                       
324700     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
324800     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
324900     PERFORM IMS-STATUSKONTROLL                                           
325000     .                                                                    
325100     EJECT                                                                
325200 IMS-GN-INDEX-RO SECTION.                                                 
325300                                                                          
325400     STRING 'WLORDT01(WDA5D1KY>=' W-WDA5D1KY-MIN                          
325500                    '&WDA5D1KY<=' W-WDA5D1KY-MAX                          
325600                    '&IDARTNR >=' W-IDARTNR-MIN-X                         
325700                    '&IDARTNR <=' W-IDARTNR-MAX-X                         
325800                    '&IDKUNDRF>=' W-IDKUNDRF-MIN                          
325900                    '&IDKUNDRF<=' W-IDKUNDRF-MAX                          
326000                    '&IDDC    >=' W-IDDC-MIN                              
326100                    '&IDDC    <=' W-IDDC-MAX                              
326200                    '&KDORDKL >=' W-KDORDKL-MIN-X                         
326300                    '&KDORDKL <=' W-KDORDKL-MAX-X                         
326400                    '&KDPRODSL>=' W-KDPRODSL-MIN-X                        
326500                    '&KDPRODSL<=' W-KDPRODSL-MAX-X                        
326600                    '&KDSTARAD<=' W-KDSTARAD-MAX                          
326700                    '&KDTPOTYP>=' W-KDTPOTYP-MIN-X                        
326800                    '&KDTPOTYP<=' W-KDTPOTYP-MAX-X ')'                    
326900            DELIMITED BY SIZE INTO SSA1                                   
327000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
327100     CALL CBLTDLI USING GN ORDT-PCB DLI-IO-WDA5D1 SSA1                    
327200     MOVE ORDT-STATUS-CODE TO STATUS-WS                                   
327300     PERFORM IMS-STATUSKONTROLL                                           
327400     .                                                                    
327500     SKIP2                                                                
327600 IMS-GU-INDEX-RO SECTION.                                                 
327700                                                                          
327800     STRING 'WLORDT01(WDA5D1KY>=' W-WDA5D1KY-MIN                          
327900                    '&WDA5D1KY<=' W-WDA5D1KY-MAX ')'                      
328000            DELIMITED BY SIZE INTO SSA1                                   
328100     MOVE '  ' TO GODK-STATUSKODER                                        
328200     CALL CBLTDLI USING GN ORDT-PCB DLI-IO-WDA5D1 SSA1                    
328300     MOVE ORDT-STATUS-CODE TO STATUS-WS                                   
328400     PERFORM IMS-STATUSKONTROLL                                           
328500     .                                                                    
328600                                                                          
328700 IMS-GHU-RO SECTION.                                                      
328800                                                                          
328900     STRING 'WLORDP01(WDA501KY =' W-WDA501KY ')'                          
329000            DELIMITED BY SIZE INTO SSA1                                   
329100     MOVE '  GE' TO GODK-STATUSKODER                                      
329200     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-WDA501 SSA1                   
329300     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
329400     PERFORM IMS-STATUSKONTROLL                                           
329500     .                                                                    
329600     SKIP2                                                                
329700                                                                          
329800 IMS-GU-RO    SECTION.                                                    
329900                                                                          
330000     STRING 'WLORDP01(WDA501KY =' W-WDA501KY ')'                          
330100            DELIMITED BY SIZE INTO SSA1                                   
330200     MOVE '  GE' TO GODK-STATUSKODER                                      
330300     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-WDA501 SSA1                    
330400     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
330500     PERFORM IMS-STATUSKONTROLL                                           
330600     .                                                                    
330700     SKIP2                                                                
330800                                                                          
330900 IMS-DLET-RO   SECTION.                                                   
331000                                                                          
331100     MOVE '  ' TO GODK-STATUSKODER                                        
331200     CALL CBLTDLI USING DLET ORDP-PCB DLI-IO-WDA501                       
331300     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
331400     PERFORM IMS-STATUSKONTROLL                                           
331500     .                                                                    
331600     SKIP2                                                                
331700                                                                          
331800 IMS-REPL-RO SECTION.                                                     
331900                                                                          
332000     MOVE '  ' TO GODK-STATUSKODER                                        
332100     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-WDA501                       
332200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
332300     PERFORM IMS-STATUSKONTROLL                                           
332400     .                                                                    
332500     SKIP2                                                                
332600                                                                          
332700 IMS-ISRT-RO   SECTION.                                                   
332800                                                                          
332900     MOVE 'WLORDP01' TO SSA1                                              
333000     MOVE '  II' TO GODK-STATUSKODER                                      
333100     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-WDA501 SSA1                  
333200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
333300     PERFORM IMS-STATUSKONTROLL                                           
333400     .                                                                    
333500     EJECT                                                                
333600                                                                          
333700                                                                          
333800 IMS-GU-ARTC01 SECTION.                                                   
333900                                                                          
334000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-ARTC01-X ')'                  
334100            DELIMITED BY SIZE INTO SSA1                                   
334200     MOVE '  ' TO GODK-STATUSKODER                                        
334300     CALL CBLTDLI USING GU ART-PCB DLI-IO-WDK601 SSA1                     
334400     MOVE ART-STATUS-CODE TO STATUS-WS                                    
334500     PERFORM IMS-STATUSKONTROLL                                           
334600     .                                                                    
334700     SKIP2                                                                
334800                                                                          
334900 IMS-GU-ARTC11 SECTION.                                                   
335000                                                                          
335100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-ARTC01-X ')'                  
335200            DELIMITED BY SIZE INTO SSA1                                   
335300     MOVE   'WLARTC11'          TO SSA2                                   
335400     MOVE '  '                  TO GODK-STATUSKODER                       
335500     CALL CBLTDLI USING GU ART-PCB DLI-IO-WDK611 SSA1 SSA2                
335600     MOVE ART-STATUS-CODE       TO STATUS-WS                              
335700     PERFORM IMS-STATUSKONTROLL                                           
335800     .                                                                    
335900     SKIP2                                                                
336000                                                                          
336100 IMS-GNP-ARTC11 SECTION.                                                  
336200                                                                          
336300     MOVE 'WLARTC11'      TO SSA1                                         
336400     MOVE '  '            TO GODK-STATUSKODER                             
336500     CALL CBLTDLI USING GNP ART-PCB DLI-IO-WDK611 SSA1                    
336600     MOVE ART-STATUS-CODE TO STATUS-WS                                    
336700     PERFORM IMS-STATUSKONTROLL                                           
336800     .                                                                    
336900     SKIP2                                                                
337000                                                                          
337100 IMS-GHU-ARTC11   SECTION.                                                
337200                                                                          
337300     STRING  'WLARTC01(IDARTNR  =' W-IDARTNR-ARTC01-X ')'                 
337400              DELIMITED BY SIZE INTO SSA1                                 
337500     MOVE    'WLARTC11'           TO SSA2                                 
337600     MOVE    '  '                 TO GODK-STATUSKODER                     
337700     CALL     CBLTDLI USING GHU ART-PCB DLI-IO-WDK611 SSA1 SSA2           
337800     MOVE     ART-STATUS-CODE     TO STATUS-WS                            
337900     PERFORM  IMS-STATUSKONTROLL                                          
338000     .                                                                    
338100     SKIP2                                                                
338200 IMS-GHU-WDK711 SECTION.                                                  
338300                                                                          
338400     STRING 'WLARTS01(IDARTNR  =' W-WDK711-IDARTNR-X ')'                  
338500          DELIMITED BY SIZE INTO SSA1                                     
338600     STRING 'WLARTS11(IDDC     =' W-WDK711-IDDC-X ')'                     
338700          DELIMITED BY SIZE INTO SSA2                                     
338800     MOVE '  GE' TO GODK-STATUSKODER                                      
338900     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-WDK711 SSA1 SSA2              
339000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
339100     PERFORM IMS-STATUSKONTROLL                                           
339200     .                                                                    
339300     EJECT                                                                
339400                                                                          
339500 IMS-REPL-WDK711 SECTION.                                                 
339600                                                                          
339700     MOVE '  ' TO GODK-STATUSKODER                                        
339800     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-WDK711                       
339900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
340000     PERFORM IMS-STATUSKONTROLL                                           
340100     .                                                                    
340200     EJECT                                                                
340300                                                                          
340400 IMS-REPL-ARTK611 SECTION.                                                
340500                                                                          
340600     MOVE '  ' TO GODK-STATUSKODER                                        
340700     CALL CBLTDLI USING REPL ART-PCB DLI-IO-WDK611                        
340800     MOVE ART-STATUS-CODE TO STATUS-WS                                    
340900     PERFORM IMS-STATUSKONTROLL                                           
341000     .                                                                    
341100                                                                          
341200     EJECT                                                                
341300 IMS-GHU-ARTM-WDK901 SECTION.                                             
341400                                                                          
341500     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
341600          DELIMITED BY SIZE INTO SSA1                                     
341700     MOVE '  ' TO GODK-STATUSKODER                                        
341800     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-WDK901 SSA1                   
341900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
342000     PERFORM IMS-STATUSKONTROLL                                           
342100     .                                                                    
342200     SKIP3                                                                
342300 IMS-GHNP-ARTM-WDK911 SECTION.                                            
342400                                                                          
342500     STRING 'WLARTM11(DABEHOV  =' W-DABEHOV-X ')'                         
342600          DELIMITED BY SIZE INTO SSA1                                     
342700     MOVE '  GE' TO GODK-STATUSKODER                                      
342800     CALL CBLTDLI USING GHNP ARTM-PCB DLI-IO-WDK911 SSA1                  
342900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
343000     PERFORM IMS-STATUSKONTROLL                                           
343100     .                                                                    
343200     SKIP2                                                                
343300 IMS-REPL-ARTM-WDK901 SECTION.                                            
343400                                                                          
343500     MOVE '  ' TO GODK-STATUSKODER                                        
343600     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-WDK901                       
343700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
343800     PERFORM IMS-STATUSKONTROLL                                           
343900     .                                                                    
344000     SKIP2                                                                
344100 IMS-REPL-ARTM-WDK911 SECTION.                                            
344200                                                                          
344300     MOVE '  ' TO GODK-STATUSKODER                                        
344400     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-WDK911                       
344500     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
344600     PERFORM IMS-STATUSKONTROLL                                           
344700     .                                                                    
344800     SKIP2                                                                
344900 IMS-DLET-ARTM-WDK9 SECTION.                                              
345000                                                                          
345100     MOVE '  ' TO GODK-STATUSKODER                                        
345200     CALL CBLTDLI USING DLET ARTM-PCB DLI-IO-WDK911                       
345300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
345400     PERFORM IMS-STATUSKONTROLL                                           
345500     .                                                                    
345600     EJECT                                                                
345700 IMS-GHU-XXBU-WDR5 SECTION.                                               
345800                                                                          
345900     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
346000          DELIMITED BY SIZE INTO SSA1                                     
346100     STRING 'WLXXBU11(WDGXKEY  =' W-WDGX2224-X ')'                        
346200          DELIMITED BY SIZE INTO SSA2                                     
346300     MOVE '  GE' TO GODK-STATUSKODER                                      
346400     CALL CBLTDLI USING GHU XXBU-PCB DLI-IO-2224 SSA1 SSA2                
346500     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
346600     PERFORM IMS-STATUSKONTROLL                                           
346700     .                                                                    
346800     SKIP2                                                                
346900 IMS-DLET-XXBU-WDR5 SECTION.                                              
347000                                                                          
347100     MOVE '  ' TO GODK-STATUSKODER                                        
347200     CALL CBLTDLI USING DLET XXBU-PCB DLI-IO-2224                         
347300     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
347400     PERFORM IMS-STATUSKONTROLL                                           
347500     .                                                                    
347600     EJECT                                                                
347700 IMS-ISRT-LOGG SECTION.                                                   
347800                                                                          
347900     MOVE 'WLZZAC01' TO SSA1                                              
348000     MOVE '  II' TO GODK-STATUSKODER                                      
348100     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-ZZAC01 SSA1                  
348200     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
348300     PERFORM IMS-STATUSKONTROLL                                           
348400     .                                                                    
348500     EJECT                                                                
348600 IMS-GU-WDB501 SECTION.                                                   
348700                                                                          
348800     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-X ')'                        
348900          DELIMITED BY SIZE INTO SSA1                                     
349000     MOVE '  GE' TO GODK-STATUSKODER                                      
349100     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-WDB501 SSA1                    
349200     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
349300     PERFORM IMS-STATUSKONTROLL                                           
349400     .                                                                    
349500     EJECT                                                                
349600 IMS-GU-WDB201 SECTION.                                                   
349700                                                                          
349800     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
349900            DELIMITED BY SIZE INTO SSA1                                   
350000     MOVE '  ' TO GODK-STATUSKODER                                        
350100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
350200     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
350300     PERFORM IMS-STATUSKONTROLL                                           
350400     .                                                                    
350500     EJECT                                                                
350600 IMS-GET-WDB201-UNIK SECTION.                                             
350700                                                                          
350800     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
350900            DELIMITED BY SIZE INTO SSA1                                   
351000     MOVE '  GE' TO GODK-STATUSKODER                                      
351100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
351200     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
351300     PERFORM IMS-STATUSKONTROLL                                           
351400     .                                                                    
351500     EJECT                                                                
351600 IMS-GET-WDB201 SECTION.                                                  
351700                                                                          
351800     STRING 'WDB201  (IDGMT   >=' W-WDB201KY-MIN-X                        
351900                    '&IDGMT   <=' W-WDB201KY-MAX-X ')'                    
352000            DELIMITED BY SIZE INTO SSA1                                   
352100     MOVE '  ' TO GODK-STATUSKODER                                        
352200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
352300     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
352400     PERFORM IMS-STATUSKONTROLL                                           
352500     .                                                                    
352600     EJECT                                                                
352700 IMS-GU-WDB101 SECTION.                                                   
352800                                                                          
352900     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
353000          DELIMITED BY SIZE INTO SSA1                                     
353100     MOVE '  ' TO GODK-STATUSKODER                                        
353200     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
353300     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
353400     PERFORM IMS-STATUSKONTROLL                                           
353500     .                                                                    
353600     EJECT                                                                
353700 IMS-GU-STYR SECTION.                                                     
353800                                                                          
353900     STRING 'WLXXJN01(WDGXKEY  =' W-WDGX01 ')'                            
354000            DELIMITED BY SIZE INTO SSA1                                   
354100     STRING 'WLXXJN11(KDTPOTYP =' W-KDTPOTYP-N5-X                         
354200                    '&KDORDKL  =' W-KDORDKL-N5-X                          
354300                    '&IDDISTRF<=' W-IDDISTR-FOM-N5-X                      
354400                    '&IDDISTRT>=' W-IDDISTR-TOM-N5-X ')'                  
354500            DELIMITED BY SIZE INTO SSA2                                   
354600     MOVE '  ' TO GODK-STATUSKODER                                        
354700     CALL CBLTDLI USING GU STYR-PCB DLI-IO-4512 SSA1 SSA2                 
354800     MOVE STYR-STATUS-CODE TO STATUS-WS                                   
354900     PERFORM IMS-STATUSKONTROLL                                           
355000     .                                                                    
355100     EJECT                                                                
355200 IMS-GU-ORQM-WDQ1 SECTION.                                                
355300     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
355400                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
355500          DELIMITED BY SIZE INTO SSA1                                     
355600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
355700     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-GU-Q101 SSA1                   
355800     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
355900     PERFORM IMS-STATUSKONTROLL                                           
356000     .                                                                    
356100     EJECT                                                                
356200 IMS-ISRT-ORQM-WDQ1 SECTION.                                              
356300                                                                          
356400     MOVE 'WLORQM01 ' TO SSA1                                             
356500     MOVE '  II' TO GODK-STATUSKODER                                      
356600     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-ISRT-Q101 SSA1               
356700     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
356800     PERFORM IMS-STATUSKONTROLL                                           
356900     .                                                                    
357000     EJECT                                                                
357100 IMS-GU-ORQI01-CSEQ SECTION.                                              
357200                                                                          
357300     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
357400          DELIMITED BY SIZE INTO SSA1                                     
357500     MOVE '  ' TO GODK-STATUSKODER                                        
357600     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-WDQ201 SSA1                    
357700     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
357800     PERFORM IMS-STATUSKONTROLL                                           
357900     .                                                                    
358000     EJECT                                                                
358100 IMS-GU-WDM211 SECTION.                                                   
358200                                                                          
358300     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
358400          DELIMITED BY SIZE INTO SSA1                                     
358500     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
358600          DELIMITED BY SIZE INTO SSA2                                     
358700     MOVE '  GE'              TO GODK-STATUSKODER                         
358800     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
358900     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
359000     PERFORM IMS-STATUSKONTROLL                                           
359100     .                                                                    
359200                                                                          
359300 IMS-GNP-WDM221 SECTION.                                                  
359400                                                                          
359500     MOVE 'WDM221 '           TO SSA1                                     
359600     MOVE '    GE'            TO GODK-STATUSKODER                         
359700     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
359800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
359900     PERFORM IMS-STATUSKONTROLL                                           
360000     .                                                                    
360100                                                                          
360200 IMS-GHU-WDM211 SECTION.                                                  
360300                                                                          
360400     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
360500          DELIMITED BY SIZE INTO SSA1                                     
360600     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
360700          DELIMITED BY SIZE INTO SSA2                                     
360800     MOVE '  GE'              TO GODK-STATUSKODER                         
360900     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
361000     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
361100     PERFORM IMS-STATUSKONTROLL                                           
361200     .                                                                    
361300                                                                          
361400 IMS-REPL-WDM211 SECTION.                                                 
361500                                                                          
361600     MOVE '  '             TO GODK-STATUSKODER                            
361700     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
361800     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
361900     PERFORM IMS-STATUSKONTROLL                                           
362000     .                                                                    
362100     EJECT                                                                
362200 IMS-GHU-WDM221 SECTION.                                                  
362300                                                                          
362400     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
362500          DELIMITED BY SIZE INTO SSA1                                     
362600     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
362700          DELIMITED BY SIZE INTO SSA2                                     
362800     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
362900          DELIMITED BY SIZE INTO SSA3                                     
363000     MOVE '  GE' TO GODK-STATUSKODER                                      
363100     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
363200     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
363300     PERFORM IMS-STATUSKONTROLL                                           
363400     .                                                                    
363500                                                                          
363600 IMS-REPL-WDM221 SECTION.                                                 
363700                                                                          
363800     MOVE '  '             TO GODK-STATUSKODER                            
363900     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
364000     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
364100     PERFORM IMS-STATUSKONTROLL                                           
364200     .                                                                    
364300     EJECT                                                                
364400 IMS-GU-XXBX-WDR220 SECTION.                                              
364500                                                                          
364600     STRING 'WLXXBX01(WDGXKEY  =' W-WDGX2231-X ')'                        
364700          DELIMITED BY SIZE INTO SSA1                                     
364800     STRING 'WLXXBX11(WDGXKEY  =' W-WDGX2232-X ')'                        
364900          DELIMITED BY SIZE INTO SSA2                                     
365000     MOVE '  GE' TO GODK-STATUSKODER                                      
365100     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-2232 SSA1 SSA2                 
365200     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
365300     PERFORM IMS-STATUSKONTROLL                                           
365400     .                                                                    
365500     EJECT                                                                
365600 DB2-SELECT-TP4TRAN     SECTION.                                          
365700     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
365800                                                                          
365900     MOVE 000100 TO GODK-SQLCODEKODER                                     
366000                                                                          
366100     EXEC SQL                                                             
366200           SELECT  DISTINCT                                               
366300                   IDDC_REC                                               
366400                                                                          
366500           INTO   :TP4TRAN-IDDC-REC                                       
366600                                                                          
366700           FROM    TP4TRAN                                                
366800                                                                          
366900           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
367000     END-EXEC                                                             
367100                                                                          
367200     MOVE SQLCODE TO SQLCODE-WS                                           
367300     PERFORM DB2-STATUSKONTROLL                                           
367400     .                                                                    
367500     EJECT                                                                
367600 IMS-GU-WDB601    SECTION.                                                
367700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
367800          DELIMITED BY SIZE INTO SSA1                                     
367900     MOVE '  GE' TO GODK-STATUSKODER                                      
368000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
368100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
368200     PERFORM IMS-STATUSKONTROLL                                           
368300     IF SEGMENT-SAKNAS                                                    
368400         MOVE SPACE TO DCS-KDDC                                           
368500     END-IF                                                               
368600     .                                                                    
368700                                                                          
368800 IMS-STATUSKONTROLL SECTION.                                              
368900                                                                          
369000     SET STATUS-IX TO 1                                                   
369100     SEARCH GODK-STATUS AT END                                            
369200       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
369300       DELIMITED BY SIZE INTO FELTEXT                                     
369400       CALL FELLOG                                                        
369500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
369600     END-SEARCH                                                           
369700     .                                                                    
369800     EJECT                                                                
369900 DB2-STATUSKONTROLL  SECTION.                                             
370000                                                                          
370100     SET SQLCODE-IX TO 1                                                  
370200     SEARCH GODK-SQLCODE                                                  
370300       AT END                                                             
370400          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
370500          DELIMITED BY SIZE INTO FELTEXT                                  
370600          CALL ABEND USING RKOD-ABEND-DB2                                 
370700       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
370800     END-SEARCH                                                           
370900     .                                                                    
371000     EJECT                                                                
372000*    -COPY WY2000P3                                                       
