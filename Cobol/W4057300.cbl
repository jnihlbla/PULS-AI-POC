000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W4057300.                                                
000500 AUTHOR.         LENNART LUNDGREN ADB-GRUPPEN.                            
000600 DATE-WRITTEN.   OKT. 1988.                                               
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*                                                                         
001200*     BESKRIVNING                                                         
001300*                                                                         
001400*     DETTA PROGRAM HANTERAR BILD 4573 RO/TPO 3                           
001500*     PROGRAMMET TILLÅTER LÄSNING,                                        
001600*     FÖRÄNDRING OCH DELNING AV RO RADER MED KDSTARAD = 3                 
001700*     I PROGRAMMET ANVÄNDS FÖLJANDE BASER:                                
001800*                                                                         
001900*     ROREG.                                                              
002000*     KUNDREG.                                                            
002100*     ARTREG.                                                             
002200*     LOGGREG.                                                            
002300*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T573                                              
002700*        MID:         W4I57301                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W4O57301                                            
003100* ETRACK 1290414 INTERVALL FOR DISTRICT AND CUSTOMER                      
003200* ETRACK 7450328 2008-HÖST  VOHF                                          
003300* ETRACK 10254592     2015  DECOMISSION VOHF                              
003400* ETRACK 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2             
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     SKIP3                                                                
003800 DATA DIVISION.                                                           
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100*    -- CHECKED BY WY2000                                                 
004200     SKIP3                                                                
004300 77  IDPGM                     PIC X(8)     VALUE 'W4057300'.             
004400 77  FELTEXT                   PIC X(80)    VALUE SPACE.                  
004500 77  RKOD-ABEND                PIC S9(4)    VALUE +33 COMP SYNC.          
004600 77  JA                        PIC X(1)     VALUE 'J'.                    
004700 77  NEJ                       PIC X(1)     VALUE 'N'.                    
004800 77  W-ENTER                   PIC X(1)     VALUE SPACE.                  
004900 77  W-KVRAD                   PIC 9(6)     VALUE ZERO.                   
005000 77  W-PUNKT                   PIC 9(1)     VALUE ZERO.                   
005100 77  W-ANTAL-1                 PIC 9(6)V    VALUE ZERO.                   
005200 77  W-ANTAL-2                 PIC 9(6)V    VALUE ZERO.                   
005300 77  W-VALKOD                  PIC X(1)     VALUE ' '.                    
005400 77  W-OPPNAD                  PIC X(1)     VALUE 'N'.                    
005500 77  W-NEWKEY                  PIC X(1)     VALUE ' '.                    
005600 77  W-FAELT-IFYLLT            PIC X(1)     VALUE 'N'.                    
005700 77  W-DELNING                 PIC X(1)     VALUE 'N'.                    
005800 77  W-UPDATE                  PIC X(1)     VALUE 'N'.                    
005900 77  KVQPACK-OK                PIC X(1)     VALUE 'J'.                    
006000 77  FRAKT-OK                  PIC X(1)     VALUE 'J'.                    
006100 77  ANTAL-OK                  PIC X(1)     VALUE 'J'.                    
006200 77  KLASS-OK                  PIC X(1)     VALUE 'J'.                    
006300 77  PRIS-OK                   PIC X(1)     VALUE 'J'.                    
006400 77  INPUT-OK                  PIC X(1)     VALUE 'J'.                    
006500 77  VALKOD-OK                 PIC X(1)     VALUE 'J'.                    
006600 77  AENDRA-ENTER              PIC X(1)     VALUE 'J'.                    
006700 77  OPP-FAELT-OK              PIC X(1)     VALUE 'J'.                    
006800 77  IDDISTR-WS                PIC X(4)     VALUE SPACE.                  
006900 77  IDKUNDNR-WS               PIC X(6)     VALUE SPACE.                  
007000 77  IDARTNR-WS                PIC X(9)     VALUE SPACE.                  
007100 77  KDORDKL-WS                PIC X(1)     VALUE SPACE.                  
007200 77  KDPRODSL-WS               PIC X(2)     VALUE SPACE.                  
007300 77  IDLOPNR-WS                PIC X(2)     VALUE SPACE.                  
007400 77  IDORDNR-WS                PIC X(5)     VALUE SPACE.                  
007500 77  KDTPOTYP-WS               PIC X(1)     VALUE SPACE.                  
007600 77  IDDC-WS                   PIC X(2)     VALUE SPACE.                  
007700 77  INDX                      PIC S9(9)    VALUE ZERO  COMP SYNC.        
007800 77  SPRAK-IX                  PIC S9(9)    VALUE ZERO  COMP SYNC.        
007900 77  MAX-LINE                  PIC S9(9)    VALUE +13   COMP SYNC.        
008000 77  SPAR-PRARTNTO             PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
008100 77  SPAR-IDARTNR              PIC S9(9)     COMP-3 VALUE ZERO.           
008200 77  SPAR-KVART                PIC 9(7)      VALUE ZERO.                  
008300 77  SPAR-KVOI                 PIC 9(7)      VALUE ZERO.                  
008400 77  SPAR-BEART                PIC X(20).                                 
008500 77  SPAR-KDFARLIG             PIC 9.                                     
008600 77  W-KVART                   PIC 9(7)      VALUE ZERO.                  
008700 77  W-IDDISTR                 PIC 9(4)      VALUE ZERO.                  
008800 77  W-IDKUNDNR                PIC 9(6)      VALUE ZERO.                  
008900 77  W-IDARTNR                 PIC 9(9)      VALUE ZERO.                  
009000 77  W-KDORDKL                 PIC 9(1)      VALUE ZERO.                  
009100 77  W-KDPRODSL                PIC 9(3)      VALUE ZERO.                  
009200 77  W-IDORDNR                 PIC 9(5)      VALUE ZERO.                  
009300 77  W-KDTPOTYP                PIC 9(1)      VALUE ZERO.                  
009400 77  W-IDDC                    PIC X(2)      VALUE ZERO.                  
009500 77  KVART-OPP                 PIC 9(7)      VALUE ZERO.                  
009600 77  KDORDKL-OPP               PIC 9(1)      VALUE ZERO.                  
009700 77  KDFRAKT-OPP               PIC 9(2)      VALUE ZERO.                  
009800                                                                          
009900 77  W-TIREGDAT-TIAAVV         PIC 9(4)      VALUE ZERO.                  
010000 77  W-DAGENS-DAT-TIAAVV       PIC 9(4)      VALUE ZERO.                  
010100 77  W-TIBEHOV                 PIC 9(4)      VALUE ZERO.                  
010200                                                                          
010300 77  DATUM-MED-ARHUNDR         PIC 9(8)      VALUE ZERO.                  
010400                                                                          
010500 77  WS-IDTRANS                PIC X(4).                                  
010600     88  WS-GODKAEND-BILD           VALUE '4571' '4572' '4573'.           
010700     88  EGEN-MID                   VALUE '4573'.                         
010800                                                                          
010900 77  IFYLLT-SW                 PIC X        VALUE 'N'.                    
011000     88  INGET-IFYLLT                       VALUE 'N'.                    
011100                                                                          
011200 77  KVOKS-SW                  PIC X        VALUE 'N'.                    
011300     88  KVOKS-ANDRAT                       VALUE 'J'.                    
011400                                                                          
011500*      --- VALID IDDC CODES                                               
011600*01    -COPY WWDCKONS                                                     
011700                                                                          
011800*      BYTESARTIKLAR                                                      
011900*01  -COPY WWBYT03                                                        
012000                                                                          
012100     EJECT                                                                
012200 01  W-SPAR-RAD.                                                          
012300     03  W-SPAR-KVART          PIC X(7)     VALUE SPACE.                  
012400     03  WW-SPAR-KVART         PIC 9(7)     VALUE ZERO.                   
012500     03  W-SPAR-KDORDKL        PIC 9(1)     VALUE ZERO.                   
012600     03  W-SPAR-KDFRAKT        PIC 9(3)     VALUE ZERO.                   
012700     03  W-SPAR-KDRAPRIO       PIC 9(3)     VALUE ZERO.                   
012800     03  W-SPAR-PRARTNTO       PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
012900     03  W-SPAR-PRARTNTO-LOC   PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
013000     03  W-SPAR-PRARTNTO-LOCPREL PIC S9(7)V9(2) COMP-3 VALUE ZERO.        
013100     03  W-SPAR-IDLOPNR        PIC 9(2)     VALUE ZERO.                   
013200                                                                          
013300 01  W-PRARTNTO.                                                          
013400     03  WW-SPAR-PRARTNTO      PIC X(10)    VALUE ZERO.                   
013500                                                                          
013600 01  REDIGERADE-OPPRAD-9.                                                 
013700     03 W9-UT-KVART-OPP          PIC Z(6)9.                               
013800     03 W9-UT-KDORDKL-OPP        PIC 9.                                   
013900     03 W9-UT-KDFRAKT-OPP        PIC Z9.                                  
014000     03 W9-UT-PRARTNTO-OPP       PIC Z(9)9.                               
014100                                                                          
014200 01  W-IDKUNDRF.                                                          
014300     03  W-IDKUNDRF-1-5        PIC 9(5).                                  
014400     03  FILLER                PIC X(5) VALUE SPACE.                      
014500                                                                          
014600 01  W-BEART-01.                                                          
014700     03  W-BEART-01-01         PIC X(20) VALUE SPACE.                     
014800     03  FILLER                PIC X(10) VALUE SPACE.                     
014900                                                                          
015000 01  W-BEART-02.                                                          
015100     03  W-IDKONTO             PIC X(10) VALUE SPACE.                     
015200     03  W-IDKST               PIC X(10) VALUE SPACE.                     
015300                                                                          
015400 01  WS-RAD-IDKONTO            PIC 9(11).                                 
015500 01  FILLER REDEFINES WS-RAD-IDKONTO.                                     
015600     03  FILLER                PIC X(01).                                 
015700     03  WS-IDKONTO            PIC X(10).                                 
015800 01 DB2-LASNING.                                                          
015900     03 FILLER                   PIC X(16)   VALUE                        
016000                                             'WS-DB2-SEKTION'.            
016100     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
016200                                                                          
016300                                                                          
016400 01 NYCKLAR-TP4TRAN.                                                      
016500     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
016600                                                                          
016700     EJECT                                                                
016800*  ---PARAMETRAR TILL IDDISTR                                             
016900                                                                          
017000 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
017100*01  FILLER -COPY WWDIST19 -RED TEST-IDDISTR.                             
017200     SKIP3                                                                
017300*01  FILLER -COPY WWDIST20 -RED TEST-IDDISTR.                             
017400     EJECT                                                                
017500*01  FILLER -COPY WWDIST35 -RED TEST-IDDISTR.                             
017600     EJECT                                                                
017700*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
017800     EJECT                                                                
017900 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
018000*   -COPY WWDIST57                                                        
018100     EJECT                                                                
018200                                                                          
018300 01  FILLER                       PIC X(12) VALUE 'DLI NYCKLAR'.          
018400 01  FILLER.                                                              
018500                                                                          
018600     03  W-SOK-KEY-MIN.                                                   
018700         05  W-IDARTNR-MIN-X.                                             
018800             07  W-IDARTNR-MIN    PIC S9(9)  COMP-3 VALUE ZERO.           
018900         05  W-IDKUNDRF-MIN.                                              
019000             07  W-IDORDNR-MIN    PIC 9(5)          VALUE ZERO.           
019100             07  FILLER           PIC X(5)          VALUE SPACE.          
019200         05  W-KDORDKL-MIN-X.                                             
019300             07  W-KDORDKL-MIN    PIC S9(1)  COMP-3 VALUE ZERO.           
019400         05  W-KDPRODSL-MIN-X.                                            
019500             07  W-KDPRODSL-MIN   PIC S9(3)  COMP-3 VALUE ZERO.           
019600         05  W-KDTPOTYP-MIN-X.                                            
019700             07  W-KDTPOTYP-MIN   PIC S9(1)  COMP-3 VALUE ZERO.           
019800         05  W-IDDC-MIN-X.                                                
019900             07  W-IDDC-MIN       PIC X(2)          VALUE ZERO.           
020000                                                                          
020100     03  W-SOK-KEY-MAX.                                                   
020200         05  W-IDARTNR-MAX-X.                                             
020300             07  W-IDARTNR-MAX    PIC S9(9)  COMP-3  VALUE ZERO.          
020400         05  W-IDKUNDRF-MAX.                                              
020500             07  W-IDORDNR-MAX    PIC 9(5)           VALUE ZERO.          
020600             07  FILLER           PIC X(5)           VALUE SPACE.         
020700         05  W-KDORDKL-MAX-X.                                             
020800             07  W-KDORDKL-MAX    PIC S9(1)  COMP-3  VALUE ZERO.          
020900         05  W-KDPRODSL-MAX-X.                                            
021000             07  W-KDPRODSL-MAX   PIC S9(3)  COMP-3  VALUE ZERO.          
021100         05  W-KDTPOTYP-MAX-X.                                            
021200             07  W-KDTPOTYP-MAX   PIC S9(1)  COMP-3  VALUE ZERO.          
021300         05  W-IDDC-MAX-X.                                                
021400             07  W-IDDC-MAX       PIC X(2)           VALUE ZERO.          
021500     EJECT                                                                
021600     03  W-WDA5B1KY-MIN.                                                  
021700         05  W-IDDISTR-N1-MIN     PIC S9(5) COMP-3   VALUE ZERO.          
021800         05  W-IDKUNDNR-N1-MIN    PIC S9(7) COMP-3   VALUE ZERO.          
021900         05  W-IDDC-N1-MIN        PIC X(2)           VALUE ZERO.          
022000         05  W-IDARTNR-N1-MIN     PIC S9(9) COMP-3   VALUE ZERO.          
022100         05  W-IDKUNDRF-N1-MIN.                                           
022200             07  W-IDORDNR-N1-MIN PIC S9(5)          VALUE ZERO.          
022300             07  FILLER           PIC X(5)           VALUE SPACE.         
022400         05  W-IDLOPNR-N1-MIN     PIC S9(3) COMP-3   VALUE ZERO.          
022500                                                                          
022600     03  W-WDA5B1KY-MAX.                                                  
022700         05  W-IDDISTR-N1-MAX     PIC S9(5) COMP-3   VALUE ZERO.          
022800         05  W-IDKUNDNR-N1-MAX    PIC S9(7) COMP-3   VALUE ZERO.          
022900         05  W-IDDC-N1-MAX        PIC X(2)           VALUE ZERO.          
023000         05  W-IDARTNR-N1-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
023100         05  W-IDKUNDRF-N1-MAX.                                           
023200             07  W-IDORDNR-N1-MAX PIC S9(5)          VALUE ZERO.          
023300             07  FILLER           PIC X(5)           VALUE SPACE.         
023400         05  W-IDLOPNR-N1-MAX     PIC S9(3) COMP-3   VALUE ZERO.          
023500                                                                          
023600     03  W-WDA5B1KY.                                                      
023700         05  W-IDDISTR-B1         PIC S9(5) COMP-3   VALUE ZERO.          
023800         05  W-IDKUNDNR-B1        PIC S9(7) COMP-3   VALUE ZERO.          
023900         05  W-IDDC-B1            PIC X(2)           VALUE ZERO.          
024000         05  W-IDARTNR-B1         PIC S9(9) COMP-3   VALUE ZERO.          
024100         05  W-IDKUNDRF-X.                                                
024200             07  W-IDORDNR-B1     PIC 9(5)           VALUE ZERO.          
024300             07  FILLER           PIC X(5)           VALUE SPACE.         
024400         05  W-IDLOPNR-B1         PIC S9(3) COMP-3   VALUE ZERO.          
024500                                                                          
024600     03  W-WDA5B1MIN.                                                     
024700         05  W-IDDISTR-B2-MIN     PIC S9(5) COMP-3   VALUE ZERO.          
024800         05  W-IDKUNDNR-B2-MIN    PIC S9(7) COMP-3   VALUE ZERO.          
024900         05  W-IDDC-B2-MIN        PIC X(2)           VALUE ZERO.          
025000         05  W-IDARTNR-B2-MIN     PIC S9(9) COMP-3   VALUE ZERO.          
025100         05  W-IDKUNDRF-B2-MIN.                                           
025200             07  W-IDORDNR-B2-MIN PIC 9(5)           VALUE ZERO.          
025300             07  FILLER           PIC X(5)           VALUE SPACE.         
025400         05  W-IDLOPNR-B2-MIN     PIC S9(3) COMP-3   VALUE ZERO.          
025500                                                                          
025600     03  W-WDA5B1MAX.                                                     
025700         05  W-IDDISTR-B2-MAX     PIC S9(5) COMP-3   VALUE ZERO.          
025800         05  W-IDKUNDNR-B2-MAX    PIC S9(7) COMP-3   VALUE ZERO.          
025900         05  W-IDDC-B2-MAX        PIC X(2)           VALUE ZERO.          
026000         05  W-IDARTNR-B2-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
026100         05  W-IDKUNDRF-B2-MAX.                                           
026200             07  W-IDORDNR-B2-MAX PIC 9(5)           VALUE ZERO.          
026300             07  FILLER           PIC X(5)           VALUE SPACE.         
026400         05  W-IDLOPNR-B2-MAX     PIC S9(3) COMP-3   VALUE ZERO.          
026500                                                                          
026600     03  W-WDA501KY.                                                      
026700         05  W-IDDISTR-N2         PIC S9(5) COMP-3   VALUE ZERO.          
026800         05  W-IDKUNDNR-N2        PIC S9(7) COMP-3   VALUE ZERO.          
026900         05  W-IDKUNDRF-N2.                                               
027000             07  W-IDORDNR-N2     PIC 9(5)           VALUE ZERO.          
027100             07  FILLER           PIC X(5)           VALUE SPACE.         
027200         05  W-IDARTNR-N2         PIC S9(9) COMP-3   VALUE ZERO.          
027300         05  W-IDLOPNR-N2         PIC S9(3) COMP-3   VALUE ZERO.          
027400     EJECT                                                                
027500                                                                          
027600     03  W-WDK711-IDARTNR-X.                                              
027700         05  W-WDK711-IDARTNR-N   PIC S9(9) COMP-3   VALUE ZERO.          
027800                                                                          
027900     03  W-WDK711-IDDC-X.                                                 
028000         05  W-WDK711-IDDC        PIC X(2)           VALUE ZERO.          
028100                                                                          
028200     03  W-IDARTNR-WDK6-X.                                                
028300         05  W-IDARTNR-WDK601     PIC S9(9) COMP-3   VALUE ZERO.          
028400                                                                          
028500     03  W-IDARTNR-X.                                                     
028600         05  W-IDARTNR-N          PIC S9(9) COMP-3   VALUE ZERO.          
028700                                                                          
028800     03  W-WDB201KY-X.                                                    
028900         05  W-IDDISTR-B201       PIC S9(5)  COMP-3  VALUE ZERO.          
029000         05  W-IDKUNDNR-B201      PIC S9(7)  COMP-3  VALUE ZERO.          
029100                                                                          
029200     03 W-WDB201KY-MIN-X.                                                 
029300        05  W-IDDISTR-B201-MIN  PIC S9(5) COMP-3 VALUE ZERO.              
029400        05  W-IDKUNDNR-B201-MIN PIC S9(7) COMP-3 VALUE ZERO.              
029500                                                                          
029600     03 W-WDB201KY-MAX-X.                                                 
029700        05  W-IDDISTR-B201-MAX  PIC S9(5) COMP-3 VALUE ZERO.              
029800        05  W-IDKUNDNR-B201-MAX PIC S9(7) COMP-3 VALUE ZERO.              
029900                                                                          
030000     03  W-WDB101KY-X.                                                    
030100         05  W-WDB1-IDPARTNR     PIC X(9)   VALUE   SPACE.                
030200         05  W-WDB1-IDFTG        PIC 9(2)   VALUE   ZERO.                 
030300                                                                          
030400     03  W-WDB501KY-X.                                                    
030500         05  W-IDDC-WDB5         PIC X(2)    VALUE SPACE.                 
030600         05  W-KDFRAKT-WDB5      PIC S9(3)   VALUE ZERO COMP-3.           
030700         05  W-IDDISTR-WDB5      PIC S9(5)   VALUE ZERO COMP-3.           
030800         05  W-IDKUNDNR-WDB5     PIC S9(7)   VALUE ZERO COMP-3.           
030900                                                                          
031000     03  W-BENA01-X.                                                      
031100         05  W-IDARTNR-N5         PIC S9(9) COMP-3  VALUE ZERO.           
031200     03  W-BENA11-X.                                                      
031300         05  W-IDSKYLT-N5         PIC X(3)          VALUE SPACE.          
031400                                                                          
031500     03  W-IDGMTREF-X.                                                    
031600         05  W-IDDISTR-N9         PIC S9(5)  COMP-3  VALUE ZERO.          
031700         05  W-IDKUNDNR-N9        PIC S9(7)  COMP-3  VALUE ZERO.          
031800         05  W-IDKUNDRF-N9        PIC X(10)  VALUE SPACE.                 
031900                                                                          
032000     EJECT                                                                
032100     03  W-WDM201-X.                                                      
032200         05  W-KAMP-IDKAMPRF      PIC S9(07)   VALUE ZERO COMP-3.         
032300         05  W-KAMP-IDDC          PIC X(02)    VALUE SPACE.               
032400                                                                          
032500     03  W-WDM211-X.                                                      
032600         05  W-KART-IDARTNR       PIC S9(09)   VALUE ZERO COMP-3.         
032700                                                                          
032800     03  W-WDM221-X.                                                      
032900         05  W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.           
033000         05  W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.           
033100         05  W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.           
033200         05  W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.           
033300                                                                          
033400     03  W-IDDC-B6-X.                                                     
033500         05 W-IDDC-B6            PIC X(2).                                
033600                                                                          
033700     03  W-IDDC-B6-USER-X.                                                
033800         05 W-IDDC-B6-USER       PIC X(2).                                
033900     EJECT                                                                
034000*01    -COPY WWTEXT01                                                     
034100     EJECT                                                                
034200                                                                          
034300 01  SUBPROGRAM.                                                          
034400     03  CBLTDLI                  PIC X(8)      VALUE 'CBLTDLI '.         
034500     03  FELLOG                   PIC X(8)      VALUE 'FELLOG  '.         
034600     03  ABEND                    PIC X(8)      VALUE 'ABEND   '.         
034700     03  WSECURIT                 PIC X(8)      VALUE 'WSECURIT'.         
034800     03  WDECEDIT                 PIC X(8)      VALUE 'WDECEDIT'.         
034900     03  WDATKONV                 PIC X(8)      VALUE 'WDATKONV'.         
035000     03  W005INIT                 PIC X(8)      VALUE 'W005INIT'.         
035100     EJECT                                                                
035200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
035300*01 -COPY WMSGINIT                                                        
035400     EJECT                                                                
035500     EJECT                                                                
035600*   --- PARAMETRAR TILL SUBPROGRAM WSECURIT                               
035700                                                                          
035800*01      FILLER -COPY WSECAREA.                                           
035900     EJECT                                                                
036000*   --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                               
036100                                                                          
036200*01      FILLER -COPY WDECAREA.                                           
036300     EJECT                                                                
036400*   --- PARAMETRAR TILL SUBPROGRAM WDATKONV                               
036500                                                                          
036600*01    FILLER -COPY WDATAREA                                              
036700     EJECT                                                                
036800*  ---COPYTEXT TILL SUC-TRANS                                             
036900     SKIP2                                                                
037000*01    -COPY WDGZSUC                                                      
037100     EJECT                                                                
037200*  ---COPYTEXT TILL RY9-TRANS                                             
037300     SKIP2                                                                
037400*01    -COPY WDGZRY9                                                      
037500     EJECT                                                                
037600*01    -COPY WDGZRY9S                                                     
037700     EJECT                                                                
037800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
037900                                                                          
038000 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
038100     SKIP3                                                                
038200*01    MID -COPY W4I57301.                                                
038300     EJECT                                                                
038400*01    -COPY WMSGAREA                                                     
038500     EJECT                                                                
038600*  03    MOD -COPY W4O57301  -RED MSG-AREA.                               
038700     EJECT                                                                
038800*01    -COPY WMFSAREA                                                     
038900     EJECT                                                                
039000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
039100                                                                          
039200 01    IMS-WS.                                                            
039300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
039400     SKIP3                                                                
039500*                        **** STATUS-KOD FRÅN IMS                         
039600   03    STATUS-WS               PIC XX.                                  
039700     88    SEGMENT-FINNS                    VALUE '  '.                   
039800     88    SEGMENT-HAR-LAGTS-TILL           VALUE '  '.                   
039900     88    SEGMENT-FINNS-REDAN              VALUE 'II'.                   
040000     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
040100     88    SEGMENT-SLUT                     VALUE 'GB'.                   
040200     SKIP3                                                                
040300   03    GODK-STATUSKODER.                                                
040400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
040500                                                                          
040600 01    SSA1                      PIC X(320).                              
040700 01    SSA2                      PIC X(128).                              
040800 01    SSA3                      PIC X(128).                              
040900     EJECT                                                                
041000*                            DB2 FUNKTIONSKODER                           
041100 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
041200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
041300                                                                          
041400 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
041500 01  DB2-WS.                                                              
041600     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
041700         88  CURSOR-OK                       VALUE 000.                   
041800         88  RADER-FINNS                     VALUE 000.                   
041900         88  RADER-SAKNAS                    VALUE 100.                   
042000         88  ATKOMST-FEL                     VALUE 904.                   
042100     03  GODK-SQLCODEKODER.                                               
042200         05  GODK-SQLCODE OCCURS 5                                        
042300             INDEXED BY SQLCODE-IX PIC 9(3).                              
042400 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
042500     EJECT                                                                
042600*                            IMS FUNKTIONSKODER                           
042700*01    -COPY W0003                                                        
042800     EJECT                                                                
042900*                            DLI INPUT-OUTPUT AREA                        
043000*                            DLI-IO-AREA                                  
043100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDA5'.             
043200 01  DLI-IO-WDA5.                                                         
043300*  03    WDA501 -COPY WDA501                                              
043400     EJECT                                                                
043500                                                                          
043600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDA5B'.            
043700 01  DLI-IO-WDA5B.                                                        
043800*  03    WDA5B1 -COPY WDA5B1                                              
043900     EJECT                                                                
044000                                                                          
044100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK601'.           
044200 01    DLI-IO-WDK601.                                                     
044300*  03    WDK601 -COPY WDK601                                              
044400     EJECT                                                                
044500                                                                          
044600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK611'.           
044700 01  DLI-IO-WDK611.                                                       
044800*  03    WDK611 -COPY WDK611                                              
044900     EJECT                                                                
045000                                                                          
045100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
045200 01  DLI-IO-WDK711.                                                       
045300*  03    WDK711 -COPY WDK711                                              
045400     EJECT                                                                
045500                                                                          
045600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ZZAC01'.           
045700 01  DLI-IO-ZZAC01.                                                       
045800*  03    WDGZ01 -COPY WDGZ01 -PRE ZZAC-                                   
045900     EJECT                                                                
046000                                                                          
046100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-KNDC01'.           
046200 01  DLI-IO-KNDC01.                                                       
046300*  03    WDB201 -COPY WDB201 -PRE K-201-                                  
046400     EJECT                                                                
046500                                                                          
046600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB501'.           
046700 01  DLI-IO-WDB501.                                                       
046800*  03    WDB501 -COPY WDB501                                              
046900     EJECT                                                                
047000                                                                          
047100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-BENA11'.           
047200 01  DLI-IO-BENA11.                                                       
047300*  03    WDD311 -COPY WDD311 -PRE BENA-                                   
047400     EJECT                                                                
047500                                                                          
047600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ARTM01'.           
047700 01  DLI-IO-ARTM01.                                                       
047800*  03    WDK901 -COPY WDK901                                              
047900     EJECT                                                                
048000                                                                          
048100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB201'.           
048200 01  DLI-IO-WDB201.                                                       
048300*  03    WDB201 -COPY WDB201                                              
048400     EJECT                                                                
048500                                                                          
048600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB101'.           
048700 01  DLI-IO-WDB101.                                                       
048800*  03    WDB101 -COPY WDB101                                              
048900     EJECT                                                                
049000                                                                          
049100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ORQI01'.           
049200 01  DLI-IO-ORQI01.                                                       
049300*  03    WDQ201 -COPY WDQ201 -PRE ORQI-                                   
049400     EJECT                                                                
049500                                                                          
049600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ORQM01'.           
049700 01  DLI-IO-ORQM01.                                                       
049800*  03    WDQ101 -COPY WDQ101 -PRE ORQM-                                   
049900     EJECT                                                                
050000                                                                          
050100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
050200 01  DLI-IO-WDM211.                                                       
050300*    03 -COPY WDM211                                                      
050400     EJECT                                                                
050500 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
050600 01  DLI-IO-WDM221.                                                       
050700*    03 -COPY WDM221                                                      
050800     EJECT                                                                
050900*    MSG-AREA FÖR HOPP TILL W20109                                        
051000 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
051100 01  W-PROG-TO-PROG-SW-1.                                                 
051200     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
051300     03  2109-Z1                   PIC X.                                 
051400     03  2109-Z2                   PIC X.                                 
051500     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
051600     03  2109-IDTRANS              PIC X(4)  VALUE '4573'.                
051700     03  2109-KDMFSFOR             PIC X.                                 
051800*    03  -COPY W2I10902    -PRE 2109-                                     
051900                                                                          
052000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
052100 01   DLI-IO-AREA-B601.                                                   
052200*     03  -COPY WDB601                                                    
052300                                                                          
052400 01  FILLER               PIC X(16)   VALUE 'WDB601 USER'.                
052500 01   DLI-IO-AREA-B601-USER.                                              
052600*     03  -COPY WDB601   -PRE USER-                                       
052700     EJECT                                                                
052800 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
052900                                                                          
053000*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
053100     EJECT                                                                
053200     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
053300     EJECT                                                                
053400 LINKAGE SECTION.                                                         
053500     SKIP2                                                                
053600*01    -COPY W0009     -PRE MSG-                                          
053700     EJECT                                                                
053800*01    -COPY W0009     -PRE 2109-                                         
053900     EJECT                                                                
054000*01    -COPY W0008     -PRE USEA-                                         
054100     05  FILLER                  PIC X.                                   
054200     EJECT                                                                
054300*01    -COPY W0008     -PRE ORDP-                                         
054400     05  FILLER                  PIC X.                                   
054500     EJECT                                                                
054600*01    -COPY W0008     -PRE ORDR-                                         
054700     05  FILLER                  PIC X.                                   
054800     EJECT                                                                
054900*01    -COPY W0008     -PRE ZZAC-                                         
055000     05  FILLER                  PIC X.                                   
055100     EJECT                                                                
055200*01    -COPY W0008     -PRE ART-                                          
055300     05  FILLER                  PIC X.                                   
055400     EJECT                                                                
055500*01    -COPY W0008     -PRE ARTS-                                         
055600     05  FILLER                  PIC X.                                   
055700     EJECT                                                                
055800*01    -COPY W0008     -PRE GMTC-                                         
055900     05  FILLER                  PIC X.                                   
056000     EJECT                                                                
056100*01    -COPY W0008     -PRE BENA-                                         
056200     05  FILLER                  PIC X.                                   
056300     EJECT                                                                
056400*01    -COPY W0008     -PRE ARTM-                                         
056500     05  FILLER                  PIC X.                                   
056600     EJECT                                                                
056700*01    -COPY W0008     -PRE WDB2-                                         
056800     05  FILLER                  PIC X.                                   
056900     EJECT                                                                
057000*01    -COPY W0008     -PRE WDB1-                                         
057100     05  FILLER                  PIC X.                                   
057200     EJECT                                                                
057300*01    -COPY W0008     -PRE ORQI-                                         
057400     05  FILLER                  PIC X.                                   
057500     EJECT                                                                
057600*01    -COPY W0008     -PRE ORQM-                                         
057700     05  FILLER                  PIC X.                                   
057800     EJECT                                                                
057900*01    -COPY W0008     -PRE WDM2-                                         
058000     05  FILLER                  PIC X.                                   
058100     EJECT                                                                
058200*01    -COPY W0008     -PRE WDB6-                                         
058300     05  FILLER                  PIC X.                                   
058400     EJECT                                                                
058500                                                                          
058600 PROCEDURE DIVISION USING MSG-PCB 2109-PCB                                
058700     USEA-PCB ORDP-PCB ORDR-PCB                                           
058800     ZZAC-PCB ART-PCB ARTS-PCB GMTC-PCB BENA-PCB ARTM-PCB                 
058900     WDB2-PCB WDB1-PCB ORQI-PCB ORQM-PCB WDM2-PCB WDB6-PCB.               
059000                                                                          
059100     ENTRY 'DLITCBL' USING MSG-PCB 2109-PCB                               
059200     USEA-PCB ORDP-PCB ORDR-PCB                                           
059300     ZZAC-PCB ART-PCB ARTS-PCB GMTC-PCB BENA-PCB ARTM-PCB                 
059400     WDB2-PCB WDB1-PCB ORQI-PCB ORQM-PCB WDM2-PCB WDB6-PCB.               
059500                                                                          
059600     PERFORM IMS-GU-MSG                                                   
059700     IF SEGMENT-FINNS                                                     
059800       PERFORM A-INIT                                                     
059900       IF INPUT-OK        = JA AND                                        
060000          VALKOD-OK       = JA AND                                        
060100          OPP-FAELT-OK    = JA                                            
060200          MOVE +1 TO INDX                                                 
060300          EVALUATE TRUE                                                   
060400         WHEN WS-IDTRANS NOT = '4573'                                     
060500           PERFORM B-FLYTTA-GAMMAL-KEY                                    
060600           PERFORM IMS-GN-ORDR                                            
060700           PERFORM M-LAES-RESTORDER                                       
060800         WHEN MFS-IDPFK = '7'                                             
060900           PERFORM B-FLYTTA-GAMMAL-KEY                                    
061000           PERFORM IMS-GN-ORDR                                            
061100           PERFORM M-LAES-RESTORDER                                       
061200         WHEN MFS-IDPFK = '8'                                             
061300           IF MID-IDDISTR-SPAR > ZERO                                     
061400             PERFORM C-FLYTTA-SPARAD-KEY                                  
061500             PERFORM IMS-GU-ORDR-PF8                                      
061600           ELSE                                                           
061700             PERFORM G2-FLYTTA-KEY                                        
061800             PERFORM IMS-GN-ORDR                                          
061900           END-IF                                                         
062000           PERFORM M-LAES-RESTORDER                                       
062100         WHEN MFS-UPDATE                                                  
062200           PERFORM UNTIL INDX > MID-KVRAD-SPAR                            
062300             EVALUATE TRUE                                                
062400             WHEN MID-VALKOD(INDX) = ' ' AND                              
062500                MID-KVRAD-SPAR = +1                                       
062600               PERFORM D-ANDRA                                            
062700               PERFORM IMS-GU-ORDR                                        
062800               MOVE JA TO IFYLLT-SW                                       
062900             WHEN MID-VALKOD(INDX) = 'D'                                  
063000               IF DIST19-SATS                                             
063100                 MOVE TEXT-0422(SPRAK-IX) TO                              
063200                 MOD-TEMFSFEL                                             
063300               ELSE                                                       
063400                 PERFORM E-DELETE                                         
063500                 PERFORM G2-FLYTTA-KEY                                    
063600                 PERFORM IMS-GN-ORDR                                      
063700                 MOVE JA TO IFYLLT-SW                                     
063800               END-IF                                                     
063900             END-EVALUATE                                                 
064000             ADD +1 TO INDX                                               
064100           END-PERFORM                                                    
064200           MOVE +1 TO INDX                                                
064300           IF INGET-IFYLLT                                                
064400             PERFORM G2-FLYTTA-KEY                                        
064500             PERFORM IMS-GN-ORDR                                          
064600           END-IF                                                         
064700           PERFORM M-LAES-RESTORDER                                       
064800           IF W-UPDATE = JA                                               
064900             MOVE TEXT-0404 (SPRAK-IX) TO MOD-TEMFSINF                    
065000           END-IF                                                         
065100         WHEN OTHER                                                       
065200           IF W-VALKOD = ' '                                              
065300             IF MID-OPP-RAD = ALL '+'                                     
065400               PERFORM G2-FLYTTA-KEY                                      
065500               PERFORM IMS-GN-ORDR                                        
065600               PERFORM M-LAES-RESTORDER                                   
065700             ELSE                                                         
065800               MOVE NEJ TO OPP-FAELT-OK                                   
065900                           AENDRA-ENTER                                   
066000               PERFORM K-FLYTTA-FELTEXT                                   
066100             END-IF                                                       
066200           ELSE                                                           
066300             PERFORM UNTIL INDX > MID-KVRAD-SPAR                          
066400               IF MID-VALKOD(INDX) = 'A'                                  
066500                 IF DIST19-SATS                                           
066600                   MOVE TEXT-0422(SPRAK-IX) TO                            
066700                   MOD-TEMFSFEL                                           
066800                 ELSE                                                     
066900                   PERFORM H-OPPNA                                        
067000                 END-IF                                                   
067100               END-IF                                                     
067200               ADD +1 TO INDX                                             
067300             END-PERFORM                                                  
067400             MOVE +1 TO INDX                                              
067500           END-IF                                                         
067600         END-EVALUATE                                                     
067700       ELSE                                                               
067800         PERFORM K-FLYTTA-FELTEXT                                         
067900       END-IF                                                             
068000       IF W-ENTER NOT = NEJ                                               
068100         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O57301 + 4                    
068200         PERFORM IMS-ISRT-MSG                                             
068300       END-IF                                                             
068400     END-IF                                                               
068500     MOVE ZERO TO RETURN-CODE                                             
068600     GOBACK.                                                              
068700     EJECT                                                                
068800 A-INIT SECTION.                                                          
068900                                                                          
069000     IF MSG-DUBBLA-TRANSKODER                                             
069100         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I57301               
069200         MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                
069300         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
069400                                               2109-KDMFSFOR              
069500         MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                
069600     ELSE                                                                 
069700         MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I57301                 
069800         MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                  
069900         MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                 
070000                                             2109-KDMFSFOR                
070100     END-IF                                                               
070200     MOVE MSG-IDPFK TO MFS-IDPFK                                          
070300     MOVE MFS-IDTRANS                     TO WS-IDTRANS                   
070400                                                                          
070500     IF NOT EGEN-MID                                                      
070600         MOVE '+++++++'     TO     MOD-KVART-OPP                          
070700         MOVE '+'           TO     MOD-KDORDKL-OPP                        
070800         MOVE '++'          TO     MOD-KDFRAKT-OPP                        
070900         MOVE '++++++++++'  TO     MOD-PRARTNTO-OPP                       
071000         MOVE '7'           TO     MFS-IDPFK                              
071100     END-IF                                                               
071200     MOVE LOW-VALUE TO MSG-AREA                                           
071300     MOVE 'W4O573N1' TO MFS-IDMOD                                         
071400     MOVE '4573' TO MOD-IDTRANS                                           
071500     MOVE ZERO TO MOD-SPARADE-NYCKLAR                                     
071600                  MOD-SPARADE-NYCKLAR-E                                   
071700                                                                          
071800     IF MID-IDDISTR-IN       NOT = ALL '+'                                
071900         MOVE '7'   TO MFS-IDPFK                                          
072000     END-IF                                                               
072100                                                                          
072200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
072300     MOVE '001'             TO MSGI-KDCALL                                
072400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
072500     MOVE '4573'            TO MSGI-IDTRANS                               
072600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
072700     IF EGEN-MID                                                          
072800        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
072900        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
073000        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
073100     END-IF                                                               
073200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
073300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
073400         MOVE +1 TO SPRAK-IX                                              
073500     ELSE                                                                 
073600         MOVE +2 TO SPRAK-IX                                              
073700     END-IF                                                               
073800     MOVE MSGI-IDDISTR       TO IDDISTR-WS                                
073900     MOVE MSGI-IDDC          TO W-IDDC-B6-USER                            
074000     PERFORM IMS-GU-WDB601-USER                                           
074100                                                                          
074200     PERFORM S30-SECURIT                                                  
074300     IF SEC-KDSVAR = ' ' OR                                               
074400        SEC-KDSVAR = '1' OR                                               
074500        SEC-KDSVAR = '2' OR                                               
074600        SEC-KDSVAR = '3' OR                                               
074700        SEC-KDSVAR = '5' OR                                               
074800        SEC-KDSVAR = '6'                                                  
074900                                                                          
075000        MOVE JA       TO OPP-FAELT-OK                                     
075100                      INPUT-OK                                            
075200                      VALKOD-OK                                           
075300                      FRAKT-OK                                            
075400                      KVQPACK-OK                                          
075500                      ANTAL-OK                                            
075600                      KLASS-OK                                            
075700                      PRIS-OK                                             
075800                      AENDRA-ENTER                                        
075900        MOVE NEJ      TO W-OPPNAD                                         
076000        MOVE SPACE TO W-VALKOD                                            
076100                      W-ENTER                                             
076200                      SPAR-BEART                                          
076300        MOVE +13      TO MAX-LINE                                         
076400        MOVE ZERO     TO W-KVRAD                                          
076500                      W-PUNKT                                             
076600                      SPAR-IDARTNR                                        
076700                      SPAR-KDFARLIG                                       
076800                                                                          
076900        PERFORM AC-SPARA-INPUT                                            
077000        IF INPUT-OK = JA                                                  
077100           PERFORM AB-KOLLA-VALKOD                                        
077200        END-IF                                                            
077300        MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                            
077400                                MOD-IDKUNDNR-IN                           
077500                                MOD-IDARTNR-IN                            
077600                                MOD-KDORDKL-IN                            
077700                                MOD-KDPRODSL-IN                           
077800                                MOD-IDORDNR-IN                            
077900                                MOD-KDTPOTYP-IN                           
078000                                MOD-IDDC-IN                               
078100                                MOD-TEDDI                                 
078200                                MOD-TEMFSFEL                              
078300                                MOD-TEMFSINF                              
078400                                MOD-KVART-OPP                             
078500                                MOD-KDORDKL-OPP                           
078600                                MOD-KDFRAKT-OPP                           
078700                                MOD-PRARTNTO-OPP                          
078800        IF MID-SPARADE-NYCKLAR         = ALL '0' AND                      
078900           MID-SPARADE-NYCKLAR-E       = ALL '0'                          
079000          MOVE '7'   TO MFS-IDPFK                                         
079100        END-IF                                                            
079200                                                                          
079300     ELSE                                                                 
079400        MOVE NEJ TO INPUT-OK                                              
079500     END-IF                                                               
079600                                                                          
079700     IF DIST79-DEALER-PRICE                                               
079800       IF ENGLISH-TEXT                                                    
079900         MOVE 'DPR'       TO MOD-TEDDI                                    
080000       ELSE                                                               
080100         MOVE 'ÅFP'       TO MOD-TEDDI                                    
080200       END-IF                                                             
080300     ELSE                                                                 
080400       MOVE SPACES        TO MOD-TEDDI                                    
080500     END-IF                                                               
080600     .                                                                    
080700     EJECT                                                                
080800 AB-KOLLA-VALKOD SECTION.                                                 
080900                                                                          
081000     MOVE +1 TO INDX                                                      
081100           EVALUATE TRUE                                                  
081200           WHEN MFS-IDPFK = '7'                                           
081300               MOVE JA  TO VALKOD-OK                                      
081400           WHEN MFS-IDPFK = '8'                                           
081500               MOVE JA  TO VALKOD-OK                                      
081600           WHEN MFS-UPDATE                                                
081700               PERFORM UNTIL INDX > MID-KVRAD-SPAR OR                     
081800                 VALKOD-OK = NEJ                                          
081900                   EVALUATE TRUE                                          
082000                   WHEN MID-VALKOD(INDX) = 'D'                            
082100                       IF W-VALKOD = 'A' OR                               
082200                          W-OPPNAD = JA                                   
082300                           MOVE NEJ TO VALKOD-OK                          
082400                       ELSE                                               
082500                           MOVE JA  TO VALKOD-OK                          
082600                           MOVE 'D' TO W-VALKOD                           
082700                       END-IF                                             
082800                   WHEN MID-VALKOD(INDX) = ' '                            
082900                       IF MID-KVRAD-SPAR = +1                             
083000                          MOVE JA TO W-OPPNAD                             
083100                          PERFORM ABB-FLYTTA-RO-KEY                       
083200                          PERFORM IMS-GHU-ORDP                            
083300                          IF OPP-FAELT-OK = JA                            
083400                             PERFORM ABA-FRAKT                            
083500                             PERFORM ABC-KOLLA-ANTAL                      
083600                          END-IF                                          
083700                          MOVE JA TO VALKOD-OK                            
083800                       END-IF                                             
083900                   WHEN OTHER                                             
084000                       MOVE NEJ TO VALKOD-OK                              
084100                   END-EVALUATE                                           
084200                  ADD +1 TO INDX                                          
084300               END-PERFORM                                                
084400               IF VALKOD-OK NOT = NEJ                                     
084500                   IF W-VALKOD = ' '                                      
084600                     IF INDX > 1                                          
084700                       ADD -1 TO INDX                                     
084800                     END-IF                                               
084900                     IF MID-KVART(INDX) = ZERO AND                        
085000                        MID-KDORDKL(INDX) = ZERO AND                      
085100                        MID-KDFRAKT(INDX) = ZERO AND                      
085200                        MID-PRARTNTO(INDX) = ZERO                         
085300                       MOVE NEJ TO VALKOD-OK                              
085400                     END-IF                                               
085500                   END-IF                                                 
085600               END-IF                                                     
085700           WHEN OTHER                                                     
085800               PERFORM UNTIL INDX > MID-KVRAD-SPAR OR                     
085900                 VALKOD-OK = NEJ                                          
086000                   EVALUATE TRUE                                          
086100                   WHEN MID-VALKOD(INDX) = 'A'                            
086200                       IF W-VALKOD = ' ' AND                              
086300                          MID-IDARTNR(INDX) NOT = SPACE                   
086400                           MOVE JA  TO VALKOD-OK                          
086500                           MOVE 'A' TO W-VALKOD                           
086600                       ELSE                                               
086700                           MOVE NEJ TO VALKOD-OK                          
086800                       END-IF                                             
086900                   WHEN MID-VALKOD(INDX) NOT = ' '                        
087000                       MOVE NEJ TO VALKOD-OK                              
087100                   END-EVALUATE                                           
087200                   ADD +1 TO INDX                                         
087300               END-PERFORM                                                
087400               IF W-VALKOD = ' '                                          
087500                   MOVE JA  TO VALKOD-OK                                  
087600               END-IF                                                     
087700           END-EVALUATE                                                   
087800     .                                                                    
087900     EJECT                                                                
088000 ABA-FRAKT SECTION.                                                       
088100* I DENNA SEKTION KONTROLLERAS OM FRAKTKODEN FINNS PÅ KUNDREG.            
088200     IF KDFRAKT-OPP > 0                                                   
088300       MOVE RAD-IDDC     TO W-IDDC-WDB5                                   
088400       MOVE KDFRAKT-OPP  TO W-KDFRAKT-WDB5                                
088500       MOVE RAD-IDDISTR  TO W-IDDISTR-WDB5                                
088600       MOVE 9999999      TO W-IDKUNDNR-WDB5                               
088700        PERFORM IMS-GU-WDB501                                             
088800        IF SEGMENT-FINNS                                                  
088900           MOVE JA TO FRAKT-OK                                            
089000        ELSE                                                              
089100           MOVE NEJ TO OPP-FAELT-OK                                       
089200           MOVE NEJ TO FRAKT-OK                                           
089300        END-IF                                                            
089400     END-IF                                                               
089500     .                                                                    
089600     EJECT                                                                
089700 ABB-FLYTTA-RO-KEY SECTION.                                               
089800                                                                          
089900     INSPECT MID-IDKUNDNR(1) REPLACING LEADING SPACE BY ZERO              
090000     INSPECT MID-IDLOPNR(1)  REPLACING LEADING SPACE BY ZERO              
090100     INSPECT MID-IDARTNR(1)  REPLACING LEADING SPACE BY ZERO              
090200     INSPECT MID-IDORDNR(1)  REPLACING LEADING SPACE BY ZERO              
090300     MOVE LOW-VALUE          TO W-WDA5B1KY-MIN                            
090400                                W-SOK-KEY-MIN                             
090500     MOVE HIGH-VALUE         TO W-WDA5B1KY-MAX                            
090600                                W-SOK-KEY-MAX                             
090700     MOVE W-IDDISTR          TO W-IDDISTR-N2                              
090800                                W-IDDISTR-N1-MIN                          
090900                                W-IDDISTR-N1-MAX                          
091000                                W-IDDISTR-B1                              
091100                                W-IDDISTR-B201                            
091200                                W-IDDISTR-B201-MIN                        
091300                                W-IDDISTR-B201-MAX                        
091400     MOVE MID-IDKUNDNR(1)    TO W-IDKUNDNR-N2                             
091500                                W-IDKUNDNR-N1-MIN                         
091600                                W-IDKUNDNR-B1                             
091700                                W-IDKUNDNR-B201                           
091800                                W-IDKUNDNR-B201-MIN                       
091900                                W-IDKUNDNR-B201-MAX                       
092000     MOVE MID-IDDC(1)        TO W-IDDC-B1                                 
092100     MOVE MID-IDORDNR(1)     TO W-IDORDNR-N2                              
092200                                W-IDORDNR-B1                              
092300     MOVE MID-IDARTNR(1)     TO W-IDARTNR-N2                              
092400                                W-IDARTNR-B1                              
092500     MOVE MID-IDLOPNR(1)     TO W-IDLOPNR-N2                              
092600                                W-IDLOPNR-B1                              
092700     IF W-IDKUNDNR           >  ZERO                                      
092800        MOVE W-IDKUNDNR      TO W-IDKUNDNR-N1-MAX                         
092900     END-IF                                                               
093000     IF W-IDORDNR            >  ZERO                                      
093100        MOVE W-IDORDNR       TO W-IDORDNR-MIN                             
093200                                W-IDORDNR-MAX                             
093300     END-IF                                                               
093400     IF W-IDARTNR            >  ZERO                                      
093500        MOVE W-IDARTNR       TO W-IDARTNR-MIN                             
093600                                W-IDARTNR-MAX                             
093700     END-IF                                                               
093800     IF W-KDORDKL            >  ZERO                                      
093900        MOVE W-KDORDKL       TO W-KDORDKL-MIN                             
094000                                W-KDORDKL-MAX                             
094100     END-IF                                                               
094200     IF W-KDPRODSL           >  ZERO                                      
094300        MOVE W-KDPRODSL      TO W-KDPRODSL-MIN                            
094400                                W-KDPRODSL-MAX                            
094500     END-IF                                                               
094600     IF W-KDTPOTYP           >  ZERO                                      
094700        MOVE W-KDTPOTYP      TO W-KDTPOTYP-MIN                            
094800                                W-KDTPOTYP-MAX                            
094900     END-IF.                                                              
095000     IF W-IDDC               >  ZERO                                      
095100        MOVE W-IDDC          TO W-IDDC-MIN                                
095200                                W-IDDC-MAX                                
095300     END-IF.                                                              
095400     EJECT                                                                
095500 ABC-KOLLA-ANTAL             SECTION.                                     
095600                                                                          
095700     MOVE KVART-OPP         TO W-SPAR-KVART                               
095800     MOVE W-SPAR-KVART      TO WW-SPAR-KVART                              
095900     IF WW-SPAR-KVART NOT   > RAD-KVART                                   
096000        MOVE RAD-IDARTNR    TO W-IDARTNR-WDK601                           
096100        PERFORM IMS-GU-ARTC01                                             
096200        IF ART-KDSORT   = 'KG' OR 'L ' OR 'M ' OR                         
096300           RAD-KDKVBRYT = 0                                               
096400           PERFORM IMS-GNP-ARTC11                                         
096500           MOVE +1 TO INDX                                                
096600           MOVE MID-KVART(1)   TO W-KVART                                 
096700           IF CLAG-KVQPACK-1 > +1                                         
096800              COMPUTE W-ANTAL-1 =                                         
096900                      WW-SPAR-KVART / CLAG-KVQPACK-1                      
097000              MULTIPLY W-ANTAL-1 BY CLAG-KVQPACK-1                        
097100                                 GIVING W-ANTAL-2                         
097200              IF W-ANTAL-2 = WW-SPAR-KVART                                
097300                  MOVE JA TO ANTAL-OK                                     
097400              ELSE                                                        
097500                  MOVE NEJ TO KVQPACK-OK                                  
097600                  MOVE NEJ TO OPP-FAELT-OK                                
097700                  MOVE NEJ TO ANTAL-OK                                    
097800              END-IF                                                      
097900           END-IF                                                         
098000        END-IF                                                            
098100     ELSE                                                                 
098200        MOVE NEJ TO ANTAL-OK                                              
098300        MOVE NEJ TO OPP-FAELT-OK                                          
098400     END-IF                                                               
098500     .                                                                    
098600     EJECT                                                                
098700 AC-SPARA-INPUT SECTION.                                                  
098800                                                                          
098900     MOVE JA TO INPUT-OK                                                  
099000                                                                          
099100     IF MID-IDKUNDNR-IN = ALL '+'                                         
099200         MOVE MID-IDKUNDNR-UT TO IDKUNDNR-WS                              
099300         INSPECT IDKUNDNR-WS REPLACING LEADING SPACE BY ZERO              
099400     ELSE                                                                 
099500         MOVE MID-IDKUNDNR-IN TO IDKUNDNR-WS                              
099600         MOVE '7'   TO MFS-IDPFK                                          
099700     END-IF                                                               
099800                                                                          
099900     IF MID-IDARTNR-IN = ALL '+'                                          
100000         MOVE MID-IDARTNR-UT TO IDARTNR-WS                                
100100         INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO               
100200     ELSE                                                                 
100300         MOVE MID-IDARTNR-IN TO IDARTNR-WS                                
100400         MOVE '7'   TO MFS-IDPFK                                          
100500     END-IF                                                               
100600                                                                          
100700     IF MID-KDORDKL-IN = ALL '+'                                          
100800         MOVE MID-KDORDKL-UT TO KDORDKL-WS                                
100900         INSPECT KDORDKL-WS REPLACING LEADING SPACE BY ZERO               
101000     ELSE                                                                 
101100         MOVE MID-KDORDKL-IN TO KDORDKL-WS                                
101200         MOVE '7'   TO MFS-IDPFK                                          
101300     END-IF                                                               
101400                                                                          
101500     IF MID-KDPRODSL-IN = ALL '+'                                         
101600         MOVE MID-KDPRODSL-UT TO KDPRODSL-WS                              
101700         INSPECT KDPRODSL-WS REPLACING LEADING SPACE BY ZERO              
101800     ELSE                                                                 
101900         MOVE MID-KDPRODSL-IN TO KDPRODSL-WS                              
102000         MOVE '7'   TO MFS-IDPFK                                          
102100     END-IF                                                               
102200                                                                          
102300     IF MID-IDORDNR-IN = ALL '+'                                          
102400         MOVE MID-IDORDNR-UT TO IDORDNR-WS                                
102500         INSPECT IDORDNR-WS REPLACING LEADING SPACE BY ZERO               
102600     ELSE                                                                 
102700         MOVE MID-IDORDNR-IN TO IDORDNR-WS                                
102800         MOVE '7'   TO MFS-IDPFK                                          
102900     END-IF                                                               
103000                                                                          
103100     IF MID-KDTPOTYP-IN = ALL '+'                                         
103200         MOVE MID-KDTPOTYP-UT TO KDTPOTYP-WS                              
103300         INSPECT KDTPOTYP-WS REPLACING LEADING SPACE BY ZERO              
103400     ELSE                                                                 
103500         MOVE MID-KDTPOTYP-IN TO KDTPOTYP-WS                              
103600         MOVE '7'   TO MFS-IDPFK                                          
103700     END-IF                                                               
103800                                                                          
103900     IF MID-IDDC-IN = ALL '+'                                             
104000         IF  MID-IDDC-UT NOT = SPACE                                      
104100           MOVE MID-IDDC-UT TO IDDC-WS                                    
104200           INSPECT IDDC-WS REPLACING LEADING SPACE BY ZERO                
104300         ELSE                                                             
104400           MOVE MSGI-IDDC   TO IDDC-WS                                    
104500         END-IF                                                           
104600     ELSE                                                                 
104700         IF  MID-IDDC-IN = ZERO                                           
104800           MOVE MSGI-IDDC   TO IDDC-WS                                    
104900         ELSE                                                             
105000           MOVE MID-IDDC-IN TO IDDC-WS                                    
105100         END-IF                                                           
105200         MOVE '7'   TO MFS-IDPFK                                          
105300     END-IF                                                               
105400                                                                          
105500     IF WS-GODKAEND-BILD                                                  
105600        CONTINUE                                                          
105700     ELSE                                                                 
105800        MOVE ZERO                       TO IDKUNDNR-WS                    
105900                                           IDARTNR-WS                     
106000                                           KDORDKL-WS                     
106100                                           KDPRODSL-WS                    
106200                                           IDORDNR-WS                     
106300                                           KDTPOTYP-WS                    
106400                                           IDDC-WS                        
106500     END-IF                                                               
106600                                                                          
106700     MOVE MSGI-IDDISTR      TO MOD-IDDISTR-UT                             
106800     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
106900                                                                          
107000     MOVE IDKUNDNR-WS  TO MOD-IDKUNDNR-UT                                 
107100     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
107200                                                                          
107300     MOVE IDARTNR-WS   TO MOD-IDARTNR-UT                                  
107400     INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE              
107500                                                                          
107600     MOVE KDORDKL-WS   TO MOD-KDORDKL-UT                                  
107700     INSPECT MOD-KDORDKL-UT  REPLACING LEADING ZERO BY SPACE              
107800                                                                          
107900     MOVE KDPRODSL-WS  TO MOD-KDPRODSL-UT                                 
108000     INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO BY SPACE              
108100                                                                          
108200     MOVE IDORDNR-WS   TO MOD-IDORDNR-UT                                  
108300     INSPECT MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE              
108400                                                                          
108500     MOVE KDTPOTYP-WS  TO MOD-KDTPOTYP-UT                                 
108600     INSPECT MOD-KDTPOTYP-UT REPLACING LEADING ZERO BY SPACE              
108700                                                                          
108800     MOVE IDDC-WS      TO MOD-IDDC-UT                                     
108900                                                                          
109000     IF DCS-IDDC NOT = IDDC-WS                                            
109100        MOVE IDDC-WS TO W-IDDC-B6                                         
109200        PERFORM IMS-GU-WDB601                                             
109300     END-IF                                                               
109400     IF DCS-KDDC = SPACE OR DCS-CDC-TR                                    
109500       MOVE MSGI-IDDC TO MOD-IDDC-UT                                      
109600                         IDDC-WS                                          
109700     END-IF                                                               
109800     INSPECT MOD-IDDC-UT     REPLACING LEADING ZERO BY SPACE              
109900                                                                          
110000     IF WS-IDTRANS = '4573'                                               
110100        IF MID-PRARTNTO-OPP = ALL '+'                                     
110200           MOVE ZERO          TO SPAR-PRARTNTO                            
110300        ELSE                                                              
110400            IF MID-PRARTNTO-OPP = ZERO                                    
110500               MOVE ZERO           TO SPAR-PRARTNTO                       
110600               MOVE NEJ TO PRIS-OK                                        
110700               MOVE NEJ TO OPP-FAELT-OK                                   
110800            ELSE                                                          
110900               IF USER-DCS-NDC-NA OR USER-DCS-NDC-CN                      
111000                 MOVE NEJ TO PRIS-OK                                      
111100                 MOVE NEJ TO OPP-FAELT-OK                                 
111200               ELSE                                                       
111300                 IF MID-PRARTNTO-OPP NOT = ZERO                           
111400                    MOVE +7               TO DEC-KVHELTAL                 
111500                    MOVE +2               TO DEC-KVDECIMAL                
111600                    MOVE MID-PRARTNTO-OPP TO DEC-IDFRIDATA                
111700                    CALL WDECEDIT USING DEC-WDECAREA                      
111800                    IF DEC-KDSVAR-OK                                      
111900                       MOVE DEC-IDEDITDATA TO SPAR-PRARTNTO               
112000                       MOVE JA TO PRIS-OK                                 
112100                    ELSE                                                  
112200                       MOVE NEJ TO PRIS-OK                                
112300                       MOVE NEJ TO OPP-FAELT-OK                           
112400                    END-IF                                                
112500                 END-IF                                                   
112600               END-IF                                                     
112700            END-IF                                                        
112800        END-IF                                                            
112900                                                                          
113000        IF MID-KVART-OPP = ALL '+'                                        
113100            MOVE ZERO                 TO   KVART-OPP                      
113200        ELSE                                                              
113300            INSPECT MID-KVART-OPP REPLACING ALL SPACE BY ZERO             
113400            IF MID-KVART-OPP = ALL '0'                                    
113500               MOVE NEJ TO ANTAL-OK                                       
113600               MOVE NEJ TO OPP-FAELT-OK                                   
113700            ELSE                                                          
113800               MOVE MID-KVART-OPP     TO   KVART-OPP                      
113900            END-IF                                                        
114000        END-IF                                                            
114100                                                                          
114200        IF MID-KDORDKL-OPP = ALL '+'                                      
114300           MOVE ZERO               TO   KDORDKL-OPP                       
114400        ELSE                                                              
114500           IF MID-KDORDKL-OPP = ALL '0'                                   
114600              MOVE NEJ TO KLASS-OK                                        
114700              MOVE NEJ TO OPP-FAELT-OK                                    
114800           ELSE                                                           
114900              MOVE MID-KDORDKL-OPP     TO   KDORDKL-OPP                   
115000              IF KDORDKL-OPP > +4                                         
115100                 MOVE NEJ TO KLASS-OK                                     
115200                 MOVE NEJ TO OPP-FAELT-OK                                 
115300              END-IF                                                      
115400           END-IF                                                         
115500        END-IF                                                            
115600                                                                          
115700        IF MID-KDFRAKT-OPP = ALL '+'                                      
115800           MOVE ZERO             TO   KDFRAKT-OPP                         
115900        ELSE                                                              
116000           IF MID-KDFRAKT-OPP    = ALL '0'                                
116100              MOVE NEJ TO FRAKT-OK                                        
116200              MOVE NEJ TO OPP-FAELT-OK                                    
116300           ELSE                                                           
116400              MOVE MID-KDFRAKT-OPP     TO   KDFRAKT-OPP                   
116500           END-IF                                                         
116600        END-IF                                                            
116700     END-IF                                                               
116800                                                                          
116900     IF DCS-IDDC NOT = IDDC-WS                                            
117000        MOVE IDDC-WS    TO W-IDDC-B6                                      
117100        PERFORM IMS-GU-WDB601                                             
117200     END-IF                                                               
117300     IF MSGI-IDDISTR    NOT NUMERIC  OR                                   
117400        IDKUNDNR-WS     NOT NUMERIC  OR                                   
117500        IDARTNR-WS      NOT NUMERIC  OR                                   
117600        KDORDKL-WS      NOT NUMERIC  OR                                   
117700        KDPRODSL-WS     NOT NUMERIC  OR                                   
117800        IDORDNR-WS      NOT NUMERIC  OR                                   
117900        KDTPOTYP-WS     NOT NUMERIC  OR                                   
118000        DCS-KDDC = SPACE OR                                               
118100        DCS-DDC                                                           
118200       MOVE NEJ TO INPUT-OK                                               
118300     END-IF                                                               
118400                                                                          
118500     IF INPUT-OK = JA                                                     
118600        MOVE IDDISTR-WS   TO W-IDDISTR                                    
118700                             TEST-IDDISTR                                 
118800        MOVE IDKUNDNR-WS  TO W-IDKUNDNR                                   
118900        MOVE IDARTNR-WS   TO W-IDARTNR                                    
119000        MOVE KDORDKL-WS   TO W-KDORDKL                                    
119100        MOVE KDPRODSL-WS  TO W-KDPRODSL                                   
119200        MOVE IDORDNR-WS   TO W-IDORDNR                                    
119300        MOVE KDTPOTYP-WS  TO W-KDTPOTYP                                   
119400        MOVE IDDC-WS      TO W-IDDC                                       
119500                                                                          
119600        IF DCS-IDDC NOT = IDDC-WS                                         
119700           MOVE IDDC-WS    TO W-IDDC-B6                                   
119800           PERFORM IMS-GU-WDB601                                          
119900        END-IF                                                            
120000                                                                          
120100        IF (W-IDDISTR NOT      > 0     OR                                 
120200            W-KDORDKL          > 5     OR                                 
120300            W-KDTPOTYP         > 7)    OR                                 
120400           (W-IDDISTR          = ZERO  AND                                
120500            MID-IDDISTR-SPAR   = ZERO  AND                                
120600            MID-IDDISTR-SPAR-E = ZERO) OR                                 
120700*          (W-IDDC       NOT = ZERO    AND                                
120800*           NOT USER-DCS-NDC-NA)                                          
120900           (DCS-CDC                    AND                                
121000            USER-DCS-NDC-NA)                                              
121100          MOVE NEJ TO INPUT-OK                                            
121200        END-IF                                                            
121300     END-IF                                                               
121400     .                                                                    
121500     EJECT                                                                
121600 B-FLYTTA-GAMMAL-KEY        SECTION.                                      
121700                                                                          
121800     MOVE LOW-VALUE           TO W-SOK-KEY-MIN                            
121900                                 W-WDA5B1KY-MIN                           
122000     MOVE HIGH-VALUE          TO W-SOK-KEY-MAX                            
122100                                 W-WDA5B1KY-MAX                           
122200     MOVE W-IDDISTR           TO W-IDDISTR-N1-MIN                         
122300                                 W-IDDISTR-N1-MAX                         
122400     IF W-IDKUNDNR            >  ZERO                                     
122500        MOVE W-IDKUNDNR       TO W-IDKUNDNR-N1-MIN                        
122600                                 W-IDKUNDNR-N1-MAX                        
122700     END-IF                                                               
122800     IF W-IDDC                >  ZERO                                     
122900        MOVE W-IDDC           TO W-IDDC-MIN                               
123000                                 W-IDDC-MAX                               
123100        IF W-IDKUNDNR         >  ZERO                                     
123200           MOVE W-IDDC        TO W-IDDC-N1-MIN                            
123300                                 W-IDDC-N1-MAX                            
123400        END-IF                                                            
123500     END-IF                                                               
123600     IF W-IDARTNR             >  ZERO                                     
123700        MOVE W-IDARTNR        TO W-IDARTNR-MIN                            
123800                                 W-IDARTNR-MAX                            
123900        IF  W-IDKUNDNR        >  ZERO                                     
124000        AND W-IDDC            >  ZERO                                     
124100           MOVE W-IDARTNR     TO W-IDARTNR-N1-MIN                         
124200                                 W-IDARTNR-N1-MAX                         
124300        END-IF                                                            
124400     END-IF                                                               
124500     IF W-IDORDNR             >  ZERO                                     
124600        MOVE W-IDORDNR        TO W-IDKUNDRF-1-5                           
124700        MOVE W-IDKUNDRF       TO W-IDKUNDRF-MIN                           
124800                                 W-IDKUNDRF-MAX                           
124900        IF W-IDKUNDNR         >  ZERO AND                                 
125000           W-IDDC             >  ZERO AND                                 
125100           W-IDARTNR          >  ZERO                                     
125200           MOVE W-IDKUNDRF    TO W-IDKUNDRF-N1-MIN                        
125300                                 W-IDKUNDRF-N1-MAX                        
125400        END-IF                                                            
125500     END-IF                                                               
125600     IF W-KDORDKL             >  ZERO                                     
125700       MOVE W-KDORDKL         TO W-KDORDKL-MIN                            
125800                                 W-KDORDKL-MAX                            
125900     END-IF                                                               
126000     IF W-KDPRODSL            >  ZERO                                     
126100       MOVE W-KDPRODSL        TO W-KDPRODSL-MIN                           
126200                                 W-KDPRODSL-MAX                           
126300     END-IF                                                               
126400     IF W-KDTPOTYP            >  ZERO                                     
126500       MOVE W-KDTPOTYP        TO W-KDTPOTYP-MIN                           
126600                                 W-KDTPOTYP-MAX                           
126700     END-IF.                                                              
126800     EJECT                                                                
126900 C-FLYTTA-SPARAD-KEY          SECTION.                                    
127000* HÄR FLYTTAS DE KEY SOM LIGGER SPARAD PÅ RAD 4 PÅ BILDEN                 
127100     MOVE LOW-VALUE              TO W-WDA5B1KY-MIN                        
127200                                    W-WDA5B1MIN                           
127300                                    W-SOK-KEY-MIN                         
127400     MOVE HIGH-VALUE             TO W-WDA5B1KY-MAX                        
127500                                    W-WDA5B1MAX                           
127600                                    W-SOK-KEY-MAX                         
127700     MOVE MID-IDDISTR-SPAR       TO W-IDDISTR-N1-MIN                      
127800                                    W-IDDISTR-N1-MAX                      
127900                                    W-IDDISTR                             
128000                                    W-IDDISTR-B2-MIN                      
128100                                    W-IDDISTR-B2-MAX                      
128200     MOVE MID-IDKUNDNR-SPAR      TO W-IDKUNDNR-N1-MIN                     
128300                                    W-IDKUNDNR-B1                         
128400                                    W-IDKUNDNR-B2-MIN                     
128500                                    W-IDKUNDNR-B2-MAX                     
128600     MOVE MID-IDORDNR-SPAR       TO W-IDKUNDRF-1-5                        
128700                                    W-IDORDNR-B1                          
128800                                    W-IDORDNR-B2-MIN                      
128900                                    W-IDORDNR-B2-MAX                      
129000     MOVE MID-IDARTNR-SPAR       TO W-IDARTNR-B2-MIN                      
129100                                    W-IDARTNR-B2-MAX                      
129200     MOVE MID-IDDC-SPAR          TO W-IDDC-B2-MIN                         
129300                                    W-IDDC-B2-MAX                         
129400     MOVE MID-IDLOPNR-SPAR       TO W-IDLOPNR-B2-MIN                      
129500                                    W-IDLOPNR-B2-MAX                      
129600     IF W-IDKUNDNR               >  ZERO                                  
129700        MOVE MID-IDKUNDNR-SPAR   TO W-IDKUNDNR-N1-MAX                     
129800     END-IF                                                               
129900     IF W-IDARTNR                >  ZERO                                  
130000        MOVE MID-IDARTNR-SPAR    TO W-IDARTNR-MIN                         
130100                                    W-IDARTNR-MAX                         
130200     END-IF                                                               
130300     IF W-IDORDNR                >  ZERO                                  
130400        MOVE W-IDKUNDRF          TO W-IDKUNDRF-MIN                        
130500                                    W-IDKUNDRF-MAX                        
130600     END-IF                                                               
130700     IF W-KDORDKL                >  ZERO                                  
130800        MOVE MID-KDORDKL-SPAR    TO W-KDORDKL-MIN                         
130900                                    W-KDORDKL-MAX                         
131000     END-IF                                                               
131100     IF W-KDPRODSL               >  ZERO                                  
131200        MOVE MID-KDPRODSL-SPAR   TO W-KDPRODSL-MIN                        
131300                                    W-KDPRODSL-MAX                        
131400     END-IF                                                               
131500     IF W-KDTPOTYP               >  ZERO                                  
131600        MOVE MID-KDTPOTYP-SPAR   TO W-KDTPOTYP-MIN                        
131700                                    W-KDTPOTYP-MAX                        
131800     END-IF.                                                              
131900     IF W-IDDC                   >  ZERO                                  
132000        MOVE MID-IDDC-SPAR       TO W-IDDC-MIN                            
132100                                    W-IDDC-MAX                            
132200     END-IF.                                                              
132300     EJECT                                                                
132400 D-ANDRA SECTION.                                                         
132500                                                                          
132600     IF OPP-FAELT-OK = JA                                                 
132700        MOVE RAD-IDLOPNR    TO W-SPAR-IDLOPNR                             
132800*       MOVE RAD-KVART      TO W-SPAR-KVART                               
132900        MOVE RAD-KDORDKL    TO W-SPAR-KDORDKL                             
133000        MOVE RAD-KDFRAKT    TO W-SPAR-KDFRAKT                             
133100        MOVE RAD-KDRAPRIO   TO W-SPAR-KDRAPRIO                            
133200        MOVE RAD-PRARTNTO   TO W-SPAR-PRARTNTO                            
133300        MOVE RAD-PRARTNTO-LOC       TO W-SPAR-PRARTNTO-LOC                
133400        MOVE RAD-PRARTNTO-LOCPREL   TO W-SPAR-PRARTNTO-LOCPREL            
133500        MOVE +1 TO INDX                                                   
133600        IF KDFRAKT-OPP NOT = ALL '0'                                      
133700            IF KDFRAKT-OPP NOT = MID-KDFRAKT(INDX)                        
133800               MOVE KDFRAKT-OPP TO RAD-KDFRAKT                            
133900               MOVE JA TO W-FAELT-IFYLLT                                  
134000            END-IF                                                        
134100        END-IF                                                            
134200        IF KDORDKL-OPP NOT = ALL '0'                                      
134300            IF KDORDKL-OPP NOT = MID-KDORDKL(INDX)                        
134400               MOVE JA TO W-FAELT-IFYLLT                                  
134500               MOVE KDORDKL-OPP TO RAD-KDORDKL                            
134600               IF RAD-DARODAT NOT = ZERO                                  
134700                 PERFORM S01-SKAPA-SUC-TRANS                              
134800               END-IF                                                     
134900            END-IF                                                        
135000        END-IF                                                            
135100        IF SPAR-PRARTNTO > +0                                             
135200            MOVE JA TO W-FAELT-IFYLLT                                     
135300            IF DIST79-DEALER-PRICE OR                                     
135500               DIST79-ECOM-PRICE                                          
135600              MOVE SPAR-PRARTNTO  TO RAD-PRARTNTO-LOC                     
135700              MOVE +0             TO RAD-PRARTNTO-LOCPREL                 
135900              IF DIST79-ECOM-PRICE                                        
136000                MOVE SPAR-PRARTNTO  TO RAD-PRARTBTO-LOC                   
136100              END-IF                                                      
136200            ELSE                                                          
136300              MOVE SPAR-PRARTNTO TO RAD-PRARTNTO                          
136400            END-IF                                                        
136500        END-IF                                                            
136600        IF KVART-OPP NOT = ALL '0'                                        
136700            IF KVART-OPP NOT = RAD-KVART                                  
136800               IF W-FAELT-IFYLLT = JA                                     
136900                  PERFORM DC-ANTAL                                        
137000                  IF KDORDKL-OPP NOT = ALL '0' AND                        
137100                     RAD-DARODAT = ZERO                                   
137200                     PERFORM DE-UPPDATERA-WDK9                            
137300                  END-IF                                                  
137400                  MOVE JA TO W-DELNING                                    
137500               ELSE                                                       
137600                  PERFORM DD-ANTAL-ENDAST                                 
137700                  PERFORM S04-UPPDATERA-WDQ1                              
137800                  MOVE    SPAR-KVART TO RY9-KVART                         
137900                  PERFORM S02-SKAPA-RY9-TRANS                             
138000                  MOVE    SPAR-KVART TO SPAR-KVOI                         
138100                  PERFORM S03-SKAPA-2109-TRANS                            
138200               END-IF                                                     
138300            END-IF                                                        
138400        END-IF                                                            
138500                                                                          
138600        IF W-DELNING = NEJ AND KDORDKL-OPP NOT = ALL '0'                  
138700          AND RAD-DARODAT = ZERO                                          
138800          PERFORM DE-UPPDATERA-WDK9                                       
138900        END-IF                                                            
139000                                                                          
139100        IF OPP-FAELT-OK = JA                                              
139200            IF W-DELNING = JA                                             
139300              PERFORM IMS-GHU-ORDP                                        
139400              MOVE W-SPAR-IDLOPNR    TO RAD-IDLOPNR                       
139500              MOVE WW-SPAR-KVART     TO RAD-KVART                         
139600              MOVE W-SPAR-KDORDKL    TO RAD-KDORDKL                       
139700              MOVE W-SPAR-KDFRAKT    TO RAD-KDFRAKT                       
139800              MOVE W-SPAR-KDRAPRIO TO RAD-KDRAPRIO                        
139900              MOVE W-SPAR-PRARTNTO TO RAD-PRARTNTO                        
140000              MOVE W-SPAR-PRARTNTO-LOC TO RAD-PRARTNTO-LOC                
140100              MOVE W-SPAR-PRARTNTO-LOCPREL TO RAD-PRARTNTO-LOCPREL        
140200            END-IF                                                        
140300            MOVE JA TO W-UPDATE                                           
140400            PERFORM IMS-REPL-ORDP                                         
140500        END-IF                                                            
140600     END-IF                                                               
140700     MOVE MFS-RENSA-FAELT TO MOD-KVART-OPP                                
140800                             MOD-KDORDKL-OPP                              
140900                             MOD-KDFRAKT-OPP                              
141000                             MOD-PRARTNTO-OPP.                            
141100     EJECT                                                                
141200 DC-ANTAL SECTION.                                                        
141300* I DENNA SEKTIONEN BEHANDLAS ANTAL OM YTTERLIGARE FÄLT HAR               
141400* FÖRÄNDRATS.                                                             
141500     MOVE KVART-OPP          TO SPAR-KVART                                
141600     COMPUTE WW-SPAR-KVART = RAD-KVART - SPAR-KVART                       
141700     MOVE SPAR-KVART TO RAD-KVART                                         
141800                                                                          
141900     ADD +1 TO RAD-IDLOPNR                                                
142000     PERFORM IMS-ISRT-ORDP                                                
142100                                                                          
142200     PERFORM UNTIL SEGMENT-FINNS                                          
142300         ADD +1 TO RAD-IDLOPNR                                            
142400         PERFORM IMS-ISRT-ORDP                                            
142500     END-PERFORM                                                          
142600     MOVE RAD-PRARTNTO TO RY9-PRARTNTO                                    
142700     MOVE RAD-IDLOPNR  TO RY9-IDLOPNR                                     
142800     .                                                                    
142900     EJECT                                                                
143000 DD-ANTAL-ENDAST SECTION.                                                 
143100                                                                          
143200* I DENNA SEKTIONEN BEHANDLAS ANTAL OM ENDAST ANTAL HAR                   
143300* FÖRÄNDRATS.                                                             
143400                                                                          
143500     MOVE MID-IDARTNR(INDX)  TO W-IDARTNR-WDK601                          
143600                                W-WDK711-IDARTNR-N                        
143700     MOVE RAD-IDDC           TO W-WDK711-IDDC                             
143800     MOVE KVART-OPP          TO SPAR-KVART                                
143900     COMPUTE SPAR-KVART = RAD-KVART - SPAR-KVART                          
144000     IF RAD-DARODAT > ZERO                                                
144100       IF DCS-IDDC NOT = RAD-IDDC                                         
144200          MOVE RAD-IDDC   TO W-IDDC-B6                                    
144300          PERFORM IMS-GU-WDB601                                           
144400       END-IF                                                             
144500       IF  DCS-CDC                                                        
144600           PERFORM IMS-GHU-ARTC11                                         
144700           COMPUTE CLAG-KVRESS = CLAG-KVRESS - SPAR-KVART                 
144800           PERFORM IMS-REPL-ART                                           
144900           PERFORM DDB-EV-UPPDAT-WDK7-REFILL                              
145000       ELSE                                                               
145100           PERFORM IMS-GHU-WDK711                                         
145200           COMPUTE SLAG-KVRESS = SLAG-KVRESS - SPAR-KVART                 
145300           PERFORM IMS-REPL-WDK711                                        
145400           PERFORM DDB-EV-UPPDAT-WDK7-REFILL                              
145500       END-IF                                                             
145600     ELSE                                                                 
145700       IF RAD-KDTPOTYP = +4                                               
145800         PERFORM S06-UPPDATERA-KAMPANJ-TAB                                
145900       ELSE                                                               
146000         PERFORM DDA-UPPDATER-KVOKS                                       
146100       END-IF                                                             
146200     END-IF                                                               
146300                                                                          
146400     MOVE KVART-OPP       TO RAD-KVART                                    
146500     .                                                                    
146600     EJECT                                                                
146700 DDA-UPPDATER-KVOKS                      SECTION.                         
146800                                                                          
146900     MOVE RAD-IDARTNR TO W-IDARTNR-N                                      
147000     PERFORM IMS-GHU-ARTM-WDK901                                          
147100                                                                          
147200     IF RAD-KDORDKL = 0                                                   
147300       COMPUTE ART-KVOKS-VOR =                                            
147400       ART-KVOKS-VOR - SPAR-KVART                                         
147500     END-IF                                                               
147600                                                                          
147700     IF RAD-KDORDKL = 1                                                   
147800       COMPUTE ART-KVOKS-DAG =                                            
147900       ART-KVOKS-DAG - SPAR-KVART                                         
148000     END-IF                                                               
148100                                                                          
148200     IF RAD-KDORDKL = 2 OR 3 OR 4                                         
148300       COMPUTE ART-KVOKS-BULK =                                           
148400       ART-KVOKS-BULK - SPAR-KVART                                        
148500     END-IF                                                               
148600                                                                          
148700     PERFORM IMS-REPL-ARTM-WDK901                                         
148800     .                                                                    
148900     EJECT                                                                
149000 DDB-EV-UPPDAT-WDK7-REFILL         SECTION.                               
149100                                                                          
149200******************************************************************        
149300*                                                                         
149400*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
149500*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
149600*                                                                         
149700******************************************************************        
149800                                                                          
149900     MOVE RAD-IDDISTR         TO W-TP4TRAN-IDDISTR                        
150000                                                                          
150100     PERFORM DB2-SELECT-TP4TRAN                                           
150200                                                                          
150300     MOVE RAD-IDDISTR       TO TEST-IDDISTR                               
150400     IF  DIST35-REFILL                                                    
150500     OR  DIST35-REFILL-INOM-NDC                                           
150600     OR  DIST35-NONVCC-NONVCC-REFILL                                      
150700     OR  DIST35-NONVCC-NONVCC-TRANSFER                                    
150800     OR  DIST35-NA-TRANSFER                                               
150900     OR  DIST35-NA-NDC-RETURNS                                            
151000     OR  DIST35-PACIFIC-TRANSFER                                          
151100     OR  DIST35-REFILL-INOM-JP                                            
151200     OR  DIST35-CN-TRANSFER                                               
151300     OR  DIST35-NONVCC-VCC-REFILL                                         
151400     OR  DIST35-NONVCC-VCC-TRANSFER                                       
151500     OR  RADER-FINNS                                                      
151600                                                                          
151700       IF RADER-FINNS                                                     
151800         MOVE TP4TRAN-IDDC-REC  TO W-WDK711-IDDC                          
151900       ELSE                                                               
152000         SEARCH ALL DIST57-REFILL-DC                                      
152100            AT END                                                        
152200               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
152300                                TO FELTEXT                                
152400               CALL FELLOG                                                
152500            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR             
152600               MOVE DIST57-REFILL-TO-DC(DIST57-IX)                        
152700                                TO W-WDK711-IDDC                          
152800         END-SEARCH                                                       
152900       END-IF                                                             
153000                                                                          
153100       MOVE RAD-IDARTNR       TO W-WDK711-IDARTNR-N                       
153200       PERFORM IMS-GHU-WDK711                                             
153300       SUBTRACT SPAR-KVART    FROM SLAG-KVBEART                           
153400                                                                          
153500       PERFORM IMS-REPL-WDK711                                            
153600     ELSE                                                                 
153700       IF DIST35-NONVCC-CDC-REFILL                                        
153800                                                                          
153900          SEARCH ALL DIST57-REFILL-DC                                     
154000             AT END                                                       
154100                MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                      
154200                                 TO FELTEXT                               
154300                CALL FELLOG                                               
154400             WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR            
154500                MOVE DIST57-REFILL-TO-DC(DIST57-IX)                       
154600                                 TO W-WDK711-IDDC                         
154700          END-SEARCH                                                      
154800                                                                          
154900          MOVE RAD-IDARTNR     TO W-IDARTNR-WDK601                        
155000          PERFORM IMS-GHU-ARTC11                                          
155100          SUBTRACT SPAR-KVART  FROM CLAG-KVBEART                          
155200                                                                          
155300          PERFORM IMS-REPL-ART                                            
155400       END-IF                                                             
155500     END-IF                                                               
155600     .                                                                    
155700     EJECT                                                                
155800 DE-UPPDATERA-WDK9 SECTION.                                               
155900                                                                          
156000     MOVE RAD-IDARTNR TO W-IDARTNR-N                                      
156100     PERFORM IMS-GHU-ARTM-WDK901                                          
156200     IF W-SPAR-KDORDKL = 0                                                
156300       IF RAD-KDORDKL NOT = 0                                             
156400         COMPUTE ART-KVOKS-VOR =                                          
156500         ART-KVOKS-VOR - RAD-KVART                                        
156600         MOVE JA TO KVOKS-SW                                              
156700       END-IF                                                             
156800     END-IF                                                               
156900     IF W-SPAR-KDORDKL = 1                                                
157000       IF RAD-KDORDKL NOT = 1                                             
157100         COMPUTE ART-KVOKS-DAG =                                          
157200         ART-KVOKS-DAG - RAD-KVART                                        
157300         MOVE JA TO KVOKS-SW                                              
157400       END-IF                                                             
157500     END-IF                                                               
157600     IF W-SPAR-KDORDKL = 2 OR 3 OR 4                                      
157700       IF RAD-KDORDKL NOT = 2 AND 3 AND 4                                 
157800         COMPUTE ART-KVOKS-BULK =                                         
157900         ART-KVOKS-BULK - RAD-KVART                                       
158000         MOVE JA TO KVOKS-SW                                              
158100       END-IF                                                             
158200     END-IF                                                               
158300     IF KVOKS-ANDRAT                                                      
158400       IF RAD-KDORDKL = 0                                                 
158500         COMPUTE ART-KVOKS-VOR =                                          
158600         ART-KVOKS-VOR + RAD-KVART                                        
158700       END-IF                                                             
158800       IF RAD-KDORDKL = 1                                                 
158900         COMPUTE ART-KVOKS-DAG =                                          
159000         ART-KVOKS-DAG + RAD-KVART                                        
159100       END-IF                                                             
159200       IF RAD-KDORDKL = 2 OR 3 OR 4                                       
159300         COMPUTE ART-KVOKS-BULK =                                         
159400         ART-KVOKS-BULK + RAD-KVART                                       
159500       END-IF                                                             
159600       PERFORM IMS-REPL-ARTM-WDK901                                       
159700     END-IF                                                               
159800                                                                          
159900     .                                                                    
160000     EJECT                                                                
160100 E-DELETE SECTION.                                                        
160200                                                                          
160300     MOVE MID-IDKUNDNR(INDX) TO  IDKUNDNR-WS                              
160400     MOVE MID-IDORDNR(INDX)  TO  IDORDNR-WS                               
160500     MOVE MID-IDARTNR(INDX)  TO  IDARTNR-WS                               
160600     MOVE MID-IDLOPNR(INDX)  TO  IDLOPNR-WS                               
160700                                                                          
160800     INSPECT IDKUNDNR-WS     REPLACING LEADING SPACE BY ZERO              
160900     INSPECT IDARTNR-WS      REPLACING LEADING SPACE BY ZERO              
161000     INSPECT IDORDNR-WS      REPLACING LEADING SPACE BY ZERO              
161100     INSPECT IDLOPNR-WS      REPLACING LEADING SPACE BY ZERO              
161200                                                                          
161300     MOVE W-IDDISTR          TO  W-IDDISTR-N2  RY9S-IDDISTR               
161400     MOVE IDKUNDNR-WS        TO  W-IDKUNDNR-N2 RY9S-IDKUNDNR              
161500     MOVE SPACE              TO  W-IDKUNDRF-N2                            
161600     MOVE IDORDNR-WS         TO  W-IDORDNR-N2                             
161700     MOVE IDARTNR-WS         TO  W-IDARTNR-N2                             
161800                                 W-IDARTNR-WDK601                         
161900                                W-WDK711-IDARTNR-N                        
162000     MOVE IDLOPNR-WS         TO  W-IDLOPNR-N2                             
162100     MOVE MID-KVART(INDX)    TO SPAR-KVART                                
162200                                                                          
162300     PERFORM IMS-GHU-ORDP                                                 
162400     IF SEGMENT-FINNS                                                     
162500       MOVE RAD-IDDC         TO RY9S-IDDC                                 
162600       MOVE RAD-IDDC         TO W-WDK711-IDDC                             
162700                                                                          
162800       IF DCS-IDDC NOT = RAD-IDDC                                         
162900          MOVE RAD-IDDC   TO W-IDDC-B6                                    
163000          PERFORM IMS-GU-WDB601                                           
163100       END-IF                                                             
163200       IF RAD-DARODAT > ZERO                                              
163300         IF DCS-CDC                                                       
163400             PERFORM IMS-GHU-ARTC11                                       
163500             COMPUTE CLAG-KVRESS = CLAG-KVRESS - SPAR-KVART               
163600             PERFORM IMS-REPL-ART                                         
163700             PERFORM EB-EV-UPPDAT-WDK7-REFILL                             
163800         ELSE                                                             
163900             PERFORM IMS-GHU-WDK711                                       
164000             COMPUTE SLAG-KVRESS = SLAG-KVRESS - SPAR-KVART               
164100             PERFORM IMS-REPL-WDK711                                      
164200             PERFORM EB-EV-UPPDAT-WDK7-REFILL                             
164300         END-IF                                                           
164400       ELSE                                                               
164500         IF RAD-KDTPOTYP = +4                                             
164600           PERFORM S06-UPPDATERA-KAMPANJ-TAB                              
164700         ELSE                                                             
164800           PERFORM EA-UPPDATERA-KVOKS                                     
164900         END-IF                                                           
165000       END-IF                                                             
165100                                                                          
165200       PERFORM IMS-DLET-ORDP                                              
165300       MOVE JA               TO W-UPDATE                                  
165400       PERFORM S04-UPPDATERA-WDQ1                                         
165500       MOVE RAD-KVART TO RY9-KVART                                        
165600       PERFORM S02-SKAPA-RY9-TRANS                                        
165700       MOVE RAD-KVART TO SPAR-KVOI                                        
165800       PERFORM S03-SKAPA-2109-TRANS                                       
165900     END-IF                                                               
166000     .                                                                    
166100     EJECT                                                                
166200 EA-UPPDATERA-KVOKS                      SECTION.                         
166300                                                                          
166400     MOVE RAD-IDARTNR TO W-IDARTNR-N                                      
166500     PERFORM IMS-GHU-ARTM-WDK901                                          
166600                                                                          
166700     IF RAD-KDORDKL = 0                                                   
166800       COMPUTE ART-KVOKS-VOR =                                            
166900       ART-KVOKS-VOR - SPAR-KVART                                         
167000     END-IF                                                               
167100                                                                          
167200     IF RAD-KDORDKL = 1                                                   
167300       COMPUTE ART-KVOKS-DAG =                                            
167400       ART-KVOKS-DAG - SPAR-KVART                                         
167500     END-IF                                                               
167600                                                                          
167700     IF RAD-KDORDKL = 2 OR 3 OR 4                                         
167800       COMPUTE ART-KVOKS-BULK =                                           
167900       ART-KVOKS-BULK - SPAR-KVART                                        
168000     END-IF                                                               
168100                                                                          
168200     PERFORM IMS-REPL-ARTM-WDK901                                         
168300     .                                                                    
168400     EJECT                                                                
168500 EB-EV-UPPDAT-WDK7-REFILL         SECTION.                                
168600                                                                          
168700******************************************************************        
168800*                                                                         
168900*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
169000*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
169100*                                                                         
169200******************************************************************        
169300                                                                          
169400     MOVE W-IDDISTR         TO W-TP4TRAN-IDDISTR                          
169500                                                                          
169600     PERFORM DB2-SELECT-TP4TRAN                                           
169700                                                                          
169800     MOVE W-IDDISTR         TO TEST-IDDISTR                               
169900     IF  DIST35-REFILL                                                    
170000     OR  DIST35-REFILL-INOM-NDC                                           
170100     OR  DIST35-NONVCC-NONVCC-REFILL                                      
170200     OR  DIST35-NONVCC-NONVCC-TRANSFER                                    
170300     OR  DIST35-NA-TRANSFER                                               
170310     OR  DIST35-NA-NDC-RETURNS                                            
170320     OR  DIST35-PACIFIC-TRANSFER                                          
170330     OR  DIST35-REFILL-INOM-JP                                            
170340     OR  DIST35-CN-TRANSFER                                               
170350     OR  DIST35-NONVCC-VCC-REFILL                                         
170360     OR  DIST35-NONVCC-VCC-TRANSFER                                       
170370     OR  RADER-FINNS                                                      
170380       IF RADER-FINNS                                                     
170390         MOVE TP4TRAN-IDDC-REC  TO W-WDK711-IDDC                          
170400       ELSE                                                               
170500                                                                          
170600         SEARCH ALL DIST57-REFILL-DC                                      
170700            AT END                                                        
170800               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
170900                                TO FELTEXT                                
171000               CALL FELLOG                                                
171100            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR             
171200               MOVE DIST57-REFILL-TO-DC(DIST57-IX)                        
171300                                TO W-WDK711-IDDC                          
171400         END-SEARCH                                                       
171500       END-IF                                                             
171600                                                                          
171700       MOVE IDARTNR-WS        TO W-WDK711-IDARTNR-N                       
171800       PERFORM IMS-GHU-WDK711                                             
171900       SUBTRACT SPAR-KVART    FROM SLAG-KVBEART                           
172000                                                                          
172100       PERFORM IMS-REPL-WDK711                                            
172200     ELSE                                                                 
172300       IF DIST35-NONVCC-CDC-REFILL                                        
172400                                                                          
172500          SEARCH ALL DIST57-REFILL-DC                                     
172600             AT END                                                       
172700                MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                      
172800                                 TO FELTEXT                               
172900                CALL FELLOG                                               
173000             WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR            
173100                MOVE DIST57-REFILL-TO-DC(DIST57-IX)                       
173200                                 TO W-WDK711-IDDC                         
173300          END-SEARCH                                                      
173400                                                                          
173500          MOVE IDARTNR-WS      TO W-IDARTNR-WDK601                        
173600          PERFORM IMS-GHU-ARTC11                                          
173700          SUBTRACT SPAR-KVART  FROM CLAG-KVBEART                          
173800                                                                          
173900          PERFORM IMS-REPL-ART                                            
174000       END-IF                                                             
174100     END-IF                                                               
174200     .                                                                    
174300     EJECT                                                                
174400 G2-FLYTTA-KEY SECTION.                                                   
174500                                                                          
174600     MOVE LOW-VALUE              TO W-WDA5B1KY-MIN                        
174700                                    W-SOK-KEY-MIN                         
174800     MOVE HIGH-VALUE             TO W-WDA5B1KY-MAX                        
174900                                    W-SOK-KEY-MAX                         
175000                                                                          
175100     IF MID-IDDISTR-IN  NOT      =  ALL '+' OR                            
175200        MID-IDKUNDNR-IN NOT      =  ALL '+' OR                            
175300        MID-IDARTNR-IN  NOT      =  ALL '+' OR                            
175400        MID-KDORDKL-IN  NOT      =  ALL '+' OR                            
175500        MID-KDPRODSL-IN NOT      =  ALL '+' OR                            
175600        MID-IDORDNR-IN  NOT      =  ALL '+' OR                            
175700        MID-KDTPOTYP-IN NOT      =  ALL '+' OR                            
175800        MID-IDDC-IN NOT          =  ALL '+'                               
175900                                                                          
176000        MOVE JA                  TO W-NEWKEY                              
176100        MOVE W-IDDISTR           TO W-IDDISTR-N1-MIN                      
176200                                    W-IDDISTR-N1-MAX                      
176300        IF W-IDKUNDNR            >  ZERO                                  
176400           MOVE W-IDKUNDNR       TO W-IDKUNDNR-N1-MIN                     
176500                                    W-IDKUNDNR-N1-MAX                     
176600        END-IF                                                            
176700        IF W-IDDC                >  ZERO                                  
176800           MOVE W-IDDC           TO W-IDDC-MIN                            
176900                                    W-IDDC-MAX                            
177000           IF W-IDKUNDNR         >  ZERO                                  
177100              MOVE W-IDDC        TO W-IDDC-N1-MIN                         
177200                                    W-IDDC-N1-MAX                         
177300           END-IF                                                         
177400        END-IF                                                            
177500        IF W-IDARTNR             >  ZERO                                  
177600           MOVE W-IDARTNR        TO W-IDARTNR-MIN                         
177700                                    W-IDARTNR-MAX                         
177800           IF W-IDKUNDNR         >  ZERO AND                              
177900              W-IDDC             >  ZERO                                  
178000              MOVE W-IDARTNR     TO W-IDARTNR-N1-MIN                      
178100                                    W-IDARTNR-N1-MAX                      
178200           END-IF                                                         
178300        END-IF                                                            
178400        IF W-IDORDNR             >  ZERO                                  
178500           MOVE W-IDORDNR        TO W-IDKUNDRF-1-5                        
178600           MOVE W-IDKUNDRF       TO W-IDKUNDRF-MIN                        
178700                                    W-IDKUNDRF-MAX                        
178800           IF W-IDKUNDNR         >  ZERO AND                              
178900              W-IDDC             >  ZERO AND                              
179000              W-IDARTNR          >  ZERO                                  
179100              MOVE W-IDKUNDRF    TO W-IDKUNDRF-N1-MIN                     
179200                                    W-IDKUNDRF-N1-MAX                     
179300           END-IF                                                         
179400        END-IF                                                            
179500        IF W-KDORDKL             >  ZERO                                  
179600           MOVE W-KDORDKL        TO W-KDORDKL-MIN                         
179700                                    W-KDORDKL-MAX                         
179800        END-IF                                                            
179900        IF W-KDPRODSL            >  ZERO                                  
180000           MOVE W-KDPRODSL       TO W-KDPRODSL-MIN                        
180100                                    W-KDPRODSL-MAX                        
180200        END-IF                                                            
180300        IF W-KDTPOTYP            >  ZERO                                  
180400           MOVE W-KDTPOTYP       TO W-KDTPOTYP-MIN                        
180500                                    W-KDTPOTYP-MAX                        
180600        END-IF                                                            
180700     EJECT                                                                
180800*    VID ENTER UTAN NYA NYCKLAR                                           
180900     ELSE                                                                 
181000        MOVE NEJ                    TO W-NEWKEY                           
181100        MOVE MID-IDDISTR-SPAR-E     TO W-IDDISTR-N1-MIN                   
181200                                       W-IDDISTR-N1-MAX                   
181300                                       W-IDDISTR                          
181400        MOVE MID-IDKUNDNR-SPAR-E    TO W-IDKUNDNR-N1-MIN                  
181500        MOVE MID-IDARTNR-SPAR-E     TO W-IDARTNR-MIN                      
181600                                       W-IDARTNR-N1-MIN                   
181700        MOVE MID-KDORDKL-SPAR-E     TO W-KDORDKL-MIN                      
181800        MOVE MID-KDPRODSL-SPAR-E    TO W-KDPRODSL-MIN                     
181900        MOVE MID-KDTPOTYP-SPAR-E    TO W-KDTPOTYP-MIN                     
182000        MOVE MID-IDDC-SPAR-E        TO W-IDDC-MIN                         
182100                                                                          
182200        IF W-IDARTNR                >  ZERO                               
182300           MOVE MID-IDARTNR-SPAR-E  TO W-IDARTNR-MAX                      
182400                                       W-IDARTNR-N1-MAX                   
182500        END-IF                                                            
182600        IF W-IDORDNR                >  ZERO                               
182700           MOVE MID-IDORDNR-SPAR-E  TO W-IDKUNDRF-MIN                     
182800                                       W-IDKUNDRF-MAX                     
182900                                       W-IDKUNDRF-N1-MIN                  
183000                                       W-IDKUNDRF-N1-MAX                  
183100        END-IF                                                            
183200        IF W-KDORDKL                >  ZERO                               
183300           MOVE MID-KDORDKL-SPAR-E  TO W-KDORDKL-MAX                      
183400        END-IF                                                            
183500        IF W-KDPRODSL               >  ZERO                               
183600           MOVE MID-KDPRODSL-SPAR-E TO W-KDPRODSL-MAX                     
183700        END-IF                                                            
183800        IF W-IDDC                   >  ZERO                               
183900           MOVE MID-IDDC-SPAR-E     TO W-IDDC-MAX                         
184000        END-IF                                                            
184100     END-IF.                                                              
184200     EJECT                                                                
184300 H-OPPNA SECTION.                                                         
184400* I DENNA SEKTION FLYTTAS MARKERAD RAD TILL RAD 1, FÖRÄNDRINGS-           
184500* RADEN ÖPPNAS OCH ÖVRIGA RENSAS                                          
184600     IF DIST79-DEALER-PRICE                                               
184800       MOVE MID-IDDISTR-UT      TO  W-IDDISTR-B201-MIN                    
184900                                    W-IDDISTR-B201-MAX                    
185000       MOVE MID-IDKUNDNR (INDX) TO  W-IDKUNDNR-B201-MIN                   
185100                                    W-IDKUNDNR-B201-MAX                   
185200       IF MID-IDDC (INDX) NOT = DCS-IDDC                                  
185300          MOVE MID-IDDC (INDX)  TO W-IDDC-B6                              
185400          PERFORM IMS-GU-WDB601                                           
185500          IF SEGMENT-SAKNAS                                               
185510             MOVE ZERO TO DCS-IDFTG                                       
185520          END-IF                                                          
185530       END-IF                                                             
185540       PERFORM S11-HAMTA-KDVALISO-TILL-MOD                                
185550     END-IF                                                               
185560     MOVE MID-IDKUNDNR (INDX)   TO  IDKUNDNR-WS                           
185570                                    MOD-IDKUNDNR (1)                      
185580     MOVE MID-IDORDNR  (INDX)   TO  IDORDNR-WS                            
185590                                    MOD-IDORDNR  (1)                      
185600     MOVE MID-IDARTNR  (INDX)   TO  IDARTNR-WS                            
185700                                    MOD-IDARTNR  (1)                      
185800     MOVE MID-IDLOPNR  (INDX)   TO  IDLOPNR-WS                            
185900                                    MOD-IDLOPNR  (1)                      
186000     MOVE W-IDDISTR             TO  W-IDDISTR-N2                          
186100                                    TEST-IDDISTR                          
186200     MOVE MID-BEART    (INDX)   TO  MOD-BEART    (1)                      
186300     MOVE MID-KVART    (INDX)   TO  MOD-KVART    (1)                      
186400     MOVE MID-KDORDKL  (INDX)   TO  MOD-KDORDKL  (1)                      
186500     MOVE MID-KDFRAKT  (INDX)   TO  MOD-KDFRAKT  (1)                      
186600     MOVE MID-KDTPOTYP (INDX)   TO  MOD-KDTPOTYP (1)                      
186700     MOVE MID-IDDC     (INDX)   TO  MOD-IDDC     (1)                      
186800     MOVE MID-KDFAKTYP (INDX)   TO  MOD-KDFAKTYP (1)                      
186900     MOVE MID-TEASTRIX (INDX)   TO  MOD-TEASTRIX (1)                      
187000     IF SEC-KDSVAR = '2' OR '6'                                           
187100        CONTINUE                                                          
187200     ELSE                                                                 
187300        MOVE MID-PRARTNTO(INDX) TO  WW-SPAR-PRARTNTO                      
187400        MOVE W-PRARTNTO         TO  MOD-PRARTNTO (1)                      
187500     END-IF                                                               
187600     MOVE MFS-OEPPNA-NUM-FAELT  TO  MOD-KVART-OPP-ATTR                    
187700                                    MOD-KDFRAKT-OPP-ATTR                  
187800     IF NOT DIST19-SATS                                                   
187900        MOVE MFS-OEPPNA-NUM-FAELT TO  MOD-KDORDKL-OPP-ATTR                
188000     END-IF                                                               
188100     IF SEC-KDSVAR = ' '                                                  
188200        MOVE MFS-OEPPNA-NUM-FAELT TO  MOD-PRARTNTO-OPP-ATTR               
188300     END-IF                                                               
188400     MOVE +1                    TO MOD-KVRAD-SPAR                         
188500     MOVE +2                    TO INDX                                   
188600     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSINF                           
188700     PERFORM UNTIL INDX > MAX-LINE                                        
188800        MOVE MFS-RENSA-FAELT    TO                                        
188900                             MOD-IDKUNDNR(INDX)                           
189000                             MOD-IDARTNR(INDX)                            
189100                             MOD-KVART(INDX)                              
189200                             MOD-IDORDNR(INDX)                            
189300                             MOD-KDORDKL(INDX)                            
189400                             MOD-KDFRAKT(INDX)                            
189500                             MOD-KDFAKTYP(INDX)                           
189600                             MOD-KDFARLIG(INDX)                           
189700                             MOD-KDTPOTYP(INDX)                           
189800                             MOD-IDDC(INDX)                               
189900                             MOD-BEART(INDX)                              
190000                             MOD-PRARTNTO(INDX)                           
190100                             MOD-TEASTRIX(INDX)                           
190200                             MOD-IDLOPNR(INDX)                            
190300        ADD +1 TO INDX                                                    
190400     END-PERFORM.                                                         
190500     EJECT                                                                
190600 K-FLYTTA-FELTEXT SECTION.                                                
190700                                                                          
190800     IF WS-GODKAEND-BILD                                                  
190900        IF OPP-FAELT-OK = NEJ                                             
191000           MOVE TEXT-0409 (SPRAK-IX)  TO MOD-TEMFSFEL                     
191100           MOVE MID-IDKUNDNR(+1)      TO MOD-IDKUNDNR(+1)                 
191200           MOVE MID-BEART(+1)         TO MOD-BEART(+1)                    
191300           MOVE MID-IDARTNR (+1)      TO MOD-IDARTNR(+1)                  
191400           MOVE MID-KVART(+1)         TO MOD-KVART(+1)                    
191500           MOVE MID-IDORDNR(+1)       TO MOD-IDORDNR(+1)                  
191600           MOVE MID-KDORDKL(+1)       TO MOD-KDORDKL(+1)                  
191700           MOVE MID-KDFRAKT(+1)       TO MOD-KDFRAKT(+1)                  
191800           MOVE MID-KDTPOTYP(+1)      TO MOD-KDTPOTYP(+1)                 
191900           MOVE MID-IDDC(+1)          TO MOD-IDDC(+1)                     
192000           MOVE MID-KDFAKTYP(+1)      TO MOD-KDFAKTYP(+1)                 
192100           MOVE MID-KDFARLIG(+1)      TO MOD-KDFARLIG(+1)                 
192200           MOVE MID-IDLOPNR(+1)       TO MOD-IDLOPNR(+1)                  
192300           MOVE MID-PRARTNTO(+1)      TO WW-SPAR-PRARTNTO                 
192400           MOVE W-PRARTNTO            TO MOD-PRARTNTO(+1)                 
192500           MOVE MID-TEASTRIX(+1)      TO MOD-TEASTRIX(+1)                 
192600           MOVE +1                    TO MOD-KVRAD-SPAR                   
192700                                                                          
192800           IF MID-KVART-OPP = ALL '+'                                     
192900              MOVE MFS-RENSA-FAELT    TO MOD-KVART-OPP                    
193000           ELSE                                                           
193100              MOVE KVART-OPP          TO W9-UT-KVART-OPP                  
193200              MOVE W9-UT-KVART-OPP    TO MOD-KVART-OPP                    
193300           END-IF                                                         
193400           IF MID-KDORDKL-OPP   = ALL '+'                                 
193500              MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-OPP                     
193600           ELSE                                                           
193700              MOVE KDORDKL-OPP            TO W9-UT-KDORDKL-OPP            
193800              MOVE W9-UT-KDORDKL-OPP      TO MOD-KDORDKL-OPP              
193900           END-IF                                                         
194000           IF MID-KDFRAKT-OPP   = ALL '+'                                 
194100              MOVE MFS-RENSA-FAELT TO MOD-KDFRAKT-OPP                     
194200           ELSE                                                           
194300              MOVE KDFRAKT-OPP            TO W9-UT-KDFRAKT-OPP            
194400              MOVE W9-UT-KDFRAKT-OPP      TO MOD-KDFRAKT-OPP              
194500           END-IF                                                         
194600           IF MID-PRARTNTO-OPP   = ALL '+'                                
194700              MOVE MFS-RENSA-FAELT TO MOD-PRARTNTO-OPP                    
194800           ELSE                                                           
194900              MOVE SPAR-PRARTNTO          TO W9-UT-PRARTNTO-OPP           
195000              MOVE W9-UT-PRARTNTO-OPP     TO MOD-PRARTNTO-OPP             
195100           END-IF                                                         
195200                                                                          
195300           MOVE RAD-IDDISTR    TO MOD-IDDISTR-SPAR-E                      
195400           MOVE RAD-IDKUNDNR   TO MOD-IDKUNDNR-SPAR-E                     
195500           MOVE RAD-IDARTNR    TO MOD-IDARTNR-SPAR-E                      
195600           MOVE RAD-IDKUNDRF   TO W-IDKUNDRF                              
195700           MOVE W-IDKUNDRF-1-5 TO MOD-IDORDNR-SPAR-E                      
195800           MOVE RAD-KDTPOTYP   TO MOD-KDTPOTYP-SPAR-E                     
195900           MOVE RAD-IDDC       TO MOD-IDDC-SPAR-E                         
196000           MOVE RAD-KDORDKL    TO MOD-KDORDKL-SPAR-E                      
196100           MOVE RAD-KDPRODSL   TO MOD-KDPRODSL-SPAR-E                     
196200           IF ANTAL-OK = NEJ                                              
196300               MOVE MFS-NUM-FAELT-FEL TO MOD-KVART-OPP-ATTR               
196400           ELSE                                                           
196500               MOVE MFS-NUM-FAELT-RAETT TO MOD-KVART-OPP-ATTR             
196600           END-IF                                                         
196700           IF KLASS-OK = NEJ                                              
196800               MOVE MFS-NUM-FAELT-FEL TO MOD-KDORDKL-OPP-ATTR             
196900           ELSE                                                           
197000              IF NOT DIST19-SATS                                          
197100                 MOVE MFS-NUM-FAELT-RAETT TO MOD-KDORDKL-OPP-ATTR         
197200              END-IF                                                      
197300           END-IF                                                         
197400           IF FRAKT-OK = NEJ                                              
197500               MOVE MFS-NUM-FAELT-FEL TO MOD-KDFRAKT-OPP-ATTR             
197600           ELSE                                                           
197700               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFRAKT-OPP-ATTR           
197800           END-IF                                                         
197900           IF PRIS-OK = NEJ                                               
198000               MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-OPP-ATTR            
198100           ELSE                                                           
198200              IF SEC-KDSVAR = ' '                                         
198300                MOVE MFS-NUM-FAELT-RAETT TO MOD-PRARTNTO-OPP-ATTR         
198400              END-IF                                                      
198500           END-IF                                                         
198600           IF KVQPACK-OK = NEJ                                            
198700               MOVE MFS-NUM-FAELT-FEL TO MOD-KVART-OPP-ATTR               
198800               MOVE TEXT-0419 (SPRAK-IX) TO MOD-TEMFSFEL                  
198900           END-IF                                                         
199000        END-IF                                                            
199100        IF VALKOD-OK = NEJ                                                
199200            MOVE TEXT-0416 (SPRAK-IX) TO MOD-TEMFSFEL                     
199300            MOVE +1 TO INDX                                               
199400            PERFORM MFS-ROER-EJ                                           
199500        END-IF                                                            
199600        IF INPUT-OK = NEJ                                                 
199700            MOVE TEXT-0401 (SPRAK-IX) TO MOD-TEMFSFEL                     
199800        END-IF                                                            
199900        IF SEC-KDSVAR NOT = ' ' AND                                       
200000           SEC-KDSVAR NOT = '1' AND                                       
200100           SEC-KDSVAR NOT = '2' AND                                       
200200           SEC-KDSVAR NOT = '3' AND                                       
200300           SEC-KDSVAR NOT = '5' AND                                       
200400           SEC-KDSVAR NOT = '6'                                           
200500           MOVE TEXT-0405 (SPRAK-IX) TO MOD-TEMFSFEL                      
200600        END-IF                                                            
200700        IF AENDRA-ENTER = NEJ                                             
200800           MOVE TEXT-0407 (SPRAK-IX) TO MOD-TEMFSFEL                      
200900           MOVE MID-IDDISTR-SPAR-E   TO MOD-IDDISTR-SPAR-E                
201000           MOVE MID-IDKUNDNR-SPAR-E  TO MOD-IDKUNDNR-SPAR-E               
201100           MOVE MID-IDARTNR-SPAR-E   TO MOD-IDARTNR-SPAR-E                
201200           MOVE MID-IDORDNR-SPAR-E   TO MOD-IDORDNR-SPAR-E                
201300           MOVE MID-KDTPOTYP-SPAR-E  TO MOD-KDTPOTYP-SPAR-E               
201400           MOVE MID-IDDC-SPAR-E      TO MOD-IDDC-SPAR-E                   
201500           MOVE MID-KDORDKL-SPAR-E   TO MOD-KDORDKL-SPAR-E                
201600           MOVE MID-KDPRODSL-SPAR-E  TO MOD-KDPRODSL-SPAR-E               
201700        END-IF                                                            
201800     END-IF.                                                              
201900     EJECT                                                                
202000 M-LAES-RESTORDER  SECTION.                                               
202100                                                                          
202200     IF SEGMENT-FINNS                                                     
202300        PERFORM MD-FLYTTA-NYCKEL                                          
202400        MOVE +1 TO INDX                                                   
202500        IF DIST79-DEALER-PRICE                                            
202700          MOVE SEQB-IDDISTR      TO W-IDDISTR-B201                        
202800                                    W-IDDISTR-B201-MIN                    
202900                                    W-IDDISTR-B201-MAX                    
203000          MOVE SEQB-IDKUNDNR     TO W-IDKUNDNR-B201                       
203100                                    W-IDKUNDNR-B201-MIN                   
203200                                    W-IDKUNDNR-B201-MAX                   
203300          IF SEQB-IDDC NOT = DCS-IDDC                                     
203400             MOVE SEQB-IDDC  TO W-IDDC-B6                                 
203500             PERFORM IMS-GU-WDB601                                        
203600             IF SEGMENT-SAKNAS                                            
203700                MOVE ZERO TO DCS-IDFTG                                    
203710             END-IF                                                       
203720          END-IF                                                          
203730          PERFORM S11-HAMTA-KDVALISO-TILL-MOD                             
203740        END-IF                                                            
203750        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
203760            SEGMENT-SLUT OR                                               
203770            INDX > MAX-LINE                                               
203780                                                                          
203790            IF SEQB-IDARTNR        =   SPAR-IDARTNR                       
203800              MOVE W-IDDISTR         TO TEST-IDDISTR                      
203900              IF (SEQB-KDFAKTYP = 'G' OR 'N')                             
204000                MOVE SEQB-IDKONTO TO WS-RAD-IDKONTO                       
204100                MOVE WS-IDKONTO   TO W-IDKONTO                            
204200                MOVE SEQB-IDKST   TO W-IDKST                              
204300                MOVE W-BEART-02   TO MOD-BEART(INDX)                      
204400                IF MOD-BEART(INDX) = ZERO OR                              
204500                   MOD-BEART(INDX) = SPACE                                
204600                  MOVE SPAR-BEART        TO MOD-BEART(INDX)               
204700                ELSE                                                      
204800                  INSPECT MOD-BEART(INDX) REPLACING                       
204900                  LEADING ZERO BY SPACE                                   
205000                END-IF                                                    
205100              ELSE                                                        
205200                MOVE SPAR-BEART     TO MOD-BEART(INDX)                    
205300              END-IF                                                      
205400              MOVE SPAR-KDFARLIG     TO MOD-KDFARLIG(INDX)                
205500            ELSE                                                          
205600              MOVE SEQB-IDARTNR     TO SPAR-IDARTNR                       
205700                                        W-IDARTNR-N5                      
205800                                        W-IDARTNR-WDK601                  
205900              PERFORM MA-LAES-BENA                                        
206000              PERFORM IMS-GU-ARTC11                                       
206100              MOVE CLAG-KDFARLIG    TO MOD-KDFARLIG(INDX)                 
206200                                       SPAR-KDFARLIG                      
206300            END-IF                                                        
206400*           IF DIST79-DEALER-PRICE                                        
206500                                                                          
206600              MOVE SEQB-IDWDA501      TO W-WDA501KY                       
206700              PERFORM IMS-GU-ORDP                                         
206800                                                                          
206900*           END-IF                                                        
207000                                                                          
207100            PERFORM MB-FLYTTA-TILL-MOD                                    
207200            ADD +1 TO W-KVRAD                                             
207300            ADD +1 TO INDX                                                
207400            PERFORM IMS-GN-ORDR                                           
207500        END-PERFORM                                                       
207600                                                                          
207700        MOVE W-KVRAD        TO MOD-KVRAD-SPAR                             
207800        IF SEGMENT-FINNS                                                  
207900           PERFORM MC-FLYTTA-TILL-SPAR-MOD                                
208000        END-IF                                                            
208100                                                                          
208200        PERFORM UNTIL INDX > MAX-LINE                                     
208300           MOVE MFS-RENSA-FAELT TO                                        
208400                             MOD-VALKOD   (INDX)                          
208500                             MOD-IDKUNDNR (INDX)                          
208600                             MOD-IDARTNR  (INDX)                          
208700                             MOD-KVART    (INDX)                          
208800                             MOD-IDORDNR  (INDX)                          
208900                             MOD-KDORDKL  (INDX)                          
209000                             MOD-PRARTNTO (INDX)                          
209100                             MOD-TEASTRIX (INDX)                          
209200                             MOD-IDLOPNR  (INDX)                          
209300                             MOD-KDFRAKT  (INDX)                          
209400                             MOD-KDFAKTYP (INDX)                          
209500                             MOD-KDFARLIG (INDX)                          
209600                             MOD-KDTPOTYP (INDX)                          
209700                             MOD-IDDC     (INDX)                          
209800                             MOD-BEART    (INDX)                          
209900           ADD +1 TO INDX                                                 
210000        END-PERFORM                                                       
210100     ELSE                                                                 
210200         MOVE TEXT-0403 (SPRAK-IX) TO MOD-TEMFSFEL                        
210300         MOVE ' '                  TO MOD-KDVALISO                        
210400     END-IF.                                                              
210500     EJECT                                                                
210600 MA-LAES-BENA SECTION.                                                    
210700                                                                          
210800     MOVE W-IDDISTR          TO TEST-IDDISTR                              
210900     IF (SEQB-KDFAKTYP = 'G' OR 'N')                                      
211000       MOVE SEQB-IDKONTO TO WS-RAD-IDKONTO                                
211100       MOVE WS-IDKONTO   TO W-IDKONTO                                     
211200       MOVE SEQB-IDKST   TO W-IDKST                                       
211300       MOVE W-BEART-02   TO MOD-BEART(INDX)                               
211400       INSPECT MOD-BEART(INDX) REPLACING                                  
211500       LEADING ZERO BY SPACE                                              
211600     ELSE                                                                 
211700       IF SWEDISH-TEXT                                                    
211800         MOVE 'S  '           TO W-IDSKYLT-N5                             
211900       ELSE                                                               
212000         MOVE 'GB '           TO W-IDSKYLT-N5                             
212100       END-IF                                                             
212200       PERFORM IMS-GU-BENA-11                                             
212300       IF SEGMENT-FINNS                                                   
212400         MOVE BENA-TEXT-BEART TO W-BEART-01                               
212500         MOVE W-BEART-01-01   TO MOD-BEART(INDX)                          
212600       ELSE                                                               
212700         MOVE SPACE           TO MOD-BEART(INDX)                          
212800       END-IF                                                             
212900     END-IF                                                               
213000     MOVE MOD-BEART(INDX)    TO SPAR-BEART                                
213100     .                                                                    
213200     EJECT                                                                
213300 MB-FLYTTA-TILL-MOD SECTION.                                              
213400                                                                          
213500     MOVE SEQB-IDKUNDNR      TO MOD-IDKUNDNR(INDX)                        
213600     MOVE SEQB-IDARTNR       TO MOD-IDARTNR (INDX)                        
213700     MOVE SEQB-KVART         TO MOD-KVART   (INDX)                        
213800     MOVE SEQB-KDORDKL       TO MOD-KDORDKL (INDX)                        
213900     MOVE SEQB-KDFRAKT       TO MOD-KDFRAKT (INDX)                        
214000     MOVE SEQB-KDTPOTYP      TO MOD-KDTPOTYP(INDX)                        
214100     MOVE SEQB-IDDC          TO MOD-IDDC    (INDX)                        
214200     MOVE SEQB-KDFAKTYP      TO MOD-KDFAKTYP(INDX)                        
214300     MOVE SEQB-IDLOPNR       TO MOD-IDLOPNR (INDX)                        
214400     MOVE SEQB-IDKUNDRF      TO W-IDKUNDRF                                
214500     MOVE W-IDKUNDRF-1-5     TO MOD-IDORDNR (INDX)                        
214600                                                                          
214700     IF SEQB-FLERS           =  'J'                                       
214800        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-ATTR(INDX)              
214900     END-IF                                                               
215000     IF SEC-KDSVAR =  '2' OR '6'                                          
215100        CONTINUE                                                          
215200     ELSE                                                                 
215300       IF DIST79-DEALER-PRICE OR                                          
215500          DIST79-ECOM-PRICE                                               
215600         IF RAD-PRARTNTO-LOC > +0                                         
215700           MOVE RAD-PRARTNTO-LOC    TO MOD-PRARTNTO  (INDX)               
215800           MOVE ' '                 TO MOD-TEASTRIX  (INDX)               
215900         ELSE                                                             
216000           MOVE RAD-PRARTNTO-LOCPREL TO MOD-PRARTNTO (INDX)               
216100           MOVE '*'                  TO MOD-TEASTRIX (INDX)               
216200         END-IF                                                           
216300       ELSE                                                               
216400         IF RAD-PRAVCOST > +0                                             
216500            MOVE RAD-PRAVCOST TO MOD-PRARTNTO (INDX)                      
216600         ELSE                                                             
216700            MOVE RAD-PRARTNTO TO MOD-PRARTNTO (INDX)                      
216800         END-IF                                                           
216900         MOVE ' '             TO MOD-TEASTRIX  (INDX)                     
217000         MOVE RAD-KDVALISO           TO MOD-KDVALISO                      
217100       END-IF                                                             
217200                                                                          
217300     END-IF                                                               
217400                                                                          
217500     IF INDX = 1                                                          
217600       MOVE W-IDDISTR       TO MOD-IDDISTR-SPAR-E                         
217700       MOVE SEQB-IDKUNDNR    TO MOD-IDKUNDNR-SPAR-E                       
217800       MOVE SEQB-IDARTNR     TO MOD-IDARTNR-SPAR-E                        
217900       MOVE W-IDKUNDRF-1-5  TO MOD-IDORDNR-SPAR-E                         
218000       MOVE W-KDTPOTYP      TO MOD-KDTPOTYP-SPAR-E                        
218100       MOVE W-IDDC          TO MOD-IDDC-SPAR-E                            
218200       MOVE W-KDORDKL       TO MOD-KDORDKL-SPAR-E                         
218300       MOVE W-KDPRODSL      TO MOD-KDPRODSL-SPAR-E                        
218400     END-IF.                                                              
218500     EJECT                                                                
218600 MC-FLYTTA-TILL-SPAR-MOD SECTION.                                         
218700                                                                          
218800     MOVE SEQB-IDDISTR         TO MOD-IDDISTR-SPAR                        
218900     MOVE SEQB-IDKUNDNR        TO MOD-IDKUNDNR-SPAR                       
219000     MOVE SEQB-IDARTNR         TO MOD-IDARTNR-SPAR                        
219100     MOVE SEQB-IDLOPNR         TO MOD-IDLOPNR-SPAR                        
219200     MOVE SEQB-IDKUNDRF        TO W-IDKUNDRF                              
219300     MOVE W-IDKUNDRF-1-5       TO MOD-IDORDNR-SPAR                        
219400     IF W-KDORDKL              >  ZERO                                    
219500        MOVE W-KDORDKL         TO MOD-KDORDKL-SPAR                        
219600     END-IF                                                               
219700     IF W-KDPRODSL             >  ZERO                                    
219800        MOVE W-KDPRODSL        TO MOD-KDPRODSL-SPAR                       
219900     END-IF                                                               
220000     IF W-KDTPOTYP             >  ZERO                                    
220100        MOVE W-KDTPOTYP        TO MOD-KDTPOTYP-SPAR                       
220200     END-IF                                                               
220300     IF W-IDDC                 >  ZERO                                    
220400        MOVE W-IDDC            TO MOD-IDDC-SPAR                           
220500     END-IF                                                               
220600     MOVE TEXT-0402 (SPRAK-IX) TO MOD-TEMFSINF                            
220700     .                                                                    
220800     EJECT                                                                
220900 MD-FLYTTA-NYCKEL SECTION.                                                
221000                                                                          
221100     MOVE SEQB-IDDISTR     TO W-IDDISTR-N2                                
221200     MOVE SEQB-IDKUNDNR    TO W-IDKUNDNR-N2                               
221300     MOVE SEQB-IDKUNDRF    TO W-IDKUNDRF-N2                               
221400     MOVE SEQB-IDARTNR     TO W-IDARTNR-N2                                
221500     MOVE SEQB-IDLOPNR     TO W-IDLOPNR-N2.                               
221600     EJECT                                                                
221700 S01-SKAPA-SUC-TRANS SECTION.                                             
221800                                                                          
221900     MOVE 'SUC'            TO SUC-IDPTYP                                  
222000     MOVE RAD-IDDISTR      TO SUC-IDDISTR                                 
222100     MOVE RAD-IDKUNDNR     TO SUC-IDKUNDNR                                
222200     MOVE RAD-IDDC         TO SUC-IDDC                                    
222300     MOVE RAD-IDORDNR5     TO SUC-IDORDNR                                 
222400     MOVE RAD-IDARTNR      TO SUC-IDARTNR                                 
222500     MOVE RAD-REKSIFFR     TO SUC-REKSIFFR                                
222600     MOVE RAD-KVART        TO SUC-KVBEART                                 
222700     IF RAD-DARODAT > ZERO                                                
222800       MOVE 2                TO SUC-KDRO                                  
222900     ELSE                                                                 
223000       MOVE 1                TO SUC-KDRO                                  
223100     END-IF                                                               
223200     MOVE W-SPAR-KDORDKL   TO SUC-KDORDER                                 
223300     MOVE RAD-KDORDKL      TO SUC-KDORDER-NY                              
223400     MOVE RAD-KDVRINFO     TO SUC-KDVRINFO                                
223500                                                                          
223600     MOVE SUC-WDGZSUC      TO ZZAC-LOGGPOST                               
223700     MOVE SPACE            TO ZZAC-SORTPOST                               
223800                                                                          
223900     PERFORM S10-SKRIV-LOGG                                               
224000                                                                          
224100     .                                                                    
224200     EJECT                                                                
224300 S02-SKAPA-RY9-TRANS  SECTION.                                            
224400                                                                          
224500     MOVE 'RY9'                 TO RY9-IDPTYP                             
224600     MOVE RAD-BERADREF          TO RY9-BERADREF                           
224700     MOVE RAD-BEVOLREF          TO RY9-BEVOLREF                           
224800     MOVE RAD-FLERS             TO RY9-FLERS                              
224900     MOVE RAD-IDDISTR           TO W-IDDISTR-B201                         
225000     MOVE RAD-IDKUNDNR          TO W-IDKUNDNR-B201                        
225100     PERFORM IMS-GU-WDB201                                                
225200     MOVE GMT-FLNC              TO RY9-FLNC                               
225300     MOVE RAD-IDARTNR           TO RY9-IDARTNR                            
225400     MOVE ZERO                  TO RY9-IDDIVORD                           
225500     MOVE RAD-IDKUNDRF          TO RY9-IDKUNDRF                           
225600     MOVE RAD-IDLOPNR           TO RY9-IDLOPNR                            
225700     MOVE MSG-LTERM-NAME        TO RY9-IDUSER                             
225800     MOVE RAD-KDFAKTYP          TO RY9-KDFAKTYP                           
225900     MOVE RAD-KDKVBRYT          TO RY9-KDKVBRYT                           
226000     MOVE RAD-KDORDKL           TO RY9-KDORDKL                            
226100     MOVE RAD-KDDSP             TO RY9-KDDSP                              
226200     MOVE RAD-KDRAPRIO          TO RY9-KDRAPRIO                           
226300     MOVE RAD-KDSTARAD          TO RY9-KDSTARAD                           
226400     MOVE RAD-KDTPOTYP          TO RY9-KDTPOTYP                           
226500     MOVE RAD-IDARTNR           TO W-IDARTNR-WDK601                       
226600     PERFORM IMS-GU-ARTC11                                                
226700     MOVE CLAG-KDUART           TO RY9-KDUART                             
226800     MOVE RAD-KDVRINFO          TO RY9-KDVRINFO                           
226900     IF RAD-KDTPOTYP = 1 AND RAD-IDSYSTEM = 'VR'                          
227000       MOVE 1                   TO RY9-KDVRTPO                            
227100     END-IF                                                               
227200     IF RAD-KDTPOTYP = 1 AND RAD-IDSYSTEM NOT = 'VR'                      
227300       MOVE 2                   TO RY9-KDVRTPO                            
227400     END-IF                                                               
227500     IF RAD-KDTPOTYP NOT = 1                                              
227600       MOVE 0                   TO RY9-KDVRTPO                            
227700     END-IF                                                               
227800     MOVE RAD-PRARTNTO          TO RY9-PRARTNTO                           
227900     MOVE RAD-TIREGDAT          TO RY9-TIREGDAT                           
228000     MOVE RAD-TIRES             TO RY9-TIRES                              
228100     MOVE RAD-TITPO             TO RY9-TITPO                              
228200     MOVE RAD-DARODAT (3:6)     TO RY9-TIRODAT                            
228300     MOVE GMT-FLVR              TO RY9-FLVR                               
228400                                                                          
228500     MOVE RY9-WDGZRY9           TO ZZAC-LOGGPOST                          
228600                                                                          
228700     MOVE SPACE                 TO RY9S-WDGZRY9S                          
228800     MOVE RAD-IDDISTR           TO RY9S-IDDISTR                           
228900     MOVE RAD-IDKUNDNR          TO RY9S-IDKUNDNR                          
229000     MOVE ORQI-OHUV-IDORDER     TO RY9S-IDORDER                           
229100     MOVE RAD-IDDC              TO RY9S-IDDC                              
229200     MOVE RAD-KDFRAKT           TO RY9S-KDFRAKT                           
229300     MOVE RAD-KDORDKL           TO RY9S-KDORDKL                           
229400     MOVE 87                    TO RY9S-KDORDBEK                          
229500                                                                          
229600     MOVE RY9S-WDGZRY9S         TO ZZAC-SORTPOST                          
229700                                                                          
229800     PERFORM S10-SKRIV-LOGG                                               
229900                                                                          
230000     .                                                                    
230100     EJECT                                                                
230200 S03-SKAPA-2109-TRANS SECTION.                                            
230300                                                                          
230400*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
230410     MOVE RAD-IDARTNR       TO BYT03-IDARTNR                              
230420     IF NOT BYT03-OBJEKT                                                  
230430                                                                          
230440        IF RAD-KDOI NOT = SPACE                                           
230450          MOVE SPACE         TO 2109-MID2-W2I10902                        
230460          MOVE 1             TO 2109-MID2-KVANTART                        
230470          MOVE RAD-IDARTNR   TO 2109-MID2-IDARTNR (1)                     
230480          MOVE ORQI-OHUV-IDDC-PRIM                                        
230490                             TO 2109-MID2-IDDC(1)                         
230500          MOVE '-'           TO 2109-MID2-KDTECKEN (1)                    
230600          MOVE RAD-KDOI      TO 2109-MID2-KDOI (1)                        
230700          MOVE SPAR-KVOI     TO 2109-MID2-KVOI (1)                        
230800          MOVE RAD-CLEARGROUP                                             
230900                             TO 2109-MID2-CLEARGROUP(1)                   
231000                                                                          
231100          IF RAD-KDTPOTYP >= 1 AND <= 5 AND RAD-DARODAT = ZERO            
231200            MOVE RAD-TITPO   TO 2109-MID2-TIUPPDAT (1)                    
231300          ELSE                                                            
231400            IF RAD-KDTPOTYP >= 1 AND <= 5                                 
231500              MOVE 'DT'      TO 2109-MID2-KDOI (1)                        
231600              MOVE RAD-TITPO TO 2109-MID2-TIUPPDAT (1)                    
231700            ELSE                                                          
231800*             MOVE RAD-TIREGDAT TO 2109-MID2-TIUPPDAT (1)                 
231900              MOVE ORQI-OHUV-TIREGDAT TO 2109-MID2-TIUPPDAT (1)           
232000            END-IF                                                        
232100          END-IF                                                          
232200                                                                          
232300          COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17           
232400                                                                          
232500          PERFORM IMS-PURG-ALT-MSG-2109                                   
232600        END-IF                                                            
232700     END-IF                                                               
232800     .                                                                    
232900     EJECT                                                                
233000 S04-UPPDATERA-WDQ1 SECTION.                                              
233100                                                                          
233200***LÄS WDQ2 VIA SEKINDX                                                   
233300                                                                          
233400     MOVE RAD-IDDISTR  TO W-IDDISTR-N9                                    
233500     MOVE RAD-IDKUNDNR TO W-IDKUNDNR-N9                                   
233600     MOVE '00'         TO W-IDKUNDRF-N9(1:2)                              
233700     MOVE RAD-IDKUNDRF TO W-IDKUNDRF-N9(3:5)                              
233800     PERFORM IMS-GU-ORQI01-WDQ2C                                          
233900                                                                          
234000     MOVE SPACE TO DLI-IO-ORQM01                                          
234100     MOVE ORQI-OHUV-IDORDER         TO ORQM-OBKR-IDORDER                  
234200     MOVE RAD-IDARTNR               TO ORQM-OBKR-IDARTNR                  
234300     MOVE +1                        TO ORQM-OBKR-IDLOPNR                  
234400                                       ORQM-OBKR-IDSEKVNR                 
234500     MOVE RAD-IDDC                  TO ORQM-OBKR-IDDC                     
234600     MOVE RAD-IDDC-RO               TO ORQM-OBKR-IDDC-RO                  
234700     MOVE 87                        TO ORQM-OBKR-KDORDBEK                 
234800     MOVE IDPGM                     TO ORQM-OBKR-IDPGM                    
234900     MOVE SPACE                     TO ORQM-OBKR-BEERS                    
235000     MOVE SPACE                     TO ORQM-OBKR-IDBIL                    
235100     MOVE RAD-BEKUNDRF              TO ORQM-OBKR-BEKUNDRF                 
235200     MOVE RAD-BERADREF              TO ORQM-OBKR-BERADREF                 
235300     MOVE RAD-BEVOLREF              TO ORQM-OBKR-BEVOLREF                 
235400     MOVE RAD-IDKAMPRF              TO ORQM-OBKR-IDKAMPRF                 
235500     MOVE ZERO                      TO ORQM-OBKR-DIERS-KVOT               
235600     MOVE NEJ                       TO ORQM-OBKR-FLAKPLOC                 
235700     MOVE RAD-FLINVEST              TO ORQM-OBKR-FLINVEST                 
235800     MOVE JA                        TO ORQM-OBKR-FLOBOK                   
235900     MOVE NEJ                       TO ORQM-OBKR-FLOBTRAN                 
236000                                       ORQM-OBKR-FLOBPRT                  
236100     MOVE RAD-FLPRTILL              TO ORQM-OBKR-FLPRTILL                 
236200     MOVE JA                        TO ORQM-OBKR-FLRESTN                  
236300     MOVE NEJ                       TO ORQM-OBKR-FLSLATT                  
236400     MOVE RAD-FLERS                 TO ORQM-OBKR-FLTILLK                  
236500     MOVE ZERO                      TO ORQM-OBKR-IDARTNR-TILLK            
236600     MOVE RAD-IDDISTR               TO ORQM-OBKR-IDDISTR                  
236700     MOVE RAD-IDKUNDNR              TO ORQM-OBKR-IDKUNDNR                 
236800     MOVE '00'                      TO ORQM-OBKR-IDKUNDRF(1:2)            
236900     MOVE RAD-IDKUNDRF              TO ORQM-OBKR-IDKUNDRF(3:5)            
237000     MOVE '0000000   '              TO ORQM-OBKR-IDKUNDRF-RO              
237100     MOVE RAD-IDLEVNR               TO ORQM-OBKR-IDLEVNR                  
237200     MOVE RAD-IDLOPNR               TO ORQM-OBKR-IDLOPNR-RO               
237300     MOVE RAD-IDSYSTEM              TO ORQM-OBKR-IDSYSTEM                 
237400     MOVE RAD-KDDSP                 TO ORQM-OBKR-KDDSP                    
237500     MOVE ZERO                      TO ORQM-OBKR-KDERS                    
237600     MOVE RAD-KDKVBRYT              TO ORQM-OBKR-KDKVBRYT                 
237700     MOVE RAD-KDPRTYP               TO ORQM-OBKR-KDPRTYP                  
237800     MOVE RAD-KDTPOTYP              TO ORQM-OBKR-KDTPOTYP                 
237900     MOVE RAD-KDVRINFO              TO ORQM-OBKR-KDVRINFO                 
238000     MOVE SPAR-KVART                TO ORQM-OBKR-KVANNANT                 
238100     MOVE ZERO                      TO ORQM-OBKR-KVAVBART                 
238200     MOVE RAD-KVART                 TO ORQM-OBKR-KVBEART                  
238300                                       ORQM-OBKR-KVBEART-Q                
238400     MOVE ZERO                      TO ORQM-OBKR-KVBEART-TILLK            
238500                                       ORQM-OBKR-KVPREAVB                 
238600                                       ORQM-OBKR-KVPRERO                  
238700                                       ORQM-OBKR-KVQPACK                  
238800                                       ORQM-OBKR-KVRO                     
238900                                       ORQM-OBKR-KVSLATT                  
239000     MOVE RAD-PRARTNTO              TO ORQM-OBKR-PRARTNTO                 
239100     MOVE RAD-PRARTNTO-LOC          TO ORQM-OBKR-PRARTNTO-LOC             
239200     MOVE RAD-PRARTNTO-LOCPREL      TO ORQM-OBKR-PRARTNTO-LOCPREL         
239300     MOVE RAD-PRARTBTO-LOC          TO ORQM-OBKR-PRARTBTO-LOC             
239400     MOVE RAD-IDPRQUES              TO ORQM-OBKR-IDPRQUES                 
239500     MOVE RAD-KDVALISO              TO ORQM-OBKR-KDVALISO                 
239600     MOVE RAD-KDVAT                 TO ORQM-OBKR-KDVAT                    
239700     MOVE RAD-RERAB                 TO ORQM-OBKR-RERAB                    
239800     MOVE RAD-KDRAB                 TO ORQM-OBKR-KDRAB                    
239900     MOVE RAD-BEART-VIPS            TO ORQM-OBKR-BEART-VIPS               
240000     MOVE ZERO                      TO ORQM-OBKR-PRBPRIS                  
240100     MOVE RAD-REKSIFFR              TO ORQM-OBKR-REKSIFFR                 
240200     MOVE ZERO                      TO ORQM-OBKR-REKSIFFR-TILLK           
240300                                       ORQM-OBKR-RERF-RAD                 
240400                                       ORQM-OBKR-TIDISPIN                 
240500     MOVE ORQI-OHUV-TIREGDAT        TO ORQM-OBKR-TIORDREG                 
240600     MOVE ZERO                      TO ORQM-OBKR-TIPRIS                   
240700                                       ORQM-OBKR-TIRODAT                  
240800     ACCEPT ORQM-OBKR-TIREGDAT FROM DATE                                  
240900     ACCEPT ORQM-OBKR-TIREGTID FROM TIME                                  
241000                                                                          
241100     MOVE MSGI-TILOKDAT TO ORQM-OBKR-TIREGDAT                             
241200     MOVE MSGI-TILOKTID TO ORQM-OBKR-TIREGTID                             
241300                                                                          
241400     MOVE FUNCTION CURRENT-DATE (1:8) TO DATUM-MED-ARHUNDR                
241500     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
241600                                     ORQM-OBKR-TITIREGD-9KOMPL            
241700     MOVE RAD-TITPO                 TO ORQM-OBKR-TITPO                    
241800     IF  ORQI-OHUV-TIREGDAT > +500000                                     
241900         ADD +19000000 TO ORQI-OHUV-TIREGDAT GIVING                       
242000                                     DATUM-MED-ARHUNDR                    
242100     ELSE                                                                 
242200         ADD +20000000 TO ORQI-OHUV-TIREGDAT GIVING                       
242300                                     DATUM-MED-ARHUNDR                    
242400     END-IF                                                               
242500     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
242600                                     ORQM-OBKR-TITIORDD-9KOMPL            
242700     MOVE RAD-KDFRAKT               TO ORQM-OBKR-KDFRAKT                  
242800     MOVE ORQI-OHUV-KDORDKL         TO ORQM-OBKR-KDORDKL                  
242900                                                                          
243000     MOVE RAD-KDORDTYP-LDC          TO ORQM-OBKR-KDORDTYP-LDC             
243100     MOVE RAD-TIREPDAT              TO ORQM-OBKR-TIREPDAT                 
243200     MOVE RAD-IDKUNDRF-WIP          TO ORQM-OBKR-IDKUNDRF-WIP             
243300     MOVE ZERO                      TO ORQM-OBKR-TIDLEVDAT                
243400     MOVE RAD-PRAVCOST              TO ORQM-OBKR-PRAVCOST                 
243500                                                                          
243600     PERFORM IMS-ISRT-ORQM-WDQ1                                           
243700     IF SEGMENT-FINNS-REDAN                                               
243800       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
243900         ADD +1 TO ORQM-OBKR-IDLOPNR                                      
244000         PERFORM IMS-ISRT-ORQM-WDQ1                                       
244100       END-PERFORM                                                        
244200     END-IF                                                               
244300     .                                                                    
244400     EJECT                                                                
244500 S06-UPPDATERA-KAMPANJ-TAB               SECTION.                         
244600                                                                          
244700     MOVE RAD-IDKAMPRF          TO W-KAMP-IDKAMPRF                        
244800     MOVE RAD-IDDC              TO W-KAMP-IDDC                            
244900     MOVE RAD-IDARTNR           TO W-KART-IDARTNR                         
245000                                                                          
245100     PERFORM IMS-GHU-WDM211                                               
245200     IF SEGMENT-FINNS                                                     
245300       SUBTRACT SPAR-KVART    FROM KART-KVBEART-KUND                      
245400       SUBTRACT SPAR-KVART    FROM KART-KVBEART-TPO4                      
245500       PERFORM IMS-REPL-WDM211                                            
245600                                                                          
245700       MOVE RAD-IDKAMPRF        TO W-KAMP-IDKAMPRF                        
245800       MOVE RAD-IDDC            TO W-KAMP-IDDC                            
245900       MOVE RAD-IDARTNR         TO W-KART-IDARTNR                         
246000       MOVE RAD-IDDISTR         TO W-KMRK-IDDISTR-FOM                     
246100       MOVE RAD-IDDISTR         TO W-KMRK-IDDISTR-TOM                     
246200       MOVE RAD-IDKUNDNR        TO W-KMRK-IDKUNDNR-FOM                    
246300       MOVE RAD-IDKUNDNR        TO W-KMRK-IDKUNDNR-TOM                    
246400                                                                          
246500       PERFORM S20-FINN-INTERVALL                                         
246600                                                                          
246700       PERFORM IMS-GHU-WDM221                                             
246800       IF SEGMENT-FINNS                                                   
246900         SUBTRACT SPAR-KVART  FROM KMRK-KVBEART-KUND                      
247000         PERFORM IMS-REPL-WDM221                                          
247100       ELSE                                                               
247200         MOVE ZERO              TO W-KMRK-IDKUNDNR-FOM                    
247300         MOVE ZERO              TO W-KMRK-IDKUNDNR-TOM                    
247400         PERFORM IMS-GHU-WDM221                                           
247500         IF SEGMENT-FINNS                                                 
247600           SUBTRACT SPAR-KVART FROM KMRK-KVBEART-KUND                     
247700           PERFORM IMS-REPL-WDM221                                        
247800         END-IF                                                           
247900       END-IF                                                             
248000     END-IF                                                               
248100     .                                                                    
248200     EJECT                                                                
248300 S10-SKRIV-LOGG SECTION.                                                  
248400                                                                          
248500     ACCEPT ZZAC-TIAAMMDD  FROM DATE                                      
248600     ACCEPT ZZAC-TIKLOCK   FROM TIME                                      
248700     MOVE  +1            TO ZZAC-IDLOGLOP                                 
248800                                                                          
248900     PERFORM IMS-ISRT-LOGG                                                
249000     IF SEGMENT-FINNS-REDAN                                               
249100       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
249200         ADD +1 TO ZZAC-IDLOGLOP                                          
249300         PERFORM IMS-ISRT-LOGG                                            
249400       END-PERFORM                                                        
249500     END-IF                                                               
249600     .                                                                    
249700     EJECT                                                                
249800 S11-HAMTA-KDVALISO-TILL-MOD SECTION.                                     
249900                                                                          
250000     PERFORM IMS-GET-WDB201-UNIK                                          
250100     IF SEGMENT-FINNS                                                     
250200       CONTINUE                                                           
250300     ELSE                                                                 
250400       PERFORM IMS-GET-WDB201                                             
250500     END-IF                                                               
250600     MOVE GMT-IDPARTNR     TO W-WDB1-IDPARTNR                             
250700     MOVE GMT-IDFTG        TO W-WDB1-IDFTG                                
250800     PERFORM IMS-GU-WDB101                                                
250900     MOVE BET-KDVALISO     TO MOD-KDVALISO                                
251000     .                                                                    
251100     EJECT                                                                
251200 S20-FINN-INTERVALL SECTION.                                              
251300                                                                          
251400     PERFORM IMS-GU-WDM211                                                
251500     IF SEGMENT-FINNS                                                     
251600       PERFORM IMS-GNP-WDM221                                             
251700       PERFORM UNTIL SEGMENT-SAKNAS                                       
251800         IF  RAD-IDDISTR > KMRK-IDDISTR-TOM                               
251900         OR  RAD-IDDISTR < KMRK-IDDISTR-FOM                               
252000           CONTINUE                                                       
252100         ELSE                                                             
252200           IF  RAD-IDDISTR  = KMRK-IDDISTR-TOM                            
252300           AND RAD-IDKUNDNR > KMRK-IDKUNDNR-TOM                           
252400             CONTINUE                                                     
252500           ELSE                                                           
252600             IF  RAD-IDDISTR  = KMRK-IDDISTR-FOM                          
252700             AND RAD-IDKUNDNR < KMRK-IDKUNDNR-FOM                         
252800               CONTINUE                                                   
252900             ELSE                                                         
253000               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
253100               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
253200               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
253300               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
253400             END-IF                                                       
253500           END-IF                                                         
253600         END-IF                                                           
253700         PERFORM IMS-GNP-WDM221                                           
253800       END-PERFORM                                                        
253900     END-IF                                                               
254000     .                                                                    
254100     EJECT                                                                
254200                                                                          
254300 S30-SECURIT     SECTION.                                                 
254400                                                                          
254500     MOVE MSG-SIGNON-USERID TO SEC-IDUSER                                 
254600     MOVE '4573'            TO SEC-IDTRANS                                
254700     MOVE IDDISTR-WS        TO SEC-IDKEY                                  
254800                                                                          
254900     CALL WSECURIT USING       SEC-IDUSER                                 
255000                               SEC-IDTRANS                                
255100                               SEC-IDKEY                                  
255200                               SEC-KDSVAR.                                
255300     EJECT                                                                
255400 MFS-ROER-EJ       SECTION.                                               
255500                                                                          
255600     PERFORM UNTIL INDX > MAX-LINE                                        
255700        MOVE MFS-ROER-EJ-FAELT TO                                         
255800                          MOD-VALKOD  (INDX)                              
255900                          MOD-IDKUNDNR(INDX)                              
256000                          MOD-IDARTNR (INDX)                              
256100                          MOD-KVART   (INDX)                              
256200                          MOD-IDORDNR (INDX)                              
256300                          MOD-KDORDKL (INDX)                              
256400                          MOD-KDFRAKT (INDX)                              
256500                          MOD-PRARTNTO(INDX)                              
256600                          MOD-IDLOPNR (INDX)                              
256700                          MOD-KDFAKTYP(INDX)                              
256800                          MOD-KDFARLIG(INDX)                              
256900                          MOD-KDTPOTYP(INDX)                              
257000                          MOD-IDDC    (INDX)                              
257100                          MOD-BEART   (INDX)                              
257200        ADD +1 TO INDX                                                    
257300     END-PERFORM.                                                         
257400     EJECT                                                                
257500* IMS SEKTIONER                                                           
257600 IMS-GU-MSG SECTION.                                                      
257700                                                                          
257800     SKIP2                                                                
257900     MOVE '  QC' TO GODK-STATUSKODER                                      
258000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
258100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
258200     PERFORM IMS-STATUSKONTROLL.                                          
258300                                                                          
258400 IMS-ISRT-MSG SECTION.                                                    
258500                                                                          
258600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
258700       MOVE '0' TO MFS-KDHUVOMR                                           
258800     END-IF                                                               
258900                                                                          
259000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
259100     MOVE SPACE TO GODK-STATUSKODER                                       
259200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
259300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
259400     PERFORM IMS-STATUSKONTROLL.                                          
259500     SKIP2                                                                
259600 IMS-PURG-ALT-MSG-2109 SECTION.                                           
259700     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
259800     MOVE SPACE TO GODK-STATUSKODER                                       
259900     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
260000     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
260100     PERFORM IMS-STATUSKONTROLL                                           
260200     .                                                                    
260300     EJECT                                                                
260400 IMS-GN-ORDR SECTION.                                                     
260500                                                                          
260600     STRING 'WLORDR01(WDA5B1KY>=' W-WDA5B1KY-MIN                          
260700                    '&WDA5B1KY<=' W-WDA5B1KY-MAX                          
260800                    '&IDKUNDRF>=' W-IDKUNDRF-MIN                          
260900                    '&IDKUNDRF<=' W-IDKUNDRF-MAX                          
261000                    '&IDDC    >=' W-IDDC-MIN-X                            
261100                    '&IDDC    <=' W-IDDC-MAX-X                            
261200                    '&IDARTNR >=' W-IDARTNR-MIN-X                         
261300                    '&IDARTNR <=' W-IDARTNR-MAX-X                         
261400                    '&KDORDKL >=' W-KDORDKL-MIN-X                         
261500                    '&KDORDKL <=' W-KDORDKL-MAX-X                         
261600                    '&KDPRODSL>=' W-KDPRODSL-MIN-X                        
261700                    '&KDPRODSL<=' W-KDPRODSL-MAX-X                        
261800                    '&KDTPOTYP>=' W-KDTPOTYP-MIN-X                        
261900                    '&KDTPOTYP<=' W-KDTPOTYP-MAX-X ')'                    
262000            DELIMITED BY SIZE INTO SSA1                                   
262100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
262200     CALL CBLTDLI USING GN ORDR-PCB DLI-IO-WDA5B SSA1                     
262300     MOVE ORDR-STATUS-CODE TO STATUS-WS                                   
262400     PERFORM IMS-STATUSKONTROLL.                                          
262500     EJECT                                                                
262600 IMS-GU-ORDR SECTION.                                                     
262700                                                                          
262800     STRING 'WLORDR01(WDA5B1KY =' W-WDA5B1KY ')'                          
262900            DELIMITED BY SIZE INTO SSA1                                   
263000     MOVE '  GE' TO GODK-STATUSKODER                                      
263100     CALL CBLTDLI USING GU ORDR-PCB DLI-IO-WDA5B SSA1                     
263200     MOVE ORDR-STATUS-CODE TO STATUS-WS                                   
263300     PERFORM IMS-STATUSKONTROLL.                                          
263400                                                                          
263500 IMS-GU-ORDR-PF8 SECTION.                                                 
263600                                                                          
263700     STRING 'WLORDR01(WDA5B1KY>=' W-WDA5B1MIN                             
263800                    '&WDA5B1KY<=' W-WDA5B1MAX ')'                         
263900            DELIMITED BY SIZE INTO SSA1                                   
264000     MOVE '  GE' TO GODK-STATUSKODER                                      
264100     CALL CBLTDLI USING GU ORDR-PCB DLI-IO-WDA5B SSA1                     
264200     MOVE ORDR-STATUS-CODE TO STATUS-WS                                   
264300     PERFORM IMS-STATUSKONTROLL.                                          
264400                                                                          
264500 IMS-GHU-ORDP  SECTION.                                                   
264600                                                                          
264700     STRING 'WLORDP01(WDA501KY =' W-WDA501KY ')'                          
264800            DELIMITED BY SIZE INTO SSA1                                   
264900     MOVE '  GE' TO GODK-STATUSKODER                                      
265000     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-WDA5 SSA1                     
265100     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
265200     PERFORM IMS-STATUSKONTROLL.                                          
265300     EJECT                                                                
265400                                                                          
265500 IMS-GU-ORDP  SECTION.                                                    
265600                                                                          
265700     STRING 'WLORDP01(WDA501KY =' W-WDA501KY ')'                          
265800            DELIMITED BY SIZE INTO SSA1                                   
265900     MOVE '  GE' TO GODK-STATUSKODER                                      
266000     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-WDA5 SSA1                      
266100     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
266200     PERFORM IMS-STATUSKONTROLL.                                          
266300     EJECT                                                                
266400 IMS-DLET-ORDP SECTION.                                                   
266500                                                                          
266600     MOVE '  ' TO GODK-STATUSKODER                                        
266700     CALL CBLTDLI USING DLET ORDP-PCB DLI-IO-WDA5                         
266800     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
266900     PERFORM IMS-STATUSKONTROLL.                                          
267000                                                                          
267100 IMS-REPL-ORDP SECTION.                                                   
267200                                                                          
267300     MOVE '  ' TO GODK-STATUSKODER                                        
267400     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-WDA5                         
267500     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
267600     PERFORM IMS-STATUSKONTROLL.                                          
267700                                                                          
267800 IMS-ISRT-ORDP SECTION.                                                   
267900                                                                          
268000     MOVE 'WLORDP01' TO SSA1                                              
268100     MOVE '  II' TO GODK-STATUSKODER                                      
268200     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-WDA5 SSA1                    
268300     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
268400     PERFORM IMS-STATUSKONTROLL.                                          
268500     EJECT                                                                
268600 IMS-GU-ARTC01 SECTION.                                                   
268700                                                                          
268800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-WDK6-X ')'                    
268900            DELIMITED BY SIZE INTO SSA1                                   
269000     MOVE '  ' TO GODK-STATUSKODER                                        
269100     CALL CBLTDLI USING GU ART-PCB DLI-IO-WDK601 SSA1                     
269200     MOVE ART-STATUS-CODE TO STATUS-WS                                    
269300     PERFORM IMS-STATUSKONTROLL.                                          
269400                                                                          
269500                                                                          
269600 IMS-GU-ARTC11 SECTION.                                                   
269700                                                                          
269800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-WDK6-X ')'                    
269900            DELIMITED BY SIZE INTO SSA1                                   
270000     MOVE   'WLARTC11'          TO SSA2                                   
270100     MOVE '  ' TO GODK-STATUSKODER                                        
270200     CALL CBLTDLI USING GU ART-PCB DLI-IO-WDK611 SSA1 SSA2                
270300     MOVE ART-STATUS-CODE TO STATUS-WS                                    
270400     PERFORM IMS-STATUSKONTROLL.                                          
270500                                                                          
270600                                                                          
270700 IMS-GNP-ARTC11 SECTION.                                                  
270800                                                                          
270900     MOVE    'WLARTC11'       TO SSA1                                     
271000     MOVE    '  '             TO GODK-STATUSKODER                         
271100     CALL     CBLTDLI USING GNP ART-PCB DLI-IO-WDK611 SSA1                
271200     MOVE     ART-STATUS-CODE TO STATUS-WS                                
271300     PERFORM  IMS-STATUSKONTROLL.                                         
271400                                                                          
271500 IMS-GHU-ARTC11   SECTION.                                                
271600                                                                          
271700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-WDK6-X ')'                    
271800            DELIMITED BY SIZE INTO SSA1                                   
271900     MOVE   'WLARTC11'          TO SSA2                                   
272000     MOVE '  ' TO GODK-STATUSKODER                                        
272100     CALL CBLTDLI USING GHU ART-PCB DLI-IO-WDK611 SSA1 SSA2               
272200     MOVE ART-STATUS-CODE TO STATUS-WS                                    
272300     PERFORM IMS-STATUSKONTROLL.                                          
272400                                                                          
272500 IMS-REPL-ART  SECTION.                                                   
272600                                                                          
272700     MOVE '  ' TO GODK-STATUSKODER                                        
272800     CALL CBLTDLI USING REPL ART-PCB DLI-IO-WDK611                        
272900     MOVE ART-STATUS-CODE TO STATUS-WS                                    
273000     PERFORM IMS-STATUSKONTROLL.                                          
273100     SKIP2                                                                
273200 IMS-GHU-WDK711 SECTION.                                                  
273300                                                                          
273400     STRING 'WLARTS01(IDARTNR  =' W-WDK711-IDARTNR-X ')'                  
273500          DELIMITED BY SIZE INTO SSA1                                     
273600     STRING 'WLARTS11(IDDC     =' W-WDK711-IDDC-X ')'                     
273700          DELIMITED BY SIZE INTO SSA2                                     
273800     MOVE '  GE' TO GODK-STATUSKODER                                      
273900     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-WDK711 SSA1 SSA2              
274000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
274100     PERFORM IMS-STATUSKONTROLL                                           
274200     .                                                                    
274300     EJECT                                                                
274400                                                                          
274500 IMS-REPL-WDK711 SECTION.                                                 
274600                                                                          
274700     MOVE '  ' TO GODK-STATUSKODER                                        
274800     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-WDK711                       
274900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
275000     PERFORM IMS-STATUSKONTROLL                                           
275100     .                                                                    
275200     EJECT                                                                
275300 IMS-GHU-ARTM-WDK901 SECTION.                                             
275400                                                                          
275500     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
275600          DELIMITED BY SIZE INTO SSA1                                     
275700     MOVE '  ' TO GODK-STATUSKODER                                        
275800     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-ARTM01 SSA1                   
275900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
276000     PERFORM IMS-STATUSKONTROLL                                           
276100     .                                                                    
276200     SKIP3                                                                
276300 IMS-REPL-ARTM-WDK901 SECTION.                                            
276400                                                                          
276500     MOVE '  ' TO GODK-STATUSKODER                                        
276600     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-ARTM01                       
276700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
276800     PERFORM IMS-STATUSKONTROLL                                           
276900     .                                                                    
277000     EJECT                                                                
277100 IMS-ISRT-LOGG SECTION.                                                   
277200                                                                          
277300     MOVE 'WLZZAC01' TO SSA1                                              
277400     MOVE '  II' TO GODK-STATUSKODER                                      
277500     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-ZZAC01 SSA1                  
277600     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
277700     PERFORM IMS-STATUSKONTROLL.                                          
277800     EJECT                                                                
277900 IMS-GU-WDB501 SECTION.                                                   
278000                                                                          
278100     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-X ')'                        
278200          DELIMITED BY SIZE INTO SSA1                                     
278300     MOVE '  GE' TO GODK-STATUSKODER                                      
278400     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-WDB501 SSA1                    
278500     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
278600     PERFORM IMS-STATUSKONTROLL                                           
278700     .                                                                    
278800     EJECT                                                                
278900 IMS-GU-WDB201 SECTION.                                                   
279000                                                                          
279100     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
279200            DELIMITED BY SIZE INTO SSA1                                   
279300     MOVE '  ' TO GODK-STATUSKODER                                        
279400     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
279500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
279600     PERFORM IMS-STATUSKONTROLL                                           
279700     .                                                                    
279800     SKIP2                                                                
279900 IMS-GET-WDB201-UNIK SECTION.                                             
280000                                                                          
280100     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
280200            DELIMITED BY SIZE INTO SSA1                                   
280300     MOVE '  GE' TO GODK-STATUSKODER                                      
280400     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
280500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
280600     PERFORM IMS-STATUSKONTROLL                                           
280700     .                                                                    
280800     EJECT                                                                
280900 IMS-GET-WDB201 SECTION.                                                  
281000                                                                          
281100     STRING 'WDB201  (IDGMT   >=' W-WDB201KY-MIN-X                        
281200                    '&IDGMT   <=' W-WDB201KY-MAX-X ')'                    
281300            DELIMITED BY SIZE INTO SSA1                                   
281400     MOVE '  ' TO GODK-STATUSKODER                                        
281500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
281600     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
281700     PERFORM IMS-STATUSKONTROLL                                           
281800     .                                                                    
281900     EJECT                                                                
282000 IMS-GU-WDB101 SECTION.                                                   
282100                                                                          
282200     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
282300          DELIMITED BY SIZE INTO SSA1                                     
282400     MOVE '  ' TO GODK-STATUSKODER                                        
282500     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
282600     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
282700     PERFORM IMS-STATUSKONTROLL                                           
282800     .                                                                    
282900     EJECT                                                                
283000 IMS-GU-BENA-11 SECTION.                                                  
283100                                                                          
283200     STRING 'WLBENA01(WDD3BSEQ =' W-BENA01-X ')'                          
283300             DELIMITED BY SIZE INTO SSA1                                  
283400     STRING 'WLBENA11(IDSKYLT  =' W-BENA11-X ')'                          
283500             DELIMITED BY SIZE INTO SSA2                                  
283600     MOVE '  GE' TO GODK-STATUSKODER                                      
283700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1 SSA2               
283800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
283900     PERFORM IMS-STATUSKONTROLL.                                          
284000                                                                          
284100     EJECT                                                                
284200 IMS-ISRT-ORQM-WDQ1 SECTION.                                              
284300                                                                          
284400     MOVE 'WLORQM01 ' TO SSA1                                             
284500     MOVE '  II' TO GODK-STATUSKODER                                      
284600     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-ORQM01 SSA1                  
284700     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
284800     PERFORM IMS-STATUSKONTROLL                                           
284900     .                                                                    
285000     EJECT                                                                
285100 IMS-GU-ORQI01-WDQ2C SECTION.                                             
285200                                                                          
285300     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
285400          DELIMITED BY SIZE INTO SSA1                                     
285500     MOVE '  ' TO GODK-STATUSKODER                                        
285600     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-ORQI01 SSA1                    
285700     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
285800     PERFORM IMS-STATUSKONTROLL                                           
285900     .                                                                    
286000     EJECT                                                                
286100 IMS-GU-WDM211 SECTION.                                                   
286200                                                                          
286300     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
286400          DELIMITED BY SIZE INTO SSA1                                     
286500     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
286600          DELIMITED BY SIZE INTO SSA2                                     
286700     MOVE '  GE'              TO GODK-STATUSKODER                         
286800     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
286900     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
287000     PERFORM IMS-STATUSKONTROLL                                           
287100     .                                                                    
287200                                                                          
287300 IMS-GNP-WDM221 SECTION.                                                  
287400                                                                          
287500     MOVE 'WDM221 '           TO SSA1                                     
287600     MOVE '    GE'            TO GODK-STATUSKODER                         
287700     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
287800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
287900     PERFORM IMS-STATUSKONTROLL                                           
288000     .                                                                    
288100                                                                          
288200 IMS-GHU-WDM211 SECTION.                                                  
288300                                                                          
288400     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
288500          DELIMITED BY SIZE INTO SSA1                                     
288600     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
288700          DELIMITED BY SIZE INTO SSA2                                     
288800     MOVE '  GE'              TO GODK-STATUSKODER                         
288900     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
289000     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
289100     PERFORM IMS-STATUSKONTROLL                                           
289200     .                                                                    
289300                                                                          
289400 IMS-REPL-WDM211 SECTION.                                                 
289500                                                                          
289600     MOVE '  '             TO GODK-STATUSKODER                            
289700     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
289800     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
289900     PERFORM IMS-STATUSKONTROLL                                           
290000     .                                                                    
290100     EJECT                                                                
290200 IMS-GHU-WDM221 SECTION.                                                  
290300                                                                          
290400     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
290500          DELIMITED BY SIZE INTO SSA1                                     
290600     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
290700          DELIMITED BY SIZE INTO SSA2                                     
290800     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
290900          DELIMITED BY SIZE INTO SSA3                                     
291000     MOVE '  GE' TO GODK-STATUSKODER                                      
291100     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
291200     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
291300     PERFORM IMS-STATUSKONTROLL                                           
291400     .                                                                    
291500                                                                          
291600 IMS-REPL-WDM221 SECTION.                                                 
291700                                                                          
291800     MOVE '  '             TO GODK-STATUSKODER                            
291900     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
292000     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
292100     PERFORM IMS-STATUSKONTROLL                                           
292200     .                                                                    
292300     EJECT                                                                
292400 DB2-SELECT-TP4TRAN     SECTION.                                          
292500     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
292600                                                                          
292700     MOVE 000100 TO GODK-SQLCODEKODER                                     
292800                                                                          
292900     EXEC SQL                                                             
293000           SELECT  DISTINCT                                               
293100                   IDDC_REC                                               
293200                                                                          
293300           INTO   :TP4TRAN-IDDC-REC                                       
293400                                                                          
293500           FROM    TP4TRAN                                                
293600                                                                          
293700           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
293800     END-EXEC                                                             
293900                                                                          
294000     MOVE SQLCODE TO SQLCODE-WS                                           
294100     PERFORM DB2-STATUSKONTROLL                                           
294200     .                                                                    
294300     EJECT                                                                
294400 IMS-GU-WDB601    SECTION.                                                
294500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
294600          DELIMITED BY SIZE INTO SSA1                                     
294700     MOVE '  GE' TO GODK-STATUSKODER                                      
294800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
294900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
295000     PERFORM IMS-STATUSKONTROLL                                           
295100     IF SEGMENT-SAKNAS                                                    
295200         MOVE SPACE TO DCS-KDDC                                           
295300     END-IF                                                               
295400     .                                                                    
295500                                                                          
295600 IMS-GU-WDB601-USER    SECTION.                                           
295700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-USER-X ')'                    
295800          DELIMITED BY SIZE INTO SSA1                                     
295900     MOVE '  GE' TO GODK-STATUSKODER                                      
296000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-USER SSA1            
296100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
296200     PERFORM IMS-STATUSKONTROLL                                           
296300     IF SEGMENT-SAKNAS                                                    
296400         MOVE SPACE TO USER-DCS-KDDC                                      
296500     END-IF                                                               
296600     .                                                                    
296700                                                                          
296800 IMS-STATUSKONTROLL SECTION.                                              
296900                                                                          
297000     SET STATUS-IX TO 1                                                   
297100     SEARCH GODK-STATUS AT END                                            
297200       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
297300       DELIMITED BY SIZE INTO FELTEXT                                     
297400       CALL FELLOG                                                        
297500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
297600     END-SEARCH.                                                          
297700     EJECT                                                                
297800 DB2-STATUSKONTROLL  SECTION.                                             
297900                                                                          
298000     SET SQLCODE-IX TO 1                                                  
298100     SEARCH GODK-SQLCODE                                                  
298200       AT END                                                             
298300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
298400          DELIMITED BY SIZE INTO FELTEXT                                  
298500          CALL ABEND USING RKOD-ABEND-DB2                                 
298600       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
298700     END-SEARCH                                                           
298800     .                                                                    
298900                                                                          
