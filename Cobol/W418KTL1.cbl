000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W418KTL1.                                                
000400*AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500*DATE-WRITTEN.   94/06/02.                                                
000600*                                                                         
000610*                                                                         
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W41810 W4070400                
001000*                                                                         
001100*        KONTROLL AV LEVERANSANMÄRKNINGSRADER  POSTTYP STA/BAT            
001200*                    STA=FRÅN STANSBILD W40704                            
001300*                    BAT=FRÅN BATCHEN                                     
001400*                    REF=FRÅN BATCHEN REFILL NA                           
001500*                    FAK=FRÅN BATCHEN LAGERSTYRNING/KVALITE NA            
001600*                                     KOD 94, 54 OCH 55                   
001700*                                                                         
001800*        PROGRAMMET LÄSER DL1 OCH DB2 (TP8GRET)                           
001900*                                                                         
002000*        E-TRACKER : 2756833  051125                                      
002100*        E-TRACKER : 1658417  060301                                      
002200*        E-TRACKER : 3465041  060608                                      
002300*        E-TRACKER : 3739101  060717                                      
002400*        E-TRACKER : 3817243  060905                                      
002500*        E-TRACKER : 850114   070404                                      
002600*        E-TRACKER : 4996031  070507                                      
002700*        E-TRACKER : 5165899  070912                                      
002800*        E-TRACKER : 6086704  071218  STOPPA RENAULT ARTIKLAR             
002900*        E-TRACKER : 6093203  071220  STOPPA RENAULT KOD 72               
003000*        E-TRACKER : 6123497  080104  upd. datum ovanstående 2            
003100*        E-TRACKER : 4823800  080114  NEW LDC ROLL-OUT                    
003200*        E-TRACKER : 6292160  080206  TILLÄGG RENAULT LEV. 3636           
003300*        E-TRACKER : 6330259  080212  Stoppa kod 72 för klass 0.          
003400*        E-TRACKER : 6501332  080306  Tag bort 1378 igen 72/VOR.          
003500*        E-TRACKER : 6483431  080307  Stoppa kod 72 för klass 0.          
003600*        E-TRACKER : 6544939  080409  DNI Portugal distr. 1958.           
003700*        E-TRACKER : 6900027  080602  Value control for returns.          
003800*        E-TRACKER : 7289235  080828  Value control for returns.          
003900*        E-TRACKER : 7299238  080902  Stoppa kod 72 VOR för 1378.         
004000*        E-TRACKER : 7434702  081007  Del. value control for ret.         
004100*        E-TRACKER : 8258308  091108  Stop code 72 for all markets        
004200*        E-TRACKER : 8735608  091130  Minimum value control               
004300*        E-TRACKER : 9271104  100310  Fix för att kunna reklamera         
004400*                                     på gamla fakturor,före WDL5         
004500*                                     ändringen av rensningsregler        
004600*                                     den 8/3 2010.                       
004700*        E-TRACKER: 10133838  110323  Rättning pga 0C7 abend.             
004800*        E-TRACKER: 10162027  130206  OPEN DIRDEL FOR DE(KONRAD)          
004900*        E-TRACKER: 10137113  131118  LEAD TIME CLAIM, RETURN             
005000*        STORY 2373879 / REPLACE DIST35- 88 LEVELS OF IN,KR,              
005100*        AE,CN CDC RETURNS TO DIST35-CDC-RETURNS-NON-VCC                  
005200*        STORY 2883215 / REPLACE TP8FRET WITH TP8GRET                     
005300                                                                          
005400                                                                          
005500 ENVIRONMENT DIVISION.                                                    
005600                                                                          
005700                                                                          
005800 DATA DIVISION.                                                           
005900                                                                          
006000 WORKING-STORAGE SECTION.                                                 
006100                                                                          
006200*    -COPY WY2000W1                                                       
006300     SKIP3                                                                
006400 77  FELTEXT-STR                 PIC X(75)   VALUE SPACE.                 
006500 77  IDPGM                       PIC X(08)   VALUE 'W418KTL1'.            
006600 77  JA                          PIC X       VALUE 'J'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800 77  INDX                        PIC S9(3)   VALUE +0    COMP-3.          
006900 77  T-INDX                      PIC S9(5)   VALUE +0    COMP-3.          
007000 77  WS-IDDC-API                 PIC X(2)    VALUE SPACE.                 
007100 77  WS-IDFAKT                   PIC S9(7)   VALUE +0   COMP-3.           
007200 77  WS-IDORDNR7                 PIC  9(7)   VALUE ZERO.                  
007300 77  WS-IDARTNR                  PIC S9(9)         COMP  SYNC.            
007400 77  WS-KVLEVART                 PIC S9(7)   VALUE +0    COMP-3.          
007500 77  WS-PRARTBTO                 PIC S9(7)V9(2) VALUE +0 COMP-3.          
007600 77  WS-PRARTBTO-LOC             PIC S9(7)V9(2) VALUE +0 COMP-3.          
007700 77  WS-KVDAGAR                  PIC  9(3)   VALUE ZERO.                  
007800 77  DUMMY-IDARTNR               PIC S9(9)   VALUE +100  COMP-3.          
007900 77  LINK-MINI-SUMMA             PIC S9(7)V9(2) VALUE +0 COMP-3.          
008000 77  WS-DAGENS-DATUM-AADDD       PIC 9(5).                                
008100 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008200 77  WS-ETT-AR                   PIC 9(3)    VALUE 365.                   
008300 77  WS-TVA-AR                   PIC 9(5)    VALUE 730.                   
008400 77  WS-DDD                      PIC 9(3)    VALUE ZERO.                  
008500 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
008600 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
008700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
008800 77  CURRENT-IMS-SECTION         PIC X(20)   VALUE SPACE.                 
008900 77  DB2-SECTION                 PIC X(30)  VALUE SPACE.                  
009000                                                                          
009100 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
009200 77  WS-KDVALISO-MC              PIC X(3)   VALUE SPACE.                  
009300                                                                          
009400 01  SW-REGEL-VALD               PIC X       VALUE 'N'.                   
009500 01  CURR-IDDISTR-FOM            PIC 9(5)    VALUE ZERO.                  
009600 01  CURR-IDDISTR-TOM            PIC 9(5)    VALUE 9999.                  
009700 01  CURR-IDKUNDNR-FOM           PIC 9(7)    VALUE ZERO.                  
009800 01  CURR-IDKUNDNR-TOM           PIC 9(7)    VALUE 999999.                
009900 01  CURR-KDANMORS               PIC X(2)    VALUE SPACE.                 
010000 01  WS-KDANMORS                 PIC X(2)    VALUE SPACE.                 
010100                                                                          
010200 01  WS-IDKUNDNR                 PIC S9(7) VALUE +0 COMP-3.               
010300                                                                          
010400 77  WS-KVDAGAR-LEV-DEF          PIC 9(3)    VALUE ZERO.                  
010500 77  WS-KVDAGAR-LTRP-DEF         PIC 9(3)    VALUE ZERO.                  
010600                                                                          
010700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
010800     88  ALLT-OK                             VALUE 'J'.                   
010900                                                                          
011000 77  WDB201-SW                   PIC X       VALUE 'J'.                   
011100     88  WDB201-FINNS                        VALUE 'J'.                   
011200     88  WDB201-SAKNAS                       VALUE 'N'.                   
011300                                                                          
011400 77  WLGMTB01-SW                 PIC X       VALUE 'J'.                   
011500     88  WLGMTB01-FINNS                      VALUE 'J'.                   
011600     88  WLGMTB01-SAKNAS                     VALUE 'N'.                   
011700                                                                          
011800 77  WLARTC01-SW                 PIC X       VALUE 'J'.                   
011900     88  WLARTC01-FINNS                      VALUE 'J'.                   
012000     88  WLARTC01-SAKNAS                     VALUE 'N'.                   
012100                                                                          
012200 77  WLARTC11-SW                 PIC X       VALUE 'J'.                   
012300     88  WLARTC11-FINNS                      VALUE 'J'.                   
012400     88  WLARTC11-SAKNAS                     VALUE 'N'.                   
012500                                                                          
012600 77  WLKREE01-SW                 PIC X       VALUE 'J'.                   
012700     88  WLKREE01-FINNS                      VALUE 'J'.                   
012800     88  WLKREE01-SAKNAS                     VALUE 'N'.                   
012900                                                                          
013000 77  WDL5-SW                     PIC X       VALUE 'J'.                   
013100     88  WDL5-FINNS                          VALUE 'J'.                   
013200     88  WDL5-SAKNAS                         VALUE 'N'.                   
013300                                                                          
013400 77  WL410901-SW                 PIC X       VALUE 'J'.                   
013500     88  WL410901-FINNS                      VALUE 'J'.                   
013600     88  WL410901-SAKNAS                     VALUE 'N'.                   
013700                                                                          
013800     EJECT                                                                
013900*      --- VALID IDDC CODES                                               
014000*                                                                         
014100*01    -COPY WWDC99                                                       
014200*01    -COPY WWDCKONS                                                     
014300*01    -COPY WWLEV06                                                      
014400       EJECT                                                              
014500                                                                          
014600 01  W-TILEVANM.                                                          
014700     03 W-TILEVANM-NUM          PIC 9(7).                                 
014800     03 W-TILEVANM-ALFA REDEFINES W-TILEVANM-NUM.                         
014900        05 W-TILEVANM-AAAA      PIC 9(04).                                
015000        05 W-TILEVANM-DDD       PIC 9(03).                                
015100                                                                          
015200 01  W-TIFAKT.                                                            
015300     03 W-TIFAKT-NUM            PIC 9(7).                                 
015400     03 W-TIFAKT-ALFA REDEFINES W-TIFAKT-NUM.                             
015500        05 W-TIFAKT-AAAA        PIC 9(04).                                
015600        05 W-TIFAKT-DDD         PIC 9(03).                                
015700                                                                          
015800 01  WS-FAK-DATUM-AADDD          PIC 9(5)      VALUE ZERO.                
015900 01  FILLER REDEFINES WS-FAK-DATUM-AADDD.                                 
016000     03  WS-FAK-DATUM-AA         PIC 9(2).                                
016100     03  WS-FAK-DATUM-DDD        PIC 9(3).                                
016200*                                                                         
016300 01  W-KVAAR                    PIC S9(02).                               
016400 01  W-TIDSGRANS                PIC S9(03).                               
016500                                                                          
016600 01  WS-TAL                      PIC 9(2)V9(3) VALUE ZERO.                
016700 01  FILLER REDEFINES WS-TAL.                                             
016800     03  WS-HELTAL               PIC 9(2).                                
016900     03  WS-RESTEN               PIC 9(3).                                
017000*                                                                         
017100 01  WS-JFRDAT                   PIC 9(5).                                
017200 01  WS-JFRDAT-TEST.                                                      
017300     03  WS-JFRDAT-AA            PIC 9(2).                                
017400     03  WS-JFRDAT-DDD           PIC 9(3).                                
017500*                                                                         
017600     SKIP2                                                                
017700 01  DYNAMISKA-SUBPROGRAM.                                                
017800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018200     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
018300     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
018400     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
018500     03  W335COST                PIC X(8)    VALUE 'W335COST'.            
018600     03  W335CURR                PIC X(8)    VALUE 'W335CURR'.            
018700     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
018800                                                                          
018900 01  FILLER                      PIC X(70)   VALUE ALL 'A'.               
019000                                                                          
019100 01  NYCKLAR-TILL-DLI.                                                    
019200    03 W-IDLEVANM-X.                                                      
019300       05 W-IDDISTR-WDA2         PIC S9(5)    COMP-3.                     
019400       05 W-IDKUNDNR-WDA2        PIC S9(7)    COMP-3.                     
019500       05 W-IDRAPPNR-WDA2        PIC  X(7).                               
019600                                                                          
019700    03 W-IDARTNR-X.                                                       
019800       05 W-IDARTNR              PIC S9(9)    COMP-3.                     
019900                                                                          
020000    03 W-IDGMT-X.                                                         
020100       05 W-IDDISTR-WDB2         PIC S9(5)    COMP-3.                     
020200       05 W-IDKUNDNR-WDB2        PIC S9(7)    COMP-3.                     
020300                                                                          
020400    03 W-IDGMT-MIN-X.                                                     
020500       05 W-IDDISTR-WDB2-MIN     PIC S9(5)    COMP-3.                     
020600       05 W-IDKUNDNR-WDB2-MIN    PIC S9(7)    COMP-3.                     
020700                                                                          
020800    03 W-IDGMT-MAX-X.                                                     
020900       05 W-IDDISTR-WDB2-MAX     PIC S9(5)    COMP-3.                     
021000       05 W-IDKUNDNR-WDB2-MAX    PIC S9(7)    COMP-3.                     
021100                                                                          
021200    03 W-WDB101KY-X.                                                      
021300       05 W-IDPARTNR             PIC X(9).                                
021400       05 W-IDFTG                PIC 9(2).                                
021500                                                                          
021600    03 W-WDB301KY-X.                                                      
021700       05 W-IDDC-WDB3            PIC  X(2).                               
021800       05 W-IDDISTR-WDB3         PIC S9(5)    COMP-3.                     
021900       05 W-IDKUNDNR-WDB3        PIC S9(7)    COMP-3.                     
022000                                                                          
022100    03 W-WDB301KY-DEF-X.                                                  
022200       05 W-IDDC-WDB3-DEF        PIC  X(2).                               
022300       05 W-IDDISTR-WDB3-DEF     PIC S9(5)    COMP-3.                     
022400       05 W-IDKUNDNR-WDB3-DEF  PIC S9(7)    VALUE +9999999 COMP-3.        
022500                                                                          
022700    03   FILLER                  PIC X(70)   VALUE ALL 'B'.               
022800                                                                          
023000    03 W-IDFAKT-X.                                                        
023100      05 W-IDFAKT                PIC S9(7)    COMP-3.                     
023600                                                                          
024400    03 W-IDGMTREF-X.                                                      
024500      05 W-IDDISTR-WDL5          PIC S9(5)    COMP-3.                     
024600      05 W-IDKUNDNR-WDL5         PIC S9(7)    COMP-3.                     
024700      05 W-IDKUNDRF              PIC  X(10).                              
024800      05 W-IDORDNR5-FILLER REDEFINES W-IDKUNDRF.                          
024900        07 W-IDORDNR5            PIC  9(5).                               
025000        07 FILLER                PIC  X(5).                               
025100      05 W-IDORDNR7-FILLER REDEFINES W-IDKUNDRF.                          
025200        07 W-IDORDNR7            PIC  9(7).                               
025300        07 FILLER                PIC  X(3).                               
025400                                                                          
025500    03 W-WDL511KY-X.                                                      
025600      05 W-IDPRODNR              PIC S9(7)   VALUE ZERO  COMP-3.          
025700      05 W-IDKOLLI-X.                                                     
025800        07 W-IDKOLLI             PIC S9(5)   VALUE ZERO  COMP-3.          
025900                                                                          
026300    03   W-WDGXKEY-ROT-X.                                                 
026400      05     FILLER              PIC X(04)    VALUE '4109'.               
026500      05     W-IDFTG-4109        PIC 9(02)    VALUE ZERO.                 
026600      05     FILLER              PIC X(24)    VALUE LOW-VALUE.            
026700                                                                          
026800    03   W-WDGXKEY-MIN-X.                                                 
026900      05 W-IDARTNR-4109-MIN      PIC S9(9)   VALUE ZERO COMP-3.           
027000      05 W-KDANMORS-4109-MIN     PIC X(2)    VALUE SPACE.                 
027100      05 FILLER                  PIC X(8)    VALUE LOW-VALUE.             
027200                                                                          
027300    03   W-WDGXKEY-MAX-X.                                                 
027400      05 W-IDARTNR-4109-MAX      PIC S9(9)   VALUE ZERO COMP-3.           
027500      05 W-KDANMORS-4109-MAX     PIC X(2)    VALUE SPACE.                 
027600      05 FILLER                  PIC X(8)    VALUE HIGH-VALUE.            
027700                                                                          
027800    03   W-WDGXKEY-4127-X.                                                
027900        05   W-IDHTYP-4127       PIC  X(4)   VALUE '4127'.                
028000        05   FILLER              PIC  X(26)  VALUE LOW-VALUE.             
028100                                                                          
028200    03   W-KEY4128-X.                                                     
028300        05   W-4128-IDDISTR-FKY  PIC S9(5)        COMP-3.                 
028400        05   W-4128-IDDISTR-TKY  PIC S9(5)        COMP-3.                 
028500        05   W-4128-IDKUNDNR-FKY PIC S9(7)        COMP-3.                 
028600        05   W-4128-IDKUNDNR-TKY PIC S9(7)        COMP-3.                 
028700        05   W-4128-KDANMORS-KY  PIC  X(2).                               
028800                                                                          
028900    03   W-4128-IDDISTR-FOM-X.                                            
029000        05   W-4128-IDDISTR-FOM  PIC S9(5)        COMP-3.                 
029100                                                                          
029200    03   W-4128-IDDISTR-TOM-X.                                            
029300        05   W-4128-IDDISTR-TOM  PIC S9(5)        COMP-3.                 
029400                                                                          
029500    03   W-4128-IDKUNDNR-FOM-X.                                           
029600        05   W-4128-IDKUNDNR-FOM PIC S9(7)        COMP-3.                 
029700                                                                          
029800    03   W-4128-IDKUNDNR-TOM-X.                                           
029900        05   W-4128-IDKUNDNR-TOM PIC S9(7)        COMP-3.                 
030000                                                                          
030100     EJECT                                                                
030200                                                                          
030300 01  FILLER                      PIC X(70)   VALUE ALL 'C'.               
030400                                                                          
030500*    --- STATUS-KOD FRÅN IMS                                              
030600 01  STATUS-WS                    PIC XX.                                 
030700     88  SEGMENT-FINNS                       VALUE '  '.                  
030800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
031000                                                                          
031100 01  GODK-STATUSKODER.                                                    
031200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031300                                                                          
031400                                                                          
031500 01  SSA1                        PIC X(160).                              
031600 01  SSA2                        PIC X(64).                               
031700     EJECT                                                                
031800                                                                          
031900*    --- PARAMETRAR TILL SUBPGM WDECEDIT                                  
032000 01  FILLER                      PIC X(8)    VALUE 'WDECAREA'.            
032100*    -COPY WDECAREA                                                       
032200     EJECT                                                                
032300                                                                          
032400*    --- PARAMETRAR TILL SUBPGM WDATKONV                                  
032500 01  FILLER                      PIC X(8)    VALUE 'WDATAREA'.            
032600*    -COPY WDATAREA                                                       
032700     EJECT                                                                
032800*    ---  PARAMETER FÖR W335PRIS                                          
032900 01  FILLER                      PIC X(8)    VALUE 'W335PRIS'.            
033000*01  -COPY W335PRIS.                                                      
033100     EJECT                                                                
033200*    ---  PARAMETRAR TILL W335COST                                        
033300 01 FILLER                       PIC X(8)    VALUE 'W335COST'.            
033400*   -COPY W335COST                                                        
033500     EJECT                                                                
033600*    ---  PARAMETRAR TILL W335CURR                                        
033700 01 FILLER                       PIC X(8)    VALUE 'W335CURR'.            
033800*   -COPY W335CURR                                                        
033900     EJECT                                                                
034000*    ---  PARAMETRAR TILL W510CURR                                        
034100 01 FILLER                       PIC X(8)    VALUE 'W510CURR'.            
034200*   -COPY W510CURR                                                        
034300     EJECT                                                                
034400                                                                          
034500 01  FILLER                    PIC X(16) VALUE 'TEST-IDDISTRIKT '.        
034600                                                                          
034700 01  TEST-IDDISTR                PIC 9(5)  COMP-3.                        
034800*01  FILLER   -COPY WWDIST07   -RED TEST-IDDISTR.                         
034900     EJECT                                                                
035000*01  FILLER  -COPY WWDIS104   -RED TEST-IDDISTR.                          
035100     EJECT                                                                
035200*01  FILLER   -COPY WWDIST18   -RED TEST-IDDISTR.                         
035300     EJECT                                                                
035400*01  FILLER   -COPY WWDIST35   -RED TEST-IDDISTR.                         
035500     EJECT                                                                
035600*01  FILLER   -COPY WWDIST03   -RED TEST-IDDISTR.                         
035700     EJECT                                                                
035800*01  FILLER   -COPY WWDIST79   -RED TEST-IDDISTR.                         
035900     EJECT                                                                
036000                                                                          
036100*01  -COPY WWIDFTG                                                        
036200     EJECT                                                                
036300                                                                          
036400 01  FILLER                      PIC X(16)  VALUE 'BYTES-ARTIKEL'.        
036500 01  TEST-IDARTNR                PIC 9(9)   COMP-3.                       
036600                                                                          
036700*01  FILLER  -COPY WWBYT09   -RED TEST-IDARTNR.                           
036800                                                                          
036900     EJECT                                                                
037000*    --- IMS FUNKTIONSKODER                                               
037100*01  -COPY W0003                                                          
037200     EJECT                                                                
037300                                                                          
037400*    ---  DLI INPUT-OUTPUT AREA                                           
037500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
037600                                                                          
037700 01  DLI-IO-AREA-WDA201.                                                  
037800     03  WLKREE01.                                                        
037900*       05 -COPY WDA201.                                                  
038000     EJECT                                                                
038100                                                                          
038200 01  FILLER                      PIC X(70)   VALUE ALL 'D'.               
038300                                                                          
038400 01  DLI-IO-AREA-WDB201.                                                  
038500     03  WDB201.                                                          
038600*       05 -COPY WDB201.                                                  
038700     EJECT                                                                
038800                                                                          
038900 01  FILLER                      PIC X(70)   VALUE ALL 'E'.               
039000                                                                          
039100 01  DLI-IO-AREA-WDB301.                                                  
039200     03  WLGMTB01.                                                        
039300*       05 -COPY WDB301.                                                  
039400     EJECT                                                                
039500                                                                          
039600 01  FILLER                      PIC X(70)   VALUE ALL 'G'.               
039700                                                                          
039800 01  DLI-IO-AREA-WDK601.                                                  
039900     03  WLARTC01.                                                        
040000*       05 -COPY WDK601.                                                  
040100     EJECT                                                                
040200                                                                          
040300 01  FILLER                      PIC X(70)   VALUE 'L501-AREA'.           
040400 01  DLI-IO-AREA-WDL501.                                                  
040500*    03 -COPY WDL501.                                                     
040600     EJECT                                                                
040700 01  FILLER                      PIC X(70)   VALUE 'L511-AREA'.           
040800 01  DLI-IO-AREA-WDL511.                                                  
040900*    03 -COPY WDL511.                                                     
041000     EJECT                                                                
041100 01  FILLER                      PIC X(70)   VALUE 'L521-AREA'.           
041200 01  DLI-IO-AREA-WDL521.                                                  
041300*    03 -COPY WDL521.                                                     
041400     EJECT                                                                
041500                                                                          
041600 01  FILLER                      PIC X(70)   VALUE ALL 'M'.               
041700                                                                          
041800 01  DLI-IO-AREA-WDK611.                                                  
041900     03  WLARTC11.                                                        
042000*       05 -COPY WDK611.                                                  
042100     EJECT                                                                
042200                                                                          
042300 01  FILLER                      PIC X(70)   VALUE ALL 'N'.               
042400                                                                          
042500 01  DLI-IO-AREA-4109.                                                    
042600     03  WL410901.                                                        
042700*       05 -COPY WDGX4110.                                                
042800     EJECT                                                                
042900                                                                          
043000     03  FILLER                  PIC X(70)   VALUE ALL 'O'.               
043100                                                                          
043200*    ---  LÄNKAREA TILL W418OKOD                                          
043300                                                                          
043400*    03 -COPY W418OKOD           -PRE OKOD-.                              
043500     EJECT                                                                
043600                                                                          
043700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB101'.             
043800 01  DLI-IO-WDB101.                                                       
043900*    03  -COPY WDB101.                                                    
044000     EJECT                                                                
044100 01  FILLER                 PIC X(16) VALUE 'DLI-IO-WDGX01'.              
044200 01  DLI-IO-WDGX01.                                                       
044300*    03  -COPY WDGX01                                                     
044400                                                                          
044500     EJECT                                                                
044600 01  FILLER                 PIC X(16) VALUE 'DLI-IO-WDGX4128'.            
044700 01  DLI-IO-WDGX4128.                                                     
044800*    03  -COPY WDGX4128                                                   
044900                                                                          
045000     EJECT                                                                
045100*------------------------------------------------------- DB2-AREA         
045200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
045300                                                                          
045400 01  DB2-WS.                                                              
045500     03  SQLCODE-WS              PIC S9(4)  VALUE ZERO.                   
045600         88  ROW-FOUND                      VALUE ZERO.                   
045700         88  ROWS-MISSING                   VALUE +100.                   
045800         88  VALUE-MISSING                  VALUE -305.                   
045900         88  MORE-THAN-ONE-ROW              VALUE -811.                   
046000     03  GOOD-SQLCODECODES.                                               
046100         05  GOOD-SQLCODE OCCURS 5                                        
046200             INDEXED BY SQLCODE-IX PIC S9(4).                             
046300                                                                          
046400                                                                          
046500*   -COPY TP8GRET               -PRE TP8GRET-                             
046600                                                                          
046700     EXEC SQL INCLUDE TP8GRET END-EXEC.                                   
046800                                                                          
046900                                                                          
047000 LINKAGE SECTION.                                                         
047100                                                                          
047200*   -COPY W418KTL1               -PRE LINK-.                              
047300                                                                          
047400     EJECT                                                                
047500                                                                          
047600*01  -COPY W0008                 -PRE KREE-.                              
047700         05  FILLER              PIC X.                                   
047800     EJECT                                                                
047900                                                                          
048000*01  -COPY W0008                 -PRE WDL5-.                              
048100         05  FILLER              PIC X.                                   
048200     EJECT                                                                
048300*01  -COPY W0008                 -PRE WDB2-.                              
048400         05  FILLER              PIC X.                                   
048500     EJECT                                                                
048600                                                                          
048700*01  -COPY W0008                 -PRE ARTC-.                              
048800         05  FILLER              PIC X.                                   
048900     EJECT                                                                
049000                                                                          
049100*01  -COPY W0008                 -PRE 4109-.                              
049200         05  FILLER              PIC X.                                   
049300     EJECT                                                                
049400                                                                          
049500*01  -COPY W0008                 -PRE PARTC-.                             
049600         05  FILLER              PIC X.                                   
049700     EJECT                                                                
049800                                                                          
049900*01  -COPY W0008                 -PRE PWDK7-.                             
050000         05  FILLER              PIC X.                                   
050100     EJECT                                                                
050200                                                                          
050300*01  -COPY W0008                 -PRE PGMTA-.                             
050400         05  FILLER              PIC X.                                   
050500     EJECT                                                                
050600                                                                          
050700*01  -COPY W0008                 -PRE BETA-.                              
050800         05  FILLER              PIC X.                                   
050900     EJECT                                                                
051000                                                                          
051100*01  -COPY W0008                 -PRE PRIA-.                              
051200         05  FILLER              PIC X.                                   
051300     EJECT                                                                
051400                                                                          
051500*01  -COPY W0008                 -PRE GPRIB-.                             
051600         05  FILLER               PIC X.                                  
051700     EJECT                                                                
051800*01  -COPY W0008                 -PRE GMTB-.                              
051900         05  FILLER              PIC X.                                   
052000     EJECT                                                                
052100*01  -COPY W0008                 -PRE 9305-                               
052200     05  FILLER                  PIC X.                                   
052300     EJECT                                                                
052400*01  -COPY W0008                 -PRE WDB1-                               
052500     05  FILLER                  PIC X.                                   
052600     EJECT                                                                
052700*01  -COPY W0008                 -PRE 4128-                               
052800     05  FILLER                  PIC X.                                   
052900                                                                          
053000     EJECT                                                                
053100 01  COST-WDK6-PCB               PIC X.                                   
053200 01  COST-WDK7-PCB               PIC X.                                   
053300 01  COST-WDF1-PCB               PIC X.                                   
053400 01  COST-9305-PCB               PIC X.                                   
053500 01  COST-WDK72-PCB              PIC X.                                   
053600 01  COST-WDB6-PCB               PIC X.                                   
053700 01  PRIS-COST-WDK6-PCB          PIC X.                                   
053800 01  PRIS-COST-WDK7-PCB          PIC X.                                   
053900 01  PRIS-COST-WDF1-PCB          PIC X.                                   
054000 01  PRIS-COST-9305-PCB          PIC X.                                   
054100 01  PRIS-COST-WDK72-PCB         PIC X.                                   
054200 01  PRIS-COST-WDB6-PCB          PIC X.                                   
054300                                                                          
054400                                                                          
054500 PROCEDURE DIVISION  USING LINK-W418KTL1 KREE-PCB                         
054600                                         WDL5-PCB                         
054700                                         WDB2-PCB                         
054800                                         ARTC-PCB                         
054900                                         4109-PCB                         
055000                                        PARTC-PCB                         
055100                                        PWDK7-PCB                         
055200                                        PGMTA-PCB                         
055300                                         BETA-PCB                         
055400                                         PRIA-PCB                         
055500                                        GPRIB-PCB                         
055600                                         GMTB-PCB                         
055700                                         9305-PCB                         
055800                                         WDB1-PCB                         
055900                                         4128-PCB                         
056000                                    COST-WDK6-PCB                         
056100                                    COST-WDK7-PCB                         
056200                                    COST-WDF1-PCB                         
056300                                    COST-9305-PCB                         
056400                                    COST-WDK72-PCB                        
056500                                    COST-WDB6-PCB                         
056600                               PRIS-COST-WDK6-PCB                         
056700                               PRIS-COST-WDK7-PCB                         
056800                               PRIS-COST-WDF1-PCB                         
056900                               PRIS-COST-9305-PCB                         
057000                               PRIS-COST-WDK72-PCB                        
057100                               PRIS-COST-WDB6-PCB.                        
057200                                                                          
057300 STYR SECTION.                                                            
057400     MOVE '*** STYR SECTION *** '                                         
057500                              TO FELTEXT-STR                              
057600*    DISPLAY '*** INNE I W418KTL1 !!! '                                   
057700     PERFORM A-INIT                                                       
057800                                                                          
057900     PERFORM B-LAS-BASER                                                  
058000                                                                          
058100     PERFORM C-SAETT-EV-FELKOD                                            
058200     IF LINK-KDANMORS = '39'                                              
058300        MOVE 'R39'            TO LINK-KDKREBEH                            
058400     END-IF                                                               
058500                                                                          
058600     IF ALLT-OK                                                           
058700       MOVE '   '             TO LINK-KDSVAR                              
058800     ELSE                                                                 
058900       MOVE 'F'               TO LINK-KDSVAR                              
059000     END-IF                                                               
059100*    CALL fellog                                                          
059200*    DISPLAY '*** SLUT I W418KTL1 !!! '                                   
059300     MOVE ZERO                TO RETURN-CODE                              
059400     GOBACK                                                               
059500     .                                                                    
059600     EJECT                                                                
059700                                                                          
059800 A-INIT SECTION.                                                          
059900     MOVE '*** A-INIT *** '                                               
060000                              TO FELTEXT-STR                              
060100                                                                          
060200     MOVE FUNCTION CURRENT-DATE(5:2) TO W-DATE-AAMM(3:2)                  
060300     MOVE FUNCTION CURRENT-DATE(3:2) TO W-DATE-AAMM(1:2)                  
060400                                                                          
060500     MOVE LOW-VALUE           TO W-IDLEVANM-X                             
060600                                 W-IDARTNR-X                              
060700                                 W-IDGMT-MIN-X                            
060800                                 W-IDGMT-X                                
060900                                 W-WDB301KY-X                             
061000                                 W-IDGMTREF-X                             
061100                                                                          
061200     MOVE HIGH-VALUE          TO W-IDGMT-MAX-X                            
061300                                                                          
061400     MOVE ZERO                TO WS-KVDAGAR                               
061500                                                                          
061600     MOVE SPACE               TO LINK-KDSVAR                              
061700                                                                          
061800     MOVE JA                  TO ALLT-SW                                  
061900                                                                          
062000                                 WDB201-SW                                
062100                                 WLGMTB01-SW                              
062200                                                                          
062300                                 WLARTC01-SW                              
062400                                 WLARTC11-SW                              
062500                                                                          
062600                                 WLKREE01-SW                              
062700                                                                          
062800                                 WDL5-SW                                  
062900*--- FLYTTA NYCKLAR FÖR LÄSNINGAR                                         
063000                                                                          
063100                                                                          
063200     INITIALIZE                  LINK-TEMFSINF                            
063300     MOVE LINK-IDDISTR        TO W-IDDISTR-WDB2-MIN                       
063400                                 W-IDDISTR-WDB2-MAX                       
063500                                 W-IDDISTR-WDB2                           
063600                                 W-IDDISTR-WDB3                           
063700                                 W-IDDISTR-WDB3-DEF                       
063800                                 W-IDDISTR-WDA2                           
063900                                 W-IDDISTR-WDL5                           
064000                                 TP8GRET-IDDISTR                          
064100     MOVE LINK-IDKUNDNR       TO W-IDKUNDNR-WDB3                          
064200                                 W-IDKUNDNR-WDB2                          
064300                                 W-IDKUNDNR-WDA2                          
064400                                 W-IDKUNDNR-WDL5                          
064500                                 TP8GRET-IDKUNDNR                         
064600     MOVE +9999999            TO W-IDKUNDNR-WDB3-DEF                      
064700     MOVE SPACE               TO W-IDKUNDRF                               
064800     MOVE LINK-IDORDNR5       TO W-IDORDNR7                               
064900     MOVE LINK-IDKOLLI        TO W-IDKOLLI                                
065000     MOVE LINK-IDRAPPNR       TO W-IDRAPPNR-WDA2                          
065100     MOVE LINK-IDARTNR        TO W-IDARTNR                                
065200                                 W-IDARTNR-4109-MIN                       
065300                                 W-IDARTNR-4109-MAX                       
065400     MOVE LINK-KDANMORS       TO W-KDANMORS-4109-MIN                      
065500                                 W-KDANMORS-4109-MAX                      
065600     MOVE LINK-IDFAKT         TO W-IDFAKT                                 
065700                                                                          
065800     MOVE LINK-IDFTG          TO W-IDFTG-4109                             
065900                                 WS-IDFTG                                 
066000     PERFORM IMS-GU-WDL501                                                
066100     IF SEGMENT-SAKNAS                                                    
066200        MOVE NEJ              TO WDL5-SW                                  
066300     END-IF                                                               
066400*--- FOR API IDDC IS MOVED FROM INVOICE DB HENCE IT WILL BE SPACE         
066500     IF LINK-IDDC  = SPACE                                                
066600        IF SEGMENT-FINNS                                                  
066700           MOVE FAK-IDDC        TO LINK-IDDC                              
066800          IF LINK-KDANMORS = '20'                                         
066900            MOVE  LINK-IDDC     TO WS-IDDC-API                            
067000            MOVE  FAK-IDFAKT    TO WS-IDFAKT                              
067100          END-IF                                                          
067200*-------IF CODE IS 22/23 THEN  SEGMENT WILL NOT BE PRESENT IN WDL5        
067300*-------HENCE MOVE IDDC FROM CODE 20 TO CODE 22/23                        
067400        ELSE                                                              
067500          IF LINK-KDANMORS = '22' OR '23'                                 
067600            IF LINK-IDFAKT = WS-IDFAKT                                    
067700              MOVE WS-IDDC-API  TO LINK-IDDC                              
067800            END-IF                                                        
067900          ELSE                                                            
068000*-------IF CANNOT FIND DETAILS IN INVVOICE DB THEN THROW ERROR            
068100            IF LINK-TEMFSINF= SPACE                                       
068200              MOVE 'WRONG INVOICE INFORMATION  '                          
068300                                TO LINK-TEMFSINF                          
068400            END-IF                                                        
068500          END-IF                                                          
068600        END-IF                                                            
068700     END-IF                                                               
068800*--- NÄR DET ÄR ALFA-DC SKALL MAN HÄMTA DATA SOM FÖR CDC(DC11).           
068900     MOVE LINK-IDDC             TO WS-IDDC                                
069000     IF GOOD-DDC                                                          
069100       MOVE '11'                TO W-IDDC-WDB3                            
069200                                   W-IDDC-WDB3-DEF                        
069400     ELSE                                                                 
069500       MOVE LINK-IDDC           TO W-IDDC-WDB3                            
069600                                   W-IDDC-WDB3-DEF                        
069800     END-IF                                                               
069900                                                                          
070000                                                                          
070100*--- ANROPA KONTROLL AV ORSAKSKODER                                       
070200                                                                          
070300     MOVE LINK-KDANMORS       TO OKOD-KDANMORS                            
070400                                                                          
070500     CALL W418OKOD USING OKOD-W418OKOD                                    
070600                                                                          
070700     ACCEPT DAGENS-DATUM      FROM DATE                                   
070800     PERFORM AA-HAMTA-DEFAULT-LEDTID                                      
070900     MOVE W-DATE-AAMM         TO CURR-TIAAMM                              
071000     MOVE WS-KDVALISO-HUV     TO CURR-KDVALISO-HUV                        
071100     MOVE 'M'                 TO CURR-KDVALTYP                            
071200     .                                                                    
071300     EJECT                                                                
071400 AA-HAMTA-DEFAULT-LEDTID SECTION.                                         
071500     MOVE '*** AA-DEF-DLEDTID ***'                                        
071600                                 TO FELTEXT-STR                           
071700                                                                          
071800     MOVE 1                      TO W-4128-IDDISTR-FKY                    
071900     MOVE 9999                   TO W-4128-IDDISTR-TKY                    
072000     MOVE ZERO                   TO W-4128-IDKUNDNR-FKY                   
072100     MOVE 999999                 TO W-4128-IDKUNDNR-TKY                   
072200     MOVE SPACE                  TO W-4128-KDANMORS-KY                    
072300     PERFORM IMS-GU-WDGX4128                                              
072400                                                                          
072500     IF SEGMENT-FINNS                                                     
072600        MOVE 4128-KVDAGAR-LTRP   TO WS-KVDAGAR-LTRP-DEF                   
072700        MOVE 4128-KVDAGAR-LEVANM TO WS-KVDAGAR-LEV-DEF                    
072800     END-IF                                                               
072900     .                                                                    
073000     EJECT                                                                
073100                                                                          
073200 B-LAS-BASER SECTION.                                                     
073300     MOVE '*** B-LAS-BASER *** '                                          
073400                              TO FELTEXT-STR                              
073500                                                                          
073600     PERFORM BA-LAS-KUNDREGISTRET                                         
073700                                                                          
073800     PERFORM BB-LAS-ARTIKELREGISTRET                                      
073900                                                                          
074000     PERFORM BC-LAS-KREDITERINGSREGISTRET                                 
074100     MOVE LINK-IDDISTR           TO TEST-IDDISTR                          
074200     IF DIST07-USA-RET-DISCR     OR                                       
074300        DIST07-CAN-RET-DISCR     OR                                       
074400        DIST07-KINA-RET-DISCR    OR                                       
074500        DIST07-INDIEN-RET-DISCR  OR                                       
074600        DIST07-KOREA-RET-DISCR   OR                                       
074700        DIST07-TURKEY-RET-DISCR  OR                                       
074800        DIST07-MALAYSIA-RET-DISCR OR                                      
074900        DIST07-THAILAND-RET-DISCR OR                                      
075000        DIST07-TAIWAN-RET-DISCR OR                                        
075100        DIST07-MEXICO-RET-DISCR OR                                        
075101        DIST07-BRAZIL-RET-DISCR OR                                        
075102        DIST07-S-AFRICA-RET-DISCR OR                                      
075200        NOT IDFTG-PV                                                      
075300       IF WDL5-FINNS                                                      
076000         PERFORM IMS-GNP-WDL511                                           
076100         IF SEGMENT-SAKNAS                                                
076200            MOVE NEJ           TO WDL5-SW                                 
076300         ELSE                                                             
076500           MOVE FAKC-IDPRODNR  TO W-IDPRODNR                              
076600           PERFORM IMS-GNP-WDL521                                         
076700           IF SEGMENT-SAKNAS                                              
076800              MOVE NEJ         TO WDL5-SW                                 
076900           END-IF                                                         
077000         END-IF                                                           
077100       END-IF                                                             
077200     ELSE                                                                 
077300        PERFORM BD-LAS-FAKTURAHISTREGISTRET                               
077400     END-IF                                                               
077500                                                                          
077600     IF WDL5-FINNS                                                        
077700       PERFORM BE-HAMTA-CLAIM-TIDER                                       
077800     END-IF                                                               
077900                                                                          
078000     IF OKOD-FL-ANALYSNR = JA                                             
078100        PERFORM BF-LAS-ANALYSNRREGISTRET                                  
078200     END-IF                                                               
078300     .                                                                    
078400     EJECT                                                                
078500 BA-LAS-KUNDREGISTRET SECTION.                                            
078600     MOVE '*** BA-LAS-KUNDREGISTRET *** '                                 
078700                              TO FELTEXT-STR                              
078800*    DISPLAY '*** BA-LAS-KUNDREG'                                         
078900*-- KUNDREGISTRET WDB2/WDB3                                               
079000                                                                          
079100     IF LINK-IDKUNDNR = 0                                                 
079200       PERFORM IMS-GET-WDB201                                             
079300       IF SEGMENT-FINNS                                                   
079400                                                                          
079500*** LÄS UNIK FÖR ATT BL.A. KOLLA SÅ ATT KUNDNR 0 FINNS FÖR DISTR.         
079600                                                                          
079700         PERFORM IMS-GET-WDB201-UNIK                                      
079800       END-IF                                                             
079900     ELSE                                                                 
080000       PERFORM IMS-GET-WDB201-UNIK                                        
080100     END-IF                                                               
080200                                                                          
080300     IF SEGMENT-SAKNAS                                                    
080400        MOVE NEJ              TO WDB201-SW                                
080500                                 WLGMTB01-SW                              
080600     ELSE                                                                 
080700        MOVE JA               TO WDB201-SW                                
080800        PERFORM IMS-GET-WLGMTB01                                          
080900                                                                          
081000        IF SEGMENT-SAKNAS                                                 
081100           MOVE NEJ           TO WLGMTB01-SW                              
081200        ELSE                                                              
081300           MOVE JA            TO WLGMTB01-SW                              
081400        END-IF                                                            
081500     END-IF                                                               
081600                                                                          
081700     .                                                                    
081800     EJECT                                                                
081900                                                                          
082000 BB-LAS-ARTIKELREGISTRET SECTION.                                         
082100     MOVE '*** BB-LAS-ARTIKELREGISTRET *** '                              
082200                              TO FELTEXT-STR                              
082300                                                                          
082400*-- ARTIKELREGISTRET WDK6                                                 
082500                                                                          
082600     PERFORM IMS-GET-WLARTC01                                             
082700                                                                          
082800     IF SEGMENT-SAKNAS                                                    
082900        MOVE NEJ              TO WLARTC01-SW                              
083000                                 WLARTC11-SW                              
083100     ELSE                                                                 
083200        MOVE JA               TO WLARTC01-SW                              
083300        PERFORM IMS-GET-WLARTC11                                          
083400                                                                          
083500        IF SEGMENT-SAKNAS                                                 
083600           MOVE NEJ           TO WLARTC11-SW                              
083700        ELSE                                                              
083800           MOVE JA            TO WLARTC11-SW                              
083900        END-IF                                                            
084000     END-IF                                                               
084100     .                                                                    
084200     EJECT                                                                
084300 BC-LAS-KREDITERINGSREGISTRET SECTION.                                    
084400     MOVE '*** BC-LAS-KREDITERINGSREGISTRET *** '                         
084500                              TO FELTEXT-STR                              
084600                                                                          
084700*-- KREDITERINGSREGISTRET WDA2                                            
084800                                                                          
084900     PERFORM IMS-GET-WLKREE01                                             
085000                                                                          
085100     IF SEGMENT-SAKNAS                                                    
085200        MOVE NEJ              TO WLKREE01-SW                              
085300     ELSE                                                                 
085400        MOVE JA               TO WLKREE01-SW                              
085500     END-IF                                                               
085600     .                                                                    
085700     EJECT                                                                
085800 BD-LAS-FAKTURAHISTREGISTRET SECTION.                                     
085900     MOVE '*** BD-LAS-FAKTURAHISTREGISTRET *** '                          
086000                              TO FELTEXT-STR                              
086100                                                                          
086400     MOVE +0                  TO WS-KVLEVART                              
086500                                 WS-PRARTBTO                              
086600                                 WS-PRARTBTO-LOC                          
086700     IF WDL5-FINNS                                                        
086800        IF LINK-KDANMORS = '74'                                           
086900           MOVE FAK-IDKONTO   TO LINK-IDKONTO                             
087000           MOVE FAK-IDKST     TO LINK-IDKST                               
087100           MOVE FAK-IDANALYS  TO LINK-IDANALYS                            
087200        END-IF                                                            
087300        PERFORM IMS-GNP-WDL511                                            
087400        IF SEGMENT-SAKNAS                                                 
087500           MOVE NEJ           TO WDL5-SW                                  
087600        ELSE                                                              
087700          MOVE FAKC-IDPRODNR  TO W-IDPRODNR                               
087800          PERFORM IMS-GNP-WDL521                                          
087900          IF SEGMENT-SAKNAS                                               
088000             MOVE NEJ         TO WDL5-SW                                  
088100          END-IF                                                          
088200          PERFORM UNTIL SEGMENT-SAKNAS                                    
088300             COMPUTE WS-KVLEVART = WS-KVLEVART + FAKL-KVLEVART            
088400             COMPUTE WS-PRARTBTO = WS-PRARTBTO + FAKL-PRARTNTO            
088500             COMPUTE WS-PRARTBTO-LOC = WS-PRARTBTO-LOC +                  
088600                                       FAKL-PRARTNTO-LOC                  
088700            PERFORM IMS-GNP-WDL521                                        
088800          END-PERFORM                                                     
088900        END-IF                                                            
089000        MOVE SPACE            TO STATUS-WS                                
089100     END-IF                                                               
089200     .                                                                    
089300     EJECT                                                                
089400 BE-HAMTA-CLAIM-TIDER SECTION.                                            
089500     MOVE '*** BE-HAMTA-CLAIM-TIDER     *** '                             
089600                               TO FELTEXT-STR                             
089700                                                                          
089800     MOVE SPACE                TO CURR-KDANMORS                           
089900     PERFORM IMS-GU-WDR501                                                
090000     MOVE LINK-IDDISTR         TO W-4128-IDDISTR-FKY                      
090100                                  W-4128-IDDISTR-TKY                      
090200                                  W-4128-IDDISTR-FOM                      
090300                                  W-4128-IDDISTR-TOM                      
090400     MOVE LINK-IDKUNDNR        TO W-4128-IDKUNDNR-FKY                     
090500                                  W-4128-IDKUNDNR-TKY                     
090600     MOVE LINK-KDANMORS        TO W-4128-KDANMORS-KY                      
090700                                  WS-KDANMORS                             
090800     PERFORM IMS-GU-WDGX4128                                              
090900     IF SEGMENT-SAKNAS                                                    
091000       PERFORM IMS-GU-WDR501                                              
091100       PERFORM IMS-GNP-WDGX4128-DISTRIKT                                  
091200       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
091300         PERFORM BEA-KOLLA-SPARA-REGEL                                    
091400         PERFORM IMS-GNP-WDGX4128-DISTRIKT                                
091500       END-PERFORM                                                        
091600     ELSE                                                                 
091700       MOVE JA                  TO SW-REGEL-VALD                          
091800       MOVE 4128-IDDISTR-FOM    TO CURR-IDDISTR-FOM                       
091900       MOVE 4128-IDDISTR-TOM    TO CURR-IDDISTR-TOM                       
092000       MOVE 4128-IDKUNDNR-FOM   TO CURR-IDKUNDNR-FOM                      
092100       MOVE 4128-IDKUNDNR-TOM   TO CURR-IDKUNDNR-TOM                      
092200       MOVE 4128-KDANMORS       TO CURR-KDANMORS                          
092300     END-IF                                                               
092400                                                                          
092500     IF SW-REGEL-VALD = JA                                                
092600       MOVE CURR-IDDISTR-FOM     TO W-4128-IDDISTR-FKY                    
092700       MOVE CURR-IDDISTR-TOM     TO W-4128-IDDISTR-TKY                    
092800       MOVE CURR-IDKUNDNR-FOM    TO W-4128-IDKUNDNR-FKY                   
092900       MOVE CURR-IDKUNDNR-TOM    TO W-4128-IDKUNDNR-TKY                   
093000       MOVE CURR-KDANMORS        TO W-4128-KDANMORS-KY                    
093100                                                                          
093200       PERFORM IMS-GU-WDGX4128                                            
093300       IF SEGMENT-FINNS                                                   
093400         IF GMT-FLLDCKND = JA                                             
093500           IF 4128-KVDAGAR-LTRP-LDC   > ZERO OR                           
093600              4128-KVDAGAR-LEVANM-LDC > ZERO                              
093700             COMPUTE WS-KVDAGAR =                                         
093800             4128-KVDAGAR-LTRP-LDC + 4128-KVDAGAR-LEVANM-LDC              
093900           ELSE                                                           
094000             COMPUTE WS-KVDAGAR =                                         
094100             4128-KVDAGAR-LTRP + 4128-KVDAGAR-LEVANM                      
094200           END-IF                                                         
094300         ELSE                                                             
094400           COMPUTE WS-KVDAGAR =                                           
094500           4128-KVDAGAR-LTRP + 4128-KVDAGAR-LEVANM                        
094600         END-IF                                                           
094700       END-IF                                                             
094800     ELSE                                                                 
094900       COMPUTE WS-KVDAGAR =                                               
095000       WS-KVDAGAR-LTRP-DEF + WS-KVDAGAR-LEV-DEF                           
095100     END-IF                                                               
095200                                                                          
095300     IF WS-KVDAGAR > ZERO                                                 
095400       PERFORM BEB-KOLLA-OM-VALID-FAKT                                    
095500     END-IF                                                               
095600     .                                                                    
095700     EJECT                                                                
095800 BEA-KOLLA-SPARA-REGEL SECTION.                                           
095900     MOVE '*** BEA-KOLLA-SPARA-REGEL    *** '                             
096000                                  TO FELTEXT-STR                          
096100     IF 4128-KDANMORS(1:1) = WS-KDANMORS(1:1)                             
096200     OR 4128-KDANMORS      = SPACE                                        
096300       IF   4128-IDKUNDNR-FOM = W-4128-IDKUNDNR-FKY                       
096400       AND  4128-IDKUNDNR-TOM = W-4128-IDKUNDNR-TKY                       
096500         IF  4128-KDANMORS      = WS-KDANMORS                             
096600         OR (4128-KDANMORS(2:1) = 'X'                                     
096700         AND 4128-KDANMORS(1:1) = WS-KDANMORS(1:1))                       
096800         OR  4128-KDANMORS      = SPACE                                   
096900           MOVE JA                TO SW-REGEL-VALD                        
097000           MOVE 4128-IDDISTR-FOM  TO CURR-IDDISTR-FOM                     
097100           MOVE 4128-IDDISTR-TOM  TO CURR-IDDISTR-TOM                     
097200           MOVE 4128-IDKUNDNR-FOM TO CURR-IDKUNDNR-FOM                    
097300           MOVE 4128-IDKUNDNR-TOM TO CURR-IDKUNDNR-TOM                    
097400           MOVE 4128-KDANMORS     TO CURR-KDANMORS                        
097500         END-IF                                                           
097600       ELSE                                                               
097700         IF  W-4128-IDKUNDNR-FKY NOT < 4128-IDKUNDNR-FOM                  
097800         AND W-4128-IDKUNDNR-TKY NOT > 4128-IDKUNDNR-TOM                  
097900           IF (4128-IDKUNDNR-FOM > CURR-IDKUNDNR-FOM                      
098000           OR  4128-IDKUNDNR-TOM < CURR-IDKUNDNR-TOM)                     
098100             MOVE JA                TO SW-REGEL-VALD                      
098200             MOVE 4128-IDDISTR-FOM  TO CURR-IDDISTR-FOM                   
098300             MOVE 4128-IDDISTR-TOM  TO CURR-IDDISTR-TOM                   
098400             MOVE 4128-IDKUNDNR-FOM TO CURR-IDKUNDNR-FOM                  
098500             MOVE 4128-IDKUNDNR-TOM TO CURR-IDKUNDNR-TOM                  
098600             MOVE 4128-KDANMORS     TO CURR-KDANMORS                      
098700           ELSE                                                           
098800             IF (4128-IDKUNDNR-FOM = CURR-IDKUNDNR-FOM                    
098900             AND 4128-IDKUNDNR-TOM = CURR-IDKUNDNR-TOM)                   
099000               IF  4128-KDANMORS      = WS-KDANMORS                       
099100               OR (4128-KDANMORS(2:1) = 'X'                               
099200               AND 4128-KDANMORS(1:1) = WS-KDANMORS(1:1))                 
099300               OR  4128-KDANMORS      = SPACE                             
099400                 MOVE JA                TO SW-REGEL-VALD                  
099500                 MOVE 4128-IDDISTR-FOM  TO CURR-IDDISTR-FOM               
099600                 MOVE 4128-IDDISTR-TOM  TO CURR-IDDISTR-TOM               
099700                 MOVE 4128-IDKUNDNR-FOM TO CURR-IDKUNDNR-FOM              
099800                 MOVE 4128-IDKUNDNR-TOM TO CURR-IDKUNDNR-TOM              
099900                 MOVE 4128-KDANMORS     TO CURR-KDANMORS                  
100000               END-IF                                                     
100100             END-IF                                                       
100200           END-IF                                                         
100300         END-IF                                                           
100400       END-IF                                                             
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800 BEB-KOLLA-OM-VALID-FAKT SECTION.                                         
100900     MOVE '*** BEB-KOLLA-OM-VALID-FAKT  *** '                             
101000                              TO FELTEXT-STR                              
101100                                                                          
101200*    --- KONTROLLERA OM TIDSGRÄNSEN ENLIGT DE NYA REGLERNA SOM            
101300*    --- SYNS PÅ BILD 4753 ÄR UPPNÅDD!                                    
101400*    --- TROTS ATT FAKT. FINNS PÅ WDL5 KOLLAR MAN OM JUST DENNA           
101500*    --- KOMBINATION D+K+KOD HAR EN LÄGRE TIDSGRÄNS OCH SKICKAR           
101600*    --- I SÅ FALL TILLBAKA 'FAKTURA SAKNAS' TILL HUVUD PGM.              
101700                                                                          
101800      MOVE 'AAMMDD'             TO DAT-KDDATFORM                          
101900      MOVE FAKC-TIFAKT          TO DAT-I-TIDATUM                          
102000                                                                          
102100      CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                     
102200                      DAT-O-TIDATUM DAT-KDSVAR                            
102300                                                                          
102400      IF DAT-KDSVAR-OK                                                    
102500        MOVE DAT-TIAADDD        TO WS-FAK-DATUM-AADDD                     
102600      ELSE                                                                
102700        MOVE 'FEL FRÅN DATKONV' TO FELTEXT-STR                            
102800        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
102900      END-IF                                                              
103000                                                                          
103100     COMPUTE WS-TAL            =  WS-KVDAGAR /                            
103200                                  365                                     
103300     IF WS-HELTAL = 0                                                     
103400       MOVE WS-FAK-DATUM-AA     TO WS-JFRDAT-AA                           
103500                                                                          
103600       COMPUTE WS-JFRDAT-DDD   = WS-FAK-DATUM-DDD +                       
103700                                 WS-KVDAGAR                               
103800       END-COMPUTE                                                        
103900     END-IF                                                               
104000                                                                          
104100     IF WS-HELTAL = 1                                                     
104200       COMPUTE WS-DDD          = WS-KVDAGAR -                             
104300                                 WS-ETT-AR                                
104400       END-COMPUTE                                                        
104500                                                                          
104600       COMPUTE WS-JFRDAT-AA    = WS-FAK-DATUM-AA +                        
104700                                 1                                        
104800       END-COMPUTE                                                        
104900                                                                          
105000       COMPUTE WS-JFRDAT-DDD   = WS-FAK-DATUM-DDD +                       
105100                                 WS-DDD                                   
105200       END-COMPUTE                                                        
105300     END-IF                                                               
105400                                                                          
105500     IF WS-HELTAL = 2                                                     
105600       COMPUTE WS-DDD          = WS-KVDAGAR -                             
105700                                 WS-TVA-AR                                
105800       END-COMPUTE                                                        
105900                                                                          
106000       COMPUTE WS-JFRDAT-AA    = WS-FAK-DATUM-AA +                        
106100                                 2                                        
106200       END-COMPUTE                                                        
106300                                                                          
106400       COMPUTE WS-JFRDAT-DDD   = WS-FAK-DATUM-DDD +                       
106500                                 WS-DDD                                   
106600       END-COMPUTE                                                        
106700     END-IF                                                               
106800                                                                          
106900     IF WS-JFRDAT-DDD > 365                                               
107000       SUBTRACT 365 FROM WS-JFRDAT-DDD                                    
107100       ADD      1   TO   WS-JFRDAT-AA                                     
107200     END-IF                                                               
107300     MOVE WS-JFRDAT-TEST        TO WS-JFRDAT                              
107400                                                                          
107500     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
107600     MOVE DAGENS-DATUM          TO DAT-I-TIDATUM                          
107700                                                                          
107800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
107900                     DAT-O-TIDATUM DAT-KDSVAR                             
108000                                                                          
108100     IF DAT-KDSVAR-OK                                                     
108200       MOVE DAT-TIAADDD         TO WS-DAGENS-DATUM-AADDD                  
108300     ELSE                                                                 
108400       MOVE 'FEL FRÅN DATKONV'  TO FELTEXT-STR                            
108500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
108600     END-IF                                                               
108700                                                                          
108800     IF WS-DAGENS-DATUM-AADDD > WS-JFRDAT                                 
108900       MOVE NEJ                 TO WDL5-SW                                
109000     END-IF                                                               
109100     .                                                                    
109200     EJECT                                                                
109300 BF-LAS-ANALYSNRREGISTRET SECTION.                                        
109400     MOVE '*** BF-LAS-ANALYSNRREGISTRET *** '                             
109500                              TO FELTEXT-STR                              
109600                                                                          
109700     PERFORM IMS-GU-410901-ROT                                            
109800                                                                          
109900     PERFORM IMS-GNP-410911-KVAL                                          
110000     IF SEGMENT-SAKNAS                                                    
110100        MOVE NEJ              TO WL410901-SW                              
110200     ELSE                                                                 
110300        MOVE NEJ              TO WL410901-SW                              
110400        PERFORM UNTIL SEGMENT-SAKNAS OR WL410901-SW = JA                  
110500           MOVE LINK-TILEVANM           TO TMP1-YYMMDD                    
110600           MOVE 4110-DAGILTIG-FOM (3:6) TO TMP2-YYMMDD                    
110700           MOVE 4110-DAGILTIG-TOM (3:6) TO TMP3-YYMMDD                    
110800           PERFORM WY2000Q1                                               
110900           IF TMP1-YYMMDD >= TMP2-YYMMDD AND                              
111000                          <= TMP3-YYMMDD                                  
111100              MOVE JA         TO WL410901-SW                              
111200           ELSE                                                           
111300              PERFORM IMS-GNP-410911-KVAL                                 
111400           END-IF                                                         
111500        END-PERFORM                                                       
111600     END-IF                                                               
111700     .                                                                    
111800     EJECT                                                                
111900 C-SAETT-EV-FELKOD SECTION.                                               
112000     MOVE '*** C-SAETT-EV-FELKOD *** '                                    
112100                              TO FELTEXT-STR                              
112200                                                                          
112300     PERFORM CA-KOLLA-DISTRIKT-KUND                                       
112400     IF WDB201-FINNS                                                      
112500        PERFORM CB-KOLLA-DIV-FEL-ORSAKSKOD                                
112600        PERFORM CC-KOLLA-ARTIKEL                                          
112700        PERFORM CD-KOLLA-ANTAL-PRIS                                       
112800        IF DIST07-USA-RET-DISCR    OR                                     
112900           DIST07-CAN-RET-DISCR    OR                                     
113000           DIST07-KINA-RET-DISCR   OR                                     
113100           DIST07-INDIEN-RET-DISCR OR                                     
113200           DIST07-KOREA-RET-DISCR  OR                                     
113300           DIST07-TURKEY-RET-DISCR OR                                     
113400           DIST07-MALAYSIA-RET-DISCR OR                                   
113500           DIST07-THAILAND-RET-DISCR OR                                   
113600           DIST07-TAIWAN-RET-DISCR OR                                     
113700           DIST07-MEXICO-RET-DISCR OR                                     
113701           DIST07-BRAZIL-RET-DISCR OR                                     
113710           DIST07-S-AFRICA-RET-DISCR OR                                   
113800           LINK-IDPTYP     = 'REF' OR                                     
113900           NOT IDFTG-PV                                                   
114000                                                                          
114100*------TILLAGT 971203 BO S                                                
114200*------SE äVEN B-SEKTIONEN                                                
114300           IF WDL5-FINNS                                                  
114400              MOVE FAKL-IDUSER-PACK TO LINK-IDUSER-PACK                   
114500              MOVE FAKC-KDORDKL     TO LINK-KDORDKL                       
114600           ELSE                                                           
114700              MOVE SPACE            TO LINK-IDUSER-PACK                   
114800              MOVE +9               TO LINK-KDORDKL                       
114900           END-IF                                                         
115000*------HIT 971203 BO S                                                    
115100                                                                          
115200*- FÖR ATT PRISTILLÄMPA FÖR KOD 11,21 PÅ DISTR 7580/7680 USA-VOR          
115300*- TILLAGT I DDGS-PROJEKTET 990526                                        
115400*- 20111018 SKALL GÄLLA SAMMA FÖR DISTR 6280 KINA OCH DIST 6271           
115500*- ÄVEN INDIEN                                                            
115600           IF DIST07-USA-RET-DISCR    OR                                  
115700              DIST07-CAN-RET-DISCR    OR                                  
115800              DIST07-KINA-RET-DISCR   OR                                  
115900              DIST07-KINA             OR                                  
116000              DIST07-INDIEN           OR                                  
116100              DIST07-INDIEN-RET-DISCR OR                                  
116200              DIST07-KOREA            OR                                  
116300              DIST07-KOREA-RET-DISCR  OR                                  
116400              DIST07-TURKEY           OR                                  
116500              DIST07-TURKEY-RET-DISCR OR                                  
116510              DIST07-S-AFRICA         OR                                  
116520              DIST07-S-AFRICA-RET-DISCR OR                                
116600              DIST07-MALAYSIA         OR                                  
116700              DIST07-MALAYSIA-RET-DISCR OR                                
116800              DIST07-THAILAND           OR                                
116900              DIST07-THAILAND-RET-DISCR OR                                
117000              DIST07-TAIWAN             OR                                
117100              DIST07-TAIWAN-RET-DISCR   OR                                
117200              DIST07-MEXICO             OR                                
117300              DIST07-MEXICO-RET-DISCR   OR                                
117310              DIST07-BRAZIL             OR                                
117320              DIST07-BRAZIL-RET-DISCR                                     
117400              IF LINK-KDANMORS = '11' OR '21'                             
117500                IF WLARTC01-FINNS                                         
117600                  IF WLARTC11-FINNS                                       
117700                    IF OKOD-FL-PRISTILLAEMPA = JA                         
117800                      IF DIST79-DEALER-PRICE OR                           
118000                         DIST79-ECOM-PRICE                                
118100                        IF DIST79-ECOM-PRICE                              
118200                         CONTINUE                                         
118300                        ELSE                                              
118400                          IF LINK-PRARTBTO-LOC = ZERO                     
118500                            PERFORM CGAA-PRISTILLAEMPA                    
118600                          END-IF                                          
118700                        END-IF                                            
118800                      ELSE                                                
118900                        IF LINK-PRARTBTO = ZERO                           
119000                          PERFORM CGAA-PRISTILLAEMPA                      
119100                        END-IF                                            
119200                      END-IF                                              
119300                    END-IF                                                
119400                  END-IF                                                  
119500                END-IF                                                    
119600              END-IF                                                      
119700           END-IF                                                         
119800        ELSE                                                              
119900           PERFORM CE-KOLLA-FAKTURAINFO                                   
120000           PERFORM CG-KOLLA-FAKTURAINFO                                   
120100        END-IF                                                            
120200        PERFORM CF-KOLLA-ANALYSNR                                         
120300        PERFORM CH-KOLLA-OM-DEN-FINNS-PA-KREE                             
120400        PERFORM CI-KOLLA-TIDS-VAERDEGRAENS                                
120500        PERFORM CJ-FLYTTA-DATA                                            
120600     END-IF                                                               
120700     .                                                                    
120800     EJECT                                                                
120900 CA-KOLLA-DISTRIKT-KUND SECTION.                                          
121000     MOVE '*** CA-KOLLA-DISTRIKT-KUND *** '                               
121100                              TO FELTEXT-STR                              
121200                                                                          
121300     MOVE +1                  TO INDX                                     
121400                                                                          
121500*    DISPLAY '*** WDB201-SW   = ' WDB201-SW                               
121600*    DISPLAY '*** WLGMTB01-SW   = ' WLGMTB01-SW                           
121700*--- DISTRIKT/KUND                                                        
121800     IF WDB201-SAKNAS                                                     
121900*       DISPLAY '*** GMTA01-SAKNAS 701 '                                  
122000        MOVE '701'            TO LINK-IDFELKOD(INDX)                      
122100        MOVE NEJ              TO ALLT-SW                                  
122200        ADD +1                TO INDX                                     
122300*--- LINK-TEMF IS EMPTY ONLY FOR API                                      
122400        IF LINK-TEMFSINF = SPACE                                          
122500          MOVE 'WRONG DISTRICT NO  '                                      
122600                              TO LINK-TEMFSINF                            
122700        END-IF                                                            
122800     END-IF                                                               
122900                                                                          
123000                                                                          
123100*--- DC                                                                   
123200     IF WDB201-FINNS                                                      
123300        IF WLGMTB01-SAKNAS                                                
123400           MOVE '737'         TO LINK-IDFELKOD(INDX)                      
123500           MOVE NEJ           TO ALLT-SW                                  
123600           ADD +1             TO INDX                                     
123700           IF LINK-TEMFSINF = SPACE                                       
123800             MOVE 'WRONG CUSTOMER INFO  '                                 
123900                              TO LINK-TEMFSINF                            
124000           END-IF                                                         
124100        END-IF                                                            
124200     END-IF                                                               
124300     .                                                                    
124400     EJECT                                                                
124500 CB-KOLLA-DIV-FEL-ORSAKSKOD SECTION.                                      
124600     MOVE '*** CB-KOLLA-DIV-FEL-ORSAKSKOD *** '                           
124700                              TO FELTEXT-STR                              
124800                                                                          
124900*--- GODKÄND ORSAKSKOD                                                    
125000     IF OKOD-FL-GODK-KOD = NEJ                                            
125100        MOVE '712'            TO LINK-IDFELKOD(INDX)                      
125200        MOVE NEJ              TO ALLT-SW                                  
125300        ADD +1                TO INDX                                     
125400        IF LINK-TEMFSINF = SPACE                                          
125500           MOVE 'INVALID APPROVE CODE  '                                  
125600                              TO LINK-TEMFSINF                            
125700        END-IF                                                            
125800     END-IF                                                               
125900                                                                          
126000*--- ORSAKSKOD/EMBKOD                                                     
126100     IF OKOD-FL-EMBLEV = JA                                               
126200        IF LINK-KDEMBLEV = +1 OR +2 OR +3 OR +4 OR +5 OR +6               
126300           CONTINUE                                                       
126400        ELSE                                                              
126500           MOVE '713'         TO LINK-IDFELKOD(INDX)                      
126600           MOVE NEJ           TO ALLT-SW                                  
126700           ADD +1             TO INDX                                     
126800           IF LINK-TEMFSINF = SPACE                                       
126900             MOVE 'WRONG EMB CODE  '                                      
127000                              TO LINK-TEMFSINF                            
127100           END-IF                                                         
127200        END-IF                                                            
127300     END-IF                                                               
127400                                                                          
127500*--- E-TRACKER 1658417 2006-03-01                                         
127600*--- GODKÄND ORSAKSKOD 25,26,27 OCH 28 BARA FÖR FTG=57, EJ USA/CAN        
127700*--- GODKÄND ORSAKSKOD FÖR KINA - SE E'TRACKER 10143271                   
127800*--- GÄLLER ÄVEN INDIEN.                                                  
127900     IF LINK-KDANMORS = '25' OR '26' OR '27' OR '28'                      
128000       IF DIST07-USA-RET-DISCR  OR                                        
128100          DIST07-CAN-RET-DISCR  OR                                        
128200          DIST35-REFILL-NA-JAP  OR                                        
128300          DIST35-REFILL-NA      OR                                        
128400          DIST35-REFILL-CN      OR                                        
128500          DIST35-CDC-IN-REFILL  OR                                        
128600          DIST35-CDC-KR-REFILL  OR                                        
128700          DIST35-CDC-TR-REFILL  OR                                        
128710          DIST35-CDC-ZA-REFILL  OR                                        
128800          DIST35-CDC-MX-REFILL  OR                                        
128900          DIST35-CDC-BR-REFILL  OR                                        
129000          DIST35-CDC-AE-REFILL  OR                                        
129100          DIST35-CDC-MY-REFILL  OR                                        
129200          DIST35-CDC-TH-REFILL  OR                                        
129300          DIST35-CDC-TW-REFILL  OR                                        
129400          DIST35-NONVCC-NONVCC-REFILL OR                                  
129500          DIST35-NONVCC-NONVCC-TRANSFER OR                                
129600*---      NOT IDFTG-PV                                                    
129700          IDFTG-US              OR                                        
129800          IDFTG-CA                                                        
129900                                                                          
130000          MOVE '712'            TO LINK-IDFELKOD(INDX)                    
130100          MOVE NEJ              TO ALLT-SW                                
130200          ADD +1                TO INDX                                   
130300          IF LINK-TEMFSINF= SPACE                                         
130400            MOVE 'CODE NOT ALLOWED WITH DISTRICT'                         
130500                                TO LINK-TEMFSINF                          
130600          END-IF                                                          
130700       END-IF                                                             
130800     END-IF                                                               
130900                                                                          
131000*--- GODKÄND ORSAKSKOD 74 BARA FÖR DISTRIKT < 100 OCH N-FAKTURA           
131100     IF LINK-KDANMORS = '74'                                              
131200       IF LINK-IDDISTR < 100    AND                                       
131300          LINK-KDFAKTYP = 'N'                                             
131400                                                                          
131500          CONTINUE                                                        
131600       ELSE                                                               
131700                                                                          
131800          MOVE '712'            TO LINK-IDFELKOD(INDX)                    
131900          MOVE NEJ              TO ALLT-SW                                
132000          ADD +1                TO INDX                                   
132100          IF LINK-TEMFSINF= SPACE                                         
132200            MOVE 'DIST/INVOICE TYPE NOT ALLOWED WITH 74'                  
132300                                TO LINK-TEMFSINF                          
132400          END-IF                                                          
132500       END-IF                                                             
132600     END-IF                                                               
132700                                                                          
132800*--- OBLIGATORISKA FÄLT FÖR DDI-DISTRIKT FRÅN VIPS.                       
132900     IF LINK-IDPTYP = '01C' AND DIST79-DEALER-PRICE                       
133100       IF LINK-KDVAT = SPACE                                              
133200          MOVE '709'            TO LINK-IDFELKOD(INDX)                    
133300          MOVE NEJ              TO ALLT-SW                                
133400          ADD +1                TO INDX                                   
133500          IF LINK-TEMFSINF= SPACE                                         
133600            MOVE 'PART IS TORAS PART  '                                   
133700                                TO LINK-TEMFSINF                          
133800          END-IF                                                          
133900       END-IF                                                             
134000       IF LINK-KDVALISO = SPACE                                           
134100          MOVE '707'            TO LINK-IDFELKOD(INDX)                    
134200          MOVE NEJ              TO ALLT-SW                                
134300          ADD +1                TO INDX                                   
134400          IF LINK-TEMFSINF= SPACE                                         
134500            MOVE 'CURRENCY MISSING  '                                     
134600                                TO LINK-TEMFSINF                          
134700          END-IF                                                          
134800       END-IF                                                             
134900     END-IF                                                               
135000                                                                          
135100*--- OBLIGATORISKA FÄLT FÖR LOCAL-CURRENCY-DISTRIKT FRÅN VIPS.            
135200     IF LINK-IDPTYP = '01C' AND DIST79-DEALER-PRICE                       
135400       IF LINK-KDVALISO = SPACE                                           
135500          MOVE '707'            TO LINK-IDFELKOD(INDX)                    
135600          MOVE NEJ              TO ALLT-SW                                
135700          ADD +1                TO INDX                                   
135800          IF LINK-TEMFSINF= SPACE                                         
135900            MOVE 'CURRENCY MISSING  '                                     
136000                                TO LINK-TEMFSINF                          
136100          END-IF                                                          
136200       END-IF                                                             
136300     END-IF                                                               
136400                                                                          
136500*--- ORSAKSKOD SOM GER KREDIT/TF ÄR BARA OK OM KUNDEN ÄR GODKÄND          
136600*--- FÖR R-FAKTURERING.                                                   
136700     IF LINK-KDANMORS = '90' OR '92' OR '93' OR                           
136800                        '11' OR '21' OR '26'                              
136900       IF WDB201-FINNS AND (GMT-FLOKFAK-R = 'J' OR 'Y')                   
137000                                                                          
137100         CONTINUE                                                         
137200       ELSE                                                               
137300         MOVE '712'            TO LINK-IDFELKOD(INDX)                     
137400         MOVE NEJ              TO ALLT-SW                                 
137500         ADD +1                TO INDX                                    
137600         IF LINK-TEMFSINF= SPACE                                          
137700           MOVE 'WRONG CODE USED WITH INVOICE TYPE'                       
137800                               TO LINK-TEMFSINF                           
137900         END-IF                                                           
138000       END-IF                                                             
138100     END-IF                                                               
138200                                                                          
138300*--- E'TR 8258308 KOD 72 ÄR INTE TILLÅTET FÖR VOR ORDER KLASS=0           
138400*--- ALLA DISTRIKT                                                        
138500     IF LINK-KDANMORS = '72'                                              
138600       IF WDL5-FINNS AND (FAKC-KDORDKL = +0)                              
138700                                                                          
138800         MOVE '712'            TO LINK-IDFELKOD(INDX)                     
138900         MOVE NEJ              TO ALLT-SW                                 
139000         ADD +1                TO INDX                                    
139100         IF LINK-TEMFSINF= SPACE                                          
139200           MOVE 'INVALID CODE WITH CLASS 0 '                              
139300                               TO LINK-TEMFSINF                           
139400         END-IF                                                           
139500       END-IF                                                             
139600     END-IF                                                               
139700                                                                          
139800     .                                                                    
139900     EJECT                                                                
140000 CC-KOLLA-ARTIKEL SECTION.                                                
140100     MOVE '*** CC-KOLLA-ARTIKEL *** '                                     
140200                              TO FELTEXT-STR                              
140300                                                                          
140400*--- ARTIKELNUMMER                                                        
140500     IF WLARTC01-SAKNAS                                                   
140600        MOVE '708'            TO LINK-IDFELKOD(INDX)                      
140700        MOVE NEJ              TO ALLT-SW                                  
140800        ADD +1                TO INDX                                     
140900        IF LINK-TEMFSINF= SPACE                                           
141000           MOVE 'UNKOWN PART  '                                           
141100                              TO LINK-TEMFSINF                            
141200        END-IF                                                            
141300     ELSE                                                                 
141400        PERFORM CCA-KOLLA-ARTIKEL                                         
141500     END-IF                                                               
141600     .                                                                    
141700     EJECT                                                                
141800 CCA-KOLLA-ARTIKEL SECTION.                                               
141900     MOVE '*** CCA-KOLLA-ARTIKEL *** '                                    
142000                              TO FELTEXT-STR                              
142100                                                                          
142200*--- EJ UTGÅNGEN ARTIKEL OCH C-LAGER SEGMENT SAKNAS                       
142300     IF ART-KDERS-UTG = 0                                                 
142400        MOVE LINK-IDDC        TO WS-IDDC                                  
142500        IF CDC  OR GOOD-DDC                                               
142600           IF WLARTC11-SAKNAS                                             
142700              MOVE '742'      TO LINK-IDFELKOD(INDX)                      
142800              MOVE NEJ        TO ALLT-SW                                  
142900              ADD +1          TO INDX                                     
143000              IF LINK-TEMFSINF= SPACE                                     
143100                MOVE 'WRONG WAREHOUSE  '                                  
143200                              TO LINK-TEMFSINF                            
143300              END-IF                                                      
143400           END-IF                                                         
143500        END-IF                                                            
143600     END-IF                                                               
143700                                                                          
143800*--- UTGÅNGEN ARTIKEL                                                     
143900     IF ART-KDERS-UTG > 0                                                 
144000        MOVE '708'            TO LINK-IDFELKOD(INDX)                      
144100        MOVE NEJ              TO ALLT-SW                                  
144200        ADD +1                TO INDX                                     
144300        IF LINK-TEMFSINF= SPACE                                           
144400           MOVE 'UNKOWN PART  '                                           
144500                              TO LINK-TEMFSINF                            
144600       END-IF                                                             
144700     END-IF                                                               
144800                                                                          
144900*--- EJ MÖJLIGT LEVANM. ART.NR 1 SOM HAR ERS.KOD=52 OCH SAKNAR            
145000*--- BÅDE STD-/SJK-PRIS.VID UPPREPADE TILLFÄLLEN HAR VIPS FEL-            
145100*--- AKTIGT SKICKAT IN ART.NR 1 ISTÄLLET FÖR 100. DETTA SKAPAR            
145200*--- PROBLEM HOS EKONOMI ETC. I PULS. I VÄNTAN PÅ ATT VIPS RÄTTAR         
145300*--- TILL DETTA FEL SÅ HAR VI DENNA FIX I PGM.                            
145400     IF LINK-IDARTNR = +1                                                 
145500        MOVE '710'          TO LINK-IDFELKOD(INDX)                        
145600        MOVE NEJ            TO ALLT-SW                                    
145700        ADD +1              TO INDX                                       
145800        IF LINK-TEMFSINF = ' '                                            
145900          MOVE 'EXPIRED PART  '                                           
146000                            TO LINK-TEMFSINF                              
146100        END-IF                                                            
146200     END-IF                                                               
146300                                                                          
146400*--- ÄNDRING 051125 PGA STOPPADE TRANSAR EKONOMI/PRIS FLERA GÅNGER        
146500*--- EJ MÖJLIGT LEVANM. ART.NR SOM HAR ERS.KOD=52 OCH SAKNAR              
146600*--- BÅDE STD-/SJK-PRIS.                                                  
146700     IF WLARTC11-FINNS                                                    
146800       IF CLAG-KDERS = +52                                                
146900          MOVE '710'        TO LINK-IDFELKOD(INDX)                        
147000          MOVE NEJ          TO ALLT-SW                                    
147100          ADD +1            TO INDX                                       
147200        IF LINK-TEMFSINF= SPACE                                           
147300           MOVE 'EXPIRED PART  '                                          
147400                            TO LINK-TEMFSINF                              
147500        END-IF                                                            
147600       END-IF                                                             
147700     END-IF                                                               
147800                                                                          
147900*--- ÄNDRING 060717 PGA STOPPADE TRANSAR EKONOMI/PRIS FLERA GÅNGER        
148000*--- EJ MÖJLIGT LEVANM. ART.NR SOM HAR ERS.KOD=21 OCH SAKNAR              
148100*--- BÅDE STD-/SJK-PRIS.                                                  
148200     IF WLARTC11-FINNS                                                    
148300       IF CLAG-KDERS = +21 AND                                            
148400          CLAG-PRARTSTD = 0 AND                                           
148500          CLAG-PRARTSJK = 0                                               
148600          MOVE '710'        TO LINK-IDFELKOD(INDX)                        
148700          MOVE NEJ          TO ALLT-SW                                    
148800          ADD +1            TO INDX                                       
148900          IF LINK-TEMFSINF= SPACE                                         
149000            MOVE 'EXPIRED PART  '                                         
149100                            TO LINK-TEMFSINF                              
149200         END-IF                                                           
149300       END-IF                                                             
149400     END-IF                                                               
149500                                                                          
149600*--- EJ MÖJLIGT LEVANM. OBJEKT AV DÅLIG KVALITET                          
149700     IF OKOD-FL-KVALITET-AVVIK = JA                                       
149800         MOVE LINK-IDARTNR     TO TEST-IDARTNR                            
149900         IF BYT09-OBJEKT                                                  
150000           MOVE '767'          TO LINK-IDFELKOD(INDX)                     
150100           MOVE NEJ            TO ALLT-SW                                 
150200           ADD +1              TO INDX                                    
150300          IF LINK-TEMFSINF= SPACE                                         
150400            MOVE 'POOR QUALITY PART  '                                    
150500                               TO LINK-TEMFSINF                           
150600          END-IF                                                          
150700        END-IF                                                            
150800     END-IF                                                               
150900                                                                          
151000*--- DUMMY-ARTIKELNUMMER SKALL HA ORSAKSKOD 22                            
151100     IF LINK-IDARTNR = DUMMY-IDARTNR                                      
151200        IF LINK-KDANMORS NOT = '22'                                       
151300          MOVE '769'          TO LINK-IDFELKOD(INDX)                      
151400          MOVE NEJ            TO ALLT-SW                                  
151500          ADD +1              TO INDX                                     
151600          IF LINK-TEMFSINF= SPACE                                         
151700            MOVE 'DUMMY PART NO  '                                        
151800                               TO LINK-TEMFSINF                           
151900          END-IF                                                          
152000        END-IF                                                            
152100                                                                          
152200        IF DIST79-DEALER-PRICE OR                                         
152400           DIST79-ECOM-PRICE                                              
152500          IF LINK-PRARTBTO-LOC > ZERO                                     
152600            MOVE '762'      TO LINK-IDFELKOD(INDX)                        
152700            MOVE NEJ        TO ALLT-SW                                    
152800            ADD +1          TO INDX                                       
152900            IF LINK-TEMFSINF= SPACE                                       
153000               MOVE 'NO PRICE INPUT  '                                    
153100                            TO LINK-TEMFSINF                              
153200          END-IF                                                          
153300          END-IF                                                          
153400        ELSE                                                              
153500          IF LINK-PRARTBTO     > ZERO                                     
153600            MOVE '762'      TO LINK-IDFELKOD(INDX)                        
153700            MOVE NEJ        TO ALLT-SW                                    
153800            ADD +1          TO INDX                                       
153900            IF LINK-TEMFSINF= SPACE                                       
154000              MOVE 'NO PRICE INPUT  '                                     
154100                            TO LINK-TEMFSINF                              
154200            END-IF                                                        
154300          END-IF                                                          
154400        END-IF                                                            
154500     END-IF                                                               
154600                                                                          
154700*--- SOFTVARA-ARTIKELNUMMER SKALL HA ORSAKSKOD 70 ELLER 30                
154800     IF ART-KDSORT = 'SW'                                                 
154900        IF LINK-KDANMORS = '70' OR '30' OR '31' OR '39'                   
155000          CONTINUE                                                        
155100        ELSE                                                              
155200          MOVE '768'          TO LINK-IDFELKOD(INDX)                      
155300          MOVE NEJ            TO ALLT-SW                                  
155400          ADD +1              TO INDX                                     
155500          IF LINK-TEMFSINF= SPACE                                         
155600             MOVE 'SOFTWARE PARTNO  '                                     
155700                              TO LINK-TEMFSINF                            
155800          END-IF                                                          
155900        END-IF                                                            
156000     ELSE                                                                 
156100        IF LINK-KDANMORS  = '70'                                          
156200          MOVE '768'          TO LINK-IDFELKOD(INDX)                      
156300          MOVE NEJ            TO ALLT-SW                                  
156400          ADD +1              TO INDX                                     
156500          IF LINK-TEMFSINF= SPACE                                         
156600            MOVE 'SOFTWARE PARTNO  '                                      
156700                              TO LINK-TEMFSINF                            
156800          END-IF                                                          
156900        END-IF                                                            
157000     END-IF                                                               
157100                                                                          
157200*--- STANDARD-/SJÄLVKOST-PRIS SKALL UPPDATERAS OM DET ÄR EN DDI-          
157300*--- MARKNAD ( FAKT. DEALERN DIREKT VIA BILL-IT I LOKAL VALUTA ).         
157400*--- OM BÅDA FÄLTEN SAKNAS FRÅN VIPS - HÄMTAS PRISER.DET ÄR OK OM         
157500*--- VIPS BARA SKICKAR MED DET ENA. FRÅN 4704 HÄMTAS ALLTID BÅDA.         
157600*--- VID LOKAL CURRENCY SKALL PRISET VARA I MB VALUTA.STDPRIS             
157700*--- HÄMTAS FRÅN K6 / SJKPRIS FRÅN W335COST ( ÅRS-/MÅN-KURS)              
157800*--- NY SJÄLVKOST PGA CENTRAL PRICING 2004-03-09                          
157900                                                                          
158000     IF DIST79-DEALER-PRICE                                               
158200       IF WLARTC11-SAKNAS                                                 
158300         CONTINUE                                                         
158400       ELSE                                                               
158500         IF (LINK-PRARTSTD = 0) AND (LINK-PRARTSJK = 0)                   
158600                                                                          
158700           MOVE CLAG-PRARTSTD      TO LINK-PRARTSTD                       
158800                                                                          
158900           MOVE '11'               TO COST-IDDC                           
159000           MOVE LINK-IDARTNR       TO COST-IDARTNR                        
159100           MOVE FUNCTION CURRENT-DATE(5:2) TO COST-TIMM                   
159200           CALL W335COST USING COST-W335COST COST-WDK6-PCB                
159300                                             COST-WDK7-PCB                
159400                                             COST-WDF1-PCB                
159500                                             COST-9305-PCB                
159600                                             COST-WDK72-PCB               
159700                                             COST-WDB6-PCB                
159800           MOVE COST-PRARTSJK-MON  TO LINK-PRARTSJK                       
159900                                                                          
160000           PERFORM S01-HAMTA-MARK-BOLAG-VALUTA                            
160100         END-IF                                                           
160200       END-IF                                                             
160300     END-IF                                                               
160400                                                                          
160500                                                                          
160600*--- OBS ! TILLFÄLL.LÖSNING I VÄNTAN PÅ 1 FAKT/DC I LDC-PROJEKTET         
160700*--- FIX FÖR SVERIGE 778 PGA SAMFAKT 11/SE OCH FEL DC FRÅN VIPS.          
160800*--- EJ DIRLEV-ARTIKEL (DDGS=SE) OCH DC=11 / CDC-ART OCH DC-SE !!         
160900     IF LINK-KDANMORS = '21' OR '22' OR '23' OR '80' OR                   
161000                        '26' OR '27' OR '28'                              
161100       IF WLARTC11-SAKNAS                                                 
161200         CONTINUE                                                         
161300       ELSE                                                               
161400         MOVE LINK-IDDC         TO WS-IDDC                                
161500         MOVE LINK-IDDISTR      TO TEST-IDDISTR                           
161600                                                                          
161700         IF DIST03-SVERIGE                                                
161800           IF CLAG-REDIRLEV = ZERO AND DDC-SE                             
161900             MOVE WC-CDC-SE  TO LINK-IDDC                                 
162000           END-IF                                                         
162100                                                                          
162200           MOVE ART-IDLEVNR TO LEV06-IDLEVNR                              
162300           IF CLAG-REDIRLEV = 1.00 AND CDC-SE AND LEV06-DDGS              
162400               MOVE WC-DDC-SE  TO LINK-IDDC                               
162500           END-IF                                                         
162600         END-IF                                                           
162700       END-IF                                                             
162800     END-IF                                                               
162900     .                                                                    
163000     EJECT                                                                
163100 CD-KOLLA-ANTAL-PRIS SECTION.                                             
163200     MOVE '*** CD-KOLLA-ANTAL-PRIS *** '                                  
163300                              TO FELTEXT-STR                              
163400                                                                          
163500*--- ANTAL                                                                
163600     IF OKOD-FL-ANT-LEVANM = JA                                           
163700        IF LINK-KVLEVANM = +0                                             
163800          MOVE '736'          TO LINK-IDFELKOD(INDX)                      
163900          MOVE NEJ            TO ALLT-SW                                  
164000          ADD +1              TO INDX                                     
164100          IF LINK-TEMFSINF= SPACE                                         
164200            MOVE 'QTY IS ZERO  '                                          
164300                              TO LINK-TEMFSINF                            
164400          END-IF                                                          
164500        END-IF                                                            
164600     END-IF                                                               
164700                                                                          
164800*--- PRIS                                                                 
164900     IF OKOD-FL-GODK-PRIS-ZERO = NEJ                                      
165000       IF DIST79-DEALER-PRICE                                             
165200         IF LINK-PRARTBTO-LOC  = ZERO                                     
165300           MOVE '714'          TO LINK-IDFELKOD(INDX)                     
165400           MOVE NEJ            TO ALLT-SW                                 
165500           ADD +1              TO INDX                                    
165600           IF LINK-TEMFSINF= SPACE                                        
165700             MOVE 'PRICE IS MISSING  '                                    
165800                                TO LINK-TEMFSINF                          
165900           END-IF                                                         
166000         END-IF                                                           
166100       ELSE                                                               
166200         IF LINK-PRARTBTO     = ZERO                                      
166300           MOVE '714'          TO LINK-IDFELKOD(INDX)                     
166400           MOVE NEJ            TO ALLT-SW                                 
166500           ADD +1              TO INDX                                    
166600           IF LINK-TEMFSINF= SPACE                                        
166700          MOVE 'PRICE IS MISSING  '  TO LINK-TEMFSINF                     
166800         END-IF                                                           
166900         END-IF                                                           
167000       END-IF                                                             
167100     END-IF                                                               
167200                                                                          
167300     IF LINK-IDARTNR = DUMMY-IDARTNR                                      
167400        CONTINUE                                                          
167500     ELSE                                                                 
167600       IF OKOD-FL-PRIS-ZERO = JA                                          
167700         IF DIST79-DEALER-PRICE OR                                        
167900            DIST79-ECOM-PRICE                                             
168000          IF LINK-PRARTBTO-LOC > ZERO                                     
168100               MOVE '762'      TO LINK-IDFELKOD(INDX)                     
168200               MOVE NEJ        TO ALLT-SW                                 
168300               ADD +1          TO INDX                                    
168400            IF LINK-TEMFSINF= SPACE                                       
168500               MOVE 'PRICE SHOULD NOT BE ENTERED  '                       
168600                               TO LINK-TEMFSINF                           
168700            END-IF                                                        
168800          END-IF                                                          
168900         ELSE                                                             
169000            IF LINK-PRARTBTO     > ZERO                                   
169100               MOVE '762'      TO LINK-IDFELKOD(INDX)                     
169200               MOVE NEJ        TO ALLT-SW                                 
169300               ADD +1          TO INDX                                    
169400              IF LINK-TEMFSINF= SPACE                                     
169500                 MOVE 'PRICE SHOULD NOT BE ENTERED  '                     
169600                               TO   LINK-TEMFSINF                         
169700              END-IF                                                      
169800            END-IF                                                        
169900         END-IF                                                           
170000       END-IF                                                             
170100     END-IF                                                               
170200                                                                          
170300*- NÄR USA LÄGGER IN REKL. I PULS PÅ DISTR 7580/7680 SÅ KAN INTE          
170400*  FAKTURAN HITTAS , ALLTSÅ MÅSTE MAN ANGE FAKTNR. OCH PRIS !             
170500*-20111018 SAMMA SAK FÖR KINA SOM HAR MOTSVARANDE DIST 6280.              
170600*-ÄVEN INDIEN.                                                            
170700                                                                          
170800     IF  DIST07-USA-RET-DISCR    OR                                       
170900         DIST07-CAN-RET-DISCR    OR                                       
171000         DIST07-KINA-RET-DISCR   OR                                       
171100         DIST07-INDIEN-RET-DISCR OR                                       
171200         DIST07-KOREA-RET-DISCR  OR                                       
171300         DIST07-TURKEY-RET-DISCR OR                                       
171400         DIST07-MALAYSIA-RET-DISCR OR                                     
171500         DIST07-THAILAND-RET-DISCR OR                                     
171600         DIST07-TAIWAN-RET-DISCR   OR                                     
171610         DIST07-S-AFRICA-RET-DISCR OR                                     
171700         DIST07-MEXICO-RET-DISCR   OR                                     
171710         DIST07-BRAZIL-RET-DISCR                                          
171800                                                                          
171900        IF DIST79-DEALER-PRICE                                            
172100          IF LINK-PRARTBTO-LOC = ZERO                                     
172200            IF OKOD-FL-PRIS-ZERO = JA  OR                                 
172300              ( LINK-KDANMORS = '11' ) OR ( LINK-KDANMORS = '21')         
172400              CONTINUE                                                    
172500            ELSE                                                          
172600              MOVE '714'          TO LINK-IDFELKOD(INDX)                  
172700              MOVE NEJ            TO ALLT-SW                              
172800              ADD +1              TO INDX                                 
172900              IF LINK-TEMFSINF = SPACE                                    
173000                MOVE 'PRICE IS MISSING  '                                 
173100                                  TO LINK-TEMFSINF                        
173200              END-IF                                                      
173300            END-IF                                                        
173400          END-IF                                                          
173500        ELSE                                                              
173600          IF LINK-PRARTBTO = ZERO                                         
173700            IF OKOD-FL-PRIS-ZERO = JA  OR                                 
173800              ( LINK-KDANMORS = '11' ) OR ( LINK-KDANMORS = '21')         
173900              CONTINUE                                                    
174000            ELSE                                                          
174100              MOVE '714'          TO LINK-IDFELKOD(INDX)                  
174200              MOVE NEJ            TO ALLT-SW                              
174300              ADD +1              TO INDX                                 
174400              IF LINK-TEMFSINF= SPACE                                     
174500                MOVE 'PRICE IS MISSING  '                                 
174600                                  TO LINK-TEMFSINF                        
174700              END-IF                                                      
174800            END-IF                                                        
174900          END-IF                                                          
175000        END-IF                                                            
175100                                                                          
175200     END-IF                                                               
175300                                                                          
175400*-20120323 VIPS LEV.ANM KINA SOM SAKNAR PRIS ABENDAR I BILL-IT.           
175500*-KINA LEV.ANM SKALL BEHANDLAS LIKADANT SOM FÖR USA.PULS-PRIS             
175600*-(SÄLJBOLAGSPRIS) SKALL KOMMA MED FRÅN VIPS.KOLLA ENLIGT SUSSI           
175700     IF DIST07-KINA   OR                                                  
175800        DIST07-INDIEN OR                                                  
175900        DIST07-KOREA  OR                                                  
176000        DIST07-TURKEY OR                                                  
176010        DIST07-S-AFRICA OR                                                
176100        DIST07-MALAYSIA OR                                                
176200        DIST07-THAILAND OR                                                
176300        DIST07-TAIWAN   OR                                                
176400        DIST07-MEXICO   OR                                                
176410        DIST07-BRAZIL                                                     
176500       IF LINK-KDANMORS = '80'                                            
176600         IF LINK-PRARTBTO = ZERO                                          
176700           MOVE '714'          TO LINK-IDFELKOD(INDX)                     
176800           MOVE NEJ            TO ALLT-SW                                 
176900           ADD +1              TO INDX                                    
177000           IF LINK-TEMFSINF= SPACE                                        
177100             MOVE 'PRICE IS MISSING  '                                    
177200                               TO LINK-TEMFSINF                           
177300           END-IF                                                         
177400         END-IF                                                           
177500       END-IF                                                             
177600     END-IF                                                               
177700     .                                                                    
177800     EJECT                                                                
177900 CE-KOLLA-FAKTURAINFO SECTION.                                            
178000     MOVE '*** CE-KOLLA-FAKTURAINFO *** '                                 
178100                              TO FELTEXT-STR                              
178200                                                                          
178300*--- FAKTURA-TYP                                                          
178400     IF OKOD-FL-KDFAKTYP-R = JA                                           
178500        IF LINK-KDFAKTYP NOT = 'R'                                        
178600           MOVE '716'       TO LINK-IDFELKOD(INDX)                        
178700           MOVE NEJ         TO ALLT-SW                                    
178800           ADD +1           TO INDX                                       
178900           IF LINK-TEMFSINF= SPACE                                        
179000             MOVE 'WRONG INV TYPE  '                                      
179100                               TO LINK-TEMFSINF                           
179200           END-IF                                                         
179300        END-IF                                                            
179400     END-IF                                                               
179500                                                                          
179600*--- FAKTURA-NUMMER                                                       
179700     IF OKOD-FL-IDFAKT = JA                                               
179800        IF LINK-IDFAKT = +0                                               
179900           MOVE '717'       TO LINK-IDFELKOD(INDX)                        
180000           MOVE NEJ         TO ALLT-SW                                    
180100           ADD +1           TO INDX                                       
180200           IF LINK-TEMFSINF= SPACE                                        
180300             MOVE 'INV NOT NUMERIC  '                                     
180400                            TO LINK-TEMFSINF                              
180500           END-IF                                                         
180600        END-IF                                                            
180700     END-IF                                                               
180800                                                                          
180900     IF (LINK-KDFAKTYP = ' ' AND LINK-IDFAKT > +0) OR                     
181000        (LINK-KDFAKTYP > ' ' AND LINK-IDFAKT = +0)                        
181100        MOVE '718'          TO LINK-IDFELKOD(INDX)                        
181200        MOVE NEJ            TO ALLT-SW                                    
181300        ADD +1              TO INDX                                       
181400        IF LINK-TEMFSINF= SPACE                                           
181500          MOVE 'INV NO MISSING  '                                         
181600                            TO LINK-TEMFSINF                              
181700        END-IF                                                            
181800     END-IF                                                               
181900     .                                                                    
182000     EJECT                                                                
182100 CF-KOLLA-ANALYSNR SECTION.                                               
182200     MOVE '*** CF-KOLLA-ANALYSNR *** '                                    
182300                              TO FELTEXT-STR                              
182400                                                                          
182500     IF OKOD-FL-ANALYSNR = JA                                             
182600        IF WL410901-SAKNAS                                                
182700           IF LINK-KDANMORS = '54' OR '55'                                
182800              MOVE JA         TO LINK-FLANLYSF                            
182900              IF LINK-KDANMORS = '55'                                     
183000                MOVE '158600001701' TO LINK-IDANALYS                      
183100                MOVE '481412'       TO LINK-IDKONTO                       
183200                MOVE '57576'        TO LINK-IDKST                         
183300              ELSE                                                        
183400                MOVE '158600001701' TO LINK-IDANALYS                      
183500              END-IF                                                      
183600           ELSE                                                           
183700              MOVE '721'      TO LINK-IDFELKOD(INDX)                      
183800              MOVE NEJ        TO ALLT-SW                                  
183900              ADD +1          TO INDX                                     
184000              IF LINK-TEMFSINF= SPACE                                     
184100                MOVE 'ACCOUNT NO MISSING  '                               
184200                              TO LINK-TEMFSINF                            
184300              END-IF                                                      
184400           END-IF                                                         
184500        ELSE                                                              
184600           IF LINK-KDANMORS = '53' OR                                     
184700              (IDFTG-PV AND LINK-KDANMORS = '55' )                        
184800             MOVE 4110-IDANALYS  TO LINK-IDANALYS                         
184900             MOVE 4110-IDKONTO   TO LINK-IDKONTO                          
185000             MOVE 4110-IDKST     TO LINK-IDKST                            
185100           ELSE                                                           
185200             MOVE 4110-IDANALYS  TO LINK-IDANALYS                         
185300           END-IF                                                         
185400        END-IF                                                            
185500     END-IF                                                               
185600     .                                                                    
185700     EJECT                                                                
185800 CG-KOLLA-FAKTURAINFO SECTION.                                            
185900     MOVE '*** CG-KOLLA-FAKTURAINFO *** '                                 
186000                              TO FELTEXT-STR                              
186100     IF OKOD-FL-IDFAKT = JA                                               
186200        MOVE LINK-IDDISTR  TO TEST-IDDISTR                                
186300        IF WDL5-SAKNAS                                                    
186400                                                                          
186500**-pd-FIX Add rule in PULS to approve turkey claims from VIPS             
186600*  for invoices before 2022-03-01                                         
186700**after this date conversion will be done                                 
186800          IF ((LINK-IDDISTR = 5811)     AND                               
186900             (LINK-TIFAKT   > ZERO)     AND                               
187000             (LINK-TIFAKT   < 0220301))                                   
187100             OR                                                           
187200**MEXICO invoices date 2025-05-30                                         
187300             ((LINK-IDDISTR = 6591)     AND                               
187400             (LINK-TIFAKT   > ZERO)     AND                               
187500             (LINK-TIFAKT   < 0250530))                                   
187501             OR                                                           
187503**BRAZIL invoices date 2026-04-30                                         
187504             ((LINK-IDDISTR = 7051)     AND                               
187505             (LINK-TIFAKT   > ZERO)     AND                               
187506             (LINK-TIFAKT   < 0260430))                                   
187507             OR                                                           
187510**SOUTH AFRICA invoices date 2026-01-30                                   
187520             ((LINK-IDDISTR = 3162)     AND                               
187530             (LINK-TIFAKT   > ZERO)     AND                               
187540             (LINK-TIFAKT   < 0260130))                                   
187600             OR                                                           
187700             ((LINK-IDDISTR = 5627)     AND                               
187800             (LINK-TIFAKT   > ZERO)     AND                               
187900             (LINK-TIFAKT   < 0221028))                                   
188000            CONTINUE                                                      
188100          ELSE                                                            
188200                                                                          
188300            MOVE '730'     TO LINK-IDFELKOD(INDX)                         
188400            MOVE NEJ       TO ALLT-SW                                     
188500            ADD +1         TO INDX                                        
188600            IF LINK-TEMFSINF= SPACE                                       
188700              MOVE 'INVOICE-NO OLD TO CLAIM  '                            
188800                           TO LINK-TEMFSINF                               
188900            END-IF                                                        
189000          END-IF                                                          
189100        ELSE                                                              
189200           MOVE FAK-FLDIRLEV      TO LINK-FLDIRLEV                        
189300           IF LINK-IDPTYP = 'STA'                                         
189400              MOVE FAKC-TIFAKT    TO LINK-TIFAKT                          
189500              MOVE FAK-KDVAT      TO LINK-KDVAT                           
189600                                                                          
189700*- FÖR ATT EJ SKRIVA ÖVER KDVALISO SOM FINNS MED FRÅN 4704 NÄR            
189800*- DET ÄR DIST79-DEALER-PRICE."GAMLA" FAKT HAR KDVALISO = SPACE           
189900              IF LINK-KDVALISO = SPACE                                    
190000                MOVE FAK-KDVALISO TO LINK-KDVALISO                        
190100              END-IF                                                      
190200                                                                          
190300              MOVE FAKL-BEART     TO LINK-BEART-VIPS                      
190400           END-IF                                                         
190500           PERFORM CGA-KOLLA-OVR                                          
190600        END-IF                                                            
190700     ELSE                                                                 
190800       IF DIST79-ECOM-PRICE                                               
190900         CONTINUE                                                         
191000       ELSE                                                               
191100        IF WLARTC01-FINNS                                                 
191200          IF WLARTC11-FINNS                                               
191300            IF OKOD-FL-PRISTILLAEMPA = JA                                 
191400              IF DIST79-DEALER-PRICE                                      
191500                IF LINK-PRARTBTO-LOC = ZERO                               
191600                  MOVE JA     TO LINK-FLPRQUES                            
191700                END-IF                                                    
191800              ELSE                                                        
191900                 IF LINK-PRARTBTO = ZERO                                  
192000                   PERFORM CGAA-PRISTILLAEMPA                             
192100                 END-IF                                                   
192200              END-IF                                                      
192300                                                                          
192400            END-IF                                                        
192500          END-IF                                                          
192600        END-IF                                                            
192700       END-IF                                                             
192800     END-IF                                                               
192900                                                                          
193000     IF OKOD-FL-IDFAKT = JA                                               
193100       IF WDL5-FINNS                                                      
193200         MOVE LINK-IDDISTR     TO TEST-IDDISTR                            
193300                                                                          
193400*---- KOLLA SÅ ATT ANGIVET DC STÄMMER MED ANGIVEN FAKTURA.                
193500         IF LINK-IDDC = FAK-IDDC       OR                                 
193600            DIST35-NA-CDC-RETURN       OR                                 
193700            DIST35-CDC-RETURNS-NON-VCC OR                                 
193800            DIST18-SCRAP-NDC-QUAL                                         
193900                                                                          
194000            CONTINUE                                                      
194100         ELSE                                                             
194200            IF DIST03-SVERIGE                                             
194300              MOVE FAK-IDDC      TO LINK-IDDC                             
194400            ELSE                                                          
194500              MOVE '704'         TO LINK-IDFELKOD(INDX)                   
194600              MOVE NEJ           TO ALLT-SW                               
194700              ADD +1             TO INDX                                  
194800              IF LINK-TEMFSINF= SPACE                                     
194900               MOVE 'WRONG WAREHOUSE  '                                   
195000                                 TO LINK-TEMFSINF                         
195100              END-IF                                                      
195200            END-IF                                                        
195300         END-IF                                                           
195400       END-IF                                                             
195500     END-IF                                                               
195600                                                                          
195700*- LAGT TILL KONTROLL SÅ MAN EJ KAN LÄGGA IN G-FAKTURA-NR MED             
195800*- KDFAKTYP = R                                                           
195900     IF OKOD-FL-IDFAKT = JA                                               
196000       IF WDL5-FINNS                                                      
196100         IF LINK-KDFAKTYP  = FAK-KDFAKTYP                                 
196200           CONTINUE                                                       
196300         ELSE                                                             
196400            MOVE '716'         TO LINK-IDFELKOD(INDX)                     
196500            MOVE NEJ           TO ALLT-SW                                 
196600            ADD +1             TO INDX                                    
196700            IF LINK-TEMFSINF= SPACE                                       
196800              MOVE 'WRONG INV TYPE  '                                     
196900                               TO LINK-TEMFSINF                           
197000           END-IF                                                         
197100         END-IF                                                           
197200       END-IF                                                             
197300     END-IF                                                               
197400                                                                          
197500*- LAGT TILL KONTROLL SÅ MAN EJ KAN LÄGGA EN LEV.ANM. MED ANNAT           
197600*- DIREKTLEV. DC ÄN DC= BE, SE, NO . KONRADARTIKLAR DC=DE SKALL EJ        
197700*- REKLAMERAS I PULS, BARA I VIPS ENLIGT BOSSE S, 020404. PULS ÄR         
197800*- EJ KLART FÖR ATT TA HAND OM NYA DDC'R ÄNNU.                            
197900                                                                          
198000     MOVE LINK-IDDC         TO WS-IDDC                                    
198100     IF GOOD-DDC                                                          
198200       IF DDC-SE OR DDC-NO OR DDC-FI OR DDC-BE OR DDC-FR OR DDC-DE        
198300       OR DDC-KR OR DDC-TR OR DDC-HU OR DDC-PL OR DDC-MA OR DDC-GB        
198300       OR DDC-AU                                                          
198400         CONTINUE                                                         
198500       ELSE                                                               
198600         MOVE '704'         TO LINK-IDFELKOD(INDX)                        
198700         MOVE NEJ           TO ALLT-SW                                    
198800         ADD +1             TO INDX                                       
198900         IF LINK-TEMFSINF= SPACE                                          
199000            MOVE 'WRONG WAREHOUSE  '                                      
199100                            TO LINK-TEMFSINF                              
199200         END-IF                                                           
199300       END-IF                                                             
199400     END-IF                                                               
199500                                                                          
199600                                                                          
199700     IF WDL5-FINNS                                                        
199800        MOVE FAKL-IDUSER-PACK TO LINK-IDUSER-PACK                         
199900        MOVE FAKC-KDORDKL     TO LINK-KDORDKL                             
200000     ELSE                                                                 
200100        MOVE SPACE            TO LINK-IDUSER-PACK                         
200200        MOVE +9               TO LINK-KDORDKL                             
200300     END-IF                                                               
200400     .                                                                    
200500     EJECT                                                                
200600 CGA-KOLLA-OVR SECTION.                                                   
200700     MOVE '*** CGA-KOLLA-OVR *** '                                        
200800                              TO FELTEXT-STR                              
200900     IF WS-KVLEVART     < LINK-KVLEVANM                                   
201000       IF LINK-KDANMORS = '11' OR '12' OR '13'                            
201100         CONTINUE                                                         
201200       ELSE                                                               
201300         MOVE '733'            TO LINK-IDFELKOD(INDX)                     
201400         MOVE NEJ              TO ALLT-SW                                 
201500         ADD +1                TO INDX                                    
201600         IF LINK-TEMFSINF= SPACE                                          
201700           MOVE 'WRONG QTY  '                                             
201800                               TO LINK-TEMFSINF                           
201900         END-IF                                                           
202000       END-IF                                                             
202100     END-IF                                                               
202200                                                                          
202300     IF OKOD-FL-VALFRITT-PRIS = NEJ                                       
202401       IF DIST79-DEALER-PRICE OR                                          
202600          DIST79-ECOM-PRICE                                               
202700*-- FIX FÖR ATT KLARA LEV.ANM. SOM KOMMER IN EFTER INSTALLATION           
202800*-- AV EN DEALER-NET/PÅ 'GAMLA' FAKTUROR.                                 
202900*-- FIX BÖRJAR.                                                           
203000*        IF ( LINK-IDDISTR = 1958 AND FAKC-TIFAKT < 0080511 )             
203100*                                                                         
203200*          CONTINUE                                                       
203300*        ELSE                                                             
203400*-- FIX SLUTAR.                                                           
203500           IF FAKL-PRARTNTO-LOC NOT = LINK-PRARTBTO-LOC                   
203600             IF OKOD-FL-GODK-PRIS-ZERO = JA                               
203700                AND LINK-PRARTBTO-LOC = ZERO                              
203800               CONTINUE                                                   
203900             ELSE                                                         
204000*- ENLIGT KLAS LIDELL SKALL MAN HÄMTA PULS-PRISET NÄR VIPS-PRISET         
204100*- EJ STÄMMER. BILL-IT FAKTURERAR 4 X SJ.KOST NÄR PRIS SAKNAS MEN         
204200*- VIPS HAR PRIS SOM FAKTURERAS UT TILL DEALER. FEL I ORDER-PRIS-         
204300*- SÄTTNINGEN I PULS.OBS!!! BARA DEALER-NET-MARKNADER !!!                 
204400*              IF DIST79-DEALER-PRICE OR DIST79-NON-SEK                   
204500*                                                                         
204600               MOVE FAKL-PRARTNTO-LOC TO LINK-PRARTBTO-LOC                
204700             END-IF                                                       
204800           END-IF                                                         
204900*        END-IF                                                           
205000       ELSE                                                               
205100         IF FAKL-PRARTNTO NOT = LINK-PRARTBTO                             
205200           IF OKOD-FL-GODK-PRIS-ZERO = JA                                 
205300              AND LINK-PRARTBTO = ZERO                                    
205400             CONTINUE                                                     
205500           ELSE                                                           
205600*- CR 8735608 091215 / FAKTURANS PRIS GÄLLER FÖR KOD 72.                  
205700             IF LINK-KDANMORS = '72'                                      
205800               MOVE FAKL-PRARTNTO     TO LINK-PRARTBTO                    
205900             ELSE                                                         
206000               IF WS-PRARTBTO > LINK-PRARTBTO                             
206100                  CONTINUE                                                
206200               ELSE                                                       
206300                  MOVE '734'    TO LINK-IDFELKOD(INDX)                    
206400                  MOVE NEJ      TO ALLT-SW                                
206500                  ADD +1        TO INDX                                   
206600                  IF LINK-TEMFSINF= SPACE                                 
206700                    MOVE 'WRONG PRICE  '                                  
206800                                TO LINK-TEMFSINF                          
206900                  END-IF                                                  
207000               END-IF                                                     
207100             END-IF                                                       
207200           END-IF                                                         
207300         END-IF                                                           
207400       END-IF                                                             
207500     END-IF                                                               
207600                                                                          
207700*- FÖR ATT PRISTILLÄMPA FÖR KOD 11 ,DÄR FAKT. KRÄVS PGA DDGS.             
207800     IF LINK-KDANMORS = '11'                                              
207900       IF WLARTC01-FINNS                                                  
208000         IF WLARTC11-FINNS                                                
208100           IF OKOD-FL-PRISTILLAEMPA = JA                                  
208200             IF DIST79-DEALER-PRICE                                       
208300               IF LINK-PRARTBTO-LOC = ZERO                                
208400                 MOVE JA    TO LINK-FLPRQUES                              
208500               END-IF                                                     
208600             ELSE                                                         
209200               IF DIST79-ECOM-PRICE                                       
209300                 MOVE FAKL-PRARTNTO-LOC TO LINK-PRARTBTO-LOC              
209400               ELSE                                                       
209500                 IF LINK-PRARTBTO = ZERO                                  
209600                   PERFORM CGAA-PRISTILLAEMPA                             
209700                 END-IF                                                   
209800               END-IF                                                     
210000             END-IF                                                       
210100           END-IF                                                         
210200         END-IF                                                           
210300       END-IF                                                             
210400     END-IF                                                               
210500                                                                          
210600     IF OKOD-FL-HAEMTA-PRIS-FAKTURA = JA                                  
210700         IF DIST79-DEALER-PRICE OR                                        
210900            DIST79-ECOM-PRICE                                             
211000                                                                          
211100*-- FIX FÖR ATT KLARA LEV.ANM. SOM KOMMER IN EFTER INSTALLATION           
211200*-- AV EN DEALER-NET-MARKNAD PÅ 'GAMLA' FAKTUROR.                         
211300*-- FIX BÖRJAR.                                                           
211400                                                                          
211500         IF ( LINK-IDDISTR = 1958 AND FAKC-TIFAKT < 0080511 )             
211600                                                                          
211700           IF LINK-KDVALISO    = 'SEK'                                    
211800             MOVE FAKL-PRARTNTO          TO LINK-PRARTBTO-LOC             
211900           ELSE                                                           
212000             MOVE LINK-KDVALISO     TO CURR-KDVALISO-ROW                  
212100                                                                          
212200** LÄS PRKURS HTYP 9305               *****                               
212300             CALL W510CURR USING CURR-W510CURR 9305-PCB                   
212400             IF CURR-KDSVAR = ' '                                         
212500               CONTINUE                                                   
212600             ELSE                                                         
212700               MOVE 1               TO CURR-PRKURS-NEW                    
212800             END-IF                                                       
212900             COMPUTE LINK-PRARTBTO-LOC ROUNDED =                          
213000                FAKL-PRARTNTO / CURR-PRKURS-NEW                           
213100             END-COMPUTE                                                  
213200           END-IF                                                         
213300         ELSE                                                             
213400*-- FIX SLUTAR.                                                           
213500           IF LINK-PRARTBTO-LOC = ZERO                                    
213600             MOVE FAKL-PRARTNTO-LOC      TO LINK-PRARTBTO-LOC             
213700           END-IF                                                         
213800           IF DIST79-ECOM-PRICE                                           
213900             MOVE FAK-KDVALISO          TO LINK-KDVALISO                  
214000           END-IF                                                         
214100         END-IF                                                           
214200       ELSE                                                               
214300         IF LINK-PRARTBTO = ZERO                                          
214400            MOVE FAKL-PRARTNTO          TO LINK-PRARTBTO                  
214500         END-IF                                                           
214600       END-IF                                                             
214700     END-IF                                                               
214800                                                                          
214900                                                                          
215000     .                                                                    
215100     EJECT                                                                
215200 CGAA-PRISTILLAEMPA SECTION.                                              
215300     MOVE '*** CGAA-PRISTILLAEMPA *** '                                   
215400                              TO FELTEXT-STR                              
215500                                                                          
215600     MOVE 1                         TO PRIS-KDCALL                        
215700     MOVE 'W418KTL1'                TO PRIS-IDPGM                         
215800     MOVE LINK-IDARTNR              TO PRIS-IDARTNR                       
215900     MOVE LINK-IDDISTR              TO PRIS-IDDISTR                       
216000     MOVE LINK-IDKUNDNR             TO PRIS-IDKUNDNR                      
216100                                                                          
216200*--- NÄR DET ÄR ALFA-DC SKALL MAN HÄMTA DATA SOM FÖR CDC(DC11).           
216300     MOVE LINK-IDDC                 TO WS-IDDC                            
216400     IF GOOD-DDC                                                          
216500       MOVE '11'                    TO PRIS-IDDC                          
216600     ELSE                                                                 
216700       MOVE LINK-IDDC               TO PRIS-IDDC                          
216800     END-IF                                                               
216900                                                                          
217000     MOVE +4                        TO PRIS-KDORDKL                       
217100     MOVE LINK-KVLEVANM             TO PRIS-KVBEART                       
217200     MOVE NEJ                       TO PRIS-FLINVEST                      
217300                                                                          
217400     CALL W335PRIS USING PRIS-W335PRIS PARTC-PCB                          
217500                                      PWDK7-PCB                           
217600                                      PGMTA-PCB                           
217700                                       BETA-PCB                           
217800                                       PRIA-PCB                           
217900                                       GPRIB-PCB                          
218000                              PRIS-COST-WDK6-PCB                          
218100                              PRIS-COST-WDK7-PCB                          
218200                              PRIS-COST-WDF1-PCB                          
218300                              PRIS-COST-9305-PCB                          
218400                              PRIS-COST-WDK72-PCB                         
218500                              PRIS-COST-WDB6-PCB                          
218600                                                                          
218700     IF PRIS-KDSVAR = SPACE                                               
218800        IF DIST79-DEALER-PRICE                                            
219000          MOVE PRIS-PRARTNTO           TO LINK-PRARTBTO-LOC               
219100          MOVE PRIS-KDVALISO           TO LINK-KDVALISO                   
219200        ELSE                                                              
219300          MOVE PRIS-PRARTNTO           TO LINK-PRARTBTO                   
219400          MOVE PRIS-KDVALISO           TO LINK-KDVALISO                   
219500        END-IF                                                            
219600     ELSE                                                                 
219700        DISPLAY 'W41830: FELAKTIG RETURKOD FRÅN W335PRIS'                 
219800        DISPLAY 'IDDISTR   = ' LINK-IDDISTR                               
219900        DISPLAY 'IDKUNDNR  = ' LINK-IDKUNDNR                              
220000        DISPLAY 'IDRAPPNR  = ' LINK-IDRAPPNR                              
220100        DISPLAY 'IDARTNR   = ' LINK-IDARTNR                               
220200        MOVE ZERO                   TO LINK-PRARTBTO                      
220300                                       LINK-PRARTBTO-LOC                  
220400        MOVE '703'                  TO LINK-IDFELKOD(INDX)                
220500        MOVE NEJ                    TO ALLT-SW                            
220600        ADD +1                      TO INDX                               
220700        IF LINK-TEMFSINF= SPACE                                           
220800            MOVE 'CUSTOMER MISSING  '                                     
220900                                    TO LINK-TEMFSINF                      
221000        END-IF                                                            
221100     END-IF                                                               
221200     .                                                                    
221300     EJECT                                                                
221400 CH-KOLLA-OM-DEN-FINNS-PA-KREE SECTION.                                   
221500     MOVE '*** CH-KOLLA-OM-DEN-FINNS-PA-KREE *** '                        
221600                              TO FELTEXT-STR                              
221700                                                                          
221800*--- SKALL EJ FINNAS PÅ KREDITERING-REGISTRET I STATUS 1                  
221900     IF WLKREE01-FINNS                                                    
222000        IF ANM-KDLEVANM = '0'                                             
222100           CONTINUE                                                       
222200        ELSE                                                              
222300           MOVE '757'         TO LINK-IDFELKOD(INDX)                      
222400           MOVE NEJ           TO ALLT-SW                                  
222500           ADD +1             TO INDX                                     
222600           IF LINK-TEMFSINF= SPACE                                        
222700             MOVE 'DISC NO USED  '                                        
222800                              TO LINK-TEMFSINF                            
222900           END-IF                                                         
223000        END-IF                                                            
223100     END-IF                                                               
223200                                                                          
223300     .                                                                    
223400     EJECT                                                                
223500 CI-KOLLA-TIDS-VAERDEGRAENS SECTION.                                      
223600     MOVE '*** CI-KOLLA-TIDS-VAERDEGRAENS *** '                           
223700                              TO FELTEXT-STR                              
223800                                                                          
223900*-EFTERSOM KOD 72 AUTOMATGODKÄNNS, SÅ KOLLAR MAN EJ TIDSGRÄNSEN.          
224000*-ISTÄLLET SÄTTER SPARTIDEN FÖR FAKTURAHISTORIKEN TIDSGRÄNS.              
224100*-ÄVEN KODERNA 52 OCH 53 AUTOMATGODKÄNNS.                                 
224200                                                                          
224300     iF LINK-KDANMORS = '72' OR '52'  OR '53' OR                          
224400        GOOD-DDC                      OR                                  
224500      ((LINK-KDANMORS = '70' OR '92'  OR '20' OR '21') AND                
224600        (IDFTG-PV                     AND                                 
224700         LINK-IDDC NOT = '61' AND '6A' AND '62'))                         
224800        CONTINUE                                                          
224900     ELSE                                                                 
225000       IF ((LINK-IDPTYP = '01B' OR '01C') AND IDFTG-PV) OR                
225100          ((LINK-IDPTYP = '01B' OR '01C') AND IDFTG-CN) OR                
225200          ((LINK-IDPTYP = '01B' OR '01C') AND IDFTG-IN) OR                
225300          ((LINK-IDPTYP = '01B' OR '01C') AND IDFTG-KR) OR                
225400          ((LINK-IDPTYP = '01B' OR '01C') AND IDFTG-TR) OR                
225500          ((LINK-IDPTYP = '01B' OR '01C') AND IDFTG-MX) OR                
225600          ((LINK-IDPTYP = '01B' OR '01C') AND IDFTG-MY) OR                
225700          ((LINK-IDPTYP = '01B' OR '01C') AND IDFTG-TH) OR                
225800          ((LINK-IDPTYP = '01B' OR '01C') AND IDFTG-TW) OR                
225810          ((LINK-IDPTYP = '01B' OR '01C') AND IDFTG-ZA)                   
225900          IF OKOD-FL-TIDSGRAENS = JA                                      
226000             PERFORM CIA-KOLLA-TIDSGRAENS                                 
226100          END-IF                                                          
226200       END-IF                                                             
226300     END-IF                                                               
226400                                                                          
226500     IF (LINK-IDPTYP = '01C') AND                                         
226600        (IDFTG-PV OR IDFTG-CN OR IDFTG-IN OR IDFTG-KR OR IDFTG-TR         
226700                  OR IDFTG-MY OR IDFTG-TH OR IDFTG-TW OR IDFTG-MX         
226710                  OR IDFTG-ZA)                                            
226800       PERFORM CIB-KOLLA-VAERDEGRAENS                                     
226900       IF LINK-MINI-SUMMA > ZERO                                          
227000         PERFORM CIC-LAES-TP8GRET                                         
227100       END-IF                                                             
227200     ELSE                                                                 
227300       IF LINK-IDPTYP = 'STA'                                             
227400         PERFORM CIB-KOLLA-VAERDEGRAENS                                   
227500         IF LINK-MINI-SUMMA > ZERO                                        
227600           PERFORM CIC-LAES-TP8GRET                                       
227700         END-IF                                                           
227800       END-IF                                                             
227900     END-IF                                                               
228000                                                                          
228100     .                                                                    
228200     EJECT                                                                
228300 CIA-KOLLA-TIDSGRAENS SECTION.                                            
228400     MOVE '*** CIA-KOLLA-TIDSGRAENS *** '                                 
228500                              TO FELTEXT-STR                              
228600******************************************************************        
228700*    KONTROLLERAR ANTAL ARBETSDAGAR MELLAN                       *        
228800*    UTFÄRDANDEDATUM OCH FAKTURADATUM                            *        
228900*   (OM TIDSGRÄNS ÄR ÖVERSKRIDEN GENERERAS                       *        
229000*    FELMEDDELANDE R72 MEN POSTEN AVVISAS INTE)                  *        
229100******************************************************************        
229200     MOVE 'AAMMDD'            TO DAT-KDDATFORM                            
229300     MOVE LINK-TILEVANM       TO DAT-I-TIDATUM                            
229400     CALL WDATKONV USING DAT-KDDATFORM                                    
229500                         DAT-I-TIDATUM                                    
229600                         DAT-O-TIDATUM                                    
229700                         DAT-KDSVAR                                       
229800     IF DAT-KDSVAR-OK                                                     
229900        MOVE DAT-TIAADDD      TO W-TILEVANM-NUM (3:5)                     
230000        MOVE DAT-TISEKEL      TO W-TILEVANM-NUM (1:2)                     
230100     END-IF                                                               
230200                                                                          
230300     MOVE LINK-TIFAKT         TO DAT-I-TIDATUM                            
230400     CALL WDATKONV USING DAT-KDDATFORM                                    
230500                         DAT-I-TIDATUM                                    
230600                         DAT-O-TIDATUM                                    
230700                         DAT-KDSVAR                                       
230800     IF DAT-KDSVAR-OK                                                     
230900        MOVE DAT-TIAADDD      TO W-TIFAKT-NUM (3:5)                       
231000        MOVE DAT-TISEKEL      TO W-TIFAKT-NUM (1:2)                       
231100     END-IF                                                               
231200                                                                          
231300     COMPUTE W-KVAAR     = W-TILEVANM-AAAA  - W-TIFAKT-AAAA               
231400     COMPUTE W-TIDSGRANS = W-TILEVANM-NUM - W-TIFAKT-NUM -                
231500                          (W-KVAAR * 635)                                 
231600                                                                          
231700*                                                                         
231800*---BORTSTJÄRNAD HÖGRE TIDSGRÄNS BRUKAR GÄLLA UNDER SEMESTRAR             
231900*                                                                         
232000     MOVE LINK-IDDISTR        TO TEST-IDDISTR                             
232100     IF DIS104-60-DAGAR                                                   
232200*                                                                         
232300*CO* GÄLLER UNDER SEMESTER                                                
232400*      IF LINK-IDDISTR = 778                                              
232500*        CONTINUE                                                         
232600*      ELSE                                                               
232700*                                                                         
232800                                                                          
232900        IF W-TIDSGRANS > +60  OR                                          
233000           W-TIDSGRANS < +0                                               
233100*       IF W-TIDSGRANS > +120 OR                                          
233200*          W-TIDSGRANS < +0                                               
233300           MOVE 'R72'         TO LINK-KDKREBEH                            
233400        END-IF                                                            
233500*      END-IF                                                             
233600     ELSE                                                                 
233700        IF DIS104-90-DAGAR                                                
233800           IF W-TIDSGRANS > +90 OR                                        
233900              W-TIDSGRANS < +0                                            
234000*          IF W-TIDSGRANS > +200 OR                                       
234100*             W-TIDSGRANS < +0                                            
234200              MOVE 'R72'      TO LINK-KDKREBEH                            
234300           END-IF                                                         
234400        END-IF                                                            
234500     END-IF                                                               
234600     .                                                                    
234700     EJECT                                                                
234800 CIB-KOLLA-VAERDEGRAENS SECTION.                                          
234900     MOVE '*** CIB-KOLLA-VAERDEGRAENS *** '                               
235000                              TO FELTEXT-STR                              
235100******************************************************************        
235200*    KONTROLLERAR ATT VÄRDET INTE UNDERSTIGER MIN-GRÄNSEN        *        
235300*    SOM FINNS UPPDATERAD PER KUND OCH KOD PÅ BILD 4707.         *        
235400*    GER FELMEDDELANDE N73 OCH POSTEN AVVISAS.                   *        
235500*    ENBART VIPS-POSTER SKALL KOLLAS OCH MANUELLA FRÅN 4704.     *        
235600*    MIN-GRÄNSERNA ÄR I SEK.ÄNDRING 200912 ETRACKER 8735608.     *        
235700******************************************************************        
235800                                                                          
235900     MOVE ZERO TO LINK-MINI-SUMMA                                         
236000                                                                          
236100     IF LINK-KDKREBEH = '   '  OR 'R  '                                   
236200       IF DIST79-DEALER-PRICE                                             
236400         IF LINK-PRARTBTO-LOC     > ZERO                                  
236500           IF LINK-KDVALISO = SPACE                                       
236600             CONTINUE                                                     
236700           ELSE                                                           
236800             IF LINK-KDVALISO  = 'SEK'                                    
236900               COMPUTE LINK-MINI-SUMMA = LINK-KVLEVANM *                  
237000                                         LINK-PRARTBTO-LOC                
237100               END-COMPUTE                                                
237200             ELSE                                                         
237300               MOVE LINK-KDVALISO   TO CURR-KDVALISO-ROW                  
237400                                                                          
237500****** LÄS PRKURS HTYP 9305 MÅNADSKURS    *****                           
237600                                                                          
237700               CALL W510CURR USING CURR-W510CURR 9305-PCB                 
237800               IF CURR-KDSVAR = ' '                                       
237900                 CONTINUE                                                 
238000               ELSE                                                       
238100                 MOVE 1               TO CURR-PRKURS-NEW                  
238200               END-IF                                                     
238300               COMPUTE LINK-MINI-SUMMA ROUNDED =                          
238400                 (LINK-PRARTBTO-LOC * CURR-PRKURS-NEW) *                  
238500                                     LINK-KVLEVANM                        
238600               END-COMPUTE                                                
238700             END-IF                                                       
238800           END-IF                                                         
238900         END-IF                                                           
239000       ELSE                                                               
239100         IF DIST79-DEALER-PRICE                                           
239300           IF LINK-PRARTBTO-LOC     > ZERO                                
239400             IF LINK-KDVALISO = SPACE                                     
239500               CONTINUE                                                   
239600             ELSE                                                         
239700               IF LINK-KDVALISO  = 'SEK'                                  
239800                 COMPUTE LINK-MINI-SUMMA = LINK-KVLEVANM *                
239900                                           LINK-PRARTBTO-LOC              
240000                 END-COMPUTE                                              
240100               ELSE                                                       
240200                 MOVE LINK-KDVALISO   TO CURR-KDVALISO-ROW                
240300                                                                          
240400****** LÄS PRKURS HTYP 9305 MÅNADSKURS    *****                           
240500                                                                          
240600                 CALL W510CURR USING CURR-W510CURR 9305-PCB               
240700                 IF CURR-KDSVAR = ' '                                     
240800                   CONTINUE                                               
240900                 ELSE                                                     
241000                   MOVE 1               TO CURR-PRKURS-NEW                
241100                 END-IF                                                   
241200                 COMPUTE LINK-MINI-SUMMA ROUNDED =                        
241300                   (LINK-PRARTBTO-LOC * CURR-PRKURS-NEW) *                
241400                                       LINK-KVLEVANM                      
241500                 END-COMPUTE                                              
241600               END-IF                                                     
241700             END-IF                                                       
241800           END-IF                                                         
241900         ELSE                                                             
242000           IF LINK-PRARTBTO     > ZERO                                    
242100              COMPUTE LINK-MINI-SUMMA = LINK-KVLEVANM *                   
242200                                        LINK-PRARTBTO                     
242300           END-IF                                                         
242400         END-IF                                                           
242500       END-IF                                                             
242600     END-IF                                                               
242700     .                                                                    
242800     EJECT                                                                
242900 CIC-LAES-TP8GRET  SECTION.                                               
243000     MOVE '*** CIC-LAES-TP8GRET *** '                                     
243100                              TO FELTEXT-STR                              
243200                                                                          
243300     MOVE LINK-KDANMORS       TO TP8GRET-KDANMORS                         
243400                                                                          
243500     PERFORM DB2-SELECT-TP8GRET                                           
243600     IF ROW-FOUND                                                         
243700       PERFORM CICA-CHECK-GRET                                            
243800     ELSE                                                                 
243900       MOVE TP8GRET-IDKUNDNR      TO WS-IDKUNDNR                          
244000       MOVE 0                     TO TP8GRET-IDKUNDNR                     
244100       PERFORM DB2-SELECT-TP8GRET                                         
244200       MOVE WS-IDKUNDNR           TO TP8GRET-IDKUNDNR                     
244300       IF ROW-FOUND                                                       
244400          PERFORM CICA-CHECK-GRET                                         
244500       END-IF                                                             
244600     END-IF                                                               
244700     .                                                                    
244800     EJECT                                                                
244900 CICA-CHECK-GRET SECTION.                                                 
245000     MOVE '*** CICA-CHECK-GRET*** '                                       
245100                              TO FELTEXT-STR                              
245200                                                                          
245300     IF TP8GRET-FLINVLDC = 'N'                                            
245400       IF GMT-FLLDCKND = 'N'                                              
245500         IF LINK-MINI-SUMMA < TP8GRET-SUARTBTO-MIN                        
245600            MOVE '773' TO LINK-IDFELKOD(INDX)                             
245700            MOVE NEJ              TO ALLT-SW                              
245800            ADD +1                TO INDX                                 
245900            IF LINK-TEMFSINF= SPACE                                       
246000              MOVE 'MINIMUM LIMIT   '                                     
246100                                  TO LINK-TEMFSINF                        
246200            END-IF                                                        
246300         END-IF                                                           
246400       END-IF                                                             
246500     END-IF                                                               
246600                                                                          
246700     IF TP8GRET-FLINVLDC = 'Y' OR 'J'                                     
246800       IF GMT-FLLDCKND = 'Y' OR 'J'                                       
246900         IF LINK-MINI-SUMMA < TP8GRET-SUARTBTO-LDC-MIN                    
247000            MOVE '773' TO LINK-IDFELKOD(INDX)                             
247100            MOVE NEJ              TO ALLT-SW                              
247200            ADD +1                TO INDX                                 
247300            IF LINK-TEMFSINF= SPACE                                       
247400              MOVE 'MINIMUM LIMIT   '                                     
247500                                  TO LINK-TEMFSINF                        
247600            END-IF                                                        
247700         END-IF                                                           
247800       ELSE                                                               
247900         IF LINK-MINI-SUMMA < TP8GRET-SUARTBTO-MIN                        
248000            MOVE '773' TO LINK-IDFELKOD(INDX)                             
248100            MOVE NEJ              TO ALLT-SW                              
248200            ADD +1                TO INDX                                 
248300            IF LINK-TEMFSINF= SPACE                                       
248400              MOVE 'MINIMUM LIMIT   '                                     
248500                                  TO LINK-TEMFSINF                        
248600            END-IF                                                        
248700         END-IF                                                           
248800       END-IF                                                             
248900     END-IF                                                               
249000     .                                                                    
249100     EJECT                                                                
249200 CJ-FLYTTA-DATA SECTION.                                                  
249300     MOVE '*** CJ-FLYTTA-DATA *** '                                       
249400                              TO FELTEXT-STR                              
249500                                                                          
249600     IF LINK-KDFRAKT = +0 AND WLGMTB01-FINNS                              
249700        MOVE DC-KDGENFRA-MO  TO LINK-KDFRAKT                              
249800     END-IF                                                               
249900                                                                          
250000     .                                                                    
250100     EJECT                                                                
250200 S01-HAMTA-MARK-BOLAG-VALUTA SECTION.                                     
250300                                                                          
250400     MOVE GMT-IDPARTNR          TO W-IDPARTNR                             
250500     MOVE GMT-IDFTG             TO W-IDFTG                                
250600                                                                          
250700     PERFORM IMS-GET-WDB101                                               
250800     IF SEGMENT-FINNS                                                     
250900       MOVE BET-IDMARKBO        TO WS-IDMARKBO                            
251000     END-IF                                                               
251100                                                                          
251200     .                                                                    
251300     EJECT                                                                
251400 IMS-GET-WLKREE01 SECTION.                                                
251500     MOVE '*** IMS-GET-WLKREE01 *** '                                     
251600                              TO FELTEXT-STR                              
251700                                                                          
251800     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
251900            DELIMITED BY SIZE INTO SSA1                                   
252000                                                                          
252100     MOVE '  GE' TO GODK-STATUSKODER                                      
252200     CALL CBLTDLI USING                                                   
252300           GU KREE-PCB DLI-IO-AREA-WDA201 SSA1                            
252400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
252500     PERFORM IMS-STATUSKONTROLL                                           
252600     .                                                                    
252700     EJECT                                                                
252800 IMS-GET-WDB201 SECTION.                                                  
252900     MOVE '*** IMS-GET-WDB201 *** '                                       
253000                              TO FELTEXT-STR                              
253100                                                                          
253200     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
253300                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
253400            DELIMITED BY SIZE INTO SSA1                                   
253500                                                                          
253600     MOVE '  GE' TO GODK-STATUSKODER                                      
253700     CALL CBLTDLI USING                                                   
253800           GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1                            
253900     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
254000     PERFORM IMS-STATUSKONTROLL                                           
254100     .                                                                    
254200     EJECT                                                                
254300 IMS-GET-WDB201-UNIK SECTION.                                             
254400     MOVE '*** IMS-GET-WDB201-UNIK *** '                                  
254500                              TO FELTEXT-STR                              
254600                                                                          
254700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
254800            DELIMITED BY SIZE INTO SSA1                                   
254900                                                                          
255000     MOVE '  GE' TO GODK-STATUSKODER                                      
255100     CALL CBLTDLI USING                                                   
255200           GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1                            
255300     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
255400     PERFORM IMS-STATUSKONTROLL                                           
255500     .                                                                    
255600     EJECT                                                                
255700 IMS-GET-WDB101                 SECTION.                                  
255800                                                                          
255900     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
256000            DELIMITED BY SIZE INTO SSA1                                   
256100     MOVE '  GE' TO GODK-STATUSKODER                                      
256200     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
256300     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
256400     PERFORM IMS-STATUSKONTROLL                                           
256500     .                                                                    
256600     EJECT                                                                
256700 IMS-GET-WLGMTB01 SECTION.                                                
256800     MOVE '*** IMS-GET-WLGMTB01 *** '                                     
256900                              TO FELTEXT-STR                              
257000                                                                          
257100     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
257200                    '!WDB301KY =' W-WDB301KY-DEF-X ')'                    
257300            DELIMITED BY SIZE INTO SSA1                                   
257400                                                                          
257500     MOVE '  GE' TO GODK-STATUSKODER                                      
257600     CALL CBLTDLI USING                                                   
257700           GU GMTB-PCB DLI-IO-AREA-WDB301 SSA1                            
257800     MOVE GMTB-STATUS-CODE TO STATUS-WS                                   
257900     PERFORM IMS-STATUSKONTROLL                                           
258000     .                                                                    
258100     EJECT                                                                
258200 IMS-GET-WLARTC01 SECTION.                                                
258300     MOVE '*** IMS-GET-WLARTC01 *** '                                     
258400                              TO FELTEXT-STR                              
258500                                                                          
258600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
258700            DELIMITED BY SIZE INTO SSA1                                   
258800                                                                          
258900     MOVE '  GE' TO GODK-STATUSKODER                                      
259000     CALL CBLTDLI USING                                                   
259100           GU ARTC-PCB DLI-IO-AREA-WDK601 SSA1                            
259200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
259300     PERFORM IMS-STATUSKONTROLL                                           
259400     .                                                                    
259500     EJECT                                                                
259600 IMS-GET-WLARTC11 SECTION.                                                
259700     MOVE '*** IMS-GET-WLARTC11 *** '                                     
259800                              TO FELTEXT-STR                              
259900                                                                          
260000     MOVE 'WLARTC11 ' TO SSA1                                             
260100                                                                          
260200     MOVE '  GE' TO GODK-STATUSKODER                                      
260300     CALL CBLTDLI USING                                                   
260400           GNP ARTC-PCB DLI-IO-AREA-WDK611 SSA1                           
260500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
260600     PERFORM IMS-STATUSKONTROLL                                           
260700     .                                                                    
260800     EJECT                                                                
260900 IMS-GU-WDL501 SECTION.                                                   
261000     MOVE '*** IMS-GET-WDL501 *** '                                       
261100                              TO FELTEXT-STR                              
261200                                                                          
261300     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
261400            DELIMITED BY SIZE INTO SSA1                                   
261600     MOVE '  GE' TO GODK-STATUSKODER                                      
261700     CALL CBLTDLI USING GU  WDL5-PCB DLI-IO-AREA-WDL501 SSA1              
261900     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
262000     PERFORM IMS-STATUSKONTROLL                                           
262100     .                                                                    
262200     EJECT                                                                
262300 IMS-GNP-WDL511 SECTION.                                                  
262400     MOVE '*** IMS-GNP-WDL511 *** '                                       
262500                              TO FELTEXT-STR                              
262700     STRING 'WDL511  (IDGMTREF =' W-IDGMTREF-X                            
263000                    '&IDKOLLI  =' W-IDKOLLI-X ')'                         
263200            DELIMITED BY SIZE INTO SSA1                                   
263400     MOVE '  GE' TO GODK-STATUSKODER                                      
263500     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-AREA-WDL511 SSA1              
263700     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
263800     PERFORM IMS-STATUSKONTROLL                                           
263900     .                                                                    
264000     EJECT                                                                
264100 IMS-GNP-WDL521 SECTION.                                                  
264200     MOVE '*** IMS-GNP-WDL521 *** '                                       
264300                              TO FELTEXT-STR                              
264500     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
264800            DELIMITED BY SIZE INTO SSA1                                   
264900     STRING 'WDL521  (IDARTNR  =' W-IDARTNR-X ')'                         
265200            DELIMITED BY SIZE INTO SSA2                                   
265300                                                                          
265400     MOVE '  GE' TO GODK-STATUSKODER                                      
265500     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-AREA-WDL521 SSA1 SSA2         
265600     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
265700     PERFORM IMS-STATUSKONTROLL                                           
265800     .                                                                    
265900     EJECT                                                                
266000 IMS-GU-410901-ROT SECTION.                                               
266100     MOVE '*** IMS-GU-410901 *** '                                        
266200                              TO FELTEXT-STR                              
266300                                                                          
266400     STRING 'WL410901(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
266500          DELIMITED BY SIZE INTO SSA1                                     
266600     MOVE '    ' TO GODK-STATUSKODER                                      
266700     CALL CBLTDLI USING                                                   
266800           GU 4109-PCB DLI-IO-AREA-4109 SSA1                              
266900     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
267000     PERFORM IMS-STATUSKONTROLL                                           
267100     .                                                                    
267200                                                                          
267300 IMS-GNP-410911-KVAL SECTION.                                             
267400     MOVE '*** IMS-GNP-410911-KVAL *** '                                  
267500                              TO FELTEXT-STR                              
267600                                                                          
267700     STRING 'WL410911(KEY4110 =>' W-WDGXKEY-MIN-X                         
267800                    '&KEY4110 =<' W-WDGXKEY-MAX-X ')'                     
267900          DELIMITED BY SIZE INTO SSA1                                     
268000     MOVE '  GE' TO GODK-STATUSKODER                                      
268100     CALL CBLTDLI USING                                                   
268200           GNP 4109-PCB DLI-IO-AREA-4109 SSA1                             
268300     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
268400     PERFORM IMS-STATUSKONTROLL                                           
268500     .                                                                    
268600                                                                          
268700 IMS-GU-WDR501 SECTION.                                                   
268800                                                                          
268900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4127-X ')'                    
269000          DELIMITED BY SIZE INTO SSA1                                     
269100     MOVE '  '                TO GODK-STATUSKODER                         
269200     CALL CBLTDLI USING GU 4128-PCB DLI-IO-WDGX01 SSA1                    
269300     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
269400     PERFORM IMS-STATUSKONTROLL                                           
269500     .                                                                    
269600     SKIP2                                                                
269700 IMS-GU-WDGX4128 SECTION.                                                 
269800                                                                          
269900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4127-X ')'                    
270000          DELIMITED BY SIZE INTO SSA1                                     
270100     STRING 'WDGX4128(KY4128   =' W-KEY4128-X ')'                         
270200          DELIMITED BY SIZE INTO SSA2                                     
270300     MOVE '  GE'              TO GODK-STATUSKODER                         
270400     CALL CBLTDLI USING GU 4128-PCB DLI-IO-WDGX4128 SSA1 SSA2             
270500     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
270600     PERFORM IMS-STATUSKONTROLL                                           
270700     .                                                                    
270800     SKIP2                                                                
270900 IMS-GNP-WDGX4128-DISTRIKT SECTION.                                       
271000                                                                          
271100     STRING 'WDGX4128(IDDISTRF =' W-4128-IDDISTR-FOM-X                    
271200                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X ')'                
271300          DELIMITED BY SIZE INTO SSA1                                     
271400     MOVE '  GEGB'            TO GODK-STATUSKODER                         
271500     CALL CBLTDLI USING GNP 4128-PCB DLI-IO-WDGX4128 SSA1                 
271600     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
271700     PERFORM IMS-STATUSKONTROLL                                           
271800     .                                                                    
271900     EJECT                                                                
272000 IMS-STATUSKONTROLL SECTION.                                              
272100                                                                          
272200     SET STATUS-IX TO 1                                                   
272300     SEARCH GODK-STATUS                                                   
272400       AT END                                                             
272500*        MOVE 'FEL KOD FRÅN IMS' TO FELTEXT-STR                           
272600         CALL FELLOG                                                      
272700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
272800         CONTINUE                                                         
272900     END-SEARCH                                                           
273000     .                                                                    
273100     EJECT                                                                
273200*--------------------------------------------------------------- *        
273300*--- TP8GRET                                                     *        
273400*--------------------------------------------------------------- *        
273500 DB2-SELECT-TP8GRET SECTION.                                              
273600     MOVE 'DB2-SELECT-TP8GRET'   TO DB2-SECTION                           
273700                                                                          
273800     EXEC SQL                                                             
273900       SELECT  SUARTBTO_MIN                                               
274000              ,FLINVLDC                                                   
274100              ,SUARTBTO_LDC_MIN                                           
274200         INTO :TP8GRET-SUARTBTO-MIN                                       
274300             ,:TP8GRET-FLINVLDC                                           
274400             ,:TP8GRET-SUARTBTO-LDC-MIN                                   
274500         FROM  TP8GRET                                                    
274600        WHERE  IDDISTR  = :TP8GRET-IDDISTR                                
274700          AND  IDKUNDNR = :TP8GRET-IDKUNDNR                               
274800          AND  KDANMORS = :TP8GRET-KDANMORS                               
274900     END-EXEC                                                             
275000                                                                          
275100     PERFORM DB2-INITIALIZE-GOOD-SQLCODE                                  
275200     MOVE ZERO                   TO GOOD-SQLCODE (1)                      
275300     MOVE +100                   TO GOOD-SQLCODE (2)                      
275400     MOVE -811                   TO GOOD-SQLCODE (3)                      
275500     MOVE SQLCODE                TO SQLCODE-WS                            
275600     PERFORM DB2-STATUS-KONTROLL                                          
275700     .                                                                    
275800     EJECT                                                                
275900 DB2-INITIALIZE-GOOD-SQLCODE SECTION.                                     
276000                                                                          
276100     SET SQLCODE-IX TO 1                                                  
276200     PERFORM UNTIL SQLCODE-IX > 5                                         
276300       MOVE ZERO                TO GOOD-SQLCODE(SQLCODE-IX)               
276400       SET SQLCODE-IX UP BY 1                                             
276500     END-PERFORM                                                          
276600     .                                                                    
276700     EJECT                                                                
276800 DB2-STATUS-KONTROLL  SECTION.                                            
276900     MOVE 'DB2-STATUS-KONTROLL'   TO DB2-SECTION                          
277000                                                                          
277100     SET SQLCODE-IX TO 1                                                  
277200     SEARCH GOOD-SQLCODE                                                  
277300       AT END                                                             
277400          STRING 'INVALID SQLCODE FROM DB2 ' SQLCODE-WS                   
277500          DELIMITED BY SIZE INTO FELTEXT-STR                              
277600          CALL FELLOG                                                     
277700       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
277800          CONTINUE                                                        
277900     END-SEARCH                                                           
278000     .                                                                    
278100     EJECT                                                                
279000*    -COPY WY2000Q1                                                       
