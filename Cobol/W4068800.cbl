000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4068800.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   97/01/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKRIVER 'BILL OF LADING' FÖR NDC:ERNA.                           
001000*        (VIA D&P FÖR DC 51, TORONTO)                                     
001100*        (VIA D&P FÖR DC 44, USA)                                         
001200*                                                                         
001300*        PROGRAMMET LÄSER      WL4463 (WDR4)                              
001400*                              WLORQI (WDQ2)                              
001500*                              WL1165 (WDR2)                              
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W4T688                                              
001900*        MID:         W4I68801                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W4O68801                                            
002300*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W4068800'.            
003200 77  WS-ADRESS-DP                PIC X(50)                                
003300         VALUE 'CARPARTS.DAP.DISTRDOCWEB'.                                
003400 77  KDRC-DISPLAY                PIC Z(5).                                
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                         PIC X          VALUE 'J'.                 
004000 77  NEJ                        PIC X          VALUE 'N'.                 
004100 77  WS-KRYSS                   PIC X          VALUE 'X'.                 
004200 77  WS-KG                      PIC X(2)       VALUE 'KG'.                
004300 77  WS-LBS                     PIC X(2)       VALUE 'LB'.                
004400 77  IX                         PIC S9(9)     VALUE +0 COMP SYNC.         
004500 77  INDX                       PIC S9(9)     VALUE +0 COMP SYNC.         
004600 77  MAX-INDX                   PIC S9(9)     VALUE +10 COMP SYNC.        
004700 77  FG-INDX                    PIC S9(9)     VALUE +0  COMP SYNC.        
004800 77  FG-MAX-INDX                PIC S9(9)     VALUE +10 COMP SYNC.        
004900 77  IDORDER-MAX-INDX           PIC S9(9)     VALUE +13 COMP SYNC.        
005000 77  WS-RADNR                   PIC S9(3)      VALUE +1   COMP-3.         
005100 77  WS-ANTAL-RADER             PIC S9(3)      VALUE ZERO COMP-3.         
005200 77  MAX-ANTAL-RADER            PIC S9(3)      VALUE +16  COMP-3.         
005300 77  WS-ANTAL-KOLLI-GLAS        PIC S9(5)      VALUE ZERO COMP-3.         
005400 77  WS-ANTAL-KOLLI-PARTS       PIC S9(5)      VALUE ZERO COMP-3.         
005500 77  WS-ANTAL-KOLLI-RETUREMB    PIC S9(5)      VALUE ZERO COMP-3.         
005600 77  WS-VKORDBTO-LB             PIC S9(11)     VALUE ZERO COMP-3.         
005700*                                                                         
005800*                                                                         
005900 77  SPAR-IDDISTR               PIC S9(5)      VALUE ZERO COMP-3.         
006000 77  SPAR-IDKUNDNR              PIC S9(7)      VALUE ZERO COMP-3.         
006100 77  SPAR-IDKUNDRF              PIC X(10)      VALUE SPACE.               
006200 77  SPAR-VKORDBTO-GLAS         PIC S9(8)V9(1) VALUE ZERO COMP-3.         
006300 77  SPAR-VKORDBTO-PARTS        PIC S9(8)V9(1) VALUE ZERO COMP-3.         
006400 77  SPAR-ANTAL-KOLLI-TOT       PIC S9(7)      VALUE ZERO COMP-3.         
006500 77  SPAR-VKORDBTO-LB-TOTAL     PIC S9(11)     VALUE ZERO COMP-3.         
006600 77  SPAR-VKORDBTO-TOTAL        PIC S9(11)     VALUE ZERO COMP-3.         
006700                                                                          
006800 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +33 COMP SYNC.          
006900                                                                          
007000 77  WS-IDKUNDNR                PIC S9(7)      VALUE ZERO COMP-3.         
007100 77  WS-IDTRPTNR                PIC S9(3)      VALUE ZERO COMP-3.         
007200 77  WS-IDTRPBOR                PIC S9(5)      VALUE ZERO COMP-3.         
007300 77  WS-USA-TELNR               PIC X(15)      VALUE                      
007400                                               '1-800-255-3924'.          
007500 77  WS-CANADA-TELNR            PIC X(15)      VALUE                      
007600                                               '613-996-6666'.            
007700                                                                          
007800     EJECT                                                                
007900*      --- VALID IDDC CODES                                               
008000*                                                                         
008100*01    -COPY WWDC99                                                       
008200       EJECT                                                              
008300                                                                          
008400 01  PSN-SW                     PIC 9(3).                                 
008500     88 EJ-GODK-PSN                         VALUE 120                     
008600                                                  130 131 132 133         
008700                                                  134 135 136 137         
008800                                                  138 139                 
008900                                                  140 141 142 143         
009000                                                  144 145 146 147         
009100                                                  148                     
009200                                                  220                     
009300                                                  232 233 235 236         
009400                                                  238 239                 
009500                                                  240 241 242 243         
009600                                                  244 245 246 247         
009700                                                  248                     
009800                                                  250 252 253 254         
009900                                                  255                     
010000                                                  333                     
010010                                                  900 THRU 999.           
010100                                                                          
010200     EJECT                                                                
010300 01  DAGENS-DATUM.                                                        
010400     03  DAGENS-AAR              PIC 9(2).                                
010500     03  DAGENS-MAN              PIC 9(2).                                
010600     03  DAGENS-DAG              PIC 9(2).                                
010700                                                                          
010800 01  GLAS-TEXT1                  PIC X(43)   VALUE                        
010900     'Glass, automobile windshield, cut to shape.'.                       
011000 01  GLAS-TEXT2                  PIC X(43)   VALUE                        
011100     'Bent tempered. Not framed.                 '.                       
011200*                                                                         
011300 01  PARTS-TEXT                  PIC X(43)   VALUE                        
011400     'AUTOMOBILE PARTS                           '.                       
011500*                                                                         
011600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
011700                                                                          
011800                                                                          
011900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
012000     88  NYCKLAR-OK                          VALUE 'J'.                   
012100     88  NYCKLAR-FEL                         VALUE 'N'.                   
012200                                                                          
012300 77  SEG-4463-SW                 PIC X.                                   
012400     88  SEG-4463-FINNS                      VALUE 'J'.                   
012500     88  SEG-4463-SAKNAS                     VALUE 'N'.                   
012600                                                                          
012700 77  FARLIGT-GODS               PIC X        VALUE 'N'.                   
012800       88 FARLIGT-GODS-FINNS                 VALUE 'J'.                   
012900       88 FARLIGT-GODS-FINNS-EJ              VALUE 'N'.                   
013000                                                                          
013100 77  GLAS-SW                    PIC X        VALUE 'N'.                   
013200       88 GLAS-FINNS                         VALUE 'J'.                   
013300                                                                          
013400 77  SW-DAP                     PIC X        VALUE 'N'.                   
013500       88 SW-DAP-OPEN                        VALUE 'J'.                   
013600                                                                          
013700 77  TRAFF-SW                    PIC X       VALUE 'J'.                   
013800     88  TRAFF                               VALUE 'J'.                   
013900                                                                          
014000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014100     88  EGEN-MID                            VALUE '4688'.                
014200     88  GODK-MID                            VALUE '4681' '4682'          
014300                                                   '4683' '4684'          
014400                                                   '4685' '4686'          
014500                                                   '4687' '4688'          
014600                                                   '4689'.                
014700     88  HELP-MID                            VALUE '0551'.                
014800     EJECT                                                                
014900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015000 01  GENERELLA-SUBPROGRAM.                                                
015100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
015300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015600     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
015700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
015800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
015900     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
016000                                                                          
016100                                                                          
016200*    --- PARAMETERS FOR SUBPROGRAM WL10WBDC                               
016300                                                                          
016400 01  FILLER                      PIC X(16)  VALUE 'WL10WBDC AREA'.        
016500*01  -COPY WL10WBDC                                                       
016600                                                                          
016700     EJECT                                                                
016800 01    FILLER                    PIC X(16)   VALUE 'FG-TABELL'.           
016900                                                                          
017000 01    FG-TABELL.                                                         
017100   03    TAB-POST OCCURS 10.                                              
017200                                                                          
017300     05  TAB-IDPSN               PIC 9(3)              VALUE ZERO.        
017400                                                                          
017500     05  TAB-VKORDBTO-KOLLI      PIC S9(8)V9(1) COMP-3 VALUE ZERO.        
017600                                                                          
017700     05  TAB-ANTAL-KOLLI-FG      PIC S9(5)      COMP-3 VALUE ZERO.        
017800*                                                                         
017900 01    NDC-IDTRPTNR-TABELL.                                               
018000   03    NDC-POST OCCURS 3.                                               
018100                                                                          
018200     05  NDC-IDTRPTNR            PIC 9(3)              VALUE ZERO.        
018300                                                                          
018400     05  NDC-BETRANSP            PIC X(25)   VALUE 'CAMI'.                
018500                                                                          
018600     EJECT                                                                
018700 01  TEST-IDDISTR                PIC 9(5)    COMP-3 VALUE ZERO.           
018800*01  FILLER   -COPY WWDIST07    -RED TEST-IDDISTR.                        
018900     EJECT                                                                
019000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
019100*01 -COPY WMEDAREA                                                        
019200     SKIP3                                                                
019300 01  MESSAGE-CODES.                                                       
019400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
019500     EJECT                                                                
019600*01  -COPY W006PRAR                                                       
019700     EJECT                                                                
019800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
019900*                                                                         
020000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
020100     SKIP3                                                                
020200*01 -COPY WMSGINIT                                                        
020300     SKIP3                                                                
020400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020500*                                                                         
020600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020700     SKIP3                                                                
020800*01  MID -COPY W4I68801                                                   
020900     EJECT                                                                
021000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021100     SKIP3                                                                
021200*01  -COPY WMSGAREA                                                       
021300     EJECT                                                                
021400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021500     SKIP3                                                                
021600*01  -COPY WMFSAREA                                                       
021700     EJECT                                                                
021800*    --- PARAMETRAR TILL COPYTEXT   WWOMVAND                              
021900*01 -COPY WWOMVAND                                                        
022000     EJECT                                                                
022100 01  FILLER                      PIC X(16)   VALUE 'PRT-AREA  '.          
022200 01  WS-PRT-AREA.                                                         
022300                                                                          
022400     03  WS-PRT-DUMMY          PIC X.                                     
022500     03  WS-PRT-IDPRTLST       PIC X(8)  VALUE SPACE.                     
022600     03  WS-LIST-RAD           PIC X(132).                                
022700*                                                                         
022800 01  FILLER                      PIC X(16)   VALUE 'LIST-RADER'.          
022900 01  LIST-RADER.                                                          
023000                                                                          
023100     03 RUBRIKRAD-1.                                                      
023200       05 FILLER               PIC X(53) VALUE SPACE.                     
023300       05 RUB1-IDKUNDNR        PIC Z(5)9.                                 
023400       05 FILLER               PIC X(3)  VALUE ' - '.                     
023500       05 RUB1-KDORDKL         PIC 9.                                     
023600       05 FILLER               PIC X(3)  VALUE ' - '.                     
023700       05 RUB1-IDORDNR         PIC Z(4)9.                                 
023800                                                                          
023900     03 RUBRIKRAD-2.                                                      
024000       05 FILLER               PIC X(64) VALUE SPACE.                     
024100       05 RUB2-MAN             PIC 9(2).                                  
024200       05 FILLER               PIC X     VALUE '/'.                       
024300       05 RUB2-DAG             PIC 9(2).                                  
024400       05 FILLER               PIC X     VALUE '/'.                       
024500       05 RUB2-AAR             PIC 9(2).                                  
024600                                                                          
024700     03 RUBRIKRAD-3.                                                      
024800       05 FILLER               PIC X(64) VALUE SPACE.                     
024900       05 RUB3-BETRANSP        PIC X(25).                                 
025000                                                                          
025100     03 RUBRIKRAD-4.                                                      
025200       05 FILLER               PIC X(5)  VALUE SPACE.                     
025300       05 RUB4-BEGMT-RAD1      PIC X(35) VALUE SPACE.                     
025400                                                                          
025500     03 RUBRIKRAD-4A.                                                     
025600       05 FILLER               PIC X(5)  VALUE SPACE.                     
025700       05 RUB4-BEGMT-RAD2      PIC X(35) VALUE SPACE.                     
025800                                                                          
025900     03 RUBRIKRAD-5.                                                      
026000       05 FILLER               PIC X(5)  VALUE SPACE.                     
026100       05 RUB5-ADGMT-GATA      PIC X(35) VALUE SPACE.                     
026200                                                                          
026300     03 RUBRIKRAD-6.                                                      
026400       05 FILLER               PIC X(5)  VALUE SPACE.                     
026500       05 RUB6-ADGMT-PADR      PIC X(35) VALUE SPACE.                     
026600                                                                          
026700     03 DETALJRAD1.                                                       
026800       05 LISTRAD-ANTAL-KOLLI  PIC Z(3)9.                                 
026900       05 FILLER               PIC X(3)   VALUE SPACE.                    
027000       05 LISTRAD-RADNR        PIC 9.                                     
027100       05 FILLER               PIC X(2)   VALUE SPACE.                    
027200       05 LISTRAD-KRYSS        PIC X      VALUE SPACE.                    
027300       05 FILLER               PIC X      VALUE SPACE.                    
027400       05 LISTRAD-BENAEMN1     PIC X(55).                                 
027500       05 FILLER               PIC X(3)   VALUE SPACE.                    
027600       05 LISTRAD-VKORDBTO     PIC ZZZ,ZZ9.                               
027700       05 FILLER               PIC X      VALUE SPACE.                    
027800       05 LISTRAD-KG           PIC X(2)   VALUE SPACE.                    
027900*                                                                         
028000     03 DETALJRAD2.                                                       
028100       05 FILLER               PIC X(12)  VALUE SPACE.                    
028200       05 LISTRAD-BENAEMN2     PIC X(55).                                 
028300*                                                                         
028400     03 TOTALRAD.                                                         
028500       05 TOTAL-ANTAL-KOLLI    PIC Z(3)9.                                 
028510*      05 TOTAL-ANTAL-KOLLI    PIC X(4)   VALUE SPACE.                    
028600       05 FILLER               PIC X(60)  VALUE SPACE.                    
028700       05 TOTAL-VKORDBTO       PIC Z,ZZZ,ZZZ,ZZ9.                         
028800       05 FILLER               PIC X      VALUE SPACE.                    
028900       05 TOTAL-KG             PIC X(2)   VALUE SPACE.                    
029000*                                                                         
029100     03 FINALRAD1.                                                        
029200       05 FILLER               PIC X     VALUE SPACE.                     
029300       05 FINAL1-IDORDNR-GRP OCCURS 13.                                   
029400         07 FINAL1-IDORDNR     PIC Z(5).                                  
029500         07 FILLER             PIC X     VALUE SPACE.                     
029600                                                                          
029700     03 FINALRAD2.                                                        
029800       05 FILLER               PIC X(18) VALUE SPACE.                     
029900       05 FINAL2-KDKOLLI-EMB   PIC Z(3)9.                                 
030000*                                                                         
030100     EJECT                                                                
030200 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
030300*01  -COPY WZ01SEND                                                       
030400     EJECT                                                                
030500 01  PRINTAREA-START             PIC X(24)   VALUE                        
030600                                 'PRINTAREA-START'.                       
030700 01  HDR-AREA.                                                            
030800*    03  -COPY WZ01REQU  -PRE HDR-                                        
030900*    03  -COPY WZ04HDR                                                    
031000     EJECT                                                                
031100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031200*                                                                         
031300*    --- POSTBESKRIVNINGAR TILL D&P                                       
031400 01  DOC-HEAD-AREA.                                                       
031500*    03 -COPY W476BL1  -PRE HUVUD-                                        
031600 01  DOC-LINE-AREA.                                                       
031700*    03 -COPY W476BL2  -PRE RAD-                                          
031800 01  DOC-FOOT-AREA.                                                       
031900*    03 -COPY W476BL3  -PRE FOT-                                          
032000     SKIP2                                                                
032100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
032200     SKIP3                                                                
032300 01  NYCKLAR-TILL-DLI.                                                    
032400                                                                          
032500     03  W-IDGMTREF-X.                                                    
032600         05  W-IDDISTR-WDQ2      PIC S9(5)   VALUE ZERO COMP-3.           
032700         05  W-IDKUNDNR-WDQ2     PIC S9(7)   VALUE ZERO COMP-3.           
032800         05  W-IDKUNDRF-WDQ2     PIC X(10)   VALUE SPACE.                 
032900                                                                          
033000     03  W-1165-X.                                                        
033100         05  W-IDHTYP            PIC X(4)    VALUE '1165'.                
033200         05  W-IDPSN             PIC 9(3).                                
033300         05  W-IDSPRAK           PIC X(2)    VALUE 'GB'.                  
033400         05  W-LOW-VALUE         PIC X(21)   VALUE LOW-VALUE.             
033500                                                                          
033600     03  W-KDFGTRP-X.                                                     
033700         05  W-KDFGTRP           PIC 9(2)    VALUE 4.                     
033800*                                                     4 = BIL*            
033900                                                                          
034000     03  W-4463-X.                                                        
034100         05  W-IDHTYP            PIC X(4)    VALUE '4463'.                
034200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
034300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
034400                                                                          
034500     03  W-DASKEPPN-X.                                                    
034600         05  W-DASKEPPN          PIC  9(8)   VALUE ZERO.                  
034700                                                                          
034800     03  W-4466-X.                                                        
034900         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
035000         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
035100                                                                          
035200     03  W-4468-X.                                                        
035300         05  W-IDDISTR-4468       PIC S9(5)   VALUE ZERO  COMP-3.         
035400         05  W-IDKUNDNR-4468      PIC S9(7)   VALUE ZERO  COMP-3.         
035500         05  W-IDKUNDRF           PIC X(10).                              
035600         05  W-IDKUNDRF-IDORDNR-FILLER REDEFINES W-IDKUNDRF.              
035700           07  W-IDORDNR7         PIC  9(7).                              
035800           07  FILLER             PIC  X(3).                              
035900         05  W-IDPRODNR           PIC S9(7)   VALUE ZERO  COMP-3.         
036000         05  W-IDKOLLI            PIC S9(5)   VALUE ZERO  COMP-3.         
036100     SKIP2                                                                
036200*    --- STATUS-KOD FRÅN IMS                                              
036300 01  STATUS-WS                   PIC XX.                                  
036400     88  SEGMENT-FINNS                       VALUE '  '.                  
036500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
036600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
036700     88  BASEN-SLUT                          VALUE 'GB'.                  
036800     SKIP2                                                                
036900 01  GODK-STATUSKODER.                                                    
037000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
037100     SKIP3                                                                
037200 01  SSA1                        PIC X(128).                              
037300 01  SSA2                        PIC X(64).                               
037400 01  SSA3                        PIC X(64).                               
037500     EJECT                                                                
037600*    --- IMS FUNKTIONSKODER                                               
037700*01  -COPY W0003                                                          
037800     EJECT                                                                
037900*    ---  DLI INPUT-OUTPUT AREA                                           
038000 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-4463'.        
038100     SKIP3                                                                
038200 01  DLI-IO-AREA-4463.                                                    
038300     03  WL446301.                                                        
038400*        05  -COPY WDGX4463                                               
038500     EJECT                                                                
038600 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-4464'.        
038700     SKIP3                                                                
038800 01  DLI-IO-AREA-4464.                                                    
038900     03  WL446311.                                                        
039000*        05  -COPY WDGX4464                                               
039100     EJECT                                                                
039200 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-4466'.        
039300     SKIP3                                                                
039400 01  DLI-IO-AREA-4466.                                                    
039500     03  WL446321.                                                        
039600*        05  -COPY WDGX4466                                               
039700     EJECT                                                                
039800 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-4468'.        
039900     SKIP3                                                                
040000 01  DLI-IO-AREA-4468.                                                    
040100     03  WL446331.                                                        
040200*        05  -COPY WDGX4468                                               
040300     EJECT                                                                
040400 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-WDQ2'.        
040500 01  DLI-IO-AREA-OHUV.                                                    
040600     03  WLORQI01.                                                        
040700*        05  -COPY WDQ201                                                 
040800     EJECT                                                                
040900 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-1168'.        
041000 01  DLI-IO-AREA-1168.                                                    
041100     03  WL116512.                                                        
041200*        05  -COPY WDGX1168                                               
041300     EJECT                                                                
041400 LINKAGE SECTION.                                                         
041500                                                                          
041600*01  -COPY W0009   -PRE MSG-                                              
041700*01  -COPY W0009   -PRE DAP-                                              
041800                                                                          
041900*01  -COPY W0009   -PRE ALT-                                              
042000                                                                          
042100*01  -COPY W0008   -PRE 4463-                                             
042200     05  FILLER                  PIC X.                                   
042300     EJECT                                                                
042400*01  -COPY W0008  -PRE ORQI-                                              
042500     05  FILLER                  PIC X.                                   
042600*01  -COPY W0008  -PRE 1165-                                              
042700     05  FILLER                  PIC X.                                   
042800     EJECT                                                                
042900 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB  DAP-PCB 4463-PCB             
043000                           ORQI-PCB 1165-PCB.                             
043100     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  DAP-PCB 4463-PCB             
043200                           ORQI-PCB 1165-PCB.                             
043300                                                                          
043400     PERFORM IMS-GET-MSG                                                  
043500     IF SEGMENT-FINNS                                                     
043600       PERFORM A-INIT                                                     
043700       IF NYCKLAR-OK                                                      
043800                                                                          
043900         IF MID-KDSVAR = 'P' OR 'p' OR 'X' or 'x'                         
044000         OR 'F' OR 'f'                                                    
044100           PERFORM IMS-GU-WL446321-TRANSP                                 
044200                                                                          
044300           IF SEGMENT-FINNS                                               
044400             PERFORM C-LAES-SKRIV-BILL-OF-LADING                          
044500           END-IF                                                         
044600         END-IF                                                           
044700                                                                          
044800         PERFORM Z-STAENG-PRINTER                                         
044900       END-IF                                                             
045000     END-IF                                                               
045100                                                                          
045200     MOVE ZERO TO RETURN-CODE                                             
045300     GOBACK                                                               
045400     .                                                                    
045500     EJECT                                                                
045600 A-INIT SECTION.                                                          
045700                                                                          
045800     MOVE MSG-INDATA-MINUS-1-TRANSKOD    TO MID-W4I68801                  
045900     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
046000                                                                          
046100     MOVE MID-TIDATUM     TO W-DASKEPPN                                   
046200     IF MID-TIDATUM NOT = ZERO                                            
046300       IF MID-TIDATUM < 500000                                            
046400         MOVE 20          TO W-DASKEPPN (1:2)                             
046500       ELSE                                                               
046600         IF MID-TIDATUM < 999999                                          
046700           MOVE 19        TO W-DASKEPPN (1:2)                             
046800         ELSE                                                             
046900           MOVE 99999999  TO W-DASKEPPN                                   
047000         END-IF                                                           
047100       END-IF                                                             
047200     END-IF                                                               
047300                                                                          
047400     MOVE MID-IDDC        TO W-IDDC                                       
047500     MOVE MID-IDTRPTNR    TO W-IDTRPTNR                                   
047600     MOVE MID-IDLBBET     TO W-IDLBBET                                    
047700                                                                          
047800     MOVE SPACE           TO RAD-W476BL2                                  
047900     MOVE SPACE           TO FOT-W476BL3                                  
048000                                                                          
048100     PERFORM S01-NOLLA-TABELL                                             
048200                                                                          
048300     PERFORM S02-INIT-PRINTER-ID                                          
048400                                                                          
048500     CALL W006PRS1  USING PRT-SPOOL-A4S                                   
048600                          PRT-OPEN                                        
048700                          WS-PRT-IDPRTLST                                 
048800                          ALT-PCB                                         
048900                          WS-PRT-DUMMY                                    
049000                          WS-PRT-DUMMY                                    
049100                                                                          
049200     MOVE MID-IDDC        TO WBDC-IDDC                                    
049300     CALL WL10WBDC USING WBDC-AREA                                        
049400     .                                                                    
049500     EJECT                                                                
049600 C-LAES-SKRIV-BILL-OF-LADING SECTION.                                     
049700                                                                          
049800     PERFORM IMS-GNP-WL446331-KOLLI                                       
049900     IF SEGMENT-FINNS                                                     
050000       MOVE JA  TO SEG-4463-SW                                            
050100     ELSE                                                                 
050200       MOVE NEJ TO SEG-4463-SW                                            
050300     END-IF                                                               
050400                                                                          
050500     IF SEG-4463-FINNS AND WBDC-FLWEBDC = JA                              
050600       PERFORM S10-DAP-OPEN                                               
050700     END-IF                                                               
050800                                                                          
050900     PERFORM UNTIL SEG-4463-SAKNAS                                        
051000                                                                          
051100       PERFORM CA-SKAPA-HUVUDET                                           
051200                                                                          
051300       PERFORM UNTIL SEG-4463-SAKNAS                     OR               
051400                                                                          
051500                     (4468-IDDISTR  NOT = SPAR-IDDISTR)  OR               
051600                     (4468-IDKUNDNR NOT = SPAR-IDKUNDNR)                  
051700                                                                          
051800         PERFORM CB-SPARA-DATA                                            
051900                                                                          
052000         PERFORM IMS-GNP-WL446331-KOLLI                                   
052100         IF SEGMENT-FINNS                                                 
052200           MOVE JA TO SEG-4463-SW                                         
052300         ELSE                                                             
052400           MOVE NEJ TO SEG-4463-SW                                        
052500         END-IF                                                           
052600       END-PERFORM                                                        
052700                                                                          
052800       IF WBDC-FLWEBDC = JA                                               
052900          PERFORM CD-SKRIV-BILL-OF-LADING-WEB                             
053000       ELSE                                                               
053100          PERFORM CC-SKRIV-BILL-OF-LADING                                 
053200       END-IF                                                             
053300                                                                          
053400       PERFORM S01-NOLLA-TABELL                                           
053500                                                                          
053600     END-PERFORM                                                          
053700                                                                          
053800     IF SW-DAP-OPEN                                                       
053900       PERFORM S95-SEND-CLOSE                                             
054000     END-IF                                                               
054100     .                                                                    
054200                                                                          
054300 CA-SKAPA-HUVUDET SECTION.                                                
054400                                                                          
054500     MOVE 4468-IDKUNDNR       TO RUB1-IDKUNDNR                            
054600     MOVE 4468-KDORDKL        TO RUB1-KDORDKL                             
054700     MOVE 4468-IDKUNDRF (3:5) TO RUB1-IDORDNR                             
054800                                                                          
054900     MOVE MID-TIDATUM         TO DAGENS-DATUM                             
055000     MOVE DAGENS-AAR          TO RUB2-AAR                                 
055100     MOVE DAGENS-MAN          TO RUB2-MAN                                 
055200     MOVE DAGENS-DAG          TO RUB2-DAG                                 
055300                                                                          
055400     PERFORM CAA-HAEMTA-TRANSP-NAMN                                       
055500                                                                          
055600     PERFORM CAB-HAEMTA-KUND-ADR-Q2                                       
055700                                                                          
055800     MOVE 4468-IDDISTR        TO SPAR-IDDISTR                             
055900     MOVE 4468-IDKUNDNR       TO SPAR-IDKUNDNR                            
056000     MOVE 4468-IDKUNDRF       TO SPAR-IDKUNDRF                            
056100     .                                                                    
056200     EJECT                                                                
056300 CAA-HAEMTA-TRANSP-NAMN SECTION.                                          
056400                                                                          
056500*CO*OBS* I VÄNTAN PÅ TRANSPORTNAMNEN SKRIVER PGM-ET 'LASTBÄRARE'          
056600*        PÅ BILL OF LADING*                                               
056700*                                                                         
056800                                                                          
056900     MOVE 4466-IDLBBET TO RUB3-BETRANSP                                   
057000                                                                          
057100*                                                                         
057200*    MOVE +1 TO INDX                                                      
057300*    MOVE NEJ TO TRAFF-SW                                                 
057400                                                                          
057500*    PERFORM UNTIL (INDX > MAX-INDX)    OR TRAFF                          
057600                                                                          
057700*      IF NDC-IDTRPTNR (INDX) = 4466-IDTRPTNR                             
057800*        MOVE NDC-BETRANSP (INDX) TO RUB3-BETRANSP                        
057900*        MOVE JA TO TRAFF-SW                                              
058000*      END-IF                                                             
058100                                                                          
058200*      ADD +1 TO INDX                                                     
058300*    END-PERFORM                                                          
058400                                                                          
058500*    IF NOT TRAFF                                                         
058600*      MOVE 'TRP MISSING'           TO RUB3-BETRANSP                      
058700*    END-IF                                                               
058800     .                                                                    
058900     EJECT                                                                
059000 CAB-HAEMTA-KUND-ADR-Q2 SECTION.                                          
059100                                                                          
059200     MOVE 4468-IDDISTR        TO W-IDDISTR-WDQ2                           
059300     MOVE 4468-IDKUNDNR       TO W-IDKUNDNR-WDQ2                          
059400     MOVE 4468-IDKUNDRF       TO W-IDKUNDRF-WDQ2                          
059500                                                                          
059600     PERFORM IMS-GU-ORQI-WDQ201                                           
059700                                                                          
059800     IF SEGMENT-FINNS                                                     
059900       MOVE OHUV-BEGMT-RAD1     TO RUB4-BEGMT-RAD1                        
060000       MOVE OHUV-BEGMT-RAD2     TO RUB4-BEGMT-RAD2                        
060100       MOVE OHUV-ADGMT-GATA     TO RUB5-ADGMT-GATA                        
060200       MOVE OHUV-ADGMT-PADR     TO RUB6-ADGMT-PADR                        
060300     ELSE                                                                 
060400       MOVE 'ORDERNUMMER ÄR BLANKT' TO FELTEXT                            
060500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
060600     END-IF                                                               
060700     .                                                                    
060800     EJECT                                                                
060900 CB-SPARA-DATA SECTION.                                                   
061000                                                                          
061100     PERFORM CBA-KOLLA-OM-FARLIGT-GODS-GLAS                               
061200                                                                          
061300     IF FARLIGT-GODS-FINNS                                                
061400       PERFORM CBB-SPARA-FARLIGT-GODS                                     
061500     ELSE                                                                 
061600*      IF GLAS-FINNS                                                      
061700*        PERFORM CBC-SPARA-GLAS                                           
061800*      ELSE                                                               
061900         PERFORM CBD-SPARA-PARTS                                          
062000*      END-IF                                                             
062100     END-IF                                                               
062200                                                                          
062300     PERFORM CBE-SPARA-IDORDNR                                            
062400                                                                          
062500     PERFORM CBF-SPARA-RETUREMB                                           
062600     .                                                                    
062700     EJECT                                                                
062800 CBA-KOLLA-OM-FARLIGT-GODS-GLAS SECTION.                                  
062900                                                                          
063000     MOVE NEJ        TO FARLIGT-GODS                                      
063100     MOVE NEJ        TO GLAS-SW                                           
063200                                                                          
063300     MOVE +1         TO INDX                                              
063400     PERFORM UNTIL INDX > FG-MAX-INDX                                     
063500       IF 4468-IDPSN(INDX) > ZERO                                         
063600                                                                          
063700         MOVE 4468-IDPSN(INDX)   TO PSN-SW                                
063800         IF EJ-GODK-PSN                                                   
063900           CONTINUE                                                       
064000         ELSE                                                             
064100          MOVE JA    TO FARLIGT-GODS                                      
064200          MOVE +10   TO INDX                                              
064300         END-IF                                                           
064400                                                                          
064500       END-IF                                                             
064600       ADD +1        TO INDX                                              
064700     END-PERFORM                                                          
064800                                                                          
064900*    IF FARLIGT-GODS-FINNS-EJ                                             
065000*      IF 4468-KDKOLLI (1:2) = 'UW'                                       
065100*        MOVE JA TO GLAS-SW                                               
065200*      END-IF                                                             
065300*    END-IF                                                               
065400     .                                                                    
065500     EJECT                                                                
065600 CBB-SPARA-FARLIGT-GODS SECTION.                                          
065700                                                                          
065800     MOVE +1 TO FG-INDX                                                   
065900                INDX                                                      
066000     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
066100                                                                          
066200       MOVE 4468-IDPSN(FG-INDX)     TO PSN-SW                             
066300       IF EJ-GODK-PSN                                                     
066400         CONTINUE                                                         
066500       ELSE                                                               
066600                                                                          
066700         IF TAB-IDPSN(INDX) = ZERO                                        
066800                                                                          
066900           IF 4468-IDPSN(FG-INDX) > ZERO                                  
067000             MOVE 4468-IDPSN(FG-INDX) TO TAB-IDPSN (INDX)                 
067100             MOVE 4468-VKORDBTO-KOLLI TO TAB-VKORDBTO-KOLLI (INDX)        
067200             ADD +1                 TO TAB-ANTAL-KOLLI-FG (INDX)          
067300                                         INDX                             
067400           END-IF                                                         
067500                                                                          
067600         ELSE                                                             
067700           IF 4468-IDPSN(FG-INDX) > ZERO                                  
067800             PERFORM UNTIL INDX > FG-MAX-INDX                             
067900               IF 4468-IDPSN(FG-INDX) = TAB-IDPSN(INDX)                   
068000                 ADD 4468-VKORDBTO-KOLLI TO                               
068100                                       TAB-VKORDBTO-KOLLI(INDX)           
068200                 ADD +1                TO TAB-ANTAL-KOLLI-FG(INDX)        
068300                 MOVE +10              TO INDX                            
068400               ELSE                                                       
068500                 IF TAB-IDPSN(INDX) = ZERO                                
068600                   MOVE 4468-IDPSN(FG-INDX) TO TAB-IDPSN(INDX)            
068700                   MOVE 4468-VKORDBTO-KOLLI TO                            
068800                                       TAB-VKORDBTO-KOLLI(INDX)           
068900                   ADD +1              TO TAB-ANTAL-KOLLI-FG(INDX)        
069000                   MOVE +10            TO INDX                            
069100                 END-IF                                                   
069200               END-IF                                                     
069300                                                                          
069400               ADD +1 TO INDX                                             
069500             END-PERFORM                                                  
069600           END-IF                                                         
069700                                                                          
069800           MOVE +1 TO INDX                                                
069900         END-IF                                                           
070000       END-IF                                                             
070100                                                                          
070200       ADD +1 TO FG-INDX                                                  
070300     END-PERFORM                                                          
070400     .                                                                    
070500     EJECT                                                                
070600*CBC-SPARA-GLAS SECTION.                                                  
070700*                                                                         
070800*    IF 4468-KDKOLLI (1:2) = 'UW'                                         
070900*                                                                         
071000*      ADD 4468-VKORDBTO-KOLLI TO SPAR-VKORDBTO-GLAS                      
071100*      ADD +1                  TO WS-ANTAL-KOLLI-GLAS                     
071200*                                                                         
071300*    END-IF                                                               
071400*    .                                                                    
071500*    EJECT                                                                
071600 CBD-SPARA-PARTS SECTION.                                                 
071700                                                                          
071800     ADD 4468-VKORDBTO-KOLLI   TO SPAR-VKORDBTO-PARTS                     
071900     ADD +1                    TO WS-ANTAL-KOLLI-PARTS                    
072000     .                                                                    
072100     EJECT                                                                
072200 CBE-SPARA-IDORDNR SECTION.                                               
072300                                                                          
072400     IF IX <= IDORDER-MAX-INDX                                            
072500                                                                          
072600       IF 4468-IDKUNDRF = SPAR-IDKUNDRF                                   
072700         CONTINUE                                                         
072800       ELSE                                                               
072900         MOVE 4468-IDKUNDRF (3:5) TO FINAL1-IDORDNR (IX)                  
073000         MOVE 4468-IDKUNDRF     TO SPAR-IDKUNDRF                          
073100                                                                          
073200         ADD +1                 TO IX                                     
073300       END-IF                                                             
073400     END-IF                                                               
073500     .                                                                    
073600     EJECT                                                                
073700 CBF-SPARA-RETUREMB SECTION.                                              
073800                                                                          
073900     IF 4468-KDKOLLI = 'UBT1' OR                                          
074000                       'UBT2'                                             
074100       ADD +1 TO WS-ANTAL-KOLLI-RETUREMB                                  
074200     END-IF                                                               
074300     .                                                                    
074400     EJECT                                                                
074500 CC-SKRIV-BILL-OF-LADING SECTION.                                         
074600                                                                          
074700     MOVE MID-IDDC        TO WS-IDDC                                      
074800     PERFORM CCA-SKRIV-HUVUD                                              
074900                                                                          
075000     MOVE +1              TO INDX                                         
075100     IF TAB-IDPSN(INDX) > ZERO                                            
075200       PERFORM UNTIL INDX > FG-MAX-INDX                                   
075300                                                                          
075400         IF TAB-IDPSN(INDX) > ZERO                                        
075500           PERFORM CCB-SKRIV-FG-RADER                                     
075600         END-IF                                                           
075700                                                                          
075800         ADD +1           TO INDX                                         
075900       END-PERFORM                                                        
076000     END-IF                                                               
076100                                                                          
076200*    IF SPAR-VKORDBTO-GLAS > ZERO                                         
076300*      PERFORM CCC-SKRIV-GLAS-RAD                                         
076400*    END-IF                                                               
076500                                                                          
076600     IF WS-ANTAL-KOLLI-PARTS > ZERO                                       
076700       PERFORM CCD-SKRIV-PARTS-RAD                                        
076800     END-IF                                                               
076900                                                                          
077000     PERFORM CCE-SKRIV-FOTEN                                              
077100     .                                                                    
077200     EJECT                                                                
077300 CCA-SKRIV-HUVUD SECTION.                                                 
077400                                                                          
077500     MOVE SPACE           TO WS-LIST-RAD                                  
077600     MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                                  
077700     PERFORM S03-SKRIV                                                    
077800                                                                          
077900     MOVE SPACE           TO WS-LIST-RAD                                  
078000     MOVE PRT-AFTER-9     TO PRT-RADSKIP                                  
078100     PERFORM S03-SKRIV                                                    
078200                                                                          
078300     MOVE SPACE           TO WS-LIST-RAD                                  
078400     MOVE PRT-AFTER-9     TO PRT-RADSKIP                                  
078500     PERFORM S03-SKRIV                                                    
078600                                                                          
078700     MOVE RUBRIKRAD-1     TO WS-LIST-RAD                                  
078800     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
078900     PERFORM S03-SKRIV                                                    
079000                                                                          
079100     MOVE RUBRIKRAD-2     TO WS-LIST-RAD                                  
079200     MOVE PRT-AFTER-3     TO PRT-RADSKIP                                  
079300     PERFORM S03-SKRIV                                                    
079400                                                                          
079500     MOVE RUBRIKRAD-3     TO WS-LIST-RAD                                  
079600     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
079700     PERFORM S03-SKRIV                                                    
079800                                                                          
079900     MOVE RUBRIKRAD-4     TO WS-LIST-RAD                                  
080000     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
080100     PERFORM S03-SKRIV                                                    
080200                                                                          
080300     MOVE RUBRIKRAD-4A    TO WS-LIST-RAD                                  
080400     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
080500     PERFORM S03-SKRIV                                                    
080600                                                                          
080700     MOVE RUBRIKRAD-5     TO WS-LIST-RAD                                  
080800     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
080900     PERFORM S03-SKRIV                                                    
081000                                                                          
081100     MOVE RUBRIKRAD-6     TO WS-LIST-RAD                                  
081200     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
081300     PERFORM S03-SKRIV                                                    
081400                                                                          
081500     MOVE SPACE           TO WS-LIST-RAD                                  
081600     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
081700     PERFORM S03-SKRIV                                                    
081800     .                                                                    
081900     EJECT                                                                
082000 CCB-SKRIV-FG-RADER SECTION.                                              
082100                                                                          
082200     MOVE TAB-ANTAL-KOLLI-FG (INDX)  TO LISTRAD-ANTAL-KOLLI               
082300     MOVE WS-RADNR                   TO LISTRAD-RADNR                     
082400     MOVE WS-KRYSS                   TO LISTRAD-KRYSS                     
082500*                                                                         
082600     IF NDC-CA                                                            
082700       ADD TAB-VKORDBTO-KOLLI (INDX) TO SPAR-VKORDBTO-TOTAL               
082800                                                                          
082900       MOVE TAB-VKORDBTO-KOLLI (INDX)                                     
083000                                     TO LISTRAD-VKORDBTO                  
083100                                                                          
083200       MOVE WS-KG                    TO LISTRAD-KG                        
083300     ELSE                                                                 
083400       MOVE ZERO                     TO WS-VKORDBTO-LB                    
083500       COMPUTE WS-VKORDBTO-LB ROUNDED =                                   
083600                              TAB-VKORDBTO-KOLLI (INDX) *                 
083700                              CONV-KG-TO-LB                               
083800       ADD WS-VKORDBTO-LB            TO SPAR-VKORDBTO-LB-TOTAL            
083900                                                                          
084000       MOVE WS-VKORDBTO-LB           TO LISTRAD-VKORDBTO                  
084100                                                                          
084200       MOVE SPACE                    TO LISTRAD-KG                        
084300     END-IF                                                               
084400*                                                                         
084500                                                                          
084600     MOVE TAB-IDPSN (INDX)           TO W-IDPSN                           
084700                                                                          
084800     PERFORM IMS-GU-WL116512-BEPSN                                        
084900                                                                          
085000     IF SEGMENT-FINNS                                                     
085100       IF 1168-BEPSN (1) NOT = SPACE                                      
085200                                                                          
085300         MOVE 1168-BEPSN (1)         TO LISTRAD-BENAEMN1                  
085400         ADD +1                      TO WS-ANTAL-RADER                    
085500                                          WS-RADNR                        
085600                                                                          
085700         MOVE DETALJRAD1             TO WS-LIST-RAD                       
085800         MOVE PRT-AFTER-1            TO PRT-RADSKIP                       
085900         PERFORM S03-SKRIV                                                
086000                                                                          
086100         IF 1168-BEPSN (2) NOT = SPACE                                    
086200                                                                          
086300           MOVE 1168-BEPSN(2)        TO LISTRAD-BENAEMN2                  
086400           ADD +1                    TO WS-ANTAL-RADER                    
086500                                                                          
086600           MOVE DETALJRAD2           TO WS-LIST-RAD                       
086700           MOVE PRT-AFTER-1          TO PRT-RADSKIP                       
086800           PERFORM S03-SKRIV                                              
086900                                                                          
087000           IF 1168-BEPSN (3) NOT = SPACE                                  
087100                                                                          
087200             MOVE 1168-BEPSN(3)      TO LISTRAD-BENAEMN2                  
087300             ADD +1                  TO WS-ANTAL-RADER                    
087400                                                                          
087500             MOVE DETALJRAD2         TO WS-LIST-RAD                       
087600             MOVE PRT-AFTER-1        TO PRT-RADSKIP                       
087700             PERFORM S03-SKRIV                                            
087800                                                                          
087900           END-IF                                                         
088000         END-IF                                                           
088100       ELSE                                                               
088200         ADD +1                      TO WS-ANTAL-RADER                    
088300                                        WS-RADNR                          
088400                                                                          
088500         MOVE SPACE                  TO LISTRAD-BENAEMN1                  
088600         MOVE DETALJRAD1             TO WS-LIST-RAD                       
088700         MOVE PRT-AFTER-1            TO PRT-RADSKIP                       
088800         PERFORM S03-SKRIV                                                
088900       END-IF                                                             
089000                                                                          
089100     ELSE                                                                 
089200       MOVE 'UNKNOWN PSN'            TO LISTRAD-BENAEMN1                  
089300       ADD +1                        TO WS-ANTAL-RADER                    
089400                                        WS-RADNR                          
089500                                                                          
089600       MOVE DETALJRAD1               TO WS-LIST-RAD                       
089700       MOVE PRT-AFTER-1              TO PRT-RADSKIP                       
089800       PERFORM S03-SKRIV                                                  
089900     END-IF                                                               
090000                                                                          
090100     ADD TAB-ANTAL-KOLLI-FG (INDX)   TO SPAR-ANTAL-KOLLI-TOT              
090200     .                                                                    
090300     EJECT                                                                
090400*CCC-SKRIV-GLAS-RAD SECTION.                                              
090500*                                                                         
090600*    MOVE WS-ANTAL-KOLLI-GLAS        TO LISTRAD-ANTAL-KOLLI               
090700*    MOVE WS-RADNR                   TO LISTRAD-RADNR                     
090800*    MOVE GLAS-TEXT1                 TO LISTRAD-BENAEMN1                  
090900*                                                                         
091000*    IF NDC-CA                                                            
091100*      ADD SPAR-VKORDBTO-GLAS        TO SPAR-VKORDBTO-TOTAL               
091200*      MOVE SPAR-VKORDBTO-GLAS       TO LISTRAD-VKORDBTO                  
091300*      MOVE WS-KG                    TO LISTRAD-KG                        
091400*                                                                         
091500*    ELSE                                                                 
091600*      MOVE ZERO                     TO WS-VKORDBTO-LB                    
091700*      COMPUTE WS-VKORDBTO-LB ROUNDED =                                   
091800*                             SPAR-VKORDBTO-GLAS *                        
091900*                             CONV-KG-TO-LB                               
092000*      MOVE SPACE                    TO LISTRAD-KG                        
092100*                                                                         
092200*      ADD WS-VKORDBTO-LB            TO SPAR-VKORDBTO-LB-TOTAL            
092300*                                                                         
092400*      MOVE WS-VKORDBTO-LB           TO LISTRAD-VKORDBTO                  
092500*    END-IF                                                               
092600*                                                                         
092700*    MOVE DETALJRAD1                 TO WS-LIST-RAD                       
092800*    IF WS-RADNR < +6                                                     
092900*      MOVE PRT-AFTER-2              TO PRT-RADSKIP                       
093000*    ELSE                                                                 
093100*      MOVE PRT-AFTER-1              TO PRT-RADSKIP                       
093200*    END-IF                                                               
093300*    PERFORM S03-SKRIV                                                    
093400*                                                                         
093500*    MOVE GLAS-TEXT2                 TO LISTRAD-BENAEMN2                  
093600*    MOVE DETALJRAD2                 TO WS-LIST-RAD                       
093700*    MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
093800*    PERFORM S03-SKRIV                                                    
093900*                                                                         
094000*    ADD WS-ANTAL-KOLLI-GLAS         TO SPAR-ANTAL-KOLLI-TOT              
094100*    ADD +1                          TO WS-RADNR                          
094200*                                       WS-ANTAL-RADER                    
094300*    .                                                                    
094400*    EJECT                                                                
094500 CCD-SKRIV-PARTS-RAD SECTION.                                             
094600                                                                          
094700     MOVE WS-ANTAL-KOLLI-PARTS       TO LISTRAD-ANTAL-KOLLI               
094800     MOVE WS-RADNR                   TO LISTRAD-RADNR                     
094900     MOVE SPACE                      TO LISTRAD-KRYSS                     
095000     MOVE PARTS-TEXT                 TO LISTRAD-BENAEMN1                  
095100*                                                                         
095200     IF NDC-CA                                                            
095300       ADD SPAR-VKORDBTO-PARTS       TO SPAR-VKORDBTO-TOTAL               
095400       MOVE SPAR-VKORDBTO-PARTS      TO LISTRAD-VKORDBTO                  
095500       MOVE WS-KG                    TO LISTRAD-KG                        
095600                                                                          
095700     ELSE                                                                 
095800       MOVE ZERO                     TO WS-VKORDBTO-LB                    
095900       COMPUTE WS-VKORDBTO-LB ROUNDED =                                   
096000                              SPAR-VKORDBTO-PARTS *                       
096100                              CONV-KG-TO-LB                               
096200       ADD WS-VKORDBTO-LB            TO SPAR-VKORDBTO-LB-TOTAL            
096300                                                                          
096400       MOVE WS-VKORDBTO-LB           TO LISTRAD-VKORDBTO                  
096500       MOVE SPACE                    TO LISTRAD-KG                        
096600     END-IF                                                               
096700*                                                                         
096800                                                                          
096900     MOVE DETALJRAD1                 TO WS-LIST-RAD                       
097000     IF WS-RADNR < +7                                                     
097100       MOVE PRT-AFTER-2              TO PRT-RADSKIP                       
097200     ELSE                                                                 
097300       MOVE PRT-AFTER-1              TO PRT-RADSKIP                       
097400     END-IF                                                               
097500     PERFORM S03-SKRIV                                                    
097600                                                                          
097700     ADD WS-ANTAL-KOLLI-PARTS        TO SPAR-ANTAL-KOLLI-TOT              
097800     ADD +1                          TO WS-RADNR                          
097900                                        WS-ANTAL-RADER                    
098000     .                                                                    
098100     EJECT                                                                
098200 CCE-SKRIV-FOTEN SECTION.                                                 
098300                                                                          
098400     MOVE SPAR-ANTAL-KOLLI-TOT       TO TOTAL-ANTAL-KOLLI                 
098410*    MOVE SPACE                      TO TOTAL-ANTAL-KOLLI                 
098500                                                                          
098600     IF NDC-CA                                                            
098700       MOVE SPAR-VKORDBTO-TOTAL      TO TOTAL-VKORDBTO                    
098800       MOVE WS-KG                    TO TOTAL-KG                          
098900     ELSE                                                                 
099000       MOVE SPAR-VKORDBTO-LB-TOTAL   TO TOTAL-VKORDBTO                    
099100       MOVE SPACE                    TO TOTAL-KG                          
099200     END-IF                                                               
099300                                                                          
099400     IF WS-ANTAL-RADER < MAX-ANTAL-RADER                                  
099500                                                                          
099600       MOVE PRT-EQUAL-49             TO PRT-RADSKIP                       
099700       MOVE SPACE                    TO WS-LIST-RAD                       
099800       PERFORM S03-SKRIV                                                  
099900                                                                          
100000       MOVE TOTALRAD                 TO WS-LIST-RAD                       
100100       MOVE PRT-AFTER-1              TO PRT-RADSKIP                       
100200       PERFORM S03-SKRIV                                                  
100300                                                                          
100400                                                                          
100500     ELSE                                                                 
100600       MOVE TOTALRAD                 TO WS-LIST-RAD                       
100700       MOVE PRT-AFTER-1              TO PRT-RADSKIP                       
100800       PERFORM S03-SKRIV                                                  
100900                                                                          
101000     END-IF                                                               
101100                                                                          
101200     MOVE FINALRAD1                  TO WS-LIST-RAD                       
101300     MOVE PRT-AFTER-3                TO PRT-RADSKIP                       
101400     PERFORM S03-SKRIV                                                    
101500                                                                          
101600     MOVE WS-ANTAL-KOLLI-RETUREMB    TO FINAL2-KDKOLLI-EMB                
101700     MOVE FINALRAD2                  TO WS-LIST-RAD                       
101800     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
101900     PERFORM S03-SKRIV                                                    
102000     .                                                                    
102100     EJECT                                                                
102200 CD-SKRIV-BILL-OF-LADING-WEB SECTION.                                     
102300                                                                          
102400     MOVE MID-IDDC        TO WS-IDDC                                      
102500     PERFORM CDA-SKRIV-HUVUD-WEB                                          
102600                                                                          
102700     MOVE +1              TO INDX                                         
102800     IF TAB-IDPSN(INDX) > ZERO                                            
102900       PERFORM UNTIL INDX > FG-MAX-INDX                                   
103000                                                                          
103100         IF TAB-IDPSN(INDX) > ZERO                                        
103200           PERFORM CDB-SKRIV-FG-RADER-WEB                                 
103300         END-IF                                                           
103400                                                                          
103500         ADD +1           TO INDX                                         
103600       END-PERFORM                                                        
103700     END-IF                                                               
103800                                                                          
103900     IF WS-ANTAL-KOLLI-PARTS > ZERO                                       
104000       PERFORM CDD-SKRIV-PARTS-RAD-WEB                                    
104100     END-IF                                                               
104200                                                                          
104300     PERFORM CDE-SKRIV-FOTEN-WEB                                          
104400     .                                                                    
104500     EJECT                                                                
104600 CDA-SKRIV-HUVUD-WEB SECTION.                                             
104700                                                                          
104800     MOVE '1'              TO HUVUD-IDAFPRCD                              
104900     MOVE RUB1-IDKUNDNR    TO HUVUD-IDKUNDNR                              
105000     MOVE RUB1-KDORDKL     TO HUVUD-KDORDKL                               
105100     MOVE RUB1-IDORDNR     TO HUVUD-IDORDNR                               
105200     MOVE DAGENS-DATUM     TO HUVUD-DATUM                                 
105300     MOVE RUB3-BETRANSP    TO HUVUD-IDLBBET                               
105400     MOVE RUB4-BEGMT-RAD1  TO HUVUD-BEGMT-RAD1                            
105500     MOVE RUB4-BEGMT-RAD2  TO HUVUD-BEGMT-RAD2                            
105600     MOVE RUB5-ADGMT-GATA  TO HUVUD-ADGMT-GATA                            
105700     MOVE RUB6-ADGMT-PADR  TO HUVUD-ADGMT-PADR                            
105800                                                                          
105900     PERFORM S92-PUT-DOC-HEAD                                             
106000     .                                                                    
106100     EJECT                                                                
106200 CDB-SKRIV-FG-RADER-WEB SECTION.                                          
106300                                                                          
106400     MOVE '2'                        TO RAD-IDAFPRCD                      
106500     MOVE TAB-ANTAL-KOLLI-FG (INDX)  TO RAD-ANTAL-KOLLI                   
106600     MOVE WS-RADNR                   TO RAD-RADNR                         
106700     MOVE WS-KRYSS                   TO RAD-KRYSS                         
106800*                                                                         
106900     IF NDC-CA                                                            
107000       ADD TAB-VKORDBTO-KOLLI (INDX) TO SPAR-VKORDBTO-TOTAL               
107100                                                                          
107200       MOVE TAB-VKORDBTO-KOLLI (INDX)                                     
107300                                     TO RAD-VKORDBTO                      
107400       MOVE WS-KG                    TO RAD-KG                            
107500     ELSE                                                                 
107600       MOVE ZERO                     TO WS-VKORDBTO-LB                    
107700       COMPUTE WS-VKORDBTO-LB ROUNDED =                                   
107800                              TAB-VKORDBTO-KOLLI (INDX) *                 
107900                              CONV-KG-TO-LB                               
108000       ADD WS-VKORDBTO-LB            TO SPAR-VKORDBTO-LB-TOTAL            
108100                                                                          
108200       MOVE WS-VKORDBTO-LB           TO RAD-VKORDBTO                      
108300       MOVE WS-LBS                   TO RAD-KG                            
108400     END-IF                                                               
108500*                                                                         
108600                                                                          
108700     MOVE TAB-IDPSN (INDX)           TO W-IDPSN                           
108800                                                                          
108900     PERFORM IMS-GU-WL116512-BEPSN                                        
109000                                                                          
109100     IF SEGMENT-FINNS                                                     
109200       IF 1168-BEPSN (1) NOT = SPACE                                      
109300                                                                          
109400         MOVE 1168-BEPSN (1)         TO RAD-BENAEMN1                      
109500         ADD +1                      TO WS-ANTAL-RADER                    
109600                                        WS-RADNR                          
109700                                                                          
109800         IF 1168-BEPSN (2) NOT = SPACE                                    
109900                                                                          
110000           MOVE 1168-BEPSN(2)        TO RAD-BENAEMN2                      
110100           ADD +1                    TO WS-ANTAL-RADER                    
110200                                                                          
110300           PERFORM S93-PUT-DOC-LINE                                       
110400                                                                          
110500           IF 1168-BEPSN (3) NOT = SPACE                                  
110600                                                                          
110700             MOVE SPACE              TO RAD-W476BL2                       
110800             MOVE 1168-BEPSN(3)      TO RAD-BENAEMN2                      
110900             ADD +1                  TO WS-ANTAL-RADER                    
111000                                                                          
111100             PERFORM S93-PUT-DOC-LINE                                     
111200                                                                          
111300           END-IF                                                         
111400         ELSE                                                             
111500           PERFORM S93-PUT-DOC-LINE                                       
111600         END-IF                                                           
111700                                                                          
111800       ELSE                                                               
111900         ADD +1                      TO WS-ANTAL-RADER                    
112000                                        WS-RADNR                          
112100                                                                          
112200         MOVE SPACE                  TO RAD-BENAEMN1                      
112300         PERFORM S93-PUT-DOC-LINE                                         
112400       END-IF                                                             
112500                                                                          
112600     ELSE                                                                 
112700       MOVE 'UNKNOWN PSN'            TO RAD-BENAEMN1                      
112800       ADD +1                        TO WS-ANTAL-RADER                    
112900                                        WS-RADNR                          
113000                                                                          
113100       PERFORM S93-PUT-DOC-LINE                                           
113200     END-IF                                                               
113300                                                                          
113400     ADD TAB-ANTAL-KOLLI-FG (INDX)   TO SPAR-ANTAL-KOLLI-TOT              
113500     .                                                                    
113600     EJECT                                                                
113700 CDD-SKRIV-PARTS-RAD-WEB SECTION.                                         
113800                                                                          
113900     MOVE '2'                        TO RAD-IDAFPRCD                      
114000     MOVE WS-ANTAL-KOLLI-PARTS       TO RAD-ANTAL-KOLLI                   
114100     MOVE WS-RADNR                   TO RAD-RADNR                         
114200     MOVE SPACE                      TO RAD-KRYSS                         
114300                                        RAD-BENAEMN2                      
114400     MOVE PARTS-TEXT                 TO RAD-BENAEMN1                      
114500*                                                                         
114600     IF NDC-CA                                                            
114700       ADD SPAR-VKORDBTO-PARTS       TO SPAR-VKORDBTO-TOTAL               
114800       MOVE SPAR-VKORDBTO-PARTS      TO RAD-VKORDBTO                      
114900       MOVE WS-KG                    TO RAD-KG                            
115000                                                                          
115100     ELSE                                                                 
115200       MOVE ZERO                     TO WS-VKORDBTO-LB                    
115300       COMPUTE WS-VKORDBTO-LB ROUNDED =                                   
115400                              SPAR-VKORDBTO-PARTS *                       
115500                              CONV-KG-TO-LB                               
115600       ADD WS-VKORDBTO-LB            TO SPAR-VKORDBTO-LB-TOTAL            
115700                                                                          
115800       MOVE WS-VKORDBTO-LB           TO RAD-VKORDBTO                      
115900       MOVE WS-LBS                   TO RAD-KG                            
116000     END-IF                                                               
116100*                                                                         
116200                                                                          
116300     PERFORM S93-PUT-DOC-LINE                                             
116400                                                                          
116500     ADD WS-ANTAL-KOLLI-PARTS        TO SPAR-ANTAL-KOLLI-TOT              
116600     ADD +1                          TO WS-RADNR                          
116700                                        WS-ANTAL-RADER                    
116800     .                                                                    
116900     EJECT                                                                
117000 CDE-SKRIV-FOTEN-WEB SECTION.                                             
117100                                                                          
117200     MOVE '3'                        TO FOT-IDAFPRCD                      
117300     MOVE SPAR-ANTAL-KOLLI-TOT       TO FOT-ANTAL-KOLLI                   
117310*    MOVE ZERO                       TO FOT-ANTAL-KOLLI                   
117400                                                                          
117500     IF NDC-CA                                                            
117600       MOVE SPAR-VKORDBTO-TOTAL      TO FOT-VKORDBTO                      
117700       MOVE WS-KG                    TO FOT-KG                            
117800     ELSE                                                                 
117900       MOVE SPAR-VKORDBTO-LB-TOTAL   TO FOT-VKORDBTO                      
118000       MOVE WS-LBS                   TO FOT-KG                            
118100     END-IF                                                               
118200                                                                          
118300     MOVE 1 TO IX                                                         
118400     PERFORM UNTIL IX >  IDORDER-MAX-INDX                                 
118500        MOVE FINAL1-IDORDNR-GRP (IX) TO FOT-IDORDNR-GRP (IX)              
118600        ADD 1 TO IX                                                       
118700     END-PERFORM                                                          
118800                                                                          
118900     MOVE WS-ANTAL-KOLLI-RETUREMB    TO FOT-KDKOLLI-EMB                   
119000                                                                          
119100     MOVE SPAR-IDDISTR               TO TEST-IDDISTR                      
119200     IF NDC-US AND DIST07-USA-CUSTOMERS                                   
119300       MOVE WS-USA-TELNR             TO FOT-BETELNR                       
119400     ELSE                                                                 
119500       MOVE WS-CANADA-TELNR          TO FOT-BETELNR                       
119600     END-IF                                                               
119700                                                                          
119800     MOVE WS-ANTAL-KOLLI-RETUREMB    TO FOT-KDKOLLI-EMB                   
119900                                                                          
120000     PERFORM S94-PUT-DOC-FOOT                                             
120100     .                                                                    
120200     EJECT                                                                
120300 Z-STAENG-PRINTER SECTION.                                                
120400                                                                          
120500     CALL W006PRS1  USING PRT-SPOOL-A4S                                   
120600                          PRT-CLOSE                                       
120700                          WS-PRT-IDPRTLST                                 
120800                          ALT-PCB                                         
120900                          WS-PRT-DUMMY                                    
121000                          WS-PRT-DUMMY                                    
121100                                                                          
121200     .                                                                    
121300     EJECT                                                                
121400 S01-NOLLA-TABELL SECTION.                                                
121500                                                                          
121600     MOVE +1            TO INDX                                           
121700     PERFORM UNTIL INDX > FG-MAX-INDX                                     
121800        MOVE ZERO       TO TAB-IDPSN (INDX)                               
121900                           TAB-VKORDBTO-KOLLI (INDX)                      
122000                           TAB-ANTAL-KOLLI-FG (INDX)                      
122100        ADD +1          TO INDX                                           
122200     END-PERFORM                                                          
122300                                                                          
122400     MOVE +1            TO INDX                                           
122500     PERFORM UNTIL INDX > IDORDER-MAX-INDX                                
122600        MOVE ZERO       TO FINAL1-IDORDNR (INDX)                          
122700        ADD +1          TO INDX                                           
122800     END-PERFORM                                                          
122900                                                                          
123000     MOVE ZERO          TO WS-ANTAL-KOLLI-GLAS                            
123100                           WS-ANTAL-KOLLI-PARTS                           
123200                           WS-ANTAL-KOLLI-RETUREMB                        
123300                           WS-ANTAL-RADER                                 
123400                           WS-VKORDBTO-LB                                 
123500                                                                          
123600                           SPAR-VKORDBTO-GLAS                             
123700                           SPAR-VKORDBTO-PARTS                            
123800                           SPAR-ANTAL-KOLLI-TOT                           
123900                           SPAR-VKORDBTO-TOTAL                            
124000                           SPAR-VKORDBTO-LB-TOTAL                         
124100                                                                          
124200                                                                          
124300     MOVE SPACE         TO SPAR-IDKUNDRF                                  
124400                                                                          
124500     MOVE +1            TO IX                                             
124600                           WS-RADNR                                       
124700     .                                                                    
124800     EJECT                                                                
124900 S02-INIT-PRINTER-ID SECTION.                                             
125000                                                                          
125100     IF MSG-SIGNON-USERID = 'PC59701 '                                    
125200       MOVE '6LPVV1  '                TO WS-PRT-IDPRTLST                  
125300     ELSE                                                                 
125400       MOVE MID-IDDC TO WS-IDDC                                           
125500                                                                          
125600       EVALUATE TRUE                                                      
125700         WHEN NDC-US-RU                                                   
125800           MOVE 'RU7     '            TO WS-PRT-IDPRTLST                  
125900         WHEN NDC-US-LA                                                   
126000           MOVE 'LA5     '            TO WS-PRT-IDPRTLST                  
126100       END-EVALUATE                                                       
126200     END-IF                                                               
126300     .                                                                    
126400     EJECT                                                                
126500 S03-SKRIV SECTION.                                                       
126600                                                                          
126700     CALL W006PRS1 USING PRT-SPOOL-A4S                                    
126800                         PRT-WRITE                                        
126900                         WS-PRT-IDPRTLST                                  
127000                         ALT-PCB                                          
127100                         PRT-RADSKIP                                      
127200                         WS-LIST-RAD                                      
127300     .                                                                    
127400                                                                          
127500 S10-DAP-OPEN SECTION.                                                    
127600                                                                          
127700     MOVE 1                TO HDR-REQU-IDMSGVER                           
127800     MOVE 'R'              TO HDR-REQU-KDPGMACT                           
127900     MOVE MID-IDUSER       TO HDR-REQU-IDUSER                             
128000                                                                          
128100     MOVE SPACE            TO HDR-IDOUTREC                                
128200     MOVE 'BILL-OF-LADING' TO HDR-IDOUTTYPE                               
128300     MOVE MID-IDDC         TO HDR-IDOUTREC(1:2)                           
128400     MOVE MID-IDUSER       TO HDR-IDOUTREC(3:8)                           
128500     MOVE MID-IDTRPTNR     TO HDR-IDLIST(1:3)                             
128600     MOVE MID-IDLBBET      TO HDR-IDLIST(4:7)                             
128700     PERFORM S90-SEND-OPEN                                                
128800     PERFORM S91-PUT-HEADER                                               
128900     SET SW-DAP-OPEN       TO TRUE                                        
129000     .                                                                    
129100                                                                          
129200 S90-SEND-OPEN SECTION.                                                   
129300                                                                          
129400     MOVE WS-ADRESS-DP                    TO SEND-ADDISPABS               
129500     MOVE 'OPEN'                          TO SEND-KDFUNC                  
129600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
129700                         SEND-OPEN-AREA                                   
129800     IF SEND-KDRC > ZERO                                                  
129900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
130000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
130100       DELIMITED BY SIZE INTO FELTEXT                                     
130200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
130300     END-IF                                                               
130400     .                                                                    
130500     SKIP3                                                                
130600 S91-PUT-HEADER SECTION.                                                  
130700                                                                          
130800     MOVE 'PUT'                           TO SEND-KDFUNC                  
130900     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
131000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
131100                         SEND-KVDLEN                                      
131200                         HDR-AREA                                         
131300     IF SEND-KDRC > ZERO                                                  
131400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
131500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
131600       DELIMITED BY SIZE INTO FELTEXT                                     
131700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
131800     END-IF                                                               
131900     .                                                                    
132000     EJECT                                                                
132100 S92-PUT-DOC-HEAD SECTION.                                                
132200                                                                          
132300     MOVE 'PUT'                           TO SEND-KDFUNC                  
132400     MOVE LENGTH OF DOC-HEAD-AREA         TO SEND-KVDLEN                  
132500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
132600                         SEND-KVDLEN                                      
132700                         DOC-HEAD-AREA                                    
132800     IF SEND-KDRC > ZERO                                                  
132900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
133000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
133100       DELIMITED BY SIZE INTO FELTEXT                                     
133200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
133300     END-IF                                                               
133400     .                                                                    
133500     SKIP3                                                                
133600 S93-PUT-DOC-LINE SECTION.                                                
133700                                                                          
133800     MOVE 'PUT'                           TO SEND-KDFUNC                  
133900     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
134000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
134100                         SEND-KVDLEN                                      
134200                         DOC-LINE-AREA                                    
134300     IF SEND-KDRC > ZERO                                                  
134400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
134500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
134600       DELIMITED BY SIZE INTO FELTEXT                                     
134700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
134800     END-IF                                                               
134900     .                                                                    
135000     EJECT                                                                
135100 S94-PUT-DOC-FOOT SECTION.                                                
135200                                                                          
135300     MOVE 'PUT'                           TO SEND-KDFUNC                  
135400     MOVE LENGTH OF DOC-FOOT-AREA         TO SEND-KVDLEN                  
135500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
135600                         SEND-KVDLEN                                      
135700                         DOC-FOOT-AREA                                    
135800     IF SEND-KDRC > ZERO                                                  
135900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
136000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
136100       DELIMITED BY SIZE INTO FELTEXT                                     
136200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
136300     END-IF                                                               
136400     .                                                                    
136500     EJECT                                                                
136600 S95-SEND-CLOSE SECTION.                                                  
136700                                                                          
136800     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
136900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
137000     IF SEND-KDRC > ZERO                                                  
137100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
137200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
137300       DELIMITED BY SIZE INTO FELTEXT                                     
137400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
137500     END-IF                                                               
137600     .                                                                    
137700     EJECT                                                                
137800                                                                          
137900* --- IMS SEKTIONER ---                                                   
138000     SKIP3                                                                
138100 IMS-GET-MSG SECTION.                                                     
138200                                                                          
138300     MOVE '  QC' TO GODK-STATUSKODER                                      
138400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
138500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
138600     PERFORM IMS-STATUSKONTROLL                                           
138700     .                                                                    
138800     SKIP3                                                                
138900 IMS-GU-WL446321-TRANSP SECTION.                                          
139000                                                                          
139100     STRING 'WL446301(WDGXKEY  =' W-4463-X ')'                            
139200          DELIMITED BY SIZE INTO SSA1                                     
139300     STRING 'WL446311(DASKEPPN =' W-DASKEPPN-X ')'                        
139400          DELIMITED BY SIZE INTO SSA2                                     
139500     STRING 'WL446321(KY4466   =' W-4466-X ')'                            
139600          DELIMITED BY SIZE INTO SSA3                                     
139700     MOVE '  GE' TO GODK-STATUSKODER                                      
139800     CALL CBLTDLI USING GU 4463-PCB DLI-IO-AREA-4466                      
139900                        SSA1 SSA2 SSA3                                    
140000     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
140100     PERFORM IMS-STATUSKONTROLL                                           
140200     .                                                                    
140300     EJECT                                                                
140400 IMS-GNP-WL446331-KOLLI SECTION.                                          
140500                                                                          
140600     MOVE 'WL446331 ' TO SSA1                                             
140700     MOVE '  GE' TO GODK-STATUSKODER                                      
140800     CALL CBLTDLI USING GNP 4463-PCB DLI-IO-AREA-4468 SSA1                
140900     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
141000     PERFORM IMS-STATUSKONTROLL                                           
141100     .                                                                    
141200     SKIP3                                                                
141300 IMS-GU-ORQI-WDQ201 SECTION.                                              
141400                                                                          
141500     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
141600          DELIMITED BY SIZE INTO SSA1                                     
141700     MOVE '  GE'               TO GODK-STATUSKODER                        
141800     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV SSA1                 
141900     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
142000     PERFORM IMS-STATUSKONTROLL                                           
142100     .                                                                    
142200     EJECT                                                                
142300 IMS-GU-WL116512-BEPSN SECTION.                                           
142400                                                                          
142500     STRING 'WL116501(WDGXKEY  =' W-1165-X ')'                            
142600          DELIMITED BY SIZE INTO SSA1                                     
142700     STRING 'WL116512(KDFGTRP  =' W-KDFGTRP-X ')'                         
142800          DELIMITED BY SIZE INTO SSA2                                     
142900     MOVE '  GE' TO GODK-STATUSKODER                                      
143000     CALL CBLTDLI USING GU 1165-PCB DLI-IO-AREA-1168 SSA1 SSA2            
143100     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
143200     PERFORM IMS-STATUSKONTROLL                                           
143300     .                                                                    
143400     SKIP3                                                                
143500 IMS-STATUSKONTROLL SECTION.                                              
143600                                                                          
143700     SET STATUS-IX TO 1                                                   
143800     SEARCH GODK-STATUS                                                   
143900       AT END                                                             
144000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
144100         DELIMITED BY SIZE INTO FELTEXT                                   
144200         CALL FELLOG                                                      
144300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
144400         CONTINUE                                                         
144500     END-SEARCH                                                           
144600     .                                                                    
