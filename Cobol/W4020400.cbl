000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4020400.                                                
000400 AUTHOR.         GERRY CARMICHAEL.                                        
000500 DATE-WRITTEN.   90/08/08.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR ANNULLATION/ÄNDRING AV ORDER-                
001100*        RADER OM DE EJ ÄR UTSKRIVNA.                                     
001200*        OM ANNULLATION AV HEL ORDER ANROPAS BACKGRUNDS-                  
001300*        MPP W4029200.                                                    
001400*                                                                         
001500*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001600*        PROGRAMMET UPPDATERAR WLORQI (WDQ2)                              
001700*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)                              
001800*        PROGRAMMET UPPDATERAR WLARTM (WDK9)                              
001900*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
002000*        PROGRAMMET UPPDATERAR WLORQM (WDQ1)                              
002100*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)                              
002200*        PROGRAMMET UPPDATERAR        (WDM2)                              
002300*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002400*        PROGRAMMET LÄSER      WLORQA (WDQ3)                              
002500*                                                                         
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W4T204                                              
002900*        MID:         W4I20401                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W4O20401                                            
003300*        TRANSAKTION: W4T292X                                             
003400*        TRANSAKTION: W2T109X                                             
003500* ETRACK 1290414 INTERVALL FOR DISTRICT AND CUSTOMER                      
003600*                                                                         
003700*HÖSTEN 2004 GÖRAN KJELLSON                                               
003800*ETRACKER 887753                                                          
003900*                                                                         
004000*  SEPT 2005 LINDA NILSSON                                                
004100*  ETRACKER 1334295                                                       
004200*  E-TRACKER: 7450328  2008-HÖST  VOHF                                    
004300*  E-TRACKER: 10254592 2015       DECOMISSION VOHF                        
004400*  E-TRACKER: 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2        
004500*                                                                         
004600     SKIP3                                                                
004700 ENVIRONMENT DIVISION.                                                    
004800     EJECT                                                                
004900 DATA DIVISION.                                                           
005000 WORKING-STORAGE SECTION.                                                 
005100*    -- CHECKED BY WY2000                                                 
005200     SKIP3                                                                
005300 77  IDPGM                       PIC X(08)   VALUE 'W4020400'.            
005400 77  FELTEXT                     PIC X(80)    VALUE SPACE.                
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  YES                         PIC X       VALUE 'Y'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800                                                                          
005900*01  -COPY WWDCKONS                                                       
006000                                                                          
006100*01  -COPY WWBYT03                                                        
006200                                                                          
006300 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
006400                                                                          
006500 77  DIRLEV-KOLL                 PIC X       VALUE 'N'.                   
006600 01  SW-WLARTC-LAST              PIC X       VALUE 'N'.                   
006700                                                                          
006800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
007000 77  MAX-INDX                    PIC S9(4)  VALUE +6    COMP SYNC.        
007100                                                                          
007200 77  2109-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
007300 77  MAX-2109-INDX               PIC S9(4)  VALUE +18   COMP SYNC.        
007400                                                                          
007500 77  AVSR-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
007600 77  IX-CD-OMR                   PIC S9(9)  VALUE ZERO  COMP-3.           
007700                                                                          
007800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
007900                                                                          
008000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008100 01  WS-TIHHMMSS                 PIC 9(6)    VALUE ZERO.                  
008200 01  FILLER REDEFINES WS-TIHHMMSS.                                        
008300     03 WS-TIHHMM                PIC 9(4).                                
008400     03 FILLER                   PIC 9(2).                                
008500                                                                          
008600 01  WS-9KOMPL-DATUM             PIC 9(8).                                
008700 01  FILLER REDEFINES WS-9KOMPL-DATUM.                                    
008800     03 WS-CENTURY               PIC 9(2).                                
008900     03 WS-AAMMDD                PIC 9(6).                                
009000 77  WS-9KOMPL                   PIC 9(9)    VALUE 999999999.             
009100 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
009200 77  WS-IDDISTR-NUM              PIC 9(5)    VALUE ZERO.                  
009300 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
009400 77  WS-IDKUNDNR-NUM             PIC 9(7)    VALUE ZERO.                  
009500 77  WS-IDKUNDRF                 PIC X(7)    VALUE SPACE.                 
009600 77  WS-IDKUNDRF-NUM             PIC 9(7)    VALUE ZERO.                  
009700 01  WS-IDKUNDRF-RED.                                                     
009800     03 WS-IDKUNDRF-1-7          PIC X(7)    VALUE SPACE.                 
009900     03 WS-IDKUNDRF-8-10         PIC X(3)    VALUE SPACE.                 
010000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
010100 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
010200 77  WS-TINUDAT                  PIC S9(7)  COMP-3.                       
010300 77  WS-TINUTID                  PIC S9(9)  COMP-3.                       
010400 77  WS-SEK                      PIC X(3)   VALUE 'SEK'.                  
010500                                                                          
010600     EJECT                                                                
010700 01 TABELL.                                                               
010800     03 PRARTNTO-TABELL OCCURS 6.                                         
010900        05 WS-PRARTNTO           PIC S9(7)V9(2) VALUE +0 COMP-3.          
011000 01 TABELL.                                                               
011100     03 KVBEART-Q-TABELL OCCURS 6.                                        
011200        05 WS-KVBEART-Q          PIC S9(7)    VALUE +0  COMP-3.           
011300 77  KVRADER-LOR-RAKNARE         PIC S9(5)    VALUE +0  COMP-3.           
011400 77  KVRADER-ODEL-RAKNARE        PIC S9(5)    VALUE +0  COMP-3.           
011500 77  KVRADER-UP-RAKNARE          PIC S9(5)    VALUE +0  COMP-3.           
011600 77  KVRADER-DIRL-RAKNARE        PIC S9(5)    VALUE +0  COMP-3.           
011700 77  TOTAL-KVRADER               PIC S9(5)    VALUE +0  COMP-3.           
011800 77  TOTAL-UTSKRIVNA             PIC S9(5)    VALUE +0  COMP-3.           
011900 77  MOJLIG-ANTAL                PIC S9(5)    VALUE +0  COMP-3.           
012000 77  MINSKAT-ANTAL               PIC S9(7)    VALUE +0  COMP-3.           
012100 77  ANNULLERAT-ANTAL            PIC S9(7)    VALUE +0  COMP-3.           
012200 77  WS-ANTOBKR                  PIC S9(3)    VALUE +0  COMP-3.           
012300 77  WS-KVQPACK-1                PIC S9(5)    VALUE +0  COMP-3.           
012400 77  WS-TIDISPIN                 PIC 9(7)     VALUE ZERO.                 
012500 77  WS-IDORDER                  PIC S9(7)    VALUE +0  COMP-3.           
012600                                                                          
012700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
012800     88  INDATA-OK                           VALUE 'J'.                   
012900     88  INDATA-FEL                          VALUE 'N'.                   
013000                                                                          
013100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013200     88  NYCKLAR-OK                          VALUE 'J'.                   
013300     88  NYCKLAR-FEL                         VALUE 'N'.                   
013400                                                                          
013500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
013600     88  ALLT-OK                             VALUE 'J'.                   
013700                                                                          
013800 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
013900     88  FIRST-TIME                          VALUE 'J'.                   
014000                                                                          
014100 77  DIRLEV-SW                   PIC X       VALUE 'J'.                   
014200     88  DIRLEV-RADER                        VALUE 'J'.                   
014300                                                                          
014400 77  KVBEART-SW                  PIC X       VALUE 'J'.                   
014500     88  KVBEART-OK                          VALUE 'J'.                   
014600                                                                          
014700 77  AVSR-SW                     PIC X       VALUE 'J'.                   
014800     88  AVSR-OK                             VALUE 'J'.                   
014900                                                                          
015000 77  RAD-SW                      PIC X       VALUE 'J'.                   
015100     88  RAD-AENDRING                        VALUE 'J'.                   
015200                                                                          
015300 77  IFYLLT-SW                   PIC X       VALUE 'J'.                   
015400     88  IFYLLT-OK                           VALUE 'J'.                   
015500                                                                          
015600 77  SKRIV-SW                    PIC X       VALUE 'J'.                   
015700     88  SKRIV-OK                            VALUE 'J'.                   
015800                                                                          
015900 77  ANNULL-SW                   PIC X       VALUE 'J'.                   
016000     88  ANNULL-HELORDER                     VALUE 'J'.                   
016100                                                                          
016200 77  STATUS-SW                   PIC X       VALUE 'J'.                   
016300     88  STATUS-OK                           VALUE 'J'.                   
016400                                                                          
016500 77  CMD-SW                      PIC X       VALUE 'J'.                   
016600     88  CMD-OK                              VALUE 'J'.                   
016700     88  CMD-FEL                             VALUE 'N'.                   
016800                                                                          
016900 77  IDARTNR-SW                  PIC X       VALUE 'J'.                   
017000     88  IDARTNR-IFYLLT                      VALUE 'J'.                   
017100                                                                          
017200 77  SPAR-ORAD-KVBEART-Q         PIC S9(7)   VALUE +0  COMP-3.            
017300 77  SPAR-ORAD-KVPRERO           PIC S9(7)   VALUE +0  COMP-3.            
017400 77  SPAR-ORAD-KVPREAVB          PIC S9(7)   VALUE +0  COMP-3.            
017500     SKIP3                                                                
017600 01 DB2-LASNING.                                                          
017700     03 FILLER                   PIC X(16)   VALUE                        
017800                                             'WS-DB2-SEKTION'.            
017900     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
018000                                                                          
018100     EJECT                                                                
018200 01 NYCKLAR-TP4TRAN.                                                      
018300     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
018400                                                                          
018500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
018600     88  EGEN-MID                            VALUE '4204'.                
018700     88  GODK-MID                            VALUE '4204'.                
018800                                                                          
018900                                                                          
019000 77  EGEN-IDTRANS                PIC X(4)    VALUE '4204'.                
019100     EJECT                                                                
019200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
019300 01  GENERELLA-SUBPROGRAM.                                                
019400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
019500     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
019600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
019700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
019800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
020100                                                                          
020200 01  GEMENSAMMA-SUBPROGRAM.                                               
020300     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
020400     03  W413AVSO                PIC X(8)    VALUE 'W413AVSO'.            
020500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
020600     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
020700     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
020800     EJECT                                                                
020900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
021000*01 -COPY WMSGINIT                                                        
021100     EJECT                                                                
021200*    --- PARAMETRAR TILL ABEND                                            
021300 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
021400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
021500*   -COPY WMEDAREA                                                        
021600     SKIP3                                                                
021700*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
021800*   -COPY WDATAREA                                                        
021900     SKIP3                                                                
022000*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
022100*   -COPY WDECAREA                                                        
022200     SKIP3                                                                
022300*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
022400*   -COPY WSECAREA                                                        
022500     SKIP3                                                                
022600 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
022700*01  FILLER -COPY WWDIST07 -RED TEST-IDDISTR.                             
022800     EJECT                                                                
022900*01  FILLER -COPY WWDIST18 -RED TEST-IDDISTR.                             
023000     EJECT                                                                
023100*01  FILLER -COPY WWDIST35 -RED TEST-IDDISTR.                             
023200     EJECT                                                                
023300*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
023400     EJECT                                                                
023500 01  FILLER                      PIC X(16)   VALUE 'DIST-DC-TAB'.         
023600*    -COPY WWDIST57                                                       
023700     EJECT                                                                
023800 01  MESSAGE-CODES.                                                       
023900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
024000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
024100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
024200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
024300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
024400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
024500     03  INF-PART-MISSING        PIC X(3)    VALUE '017'.                 
024600     03  INF-ORDER-DELETE        PIC X(3)    VALUE '052'.                 
024700     03  INF-ORDERLINE-DELETE    PIC X(3)    VALUE '752'.                 
024800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
024900     03  ERR-ANNULL-NOT-POSS     PIC X(3)    VALUE '066'.                 
025000     03  ERR-ORDER-EJ-AVSLUTAD   PIC X(3)    VALUE '053'.                 
025100     03  ERR-LINES-MISSING-CL    PIC X(3)    VALUE '028'.                 
025200     03  ERR-LINES-MISSING       PIC X(3)    VALUE '029'.                 
025300     03  ERR-LINES-WRITTEN       PIC X(3)    VALUE '067'.                 
025400     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
025500     03  ERR-ORDER-MISSING       PIC X(3)    VALUE '701'.                 
025600     03  ERR-CMD-FEL             PIC X(3)    VALUE '156'.                 
025700     EJECT                                                                
025800*    --- AREOR FÖR GEMENSAMMA-SUBPROGRAM                                  
025900*                                                                         
026000 01  FILLER                      PIC X(16)   VALUE 'LÄNKAREOR'.           
026100*01  -COPY W413AVSR                                                       
026200     EJECT                                                                
026300*01  -COPY W413AVSO                                                       
026400     EJECT                                                                
026500 01 FILLER                       PIC X(8) VALUE 'W411DNOT'.               
026600*   -COPY W411DNOT                                                        
026700     EJECT                                                                
026800 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
026900*   -COPY W335PRQU                                                        
027000     EJECT                                                                
027100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
027200*                                                                         
027300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
027400     SKIP3                                                                
027500*01  MID -COPY W4I20401                                                   
027600     EJECT                                                                
027700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
027800     SKIP3                                                                
027900*01  -COPY WMSGAREA                                                       
028000     03  MOD REDEFINES MSG-AREA.                                          
028100*      05  -COPY W4O20401                                                 
028200     EJECT                                                                
028300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
028400     SKIP3                                                                
028500*01  -COPY WMFSAREA                                                       
028600     EJECT                                                                
028700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028800*                                                                         
028900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
029000     SKIP3                                                                
029100 01  NYCKLAR-TILL-DLI.                                                    
029200*    ---------TILL WDQ101                                                 
029300     03  W-WDQ101KY-MIN-X.                                                
029400         05  W-OBKR-IDORDER-MIN   PIC S9(7)   VALUE ZERO COMP-3.          
029500         05  W-OBKR-IDARTNR-MIN   PIC S9(9)   VALUE ZERO COMP-3.          
029600         05  W-OBKR-IDLOPNR-MIN   PIC S9(3)   VALUE ZERO COMP-3.          
029700         05  W-OBKR-IDSEKVNR-MIN  PIC S9(3)   VALUE ZERO COMP-3.          
029800         05  W-OBKR-IDDC-MIN      PIC  X(2)   VALUE ZERO.                 
029900         05  W-OBKR-KDORDBEK-MIN  PIC 9(2)    VALUE ZERO.                 
030000                                                                          
030100     03  W-WDQ101KY-MAX-X.                                                
030200         05  W-OBKR-IDORDER-MAX   PIC S9(7)   VALUE ZERO COMP-3.          
030300         05  W-OBKR-IDARTNR-MAX   PIC S9(9)   VALUE ZERO COMP-3.          
030400         05  W-OBKR-IDLOPNR-MAX   PIC S9(3)   VALUE ZERO COMP-3.          
030500         05  W-OBKR-IDSEKVNR-MAX  PIC S9(3)   VALUE ZERO COMP-3.          
030600         05  W-OBKR-IDDC-MAX      PIC  X(2)   VALUE ZERO.                 
030700         05  W-OBKR-KDORDBEK-MAX  PIC 9(2)    VALUE ZERO.                 
030800                                                                          
030900*    ---------TILL WDQ201 VIA WDQ2C1(SEK. INDX)                           
031000     03  W-WDQ2CSEQ-X.                                                    
031100         05  W-Q2CSEQ-IDDISTR     PIC S9(5)    VALUE ZERO COMP-3.         
031200         05  W-Q2CSEQ-IDKUNDNR    PIC S9(7)    VALUE ZERO COMP-3.         
031300         05  W-Q2CSEQ-IDKUNDRF    PIC  X(10)   VALUE SPACE.               
031400*    ---------TILL WDQ212                                                 
031500     03  W-IDDC-X.                                                        
031600         05  W-IDDC               PIC X(2)    VALUE SPACE.                
031700*    ---------TILL WDQ301                                                 
031800     03  W-WDQ301KY-MIN-X.                                                
031900         05  W-ODEL-IDORDER-MIN   PIC S9(7)    VALUE ZERO COMP-3.         
032000         05  W-ODEL-IDDC-MIN      PIC  X(2)    VALUE ZERO.                
032100         05  FILLER               PIC  X(6)    VALUE LOW-VALUE.           
032200     03  W-WDQ301KY-MAX-X.                                                
032300         05  W-ODEL-IDORDER-MAX   PIC S9(7)    VALUE ZERO COMP-3.         
032400         05  W-ODEL-IDDC-MAX      PIC  X(2)    VALUE ZERO.                
032500         05  FILLER               PIC  X(6)    VALUE HIGH-VALUE.          
032600*    ---------TILL WDQ401                                                 
032700     03  W-WDQ401KY-MIN-X.                                                
032800         05  W-ORAD-IDORDER-MIN   PIC S9(7)    VALUE ZERO COMP-3.         
032900         05  W-ORAD-IDDC-MIN      PIC  X(2)    VALUE ZERO.                
033000         05  W-ORAD-ADLAGOMR-MIN  PIC S9(3)    VALUE ZERO COMP-3.         
033100         05  W-ORAD-ADGANG-MIN    PIC S9(3)    VALUE ZERO COMP-3.         
033200         05  W-ORAD-ADPLATS-MIN   PIC S9(5)    VALUE ZERO COMP-3.         
033300         05  FILLER               PIC S9(9)    VALUE ZERO COMP-3.         
033400         05  FILLER               PIC S9(3)    VALUE ZERO COMP-3.         
033500     03  W-ORAD-IDARTNR-MIN-X.                                            
033600         05  W-ORAD-IDARTNR-MIN   PIC S9(9)    VALUE ZERO COMP-3.         
033700     03  W-ORAD-IDLOPNR-MIN-X.                                            
033800         05  W-ORAD-IDLOPNR-MIN   PIC S9(3)    VALUE ZERO COMP-3.         
033900     03  W-WDQ401KY-MAX-X.                                                
034000         05  W-ORAD-IDORDER-MAX   PIC S9(7)    VALUE ZERO COMP-3.         
034100         05  W-ORAD-IDDC-MAX      PIC  X(2)    VALUE ZERO.                
034200         05  W-ORAD-ADLAGOMR-MAX  PIC S9(3)    VALUE ZERO COMP-3.         
034300         05  W-ORAD-ADGANG-MAX    PIC S9(3)    VALUE ZERO COMP-3.         
034400         05  W-ORAD-ADPLATS-MAX   PIC S9(5)    VALUE ZERO COMP-3.         
034500         05  FILLER               PIC S9(9)    VALUE ZERO COMP-3.         
034600         05  FILLER               PIC S9(3)    VALUE ZERO COMP-3.         
034700     03  W-ORAD-IDARTNR-MAX-X.                                            
034800         05  W-ORAD-IDARTNR-MAX   PIC S9(9)    VALUE ZERO COMP-3.         
034900     03  W-ORAD-IDLOPNR-MAX-X.                                            
035000         05  W-ORAD-IDLOPNR-MAX   PIC S9(3)    VALUE ZERO COMP-3.         
035100     03  W-WDQ401KY-MIN-MIN-X.                                            
035200         05  W-ORAD-IDORDER-MIN-MIN  PIC S9(7)  VALUE ZERO COMP-3.        
035300         05  W-ORAD-IDDC-MIN-MIN     PIC  X(2)  VALUE ZERO.               
035400         05  W-ORAD-ADLAGOMR-MIN-MIN PIC S9(3)  VALUE ZERO COMP-3.        
035500         05  W-ORAD-ADGANG-MIN-MIN   PIC S9(3)  VALUE ZERO COMP-3.        
035600         05  W-ORAD-ADPLATS-MIN-MIN  PIC S9(5)  VALUE ZERO COMP-3.        
035700         05  W-ORAD-IDARTNR-MIN-MIN  PIC S9(9)  VALUE ZERO COMP-3.        
035800         05  W-ORAD-IDLOPNR-MIN-MIN  PIC S9(3)  VALUE ZERO COMP-3.        
035900     03  W-WDQ401KY-MAX-MAX-X.                                            
036000         05  W-ORAD-IDORDER-MAX-MAX  PIC S9(7)  VALUE ZERO COMP-3.        
036100         05  W-ORAD-IDDC-MAX-MAX     PIC  X(2)  VALUE ZERO.               
036200         05  W-ORAD-ADLAGOMR-MAX-MAX PIC S9(3)  VALUE ZERO COMP-3.        
036300         05  W-ORAD-ADGANG-MAX-MAX   PIC S9(3)  VALUE ZERO COMP-3.        
036400         05  W-ORAD-ADPLATS-MAX-MAX  PIC S9(5)  VALUE ZERO COMP-3.        
036500         05  W-ORAD-IDARTNR-MAX-MAX  PIC S9(9)  VALUE ZERO COMP-3.        
036600         05  W-ORAD-IDLOPNR-MAX-MAX  PIC S9(3)  VALUE ZERO COMP-3.        
036700     03  W-WDQ401KY-UNIK-X.                                               
036800         05  W-ORAD-IDORDER-UNIK     PIC S9(7)  VALUE ZERO COMP-3.        
036900         05  W-ORAD-IDDC-UNIK        PIC  X(2)  VALUE ZERO.               
037000         05  W-ORAD-ADLAGOMR-UNIK    PIC S9(3)  VALUE ZERO COMP-3.        
037100         05  W-ORAD-ADGANG-UNIK      PIC S9(3)  VALUE ZERO COMP-3.        
037200         05  W-ORAD-ADPLATS-UNIK     PIC S9(5)  VALUE ZERO COMP-3.        
037300         05  W-ORAD-IDARTNR-UNIK     PIC S9(9)  VALUE ZERO COMP-3.        
037400         05  W-ORAD-IDLOPNR-UNIK     PIC S9(3)  VALUE ZERO COMP-3.        
037500     SKIP2                                                                
037600   03    W-WDA601KY-MIN-X.                                                
037700     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
037800     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
037900     05    W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.               
038000     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
038100     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
038200     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
038300     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
038400     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
038500     SKIP2                                                                
038600   03    W-WDA601KY-MAX-X.                                                
038700     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
038800     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
038900     05    W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.               
039000     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
039100     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
039200     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
039300     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
039400     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
039500*    _________TILL WDD311                                                 
039600     03  W-WDD3BSEQ-X.                                                    
039700         05  W-WDD3-IDARTNR          PIC S9(9)  VALUE ZERO COMP-3.        
039800     03  W-IDSKYLT-X.                                                     
039900         05  W-TEXT-IDSKYLT          PIC X(3)   VALUE SPACE.              
040000*    ---------TILL WDK901,WDK601,WDD101                                   
040100     03  W-IDARTNR-X.                                                     
040200         05  W-IDARTNR               PIC S9(9)  VALUE ZERO COMP-3.        
040300*    ---------TILL WDA501                                                 
040400     03  W-WDA501KY-X.                                                    
040500         05  W-RAD-IDDISTR           PIC S9(5)  VALUE ZERO COMP-3.        
040600         05  W-RAD-IDKUNDNR          PIC S9(7)  VALUE ZERO COMP-3.        
040700         05  W-RAD-IDKUNDRF          PIC  X(10) VALUE SPACE.              
040800         05  W-RAD-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.        
040900         05  W-RAD-IDLOPNR           PIC S9(3)  VALUE ZERO COMP-3.        
041000                                                                          
041100*    _________TILL WDM2                                                   
041200     03  W-WDM201-X.                                                      
041300         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
041400         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
041500                                                                          
041600     03  W-WDM211-X.                                                      
041700         05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
041800                                                                          
041900     03  W-WDM221-X.                                                      
042000         05  W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.           
042100         05  W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.           
042200         05  W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.           
042300         05  W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.           
042400     03  W-IDDC-B6-X.                                                     
042500         05 W-IDDC-B6                  PIC X(2).                          
042600                                                                          
042700*    --- STATUS-KOD FRÅN IMS                                              
042800 01  STATUS-WS                   PIC XX.                                  
042900     88  SEGMENT-FINNS                       VALUE '  '.                  
043000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
043100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
043200     88  BASEN-SLUT                          VALUE 'GB'.                  
043300     SKIP2                                                                
043400 01  GODK-STATUSKODER.                                                    
043500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
043600     SKIP3                                                                
043700 01  SSA1                        PIC X(200).                              
043800 01  SSA2                        PIC X(128).                              
043900 01  SSA3                        PIC X(128).                              
044000     EJECT                                                                
044100******************************************************                    
044200*    ARBETSAREA FÖR RYB-TRANS TILL WDG6              *                    
044300******************************************************                    
044400 01  W-RYBPOST.                                                           
044500*    03  -COPY WDGZRYB    -PRE W-                                         
044600     EJECT                                                                
044700******************************************************                    
044800*    ARBETSAREA FÖR RYC-TRANS TILL WDG6              *                    
044900******************************************************                    
045000 01  W-RYCPOST.                                                           
045100*    03  -COPY WDGZRYC -PRE W-                                            
045200     EJECT                                                                
045300 01  W-RYCSPOST.                                                          
045400*    03  -COPY WDGZRYCS -PRE W-                                           
045500     EJECT                                                                
045600*                            DB2 FUNKTIONSKODER                           
045700 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
045800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
045900                                                                          
046000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
046100 01  DB2-WS.                                                              
046200     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
046300         88  CURSOR-OK                       VALUE 000.                   
046400         88  RADER-FINNS                     VALUE 000.                   
046500         88  RADER-SAKNAS                    VALUE 100.                   
046600         88  ATKOMST-FEL                     VALUE 904.                   
046700     03  GODK-SQLCODEKODER.                                               
046800         05  GODK-SQLCODE OCCURS 5                                        
046900             INDEXED BY SQLCODE-IX PIC 9(3).                              
047000 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
047100     EJECT                                                                
047200*    --- IMS FUNKTIONSKODER                                               
047300*01  -COPY W0003                                                          
047400     EJECT                                                                
047500*    ---  DLI INPUT-OUTPUT AREA                                           
047600 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-AREA'.            
047700     SKIP3                                                                
047800 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ORQI01'.         
047900 01  DLI-IO-AREA-OHUV.                                                    
048000     03  WLORQI01.                                                        
048100*        05  -COPY WDQ201                                                 
048200     EJECT                                                                
048300                                                                          
048400 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ORQI11'.         
048500 01  DLI-IO-AREA-DIRL.                                                    
048600     03  WLORQI11.                                                        
048700*        05  -COPY WDQ211                                                 
048800     EJECT                                                                
048900                                                                          
049000 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ORQI12'.         
049100 01  DLI-IO-AREA-ARB.                                                     
049200     03  WLORQI12.                                                        
049300*        05  -COPY WDQ212                                                 
049400                                                                          
049500 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-Q221'.           
049600 01  DLI-IO-Q221.                                                         
049700*    03  -COPY WDQ221                                                     
049800     EJECT                                                                
049900                                                                          
050000 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ORQA01'.         
050100 01  DLI-IO-AREA-ODEL.                                                    
050200     03  WLORQA01.                                                        
050300*        05  -COPY WDQ301                                                 
050400     EJECT                                                                
050500                                                                          
050600 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ORQF01'.         
050700 01  DLI-IO-AREA-ORAD.                                                    
050800     03  WLORQF01.                                                        
050900*        05  -COPY WDQ401                                                 
051000     EJECT                                                                
051100 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
051200 01  DLI-IO-AREA-WDA6.                                                    
051300*  03    -COPY WDA601                                                     
051400     EJECT                                                                
051500                                                                          
051600 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ARTM01'.         
051700 01  DLI-IO-AREA-ARTM.                                                    
051800     03  WLARTM01.                                                        
051900*        05  -COPY WDK901                                                 
052000     EJECT                                                                
052100                                                                          
052200 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ARTC11'.         
052300 01  DLI-IO-AREA-K611.                                                    
052400     03  WLARTC11.                                                        
052500*        05  -COPY WDK611                                                 
052600     EJECT                                                                
052700                                                                          
052800 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ARTS11'.         
052900 01  DLI-IO-AREA-K711.                                                    
053000     03  WLARTS11.                                                        
053100*        05  -COPY WDK711                                                 
053200     EJECT                                                                
053300                                                                          
053400 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ZZAC01'.         
053500 01  DLI-IO-AREA-ZZAC.                                                    
053600     03  WLZZAC01.                                                        
053700*        05  -COPY WDGZ01                                                 
053800     EJECT                                                                
053900                                                                          
054000 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ORQM01'.         
054100 01  DLI-IO-AREA-OBKR.                                                    
054200     03  WLORQM01.                                                        
054300*        05  -COPY WDQ101                                                 
054400     EJECT                                                                
054500                                                                          
054600 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-ORDP01'.         
054700 01  DLI-IO-AREA-ORDP.                                                    
054800     03  WLORDP01.                                                        
054900*        05  -COPY WDA501                                                 
055000     EJECT                                                                
055100                                                                          
055200 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-BENA11'.         
055300 01  DLI-IO-AREA-BENA.                                                    
055400     03  WLBENA11.                                                        
055500*        05  -COPY WDD311                                                 
055600     EJECT                                                                
055700                                                                          
055800 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
055900 01  DLI-IO-WDM211.                                                       
056000*    03 -COPY WDM211                                                      
056100     EJECT                                                                
056200 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
056300 01  DLI-IO-WDM221.                                                       
056400*    03 -COPY WDM221                                                      
056500     EJECT                                                                
056600                                                                          
056700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
056800 01   DLI-IO-AREA-B601.                                                   
056900*     03  -COPY WDB601                                                    
057000                                                                          
057100                                                                          
057200*    MSG-AREA FÖR HOPP TILL W40292                                        
057300 01  FILLER            PIC X(16)   VALUE 'ALT1-MSG-IO-AREA'.              
057400 01  W-PROG-TO-PROG-SW.                                                   
057500     03  ALT1-LL                   PIC S9(4) VALUE +76 COMP SYNC.         
057600     03  ALT1-Z1                   PIC X.                                 
057700     03  ALT1-Z2                   PIC X.                                 
057800     03  ALT1-TRANSKOD             PIC X(8)  VALUE 'W4T292X '.            
057900     03  ALT1-IDTRANS              PIC X(4)  VALUE '4204'.                
058000     03  ALT1-KDMFSFOR             PIC X.                                 
058100*    03  MID -COPY W4I29201    -PRE ALT1-                                 
058200     EJECT                                                                
058300*    MSG-AREA FÖR HOPP TILL W20109                                        
058400 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
058500 01  W-PROG-TO-PROG-SW-2.                                                 
058600     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
058700     03  2109-Z1                   PIC X.                                 
058800     03  2109-Z2                   PIC X.                                 
058900     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
059000     03  2109-IDTRANS              PIC X(4)  VALUE '4204'.                
059100     03  2109-KDMFSFOR             PIC X.                                 
059200*    03  -COPY W2I10902    -PRE 2109-                                     
059300     EJECT                                                                
059400 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
059500                                                                          
059600*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
059700     EJECT                                                                
059800     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
059900     EJECT                                                                
060000 LINKAGE SECTION.                                                         
060100                                                                          
060200*01  -COPY W0009      -PRE MSG-                                           
060300     EJECT                                                                
060400*01  -COPY W0009      -PRE ALT1-                                          
060500     EJECT                                                                
060600*                        W2T109                                           
060700     -COPY W0009 -PRE 2109-                                               
060800     EJECT                                                                
060900 01  AVSR-ALT2-PCB               PIC X.                                   
061000     EJECT                                                                
061100*01  -COPY W0008      -PRE USEA-                                          
061200     05  FILLER                  PIC X.                                   
061300     EJECT                                                                
061400*01  -COPY W0008      -PRE ORQI-                                          
061500     05  FILLER                  PIC X.                                   
061600     EJECT                                                                
061700*01  -COPY W0008      -PRE ORQA-                                          
061800     05  FILLER                  PIC X.                                   
061900     EJECT                                                                
062000*01  -COPY W0008      -PRE ORQF-                                          
062100     05  FILLER                  PIC X.                                   
062200     EJECT                                                                
062300*01  -COPY W0008      -PRE BENA-                                          
062400     05  FILLER                  PIC X.                                   
062500     EJECT                                                                
062600*01  -COPY W0008      -PRE ARTC-                                          
062700     05  FILLER                  PIC X.                                   
062800     EJECT                                                                
062900*01  -COPY W0008      -PRE ARTS-                                          
063000     05  FILLER                  PIC X.                                   
063100     EJECT                                                                
063200*01  -COPY W0008      -PRE ARTM-                                          
063300     05  FILLER                  PIC X.                                   
063400     EJECT                                                                
063500*01  -COPY W0008      -PRE ZZAC-                                          
063600     05  FILLER                  PIC X.                                   
063700     EJECT                                                                
063800*01  -COPY W0008      -PRE ORQM-                                          
063900     05  FILLER                  PIC X.                                   
064000     EJECT                                                                
064100*01  -COPY W0008      -PRE ORDP-                                          
064200     05  FILLER                  PIC X.                                   
064300     EJECT                                                                
064400*01  -COPY W0008      -PRE WDM2-                                          
064500     05  FILLER                  PIC X.                                   
064600     EJECT                                                                
064700*01  -COPY W0008      -PRE WDA6B-                                         
064800     05  FILLER                  PIC X.                                   
064900     EJECT                                                                
065000*01  -COPY W0008      -PRE WDB6-                                          
065100     05  FILLER                  PIC X.                                   
065200     EJECT                                                                
065300*----> SUBPROGRAM W413AVSR.                                               
065400 01  AVSR-ORQI-PCB               PIC X.                                   
065500 01  AVSR-GMTB-PCB               PIC X.                                   
065600 01  AVSR-GMTC-PCB               PIC X.                                   
065700 01  AVSR-WDB2-PCB               PIC X.                                   
065800 01  AVSR-WDB6-PCB               PIC X.                                   
065900 01  TRAN-XXKB-PCB               PIC X.                                   
066000     EJECT                                                                
066100*----> SUBPROGRAM W413AVSO.                                               
066200 01  AVSO-WDE6-PCB               PIC X.                                   
066300 01  AVSO-ORQA-PCB               PIC X.                                   
066400 01  AVSO-WDQ2-PCB               PIC X.                                   
066500 01  AVSO-GMTB-PCB               PIC X.                                   
066600 01  AVSO-XXKA-PCB               PIC X.                                   
066700 01  AVSO-4437-PCB               PIC X.                                   
066800 01  AVSO-XXKE-PCB               PIC X.                                   
066900 01  AVSO-XXKF-PCB               PIC X.                                   
067000 01  AVSO-XXKG-PCB               PIC X.                                   
067100 01  AVSO-XXKH-PCB               PIC X.                                   
067200 01  AVSO-XXKI-PCB               PIC X.                                   
067300 01  AVSO-XXKP-PCB               PIC X.                                   
067400 01  AVSO-WDB2-PCB               PIC X.                                   
067500 01  AVSO-WDB6-PCB               PIC X.                                   
067600 01  ORDN-ORQL-PCB               PIC X.                                   
067700 01  ORDN-PROC-PCB               PIC X.                                   
067800 01  ORDN-ORQI-PCB               PIC X.                                   
067900 01  ORDN-WDQ3-PCB               PIC X.                                   
068000*----> SUBPROGRAM W411DNOT.                                               
068100 01  DNOT-ORQP-PCB               PIC X.                                   
068200 01  DNOT-ORQP2-PCB              PIC X.                                   
068300 01  DNOT-ORQP3-PCB              PIC X.                                   
068400 01  DNOT-4013-PCB               PIC X.                                   
068500 01  DNOT-BENA-PCB               PIC X.                                   
068600*----> SUBPROGRAM W335PRQU.                                               
068700 01  PRQU-WDG2-PCB               PIC X.                                   
068800 01  PRQU-WDC7-PCB               PIC X.                                   
068900 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
069000     EJECT                                                                
069100                                                                          
069200 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB 2109-PCB AVSR-ALT2-PCB        
069300                           USEA-PCB                                       
069400                           ORQI-PCB                                       
069500                           ORQA-PCB ORQF-PCB BENA-PCB ARTC-PCB            
069600                           ARTS-PCB ARTM-PCB ZZAC-PCB ORQM-PCB            
069700                           ORDP-PCB WDM2-PCB                              
069800                           WDA6B-PCB WDB6-PCB                             
069900                           AVSR-ORQI-PCB AVSR-GMTB-PCB                    
070000                           AVSR-GMTC-PCB                                  
070100                           AVSR-WDB2-PCB AVSR-WDB6-PCB                    
070200                           TRAN-XXKB-PCB                                  
070300                           AVSO-WDE6-PCB AVSO-ORQA-PCB                    
070400                           AVSO-WDQ2-PCB                                  
070500                           AVSO-GMTB-PCB AVSO-XXKA-PCB                    
070600                           AVSO-4437-PCB AVSO-XXKE-PCB                    
070700                           AVSO-XXKF-PCB AVSO-XXKG-PCB                    
070800                           AVSO-XXKH-PCB AVSO-XXKI-PCB                    
070900                           AVSO-XXKP-PCB AVSO-WDB2-PCB                    
071000                           AVSO-WDB6-PCB                                  
071100                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
071200                           ORDN-ORQI-PCB ORDN-WDQ3-PCB                    
071300                           DNOT-ORQP-PCB                                  
071400                           DNOT-ORQP2-PCB                                 
071500                           DNOT-ORQP3-PCB                                 
071600                           DNOT-4013-PCB                                  
071700                           DNOT-BENA-PCB                                  
071800                           PRQU-WDG2-PCB                                  
071900                           PRQU-WDC7-PCB                                  
072000                           PRQU-SJKO-WDK6-PCB.                            
072100 MAIN SECTION.                                                            
072200     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB 2109-PCB AVSR-ALT2-PCB        
072300                           USEA-PCB                                       
072400                           ORQI-PCB                                       
072500                           ORQA-PCB ORQF-PCB BENA-PCB ARTC-PCB            
072600                           ARTS-PCB ARTM-PCB ZZAC-PCB ORQM-PCB            
072700                           ORDP-PCB WDM2-PCB                              
072800                           WDA6B-PCB WDB6-PCB                             
072900                           AVSR-ORQI-PCB AVSR-GMTB-PCB                    
073000                           AVSR-GMTC-PCB                                  
073100                           AVSR-WDB2-PCB AVSR-WDB6-PCB                    
073200                           TRAN-XXKB-PCB                                  
073300                           AVSO-WDE6-PCB AVSO-ORQA-PCB                    
073400                           AVSO-WDQ2-PCB                                  
073500                           AVSO-GMTB-PCB AVSO-XXKA-PCB                    
073600                           AVSO-4437-PCB AVSO-XXKE-PCB                    
073700                           AVSO-XXKF-PCB AVSO-XXKG-PCB                    
073800                           AVSO-XXKH-PCB AVSO-XXKI-PCB                    
073900                           AVSO-XXKP-PCB AVSO-WDB2-PCB                    
074000                           AVSO-WDB6-PCB                                  
074100                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
074200                           ORDN-ORQI-PCB ORDN-WDQ3-PCB                    
074300                           DNOT-ORQP-PCB                                  
074400                           DNOT-ORQP2-PCB                                 
074500                           DNOT-ORQP3-PCB                                 
074600                           DNOT-4013-PCB                                  
074700                           DNOT-BENA-PCB                                  
074800                           PRQU-WDG2-PCB                                  
074900                           PRQU-WDC7-PCB                                  
075000                           PRQU-SJKO-WDK6-PCB.                            
075100     EJECT                                                                
075200                                                                          
075300     PERFORM IMS-GET-MSG                                                  
075400     IF SEGMENT-FINNS                                                     
075500       PERFORM A-INIT                                                     
075600       PERFORM C-KOLLA-NYCKLAR                                            
075700       IF NYCKLAR-OK                                                      
075800         PERFORM B-KOLLA-BEHOERIGHET                                      
075900         IF SEC-KDSVAR = ' '                                              
076000           PERFORM D-LAES-IN-ORDER                                        
076100           IF ALLT-OK                                                     
076200             MOVE NEJ TO ANNULL-SW                                        
076300             IF MFS-UPDATE                                                
076400               PERFORM I-KOLLA-INPUT                                      
076500               IF INDATA-OK                                               
076600                 PERFORM J-UPPDATERA                                      
076700               END-IF                                                     
076800             ELSE                                                         
076900               IF MFS-FIRST                                               
077000                 PERFORM E-FOERSTA-SIDA                                   
077100               ELSE                                                       
077200                 IF MFS-NEXT                                              
077300                   PERFORM F-NAESTA-SIDA                                  
077400                 ELSE                                                     
077500                   PERFORM G-SAMMA-SIDA                                   
077600                 END-IF                                                   
077700               END-IF                                                     
077800             END-IF                                                       
077900             IF INDATA-OK AND NYCKLAR-OK AND                              
078000               NOT ANNULL-HELORDER                                        
078100               PERFORM H-LAES-VISA-INFO                                   
078200             END-IF                                                       
078300             IF INDATA-OK AND NYCKLAR-OK                                  
078400               IF 2109-MID2-KVANTART > ZERO                               
078500                 PERFORM S16-STARTA-2109                                  
078600               END-IF                                                     
078700             END-IF                                                       
078800           END-IF                                                         
078900         ELSE                                                             
079000           MOVE ERR-OBEHORIG TO MED-IDMFSFEL                              
079100           CALL WMEDKONV USING MED-WMEDAREA                               
079200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
079300         END-IF                                                           
079400       END-IF                                                             
079500       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
079600       PERFORM IMS-INSERT-MSG                                             
079700     END-IF                                                               
079800                                                                          
079900     MOVE ZERO TO RETURN-CODE                                             
080000     GOBACK                                                               
080100     .                                                                    
080200     EJECT                                                                
080300                                                                          
080400 A-INIT SECTION.                                                          
080500                                                                          
080600     IF MSG-DUBBLA-TRANSKODER                                             
080700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I20401                 
080800       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
080900       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
081000                                             ALT1-KDMFSFOR                
081100                                             2109-KDMFSFOR                
081200     ELSE                                                                 
081300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I20401                 
081400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
081500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
081600                                             ALT1-KDMFSFOR                
081700                                             2109-KDMFSFOR                
081800     END-IF                                                               
081900                                                                          
082000     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
082100     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
082200     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
082300                                                                          
082400     MOVE LOW-VALUE       TO MSG-AREA                                     
082500     MOVE 'W4O204N1'      TO MFS-IDMOD                                    
082600     MOVE '4204'          TO MOD-IDTRANS                                  
082700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
082800                                                                          
082900     IF MFS-UPDATE                                                        
083000       INITIALIZE AVSR-W413AVSR                                           
083100     END-IF                                                               
083200                                                                          
083300     IF NOT EGEN-MID                                                      
083400       MOVE MFS-RENSA-FAELT TO MID-IDDISTR-IN                             
083500                               MID-IDKUNDNR-IN                            
083600                               MID-IDKUNDRF-IN                            
083700                               MID-IDARTNR-IN                             
083800                               MID-IDDC-IN                                
083900                               MID-IDDISTR-UT                             
084000                               MID-IDKUNDNR-UT                            
084100                               MID-IDKUNDRF-UT                            
084200                               MID-IDARTNR-UT                             
084300                               MID-IDDC-UT                                
084400       MOVE SPACE TO MFS-KDTRTYP                                          
084500       MOVE '7' TO MFS-IDPFK                                              
084600     END-IF                                                               
084700                                                                          
084800     ACCEPT WS-TINUDAT FROM DATE                                          
084900     ACCEPT WS-TINUTID FROM TIME                                          
085000                                                                          
085100     IF ENGLISH-TEXT                                                      
085200       MOVE 'GB ' TO MED-IDSKYLT                                          
085300     ELSE                                                                 
085400       MOVE 'S  ' TO MED-IDSKYLT                                          
085500     END-IF                                                               
085600                                                                          
085700     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O20401 + 4                  
085800                                                                          
085900     MOVE SPACE         TO 2109-MID2-W2I10902                             
086000     MOVE +1            TO 2109-INDX                                      
086100     .                                                                    
086200     EJECT                                                                
086300                                                                          
086400 B-KOLLA-BEHOERIGHET SECTION.                                             
086500                                                                          
086600     IF WS-IDDC NOT = W-IDDC-B6                                           
086700        MOVE WS-IDDC TO W-IDDC-B6                                         
086800        PERFORM IMS-GU-WDB601                                             
086900     END-IF                                                               
087000                                                                          
087100     MOVE MSG-SIGNON-USERID TO SEC-IDUSER                                 
087200     MOVE '4204'            TO SEC-IDTRANS                                
087300     IF DCS-NDC                                                           
087400       MOVE MSGI-IDDISTR    TO SEC-IDKEY                                  
087500     ELSE                                                                 
087600       MOVE ZERO            TO SEC-IDKEY                                  
087700     END-IF                                                               
087800                                                                          
087900     CALL WSECURIT USING       SEC-IDUSER                                 
088000                               SEC-IDTRANS                                
088100                               SEC-IDKEY                                  
088200                               SEC-KDSVAR                                 
088300     .                                                                    
088400     EJECT                                                                
088500                                                                          
088600 C-KOLLA-NYCKLAR SECTION.                                                 
088700                                                                          
088800     MOVE JA TO NYCKLAR-SW                                                
088900     MOVE ALL '+'            TO MSGI-WMSGINIT                             
089000     MOVE '001'              TO MSGI-KDCALL                               
089100     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
089200     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
089300     MOVE '4204'             TO MSGI-IDTRANS                              
089400     IF MFS-IDTRANS = '4204'                                              
089500        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
089600        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
089700                                                                          
089800        MOVE MID-IDKUNDRF-IN TO WS-IDKUNDRF-1-7                           
089900        IF WS-IDKUNDRF-1-7 = ALL '+'                                      
090000          MOVE '+++'         TO WS-IDKUNDRF-8-10                          
090100        ELSE                                                              
090200          MOVE SPACE         TO WS-IDKUNDRF-8-10                          
090300        END-IF                                                            
090400        MOVE WS-IDKUNDRF-RED TO MSGI-IDKUNDRF                             
090500        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
090600     END-IF                                                               
090700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
090800     MOVE NEJ TO IDARTNR-SW                                               
090900     MOVE LOW-VALUE TO       W-WDQ2CSEQ-X                                 
091000                             W-ORAD-IDLOPNR-MIN-X                         
091100                             W-ORAD-IDARTNR-MIN-X                         
091200                             W-WDQ101KY-MIN-X                             
091300                             W-WDQ301KY-MIN-X                             
091400                             W-WDQ401KY-MIN-X                             
091500                             W-WDQ401KY-UNIK-X                            
091600                             W-WDQ401KY-MIN-MIN-X                         
091700                             W-WDD3BSEQ-X                                 
091800                             W-WDA501KY-X                                 
091900                             W-IDSKYLT-X                                  
092000                             W-IDARTNR-X                                  
092100                                                                          
092200     MOVE HIGH-VALUE TO      W-WDQ101KY-MAX-X                             
092300                             W-WDQ301KY-MAX-X                             
092400                             W-WDQ401KY-MAX-X                             
092500                             W-WDQ401KY-MAX-MAX-X                         
092600                             W-ORAD-IDLOPNR-MAX-X                         
092700                             W-ORAD-IDARTNR-MAX-X                         
092800                                                                          
092900     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
093000                             MOD-IDKUNDNR-IN                              
093100                             MOD-IDKUNDRF-IN                              
093200                             MOD-IDARTNR-IN                               
093300                             MOD-IDDC-IN                                  
093400                                                                          
093500     PERFORM CA-KOLLA-DISTRIKT                                            
093600     PERFORM CB-KOLLA-KUNDNUMMER                                          
093700     PERFORM CC-KOLLA-ORDERNUMMER                                         
093800     PERFORM CD-KOLLA-CLAGER                                              
093900                                                                          
094000     IF IFYLLT-OK                                                         
094100       PERFORM CE-KOLLA-ARTIKELNUMMER                                     
094200     END-IF                                                               
094300                                                                          
094400     MOVE WS-IDDISTR     TO MOD-IDDISTR-UT                                
094500     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
094600     MOVE WS-IDKUNDNR    TO MOD-IDKUNDNR-UT                               
094700     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
094800     IF MOD-IDKUNDNR-UT = ALL SPACE                                       
094900       MOVE '     0'     TO MOD-IDKUNDNR-UT                               
095000     END-IF                                                               
095100     MOVE WS-IDKUNDRF    TO MOD-IDKUNDRF-UT                               
095200     INSPECT MOD-IDKUNDRF-UT REPLACING LEADING ZERO BY SPACE              
095300     MOVE WS-IDDC        TO MOD-IDDC-UT                                   
095400     IF IDARTNR-IFYLLT                                                    
095500       IF WS-IDARTNR NOT = ZERO                                           
095600         MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                
095700         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
095800       END-IF                                                             
095900     END-IF                                                               
096000     IF NYCKLAR-FEL                                                       
096100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
096200       CALL WMEDKONV USING MED-WMEDAREA                                   
096300       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
096400       PERFORM MFS-RENSA-FAELT-IN                                         
096500       PERFORM MFS-RENSA-FAELT-UT                                         
096600       MOVE MFS-RENSA-FAELT TO MOD-KVRADER                                
096700                               MOD-KVORDRAD                               
096800                               MOD-TEDDI                                  
096900                               MOD-KDVALISO                               
097000     END-IF                                                               
097100     .                                                                    
097200     EJECT                                                                
097300                                                                          
097400 CA-KOLLA-DISTRIKT SECTION.                                               
097500                                                                          
097600     MOVE MSGI-IDDISTR   TO WS-IDDISTR                                    
097700     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
097800                                                                          
097900     IF WS-IDDISTR = ZERO                                                 
098000         MOVE NEJ TO IFYLLT-SW                                            
098100     END-IF                                                               
098200                                                                          
098300     IF MID-IDDISTR-IN = ALL '+'                                          
098400       CONTINUE                                                           
098500     ELSE                                                                 
098600       MOVE '7'         TO MFS-IDPFK                                      
098700       MOVE SPACE       TO MFS-KDTRTYP                                    
098800     END-IF                                                               
098900                                                                          
099000     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
099100       MOVE WS-IDDISTR TO WS-IDDISTR-NUM                                  
099200       MOVE WS-IDDISTR-NUM TO W-Q2CSEQ-IDDISTR                            
099300       MOVE WS-IDDISTR    TO DIST79-IDDISTR                               
099400       IF DIST79-DEALER-PRICE                                             
099500         IF ENGLISH-TEXT                                                  
099600           MOVE 'DEALERPRICE'   TO MOD-TEDDI                              
099700         ELSE                                                             
099800           MOVE '    ÅF PRIS'   TO MOD-TEDDI                              
099900         END-IF                                                           
100000       ELSE                                                               
100100          MOVE SPACES            TO MOD-TEDDI                             
100200       END-IF                                                             
100300     ELSE                                                                 
100400       MOVE NEJ TO NYCKLAR-SW                                             
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800                                                                          
100900 CB-KOLLA-KUNDNUMMER SECTION.                                             
101000                                                                          
101100     MOVE MSGI-IDKUNDNR    TO WS-IDKUNDNR                                 
101200     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
101300                                                                          
101400                                                                          
101500     IF MID-IDKUNDNR-IN = ALL '+'                                         
101600       CONTINUE                                                           
101700     ELSE                                                                 
101800       MOVE '7'         TO MFS-IDPFK                                      
101900       MOVE SPACE       TO MFS-KDTRTYP                                    
102000     END-IF                                                               
102100                                                                          
102200     IF WS-IDKUNDNR NUMERIC                                               
102300       MOVE WS-IDKUNDNR TO WS-IDKUNDNR-NUM                                
102400       MOVE WS-IDKUNDNR-NUM TO W-Q2CSEQ-IDKUNDNR                          
102500     ELSE                                                                 
102600       MOVE NEJ TO NYCKLAR-SW                                             
102700     END-IF                                                               
102800     .                                                                    
102900     EJECT                                                                
103000                                                                          
103100 CC-KOLLA-ORDERNUMMER SECTION.                                            
103200                                                                          
103300     MOVE MSGI-IDKUNDRF    TO WS-IDKUNDRF                                 
103400     INSPECT WS-IDKUNDRF REPLACING LEADING SPACE BY ZERO                  
103500                                                                          
103600     IF WS-IDKUNDRF = ZERO                                                
103700         MOVE NEJ TO IFYLLT-SW                                            
103800     END-IF                                                               
103900                                                                          
104000     IF MID-IDKUNDRF-IN = ALL '+'                                         
104100       CONTINUE                                                           
104200     ELSE                                                                 
104300       MOVE '7'         TO MFS-IDPFK                                      
104400       MOVE SPACE       TO MFS-KDTRTYP                                    
104500     END-IF                                                               
104600                                                                          
104700     IF WS-IDKUNDRF NUMERIC AND WS-IDKUNDRF > ZERO                        
104800       MOVE WS-IDKUNDRF TO WS-IDKUNDRF-NUM                                
104900       MOVE WS-IDKUNDRF-NUM TO W-Q2CSEQ-IDKUNDRF                          
105000     ELSE                                                                 
105100       MOVE NEJ TO NYCKLAR-SW                                             
105200     END-IF                                                               
105300     .                                                                    
105400     EJECT                                                                
105500                                                                          
105600 CD-KOLLA-CLAGER SECTION.                                                 
105700                                                                          
105800     IF EGEN-MID                                                          
105900                                                                          
106000       IF MID-IDDC-IN = ALL '+'                                           
106100         IF MID-IDDC-UT = SPACE                                           
106200           MOVE NEJ TO NYCKLAR-SW                                         
106300         ELSE                                                             
106400           MOVE MID-IDDC-UT TO WS-IDDC                                    
106500         END-IF                                                           
106600       ELSE                                                               
106700         MOVE MID-IDDC-IN TO WS-IDDC                                      
106800         MOVE '7'         TO MFS-IDPFK                                    
106900         MOVE SPACE       TO MFS-KDTRTYP                                  
107000       END-IF                                                             
107100                                                                          
107200     ELSE                                                                 
107300       MOVE MSGI-IDDC     TO WS-IDDC                                      
107400     END-IF                                                               
107500                                                                          
107600     IF WS-IDDC NOT = W-IDDC-B6                                           
107700        MOVE WS-IDDC TO W-IDDC-B6                                         
107800        PERFORM IMS-GU-WDB601                                             
107900     END-IF                                                               
108000                                                                          
108100     IF DCS-KDDC = SPACE OR DCS-DDC                                       
108200       MOVE MSGI-IDDC     TO WS-IDDC                                      
108300     END-IF                                                               
108400                                                                          
108500     MOVE WS-IDDC         TO W-ORAD-IDDC-MIN                              
108600                             W-ORAD-IDDC-MAX                              
108700                             W-ORAD-IDDC-MIN-MIN                          
108800                             W-ORAD-IDDC-MAX-MAX                          
108900                             W-ORAD-IDDC-UNIK                             
109000                             W-IDDC                                       
109100     .                                                                    
109200     EJECT                                                                
109300                                                                          
109400 CE-KOLLA-ARTIKELNUMMER SECTION.                                          
109500                                                                          
109600     IF EGEN-MID                                                          
109700                                                                          
109800       IF MID-IDARTNR-IN = ALL '+'                                        
109900         MOVE MID-IDARTNR-UT TO WS-IDARTNR                                
110000         INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO               
110100       ELSE                                                               
110200         MOVE MID-IDARTNR-IN TO WS-IDARTNR                                
110300         MOVE '7'         TO MFS-IDPFK                                    
110400         MOVE SPACE       TO MFS-KDTRTYP                                  
110500       END-IF                                                             
110600                                                                          
110700     ELSE                                                                 
110800       MOVE ZERO          TO WS-IDARTNR                                   
110900     END-IF                                                               
111000                                                                          
111100     IF WS-IDARTNR NUMERIC                                                
111200       IF WS-IDARTNR = ZERO                                               
111300         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                           
111400         MOVE NEJ TO IDARTNR-SW                                           
111500       ELSE                                                               
111600         MOVE WS-IDARTNR     TO WS-IDARTNR-NUM                            
111700         MOVE WS-IDARTNR-NUM TO W-ORAD-IDARTNR-MIN                        
111800                                W-ORAD-IDARTNR-MAX                        
111900                                W-IDARTNR                                 
112000         MOVE JA TO IDARTNR-SW                                            
112100       END-IF                                                             
112200     ELSE                                                                 
112300       MOVE NEJ TO NYCKLAR-SW                                             
112400       MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                            
112500     END-IF                                                               
112600     .                                                                    
112700     EJECT                                                                
112800                                                                          
112900 D-LAES-IN-ORDER SECTION.                                                 
113000                                                                          
113100     MOVE JA TO ALLT-SW                                                   
113200     PERFORM IMS-GU-ORQI-ORQI01                                           
113300     IF SEGMENT-FINNS                                                     
113400       IF OHUV-FLKLAR = JA                                                
113500         IF OHUV-KDTPOTYP = +0                                            
113600           MOVE OHUV-IDORDER TO  W-ORAD-IDORDER-MIN                       
113700                                 W-ORAD-IDORDER-MAX                       
113800                                 W-ORAD-IDORDER-MIN-MIN                   
113900                                 W-ORAD-IDORDER-MAX-MAX                   
114000                                 W-ORAD-IDORDER-UNIK                      
114100                                 W-ODEL-IDORDER-MIN                       
114200                                 W-ODEL-IDORDER-MAX                       
114300                                 WS-IDORDER                               
114400                                                                          
114500           PERFORM S01-RAEKNA-TOTALA-RADER                                
114600                                                                          
114700           IF ALLT-OK                                                     
114800             PERFORM S02-KOLLA-ARBETSTABELL                               
114900           END-IF                                                         
115000         ELSE                                                             
115100           MOVE ERR-ANNULL-NOT-POSS TO MED-IDMFSFEL                       
115200           CALL WMEDKONV USING MED-WMEDAREA                               
115300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
115400           PERFORM MFS-RENSA-FAELT-UT                                     
115500           MOVE MFS-RENSA-FAELT TO MOD-KVRADER                            
115600                                   MOD-KVORDRAD                           
115700                                   MOD-TEDDI                              
115800                                   MOD-KDVALISO                           
115900           MOVE NEJ TO ALLT-SW                                            
116000         END-IF                                                           
116100       ELSE                                                               
116200         MOVE ERR-ORDER-EJ-AVSLUTAD TO MED-IDMFSFEL                       
116300         CALL WMEDKONV USING MED-WMEDAREA                                 
116400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
116500         MOVE NEJ TO ALLT-SW                                              
116600       END-IF                                                             
116700                                                                          
116800     ELSE                                                                 
116900       MOVE ERR-ORDER-MISSING TO MED-IDMFSFEL                             
117000       CALL WMEDKONV USING MED-WMEDAREA                                   
117100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
117200       MOVE NEJ TO ALLT-SW                                                
117300     END-IF                                                               
117400     .                                                                    
117500     EJECT                                                                
117600                                                                          
117700 E-FOERSTA-SIDA SECTION.                                                  
117800                                                                          
117900     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
118000     CALL WMEDKONV USING MED-WMEDAREA                                     
118100     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
118200                                                                          
118300*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
118400     MOVE ZERO       TO MOD-IDORDER-ENTER                                 
118500                        MOD-IDORDER-NEXT                                  
118600                        MOD-IDARTNR-ENTER                                 
118700                        MOD-IDARTNR-NEXT                                  
118800                        MOD-IDDC-ENTER                                    
118900                        MOD-IDDC-NEXT                                     
119000                        MOD-ADLAGOMR-ENTER                                
119100                        MOD-ADLAGOMR-NEXT                                 
119200                        MOD-ADGANG-ENTER                                  
119300                        MOD-ADGANG-NEXT                                   
119400                        MOD-ADPLATS-ENTER                                 
119500                        MOD-ADPLATS-NEXT                                  
119600                        MOD-IDLOPNR-ENTER                                 
119700                        MOD-IDLOPNR-NEXT                                  
119800     MOVE +1 TO INDX                                                      
119900     PERFORM UNTIL INDX > MAX-INDX                                        
120000       MOVE ZERO TO MOD-IDARTNR-SPAR(INDX)                                
120100                    MOD-ADLAGOMR-SPAR(INDX)                               
120200                    MOD-ADGANG-SPAR(INDX)                                 
120300                    MOD-ADPLATS-SPAR(INDX)                                
120400                    MOD-IDLOPNR-SPAR(INDX)                                
120500       ADD +1 TO INDX                                                     
120600     END-PERFORM                                                          
120700     .                                                                    
120800     EJECT                                                                
120900                                                                          
121000 F-NAESTA-SIDA SECTION.                                                   
121100                                                                          
121200     MOVE MID-IDARTNR-NEXT  TO W-ORAD-IDARTNR-MIN                         
121300     MOVE MID-IDARTNR-NEXT  TO W-ORAD-IDARTNR-MAX                         
121400     MOVE MID-IDARTNR-NEXT  TO W-ORAD-IDARTNR-MIN-MIN                     
121500     MOVE MID-IDORDER-NEXT  TO W-ORAD-IDORDER-MIN                         
121600     MOVE MID-IDORDER-NEXT  TO W-ORAD-IDORDER-MIN-MIN                     
121700     MOVE MID-IDORDER-NEXT  TO W-ORAD-IDORDER-MAX                         
121800     MOVE MID-IDORDER-NEXT  TO W-ORAD-IDORDER-MAX-MAX                     
121900     MOVE MID-IDDC-NEXT     TO W-ORAD-IDDC-MIN                            
122000     MOVE MID-IDDC-NEXT     TO W-ORAD-IDDC-MAX                            
122100     MOVE MID-IDDC-NEXT     TO W-ORAD-IDDC-MIN-MIN                        
122200     MOVE MID-IDDC-NEXT     TO W-ORAD-IDDC-MAX-MAX                        
122300     MOVE MID-ADLAGOMR-NEXT TO W-ORAD-ADLAGOMR-MIN                        
122400     MOVE MID-ADLAGOMR-NEXT TO W-ORAD-ADLAGOMR-MIN-MIN                    
122500     MOVE MID-ADGANG-NEXT   TO W-ORAD-ADGANG-MIN                          
122600     MOVE MID-ADGANG-NEXT   TO W-ORAD-ADGANG-MIN-MIN                      
122700     MOVE MID-ADPLATS-NEXT  TO W-ORAD-ADPLATS-MIN                         
122800     MOVE MID-ADPLATS-NEXT  TO W-ORAD-ADPLATS-MIN-MIN                     
122900     MOVE MID-IDLOPNR-NEXT  TO W-ORAD-IDLOPNR-MIN                         
123000     MOVE MID-IDLOPNR-NEXT  TO W-ORAD-IDLOPNR-MIN-MIN                     
123100     .                                                                    
123200     EJECT                                                                
123300                                                                          
123400 G-SAMMA-SIDA SECTION.                                                    
123500                                                                          
123600     MOVE MID-IDARTNR-ENTER  TO W-ORAD-IDARTNR-MIN                        
123700     MOVE MID-IDARTNR-ENTER  TO W-ORAD-IDARTNR-MAX                        
123800     MOVE MID-IDARTNR-ENTER  TO W-ORAD-IDARTNR-MIN-MIN                    
123900     MOVE MID-IDORDER-ENTER  TO W-ORAD-IDORDER-MIN                        
124000     MOVE MID-IDORDER-ENTER  TO W-ORAD-IDORDER-MIN-MIN                    
124100     MOVE MID-IDORDER-ENTER  TO W-ORAD-IDORDER-MAX                        
124200     MOVE MID-IDORDER-ENTER  TO W-ORAD-IDORDER-MAX-MAX                    
124300     MOVE MID-IDDC-ENTER     TO W-ORAD-IDDC-MIN                           
124400     MOVE MID-IDDC-ENTER     TO W-ORAD-IDDC-MAX                           
124500     MOVE MID-IDDC-ENTER     TO W-ORAD-IDDC-MIN-MIN                       
124600     MOVE MID-IDDC-ENTER     TO W-ORAD-IDDC-MAX-MAX                       
124700     MOVE MID-ADLAGOMR-ENTER TO W-ORAD-ADLAGOMR-MIN                       
124800     MOVE MID-ADLAGOMR-ENTER TO W-ORAD-ADLAGOMR-MIN-MIN                   
124900     MOVE MID-ADGANG-ENTER   TO W-ORAD-ADGANG-MIN                         
125000     MOVE MID-ADGANG-ENTER   TO W-ORAD-ADGANG-MIN-MIN                     
125100     MOVE MID-ADPLATS-ENTER  TO W-ORAD-ADPLATS-MIN                        
125200     MOVE MID-ADPLATS-ENTER  TO W-ORAD-ADPLATS-MIN-MIN                    
125300     MOVE MID-IDLOPNR-ENTER  TO W-ORAD-IDLOPNR-MIN                        
125400     MOVE MID-IDLOPNR-ENTER  TO W-ORAD-IDLOPNR-MIN-MIN                    
125500                                                                          
125600     IF MID-INPUT NOT = ALL '+'                                           
125700       MOVE NEJ TO INDATA-SW                                              
125800       MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                                
125900       CALL WMEDKONV USING MED-WMEDAREA                                   
126000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
126100       PERFORM MFS-ROR-EJ-FAELT-IN                                        
126200       PERFORM MFS-ROR-EJ-FAELT-UT                                        
126300       PERFORM MFS-LAS-IN-IGEN                                            
126400     END-IF                                                               
126500     .                                                                    
126600     EJECT                                                                
126700                                                                          
126800 H-LAES-VISA-INFO SECTION.                                                
126900                                                                          
127000     MOVE +1 TO INDX                                                      
127100                                                                          
127200     IF IDARTNR-IFYLLT                                                    
127300       PERFORM IMS-GU-ORQF-ORQF01-M-IDARTNR                               
127400     ELSE                                                                 
127500       PERFORM IMS-GU-ORQF-ORQF01                                         
127600     END-IF                                                               
127700                                                                          
127800     IF SEGMENT-FINNS                                                     
127900                                                                          
128000       IF ODEL-IDDC-EXP = SPACE OR ODEL-IDDC-EXP = WC-CDC-SE              
128100          MOVE ORAD-KDVALISO           TO MOD-KDVALISO                    
128200       ELSE                                                               
128300          MOVE WS-SEK                  TO MOD-KDVALISO                    
128400       END-IF                                                             
128500                                                                          
128600       PERFORM HA-SPAR-WDQ401KY-ENTER                                     
128700       PERFORM HB-SPAR-WDQ401KY-NEXT                                      
128800                                                                          
128900       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
129000                                                                          
129100         PERFORM HC-FLYTTA-TILL-MOD                                       
129200                                                                          
129300         MOVE ORAD-IDARTNR TO W-WDD3-IDARTNR                              
129400         MOVE MED-IDSKYLT TO W-TEXT-IDSKYLT                               
129500         PERFORM HD-LAS-BENAEMNING                                        
129600                                                                          
129700         ADD +1 TO INDX                                                   
129800         IF IDARTNR-IFYLLT                                                
129900           PERFORM IMS-GN-ORQF-ORQF01-M-IDARTNR                           
130000         ELSE                                                             
130100           PERFORM IMS-GN-ORQF-ORQF01                                     
130200         END-IF                                                           
130300       END-PERFORM                                                        
130400                                                                          
130500       IF SEGMENT-FINNS                                                   
130600         PERFORM HB-SPAR-WDQ401KY-NEXT                                    
130700         IF NOT MFS-UPDATE                                                
130800           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
130900           CALL WMEDKONV USING MED-WMEDAREA                               
131000           MOVE MED-MFSINF TO MOD-TEMFSINF                                
131100         END-IF                                                           
131200       ELSE                                                               
131300         PERFORM UNTIL INDX > MAX-INDX                                    
131400           MOVE MFS-STAENG-FAELT-OSYNLIGT TO                              
131500                                 MOD-CMD-UPDATE-ATTR(INDX)                
131600           MOVE MFS-STAENG-FAELT-OSYNLIGT TO                              
131700                                 MOD-KVBEART-Q-UPDATE-ATTR(INDX)          
131800           MOVE MFS-STAENG-FAELT-OSYNLIGT TO                              
131900                                 MOD-PRARTNTO-UPDATE-ATTR(INDX)           
132000           MOVE MFS-STAENG-FAELT-OSYNLIGT TO                              
132100                                 MOD-BERADREF-UPDATE-ATTR(INDX)           
132200           MOVE ZERO TO MOD-IDARTNR-SPAR(INDX)                            
132300                        MOD-ADLAGOMR-SPAR(INDX)                           
132400                        MOD-ADGANG-SPAR(INDX)                             
132500                        MOD-ADPLATS-SPAR(INDX)                            
132600                        MOD-IDLOPNR-SPAR(INDX)                            
132700           ADD +1 TO INDX                                                 
132800         END-PERFORM                                                      
132900       END-IF                                                             
133000                                                                          
133100       PERFORM MFS-RENSA-FAELT-IN                                         
133200                                                                          
133300     ELSE                                                                 
133400       IF IDARTNR-IFYLLT                                                  
133500         MOVE INF-PART-MISSING TO MED-IDMFSINF                            
133600         CALL WMEDKONV USING MED-WMEDAREA                                 
133700         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
133800       ELSE                                                               
133900         MOVE ERR-LINES-MISSING TO MED-IDMFSFEL                           
134000         CALL WMEDKONV USING MED-WMEDAREA                                 
134100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
134200       END-IF                                                             
134300       PERFORM MFS-RENSA-FAELT-IN                                         
134400       PERFORM MFS-RENSA-FAELT-UT                                         
134500       MOVE MFS-RENSA-FAELT TO MOD-KVRADER                                
134600                               MOD-KVORDRAD                               
134700                               MOD-TEDDI                                  
134800                               MOD-KDVALISO                               
134900     END-IF                                                               
135000     .                                                                    
135100     EJECT                                                                
135200                                                                          
135300 HA-SPAR-WDQ401KY-ENTER SECTION.                                          
135400                                                                          
135500     MOVE ORAD-IDORDER  TO MOD-IDORDER-ENTER                              
135600     MOVE ORAD-IDDC     TO MOD-IDDC-ENTER                                 
135700     MOVE ORAD-ADLAGOMR TO MOD-ADLAGOMR-ENTER                             
135800     MOVE ORAD-ADGANG   TO MOD-ADGANG-ENTER                               
135900     MOVE ORAD-ADPLATS  TO MOD-ADPLATS-ENTER                              
136000     MOVE ORAD-IDARTNR  TO MOD-IDARTNR-ENTER                              
136100     MOVE ORAD-IDLOPNR  TO MOD-IDLOPNR-ENTER                              
136200     .                                                                    
136300     EJECT                                                                
136400                                                                          
136500 HB-SPAR-WDQ401KY-NEXT SECTION.                                           
136600                                                                          
136700     MOVE ORAD-IDORDER  TO MOD-IDORDER-NEXT                               
136800     MOVE ORAD-IDDC     TO MOD-IDDC-NEXT                                  
136900     MOVE ORAD-ADLAGOMR TO MOD-ADLAGOMR-NEXT                              
137000     MOVE ORAD-ADGANG   TO MOD-ADGANG-NEXT                                
137100     MOVE ORAD-ADPLATS  TO MOD-ADPLATS-NEXT                               
137200     MOVE ORAD-IDARTNR  TO MOD-IDARTNR-NEXT                               
137300     MOVE ORAD-IDLOPNR  TO MOD-IDLOPNR-NEXT                               
137400     .                                                                    
137500     EJECT                                                                
137600                                                                          
137700 HC-FLYTTA-TILL-MOD SECTION.                                              
137800                                                                          
137900     MOVE ORAD-IDARTNR  TO MOD-IDARTNR-RAD(INDX)                          
138000     MOVE ORAD-IDARTNR  TO MOD-IDARTNR-SPAR(INDX)                         
138100     MOVE ORAD-IDLOPNR  TO MOD-IDLOPNR-SPAR(INDX)                         
138200     MOVE ORAD-ADLAGOMR TO MOD-ADLAGOMR-SPAR(INDX)                        
138300     MOVE ORAD-ADGANG   TO MOD-ADGANG-SPAR(INDX)                          
138400     MOVE ORAD-ADPLATS  TO MOD-ADPLATS-SPAR(INDX)                         
138500     INSPECT MOD-IDARTNR-RAD(INDX) REPLACING LEADING ZERO                 
138600                                   BY SPACE                               
138700     MOVE ORAD-KVBEART-Q TO MOD-KVBEART-Q-RAD(INDX)                       
138800                                                                          
138900     IF ORAD-PRAVCOST > ZERO AND (ODEL-IDDC-EXP = SPACE OR                
139000         ODEL-IDDC-EXP = 11)                                              
139100         MOVE ORAD-PRAVCOST         TO MOD-PRARTNTO-RAD(INDX)             
139200         MOVE ' '                   TO MOD-TEASTRIX-RAD(INDX)             
139300     ELSE                                                                 
139400       IF DIST79-DEALER-PRICE                                             
139500                                                                          
139600         IF ORAD-PRARTNTO-LOC > +0                                        
139700           MOVE ORAD-PRARTNTO-LOC   TO MOD-PRARTNTO-RAD(INDX)             
139800           MOVE ' '                 TO MOD-TEASTRIX-RAD(INDX)             
139900         ELSE                                                             
140000           MOVE ORAD-PRARTNTO-LOCPREL TO MOD-PRARTNTO-RAD(INDX)           
140100           MOVE '*'                 TO MOD-TEASTRIX-RAD(INDX)             
140200         END-IF                                                           
140300       ELSE                                                               
140500         IF DIST79-ECOM-PRICE                                             
140600            MOVE ORAD-PRARTNTO-LOC  TO MOD-PRARTNTO-RAD(INDX)             
140700            MOVE ' '          TO MOD-TEASTRIX-RAD(INDX)                   
140800         ELSE                                                             
140900           MOVE ORAD-PRARTNTO TO MOD-PRARTNTO-RAD(INDX)                   
141000           MOVE ' '          TO MOD-TEASTRIX-RAD(INDX)                    
141100         END-IF                                                           
141200*        MOVE ' '          TO MOD-KDVALISO                                
141300       END-IF                                                             
141400     END-IF                                                               
141500     MOVE ORAD-BERADREF  TO MOD-BERADREF-RAD(INDX)                        
141600     MOVE ORAD-IDDC      TO MOD-IDDC-RAD(INDX)                            
141700                                                                          
141800     IF ORAD-IDKUNDRF-RO NOT = '0000000   ' AND                           
141900        ORAD-IDKUNDRF-RO NOT = ORAD-IDKUNDRF                              
142000       MOVE ORAD-IDKUNDRF-RO (1:7)    TO MOD-OREF-RAD(INDX)               
142100       INSPECT MOD-OREF-RAD(INDX) REPLACING LEADING                       
142200                                  ZERO BY SPACE                           
142300       MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                  
142400                                MOD-KVBEART-Q-UPDATE-ATTR(INDX)           
142500       MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                  
142600                                MOD-PRARTNTO-UPDATE-ATTR(INDX)            
142700       MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                  
142800                                MOD-BERADREF-UPDATE-ATTR(INDX)            
142900     ELSE                                                                 
143000       MOVE MFS-RENSA-FAELT           TO MOD-OREF-RAD(INDX)               
143100     END-IF                                                               
143200     .                                                                    
143300     EJECT                                                                
143400                                                                          
143500 HD-LAS-BENAEMNING SECTION.                                               
143600                                                                          
143700     PERFORM IMS-GU-BENA-BENA11                                           
143800     IF SEGMENT-FINNS                                                     
143900       MOVE TEXT-BEART TO MOD-BEART-RAD(INDX)                             
144000     END-IF                                                               
144100     .                                                                    
144200     EJECT                                                                
144300                                                                          
144400 I-KOLLA-INPUT SECTION.                                                   
144500                                                                          
144600     MOVE JA  TO INDATA-SW                                                
144700                                                                          
144800     IF MID-INPUT = ALL '+'                                               
144900       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
145000       CALL WMEDKONV USING MED-WMEDAREA                                   
145100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
145200     ELSE                                                                 
145300       IF MID-FLAGGA-UPDATE NOT = ALL '+'                                 
145400         IF MID-FLAGGA-UPDATE = JA OR                                     
145500           MID-FLAGGA-UPDATE = YES OR                                     
145600           MID-FLAGGA-UPDATE = NEJ                                        
145700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAGGA-UPDATE-ATTR            
145800         ELSE                                                             
145900           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAGGA-UPDATE-ATTR              
146000           MOVE NEJ TO INDATA-SW                                          
146100         END-IF                                                           
146200       END-IF                                                             
146300                                                                          
146400       MOVE +1 TO INDX                                                    
146500       PERFORM UNTIL INDX > MAX-INDX OR                                   
146600         MID-IDARTNR-SPAR(INDX) = ZERO                                    
146700        IF MID-CMD-UPDATE(INDX) NOT = ALL '+'                             
146800          IF MID-CMD-UPDATE(INDX) = 'D' OR                                
146900            MID-CMD-UPDATE(INDX) = 'X' OR                                 
147000            MID-CMD-UPDATE(INDX) = 'A'                                    
147100            MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-UPDATE-ATTR              
147200                                                       (INDX)             
147300          ELSE                                                            
147400            MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR                
147500                                                  (INDX)                  
147600            MOVE NEJ TO INDATA-SW                                         
147700          END-IF                                                          
147800        END-IF                                                            
147900                                                                          
148000        IF MID-KVBEART-Q-UPDATE(INDX) NOT = ALL '+'                       
148100         IF MID-KVBEART-Q-UPDATE(INDX) NOT NUMERIC                        
148200           MOVE MFS-NUM-FAELT-FEL TO MOD-KVBEART-Q-UPDATE-ATTR            
148300                                                        (INDX)            
148400           MOVE NEJ TO INDATA-SW                                          
148500         ELSE                                                             
148600           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVBEART-Q-UPDATE-ATTR          
148700                                                         (INDX)           
148800           MOVE MID-KVBEART-Q-UPDATE(INDX) TO WS-KVBEART-Q(INDX)          
148900         END-IF                                                           
149000        END-IF                                                            
149100                                                                          
149200        IF MID-PRARTNTO-UPDATE(INDX) NOT = ALL '+'                        
149300          MOVE MID-PRARTNTO-UPDATE(INDX) TO DEC-IDFRIDATA                 
149400          MOVE +7 TO DEC-KVHELTAL                                         
149500          MOVE +2 TO DEC-KVDECIMAL                                        
149600          CALL WDECEDIT USING DEC-WDECAREA                                
149700          IF DEC-KDSVAR-FEL                                               
149800            MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-UPDATE-ATTR            
149900                                                        (INDX)            
150000            MOVE NEJ TO INDATA-SW                                         
150100          ELSE                                                            
150200            MOVE MFS-NUM-FAELT-RAETT TO                                   
150300                 MOD-PRARTNTO-UPDATE-ATTR(INDX)                           
150400            MOVE DEC-IDEDITDATA TO WS-PRARTNTO(INDX)                      
150500            IF WS-PRARTNTO(INDX) = ZERO                                   
150600               MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-UPDATE-ATTR         
150700                                                        (INDX)            
150800               MOVE NEJ TO INDATA-SW                                      
150900            END-IF                                                        
151000          END-IF                                                          
151100        END-IF                                                            
151200                                                                          
151300        IF MID-BERADREF-UPDATE(INDX) NOT = ALL '+'                        
151400          MOVE MFS-ALFA-FAELT-RAETT TO MOD-BERADREF-UPDATE-ATTR           
151500                                                         (INDX)           
151600        END-IF                                                            
151700        ADD +1 TO INDX                                                    
151800       END-PERFORM                                                        
151900                                                                          
152000       IF INDATA-FEL                                                      
152100         PERFORM UNTIL INDX > MAX-INDX                                    
152200         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
152300                                  MOD-CMD-UPDATE-ATTR(INDX)               
152400         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
152500                                  MOD-KVBEART-Q-UPDATE-ATTR(INDX)         
152600         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
152700                                  MOD-PRARTNTO-UPDATE-ATTR(INDX)          
152800         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
152900                                  MOD-BERADREF-UPDATE-ATTR(INDX)          
153000         ADD +1 TO INDX                                                   
153100         END-PERFORM                                                      
153200         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
153300         CALL WMEDKONV USING MED-WMEDAREA                                 
153400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
153500         PERFORM MFS-ROR-EJ-FAELT-UT                                      
153600         PERFORM MFS-ROR-EJ-FAELT-IN                                      
153700       END-IF                                                             
153800     END-IF                                                               
153900     .                                                                    
154000     EJECT                                                                
154100                                                                          
154200 J-UPPDATERA SECTION.                                                     
154300                                                                          
154400     IF MID-FLAGGA-UPDATE = 'J' OR 'Y'                                    
154500                                                                          
154600       IF MOJLIG-ANTAL NOT = TOTAL-KVRADER                                
154700         MOVE ERR-ANNULL-NOT-POSS TO MED-IDMFSFEL                         
154800         CALL WMEDKONV USING MED-WMEDAREA                                 
154900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
155000         MOVE NEJ TO ALLT-SW                                              
155100       ELSE                                                               
155200         PERFORM JA-ANNULLERA-HEL-ORDER                                   
155300         PERFORM MFS-RENSA-FAELT-UT                                       
155400         MOVE MFS-RENSA-FAELT TO MOD-KVRADER                              
155500                                 MOD-KVORDRAD                             
155600                                 MOD-TEDDI                                
155700                                 MOD-KDVALISO                             
155800       END-IF                                                             
155900     ELSE                                                                 
156000       PERFORM JB-UPPDATERA-EN-SIDA                                       
156100     END-IF                                                               
156200                                                                          
156300     IF MID-INPUT NOT = ALL '+'                                           
156400       IF INDATA-OK                                                       
156500         IF ANNULL-HELORDER                                               
156600           MOVE INF-ORDER-DELETE TO MED-IDMFSINF                          
156700           CALL WMEDKONV USING MED-WMEDAREA                               
156800           MOVE MED-MFSINF TO MOD-TEMFSINF                                
156900           PERFORM MFS-FORM-ATTR                                          
157000           PERFORM MFS-RENSA-FAELT-IN                                     
157100           MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                           
157200         ELSE                                                             
157300           IF MID-FLAGGA-UPDATE = 'Y' OR 'J'                              
157400             PERFORM MFS-FORM-ATTR                                        
157500             PERFORM MFS-RENSA-FAELT-IN                                   
157600           ELSE                                                           
157700             IF RAD-AENDRING                                              
157800               MOVE INF-UPDATE-DONE TO MED-IDMFSINF                       
157900               CALL WMEDKONV USING MED-WMEDAREA                           
158000               MOVE MED-MFSINF TO MOD-TEMFSINF                            
158100               PERFORM MFS-FORM-ATTR                                      
158200               PERFORM MFS-RENSA-FAELT-IN                                 
158300             ELSE                                                         
158400               MOVE INF-ORDERLINE-DELETE TO MED-IDMFSINF                  
158500               CALL WMEDKONV USING MED-WMEDAREA                           
158600               MOVE MED-MFSINF TO MOD-TEMFSINF                            
158700               PERFORM MFS-FORM-ATTR                                      
158800               PERFORM MFS-RENSA-FAELT-IN                                 
158900               MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                       
159000             END-IF                                                       
159100           END-IF                                                         
159200         END-IF                                                           
159300       END-IF                                                             
159400     END-IF                                                               
159500     .                                                                    
159600     EJECT                                                                
159700                                                                          
159800 JA-ANNULLERA-HEL-ORDER SECTION.                                          
159900                                                                          
160000     MOVE JA TO ANNULL-SW                                                 
160100                                                                          
160200     PERFORM IMS-GHU-ORQI-ORQI01                                          
160300                                                                          
160400     IF SEGMENT-FINNS                                                     
160500       MOVE 'N' TO OHUV-FLKLAR                                            
160600       MOVE 'J' TO OHUV-FLBORT                                            
160700       PERFORM IMS-REPL-ORQI01                                            
160800     END-IF                                                               
160900                                                                          
161000     PERFORM S19-DELETE-PRICE-Q-ORDER                                     
161100     MOVE SPACE             TO  ALT1-MID                                  
161200     MOVE WS-IDORDER        TO  ALT1-MID-IDORDER                          
161300     PERFORM IMS-INSERT-ALTMSG                                            
161400     .                                                                    
161500     EJECT                                                                
161600                                                                          
161700 JB-UPPDATERA-EN-SIDA SECTION.                                            
161800                                                                          
161900     MOVE +1  TO INDX                                                     
162000     MOVE JA  TO INDATA-SW                                                
162100                 CMD-SW                                                   
162200     MOVE NEJ TO AVSR-SW                                                  
162300     MOVE NEJ TO RAD-SW                                                   
162400                                                                          
162500     PERFORM JBA-KOLLA-Q4RAD-MOT-INPUT                                    
162600                                                                          
162700     IF INDATA-OK                                                         
162800                                                                          
162900       PERFORM JBB-BEHANDLA-INPUT-UPDATE-Q4                               
163000                                                                          
163100     ELSE                                                                 
163200                                                                          
163300       PERFORM UNTIL INDX > MAX-INDX                                      
163400         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
163500                                  MOD-CMD-UPDATE-ATTR(INDX)               
163600         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
163700                                  MOD-KVBEART-Q-UPDATE-ATTR(INDX)         
163800         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
163900                                  MOD-PRARTNTO-UPDATE-ATTR(INDX)          
164000         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
164100                                  MOD-BERADREF-UPDATE-ATTR(INDX)          
164200         ADD +1 TO INDX                                                   
164300       END-PERFORM                                                        
164400                                                                          
164500       IF CMD-FEL                                                         
164600         MOVE ERR-CMD-FEL TO MED-IDMFSFEL                                 
164700         CALL WMEDKONV USING MED-WMEDAREA                                 
164800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
164900       ELSE                                                               
165000         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
165100         CALL WMEDKONV USING MED-WMEDAREA                                 
165200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
165300       END-IF                                                             
165400       PERFORM MFS-ROR-EJ-FAELT-UT                                        
165500       PERFORM MFS-ROR-EJ-FAELT-IN                                        
165600     END-IF                                                               
165700     .                                                                    
165800     EJECT                                                                
165900                                                                          
166000 JBA-KOLLA-Q4RAD-MOT-INPUT SECTION.                                       
166100                                                                          
166200     PERFORM UNTIL INDX > MAX-INDX OR                                     
166300       MID-IDARTNR-SPAR(INDX) = ZERO                                      
166400                                                                          
166500       MOVE MID-IDARTNR-SPAR(INDX)  TO W-ORAD-IDARTNR-UNIK                
166600       MOVE MID-IDLOPNR-SPAR(INDX)  TO W-ORAD-IDLOPNR-UNIK                
166700       MOVE MID-ADLAGOMR-SPAR(INDX) TO W-ORAD-ADLAGOMR-UNIK               
166800       MOVE MID-ADGANG-SPAR(INDX)   TO W-ORAD-ADGANG-UNIK                 
166900       MOVE MID-ADPLATS-SPAR(INDX)  TO W-ORAD-ADPLATS-UNIK                
167000                                                                          
167100       PERFORM IMS-GHU-ORQF-ORQF01                                        
167200                                                                          
167300       IF SEGMENT-FINNS                                                   
167400                                                                          
167500         IF MID-CMD-UPDATE(INDX) NOT = ALL '+'                            
167600                                                                          
167700           PERFORM JBAA-RAD-KOMMAND-IFYLLT                                
167800                                                                          
167900         ELSE                                                             
168000                                                                          
168100           PERFORM JBAB-RAD-KOMMAND-ALL-PLUS                              
168200                                                                          
168300         END-IF                                                           
168400       END-IF                                                             
168500       ADD +1 TO INDX                                                     
168600     END-PERFORM                                                          
168700     .                                                                    
168800     EJECT                                                                
168900                                                                          
169000 JBAA-RAD-KOMMAND-IFYLLT SECTION.                                         
169100                                                                          
169200     IF ORAD-IDKUNDRF-RO NOT = '0000000   ' AND                           
169300        ORAD-IDKUNDRF-RO NOT = ORAD-IDKUNDRF                              
169400       IF MID-CMD-UPDATE(INDX) NOT = 'X'                                  
169500         MOVE NEJ TO INDATA-SW                                            
169600                     CMD-SW                                               
169700         MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR                   
169800                                                (INDX)                    
169900         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
170000                      MOD-KVBEART-Q-UPDATE-ATTR(INDX)                     
170100         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
170200                      MOD-PRARTNTO-UPDATE-ATTR(INDX)                      
170300         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
170400                      MOD-BERADREF-UPDATE-ATTR(INDX)                      
170500                                                                          
170600       END-IF                                                             
170700     ELSE                                                                 
170800       IF ORAD-IDKUNDRF-RO = '0000000   ' OR                              
170900          ORAD-IDKUNDRF-RO = ORAD-IDKUNDRF                                
171000         IF MID-CMD-UPDATE(INDX) = 'X'                                    
171100           MOVE NEJ TO INDATA-SW                                          
171200           MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR                 
171300                                                  (INDX)                  
171400         ELSE                                                             
171500           IF MID-CMD-UPDATE(INDX) = 'D'                                  
171600            PERFORM JBAAA-COMMAND-DELETE                                  
171700           ELSE                                                           
171800             IF MID-CMD-UPDATE(INDX) = 'A'                                
171900              PERFORM JBAAB-COMMAND-AENDRA-RAD                            
172000             END-IF                                                       
172100           END-IF                                                         
172200         END-IF                                                           
172300       END-IF                                                             
172400     END-IF                                                               
172500     .                                                                    
172600     EJECT                                                                
172700                                                                          
172800 JBAAA-COMMAND-DELETE SECTION.                                            
172900                                                                          
173000     IF MID-KVBEART-Q-UPDATE(INDX) NOT = ALL '+'                          
173100      MOVE NEJ TO INDATA-SW                                               
173200      MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR                      
173300                                               (INDX)                     
173400      MOVE MFS-NUM-FAELT-FEL TO                                           
173500                      MOD-KVBEART-Q-UPDATE-ATTR(INDX)                     
173600     END-IF                                                               
173700     IF MID-PRARTNTO-UPDATE(INDX) NOT = ALL '+'                           
173800      MOVE NEJ TO INDATA-SW                                               
173900      MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR                      
174000                                               (INDX)                     
174100      MOVE MFS-NUM-FAELT-FEL TO                                           
174200                      MOD-PRARTNTO-UPDATE-ATTR(INDX)                      
174300     END-IF                                                               
174400     IF MID-BERADREF-UPDATE(INDX) NOT = ALL '+'                           
174500      MOVE NEJ TO INDATA-SW                                               
174600      MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR                      
174700                                               (INDX)                     
174800      MOVE MFS-NUM-FAELT-FEL TO                                           
174900                      MOD-BERADREF-UPDATE-ATTR(INDX)                      
175000     END-IF                                                               
175100     .                                                                    
175200     EJECT                                                                
175300                                                                          
175400 JBAAB-COMMAND-AENDRA-RAD SECTION.                                        
175500                                                                          
175600     IF MID-KVBEART-Q-UPDATE(INDX) NOT = ALL '+'                          
175700       IF ORAD-KVBEART-Q < WS-KVBEART-Q(INDX)                             
175800         MOVE NEJ TO INDATA-SW                                            
175900         MOVE MFS-NUM-FAELT-FEL TO                                        
176000                    MOD-KVBEART-Q-UPDATE-ATTR(INDX)                       
176100       END-IF                                                             
176200     END-IF                                                               
176300     IF (MID-KVBEART-Q-UPDATE(INDX) = ALL '+' AND                         
176400         MID-PRARTNTO-UPDATE(INDX) = ALL '+' AND                          
176500         MID-BERADREF-UPDATE(INDX) = ALL '+')                             
176600        MOVE NEJ TO INDATA-SW                                             
176700        MOVE MFS-NUM-FAELT-FEL TO                                         
176800                   MOD-KVBEART-Q-UPDATE-ATTR(INDX)                        
176900        MOVE MFS-NUM-FAELT-FEL TO                                         
177000                   MOD-PRARTNTO-UPDATE-ATTR(INDX)                         
177100        MOVE MFS-ALFA-FAELT-FEL TO                                        
177200                   MOD-BERADREF-UPDATE-ATTR(INDX)                         
177300        MOVE MFS-ALFA-FAELT-FEL TO                                        
177400                   MOD-CMD-UPDATE-ATTR(INDX)                              
177500     END-IF                                                               
177600     .                                                                    
177700     EJECT                                                                
177800                                                                          
177900 JBAB-RAD-KOMMAND-ALL-PLUS SECTION.                                       
178000                                                                          
178100     IF ORAD-IDKUNDRF-RO NOT = '0000000   ' AND                           
178200        ORAD-IDKUNDRF-RO NOT = ORAD-IDKUNDRF                              
178300         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
178400                      MOD-KVBEART-Q-UPDATE-ATTR(INDX)                     
178500         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
178600                      MOD-PRARTNTO-UPDATE-ATTR(INDX)                      
178700         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
178800                      MOD-BERADREF-UPDATE-ATTR(INDX)                      
178900     END-IF                                                               
179000                                                                          
179100     IF MID-KVBEART-Q-UPDATE(INDX) NOT = ALL '+'                          
179200       MOVE NEJ TO INDATA-SW                                              
179300       MOVE MFS-ALFA-FAELT-FEL TO                                         
179400                  MOD-CMD-UPDATE-ATTR(INDX)                               
179500       MOVE MFS-NUM-FAELT-FEL TO                                          
179600                  MOD-KVBEART-Q-UPDATE-ATTR(INDX)                         
179700     END-IF                                                               
179800     IF MID-PRARTNTO-UPDATE(INDX) NOT = ALL '+'                           
179900       MOVE NEJ TO INDATA-SW                                              
180000       MOVE MFS-ALFA-FAELT-FEL TO                                         
180100                  MOD-CMD-UPDATE-ATTR(INDX)                               
180200       MOVE MFS-NUM-FAELT-FEL TO                                          
180300                  MOD-PRARTNTO-UPDATE-ATTR(INDX)                          
180400     END-IF                                                               
180500     IF MID-BERADREF-UPDATE(INDX) NOT = ALL '+'                           
180600       MOVE NEJ TO INDATA-SW                                              
180700       MOVE MFS-ALFA-FAELT-FEL TO                                         
180800                  MOD-CMD-UPDATE-ATTR(INDX)                               
180900       MOVE MFS-NUM-FAELT-FEL TO                                          
181000                  MOD-BERADREF-UPDATE-ATTR(INDX)                          
181100     END-IF                                                               
181200     .                                                                    
181300     EJECT                                                                
181400                                                                          
181500 JBB-BEHANDLA-INPUT-UPDATE-Q4 SECTION.                                    
181600                                                                          
181700     MOVE +1 TO INDX                                                      
181800     MOVE JA TO FIRST-TIME-SW                                             
181900                                                                          
182000     PERFORM IMS-GHU-ORQI-ORQI01                                          
182100                                                                          
182200     IF SEGMENT-FINNS                                                     
182300       MOVE 'N' TO OHUV-FLKLAR                                            
182400       PERFORM IMS-REPL-ORQI01                                            
182500     END-IF                                                               
182600                                                                          
182700     PERFORM UNTIL INDX > MAX-INDX                                        
182800                                                                          
182900       MOVE +1 TO AVSR-INDX                                               
183000       MOVE NEJ TO KVBEART-SW                                             
183100                                                                          
183200       IF MID-CMD-UPDATE(INDX) NOT = ALL '+'                              
183300                                                                          
183400         PERFORM JBBA-COMMAND-IFYLLD                                      
183500                                                                          
183600       ELSE                                                               
183700                                                                          
183800         PERFORM JBBB-COMMAND-EJ-IFYLLD                                   
183900                                                                          
184000       END-IF                                                             
184100                                                                          
184200     END-PERFORM                                                          
184300                                                                          
184400     IF FIRST-TIME                                                        
184500       PERFORM JBBC-FLYTTA-NEXT-ARTNR                                     
184600     END-IF                                                               
184700                                                                          
184800     PERFORM JBBD-AVSLUTA-OCH-LAES-IGEN                                   
184900     .                                                                    
185000     EJECT                                                                
185100                                                                          
185200 JBBA-COMMAND-IFYLLD SECTION.                                             
185300                                                                          
185400     MOVE MID-IDARTNR-SPAR(INDX)  TO W-ORAD-IDARTNR-UNIK                  
185500     MOVE MID-IDLOPNR-SPAR(INDX)  TO W-ORAD-IDLOPNR-UNIK                  
185600     MOVE MID-ADLAGOMR-SPAR(INDX) TO W-ORAD-ADLAGOMR-UNIK                 
185700     MOVE MID-ADGANG-SPAR(INDX)   TO W-ORAD-ADGANG-UNIK                   
185800     MOVE MID-ADPLATS-SPAR(INDX)  TO W-ORAD-ADPLATS-UNIK                  
185900                                                                          
186000     PERFORM IMS-GHU-ORQF-ORQF01                                          
186100                                                                          
186200     IF SEGMENT-FINNS                                                     
186300                                                                          
186400                                                                          
186500       IF MID-CMD-UPDATE(INDX) = 'D'                                      
186600         PERFORM S04-TABORT-RADEN                                         
186700       END-IF                                                             
186800                                                                          
186900       IF MID-CMD-UPDATE(INDX) = 'X'                                      
187000         PERFORM S05-BACKA-BIPACKADE-RADER                                
187100         MOVE JA TO RAD-SW                                                
187200       END-IF                                                             
187300                                                                          
187400       IF MID-CMD-UPDATE(INDX) = 'A'                                      
187500         PERFORM JBBAA-AENDRA-Q4-RAD                                      
187600       END-IF                                                             
187700                                                                          
187800     END-IF                                                               
187900     ADD +1 TO INDX                                                       
188000     .                                                                    
188100     EJECT                                                                
188200                                                                          
188300 JBBAA-AENDRA-Q4-RAD SECTION.                                             
188400                                                                          
188500     IF MID-BERADREF-UPDATE(INDX) NOT = ALL '+'                           
188600       IF FIRST-TIME                                                      
188700         PERFORM S14-SPARA-ARTIKEL                                        
188800         MOVE NEJ TO FIRST-TIME-SW                                        
188900       END-IF                                                             
189000       MOVE MID-BERADREF-UPDATE(INDX) TO ORAD-BERADREF                    
189100       MOVE JA TO RAD-SW                                                  
189200     END-IF                                                               
189300                                                                          
189400     IF MID-PRARTNTO-UPDATE(INDX) NOT = ALL '+' AND                       
189500        MID-KVBEART-Q-UPDATE(INDX) = ALL '+'                              
189600       IF FIRST-TIME                                                      
189700         PERFORM S14-SPARA-ARTIKEL                                        
189800         MOVE NEJ TO FIRST-TIME-SW                                        
189900       END-IF                                                             
190000       MOVE ORAD-KVBEART-Q TO ANNULLERAT-ANTAL                            
190100       PERFORM S06-DELETE-PRARTNTO-AVSR                                   
190200       PERFORM S07-AVSR-ANROP                                             
190300       PERFORM S08-AENDRA-PRARTNTO                                        
190400       MOVE +1 TO AVSR-INDX                                               
190500       PERFORM S09-TILLAEGG-PRARTNTO-AVSR                                 
190600       PERFORM S07-AVSR-ANROP                                             
190700       MOVE JA TO RAD-SW                                                  
190800     END-IF                                                               
190900                                                                          
191000     IF MID-KVBEART-Q-UPDATE(INDX) NOT = ALL '+'                          
191100      PERFORM JBBAAA-AENDRA-Q4-ANTAL                                      
191200     END-IF                                                               
191300                                                                          
191400     IF NOT KVBEART-OK                                                    
191500       PERFORM S21-CHANGE-PRICE-Q-LINE                                    
191600       PERFORM IMS-REPL-ORQF                                              
191700     END-IF                                                               
191800     .                                                                    
191900     EJECT                                                                
192000                                                                          
192100 JBBAAA-AENDRA-Q4-ANTAL SECTION.                                          
192200                                                                          
192300     IF WS-KVBEART-Q(INDX) = ZERO                                         
192400       MOVE JA TO KVBEART-SW                                              
192500       PERFORM S04-TABORT-RADEN                                           
192600     ELSE                                                                 
192700       IF FIRST-TIME                                                      
192800         PERFORM S14-SPARA-ARTIKEL                                        
192900         MOVE NEJ TO FIRST-TIME-SW                                        
193000       END-IF                                                             
193100                                                                          
193200       COMPUTE MINSKAT-ANTAL = ORAD-KVBEART-Q                             
193300                        - WS-KVBEART-Q(INDX)                              
193400       MOVE MINSKAT-ANTAL TO ANNULLERAT-ANTAL                             
193500                                                                          
193600       MOVE NEJ TO SW-WLARTC-LAST                                         
193700       IF MID-PRARTNTO-UPDATE(INDX) NOT = ALL '+'                         
193800         MOVE ORAD-KVBEART-Q TO ANNULLERAT-ANTAL                          
193900         PERFORM S06-DELETE-PRARTNTO-AVSR                                 
194000         MOVE JA TO SW-WLARTC-LAST                                        
194100         PERFORM S07-AVSR-ANROP                                           
194200         PERFORM S08-AENDRA-PRARTNTO                                      
194300         MOVE +1 TO AVSR-INDX                                             
194400         MOVE MINSKAT-ANTAL TO ANNULLERAT-ANTAL                           
194500         PERFORM JBBAAAA-AENDRA-KVBEART-Q                                 
194600         MOVE WS-KVBEART-Q(INDX) TO ORAD-KVBEART-Q                        
194700                                    ORAD-KVBEART                          
194800         PERFORM S09-TILLAEGG-PRARTNTO-AVSR                               
194900         PERFORM S07-AVSR-ANROP                                           
195000       ELSE                                                               
195100         PERFORM JBBAAAA-AENDRA-KVBEART-Q                                 
195200         PERFORM S10-SKAPA-AVSR                                           
195300         PERFORM S07-AVSR-ANROP                                           
195400         MOVE WS-KVBEART-Q(INDX) TO ORAD-KVBEART-Q                        
195500                                    ORAD-KVBEART                          
195600       END-IF                                                             
195700                                                                          
195800       MOVE +0 TO WS-ANTOBKR                                              
195900       PERFORM S11-SKAPA-ORDERBEKR                                        
196000       PERFORM S12-SKAPA-TRANSAR                                          
196100       IF WS-IDDC NOT = W-IDDC-B6                                         
196200          MOVE WS-IDDC TO W-IDDC-B6                                       
196300          PERFORM IMS-GU-WDB601                                           
196400       END-IF                                                             
196500       IF DCS-NDC-NA                                                      
196600          PERFORM S18-DATA-TILL-DEL-NOTE                                  
196700       END-IF                                                             
196800       MOVE JA TO RAD-SW                                                  
196900     END-IF                                                               
197000     .                                                                    
197100     EJECT                                                                
197200                                                                          
197300 JBBAAAA-AENDRA-KVBEART-Q SECTION.                                        
197400                                                                          
197500     MOVE ORAD-KVPRERO TO SPAR-ORAD-KVPRERO                               
197600                                                                          
197700     IF MINSKAT-ANTAL > ORAD-KVPRERO                                      
197800       COMPUTE MINSKAT-ANTAL = MINSKAT-ANTAL - ORAD-KVPRERO               
197900       MOVE ZERO TO ORAD-KVPRERO                                          
198000       COMPUTE ORAD-KVPREAVB = ORAD-KVPREAVB - MINSKAT-ANTAL              
198100     ELSE                                                                 
198200       COMPUTE ORAD-KVPRERO = ORAD-KVPRERO - MINSKAT-ANTAL                
198300     END-IF                                                               
198400                                                                          
198500     MOVE ORAD-IDARTNR TO W-IDARTNR                                       
198600     IF ORAD-IDKAMPRF > ZERO                                              
198700       MOVE ORAD-IDKAMPRF       TO W-KAMP-IDKAMPRF                        
198800       MOVE ORAD-IDDC           TO W-KAMP-IDDC                            
198900       MOVE ORAD-IDARTNR        TO W-KART-IDARTNR                         
199000       MOVE ORAD-IDDISTR        TO W-KMRK-IDDISTR-FOM                     
199100       MOVE ORAD-IDDISTR        TO W-KMRK-IDDISTR-TOM                     
199200       MOVE ORAD-IDKUNDNR       TO W-KMRK-IDKUNDNR-FOM                    
199300       MOVE ORAD-IDKUNDNR       TO W-KMRK-IDKUNDNR-TOM                    
199400       PERFORM S13-BACKA-KAMPANJ                                          
199500     END-IF                                                               
199600                                                                          
199700     PERFORM JBBAAAAA-LAS-K9-SKAPA-ORDBEKR                                
199800     .                                                                    
199900     EJECT                                                                
200000                                                                          
200100 JBBAAAAA-LAS-K9-SKAPA-ORDBEKR SECTION.                                   
200200                                                                          
200300     MOVE ORAD-IDARTNR TO W-IDARTNR                                       
200400                                                                          
200500     IF OHUV-IDSYSTEM = 'W216' AND OHUV-FLORDSPE = JA                     
200600       CONTINUE                                                           
200700*SKROT FRÅN 6322 RÄKNAR INTE UPP OKS/PREAVB                               
200800     ELSE                                                                 
200900       IF ORAD-IDLEVNR = SPACE                                            
201000         IF ORAD-IDDC NOT = W-IDDC-B6                                     
201100            MOVE ORAD-IDDC TO W-IDDC-B6                                   
201200            PERFORM IMS-GU-WDB601                                         
201300         END-IF                                                           
201400         IF DCS-CDC                                                       
201500           PERFORM IMS-GHU-ARTM-ARTM01                                    
201600           IF SEGMENT-FINNS                                               
201700             PERFORM JBBAAAAAA-UPPDATERA-ARTM                             
201800             PERFORM IMS-REPL-ARTM                                        
201900           END-IF                                                         
202000         ELSE                                                             
202100           IF OHUV-KDORDKL = +0                                           
202200              PERFORM S04C-DELBACKA-NYVORKO                               
202300           END-IF                                                         
202400           MOVE WS-IDDC    TO W-IDDC                                      
202500           PERFORM IMS-GHU-ARTS-ARTS11                                    
202600           IF OHUV-KDORDKL = +0 OR +1                                     
202700             COMPUTE SLAG-KVOKS-DAG =                                     
202800                     SLAG-KVOKS-DAG - ANNULLERAT-ANTAL                    
202900           ELSE                                                           
203000             IF (OHUV-KDORDKL = +2 OR +3 OR +4)                           
203100             AND ORAD-KVOKS-PREL = ZERO                                   
203200               COMPUTE SLAG-KVOKS-BULK =                                  
203300                       SLAG-KVOKS-BULK - ANNULLERAT-ANTAL                 
203400             END-IF                                                       
203500           END-IF                                                         
203600           PERFORM IMS-REPL-ARTS                                          
203700         END-IF                                                           
203800       END-IF                                                             
203900       PERFORM S17-FIXA-REFILL-TRANSFER                                   
204000     END-IF                                                               
204100                                                                          
204200     IF SW-WLARTC-LAST = JA                                               
204300*      SWITCHEN SÄTTS I JBBAAA- STRAX EFTER PERFORM S06-DELETE-...        
204400       CONTINUE                                                           
204500     ELSE                                                                 
204600       PERFORM IMS-GET-ARTC11                                             
204700       MOVE    CLAG-KVQPACK-1 TO WS-KVQPACK-1                             
204800       MOVE    CLAG-TIDISPIN  TO WS-TIDISPIN                              
204900     END-IF                                                               
205000     .                                                                    
205100     EJECT                                                                
205200                                                                          
205300 JBBAAAAAA-UPPDATERA-ARTM SECTION.                                        
205400                                                                          
205500     MOVE ANNULLERAT-ANTAL TO MINSKAT-ANTAL                               
205600                                                                          
205700     IF OHUV-KDORDKL = ZERO                                               
205800                                                                          
205900       IF ORAD-IDKAMPRF NOT > ZERO                                        
206000         COMPUTE ART-KVOKS-VOR =                                          
206100          ART-KVOKS-VOR - MINSKAT-ANTAL                                   
206200       END-IF                                                             
206300                                                                          
206400       IF MINSKAT-ANTAL > SPAR-ORAD-KVPRERO                               
206500         COMPUTE ART-KVPREAVB-VOR =                                       
206600           ART-KVPREAVB-VOR - MINSKAT-ANTAL                               
206700       END-IF                                                             
206800                                                                          
206900       PERFORM S04C-DELBACKA-NYVORKO                                      
207000                                                                          
207100     END-IF                                                               
207200                                                                          
207300     IF OHUV-KDORDKL = +1                                                 
207400                                                                          
207500       IF ORAD-IDKAMPRF NOT > ZERO                                        
207600       COMPUTE ART-KVOKS-DAG =                                            
207700           ART-KVOKS-DAG - MINSKAT-ANTAL                                  
207800       END-IF                                                             
207900                                                                          
208000       IF MINSKAT-ANTAL > SPAR-ORAD-KVPRERO                               
208100         COMPUTE MINSKAT-ANTAL = MINSKAT-ANTAL -                          
208200                                        SPAR-ORAD-KVPRERO                 
208300         COMPUTE ART-KVPREAVB-DAG =                                       
208400           ART-KVPREAVB-DAG - MINSKAT-ANTAL                               
208500         COMPUTE ART-KVPRERO-DAG =                                        
208600           ART-KVPRERO-DAG - SPAR-ORAD-KVPRERO                            
208700       ELSE                                                               
208800         COMPUTE ART-KVPRERO-DAG =                                        
208900           ART-KVPRERO-DAG - MINSKAT-ANTAL                                
209000       END-IF                                                             
209100                                                                          
209200     END-IF                                                               
209300                                                                          
209400     IF OHUV-KDORDKL = +2 OR +3 OR +4                                     
209500                                                                          
209600       IF ORAD-IDKAMPRF NOT > ZERO                                        
209700         COMPUTE ART-KVOKS-BULK =                                         
209800         ART-KVOKS-BULK - MINSKAT-ANTAL                                   
209900       END-IF                                                             
210000                                                                          
210100       IF MINSKAT-ANTAL > SPAR-ORAD-KVPRERO                               
210200         COMPUTE MINSKAT-ANTAL = MINSKAT-ANTAL -                          
210300                                         SPAR-ORAD-KVPRERO                
210400         COMPUTE ART-KVPREAVB-BULK =                                      
210500           ART-KVPREAVB-BULK - MINSKAT-ANTAL                              
210600         COMPUTE ART-KVPRERO-BULK =                                       
210700           ART-KVPRERO-BULK - SPAR-ORAD-KVPRERO                           
210800       ELSE                                                               
210900         COMPUTE ART-KVPRERO-BULK =                                       
211000           ART-KVPRERO-BULK - MINSKAT-ANTAL                               
211100       END-IF                                                             
211200                                                                          
211300     END-IF                                                               
211400     .                                                                    
211500     EJECT                                                                
211600                                                                          
211700 JBBB-COMMAND-EJ-IFYLLD SECTION.                                          
211800                                                                          
211900     IF FIRST-TIME                                                        
212000        MOVE MID-IDARTNR-SPAR(INDX) TO W-ORAD-IDARTNR-UNIK                
212100        MOVE MID-IDLOPNR-SPAR(INDX) TO W-ORAD-IDLOPNR-UNIK                
212200        MOVE MID-ADLAGOMR-SPAR(INDX) TO W-ORAD-ADLAGOMR-UNIK              
212300        MOVE MID-ADGANG-SPAR(INDX) TO W-ORAD-ADGANG-UNIK                  
212400        MOVE MID-ADPLATS-SPAR(INDX) TO W-ORAD-ADPLATS-UNIK                
212500                                                                          
212600        PERFORM IMS-GHU-ORQF-ORQF01                                       
212700                                                                          
212800        IF SEGMENT-FINNS                                                  
212900          PERFORM S14-SPARA-ARTIKEL                                       
213000          MOVE NEJ TO FIRST-TIME-SW                                       
213100        END-IF                                                            
213200     END-IF                                                               
213300     ADD +1 TO INDX                                                       
213400     .                                                                    
213500     EJECT                                                                
213600                                                                          
213700 JBBC-FLYTTA-NEXT-ARTNR SECTION.                                          
213800                                                                          
213900     MOVE MID-IDARTNR-NEXT  TO W-ORAD-IDARTNR-MIN                         
214000     MOVE MID-IDARTNR-NEXT  TO W-ORAD-IDARTNR-MAX                         
214100     MOVE MID-IDARTNR-NEXT  TO W-ORAD-IDARTNR-MIN-MIN                     
214200     MOVE MID-IDORDER-NEXT  TO W-ORAD-IDORDER-MIN                         
214300     MOVE MID-IDORDER-NEXT  TO W-ORAD-IDORDER-MIN-MIN                     
214400     MOVE MID-IDORDER-NEXT  TO W-ORAD-IDORDER-MAX                         
214500     MOVE MID-IDORDER-NEXT  TO W-ORAD-IDORDER-MAX-MAX                     
214600     MOVE MID-IDDC-NEXT     TO W-ORAD-IDDC-MIN                            
214700     MOVE MID-IDDC-NEXT     TO W-ORAD-IDDC-MAX                            
214800     MOVE MID-IDDC-NEXT     TO W-ORAD-IDDC-MIN-MIN                        
214900     MOVE MID-IDDC-NEXT     TO W-ORAD-IDDC-MAX-MAX                        
215000     MOVE MID-ADLAGOMR-NEXT TO W-ORAD-ADLAGOMR-MIN                        
215100     MOVE MID-ADLAGOMR-NEXT TO W-ORAD-ADLAGOMR-MIN-MIN                    
215200     MOVE MID-ADGANG-NEXT   TO W-ORAD-ADGANG-MIN                          
215300     MOVE MID-ADGANG-NEXT   TO W-ORAD-ADGANG-MIN-MIN                      
215400     MOVE MID-ADPLATS-NEXT  TO W-ORAD-ADPLATS-MIN                         
215500     MOVE MID-ADPLATS-NEXT  TO W-ORAD-ADPLATS-MIN-MIN                     
215600     MOVE MID-IDLOPNR-NEXT  TO W-ORAD-IDLOPNR-MIN                         
215700     MOVE MID-IDLOPNR-NEXT  TO W-ORAD-IDLOPNR-MIN-MIN                     
215800     PERFORM IMS-GU-ORQF-ORQF01                                           
215900     IF SEGMENT-SAKNAS                                                    
216000       MOVE OHUV-IDORDER    TO W-ORAD-IDORDER-MIN                         
216100       MOVE OHUV-IDORDER    TO W-ORAD-IDORDER-MIN-MIN                     
216200       MOVE OHUV-IDORDER    TO W-ORAD-IDORDER-MAX                         
216300       MOVE OHUV-IDORDER    TO W-ORAD-IDORDER-MAX-MAX                     
216400       MOVE WS-IDDC         TO W-ORAD-IDDC-MIN                            
216500       MOVE WS-IDDC         TO W-ORAD-IDDC-MIN-MIN                        
216600       MOVE WS-IDDC         TO W-ORAD-IDDC-MAX                            
216700       MOVE WS-IDDC         TO W-ORAD-IDDC-MAX-MAX                        
216800       MOVE ZERO            TO W-ORAD-IDARTNR-MIN                         
216900       MOVE ZERO            TO W-ORAD-IDARTNR-MIN-MIN                     
217000       MOVE ZERO            TO W-ORAD-ADLAGOMR-MIN                        
217100       MOVE ZERO            TO W-ORAD-ADLAGOMR-MIN-MIN                    
217200       MOVE ZERO            TO W-ORAD-ADGANG-MIN                          
217300       MOVE ZERO            TO W-ORAD-ADGANG-MIN-MIN                      
217400       MOVE ZERO            TO W-ORAD-ADPLATS-MIN                         
217500       MOVE ZERO            TO W-ORAD-ADPLATS-MIN-MIN                     
217600       MOVE ZERO            TO W-ORAD-IDLOPNR-MIN                         
217700       MOVE ZERO            TO W-ORAD-IDLOPNR-MIN-MIN                     
217800     END-IF                                                               
217900     .                                                                    
218000     EJECT                                                                
218100                                                                          
218200 JBBD-AVSLUTA-OCH-LAES-IGEN SECTION.                                      
218300                                                                          
218400     IF AVSR-OK                                                           
218500                                                                          
218600       PERFORM S15-SKAPA-AVSO                                             
218700                                                                          
218800       CALL W413AVSO USING AVSO-W413AVSO                                  
218900       AVSO-WDE6-PCB AVSO-ORQA-PCB                                        
219000       AVSO-WDQ2-PCB AVSO-GMTB-PCB                                        
219100       AVSO-XXKA-PCB AVSO-4437-PCB AVSO-XXKE-PCB                          
219200       AVSO-XXKF-PCB AVSO-XXKG-PCB AVSO-XXKH-PCB                          
219300       AVSO-XXKI-PCB AVSO-XXKP-PCB AVSO-WDB2-PCB                          
219400       AVSO-WDB6-PCB                                                      
219500       USEA-PCB      TRAN-XXKB-PCB                                        
219600       ORDN-ORQL-PCB ORDN-PROC-PCB                                        
219700       ORDN-ORQI-PCB ORDN-WDQ3-PCB                                        
219800                                                                          
219900     END-IF                                                               
220000                                                                          
220100     PERFORM IMS-GHU-ORQI-ORQI01                                          
220200                                                                          
220300     IF SEGMENT-FINNS                                                     
220400                                                                          
220500       MOVE JA TO OHUV-FLKLAR                                             
220600       PERFORM IMS-REPL-ORQI01                                            
220700                                                                          
220800     END-IF                                                               
220900                                                                          
221000                                                                          
221100     MOVE ZERO                 TO  KVRADER-LOR-RAKNARE                    
221200                                   KVRADER-ODEL-RAKNARE                   
221300                                   KVRADER-UP-RAKNARE                     
221400                                   KVRADER-DIRL-RAKNARE                   
221500                                   TOTAL-KVRADER                          
221600                                   TOTAL-UTSKRIVNA                        
221700                                   MOJLIG-ANTAL                           
221800                                                                          
221900     PERFORM S01-RAEKNA-TOTALA-RADER                                      
222000                                                                          
222100     PERFORM S02-KOLLA-ARBETSTABELL                                       
222200     .                                                                    
222300     EJECT                                                                
222400                                                                          
222500 S01-RAEKNA-TOTALA-RADER SECTION.                                         
222600                                                                          
222700     PERFORM IMS-GU-ORQA-ORQA01                                           
222800     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
222900       ADD ODEL-KVRADER TO KVRADER-ODEL-RAKNARE                           
223000       IF ODEL-KDODELSTA = 'U' OR 'P'                                     
223100         ADD ODEL-KVRADER TO KVRADER-UP-RAKNARE                           
223200       END-IF                                                             
223300       PERFORM IMS-GN-ORQA-ORQA01                                         
223400     END-PERFORM                                                          
223500                                                                          
223600     PERFORM IMS-GNP-ORQI12-OKVAL                                         
223700     PERFORM UNTIL SEGMENT-SAKNAS                                         
223800       IF ARB-KDTRPKAT = 'B' OR 'C'                                       
223900         IF ARB-IDDC NOT = W-IDDC-B6                                      
224000            MOVE ARB-IDDC TO W-IDDC-B6                                    
224100            PERFORM IMS-GU-WDB601                                         
224200         END-IF                                                           
224300         IF DCS-CDC                                                       
224400           MOVE JA TO DIRLEV-KOLL                                         
224500         END-IF                                                           
224600                                                                          
224700         MOVE ARB-IDDC TO W-IDDC                                          
224800         PERFORM IMS-GNP-WDQ221                                           
224900         PERFORM UNTIL SEGMENT-SAKNAS                                     
225000           ADD LOR-KVRADER TO KVRADER-LOR-RAKNARE                         
225100           PERFORM IMS-GNP-WDQ221                                         
225200         END-PERFORM                                                      
225300       END-IF                                                             
225400       PERFORM IMS-GNP-ORQI12-OKVAL                                       
225500     END-PERFORM                                                          
225600                                                                          
225700     IF DIRLEV-KOLL = JA                                                  
225800       PERFORM IMS-GNP-ORQI-ORQI11-FIRST                                  
225900       PERFORM UNTIL SEGMENT-SAKNAS                                       
226000         ADD DIRL-KVRADER TO KVRADER-DIRL-RAKNARE                         
226100         PERFORM IMS-GNP-ORQI-ORQI11                                      
226200       END-PERFORM                                                        
226300     END-IF                                                               
226400     EJECT                                                                
226500                                                                          
226600     COMPUTE TOTAL-KVRADER = KVRADER-ODEL-RAKNARE +                       
226700                             KVRADER-LOR-RAKNARE  +                       
226800                             KVRADER-DIRL-RAKNARE                         
226900                                                                          
227000     COMPUTE MOJLIG-ANTAL = TOTAL-KVRADER - KVRADER-UP-RAKNARE            
227100     IF MOJLIG-ANTAL = ZERO                                               
227200       IF ARB-KDTRPKAT = 'B' OR 'C'                                       
227300         MOVE ERR-LINES-MISSING TO MED-IDMFSFEL                           
227400       ELSE                                                               
227500         IF TOTAL-KVRADER = ZERO                                          
227600           MOVE ERR-LINES-MISSING TO MED-IDMFSFEL                         
227700         ELSE                                                             
227800           MOVE ERR-LINES-WRITTEN TO MED-IDMFSFEL                         
227900         END-IF                                                           
228000       END-IF                                                             
228100       CALL WMEDKONV USING MED-WMEDAREA                                   
228200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
228300       PERFORM MFS-RENSA-FAELT-UT                                         
228400       MOVE MFS-RENSA-FAELT  TO MOD-KVRADER                               
228500                                MOD-KVORDRAD                              
228600       MOVE NEJ TO ALLT-SW                                                
228700     ELSE                                                                 
228800       MOVE MOJLIG-ANTAL   TO MOD-KVRADER                                 
228900       MOVE TOTAL-KVRADER  TO MOD-KVORDRAD                                
229000     END-IF                                                               
229100     .                                                                    
229200     EJECT                                                                
229300                                                                          
229400 S02-KOLLA-ARBETSTABELL SECTION.                                          
229500                                                                          
229600     MOVE WS-IDDC     TO W-IDDC                                           
229700     PERFORM IMS-GNP-ORQI-ORQI12                                          
229800                                                                          
229900     IF SEGMENT-FINNS                                                     
230000       IF ARB-KDTRPKAT = 'A'                                              
230100         PERFORM S02A-TRANSPORTKAT-A                                      
230200       ELSE                                                               
230300         PERFORM S02B-TRANSPORTKAT-B-C                                    
230400       END-IF                                                             
230500     END-IF                                                               
230600     .                                                                    
230700     EJECT                                                                
230800                                                                          
230900 S02A-TRANSPORTKAT-A SECTION.                                             
231000                                                                          
231100     MOVE JA TO STATUS-SW                                                 
231200     MOVE WS-IDDC         TO W-ODEL-IDDC-MIN                              
231300                             W-ODEL-IDDC-MAX                              
231400     PERFORM IMS-GU-ORQA-ORQA01                                           
231500                                                                          
231600     IF SEGMENT-FINNS                                                     
231700                                                                          
231800       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
231900         NOT STATUS-OK                                                    
232000                                                                          
232100         IF ODEL-KDODELSTA = 'R'                                          
232200           MOVE NEJ TO STATUS-SW                                          
232300         END-IF                                                           
232400                                                                          
232500         PERFORM IMS-GN-ORQA-ORQA01                                       
232600                                                                          
232700       END-PERFORM                                                        
232800       IF STATUS-OK                                                       
232900         IF MOJLIG-ANTAL > ZERO                                           
233000           MOVE ERR-LINES-MISSING-CL TO MED-IDMFSFEL                      
233100         ELSE                                                             
233200           MOVE ERR-LINES-MISSING    TO MED-IDMFSFEL                      
233300         END-IF                                                           
233400         CALL WMEDKONV USING MED-WMEDAREA                                 
233500         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
233600         PERFORM MFS-RENSA-FAELT-UT                                       
233700         MOVE NEJ TO ALLT-SW                                              
233800       END-IF                                                             
233900                                                                          
234000     ELSE                                                                 
234100       IF MOJLIG-ANTAL > ZERO                                             
234200         MOVE ERR-LINES-MISSING-CL TO MED-IDMFSFEL                        
234300       ELSE                                                               
234400         MOVE ERR-LINES-MISSING    TO MED-IDMFSFEL                        
234500       END-IF                                                             
234600       CALL WMEDKONV USING MED-WMEDAREA                                   
234700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
234800       PERFORM MFS-RENSA-FAELT-UT                                         
234900       MOVE NEJ TO ALLT-SW                                                
235000     END-IF                                                               
235100     .                                                                    
235200     EJECT                                                                
235300                                                                          
235400 S02B-TRANSPORTKAT-B-C SECTION.                                           
235500                                                                          
235600     MOVE JA TO STATUS-SW                                                 
235700     MOVE WS-IDDC         TO W-ODEL-IDDC-MIN                              
235800                             W-ODEL-IDDC-MAX                              
235900     PERFORM IMS-GU-ORQA-ORQA01                                           
236000                                                                          
236100     IF SEGMENT-FINNS                                                     
236200                                                                          
236300       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
236400         NOT STATUS-OK                                                    
236500                                                                          
236600         IF ODEL-KDODELSTA = 'R'                                          
236700           MOVE NEJ TO STATUS-SW                                          
236800         END-IF                                                           
236900                                                                          
237000         PERFORM IMS-GN-ORQA-ORQA01                                       
237100                                                                          
237200       END-PERFORM                                                        
237300       IF STATUS-OK                                                       
237400         IF MOJLIG-ANTAL > ZERO                                           
237500           MOVE ERR-LINES-MISSING-CL TO MED-IDMFSFEL                      
237600         ELSE                                                             
237700           MOVE ERR-LINES-MISSING    TO MED-IDMFSFEL                      
237800         END-IF                                                           
237900         CALL WMEDKONV USING MED-WMEDAREA                                 
238000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
238100         PERFORM MFS-RENSA-FAELT-UT                                       
238200         MOVE NEJ TO ALLT-SW                                              
238300       END-IF                                                             
238400                                                                          
238500     ELSE                                                                 
238600       PERFORM IMS-GNP-WDQ221                                             
238700       IF SEGMENT-SAKNAS                                                  
238800         MOVE NEJ TO DIRLEV-SW                                            
238900         PERFORM IMS-GNP-ORQI-ORQI11-FIRST                                
239000         PERFORM UNTIL SEGMENT-SAKNAS OR DIRLEV-RADER                     
239100             IF DIRL-KVRADER > ZERO                                       
239200               MOVE JA  TO DIRLEV-SW                                      
239300             ELSE                                                         
239400               PERFORM IMS-GNP-ORQI-ORQI11                                
239500             END-IF                                                       
239600         END-PERFORM                                                      
239700         IF NOT DIRLEV-RADER                                              
239800           IF MOJLIG-ANTAL > ZERO                                         
239900             MOVE ERR-LINES-MISSING-CL TO MED-IDMFSFEL                    
240000           ELSE                                                           
240100             MOVE ERR-LINES-MISSING TO MED-IDMFSFEL                       
240200           END-IF                                                         
240300           CALL WMEDKONV USING MED-WMEDAREA                               
240400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
240500           PERFORM MFS-RENSA-FAELT-UT                                     
240600           MOVE NEJ TO ALLT-SW                                            
240700         END-IF                                                           
240800       ELSE                                                               
240900         CONTINUE                                                         
241000       END-IF                                                             
241100     END-IF                                                               
241200     .                                                                    
241300     EJECT                                                                
241400                                                                          
241500 S04-TABORT-RADEN SECTION.                                                
241600                                                                          
241700     MOVE ORAD-KVBEART-Q         TO SPAR-ORAD-KVBEART-Q                   
241800                                    ANNULLERAT-ANTAL                      
241900     MOVE ORAD-KVPREAVB          TO SPAR-ORAD-KVPREAVB                    
242000     MOVE ORAD-KVPRERO           TO SPAR-ORAD-KVPRERO                     
242100     MOVE ORAD-IDARTNR           TO W-IDARTNR                             
242200                                                                          
242300     IF ORAD-IDKAMPRF > ZERO                                              
242400       MOVE ORAD-IDKAMPRF        TO W-KAMP-IDKAMPRF                       
242500       MOVE ORAD-IDDC            TO W-KAMP-IDDC                           
242600       MOVE ORAD-IDARTNR         TO W-KART-IDARTNR                        
242700       MOVE ORAD-IDDISTR         TO W-KMRK-IDDISTR-FOM                    
242800       MOVE ORAD-IDDISTR         TO W-KMRK-IDDISTR-TOM                    
242900       MOVE ORAD-IDKUNDNR        TO W-KMRK-IDKUNDNR-FOM                   
243000       MOVE ORAD-IDKUNDNR        TO W-KMRK-IDKUNDNR-TOM                   
243100       PERFORM S13-BACKA-KAMPANJ                                          
243200     END-IF                                                               
243300                                                                          
243400     IF OHUV-IDSYSTEM = 'W216' AND OHUV-FLORDSPE = JA                     
243500       CONTINUE                                                           
243600     ELSE                                                                 
243700       IF ORAD-IDLEVNR = SPACE                                            
243800         IF ORAD-IDDC NOT = W-IDDC-B6                                     
243900            MOVE ORAD-IDDC TO W-IDDC-B6                                   
244000            PERFORM IMS-GU-WDB601                                         
244100         END-IF                                                           
244200         IF DCS-CDC                                                       
244300           PERFORM IMS-GHU-ARTM-ARTM01                                    
244400           IF SEGMENT-FINNS                                               
244500             PERFORM S04A-BACKA-RAD-K9                                    
244600           END-IF                                                         
244700         ELSE                                                             
244800           IF OHUV-KDORDKL = +0                                           
244900              PERFORM S04B-BACKA-NYVORKO                                  
245000           END-IF                                                         
245100           MOVE WS-IDDC    TO W-IDDC                                      
245200           PERFORM IMS-GHU-ARTS-ARTS11                                    
245300           IF OHUV-KDORDKL = +0 OR +1                                     
245400             COMPUTE SLAG-KVOKS-DAG =                                     
245500                     SLAG-KVOKS-DAG - ANNULLERAT-ANTAL                    
245600           ELSE                                                           
245700             IF (OHUV-KDORDKL = +2 OR +3 OR +4)                           
245800             AND ORAD-KVOKS-PREL = ZERO                                   
245900               COMPUTE SLAG-KVOKS-BULK =                                  
246000                       SLAG-KVOKS-BULK - ANNULLERAT-ANTAL                 
246100             END-IF                                                       
246200           END-IF                                                         
246300************                                                              
246400           MOVE ORAD-IDDISTR TO TEST-IDDISTR                              
246500           IF DIST18-SCRAP-NDC                                            
246600             MOVE 'N'       TO SLAG-FLSKROT-BEORD                         
246700           END-IF                                                         
246800**************                                                            
246900           PERFORM IMS-REPL-ARTS                                          
247000         END-IF                                                           
247100       END-IF                                                             
247200       PERFORM S17-FIXA-REFILL-TRANSFER                                   
247300     END-IF                                                               
247400                                                                          
247500     PERFORM IMS-GET-ARTC11                                               
247600     MOVE    CLAG-KVQPACK-1 TO WS-KVQPACK-1                               
247700     MOVE    CLAG-TIDISPIN  TO WS-TIDISPIN                                
247800                                                                          
247900     PERFORM S10-SKAPA-AVSR                                               
248000     PERFORM S07-AVSR-ANROP                                               
248100     MOVE +0 TO WS-ANTOBKR                                                
248200     PERFORM S11-SKAPA-ORDERBEKR                                          
248300     PERFORM S12-SKAPA-TRANSAR                                            
248400     PERFORM S20-DELETE-PRICE-Q-LINE                                      
248500     PERFORM IMS-DLET-ORQF                                                
248600     .                                                                    
248700     EJECT                                                                
248800                                                                          
248900 S04A-BACKA-RAD-K9 SECTION.                                               
249000                                                                          
249100     IF OHUV-KDORDKL = ZERO                                               
249200                                                                          
249300       PERFORM S04B-BACKA-NYVORKO                                         
249400                                                                          
249500       IF ORAD-IDKAMPRF NOT > ZERO                                        
249600         COMPUTE ART-KVOKS-VOR =                                          
249700         ART-KVOKS-VOR - SPAR-ORAD-KVBEART-Q                              
249800       END-IF                                                             
249900       COMPUTE ART-KVPREAVB-VOR =                                         
250000         ART-KVPREAVB-VOR - SPAR-ORAD-KVPREAVB                            
250100     END-IF                                                               
250200                                                                          
250300     IF OHUV-KDORDKL = +1                                                 
250400       IF ORAD-IDKAMPRF NOT > ZERO                                        
250500         COMPUTE ART-KVOKS-DAG =                                          
250600         ART-KVOKS-DAG - SPAR-ORAD-KVBEART-Q                              
250700       END-IF                                                             
250800       COMPUTE ART-KVPRERO-DAG =                                          
250900         ART-KVPRERO-DAG - SPAR-ORAD-KVPRERO                              
251000       COMPUTE ART-KVPREAVB-DAG =                                         
251100         ART-KVPREAVB-DAG - SPAR-ORAD-KVPREAVB                            
251200     END-IF                                                               
251300                                                                          
251400     IF OHUV-KDORDKL = +2 OR +3 OR +4                                     
251500       IF ORAD-IDKAMPRF NOT > ZERO                                        
251600         COMPUTE ART-KVOKS-BULK =                                         
251700         ART-KVOKS-BULK - SPAR-ORAD-KVBEART-Q                             
251800       END-IF                                                             
251900       COMPUTE ART-KVPRERO-BULK =                                         
252000         ART-KVPRERO-BULK - SPAR-ORAD-KVPRERO                             
252100       COMPUTE ART-KVPREAVB-BULK =                                        
252200         ART-KVPREAVB-BULK - SPAR-ORAD-KVPREAVB                           
252300     END-IF                                                               
252400                                                                          
252500     PERFORM IMS-REPL-ARTM                                                
252600     .                                                                    
252700     EJECT                                                                
252800 S04B-BACKA-NYVORKO SECTION.                                              
252900                                                                          
253000     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
253100     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
253200     MOVE OHUV-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
253300                                    W-A601KY-MAX-IDDISTR                  
253400     MOVE OHUV-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
253500                                    W-A601KY-MAX-IDKUNDNR                 
253600     MOVE OHUV-IDKUNDRF          TO W-A601KY-MIN-IDKUNDRF                 
253700                                    W-A601KY-MAX-IDKUNDRF                 
253800     MOVE OHUV-TIREGDAT          TO W-A601KY-MIN-TIREGDAT                 
253900                                    W-A601KY-MAX-TIREGDAT                 
254000     MOVE ORAD-IDARTNR           TO W-A601KY-MIN-IDARTNR                  
254100                                    W-A601KY-MAX-IDARTNR                  
254200                                                                          
254300     PERFORM IMS-GHN-WDA6B                                                
254400     PERFORM UNTIL SEGMENT-SAKNAS                                         
254500                OR BASEN-SLUT                                             
254600                                                                          
254700         IF  VOR-KDVORATG > '1'                                           
254800         AND VOR-KDVORATG < '6'                                           
254900         AND VOR-KVPREAVB        = SPAR-ORAD-KVBEART-Q                    
255000             MOVE '8'            TO VOR-KDVORATG                          
255100             MOVE 83             TO VOR-KDORDBEK                          
255200             MOVE 0              TO VOR-KVPREAVB                          
255300             IF VOR-TIKLAR = ZERO                                         
255400                MOVE WS-TINUDAT  TO VOR-TIKLAR                            
255500                COMPUTE VOR-TIKLATID    = WS-TINUTID                      
255600                                        / 100                             
255700                END-COMPUTE                                               
255800             END-IF                                                       
255900             PERFORM IMS-REPL-WDA6B                                       
256000         END-IF                                                           
256100                                                                          
256200         PERFORM IMS-GHN-WDA6B                                            
256300     END-PERFORM                                                          
256400     .                                                                    
256500     EJECT                                                                
256600 S04C-DELBACKA-NYVORKO SECTION.                                           
256700                                                                          
256800     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
256900     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
257000     MOVE OHUV-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
257100                                    W-A601KY-MAX-IDDISTR                  
257200     MOVE OHUV-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
257300                                    W-A601KY-MAX-IDKUNDNR                 
257400     MOVE OHUV-IDKUNDRF          TO W-A601KY-MIN-IDKUNDRF                 
257500                                    W-A601KY-MAX-IDKUNDRF                 
257600     MOVE OHUV-TIREGDAT          TO W-A601KY-MIN-TIREGDAT                 
257700                                    W-A601KY-MAX-TIREGDAT                 
257800     MOVE ORAD-IDARTNR           TO W-A601KY-MIN-IDARTNR                  
257900                                    W-A601KY-MAX-IDARTNR                  
258000                                                                          
258100     PERFORM IMS-GHN-WDA6B                                                
258200     PERFORM UNTIL SEGMENT-SAKNAS                                         
258300                OR BASEN-SLUT                                             
258400                                                                          
258500         IF  VOR-KDVORATG > '1'                                           
258600         AND VOR-KDVORATG < '6'                                           
258700         AND VOR-KVPREAVB >= MINSKAT-ANTAL                                
258800             SUBTRACT MINSKAT-ANTAL FROM VOR-KVPREAVB                     
258900             IF  VOR-KVPREAVB = 0                                         
259000                 MOVE '8'           TO VOR-KDVORATG                       
259100                 MOVE 83            TO VOR-KDORDBEK                       
259200                 IF VOR-TIKLAR = ZERO                                     
259300                    MOVE WS-TINUDAT TO VOR-TIKLAR                         
259400                    COMPUTE VOR-TIKLATID = WS-TINUTID                     
259500                                           / 100                          
259600                    END-COMPUTE                                           
259700                 END-IF                                                   
259800             END-IF                                                       
259900             PERFORM IMS-REPL-WDA6B                                       
260000         END-IF                                                           
260100                                                                          
260200         PERFORM IMS-GHN-WDA6B                                            
260300     END-PERFORM                                                          
260400     .                                                                    
260500     EJECT                                                                
260600                                                                          
260700 S05-BACKA-BIPACKADE-RADER SECTION.                                       
260800                                                                          
260900     MOVE ORAD-KVBEART-Q         TO   ANNULLERAT-ANTAL                    
261000                                      SPAR-ORAD-KVBEART-Q                 
261100     MOVE ORAD-IDDISTR           TO   W-RAD-IDDISTR                       
261200     MOVE ORAD-IDKUNDNR          TO   W-RAD-IDKUNDNR                      
261300     MOVE ORAD-IDKUNDRF-RO (3:5) TO   W-RAD-IDKUNDRF                      
261400     MOVE ORAD-IDARTNR           TO   W-RAD-IDARTNR                       
261500                                      W-IDARTNR                           
261600     MOVE ORAD-IDLOPNR-RO        TO   W-RAD-IDLOPNR                       
261700                                                                          
261800     IF ORAD-IDKAMPRF > ZERO                                              
261900       MOVE ORAD-IDKAMPRF        TO W-KAMP-IDKAMPRF                       
262000       MOVE ORAD-IDDC            TO W-KAMP-IDDC                           
262100       MOVE ORAD-IDARTNR         TO W-KART-IDARTNR                        
262200       MOVE ORAD-IDDISTR         TO W-KMRK-IDDISTR-FOM                    
262300       MOVE ORAD-IDDISTR         TO W-KMRK-IDDISTR-TOM                    
262400       MOVE ORAD-IDKUNDNR        TO W-KMRK-IDKUNDNR-FOM                   
262500       MOVE ORAD-IDKUNDNR        TO W-KMRK-IDKUNDNR-TOM                   
262600       PERFORM S13-BACKA-KAMPANJ                                          
262700     END-IF                                                               
262800                                                                          
262900     IF OHUV-IDSYSTEM = 'W216' AND OHUV-FLORDSPE = JA                     
263000       CONTINUE                                                           
263100     ELSE                                                                 
263200       IF ORAD-IDLEVNR = SPACE                                            
263300         IF ORAD-IDDC NOT = W-IDDC-B6                                     
263400            MOVE ORAD-IDDC TO W-IDDC-B6                                   
263500            PERFORM IMS-GU-WDB601                                         
263600         END-IF                                                           
263700         IF DCS-CDC                                                       
263800           IF ORAD-TIRODAT = ZERO                                         
263900             PERFORM IMS-GHU-ARTM-ARTM01                                  
264000                                                                          
264100             IF SEGMENT-FINNS                                             
264200               PERFORM S05A-AENDRA-WDK901                                 
264300               PERFORM IMS-REPL-ARTM                                      
264400             END-IF                                                       
264500           END-IF                                                         
264600         END-IF                                                           
264700       END-IF                                                             
264800     END-IF                                                               
264900                                                                          
265000     PERFORM S05B-UPPDATERA-WDA5                                          
265100                                                                          
265200     PERFORM IMS-GET-ARTC11                                               
265300     MOVE    CLAG-KVQPACK-1 TO WS-KVQPACK-1                               
265400     MOVE    CLAG-TIDISPIN  TO WS-TIDISPIN                                
265500                                                                          
265600     PERFORM S10-SKAPA-AVSR                                               
265700     PERFORM S07-AVSR-ANROP                                               
265800     MOVE +0 TO WS-ANTOBKR                                                
265900     PERFORM S11-SKAPA-ORDERBEKR                                          
266000     PERFORM S12-SKAPA-TRANSAR                                            
266100                                                                          
266200     PERFORM IMS-GHNP-ORQI-ORQI12                                         
266300     IF SEGMENT-FINNS                                                     
266400       MOVE 0  TO ARB-KDROPACK                                            
266500       PERFORM IMS-REPL-ORQI12                                            
266600     END-IF                                                               
266700                                                                          
266800     PERFORM IMS-DLET-ORQF                                                
266900     .                                                                    
267000     EJECT                                                                
267100                                                                          
267200 S05A-AENDRA-WDK901 SECTION.                                              
267300                                                                          
267400     IF ORAD-KDORDKL = ZERO                                               
267500       IF ORAD-KVPREAVB > ZERO                                            
267600         COMPUTE ART-KVPREAVB-VOR =                                       
267700         ART-KVPREAVB-VOR - ORAD-KVPREAVB                                 
267800       END-IF                                                             
267900     END-IF                                                               
268000                                                                          
268100     IF ORAD-KDORDKL = +1                                                 
268200       IF ORAD-KVPREAVB > ZERO                                            
268300         COMPUTE ART-KVPREAVB-DAG =                                       
268400         ART-KVPREAVB-DAG - ORAD-KVPREAVB                                 
268500       END-IF                                                             
268600       IF ORAD-KVPRERO > ZERO                                             
268700          COMPUTE ART-KVPRERO-DAG =                                       
268800          ART-KVPRERO-DAG - ORAD-KVPRERO                                  
268900       END-IF                                                             
269000     END-IF                                                               
269100                                                                          
269200     IF ORAD-KDORDKL = +2 OR +3 OR +4                                     
269300       IF ORAD-KVPREAVB > ZERO                                            
269400          COMPUTE ART-KVPREAVB-BULK =                                     
269500          ART-KVPREAVB-BULK - ORAD-KVPREAVB                               
269600       END-IF                                                             
269700       IF ORAD-KVPRERO > ZERO                                             
269800          COMPUTE ART-KVPRERO-BULK =                                      
269900          ART-KVPRERO-BULK - ORAD-KVPRERO                                 
270000       END-IF                                                             
270100     END-IF                                                               
270200     .                                                                    
270300     EJECT                                                                
270400                                                                          
270500 S05B-UPPDATERA-WDA5 SECTION.                                             
270600                                                                          
270700     PERFORM IMS-GHU-ORDP-ORDP01                                          
270800                                                                          
270900     IF SEGMENT-FINNS                                                     
271000       IF RAD-KVART = ORAD-KVBEART-Q                                      
271100         MOVE '00000     '       TO   RAD-IDKUNDRF-LEV                    
271200         MOVE '3'                TO   RAD-KDSTARAD                        
271300         PERFORM IMS-REPL-ORDP                                            
271400       ELSE                                                               
271500         COMPUTE RAD-KVART = RAD-KVART - ORAD-KVBEART-Q                   
271600         PERFORM IMS-REPL-ORDP                                            
271700         MOVE ORAD-KVBEART-Q     TO RAD-KVART                             
271800         MOVE '00000     '       TO RAD-IDKUNDRF-LEV                      
271900         MOVE '3'                TO RAD-KDSTARAD                          
272000         ADD +1                  TO RAD-IDLOPNR                           
272100                                                                          
272200         MOVE OHUV-KDORDTYP-LDC  TO RAD-KDORDTYP-LDC                      
272300         MOVE OHUV-TIREPDAT      TO RAD-TIREPDAT                          
272400         MOVE ORAD-IDKUNDRF-WIP  TO RAD-IDKUNDRF-WIP                      
272500         PERFORM IMS-ISRT-ORDP                                            
272600         PERFORM UNTIL SEGMENT-FINNS                                      
272700           ADD +1                TO RAD-IDLOPNR                           
272800           PERFORM IMS-ISRT-ORDP                                          
272900         END-PERFORM                                                      
273000       END-IF                                                             
273100     END-IF                                                               
273200     .                                                                    
273300     EJECT                                                                
273400                                                                          
273500 S06-DELETE-PRARTNTO-AVSR SECTION.                                        
273600                                                                          
273700     MOVE ORAD-IDARTNR TO W-IDARTNR                                       
273800                                                                          
273900     PERFORM IMS-GET-ARTC11                                               
274000     MOVE    CLAG-KVQPACK-1 TO WS-KVQPACK-1                               
274100     MOVE    CLAG-TIDISPIN  TO WS-TIDISPIN                                
274200                                                                          
274300     PERFORM S10-SKAPA-AVSR                                               
274400     .                                                                    
274500     EJECT                                                                
274600                                                                          
274700 S07-AVSR-ANROP SECTION.                                                  
274800                                                                          
274900     CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT2-PCB                      
275000                         AVSR-ORQI-PCB AVSR-GMTB-PCB                      
275100                         AVSR-GMTC-PCB AVSR-WDB2-PCB                      
275200                         AVSR-WDB6-PCB TRAN-XXKB-PCB                      
275300     .                                                                    
275400     EJECT                                                                
275500                                                                          
275600 S08-AENDRA-PRARTNTO SECTION.                                             
275700                                                                          
275800     IF DIST79-DEALER-PRICE                                               
275900       MOVE WS-PRARTNTO(INDX) TO ORAD-PRARTNTO-LOC                        
276000       MOVE +0                TO ORAD-PRARTNTO-LOCPREL                    
276100     ELSE                                                                 
276300       IF DIST79-ECOM-PRICE                                               
276400          MOVE WS-PRARTNTO(INDX) TO ORAD-PRARTNTO-LOC                     
276500                                    ORAD-PRARTBTO-LOC                     
276600       ELSE                                                               
276700         MOVE WS-PRARTNTO(INDX)  TO ORAD-PRARTNTO                         
276800       END-IF                                                             
276900     END-IF                                                               
277000     MOVE 'P'                 TO ORAD-KDPRTYP                             
277100     MOVE MSGI-TILOKDAT       TO ORAD-TIPRIS                              
277200     MOVE 'N'                 TO ORAD-FLPRTILL                            
277300     .                                                                    
277400     EJECT                                                                
277500                                                                          
277600 S09-TILLAEGG-PRARTNTO-AVSR SECTION.                                      
277700                                                                          
277800     MOVE JA TO AVSR-SW                                                   
277900     MOVE +1                 TO   AVSR-KDCALL                             
278000     MOVE OHUV-IDORDER       TO   AVSR-IDORDER                            
278100     MOVE OHUV-KDORDKL       TO   AVSR-KDORDKL                            
278200     MOVE +0                 TO   AVSR-KDFRAKT                            
278300     MOVE ZERO               TO   AVSR-KDROPACK                           
278400     MOVE MSGI-TILOKDAT      TO   AVSR-TIREGDAT                           
278500     MOVE MSGI-TILOKTID      TO   AVSR-TIHHMM                             
278600     MOVE ORAD-ADLAGOMR      TO   AVSR-ADLAGOMR(AVSR-INDX)                
278700     MOVE ORAD-IDLEVNR       TO   AVSR-IDLEVNR(AVSR-INDX)                 
278800     MOVE ORAD-IDDC          TO   AVSR-IDDC(AVSR-INDX)                    
278900     MOVE ORAD-KDSPEEMB      TO   AVSR-KDSPEEMB(AVSR-INDX)                
279000     MOVE +0                 TO   AVSR-KVANNANT(AVSR-INDX)                
279100     MOVE ORAD-KVBEART-Q     TO   AVSR-KVBEART-Q(AVSR-INDX)               
279200     MOVE ORAD-PRARTNTO      TO   AVSR-PRARTNTO(AVSR-INDX)                
279300     MOVE ORAD-PRAVCOST      TO   AVSR-PRAVCOST(AVSR-INDX)                
279400     MOVE ORAD-DEAL-PR-LINE  TO   AVSR-DEAL-PR-LINE(AVSR-INDX)            
279500     MOVE ORAD-VKART         TO   AVSR-VKART(AVSR-INDX)                   
279600     MOVE ORAD-VLARTNTO      TO   AVSR-VLARTNTO(AVSR-INDX)                
279700     MOVE SPACE              TO   AVSR-KDORDSTA(AVSR-INDX)                
279800     MOVE +0                 TO   AVSR-KDVIA   (AVSR-INDX)                
279900     MOVE +0                 TO   AVSR-KVDAGAR-DIFF(AVSR-INDX)            
280000     MOVE +0                 TO   AVSR-TISKEPPN-DDC(AVSR-INDX)            
280100     .                                                                    
280200     EJECT                                                                
280300                                                                          
280400 S10-SKAPA-AVSR SECTION.                                                  
280500                                                                          
280600     MOVE JA TO AVSR-SW                                                   
280700     MOVE +2                 TO   AVSR-KDCALL                             
280800     MOVE OHUV-IDORDER       TO   AVSR-IDORDER                            
280900     MOVE OHUV-KDORDKL       TO   AVSR-KDORDKL                            
281000     MOVE +0                 TO   AVSR-KDFRAKT                            
281100     MOVE ZERO               TO   AVSR-KDROPACK                           
281200     MOVE ORAD-ADLAGOMR      TO   AVSR-ADLAGOMR(AVSR-INDX)                
281300     MOVE ORAD-IDLEVNR       TO   AVSR-IDLEVNR(AVSR-INDX)                 
281400     MOVE ORAD-IDDC          TO   AVSR-IDDC(AVSR-INDX)                    
281500     MOVE ORAD-KDSPEEMB      TO   AVSR-KDSPEEMB(AVSR-INDX)                
281600     MOVE ANNULLERAT-ANTAL   TO   AVSR-KVANNANT(AVSR-INDX)                
281700     MOVE ORAD-KVBEART-Q     TO   AVSR-KVBEART-Q(AVSR-INDX)               
281800     MOVE ORAD-PRARTNTO      TO   AVSR-PRARTNTO(AVSR-INDX)                
281900     MOVE ORAD-PRAVCOST      TO   AVSR-PRAVCOST(AVSR-INDX)                
282000     MOVE ORAD-DEAL-PR-LINE  TO   AVSR-DEAL-PR-LINE(AVSR-INDX)            
282100     MOVE ORAD-VKART         TO   AVSR-VKART(AVSR-INDX)                   
282200     MOVE ORAD-VLARTNTO      TO   AVSR-VLARTNTO(AVSR-INDX)                
282300     MOVE SPACE              TO   AVSR-KDORDSTA(AVSR-INDX)                
282400     MOVE +0                 TO   AVSR-KDVIA   (AVSR-INDX)                
282500     MOVE +0                 TO   AVSR-KVDAGAR-DIFF(AVSR-INDX)            
282600     MOVE +0                 TO   AVSR-TISKEPPN-DDC(AVSR-INDX)            
282700     .                                                                    
282800     EJECT                                                                
282900                                                                          
283000 S11-SKAPA-ORDERBEKR SECTION.                                             
283100                                                                          
283200     IF WS-ANTOBKR = +0                                                   
283300       MOVE OHUV-IDORDER         TO   W-OBKR-IDORDER-MIN                  
283400                                      W-OBKR-IDORDER-MAX                  
283500       MOVE ORAD-IDARTNR         TO   W-OBKR-IDARTNR-MIN                  
283600                                      W-OBKR-IDARTNR-MAX                  
283700       MOVE +1                   TO   W-OBKR-IDLOPNR-MIN                  
283800                                      W-OBKR-IDLOPNR-MAX                  
283900                                      W-OBKR-IDLOPNR-MAX                  
284000       MOVE +1                   TO   W-OBKR-IDSEKVNR-MIN                 
284100                                      W-OBKR-IDSEKVNR-MAX                 
284200       PERFORM IMS-GU-ORQM-ORQM01                                         
284300                                                                          
284400       PERFORM UNTIL SEGMENT-SAKNAS                                       
284500         ADD +1 TO W-OBKR-IDLOPNR-MIN                                     
284600                   W-OBKR-IDLOPNR-MAX                                     
284700         PERFORM IMS-GU-ORQM-ORQM01                                       
284800       END-PERFORM                                                        
284900                                                                          
285000     END-IF                                                               
285100     MOVE OHUV-IDORDER           TO   OBKR-IDORDER                        
285200     MOVE ORAD-IDARTNR           TO   OBKR-IDARTNR                        
285300     MOVE W-OBKR-IDLOPNR-MIN     TO   OBKR-IDLOPNR                        
285400     MOVE 1                      TO   OBKR-IDSEKVNR                       
285500     MOVE ORAD-IDDC              TO   OBKR-IDDC                           
285600     MOVE ORAD-IDDC-RO           TO   OBKR-IDDC-RO                        
285700     MOVE SPACE                  TO   OBKR-BEERS                          
285800     MOVE SPACE                  TO   OBKR-IDBIL                          
285900     MOVE OHUV-BEKUNDRF          TO   OBKR-BEKUNDRF                       
286000     MOVE ORAD-BERADREF          TO   OBKR-BERADREF                       
286100     MOVE ORAD-BEVOLREF          TO   OBKR-BEVOLREF                       
286200     MOVE ORAD-IDKAMPRF          TO   OBKR-IDKAMPRF                       
286300     MOVE ZERO                   TO   OBKR-DIERS-KVOT                     
286400     MOVE ORAD-FLAKPLOC          TO   OBKR-FLAKPLOC                       
286500     MOVE ORAD-FLINVEST          TO   OBKR-FLINVEST                       
286600     MOVE 'J'                    TO   OBKR-FLOBOK                         
286700     MOVE 'J'                    TO   OBKR-FLOBTRAN                       
286800     MOVE 'N'                    TO   OBKR-FLOBPRT                        
286900     MOVE ORAD-FLPRTILL          TO   OBKR-FLPRTILL                       
287000     MOVE ORAD-FLRESTN           TO   OBKR-FLRESTN                        
287100     MOVE JA                     TO   OBKR-FLSLATT                        
287200     MOVE ORAD-FLTILLK           TO   OBKR-FLTILLK                        
287300     MOVE ZERO                   TO   OBKR-IDARTNR-TILLK                  
287400     MOVE ORAD-IDDISTR           TO   OBKR-IDDISTR                        
287500     MOVE ORAD-IDKUNDNR          TO   OBKR-IDKUNDNR                       
287600     MOVE ORAD-IDKUNDRF          TO   OBKR-IDKUNDRF                       
287700     MOVE ORAD-IDKUNDRF-RO       TO   OBKR-IDKUNDRF-RO                    
287800     MOVE ORAD-IDLEVNR           TO   OBKR-IDLEVNR                        
287900     MOVE ORAD-IDLOPNR-RO        TO   OBKR-IDLOPNR-RO                     
288000     MOVE ORAD-IDSYSTEM          TO   OBKR-IDSYSTEM                       
288100     MOVE ORAD-KDDSP             TO   OBKR-KDDSP                          
288200     MOVE ZERO                   TO   OBKR-KDERS                          
288300     MOVE ORAD-KDKVBRYT          TO   OBKR-KDKVBRYT                       
288400     MOVE ORAD-KDOI              TO   OBKR-KDOI                           
288500     MOVE ORAD-CLEARGROUP        TO   OBKR-CLEARGROUP                     
288600     MOVE ORAD-KDPRTYP           TO   OBKR-KDPRTYP                        
288700     MOVE ORAD-KDTPOTYP          TO   OBKR-KDTPOTYP                       
288800     MOVE ORAD-KDVRINFO          TO   OBKR-KDVRINFO                       
288900     MOVE ZERO                   TO   OBKR-KVANNANT                       
289000     MOVE ZERO                   TO   OBKR-KVAVBART                       
289100     MOVE ORAD-KVBEART           TO   OBKR-KVBEART                        
289200     MOVE ORAD-KVBEART-Q         TO   OBKR-KVBEART-Q                      
289300     MOVE ZERO                   TO   OBKR-KVBEART-TILLK                  
289400     MOVE ZERO                   TO   OBKR-KVPREAVB                       
289500     MOVE ZERO                   TO   OBKR-KVPRERO                        
289600     MOVE WS-KVQPACK-1           TO   OBKR-KVQPACK                        
289700     MOVE ZERO                   TO   OBKR-KVRO                           
289800     MOVE ORAD-KVSLATT           TO   OBKR-KVSLATT                        
289900     MOVE ORAD-PRARTNTO          TO   OBKR-PRARTNTO                       
290000     MOVE ORAD-DEAL-PR-LINE      TO   OBKR-DEAL-PR-LINE                   
290100     MOVE ORAD-PRBPRIS           TO   OBKR-PRBPRIS                        
290200     MOVE ORAD-REKSIFFR          TO   OBKR-REKSIFFR                       
290300     MOVE ZERO                   TO   OBKR-REKSIFFR-TILLK                 
290400     MOVE ORAD-RERF-RAD          TO   OBKR-RERF-RAD                       
290500     MOVE ZERO                   TO   OBKR-TIDISPIN                       
290600     MOVE OHUV-TIREGDAT          TO   OBKR-TIORDREG                       
290700     MOVE ORAD-TIPRIS            TO   OBKR-TIPRIS                         
290800     MOVE MSGI-TILOKDAT          TO   OBKR-TIREGDAT                       
290900     MOVE MSGI-TILOKTID          TO   WS-TIHHMM                           
291000     MOVE WS-TIHHMMSS            TO   OBKR-TIREGTID                       
291100     MOVE ZERO                   TO   OBKR-TIRODAT                        
291200                                                                          
291300     MOVE OHUV-TIREGDAT          TO WS-AAMMDD                             
291400     IF WS-AAMMDD(1:2) < 50                                               
291500       MOVE 20                   TO WS-CENTURY                            
291600     ELSE                                                                 
291700       MOVE 19                   TO WS-CENTURY                            
291800     END-IF                                                               
291900     COMPUTE OBKR-TITIORDD-9KOMPL = WS-9KOMPL - WS-9KOMPL-DATUM           
292000                                                                          
292100     MOVE ORAD-TITPO             TO   OBKR-TITPO                          
292200                                                                          
292300     MOVE OBKR-TIREGDAT          TO WS-AAMMDD                             
292400     COMPUTE OBKR-TITIREGD-9KOMPL = WS-9KOMPL - WS-9KOMPL-DATUM           
292500                                                                          
292600     MOVE ARB-KDFRAKT            TO   OBKR-KDFRAKT                        
292700     MOVE OHUV-KDORDKL           TO   OBKR-KDORDKL                        
292800                                                                          
292900     IF ORAD-IDKUNDRF-RO = '0000000   ' OR                                
293000        ORAD-IDKUNDRF-RO = ORAD-IDKUNDRF                                  
293100       MOVE 83                   TO   OBKR-KDORDBEK                       
293200       MOVE ANNULLERAT-ANTAL     TO   OBKR-KVANNANT                       
293300     ELSE                                                                 
293400       MOVE 91                   TO   OBKR-KDORDBEK                       
293500       MOVE ANNULLERAT-ANTAL     TO   OBKR-KVRO                           
293600     END-IF                                                               
293700     MOVE IDPGM                  TO   OBKR-IDPGM                          
293800                                                                          
293900     MOVE OHUV-KDORDTYP-LDC      TO   OBKR-KDORDTYP-LDC                   
294000     MOVE OHUV-TIREPDAT          TO   OBKR-TIREPDAT                       
294100     MOVE ORAD-IDKUNDRF-WIP      TO   OBKR-IDKUNDRF-WIP                   
294200     MOVE ZERO                   TO   OBKR-TIDLEVDAT                      
294300     MOVE ORAD-PRAVCOST          TO   OBKR-PRAVCOST                       
294400     MOVE ORAD-KDVALISO          TO   OBKR-KDVALISO                       
294500                                                                          
294600     PERFORM IMS-ISRT-ORQM-ORQM01                                         
294700                                                                          
294800     PERFORM UNTIL SEGMENT-FINNS                                          
294900       ADD 1 TO OBKR-IDSEKVNR                                             
295000       PERFORM IMS-ISRT-ORQM-ORQM01                                       
295100     END-PERFORM                                                          
295200     ADD 1 TO WS-ANTOBKR                                                  
295300     .                                                                    
295400     EJECT                                                                
295500                                                                          
295600 S12-SKAPA-TRANSAR SECTION.                                               
295700                                                                          
295800     IF ORAD-IDKUNDRF-RO = '0000000   ' OR                                
295900       ORAD-IDKUNDRF-RO = ORAD-IDKUNDRF                                   
296000                                                                          
296100       IF ORAD-KDOI NOT = SPACE                                           
296200         PERFORM S12A-SKAPA-W2I109MID                                     
296300       END-IF                                                             
296400                                                                          
296500       PERFORM S12B-SKAPA-RYC                                             
296600                                                                          
296700     ELSE                                                                 
296800                                                                          
296900       PERFORM S12C-SKAPA-RYB                                             
297000                                                                          
297100     END-IF                                                               
297200     .                                                                    
297300     EJECT                                                                
297400                                                                          
297500 S12A-SKAPA-W2I109MID SECTION.                                            
297600                                                                          
297700*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
297800     MOVE ORAD-IDARTNR       TO BYT03-IDARTNR                             
297900     IF NOT BYT03-OBJEKT                                                  
298000                                                                          
298100        MOVE 2109-INDX          TO 2109-MID2-KVANTART                     
298200        MOVE ORAD-IDARTNR       TO 2109-MID2-IDARTNR (2109-INDX)          
298300        MOVE OHUV-IDDC-PRIM     TO 2109-MID2-IDDC(2109-INDX)              
298400        MOVE '-'                TO 2109-MID2-KDTECKEN(2109-INDX)          
298500        MOVE ORAD-KDOI          TO 2109-MID2-KDOI    (2109-INDX)          
298600        MOVE ORAD-CLEARGROUP    TO 2109-MID2-CLEARGROUP(2109-INDX)        
298700        MOVE ANNULLERAT-ANTAL   TO 2109-MID2-KVOI    (2109-INDX)          
298800        MOVE ORAD-TIREGDAT      TO 2109-MID2-TIUPPDAT(2109-INDX)          
298900                                                                          
299000        ADD +1                  TO 2109-INDX                              
299100        IF 2109-INDX > MAX-2109-INDX                                      
299200          PERFORM S16-STARTA-2109                                         
299300          MOVE ZERO TO 2109-MID2-KVANTART                                 
299400        END-IF                                                            
299500     END-IF                                                               
299600     .                                                                    
299700     EJECT                                                                
299800                                                                          
299900 S12B-SKAPA-RYC SECTION.                                                  
300000                                                                          
300100     MOVE 'RYC'               TO   W-RYC-IDPTYP                           
300200     MOVE ORAD-BERADREF       TO   W-RYC-BERADREF                         
300300     MOVE ORAD-BEVOLREF       TO   W-RYC-BEVOLREF                         
300400     MOVE ORAD-FLTILLK        TO   W-RYC-FLTILLK                          
300500     MOVE ORAD-IDARTNR        TO   W-RYC-IDARTNR                          
300600     MOVE ORAD-IDDISTR        TO   W-RYC-IDDISTR                          
300700     MOVE ORAD-IDKUNDNR       TO   W-RYC-IDKUNDNR                         
300800     MOVE ORAD-IDKUNDRF       TO   W-RYC-IDKUNDRF                         
300900     MOVE ORAD-IDKUNDRF-RO    TO   W-RYC-IDKUNDRF-RO                      
301000     MOVE ORAD-KDDSP          TO   W-RYC-KDDSP                            
301100     MOVE OHUV-KDFAKTYP       TO   W-RYC-KDFAKTYP                         
301200     MOVE ARB-KDFRAKT         TO   W-RYC-KDFRAKT                          
301300     MOVE 83                  TO   W-RYC-KDORDBEK                         
301400     MOVE OHUV-KDORDKL        TO   W-RYC-KDORDKL                          
301500     MOVE ORAD-KDKVBRYT       TO   W-RYC-KDKVBRYT                         
301600     MOVE ORAD-KDVRINFO       TO   W-RYC-KDVRINFO                         
301700     MOVE 0                   TO   W-RYC-KDVRTPO                          
301800     MOVE ANNULLERAT-ANTAL    TO   W-RYC-KVANNANT                         
301900     MOVE ORAD-REKSIFFR       TO   W-RYC-REKSIFFR                         
302000     MOVE OHUV-TIREGDAT       TO   W-RYC-TIORDREG                         
302100     MOVE ORAD-TIRODAT        TO   W-RYC-TIRODAT                          
302200     ACCEPT TIAAMMDD FROM DATE                                            
302300     ACCEPT TIKLOCK FROM TIME                                             
302400     MOVE +1 TO IDLOGLOP                                                  
302500     MOVE 'RYC' TO IDPTYP                                                 
302600     MOVE W-RYCPOST TO LOGGPOST                                           
302700                                                                          
302800     MOVE SPACE               TO W-RYCS-WDGZRYCS                          
302900     MOVE ORAD-IDDC           TO W-RYCS-IDDC                              
303000     MOVE W-RYCSPOST TO SORTPOST                                          
303100                                                                          
303200     PERFORM IMS-ISRT-ZZAC-ZZAC01                                         
303300     PERFORM UNTIL SEGMENT-FINNS                                          
303400       ADD +1 TO IDLOGLOP                                                 
303500       PERFORM IMS-ISRT-ZZAC-ZZAC01                                       
303600     END-PERFORM                                                          
303700     .                                                                    
303800     EJECT                                                                
303900                                                                          
304000 S12C-SKAPA-RYB SECTION.                                                  
304100                                                                          
304200     MOVE 'RYB'               TO   W-RYB-IDPTYP                           
304300     MOVE ORAD-BERADREF       TO   W-RYB-BERADREF                         
304400     MOVE ORAD-BEVOLREF       TO   W-RYB-BEVOLREF                         
304500     MOVE ORAD-FLTILLK        TO   W-RYB-FLTILLK                          
304600     MOVE ORAD-IDARTNR        TO   W-RYB-IDARTNR                          
304700     MOVE ORAD-IDDISTR        TO   W-RYB-IDDISTR                          
304800     MOVE ORAD-IDKUNDNR       TO   W-RYB-IDKUNDNR                         
304900     MOVE ORAD-IDKUNDRF       TO   W-RYB-IDKUNDRF                         
305000     MOVE ORAD-IDKUNDRF-RO    TO   W-RYB-IDKUNDRF-RO                      
305100     MOVE ORAD-IDDC           TO   W-RYB-IDDC                             
305200     MOVE ORAD-KDDSP          TO   W-RYB-KDDSP                            
305300     MOVE OHUV-KDFAKTYP       TO   W-RYB-KDFAKTYP                         
305400     MOVE ARB-KDFRAKT         TO   W-RYB-KDFRAKT                          
305500     MOVE +1                  TO   W-RYB-KDLIDEL                          
305600     MOVE 91                  TO   W-RYB-KDORDBEK                         
305700     MOVE ORAD-KDORDKL        TO   W-RYB-KDORDKL                          
305800     MOVE ORAD-KDKVBRYT       TO   W-RYB-KDKVBRYT                         
305900     MOVE +1                  TO   W-RYB-KDRO                             
306000     MOVE ORAD-KDVRINFO       TO   W-RYB-KDVRINFO                         
306100     MOVE ORAD-KVBEART-Q      TO   W-RYB-KVRO                             
306200     MOVE ORAD-REKSIFFR       TO   W-RYB-REKSIFFR                         
306300     MOVE WS-TIDISPIN         TO   W-RYB-TIDISPIN                         
306400     MOVE OHUV-TIREGDAT       TO   W-RYB-TIORDREG                         
306500     MOVE ORAD-TIRODAT        TO   W-RYB-TIRODAT                          
306600     ACCEPT TIAAMMDD FROM DATE                                            
306700     ACCEPT TIKLOCK FROM TIME                                             
306800     MOVE +1 TO IDLOGLOP                                                  
306900     MOVE 'RYB' TO IDPTYP                                                 
307000     MOVE W-RYBPOST TO LOGGPOST                                           
307100     MOVE SPACE TO SORTPOST                                               
307200                                                                          
307300     PERFORM IMS-ISRT-ZZAC-ZZAC01                                         
307400     PERFORM UNTIL SEGMENT-FINNS                                          
307500       ADD +1 TO IDLOGLOP                                                 
307600       PERFORM IMS-ISRT-ZZAC-ZZAC01                                       
307700     END-PERFORM                                                          
307800     .                                                                    
307900     EJECT                                                                
308000                                                                          
308100 S13-BACKA-KAMPANJ SECTION.                                               
308200                                                                          
308300     PERFORM IMS-GHU-WDM211                                               
308400     IF SEGMENT-FINNS                                                     
308500       COMPUTE KART-KVBEART-KUND = KART-KVBEART-KUND                      
308600                                 - ANNULLERAT-ANTAL                       
308700       IF KART-KVBEART-KUND < ZERO                                        
308800         MOVE 'WDM211 - ANTAL SALDO NEGATIV - ABEND' TO FELTEXT           
308900         CALL FELLOG USING RKOD-ABEND                                     
309000       ELSE                                                               
309100         PERFORM IMS-REPL-WDM211                                          
309200       END-IF                                                             
309300     ELSE                                                                 
309400       MOVE 'WDM211 SAKNAS - ABEND' TO FELTEXT                            
309500       CALL FELLOG USING RKOD-ABEND                                       
309600     END-IF                                                               
309700                                                                          
309800     PERFORM S20-FINN-INTERVALL                                           
309900     PERFORM IMS-GHU-WDM221                                               
310000     IF SEGMENT-FINNS                                                     
310100       COMPUTE KMRK-KVBEART-KUND = KMRK-KVBEART-KUND                      
310200                                 - ANNULLERAT-ANTAL                       
310300       IF KMRK-KVBEART-KUND < ZERO                                        
310400         MOVE 'WDM221 - ANTALSTABELL NEGATIV - ABEND' TO FELTEXT          
310500         CALL FELLOG USING RKOD-ABEND                                     
310600       ELSE                                                               
310700         PERFORM IMS-REPL-WDM221                                          
310800       END-IF                                                             
310900     END-IF                                                               
311000     .                                                                    
311100     EJECT                                                                
311200                                                                          
311300 S14-SPARA-ARTIKEL SECTION.                                               
311400                                                                          
311500     MOVE ORAD-IDARTNR  TO W-ORAD-IDARTNR-MIN                             
311600     MOVE ORAD-IDARTNR  TO W-ORAD-IDARTNR-MAX                             
311700     MOVE ORAD-IDARTNR  TO W-ORAD-IDARTNR-MIN-MIN                         
311800     MOVE ORAD-IDORDER  TO W-ORAD-IDORDER-MIN                             
311900     MOVE ORAD-IDORDER  TO W-ORAD-IDORDER-MIN-MIN                         
312000     MOVE ORAD-IDORDER  TO W-ORAD-IDORDER-MAX                             
312100     MOVE ORAD-IDORDER  TO W-ORAD-IDORDER-MAX-MAX                         
312200     MOVE ORAD-IDDC     TO W-ORAD-IDDC-MIN                                
312300     MOVE ORAD-IDDC     TO W-ORAD-IDDC-MAX                                
312400     MOVE ORAD-IDDC     TO W-ORAD-IDDC-MIN-MIN                            
312500     MOVE ORAD-IDDC     TO W-ORAD-IDDC-MAX-MAX                            
312600     MOVE ORAD-ADLAGOMR TO W-ORAD-ADLAGOMR-MIN                            
312700     MOVE ORAD-ADLAGOMR TO W-ORAD-ADLAGOMR-MIN-MIN                        
312800     MOVE ORAD-ADGANG   TO W-ORAD-ADGANG-MIN                              
312900     MOVE ORAD-ADGANG   TO W-ORAD-ADGANG-MIN-MIN                          
313000     MOVE ORAD-ADPLATS  TO W-ORAD-ADPLATS-MIN                             
313100     MOVE ORAD-ADPLATS  TO W-ORAD-ADPLATS-MIN-MIN                         
313200     MOVE ORAD-IDLOPNR  TO W-ORAD-IDLOPNR-MIN                             
313300     MOVE ORAD-IDLOPNR  TO W-ORAD-IDLOPNR-MIN-MIN                         
313400     .                                                                    
313500     EJECT                                                                
313600                                                                          
313700 S15-SKAPA-AVSO SECTION.                                                  
313800                                                                          
313900     MOVE WS-IDDISTR         TO   AVSO-IDDISTR                            
314000     MOVE WS-IDKUNDNR        TO   AVSO-IDKUNDNR                           
314100     MOVE WS-IDKUNDRF        TO   AVSO-IDKUNDRF                           
314200     MOVE WS-IDORDER         TO   AVSO-IDORDER                            
314300     MOVE SPACE              TO   AVSO-IDDC                               
314400     MOVE ZERO               TO   AVSO-TIRFS                              
314500     MOVE ZERO               TO   AVSO-TIAAMMDD                           
314600     MOVE ZERO               TO   AVSO-TIHHMM                             
314700     MOVE W-IDTRANS          TO   AVSO-IDTRANS                            
314800     .                                                                    
314900     EJECT                                                                
315000                                                                          
315100 S16-STARTA-2109 SECTION.                                                 
315200                                                                          
315300     COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                
315400                                                                          
315500     PERFORM IMS-PURG-ALT-MSG-2109                                        
315600                                                                          
315700     MOVE SPACE              TO 2109-MID2-W2I10902                        
315800     MOVE +1                 TO 2109-INDX                                 
315900     .                                                                    
316000     EJECT                                                                
316100 S17-FIXA-REFILL-TRANSFER SECTION.                                        
316200                                                                          
316300******************************************************************        
316400*                                                                         
316500*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
316600*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
316700*                                                                         
316800******************************************************************        
316900                                                                          
317000     MOVE ORAD-IDDISTR         TO W-TP4TRAN-IDDISTR                       
317100                                                                          
317200     PERFORM DB2-SELECT-TP4TRAN                                           
317300                                                                          
317400     MOVE ORAD-IDDISTR TO TEST-IDDISTR                                    
317500     IF DIST35-REFILL           OR                                        
317600        DIST35-REFILL-INOM-NDC  OR                                        
317700        DIST35-NONVCC-NONVCC-REFILL   OR                                  
317800        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
317900        DIST35-NA-TRANSFER      OR                                        
318000        DIST35-NA-NDC-RETURNS   OR                                        
318100        DIST35-PACIFIC-TRANSFER OR                                        
318200        DIST35-REFILL-INOM-JP   OR                                        
318300        DIST35-CN-TRANSFER      OR                                        
318400        DIST35-NONVCC-VCC-REFILL OR                                       
318500        DIST35-NONVCC-VCC-TRANSFER OR                                     
318600        RADER-FINNS                                                       
318700                                                                          
318800       IF RADER-FINNS                                                     
318900         MOVE TP4TRAN-IDDC-REC                                            
319000                             TO W-IDDC                                    
319100       ELSE                                                               
319200         SEARCH ALL DIST57-REFILL-DC                                      
319300           AT END                                                         
319400             MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                         
319500                               TO FELTEXT                                 
319600             CALL FELLOG                                                  
319700           WHEN DIST57-SOK-IDDISTR(DIST57-IX) = ORAD-IDDISTR              
319800             MOVE DIST57-REFILL-TO-DC(DIST57-IX)                          
319900                               TO W-IDDC                                  
320000         END-SEARCH                                                       
320100       END-IF                                                             
320200                                                                          
320300       PERFORM IMS-GHU-ARTS-ARTS11                                        
320400       COMPUTE SLAG-KVBEART = SLAG-KVBEART - ANNULLERAT-ANTAL             
320500       PERFORM IMS-REPL-ARTS                                              
320600                                                                          
320700       MOVE ORAD-IDARTNR   TO W-IDARTNR                                   
320800       PERFORM IMS-GHU-ARTC11                                             
320900       MOVE 1                TO IX-CD-OMR                                 
321000       PERFORM UNTIL IX-CD-OMR > 4                                        
321100       OR ORAD-ADLAGOMR = CLAG-ADLAGOMR-CD (IX-CD-OMR)                    
321200         ADD 1               TO IX-CD-OMR                                 
321300       END-PERFORM                                                        
321400       IF IX-CD-OMR <= 4                                                  
321500*                                                                         
321600*BOKA UPP CROSS DOCKING SALDO                                             
321700*                                                                         
321800          ADD ANNULLERAT-ANTAL                                            
321900                           TO CLAG-KVLS-CD (IX-CD-OMR)                    
322000       END-IF                                                             
322100       PERFORM IMS-REPL-ARTC                                              
322200     ELSE                                                                 
322300        IF DIST35-NONVCC-CDC-REFILL                                       
322400           SEARCH ALL DIST57-REFILL-DC                                    
322500            AT END                                                        
322600              MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                        
322700                               TO FELTEXT                                 
322800               CALL FELLOG                                                
322900            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = ORAD-IDDISTR             
323000              MOVE DIST57-REFILL-TO-DC(DIST57-IX)                         
323100                               TO W-IDDC                                  
323200           END-SEARCH                                                     
323300                                                                          
323400           PERFORM IMS-GHU-ARTC11                                         
323500           COMPUTE CLAG-KVBEART = CLAG-KVBEART - ANNULLERAT-ANTAL         
323600           PERFORM IMS-REPL-ARTC                                          
323700        END-IF                                                            
323800                                                                          
323900     END-IF                                                               
324000     .                                                                    
324100     EJECT                                                                
324200                                                                          
324300 S18-DATA-TILL-DEL-NOTE SECTION.                                          
324400                                                                          
324500     MOVE ORAD-IDDISTR             TO TEST-IDDISTR                        
324600     IF DIST07-USA-RETAILER-DNOTE                                         
324700     OR DIST07-CAN-RETAILER                                               
324800        INITIALIZE DNOT-ORDER-INFO                                        
324900                                                                          
325000        MOVE IDPGM                    TO DNOT-IDPGM                       
325100        MOVE OHUV-IDORDER             TO DNOT-IDORDER                     
325200        MOVE ORAD-IDARTNR             TO DNOT-IDARTNR                     
325300        MOVE WS-IDDC                  TO DNOT-IDDC                        
325400*OBS ANNULLERAT ANTAL LÄGGS I KVBEART                                     
325500        MOVE OBKR-KVANNANT            TO DNOT-KVBEART                     
325600        MOVE ORAD-KVBEART-Q           TO DNOT-KVBEART-Q                   
325700                                                                          
325800        CALL W411DNOT USING DNOT-W411DNOT                                 
325900                            DNOT-ORQP-PCB                                 
326000                            DNOT-ORQP2-PCB                                
326100                            DNOT-ORQP3-PCB                                
326200                            DNOT-4013-PCB                                 
326300                            DNOT-BENA-PCB                                 
326400     END-IF                                                               
326500     .                                                                    
326600     EJECT                                                                
326700                                                                          
326800 S20-FINN-INTERVALL SECTION.                                              
326900                                                                          
327000     PERFORM IMS-GU-WDM211                                                
327100     IF SEGMENT-FINNS                                                     
327200       PERFORM IMS-GNP-WDM221                                             
327300       PERFORM UNTIL SEGMENT-SAKNAS                                       
327400         IF  ORAD-IDDISTR > KMRK-IDDISTR-TOM                              
327500         OR  ORAD-IDDISTR < KMRK-IDDISTR-FOM                              
327600           CONTINUE                                                       
327700         ELSE                                                             
327800           IF  ORAD-IDDISTR  = KMRK-IDDISTR-TOM                           
327900           AND ORAD-IDKUNDNR > KMRK-IDKUNDNR-TOM                          
328000             CONTINUE                                                     
328100           ELSE                                                           
328200             IF  ORAD-IDDISTR  = KMRK-IDDISTR-FOM                         
328300             AND ORAD-IDKUNDNR < KMRK-IDKUNDNR-FOM                        
328400               CONTINUE                                                   
328500             ELSE                                                         
328600               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
328700               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
328800               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
328900               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
329000             END-IF                                                       
329100           END-IF                                                         
329200         END-IF                                                           
329300         PERFORM IMS-GNP-WDM221                                           
329400       END-PERFORM                                                        
329500     END-IF                                                               
329600     .                                                                    
329700     EJECT                                                                
329800                                                                          
329900 MFS-RENSA-FAELT-UT SECTION.                                              
330000                                                                          
330100*    --- ALLA UTDATA-FÄLT                                                 
330200     MOVE MFS-RENSA-FAELT TO MOD-IDORDER-ENTER                            
330300                             MOD-IDORDER-NEXT                             
330400                             MOD-IDDC-ENTER                               
330500                             MOD-IDDC-NEXT                                
330600                             MOD-ADLAGOMR-ENTER                           
330700                             MOD-ADLAGOMR-NEXT                            
330800                             MOD-ADGANG-ENTER                             
330900                             MOD-ADGANG-NEXT                              
331000                             MOD-ADPLATS-ENTER                            
331100                             MOD-ADPLATS-NEXT                             
331200                             MOD-IDARTNR-ENTER                            
331300                             MOD-IDARTNR-NEXT                             
331400                             MOD-IDLOPNR-ENTER                            
331500                             MOD-IDLOPNR-NEXT                             
331600     MOVE +1 TO INDX                                                      
331700     PERFORM UNTIL INDX > MAX-INDX                                        
331800       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-RAD(INDX)                      
331900                               MOD-KVBEART-Q-RAD(INDX)                    
332000                               MOD-PRARTNTO-RAD(INDX)                     
332100                               MOD-BEART-RAD(INDX)                        
332200                               MOD-OREF-RAD(INDX)                         
332300                               MOD-BERADREF-RAD(INDX)                     
332400                               MOD-IDDC-RAD(INDX)                         
332500                               MOD-IDARTNR-SPAR(INDX)                     
332600                               MOD-IDLOPNR-SPAR(INDX)                     
332700                               MOD-ADLAGOMR-SPAR(INDX)                    
332800                               MOD-ADGANG-SPAR(INDX)                      
332900                               MOD-ADPLATS-SPAR(INDX)                     
333000       ADD +1 TO INDX                                                     
333100     END-PERFORM                                                          
333200     .                                                                    
333300     EJECT                                                                
333400 MFS-RENSA-FAELT-IN SECTION.                                              
333500                                                                          
333600*    --- ALLA INDATA-FÄLT                                                 
333700     MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-UPDATE                            
333800     MOVE +1 TO INDX                                                      
333900     PERFORM UNTIL INDX > MAX-INDX                                        
334000       MOVE MFS-RENSA-FAELT TO MOD-CMD-UPDATE(INDX)                       
334100                               MOD-KVBEART-Q-UPDATE(INDX)                 
334200                               MOD-PRARTNTO-UPDATE(INDX)                  
334300                               MOD-BERADREF-UPDATE(INDX)                  
334400       ADD +1 TO INDX                                                     
334500     END-PERFORM                                                          
334600     .                                                                    
334700     EJECT                                                                
334800                                                                          
334900 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
335000                                                                          
335100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDORDER-ENTER                          
335200                               MOD-IDORDER-NEXT                           
335300                               MOD-IDDC-ENTER                             
335400                               MOD-IDDC-NEXT                              
335500                               MOD-ADLAGOMR-ENTER                         
335600                               MOD-ADLAGOMR-NEXT                          
335700                               MOD-ADGANG-ENTER                           
335800                               MOD-ADGANG-NEXT                            
335900                               MOD-ADPLATS-ENTER                          
336000                               MOD-ADPLATS-NEXT                           
336100                               MOD-IDARTNR-ENTER                          
336200                               MOD-IDARTNR-NEXT                           
336300                               MOD-IDLOPNR-ENTER                          
336400                               MOD-IDLOPNR-NEXT                           
336500                               MOD-KVORDRAD                               
336600                               MOD-KVRADER                                
336700     MOVE +1 TO INDX                                                      
336800     PERFORM UNTIL INDX > MAX-INDX                                        
336900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-RAD(INDX)                    
337000                                 MOD-KVBEART-Q-RAD(INDX)                  
337100                                 MOD-PRARTNTO-RAD(INDX)                   
337200                                 MOD-BEART-RAD(INDX)                      
337300                                 MOD-OREF-RAD(INDX)                       
337400                                 MOD-BERADREF-RAD(INDX)                   
337500                                 MOD-IDDC-RAD(INDX)                       
337600                                 MOD-IDARTNR-SPAR(INDX)                   
337700                                 MOD-IDLOPNR-SPAR(INDX)                   
337800                                 MOD-ADLAGOMR-SPAR(INDX)                  
337900                                 MOD-ADGANG-SPAR(INDX)                    
338000                                 MOD-ADPLATS-SPAR(INDX)                   
338100       ADD +1 TO INDX                                                     
338200     END-PERFORM                                                          
338300     .                                                                    
338400     EJECT                                                                
338500                                                                          
338600 S19-DELETE-PRICE-Q-ORDER SECTION.                                        
338700                                                                          
338800     IF DIST79-DEALER-PRICE                                               
338900       INITIALIZE PRQU-W335PRQU                                           
339000       MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                            
339100       MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                           
339200       MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                           
339300       MOVE 3                  TO PRQU-KDCALL                             
339400       CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                   
339500                                          PRQU-WDC7-PCB                   
339600                                          PRQU-SJKO-WDK6-PCB              
339700                                                                          
339800     END-IF                                                               
339900     .                                                                    
340000     EJECT                                                                
340100                                                                          
340200 S20-DELETE-PRICE-Q-LINE SECTION.                                         
340300                                                                          
340400     IF DIST79-DEALER-PRICE                                               
340500       IF ORAD-IDPRQUES > ZERO                                            
340600         INITIALIZE PRQU-W335PRQU                                         
340700         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
340800         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
340900         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
341000         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
341100         MOVE 4                  TO PRQU-KDCALL                           
341200         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
341300                                            PRQU-WDC7-PCB                 
341400                                            PRQU-SJKO-WDK6-PCB            
341500       END-IF                                                             
341600     END-IF                                                               
341700     .                                                                    
341800     EJECT                                                                
341900                                                                          
342000 S21-CHANGE-PRICE-Q-LINE SECTION.                                         
342100                                                                          
342200     IF DIST79-DEALER-PRICE                                               
342300       IF ORAD-IDPRQUES > ZERO                                            
342400         INITIALIZE PRQU-W335PRQU                                         
342500         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
342600         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
342700         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
342800         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
342900         MOVE WS-KVBEART-Q(INDX) TO PRQU-KVBEART-Q                        
343000         MOVE 5                  TO PRQU-KDCALL                           
343100         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
343200                                            PRQU-WDC7-PCB                 
343300                                            PRQU-SJKO-WDK6-PCB            
343400       END-IF                                                             
343500     END-IF                                                               
343600     .                                                                    
343700     EJECT                                                                
343800                                                                          
343900 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
344000                                                                          
344100*    --- ALLA INDATA-FÄLT                                                 
344200     MOVE MFS-ROER-EJ-FAELT TO MOD-FLAGGA-UPDATE                          
344300     MOVE +1 TO INDX                                                      
344400     PERFORM UNTIL INDX > MAX-INDX                                        
344500       MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-UPDATE(INDX)                     
344600                                 MOD-KVBEART-Q-UPDATE(INDX)               
344700                                 MOD-PRARTNTO-UPDATE(INDX)                
344800                                 MOD-BERADREF-UPDATE(INDX)                
344900       ADD +1 TO INDX                                                     
345000     END-PERFORM                                                          
345100     .                                                                    
345200     EJECT                                                                
345300                                                                          
345400 MFS-FORM-ATTR SECTION.                                                   
345500                                                                          
345600*    --- ALLA INDATA-FÄLT                                                 
345700     MOVE MFS-FORMATETS-ATTR TO MOD-FLAGGA-UPDATE-ATTR                    
345800     MOVE +1 TO INDX                                                      
345900     PERFORM UNTIL INDX > MAX-INDX                                        
346000       MOVE MFS-FORMATETS-ATTR TO MOD-CMD-UPDATE-ATTR(INDX)               
346100                                  MOD-KVBEART-Q-UPDATE-ATTR(INDX)         
346200                                  MOD-PRARTNTO-UPDATE-ATTR(INDX)          
346300                                  MOD-BERADREF-UPDATE-ATTR(INDX)          
346400       ADD +1 TO INDX                                                     
346500     END-PERFORM                                                          
346600     .                                                                    
346700     SKIP2                                                                
346800 MFS-LAS-IN-IGEN SECTION.                                                 
346900                                                                          
347000*    --- ALLA INDATA-FÄLT                                                 
347100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAGGA-UPDATE-ATTR                 
347200     MOVE +1 TO INDX                                                      
347300     PERFORM UNTIL INDX > MAX-INDX                                        
347400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-UPDATE-ATTR(INDX)            
347500                               MOD-KVBEART-Q-UPDATE-ATTR(INDX)            
347600                                MOD-PRARTNTO-UPDATE-ATTR(INDX)            
347700                                MOD-BERADREF-UPDATE-ATTR(INDX)            
347800       ADD +1 TO INDX                                                     
347900     END-PERFORM                                                          
348000     .                                                                    
348100     EJECT                                                                
348200                                                                          
348300* --- IMS SEKTIONER ---                                                   
348400     SKIP3                                                                
348500 IMS-GET-MSG SECTION.                                                     
348600                                                                          
348700     MOVE '  QC' TO GODK-STATUSKODER                                      
348800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
348900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
349000     PERFORM IMS-STATUSKONTROLL                                           
349100     .                                                                    
349200                                                                          
349300 IMS-INSERT-MSG SECTION.                                                  
349400                                                                          
349500     IF NOT ENGLISH-TEXT                                                  
349600       MOVE '0' TO MFS-KDHUVOMR                                           
349700     END-IF                                                               
349800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
349900     MOVE SPACE TO GODK-STATUSKODER                                       
350000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
350100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
350200     PERFORM IMS-STATUSKONTROLL                                           
350300     .                                                                    
350400                                                                          
350500 IMS-INSERT-ALTMSG SECTION.                                               
350600     MOVE LOW-VALUE TO ALT1-Z1 ALT1-Z2                                    
350700     MOVE SPACE TO GODK-STATUSKODER                                       
350800     CALL CBLTDLI USING ISRT ALT1-PCB W-PROG-TO-PROG-SW                   
350900     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
351000     PERFORM IMS-STATUSKONTROLL                                           
351100     .                                                                    
351200     SKIP2                                                                
351300 IMS-PURG-ALT-MSG-2109 SECTION.                                           
351400     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
351500     MOVE SPACE TO GODK-STATUSKODER                                       
351600     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-2                 
351700     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
351800     PERFORM IMS-STATUSKONTROLL                                           
351900     .                                                                    
352000     EJECT                                                                
352100                                                                          
352200 IMS-GET-ARTC11 SECTION.                                                  
352300                                                                          
352400     STRING  'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                        
352500              DELIMITED BY SIZE INTO    SSA1                              
352600     MOVE    'WLARTC11'           TO    SSA2                              
352700     MOVE    '  '                 TO    GODK-STATUSKODER                  
352800     CALL    CBLTDLI              USING GU               ARTC-PCB         
352900                                        DLI-IO-AREA-K611                  
353000                                        SSA1             SSA2             
353100     MOVE    ARTC-STATUS-CODE     TO    STATUS-WS                         
353200     PERFORM IMS-STATUSKONTROLL                                           
353300     .                                                                    
353400     SKIP2                                                                
353500                                                                          
353600 IMS-GHU-ARTC11 SECTION.                                                  
353700                                                                          
353800     STRING  'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                        
353900              DELIMITED BY SIZE INTO    SSA1                              
354000     MOVE    'WLARTC11'           TO    SSA2                              
354100     MOVE    '  '                 TO    GODK-STATUSKODER                  
354200     CALL    CBLTDLI              USING GHU              ARTC-PCB         
354300                                        DLI-IO-AREA-K611                  
354400                                        SSA1             SSA2             
354500     MOVE    ARTC-STATUS-CODE     TO    STATUS-WS                         
354600     PERFORM IMS-STATUSKONTROLL                                           
354700     .                                                                    
354800     SKIP2                                                                
354900                                                                          
355000 IMS-REPL-ARTC       SECTION.                                             
355100     MOVE '  '   TO GODK-STATUSKODER                                      
355200     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-K611                    
355300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
355400     PERFORM IMS-STATUSKONTROLL                                           
355500     .                                                                    
355600     SKIP2                                                                
355700                                                                          
355800 IMS-GHU-ARTM-ARTM01 SECTION.                                             
355900                                                                          
356000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
356100          DELIMITED BY SIZE INTO SSA1                                     
356200     MOVE '  GE' TO GODK-STATUSKODER                                      
356300     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ARTM SSA1                
356400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
356500     PERFORM IMS-STATUSKONTROLL                                           
356600     .                                                                    
356700                                                                          
356800 IMS-REPL-ARTM SECTION.                                                   
356900                                                                          
357000     MOVE '  ' TO GODK-STATUSKODER                                        
357100     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ARTM                    
357200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
357300     PERFORM IMS-STATUSKONTROLL                                           
357400     .                                                                    
357500     EJECT                                                                
357600 IMS-GHU-ARTS-ARTS11 SECTION.                                             
357700                                                                          
357800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
357900          DELIMITED BY SIZE INTO SSA1                                     
358000     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
358100          DELIMITED BY SIZE INTO SSA2                                     
358200     MOVE '    ' TO GODK-STATUSKODER                                      
358300     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-AREA-K711 SSA1 SSA2           
358400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
358500     PERFORM IMS-STATUSKONTROLL                                           
358600     .                                                                    
358700                                                                          
358800 IMS-REPL-ARTS SECTION.                                                   
358900                                                                          
359000     MOVE '  ' TO GODK-STATUSKODER                                        
359100     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA-K711                    
359200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
359300     PERFORM IMS-STATUSKONTROLL                                           
359400     .                                                                    
359500     EJECT                                                                
359600                                                                          
359700 IMS-GU-BENA-BENA11 SECTION.                                              
359800                                                                          
359900     STRING 'WLBENA01(WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
360000          DELIMITED BY SIZE INTO SSA1                                     
360100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
360200          DELIMITED BY SIZE INTO SSA2                                     
360300     MOVE '  GE' TO GODK-STATUSKODER                                      
360400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA SSA1 SSA2            
360500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
360600     PERFORM IMS-STATUSKONTROLL                                           
360700     .                                                                    
360800     EJECT                                                                
360900 IMS-GHU-ORDP-ORDP01 SECTION.                                             
361000                                                                          
361100     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
361200          DELIMITED BY SIZE INTO SSA1                                     
361300     MOVE '  GE' TO GODK-STATUSKODER                                      
361400     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA-ORDP SSA1                
361500     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
361600     PERFORM IMS-STATUSKONTROLL                                           
361700     .                                                                    
361800                                                                          
361900 IMS-REPL-ORDP SECTION.                                                   
362000                                                                          
362100     MOVE '  ' TO GODK-STATUSKODER                                        
362200     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-AREA-ORDP                    
362300     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
362400     PERFORM IMS-STATUSKONTROLL                                           
362500     .                                                                    
362600                                                                          
362700 IMS-ISRT-ORDP SECTION.                                                   
362800                                                                          
362900     MOVE 'WLORDP01 ' TO SSA1                                             
363000     MOVE '  II' TO GODK-STATUSKODER                                      
363100     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA-ORDP SSA1               
363200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
363300     PERFORM IMS-STATUSKONTROLL                                           
363400     .                                                                    
363500     EJECT                                                                
363600 IMS-GU-ORQA-ORQA01 SECTION.                                              
363700                                                                          
363800     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
363900                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
364000          DELIMITED BY SIZE INTO SSA1                                     
364100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
364200     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
364300     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
364400     PERFORM IMS-STATUSKONTROLL                                           
364500     .                                                                    
364600                                                                          
364700 IMS-GN-ORQA-ORQA01 SECTION.                                              
364800                                                                          
364900     STRING 'WLORQA01(WDQ301KY >' W-WDQ301KY-MIN-X                        
365000                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
365100          DELIMITED BY SIZE INTO SSA1                                     
365200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
365300     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
365400     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
365500     PERFORM IMS-STATUSKONTROLL                                           
365600     .                                                                    
365700     EJECT                                                                
365800 IMS-GU-ORQF-ORQF01 SECTION.                                              
365900                                                                          
366000     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-MIN-X                    
366100                    '&WDQ401KY<=' W-WDQ401KY-MAX-MAX-X ')'                
366200          DELIMITED BY SIZE INTO SSA1                                     
366300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
366400     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA-ORAD SSA1                 
366500     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
366600     PERFORM IMS-STATUSKONTROLL                                           
366700     .                                                                    
366800                                                                          
366900 IMS-GU-ORQF-ORQF01-M-IDARTNR SECTION.                                    
367000                                                                          
367100     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-X                        
367200                    '&WDQ401KY<=' W-WDQ401KY-MAX-X                        
367300                    '&IDARTNR  =' W-ORAD-IDARTNR-MIN-X                    
367400                    '&IDLOPNR >=' W-ORAD-IDLOPNR-MIN-X ')'                
367500          DELIMITED BY SIZE INTO SSA1                                     
367600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
367700     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA-ORAD SSA1                 
367800     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
367900     PERFORM IMS-STATUSKONTROLL                                           
368000     .                                                                    
368100                                                                          
368200 IMS-GN-ORQF-ORQF01 SECTION.                                              
368300                                                                          
368400     STRING 'WLORQF01(WDQ401KY >' W-WDQ401KY-MIN-MIN-X                    
368500                    '&WDQ401KY<=' W-WDQ401KY-MAX-MAX-X ')'                
368600          DELIMITED BY SIZE INTO SSA1                                     
368700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
368800     CALL CBLTDLI USING GN ORQF-PCB DLI-IO-AREA-ORAD SSA1                 
368900     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
369000     PERFORM IMS-STATUSKONTROLL                                           
369100     .                                                                    
369200     EJECT                                                                
369300 IMS-GN-ORQF-ORQF01-M-IDARTNR SECTION.                                    
369400                                                                          
369500     STRING 'WLORQF01(WDQ401KY >' W-WDQ401KY-MIN-X                        
369600                    '&WDQ401KY<=' W-WDQ401KY-MAX-X                        
369700                    '&IDARTNR  =' W-ORAD-IDARTNR-MIN-X                    
369800                    '&IDLOPNR >=' W-ORAD-IDLOPNR-MIN-X ')'                
369900          DELIMITED BY SIZE INTO SSA1                                     
370000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
370100     CALL CBLTDLI USING GN ORQF-PCB DLI-IO-AREA-ORAD SSA1                 
370200     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
370300     PERFORM IMS-STATUSKONTROLL                                           
370400     .                                                                    
370500                                                                          
370600 IMS-GHU-ORQF-ORQF01 SECTION.                                             
370700                                                                          
370800     STRING 'WLORQF01(WDQ401KY =' W-WDQ401KY-UNIK-X ')'                   
370900          DELIMITED BY SIZE INTO SSA1                                     
371000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
371100     CALL CBLTDLI USING GHU ORQF-PCB DLI-IO-AREA-ORAD SSA1                
371200     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
371300     PERFORM IMS-STATUSKONTROLL                                           
371400     .                                                                    
371500                                                                          
371600 IMS-REPL-ORQF SECTION.                                                   
371700                                                                          
371800     MOVE '  ' TO GODK-STATUSKODER                                        
371900     CALL CBLTDLI USING REPL ORQF-PCB DLI-IO-AREA-ORAD                    
372000     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
372100     PERFORM IMS-STATUSKONTROLL                                           
372200     .                                                                    
372300     EJECT                                                                
372400 IMS-DLET-ORQF SECTION.                                                   
372500                                                                          
372600     MOVE '  ' TO GODK-STATUSKODER                                        
372700     CALL CBLTDLI USING DLET ORQF-PCB DLI-IO-AREA-ORAD                    
372800     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
372900     PERFORM IMS-STATUSKONTROLL                                           
373000     .                                                                    
373100   EJECT                                                                  
373200 IMS-GU-ORQI-ORQI01 SECTION.                                              
373300                                                                          
373400     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
373500          DELIMITED BY SIZE INTO SSA1                                     
373600     MOVE '  GE' TO GODK-STATUSKODER                                      
373700     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV SSA1                 
373800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
373900     PERFORM IMS-STATUSKONTROLL                                           
374000     .                                                                    
374100                                                                          
374200 IMS-GHU-ORQI-ORQI01 SECTION.                                             
374300                                                                          
374400     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
374500          DELIMITED BY SIZE INTO SSA1                                     
374600     MOVE '  GE' TO GODK-STATUSKODER                                      
374700     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA-OHUV SSA1                
374800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
374900     PERFORM IMS-STATUSKONTROLL                                           
375000     .                                                                    
375100     EJECT                                                                
375200 IMS-GHNP-ORQI-ORQI12 SECTION.                                            
375300                                                                          
375400     STRING 'WLORQI12*F(IDDC     =' W-IDDC-X ')'                          
375500          DELIMITED BY SIZE INTO SSA1                                     
375600     MOVE '  GE' TO GODK-STATUSKODER                                      
375700     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-AREA-ARB SSA1                
375800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
375900     PERFORM IMS-STATUSKONTROLL                                           
376000     .                                                                    
376100                                                                          
376200 IMS-GNP-ORQI-ORQI12 SECTION.                                             
376300                                                                          
376400     STRING 'WLORQI12*F(IDDC     =' W-IDDC-X ')'                          
376500          DELIMITED BY SIZE INTO SSA1                                     
376600     MOVE '  GE' TO GODK-STATUSKODER                                      
376700     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
376800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
376900     PERFORM IMS-STATUSKONTROLL                                           
377000     .                                                                    
377100                                                                          
377200 IMS-GNP-ORQI12-OKVAL SECTION.                                            
377300                                                                          
377400     MOVE   'WLORQI12' TO SSA1                                            
377500     MOVE '  GE' TO GODK-STATUSKODER                                      
377600     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
377700     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
377800     PERFORM IMS-STATUSKONTROLL                                           
377900     .                                                                    
378000     EJECT                                                                
378100                                                                          
378200 IMS-GNP-WDQ221 SECTION.                                                  
378300                                                                          
378400     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
378500          DELIMITED BY SIZE INTO SSA1                                     
378600     MOVE 'WLORQI21 '         TO SSA2                                     
378700     MOVE '  GE' TO GODK-STATUSKODER                                      
378800     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-Q221 SSA1 SSA2                
378900     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
379000     PERFORM IMS-STATUSKONTROLL                                           
379100     .                                                                    
379200                                                                          
379300 IMS-GNP-ORQI-ORQI11-FIRST SECTION.                                       
379400                                                                          
379500     MOVE 'WLORQI11*F' TO SSA1                                            
379600     MOVE '  GE' TO GODK-STATUSKODER                                      
379700     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-DIRL SSA1                
379800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
379900     PERFORM IMS-STATUSKONTROLL                                           
380000     .                                                                    
380100     SKIP2                                                                
380200 IMS-GNP-ORQI-ORQI11 SECTION.                                             
380300                                                                          
380400     MOVE 'WLORQI11 ' TO SSA1                                             
380500     MOVE '  GE' TO GODK-STATUSKODER                                      
380600     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-DIRL SSA1                
380700     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
380800     PERFORM IMS-STATUSKONTROLL                                           
380900     .                                                                    
381000                                                                          
381100 IMS-REPL-ORQI01 SECTION.                                                 
381200                                                                          
381300     MOVE '  ' TO GODK-STATUSKODER                                        
381400     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-OHUV                    
381500     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
381600     PERFORM IMS-STATUSKONTROLL                                           
381700     .                                                                    
381800                                                                          
381900 IMS-REPL-ORQI12 SECTION.                                                 
382000                                                                          
382100     MOVE '  ' TO GODK-STATUSKODER                                        
382200     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-ARB                     
382300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
382400     PERFORM IMS-STATUSKONTROLL                                           
382500     .                                                                    
382600     EJECT                                                                
382700 IMS-GU-ORQM-ORQM01 SECTION.                                              
382800                                                                          
382900     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
383000                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
383100          DELIMITED BY SIZE INTO SSA1                                     
383200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
383300     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA-OBKR SSA1                 
383400     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
383500     PERFORM IMS-STATUSKONTROLL                                           
383600     .                                                                    
383700                                                                          
383800 IMS-ISRT-ORQM-ORQM01 SECTION.                                            
383900                                                                          
384000     MOVE 'WLORQM01 ' TO SSA1                                             
384100     MOVE '  II' TO GODK-STATUSKODER                                      
384200     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-OBKR SSA1               
384300     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
384400     PERFORM IMS-STATUSKONTROLL                                           
384500     .                                                                    
384600     EJECT                                                                
384700 IMS-ISRT-ZZAC-ZZAC01 SECTION.                                            
384800                                                                          
384900     MOVE 'WLZZAC01 ' TO SSA1                                             
385000     MOVE '  II' TO GODK-STATUSKODER                                      
385100     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA-ZZAC SSA1               
385200     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
385300     PERFORM IMS-STATUSKONTROLL                                           
385400     .                                                                    
385500     EJECT                                                                
385600                                                                          
385700 IMS-GU-WDM211 SECTION.                                                   
385800                                                                          
385900     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
386000          DELIMITED BY SIZE INTO SSA1                                     
386100     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
386200          DELIMITED BY SIZE INTO SSA2                                     
386300     MOVE '  GE'              TO GODK-STATUSKODER                         
386400     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
386500     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
386600     PERFORM IMS-STATUSKONTROLL                                           
386700     .                                                                    
386800                                                                          
386900 IMS-GNP-WDM221 SECTION.                                                  
387000                                                                          
387100     MOVE 'WDM221 '           TO SSA1                                     
387200     MOVE '    GE'            TO GODK-STATUSKODER                         
387300     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
387400     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
387500     PERFORM IMS-STATUSKONTROLL                                           
387600     .                                                                    
387700                                                                          
387800 IMS-GHU-WDM211 SECTION.                                                  
387900                                                                          
388000     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
388100          DELIMITED BY SIZE INTO SSA1                                     
388200     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
388300          DELIMITED BY SIZE INTO SSA2                                     
388400     MOVE '  GE'              TO GODK-STATUSKODER                         
388500     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
388600     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
388700     PERFORM IMS-STATUSKONTROLL                                           
388800     .                                                                    
388900                                                                          
389000 IMS-REPL-WDM211 SECTION.                                                 
389100                                                                          
389200     MOVE '  '             TO GODK-STATUSKODER                            
389300     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
389400     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
389500     PERFORM IMS-STATUSKONTROLL                                           
389600     .                                                                    
389700     EJECT                                                                
389800 IMS-GHU-WDM221 SECTION.                                                  
389900                                                                          
390000     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
390100          DELIMITED BY SIZE INTO SSA1                                     
390200     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
390300          DELIMITED BY SIZE INTO SSA2                                     
390400     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
390500          DELIMITED BY SIZE INTO SSA3                                     
390600     MOVE '  GE' TO GODK-STATUSKODER                                      
390700     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
390800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
390900     PERFORM IMS-STATUSKONTROLL                                           
391000     .                                                                    
391100                                                                          
391200 IMS-REPL-WDM221 SECTION.                                                 
391300                                                                          
391400     MOVE '  '             TO GODK-STATUSKODER                            
391500     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
391600     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
391700     PERFORM IMS-STATUSKONTROLL                                           
391800     .                                                                    
391900     EJECT                                                                
392000 IMS-GHN-WDA6B SECTION.                                                   
392100     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
392200                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
392300            DELIMITED BY SIZE INTO SSA1                                   
392400     MOVE '  GEGB'               TO GODK-STATUSKODER                      
392500     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA-WDA6 SSA1           
392600     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
392700     PERFORM IMS-STATUSKONTROLL                                           
392800     .                                                                    
392900 IMS-REPL-WDA6B SECTION.                                                  
393000     MOVE 'WDA601  '           TO SSA1                                    
393100     MOVE '    '               TO GODK-STATUSKODER                        
393200     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA-WDA6 SSA1            
393300     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
393400     PERFORM IMS-STATUSKONTROLL                                           
393500     .                                                                    
393600     EJECT                                                                
393700 IMS-GU-WDB601    SECTION.                                                
393800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
393900          DELIMITED BY SIZE INTO SSA1                                     
394000     MOVE '  GE' TO GODK-STATUSKODER                                      
394100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
394200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
394300     PERFORM IMS-STATUSKONTROLL                                           
394400     IF SEGMENT-SAKNAS                                                    
394500         MOVE SPACE TO DCS-KDDC                                           
394600     END-IF                                                               
394700     .                                                                    
394800     EJECT                                                                
394900 DB2-SELECT-TP4TRAN     SECTION.                                          
395000     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
395100                                                                          
395200     MOVE 000100 TO GODK-SQLCODEKODER                                     
395300                                                                          
395400     EXEC SQL                                                             
395500           SELECT  DISTINCT                                               
395600                   IDDC_REC                                               
395700                                                                          
395800           INTO   :TP4TRAN-IDDC-REC                                       
395900                                                                          
396000           FROM    TP4TRAN                                                
396100                                                                          
396200           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
396300     END-EXEC                                                             
396400                                                                          
396500     MOVE SQLCODE TO SQLCODE-WS                                           
396600     PERFORM DB2-STATUSKONTROLL                                           
396700     .                                                                    
396800     EJECT                                                                
396900                                                                          
397000 IMS-STATUSKONTROLL SECTION.                                              
397100                                                                          
397200     SET STATUS-IX TO 1                                                   
397300     SEARCH GODK-STATUS                                                   
397400       AT END CALL FELLOG                                                 
397500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
397600     END-SEARCH                                                           
397700     .                                                                    
397800 DB2-STATUSKONTROLL  SECTION.                                             
397900                                                                          
398000     SET SQLCODE-IX TO 1                                                  
398100     SEARCH GODK-SQLCODE                                                  
398200       AT END                                                             
398300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
398400          DELIMITED BY SIZE INTO FELTEXT                                  
398500          CALL ABEND USING RKOD-ABEND-DB2                                 
398600       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
398700     END-SEARCH                                                           
398800     .                                                                    
