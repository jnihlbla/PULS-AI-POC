000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W4024500.                                                
000500 AUTHOR.         GERRY CARMICHAEL.                                        
000600 DATE-WRITTEN.   90/11/12.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET HANTERAR ANNULLATION/ÄNDRING AV DC-ORDER-             
001200*        RADER AV EXTERNA ANVÄNDARE OM DE EJ ÄR UTSKRIVNA.                
001300*        OM ANNULLATION AV HEL ORDER ANROPAS BACKGRUNDS-                  
001400*        MPP W4029200.                                                    
001500*                                                                         
001600*        UTSKRIVNA DIREKTLEVERANSRADER PRELIMINÄRANNULLERAS               
001700*        VIA TRANS TILL 4680.                                             
001800*                                                                         
001900*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002000*        PROGRAMMET UPPDATERAR WLORQL (WDQ2)                              
002100*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)                              
002200*        PROGRAMMET UPPDATERAR WLARTM (WDK9)                              
002300*        PROGRAMMET UPPDATERAR WLORQM (WDQ1)                              
002400*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)                              
002500*        PROGRAMMET UPPDATERAR        (WDM2)                              
002600*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002700*        PROGRAMMET LÄSER      WLORQA (WDQ3)                              
002800*        PROGRAMMET LÄSER      WLORDP (WDA5)                              
002900*        PROGRAMMET LÄSER      WLXXKS (WDR4)                              
003000*                                                                         
003100*                                                                         
003200*    INDATA.                                                              
003300*        TRANSAKTION: W4T245                                              
003400*        MID:         W4I24501                                            
003500*                                                                         
003600*    UTDATA.                                                              
003700*        MOD:         W4O24501                                            
003800*        TRANSAKTION: W4T292U                                             
003900*        TRANSAKTION: W4T680X                                             
004000* CHANGE LOG:                                                             
004100*                                                                         
004200* ETRACK 1290414 INTERVALL FOR DISTRICT AND CUSTOMER                      
004300*                                                                         
004400*HÖSTEN 2004 GÖRAN KJELLSON                                               
004500*ETRACKER 887753                                                          
004600*                                                                         
004700* E'TRACKER 3921352 2006-09  RENSA PRISFRÅGA VID ANNULLATION              
004800* E'TRACKER 7450328 2008-HÖST  VOHF                                       
004900* E'TRACKER 10254592 2015       DECOMISSION VOHF                          
005000* E'TRACKER 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2          
005100*                                                                         
005200     SKIP3                                                                
005300 ENVIRONMENT DIVISION.                                                    
005400     EJECT                                                                
005500 DATA DIVISION.                                                           
005600 WORKING-STORAGE SECTION.                                                 
005700*    -- CHECKED BY WY2000                                                 
005800     SKIP3                                                                
005900 77  IDPGM                       PIC X(08)   VALUE 'W4024500'.            
006000 77  WS-PGM-POSITION             PIC X(24)   VALUE SPACE.                 
006100                                                                          
006200 77  JA                          PIC X       VALUE 'J'.                   
006300 77  YES                         PIC X       VALUE 'Y'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500                                                                          
006600 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
006700                                                                          
006800*01 -COPY WWDCKONS                                                        
006900                                                                          
007000*01 -COPY WWBYT03                                                         
007100                                                                          
007200 77  DIRLEV-KOLL                 PIC X       VALUE 'N'.                   
007300 77  WS-TINUDAT                  PIC S9(7)  COMP-3.                       
007400 77  WS-TINUTID                  PIC S9(9)  COMP-3.                       
007500                                                                          
007600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
007700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
007800 77  MAX-INDX                    PIC S9(4)  VALUE +6    COMP SYNC.        
007900                                                                          
008000 77  2109-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
008100 77  MAX-2109-INDX               PIC S9(4)  VALUE +18   COMP SYNC.        
008200                                                                          
008300 77  4360-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
008400                                                                          
008500 77  AVSR-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
008600                                                                          
008700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
008800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +836  COMP SYNC.        
008900                                                                          
009000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009100 01  WS-TIHHMMSS                 PIC 9(6)    VALUE ZERO.                  
009200 01  FILLER REDEFINES WS-TIHHMMSS.                                        
009300     03 WS-TIHHMM                PIC 9(4).                                
009400     03 FILLER                   PIC 9(2).                                
009500                                                                          
009600 01  WS-9KOMPL-DATUM             PIC 9(8).                                
009700 01  FILLER REDEFINES WS-9KOMPL-DATUM.                                    
009800     03 WS-CENTURY               PIC 9(2).                                
009900     03 WS-AAMMDD                PIC 9(6).                                
010000 77  WS-9KOMPL                   PIC 9(9)    VALUE 999999999.             
010100 77  WS-INDX-CL                  PIC S9(3)   VALUE ZERO COMP-3.           
010200 77  WS-INDX-CL-MAX              PIC S9(3)   VALUE +2   COMP-3.           
010300 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
010400 77  WS-IDDISTR-NUM              PIC 9(5)    VALUE ZERO.                  
010500 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
010600 77  WS-IDKUNDNR-NUM             PIC 9(7)    VALUE ZERO.                  
010700 77  WS-IDKUNDRF                 PIC X(7)    VALUE SPACE.                 
010800 77  WS-IDKUNDRF-NUM             PIC 9(7)    VALUE ZERO.                  
010900 01  WS-IDKUNDRF-RED.                                                     
011000     03 WS-IDKUNDRF-1-7          PIC X(7)    VALUE SPACE.                 
011100     03 WS-IDKUNDRF-8-10         PIC X(3)    VALUE SPACE.                 
011200 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
011300 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
011400 77  WS-IDRADNR                  PIC 9(4)    VALUE ZERO.                  
011500 77  WS-ODEL-IDDISTR-NUM         PIC 9(4)    VALUE ZERO.                  
011600 77  WS-ODEL-IDKUNDNR-NUM        PIC 9(6)    VALUE ZERO.                  
011700 77  HOPP                        PIC X(1)   VALUE 'N'.                    
011800 01 TABELL.                                                               
011900     03 KVBEART-Q-TABELL OCCURS 6.                                        
012000        05 WS-KVBEART-Q          PIC S9(7)    VALUE +0  COMP-3.           
012100 01  FILLER                      PIC X(16)   VALUE 'AAAAAAAAAAA'.         
012200 77  KVRADER-LOR-RAKNARE         PIC S9(5)    VALUE +0  COMP-3.           
012300 77  KVRADER-ODEL-RAKNARE        PIC S9(5)    VALUE +0  COMP-3.           
012400 77  KVRADER-UP-RAKNARE          PIC S9(5)    VALUE +0  COMP-3.           
012500 77  KVRADER-DIRL-RAKNARE        PIC S9(5)    VALUE +0  COMP-3.           
012600 77  TOTAL-KVRADER               PIC S9(5)    VALUE +0  COMP-3.           
012700 77  TOTAL-UTSKRIVNA             PIC S9(5)    VALUE +0  COMP-3.           
012800 77  MOJLIG-ANTAL                PIC S9(5)    VALUE +0  COMP-3.           
012900 77  MINSKAT-ANTAL               PIC S9(7)    VALUE +0  COMP-3.           
013000 77  ANNULLERAT-ANTAL            PIC S9(7)    VALUE +0  COMP-3.           
013100 01  FILLER                      PIC X(16)   VALUE 'BBBBBBBBBBB'.         
013200 77  WS-ANTOBKR                  PIC S9(3)    VALUE +0  COMP-3.           
013300 77  WS-KVQPACK-1                PIC S9(5)    VALUE +0  COMP-3.           
013400 77  WS-KVANNANT                 PIC S9(7)    VALUE +0  COMP-3.           
013500 77  WS-KVANNANT-NUM             PIC 9(6)     VALUE ZERO.                 
013600 77  WS-IDPURAD                  PIC 9(4)     VALUE ZERO.                 
013700 77  WS-TIDISPIN                 PIC 9(7)     VALUE ZERO.                 
013800 77  WS-IDORDER                  PIC S9(7)    VALUE +0  COMP-3.           
013900 77  FELTEXT                     PIC X(80)    VALUE SPACE.                
014000 77  WS-KDTRPKAT                 PIC X(1)     VALUE SPACE.                
014100 77  OBEHORIG                    PIC X(1)     VALUE 'F'.                  
014200                                                                          
014300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
014400     88  INDATA-OK                           VALUE 'J'.                   
014500     88  INDATA-FEL                          VALUE 'N'.                   
014600                                                                          
014700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
014800     88  NYCKLAR-OK                          VALUE 'J'.                   
014900     88  NYCKLAR-FEL                         VALUE 'N'.                   
015000                                                                          
015100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
015200     88  ALLT-OK                             VALUE 'J'.                   
015300                                                                          
015400 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
015500     88  FIRST-TIME                          VALUE 'J'.                   
015600                                                                          
015700 77  PF11-SW                     PIC X       VALUE 'J'.                   
015800     88  PRESS-PF11                          VALUE 'J'.                   
015900                                                                          
016000 77  DIRLEV-SW                   PIC X       VALUE 'J'.                   
016100     88  DIRLEV-RADER                        VALUE 'J'.                   
016200                                                                          
016300 77  AVSR-SW                     PIC X       VALUE 'J'.                   
016400     88  AVSR-OK                             VALUE 'J'.                   
016500                                                                          
016600 77  RAD-SW                      PIC X       VALUE 'J'.                   
016700     88  RAD-AENDRING                        VALUE 'J'.                   
016800                                                                          
016900 77  IFYLLT-SW                   PIC X       VALUE 'J'.                   
017000     88  IFYLLT-OK                           VALUE 'J'.                   
017100                                                                          
017200 77  SKRIV-SW                    PIC X       VALUE 'J'.                   
017300     88  SKRIV-OK                            VALUE 'J'.                   
017400                                                                          
017500 77  ANNULL-SW                   PIC X       VALUE 'J'.                   
017600     88  ANNULL-HELORDER                     VALUE 'J'.                   
017700                                                                          
017800 77  STATUS-SW                   PIC X       VALUE 'J'.                   
017900     88  STATUS-OK                           VALUE 'J'.                   
018000                                                                          
018100 77  CMD-SW                      PIC X       VALUE 'J'.                   
018200     88  CMD-OK                              VALUE 'J'.                   
018300     88  CMD-FEL                             VALUE 'N'.                   
018400                                                                          
018500 77  IDARTNR-SW                  PIC X       VALUE 'J'.                   
018600     88  IDARTNR-IFYLLT                      VALUE 'J'.                   
018700                                                                          
018800 77  REQUEST-SW                  PIC X       VALUE 'J'.                   
018900     88  REQUEST-SENT                        VALUE 'J'.                   
019000                                                                          
019100 77  SUPPLIER-REJECTED-SW        PIC X       VALUE 'J'.                   
019200     88  SUPPLIER-REJECTED                   VALUE 'J'.                   
019300                                                                          
019400 77  ORDER-SW                    PIC X       VALUE 'J'.                   
019500     88  ORDER-FINNS                         VALUE 'J'.                   
019600                                                                          
019700 77  SPAR-ORAD-KVBEART-Q         PIC S9(7)   VALUE +0  COMP-3.            
019800 77  SPAR-ORAD-KVPRERO           PIC S9(7)   VALUE +0  COMP-3.            
019900 77  SPAR-ORAD-KVPREAVB          PIC S9(7)   VALUE +0  COMP-3.            
020000 01 DB2-LASNING.                                                          
020100     03 FILLER                   PIC X(16)   VALUE                        
020200                                             'WS-DB2-SEKTION'.            
020300     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
020400                                                                          
020500     SKIP3                                                                
020600 01 NYCKLAR-TP4TRAN.                                                      
020700     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
020800     EJECT                                                                
020900                                                                          
021000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
021100     88  EGEN-MID                            VALUE '4245'.                
021200     88  GODK-MID                            VALUE '4245'.                
021300     EJECT                                                                
021400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
021500 01  GENERELLA-SUBPROGRAM.                                                
021600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
021700     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
021800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
021900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
022000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
022100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
022200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
022300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
022400                                                                          
022500 01  GEMENSAMMA-SUBPROGRAM.                                               
022600     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
022700     03  W413AVSO                PIC X(8)    VALUE 'W413AVSO'.            
022800     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
022900     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
023000     EJECT                                                                
023100*    --- PARAMETRAR TILL ABEND                                            
023200 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
023300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
023400*   -COPY WMEDAREA                                                        
023500     SKIP3                                                                
023600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
023700*   -COPY WMSGINIT                                                        
023800     SKIP3                                                                
023900*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
024000*   -COPY WDATAREA                                                        
024100     SKIP3                                                                
024200*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
024300*   -COPY WDECAREA                                                        
024400     SKIP3                                                                
024500*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
024600*   -COPY WSECAREA                                                        
024700     SKIP3                                                                
024800*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
024900*01  -COPY W009CIA                                                        
025000                                                                          
025100 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
025200*01  FILLER -COPY WWDIST18 -RED TEST-IDDISTR.                             
025300     EJECT                                                                
025400*01  FILLER -COPY WWDIST35 -RED TEST-IDDISTR.                             
025500     EJECT                                                                
025600*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
025700     EJECT                                                                
025800 01  FILLER                      PIC X(16)   VALUE 'DIST-DC-TAB'.         
025900     -COPY WWDIST57                                                       
026000     EJECT                                                                
026100 01  MESSAGE-CODES.                                                       
026200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
026300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
026400     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
026500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
026600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
026700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
026800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
026900     03  INF-PART-MISSING        PIC X(3)    VALUE '017'.                 
027000     03  INF-ORDER-DELETE        PIC X(3)    VALUE '052'.                 
027100     03  INF-ORDERLINE-DELETE    PIC X(3)    VALUE '752'.                 
027200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
027300     03  ERR-ANNULL-NOT-POSS     PIC X(3)    VALUE '066'.                 
027400     03  ERR-ORDER-EJ-AVSLUTAD   PIC X(3)    VALUE '053'.                 
027500     03  ERR-LINES-MISSING       PIC X(3)    VALUE '029'.                 
027600     03  ERR-LINES-MISSING-CL    PIC X(3)    VALUE '028'.                 
027700     03  ERR-LINES-WRITTEN       PIC X(3)    VALUE '067'.                 
027800     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
027900     03  ERR-ORDER-MISSING       PIC X(3)    VALUE '701'.                 
028000     03  ERR-CMD-FEL             PIC X(3)    VALUE '156'.                 
028100     EJECT                                                                
028200*    --- AREOR FÖR GEMENSAMMA-SUBPROGRAM                                  
028300*                                                                         
028400 01  FILLER                      PIC X(16)   VALUE 'LÄNKAREOR'.           
028500*01  -COPY W413AVSR                                                       
028600     EJECT                                                                
028700*01  -COPY W413AVSO                                                       
028800     EJECT                                                                
028900 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
029000*   -COPY W335PRQU                                                        
029100     EJECT                                                                
029200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
029300*                                                                         
029400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
029500     SKIP3                                                                
029600*01  MID -COPY W4I24501                                                   
029700     EJECT                                                                
029800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
029900     SKIP3                                                                
030000*01  -COPY WMSGAREA                                                       
030100     03  MOD REDEFINES MSG-AREA.                                          
030200*      05  -COPY W4O24501                                                 
030300     EJECT                                                                
030400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
030500     SKIP3                                                                
030600*01  -COPY WMFSAREA                                                       
030700     EJECT                                                                
030800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
030900*                                                                         
031000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031100     SKIP3                                                                
031200 01  NYCKLAR-TILL-DLI.                                                    
031300*    ---------TILL WDQ101                                                 
031400     03  W-WDQ101KY-MIN-X.                                                
031500         05  W-OBKR-IDORDER-MIN   PIC S9(7)   VALUE ZERO COMP-3.          
031600         05  W-OBKR-IDARTNR-MIN   PIC S9(9)   VALUE ZERO COMP-3.          
031700         05  W-OBKR-IDLOPNR-MIN   PIC S9(3)   VALUE ZERO COMP-3.          
031800         05  W-OBKR-IDSEKVNR-MIN  PIC S9(3)   VALUE ZERO COMP-3.          
031900         05  W-OBKR-IDDC-MIN      PIC  X(2)   VALUE SPACE.                
032000         05  W-OBKR-KDORDBEK-MIN  PIC 9(2)    VALUE ZERO.                 
032100                                                                          
032200     03  W-WDQ101KY-MAX-X.                                                
032300         05  W-OBKR-IDORDER-MAX   PIC S9(7)   VALUE ZERO COMP-3.          
032400         05  W-OBKR-IDARTNR-MAX   PIC S9(9)   VALUE ZERO COMP-3.          
032500         05  W-OBKR-IDLOPNR-MAX   PIC S9(3)   VALUE ZERO COMP-3.          
032600         05  W-OBKR-IDSEKVNR-MAX  PIC S9(3)   VALUE ZERO COMP-3.          
032700         05  W-OBKR-IDDC-MAX      PIC  X(2)   VALUE SPACE.                
032800         05  W-OBKR-KDORDBEK-MAX  PIC 9(2)    VALUE ZERO.                 
032900                                                                          
033000*    ---------TILL WDQ201                                                 
033100     03  W-IDORDER-X.                                                     
033200         05  W-OHUV-IDORDER       PIC S9(7)   VALUE ZERO COMP-3.          
033300*    ---------TILL WDQ201 VIA WDQ2C1(SEK. INDX)                           
033400     03  W-WDQ2CSEQ-X.                                                    
033500         05  W-Q2CSEQ-IDDISTR     PIC S9(5)    VALUE ZERO COMP-3.         
033600         05  W-Q2CSEQ-IDKUNDNR    PIC S9(7)    VALUE ZERO COMP-3.         
033700         05  W-Q2CSEQ-IDKUNDRF    PIC  X(10)   VALUE SPACE.               
033800*    ---------TILL WDQ212                                                 
033900     03  W-IDDC-X.                                                        
034000         05  W-IDDC               PIC X(2)    VALUE SPACE.                
034100*    ---------TILL WDQ301                                                 
034200     03  W-WDQ301KY-MIN-X.                                                
034300         05  W-ODEL-IDORDER-MIN   PIC S9(7)    VALUE ZERO COMP-3.         
034400         05  W-ODEL-IDDC-MIN      PIC  X(2)    VALUE SPACE.               
034500         05  W-ODEL-IDPRODNR-MIN  PIC S9(7)    VALUE ZERO COMP-3.         
034600         05  W-ODEL-IDPLKLST-MIN  PIC S9(3)    VALUE ZERO COMP-3.         
034700     03  W-WDQ301KY-MAX-X.                                                
034800         05  W-ODEL-IDORDER-MAX   PIC S9(7)    VALUE ZERO COMP-3.         
034900         05  W-ODEL-IDDC-MAX      PIC  X(2)    VALUE SPACE.               
035000         05  W-ODEL-IDPRODNR-MAX  PIC S9(7)    VALUE ZERO COMP-3.         
035100         05  W-ODEL-IDPLKLST-MAX  PIC S9(3)    VALUE ZERO COMP-3.         
035200*    ---------TILL WDQ401                                                 
035300     03  W-WDQ401KY-MIN-X.                                                
035400         05  W-ORAD-IDORDER-MIN   PIC S9(7)    VALUE ZERO COMP-3.         
035500         05  W-ORAD-IDDC-MIN      PIC  X(2)    VALUE SPACE.               
035600         05  W-ORAD-ADLAGOMR-MIN  PIC S9(3)    VALUE ZERO COMP-3.         
035700         05  W-ORAD-ADGANG-MIN    PIC S9(3)    VALUE ZERO COMP-3.         
035800         05  W-ORAD-ADPLATS-MIN   PIC S9(5)    VALUE ZERO COMP-3.         
035900         05  FILLER               PIC S9(9)    VALUE ZERO COMP-3.         
036000         05  FILLER               PIC S9(3)    VALUE ZERO COMP-3.         
036100     03  W-ORAD-IDARTNR-MIN-X.                                            
036200         05  W-ORAD-IDARTNR-MIN   PIC S9(9)    VALUE ZERO COMP-3.         
036300     03  W-ORAD-IDLOPNR-MIN-X.                                            
036400         05  W-ORAD-IDLOPNR-MIN   PIC S9(3)    VALUE ZERO COMP-3.         
036500     03  W-WDQ401KY-MAX-X.                                                
036600         05  W-ORAD-IDORDER-MAX   PIC S9(7)    VALUE ZERO COMP-3.         
036700         05  W-ORAD-IDDC-MAX      PIC  X(2)    VALUE SPACE.               
036800         05  W-ORAD-ADLAGOMR-MAX  PIC S9(3)    VALUE ZERO COMP-3.         
036900         05  W-ORAD-ADGANG-MAX    PIC S9(3)    VALUE ZERO COMP-3.         
037000         05  W-ORAD-ADPLATS-MAX   PIC S9(5)    VALUE ZERO COMP-3.         
037100         05  FILLER               PIC S9(9)    VALUE ZERO COMP-3.         
037200         05  FILLER               PIC S9(3)    VALUE ZERO COMP-3.         
037300     03  W-ORAD-IDARTNR-MAX-X.                                            
037400         05  W-ORAD-IDARTNR-MAX   PIC S9(9)    VALUE ZERO COMP-3.         
037500     03  W-ORAD-IDLOPNR-MAX-X.                                            
037600         05  W-ORAD-IDLOPNR-MAX   PIC S9(3)    VALUE ZERO COMP-3.         
037700     03  W-WDQ401KY-MIN-MIN-X.                                            
037800         05  W-ORAD-IDORDER-MIN-MIN  PIC S9(7)  VALUE ZERO COMP-3.        
037900         05  W-ORAD-IDDC-MIN-MIN     PIC  X(2)  VALUE SPACE.              
038000         05  W-ORAD-ADLAGOMR-MIN-MIN PIC S9(3)  VALUE ZERO COMP-3.        
038100         05  W-ORAD-ADGANG-MIN-MIN   PIC S9(3)  VALUE ZERO COMP-3.        
038200         05  W-ORAD-ADPLATS-MIN-MIN  PIC S9(5)  VALUE ZERO COMP-3.        
038300         05  W-ORAD-IDARTNR-MIN-MIN  PIC S9(9)  VALUE ZERO COMP-3.        
038400         05  W-ORAD-IDLOPNR-MIN-MIN  PIC S9(3)  VALUE ZERO COMP-3.        
038500     03  W-WDQ401KY-MAX-MAX-X.                                            
038600         05  W-ORAD-IDORDER-MAX-MAX  PIC S9(7)  VALUE ZERO COMP-3.        
038700         05  W-ORAD-IDDC-MAX-MAX     PIC  X(2)  VALUE SPACE.              
038800         05  W-ORAD-ADLAGOMR-MAX-MAX PIC S9(3)  VALUE ZERO COMP-3.        
038900         05  W-ORAD-ADGANG-MAX-MAX   PIC S9(3)  VALUE ZERO COMP-3.        
039000         05  W-ORAD-ADPLATS-MAX-MAX  PIC S9(5)  VALUE ZERO COMP-3.        
039100         05  W-ORAD-IDARTNR-MAX-MAX  PIC S9(9)  VALUE ZERO COMP-3.        
039200         05  W-ORAD-IDLOPNR-MAX-MAX  PIC S9(3)  VALUE ZERO COMP-3.        
039300     03  W-WDQ401KY-UNIK-X.                                               
039400         05  W-ORAD-IDORDER-UNIK     PIC S9(7)  VALUE ZERO COMP-3.        
039500         05  W-ORAD-IDDC-UNIK        PIC  X(2)  VALUE SPACE.              
039600         05  W-ORAD-ADLAGOMR-UNIK    PIC S9(3)  VALUE ZERO COMP-3.        
039700         05  W-ORAD-ADGANG-UNIK      PIC S9(3)  VALUE ZERO COMP-3.        
039800         05  W-ORAD-ADPLATS-UNIK     PIC S9(5)  VALUE ZERO COMP-3.        
039900         05  W-ORAD-IDARTNR-UNIK     PIC S9(9)  VALUE ZERO COMP-3.        
040000         05  W-ORAD-IDLOPNR-UNIK     PIC S9(3)  VALUE ZERO COMP-3.        
040100*    _________TILL WDD311                                                 
040200     03  W-WDD3BSEQ-X.                                                    
040300         05  W-WDD3-IDARTNR          PIC S9(9)  VALUE ZERO COMP-3.        
040400     03  W-IDSKYLT-X.                                                     
040500         05  W-TEXT-IDSKYLT          PIC X(3)   VALUE SPACE.              
040600*    ---------TILL WDK901,WDK601                                          
040700     03  W-IDARTNR-X.                                                     
040800         05  W-IDARTNR               PIC S9(9)  VALUE ZERO COMP-3.        
040900                                                                          
041000*    ---------TILL WDA501                                                 
041100     03  W-WDA501KY-X.                                                    
041200         05  W-RAD-IDDISTR           PIC S9(5)  VALUE ZERO COMP-3.        
041300         05  W-RAD-IDKUNDNR          PIC S9(7)  VALUE ZERO COMP-3.        
041400         05  W-RAD-IDKUNDRF          PIC  X(10) VALUE SPACE.              
041500         05  W-RAD-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.        
041600         05  W-RAD-IDLOPNR           PIC S9(3)  VALUE ZERO COMP-3.        
041700     SKIP2                                                                
041800   03    W-WDA601KY-MIN-X.                                                
041900     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
042000     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
042100     05    W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.               
042200     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
042300     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
042400     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
042500     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
042600     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
042700     SKIP2                                                                
042800   03    W-WDA601KY-MAX-X.                                                
042900     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
043000     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
043100     05    W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.               
043200     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
043300     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
043400     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
043500     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
043600     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
043700                                                                          
043800*    -------- TILL WDM2                                                   
043900     03  W-WDM201-X.                                                      
044000         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
044100         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
044200                                                                          
044300     03  W-WDM211-X.                                                      
044400         05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
044500                                                                          
044600     03  W-WDM221-X.                                                      
044700         05  W-KMRK-IDDISTR-FOM  PIC S9(05) VALUE ZERO COMP-3.            
044800         05  W-KMRK-IDDISTR-TOM  PIC S9(05) VALUE ZERO COMP-3.            
044900         05  W-KMRK-IDKUNDNR-FOM PIC S9(07) VALUE ZERO COMP-3.            
045000         05  W-KMRK-IDKUNDNR-TOM PIC S9(07) VALUE ZERO COMP-3.            
045100*                                                                         
045200     03  W-WDE401-X.                                                      
045300         05  W-401-IDDISTR      PIC S9(5)      VALUE ZERO COMP-3.         
045400         05  W-401-IDKUNDNR     PIC S9(7)      VALUE ZERO COMP-3.         
045500         05  W-401-IDKUNDRF.                                              
045600           07  W-401-IDORDNR    PIC 9(5)       VALUE ZERO.                
045700           07  FILLER           PIC X(5)       VALUE SPACE.               
045800         05  W-401-IDPRODNR     PIC S9(7)      VALUE ZERO COMP-3.         
045900         05  W-401-IDPLKLST     PIC S9(3)      VALUE ZERO COMP-3.         
046000*                                                                         
046100     03  W-WDE411-X.                                                      
046200         05  W-411-IDPURAD      PIC S9(5)      VALUE ZERO COMP-3.         
046300*                                                                         
046400     03  W-KDODELST-X.                                                    
046500         05  W-KDODELST         PIC  X(1)      VALUE 'U'.                 
046600*                                                                         
046700     03  W-IDDC-B6-X.                                                     
046800         05 W-IDDC-B6                  PIC X(2).                          
046900                                                                          
047000     03  W-IDPRODNR-X.                                                    
047100         05 W-IDPRODNR            PIC S9(7) COMP-3.                       
047200*                                                                         
047300     03  W-IDPURAD-X.                                                     
047400         05 W-IDPURAD             PIC S9(5) COMP-3.                       
047500*                                                                         
047600                                                                          
047700*    --- STATUS-KOD FRÅN IMS                                              
047800 01  STATUS-WS                   PIC XX.                                  
047900     88  SEGMENT-FINNS                       VALUE '  '.                  
048000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
048100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
048200     88  BASEN-SLUT                          VALUE 'GB'.                  
048300     SKIP2                                                                
048400 01  GODK-STATUSKODER.                                                    
048500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
048600     SKIP3                                                                
048700 01  SSA1                        PIC X(200).                              
048800 01  SSA2                        PIC X(128).                              
048900 01  SSA3                        PIC X(128).                              
049000     EJECT                                                                
049100******************************************************                    
049200*    ARBETSAREA FÖR RYB-TRANS TILL WDG6              *                    
049300******************************************************                    
049400 01  W-RYBPOST.                                                           
049500*    03  -COPY WDGZRYB    -PRE W-                                         
049600     EJECT                                                                
049700******************************************************                    
049800*    ARBETSAREA FÖR RYC-TRANS TILL WDG6              *                    
049900******************************************************                    
050000 01  W-RYCPOST.                                                           
050100*    03  -COPY WDGZRYC -PRE W-                                            
050200     EJECT                                                                
050300 01  W-RYCSPOST.                                                          
050400*    03  -COPY WDGZRYCS -PRE W-                                           
050500     EJECT                                                                
050600*                            DB2 FUNKTIONSKODER                           
050700 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
050800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
050900                                                                          
051000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
051100 01  DB2-WS.                                                              
051200     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
051300         88  CURSOR-OK                       VALUE 000.                   
051400         88  RADER-FINNS                     VALUE 000.                   
051500         88  RADER-SAKNAS                    VALUE 100.                   
051600         88  ATKOMST-FEL                     VALUE 904.                   
051700     03  GODK-SQLCODEKODER.                                               
051800         05  GODK-SQLCODE OCCURS 5                                        
051900             INDEXED BY SQLCODE-IX PIC 9(3).                              
052000 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
052100     EJECT                                                                
052200*    --- IMS FUNKTIONSKODER                                               
052300*01  -COPY W0003                                                          
052400     EJECT                                                                
052500*    ---  DLI INPUT-OUTPUT AREA                                           
052600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
052700     SKIP3                                                                
052800 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-WDQ201'.        
052900 01  DLI-IO-AREA-OHUV.                                                    
053000     03  WLORQI01.                                                        
053100*        05  -COPY WDQ201                                                 
053200     EJECT                                                                
053300                                                                          
053400 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-WDQ211'.        
053500 01  DLI-IO-AREA-DIRL.                                                    
053600     03  WLORQI11.                                                        
053700*        05  -COPY WDQ211                                                 
053800     EJECT                                                                
053900                                                                          
054000 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-WDQ212'.        
054100 01  DLI-IO-AREA-ARB.                                                     
054200     03  WLORQI12.                                                        
054300*        05  -COPY WDQ212                                                 
054400     EJECT                                                                
054500                                                                          
054600 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-WDQ221'.        
054700 01  DLI-IO-Q221.                                                         
054800*    03  -COPY WDQ221                                                     
054900     EJECT                                                                
055000                                                                          
055100 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-WDQ301'.        
055200 01  DLI-IO-AREA-ODEL.                                                    
055300     03  WLORQA01.                                                        
055400*        05  -COPY WDQ301                                                 
055500     EJECT                                                                
055600                                                                          
055700 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-WDQ401'.        
055800 01  DLI-IO-AREA-ORAD.                                                    
055900     03  WLORQF01.                                                        
056000*        05  -COPY WDQ401                                                 
056100     EJECT                                                                
056200                                                                          
056300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E401'.             
056400 01  DLI-IO-AREA-E401.                                                    
056500*    03             -COPY WDE401                                          
056600     EJECT                                                                
056700                                                                          
056800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-E411'.             
056900 01  DLI-IO-AREA-E411.                                                    
057000*    03             -COPY WDE411 -PRE E4-                                 
057100     EJECT                                                                
057200 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
057300 01  DLI-IO-AREA-WDA6.                                                    
057400*  03    -COPY WDA601                                                     
057500     EJECT                                                                
057600 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-WDK901'.        
057700 01  DLI-IO-AREA-ARTM.                                                    
057800     03  WLARTM01.                                                        
057900*        05  -COPY WDK901                                                 
058000     EJECT                                                                
058100                                                                          
058200 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-WDK611'.        
058300 01  DLI-IO-AREA-K611.                                                    
058400     03  WLARTC11.                                                        
058500*        05  -COPY WDK611                                                 
058600     EJECT                                                                
058700                                                                          
058800 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-WDK711'.        
058900 01  DLI-IO-AREA-K711.                                                    
059000     03  WLARTS11.                                                        
059100*        05  -COPY WDK711                                                 
059200     EJECT                                                                
059300                                                                          
059400 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-ZZAC01'.        
059500 01  DLI-IO-AREA-ZZAC.                                                    
059600     03  WLZZAC01.                                                        
059700*        05  -COPY WDGZ01                                                 
059800     EJECT                                                                
059900                                                                          
060000 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-WDQ101'.        
060100 01  DLI-IO-AREA-OBKR.                                                    
060200     03  WLORQM01.                                                        
060300*        05  -COPY WDQ101                                                 
060400     EJECT                                                                
060500                                                                          
060600 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-WDA501'.        
060700 01  DLI-IO-AREA-ORDP.                                                    
060800     03  WLORDP01.                                                        
060900*        05  -COPY WDA501                                                 
061000     EJECT                                                                
061100                                                                          
061200 01  FILLER                    PIC X(16)   VALUE 'IO-AREA-WDD311'.        
061300 01  DLI-IO-AREA-BENA.                                                    
061400     03  WLBENA11.                                                        
061500*        05  -COPY WDD311                                                 
061600     EJECT                                                                
061700                                                                          
061800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
061900 01   DLI-IO-AREA-B601.                                                   
062000*     03  -COPY WDB601                                                    
062100                                                                          
062200     EJECT                                                                
062300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
062400 01  DLI-IO-WDM211.                                                       
062500*    03 -COPY WDM211                                                      
062600                                                                          
062700     EJECT                                                                
062800 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
062900 01  DLI-IO-WDM221.                                                       
063000*    03 -COPY WDM221                                                      
063100                                                                          
063200     EJECT                                                                
063300 01  FILLER               PIC X(16)   VALUE 'WDF601 AREA'.                
063400 01   DLI-IO-AREA-F601.                                                   
063500*     03  -COPY WDF601                                                    
063600                                                                          
063700     EJECT                                                                
063800 01  FILLER               PIC X(16)   VALUE 'WDF611 AREA'.                
063900 01   DLI-IO-AREA-F611.                                                   
064000*     03  -COPY WDF611                                                    
064100                                                                          
064200     EJECT                                                                
064300                                                                          
064400*    MSG-AREA FÖR HOPP TILL W40292                                        
064500 01  FILLER            PIC X(16)   VALUE 'ALT1-MSG-IO-AREA'.              
064600 01  W-PROG-TO-PROG-SW.                                                   
064700     03  ALT1-LL                   PIC S9(4) VALUE +76 COMP SYNC.         
064800     03  ALT1-Z1                   PIC X.                                 
064900     03  ALT1-Z2                   PIC X.                                 
065000     03  ALT1-TRANSKOD             PIC X(8)  VALUE 'W4T292X '.            
065100     03  ALT1-IDTRANS              PIC X(4)  VALUE '4245'.                
065200     03  ALT1-KDMFSFOR             PIC X.                                 
065300*    03  MID -COPY W4I29201    -PRE ALT1-                                 
065400     EJECT                                                                
065500 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
065600 01  W-PROG-TO-PROG-SW-2.                                                 
065700     03  2109-KVLL                PIC S9(4)  COMP SYNC.                   
065800     03  2109-Z1                   PIC X.                                 
065900     03  2109-Z2                   PIC X.                                 
066000     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
066100     03  2109-IDTRANS              PIC X(4)  VALUE '4245'.                
066200     03  2109-KDMFSFOR             PIC X.                                 
066300*    03  -COPY W2I10902    -PRE 2109-                                     
066400     EJECT                                                                
066500*    MSG-AREA FÖR HOPP TILL W40680                                        
066600 01  FILLER            PIC X(16)   VALUE '4680-MSG-IO-AREA'.              
066700 01  W-PROG-TO-PROG-SW-3.                                                 
066800     03  4680-KVLL                 PIC S9(4) COMP SYNC.                   
066900     03  4680-Z1                   PIC X.                                 
067000     03  4680-Z2                   PIC X.                                 
067100     03  4680-TRANSKOD             PIC X(8)  VALUE 'W4T680X '.            
067200     03  4680-IDTRANS              PIC X(4)  VALUE '4245'.                
067300     03  4680-KDMFSFOR             PIC X.                                 
067400*    03  -COPY W4I68001    -PRE 4680-                                     
067500     EJECT                                                                
067600*    MSG-AREA FÖR HOPP TILL W40360                                        
067700 01  FILLER            PIC X(16)   VALUE '4360-MSG-IO-AREA'.              
067800 01  W-PROG-TO-PROG-SW-4.                                                 
067900     03  4360-KVLL                 PIC S9(4) COMP SYNC.                   
068000     03  4360-Z1                   PIC X.                                 
068100     03  4360-Z2                   PIC X.                                 
068200     03  4360-TRANSKOD             PIC X(8)  VALUE 'W4T360X '.            
068300     03  4360-IDTRANS              PIC X(4)  VALUE '4245'.                
068400     03  4360-KDMFSFOR             PIC X.                                 
068500*    03  -COPY W4I36001    -PRE 4360-                                     
068600     EJECT                                                                
068700 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
068800                                                                          
068900*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
069000     EJECT                                                                
069100     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
069200     EJECT                                                                
069300 LINKAGE SECTION.                                                         
069400                                                                          
069500*01  -COPY W0009      -PRE MSG-                                           
069600     EJECT                                                                
069700*01  -COPY W0009      -PRE ALT1-                                          
069800     EJECT                                                                
069900*01  -COPY W0009      -PRE 2109-                                          
070000     EJECT                                                                
070100*01  -COPY W0009      -PRE 4680-                                          
070200     EJECT                                                                
070300*01  -COPY W0009      -PRE 4360-                                          
070400     EJECT                                                                
070500 01  AVSR-ALT2-PCB               PIC X.                                   
070600     EJECT                                                                
070700*01  -COPY W0008      -PRE USEA-                                          
070800     05  FILLER                  PIC X.                                   
070900     EJECT                                                                
071000*01  -COPY W0008      -PRE WDQ2-                                          
071100     05  FILLER                  PIC X.                                   
071200     EJECT                                                                
071300*01  -COPY W0008      -PRE WDQ3-                                          
071400     05  FILLER                  PIC X.                                   
071500     EJECT                                                                
071600*01  -COPY W0008      -PRE WDQ4-                                          
071700     05  FILLER                  PIC X.                                   
071800     EJECT                                                                
071900*01  -COPY W0008      -PRE BENA-                                          
072000     05  FILLER                  PIC X.                                   
072100     EJECT                                                                
072200*01  -COPY W0008      -PRE WDK6-                                          
072300     05  FILLER                  PIC X.                                   
072400     EJECT                                                                
072500*01  -COPY W0008      -PRE WDK7-                                          
072600     05  FILLER                  PIC X.                                   
072700     EJECT                                                                
072800*01  -COPY W0008      -PRE ARTM-                                          
072900     05  FILLER                  PIC X.                                   
073000     EJECT                                                                
073100*01  -COPY W0008      -PRE ZZAC-                                          
073200     05  FILLER                  PIC X.                                   
073300     EJECT                                                                
073400*01  -COPY W0008      -PRE ORQM-                                          
073500     05  FILLER                  PIC X.                                   
073600     EJECT                                                                
073700*01  -COPY W0008      -PRE WDA5-                                          
073800     05  FILLER                  PIC X.                                   
073900     EJECT                                                                
074000*01  -COPY W0008      -PRE WDM2-                                          
074100     05  FILLER                  PIC X.                                   
074200     EJECT                                                                
074300*01  -COPY W0008      -PRE WDE4-                                          
074400     05  FILLER                  PIC X.                                   
074500     EJECT                                                                
074600*01    -COPY W0008     -PRE WDA6B-                                        
074700     05  FILLER                  PIC X.                                   
074800     EJECT                                                                
074900*01    -COPY W0008     -PRE WDB6-                                         
075000     05  FILLER                  PIC X.                                   
075100     EJECT                                                                
075200*01    -COPY W0008     -PRE WDF6-                                         
075300     05  FILLER                  PIC X.                                   
075400     EJECT                                                                
075500*----> SUBPROGRAM W413AVSR.                                               
075600 01  AVSR-ORQI-PCB               PIC X.                                   
075700 01  AVSR-GMTB-PCB               PIC X.                                   
075800 01  AVSR-GMTC-PCB               PIC X.                                   
075900 01  AVSR-WDB2-PCB               PIC X.                                   
076000 01  AVSR-WDB6-PCB               PIC X.                                   
076100     EJECT                                                                
076200*----> SUBPROGRAM W413AVSO.                                               
076300 01  AVSO-WDE6-PCB               PIC X.                                   
076400 01  AVSO-ORQA-PCB               PIC X.                                   
076500 01  AVSO-WDQ2-PCB               PIC X.                                   
076600 01  AVSO-GMTB-PCB               PIC X.                                   
076700 01  AVSO-XXKA-PCB               PIC X.                                   
076800 01  AVSO-4437-PCB               PIC X.                                   
076900 01  AVSO-XXKE-PCB               PIC X.                                   
077000 01  AVSO-XXKF-PCB               PIC X.                                   
077100 01  AVSO-XXKG-PCB               PIC X.                                   
077200 01  AVSO-XXKH-PCB               PIC X.                                   
077300 01  AVSO-XXKI-PCB               PIC X.                                   
077400 01  AVSO-XXKP-PCB               PIC X.                                   
077500 01  AVSO-WDB2-PCB               PIC X.                                   
077600 01  AVSO-WDB6-PCB               PIC X.                                   
077700 01  AVSO-WDP7-PCB               PIC X.                                   
077800 01  TRAN-XXKB-PCB               PIC X.                                   
077900 01  ORDN-ORQL-PCB               PIC X.                                   
078000 01  ORDN-PROC-PCB               PIC X.                                   
078100 01  ORDN-ORQI-PCB               PIC X.                                   
078200 01  ORDN-WDQ3-PCB               PIC X.                                   
078300     EJECT                                                                
078400*----> SUBPROGRAM W335PRQU.                                               
078500 01  PRQU-WDC7-PCB               PIC X.                                   
078600 01  PRQU-WDG2-PCB               PIC X.                                   
078700 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
078800     EJECT                                                                
078900 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB 2109-PCB 4680-PCB             
079000                           4360-PCB                                       
079100                           AVSR-ALT2-PCB USEA-PCB WDQ2-PCB                
079200                           WDQ3-PCB WDQ4-PCB BENA-PCB WDK6-PCB            
079300                           WDK7-PCB ARTM-PCB ZZAC-PCB ORQM-PCB            
079400                           WDA5-PCB WDM2-PCB WDE4-PCB                     
079500                           WDA6B-PCB WDB6-PCB WDF6-PCB                    
079600                           AVSR-ORQI-PCB AVSR-GMTB-PCB                    
079700                           AVSR-GMTC-PCB                                  
079800                           AVSR-WDB2-PCB AVSR-WDB6-PCB                    
079900                           AVSO-WDE6-PCB AVSO-ORQA-PCB                    
080000                           AVSO-WDQ2-PCB                                  
080100                           AVSO-GMTB-PCB AVSO-XXKA-PCB                    
080200                           AVSO-4437-PCB AVSO-XXKE-PCB                    
080300                           AVSO-XXKF-PCB AVSO-XXKG-PCB                    
080400                           AVSO-XXKH-PCB AVSO-XXKI-PCB                    
080500                           AVSO-XXKP-PCB AVSO-WDB2-PCB                    
080600                           AVSO-WDB6-PCB                                  
080700                           AVSO-WDP7-PCB TRAN-XXKB-PCB                    
080800                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
080900                           ORDN-ORQI-PCB ORDN-WDQ3-PCB                    
081000                           PRQU-WDG2-PCB PRQU-WDC7-PCB                    
081100                           PRQU-SJKO-WDK6-PCB.                            
081200                                                                          
081300     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB 2109-PCB 4680-PCB             
081400                           4360-PCB                                       
081500                           AVSR-ALT2-PCB USEA-PCB WDQ2-PCB                
081600                           WDQ3-PCB WDQ4-PCB BENA-PCB WDK6-PCB            
081700                           WDK7-PCB ARTM-PCB ZZAC-PCB ORQM-PCB            
081800                           WDA5-PCB WDM2-PCB WDE4-PCB                     
081900                           WDA6B-PCB WDB6-PCB WDF6-PCB                    
082000                           AVSR-ORQI-PCB AVSR-GMTB-PCB                    
082100                           AVSR-GMTC-PCB                                  
082200                           AVSR-WDB2-PCB AVSR-WDB6-PCB                    
082300                           AVSO-WDE6-PCB AVSO-ORQA-PCB                    
082400                           AVSO-WDQ2-PCB                                  
082500                           AVSO-GMTB-PCB AVSO-XXKA-PCB                    
082600                           AVSO-4437-PCB AVSO-XXKE-PCB                    
082700                           AVSO-XXKF-PCB AVSO-XXKG-PCB                    
082800                           AVSO-XXKH-PCB AVSO-XXKI-PCB                    
082900                           AVSO-XXKP-PCB AVSO-WDB2-PCB                    
083000                           AVSO-WDB6-PCB                                  
083100                           AVSO-WDP7-PCB TRAN-XXKB-PCB                    
083200                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
083300                           ORDN-ORQI-PCB ORDN-WDQ3-PCB                    
083400                           PRQU-WDG2-PCB PRQU-WDC7-PCB                    
083500                           PRQU-SJKO-WDK6-PCB.                            
083600                                                                          
083700     PERFORM IMS-GET-MSG                                                  
083800     IF SEGMENT-FINNS                                                     
083900       PERFORM A-INIT                                                     
084000       PERFORM C-KOLLA-NYCKLAR                                            
084100       IF NYCKLAR-OK                                                      
084200         PERFORM B-KOLLA-BEHOERIGHET                                      
084300         IF SEC-KDSVAR NOT = OBEHORIG                                     
084400           PERFORM D-LAES-IN-ORDER                                        
084500           IF ALLT-OK                                                     
084600             MOVE NEJ TO ANNULL-SW                                        
084700             IF MFS-UPD-V OR MFS-UPDATE                                   
084800               PERFORM I-KOLLA-INPUT                                      
084900               IF INDATA-OK                                               
085000                 PERFORM J-UPPDATERA                                      
085100               END-IF                                                     
085200             ELSE                                                         
085300               IF MFS-FIRST                                               
085400                 PERFORM E-FOERSTA-SIDA                                   
085500               ELSE                                                       
085600                 IF MFS-NEXT                                              
085700                   PERFORM F-NAESTA-SIDA                                  
085800                 ELSE                                                     
085900                   PERFORM G-SAMMA-SIDA                                   
086000                 END-IF                                                   
086100               END-IF                                                     
086200             END-IF                                                       
086300             IF INDATA-OK AND NYCKLAR-OK AND                              
086400               NOT ANNULL-HELORDER                                        
086500               IF WS-IDDC NOT = W-IDDC-B6                                 
086600                  MOVE WS-IDDC TO W-IDDC-B6                               
086700                  PERFORM IMS-GU-WDB601                                   
086800               END-IF                                                     
086900               IF DCS-KDDC = SPACE OR DCS-DDC                             
087000                 PERFORM L-LAES-VISA-DDGS                                 
087100               ELSE                                                       
087200                 PERFORM H-LAES-VISA-INFO                                 
087300               END-IF                                                     
087400               IF 2109-MID2-KVANTART > ZERO                               
087500                 PERFORM K-STARTA-2109                                    
087600               END-IF                                                     
087700             END-IF                                                       
087800           END-IF                                                         
087900         ELSE                                                             
088000           MOVE ERR-OBEHORIG TO MED-IDMFSFEL                              
088100           CALL WMEDKONV USING MED-WMEDAREA                               
088200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
088300         END-IF                                                           
088400       END-IF                                                             
088500       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
088600       PERFORM IMS-INSERT-MSG                                             
088700     END-IF                                                               
088800                                                                          
088900     MOVE ZERO TO RETURN-CODE                                             
089000     GOBACK                                                               
089100     .                                                                    
089200     EJECT                                                                
089300 A-INIT SECTION.                                                          
089400                                                                          
089500     MOVE 'STA A-INIT    '                TO   WS-PGM-POSITION            
089600                                                                          
089700     IF MSG-DUBBLA-TRANSKODER                                             
089800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I24501                 
089900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
090000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
090100                              ALT1-KDMFSFOR                               
090200                              2109-KDMFSFOR                               
090300     ELSE                                                                 
090400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I24501                  
090500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
090600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
090700                              ALT1-KDMFSFOR                               
090800                              2109-KDMFSFOR                               
090900     END-IF                                                               
091000                                                                          
091100     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
091200     MOVE MSG-IDPFK TO MFS-IDPFK                                          
091300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
091400                                                                          
091500     MOVE LOW-VALUE TO MSG-AREA                                           
091600     MOVE 'W4O24501' TO MFS-IDMOD                                         
091700     MOVE '4245' TO MOD-IDTRANS                                           
091800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
091900                                                                          
092000     IF MFS-UPDATE                                                        
092100       INITIALIZE AVSR-W413AVSR                                           
092200     END-IF                                                               
092300                                                                          
092400     IF NOT EGEN-MID                                                      
092500       MOVE MFS-RENSA-FAELT TO MID-IDDISTR-IN                             
092600                               MID-IDKUNDNR-IN                            
092700                               MID-IDKUNDRF-IN                            
092800                               MID-IDARTNR-IN                             
092900                               MID-IDDC-IN                                
093000                               MID-IDDISTR-UT                             
093100                               MID-IDKUNDNR-UT                            
093200                               MID-IDKUNDRF-UT                            
093300                               MID-IDARTNR-UT                             
093400                               MID-IDDC-UT                                
093500       MOVE SPACE TO MFS-KDTRTYP                                          
093600       MOVE '7' TO MFS-IDPFK                                              
093700     END-IF                                                               
093800                                                                          
093900     IF ENGLISH-TEXT                                                      
094000       MOVE +2 TO SPRAK-IX                                                
094100       MOVE 'GB ' TO MED-IDSKYLT                                          
094200     ELSE                                                                 
094300       MOVE +1 TO SPRAK-IX                                                
094400       MOVE 'S  ' TO MED-IDSKYLT                                          
094500     END-IF                                                               
094600                                                                          
094700     ACCEPT WS-TINUDAT FROM DATE                                          
094800     ACCEPT WS-TINUTID FROM TIME                                          
094900                                                                          
095000     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSFEL MOD-TEMFSINF              
095100     MOVE SPACE                 TO 2109-MID2-W2I10902                     
095200     MOVE +1                    TO 2109-INDX                              
095300     .                                                                    
095400     EJECT                                                                
095500 B-KOLLA-BEHOERIGHET SECTION.                                             
095600                                                                          
095700     MOVE 'STA B-KOLLA   '                TO   WS-PGM-POSITION            
095800     MOVE MSG-SIGNON-USERID TO SEC-IDUSER                                 
095900     MOVE '4245'            TO SEC-IDTRANS                                
096000     MOVE WS-IDDISTR        TO SEC-IDKEY                                  
096100                                                                          
096200     CALL WSECURIT USING       SEC-IDUSER                                 
096300                               SEC-IDTRANS                                
096400                               SEC-IDKEY                                  
096500                               SEC-KDSVAR                                 
096600     .                                                                    
096700     EJECT                                                                
096800 C-KOLLA-NYCKLAR SECTION.                                                 
096900                                                                          
097000     MOVE 'STA C-KOLLA   '                TO   WS-PGM-POSITION            
097100     MOVE JA TO NYCKLAR-SW                                                
097200     MOVE NEJ TO IDARTNR-SW                                               
097300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
097400     MOVE '001'             TO MSGI-KDCALL                                
097500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
097600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
097700     MOVE '4245'            TO MSGI-IDTRANS                               
097800     IF MFS-IDTRANS = '4245'                                              
097900        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
098000        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
098100                                                                          
098200        MOVE MID-IDKUNDRF-IN TO WS-IDKUNDRF-1-7                           
098300        IF WS-IDKUNDRF-1-7 = ALL '+'                                      
098400          MOVE '+++'         TO WS-IDKUNDRF-8-10                          
098500        ELSE                                                              
098600          MOVE SPACE         TO WS-IDKUNDRF-8-10                          
098700        END-IF                                                            
098800        MOVE WS-IDKUNDRF-RED TO MSGI-IDKUNDRF                             
098900        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
099000     END-IF                                                               
099100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
099200     MOVE LOW-VALUE TO       W-IDORDER-X                                  
099300                             W-WDQ2CSEQ-X                                 
099400                             W-ORAD-IDLOPNR-MIN-X                         
099500                             W-ORAD-IDARTNR-MIN-X                         
099600                             W-WDQ301KY-MIN-X                             
099700                             W-WDQ401KY-MIN-X                             
099800                             W-WDQ401KY-MIN-MIN-X                         
099900                             W-WDQ401KY-UNIK-X                            
100000                             W-WDQ101KY-MIN-X                             
100100                             W-WDD3BSEQ-X                                 
100200                             W-WDA501KY-X                                 
100300                             W-IDSKYLT-X                                  
100400                             W-IDARTNR-X                                  
100500                             W-WDE411-X                                   
100600                                                                          
100700     MOVE HIGH-VALUE TO      W-WDQ301KY-MAX-X                             
100800                             W-WDQ401KY-MAX-X                             
100900                             W-WDQ401KY-MAX-MAX-X                         
101000                             W-ORAD-IDLOPNR-MAX-X                         
101100                             W-ORAD-IDARTNR-MAX-X                         
101200                             W-WDQ101KY-MAX-X                             
101300                                                                          
101400     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
101500                             MOD-IDKUNDNR-IN                              
101600                             MOD-IDKUNDRF-IN                              
101700                             MOD-IDARTNR-IN                               
101800                             MOD-IDDC-IN                                  
101900                                                                          
102000     PERFORM CA-KOLLA-DISTRIKT                                            
102100     PERFORM CB-KOLLA-KUNDNUMMER                                          
102200     PERFORM CC-KOLLA-ORDERNUMMER                                         
102300     PERFORM CD-KOLLA-IDDC                                                
102400                                                                          
102500     IF IFYLLT-OK                                                         
102600       PERFORM CE-KOLLA-ARTIKELNUMMER                                     
102700     END-IF                                                               
102800                                                                          
102900     MOVE WS-IDDISTR TO MOD-IDDISTR-UT                                    
103000     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
103100     MOVE WS-IDKUNDNR    TO MOD-IDKUNDNR-UT                               
103200     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
103300     IF MOD-IDKUNDNR-UT = ALL SPACE                                       
103400       MOVE '     0' TO MOD-IDKUNDNR-UT                                   
103500     END-IF                                                               
103600     MOVE WS-IDKUNDRF    TO MOD-IDKUNDRF-UT                               
103700     INSPECT MOD-IDKUNDRF-UT REPLACING LEADING ZERO BY SPACE              
103800     MOVE WS-IDDC        TO MOD-IDDC-UT                                   
103900     IF WS-IDARTNR NOT = ZERO                                             
104000       MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
104100       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
104200     END-IF                                                               
104300     IF NYCKLAR-FEL                                                       
104400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
104500       CALL WMEDKONV USING MED-WMEDAREA                                   
104600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
104700       PERFORM MFS-RENSA-FAELT-IN                                         
104800       PERFORM MFS-RENSA-FAELT-UT                                         
104900       MOVE MFS-RENSA-FAELT  TO MOD-KVRADER                               
105000                                MOD-KVORDRAD                              
105100     END-IF                                                               
105200     .                                                                    
105300     EJECT                                                                
105400 CA-KOLLA-DISTRIKT SECTION.                                               
105500                                                                          
105600     MOVE MSGI-IDDISTR   TO WS-IDDISTR                                    
105700     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
105800                                                                          
105900     IF WS-IDDISTR = ZERO                                                 
106000       MOVE NEJ TO IFYLLT-SW                                              
106100     END-IF                                                               
106200                                                                          
106300     IF MID-IDDISTR-IN = ALL '+'                                          
106400       CONTINUE                                                           
106500     ELSE                                                                 
106600       MOVE '7'         TO MFS-IDPFK                                      
106700       MOVE SPACE       TO MFS-KDTRTYP                                    
106800     END-IF                                                               
106900                                                                          
107000     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
107100       MOVE WS-IDDISTR TO WS-IDDISTR-NUM                                  
107200       MOVE WS-IDDISTR-NUM TO W-Q2CSEQ-IDDISTR                            
107300     ELSE                                                                 
107400       MOVE NEJ TO NYCKLAR-SW                                             
107500     END-IF                                                               
107600     .                                                                    
107700     EJECT                                                                
107800 CB-KOLLA-KUNDNUMMER SECTION.                                             
107900                                                                          
108000     MOVE MSGI-IDKUNDNR    TO WS-IDKUNDNR                                 
108100     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
108200                                                                          
108300                                                                          
108400     IF MID-IDKUNDNR-IN = ALL '+'                                         
108500       CONTINUE                                                           
108600     ELSE                                                                 
108700       MOVE '7'         TO MFS-IDPFK                                      
108800       MOVE SPACE       TO MFS-KDTRTYP                                    
108900     END-IF                                                               
109000                                                                          
109100     IF WS-IDKUNDNR NUMERIC                                               
109200       MOVE WS-IDKUNDNR TO WS-IDKUNDNR-NUM                                
109300       MOVE WS-IDKUNDNR-NUM TO W-Q2CSEQ-IDKUNDNR                          
109400     ELSE                                                                 
109500       MOVE NEJ TO NYCKLAR-SW                                             
109600     END-IF                                                               
109700     .                                                                    
109800     EJECT                                                                
109900 CC-KOLLA-ORDERNUMMER SECTION.                                            
110000                                                                          
110100     MOVE MSGI-IDKUNDRF    TO WS-IDKUNDRF                                 
110200     INSPECT WS-IDKUNDRF REPLACING LEADING SPACE BY ZERO                  
110300                                                                          
110400     IF WS-IDKUNDRF = ZERO                                                
110500       MOVE NEJ TO IFYLLT-SW                                              
110600     END-IF                                                               
110700                                                                          
110800     IF MID-IDKUNDRF-IN = ALL '+'                                         
110900       CONTINUE                                                           
111000     ELSE                                                                 
111100       MOVE '7'         TO MFS-IDPFK                                      
111200       MOVE SPACE       TO MFS-KDTRTYP                                    
111300     END-IF                                                               
111400                                                                          
111500     IF WS-IDKUNDRF NUMERIC AND WS-IDKUNDRF > ZERO                        
111600       MOVE WS-IDKUNDRF TO WS-IDKUNDRF-NUM                                
111700       MOVE WS-IDKUNDRF-NUM TO W-Q2CSEQ-IDKUNDRF                          
111800     ELSE                                                                 
111900       MOVE NEJ TO NYCKLAR-SW                                             
112000     END-IF                                                               
112100     .                                                                    
112200     EJECT                                                                
112300 CD-KOLLA-IDDC   SECTION.                                                 
112400                                                                          
112500     IF EGEN-MID                                                          
112600                                                                          
112700       IF MID-IDDC-IN = ALL '+'                                           
112800         IF MID-IDDC-UT = SPACE                                           
112900           MOVE NEJ TO NYCKLAR-SW                                         
113000         ELSE                                                             
113100           MOVE MID-IDDC-UT TO WS-IDDC                                    
113200         END-IF                                                           
113300       ELSE                                                               
113400         MOVE MID-IDDC-IN TO WS-IDDC                                      
113500         MOVE '7'         TO MFS-IDPFK                                    
113600         MOVE SPACE       TO MFS-KDTRTYP                                  
113700       END-IF                                                             
113800                                                                          
113900     ELSE                                                                 
114000       MOVE MSGI-IDDC          TO WS-IDDC                                 
114100     END-IF                                                               
114200                                                                          
114300     IF WS-IDDC NOT = W-IDDC-B6                                           
114400        MOVE WS-IDDC TO W-IDDC-B6                                         
114500        PERFORM IMS-GU-WDB601                                             
114600     END-IF                                                               
114700     IF DCS-KDDC = SPACE                                                  
114800       MOVE NEJ TO NYCKLAR-SW                                             
114900     ELSE                                                                 
115000       MOVE WS-IDDC         TO W-ORAD-IDDC-MIN                            
115100                               W-ORAD-IDDC-MAX                            
115200                               W-ORAD-IDDC-MIN-MIN                        
115300                               W-ORAD-IDDC-MAX-MAX                        
115400                               W-ORAD-IDDC-UNIK                           
115500                               W-IDDC                                     
115600     END-IF                                                               
115700     .                                                                    
115800     EJECT                                                                
115900 CE-KOLLA-ARTIKELNUMMER SECTION.                                          
116000                                                                          
116100     IF EGEN-MID                                                          
116200                                                                          
116300       IF MID-IDARTNR-IN = ALL '+'                                        
116400         MOVE MID-IDARTNR-UT TO WS-IDARTNR                                
116500         INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO               
116600       ELSE                                                               
116700         MOVE MID-IDARTNR-IN TO WS-IDARTNR                                
116800         MOVE '7'         TO MFS-IDPFK                                    
116900         MOVE SPACE       TO MFS-KDTRTYP                                  
117000       END-IF                                                             
117100                                                                          
117200     ELSE                                                                 
117300       MOVE ZERO          TO WS-IDARTNR                                   
117400     END-IF                                                               
117500                                                                          
117600     IF WS-IDARTNR NUMERIC                                                
117700       IF WS-IDARTNR = ZERO                                               
117800         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                           
117900         MOVE NEJ TO IDARTNR-SW                                           
118000       ELSE                                                               
118100         MOVE WS-IDARTNR TO WS-IDARTNR-NUM                                
118200         MOVE WS-IDARTNR-NUM TO W-ORAD-IDARTNR-MIN                        
118300                                W-ORAD-IDARTNR-MAX                        
118400                                W-IDARTNR                                 
118500         MOVE JA TO IDARTNR-SW                                            
118600       END-IF                                                             
118700     ELSE                                                                 
118800       MOVE NEJ TO NYCKLAR-SW                                             
118900       MOVE WS-IDARTNR     TO MOD-IDARTNR-UT                              
119000     END-IF                                                               
119100     .                                                                    
119200     EJECT                                                                
119300 D-LAES-IN-ORDER SECTION.                                                 
119400     MOVE 'STA D-LAES    '                TO   WS-PGM-POSITION            
119500     MOVE JA TO ALLT-SW                                                   
119600                                                                          
119700     PERFORM IMS-GU-ORQI-WDQ201                                           
119800                                                                          
119900     IF SEGMENT-FINNS                                                     
120000                                                                          
120100       IF OHUV-FLKLAR = JA                                                
120200                                                                          
120300         MOVE OHUV-IDORDER TO  W-OHUV-IDORDER                             
120400                                 W-ORAD-IDORDER-MIN                       
120500                                 W-ORAD-IDORDER-MAX                       
120600                                 W-ORAD-IDORDER-MIN-MIN                   
120700                                 W-ORAD-IDORDER-MAX-MAX                   
120800                                 W-ORAD-IDORDER-UNIK                      
120900                                 W-ODEL-IDORDER-MIN                       
121000                                 W-ODEL-IDORDER-MAX                       
121100                                 WS-IDORDER                               
121200                                                                          
121300         PERFORM DB-KOLLA-ORDERTYP                                        
121400                                                                          
121500         IF ALLT-OK                                                       
121600           IF WS-IDDC NOT = W-IDDC-B6                                     
121700              MOVE WS-IDDC TO W-IDDC-B6                                   
121800              PERFORM IMS-GU-WDB601                                       
121900           END-IF                                                         
122000           IF DCS-KDDC = SPACE OR DCS-DDC                                 
122100             PERFORM S12-RAEKNA-RADER-DDGS                                
122200             IF MFS-UPDATE                                                
122300               CONTINUE                                                   
122400             ELSE                                                         
122500               PERFORM S14-CHECK-IF-SENT                                  
122600               PERFORM S16-CHECK-IF-SUPP-REJ                              
122700             END-IF                                                       
122800           ELSE                                                           
122900             PERFORM S01-RAEKNA-TOTALA-RADER                              
123000           END-IF                                                         
123100         END-IF                                                           
123200                                                                          
123300         IF ALLT-OK AND (DCS-KDDC NOT = SPACE AND NOT DCS-DDC)            
123400           PERFORM S02-KOLLA-ARBETSTABELL                                 
123500         END-IF                                                           
123600       ELSE                                                               
123700         MOVE ERR-ORDER-EJ-AVSLUTAD TO MED-IDMFSFEL                       
123800         CALL WMEDKONV USING MED-WMEDAREA                                 
123900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
124000         MOVE NEJ TO ALLT-SW                                              
124100       END-IF                                                             
124200                                                                          
124300     ELSE                                                                 
124400       MOVE ERR-ORDER-MISSING TO MED-IDMFSFEL                             
124500       CALL WMEDKONV USING MED-WMEDAREA                                   
124600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
124700       MOVE NEJ TO ALLT-SW                                                
124800     END-IF                                                               
124900     .                                                                    
125000     EJECT                                                                
125100 DB-KOLLA-ORDERTYP SECTION.                                               
125200                                                                          
125300     IF OHUV-KDTPOTYP > 0                                                 
125400       MOVE ERR-ANNULL-NOT-POSS TO MED-IDMFSFEL                           
125500       CALL WMEDKONV USING MED-WMEDAREA                                   
125600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
125700       PERFORM MFS-RENSA-FAELT-UT                                         
125800       MOVE MFS-RENSA-FAELT  TO MOD-KVRADER                               
125900                                MOD-KVORDRAD                              
126000       MOVE NEJ TO ALLT-SW                                                
126100     END-IF                                                               
126200     .                                                                    
126300     EJECT                                                                
126400 E-FOERSTA-SIDA SECTION.                                                  
126500                                                                          
126600     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
126700     CALL WMEDKONV USING MED-WMEDAREA                                     
126800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
126900                                                                          
127000*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
127100     MOVE ZERO       TO MOD-IDORDER-ENTER                                 
127200                        MOD-IDORDER-NEXT                                  
127300                        MOD-IDARTNR-ENTER                                 
127400                        MOD-IDARTNR-NEXT                                  
127500                        MOD-IDDC-ENTER                                    
127600                        MOD-IDDC-NEXT                                     
127700                        MOD-ADLAGOMR-ENTER                                
127800                        MOD-ADLAGOMR-NEXT                                 
127900                        MOD-ADGANG-ENTER                                  
128000                        MOD-ADGANG-NEXT                                   
128100                        MOD-ADPLATS-ENTER                                 
128200                        MOD-ADPLATS-NEXT                                  
128300                        MOD-IDLOPNR-ENTER                                 
128400                        MOD-IDLOPNR-NEXT                                  
128500                        MOD-IDPRODNR-ENTER                                
128600                        MOD-IDPRODNR-NEXT                                 
128700                        MOD-IDPLKLST-ENTER                                
128800                        MOD-IDPLKLST-NEXT                                 
128900                        MOD-IDPURAD-ENTER                                 
129000                        MOD-IDPURAD-NEXT                                  
129100     MOVE +1 TO INDX                                                      
129200     PERFORM UNTIL INDX > MAX-INDX                                        
129300       MOVE ZERO TO MOD-IDARTNR-SPAR(INDX)                                
129400                    MOD-IDLOPNR-SPAR(INDX)                                
129500                    MOD-ADLAGOMR-SPAR(INDX)                               
129600                    MOD-ADGANG-SPAR(INDX)                                 
129700                    MOD-ADPLATS-SPAR(INDX)                                
129800                    MOD-IDPRODNR-SPAR(INDX)                               
129900                    MOD-IDPLKLST-SPAR(INDX)                               
130000                    MOD-IDPURAD-SPAR(INDX)                                
130100       ADD +1 TO INDX                                                     
130200     END-PERFORM                                                          
130300     .                                                                    
130400     EJECT                                                                
130500 F-NAESTA-SIDA SECTION.                                                   
130600                                                                          
130700     MOVE MID-IDARTNR-NEXT   TO W-ORAD-IDARTNR-MIN                        
130800                                W-ORAD-IDARTNR-MAX                        
130900                                W-ORAD-IDARTNR-MIN-MIN                    
131000     MOVE MID-IDORDER-NEXT   TO W-ORAD-IDORDER-MIN                        
131100                                W-ORAD-IDORDER-MIN-MIN                    
131200                                W-ORAD-IDORDER-MAX                        
131300                                W-ORAD-IDORDER-MAX-MAX                    
131400                                W-ODEL-IDORDER-MIN                        
131500                                W-ODEL-IDORDER-MAX                        
131600     MOVE MID-IDDC-NEXT      TO W-ORAD-IDDC-MIN                           
131700                                W-ORAD-IDDC-MAX                           
131800                                W-ODEL-IDDC-MIN                           
131900                                W-ODEL-IDDC-MAX                           
132000                                W-ORAD-IDDC-MIN-MIN                       
132100                                W-ORAD-IDDC-MAX-MAX                       
132200     MOVE MID-ADLAGOMR-NEXT  TO W-ORAD-ADLAGOMR-MIN                       
132300                                W-ORAD-ADLAGOMR-MIN-MIN                   
132400     MOVE MID-ADGANG-NEXT    TO W-ORAD-ADGANG-MIN                         
132500                                W-ORAD-ADGANG-MIN-MIN                     
132600     MOVE MID-ADPLATS-NEXT   TO W-ORAD-ADPLATS-MIN                        
132700                                W-ORAD-ADPLATS-MIN-MIN                    
132800     MOVE MID-IDLOPNR-NEXT   TO W-ORAD-IDLOPNR-MIN                        
132900                                W-ORAD-IDLOPNR-MIN-MIN                    
133000     MOVE MID-IDPRODNR-NEXT  TO W-401-IDPRODNR                            
133100                                W-ODEL-IDPRODNR-MIN                       
133200     MOVE MID-IDPLKLST-NEXT  TO W-401-IDPLKLST                            
133300                                W-ODEL-IDPLKLST-MIN                       
133400     MOVE MID-IDPURAD-NEXT   TO W-411-IDPURAD                             
133500     .                                                                    
133600     EJECT                                                                
133700 G-SAMMA-SIDA SECTION.                                                    
133800                                                                          
133900     MOVE MID-IDARTNR-ENTER  TO W-ORAD-IDARTNR-MIN                        
134000                                W-ORAD-IDARTNR-MAX                        
134100                                W-ORAD-IDARTNR-MIN-MIN                    
134200     MOVE MID-IDORDER-ENTER  TO W-ORAD-IDORDER-MIN                        
134300                                W-ORAD-IDORDER-MIN-MIN                    
134400                                W-ORAD-IDORDER-MAX                        
134500                                W-ORAD-IDORDER-MAX-MAX                    
134600                                W-ODEL-IDORDER-MIN                        
134700                                W-ODEL-IDORDER-MAX                        
134800     MOVE MID-IDDC-ENTER     TO W-ORAD-IDDC-MIN                           
134900                                W-ORAD-IDDC-MAX                           
135000                                W-ORAD-IDDC-MIN-MIN                       
135100                                W-ORAD-IDDC-MAX-MAX                       
135200                                W-ODEL-IDDC-MIN                           
135300                                W-ODEL-IDDC-MAX                           
135400     MOVE MID-ADLAGOMR-ENTER TO W-ORAD-ADLAGOMR-MIN                       
135500                                W-ORAD-ADLAGOMR-MIN-MIN                   
135600     MOVE MID-ADGANG-ENTER   TO W-ORAD-ADGANG-MIN                         
135700                                W-ORAD-ADGANG-MIN-MIN                     
135800     MOVE MID-ADPLATS-ENTER  TO W-ORAD-ADPLATS-MIN                        
135900                                W-ORAD-ADPLATS-MIN-MIN                    
136000     MOVE MID-IDLOPNR-ENTER  TO W-ORAD-IDLOPNR-MIN                        
136100                                W-ORAD-IDLOPNR-MIN-MIN                    
136200     MOVE MID-IDPRODNR-ENTER TO W-401-IDPRODNR                            
136300                                W-ODEL-IDPRODNR-MIN                       
136400     MOVE MID-IDPLKLST-ENTER TO W-401-IDPLKLST                            
136500                                W-ODEL-IDPLKLST-MIN                       
136600     MOVE MID-IDPURAD-ENTER  TO W-411-IDPURAD                             
136700                                                                          
136800     IF MID-INPUT NOT = ALL '+'                                           
136900       MOVE NEJ TO INDATA-SW                                              
137000       MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                                
137100       CALL WMEDKONV USING MED-WMEDAREA                                   
137200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
137300       PERFORM MFS-ROR-EJ-FAELT-IN                                        
137400       PERFORM MFS-ROR-EJ-FAELT-UT                                        
137500       PERFORM MFS-LAS-IN-IGEN                                            
137600     END-IF                                                               
137700     .                                                                    
137800     EJECT                                                                
137900 H-LAES-VISA-INFO SECTION.                                                
138000                                                                          
138100     MOVE 'STA H-LAES    '                TO   WS-PGM-POSITION            
138200     MOVE +1 TO INDX                                                      
138300                                                                          
138400     IF IDARTNR-IFYLLT                                                    
138500       PERFORM IMS-GU-ORQF-WDQ401-M-IDARTNR                               
138600     ELSE                                                                 
138700       PERFORM IMS-GU-ORQF-WDQ401                                         
138800     END-IF                                                               
138900                                                                          
139000     IF SEGMENT-FINNS                                                     
139100                                                                          
139200       PERFORM HA-SPAR-WDQ401KY-ENTER                                     
139300       PERFORM HB-SPAR-WDQ401KY-NEXT                                      
139400                                                                          
139500       PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                    
139600                                                                          
139700         PERFORM HC-FLYTTA-TILL-MOD                                       
139800                                                                          
139900         MOVE ORAD-IDARTNR TO W-WDD3-IDARTNR                              
140000         MOVE MED-IDSKYLT TO W-TEXT-IDSKYLT                               
140100         PERFORM S15-LAS-BENAEMNING                                       
140200                                                                          
140300         ADD +1 TO INDX                                                   
140400                                                                          
140500         IF IDARTNR-IFYLLT                                                
140600           PERFORM IMS-GN-ORQF-WDQ401-M-IDARTNR                           
140700         ELSE                                                             
140800           PERFORM IMS-GN-ORQF-WDQ401                                     
140900         END-IF                                                           
141000                                                                          
141100       END-PERFORM                                                        
141200                                                                          
141300       IF SEGMENT-FINNS                                                   
141400                                                                          
141500         PERFORM HB-SPAR-WDQ401KY-NEXT                                    
141600                                                                          
141700         IF NOT MFS-UPDATE                                                
141800           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
141900           CALL WMEDKONV USING MED-WMEDAREA                               
142000           MOVE MED-MFSINF TO MOD-TEMFSINF                                
142100         END-IF                                                           
142200                                                                          
142300       ELSE                                                               
142400         PERFORM UNTIL INDX > MAX-INDX                                    
142500           MOVE MFS-STAENG-FAELT-OSYNLIGT TO                              
142600                                 MOD-CMD-UPDATE-ATTR(INDX)                
142700           MOVE MFS-STAENG-FAELT-OSYNLIGT TO                              
142800                                 MOD-KVBEART-Q-UPDATE-ATTR(INDX)          
142900           MOVE ZERO TO MOD-IDARTNR-SPAR(INDX)                            
143000                        MOD-IDLOPNR-SPAR(INDX)                            
143100                        MOD-ADLAGOMR-SPAR(INDX)                           
143200                        MOD-ADGANG-SPAR(INDX)                             
143300                        MOD-ADPLATS-SPAR(INDX)                            
143400           ADD +1 TO INDX                                                 
143500         END-PERFORM                                                      
143600       END-IF                                                             
143700                                                                          
143800       PERFORM MFS-RENSA-FAELT-IN                                         
143900                                                                          
144000     ELSE                                                                 
144100       IF IDARTNR-IFYLLT                                                  
144200         MOVE INF-PART-MISSING TO MED-IDMFSINF                            
144300         CALL WMEDKONV USING MED-WMEDAREA                                 
144400         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
144500       ELSE                                                               
144600         MOVE ERR-LINES-MISSING TO MED-IDMFSFEL                           
144700         CALL WMEDKONV USING MED-WMEDAREA                                 
144800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
144900       END-IF                                                             
145000                                                                          
145100       PERFORM MFS-RENSA-FAELT-IN                                         
145200       PERFORM MFS-RENSA-FAELT-UT                                         
145300       MOVE MFS-RENSA-FAELT  TO MOD-KVRADER                               
145400                                MOD-KVORDRAD                              
145500                                                                          
145600     END-IF                                                               
145700     .                                                                    
145800     EJECT                                                                
145900 HA-SPAR-WDQ401KY-ENTER SECTION.                                          
146000                                                                          
146100     MOVE ORAD-IDORDER TO MOD-IDORDER-ENTER                               
146200     MOVE ORAD-IDDC    TO MOD-IDDC-ENTER                                  
146300     MOVE ORAD-ADLAGOMR TO MOD-ADLAGOMR-ENTER                             
146400     MOVE ORAD-ADGANG    TO MOD-ADGANG-ENTER                              
146500     MOVE ORAD-ADPLATS TO MOD-ADPLATS-ENTER                               
146600     MOVE ORAD-IDARTNR TO MOD-IDARTNR-ENTER                               
146700     MOVE ORAD-IDLOPNR TO MOD-IDLOPNR-ENTER                               
146800     .                                                                    
146900     EJECT                                                                
147000 HB-SPAR-WDQ401KY-NEXT SECTION.                                           
147100                                                                          
147200     MOVE ORAD-IDORDER TO MOD-IDORDER-NEXT                                
147300     MOVE ORAD-IDDC    TO MOD-IDDC-NEXT                                   
147400     MOVE ORAD-ADLAGOMR TO MOD-ADLAGOMR-NEXT                              
147500     MOVE ORAD-ADGANG TO MOD-ADGANG-NEXT                                  
147600     MOVE ORAD-ADPLATS TO MOD-ADPLATS-NEXT                                
147700     MOVE ORAD-IDARTNR TO MOD-IDARTNR-NEXT                                
147800     MOVE ORAD-IDLOPNR TO MOD-IDLOPNR-NEXT                                
147900     .                                                                    
148000     EJECT                                                                
148100 HC-FLYTTA-TILL-MOD SECTION.                                              
148200                                                                          
148300     MOVE ORAD-IDARTNR TO MOD-IDARTNR-RAD(INDX)                           
148400     MOVE ORAD-IDARTNR TO MOD-IDARTNR-SPAR(INDX)                          
148500     MOVE ORAD-IDLOPNR TO MOD-IDLOPNR-SPAR(INDX)                          
148600     MOVE ORAD-ADLAGOMR TO MOD-ADLAGOMR-SPAR(INDX)                        
148700     MOVE ORAD-ADGANG  TO MOD-ADGANG-SPAR(INDX)                           
148800     MOVE ORAD-ADPLATS TO MOD-ADPLATS-SPAR(INDX)                          
148900     INSPECT MOD-IDARTNR-RAD(INDX) REPLACING LEADING ZERO                 
149000                                   BY SPACE                               
149100     MOVE ORAD-KVBEART-Q TO MOD-KVBEART-Q-RAD(INDX)                       
149200     MOVE ORAD-IDDC      TO MOD-IDDC-RAD(INDX)                            
149300                                                                          
149400     IF ORAD-IDKUNDRF-RO NOT = '0000000   '                               
149500       MOVE ORAD-IDKUNDRF-RO (1:7) TO MOD-OREF-RAD(INDX)                  
149600       INSPECT MOD-OREF-RAD(INDX) REPLACING LEADING ZERO                  
149700                                   BY SPACE                               
149800       MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                  
149900                                MOD-KVBEART-Q-UPDATE-ATTR(INDX)           
150000     ELSE                                                                 
150100       MOVE MFS-RENSA-FAELT TO MOD-OREF-RAD(INDX)                         
150200     END-IF                                                               
150300     .                                                                    
150400     EJECT                                                                
150500 I-KOLLA-INPUT SECTION.                                                   
150600                                                                          
150700     MOVE 'STA I-KOLLA   '                TO   WS-PGM-POSITION            
150800     MOVE JA  TO INDATA-SW                                                
150900     MOVE NEJ TO PF11-SW                                                  
151000     MOVE NEJ TO REQUEST-SW                                               
151100                 SUPPLIER-REJECTED-SW                                     
151200                                                                          
151300     IF MID-INPUT = ALL '+'                                               
151400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
151500       CALL WMEDKONV USING MED-WMEDAREA                                   
151600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
151700     ELSE                                                                 
151800                                                                          
151900       IF MID-FLAGGA-UPDATE NOT = ALL '+'                                 
152000                                                                          
152100         IF MID-FLAGGA-UPDATE = JA OR                                     
152200           MID-FLAGGA-UPDATE = YES OR                                     
152300           MID-FLAGGA-UPDATE = NEJ                                        
152400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAGGA-UPDATE-ATTR            
152500         ELSE                                                             
152600           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAGGA-UPDATE-ATTR              
152700           MOVE NEJ TO INDATA-SW                                          
152800         END-IF                                                           
152900                                                                          
153000       END-IF                                                             
153100                                                                          
153200       MOVE +1 TO INDX                                                    
153300                                                                          
153400       PERFORM UNTIL INDX > MAX-INDX OR                                   
153500         MID-IDARTNR-SPAR(INDX) = ZERO                                    
153600                                                                          
153700        IF MID-CMD-UPDATE(INDX) NOT = ALL '+'                             
153800          IF WS-IDDC NOT = W-IDDC-B6                                      
153900             MOVE WS-IDDC TO W-IDDC-B6                                    
154000             PERFORM IMS-GU-WDB601                                        
154100          END-IF                                                          
154200          IF DCS-DDC                                                      
154300            IF MID-CMD-UPDATE(INDX) = 'D'                                 
154400                                                                          
154500              IF MFS-UPD-V                                                
154600                MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-UPDATE-ATTR          
154700                                                           (INDX)         
154800              ELSE                                                        
154900                MOVE ODEL-IDDISTR            TO W-401-IDDISTR             
155000                MOVE ODEL-IDKUNDNR           TO W-401-IDKUNDNR            
155100                MOVE ODEL-IDKUNDRF(3:5)      TO W-401-IDORDNR             
155200                MOVE MID-IDPRODNR-SPAR(INDX) TO W-401-IDPRODNR            
155300                MOVE MID-IDPLKLST-SPAR(INDX) TO W-401-IDPLKLST            
155400                MOVE MID-IDPURAD-SPAR(INDX)  TO W-411-IDPURAD             
155500                PERFORM IMS-GU-WDE411                                     
155600                IF E4-ORAD-KDANNULL = '0'                                 
155700                  MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-UPDATE-ATTR        
155800                                                           (INDX)         
155900                ELSE                                                      
156000*                 *SUPPLIER REJECTED CANCELLATION.CANNOT DO D+PF11        
156100                 IF E4-ORAD-KDANNULL = '2'                                
156200                   MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR         
156300                                                           (INDX)         
156400                   MOVE NEJ TO INDATA-SW                                  
156500                   MOVE JA TO SUPPLIER-REJECTED-SW                        
156600                 ELSE                                                     
156700*                 *CANCELLATION REQUEST IS ALREADY SENT                   
156800                  MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR          
156900                                                           (INDX)         
157000                  MOVE NEJ TO INDATA-SW                                   
157100                  MOVE JA TO REQUEST-SW                                   
157200                 END-IF                                                   
157300                END-IF                                                    
157400              END-IF                                                      
157500                                                                          
157600              IF MFS-UPD-V                                                
157700                 PERFORM S23-CHECK-ORDER-PRINTED                          
157800                 IF ORDER-FINNS                                           
157900                  MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR          
158000                                                        (INDX)            
158100                  MOVE NEJ TO INDATA-SW                                   
158200                  MOVE JA  TO PF11-SW                                     
158300                 END-IF                                                   
158400              END-IF                                                      
158500            ELSE                                                          
158600              MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR              
158700                                                    (INDX)                
158800              MOVE MFS-STAENG-FAELT-OSYNLIGT TO                           
158900                                MOD-KVBEART-Q-UPDATE-ATTR(INDX)           
159000              MOVE NEJ TO INDATA-SW                                       
159100            END-IF                                                        
159200          ELSE                                                            
159300*           *NOT DDGS LINE                                                
159400            IF MFS-UPDATE                                                 
159500              IF MID-CMD-UPDATE(INDX) = 'D' OR                            
159600                MID-CMD-UPDATE(INDX) = 'X' OR                             
159700                MID-CMD-UPDATE(INDX) = 'A'                                
159800                MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-UPDATE-ATTR          
159900                                                           (INDX)         
160000              ELSE                                                        
160100                MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR            
160200                                                      (INDX)              
160300                MOVE NEJ TO INDATA-SW                                     
160400              END-IF                                                      
160500            ELSE                                                          
160600*             *PF23 IS ONLY ALLOWED FOR DDGS LINES                        
160700                MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR            
160800                                                      (INDX)              
160900                MOVE NEJ TO INDATA-SW                                     
161000                MOVE JA  TO PF11-SW                                       
161100            END-IF                                                        
161200          END-IF                                                          
161300                                                                          
161400        END-IF                                                            
161500                                                                          
161600        IF MID-KVBEART-Q-UPDATE(INDX) NOT = ALL '+'                       
161700                                                                          
161800         IF MID-KVBEART-Q-UPDATE(INDX) NOT NUMERIC                        
161900           MOVE MFS-NUM-FAELT-FEL TO MOD-KVBEART-Q-UPDATE-ATTR            
162000                                                        (INDX)            
162100           MOVE NEJ TO INDATA-SW                                          
162200         ELSE                                                             
162300           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVBEART-Q-UPDATE-ATTR          
162400                                                         (INDX)           
162500           MOVE MID-KVBEART-Q-UPDATE(INDX) TO WS-KVBEART-Q(INDX)          
162600         END-IF                                                           
162700                                                                          
162800        END-IF                                                            
162900        ADD +1 TO INDX                                                    
163000                                                                          
163100       END-PERFORM                                                        
163200                                                                          
163300       IF INDATA-FEL                                                      
163400         IF WS-IDDC NOT = W-IDDC-B6                                       
163500            MOVE WS-IDDC TO W-IDDC-B6                                     
163600            PERFORM IMS-GU-WDB601                                         
163700         END-IF                                                           
163800         IF DCS-DDC                                                       
163900           MOVE +1 TO INDX                                                
164000           PERFORM UNTIL INDX > MAX-INDX                                  
164100             MOVE MFS-STAENG-FAELT-OSYNLIGT TO                            
164200                                   MOD-KVBEART-Q-UPDATE-ATTR(INDX)        
164300             IF MID-IDARTNR-SPAR(INDX) = ZERO                             
164400               MOVE MFS-STAENG-FAELT-OSYNLIGT TO                          
164500                                   MOD-CMD-UPDATE-ATTR(INDX)              
164600             END-IF                                                       
164700             ADD +1 TO INDX                                               
164800           END-PERFORM                                                    
164900           MOVE MFS-STAENG-FAELT-OSYNLIGT TO                              
165000                                 MOD-FLAGGA-UPDATE-ATTR                   
165100         END-IF                                                           
165200         IF PRESS-PF11                                                    
165300           MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                            
165400           CALL WMEDKONV USING MED-WMEDAREA                               
165500           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
165600           PERFORM MFS-ROR-EJ-FAELT-UT                                    
165700           PERFORM MFS-ROR-EJ-FAELT-IN                                    
165800         ELSE                                                             
165900           IF REQUEST-SENT OR SUPPLIER-REJECTED                           
166000             IF REQUEST-SENT                                              
166100               PERFORM S14-CHECK-IF-SENT                                  
166200             ELSE                                                         
166300               MOVE ERR-ANNULL-NOT-POSS TO MED-IDMFSFEL                   
166400               CALL WMEDKONV USING MED-WMEDAREA                           
166500               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
166600             END-IF                                                       
166700             PERFORM MFS-ROR-EJ-FAELT-UT                                  
166800             PERFORM MFS-ROR-EJ-FAELT-IN                                  
166900           ELSE                                                           
167000             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
167100             CALL WMEDKONV USING MED-WMEDAREA                             
167200             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
167300             PERFORM MFS-ROR-EJ-FAELT-UT                                  
167400             PERFORM MFS-ROR-EJ-FAELT-IN                                  
167500           END-IF                                                         
167600         END-IF                                                           
167700       END-IF                                                             
167800                                                                          
167900     END-IF                                                               
168000     .                                                                    
168100     EJECT                                                                
168200 J-UPPDATERA SECTION.                                                     
168300                                                                          
168400     MOVE 'STA J-UPPDAT  '                TO   WS-PGM-POSITION            
168500                                                                          
168600     IF WS-IDDC NOT = W-IDDC-B6                                           
168700        MOVE WS-IDDC TO W-IDDC-B6                                         
168800        PERFORM IMS-GU-WDB601                                             
168900     END-IF                                                               
169000     IF MID-FLAGGA-UPDATE = 'J' OR 'Y' AND                                
169100       (DCS-KDDC NOT = SPACE AND NOT DCS-DDC)                             
169200                                                                          
169300       IF MOJLIG-ANTAL NOT = TOTAL-KVRADER                                
169400         MOVE ERR-ANNULL-NOT-POSS TO MED-IDMFSFEL                         
169500         CALL WMEDKONV USING MED-WMEDAREA                                 
169600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
169700         MOVE NEJ TO ALLT-SW                                              
169800       ELSE                                                               
169900         PERFORM JA-ANNULLERA-HEL-ORDER                                   
170000         PERFORM MFS-RENSA-FAELT-UT                                       
170100         MOVE MFS-RENSA-FAELT TO MOD-KVRADER                              
170200                                  MOD-KVORDRAD                            
170300       END-IF                                                             
170400     ELSE                                                                 
170500       IF DCS-KDDC NOT = SPACE AND NOT DCS-DDC                            
170600         PERFORM JB-UPPDATERA-EN-SIDA                                     
170700       ELSE                                                               
170800         IF MFS-UPDATE                                                    
170900           PERFORM JC-SKAPA-EDI-DGS                                       
171000         END-IF                                                           
171100         IF MFS-UPD-V                                                     
171200           PERFORM JD-DELETE-LINES                                        
171300         END-IF                                                           
171400       END-IF                                                             
171500     END-IF                                                               
171600                                                                          
171700     IF MID-INPUT NOT = ALL '+'                                           
171800       IF INDATA-OK                                                       
171900           IF ANNULL-HELORDER                                             
172000             MOVE INF-ORDER-DELETE TO MED-IDMFSINF                        
172100             CALL WMEDKONV USING MED-WMEDAREA                             
172200             MOVE MED-MFSINF TO MOD-TEMFSINF                              
172300             PERFORM MFS-FORM-ATTR                                        
172400             PERFORM MFS-RENSA-FAELT-IN                                   
172500             MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                         
172600           ELSE                                                           
172700             IF MID-FLAGGA-UPDATE = 'Y' OR 'J'                            
172800               PERFORM MFS-FORM-ATTR                                      
172900               PERFORM MFS-RENSA-FAELT-IN                                 
173000             ELSE                                                         
173100               IF RAD-AENDRING                                            
173200                 MOVE INF-UPDATE-DONE TO MED-IDMFSINF                     
173300                 CALL WMEDKONV USING MED-WMEDAREA                         
173400                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
173500                 PERFORM MFS-FORM-ATTR                                    
173600                 PERFORM MFS-RENSA-FAELT-IN                               
173700                 MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                     
173800               ELSE                                                       
173900                 MOVE INF-ORDERLINE-DELETE TO MED-IDMFSINF                
174000                 CALL WMEDKONV USING MED-WMEDAREA                         
174100                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
174200                 PERFORM MFS-FORM-ATTR                                    
174300                 PERFORM MFS-RENSA-FAELT-IN                               
174400                 MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                     
174500               END-IF                                                     
174600             END-IF                                                       
174700           END-IF                                                         
174800       END-IF                                                             
174900     END-IF                                                               
175000     .                                                                    
175100     EJECT                                                                
175200 JA-ANNULLERA-HEL-ORDER SECTION.                                          
175300                                                                          
175400     MOVE 'STA JA-ANNULL '                TO   WS-PGM-POSITION            
175500     MOVE JA TO ANNULL-SW                                                 
175600     PERFORM IMS-GHU-ORQI-WDQ201                                          
175700                                                                          
175800     IF SEGMENT-FINNS                                                     
175900       MOVE 'N' TO OHUV-FLKLAR                                            
176000       MOVE 'J' TO OHUV-FLBORT                                            
176100       PERFORM IMS-REPL-WDQ2                                              
176200     END-IF                                                               
176300                                                                          
176400     PERFORM S21-DELETE-PRICE-Q-ORDER                                     
176500     MOVE SPACE             TO  ALT1-MID                                  
176600     MOVE WS-IDORDER        TO  ALT1-MID-IDORDER                          
176700     PERFORM IMS-INSERT-ALT1MSG                                           
176800     .                                                                    
176900     EJECT                                                                
177000 JB-UPPDATERA-EN-SIDA SECTION.                                            
177100                                                                          
177200     MOVE 'STA JB-UPPDAT '                TO   WS-PGM-POSITION            
177300     MOVE +1 TO INDX                                                      
177400     MOVE +1 TO AVSR-INDX                                                 
177500     MOVE JA TO INDATA-SW                                                 
177600                CMD-SW                                                    
177700     MOVE NEJ TO AVSR-SW                                                  
177800     MOVE NEJ TO RAD-SW                                                   
177900                                                                          
178000     PERFORM JBA-KOLLA-Q4-RAD-MOT-INPUT                                   
178100                                                                          
178200     IF INDATA-OK                                                         
178300                                                                          
178400       PERFORM JBB-BEHANDLA-INPUT-UPDATE-Q4                               
178500                                                                          
178600     ELSE                                                                 
178700       PERFORM UNTIL INDX > MAX-INDX                                      
178800         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
178900                               MOD-CMD-UPDATE-ATTR(INDX)                  
179000         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
179100                               MOD-KVBEART-Q-UPDATE-ATTR(INDX)            
179200         ADD +1 TO INDX                                                   
179300       END-PERFORM                                                        
179400       IF CMD-FEL                                                         
179500         MOVE ERR-CMD-FEL TO MED-IDMFSFEL                                 
179600         CALL WMEDKONV USING MED-WMEDAREA                                 
179700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
179800       ELSE                                                               
179900         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
180000         CALL WMEDKONV USING MED-WMEDAREA                                 
180100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
180200       END-IF                                                             
180300       PERFORM MFS-ROR-EJ-FAELT-UT                                        
180400       PERFORM MFS-ROR-EJ-FAELT-IN                                        
180500     END-IF                                                               
180600     .                                                                    
180700     EJECT                                                                
180800 JBA-KOLLA-Q4-RAD-MOT-INPUT SECTION.                                      
180900                                                                          
181000     MOVE 'STA JBA-KOLLA '                TO   WS-PGM-POSITION            
181100     PERFORM UNTIL INDX > MAX-INDX OR                                     
181200       MID-IDARTNR-SPAR(INDX) = ZERO                                      
181300                                                                          
181400       MOVE MID-IDARTNR-SPAR(INDX)  TO  W-ORAD-IDARTNR-UNIK               
181500       MOVE MID-IDLOPNR-SPAR(INDX)  TO  W-ORAD-IDLOPNR-UNIK               
181600       MOVE MID-ADLAGOMR-SPAR(INDX) TO  W-ORAD-ADLAGOMR-UNIK              
181700       MOVE MID-ADGANG-SPAR(INDX)   TO  W-ORAD-ADGANG-UNIK                
181800       MOVE MID-ADPLATS-SPAR(INDX)  TO  W-ORAD-ADPLATS-UNIK               
181900                                                                          
182000       PERFORM IMS-GHU-ORQF-WDQ401                                        
182100                                                                          
182200       IF SEGMENT-FINNS                                                   
182300                                                                          
182400         IF MID-CMD-UPDATE(INDX) NOT = ALL '+'                            
182500                                                                          
182600           PERFORM JBAA-RAD-KOMMAND-IFYLLT                                
182700                                                                          
182800         ELSE                                                             
182900                                                                          
183000           PERFORM JBAB-RAD-KOMMAND-ALL-PLUS                              
183100                                                                          
183200         END-IF                                                           
183300       END-IF                                                             
183400       ADD +1 TO INDX                                                     
183500     END-PERFORM                                                          
183600     .                                                                    
183700     EJECT                                                                
183800 JBAA-RAD-KOMMAND-IFYLLT SECTION.                                         
183900                                                                          
184000     MOVE 'STA JBAA-RAD  '                TO   WS-PGM-POSITION            
184100     IF ORAD-IDKUNDRF-RO NOT = '0000000   ' AND                           
184200        ORAD-IDKUNDRF-RO NOT = ORAD-IDKUNDRF                              
184300       IF MID-CMD-UPDATE(INDX) NOT = 'X'                                  
184400         MOVE NEJ TO INDATA-SW                                            
184500                     CMD-SW                                               
184600         MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR                   
184700                                                (INDX)                    
184800         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
184900                      MOD-KVBEART-Q-UPDATE-ATTR(INDX)                     
185000                                                                          
185100       END-IF                                                             
185200     ELSE                                                                 
185300       IF ORAD-IDKUNDRF-RO = '0000000   ' OR                              
185400          ORAD-IDKUNDRF-RO = ORAD-IDKUNDRF                                
185500         IF MID-CMD-UPDATE(INDX) = 'X'                                    
185600           MOVE NEJ TO INDATA-SW                                          
185700           MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR                 
185800                                                  (INDX)                  
185900         ELSE                                                             
186000           IF MID-CMD-UPDATE(INDX) = 'D' AND                              
186100             MID-KVBEART-Q-UPDATE(INDX) NOT = ALL '+'                     
186200             MOVE NEJ TO INDATA-SW                                        
186300             MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-UPDATE-ATTR               
186400                                                      (INDX)              
186500             MOVE MFS-NUM-FAELT-FEL TO                                    
186600                             MOD-KVBEART-Q-UPDATE-ATTR(INDX)              
186700           ELSE                                                           
186800             IF MID-CMD-UPDATE(INDX) = 'A' AND                            
186900               MID-KVBEART-Q-UPDATE(INDX) NOT = ALL '+'                   
187000                IF ORAD-KVBEART-Q < WS-KVBEART-Q(INDX)                    
187100                  MOVE NEJ TO INDATA-SW                                   
187200                  MOVE MFS-NUM-FAELT-FEL TO                               
187300                             MOD-KVBEART-Q-UPDATE-ATTR(INDX)              
187400                END-IF                                                    
187500             ELSE                                                         
187600               IF MID-CMD-UPDATE(INDX) = 'A' AND                          
187700                 MID-KVBEART-Q-UPDATE(INDX) = ALL '+'                     
187800                 MOVE NEJ TO INDATA-SW                                    
187900                 MOVE MFS-NUM-FAELT-FEL TO                                
188000                            MOD-KVBEART-Q-UPDATE-ATTR(INDX)               
188100                 MOVE MFS-ALFA-FAELT-FEL TO                               
188200                            MOD-CMD-UPDATE-ATTR(INDX)                     
188300               END-IF                                                     
188400             END-IF                                                       
188500           END-IF                                                         
188600         END-IF                                                           
188700       END-IF                                                             
188800     END-IF                                                               
188900     .                                                                    
189000     EJECT                                                                
189100 JBAB-RAD-KOMMAND-ALL-PLUS SECTION.                                       
189200                                                                          
189300     MOVE 'STA JBAB-RAD  '                TO   WS-PGM-POSITION            
189400     IF ORAD-IDKUNDRF-RO NOT = '0000000   ' AND                           
189500        ORAD-IDKUNDRF-RO NOT = ORAD-IDKUNDRF                              
189600         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
189700                      MOD-KVBEART-Q-UPDATE-ATTR(INDX)                     
189800     END-IF                                                               
189900                                                                          
190000     IF MID-KVBEART-Q-UPDATE(INDX) NOT = ALL '+'                          
190100       MOVE NEJ TO INDATA-SW                                              
190200       MOVE MFS-ALFA-FAELT-FEL TO                                         
190300                  MOD-CMD-UPDATE-ATTR(INDX)                               
190400       MOVE MFS-NUM-FAELT-FEL TO                                          
190500                  MOD-KVBEART-Q-UPDATE-ATTR(INDX)                         
190600     END-IF                                                               
190700     .                                                                    
190800     EJECT                                                                
190900 JBB-BEHANDLA-INPUT-UPDATE-Q4 SECTION.                                    
191000                                                                          
191100     MOVE 'STA JBB-BEHANDLA '             TO   WS-PGM-POSITION            
191200     MOVE +1 TO INDX                                                      
191300     MOVE JA TO FIRST-TIME-SW                                             
191400                                                                          
191500     PERFORM IMS-GHU-ORQI-WDQ201                                          
191600                                                                          
191700     IF SEGMENT-FINNS                                                     
191800       MOVE 'N' TO OHUV-FLKLAR                                            
191900       PERFORM IMS-REPL-WDQ2                                              
192000     END-IF                                                               
192100                                                                          
192200     PERFORM UNTIL INDX > MAX-INDX                                        
192300                                                                          
192400       IF MID-CMD-UPDATE(INDX) NOT = ALL '+'                              
192500                                                                          
192600         PERFORM JBBA-COMMAND-IFYLLD                                      
192700                                                                          
192800       ELSE                                                               
192900                                                                          
193000         PERFORM JBBB-COMMAND-EJ-IFYLLD                                   
193100                                                                          
193200       END-IF                                                             
193300                                                                          
193400     END-PERFORM                                                          
193500                                                                          
193600     IF FIRST-TIME                                                        
193700       PERFORM JBBC-FLYTTA-NEXT-ARTNR                                     
193800     END-IF                                                               
193900                                                                          
194000     PERFORM JBBD-AVSLUTA-OCH-LAES-IGEN                                   
194100     .                                                                    
194200     EJECT                                                                
194300 JBBA-COMMAND-IFYLLD SECTION.                                             
194400                                                                          
194500     MOVE 'STA JBBA-COMMAND '             TO   WS-PGM-POSITION            
194600     MOVE MID-IDARTNR-SPAR(INDX) TO W-ORAD-IDARTNR-UNIK                   
194700     MOVE MID-IDLOPNR-SPAR(INDX) TO W-ORAD-IDLOPNR-UNIK                   
194800     MOVE MID-ADLAGOMR-SPAR(INDX) TO W-ORAD-ADLAGOMR-UNIK                 
194900     MOVE MID-ADGANG-SPAR(INDX) TO W-ORAD-ADGANG-UNIK                     
195000     MOVE MID-ADPLATS-SPAR(INDX) TO W-ORAD-ADPLATS-UNIK                   
195100                                                                          
195200     PERFORM IMS-GHU-ORQF-WDQ401                                          
195300                                                                          
195400     IF SEGMENT-FINNS                                                     
195500                                                                          
195600       IF MID-CMD-UPDATE(INDX) = 'D'                                      
195700         PERFORM S04-TABORT-RADEN                                         
195800         ADD +1 TO AVSR-INDX                                              
195900       END-IF                                                             
196000                                                                          
196100       IF MID-CMD-UPDATE(INDX) = 'X'                                      
196200         PERFORM S05-BACKA-BIPACKADE-RADER                                
196300         ADD +1 TO AVSR-INDX                                              
196400         MOVE JA TO RAD-SW                                                
196500       END-IF                                                             
196600                                                                          
196700       IF MID-CMD-UPDATE(INDX) = 'A'                                      
196800         PERFORM JBBAA-AENDRA-Q4-ANTAL                                    
196900       END-IF                                                             
197000                                                                          
197100     END-IF                                                               
197200                                                                          
197300     ADD +1 TO INDX                                                       
197400     .                                                                    
197500     EJECT                                                                
197600 JBBAA-AENDRA-Q4-ANTAL SECTION.                                           
197700                                                                          
197800     MOVE 'STA JBBAA-AENDRA '             TO   WS-PGM-POSITION            
197900     IF WS-KVBEART-Q(INDX) = ZERO                                         
198000       PERFORM S04-TABORT-RADEN                                           
198100       ADD +1 TO AVSR-INDX                                                
198200     ELSE                                                                 
198300       IF FIRST-TIME                                                      
198400         PERFORM S06-SPARA-ARTIKEL                                        
198500         MOVE NEJ TO FIRST-TIME-SW                                        
198600       END-IF                                                             
198700       COMPUTE MINSKAT-ANTAL = ORAD-KVBEART-Q                             
198800                        - WS-KVBEART-Q(INDX)                              
198900       MOVE MINSKAT-ANTAL TO ANNULLERAT-ANTAL                             
199000       PERFORM JBBAAA-AENDRA-KVBEART-Q                                    
199100       MOVE JA TO RAD-SW                                                  
199200       PERFORM S22-CHANGE-PRICE-Q-LINE                                    
199300       PERFORM IMS-REPL-WDQ4                                              
199400       ADD +1 TO AVSR-INDX                                                
199500     END-IF                                                               
199600     .                                                                    
199700     EJECT                                                                
199800 JBBAAA-AENDRA-KVBEART-Q SECTION.                                         
199900                                                                          
200000     MOVE 'STA JBBAAA-KVBEART '           TO   WS-PGM-POSITION            
200100     MOVE ORAD-KVPRERO TO SPAR-ORAD-KVPRERO                               
200200                                                                          
200300     IF MINSKAT-ANTAL > ORAD-KVPRERO                                      
200400       COMPUTE MINSKAT-ANTAL = MINSKAT-ANTAL - ORAD-KVPRERO               
200500       MOVE ZERO TO ORAD-KVPRERO                                          
200600       COMPUTE ORAD-KVPREAVB = ORAD-KVPREAVB - MINSKAT-ANTAL              
200700     ELSE                                                                 
200800       COMPUTE ORAD-KVPRERO = ORAD-KVPRERO - MINSKAT-ANTAL                
200900     END-IF                                                               
201000                                                                          
201100     MOVE ORAD-IDARTNR TO W-IDARTNR                                       
201200     IF ORAD-IDKAMPRF > ZERO                                              
201300       MOVE ORAD-IDKAMPRF       TO W-KAMP-IDKAMPRF                        
201400       MOVE ORAD-IDDC           TO W-KAMP-IDDC                            
201500       MOVE ORAD-IDARTNR        TO W-KART-IDARTNR                         
201600       MOVE ORAD-IDDISTR        TO W-KMRK-IDDISTR-FOM                     
201700       MOVE ORAD-IDDISTR        TO W-KMRK-IDDISTR-TOM                     
201800       MOVE ORAD-IDKUNDNR       TO W-KMRK-IDKUNDNR-FOM                    
201900       MOVE ORAD-IDKUNDNR       TO W-KMRK-IDKUNDNR-TOM                    
202000       PERFORM S11-BACKA-KAMPANJ                                          
202100     END-IF                                                               
202200                                                                          
202300     PERFORM JBBAAAA-LAS-K9-SKAPA-ORDBEKR                                 
202400     .                                                                    
202500     EJECT                                                                
202600 JBBAAAA-LAS-K9-SKAPA-ORDBEKR SECTION.                                    
202700                                                                          
202800     MOVE 'STA JBBAAAA-LAS-K9 '           TO   WS-PGM-POSITION            
202900     IF OHUV-IDSYSTEM = 'W216' AND OHUV-FLORDSPE = JA                     
203000       CONTINUE                                                           
203100*SKROT FRÅN 6322 RÄKNAR INTE UPP OKS/PREAVB                               
203200     ELSE                                                                 
203300       IF ORAD-IDLEVNR = SPACE                                            
203400         IF ORAD-IDDC NOT = W-IDDC-B6                                     
203500            MOVE ORAD-IDDC TO W-IDDC-B6                                   
203600            PERFORM IMS-GU-WDB601                                         
203700         END-IF                                                           
203800         IF DCS-CDC                                                       
203900           PERFORM IMS-GHU-ARTM-ARTM01                                    
204000           IF SEGMENT-FINNS                                               
204100             PERFORM JBBAAAAA-UPPDATERA-ARTM                              
204200             PERFORM IMS-REPL-ARTM                                        
204300           END-IF                                                         
204400         ELSE                                                             
204500           IF OHUV-KDORDKL = +0                                           
204600              PERFORM S04C-DELBACKA-NYVORKO                               
204700           END-IF                                                         
204800           MOVE WS-IDDC    TO W-IDDC                                      
204900           PERFORM IMS-GHU-ARTS-WDK711                                    
205000           IF OHUV-KDORDKL = +0 OR +1                                     
205100             COMPUTE SLAG-KVOKS-DAG =                                     
205200                     SLAG-KVOKS-DAG - ANNULLERAT-ANTAL                    
205300           ELSE                                                           
205400             IF OHUV-KDORDKL = +2 OR +3 OR +4                             
205500               IF ORAD-KVOKS-PREL = ZERO                                  
205600                  COMPUTE SLAG-KVOKS-BULK =                               
205700                          SLAG-KVOKS-BULK - ANNULLERAT-ANTAL              
205800               END-IF                                                     
205900             END-IF                                                       
206000           END-IF                                                         
206100           PERFORM IMS-REPL-WDK7                                          
206200         END-IF                                                           
206300       END-IF                                                             
206400       PERFORM S17-FIXA-REFILL-TRANSFER                                   
206500     END-IF                                                               
206600                                                                          
206700     PERFORM IMS-GU-WDK611                                                
206800     MOVE    CLAG-KVQPACK-1 TO WS-KVQPACK-1                               
206900     MOVE    CLAG-TIDISPIN  TO WS-TIDISPIN                                
207000                                                                          
207100     PERFORM S07-SKAPA-AVSR                                               
207200                                                                          
207300     MOVE WS-KVBEART-Q(INDX) TO ORAD-KVBEART-Q                            
207400                                ORAD-KVBEART                              
207500     MOVE +0 TO WS-ANTOBKR                                                
207600     PERFORM S08-SKAPA-ORDERBEKR                                          
207700                                                                          
207800     PERFORM S09-SKAPA-TRANSAR                                            
207900     .                                                                    
208000     EJECT                                                                
208100 JBBAAAAA-UPPDATERA-ARTM SECTION.                                         
208200                                                                          
208300     MOVE 'STA JBBAAAAA-ARTM  '           TO   WS-PGM-POSITION            
208400     MOVE ANNULLERAT-ANTAL TO MINSKAT-ANTAL                               
208500                                                                          
208600     IF OHUV-KDORDKL = ZERO                                               
208700                                                                          
208800       IF ORAD-IDKAMPRF NOT > ZERO                                        
208900         COMPUTE ART-KVOKS-VOR =                                          
209000          ART-KVOKS-VOR - MINSKAT-ANTAL                                   
209100       END-IF                                                             
209200                                                                          
209300       IF MINSKAT-ANTAL > SPAR-ORAD-KVPRERO                               
209400         COMPUTE ART-KVPREAVB-VOR =                                       
209500           ART-KVPREAVB-VOR - MINSKAT-ANTAL                               
209600       END-IF                                                             
209700                                                                          
209800       PERFORM S04C-DELBACKA-NYVORKO                                      
209900                                                                          
210000     END-IF                                                               
210100                                                                          
210200     IF OHUV-KDORDKL = +1                                                 
210300                                                                          
210400       IF ORAD-IDKAMPRF NOT > ZERO                                        
210500       COMPUTE ART-KVOKS-DAG =                                            
210600           ART-KVOKS-DAG - MINSKAT-ANTAL                                  
210700       END-IF                                                             
210800                                                                          
210900       IF MINSKAT-ANTAL > SPAR-ORAD-KVPRERO                               
211000         COMPUTE MINSKAT-ANTAL = MINSKAT-ANTAL -                          
211100                                        SPAR-ORAD-KVPRERO                 
211200         COMPUTE ART-KVPREAVB-DAG =                                       
211300           ART-KVPREAVB-DAG - MINSKAT-ANTAL                               
211400         COMPUTE ART-KVPRERO-DAG =                                        
211500           ART-KVPRERO-DAG - SPAR-ORAD-KVPRERO                            
211600       ELSE                                                               
211700         COMPUTE ART-KVPRERO-DAG =                                        
211800           ART-KVPRERO-DAG - MINSKAT-ANTAL                                
211900       END-IF                                                             
212000                                                                          
212100     END-IF                                                               
212200                                                                          
212300     IF OHUV-KDORDKL = +2 OR +3 OR +4                                     
212400                                                                          
212500       IF ORAD-IDKAMPRF NOT > ZERO                                        
212600         COMPUTE ART-KVOKS-BULK =                                         
212700         ART-KVOKS-BULK - MINSKAT-ANTAL                                   
212800       END-IF                                                             
212900                                                                          
213000       IF MINSKAT-ANTAL > SPAR-ORAD-KVPRERO                               
213100         COMPUTE MINSKAT-ANTAL = MINSKAT-ANTAL -                          
213200                                         SPAR-ORAD-KVPRERO                
213300         COMPUTE ART-KVPREAVB-BULK =                                      
213400           ART-KVPREAVB-BULK - MINSKAT-ANTAL                              
213500         COMPUTE ART-KVPRERO-BULK =                                       
213600           ART-KVPRERO-BULK - SPAR-ORAD-KVPRERO                           
213700       ELSE                                                               
213800         COMPUTE ART-KVPRERO-BULK =                                       
213900           ART-KVPRERO-BULK - MINSKAT-ANTAL                               
214000       END-IF                                                             
214100                                                                          
214200     END-IF                                                               
214300     .                                                                    
214400     EJECT                                                                
214500 JBBB-COMMAND-EJ-IFYLLD SECTION.                                          
214600                                                                          
214700     MOVE 'STA JBBB-COMMAND   '           TO   WS-PGM-POSITION            
214800     IF FIRST-TIME                                                        
214900                                                                          
215000       MOVE MID-IDARTNR-SPAR(INDX) TO W-ORAD-IDARTNR-UNIK                 
215100       MOVE MID-IDLOPNR-SPAR(INDX) TO W-ORAD-IDLOPNR-UNIK                 
215200       MOVE MID-ADLAGOMR-SPAR(INDX) TO W-ORAD-ADLAGOMR-UNIK               
215300       MOVE MID-ADGANG-SPAR(INDX) TO W-ORAD-ADGANG-UNIK                   
215400       MOVE MID-ADPLATS-SPAR(INDX) TO W-ORAD-ADPLATS-UNIK                 
215500                                                                          
215600       PERFORM IMS-GHU-ORQF-WDQ401                                        
215700                                                                          
215800       IF SEGMENT-FINNS                                                   
215900         PERFORM S06-SPARA-ARTIKEL                                        
216000         MOVE NEJ TO FIRST-TIME-SW                                        
216100       END-IF                                                             
216200                                                                          
216300     END-IF                                                               
216400     ADD +1 TO INDX                                                       
216500     .                                                                    
216600     EJECT                                                                
216700 JBBC-FLYTTA-NEXT-ARTNR SECTION.                                          
216800                                                                          
216900     MOVE MID-IDARTNR-NEXT  TO W-ORAD-IDARTNR-MIN                         
217000     MOVE MID-IDARTNR-NEXT  TO W-ORAD-IDARTNR-MAX                         
217100     MOVE MID-IDARTNR-NEXT  TO W-ORAD-IDARTNR-MIN-MIN                     
217200     MOVE MID-IDORDER-NEXT  TO W-ORAD-IDORDER-MIN                         
217300     MOVE MID-IDORDER-NEXT  TO W-ORAD-IDORDER-MIN-MIN                     
217400     MOVE MID-IDORDER-NEXT  TO W-ORAD-IDORDER-MAX                         
217500     MOVE MID-IDORDER-NEXT  TO W-ORAD-IDORDER-MAX-MAX                     
217600     MOVE MID-IDDC-NEXT     TO W-ORAD-IDDC-MIN                            
217700     MOVE MID-IDDC-NEXT     TO W-ORAD-IDDC-MAX                            
217800     MOVE MID-IDDC-NEXT     TO W-ORAD-IDDC-MIN-MIN                        
217900     MOVE MID-IDDC-NEXT     TO W-ORAD-IDDC-MAX-MAX                        
218000     MOVE MID-ADLAGOMR-NEXT TO W-ORAD-ADLAGOMR-MIN                        
218100     MOVE MID-ADLAGOMR-NEXT TO W-ORAD-ADLAGOMR-MIN-MIN                    
218200     MOVE MID-ADGANG-NEXT   TO W-ORAD-ADGANG-MIN                          
218300     MOVE MID-ADGANG-NEXT   TO W-ORAD-ADGANG-MIN-MIN                      
218400     MOVE MID-ADPLATS-NEXT  TO W-ORAD-ADPLATS-MIN                         
218500     MOVE MID-ADPLATS-NEXT  TO W-ORAD-ADPLATS-MIN-MIN                     
218600     MOVE MID-IDLOPNR-NEXT  TO W-ORAD-IDLOPNR-MIN                         
218700     MOVE MID-IDLOPNR-NEXT  TO W-ORAD-IDLOPNR-MIN-MIN                     
218800     PERFORM IMS-GU-ORQF-WDQ401                                           
218900     IF SEGMENT-SAKNAS                                                    
219000       MOVE OHUV-IDORDER    TO W-ORAD-IDORDER-MIN                         
219100       MOVE OHUV-IDORDER    TO W-ORAD-IDORDER-MIN-MIN                     
219200       MOVE OHUV-IDORDER    TO W-ORAD-IDORDER-MAX                         
219300       MOVE OHUV-IDORDER    TO W-ORAD-IDORDER-MAX-MAX                     
219400       MOVE WS-IDDC         TO W-ORAD-IDDC-MIN                            
219500       MOVE WS-IDDC         TO W-ORAD-IDDC-MIN-MIN                        
219600       MOVE WS-IDDC         TO W-ORAD-IDDC-MAX                            
219700       MOVE WS-IDDC         TO W-ORAD-IDDC-MAX-MAX                        
219800       MOVE ZERO            TO W-ORAD-IDARTNR-MIN                         
219900       MOVE ZERO            TO W-ORAD-IDARTNR-MIN-MIN                     
220000       MOVE ZERO            TO W-ORAD-ADLAGOMR-MIN                        
220100       MOVE ZERO            TO W-ORAD-ADLAGOMR-MIN-MIN                    
220200       MOVE ZERO            TO W-ORAD-ADGANG-MIN                          
220300       MOVE ZERO            TO W-ORAD-ADGANG-MIN-MIN                      
220400       MOVE ZERO            TO W-ORAD-ADPLATS-MIN                         
220500       MOVE ZERO            TO W-ORAD-ADPLATS-MIN-MIN                     
220600       MOVE ZERO            TO W-ORAD-IDLOPNR-MIN                         
220700       MOVE ZERO            TO W-ORAD-IDLOPNR-MIN-MIN                     
220800     END-IF                                                               
220900     .                                                                    
221000     EJECT                                                                
221100 JBBD-AVSLUTA-OCH-LAES-IGEN SECTION.                                      
221200                                                                          
221300     MOVE 'STA JBBD-AVSLUTA   '           TO   WS-PGM-POSITION            
221400     IF AVSR-OK                                                           
221500                                                                          
221600       PERFORM S10-SKAPA-AVSO                                             
221700                                                                          
221800       CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT2-PCB                    
221900       AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                          
222000       AVSR-WDB2-PCB AVSR-WDB6-PCB TRAN-XXKB-PCB                          
222100                                                                          
222200                                                                          
222300       CALL W413AVSO USING AVSO-W413AVSO                                  
222400       AVSO-WDE6-PCB AVSO-ORQA-PCB                                        
222500       AVSO-WDQ2-PCB AVSO-GMTB-PCB                                        
222600       AVSO-XXKA-PCB AVSO-4437-PCB AVSO-XXKE-PCB                          
222700       AVSO-XXKF-PCB AVSO-XXKG-PCB AVSO-XXKH-PCB                          
222800       AVSO-XXKI-PCB AVSO-XXKP-PCB AVSO-WDB2-PCB AVSO-WDB6-PCB            
222900       AVSO-WDP7-PCB TRAN-XXKB-PCB                                        
223000       ORDN-ORQL-PCB ORDN-PROC-PCB ORDN-ORQI-PCB ORDN-WDQ3-PCB            
223100                                                                          
223200     END-IF                                                               
223300                                                                          
223400     PERFORM IMS-GHU-ORQI-WDQ201                                          
223500                                                                          
223600     IF SEGMENT-FINNS                                                     
223700                                                                          
223800       MOVE JA TO OHUV-FLKLAR                                             
223900       PERFORM IMS-REPL-WDQ2                                              
224000                                                                          
224100     END-IF                                                               
224200                                                                          
224300                                                                          
224400     MOVE ZERO                 TO  KVRADER-LOR-RAKNARE                    
224500                                   KVRADER-ODEL-RAKNARE                   
224600                                   KVRADER-UP-RAKNARE                     
224700                                   KVRADER-DIRL-RAKNARE                   
224800                                   TOTAL-KVRADER                          
224900                                   TOTAL-UTSKRIVNA                        
225000                                   MOJLIG-ANTAL                           
225100                                                                          
225200     PERFORM S01-RAEKNA-TOTALA-RADER                                      
225300                                                                          
225400     PERFORM S02-KOLLA-ARBETSTABELL                                       
225500     .                                                                    
225600     EJECT                                                                
225700                                                                          
225800 JC-SKAPA-EDI-DGS SECTION.                                                
225900                                                                          
226000     MOVE 'STA JC-SKAPA-EDI '             TO   WS-PGM-POSITION            
226100     MOVE JA TO INDATA-SW                                                 
226200     MOVE +1 TO INDX                                                      
226300                                                                          
226400     IF MID-INPUT NOT = ALL '+'                                           
226500                                                                          
226600       PERFORM UNTIL INDX > MAX-INDX OR                                   
226700         MID-IDARTNR-SPAR(INDX) = ZERO                                    
226800                                                                          
226900         IF MID-CMD-UPDATE(INDX) NOT = ALL '+'                            
227000           MOVE ZERO                  TO WS-KVANNANT                      
227100           MOVE ODEL-IDDISTR          TO W-401-IDDISTR                    
227200           MOVE ODEL-IDKUNDNR         TO W-401-IDKUNDNR                   
227300           MOVE ODEL-IDKUNDRF(3:5)    TO W-401-IDORDNR                    
227400           MOVE MID-IDPRODNR-SPAR(INDX) TO W-401-IDPRODNR                 
227500           MOVE MID-IDPLKLST-SPAR(INDX) TO W-401-IDPLKLST                 
227600           MOVE MID-IDPURAD-SPAR(INDX) TO W-411-IDPURAD                   
227700                                                                          
227800           PERFORM IMS-GHU-WDE411                                         
227900           IF SEGMENT-FINNS                                               
228000                                                                          
228100             IF E4-ORAD-KDANNULL = '0' OR '1'                             
228200               IF E4-ORAD-KVANNANT = ZERO                                 
228300                 COMPUTE WS-KVANNANT = E4-ORAD-KVAVBART -                 
228400                                       E4-ORAD-KVLEVART                   
228500               END-IF                                                     
228600             END-IF                                                       
228700                                                                          
228800             IF WS-KVANNANT > ZERO                                        
228900               PERFORM S23-CHECK-ORDER-PRINTED                            
229000               IF ORDER-FINNS                                             
229100                  MOVE '3'            TO E4-ORAD-KDANNULL                 
229200               ELSE                                                       
229300                  MOVE '1'            TO E4-ORAD-KDANNULL                 
229400               END-IF                                                     
229500               MOVE '4245'            TO E4-ORAD-IDSYSTEM                 
229600               PERFORM IMS-REPL-WDE411                                    
229700               PERFORM JCA-EDI-TRANS                                      
229800             END-IF                                                       
229900                                                                          
230000           END-IF                                                         
230100         END-IF                                                           
230200         ADD +1 TO INDX                                                   
230300       END-PERFORM                                                        
230400       MOVE LOW-VALUE   TO W-WDQ301KY-MIN-X                               
230500       MOVE HIGH-VALUE  TO W-WDQ301KY-MAX-X                               
230600       PERFORM S12-RAEKNA-RADER-DDGS                                      
230700     END-IF                                                               
230800     .                                                                    
230900     EJECT                                                                
231000 JCA-EDI-TRANS SECTION.                                                   
231100                                                                          
231200     MOVE 'STA JCA-EDI-TRANS '            TO   WS-PGM-POSITION            
231300     COMPUTE 4680-KVLL = LENGTH OF 4680-MID-W4I68001 + 17                 
231400                                                                          
231500     MOVE FUNCTION CURRENT-DATE (1:8) TO 4680-MID-DABEKDAT                
231600     ACCEPT 4680-MID-TIBEKR FROM TIME                                     
231700     MOVE ODEL-IDDC                   TO 4680-MID-IDDC                    
231800     MOVE ODEL-IDDISTR                TO 4680-MID-IDDISTR                 
231900     MOVE ODEL-IDKUNDNR               TO 4680-MID-IDKUNDNR                
232000     MOVE E4-ORAD-IDLEVNR             TO 4680-MID-IDLEVNR                 
232100     MOVE ODEL-IDORDNR7               TO 4680-MID-IDORDNR7                
232200     MOVE E4-ORAD-IDPRODNR            TO 4680-MID-IDPRODNR                
232300     MOVE W-401-IDPLKLST              TO 4680-MID-IDPLKLST                
232400     MOVE E4-ORAD-IDARTNR             TO 4680-MID-IDARTNR(1)              
232500     MOVE E4-ORAD-IDPURAD             TO 4680-MID-IDRADNR(1)              
232600     MOVE WS-KVANNANT                 TO 4680-MID-KVBEART(1)              
232700                                                                          
232800     MOVE 'W4T680X '                  TO 4680-TRANSKOD                    
232900     MOVE '4245'                      TO 4680-IDTRANS                     
233000     MOVE SPACE                       TO 4680-KDMFSFOR                    
233100                                                                          
233200     PERFORM IMS-PURG-ALT-MSG-4680                                        
233300     .                                                                    
233400     EJECT                                                                
233500 JD-DELETE-LINES SECTION.                                                 
233600                                                                          
233700     MOVE 'STA JD-DELETE-LIN'             TO   WS-PGM-POSITION            
233800     MOVE SPACE TO 4360-MID-W4I36001                                      
233900     MOVE JA TO INDATA-SW                                                 
234000     MOVE +1 TO INDX                                                      
234100                                                                          
234200     IF MID-INPUT NOT = ALL '+'                                           
234300                                                                          
234400       PERFORM UNTIL INDX > MAX-INDX OR                                   
234500         MID-IDARTNR-SPAR(INDX) = ZERO                                    
234600                                                                          
234700         IF MID-CMD-UPDATE(INDX) = 'D'                                    
234800           MOVE ZERO                  TO WS-KVANNANT                      
234900           MOVE ODEL-IDDISTR          TO W-401-IDDISTR                    
235000           MOVE ODEL-IDKUNDNR         TO W-401-IDKUNDNR                   
235100           MOVE ODEL-IDKUNDRF(3:5)    TO W-401-IDORDNR                    
235200           MOVE MID-IDPRODNR-SPAR(INDX) TO W-401-IDPRODNR                 
235300           MOVE MID-IDPLKLST-SPAR(INDX) TO W-401-IDPLKLST                 
235400           MOVE MID-IDPURAD-SPAR(INDX) TO W-411-IDPURAD                   
235500                                                                          
235600           PERFORM IMS-GHU-WDE411                                         
235700           IF SEGMENT-FINNS                                               
235800                                                                          
235900             PERFORM S23-CHECK-ORDER-PRINTED                              
236000                                                                          
236100             IF NOT ORDER-FINNS                                           
236200               IF E4-ORAD-KVANNANT = ZERO                                 
236300                  COMPUTE WS-KVANNANT = E4-ORAD-KVAVBART -                
236400                                        E4-ORAD-KVLEVART                  
236500               END-IF                                                     
236600             END-IF                                                       
236700                                                                          
236800             IF WS-KVANNANT > ZERO                                        
236900               MOVE '3'               TO E4-ORAD-KDANNULL                 
237000               MOVE '4245'            TO E4-ORAD-IDSYSTEM                 
237100               PERFORM IMS-REPL-WDE411                                    
237200               PERFORM JDA-CALL-W4T360X                                   
237300             END-IF                                                       
237400                                                                          
237500           END-IF                                                         
237600         END-IF                                                           
237700         ADD +1 TO INDX                                                   
237800       END-PERFORM                                                        
237900       MOVE LOW-VALUE   TO W-WDQ301KY-MIN-X                               
238000       MOVE HIGH-VALUE  TO W-WDQ301KY-MAX-X                               
238100       PERFORM S12-RAEKNA-RADER-DDGS                                      
238200     END-IF                                                               
238300     .                                                                    
238400 JDA-CALL-W4T360X SECTION.                                                
238500                                                                          
238600     MOVE 1                    TO 4360-INDX                               
238700                                                                          
238800     COMPUTE 4360-KVLL = LENGTH OF 4360-MID-W4I36001 + 17                 
238900                                                                          
239000     MOVE 0                    TO 4360-MID-DABEKDAT                       
239100     MOVE ODEL-IDDC            TO 4360-MID-IDDC                           
239200     MOVE ODEL-IDDISTR         TO WS-ODEL-IDDISTR-NUM                     
239300     MOVE WS-ODEL-IDDISTR-NUM  TO 4360-MID-IDDISTR                        
239400     MOVE ODEL-IDKUNDNR        TO WS-ODEL-IDKUNDNR-NUM                    
239500     MOVE WS-ODEL-IDKUNDNR-NUM TO 4360-MID-IDKUNDNR                       
239600     MOVE E4-ORAD-IDLEVNR      TO 4360-MID-IDLEVNR                        
239700     MOVE ODEL-IDORDNR7        TO 4360-MID-IDORDNR7                       
239800     MOVE E4-ORAD-IDPRODNR     TO 4360-MID-IDPRODNR                       
239900     MOVE FUNCTION CURRENT-DATE (1:8) TO 4360-MID-TIBEKR                  
240000     MOVE MSGI-IDUSER          TO 4360-MID-IDUSER                         
240100                                                                          
240200     MOVE 'VO '                TO CIA-IDARTPRE-IN                         
240300                                  4360-MID-IDARTPRE(4360-INDX)            
240400     MOVE E4-ORAD-IDARTNR      TO CIA-IDARTBET-IN                         
240500     CALL W009CIA              USING CIA-W009CIA                          
240600     MOVE CIA-IDARTBET-UT      TO 4360-MID-IDARTBET(4360-INDX)            
240700     MOVE E4-ORAD-IDARTNR      TO 4360-MID-IDARTNR(4360-INDX)             
240800     MOVE E4-ORAD-IDPURAD      TO WS-IDRADNR                              
240900     MOVE WS-IDRADNR           TO 4360-MID-IDRADNR(4360-INDX)             
241000                                                                          
241100     MOVE '83'                 TO 4360-MID-KDORDBEK (4360-INDX)           
241200     MOVE E4-ORAD-KVBEART      TO 4360-MID-KVBEART  (4360-INDX)           
241300     MOVE 00000000             TO 4360-MID-DALEVDAT (4360-INDX)           
241400                                                                          
241500     MOVE 'W4T360X '           TO 4360-TRANSKOD                           
241600     MOVE '4245'               TO 4360-IDTRANS                            
241700     MOVE SPACE                TO 4360-KDMFSFOR                           
241800                                                                          
241900     PERFORM IMS-PURG-ALT-MSG-4360                                        
242000                                                                          
242100                                                                          
242200     .                                                                    
242300     EJECT                                                                
242400 K-STARTA-2109  SECTION.                                                  
242500                                                                          
242600     MOVE 'STA K-STARTA-2109    '         TO   WS-PGM-POSITION            
242700     COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                
242800                                                                          
242900     PERFORM IMS-PURG-ALT-MSG-2109                                        
243000                                                                          
243100     MOVE SPACE              TO 2109-MID2-W2I10902                        
243200     MOVE +1                 TO 2109-INDX                                 
243300     .                                                                    
243400     EJECT                                                                
243500 L-LAES-VISA-DDGS SECTION.                                                
243600                                                                          
243700     MOVE 'STA L-LAES-DDGS      '         TO   WS-PGM-POSITION            
243800     IF MOJLIG-ANTAL > ZERO                                               
243900       MOVE +1 TO INDX                                                    
244000       PERFORM IMS-GU-ORQA-WDQ301-STATUS                                  
244100       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
244200         INDX > MAX-INDX                                                  
244300           MOVE ODEL-IDDISTR          TO W-401-IDDISTR                    
244400           MOVE ODEL-IDKUNDNR         TO W-401-IDKUNDNR                   
244500           MOVE ODEL-IDKUNDRF(3:5)    TO W-401-IDORDNR                    
244600           MOVE ODEL-IDPRODNR         TO W-401-IDPRODNR                   
244700           MOVE ODEL-IDPLKLST         TO W-401-IDPLKLST                   
244800                                                                          
244900         PERFORM IMS-GU-WDE401                                            
245000         IF SEGMENT-FINNS                                                 
245100           IF KORD-KVORDRAD-LEVPL > ZERO                                  
245200             IF IDARTNR-IFYLLT                                            
245300               PERFORM IMS-GNP-WDE411-ARTNR                               
245400             ELSE                                                         
245500               IF MFS-NEXT                                                
245600                 PERFORM IMS-GNP-WDE411-UNIK                              
245700               ELSE                                                       
245800                 PERFORM IMS-GNP-WDE411                                   
245900               END-IF                                                     
246000             END-IF                                                       
246100                                                                          
246200             IF SEGMENT-FINNS                                             
246300               IF INDX = +1                                               
246400                 MOVE ODEL-IDORDER    TO MOD-IDORDER-ENTER                
246500                 MOVE ODEL-IDPRODNR   TO MOD-IDPRODNR-ENTER               
246600                 MOVE ODEL-IDPLKLST   TO MOD-IDPLKLST-ENTER               
246700                 MOVE WS-IDDC         TO MOD-IDDC-ENTER                   
246800                 PERFORM LA-SPAR-WDE4-ENTER                               
246900                 PERFORM LB-SPAR-WDE4-NEXT                                
247000               END-IF                                                     
247100                                                                          
247200               PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX            
247300                                                                          
247400                 IF (E4-ORAD-KDANNULL = ZERO OR 1 OR 2) AND               
247500                    E4-ORAD-KDRADSTA  < +4                                
247600                   PERFORM LC-FLYTTA-TILL-MOD                             
247700                                                                          
247800                   MOVE E4-ORAD-IDARTNR TO W-WDD3-IDARTNR                 
247900                   MOVE MED-IDSKYLT TO W-TEXT-IDSKYLT                     
248000                   PERFORM S15-LAS-BENAEMNING                             
248100                                                                          
248200                   ADD +1 TO INDX                                         
248300                 END-IF                                                   
248400                                                                          
248500                 IF IDARTNR-IFYLLT                                        
248600                   PERFORM IMS-GNP-WDE411-ARTNR                           
248700                 ELSE                                                     
248800                   PERFORM IMS-GNP-WDE411                                 
248900                 END-IF                                                   
249000                                                                          
249100               END-PERFORM                                                
249200                                                                          
249300               IF SEGMENT-FINNS                                           
249400                                                                          
249500                 PERFORM LB-SPAR-WDE4-NEXT                                
249600                                                                          
249700                 IF NOT MFS-UPDATE                                        
249800                   MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF              
249900                   CALL WMEDKONV USING MED-WMEDAREA                       
250000                   MOVE MED-MFSINF TO MOD-TEMFSINF                        
250100                 END-IF                                                   
250200                                                                          
250300               END-IF                                                     
250400                                                                          
250500             END-IF                                                       
250600           END-IF                                                         
250700         END-IF                                                           
250800         PERFORM IMS-GN-ORQA-WDQ301-STATUS                                
250900       END-PERFORM                                                        
251000       PERFORM UNTIL INDX > MAX-INDX                                      
251100         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
251200                               MOD-CMD-UPDATE-ATTR(INDX)                  
251300         MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                
251400                         MOD-KVBEART-Q-UPDATE-ATTR(INDX)                  
251500         MOVE ZERO TO MOD-IDARTNR-SPAR(INDX)                              
251600                      MOD-IDLOPNR-SPAR(INDX)                              
251700                      MOD-ADLAGOMR-SPAR(INDX)                             
251800                      MOD-ADGANG-SPAR(INDX)                               
251900                      MOD-ADPLATS-SPAR(INDX)                              
252000         ADD +1 TO INDX                                                   
252100       END-PERFORM                                                        
252200       MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                  
252300                         MOD-FLAGGA-UPDATE-ATTR                           
252400                                                                          
252500       PERFORM MFS-RENSA-FAELT-IN                                         
252600     ELSE                                                                 
252700       IF IDARTNR-IFYLLT                                                  
252800         MOVE INF-PART-MISSING TO MED-IDMFSINF                            
252900         CALL WMEDKONV USING MED-WMEDAREA                                 
253000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
253100       ELSE                                                               
253200         MOVE ERR-LINES-MISSING TO MED-IDMFSFEL                           
253300         CALL WMEDKONV USING MED-WMEDAREA                                 
253400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
253500       END-IF                                                             
253600                                                                          
253700       PERFORM MFS-RENSA-FAELT-IN                                         
253800       PERFORM MFS-RENSA-FAELT-UT                                         
253900       MOVE MFS-RENSA-FAELT  TO MOD-KVRADER                               
254000                                MOD-KVORDRAD                              
254100                                                                          
254200     END-IF                                                               
254300     .                                                                    
254400     EJECT                                                                
254500 LA-SPAR-WDE4-ENTER SECTION.                                              
254600                                                                          
254700     MOVE E4-ORAD-IDPURAD  TO MOD-IDPURAD-ENTER                           
254800     MOVE E4-ORAD-IDARTNR  TO MOD-IDARTNR-ENTER                           
254900     .                                                                    
255000     EJECT                                                                
255100 LB-SPAR-WDE4-NEXT SECTION.                                               
255200                                                                          
255300     MOVE ODEL-IDORDER     TO MOD-IDORDER-NEXT                            
255400     MOVE WS-IDDC          TO MOD-IDDC-NEXT                               
255500     MOVE KORD-IDPRODNR    TO MOD-IDPRODNR-NEXT                           
255600     MOVE KORD-IDPLKLST    TO MOD-IDPLKLST-NEXT                           
255700     MOVE E4-ORAD-IDPURAD  TO MOD-IDPURAD-NEXT                            
255800     MOVE E4-ORAD-IDARTNR  TO MOD-IDARTNR-NEXT                            
255900     .                                                                    
256000     EJECT                                                                
256100 LC-FLYTTA-TILL-MOD SECTION.                                              
256200                                                                          
256300     MOVE 'STA LC-FLYTTA        '         TO   WS-PGM-POSITION            
256400     MOVE KORD-IDPRODNR     TO MOD-IDPRODNR-SPAR(INDX)                    
256500     MOVE KORD-IDPLKLST     TO MOD-IDPLKLST-SPAR(INDX)                    
256600     MOVE E4-ORAD-IDARTNR   TO MOD-IDARTNR-RAD(INDX)                      
256700                               MOD-IDARTNR-SPAR(INDX)                     
256800     MOVE E4-ORAD-IDPURAD   TO MOD-IDPURAD-SPAR(INDX)                     
256900     INSPECT MOD-IDARTNR-RAD(INDX) REPLACING LEADING ZERO                 
257000                                   BY SPACE                               
257100*    MOVE E4-ORAD-KVBEART   TO MOD-KVBEART-Q-RAD(INDX)                    
257200     COMPUTE MOD-KVBEART-Q-RAD(INDX) =                                    
257300          E4-ORAD-KVBEART - E4-ORAD-KVLEVART                              
257400     MOVE WS-IDDC           TO MOD-IDDC-RAD(INDX)                         
257500                                                                          
257600     IF E4-ORAD-IDKUNDRF-RO NOT = '0000000   '                            
257700       MOVE E4-ORAD-IDKUNDRF-RO (1:7) TO MOD-OREF-RAD(INDX)               
257800       INSPECT MOD-OREF-RAD(INDX) REPLACING LEADING ZERO                  
257900                                   BY SPACE                               
258000       MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                  
258100                                MOD-KVBEART-Q-UPDATE-ATTR(INDX)           
258200       MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                  
258300                                MOD-FLAGGA-UPDATE-ATTR                    
258400     ELSE                                                                 
258500       MOVE MFS-RENSA-FAELT TO MOD-OREF-RAD(INDX)                         
258600     END-IF                                                               
258700                                                                          
258800     MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                    
258900                                MOD-KVBEART-Q-UPDATE-ATTR(INDX)           
259000     MOVE MFS-STAENG-FAELT-OSYNLIGT TO                                    
259100                                MOD-FLAGGA-UPDATE-ATTR                    
259200     .                                                                    
259300     EJECT                                                                
259400 S01-RAEKNA-TOTALA-RADER SECTION.                                         
259500                                                                          
259600     MOVE 'STA S01-RAEKNA     '           TO   WS-PGM-POSITION            
259700                                                                          
259800     MOVE WS-IDDC TO W-ODEL-IDDC-MIN                                      
259900                     W-ODEL-IDDC-MAX                                      
260000                                                                          
260100     PERFORM IMS-GU-ORQA-WDQ301                                           
260200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
260300       ADD ODEL-KVRADER TO KVRADER-ODEL-RAKNARE                           
260400       IF ODEL-KDODELSTA = 'U' OR 'P'                                     
260500         ADD ODEL-KVRADER TO KVRADER-UP-RAKNARE                           
260600       END-IF                                                             
260700       PERFORM IMS-GN-ORQA-WDQ301                                         
260800     END-PERFORM                                                          
260900                                                                          
261000     PERFORM IMS-GNP-WDQ212-OKVAL                                         
261100     PERFORM UNTIL SEGMENT-SAKNAS                                         
261200       IF ARB-KDTRPKAT = 'B' OR 'C'                                       
261300         IF ARB-IDDC NOT = W-IDDC-B6                                      
261400            MOVE ARB-IDDC TO W-IDDC-B6                                    
261500            PERFORM IMS-GU-WDB601                                         
261600         END-IF                                                           
261700         IF DCS-CDC                                                       
261800           MOVE JA TO DIRLEV-KOLL                                         
261900         END-IF                                                           
262000                                                                          
262100         MOVE ARB-IDDC TO W-IDDC                                          
262200         PERFORM IMS-GNP-WDQ221                                           
262300         PERFORM UNTIL SEGMENT-SAKNAS                                     
262400           ADD LOR-KVRADER TO KVRADER-LOR-RAKNARE                         
262500           PERFORM IMS-GNP-WDQ221                                         
262600         END-PERFORM                                                      
262700       END-IF                                                             
262800       PERFORM IMS-GNP-WDQ212-OKVAL                                       
262900     END-PERFORM                                                          
263000                                                                          
263100     IF DIRLEV-KOLL = JA                                                  
263200       PERFORM IMS-GNP-ORQI-WDQ211-FIRST                                  
263300       PERFORM UNTIL SEGMENT-SAKNAS                                       
263400         ADD DIRL-KVRADER TO KVRADER-DIRL-RAKNARE                         
263500         PERFORM IMS-GNP-ORQI-WDQ211                                      
263600       END-PERFORM                                                        
263700     END-IF                                                               
263800     EJECT                                                                
263900     COMPUTE TOTAL-KVRADER = KVRADER-ODEL-RAKNARE +                       
264000                             KVRADER-LOR-RAKNARE  +                       
264100                             KVRADER-DIRL-RAKNARE                         
264200                                                                          
264300     COMPUTE MOJLIG-ANTAL = TOTAL-KVRADER - KVRADER-UP-RAKNARE            
264400     IF MOJLIG-ANTAL = ZERO                                               
264500       IF ARB-KDTRPKAT = 'B' OR 'C'                                       
264600         MOVE ERR-LINES-MISSING TO MED-IDMFSFEL                           
264700       ELSE                                                               
264800         IF TOTAL-KVRADER = ZERO                                          
264900           MOVE ERR-LINES-MISSING TO MED-IDMFSFEL                         
265000         ELSE                                                             
265100           IF WS-IDDC NOT = W-IDDC-B6                                     
265200              MOVE WS-IDDC TO W-IDDC-B6                                   
265300              PERFORM IMS-GU-WDB601                                       
265400           END-IF                                                         
265500           IF DCS-DDC                                                     
265600             MOVE ERR-ANNULL-NOT-POSS TO MED-IDMFSFEL                     
265700           ELSE                                                           
265800             MOVE ERR-LINES-WRITTEN   TO MED-IDMFSFEL                     
265900           END-IF                                                         
266000         END-IF                                                           
266100       END-IF                                                             
266200       CALL WMEDKONV USING MED-WMEDAREA                                   
266300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
266400       PERFORM MFS-RENSA-FAELT-UT                                         
266500       MOVE MFS-RENSA-FAELT  TO MOD-KVRADER                               
266600                                MOD-KVORDRAD                              
266700       MOVE NEJ TO ALLT-SW                                                
266800     ELSE                                                                 
266900       MOVE MOJLIG-ANTAL   TO MOD-KVRADER                                 
267000       MOVE TOTAL-KVRADER  TO MOD-KVORDRAD                                
267100     END-IF                                                               
267200     .                                                                    
267300     EJECT                                                                
267400 S02-KOLLA-ARBETSTABELL SECTION.                                          
267500                                                                          
267600                                                                          
267700     MOVE 'STA S02-KOLLA      '           TO   WS-PGM-POSITION            
267800     MOVE WS-IDDC           TO W-IDDC                                     
267900     PERFORM IMS-GNP-ORQI-WDQ212                                          
268000                                                                          
268100     IF SEGMENT-FINNS                                                     
268200       IF ARB-KDTRPKAT = 'A'                                              
268300         PERFORM S02A-TRANSPORTKAT-A                                      
268400       ELSE                                                               
268500         PERFORM S02B-TRANSPORTKAT-B-C                                    
268600       END-IF                                                             
268700     END-IF                                                               
268800     .                                                                    
268900     EJECT                                                                
269000 S02A-TRANSPORTKAT-A SECTION.                                             
269100                                                                          
269200     MOVE 'STA S02A-TRANSPORT '           TO   WS-PGM-POSITION            
269300     MOVE JA TO STATUS-SW                                                 
269400     MOVE WS-IDDC         TO W-ODEL-IDDC-MIN                              
269500                             W-ODEL-IDDC-MAX                              
269600     PERFORM IMS-GU-ORQA-WDQ301                                           
269700                                                                          
269800     IF SEGMENT-FINNS                                                     
269900                                                                          
270000       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
270100         NOT STATUS-OK                                                    
270200                                                                          
270300         IF ODEL-KDODELSTA = 'R'                                          
270400           MOVE NEJ TO STATUS-SW                                          
270500         END-IF                                                           
270600                                                                          
270700         PERFORM IMS-GN-ORQA-WDQ301                                       
270800                                                                          
270900       END-PERFORM                                                        
271000       IF STATUS-OK                                                       
271100         IF MOJLIG-ANTAL > ZERO                                           
271200           MOVE ERR-LINES-MISSING-CL TO MED-IDMFSFEL                      
271300         ELSE                                                             
271400           MOVE ERR-LINES-MISSING    TO MED-IDMFSFEL                      
271500         END-IF                                                           
271600         CALL WMEDKONV USING MED-WMEDAREA                                 
271700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
271800         PERFORM MFS-RENSA-FAELT-UT                                       
271900         MOVE NEJ TO ALLT-SW                                              
272000       END-IF                                                             
272100                                                                          
272200     ELSE                                                                 
272300       IF MOJLIG-ANTAL > ZERO                                             
272400         MOVE ERR-LINES-MISSING-CL TO MED-IDMFSFEL                        
272500       ELSE                                                               
272600         MOVE ERR-LINES-MISSING      TO MED-IDMFSFEL                      
272700       END-IF                                                             
272800       CALL WMEDKONV USING MED-WMEDAREA                                   
272900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
273000       PERFORM MFS-RENSA-FAELT-UT                                         
273100       MOVE NEJ TO ALLT-SW                                                
273200     END-IF                                                               
273300     .                                                                    
273400     EJECT                                                                
273500 S02B-TRANSPORTKAT-B-C SECTION.                                           
273600                                                                          
273700     MOVE 'STA S02B-TRANSPORT '           TO   WS-PGM-POSITION            
273800     MOVE JA TO STATUS-SW                                                 
273900     MOVE WS-IDDC         TO W-ODEL-IDDC-MIN                              
274000                             W-ODEL-IDDC-MAX                              
274100     PERFORM IMS-GU-ORQA-WDQ301                                           
274200                                                                          
274300     IF SEGMENT-FINNS                                                     
274400                                                                          
274500       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
274600         NOT STATUS-OK                                                    
274700                                                                          
274800         IF ODEL-KDODELSTA = 'R'                                          
274900           MOVE NEJ TO STATUS-SW                                          
275000         END-IF                                                           
275100                                                                          
275200         PERFORM IMS-GN-ORQA-WDQ301                                       
275300                                                                          
275400       END-PERFORM                                                        
275500       IF STATUS-OK                                                       
275600         IF MOJLIG-ANTAL > ZERO                                           
275700           MOVE ERR-LINES-MISSING-CL TO MED-IDMFSFEL                      
275800         ELSE                                                             
275900           MOVE ERR-LINES-MISSING    TO MED-IDMFSFEL                      
276000         END-IF                                                           
276100         CALL WMEDKONV USING MED-WMEDAREA                                 
276200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
276300         PERFORM MFS-RENSA-FAELT-UT                                       
276400         MOVE NEJ TO ALLT-SW                                              
276500       END-IF                                                             
276600                                                                          
276700     ELSE                                                                 
276800       PERFORM IMS-GNP-WDQ221                                             
276900       IF SEGMENT-SAKNAS                                                  
277000         MOVE NEJ TO DIRLEV-SW                                            
277100         PERFORM IMS-GNP-ORQI-WDQ211-FIRST                                
277200         IF SEGMENT-FINNS                                                 
277300           PERFORM UNTIL SEGMENT-SAKNAS OR DIRLEV-RADER                   
277400               IF DIRL-KVRADER > ZERO                                     
277500                 MOVE JA  TO DIRLEV-SW                                    
277600               ELSE                                                       
277700                 PERFORM IMS-GNP-ORQI-WDQ211                              
277800               END-IF                                                     
277900           END-PERFORM                                                    
278000         END-IF                                                           
278100         IF NOT DIRLEV-RADER                                              
278200           IF MOJLIG-ANTAL > ZERO                                         
278300             MOVE ERR-LINES-MISSING-CL TO MED-IDMFSFEL                    
278400           ELSE                                                           
278500             MOVE ERR-LINES-MISSING TO MED-IDMFSFEL                       
278600           END-IF                                                         
278700           CALL WMEDKONV USING MED-WMEDAREA                               
278800           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
278900           PERFORM MFS-RENSA-FAELT-UT                                     
279000           MOVE NEJ TO ALLT-SW                                            
279100         END-IF                                                           
279200       ELSE                                                               
279300         CONTINUE                                                         
279400       END-IF                                                             
279500     END-IF                                                               
279600     .                                                                    
279700     EJECT                                                                
279800 S04-TABORT-RADEN SECTION.                                                
279900                                                                          
280000     MOVE 'STA S04-TABORT     '           TO   WS-PGM-POSITION            
280100     MOVE ORAD-KVBEART-Q      TO SPAR-ORAD-KVBEART-Q                      
280200                                 ANNULLERAT-ANTAL                         
280300     MOVE ORAD-KVPREAVB       TO SPAR-ORAD-KVPREAVB                       
280400     MOVE ORAD-KVPRERO        TO SPAR-ORAD-KVPRERO                        
280500     MOVE ORAD-IDARTNR        TO W-IDARTNR                                
280600     IF ORAD-IDKAMPRF > ZERO                                              
280700       MOVE ORAD-IDKAMPRF     TO W-KAMP-IDKAMPRF                          
280800       MOVE ORAD-IDDC         TO W-KAMP-IDDC                              
280900       MOVE ORAD-IDARTNR      TO W-KART-IDARTNR                           
281000       MOVE ORAD-IDDISTR      TO W-KMRK-IDDISTR-FOM                       
281100       MOVE ORAD-IDDISTR      TO W-KMRK-IDDISTR-TOM                       
281200       MOVE ORAD-IDKUNDNR     TO W-KMRK-IDKUNDNR-FOM                      
281300       MOVE ORAD-IDKUNDNR     TO W-KMRK-IDKUNDNR-TOM                      
281400       PERFORM S11-BACKA-KAMPANJ                                          
281500     END-IF                                                               
281600                                                                          
281700     IF OHUV-IDSYSTEM = 'W216' AND OHUV-FLORDSPE = JA                     
281800       CONTINUE                                                           
281900     ELSE                                                                 
282000       IF ORAD-IDLEVNR = SPACE                                            
282100         IF ORAD-IDDC NOT = W-IDDC-B6                                     
282200            MOVE ORAD-IDDC TO W-IDDC-B6                                   
282300            PERFORM IMS-GU-WDB601                                         
282400         END-IF                                                           
282500         IF DCS-CDC                                                       
282600           PERFORM IMS-GHU-ARTM-ARTM01                                    
282700           IF SEGMENT-FINNS                                               
282800             PERFORM S04A-BACKA-RAD-K9                                    
282900           END-IF                                                         
283000         ELSE                                                             
283100           IF OHUV-KDORDKL = +0                                           
283200              PERFORM S04B-BACKA-NYVORKO                                  
283300           END-IF                                                         
283400           MOVE WS-IDDC    TO W-IDDC                                      
283500           PERFORM IMS-GHU-ARTS-WDK711                                    
283600           IF OHUV-KDORDKL = +0 OR +1                                     
283700             COMPUTE SLAG-KVOKS-DAG =                                     
283800                     SLAG-KVOKS-DAG - ANNULLERAT-ANTAL                    
283900           ELSE                                                           
284000             IF OHUV-KDORDKL = +2 OR +3 OR +4                             
284100               IF ORAD-KVOKS-PREL = ZERO                                  
284200                 COMPUTE SLAG-KVOKS-BULK =                                
284300                         SLAG-KVOKS-BULK - ANNULLERAT-ANTAL               
284400               END-IF                                                     
284500             END-IF                                                       
284600           END-IF                                                         
284700                                                                          
284800           MOVE ORAD-IDDISTR TO TEST-IDDISTR                              
284900           IF DIST18-SCRAP-NDC                                            
285000             MOVE 'N'       TO SLAG-FLSKROT-BEORD                         
285100           END-IF                                                         
285200                                                                          
285300           PERFORM IMS-REPL-WDK7                                          
285400         END-IF                                                           
285500       END-IF                                                             
285600       PERFORM S17-FIXA-REFILL-TRANSFER                                   
285700     END-IF                                                               
285800                                                                          
285900     PERFORM IMS-GU-WDK611                                                
286000     MOVE    CLAG-KVQPACK-1 TO WS-KVQPACK-1                               
286100     MOVE    CLAG-TIDISPIN  TO WS-TIDISPIN                                
286200                                                                          
286300     PERFORM S07-SKAPA-AVSR                                               
286400                                                                          
286500     MOVE +0 TO WS-ANTOBKR                                                
286600     PERFORM S08-SKAPA-ORDERBEKR                                          
286700                                                                          
286800     PERFORM S09-SKAPA-TRANSAR                                            
286900                                                                          
287000     IF OBKR-KDORDBEK = 83                                                
287100       PERFORM S04D-DELETE-PRICE-Q-LINE                                   
287200     END-IF                                                               
287300                                                                          
287400     PERFORM IMS-DLET-WDQ4                                                
287500     .                                                                    
287600     EJECT                                                                
287700 S04A-BACKA-RAD-K9 SECTION.                                               
287800                                                                          
287900     MOVE 'STA S04A-BACKA-K9  '           TO   WS-PGM-POSITION            
288000     IF OHUV-KDORDKL = ZERO                                               
288100                                                                          
288200       PERFORM S04B-BACKA-NYVORKO                                         
288300                                                                          
288400       IF ORAD-IDKAMPRF NOT > ZERO                                        
288500         COMPUTE ART-KVOKS-VOR =                                          
288600         ART-KVOKS-VOR - SPAR-ORAD-KVBEART-Q                              
288700       END-IF                                                             
288800       COMPUTE ART-KVPREAVB-VOR =                                         
288900         ART-KVPREAVB-VOR - SPAR-ORAD-KVPREAVB                            
289000     END-IF                                                               
289100                                                                          
289200     IF OHUV-KDORDKL = +1                                                 
289300       IF ORAD-IDKAMPRF NOT > ZERO                                        
289400         COMPUTE ART-KVOKS-DAG =                                          
289500         ART-KVOKS-DAG - SPAR-ORAD-KVBEART-Q                              
289600       END-IF                                                             
289700       COMPUTE ART-KVPRERO-DAG =                                          
289800         ART-KVPRERO-DAG - SPAR-ORAD-KVPRERO                              
289900       COMPUTE ART-KVPREAVB-DAG =                                         
290000         ART-KVPREAVB-DAG - SPAR-ORAD-KVPREAVB                            
290100     END-IF                                                               
290200                                                                          
290300     IF OHUV-KDORDKL = +2 OR +3 OR +4                                     
290400       IF ORAD-IDKAMPRF NOT > ZERO                                        
290500         COMPUTE ART-KVOKS-BULK =                                         
290600         ART-KVOKS-BULK - SPAR-ORAD-KVBEART-Q                             
290700       END-IF                                                             
290800       COMPUTE ART-KVPRERO-BULK =                                         
290900         ART-KVPRERO-BULK - SPAR-ORAD-KVPRERO                             
291000       COMPUTE ART-KVPREAVB-BULK =                                        
291100         ART-KVPREAVB-BULK - SPAR-ORAD-KVPREAVB                           
291200     END-IF                                                               
291300                                                                          
291400     PERFORM IMS-REPL-ARTM                                                
291500     .                                                                    
291600     EJECT                                                                
291700 S04B-BACKA-NYVORKO SECTION.                                              
291800                                                                          
291900     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
292000     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
292100     MOVE OHUV-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
292200                                    W-A601KY-MAX-IDDISTR                  
292300     MOVE OHUV-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
292400                                    W-A601KY-MAX-IDKUNDNR                 
292500     MOVE OHUV-IDKUNDRF          TO W-A601KY-MIN-IDKUNDRF                 
292600                                    W-A601KY-MAX-IDKUNDRF                 
292700     MOVE OHUV-TIREGDAT          TO W-A601KY-MIN-TIREGDAT                 
292800                                    W-A601KY-MAX-TIREGDAT                 
292900     MOVE ORAD-IDARTNR           TO W-A601KY-MIN-IDARTNR                  
293000                                    W-A601KY-MAX-IDARTNR                  
293100                                                                          
293200     PERFORM IMS-GHN-WDA6B                                                
293300     PERFORM UNTIL SEGMENT-SAKNAS                                         
293400                OR BASEN-SLUT                                             
293500                                                                          
293600         IF  VOR-KDVORATG > '1'                                           
293700         AND VOR-KDVORATG < '6'                                           
293800         AND VOR-KVPREAVB        = SPAR-ORAD-KVBEART-Q                    
293900             MOVE '8'            TO VOR-KDVORATG                          
294000             MOVE 83             TO VOR-KDORDBEK                          
294100             MOVE 0              TO VOR-KVPREAVB                          
294200             IF VOR-TIKLAR = ZERO                                         
294300                MOVE WS-TINUDAT     TO VOR-TIKLAR                         
294400                COMPUTE VOR-TIKLATID    = WS-TINUTID                      
294500                                        / 100                             
294600                END-COMPUTE                                               
294700             END-IF                                                       
294800             PERFORM IMS-REPL-WDA6B                                       
294900         END-IF                                                           
295000                                                                          
295100         PERFORM IMS-GHN-WDA6B                                            
295200     END-PERFORM                                                          
295300     .                                                                    
295400     EJECT                                                                
295500 S04C-DELBACKA-NYVORKO SECTION.                                           
295600                                                                          
295700     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
295800     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
295900     MOVE OHUV-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
296000                                    W-A601KY-MAX-IDDISTR                  
296100     MOVE OHUV-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
296200                                    W-A601KY-MAX-IDKUNDNR                 
296300     MOVE OHUV-IDKUNDRF          TO W-A601KY-MIN-IDKUNDRF                 
296400                                    W-A601KY-MAX-IDKUNDRF                 
296500     MOVE OHUV-TIREGDAT          TO W-A601KY-MIN-TIREGDAT                 
296600                                    W-A601KY-MAX-TIREGDAT                 
296700     MOVE ORAD-IDARTNR           TO W-A601KY-MIN-IDARTNR                  
296800                                    W-A601KY-MAX-IDARTNR                  
296900                                                                          
297000     PERFORM IMS-GHN-WDA6B                                                
297100     PERFORM UNTIL SEGMENT-SAKNAS                                         
297200                OR BASEN-SLUT                                             
297300                                                                          
297400         IF  VOR-KDVORATG > '1'                                           
297500         AND VOR-KDVORATG < '6'                                           
297600         AND VOR-KVPREAVB >= MINSKAT-ANTAL                                
297700             SUBTRACT MINSKAT-ANTAL FROM VOR-KVPREAVB                     
297800             IF  VOR-KVPREAVB = 0                                         
297900                 MOVE '8'        TO VOR-KDVORATG                          
298000                 MOVE 83         TO VOR-KDORDBEK                          
298100                 IF VOR-TIKLAR = ZERO                                     
298200                    MOVE WS-TINUDAT TO VOR-TIKLAR                         
298300                    COMPUTE VOR-TIKLATID = WS-TINUTID                     
298400                                            / 100                         
298500                    END-COMPUTE                                           
298600                 END-IF                                                   
298700             END-IF                                                       
298800             PERFORM IMS-REPL-WDA6B                                       
298900         END-IF                                                           
299000                                                                          
299100         PERFORM IMS-GHN-WDA6B                                            
299200     END-PERFORM                                                          
299300     .                                                                    
299400     EJECT                                                                
299500 S04D-DELETE-PRICE-Q-LINE SECTION.                                        
299600                                                                          
299700     MOVE ORAD-IDDISTR TO TEST-IDDISTR                                    
299800                                                                          
299900     IF DIST79-DEALER-PRICE                                               
300000       IF ORAD-IDPRQUES > ZERO                                            
300100         INITIALIZE PRQU-W335PRQU                                         
300200         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
300300         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
300400         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
300500         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
300600         MOVE 4                  TO PRQU-KDCALL                           
300700         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
300800                                            PRQU-WDC7-PCB                 
300900                                            PRQU-SJKO-WDK6-PCB            
301000       END-IF                                                             
301100     END-IF                                                               
301200     .                                                                    
301300     EJECT                                                                
301400 S05-BACKA-BIPACKADE-RADER SECTION.                                       
301500                                                                          
301600     MOVE 'STA S05-BACKA-BIPACK '         TO   WS-PGM-POSITION            
301700     MOVE ORAD-KVBEART-Q TO ANNULLERAT-ANTAL                              
301800                            SPAR-ORAD-KVBEART-Q                           
301900     MOVE ORAD-IDDISTR           TO   W-RAD-IDDISTR                       
302000     MOVE ORAD-IDKUNDNR          TO   W-RAD-IDKUNDNR                      
302100     MOVE ORAD-IDKUNDRF-RO (3:5) TO   W-RAD-IDKUNDRF                      
302200     MOVE ORAD-IDARTNR           TO   W-RAD-IDARTNR                       
302300                                      W-IDARTNR                           
302400     MOVE ORAD-IDLOPNR-RO        TO   W-RAD-IDLOPNR                       
302500                                                                          
302600     IF ORAD-IDKAMPRF > ZERO                                              
302700       MOVE ORAD-IDKAMPRF       TO W-KAMP-IDKAMPRF                        
302800       MOVE ORAD-IDDC           TO W-KAMP-IDDC                            
302900       MOVE ORAD-IDARTNR        TO W-KART-IDARTNR                         
303000       MOVE ORAD-IDDISTR        TO W-KMRK-IDDISTR-FOM                     
303100       MOVE ORAD-IDDISTR        TO W-KMRK-IDDISTR-TOM                     
303200       MOVE ORAD-IDKUNDNR       TO W-KMRK-IDKUNDNR-FOM                    
303300       MOVE ORAD-IDKUNDNR       TO W-KMRK-IDKUNDNR-TOM                    
303400       PERFORM S11-BACKA-KAMPANJ                                          
303500     END-IF                                                               
303600                                                                          
303700     IF OHUV-IDSYSTEM = 'W216' AND OHUV-FLORDSPE = JA                     
303800       CONTINUE                                                           
303900     ELSE                                                                 
304000       IF ORAD-IDLEVNR = SPACE                                            
304100         PERFORM IMS-GHU-ARTM-ARTM01                                      
304200                                                                          
304300         IF SEGMENT-FINNS                                                 
304400           IF ORAD-KDTPOTYP > ZERO AND                                    
304500             ORAD-TIRODAT NOT > ZERO                                      
304600             PERFORM S05A-AENDRA-WDK901                                   
304700                                                                          
304800             PERFORM IMS-REPL-ARTM                                        
304900           END-IF                                                         
305000         END-IF                                                           
305100       END-IF                                                             
305200     END-IF                                                               
305300                                                                          
305400     PERFORM S05B-UPPDATERA-WDA5                                          
305500                                                                          
305600     PERFORM IMS-GU-WDK611                                                
305700     MOVE    CLAG-KVQPACK-1 TO WS-KVQPACK-1                               
305800     MOVE    CLAG-TIDISPIN  TO WS-TIDISPIN                                
305900                                                                          
306000     PERFORM S07-SKAPA-AVSR                                               
306100                                                                          
306200     MOVE +0 TO WS-ANTOBKR                                                
306300     PERFORM S08-SKAPA-ORDERBEKR                                          
306400                                                                          
306500     PERFORM S09-SKAPA-TRANSAR                                            
306600                                                                          
306700     PERFORM IMS-GHNP-ORQI-WDQ212                                         
306800                                                                          
306900     IF SEGMENT-FINNS                                                     
307000       MOVE 0  TO ARB-KDROPACK                                            
307100       PERFORM IMS-REPL-WDQ212                                            
307200     END-IF                                                               
307300                                                                          
307400     PERFORM IMS-DLET-WDQ4                                                
307500     .                                                                    
307600     EJECT                                                                
307700 S05A-AENDRA-WDK901 SECTION.                                              
307800                                                                          
307900     MOVE 'STA S05A-AENDRA-K9   '         TO   WS-PGM-POSITION            
308000     IF ORAD-KDORDKL = ZERO                                               
308100       IF ORAD-KVPREAVB > ZERO                                            
308200          COMPUTE ART-KVPREAVB-VOR =                                      
308300          ART-KVPREAVB-VOR - ORAD-KVPREAVB                                
308400       END-IF                                                             
308500     END-IF                                                               
308600                                                                          
308700     IF ORAD-KDORDKL = +1                                                 
308800       IF ORAD-KVPREAVB > ZERO                                            
308900         COMPUTE ART-KVPREAVB-DAG =                                       
309000         ART-KVPREAVB-DAG - ORAD-KVPREAVB                                 
309100       END-IF                                                             
309200       IF ORAD-KVPRERO > ZERO                                             
309300          COMPUTE ART-KVPRERO-DAG =                                       
309400          ART-KVPRERO-DAG - ORAD-KVPRERO                                  
309500       END-IF                                                             
309600     END-IF                                                               
309700                                                                          
309800     IF ORAD-KDORDKL = +2 OR +3 OR +4                                     
309900       IF ORAD-KVPREAVB > ZERO                                            
310000          COMPUTE ART-KVPREAVB-BULK =                                     
310100          ART-KVPREAVB-BULK - ORAD-KVPREAVB                               
310200       END-IF                                                             
310300       IF ORAD-KVPRERO > ZERO                                             
310400          COMPUTE ART-KVPRERO-BULK =                                      
310500          ART-KVPRERO-BULK - ORAD-KVPRERO                                 
310600       END-IF                                                             
310700     END-IF                                                               
310800     .                                                                    
310900     EJECT                                                                
311000 S05B-UPPDATERA-WDA5 SECTION.                                             
311100                                                                          
311200     MOVE 'STA S05B-UPDATE-A5   '         TO   WS-PGM-POSITION            
311300     PERFORM IMS-GHU-ORDP-WDA501                                          
311400                                                                          
311500     IF SEGMENT-FINNS                                                     
311600       IF RAD-KVART = ORAD-KVBEART-Q                                      
311700         MOVE '00000     '       TO   RAD-IDKUNDRF-LEV                    
311800         MOVE '3'                TO   RAD-KDSTARAD                        
311900         PERFORM IMS-REPL-WDA5                                            
312000       ELSE                                                               
312100         COMPUTE RAD-KVART = RAD-KVART - ORAD-KVBEART-Q                   
312200         PERFORM IMS-REPL-WDA5                                            
312300         MOVE ORAD-KVBEART-Q     TO RAD-KVART                             
312400         MOVE '00000     '       TO RAD-IDKUNDRF-LEV                      
312500         MOVE '3'                TO RAD-KDSTARAD                          
312600         ADD +1                  TO RAD-IDLOPNR                           
312700         PERFORM IMS-ISRT-WDA5                                            
312800         PERFORM UNTIL SEGMENT-FINNS                                      
312900           ADD +1                TO RAD-IDLOPNR                           
313000           PERFORM IMS-ISRT-WDA5                                          
313100         END-PERFORM                                                      
313200       END-IF                                                             
313300     END-IF                                                               
313400     .                                                                    
313500     EJECT                                                                
313600 S06-SPARA-ARTIKEL SECTION.                                               
313700                                                                          
313800     MOVE ORAD-IDARTNR TO W-ORAD-IDARTNR-MIN                              
313900     MOVE ORAD-IDARTNR TO W-ORAD-IDARTNR-MAX                              
314000     MOVE ORAD-IDARTNR TO W-ORAD-IDARTNR-MIN-MIN                          
314100     MOVE ORAD-IDORDER TO W-ORAD-IDORDER-MIN                              
314200     MOVE ORAD-IDORDER TO W-ORAD-IDORDER-MIN-MIN                          
314300     MOVE ORAD-IDORDER TO W-ORAD-IDORDER-MAX                              
314400     MOVE ORAD-IDORDER TO W-ORAD-IDORDER-MAX-MAX                          
314500     MOVE ORAD-IDDC    TO W-ORAD-IDDC-MIN                                 
314600     MOVE ORAD-IDDC    TO W-ORAD-IDDC-MAX                                 
314700     MOVE ORAD-IDDC    TO W-ORAD-IDDC-MIN-MIN                             
314800     MOVE ORAD-IDDC    TO W-ORAD-IDDC-MAX-MAX                             
314900     MOVE ORAD-ADLAGOMR TO W-ORAD-ADLAGOMR-MIN                            
315000     MOVE ORAD-ADLAGOMR TO W-ORAD-ADLAGOMR-MIN-MIN                        
315100     MOVE ORAD-ADGANG TO W-ORAD-ADGANG-MIN                                
315200     MOVE ORAD-ADGANG TO W-ORAD-ADGANG-MIN-MIN                            
315300     MOVE ORAD-ADPLATS TO W-ORAD-ADPLATS-MIN                              
315400     MOVE ORAD-ADPLATS TO W-ORAD-ADPLATS-MIN-MIN                          
315500     MOVE ORAD-IDLOPNR TO W-ORAD-IDLOPNR-MIN                              
315600     MOVE ORAD-IDLOPNR TO W-ORAD-IDLOPNR-MIN-MIN                          
315700     .                                                                    
315800     EJECT                                                                
315900 S07-SKAPA-AVSR SECTION.                                                  
316000                                                                          
316100     MOVE 'STA S07-SKAPA-AVSR   '         TO   WS-PGM-POSITION            
316200     MOVE JA TO AVSR-SW                                                   
316300     MOVE 2                  TO   AVSR-KDCALL                             
316400     MOVE OHUV-IDORDER       TO   AVSR-IDORDER                            
316500     MOVE OHUV-KDORDKL       TO   AVSR-KDORDKL                            
316600     MOVE ZERO               TO   AVSR-KDFRAKT                            
316700     MOVE ZERO               TO   AVSR-KDROPACK                           
316800     MOVE MSGI-TILOKDAT      TO   AVSR-TIREGDAT                           
316900     MOVE MSGI-TILOKTID      TO   AVSR-TIHHMM                             
317000                                                                          
317100     MOVE ORAD-ADLAGOMR      TO   AVSR-ADLAGOMR(AVSR-INDX)                
317200     MOVE ORAD-IDLEVNR       TO   AVSR-IDLEVNR(AVSR-INDX)                 
317300     MOVE ORAD-IDDC          TO   AVSR-IDDC(AVSR-INDX)                    
317400     MOVE ORAD-KDSPEEMB      TO   AVSR-KDSPEEMB(AVSR-INDX)                
317500     MOVE ANNULLERAT-ANTAL   TO   AVSR-KVANNANT(AVSR-INDX)                
317600     MOVE ORAD-KVBEART-Q     TO   AVSR-KVBEART-Q(AVSR-INDX)               
317700     MOVE ORAD-PRARTNTO      TO   AVSR-PRARTNTO(AVSR-INDX)                
317800     MOVE ORAD-PRAVCOST      TO   AVSR-PRAVCOST(AVSR-INDX)                
317900     MOVE ORAD-VKART         TO   AVSR-VKART(AVSR-INDX)                   
318000     MOVE ORAD-VLARTNTO      TO   AVSR-VLARTNTO(AVSR-INDX)                
318100     MOVE SPACE              TO   AVSR-KDORDSTA(AVSR-INDX)                
318200     MOVE +0                 TO   AVSR-KDVIA   (AVSR-INDX)                
318300     MOVE +0                 TO   AVSR-KVDAGAR-DIFF(AVSR-INDX)            
318400     MOVE +0                 TO   AVSR-TISKEPPN-DDC(AVSR-INDX)            
318500     MOVE ORAD-DEAL-PR-LINE  TO   AVSR-DEAL-PR-LINE(AVSR-INDX)            
318600     .                                                                    
318700     EJECT                                                                
318800 S08-SKAPA-ORDERBEKR SECTION.                                             
318900                                                                          
319000     MOVE 'STA S08-SKAPA-ORDBEK '         TO   WS-PGM-POSITION            
319100     IF WS-ANTOBKR = +0                                                   
319200       MOVE OHUV-IDORDER         TO   W-OBKR-IDORDER-MIN                  
319300                                      W-OBKR-IDORDER-MAX                  
319400       MOVE ORAD-IDARTNR         TO   W-OBKR-IDARTNR-MIN                  
319500                                      W-OBKR-IDARTNR-MAX                  
319600       MOVE +1                   TO   W-OBKR-IDLOPNR-MIN                  
319700                                      W-OBKR-IDLOPNR-MAX                  
319800                                      W-OBKR-IDLOPNR-MAX                  
319900       MOVE +1                   TO   W-OBKR-IDSEKVNR-MIN                 
320000                                      W-OBKR-IDSEKVNR-MAX                 
320100       PERFORM IMS-GU-ORQM-ORQM01                                         
320200                                                                          
320300       PERFORM UNTIL SEGMENT-SAKNAS                                       
320400         ADD +1 TO W-OBKR-IDLOPNR-MIN                                     
320500                   W-OBKR-IDLOPNR-MAX                                     
320600         PERFORM IMS-GU-ORQM-ORQM01                                       
320700       END-PERFORM                                                        
320800                                                                          
320900     END-IF                                                               
321000     MOVE OHUV-IDORDER           TO   OBKR-IDORDER                        
321100     MOVE ORAD-IDARTNR           TO   OBKR-IDARTNR                        
321200     MOVE W-OBKR-IDLOPNR-MIN     TO   OBKR-IDLOPNR                        
321300     MOVE 1                      TO   OBKR-IDSEKVNR                       
321400     MOVE ORAD-IDDC              TO   OBKR-IDDC                           
321500     MOVE ORAD-IDDC-RO           TO   OBKR-IDDC-RO                        
321600     MOVE SPACE                  TO   OBKR-BEERS                          
321700     MOVE SPACE                  TO   OBKR-IDBIL                          
321800     MOVE OHUV-BEKUNDRF          TO   OBKR-BEKUNDRF                       
321900     MOVE ORAD-BERADREF          TO   OBKR-BERADREF                       
322000     MOVE ORAD-BEVOLREF          TO   OBKR-BEVOLREF                       
322100     MOVE ORAD-IDKAMPRF          TO   OBKR-IDKAMPRF                       
322200     MOVE ZERO                   TO   OBKR-DIERS-KVOT                     
322300     MOVE ORAD-FLAKPLOC          TO   OBKR-FLAKPLOC                       
322400     MOVE ORAD-FLINVEST          TO   OBKR-FLINVEST                       
322500     MOVE 'J'                    TO   OBKR-FLOBOK                         
322600     MOVE 'J'                    TO   OBKR-FLOBTRAN                       
322700     MOVE 'N'                    TO   OBKR-FLOBPRT                        
322800     MOVE ORAD-FLPRTILL          TO   OBKR-FLPRTILL                       
322900     MOVE ORAD-FLRESTN           TO   OBKR-FLRESTN                        
323000     MOVE JA                     TO   OBKR-FLSLATT                        
323100     MOVE ORAD-FLTILLK           TO   OBKR-FLTILLK                        
323200     MOVE ZERO                   TO   OBKR-IDARTNR-TILLK                  
323300     MOVE ORAD-IDDISTR           TO   OBKR-IDDISTR                        
323400     MOVE ORAD-IDKUNDNR          TO   OBKR-IDKUNDNR                       
323500     MOVE ORAD-IDKUNDRF          TO   OBKR-IDKUNDRF                       
323600     MOVE ORAD-IDKUNDRF-RO       TO   OBKR-IDKUNDRF-RO                    
323700     MOVE ORAD-IDLEVNR           TO   OBKR-IDLEVNR                        
323800     MOVE ORAD-IDLOPNR-RO        TO   OBKR-IDLOPNR-RO                     
323900     MOVE IDPGM                  TO   OBKR-IDPGM                          
324000     MOVE ORAD-IDSYSTEM          TO   OBKR-IDSYSTEM                       
324100     MOVE ORAD-KDDSP             TO   OBKR-KDDSP                          
324200     MOVE ZERO                   TO   OBKR-KDERS                          
324300     MOVE ORAD-KDKVBRYT          TO   OBKR-KDKVBRYT                       
324400     MOVE ORAD-KDPRTYP           TO   OBKR-KDPRTYP                        
324500     MOVE ORAD-KDTPOTYP          TO   OBKR-KDTPOTYP                       
324600     MOVE ORAD-KDVRINFO          TO   OBKR-KDVRINFO                       
324700     MOVE ORAD-KDOI              TO   OBKR-KDOI                           
324800     MOVE ORAD-CLEARGROUP        TO   OBKR-CLEARGROUP                     
324900     MOVE ZERO                   TO   OBKR-KVANNANT                       
325000     MOVE ZERO                   TO   OBKR-KVAVBART                       
325100     MOVE ORAD-KVBEART           TO   OBKR-KVBEART                        
325200     MOVE ORAD-KVBEART-Q         TO   OBKR-KVBEART-Q                      
325300     MOVE ZERO                   TO   OBKR-KVBEART-TILLK                  
325400     MOVE ZERO                   TO   OBKR-KVPREAVB                       
325500     MOVE ZERO                   TO   OBKR-KVPRERO                        
325600     MOVE WS-KVQPACK-1           TO   OBKR-KVQPACK                        
325700     MOVE ZERO                   TO   OBKR-KVRO                           
325800     MOVE ORAD-KVSLATT           TO   OBKR-KVSLATT                        
325900     MOVE ORAD-PRARTNTO          TO   OBKR-PRARTNTO                       
326000     MOVE ORAD-DEAL-PR-LINE      TO   OBKR-DEAL-PR-LINE                   
326100     MOVE ORAD-PRBPRIS           TO   OBKR-PRBPRIS                        
326200     MOVE ORAD-REKSIFFR          TO   OBKR-REKSIFFR                       
326300     MOVE ZERO                   TO   OBKR-REKSIFFR-TILLK                 
326400     MOVE ORAD-RERF-RAD          TO   OBKR-RERF-RAD                       
326500     MOVE ZERO                   TO   OBKR-TIDISPIN                       
326600     MOVE OHUV-TIREGDAT          TO   OBKR-TIORDREG                       
326700     MOVE ORAD-TIPRIS            TO   OBKR-TIPRIS                         
326800     MOVE MSGI-TILOKDAT          TO   OBKR-TIREGDAT                       
326900     MOVE MSGI-TILOKTID          TO   WS-TIHHMM                           
327000     MOVE WS-TIHHMMSS            TO   OBKR-TIREGTID                       
327100     MOVE ZERO                   TO   OBKR-TIRODAT                        
327200                                                                          
327300     MOVE OHUV-TIREGDAT          TO WS-AAMMDD                             
327400     IF WS-AAMMDD(1:2) < 50                                               
327500       MOVE 20                   TO WS-CENTURY                            
327600     ELSE                                                                 
327700       MOVE 19                   TO WS-CENTURY                            
327800     END-IF                                                               
327900     COMPUTE OBKR-TITIORDD-9KOMPL = WS-9KOMPL - WS-9KOMPL-DATUM           
328000                                                                          
328100     MOVE ORAD-TITPO             TO   OBKR-TITPO                          
328200                                                                          
328300     MOVE OBKR-TIREGDAT          TO WS-AAMMDD                             
328400     COMPUTE OBKR-TITIREGD-9KOMPL = WS-9KOMPL - WS-9KOMPL-DATUM           
328500                                                                          
328600     MOVE ARB-KDFRAKT            TO   OBKR-KDFRAKT                        
328700     MOVE OHUV-KDORDKL           TO   OBKR-KDORDKL                        
328800                                                                          
328900     IF ORAD-IDKUNDRF-RO = '0000000   ' OR                                
329000        ORAD-IDKUNDRF-RO = ORAD-IDKUNDRF                                  
329100       MOVE 83                   TO   OBKR-KDORDBEK                       
329200       MOVE ANNULLERAT-ANTAL     TO   OBKR-KVANNANT                       
329300     ELSE                                                                 
329400       MOVE 91                   TO   OBKR-KDORDBEK                       
329500       MOVE ANNULLERAT-ANTAL     TO   OBKR-KVRO                           
329600     END-IF                                                               
329700                                                                          
329800     MOVE OHUV-KDORDTYP-LDC      TO OBKR-KDORDTYP-LDC                     
329900     MOVE OHUV-TIREPDAT          TO OBKR-TIREPDAT                         
330000     MOVE ORAD-IDKUNDRF-WIP      TO OBKR-IDKUNDRF-WIP                     
330100     MOVE ZERO                   TO OBKR-TIDLEVDAT                        
330200     MOVE ORAD-PRAVCOST          TO OBKR-PRAVCOST                         
330300     MOVE ORAD-KDVALISO          TO OBKR-KDVALISO                         
330400                                                                          
330500     PERFORM IMS-ISRT-ORQM-ORQM01                                         
330600                                                                          
330700     PERFORM UNTIL SEGMENT-FINNS                                          
330800       ADD 1 TO OBKR-IDSEKVNR                                             
330900       PERFORM IMS-ISRT-ORQM-ORQM01                                       
331000     END-PERFORM                                                          
331100     ADD 1 TO WS-ANTOBKR                                                  
331200     .                                                                    
331300     EJECT                                                                
331400 S09-SKAPA-TRANSAR SECTION.                                               
331500                                                                          
331600     MOVE 'STA S09-SKAPA-TRANSAR'         TO   WS-PGM-POSITION            
331700     IF ORAD-IDKUNDRF-RO = '0000000   ' OR                                
331800       ORAD-IDKUNDRF-RO = ORAD-IDKUNDRF                                   
331900                                                                          
332000       IF ORAD-KDOI NOT = SPACE                                           
332100         PERFORM S09A-SKAPA-W2I109MID                                     
332200       END-IF                                                             
332300                                                                          
332400       PERFORM S09B-SKAPA-RYC                                             
332500                                                                          
332600     ELSE                                                                 
332700                                                                          
332800       PERFORM S09C-SKAPA-RYB                                             
332900                                                                          
333000     END-IF                                                               
333100     .                                                                    
333200     EJECT                                                                
333300                                                                          
333400 S09A-SKAPA-W2I109MID SECTION.                                            
333500                                                                          
333600*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
333700     MOVE ORAD-IDARTNR       TO BYT03-IDARTNR                             
333800     IF NOT BYT03-OBJEKT                                                  
333900                                                                          
334000        MOVE 'STA S09A-2109        '      TO   WS-PGM-POSITION            
334100        MOVE 2109-INDX        TO   2109-MID2-KVANTART                     
334200        MOVE ORAD-IDARTNR     TO 2109-MID2-IDARTNR (2109-INDX)            
334300        MOVE OHUV-IDDC-PRIM   TO 2109-MID2-IDDC(2109-INDX)                
334400        MOVE '-'              TO 2109-MID2-KDTECKEN(2109-INDX)            
334500        MOVE ORAD-KDOI        TO 2109-MID2-KDOI (2109-INDX)               
334600        MOVE ORAD-CLEARGROUP  TO 2109-MID2-CLEARGROUP (2109-INDX)         
334700        MOVE ANNULLERAT-ANTAL TO 2109-MID2-KVOI (2109-INDX)               
334800        MOVE ORAD-TIREGDAT    TO 2109-MID2-TIUPPDAT(2109-INDX)            
334900                                                                          
335000        ADD +1 TO 2109-INDX                                               
335100        IF 2109-INDX > MAX-2109-INDX                                      
335200          PERFORM K-STARTA-2109                                           
335300          MOVE ZERO TO 2109-MID2-KVANTART                                 
335400        END-IF                                                            
335500     END-IF                                                               
335600     .                                                                    
335700     EJECT                                                                
335800                                                                          
335900 S09B-SKAPA-RYC SECTION.                                                  
336000                                                                          
336100     MOVE 'STA S09B-RYC         '         TO   WS-PGM-POSITION            
336200     MOVE 'RYC'               TO   W-RYC-IDPTYP                           
336300     MOVE ORAD-BERADREF       TO   W-RYC-BERADREF                         
336400     MOVE ORAD-BEVOLREF       TO   W-RYC-BEVOLREF                         
336500     MOVE ORAD-FLTILLK        TO   W-RYC-FLTILLK                          
336600     MOVE ORAD-IDARTNR        TO   W-RYC-IDARTNR                          
336700     MOVE ORAD-IDDISTR        TO   W-RYC-IDDISTR                          
336800     MOVE ORAD-IDKUNDNR       TO   W-RYC-IDKUNDNR                         
336900     MOVE ORAD-IDKUNDRF       TO   W-RYC-IDKUNDRF                         
337000     MOVE ORAD-IDKUNDRF-RO    TO   W-RYC-IDKUNDRF-RO                      
337100     MOVE ORAD-KDDSP          TO   W-RYC-KDDSP                            
337200     MOVE OHUV-KDFAKTYP       TO   W-RYC-KDFAKTYP                         
337300     MOVE ARB-KDFRAKT         TO   W-RYC-KDFRAKT                          
337400     MOVE 83                  TO   W-RYC-KDORDBEK                         
337500     MOVE OHUV-KDORDKL        TO   W-RYC-KDORDKL                          
337600     MOVE ORAD-KDKVBRYT       TO   W-RYC-KDKVBRYT                         
337700     MOVE ORAD-KDVRINFO       TO   W-RYC-KDVRINFO                         
337800     MOVE 0                   TO   W-RYC-KDVRTPO                          
337900     MOVE ANNULLERAT-ANTAL    TO   W-RYC-KVANNANT                         
338000     MOVE ORAD-REKSIFFR       TO   W-RYC-REKSIFFR                         
338100     MOVE OHUV-TIREGDAT       TO   W-RYC-TIORDREG                         
338200     MOVE ORAD-TIRODAT        TO   W-RYC-TIRODAT                          
338300     ACCEPT TIAAMMDD FROM DATE                                            
338400     ACCEPT TIKLOCK FROM TIME                                             
338500     MOVE +1 TO IDLOGLOP                                                  
338600     MOVE 'RYC' TO IDPTYP                                                 
338700     MOVE W-RYCPOST TO LOGGPOST                                           
338800                                                                          
338900     MOVE SPACE               TO W-RYCS-WDGZRYCS                          
339000     MOVE ORAD-IDDC           TO W-RYCS-IDDC                              
339100     MOVE W-RYCSPOST TO SORTPOST                                          
339200                                                                          
339300     PERFORM IMS-ISRT-ZZAC-ZZAC01                                         
339400                                                                          
339500     PERFORM UNTIL SEGMENT-FINNS                                          
339600       ADD +1 TO IDLOGLOP                                                 
339700       PERFORM IMS-ISRT-ZZAC-ZZAC01                                       
339800     END-PERFORM                                                          
339900     .                                                                    
340000     EJECT                                                                
340100 S09C-SKAPA-RYB SECTION.                                                  
340200                                                                          
340300     MOVE 'STA S09C-RYB         '         TO   WS-PGM-POSITION            
340400     MOVE 'RYB'               TO   W-RYB-IDPTYP                           
340500     MOVE ORAD-BERADREF       TO   W-RYB-BERADREF                         
340600     MOVE ORAD-BEVOLREF       TO   W-RYB-BEVOLREF                         
340700     MOVE ORAD-FLTILLK        TO   W-RYB-FLTILLK                          
340800     MOVE ORAD-IDARTNR        TO   W-RYB-IDARTNR                          
340900     MOVE ORAD-IDDISTR        TO   W-RYB-IDDISTR                          
341000     MOVE ORAD-IDKUNDNR       TO   W-RYB-IDKUNDNR                         
341100     MOVE ORAD-IDKUNDRF       TO   W-RYB-IDKUNDRF                         
341200     MOVE ORAD-IDKUNDRF-RO    TO   W-RYB-IDKUNDRF-RO                      
341300     MOVE ORAD-IDDC           TO   W-RYB-IDDC                             
341400     MOVE ORAD-KDDSP          TO   W-RYB-KDDSP                            
341500     MOVE OHUV-KDFAKTYP       TO   W-RYB-KDFAKTYP                         
341600     MOVE ARB-KDFRAKT         TO   W-RYB-KDFRAKT                          
341700     MOVE +1                  TO   W-RYB-KDLIDEL                          
341800     MOVE 91                  TO   W-RYB-KDORDBEK                         
341900     MOVE ORAD-KDORDKL        TO   W-RYB-KDORDKL                          
342000     MOVE ORAD-KDKVBRYT       TO   W-RYB-KDKVBRYT                         
342100     MOVE +1                  TO   W-RYB-KDRO                             
342200     MOVE ORAD-KDVRINFO       TO   W-RYB-KDVRINFO                         
342300     MOVE ORAD-KVBEART-Q      TO   W-RYB-KVRO                             
342400     MOVE ORAD-REKSIFFR       TO   W-RYB-REKSIFFR                         
342500     MOVE WS-TIDISPIN         TO   W-RYB-TIDISPIN                         
342600     MOVE OHUV-TIREGDAT       TO   W-RYB-TIORDREG                         
342700     MOVE ORAD-TIRODAT        TO   W-RYB-TIRODAT                          
342800     ACCEPT TIAAMMDD FROM DATE                                            
342900     ACCEPT TIKLOCK FROM TIME                                             
343000     MOVE +1 TO IDLOGLOP                                                  
343100     MOVE 'RYB' TO IDPTYP                                                 
343200     MOVE W-RYBPOST TO LOGGPOST                                           
343300     MOVE SPACE TO SORTPOST                                               
343400                                                                          
343500     PERFORM IMS-ISRT-ZZAC-ZZAC01                                         
343600                                                                          
343700     PERFORM UNTIL SEGMENT-FINNS                                          
343800       ADD +1 TO IDLOGLOP                                                 
343900       PERFORM IMS-ISRT-ZZAC-ZZAC01                                       
344000     END-PERFORM                                                          
344100     .                                                                    
344200     EJECT                                                                
344300 S10-SKAPA-AVSO SECTION.                                                  
344400                                                                          
344500     MOVE WS-IDDISTR         TO   AVSO-IDDISTR                            
344600     MOVE WS-IDKUNDNR        TO   AVSO-IDKUNDNR                           
344700     MOVE WS-IDKUNDRF        TO   AVSO-IDKUNDRF                           
344800     MOVE WS-IDORDER         TO   AVSO-IDORDER                            
344900     MOVE SPACE              TO   AVSO-IDDC                               
345000     MOVE ZERO               TO   AVSO-TIRFS                              
345100     MOVE ZERO               TO   AVSO-TIAAMMDD                           
345200     MOVE ZERO               TO   AVSO-TIHHMM                             
345300     MOVE W-IDTRANS          TO   AVSO-IDTRANS                            
345400     .                                                                    
345500     EJECT                                                                
345600 S11-BACKA-KAMPANJ SECTION.                                               
345700     MOVE 'STA S11-KAMPANJ      '         TO   WS-PGM-POSITION            
345800                                                                          
345900     PERFORM IMS-GHU-WDM211                                               
346000     IF SEGMENT-FINNS                                                     
346100       COMPUTE KART-KVBEART-KUND = KART-KVBEART-KUND                      
346200                                 - ANNULLERAT-ANTAL                       
346300       IF KART-KVBEART-KUND < ZERO                                        
346400         MOVE 'WDM211 - ANTAL SALDO NEGATIV - ABEND' TO FELTEXT           
346500         CALL ABEND USING RKOD-ABEND                                      
346600       ELSE                                                               
346700         PERFORM IMS-REPL-WDM211                                          
346800       END-IF                                                             
346900     ELSE                                                                 
347000       MOVE 'WDM211 SAKNAS - ABEND' TO FELTEXT                            
347100       CALL ABEND USING RKOD-ABEND                                        
347200     END-IF                                                               
347300                                                                          
347400     PERFORM S20-FINN-INTERVALL                                           
347500     PERFORM IMS-GHU-WDM221                                               
347600     IF SEGMENT-FINNS                                                     
347700       COMPUTE KMRK-KVBEART-KUND = KMRK-KVBEART-KUND                      
347800                                 - ANNULLERAT-ANTAL                       
347900       IF KMRK-KVBEART-KUND < ZERO                                        
348000         MOVE 'WDM221 - ANTALSTABELL NEGATIV - ABEND' TO FELTEXT          
348100         CALL ABEND USING RKOD-ABEND                                      
348200       ELSE                                                               
348300         PERFORM IMS-REPL-WDM221                                          
348400       END-IF                                                             
348500     END-IF                                                               
348600     .                                                                    
348700     EJECT                                                                
348800 S12-RAEKNA-RADER-DDGS SECTION.                                           
348900                                                                          
349000     MOVE 'STA S12-RADER-DDGS   '         TO   WS-PGM-POSITION            
349100     MOVE ZERO    TO TOTAL-KVRADER                                        
349200                     KVRADER-UP-RAKNARE                                   
349300                     MOJLIG-ANTAL                                         
349400     MOVE NEJ     TO REQUEST-SW                                           
349500                     SUPPLIER-REJECTED-SW                                 
349600                                                                          
349700     MOVE WS-IDDC      TO W-ODEL-IDDC-MIN                                 
349800                          W-ODEL-IDDC-MAX                                 
349900     MOVE OHUV-IDORDER TO W-ODEL-IDORDER-MIN                              
350000                          W-ODEL-IDORDER-MAX                              
350100                                                                          
350200     PERFORM IMS-GU-ORQA-WDQ301                                           
350300     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
350400       COMPUTE TOTAL-KVRADER = TOTAL-KVRADER + ODEL-KVRADER               
350500       IF ODEL-KDODELSTA = 'P'                                            
350600         ADD ODEL-KVRADER TO KVRADER-UP-RAKNARE                           
350700       ELSE                                                               
350800         MOVE ODEL-IDDISTR            TO W-401-IDDISTR                    
350900         MOVE ODEL-IDKUNDNR           TO W-401-IDKUNDNR                   
351000         MOVE ODEL-IDKUNDRF(3:5)      TO W-401-IDORDNR                    
351100         MOVE ODEL-IDPRODNR           TO W-401-IDPRODNR                   
351200         MOVE ODEL-IDPLKLST           TO W-401-IDPLKLST                   
351300                                                                          
351400         PERFORM IMS-GU-WDE401                                            
351500         IF SEGMENT-FINNS                                                 
351600           IF KORD-KVORDRAD-LEVPL > ZERO                                  
351700             PERFORM IMS-GNP-WDE411                                       
351800             PERFORM UNTIL SEGMENT-SAKNAS                                 
351900               IF MFS-UPD-V                                               
352000                 IF ((E4-ORAD-KDANNULL = '0' OR '1') AND                  
352100                    E4-ORAD-KVANNANT = ZERO AND                           
352200                    E4-ORAD-KDRADSTA < +4)                                
352300                    OR E4-ORAD-KDANNULL = '2'                             
352400                   CONTINUE                                               
352500                 ELSE                                                     
352600                   COMPUTE KVRADER-UP-RAKNARE =                           
352700                           KVRADER-UP-RAKNARE + 1                         
352800                 END-IF                                                   
352900               ELSE                                                       
353000                 IF ((E4-ORAD-KDANNULL = '0' OR '1') AND                  
353100                    E4-ORAD-KVANNANT = ZERO AND                           
353200                    E4-ORAD-KDRADSTA < +4)                                
353300                    OR E4-ORAD-KDANNULL = '2'                             
353400                   CONTINUE                                               
353500                 ELSE                                                     
353600                   COMPUTE KVRADER-UP-RAKNARE =                           
353700                           KVRADER-UP-RAKNARE + 1                         
353800                 END-IF                                                   
353900               END-IF                                                     
354000               IF E4-ORAD-KDANNULL = '1'                                  
354100                  MOVE JA        TO REQUEST-SW                            
354200               ELSE                                                       
354300                  IF E4-ORAD-KDANNULL = '2'                               
354400                     MOVE JA     TO SUPPLIER-REJECTED-SW                  
354500                  END-IF                                                  
354600               END-IF                                                     
354700               PERFORM IMS-GNP-WDE411                                     
354800             END-PERFORM                                                  
354900           END-IF                                                         
355000         END-IF                                                           
355100       END-IF                                                             
355200       PERFORM IMS-GN-ORQA-WDQ301                                         
355300     END-PERFORM                                                          
355400                                                                          
355500     COMPUTE MOJLIG-ANTAL = TOTAL-KVRADER - KVRADER-UP-RAKNARE            
355600                                                                          
355700     IF MOJLIG-ANTAL = ZERO                                               
355800       IF WS-IDDC NOT = W-IDDC-B6                                         
355900          MOVE WS-IDDC TO W-IDDC-B6                                       
356000          PERFORM IMS-GU-WDB601                                           
356100       END-IF                                                             
356200       IF DCS-DDC                                                         
356300         MOVE ERR-ANNULL-NOT-POSS TO MED-IDMFSFEL                         
356400       ELSE                                                               
356500         MOVE ERR-LINES-WRITTEN   TO MED-IDMFSFEL                         
356600       END-IF                                                             
356700       CALL WMEDKONV USING MED-WMEDAREA                                   
356800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
356900       PERFORM MFS-RENSA-FAELT-UT                                         
357000       MOVE MFS-RENSA-FAELT  TO MOD-KVRADER                               
357100                                MOD-KVORDRAD                              
357200       MOVE NEJ TO ALLT-SW                                                
357300     ELSE                                                                 
357400       MOVE MOJLIG-ANTAL   TO MOD-KVRADER                                 
357500       MOVE TOTAL-KVRADER  TO MOD-KVORDRAD                                
357600       PERFORM S16-CHECK-IF-SUPP-REJ                                      
357700     END-IF                                                               
357800     .                                                                    
357900     EJECT                                                                
358000 S14-CHECK-IF-SENT     SECTION.                                           
358100                                                                          
358200     MOVE 'STA S14-CHECK-IF-SENT'         TO   WS-PGM-POSITION            
358300                                                                          
358400     IF REQUEST-SENT                                                      
358500        IF MED-IDSKYLT = 'S  '                                            
358600          MOVE 'ANNULERING ÄR SÄND, SE RADER PÅ 4282'                     
358700                                           TO MOD-TEMFSFEL                
358800        ELSE                                                              
358900          MOVE 'CANCELLATION IS SENT, SEE LINES ON 4282'                  
359000                                           TO MOD-TEMFSFEL                
359100        END-IF                                                            
359200     END-IF                                                               
359300                                                                          
359400     .                                                                    
359500     EJECT                                                                
359600 S16-CHECK-IF-SUPP-REJ SECTION.                                           
359700                                                                          
359800     MOVE 'STA S16-CHECK-IF-SUPP'         TO   WS-PGM-POSITION            
359900                                                                          
360000     IF SUPPLIER-REJECTED                                                 
360100        IF MED-IDSKYLT = 'S  '                                            
360200          MOVE 'AVBRYT AVVISA BEGÄRAN,SE 4282'                            
360300                                           TO MOD-TEMFSFEL                
360400        ELSE                                                              
360500          MOVE 'CANCEL REQUEST REJECTED.SEE 4282'                         
360600                                           TO MOD-TEMFSFEL                
360700        END-IF                                                            
360800     END-IF                                                               
360900     .                                                                    
361000     EJECT                                                                
361100 S15-LAS-BENAEMNING SECTION.                                              
361200                                                                          
361300     MOVE 'STA S15-LAS   '                TO   WS-PGM-POSITION            
361400     PERFORM IMS-GU-BENA-BENA11                                           
361500                                                                          
361600     IF SEGMENT-FINNS                                                     
361700                                                                          
361800       MOVE TEXT-BEART TO MOD-BEART-RAD(INDX)                             
361900                                                                          
362000     END-IF                                                               
362100     .                                                                    
362200     EJECT                                                                
362300 S17-FIXA-REFILL-TRANSFER SECTION.                                        
362400                                                                          
362500******************************************************************        
362600*                                                                         
362700*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
362800*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
362900*                                                                         
363000******************************************************************        
363100                                                                          
363200     MOVE ORAD-IDDISTR         TO W-TP4TRAN-IDDISTR                       
363300                                                                          
363400     PERFORM DB2-SELECT-TP4TRAN                                           
363500                                                                          
363600     MOVE 'STA S17-REFILL       '         TO   WS-PGM-POSITION            
363700     MOVE ORAD-IDDISTR TO TEST-IDDISTR                                    
363800     IF DIST35-REFILL           OR                                        
363900        DIST35-REFILL-INOM-NDC  OR                                        
364000        DIST35-NONVCC-NONVCC-REFILL      OR                               
364100        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
364200        DIST35-NA-TRANSFER      OR                                        
364300        DIST35-NA-NDC-RETURNS   OR                                        
364400        DIST35-PACIFIC-TRANSFER OR                                        
364500        DIST35-REFILL-INOM-JP   OR                                        
364600        DIST35-CN-TRANSFER      OR                                        
364700        DIST35-NONVCC-VCC-REFILL OR                                       
364800        DIST35-NONVCC-VCC-TRANSFER OR                                     
364900        RADER-FINNS                                                       
365000                                                                          
365100       IF RADER-FINNS                                                     
365200         MOVE TP4TRAN-IDDC-REC                                            
365300                             TO W-IDDC                                    
365400       ELSE                                                               
365500                                                                          
365600         SEARCH ALL DIST57-REFILL-DC                                      
365700           AT END                                                         
365800             MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                         
365900                               TO FELTEXT                                 
366000             CALL FELLOG                                                  
366100           WHEN DIST57-SOK-IDDISTR(DIST57-IX) = ORAD-IDDISTR              
366200             MOVE DIST57-REFILL-TO-DC(DIST57-IX)                          
366300                               TO W-IDDC                                  
366400         END-SEARCH                                                       
366500       END-IF                                                             
366600                                                                          
366700       PERFORM IMS-GHU-ARTS-WDK711                                        
366800       COMPUTE SLAG-KVBEART = SLAG-KVBEART - ANNULLERAT-ANTAL             
366900       PERFORM IMS-REPL-WDK7                                              
367000     ELSE                                                                 
367100        IF DIST35-NONVCC-CDC-REFILL                                       
367200           SEARCH ALL DIST57-REFILL-DC                                    
367300            AT END                                                        
367400              MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                        
367500                               TO FELTEXT                                 
367600              CALL FELLOG                                                 
367700            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = ORAD-IDDISTR             
367800              MOVE DIST57-REFILL-TO-DC(DIST57-IX)                         
367900                               TO W-IDDC                                  
368000           END-SEARCH                                                     
368100                                                                          
368200           PERFORM IMS-GHU-WDK611                                         
368300           COMPUTE CLAG-KVBEART = CLAG-KVBEART - ANNULLERAT-ANTAL         
368400           PERFORM IMS-REPL-WDK611                                        
368500        END-IF                                                            
368600     END-IF                                                               
368700     .                                                                    
368800     EJECT                                                                
368900 S20-FINN-INTERVALL SECTION.                                              
369000                                                                          
369100     PERFORM IMS-GU-WDM211                                                
369200     IF SEGMENT-FINNS                                                     
369300       PERFORM IMS-GNP-WDM221                                             
369400       PERFORM UNTIL SEGMENT-SAKNAS                                       
369500         IF  ORAD-IDDISTR > KMRK-IDDISTR-TOM                              
369600         OR  ORAD-IDDISTR < KMRK-IDDISTR-FOM                              
369700           CONTINUE                                                       
369800         ELSE                                                             
369900           IF  ORAD-IDDISTR  = KMRK-IDDISTR-TOM                           
370000           AND ORAD-IDKUNDNR > KMRK-IDKUNDNR-TOM                          
370100             CONTINUE                                                     
370200           ELSE                                                           
370300             IF  ORAD-IDDISTR  = KMRK-IDDISTR-FOM                         
370400             AND ORAD-IDKUNDNR < KMRK-IDKUNDNR-FOM                        
370500               CONTINUE                                                   
370600             ELSE                                                         
370700               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
370800               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
370900               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
371000               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
371100             END-IF                                                       
371200           END-IF                                                         
371300         END-IF                                                           
371400         PERFORM IMS-GNP-WDM221                                           
371500       END-PERFORM                                                        
371600     END-IF                                                               
371700     .                                                                    
371800     EJECT                                                                
371900 S21-DELETE-PRICE-Q-ORDER SECTION.                                        
372000                                                                          
372100     IF DIST79-DEALER-PRICE                                               
372200       INITIALIZE PRQU-W335PRQU                                           
372300       MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                            
372400       MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                           
372500       MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                           
372600       MOVE 3                  TO PRQU-KDCALL                             
372700       CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                   
372800                                          PRQU-WDC7-PCB                   
372900                                          PRQU-SJKO-WDK6-PCB              
373000                                                                          
373100     END-IF                                                               
373200     .                                                                    
373300     EJECT                                                                
373400 S22-CHANGE-PRICE-Q-LINE SECTION.                                         
373500                                                                          
373600     IF DIST79-DEALER-PRICE                                               
373700       IF ORAD-IDPRQUES > ZERO                                            
373800         INITIALIZE PRQU-W335PRQU                                         
373900         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
374000         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
374100         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
374200         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
374300         MOVE WS-KVBEART-Q(INDX) TO PRQU-KVBEART-Q                        
374400         MOVE 5                  TO PRQU-KDCALL                           
374500         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
374600                                            PRQU-WDC7-PCB                 
374700                                            PRQU-SJKO-WDK6-PCB            
374800       END-IF                                                             
374900     END-IF                                                               
375000     .                                                                    
375100     EJECT                                                                
375200                                                                          
375300 S23-CHECK-ORDER-PRINTED SECTION.                                         
375400                                                                          
375500     MOVE NEJ                     TO ORDER-SW                             
375600                                                                          
375700     MOVE MID-IDPRODNR-SPAR(INDX) TO W-IDPRODNR                           
375800     MOVE MID-IDPURAD-SPAR(INDX)  TO W-IDPURAD                            
375900                                                                          
376000     PERFORM IMS-GU-WDF601                                                
376100     IF SEGMENT-FINNS                                                     
376200        PERFORM IMS-GNP-WDF611                                            
376300        IF SEGMENT-FINNS                                                  
376400           MOVE JA                TO ORDER-SW                             
376500        END-IF                                                            
376600     END-IF                                                               
376700     .                                                                    
376800     EJECT                                                                
376900                                                                          
377000 MFS-RENSA-FAELT-UT SECTION.                                              
377100                                                                          
377200*    --- ALLA UTDATA-FÄLT                                                 
377300     MOVE MFS-RENSA-FAELT TO MOD-IDORDER-ENTER                            
377400                             MOD-IDORDER-NEXT                             
377500                             MOD-IDDC-ENTER                               
377600                             MOD-IDDC-NEXT                                
377700                             MOD-ADLAGOMR-ENTER                           
377800                             MOD-ADLAGOMR-NEXT                            
377900                             MOD-ADGANG-ENTER                             
378000                             MOD-ADGANG-NEXT                              
378100                             MOD-ADPLATS-ENTER                            
378200                             MOD-ADPLATS-NEXT                             
378300                             MOD-IDARTNR-ENTER                            
378400                             MOD-IDARTNR-NEXT                             
378500                             MOD-IDLOPNR-ENTER                            
378600                             MOD-IDLOPNR-NEXT                             
378700                             MOD-IDPRODNR-ENTER                           
378800                             MOD-IDPRODNR-NEXT                            
378900                             MOD-IDPLKLST-ENTER                           
379000                             MOD-IDPLKLST-NEXT                            
379100                             MOD-IDPURAD-ENTER                            
379200                             MOD-IDPURAD-NEXT                             
379300     MOVE +1 TO INDX                                                      
379400     PERFORM UNTIL INDX > MAX-INDX                                        
379500       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-RAD(INDX)                      
379600                               MOD-KVBEART-Q-RAD(INDX)                    
379700                               MOD-BEART-RAD(INDX)                        
379800                               MOD-IDDC-RAD(INDX)                         
379900                               MOD-IDARTNR-SPAR(INDX)                     
380000                               MOD-IDLOPNR-SPAR(INDX)                     
380100                               MOD-ADLAGOMR-SPAR(INDX)                    
380200                               MOD-ADGANG-SPAR(INDX)                      
380300                               MOD-ADPLATS-SPAR(INDX)                     
380400                               MOD-IDPRODNR-SPAR(INDX)                    
380500                               MOD-IDPLKLST-SPAR(INDX)                    
380600                               MOD-IDPURAD-SPAR(INDX)                     
380700       ADD +1 TO INDX                                                     
380800     END-PERFORM                                                          
380900     .                                                                    
381000     EJECT                                                                
381100 MFS-RENSA-FAELT-IN SECTION.                                              
381200                                                                          
381300*    --- ALLA INDATA-FÄLT                                                 
381400     MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-UPDATE                            
381500     MOVE +1 TO INDX                                                      
381600     PERFORM UNTIL INDX > MAX-INDX                                        
381700       MOVE MFS-RENSA-FAELT TO MOD-CMD-UPDATE(INDX)                       
381800                               MOD-KVBEART-Q-UPDATE(INDX)                 
381900       ADD +1 TO INDX                                                     
382000     END-PERFORM                                                          
382100     .                                                                    
382200     EJECT                                                                
382300 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
382400                                                                          
382500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDORDER-ENTER                          
382600                               MOD-IDORDER-NEXT                           
382700                               MOD-IDDC-ENTER                             
382800                               MOD-IDDC-NEXT                              
382900                               MOD-ADLAGOMR-ENTER                         
383000                               MOD-ADLAGOMR-NEXT                          
383100                               MOD-ADGANG-ENTER                           
383200                               MOD-ADGANG-NEXT                            
383300                               MOD-ADPLATS-ENTER                          
383400                               MOD-ADPLATS-NEXT                           
383500                               MOD-IDARTNR-ENTER                          
383600                               MOD-IDARTNR-NEXT                           
383700                               MOD-IDLOPNR-ENTER                          
383800                               MOD-IDLOPNR-NEXT                           
383900                               MOD-IDPRODNR-ENTER                         
384000                               MOD-IDPRODNR-NEXT                          
384100                               MOD-IDPLKLST-ENTER                         
384200                               MOD-IDPLKLST-NEXT                          
384300                               MOD-IDPURAD-ENTER                          
384400                               MOD-IDPURAD-NEXT                           
384500                               MOD-KVORDRAD                               
384600                               MOD-KVRADER                                
384700     MOVE +1 TO INDX                                                      
384800     PERFORM UNTIL INDX > MAX-INDX                                        
384900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-RAD(INDX)                    
385000                                 MOD-KVBEART-Q-RAD(INDX)                  
385100                                 MOD-BEART-RAD(INDX)                      
385200                                 MOD-OREF-RAD(INDX)                       
385300                                 MOD-IDDC-RAD(INDX)                       
385400                                 MOD-IDARTNR-SPAR(INDX)                   
385500                                 MOD-IDLOPNR-SPAR(INDX)                   
385600                                 MOD-ADLAGOMR-SPAR(INDX)                  
385700                                 MOD-ADGANG-SPAR(INDX)                    
385800                                 MOD-ADPLATS-SPAR(INDX)                   
385900                                 MOD-IDPRODNR-SPAR(INDX)                  
386000                                 MOD-IDPLKLST-SPAR(INDX)                  
386100                                 MOD-IDPURAD-SPAR(INDX)                   
386200       ADD +1 TO INDX                                                     
386300     END-PERFORM                                                          
386400     .                                                                    
386500     EJECT                                                                
386600 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
386700                                                                          
386800*    --- ALLA INDATA-FÄLT                                                 
386900     MOVE MFS-ROER-EJ-FAELT TO MOD-FLAGGA-UPDATE                          
387000     MOVE +1 TO INDX                                                      
387100     PERFORM UNTIL INDX > MAX-INDX                                        
387200       MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-UPDATE(INDX)                     
387300                                 MOD-KVBEART-Q-UPDATE(INDX)               
387400       ADD +1 TO INDX                                                     
387500     END-PERFORM                                                          
387600     .                                                                    
387700     EJECT                                                                
387800 MFS-FORM-ATTR SECTION.                                                   
387900                                                                          
388000*    --- ALLA INDATA-FÄLT                                                 
388100     MOVE MFS-FORMATETS-ATTR TO MOD-FLAGGA-UPDATE-ATTR                    
388200     MOVE +1 TO INDX                                                      
388300     PERFORM UNTIL INDX > MAX-INDX                                        
388400       MOVE MFS-FORMATETS-ATTR TO MOD-CMD-UPDATE-ATTR(INDX)               
388500                                  MOD-KVBEART-Q-UPDATE-ATTR(INDX)         
388600      ADD +1 TO INDX                                                      
388700     END-PERFORM                                                          
388800     .                                                                    
388900     EJECT                                                                
389000 MFS-LAS-IN-IGEN SECTION.                                                 
389100                                                                          
389200*    --- ALLA INDATA-FÄLT                                                 
389300     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAGGA-UPDATE-ATTR                 
389400     MOVE +1 TO INDX                                                      
389500     PERFORM UNTIL INDX > MAX-INDX                                        
389600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-UPDATE-ATTR(INDX)            
389700                                     MOD-KVBEART-Q-UPDATE-ATTR            
389800                                                        (INDX)            
389900       ADD +1 TO INDX                                                     
390000     END-PERFORM                                                          
390100     .                                                                    
390200     EJECT                                                                
390300* --- IMS SEKTIONER ---                                                   
390400     SKIP3                                                                
390500 IMS-GET-MSG SECTION.                                                     
390600                                                                          
390700     MOVE '  QC' TO GODK-STATUSKODER                                      
390800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
390900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
391000     PERFORM IMS-STATUSKONTROLL                                           
391100     .                                                                    
391200                                                                          
391300 IMS-INSERT-MSG SECTION.                                                  
391400                                                                          
391500     IF ENGLISH-TEXT                                                      
391600       MOVE 'N' TO MFS-KDHUVOMR                                           
391700     END-IF                                                               
391800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
391900     MOVE SPACE TO GODK-STATUSKODER                                       
392000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
392100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
392200     PERFORM IMS-STATUSKONTROLL                                           
392300     .                                                                    
392400     EJECT                                                                
392500 IMS-INSERT-ALT1MSG SECTION.                                              
392600                                                                          
392700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
392800     MOVE SPACE TO GODK-STATUSKODER                                       
392900     CALL CBLTDLI USING ISRT ALT1-PCB W-PROG-TO-PROG-SW                   
393000     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
393100     PERFORM IMS-STATUSKONTROLL                                           
393200     .                                                                    
393300     EJECT                                                                
393400 IMS-PURG-ALT-MSG-4680 SECTION.                                           
393500     MOVE LOW-VALUE TO 4680-Z1 4680-Z2                                    
393600     MOVE SPACE TO GODK-STATUSKODER                                       
393700     CALL CBLTDLI USING PURG 4680-PCB W-PROG-TO-PROG-SW-3                 
393800     MOVE 4680-STATUS-CODE TO STATUS-WS                                   
393900     PERFORM IMS-STATUSKONTROLL                                           
394000     .                                                                    
394100     SKIP2                                                                
394200 IMS-PURG-ALT-MSG-2109 SECTION.                                           
394300     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
394400     MOVE SPACE TO GODK-STATUSKODER                                       
394500     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-2                 
394600     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
394700     PERFORM IMS-STATUSKONTROLL                                           
394800     .                                                                    
394900     EJECT                                                                
395000                                                                          
395100 IMS-PURG-ALT-MSG-4360 SECTION.                                           
395200     MOVE LOW-VALUE TO 4360-Z1 4360-Z2                                    
395300     MOVE SPACE TO GODK-STATUSKODER                                       
395400     CALL CBLTDLI USING PURG 4360-PCB W-PROG-TO-PROG-SW-4                 
395500     MOVE 4360-STATUS-CODE TO STATUS-WS                                   
395600     PERFORM IMS-STATUSKONTROLL                                           
395700     .                                                                    
395800     SKIP2                                                                
395900 IMS-GU-WDK611 SECTION.                                                   
396000                                                                          
396100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
396200          DELIMITED BY SIZE INTO SSA1                                     
396300     MOVE 'WLARTC11 ' TO SSA2                                             
396400     MOVE '  '   TO GODK-STATUSKODER                                      
396500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K611 SSA1 SSA2            
396600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
396700     PERFORM IMS-STATUSKONTROLL                                           
396800     .                                                                    
396900     EJECT                                                                
397000 IMS-GHU-WDK611 SECTION.                                                  
397100                                                                          
397200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
397300          DELIMITED BY SIZE INTO SSA1                                     
397400     MOVE 'WLARTC11 ' TO SSA2                                             
397500     MOVE '  '   TO GODK-STATUSKODER                                      
397600     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-K611 SSA1 SSA2           
397700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
397800     PERFORM IMS-STATUSKONTROLL                                           
397900     .                                                                    
398000     EJECT                                                                
398100                                                                          
398200 IMS-REPL-WDK611 SECTION.                                                 
398300                                                                          
398400     MOVE '  ' TO GODK-STATUSKODER                                        
398500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-K611                    
398600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
398700     PERFORM IMS-STATUSKONTROLL                                           
398800     .                                                                    
398900     EJECT                                                                
399000 IMS-GHU-ARTM-ARTM01 SECTION.                                             
399100                                                                          
399200     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
399300          DELIMITED BY SIZE INTO SSA1                                     
399400     MOVE '  GE' TO GODK-STATUSKODER                                      
399500     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ARTM SSA1                
399600     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
399700     PERFORM IMS-STATUSKONTROLL                                           
399800     .                                                                    
399900                                                                          
400000 IMS-REPL-ARTM SECTION.                                                   
400100                                                                          
400200     MOVE '  ' TO GODK-STATUSKODER                                        
400300     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ARTM                    
400400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
400500     PERFORM IMS-STATUSKONTROLL                                           
400600     .                                                                    
400700     EJECT                                                                
400800 IMS-GHU-ARTS-WDK711 SECTION.                                             
400900                                                                          
401000     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
401100          DELIMITED BY SIZE INTO SSA1                                     
401200     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
401300          DELIMITED BY SIZE INTO SSA2                                     
401400     MOVE '    ' TO GODK-STATUSKODER                                      
401500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-K711 SSA1 SSA2           
401600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
401700     PERFORM IMS-STATUSKONTROLL                                           
401800     .                                                                    
401900                                                                          
402000 IMS-REPL-WDK7 SECTION.                                                   
402100                                                                          
402200     MOVE '  ' TO GODK-STATUSKODER                                        
402300     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-K711                    
402400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
402500     PERFORM IMS-STATUSKONTROLL                                           
402600     .                                                                    
402700     EJECT                                                                
402800 IMS-GU-BENA-BENA11 SECTION.                                              
402900                                                                          
403000     STRING 'WLBENA01(WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
403100          DELIMITED BY SIZE INTO SSA1                                     
403200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
403300          DELIMITED BY SIZE INTO SSA2                                     
403400     MOVE '  GE' TO GODK-STATUSKODER                                      
403500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA SSA1 SSA2            
403600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
403700     PERFORM IMS-STATUSKONTROLL                                           
403800     .                                                                    
403900     EJECT                                                                
404000 IMS-GHU-ORDP-WDA501 SECTION.                                             
404100                                                                          
404200     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
404300          DELIMITED BY SIZE INTO SSA1                                     
404400     MOVE '  GE' TO GODK-STATUSKODER                                      
404500     CALL CBLTDLI USING GHU WDA5-PCB DLI-IO-AREA-ORDP SSA1                
404600     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
404700     PERFORM IMS-STATUSKONTROLL                                           
404800     .                                                                    
404900                                                                          
405000 IMS-REPL-WDA5 SECTION.                                                   
405100                                                                          
405200     MOVE '  ' TO GODK-STATUSKODER                                        
405300     CALL CBLTDLI USING REPL WDA5-PCB DLI-IO-AREA-ORDP                    
405400     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
405500     PERFORM IMS-STATUSKONTROLL                                           
405600     .                                                                    
405700                                                                          
405800 IMS-ISRT-WDA5 SECTION.                                                   
405900                                                                          
406000     MOVE 'WLORDP01 ' TO SSA1                                             
406100     MOVE '  II' TO GODK-STATUSKODER                                      
406200     CALL CBLTDLI USING ISRT WDA5-PCB DLI-IO-AREA-ORDP SSA1               
406300     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
406400     PERFORM IMS-STATUSKONTROLL                                           
406500     .                                                                    
406600     EJECT                                                                
406700 IMS-GU-ORQA-WDQ301 SECTION.                                              
406800                                                                          
406900     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
407000                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
407100          DELIMITED BY SIZE INTO SSA1                                     
407200     MOVE '  GE' TO GODK-STATUSKODER                                      
407300     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-AREA-ODEL SSA1                 
407400     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
407500     PERFORM IMS-STATUSKONTROLL                                           
407600     .                                                                    
407700                                                                          
407800 IMS-GN-ORQA-WDQ301 SECTION.                                              
407900                                                                          
408000     STRING 'WLORQA01(WDQ301KY >' W-WDQ301KY-MIN-X                        
408100                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
408200          DELIMITED BY SIZE INTO SSA1                                     
408300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
408400     CALL CBLTDLI USING GN WDQ3-PCB DLI-IO-AREA-ODEL SSA1                 
408500     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
408600     PERFORM IMS-STATUSKONTROLL                                           
408700     .                                                                    
408800     EJECT                                                                
408900 IMS-GU-ORQA-WDQ301-STATUS SECTION.                                       
409000                                                                          
409100     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
409200                    '&WDQ301KY<=' W-WDQ301KY-MAX-X                        
409300                    '&KDODELST =' W-KDODELST-X ')'                        
409400          DELIMITED BY SIZE INTO SSA1                                     
409500     MOVE '  GE' TO GODK-STATUSKODER                                      
409600     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-AREA-ODEL SSA1                 
409700     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
409800     PERFORM IMS-STATUSKONTROLL                                           
409900     .                                                                    
410000                                                                          
410100 IMS-GN-ORQA-WDQ301-STATUS SECTION.                                       
410200                                                                          
410300     STRING 'WLORQA01(WDQ301KY >' W-WDQ301KY-MIN-X                        
410400                    '&WDQ301KY<=' W-WDQ301KY-MAX-X                        
410500                    '&KDODELST =' W-KDODELST-X ')'                        
410600          DELIMITED BY SIZE INTO SSA1                                     
410700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
410800     CALL CBLTDLI USING GN WDQ3-PCB DLI-IO-AREA-ODEL SSA1                 
410900     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
411000     PERFORM IMS-STATUSKONTROLL                                           
411100     .                                                                    
411200     EJECT                                                                
411300 IMS-GU-ORQF-WDQ401 SECTION.                                              
411400                                                                          
411500     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-MIN-X                    
411600                    '&WDQ401KY<=' W-WDQ401KY-MAX-MAX-X ')'                
411700          DELIMITED BY SIZE INTO SSA1                                     
411800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
411900     CALL CBLTDLI USING GU WDQ4-PCB DLI-IO-AREA-ORAD SSA1                 
412000     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
412100     PERFORM IMS-STATUSKONTROLL                                           
412200     .                                                                    
412300                                                                          
412400 IMS-GU-ORQF-WDQ401-M-IDARTNR SECTION.                                    
412500                                                                          
412600     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-X                        
412700                    '&WDQ401KY<=' W-WDQ401KY-MAX-X                        
412800                    '&IDARTNR  =' W-ORAD-IDARTNR-MIN-X                    
412900                    '&IDLOPNR >=' W-ORAD-IDLOPNR-MIN-X ')'                
413000          DELIMITED BY SIZE INTO SSA1                                     
413100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
413200     CALL CBLTDLI USING GU WDQ4-PCB DLI-IO-AREA-ORAD SSA1                 
413300     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
413400     PERFORM IMS-STATUSKONTROLL                                           
413500     .                                                                    
413600                                                                          
413700 IMS-GN-ORQF-WDQ401 SECTION.                                              
413800                                                                          
413900     STRING 'WLORQF01(WDQ401KY >' W-WDQ401KY-MIN-MIN-X                    
414000                    '&WDQ401KY<=' W-WDQ401KY-MAX-MAX-X ')'                
414100          DELIMITED BY SIZE INTO SSA1                                     
414200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
414300     CALL CBLTDLI USING GN WDQ4-PCB DLI-IO-AREA-ORAD SSA1                 
414400     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
414500     PERFORM IMS-STATUSKONTROLL                                           
414600     .                                                                    
414700     EJECT                                                                
414800 IMS-GN-ORQF-WDQ401-M-IDARTNR SECTION.                                    
414900                                                                          
415000     STRING 'WLORQF01(WDQ401KY >' W-WDQ401KY-MIN-X                        
415100                    '&WDQ401KY<=' W-WDQ401KY-MAX-X                        
415200                    '&IDARTNR  =' W-ORAD-IDARTNR-MIN-X                    
415300                    '&IDLOPNR >=' W-ORAD-IDLOPNR-MIN-X ')'                
415400          DELIMITED BY SIZE INTO SSA1                                     
415500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
415600     CALL CBLTDLI USING GN WDQ4-PCB DLI-IO-AREA-ORAD SSA1                 
415700     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
415800     PERFORM IMS-STATUSKONTROLL                                           
415900     .                                                                    
416000                                                                          
416100 IMS-GHU-ORQF-WDQ401 SECTION.                                             
416200                                                                          
416300     STRING 'WLORQF01(WDQ401KY =' W-WDQ401KY-UNIK-X ')'                   
416400          DELIMITED BY SIZE INTO SSA1                                     
416500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
416600     CALL CBLTDLI USING GHU WDQ4-PCB DLI-IO-AREA-ORAD SSA1                
416700     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
416800     PERFORM IMS-STATUSKONTROLL                                           
416900     .                                                                    
417000                                                                          
417100 IMS-REPL-WDQ4 SECTION.                                                   
417200                                                                          
417300     MOVE '  ' TO GODK-STATUSKODER                                        
417400     CALL CBLTDLI USING REPL WDQ4-PCB DLI-IO-AREA-ORAD                    
417500     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
417600     PERFORM IMS-STATUSKONTROLL                                           
417700     .                                                                    
417800     EJECT                                                                
417900 IMS-DLET-WDQ4 SECTION.                                                   
418000                                                                          
418100     MOVE '  ' TO GODK-STATUSKODER                                        
418200     CALL CBLTDLI USING DLET WDQ4-PCB DLI-IO-AREA-ORAD                    
418300     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
418400     PERFORM IMS-STATUSKONTROLL                                           
418500     .                                                                    
418600     EJECT                                                                
418700 IMS-GU-ORQI-WDQ201 SECTION.                                              
418800                                                                          
418900     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
419000          DELIMITED BY SIZE INTO SSA1                                     
419100     MOVE '  GE' TO GODK-STATUSKODER                                      
419200     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-OHUV SSA1                 
419300     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
419400     PERFORM IMS-STATUSKONTROLL                                           
419500     .                                                                    
419600                                                                          
419700 IMS-GHU-ORQI-WDQ201 SECTION.                                             
419800                                                                          
419900     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
420000          DELIMITED BY SIZE INTO SSA1                                     
420100     MOVE '  GE' TO GODK-STATUSKODER                                      
420200     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-AREA-OHUV SSA1                
420300     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
420400     PERFORM IMS-STATUSKONTROLL                                           
420500     .                                                                    
420600     EJECT                                                                
420700 IMS-GHNP-ORQI-WDQ212 SECTION.                                            
420800                                                                          
420900     STRING 'WLORQI12*F(IDDC     =' W-IDDC-X ')'                          
421000          DELIMITED BY SIZE INTO SSA1                                     
421100     MOVE '  GE' TO GODK-STATUSKODER                                      
421200     CALL CBLTDLI USING GHNP WDQ2-PCB DLI-IO-AREA-ARB SSA1                
421300     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
421400     PERFORM IMS-STATUSKONTROLL                                           
421500     .                                                                    
421600                                                                          
421700 IMS-GNP-ORQI-WDQ212 SECTION.                                             
421800                                                                          
421900     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
422000          DELIMITED BY SIZE INTO SSA1                                     
422100     MOVE '  GE' TO GODK-STATUSKODER                                      
422200     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-ARB SSA1                 
422300     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
422400     PERFORM IMS-STATUSKONTROLL                                           
422500     .                                                                    
422600                                                                          
422700 IMS-GNP-WDQ212-OKVAL SECTION.                                            
422800                                                                          
422900     MOVE   'WLORQI12' TO SSA1                                            
423000     MOVE '  GE' TO GODK-STATUSKODER                                      
423100     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-ARB SSA1                 
423200     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
423300     PERFORM IMS-STATUSKONTROLL                                           
423400     .                                                                    
423500                                                                          
423600 IMS-GNP-WDQ221 SECTION.                                                  
423700                                                                          
423800     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
423900          DELIMITED BY SIZE INTO SSA1                                     
424000     MOVE 'WLORQI21 '         TO SSA2                                     
424100     MOVE '  GE' TO GODK-STATUSKODER                                      
424200     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-Q221 SSA1 SSA2                
424300     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
424400     PERFORM IMS-STATUSKONTROLL                                           
424500     .                                                                    
424600                                                                          
424700 IMS-GNP-ORQI-WDQ211-FIRST SECTION.                                       
424800                                                                          
424900     MOVE 'WLORQI11*F' TO SSA1                                            
425000     MOVE '  GE' TO GODK-STATUSKODER                                      
425100     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-DIRL SSA1                
425200     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
425300     PERFORM IMS-STATUSKONTROLL                                           
425400     .                                                                    
425500     EJECT                                                                
425600 IMS-GNP-ORQI-WDQ211 SECTION.                                             
425700                                                                          
425800     MOVE 'WLORQI11 ' TO SSA1                                             
425900     MOVE '  GE' TO GODK-STATUSKODER                                      
426000     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-DIRL SSA1                
426100     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
426200     PERFORM IMS-STATUSKONTROLL                                           
426300     .                                                                    
426400                                                                          
426500 IMS-REPL-WDQ2 SECTION.                                                   
426600                                                                          
426700     MOVE '  ' TO GODK-STATUSKODER                                        
426800     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-AREA-OHUV                    
426900     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
427000     PERFORM IMS-STATUSKONTROLL                                           
427100     .                                                                    
427200     EJECT                                                                
427300 IMS-REPL-WDQ212 SECTION.                                                 
427400                                                                          
427500     MOVE '  ' TO GODK-STATUSKODER                                        
427600     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-AREA-ARB                     
427700     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
427800     PERFORM IMS-STATUSKONTROLL                                           
427900     .                                                                    
428000     EJECT                                                                
428100 IMS-GU-ORQM-ORQM01 SECTION.                                              
428200                                                                          
428300     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
428400                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
428500          DELIMITED BY SIZE INTO SSA1                                     
428600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
428700     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA-OBKR SSA1                 
428800     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
428900     PERFORM IMS-STATUSKONTROLL                                           
429000     .                                                                    
429100                                                                          
429200 IMS-ISRT-ORQM-ORQM01 SECTION.                                            
429300                                                                          
429400     MOVE 'WLORQM01 ' TO SSA1                                             
429500     MOVE '  II' TO GODK-STATUSKODER                                      
429600     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-OBKR SSA1               
429700     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
429800     PERFORM IMS-STATUSKONTROLL                                           
429900     .                                                                    
430000     EJECT                                                                
430100 IMS-ISRT-ZZAC-ZZAC01 SECTION.                                            
430200                                                                          
430300     MOVE 'WLZZAC01 ' TO SSA1                                             
430400     MOVE '  II' TO GODK-STATUSKODER                                      
430500     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA-ZZAC SSA1               
430600     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
430700     PERFORM IMS-STATUSKONTROLL                                           
430800     .                                                                    
430900     EJECT                                                                
431000 IMS-GU-WDM211 SECTION.                                                   
431100                                                                          
431200     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
431300          DELIMITED BY SIZE INTO SSA1                                     
431400     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
431500          DELIMITED BY SIZE INTO SSA2                                     
431600     MOVE '  GE'              TO GODK-STATUSKODER                         
431700     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
431800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
431900     PERFORM IMS-STATUSKONTROLL                                           
432000     .                                                                    
432100                                                                          
432200 IMS-GNP-WDM221 SECTION.                                                  
432300                                                                          
432400     MOVE 'WDM221 '           TO SSA1                                     
432500     MOVE '    GE'            TO GODK-STATUSKODER                         
432600     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
432700     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
432800     PERFORM IMS-STATUSKONTROLL                                           
432900     .                                                                    
433000                                                                          
433100 IMS-GHU-WDM211 SECTION.                                                  
433200                                                                          
433300     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
433400          DELIMITED BY SIZE INTO SSA1                                     
433500     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
433600          DELIMITED BY SIZE INTO SSA2                                     
433700     MOVE '  GE'              TO GODK-STATUSKODER                         
433800     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
433900     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
434000     PERFORM IMS-STATUSKONTROLL                                           
434100     .                                                                    
434200                                                                          
434300 IMS-REPL-WDM211 SECTION.                                                 
434400                                                                          
434500     MOVE '  '             TO GODK-STATUSKODER                            
434600     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
434700     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
434800     PERFORM IMS-STATUSKONTROLL                                           
434900     .                                                                    
435000     EJECT                                                                
435100 IMS-GHU-WDM221 SECTION.                                                  
435200                                                                          
435300     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
435400          DELIMITED BY SIZE INTO SSA1                                     
435500     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
435600          DELIMITED BY SIZE INTO SSA2                                     
435700     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
435800          DELIMITED BY SIZE INTO SSA3                                     
435900     MOVE '  GE' TO GODK-STATUSKODER                                      
436000     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
436100     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
436200     PERFORM IMS-STATUSKONTROLL                                           
436300     .                                                                    
436400                                                                          
436500 IMS-REPL-WDM221 SECTION.                                                 
436600                                                                          
436700     MOVE '  '             TO GODK-STATUSKODER                            
436800     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
436900     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
437000     PERFORM IMS-STATUSKONTROLL                                           
437100     .                                                                    
437200 IMS-GU-WDE401    SECTION.                                                
437300     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
437400            DELIMITED BY SIZE INTO SSA1                                   
437500     MOVE '  GE' TO GODK-STATUSKODER                                      
437600     CALL CBLTDLI USING GU  WDE4-PCB KORD-WDE401 SSA1                     
437700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
437800     PERFORM IMS-STATUSKONTROLL                                           
437900     SKIP2                                                                
438000     .                                                                    
438100 IMS-GNP-WDE411-UNIK    SECTION.                                          
438200     STRING 'WDE411  (IDPURAD  =' W-WDE411-X ')'                          
438300            DELIMITED BY SIZE INTO SSA1                                   
438400     MOVE '  GE' TO GODK-STATUSKODER                                      
438500     CALL CBLTDLI USING GNP WDE4-PCB E4-ORAD-WDE411 SSA1                  
438600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
438700     PERFORM IMS-STATUSKONTROLL                                           
438800     SKIP2                                                                
438900     .                                                                    
439000 IMS-GNP-WDE411-ARTNR   SECTION.                                          
439100     STRING 'WDE411  (IDPURAD >=' W-WDE411-X                              
439200                    '&IDARTNR  =' W-IDARTNR-X ')'                         
439300            DELIMITED BY SIZE INTO SSA1                                   
439400     MOVE '  GE' TO GODK-STATUSKODER                                      
439500     CALL CBLTDLI USING GNP WDE4-PCB E4-ORAD-WDE411 SSA1                  
439600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
439700     PERFORM IMS-STATUSKONTROLL                                           
439800     SKIP2                                                                
439900     .                                                                    
440000 IMS-GNP-WDE411    SECTION.                                               
440100     MOVE   'WDE411   '         TO SSA1                                   
440200     MOVE '  GE' TO GODK-STATUSKODER                                      
440300     CALL CBLTDLI USING GNP WDE4-PCB E4-ORAD-WDE411 SSA1                  
440400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
440500     PERFORM IMS-STATUSKONTROLL                                           
440600     SKIP2                                                                
440700     .                                                                    
440800 IMS-GU-WDE411    SECTION.                                                
440900     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
441000            DELIMITED BY SIZE INTO SSA1                                   
441100     STRING 'WDE411  (IDPURAD  =' W-WDE411-X ')'                          
441200            DELIMITED BY SIZE INTO SSA2                                   
441300     MOVE '  ' TO GODK-STATUSKODER                                        
441400     CALL CBLTDLI USING GU WDE4-PCB E4-ORAD-WDE411 SSA1 SSA2              
441500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
441600     PERFORM IMS-STATUSKONTROLL                                           
441700     SKIP2                                                                
441800     .                                                                    
441900 IMS-GHU-WDE411    SECTION.                                               
442000     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
442100            DELIMITED BY SIZE INTO SSA1                                   
442200     STRING 'WDE411  (IDPURAD  =' W-WDE411-X ')'                          
442300            DELIMITED BY SIZE INTO SSA2                                   
442400     MOVE '  ' TO GODK-STATUSKODER                                        
442500     CALL CBLTDLI USING GHU WDE4-PCB E4-ORAD-WDE411 SSA1 SSA2             
442600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
442700     PERFORM IMS-STATUSKONTROLL                                           
442800     SKIP2                                                                
442900     .                                                                    
443000 IMS-REPL-WDE411        SECTION.                                          
443100     MOVE '    ' TO GODK-STATUSKODER                                      
443200     CALL CBLTDLI USING REPL WDE4-PCB E4-ORAD-WDE411                      
443300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
443400     PERFORM IMS-STATUSKONTROLL                                           
443500     .                                                                    
443600 IMS-GHN-WDA6B SECTION.                                                   
443700     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
443800                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
443900            DELIMITED BY SIZE INTO SSA1                                   
444000     MOVE '  GEGB'               TO GODK-STATUSKODER                      
444100     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA-WDA6 SSA1           
444200     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
444300     PERFORM IMS-STATUSKONTROLL                                           
444400     .                                                                    
444500 IMS-REPL-WDA6B SECTION.                                                  
444600     MOVE 'WDA601  '           TO SSA1                                    
444700     MOVE '    '               TO GODK-STATUSKODER                        
444800     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA-WDA6 SSA1            
444900     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
445000     PERFORM IMS-STATUSKONTROLL                                           
445100     .                                                                    
445200     EJECT                                                                
445300 IMS-GU-WDB601    SECTION.                                                
445400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
445500          DELIMITED BY SIZE INTO SSA1                                     
445600     MOVE '  GE' TO GODK-STATUSKODER                                      
445700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
445800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
445900     PERFORM IMS-STATUSKONTROLL                                           
446000     IF SEGMENT-SAKNAS                                                    
446100         MOVE SPACE TO DCS-KDDC                                           
446200     END-IF                                                               
446300     .                                                                    
446400     EJECT                                                                
446500 IMS-GU-WDF601 SECTION.                                                   
446600     STRING 'WDF601  (IDPRODNR =' W-IDPRODNR-X ')'                        
446700             DELIMITED BY SIZE INTO SSA1                                  
446800     MOVE '  GE'                 TO GODK-STATUSKODER                      
446900     CALL CBLTDLI USING GU WDF6-PCB DLI-IO-AREA-F601 SSA1                 
447000     MOVE WDF6-STATUS-CODE       TO STATUS-WS                             
447100     PERFORM IMS-STATUSKONTROLL                                           
447200     .                                                                    
447300                                                                          
447400 IMS-GNP-WDF611 SECTION.                                                  
447500     STRING 'WDF611  (IDPURAD  =' W-IDPURAD-X ')'                         
447600             DELIMITED BY SIZE INTO SSA1                                  
447700     MOVE '  GE'                 TO GODK-STATUSKODER                      
447800     CALL CBLTDLI USING GNP WDF6-PCB DLI-IO-AREA-F611 SSA1                
447900     MOVE WDF6-STATUS-CODE       TO STATUS-WS                             
448000     PERFORM IMS-STATUSKONTROLL                                           
448100     .                                                                    
448200                                                                          
448300 DB2-SELECT-TP4TRAN     SECTION.                                          
448400     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
448500                                                                          
448600     MOVE 000100 TO GODK-SQLCODEKODER                                     
448700                                                                          
448800     EXEC SQL                                                             
448900           SELECT  DISTINCT                                               
449000                   IDDC_REC                                               
449100                                                                          
449200           INTO   :TP4TRAN-IDDC-REC                                       
449300                                                                          
449400           FROM    TP4TRAN                                                
449500                                                                          
449600           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
449700     END-EXEC                                                             
449800                                                                          
449900     MOVE SQLCODE TO SQLCODE-WS                                           
450000     PERFORM DB2-STATUSKONTROLL                                           
450100     .                                                                    
450200     EJECT                                                                
450300 IMS-STATUSKONTROLL SECTION.                                              
450400                                                                          
450500     SET STATUS-IX TO 1                                                   
450600     SEARCH GODK-STATUS                                                   
450700       AT END CALL FELLOG                                                 
450800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
450900     END-SEARCH                                                           
451000     .                                                                    
451100 DB2-STATUSKONTROLL  SECTION.                                             
451200                                                                          
451300     SET SQLCODE-IX TO 1                                                  
451400     SEARCH GODK-SQLCODE                                                  
451500       AT END                                                             
451600          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
451700          DELIMITED BY SIZE INTO FELTEXT                                  
451800          CALL ABEND USING RKOD-ABEND-DB2                                 
451900       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
452000     END-SEARCH                                                           
452100     .                                                                    
