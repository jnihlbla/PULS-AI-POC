000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W9011500.                                                
000400 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500 DATE-WRITTEN.   01/08/22.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*        PROGRAMMET VISAR LEVERANSINFORMATION FÖR NDC                     
000900*        FÖR EXTERNA ANVÄNDARE                                            
001600*                                                                         
001700*        PROGRAMMET LÄSER              WDP7                               
001800*                                      WDD3                               
001900*                                      WDD9                               
002000*                                      WDL6                               
002100*                                      WDK6                               
002200*                                      WDK7                               
002500*                                      WDB6                               
002600*                                                                         
002700*    INDATA.                                                              
003000*        TRANSAKTION: W9T115                                              
003100*        MID:         W9I11501                                            
003200*                                                                         
003700*    UTDATA.                                                              
004000*        MOD:         W9O11501                                            
004400*                                                                         
004500*      2012-01-04  E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1           
004600*                                                                         
004700*      2021-11-19  STORY 2217565 - NEW NDC ROLLOUTS                       
004800*      2021-12-14  STORY 2235799 - ADD DC11 CHECK ALSO.                   
004900*      2022-01-20  STORY 2574989 - ABEND FIX FOR NON NUMERIC ARTNR        
005000*      2022-04-13  STORY 2583419 - NEW JP NDC-6A(WWDCKONS CHANGES)        
005100*      2023-01-31  STORY 3169645 - NEW NDCS TH AND TW ROLLOUT             
005200*                                                                         
005300                                                                          
005400     SKIP3                                                                
005500 ENVIRONMENT DIVISION.                                                    
005600     EJECT                                                                
005700 DATA DIVISION.                                                           
005800 WORKING-STORAGE SECTION.                                                 
005900*    -COPY WY2000W3                                                       
006000     SKIP3                                                                
006100*    -COPY WY2000W2                                                       
006200     SKIP3                                                                
006300*    -COPY WY2000W1                                                       
006400     SKIP3                                                                
006500 77  IDPGM                       PIC X(08)   VALUE 'W9011500'.            
006600                                                                          
006700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006900                                                                          
007000 77  JA                          PIC X       VALUE 'J'.                   
007100 77  NEJ                         PIC X       VALUE 'N'.                   
007200                                                                          
007300*01  -COPY WWDCKONS                                                       
007400                                                                          
007500 77  KVAK-SALDO                  PIC S9(7)  COMP-3 VALUE ZERO.            
007600 77  NASTA-INLEV                 PIC S9(7)  COMP-3.                       
007700 77  VECKOR-TILL-PUBLICERING     PIC S9(7)  COMP-3.                       
007800 77  SISTA-INLEV-DATUM           PIC 9(6).                                
007900 77  TABELL-IX                   PIC S9(5)  COMP-3.                       
008000 77  MOD-IX                      PIC S9(5)  COMP-3.                       
008100 77  SPARA-IX                    PIC S9(5)  COMP-3.                       
008200 77  IX                          PIC S9(5)  COMP-3.                       
008500 77  MAX-ANTAL-RADER             PIC S9(2)  VALUE ZERO.                   
008600 77  SPARA-LAGSTA-DATUM          PIC S9(7)  COMP-3.                       
008700 77  SPARA-IDLEVNR               PIC X(5).                                
008800 77  SPARA-TIDISPIN              PIC 9(6).                                
008900 77  SPARA-TIAVROP               PIC 9(5).                                
009000 77  SPARA-TIAVROP-DISP          PIC 9(4).                                
009100 77  SPAR-IDTRANS                PIC X(4)   VALUE '9115'.                 
009200 77  SPAR-DISPLAY                PIC X(20)  VALUE 'DISPLAY-TEST'.         
009300 01  WS.                                                                  
009400   03 WS-RESTKVANT-DAG           PIC S9(7)  VALUE ZERO.                   
009500   03 WS-RESTKVANT-BULK          PIC S9(7)  VALUE ZERO.                   
009600   03 WS-RETUR                   PIC S9(7)  COMP-3 VALUE ZERO.            
009700   03 WS-AKT-TIAA                PIC 9(2)   VALUE ZERO.                   
009800   03 WS-AKT-TIMM                PIC 9(2)   VALUE ZERO.                   
009900   03 WS-TAB-AVROP               OCCURS 5.                                
010000      05 WS-TAB-TIAAMMDD         PIC 9(6)   VALUE ZERO.                   
010100      05 WS-TAB-KVAVROP          PIC 9(7)   VALUE ZERO.                   
010200   03 WS-TIAAVV                  PIC 9(4)   VALUE ZERO.                   
010300   03 WS-TIAAVVD                 PIC 9(5).                                
010400   03 FILLER REDEFINES           WS-TIAAVVD.                              
010500      05 WS-TIAA                 PIC 9(2).                                
010600      05 WS-TIVV                 PIC 9(2).                                
010700      05 WS-TID                  PIC 9.                                   
010800   03 WS-IDARTNR-NUM             PIC 9(9)   VALUE ZERO.                   
010900   03 WS-KVAKS                   PIC 9(9)   VALUE ZERO.                   
011800   03 WS-CDC-DATA.                                                        
011900     05 WS-WDK901-OKS-VOR        PIC S9(7) COMP-3 VALUE ZERO.             
012000     05 WS-WDK901-OKS-DAY        PIC S9(7) COMP-3 VALUE ZERO.             
012100     05 WS-WDK901-OKS-BULK       PIC S9(7) COMP-3 VALUE ZERO.             
012200     05 WS-WDK901-OKS-TOT        PIC S9(7) COMP-3 VALUE ZERO.             
012300     05 WS-WDK611-KVROS-DAG      PIC -(6)9.                               
012400     05 WS-WDK611-KVROS-BULK     PIC -(6)9.                               
012500     05 WS-WDK611-KVOKS-VOR      PIC -(6)9.                               
012600                                                                          
012700   03 W-BO-QTY-VOR               PIC S9(9) COMP-3 VALUE ZERO.             
012800   03 W-BO-QTY-C1                PIC S9(9) COMP-3 VALUE ZERO.             
012900   03 W-BO-QTY-C234              PIC S9(9) COMP-3 VALUE ZERO.             
013000   03 W-BO-QTY-DAY               PIC S9(9) COMP-3 VALUE ZERO.             
013100                                                                          
013200                                                                          
013300   03 W-WDA5ASEQ-MIN-X.                                                   
013400       05  W-WDA5A-IDARTNR-MIN     PIC S9(9) VALUE ZERO COMP-3.           
013500       05  W-WDA5A-IDDC-MIN        PIC X(2)  VALUE SPACE.                 
013600       05  FILLER                  PIC X(2)  VALUE LOW-VALUE.             
013700                                                                          
013800   03 W-WDA5ASEQ-MAX-X.                                                   
013900       05  W-WDA5A-IDARTNR-MAX     PIC S9(9) VALUE ZERO COMP-3.           
014000       05  W-WDA5A-IDDC-MAX        PIC X(2)  VALUE SPACE.                 
014100       05  FILLER                  PIC X(2)  VALUE HIGH-VALUE.            
014200                                                                          
014300   03 W-WDA6JSEQ-MIN-X.                                                   
014400       05  SEQJ-IDARTNR-MIN        PIC S9(9) VALUE ZERO COMP-3.           
014500       05  SEQJ-TIREGDAT-MIN       PIC S9(7) VALUE ZERO COMP-3.           
014600       05  SEQJ-TIREFTID-MIN       PIC S9(9) VALUE ZERO COMP-3.           
014700   03 W-WDA6JSEQ-MAX-X.                                                   
014800       05  SEQJ-IDARTNR-MAX        PIC S9(9) VALUE ZERO COMP-3.           
014900       05  SEQJ-TIREGDAT-MAX       PIC S9(7) VALUE ZERO COMP-3.           
015000       05  SEQJ-TIREFTID-MAX       PIC S9(9) VALUE ZERO COMP-3.           
015100                                                                          
016100 01  W-TIDISPIN.                                                          
016200     03  W-TIDISPIN-DATUM        PIC 9(5).                                
016300     03  W-TIDISPIN-GRP REDEFINES W-TIDISPIN-DATUM.                       
016400         05  W-TIDISPIN-AA       PIC 9(2).                                
016500         05  W-TIDISPIN-VV       PIC 9(2).                                
016600         05  W-TIDISPIN-DD       PIC 9(1).                                
016700                                                                          
016800 01  W-AVROP.                                                             
016900     03  W-AVROP-DATUM           PIC 9(5).                                
017000     03  W-AVROP-GRP REDEFINES W-AVROP-DATUM.                             
017100         05  W-AVROP-AA          PIC 9(2).                                
017200         05  W-AVROP-VV          PIC 9(2).                                
017300         05  W-AVROP-DD          PIC 9(1).                                
017400     03  W-AVROP-FRAM-GRP        PIC 9(5).                                
017500     03  W-AVROP-FRAM REDEFINES W-AVROP-FRAM-GRP.                         
017600         05  W-FRAM-AAVV        PIC 9(4).                                 
017700         05  W-FRAM-D           PIC 9(1).                                 
017800                                                                          
017900 01  W-DAGENS-DATUM.                                                      
018000     03  W-DAGENS-AAVVD          PIC 9(5).                                
018100     03  W-DAGENS-GRP REDEFINES W-DAGENS-AAVVD.                           
018200         05  W-DAGENS-AAVV       PIC 9(4).                                
018300         05  W-DAGENS-DD         PIC 9(1).                                
018400                                                                          
018500                                                                          
018600 01  WS-MSGI-SPAR-AREA.                                                   
018700     03  WS-MSGI-PGM             PIC X(6) VALUE SPACE.                    
018800     03  WS-MSGI-RADNR           PIC 9(3) VALUE ZERO.                     
018900     03  FILLER                  PIC X(191) VALUE SPACE.                  
019000                                                                          
019100 01  MIN-IDDC                    PIC X(2).                                
019200 01  MAX-IDDC                    PIC X(2).                                
019300                                                                          
019400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
019500                                                                          
019600                                                                          
019700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
019800     88  NYCKLAR-OK                          VALUE 'J'.                   
019900     88  NYCKLAR-FEL                         VALUE 'N'.                   
020000                                                                          
020100 77  FLAGGA-WDD9                 PIC X       VALUE 'J'.                   
020200                                                                          
020300 77  FLAGGA-WDD925               PIC X       VALUE 'J'.                   
020400     88  WDD925-FINNS                        VALUE 'J'.                   
020500     88  WDD925-FINNS-INTE                   VALUE 'N'.                   
020600                                                                          
020700 77  FLAGGA-CDC                  PIC X       VALUE 'N'.                   
020800 77  FLAGGA-WDK7-DC41            PIC X       VALUE 'N'.                   
020900 77  FLAGGA-WDK7-DC42            PIC X       VALUE 'N'.                   
021000 77  FLAGGA-WDK7-DC43            PIC X       VALUE 'N'.                   
021100 77  FLAGGA-WDK7-DC44            PIC X       VALUE 'N'.                   
021200 77  FLAGGA-WDK7-DC45            PIC X       VALUE 'N'.                   
021300 77  FLAGGA-WDK7-DC46            PIC X       VALUE 'N'.                   
021400 77  FLAGGA-WDK7-DC47            PIC X       VALUE 'N'.                   
021500 77  FLAGGA-WDK7-DC51            PIC X       VALUE 'N'.                   
021600 77  FLAGGA-WDK7-DC52            PIC X       VALUE 'N'.                   
021700 77  FLAGGA-WDK7-DC53            PIC X       VALUE 'N'.                   
021800 77  FLAGGA-WDK7-DC6A            PIC X       VALUE 'N'.                   
021900 77  FLAGGA-WDK7-DC61            PIC X       VALUE 'N'.                   
022000 77  FLAGGA-WDK7-DC62            PIC X       VALUE 'N'.                   
022100 77  FLAGGA-WDK7-DC63            PIC X       VALUE 'N'.                   
022200 77  FLAGGA-WDK7-DC64            PIC X       VALUE 'N'.                   
022300 77  FLAGGA-WDK7-DC65            PIC X       VALUE 'N'.                   
022400 77  FLAGGA-WDK7-DC66            PIC X       VALUE 'N'.                   
022500 77  FLAGGA-WDK7-DC67            PIC X       VALUE 'N'.                   
022600 77  FLAGGA-WDK7-DC71            PIC X       VALUE 'N'.                   
022700 77  FLAGGA-WDK7-DC72            PIC X       VALUE 'N'.                   
022800 77  FLAGGA-WDK7-DC73            PIC X       VALUE 'N'.                   
022900 77  FLAGGA-WDK7-DC74            PIC X       VALUE 'N'.                   
023000 77  FLAGGA-WDK7-DC85            PIC X       VALUE 'N'.                   
023100 77  FLAGGA-WDK7-DC86            PIC X       VALUE 'N'.                   
023200 77  FLAGGA-WDK7-DC87            PIC X       VALUE 'N'.                   
023200 77  FLAGGA-WDK7-DC93            PIC X       VALUE 'N'.                   
023300                                                                          
023800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
023900     88  EGEN-MID                            VALUE '9115'.                
024000     88  GODK-MID                            VALUE '9111' '9112'          
024100                                                   '9113' '9114'          
024200                                                   '9115' '9116'          
024300                                                   '9117' '9118'          
024400                                                   '9119'.                
024500     88  HELP-MID                            VALUE '0551'.                
024600                                                                          
024700                                                                          
024800 01  DATUM.                                                               
024900     05  DAGENS-DATUM            PIC S9(7) COMP-3.                        
025000                                                                          
025100     EJECT                                                                
025200*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
025300 01  TABENTRY-PARM.                                                       
025400     03  STEGLANGD               PIC S9(9) COMP  VALUE 59.                
025500     03  ANTAL                   PIC S9(9) COMP.                          
025600     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
025700                                                                          
025800 01  IX-DISP                     PIC S9(3) COMP-3 VALUE 1.                
025810 01  IX-RAD                      PIC S9(3) COMP-3 VALUE 1.                
025900 01  IX-RAD-TAB                  PIC S9(3) COMP-3 VALUE 1.                
026000                                                                          
026100 01  TAB-MAX                     PIC S9(9) COMP VALUE 200.                
026200     EJECT                                                                
026300*    --- TABELL SOM SORTERAS AV WINTSOR                                   
026400 01  TABELL.                                                              
026500     03  TAB-POST  OCCURS 200.                                            
026600       04  TAB-RAD.                                                       
026700         05  TAB-IDDC            PIC X(2).                                
026800         05  TAB-KVAVIS          PIC X(7).                                
026900         05  TAB-TIBERANK        PIC 9(6).                                
027000         05  TAB-KVROS-DAG       PIC X(7).                                
027100         05  TAB-KVROS-BULK      PIC X(7).                                
027200         05  TAB-KVOKS-DAG       PIC X(7).                                
027300         05  TAB-KVOKS-BULK      PIC X(7).                                
027400         05  TAB-KVAKS           PIC Z(7).                                
027500       04  TAB-SORT.                                                      
027600         05  TAB-IDDC-PRIO-SORT  PIC X(1) VALUE ZERO.                     
027700         05  TAB-IDDC-SORT       PIC X(2).                                
027800         05  TAB-TIBERANK-SORT   PIC X(6).                                
027900                                                                          
028000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
028100 01  GENERELLA-SUBPROGRAM.                                                
028200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
028300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
028400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
028500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
028600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
028700     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
028800     EJECT                                                                
028900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
029000*01 -COPY WMSGINIT                                                        
029100     EJECT                                                                
029200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
029300*01 -COPY WMEDAREA                                                        
029400                                                                          
029500     SKIP3                                                                
029600 01  MESSAGE-CODES.                                                       
029700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
029800     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
029900     03  ERR-ARTIKEL-UTGANGEN    PIC X(3)    VALUE '018'.                 
030000     03  ERR-NO-ORDER            PIC X(3)    VALUE '248'.                 
030100     03  ERR-NO-BACKORDERS       PIC X(3)    VALUE '403'.                 
030200 01  MESSAGE-TEXT.                                                        
030300     03  MORE-LINES              PIC X(30)                                
030400                         VALUE 'MORE LINES EXIST, PRESS PF8'.             
030500     EJECT                                                                
030600*01  -COPY WDATAREA                                                       
030700     EJECT                                                                
030800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
030900*                                                                         
031000 01  FILLER                      PIC X(16)  VALUE 'MID-AREA'.             
031100     SKIP3                                                                
031200*01  MID -COPY W9I11501 -PRE MID-                                         
031300     EJECT                                                                
031400*                                                                         
032200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
032300     SKIP3                                                                
032400*01  -COPY WMSGAREA                                                       
032500     EJECT                                                                
032700     03  MOD REDEFINES MSG-AREA.                                          
032800*      05  -COPY W9O11501 -PRE MOD-                                       
032900     EJECT                                                                
033400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
033500     SKIP3                                                                
033600*01  -COPY WMFSAREA                                                       
033700     EJECT                                                                
033800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
033900*                                                                         
034000     EJECT                                                                
034100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
034200     SKIP3                                                                
034300 01  NYCKLAR-TILL-DLI.                                                    
034400     03  W-IDARTNR-X.                                                     
034500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
034600     03  W-IDDC-X.                                                        
034700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
034800     03  W-IDSKYLT-X.                                                     
034900         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
035000     03  W-IDLEVNR-X.                                                     
035100         05  W-IDLEVNR           PIC X(5).                                
035200     03  W-WDD901KY-X.                                                    
035300         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
035400         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
035500     03  ALT-IDLEVNR-X.                                                   
035600         05  ALT-IDLEVNR         PIC X(5).                                
035700     03  W-TILEVBSK-X.                                                    
035800         05  W-TILEVBSK          PIC S9(7)   VALUE ZERO COMP-3.           
035900     03  W-IDLEVBSK-X.                                                    
036000         05  W-IDLEVBSK          PIC S9(1)   VALUE +2   COMP-3.           
036100     03  W-IDLEVBSK-4-X.                                                  
036200         05  W-IDLEVBSK-4        PIC S9(1)   VALUE +4   COMP-3.           
036300     03  W-KDAVROP-X.                                                     
036400         05  W-KDAVROP           PIC S9(1)   VALUE +2   COMP-3.           
036500     03  W-KDSEGKEY-X.                                                    
036600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
036700     03  W-IDPTYP-X.                                                      
036800       05  W-IDPTYP      PIC X(3)   VALUE SPACE.                          
037200     03  W-IDDC-B6-X.                                                     
037300         05 W-IDDC-B6                  PIC X(2).                          
037400                                                                          
037500     SKIP2                                                                
037600*    --- STATUS-KOD FRÅN IMS                                              
037700 01  STATUS-WS                   PIC XX.                                  
037800     88  SEGMENT-FINNS                       VALUE '  '.                  
037900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
038000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
038100     88  SEGMENT-END                         VALUE 'GB'.                  
038200     SKIP2                                                                
038300 01  GODK-STATUSKODER.                                                    
038400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
038500     SKIP3                                                                
038600 01  SSA1                        PIC X(128).                              
038700 01  SSA2                        PIC X(128).                              
038800 01  SSA3                        PIC X(128).                              
038900     EJECT                                                                
039000*    --- IMS FUNKTIONSKODER                                               
039100*01  -COPY W0003                                                          
039200     EJECT                                                                
039300*    ---  DLI INPUT-OUTPUT AREA                                           
039400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
039500     SKIP3                                                                
039600                                                                          
039700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD301'.                      
039800 01  DLI-IO-WDD301.                                                       
039900*    03  -COPY WDD301                                                     
040000     EJECT                                                                
040100                                                                          
040200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
040300 01  DLI-IO-WDD311.                                                       
040400*    03  -COPY WDD311                                                     
040500     EJECT                                                                
040600                                                                          
040700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD901'.                      
040800 01  DLI-IO-WDD901.                                                       
040900*    03  -COPY WDD901                                                     
041000     EJECT                                                                
041100                                                                          
041200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD925'.                      
041300 01  DLI-IO-WDD925.                                                       
041400*    03  -COPY WDD925                                                     
041500     EJECT                                                                
041600                                                                          
041700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
041800 01  DLI-IO-WDK601.                                                       
041900*    03  -COPY WDK601                                                     
042000     EJECT                                                                
042100                                                                          
042200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
042300 01  DLI-IO-WDK611.                                                       
042400*    03  -COPY WDK611                                                     
042500     EJECT                                                                
042600                                                                          
042700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL601'.                      
042800 01  DLI-IO-WDL601.                                                       
042900*    03  -COPY WDL601                                                     
043000     EJECT                                                                
043100                                                                          
043200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL611'.                      
043300 01  DLI-IO-WDL611.                                                       
043400*    03  -COPY WDL611                                                     
043500     EJECT                                                                
043600                                                                          
043700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
043800 01  DLI-IO-WDK701.                                                       
043900*    03  -COPY WDK701                                                     
044000     EJECT                                                                
044100                                                                          
044200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
044300 01  DLI-IO-WDK711.                                                       
044400*    03  -COPY WDK711                                                     
044500     EJECT                                                                
044600                                                                          
045100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
045200 01   DLI-IO-AREA-B601.                                                   
045300*     03  -COPY WDB601                                                    
045400                                                                          
045500 01  FILLER               PIC X(16)   VALUE 'WDK901 AREA'.                
045600 01   DLI-IO-AREA-K901.                                                   
045700*     03  -COPY WDK901                                                    
045800                                                                          
045900 01  FILLER               PIC X(16)   VALUE 'WDA501 AREA'.                
046000 01   DLI-IO-AREA-A501.                                                   
046100*     03  -COPY WDA501                                                    
046200                                                                          
046300 01  FILLER               PIC X(16)   VALUE 'WDA601 AREA'.                
046400 01   DLI-IO-AREA-A601.                                                   
046500*     03  -COPY WDA601                                                    
046600                                                                          
046700     EJECT                                                                
046800 LINKAGE SECTION.                                                         
046900                                                                          
047000*01  -COPY W0009   -PRE MSG-                                              
047100     EJECT                                                                
047200*01  -COPY W0008  -PRE WDP7-                                              
047300     05  FILLER                  PIC X.                                   
047400     EJECT                                                                
047500*01  -COPY W0008  -PRE WDD3-                                              
047600     05  FILLER                  PIC X.                                   
047700     EJECT                                                                
047800*01  -COPY W0008  -PRE WDD9-                                              
047900         05  KFBA-IDARTNR     PIC S9(9) COMP-3.                           
048000         05  KFBA-IDLEVNR     PIC X(5).                                   
048100     EJECT                                                                
048200*01  -COPY W0008  -PRE WDK6-                                              
048300     05  FILLER                  PIC X.                                   
048400     EJECT                                                                
048500*01  -COPY W0008  -PRE WDL6-                                              
048600     05  FILLER                  PIC X.                                   
048700     EJECT                                                                
048800*01  -COPY W0008  -PRE WDK7-                                              
048900     05  FILLER                  PIC X.                                   
049400*01  -COPY W0008  -PRE WDB6-                                              
049500     05  FILLER                  PIC X.                                   
049600*01  -COPY W0008  -PRE WDK9-                                              
049700     05  FILLER                  PIC X.                                   
049800*01  -COPY W0008  -PRE WDA5-                                              
049900     05  FILLER                  PIC X.                                   
050000*01  -COPY W0008  -PRE WDA6J-                                             
050100     05  FILLER                  PIC X.                                   
050200                                                                          
050300     EJECT                                                                
050400 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB                               
050500                                   WDD3-PCB WDD9-PCB WDK6-PCB             
050600                                   WDL6-PCB WDK7-PCB                      
050700                                   WDB6-PCB WDK9-PCB                      
050800                                   WDA5-PCB WDA6J-PCB.                    
050900     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB                               
051000                                   WDD3-PCB WDD9-PCB WDK6-PCB             
051100                                   WDL6-PCB WDK7-PCB                      
051200                                   WDB6-PCB WDK9-PCB                      
051300                                   WDA5-PCB WDA6J-PCB.                    
051400                                                                          
051500     PERFORM IMS-GET-MSG                                                  
051600     IF SEGMENT-FINNS                                                     
051700       PERFORM A-INIT                                                     
051800       PERFORM B-KOLLA-NYCKLAR                                            
051900       IF NYCKLAR-OK                                                      
052000         PERFORM C-LAES-VISA-INFO                                         
052100       END-IF                                                             
053500                                                                          
053700       COMPUTE MSG-KVLL = LENGTH OF MOD-W9O11501 + 4                      
053800       PERFORM IMS-INSERT-MSG                                             
053900     END-IF                                                               
054000                                                                          
054100     MOVE ZERO TO RETURN-CODE                                             
054200     GOBACK                                                               
054300     .                                                                    
054400     EJECT                                                                
054500 A-INIT SECTION.                                                          
054600                                                                          
054700     MOVE WC-NDC-US-RU TO MIN-IDDC                                        
054800     MOVE WC-NDC-AE    TO MAX-IDDC                                        
055500                                                                          
055800     IF MSG-DUBBLA-TRANSKODER                                             
055900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W9I11501                 
056000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
056100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
056200     ELSE                                                                 
056300       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W9I11501                   
056400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
056500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
056600     END-IF                                                               
056700                                                                          
056800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
056900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
057000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
057100                                                                          
057200     MOVE LOW-VALUE TO MSG-AREA                                           
057300     MOVE 'W9O115N1' TO MFS-IDMOD                                         
057500     MOVE '9115' TO MOD-IDTRANS                                           
057600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
057700     PERFORM MFS-RENSA-FAELT-UT                                           
057800                                                                          
057900     MOVE 'GB' TO MED-IDSKYLT                                             
058000                                                                          
058100     ACCEPT DAGENS-DATUM FROM DATE                                        
058200                                                                          
058300                                                                          
058400     IF EGEN-MID OR HELP-MID                                              
058500       CONTINUE                                                           
058600     ELSE                                                                 
058700       MOVE SPACE TO MFS-KDTRTYP                                          
058800       MOVE '7' TO MFS-IDPFK                                              
058900     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 B-KOLLA-NYCKLAR SECTION.                                                 
063200                                                                          
063500     MOVE ALL '+'         TO MSGI-WMSGINIT                                
063600     MOVE '001'           TO MSGI-KDCALL                                  
063700     MOVE MSG-LTERM-NAME  TO MSGI-IDLTERM-USER                            
063800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
063900     MOVE '9115'          TO MSGI-IDTRANS                                 
064000     IF MFS-IDTRANS = '9115'                                              
064100     OR (MID-IDARTNR-IN NUMERIC                                           
064200     AND MID-IDARTNR-IN > ZERO)                                           
064300        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
064400     END-IF                                                               
064500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
064600                                                                          
064700     MOVE MSGI-SPAR-AREA  TO WS-MSGI-SPAR-AREA                            
064800     MOVE JA TO NYCKLAR-SW                                                
064900                                                                          
065000*    -- KONTROLL AV IDARTNR                                               
065100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
065200                                                                          
065300*    OM EJ PF8     ?                                                      
065400     IF MID-IDARTNR-IN NOT = ALL '+'                                      
065500       MOVE '7'       TO MFS-IDPFK                                        
065600       MOVE SPACE     TO MFS-KDTRTYP                                      
065700     END-IF                                                               
065800     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
065900     IF MSGI-IDARTNR NUMERIC                                              
066000       MOVE MSGI-IDARTNR TO WS-IDARTNR-NUM                                
066100       MOVE WS-IDARTNR-NUM TO W-IDARTNR                                   
066200     ELSE                                                                 
066300       MOVE NEJ TO NYCKLAR-SW                                             
066400     END-IF                                                               
066500                                                                          
066600*    -- KONTROLL AV IDDC                                                  
066700     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
066800                                                                          
066900*    OM EJ PF8 ?                                                          
067000     IF MID-IDDC-IN NOT = ALL '+'                                         
067100       MOVE '7'       TO MFS-IDPFK                                        
067200       MOVE SPACE     TO MFS-KDTRTYP                                      
067300     END-IF                                                               
067400     IF MID-IDDC-IN = '++'                                                
067500        MOVE SPACE    TO MID-IDDC-IN                                      
067600        MOVE MID-IDDC-UT TO W-IDDC-B6                                     
067700        PERFORM IMS-GU-WDB601                                             
067800        IF DCS-KDDC > SPACE AND NOT DCS-DDC                               
067900           MOVE MID-IDDC-UT TO MID-IDDC-IN                                
068000        END-IF                                                            
068100     END-IF                                                               
068200     IF MID-IDDC-IN NOT = DCS-IDDC                                        
068300        MOVE MID-IDDC-IN TO W-IDDC-B6                                     
068400        PERFORM IMS-GU-WDB601                                             
068500     END-IF                                                               
068600     IF DCS-NDC-NA OR DCS-NDC-PF OR DCS-NDC-CN OR DCS-CDC OR              
068700        DCS-KDDC = SPACE                                                  
068800        CONTINUE                                                          
068900     ELSE                                                                 
069000        IF EGEN-MID                                                       
069100           MOVE NEJ    TO NYCKLAR-SW                                      
069200        ELSE                                                              
069300           MOVE SPACE  TO MID-IDDC-IN                                     
069400        END-IF                                                            
069500     END-IF                                                               
069600                                                                          
069700     IF MID-IDDC-IN = SPACE                                               
069800        MOVE WC-NDC-US-RU TO MIN-IDDC                                     
069900        MOVE WC-NDC-AE    TO MAX-IDDC                                     
070000     ELSE                                                                 
070100        MOVE MID-IDDC-IN TO MIN-IDDC MAX-IDDC                             
070200     END-IF                                                               
070300                                                                          
070400     IF NYCKLAR-FEL                                                       
070500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
070600       CALL WMEDKONV USING MED-WMEDAREA                                   
070700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
070900       PERFORM MFS-RENSA-FAELT-UT                                         
071000     ELSE                                                                 
071100       MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT WS-IDARTNR-NUM            
071200       MOVE WS-IDARTNR-NUM    TO W-IDARTNR                                
071300       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
071400       MOVE MID-IDDC-IN       TO MOD-IDDC-UT                              
071500     END-IF                                                               
073400     .                                                                    
073500     EJECT                                                                
080700                                                                          
080800 C-LAES-VISA-INFO SECTION.                                                
080900                                                                          
081000                                                                          
081100     PERFORM IMS-GU-K601                                                  
081200                                                                          
081300     IF SEGMENT-FINNS                                                     
081400                                                                          
081500        MOVE ART-IDLEVNR TO SPARA-IDLEVNR                                 
081600                                                                          
081700        PERFORM CA-BERAEKNA-DATUM                                         
081800                                                                          
081900        MOVE ART-TIFINLV        TO TMP1-YYWWD                             
082000        MOVE DAT-TIAAVVD        TO TMP2-YYWWD                             
082100        PERFORM WY2000P2                                                  
082200        IF TMP1-YYWWD <= TMP2-YYWWD                                       
082300                                                                          
082400            PERFORM IMS-GNP-K611                                          
082500                                                                          
082600            IF SEGMENT-FINNS                                              
082700                IF (CLAG-KDUART = 'M' OR 'S')                             
082800                   OR (CLAG-KDERS = 52)                                   
082900                   OR (CLAG-FLLSRDEL = 'N')                               
083100                  MOVE ERR-ARTIKEL-SAKNAS TO MED-IDMFSFEL                 
083200                  CALL WMEDKONV USING MED-WMEDAREA                        
083300                  MOVE MED-MFSFEL TO MOD-TEMFSFEL                         
083700                ELSE                                                      
083800                   IF CLAG-KDERS > +10                                    
084000                     MOVE ERR-ARTIKEL-UTGANGEN TO MED-IDMFSINF            
084100                     CALL WMEDKONV USING MED-WMEDAREA                     
084200                     MOVE MED-MFSINF TO MOD-TEMFSINF                      
084600                   END-IF                                                 
084700                                                                          
085100                   PERFORM CC-LAES-VISA-BENAMNINGAR                       
085200                   PERFORM CD-LAES-WDD9                                   
085300                   PERFORM CF-LAES-WDL6                                   
085400                   PERFORM CG-SORTERA-INFO                                
085500                   PERFORM CH-VISA-INFO                                   
085800                END-IF                                                    
085900                                                                          
086000             ELSE                                                         
086200                MOVE ERR-ARTIKEL-SAKNAS TO MED-IDMFSFEL                   
086300                CALL WMEDKONV USING MED-WMEDAREA                          
086400                MOVE MED-MFSFEL TO MOD-TEMFSFEL                           
086800            END-IF                                                        
086900        ELSE                                                              
087100          MOVE ERR-NO-ORDER TO MED-IDMFSFEL                               
087200          CALL WMEDKONV USING MED-WMEDAREA                                
087300          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
087700        END-IF                                                            
087800     ELSE                                                                 
088000       MOVE ERR-ARTIKEL-SAKNAS TO MED-IDMFSFEL                            
088100       CALL WMEDKONV USING MED-WMEDAREA                                   
088200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
088600     END-IF                                                               
088700     .                                                                    
088800     EJECT                                                                
088900 CA-BERAEKNA-DATUM SECTION.                                               
089000                                                                          
089100                                                                          
089200     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
089300     CALL WDATKONV USING                                                  
089400          DAT-KDDATFORM,                                                  
089500          DAT-I-TIDATUM,                                                  
089600          DAT-O-TIDATUM,                                                  
089700          DAT-KDSVAR                                                      
089800                                                                          
089900     IF DAT-KDSVAR-OK                                                     
090000         ADD +5 TO DAT-TIVV                                               
090100         IF DAT-TIVV > 52                                                 
090200             COMPUTE DAT-TIVV = DAT-TIVV - 52                             
090300             ADD +1 TO DAT-TIAA-VECKA                                     
090400         END-IF                                                           
090500                                                                          
090600                                                                          
090700     ELSE                                                                 
090800         CALL FELLOG                                                      
090900     END-IF                                                               
091000                                                                          
091100     .                                                                    
091200     EJECT                                                                
092400 CC-LAES-VISA-BENAMNINGAR SECTION.                                        
092500                                                                          
092600     PERFORM IMS-GU-D301                                                  
092700                                                                          
092800     IF SEGMENT-FINNS                                                     
092900         MOVE 'GB ' TO W-IDSKYLT-X                                        
093000         PERFORM IMS-GNP-D311                                             
093100                                                                          
093200         IF SEGMENT-FINNS                                                 
093400           MOVE TEXT-BEART TO MOD-BEART-ENG                               
093600         END-IF                                                           
093700                                                                          
093800         MOVE 'S  ' TO W-IDSKYLT-X                                        
093900         PERFORM IMS-GNP-D311                                             
094000                                                                          
094100         IF SEGMENT-FINNS                                                 
094300           MOVE TEXT-BEART TO MOD-BEART-SVE                               
094500         END-IF                                                           
094600     END-IF                                                               
094700     .                                                                    
094800     EJECT                                                                
094900 CD-LAES-WDD9 SECTION.                                                    
095000                                                                          
095100     MOVE 'NONE' TO MOD-TELEVBSK-EXT                                      
095200     MOVE SPACE  TO MOD-TELEVBSK-EXT2                                     
095300                                                                          
095400     MOVE SPARA-IDLEVNR  TO W-IDLEVNR                                     
095500                                                                          
095600     MOVE W-IDARTNR      TO W-IDARTNR-D9                                  
095700     MOVE WC-CDC-SE      TO W-IDDC-D9                                     
095800     PERFORM IMS-GU-D901                                                  
095900                                                                          
096000     IF SEGMENT-FINNS                                                     
096100                                                                          
096200         PERFORM IMS-GNP-D925                                             
096300                                                                          
096400         IF SEGMENT-FINNS                                                 
096500             MOVE INFO-TIBORT   TO TMP1-YYMMDD                            
096600             MOVE DAGENS-DATUM       TO TMP2-YYMMDD                       
096700             PERFORM WY2000P1                                             
096800             IF TMP1-YYMMDD >= TMP2-YYMMDD                                
097000               MOVE INFO-TELEVBSK TO MOD-TELEVBSK-EXT                     
097400               PERFORM IMS-GNP-D925-TEXT2                                 
097500               IF SEGMENT-FINNS                                           
097700                 MOVE INFO-TELEVBSK TO MOD-TELEVBSK-EXT2                  
098100               ELSE                                                       
098200                 MOVE MFS-RENSA-FAELT      TO MOD-TELEVBSK-EXT2           
098400               END-IF                                                     
098500             ELSE                                                         
098600                 MOVE NEJ TO FLAGGA-WDD925                                
098700             END-IF                                                       
098800         ELSE                                                             
098900             MOVE NEJ TO FLAGGA-WDD925                                    
099000         END-IF                                                           
099100     ELSE                                                                 
099200         MOVE NEJ TO FLAGGA-WDD9                                          
099300         MOVE NEJ TO FLAGGA-WDD925                                        
099400     END-IF                                                               
099500                                                                          
099600     IF FLAGGA-WDD9 = JA AND                                              
099700        WDD925-FINNS-INTE                                                 
099800        IF ALT-IDLEVNR NOT = SPARA-IDLEVNR                                
099900           PERFORM IMS-GNP-D925-ALT                                       
100000                                                                          
100100           IF SEGMENT-FINNS                                               
100200              MOVE INFO-TIBORT        TO TMP1-YYMMDD                      
100300              MOVE DAGENS-DATUM       TO TMP2-YYMMDD                      
100400              PERFORM WY2000P1                                            
100500              IF TMP1-YYMMDD >= TMP2-YYMMDD                               
100600                 MOVE JA  TO FLAGGA-WDD925                                
100800                 MOVE INFO-TELEVBSK TO MOD-TELEVBSK-EXT                   
101200                 PERFORM IMS-GNP-D925-ALT-TEXT2                           
101300                 IF SEGMENT-FINNS                                         
101500                   MOVE INFO-TELEVBSK  TO MOD-TELEVBSK-EXT2               
102100                 END-IF                                                   
102200              END-IF                                                      
102300           END-IF                                                         
102400        END-IF                                                            
102500     END-IF                                                               
102600     .                                                                    
102700     EJECT                                                                
102800******************************************************************        
102900*IF INPUT DC GIVEN - ONLY ROWS FOR INPUT DC DISPLAYED (A IS TRUE)         
103000*IF ONLY PART NUMBER NUMBER AND NO DC GIVEN AS INPUTS - ROWS FOR          
103100*ALL DC INCLUDING DC 11 IS DISPLAYED (B IS TRUE)                          
103200******************************************************************        
103300 CF-LAES-WDL6      SECTION.                                               
103400                                                                          
103500     PERFORM IMS-GU-L601                                                  
103600     IF SEGMENT-FINNS                                                     
103700        PERFORM IMS-GNP-L611                                              
103800        PERFORM UNTIL SEGMENT-SAKNAS                                      
103900        OR IX-RAD > TAB-MAX                                               
104000* IF A OR B //A-(INL-IDDC >= MIN-IDDC AND INL-IDDC <= MAX-IDDC)           
104100*             B-(INL-IDDC = WC-CDC-SE)AND (MIN-IDDC NOT= MAX-IDDC)        
104200           IF (INL-IDDC >= MIN-IDDC AND                                   
104300               INL-IDDC <= MAX-IDDC ) OR                                  
104400             ((INL-IDDC = WC-CDC-SE) AND                                  
104500              (MIN-IDDC NOT = MAX-IDDC))                                  
104600              IF INL-IDPTYP = '310' OR 'R31' OR 'R30'                     
104700                 PERFORM CFA-FYLL-I-RAD-FRAN-WDL611                       
104800              END-IF                                                      
104900           END-IF                                                         
105000           PERFORM IMS-GNP-L611                                           
105100        END-PERFORM                                                       
105200     END-IF                                                               
105300     .                                                                    
105400     EJECT                                                                
105500                                                                          
105600 CFA-FYLL-I-RAD-FRAN-WDL611 SECTION.                                      
109500                                                                          
109510     MOVE ZERO             TO TAB-IDDC-PRIO-SORT (IX-RAD)                 
109520                                                                          
109600     MOVE INL-IDDC         TO TAB-IDDC      (IX-RAD)                      
109700                              TAB-IDDC-SORT (IX-RAD)                      
109800     IF INL-IDPTYP = '310' OR 'R30'                                       
109900       MOVE INL-KVAVIS     TO TAB-KVAVIS    (IX-RAD)                      
110000       MOVE INL-TIBERANK   TO TAB-TIBERANK  (IX-RAD)                      
110100                              TAB-TIBERANK-SORT (IX-RAD)                  
110200       MOVE ZERO           TO TAB-KVAKS (IX-RAD)                          
110300     ELSE                                                                 
110400*       (R31)                                                             
110500       IF INL-KDRT = 7                                                    
110600*             VISA EJ RETURER                                             
110700         CONTINUE                                                         
110800       ELSE                                                               
110900         MOVE INL-KVAVIS   TO TAB-KVAKS (IX-RAD)                          
111000         MOVE SPACE        TO TAB-KVAVIS    (IX-RAD)                      
111100                              TAB-TIBERANK-SORT (IX-RAD)                  
111200         MOVE ZERO         TO TAB-TIBERANK  (IX-RAD)                      
111300       END-IF                                                             
111400     END-IF                                                               
111500                                                                          
111600     ADD +1                TO IX-RAD                                      
111700     .                                                                    
111800     EJECT                                                                
111900                                                                          
112000 CG-SORTERA-INFO SECTION.                                                 
112100                                                                          
112200     SUBTRACT 1 FROM IX-RAD                                               
112300     MOVE IX-RAD TO ANTAL                                                 
112400                                                                          
112500     CALL WINTSOR USING TABELL STEGLANGD ANTAL                            
112600                  TAB-SORT (1) NYCKELLANGD                                
112692                                                                          
112700     .                                                                    
112800     EJECT                                                                
112900                                                                          
113000 CH-VISA-INFO SECTION.                                                    
113100                                                                          
113200***  HÄR LÄSES ETT STARTVÄRDE FÖR IX-RAD FRÅN USER-BASEN                  
113300***  SÅ ATT VID BLÄDDRING, START SKER MED RÄTT RAD                        
113400* *  MOVE MSGI-SPAR-AREA    TO WS-MSGI-SPAR-AREA                          
113500     IF MFS-IDPFK = '8' AND WS-MSGI-PGM = 'W90115'                        
113600        MOVE WS-MSGI-RADNR  TO IX-RAD-TAB                                 
113700     ELSE                                                                 
113800        MOVE +1             TO IX-RAD-TAB                                 
113900     END-IF                                                               
114000                                                                          
114100     MOVE +1 TO IX-RAD                                                    
114200     PERFORM UNTIL IX-RAD > 8 OR IX-RAD-TAB > TAB-MAX                     
114300     OR TAB-IDDC (IX-RAD-TAB) = SPACE                                     
114400     OR TAB-IDDC (IX-RAD-TAB) = LOW-VALUE                                 
114500        MOVE TAB-RAD (IX-RAD-TAB) TO MOD-GRP-RAD (IX-RAD)                 
114600        MOVE TAB-IDDC       (IX-RAD-TAB)                                  
114700                             TO MOD-IDDC (IX-RAD)                         
114800                                W-IDDC                                    
114900        MOVE TAB-KVAVIS     (IX-RAD-TAB)                                  
115000                             TO MOD-KVAVIS (IX-RAD)                       
115100        IF TAB-TIBERANK (IX-RAD-TAB) > ZERO                               
115200          MOVE TAB-TIBERANK (IX-RAD-TAB)                                  
115300                             TO MOD-TIBERANK (IX-RAD)                     
115400        ELSE                                                              
115500          MOVE SPACE         TO MOD-TIBERANK (IX-RAD)                     
115600        END-IF                                                            
115700        MOVE TAB-KVAKS      (IX-RAD-TAB)                                  
115800                             TO MOD-KVAKS (IX-RAD)                        
115900                                                                          
116000        IF (TAB-IDDC (IX-RAD-TAB) = WC-NDC-US-RU                          
116100        AND FLAGGA-WDK7-DC41 = NEJ)                                       
116200        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-US-LA                          
116300        AND FLAGGA-WDK7-DC43 = NEJ)                                       
116400        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-US-SE                          
116500        AND FLAGGA-WDK7-DC44 = NEJ)                                       
116600        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-US-CH                          
116700        AND FLAGGA-WDK7-DC45 = NEJ)                                       
116800        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-US-JA                          
116900        AND FLAGGA-WDK7-DC46 = NEJ)                                       
117000        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-US-DA                          
117100        AND FLAGGA-WDK7-DC47 = NEJ)                                       
117200        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-CA                             
117300        AND FLAGGA-WDK7-DC51 = NEJ)                                       
117400        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-JP-61                          
117500        AND FLAGGA-WDK7-DC61 = NEJ)                                       
117600        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-JP-6A                          
117700        AND FLAGGA-WDK7-DC6A = NEJ)                                       
117800        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-TH                             
117900        AND FLAGGA-WDK7-DC63 = NEJ)                                       
118000        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-TW                             
118100        AND FLAGGA-WDK7-DC64 = NEJ)                                       
118200        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-IN                             
118300        AND FLAGGA-WDK7-DC67 = NEJ)                                       
118400        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-KR                             
118500        AND FLAGGA-WDK7-DC65 = NEJ)                                       
118600        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-MY                             
118700        AND FLAGGA-WDK7-DC66 = NEJ)                                       
118800        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-AU                             
118900        AND FLAGGA-WDK7-DC62 = NEJ)                                       
119000        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-CN-71                          
119100        AND FLAGGA-WDK7-DC71 = NEJ)                                       
119200        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-CN-72                          
119300        AND FLAGGA-WDK7-DC72 = NEJ)                                       
119400        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-CN-73                          
119500        AND FLAGGA-WDK7-DC73 = NEJ)                                       
119600        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-CN-74                          
119700        AND FLAGGA-WDK7-DC74 = NEJ)                                       
119800        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-AE                             
119900        AND FLAGGA-WDK7-DC87 = NEJ)                                       
120000        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-BR                             
120100        AND FLAGGA-WDK7-DC52 = NEJ)                                       
120200        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-MX                             
120300        AND FLAGGA-WDK7-DC53 = NEJ)                                       
120400        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-ZA                             
120500        AND FLAGGA-WDK7-DC85 = NEJ)                                       
120600        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-TR                             
120700        AND FLAGGA-WDK7-DC86 = NEJ)                                       
120600        OR (TAB-IDDC (IX-RAD-TAB) = WC-NDC-TH-93                          
120700        AND FLAGGA-WDK7-DC93 = NEJ)                                       
120800                                                                          
120900          PERFORM IMS-GU-K711                                             
121000                                                                          
121100          IF SEGMENT-FINNS                                                
121200            MOVE SLAG-KVROS-DAG                                           
121300                             TO MOD-KVROS-DAG  (IX-RAD)                   
121400            MOVE SLAG-KVROS-BULK                                          
121500                             TO MOD-KVROS-BULK (IX-RAD)                   
121600            MOVE SLAG-KVOKS-DAG                                           
121700                             TO MOD-KVOKS-DAG  (IX-RAD)                   
121800            MOVE SLAG-KVOKS-BULK                                          
121900                             TO MOD-KVOKS-BULK (IX-RAD)                   
122000          ELSE                                                            
122100            MOVE ZERO        TO MOD-KVROS-DAG  (IX-RAD)                   
122200                                MOD-KVROS-BULK (IX-RAD)                   
122300                                MOD-KVOKS-DAG  (IX-RAD)                   
122400                                MOD-KVOKS-BULK (IX-RAD)                   
122500          END-IF                                                          
122600                                                                          
122700          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-US-RU                         
122800            MOVE JA          TO FLAGGA-WDK7-DC41                          
122900          END-IF                                                          
123000          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-US-LA                         
123100            MOVE JA          TO FLAGGA-WDK7-DC43                          
123200          END-IF                                                          
123300          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-US-SE                         
123400            MOVE JA          TO FLAGGA-WDK7-DC44                          
123500          END-IF                                                          
123600          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-US-CH                         
123700            MOVE JA          TO FLAGGA-WDK7-DC45                          
123800          END-IF                                                          
123900          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-US-JA                         
124000            MOVE JA          TO FLAGGA-WDK7-DC46                          
124100          END-IF                                                          
124200          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-US-DA                         
124300            MOVE JA          TO FLAGGA-WDK7-DC47                          
124400          END-IF                                                          
124500          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-CA                            
124600            MOVE JA          TO FLAGGA-WDK7-DC51                          
124700          END-IF                                                          
124800          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-JP-61                         
124900            MOVE JA          TO FLAGGA-WDK7-DC61                          
125000          END-IF                                                          
125100          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-JP-6A                         
125200            MOVE JA          TO FLAGGA-WDK7-DC6A                          
125300          END-IF                                                          
125400          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-AU                            
125500            MOVE JA          TO FLAGGA-WDK7-DC62                          
125600          END-IF                                                          
125700          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-TH                            
125800            MOVE JA          TO FLAGGA-WDK7-DC63                          
125900          END-IF                                                          
126000          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-TW                            
126100            MOVE JA          TO FLAGGA-WDK7-DC64                          
126200          END-IF                                                          
126300          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-IN                            
126400            MOVE JA          TO FLAGGA-WDK7-DC67                          
126500          END-IF                                                          
126600          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-KR                            
126700            MOVE JA          TO FLAGGA-WDK7-DC65                          
126800          END-IF                                                          
126900          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-MY                            
127000            MOVE JA          TO FLAGGA-WDK7-DC66                          
127100          END-IF                                                          
127200          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-CN-71                         
127300            MOVE JA          TO FLAGGA-WDK7-DC71                          
127400          END-IF                                                          
127500          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-CN-72                         
127600            MOVE JA          TO FLAGGA-WDK7-DC72                          
127700          END-IF                                                          
127800          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-CN-73                         
127900            MOVE JA          TO FLAGGA-WDK7-DC73                          
128000          END-IF                                                          
128100          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-CN-74                         
128200            MOVE JA          TO FLAGGA-WDK7-DC74                          
128300          END-IF                                                          
128400          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-AE                            
128500            MOVE JA          TO FLAGGA-WDK7-DC87                          
128600          END-IF                                                          
128700          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-BR                            
128800            MOVE JA          TO FLAGGA-WDK7-DC52                          
128900          END-IF                                                          
129000          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-MX                            
129100            MOVE JA          TO FLAGGA-WDK7-DC53                          
129200          END-IF                                                          
129300          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-ZA                            
129400            MOVE JA          TO FLAGGA-WDK7-DC85                          
130000          END-IF                                                          
131000          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-TR                            
131100            MOVE JA          TO FLAGGA-WDK7-DC86                          
131200          END-IF                                                          
131000          IF TAB-IDDC (IX-RAD-TAB) = WC-NDC-TH                            
131100            MOVE JA          TO FLAGGA-WDK7-DC93                          
131200          END-IF                                                          
131300        ELSE                                                              
131400          IF (TAB-IDDC (IX-RAD-TAB) = WC-CDC-SE                           
131500          AND FLAGGA-CDC = NEJ)                                           
131600                                                                          
131700              PERFORM CHA-GET-ON-ORDER-CDC                                
131800              PERFORM CHB-GET-BO-ORDER-CDC                                
131900                                                                          
132000              IF TAB-IDDC (IX-RAD-TAB) = WC-CDC-SE                        
132100                MOVE JA          TO FLAGGA-CDC                            
132200              END-IF                                                      
132300                                                                          
132400          END-IF                                                          
132500        END-IF                                                            
132600                                                                          
132700        ADD +1 TO IX-RAD IX-RAD-TAB                                       
132800     END-PERFORM                                                          
132900                                                                          
133000***  OM FLER RADER FINNS, SÅ SPARAS I USERBASEN NÄSTA RADNR               
133100***  FRÅN TABELLEN , ANNARS BLANKAS                                       
133200     IF IX-RAD-TAB       <= ANTAL                                         
133300        MOVE IX-RAD-TAB TO WS-MSGI-RADNR                                  
133400        MOVE 'W90115'   TO WS-MSGI-PGM                                    
133500        MOVE MORE-LINES TO MOD-TEMFSINF                                   
133600     ELSE                                                                 
133700        MOVE SPACE      TO WS-MSGI-SPAR-AREA                              
133800     END-IF                                                               
133900                                                                          
134000     MOVE '002'             TO MSGI-KDCALL                                
134100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
134200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
134300     MOVE '9115'            TO MSGI-IDTRANS                               
134400     MOVE WS-MSGI-SPAR-AREA TO MSGI-SPAR-AREA                             
134500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
134600     .                                                                    
134700     EJECT                                                                
134800*****************************************************************         
134900*FOR ON ORDER DAY & BULK QUANTITY, QUERY WDK9                             
135000*****************************************************************         
135100 CHA-GET-ON-ORDER-CDC SECTION.                                            
135200                                                                          
135300     PERFORM IMS-GU-WDK901                                                
135400     IF SEGMENT-FINNS                                                     
135500        MOVE ART-KVOKS-VOR     TO WS-WDK901-OKS-VOR                       
135600        MOVE ART-KVOKS-DAG     TO WS-WDK901-OKS-DAY                       
135700                                                                          
135800        COMPUTE WS-WDK901-OKS-TOT = WS-WDK901-OKS-VOR +                   
135900                                    WS-WDK901-OKS-DAY                     
136000                                                                          
136100        MOVE WS-WDK901-OKS-TOT TO MOD-KVOKS-DAG  (IX-RAD)                 
136200        MOVE ART-KVOKS-BULK    TO MOD-KVOKS-BULK (IX-RAD)                 
136300                                                                          
136400     ELSE                                                                 
136500        MOVE ZEROS             TO MOD-KVOKS-DAG  (IX-RAD)                 
136600                                  MOD-KVOKS-BULK (IX-RAD)                 
136700     END-IF                                                               
136800     .                                                                    
136900     EJECT                                                                
137000                                                                          
137100*****************************************************************         
137200*FOR BO ON CDC, QUERY WDA5 FOR CLASS 1,2,3,4 ORDERS                       
137300*               QUERY WDA6 FOR VOR ORDERS                                 
137400*FOR DAY ORDERS,SUMMARIZE VOR AND CLASS 1 QTY                             
137500*FOR BULK ORDER,SUMMARIZE CLASS 2,3,4,QTY                                 
137600*****************************************************************         
137700 CHB-GET-BO-ORDER-CDC SECTION.                                            
137800                                                                          
137900     PERFORM CHBA-GET-CL1234-BO-WDA5                                      
138000     PERFORM CHBB-GET-VOR-BO-WDA6                                         
138100                                                                          
138200     COMPUTE W-BO-QTY-DAY = W-BO-QTY-VOR + W-BO-QTY-C1                    
138300                                                                          
138400     MOVE W-BO-QTY-DAY         TO MOD-KVROS-DAG  (IX-RAD)                 
138500     MOVE W-BO-QTY-C234        TO MOD-KVROS-BULK (IX-RAD)                 
138600     .                                                                    
138700     EJECT                                                                
138800                                                                          
138900 CHBA-GET-CL1234-BO-WDA5 SECTION.                                         
139000                                                                          
139100     MOVE W-IDARTNR            TO W-WDA5A-IDARTNR-MIN                     
139200                                  W-WDA5A-IDARTNR-MAX                     
139300     MOVE W-IDDC               TO W-WDA5A-IDDC-MIN                        
139400                                  W-WDA5A-IDDC-MAX                        
139500     MOVE ZEROES               TO W-BO-QTY-C1                             
139600                                  W-BO-QTY-C234                           
139700     PERFORM IMS-GU-WDA5ASEQ                                              
139800     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
139900                   SEGMENT-END                                            
140000        IF RAD-KDSTARAD = '2'                                             
140100           IF RAD-KDORDKL = 1                                             
140200              COMPUTE W-BO-QTY-C1 = W-BO-QTY-C1 + RAD-KVRO                
140300           ELSE                                                           
140400              IF (RAD-KDORDKL = 1 OR 2 OR 3)                              
140500                 COMPUTE W-BO-QTY-C234 = W-BO-QTY-C234 +                  
140600                                         RAD-KVRO                         
140700              END-IF                                                      
140800           END-IF                                                         
140900        END-IF                                                            
141000        PERFORM IMS-GN-WDA5ASEQ                                           
141100     END-PERFORM                                                          
141200     .                                                                    
141300     EJECT                                                                
141400                                                                          
141500                                                                          
141600 CHBB-GET-VOR-BO-WDA6 SECTION.                                            
141700                                                                          
141800     MOVE LOW-VALUE             TO W-WDA6JSEQ-MIN-X                       
141900     MOVE HIGH-VALUE            TO W-WDA6JSEQ-MAX-X                       
142000                                                                          
142100     MOVE W-IDARTNR             TO SEQJ-IDARTNR-MIN                       
142200                                   SEQJ-IDARTNR-MAX                       
142300     MOVE ZEROES                TO W-BO-QTY-VOR                           
142400                                                                          
142500     PERFORM IMS-GU-WDA6JSEQ                                              
142600     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
142700                   SEGMENT-END                                            
142800        COMPUTE W-BO-QTY-VOR = W-BO-QTY-VOR + VOR-KVBEART-Q               
142900        PERFORM IMS-GN-WDA6JSEQ                                           
143000     END-PERFORM                                                          
143100     .                                                                    
143200     EJECT                                                                
143300                                                                          
165100 MFS-RENSA-FAELT-UT SECTION.                                              
165200                                                                          
165300*    --- ALLA UTDATA-FÄLT                                                 
165400                                                                          
165500     MOVE MFS-RENSA-FAELT TO MOD-BEART-SVE                                
165600                             MOD-BEART-ENG                                
165700                             MOD-TELEVBSK-EXT                             
165800                             MOD-TELEVBSK-EXT2                            
165900                                                                          
166000     MOVE +1 TO MOD-IX                                                    
166100     PERFORM UNTIL MOD-IX > 8                                             
166200       MOVE MFS-RENSA-FAELT  TO MOD-IDDC       (MOD-IX)                   
166300                                MOD-KVAVIS     (MOD-IX)                   
166400                                MOD-TIBERANK   (MOD-IX)                   
166500                                MOD-KVROS-DAG  (MOD-IX)                   
166600                                MOD-KVROS-BULK (MOD-IX)                   
166700                                MOD-KVOKS-DAG  (MOD-IX)                   
166800                                MOD-KVOKS-BULK (MOD-IX)                   
166900                                MOD-KVAKS      (MOD-IX)                   
167000         ADD +1 TO MOD-IX                                                 
167100     END-PERFORM                                                          
167200     .                                                                    
167300     EJECT                                                                
167400 IMS-GET-MSG SECTION.                                                     
167500                                                                          
167600     MOVE '  QC' TO GODK-STATUSKODER                                      
167700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
167800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
167900     PERFORM IMS-STATUSKONTROLL                                           
168000     .                                                                    
168100     SKIP3                                                                
168200 IMS-INSERT-MSG SECTION.                                                  
168300                                                                          
168400     IF ENGLISH-TEXT                                                      
168500       MOVE 'N' TO MFS-KDHUVOMR                                           
168600     END-IF                                                               
168700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
168800     MOVE SPACE TO GODK-STATUSKODER                                       
168900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
169000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
169100     PERFORM IMS-STATUSKONTROLL                                           
169200     .                                                                    
169300     EJECT                                                                
169400 IMS-GU-D301 SECTION.                                                     
169500                                                                          
169600     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
169700          DELIMITED BY SIZE INTO SSA1                                     
169800     MOVE '  GE' TO GODK-STATUSKODER                                      
169900     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
170000     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
170100     PERFORM IMS-STATUSKONTROLL                                           
170200     .                                                                    
170300     EJECT                                                                
170400 IMS-GNP-D311 SECTION.                                                    
170500                                                                          
170600     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
170700          DELIMITED BY SIZE INTO SSA1                                     
170800     MOVE '  GE' TO GODK-STATUSKODER                                      
170900     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
171000     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
171100     PERFORM IMS-STATUSKONTROLL                                           
171200     .                                                                    
171300     EJECT                                                                
171400 IMS-GU-D901 SECTION.                                                     
171500                                                                          
171600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
171700          DELIMITED BY SIZE INTO SSA1                                     
171800     MOVE '  GE' TO GODK-STATUSKODER                                      
171900     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
172000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
172100     PERFORM IMS-STATUSKONTROLL                                           
172200     .                                                                    
172300     EJECT                                                                
172400 IMS-GNP-D925 SECTION.                                                    
172500                                                                          
172600     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
172700          DELIMITED BY SIZE INTO SSA1                                     
172800     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
172900          DELIMITED BY SIZE INTO SSA2                                     
173000     MOVE '  GE' TO GODK-STATUSKODER                                      
173100     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD925 SSA1 SSA2              
173200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
173300     PERFORM IMS-STATUSKONTROLL                                           
173400     .                                                                    
173500     SKIP3                                                                
173600 IMS-GNP-D925-ALT SECTION.                                                
173700                                                                          
173800     STRING 'WDD902  *F(IDLEVNR  =' ALT-IDLEVNR-X ')'                     
173900          DELIMITED BY SIZE INTO SSA1                                     
174000     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
174100          DELIMITED BY SIZE INTO SSA2                                     
174200     MOVE '  GE' TO GODK-STATUSKODER                                      
174300     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD925 SSA1 SSA2              
174400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
174500     PERFORM IMS-STATUSKONTROLL                                           
174600     .                                                                    
174700     EJECT                                                                
174800 IMS-GNP-D925-TEXT2 SECTION.                                              
174900                                                                          
175000     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
175100          DELIMITED BY SIZE INTO SSA1                                     
175200     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-4-X ')'                      
175300          DELIMITED BY SIZE INTO SSA2                                     
175400     MOVE '  GE' TO GODK-STATUSKODER                                      
175500     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD925 SSA1 SSA2              
175600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
175700     PERFORM IMS-STATUSKONTROLL                                           
175800     .                                                                    
175900     SKIP3                                                                
176000 IMS-GNP-D925-ALT-TEXT2 SECTION.                                          
176100                                                                          
176200     STRING 'WDD902  (IDLEVNR  =' ALT-IDLEVNR-X ')'                       
176300          DELIMITED BY SIZE INTO SSA1                                     
176400     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-4-X ')'                      
176500          DELIMITED BY SIZE INTO SSA2                                     
176600     MOVE '  GE' TO GODK-STATUSKODER                                      
176700     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD925 SSA1 SSA2              
176800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
176900     PERFORM IMS-STATUSKONTROLL                                           
177000     .                                                                    
177100     EJECT                                                                
177200 IMS-GU-K601 SECTION.                                                     
177300                                                                          
177400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
177500          DELIMITED BY SIZE INTO SSA1                                     
177600     MOVE '  GE' TO GODK-STATUSKODER                                      
177700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
177800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
177900     PERFORM IMS-STATUSKONTROLL                                           
178000     .                                                                    
178100     EJECT                                                                
178200 IMS-GNP-K611 SECTION.                                                    
178300                                                                          
178400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
178500          DELIMITED BY SIZE INTO SSA1                                     
178600     MOVE '  GE' TO GODK-STATUSKODER                                      
178700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
178800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
178900     PERFORM IMS-STATUSKONTROLL                                           
179000     .                                                                    
179100     EJECT                                                                
179200 IMS-GU-L601 SECTION.                                                     
179300                                                                          
179400     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
179500          DELIMITED BY SIZE INTO SSA1                                     
179600     MOVE '  GE' TO GODK-STATUSKODER                                      
179700     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
179800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
179900     PERFORM IMS-STATUSKONTROLL                                           
180000     .                                                                    
180100     SKIP2                                                                
180200 IMS-GNP-L611 SECTION.                                                    
180300                                                                          
180400     STRING 'WDL611     '                                                 
180500          DELIMITED BY SIZE INTO SSA1                                     
180600     MOVE '  GE' TO GODK-STATUSKODER                                      
180700     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
180800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
180900     PERFORM IMS-STATUSKONTROLL                                           
181000     .                                                                    
181100     EJECT                                                                
181200 IMS-GU-K711 SECTION.                                                     
181300                                                                          
181400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
181500             DELIMITED BY SIZE INTO SSA1                                  
181600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
181700             DELIMITED BY SIZE INTO SSA2                                  
181800     MOVE '  GE' TO GODK-STATUSKODER                                      
181900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
182000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
182100     PERFORM IMS-STATUSKONTROLL                                           
182200     .                                                                    
182300     EJECT                                                                
183400 IMS-GU-WDB601    SECTION.                                                
183500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
183600          DELIMITED BY SIZE INTO SSA1                                     
183700     MOVE '  GE' TO GODK-STATUSKODER                                      
183800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
183900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
184000     PERFORM IMS-STATUSKONTROLL                                           
184100     IF SEGMENT-SAKNAS                                                    
184200         MOVE SPACE TO DCS-KDDC                                           
184300     END-IF                                                               
184400     .                                                                    
184500 IMS-GU-WDK901  SECTION.                                                  
184600     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
184700          DELIMITED BY SIZE INTO SSA1                                     
184800     MOVE '  GE' TO GODK-STATUSKODER                                      
184900     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-AREA-K901 SSA1                 
185000     MOVE WDK9-STATUS-CODE    TO STATUS-WS                                
185100     PERFORM IMS-STATUSKONTROLL                                           
185200     .                                                                    
185300     EJECT                                                                
185400 IMS-GU-WDA5ASEQ SECTION.                                                 
185500                                                                          
185600     STRING 'WDA501  (WDA5ASEQ>=' W-WDA5ASEQ-MIN-X                        
185700                    '&WDA5ASEQ<=' W-WDA5ASEQ-MAX-X ')'                    
185800          DELIMITED BY SIZE INTO SSA1                                     
185900     MOVE '  GE'            TO GODK-STATUSKODER                           
186000     CALL CBLTDLI USING GU WDA5-PCB DLI-IO-AREA-A501 SSA1                 
186100     MOVE WDA5-STATUS-CODE   TO STATUS-WS                                 
186200     PERFORM IMS-STATUSKONTROLL                                           
186300     .                                                                    
186400                                                                          
186500 IMS-GN-WDA5ASEQ SECTION.                                                 
186600                                                                          
186700     STRING 'WDA501  (WDA5ASEQ>=' W-WDA5ASEQ-MIN-X                        
186800                    '&WDA5ASEQ<=' W-WDA5ASEQ-MAX-X ')'                    
186900          DELIMITED BY SIZE INTO SSA1                                     
187000     MOVE '  GEGB'          TO GODK-STATUSKODER                           
187100     CALL CBLTDLI USING GN WDA5-PCB DLI-IO-AREA-A501 SSA1                 
187200     MOVE WDA5-STATUS-CODE   TO STATUS-WS                                 
187300     PERFORM IMS-STATUSKONTROLL                                           
187400     .                                                                    
187500                                                                          
187600 IMS-GU-WDA6JSEQ SECTION.                                                 
187700                                                                          
187800     STRING 'WDA601  (WDA6JSEQ>=' W-WDA6JSEQ-MIN-X                        
187900                    '&WDA6JSEQ<=' W-WDA6JSEQ-MAX-X ')'                    
188000          DELIMITED BY SIZE INTO SSA1                                     
188100     MOVE '  GE' TO GODK-STATUSKODER                                      
188200     CALL CBLTDLI USING GU WDA6J-PCB DLI-IO-AREA-A601 SSA1                
188300     MOVE WDA6J-STATUS-CODE TO STATUS-WS                                  
188400     PERFORM IMS-STATUSKONTROLL                                           
188500     .                                                                    
188600                                                                          
188700 IMS-GN-WDA6JSEQ SECTION.                                                 
188800                                                                          
188900     STRING 'WDA601  (WDA6JSEQ>=' W-WDA6JSEQ-MIN-X                        
189000                    '&WDA6JSEQ<=' W-WDA6JSEQ-MAX-X ')'                    
189100          DELIMITED BY SIZE INTO SSA1                                     
189200     MOVE '  GEGB'          TO GODK-STATUSKODER                           
189300     CALL CBLTDLI USING GN WDA6J-PCB DLI-IO-AREA-A601 SSA1                
189400     MOVE WDA6J-STATUS-CODE TO STATUS-WS                                  
189500     PERFORM IMS-STATUSKONTROLL                                           
189600     .                                                                    
189700     EJECT                                                                
189800 IMS-STATUSKONTROLL SECTION.                                              
189900                                                                          
190000     SET STATUS-IX TO 1                                                   
190100     SEARCH GODK-STATUS                                                   
190200       AT END                                                             
190300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
190400         DELIMITED BY SIZE INTO FELTEXT                                   
190500         CALL FELLOG                                                      
190600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
190700         CONTINUE                                                         
190800     END-SEARCH                                                           
190900     .                                                                    
191000     EJECT                                                                
191100*    -COPY WY2000P1                                                       
191200     EJECT                                                                
191300*    -COPY WY2000P2                                                       
