000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W9043800.                                                
000400 AUTHOR.         EGHOLT CONNY.                                            
000500 DATE-WRITTEN.   07/10/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        DATAINSAMLINGSPROGRAM FÖR SPIE2                                  
001000*                          ETRACKER 3408639 + 5763467                     
001100*                                                                         
001200*        SAMLAR URVALDA INFO-FÄLT FRÅN FÖLJANDE PULS-BILDER               
001300*        2101, 2102, 2103, 2107, 2111, 3304 OCH 6108.                     
001400*                                                                         
001500*        SOM INPUT GODKÄNNS IDARTNR OCH/ELLER IDLEVNR.                    
001600*                                                                         
001700*        ARTIKELSTATISTIK-SÖKNING SKER MED ARTIKELNUMMER                  
001800*        I DB2-TABELL FSG2 WORLD-WIDE.                                    
001900*                                                                         
002000*        PROGRAMMET LÄSER      WDK6                                       
002100*        PROGRAMMET LÄSER      WDK7                                       
002200*        PROGRAMMET LÄSER      WDD9                                       
002300*        PROGRAMMET LÄSER      WDF1                                       
002400*        PROGRAMMET LÄSER      WDP3, WDP3A, WDP3B, WDP3C                  
002500*        PROGRAMMET LÄSER      WDL8                                       
002600*        PROGRAMMET LÄSER      FSG2                                       
002700*                                                                         
002800*        PROGRAMMET LÄSER VIA W222PBTO  WLARTC                            
002900*        PROGRAMMET LÄSER VIA W222PBTO  WLARTS                            
003000*        PROGRAMMET LÄSER VIA W222PBTO  WLARTM                            
003100*        PROGRAMMET LÄSER VIA W222PBTO  WL2501                            
003200*        PROGRAMMET LÄSER VIA W222PBTO  WDB6                              
003300*        PROGRAMMET LÄSER VIA W222PBTO  WDD7                              
003400*        PROGRAMMET LÄSER VIA W222PBTO  WDK7E                             
003500*                                                                         
003600*    INDATA.                                                              
003700*        TRANSAKTION: W90438T                                             
003800*        MID:         W90438I1                                            
003900*                                                                         
004000*    UTDATA.                                                              
004100*        MOD:         W90438O1                                            
004200*                                                                         
004300*    2012-01-09  E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1             
004400*                                                                         
004500                                                                          
004600     SKIP3                                                                
004700 ENVIRONMENT DIVISION.                                                    
004800                                                                          
004900 DATA DIVISION.                                                           
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200 77  IDPGM                       PIC X(08)   VALUE 'W9043800'.            
005300                                                                          
005400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005600                                                                          
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  SW-TRAEFF                   PIC X       VALUE SPACE.                 
006000 77  ART-FINNS                   PIC X       VALUE 'N'.                   
006100 77  CLAG-FINNS                  PIC X       VALUE 'N'.                   
006200 77  SPIND                       PIC S9(9)   VALUE +0 COMP SYNC.          
006300 77  MAX-PG                      PIC S9(3)   VALUE +8   COMP SYNC.        
006400 77  MAX-IDATTENT                PIC S9(3)   VALUE +7   COMP SYNC.        
006500 77  MAX-TILEVDAGAR              PIC 9(5)    VALUE 5.                     
006600                                                                          
006700 77  IX-AR                       PIC 9(9)    VALUE ZERO  COMP-3.          
006800 77  IX-PER                      PIC 9(9)    VALUE ZERO  COMP-3.          
006900                                                                          
007000 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
007100 77  SPAR-IDFKNGRP               PIC S9(5)   VALUE ZERO COMP-3.           
007200 77  SPAR-IDLEVNR                PIC X(5)    VALUE SPACE.                 
007300 77  SPAR-IDANSK                 PIC S9(3)   VALUE +0 COMP-3.             
007400 77  SPAR-IDBERED                PIC S9(3)   VALUE +0 COMP-3.             
007500 77  SPAR-IDINK                  PIC S9(3)   VALUE +0 COMP-3.             
007600 77  SPAR-IDPERSON-ANSV          PIC S9(3)   VALUE +0 COMP-3.             
007700 77  WS-IDPERSON                 PIC S9(3)   VALUE +0 COMP-3.             
007800 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007900 77  DAGENS-PER                  PIC  9(4)   VALUE ZERO.                  
008000 77  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
008100                                                                          
008200 01  W.                                                                   
008300     03  W-ARB-SALDO         PIC S9(7)    VALUE ZERO COMP-3.              
008400     03  W-DISPONIBELT       PIC S9(7)    VALUE ZERO COMP-3.              
008500     03  W-CDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.              
008600     03  W-SDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.              
008700     03  W-NDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.              
008800     03  W-SDC-KVRESS        PIC S9(7)    VALUE ZERO COMP-3.              
008900     03  W-NDC-KVRESS        PIC S9(7)    VALUE ZERO COMP-3.              
009000     03  W-NDC-KVROS         PIC S9(7)    VALUE ZERO COMP-3.              
009100     03  WS-KVOKS-TOT        PIC S9(9)    VALUE ZERO COMP-3.              
009200     03  WS-SUTPO-TOT        PIC S9(9)    VALUE ZERO COMP-3.              
009300     03  W-KVAKS             PIC S9(7)    VALUE ZERO COMP-3.              
009400     03  W-SDC-KVAKS-SDC     PIC S9(7)    VALUE ZERO COMP-3.              
009500     03  W-NDC-KVAKS-NDC     PIC S9(7)    VALUE ZERO COMP-3.              
009600     03  W-TIAAAAVV.                                                      
009700       05 W-TISEKEL          PIC  9(2)    VALUE ZERO.                     
009800       05 W-TIAA             PIC  9(2)    VALUE ZERO.                     
009900       05 W-TIVV             PIC  9(2)    VALUE ZERO.                     
010000     03  W-DADISPIN          PIC  9(6)    VALUE ZERO.                     
010100     03  WS-TPO-NAESTA-INLEV PIC S9(9)    VALUE ZERO COMP-3.              
010200***  03  W-OVERLAGER-SDC     PIC S9(7)               COMP-3.              
010300                                                                          
010400 01  WS-WDL8-CALL.                                                        
010500     03 WS-DAGENS-AAAAMMDD       PIC 9(8)    VALUE ZERO.                  
010600     03 FILLER                   PIC X(08)   VALUE 'WS-AAPP'.             
010700     03 WS-AAPP                  PIC 9(4)    VALUE ZERO.                  
010800     03 FILLER REDEFINES         WS-AAPP.                                 
010900       05 WS-AA                  PIC 9(2).                                
011000       05 WS-PP                  PIC 9(2).                                
011100     03 WS-VAL                   PIC X(1)    VALUE SPACE.                 
011200     03 WS-AARTAL                PIC 9(4)    VALUE ZERO.                  
011300     03 WS-PER                   PIC 9(2)    VALUE ZERO.                  
011400     03 WS-KVOI-RED              PIC 9(7)    VALUE ZERO.                  
011500     03 WS-TOTAL-KVOI            PIC 9(7)    VALUE ZERO.                  
011600     03 WS-KVOI-PER      OCCURS 6 TIMES.                                  
011700       05 WS-KVOI-TOT-TAB        PIC 9(7)        VALUE ZERO.              
011800       05 WS-KVOI-TAB    OCCURS 12 TIMES                                  
011900                                    PIC 9(7)     VALUE ZERO.              
012000     03 WS-VV                    PIC  9(2)   VALUE ZERO.                  
012100     03 FILLER                   PIC  X(16)  VALUE 'WS-TABELL'.           
012200     03 WS-TABELL  OCCURS 12.                                             
012300       05 WS-FORSTA-V            PIC  9(2)   VALUE ZERO.                  
012400       05 WS-SISTA-V             PIC  9(2)   VALUE ZERO.                  
012500       05 WS-KVVIPER             PIC 9       VALUE ZERO.                  
012600       05 WS-KVOI                PIC S9(7)   VALUE ZERO COMP-3.           
012700     03 WS-TESTFAELT.                                                     
012800       05 WS-FORSTA-TF           PIC  9(2)   VALUE ZERO.                  
012900       05 FILLER                 PIC  X      VALUE SPACE.                 
013000       05 WS-SISTA-TF            PIC  9(2)   VALUE ZERO.                  
013100       05 FILLER                 PIC  X      VALUE SPACE.                 
013200       05 WS-KVOI-TF             PIC  9(7)   VALUE ZERO.                  
013300     EJECT                                                                
013400                                                                          
013500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013600     88  NYCKLAR-OK                          VALUE 'J'.                   
013700     88  NYCKLAR-FEL                         VALUE 'N'.                   
013800                                                                          
013900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014000     88  EGEN-MID                            VALUE '9438'.                
014100     88  GODK-MID                            VALUE '9438'.                
014200     EJECT                                                                
014300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014400 01  GENERELLA-SUBPROGRAM.                                                
014500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014900     03  W222PBTO                PIC X(8)    VALUE 'W222PBTO'.            
015000     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
015100     EJECT                                                                
015200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
015300*01  -COPY WDATAREA                                                       
015400     EJECT                                                                
015500                                                                          
015600*01  -COPY W222PBTO                                                       
015700     EJECT                                                                
015800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015900*                                                                         
016000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016100     SKIP3                                                                
016200*01 -COPY WMSGINIT                                                        
016300     EJECT                                                                
016400*                                                                         
016500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016600     SKIP3                                                                
016700*01  MID -COPY W90438I1                                                   
016800     EJECT                                                                
016900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017000     SKIP3                                                                
017100*01  -COPY WMSGAREA                                                       
017200     EJECT                                                                
017300     03  MOD REDEFINES MSG-AREA.                                          
017400*      05  -COPY W90438O1                                                 
017500     EJECT                                                                
017600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017700     SKIP3                                                                
017800*01  -COPY WMFSAREA                                                       
017900     EJECT                                                                
018000*                                                                         
018100*01  FILLER -COPY FSG2 -PRE FSG-                                          
018200     EJECT                                                                
018300 01  FILLER                      PIC X(16)  VALUE 'FSG2-AREA'.            
018400       EXEC SQL INCLUDE FSG2     END-EXEC.                                
018500                                                                          
018600 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
018700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
018800                                                                          
018900 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
019000 01  DB2-WS.                                                              
019100     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
019200         88  CURSOR-OK                      VALUE 000.                    
019300         88  RADER-FINNS                    VALUE 000.                    
019400         88  RADER-SAKNAS                   VALUE 100.                    
019500         88  904-KOD                        VALUE 904.                    
019600     03  GODK-SQLCODEKODER.                                               
019700         05  GODK-SQLCODE OCCURS 4                                        
019800             INDEXED BY SQLCODE-IX PIC 9(3).                              
019900     EJECT                                                                
020000*                                                                         
020100 01 MEDDELANDEN.                                                          
020200* FEL-MEDDELANDEN                                                         
020300   03 W-FEL-1.                                                            
020400     05 FILLER  PIC X(40) VALUE 'EJ NUMERISKT ARTIKELNUMMER'.             
020500     05 FILLER  PIC X(40) VALUE 'NOT NUMERIC PART NO.'.                   
020600   03 FILLER REDEFINES W-FEL-1.                                           
020700     05 FEL-1   PIC X(40) OCCURS 2.                                       
020800                                                                          
020900   03 W-FEL-2.                                                            
021000     05 FILLER  PIC X(40) VALUE 'ARTIKEL SAKNAS I DATABAS'.               
021100     05 FILLER  PIC X(40) VALUE 'PART NO.  MISSING IN DATA BASE'.         
021200   03 FILLER REDEFINES W-FEL-2.                                           
021300     05 FEL-2   PIC X(40) OCCURS 2.                                       
021400                                                                          
021500   03 W-FEL-3.                                                            
021600     05 FILLER  PIC X(40) VALUE 'LEVERANTÖR SAKNAS'.                      
021700     05 FILLER  PIC X(40) VALUE 'SUPPLIER NO. MISSING '.                  
021800   03 FILLER REDEFINES W-FEL-3.                                           
021900     05 FEL-3   PIC X(40) OCCURS 2.                                       
022000                                                                          
022100   03 W-FEL-4.                                                            
022200     05 FILLER  PIC X(40) VALUE 'LEVERANTÖRADRESS SAKNAS     '.           
022300     05 FILLER  PIC X(40) VALUE 'SUPPLIER ADDRESS MISSING      '.         
022400   03 FILLER REDEFINES W-FEL-4.                                           
022500     05 FEL-4   PIC X(40) OCCURS 2.                                       
022600                                                                          
022700   03 W-FEL-13.                                                           
022800     05 FILLER  PIC X(40) VALUE 'ARTIKEL SAKNAR LAGERDATA'.               
022900     05 FILLER  PIC X(40) VALUE 'PART NO. HAS NO WAREHOUSE DATA'.         
023000   03 FILLER REDEFINES W-FEL-13.                                          
023100     05 FEL-13   PIC X(40) OCCURS 2.                                      
023200                                                                          
023300   03 W-MED-1.                                                            
023400     05 FILLER  PIC X(30) VALUE 'VAL:  T, P, D, S, N, E, R, L '.          
023500     05 FILLER  PIC X(30) VALUE 'TYPE: T,P,D,S,N,E,R OR L     '.          
023600   03 FILLER REDEFINES W-MED-1.                                           
023700     05 MED-1   PIC X(30) OCCURS 2.                                       
023800*                                                                         
023900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024100     SKIP3                                                                
024200 01  NYCKLAR-TILL-DLI.                                                    
024300     03  W-IDARTNR-X.                                                     
024400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
024500     03  W-KDSEGKEY-X.                                                    
024600         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
024700     03  W-IDLEVNR-X.                                                     
024800         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
024900     03  W-IDATTENT-X.                                                    
025000         05  W-IDATTENT          PIC S9(3)  VALUE ZERO  COMP-3.           
025100                                                                          
025200     03  W-KDARBTYP-X.                                                    
025300         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
025400     03  W-IDPERSON-X.                                                    
025500         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
025600                                                                          
025700     03  W-WDP3A1-MIN.                                                    
025800       05  W-IDLANDA1-MIN        PIC X(2)    VALUE SPACE.                 
025900       05  W-IDARTNRF-MIN        PIC S9(9)   VALUE ZERO COMP-3.           
026000       05  W-IDARTNRT-MIN        PIC S9(9)   VALUE ZERO COMP-3.           
026100       05  W-KDARBTYP-A-MIN      PIC X(8)    VALUE SPACE.                 
026200     03  W-WDP3A1-MAX.                                                    
026300       05  W-IDLANDA1-MAX        PIC X(2)    VALUE SPACE.                 
026400       05  W-IDARTNRF-MAX        PIC S9(9)   VALUE ZERO COMP-3.           
026500       05  W-IDARTNRT-MAX        PIC S9(9)   VALUE ZERO COMP-3.           
026600       05  W-KDARBTYP-A-MAX      PIC X(8)    VALUE SPACE.                 
026700                                                                          
026800     03  W-WDP3B1-X.                                                      
026900         05  W-IDLAND-B           PIC X(2)   VALUE SPACE.                 
027000         05  W-IDLEVNR-B          PIC X(5)   VALUE SPACE.                 
027100         05  W-KDARBTYP-B         PIC X(8)   VALUE SPACE.                 
027200                                                                          
027300     03  W-WDP3C1-MIN.                                                    
027400       05  W-IDLANDC1-MIN        PIC X(2)    VALUE SPACE.                 
027500       05  W-IDFKNGRPF-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
027600       05  W-IDFKNGRPT-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
027700       05  W-KDARBTYP-C-MIN      PIC X(8)    VALUE SPACE.                 
027800     03  W-WDP3C1-MAX.                                                    
027900       05  W-IDLANDC1-MAX        PIC X(2)    VALUE SPACE.                 
028000       05  W-IDFKNGRPF-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
028100       05  W-IDFKNGRPT-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
028200       05  W-KDARBTYP-C-MAX      PIC X(8)    VALUE SPACE.                 
028300                                                                          
028400     03  W-TIAAAA-X.                                                      
028500         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
028600                                                                          
028700     03 W-DABEHOV-MIN-X.                                                  
028800         05 W-DABEHOV-MIN        PIC  9(6)  VALUE ZERO.                   
028900     03 W-DABEHOV-MAX-X.                                                  
029000         05 W-DABEHOV-MAX        PIC  9(6)  VALUE ZERO.                   
029100                                                                          
029200     03  W-IDDC-B6-X.                                                     
029300         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
029400     SKIP2                                                                
029500                                                                          
029600     03  W-KVPB-PLAN         PIC S9(6)V9             COMP-3.              
029700     03  W-KVPB-SATS         PIC S9(6)V9             COMP-3.              
029800                                                                          
029900*    --- STATUS-KOD FRÅN IMS                                              
030000 01  STATUS-WS                   PIC XX.                                  
030100     88  SEGMENT-FINNS                       VALUE '  '.                  
030200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
030300     88  SEGMENT-SAKNAS                      VALUE 'GE' 'GB'.             
030400     SKIP2                                                                
030500 01  GODK-STATUSKODER.                                                    
030600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030700     SKIP3                                                                
030800 01  SSA1                        PIC X(96).                               
030900 01  SSA2                        PIC X(96).                               
031000 01  SSA3                        PIC X(96).                               
031100     EJECT                                                                
031200*    --- IMS FUNKTIONSKODER                                               
031300*01  -COPY W0003                                                          
031400     EJECT                                                                
031500*    ---  DLI INPUT-OUTPUT AREA                                           
031600                                                                          
031700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
031800 01  DLI-IO-WDK601.                                                       
031900*    03  -COPY WDK601                                                     
032000     EJECT                                                                
032100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
032200 01  DLI-IO-WDK611.                                                       
032300*    03  -COPY WDK611                                                     
032400     EJECT                                                                
032500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
032600 01  DLI-IO-WDF101.                                                       
032700*    03  -COPY WDF101                                                     
032800     EJECT                                                                
032900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF106'.                      
033000 01  DLI-IO-WDF106.                                                       
033100*    03  -COPY WDF106                                                     
033200     EJECT                                                                
033300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
033400 01  DLI-IO-WDP311.                                                       
033500*    03  -COPY WDP311                                                     
033600     EJECT                                                                
033700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP3A1'.                      
033800 01  DLI-IO-WDP3A1.                                                       
033900*    03  -COPY WDP3A1                                                     
034000     EJECT                                                                
034100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP3B1'.                      
034200 01  DLI-IO-WDP3B1.                                                       
034300*    03  -COPY WDP3B1                                                     
034400     EJECT                                                                
034500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP3C1'.                      
034600 01  DLI-IO-WDP3C1.                                                       
034700*    03  -COPY WDP3C1                                                     
034800     EJECT                                                                
034900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
035000 01  DLI-IO-WDK701.                                                       
035100*03  WDK701 -COPY WDK701   -PRE WDK7-                                     
035200     EJECT                                                                
035300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
035400 01  DLI-IO-WDK711.                                                       
035500*03  WDK711 -COPY WDK711   -PRE WDK7-                                     
035600     EJECT                                                                
035700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
035800 01  DLI-IO-WDK901.                                                       
035900*03  WDK901 -COPY WDK901   -PRE WDK9-                                     
036000     EJECT                                                                
036100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK911'.                      
036200 01  DLI-IO-WDK911.                                                       
036300*03  WDK911 -COPY WDK911   -PRE WDK9-                                     
036400     EJECT                                                                
036500 01  FILLER         PIC X(16)   VALUE 'DLI-IO-WDB601'.                    
036600 01  DLI-IO-WDB601.                                                       
036700* 03 WDB601  -COPY WDB601                                                 
036800     EJECT                                                                
036900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL811'.                      
037000 01  DLI-IO-WDL811.                                                       
037100*03  WDL811 -COPY WDL811                                                  
037200     EJECT                                                                
037300 LINKAGE SECTION.                                                         
037400*01  -COPY W0009   -PRE MSG-                                              
037500     05  FILLER                  PIC X.                                   
037600*01  -COPY W0008   -PRE USEA-                                             
037700     05  FILLER                  PIC X.                                   
037800*01  -COPY W0008  -PRE WDK6-                                              
037900     05  FILLER                  PIC X.                                   
038000*01  -COPY W0008  -PRE WDD9-                                              
038100     05  FILLER                  PIC X.                                   
038200*01  -COPY W0008  -PRE WDF1-                                              
038300     05  FILLER                  PIC X.                                   
038400*01  -COPY W0008  -PRE WDP3-                                              
038500     05  FILLER                  PIC X.                                   
038600*01  -COPY W0008  -PRE WDP3A-                                             
038700     05  FILLER                  PIC X.                                   
038800*01  -COPY W0008  -PRE WDP3B-                                             
038900     05  FILLER                  PIC X.                                   
039000*01  -COPY W0008  -PRE WDP3C-                                             
039100     05  FILLER                  PIC X.                                   
039200*01  -COPY W0008  -PRE WDK9-                                              
039300     05  FILLER                  PIC X.                                   
039400*01  -COPY W0008  -PRE WDL8-                                              
039500     05  FILLER                  PIC X.                                   
039600*01  -COPY W0008  -PRE WDK7-                                              
039700     05  FILLER                  PIC X.                                   
039800*01  -COPY W0008  -PRE WDB6-                                              
039900     05  FILLER                  PIC X.                                   
040000     EJECT                                                                
040100 01  PBTO-WDK6-PCB            PIC X.                                      
040200 01  PBTO-WDK7-PCB            PIC X.                                      
040300 01  PBTO-ARTM-PCB            PIC X.                                      
040400 01  PBTO-2501-PCB            PIC X.                                      
040500 01  PBTO-WDB6R-PCB           PIC X.                                      
040600 01  PBTO-WDK7R-PCB           PIC X.                                      
040700 01  PBTO-WDB6-PCB            PIC X.                                      
040800 01  PBTO-WDD7-PCB            PIC X.                                      
040900 01  PBTO-WDK7E-PCB           PIC X.                                      
041000 01  PBTO-W222-UTIL-WDK6-PCB       PIC X.                                 
041100 01  PBTO-W222-UTIL-WDK7-PCB       PIC X.                                 
041200 01  PBTO-W222-UTIL-WDB6-PCB       PIC X.                                 
041300 01  PBTO-W222-UTUP-WDK7-PCB       PIC X.                                 
041400 01  PBTO-W222-UTUP-WDB6-PCB       PIC X.                                 
041500 01  PBTO-W222-UTUP-UTIL-WDK6-PCB  PIC X.                                 
041600 01  PBTO-W222-UTUP-UTIL-WDK7-PCB  PIC X.                                 
041700 01  PBTO-W222-UTUP-UTIL-WDB6-PCB  PIC X.                                 
041800     EJECT                                                                
041900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK6-PCB WDD9-PCB             
042000     WDF1-PCB WDP3-PCB  WDP3A-PCB WDP3B-PCB WDP3C-PCB WDK9-PCB            
042100     WDL8-PCB WDK7-PCB  WDB6-PCB                                          
042200     PBTO-WDK6-PCB PBTO-WDK7-PCB  PBTO-ARTM-PCB                           
042300     PBTO-2501-PCB PBTO-WDB6R-PCB PBTO-WDK7R-PCB                          
042400     PBTO-WDB6-PCB PBTO-WDD7-PCB                                          
042500     PBTO-WDK7E-PCB                                                       
042600     PBTO-W222-UTIL-WDK6-PCB                                              
042700     PBTO-W222-UTIL-WDK7-PCB                                              
042800     PBTO-W222-UTIL-WDB6-PCB                                              
042900     PBTO-W222-UTUP-WDK7-PCB                                              
043000     PBTO-W222-UTUP-WDB6-PCB                                              
043100     PBTO-W222-UTUP-UTIL-WDK6-PCB                                         
043200     PBTO-W222-UTUP-UTIL-WDK7-PCB                                         
043300     PBTO-W222-UTUP-UTIL-WDB6-PCB                                         
043400     .                                                                    
043500 MAIN SECTION.                                                            
043600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK6-PCB WDD9-PCB             
043700     WDF1-PCB WDP3-PCB WDP3A-PCB WDP3B-PCB WDP3C-PCB WDK9-PCB             
043800     WDL8-PCB WDK7-PCB  WDB6-PCB                                          
043900     PBTO-WDK6-PCB PBTO-WDK7-PCB  PBTO-ARTM-PCB                           
044000     PBTO-2501-PCB PBTO-WDB6R-PCB PBTO-WDK7R-PCB                          
044100     PBTO-WDB6-PCB PBTO-WDD7-PCB                                          
044200     PBTO-WDK7E-PCB                                                       
044300     PBTO-W222-UTIL-WDK6-PCB                                              
044400     PBTO-W222-UTIL-WDK7-PCB                                              
044500     PBTO-W222-UTIL-WDB6-PCB                                              
044600     PBTO-W222-UTUP-WDK7-PCB                                              
044700     PBTO-W222-UTUP-WDB6-PCB                                              
044800     PBTO-W222-UTUP-UTIL-WDK6-PCB                                         
044900     PBTO-W222-UTUP-UTIL-WDK7-PCB                                         
045000     PBTO-W222-UTUP-UTIL-WDB6-PCB                                         
045100     .                                                                    
045200                                                                          
045300     PERFORM IMS-GET-MSG                                                  
045400     IF SEGMENT-FINNS                                                     
045500       PERFORM A-INIT                                                     
045600       PERFORM B-KOLLA-NYCKLAR                                            
045700       IF NYCKLAR-OK                                                      
045800         IF W-IDARTNR > ZERO                                              
045900           PERFORM C-LAES-VISA-ARTIKELINFO                                
046000           PERFORM F-LAES-VISA-LAGERSALDON                                
046100           PERFORM G-LAES-VISA-ORDERINGANG                                
046200           PERFORM H-LAES-VISA-ARTKELSTATISTIK                            
046300         END-IF                                                           
046400         IF W-IDLEVNR NOT = SPACE                                         
046500           PERFORM D-LAES-VISA-LEV-INFO                                   
046600         END-IF                                                           
046700         PERFORM E-LAES-VISA-NAMN-OCH-TEL                                 
046800       END-IF                                                             
046900       COMPUTE MSG-KVLL = LENGTH OF MOD-W90438O1 + 4                      
047000       PERFORM IMS-INSERT-MSG                                             
047100     END-IF                                                               
047200                                                                          
047300     MOVE ZERO TO RETURN-CODE                                             
047400     GOBACK                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 A-INIT SECTION.                                                          
047800                                                                          
047900     IF MSG-DUBBLA-TRANSKODER                                             
048000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90438I1                 
048100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
048200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
048300     ELSE                                                                 
048400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90438I1                  
048500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
048600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
048700     END-IF                                                               
048800                                                                          
048900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
049000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
049100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
049200                                                                          
049300     MOVE LOW-VALUE TO MSG-AREA                                           
049400     MOVE 'W90438O1' TO MFS-IDMOD                                         
049500     MOVE '9438' TO MOD-IDTRANS                                           
049600     MOVE SPACE  TO MOD-FELTEXT MOD-MEDDELANDE                            
049700                                                                          
049800     IF EGEN-MID                                                          
049900       CONTINUE                                                           
050000     ELSE                                                                 
050100       MOVE SPACE TO MFS-KDTRTYP                                          
050200       MOVE '7' TO MFS-IDPFK                                              
050300     END-IF                                                               
050400                                                                          
050500     MOVE LOW-VALUE  TO W-WDP3A1-MIN                                      
050600                        W-WDP3C1-MIN                                      
050700     MOVE HIGH-VALUE TO W-WDP3A1-MAX                                      
050800                        W-WDP3C1-MAX                                      
050900     MOVE 'SE'       TO W-IDLANDA1-MIN                                    
051000                        W-IDLANDA1-MAX                                    
051100                        W-IDLAND-B                                        
051200                        W-IDLANDC1-MIN                                    
051300                        W-IDLANDC1-MAX                                    
051400                                                                          
051500     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
051600     ACCEPT DAGENS-DATUM  FROM DATE                                       
051700     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
051800     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
051900                                                                          
052000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
052100                     DAT-O-TIDATUM DAT-KDSVAR                             
052200     IF DAT-KDSVAR-OK                                                     
052300****             HÄMTA SEKELSIFFROR                                       
052400       MOVE DAT-TISEKEL      TO DAGENS-AAR(1:2)                           
052500       MOVE DAT-TIAARP       TO DAGENS-PER                                
052600       MOVE DAGENS-PER(3:2)  TO WS-PER                                    
052700     ELSE                                                                 
052800       STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                  
052900       DELIMITED BY SIZE INTO FELTEXT                                     
053000       CALL FELLOG                                                        
053100     END-IF                                                               
053200     MOVE DAGENS-DATUM(1:2)   TO DAGENS-AAR(3:2)                          
053300     .                                                                    
053400     EJECT                                                                
053500                                                                          
053600 B-KOLLA-NYCKLAR SECTION.                                                 
053700     SKIP2                                                                
053800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
053900     MOVE '001'             TO MSGI-KDCALL                                
054000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
054100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
054200     MOVE '9438'            TO MSGI-IDTRANS                               
054300     IF GODK-MID                                                          
054400       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
054500       MOVE MID-IDLEVNR-IN  TO WS-IDLEVNR                                 
054600                               MSGI-IDLEVNR                               
054700     END-IF                                                               
054800                                                                          
054900     IF MID-IDLEVNR-IN = ALL '+' OR SPACE OR LOW-VALUES                   
055000       MOVE SPACE          TO  WS-IDLEVNR                                 
055100     ELSE                                                                 
055200       MOVE MID-IDLEVNR-IN TO WS-IDLEVNR                                  
055300     END-IF                                                               
055400                                                                          
055500     IF WS-IDLEVNR NOT = SPACE                                            
055600       MOVE WS-IDLEVNR TO W-IDLEVNR                                       
055700     END-IF                                                               
055800                                                                          
055900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
056000*    MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
056100                                                                          
056200*    - SPRÅK SOM SKA ANVÄNDAS                                             
056300     MOVE +1 TO SPIND                                                     
056400                                                                          
056500     MOVE JA TO NYCKLAR-SW                                                
056600                                                                          
056700*    -- KONTROLL AV IDARTNR                                               
056800     MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR-IN                              
056900                                                                          
057000     IF MID-IDARTNR-IN NOT = ALL '+' AND SPACE AND LOW-VALUES             
057100       MOVE '7'    TO MFS-IDPFK                                           
057200       MOVE SPACE  TO MFS-KDTRTYP                                         
057300     END-IF                                                               
057400                                                                          
057500     INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE      BY ZERO          
057600                                              LOW-VALUES BY ZERO          
057700                                                                          
057800     IF MID-IDARTNR-IN NUMERIC                                            
057900       MOVE MID-IDARTNR-IN TO W-IDARTNR                                   
058000     ELSE                                                                 
058100       MOVE ZERO           TO W-IDARTNR                                   
058200       MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                      
058300*       --- ALTERNATIV NYCKEL DÅ ?                                        
058400       IF W-IDLEVNR = SPACE                                               
058500         MOVE NEJ TO NYCKLAR-SW                                           
058600         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR                   
058700         MOVE FEL-1 (SPIND) TO MOD-FELTEXT                                
058800       END-IF                                                             
058900     END-IF                                                               
059000                                                                          
059100*    -- KONTROLL AV VAL                                                   
059200     MOVE MFS-RENSA-FAELT  TO MOD-VAL-IN                                  
059300                                                                          
059400     IF NYCKLAR-FEL                                                       
059500       MOVE SPACE TO WS-VAL                                               
059600     ELSE                                                                 
059700       IF MID-VAL-IN = ALL '+'                                            
059800                    OR LOW-VALUE                                          
059900         MOVE 'T' TO WS-VAL                                               
060000       ELSE                                                               
060100         MOVE MID-VAL-IN TO WS-VAL                                        
060200         MOVE '7'      TO MFS-IDPFK                                       
060300         MOVE SPACE    TO MFS-KDTRTYP                                     
060400       END-IF                                                             
060500     END-IF                                                               
060600                                                                          
060700     IF WS-VAL = SPACE                                                    
060800       CONTINUE                                                           
060900     ELSE                                                                 
061000       IF WS-VAL = 'T'                                                    
061100       OR WS-VAL = 'P'                                                    
061200       OR WS-VAL = 'D'                                                    
061300       OR WS-VAL = 'N'                                                    
061400       OR WS-VAL = 'E'                                                    
061500       OR WS-VAL = 'S'                                                    
061600       OR WS-VAL = 'L'                                                    
061700       OR WS-VAL = 'R'                                                    
061800         IF WS-VAL = 'T'                                                  
061900            MOVE 'TOTALT'    TO MOD-VAL-UT                                
062000         END-IF                                                           
062100         IF WS-VAL = 'P'                                                  
062200            MOVE 'PROGNOS'   TO MOD-VAL-UT                                
062300         END-IF                                                           
062400         IF WS-VAL = 'D'                                                  
062500            MOVE 'DIVERSE'   TO MOD-VAL-UT                                
062600         END-IF                                                           
062700         IF WS-VAL = 'N'                                                  
062800            MOVE 'NDC    '   TO MOD-VAL-UT                                
062900         END-IF                                                           
063000         IF WS-VAL = 'E'                                                  
063100            MOVE 'SDC    '   TO MOD-VAL-UT                                
063200         END-IF                                                           
063300         IF WS-VAL = 'S'                                                  
063400            MOVE 'SATS   '   TO MOD-VAL-UT                                
063500         END-IF                                                           
063600         IF WS-VAL = 'L'                                                  
063700            MOVE 'LEDTID '   TO MOD-VAL-UT                                
063800         END-IF                                                           
063900         IF WS-VAL = 'R'                                                  
064000            MOVE 'REFILL '   TO MOD-VAL-UT                                
064100         END-IF                                                           
064200       ELSE                                                               
064300         MOVE MED-1 (SPIND)  TO MOD-MEDDELANDE                            
064400         MOVE MFS-ALFA-FAELT-FEL TO MOD-VAL-IN-ATTR                       
064500         MOVE NEJ TO NYCKLAR-SW                                           
064600       END-IF                                                             
064700     END-IF                                                               
064800                                                                          
064900                                                                          
065000     IF GODK-MID OR NYCKLAR-OK                                            
065100       IF W-IDARTNR NOT = ZERO                                            
065200         MOVE MID-IDARTNR-IN   TO MOD-IDARTNR-UT                          
065300       END-IF                                                             
065400       IF W-IDLEVNR NOT = SPACE                                           
065500         MOVE MID-IDLEVNR-IN   TO MOD-IDLEVNR-UT                          
065600       END-IF                                                             
065700       MOVE NEJ TO ART-FINNS , CLAG-FINNS                                 
065800     ELSE                                                                 
065900       MOVE ZERO            TO MOD-IDARTNR-UT                             
066000       MOVE SPACE           TO MOD-IDLEVNR-UT                             
066100     END-IF                                                               
066200                                                                          
066300     IF NYCKLAR-FEL                                                       
066400*      MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
066500*      CALL WMEDKONV USING MED-WMEDAREA                                   
066600*      MOVE MED-MFSFEL TO MOD-FELTEXT                                     
066700       PERFORM MOD-RENSA-LEV-FAELT-IN                                     
066800       PERFORM MOD-RENSA-ART-FAELT-IN                                     
066900       PERFORM MOD-RENSA-LEV-FAELT-UT                                     
067000       PERFORM MOD-RENSA-ART-FAELT-UT                                     
067100     END-IF                                                               
067200     .                                                                    
067300     EJECT                                                                
067400                                                                          
067500 C-LAES-VISA-ARTIKELINFO SECTION.                                         
067600     SKIP2                                                                
067700     PERFORM CA-LAES-WDK6-DATA                                            
067800                                                                          
067900     IF CLAG-FINNS = JA                                                   
068000     OR ART-FINNS = JA                                                    
068100*      -- KOLLA INMATAT LEVNR                                             
068200       IF WS-IDLEVNR = SPACE                                              
068300         MOVE SPAR-IDLEVNR TO WS-IDLEVNR                                  
068400                              W-IDLEVNR                                   
068500                              MOD-IDLEVNR-UT                              
068600       END-IF                                                             
068700     END-IF                                                               
068800     .                                                                    
068900     EJECT                                                                
069000                                                                          
069100 CA-LAES-WDK6-DATA SECTION.                                               
069200     SKIP2                                                                
069300     PERFORM IMS-GET-WDK601                                               
069400     IF SEGMENT-FINNS                                                     
069500       MOVE  JA TO ART-FINNS                                              
069600       MOVE ART-IDLEVNR  TO SPAR-IDLEVNR                                  
069700       MOVE ART-IDFKNGRP TO SPAR-IDFKNGRP                                 
069800       PERFORM IMS-GNP-WDK611                                             
069900       IF SEGMENT-FINNS                                                   
070000         MOVE  JA TO CLAG-FINNS                                           
070100*        -- DELDATA ENLIGT 2101-BILDEN  (SPIE CR 2894)                    
070200         MOVE CLAG-KVVECKOR-LT   TO MOD-VECKORLT                          
070300         MOVE CLAG-KVVECKOR-FT   TO MOD-VECKORFT                          
070400         MOVE CLAG-KVDAGAR-INLEV TO MOD-DAGARINL                          
070500*        -- DELDATA ENLIGT 2102-BILDEN  (SPIE CR 2894)                    
070600         MOVE CLAG-KDAVT         TO MOD-KDAVT                             
070700         MOVE CLAG-ADLAGOMR      TO MOD-ADLAGOMR                          
070800         MOVE CLAG-ADGANG        TO MOD-ADGANG                            
070900         MOVE CLAG-ADPLATS       TO MOD-ADPLATS                           
071000*        -- SAMLA DATA FÖR E-VISA                                         
071100         MOVE CLAG-IDANSK        TO SPAR-IDANSK                           
071200         MOVE CLAG-IDBERED       TO SPAR-IDBERED                          
071300         IF CLAG-IDINK (1:3) NUMERIC                                      
071400           MOVE CLAG-IDINK (1:3) TO SPAR-IDINK                            
071500         ELSE                                                             
071600           IF CLAG-IDINK (2:3) NUMERIC                                    
071700             MOVE CLAG-IDINK (2:3) TO SPAR-IDINK                          
071800           ELSE                                                           
071900             IF CLAG-IDINK (1:2) NUMERIC                                  
072000               MOVE CLAG-IDINK (1:2) TO SPAR-IDINK                        
072100             ELSE                                                         
072200               IF CLAG-IDINK (1:1) NUMERIC                                
072300                 MOVE CLAG-IDINK (1:1) TO SPAR-IDINK                      
072400               ELSE                                                       
072500                 MOVE ZERO TO SPAR-IDINK                                  
072600               END-IF                                                     
072700             END-IF                                                       
072800           END-IF                                                         
072900         END-IF                                                           
073000*        -- DELDATA ENLIGT 2103-BILDEN  (SPIE CR 2894)                    
073100         PERFORM CAA-BEHANDLA-KVPB                                        
073200       ELSE                                                               
073300         MOVE NEJ TO CLAG-FINNS                                           
073400         MOVE FEL-13 (SPIND) TO MOD-MEDDELANDE                            
073500         END-IF                                                           
073600     ELSE                                                                 
073700       MOVE FEL-2 (SPIND) TO MOD-MEDDELANDE                               
073800     END-IF                                                               
073900     .                                                                    
074000     EJECT                                                                
074100 CAA-BEHANDLA-KVPB   SECTION.                                             
074200     SKIP2                                                                
074300     MOVE CLAG-KVPB-PLAN         TO W-KVPB-PLAN                           
074400     MOVE CLAG-KVPB-SATS         TO W-KVPB-SATS                           
074500     MOVE W-KVPB-PLAN     TO MOD-KVPB-PLAN                                
074600     MOVE W-KVPB-SATS     TO MOD-KVPB-SATS                                
074700                                                                          
074800     IF CLAG-DAPBPLAN > ZERO                                              
074900        IF CLAG-DAPBPLAN < WS-DAGENS-AAAAMMDD                             
075000           PERFORM CAAA-BER-NYTT-MASK-KVPB-PLAN                           
075100*       ELSE                                                              
075200*          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVPB-PLAN-ATTR               
075300        END-IF                                                            
075400     ELSE                                                                 
075500        PERFORM CAAA-BER-NYTT-MASK-KVPB-PLAN                              
075600     END-IF                                                               
075700     .                                                                    
075800     EJECT                                                                
075900                                                                          
076000 CAAA-BER-NYTT-MASK-KVPB-PLAN   SECTION.                                  
076100     SKIP2                                                                
076200     MOVE ART-IDARTNR TO PBTO-IDARTNR                                     
076300     CALL W222PBTO USING  PBTO-W222PBTO                                   
076400                          PBTO-WDK6-PCB                                   
076500                          PBTO-WDK7-PCB                                   
076600                          PBTO-ARTM-PCB                                   
076700                          PBTO-2501-PCB                                   
076800                          PBTO-WDB6R-PCB                                  
076900                          PBTO-WDK7R-PCB                                  
077000                          PBTO-WDB6-PCB                                   
077100                          PBTO-WDD7-PCB                                   
077200                          PBTO-WDK7E-PCB                                  
077300                          PBTO-W222-UTIL-WDK6-PCB                         
077400                          PBTO-W222-UTIL-WDK7-PCB                         
077500                          PBTO-W222-UTIL-WDB6-PCB                         
077600                          PBTO-W222-UTUP-WDK7-PCB                         
077700                          PBTO-W222-UTUP-WDB6-PCB                         
077800                          PBTO-W222-UTUP-UTIL-WDK6-PCB                    
077900                          PBTO-W222-UTUP-UTIL-WDK7-PCB                    
078000                          PBTO-W222-UTUP-UTIL-WDB6-PCB                    
078100                                                                          
078200     IF PBTO-KDSVAR = JA                                                  
078300       MOVE PBTO-KVPB-PLAN TO MOD-KVPB-PLAN                               
078400     END-IF                                                               
078500     .                                                                    
078600     EJECT                                                                
078700                                                                          
078800 D-LAES-VISA-LEV-INFO    SECTION.                                         
078900     SKIP2                                                                
079000                                                                          
079100     PERFORM IMS-GU-WDF101                                                
079200     IF SEGMENT-FINNS                                                     
079300       PERFORM IMS-GNP-WDF106-ADRESS                                      
079400       IF SEGMENT-FINNS                                                   
079500         MOVE ADR-BELEV-VCC TO MOD-BELEV-VCC                              
079600         MOVE ADR-ADLEVLND TO MOD-ADLEVLND                                
079700       ELSE                                                               
079800         MOVE FEL-4 (SPIND) TO MOD-MEDDELANDE                             
079900         PERFORM MOD-RENSA-LEV-FAELT-UT                                   
080000       END-IF                                                             
080100     ELSE                                                                 
080200       MOVE FEL-3 (SPIND) TO MOD-MEDDELANDE                               
080300       PERFORM MOD-RENSA-LEV-FAELT-UT                                     
080400     END-IF                                                               
080500     .                                                                    
080600     EJECT                                                                
080700                                                                          
080800 E-LAES-VISA-NAMN-OCH-TEL SECTION.                                        
080900     SKIP2                                                                
081000***  -- HÄMTA ANSKAFFAR-NAMN                                              
081100     IF SPAR-IDANSK > +0                                                  
081200       MOVE 'ANSK    '  TO W-KDARBTYP                                     
081300       MOVE SPAR-IDANSK TO W-IDPERSON                                     
081400       PERFORM IMS-GU-WDP311                                              
081500       IF SEGMENT-FINNS                                                   
081600         MOVE PERS-IDNAMN TO MOD-IDNAMN-ANSK                              
081700         MOVE PERS-IDTFN  TO MOD-IDTFN-ANSK                               
081800       ELSE                                                               
081900         MOVE SPACES TO MOD-IDNAMN-ANSK, MOD-IDTFN-ANSK                   
082000       END-IF                                                             
082100     ELSE                                                                 
082200       MOVE SPACES TO MOD-IDNAMN-ANSK, MOD-IDTFN-ANSK                     
082300     END-IF                                                               
082400                                                                          
082500*    -- HÄMTA BEREDAR-NAMN                                                
082600     IF SPAR-IDBERED > +0                                                 
082700       MOVE 'BER     '  TO W-KDARBTYP                                     
082800       MOVE SPAR-IDBERED  TO W-IDPERSON                                   
082900       PERFORM IMS-GU-WDP311                                              
083000       IF SEGMENT-FINNS                                                   
083100         MOVE PERS-IDNAMN TO MOD-IDNAMN-BEREDARE                          
083200         MOVE PERS-IDTFN  TO MOD-IDTFN-BEREDARE                           
083300       ELSE                                                               
083400         MOVE SPACES TO MOD-IDNAMN-BEREDARE, MOD-IDTFN-BEREDARE           
083500       END-IF                                                             
083600     ELSE                                                                 
083700       MOVE SPACES TO MOD-IDNAMN-BEREDARE, MOD-IDTFN-BEREDARE             
083800     END-IF                                                               
083900                                                                          
084000***  -- HÄMTA INKÖPAR-NAMN                                                
084100     IF SPAR-IDINK > +0                                                   
084200       MOVE 'INK     '  TO W-KDARBTYP                                     
084300       MOVE SPAR-IDINK  TO W-IDPERSON                                     
084400       PERFORM IMS-GU-WDP311                                              
084500       IF SEGMENT-FINNS                                                   
084600         MOVE PERS-IDNAMN TO MOD-IDNAMN-INK                               
084700         MOVE PERS-IDTFN  TO MOD-IDTFN-INK                                
084800       ELSE                                                               
084900         MOVE SPACES TO MOD-IDNAMN-INK, MOD-IDTFN-INK                     
085000       END-IF                                                             
085100     ELSE                                                                 
085200       MOVE SPACES TO MOD-IDNAMN-INK, MOD-IDTFN-INK                       
085300     END-IF                                                               
085400                                                                          
085500***  -- HÄMTA FÖRPACKNINGSTEKNIKER-NAMN                                   
085600     MOVE 'CDC' TO W-KDARBTYP, W-KDARBTYP-B                               
085700     IF W-IDARTNR > ZERO                                                  
085800        PERFORM EA-SOEK-IDARTNR                                           
085900     ELSE                                                                 
086000        PERFORM EB-SOEK-IDLEVNR                                           
086100     END-IF                                                               
086200     IF SW-TRAEFF = JA                                                    
086300       MOVE WS-IDPERSON TO W-IDPERSON                                     
086400       PERFORM IMS-GU-WDP311                                              
086500       IF SEGMENT-FINNS                                                   
086600         MOVE PERS-IDNAMN TO MOD-IDNAMN-FORP                              
086700         MOVE PERS-IDTFN TO MOD-IDTFN-FORP                                
086800       ELSE                                                               
086900         MOVE SPACES TO MOD-IDNAMN-FORP, MOD-IDTFN-FORP                   
087000       END-IF                                                             
087100     ELSE                                                                 
087200       MOVE SPACES TO MOD-IDNAMN-FORP, MOD-IDTFN-FORP                     
087300     END-IF                                                               
087400                                                                          
087500***  -- HÄMTA KVALITETSTEKNIKER-NAMN                                      
087600     MOVE 'QUAL' TO W-KDARBTYP,  W-KDARBTYP-B                             
087700     IF W-IDARTNR > ZERO                                                  
087800        PERFORM EA-SOEK-IDARTNR                                           
087900     ELSE                                                                 
088000        PERFORM EB-SOEK-IDLEVNR                                           
088100     END-IF                                                               
088200     IF SW-TRAEFF = JA                                                    
088300       MOVE WS-IDPERSON TO W-IDPERSON                                     
088400       PERFORM IMS-GU-WDP311                                              
088500       IF SEGMENT-FINNS                                                   
088600         MOVE PERS-IDNAMN TO MOD-IDNAMN-KVAL                              
088700         MOVE PERS-IDTFN TO MOD-IDTFN-KVAL                                
088800       ELSE                                                               
088900         MOVE SPACES TO MOD-IDNAMN-KVAL, MOD-IDTFN-KVAL                   
089000       END-IF                                                             
089100     ELSE                                                                 
089200       MOVE SPACES TO MOD-IDNAMN-KVAL, MOD-IDTFN-KVAL                     
089300     END-IF                                                               
089400     .                                                                    
089500     EJECT                                                                
089600                                                                          
089700 EA-SOEK-IDARTNR SECTION.                                                 
089800     SKIP2                                                                
089900     MOVE NEJ TO SW-TRAEFF                                                
090000                                                                          
090100     PERFORM IMS-GU-WDP3A                                                 
090200                                                                          
090300     PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = JA                       
090400       IF SEQA-IDARTNR-TOM < W-IDARTNR                                    
090500         PERFORM IMS-GN-WDP3A                                             
090600       ELSE                                                               
090700         IF SEQA-IDARTNR-FOM <= W-IDARTNR                                 
090800         AND SEQA-IDARTNR-TOM >= W-IDARTNR                                
090900           MOVE JA TO SW-TRAEFF                                           
091000         ELSE                                                             
091100           MOVE 'GE' TO STATUS-WS                                         
091200         END-IF                                                           
091300       END-IF                                                             
091400     END-PERFORM                                                          
091500                                                                          
091600     IF SW-TRAEFF = JA                                                    
091700       MOVE SEQA-IDPERSON TO WS-IDPERSON                                  
091800     ELSE                                                                 
091900       MOVE SPAR-IDLEVNR TO W-IDLEVNR-B                                   
092000       PERFORM IMS-GU-WDP3B                                               
092100       IF SEGMENT-FINNS                                                   
092200         MOVE JA TO SW-TRAEFF                                             
092300         MOVE SEQB-IDPERSON TO WS-IDPERSON                                
092400       ELSE                                                               
092500                                                                          
092600         PERFORM IMS-GU-WDP3C                                             
092700         PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = JA                   
092800           IF SEQC-IDFKNGRP-TOM < SPAR-IDFKNGRP                           
092900             PERFORM IMS-GN-WDP3C                                         
093000           ELSE                                                           
093100             IF SEQC-IDFKNGRP-FOM <= SPAR-IDFKNGRP                        
093200             AND SEQC-IDFKNGRP-TOM >= SPAR-IDFKNGRP                       
093300               MOVE JA TO SW-TRAEFF                                       
093400             ELSE                                                         
093500               MOVE 'GE' TO STATUS-WS                                     
093600             END-IF                                                       
093700           END-IF                                                         
093800         END-PERFORM                                                      
093900                                                                          
094000         IF SW-TRAEFF = JA                                                
094100            MOVE SEQC-IDPERSON TO WS-IDPERSON                             
094200         END-IF                                                           
094300       END-IF                                                             
094400     END-IF                                                               
094500     .                                                                    
094600     EJECT                                                                
094700 EB-SOEK-IDLEVNR SECTION.                                                 
094800     SKIP2                                                                
094900     MOVE NEJ TO SW-TRAEFF                                                
095000     MOVE WS-IDLEVNR TO W-IDLEVNR-B                                       
095100     PERFORM IMS-GU-WDP3B                                                 
095200     IF SEGMENT-FINNS                                                     
095300       MOVE JA TO SW-TRAEFF                                               
095400       MOVE SEQB-IDPERSON TO WS-IDPERSON                                  
095500     END-IF                                                               
095600     .                                                                    
095700     EJECT                                                                
095800                                                                          
095900 F-LAES-VISA-LAGERSALDON  SECTION.                                        
096000     SKIP2                                                                
096100     MOVE ZERO TO W-ARB-SALDO                                             
096200     IF CLAG-FINNS = JA                                                   
096300       PERFORM IMS-GU-WDK901                                              
096400       IF SEGMENT-FINNS                                                   
096500          MOVE WDK9-ART-SUTPO-TOT TO WS-SUTPO-TOT                         
096600          COMPUTE WS-KVOKS-TOT = WDK9-ART-KVOKS-BULK                      
096700                               + WDK9-ART-KVOKS-DAG                       
096800                               + WDK9-ART-KVOKS-VOR                       
096900       ELSE                                                               
097000          MOVE ZERO TO WS-SUTPO-TOT  WS-KVOKS-TOT                         
097100       END-IF                                                             
097200*                                                                         
097300       IF CLAG-TIDISPIN = ZERO                                            
097400          MOVE WS-SUTPO-TOT TO WS-TPO-NAESTA-INLEV                        
097500       ELSE                                                               
097600          IF SEGMENT-FINNS                                                
097700            MOVE ZERO TO W-DABEHOV-MIN                                    
097800            PERFORM S01-KONV-TIDISPIN                                     
097900            MOVE W-DADISPIN TO W-DABEHOV-MAX                              
098000            PERFORM IMS-GNP-WDK911                                        
098100            PERFORM UNTIL SEGMENT-SAKNAS                                  
098200              COMPUTE WS-TPO-NAESTA-INLEV = WS-TPO-NAESTA-INLEV           
098300                                          + WDK9-ANT-SUTPO-PB             
098400                                          + WDK9-ANT-SUTPO-EJPB           
098500              PERFORM IMS-GNP-WDK911                                      
098600            END-PERFORM                                                   
098700          END-IF                                                          
098800       END-IF                                                             
098900                                                                          
099000       COMPUTE W-KVAKS = CLAG-KVAKS-CDC                                   
099100                       + CLAG-KVAKS-PAV                                   
099200                       + CLAG-KVAKS-T                                     
099300                                                                          
099400       COMPUTE W-CDC-KVLS = CLAG-KVLS                                     
099500       MOVE  W-CDC-KVLS  TO MOD-KVLS (1)                                  
099600                                                                          
099700       COMPUTE W-DISPONIBELT = CLAG-KVLS - CLAG-KVRESS                    
099800***     *******  ARB-SALDO RÄKNAS ENDAST UT FÖR CDC                       
099900       COMPUTE W-ARB-SALDO = W-ARB-SALDO                                  
100000                           + W-DISPONIBELT                                
100100                           - CLAG-KVROS                                   
100200                           - WS-TPO-NAESTA-INLEV                          
100300                           - WS-KVOKS-TOT                                 
100400                           + W-KVAKS                                      
100500                                                                          
100600       MOVE W-DISPONIBELT  TO MOD-DISP  (1)                               
100700       MOVE CLAG-KVAKS-CDC TO MOD-KVAKS-LAGER (1)                         
100800       MOVE W-ARB-SALDO    TO MOD-ARB-SALDO (1)                           
100900       MOVE ZEROES         TO MOD-ARB-SALDO (2)                           
101000       MOVE ZEROES         TO MOD-ARB-SALDO (3)                           
101100                                                                          
101200       PERFORM FA-RED-FRAN-SDC-NDC                                        
101300***                                                                       
101400       MOVE W-SDC-KVLS      TO MOD-KVLS (2)                               
101500       COMPUTE W-DISPONIBELT = W-SDC-KVLS - W-SDC-KVRESS                  
101600*            --- OBS   SDC+LDC  HAR ALLTID NOLL I KVRESS                  
101700       MOVE W-DISPONIBELT   TO MOD-DISP (2)                               
101800                                                                          
101900       MOVE W-NDC-KVLS      TO MOD-KVLS (3)                               
102000       COMPUTE W-DISPONIBELT = W-NDC-KVLS - W-NDC-KVRESS                  
102100       MOVE W-DISPONIBELT   TO MOD-DISP (3)                               
102200                                                                          
102300       MOVE W-SDC-KVAKS-SDC TO MOD-KVAKS-LAGER (2)                        
102400       MOVE W-NDC-KVAKS-NDC TO MOD-KVAKS-LAGER (3)                        
102500                                                                          
102600*   EJ BESTÄLLDA FÄLT ÄR KOMMENTARMÄRKTA MED ***                          
102700*                 ORIGINALKOD I W2010200      ***                         
102800*                                                                         
102900***    MOVE W-OVERLAGER-SDC TO MOD-KVLS-SDC-OVER                          
103000***    MOVE W-SDC-KVPB      TO MOD-KVPB-SEP    (2)                        
103100***    MOVE W-NDC-KVPB      TO MOD-KVPB-SEP    (3)                        
103200***    MOVE W-SDC-KVOKS     TO WS-KVOKS-TOT    (2)                        
103300***                              MOD-KVOKS     (2)                        
103400***    MOVE W-NDC-KVOKS     TO WS-KVOKS-TOT    (3)                        
103500***                              MOD-KVOKS     (3)                        
103600***    MOVE W-SDC-KVAKS-PAV TO MOD-KVAKS-PAV   (2)                        
103700***    MOVE W-NDC-KVAKS-PAV TO MOD-KVAKS-PAV   (3)                        
103800                                                                          
103900***    MOVE W-SDC-KVEFRS    TO MOD-KVEFRS-SDC                             
104000***    MOVE W-NDC-KVEFRS    TO MOD-KVEFRS-NDC                             
104100***    MOVE +0              TO MOD-KVRESS      (2)                        
104200***    MOVE W-NDC-KVRESS    TO MOD-KVRESS      (3)                        
104300***    MOVE +0              TO MOD-KVROS       (2)                        
104400***    MOVE W-NDC-KVROS     TO MOD-KVROS       (3)                        
104500***    MOVE W-SDC-KVUTRS    TO MOD-KVUTRS      (2)                        
104600***    MOVE W-NDC-KVUTRS    TO MOD-KVUTRS      (3)                        
104700*                                                                         
104800***    IF W-TIINVDAT > ZERO                                               
104900***        MOVE W-TIINVDAT TO MOD-TIINVDAT                                
105000***    END-IF                                                             
105100*                                                                         
105200***    PERFORM IMS-GET-INLB01                                             
105300***    IF SEGMENT-FINNS                                                   
105400***        PERFORM IMS-GET-LEVERANTOER-SEG                                
105500***        MOVE +0 TO W-KVBR-TOT                                          
105600***                   W-KVBR-OVR                                          
105700***        PERFORM UNTIL SEGMENT-SAKNAS                                   
105800***             ADD LEVNR-KVBR TO W-KVBR-TOT                              
105900***             IF LEVNR-IDLEVNR NOT = W-HUVUDIDLEVNR                     
106000***                 ADD LEVNR-KVBR TO W-KVBR-OVR                          
106100***             END-IF                                                    
106200***             MOVE W-KVBR-OVR TO MOD-KVBR-OVR                           
106300***             MOVE W-KVBR-TOT TO MOD-KVBR-TOT                           
106400***             PERFORM IMS-GET-LEVERANTOER-SEG                           
106500***        END-PERFORM                                                    
106600***    END-IF                                                             
106700     END-IF                                                               
106800     .                                                                    
106900     EJECT                                                                
107000                                                                          
107100 FA-RED-FRAN-SDC-NDC SECTION.                                             
107200     SKIP2                                                                
107300*    EJ BESTÄLLDA FÄLT ÄR KOMMENTARMÄRKTA MED ***                         
107400*                 ORIGINALKOD I W2010200      ***                         
107500***  MOVE ZERO TO W-OVERLAGER-SDC                                         
107600     PERFORM IMS-GU-WDK701                                                
107700     IF SEGMENT-FINNS                                                     
107800        PERFORM IMS-GNP-WDK711                                            
107900        PERFORM UNTIL SEGMENT-SAKNAS                                      
108000          MOVE WDK7-SLAG-IDDC     TO W-IDDC-B6                            
108100          PERFORM IMS-GU-WDB601                                           
108300          IF DCS-SDC                                                      
108400            ADD WDK7-SLAG-KVLS       TO   W-SDC-KVLS                      
108500            ADD WDK7-SLAG-KVAKS-SDC  TO   W-SDC-KVAKS-SDC                 
108600                                                                          
108700***         ADD WDK7-SLAG-KVAKS-PAV  TO   W-SDC-KVAKS-PAV                 
108800***         ADD WDK7-SLAG-KVPB-REF   TO   W-SDC-KVPB                      
108900***         ADD WDK7-SLAG-KVOKS-BULK TO   W-SDC-KVOKS                     
109000***         ADD WDK7-SLAG-KVOKS-DAG  TO   W-SDC-KVOKS                     
109100***                                                                       
109200***         ADD WDK7-SLAG-KVEFRS     TO   W-SDC-KVEFRS                    
109300***         ADD WDK7-SLAG-KVUTRS     TO   W-SDC-KVUTRS                    
109400*                                                                         
109500***         MOVE ZERO                TO   W-TILLG-SDC                     
109600***         ADD WDK7-SLAG-KVLS       TO   W-TILLG-SDC                     
109700***         ADD WDK7-SLAG-KVBEART    TO   W-TILLG-SDC                     
109800***         ADD WDK7-SLAG-KVAKS-SDC  TO   W-TILLG-SDC                     
109900***         ADD WDK7-SLAG-KVAKS-PAV  TO   W-TILLG-SDC                     
110000***         SUBTRACT WDK7-SLAG-KVOKS-BULK FROM W-TILLG-SDC                
110100***         SUBTRACT WDK7-SLAG-KVOKS-DAG FROM W-TILLG-SDC                 
110200***         IF WDK7-SLAG-KVREFOVL <    W-TILLG-SDC                        
110300***            COMPUTE W-OVERLAGER-SDC = W-OVERLAGER-SDC                  
110400***                                    + W-TILLG-SDC                      
110500***                                    - WDK7-SLAG-KVREFOVL               
110600***         END-IF                                                        
110700***         MOVE ART-TIFINLV   TO TIFINLV-AAVVD                           
110800***         MOVE DAGENS-AA     TO TMP1-YY                                 
110900***         MOVE TIFINLV-AA    TO TMP2-YY                                 
111000***         PERFORM WY2000P9                                              
111100***         COMPUTE VECKO-SKILLNAD = (TMP1-YY - TMP2-YY) * 52             
111200***                                + DAGENS-VV - TIFINLV-VV               
111300***         IF VECKO-SKILLNAD < 52                                        
111400***            MOVE ZERO TO W-OVERLAGER-SDC                               
111500***         END-IF                                                        
111600          ELSE                                                            
112110            IF DCS-NDC                                                    
112200              ADD WDK7-SLAG-KVLS     TO W-NDC-KVLS                        
112300              ADD WDK7-SLAG-KVRESS   TO W-NDC-KVRESS                      
112400              ADD WDK7-SLAG-KVAKS-SDC TO W-NDC-KVAKS-NDC                  
112500                                                                          
112600***           ADD WDK7-SLAG-KVAKS-PAV TO W-NDC-KVAKS-PAV                  
112700***           IF WDK7-SLAG-IDLEVNR = '1441'                               
112800***           OR WDK7-SLAG-IDLEVNR = 'BP2TW'                              
112900***             ADD WDK7-SLAG-KVPB-REF TO W-NDC-KVPB                      
113000***           END-IF                                                      
113100***           ADD WDK7-SLAG-KVOKS-BULK TO W-NDC-KVOKS                     
113200***           ADD WDK7-SLAG-KVOKS-DAG TO W-NDC-KVOKS                      
113300***           ADD WDK7-SLAG-KVROS-BULK TO W-NDC-KVROS                     
113400***           ADD WDK7-SLAG-KVROS-DAG TO W-NDC-KVROS                      
113500***           ADD WDK7-SLAG-KVEFRS   TO W-NDC-KVEFRS                      
113600***           ADD WDK7-SLAG-KVUTRS   TO W-NDC-KVUTRS                      
113700            END-IF                                                        
113800          END-IF                                                          
113900          PERFORM IMS-GNP-WDK711                                          
114000        END-PERFORM                                                       
114100***     COMPUTE W-SDC-KVPB ROUNDED =  W-SDC-KVPB                          
114200***     COMPUTE W-NDC-KVPB ROUNDED =  W-NDC-KVPB                          
114300     END-IF                                                               
114400     .                                                                    
114500     EJECT                                                                
114600 G-LAES-VISA-ORDERINGANG     SECTION.                                     
114700     SKIP2                                                                
114800     PERFORM GA-HAMTA-VV-I-PER                                            
114900     MOVE DAGENS-AAR         TO MOD-AARTAL (6)                            
115000                                WS-AARTAL                                 
115100     SUBTRACT 1 FROM WS-AARTAL  MOVE WS-AARTAL TO MOD-AARTAL (5)          
115200     SUBTRACT 1 FROM WS-AARTAL  MOVE WS-AARTAL TO MOD-AARTAL (4)          
115300     SUBTRACT 1 FROM WS-AARTAL  MOVE WS-AARTAL TO MOD-AARTAL (3)          
115400     SUBTRACT 1 FROM WS-AARTAL  MOVE WS-AARTAL TO MOD-AARTAL (2)          
115500     SUBTRACT 1 FROM WS-AARTAL  MOVE WS-AARTAL TO MOD-AARTAL (1)          
115600                                                                          
115700     PERFORM GB-HAEMTA-HISTORIK                                           
115800                                                                          
115900     MOVE +1  TO IX-AR                                                    
116000***              IX-PER                                                   
116100     PERFORM UNTIL IX-AR > +6                                             
116200***                       PERFORM UNTIL IX-PER > +12                      
116300***                         MOVE WS-KVOI-TAB (IX-AR, IX-PER)              
116400***                              TO MOD-KVOI (IX-AR, IX-PER)              
116500***                           ADD +1  TO IX-PER                           
116600***                       END-PERFORM                                     
116700       MOVE WS-KVOI-TOT-TAB (IX-AR) TO MOD-KVOI-TOT (IX-AR)               
116800       ADD +1  TO IX-AR                                                   
116900***                       MOVE +1 TO IX-PER                               
117000     END-PERFORM                                                          
117100     .                                                                    
117200     EJECT                                                                
117300 GA-HAMTA-VV-I-PER SECTION.                                               
117400     SKIP2                                                                
117500*    --- FYLL I VECKONR FÖR PERIODERNA                                    
117600     MOVE DAGENS-DATUM(1:2)  TO WS-AAPP(1:2)                              
117700     MOVE 01                 TO WS-AAPP(3:2)                              
117800     MOVE WS-AAPP            TO DAT-I-TIDATUM                             
117900     MOVE 'AARP'             TO DAT-KDDATFORM                             
118000     MOVE 1                  TO WS-PP                                     
118100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
118200                     DAT-O-TIDATUM DAT-KDSVAR                             
118300     IF DAT-KDSVAR-OK                                                     
118400       MOVE DAT-KVVIPER      TO WS-KVVIPER(WS-PP)                         
118500       MOVE 1                TO WS-FORSTA-V(WS-PP)                        
118600     ELSE                                                                 
118700         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
118800         DELIMITED BY SIZE INTO FELTEXT                                   
118900         CALL FELLOG                                                      
119000     END-IF                                                               
119100                                                                          
119200     PERFORM UNTIL WS-PP > 12                                             
119300       ADD 1  TO WS-PP                                                    
119400       IF WS-PP = 13                                                      
119500         MOVE 53 TO WS-SISTA-V(12)                                        
119600       ELSE                                                               
119700         MOVE WS-AAPP  TO DAT-I-TIDATUM                                   
119800         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
119900                             DAT-O-TIDATUM DAT-KDSVAR                     
120000         IF DAT-KDSVAR-OK                                                 
120100             COMPUTE WS-SISTA-V(WS-PP - 1) = DAT-TIVV - 1                 
120200             IF WS-PP > 1                                                 
120300               MOVE DAT-TIVV  TO WS-FORSTA-V(WS-PP)                       
120400               MOVE DAT-KVVIPER TO WS-KVVIPER(WS-PP)                      
120500             END-IF                                                       
120600         ELSE                                                             
120700           MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                   
120800           CALL FELLOG                                                    
120900         END-IF                                                           
121000       END-IF                                                             
121100     END-PERFORM                                                          
121200     .                                                                    
121300     EJECT                                                                
121400                                                                          
121500 GB-HAEMTA-HISTORIK SECTION.                                              
121600     SKIP2                                                                
121700     MOVE WS-DAGENS-AAAAMMDD (1:4) TO W-TIAAAA                            
121800     PERFORM IMS-GU-WDL811                                                
121900     IF SEGMENT-FINNS                                                     
122000       MOVE +6 TO IX-AR                                                   
122100       MOVE +1 TO IX-PER                                                  
122200       MOVE ZERO TO WS-TOTAL-KVOI                                         
122300       PERFORM UNTIL IX-PER = WS-PER                                      
122400         MOVE ZERO TO WS-KVOI (IX-PER)                                    
122500         MOVE WS-FORSTA-V(IX-PER)  TO WS-VV                               
122600         PERFORM UNTIL WS-VV  >  WS-SISTA-V(IX-PER)                       
122700           EVALUATE WS-VAL                                                
122800            WHEN 'T' ADD AAR-KVOI-PROG(WS-VV) TO WS-KVOI(IX-PER)          
122900                     ADD AAR-KVOI-NDC(WS-VV) TO WS-KVOI(IX-PER)           
123000                     ADD AAR-KVOI-SDC(WS-VV) TO WS-KVOI(IX-PER)           
123100                     ADD AAR-KVOI-SATS(WS-VV) TO WS-KVOI(IX-PER)          
123200                     ADD AAR-KVOI-DIV(WS-VV) TO WS-KVOI(IX-PER)           
123300            WHEN 'P' ADD AAR-KVOI-PROG(WS-VV) TO WS-KVOI(IX-PER)          
123400            WHEN 'D' ADD AAR-KVOI-DIV(WS-VV)  TO WS-KVOI(IX-PER)          
123500            WHEN 'N' ADD AAR-KVOI-NDC(WS-VV)  TO WS-KVOI(IX-PER)          
123600            WHEN 'E' ADD AAR-KVOI-SDC(WS-VV)  TO WS-KVOI(IX-PER)          
123700            WHEN 'R' ADD AAR-KVOI-REFILL(WS-VV) TO WS-KVOI(IX-PER)        
123800            WHEN 'S' ADD AAR-KVOI-SATS(WS-VV)  TO WS-KVOI(IX-PER)         
123900            WHEN 'L' ADD AAR-KVOI-LEDTID(WS-VV) TO WS-KVOI(IX-PER)        
124000           END-EVALUATE                                                   
124100           ADD +1  TO WS-VV                                               
124200         END-PERFORM                                                      
124300         COMPUTE WS-KVOI-RED ROUNDED =                                    
124400                 WS-KVOI (IX-PER) / WS-KVVIPER (IX-PER) * +4.33           
124500         ADD WS-KVOI-RED       TO WS-KVOI-TAB (IX-AR, IX-PER)             
124600                                  WS-TOTAL-KVOI                           
124700         ADD +1 TO IX-PER                                                 
124800       END-PERFORM                                                        
124900       ADD WS-TOTAL-KVOI       TO WS-KVOI-TOT-TAB (IX-AR)                 
125000     END-IF                                                               
125100                                                                          
125200     MOVE +5 TO IX-AR                                                     
125300     PERFORM UNTIL IX-AR < +1                                             
125400***    LÄGG UT HISTORIK FÖR FÖREGÅENDE ÅR OCH TIDIGARE                    
125500       SUBTRACT 1              FROM W-TIAAAA                              
125600       PERFORM IMS-GU-WDL811                                              
125700       IF SEGMENT-FINNS                                                   
125800         MOVE 1                TO IX-PER                                  
125900         MOVE ZERO             TO WS-TOTAL-KVOI                           
126000         PERFORM UNTIL IX-PER > 12                                        
126100           MOVE ZERO           TO WS-KVOI (IX-PER)                        
126200           MOVE WS-FORSTA-V (IX-PER)  TO WS-VV                            
126300           PERFORM UNTIL WS-VV > WS-SISTA-V (IX-PER)                      
126400            EVALUATE WS-VAL                                               
126500             WHEN 'T' ADD AAR-KVOI-PROG(WS-VV) TO WS-KVOI(IX-PER)         
126600                      ADD AAR-KVOI-NDC(WS-VV)  TO WS-KVOI(IX-PER)         
126700                      ADD AAR-KVOI-SDC(WS-VV)  TO WS-KVOI(IX-PER)         
126800                      ADD AAR-KVOI-SATS(WS-VV) TO WS-KVOI(IX-PER)         
126900                      ADD AAR-KVOI-DIV(WS-VV) TO WS-KVOI(IX-PER)          
127000             WHEN 'P' ADD AAR-KVOI-PROG(WS-VV) TO WS-KVOI(IX-PER)         
127100             WHEN 'D' ADD AAR-KVOI-DIV(WS-VV) TO WS-KVOI(IX-PER)          
127200             WHEN 'N' ADD AAR-KVOI-NDC(WS-VV) TO WS-KVOI(IX-PER)          
127300             WHEN 'E' ADD AAR-KVOI-SDC(WS-VV) TO WS-KVOI(IX-PER)          
127400             WHEN 'R' ADD AAR-KVOI-REFILL(WS-VV)                          
127500                                              TO WS-KVOI(IX-PER)          
127600             WHEN 'S' ADD AAR-KVOI-SATS(WS-VV) TO WS-KVOI(IX-PER)         
127700             WHEN 'L' ADD AAR-KVOI-LEDTID(WS-VV)                          
127800                                               TO WS-KVOI(IX-PER)         
127900             END-EVALUATE                                                 
128000             ADD 1 TO WS-VV                                               
128100           END-PERFORM                                                    
128200           COMPUTE WS-KVOI-RED ROUNDED =                                  
128300                WS-KVOI (IX-PER) * +4.33 / WS-KVVIPER (IX-PER)            
128400           ADD WS-KVOI-RED  TO WS-KVOI-TAB (IX-AR, IX-PER)                
128500                               WS-TOTAL-KVOI                              
128600           ADD +1 TO IX-PER                                               
128700         END-PERFORM                                                      
128800         ADD WS-TOTAL-KVOI  TO WS-KVOI-TOT-TAB (IX-AR)                    
128900       END-IF                                                             
129000       SUBTRACT +1  FROM IX-AR                                            
129100     END-PERFORM                                                          
129200     .                                                                    
129300     EJECT                                                                
129400                                                                          
129500 H-LAES-VISA-ARTKELSTATISTIK   SECTION.                                   
129600     SKIP2                                                                
129700     PERFORM  DB2-SELECT-FSG2-TAB                                         
129800     IF RADER-FINNS                                                       
129900        PERFORM HA-LAGG-UT-DB2-ARTINFO                                    
130000     ELSE                                                                 
130100        PERFORM S00-NOLLA-MOD-FAELT                                       
130200     END-IF                                                               
130300     .                                                                    
130400     EJECT                                                                
130500                                                                          
130600 HA-LAGG-UT-DB2-ARTINFO     SECTION.                                      
130700     SKIP2                                                                
130800     PERFORM S00-NOLLA-MOD-FAELT                                          
130900     MOVE FSG-SULEVANT-PER            TO MOD-SULEVANT-PER                 
131000     MOVE FSG-SULEVANT-AAR            TO MOD-SULEVANT-AAR                 
131100     MOVE FSG-SULEVANT-FAAR           TO MOD-SULEVANT-FAAR                
131200     MOVE FSG-SULEVANT-RAAR           TO MOD-SULEVANT-RAAR                
131300     MOVE FSG-SULEVANT-FRAAR          TO MOD-SULEVANT-FRAAR               
131400     .                                                                    
131500     EJECT                                                                
131600                                                                          
131700 S00-NOLLA-MOD-FAELT         SECTION.                                     
131800     SKIP2                                                                
131900      MOVE ZERO    TO MOD-SULEVANT-PER                                    
132000      MOVE ZERO    TO MOD-SULEVANT-AAR                                    
132100      MOVE ZERO    TO MOD-SULEVANT-FAAR                                   
132200      MOVE ZERO    TO MOD-SULEVANT-RAAR                                   
132300      MOVE ZERO    TO MOD-SULEVANT-FRAAR                                  
132400     .                                                                    
132500     EJECT                                                                
132600                                                                          
132700 S01-KONV-TIDISPIN SECTION.                                               
132800     SKIP2                                                                
132900     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
133000     MOVE CLAG-TIDISPIN TO DAT-I-TIDATUM                                  
133100     CALL WDATKONV USING DAT-KDDATFORM                                    
133200                         DAT-I-TIDATUM                                    
133300                         DAT-O-TIDATUM                                    
133400                         DAT-KDSVAR                                       
133500     IF DAT-KDSVAR-OK                                                     
133600       MOVE DAT-TIAA-VECKA TO W-TIAA                                      
133700       MOVE DAT-TIVV       TO W-TIVV                                      
133800       MOVE DAT-TISEKEL    TO W-TISEKEL                                   
133900       MOVE W-TIAAAAVV     TO W-DADISPIN                                  
134000     ELSE                                                                 
134100       MOVE ZERO           TO W-DADISPIN                                  
134200     END-IF                                                               
134300     .                                                                    
134400     EJECT                                                                
134500                                                                          
134600 MOD-RENSA-LEV-FAELT-UT SECTION.                                          
134700     SKIP2                                                                
134800*    --- ALLA LEV-UTDATA-FÄLT                                             
134900     MOVE SPACES         TO MOD-BELEV-VCC                                 
135000     MOVE SPACES         TO MOD-ADLEVLND                                  
135100     .                                                                    
135200     SKIP3                                                                
135300 MOD-RENSA-ART-FAELT-UT SECTION.                                          
135400     SKIP2                                                                
135500*    --- ALLA LEV-UTDATA-FÄLT                                             
135600     MOVE ZEROES         TO MOD-VECKORLT                                  
135700     MOVE ZEROES         TO MOD-VECKORFT                                  
135800     MOVE ZEROES         TO MOD-DAGARINL                                  
135900     MOVE ZEROES         TO MOD-KVPB-PLAN                                 
136000     MOVE ZEROES         TO MOD-KVPB-SATS                                 
136100     .                                                                    
136200     SKIP3                                                                
136300 MOD-RENSA-WDP3-FAELT-UT  SECTION.                                        
136400     SKIP2                                                                
136500*    --- ALLA WDP3-UTDATA-FÄLT                                            
136600     MOVE SPACES         TO MOD-IDNAMN-ANSK                               
136700     MOVE SPACES         TO MOD-IDNAMN-BEREDARE                           
136800     MOVE SPACES         TO MOD-IDNAMN-INK                                
136900     MOVE SPACES         TO MOD-IDNAMN-FORP                               
137000     MOVE SPACES         TO MOD-IDNAMN-KVAL                               
137100     MOVE SPACES         TO MOD-IDTFN-ANSK                                
137200     MOVE SPACES         TO MOD-IDTFN-BEREDARE                            
137300     MOVE SPACES         TO MOD-IDTFN-INK                                 
137400     MOVE SPACES         TO MOD-IDTFN-FORP                                
137500     MOVE SPACES         TO MOD-IDTFN-KVAL                                
137600     .                                                                    
137700     SKIP3                                                                
137800 MOD-RENSA-LEV-FAELT-IN SECTION.                                          
137900                                                                          
138000*    --- ALLA INDATA-FÄLT                                                 
138100     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
138200     .                                                                    
138300     EJECT                                                                
138400 MOD-RENSA-ART-FAELT-IN SECTION.                                          
138500                                                                          
138600*    --- ALLA INDATA-FÄLT                                                 
138700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
138800     .                                                                    
138900     EJECT                                                                
139000* --- IMS SEKTIONER ---                                                   
139100     SKIP3                                                                
139200 IMS-GET-MSG SECTION.                                                     
139300                                                                          
139400     MOVE '  QC' TO GODK-STATUSKODER                                      
139500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
139600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
139700     PERFORM IMS-STATUSKONTROLL                                           
139800     .                                                                    
139900     SKIP3                                                                
140000 IMS-INSERT-MSG SECTION.                                                  
140100                                                                          
140200*    IF MSGI-IDLAND-SPR = 'SE'                                            
140300*      MOVE '0' TO MFS-KDHUVOMR                                           
140400*    END-IF                                                               
140500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
140600     MOVE SPACE TO GODK-STATUSKODER                                       
140700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
140800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
140900     PERFORM IMS-STATUSKONTROLL                                           
141000     .                                                                    
141100     EJECT                                                                
141200 IMS-GET-WDK601 SECTION.                                                  
141300                                                                          
141400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
141500          DELIMITED BY SIZE INTO SSA1                                     
141600     MOVE '  GE' TO GODK-STATUSKODER                                      
141700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
141800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
141900     PERFORM IMS-STATUSKONTROLL                                           
142000     .                                                                    
142100     EJECT                                                                
142200 IMS-GNP-WDK611 SECTION.                                                  
142300                                                                          
142400     MOVE 'WDK611   '         TO SSA1                                     
142500     MOVE '  GE' TO GODK-STATUSKODER                                      
142600     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
142700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
142800     PERFORM IMS-STATUSKONTROLL                                           
142900     .                                                                    
143000     EJECT                                                                
143100*                                                                         
143200 IMS-GU-WDK701 SECTION.                                                   
143300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
143400            DELIMITED BY SIZE INTO SSA1                                   
143500     MOVE '  GE' TO GODK-STATUSKODER                                      
143600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
143700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
143800     PERFORM IMS-STATUSKONTROLL                                           
143900     .                                                                    
144000     SKIP3                                                                
144100                                                                          
144200 IMS-GNP-WDK711 SECTION.                                                  
144300     MOVE 'WDK711  ' TO SSA1                                              
144400     MOVE '  GE' TO GODK-STATUSKODER                                      
144500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
144600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
144700     PERFORM IMS-STATUSKONTROLL                                           
144800     .                                                                    
144900     EJECT                                                                
145000                                                                          
145100 IMS-GU-WDK901          SECTION.                                          
145200     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
145300            DELIMITED BY SIZE INTO SSA1                                   
145400     MOVE '  GE' TO GODK-STATUSKODER                                      
145500     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
145600     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
145700     PERFORM IMS-STATUSKONTROLL                                           
145800     .                                                                    
145900     SKIP3                                                                
146000 IMS-GNP-WDK911         SECTION.                                          
146100     STRING 'WDK911  (DABEHOV >=' W-DABEHOV-MIN-X                         
146200                    '&DABEHOV <=' W-DABEHOV-MAX-X ')'                     
146300            DELIMITED BY SIZE INTO SSA1                                   
146400     MOVE '  GE' TO GODK-STATUSKODER                                      
146500     CALL CBLTDLI USING GNP WDK9-PCB DLI-IO-WDK911 SSA1                   
146600     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
146700     PERFORM IMS-STATUSKONTROLL                                           
146800     .                                                                    
146900     EJECT                                                                
147000                                                                          
147100 IMS-GU-WDF101 SECTION.                                                   
147200     SKIP2                                                                
147300     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
147400          DELIMITED BY SIZE INTO SSA1                                     
147500     MOVE '  GE' TO GODK-STATUSKODER                                      
147600     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
147700     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
147800     PERFORM IMS-STATUSKONTROLL                                           
147900     .                                                                    
148000     EJECT                                                                
148100 IMS-GNP-WDF106-ADRESS SECTION.                                           
148200     SKIP2                                                                
148300     MOVE 'WDF106   ' TO SSA1                                             
148400     MOVE '  GE' TO GODK-STATUSKODER                                      
148500     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF106 SSA1                   
148600     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
148700     PERFORM IMS-STATUSKONTROLL                                           
148800     .                                                                    
148900     EJECT                                                                
149000 IMS-GU-WDP311      SECTION.                                              
149100     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
149200          DELIMITED BY SIZE INTO SSA1                                     
149300     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
149400          DELIMITED BY SIZE INTO SSA2                                     
149500     MOVE '  GE' TO GODK-STATUSKODER                                      
149600     CALL CBLTDLI USING GU  WDP3-PCB                                      
149700                            DLI-IO-WDP311                                 
149800                            SSA1 SSA2                                     
149900     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
150000     PERFORM IMS-STATUSKONTROLL                                           
150100     .                                                                    
150200     SKIP2                                                                
150300*                                                                         
150400 IMS-GU-WDP3A SECTION.                                                    
150500     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
150600                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
150700                    '&KDARBTYP =' W-KDARBTYP-X ')'                        
150800            DELIMITED BY SIZE INTO SSA1                                   
150900     MOVE '  GE' TO GODK-STATUSKODER                                      
151000     CALL CBLTDLI USING GU WDP3A-PCB DLI-IO-WDP3A1 SSA1                   
151100     MOVE WDP3A-STATUS-CODE TO STATUS-WS                                  
151200     PERFORM IMS-STATUSKONTROLL                                           
151300     .                                                                    
151400     SKIP2                                                                
151500 IMS-GN-WDP3A SECTION.                                                    
151600     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
151700                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
151800                    '&KDARBTYP =' W-KDARBTYP ')'                          
151900            DELIMITED BY SIZE INTO SSA1                                   
152000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
152100     CALL CBLTDLI USING GN WDP3A-PCB DLI-IO-WDP3A1 SSA1                   
152200     MOVE WDP3A-STATUS-CODE TO STATUS-WS                                  
152300     PERFORM IMS-STATUSKONTROLL                                           
152400     .                                                                    
152500     SKIP2                                                                
152600*                                                                         
152700 IMS-GU-WDP3B SECTION.                                                    
152800     STRING 'WDP3B1  (WDP3B1KY =' W-WDP3B1-X                              
152900                    '&KDARBTYP =' W-KDARBTYP-X ')'                        
153000            DELIMITED BY SIZE INTO SSA1                                   
153100     MOVE '  GE' TO GODK-STATUSKODER                                      
153200     CALL CBLTDLI USING GU WDP3B-PCB DLI-IO-WDP3B1 SSA1                   
153300     MOVE WDP3B-STATUS-CODE TO STATUS-WS                                  
153400     PERFORM IMS-STATUSKONTROLL                                           
153500     .                                                                    
153600     SKIP2                                                                
153700*                                                                         
153800 IMS-GU-WDP3C SECTION.                                                    
153900     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
154000                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
154100                    '&KDARBTYP =' W-KDARBTYP-X ')'                        
154200            DELIMITED BY SIZE INTO SSA1                                   
154300     MOVE '  GE' TO GODK-STATUSKODER                                      
154400     CALL CBLTDLI USING GU WDP3C-PCB DLI-IO-WDP3C1 SSA1                   
154500     MOVE WDP3C-STATUS-CODE TO STATUS-WS                                  
154600     PERFORM IMS-STATUSKONTROLL                                           
154700     .                                                                    
154800     SKIP2                                                                
154900*                                                                         
155000 IMS-GN-WDP3C SECTION.                                                    
155100     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
155200                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
155300                    '&KDARBTYP =' W-KDARBTYP-X ')'                        
155400            DELIMITED BY SIZE INTO SSA1                                   
155500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
155600     CALL CBLTDLI USING GN WDP3C-PCB DLI-IO-WDP3C1 SSA1                   
155700     MOVE WDP3C-STATUS-CODE TO STATUS-WS                                  
155800     PERFORM IMS-STATUSKONTROLL                                           
155900     .                                                                    
156000     SKIP2                                                                
156100 IMS-GU-WDL811  SECTION.                                                  
156200                                                                          
156300     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
156400          DELIMITED BY SIZE INTO SSA1                                     
156500     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
156600          DELIMITED BY SIZE INTO SSA2                                     
156700     MOVE '  GE' TO GODK-STATUSKODER                                      
156800     CALL CBLTDLI USING GU WDL8-PCB  DLI-IO-WDL811 SSA1 SSA2              
156900     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
157000     PERFORM IMS-STATUSKONTROLL                                           
157100     .                                                                    
157200     EJECT                                                                
157300 IMS-GU-WDB601    SECTION.                                                
157400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
157500          DELIMITED BY SIZE INTO SSA1                                     
157600     MOVE '  ' TO GODK-STATUSKODER                                        
157700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
157800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
157900     PERFORM IMS-STATUSKONTROLL                                           
158000     .                                                                    
158100     EJECT                                                                
158200 IMS-STATUSKONTROLL SECTION.                                              
158300                                                                          
158400     SET STATUS-IX TO 1                                                   
158500     SEARCH GODK-STATUS                                                   
158600       AT END                                                             
158700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
158800         DELIMITED BY SIZE INTO FELTEXT                                   
158900         CALL FELLOG                                                      
159000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
159100         CONTINUE                                                         
159200     END-SEARCH                                                           
159300     .                                                                    
159400 DB2-SELECT-FSG2-TAB SECTION.                                             
159500                                                                          
159600     MOVE 000100           TO GODK-SQLCODEKODER                           
159700     EXEC SQL                                                             
159800         SELECT SULEVANT_PER,                                             
159900                SULEVANT_AAR,                                             
160000                SULEVANT_FAAR,                                            
160100                SULEVANT_RAAR,                                            
160200                SULEVANT_FRAAR                                            
160300         INTO                                                             
160400               :FSG-SULEVANT-PER,                                         
160500               :FSG-SULEVANT-AAR,                                         
160600               :FSG-SULEVANT-FAAR,                                        
160700               :FSG-SULEVANT-RAAR,                                        
160800               :FSG-SULEVANT-FRAAR                                        
160900         FROM FSG2                                                        
161000         WHERE IDARTNR = :W-IDARTNR                                       
161100     END-EXEC                                                             
161200     MOVE SQLCODE          TO SQLCODE-WS                                  
161300     PERFORM DB2-STATUSKONTROLL                                           
161400     .                                                                    
161500     EJECT                                                                
161600 DB2-STATUSKONTROLL SECTION.                                              
161700                                                                          
161800     SET SQLCODE-IX        TO 1                                           
161900     SEARCH GODK-SQLCODE AT END CALL FELLOG                               
162000       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
162100          CONTINUE                                                        
162200     END-SEARCH                                                           
162300     .                                                                    
