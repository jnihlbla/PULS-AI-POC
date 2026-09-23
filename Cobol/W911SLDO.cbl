000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W911SLDO.                                                
000500 AUTHOR.         GÖRAN KJELLSON.                                          
000600 DATE-WRITTEN.   HÖSTEN 2017.                                             
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*        PROGRAMMET ÄR EN SUBMODUL TILL:                                  
001100*        W9011600, W9011300 OCH W9033100                                  
001200*                                                                         
001300*    FUNKTION.                                                            
001400*      - SALDOFRÅGA                                                       
001500*        FÖR GIVET DISTRIKT, KUND, ARTIKEL OCH ANTAL                      
001600*        KONTROLLERAS OM SALDO FINNS TILLGÄNGLIGT I LAGER ELLER           
001700*        HOS DIREKTLEVERANTÖR.                                            
001800*                                                                         
001900*        LÄNKAREA: W911SLDO                                               
002000*                                                                         
002100*    CHANGE LOG:                                                          
002200*                                                                         
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002710*    -COPY WY2000W1                                                       
002720     SKIP3                                                                
002800                                                                          
002900 77  IDPGM                       PIC X(08)   VALUE 'W911SLDO'.            
003000 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003100 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  YES                         PIC X       VALUE 'Y'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 01  WS-KVFRYSTI                 PIC 9(2).                                
003600 01  WS-KVDAGAR-DIFF             PIC 9(3).                                
003610 01  WS-KDORDBEK-SUPERS          PIC 9(2)    VALUE ZERO.                  
003620                                                                          
003630*01  -COPY WWDCKONS                                                       
003631                                                                          
003640 01  FILLER                      PIC  X(16) VALUE 'EG-LAND'.              
003641 01  TEST-IDLANDX2               PIC  X(2).                               
003642 01  FILLER REDEFINES TEST-IDLANDX2.                                      
003643*    03    -COPY WWLANDX2.                                                
003644     EJECT                                                                
003645                                                                          
003650 77  ALLT-SW                     PIC X       VALUE 'J'.                   
003660     88  ALLT-OK                             VALUE 'J'.                   
003670     88  ALLT-EJ-OK                          VALUE 'N'.                   
003680                                                                          
003690 77  SALDO-SW                    PIC X       VALUE 'J'.                   
003700     88  SALDO-FINNS                         VALUE 'J'.                   
003800     88  SALDO-SAKNAS                        VALUE 'N'.                   
003900                                                                          
004000 77  SALDO-LEV-SW                PIC X       VALUE 'N'.                   
004100     88  SALDO-LEV                           VALUE 'J'.                   
004200                                                                          
004300 77  DIRLEV-SW                   PIC X       VALUE 'J'.                   
004400     88  DIRLEV                              VALUE 'J'.                   
004500                                                                          
004600 01  CURR-DC-IX                  PIC 9(3)    VALUE ZERO.                  
004700 01  IX-DCCLEAR-MAX              PIC 9(2)    VALUE 99.                    
004710 01  IDDC-IX                     PIC S9(3)   VALUE 0 COMP-3.              
004720 01  W-GMT-IDDC-CLEAR-GRP.                                                
004730     03 W-GMT-IDDC-CLEAR             PIC X(2) OCCURS 99 TIMES.            
004800                                                                          
004900 01  CURRENT-DATE                PIC 9(6).                                
005000                                                                          
005100 01  CURRENT-TIME-NUM            PIC 9(8).                                
005200 01  FILLER REDEFINES CURRENT-TIME-NUM.                                   
005300     03  CURRENT-TIME            PIC 9(6).                                
005400     03  FILLER                  PIC 9(2).                                
005500                                                                          
005600 01  W-WORK-VAR.                                                          
005700     03  W-TODAY-DATE.                                                    
005800         05  W-CENTURY           PIC 9(2) VALUE 20.                       
005900         05  W-DATE              PIC 9(6) VALUE 0.                        
006000                                                                          
006100     03  W-DASTADAT-X.                                                    
006200         05  W-DASTADAT          PIC 9(8)    VALUE ZERO.                  
006300     03  W-IDDISTR-X.                                                     
006400         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
006500                                                                          
006600     03  W-IDKUNDNR-FOM-X.                                                
006700         05  W-IDKUNDNR-FOM      PIC S9(7)   VALUE ZERO COMP-3.           
006800     03  W-IDKUNDNR-TOM-X.                                                
006900         05  W-IDKUNDNR-TOM      PIC S9(7)   VALUE ZERO COMP-3.           
007000     03 DDGS-TABLE.                                                       
007100        05 DDGS-LINE OCCURS 5 INDEXED BY DDGS-IX.                         
007200           07 DDGS-CLASS           PIC S9  COMP-3.                        
007300           07 DDGS-MIN-QTY         PIC S9(7) COMP-3.                      
007400           07 DDGS-IDDC            PIC X(2).                              
007500     03  DIR-INDX                PIC S9(9)   VALUE +0  COMP SYNC.         
007600     03  DDGS-ROWS               PIC 9(1).                                
007700     03  SUPPLIER-SW                 PIC X       VALUE 'N'.               
007800         88  SUPPLIER-FOUND                      VALUE 'J'.               
007900     03  DC21-SW                     PIC X       VALUE 'N'.               
008000         88  DC21-FOUND                          VALUE 'J'.               
008010     03  DDGS-IDDC-SW                PIC X       VALUE 'N'.               
008020         88  DDGS-IDDC-FND                       VALUE 'J'.               
008100     03  W-SDCLEV-SW                 PIC X       VALUE 'J'.               
008200         88  SDCLEV-OK                           VALUE 'J'.               
008300         88  SDCLEV-NT-OK                        VALUE 'N'.               
008400     03  DC-INSERTED-SW              PIC X       VALUE 'N'.               
008500         88  DC-INSERTED                         VALUE 'J'.               
008600         88  NT-DC-INSERTED                      VALUE 'N'.               
008700     03  W-IDDC-X.                                                        
008800         05  W-IDDC                  PIC X(02)   VALUE SPACE.             
008900     03  W-DISP                      PIC S9(8)   VALUE ZERO.              
009000     03  W-KVAKS-SDC                 PIC 9(7)    VALUE ZERO.              
009100     03  W-KVOKS-DAG                 PIC 9(7)    VALUE ZERO.              
009200     03  W-KVOKS-BULK                PIC 9(7)    VALUE ZERO.              
009300     03  WS-IDLEVNR              PIC X(5)    VALUE SPACE.                 
009310     03 W-WORK.                                                           
009320        05 W-IDDISTR-OK              PIC X.                               
009330        05 W-IDKUNDNR-OK             PIC X.                               
009340        05 W-KDFRAKT-OK              PIC X.                               
009350        05 W-IDVAT-OK                PIC X.                               
009360        05 W-IDPARTNR-OK             PIC X.                               
009370        05 W-KDKREDSP-OK             PIC X.                               
009380        05 W-KDKREDSP                PIC X.                               
009390        05 W-IDRFTAB                 PIC X(3).                            
009391                                                                          
009400*                                                                         
009500*    -COPY WWDC99                                                         
009600*                                                                         
009700*01  -COPY WWDC99 -PRE ALT3-                                              
009800*                                                                         
009900 01  GENERELLA-SUBPROGRAM.                                                
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010300     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
010500     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
010600     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
010700     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
010800     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
010900     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
011000     03  W411NDCA                PIC X(8)    VALUE 'W411NDCA'.            
011100     03  W411XDCA                PIC X(8)    VALUE 'W411XDCA'.            
011200     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
011300     03  W411STOR                PIC X(8)    VALUE 'W411STOR'.            
011400     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
011500     03  W411CLDC                PIC X(8)    VALUE 'W411CLDC'.            
011600     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
011700                                                                          
011800*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
011900*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
012000*01 -COPY WORKAREA                                                        
012100                                                                          
012200*    --- PARAMETRAR TILL ABEND                                            
012300                                                                          
012400 01  ERROR-TEXT                  PIC X(80).                               
012500 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
012600 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33 COMP SYNC.         
012700                                                                          
013100 01   FILLER             PIC X(5)  VALUE 'AREG '.                         
013200*   -COPY W411AREG                                                        
013300                                                                          
013400 01   FILLER             PIC X(5)  VALUE 'KVAN '.                         
013500*   -COPY W411KVAN                                                        
013600                                                                          
013700 01   FILLER             PIC X(5)  VALUE 'DLEV '.                         
013800*   -COPY W411DLEV                                                        
013900                                                                          
014000 01   FILLER             PIC X(5)  VALUE 'SPAR '.                         
014100*   -COPY W411SPAR                                                        
014200                                                                          
014300 01   FILLER             PIC X(5)  VALUE 'SDCA '.                         
014400*   -COPY W411SDCA                                                        
014500                                                                          
014600 01   FILLER             PIC X(5)  VALUE 'NDCA '.                         
014700*   -COPY W411NDCA                                                        
014800                                                                          
014900 01   FILLER             PIC X(5)  VALUE 'XDCA '.                         
015000*   -COPY W411XDCA                                                        
015100                                                                          
015200 01 FILLER               PIC X(8)  VALUE 'W411XDK7'.                      
015300*   -COPY W411XDK7 -PRE NDCA-                                             
015400                                                                          
015500 01   FILLER             PIC X(5)  VALUE 'RANS '.                         
015600*   -COPY W411RANS                                                        
015700                                                                          
015800 01   FILLER             PIC X(5)  VALUE 'STOR '.                         
015900*   -COPY W411STOR                                                        
016000                                                                          
016100 01   FILLER             PIC X(5)  VALUE 'CDCA '.                         
016200*   -COPY W411CDCA                                                        
016300                                                                          
016400 01   FILLER             PIC X(5)  VALUE 'CLDC '.                         
016500*   -COPY W411CLDC                                                        
016600                                                                          
016700*  --- FÖR ATT FÅ RÄTT SEKEL VID ANROP TILL W218ETA                       
016800 01  WS-ETA-DATUM.                                                        
016900     03  WS-ETA-DATUM-AAR        PIC 9(2).                                
017000     03  WS-ETA-DATUM-MAANAD     PIC 9(2).                                
017100     03  WS-ETA-DATUM-DAG        PIC 9(2).                                
017200 01   FILLER             PIC X(5)  VALUE 'LETA '.                         
017300*   -COPY W218LETA -PRE ETA-.                                             
017400                                                                          
017500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017600*                                                                         
017700                                                                          
017800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017900 01  NYCKLAR-TILL-DLI.                                                    
018000     03  W-WDF101KY-X.                                                    
018100         05  W-IDLEVNR-WDF1      PIC  X(5)   VALUE SPACE.                 
018200     03 W-WDF118KY-X.                                                     
018300         05 W-IDDISTR-WDF1       PIC S9(5)   VALUE ZERO COMP-3.           
018400         05 W-IDKUNDNR-WDF1      PIC S9(7)   VALUE ZERO COMP-3.           
018500         05 W-KDORDKL-WDF1       PIC S9(1)   VALUE ZERO COMP-3.           
018600     03  W-IDARTNR-X.                                                     
018700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018800     03  W-FLTEXT-X.                                                      
018900         05  W-FLTEXT            PIC  X(1)   VALUE 'N'.                   
019000     03  W-IDLEVNR-X.                                                     
019100         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
019200                                                                          
019300     03  W-IDSKYLT-X.                                                     
019400         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
019500                                                                          
019600     03  W-WDGX01KEY-X.                                                   
019700         05  W-IDHTYP            PIC  X(4)   VALUE '4521'.                
019800         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
019900                                                                          
020000     03  W-WDGX11KEY-X.                                                   
020100         05  W-KDORDBEK          PIC  9(2)   VALUE ZERO.                  
020200         05  W-IDSKYLT-WDGX      PIC  X(3)   VALUE SPACE.                 
020300                                                                          
020400     03  W-WDB301KY-X.                                                    
020500         05  W-IDDC-WDB3         PIC X(2)    VALUE SPACE.                 
020600         05  W-IDDISTR-WDB3      PIC S9(5)   VALUE ZERO COMP-3.           
020700         05  W-IDKUNDNR-WDB3     PIC S9(7)   VALUE ZERO COMP-3.           
020800                                                                          
020900     03  W-WDB301KY-DEF-X.                                                
021000         05  W-IDDC-WDB3-DEF     PIC X(2)    VALUE SPACE.                 
021100         05  W-IDDISTR-WDB3-DEF  PIC S9(5)   VALUE ZERO COMP-3.           
021200         05  W-IDKUNDNR-WDB3-DEF PIC S9(7) VALUE +9999999 COMP-3.         
021300                                                                          
021400     03  W-WDF2A1KY-MIN-X.                                                
021500         05  W-IDARTNR-MIN         PIC S9(9)   VALUE ZERO COMP-3.         
021600         05  W-IDLEVNR-MIN         PIC  X(5)   VALUE SPACE.               
021700                                                                          
021800     03  W-WDF2A1KY-MAX-X.                                                
021900         05  W-IDARTNR-MAX          PIC S9(9)   VALUE ZERO COMP-3.        
022000         05  W-IDLEVNR-MAX          PIC  X(5)   VALUE SPACE.              
022100                                                                          
022200     03  W-WDF201KY-X.                                                    
022300         05  W-IDLEVNR-WDF2         PIC  X(5)   VALUE SPACE.              
022400         05  W-IDDIRGRP-WDF2        PIC X(10)   VALUE SPACE.              
022500                                                                          
022510     03  W-IDGMT-X.                                                       
022520         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
022530         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
022540                                                                          
022550     03  W-WDB101KY-X.                                                    
022560         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
022570         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
022580                                                                          
022600*    --- IMS FUNKTIONSKODER                                               
022700*01  -COPY W0003                                                          
022800                                                                          
022900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
023000 01  DLI-IO-WDF101.                                                       
023100*    03  -COPY WDF101                                                     
023200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF118'.                      
023300 01  DLI-IO-WLLEVA18.                                                     
023400*    03  -COPY WDF118                                                     
023500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF201'.                      
023600 01  DLI-IO-WDF201.                                                       
023700*    03  -COPY WDF201                                                     
023800                                                                          
023900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF211'.                      
024000 01  DLI-IO-WDF211.                                                       
024100*    03  -COPY WDF211                                                     
024200                                                                          
024300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF2A1'.                      
024400 01   DLI-IO-WDF2A1.                                                      
024500*     03  -COPY WDF2A1                                                    
024600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
024700 01  DLI-IO-WDK701.                                                       
024800*    03  -COPY WDK701                                                     
024900                                                                          
025000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
025100 01  DLI-IO-WDK711.                                                       
025200*    03  -COPY WDK711                                                     
025300                                                                          
025400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
025500 01  DLI-IO-WDK601.                                                       
025600*    03  -COPY WDK601                                                     
025700                                                                          
025800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
025900 01  DLI-IO-WDK611.                                                       
026000*    03  -COPY WDK611                                                     
026100                                                                          
026200 01  FILLER         PIC X(16) VALUE 'DLI-IO-XXKJ11'.                      
026300 01  DLI-IO-XXKJ11.                                                       
026400*    03  -COPY WDGX4522                                                   
026500                                                                          
026600 01  FILLER                      PIC X(16)   VALUE 'WDD311-AREA'.         
026700 01  DLI-IO-WDD311.                                                       
026800*    03  -COPY WDD311                                                     
026900                                                                          
027000 01  FILLER                      PIC X(16)   VALUE 'WDD701-AREA'.         
027100 01  DLI-IO-WDD701.                                                       
027200*    03  -COPY WDD701                                                     
027300                                                                          
027400 01  FILLER                      PIC X(16)   VALUE 'WDD702-AREA'.         
027500 01  DLI-IO-WDD702.                                                       
027600*    03  -COPY WDD702                                                     
027700                                                                          
027800 01  FILLER                      PIC X(16)   VALUE 'WDB301-AREA'.         
027900 01  DLI-IO-WDB301.                                                       
028000*    03  -COPY WDB301                                                     
028100                                                                          
028200 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
028300 01   DLI-IO-AREA-R601.                                                   
028400*     03  -COPY WDR601                                                    
028500*     05  -COPY W414XDCA      -RED FIL-WDR601-DATA                        
028600                                                                          
028610 01  FILLER         PIC X(16)   VALUE 'WDB201-AREA'.                      
028620 01  DLI-IO-AREA-WDB201.                                                  
028630*    03  -COPY WDB201                                                     
028640*                                                                         
028650      EJECT                                                               
028660 01  FILLER         PIC X(16)   VALUE 'WDB101-AREA'.                      
028670 01  DLI-IO-AREA-WDB101.                                                  
028680*    03  -COPY WDB101                                                     
028690*                                                                         
028700 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
028800 01  DLI-IO-AREA-OHUV.                                                    
028900     03  WDQ201.                                                          
029000*        05  -COPY WDQ201                                                 
029100                                                                          
029200 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
029300 01  DLI-IO-AREA-ARB.                                                     
029400     03  WDQ212.                                                          
029500*        05  -COPY WDQ212                                                 
029600                                                                          
029700*    --- STATUS-KOD FRÅN IMS                                              
029800 01  STATUS-WS                   PIC XX.                                  
029900     88  SEGMENT-FINNS                       VALUE '  '.                  
030000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
030100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030200     88  BASEN-SLUT                          VALUE 'GB'.                  
030300     SKIP2                                                                
030400 01  GODK-STATUSKODER.                                                    
030500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030600                                                                          
030700 01  ALL-SSA.                                                             
030800    03 SSA1                      PIC X(160).                              
030900    03 SSA2                      PIC X(96).                               
031000                                                                          
031100                                                                          
031200 LINKAGE SECTION.                                                         
031300                                                                          
031400*   -COPY W911SLDO                                                        
031500                                                                          
031600*01  -COPY W0008      -PRE WDF1-                                          
031700     05  FILLER                  PIC X.                                   
031800*01  -COPY W0008      -PRE WDF2-                                          
031900     05  FILLER                  PIC X.                                   
032000*01  -COPY W0008      -PRE WDF2A-                                         
032100     05  FILLER                  PIC X.                                   
032200*01  -COPY W0008      -PRE WDK7-                                          
032300     05  FILLER                  PIC X.                                   
032400*01  -COPY W0008      -PRE WDK6-                                          
032500     05  FILLER                  PIC X.                                   
032600*01  -COPY W0008      -PRE XXKJ-                                          
032700     05  FILLER                  PIC X.                                   
032800*01  -COPY W0008      -PRE WDD7-                                          
032900     05  FILLER                  PIC X.                                   
033000*01  -COPY W0008      -PRE BENA-                                          
033100     05  FILLER                  PIC X.                                   
033200*01  -COPY W0008      -PRE WDB3-                                          
033300     05  FILLER                  PIC X.                                   
033400*01  -COPY W0008      -PRE WDR6-                                          
033500     05  FILLER                  PIC X.                                   
033510*01  -COPY W0008      -PRE WDB2-                                          
033520     05  FILLER                  PIC X.                                   
033530*01  -COPY W0008      -PRE WDB1-                                          
033540     05  FILLER                  PIC X.                                   
033600                                                                          
034200 01  KVAN-WDB2-PCB               PIC X.                                   
034300 01  KVAN-WDC1-PCB               PIC X.                                   
034400                                                                          
034500 01  AREG-WDK6-PCB               PIC X.                                   
034600 01  AREG-WDK7-PCB               PIC X.                                   
034700                                                                          
034800 01  DLEV-LEVF-PCB               PIC X.                                   
034900 01  DLEV-LEVG-PCB               PIC X.                                   
035000 01  DLEV-LEVA-PCB               PIC X.                                   
035100 01  DLEV-ARTS-PCB               PIC X.                                   
035200 01  DLEV-WDB6-PCB               PIC X.                                   
035300 01  DLEV-FILA-PCB               PIC X.                                   
035400                                                                          
035500 01  SPAR-WDF8-PCB               PIC X.                                   
035600 01  SPAR-WDF8A-PCB              PIC X.                                   
035700 01  SPAR-WDK6-PCB               PIC X.                                   
035800                                                                          
035900 01  SDCA-ARTS-PCB               PIC X.                                   
036000 01  SDCA-WDB6-PCB               PIC X.                                   
036100 01  SDCA-WDK9-PCB               PIC X.                                   
036200 01  SDCA-WDR6-PCB               PIC X.                                   
036300 01  SDCA-WDK6-PCB               PIC X.                                   
036400 01  SDCA-WDQ4C-PCB              PIC X.                                   
036500 01  SDCA-WDQ2-PCB               PIC X.                                   
036600 01  SDCA-WDQ4-PCB               PIC X.                                   
036700 01  SDCA-WDB6-2-PCB             PIC X.                                   
036800 01  SDCA-WDK6-2-PCB             PIC X.                                   
036900 01  SDCA-WDK7-2-PCB             PIC X.                                   
037000 01  SDCA-WDK7-3-PCB             PIC X.                                   
037100                                                                          
037200 01  NDCA-USEA-PCB               PIC X.                                   
037300 01  NDCA-WDK7-PCB               PIC X.                                   
037400 01  NDCA-WDL6-PCB               PIC X.                                   
037500 01  NDCA-WDB6-PCB               PIC X.                                   
037600                                                                          
037700 01  RANS-XXKM-PCB               PIC X.                                   
037800 01  RANS-ARTM-PCB               PIC X.                                   
037900 01  RANS-ARTS-PCB               PIC X.                                   
038000                                                                          
038100 01  CDCA-ARTM-PCB               PIC X.                                   
038200 01  CDCA-INLB-PCB               PIC X.                                   
038300 01  CDCA-WDB2-PCB               PIC X.                                   
038400 01  CDCA-WDC1-PCB               PIC X.                                   
038500                                                                          
038600 01  CLDC-WDB6-PCB               PIC X.                                   
038700                                                                          
038800 01  ETA-ARTC-PCB                PIC X.                                   
038900 01  ETA-WDK7-PCB                PIC X.                                   
039000 01  ETA-INLC-PCB                PIC X.                                   
039100 01  ETA-LEVA-PCB                PIC X.                                   
039200 01  ETA-WDB6-PCB                PIC X.                                   
039300 01  ETA-WDD9-PCB                PIC X.                                   
039400                                                                          
039500 01  XDCA-USEA-PCB               PIC X.                                   
039600 01  XDCA-WDB6-PCB               PIC X.                                   
039700 01  XDCA-WDK6-PCB               PIC X.                                   
039800 01  XDCA-WDK7-PCB               PIC X.                                   
039900 01  XDCA-WDK9-PCB               PIC X.                                   
040000 01  XDCA-WDL6-PCB               PIC X.                                   
040100 01  XDCA-WDQ4B-PCB              PIC X.                                   
040200 01  XDCA-WDQ2-PCB               PIC X.                                   
040300 01  XDCA-WDQ4-PCB               PIC X.                                   
040400 01  XDCA-WDR6-PCB               PIC X.                                   
040500 01  XDCA-WDB6-2-PCB             PIC X.                                   
040600 01  XDCA-WDK6-2-PCB             PIC X.                                   
040700 01  XDCA-WDK7-2-PCB             PIC X.                                   
040800 01  XDCA-WDK7-3-PCB             PIC X.                                   
040900                                                                          
041000 PROCEDURE DIVISION  USING SLDO-W911SLDO                                  
041100                     WDF1-PCB WDF2-PCB WDF2A-PCB WDK7-PCB                 
041200                     WDK6-PCB XXKJ-PCB WDD7-PCB BENA-PCB                  
041300                     WDB3-PCB WDR6-PCB WDB2-PCB WDB1-PCB                  
041400                                                                          
041800                     KVAN-WDB2-PCB KVAN-WDC1-PCB                          
041900                                                                          
042000                     AREG-WDK6-PCB AREG-WDK7-PCB                          
042100                                                                          
042200                     DLEV-LEVF-PCB DLEV-LEVG-PCB  DLEV-LEVA-PCB           
042300                     DLEV-ARTS-PCB DLEV-WDB6-PCB  DLEV-FILA-PCB           
042400                                                                          
042500                     SPAR-WDF8-PCB SPAR-WDF8A-PCB SPAR-WDK6-PCB           
042600                                                                          
042700                     SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB            
042800                     SDCA-WDR6-PCB SDCA-WDK6-PCB SDCA-WDQ4C-PCB           
042900                     SDCA-WDQ2-PCB SDCA-WDQ4-PCB                          
042910                     SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB                      
042920                     SDCA-WDK7-2-PCB                                      
042930                     SDCA-WDK7-3-PCB                                      
042940                                                                          
042950                     NDCA-USEA-PCB NDCA-WDK7-PCB  NDCA-WDL6-PCB           
042960                     NDCA-WDB6-PCB                                        
042970                                                                          
042980                     RANS-XXKM-PCB RANS-ARTM-PCB  RANS-ARTS-PCB           
042990                                                                          
043000                     CDCA-ARTM-PCB CDCA-INLB-PCB                          
043100                     CDCA-WDB2-PCB CDCA-WDC1-PCB                          
043200                                                                          
043300                     CLDC-WDB6-PCB                                        
043400                                                                          
043500                     ETA-ARTC-PCB  ETA-WDK7-PCB   ETA-INLC-PCB            
043600                     ETA-LEVA-PCB  ETA-WDB6-PCB   ETA-WDD9-PCB            
043700                                                                          
043800                     XDCA-USEA-PCB                                        
043900                     XDCA-WDB6-PCB XDCA-WDK6-PCB  XDCA-WDK7-PCB           
044000                     XDCA-WDK9-PCB XDCA-WDL6-PCB  XDCA-WDQ4B-PCB          
044100                     XDCA-WDQ2-PCB XDCA-WDQ4-PCB  XDCA-WDR6-PCB           
044200                     XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB                      
044300                     XDCA-WDK7-2-PCB                                      
044400                     XDCA-WDK7-3-PCB.                                     
044500                                                                          
044600     MOVE JA  TO ALLT-SW                                                  
044700     MOVE NEJ TO SALDO-SW                                                 
044800     MOVE NEJ TO DIRLEV-SW                                                
044900                                                                          
045000     PERFORM A-INITIERA-UT-AREA                                           
045100     PERFORM B-KOLLA-KUND                                                 
045200     IF ALLT-OK                                                           
045300        PERFORM C-KOLLA-ARTIKEL                                           
045400        MOVE 1 TO CURR-DC-IX                                              
045500        MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX) TO WS-IDDC                      
045600                                                                          
045700        IF ALLT-OK OR (NDC AND SLDO-KDORDBEK = ZERO)                      
045800           PERFORM D-KOLLA-KVANT-BRYTES                                   
045900           PERFORM E-KOLLA-SPARRAR                                        
046000*          IF ALLT-OK OR (NDC AND SPAR-KDORDBEK = ZERO)                   
046100           IF ALLT-OK OR SPAR-FLPUBCDC = YES                              
046200              IF AREG-REDIRLEV > 0                                        
046300                 PERFORM P-GET-DC21-DDGS                                  
046400                 IF DC21-FOUND                                            
046500                   CONTINUE                                               
046600                 ELSE                                                     
046700                   PERFORM F-KOLLA-DIREKTLEVERANS                         
046800                 END-IF                                                   
046900              END-IF                                                      
047000              IF NOT DIRLEV                                               
047100                 MOVE 1 TO CURR-DC-IX                                     
047200                 MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX) TO WS-IDDC             
047300                 IF SDC OR LDC                                            
047400                    PERFORM G-KOLLA-SDC-LDC                               
047500                 ELSE                                                     
047600                    IF NDC                                                
047700                       PERFORM H-KOLLA-XDC                                
047800                    END-IF                                                
047900                 END-IF                                                   
048000                                                                          
048100                 IF ALLT-OK AND SALDO-SAKNAS AND CDC                      
048200                    PERFORM I-KOLLA-RANSONERING                           
048300                    IF ALLT-OK                                            
048400                       PERFORM J-KOLLA-STORT-UTTAG                        
048500                    END-IF                                                
048600                    IF ALLT-OK                                            
048700                       PERFORM K-KOLLA-CDC                                
048800                    END-IF                                                
048900                 END-IF                                                   
049000              END-IF                                                      
049100           END-IF                                                         
049200        END-IF                                                            
049300     END-IF                                                               
049400                                                                          
049500     IF ALLT-OK                                                           
049600        IF SALDO-SAKNAS                                                   
049700           IF DIRLEV                                                      
050200              IF SLDO-KVAVBART = ZERO AND                                 
050300                 SLDO-IDDC NOT = '21' AND                                 
050400                 SLDO-KDORDBEK NOT = '21' AND                             
050410                 AREG-KDERS < 19                                          
050500                 PERFORM L-KOLLA-DIRLEV-DISP                              
050600              END-IF                                                      
050800           ELSE                                                           
050900              PERFORM M-KOLLA-SALDO-DISP                                  
051000           END-IF                                                         
051100        ELSE                                                              
051200           PERFORM N-CALC-CUST-ETA                                        
051300        END-IF                                                            
051400     END-IF                                                               
051500                                                                          
051600     PERFORM O-REDIGERA-UT-AREA                                           
051700                                                                          
051800     GOBACK                                                               
051900     .                                                                    
052000                                                                          
052100                                                                          
052200 A-INITIERA-UT-AREA  SECTION.                                             
052300     MOVE 'A-INITIERA' TO CURRENT-SECTION                                 
052400                                                                          
052500                                                                          
052600*TO EXCLUDE INSERTION OF LOG INTO WDR6 DB FOR THE CALL FROM               
052700*W91091 PGM                                                               
052800     IF SLDO-IDDC = 'XX'                                                  
052900        MOVE 'W91091' TO IDPGM                                            
053000     END-IF                                                               
053100*                                                                         
053200     MOVE ZERO    TO SLDO-KVAVBART                                        
053300     MOVE SPACE   TO SLDO-IDDC                                            
053400     MOVE ZERO    TO SLDO-TIDISPIN                                        
053500                     SLDO-TIKLAR                                          
053600                     SLDO-KDORDBEK                                        
053700                       WS-KDORDBEK-SUPERS                                 
053800                     SLDO-TIREGDAT                                        
053900     MOVE SPACE   TO SLDO-FLTPO1                                          
054000     MOVE ZERO    TO SLDO-KVFRYSTI                                        
054100     MOVE SPACE   TO SLDO-TEORDBEK                                        
054200                     SLDO-TEORDBEK-ENG                                    
054300                     SLDO-BEART                                           
054400                     SLDO-BEART-ENG                                       
054500                     SLDO-KDSORT                                          
054600     MOVE ZERO    TO SLDO-PRINK                                           
054700                     SLDO-IDARTNR-TILLK                                   
054800     MOVE SPACE   TO SLDO-IDMFSMED                                        
054900                                                                          
055000     ACCEPT CURRENT-DATE     FROM DATE                                    
055100     ACCEPT CURRENT-TIME-NUM FROM TIME                                    
055200     ACCEPT WS-ETA-DATUM     FROM DATE                                    
055300     .                                                                    
055400                                                                          
055500                                                                          
055600 B-KOLLA-KUND SECTION.                                                    
055700     MOVE 'B-KOLLA-KUND' TO CURRENT-SECTION                               
055701                                                                          
055710     MOVE JA                   TO W-IDDISTR-OK                            
055720                                  W-IDKUNDNR-OK                           
055730                                  W-KDKREDSP-OK                           
055740                                  W-IDVAT-OK                              
055750                                  W-IDPARTNR-OK                           
055760*VALIDATE DISTRICT/CUSTOMER                                               
055770     MOVE SLDO-IDDISTR-IN      TO W-IDDISTR-WDB2                          
055780     MOVE SLDO-IDKUNDNR-IN     TO W-IDKUNDNR-WDB2                         
055790                                                                          
055791     PERFORM IMS-GU-WDB201                                                
055792                                                                          
055793     IF SEGMENT-SAKNAS                                                    
055794        MOVE NEJ               TO W-IDDISTR-OK                            
055795                                  W-IDKUNDNR-OK                           
055796                                  ALLT-SW                                 
055797     ELSE                                                                 
055798        MOVE CURRENT-DATE   TO TMP1-YYMMDD                                
055799        MOVE GMT-TISTADAT   TO TMP2-YYMMDD                                
055800        MOVE GMT-TISTODAT   TO TMP3-YYMMDD                                
055810        PERFORM WY2000Q1                                                  
055820        IF(TMP1-YYMMDD < TMP3-YYMMDD OR                                   
055830            TMP3-YYMMDD = +0)                                             
055840        AND                                                               
055850          ((TMP1-YYMMDD NOT < TMP2-YYMMDD) AND                            
055860            TMP2-YYMMDD > +0)                                             
055870           PERFORM BA-MOVE-GMT-FIELDS                                     
055880        ELSE                                                              
055890          MOVE NEJ             TO W-IDKUNDNR-OK                           
055891                                  ALLT-SW                                 
055892        END-IF                                                            
055893     END-IF                                                               
055894                                                                          
055895*VALIDATE IDVAT/KDKREDSP/IDPARTNR                                         
055896     MOVE SPACE                TO W-KDKREDSP                              
055897     IF ALLT-SW = JA                                                      
055898        IF GMT-IDPARTNR NOT = SPACE                                       
055899          MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                      
055900          MOVE GMT-IDFTG          TO W-WDB1-IDFTG                         
055910          PERFORM IMS-GU-WDB1-WDB101                                      
055920          IF SEGMENT-FINNS                                                
055930             MOVE BET-KDKREDSP    TO W-KDKREDSP                           
055940             MOVE BET-IDLANDX2    TO TEST-IDLANDX2                        
055950             IF LANDX2-EU-EJ-SE AND BET-IDVAT = SPACE                     
055960               MOVE NEJ           TO W-IDVAT-OK                           
055970                                     ALLT-SW                              
055980             END-IF                                                       
055990          ELSE                                                            
055991             MOVE NEJ             TO ALLT-SW                              
055992          END-IF                                                          
055993        ELSE                                                              
055994          MOVE NEJ                TO W-IDPARTNR-OK                        
055995        END-IF                                                            
055996     END-IF                                                               
055997                                                                          
055998     IF W-IDDISTR-OK   = NEJ OR                                           
055999        W-IDKUNDNR-OK  = NEJ OR                                           
056000        W-KDKREDSP     = 1   OR                                           
056010        W-IDVAT-OK     = NEJ OR                                           
056020        W-IDPARTNR-OK  = NEJ OR                                           
056030        ALLT-SW        = NEJ                                              
056040                                                                          
056050        MOVE NEJ               TO ALLT-SW                                 
056060        MOVE 'B10' TO SLDO-IDMFSMED                                       
056070     ELSE                                                                 
056080        MOVE GMT-IDSKYLT       TO W-IDSKYLT                               
056090                                  W-IDSKYLT-WDGX                          
056091     END-IF                                                               
056100                                                                          
058300     .                                                                    
058400                                                                          
058410 BA-MOVE-GMT-FIELDS SECTION.                                              
058420     MOVE 'BA-MOVE-GMT-FIE'    TO CURRENT-SECTION                         
058430                                                                          
058440     INITIALIZE W-GMT-IDDC-CLEAR-GRP                                      
058450                                                                          
058460     MOVE +1                   TO IDDC-IX                                 
058470     PERFORM UNTIL IDDC-IX > IX-DCCLEAR-MAX                               
058480       IF SLDO-KDORDKL-IN = 0                                             
058490         MOVE GMT-IDDC-VOR(IDDC-IX)  TO W-GMT-IDDC-CLEAR(IDDC-IX)         
058491       END-IF                                                             
058492       IF SLDO-KDORDKL-IN = 1                                             
058493         MOVE GMT-IDDC-DAY(IDDC-IX)  TO W-GMT-IDDC-CLEAR(IDDC-IX)         
058494       END-IF                                                             
058495       IF SLDO-KDORDKL-IN > 1                                             
058496         MOVE GMT-IDDC-BULK(IDDC-IX) TO W-GMT-IDDC-CLEAR(IDDC-IX)         
058497       END-IF                                                             
058498       ADD +1                  TO IDDC-IX                                 
058499     END-PERFORM                                                          
058500     MOVE GMT-IDRFTAB          TO W-IDRFTAB                               
058501     MOVE GMT-FLLDCKND         TO SLDO-FLLDCKND                           
058502     IF GMT-RESLATT = ZERO                                                
058503        MOVE '0'               TO W-IDRFTAB (3:1)                         
058504     END-IF                                                               
058505     .                                                                    
058510                                                                          
058600 C-KOLLA-ARTIKEL SECTION.                                                 
058700     MOVE 'C-KOLLA-ARTIKEL' TO CURRENT-SECTION                            
058800                                                                          
058900     MOVE SLDO-IDARTNR-IN    TO AREG-IDARTNR                              
059000     MOVE W-GMT-IDDC-CLEAR(1) TO AREG-IDDC                                
059100                                                                          
059200     CALL W411AREG USING AREG-W411AREG                                    
059300                         AREG-WDK6-PCB                                    
059400                         AREG-WDK7-PCB                                    
059500                                                                          
059600     IF AREG-KDORDBEK > 0 OR AREG-KDERS-UTG > 0                           
059700        IF AREG-KDORDBEK > 0                                              
059800           MOVE AREG-KDORDBEK TO SLDO-KDORDBEK                            
059900        ELSE                                                              
060000           MOVE 41            TO SLDO-KDORDBEK                            
060100        END-IF                                                            
060200        MOVE AREG-FLTPO1   TO SLDO-FLTPO1                                 
060300        IF SLDO-KDORDBEK   = 41                                           
060400           MOVE SLDO-KDORDBEK TO WS-KDORDBEK-SUPERS                       
060410        ELSE                                                              
060420           MOVE NEJ           TO ALLT-SW                                  
060430        END-IF                                                            
060440     ELSE                                                                 
060450        EVALUATE TRUE                                                     
060460          WHEN AREG-KDERS = 19 OR 29                                      
060470*           MOVE '054' TO WS-IDMFSMED                                     
060480            MOVE 54 TO SLDO-KDORDBEK                                      
060490                         WS-KDORDBEK-SUPERS                               
060500            MOVE '080' TO SLDO-IDMFSMED                                   
060600                                                                          
060700          WHEN AREG-KDERS = 52                                            
060800*           MOVE '052' TO WS-IDMFSMED                                     
060900            MOVE 52 TO SLDO-KDORDBEK                                      
061000            MOVE '080' TO SLDO-IDMFSMED                                   
061100            MOVE NEJ TO ALLT-SW                                           
061200                                                                          
061300          WHEN (AREG-KDERS > 10 AND < 14) OR                              
061400               (AREG-KDERS > 20 AND < 24) OR                              
061500               (AREG-KDERS = 17 OR 27)                                    
061600*           MOVE '041' TO WS-IDMFSMED                                     
061700            MOVE 41 TO SLDO-KDORDBEK                                      
061800                         WS-KDORDBEK-SUPERS                               
061900            MOVE '080' TO SLDO-IDMFSMED                                   
062000            IF AREG-KDERS = 13 OR 23                                      
062100               MOVE NEJ TO ALLT-SW                                        
062200            END-IF                                                        
062300                                                                          
062400          WHEN (AREG-KDERS > 13 AND < 17) OR                              
062500               (AREG-KDERS > 23 AND < 27) OR                              
062600               (AREG-KDERS = 18 OR 28)                                    
062700*           MOVE '061' TO WS-IDMFSMED                                     
062800            MOVE 61 TO SLDO-KDORDBEK                                      
062900                         WS-KDORDBEK-SUPERS                               
063000            MOVE '080' TO SLDO-IDMFSMED                                   
063100            IF AREG-KDERS = 16 OR 26                                      
063200               MOVE NEJ TO ALLT-SW                                        
063300            END-IF                                                        
063400                                                                          
063500        END-EVALUATE                                                      
063600        PERFORM CA-KOMPLETTERA-ARTIKEL                                    
063700     END-IF                                                               
063800     .                                                                    
063900                                                                          
064000                                                                          
064100 CA-KOMPLETTERA-ARTIKEL SECTION.                                          
064200     MOVE 'CA-KOMPL-ART ' TO CURRENT-SECTION                              
064300                                                                          
064400     MOVE SLDO-IDARTNR-IN    TO W-IDARTNR                                 
064500     PERFORM IMS-GU-WDK601                                                
064600     IF SEGMENT-FINNS                                                     
064700        MOVE AREG-KDSORT     TO SLDO-KDSORT                               
064800        PERFORM IMS-GNP-WDK611                                            
064900        IF SEGMENT-FINNS                                                  
065000           MOVE CLAG-PRINK   TO SLDO-PRINK                                
065100        END-IF                                                            
065200        PERFORM IMS-GU-BENA11-BSEQ                                        
065300        IF SEGMENT-FINNS                                                  
065400           MOVE TEXT-BEART    TO SLDO-BEART                               
065500           IF W-IDSKYLT = 'GB'                                            
065600              MOVE TEXT-BEART TO SLDO-BEART-ENG                           
065700           END-IF                                                         
065800        END-IF                                                            
065900        IF W-IDSKYLT NOT = 'GB'                                           
066000           MOVE 'GB'          TO W-IDSKYLT                                
066100           PERFORM IMS-GU-BENA11-BSEQ                                     
066200           IF SEGMENT-FINNS                                               
066300              MOVE TEXT-BEART TO SLDO-BEART-ENG                           
066400           END-IF                                                         
066500        END-IF                                                            
066600     END-IF                                                               
066700     .                                                                    
066800                                                                          
066900                                                                          
067000 D-KOLLA-KVANT-BRYTES SECTION.                                            
067100     MOVE 'D-KOLLA-KVANT' TO CURRENT-SECTION                              
067200                                                                          
067300     MOVE ZERO                 TO KVAN-KDKVBRYT-IN                        
067400     MOVE 'IMS '               TO KVAN-IDSYSTEM-IN                        
067500     MOVE SLDO-KVBEART-IN      TO KVAN-KVBEART-IN                         
067600     MOVE AREG-KVQPACK-0       TO KVAN-KVQPACK-0-IN                       
067700     MOVE AREG-KVQPACK-1       TO KVAN-KVQPACK-1-IN                       
067800     MOVE AREG-KDPRODSL        TO KVAN-KDPRODSL-IN                        
067900     MOVE AREG-KDSORT          TO KVAN-KDSORT-IN                          
068000     MOVE AREG-IDFKNGRP        TO KVAN-IDFKNGRP-IN                        
068100     MOVE SLDO-KDORDKL-IN      TO KVAN-KDORDKL-IN                         
068200     MOVE NEJ                  TO KVAN-FLEMBORD-IN                        
068300     MOVE NEJ                  TO KVAN-FLFORBI-IN                         
068400     MOVE NEJ                  TO KVAN-FLORDSPE-IN                        
068500     MOVE NEJ                  TO KVAN-FLOVRLEV-IN                        
068600     MOVE +0                   TO KVAN-IDKAMPRF-IN                        
068700     MOVE AREG-IDDC            TO KVAN-IDDC-IN                            
068800     MOVE SLDO-IDDISTR-IN      TO KVAN-IDDISTR-IN                         
068900     MOVE SLDO-IDKUNDNR-IN     TO KVAN-IDKUNDNR-IN                        
069000     MOVE SPACE                TO KVAN-BERADREF-IN                        
069100     MOVE AREG-IDARTNR         TO KVAN-IDARTNR-IN                         
069200                                                                          
069300     CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB KVAN-WDC1-PCB        
069400     .                                                                    
069500                                                                          
069600                                                                          
069700 E-KOLLA-SPARRAR SECTION.                                                 
069800     MOVE 'E-KOLLA-SPARRAR'  TO CURRENT-SECTION                           
069900                                                                          
070000     MOVE SPACE                TO SPAR-BERADREF                           
070100     MOVE SPACE                TO SPAR-BEKUNDRF                           
070200     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
070300     MOVE NEJ                  TO SPAR-FLEMBORD                           
070400     MOVE NEJ                  TO SPAR-FLFORBI                            
070500     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
070600     MOVE NEJ                  TO SPAR-FLORDSPE                           
070700     MOVE NEJ                  TO SPAR-FLOVRLEV                           
070800     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
070900     MOVE SPACE                TO SPAR-FLRESTN                            
071000     MOVE AREG-IDARTNR         TO SPAR-IDARTNR                            
071100     MOVE AREG-FLIART          TO SPAR-FLIART                             
071200     MOVE SPACE                TO SPAR-FLMARKSP                           
071300     MOVE SLDO-IDDISTR-IN      TO SPAR-IDDISTR                            
071400     MOVE SLDO-IDKUNDNR-IN     TO SPAR-IDKUNDNR                           
071500     MOVE '0000000   '         TO SPAR-IDKUNDRF-RO                        
071600     MOVE AREG-IDDC            TO SPAR-IDDC                               
071700     MOVE 'VDI '               TO SPAR-IDSYSTEM                           
071800     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
071900     MOVE AREG-KDERS           TO SPAR-KDERS                              
072000     MOVE SPACE                TO SPAR-KDFAKTYP                           
072100     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
072200     MOVE +0                   TO SPAR-KDORDBEH                           
072300     MOVE SLDO-KDORDKL-IN      TO SPAR-KDORDKL                            
072400     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
072500     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
072600     MOVE SPACE                TO SPAR-KDPRTYP                            
072700     MOVE +0                   TO SPAR-KDTPOTYP                           
072800     MOVE AREG-KDUART          TO SPAR-KDUART                             
072900     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
073000     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
073100     MOVE +0                   TO SPAR-TIRODAT                            
073200     MOVE +0                   TO SPAR-TITPO                              
073300     MOVE NEJ                  TO SPAR-FLSDCLEV                           
073400     MOVE ZERO                 TO SPAR-TIREPDAT                           
073500                                                                          
073600     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
073700                                       SPAR-WDF8A-PCB                     
073800                                       SPAR-WDK6-PCB                      
073900     IF SPAR-KDORDBEK > ZERO                                              
074000        MOVE SPAR-KDORDBEK  TO SLDO-KDORDBEK                              
074010        IF SPAR-KDORDBEK = 67                                             
074100           MOVE JA             TO ALLT-SW                                 
074101        ELSE                                                              
074102           MOVE NEJ            TO ALLT-SW                                 
074103        END-IF                                                            
074210     END-IF                                                               
074300     .                                                                    
074400                                                                          
074500                                                                          
074600 F-KOLLA-DIREKTLEVERANS SECTION.                                          
074700     MOVE 'F-KOLLA-DIRLEV'  TO CURRENT-SECTION                            
074800                                                                          
074900     MOVE SLDO-IDDISTR-IN      TO DLEV-IDDISTR-IN                         
075000     MOVE SPACE                TO DLEV-IDDC-IN                            
075100     MOVE SPACE                TO DLEV-IDDC-ORD-IN                        
075200     MOVE SLDO-IDKUNDNR-IN     TO DLEV-IDKUNDNR-IN                        
075300     MOVE SLDO-KDORDKL-IN      TO DLEV-KDORDKL-IN                         
075400     MOVE AREG-IDARTNR         TO DLEV-IDARTNR-IN                         
075500     MOVE AREG-IDLEVNR         TO DLEV-IDLEVNR-IN                         
075600     MOVE KVAN-KVBEART-Q-UT    TO DLEV-KVBEART-Q-IN                       
075700     MOVE AREG-REDIRLEV        TO DLEV-REDIRLEV-IN                        
075800     MOVE +0                   TO DLEV-IDKAMPRF-IN                        
075900     MOVE +0                   TO DLEV-KDTPOTYP-IN                        
076000     MOVE AREG-KDUART          TO DLEV-KDUART-IN                          
076100     MOVE NEJ                  TO DLEV-FLFORBI-IN                         
076200     MOVE GMT-FLRESTN          TO DLEV-FLRESTN-IN                         
076300     MOVE AREG-FLREFILL        TO DLEV-FLREFILL-IN                        
076400     MOVE GMT-KDORDING         TO DLEV-KDORDING-IN                        
076500                                                                          
076600     MOVE 1                    TO IDDC-IX                                 
076700     PERFORM UNTIL IDDC-IX > IX-DCCLEAR-MAX                               
076800       MOVE W-GMT-IDDC-CLEAR(IDDC-IX)                                     
076900                               TO DLEV-IDDC-CLEAR-IN(IDDC-IX)             
077000       ADD +1                TO IDDC-IX                                   
077100     END-PERFORM                                                          
077110                                                                          
077200     MOVE SPACE                TO DLEV-IDKUNDRF-IN                        
077300     MOVE 0                    TO DLEV-KDCALL                             
077400     MOVE SPACE                TO DLEV-CLEARGROUP                         
077500     MOVE SPACE                TO DLEV-KDOI-UT                            
077600                                                                          
077700     CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                      
077800                                       DLEV-LEVG-PCB                      
077900                                       DLEV-LEVA-PCB                      
078000                                       DLEV-ARTS-PCB                      
078100                                       DLEV-WDB6-PCB                      
078200                                       DLEV-FILA-PCB                      
078300     IF DLEV-FLSDCLEV-UT = 'J'                                            
078400       MOVE ZERO               TO SLDO-KVFRYSTI                           
078500     ELSE                                                                 
078600       MOVE AREG-KVFRYSTI      TO SLDO-KVFRYSTI                           
078700     END-IF                                                               
078800     MOVE JA TO DIRLEV-SW                                                 
078900     IF DLEV-KDORDBEK-UT = ZERO                                           
079000        IF DLEV-IDLEVNR-UT = SPACE                                        
079100           MOVE NEJ TO DIRLEV-SW                                          
079200        ELSE                                                              
079300*    LEVERANS FRÅN DLEV-IDDC-UT                                           
079400           MOVE DLEV-KVBEART-Q-IN   TO SLDO-KVAVBART                      
079500           MOVE DLEV-IDDC-UT        TO SLDO-IDDC                          
079600           MOVE '010'               TO SLDO-IDMFSMED                      
079700           MOVE JA                  TO SALDO-SW                           
079800        END-IF                                                            
079900     ELSE                                                                 
080000        IF DLEV-KDORDBEK-UT = 95                                          
080100           MOVE DLEV-KDORDBEK-UT     TO SLDO-KDORDBEK                     
080200           PERFORM FA-KOLLA-SALDO-LEV                                     
080300           IF NOT SALDO-LEV                                               
080400*   VI HAR DLEV-IDLEVNR-UT, SKALL DET VISAS I SVARET???                   
080500              MOVE DLEV-IDDC-UT      TO SLDO-IDDC                         
080600           END-IF                                                         
080700        ELSE                                                              
080800           MOVE DLEV-KDORDBEK-UT     TO SLDO-KDORDBEK                     
080900           MOVE '080'                TO SLDO-IDMFSMED                     
081000           MOVE NEJ                  TO ALLT-SW                           
081100        END-IF                                                            
081200     END-IF                                                               
081300     .                                                                    
081400                                                                          
081500                                                                          
081600 FA-KOLLA-SALDO-LEV SECTION.                                              
081700     MOVE 'FA-KOLLA-SLD-LEV' TO CURRENT-SECTION                           
081800                                                                          
081900     IF DLEV-KVLS-DLEV-UT < ZERO                                          
082000        MOVE NEJ          TO SALDO-LEV-SW                                 
082100        MOVE '080'        TO SLDO-IDMFSMED                                
082200     ELSE                                                                 
082300        MOVE JA           TO SALDO-LEV-SW                                 
082400        MOVE DLEV-TIREGDAT-UT                                             
082500                          TO SLDO-TIREGDAT                                
082600        IF DLEV-KVBEART-Q-IN NOT > DLEV-KVLS-DLEV-UT                      
082700           MOVE DLEV-KVBEART-Q-IN                                         
082800                          TO SLDO-KVAVBART                                
082900           MOVE ZERO      TO SLDO-TIDISPIN                                
083000           MOVE '010'     TO SLDO-IDMFSMED                                
083100           MOVE JA        TO SALDO-SW                                     
083200        ELSE                                                              
083300           IF DLEV-TIINLMOT-UT > ZERO                                     
083400              MOVE DLEV-TIINLMOT-UT                                       
083500                          TO SLDO-TIDISPIN                                
083600           END-IF                                                         
083700           MOVE '080'     TO SLDO-IDMFSMED                                
083800        END-IF                                                            
083900     END-IF                                                               
084000     .                                                                    
084100                                                                          
084200                                                                          
084300 G-KOLLA-SDC-LDC SECTION.                                                 
084400     MOVE 'G-KOLLA-SDC-LDC' TO CURRENT-SECTION                            
084500                                                                          
084600     MOVE SLDO-IDDISTR-IN      TO SDCA-IDDISTR                            
084700     MOVE NEJ                  TO SDCA-FLFORBI                            
084800     MOVE NEJ                  TO SDCA-FLORDSPE                           
084900     MOVE AREG-FLREFILL        TO SDCA-FLREFILL                           
085000     MOVE AREG-IDARTNR         TO SDCA-IDARTNR                            
085100     MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX)                                    
085200                               TO SDCA-IDDC                               
085300     MOVE SPACE                TO SDCA-IDDC-TVS                           
085400     MOVE SPACE                TO SDCA-IDLEVNR                            
085500     MOVE 'VDI '               TO SDCA-IDSYSTEM                           
085510*BELOW LINE IS COMMENTED B4 DCCLEAR CHANGE                                
085600*    MOVE KREG-KDORDKL         TO SDCA-KDORDKL                            
085700     MOVE 1                    TO SDCA-KDORDKL                            
085800     MOVE GMT-KDORDING         TO SDCA-KDORDING                           
085900     MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                           
086000     MOVE AREG-KDSORT          TO SDCA-KDSORT                             
086100     MOVE KVAN-KVBEART-Q-UT    TO SDCA-KVBEART-Q                          
086200     MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                          
086300     MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                           
086400     MOVE NEJ                  TO SDCA-FLSDCLEV                           
086500     MOVE ZERO                 TO SDCA-TIREPDAT                           
086600     MOVE ZERO                 TO SDCA-KVOKS-PREL                         
086700     MOVE +0                   TO SDCA-TIREPDAT                           
086800     MOVE +0                   TO SDCA-KVOKS-PREL                         
086900     MOVE +2                   TO SDCA-KDCALL                             
087000     MOVE CURR-DC-IX           TO SDCA-IXDCCLEAR                          
087100     MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX)                                    
087200                               TO WS-IDDC                                 
087300                                                                          
087400     PERFORM UNTIL CURR-DC-IX > IX-DCCLEAR-MAX                            
087500                OR W-GMT-IDDC-CLEAR(CURR-DC-IX) = SPACE                   
087600                OR SALDO-FINNS                                            
087700                OR NOT (SDC OR LDC)                                       
087800                                                                          
087900        CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                   
088000                            SDCA-WDB6-PCB SDCA-WDK9-PCB                   
088100                            SDCA-WDR6-PCB SDCA-WDK6-PCB                   
088200                            SDCA-WDQ4C-PCB SDCA-WDQ2-PCB                  
088300                            SDCA-WDQ4-PCB                                 
088310                            SDCA-WDB6-2-PCB                               
088320                            SDCA-WDK6-2-PCB                               
088330                            SDCA-WDK7-2-PCB                               
088340                            SDCA-WDK7-3-PCB                               
088350        IF SDCA-KDORDBEK = ZERO                                           
088360           MOVE JA               TO SALDO-SW                              
088370        ELSE                                                              
088380           ADD 1                 TO CURR-DC-IX                            
088390                                    SDCA-IXDCCLEAR                        
088400           MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX)                              
088500                                 TO WS-IDDC                               
088600                                    SDCA-IDDC                             
088700        END-IF                                                            
088800     END-PERFORM                                                          
088900                                                                          
089000     IF (SDC OR LDC) AND SALDO-FINNS                                      
089100        MOVE JA               TO SALDO-SW                                 
089200        MOVE SDCA-KVBEART-Q   TO SLDO-KVAVBART                            
089300        MOVE WS-IDDC          TO SLDO-IDDC                                
089400        MOVE '010'            TO SLDO-IDMFSMED                            
089500        IF CURR-DC-IX > 1                                                 
089600           MOVE '15'          TO SLDO-KDORDBEK                            
089700        END-IF                                                            
089800     END-IF                                                               
089900     .                                                                    
090000                                                                          
090100                                                                          
090200 H-KOLLA-XDC SECTION.                                                     
090300     MOVE 'H-KOLLA-XDC' TO CURRENT-SECTION                                
090400                                                                          
090500     MOVE SPACE                TO CLDC-W411CLDC                           
090600     MOVE W-GMT-IDDC-CLEAR-GRP TO CLDC-IDDC-CLEAR-GRP                     
090700     CALL W411CLDC USING CLDC-W411CLDC CLDC-WDB6-PCB                      
090800*CALL TO XDCA MODULE                                                      
090900     PERFORM HX-KOLLA-XDC                                                 
091000*                                                                         
091100*    MOVE NEJ                  TO NDCA-FLFORBI                            
091200*    MOVE NEJ                  TO NDCA-FLLDCKND                           
091300*    MOVE GMT-FLPRELRO         TO NDCA-FLPRELRO                           
091400*    MOVE GMT-FLRESTN          TO NDCA-FLRESTN                            
091500*    MOVE NEJ                  TO NDCA-FLORDSPE                           
091600*    MOVE AREG-IDARTNR         TO NDCA-IDARTNR                            
091700*    MOVE AREG-IDDC            TO NDCA-IDDC                               
091800*    MOVE W-GMT-IDDC-CLEAR-GRP TO NDCA-IDDC-CLEAR-GRP                     
091900*    MOVE SPACE                TO NDCA-CLEARGROUP                         
092000*    MOVE SPACE                TO NDCA-IDDC-TVS                           
092100*    MOVE SLDO-IDDISTR-IN      TO NDCA-IDDISTR                            
092200*    MOVE 1                    TO NDCA-IXDCCLEAR                          
092300*    MOVE AREG-KDARTURS        TO NDCA-KDARTURS                           
092400*    MOVE AREG-KDERS           TO NDCA-KDERS                              
092500*    MOVE GMT-KDORDING         TO NDCA-KDORDING                           
092600*    MOVE GMT-KDORDKL          TO NDCA-KDORDKL                            
092700*    MOVE 1                    TO NDCA-KDORDKL                            
092800*    MOVE AREG-KDSORT          TO NDCA-KDSORT                             
092900*    MOVE GMT-KVDAGAR-DOW      TO NDCA-KVDAGAR-DOW                        
093000*    MOVE KVAN-KVBEART-Q-UT    TO NDCA-KVBEART-Q                          
093100*    MOVE AREG-KVQPACK-1       TO NDCA-KVQPACK-1                          
093200*    MOVE CURRENT-DATE         TO NDCA-TIREGDAT                           
093300*    MOVE CURRENT-TIME         TO NDCA-TIREGTID                           
093400*    MOVE AREG-VKART           TO NDCA-VKART                              
093500*    MOVE AREG-VKART-NTO       TO NDCA-VKART-NTO                          
093600*    MOVE AREG-VLARTNTO        TO NDCA-VLARTNTO                           
093700*    MOVE +2                   TO NDCA-KDCALL                             
093800*    MOVE SPACE                TO NDCA-XDK7-IDDC                          
093900*    MOVE ZERO                 TO NDCA-XDK7-KDIDDC                        
094000*                                 NDCA-XDK7-KVOKS-DAG                     
094100*                                 NDCA-XDK7-KVOKS-BULK                    
094200                                                                          
094300*    CALL W411NDCA USING NDCA-W411NDCA CLDC-W411CLDC                      
094400*                                      NDCA-USEA-PCB                      
094500*                                      NDCA-WDK7-PCB                      
094600*                                      NDCA-WDL6-PCB                      
094700*                                      NDCA-WDB6-PCB                      
094800*                                      NDCA-XDK7-W411XDK7                 
094900*TO COMPARE XDCA & NDCA RESULTS                                           
095000*    PERFORM HX-CHECK-DIFF                                                
095100*                                                                         
095200     IF XDCA-KDORDBEK = ZERO                                              
095300        MOVE JA TO SALDO-SW                                               
095400        MOVE XDCA-KVBEART-Q TO SLDO-KVAVBART                              
095500        MOVE '010'          TO SLDO-IDMFSMED                              
095600        MOVE XDCA-IDDC-OUT  TO SLDO-IDDC                                  
095700        IF XDCA-DAPUBL > 0 AND SPAR-FLPUBCDC = YES                        
095800*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR PUBL.DATE ON CDC         
095900*** THAT BLOCK SHALL BE REMOVED IF CHECK ON DAPUBL FOR XDC IS OK.         
096000           MOVE 0           TO SLDO-KDORDBEK                              
096100           MOVE JA          TO ALLT-SW                                    
096200        END-IF                                                            
096300     ELSE                                                                 
096400        IF SPAR-FLPUBCDC = YES                                            
096500*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR CDC PUL.DATE.            
096600*** IF XDCA-KDORDBEK = 15 MEANS LINE IS MOVED TO ANOTHER DC               
096700*** THEN DAPUBL IS TESTED OK IN W411XDCA                                  
096800           IF (XDCA-KDORDBEK = 15 OR 92 OR 98 OR 99)                      
096900               AND XDCA-DAPUBL > ZERO                                     
097000              IF SPAR-KDORDBEK > ZERO                                     
097100                IF SPAR-KDORDBEK = SLDO-KDORDBEK                          
097200                   MOVE ZERO TO SLDO-KDORDBEK                             
097300                   MOVE ZERO TO SPAR-KDORDBEK                             
097400                ELSE                                                      
097500                   MOVE ZERO TO XDCA-KDORDBEK                             
097600                END-IF                                                    
097700              END-IF                                                      
097800           END-IF                                                         
097900        END-IF                                                            
098000        IF XDCA-KDORDBEK NOT = 15                                         
098100           MOVE NEJ             TO ALLT-SW                                
098200           MOVE '080'           TO SLDO-IDMFSMED                          
098300           MOVE XDCA-KDORDBEK   TO SLDO-KDORDBEK                          
098400           IF XDCA-KDORDBEK = 99                                          
098500              MOVE XDCA-IDDC-OUT TO SLDO-IDDC                             
098600           END-IF                                                         
098700        ELSE                                                              
098800           MOVE XDCA-IDDC-OUT    TO WS-IDDC                               
098900           IF NDC                                                         
099000              MOVE JA            TO SALDO-SW                              
099100              MOVE XDCA-KVBEART-Q                                         
099200                                 TO SLDO-KVAVBART                         
099300              MOVE XDCA-KDORDBEK TO SLDO-KDORDBEK                         
099400              MOVE '010'         TO SLDO-IDMFSMED                         
099500              MOVE XDCA-IDDC-OUT                                          
099600                                 TO SLDO-IDDC                             
099700           END-IF                                                         
099800        END-IF                                                            
099900     END-IF                                                               
100000     .                                                                    
100100                                                                          
100200                                                                          
100300 HX-KOLLA-XDC SECTION.                                                    
100400     MOVE 'HX-KOLLA-XDC' TO CURRENT-SECTION                               
100500                                                                          
100600* HARDCODED VALUES FOR OHUV AREA                                          
100700     INITIALIZE OHUV-WDQ201                                               
100800     MOVE 1                    TO OHUV-KDORDKL                            
100900     MOVE SPACE                TO OHUV-IDDC-TVS                           
101000     MOVE SLDO-IDDISTR-IN      TO OHUV-IDDISTR                            
101100     MOVE NEJ                  TO OHUV-FLORDSPE                           
101200                                  OHUV-FLFORBI                            
101300     MOVE GMT-FLPRELRO         TO OHUV-FLPRELRO                           
101400     MOVE GMT-FLRESTN          TO OHUV-FLRESTN                            
101500     MOVE GMT-KDORDING         TO OHUV-KDORDING                           
101600* XDCA-INPUT                                                              
101700                                                                          
101800     MOVE 1                    TO IDDC-IX                                 
101810     PERFORM UNTIL IDDC-IX > IX-DCCLEAR-MAX                               
101820       MOVE W-GMT-IDDC-CLEAR(IDDC-IX)                                     
101830                               TO XDCA-IDDC-CLEAR-IN(IDDC-IX)             
101840       ADD +1                  TO IDDC-IX                                 
101850     END-PERFORM                                                          
101860                                                                          
101900     MOVE SPACE                TO XDCA-IDDC-TVS                           
102000     MOVE NEJ                  TO XDCA-FLLDCKND                           
102100     MOVE AREG-IDARTNR         TO XDCA-IDARTNR                            
102200     MOVE AREG-IDDC            TO XDCA-IDDC                               
102300     MOVE SPACE                TO XDCA-IDLEVNR                            
102400     MOVE AREG-KDERS           TO XDCA-KDERS                              
102500     MOVE AREG-KDSORT          TO XDCA-KDSORT                             
102600     MOVE KVAN-KVBEART-Q-UT    TO XDCA-KVBEART-Q                          
102700     MOVE AREG-KVQPACK-1       TO XDCA-KVQPACK-1                          
102800     MOVE CURRENT-DATE         TO XDCA-TIREGDAT                           
102900     MOVE CURRENT-TIME         TO XDCA-TIREGTID                           
103000     MOVE AREG-VKART           TO XDCA-VKART                              
103100     MOVE +4                   TO XDCA-KDCALL                             
103200* XDCA-OUTPUT                                                             
103300     MOVE SPACE                TO XDCA-IDDC-OUT                           
103400                                  XDCA-IDDC-RO                            
103500                                  XDCA-KDARTURS                           
103600                                  XDCA-KDOI                               
103700                                  XDCA-CLEARGROUP                         
103800     MOVE ZERO                 TO XDCA-ADLAGOMR                           
103900                                  XDCA-ADGANG                             
104000                                  XDCA-ADPLATS                            
104100                                  XDCA-KDORDBEK                           
104200                                  XDCA-KVPREAVB                           
104300                                  XDCA-KVPRERO                            
104400                                  XDCA-TIREGDAT-OUT                       
104500                                  XDCA-TIREGTID-OUT                       
104600                                  XDCA-VKART-OUT                          
104700                                  XDCA-VKART-NTO                          
104800                                  XDCA-VLARTNTO                           
105000     MOVE ZERO                 TO                                         
105100                                  XDCA-KVOKS-DAG                          
105200                                  XDCA-KVOKS-BULK                         
105300                                                                          
105301      IF XDCA-DAPUBL NOT = 99999999                                       
105310         MOVE ZERO              TO XDCA-DAPUBL                            
105320      END-IF                                                              
105330                                                                          
105400     CALL W411XDCA USING OHUV-WDQ201 XDCA-W411XDCA                        
105500     XDCA-USEA-PCB                                                        
105600     XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                            
105700     XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                           
105800     XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                            
105900     XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                      
106000     XDCA-WDK7-3-PCB                                                      
106100                                                                          
106200* THIS IS JUST TO BE ABLE TO COMPARE THE RESULT WITH VALUES               
106300* FROM NDCA. IF VALU NOT = SPACE ORAD- VALUES SHULD BE OVERRIDDEN         
106400     IF XDCA-KDARTURS = SPACE                                             
106500       MOVE AREG-KDARTURS  TO XDCA-KDARTURS                               
106600     END-IF                                                               
106700     IF XDCA-VKART-NTO = ZERO                                             
106800       MOVE AREG-VKART-NTO TO XDCA-VKART-NTO                              
106900     END-IF                                                               
107000     IF XDCA-VLARTNTO = ZERO                                              
107100       MOVE AREG-VLARTNTO  TO XDCA-VLARTNTO                               
107200     END-IF                                                               
107300     .                                                                    
107400                                                                          
107500 HX-CHECK-DIFF SECTION.                                                   
107600     MOVE 'HX-CHECK-DIFF' TO CURRENT-SECTION                              
107700                                                                          
107800     IF  NDCA-KDORDBEK   = XDCA-KDORDBEK                                  
107900     AND NDCA-KVPREAVB   = XDCA-KVPREAVB                                  
108000     AND NDCA-ADLAGOMR   = XDCA-ADLAGOMR                                  
108100     AND NDCA-ADGANG     = XDCA-ADGANG                                    
108200     AND NDCA-ADPLATS    = XDCA-ADPLATS                                   
108300     AND NDCA-IDDC       = XDCA-IDDC-OUT                                  
108400     AND NDCA-IDDC-RO    = XDCA-IDDC-RO                                   
108500     AND NDCA-KDARTURS   = XDCA-KDARTURS                                  
108600     AND NDCA-KDOI       = XDCA-KDOI                                      
108700     AND NDCA-KVPRERO    = XDCA-KVPRERO                                   
108800     AND NDCA-VKART      = XDCA-VKART-OUT                                 
108900     AND NDCA-VKART-NTO  = XDCA-VKART-NTO                                 
109000     AND NDCA-VLARTNTO   = XDCA-VLARTNTO                                  
109100     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
109200     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
109300     AND XDCA-KVOKS-DAG     = NDCA-XDK7-KVOKS-DAG                         
109400     AND XDCA-KVOKS-BULK    = NDCA-XDK7-KVOKS-BULK                        
109500         MOVE NEJ TO DIFF-FLSVAR                                          
109600     ELSE                                                                 
109700        MOVE JA           TO DIFF-FLSVAR                                  
109800     END-IF                                                               
109900                                                                          
110000* ORDER LOG INFO                                                          
110100     IF DIFF-FLSVAR = JA AND IDPGM= 'W911SLDO'                            
110200       MOVE IDPGM         TO FIL-IDPGM                                    
110300       ACCEPT FIL-TIREGDAT FROM DATE                                      
110400       ACCEPT FIL-TIKLOCK FROM TIME                                       
110500       MOVE 1             TO FIL-IDSEKVNR                                 
110600       MOVE 'W414'        TO FIL-CT-IDSYSTEM                              
110700       MOVE 'A'           TO FIL-CT-IDVTYP                                
110800       MOVE 'XDC'         TO FIL-CT-IDPTYP                                
110900                                                                          
111000*   ORDER LINE INFO                                                       
111100       MOVE SLDO-IDDISTR-IN  TO DIFF-IDDISTR                              
111200       MOVE SLDO-IDKUNDNR-IN TO DIFF-IDKUNDNR                             
111300       MOVE ZERO           TO DIFF-IDORDNR5                               
111400       MOVE SLDO-KDORDKL-IN TO DIFF-KDORDKL                               
111500       MOVE AREG-IDARTNR   TO DIFF-IDARTNR                                
111600       MOVE KVAN-KVBEART-Q-UT                                             
111700                           TO DIFF-KVBEART-Q                              
111800       MOVE AREG-IDDC      TO DIFF-IDDC                                   
111900       MOVE 'SLDO'         TO DIFF-IDSYSTEM                               
112000                                                                          
112100*   NDCA INFO                                                             
112200       MOVE NDCA-XDK7-KVOKS-DAG TO DIFF-KVOKS-DAG-NDCA                    
112300       MOVE NDCA-XDK7-KVOKS-BULK TO DIFF-KVOKS-BULK-NDCA                  
112400       MOVE NDCA-KDORDBEK TO DIFF-KDORDBEK-NDCA                           
112500       MOVE NDCA-KVPREAVB TO DIFF-KVPREAVB-NDCA                           
112600       MOVE NDCA-ADLAGOMR TO DIFF-ADLAGOMR-NDCA                           
112700       MOVE NDCA-ADGANG   TO DIFF-ADGANG-NDCA                             
112800       MOVE NDCA-ADPLATS  TO DIFF-ADPLATS-NDCA                            
112900       MOVE NDCA-IDDC     TO DIFF-IDDC-NDCA                               
113000       MOVE NDCA-IDDC-RO  TO DIFF-IDDC-RO-NDCA                            
113100       MOVE NDCA-KDARTURS TO DIFF-KDARTURS-NDCA                           
113200       MOVE NDCA-KDOI     TO DIFF-KDOI-NDCA                               
113300       MOVE NDCA-KVPRERO  TO DIFF-KVPRERO-NDCA                            
113400       MOVE NDCA-VKART    TO DIFF-VKART-NDCA                              
113500       MOVE NDCA-VKART-NTO TO DIFF-VKART-NTO-NDCA                         
113600       MOVE NDCA-VLARTNTO TO DIFF-VLARTNTO-NDCA                           
113700       MOVE NDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-NDCA                     
113800                                                                          
113900*   XDCA INFO                                                             
114000       MOVE XDCA-KVOKS-DAG TO DIFF-KVOKS-DAG-XDCA                         
114100       MOVE XDCA-KVOKS-BULK TO DIFF-KVOKS-BULK-XDCA                       
114200       MOVE XDCA-KDORDBEK TO DIFF-KDORDBEK-XDCA                           
114300       MOVE XDCA-KVPREAVB TO DIFF-KVPREAVB-XDCA                           
114400       MOVE XDCA-ADLAGOMR TO DIFF-ADLAGOMR-XDCA                           
114500       MOVE XDCA-ADGANG   TO DIFF-ADGANG-XDCA                             
114600       MOVE XDCA-ADPLATS  TO DIFF-ADPLATS-XDCA                            
114700       MOVE XDCA-IDDC-OUT TO DIFF-IDDC-XDCA                               
114800       MOVE XDCA-IDDC-RO  TO DIFF-IDDC-RO-XDCA                            
114900       MOVE XDCA-KDARTURS TO DIFF-KDARTURS-XDCA                           
115000       MOVE XDCA-KDOI     TO DIFF-KDOI-XDCA                               
115100       MOVE XDCA-KVPRERO  TO DIFF-KVPRERO-XDCA                            
115200       MOVE XDCA-VKART-OUT TO DIFF-VKART-XDCA                             
115300       MOVE XDCA-VKART-NTO TO DIFF-VKART-NTO-XDCA                         
115400       MOVE XDCA-VLARTNTO TO DIFF-VLARTNTO-XDCA                           
115500       MOVE XDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-XDCA                     
115600                                                                          
115700       PERFORM IMS-ISRT-WDR601                                            
115800       IF SEGMENT-FINNS-REDAN                                             
115900          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
116000             ADD 1 TO FIL-IDSEKVNR                                        
116100             PERFORM IMS-ISRT-WDR601                                      
116200          END-PERFORM                                                     
116300       END-IF                                                             
116400     END-IF                                                               
116500     .                                                                    
116600     EJECT                                                                
116700     .                                                                    
116800                                                                          
116900 I-KOLLA-RANSONERING SECTION.                                             
117000     MOVE 'I-KOLLA-RANS'  TO CURRENT-SECTION                              
117100                                                                          
117200     MOVE SPACE                TO RANS-BERADREF                           
117300     MOVE NEJ                  TO RANS-FLEMBORD                           
117400     MOVE NEJ                  TO RANS-FLFORBI                            
117500     MOVE NEJ                  TO RANS-FLORDSPE                           
117600     MOVE NEJ                  TO RANS-FLOVRLEV                           
117700     MOVE +0                   TO RANS-IDKAMPRF                           
117800     MOVE AREG-IDARTNR         TO RANS-IDARTNR                            
117900     MOVE SPACE                TO RANS-IDLEVNR                            
118000     MOVE W-IDRFTAB            TO RANS-IDRFTAB                            
118100     MOVE +0                   TO RANS-TIRODAT                            
118200     MOVE SLDO-KDORDKL-IN      TO RANS-KDORDKL                            
118300     MOVE +1                   TO RANS-KDORDBEH                           
118400     MOVE KVAN-KVBEART-Q-UT    TO RANS-KVBEART-Q                          
118500     MOVE +0                   TO RANS-KDTPOTYP                           
118600     MOVE AREG-KDERS           TO RANS-KDERS                              
118700     MOVE AREG-KVLS            TO RANS-KVLS                               
118800     MOVE AREG-KVPB-SATS       TO RANS-KVPB-SATS                          
118900     MOVE AREG-KVPB-SEP        TO RANS-KVPB-SEP                           
119000     MOVE AREG-REDIRLEV        TO RANS-REDIRLEV                           
119100     MOVE AREG-KVRESS          TO RANS-KVRESS                             
119200     MOVE AREG-KVSPANT         TO RANS-KVSPANT                            
119300     MOVE AREG-KVUTRS          TO RANS-KVUTRS                             
119400     MOVE AREG-TIDISPIN        TO RANS-TIDISPIN                           
119500                                                                          
119600     IF AREG-KDPRODSL = 71 OR 72 OR 73 OR 74                              
119700        MOVE 1                 TO RANS-RERF-RAD-UT                        
119800        MOVE ZERO              TO RANS-SUTPO-PB-UT                        
119900                                  RANS-SUTPO-EJPB-UT                      
120000                                  RANS-RERF-ART-UT                        
120100     ELSE                                                                 
120200        CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                   
120300                            RANS-ARTM-PCB RANS-ARTS-PCB                   
120400                                                                          
120500     END-IF                                                               
120600     .                                                                    
120700                                                                          
120800                                                                          
120900 J-KOLLA-STORT-UTTAG SECTION.                                             
121000     MOVE 'J-KOLLA-STORT'  TO CURRENT-SECTION                             
121100                                                                          
121200     MOVE 'VDI '               TO STOR-IDSYSTEM                           
121300     MOVE SPACE                TO STOR-IDLEVNR                            
121400     MOVE '0000000   '         TO STOR-IDKUNDRF-RO                        
121500     MOVE SPACE                TO STOR-KDPROTYP                           
121600     MOVE SPACE                TO STOR-BERADREF                           
121700     MOVE NEJ                  TO STOR-FLFORBI                            
121800     MOVE NEJ                  TO STOR-FLORDSPE                           
121900     MOVE NEJ                  TO STOR-FLOVRLEV                           
122000     MOVE SLDO-KDORDKL-IN      TO STOR-KDORDKL                            
122100     MOVE AREG-KDERS           TO STOR-KDERS                              
122200     MOVE AREG-KDVVKL          TO STOR-KDVVKL                             
122300     MOVE KVAN-KVBEART-Q-UT    TO STOR-KVBEART-Q                          
122400     MOVE AREG-KVPB-SEP        TO STOR-KVPB-SEP                           
122500     MOVE AREG-KVSLUTKP        TO STOR-KVSLUTKP                           
122600     MOVE RANS-RERF-ART-UT     TO STOR-RERF-ART                           
122700     MOVE +0                   TO STOR-IDKAMPRF                           
122800     MOVE SLDO-IDDISTR-IN      TO STOR-IDDISTR                            
122900     MOVE AREG-KDPRODSL        TO STOR-KDPRODSL                           
123000                                                                          
123100     CALL W411STOR USING STOR-W411STOR                                    
123200                                                                          
123300     IF STOR-KDORDBEK > +0                                                
123400        MOVE '080'             TO SLDO-IDMFSMED                           
123500        MOVE NEJ               TO ALLT-SW                                 
123600        MOVE STOR-KDORDBEK   TO SLDO-KDORDBEK                             
123700     END-IF                                                               
123800     .                                                                    
123900                                                                          
124000                                                                          
124100 K-KOLLA-CDC SECTION.                                                     
124200     MOVE 'K-KOLLA-CDC' TO CURRENT-SECTION                                
124300                                                                          
124400     MOVE JA                   TO CDCA-FLFINLV-IN                         
124500     MOVE NEJ                  TO CDCA-FLFORBI-IN                         
124600     MOVE NEJ                  TO CDCA-FLORDSPE-IN                        
124700     MOVE NEJ                  TO CDCA-FLOVRLEV-IN                        
124800     MOVE GMT-FLPRELRO         TO CDCA-FLPRELRO-IN                        
124900     MOVE JA                   TO CDCA-FLRESTN-IN                         
125000     MOVE JA                   TO CDCA-FLSLATT-IN                         
125100     MOVE AREG-IDARTNR         TO CDCA-IDARTNR-IN                         
125200     MOVE SLDO-IDDISTR-IN      TO CDCA-IDDISTR-IN                         
125300     MOVE WS-IDDC              TO CDCA-IDDC-IN                            
125400     MOVE +0                   TO CDCA-IDKAMPRF-IN                        
125500     MOVE '0000000   '         TO CDCA-IDKUNDRF-RO-IN                     
125600     MOVE 'VDI '               TO CDCA-IDSYSTEM-IN                        
125700     MOVE SPACE                TO CDCA-IDLEVNR-IN                         
125800     MOVE AREG-KDERS           TO CDCA-KDERS-IN                           
125900     MOVE +0                   TO CDCA-KDKVBRYT-IN                        
126000     MOVE SPACE                TO CDCA-KDPROTYP-IN                        
126100*    MOVE KREG-KDORDKL         TO CDCA-KDORDKL-IN                         
126200     MOVE 1                    TO CDCA-KDORDKL-IN                         
126300     MOVE AREG-KDPRODSL        TO CDCA-KDPRODSL-IN                        
126400     MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                          
126500     MOVE +0                   TO CDCA-KDTPOTYP-IN                        
126600     MOVE AREG-KDUART          TO CDCA-KDUART-IN                          
126700     MOVE KVAN-KVBEART-Q-UT    TO CDCA-KVBEART-IN                         
126800     MOVE KVAN-KVBEART-Q-UT    TO CDCA-KVBEART-Q-IN                       
126900     MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                       
127000     MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                       
127100     MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                        
127200     MOVE RANS-RERF-RAD-UT     TO CDCA-RERF-RAD-IN                        
127300     MOVE RANS-RERF-ART-UT     TO CDCA-RERF-ART-IN                        
127400     MOVE GMT-RESLATT          TO CDCA-RESLATT-IN                         
127500     MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                       
127600     MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                       
127700     MOVE AREG-KVLS            TO CDCA-KVLS-IN                            
127800     MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                          
127900     MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                         
128000     MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                          
128100     MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                    
128200     MOVE SLDO-IDKUNDNR-IN     TO CDCA-IDKUNDNR-IN                        
128300     MOVE SPACE                TO CDCA-BERADREF-IN                        
128400     MOVE +2                   TO CDCA-KDCALL                             
128500                                                                          
128600     CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                      
128700                                       CDCA-INLB-PCB                      
128800                                       CDCA-WDB2-PCB                      
128900                                       CDCA-WDC1-PCB                      
129000     IF CDCA-KDORDBEK-UT = ZERO                                           
129100        MOVE '010'             TO SLDO-IDMFSMED                           
129200        MOVE CDCA-KVBEART-UT   TO SLDO-KVAVBART                           
129300        MOVE WS-IDDC           TO SLDO-IDDC                               
129400        MOVE JA                TO SALDO-SW                                
129500        IF CURR-DC-IX > 1                                                 
129600           MOVE '15'          TO SLDO-KDORDBEK                            
129700        END-IF                                                            
129800     ELSE                                                                 
129900        MOVE CDCA-KDORDBEK-UT  TO SLDO-KDORDBEK                           
130000        MOVE '080'             TO SLDO-IDMFSMED                           
130100        MOVE AREG-TIDISPIN     TO SLDO-TIDISPIN                           
130200        MOVE WS-IDDC           TO SLDO-IDDC                               
130300     END-IF                                                               
130400     .                                                                    
130500                                                                          
130600                                                                          
130700 L-KOLLA-DIRLEV-DISP SECTION.                                             
130800     MOVE 'L-KOLLA-DIRLEV' TO CURRENT-SECTION                             
130900                                                                          
131000     IF DC21-FOUND                                                        
131100        MOVE WS-IDLEVNR      TO W-IDLEVNR-WDF1                            
131200     ELSE                                                                 
131300        MOVE DLEV-IDLEVNR-UT TO W-IDLEVNR-WDF1                            
131400     END-IF                                                               
131500     MOVE SLDO-IDDISTR-IN TO W-IDDISTR-WDF1                               
131600     MOVE SLDO-IDKUNDNR-IN TO W-IDKUNDNR-WDF1                             
131700     MOVE SLDO-KDORDKL-IN TO W-KDORDKL-WDF1                               
131800     PERFORM IMS-GU-WDF118                                                
131900     IF SEGMENT-SAKNAS                                                    
132000        MOVE 999999 TO W-IDKUNDNR-WDF1                                    
132100        PERFORM IMS-GU-WDF118                                             
132200     END-IF                                                               
132300     IF SEGMENT-SAKNAS                                                    
132400        MOVE 9999 TO W-IDDISTR-WDF1                                       
132500        PERFORM IMS-GU-WDF118                                             
132600     END-IF                                                               
132700     IF SEGMENT-FINNS                                                     
132800        MOVE DSTY-KVDAGAR-DIFF  TO WS-KVDAGAR-DIFF                        
132900        IF SLDO-TIDISPIN = ZERO                                           
133000           MOVE WC-CDC-SE       TO WORK-IDDC                              
133100           MOVE +002            TO WORK-KDCALL                            
133200           COMPUTE WORK-KVWORKD = DSTY-KVDAGAR-LEV + 1                    
133300           MOVE CURRENT-DATE    TO WORK-TIAAMMDD-FOM                      
133400           CALL WORKDAY      USING WORK-KDCALL                            
133500                                      WORK-DATE-AREA                      
133600                                      WORK-KDSVAR                         
133700           IF WORK-KDSVAR-FEL                                             
133800              MOVE 'L-KOLLA-DIRLEV-DISP, DATUM SAKNAS I WORKDAY'          
133900                                     TO ERROR-TEXT                        
134000              CALL ABEND USING RKOD-ABEND-MED-DUMP                        
134100           ELSE                                                           
134200              MOVE WORK-TIAAMMDD-TOM TO SLDO-TIDISPIN                     
134300           END-IF                                                         
134400        END-IF                                                            
134500     ELSE                                                                 
134600        MOVE ZERO               TO WS-KVDAGAR-DIFF                        
134700     END-IF                                                               
134800     IF SLDO-TIDISPIN > ZERO                                              
134900        MOVE 002                     TO WORK-KDCALL                       
135000        MOVE WC-CDC-SE               TO WORK-IDDC                         
135100        MOVE SLDO-TIDISPIN           TO WORK-TIAAMMDD-FOM                 
135200        COMPUTE WORK-KVWORKD = WS-KVDAGAR-DIFF + 1                        
135300        CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA                     
135400                           WORK-KDSVAR                                    
135500        IF WORK-KDSVAR-OK                                                 
135600           MOVE WORK-TIAAMMDD-TOM TO SLDO-TIKLAR                          
135700        END-IF                                                            
135800     END-IF                                                               
135900     .                                                                    
136000                                                                          
136100                                                                          
136200 M-KOLLA-SALDO-DISP  SECTION.                                             
136300     MOVE 'M-KOLLA-SALDO ' TO CURRENT-SECTION                             
136400                                                                          
136500     MOVE SLDO-IDDC               TO WS-IDDC                              
136600     IF NDC OR LDC-CN                                                     
136700        PERFORM MA-HAEMTA-TIBERANK                                        
136800        IF ETA-SVAR-OK = JA                                               
136900           MOVE ETA-TIAAMMDD-SVAR TO SLDO-TIDISPIN                        
137000        END-IF                                                            
137100     ELSE                                                                 
137200        MOVE AREG-TIDISPIN        TO SLDO-TIDISPIN                        
137300     END-IF                                                               
137400                                                                          
137500     IF SLDO-TIDISPIN > ZERO                                              
137600        MOVE SLDO-IDDC            TO W-IDDC-WDB3                          
137700                                     W-IDDC-WDB3-DEF                      
137800        MOVE SLDO-IDDISTR-IN      TO W-IDDISTR-WDB3                       
137900                                     W-IDDISTR-WDB3-DEF                   
138000        MOVE SLDO-IDKUNDNR-IN     TO W-IDKUNDNR-WDB3                      
138100        PERFORM IMS-GU-WDB301                                             
138200        IF SEGMENT-FINNS                                                  
138300           MOVE 002                  TO WORK-KDCALL                       
138400           MOVE SLDO-IDDC            TO WORK-IDDC                         
138500           MOVE SLDO-TIDISPIN        TO WORK-TIAAMMDD-FOM                 
138600           MOVE DC-KVDAGAR-TRP-DAY   TO WORK-KVWORKD                      
138700           CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA                  
138800                              WORK-KDSVAR                                 
138900           IF WORK-KDSVAR-OK                                              
139000              MOVE WORK-TIAAMMDD-TOM TO SLDO-TIKLAR                       
139100           END-IF                                                         
139200        END-IF                                                            
139300     END-IF                                                               
139400     .                                                                    
139500     EJECT                                                                
139600 MA-HAEMTA-TIBERANK SECTION.                                              
139700     MOVE 'MA-HAEMTA-TIBERANK' TO CURRENT-SECTION                         
139800                                                                          
139900     MOVE '612'                TO ETA-KDCALL                              
140000     MOVE SLDO-IDDC            TO ETA-IDDC-REC                            
140100     MOVE AREG-IDARTNR         TO ETA-IDARTNR                             
140200     MOVE AREG-IDLEVNR         TO ETA-IDLEVNR                             
140300     MOVE ZERO                 TO ETA-KDFRAKT                             
140400     MOVE WS-ETA-DATUM         TO ETA-TIAAMMDD-ANROP                      
140500     MOVE 20                   TO ETA-TISEKEL-ANROP                       
140600                                                                          
140700     CALL W218ETA  USING ETA-W218LETA                                     
140800                         ETA-ARTC-PCB ETA-WDK7-PCB                        
140900                         ETA-INLC-PCB ETA-LEVA-PCB                        
141000                         ETA-WDB6-PCB ETA-WDD9-PCB                        
141100     .                                                                    
141200                                                                          
141300                                                                          
141400 N-CALC-CUST-ETA    SECTION.                                              
141500     MOVE 'N-CALC-ETA   ' TO CURRENT-SECTION                              
141600                                                                          
141700     MOVE ZERO               TO WS-KVDAGAR-DIFF                           
141800     IF DIRLEV                                                            
141900        IF DC21-FOUND                                                     
142000           MOVE WS-IDLEVNR   TO W-IDLEVNR-WDF1                            
142100        ELSE                                                              
142200           MOVE DLEV-IDLEVNR-UT TO W-IDLEVNR-WDF1                         
142300        END-IF                                                            
142400        MOVE SLDO-IDDISTR-IN TO W-IDDISTR-WDF1                            
142500        MOVE SLDO-IDKUNDNR-IN TO W-IDKUNDNR-WDF1                          
142600        MOVE SLDO-KDORDKL-IN TO W-KDORDKL-WDF1                            
142700        PERFORM IMS-GU-WDF118                                             
142800        IF SEGMENT-SAKNAS                                                 
142900           MOVE 999999 TO W-IDKUNDNR-WDF1                                 
143000           PERFORM IMS-GU-WDF118                                          
143100        END-IF                                                            
143200        IF SEGMENT-SAKNAS                                                 
143300           MOVE 9999 TO W-IDDISTR-WDF1                                    
143400           PERFORM IMS-GU-WDF118                                          
143500        END-IF                                                            
143600        IF SEGMENT-FINNS                                                  
143700           COMPUTE WORK-KVWORKD = DSTY-KVDAGAR-DIFF + 1                   
143800        END-IF                                                            
143810        MOVE WC-CDC-SE             TO SLDO-IDDC                           
143900     ELSE                                                                 
144000        MOVE SLDO-IDDC             TO W-IDDC-WDB3                         
144100                                      W-IDDC-WDB3-DEF                     
144200        MOVE SLDO-IDDISTR-IN       TO W-IDDISTR-WDB3                      
144300                                      W-IDDISTR-WDB3-DEF                  
144400        MOVE SLDO-IDKUNDNR-IN      TO W-IDKUNDNR-WDB3                     
144500        PERFORM IMS-GU-WDB301                                             
144600        IF SEGMENT-FINNS                                                  
144700           COMPUTE WORK-KVWORKD = DC-KVDAGAR-TRP-DAY + 1                  
144800        END-IF                                                            
144900     END-IF                                                               
145000     MOVE SLDO-IDDC             TO WORK-IDDC                              
145100     MOVE +002                  TO WORK-KDCALL                            
145200     MOVE CURRENT-DATE          TO WORK-TIAAMMDD-FOM                      
145300     CALL WORKDAY            USING WORK-KDCALL                            
145400                                WORK-DATE-AREA                            
145500                                WORK-KDSVAR                               
145600     IF WORK-KDSVAR-FEL                                                   
145700        MOVE 'N-CALC-CUST-ETA, DATUM SAKNAS I WORKDAY'                    
145800                               TO ERROR-TEXT                              
145900        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
146000     ELSE                                                                 
146100        MOVE WORK-TIAAMMDD-TOM TO SLDO-TIKLAR                             
146200     END-IF                                                               
146300     .                                                                    
146400                                                                          
146500                                                                          
146600 O-REDIGERA-UT-AREA SECTION.                                              
146700     MOVE 'O-REDIGERA-UT' TO CURRENT-SECTION                              
146800                                                                          
146900     IF WS-KDORDBEK-SUPERS > ZERO                                         
147000        IF  SALDO-FINNS                                                   
147011           IF SLDO-KDORDBEK = 15                                          
147012              CONTINUE                                                    
147013           ELSE                                                           
147014              MOVE ZERO             TO SLDO-KDORDBEK                      
147015           END-IF                                                         
147020        ELSE                                                              
147030           MOVE SPACE               TO SLDO-IDDC                          
147040           MOVE WS-KDORDBEK-SUPERS  TO SLDO-KDORDBEK                      
147050        END-IF                                                            
147060     END-IF                                                               
147070     IF (SLDO-KDORDBEK = ZERO OR 15) AND                                  
147080        KVAN-KDORDBEK-UT > ZERO                                           
147090*       VI KAN LEVERERA MEN ANTALET HAR KVANTANPASSATS                    
147100        MOVE KVAN-KDORDBEK-UT  TO SLDO-KDORDBEK                           
147200        MOVE KVAN-KVBEART-Q-UT TO SLDO-KVAVBART                           
147300     END-IF                                                               
147400                                                                          
147500     IF SLDO-TIDISPIN > ZERO AND SLDO-TIDISPIN < CURRENT-DATE             
147600        MOVE ZERO     TO SLDO-TIDISPIN                                    
147700                         SLDO-TIKLAR                                      
147800     END-IF                                                               
147900                                                                          
147904     IF SPAR-KDORDBEK = 67                                                
147905        IF NDC                                                            
147906          IF XDCA-KDORDBEK = 55 OR 99 OR ZERO                             
147907             CONTINUE                                                     
147908          ELSE                                                            
147909            MOVE SPAR-KDORDBEK TO SLDO-KDORDBEK                           
147910          END-IF                                                          
147911        ELSE                                                              
147912          MOVE SPAR-KDORDBEK   TO SLDO-KDORDBEK                           
147913        END-IF                                                            
147914     END-IF                                                               
147920                                                                          
148000     IF SLDO-KDORDBEK > ZERO                                              
148100        MOVE SLDO-KDORDBEK TO W-KDORDBEK                                  
148200        PERFORM IMS-GU-XXKJ11                                             
148300        IF SEGMENT-FINNS                                                  
148400           MOVE 4522-TEORDBEK                                             
148500                           TO SLDO-TEORDBEK                               
148600        END-IF                                                            
148700        IF W-IDSKYLT-WDGX = 'GB'                                          
148800           MOVE 4522-TEORDBEK                                             
148900                           TO SLDO-TEORDBEK-ENG                           
149000        ELSE                                                              
149100           MOVE 'GB'       TO W-IDSKYLT-WDGX                              
149200           PERFORM IMS-GU-XXKJ11                                          
149300           IF SEGMENT-FINNS                                               
149400              MOVE 4522-TEORDBEK                                          
149500                           TO SLDO-TEORDBEK-ENG                           
149600           END-IF                                                         
149700        END-IF                                                            
149800        IF SLDO-KDORDBEK = 41 OR 61                                       
149900           PERFORM IMS-GU-WDD701                                          
150000           IF SEGMENT-FINNS                                               
150100              PERFORM IMS-GNP-WDD702                                      
150200              IF SEGMENT-FINNS                                            
150300                 MOVE IDARTNR-TILLK                                       
150400                              TO SLDO-IDARTNR-TILLK                       
150500                 PERFORM IMS-GNP-WDD702                                   
150600                 IF SEGMENT-FINNS                                         
150700                    MOVE ZERO TO SLDO-IDARTNR-TILLK                       
150800                 END-IF                                                   
150900              END-IF                                                      
151000           END-IF                                                         
151100        END-IF                                                            
151200     END-IF                                                               
151300     .                                                                    
151400*****************************************************************         
151500*THIS SECTION CHECKS ONLY FOR PARTS DELIVERED FROM DC21                   
151600*IF STOCKS ARE IN DC21,DISPLAY QTY AVAILABLE AND ETA                      
151700*IF NO STOCKS IN DC21,NO QTY AVAILABE AND NO ETA                          
151800*IF REQUESTED PART IS NOT INSERTED IN DC21,CHECK THE SUPPLIER             
151900*****************************************************************         
152000 P-GET-DC21-DDGS SECTION.                                                 
152100     MOVE 'P-GET-DC21-DD' TO CURRENT-SECTION                              
152200                                                                          
152300     MOVE LOW-VALUE              TO W-WDF2A1KY-MIN-X                      
152400     MOVE HIGH-VALUE             TO W-WDF2A1KY-MAX-X                      
152500     MOVE CURRENT-DATE           TO W-DATE                                
152600     MOVE W-TODAY-DATE           TO W-DASTADAT                            
152700     MOVE AREG-IDARTNR           TO W-IDARTNR-MIN                         
152800                                    W-IDARTNR-MAX                         
152900     MOVE SLDO-IDDISTR-IN        TO W-IDDISTR                             
153000                                                                          
153100     MOVE +0                     TO W-IDKUNDNR-FOM                        
153200     MOVE +9999999               TO W-IDKUNDNR-TOM                        
153300     SET DIRLEV                  TO TRUE                                  
153400                                                                          
153500     PERFORM IMS-GU-WDF2A                                                 
153600     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
153700                   BASEN-SLUT     OR                                      
153800                   SUPPLIER-FOUND OR                                      
153900                   SALDO-FINNS                                            
154000        MOVE SEQA-IDLEVNR         TO W-IDLEVNR-WDF2                       
154100        MOVE SEQA-IDDIRGRP        TO W-IDDIRGRP-WDF2                      
154200        PERFORM IMS-GU-WDF201                                             
154300        IF SEGMENT-FINNS                                                  
154400          IF W-TODAY-DATE >= LEV-DASTADAT                                 
154500             MOVE LEV-IDLEVNR     OF LEV-WDF201                           
154600                                  TO WS-IDLEVNR                           
154700             MOVE SLDO-IDKUNDNR-IN TO W-IDKUNDNR-FOM                      
154800                                     W-IDKUNDNR-TOM                       
154900             PERFORM PB-FIND-WDF2-DIST-CUS                                
155000             IF SUPPLIER-FOUND                                            
155100                PERFORM PA-CHK-STOCKBAL-AND-DATE                          
155200             END-IF                                                       
155300           ELSE                                                           
155400             MOVE +0              TO W-IDKUNDNR-FOM                       
155500             MOVE +9999999        TO W-IDKUNDNR-TOM                       
155600              PERFORM IMS-GN-WDF2A                                        
155700           END-IF                                                         
155800        END-IF                                                            
155900        MOVE +0                   TO W-IDKUNDNR-FOM                       
156000        MOVE +9999999             TO W-IDKUNDNR-TOM                       
156100        PERFORM IMS-GN-WDF2A                                              
156200     END-PERFORM                                                          
156300     IF NOT SUPPLIER-FOUND                                                
156400         MOVE NEJ                 TO DC21-SW                              
156500                                     DIRLEV-SW                            
156600     END-IF                                                               
156700     .                                                                    
156800                                                                          
156900 PA-CHK-STOCKBAL-AND-DATE SECTION.                                        
157000     MOVE 'PA-CHK-STOCKB'         TO CURRENT-SECTION                      
157100                                                                          
157200     COMPUTE DIR-INDX = SLDO-KDORDKL-IN + 1                               
157300     SET DDGS-IX                  TO DIR-INDX                             
157400     MOVE DDGS-IDDC(DDGS-IX)      TO ALT3-WS-IDDC                         
157500     SET NT-DC-INSERTED           TO TRUE                                 
157600     PERFORM PAC-FIND-DDGS-IDDC                                           
157700                                                                          
157800     IF  DDGS-IDDC(DDGS-IX) > '  '                                        
157900     AND (ALT3-SDC OR ALT3-LDC)                                           
158000     AND DDGS-IDDC-FND                                                    
158100        SET DC21-FOUND            TO TRUE                                 
158200        PERFORM PAA-GET-SDCLEV-BALANCE                                    
158300        MOVE W-IDDC               TO SLDO-IDDC                            
158400        IF SDCLEV-OK                                                      
159000            MOVE KVAN-KVBEART-Q-UT TO SLDO-KVAVBART                       
159100            MOVE JA               TO SALDO-SW                             
159200            MOVE '010'            TO SLDO-IDMFSMED                        
159300        ELSE                                                              
159400* IF NO BALANCE FROM DC21,DC= 21 AND ETD=BLANK & QTY UNAVAILABLE          
159500            IF DC-INSERTED                                                
159600               MOVE ZEROES        TO SLDO-KVAVBART                        
159700               MOVE NEJ           TO SALDO-SW                             
159800               MOVE '080'         TO SLDO-IDMFSMED                        
159900               MOVE '95'          TO SLDO-KDORDBEK                        
160000            ELSE                                                          
160100               PERFORM PAB-CHK-SUPPLIER                                   
160200            END-IF                                                        
160300        END-IF                                                            
160400     END-IF                                                               
160500     .                                                                    
160600                                                                          
160700 PAA-GET-SDCLEV-BALANCE SECTION.                                          
160800     MOVE 'PAA-GET-SDCLE'         TO CURRENT-SECTION                      
160900                                                                          
161000     MOVE DDGS-IDDC(DDGS-IX)      TO W-IDDC                               
161100     PERFORM IMS-GU-WDK711                                                
161200     IF SEGMENT-FINNS                                                     
161300        SET DC-INSERTED           TO TRUE                                 
161400        IF SLAG-KVAKS-SDC < 0                                             
161500           MOVE 0                 TO W-KVAKS-SDC                          
161600        ELSE                                                              
161700           MOVE SLAG-KVAKS-SDC    TO W-KVAKS-SDC                          
161800        END-IF                                                            
161900        IF SLAG-KVOKS-DAG < 0                                             
162000           MOVE 0                 TO W-KVOKS-DAG                          
162100        ELSE                                                              
162200           MOVE SLAG-KVOKS-DAG    TO W-KVOKS-DAG                          
162300        END-IF                                                            
162400        IF SLAG-KVOKS-BULK < 0                                            
162500           MOVE 0                 TO W-KVOKS-BULK                         
162600        ELSE                                                              
162700           MOVE SLAG-KVOKS-BULK   TO W-KVOKS-BULK                         
162800        END-IF                                                            
162900                                                                          
163000        COMPUTE W-DISP = SLAG-KVLS                                        
163100                       + W-KVAKS-SDC                                      
163200                       - W-KVOKS-DAG                                      
163300                       - W-KVOKS-BULK                                     
163400        IF W-DISP > 0                                                     
163500           COMPUTE W-DISP = W-DISP                                        
163600                          - SLAG-KVUTRS                                   
163700                          - SLAG-KVSPARR-KVAL                             
163800        END-IF                                                            
163900        IF W-DISP < 0                                                     
164000           MOVE 0                 TO W-DISP                               
164100        END-IF                                                            
164200        IF KVAN-KVBEART-Q-UT > W-DISP OR                                  
164300           SLAG-KDLEVSP > 0                                               
164400           MOVE NEJ               TO W-SDCLEV-SW                          
164500        ELSE                                                              
164600          IF DDGS-MIN-QTY(DDGS-IX)  NOT = +0                              
164700            IF KVAN-KVBEART-Q-UT >= DDGS-MIN-QTY(DDGS-IX)                 
164800              MOVE JA             TO W-SDCLEV-SW                          
164900            ELSE                                                          
165000              MOVE NEJ            TO W-SDCLEV-SW                          
165100            END-IF                                                        
165200          ELSE                                                            
165300            MOVE NEJ              TO W-SDCLEV-SW                          
165400          END-IF                                                          
165500        END-IF                                                            
165600     ELSE                                                                 
165700       SET NT-DC-INSERTED         TO TRUE                                 
165800       MOVE NEJ                   TO W-SDCLEV-SW                          
165900     END-IF                                                               
166000     .                                                                    
166100                                                                          
166200 PAB-CHK-SUPPLIER SECTION.                                                
166300     MOVE 'PAB-CHK-SUPPL'         TO CURRENT-SECTION                      
166400                                                                          
166500     IF DDGS-MIN-QTY(DDGS-IX) NOT = +0                                    
166600        IF  KVAN-KVBEART-Q-UT >= DDGS-MIN-QTY(DDGS-IX)                    
166700           MOVE JA                TO SALDO-SW                             
166800           PERFORM PABA-DDGS-PROCESSING                                   
166900           IF KVAN-KVBEART-Q-UT <= SEQA-KVLS-DLEV                         
167000              MOVE KVAN-KVBEART-Q-UT TO SLDO-KVAVBART                     
167100              MOVE '010'          TO SLDO-IDMFSMED                        
167200           ELSE                                                           
167300              IF SLDO-KVAVBART < 0                                        
167400                 MOVE ZEROES      TO SLDO-KVAVBART                        
167500              END-IF                                                      
167600              MOVE ZEROES         TO SLDO-TIKLAR                          
167700              MOVE NEJ            TO SALDO-SW                             
167800              MOVE '080'          TO SLDO-IDMFSMED                        
167900              MOVE '95'           TO SLDO-KDORDBEK                        
168000           END-IF                                                         
168100        ELSE                                                              
168200*WHAT HAPPENS WHEN USER QTY<MIN DIRECT DELIVERY QTY,CODE 21               
168300           MOVE NEJ               TO SALDO-SW                             
168400           MOVE ZEROES            TO SLDO-KVAVBART                        
168500           MOVE '080'             TO SLDO-IDMFSMED                        
168600           MOVE '21'              TO SLDO-KDORDBEK                        
168700        END-IF                                                            
168800     ELSE                                                                 
168900*WHAT HAPPENS WHEN MIN DIRECT DELIVERY QTY = 0,CODE 21                    
169000        MOVE NEJ                  TO SALDO-SW                             
169100        MOVE '21'                 TO SLDO-KDORDBEK                        
169200     END-IF                                                               
169300     .                                                                    
169400 PABA-DDGS-PROCESSING SECTION.                                            
169500     MOVE 'PABA-DDGS-PRO'         TO CURRENT-SECTION                      
169600     MOVE WS-IDLEVNR              TO W-IDLEVNR-WDF1                       
169700     PERFORM IMS-GU-WDF101                                                
169800     IF SEGMENT-FINNS                                                     
169900       IF SEGMENT-FINNS                                                   
170000         MOVE SLDO-KDORDKL-IN     TO W-KDORDKL-WDF1                       
170100         MOVE W-IDDISTR           TO W-IDDISTR-WDF1                       
170200         MOVE SLDO-IDKUNDNR-IN    TO W-IDKUNDNR-WDF1                      
170300         PERFORM IMS-GU-WDF118                                            
170400         IF SEGMENT-SAKNAS                                                
170500           MOVE 999999            TO W-IDKUNDNR-WDF1                      
170600           PERFORM IMS-GU-WDF118                                          
170700         END-IF                                                           
170800         IF SEGMENT-SAKNAS                                                
170900           MOVE 9999              TO W-IDDISTR-WDF1                       
171000           MOVE 999999            TO W-IDKUNDNR-WDF1                      
171100           PERFORM IMS-GU-WDF118                                          
171200         END-IF                                                           
171300         IF SEGMENT-FINNS                                                 
171400            MOVE DSTY-IDDC        TO SLDO-IDDC                            
171500         END-IF                                                           
171600       END-IF                                                             
171700     ELSE                                                                 
171800        MOVE NEJ                  TO SALDO-SW                             
171900        MOVE '21'                 TO SLDO-KDORDBEK                        
172000     END-IF                                                               
172100     .                                                                    
172200                                                                          
172210 PAC-FIND-DDGS-IDDC    SECTION.                                           
172220     MOVE 'PAC-FIND-DDG-'         TO CURRENT-SECTION                      
172230                                                                          
172240     MOVE 1                       TO IDDC-IX                              
172250     MOVE NEJ                     TO DDGS-IDDC-SW                         
172260                                                                          
172270     PERFORM UNTIL IDDC-IX > IX-DCCLEAR-MAX OR                            
172280                   W-GMT-IDDC-CLEAR(IDDC-IX) = SPACE                      
172290       IF DDGS-IDDC(DDGS-IX) = W-GMT-IDDC-CLEAR(IDDC-IX)                  
172291          MOVE JA                 TO DDGS-IDDC-SW                         
172292       END-IF                                                             
172293       ADD +1                     TO IDDC-IX                              
172294     END-PERFORM                                                          
172295      .                                                                   
172296                                                                          
172300 PB-FIND-WDF2-DIST-CUS SECTION.                                           
172400     MOVE 'PB-FIND-WDF2-'         TO CURRENT-SECTION                      
172500                                                                          
172600     MOVE NEJ                     TO SUPPLIER-SW                          
172700                                                                          
172800     PERFORM IMS-GU-WDF201                                                
172900     PERFORM IMS-GNP-WDF211-DIST-CUS                                      
173000     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
173100                   SUPPLIER-FOUND                                         
173200        IF (SLDO-IDDISTR-IN >= DIR-IDDISTR-FOM  AND                       
173300            SLDO-IDDISTR-IN <= DIR-IDDISTR-TOM) AND                       
173400           (SLDO-IDKUNDNR-IN >= DIR-IDKUNDNR-FOM AND                      
173500            SLDO-IDKUNDNR-IN <= DIR-IDKUNDNR-TOM)                         
173600            SET SUPPLIER-FOUND    TO TRUE                                 
173700            PERFORM S01-FILL-DDGS-TABLE                                   
173800        END-IF                                                            
173900        PERFORM IMS-GNP-WDF211-DIST-CUS                                   
174000     END-PERFORM                                                          
174100     .                                                                    
174200                                                                          
174300 S01-FILL-DDGS-TABLE SECTION.                                             
174400     MOVE 'S01-FILL-D'            TO CURRENT-SECTION                      
174500                                                                          
174600     INITIALIZE DDGS-TABLE                                                
174700     SET DDGS-IX                  TO +1                                   
174800     MOVE +1                      TO DIR-INDX                             
174900     MOVE 0                       TO DDGS-ROWS                            
175000                                                                          
175100     PERFORM UNTIL DDGS-IX > 5                                            
175200                                                                          
175300       MOVE DDGS-ROWS             TO DDGS-CLASS(DDGS-IX)                  
175400       MOVE DIR-KVBEART-MIN(DIR-INDX) TO DDGS-MIN-QTY(DDGS-IX)            
175500       MOVE DIR-IDDC(DIR-INDX)    TO DDGS-IDDC(DDGS-IX)                   
175600                                                                          
175700       ADD +1                     TO DDGS-ROWS                            
175800                                     DIR-INDX                             
175900       SET DDGS-IX                UP BY +1                                
176000                                                                          
176100     END-PERFORM                                                          
176200     .                                                                    
176300                                                                          
176400 IMS-GU-WDF2A SECTION.                                                    
176500     MOVE 'IMS-GU-WDF2A'     TO CURRENT-IMS-SECTION                       
176600                                                                          
176700     STRING 'WDF2A1  (WDF2A1KY>=' W-WDF2A1KY-MIN-X                        
176800                    '&WDF2A1KY<=' W-WDF2A1KY-MAX-X                        
176900                    '&DASTADAT<=' W-DASTADAT-X ')'                        
177000          DELIMITED BY SIZE  INTO SSA1                                    
177100     MOVE '  GE'             TO GODK-STATUSKODER                          
177200     CALL CBLTDLI            USING GU                                     
177300                                   WDF2A-PCB                              
177400                                   DLI-IO-WDF2A1                          
177500                                   SSA1                                   
177600     MOVE WDF2A-STATUS-CODE  TO STATUS-WS                                 
177700     PERFORM IMS-STATUSKONTROLL                                           
177800     .                                                                    
177900     EJECT                                                                
178000 IMS-GN-WDF2A SECTION.                                                    
178100     MOVE 'IMS-GU-WDF2A'     TO CURRENT-IMS-SECTION                       
178200                                                                          
178300     STRING 'WDF2A1  (WDF2A1KY>=' W-WDF2A1KY-MIN-X                        
178400                    '&WDF2A1KY<=' W-WDF2A1KY-MAX-X                        
178500                    '&DASTADAT<=' W-DASTADAT-X ')'                        
178600          DELIMITED BY SIZE INTO SSA1                                     
178700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
178800     CALL CBLTDLI USING GN                                                
178900                        WDF2A-PCB                                         
179000                        DLI-IO-WDF2A1                                     
179100                        SSA1                                              
179200     MOVE WDF2A-STATUS-CODE TO STATUS-WS                                  
179300     PERFORM IMS-STATUSKONTROLL                                           
179400     .                                                                    
179500     EJECT                                                                
179600 IMS-GU-WDF201 SECTION.                                                   
179700                                                                          
179800     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
179900          DELIMITED BY SIZE INTO SSA1                                     
180000     MOVE '  GE'             TO GODK-STATUSKODER                          
180100     CALL CBLTDLI USING GU                                                
180200                        WDF2-PCB                                          
180300                        DLI-IO-WDF201                                     
180400                        SSA1                                              
180500     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
180600     PERFORM IMS-STATUSKONTROLL                                           
180700     .                                                                    
180800     EJECT                                                                
180900 IMS-GNP-WDF211-DIST-CUS SECTION.                                         
181000                                                                          
181100     MOVE 'WDF211'               TO SSA1                                  
181200     MOVE '  GE'   TO GODK-STATUSKODER                                    
181300     CALL CBLTDLI USING GNP                                               
181400                        WDF2-PCB                                          
181500                        DLI-IO-WDF211                                     
181600                        SSA1                                              
181700     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
181800     PERFORM IMS-STATUSKONTROLL                                           
181900     .                                                                    
182000     EJECT                                                                
182100 IMS-GU-WDF101 SECTION.                                                   
182200     MOVE 'IMS-GU-WDF101   ' TO CURRENT-IMS-SECTION                       
182300                                                                          
182400                                                                          
182500     STRING 'WDF101  (IDLEVNR  =' W-WDF101KY-X ')'                        
182600          DELIMITED BY SIZE INTO SSA1                                     
182700     MOVE '  GE' TO GODK-STATUSKODER                                      
182800     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
182900     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
183000     PERFORM IMS-STATUSKONTROLL                                           
183100     .                                                                    
183200     EJECT                                                                
183300 IMS-GU-WDF118 SECTION.                                                   
183400     MOVE 'IMS-GU-WDF118   ' TO CURRENT-IMS-SECTION                       
183500                                                                          
183600     MOVE SPACE              TO ALL-SSA                                   
183700     STRING 'WDF101  (IDLEVNR  =' W-WDF101KY-X ')'                        
183800          DELIMITED BY SIZE INTO SSA1                                     
183900     STRING 'WDF118  (WDF118KY =' W-WDF118KY-X ')'                        
184000          DELIMITED BY SIZE INTO SSA2                                     
184100     MOVE '  GE'                 TO GODK-STATUSKODER                      
184200     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WLLEVA18 SSA1 SSA2             
184300     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
184400     PERFORM IMS-STATUSKONTROLL                                           
184500     .                                                                    
184600     EJECT                                                                
184700 IMS-GU-WDK601                 SECTION.                                   
184800     MOVE 'IMS-GU-WDK601   ' TO CURRENT-IMS-SECTION                       
184900                                                                          
185000     MOVE SPACE              TO ALL-SSA                                   
185100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
185200            DELIMITED BY SIZE INTO SSA1                                   
185300     MOVE '  GE'             TO GODK-STATUSKODER                          
185400     CALL  CBLTDLI  USING GU   WDK6-PCB DLI-IO-WDK601 SSA1                
185500     MOVE WDK6-STATUS-CODE   TO STATUS-WS                                 
185600     PERFORM IMS-STATUSKONTROLL                                           
185700     .                                                                    
185800                                                                          
185900 IMS-GNP-WDK611                SECTION.                                   
186000     MOVE 'IMS-GNP-WDK601  ' TO CURRENT-IMS-SECTION                       
186100                                                                          
186200     MOVE SPACE              TO ALL-SSA                                   
186300     MOVE 'WDK611  '                                                      
186400                             TO SSA1                                      
186500     MOVE '  GE'             TO GODK-STATUSKODER                          
186600     CALL  CBLTDLI  USING GNP  WDK6-PCB DLI-IO-WDK611 SSA1                
186700     MOVE WDK6-STATUS-CODE   TO STATUS-WS                                 
186800     PERFORM IMS-STATUSKONTROLL                                           
186900     .                                                                    
187000                                                                          
187100 IMS-GU-XXKJ11 SECTION.                                                   
187200     MOVE 'IMS-GU-XXKJ11   ' TO CURRENT-IMS-SECTION                       
187300                                                                          
187400     MOVE SPACE              TO ALL-SSA                                   
187500     STRING  'WLXXKJ01(WDGXKEY  =' W-WDGX01KEY-X ')'                      
187600            DELIMITED BY SIZE INTO SSA1                                   
187700     STRING  'WLXXKJ11(WDGXKEY >=' W-WDGX11KEY-X ')'                      
187800            DELIMITED BY SIZE INTO SSA2                                   
187900     MOVE    '  GE'             TO GODK-STATUSKODER                       
188000     CALL    CBLTDLI USING GU XXKJ-PCB DLI-IO-XXKJ11 SSA1 SSA2            
188100     MOVE    XXKJ-STATUS-CODE   TO STATUS-WS                              
188200     PERFORM IMS-STATUSKONTROLL                                           
188300     .                                                                    
188400                                                                          
188500 IMS-GU-WDD701 SECTION.                                                   
188600     MOVE 'IMS-GU-WDD701   ' TO CURRENT-IMS-SECTION                       
188700                                                                          
188800     MOVE SPACE              TO ALL-SSA                                   
188900                                                                          
189000     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
189100          DELIMITED BY SIZE INTO SSA1                                     
189200     MOVE 'WDD701   '         TO SSA2                                     
189300     MOVE '  GE'              TO GODK-STATUSKODER                         
189400     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
189500     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
189600     PERFORM IMS-STATUSKONTROLL                                           
189700     .                                                                    
189800                                                                          
189900 IMS-GNP-WDD702 SECTION.                                                  
190000     MOVE 'IMS-GNP-WDD702  ' TO CURRENT-IMS-SECTION                       
190100                                                                          
190200     MOVE SPACE              TO ALL-SSA                                   
190300                                                                          
190400     STRING 'WDD702  (FLTEXT   =' W-FLTEXT-X ')'                          
190500          DELIMITED BY SIZE INTO SSA1                                     
190600     MOVE '  GE'              TO GODK-STATUSKODER                         
190700     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
190800     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
190900     PERFORM IMS-STATUSKONTROLL                                           
191000     .                                                                    
191100                                                                          
191200 IMS-GU-BENA11-BSEQ   SECTION.                                            
191300     MOVE 'IMS-GNP-BENA11-B' TO CURRENT-IMS-SECTION                       
191400                                                                          
191500     MOVE SPACE              TO ALL-SSA                                   
191600                                                                          
191700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
191800            DELIMITED BY SIZE INTO SSA1                                   
191900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
192000            DELIMITED BY SIZE INTO SSA2                                   
192100     MOVE '  GE' TO GODK-STATUSKODER                                      
192200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WDD311 SSA1 SSA2               
192300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
192400     PERFORM IMS-STATUSKONTROLL                                           
192500     .                                                                    
192600                                                                          
192700 IMS-GU-WDB301 SECTION.                                                   
192800     MOVE 'IMS-GU-WDB301'   TO CURRENT-IMS-SECTION                        
192900                                                                          
193000     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
193100                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
193200          DELIMITED BY SIZE INTO SSA1                                     
193300     MOVE '  GE' TO GODK-STATUSKODER                                      
193400     CALL CBLTDLI USING GHU WDB3-PCB DLI-IO-WDB301 SSA1                   
193500     MOVE WDB3-STATUS-CODE TO STATUS-WS                                   
193600     PERFORM IMS-STATUSKONTROLL                                           
193700     .                                                                    
193800                                                                          
193900 IMS-GU-WDK711                SECTION.                                    
194000     MOVE 'IMS-GU-WDK711  '  TO CURRENT-IMS-SECTION                       
194100                                                                          
194200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
194300          DELIMITED BY SIZE INTO SSA1                                     
194400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
194500          DELIMITED BY SIZE INTO SSA2                                     
194600     MOVE '  GE' TO GODK-STATUSKODER                                      
194700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
194800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
194900     PERFORM IMS-STATUSKONTROLL                                           
195000     .                                                                    
195100 IMS-ISRT-WDR601 SECTION.                                                 
195200     MOVE 'IMS-ISRT-WDR601'  TO CURRENT-IMS-SECTION                       
195300                                                                          
195400     MOVE 'WDR601' TO SSA1                                                
195500     MOVE '   II' TO GODK-STATUSKODER                                     
195600     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
195700     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
195800     PERFORM IMS-STATUSKONTROLL                                           
195900     .                                                                    
195910 IMS-GU-WDB201 SECTION.                                                   
195920     MOVE 'IMS-GU-WDB201  '  TO CURRENT-IMS-SECTION                       
195930                                                                          
195940     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
195950          DELIMITED BY SIZE INTO SSA1                                     
195960     MOVE '  GE'               TO GODK-STATUSKODER                        
195970     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
195980     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
195990     PERFORM IMS-STATUSKONTROLL                                           
195991     .                                                                    
195992 IMS-GU-WDB1-WDB101 SECTION.                                              
195993     MOVE 'IMS-GU-WDB1-WDB'  TO CURRENT-IMS-SECTION                       
195994                                                                          
195995     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
195996          DELIMITED BY SIZE INTO SSA1                                     
195997     MOVE '  GE'              TO GODK-STATUSKODER                         
195998     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
195999     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
196000     PERFORM IMS-STATUSKONTROLL                                           
196001     .                                                                    
196002     SKIP2                                                                
196010 IMS-STATUSKONTROLL SECTION.                                              
196100                                                                          
196200     SET STATUS-IX TO 1                                                   
196300     SEARCH GODK-STATUS                                                   
196400       AT END                                                             
196500         CALL FELLOG                                                      
196600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
196700         CONTINUE                                                         
196800     END-SEARCH                                                           
196900     .                                                                    
197000*    -COPY WY2000Q1                                                       
