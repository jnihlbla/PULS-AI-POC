000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411DNOT.                                                
000500 AUTHOR.         STEFAN KIHLBERG.                                         
000600 DATE-WRITTEN.   AUG-98.                                                  
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    DETTA ÄR EN SUBMODUL SOM ANROPAS I ORDER-ENTRY,                      
001100*    UTSKRIFT OCH PACNING FÖR ATT LÄGGA UPP, ÄNDRA                        
001200*    ELLER TA BORT SEGMENT PÅ WDQ5.                                       
001300*    FRÅN VIPS.                                                           
001400*                                                                         
001500*    REGISTER :    WLORQP (WDQ5) PULSORDRAR OCH ORDERBEKRÄFTELSER         
001600*                  FRÅN VIPS                                              
001700*                                                                         
001800*    LÄNKAREA :    W411DNOT-CTX                                           
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400                                                                          
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 01  FILLER                      PIC X(16)   VALUE 'KONSTANTER'.          
003000 01  IDPGM                       PIC X(08)   VALUE 'W411DNOT'.            
003100 01  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200 01  KONSTANTER.                                                          
003300     03  JA                  PIC X       VALUE 'J'.                       
003400     03  NEJ                 PIC X       VALUE 'N'.                       
003500     03  VIPS                PIC X       VALUE 'V'.                       
003600                                                                          
003700                                                                          
003800 01  ARBETSFALT.                                                          
003900     03 WS-IDLOPNR               PIC S9(03)  VALUE 1.                     
004000     03 WS-IDSEKVNR              PIC S9(03)  VALUE 1.                     
004100     03 WS-BEART-USA             PIC  X(25)  VALUE SPACE.                 
004200     03 WS-KVBEART               PIC S9(07)  VALUE ZERO.                  
004300     03 WS-NYTT-DC               PIC  X(02)  VALUE SPACE.                 
004400     03 WS-KVLEVART-KVAR         PIC S9(07)  VALUE ZERO COMP-3.           
004500     03 WS-KVLEVART-ANDR         PIC S9(07)  VALUE ZERO COMP-3.           
004600                                                                          
004700     03 WS-IDARTNR-TILLK-ALFA-GRP.                                        
004800        05 FILLER                PIC  X(16).                              
004900        05 WS-IDARTNR-TILLK-ALFA PIC  X(09).                              
005000                                                                          
005100     03 WS-IDARTNR-TILLK-NUM     PIC S9(09)  COMP-3 VALUE ZERO.           
005200     03 DAGENS-DATUM             PIC S9(07)  COMP-3 VALUE ZERO.           
005300                                                                          
005400     03 WS-IDKUNDRF-RO.                                                   
005500        05 FILLER                PIC  X(02)  VALUE '00'.                  
005600        05 WS-IDKUNDRF-RO-5      PIC  X(05)  VALUE ZERO.                  
005700        05 FILLER                PIC  X(03)  VALUE SPACE.                 
005800                                                                          
005900 01  FILLER                      PIC X(16)   VALUE 'VIPS-FÄLT'.           
006000 01  VIPSFALT.                                                            
006100     03  WS-VIPS-IDORDER         PIC S9(07)  COMP-3 VALUE ZERO.           
006200     03  WS-VIPS-IDARTNR         PIC S9(09)  COMP-3 VALUE ZERO.           
006300     03  WS-VIPS-IDLOPNR         PIC S9(03)  COMP-3 VALUE ZERO.           
006400     03  WS-VIPS-IDSEKVNR        PIC S9(03)  COMP-3 VALUE ZERO.           
006500     03  WS-VIPS-IDDC            PIC  X(02)  VALUE SPACE.                 
006600     03  WS-VIPS-KDORDBEK        PIC  9(02)  VALUE ZERO.                  
006700     03  WS-VIPS-BEART-USA       PIC  X(25)  VALUE SPACE.                 
006800     03  WS-VIPS-KDORDKL         PIC S9(01)  COMP-3 VALUE ZERO.           
006900     03  WS-VIPS-KVBEART         PIC S9(07)  COMP-3 VALUE ZERO.           
007000     03  WS-VIPS-KVBEART-Q       PIC S9(07)  COMP-3 VALUE ZERO.           
007100                                                                          
007200     03  WS-VIPS-IDGMTREF.                                                
007300       05  WS-VIPS-IDDISTR         PIC S9(05)  COMP-3                     
007400                                               VALUE ZERO.                
007500       05  WS-VIPS-IDKUNDNR        PIC S9(07)  COMP-3                     
007600                                               VALUE ZERO.                
007700       05  WS-VIPS-IDKUNDRF-GRP.                                          
007800         07  WS-VIPS-IDKUNDRF      PIC  X(10)  VALUE ZERO.                
007900         07  WS-VIPS-IDORDNR5-FILLER REDEFINES WS-VIPS-IDKUNDRF.          
008000           09 WS-VIPS-IDORDNR5     PIC  9(05).                            
008100           09 FILLER               PIC  X(05).                            
008200         07  WS-VIPS-IDORDNR5-FILLER REDEFINES WS-VIPS-IDKUNDRF.          
008300           09 WS-VIPS-IDORDNR5     PIC  9(07).                            
008400           09 FILLER               PIC  X(03).                            
008500*                                                                         
008600 01  FILLER                      PIC X(24)   VALUE 'SWITCHAR'.            
008700                                                                          
008800 77  TRAFF-SW                    PIC X   VALUE 'N'.                       
008900     88  TRAFF                           VALUE 'J'.                       
009000     88  NO-TRAFF                        VALUE 'N'.                       
009100                                                                          
009200 77  NEWCASE-TRAFF-SW            PIC X   VALUE 'N'.                       
009300     88  NEWCASE-TRAFF                   VALUE 'J'.                       
009400     88  NO-NEWCASE                      VALUE 'N'.                       
009500                                                                          
009600 01  GENERELLA-SUBPROGRAM.                                                
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
010000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL ABEND                                            
010300                                                                          
010400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010600     SKIP2                                                                
010700     EJECT                                                                
010800                                                                          
010900 01  FILLER                      PIC X(16)   VALUE 'W009KSIF'.            
011000 01  W009KSIF-PARM.                                                       
011100     03 KSIF-FLT                 PIC 9(9).                                
011200     03 KSIF-LGD                 PIC 9(1).                                
011300     03 KSIF-KSIFF               PIC 9(9).                                
011400                                                                          
011500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011600*                                                                         
011700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011800     SKIP2                                                                
011900*    --- STATUS-KOD FRÅN IMS                                              
012000 01  STATUS-WS                   PIC XX.                                  
012100     88  SEGMENT-FINNS                       VALUE '  '.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012300     88  BASEN-SLUT                          VALUE 'GB'.                  
012400     88  SEGMENT-REDAN-FINNS                 VALUE 'II'.                  
012500 01  ORQP3-STATUS-WS              PIC XX.                                 
012600     88  ORQP3-SEGMENT-FINNS                  VALUE '  '.                 
012700     88  ORQP3-SEGMENT-SAKNAS                 VALUE 'GE'.                 
012800     88  ORQP3-BASEN-SLUT                     VALUE 'GB'.                 
012900     88  ORQP3-SEGMENT-REDAN-FINNS            VALUE 'II'.                 
013000     SKIP2                                                                
013100 01  GODK-STATUSKODER.                                                    
013200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013300     SKIP2                                                                
013400 01  SSA1                        PIC X(128).                              
013500 01  SSA2                        PIC X(64).                               
013600     EJECT                                                                
013700                                                                          
013800*    --- IMS FUNKTIONSKODER                                               
013900*01  -COPY W0003                                                          
014000     EJECT                                                                
014100                                                                          
014200 01  FILLER                      PIC X(16)   VALUE 'NYCKLAR'.             
014300 01  NYCKLAR-TILL-DLI.                                                    
014400                                                                          
014500     03  W-WDQ501KY-X.                                                    
014600         05  W-IDORDER           PIC S9(7) COMP-3 VALUE ZERO.             
014700         05  W-IDARTNR           PIC S9(9) COMP-3 VALUE ZERO.             
014800         05  W-IDLOPNR           PIC S9(3) COMP-3 VALUE ZERO.             
014900         05  W-IDSEKVNR          PIC S9(3) COMP-3 VALUE ZERO.             
015000         05  W-IDDC              PIC X(2)  VALUE SPACE.                   
015100         05  W-KDORDBEK          PIC 9(2)  VALUE ZERO.                    
015200                                                                          
015300                                                                          
015400     03  W-WDQ501KY-HUV-X.                                                
015500         05  W-IDORDER-HUV       PIC S9(7) COMP-3 VALUE ZERO.             
015600         05  W-IDARTNR-HUV       PIC S9(9) COMP-3 VALUE ZERO.             
015700         05  W-IDLOPNR-HUV       PIC S9(3) COMP-3 VALUE +001.             
015800         05  W-IDSEKVNR-HUV      PIC S9(3) COMP-3 VALUE +001.             
015900         05  W-IDDC-HUV          PIC  X(2) VALUE SPACE.                   
016000         05  W-KDORDBEK-HUV      PIC  9(2) VALUE ZERO.                    
016100                                                                          
016200                                                                          
016300     03  W-WDQ501KY-MIN-X.                                                
016400         05  W-IDORDER-MIN       PIC S9(7) COMP-3 VALUE ZERO.             
016500         05  W-IDARTNR-MIN       PIC S9(9) COMP-3 VALUE ZERO.             
016600         05  W-IDLOPNR-MIN       PIC S9(3) COMP-3 VALUE ZERO.             
016700         05  W-IDSEKVNR-MIN      PIC S9(3) COMP-3 VALUE ZERO.             
016800         05  W-IDDC-MIN          PIC  X(2) VALUE SPACE.                   
016900         05  W-KDORDBEK-MIN      PIC  9(2) VALUE ZERO.                    
017000                                                                          
017100     03  W-WDQ501KY-MAX-X.                                                
017200         05  W-IDORDER-MAX       PIC S9(7) COMP-3 VALUE ZERO.             
017300         05  W-IDARTNR-MAX       PIC S9(9)                                
017400                                     COMP-3 VALUE +999999999.             
017500         05  W-IDLOPNR-MAX       PIC S9(3) COMP-3 VALUE +999.             
017600         05  W-IDSEKVNR-MAX      PIC S9(3) COMP-3 VALUE +999.             
017700         05  W-IDDC-MAX          PIC  X(2) VALUE HIGH-VALUE.              
017800         05  W-KDORDBEK-MAX      PIC  9(2) VALUE 99.                      
017900                                                                          
018000                                                                          
018100     03  W-WDQ501KY-TILLK-MIN-X.                                          
018200         05  W-IDORDER-TILLK-MIN  PIC S9(7) COMP-3 VALUE ZERO.            
018300         05  W-IDARTNR-TILLK-MIN  PIC S9(9) COMP-3 VALUE ZERO.            
018400         05  W-IDLOPNR-TILLK-MIN  PIC S9(3) COMP-3 VALUE ZERO.            
018500         05  W-IDSEKVNR-TILLK-MIN PIC S9(3) COMP-3 VALUE ZERO.            
018600         05  W-IDDC-TILLK-MIN     PIC  X(2) VALUE SPACE.                  
018700         05  W-KDORDBEK-TILLK-MIN PIC  9(2) VALUE ZERO.                   
018800                                                                          
018900                                                                          
019000     03  W-WDQ501KY-TILLK-MAX-X.                                          
019100         05  W-IDORDER-TILLK-MAX  PIC S9(7) COMP-3 VALUE ZERO.            
019200         05  W-IDARTNR-TILLK-MAX  PIC S9(9) COMP-3 VALUE ZERO.            
019300         05  W-IDLOPNR-TILLK-MAX  PIC S9(3) COMP-3 VALUE +999.            
019400         05  W-IDSEKVNR-TILLK-MAX PIC S9(3) COMP-3 VALUE +999.            
019500         05  W-IDDC-TILLK-MAX     PIC  X(2) VALUE HIGH-VALUE.             
019600         05  W-KDORDBEK-TILLK-MAX PIC  9(2) VALUE 99.                     
019700                                                                          
019800                                                                          
019900     03  W-WDQ501KY-NEWCASE-X.                                            
020000         05  W-IDORDER-NEWCASE   PIC S9(7) COMP-3 VALUE ZERO.             
020100         05  W-IDARTNR-NEWCASE   PIC S9(9) COMP-3 VALUE ZERO.             
020200         05  W-IDLOPNR-NEWCASE   PIC S9(3) COMP-3 VALUE ZERO.             
020300         05  W-IDSEKVNR-NEWCASE  PIC S9(3) COMP-3 VALUE ZERO.             
020400         05  W-IDDC-NEWCASE      PIC  X(2) VALUE SPACE.                   
020500         05  W-KDORDBEK-NEWCASE  PIC  9(2) VALUE ZERO.                    
020600                                                                          
020700                                                                          
020800     03  W-WDQ501KY-NEWCASE-MIN-X.                                        
020900         05  W-IDORDER-NEWCASE-MIN PIC S9(7) COMP-3 VALUE ZERO.           
021000         05  W-IDARTNR-NEWCASE-MIN PIC S9(9) COMP-3 VALUE ZERO.           
021100         05  W-IDLOPNR-NEWCASE-MIN PIC S9(3) COMP-3 VALUE ZERO.           
021200         05  W-IDSEKVNR-NEWCASE-MIN PIC S9(3) COMP-3 VALUE ZERO.          
021300         05  W-IDDC-NEWCASE-MIN   PIC  X(2) VALUE SPACE.                  
021400         05  W-KDORDBEK-NEWCASE-MIN PIC 9(2) VALUE ZERO.                  
021500                                                                          
021600                                                                          
021700     03  W-WDQ501KY-NEWCASE-MAX-X.                                        
021800         05  W-IDORDER-NEWCASE-MAX PIC S9(7) COMP-3 VALUE ZERO.           
021900         05  W-IDARTNR-NEWCASE-MAX PIC S9(9) COMP-3 VALUE ZERO.           
022000         05  W-IDLOPNR-NEWCASE-MAX PIC S9(3) COMP-3 VALUE +999.           
022100         05  W-IDSEKVNR-NEWCASE-MAX PIC S9(3) COMP-3 VALUE +999.          
022200         05  W-IDDC-NEWCASE-MAX   PIC  X(2) VALUE HIGH-VALUE.             
022300         05  W-KDORDBEK-NEWCASE-MAX PIC 9(2) VALUE 99.                    
022400                                                                          
022500                                                                          
022600     03  W-4013-WDGXKEY-X.                                                
022700         05  W-4013-IDHTYP       PIC  X(04) VALUE '4013'.                 
022800         05  W-4013-LOW-VALUE    PIC  X(26) VALUE LOW-VALUE.              
022900                                                                          
023000     03  W-4014-IDGMTREF-X.                                               
023100         05  W-4014-IDDISTR      PIC S9(05) VALUE ZERO COMP-3.            
023200         05  W-4014-IDKUNDNR     PIC S9(07) VALUE ZERO COMP-3.            
023300         05  W-4014-IDKUNDRF.                                             
023400            07  W-4014-IDORDNR7  PIC  9(07) VALUE ZERO.                   
023500            07  FILLER           PIC  X(03) VALUE SPACE.                  
023600*                                                                         
023700     03  W-KY4016-X.                                                      
023800         05  W-4016-IDARTNR      PIC S9(09) VALUE ZERO COMP-3.            
023900         05  W-4016-IDLOPNR      PIC S9(03) VALUE ZERO COMP-3.            
024000         05  W-4016-IDSEKVNR     PIC S9(03) VALUE ZERO COMP-3.            
024100         05  W-4016-KDORDBEK     PIC  9(02) VALUE ZERO.                   
024200                                                                          
024300                                                                          
024400     03  W-BENA-IDARTNR-X.                                                
024500         05  W-BENA-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.           
024600     03  W-IDSKYLT-X.                                                     
024700        05 W-IDSKYLT             PIC X(3)   VALUE 'USA'.                  
024800                                                                          
024900     03  W-Q5-IDDC-X.                                                     
025000         05  W-Q5-IDDC           PIC  X(2)   VALUE SPACE.                 
025100                                                                          
025200*    ---  DLI INPUT-OUTPUT AREA                                           
025300 01  FILLER         PIC X(16) VALUE 'DLI-IO-ORQP01'.                      
025400 01  DLI-IO-ORQP01.                                                       
025500*    03  -COPY WDQ501                                                     
025600     EJECT                                                                
025700                                                                          
025800 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORQP01-HUV'.                  
025900 01  DLI-IO-ORQP01-HUV.                                                   
026000*    03  -COPY WDQ501    -PRE HUVUD-                                      
026100     EJECT                                                                
026200                                                                          
026300 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORQP01-TILLK'.                
026400 01  DLI-IO-ORQP01-TILLK.                                                 
026500*    03  -COPY WDQ501    -PRE TILLK-                                      
026600     EJECT                                                                
026700                                                                          
026800 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORQP01-NEWCASE'.              
026900 01  DLI-IO-ORQP01-NEWCASE.                                               
027000*    03  -COPY WDQ501    -PRE NEWCASE-                                    
027100     EJECT                                                                
027200                                                                          
027300                                                                          
027400 01  FILLER                      PIC X(16) VALUE 'WL401301-AREA'.         
027500 01  DLI-IO-WL401301.                                                     
027600     03  WL401301.                                                        
027700*        05  -COPY WDGX4013                                               
027800 01  FILLER                      PIC X(16) VALUE 'WL401311-AREA'.         
027900 01  DLI-IO-WL401311.                                                     
028000     03  WL401311.                                                        
028100*        05  -COPY WDGX4014                                               
028200 01  FILLER                      PIC X(16) VALUE 'WL401321-AREA'.         
028300 01  DLI-IO-WL401321.                                                     
028400     03  WL401321.                                                        
028500*        05  -COPY WDGX4016                                               
028600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-BENA11'.         
028700 01  DLI-IO-BENA11.                                                       
028800*    03  -COPY WDD311                                                     
028900     EJECT                                                                
029000                                                                          
029100                                                                          
029200 LINKAGE SECTION.                                                         
029300*                                                                         
029400*   -COPY W411DNOT                                                        
029500*    EJECT                                                                
029600                                                                          
029700*01  -COPY W0008      -PRE ORQP-                                          
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000*01  -COPY W0008      -PRE ORQP2-                                         
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008      -PRE ORQP3-                                         
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008      -PRE 4013-                                          
030700     05  FILLER                  PIC X.                                   
030800     EJECT                                                                
030900*01  -COPY W0008      -PRE BENA-                                          
031000     05  FILLER                  PIC X.                                   
031100     EJECT                                                                
031200*                                                                         
031300 PROCEDURE DIVISION  USING DNOT-W411DNOT                                  
031400                           ORQP-PCB                                       
031500                           ORQP2-PCB                                      
031600                           ORQP3-PCB                                      
031700                           4013-PCB                                       
031800                           BENA-PCB.                                      
031900                                                                          
032000 MAIN SECTION.                                                            
032100                                                                          
032200     MOVE 'STA-MAIN'      TO FELTEXT                                      
032300                                                                          
032400     PERFORM A-INIT                                                       
032500                                                                          
032600     EVALUATE TRUE                                                        
032700*       ORDERENTRY                                                        
032800        WHEN DNOT-IDPGM = 'W4021200' OR                                   
032900                          'W4022200' OR                                   
033000                          'W4025200'                                      
033100           PERFORM B-BEHANDLA-ORDER-ENTRY                                 
033200                                                                          
033300*       VIPS-OBKR                                                         
033400        WHEN DNOT-IDPGM = 'W4025300'                                      
033500           PERFORM C-BEHANDLA-VIPS-OBKR                                   
033600                                                                          
033700*       UTSKRIFT                                                          
033800        WHEN DNOT-IDPGM = 'W4037800' OR                                   
033900                          'WL013440'                                      
034000           PERFORM D-BEHANDLA-UTSKRIFT                                    
034100                                                                          
034200*       PACKNING                                                          
034300        WHEN DNOT-IDPGM = 'W4031400' OR                                   
034400                          'W4031500' OR                                   
034500                          'W403AVSP' OR                                   
034510                          'WL012200' OR                                   
034520                          'WL0134NY' OR                                   
034521                          'WL019700' OR                                   
034530                          'WL019900'                                      
034600           PERFORM E-BEHANDLA-PACKNING                                    
034700                                                                          
034800*       ANNULLERING                                                       
034900        WHEN DNOT-IDPGM = 'W4021300' OR                                   
035000                          'W4022300'                                      
035100           PERFORM F-BEHANDLA-ANNULLERING                                 
035200                                                                          
035300*       BORTTAG AV KOLLI                                                  
035400        WHEN DNOT-IDPGM = 'W4031600' OR                                   
035410                          'WL012700'                                      
035500           PERFORM G-BEHANDLA-BORTTAG-AV-KOLLI                            
035600                                                                          
035610*       BORTTAG AV DEFAULT KOLLI                                          
035640        WHEN DNOT-IDPGM = 'WL0199DE'                                      
035650           PERFORM M-REMOVE-OF-DEFAULT-CASE                               
035660                                                                          
035700*       ÄNDRING AV KOLLI                                                  
035800        WHEN DNOT-IDPGM = 'W4034300'                                      
035900           PERFORM H-BEHANDLA-ANDRING-AV-KOLLI                            
036000                                                                          
036100*       PRINTNING AV DEL NOTE                                             
036200        WHEN DNOT-IDPGM = 'W403AVSX' OR                                   
036300                          'W4039700'                                      
036400           PERFORM I-UPPDATERA-PRINTDAG-DELNOTE                           
036500                                                                          
036600*       TIIÄGG AV OREDERRAD                                               
036700        WHEN DNOT-IDPGM = 'W4020200'                                      
036800           PERFORM K-TILLAGG-ORDERRAD                                     
036900                                                                          
037000*       ÄNDRA AV OREDERRAD                                                
037100        WHEN DNOT-IDPGM = 'W4020400'                                      
037200           PERFORM L-ANDRA-ORDERRAD                                       
037300                                                                          
037400     END-EVALUATE                                                         
037500     GOBACK                                                               
037600     .                                                                    
037700     EJECT                                                                
037800                                                                          
037900                                                                          
038000 A-INIT SECTION.                                                          
038100                                                                          
038200     MOVE 'STA-A-INIT'    TO FELTEXT                                      
038300                                                                          
038400     ACCEPT DAGENS-DATUM           FROM DATE                              
038500     .                                                                    
038600     EJECT                                                                
038700                                                                          
038800                                                                          
038900 B-BEHANDLA-ORDER-ENTRY SECTION.                                          
039000                                                                          
039100     MOVE 'STA-B-BEHA'    TO FELTEXT                                      
039200                                                                          
039300     IF DNOT-IDPGM = 'W4025200'                                           
039400        MOVE +1              TO WS-IDLOPNR                                
039500                                WS-IDSEKVNR                               
039600        PERFORM BD-FLYTTA-KUNDORDERRAD                                    
039700        PERFORM IMS-ISRT-RADB                                             
039800        PERFORM UNTIL SEGMENT-FINNS                                       
039900           ADD 1                   TO W-IDLOPNR                           
040000                                      WS-IDLOPNR                          
040100                                      RADB-IDLOPNR                        
040200           PERFORM IMS-ISRT-RADB                                          
040300        END-PERFORM                                                       
040400                                                                          
040500*  *HÄMTA VIPSORDERBEKRÄFTELSER                                           
040600        IF DNOT-FL-ORAD-LAST = JA                                         
040700           PERFORM BB-NYCKLAR-HAMTA-VIPS                                  
040800           PERFORM IMS-GU-WL401311                                        
040900           IF SEGMENT-FINNS                                               
041000              PERFORM IMS-GNP-WL401321                                    
041100              PERFORM UNTIL SEGMENT-SAKNAS                                
041200                 PERFORM BC-FLYTTA-VIPS-OBKR                              
041300                 PERFORM S31-BEHANDLA-VIPS-OBKR                           
041400                 PERFORM IMS-GNP-WL401321                                 
041500              END-PERFORM                                                 
041600              PERFORM IMS-GU-WL401301                                     
041700              PERFORM IMS-GHN-WL401311                                    
041800              PERFORM IMS-DLET-WL401311                                   
041900           END-IF                                                         
042000           PERFORM BA-SKAPA-ORDERHUVUD-Q5                                 
042100           PERFORM IMS-ISRT-RADB-HUVUD                                    
042200        END-IF                                                            
042300                                                                          
042400     ELSE                                                                 
042500                                                                          
042600        MOVE +1              TO WS-IDLOPNR                                
042700                                WS-IDSEKVNR                               
042800        IF DNOT-FL-OHUV-OK NOT = JA                                       
042900           PERFORM BA-SKAPA-ORDERHUVUD-Q5                                 
043000           MOVE JA                    TO DNOT-FL-OHUV-OK                  
043100           PERFORM IMS-ISRT-RADB-HUVUD                                    
043200        END-IF                                                            
043300                                                                          
043400        PERFORM BD-FLYTTA-KUNDORDERRAD                                    
043500        PERFORM IMS-ISRT-RADB                                             
043600        PERFORM UNTIL SEGMENT-FINNS                                       
043700           ADD 1                   TO W-IDLOPNR                           
043800                                      WS-IDLOPNR                          
043900                                      RADB-IDLOPNR                        
044000           PERFORM IMS-ISRT-RADB                                          
044100        END-PERFORM                                                       
044200                                                                          
044300*  *HÄMTA VIPSORDERBEKRÄFTELSER                                           
044400        IF DNOT-FL-ORAD-LAST = JA                                         
044500           MOVE DNOT-IDORDER             TO W-IDORDER-HUV                 
044600           MOVE DNOT-IDDC-PRIM           TO W-IDDC-HUV                    
044700           PERFORM IMS-GU-RADB-HUVUD                                      
044800           PERFORM BB-NYCKLAR-HAMTA-VIPS                                  
044900           PERFORM IMS-GU-WL401311                                        
045000           IF SEGMENT-FINNS                                               
045100              PERFORM IMS-GNP-WL401321                                    
045200              PERFORM UNTIL SEGMENT-SAKNAS                                
045300                 PERFORM BC-FLYTTA-VIPS-OBKR                              
045400                 PERFORM S31-BEHANDLA-VIPS-OBKR                           
045500                 PERFORM IMS-GNP-WL401321                                 
045600              END-PERFORM                                                 
045700              PERFORM IMS-GU-WL401301                                     
045800              PERFORM IMS-GHN-WL401311                                    
045900              PERFORM IMS-DLET-WL401311                                   
046000           END-IF                                                         
046100        END-IF                                                            
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500                                                                          
046600                                                                          
046700 BA-SKAPA-ORDERHUVUD-Q5 SECTION.                                          
046800                                                                          
046900     MOVE 'STA-BA    '    TO FELTEXT                                      
047000                                                                          
047100     INITIALIZE HUVUD-RADB-WDQ501                                         
047200     MOVE NEJ                      TO HUVUD-RADB-FLTILLK                  
047300                                      HUVUD-RADB-FLDIRLEV                 
047400                                                                          
047500     MOVE DNOT-IDORDER             TO W-IDORDER-HUV                       
047600     MOVE ZERO                     TO W-IDARTNR-HUV                       
047700     MOVE +001                     TO W-IDLOPNR-HUV                       
047800     MOVE +001                     TO W-IDSEKVNR-HUV                      
047900     MOVE DNOT-IDDC-PRIM           TO W-IDDC-HUV                          
048000     MOVE ZERO                     TO W-KDORDBEK-HUV                      
048100                                                                          
048200     MOVE DNOT-IDORDER             TO HUVUD-RADB-IDORDER                  
048300     MOVE ZERO                     TO HUVUD-RADB-IDARTNR                  
048400     MOVE +001                     TO HUVUD-RADB-IDLOPNR                  
048500     MOVE +001                     TO HUVUD-RADB-IDSEKVNR                 
048600     MOVE DNOT-IDDC-PRIM           TO HUVUD-RADB-IDDC                     
048700     MOVE ZERO                     TO HUVUD-RADB-KDORDBEK                 
048800                                                                          
048900     MOVE DNOT-ADGMT               TO HUVUD-RADB-ADGMT                    
049000     MOVE DNOT-BEGMT               TO HUVUD-RADB-BEGMT                    
049100     MOVE DNOT-IDGMTREF            TO HUVUD-RADB-IDGMTREF                 
049300     MOVE DNOT-BEKUNDRF            TO HUVUD-RADB-BEKUNDRF                 
049400     MOVE DNOT-KDFRAKT             TO HUVUD-RADB-KDFRAKT                  
049500     MOVE DNOT-KDORDKL             TO HUVUD-RADB-KDORDKL                  
049600     MOVE DNOT-TIREGDAT            TO HUVUD-RADB-TIREGDAT                 
049700     MOVE DNOT-TIREGTID            TO HUVUD-RADB-TIREGTID                 
049800     .                                                                    
049900     EJECT                                                                
050000                                                                          
050100                                                                          
050200 BB-NYCKLAR-HAMTA-VIPS SECTION.                                           
050300                                                                          
050400     MOVE 'STA-BB    '    TO FELTEXT                                      
050500                                                                          
050600     MOVE DNOT-IDDISTR             TO W-4014-IDDISTR                      
050700     MOVE DNOT-IDKUNDNR            TO W-4014-IDKUNDNR                     
050800     MOVE DNOT-IDORDNR7            TO W-4014-IDORDNR7                     
050900     .                                                                    
051000     EJECT                                                                
051100                                                                          
051200                                                                          
051300 BC-FLYTTA-VIPS-OBKR SECTION.                                             
051400                                                                          
051500     MOVE 'STA-BC    '    TO FELTEXT                                      
051600                                                                          
051700     MOVE DNOT-IDORDER             TO WS-VIPS-IDORDER                     
051800     MOVE 4014-IDGMTREF            TO WS-VIPS-IDGMTREF                    
051900     MOVE 4016-IDARTNR             TO WS-VIPS-IDARTNR                     
052000     MOVE 4016-IDLOPNR             TO WS-VIPS-IDLOPNR                     
052100     MOVE 4016-IDSEKVNR            TO WS-VIPS-IDSEKVNR                    
052200     MOVE DNOT-IDDC-PRIM           TO WS-VIPS-IDDC                        
052300     MOVE 4016-KDORDBEK            TO WS-VIPS-KDORDBEK                    
052400     MOVE 4016-BEART-USA           TO WS-VIPS-BEART-USA                   
052500     MOVE 4016-KDORDKL             TO WS-VIPS-KDORDKL                     
052600     MOVE 4016-KVBEART             TO WS-VIPS-KVBEART                     
052700                                      WS-VIPS-KVBEART-Q                   
052800     .                                                                    
052900     EJECT                                                                
053000                                                                          
053100                                                                          
053200 BD-FLYTTA-KUNDORDERRAD SECTION.                                          
053300                                                                          
053400     MOVE 'STA-BD    '    TO FELTEXT                                      
053500                                                                          
053600     INITIALIZE RADB-WDQ501                                               
053700                                                                          
053800     MOVE +1                       TO WS-IDLOPNR                          
053900                                      WS-IDSEKVNR                         
054000     MOVE NEJ                      TO RADB-FLTILLK                        
054100                                      RADB-FLDIRLEV                       
054200                                                                          
054300     MOVE DNOT-IDORDER             TO W-IDORDER                           
054400     MOVE DNOT-IDARTNR             TO W-IDARTNR                           
054500     MOVE WS-IDLOPNR               TO W-IDLOPNR                           
054600     MOVE WS-IDSEKVNR              TO W-IDSEKVNR                          
054700     MOVE DNOT-IDDC                TO W-IDDC                              
054800     MOVE DNOT-KDORDBEK            TO W-KDORDBEK                          
054900                                                                          
055000     MOVE DNOT-IDORDER             TO RADB-IDORDER                        
055100     MOVE DNOT-IDARTNR             TO RADB-IDARTNR                        
055200     MOVE WS-IDLOPNR               TO RADB-IDLOPNR                        
055300     MOVE WS-IDSEKVNR              TO RADB-IDSEKVNR                       
055400     MOVE DNOT-IDDC                TO RADB-IDDC                           
055500     MOVE DNOT-KDORDBEK            TO RADB-KDORDBEK                       
055600                                                                          
055700     MOVE DNOT-BERADREF            TO RADB-BERADREF                       
055800     MOVE DNOT-FLTILLK             TO RADB-FLTILLK                        
055900     MOVE DNOT-FLDIRLEV            TO RADB-FLDIRLEV                       
056000     MOVE DNOT-IDGMTREF            TO RADB-IDGMTREF                       
056100     MOVE DNOT-IDKUNDRF-RO         TO RADB-IDKUNDRF-RO                    
056300     MOVE DNOT-KDFRAKT             TO RADB-KDFRAKT                        
056400     MOVE DNOT-KDORDKL             TO RADB-KDORDKL                        
056500     MOVE DNOT-KVBEART             TO RADB-KVBEART                        
056600     MOVE DNOT-KVBEART-Q           TO RADB-KVBEART-Q                      
056700     MOVE DNOT-REKSIFFR            TO RADB-REKSIFFR                       
056800     MOVE DNOT-IDARTNR             TO W-BENA-IDARTNR                      
056900     PERFORM S51-HAMTA-BENAMNING                                          
057000     MOVE WS-BEART-USA             TO RADB-BEART-USA                      
057100     .                                                                    
057200     EJECT                                                                
057300                                                                          
057400                                                                          
057500 C-BEHANDLA-VIPS-OBKR SECTION.                                            
057600                                                                          
057700     MOVE 'STA-C     '    TO FELTEXT                                      
057800                                                                          
057900     PERFORM CA-FLYTTA-VIPS-OBKR                                          
058000     PERFORM S31-BEHANDLA-VIPS-OBKR                                       
058100     .                                                                    
058200     EJECT                                                                
058300                                                                          
058400                                                                          
058500 CA-FLYTTA-VIPS-OBKR SECTION.                                             
058600                                                                          
058700     MOVE 'STA-CA    '    TO FELTEXT                                      
058800                                                                          
058900     MOVE DNOT-IDORDER             TO WS-VIPS-IDORDER                     
059000     MOVE DNOT-IDARTNR             TO WS-VIPS-IDARTNR                     
059100     MOVE DNOT-IDLOPNR             TO WS-VIPS-IDLOPNR                     
059200     MOVE DNOT-IDSEKVNR            TO WS-VIPS-IDSEKVNR                    
059300     MOVE DNOT-IDDC                TO WS-VIPS-IDDC                        
059400     MOVE DNOT-KDORDBEK            TO WS-VIPS-KDORDBEK                    
059500     MOVE DNOT-IDGMTREF            TO WS-VIPS-IDGMTREF                    
059600     MOVE DNOT-BEART-USA           TO WS-VIPS-BEART-USA                   
059700     MOVE DNOT-KDORDKL             TO WS-VIPS-KDORDKL                     
059800     MOVE DNOT-KVBEART             TO WS-VIPS-KVBEART                     
059900                                      WS-VIPS-KVBEART-Q                   
060000     .                                                                    
060100     EJECT                                                                
060200                                                                          
060300                                                                          
060400 D-BEHANDLA-UTSKRIFT SECTION.                                             
060500                                                                          
060600     MOVE 'STA-D     '    TO FELTEXT                                      
060700                                                                          
060800     MOVE DNOT-IDORDER             TO W-IDORDER-HUV                       
060900     MOVE DNOT-IDDC                TO W-IDDC-HUV                          
061000     PERFORM IMS-GU-RADB-HUVUD                                            
061100     IF SEGMENT-FINNS                                                     
061200        CONTINUE                                                          
061300     ELSE                                                                 
061400        PERFORM DA-SKAPA-ORDERHUVUD                                       
061500        PERFORM IMS-ISRT-RADB-HUVUD                                       
061600     END-IF                                                               
061700     PERFORM DB-BIPACKAD-ORDERRAD                                         
061800     PERFORM IMS-ISRT-RADB                                                
061900     PERFORM UNTIL SEGMENT-FINNS                                          
062000        ADD 1                   TO W-IDLOPNR                              
062100                                   WS-IDLOPNR                             
062200                                   RADB-IDLOPNR                           
062300        PERFORM IMS-ISRT-RADB                                             
062400     END-PERFORM                                                          
062500     .                                                                    
062600     EJECT                                                                
062700                                                                          
062800                                                                          
062900 DA-SKAPA-ORDERHUVUD SECTION.                                             
063000                                                                          
063100     MOVE 'STA-DA    '    TO FELTEXT                                      
063200                                                                          
063300     INITIALIZE HUVUD-RADB-WDQ501                                         
063400     MOVE NEJ                      TO HUVUD-RADB-FLTILLK                  
063500                                      HUVUD-RADB-FLDIRLEV                 
063600                                                                          
063700     MOVE DNOT-IDORDER             TO W-IDORDER-HUV                       
063800     MOVE ZERO                     TO W-IDARTNR-HUV                       
063900     MOVE +001                     TO W-IDLOPNR-HUV                       
064000     MOVE +001                     TO W-IDSEKVNR-HUV                      
064100     MOVE DNOT-IDDC-PRIM           TO W-IDDC-HUV                          
064200     MOVE ZERO                     TO W-KDORDBEK-HUV                      
064300                                                                          
064400     MOVE DNOT-IDORDER             TO HUVUD-RADB-IDORDER                  
064500     MOVE ZERO                     TO HUVUD-RADB-IDARTNR                  
064600     MOVE +001                     TO HUVUD-RADB-IDLOPNR                  
064700     MOVE +001                     TO HUVUD-RADB-IDSEKVNR                 
064800     MOVE DNOT-IDDC-PRIM           TO HUVUD-RADB-IDDC                     
064900     MOVE ZERO                     TO HUVUD-RADB-KDORDBEK                 
065000                                                                          
065100     MOVE DNOT-ADGMT               TO HUVUD-RADB-ADGMT                    
065200     MOVE DNOT-BEGMT               TO HUVUD-RADB-BEGMT                    
065300     MOVE DNOT-IDGMTREF            TO HUVUD-RADB-IDGMTREF                 
065500     MOVE DNOT-BEKUNDRF            TO HUVUD-RADB-BEKUNDRF                 
065600     MOVE DNOT-KDFRAKT             TO HUVUD-RADB-KDFRAKT                  
065700     MOVE DNOT-KDORDKL             TO HUVUD-RADB-KDORDKL                  
065800     MOVE DNOT-TIREGDAT            TO HUVUD-RADB-TIREGDAT                 
065900     MOVE DNOT-TIREGTID            TO HUVUD-RADB-TIREGTID                 
066000     .                                                                    
066100     EJECT                                                                
066200                                                                          
066300                                                                          
066400 DB-BIPACKAD-ORDERRAD SECTION.                                            
066500                                                                          
066600     MOVE 'STA-DB    '    TO FELTEXT                                      
066700                                                                          
066800     INITIALIZE RADB-WDQ501                                               
066900     MOVE +1              TO WS-IDLOPNR                                   
067000                             WS-IDSEKVNR                                  
067100     MOVE NEJ                      TO RADB-FLTILLK                        
067200                                      RADB-FLDIRLEV                       
067300                                                                          
067400     MOVE DNOT-IDORDER             TO W-IDORDER                           
067500     MOVE DNOT-IDARTNR             TO W-IDARTNR                           
067600     MOVE WS-IDLOPNR               TO W-IDLOPNR                           
067700     MOVE WS-IDSEKVNR              TO W-IDSEKVNR                          
067800     MOVE DNOT-IDDC                TO W-IDDC                              
067900     MOVE DNOT-KDORDBEK            TO W-KDORDBEK                          
068000                                                                          
068100     MOVE DNOT-IDORDER             TO RADB-IDORDER                        
068200     MOVE DNOT-IDARTNR             TO RADB-IDARTNR                        
068300     MOVE WS-IDLOPNR               TO RADB-IDLOPNR                        
068400     MOVE WS-IDSEKVNR              TO RADB-IDSEKVNR                       
068500     MOVE DNOT-IDDC                TO RADB-IDDC                           
068600     MOVE DNOT-KDORDBEK            TO RADB-KDORDBEK                       
068700                                                                          
068800     MOVE DNOT-BERADREF            TO RADB-BERADREF                       
068900     MOVE DNOT-FLTILLK             TO RADB-FLTILLK                        
069000     MOVE DNOT-FLDIRLEV            TO RADB-FLDIRLEV                       
069200     MOVE DNOT-IDGMTREF            TO RADB-IDGMTREF                       
069300     MOVE DNOT-IDKUNDRF-RO         TO RADB-IDKUNDRF-RO                    
069400     MOVE DNOT-KDFRAKT             TO RADB-KDFRAKT                        
069500     MOVE DNOT-KDORDKL             TO RADB-KDORDKL                        
069600     MOVE DNOT-KVBEART             TO RADB-KVBEART                        
069700     MOVE DNOT-KVBEART-Q           TO RADB-KVBEART-Q                      
069800     MOVE DNOT-REKSIFFR            TO RADB-REKSIFFR                       
069900     MOVE DNOT-TIREGDAT            TO RADB-TIREGDAT                       
070000     MOVE DNOT-TIREGTID            TO RADB-TIREGTID                       
070100     MOVE DNOT-IDARTNR             TO W-BENA-IDARTNR                      
070200     PERFORM S51-HAMTA-BENAMNING                                          
070300     MOVE WS-BEART-USA             TO RADB-BEART-USA                      
070400     .                                                                    
070500     EJECT                                                                
070600                                                                          
071039 E-BEHANDLA-PACKNING SECTION.                                             
071040                                                                          
071050     MOVE 'STA-E     '    TO FELTEXT                                      
071100                                                                          
071200* PÅ KVBEART FRÅN PACKNINGEN ÄR ANTAL KVANTANPASSAT                       
071300                                                                          
071400                                                                          
071500*DENNA SLINGA FÖR ATT PACKA I ETT KOLLI SOM REDAN FINNS                   
071600*MAN KAN I SÅ FALL BARA KOMMA FRÅN 4314                                   
071700                                                                          
071800     MOVE NEJ                         TO TRAFF-SW                         
071900                                                                          
072000     IF DNOT-IDPGM = 'W4031400'                                           
072100                                                                          
072200        MOVE LOW-VALUE                TO W-WDQ501KY-MIN-X                 
072300        MOVE HIGH-VALUE               TO W-WDQ501KY-MAX-X                 
072400        MOVE DNOT-IDORDER             TO W-IDORDER-MIN                    
072500                                         W-IDORDER-MAX                    
072600        MOVE DNOT-IDARTNR             TO W-IDARTNR-MIN                    
072700                                         W-IDARTNR-MAX                    
072800                                                                          
072900        MOVE ZERO                     TO WS-KVLEVART-ANDR                 
073000        MOVE NEJ                      TO TRAFF-SW                         
073100                                                                          
073200        PERFORM IMS-GHU-RADB-MIN-MAX                                      
073300        PERFORM UNTIL (SEGMENT-SAKNAS)                                    
073400                   OR (BASEN-SLUT)                                        
073500                   OR (TRAFF)                                             
073600           IF DNOT-IDDC = RADB-IDDC                                       
073700              IF (((DNOT-KVBEART     = RADB-KVBEART)                      
073800              OR   (DNOT-KVBEART    = RADB-KVBEART-Q))                    
073900                                                        AND               
074000                  ((DNOT-IDKUNDRF-RO(1:5) =                               
074100                               RADB-IDKUNDRF-RO(3:5)))                    
074200                                                        AND               
074300                  ((DNOT-FLTILLK     = RADB-FLTILLK)                      
074400              OR   ((DNOT-FLTILLK = NEJ) AND (RADB-FLTILLK = JA))         
074500              OR   ((DNOT-FLTILLK = NEJ) AND                              
074600                                    (RADB-FLTILLK = VIPS))))              
074700                                                                          
074800                 IF DNOT-IDPURAD = RADB-IDPURAD                           
074900                 AND DNOT-IDKOLLI = RADB-IDKOLLI                          
075000                     MOVE JA TO TRAFF-SW                                  
075100                     COMPUTE WS-KVLEVART-ANDR =                           
075200                             DNOT-KVLEVART - RADB-KVLEVART                
075300                     COMPUTE RADB-KVLEVART =                              
075400                             RADB-KVLEVART + WS-KVLEVART-ANDR             
075500                     PERFORM IMS-REPL-RADB                                
075600                     MOVE WS-KVLEVART-ANDR TO FELTEXT                     
075700                     PERFORM EF-KVLEVART-TOT-SAMMA-KOLLI                  
075800                 END-IF                                                   
075900              END-IF                                                      
076000           END-IF                                                         
076100           PERFORM IMS-GHN-RADB-MIN-MAX                                   
076200        END-PERFORM                                                       
076300     END-IF                                                               
076400                                                                          
076500     IF NO-TRAFF                                                          
076600* DENNA SLINGA FÖR ATT PACKA I FÖRSTA GÅNGEN I 'FÖRSTA KOLLIT'            
076700* ELLER I NYTT KOLLI.                                                     
076800* I DETTA FALL KAN MAN KOMMA IFRÅN 4314, 4315 ELLER W403AVSP              
076900                                                                          
077000        MOVE LOW-VALUE                TO W-WDQ501KY-MIN-X                 
077100        MOVE HIGH-VALUE               TO W-WDQ501KY-MAX-X                 
077200        MOVE DNOT-IDORDER             TO W-IDORDER-MIN                    
077300                                         W-IDORDER-MAX                    
077400        MOVE DNOT-IDARTNR             TO W-IDARTNR-MIN                    
077500                                         W-IDARTNR-MAX                    
077600                                                                          
077700        MOVE NEJ                      TO TRAFF-SW                         
077800                                                                          
077900        PERFORM IMS-GU-RADB-MIN-MAX                                       
078000        PERFORM UNTIL (SEGMENT-SAKNAS)                                    
078100                   OR (BASEN-SLUT)                                        
078200                   OR (TRAFF)                                             
078300           IF DNOT-IDDC = RADB-IDDC                                       
078400              IF (((DNOT-KVBEART     = RADB-KVBEART)                      
078500              OR   (DNOT-KVBEART    = RADB-KVBEART-Q))                    
078600                                                        AND               
078700                  ((DNOT-IDKUNDRF-RO(1:5) =                               
078800                               RADB-IDKUNDRF-RO(3:5)))                    
078900                                                        AND               
079000                  ((DNOT-FLTILLK     = RADB-FLTILLK)                      
079100              OR   ((DNOT-FLTILLK = NEJ) AND (RADB-FLTILLK = JA))         
079200              OR   ((DNOT-FLTILLK = NEJ) AND                              
079300                                   (RADB-FLTILLK = VIPS))))               
079400                                                                          
079500                 IF RADB-IDPURAD = ZERO                                   
079600                 AND RADB-IDKOLLI = ZERO                                  
079700*                   ARTIKELN HAR INTE PACKRAPPORTERATS TIDIGARE           
079800                    MOVE JA TO TRAFF-SW                                   
079900                    PERFORM EB-NYCKLAR-PACKNING                           
080000                    PERFORM IMS-GHU-RADB                                  
080100                    IF SEGMENT-FINNS                                      
080200                       PERFORM EC-FLYTTA-PACKINFO                         
080300                       PERFORM IMS-REPL-RADB                              
080400                    END-IF                                                
080500                 ELSE                                                     
080600                    IF DNOT-IDPURAD = RADB-IDPURAD                        
080700                    AND DNOT-IDKOLLI NOT = RADB-IDKOLLI                   
080800                       MOVE JA TO TRAFF-SW                                
080900                       PERFORM ED-SKAPA-NYTT-KOLLI                        
081000                       PERFORM IMS-ISRT-RADB                              
081100                       PERFORM UNTIL SEGMENT-FINNS                        
081200                          ADD +1      TO W-IDSEKVNR                       
081300                                         RADB-IDSEKVNR                    
081400                          PERFORM IMS-ISRT-RADB                           
081500                       END-PERFORM                                        
081600                       PERFORM EE-UPPDAT-KVLEVART-TOT                     
081700                    END-IF                                                
081800                 END-IF                                                   
081900              END-IF                                                      
082000           END-IF                                                         
082100           PERFORM IMS-GN-RADB-MIN-MAX                                    
082200        END-PERFORM                                                       
082300     END-IF                                                               
082400     .                                                                    
082500     EJECT                                                                
082600                                                                          
082700                                                                          
082800                                                                          
082900 EB-NYCKLAR-PACKNING SECTION.                                             
083000                                                                          
083100     MOVE 'STA-EB    '    TO FELTEXT                                      
083200                                                                          
083300     MOVE RADB-IDORDER             TO W-IDORDER                           
083400     MOVE RADB-IDARTNR             TO W-IDARTNR                           
083500     MOVE RADB-IDLOPNR             TO W-IDLOPNR                           
083600     MOVE RADB-IDSEKVNR            TO W-IDSEKVNR                          
083700     MOVE RADB-IDDC                TO W-IDDC                              
083800     MOVE RADB-KDORDBEK            TO W-KDORDBEK                          
083900     .                                                                    
084000     EJECT                                                                
084100                                                                          
084200                                                                          
084300 EC-FLYTTA-PACKINFO SECTION.                                              
084400                                                                          
084500     MOVE 'STA-EC    '    TO FELTEXT                                      
084600                                                                          
084602     IF DNOT-IDPGM = 'WL0134NY'                                           
084603     OR DNOT-IDPGM = 'WL019900'                                           
084610       MOVE DNOT-IDARTNR           TO W-BENA-IDARTNR                      
084620       PERFORM S51-HAMTA-BENAMNING                                        
084630       MOVE WS-BEART-USA           TO DNOT-BEART-USA                      
084631     END-IF                                                               
084640                                                                          
084700     MOVE DNOT-IDKOLLI             TO RADB-IDKOLLI                        
084800     MOVE DNOT-IDPURAD             TO RADB-IDPURAD                        
084900     MOVE DNOT-BEART-USA           TO RADB-BEART-USA                      
085000     MOVE DNOT-KVLEVART            TO RADB-KVLEVART                       
085100                                      RADB-KVLEVART-TOT                   
085200     .                                                                    
085300     EJECT                                                                
085400                                                                          
085500                                                                          
085600 ED-SKAPA-NYTT-KOLLI SECTION.                                             
085700                                                                          
085800     MOVE 'STA-ED    '    TO FELTEXT                                      
085900                                                                          
085910     IF DNOT-IDPGM = 'WL0134NY'                                           
085920     OR DNOT-IDPGM = 'WL019900'                                           
085940       MOVE DNOT-IDARTNR           TO W-BENA-IDARTNR                      
085950       PERFORM S51-HAMTA-BENAMNING                                        
085960       MOVE WS-BEART-USA           TO RADB-BEART-USA                      
085970     END-IF                                                               
085980                                                                          
086000     ADD +1                        TO W-IDSEKVNR                          
086100                                      RADB-IDSEKVNR                       
086200     MOVE DNOT-IDKOLLI             TO RADB-IDKOLLI                        
086300     MOVE DNOT-IDPURAD             TO RADB-IDPURAD                        
086400     MOVE DNOT-KVLEVART            TO RADB-KVLEVART                       
086410                                                                          
086500     .                                                                    
086600     EJECT                                                                
086700                                                                          
086800                                                                          
086900 EE-UPPDAT-KVLEVART-TOT SECTION.                                          
087000                                                                          
087100     MOVE 'STA-EE    '    TO FELTEXT                                      
087200                                                                          
087300     MOVE LOW-VALUE                TO W-WDQ501KY-NEWCASE-MIN-X            
087400     MOVE HIGH-VALUE               TO W-WDQ501KY-NEWCASE-MAX-X            
087500     MOVE DNOT-IDORDER             TO W-IDORDER-NEWCASE-MIN               
087600                                      W-IDORDER-NEWCASE-MAX               
087700     MOVE DNOT-IDARTNR             TO W-IDARTNR-NEWCASE-MIN               
087800                                      W-IDARTNR-NEWCASE-MAX               
087900     MOVE RADB-IDLOPNR             TO W-IDLOPNR-NEWCASE-MIN               
088000                                      W-IDLOPNR-NEWCASE-MAX               
088100                                                                          
088200                                                                          
088300                                                                          
088400     PERFORM IMS-GHU-RADB-NEWCASE-MIN-MAX                                 
088500     PERFORM UNTIL ((ORQP3-BASEN-SLUT) OR                                 
088600                    (ORQP3-SEGMENT-SAKNAS))                               
088700        COMPUTE NEWCASE-RADB-KVLEVART-TOT =                               
088800                NEWCASE-RADB-KVLEVART-TOT + DNOT-KVLEVART                 
088900        PERFORM IMS-REPL-RADB-NEWCASE                                     
089000        PERFORM IMS-GHN-RADB-NEWCASE-MIN-MAX                              
089100     END-PERFORM                                                          
089200     .                                                                    
089300     EJECT                                                                
089400                                                                          
089500                                                                          
089600 EF-KVLEVART-TOT-SAMMA-KOLLI SECTION.                                     
089700                                                                          
089800     MOVE 'STA-EF    '    TO FELTEXT                                      
089900                                                                          
090000     MOVE LOW-VALUE                TO W-WDQ501KY-NEWCASE-MIN-X            
090100     MOVE HIGH-VALUE               TO W-WDQ501KY-NEWCASE-MAX-X            
090200     MOVE DNOT-IDORDER             TO W-IDORDER-NEWCASE-MIN               
090300                                      W-IDORDER-NEWCASE-MAX               
090400     MOVE DNOT-IDARTNR             TO W-IDARTNR-NEWCASE-MIN               
090500                                      W-IDARTNR-NEWCASE-MAX               
090600     MOVE RADB-IDLOPNR             TO W-IDLOPNR-NEWCASE-MIN               
090700                                      W-IDLOPNR-NEWCASE-MAX               
090800                                                                          
090900                                                                          
091000                                                                          
091100     PERFORM IMS-GHU-RADB-NEWCASE-MIN-MAX                                 
091200     PERFORM UNTIL ((ORQP3-BASEN-SLUT) OR                                 
091300                    (ORQP3-SEGMENT-SAKNAS))                               
091400        COMPUTE NEWCASE-RADB-KVLEVART-TOT =                               
091500                NEWCASE-RADB-KVLEVART-TOT + WS-KVLEVART-ANDR              
091600        PERFORM IMS-REPL-RADB-NEWCASE                                     
091700        PERFORM IMS-GHN-RADB-NEWCASE-MIN-MAX                              
091800     END-PERFORM                                                          
091900     .                                                                    
092000     EJECT                                                                
092100                                                                          
092200                                                                          
092300 F-BEHANDLA-ANNULLERING SECTION.                                          
092400                                                                          
092500     MOVE 'STA-F     '    TO FELTEXT                                      
092600                                                                          
092700     MOVE LOW-VALUE                TO W-WDQ501KY-MIN-X                    
092800     MOVE HIGH-VALUE               TO W-WDQ501KY-MAX-X                    
092900     MOVE DNOT-IDORDER             TO W-IDORDER-MIN                       
093000                                      W-IDORDER-MAX                       
093100     PERFORM IMS-GHU-RADB-MIN-MAX                                         
093200     PERFORM UNTIL SEGMENT-SAKNAS                                         
093300        PERFORM IMS-DLET-RADB                                             
093400        PERFORM IMS-GHN-RADB-MIN-MAX                                      
093500     END-PERFORM                                                          
093600     .                                                                    
093700     EJECT                                                                
093800                                                                          
093810 M-REMOVE-OF-DEFAULT-CASE     SECTION.                                    
093910     MOVE ' M-REMOVE-OF-DEFAULT-CASE   '    TO FELTEXT                    
093920                                                                          
093940                                                                          
093950     MOVE LOW-VALUE                TO W-WDQ501KY-MIN-X                    
093960     MOVE HIGH-VALUE               TO W-WDQ501KY-MAX-X                    
093970     MOVE DNOT-IDORDER             TO W-IDORDER-MIN                       
093980                                      W-IDORDER-MAX                       
093990     MOVE DNOT-IDARTNR             TO W-IDARTNR-MIN                       
093991                                      W-IDARTNR-MAX                       
093994     PERFORM IMS-GHU-RADB-MIN-MAX                                         
093995                                                                          
093996     IF SEGMENT-FINNS                                                     
093997                                                                          
094001       IF  RADB-IDDC    = DNOT-IDDC                                       
094002       AND RADB-IDKOLLI = DNOT-IDKOLLI-BORT                               
094003       AND RADB-IDPURAD = DNOT-IDPURAD                                    
094004                                                                          
094005         COMPUTE RADB-KVLEVART-TOT =                                      
094006                 RADB-KVLEVART-TOT - RADB-KVLEVART                        
094009         MOVE ZERO          TO RADB-IDKOLLI                               
094010         MOVE ZERO          TO RADB-IDPURAD                               
094011         MOVE ZERO          TO RADB-KVLEVART                              
094014         PERFORM IMS-REPL-RADB                                            
094015       END-IF                                                             
094019     END-IF                                                               
094020     .                                                                    
094021     EJECT                                                                
094022                                                                          
094023                                                                          
094030 G-BEHANDLA-BORTTAG-AV-KOLLI SECTION.                                     
094100                                                                          
094200     MOVE 'STA-G     '    TO FELTEXT                                      
094300                                                                          
094400     MOVE NEJ                      TO TRAFF-SW                            
094500                                                                          
094600     MOVE LOW-VALUE                TO W-WDQ501KY-MIN-X                    
094700     MOVE HIGH-VALUE               TO W-WDQ501KY-MAX-X                    
094800     MOVE DNOT-IDORDER             TO W-IDORDER-MIN                       
094900                                      W-IDORDER-MAX                       
095000     MOVE DNOT-IDARTNR             TO W-IDARTNR-MIN                       
095100                                      W-IDARTNR-MAX                       
095200     PERFORM IMS-GHU-RADB-MIN-MAX                                         
095300     PERFORM UNTIL ((BASEN-SLUT)     OR                                   
095400                    (SEGMENT-SAKNAS) OR                                   
095500                    (TRAFF))                                              
095600        IF RADB-IDDC    = DNOT-IDDC    AND                                
095700           RADB-IDPURAD = DNOT-IDPURAD AND                                
095800           RADB-IDKOLLI = DNOT-IDKOLLI-BORT                               
095900           MOVE JA                  TO TRAFF-SW                           
095910                                                                          
096000           PERFORM GA-BEH-FLER-KOLLIN                                     
096100           IF NEWCASE-TRAFF                                               
096200              PERFORM IMS-DLET-RADB                                       
096300           ELSE                                                           
096400              COMPUTE RADB-KVLEVART-TOT =                                 
096500                      RADB-KVLEVART-TOT - RADB-KVLEVART                   
096600              MOVE ZERO               TO RADB-IDPURAD                     
096700                                         RADB-IDKOLLI                     
096800                                         RADB-KVLEVART                    
096900              MOVE SPACE              TO RADB-BEART-USA                   
097000              PERFORM IMS-REPL-RADB                                       
097100           END-IF                                                         
097200        END-IF                                                            
097300        PERFORM IMS-GHN-RADB-MIN-MAX                                      
097400     END-PERFORM                                                          
097500     .                                                                    
097600     EJECT                                                                
097700                                                                          
097800                                                                          
097900 GA-BEH-FLER-KOLLIN SECTION.                                              
098000                                                                          
098100     MOVE NEJ                     TO NEWCASE-TRAFF-SW                     
098200     MOVE LOW-VALUE               TO W-WDQ501KY-NEWCASE-MIN-X             
098300     MOVE HIGH-VALUE              TO W-WDQ501KY-NEWCASE-MAX-X             
098400     MOVE DNOT-IDORDER            TO W-IDORDER-NEWCASE-MIN                
098500                                     W-IDORDER-NEWCASE-MAX                
098600     MOVE DNOT-IDARTNR            TO W-IDARTNR-NEWCASE-MIN                
098700                                     W-IDARTNR-NEWCASE-MAX                
098800                                                                          
098900     PERFORM IMS-GHU-RADB-NEWCASE-MIN-MAX                                 
099000     PERFORM UNTIL ((ORQP3-BASEN-SLUT) OR                                 
099100                    (ORQP3-SEGMENT-SAKNAS))                               
099200                                                                          
099300        IF NEWCASE-RADB-IDDC         = DNOT-IDDC                          
099400           IF NEWCASE-RADB-IDPURAD   = DNOT-IDPURAD                       
099500             IF NEWCASE-RADB-IDKOLLI NOT = DNOT-IDKOLLI-BORT              
099600               COMPUTE NEWCASE-RADB-KVLEVART-TOT =                        
099700                       NEWCASE-RADB-KVLEVART-TOT - RADB-KVLEVART          
099800               MOVE JA TO NEWCASE-TRAFF-SW                                
099900               PERFORM IMS-REPL-RADB-NEWCASE                              
100000             END-IF                                                       
100100           END-IF                                                         
100200        END-IF                                                            
100300        PERFORM IMS-GHN-RADB-NEWCASE-MIN-MAX                              
100400     END-PERFORM                                                          
100500     .                                                                    
100600     EJECT                                                                
100700                                                                          
100800                                                                          
100900 H-BEHANDLA-ANDRING-AV-KOLLI SECTION.                                     
101000                                                                          
101100     MOVE 'STA-H     '    TO FELTEXT                                      
101200                                                                          
101300     IF DNOT-IDARTNR = ZERO                                               
101400        MOVE LOW-VALUE                TO W-WDQ501KY-MIN-X                 
101500        MOVE HIGH-VALUE               TO W-WDQ501KY-MAX-X                 
101600        MOVE DNOT-IDORDER             TO W-IDORDER-MIN                    
101700                                         W-IDORDER-MAX                    
101800        MOVE +001                      TO W-IDARTNR-MIN                   
101900        PERFORM IMS-GHU-RADB-MIN-MAX                                      
102000        PERFORM UNTIL ((BASEN-SLUT)     OR                                
102100                       (SEGMENT-SAKNAS))                                  
102200           IF RADB-IDDC = DNOT-IDDC                                       
102300              IF RADB-IDKOLLI = DNOT-IDKOLLI-BORT                         
102400                 PERFORM HA-MOVE-ALL-IN-ONE-CASE                          
102500              END-IF                                                      
102600           END-IF                                                         
102700           PERFORM IMS-GHN-RADB-MIN-MAX                                   
102800        END-PERFORM                                                       
102900     ELSE                                                                 
103000        MOVE ZERO TO WS-KVLEVART-KVAR                                     
103100                                                                          
103200        MOVE NEJ                      TO TRAFF-SW                         
103300        MOVE LOW-VALUE                TO W-WDQ501KY-MIN-X                 
103400        MOVE HIGH-VALUE               TO W-WDQ501KY-MAX-X                 
103500        MOVE DNOT-IDORDER             TO W-IDORDER-MIN                    
103600                                         W-IDORDER-MAX                    
103700        MOVE DNOT-IDARTNR             TO W-IDARTNR-MIN                    
103800                                         W-IDARTNR-MAX                    
103900        PERFORM IMS-GHU-RADB-MIN-MAX                                      
104000        PERFORM UNTIL ((BASEN-SLUT)     OR                                
104100                       (SEGMENT-SAKNAS) OR                                
104200                       (TRAFF))                                           
104300           IF RADB-IDDC         = DNOT-IDDC                               
104400              IF RADB-IDPURAD   = DNOT-IDPURAD                            
104500                IF RADB-IDKOLLI = DNOT-IDKOLLI-BORT                       
104600                  MOVE JA           TO TRAFF-SW                           
104700                  COMPUTE WS-KVLEVART-KVAR =                              
104800                          RADB-KVLEVART - DNOT-KVLEVART                   
104900                  IF WS-KVLEVART-KVAR = ZERO                              
105000                     PERFORM  HB-PART-TOT-MOVED-FR-OLD-CASE               
105100                  ELSE                                                    
105200                     PERFORM HC-PART-ALSO-IN-OLD-CASE                     
105300                  END-IF                                                  
105400                END-IF                                                    
105500              END-IF                                                      
105600           END-IF                                                         
105700           IF NO-TRAFF                                                    
105800              PERFORM IMS-GHN-RADB-MIN-MAX                                
105900           END-IF                                                         
106000        END-PERFORM                                                       
106100     END-IF                                                               
106200     .                                                                    
106300     EJECT                                                                
106400                                                                          
106500                                                                          
106600 HA-MOVE-ALL-IN-ONE-CASE SECTION.                                         
106700                                                                          
106800     MOVE 'STA-HA    '    TO FELTEXT                                      
106900                                                                          
107000     MOVE NEJ                     TO NEWCASE-TRAFF-SW                     
107100     MOVE LOW-VALUE               TO W-WDQ501KY-NEWCASE-MIN-X             
107200     MOVE HIGH-VALUE              TO W-WDQ501KY-NEWCASE-MAX-X             
107300     MOVE DNOT-IDORDER            TO W-IDORDER-NEWCASE-MIN                
107400                                     W-IDORDER-NEWCASE-MAX                
107500     MOVE RADB-IDARTNR            TO W-IDARTNR-NEWCASE-MIN                
107600                                     W-IDARTNR-NEWCASE-MAX                
107700     PERFORM IMS-GHU-RADB-NEWCASE-MIN-MAX                                 
107800     PERFORM UNTIL ((ORQP3-BASEN-SLUT) OR                                 
107900                    (ORQP3-SEGMENT-SAKNAS) OR                             
108000                    (NEWCASE-TRAFF))                                      
108100        IF NEWCASE-RADB-IDDC         = DNOT-IDDC                          
108200           IF NEWCASE-RADB-IDPURAD   = RADB-IDPURAD                       
108300             IF NEWCASE-RADB-IDKOLLI = DNOT-IDKOLLI                       
108400               MOVE JA           TO NEWCASE-TRAFF-SW                      
108500               COMPUTE NEWCASE-RADB-KVLEVART =                            
108600                       NEWCASE-RADB-KVLEVART + RADB-KVLEVART              
108700               PERFORM IMS-REPL-RADB-NEWCASE                              
108800               PERFORM IMS-DLET-RADB                                      
108900             END-IF                                                       
109000           END-IF                                                         
109100        END-IF                                                            
109200        IF NO-NEWCASE                                                     
109300           PERFORM IMS-GHN-RADB-NEWCASE-MIN-MAX                           
109400        END-IF                                                            
109500     END-PERFORM                                                          
109600     IF NO-NEWCASE                                                        
109700        MOVE DNOT-IDKOLLI     TO RADB-IDKOLLI                             
109800        PERFORM IMS-REPL-RADB                                             
109900     END-IF                                                               
110000     .                                                                    
110100     EJECT                                                                
110200                                                                          
110300                                                                          
110400 HB-PART-TOT-MOVED-FR-OLD-CASE SECTION.                                   
110500                                                                          
110600     MOVE 'STA-HB    '    TO FELTEXT                                      
110700                                                                          
110800     MOVE NEJ                     TO NEWCASE-TRAFF-SW                     
110900     MOVE LOW-VALUE               TO W-WDQ501KY-NEWCASE-MIN-X             
111000     MOVE HIGH-VALUE              TO W-WDQ501KY-NEWCASE-MAX-X             
111100     MOVE DNOT-IDORDER            TO W-IDORDER-NEWCASE-MIN                
111200                                     W-IDORDER-NEWCASE-MAX                
111300     MOVE DNOT-IDARTNR            TO W-IDARTNR-NEWCASE-MIN                
111400                                     W-IDARTNR-NEWCASE-MAX                
111500     PERFORM IMS-GHU-RADB-NEWCASE-MIN-MAX                                 
111600     PERFORM UNTIL ((ORQP3-BASEN-SLUT) OR                                 
111700                    (ORQP3-SEGMENT-SAKNAS) OR                             
111800                    (NEWCASE-TRAFF))                                      
111900        IF NEWCASE-RADB-IDDC         = DNOT-IDDC                          
112000           IF NEWCASE-RADB-IDPURAD   = DNOT-IDPURAD                       
112100             IF NEWCASE-RADB-IDKOLLI = DNOT-IDKOLLI                       
112200               MOVE JA           TO NEWCASE-TRAFF-SW                      
112300               COMPUTE NEWCASE-RADB-KVLEVART =                            
112400                       NEWCASE-RADB-KVLEVART + DNOT-KVLEVART              
112500               PERFORM IMS-REPL-RADB-NEWCASE                              
112600               PERFORM IMS-DLET-RADB                                      
112700             END-IF                                                       
112800           END-IF                                                         
112900        END-IF                                                            
113000        PERFORM IMS-GHN-RADB-NEWCASE-MIN-MAX                              
113100     END-PERFORM                                                          
113200     IF NO-NEWCASE                                                        
113300        MOVE DNOT-IDKOLLI     TO RADB-IDKOLLI                             
113400        PERFORM IMS-REPL-RADB                                             
113500     END-IF                                                               
113600     .                                                                    
113700     EJECT                                                                
113800                                                                          
113900                                                                          
114000 HC-PART-ALSO-IN-OLD-CASE SECTION.                                        
114100                                                                          
114200     MOVE 'STA-HC    '    TO FELTEXT                                      
114300                                                                          
114400     MOVE NEJ                     TO NEWCASE-TRAFF-SW                     
114500     MOVE LOW-VALUE               TO W-WDQ501KY-NEWCASE-MIN-X             
114600     MOVE HIGH-VALUE              TO W-WDQ501KY-NEWCASE-MAX-X             
114700     MOVE DNOT-IDORDER            TO W-IDORDER-NEWCASE-MIN                
114800                                     W-IDORDER-NEWCASE-MAX                
114900     MOVE DNOT-IDARTNR            TO W-IDARTNR-NEWCASE-MIN                
115000                                     W-IDARTNR-NEWCASE-MAX                
115100     PERFORM IMS-GHU-RADB-NEWCASE-MIN-MAX                                 
115200     PERFORM UNTIL ((ORQP3-BASEN-SLUT) OR                                 
115300                    (ORQP3-SEGMENT-SAKNAS) OR                             
115400                    (NEWCASE-TRAFF))                                      
115500        IF NEWCASE-RADB-IDDC         = DNOT-IDDC                          
115600           IF NEWCASE-RADB-IDPURAD   = DNOT-IDPURAD                       
115700             IF NEWCASE-RADB-IDKOLLI = DNOT-IDKOLLI                       
115800               MOVE JA           TO NEWCASE-TRAFF-SW                      
115900                                                                          
116000               COMPUTE NEWCASE-RADB-KVLEVART =                            
116100                       NEWCASE-RADB-KVLEVART + DNOT-KVLEVART              
116200               PERFORM IMS-REPL-RADB-NEWCASE                              
116300                                                                          
116400               COMPUTE RADB-KVLEVART =                                    
116500                       RADB-KVLEVART - DNOT-KVLEVART                      
116600               PERFORM IMS-REPL-RADB                                      
116700                                                                          
116800             END-IF                                                       
116900           END-IF                                                         
117000        END-IF                                                            
117100        PERFORM IMS-GHN-RADB-NEWCASE-MIN-MAX                              
117200     END-PERFORM                                                          
117300                                                                          
117400     IF NO-NEWCASE                                                        
117500        INITIALIZE NEWCASE-RADB-WDQ501                                    
117600        MOVE RADB-WDQ501           TO NEWCASE-RADB-WDQ501                 
117700        MOVE DNOT-KVLEVART         TO NEWCASE-RADB-KVLEVART               
117800        MOVE DNOT-IDKOLLI          TO NEWCASE-RADB-IDKOLLI                
117900        MOVE NEWCASE-RADB-IDORDER  TO W-IDORDER-NEWCASE                   
118000        MOVE NEWCASE-RADB-IDARTNR  TO W-IDARTNR-NEWCASE                   
118100        MOVE NEWCASE-RADB-IDLOPNR  TO W-IDLOPNR-NEWCASE                   
118200        MOVE NEWCASE-RADB-IDSEKVNR TO W-IDSEKVNR-NEWCASE                  
118300        MOVE NEWCASE-RADB-IDDC     TO W-IDDC-NEWCASE                      
118400        MOVE NEWCASE-RADB-KDORDBEK TO W-KDORDBEK-NEWCASE                  
118500        PERFORM IMS-ISRT-RADB-NEWCASE                                     
118600        PERFORM UNTIL ORQP3-SEGMENT-FINNS                                 
118700           ADD 1                   TO W-IDSEKVNR-NEWCASE                  
118800                                      NEWCASE-RADB-IDSEKVNR               
118900           PERFORM IMS-ISRT-RADB-NEWCASE                                  
119000        END-PERFORM                                                       
119100                                                                          
119200        COMPUTE RADB-KVLEVART =                                           
119300                RADB-KVLEVART - DNOT-KVLEVART                             
119400        PERFORM IMS-REPL-RADB                                             
119500                                                                          
119600     END-IF                                                               
119700                                                                          
119800     .                                                                    
119900     EJECT                                                                
120000                                                                          
120100                                                                          
120200 I-UPPDATERA-PRINTDAG-DELNOTE SECTION.                                    
120300                                                                          
120400     MOVE 'STA-I     '    TO FELTEXT                                      
120500                                                                          
120600     MOVE LOW-VALUE                TO W-WDQ501KY-MIN-X                    
120700     MOVE HIGH-VALUE               TO W-WDQ501KY-MAX-X                    
120800     MOVE DNOT-IDORDER             TO W-IDORDER-MIN                       
120900                                      W-IDORDER-MAX                       
121000                                                                          
121100     PERFORM IMS-GHU-RADB-MIN-MAX                                         
121200     PERFORM UNTIL SEGMENT-SAKNAS                                         
121300        IF DNOT-IDDC = RADB-IDDC                                          
121400           MOVE DAGENS-DATUM          TO RADB-TIUPPDAT                    
121500           PERFORM IMS-REPL-RADB                                          
121600        END-IF                                                            
121700        PERFORM IMS-GHN-RADB-MIN-MAX                                      
121800     END-PERFORM                                                          
121900     .                                                                    
122000     EJECT                                                                
122100                                                                          
122200                                                                          
122300 K-TILLAGG-ORDERRAD SECTION.                                              
122400                                                                          
122500     MOVE 'STA-K     '    TO FELTEXT                                      
122600                                                                          
122700     MOVE LOW-VALUE                TO W-WDQ501KY-MIN-X                    
122800     MOVE HIGH-VALUE               TO W-WDQ501KY-MAX-X                    
122900     MOVE DNOT-IDORDER             TO W-IDORDER-MIN                       
123000                                      W-IDORDER-MAX                       
123100     MOVE ZERO                     TO W-IDARTNR-MIN                       
123200                                      W-IDARTNR-MAX                       
123300     PERFORM IMS-GN-RADB-MIN-MAX                                          
123400     IF SEGMENT-FINNS                                                     
123500        PERFORM KA-FLYTTA-TILLAGG-KUNDORDERRAD                            
123600        PERFORM IMS-ISRT-RADB                                             
123700        PERFORM UNTIL SEGMENT-FINNS                                       
123800           ADD 1                   TO W-IDSEKVNR                          
123900                                      WS-IDLOPNR                          
124000                                      RADB-IDLOPNR                        
124100           PERFORM IMS-ISRT-RADB                                          
124200        END-PERFORM                                                       
124300     END-IF                                                               
124400     .                                                                    
124500     EJECT                                                                
124600                                                                          
124700                                                                          
124800 KA-FLYTTA-TILLAGG-KUNDORDERRAD SECTION.                                  
124900                                                                          
125000     MOVE 'STA-KA    '    TO FELTEXT                                      
125100                                                                          
125200     INITIALIZE RADB-WDQ501                                               
125300     MOVE +1              TO WS-IDLOPNR                                   
125400                             WS-IDSEKVNR                                  
125500     MOVE NEJ                      TO RADB-FLTILLK                        
125600                                      RADB-FLDIRLEV                       
125700                                                                          
125800     MOVE DNOT-IDORDER             TO W-IDORDER                           
125900     MOVE DNOT-IDARTNR             TO W-IDARTNR                           
126000     MOVE WS-IDLOPNR               TO W-IDLOPNR                           
126100     MOVE WS-IDSEKVNR              TO W-IDSEKVNR                          
126200     MOVE DNOT-IDDC                TO W-IDDC                              
126300     MOVE DNOT-KDORDBEK            TO W-KDORDBEK                          
126400                                                                          
126500     MOVE DNOT-IDORDER             TO RADB-IDORDER                        
126600     MOVE DNOT-IDARTNR             TO RADB-IDARTNR                        
126700     MOVE WS-IDLOPNR               TO RADB-IDLOPNR                        
126800     MOVE WS-IDSEKVNR              TO RADB-IDSEKVNR                       
126900     MOVE DNOT-IDDC                TO RADB-IDDC                           
127000     MOVE DNOT-KDORDBEK            TO RADB-KDORDBEK                       
127100                                                                          
127200     MOVE DNOT-BERADREF            TO RADB-BERADREF                       
127300     MOVE DNOT-FLTILLK             TO RADB-FLTILLK                        
127400     MOVE DNOT-FLDIRLEV            TO RADB-FLDIRLEV                       
127500     MOVE DNOT-IDGMTREF            TO RADB-IDGMTREF                       
127600     MOVE DNOT-IDKUNDRF-RO         TO RADB-IDKUNDRF-RO                    
127800     MOVE DNOT-KDFRAKT             TO RADB-KDFRAKT                        
127900     MOVE DNOT-KDORDKL             TO RADB-KDORDKL                        
128000     MOVE DNOT-KVBEART             TO RADB-KVBEART                        
128100     MOVE DNOT-KVBEART-Q           TO RADB-KVBEART-Q                      
128200     MOVE DNOT-REKSIFFR            TO RADB-REKSIFFR                       
128300     MOVE DNOT-IDARTNR             TO W-BENA-IDARTNR                      
128400     PERFORM S51-HAMTA-BENAMNING                                          
128500     MOVE WS-BEART-USA             TO RADB-BEART-USA                      
128600     .                                                                    
128700     EJECT                                                                
128800                                                                          
128900                                                                          
129000 L-ANDRA-ORDERRAD SECTION.                                                
129100                                                                          
129200*OBS DNOT-KVBEART   INNEHÅLLER KVANNANT                                   
129300*    DNOT-KVBEART-Q INNEHÅLLER DET NYA BESTÄLLDA ANTALET                  
129400                                                                          
129500     MOVE ZERO TO WS-KVBEART                                              
129600     COMPUTE WS-KVBEART = DNOT-KVBEART-Q + DNOT-KVBEART                   
129700                                                                          
129800     MOVE LOW-VALUE                TO W-WDQ501KY-MIN-X                    
129900     MOVE HIGH-VALUE               TO W-WDQ501KY-MAX-X                    
130000     MOVE DNOT-IDORDER             TO W-IDORDER-MIN                       
130100                                      W-IDORDER-MAX                       
130200     MOVE DNOT-IDARTNR             TO W-IDARTNR-MIN                       
130300                                      W-IDARTNR-MAX                       
130400                                                                          
130500     PERFORM IMS-GHU-RADB-MIN-MAX                                         
130600     PERFORM UNTIL SEGMENT-SAKNAS                                         
130700        IF DNOT-IDDC = RADB-IDDC                                          
130800           IF RADB-KVBEART-Q   = WS-KVBEART                               
130900              IF DNOT-KVBEART > ZERO                                      
131000                 MOVE DNOT-KVBEART-Q TO RADB-KVBEART                      
131100                                        RADB-KVBEART-Q                    
131200              END-IF                                                      
131300              PERFORM IMS-REPL-RADB                                       
131400           END-IF                                                         
131500        END-IF                                                            
131600        PERFORM IMS-GHN-RADB-MIN-MAX                                      
131700     END-PERFORM                                                          
131800     .                                                                    
131900     EJECT                                                                
132000                                                                          
132100                                                                          
132200 S31-BEHANDLA-VIPS-OBKR SECTION.                                          
132300                                                                          
132400     MOVE 'STA-S31   '    TO FELTEXT                                      
132500                                                                          
132600                                                                          
132700     EVALUATE WS-VIPS-KDORDBEK                                            
132800                                                                          
132900        WHEN 41                                                           
133000           PERFORM S32-VIPS-41                                            
133100        WHEN 58                                                           
133200           PERFORM S33-VIPS-58                                            
133300        WHEN 61                                                           
133400           PERFORM S34-VIPS-61-TEXT                                       
133500     END-EVALUATE                                                         
133600     .                                                                    
133700     EJECT                                                                
133800                                                                          
133900                                                                          
134000 S32-VIPS-41 SECTION.                                                     
134100                                                                          
134200     MOVE 'STA-S32   '    TO FELTEXT                                      
134300                                                                          
134400* PÅ DET ANDRA OCH EV FÖLJANDE SEGMENT PÅ EN 41'A FRÅN VIPS               
134500* INNEHÅLLER BEART-USA DEN TILLKOMMANDE ARTIKELNS ARTIKELNUMMER           
134600                                                                          
134700     INITIALIZE RADB-WDQ501                                               
134800     IF WS-VIPS-BEART-USA = SPACE                                         
134900        PERFORM S55-FLYTTA-VIPS-INFO                                      
135000        MOVE WS-VIPS-IDARTNR       TO W-BENA-IDARTNR                      
135100        PERFORM S51-HAMTA-BENAMNING                                       
135200        MOVE WS-BEART-USA          TO RADB-BEART-USA                      
135300        PERFORM S52-BERAKNA-REKSIFFR                                      
135400        MOVE KSIF-KSIFF            TO RADB-REKSIFFR                       
135500        PERFORM IMS-ISRT-RADB                                             
135600     ELSE                                                                 
135700        PERFORM S55-FLYTTA-VIPS-INFO                                      
135800        PERFORM S53-FIXA-BEART-USA-IDARTNR                                
135900        MOVE WS-IDARTNR-TILLK-NUM  TO RADB-IDARTNR-TILLK                  
136000        PERFORM IMS-ISRT-RADB                                             
136100        PERFORM S36-TILLK-MARK-INK-ORDER                                  
136200     END-IF                                                               
136300     .                                                                    
136400     EJECT                                                                
136500                                                                          
136600                                                                          
136700 S33-VIPS-58 SECTION.                                                     
136800                                                                          
136900     MOVE 'STA-S33   '             TO FELTEXT                             
137000                                                                          
137100     INITIALIZE RADB-WDQ501                                               
137200     PERFORM S55-FLYTTA-VIPS-INFO                                         
137300     PERFORM S52-BERAKNA-REKSIFFR                                         
137400     MOVE KSIF-KSIFF               TO RADB-REKSIFFR                       
137500     PERFORM IMS-ISRT-RADB                                                
137600     PERFORM UNTIL SEGMENT-FINNS                                          
137700        ADD 1                   TO W-IDLOPNR                              
137800                                   WS-IDLOPNR                             
137900                                   RADB-IDLOPNR                           
138000        PERFORM IMS-ISRT-RADB                                             
138100     END-PERFORM                                                          
138200     .                                                                    
138300     EJECT                                                                
138400                                                                          
138500                                                                          
138600 S34-VIPS-61-TEXT SECTION.                                                
138700                                                                          
138800     MOVE 'STA-S34   '    TO FELTEXT                                      
138900                                                                          
139000* PÅ DET ANDRA SEGMENTET PÅ EN 61'A FRÅN VIPS                             
139100* INNEHÅLLER BEART-USA EN KOMMENTAR T EX CLASSIC                          
139200     INITIALIZE RADB-WDQ501                                               
139300     IF WS-VIPS-BEART-USA = SPACE                                         
139400        PERFORM S55-FLYTTA-VIPS-INFO                                      
139500        MOVE WS-VIPS-IDARTNR          TO W-BENA-IDARTNR                   
139600        PERFORM S51-HAMTA-BENAMNING                                       
139700        MOVE WS-BEART-USA             TO RADB-BEART-USA                   
139800        PERFORM S52-BERAKNA-REKSIFFR                                      
139900        MOVE KSIF-KSIFF               TO RADB-REKSIFFR                    
140000        PERFORM IMS-ISRT-RADB                                             
140100     ELSE                                                                 
140200                                                                          
140300        PERFORM S55-FLYTTA-VIPS-INFO                                      
140400        MOVE WS-VIPS-BEART-USA        TO RADB-BEART-USA                   
140500        MOVE ZERO                     TO RADB-KVBEART                     
140600        PERFORM IMS-ISRT-RADB                                             
140700     END-IF                                                               
140800     .                                                                    
140900     EJECT                                                                
141000                                                                          
141100                                                                          
141200 S36-TILLK-MARK-INK-ORDER SECTION.                                        
141300                                                                          
141400     MOVE 'STA-S36   '    TO FELTEXT                                      
141500                                                                          
141600     MOVE LOW-VALUE                TO W-WDQ501KY-TILLK-MIN-X              
141700     MOVE HIGH-VALUE               TO W-WDQ501KY-TILLK-MAX-X              
141800     MOVE WS-VIPS-IDORDER          TO W-IDORDER-TILLK-MIN                 
141900                                      W-IDORDER-TILLK-MAX                 
142000     MOVE WS-IDARTNR-TILLK-NUM     TO W-IDARTNR-TILLK-MIN                 
142100                                      W-IDARTNR-TILLK-MAX                 
142200     MOVE NEJ                      TO TRAFF-SW                            
142300     PERFORM IMS-GHU-RADB-TILLK-MIN-MAX                                   
142400     PERFORM UNTIL (SEGMENT-SAKNAS)                                       
142500                OR (BASEN-SLUT)                                           
142600                OR (TRAFF)                                                
142700        IF WS-VIPS-KVBEART     = TILLK-RADB-KVBEART                       
142800           IF TILLK-RADB-FLTILLK = NEJ                                    
142900              MOVE JA           TO TRAFF-SW                               
143000              MOVE SPACE        TO WS-NYTT-DC                             
143100              MOVE VIPS         TO TILLK-RADB-FLTILLK                     
143200              PERFORM IMS-REPL-RADB-TILLK                                 
143300           END-IF                                                         
143400        END-IF                                                            
143500        PERFORM IMS-GHN-RADB-TILLK-MIN-MAX                                
143600     END-PERFORM                                                          
143700     .                                                                    
143800     EJECT                                                                
143900                                                                          
144000                                                                          
144100 S51-HAMTA-BENAMNING SECTION.                                             
144200                                                                          
144300     MOVE 'STA-S51   '    TO FELTEXT                                      
144400                                                                          
144500     PERFORM IMS-GET-BENA11-BSEQ                                          
144600     IF SEGMENT-FINNS                                                     
144700        MOVE TEXT-BEART TO WS-BEART-USA                                   
144800     ELSE                                                                 
144900        MOVE 'UNKNOWN              ' TO WS-BEART-USA                      
145000     END-IF                                                               
145100     .                                                                    
145200     EJECT                                                                
145300                                                                          
145400                                                                          
145500 S52-BERAKNA-REKSIFFR SECTION.                                            
145600                                                                          
145700     MOVE 'STA-S52   '    TO FELTEXT                                      
145800                                                                          
145900     MOVE WS-VIPS-IDARTNR          TO KSIF-FLT                            
146000     MOVE +9                       TO KSIF-LGD                            
146100     CALL W009KSIF USING KSIF-FLT KSIF-LGD KSIF-KSIFF                     
146200     .                                                                    
146300     EJECT                                                                
146400                                                                          
146500                                                                          
146600 S53-FIXA-BEART-USA-IDARTNR SECTION.                                      
146700                                                                          
146800     MOVE 'STA-S53   '    TO FELTEXT                                      
146900                                                                          
147000     IF WS-VIPS-KDORDBEK = 41                                             
147100        MOVE WS-VIPS-BEART-USA     TO WS-IDARTNR-TILLK-ALFA-GRP           
147200        MOVE WS-IDARTNR-TILLK-ALFA TO WS-IDARTNR-TILLK-NUM                
147300     END-IF                                                               
147400     .                                                                    
147500     EJECT                                                                
147600                                                                          
147700                                                                          
147800 S55-FLYTTA-VIPS-INFO SECTION.                                            
147900                                                                          
148000     MOVE 'STA-S55   '    TO FELTEXT                                      
148100                                                                          
148200     INITIALIZE RADB-WDQ501                                               
148300     MOVE NEJ                      TO RADB-FLTILLK                        
148400                                      RADB-FLDIRLEV                       
148500                                                                          
148600     MOVE WS-VIPS-IDORDER          TO W-IDORDER                           
148700     MOVE WS-VIPS-IDARTNR          TO W-IDARTNR                           
148800     MOVE WS-VIPS-IDLOPNR          TO W-IDLOPNR                           
148900     MOVE WS-VIPS-IDSEKVNR         TO W-IDSEKVNR                          
149000     MOVE WS-VIPS-IDDC             TO W-IDDC                              
149100     MOVE WS-VIPS-KDORDBEK         TO W-KDORDBEK                          
149200                                                                          
149300     MOVE WS-VIPS-IDORDER          TO RADB-IDORDER                        
149400     MOVE WS-VIPS-IDARTNR          TO RADB-IDARTNR                        
149500     MOVE WS-VIPS-IDLOPNR          TO RADB-IDLOPNR                        
149600                                      WS-IDLOPNR                          
149700     MOVE WS-VIPS-IDSEKVNR         TO RADB-IDSEKVNR                       
149800                                      WS-IDSEKVNR                         
149900     MOVE WS-VIPS-IDDC             TO RADB-IDDC                           
150000     MOVE WS-VIPS-IDGMTREF         TO RADB-IDGMTREF                       
150100     MOVE WS-VIPS-KDORDBEK         TO RADB-KDORDBEK                       
150200     MOVE WS-VIPS-KDORDKL          TO RADB-KDORDKL                        
150300     MOVE WS-VIPS-KVBEART          TO RADB-KVBEART                        
150400     MOVE WS-VIPS-KVBEART-Q        TO RADB-KVBEART-Q                      
150500     .                                                                    
150600     EJECT                                                                
150700                                                                          
150800                                                                          
150900                                                                          
151000                                                                          
151100* IMS SEKTIONER                                                           
151200                                                                          
151300 IMS-GU-RADB-HUVUD SECTION.                                               
151400                                                                          
151500     STRING 'WLORQP01(WDQ501KY =' W-WDQ501KY-HUV-X ')'                    
151600          DELIMITED BY SIZE INTO SSA1                                     
151700     MOVE '  GE' TO GODK-STATUSKODER                                      
151800     CALL CBLTDLI USING GU ORQP2-PCB DLI-IO-ORQP01-HUV SSA1               
151900     MOVE ORQP2-STATUS-CODE TO STATUS-WS                                  
152000     PERFORM IMS-STATUSKONTROLL                                           
152100     .                                                                    
152200     EJECT                                                                
152300                                                                          
152400                                                                          
152500 IMS-ISRT-RADB-HUVUD SECTION.                                             
152600     SKIP2                                                                
152700     MOVE 'WLORQP01 '                    TO SSA1                          
152800     MOVE '  II'                         TO GODK-STATUSKODER              
152900     CALL CBLTDLI USING ISRT ORQP2-PCB DLI-IO-ORQP01-HUV SSA1             
153000     MOVE ORQP2-STATUS-CODE               TO STATUS-WS                    
153100     PERFORM IMS-STATUSKONTROLL                                           
153200     .                                                                    
153300     EJECT                                                                
153400                                                                          
153500                                                                          
153600 IMS-GU-RADB-MIN-MAX SECTION.                                             
153700                                                                          
153800     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-MIN-X                        
153900                    '&WDQ501KY<=' W-WDQ501KY-MAX-X ')'                    
154000          DELIMITED BY SIZE INTO SSA1                                     
154100     MOVE '  GE' TO GODK-STATUSKODER                                      
154200     CALL CBLTDLI USING GU ORQP-PCB DLI-IO-ORQP01 SSA1                    
154300     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
154400     PERFORM IMS-STATUSKONTROLL                                           
154500     .                                                                    
154600     EJECT                                                                
154700                                                                          
154800                                                                          
154900 IMS-GN-RADB-MIN-MAX SECTION.                                             
155000                                                                          
155100     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-MIN-X                        
155200                    '&WDQ501KY<=' W-WDQ501KY-MAX-X ')'                    
155300          DELIMITED BY SIZE INTO SSA1                                     
155400     MOVE '  GE' TO GODK-STATUSKODER                                      
155500     CALL CBLTDLI USING GN ORQP-PCB DLI-IO-ORQP01 SSA1                    
155600     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
155700     PERFORM IMS-STATUSKONTROLL                                           
155800     .                                                                    
155900     EJECT                                                                
156000                                                                          
156100                                                                          
156200 IMS-GHU-RADB-MIN-MAX SECTION.                                            
156300     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-MIN-X                        
156400                    '&WDQ501KY<=' W-WDQ501KY-MAX-X ')'                    
156500          DELIMITED BY SIZE INTO SSA1                                     
156600     MOVE '  GE' TO GODK-STATUSKODER                                      
156700     CALL CBLTDLI USING GHU ORQP-PCB DLI-IO-ORQP01 SSA1                   
156800     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
156900     PERFORM IMS-STATUSKONTROLL                                           
157000     .                                                                    
157100     EJECT                                                                
157200                                                                          
157300                                                                          
157400 IMS-GHN-RADB-MIN-MAX SECTION.                                            
157500     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-MIN-X                        
157600                    '&WDQ501KY<=' W-WDQ501KY-MAX-X ')'                    
157700          DELIMITED BY SIZE INTO SSA1                                     
157800     MOVE '  GE' TO GODK-STATUSKODER                                      
157900     CALL CBLTDLI USING GHN ORQP-PCB DLI-IO-ORQP01 SSA1                   
158000     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
158100     PERFORM IMS-STATUSKONTROLL                                           
158200     .                                                                    
158300     EJECT                                                                
158400                                                                          
158500                                                                          
158600 IMS-GHU-RADB SECTION.                                                    
158700                                                                          
158800     STRING 'WLORQP01(WDQ501KY =' W-WDQ501KY-X ')'                        
158900          DELIMITED BY SIZE INTO SSA1                                     
159000     MOVE '  GE' TO GODK-STATUSKODER                                      
159100     CALL CBLTDLI USING GHU ORQP-PCB DLI-IO-ORQP01 SSA1                   
159200     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
159300     PERFORM IMS-STATUSKONTROLL                                           
159400     .                                                                    
159500     EJECT                                                                
159600                                                                          
159700                                                                          
159800 IMS-REPL-RADB SECTION.                                                   
159900                                                                          
160000     MOVE '  ' TO GODK-STATUSKODER                                        
160100     CALL CBLTDLI USING REPL ORQP-PCB DLI-IO-ORQP01                       
160200     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
160300     PERFORM IMS-STATUSKONTROLL                                           
160400     .                                                                    
160500     EJECT                                                                
160600                                                                          
160700                                                                          
160800 IMS-DLET-RADB SECTION.                                                   
160900                                                                          
161000     MOVE '  ' TO GODK-STATUSKODER                                        
161100     CALL CBLTDLI USING DLET ORQP-PCB DLI-IO-ORQP01                       
161200     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
161300     PERFORM IMS-STATUSKONTROLL                                           
161400     .                                                                    
161500     EJECT                                                                
161600                                                                          
161700                                                                          
161800 IMS-ISRT-RADB SECTION.                                                   
161900     SKIP2                                                                
162000     MOVE 'WLORQP01 '                    TO SSA1                          
162100     MOVE '  II'                         TO GODK-STATUSKODER              
162200     CALL CBLTDLI USING ISRT ORQP-PCB DLI-IO-ORQP01 SSA1                  
162300     MOVE ORQP-STATUS-CODE               TO STATUS-WS                     
162400     PERFORM IMS-STATUSKONTROLL                                           
162500     .                                                                    
162600     EJECT                                                                
162700                                                                          
162800                                                                          
162900 IMS-GHU-RADB-TILLK-MIN-MAX SECTION.                                      
163000                                                                          
163100     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-TILLK-MIN-X                  
163200                    '&WDQ501KY<=' W-WDQ501KY-TILLK-MAX-X ')'              
163300          DELIMITED BY SIZE INTO SSA1                                     
163400     MOVE '  GE' TO GODK-STATUSKODER                                      
163500     CALL CBLTDLI USING GHU ORQP3-PCB DLI-IO-ORQP01-TILLK SSA1            
163600     MOVE ORQP3-STATUS-CODE TO STATUS-WS                                  
163700     PERFORM IMS-STATUSKONTROLL                                           
163800     .                                                                    
163900     EJECT                                                                
164000                                                                          
164100 IMS-GHN-RADB-TILLK-MIN-MAX SECTION.                                      
164200                                                                          
164300     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-TILLK-MIN-X                  
164400                    '&WDQ501KY<=' W-WDQ501KY-TILLK-MAX-X ')'              
164500          DELIMITED BY SIZE INTO SSA1                                     
164600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
164700     CALL CBLTDLI USING GHN ORQP3-PCB DLI-IO-ORQP01-TILLK SSA1            
164800     MOVE ORQP3-STATUS-CODE TO STATUS-WS                                  
164900     PERFORM IMS-STATUSKONTROLL                                           
165000     .                                                                    
165100     EJECT                                                                
165200                                                                          
165300                                                                          
165400 IMS-REPL-RADB-TILLK SECTION.                                             
165500                                                                          
165600     MOVE '  ' TO GODK-STATUSKODER                                        
165700     CALL CBLTDLI USING REPL ORQP3-PCB DLI-IO-ORQP01-TILLK                
165800     MOVE ORQP3-STATUS-CODE TO STATUS-WS                                  
165900     PERFORM IMS-STATUSKONTROLL                                           
166000     .                                                                    
166100     EJECT                                                                
166200                                                                          
166300                                                                          
166400 IMS-GHU-RADB-NEWCASE-MIN-MAX SECTION.                                    
166500                                                                          
166600     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-NEWCASE-MIN-X                
166700                    '&WDQ501KY<=' W-WDQ501KY-NEWCASE-MAX-X ')'            
166800          DELIMITED BY SIZE INTO SSA1                                     
166900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
167000     CALL CBLTDLI USING GHU ORQP3-PCB DLI-IO-ORQP01-NEWCASE SSA1          
167100     MOVE ORQP3-STATUS-CODE TO ORQP3-STATUS-WS                            
167200     PERFORM IMS-STATUSKONTROLL                                           
167300     .                                                                    
167400     EJECT                                                                
167500                                                                          
167600                                                                          
167700 IMS-GHN-RADB-NEWCASE-MIN-MAX SECTION.                                    
167800                                                                          
167900     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-NEWCASE-MIN-X                
168000                    '&WDQ501KY<=' W-WDQ501KY-NEWCASE-MAX-X ')'            
168100          DELIMITED BY SIZE INTO SSA1                                     
168200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
168300     CALL CBLTDLI USING GHN ORQP3-PCB DLI-IO-ORQP01-NEWCASE SSA1          
168400     MOVE ORQP3-STATUS-CODE TO ORQP3-STATUS-WS                            
168500     PERFORM IMS-STATUSKONTROLL                                           
168600     .                                                                    
168700     EJECT                                                                
168800                                                                          
168900                                                                          
169000 IMS-ISRT-RADB-NEWCASE SECTION.                                           
169100     SKIP2                                                                
169200     MOVE 'WLORQP01 '                    TO SSA1                          
169300     MOVE '  II'                         TO GODK-STATUSKODER              
169400     CALL CBLTDLI USING ISRT ORQP3-PCB DLI-IO-ORQP01-NEWCASE              
169500                        SSA1                                              
169600     MOVE ORQP3-STATUS-CODE              TO ORQP3-STATUS-WS               
169700     PERFORM IMS-STATUSKONTROLL                                           
169800     .                                                                    
169900     EJECT                                                                
170000                                                                          
170100                                                                          
170200 IMS-REPL-RADB-NEWCASE SECTION.                                           
170300                                                                          
170400     MOVE '  ' TO GODK-STATUSKODER                                        
170500     CALL CBLTDLI USING REPL ORQP3-PCB DLI-IO-ORQP01-NEWCASE              
170600     MOVE ORQP3-STATUS-CODE TO STATUS-WS                                  
170700     PERFORM IMS-STATUSKONTROLL                                           
170800     .                                                                    
170900     EJECT                                                                
171000                                                                          
171100                                                                          
171200 IMS-GU-WL401311 SECTION.                                                 
171300                                                                          
171400     STRING 'WL401301(WDGXKEY  =' W-4013-WDGXKEY-X ')'                    
171500          DELIMITED BY SIZE INTO SSA1                                     
171600     STRING 'WL401311(IDGMTREF =' W-4014-IDGMTREF-X ')'                   
171700          DELIMITED BY SIZE INTO SSA2                                     
171800     MOVE '  GE' TO GODK-STATUSKODER                                      
171900     CALL CBLTDLI USING GU 4013-PCB DLI-IO-WL401311                       
172000          SSA1 SSA2                                                       
172100     MOVE 4013-STATUS-CODE TO STATUS-WS                                   
172200     PERFORM IMS-STATUSKONTROLL                                           
172300     .                                                                    
172400     EJECT                                                                
172500                                                                          
172600                                                                          
172700 IMS-GNP-WL401321 SECTION.                                                
172800                                                                          
172900     STRING 'WL401311(IDGMTREF =' W-4014-IDGMTREF-X ')'                   
173000          DELIMITED BY SIZE INTO SSA1                                     
173100     MOVE 'WL401321  ' TO SSA2                                            
173200     MOVE '  GE' TO GODK-STATUSKODER                                      
173300     CALL CBLTDLI USING GNP 4013-PCB DLI-IO-WL401321                      
173400          SSA1 SSA2                                                       
173500     MOVE 4013-STATUS-CODE TO STATUS-WS                                   
173600     PERFORM IMS-STATUSKONTROLL                                           
173700     .                                                                    
173800     EJECT                                                                
173900                                                                          
174000                                                                          
174100 IMS-GU-WL401301 SECTION.                                                 
174200                                                                          
174300     STRING 'WL401301(WDGXKEY  =' W-4013-WDGXKEY-X ')'                    
174400          DELIMITED BY SIZE INTO SSA1                                     
174500     MOVE '  GE' TO GODK-STATUSKODER                                      
174600     CALL CBLTDLI USING GU 4013-PCB DLI-IO-WL401301                       
174700          SSA1                                                            
174800     MOVE 4013-STATUS-CODE TO STATUS-WS                                   
174900     PERFORM IMS-STATUSKONTROLL                                           
175000     .                                                                    
175100     EJECT                                                                
175200                                                                          
175300                                                                          
175400 IMS-GHN-WL401311 SECTION.                                                
175500                                                                          
175600     STRING 'WL401311(IDGMTREF =' W-4014-IDGMTREF-X ')'                   
175700          DELIMITED BY SIZE INTO SSA1                                     
175800     MOVE '  GE' TO GODK-STATUSKODER                                      
175900     CALL CBLTDLI USING GHN 4013-PCB DLI-IO-WL401311                      
176000          SSA1                                                            
176100     MOVE 4013-STATUS-CODE TO STATUS-WS                                   
176200     PERFORM IMS-STATUSKONTROLL                                           
176300     .                                                                    
176400     EJECT                                                                
176500                                                                          
176600                                                                          
176700 IMS-DLET-WL401311 SECTION.                                               
176800                                                                          
176900     MOVE '  ' TO GODK-STATUSKODER                                        
177000     CALL CBLTDLI USING DLET 4013-PCB DLI-IO-WL401311                     
177100     MOVE 4013-STATUS-CODE TO STATUS-WS                                   
177200     PERFORM IMS-STATUSKONTROLL                                           
177300     .                                                                    
177400     EJECT                                                                
177500                                                                          
177600                                                                          
177700 IMS-GET-BENA11-BSEQ  SECTION.                                            
177800     SKIP3                                                                
177900     STRING 'WLBENA01(WDD3BSEQ =' W-BENA-IDARTNR-X ')'                    
178000            DELIMITED BY SIZE INTO SSA1                                   
178100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
178200            DELIMITED BY SIZE INTO SSA2                                   
178300     MOVE '  GE' TO GODK-STATUSKODER                                      
178400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1 SSA2               
178500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
178600     PERFORM IMS-STATUSKONTROLL                                           
178700     .                                                                    
178800     EJECT                                                                
178900                                                                          
179000                                                                          
179100 IMS-STATUSKONTROLL SECTION.                                              
179200     SKIP2                                                                
179300     SET STATUS-IX             TO 1                                       
179400     SEARCH GODK-STATUS AT END CALL FELLOG                                
179500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
179600     END-SEARCH                                                           
179700     .                                                                    
