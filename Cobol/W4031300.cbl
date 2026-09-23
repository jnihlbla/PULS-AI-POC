000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0121      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700 PROGRAM-ID.     W4031300.                                                
000800 AUTHOR.         BERT ANDERSSON.                                          
000900 DATE-WRITTEN.   96/01/08.                                                
001000 DATE-COMPILED.                                                           
001100                                                                          
001200*                                                                         
001300*    FUNCTION.                                                            
001400*        SCREEN 4313.                                                     
001500*        PACK REPORTING OF ORDERPARTS.                                    
001600*                                                                         
001700*        PACKNINGSRAPPORTERING AV ORDERDELAR.                             
001800*        EFTER KONTROLL EN ORDERDEL (=INDATA-RAD PÅ BILDEN) SÅ            
001900*        SKER ANROP AV SUBRUTIN W403AVSP DÄR PACKNING OCH                 
002000*        AVSLUT AV PACKNINGSRAPPORTERINGEN SKER. ETT ANROP PER            
002100*        ORDERDEL GÖRS AV 4313 MOT W403AVSP.                              
002200*        OM EN ORDER MED TRANSFERDISTRIKT RAPPORTERAS VISAS BILD          
002300*        4317.                                                            
002400*                                                                         
002500*        OBS! PROGRAMMET STARTAR OM SIG EFTER VARJE ORDERDEL              
002600*        (= INDATA-RAD).                                                  
002700*                                                                         
002800*        VAL 'U' VID UTSKRIFT AV KOLLIFLAGGA ELLER FÖLJESEDEL             
002900*        SKALL INTE GE NÅGON UTSKRIFT, MEN ÄR ETT GODKÄNT VAL.            
003000*                                                                         
003100*    INDATA.                                                              
003200*        TRANSAKTION: W4T313                                              
003300*        MID:         W4I31301-MID.                                       
003400*                                                                         
003500*    UTDATA.                                                              
003600*        MOD:         W4O31301-MOD.                                       
003700*                     W4O31701-MOD.                                       
003800*                                                                         
003900*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
004000*    SKIP3                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200     SKIP3                                                                
004300 DATA DIVISION.                                                           
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600*    -- CHECKED BY WY2000                                                 
004700     SKIP3                                                                
004800 77    IDPGM                     PIC X(08)    VALUE 'W4031300'.           
004900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005200 77  KDRC-DISPLAY                PIC Z(5).                                
005300                                                                          
005400 77    WS-AKTUELL-SEKTION        PIC X(16)    VALUE SPACE.                
005500 77    LINE-IX                   PIC 9(2)    VALUE ZERO.                  
005600 77    MAX-TAB                   PIC 9(2)    VALUE ZERO.                  
005700 77    MAX-RAD                   PIC 9(2)    VALUE ZERO.                  
005800 77    INDX                      PIC S9(9)   VALUE +0   COMP-3.           
005900 77    RADIX                     PIC S9(9)   VALUE +0   COMP-3.           
006000 77    RADIND                    PIC S9(9)   VALUE +0   COMP-3.           
006100 77    RADIND-9                  PIC  9(9)   VALUE  0.                    
006200 77    ERR-LINE                  PIC  9(2)   VALUE  0.                    
006300 77    RADIND-NUM9               PIC S9(9)   VALUE +0   COMP-3.           
006400 77    JMF-IND                   PIC S9(9)   VALUE +0   COMP-3.           
006500 77    MAX-LINES                 PIC S9(9)   VALUE +50  COMP-3.           
006600 77    MAX-RADINDX               PIC S9(9)   VALUE +13  COMP-3.           
006700 77    MAX-MOD-LENGTH            PIC S9(4)   VALUE +900 COMP-3.           
006800 77    JA                        PIC X       VALUE 'J'.                   
006900 77    NEJ                       PIC X       VALUE 'N'.                   
007000 77    SAKNAS                    PIC X       VALUE 'S'.                   
007100 77    RAETT                     PIC X       VALUE 'R'.                   
007200 77    FEL-FR-W403AVSP           PIC X       VALUE 'F'.                   
007300 77    WS-MID-ADRESSFL           PIC XX.                                  
007400 77    WS-MID-FOLJES             PIC XX.                                  
007500 77    WS-PRT-KDSVAR-ADRESSFL    PIC X.                                   
007600 77    WS-PRT-KDSVAR-FOLJES      PIC X.                                   
007700 77    WS-KDMFSFOR               PIC X.                                   
007800 77    WS-IDDISTR-NUM4           PIC 9(4).                                
007900 77    WS-IDDISTR-JFR            PIC 9(4).                                
008000 77    WS-VORD-IDDISTR           PIC 9(5).                                
008100 77    WS-IDKUNDNR-NUM6          PIC 9(6).                                
008200 77    WS-IDKOLLI-SAMP           PIC 9(5)   VALUE ZERO.                   
008300 77    WS-IDANSTNR               PIC 9(5)   VALUE ZERO.                   
008400 77    WS-MOD-VKORDBTO           PIC S9(6)V9.                             
008500 77    WS-IDORDNR7               PIC 9(7).                                
008600                                                                          
008700 77    WS-TRAEFF-PACKARE         PIC X(01).                               
008800 77    WS-PACKARES-ODEL-REDAN-KLARA PIC X.                                
008900 77    WS-JFR-IDPRODNR           PIC X(7)   VALUE SPACE.                  
009000 77    WS-KDPRTVAL-FS            PIC XX.                                  
009100 77    WS-KDPRTVAL-ADR           PIC XX.                                  
009200                                                                          
009300 77    INF-WEIGHT-NOT-LESS-THAN  PIC X(50)                                
009400       VALUE 'WEIGHT CANNOT BE LESS THAN                   '.             
009500                                                                          
009600 77    ENTER-GROSS-WEIGHT        PIC X(50)                                
009700       VALUE 'ENTER GROSS WEIGHT                           '.             
009800                                                                          
009900 01  WS-IDPRTLST.                                                         
010000     03 WS-SYSTDEL               PIC X(1).                                
010100     03 WS-LISTTYP               PIC X(2).                                
010200     03 WS-DC                    PIC X(2).                                
010300     03 WS-KDPRT                 PIC X(3).                                
010400 01  WS-IDPRTJAP REDEFINES WS-IDPRTLST.                                   
010500     03 WS-LASER-BLANKETT        PIC X(6).                                
010600     03 WS-NDC-JAP-KDPRT         PIC X(2).                                
010700*                                                                         
010800 01     WS-JFR-IDANSTNR.                                                  
010900   03   FILLER                    PIC X(3).                               
011000   03   WS-JFR-IDANSTNR-5         PIC X(5).                               
011100                                                                          
011200*                                                                         
011300 01  TRANSFER-KUND               PIC 9(7).                                
011400     88 TRANSFER-KUNDNR          VALUE 0000511                            
011500                                       0000512                            
011600                                       0000513.                           
011700     88  RETUR-KUNDNR            VALUE 0000051.                           
011800*                                                                         
011900 01  WS-KDMATT                   PIC X.                                   
012000     88 US-MEASUREMENT           VALUE 'U'.                               
012100     88 SIS-MEASUREMENT          VALUE 'S'.                               
012200                                                                          
012300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
012400     88  INDATA-OK                           VALUE 'J'.                   
012500     88  INDATA-FEL                          VALUE 'N'.                   
012600                                                                          
012700 77  KEYS-SW                     PIC X       VALUE 'J'.                   
012800     88  KEYS-OK                             VALUE 'J'.                   
012900     88  KEYS-FEL                            VALUE 'N'.                   
013000                                                                          
013100 77  ORDERDEL-PACKAD-SW          PIC X       VALUE 'N'.                   
013200     88  ORDERDEL-PACKAD                     VALUE 'J'.                   
013300                                                                          
013400 77  INDATA-RAD-BEHANDLAD-SW     PIC X       VALUE 'N'.                   
013500     88  INDATA-RAD-BEHANDLAD                VALUE 'J'.                   
013600                                                                          
013700 77  INDATA-FINNS-SW             PIC X       VALUE 'J'.                   
013800     88  INDATA-FINNS                        VALUE 'J'.                   
013900     88  INDATA-SAKNAS                       VALUE 'N'.                   
014000*                                                                         
014100 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
014200     88  EJ-OMSTART                          VALUE 'N'.                   
014300     88  OMSTART                             VALUE 'J'.                   
014400*                                                                         
014500 77  SW-STARTA-4317              PIC X       VALUE 'N'.                   
014600     88 4317-STARTAD                         VALUE 'J'.                   
014700     88 4317-INTE-STARTAD                    VALUE 'N'.                   
014800                                                                          
014900 77    DIRLEV-KOLLI-SW           PIC X(01).                               
015000   88  DIRLEV-KOLLI                         VALUE 'J'.                    
015100*                                                                         
015200                                                                          
015300     EJECT                                                                
015400 01    FILLER                    PIC X(16)   VALUE 'WS-AREA'.             
015500 01    WS-AREA.                                                           
015600                                                                          
015700   03    MAX-ANT-RAD             PIC S9(9)   VALUE +100.                  
015800   03    WS-ANT-RAD-REST         PIC S9(9)   VALUE ZERO.                  
015900   03    WS-ANT-RAD-INT          PIC S9(9)   VALUE ZERO.                  
016000                                                                          
016100   03    WS-IDTRANS              PIC X(4)    VALUE SPACE.                 
016200         88  OWN-MID                             VALUE '4313'.            
016300         88  GOOD-MID                            VALUE '4313'             
016400                                                       '4317'.            
016500         88  HELP-MID                            VALUE '0551'.            
016600                                                                          
016700   03    WS-VKORDBTO-RED         PIC 9(5).9  VALUE ZERO.                  
016800   03    FILLER           REDEFINES WS-VKORDBTO-RED.                      
016900         05  WS-VKORDBTO-KG      PIC 9(5).                                
017000         05  WS-VKORDBTO-PUNKT   PIC X.                                   
017100         05  WS-VKORDBTO-HK      PIC 9.                                   
017200                                                                          
017300   03    WS-VKORDBTO-TOT         PIC 9(6).999 VALUE ZERO.                 
017400                                                                          
017500 01  WS-CURRENT-DATE             PIC 9(8)    VALUE ZERO.                  
017600 01  WS-CURRENT-TIME             PIC 9(8)    VALUE ZERO.                  
017700                                                                          
017800 01  FILLER                    PIC X(16)   VALUE 'WS-ARB-TAB'.            
017900 01  WS-ARB-TAB.                                                          
018000   03  WS-TABSTEG OCCURS 13.                                              
018100     05  WS-IDDISTR              PIC 9(5).                                
018200     05  WS-IDKUNDNR             PIC 9(7).                                
018300     05  WS-IDPRODNR             PIC 9(7).                                
018400     05  WS-IDORDNR              PIC 9(5).                                
018500     05  WS-IDORDER              PIC 9(7).                                
018600     05  WS-IDPLKLST             PIC 9(3).                                
018700     05  WS-IDKOLLI              PIC 9(5).                                
018800     05  WS-KDKOLLI              PIC X(8).                                
018900     05  WS-KVORDRAD             PIC S9(5)        COMP-3.                 
019000     05  WS-KDFRAKT              PIC S9(3)        COMP-3.                 
019100     05  WS-IDKUNDRF             PIC X(10).                               
019200     05  WS-KDEMBTYP             PIC 9.                                   
019300     05  WS-DIKOLLIL             PIC 9(5).                                
019400     05  WS-DIKOLLIB             PIC 9(3).                                
019500     05  WS-DIKOLLIH             PIC 9(3).                                
019600     05  WS-KDKOLLID             PIC X(1).                                
019700     05  WS-VKTARA               PIC S9(6)V9.                             
019800     05  WS-VKORDBTO             PIC S9(6)V999.                           
019900     05  WS-KORD-VKORDNTO        PIC S9(6)V9.                             
020000     05  WS-ORAD-VKARTNTO-KG     PIC S9(6)V9(3).                          
020100     05  WS-FLAVSP               PIC X.                                   
020200     05  WS-IDSYSTEM             PIC X(4).                                
020300     05  WS-IDUSER               PIC 9(8).                                
020400     05  FILLER           REDEFINES WS-IDUSER.                            
020500         07 FILLER               PIC 9(3).                                
020600         07 WS-IDANSTNR-TAB      PIC 9(5).                                
020700     EJECT                                                                
020800 01  DYNAMISKA-SUBPROGRAM.                                                
020900     03 WMEDKONV                 PIC X(8)    VALUE 'WMEDKONV'.            
021000     03 CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.            
021100     03 FELLOG                   PIC X(8)    VALUE 'FELLOG  '.            
021200     03 WDECEDIT                 PIC X(8)    VALUE 'WDECEDIT'.            
021300     03 W005INIT                 PIC X(8)    VALUE 'W005INIT'.            
021400     03 W403AVSP                 PIC X(8)    VALUE 'W403AVSP'.            
021500     03 W006PRT                  PIC X(8)    VALUE 'W006PRT'.             
021600     03 WWOMVAND                 PIC X(8)    VALUE 'WWOMVAND'.            
021700     03 W411DNOT                 PIC X(8)    VALUE 'W411DNOT'.            
021800     03 WZ01SEND                 PIC X(8)    VALUE 'WZ01SEND'.            
021900     03 ABEND                    PIC X(8)    VALUE 'ABEND   '.            
022000     SKIP2                                                                
022100*    --- PARAMETRAR TILL ABEND                                            
022200                                                                          
022300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
022400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
022500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
022600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
022700     SKIP3                                                                
022800*    --- PARAMETERS TO WZ01SEND                                           
022900 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
023000     SKIP3                                                                
023100*01  -COPY WZ01SEND                                                       
023200     EJECT                                                                
023300 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
023400 01  HDR-AREA.                                                            
023500*    03  -COPY WZ01REQU  -PRE HDR-                                        
023600*    03  -COPY WZ04HDR                                                    
023700*                                                                         
023800 01  FILLER                      PIC X(9)    VALUE 'SEND-AREA'.           
023900 01  SEND-AREA                   PIC X(100)  VALUE SPACE.                 
024000*                                                                         
024100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
024200 01  FILLER                      PIC X(16)   VALUE 'WMEDAREA '.           
024300*   -COPY WMEDAREA                                                        
024400     SKIP3                                                                
024500*                                                                         
024600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
024700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT '.           
024800*01 -COPY WMSGINIT                                                        
024900     SKIP2                                                                
025000*                                                                         
025100 01  FILLER                      PIC X(16)   VALUE 'W006PRT  '.           
025200*   -COPY W006PRT                                                         
025300     EJECT                                                                
025400*                                                                         
025500*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
025600*                                                                         
025700 01    FILLER                    PIC X(16)   VALUE 'W403AVSP '.           
025800*      -COPY W403AVSP                                                     
025900     SKIP2                                                                
026000 01    FILLER                    PIC X(10)   VALUE 'WDECAREA'.            
026100*01    -COPY WDECAREA                                                     
026200     SKIP2                                                                
026300 01    FILLER                    PIC X(16)   VALUE 'WWOMVAND '.           
026400*01    -COPY WWOMVAND                                                     
026500     SKIP2                                                                
026600 01    FILLER                    PIC X(16)   VALUE 'WWDIST03'.            
026700*01    -COPY WWDIST03                                                     
026800     EJECT                                                                
026900 01    FILLER                    PIC X(16)   VALUE 'WWDIST07'.            
027000*01    -COPY WWDIST07                                                     
027100     SKIP2                                                                
027200 01    FILLER                    PIC X(16)   VALUE 'WWDIST08'.            
027300*01    -COPY WWDIST08                                                     
027400     SKIP2                                                                
027500 01    FILLER                    PIC X(16)   VALUE 'WWDIST19'.            
027600*01    -COPY WWDIST19                                                     
027700     SKIP2                                                                
027800 01 FILLER                       PIC X(8)    VALUE 'W411DNOT'.            
027900*01 -COPY W411DNOT                                                        
028000     EJECT                                                                
028100*      --- VALID IDDD CODES                                               
028200*                                                                         
028300*01    -COPY WWDC99                                                       
028400       EJECT                                                              
028500                                                                          
028600 01  MESSAGE-CODES.                                                       
028700     03  INF-ITEMS-MISSING       PIC X(3)    VALUE '029'.                 
028800     03  INF-ORDER-PARTS-MISSING PIC X(3)    VALUE '036'.                 
028900     03  INF-ORDER-MISSING       PIC X(3)    VALUE '054'.                 
029000     03  INF-WRONG-STATUS        PIC X(3)    VALUE '079'.                 
029100     03  INF-NO-DANGEROUS-CARGO  PIC X(3)    VALUE '255'.                 
029200     03  INF-DISTRICT-NOT-APPROV PIC X(3)    VALUE '256'.                 
029300     03  INF-MORE-THAN-200-LINES PIC X(3)    VALUE '257'.                 
029400     03  INF-ORDER-NOT-PACKED    PIC X(3)    VALUE '315'.                 
029500     03  INF-USE-SCREEN-4315     PIC X(3)    VALUE '323'.                 
029600     03  INF-SUPPLIER-OPART      PIC X(3)    VALUE '325'.                 
029700     03  INF-UPDATED             PIC X(3)    VALUE '404'.                 
029800     03  INF-WEIGHT-NOT-LESS     PIC X(3)    VALUE '441'.                 
029900     03  INF-ORDER-REGISTRED     PIC X(3)    VALUE '710'.                 
030000     03  INF-ORDER-PART-READY    PIC X(3)    VALUE '713'.                 
030100     03  INF-CASE-REPORT-STARTED PIC X(3)    VALUE '714'.                 
030200     03  INF-WRONG-PICKER        PIC X(3)    VALUE '719'.                 
030300     03  INF-PICKERS-ORDERPART-FINISHED                                   
030400                                 PIC X(3)    VALUE '720'.                 
030500     03  INF-CASE-ALREADY-REPORT PIC X(3)    VALUE '721'.                 
030600     03  INF-CASE-CODE-MISSING   PIC X(3)    VALUE '726'.                 
030700     03  INF-ERROR-IN-ADDRESS    PIC X(3)    VALUE '730'.                 
030800     03  INF-WRONG-DISTRICT      PIC X(3)    VALUE '747'.                 
030900     03  INF-WRONG-LINES         PIC X(3)    VALUE '751'.                 
031000     03  INF-LINE-ALREADY-REPORT PIC X(3)    VALUE '766'.                 
031100     03  INF-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
031200     03  INF-LINE-ALREADY-ZEROED PIC X(3)    VALUE '774'.                 
031300     03  INF-DEVIATION-CONTR-IN-PROGR   PIC X(3) VALUE '804'.             
031400     03  INF-NOT-CASECODE-AND-CASEDIM   PIC X(3) VALUE '337'.             
031500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
031600     03  ERR-HIGHLIT-FIELDS-WRON PIC X(3)    VALUE '748'.                 
031700     03  ERR-MIXED-CASE-ON-OTHER-TRANS  PIC X(3) VALUE '317'.             
031800     03  ERR-MIXED-CASE-HAS-NO-TRANS    PIC X(3) VALUE '318'.             
031900     03  INF-MUST-BE-REPORTED-ON-WEB    PIC X(3) VALUE '359'.             
032000     EJECT                                                                
032100 01    FILLER                    PIC X(16)                                
032200                                 VALUE 'KEYS-TILL-DLI'.                   
032300 01    KEYS-TILL-DLI.                                                     
032400                                                                          
032500   03  W-IDARTNR-X.                                                       
032600      05     W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
032700   03  W-IDDC-X.                                                          
032800      05     W-IDDC              PIC X(2)    VALUE SPACE.                 
032900   03  W-KDSEGKEY-X.                                                      
033000      05  W-KDSEGKEY             PIC X(1)    VALUE '1'.                   
033100                                                                          
033200   03    W-KDKOLLI-WDK5          PIC X(8)    VALUE SPACE.                 
033300                                                                          
033400   03    W-IDPRODNR-X.                                                    
033500     05    W-IDPRODNR            PIC S9(7)   VALUE ZERO  COMP-3.          
033600                                                                          
033700   03    W-IDKOLLI-X.                                                     
033800     05    W-IDKOLLI             PIC S9(5)   VALUE ZERO  COMP-3.          
033900                                                                          
034000   03    W-WDQ3D-X.                                                       
034100     05    W-IDPRODNR-WDQ3D      PIC S9(7)   VALUE ZERO  COMP-3.          
034200     05    W-IDPLKLST-WDQ3D      PIC S9(3)   VALUE ZERO  COMP-3.          
034300*                                                                         
034400   03    W-WDE4A1-KUNDORDER-X.                                            
034500     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
034600     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
034700     05    W-4A1-IDKUNDRF.                                                
034800       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
034900       07  FILLER                PIC X(05)   VALUE SPACE.                 
035000*                                                                         
035100   03    W-WDE401-KUNDORDER-X.                                            
035200     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
035300     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
035400     05    W-401-IDKUNDRF.                                                
035500       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
035600       07  FILLER                PIC X(05)   VALUE SPACE.                 
035700     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
035800     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
035900*                                                                         
036000   03    W-WDE411-IDPURAD-X.                                              
036100     05    W-420-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
036200*                                                                         
036300   03    W-WDE421-KKOLLI-X.                                               
036400     05    W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
036500     05    W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
036600*                                                                         
036700                                                                          
036800   03    W-IDDC-B6-X.                                                     
036900     05    W-IDDC-B6             PIC X(2).                                
037000                                                                          
037100     EJECT                                                                
037200******************************************************************        
037300 01  FILLER                      PIC X(16) VALUE 'USER-SPAR-AREA'.        
037400 01  SPAR-AREA.                                                           
037500   03  SPAR-IDTRANS              PIC X(4)    VALUE '4313'.                
037600   03  SPAR-W4I31301.                                                     
037700     05  -COPY W4I31301   -PRE USER-                                      
037800******************************************************************        
037900   01  FILLER                  PIC X(16) VALUE 'MID W4I33301AREA'.        
038000   01  4333-MID-IO-AREA.                                                  
038100                                                                          
038200       03  4333-MID-KVLL         PIC S9(4)   COMP SYNC.                   
038300       03  FILLER                PIC X(2)    VALUE LOW-VALUE.             
038400       03  FILLER                PIC X(8)    VALUE 'W4T333  '.            
038500       03  4333-IDTRANS          PIC X(4)    VALUE '431C'.                
038600       03  4333-MID-KDMFSFOR     PIC X.                                   
038700*      03  MID -COPY W4I33301  -PRE 4333-.                                
038800     SKIP2                                                                
038900   03  FILLER                 PIC X(16) VALUE 'MID W4I34101AREA'.         
039000   01  4341-MID-IO-AREA.                                                  
039100                                                                          
039200       03  4341-MID-KVLL         PIC S9(4)   COMP SYNC.                   
039300       03  FILLER                PIC X(2)    VALUE LOW-VALUE.             
039400       03  FILLER                PIC X(8)    VALUE 'W4T341U '.            
039500       03  FILLER                PIC X(4)    VALUE '431E'.                
039600       03  4341-MID-KDMFSFOR     PIC X.                                   
039700*      03  MID -COPY W4I34101  -PRE 4341-.                                
039800                                                                          
039900 01  FILLER               PIC X(16) VALUE '4313 OMSTART MID'.             
040000 01  4313-MID-IO-AREA.                                                    
040100     03  4313UT-KVLL             PIC S9(4)   VALUE 630 COMP SYNC.         
040200     03  FILLER                  PIC  X(2)   VALUE LOW-VALUE.             
040300     03  FILLER                  PIC  X(8)   VALUE 'W4T313  '.            
040400     03  FILLER                  PIC  X(4)   VALUE '4313'.                
040500     03  4313UT-KDMFSFOR         PIC  X(1).                               
040600*    03  -COPY W4I31301  -PRE 4313UT-                                     
040700     SKIP2                                                                
040800******************************************************************        
040900*                                                                         
041000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
041100*                                                                         
041200 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
041300                                                                          
041400 01    FILLER                 PIC X(16) VALUE 'MID W4I31301 MID'.         
041500*01    -COPY W4I31301                                                     
041600                                                                          
041700 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
041800*01    -COPY WMSGAREA                                                     
041900                                                                          
042000*  03  MOD -COPY W4O31301   -RED MSG-AREA                                 
042100*  03  MOD -COPY W4O31701   -RED MSG-AREA    -PRE 4317-                   
042200   03  FILLER                 PIC X(16) VALUE 'MOD W4O31701 MOD'.         
042300     SKIP2                                                                
042400                                                                          
042500 01  FILLER                      PIC X(08)   VALUE 'MFS-AREA'.            
042600*01    -COPY WMFSAREA                                                     
042700     EJECT                                                                
042800 01  HEADER.                                                              
042900     03  FILLER.                                                          
043000        05  FILLER               PIC X(50)  VALUE                         
043100            'Gross weight input from screen does not match with'.         
043200        05  FILLER               PIC X(50)  VALUE                         
043300            ' weight in PULS system for below ORDER details.'.            
043400     03  FILLER.                                                          
043500        05  FILLER               PIC X(50)  VALUE                         
043600            'Please check the parts and emballage weight includ'.         
043700        05  FILLER               PIC X(50)  VALUE                         
043800            'ed in this case stated below.All weight are in KG.'.         
043900     03  FILLER.                                                          
044000        05  FILLER               PIC X(100) VALUE SPACE .                 
044100     03  FILLER.                                                          
044200        05  FILLER               PIC X(5)   VALUE SPACE.                  
044300        05  FILLER               PIC X(8)   VALUE 'District'.             
044400        05  FILLER               PIC X(4)   VALUE X'05050505'.            
044500        05  HEAD-DIST            PIC Z(5)   VALUE ZERO.                   
044600        05  FILLER               PIC X(78)  VALUE SPACE.                  
044700     03  FILLER.                                                          
044800        05  FILLER               PIC X(5)   VALUE SPACE.                  
044900        05  FILLER               PIC X(8)   VALUE 'Customer'.             
045000        05  FILLER               PIC X(4)   VALUE X'05050505'.            
045100        05  HEAD-IDKUNDNR        PIC Z(6)9  VALUE ZERO.                   
045200        05  FILLER               PIC X(76)  VALUE SPACE.                  
045300     03  FILLER.                                                          
045400        05  FILLER               PIC X(5)   VALUE SPACE.                  
045500        05  FILLER               PIC X(12)  VALUE 'Order number'.         
045600        05  FILLER               PIC X(3)   VALUE X'050505'.              
045700        05  HEAD-IDORDER         PIC Z(5)   VALUE ZERO.                   
045800        05  FILLER               PIC X(75)  VALUE SPACE.                  
045900     03  FILLER.                                                          
046000        05  FILLER               PIC X(5)   VALUE SPACE.                  
046100        05  FILLER               PIC X(11)  VALUE 'Case number'.          
046200        05  FILLER               PIC X(4)   VALUE X'05050505'.            
046300        05  HEAD-IDKOLLI         PIC Z(5)   VALUE ZERO.                   
046400        05  FILLER               PIC X(75)  VALUE SPACE.                  
046500     03  FILLER.                                                          
046600        05  FILLER               PIC X(5)   VALUE SPACE.                  
046700        05  FILLER               PIC X(9)   VALUE 'Case code'.            
046800        05  FILLER               PIC X(4)   VALUE X'05050505'.            
046900        05  HEAD-KDKOLLI         PIC X(82)  VALUE SPACE.                  
047000     03  FILLER.                                                          
047100        05  FILLER               PIC X(5)   VALUE SPACE.                  
047200        05  FILLER               PIC X(10)  VALUE 'Picking ID'.           
047300        05  FILLER               PIC X(4)   VALUE X'05050505'.            
047400        05  HEAD-IDPLKLST        PIC Z(8)   VALUE ZERO.                   
047500        05  FILLER               PIC X(73)  VALUE SPACE.                  
047600     03  FILLER.                                                          
047700        05  FILLER               PIC X(5)   VALUE SPACE.                  
047800        05  FILLER               PIC X(12)  VALUE                         
047900                                            'Packing time'.               
048000        05  FILLER               PIC X(4)   VALUE X'05050505'.            
048100        05  HEAD-PACKTIME.                                                
048200           07 HEAD-DATE          PIC X(8)   VALUE SPACE.                  
048300           07 FILLER             PIC X(2)   VALUE SPACE.                  
048400           07 HEAD-TIME          PIC X(69)  VALUE SPACE.                  
048500     03  FILLER.                                                          
048600        05  FILLER               PIC X(5)   VALUE SPACE.                  
048700        05  FILLER               PIC X(17)  VALUE                         
048800                                            'Input case weight'.          
048900        05  FILLER               PIC X(3)   VALUE X'050505'.              
049000        05  HEAD-VKORDBTO        PIC Z(6).999  VALUE ZERO.                
049100        05  FILLER               PIC X(67)  VALUE SPACE.                  
049200     03  FILLER.                                                          
049300        05  FILLER               PIC X(5)   VALUE SPACE.                  
049400        05  FILLER               PIC X(25)  VALUE                         
049500                                'Calculated weight for the'.              
049600        05  FILLER               PIC X(2)   VALUE X'0505'.                
049700        05  HEAD-VKARTNTO        PIC Z(6).999 VALUE ZERO.                 
049800        05  FILLER               PIC X(60)  VALUE SPACE.                  
049900     03  FILLER.                                                          
050000        05  FILLER               PIC X(5)   VALUE SPACE.                  
050100        05  FILLER               PIC X(95)  VALUE                         
050200                                     'parts in the case'.                 
050300     03  FILLER.                                                          
050400        05  FILLER               PIC X(100) VALUE SPACE.                  
050500     03  FILLER.                                                          
050600        05  FILLER               PIC X(100) VALUE 'Order line'.           
050700     03  FILLER.                                                          
050800        05  FILLER               PIC X(2)   VALUE 'No'.                   
050900        05  FILLER               PIC X(2)   VALUE X'0505'.                
051000        05  FILLER               PIC X(7)   VALUE 'Part no'.              
051100        05  FILLER               PIC X(2)   VALUE X'0505'.                
051200        05  FILLER               PIC X(6)   VALUE SPACE.                  
051300        05  FILLER               PIC X(3)   VALUE 'Qty'.                  
051400        05  FILLER               PIC X(2)   VALUE X'0505'.                
051500        05  FILLER               PIC X(22)  VALUE                         
051600                                  'Part Weight + Part emb'.               
051700        05  FILLER               PIC X(2)   VALUE X'0505'.                
051800        05  FILLER               PIC X(11)  VALUE                         
051900                                  'Part Weight'.                          
052000        05  FILLER               PIC X(3)   VALUE X'050505'.              
052100        05  FILLER               PIC X(41)  VALUE 'Location'.             
052200 01  HEADER-TAB  REDEFINES HEADER.                                        
052300     03 TAB-LINE   OCCURS 16 TIMES.                                       
052400        05 FILLER  PIC X(100).                                            
052500                                                                          
052600 01  LINEDATA.                                                            
052700     03 LINE-TAB OCCURS 50 TIMES.                                         
052800        05  LINE1-NUMBER         PIC X(2)    VALUE SPACE.                 
052900        05  FILLER               PIC X(2)    VALUE X'0505'.               
053000        05  LINE1-IDARTNR        PIC Z(9)    VALUE ZERO.                  
053100        05  FILLER               PIC X(2)    VALUE X'0505'.               
053200        05  LINE1-KVAVBART       PIC Z(9)    VALUE ZERO.                  
053300        05  FILLER               PIC X(2)    VALUE X'0505'.               
053400        05  LINE1-VKOLDNET       PIC Z(9)9.999 VALUE ZERO.                
053500        05  FILLER               PIC X(3)    VALUE X'050505'.             
053600        05  LINE1-VKNEWNET       PIC Z(9)9.999 VALUE ZERO.                
053700        05  FILLER               PIC X(3)    VALUE X'050505'.             
053800        05  LINE1-ADLAGOMR       PIC Z(3)    VALUE ZERO.                  
053900        05  FILLER               PIC X(1)    VALUE '.'.                   
054000        05  LINE1-ADGANG         PIC 9(2)    VALUE ZERO.                  
054100        05  FILLER               PIC X(1)    VALUE '.'.                   
054200        05  LINE1-ADPLATS        PIC X(5)    VALUE SPACE.                 
054300        05  FILLER               PIC X(1)    VALUE '.'.                   
054400                                                                          
054500 01  LINE-END                    PIC X(100) VALUE                         
054600        'More parts exist.Please check the case.'.                        
054700                                                                          
054800******************************************************************        
054900*                                                                         
055000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
055100*                                                                         
055200 01    IMS-WS.                                                            
055300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
055400     SKIP3                                                                
055500*                        **** STATUS-KOD FRÅN IMS                         
055600   03    STATUS-WS               PIC XX.                                  
055700     88    SEGMENT-FINNS                     VALUE '  '.                  
055800     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
055900     SKIP3                                                                
056000   03    STATUS-KUNDORDER-SEK-WS PIC X(02).                               
056100     88    KUNDORDER-SEK-FINNS               VALUE '  '.                  
056200     88    KUNDORDER-SEK-SAKNAS              VALUE 'GE' 'GB'.             
056300   03    GODK-STATUSKODER.                                                
056400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
056500     SKIP3                                                                
056600 01    SSA1                      PIC X(64).                               
056700 01    SSA2                      PIC X(64).                               
056800 01    SSA3                      PIC X(64).                               
056900     EJECT                                                                
057000*                            IMS FUNKTIONSKODER                           
057100*01    -COPY W0003                                                        
057200     EJECT                                                                
057300*                            DLI INPUT-OUTPUT AREA                        
057400 01    FILLER                    PIC X(16) VALUE 'WDK501-AREA'.           
057500 01    WLEMBB01 -COPY WDK501                                              
057600     EJECT                                                                
057700 01    FILLER                    PIC X(16) VALUE 'WDE601-AREA'.           
057800 01    WDE601 -COPY WDE601                                                
057900     EJECT                                                                
058000 01    FILLER                    PIC X(16) VALUE 'WDE611-AREA'.           
058100 01    WDE611   -COPY WDE611                                              
058200     EJECT                                                                
058300 01    FILLER                    PIC X(16) VALUE 'WDE401-AREA'.           
058400 01    WDE401 -COPY WDE401                                                
058500     EJECT                                                                
058600 01    FILLER                    PIC X(16) VALUE 'WDE411-AREA'.           
058700 01    WDE411 -COPY WDE411                                                
058800     EJECT                                                                
058900 01    FILLER                    PIC X(16) VALUE 'WDE421-AREA'.           
059000 01    WDE421 -COPY WDE421                                                
059100     EJECT                                                                
059200 01    FILLER                    PIC X(16) VALUE 'WDQ301-AREA'.           
059300 01    WLORQA01 -COPY WDQ301                                              
059400                                                                          
059500 01    FILLER                    PIC X(16) VALUE 'WDB601 AREA'.           
059600 01    DLI-IO-AREA-B601.                                                  
059700*      03  -COPY WDB601                                                   
059800 01    FILLER                    PIC X(16) VALUE 'DLI-IO-WDK611'.         
059900 01    DLI-IO-WDK611.                                                     
060000*      03  -COPY WDK611                                                   
060100     EJECT                                                                
060200 01    FILLER                    PIC X(16) VALUE 'DLI-IO-WDK711'.         
060300 01    DLI-IO-WDK711.                                                     
060400*      03  -COPY WDK711                                                   
060500     EJECT                                                                
060600 LINKAGE SECTION.                                                         
060700*01    -COPY W0009     -PRE MSG-                                          
060800     EJECT                                                                
060900*01    -COPY W0009     -PRE ALT4313-                                      
061000     EJECT                                                                
061100*01    -COPY W0009     -PRE ALT4317-                                      
061200     EJECT                                                                
061300*01    -COPY W0009     -PRE ALT4333-                                      
061400     EJECT                                                                
061500*01    -COPY W0009     -PRE ALT4341-                                      
061600     EJECT                                                                
061700 01  AVSP-4342-PCB               PIC X.                                   
061800 01  AVSP-2191-PCB               PIC X.                                   
061900 01  AVSP-4349-PCB               PIC X.                                   
062000*01    -COPY W0009     -PRE MAIL-                                         
062100     EJECT                                                                
062200*01    -COPY W0009     -PRE DISTRDOC-                                     
062300     EJECT                                                                
062410 01  TMS-CRE-PCB                 PIC X.                                   
062420 01  TMS-DEL-PCB                 PIC X.                                   
062500     EJECT                                                                
062600 01  ATAB-PCB                    PIC X.                                   
062700                                                                          
062800*01    -COPY W0008     -PRE USEA-                                         
062900        05 FILLER                PIC X.                                   
063000     EJECT                                                                
063100*01    -COPY W0008     -PRE EMBB-                                         
063200        05 FILLER                PIC X.                                   
063300     EJECT                                                                
063400*01    -COPY W0008     -PRE WDE6-                                         
063500        05 FILLER                PIC X.                                   
063600     EJECT                                                                
063700*01    -COPY W0008     -PRE ORQA-                                         
063800        05 FILLER                PIC X.                                   
063900     EJECT                                                                
064000*01    -COPY W0008     -PRE WDE4-                                         
064100        05 FILLER                PIC X.                                   
064200     EJECT                                                                
064300*01    -COPY W0008     -PRE WDB6-                                         
064400        05 FILLER                PIC X.                                   
064500     EJECT                                                                
064600*01    -COPY W0008     -PRE WDE4A-                                        
064700        05 FILLER                PIC X.                                   
064800     EJECT                                                                
064900*01    -COPY W0008     -PRE WDK6-                                         
065000        05 FILLER                PIC X.                                   
065100     EJECT                                                                
065200*01    -COPY W0008     -PRE WDK7-                                         
065300        05 FILLER                PIC X.                                   
065400     EJECT                                                                
065500*****AVSP-PCB'N                                                           
065600 01  AVSP-USEA-PCB               PIC X.                                   
065700 01  AVSP-WDE41-PCB              PIC X.                                   
065800 01  AVSP-WDE42-PCB              PIC X.                                   
065900 01  AVSP-WDE4-PCB               PIC X.                                   
066000 01  AVSP-WDE4B-PCB              PIC X.                                   
066100 01  AVSP-WDE4E-PCB              PIC X.                                   
066200 01  AVSP-WDE6-PCB               PIC X.                                   
066300 01  AVSP-XXDV-PCB               PIC X.                                   
066400 01  AVSP-ORQA-PCB               PIC X.                                   
066500 01  AVSP-XXKW-PCB               PIC X.                                   
066600 01  AVSP-XXLB-PCB               PIC X.                                   
066700 01  AVSP-XXJK-PCB               PIC X.                                   
066800 01  AVSP-ZZAC-PCB               PIC X.                                   
066900 01  AVSP-ORQI-PCB               PIC X.                                   
067000 01  AVSP-ORQL-PCB               PIC X.                                   
067100 01  AVSP-WDE6E-PCB              PIC X.                                   
067200 01  AVSP-WDE62-PCB              PIC X.                                   
067300 01  AVSP-ORQICSQ-PCB            PIC X.                                   
067400 01  AVSP-ORQM-PCB               PIC X.                                   
067500 01  AVSP-WDE4A-PCB              PIC X.                                   
067600 01  AVSP-WDM2-PCB               PIC X.                                   
067700 01  AVSP-ORQA2-PCB              PIC X.                                   
067800 01  AVSP-ORDP1-PCB              PIC X.                                   
067900 01  AVSP-ORDP2-PCB              PIC X.                                   
068000 01  AVSP-XXJN-PCB               PIC X.                                   
068100 01  AVSP-XXKH-PCB               PIC X.                                   
068200 01  AVSP-ARTC-PCB               PIC X.                                   
068300 01  AVSP-WDK7-PCB               PIC X.                                   
068400 01  AVSP-ARTM-PCB               PIC X.                                   
068500 01  AVSP-AUTF-PCB               PIC X.                                   
068600 01  AVSP-4487-PCB               PIC X.                                   
068700 01  AVSP-4541-PCB               PIC X.                                   
068800 01  AVSP-LOGA-PCB               PIC X.                                   
068900 01  AVSP-WDK6-PCB               PIC X.                                   
069000 01  AVSP-WDA6B-PCB              PIC X.                                   
069100 01  AVSP-WDA6-PCB               PIC X.                                   
069200 01  AVSP-WDP4A-PCB              PIC X.                                   
069300 01  AVSP-WDB6-PCB               PIC X.                                   
069400 01  AVSP-PLATS-XXDM-PCB         PIC X.                                   
069500 01  AVSP-PLATS-XXDN-PCB         PIC X.                                   
069600 01  AVSP-PLATS-XXDP-PCB         PIC X.                                   
069700 01  AVSP-PLATS-XXDO-PCB         PIC X.                                   
069800 01  AVSP-PLATS-WDE6C-PCB        PIC X.                                   
069900 01  AVSP-PLATS-GMTC-PCB         PIC X.                                   
070000 01  AVSP-PLATS-WDB6-PCB         PIC X.                                   
070100 01  AVSP-DNOT-ORQP-PCB          PIC X.                                   
070200 01  AVSP-DNOT-ORQP2-PCB         PIC X.                                   
070300 01  AVSP-DNOT-ORQP3-PCB         PIC X.                                   
070400 01  AVSP-DNOT-4013-PCB          PIC X.                                   
070500 01  AVSP-DNOT-BENA-PCB          PIC X.                                   
070600 01  AVSP-PRQU-WDG2-PCB          PIC X.                                   
070700 01  AVSP-PRQU-WDC7-PCB          PIC X.                                   
070800 01  AVSP-PRQU-SJKO-WDK6-PCB     PIC X.                                   
070900 01  AVSP-PRNO-3107-PCB          PIC X.                                   
071000 01  AVSP-TMS-1165-PCB           PIC X.                                   
071100 01  AVSP-TMS-4141-PCB           PIC X.                                   
071200 01  AVSP-TMS-WDB2-PCB           PIC X.                                   
071210 01  AVSP-TMS-WDB6-PCB           PIC X.                                   
071220 01  AVSP-TMS-WDD3-PCB           PIC X.                                   
071230 01  AVSP-TMS-WDB1-PCB           PIC X.                                   
071240 01  AVSP-TMS-WDE4A-PCB          PIC X.                                   
071250 01  AVSP-TMS-WDE4F-PCB          PIC X.                                   
071260 01  AVSP-TMS-WDQ2-PCB           PIC X.                                   
071270 01  AVSP-TMS-WDQ3-PCB           PIC X.                                   
071280 01  AVSP-TMS-WDK6-PCB           PIC X.                                   
071290 01  AVSP-TMS-WDE6-PCB           PIC X.                                   
071291 01  AVSP-TMS-WDK5-PCB           PIC X.                                   
071292 01  AVSP-TMS-WDQ2C-PCB          PIC X.                                   
071300                                                                          
071400 PROCEDURE DIVISION USING MSG-PCB                                         
071500       ALT4313-PCB    ALT4317-PCB    ALT4333-PCB  ALT4341-PCB             
071600       AVSP-4342-PCB  AVSP-2191-PCB                                       
071700       AVSP-4349-PCB  MAIL-PCB DISTRDOC-PCB                               
071710       TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                                   
071800       USEA-PCB       EMBB-PCB WDE6-PCB ORQA-PCB WDE4-PCB WDB6-PCB        
071900       WDE4A-PCB WDK6-PCB WDK7-PCB                                        
072000       AVSP-USEA-PCB                                                      
072100       AVSP-WDE41-PCB AVSP-WDE42-PCB AVSP-WDE4-PCB AVSP-WDE4B-PCB         
072200       AVSP-WDE4E-PCB AVSP-WDE6-PCB  AVSP-XXDV-PCB AVSP-ORQA-PCB          
072300       AVSP-XXKW-PCB  AVSP-XXLB-PCB  AVSP-XXJK-PCB AVSP-ZZAC-PCB          
072400       AVSP-ORQI-PCB  AVSP-ORQL-PCB  AVSP-WDE6E-PCB AVSP-WDE62-PCB        
072500       AVSP-ORQICSQ-PCB              AVSP-ORQM-PCB                        
072600       AVSP-WDE4A-PCB AVSP-WDM2-PCB  AVSP-ORQA2-PCB                       
072700       AVSP-ORDP1-PCB AVSP-ORDP2-PCB AVSP-XXJN-PCB AVSP-XXKH-PCB          
072800       AVSP-ARTC-PCB  AVSP-WDK7-PCB  AVSP-ARTM-PCB AVSP-AUTF-PCB          
072900       AVSP-4487-PCB  AVSP-4541-PCB AVSP-LOGA-PCB                         
073000       AVSP-WDK6-PCB  AVSP-WDA6B-PCB AVSP-WDA6-PCB AVSP-WDP4A-PCB         
073100       AVSP-WDB6-PCB                                                      
073200       AVSP-PLATS-XXDM-PCB AVSP-PLATS-XXDN-PCB AVSP-PLATS-XXDP-PCB        
073300       AVSP-PLATS-XXDO-PCB AVSP-PLATS-WDE6C-PCB                           
073400       AVSP-PLATS-GMTC-PCB AVSP-PLATS-WDB6-PCB                            
073500       AVSP-DNOT-ORQP-PCB                                                 
073600       AVSP-DNOT-ORQP2-PCB                                                
073700       AVSP-DNOT-ORQP3-PCB                                                
073800       AVSP-DNOT-4013-PCB                                                 
073900       AVSP-DNOT-BENA-PCB                                                 
074000       AVSP-PRQU-WDG2-PCB                                                 
074100       AVSP-PRQU-WDC7-PCB                                                 
074200       AVSP-PRQU-SJKO-WDK6-PCB                                            
074300       AVSP-PRNO-3107-PCB                                                 
074400       AVSP-TMS-1165-PCB                                                  
074500       AVSP-TMS-4141-PCB                                                  
074600       AVSP-TMS-WDB2-PCB                                                  
074610       AVSP-TMS-WDB6-PCB                                                  
074620       AVSP-TMS-WDD3-PCB                                                  
074630       AVSP-TMS-WDB1-PCB                                                  
074640       AVSP-TMS-WDE4A-PCB                                                 
074650       AVSP-TMS-WDE4F-PCB                                                 
074660       AVSP-TMS-WDQ2-PCB                                                  
074670       AVSP-TMS-WDQ3-PCB                                                  
074680       AVSP-TMS-WDK6-PCB                                                  
074690       AVSP-TMS-WDE6-PCB                                                  
074691       AVSP-TMS-WDK5-PCB                                                  
074692       AVSP-TMS-WDQ2C-PCB.                                                
074700                                                                          
074800 MAIN SECTION.                                                            
074900     ENTRY 'DLITCBL' USING MSG-PCB                                        
075000       ALT4313-PCB    ALT4317-PCB    ALT4333-PCB  ALT4341-PCB             
075100       AVSP-4342-PCB  AVSP-2191-PCB                                       
075200       AVSP-4349-PCB  MAIL-PCB DISTRDOC-PCB                               
075210       TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                                   
075300       USEA-PCB       EMBB-PCB WDE6-PCB ORQA-PCB WDE4-PCB WDB6-PCB        
075400       WDE4A-PCB WDK6-PCB WDK7-PCB                                        
075500       AVSP-USEA-PCB                                                      
075600       AVSP-WDE41-PCB AVSP-WDE42-PCB AVSP-WDE4-PCB AVSP-WDE4B-PCB         
075700       AVSP-WDE4E-PCB AVSP-WDE6-PCB  AVSP-XXDV-PCB AVSP-ORQA-PCB          
075800       AVSP-XXKW-PCB  AVSP-XXLB-PCB  AVSP-XXJK-PCB AVSP-ZZAC-PCB          
075900       AVSP-ORQI-PCB  AVSP-ORQL-PCB  AVSP-WDE6E-PCB AVSP-WDE62-PCB        
076000       AVSP-ORQICSQ-PCB              AVSP-ORQM-PCB                        
076100       AVSP-WDE4A-PCB AVSP-WDM2-PCB  AVSP-ORQA2-PCB                       
076200       AVSP-ORDP1-PCB AVSP-ORDP2-PCB AVSP-XXJN-PCB AVSP-XXKH-PCB          
076300       AVSP-ARTC-PCB  AVSP-WDK7-PCB  AVSP-ARTM-PCB AVSP-AUTF-PCB          
076400       AVSP-4487-PCB  AVSP-4541-PCB AVSP-LOGA-PCB                         
076500       AVSP-WDK6-PCB  AVSP-WDA6B-PCB AVSP-WDA6-PCB AVSP-WDP4A-PCB         
076600       AVSP-WDB6-PCB                                                      
076700       AVSP-PLATS-XXDM-PCB AVSP-PLATS-XXDN-PCB AVSP-PLATS-XXDP-PCB        
076800       AVSP-PLATS-XXDO-PCB AVSP-PLATS-WDE6C-PCB                           
076900       AVSP-PLATS-GMTC-PCB AVSP-PLATS-WDB6-PCB                            
077000       AVSP-DNOT-ORQP-PCB                                                 
077100       AVSP-DNOT-ORQP2-PCB                                                
077200       AVSP-DNOT-ORQP3-PCB                                                
077300       AVSP-DNOT-4013-PCB                                                 
077400       AVSP-DNOT-BENA-PCB                                                 
077500       AVSP-PRQU-WDG2-PCB                                                 
077600       AVSP-PRQU-WDC7-PCB                                                 
077700       AVSP-PRQU-SJKO-WDK6-PCB                                            
077800       AVSP-PRNO-3107-PCB                                                 
077900       AVSP-TMS-1165-PCB                                                  
078000       AVSP-TMS-4141-PCB                                                  
078010       AVSP-TMS-WDB2-PCB                                                  
078020       AVSP-TMS-WDB6-PCB                                                  
078030       AVSP-TMS-WDD3-PCB                                                  
078040       AVSP-TMS-WDB1-PCB                                                  
078050       AVSP-TMS-WDE4A-PCB                                                 
078060       AVSP-TMS-WDE4F-PCB                                                 
078070       AVSP-TMS-WDQ2-PCB                                                  
078080       AVSP-TMS-WDQ3-PCB                                                  
078090       AVSP-TMS-WDK6-PCB                                                  
078100       AVSP-TMS-WDE6-PCB                                                  
078110       AVSP-TMS-WDK5-PCB                                                  
078120       AVSP-TMS-WDQ2C-PCB.                                                
078200                                                                          
078300     PERFORM IMS-GET-MSG                                                  
078400                                                                          
078500     IF SEGMENT-FINNS                                                     
078600       PERFORM A-INIT-SPARA-INPUT                                         
078700                                                                          
078800       IF GOOD-MID AND KEYS-OK                                            
078900                                                                          
079000          IF MID-W4I31301 NOT = ALL '+'                                   
079100             PERFORM B-KONTROLL-INDATA-RAD                                
079200             IF INDATA-OK                                                 
079300                PERFORM C-KONTROLL-READ-DB                                
079400                IF INDATA-OK                                              
079500                   PERFORM D-UPDATE                                       
079600                ELSE                                                      
079700                   MOVE ERR-HIGHLIT-FIELDS-WRON TO MED-IDMFSFEL           
079800                   PERFORM S07-ERR-ROUTINE-ROER-EJ-FAELT                  
079900                END-IF                                                    
080000             ELSE                                                         
080100                MOVE ERR-HIGHLIT-FIELDS-WRON   TO MED-IDMFSFEL            
080200                PERFORM S07-ERR-ROUTINE-ROER-EJ-FAELT                     
080300             END-IF                                                       
080400          END-IF                                                          
080500       END-IF                                                             
080600       IF NOT OMSTART AND 4317-INTE-STARTAD                               
080700         PERFORM S11-SAETT-CURSOR-FOER-SDC                                
080800         PERFORM S20-DEFAULT-TO-FLFEL                                     
080900                                                                          
081000         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O31301 + 4                    
081100         PERFORM IMS-INSERT-MSG                                           
081200       END-IF                                                             
081300     END-IF                                                               
081400                                                                          
081500     MOVE ZERO TO RETURN-CODE                                             
081600     GOBACK                                                               
081700     .                                                                    
081800     EJECT                                                                
081900 A-INIT-SPARA-INPUT SECTION.                                              
082000                                                                          
082100     MOVE 'A-INIT-SPARA-'        TO WS-AKTUELL-SEKTION                    
082200                                                                          
082300     MOVE FUNCTION CURRENT-DATE(1:8)                                      
082400                                 TO WS-CURRENT-DATE                       
082500     MOVE FUNCTION CURRENT-DATE(9:8)                                      
082600                                 TO WS-CURRENT-TIME                       
083100     IF MSG-DUBBLA-TRANSKODER                                             
083200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I31301                 
083300       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
083400       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
083500     ELSE                                                                 
083600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I31301                  
083700       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
083800       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
083900     END-IF                                                               
084000     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
084100     MOVE MSG-IDPFK            TO MFS-IDPFK                               
084200     MOVE MFS-IDTRANS            TO WS-IDTRANS                            
084300                                                                          
084400     MOVE LOW-VALUE              TO MSG-AREA                              
084500     MOVE 'W4O313N1'             TO MFS-IDMOD                             
084600     MOVE '4313'                 TO MOD-IDTRANS                           
084700                                                                          
084800     PERFORM AA-RENSA-MOD-FAELT                                           
084900     IF NOT OWN-MID                                                       
085000       MOVE '++'              TO MID-KDPRTVAL-ADR                         
085100                                 MID-KDPRTVAL-FS                          
085200     END-IF                                                               
085300                                                                          
085400     PERFORM AB-NOLLA-ARB-TAB                                             
085500     PERFORM AC-KONTROLL-IDDC                                             
085600     .                                                                    
085700     EJECT                                                                
085800 AA-RENSA-MOD-FAELT       SECTION.                                        
085900     MOVE 'AA-RENSA-MOD-'        TO WS-AKTUELL-SEKTION                    
086000                                                                          
086100     MOVE +1                TO RADIND                                     
086200     PERFORM UNTIL RADIND > MAX-RADINDX                                   
086300       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR        (RADIND)                
086400                               MOD-IDPRODNR       (RADIND)                
086500                               MOD-IDPLKLST       (RADIND)                
086600                               MOD-IDKOLLI        (RADIND)                
086700                               MOD-KDKOLLI        (RADIND)                
086800                               MOD-VKORDBTO-KOLLI (RADIND)                
086900                               MOD-KDEMBTYP       (RADIND)                
087000                               MOD-DIKOLLIL       (RADIND)                
087100                               MOD-DIKOLLIB       (RADIND)                
087200                               MOD-DIKOLLIH       (RADIND)                
087300                               MOD-KDPRTVAL-ADR-RAD (RADIND)              
087400                               MOD-KDPRTVAL-FS-RAD (RADIND)               
087500       ADD +1               TO RADIND                                     
087600     END-PERFORM                                                          
087700                                                                          
087800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
087900                             MOD-TEMFSINF                                 
088000                             MOD-IDANSTNR                                 
088100     .                                                                    
088200     SKIP2                                                                
088300 AB-NOLLA-ARB-TAB         SECTION.                                        
088400     MOVE 'AB-NOLLA-ARB-'        TO WS-AKTUELL-SEKTION                    
088500                                                                          
088600     MOVE +1                TO RADIND                                     
088700     PERFORM UNTIL RADIND > MAX-RADINDX                                   
088800       MOVE ZERO            TO WS-IDDISTR        (RADIND)                 
088900                               WS-IDPRODNR       (RADIND)                 
089000                               WS-IDKOLLI        (RADIND)                 
089100                               WS-IDKUNDNR       (RADIND)                 
089200                               WS-IDORDNR        (RADIND)                 
089300                               WS-IDPLKLST       (RADIND)                 
089400                               WS-KVORDRAD       (RADIND)                 
089500                               WS-KDFRAKT        (RADIND)                 
089600                               WS-IDKUNDRF       (RADIND)                 
089700                               WS-VKTARA         (RADIND)                 
089800                               WS-VKORDBTO       (RADIND)                 
089900                               WS-KDEMBTYP       (RADIND)                 
090000                               WS-DIKOLLIL       (RADIND)                 
090100                               WS-DIKOLLIB       (RADIND)                 
090200                               WS-DIKOLLIH       (RADIND)                 
090300                               WS-KDKOLLID       (RADIND)                 
090400       MOVE SPACE           TO WS-KDKOLLI        (RADIND)                 
090500       ADD +1               TO RADIND                                     
090600     END-PERFORM                                                          
090700     .                                                                    
090800     EJECT                                                                
090900 AC-KONTROLL-IDDC           SECTION.                                      
091000     MOVE 'AC-KONTROLL-ID'       TO WS-AKTUELL-SEKTION                    
091100                                                                          
091200     MOVE ALL '+'               TO MSGI-WMSGINIT                          
091300     MOVE '001'                 TO MSGI-KDCALL                            
091400     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
091500     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
091600     MOVE '4313'                TO MSGI-IDTRANS                           
091700                                                                          
091800                                                                          
091900     IF WS-IDTRANS = '4317'                                               
092000*OM TRANS FR 4317 HÄMTA 4313-MID FR MSGI-SPAR-AREA..                      
092100       MOVE MSGI-SPAR-AREA      TO SPAR-AREA                              
092200       MOVE USER-MID-W4I31301   TO MID-W4I31301                           
092300                                                                          
092400     END-IF                                                               
092500                                                                          
092600     MOVE MID-KDPRTVAL-ADR      TO MSGI-KDPRTVAL-ADR                      
092700     MOVE MID-KDPRTVAL-FS       TO MSGI-KDPRTVAL-FS                       
092800                                                                          
092900                                                                          
093000                                                                          
093100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
093200                                                                          
093300                                                                          
093400     MOVE MSGI-KDMATT           TO WS-KDMATT                              
093500                                                                          
093600     MOVE MFS-RENSA-FAELT       TO MOD-IDDC-IN                            
093700                                                                          
093800     MOVE NEJ                   TO OMSTART-SW                             
093900                                   SW-STARTA-4317                         
094000     MOVE JA                    TO KEYS-SW                                
094100                                   INDATA-SW                              
094200                                                                          
094300     MOVE MSGI-IDLAND-SPR       TO MED-IDSKYLT                            
094400                                                                          
094500     IF MSGI-IDLAND-SPR = 'GB'                                            
094600       MOVE +2 TO INDX                                                    
094700                  WS-KDMFSFOR                                             
094800     ELSE                                                                 
094900       MOVE +1 TO INDX                                                    
095000                  WS-KDMFSFOR                                             
095100     END-IF                                                               
095200                                                                          
095300     IF MSGI-IDDC IS > SPACE                                              
095400       MOVE MSGI-IDDC           TO MOD-IDDC-UT                            
095500                                   WS-IDDC                                
095600                                   W-IDDC                                 
095700     ELSE                                                                 
095800       MOVE NEJ                 TO KEYS-SW                                
095900     END-IF                                                               
096000                                                                          
096100     IF MSGI-KDPRTVAL-ADR > SPACE                                         
096200       MOVE MSGI-KDPRTVAL-ADR   TO WS-KDPRTVAL-ADR                        
096300                                   MOD-KDPRTVAL-ADR                       
096400     END-IF                                                               
096500                                                                          
096600     IF MSGI-KDPRTVAL-FS  > SPACE                                         
096700       MOVE MSGI-KDPRTVAL-FS    TO WS-KDPRTVAL-FS                         
096800                                   MOD-KDPRTVAL-FS                        
096900     END-IF                                                               
097000                                                                          
097100                                                                          
097200     IF KEYS-FEL                                                          
097300        MOVE ERR-WRONG-KEY             TO MED-IDMFSFEL                    
097400        PERFORM S07-ERR-ROUTINE-ROER-EJ-FAELT                             
097500     ELSE                                                                 
097600                                                                          
097700       MOVE MSGI-IDDC TO W-IDDC-B6                                        
097800       PERFORM IMS-GU-WDB601                                              
097900       IF DCS-SDC OR DCS-NDC-NA OR DCS-NDC-PF                             
098000         MOVE +1                TO RADIND                                 
098100         PERFORM UNTIL RADIND > MAX-RADINDX                               
098200           MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR      (RADIND)        
098300                                                                          
098400           MOVE MFS-STAENG-FAELT-NOMOD TO                                 
098500                MOD-IDDISTR-ATTR (RADIND)                                 
098600           ADD +1               TO RADIND                                 
098700         END-PERFORM                                                      
098800       END-IF                                                             
098900     END-IF                                                               
099000     .                                                                    
099100     SKIP2                                                                
099200 B-KONTROLL-INDATA-RAD    SECTION.                                        
099300     MOVE 'B-KONTROLL-INDAT'     TO WS-AKTUELL-SEKTION                    
099400                                                                          
099500     PERFORM BA-CHECK-MID-KDPRTVAL-ADR                                    
099600     PERFORM BK-CHECK-MID-KDPRTVAL-FS                                     
099700     PERFORM BL-CHECK-MID-IDANSTNR                                        
099800                                                                          
099900     MOVE +1         TO RADIND                                            
100000     MOVE NEJ        TO INDATA-RAD-BEHANDLAD-SW                           
100100                                                                          
100200     PERFORM UNTIL RADIND > MAX-RADINDX                                   
100300                OR INDATA-RAD-BEHANDLAD                                   
100400                                                                          
100500       IF MID-RAD    (RADIND) NOT = ALL '+'  AND                          
100600          MID-FLAVSP (RADIND)     = '+'                                   
100700                                                                          
100800         PERFORM BB-CHECK-MID-IDDISTR                                     
100900                                                                          
101000         PERFORM BC-CHECK-MID-IDPRODNR-IDPLKLST                           
101100                                                                          
101200         PERFORM BD-CHECK-MID-IDKOLLI                                     
101300                                                                          
101400         PERFORM BE-CHECK-MID-KDKOLLI-KDEMBTYP                            
101500                                                                          
101600         PERFORM BF-CHECK-MID-VKORDBTO                                    
101700                                                                          
101800         PERFORM BG-CHECK-MID-KOLLI-DIMENSION                             
101900                                                                          
102000         PERFORM BH-MOVE-TAB-PRTVAL-AF-FS                                 
102100         MOVE JA     TO INDATA-RAD-BEHANDLAD-SW                           
102200       END-IF                                                             
102300                                                                          
102400       ADD +1        TO RADIND                                            
102500     END-PERFORM                                                          
102600                                                                          
102700     IF INDATA-OK                                                         
102800                                                                          
102900        PERFORM BI-CHECK-PRODNR-PLKLST                                    
103000**** KONTROLL AV ATT INGA DUBLETTER (IDPRODNR) RAPPORTERAS                
103100     END-IF                                                               
103200                                                                          
103300     IF INDATA-FEL                                                        
103400                                                                          
103500       PERFORM MFS-ALFA-FAELT-RAETT-REST                                  
103600     END-IF                                                               
103700     .                                                                    
103800     EJECT                                                                
103900 BA-CHECK-MID-KDPRTVAL-ADR       SECTION.                                 
104000     MOVE 'BA-CHECK-MID-KD '     TO WS-AKTUELL-SEKTION                    
104100                                                                          
104200     IF  DCS-SDC AND (DCS-ENGLAND)                                        
104300       MOVE MFS-STAENG-FAELT-NOMOD TO MOD-KDPRTVAL-ADR-ATTR               
104400       MOVE 'UU'                   TO WS-MID-ADRESSFL                     
104500     ELSE                                                                 
104600       IF MSGI-KDPRTVAL-ADR = ALL '+' OR 'U ' OR 'UU'                     
104700         IF DCS-SDC AND (DCS-SPAIN OR DCS-ITALY)                          
104800           MOVE NEJ                    TO INDATA-SW                       
104900           MOVE INF-WRONG-PRINTER      TO MED-IDMFSFEL                    
105000           PERFORM S09-ERR-ROUTINE                                        
105100           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDPRTVAL-ADR-ATTR            
105200         ELSE                                                             
105300           IF DCS-NDC-NA AND (DCS-CANADA)                                 
105400             IF MSGI-KDPRTVAL-ADR = ALL '+'                               
105500               MOVE NEJ                TO INDATA-SW                       
105600               MOVE INF-WRONG-PRINTER  TO MED-IDMFSFEL                    
105700               PERFORM S09-ERR-ROUTINE                                    
105800               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-ADR-ATTR           
105900             ELSE                                                         
106000               MOVE 'UU'               TO WS-MID-ADRESSFL                 
106100                                          MOD-KDPRTVAL-ADR                
106200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-ADR-ATTR           
106300             END-IF                                                       
106400           ELSE                                                           
106500             MOVE 'UU'                 TO WS-MID-ADRESSFL                 
106600                                          MOD-KDPRTVAL-ADR                
106700             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-ADR-ATTR           
106800           END-IF                                                         
106900         END-IF                                                           
107000       ELSE                                                               
107100          MOVE '4'                     TO WS-SYSTDEL                      
107200          MOVE 'KF'                    TO WS-LISTTYP                      
107300          MOVE MSGI-IDDC               TO WS-DC                           
107400          MOVE MSGI-KDPRTVAL-ADR       TO WS-KDPRT                        
107500                                                                          
107600          MOVE 001                     TO PRT-KDCALL                      
107700          MOVE WS-IDPRTLST             TO PRT-IDPRTLST                    
107800                                                                          
107900          CALL W006PRT USING PRT-W006PRT                                  
108000                                                                          
108100          IF PRT-KDSVAR = RAETT                                           
108200             MOVE RAETT                TO WS-PRT-KDSVAR-ADRESSFL          
108300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-ADR-ATTR           
108400             MOVE MSGI-KDPRTVAL-ADR    TO WS-MID-ADRESSFL                 
108500          ELSE                                                            
108600             IF INDATA-OK                                                 
108700               MOVE NEJ                TO INDATA-SW                       
108800               MOVE INF-WRONG-PRINTER  TO MED-IDMFSFEL                    
108900               PERFORM S09-ERR-ROUTINE                                    
109000               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-ADR-ATTR           
109100             END-IF                                                       
109200          END-IF                                                          
109300       END-IF                                                             
109400     END-IF                                                               
109500     .                                                                    
109600     SKIP2                                                                
109700 BK-CHECK-MID-KDPRTVAL-FS             SECTION.                            
109800     MOVE 'BK-CHECK-MID-KDP'     TO WS-AKTUELL-SEKTION                    
109900                                                                          
110000     IF      ENGLISH-TEXT                                                 
110100     AND NOT SDC-NL                                                       
110200     AND NOT LDC-GB-3A                                                    
110300     AND NOT LDC-GB-2C                                                    
110400                                                                          
110500       MOVE MFS-STAENG-FAELT-NOMOD TO MOD-KDPRTVAL-FS-ATTR                
110600*      MOVE 'UU'                   TO WS-MID-FOLJES                       
110700*     STRING 'POS-BK-1' '*' MSGI-IDDC '*' MSGI-KDMFSFOR '*'               
110800*     DELIMITED BY SIZE INTO MOD-TEMFSINF                                 
110900     ELSE                                                                 
111000       IF MSGI-KDPRTVAL-FS = ALL '+' OR 'U ' OR 'UU'                      
111100       AND NOT SDC-NL                                                     
111200       AND NOT LDC-GB-3A                                                  
111300       AND NOT LDC-GB-2C                                                  
111400         MOVE 'UU'                TO WS-MID-FOLJES                        
111500                                     MOD-KDPRTVAL-FS                      
111600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-FS-ATTR                
111700       ELSE                                                               
111800         IF MSGI-KDPRTVAL-FS > SPACE AND                                  
111900            MSGI-KDPRTVAL-FS NOT = 'UU'                                   
112000           MOVE '4'                    TO WS-SYSTDEL                      
112100           MOVE 'FS'                   TO WS-LISTTYP                      
112200           MOVE MSGI-IDDC              TO WS-DC                           
112300           MOVE MSGI-KDPRTVAL-FS       TO WS-KDPRT                        
112400                                                                          
112500           MOVE 001                    TO PRT-KDCALL                      
112600           MOVE WS-IDPRTLST            TO PRT-IDPRTLST                    
112700                                                                          
112800           CALL W006PRT  USING PRT-W006PRT                                
112900           IF PRT-KDSVAR = RAETT                                          
113000             MOVE RAETT                TO WS-PRT-KDSVAR-FOLJES            
113100             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-FS-ATTR            
113200             MOVE MSGI-KDPRTVAL-FS     TO WS-MID-FOLJES                   
113300           ELSE                                                           
113400             IF INDATA-OK                                                 
113500               MOVE NEJ                TO INDATA-SW                       
113600               MOVE INF-WRONG-PRINTER  TO MED-IDMFSFEL                    
113700               PERFORM S09-ERR-ROUTINE                                    
113800               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-FS-ATTR            
113900             END-IF                                                       
114000           END-IF                                                         
114100         END-IF                                                           
114200       END-IF                                                             
114300     END-IF                                                               
114400     .                                                                    
114500     SKIP2                                                                
114600 BB-CHECK-MID-IDDISTR                 SECTION.                            
114700     MOVE 'BB-CHECK-MID-ID '     TO WS-AKTUELL-SEKTION                    
114800                                                                          
114900     IF DCS-CDC                                                           
115000       IF MID-IDDISTR (RADIND) = ALL '+'                                  
115100          MOVE NEJ                    TO INDATA-SW                        
115200          MOVE MFS-NUM-FAELT-FEL      TO MOD-IDDISTR-ATTR (RADIND)        
115300                                                                          
115400       ELSE                                                               
115500         IF MID-IDDISTR (RADIND) NUMERIC                                  
115600            MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-ATTR (RADIND)        
115700         ELSE                                                             
115800            MOVE NEJ                 TO INDATA-SW                         
115900            MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-ATTR (RADIND)         
116000         END-IF                                                           
116100       END-IF                                                             
116200     ELSE                                                                 
116300       MOVE MFS-RENSA-FAELT        TO MOD-IDDISTR      (RADIND)           
116400       MOVE MFS-STAENG-FAELT-NOMOD TO MOD-IDDISTR-ATTR (RADIND)           
116500     END-IF                                                               
116600     .                                                                    
116700     SKIP2                                                                
116800 BC-CHECK-MID-IDPRODNR-IDPLKLST       SECTION.                            
116900     MOVE 'BC-CHECK-MID-ID '     TO WS-AKTUELL-SEKTION                    
117000                                                                          
117100     IF MID-IDPRODNR (RADIND) = ALL '+'                                   
117200        MOVE NEJ                     TO INDATA-SW                         
117300        MOVE MFS-NUM-FAELT-FEL     TO MOD-IDPRODNR-ATTR (RADIND)          
117400     ELSE                                                                 
117500       IF MID-IDPRODNR (RADIND) NUMERIC                                   
117600          MOVE MID-IDPRODNR(RADIND) TO WS-IDPRODNR (RADIND)               
117700          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPRODNR-ATTR (RADIND)          
117800       ELSE                                                               
117900          MOVE NEJ                   TO INDATA-SW                         
118000          MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPRODNR-ATTR (RADIND)          
118100          MOVE RADIND     TO RADIND-NUM9                                  
118200       END-IF                                                             
118300     END-IF                                                               
118400                                                                          
118500     IF MID-IDPLKLST (RADIND) = ALL '+'                                   
118600        MOVE NEJ                     TO INDATA-SW                         
118700        MOVE MFS-NUM-FAELT-FEL     TO MOD-IDPLKLST-ATTR (RADIND)          
118800     ELSE                                                                 
118900       IF MID-IDPLKLST (RADIND) NUMERIC                                   
119000          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPLKLST-ATTR (RADIND)          
119100          MOVE MID-IDPLKLST(RADIND)  TO WS-IDPLKLST (RADIND)              
119200       ELSE                                                               
119300          MOVE NEJ                   TO INDATA-SW                         
119400          MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPLKLST-ATTR (RADIND)          
119500       END-IF                                                             
119600     END-IF                                                               
119700     .                                                                    
119800     SKIP2                                                                
119900 BD-CHECK-MID-IDKOLLI                 SECTION.                            
120000     MOVE 'BD-CHECK-MID-ID '     TO WS-AKTUELL-SEKTION                    
120100                                                                          
120200     IF MID-KOLLI-INFO (RADIND) NOT = ALL '+'                             
120300       IF MID-IDKOLLI (RADIND) = ALL '+'                                  
120400          MOVE NEJ                   TO INDATA-SW                         
120500          MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKOLLI-ATTR (RADIND)            
120600       END-IF                                                             
120700     END-IF                                                               
120800                                                                          
120900     IF MID-IDKOLLI (RADIND) = ALL '+'                                    
121000        MOVE NEJ                     TO INDATA-SW                         
121100        MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKOLLI-ATTR (RADIND)           
121200     ELSE                                                                 
121300       IF  MID-IDKOLLI (RADIND) NUMERIC                                   
121400         IF MID-IDKOLLI (RADIND) = ZERO                                   
121500            MOVE NEJ                 TO INDATA-SW                         
121600            MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKOLLI-ATTR (RADIND)         
121700         ELSE                                                             
121800            MOVE MFS-NUM-FAELT-RAETT TO                                   
121900                 MOD-IDKOLLI-ATTR (RADIND)                                
122000            MOVE MID-IDKOLLI (RADIND) TO WS-IDKOLLI (RADIND)              
122100         END-IF                                                           
122200       ELSE                                                               
122300          MOVE NEJ                   TO INDATA-SW                         
122400          MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKOLLI-ATTR (RADIND)         
122500       END-IF                                                             
122600     END-IF                                                               
122700     .                                                                    
122800     SKIP2                                                                
122900 BE-CHECK-MID-KDKOLLI-KDEMBTYP        SECTION.                            
123000     MOVE 'BE-CHECK-MID-KDK'     TO WS-AKTUELL-SEKTION                    
123100                                                                          
123200     IF MID-KDKOLLI (RADIND) = ALL '+'                                    
123300       IF DCS-SDC AND DCS-ENGLAND AND                                     
123400          MID-KDEMBTYP (RADIND) = ALL '+'                                 
123500                                                                          
123600          MOVE 'NC'                 TO WS-KDKOLLI  (RADIND)               
123700                                      MOD-KDKOLLI (RADIND)                
123800          MOVE MFS-ALFA-FAELT-RAETT TO                                    
123900                               MOD-KDKOLLI-ATTR (RADIND)                  
124000       ELSE                                                               
124100          MOVE MFS-ALFA-FAELT-RAETT TO                                    
124200                               MOD-KDKOLLI-ATTR (RADIND)                  
124300       END-IF                                                             
124400     ELSE                                                                 
124500        MOVE MFS-ALFA-FAELT-RAETT  TO                                     
124600                             MOD-KDKOLLI-ATTR (RADIND)                    
124700        MOVE MID-KDKOLLI (RADIND) TO WS-KDKOLLI (RADIND)                  
124800     END-IF                                                               
124900                                                                          
125000     IF MID-KDEMBTYP (RADIND) = ALL '+'                                   
125100       CONTINUE                                                           
125200     ELSE                                                                 
125300       IF (MID-KDEMBTYP (RADIND) NUMERIC)                                 
125400       AND (MID-KDEMBTYP (RADIND) > 0 AND < 8)                            
125500       AND (MID-KDKOLLI  (RADIND) = ALL '+')                              
125600          MOVE MFS-NUM-FAELT-RAETT   TO                                   
125700                            MOD-KDEMBTYP-ATTR (RADIND)                    
125800          MOVE MID-KDEMBTYP (RADIND) TO WS-KDEMBTYP (RADIND)              
125900       ELSE                                                               
126000          MOVE NEJ                   TO INDATA-SW                         
126100          MOVE MFS-NUM-FAELT-FEL   TO                                     
126200                            MOD-KDEMBTYP-ATTR (RADIND)                    
126300       END-IF                                                             
126400     END-IF                                                               
126500     .                                                                    
126600     SKIP2                                                                
126700 BF-CHECK-MID-VKORDBTO                SECTION.                            
126800     MOVE 'BF-CHECK-MID-VKO'     TO WS-AKTUELL-SEKTION                    
126900                                                                          
127000     IF MID-VKORDBTO-KOLLI (RADIND) = ALL '+'                             
127100       CONTINUE                                                           
127200     ELSE                                                                 
127300        MOVE MFS-NUM-FAELT-RAETT     TO                                   
127400              MOD-VKORDBTO-KOLLI-ATTR (RADIND)                            
127500        MOVE MID-VKORDBTO-KOLLI (RADIND) TO DEC-IDFRIDATA                 
127600        MOVE 5                     TO DEC-KVHELTAL                        
127700        MOVE 1                     TO DEC-KVDECIMAL                       
127800        CALL WDECEDIT USING DEC-WDECAREA                                  
127900                                                                          
128000        IF DEC-KDSVAR-OK AND DEC-IDEDITDATA > ZERO                        
128100           MOVE DEC-IDEDITDATA  TO  WS-VKORDBTO        (RADIND)           
128200                                    WS-MOD-VKORDBTO                       
128300           MOVE WS-MOD-VKORDBTO      TO                                   
128400                MOD-VKORDBTO-KOLLI (RADIND)                               
128500                                                                          
128600           IF US-MEASUREMENT                                              
128700             COMPUTE WS-VKORDBTO (RADIND)  =                              
128800                     WS-VKORDBTO (RADIND) * CONV-LB-TO-KG                 
128900             END-COMPUTE                                                  
129000           END-IF                                                         
129100        ELSE                                                              
129200           MOVE 'TEST VKORDBTO BF-'    TO MED-IDMFSFEL                    
129300           MOVE NEJ                TO INDATA-SW                           
129400           MOVE MFS-NUM-FAELT-FEL TO                                      
129500                MOD-VKORDBTO-KOLLI-ATTR (RADIND)                          
129600        END-IF                                                            
129700     END-IF                                                               
129800     .                                                                    
129900     SKIP2                                                                
130000 BG-CHECK-MID-KOLLI-DIMENSION         SECTION.                            
130100     MOVE 'BG-CHECK-MID-KOL'     TO WS-AKTUELL-SEKTION                    
130200                                                                          
130300     IF MID-DIKOLLIL (RADIND) = ALL '+'                                   
130400       CONTINUE                                                           
130500     ELSE                                                                 
130600       IF MID-DIKOLLIL (RADIND) NUMERIC                                   
130700       AND MID-DIKOLLIL (RADIND) > ZERO                                   
130800          MOVE MFS-NUM-FAELT-RAETT TO                                     
130900                            MOD-DIKOLLIL-ATTR (RADIND)                    
131000          MOVE MID-DIKOLLIL (RADIND) TO WS-DIKOLLIL (RADIND)              
131100       ELSE                                                               
131200          MOVE NEJ                   TO INDATA-SW                         
131300          MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIL-ATTR (RADIND)            
131400       END-IF                                                             
131500     END-IF                                                               
131600                                                                          
131700     IF MID-DIKOLLIB (RADIND) = ALL '+'                                   
131800       CONTINUE                                                           
131900     ELSE                                                                 
132000       IF MID-DIKOLLIB (RADIND) NUMERIC                                   
132100       AND MID-DIKOLLIB (RADIND) > ZERO                                   
132200          MOVE MFS-NUM-FAELT-RAETT TO                                     
132300                            MOD-DIKOLLIB-ATTR (RADIND)                    
132400          MOVE MID-DIKOLLIB (RADIND) TO WS-DIKOLLIB (RADIND)              
132500       ELSE                                                               
132600          MOVE NEJ                   TO INDATA-SW                         
132700          MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIB-ATTR (RADIND)            
132800       END-IF                                                             
132900     END-IF                                                               
133000                                                                          
133100     IF MID-DIKOLLIH (RADIND) = ALL '+'                                   
133200       CONTINUE                                                           
133300     ELSE                                                                 
133400       IF MID-DIKOLLIH (RADIND) NUMERIC                                   
133500       AND MID-DIKOLLIH (RADIND) > ZERO                                   
133600          MOVE MFS-NUM-FAELT-RAETT TO                                     
133700                            MOD-DIKOLLIH-ATTR (RADIND)                    
133800          MOVE MID-DIKOLLIH (RADIND) TO WS-DIKOLLIH (RADIND)              
133900       ELSE                                                               
134000          MOVE NEJ                   TO INDATA-SW                         
134100          MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIH-ATTR (RADIND)            
134200       END-IF                                                             
134300     END-IF                                                               
134400                                                                          
134500     IF US-MEASUREMENT AND INDATA-OK                                      
134600       COMPUTE WS-DIKOLLIL (RADIND) =                                     
134700               WS-DIKOLLIL (RADIND) * CONV-IN-TO-CM                       
134800       END-COMPUTE                                                        
134900       COMPUTE WS-DIKOLLIB (RADIND) =                                     
135000               WS-DIKOLLIB (RADIND) * CONV-IN-TO-CM                       
135100       END-COMPUTE                                                        
135200       COMPUTE WS-DIKOLLIH (RADIND) =                                     
135300               WS-DIKOLLIH (RADIND) * CONV-IN-TO-CM                       
135400       END-COMPUTE                                                        
135500     END-IF                                                               
135600     .                                                                    
135700     SKIP2                                                                
135800 BH-MOVE-TAB-PRTVAL-AF-FS         SECTION.                                
135900                                                                          
136000     MOVE 'BH-MOVE-TAB-PRT '     TO WS-AKTUELL-SEKTION                    
136100                                                                          
136200     MOVE 'UU'                 TO MOD-KDPRTVAL-FS-RAD (RADIND)            
136300                                  MOD-KDPRTVAL-ADR-RAD (RADIND)           
136400                                                                          
136500     MOVE MFS-ALFA-FAELT-RAETT TO                                         
136600                               MOD-KDPRTVAL-FS-RAD-ATTR (RADIND)          
136700                               MOD-KDPRTVAL-ADR-RAD-ATTR (RADIND)         
136800                                                                          
136900     MOVE MFS-ROER-EJ-FAELT    TO MOD-KDPRTVAL-FS-RAD (RADIND)            
137000                                  MOD-KDPRTVAL-ADR-RAD(RADIND)            
137100     .                                                                    
137200     SKIP2                                                                
137300 BI-CHECK-PRODNR-PLKLST                    SECTION.                       
137400                                                                          
137500     MOVE 'BI-CHECK-PRODNR '     TO WS-AKTUELL-SEKTION                    
137600     MOVE +1         TO RADIND                                            
137700     MOVE NEJ        TO INDATA-RAD-BEHANDLAD-SW                           
137800                                                                          
137900     PERFORM UNTIL RADIND > MAX-RADINDX                                   
138000                OR INDATA-RAD-BEHANDLAD                                   
138100        IF MID-IDPRODNR (RADIND) NOT = ALL '+'  AND                       
138200           MID-FLAVSP   (RADIND)     = '+'                                
138300           MOVE RADIND   TO JMF-IND                                       
138400           ADD +1        TO JMF-IND                                       
138500                                                                          
138600           PERFORM UNTIL JMF-IND > MAX-RADINDX                            
138700              IF MID-IDPRODNR (JMF-IND) NOT = ALL '+'                     
138800                                                                          
138900                 IF (MID-IDPRODNR (RADIND) =                              
139000                     MID-IDPRODNR (JMF-IND)                               
139100                 AND                                                      
139200                     MID-IDPLKLST (RADIND) =                              
139300                     MID-IDPLKLST (JMF-IND))                              
139400                                                                          
139500                    MOVE NEJ     TO INDATA-SW                             
139600                    MOVE MFS-NUM-FAELT-FEL TO                             
139700                         MOD-IDPRODNR-ATTR (RADIND)                       
139800                    MOVE MFS-NUM-FAELT-FEL TO                             
139900                         MOD-IDPRODNR-ATTR (JMF-IND)                      
140000                 END-IF                                                   
140100                 IF (MID-IDPRODNR (RADIND) =                              
140200                     MID-IDPRODNR (JMF-IND)                               
140300                 AND                                                      
140400                     MID-IDKOLLI  (RADIND) =                              
140500                     MID-IDKOLLI  (JMF-IND))                              
140600                    MOVE NEJ     TO INDATA-SW                             
140700                    MOVE MFS-NUM-FAELT-FEL TO                             
140800                         MOD-IDKOLLI-ATTR (RADIND)                        
140900                    MOVE MFS-NUM-FAELT-FEL TO                             
141000                         MOD-IDKOLLI-ATTR (JMF-IND)                       
141100                 END-IF                                                   
141200              END-IF                                                      
141300              ADD +1        TO JMF-IND                                    
141400           END-PERFORM                                                    
141500                                                                          
141600           MOVE JA     TO INDATA-RAD-BEHANDLAD-SW                         
141700        END-IF                                                            
141800        ADD +1        TO RADIND                                           
141900     END-PERFORM                                                          
142000     .                                                                    
142100     SKIP2                                                                
142200 BL-CHECK-MID-IDANSTNR         SECTION.                                   
142300     MOVE 'BL-CHECK-MID-IDA'     TO WS-AKTUELL-SEKTION                    
142400                                                                          
142500     IF MID-IDANSTNR = ALL '+'                                            
142600       CONTINUE                                                           
142700     ELSE                                                                 
142800       IF MID-IDANSTNR NUMERIC                                            
142900       AND MID-IDANSTNR > ZERO                                            
143000         MOVE MFS-NUM-FAELT-RAETT     TO MOD-IDANSTNR-ATTR                
143100         MOVE MID-IDANSTNR            TO WS-IDANSTNR                      
143200       ELSE                                                               
143300         MOVE NEJ                     TO INDATA-SW                        
143400         MOVE MFS-NUM-FAELT-FEL       TO MOD-IDANSTNR-ATTR                
143500       END-IF                                                             
143600       MOVE MFS-ROER-EJ-FAELT         TO MOD-IDANSTNR                     
143700     END-IF                                                               
143800     .                                                                    
143900     SKIP2                                                                
144000 C-KONTROLL-READ-DB         SECTION.                                      
144100     MOVE 'C-KONTROLL-READ-'     TO WS-AKTUELL-SEKTION                    
144200                                                                          
144300     MOVE +1        TO RADIND                                             
144400     MOVE NEJ              TO INDATA-RAD-BEHANDLAD-SW                     
144500                                                                          
144600     PERFORM UNTIL RADIND > MAX-RADINDX                                   
144700                OR INDATA-RAD-BEHANDLAD                                   
144800                                                                          
144900       IF MID-RAD    (RADIND) NOT = ALL '+'  AND                          
145000          MID-FLAVSP (RADIND)     = '+'                                   
145100                                                                          
145200         MOVE MID-IDPRODNR (RADIND) TO W-IDPRODNR                         
145300                                       W-IDPRODNR-WDQ3D                   
145400                                                                          
145500         PERFORM CA-CHECK-IDKOLLI-KDEMBTYP                                
145600                                                                          
145700         PERFORM CB-CHECK-MID-VS-WDE6                                     
145800                                                                          
145900         IF INDATA-OK                                                     
146000           PERFORM CC-CHECK-ORDERPART-ON-WDQ3                             
146100         END-IF                                                           
146200                                                                          
146300         IF INDATA-OK                                                     
146400           PERFORM CD-CHECK-KDKOLLI-DIKOLLI                               
146500         END-IF                                                           
146600                                                                          
146700         IF INDATA-OK                                                     
146800           PERFORM CE-CHECK-WDE4-AND-VKORDBTO                             
146900         END-IF                                                           
147000                                                                          
147100                                                                          
147200         IF INDATA-OK                                                     
147300           PERFORM CF-CHECK-IDANSTNR-PRODNR                               
147400         END-IF                                                           
147500                                                                          
147600         IF INDATA-FEL                                                    
147700           MOVE RADIND       TO RADIND-NUM9                               
147800           MOVE +99          TO RADIND                                    
147900         END-IF                                                           
148000                                                                          
148100         MOVE JA             TO INDATA-RAD-BEHANDLAD-SW                   
148200       END-IF                                                             
148300       ADD +1                TO RADIND                                    
148400     END-PERFORM                                                          
148500                                                                          
148600     IF INDATA-FEL                                                        
148700       ADD +1                TO RADIND-NUM9                               
148800       MOVE RADIND-NUM9      TO RADIND                                    
148900       PERFORM MFS-ALFA-FAELT-RAETT-REST                                  
149000     END-IF                                                               
149100     .                                                                    
149200     EJECT                                                                
149300 CA-CHECK-IDKOLLI-KDEMBTYP         SECTION.                               
149400     MOVE 'CA-CHECK-IDKOLLI'     TO WS-AKTUELL-SEKTION                    
149500                                                                          
149600     IF MID-IDKOLLI (RADIND) NOT = ALL '+'                                
149700                                                                          
149800        EVALUATE TRUE                                                     
149900        WHEN DCS-SDC AND DCS-ENGLAND AND                                  
150000             MID-KDKOLLI (RADIND) = ALL '+' AND                           
150100             MID-KDEMBTYP (RADIND) = ALL '+'                              
150200*       SDC-GB, LDC-GB-3A & LDC-GB-2A FÅR KOLLIKOD 'NC' AV 4313.          
150300            CONTINUE                                                      
150400        WHEN MID-KDKOLLI (RADIND) NOT = ALL '+'                           
150500            CONTINUE                                                      
150600        WHEN MID-KDEMBTYP (RADIND) NOT = ALL '+'                          
150700                                                                          
150800           IF MID-DIKOLLIL (RADIND) = ALL '+'                             
150900              MOVE NEJ     TO INDATA-SW                                   
151000              MOVE MFS-NUM-FAELT-FEL TO                                   
151100                         MOD-DIKOLLIL-ATTR (RADIND)                       
151200           END-IF                                                         
151300                                                                          
151400           IF MID-DIKOLLIB (RADIND) = ALL '+'                             
151500              MOVE NEJ     TO INDATA-SW                                   
151600              MOVE MFS-NUM-FAELT-FEL TO                                   
151700                           MOD-DIKOLLIB-ATTR (RADIND)                     
151800           END-IF                                                         
151900                                                                          
152000           IF MID-DIKOLLIH (RADIND) = ALL '+'                             
152100              MOVE NEJ     TO INDATA-SW                                   
152200              MOVE MFS-NUM-FAELT-FEL TO                                   
152300                           MOD-DIKOLLIH-ATTR (RADIND)                     
152400           END-IF                                                         
152500                                                                          
152600           IF MID-VKORDBTO-KOLLI (RADIND) = ALL '+'                       
152700              MOVE NEJ     TO INDATA-SW                                   
152800              MOVE MFS-ADD-SET-CURSOR TO                                  
152900                           MOD-VKORDBTO-KOLLI-ATTR (RADIND)               
153000              MOVE ENTER-GROSS-WEIGHT TO MOD-TEMFSINF                     
153100           END-IF                                                         
153200                                                                          
153300        WHEN OTHER                                                        
153400           MOVE NEJ    TO INDATA-SW                                       
153500           MOVE MFS-ALFA-FAELT-FEL TO                                     
153600                              MOD-KDKOLLI-ATTR (RADIND)                   
153700        END-EVALUATE                                                      
153800     END-IF                                                               
153900     .                                                                    
154000     SKIP2                                                                
154100 CB-CHECK-MID-VS-WDE6              SECTION.                               
154200     MOVE 'CB-CHECK-MID-VS '     TO WS-AKTUELL-SEKTION                    
154300                                                                          
154400     MOVE WS-IDPRODNR (RADIND)      TO W-IDPRODNR                         
154500     MOVE WS-IDKOLLI  (RADIND)      TO W-IDKOLLI                          
154600                                                                          
154700     PERFORM IMS-GU-WDE601                                                
154800     IF SEGMENT-FINNS                                                     
154900                                                                          
155000       IF VORD-IDDC = MSGI-IDDC                                           
155900         IF VORD-FLMANORD = NEJ                                           
156000*          IF VORD-FLDIRLEV = NEJ                                         
156100              MOVE VORD-IDDISTR     TO WS-IDDISTR  (RADIND)               
156200              MOVE VORD-IDKUNDNR    TO WS-IDKUNDNR (RADIND)               
156300                                                                          
156400              IF MID-IDKOLLI (RADIND) NOT = ALL '+'                       
156500                 PERFORM IMS-GNP-WDE611                                   
156600                 IF SEGMENT-FINNS                                         
156700                    MOVE NEJ        TO INDATA-SW                          
156800                    MOVE MFS-NUM-FAELT-FEL TO                             
156900                         MOD-IDKOLLI-ATTR (RADIND)                        
157000                    MOVE INF-CASE-ALREADY-REPORT TO MED-IDMFSINF          
157100                    PERFORM S10-INF-ROUTINE                               
157200                 END-IF                                                   
157300              END-IF                                                      
157400*          ELSE                                                           
157500*             IF INDATA-OK                                                
157600*               MOVE NEJ             TO INDATA-SW                         
157700*               MOVE MFS-NUM-FAELT-FEL TO                                 
157800*                    MOD-IDPRODNR-ATTR (RADIND)                           
157900*               MOVE INF-SUPPLIER-OPART TO MED-IDMFSINF                   
158000*               PERFORM S10-INF-ROUTINE                                   
158100*             END-IF                                                      
158200*          END-IF                                                         
158300         ELSE                                                             
158400            IF INDATA-OK                                                  
158500              MOVE NEJ               TO INDATA-SW                         
158600              MOVE MFS-NUM-FAELT-FEL TO                                   
158700                   MOD-IDPRODNR-ATTR (RADIND)                             
158800              MOVE INF-WRONG-LINES   TO MED-IDMFSINF                      
158900              PERFORM S10-INF-ROUTINE                                     
159000            END-IF                                                        
159100         END-IF                                                           
159300                                                                          
159400         IF DCS-CDC                                                       
159500           IF WS-IDDISTR (RADIND) = VORD-IDDISTR                          
159600             CONTINUE                                                     
159700           ELSE                                                           
159800             IF INDATA-OK                                                 
159900               MOVE NEJ               TO INDATA-SW                        
160000               MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR (RADIND)        
160100               MOVE INF-WRONG-DISTRICT    TO MED-IDMFSINF                 
160200               PERFORM S10-INF-ROUTINE                                    
160300             END-IF                                                       
160400           END-IF                                                         
160500                                                                          
160600           MOVE VORD-IDDISTR           TO  WS-VORD-IDDISTR                
160700           MOVE MID-IDDISTR(RADIND)    TO  WS-IDDISTR-JFR                 
160800           IF WS-IDDISTR-JFR = WS-VORD-IDDISTR                            
160900             CONTINUE                                                     
161000           ELSE                                                           
161100             IF INDATA-OK                                                 
161200               MOVE NEJ               TO INDATA-SW                        
161300               MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-ATTR (RADIND)        
161400               PERFORM S10-INF-ROUTINE                                    
161500               MOVE RADIND  TO  RADIND-9                                  
161600             END-IF                                                       
161700           END-IF                                                         
161800         END-IF                                                           
161900       ELSE                                                               
162000         IF INDATA-OK                                                     
162100           MOVE NEJ                   TO INDATA-SW                        
162200           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDPRODNR-ATTR (RADIND)        
162300           MOVE  INF-ORDER-MISSING    TO MED-IDMFSINF                     
162400           PERFORM S10-INF-ROUTINE                                        
162500         END-IF                                                           
162600       END-IF                                                             
162700                                                                          
162800     ELSE                                                                 
162900        MOVE NEJ      TO INDATA-SW                                        
163000        MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)              
163100     END-IF                                                               
163200     .                                                                    
163300     SKIP2                                                                
163400 CC-CHECK-ORDERPART-ON-WDQ3    SECTION.                                   
163500     MOVE 'CC-CHECK-ORDERP '     TO WS-AKTUELL-SEKTION                    
163600                                                                          
163700      MOVE WS-IDPRODNR(RADIND)  TO W-IDPRODNR-WDQ3D                       
163800      MOVE WS-IDPLKLST(RADIND)  TO W-IDPLKLST-WDQ3D                       
163900      PERFORM IMS-GU-WDQ3D1                                               
164000                                                                          
164100      IF SEGMENT-SAKNAS                                                   
164200         MOVE NEJ               TO INDATA-SW                              
164300         MOVE MFS-NUM-FAELT-FEL TO MOD-IDPLKLST-ATTR (RADIND)             
164400         MOVE INF-ORDER-PARTS-MISSING  TO MED-IDMFSINF                    
164500         PERFORM S10-INF-ROUTINE                                          
164600      ELSE                                                                
164700         MOVE ODEL-IDORDNR7        TO WS-IDORDNR (RADIND)                 
164800         MOVE ODEL-IDUSER          TO WS-IDUSER  (RADIND)                 
164900                                                                          
165000** SOFTWARE KONTROLL                                                      
165100         IF (ODEL-IDLEVNR = '1441 ' OR                                    
165200            ODEL-IDLEVNR = 'BP2TW')  AND                                  
165300            ODEL-IDPRC = '9998'                                           
165400            MOVE NEJ          TO INDATA-SW                                
165500            MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)          
165600            MOVE INF-SUPPLIER-OPART TO MED-IDMFSINF                       
165700            PERFORM S10-INF-ROUTINE                                       
165800         ELSE                                                             
165900           IF ODEL-KDODELSTA  = 'U'    AND                                
166000              ODEL-KVPACKRAD-OD = ZERO                                    
166100              CONTINUE                                                    
166200           ELSE                                                           
166300             MOVE NEJ             TO INDATA-SW                            
166400             MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)         
166500             MOVE INF-WRONG-STATUS TO MED-IDMFSINF                        
166600             IF ODEL-KDODELSTA = 'P'    AND                               
166700                ODEL-KVPACKRAD-OD > ZERO                                  
166800               MOVE INF-ORDER-PART-READY TO MED-IDMFSINF                  
166900             END-IF                                                       
167000             PERFORM S10-INF-ROUTINE                                      
167100           END-IF                                                         
167200         END-IF                                                           
167300      END-IF                                                              
167400     .                                                                    
167500     SKIP2                                                                
167600 CD-CHECK-KDKOLLI-DIKOLLI          SECTION.                               
167700     MOVE 'CD-CHECK-KDKOLLI'     TO WS-AKTUELL-SEKTION                    
167800                                                                          
167900     IF MID-KDKOLLI (RADIND) NOT = ALL '+'                                
168000        IF DCS-SDC AND DCS-ENGLAND AND                                    
168100           MID-KDKOLLI(RADIND) = 'NC'                                     
168200          CONTINUE                                                        
168300        ELSE                                                              
168400          MOVE MID-KDKOLLI (RADIND) TO W-KDKOLLI-WDK5                     
168500          PERFORM IMS-GU-EMBB                                             
168600          IF SEGMENT-SAKNAS                                               
168700             MOVE NEJ    TO INDATA-SW                                     
168800             MOVE MFS-ALFA-FAELT-FEL TO                                   
168900                                 MOD-KDKOLLI-ATTR (RADIND)                
169000             MOVE INF-CASE-CODE-MISSING TO MED-IDMFSINF                   
169100             PERFORM S10-INF-ROUTINE                                      
169200          ELSE                                                            
169300             MOVE EMB-KDEMBTYP TO WS-KDEMBTYP (RADIND)                    
169400             MOVE EMB-VKTARA   TO WS-VKTARA   (RADIND)                    
169500             MOVE EMB-KDKOLLID TO WS-KDKOLLID (RADIND)                    
169600                                                                          
169700             IF MID-DIKOLLIL (RADIND) = ALL '+'                           
169800                IF EMB-DIKOLLIL = +0                                      
169900                  MOVE NEJ     TO INDATA-SW                               
170000                  MOVE MFS-NUM-FAELT-FEL TO                               
170100                                MOD-DIKOLLIL-ATTR (RADIND)                
170200                ELSE                                                      
170300                  MOVE EMB-DIKOLLIL     TO WS-DIKOLLIL (RADIND)           
170400                END-IF                                                    
170500             END-IF                                                       
170600                                                                          
170700             IF MID-DIKOLLIB (RADIND) = ALL '+'                           
170800                IF EMB-DIKOLLIB = +0                                      
170900                  MOVE NEJ     TO INDATA-SW                               
171000                  MOVE MFS-NUM-FAELT-FEL TO                               
171100                                MOD-DIKOLLIB-ATTR (RADIND)                
171200                ELSE                                                      
171300                  MOVE EMB-DIKOLLIB     TO WS-DIKOLLIB (RADIND)           
171400                END-IF                                                    
171500             END-IF                                                       
171600                                                                          
171700             IF MID-DIKOLLIH (RADIND) = ALL '+'                           
171800                IF EMB-DIKOLLIH = +0                                      
171900                  MOVE NEJ     TO INDATA-SW                               
172000                  MOVE MFS-NUM-FAELT-FEL TO                               
172100                                MOD-DIKOLLIH-ATTR (RADIND)                
172200                ELSE                                                      
172300                  MOVE EMB-DIKOLLIH     TO WS-DIKOLLIH (RADIND)           
172400                END-IF                                                    
172500             END-IF                                                       
172600          END-IF                                                          
172700        END-IF                                                            
172800     END-IF                                                               
172900     .                                                                    
173000     SKIP2                                                                
173100 CE-CHECK-WDE4-AND-VKORDBTO        SECTION.                               
173200     MOVE 'CE-CHECK-WDE4-AN'     TO WS-AKTUELL-SEKTION                    
173300     MOVE WS-IDDISTR  (RADIND)     TO W-401-IDDISTR                       
173400     MOVE WS-IDKUNDNR (RADIND)     TO W-401-IDKUNDNR                      
173500     MOVE WS-IDORDNR  (RADIND)     TO W-401-IDORDNR                       
173600     MOVE WS-IDPRODNR (RADIND)     TO W-401-IDPRODNR                      
173700     MOVE WS-IDPLKLST (RADIND)     TO W-401-IDPLKLST                      
173800     MOVE ZERO                  TO WS-KORD-VKORDNTO    (RADIND)           
173900                                   WS-ORAD-VKARTNTO-KG (RADIND)           
174000                                   WS-VKORDBTO-TOT                        
174100     PERFORM IMS-GU-WDE401                                                
174200     IF SEGMENT-FINNS                                                     
174300       IF KORD-KVORDRAD-PACK = ZERO                                       
174400         MOVE KORD-VKORDNTO     TO WS-KORD-VKORDNTO     (RADIND)          
174500         MOVE KORD-IDORDER      TO WS-IDORDER           (RADIND)          
174600         PERFORM IMS-GNP-WDE4                                             
174700         IF SEGMENT-FINNS                                                 
174800            MOVE 1 TO LINE-IX                                             
174900            PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                    
175000                                                                          
175100               PERFORM CEA-KONTROLLERA-RADER                              
175200               PERFORM IMS-GNP-WDE411                                     
175300            END-PERFORM                                                   
175400            PERFORM CEB-COMP-VIKT-BRUTTO                                  
175500         ELSE                                                             
175600            MOVE NEJ             TO INDATA-SW                             
175700            MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)          
175800            MOVE INF-ITEMS-MISSING   TO MED-IDMFSINF                      
175900            PERFORM S10-INF-ROUTINE                                       
176000         END-IF                                                           
176100       ELSE                                                               
176200          MOVE NEJ               TO INDATA-SW                             
176300          MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)            
176400          MOVE INF-LINE-ALREADY-REPORT   TO MED-IDMFSINF                  
176500          PERFORM S10-INF-ROUTINE                                         
176600       END-IF                                                             
176700     ELSE                                                                 
176800         MOVE NEJ              TO INDATA-SW                               
176900         MOVE MFS-NUM-FAELT-FEL TO MOD-IDPLKLST-ATTR (RADIND)             
177000         MOVE INF-ORDER-PARTS-MISSING TO MED-IDMFSINF                     
177100         PERFORM S10-INF-ROUTINE                                          
177200     END-IF                                                               
177300     .                                                                    
177400     SKIP2                                                                
177500 CEA-KONTROLLERA-RADER         SECTION.                                   
177600     MOVE 'CEA-KONTROLLERA '     TO WS-AKTUELL-SEKTION                    
177700                                                                          
177800     MOVE ZERO                   TO WS-VKORDBTO-TOT                       
177900                                                                          
178000     IF LINE-IX <= MAX-LINES                                              
178100       MOVE ORAD-IDARTNR         TO W-IDARTNR                             
178200                                                                          
178300       IF CDC                                                             
178400         PERFORM IMS-GU-WDK611                                            
178500         MOVE CLAG-ADGANG        TO LINE1-ADGANG  (LINE-IX)               
178600         MOVE CLAG-ADPLATS       TO LINE1-ADPLATS (LINE-IX)               
178700         MOVE CLAG-ADLAGOMR      TO LINE1-ADLAGOMR(LINE-IX)               
178800       ELSE                                                               
178900         PERFORM IMS-GU-WDK711                                            
179000         MOVE SLAG-ADGANG        TO LINE1-ADGANG  (LINE-IX)               
179100         MOVE SLAG-ADPLATS       TO LINE1-ADPLATS (LINE-IX)               
179200         MOVE SLAG-ADLAGOMR      TO LINE1-ADLAGOMR(LINE-IX)               
179300       END-IF                                                             
179400                                                                          
179500       MOVE LINE-IX              TO LINE1-NUMBER  (LINE-IX)               
179600                                    MAX-TAB                               
179700       MOVE ORAD-IDARTNR         TO LINE1-IDARTNR (LINE-IX)               
179800       MOVE ORAD-KVAVBART        TO LINE1-KVAVBART(LINE-IX)               
179900       MOVE ORAD-VKARTNTO        TO LINE1-VKOLDNET(LINE-IX)               
180000       MOVE ORAD-VKART-NTO-KG    TO LINE1-VKNEWNET(LINE-IX)               
180100       ADD 1 TO LINE-IX                                                   
180200       MOVE LINE-IX              TO MAX-RAD                               
180300     END-IF                                                               
180400                                                                          
180500     COMPUTE WS-ORAD-VKARTNTO-KG (RADIND) = (ORAD-VKART-NTO-KG            
180600             * ORAD-KVAVBART) +  WS-ORAD-VKARTNTO-KG (RADIND)             
180700     IF ORAD-IDLEVNR NOT = SPACE                                          
180800       MOVE JA                  TO DIRLEV-KOLLI-SW                        
180900     END-IF                                                               
181000*                                                                         
181100     IF ORAD-KDRADSTA > 3                                                 
181200        MOVE NEJ               TO INDATA-SW                               
181300        MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)              
181400        MOVE INF-ORDER-REGISTRED      TO MED-IDMFSINF                     
181500        PERFORM S10-INF-ROUTINE                                           
181600     ELSE                                                                 
181700       IF ORAD-KVLEVART > ZERO                                            
181800          MOVE NEJ             TO INDATA-SW                               
181900          MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)            
182000          MOVE INF-CASE-REPORT-STARTED TO MED-IDMFSINF                    
182100          PERFORM S10-INF-ROUTINE                                         
182200       ELSE                                                               
182300         IF ORAD-FLNOLLJ = JA                                             
182400            MOVE NEJ             TO INDATA-SW                             
182500            MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)          
182600            MOVE INF-LINE-ALREADY-ZEROED  TO MED-IDMFSINF                 
182700            PERFORM S10-INF-ROUTINE                                       
182800         END-IF                                                           
182900       END-IF                                                             
183000     END-IF                                                               
183100     .                                                                    
183200     SKIP2                                                                
183300 CEB-COMP-VIKT-BRUTTO          SECTION.                                   
183400     MOVE 'CEB-COMP-VIKT-B '     TO WS-AKTUELL-SEKTION                    
183500                                                                          
183600     IF MID-VKORDBTO-KOLLI (RADIND) = ALL '+'                             
183700         COMPUTE WS-VKORDBTO (RADIND) =                                   
183800              WS-KORD-VKORDNTO (RADIND) + WS-VKTARA (RADIND)              
183900         END-COMPUTE                                                      
184000                                                                          
184100         IF WS-VKORDBTO (RADIND) > ZERO                                   
184200           MOVE WS-VKORDBTO(RADIND) TO WS-MOD-VKORDBTO                    
184300           IF US-MEASUREMENT                                              
184400              COMPUTE WS-MOD-VKORDBTO ROUNDED =                           
184500                      WS-VKORDBTO (RADIND) * CONV-LB-TO-KG                
184600              END-COMPUTE                                                 
184700           END-IF                                                         
184800           MOVE WS-MOD-VKORDBTO      TO                                   
184900                MOD-VKORDBTO-KOLLI (RADIND)                               
185000         ELSE                                                             
185100           MOVE '1'                TO WS-VKORDBTO-HK                      
185200           MOVE WS-VKORDBTO-RED    TO WS-VKORDBTO        (RADIND)         
185300                                      WS-MOD-VKORDBTO                     
185400           IF US-MEASUREMENT                                              
185500              COMPUTE WS-MOD-VKORDBTO ROUNDED =                           
185600                      WS-VKORDBTO (RADIND) * CONV-LB-TO-KG                
185700              END-COMPUTE                                                 
185800           END-IF                                                         
185900           MOVE WS-MOD-VKORDBTO      TO                                   
186000                MOD-VKORDBTO-KOLLI (RADIND)                               
186100         END-IF                                                           
186200         MOVE MFS-NUM-FAELT-RAETT     TO                                  
186300              MOD-VKORDBTO-KOLLI-ATTR (RADIND)                            
186400     ELSE                                                                 
186500*LK GROSS WT MUST BE LESS THAN (PART WT * QUANTITY).                      
186600*LK                            CASE WT IS NOT CONSIDERED                  
186700       IF INDATA-OK                                                       
186800         IF WS-VKORDBTO (RADIND) < WS-ORAD-VKARTNTO-KG (RADIND)           
186900           MOVE RADIND                   TO ERR-LINE                      
187000           MOVE NEJ                      TO INDATA-SW                     
187100           MOVE INF-WEIGHT-NOT-LESS-THAN TO MOD-TEMFSINF                  
187200           COMPUTE WS-VKORDBTO-TOT ROUNDED =                              
187300                               WS-ORAD-VKARTNTO-KG (RADIND)               
187400           END-COMPUTE                                                    
187500           MOVE WS-VKORDBTO-TOT          TO HEAD-VKARTNTO                 
187600                                                                          
187700           IF US-MEASUREMENT                                              
187800             COMPUTE WS-VKORDBTO-TOT ROUNDED =                            
187900                 CONV-KG-TO-LB * WS-ORAD-VKARTNTO-KG (RADIND)             
188000             END-COMPUTE                                                  
188100             MOVE 'LBS'                 TO MOD-TEMFSINF(41:3)             
188200           ELSE                                                           
188300             MOVE 'KG'                  TO MOD-TEMFSINF(41:2)             
188400           END-IF                                                         
188500                                                                          
188600           INSPECT WS-VKORDBTO-TOT REPLACING LEADING ZERO BY SPACE        
188700           MOVE WS-VKORDBTO-TOT         TO MOD-TEMFSINF(29:10)            
188800           MOVE 'FOR LINE-'             TO MOD-TEMFSINF(45:9)             
188900           MOVE ERR-LINE                TO MOD-TEMFSINF(54:2)             
189000                                                                          
189100           MOVE +1               TO RADIX                                 
189200           PERFORM UNTIL RADIX > MAX-RADINDX                              
189300             IF MID-RAD (RADIX) NOT = ALL '+'                             
189400               PERFORM MFS-ADD-LAES-IN-FAELT-RAD                          
189500             END-IF                                                       
189600             ADD +1               TO RADIX                                
189700           END-PERFORM                                                    
189800                                                                          
189900           MOVE MFS-NUM-FAELT-FEL       TO                                
190000                MOD-VKORDBTO-KOLLI-ATTR (RADIND)                          
190100                                                                          
190200           PERFORM S21-SEND-OPEN                                          
190300           PERFORM S22-PUT-HEADER                                         
190400           PERFORM S23-MOVE-LINEDATA                                      
190500           PERFORM S25-SEND-CLOSE                                         
190600         ELSE                                                             
190700             MOVE MFS-NUM-FAELT-RAETT TO                                  
190800                  MOD-VKORDBTO-KOLLI-ATTR (RADIND)                        
190900         END-IF                                                           
191000       END-IF                                                             
191100     END-IF                                                               
191200     .                                                                    
191300     EJECT                                                                
191400 CF-CHECK-IDANSTNR-PRODNR          SECTION.                               
191500     MOVE 'CF-CHECK-IDANST '     TO WS-AKTUELL-SEKTION                    
191600                                                                          
191700     MOVE NEJ                     TO WS-TRAEFF-PACKARE                    
191800     MOVE JA                      TO WS-PACKARES-ODEL-REDAN-KLARA         
191900*                                                                         
192000                                                                          
192100     MOVE WS-IDDISTR  (RADIND)    TO W-4A1-IDDISTR                        
192200     MOVE WS-IDKUNDNR (RADIND)    TO W-4A1-IDKUNDNR                       
192300     MOVE WS-IDORDNR  (RADIND)    TO W-4A1-IDORDNR                        
192400                                                                          
192500     PERFORM IMS-GU-KUNDORDER-SEK                                         
192600*                                                                         
192700     IF KUNDORDER-SEK-FINNS                                               
192800                                                                          
192900        PERFORM UNTIL KUNDORDER-SEK-SAKNAS                                
193000                                                                          
193100          MOVE KORD-IDDISTR             TO W-401-IDDISTR                  
193200          MOVE KORD-IDKUNDNR            TO W-401-IDKUNDNR                 
193300          MOVE KORD-IDORDNR5            TO W-401-IDORDNR                  
193400          MOVE KORD-IDPRODNR            TO W-401-IDPRODNR                 
193500          MOVE KORD-IDPLKLST            TO W-401-IDPLKLST                 
193600                                                                          
193700          PERFORM IMS-GU-WDE401                                           
193800          MOVE KORD-IDPRODNR         TO   WS-JFR-IDPRODNR                 
193900          MOVE KORD-IDUSER           TO   WS-JFR-IDANSTNR                 
194000                                                                          
194100                                                                          
194200          IF WS-IDPRODNR (RADIND) = WS-JFR-IDPRODNR AND                   
194300             WS-IDANSTNR-TAB (RADIND) = WS-JFR-IDANSTNR-5 AND             
194400             WS-IDANSTNR              = WS-JFR-IDANSTNR-5                 
194500             MOVE 'J'                TO   WS-TRAEFF-PACKARE               
194600             PERFORM CFA-KONTROLLERA-PLOCKLISTA                           
194700          END-IF                                                          
194800          PERFORM IMS-GN-SEQA-WDE4A1                                      
194900        END-PERFORM                                                       
195000     END-IF                                                               
195100*                                                                         
195200     IF WS-TRAEFF-PACKARE  = 'N'                                          
195300*----------------------------------------------SAKNAS ANGIVEN             
195400*----------------------------------------------PACKARE PÅ ORDERN          
195500        MOVE NEJ                        TO   INDATA-SW                    
195600        MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)              
195700        MOVE INF-WRONG-PICKER           TO   MED-IDMFSINF                 
195800        PERFORM S10-INF-ROUTINE                                           
195900*----------------------------------------------ÄR ANGIVEN PACKARES        
196000*----------------------------------------------ORDERDEL REDAN KLAW        
196100     ELSE                                                                 
196200       IF INDATA-OK   AND                                                 
196300          WS-PACKARES-ODEL-REDAN-KLARA = JA                               
196400          MOVE NEJ               TO INDATA-SW                             
196500          MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)            
196600          MOVE INF-PICKERS-ORDERPART-FINISHED                             
196700                                 TO   MED-IDMFSINF                        
196800          PERFORM S10-INF-ROUTINE                                         
196900       END-IF                                                             
197000     END-IF                                                               
197100     .                                                                    
197200     EJECT                                                                
197300 CFA-KONTROLLERA-PLOCKLISTA               SECTION.                        
197400     MOVE 'CFA-KONTROLLERA '     TO WS-AKTUELL-SEKTION                    
197500                                                                          
197600     IF KORD-KDPAKOLL NOT = ZERO                                          
197700*-----------------------------------------FÅR MAN EJ RAPPORTERA DÅ        
197800*-----------------------------------------AVVIKELSEKONTROLL PÅGÅR         
197900        MOVE NEJ                 TO INDATA-SW                             
198000        MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)              
198100        MOVE INF-DEVIATION-CONTR-IN-PROGR                                 
198200                               TO     MED-IDMFSINF                        
198300        PERFORM S10-INF-ROUTINE                                           
198400     ELSE                                                                 
198500                                                                          
198600                                                                          
198700       IF ( (KORD-KVORDRAD-PACK NOT = KORD-KVORDRAD)       OR             
198800            (KORD-KVORDRAD-LEVPL    > 0                    AND            
198900             KORD-KVORDRAD-PACK NOT = KORD-KVORDRAD-LEVPL) )              
199000         MOVE NEJ             TO WS-PACKARES-ODEL-REDAN-KLARA             
199100                                                                          
199200       END-IF                                                             
199300     END-IF                                                               
199400     .                                                                    
199500                                                                          
199600     SKIP2                                                                
199700 D-UPDATE                      SECTION.                                   
199800     MOVE 'D-UPDATE        '     TO WS-AKTUELL-SEKTION                    
199900                                                                          
200000     MOVE +1          TO RADIND                                           
200100     MOVE NEJ                 TO INDATA-RAD-BEHANDLAD-SW                  
200200                                                                          
200300     PERFORM UNTIL RADIND > MAX-RADINDX                                   
200400                OR INDATA-RAD-BEHANDLAD                                   
200500                                                                          
200600       IF MID-RAD    (RADIND) NOT = ALL '+' AND                           
200700          MID-FLAVSP (RADIND)     = '+'                                   
200800           PERFORM DA-INIT-AVSP-CALL                                      
200900           PERFORM DB-AVSP-CALL                                           
201000           IF AVSP-KDSVAR = SPACE                                         
201100              PERFORM S03-RENSA-FAELT                                     
201200              PERFORM S15-PLUS-TO-MID-RAD                                 
201300              MOVE JA             TO ORDERDEL-PACKAD-SW                   
201400                                                                          
201500              IF WS-PRT-KDSVAR-ADRESSFL = RAETT                           
201600                PERFORM S04-EV-SEND-CASELABEL                             
201700              END-IF                                                      
201800*             IF CDC OR NDC-JP OR NDC-AU OR LDC                           
201900              IF DCS-FLFSEDEL = JA                                        
202000                IF WS-PRT-KDSVAR-FOLJES = RAETT                           
202100                  PERFORM S12-SEND-DEL-NOTE                               
202200                END-IF                                                    
202300              END-IF                                                      
202400              MOVE WS-IDDISTR (RADIND) TO DIST08-IDDISTR                  
202500              MOVE WS-IDKUNDNR(RADIND) TO TRANSFER-KUND                   
202600             IF (DCS-CDC AND (DIST08-URSP-RAPP-CDC                        
202700                           OR DIST08-URSP-RAPP                            
202800                           OR DIST08-URSP-SPX))                           
202900               OR                                                         
203000                 ((DCS-NDC-NA OR DCS-NDC-PF) AND                          
203100                 (TRANSFER-KUNDNR OR RETUR-KUNDNR)                        
203200                 AND DIST08-URSP-TRANSFER-NDC)                            
203300               OR                                                         
203400                 (DCS-NDC-NA AND DCS-CANADA AND                           
203500                  DIST08-URSP-RAPP-CDC)                                   
203600                PERFORM DF-VISA-BILD-4317                                 
203700              ELSE                                                        
203800                PERFORM DD-RESTART-W40313                                 
203900              END-IF                                                      
204000           ELSE                                                           
204100              PERFORM DC-ERROR-MESSAGE-TO-MOD                             
204200              MOVE NEJ             TO ORDERDEL-PACKAD-SW                  
204300           END-IF                                                         
204400           MOVE JA                 TO INDATA-RAD-BEHANDLAD-SW             
204500       END-IF                                                             
204600       ADD +1      TO RADIND                                              
204700     END-PERFORM                                                          
204800                                                                          
204900     IF 4317-INTE-STARTAD                                                 
205000       PERFORM DE-CHECK-IF-INDATA-EXISTS                                  
205100                                                                          
205200       IF ORDERDEL-PACKAD AND INDATA-SAKNAS                               
205300          IF INDATA-OK                                                    
205400            PERFORM MFS-RENSA-FAELT-RAD                                   
205500            MOVE MFS-RENSA-FAELT  TO   MOD-IDANSTNR                       
205600            PERFORM S01-FORMATETS-ATTR                                    
205700            IF DCS-SDC OR DCS-NDC-NA OR DCS-NDC-PF                        
205800               PERFORM S06-EV-STAENG-FAELT                                
205900            END-IF                                                        
206000            MOVE INF-UPDATED TO MED-IDMFSINF                              
206100          END-IF                                                          
206200          PERFORM S10-INF-ROUTINE                                         
206300       END-IF                                                             
206400       IF INDATA-FEL                                                      
206500         PERFORM S10-INF-ROUTINE                                          
206600       END-IF                                                             
206700     END-IF                                                               
206800     .                                                                    
206900     SKIP2                                                                
207000 DF-VISA-BILD-4317           SECTION.                                     
207100     MOVE 'DF-VISA-BILD-43 '     TO WS-AKTUELL-SEKTION                    
207200                                                                          
207300     MOVE MID-W4I31301          TO USER-MID-W4I31301                      
207400     MOVE SPAR-AREA             TO MSGI-SPAR-AREA                         
207500     MOVE '002'                 TO MSGI-KDCALL                            
207600     MOVE '4313'                TO MSGI-IDTRANS                           
207700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
207800                                                                          
207900     PERFORM DFA-LADDA-4317                                               
208000     MOVE JA                    TO SW-STARTA-4317                         
208100     IF ENGLISH-TEXT                                                      
208200       MOVE 'W4O317N1'          TO MFS-IDMOD                              
208300       MOVE WS-KDMFSFOR         TO MFS-KDMFSFOR                           
208400     ELSE                                                                 
208500       MOVE 'W4O31701'          TO MFS-IDMOD                              
208600       MOVE WS-KDMFSFOR         TO MFS-KDMFSFOR                           
208700     END-IF                                                               
208800     COMPUTE MSG-KVLL = LENGTH OF 4317-MOD-W4O31701 + 4                   
208900     END-COMPUTE                                                          
209000     PERFORM IMS-INSERT-MSG                                               
209100     .                                                                    
209200     SKIP2                                                                
209300 DFA-LADDA-4317        SECTION.                                           
209400     MOVE 'DFA-LADDA-4317  '     TO WS-AKTUELL-SEKTION                    
209500                                                                          
209600     MOVE LOW-VALUE                TO 4317-MOD-W4O31701                   
209700     MOVE '4317'                   TO 4317-MOD-IDTRANS                    
209800     MOVE MFS-RENSA-FAELT          TO 4317-MOD-TEMFSFEL                   
209900                                      4317-MOD-IDANSTNR-IN                
210000                                      4317-MOD-IDDISTR-IN                 
210100                                      4317-MOD-IDKUNDNR-IN                
210200                                      4317-MOD-IDORDNR-IN                 
210300                                      4317-MOD-IDKOLLI-IN                 
210400                                      4317-MOD-IDPRODNR-IN                
210500                                      4317-MOD-IDDC-IN                    
210600                                      4317-MOD-FLSISTAK                   
210700                                      4317-MOD-IDRADNR-S                  
210800                                      4317-MOD-KDARTURS-S                 
210900     MOVE WS-IDANSTNR-TAB (RADIND) TO 4317-MOD-IDANSTNR-UT                
211000     INSPECT 4317-MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE         
211100     MOVE WS-IDDISTR  (RADIND)     TO WS-IDDISTR-NUM4                     
211200     MOVE WS-IDDISTR-NUM4          TO 4317-MOD-IDDISTR-UT                 
211300     INSPECT 4317-MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE         
211400     MOVE WS-IDKUNDNR (RADIND)     TO WS-IDKUNDNR-NUM6                    
211500     MOVE WS-IDKUNDNR-NUM6         TO 4317-MOD-IDKUNDNR-UT                
211600     INSPECT 4317-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
211700     MOVE WS-IDORDNR  (RADIND)     TO 4317-MOD-IDORDNR-UT                 
211800     INSPECT 4317-MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE         
211900     MOVE MSGI-IDDC                TO 4317-MOD-IDDC-UT                    
212000     MOVE WS-IDKOLLI  (RADIND)     TO 4317-MOD-IDKOLLI-UT                 
212100     INSPECT 4317-MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE         
212200     MOVE WS-IDPRODNR (RADIND)     TO 4317-MOD-IDPRODNR-UT                
212300     INSPECT 4317-MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE         
212400     MOVE '4313'                   TO 4317-MOD-IDTRANS-START              
212500     MOVE MOD-TEMFSINF             TO 4317-MOD-TEMFSINF                   
212600     MOVE NEJ                      TO 4317-MOD-FLSISTAK                   
212700                                                                          
212800     MOVE +1                       TO INDX                                
212900     PERFORM UNTIL INDX NOT < 14                                          
213000         MOVE MFS-RENSA-FAELT      TO 4317-MOD-IDRADNR (INDX)             
213100                                      4317-MOD-KDARTURS (INDX)            
213200         ADD +1 TO INDX                                                   
213300     END-PERFORM                                                          
213400     MOVE MFS-ADD-SAETT-CURSOR     TO 4317-MOD-IDRADNR-ATTR (1)           
213500     .                                                                    
213600     EJECT                                                                
213700 DA-INIT-AVSP-CALL           SECTION.                                     
213800     MOVE 'DA-INIT-AVSP-   '     TO WS-AKTUELL-SEKTION                    
213900                                                                          
214000     MOVE MSGI-IDDC                    TO   AVSP-IDDC                     
214100     MOVE WS-IDANSTNR-TAB  (RADIND)    TO   AVSP-IDANSTNR                 
214200                                                                          
214300     MOVE 4313                         TO   AVSP-IDTRANS                  
214400     MOVE ZERO                         TO   AVSP-IDKOLLI-SAMP             
214500     MOVE WS-IDPRODNR      (RADIND)    TO   AVSP-IDPRODNR                 
214600     MOVE WS-IDDISTR       (RADIND)    TO   AVSP-IDDISTR                  
214700     MOVE WS-IDKUNDNR      (RADIND)    TO   AVSP-IDKUNDNR                 
214800     MOVE WS-IDORDNR       (RADIND)    TO   AVSP-IDORDNR                  
214900     MOVE WS-IDPLKLST      (RADIND)    TO   AVSP-IDPLKLST                 
215000     MOVE WS-IDKOLLI       (RADIND)    TO   AVSP-IDKOLLI                  
215100     MOVE WS-KDKOLLI       (RADIND)    TO   AVSP-KDKOLLI                  
215200     MOVE WS-KDEMBTYP      (RADIND)    TO   AVSP-KDEMBTYP                 
215300     MOVE WS-IDORDER       (RADIND)    TO   AVSP-IDORDER                  
215400     MOVE WS-VKORDBTO      (RADIND)    TO   AVSP-VKORDBTO                 
215500     MOVE WS-DIKOLLIL      (RADIND)    TO   AVSP-DIKOLLIL                 
215600     MOVE WS-DIKOLLIB      (RADIND)    TO   AVSP-DIKOLLIB                 
215700     MOVE WS-DIKOLLIH      (RADIND)    TO   AVSP-DIKOLLIH                 
215800     MOVE WS-KDKOLLID      (RADIND)    TO   AVSP-KDKOLLID                 
215900     MOVE WS-KORD-VKORDNTO (RADIND)    TO   AVSP-VKORDNTO                 
216000     MOVE WS-VKTARA        (RADIND)    TO   AVSP-VKTARA                   
216100     .                                                                    
216200     SKIP2                                                                
216300 DB-AVSP-CALL          SECTION.                                           
216400     MOVE 'DB-AVSP-CALL    '     TO WS-AKTUELL-SEKTION                    
216500                                                                          
216600     CALL W403AVSP USING AVSP-W403AVSP                                    
216700     AVSP-4342-PCB  AVSP-2191-PCB AVSP-4349-PCB                           
216800     TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                                     
216900     AVSP-USEA-PCB                                                        
217000     AVSP-WDE41-PCB AVSP-WDE42-PCB AVSP-WDE4-PCB AVSP-WDE4B-PCB           
217100     AVSP-WDE4E-PCB AVSP-WDE6-PCB  AVSP-XXDV-PCB  AVSP-ORQA-PCB           
217200     AVSP-XXKW-PCB  AVSP-XXLB-PCB  AVSP-XXJK-PCB AVSP-ZZAC-PCB            
217300     AVSP-ORQI-PCB  AVSP-ORQL-PCB  AVSP-WDE6E-PCB AVSP-WDE62-PCB          
217400     AVSP-ORQICSQ-PCB              AVSP-ORQM-PCB                          
217500     AVSP-WDE4A-PCB AVSP-WDM2-PCB  AVSP-ORQA2-PCB                         
217600     AVSP-ORDP1-PCB AVSP-ORDP2-PCB AVSP-XXJN-PCB AVSP-XXKH-PCB            
217700     AVSP-ARTC-PCB  AVSP-WDK7-PCB  AVSP-ARTM-PCB AVSP-AUTF-PCB            
217800     AVSP-4487-PCB  AVSP-4541-PCB AVSP-LOGA-PCB                           
217900     AVSP-WDK6-PCB  AVSP-WDA6B-PCB AVSP-WDA6-PCB AVSP-WDP4A-PCB           
218000     AVSP-WDB6-PCB                                                        
218100     AVSP-PLATS-XXDM-PCB AVSP-PLATS-XXDN-PCB AVSP-PLATS-XXDP-PCB          
218200     AVSP-PLATS-XXDO-PCB AVSP-PLATS-WDE6C-PCB AVSP-PLATS-GMTC-PCB         
218300     AVSP-PLATS-WDB6-PCB                                                  
218400     AVSP-DNOT-ORQP-PCB                                                   
218500     AVSP-DNOT-ORQP2-PCB                                                  
218600     AVSP-DNOT-ORQP3-PCB                                                  
218700     AVSP-DNOT-4013-PCB                                                   
218800     AVSP-DNOT-BENA-PCB                                                   
218900     AVSP-PRQU-WDG2-PCB                                                   
219000     AVSP-PRQU-WDC7-PCB                                                   
219100     AVSP-PRQU-SJKO-WDK6-PCB                                              
219200     AVSP-PRNO-3107-PCB                                                   
219300     AVSP-TMS-1165-PCB                                                    
219400     AVSP-TMS-4141-PCB                                                    
219500     AVSP-TMS-WDB2-PCB                                                    
219510     AVSP-TMS-WDB6-PCB                                                    
219520     AVSP-TMS-WDD3-PCB                                                    
219530     AVSP-TMS-WDB1-PCB                                                    
219540     AVSP-TMS-WDE4A-PCB                                                   
219550     AVSP-TMS-WDE4F-PCB                                                   
219560     AVSP-TMS-WDQ2-PCB                                                    
219570     AVSP-TMS-WDQ3-PCB                                                    
219580     AVSP-TMS-WDK6-PCB                                                    
219590     AVSP-TMS-WDE6-PCB                                                    
219591     AVSP-TMS-WDK5-PCB                                                    
219592     AVSP-TMS-WDQ2C-PCB                                                   
219600     .                                                                    
219700     SKIP2                                                                
219800                                                                          
219900 DC-ERROR-MESSAGE-TO-MOD      SECTION.                                    
220000     MOVE 'DC-ERROR-MESSA  '     TO WS-AKTUELL-SEKTION                    
220100                                                                          
220200     EVALUATE AVSP-ERROR-MESSAGE                                          
220300        WHEN 730                                                          
220400             MOVE INF-ERROR-IN-ADDRESS   TO MED-IDMFSINF                  
220500        WHEN OTHER                                                        
220600             MOVE INF-ORDER-NOT-PACKED   TO MED-IDMFSINF                  
220700     END-EVALUATE                                                         
220800                                                                          
220900     MOVE NEJ              TO INDATA-SW                                   
221000     MOVE MFS-NUM-FAELT-FEL TO MOD-IDPRODNR-ATTR (RADIND)                 
221100     MOVE FEL-FR-W403AVSP   TO MID-FLAVSP (RADIND)                        
221200                                                                          
221300     PERFORM UNTIL RADIND > MAX-RADINDX                                   
221400       PERFORM MFS-ROER-EJ-FAELT-TO-MOD-RAD                               
221500       ADD +1                     TO RADIND                               
221600     END-PERFORM                                                          
221700                                                                          
221800     MOVE +1                     TO RADIX                                 
221900     PERFORM UNTIL RADIX > MAX-RADINDX                                    
222000       IF MID-RAD   (RADIX) NOT = ALL '+'                                 
222100         PERFORM MFS-ADD-LAES-IN-FAELT-RAD                                
222200       END-IF                                                             
222300       ADD +1                     TO RADIX                                
222400     END-PERFORM                                                          
222500     .                                                                    
222600     SKIP2                                                                
222700 DD-RESTART-W40313 SECTION.                                               
222800*EN OMSTART AV 4313 FÖR VARJE ORDER-DEL.                                  
222900     MOVE 'DD-RESTART-W403 '     TO WS-AKTUELL-SEKTION                    
223000                                                                          
223100     MOVE MSGI-IDDC              TO 4313UT-MID-IDDC-UT                    
223200     MOVE WS-MID-FOLJES          TO 4313UT-MID-KDPRTVAL-FS                
223300     MOVE WS-MID-ADRESSFL        TO 4313UT-MID-KDPRTVAL-ADR               
223400     MOVE MID-IDANSTNR           TO 4313UT-MID-IDANSTNR                   
223500                                                                          
223600     MOVE JA                     TO OMSTART-SW                            
223700     MOVE NEJ                    TO INDATA-FINNS-SW                       
223800     MOVE +1                     TO RADIX                                 
223900                                                                          
224000     PERFORM UNTIL RADIX > MAX-RADINDX                                    
224100       MOVE MID-IDDISTR  (RADIX) TO 4313UT-MID-IDDISTR  (RADIX)           
224200       MOVE MID-IDPRODNR (RADIX) TO 4313UT-MID-IDPRODNR (RADIX)           
224300       MOVE MID-IDPLKLST (RADIX) TO 4313UT-MID-IDPLKLST (RADIX)           
224400       MOVE MID-IDKOLLI  (RADIX) TO 4313UT-MID-IDKOLLI  (RADIX)           
224500       MOVE MID-KDKOLLI  (RADIX) TO 4313UT-MID-KDKOLLI  (RADIX)           
224600       MOVE MID-KDEMBTYP (RADIX) TO 4313UT-MID-KDEMBTYP (RADIX)           
224700       MOVE MID-DIKOLLIL (RADIX) TO 4313UT-MID-DIKOLLIL (RADIX)           
224800       MOVE MID-DIKOLLIB (RADIX) TO 4313UT-MID-DIKOLLIB (RADIX)           
224900       MOVE MID-DIKOLLIH (RADIX) TO 4313UT-MID-DIKOLLIH (RADIX)           
225000       MOVE MID-VKORDBTO-KOLLI        (RADIX) TO                          
225100            4313UT-MID-VKORDBTO-KOLLI (RADIX)                             
225200       MOVE MID-KDPRTVAL-ADR-RAD      (RADIX) TO                          
225300            4313UT-MID-KDPRTVAL-ADR-RAD (RADIX)                           
225400       MOVE MID-KDPRTVAL-FS-RAD       (RADIX) TO                          
225500            4313UT-MID-KDPRTVAL-FS-RAD (RADIX)                            
225600       MOVE MID-FLAVSP (RADIX) TO 4313UT-MID-FLAVSP (RADIX)               
225700                                                                          
225800       IF MID-RAD   (RADIX) NOT = ALL '+' AND                             
225900          MID-FLAVSP(RADIX)     = '+'                                     
226000         MOVE JA                  TO INDATA-FINNS-SW                      
226100       END-IF                                                             
226200       ADD +1                     TO RADIX                                
226300     END-PERFORM                                                          
226400                                                                          
226500     IF INDATA-FINNS                                                      
226600       MOVE MFS-KDMFSFOR TO 4313UT-KDMFSFOR                               
226700       COMPUTE 4313UT-KVLL = LENGTH OF 4313UT-MID-W4I31301 + 17           
226800*STARTA OM 4313.                                                          
226900       PERFORM IMS-PURGE-ALT4313-MSG                                      
227000     END-IF                                                               
227100     .                                                                    
227200     SKIP2                                                                
227300 DE-CHECK-IF-INDATA-EXISTS   SECTION.                                     
227400     MOVE 'DE-CHECK-IF-INDA'     TO WS-AKTUELL-SEKTION                    
227500                                                                          
227600     MOVE NEJ                     TO INDATA-FINNS-SW                      
227700     MOVE +1                      TO RADIX                                
227800     PERFORM UNTIL RADIX > MAX-RADINDX                                    
227900       IF MID-FLAVSP(RADIX) = FEL-FR-W403AVSP                             
228000         MOVE NEJ                 TO INDATA-SW                            
228100         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPRODNR-ATTR (RADIX)            
228200         PERFORM MFS-ROER-EJ-FAELT-TO-MOD-RADIX                           
228300       END-IF                                                             
228400                                                                          
228500       IF MID-RAD   (RADIX) NOT = ALL '+' AND                             
228600          MID-FLAVSP(RADIX)     = '+'                                     
228700         MOVE JA                  TO INDATA-FINNS-SW                      
228800       END-IF                                                             
228900       ADD +1                     TO RADIX                                
229000     END-PERFORM                                                          
229100                                                                          
229200     IF INDATA-SAKNAS                                                     
229300       MOVE NEJ                   TO OMSTART-SW                           
229400     END-IF                                                               
229500     .                                                                    
229600     SKIP2                                                                
229700 S01-FORMATETS-ATTR SECTION.                                              
229800     MOVE 'S01-FORMATETS-AT'     TO WS-AKTUELL-SEKTION                    
229900                                                                          
230000     MOVE +1                TO RADIND                                     
230100                                                                          
230200     PERFORM UNTIL RADIND > MAX-RADINDX                                   
230300                                                                          
230400       MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-ATTR  (RADIND)              
230500                                  MOD-IDPRODNR-ATTR (RADIND)              
230600                                  MOD-IDKOLLI-ATTR  (RADIND)              
230700                                  MOD-KDKOLLI-ATTR  (RADIND)              
230800                                  MOD-VKORDBTO-KOLLI-ATTR (RADIND)        
230900                                  MOD-KDEMBTYP-ATTR (RADIND)              
231000                                  MOD-DIKOLLIL-ATTR (RADIND)              
231100                                  MOD-DIKOLLIB-ATTR (RADIND)              
231200                                  MOD-DIKOLLIH-ATTR (RADIND)              
231300                              MOD-KDPRTVAL-ADR-RAD-ATTR (RADIND)          
231400                              MOD-KDPRTVAL-FS-RAD-ATTR (RADIND)           
231500       ADD +1               TO RADIND                                     
231600     END-PERFORM                                                          
231700                                                                          
231800     .                                                                    
231900     EJECT                                                                
232000 S02-ROER-EJ-FAELT SECTION.                                               
232100     MOVE 'S02-ROER-EJ-    '     TO WS-AKTUELL-SEKTION                    
232200                                                                          
232300     IF MID-IDANSTNR        NOT = ALL '+'                                 
232400         MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSTNR                           
232500     END-IF                                                               
232600                                                                          
232700     MOVE +1                TO RADIND                                     
232800                                                                          
232900     PERFORM UNTIL RADIND > MAX-RADINDX                                   
233000       IF DCS-CDC                                                         
233100         IF MID-IDDISTR (RADIND) NOT = ALL '+'                            
233200             MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR     (RADIND)           
233300         END-IF                                                           
233400       ELSE                                                               
233500         MOVE MFS-STAENG-FAELT-NOMOD TO MOD-IDDISTR-ATTR (RADIND)         
233600       END-IF                                                             
233700                                                                          
233800       IF MID-IDPRODNR (RADIND) NOT = ALL '+'                             
233900           MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRODNR      (RADIND)           
234000       END-IF                                                             
234100                                                                          
234200       IF MID-IDPLKLST (RADIND) NOT = ALL '+'                             
234300           MOVE MFS-ROER-EJ-FAELT TO MOD-IDPLKLST      (RADIND)           
234400       END-IF                                                             
234500                                                                          
234600       IF MID-IDKOLLI (RADIND) NOT = ALL '+'                              
234700           MOVE MFS-ROER-EJ-FAELT TO MOD-IDKOLLI       (RADIND)           
234800       END-IF                                                             
234900                                                                          
235000       IF MID-KDKOLLI (RADIND) NOT = ALL '+'                              
235100           MOVE MFS-ROER-EJ-FAELT TO MOD-KDKOLLI       (RADIND)           
235200       END-IF                                                             
235300                                                                          
235400       IF MID-VKORDBTO-KOLLI (RADIND) NOT = ALL '+'                       
235500           MOVE MFS-ROER-EJ-FAELT TO MOD-VKORDBTO-KOLLI (RADIND)          
235600       END-IF                                                             
235700                                                                          
235800       IF MID-KDEMBTYP (RADIND) NOT = ALL '+'                             
235900           MOVE MFS-ROER-EJ-FAELT TO MOD-KDEMBTYP      (RADIND)           
236000       END-IF                                                             
236100                                                                          
236200       IF MID-DIKOLLIL (RADIND) NOT = ALL '+'                             
236300           MOVE MFS-ROER-EJ-FAELT TO MOD-DIKOLLIL      (RADIND)           
236400       END-IF                                                             
236500                                                                          
236600       IF MID-DIKOLLIB (RADIND) NOT = ALL '+'                             
236700           MOVE MFS-ROER-EJ-FAELT TO MOD-DIKOLLIB      (RADIND)           
236800       END-IF                                                             
236900                                                                          
237000       IF MID-DIKOLLIH (RADIND) NOT = ALL '+'                             
237100           MOVE MFS-ROER-EJ-FAELT TO MOD-DIKOLLIH      (RADIND)           
237200       END-IF                                                             
237300                                                                          
237400       IF MID-VKORDBTO-KOLLI (RADIND) NOT = ALL '+'                       
237500           MOVE MFS-ROER-EJ-FAELT TO MOD-VKORDBTO-KOLLI (RADIND)          
237600       END-IF                                                             
237700                                                                          
237800       MOVE MFS-STAENG-FAELT-NOMOD TO                                     
237900            MOD-KDPRTVAL-ADR-RAD-ATTR (RADIND)                            
238000            MOD-KDPRTVAL-FS-RAD-ATTR (RADIND)                             
238100            MOD-FLAVSP             (RADIND)                               
238200                                                                          
238300       ADD +1               TO RADIND                                     
238400     END-PERFORM                                                          
238500                                                                          
238600     .                                                                    
238700     EJECT                                                                
238800 S03-RENSA-FAELT SECTION.                                                 
238900     MOVE 'S03-RENSA-FAELT '     TO WS-AKTUELL-SEKTION                    
239000                                                                          
239100     MOVE MFS-RENSA-FAELT    TO MOD-IDDISTR        (RADIND)               
239200                                MOD-IDPRODNR       (RADIND)               
239300                                MOD-IDPLKLST       (RADIND)               
239400                                MOD-IDKOLLI        (RADIND)               
239500                                MOD-KDKOLLI        (RADIND)               
239600                                MOD-VKORDBTO-KOLLI (RADIND)               
239700                                MOD-KDEMBTYP       (RADIND)               
239800                                MOD-DIKOLLIL       (RADIND)               
239900                                MOD-DIKOLLIB       (RADIND)               
240000                                MOD-DIKOLLIH       (RADIND)               
240100                                MOD-KDPRTVAL-ADR-RAD (RADIND)             
240200                                MOD-KDPRTVAL-FS-RAD (RADIND)              
240300     .                                                                    
240400     EJECT                                                                
240500 S04-EV-SEND-CASELABEL  SECTION.                                          
240600     MOVE 'S04-EV-SEND-CASE'     TO WS-AKTUELL-SEKTION                    
240700                                                                          
240800     MOVE WS-IDDISTR (RADIND)      TO DIST07-IDDISTR                      
240900                                                                          
241000     IF DCS-CDC AND                                                       
241100        (DIST07-USA-RETAILER       OR                                     
241200         DIST07-USA-SUPPL-FROM-CDC)                                       
241300       CONTINUE                                                           
241400     ELSE                                                                 
241500       PERFORM S05-SEND-PRINTTRANS                                        
241600     END-IF                                                               
241700     .                                                                    
241800     SKIP2                                                                
241900 S05-SEND-PRINTTRANS SECTION.                                             
242000     MOVE 'S05-SEND-PRINTTR'     TO WS-AKTUELL-SEKTION                    
242100                                                                          
242200     MOVE WS-MID-ADRESSFL          TO 4333-MID-KDPRTVAL-UT                
242300     MOVE WS-IDDISTR  (RADIND)     TO WS-IDDISTR-NUM4                     
242400     MOVE WS-IDDISTR-NUM4          TO 4333-MID-IDDISTR-UT                 
242500     MOVE WS-IDKUNDNR (RADIND)     TO WS-IDKUNDNR-NUM6                    
242600     MOVE WS-IDKUNDNR-NUM6         TO 4333-MID-IDKUNDNR-UT                
242700     MOVE WS-IDORDNR  (RADIND)     TO 4333-MID-IDORDNR-UT                 
242800     MOVE WS-IDKOLLI  (RADIND)     TO 4333-MID-IDKOLLI-UT                 
242900     MOVE MSGI-IDDC                TO 4333-MID-IDDC-UT                    
243000                                                                          
243100     IF DIRLEV-KOLLI                                                      
243200       MOVE WS-IDPRODNR(RADIND)    TO 4333-MID-IDPRODNR-UT                
243300     ELSE                                                                 
243400       MOVE ZERO                   TO 4333-MID-IDPRODNR-UT                
243500     END-IF                                                               
243600     MOVE ZERO                     TO 4333-MID-IDKOLLI-TOM                
243700     MOVE '++++'                   TO 4333-MID-IDDISTR-IN                 
243800     MOVE '++++++'                 TO 4333-MID-IDKUNDNR-IN                
243900     MOVE '+++++'                  TO 4333-MID-IDORDNR-IN                 
244000                                      4333-MID-IDKOLLI-IN                 
244100     MOVE '++'                     TO 4333-MID-IDDC-IN                    
244200     MOVE '+++++++'                TO 4333-MID-IDPRODNR-IN                
244300     MOVE '++'                     TO 4333-MID-KDPRTVAL-IN                
244400                                                                          
244500     MOVE WS-KDMFSFOR              TO 4333-MID-KDMFSFOR                   
244600     MOVE '431C'                   TO 4333-IDTRANS                        
244700     COMPUTE 4333-MID-KVLL = LENGTH OF 4333-MID-W4I33301 + 17             
244800     MOVE MFS-KDMFSFOR             TO 4333-MID-KDMFSFOR                   
244900                                                                          
245000     PERFORM IMS-PURGE-ALT4333-MSG                                        
245100     .                                                                    
245200     EJECT                                                                
245300 S06-EV-STAENG-FAELT   SECTION.                                           
245400     MOVE 'S06-EV-STAENG-F '     TO WS-AKTUELL-SEKTION                    
245500                                                                          
245600     EVALUATE TRUE                                                        
245700       WHEN ((DCS-NDC-PF AND DCS-JAPAN) OR                                
245800            (DCS-SDC AND (DCS-SPAIN OR DCS-ITALY OR DCS-SWEDEN)))         
245900        AND MSGI-KDPRTVAL-ADR = ALL '+'                                   
246000                                                                          
246100         MOVE MFS-ADD-SAETT-CURSOR   TO MOD-KDPRTVAL-ADR-ATTR             
246200         IF DCS-NDC-NA AND DCS-USA                                        
246300           CONTINUE                                                       
246400         ELSE                                                             
246500           MOVE INF-WRONG-PRINTER    TO MED-IDMFSFEL                      
246600           PERFORM S10-INF-ROUTINE                                        
246700         END-IF                                                           
246800                                                                          
246900       WHEN ((DCS-NDC-NA OR DCS-NDC-PF) OR                                
247000            (DCS-SDC AND (DCS-SPAIN OR DCS-ITALY OR DCS-SWEDEN)))         
247100        AND MSGI-KDPRTVAL-ADR NOT = ALL '+'                               
247200         MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDPRODNR-ATTR (1)             
247300       WHEN (DCS-NDC-PF AND DCS-AUSTRALIA) OR                             
247400            (DCS-SDC AND (DCS-HOLLAND OR DCS-ENGLAND))                    
247500         MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDPRODNR-ATTR (1)             
247600     END-EVALUATE                                                         
247700                                                                          
247800     MOVE +1                TO RADIND                                     
247900     PERFORM UNTIL RADIND > MAX-RADINDX                                   
248000       MOVE MFS-RENSA-FAELT        TO MOD-IDDISTR      (RADIND)           
248100       MOVE MFS-STAENG-FAELT-NOMOD TO MOD-IDDISTR-ATTR (RADIND)           
248200       ADD +1               TO RADIND                                     
248300     END-PERFORM                                                          
248400     .                                                                    
248500     SKIP2                                                                
248600 S07-ERR-ROUTINE-ROER-EJ-FAELT      SECTION.                              
248700                                                                          
248800     MOVE 'S07-ERR-ROUT    '     TO WS-AKTUELL-SEKTION                    
248900     CALL WMEDKONV USING MED-WMEDAREA                                     
249000     MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                     
249100     PERFORM S02-ROER-EJ-FAELT                                            
249200     .                                                                    
249300     SKIP2                                                                
249400 S09-ERR-ROUTINE                     SECTION.                             
249500                                                                          
249600     MOVE 'S09-ERR-ROUT    '     TO WS-AKTUELL-SEKTION                    
249700     CALL WMEDKONV USING MED-WMEDAREA                                     
249800     MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                     
249900     .                                                                    
250000     SKIP2                                                                
250100 S10-INF-ROUTINE                     SECTION.                             
250200                                                                          
250300     MOVE 'S10-INF-ROUTIN  '     TO WS-AKTUELL-SEKTION                    
250400     CALL WMEDKONV USING MED-WMEDAREA                                     
250500     MOVE MED-MFSINF                  TO MOD-TEMFSINF                     
250600     .                                                                    
250700     SKIP2                                                                
250800 S11-SAETT-CURSOR-FOER-SDC       SECTION.                                 
250900     MOVE 'S11-SAETT-CURSOR'     TO WS-AKTUELL-SEKTION                    
251000                                                                          
251100     IF KEYS-OK                                                           
251200       EVALUATE TRUE                                                      
251300       WHEN (DCS-NDC-NA)                                                  
251400         MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDANSTNR-ATTR                 
251500         WHEN (DCS-NDC-NA OR DCS-NDC-PF  OR                               
251600            (DCS-SDC AND (DCS-SPAIN OR DCS-ITALY OR DCS-SWEDEN)))         
251700          AND MSGI-KDPRTVAL-ADR NOT = ALL '+'                             
251800           IF INDATA-OK                                                   
251900             MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDPRODNR-ATTR (1)           
252000           END-IF                                                         
252100         WHEN (DCS-NDC-NA OR                                              
252200              (DCS-NDC-PF AND (DCS-JAPAN)) OR                             
252300            (DCS-SDC AND (DCS-SPAIN OR DCS-ITALY OR DCS-SWEDEN)))         
252400          AND MSGI-KDPRTVAL-ADR = ALL '+'                                 
252500           MOVE MFS-ADD-SAETT-CURSOR   TO MOD-KDPRTVAL-ADR-ATTR           
252600         WHEN (DCS-NDC-PF AND (DCS-AUSTRALIA)) OR                         
252700              (DCS-SDC AND (DCS-HOLLAND OR DCS-ENGLAND))                  
252800           IF INDATA-OK                                                   
252900             MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDPRODNR-ATTR (1)           
253000           END-IF                                                         
253100       END-EVALUATE                                                       
253200     END-IF                                                               
253300                                                                          
253400     IF DCS-SDC OR DCS-NDC-NA OR DCS-NDC-PF                               
253500       MOVE +1                TO RADIND                                   
253600       PERFORM UNTIL RADIND > MAX-RADINDX                                 
253700         MOVE MFS-RENSA-FAELT        TO MOD-IDDISTR      (RADIND)         
253800         MOVE MFS-STAENG-FAELT-NOMOD TO  MOD-IDDISTR-ATTR (RADIND)        
253900         ADD +1               TO RADIND                                   
254000       END-PERFORM                                                        
254100     END-IF                                                               
254200     .                                                                    
254300     SKIP2                                                                
254400 S12-SEND-DEL-NOTE         SECTION.                                       
254500     MOVE 'S12-SEND-DEL-NO '     TO WS-AKTUELL-SEKTION                    
254600*SECTIONEN SÄNDER FÖLJESEDEL-TRANS TILL SKRIVARE.                         
254700                                                                          
254800     MOVE '++++'                    TO 4341-MID-IDDISTR-IN                
254900     MOVE WS-IDDISTR      (RADIND)  TO WS-IDDISTR-NUM4                    
255000     MOVE WS-IDDISTR-NUM4           TO 4341-MID-IDDISTR-UT                
255100     MOVE '++++++'                  TO 4341-MID-IDKUNDNR-IN               
255200     MOVE WS-IDKUNDNR     (RADIND)  TO WS-IDKUNDNR-NUM6                   
255300     MOVE WS-IDKUNDNR-NUM6          TO 4341-MID-IDKUNDNR-UT               
255400     MOVE '+++++'                   TO 4341-MID-IDORDNR-IN                
255500     MOVE WS-IDORDNR      (RADIND)  TO 4341-MID-IDORDNR-UT                
255600     MOVE '+'                       TO 4341-MID-IDPLKLST-IN               
255700     MOVE ZERO                      TO 4341-MID-IDPLKLST-UT               
255800     MOVE '+++++'                   TO 4341-MID-IDKOLLI-IN                
255900     MOVE WS-IDKOLLI      (RADIND)  TO 4341-MID-IDKOLLI-UT                
256000     MOVE '+++++'                   TO 4341-MID-IDKOLLI-TOM-IN            
256100     MOVE ZERO                      TO 4341-MID-IDKOLLI-TOM-UT            
256200     MOVE '++'                      TO 4341-MID-KDPRTVAL-IN               
256300     MOVE MSGI-KDPRTVAL-FS          TO 4341-MID-KDPRTVAL-UT               
256400     MOVE '++'                      TO 4341-MID-IDDC-IN                   
256500     MOVE MSGI-IDDC                 TO 4341-MID-IDDC-UT                   
256600     MOVE 'N'                       TO 4341-MID-FL-SVENSK-FSEDEL          
256700                                                                          
256800     COMPUTE 4341-MID-KVLL = LENGTH OF 4341-MID-W4I34101 + 17             
256900     MOVE WS-KDMFSFOR               TO 4341-MID-KDMFSFOR                  
257000                                                                          
257100     PERFORM IMS-PURGE-ALT4341-MSG                                        
257200     .                                                                    
257300     SKIP2                                                                
257400 S15-PLUS-TO-MID-RAD SECTION.                                             
257500     MOVE 'S15-PLUS-TO-MID '     TO WS-AKTUELL-SEKTION                    
257600                                                                          
257700     MOVE ALL '+'            TO MID-IDDISTR        (RADIND)               
257800                                MID-IDPRODNR       (RADIND)               
257900                                MID-IDPLKLST       (RADIND)               
258000                                MID-IDKOLLI        (RADIND)               
258100                                MID-KDKOLLI        (RADIND)               
258200                                MID-VKORDBTO-KOLLI (RADIND)               
258300                                MID-KDEMBTYP       (RADIND)               
258400                                MID-DIKOLLIL       (RADIND)               
258500                                MID-DIKOLLIB       (RADIND)               
258600                                MID-DIKOLLIH       (RADIND)               
258700                                MID-KDPRTVAL-ADR-RAD (RADIND)             
258800                                MID-KDPRTVAL-FS-RAD (RADIND)              
258900     .                                                                    
259000     SKIP2                                                                
259100 S20-DEFAULT-TO-FLFEL             SECTION.                                
259200     MOVE 'S20-DEFAULT-TO-'      TO WS-AKTUELL-SEKTION                    
259300                                                                          
259400     MOVE +1                TO RADIND                                     
259500     PERFORM UNTIL RADIND > MAX-RADINDX                                   
259600       MOVE NEJ             TO MOD-FLAVSP(RADIND)                         
259700       ADD +1               TO RADIND                                     
259800     END-PERFORM                                                          
259900     .                                                                    
260000     SKIP2                                                                
260100 S21-SEND-OPEN SECTION.                                                   
260200     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
260300     MOVE 'OPEN'                  TO SEND-KDFUNC                          
260400     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
260500                                     SEND-OPEN-AREA                       
260600     IF SEND-KDRC > ZERO                                                  
260700       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
260800       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
260900       DELIMITED BY SIZE INTO FELTEXT                                     
261000       DISPLAY FELTEXT                                                    
261100       CALL FELLOG                                                        
261200     END-IF                                                               
261300     .                                                                    
261400     EJECT                                                                
261500 S22-PUT-HEADER        SECTION.                                           
261600     MOVE 001                    TO HDR-REQU-IDMSGVER                     
261700     MOVE 'R'                    TO HDR-REQU-KDPGMACT                     
261800     MOVE IDPGM                  TO HDR-REQU-IDUSER                       
261900     MOVE 'WRONGWEIGHT'          TO HDR-IDOUTTYPE                         
262000     MOVE '001'                  TO HDR-IDOUTREC                          
262100     MOVE '001'                  TO HDR-IDLIST                            
262200     MOVE 'PUT'                  TO SEND-KDFUNC                           
262300     MOVE LENGTH OF HDR-AREA     TO SEND-KVDLEN                           
262400     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
262500                                    SEND-KVDLEN                           
262600                                    HDR-AREA                              
262700     IF SEND-KDRC > ZERO                                                  
262800       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
262900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
263000       DELIMITED BY SIZE INTO FELTEXT                                     
263100       DISPLAY FELTEXT                                                    
263200       CALL FELLOG                                                        
263300     END-IF                                                               
263400     .                                                                    
263500 S23-MOVE-LINEDATA SECTION.                                               
263600     MOVE WS-IDDISTR  (RADIND)     TO HEAD-DIST                           
263700     MOVE WS-IDKUNDNR (RADIND)     TO HEAD-IDKUNDNR                       
263800     MOVE WS-IDORDNR  (RADIND)     TO HEAD-IDORDER                        
263900     MOVE WS-IDKOLLI (RADIND)      TO HEAD-IDKOLLI                        
264000     MOVE WS-KDKOLLI (RADIND)      TO HEAD-KDKOLLI                        
264100     MOVE WS-IDANSTNR              TO HEAD-IDPLKLST                       
264200     MOVE WS-VKORDBTO (RADIND)     TO HEAD-VKORDBTO                       
264300     MOVE WS-CURRENT-DATE          TO HEAD-DATE                           
264400     MOVE WS-CURRENT-TIME          TO HEAD-TIME                           
264500                                                                          
264600     MOVE 1 TO LINE-IX                                                    
264700     PERFORM UNTIL LINE-IX > 16                                           
264800        MOVE TAB-LINE(LINE-IX)      TO SEND-AREA                          
264900        PERFORM  S24-PUT-LINE                                             
265000        ADD 1 TO LINE-IX                                                  
265100     END-PERFORM                                                          
265200                                                                          
265300     MOVE 1 TO LINE-IX                                                    
265400     PERFORM UNTIL LINE-IX > MAX-TAB                                      
265500        MOVE LINE-TAB(LINE-IX)      TO SEND-AREA                          
265600        PERFORM  S24-PUT-LINE                                             
265700        ADD 1 TO LINE-IX                                                  
265800     END-PERFORM                                                          
265900                                                                          
266000      IF MAX-RAD > MAX-LINES                                              
266100        MOVE LINE-END     TO SEND-AREA                                    
266200        PERFORM  S24-PUT-LINE                                             
266300      END-IF                                                              
266400     .                                                                    
266500 S24-PUT-LINE     SECTION.                                                
266600                                                                          
266700        MOVE 'PUT'                  TO SEND-KDFUNC                        
266800        MOVE LENGTH OF SEND-AREA    TO SEND-KVDLEN                        
266900        CALL WZ01SEND            USING SEND-CONTROL-AREA                  
267000                                       SEND-KVDLEN                        
267100                                       SEND-AREA                          
267200        IF SEND-KDRC > ZERO                                               
267300          MOVE SEND-KDRC            TO KDRC-DISPLAY                       
267400          STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                    
267500          DELIMITED BY SIZE INTO FELTEXT                                  
267600          DISPLAY FELTEXT                                                 
267700          CALL FELLOG                                                     
267800        END-IF                                                            
267900     .                                                                    
268000 S25-SEND-CLOSE SECTION.                                                  
268100                                                                          
268200     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
268300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
268400                                                                          
268500     IF SEND-KDRC > 0                                                     
268600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
268700       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
268800       DELIMITED BY SIZE INTO FELTEXT                                     
268900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
269000     END-IF                                                               
269100     .                                                                    
269200     EJECT                                                                
269300 MFS-ALFA-FAELT-RAETT-REST SECTION.                                       
269400     MOVE 'MFS-ALFA-FAELT-'      TO WS-AKTUELL-SEKTION                    
269500                                                                          
269600     PERFORM UNTIL RADIND > MAX-RADINDX                                   
269700      MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDISTR-ATTR       (RADIND)        
269800                                   MOD-IDPRODNR-ATTR      (RADIND)        
269900                                   MOD-IDPLKLST-ATTR      (RADIND)        
270000                                   MOD-IDKOLLI-ATTR       (RADIND)        
270100                                   MOD-KDKOLLI-ATTR       (RADIND)        
270200                                   MOD-VKORDBTO-KOLLI-ATTR(RADIND)        
270300                                   MOD-KDEMBTYP-ATTR      (RADIND)        
270400                                   MOD-DIKOLLIL-ATTR      (RADIND)        
270500                                   MOD-DIKOLLIB-ATTR      (RADIND)        
270600                                   MOD-DIKOLLIH-ATTR      (RADIND)        
270700                               MOD-KDPRTVAL-ADR-RAD-ATTR (RADIND)         
270800                               MOD-KDPRTVAL-FS-RAD-ATTR (RADIND)          
270900       ADD +1          TO RADIND                                          
271000     END-PERFORM                                                          
271100     .                                                                    
271200     SKIP2                                                                
271300 MFS-RENSA-FAELT-RAD       SECTION.                                       
271400     MOVE 'MFS-RENSA-FAELT'      TO WS-AKTUELL-SEKTION                    
271500                                                                          
271600     MOVE +1                TO RADIND                                     
271700     PERFORM UNTIL RADIND > MAX-RADINDX                                   
271800       MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR        (RADIND)              
271900                                 MOD-IDPRODNR       (RADIND)              
272000                                 MOD-IDPLKLST       (RADIND)              
272100                                 MOD-IDKOLLI        (RADIND)              
272200                                 MOD-KDKOLLI        (RADIND)              
272300                                 MOD-VKORDBTO-KOLLI (RADIND)              
272400                                 MOD-KDEMBTYP       (RADIND)              
272500                                 MOD-DIKOLLIL       (RADIND)              
272600                                 MOD-DIKOLLIB       (RADIND)              
272700                                 MOD-DIKOLLIH       (RADIND)              
272800                                 MOD-KDPRTVAL-ADR-RAD (RADIND)            
272900                                 MOD-KDPRTVAL-FS-RAD (RADIND)             
273000       ADD +1                 TO RADIND                                   
273100     END-PERFORM                                                          
273200     .                                                                    
273300     SKIP3                                                                
273400 MFS-ROER-EJ-FAELT-TO-MOD-RAD        SECTION.                             
273500     MOVE 'MFS-ROER-EJ-FA '      TO WS-AKTUELL-SEKTION                    
273600     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDDISTR        (RADIND)              
273700                                 MOD-IDPRODNR       (RADIND)              
273800                                 MOD-IDPLKLST       (RADIND)              
273900                                 MOD-IDKOLLI        (RADIND)              
274000                                 MOD-KDKOLLI        (RADIND)              
274100                                 MOD-VKORDBTO-KOLLI (RADIND)              
274200                                 MOD-KDEMBTYP       (RADIND)              
274300                                 MOD-DIKOLLIL       (RADIND)              
274400                                 MOD-DIKOLLIB       (RADIND)              
274500                                 MOD-DIKOLLIH       (RADIND)              
274600                                 MOD-KDPRTVAL-ADR-RAD (RADIND)            
274700                                 MOD-KDPRTVAL-FS-RAD (RADIND)             
274800     .                                                                    
274900     SKIP3                                                                
275000 MFS-ROER-EJ-FAELT-TO-MOD-RADIX      SECTION.                             
275100     MOVE 'MFS-ROER-EJ-FAE'      TO WS-AKTUELL-SEKTION                    
275200                                                                          
275300     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDDISTR        (RADIX)               
275400                                 MOD-IDPRODNR       (RADIX)               
275500                                 MOD-IDPLKLST       (RADIX)               
275600                                 MOD-IDKOLLI        (RADIX)               
275700                                 MOD-KDKOLLI        (RADIX)               
275800                                 MOD-VKORDBTO-KOLLI (RADIX)               
275900                                 MOD-KDEMBTYP       (RADIX)               
276000                                 MOD-DIKOLLIL       (RADIX)               
276100                                 MOD-DIKOLLIB       (RADIX)               
276200                                 MOD-DIKOLLIH       (RADIX)               
276300                                 MOD-KDPRTVAL-ADR-RAD (RADIX)             
276400                                 MOD-KDPRTVAL-FS-RAD (RADIX)              
276500     .                                                                    
276600     SKIP3                                                                
276700 MFS-ADD-LAES-IN-FAELT-RAD        SECTION.                                
276800     MOVE 'MFS-ADD-LAES-IN'      TO WS-AKTUELL-SEKTION                    
276900                                                                          
277000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-ATTR       (RADIX)         
277100                                   MOD-IDPRODNR-ATTR      (RADIX)         
277200                                   MOD-IDPLKLST-ATTR      (RADIX)         
277300                                   MOD-IDKOLLI-ATTR       (RADIX)         
277400                                   MOD-KDKOLLI-ATTR       (RADIX)         
277500                                   MOD-VKORDBTO-KOLLI-ATTR(RADIX)         
277600                                   MOD-KDEMBTYP-ATTR      (RADIX)         
277700                                   MOD-DIKOLLIL-ATTR      (RADIX)         
277800                                   MOD-DIKOLLIB-ATTR      (RADIX)         
277900                                   MOD-DIKOLLIH-ATTR      (RADIX)         
278000                               MOD-KDPRTVAL-ADR-RAD-ATTR (RADIX)          
278100                               MOD-KDPRTVAL-FS-RAD-ATTR (RADIX)           
278200     .                                                                    
278300     SKIP3                                                                
278400*IMS SEKTIONER                                                            
278500*                                                                         
278600*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
278700*                 III     III MM MMMMM MM SSSS   SSSS                     
278800*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
278900*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
279000*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
279100*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
279200*                 III     III MM MMMMM MM SSSS   SSSS                     
279300*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
279400*                                                                         
279500*                                                                         
279600                                                                          
279700 IMS-GET-MSG SECTION.                                                     
279800     MOVE '  QC' TO GODK-STATUSKODER                                      
279900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
280000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
280100     PERFORM IMS-STATUSKONTROLL                                           
280200     .                                                                    
280300 IMS-INSERT-MSG SECTION.                                                  
280400                                                                          
280500     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
280600       MOVE '0' TO MFS-KDHUVOMR                                           
280700     END-IF                                                               
280800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
280900     MOVE SPACE TO GODK-STATUSKODER                                       
281000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
281100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
281200     PERFORM IMS-STATUSKONTROLL                                           
281300     .                                                                    
281400 IMS-PURGE-ALT4333-MSG SECTION.                                           
281500                                                                          
281600     MOVE SPACE TO GODK-STATUSKODER                                       
281700     CALL CBLTDLI USING PURG                                              
281800                        ALT4333-PCB                                       
281900                        4333-MID-IO-AREA                                  
282000     MOVE ALT4333-STATUS-CODE TO STATUS-WS                                
282100     PERFORM IMS-STATUSKONTROLL                                           
282200     .                                                                    
282300     SKIP3                                                                
282400 IMS-PURGE-ALT4341-MSG SECTION.                                           
282500                                                                          
282600     MOVE SPACE TO GODK-STATUSKODER                                       
282700     CALL CBLTDLI USING PURG                                              
282800                        ALT4341-PCB                                       
282900                        4341-MID-IO-AREA                                  
283000     MOVE ALT4341-STATUS-CODE TO STATUS-WS                                
283100     PERFORM IMS-STATUSKONTROLL                                           
283200     .                                                                    
283300     SKIP3                                                                
283400 IMS-PURGE-ALT4313-MSG    SECTION.                                        
283500                                                                          
283600     MOVE SPACE TO GODK-STATUSKODER                                       
283700     CALL CBLTDLI USING PURG                                              
283800                        ALT4313-PCB                                       
283900                        4313-MID-IO-AREA                                  
284000     MOVE ALT4313-STATUS-CODE TO STATUS-WS                                
284100     PERFORM IMS-STATUSKONTROLL                                           
284200     .                                                                    
284300     SKIP3                                                                
284400 IMS-GU-EMBB SECTION.                                                     
284500     STRING 'WLEMBB01(KDKOLLI  =' W-KDKOLLI-WDK5 ')'                      
284600            DELIMITED BY SIZE INTO SSA1                                   
284700     MOVE '  GE' TO GODK-STATUSKODER                                      
284800     CALL CBLTDLI USING GU EMBB-PCB EMB-WKOLLI SSA1                       
284900     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
285000     PERFORM IMS-STATUSKONTROLL                                           
285100     .                                                                    
285200     EJECT                                                                
285300 IMS-GU-WDE601 SECTION.                                                   
285400     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
285500            DELIMITED BY SIZE INTO SSA1                                   
285600     MOVE '  GE' TO GODK-STATUSKODER                                      
285700     CALL CBLTDLI USING GU WDE6-PCB VORD-WDE601 SSA1                      
285800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
285900     PERFORM IMS-STATUSKONTROLL                                           
286000                                                                          
286100     .                                                                    
286200 IMS-GNP-WDE611 SECTION.                                                  
286300     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
286400            DELIMITED BY SIZE INTO SSA1                                   
286500     MOVE '  GE' TO GODK-STATUSKODER                                      
286600     CALL CBLTDLI USING GNP WDE6-PCB KOLLI-WDE611 SSA1                    
286700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
286800     PERFORM IMS-STATUSKONTROLL                                           
286900     .                                                                    
287000     SKIP2                                                                
287100 IMS-GU-WDE401      SECTION.                                              
287200     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
287300            DELIMITED BY SIZE INTO SSA1                                   
287400     MOVE '  GE' TO GODK-STATUSKODER                                      
287500     CALL CBLTDLI USING GU  WDE4-PCB KORD-WDE401 SSA1                     
287600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
287700     PERFORM IMS-STATUSKONTROLL                                           
287800     .                                                                    
287900     SKIP2                                                                
288000 IMS-GNP-WDE4        SECTION.                                             
288100     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
288200            DELIMITED BY SIZE INTO SSA1                                   
288300     STRING 'WDE411  *F(IDPURAD  >' W-WDE411-IDPURAD-X ')'                
288400            DELIMITED BY SIZE INTO SSA2                                   
288500     MOVE '  GE' TO GODK-STATUSKODER                                      
288600     CALL CBLTDLI USING GNP WDE4-PCB ORAD-WDE411 SSA1 SSA2                
288700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
288800     PERFORM IMS-STATUSKONTROLL                                           
288900     .                                                                    
289000     SKIP2                                                                
289100 IMS-GNP-WDE411          SECTION.                                         
289200     MOVE   'WDE411   '          TO SSA1                                  
289300     MOVE '  GE' TO GODK-STATUSKODER                                      
289400     CALL CBLTDLI USING GNP WDE4-PCB ORAD-WDE411 SSA1                     
289500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
289600     PERFORM IMS-STATUSKONTROLL                                           
289700     SKIP3                                                                
289800     .                                                                    
289900 IMS-GU-KUNDORDER-SEK SECTION.                                            
290000     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
290100            DELIMITED BY SIZE INTO SSA1                                   
290200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
290300     CALL CBLTDLI USING GU   WDE4A-PCB KORD-WDE401 SSA1                   
290400     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
290500                               STATUS-KUNDORDER-SEK-WS                    
290600     PERFORM IMS-STATUSKONTROLL                                           
290700     SKIP2                                                                
290800     .                                                                    
290900 IMS-GN-SEQA-WDE4A1 SECTION.                                              
291000     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
291100            DELIMITED BY SIZE INTO SSA1                                   
291200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
291300     CALL CBLTDLI USING GN   WDE4A-PCB KORD-WDE401 SSA1                   
291400     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
291500                               STATUS-KUNDORDER-SEK-WS                    
291600     PERFORM IMS-STATUSKONTROLL                                           
291700     SKIP2                                                                
291800     .                                                                    
291900 IMS-GU-WDQ3D1 SECTION.                                                   
292000     STRING 'WLORQA01(WDQ3DSEQ =' W-WDQ3D-X ')'                           
292100            DELIMITED BY SIZE INTO SSA1                                   
292200     MOVE '  GE' TO GODK-STATUSKODER                                      
292300     CALL CBLTDLI USING GU  ORQA-PCB ODEL-WDQ301 SSA1                     
292400     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
292500     PERFORM IMS-STATUSKONTROLL                                           
292600     .                                                                    
292700     SKIP2                                                                
292800*IMS-GN-WDQ3D1 SECTION.                                                   
292900*    STRING 'WLORQA01(WDQ3DSEQ =' W-WDQ3D-X ')'                           
293000*           DELIMITED BY SIZE INTO SSA1                                   
293100*    MOVE '  GE' TO GODK-STATUSKODER                                      
293200*    CALL CBLTDLI USING GN  ORQA-PCB ODEL-WDQ301 SSA1                     
293300*    MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
293400*    PERFORM IMS-STATUSKONTROLL                                           
293500*    .                                                                    
293600                                                                          
293700 IMS-GU-WDB601    SECTION.                                                
293800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
293900          DELIMITED BY SIZE INTO SSA1                                     
294000     MOVE '    ' TO GODK-STATUSKODER                                      
294100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
294200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
294300     PERFORM IMS-STATUSKONTROLL                                           
294400     .                                                                    
294500     SKIP2                                                                
294600 IMS-GU-WDK611  SECTION.                                                  
294700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
294800          DELIMITED BY SIZE INTO SSA1                                     
294900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
295000          DELIMITED BY SIZE INTO SSA2                                     
295100     MOVE '  GE' TO GODK-STATUSKODER                                      
295200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
295300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
295400     PERFORM IMS-STATUSKONTROLL                                           
295500     .                                                                    
295600     SKIP3                                                                
295700 IMS-GU-WDK711 SECTION.                                                   
295800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
295900          DELIMITED BY SIZE INTO SSA1                                     
296000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
296100          DELIMITED BY SIZE INTO SSA2                                     
296200     MOVE '  GE' TO GODK-STATUSKODER                                      
296300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
296400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
296500     PERFORM IMS-STATUSKONTROLL                                           
296600     .                                                                    
296700     EJECT                                                                
296800 IMS-STATUSKONTROLL SECTION.                                              
296900     SET STATUS-IX TO 1                                                   
297000     SEARCH GODK-STATUS AT END CALL FELLOG                                
297100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
297200     END-SEARCH                                                           
297300     .                                                                    
