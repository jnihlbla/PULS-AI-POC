000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4031400.                                                
000400 AUTHOR.         CAP GEMINI AB/EP.                                        
000500     DATE-WRITTEN.   NOV   85.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*        VAL 'UU' VID UTSKRIFT AV KOLLIFLAGGA ELLER FÖLJESEDEL            
001000*        SKALL INTE GE NÅGON UTSKRIFT, MED ÄR ETT GODKÄNT VAL.            
001100*                                                                         
001200*    FUNKTION.                                                            
001300*        KOLLIVIS PACKNING - RAPPORTERING AV VAD SOM LIGGER               
001400*        I VISST KOLLI.                                                   
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T314                                              
001800*        MID:         W4I31401                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W4O31401                                            
002200*    SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800*    -- CHECKED BY WY2000                                                 
002900     SKIP3                                                                
003000 77   PROGRAM-NAMN           VALUE 'W4031400'                             
003100                                 PIC X(8).                                
003200 77   KDRC-DISPLAY               PIC Z(5).                                
003300 77    FELTEXT                   PIC X(80)   VALUE SPACE.                 
003400 77    ABEND-TEXT                PIC X(40)   VALUE SPACE.                 
003500 77    JA                        PIC X       VALUE 'J'.                   
003600 77    YES                       PIC X       VALUE 'Y'.                   
003700 77    NEJ                       PIC X       VALUE 'N'.                   
003800 77    SAKNAS                    PIC X       VALUE 'S'.                   
003900 77    OMSTART                   PIC X       VALUE 'O'.                   
004000 77    RAETT                     PIC X       VALUE 'R'.                   
004100 77    FEL                       PIC X       VALUE 'F'.                   
004200 77    ENDAST-STATUS             PIC X       VALUE 'N'.                   
004300 77    SOEK-VIA-PRODNR           PIC X       VALUE 'N'.                   
004400 77    FILLER                    PIC X(09)   VALUE '**IND1**'.            
004500 77    IND1                      PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77    IND2                      PIC S9(9)   VALUE +0   COMP SYNC.        
004700 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77    INX                       PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77    LINE-IX                   PIC 9(3)    VALUE ZERO.                  
005000 77    MAX-TAB                   PIC 9(2)    VALUE ZERO.                  
005100 77    MAX-RAD                   PIC 9(3)    VALUE ZERO.                  
005200 77    MAX-LINES                 PIC S9(9)   VALUE +50  COMP-3.           
005300 77    RAD-INX                   PIC S9(9)   VALUE +0   COMP SYNC.        
005400 77    REST-INX                  PIC S9(9)   VALUE +0   COMP SYNC.        
005500 77    BILD-RAD                  PIC S9(9)   VALUE +0   COMP SYNC.        
005600 77    MID-IDX1                  PIC S9(9)   VALUE +0   COMP SYNC.        
005700 77    MID-IDX2                  PIC S9(9)   VALUE +0   COMP SYNC.        
005800 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +443 COMP SYNC.        
005900 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +82  COMP SYNC.        
006000 77    4317-MOD-LAENGD           PIC S9(4)   VALUE +270 COMP SYNC.        
006100 77    4315-MOD-LAENGD           PIC S9(4)   VALUE +400 COMP SYNC.        
006200 77    4341-LAENGD               PIC S9(4)   VALUE +162 COMP SYNC.        
006300 77    4318-LAENGD               PIC S9(4)   VALUE +125 COMP SYNC.        
006400 77    4314-MID-LAENGD           PIC S9(4)   VALUE +295 COMP SYNC.        
006500 77    0605-LAENGD               PIC S9(4)   VALUE +58  COMP SYNC.        
006600 77    FG-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
006700 77    FG-MAX-INDX               PIC S9(9)   VALUE +10  COMP SYNC.        
006800 77    FILLER                    PIC X(8)    VALUE 'AAAAAAAA'.            
006900 77    WS-TOT-ANT-RADER          PIC S9(3)   VALUE +0    COMP-3.          
007000 77    WS-ANT-RADER-INT          PIC S9(3)   VALUE +0    COMP-3.          
007100 77    WS-ANT-RAD-I-BILD         PIC S9(3)   VALUE +0    COMP-3.          
007200 77    WS-MAX-ANT-RAD-I-BILD     PIC S9(3)   VALUE +200  COMP-3.          
007300 77    WS-MAX-ANT-FOR-OMST       PIC S9(3)   VALUE +100  COMP-3.          
007400 77    WS-ANTAL-I-MID            PIC S9(3)   VALUE +0    COMP-3.          
007500 77    WS-RAD-FOM                PIC S9(4)   VALUE +0.                    
007600 77    WS-RAD-TOM                PIC S9(4)   VALUE +0.                    
007700 77    WS-KVPALL                 PIC S9(3)   VALUE +0    COMP-3.          
007800 77    WS-KVRAM                  PIC S9(3)   VALUE +0    COMP-3.          
007900 77    WS-KVLOCK                 PIC S9(3)   VALUE +0    COMP-3.          
008000 77    FILLER                    PIC X(8)    VALUE 'BBBBBBBB'.            
008100 77    WS-KDORDKL                PIC S9(1)   VALUE +0    COMP-3.          
008200 77    WS-KDKOLSTA               PIC S9(1)   VALUE +0    COMP-3.          
008300 77    WS-SAVE-KOLLI-KDKOLSTA    PIC S9(1)   VALUE +0    COMP-3.          
008400 77    WS-EMBPROF                PIC X(1)    VALUE SPACE.                 
008500 77    WS-KOLLI-FLBANDST         PIC X(1)    VALUE SPACE.                 
008600 77    WS-SAVE-KOLLI-FLBANDST    PIC X(1)    VALUE SPACE.                 
008700 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
008800 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
008900 77    WS-TIDPUNKT               PIC 9(8)   VALUE ZERO.                   
009000 77    WS-VKORDBTO               PIC S9(6)V9(1)  VALUE ZERO.              
009100 77    WS-MOD-VKORDBTO           PIC 9(6)V999  VALUE ZERO.                
009200 77    WS-VKORDBTO-SUM           PIC 9(6)V9(3) VALUE ZERO.                
009300 77    ACC-ORAD-VKORDNTO         PIC 9(6)V9(3) VALUE ZERO.                
009400 77    WS-ACC-ORAD-VKORDNTO      PIC 9(6)V9(3) VALUE ZERO.                
009500 77    FILLER                    PIC X(8)    VALUE 'CCCCCCCC'.            
009600 77    WS-VLORDBTO               PIC S9(4)V9(3)  VALUE ZERO.              
009700 77    WS-SUORDV-KOLLI           PIC S9(9)V9(2)  VALUE ZERO.              
009800 77    WS-SUORDV-KOLLI-LOC       PIC S9(9)V9(2)  VALUE ZERO.              
009900 77    WS-SUORDV-KOLLI-LOCPREL   PIC S9(9)V9(2)  VALUE ZERO.              
010000 77    WS-KDVALISO               PIC X(3)   VALUE SPACE.                  
010100 77    WS-KDVALISO-EXP           PIC X(3)   VALUE SPACE.                  
010200 77    WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
010300 77    WS-IDDISTR                PIC X(4)   VALUE SPACE.                  
010400 77    WS-IDDISTR-NUM            PIC 9(4)   VALUE ZERO.                   
010500 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
010600 77    WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                   
010700 77    WS-IDKOLLI                PIC X(5)   VALUE SPACE.                  
010800 77    WS-IDPLKLST               PIC S9(3)  VALUE ZERO COMP-3.            
010900 77    WS-KDKOLLI                PIC X(8)   VALUE SPACE.                  
011000 77    WS-KDKOLLID               PIC X(1)   VALUE SPACE.                  
011100 77    FILLER                    PIC X(8)   VALUE 'DDDDDDDD'.             
011200 77    WS-IDPRODNR               PIC X(7)   VALUE SPACE.                  
011300 77    WS-JFR-IDPRODNR           PIC X(7)   VALUE SPACE.                  
011400 77    WS-IDPURAD                PIC 9(4)   VALUE ZERO.                   
011500 77    WS-START-RAD              PIC 9(4)   VALUE ZERO.                   
011600 77    WS-SISTA-RAD              PIC 9(4)   VALUE ZERO.                   
011700 77    WS-AKTUELL-RAD            PIC 9(4)   VALUE ZERO.                   
011800 77    WS-KVLEVART               PIC 9(6)   VALUE ZERO.                   
011900 77    WS-FLAUTFAK               PIC X(1)   VALUE SPACE.                  
012000 77    WS-KDFAKTYP               PIC X(1)   VALUE SPACE.                  
012100 77    WS-KVPTID-MIN             PIC S9(7)  VALUE ZERO COMP-3.            
012200 77    WS-KVPTID-TIM             PIC S9(3)  VALUE ZERO COMP-3.            
012300 77    WS-IDKOLLI-SAMP           PIC S9(5)  VALUE ZERO COMP-3.            
012400 77    FILLER                    PIC X(8)   VALUE 'EEEEEEEE'.             
012500 77    WS-TRAEFF-PACKARE         PIC X(1).                                
012600 77    WS-TRAEFF-RAD             PIC X(1).                                
012700 77    WS-RADER-OK               PIC X(1)   VALUE 'N'.                    
012800 77    WS-RADER-SAKNAS           PIC X(1).                                
012900 77    WS-RADER-RAPPORTERADE     PIC X(1).                                
013000 77    WS-PACKARES-ODEL-REDAN-KLARA PIC X.                                
013100 77    WS-DARFS                  PIC 9(12)  VALUE ZERO.                   
013200 77    WS-KVORAPP                PIC S9(7)  VALUE ZERO COMP-3.            
013300 77    MAX-RAD-ANTAL             PIC S9(3)  VALUE +13  COMP-3.            
013400 77    MID-MAX-RAD               PIC S9(3)  VALUE +13  COMP-3.            
013500 77    WS-SAVE-KVORDRAD-LEVPL    PIC S9(5)      COMP-3 VALUE ZERO.        
013600 77    WS-KORD-IDORDER           PIC S9(7)  VALUE ZERO COMP-3.            
013700*                                        ANTAL FÄRDIGPACKADE RADEW        
013800*                                        I ETT RAD-INTERVALL.             
013900 77    FILLER                    PIC X(8)   VALUE 'GGGGGGGG'.             
014000 77    WS-RINT-ANT-FPACK-ORAD    PIC S9(5)  VALUE ZERO COMP-3.            
014100 77    WS-KDPRTVAL-ADRESSFL      PIC XX     VALUE SPACE.                  
014200 77    WS-KDPRTVAL-FOLJEFL       PIC XX     VALUE SPACE.                  
014300 77    WS-PRT-KDSVAR-ADRESSFL    PIC X(1)   VALUE SPACE.                  
014400 77    WS-PRT-KDSVAR-FOLJEFL     PIC X(1)   VALUE SPACE.                  
014500                                                                          
014600 77    FILLER                    PIC X(8)   VALUE 'HHHHHHHH'.             
014700 77    WS-SPAR-IDORDER           PIC S9(07) VALUE ZERO COMP-3.            
014800 77    WS-SPAR-IDARTNR           PIC S9(09) VALUE ZERO COMP-3.            
014900 77    WS-SPAR-IDDC              PIC  X(02) VALUE ZERO.                   
015000 77    WS-SPAR-BEART             PIC  X(25) VALUE SPACE.                  
015100 77    WS-SPAR-KVBEART           PIC S9(07) VALUE ZERO COMP-3.            
015200 77    WS-SPAR-FLTILLK           PIC  X(01) VALUE SPACE.                  
015300 77    WS-SPAR-IDKUNDRF-RO       PIC  X(10) VALUE SPACE.                  
015400 77    WS-TIORDREG-NUM6          PIC 9(6)   VALUE ZERO.                   
015500 77    WS-SPAR-IDPURAD           PIC S9(05) VALUE ZERO COMP-3.            
015600 77    RKOD-ABEND                PIC S9(4)  VALUE +33   COMP SYNC.        
015700 77    IX                        PIC S9(9)  VALUE ZERO  COMP SYNC.        
015800 77    IX-MAX                    PIC S9(9)  VALUE +7    COMP SYNC.        
015900 77    WS-IDLEVNR                PIC X(5)   VALUE SPACES.                 
016000 77    WS-ODEL-IDDC-EXP          PIC X(2)   VALUE SPACES.                 
016100                                                                          
016200 01  WS-IDSYSTEM.                                                         
016300     03 WS-IDSYST-1-3            PIC X(3)    VALUE SPACE.                 
016400     03 WS-IDSYST-4              PIC X(1)    VALUE SPACE.                 
016500*                                                                         
016600 01    WS-AREA.                                                           
016700   03 WS-VKORDBTO-TOT            PIC 9(6).999  VALUE ZERO.                
016800                                                                          
016900 01  WS-IDPRTLST.                                                         
017000     03 WS-SYSTDEL               PIC X(1).                                
017100     03 WS-LISTTYP               PIC X(2).                                
017200     03 WS-DC                    PIC X(2).                                
017300     03 WS-KDPRT                 PIC X(3).                                
017400 01  WS-IDPRTJAP REDEFINES WS-IDPRTLST.                                   
017500     03 WS-LASER-BLANKETT        PIC X(6).                                
017600     03 WS-NDC-JAP-KDPRT         PIC X(3).                                
017700                                                                          
017800*                                                                         
017900 01  TRANSFER-KUND               PIC 9(7).                                
018000     88 TRANSFER-KUNDNR          VALUE 0000511                            
018100                                       0000512                            
018200                                       0000513.                           
018300     88  RETUR-KUNDNR            VALUE 0000051.                           
018400*                                                                         
018500 01  WS-TIPACTID-8.                                                       
018600                                                                          
018700     03  WS-TIPACTID-6           PIC 9(6).                                
018800     03  FILLER                  PIC 9(2).                                
018900*                                                                         
019000 77    WS-NYTT-KOLLI             PIC X(01).                               
019100   88  NYTT-KOLLI                           VALUE 'J'.                    
019200     SKIP2                                                                
019300 77    WS-STARTA-OM              PIC X(01).                               
019400   88  STARTA-OM                            VALUE 'J'.                    
019500     SKIP2                                                                
019600 77    WS-IDTRANS                PIC X(04).                               
019700   88  WS-SAMMA-BILD                        VALUE '4314'.                 
019800   88  WS-GODKAND-BILD                      VALUE '4311' '4312'           
019900                                                  '4313' '4314'           
020000                                                  '4315' '4316'           
020100                                                  '4317' '4318'.          
020200   88  WS-ORDERVIS-TRANS                    VALUE '0605'.                 
020300     SKIP2                                                                
020400 77    WS-INDATA-TEST            PIC X(01).                               
020500   88  WS-INDATA-FEL                        VALUE 'F'.                    
020600   88  WS-INDATA-RATT                       VALUE 'R'.                    
020700     SKIP2                                                                
020800 77    WS-BEHANDLING-TEST        PIC X(01).                               
020900   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
021000   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
021100     SKIP2                                                                
021200 77    FL-INTERVALL              PIC X(01).                               
021300   88  INTERVALL-RAD                        VALUE 'J'.                    
021400   88  AVVIKELSE-RAD                        VALUE 'N'.                    
021500     SKIP2                                                                
021600 77    FL-RAD-INOM-INTERVALL     PIC X(01).                               
021700   88  RAD-FINNS-I-INTERVALL                VALUE 'J'.                    
021800     SKIP2                                                                
021900 77    FL-SLINGA-KLAR            PIC X(01).                               
022000   88  SLINGA-KLAR                          VALUE 'J'.                    
022100                                                                          
022200 77    DIRLEV-KOLLI-SW           PIC X(01).                               
022300   88  DIRLEV-KOLLI                         VALUE 'J'.                    
022400*                                                                         
022500 77    NYA-NYCKLAR-SW            PIC X(01).                               
022600   88  NYA-NYCKLAR                          VALUE 'J'.                    
022700 77    WS-WEIGHT                 PIC X(01).                               
022800   88  WEIGHT-MISMATCH                      VALUE 'J'.                    
022900                                                                          
023000 77    SW-TIKLAR-UPPDATERAD      PIC X(01).                               
023100*                                                                         
023200     EJECT                                                                
023300 01     DYNAMISKA-SUBPROGRAM.                                             
023400   03   CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.             
023500   03   FELLOG                  PIC X(8)    VALUE 'FELLOG  '.             
023600*                                                                         
023700 01  GEMENSAMMA-SUBPROGRAM.                                               
023800     03  W006PRT                 PIC X(8)    VALUE 'W006PRT'.             
023900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
024000     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
024100     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
024200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
024300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
024400     03  W403TMS1                PIC X(8)    VALUE 'W403TMS1'.            
024500*                                                                         
024600*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
024700*                                                                         
024800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
024900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
025000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
025100 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
025200     SKIP3                                                                
025300*    --- PARAMETERS TO WZ01SEND                                           
025400 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
025500     SKIP3                                                                
025600*01  -COPY WZ01SEND                                                       
025700     EJECT                                                                
025800 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
025900 01  HDR-AREA.                                                            
026000*    03  -COPY WZ01REQU  -PRE HDR-                                        
026100*    03  -COPY WZ04HDR                                                    
026200*                                                                         
026300 01  FILLER                      PIC X(9)    VALUE 'SEND-AREA'.           
026400 01  SEND-AREA                   PIC X(100)  VALUE SPACE.                 
026500*                                                                         
026600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
026700*01 -COPY WMSGINIT                                                        
026800     EJECT                                                                
026900*        PRINTERKONTROLL                                                  
027000*                                                                         
027100 01  FILLER                     PIC X(16)   VALUE 'LÄNKAREOR'.            
027200*                                                                         
027300 01  FILLER                     PIC X(16)   VALUE 'W006PRT '.             
027400*   -COPY W006PRT                                                         
027500     EJECT                                                                
027600*   -COPY WDECAREA                                                        
027700     EJECT                                                                
027800*01    -COPY WWDC99                                                       
027900                                                                          
028000 01  FILLER                     PIC X(16)   VALUE 'W411DNOT'.             
028100*01 -COPY W411DNOT                                                        
028200     EJECT                                                                
028300*01 -COPY WWOMVAND                                                        
028400     EJECT                                                                
028500*                                                                         
028600*TMS PACKNING INFO                                                        
028700 01  FILLER                      PIC X(8)   VALUE  'W403TMS1'.            
028800*    -COPY W403TMS1                                                       
028900*                                                                         
029000 01     WS-IDKUNDRF.                                                      
029100   03   WS-IDORDNR              PIC X(5).                                 
029200   03   FILLER                  PIC X(5)    VALUE SPACE.                  
029300*                                                                         
029400 01     WS-JFR-IDANSTNR.                                                  
029500   03   FILLER                  PIC X(3).                                 
029600   03   WS-JFR-IDANSTNR-5       PIC X(5).                                 
029700*                                                                         
029800 01    WS-SUPTID-PRAPP          PIC 9(3)V99.                              
029900 01    FILLER REDEFINES WS-SUPTID-PRAPP.                                  
030000   03  WS-SUPTID-TIM            PIC 9(3).                                 
030100   03  WS-SUPTID-MIN            PIC 9(2).                                 
030200     EJECT                                                                
030300 01     HJALP-ODEL-DARFS     PIC 9(12).                                   
030400 01     FILLER               REDEFINES HJALP-ODEL-DARFS.                  
030500   03   FILLER               PIC 9(2).                                    
030600   03   HJALP-ODEL-DARFS-6   PIC 9(6).                                    
030700   03   FILLER               PIC 9(4).                                    
030800     SKIP2                                                                
030900 01     HJALP-4472-TIRFS     PIC 9(11).                                   
031000 01     FILLER               REDEFINES HJALP-4472-TIRFS.                  
031100   03   FILLER               PIC 9(1).                                    
031200   03   HJALP-4472-TIRFS-6   PIC 9(6).                                    
031300   03   FILLER               PIC 9(4).                                    
031400     SKIP2                                                                
031500 01     HJALPFALT.                                                        
031600   03   HJ-IDRADNR-FOM.                                                   
031700     05 HJ-IDRADNR-FOM-N        PIC 9(4).                                 
031800   03   HJ-IDRADNR-TOM.                                                   
031900     05 HJ-IDRADNR-TOM-N        PIC 9(4).                                 
032000 01     FILLER                  PIC X(10)   VALUE 'SPAR-AREOR'.           
032100 01     SPAR-AREOR.                                                       
032200   03   SPAR1-INTERVALL-AREA.                                             
032300     05 SPAR1-IDRADNR-FOM       PIC  9(5).                                
032400     05 SPAR1-IDRADNR-TOM       PIC  9(5).                                
032500     05 SPAR1-KVORAPP           PIC  9(7).                                
032600     05 SPAR1-FLNOLLJ           PIC  X(1).                                
032700*                                                                         
032800   03   SPAR2-INTERVALL-AREA.                                             
032900     05 SPAR2-IDRADNR-FOM       PIC  9(5).                                
033000     05 SPAR2-IDRADNR-TOM       PIC  9(5).                                
033100     05 SPAR2-KVORAPP           PIC  9(7).                                
033200     05 SPAR2-FLNOLLJ           PIC  X(1).                                
033300*                                                                         
033400   03   SPAR-PRAD-UPPG-AREA.                                              
033500     05 SPAR-PRAD-VKARTNTO      PIC  9(6)V9(3)    VALUE ZERO.             
033600     05 SPAR-PRAD-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
033700     05 SPAR-PRAD-KDFARLIG      PIC  S9           VALUE ZERO.             
033800     05 SPAR-PRAD-KVLEVART      PIC  S9(7)        VALUE ZERO.             
033900     05 SPAR-PRAD-PRARTNTO      PIC  S9(9)V9(2)   VALUE ZERO.             
034000     05 SPAR-PRAD-PRAVCOST      PIC  S9(9)V9(2)   VALUE ZERO.             
034100     05 SPAR-PRAD-PRARTNTO-LOC  PIC  S9(9)V9(2)   VALUE ZERO.             
034200     05 SPAR-PRAD-PRARTNTO-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.          
034300     05 SPAR-PRAD-KDVALISO      PIC X(3)          VALUE SPACE.            
034400     05 SPAR-PRAD-KDVALISO-EXP  PIC X(3)          VALUE SPACE.            
034500*                                                                         
034600   03   SPAR-FARLIGT-GODS-AREA.                                           
034700     05 SPAR-IDPSN              PIC 9(3)              VALUE ZERO.         
034800     05 SPAR-VKART-FG           PIC S9(7)      COMP-3 VALUE ZERO.         
034900     05 SPAR-VLFG               PIC S9(4)V9(3) COMP-3 VALUE ZERO.         
035000     05 SPAR-SUEQFG             PIC S9(3)V9(4) COMP-3 VALUE ZERO.         
035100*                                                                         
035200     05 TOTAL-SUEQFG            PIC S9(3)V9(4) COMP-3 VALUE ZERO.         
035300     EJECT                                                                
035400 01     FILLER                  PIC X(16)   VALUE 'ARBETSAREOR'.          
035500 01     ARBETSAREOR.                                                      
035600   03   ARB-AREA-RAD.                                                     
035700     05 ARB-RAD-FOM             PIC  9(4).                                
035800     05 ARB-RAD-TOM             PIC  9(4).                                
035900     05 ARB-RAD-AKTUELL         PIC  9(4).                                
036000     05 ARB-KVLEVART            PIC  9(6).                                
036100     SKIP2                                                                
036200   03   ARB-KOLLI-UPPG-AREA.                                              
036300     05 ARB-KOLLI-VKORDNTO      PIC  9(6)V9(3)    VALUE ZERO.             
036400     05 ARB-KOLLI-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
036500     05 ARB-KOLLI-KDFARLIG      PIC  S9           VALUE ZERO.             
036600     05 ARB-KOLLI-KVORDRAD      PIC  S9(5)        VALUE ZERO.             
036700     05 ARB-KOLLI-KVFALRAD      PIC  S9(5)        VALUE ZERO.             
036800     05 ARB-KOLLI-SUORDV        PIC  S9(9)V9(2)   VALUE ZERO.             
036900     05 ARB-KOLLI-SUORDV-EXP    PIC  S9(9)V9(2)   VALUE ZERO.             
037000     05 ARB-KOLLI-SUORDV-LOC    PIC  S9(9)V9(2)   VALUE ZERO.             
037100     05 ARB-KOLLI-SUORDV-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.            
037200     05 ARB-KOLLI-KDORDKL       PIC  S9(1)        VALUE ZERO.             
037300     05 ARB-KOLLI-FLAUTFAK      PIC  X(1)         VALUE SPACE.            
037400     05 ARB-KOLLI-KDVALISO      PIC X(3)          VALUE SPACE.            
037500     05 ARB-KOLLI-KDVALISO-EXP  PIC X(3)          VALUE SPACE.            
037600     SKIP2                                                                
037700   03   ARB-ANTAL-KOLLI-PLUS-1   PIC 9(5)    VALUE ZERO.                  
037800   03   ARB-ANTAL-KOLLI          PIC 9(5)    VALUE ZERO.                  
037900 01     WS-TIDPUNKT-RED.                                                  
038000   03   WS-HHMMSS               PIC  9(6).                                
038100   03   WS-DD                   PIC  9(2).                                
038200                                                                          
038300 01  WS-KDMATT                   PIC X.                                   
038400     88 US-MEASUREMENT           VALUE 'U'.                               
038500     88 SIS-MEASUREMENT          VALUE 'S'.                               
038600     EJECT                                                                
038700********************************************************                  
038800 01     FILLER                   PIC X(16)   VALUE 'DIST-AREOR'.          
038900 01    TEST-IDDISTR              PIC  9(5)               COMP-3.          
039000 01    FILLER REDEFINES TEST-IDDISTR.                                     
039100*  03  -COPY WWDIST03.                                                    
039200     SKIP3                                                                
039300 01    FILLER REDEFINES TEST-IDDISTR.                                     
039400*  03  -COPY WWDIST07.                                                    
039500     SKIP3                                                                
039600 01    FILLER REDEFINES TEST-IDDISTR.                                     
039700*  03  -COPY WWDIST08.                                                    
039800     SKIP3                                                                
039900 01    FILLER REDEFINES TEST-IDDISTR.                                     
040000*  03  -COPY WWDIST19.                                                    
040100     SKIP3                                                                
040200 01    FILLER REDEFINES TEST-IDDISTR.                                     
040300*  03  -COPY WWDIST21.                                                    
040400     SKIP3                                                                
040500 01    FILLER REDEFINES TEST-IDDISTR.                                     
040600*  03  -COPY WWDIST47.                                                    
040700     SKIP3                                                                
040800*    ----DISTR-DEALER-PRICE-----                                          
040900 01    FILLER REDEFINES TEST-IDDISTR.                                     
041000*  03  -COPY WWDIST79                                                     
041100     SKIP3                                                                
041200 01    FILLER REDEFINES TEST-IDDISTR.                                     
041300*  03  -COPY WWDIST85.                                                    
041400*                                                                         
041500     SKIP2                                                                
041600 01    NYCKLAR-TILL-DLI.                                                  
041700*                                                                         
041800   03    W-WDE401-KUNDORDER-X.                                            
041900     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
042000     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
042100     05    W-401-IDKUNDRF.                                                
042200       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
042300       07  FILLER                PIC X(05)   VALUE SPACE.                 
042400     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
042500     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
042600*                                                                         
042700   03    W-WDE4A1-KUNDORDER-X.                                            
042800     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
042900     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
043000     05    W-4A1-IDKUNDRF.                                                
043100       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
043200       07  FILLER                PIC X(05)   VALUE SPACE.                 
043300*                                                                         
043400   03    W-WDE420-IDPURAD-X.                                              
043500     05    W-420-IDPURAD2        PIC S9(5)   VALUE ZERO  COMP-3.          
043600*                                                                         
043700   03    W-WDE4B-KEYSEQ-MIN-X.                                            
043800     05    W-420-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
043900     05    W-420-IDPURAD-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
044000*                                                                         
044100   03    W-WDE4B-KEYSEQ-MAX-X.                                            
044200     05    W-420-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
044300     05    W-420-IDPURAD-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
044400*                                                                         
044500   03    W-WDE4B-KEYSEQ-X.                                                
044600     05    W-420-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
044700     05    W-420-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
044800*                                                                         
044900   03    W-WDE421-IDKOLLI-X.                                              
045000     05    W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
045100     05    W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
045200*                                                                         
045300   03    W-WDE601-IDPRODNR-X.                                             
045400     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
045500*                                                                         
045600   03    W-WDE611-IDKOLLI-X.                                              
045700     05    W-610-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
045800*                                                                         
045900   03    W-KDKOLLI-WDK5-X.                                                
046000     05    W-KDKOLLI-WDK5        PIC X(8)    VALUE SPACE.                 
046100*                                                                         
046200   03    W-4301-WDGXKEY-X.                                                
046300     05    W-4301-IDHTYP         PIC X(4)    VALUE '4301'.                
046400     05    W-4301-IDPRODNR       PIC S9(7)   VALUE ZERO COMP-3.           
046500     05    W-4301-NYCKEL-VALFRI  PIC X(22)   VALUE LOW-VALUE.             
046600*                                                                         
046700   03    W-4302-WDGXKEY-X.                                                
046800     05    W-4302-IDKOLLI-X.                                              
046900       07    W-4302-IDKOLLI      PIC S9(5)   VALUE ZERO COMP-3.           
047000     05    W-4302-IDPLKLST-X.                                             
047100       07    W-4302-IDPLKLST     PIC S9(3)   VALUE ZERO COMP-3.           
047200*                                                                         
047300   03    W-4321-IDHTYP-X.                                                 
047400     05    W-4321-IDHTYP         PIC X(4)    VALUE '4321'.                
047500     05    W-4321-NYCKEL-VALFRI  PIC X(26)   VALUE LOW-VALUE.             
047600*                                                                         
047700   03    W-4726-WDGXKEY-ROT-X.                                            
047800     05    W-4726-IDHTYP         PIC X(4)    VALUE '4726'.                
047900     05    W-4726-FLBATCH        PIC X(1)    VALUE SPACE.                 
048000     05    W-4726-LOWVALUE       PIC X(25)   VALUE LOW-VALUE.             
048100*                                                                         
048200   03    W-4726-WDGXKEY-UNDSEG-X.                                         
048300     05    W-4726-IDDISTR        PIC S9(5)   COMP-3.                      
048400     05    W-4726-IDKUNDNR       PIC S9(7)   COMP-3.                      
048500     05    W-4726-IDDC           PIC X(2).                                
048600     05    W-4726-KDFAKTYP       PIC X.                                   
048700*                                                                         
048800   03    W-WDQ201-X.                                                      
048900     05    W-201-IDORDER         PIC S9(7)   COMP-3.                      
049000*                                                                         
049100   03    W-IDDC-X.                                                        
049200     05    W-IDDC                PIC X(2).                                
049300*                                                                         
049400   03    W-WDQ2CSEQ-X.                                                    
049500     05    W-WDQ2C-IDGMTREF-X.                                            
049600       07    W-WDQ2C-IDDISTR     PIC S9(5)   COMP-3 VALUE +0.             
049700       07    W-WDQ2C-IDKUNDNR    PIC S9(7)   COMP-3 VALUE +0.             
049800       07    W-WDQ2C-IDKUNDRF.                                            
049900         09    FILLER            PIC  9(2)          VALUE ZERO.           
050000         09    W-WDQ2C-IDORDNR5                                           
050100                                 PIC  9(5)          VALUE ZERO.           
050200         09    FILLER            PIC  X(3)          VALUE SPACE.          
050300*                                                                         
050400   03    W-WDQ301-ORDERDEL-X.                                             
050500     05    W-301-IDORDER         PIC S9(7)   COMP-3.                      
050600     05    W-301-IDDC            PIC X(2).                                
050700     05    W-301-IDPRODNR        PIC S9(7)   COMP-3.                      
050800     05    W-301-IDPLKLST        PIC S9(3)   COMP-3.                      
050900*                                                                         
051000   03    W-4471-WDGXKEY-X.                                                
051100     05    W-4471-IDHTYP         PIC X(4)    VALUE '4471'.                
051200     05    W-4471-IDDC           PIC X(2).                                
051300     05    W-4471-IDPRC.                                                  
051400       07  W-4471-IDPRCBAS       PIC X(3).                                
051500       07  W-4471-IDPRCVAR       PIC X(1).                                
051600     05    FILLER                PIC X(20)   VALUE LOW-VALUE.             
051700*                                                                         
051800   03    W-4472-KDSEGKEY-X.                                               
051900     05    W-4472-KDSEGKEY       PIC X(1)    VALUE '1'.                   
052000*                                                                         
052100   03    W-4477-WDGXKEY-X.                                                
052200     05    W-4477-IDHTYP         PIC X(4)    VALUE '4477'.                
052300     05    W-4477-IDDC           PIC X(2).                                
052400     05    FILLER                PIC X(24)   VALUE LOW-VALUE.             
052500*                                                                         
052600   03    W-4478-WDGXKEY-X.                                                
052700     05    W-4478-IDSHIFT        PIC X(1).                                
052800     05    W-4478-IDUSER         PIC X(8).                                
052900     05    FILLER                PIC X(1)    VALUE LOW-VALUE.             
053000                                                                          
053100   03    W-IDARTNR-X.                                                     
053200     05    W-IDARTNR             PIC S9(9) VALUE ZERO COMP-3.             
053300   03    W-IDDC-k7-X.                                                     
053400     05    W-IDDC-K7             PIC X(2)  VALUE SPACE.                   
053500   03    W-KDSEGKEY-X.                                                    
053600     05    W-KDSEGKEY            PIC X(1)  VALUE '1'.                     
053700   03    w-IDDC-B6-X.                                                     
053800     05    W-IDDC-B6             PIC X(2).                                
053900                                                                          
054000   03    W-WDA601KY-MIN-X.                                                
054100     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
054200     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
054300     05    W-A601KY-MIN-IDORDNR      PIC 9(07) VALUE ZERO.                
054400     05    FILLER                    PIC X(03) VALUE SPACE.               
054500     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
054600     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
054700     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
054800     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
054900     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
055000                                                                          
055100   03    W-WDA601KY-MAX-X.                                                
055200     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
055300     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
055400     05    W-A601KY-MAX-IDORDNR      PIC 9(07) VALUE ZERO.                
055500     05    FILLER                    PIC X(03) VALUE SPACE.               
055600     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
055700     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
055800     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
055900     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
056000     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
056100     EJECT                                                                
056200*                                                                         
056300 01  HEADER.                                                              
056400     03  FILLER.                                                          
056500        05  FILLER               PIC X(50)  VALUE                         
056600            'Gross weight input from screen does not match with'.         
056700        05  FILLER               PIC X(50)  VALUE                         
056800            ' weight in PULS system for below ORDER details.'.            
056900     03  FILLER.                                                          
057000        05  FILLER               PIC X(50)  VALUE                         
057100            'Please check the parts and emballage weight includ'.         
057200        05  FILLER               PIC X(50)  VALUE                         
057300            'ed in this case stated below.All weight are in KG.'.         
057400     03  FILLER.                                                          
057500        05  FILLER               PIC X(100) VALUE SPACE .                 
057600     03  FILLER.                                                          
057700        05  FILLER               PIC X(5)   VALUE SPACE.                  
057800        05  FILLER               PIC X(8)   VALUE 'District'.             
057900        05  FILLER               PIC X(4)   VALUE X'05050505'.            
058000        05  HEAD-DIST            PIC Z(5)   value ZERO.                   
058100        05  FILLER               PIC X(78)  VALUE SPACE.                  
058200     03  FILLER.                                                          
058300        05  FILLER               PIC X(5)   VALUE SPACE.                  
058400        05  FILLER               PIC X(8)   VALUE 'Customer'.             
058500        05  FILLER               PIC X(4)   VALUE X'05050505'.            
058600        05  HEAD-IDKUNDNR        PIC Z(6)9  VALUE ZERO.                   
058700        05  FILLER               PIC X(76)  VALUE SPACE.                  
058800     03  FILLER.                                                          
058900        05  FILLER               PIC X(5)   VALUE SPACE.                  
059000        05  FILLER               PIC X(12)  VALUE 'Order number'.         
059100        05  FILLER               PIC X(3)   VALUE X'050505'.              
059200        05  HEAD-IDORDER         PIC Z(5)   VALUE ZERO.                   
059300        05  FILLER               PIC X(75)  VALUE SPACE.                  
059400     03  FILLER.                                                          
059500        05  FILLER               PIC X(5)   VALUE SPACE.                  
059600        05  FILLER               PIC X(11)  VALUE 'Case number'.          
059700        05  FILLER               PIC X(4)   VALUE X'05050505'.            
059800        05  HEAD-IDKOLLI         PIC Z(5)   VALUE SPACE.                  
059900        05  HEAD-FILLER          PIC X(1)   VALUE SPACE.                  
060000        05  HEAD-IDKOLLI2        PIC Z(5)   VALUE SPACE.                  
060100        05  FILLER               PIC X(69)  VALUE SPACE.                  
060200     03  FILLER.                                                          
060300        05  FILLER               PIC X(5)   VALUE SPACE.                  
060400        05  FILLER               PIC X(9)   VALUE 'Case code'.            
060500        05  FILLER               PIC X(4)   VALUE X'05050505'.            
060600        05  HEAD-KDKOLLI         PIC X(82)  VALUE SPACE.                  
060700     03  FILLER.                                                          
060800        05  FILLER               PIC X(5)   VALUE SPACE.                  
060900        05  FILLER               PIC X(10)  VALUE 'Picking ID'.           
061000        05  FILLER               PIC X(4)   VALUE X'05050505'.            
061100        05  HEAD-IDPLKLST        PIC Z(8)   VALUE ZERO.                   
061200        05  FILLER               PIC X(73)  VALUE SPACE.                  
061300     03  FILLER.                                                          
061400        05  FILLER               PIC X(5)   VALUE SPACE.                  
061500        05  FILLER               PIC X(12)  VALUE                         
061600                                            'Packing time'.               
061700        05  FILLER               PIC X(4)   VALUE X'05050505'.            
061800        05  HEAD-PACKTIME.                                                
061900           07 HEAD-DATE          PIC X(8)   VALUE SPACE.                  
062000           07 FILLER             PIC X(2)   VALUE SPACE.                  
062100           07 HEAD-TIME          PIC X(69)  VALUE SPACE.                  
062200     03  FILLER.                                                          
062300        05  FILLER               PIC X(5)   VALUE SPACE.                  
062400        05  FILLER               PIC X(17)  VALUE                         
062500                                            'Input case weight'.          
062600        05  FILLER               PIC X(3)   VALUE X'050505'.              
062700        05  HEAD-VKORDBTO        PIC Z(6).999  VALUE ZERO.                
062800        05  FILLER               PIC X(67)  VALUE SPACE.                  
062900     03  FILLER.                                                          
063000        05  FILLER               PIC X(5)   VALUE SPACE.                  
063100        05  FILLER               PIC X(25)  VALUE                         
063200                                 'Calculated weight for the'.             
063300        05  FILLER               PIC X(2)   VALUE X'0505'.                
063400        05  HEAD-VKARTNTO        PIC Z(6).999 VALUE ZERO.                 
063500        05  FILLER               PIC X(60)  VALUE SPACE.                  
063600     03  FILLER.                                                          
063700        05  FILLER               PIC X(5)   VALUE SPACE.                  
063800        05  FILLER               PIC X(95)  VALUE                         
063900                                     'parts in the case'.                 
064000     03  FILLER.                                                          
064100        05  FILLER               PIC X(100) VALUE SPACE.                  
064200     03  FILLER.                                                          
064300        05  FILLER               PIC X(100) VALUE 'Order line'.           
064400     03  FILLER.                                                          
064500        05  FILLER               PIC X(2)   VALUE 'No'.                   
064600        05  FILLER               PIC X(2)   VALUE X'0505'.                
064700        05  FILLER               PIC X(7)   VALUE 'Part no'.              
064800        05  FILLER               PIC X(2)   VALUE X'0505'.                
064900        05  FILLER               PIC X(6)   VALUE SPACE.                  
065000        05  FILLER               PIC X(3)   VALUE 'Qty'.                  
065100        05  FILLER               PIC X(2)   VALUE X'0505'.                
065200        05  FILLER               PIC X(22)  VALUE                         
065300                                  'Part Weight + Part emb'.               
065400        05  FILLER               PIC X(2)   VALUE X'0505'.                
065500        05  FILLER               PIC X(11)  VALUE                         
065600                                  'Part Weight'.                          
065700        05  FILLER               PIC X(3)   VALUE X'050505'.              
065800        05  FILLER               PIC X(41)  VALUE 'Location'.             
065900 01  HEADER-TAB  REDEFINES HEADER.                                        
066000     03 TAB-LINE   OCCURS 16 TIMES.                                       
066100        05 FILLER  PIC X(100).                                            
066200                                                                          
066300 01  LINEDATA.                                                            
066400     03 LINE-TAB OCCURS 50 TIMES.                                         
066500        05  LINE1-NUMBER         PIC X(5)    VALUE SPACE.                 
066600        05  FILLER               PIC X(2)    VALUE X'0505'.               
066700        05  LINE1-IDARTNR        PIC Z(9)    VALUE ZERO.                  
066800        05  FILLER               PIC X(2)    VALUE X'0505'.               
066900        05  LINE1-KVAVBART       PIC Z(9)    VALUE ZERO.                  
067000        05  FILLER               PIC X(2)    VALUE X'0505'.               
067100        05  LINE1-VKOLDNET       PIC Z(9)9.999 VALUE ZERO.                
067200        05  FILLER               PIC X(3)    VALUE X'050505'.             
067300        05  LINE1-VKNEWNET       PIC Z(9)9.999 VALUE ZERO.                
067400        05  FILLER               PIC X(3)    VALUE X'050505'.             
067500        05  LINE1-ADLAGOMR       PIC Z(3)    VALUE ZERO.                  
067600        05  FILLER               PIC X(1)    VALUE '.'.                   
067700        05  LINE1-ADGANG         PIC 9(2)    VALUE ZERO.                  
067800        05  FILLER               PIC X(1)    VALUE '.'.                   
067900        05  LINE1-ADPLATS        PIC X(5)    VALUE SPACE.                 
068000        05  FILLER               PIC X(1)    VALUE '.'.                   
068100                                                                          
068200 01  LINE-END                    PIC X(100) VALUE                         
068300        'More parts exist.Please check the case.'.                        
068400                                                                          
068500 01  W-RAETT-1.                                                           
068600     03 RAETT-1-SVE              PIC X(21)                                
068700        VALUE 'KOLLIT UPPDATERAT    '.                                    
068800     03 RAETT-1-ENG              PIC X(21)                                
068900        VALUE 'CASE HAS BEEN UPDATED'.                                    
069000 01  FILLER REDEFINES W-RAETT-1.                                          
069100     03 RAETT-1  OCCURS 2        PIC X(21).                               
069200*                                                                         
069300 01  W-RAETT-2.                                                           
069400     03 RAETT-2-SVE              PIC X(25)                                
069500        VALUE 'KOLLIT DELVIS UPPDATERAT'.                                 
069600     03 RAETT-2-ENG              PIC X(25)                                
069700        VALUE 'CASE PARTIALLY UPDATED   '.                                
069800 01  FILLER REDEFINES W-RAETT-2.                                          
069900     03 RAETT-2  OCCURS 2        PIC X(25).                               
070000*                                                                         
070100 01    MEDDELANDE.                                                        
070200   03   UPPLYSN-1.                                                        
070300     05 FILLER                   PIC X(61)  VALUE                         
070400        'MATA IN RADINTERVALL                   '.                        
070500     05 FILLER                   PIC X(61)  VALUE                         
070600        'ENTER LINE INTERVAL                    '.                        
070700   03    FILLER REDEFINES UPPLYSN-1.                                      
070800     05  UPPLYSNING-1  OCCURS 2   PIC X(61).                              
070900*                                                                         
071000   03    FEL-1.                                                           
071100     05  FILLER                  PIC X(40)   VALUE                        
071200        '701 ORDERN SAKNAS                       '.                       
071300     05  FILLER                  PIC X(40)   VALUE                        
071400        '701 ORDER MISSING                       '.                       
071500   03    FILLER  REDEFINES  FEL-1.                                        
071600     05  FEL-701     OCCURS 2    PIC X(40).                               
071700**********************************************                            
071800   03    FEL-41.                                                          
071900     05  FILLER                  PIC X(40)   VALUE                        
072000        '7011 ORDERN SAKNAS                      '.                       
072100     05  FILLER                  PIC X(40)   VALUE                        
072200        '7011 ORDER MISSING                      '.                       
072300   03    FILLER  REDEFINES  FEL-41.                                       
072400     05  FEL-7011    OCCURS 2    PIC X(40).                               
072500**                                                                        
072600   03    FEL-42.                                                          
072700     05  FILLER                  PIC X(40)   VALUE                        
072800        '7012 ORDERN SAKNAS                      '.                       
072900     05  FILLER                  PIC X(40)   VALUE                        
073000        '7012 ORDER MISSING                      '.                       
073100   03    FILLER  REDEFINES  FEL-42.                                       
073200     05  FEL-7012    OCCURS 2    PIC X(40).                               
073300**                                                                        
073400   03    FEL-43.                                                          
073500     05  FILLER                  PIC X(40)   VALUE                        
073600        '7013 ORDERN SAKNAS                      '.                       
073700     05  FILLER                  PIC X(40)   VALUE                        
073800        '7013 ORDER MISSING                      '.                       
073900   03    FILLER  REDEFINES  FEL-43.                                       
074000     05  FEL-7013    OCCURS 2    PIC X(40).                               
074100**                                                                        
074200   03    FEL-44.                                                          
074300     05  FILLER                  PIC X(40)   VALUE                        
074400        '7014 ORDERN SAKNAS                      '.                       
074500     05  FILLER                  PIC X(40)   VALUE                        
074600        '7014 ORDER MISSING                      '.                       
074700   03    FILLER  REDEFINES  FEL-44.                                       
074800     05  FEL-7014    OCCURS 2    PIC X(40).                               
074900**                                                                        
075000   03    FEL-45.                                                          
075100     05  FILLER                  PIC X(40)   VALUE                        
075200        '7015 ORDERN SAKNAS                      '.                       
075300     05  FILLER                  PIC X(40)   VALUE                        
075400        '7015 ORDER MISSING                      '.                       
075500   03    FILLER  REDEFINES  FEL-45.                                       
075600     05  FEL-7015    OCCURS 2    PIC X(40).                               
075700**                                                                        
075800   03    FEL-46.                                                          
075900     05  FILLER                  PIC X(40)   VALUE                        
076000        '7016 ORDERN SKALL RAPPORTERAS PÅ WEB-LDC'.                       
076100     05  FILLER                  PIC X(40)   VALUE                        
076200        '7016 ORDER MUST BE REPORTED ON WEB-LDC  '.                       
076300   03    FILLER  REDEFINES  FEL-46.                                       
076400     05  FEL-7016    OCCURS 2    PIC X(40).                               
076500**********************************************                            
076600   03    FEL-2.                                                           
076700     05  FILLER                  PIC X(40)   VALUE                        
076800        '716 ORDERN EJ DELAD                     '.                       
076900     05  FILLER                  PIC X(40)   VALUE                        
077000        '716 ORDER HAS NOT BEEN SPLIT            '.                       
077100   03    FILLER  REDEFINES  FEL-2.                                        
077200     05  FEL-716     OCCURS 2    PIC X(40).                               
077300*                                                                         
077400   03    FEL-3.                                                           
077500     05  FILLER                  PIC X(40)   VALUE                        
077600        '717 KOLLI EJ TIDIGARE RAPPORTERAT       '.                       
077700     05  FILLER                  PIC X(40)   VALUE                        
077800        '717 CASE HAS NOT BEEN REPORTED          '.                       
077900   03    FILLER  REDEFINES  FEL-3.                                        
078000     05  FEL-717     OCCURS 2    PIC X(40).                               
078100*                                                                         
078200   03    FEL-4.                                                           
078300     05  FILLER                  PIC X(40)   VALUE                        
078400        '710 ORDERN FÄRDIGRAPPORTERAD            '.                       
078500     05  FILLER                  PIC X(40)   VALUE                        
078600        '710 ORDER TOTALLY REPORTED              '.                       
078700   03    FILLER  REDEFINES  FEL-4.                                        
078800     05  FEL-718     OCCURS 2    PIC X(40).                               
078900*                                                                         
079000   03    FEL-5.                                                           
079100     05  FILLER                  PIC X(40)   VALUE                        
079200        '719 ANGIVEN PACKARE SAKNAS PÅ ORDERN    '.                       
079300     05  FILLER                  PIC X(40)   VALUE                        
079400        '719 PACKER AND ORDER DO NOT MATCH       '.                       
079500   03    FILLER  REDEFINES  FEL-5.                                        
079600     05  FEL-719     OCCURS 2    PIC X(40).                               
079700*                                                                         
079800   03    FEL-6.                                                           
079900     05  FILLER                  PIC X(40)   VALUE                        
080000        '720 ANGIVEN PACKARES ORDERDEL REDAN KLAR'.                       
080100     05  FILLER                  PIC X(40)   VALUE                        
080200        '720 ORDER PART OF PACKER READY          '.                       
080300   03    FILLER  REDEFINES  FEL-6.                                        
080400     05  FEL-720     OCCURS 2    PIC X(40).                               
080500*                                                                         
080600   03    FEL-7.                                                           
080700     05  FILLER                  PIC X(40)   VALUE                        
080800        '721 KOLLIT REDAN RAPPORTERAT            '.                       
080900     05  FILLER                  PIC X(40)   VALUE                        
081000        '721 CASE IS ALREADY REPORTED            '.                       
081100   03    FILLER  REDEFINES  FEL-7.                                        
081200     05  FEL-721     OCCURS 2    PIC X(40).                               
081300*                                                                         
081400   03    FEL-8.                                                           
081500     05  FILLER                  PIC X(40)   VALUE                        
081600        '722 INTERV. EL DELAR TILLHÖR EJ PACKAREN'.                       
081700     05  FILLER                  PIC X(40)   VALUE                        
081800        '722 INTERVAL DOES NOT BELONG TO PACKER  '.                       
081900   03    FILLER  REDEFINES  FEL-8.                                        
082000     05  FEL-722     OCCURS 2    PIC X(40).                               
082100*                                                                         
082200   03    FEL-9.                                                           
082300     05  FILLER                  PIC X(40)   VALUE                        
082400        '723 INTERVALLET EL DEL DÄRAV REDAN RAPP.'.                       
082500     05  FILLER                  PIC X(40)   VALUE                        
082600        '723 INTERVAL ALREADY REPORTED           '.                       
082700   03    FILLER  REDEFINES  FEL-9.                                        
082800     05  FEL-723     OCCURS 2    PIC X(40).                               
082900*                                                                         
083000   03    FEL-10.                                                          
083100     05  FILLER                  PIC X(40)   VALUE                        
083200        '724 NOLL FÅR EJ ANGES                   '.                       
083300     05  FILLER                  PIC X(40)   VALUE                        
083400        '724 ZERO NOT ALLOWED                    '.                       
083500   03    FILLER  REDEFINES  FEL-10.                                       
083600     05  FEL-724     OCCURS 2    PIC X(40).                               
083700*                                                                         
083800   03    FEL-11.                                                          
083900     05  FILLER                  PIC X(40)   VALUE                        
084000        '725 FÖR STORT ANTAL                     '.                       
084100     05  FILLER                  PIC X(40)   VALUE                        
084200        '725 TOO LAGRE QUANTITY                  '.                       
084300   03    FILLER  REDEFINES  FEL-11.                                       
084400     05  FEL-725     OCCURS 2    PIC X(40).                               
084500*                                                                         
084600   03    FEL-12.                                                          
084700     05  FILLER                  PIC X(40)   VALUE                        
084800        '737 KOLLIT REDAN FAKTURERAT             '.                       
084900     05  FILLER                  PIC X(40)   VALUE                        
085000        '737 CASE ALREADY INVOICED               '.                       
085100   03    FILLER  REDEFINES  FEL-12.                                       
085200     05  FEL-737     OCCURS 2    PIC X(40).                               
085300*                                                                         
085400   03    FEL-13.                                                          
085500     05  FILLER                  PIC X(40)   VALUE                        
085600        '738 FELAKTIGA INTERVALLUPPGIFTER        '.                       
085700     05  FILLER                  PIC X(40)   VALUE                        
085800        '738 WRONG INTERVAL INFORMATION          '.                       
085900   03    FILLER  REDEFINES  FEL-13.                                       
086000     05  FEL-738     OCCURS 2    PIC X(40).                               
086100*                                                                         
086200   03    FEL-14.                                                          
086300     05  FILLER                  PIC X(40)   VALUE                        
086400        '748 UPPLYSTA FÄLT FEL                   '.                       
086500     05  FILLER                  PIC X(40)   VALUE                        
086600        '748 HIGHLITED FIELDS WRONG              '.                       
086700   03    FILLER  REDEFINES  FEL-14.                                       
086800     05  FEL-748     OCCURS 2    PIC X(40).                               
086900*                                                                         
087000   03    FEL-15.                                                          
087100     05  FILLER                  PIC X(40)   VALUE                        
087200        '749 FEL NYCKEL                          '.                       
087300     05  FILLER                  PIC X(40)   VALUE                        
087400        '749 WRONG KEY                           '.                       
087500   03    FILLER  REDEFINES  FEL-15.                                       
087600     05  FEL-749     OCCURS 2    PIC X(40).                               
087700*                                                                         
087800   03    FEL-16.                                                          
087900     05  FILLER                  PIC X(40)   VALUE                        
088000        '804 AVVIKELSEKONTROLL PÅGÅR             '.                       
088100     05  FILLER                  PIC X(40)   VALUE                        
088200        '804 DEVIATION CONTROL IN PROGRESS       '.                       
088300   03    FILLER  REDEFINES  FEL-16.                                       
088400     05  FEL-804     OCCURS 2    PIC X(40).                               
088500*                                                                         
088600   03    FEL-17.                                                          
088700     05  FILLER                  PIC X(40)   VALUE                        
088800        '767 RADEN DELVIS RAPPORTERAD            '.                       
088900     05  FILLER                  PIC X(40)   VALUE                        
089000        '767 LINE PARTLY REPORTED                '.                       
089100   03    FILLER  REDEFINES  FEL-17.                                       
089200     05  FEL-767     OCCURS 2    PIC X(40).                               
089300*                                                                         
089400   03    FEL-18.                                                          
089500     05  FILLER                  PIC X(40)   VALUE                        
089600        '826 FÖR MÅNGA RADER PÅ BILDEN, MAX 200  '.                       
089700     05  FILLER                  PIC X(40)   VALUE                        
089800        '826 TOO MANY LINES, MAX 200 LINES       '.                       
089900   03    FILLER  REDEFINES  FEL-18.                                       
090000     05  FEL-826     OCCURS 2    PIC X(40).                               
090100*                                                                         
090200   03    FEL-19.                                                          
090300     05  FILLER                  PIC X(40)   VALUE                        
090400        '830 RADEN NOLLAD AV NOLLJAGARE          '.                       
090500     05  FILLER                  PIC X(40)   VALUE                        
090600        '830 LINE ZEROED BY ZEROHUNTER           '.                       
090700   03    FILLER  REDEFINES  FEL-19.                                       
090800     05  FEL-830     OCCURS 2    PIC X(40).                               
090900*                                                                         
091000   03    FEL-20.                                                          
091100     05  FILLER                  PIC X(40)   VALUE                        
091200        '772 FELAKTIG PRINTER                    '.                       
091300     05  FILLER                  PIC X(40)   VALUE                        
091400        '772 WRONG PRINTER                       '.                       
091500   03    FILLER  REDEFINES  FEL-20.                                       
091600     05  FEL-772     OCCURS 2    PIC X(40).                               
091700*                                                                         
091800   03    FEL-21.                                                          
091900     05  FILLER                  PIC X(40)   VALUE                        
092000        '794 EJ FARLIGT GODS I SAMKOLLI          '.                       
092100     05  FILLER                  PIC X(40)   VALUE                        
092200        '794 NO DANGEROUS CARGO IN MIXED CASE '.                          
092300   03    FILLER  REDEFINES  FEL-21.                                       
092400     05  FEL-794     OCCURS 2    PIC X(40).                               
092500*                                                                         
092600   03    FEL-50.                                                          
092700     05  FILLER                  PIC X(40)   VALUE                        
092800        '800 DIREKT LEV. KOLLI FÅR EJ PACK.PÅ4315'.                       
092900     05  FILLER                  PIC X(40)   VALUE                        
093000        '800 NOT ALLOW.TO PACK DIRECT SUPPL.CASE.'.                       
093100   03    FILLER  REDEFINES  FEL-50.                                       
093200     05  FEL-800     OCCURS 2    PIC X(40).                               
093300                                                                          
093400 77    INF-WEIGHT-NOT-LESS-THAN  PIC X(50)                                
093500       VALUE 'WEIGHT CANNOT BE LESS THAN                   '.             
093600*                                                                         
093700     EJECT                                                                
093800 01    FILLER                 PIC X(16) VALUE 'EMB-TABELL'.               
093900     SKIP3                                                                
094000 01    EMB-TABELL.                                                        
094100   03    EMB-TAB-X.                                                       
094200     05  KLASS        OCCURS 3   INDEXED BY KL-INDX.                      
094300         07  TYP      OCCURS 5   INDEXED BY TYP-INDX                      
094400                                         PIC S9(5)  COMP-3.               
094500   03    EMB-TAB   REDEFINES  EMB-TAB-X.                                  
094600     05  FILLER.                                                          
094700         07  PALLAR   OCCURS 5           PIC S9(5)  COMP-3.               
094800     05  FILLER.                                                          
094900         07  KRAGAR   OCCURS 5           PIC S9(5)  COMP-3.               
095000     05  FILLER.                                                          
095100         07  EMB-LOCK OCCURS 5           PIC S9(5)  COMP-3.               
095200     EJECT                                                                
095300 01    FILLER                 PIC X(16) VALUE 'FG-TABELL'.                
095400                                                                          
095500 01    FG-TABELL.                                                         
095600       03   TAB-POST  OCCURS 10.                                          
095700            05  TAB-IDPSN                PIC 9(3)       VALUE 0.          
095800                                                                          
095900            05  TAB-VKART-FG             PIC S9(7)      COMP-3            
096000                                                        VALUE 0.          
096100            05  TAB-VLFG                 PIC S9(4)V9(3) COMP-3            
096200                                                        VALUE 0.          
096300     EJECT                                                                
096400******************************************************************        
096500*                                                                *        
096600*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
096700*                                                                *        
096800******************************************************************        
096900 01    FILLER                 PIC X(16) VALUE 'MID W4I31401 MID'.         
097000     SKIP3                                                                
097100*01    MID -COPY W4I31401.                                                
097200     EJECT                                                                
097300*01    -COPY WMSGAREA                                                     
097400     EJECT                                                                
097500*  03    MOD -COPY W4O31401.                                              
097600     EJECT                                                                
097700*  03    MID -COPY W4I34101 -PRE 4341-.                                   
097800     EJECT                                                                
097900*  03    MID -COPY W0I60502 -PRE 0605-.                                   
098000     EJECT                                                                
098100*  03    MID -COPY W4I31801 -PRE 4318-.                                   
098200     EJECT                                                                
098300*  03    MOD -COPY W4O31701  -PRE 4317-.                                  
098400     EJECT                                                                
098500*  03    MOD -COPY W4O31501  -PRE 4315-.                                  
098600     EJECT                                                                
098700*01    MID -COPY W4I31401    -PRE SPAR-.                                  
098800     EJECT                                                                
098900*01    MID -COPY W4I31401    -PRE NEXT-.                                  
099000     EJECT                                                                
099100   01  4333-MID-IO-AREA.                                                  
099200                                                                          
099300       03  4333-MID-LL           PIC S9(4)   COMP SYNC.                   
099400       03  4333-MID-Z1           PIC X.                                   
099500       03  4333-MID-Z2           PIC X.                                   
099600       03  4333-MID-TRANSKOD     PIC X(8).                                
099700       03  4333-MID-IDTRANS      PIC X(4).                                
099800       03  4333-MID-KDMFSFOR     PIC X.                                   
099900*      03  MID -COPY W4I33301  -PRE 4333-.                                
100000     EJECT                                                                
100100 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
100200     SKIP3                                                                
100300*01    -COPY WMFSAREA                                                     
100400     EJECT                                                                
100500*01  WDGZRY6 -COPY WDGZRY6.                                               
100600     EJECT                                                                
100700*01  WDGZRYK -COPY WDGZRYK.                                               
100800     EJECT                                                                
100900*01  XXJK    -COPY WDGX4322   -PRE XXJK-                                  
101000     EJECT                                                                
101100******************************************************************        
101200*                                                                         
101300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
101400*                                                                         
101500 01    IMS-WS.                                                            
101600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
101700     SKIP3                                                                
101800*                        **** STATUS-KOD FRÅN IMS                         
101900   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
102000     88    KUNDORDER-SEK-FINNS       VALUE '  '.                          
102100     88    KUNDORDER-SEK-SAKNAS      VALUE 'GE' 'GB'.                     
102200   03    STATUS-WS               PIC XX.                                  
102300     88    SEGMENT-FINNS                     VALUE '  '.                  
102400     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
102500     88    BASEN-SLUT                        VALUE 'GE'.                  
102600     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
102700     SKIP3                                                                
102800   03    STATUS-WS-WDK6          PIC XX.                                  
102900     88    SEGMENT-FINNS-WDK6                VALUE '  '.                  
103000     88    SEGMENT-SAKNAS-WDK6               VALUE 'GE'.                  
103100     88    SEGMENT-FINNS-REDAN-WDK6          VALUE 'II'.                  
103200     88    END-OF-DATABASE-WDK6              VALUE 'GB'.                  
103300     SKIP3                                                                
103400   03    STATUS-WS-WDK7          PIC XX.                                  
103500     88    SEGMENT-FINNS-WDK7                VALUE '  '.                  
103600     88    SEGMENT-SAKNAS-WDK7               VALUE 'GE'.                  
103700     88    SEGMENT-FINNS-REDAN-WDK7          VALUE 'II'.                  
103800     88    END-OF-DATABASE-WDK7              VALUE 'GB'.                  
103900     SKIP3                                                                
104000   03    GODK-STATUSKODER.                                                
104100     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
104200     SKIP3                                                                
104300 01    SSA1                      PIC X(120).                              
104400 01    SSA2                      PIC X(80).                               
104500 01    SSA3                      PIC X(80).                               
104600 01    SSA4                      PIC X(80).                               
104700     EJECT                                                                
104800*                            IMS FUNKTIONSKODER                           
104900*01    -COPY W0003                                                        
105000     EJECT                                                                
105100*                            DLI INPUT-OUTPUT AREA                        
105200 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E401'.           
105300 01    DLI-IO-E401.                                                       
105400*  03    WDE401 -COPY WDE401                                              
105500     EJECT                                                                
105600 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E411'.           
105700 01    DLI-IO-E411.                                                       
105800*  03    WDE411 -COPY WDE411                                              
105900     EJECT                                                                
106000 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E421'.           
106100 01    DLI-IO-E421.                                                       
106200*  03    WDE421 -COPY WDE421                                              
106300     EJECT                                                                
106400 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E601'.           
106500 01    DLI-IO-E601.                                                       
106600*  03    WDE601 -COPY WDE601                                              
106700     EJECT                                                                
106800 01    FILLER                    PIC X(16) VALUE 'DLI-IO-E611'.           
106900 01    DLI-IO-E611.                                                       
107000*  03    WDE611 -COPY WDE611                                              
107100     EJECT                                                                
107200 01    FILLER                    PIC X(16) VALUE 'DLI-IO-K501'.           
107300 01    DLI-IO-K501.                                                       
107400*  03    WLEMBB01 -COPY WDK501                                            
107500     EJECT                                                                
107600 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA2'.          
107700 01    DLI-IO-AREA2.                                                      
107800   03    IO-AREA2                PIC X(64)  VALUE SPACE.                  
107900*  03    WLXXDV11  -COPY WDGX4726    -RED IO-AREA2.                       
108000     EJECT                                                                
108100*  03    WLXXDV21  -COPY WDGX4727    -RED IO-AREA2.                       
108200     EJECT                                                                
108300 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA3'.          
108400 01    DLI-IO-AREA3.                                                      
108500   03    IO-AREA3                PIC X(200).                              
108600*  03    WLORQA01  -COPY WDQ301      -RED IO-AREA3.                       
108700     EJECT                                                                
108800 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA4'.          
108900 01    DLI-IO-AREA4.                                                      
109000   03    IO-AREA4                PIC X(1024).                             
109100*  03    WLXXKW11  -COPY WDGX4472    -RED IO-AREA4.                       
109200     EJECT                                                                
109300*  03    WLXXLB11  -COPY WDGX4478    -RED IO-AREA4.                       
109400     EJECT                                                                
109500 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA5'.          
109600 01    DLI-IO-AREA5.                                                      
109700   03    IO-AREA5                PIC X(192).                              
109800*  03    WDGZ01   -COPY WDGZ01 -PRE LOGG-  -RED IO-AREA5.                 
109900     EJECT                                                                
110000 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA6'.          
110100 01    DLI-IO-AREA6.                                                      
110200   03    IO-AREA6                PIC X(4000).                             
110300*  03    WDQ201   -COPY WDQ201             -RED IO-AREA6.                 
110400*  03    WDQ212   -COPY WDQ212             -RED IO-AREA6.                 
110500     EJECT                                                                
110600 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA7'.          
110700 01    DLI-IO-AREA7.                                                      
110800   03    IO-AREA7                PIC X(96) VALUE SPACE.                   
110900*  03    WLXXDU01 -COPY WDGX4301           -RED IO-AREA7.                 
111000     EJECT                                                                
111100*  03    WLXXDU11 -COPY WDGX4302           -RED IO-AREA7.                 
111200     EJECT                                                                
111300*  03    WLXXJK01 -COPY WDGX01  -PRE 4321- -RED IO-AREA7.                 
111400*  03    WLXXJK11 -COPY WDGX4322           -RED IO-AREA7.                 
111500                                                                          
111600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
111700 01   DLI-IO-AREA-B601.                                                   
111800*     03  -COPY WDB601                                                    
111900                                                                          
112000 01  FILLER               PIC X(16)   VALUE 'WDA601 AREA'.                
112100 01   DLI-IO-AREA-A601.                                                   
112200*     03  -COPY WDA601                                                    
112300 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK601'.              
112400 01    DLI-IO-WDK601.                                                     
112500*      03  -COPY WDK601                                                   
112600 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK611'.              
112700 01    DLI-IO-WDK611.                                                     
112800*      03  -COPY WDK611                                                   
112900     EJECT                                                                
113000 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK711'.              
113100 01    DLI-IO-WDK711.                                                     
113200*      03  -COPY WDK711                                                   
113300     EJECT                                                                
113400 LINKAGE SECTION.                                                         
113500*01    -COPY W0009     -PRE MSG-                                          
113600     EJECT                                                                
113700*01    -COPY W0009     -PRE ALT0605-                                      
113800     EJECT                                                                
113900*01    -COPY W0009     -PRE ALT4333-                                      
114000     EJECT                                                                
114100*01    -COPY W0009     -PRE ALT-                                          
114200     EJECT                                                                
114300*01    -COPY W0009     -PRE DISTRDOC-                                     
114400     EJECT                                                                
114500 01  TMS-CRE-PCB                 PIC X.                                   
114600 01  TMS-DEL-PCB                 PIC X.                                   
114700 01  ATAB-PCB                    PIC X.                                   
114800     EJECT                                                                
114900*01    -COPY W0008     -PRE USEA-                                         
115000     05  FILLER                  PIC X.                                   
115100     EJECT                                                                
115200*01    -COPY W0008     -PRE WDE4-                                         
115300     05  FILLER                  PIC X.                                   
115400     EJECT                                                                
115500*01    -COPY W0008     -PRE WDE6-                                         
115600     05  FILLER                  PIC X.                                   
115700     EJECT                                                                
115800*01    -COPY W0008     -PRE WDE4A-                                        
115900     05  FILLER                  PIC X.                                   
116000     EJECT                                                                
116100*01    -COPY W0008     -PRE WDE42-                                        
116200     05  FILLER                  PIC X.                                   
116300     EJECT                                                                
116400*01    -COPY W0008     -PRE WDE4B-                                        
116500     05  FILLER                  PIC X.                                   
116600     EJECT                                                                
116700*01    -COPY W0008     -PRE WDE62-                                        
116800     05  FILLER                  PIC X.                                   
116900     EJECT                                                                
117000*01    -COPY W0008     -PRE EMBB-                                         
117100     05  FILLER                  PIC X.                                   
117200     EJECT                                                                
117300*01    -COPY W0008     -PRE ZZAC-                                         
117400     05  FILLER                  PIC X.                                   
117500     EJECT                                                                
117600*01    -COPY W0008     -PRE XXJK-                                         
117700     05  FILLER                  PIC X.                                   
117800     EJECT                                                                
117900*01    -COPY W0008     -PRE XXDV-                                         
118000     05  FILLER                  PIC X.                                   
118100     EJECT                                                                
118200*01    -COPY W0008     -PRE ORQA-                                         
118300     05  FILLER                  PIC X.                                   
118400     EJECT                                                                
118500*01    -COPY W0008     -PRE XXKW-                                         
118600     05  FILLER                  PIC X.                                   
118700     EJECT                                                                
118800*01    -COPY W0008     -PRE XXLB-                                         
118900     05  FILLER                  PIC X.                                   
119000     EJECT                                                                
119100*01    -COPY W0008     -PRE ORQI-                                         
119200     05  FILLER                  PIC X.                                   
119300     EJECT                                                                
119400*01  -COPY W0008       -PRE ORQL-                                         
119500     05  FILLER                  PIC X.                                   
119600     EJECT                                                                
119700*01  -COPY W0008       -PRE XXDU-                                         
119800     05  FILLER                  PIC X.                                   
119900     EJECT                                                                
120000*01  -COPY W0008       -PRE WDB6-                                         
120100     05  FILLER                  PIC X.                                   
120200     EJECT                                                                
120300*01  -COPY W0008       -PRE WDA6B-                                        
120400     05  FILLER                  PIC X.                                   
120500*01  -COPY W0008       -PRE WDK6-                                         
120600     05  FILLER                  PIC X.                                   
120700     EJECT                                                                
120800*01  -COPY W0008       -PRE WDK7-                                         
120900     05  FILLER                  PIC X.                                   
121000     EJECT                                                                
121100                                                                          
121200 01  DNOT-ORQP-PCB               PIC X.                                   
121300 01  DNOT-ORQP2-PCB              PIC X.                                   
121400 01  DNOT-ORQP3-PCB              PIC X.                                   
121500 01  DNOT-4013-PCB               PIC X.                                   
121600 01  DNOT-BENA-PCB               PIC X.                                   
121700                                                                          
121800 01  TMS-1165-PCB                PIC X.                                   
121900 01  TMS-4141-PCB                PIC X.                                   
122000 01  TMS-WDB2-PCB                PIC X.                                   
122100 01  TMS-WDB6-PCB                PIC X.                                   
122200 01  TMS-WDD3-PCB                PIC X.                                   
122300 01  TMS-WDB1-PCB                PIC X.                                   
122400 01  TMS-WDE4A-PCB               PIC X.                                   
122500 01  TMS-WDE4F-PCB               PIC X.                                   
122600 01  TMS-WDQ2-PCB                PIC X.                                   
122700 01  TMS-WDQ3-PCB                PIC X.                                   
122800 01  TMS-WDK6-PCB                PIC X.                                   
122900 01  TMS-WDE6-PCB                PIC X.                                   
123000 01  TMS-WDK5-PCB                PIC X.                                   
123100 01  TMS-WDQ2C-PCB               PIC X.                                   
123200     EJECT                                                                
123300                                                                          
123400 PROCEDURE DIVISION USING  MSG-PCB ALT0605-PCB ALT4333-PCB ALT-PCB        
123500     DISTRDOC-PCB TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                        
123600     USEA-PCB WDE4-PCB WDE6-PCB WDE4A-PCB                                 
123700     WDE42-PCB WDE4B-PCB WDE62-PCB EMBB-PCB ZZAC-PCB                      
123800     XXJK-PCB XXDV-PCB ORQA-PCB XXKW-PCB XXLB-PCB ORQI-PCB                
123900     ORQL-PCB XXDU-PCB WDB6-PCB WDA6B-PCB                                 
124000     DNOT-ORQP-PCB                                                        
124100     DNOT-ORQP2-PCB                                                       
124200     DNOT-ORQP3-PCB                                                       
124300     DNOT-4013-PCB                                                        
124400     DNOT-BENA-PCB WDK6-PCB WDK7-PCB                                      
124500     TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB TMS-WDB6-PCB                  
124600     TMS-WDD3-PCB TMS-WDB1-PCB TMS-WDE4A-PCB TMS-WDE4F-PCB                
124700     TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                  
124800     TMS-WDK5-PCB TMS-WDQ2C-PCB.                                          
124900                                                                          
125000     ENTRY 'DLITCBL' USING MSG-PCB ALT0605-PCB ALT4333-PCB ALT-PCB        
125100     DISTRDOC-PCB TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                        
125200     USEA-PCB WDE4-PCB WDE6-PCB WDE4A-PCB                                 
125300     WDE42-PCB WDE4B-PCB WDE62-PCB EMBB-PCB ZZAC-PCB                      
125400     XXJK-PCB XXDV-PCB ORQA-PCB XXKW-PCB XXLB-PCB ORQI-PCB                
125500     ORQL-PCB XXDU-PCB WDB6-PCB WDA6B-PCB                                 
125600     DNOT-ORQP-PCB                                                        
125700     DNOT-ORQP2-PCB                                                       
125800     DNOT-ORQP3-PCB                                                       
125900     DNOT-4013-PCB                                                        
126000     DNOT-BENA-PCB WDK6-PCB WDK7-PCB                                      
126100     TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB TMS-WDB6-PCB                  
126200     TMS-WDD3-PCB TMS-WDB1-PCB TMS-WDE4A-PCB TMS-WDE4F-PCB                
126300     TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                  
126400     TMS-WDK5-PCB TMS-WDQ2C-PCB.                                          
126500*                                                                         
126600     PERFORM IMS-GET-MSG                                                  
126700     IF SEGMENT-FINNS                                                     
126800         PERFORM A-INIT                                                   
126900*                                                                         
127000         EVALUATE TRUE                                                    
127100         WHEN WS-SAMMA-BILD                                               
127200         OR   WS-ORDERVIS-TRANS                                           
127300             PERFORM B-GENERELL-KONTROLL                                  
127400*                                                                         
127500             IF WS-INDATA-RATT                                            
127600                 PERFORM C-RELATIONSKONTROLL                              
127700*                                                                         
127800                 IF WS-INDATA-RATT                                        
127900                     IF  STARTA-OM                                        
128000                         MOVE MID      TO SPAR-MID                        
128100                         MOVE MID      TO NEXT-MID                        
128200                         PERFORM K-SKAPA-NY-MID                           
128300                     END-IF                                               
128400*                                                                         
128500                     IF NYTT-KOLLI                                        
128600                        PERFORM G-SKAPA-KOLLISEG                          
128700                     END-IF                                               
128800*                                                                         
128900                     IF WS-SAMMA-BILD                                     
129000                     OR   WS-ORDERVIS-TRANS                               
129100                         MOVE +1 TO INX                                   
129200                         MOVE +1 TO LINE-IX                               
129300                         PERFORM UNTIL INX NOT < MAX-RAD-ANTAL            
129400                             PERFORM D-BEHANDLA-INTERVALL                 
129500                             ADD +1 TO INX                                
129600                         END-PERFORM                                      
129700                       IF WS-BEHANDLING-RATT                              
129800                          PERFORM DAA-KONTROLLERA-WEIGHT                  
129900                       END-IF                                             
130000                          PERFORM L-HAMTA-ADRESS                          
130100                     END-IF                                               
130200                                                                          
130300                     IF WS-BEHANDLING-RATT                                
130400                         PERFORM F-UPPDATERA-KOLLIREG                     
130500                         IF (MID-FLFORTSK = JA OR YES OR OMSTART)         
130600                         AND   MID-IDRADNR-FOM (12) = ALL '+'             
130700                         AND   NOT STARTA-OM                              
130800                         AND WS-SAVE-KOLLI-FLBANDST = NEJ                 
130900                         AND WS-SAVE-KOLLI-KDKOLSTA = ZERO                
131000                             PERFORM E-UPPDATERA-KOLLIFALT                
131100                         END-IF                                           
131200*LK TMS INFO NOT FOR NEW CASE AS 4314 DOES NOT ADD CASECODE               
131300                         IF NOT NYTT-KOLLI                                
131400                           PERFORM EB-SEND-TMS                            
131500                         END-IF                                           
131600                                                                          
131700                         IF MID-IDRADNR-FOM (12) = ALL '+'                
131800                         AND NOT STARTA-OM                                
131900                         AND WS-IDTRANS = '4314'                          
132000                         AND WS-PRT-KDSVAR-FOLJEFL = RAETT                
132100                             IF WS-SAMMA-BILD                             
132200                                 PERFORM J-SKAPA-FOLJEFL-TRANS            
132300                             END-IF                                       
132400                         END-IF                                           
132500                         PERFORM I-TAG-BORT-LAASNING                      
132600                         PERFORM S02-RENSA-MOD-FALT                       
132700                     END-IF                                               
132800                 ELSE                                                     
132900                     PERFORM S04-ADD-LAES-IN-FAELT                        
133000                 END-IF                                                   
133100             END-IF                                                       
133200             MOVE MAX-MOD-LAENGD TO  MSG-KVLL                             
133300         WHEN WS-GODKAND-BILD                                             
133400           PERFORM M-INIT-TRANS-LL92                                      
133500         WHEN OTHER                                                       
133600           PERFORM N-INIT-TRANS-LL8                                       
133700         END-EVALUATE                                                     
133800                                                                          
133900         IF WS-INDATA-RATT                                                
134000           PERFORM H-AVSLUT                                               
134100         ELSE                                                             
134200           PERFORM O-INIT-FEL-TRANS                                       
134300         END-IF                                                           
134400                                                                          
134500         PERFORM P-EVALUATE-INSERT-MSG-TRANS                              
134600     END-IF                                                               
134700     MOVE ZERO TO RETURN-CODE                                             
134800     GOBACK                                                               
134900                                                                          
135000     .                                                                    
135100     EJECT                                                                
135200 A-INIT             SECTION.                                              
135300                                                                          
135400     IF MSG-DUBBLA-TRANSKODER                                             
135500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I31401               
135600       MOVE MSG-IDTRANS-2                 TO   MFS-IDTRANS                
135700                                               WS-IDTRANS                 
135800       MOVE MSG-KDMFSFOR-2                TO   MFS-KDMFSFOR               
135900                                               WS-KDMFSFOR                
136000       MOVE MSG-KDTRTYP                   TO   MFS-KDTRTYP                
136100     ELSE                                                                 
136200       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I31401               
136300       MOVE MSG-IDTRANS-1                 TO   MFS-IDTRANS                
136400                                               WS-IDTRANS                 
136500       MOVE MSG-KDMFSFOR-1                TO   MFS-KDMFSFOR               
136600                                               WS-KDMFSFOR                
136700       MOVE ' '                           TO   MFS-KDTRTYP                
136800     END-IF                                                               
136900*                                                                         
137000     MOVE LOW-VALUE                       TO   MSG-AREA                   
137100                                               MOD-W4O31401               
137200     MOVE 'W4O314N1'                      TO   MFS-IDMOD                  
137300     MOVE '4314'                          TO   MOD-IDTRANS                
137400                                               MOD-IDTRANS-START          
137500*                                                                         
137600     IF ENGLISH-TEXT                                                      
137700         MOVE +2                          TO   INDX                       
137800     ELSE                                                                 
137900         MOVE +1                          TO   INDX                       
138000     END-IF                                                               
138100     MOVE RAETT                           TO WS-INDATA-TEST               
138200                                             WS-BEHANDLING-TEST           
138300     MOVE NEJ                             TO ENDAST-STATUS                
138400                                             WS-STARTA-OM                 
138500                                             NYA-NYCKLAR-SW               
138600                                          WS-SAVE-KOLLI-FLBANDST          
138700     PERFORM AA-FLYTTA-NYCKLAR                                            
138900     IF WS-IDTRANS = '4313'                                               
139000       MOVE SPACE               TO MOD-IDDISTR-UT                         
139100                                   MOD-IDKUNDNR-UT                        
139200                                   MOD-IDORDNR-UT                         
139300                                   MOD-IDPRODNR-UT                        
139400                                   MOD-IDKOLLI-UT                         
139500                                   MOD-IDANSTNR-UT                        
139600     END-IF                                                               
139700                                                                          
139710     INITIALIZE TMS-W403TMS1                                              
139800     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
139900     ACCEPT WS-TIDPUNKT                   FROM TIME                       
140000                                                                          
140100     MOVE WS-IDDC TO W-IDDC-B6                                            
140200                     W-IDDC-K7                                            
140300     PERFORM IMS-GU-WDB601                                                
140400     IF NOT WS-ORDERVIS-TRANS                                             
140500       IF DCS-NDC-NA OR (DCS-SDC AND DCS-IDLANDX2 = 'GB')                 
140600         MOVE ALL '+'           TO MSGI-WMSGINIT                          
140700         MOVE '011'             TO MSGI-KDCALL                            
140800         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
140900                                   MSGI-IDLTERM-USER                      
141000         MOVE WS-DAGENS-DATUM   TO MSGI-TILOKDAT                          
141100         MOVE WS-TIDPUNKT       TO MSGI-TILOKTID                          
141200         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
141300         MOVE MSGI-TILOKDAT     TO WS-DAGENS-DATUM                        
141400         MOVE MSGI-TILOKTID     TO WS-TIDPUNKT (1:4)                      
141500       END-IF                                                             
141600     END-IF                                                               
141700                                                                          
141800     MOVE MFS-RENSA-FAELT                 TO   MOD-TEMFSFEL               
141900                                               MOD-IDANSTNR-IN            
142000                                               MOD-IDDISTR-IN             
142100                                               MOD-IDKUNDNR-IN            
142200                                               MOD-IDORDNR-IN             
142300                                               MOD-IDKOLLI-IN             
142400                                               MOD-IDPRODNR-IN            
142500                                               MOD-TEMFSINF               
142600                                                                          
142700     MOVE ZERO                            TO PALLAR (1)                   
142800                                             PALLAR (2)                   
142900                                             PALLAR (3)                   
143000                                             PALLAR (4)                   
143100                                             PALLAR (5)                   
143200                                                                          
143300     MOVE ZERO                            TO KRAGAR (1)                   
143400                                             KRAGAR (2)                   
143500                                             KRAGAR (3)                   
143600                                             KRAGAR (4)                   
143700                                             KRAGAR (5)                   
143800                                                                          
143900     MOVE ZERO                            TO EMB-LOCK (1)                 
144000                                             EMB-LOCK (2)                 
144100                                             EMB-LOCK (3)                 
144200                                             EMB-LOCK (4)                 
144300                                             EMB-LOCK (5)                 
144400                                             LOGG-IDLOGLOP                
144500     .                                                                    
144600     EJECT                                                                
144700 AA-FLYTTA-NYCKLAR  SECTION.                                              
144800                                                                          
144900     IF NOT WS-ORDERVIS-TRANS                                             
145000       MOVE ALL '+'         TO MSGI-WMSGINIT                              
145100       MOVE '013'           TO MSGI-KDCALL                                
145200       MOVE '4314'          TO MSGI-IDTRANS                               
145300       MOVE MSG-LTERM-NAME  TO MSGI-IDLTERM-USER                          
145400       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
145500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
145600       MOVE MSGI-KDMATT       TO WS-KDMATT                                
145700     END-IF                                                               
145800                                                                          
145900     IF MID-IDANSTNR-IN = ALL '+'                                         
146000         MOVE MID-IDANSTNR-UT             TO   WS-IDANSTNR                
146100         INSPECT WS-IDANSTNR REPLACING LEADING SPACE BY ZERO              
146200     ELSE                                                                 
146300         MOVE MID-IDANSTNR-IN             TO   WS-IDANSTNR                
146400     END-IF                                                               
146500                                                                          
146600     IF MID-IDDISTR-IN = ALL '+'                                          
146700         MOVE MID-IDDISTR-UT              TO   WS-IDDISTR                 
146800         INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO               
146900     ELSE                                                                 
147000         MOVE JA                          TO   NYA-NYCKLAR-SW             
147100         MOVE MID-IDDISTR-IN              TO   WS-IDDISTR                 
147200     END-IF                                                               
147300                                                                          
147400     IF MID-IDKUNDNR-IN = ALL '+'                                         
147500         MOVE MID-IDKUNDNR-UT             TO   WS-IDKUNDNR                
147600         INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
147700     ELSE                                                                 
147800         MOVE JA                          TO   NYA-NYCKLAR-SW             
147900         MOVE MID-IDKUNDNR-IN             TO   WS-IDKUNDNR                
148000     END-IF                                                               
148100                                                                          
148200     IF MID-IDORDNR-IN = ALL '+'                                          
148300         MOVE MID-IDORDNR-UT              TO   WS-IDORDNR                 
148400         INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO               
148500     ELSE                                                                 
148600         MOVE JA                          TO   NYA-NYCKLAR-SW             
148700         MOVE MID-IDORDNR-IN              TO   WS-IDORDNR                 
148800     END-IF                                                               
148900                                                                          
149000     IF MID-IDKOLLI-IN = ALL '+'                                          
149100         MOVE MID-IDKOLLI-UT              TO   WS-IDKOLLI                 
149200         INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO               
149300     ELSE                                                                 
149400         MOVE MID-IDKOLLI-IN              TO   WS-IDKOLLI                 
149500     END-IF                                                               
149600                                                                          
149700     IF MID-IDPRODNR-IN = ALL '+'                                         
149800       IF NYA-NYCKLAR                                                     
149900         MOVE ZERO                        TO   WS-IDPRODNR                
150000         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
150100       ELSE                                                               
150200         MOVE MID-IDPRODNR-UT             TO   WS-IDPRODNR                
150300         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
150400       END-IF                                                             
150500     ELSE                                                                 
150600         MOVE MID-IDPRODNR-IN             TO   WS-IDPRODNR                
150700     END-IF                                                               
150800                                                                          
150900     IF WS-ORDERVIS-TRANS                                                 
151000       MOVE MID-IDDC-IN                   TO WS-IDDC                      
151100     ELSE                                                                 
151200       MOVE MSGI-IDDC                     TO WS-IDDC                      
151300     END-IF                                                               
151400                                                                          
151500     IF WS-IDDC IS > SPACE                                                
151600       CONTINUE                                                           
151700     ELSE                                                                 
151800       MOVE FEL                           TO WS-INDATA-TEST               
151900       MOVE FEL-749 (INDX)                TO MOD-TEMFSFEL                 
152000     END-IF                                                               
152100                                                                          
152200     MOVE WS-IDANSTNR                     TO   MOD-IDANSTNR-UT            
152300     INSPECT MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE              
152400     MOVE WS-IDDISTR                      TO   MOD-IDDISTR-UT             
152500     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
152600     MOVE WS-IDKUNDNR                     TO   MOD-IDKUNDNR-UT            
152700     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
152800     MOVE WS-IDORDNR                      TO   MOD-IDORDNR-UT             
152900     INSPECT MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE              
153000     MOVE WS-IDKOLLI                      TO   MOD-IDKOLLI-UT             
153100     INSPECT MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE              
153200     MOVE WS-IDPRODNR                     TO   MOD-IDPRODNR-UT            
153300     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
153400     MOVE WS-IDDC                         TO   MOD-IDDC-UT                
153500                                                                          
153600     .                                                                    
153700     EJECT                                                                
153800 B-GENERELL-KONTROLL  SECTION.                                            
153900                                                                          
154000     SKIP3                                                                
154100     IF WS-IDANSTNR NOT NUMERIC                                           
154200         MOVE FEL                       TO   WS-INDATA-TEST               
154300                                             WS-BEHANDLING-TEST           
154400         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
154500     END-IF                                                               
154600     SKIP2                                                                
154700     IF WS-IDDISTR  NOT NUMERIC                                           
154800         MOVE FEL                       TO   WS-INDATA-TEST               
154900                                             WS-BEHANDLING-TEST           
155000         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
155100     ELSE                                                                 
155200       MOVE WS-IDDISTR                  TO  WS-IDDISTR-NUM                
155300       MOVE WS-IDDISTR-NUM              TO  TEST-IDDISTR                  
155400* * * SATSORDER EJ TILLÅTNA ANNAT ÄN FRÅN BILD 4303 * * *                 
155500       IF DIST19-SATS AND NOT WS-ORDERVIS-TRANS                           
155600           MOVE FEL                     TO   WS-INDATA-TEST               
155700                                             WS-BEHANDLING-TEST           
155800           MOVE FEL-749 (INDX)          TO   MOD-TEMFSFEL                 
155900       ELSE                                                               
156000           MOVE WS-IDDISTR              TO   W-4A1-IDDISTR                
156100       END-IF                                                             
156200* * * * * * * * * * * * * * * * *                                         
156300     END-IF                                                               
156400     SKIP2                                                                
156500     IF WS-IDKUNDNR NOT NUMERIC                                           
156600         MOVE FEL                       TO   WS-INDATA-TEST               
156700                                             WS-BEHANDLING-TEST           
156800         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
156900     ELSE                                                                 
157000         MOVE WS-IDKUNDNR               TO   W-4A1-IDKUNDNR               
157100     END-IF                                                               
157200     SKIP2                                                                
157300     IF WS-IDORDNR  NOT NUMERIC                                           
157400         MOVE FEL                       TO   WS-INDATA-TEST               
157500                                             WS-BEHANDLING-TEST           
157600         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
157700     ELSE                                                                 
157800         MOVE WS-IDORDNR                TO   W-4A1-IDORDNR                
157900     END-IF                                                               
158000     SKIP2                                                                
158100     IF WS-IDKOLLI NOT NUMERIC                                            
158200         MOVE FEL                       TO   WS-INDATA-TEST               
158300                                             WS-BEHANDLING-TEST           
158400         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
158500     END-IF                                                               
158600     SKIP2                                                                
158700     IF WS-IDPRODNR NOT NUMERIC                                           
158800         MOVE FEL                       TO   WS-INDATA-TEST               
158900                                             WS-BEHANDLING-TEST           
159000         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
159100     END-IF                                                               
159200******FIX 930902 P.G.A. ATT MAN INTE HAR AVSLUTAT 4314-BILDEN MED         
159300***** ENTER PÅ KORREKT SÄTT EFTER ATT FYLLT I ALLA                        
159400***** RADER PÅ 4315-BILDEN OCH FÅTT UPP 4314-BILDEN.                      
159500*    IF MSG-SIGNON-USERID = 'R037556 ' AND                                
159600*       WS-IDPRODNR = '0195897' AND                                       
159700*       WS-IDKOLLI  = '00007'                                             
159800*      MOVE WS-IDPRODNR             TO  W-601-IDPRODNR                    
159900*      PERFORM IMS-GHU-KOLLIREG                                           
160000*      ADD +1                       TO  VORD-KVKOLLI                      
160100*      ADD 0                        TO  VORD-VKORDBTO                     
160200*      ADD 2.004                    TO  VORD-VLORDBTO                     
160300*      ADD 10342.93                 TO  VORD-SUORDV-PACK                  
160400*      ADD 10342.93                 TO  VORD-SUORDV-PACK-LOC              
160500*      ADD 10342.93                 TO  VORD-SUORDV-PACK-LOCPREL          
160600*      MOVE WS-DAGENS-DATUM         TO  VORD-TIPACKN-SK                   
160700*      PERFORM IMS-REPL-KOLLIREG                                          
160800*      MOVE WS-IDKOLLI              TO  W-610-IDKOLLI                     
160900*      PERFORM IMS-GHU-KOLLI                                              
161000*      MOVE 1                       TO  KOLLI-KDKOLSTA                    
161100*      MOVE 930827                  TO  KOLLI-TIPACKN                     
161200*      MOVE WS-TIDPUNKT             TO  WS-TIDPUNKT-RED                   
161300*      MOVE WS-HHMMSS               TO  KOLLI-TIPACTID                    
161400*      PERFORM IMS-REPL-KOLLI                                             
161500*    END-IF                                                               
161600******SLUT FIX                                                            
161700*                                                                         
161800     MOVE +1                        TO INX                                
161900     MOVE ZERO                      TO RAD-INX                            
162000     PERFORM UNTIL INX NOT < MAX-RAD-ANTAL                                
162100         MOVE MFS-ROER-EJ-FAELT     TO MOD-IDRADNR-FOM (INX)              
162200                                       MOD-IDRADNR-TOM (INX)              
162300                                       MOD-KVLEVART (INX)                 
162400         PERFORM BA-KONTROLLERA-RAD                                       
162500         ADD +1                     TO INX                                
162600     END-PERFORM                                                          
162700     COMPUTE MAX-RAD-ANTAL = RAD-INX + 1                                  
162800                                                                          
162900     IF WS-INDATA-RATT                                                    
163000                                                                          
163100        EVALUATE TRUE                                                     
163200        WHEN WS-ANT-RAD-I-BILD > WS-MAX-ANT-FOR-OMST                      
163300        AND  WS-SAMMA-BILD                                                
163400            MOVE JA             TO WS-STARTA-OM                           
163500        WHEN WS-ANT-RAD-I-BILD > WS-MAX-ANT-RAD-I-BILD                    
163600        AND  NOT WS-SAMMA-BILD                                            
163700            MOVE FEL            TO WS-INDATA-TEST                         
163800                                   WS-BEHANDLING-TEST                     
163900            MOVE FEL-826 (INDX) TO MOD-TEMFSFEL                           
164000        END-EVALUATE                                                      
164100     END-IF                                                               
164200                                                                          
164300     SKIP2                                                                
164400     IF MID-VKORDBTO-KOLLI  = ALL '+'                                     
164500         MOVE MFS-RENSA-FAELT             TO  MOD-VKORDBTO-KOLLI          
164600     ELSE                                                                 
164700         MOVE MFS-ROER-EJ-FAELT           TO  MOD-VKORDBTO-KOLLI          
164800         MOVE MFS-NUM-FAELT-RAETT         TO  MOD-VKORDBTO-ATTR           
164900     END-IF                                                               
165000*LK                                                                       
165100     IF MID-VKORDBTO-4315   = ALL '+'                                     
165200         MOVE MFS-RENSA-FAELT             TO  MOD-VKORDBTO-4315           
165300     ELSE                                                                 
165400         MOVE MFS-ROER-EJ-FAELT           TO  MOD-VKORDBTO-4315           
165500         MOVE MFS-CLOSE-NONDISP-FIELD  TO MOD-VKORDBTO-4315-ATTR          
165600     END-IF                                                               
165700*LK                                                                       
165800     IF MID-FLFORTSK    = ALL '+'                                         
165900         MOVE MFS-RENSA-FAELT             TO  MOD-FLFORTSK                
166000     ELSE                                                                 
166100         MOVE MFS-ROER-EJ-FAELT           TO  MOD-FLFORTSK                
166200                                                                          
166300         IF  MID-FLFORTSK = 'J' OR 'N' OR 'O' OR 'Y'                      
166400             MOVE MFS-ALFA-FAELT-RAETT    TO  MOD-FLFORTSK-ATTR           
166500         ELSE                                                             
166600             MOVE FEL                     TO WS-INDATA-TEST               
166700                                             WS-BEHANDLING-TEST           
166800             MOVE FEL-748 (INDX)          TO MOD-TEMFSFEL                 
166900             MOVE MFS-ALFA-FAELT-FEL      TO  MOD-FLFORTSK-ATTR           
167000         END-IF                                                           
167100     END-IF                                                               
167200                                                                          
167300     IF  WS-ORDERVIS-TRANS                                                
167400         IF MID-FLSISTAK    = ALL '+'                                     
167500             MOVE MFS-RENSA-FAELT         TO  MOD-FLSISTAK                
167600         ELSE                                                             
167700             MOVE MFS-ROER-EJ-FAELT       TO  MOD-FLSISTAK                
167800             IF  MID-FLSISTAK = 'J' OR 'N' OR 'Y'                         
167900                MOVE MFS-ALFA-FAELT-RAETT TO  MOD-FLSISTAK-ATTR           
168000             ELSE                                                         
168100                 MOVE FEL                 TO WS-INDATA-TEST               
168200                                             WS-BEHANDLING-TEST           
168300                 MOVE FEL-748 (INDX)      TO MOD-TEMFSFEL                 
168400                 MOVE MFS-ALFA-FAELT-FEL  TO  MOD-FLSISTAK-ATTR           
168500             END-IF                                                       
168600         END-IF                                                           
168700     ELSE                                                                 
168800         MOVE MFS-ROER-EJ-FAELT           TO  MOD-FLSISTAK                
168900         IF  MID-FLSISTAK = 'J' OR 'N' OR 'Y'                             
169000             MOVE MFS-ALFA-FAELT-RAETT    TO  MOD-FLSISTAK-ATTR           
169100         ELSE                                                             
169200             MOVE FEL                     TO WS-INDATA-TEST               
169300                                             WS-BEHANDLING-TEST           
169400             MOVE FEL-748 (INDX)          TO MOD-TEMFSFEL                 
169500             MOVE MFS-ALFA-FAELT-FEL      TO  MOD-FLSISTAK-ATTR           
169600         END-IF                                                           
169700     END-IF                                                               
169800*ADRESSFLAGGA                                                             
169900                                                                          
170000     IF MID-KDPRTVAL-ADRESSFL NOT = ALL '+'  OR                           
170100        MID-KDPRTVAL-ADRESSFL NOT = 'UU'                                  
170200       MOVE '4'                   TO WS-SYSTDEL                           
170300       MOVE 'KF'                  TO WS-LISTTYP                           
170400       MOVE WS-IDDC               TO WS-DC                                
170500       MOVE MID-KDPRTVAL-ADRESSFL TO WS-KDPRT                             
170600                                                                          
170700       MOVE 001                   TO PRT-KDCALL                           
170800       MOVE WS-IDPRTLST           TO PRT-IDPRTLST                         
170900                                                                          
171000       CALL W006PRT USING PRT-W006PRT                                     
171100                                                                          
171200       IF PRT-KDSVAR = RAETT                                              
171300         IF WS-SAMMA-BILD                                                 
171400         MOVE MID-KDPRTVAL-ADRESSFL TO MOD-KDPRTVAL-ADRESSFL              
171500                                       WS-KDPRTVAL-ADRESSFL               
171600         ELSE                                                             
171700           MOVE 'UU'              TO MOD-KDPRTVAL-ADRESSFL                
171800                                       WS-KDPRTVAL-ADRESSFL               
171900         END-IF                                                           
172000       ELSE                                                               
172100         MOVE 'UU'                TO MOD-KDPRTVAL-ADRESSFL                
172200                                       WS-KDPRTVAL-ADRESSFL               
172300       END-IF                                                             
172400       MOVE PRT-KDSVAR            TO WS-PRT-KDSVAR-ADRESSFL               
172500     END-IF                                                               
172600                                                                          
172700*FÖLJESEDEL                                                               
172800     IF ENGLISH-TEXT                                                      
172900       IF MID-KDPRTVAL-FOLJEFL = 'UU'                                     
173000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-FOLJEFL-ATTR           
173100         MOVE 'UU'                 TO MOD-KDPRTVAL-FOLJEFL                
173200       ELSE                                                               
173300         MOVE '4'                 TO WS-SYSTDEL                           
173400         MOVE 'FS'                TO WS-LISTTYP                           
173500         MOVE WS-IDDC             TO WS-DC                                
173600         MOVE MID-KDPRTVAL-FOLJEFL TO WS-KDPRT                            
173700                                                                          
173800         MOVE 001                 TO PRT-KDCALL                           
173900         MOVE WS-IDPRTLST         TO PRT-IDPRTLST                         
174000                                                                          
174100         CALL W006PRT USING PRT-W006PRT                                   
174200                                                                          
174300         IF PRT-KDSVAR = RAETT                                            
174400           IF WS-SAMMA-BILD                                               
174500           MOVE MID-KDPRTVAL-FOLJEFL TO MOD-KDPRTVAL-FOLJEFL              
174600                                         WS-KDPRTVAL-FOLJEFL              
174700           ELSE                                                           
174800             MOVE 'UU'            TO MOD-KDPRTVAL-FOLJEFL                 
174900                                         WS-KDPRTVAL-FOLJEFL              
175000           END-IF                                                         
175100         ELSE                                                             
175200           MOVE 'UU'              TO MOD-KDPRTVAL-FOLJEFL                 
175300                                         WS-KDPRTVAL-FOLJEFL              
175400         END-IF                                                           
175500         MOVE PRT-KDSVAR          TO WS-PRT-KDSVAR-FOLJEFL                
175600       END-IF                                                             
175700                                                                          
175800     ELSE                                                                 
175900       IF MID-KDPRTVAL-FOLJEFL = ALL '+'                                  
176000           MOVE 'UU'                 TO WS-PRT-KDSVAR-FOLJEFL             
176100                                        MOD-KDPRTVAL-FOLJEFL              
176200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-FOLJEFL-ATTR         
176300       ELSE                                                               
176400                                                                          
176500         IF MID-KDPRTVAL-FOLJEFL = 'UU'                                   
176600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-FOLJEFL-ATTR         
176700           MOVE 'UU'                 TO MOD-KDPRTVAL-FOLJEFL              
176800         ELSE                                                             
176900           MOVE '4'                  TO WS-SYSTDEL                        
177000           MOVE 'FS'                 TO WS-LISTTYP                        
177100           MOVE WS-IDDC              TO WS-DC                             
177200           MOVE MID-KDPRTVAL-FOLJEFL TO WS-KDPRT                          
177300                                        MOD-KDPRTVAL-FOLJEFL              
177400                                        WS-KDPRTVAL-FOLJEFL               
177500                                                                          
177600           MOVE 001                  TO PRT-KDCALL                        
177700           MOVE WS-IDPRTLST          TO PRT-IDPRTLST                      
177800                                                                          
177900           CALL W006PRT USING PRT-W006PRT                                 
178000                                                                          
178100           IF PRT-KDSVAR = RAETT                                          
178200             MOVE PRT-KDSVAR         TO WS-PRT-KDSVAR-FOLJEFL             
178300            MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-FOLJEFL-ATTR        
178400           ELSE                                                           
178500             IF WS-INDATA-RATT                                            
178600               MOVE FEL              TO WS-INDATA-TEST                    
178700               MOVE FEL-772 (INDX)   TO MOD-TEMFSFEL                      
178800              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRTVAL-FOLJEFL-ATTR        
178900             END-IF                                                       
179000           END-IF                                                         
179100           MOVE MFS-ROER-EJ-FAELT    TO MOD-KDPRTVAL-FOLJEFL              
179200         END-IF                                                           
179300       END-IF                                                             
179400     END-IF                                                               
179500     .                                                                    
179600     EJECT                                                                
179700 BA-KONTROLLERA-RAD     SECTION.                                          
179800     SKIP3                                                                
179900     IF MID-IDRADNR-FOM (INX) = ALL '+'                                   
180000         IF MID-IDRADNR-TOM (INX) = ALL '+'                               
180100             MOVE MFS-RENSA-FAELT      TO MOD-IDRADNR-FOM (INX)           
180200                                          MOD-IDRADNR-TOM (INX)           
180300                                          MOD-KVLEVART (INX)              
180400             IF INX = +1                                                  
180500                                                                          
180600                 IF MID-FLFORTSK = JA  OR YES OR OMSTART                  
180700                     MOVE JA              TO ENDAST-STATUS                
180800                 ELSE                                                     
180900                     MOVE UPPLYSNING-1 (INDX) TO MOD-TEMFSFEL             
181000                     MOVE FEL             TO  WS-INDATA-TEST              
181100                 END-IF                                                   
181200             END-IF                                                       
181300             MOVE +13                     TO  INX                         
181400         ELSE                                                             
181500             MOVE FEL                     TO  WS-INDATA-TEST              
181600                                              WS-BEHANDLING-TEST          
181700             MOVE FEL-738 (INDX)          TO  MOD-TEMFSFEL                
181800             MOVE MFS-NUM-FAELT-FEL       TO                              
181900                                 MOD-IDRADNR-FOM-ATTR (INX)               
182000                                 MOD-IDRADNR-TOM-ATTR (INX)               
182100                                 MOD-KVLEVART-ATTR (INX)                  
182200         END-IF                                                           
182300     ELSE                                                                 
182400         IF MID-IDRADNR-FOM (INX) NUMERIC                                 
182500             IF MID-IDRADNR-FOM (INX) > ZERO                              
182600                 MOVE MFS-NUM-FAELT-RAETT TO                              
182700                                 MOD-IDRADNR-FOM-ATTR (INX)               
182800             ELSE                                                         
182900                 MOVE FEL               TO WS-INDATA-TEST                 
183000                                           WS-BEHANDLING-TEST             
183100                 MOVE FEL-738 (INX)     TO MOD-TEMFSFEL                   
183200                 MOVE MFS-NUM-FAELT-FEL TO                                
183300                                 MOD-IDRADNR-FOM-ATTR (INX)               
183400             END-IF                                                       
183500             IF MID-IDRADNR-TOM (INX) = ALL '+'                           
183600                 MOVE ZERO             TO MID-IDRADNR-TOM (INX)           
183700                 MOVE MFS-RENSA-FAELT  TO MOD-IDRADNR-TOM (INX)           
183800                 ADD +1                TO WS-ANT-RAD-I-BILD               
183900             ELSE                                                         
184000                 IF MID-IDRADNR-TOM (INX) NUMERIC                         
184100                     IF MID-IDRADNR-FOM (INX)                             
184200                                         < MID-IDRADNR-TOM (INX)          
184300                         MOVE MFS-NUM-FAELT-RAETT TO                      
184400                                 MOD-IDRADNR-TOM-ATTR (INX)               
184500                         MOVE MID-IDRADNR-FOM (INX) TO WS-RAD-FOM         
184600                         MOVE MID-IDRADNR-TOM (INX) TO WS-RAD-TOM         
184700                         COMPUTE WS-ANT-RAD-I-BILD =                      
184800                                 WS-ANT-RAD-I-BILD +                      
184900                                 WS-RAD-TOM        -                      
185000                                 WS-RAD-FOM        + 1                    
185100                     ELSE                                                 
185200                         MOVE FEL         TO  WS-INDATA-TEST              
185300                                              WS-BEHANDLING-TEST          
185400                         MOVE FEL-738 (INDX) TO  MOD-TEMFSFEL             
185500                         MOVE MFS-NUM-FAELT-FEL TO                        
185600                                 MOD-IDRADNR-FOM-ATTR (INX)               
185700                                 MOD-IDRADNR-TOM-ATTR (INX)               
185800                     END-IF                                               
185900                 ELSE                                                     
186000                     MOVE FEL             TO  WS-INDATA-TEST              
186100                                              WS-BEHANDLING-TEST          
186200                     MOVE FEL-748 (INDX)  TO  MOD-TEMFSFEL                
186300                     MOVE MFS-NUM-FAELT-FEL TO                            
186400                                       MOD-IDRADNR-TOM-ATTR (INX)         
186500                 END-IF                                                   
186600             END-IF                                                       
186700         ELSE                                                             
186800             MOVE FEL                     TO  WS-INDATA-TEST              
186900                                              WS-BEHANDLING-TEST          
187000             MOVE FEL-748 (INDX)          TO  MOD-TEMFSFEL                
187100             MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-FOM-ATTR (INX)         
187200                                       MOD-IDRADNR-TOM-ATTR (INX)         
187300         END-IF                                                           
187400         ADD +1  TO  RAD-INX                                              
187500     END-IF                                                               
187600     SKIP2                                                                
187700     IF INX = 13                                                          
187800*        << KOLLA RESTEN AV BILDEN   >>                                   
187900         ADD RAD-INX +1                   GIVING REST-INX                 
188000         PERFORM UNTIL REST-INX NOT < MAX-RAD-ANTAL                       
188100             IF  MID-IDRADNR-FOM (REST-INX) NOT = ALL '+'                 
188200                 MOVE MFS-ROER-EJ-FAELT                                   
188300                          TO MOD-IDRADNR-FOM (REST-INX)                   
188400                 MOVE MFS-NUM-FAELT-FEL                                   
188500                          TO MOD-IDRADNR-FOM-ATTR (REST-INX)              
188600                 MOVE FEL TO WS-INDATA-TEST                               
188700                 MOVE FEL-748 (INDX)                                      
188800                          TO MOD-TEMFSFEL                                 
188900             ELSE                                                         
189000                 MOVE MFS-RENSA-FAELT                                     
189100                          TO MOD-IDRADNR-FOM (REST-INX)                   
189200                 MOVE MFS-NUM-FAELT-RAETT                                 
189300                          TO MOD-IDRADNR-FOM-ATTR (REST-INX)              
189400             END-IF                                                       
189500             IF  MID-IDRADNR-TOM (REST-INX) NOT = ALL '+'                 
189600                 MOVE MFS-ROER-EJ-FAELT                                   
189700                          TO MOD-IDRADNR-TOM (REST-INX)                   
189800                 MOVE MFS-NUM-FAELT-FEL                                   
189900                          TO MOD-IDRADNR-TOM-ATTR (REST-INX)              
190000                 MOVE FEL TO WS-INDATA-TEST                               
190100                 MOVE FEL-748 (INDX)                                      
190200                          TO MOD-TEMFSFEL                                 
190300             ELSE                                                         
190400                 MOVE MFS-RENSA-FAELT                                     
190500                          TO MOD-IDRADNR-TOM (REST-INX)                   
190600                 MOVE MFS-NUM-FAELT-RAETT                                 
190700                          TO MOD-IDRADNR-TOM-ATTR (REST-INX)              
190800             END-IF                                                       
190900             IF  MID-KVLEVART (REST-INX) NOT = ALL '+'                    
191000                 MOVE MFS-ROER-EJ-FAELT                                   
191100                          TO MOD-KVLEVART (REST-INX)                      
191200                 MOVE MFS-NUM-FAELT-FEL                                   
191300                          TO MOD-KVLEVART-ATTR (REST-INX)                 
191400                 MOVE FEL TO WS-INDATA-TEST                               
191500                 MOVE FEL-748 (INDX)                                      
191600                          TO MOD-TEMFSFEL                                 
191700             ELSE                                                         
191800                 MOVE MFS-RENSA-FAELT                                     
191900                          TO MOD-KVLEVART (REST-INX)                      
192000                 MOVE MFS-NUM-FAELT-RAETT                                 
192100                          TO MOD-KVLEVART-ATTR (REST-INX)                 
192200             END-IF                                                       
192300             ADD +1       TO REST-INX                                     
192400         END-PERFORM                                                      
192500     ELSE                                                                 
192600         IF MID-KVLEVART (INX) = ALL '+'                                  
192700             MOVE MFS-RENSA-FAELT         TO MOD-KVLEVART (INX)           
192800         ELSE                                                             
192900             IF MID-KVLEVART (INX) NUMERIC                                
193000                 IF MID-KVLEVART (INX) = ZERO                             
193100*----------------------------------------------NOLLADE AVVIKELSER         
193200*----------------------------------------------GODKÄNNAS SOM              
193300*----------------------------------------------ORAPPORTERAD RAD           
193400*----------------------------------------------PÅ ANNAN BILD              
193500                     MOVE FEL             TO  WS-INDATA-TEST              
193600                                              WS-BEHANDLING-TEST          
193700                     MOVE FEL-724 (INDX)  TO  MOD-TEMFSFEL                
193800                     MOVE MFS-NUM-FAELT-FEL TO                            
193900                                          MOD-KVLEVART-ATTR (INX)         
194000                 ELSE                                                     
194100                     IF  MID-IDRADNR-TOM (INX) NOT = ALL '+'              
194200                     AND MID-IDRADNR-TOM (INX) NOT = ZERO                 
194300                         MOVE FEL         TO  WS-INDATA-TEST              
194400                                              WS-BEHANDLING-TEST          
194500                         MOVE FEL-748 (INDX)  TO  MOD-TEMFSFEL            
194600                         MOVE MFS-NUM-FAELT-FEL TO                        
194700                                          MOD-KVLEVART-ATTR (INX)         
194800                     ELSE                                                 
194900                         MOVE MFS-NUM-FAELT-RAETT TO                      
195000                                          MOD-KVLEVART-ATTR (INX)         
195100                     END-IF                                               
195200                 END-IF                                                   
195300             ELSE                                                         
195400                 MOVE FEL                 TO  WS-INDATA-TEST              
195500                                              WS-BEHANDLING-TEST          
195600                 MOVE FEL-748 (INDX)      TO  MOD-TEMFSFEL                
195700                 MOVE MFS-NUM-FAELT-FEL   TO                              
195800                                          MOD-KVLEVART-ATTR (INX)         
195900             END-IF                                                       
196000         END-IF                                                           
196100     END-IF                                                               
196200                                                                          
196300     .                                                                    
196400     EJECT                                                                
196500 C-RELATIONSKONTROLL  SECTION.                                            
196600                                                                          
196700     SKIP3                                                                
196800*    MOVE WS-IDDISTR                TO  WS-IDDISTR-NUM                    
196900     MOVE WS-IDKUNDNR               TO  WS-IDKUNDNR-NUM                   
197000*                                                                         
197100     IF WS-SAMMA-BILD                                                     
197200         PERFORM CA-KONTROLLERA-KUNDORDNR                                 
197300         IF WS-INDATA-RATT                                                
197400             PERFORM CB-KONTROLLERA-KOLLI                                 
197500             IF WS-INDATA-RATT                                            
197600                 PERFORM CC-KONTROLLERA-PACKARE                           
197700             END-IF                                                       
197800         END-IF                                                           
197900     ELSE                                                                 
198000         MOVE WS-IDPRODNR               TO  W-601-IDPRODNR                
198100         PERFORM CB-KONTROLLERA-KOLLI                                     
198200*        MOVE WS-IDDISTR                TO  WS-IDDISTR-NUM                
198300*        MOVE WS-IDKUNDNR               TO  WS-IDKUNDNR-NUM               
198400     END-IF                                                               
198500                                                                          
198600     .                                                                    
198700     EJECT                                                                
198800 CA-KONTROLLERA-KUNDORDNR SECTION.                                        
198900                                                                          
199000     IF MID-IDPRODNR-IN = ALL '+'                                         
199100         IF    MID-IDDISTR-IN  = ALL '+'                                  
199200           AND MID-IDKUNDNR-IN = ALL '+'                                  
199300           AND MID-IDORDNR-IN  = ALL '+'                                  
199400             IF WS-IDPRODNR > ZERO                                        
199500*                << ANVÄNDS GAMLA PRODNR: MID-IDPRODNR-UT >>              
199600                 MOVE JA              TO  SOEK-VIA-PRODNR                 
199700                 MOVE MFS-RENSA-FAELT TO  MOD-IDDISTR-UT                  
199800                                          MOD-IDKUNDNR-UT                 
199900                                          MOD-IDORDNR-UT                  
200000             ELSE                                                         
200100                 PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                        
200200                 MOVE MFS-RENSA-FAELT TO  MOD-IDPRODNR-UT                 
200300             END-IF                                                       
200400         ELSE                                                             
200500             PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                            
200600             MOVE MFS-RENSA-FAELT     TO  MOD-IDPRODNR-UT                 
200700         END-IF                                                           
200800     ELSE                                                                 
200900         MOVE JA                       TO SOEK-VIA-PRODNR                 
201000         MOVE MFS-RENSA-FAELT          TO MOD-IDDISTR-UT                  
201100                                          MOD-IDKUNDNR-UT                 
201200                                          MOD-IDORDNR-UT                  
201300     END-IF                                                               
201400     SKIP2                                                                
201500     IF WS-INDATA-RATT                                                    
201600         MOVE WS-IDPRODNR               TO   W-601-IDPRODNR               
201700         PERFORM CAB-HAMTA-INFO-WDE401                                    
201800     END-IF                                                               
201900                                                                          
202000     .                                                                    
202100     EJECT                                                                
202200 CAA-HAMTA-PRODNR-I-WDE4-6  SECTION.                                      
202300*                                                                         
202400     MOVE WS-IDDISTR-NUM   TO W-4A1-IDDISTR                               
202500     MOVE WS-IDKUNDNR-NUM  TO W-4A1-IDKUNDNR                              
202600     MOVE WS-IDORDNR       TO W-4A1-IDORDNR                               
202700                                                                          
202800     PERFORM IMS-GU-KUNDORDER-SEK                                         
202900                                                                          
203000     IF KUNDORDER-SEK-FINNS                                               
203100       PERFORM UNTIL (KORD-IDDC = WS-IDDC AND                             
203200         KORD-KVORDRAD-LEVPL = ZERO) OR KUNDORDER-SEK-SAKNAS              
203300          PERFORM IMS-GN-SEQA-WDE4A1                                      
203400       END-PERFORM                                                        
203500                                                                          
203600       IF KUNDORDER-SEK-FINNS                                             
203700         MOVE KORD-IDPRODNR TO W-601-IDPRODNR                             
203800                               WS-IDPRODNR                                
203900         PERFORM IMS-GU-WDE601                                            
204000         IF SEGMENT-FINNS                                                 
204100           CONTINUE                                                       
204200         ELSE                                                             
204300           MOVE FEL                       TO   WS-INDATA-TEST             
204400           MOVE FEL-7011 (INDX)           TO   MOD-TEMFSFEL               
204500         END-IF                                                           
204600       ELSE                                                               
204700         MOVE FEL                          TO   WS-INDATA-TEST            
204800         MOVE FEL-7012 (INDX)              TO   MOD-TEMFSFEL              
204900       END-IF                                                             
205000     ELSE                                                                 
205100       MOVE FEL                          TO   WS-INDATA-TEST              
205200       MOVE FEL-7013 (INDX)              TO   MOD-TEMFSFEL                
205300     END-IF                                                               
205400     .                                                                    
205500     EJECT                                                                
205600 CAB-HAMTA-INFO-WDE401      SECTION.                                      
205700                                                                          
205800     MOVE WS-IDPRODNR        TO   W-420-IDPRODNR-MIN                      
205900                                  W-420-IDPRODNR-MAX                      
206000                                  W-420-IDPRODNR                          
206100     MOVE 1                  TO   W-420-IDPURAD-MIN                       
206200                                  W-420-IDPURAD                           
206300     MOVE 99999              TO   W-420-IDPURAD-MAX                       
206400     PERFORM IMS-GU-KUNDORDER-SEK-INV                                     
206500     IF SEGMENT-FINNS                                                     
206600        MOVE KORD-IDDISTR      TO   WS-IDDISTR-NUM                        
206700        MOVE KORD-IDKUNDNR     TO   WS-IDKUNDNR-NUM                       
206800        MOVE KORD-IDKUNDRF     TO   WS-IDKUNDRF                           
206900     ELSE                                                                 
207000*-----------------------------------------------ÄR ORDERN                 
207100*-----------------------------------------------SAKNAD                    
207200        MOVE FEL             TO   WS-INDATA-TEST                          
207300        MOVE FEL-7014 (INDX)  TO   MOD-TEMFSFEL                           
207400     END-IF                                                               
207500     .                                                                    
207600     EJECT                                                                
207700 CB-KONTROLLERA-KOLLI   SECTION.                                          
207800                                                                          
207900     PERFORM IMS-GU-KOLLIREG                                              
208000*                                                                         
208100     IF VORD-IDDC NOT = WS-IDDC                                           
208200*---------------------------------------------FELAKTIGT                   
208300*---------------------------------------------CLAGER                      
208400        MOVE FEL                         TO   WS-INDATA-TEST              
208500        MOVE FEL-7015 (INDX)              TO   MOD-TEMFSFEL               
208600     ELSE                                                                 
208700*                                                                         
208800     IF VORD-KDORDSTA > 2                                                 
208900*---------------------------------------------ÄR ORDER REDAN              
209000*---------------------------------------------PACKAD                      
209100        MOVE FEL                         TO   WS-INDATA-TEST              
209200        MOVE FEL-718 (INDX)              TO   MOD-TEMFSFEL                
209300     ELSE                                                                 
209400*DIRLEV DC11                                                              
209500     IF VORD-IDDC NOT = W-IDDC-B6                                         
209600        MOVE VORD-IDDC TO W-IDDC-B6                                       
209700        PERFORM IMS-GU-WDB601                                             
209800     END-IF                                                               
209900     IF VORD-FLDIRLEV = NEJ OR DCS-CDC                                    
210000       CONTINUE                                                           
210100     ELSE                                                                 
210200       IF WS-INDATA-RATT                                                  
210300         MOVE FEL-800 (INDX)     TO MOD-TEMFSFEL                          
210400         MOVE FEL                TO WS-INDATA-TEST                        
210500       END-IF                                                             
210600     END-IF                                                               
210700     IF MID-FLFORTSK = JA OR YES OR OMSTART                               
210800         MOVE WS-IDKOLLI                 TO   W-421-IDKOLLI               
210900                                              W-610-IDKOLLI               
211000*                                                                         
211100         PERFORM IMS-GHU-KOLLI                                            
211200*                                                                         
211300         IF SEGMENT-FINNS                                                 
211400             IF KOLLI-KDKOLSTA           =    4 OR 5 OR 6                 
211500                                           OR 7 OR 8 OR 9                 
211600*---------------------------------------------ÄR KOLLIT FAKTURERAT        
211700*---------------------------------------------EL. FAKTURA-RELEASAT        
211800                 MOVE FEL                TO   WS-INDATA-TEST              
211900                 MOVE FEL-737 (INDX)     TO   MOD-TEMFSFEL                
212000             ELSE                                                         
212100                 move KOLLI-KDKOLLI      TO   WS-KDKOLLI                  
212200                 MOVE KOLLI-KDKOLSTA     TO   WS-KDKOLSTA                 
212300                 MOVE KOLLI-IDKOLLI-SAMP TO   WS-IDKOLLI-SAMP             
212400             END-IF                                                       
212500         ELSE                                                             
212600             MOVE FEL                    TO   WS-INDATA-TEST              
212700             MOVE FEL-717 (INDX)         TO   MOD-TEMFSFEL                
212800         END-IF                                                           
212900     ELSE                                                                 
213000         MOVE JA                         TO   WS-NYTT-KOLLI               
213100     END-IF                                                               
213200     END-IF                                                               
213300     END-IF                                                               
213400*                                                                         
213500     IF WS-IDKOLLI                       =    ZERO                        
213600         MOVE FEL                        TO   WS-INDATA-TEST              
213700         MOVE FEL-749 (INDX)             TO   MOD-TEMFSFEL                
213800     END-IF                                                               
213900                                                                          
214000     .                                                                    
214100     EJECT                                                                
214200 CC-KONTROLLERA-PACKARE SECTION.                                          
214300*                                                                         
214400     MOVE NEJ TO WS-TRAEFF-PACKARE                                        
214500     MOVE JA  TO WS-PACKARES-ODEL-REDAN-KLARA                             
214600*                                                                         
214700     MOVE WS-IDDISTR-NUM  TO W-4A1-IDDISTR                                
214800     MOVE WS-IDKUNDNR-NUM TO W-4A1-IDKUNDNR                               
214900     MOVE WS-IDORDNR      TO W-4A1-IDORDNR                                
215000     PERFORM IMS-GU-KUNDORDER-SEK                                         
215100*                                                                         
215200     IF KUNDORDER-SEK-FINNS                                               
215300        PERFORM UNTIL KUNDORDER-SEK-SAKNAS                                
215400        MOVE KORD-IDDISTR  TO W-401-IDDISTR                               
215500        MOVE KORD-IDKUNDNR TO W-401-IDKUNDNR                              
215600        MOVE KORD-IDORDNR5 TO W-401-IDORDNR                               
215700        MOVE KORD-IDPRODNR TO W-401-IDPRODNR                              
215800        MOVE KORD-IDPLKLST TO W-401-IDPLKLST                              
215900        MOVE KORD-IDORDER  TO WS-KORD-IDORDER                             
216000        PERFORM IMS-GU-KUNDORDER                                          
216100        MOVE KORD-IDPRODNR TO WS-JFR-IDPRODNR                             
216200        MOVE KORD-IDUSER   TO WS-JFR-IDANSTNR                             
216300        IF NOT WS-ORDERVIS-TRANS AND                                      
216400           WS-IDPRODNR = WS-JFR-IDPRODNR AND                              
216500           WS-IDANSTNR = WS-JFR-IDANSTNR-5                                
216600           MOVE 'J' TO WS-TRAEFF-PACKARE                                  
216700           PERFORM CCA-KONTROLLERA-PLOCKLISTA                             
216800        END-IF                                                            
216900        PERFORM IMS-GN-SEQA-WDE4A1                                        
217000        END-PERFORM                                                       
217100     END-IF                                                               
217200*                                                                         
217300     IF WS-TRAEFF-PACKARE  = 'N' AND                                      
217400        NOT WS-ORDERVIS-TRANS                                             
217500*----------------------------------------------ANGIVEN PACKARE            
217600*----------------------------------------------SAKNAS PÅ ORDERN           
217700        MOVE MFS-RENSA-FAELT    TO   MOD-IDDISTR-UT                       
217800                                     MOD-IDKUNDNR-UT                      
217900                                     MOD-IDORDNR-UT                       
218000        MOVE FEL                TO   WS-INDATA-TEST                       
218100        MOVE FEL-719 (INDX)     TO   MOD-TEMFSFEL                         
218200     ELSE                                                                 
218300        IF WS-INDATA-RATT                      AND                        
218400           WS-PACKARES-ODEL-REDAN-KLARA = JA   AND                        
218500           ENDAST-STATUS = NEJ                                            
218600*----------------------------------------------ÄR ANGIVEN PACKARES        
218700*----------------------------------------------ORDERDEL REDAN KLAR        
218800           MOVE FEL            TO  WS-INDATA-TEST                         
218900           MOVE FEL-720 (INDX) TO  MOD-TEMFSFEL                           
219000        END-IF                                                            
219100     END-IF                                                               
219200     .                                                                    
219300     EJECT                                                                
219400 CCA-KONTROLLERA-PLOCKLISTA              SECTION.                         
219500                                                                          
219600     IF KORD-KDPAKOLL NOT = ZERO                                          
219700*-------------------------------------------MAN FÅR EJ RAPPORTERA         
219800*-------------------------------------------DÅ AVVIK.KONTRL PÅGÅR         
219900        MOVE FEL             TO   WS-INDATA-TEST                          
220000        MOVE FEL-804 (INDX)  TO   MOD-TEMFSFEL                            
220100     ELSE                                                                 
220200                                                                          
220300       IF ((KORD-KVORDRAD-PACK NOT = KORD-KVORDRAD)  OR                   
220400         (KORD-KVORDRAD-LEVPL    > 0               AND                    
220500          KORD-KVORDRAD-PACK NOT = KORD-KVORDRAD-LEVPL))                  
220600          MOVE NEJ TO WS-PACKARES-ODEL-REDAN-KLARA                        
220700       END-IF                                                             
220800     END-IF                                                               
220900     .                                                                    
221000     EJECT                                                                
221100 D-BEHANDLA-INTERVALL SECTION.                                            
221200                                                                          
221300     SKIP3                                                                
221400     MOVE MID-IDRADNR-FOM (INX)         TO   ARB-RAD-FOM                  
221500     MOVE MID-IDRADNR-TOM (INX)         TO   ARB-RAD-TOM                  
221600     IF MID-KVLEVART (INX) NOT NUMERIC                                    
221700         MOVE ZERO                      TO   ARB-KVLEVART                 
221800                                             WS-KVLEVART                  
221900     ELSE                                                                 
222000         MOVE MID-KVLEVART (INX)        TO   ARB-KVLEVART                 
222100                                             WS-KVLEVART                  
222200     END-IF                                                               
222300     SKIP2                                                                
222400     IF     ARB-RAD-TOM                 =    ZERO                         
222500        AND ARB-KVLEVART                >    ZERO                         
222600*------------------------------------------------ÄR DET EN DELAD-         
222700*--------------------------------------------- ELLER AVVIKELSERAD         
222800         MOVE NEJ                       TO  FL-INTERVALL                  
222900         MOVE ARB-RAD-FOM               TO  WS-START-RAD                  
223000                                            WS-SISTA-RAD                  
223100     ELSE                                                                 
223200         MOVE JA                        TO  FL-INTERVALL                  
223300         IF ARB-RAD-TOM                 =   ZERO                          
223400             MOVE ARB-RAD-FOM           TO  ARB-RAD-TOM                   
223500         END-IF                                                           
223600         COMPUTE WS-ANT-RADER-INT = ARB-RAD-TOM - ARB-RAD-FOM + 1         
223700         END-COMPUTE                                                      
223800         MOVE ARB-RAD-TOM               TO  WS-SISTA-RAD                  
223900         MOVE ARB-RAD-FOM               TO  WS-START-RAD                  
224000     END-IF                                                               
224100*                                                                         
224200     IF  WS-SAMMA-BILD                                                    
224300         MOVE 'N'             TO WS-RADER-OK                              
224400         MOVE 'N'             TO WS-TRAEFF-RAD                            
224500*                                                                         
224600         MOVE WS-IDDISTR-NUM  TO W-4A1-IDDISTR                            
224700         MOVE WS-IDKUNDNR-NUM TO W-4A1-IDKUNDNR                           
224800         MOVE WS-IDORDNR      TO W-4A1-IDORDNR                            
224900         PERFORM IMS-GU-KUNDORDER-SEK                                     
225000*                                                                         
225100         IF KUNDORDER-SEK-FINNS                                           
225200                                                                          
225300            PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                         
225400                          WS-RADER-OK = 'J'                               
225500               MOVE KORD-IDDISTR  TO W-401-IDDISTR                        
225600               MOVE KORD-IDKUNDNR TO W-401-IDKUNDNR                       
225700               MOVE KORD-IDORDNR5 TO W-401-IDORDNR                        
225800               MOVE KORD-IDPRODNR TO W-401-IDPRODNR                       
225900               MOVE KORD-IDPLKLST TO W-401-IDPLKLST                       
226000                                        WS-IDPLKLST                       
226100               MOVE KORD-IDORDER  TO WS-KORD-IDORDER                      
226200               PERFORM IMS-GU-KUNDORDER                                   
226300                                                                          
226400               MOVE KORD-IDORDER   TO WS-SPAR-IDORDER                     
226500               MOVE KORD-IDDC      TO WS-SPAR-IDDC                        
226600                                                                          
226700               MOVE KORD-IDPRODNR TO WS-JFR-IDPRODNR                      
226800               IF WS-IDPRODNR = WS-JFR-IDPRODNR                           
226900                  PERFORM DA-KONTROLLERA-RADER                            
227000               END-IF                                                     
227100               PERFORM IMS-GN-SEQA-WDE4A1                                 
227200            END-PERFORM                                                   
227300                                                                          
227400            IF WS-TRAEFF-RAD = 'N'                                        
227500              MOVE FEL               TO WS-BEHANDLING-TEST                
227600              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-FOM-ATTR (INX)        
227700              MOVE FEL-722 (INDX)    TO MOD-TEMFSFEL                      
227800            END-IF                                                        
227900        END-IF                                                            
228000     END-IF                                                               
228100*                                                                         
228200     IF WS-BEHANDLING-RATT                                                
228300        PERFORM DB-BEHANDLA-RAD-INOM-INTERVALL                            
228400        IF WS-BEHANDLING-RATT                                             
228500           MOVE WS-IDDISTR-NUM TO TEST-IDDISTR                            
228600           IF NOT DIST19-SATS                                             
228700              PERFORM DC-UPPDATERA-PRODTAB                                
228800           END-IF                                                         
228900           PERFORM DD-UPPDATERA-LAASNINGSREG                              
229000        END-IF                                                            
229100     END-IF                                                               
229200     .                                                                    
229300     EJECT                                                                
229400 DA-KONTROLLERA-RADER SECTION.                                            
229500     SKIP3                                                                
229600                                                                          
229700     MOVE 'N' TO WS-RADER-OK                                              
229800     MOVE 'N' TO FL-SLINGA-KLAR                                           
229900     MOVE ARB-RAD-FOM     TO ARB-RAD-AKTUELL                              
230000     MOVE ARB-RAD-AKTUELL TO W-420-IDPURAD2                               
230100*                                                                         
230200     MOVE KORD-IDUSER     TO WS-JFR-IDANSTNR                              
230300     IF NOT WS-ORDERVIS-TRANS AND                                         
230400        WS-IDANSTNR = WS-JFR-IDANSTNR-5 OR                                
230500        WS-ORDERVIS-TRANS                                                 
230600        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
230700                      SLINGA-KLAR                                         
230800        PERFORM IMS-GU-RAD                                                
230900                                                                          
231000        IF SEGMENT-SAKNAS                                                 
231100           MOVE 'J' TO FL-SLINGA-KLAR                                     
231200        ELSE                                                              
231300          MOVE ZERO                TO WS-ACC-ORAD-VKORDNTO                
231400*                                                                         
231500          IF WS-KVLEVART          = ZERO                                  
231600            COMPUTE WS-ACC-ORAD-VKORDNTO ROUNDED =                        
231700            ORAD-VKART-NTO-KG * (ORAD-KVAVBART - ORAD-KVLEVART)           
231800            END-COMPUTE                                                   
231900          ELSE                                                            
232000                                                                          
232100            COMPUTE WS-ACC-ORAD-VKORDNTO ROUNDED =                        
232200                 ORAD-VKART-NTO-KG *    WS-KVLEVART                       
232300            END-COMPUTE                                                   
232400          END-IF                                                          
232500          ADD WS-ACC-ORAD-VKORDNTO TO ACC-ORAD-VKORDNTO                   
232600*                                                                         
232700          MOVE 'J'                    TO WS-TRAEFF-RAD                    
232800                                                                          
232900          IF NOT WS-ORDERVIS-TRANS AND                                    
233000             WS-JFR-IDANSTNR-5 = '00000'                                  
233100             MOVE 'J' TO FL-SLINGA-KLAR                                   
233200             MOVE FEL               TO WS-BEHANDLING-TEST                 
233300             MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-FOM-ATTR (INX)         
233400             MOVE FEL-716 (INDX)    TO MOD-TEMFSFEL                       
233500          ELSE                                                            
233600            IF ORAD-KDRADSTA > 3                                          
233700               MOVE 'J' TO FL-SLINGA-KLAR                                 
233800               MOVE FEL               TO WS-BEHANDLING-TEST               
233900               MOVE MFS-NUM-FAELT-FEL TO                                  
234000                    MOD-IDRADNR-FOM-ATTR (INX)                            
234100               MOVE FEL-723 (INDX)    TO MOD-TEMFSFEL                     
234200            ELSE                                                          
234300              IF ORAD-FLNOLLJ = JA                                        
234400                 MOVE 'J' TO FL-SLINGA-KLAR                               
234500                 MOVE FEL               TO WS-BEHANDLING-TEST             
234600                 MOVE MFS-NUM-FAELT-FEL TO                                
234700                      MOD-IDRADNR-FOM-ATTR (INX)                          
234800                 MOVE FEL-830 (INDX)    TO MOD-TEMFSFEL                   
234900              ELSE                                                        
235000                 IF ORAD-IDPSN             > ZERO AND                     
235100                    WS-IDKOLLI-SAMP        > ZERO                         
235200                    MOVE 'J'               TO FL-SLINGA-KLAR              
235300                    MOVE FEL               TO WS-BEHANDLING-TEST          
235400                    MOVE MFS-NUM-FAELT-FEL TO                             
235500                         MOD-IDRADNR-FOM-ATTR (INX)                       
235600                    MOVE FEL-794 (INDX) TO MOD-TEMFSFEL                   
235700                 END-IF                                                   
235800              END-IF                                                      
235900            END-IF                                                        
236000          END-IF                                                          
236100          MOVE ORAD-IDARTNR         TO W-IDARTNR                          
236200                                                                          
236300          IF LINE-IX <= MAX-LINES                                         
236400            IF CDC                                                        
236500              PERFORM IMS-GU-WDK611                                       
236600              MOVE CLAG-ADGANG       TO LINE1-ADGANG  (LINE-IX)           
236700              MOVE CLAG-ADPLATS      TO LINE1-ADPLATS (LINE-IX)           
236800              MOVE CLAG-ADLAGOMR     TO LINE1-ADLAGOMR(LINE-IX)           
236900            ELSE                                                          
237000              PERFORM IMS-GU-WDK711                                       
237100              MOVE SLAG-ADGANG       TO LINE1-ADGANG  (LINE-IX)           
237200              MOVE SLAG-ADPLATS      TO LINE1-ADPLATS (LINE-IX)           
237300              MOVE SLAG-ADLAGOMR     TO LINE1-ADLAGOMR(LINE-IX)           
237400            END-IF                                                        
237500                                                                          
237600            IF WS-KVLEVART        = ZERO                                  
237700              MOVE ORAD-KVAVBART     TO LINE1-KVAVBART(LINE-IX)           
237800            ELSE                                                          
237900              MOVE WS-KVLEVART       TO LINE1-KVAVBART(LINE-IX)           
238000            END-IF                                                        
238100            MOVE ARB-RAD-AKTUELL     TO LINE1-NUMBER (LINE-IX)            
238200            MOVE ORAD-IDARTNR        TO LINE1-IDARTNR (LINE-IX)           
238300            MOVE ORAD-VKARTNTO       TO LINE1-VKOLDNET(LINE-IX)           
238400            MOVE ORAD-VKART-NTO-KG   TO LINE1-VKNEWNET(LINE-IX)           
238500            MOVE LINE-IX             TO MAX-TAB                           
238600            ADD 1 TO    LINE-IX                                           
238700          END-IF                                                          
238800          MOVE LINE-IX               TO MAX-RAD                           
238900        END-IF                                                            
239000*                                                                         
239100        IF SEGMENT-FINNS AND                                              
239200           WS-TRAEFF-RAD = 'J'                                            
239300           COMPUTE ARB-RAD-AKTUELL = ARB-RAD-AKTUELL + 1                  
239400           END-COMPUTE                                                    
239500           IF ARB-RAD-AKTUELL > ARB-RAD-TOM                               
239600              MOVE 'J'             TO WS-RADER-OK                         
239700              MOVE 'J'             TO FL-SLINGA-KLAR                      
239800           ELSE                                                           
239900              MOVE ARB-RAD-AKTUELL TO W-420-IDPURAD2                      
240000           END-IF                                                         
240100        END-IF                                                            
240200*                                                                         
240300        IF SEGMENT-SAKNAS AND                                             
240400           WS-TRAEFF-RAD = 'J'                                            
240500           IF ARB-RAD-AKTUELL NOT > ARB-RAD-TOM                           
240600              MOVE 'J' TO FL-SLINGA-KLAR                                  
240700              MOVE FEL               TO WS-BEHANDLING-TEST                
240800              MOVE MFS-NUM-FAELT-FEL TO                                   
240900                                     MOD-IDRADNR-TOM-ATTR (INX)           
241000              MOVE FEL-738 (INDX)    TO MOD-TEMFSFEL                      
241100           END-IF                                                         
241200        END-IF                                                            
241300        END-PERFORM                                                       
241400     END-IF                                                               
241500     .                                                                    
241600     EJECT                                                                
241700                                                                          
241800 DAA-KONTROLLERA-WEIGHT SECTION.                                          
241900                                                                          
242000     MOVE MID-VKORDBTO-KOLLI TO DEC-IDFRIDATA                             
242100     MOVE 5                       TO DEC-KVHELTAL                         
242200     MOVE 1                       TO DEC-KVDECIMAL                        
242300     CALL WDECEDIT USING DEC-WDECAREA                                     
242400                                                                          
242500     IF MID-VKORDBTO-KOLLI = ALL '+'                                      
242600        MOVE MFS-NUM-FAELT-RAETT       TO MOD-VKORDBTO-ATTR               
242700        MOVE MFS-ROER-EJ-FAELT         TO MOD-VKORDBTO-KOLLI              
242800        MOVE ZERO                      TO WS-MOD-VKORDBTO                 
242900     ELSE                                                                 
243000       IF DEC-KDSVAR-OK                                                   
243100       AND DEC-IDEDITDATA > ZERO                                          
243200           MOVE MFS-NUM-FAELT-RAETT TO MOD-VKORDBTO-ATTR                  
243300           MOVE DEC-IDEDITDATA           TO WS-MOD-VKORDBTO               
243400           MOVE MFS-ROER-EJ-FAELT        TO MOD-VKORDBTO-KOLLI            
243500                                                                          
243600           IF US-MEASUREMENT                                              
243700             PERFORM S19-CONVERT-LB-TO-KG                                 
243800           END-IF                                                         
243900       ELSE                                                               
244000         IF DEC-KDSVAR-OK                                                 
244100         AND DEC-IDEDITDATA = ZERO                                        
244200               MOVE FEL                    TO WS-BEHANDLING-TEST          
244300               IF WS-ORDERVIS-TRANS                                       
244400                 MOVE '***POS22'           TO MOD-TEMFSFEL                
244500               ELSE                                                       
244600                 MOVE FEL-748 (INDX)       TO MOD-TEMFSFEL                
244700               END-IF                                                     
244800               MOVE MFS-NUM-FAELT-FEL TO MOD-VKORDBTO-ATTR                
244900         ELSE                                                             
245000             MOVE FEL                      TO WS-BEHANDLING-TEST          
245100             IF WS-ORDERVIS-TRANS                                         
245200               MOVE '***POS23'             TO  MOD-TEMFSFEL               
245300             ELSE                                                         
245400               MOVE FEL-748 (INDX)         TO MOD-TEMFSFEL                
245500             END-IF                                                       
245600             MOVE MFS-NUM-FAELT-FEL        TO MOD-VKORDBTO-ATTR           
245700         END-IF                                                           
245800       END-IF                                                             
245900     END-IF                                                               
246000                                                                          
246100     IF WS-MOD-VKORDBTO > ZERO                                            
246200        MOVE KORD-IDPRODNR   TO  W-420-IDPRODNR-MIN                       
246300                                 W-420-IDPRODNR-MAX                       
246400                                 W-421-IDPRODNR                           
246500                                 W-420-IDPRODNR                           
246600        MOVE 1               TO  W-420-IDPURAD-MIN                        
246700                                 W-420-IDPURAD                            
246800        MOVE 99999           TO  W-420-IDPURAD-MAX                        
246900        MOVE WS-IDKOLLI      TO  W-421-IDKOLLI                            
247000                                                                          
247100        PERFORM IMS-GU-RAD-SEK                                            
247200                                                                          
247300        PERFORM UNTIL BASEN-SLUT                                          
247400           PERFORM IMS-GNP-KOLLI-KOPPL                                    
247500           IF SEGMENT-FINNS                                               
247600             COMPUTE WS-VKORDBTO-SUM ROUNDED   =                          
247700             WS-VKORDBTO-SUM + ORAD-VKART-NTO-KG * KKOLLI-KVLEVART        
247800             END-COMPUTE                                                  
247900           END-IF                                                         
248000           PERFORM IMS-GN-RAD-SEK                                         
248100        END-PERFORM                                                       
248200                                                                          
248300        COMPUTE WS-VKORDBTO-TOT ROUNDED = WS-VKORDBTO-SUM                 
248400        END-COMPUTE                                                       
248500                                                                          
248600        IF WS-MOD-VKORDBTO < WS-VKORDBTO-SUM                              
248700          MOVE FEL                      TO WS-BEHANDLING-TEST             
248800          MOVE INF-WEIGHT-NOT-LESS-THAN TO MOD-TEMFSINF                   
248900          MOVE 'J' TO WS-WEIGHT                                           
249000          MOVE WS-VKORDBTO-TOT          TO HEAD-VKARTNTO                  
249100          IF US-MEASUREMENT                                               
249200            COMPUTE WS-VKORDBTO-TOT ROUNDED =                             
249300                    CONV-KG-TO-LB * WS-VKORDBTO-SUM                       
249400            END-COMPUTE                                                   
249500            MOVE 'LBS'                  TO MOD-TEMFSINF(41:3)             
249600          ELSE                                                            
249700            MOVE 'KG'                   TO MOD-TEMFSINF(41:2)             
249800          END-IF                                                          
249900          INSPECT WS-VKORDBTO-TOT REPLACING LEADING ZERO BY SPACE         
250000          MOVE WS-VKORDBTO-TOT          TO MOD-TEMFSINF(29:10)            
250100                                                                          
250200          MOVE MFS-NUM-FAELT-FEL TO MOD-VKORDBTO-ATTR                     
250300          MOVE MFS-ROER-EJ-FAELT TO MOD-VKORDBTO-KOLLI                    
250400          PERFORM S23-MOVE-LINEDATA                                       
250500        END-IF                                                            
250600     END-IF                                                               
250700     .                                                                    
250800     EJECT                                                                
250900                                                                          
251000 DB-BEHANDLA-RAD-INOM-INTERVALL SECTION.                                  
251100     SKIP3                                                                
251200     MOVE ZERO                   TO  WS-RINT-ANT-FPACK-ORAD               
251300                                                                          
251400     MOVE WS-IDPRODNR            TO  W-420-IDPRODNR-MIN                   
251500                                     W-420-IDPRODNR-MAX                   
251600                                     W-420-IDPRODNR                       
251700     MOVE WS-START-RAD           TO  W-420-IDPURAD-MIN                    
251800                                     W-420-IDPURAD                        
251900                                     WS-AKTUELL-RAD                       
252000     MOVE WS-SISTA-RAD           TO  W-420-IDPURAD-MAX                    
252100     MOVE JA                     TO  FL-RAD-INOM-INTERVALL                
252200                                                                          
252300     MOVE WS-KORD-IDORDER         TO   W-201-IDORDER                      
252400     PERFORM IMS-GU-ORQI01                                                
252500     MOVE OHUV-IDSYSTEM           TO   WS-IDSYSTEM                        
252600                                                                          
252700     SKIP2                                                                
252800     PERFORM IMS-GHU-RAD-SEK                                              
252900                                                                          
253000     PERFORM UNTIL (NOT RAD-FINNS-I-INTERVALL)                            
253100               OR  (NOT WS-BEHANDLING-RATT)                               
253200                                                                          
253300         PERFORM DBA-SPARA-RAD-INFO                                       
253400                                                                          
253500         IF WS-BEHANDLING-RATT                                            
253600          PERFORM DBB-UPPDATERA-RAD                                       
253700*                                                                         
253800* OBS - OBS -OBS                                                          
253900* ÖPPNA DENNA IF-SATS VID TESTER I BTS OCH                                
254000* STÄNG MOTSVARANDE I STYR-SECTION                                        
254100* (BTS KLARAR INTE AV ROLL-BACK)                                          
254200*                                                                         
254300*           IF NYTT-KOLLI                                                 
254400*              PERFORM G-SKAPA-KOLLISEG                                   
254500*           END-IF                                                        
254600*                                                                         
254700          IF WS-BEHANDLING-RATT                                           
254800            PERFORM DBC-LAGG-UPP-KOLLI-KOPPL                              
254900                                                                          
255000            ADD 1 TO WS-AKTUELL-RAD                                       
255100            IF WS-AKTUELL-RAD > WS-SISTA-RAD                              
255200               MOVE NEJ TO FL-RAD-INOM-INTERVALL                          
255300            ELSE                                                          
255400               PERFORM IMS-GHN-RAD-SEK                                    
255500            END-IF                                                        
255600          END-IF                                                          
255700         END-IF                                                           
255800     END-PERFORM                                                          
255900     .                                                                    
256000     EJECT                                                                
256100 DBA-SPARA-RAD-INFO        SECTION.                                       
256200     MOVE ORAD-VKARTNTO         TO  SPAR-PRAD-VKARTNTO                    
256300     MOVE ORAD-KVFLAMP          TO  SPAR-PRAD-KVFLAMP                     
256400     MOVE ORAD-KDFARLIG         TO  SPAR-PRAD-KDFARLIG                    
256500     MOVE ORAD-PRARTNTO         TO  SPAR-PRAD-PRARTNTO                    
256600     MOVE ORAD-PRAVCOST         TO  SPAR-PRAD-PRAVCOST                    
256700     MOVE ORAD-PRARTNTO-LOC     TO  SPAR-PRAD-PRARTNTO-LOC                
256800     MOVE ORAD-PRARTNTO-LOCPREL TO  SPAR-PRAD-PRARTNTO-LOCPREL            
256900     MOVE ORAD-KDVALISO         TO  SPAR-PRAD-KDVALISO                    
257000     MOVE ORAD-KDVALISO-EXP     TO  SPAR-PRAD-KDVALISO-EXP                
257100                                                                          
257200     MOVE ORAD-IDPSN            TO  SPAR-IDPSN                            
257300     MOVE ORAD-VKART-FG         TO  SPAR-VKART-FG                         
257400     MOVE ORAD-VLFG             TO  SPAR-VLFG                             
257500     MOVE ORAD-SUEQFG           TO  SPAR-SUEQFG                           
257600                                                                          
257700     MOVE ORAD-IDARTNR          TO WS-SPAR-IDARTNR                        
257800     MOVE ORAD-BEART            TO WS-SPAR-BEART                          
257900     MOVE ORAD-KVBEART          TO WS-SPAR-KVBEART                        
258000     MOVE ORAD-FLTILLK          TO WS-SPAR-FLTILLK                        
258100     MOVE ORAD-IDKUNDRF-RO      TO WS-SPAR-IDKUNDRF-RO                    
258200     MOVE ORAD-IDPURAD          TO WS-SPAR-IDPURAD                        
258300                                                                          
258400     IF  ARB-KVLEVART           >   ORAD-KVAVBART                         
258500         MOVE FEL              TO WS-BEHANDLING-TEST                      
258600         MOVE MFS-NUM-FAELT-FEL                                           
258700                               TO MOD-KVLEVART-ATTR (INX)                 
258800         MOVE FEL-725 (INDX)   TO MOD-TEMFSFEL                            
258900     ELSE                                                                 
259000         IF  ARB-KVLEVART       >   ZERO                                  
259100             MOVE ARB-KVLEVART  TO  SPAR-PRAD-KVLEVART                    
259200         ELSE                                                             
259300             MOVE ORAD-KVAVBART TO  SPAR-PRAD-KVLEVART                    
259400         END-IF                                                           
259500                                                                          
259600         PERFORM S10-UPPD-SPAR-KOLLI                                      
259700     END-IF                                                               
259800     .                                                                    
259900     EJECT                                                                
260000 DBB-UPPDATERA-RAD         SECTION.                                       
260100*                                                                         
260200     IF ORAD-IDLEVNR NOT = SPACE                                          
260300       MOVE JA                  TO DIRLEV-KOLLI-SW                        
260400     END-IF                                                               
260500*                                                                         
260600     IF  INTERVALL-RAD                                                    
260700*      * RAPPORTERING HEL RAD                                             
260800       IF  ORAD-KVLEVART > ZERO                                           
260900*        * RAPP. FÖR RADEN REDAN PÅBÖRJAD                                 
261000         MOVE FEL              TO WS-BEHANDLING-TEST                      
261100         MOVE MFS-NUM-FAELT-FEL                                           
261200                               TO MOD-KVLEVART-ATTR (INX)                 
261300         MOVE FEL-767 (INDX)   TO MOD-TEMFSFEL                            
261400       ELSE                                                               
261500         MOVE ORAD-KVAVBART    TO ORAD-KVLEVART                           
261600       END-IF                                                             
261700                                                                          
261800     ELSE                                                                 
261900*      * DELRAPPORERING AV RAD                                            
262000       IF  ORAD-KVLEVART + ARB-KVLEVART > ORAD-KVAVBART                   
262100*        * LEVERERAT ÖVERSTIGER AVBOKAT                                   
262200         MOVE FEL              TO WS-BEHANDLING-TEST                      
262300         MOVE MFS-NUM-FAELT-FEL                                           
262400                               TO MOD-KVLEVART-ATTR (INX)                 
262500         MOVE FEL-725 (INDX)   TO MOD-TEMFSFEL                            
262600       ELSE                                                               
262700         ADD ARB-KVLEVART      TO ORAD-KVLEVART                           
262800       END-IF                                                             
262900     END-IF                                                               
263000                                                                          
263100     IF  WS-BEHANDLING-RATT                                               
263200       IF  ORAD-KVLEVART = ORAD-KVAVBART                                  
263300         MOVE +4    TO ORAD-KDRADSTA                                      
263400         ADD 1      TO WS-TOT-ANT-RADER                                   
263500         ADD 1      TO WS-RINT-ANT-FPACK-ORAD                             
263600                                                                          
263700          IF KORD-KDORDKL = +0                                            
263800             PERFORM DBBB-UPPDATERA-VOR-TIKLAR                            
263900          END-IF                                                          
264000       END-IF                                                             
264100                                                                          
264200       PERFORM IMS-REPL-BEHANDLAD-RAD                                     
264300       PERFORM DBBA-EV-SKAPA-RYK-TRANS                                    
264400     END-IF                                                               
264500     .                                                                    
264600     EJECT                                                                
264700 DBBA-EV-SKAPA-RYK-TRANS SECTION.                                         
264800                                                                          
264900     IF ORAD-IDKUNDRF-RO NOT = '00000     ' AND                           
265000        ORAD-TIRODAT         > ZERO                                       
265100                                                                          
265200       IF LOGG-IDLOGLOP = 9                                               
265300         MOVE ZERO                TO   LOGG-IDLOGLOP                      
265400       END-IF                                                             
265500       ACCEPT  LOGG-TIAAMMDD      FROM DATE                               
265600       ACCEPT  LOGG-TIKLOCK       FROM TIME                               
265700       ADD     +1                 TO   LOGG-IDLOGLOP                      
265800                                                                          
265900       MOVE    WS-IDDISTR-NUM     TO   RYK-IDDISTR                        
266000                                       W-WDQ2C-IDDISTR                    
266100       MOVE    WS-IDKUNDNR-NUM    TO   RYK-IDKUNDNR                       
266200                                       W-WDQ2C-IDKUNDNR                   
266300       MOVE    ORAD-IDKUNDRF-RO(1:5)                                      
266400                                  TO   W-WDQ2C-IDORDNR5                   
266500                                                                          
266600       MOVE WS-KORD-IDORDER       TO   W-201-IDORDER                      
266700       PERFORM IMS-GET-ORQI01-CSEQ                                        
266800       IF SEGMENT-FINNS                                                   
266900          MOVE OHUV-IDORDER       TO   RYK-IDORDER                        
267000       ELSE                                                               
267100          MOVE +0                 TO   RYK-IDORDER                        
267200       END-IF                                                             
267300                                                                          
267400       MOVE    'RYK'              TO   RYK-IDPTYP                         
267500                                       LOGG-IDPTYP                        
267600                                                                          
267700       MOVE    ORAD-IDARTNR       TO   RYK-IDARTNR                        
267800       MOVE    LOGG-TIAAMMDD      TO   RYK-TIRODAT                        
267900       MOVE    ORAD-KVLEVART      TO   RYK-KVLEVART                       
268000       MOVE    ORAD-KVBEART       TO   RYK-KVBEART-Q                      
268100       MOVE    ORAD-KDORDKL       TO   RYK-KDORDKL                        
268200       MOVE    ORAD-KDPRODSL      TO   RYK-KDPRODSL                       
268300       MOVE    ZERO               TO   RYK-KDORDBEK                       
268400                                                                          
268500       MOVE    SPACE              TO   LOGG-SORTPOST                      
268600       MOVE    RYK-WDGZRYK        TO   LOGG-LOGGPOST                      
268700                                                                          
268800       PERFORM IMS-ISRT-ZZAC01                                            
268900                                                                          
269000       PERFORM UNTIL SEGMENT-FINNS                                        
269100         IF LOGG-IDLOGLOP = 9                                             
269200           MOVE ZERO              TO LOGG-IDLOGLOP                        
269300           ACCEPT  LOGG-TIKLOCK   FROM TIME                               
269400         END-IF                                                           
269500         ADD +1                   TO LOGG-IDLOGLOP                        
269600         PERFORM IMS-ISRT-ZZAC01                                          
269700       END-PERFORM                                                        
269800     END-IF                                                               
269900     .                                                                    
270000     EJECT                                                                
270100 DBBB-UPPDATERA-VOR-TIKLAR SECTION.                                       
270200     SKIP3                                                                
270300                                                                          
270400     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
270500     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
270600     MOVE KORD-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
270700                                    W-A601KY-MAX-IDDISTR                  
270800     MOVE KORD-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
270900                                    W-A601KY-MAX-IDKUNDNR                 
271000     MOVE KORD-IDORDNR5          TO W-A601KY-MIN-IDORDNR                  
271100                                    W-A601KY-MAX-IDORDNR                  
271200                                                                          
271300     MOVE NEJ                    TO SW-TIKLAR-UPPDATERAD                  
271400                                                                          
271500     PERFORM IMS-GHN-WDA6B                                                
271600     PERFORM UNTIL SEGMENT-SAKNAS                                         
271700                OR BASEN-SLUT                                             
271800                OR SW-TIKLAR-UPPDATERAD = JA                              
271900                                                                          
272000         IF  VOR-IDARTNR = ORAD-IDARTNR                                   
272100         AND VOR-TIKLAR = +0                                              
272200                                                                          
272300             MOVE WS-DAGENS-DATUM  TO VOR-TIKLAR                          
272400             MOVE WS-TIDPUNKT      TO WS-TIDPUNKT-RED                     
272500             MOVE WS-HHMMSS        TO VOR-TIKLATID                        
272600             PERFORM IMS-REPL-WDA6B                                       
272700             MOVE JA               TO SW-TIKLAR-UPPDATERAD                
272800         END-IF                                                           
272900                                                                          
273000         PERFORM IMS-GHN-WDA6B                                            
273100     END-PERFORM                                                          
273200     .                                                                    
273300     EJECT                                                                
273400 DBC-LAGG-UPP-KOLLI-KOPPL  SECTION.                                       
273500     SKIP3                                                                
273600     MOVE WS-IDPRODNR            TO KKOLLI-IDPRODNR                       
273700     MOVE WS-IDKOLLI             TO KKOLLI-IDKOLLI                        
273800     MOVE SPAR-PRAD-KVLEVART     TO KKOLLI-KVLEVART                       
273900     PERFORM IMS-ISRT-KOLLI-KOPPL                                         
274000     IF SEGMENT-FINNS-REDAN                                               
274100         MOVE WS-IDPRODNR        TO  W-421-IDPRODNR                       
274200         MOVE WS-IDKOLLI         TO  W-421-IDKOLLI                        
274300*                                                                         
274400         PERFORM IMS-GHNP-KOLLI-KOPPL                                     
274500         ADD SPAR-PRAD-KVLEVART  TO KKOLLI-KVLEVART                       
274600         PERFORM IMS-REPL-KOLLI-KOPPL                                     
274700     ELSE                                                                 
274800         ADD +1                  TO ARB-KOLLI-KVORDRAD                    
274900                                                                          
275000         IF SPAR-PRAD-KDFARLIG = +4                                       
275100         OR SPAR-PRAD-KDFARLIG = +7                                       
275200             ADD +1              TO  ARB-KOLLI-KVFALRAD                   
275300         END-IF                                                           
275400                                                                          
275500     END-IF                                                               
275600*                                                                         
275700     IF WS-IDDC NOT = W-IDDC-B6                                           
275800        MOVE WS-IDDC TO W-IDDC-B6                                         
275900        PERFORM IMS-GU-WDB601                                             
276000     END-IF                                                               
276100     IF DCS-NDC-NA                                                        
276200        PERFORM S13-DATA-TILL-DEL-NOTE                                    
276300     END-IF                                                               
276400                                                                          
276500     IF SPAR-IDPSN > ZERO                                                 
276600       PERFORM DBCA-SPARA-FG-DATA                                         
276700     END-IF                                                               
276800     .                                                                    
276900     EJECT                                                                
277000 DBCA-SPARA-FG-DATA SECTION.                                              
277100     SKIP3                                                                
277200     MOVE +1                     TO FG-INDX                               
277300     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
277400       IF TAB-IDPSN(FG-INDX) = ZERO                                       
277500         MOVE SPAR-IDPSN         TO TAB-IDPSN(FG-INDX)                    
277600         PERFORM S12-BERAEKNA-FG-FAELT                                    
277700                                                                          
277800       ELSE                                                               
277900         IF SPAR-IDPSN = TAB-IDPSN(FG-INDX)                               
278000           PERFORM S12-BERAEKNA-FG-FAELT                                  
278100         END-IF                                                           
278200       END-IF                                                             
278300                                                                          
278400       ADD +1 TO FG-INDX                                                  
278500     END-PERFORM                                                          
278600                                                                          
278700     COMPUTE TOTAL-SUEQFG      = TOTAL-SUEQFG     +                       
278800                                 (SPAR-SUEQFG     *                       
278900                                  KKOLLI-KVLEVART)                        
279000     END-COMPUTE                                                          
279100     .                                                                    
279200     EJECT                                                                
279300 DC-UPPDATERA-PRODTAB      SECTION.                                       
279400     SKIP3                                                                
279500     IF  WS-RINT-ANT-FPACK-ORAD > ZERO                                    
279600*      * RAD-INTERVALLET HAR FÄRDIGPACKADE ORADER                         
279700                                                                          
279800       MOVE WS-IDPRODNR          TO W-420-IDPRODNR                        
279900       MOVE WS-IDPRODNR          TO W-420-IDPRODNR                        
280000       PERFORM IMS-GU-WDE411-01-BSEQ                                      
280100                                                                          
280200       PERFORM DCA-LAES-SHIFTTAB                                          
280300                                                                          
280400       MOVE KORD-IDORDER         TO W-301-IDORDER                         
280500       MOVE KORD-IDDC            TO W-301-IDDC                            
280600       MOVE KORD-IDPRODNR        TO W-301-IDPRODNR                        
280700       MOVE KORD-IDPLKLST        TO W-301-IDPLKLST                        
280800       PERFORM IMS-GU-ORQA01                                              
280900       IF SEGMENT-FINNS                                                   
281000          MOVE ODEL-IDLEVNR         TO WS-IDLEVNR                         
281100          MOVE ODEL-IDDC-EXP        TO WS-ODEL-IDDC-EXP                   
281200       END-IF                                                             
281300                                                                          
281400*CO      * PRODTAB UPPDATERAS FÖR ALLA PRODKL. (MARS 2013)                
281500       IF  ODEL-KDPRODKL = 'B'                                            
281600       OR  ODEL-KDPRODKL = 'C'                                            
281700*        * PRODTAB UPPDATERAS ENDAST FÖR PRODKL B OCH C.                  
281800                                                                          
281900         MOVE KORD-IDDC          TO W-4471-IDDC                           
282000         MOVE ODEL-IDPRCBAS      TO W-4471-IDPRCBAS                       
282100         MOVE ODEL-IDPRCVAR      TO W-4471-IDPRCVAR                       
282200         PERFORM IMS-GHU-XXKW11                                           
282300                                                                          
282400         IF  SEGMENT-FINNS                                                
282500*          * PRODTAB UPPDATERAS ENDAST OM ORDERDELENS PRC FINNS.          
282600                                                                          
282700           MOVE 1                TO IND1                                  
282800           MOVE W-4478-IDSHIFT   TO IND2                                  
282900           MOVE ODEL-DARFS       TO HJALP-ODEL-DARFS                      
283000           MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                     
283100                                                                          
283200           PERFORM UNTIL IND1 = 30 OR                                     
283300                         4472-TIRFS (IND1) = ZERO OR                      
283400                         HJALP-ODEL-DARFS-6 = HJALP-4472-TIRFS-6          
283500             ADD 1                TO IND1                                 
283600             MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                   
283700           END-PERFORM                                                    
283800                                                                          
283900           MOVE ODEL-DARFS (3:10) TO 4472-TIRFS (IND1)                    
284000           MOVE W-4478-IDSHIFT  TO 4472-IDSHIFT (IND1, IND2)              
284100           ADD WS-RINT-ANT-FPACK-ORAD                                     
284200                                TO 4472-KVRADER-PRAPP (IND1, IND2)        
284300           PERFORM DCB-ADDERA-TOTAL-PRODTID                               
284400           PERFORM IMS-REPL-XXKW11                                        
284500         END-IF                                                           
284600       END-IF                                                             
284700     END-IF                                                               
284800     .                                                                    
284900     EJECT                                                                
285000 DCA-LAES-SHIFTTAB         SECTION.                                       
285100*                                                                         
285200     MOVE KORD-IDDC         TO W-4477-IDDC                                
285300     MOVE '1'               TO W-4478-IDSHIFT                             
285400     MOVE KORD-IDUSER       TO W-4478-IDUSER                              
285500     PERFORM IMS-GU-XXLB                                                  
285600*                                                                         
285700     IF SEGMENT-SAKNAS                                                    
285800        MOVE '2'            TO W-4478-IDSHIFT                             
285900        PERFORM IMS-GU-XXLB                                               
286000*                                                                         
286100        IF SEGMENT-SAKNAS                                                 
286200           MOVE '3'         TO W-4478-IDSHIFT                             
286300           PERFORM IMS-GU-XXLB                                            
286400*                                                                         
286500           IF SEGMENT-SAKNAS                                              
286600              MOVE '1'      TO W-4478-IDSHIFT                             
286700           END-IF                                                         
286800        END-IF                                                            
286900     END-IF                                                               
287000     .                                                                    
287100     EJECT                                                                
287200 DCB-ADDERA-TOTAL-PRODTID             SECTION.                            
287300                                                                          
287400     MOVE 4472-SUPTID-PRAPP (IND1, IND2) TO WS-SUPTID-PRAPP               
287500                                                                          
287600     COMPUTE WS-KVPTID-MIN ROUNDED = WS-RINT-ANT-FPACK-ORAD *             
287700                                     ODEL-KVPTID                          
287800     END-COMPUTE                                                          
287900                                                                          
288000     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
288100     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
288200     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
288300                            (WS-KVPTID-TIM * 60)                          
288400     END-COMPUTE                                                          
288500                                                                          
288600     ADD  WS-SUPTID-MIN                  TO WS-KVPTID-MIN                 
288700     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
288800     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
288900     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
289000                            (WS-KVPTID-TIM * 60)                          
289100     END-COMPUTE                                                          
289200     MOVE WS-KVPTID-MIN                  TO WS-SUPTID-MIN                 
289300                                                                          
289400     MOVE WS-SUPTID-PRAPP TO 4472-SUPTID-PRAPP (IND1, IND2)               
289500     .                                                                    
289600     EJECT                                                                
289700 DD-UPPDATERA-LAASNINGSREG SECTION.                                       
289800     IF WS-GODKAND-BILD                                                   
289900** GÄLLER EJ ORDERVIS PACKNING                                            
290000       MOVE WS-IDKOLLI              TO  W-610-IDKOLLI                     
290100       PERFORM IMS-GHU-KOLLI                                              
290200                                                                          
290300       IF KOLLI-FLBANDST = 'J' AND KOLLI-KDKOLSTA = 0                     
290400       MOVE WS-IDPRODNR TO W-4301-IDPRODNR                                
290500       PERFORM IMS-GHU-XXDU01                                             
290600       IF SEGMENT-SAKNAS                                                  
290700         MOVE '4301'      TO 4301-IDHTYP                                  
290800         MOVE WS-IDPRODNR TO 4301-IDPRODNR                                
290900         MOVE LOW-VALUE   TO 4301-LOWVALUE                                
291000         PERFORM IMS-ISRT-XXDU01                                          
291100*                                                                         
291200         MOVE WS-IDKOLLI  TO 4302-IDKOLLI                                 
291300         MOVE WS-IDPLKLST TO 4302-IDPLKLST                                
291400         MOVE  ZERO       TO 4302-KDKOLSTA                                
291500         MOVE  JA         TO 4302-FLBANDST                                
291600         MOVE  SPACE      TO 4302-FILLER                                  
291700         PERFORM IMS-ISRT-XXDU11                                          
291800       ELSE                                                               
291900         MOVE WS-IDKOLLI  TO W-4302-IDKOLLI                               
292000         MOVE WS-IDPLKLST TO W-4302-IDPLKLST                              
292100         PERFORM IMS-GNP-XXDU11                                           
292200         IF SEGMENT-SAKNAS                                                
292300            MOVE WS-IDKOLLI  TO 4302-IDKOLLI                              
292400            MOVE WS-IDPLKLST TO 4302-IDPLKLST                             
292500            MOVE  ZERO       TO 4302-KDKOLSTA                             
292600            MOVE  JA         TO 4302-FLBANDST                             
292700            MOVE  SPACE      TO 4302-FILLER                               
292800            PERFORM IMS-ISRT-XXDU11                                       
292900         END-IF                                                           
293000       END-IF                                                             
293100     END-IF                                                               
293200     END-IF                                                               
293300     .                                                                    
293400     EJECT                                                                
293500 E-UPPDATERA-KOLLIFALT     SECTION.                                       
293600                                                                          
293700     PERFORM EA-SKAPA-4322                                                
293800     PERFORM IMS-GHU-KOLLIREG                                             
293900     MOVE VORD-IDDC               TO  WS-IDDC                             
294000     MOVE VORD-FLAUTFAK           TO  WS-FLAUTFAK                         
294100     MOVE VORD-KDFAKTYP           TO  WS-KDFAKTYP                         
294200     MOVE VORD-DARFS              TO  WS-DARFS                            
294300     ADD +1                       TO  VORD-KVKOLLI                        
294400     ADD WS-VKORDBTO              TO  VORD-VKORDBTO                       
294500     ADD WS-VLORDBTO              TO  VORD-VLORDBTO                       
294600     ADD WS-SUORDV-KOLLI          TO  VORD-SUORDV-PACK                    
294700     ADD WS-SUORDV-KOLLI-LOC      TO  VORD-SUORDV-PACK-LOC                
294800     ADD WS-SUORDV-KOLLI-LOCPREL  TO  VORD-SUORDV-PACK-LOCPREL            
294900     MOVE WS-DAGENS-DATUM         TO  VORD-TIPACKN-SK                     
295000     MOVE WS-KDVALISO             TO  VORD-KDVALISO                       
295100     MOVE WS-KDVALISO-EXP         TO  VORD-KDVALISO-EXP                   
295200     PERFORM IMS-REPL-KOLLIREG                                            
295300     SKIP2                                                                
295400     IF WS-PRT-KDSVAR-ADRESSFL = RAETT                                    
295500       PERFORM ED-SEND-PRINTTRANS                                         
295600     END-IF                                                               
295700     MOVE WS-IDKOLLI              TO  W-610-IDKOLLI                       
295800     PERFORM IMS-GHU-KOLLI                                                
295900     MOVE 1                       TO  KOLLI-KDKOLSTA                      
296000     MOVE WS-DAGENS-DATUM         TO  KOLLI-TIPACKN                       
296100     MOVE WS-TIDPUNKT             TO  WS-TIDPUNKT-RED                     
296200     MOVE WS-HHMMSS               TO  KOLLI-TIPACTID                      
296300     MOVE WS-DARFS                TO  KOLLI-DARFS                         
296400                                                                          
296500                                                                          
296600     PERFORM IMS-REPL-KOLLI                                               
296700     PERFORM EC-UPPDAT-KDORDSTA                                           
296800     .                                                                    
296900     EJECT                                                                
297000 EA-SKAPA-4322   SECTION.                                                 
297100                                                                          
297200*SKAPA INFO TILL SVENSKA ÅF, SÄNDS VIA VR.                                
297300     IF DIST03-SVERIGE-100-799 OR DIST03-NORGE                            
297400     OR DIST03-DANMARK-900                                                
297500     OR DIST85-PU-VIA-VR                                                  
297600     OR DIST21-TYRE                                                       
297700     AND NOT DIST47-INTERNA                                               
297800        MOVE WS-IDPRODNR          TO XXJK-4322-IDPRODNR                   
297900        MOVE WS-IDKOLLI           TO XXJK-4322-IDKOLLI                    
298000        MOVE XXJK-4322-WDGX4322   TO 4322-WDGX4322                        
298100        PERFORM IMS-ISRT-4322-SEGM                                        
298200     END-IF                                                               
298300     .                                                                    
298400     SKIP2                                                                
298500 EB-SEND-TMS    SECTION.                                                  
298600                                                                          
298700*LK TMS PACK-REPORT                                                       
298800     MOVE WS-IDDC                TO TMS-IDDC                              
298900     MOVE KORD-IDDISTR           TO TMS-IDDISTR                           
299000     MOVE KORD-IDKUNDNR          TO TMS-IDKUNDNR                          
299100     MOVE KORD-IDORDNR5          TO TMS-IDORDNR7                          
299200     MOVE KOLLI-IDKOLLI          TO TMS-IDKOLLI(1)                        
299300                                                                          
299400     CALL W403TMS1 USING TMS-W403TMS1                                     
299500               TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                           
299600               TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                     
299700               TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                     
299800               TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                   
299900               TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                     
300000               TMS-WDK5-PCB TMS-WDQ2C-PCB                                 
300100     .                                                                    
300200     SKIP2                                                                
300300 EC-UPPDAT-KDORDSTA  SECTION.                                             
300400     PERFORM IMS-GU-ORQI01                                                
300500     MOVE WS-IDDC           TO  W-IDDC                                    
300600     PERFORM IMS-GHNP-ORQI12                                              
300700     IF ARB-KDORDSTA = 'U '                                               
300800        MOVE 'U*' TO ARB-KDORDSTA                                         
300900        PERFORM IMS-REPL-ORQI12                                           
301000     END-IF                                                               
301100     .                                                                    
301200     EJECT                                                                
301300 ED-SEND-PRINTTRANS SECTION.                                              
301400                                                                          
301500     IF WS-IDDC NOT = W-IDDC-B6                                           
301600        MOVE WS-IDDC TO W-IDDC-B6                                         
301700        PERFORM IMS-GU-WDB601                                             
301800     END-IF                                                               
301900     IF DIST03-SVERIGE-2 OR                                               
302000        WS-ORDERVIS-TRANS OR                                              
302100        (DCS-CDC AND                                                      
302200         DIST07-USA-RETAILER       OR                                     
302300         DIST07-USA-SUPPL-FROM-CDC)                                       
302400                                                                          
302500       CONTINUE                                                           
302600     ELSE                                                                 
302700         PERFORM EDA-SEND-PRINTTRANS                                      
302800     END-IF                                                               
302900     .                                                                    
303000     SKIP2                                                                
303100 EDA-SEND-PRINTTRANS SECTION.                                             
303200                                                                          
303300     MOVE WS-IDDISTR-NUM           TO 4333-MID-IDDISTR-UT                 
303400     MOVE WS-IDKUNDNR-NUM          TO 4333-MID-IDKUNDNR-UT                
303500     MOVE WS-IDORDNR               TO 4333-MID-IDORDNR-UT                 
303600     MOVE WS-IDKOLLI               TO 4333-MID-IDKOLLI-UT                 
303700     MOVE WS-IDDC                  TO 4333-MID-IDDC-UT                    
303800                                                                          
303900     IF DIRLEV-KOLLI                                                      
304000       MOVE WS-IDPRODNR            TO 4333-MID-IDPRODNR-UT                
304100     ELSE                                                                 
304200       MOVE ZERO                   TO 4333-MID-IDPRODNR-UT                
304300     END-IF                                                               
304400     MOVE WS-KDPRTVAL-ADRESSFL     TO 4333-MID-KDPRTVAL-UT                
304500     MOVE ZERO                     TO 4333-MID-IDKOLLI-TOM                
304600     MOVE '++++'                   TO 4333-MID-IDDISTR-IN                 
304700     MOVE '++++++'                 TO 4333-MID-IDKUNDNR-IN                
304800     MOVE '+++++'                  TO 4333-MID-IDORDNR-IN                 
304900                                      4333-MID-IDKOLLI-IN                 
305000     MOVE '++'                     TO 4333-MID-IDDC-IN                    
305100     MOVE '+++++++'                TO 4333-MID-IDPRODNR-IN                
305200     MOVE '++'                     TO 4333-MID-KDPRTVAL-IN                
305300                                                                          
305400     COMPUTE 4333-MID-LL  = LENGTH OF 4333-MID-W4I33301 + 17              
305500     END-COMPUTE                                                          
305600     MOVE 'W4T333  '               TO 4333-MID-TRANSKOD                   
305700     MOVE '431E'                   TO 4333-MID-IDTRANS                    
305800     MOVE WS-KDMFSFOR              TO 4333-MID-KDMFSFOR                   
305900                                                                          
306000     PERFORM IMS-PURGE-ALT4333-MSG                                        
306100     .                                                                    
306200     SKIP2                                                                
306300 F-UPPDATERA-KOLLIREG      SECTION.                                       
306400                                                                          
306500     PERFORM IMS-GHU-KOLLIREG                                             
306600     MOVE VORD-IDDISTR            TO  TEST-IDDISTR                        
306700     MOVE VORD-KDORDKL            TO  ARB-KOLLI-KDORDKL                   
306800                                      WS-KDORDKL                          
306900     MOVE VORD-FLAUTFAK           TO  ARB-KOLLI-FLAUTFAK                  
307000     MOVE VORD-DARFS              TO  WS-DARFS                            
307100     IF NYTT-KOLLI                                                        
307200         COMPUTE VORD-KVKOLPAC = VORD-KVKOLPAC + 1                        
307300     END-COMPUTE                                                          
307400     END-IF                                                               
307500     IF VORD-KDORDSTA = 1                                                 
307600         MOVE 2                   TO  VORD-KDORDSTA                       
307700     END-IF                                                               
307800     COMPUTE VORD-KVORDRAD-PACK                                           
307900                         = VORD-KVORDRAD-PACK + WS-TOT-ANT-RADER          
308000     END-COMPUTE                                                          
308100                                                                          
308200     IF (MID-FLFORTSK = JA  OR YES OR OMSTART)                            
308300     AND WS-KDKOLSTA  > +0                                                
308400         ADD ARB-KOLLI-SUORDV     TO  VORD-SUORDV-PACK                    
308500         ADD ARB-KOLLI-SUORDV-LOC TO  VORD-SUORDV-PACK-LOC                
308600         ADD ARB-KOLLI-SUORDV-LOCPREL TO  VORD-SUORDV-PACK-LOCPREL        
308700         MOVE ARB-KOLLI-KDVALISO     TO  VORD-KDVALISO                    
308800         MOVE ARB-KOLLI-KDVALISO-EXP TO  VORD-KDVALISO-EXP                
308900     END-IF                                                               
309000                                                                          
309100     IF MID-VKORDBTO-KOLLI = ALL '+'                                      
309200        ADD ARB-KOLLI-VKORDNTO       TO  VORD-VKORDBTO                    
309300     ELSE                                                                 
309400        ADD WS-MOD-VKORDBTO          TO  VORD-VKORDBTO                    
309500     END-IF                                                               
309600                                                                          
309700     PERFORM IMS-REPL-KOLLIREG                                            
309800                                                                          
309900     MOVE WS-IDKOLLI              TO  W-610-IDKOLLI                       
310000     PERFORM IMS-GHU-KOLLI                                                
310100     MOVE KOLLI-VKORDBTO-KOLLI    TO  WS-VKORDBTO                         
310200     MOVE KOLLI-VLORDBTO-KOLLI    TO  WS-VLORDBTO                         
310300     MOVE KOLLI-FLBANDST          TO  WS-KOLLI-FLBANDST                   
310400                                      WS-SAVE-KOLLI-FLBANDST              
310500     MOVE KOLLI-KDKOLSTA          TO  WS-SAVE-KOLLI-KDKOLSTA              
310600     MOVE WS-DARFS                TO  KOLLI-DARFS                         
310700     PERFORM S11-UPPD-KOLLI-FRAN-ARB                                      
310800*                                                                         
310900     PERFORM FA-UPPD-FARLIGT-GODS-DATA                                    
311000                                                                          
311100     IF KOLLI-SUORDV-KOLLI > 0                                            
311200       MOVE KOLLI-SUORDV-KOLLI    TO  WS-SUORDV-KOLLI                     
311300     ELSE                                                                 
311400       MOVE KOLLI-SUORDV-KLI-EXP  TO  WS-SUORDV-KOLLI                     
311500     END-IF                                                               
311600     MOVE KOLLI-SUORDV-LOC        TO  WS-SUORDV-KOLLI-LOC                 
311700     MOVE KOLLI-SUORDV-LOCPREL    TO  WS-SUORDV-KOLLI-LOCPREL             
311800     MOVE KOLLI-KDVALISO          TO  WS-KDVALISO                         
311900     MOVE KOLLI-KDVALISO-EXP      TO  WS-KDVALISO-EXP                     
312000     PERFORM IMS-REPL-KOLLI                                               
312100     .                                                                    
312200     EJECT                                                                
312300 FA-UPPD-FARLIGT-GODS-DATA SECTION.                                       
312400                                                                          
312500     MOVE +1 TO FG-INDX                                                   
312600     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
312700                                                                          
312800       IF TAB-IDPSN(FG-INDX) > ZERO                                       
312900         MOVE TAB-IDPSN(FG-INDX)    TO KOLLI-IDPSN(FG-INDX)               
313000         MOVE TAB-VKART-FG(FG-INDX) TO KOLLI-VKART-FG(FG-INDX)            
313100         MOVE TAB-VLFG(FG-INDX)     TO KOLLI-VLFG(FG-INDX)                
313200                                                                          
313300       ELSE                                                               
313400         MOVE ZERO                  TO KOLLI-IDPSN(FG-INDX)               
313500                                       KOLLI-VKART-FG(FG-INDX)            
313600                                       KOLLI-VLFG(FG-INDX)                
313700       END-IF                                                             
313800                                                                          
313900       ADD +1 TO FG-INDX                                                  
314000     END-PERFORM                                                          
314100                                                                          
314200     MOVE TOTAL-SUEQFG               TO KOLLI-SUEQFG                      
314300     .                                                                    
314400     EJECT                                                                
314500 G-SKAPA-KOLLISEG           SECTION.                                      
314600                                                                          
314700     MOVE WS-KORD-IDORDER   TO  W-201-IDORDER                             
314800     MOVE WS-IDDC           TO  W-IDDC                                    
314900     PERFORM  IMS-GU-ORQI12                                               
315000                                                                          
315100*    ACCEPT KOLLI-TIPACKN FROM DATE                                       
315200*    ACCEPT WS-TIPACTID-8 FROM TIME                                       
315300                                                                          
315400     MOVE WS-DAGENS-DATUM   TO  KOLLI-TIPACKN                             
315500     MOVE WS-TIDPUNKT       TO  KOLLI-TIPACTID                            
315600                                                                          
315700     MOVE WS-TIPACTID-6 TO KOLLI-TIPACTID                                 
315800     MOVE WS-IDKOLLI        TO  KOLLI-IDKOLLI                             
315900     MOVE WS-IDDISTR-NUM    TO  KOLLI-IDDISTR                             
316000     MOVE WS-IDKUNDNR-NUM   TO  KOLLI-IDKUNDNR                            
316100     MOVE WS-IDANSTNR       TO  KOLLI-IDPLOCK                             
316200     MOVE WS-IDDC           TO  KOLLI-IDDC                                
316300     MOVE ARB-IDTRP         TO  KOLLI-IDTRP                               
316400     MOVE ZERO              TO  KOLLI-IDKOLLI-FLER                        
316500                                KOLLI-IDTRPTNR                            
316600                                KOLLI-ADFLOMR                             
316700                                KOLLI-ADRUTNIV                            
316800                                KOLLI-ADVMODUL                            
316900                                KOLLI-ADHMODUL                            
317000                                KOLLI-DIKOLLIL                            
317100                                KOLLI-DIKOLLIB                            
317200                                KOLLI-DIKOLLIH                            
317300                                KOLLI-DIDMODUL                            
317400                                KOLLI-DIHMODUL                            
317500                                KOLLI-IDFAKLOP                            
317600                                KOLLI-IDFAKT                              
317700                                KOLLI-IDFAKT-EXP                          
317800                                KOLLI-KDEMBTYP                            
317900                                KOLLI-KDFARLIG-KOLLI                      
318000                                KOLLI-KDKOLSTA                            
318100                                KOLLI-KDORDKL                             
318200                                KOLLI-KVFLAMP-KOLLI                       
318300                                KOLLI-KVFALRAD                            
318400                                KOLLI-KVORDRAD                            
318500                                KOLLI-TIAAVVD-PATR                        
318600                                KOLLI-SUORDV-KOLLI                        
318700                                KOLLI-SUORDV-KLI-EXP                      
318800                                KOLLI-SUORDV-LOC                          
318900                                KOLLI-SUORDV-LOCPREL                      
319000                                KOLLI-TIFAKT                              
319100                                KOLLI-TIFAKT-EXP                          
319200                                KOLLI-TIFAKTID                            
319300                                KOLLI-TIFAKTID-EXP                        
319400                                KOLLI-TILASTN                             
319500                                KOLLI-TILASTID                            
319600                                KOLLI-TIPACKN                             
319700                                KOLLI-TIPACTID                            
319800                                KOLLI-VKORDBTO-KOLLI                      
319900                                KOLLI-VKORDNTO-KOLLI                      
320000                                KOLLI-VLORDBTO-KOLLI                      
320100     MOVE JA                TO  KOLLI-FLBANDST                            
320200                                WS-KOLLI-FLBANDST                         
320300     MOVE NEJ               TO  KOLLI-FLUTLAST                            
320400                                KOLLI-FLFRSUTS                            
320500     MOVE SPACE             TO  KOLLI-ADFLGEO                             
320600                                KOLLI-IDSUPREF                            
320700                                KOLLI-FLAUTFAK                            
320800                                KOLLI-FLTULLG                             
320900                                KOLLI-IDLBBET                             
321000                                KOLLI-KDKOLLI                             
321100                                KOLLI-KDARTURS-KOLLI                      
321200                                KOLLI-IDTULFTG                            
321300                                KOLLI-KDVALISO                            
321400                                KOLLI-KDVALISO-EXP                        
321600     MOVE ZERO               TO KOLLI-IDLASTN                             
321700                                KOLLI-DARFS                               
321800                                KOLLI-IDKOLLI-SAMP                        
321900                                KOLLI-SUEQFG                              
322000                                KOLLI-IDTULLNR                            
322100                                KOLLI-RETULKS                             
322200* OBS - HÄR INITIERAS NYA DDGS-FÄLT RENT GENERELLT                        
322300* OBS - KONTROLLERA OM DETTA ÄR KORREKT!!!                                
322400                                KOLLI-DASUPREF                            
322500                                KOLLI-TISUPTID                            
322600                                KOLLI-KDVIA                               
322700                                KOLLI-IDSHIPM                             
322800     MOVE SPACE             TO  KOLLI-IDSUPREF                            
322900                                KOLLI-IDLEVNR                             
323000                                KOLLI-KDSTASKLI                           
323100*                                                                         
323200     MOVE +1 TO FG-INDX                                                   
323300     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
323400         MOVE ZERO           TO KOLLI-IDPSN(FG-INDX)                      
323500                                KOLLI-VKART-FG(FG-INDX)                   
323600                                KOLLI-VLFG(FG-INDX)                       
323700         ADD +1 TO FG-INDX                                                
323800     END-PERFORM                                                          
323900*                                                                         
324000     PERFORM IMS-ISRT-KOLLI                                               
324100*                                                                         
324200     IF SEGMENT-FINNS-REDAN                                               
324300         MOVE FEL            TO  WS-BEHANDLING-TEST                       
324400         MOVE FEL-721 (INDX) TO  MOD-TEMFSFEL                             
324500     END-IF                                                               
324600     .                                                                    
324700     EJECT                                                                
324800 H-AVSLUT             SECTION.                                            
324900                                                                          
325000     SKIP3                                                                
325100     IF WS-IDDC NOT = W-IDDC-B6                                           
325200        MOVE WS-IDDC TO W-IDDC-B6                                         
325300        PERFORM IMS-GU-WDB601                                             
325400     END-IF                                                               
325500     IF WS-BEHANDLING-RATT                                                
325600       MOVE RAETT-1 (INDX)               TO   MOD-TEMFSINF                
325700       IF  WS-ORDERVIS-TRANS                                              
325800           MOVE '0605'                   TO MFS-IDTRANS                   
325900           MOVE 'W0T605U '               TO MSG-KDTRANS-1                 
326000           MOVE '4314'                   TO MSG-IDTRANS-1                 
326100           MOVE WS-KDMFSFOR              TO MSG-KDMFSFOR-1                
326200           MOVE 0605-LAENGD              TO MSG-KVLL                      
326300           MOVE SPACE                    TO 0605-MID-TEMFSFEL             
326400           MOVE JA                       TO 0605-MID-FLSVAR               
326500           MOVE 0605-MID                                                  
326600                       TO MSG-INDATA-MINUS-1-TRANSKOD                     
326700       ELSE                                                               
326800                                                                          
326900           IF MID-IDRADNR-FOM (12) = ALL '+'                              
327000           AND NOT STARTA-OM                                              
327100             MOVE WS-IDKUNDNR-NUM        TO TRANSFER-KUND                 
327200             EVALUATE TRUE                                                
327300             WHEN ((DCS-CDC AND (DIST08-URSP-RAPP                         
327400                            OR DIST08-URSP-RAPP-CDC                       
327500                            OR DIST08-URSP-SPX))                          
327600                  OR                                                      
327700                  (DCS-NDC-NA AND ((TRANSFER-KUNDNR AND                   
327800                                   DIST08-URSP-TRANSFER-NDC)              
327900                                 OR (RETUR-KUNDNR AND                     
328000                                   DIST08-URSP-RETUR-NDC)))               
328100                  OR                                                      
328200                  (DCS-NDC-NA AND DCS-IDLANDX2 = 'CA'                     
328300                              AND DIST08-URSP-RAPP-CDC))                  
328400                 PERFORM HB-LADDA-4317                                    
328500                 MOVE '4314'             TO   MFS-IDTRANS                 
328600                 MOVE 'W4O31701'         TO   MFS-IDMOD                   
328700                 MOVE WS-KDMFSFOR        TO   MSG-KDMFSFOR-1              
328800                 MOVE 4317-MOD-LAENGD TO      MSG-KVLL                    
328900                 MOVE 4317-MOD           TO   MSG-AREA                    
329000             WHEN MID-FLSISTAK         =    JA OR YES                     
329100                 MOVE '4318'             TO   MFS-IDTRANS                 
329200                 MOVE 'W4T318U '         TO   MSG-KDTRANS-1               
329300                 MOVE '4314'             TO   MSG-IDTRANS-1               
329400                 MOVE WS-KDMFSFOR        TO   MSG-KDMFSFOR-1              
329500                 MOVE 4318-LAENGD        TO   MSG-KVLL                    
329600                 PERFORM HA-LADDA-4318-AREA                               
329700                 MOVE 4318-MID                                            
329800                       TO MSG-INDATA-MINUS-1-TRANSKOD                     
329900             WHEN WS-KOLLI-FLBANDST    = NEJ                              
330000                 PERFORM HC-LADDA-4315                                    
330100                 MOVE '4314'             TO   MFS-IDTRANS                 
330200                 IF ENGLISH-TEXT                                          
330300                   MOVE 'W4O315N1'       TO   MFS-IDMOD                   
330400                 ELSE                                                     
330500                   MOVE 'W4O31501'       TO   MFS-IDMOD                   
330600                 END-IF                                                   
330700                 MOVE 4315-MOD-LAENGD TO      MSG-KVLL                    
330800                 MOVE 4315-MOD           TO   MSG-AREA                    
330900             WHEN OTHER                                                   
331000                 MOVE '4314'             TO   MFS-IDTRANS                 
331100                 MOVE WS-IDDISTR-NUM     TO   MOD-IDDISTR-UT              
331200                 INSPECT MOD-IDDISTR-UT                                   
331300                                  REPLACING LEADING ZERO BY SPACE         
331400                 MOVE WS-IDKUNDNR-NUM    TO   MOD-IDKUNDNR-UT             
331500                 INSPECT MOD-IDKUNDNR-UT                                  
331600                                  REPLACING LEADING ZERO BY SPACE         
331700                 MOVE WS-IDORDNR         TO   MOD-IDORDNR-UT              
331800                 INSPECT MOD-IDORDNR-UT                                   
331900                                  REPLACING LEADING ZERO BY SPACE         
332000                 MOVE MOD-W4O31401       TO   MSG-AREA                    
332100             END-EVALUATE                                                 
332200           ELSE                                                           
332300                                                                          
332400            IF  STARTA-OM                                                 
332500                 MOVE '4314'             TO   MFS-IDTRANS                 
332600                 MOVE 'W4T314  '         TO   MSG-KDTRANS-1               
332700                 MOVE '4314'             TO   MSG-IDTRANS-1               
332800                 MOVE WS-KDMFSFOR        TO   MSG-KDMFSFOR-1              
332900                 MOVE 4314-MID-LAENGD    TO   MSG-KVLL                    
333000                 MOVE NEXT-MID                                            
333100                       TO MSG-INDATA-MINUS-1-TRANSKOD                     
333200            ELSE                                                          
333300            IF DCS-NDC-NA OR (DCS-SDC AND DCS-IDLANDX2 NOT = 'SE')        
333400              MOVE YES                   TO MOD-FLFORTSK                  
333500            ELSE                                                          
333600              MOVE JA                    TO MOD-FLFORTSK                  
333700            END-IF                                                        
333800            MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDRADNR-FOM-ATTR (1)         
333900            MOVE MID-IDRADNR-FOM (12)    TO MOD-IDRADNR-FOM-S             
334000            MOVE MID-IDRADNR-TOM (12)    TO MOD-IDRADNR-TOM-S             
334100            IF   MID-KVLEVART (12) NOT NUMERIC                            
334200                 MOVE ZERO               TO MOD-KVLEVART-S                
334300            ELSE                                                          
334400                 MOVE MID-KVLEVART (12)  TO MOD-KVLEVART-S                
334500            END-IF                                                        
334600            MOVE '4314'                  TO   MFS-IDTRANS                 
334700                                                                          
334800            IF MID-FLSISTAK = JA OR YES                                   
334900               IF DCS-NDC-NA OR                                           
335000                 (DCS-SDC AND DCS-IDLANDX2 NOT = 'SE')                    
335100                  MOVE YES               TO MOD-FLSISTAK                  
335200               ELSE                                                       
335300                  MOVE JA                TO MOD-FLSISTAK                  
335400               END-IF                                                     
335500            END-IF                                                        
335600            MOVE WS-IDDISTR-NUM          TO   MOD-IDDISTR-UT              
335700            INSPECT MOD-IDDISTR-UT                                        
335800                             REPLACING LEADING ZERO BY SPACE              
335900            MOVE WS-IDKUNDNR-NUM         TO   MOD-IDKUNDNR-UT             
336000            INSPECT MOD-IDKUNDNR-UT                                       
336100                             REPLACING LEADING ZERO BY SPACE              
336200            MOVE WS-IDORDNR              TO   MOD-IDORDNR-UT              
336300            INSPECT MOD-IDORDNR-UT                                        
336400                             REPLACING LEADING ZERO BY SPACE              
336500            MOVE MOD-W4O31401            TO   MSG-AREA                    
336600            END-IF                                                        
336700           END-IF                                                         
336800       END-IF                                                             
336900     ELSE                                                                 
337000       EVALUATE TRUE                                                      
337100       WHEN  STARTA-OM                                                    
337200       AND SPAR-MID-FLFORTSK = 'O'                                        
337300           IF DCS-NDC-NA OR                                               
337400             (DCS-SDC AND DCS-IDLANDX2 NOT = 'SE')                        
337500             MOVE YES                    TO MOD-FLFORTSK                  
337600           ELSE                                                           
337700             MOVE JA                     TO MOD-FLFORTSK                  
337800           END-IF                                                         
337900           MOVE 1                        TO MID-IDX1                      
338000           PERFORM UNTIL MID-IDX1 NOT < MID-MAX-RAD                       
338100            OR     SPAR-MID-IDRADNR-FOM (MID-IDX1) = ALL '+'              
338200               MOVE SPAR-MID-IDRADNR-FOM (MID-IDX1) TO                    
338300                    MOD-IDRADNR-FOM (MID-IDX1)                            
338400               IF  SPAR-MID-IDRADNR-TOM (MID-IDX1) NOT = ALL '+'          
338500               AND SPAR-MID-IDRADNR-TOM (MID-IDX1) NOT = ZERO             
338600                   MOVE SPAR-MID-IDRADNR-TOM (MID-IDX1) TO                
338700                    MOD-IDRADNR-TOM (MID-IDX1)                            
338800               ELSE                                                       
338900                   MOVE MFS-RENSA-FAELT  TO                               
339000                                      MOD-IDRADNR-TOM (MID-IDX1)          
339100               END-IF                                                     
339200               IF  SPAR-MID-KVLEVART    (MID-IDX1) NOT = ALL '+'          
339300               AND SPAR-MID-KVLEVART    (MID-IDX1) NOT = ZERO             
339400                   MOVE SPAR-MID-KVLEVART    (MID-IDX1) TO                
339500                    MOD-KVLEVART    (MID-IDX1)                            
339600               ELSE                                                       
339700                   MOVE MFS-RENSA-FAELT  TO                               
339800                                      MOD-KVLEVART (MID-IDX1)             
339900               END-IF                                                     
340000               ADD 1                     TO MID-IDX1                      
340100           END-PERFORM                                                    
340200       WHEN  MID-FLFORTSK = 'O'                                           
340300           IF DCS-NDC-NA OR                                               
340400             (DCS-SDC AND DCS-IDLANDX2 NOT = 'SE')                        
340500             MOVE YES                    TO MOD-FLFORTSK                  
340600           ELSE                                                           
340700             MOVE JA                     TO MOD-FLFORTSK                  
340800           END-IF                                                         
340900           MOVE 1                        TO MID-IDX1                      
341000           PERFORM UNTIL MID-IDX1 NOT < MID-MAX-RAD                       
341100            OR     MID-IDRADNR-FOM (MID-IDX1) = ALL '+'                   
341200               MOVE MID-IDRADNR-FOM (MID-IDX1) TO                         
341300                    MOD-IDRADNR-FOM (MID-IDX1)                            
341400               IF  MID-IDRADNR-TOM (MID-IDX1) NOT = ALL '+'               
341500               AND MID-IDRADNR-TOM (MID-IDX1) NOT = ZERO                  
341600                   MOVE MID-IDRADNR-TOM (MID-IDX1) TO                     
341700                    MOD-IDRADNR-TOM (MID-IDX1)                            
341800               ELSE                                                       
341900                   MOVE MFS-RENSA-FAELT  TO                               
342000                                      MOD-IDRADNR-TOM (MID-IDX1)          
342100               END-IF                                                     
342200               IF  MID-KVLEVART    (MID-IDX1) NOT = ALL '+'               
342300               AND MID-KVLEVART    (MID-IDX1) NOT = ZERO                  
342400                   MOVE MID-KVLEVART    (MID-IDX1) TO                     
342500                    MOD-KVLEVART    (MID-IDX1)                            
342600               ELSE                                                       
342700                   MOVE MFS-RENSA-FAELT  TO                               
342800                                      MOD-KVLEVART (MID-IDX1)             
342900               END-IF                                                     
343000               ADD 1                     TO MID-IDX1                      
343100           END-PERFORM                                                    
343200       END-EVALUATE                                                       
343300       PERFORM IMS-ROLLBACK                                               
343400       IF  WS-ORDERVIS-TRANS                                              
343500           MOVE '0605'                   TO MFS-IDTRANS                   
343600           MOVE 'W0T605U '               TO MSG-KDTRANS-1                 
343700           MOVE '4314'                   TO MSG-IDTRANS-1                 
343800           MOVE WS-KDMFSFOR              TO MSG-KDMFSFOR-1                
343900           MOVE 0605-LAENGD              TO MSG-KVLL                      
344000           MOVE NEJ                      TO 0605-MID-FLSVAR               
344100           MOVE MOD-TEMFSFEL             TO 0605-MID-TEMFSFEL             
344200           MOVE 0605-MID                                                  
344300                       TO MSG-INDATA-MINUS-1-TRANSKOD                     
344400       ELSE                                                               
344500           MOVE NEJ                      TO   WS-STARTA-OM                
344600           MOVE '4314'                   TO   MFS-IDTRANS                 
344700           MOVE WS-IDDISTR-NUM           TO   MOD-IDDISTR-UT              
344800           INSPECT MOD-IDDISTR-UT                                         
344900                            REPLACING LEADING ZERO BY SPACE               
345000           MOVE WS-IDKUNDNR-NUM          TO   MOD-IDKUNDNR-UT             
345100           INSPECT MOD-IDKUNDNR-UT                                        
345200                            REPLACING LEADING ZERO BY SPACE               
345300           MOVE WS-IDORDNR               TO   MOD-IDORDNR-UT              
345400           INSPECT MOD-IDORDNR-UT                                         
345500                            REPLACING LEADING ZERO BY SPACE               
345600           IF  MID-FLFORTSK = 'O'                                         
345700               MOVE RAETT-2 (INDX)    TO   MOD-TEMFSINF                   
345800           END-IF                                                         
345900           MOVE MOD-W4O31401             TO   MSG-AREA                    
346000       END-IF                                                             
346100          IF WEIGHT-MISMATCH                                              
346200              PERFORM S21-SEND-OPEN                                       
346300              PERFORM S22-PUT-HEADER                                      
346400              PERFORM S24-WRITE-LINE                                      
346500              PERFORM S25-SEND-CLOSE                                      
346600          END-IF                                                          
346700     END-IF                                                               
346800     .                                                                    
346900     EJECT                                                                
347000 HA-LADDA-4318-AREA   SECTION.                                            
347100     SKIP3                                                                
347200     MOVE LOW-VALUE                TO 4318-MID                            
347300     MOVE WS-IDANSTNR              TO 4318-MID-IDANSTNR-IN                
347400     MOVE SPACE                    TO 4318-MID-IDANSTNR-UT                
347500     MOVE WS-IDDISTR-NUM           TO 4318-MID-IDDISTR-IN                 
347600     MOVE SPACE                    TO 4318-MID-IDDISTR-UT                 
347700     MOVE WS-IDKUNDNR-NUM          TO 4318-MID-IDKUNDNR-IN                
347800     MOVE SPACE                    TO 4318-MID-IDKUNDNR-UT                
347900     MOVE WS-IDORDNR               TO 4318-MID-IDORDNR-IN                 
348000     MOVE SPACE                    TO 4318-MID-IDORDNR-UT                 
348100     MOVE WS-IDKOLLI               TO 4318-MID-IDKOLLI-IN                 
348200     MOVE SPACE                    TO 4318-MID-IDKOLLI-UT                 
348300     MOVE WS-IDPRODNR              TO 4318-MID-IDPRODNR-IN                
348400     MOVE SPACE                    TO 4318-MID-IDPRODNR-UT                
348500     MOVE WS-IDDC                  TO 4318-MID-IDDC-IN                    
348600     MOVE SPACE                    TO 4318-MID-IDDC-UT                    
348700     MOVE '+'                      TO 4318-MID-FLSVAR                     
348800     MOVE SPACE                    TO 4318-MID-ADFLGEO                    
348900     MOVE ZERO                     TO 4318-MID-ADFLOMR                    
349000                                      4318-MID-ADRUTNIV                   
349100                                                                          
349200     IF WS-KOLLI-FLBANDST = JA                                            
349300         MOVE '4314'               TO 4318-MID-IDTRANS-START              
349400     ELSE                                                                 
349500         MOVE '4315'               TO 4318-MID-IDTRANS-START              
349600     END-IF                                                               
349700                                                                          
349800     .                                                                    
349900     EJECT                                                                
350000 HB-LADDA-4317        SECTION.                                            
350100                                                                          
350200     MOVE LOW-VALUE                TO 4317-MOD                            
350300     MOVE '4317'                   TO 4317-MOD-IDTRANS                    
350400     MOVE SPACE                    TO 4317-MOD-TEMFSFEL                   
350500     MOVE MFS-RENSA-FAELT          TO 4317-MOD-IDANSTNR-IN                
350600                                      4317-MOD-IDDISTR-IN                 
350700                                      4317-MOD-IDKUNDNR-IN                
350800                                      4317-MOD-IDORDNR-IN                 
350900                                      4317-MOD-IDKOLLI-IN                 
351000                                      4317-MOD-IDPRODNR-IN                
351100                                      4317-MOD-IDRADNR-S                  
351200                                      4317-MOD-KDARTURS-S                 
351300     MOVE WS-IDANSTNR              TO 4317-MOD-IDANSTNR-UT                
351400     INSPECT 4317-MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE         
351500     MOVE WS-IDDISTR-NUM           TO 4317-MOD-IDDISTR-UT                 
351600     INSPECT 4317-MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE         
351700     MOVE WS-IDKUNDNR-NUM          TO 4317-MOD-IDKUNDNR-UT                
351800     INSPECT 4317-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
351900     MOVE WS-IDORDNR               TO 4317-MOD-IDORDNR-UT                 
352000     INSPECT 4317-MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE         
352100     MOVE WS-IDKOLLI               TO 4317-MOD-IDKOLLI-UT                 
352200     INSPECT 4317-MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE         
352300     MOVE WS-IDPRODNR              TO 4317-MOD-IDPRODNR-UT                
352400     INSPECT 4317-MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE         
352500                                                                          
352600     IF WS-KOLLI-FLBANDST = JA                                            
352700         MOVE '4314'               TO 4317-MOD-IDTRANS-START              
352800     ELSE                                                                 
352900         MOVE '4315'               TO 4317-MOD-IDTRANS-START              
353000     END-IF                                                               
353100                                                                          
353200     IF  MID-FLSISTAK = JA OR YES                                         
353300         MOVE JA                   TO 4317-MOD-FLSISTAK                   
353400     END-IF                                                               
353500                                                                          
353600     MOVE MOD-TEMFSINF             TO 4317-MOD-TEMFSINF                   
353700     MOVE +1                       TO INX                                 
353800     PERFORM UNTIL INX NOT < 14                                           
353900         MOVE MFS-RENSA-FAELT      TO 4317-MOD-IDRADNR-ATTR (INX)         
354000                                      4317-MOD-IDRADNR (INX)              
354100                                      4317-MOD-KDARTURS-ATTR (INX)        
354200                                      4317-MOD-KDARTURS (INX)             
354300         ADD +1 TO INX                                                    
354400     END-PERFORM                                                          
354500     MOVE MFS-ADD-SAETT-CURSOR     TO 4317-MOD-IDRADNR-ATTR (1)           
354600                                                                          
354700     .                                                                    
354800     EJECT                                                                
354900 HC-LADDA-4315        SECTION.                                            
355000     SKIP3                                                                
355100     MOVE LOW-VALUE                TO 4315-MOD                            
355200     MOVE '4315'                   TO 4315-MOD-IDTRANS                    
355300                                      4315-MOD-IDTRANS-START              
355400     MOVE SPACE                    TO 4315-MOD-TEMFSFEL                   
355500     MOVE MFS-RENSA-FAELT          TO 4315-MOD-IDANSTNR-IN                
355600                                      4315-MOD-IDDISTR-IN                 
355700                                      4315-MOD-IDKUNDNR-IN                
355800                                      4315-MOD-IDORDNR-IN                 
355900                                      4315-MOD-IDKOLLI-IN                 
356000                                      4315-MOD-IDPRODNR-IN                
356100                                      4315-MOD-FLSISTAK                   
356200                                      4315-MOD-KDKOLLI                    
356300                                      4315-MOD-VKORDBTO-KOLLI             
356400                                      4315-MOD-KDEMBTYP                   
356500                                      4315-MOD-DIKOLLIL                   
356600                                      4315-MOD-DIKOLLIB                   
356700                                      4315-MOD-DIKOLLIH                   
356800                                      4315-MOD-ADFLGEO                    
356900                                      4315-MOD-ADFLOMR                    
357000                                      4315-MOD-ADRUTNIV                   
357100                                      4315-MOD-IDKOLLI-FOM                
357200                                      4315-MOD-IDKOLLI-TOM                
357300     MOVE WS-KDPRTVAL-ADRESSFL     TO 4315-MOD-PRTVAL-ADRESSFL            
357400     MOVE WS-KDPRTVAL-FOLJEFL      TO 4315-MOD-PRTVAL-FOLJEFL             
357500     MOVE WS-IDANSTNR              TO 4315-MOD-IDANSTNR-UT                
357600     INSPECT 4315-MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE         
357700     MOVE WS-IDDISTR-NUM           TO 4315-MOD-IDDISTR-UT                 
357800     INSPECT 4315-MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE         
357900     MOVE WS-IDKUNDNR-NUM          TO 4315-MOD-IDKUNDNR-UT                
358000     INSPECT 4315-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
358100     MOVE WS-IDORDNR               TO 4315-MOD-IDORDNR-UT                 
358200     INSPECT 4315-MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE         
358300     MOVE WS-IDKOLLI               TO 4315-MOD-IDKOLLI-UT                 
358400     INSPECT 4315-MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE         
358500     MOVE WS-IDPRODNR              TO 4315-MOD-IDPRODNR-UT                
358600     INSPECT 4315-MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE         
358700                                                                          
358800     MOVE MOD-TEMFSINF             TO 4315-MOD-TEMFSINF                   
358900                                                                          
359000     MOVE +1                       TO INX                                 
359100     PERFORM UNTIL INX NOT < 13                                           
359200         MOVE MFS-RENSA-FAELT      TO 4315-MOD-IDRADNR-FOM  (INX)         
359300                                      4315-MOD-IDRADNR-TOM  (INX)         
359400                                      4315-MOD-KVLEVART     (INX)         
359500         ADD +1 TO INX                                                    
359600     END-PERFORM                                                          
359700                                                                          
359800     .                                                                    
359900     EJECT                                                                
360000 I-TAG-BORT-LAASNING SECTION.                                             
360100                                                                          
360200     IF MID-IDRADNR-FOM (12) = ALL '+' AND                                
360300        WS-SAVE-KOLLI-FLBANDST = NEJ   AND                                
360400        WS-GODKAND-BILD                                                   
360500*                                                                         
360600        MOVE WS-IDPRODNR   TO W-4301-IDPRODNR                             
360700        PERFORM IMS-GHU-XXDU01                                            
360800        IF SEGMENT-FINNS                                                  
360900           MOVE WS-IDKOLLI  TO W-4302-IDKOLLI                             
361000           PERFORM IMS-GHNP-XXDU11                                        
361100           PERFORM UNTIL SEGMENT-SAKNAS                                   
361200              PERFORM IMS-DLET-XXDU                                       
361300              PERFORM IMS-GHNP-XXDU11                                     
361400           END-PERFORM                                                    
361500           PERFORM IMS-GNP-XXDU11-FIRST                                   
361600           IF SEGMENT-SAKNAS                                              
361700              PERFORM IMS-GHU-XXDU01                                      
361800              PERFORM IMS-DLET-XXDU                                       
361900           END-IF                                                         
362000        END-IF                                                            
362100     END-IF                                                               
362200     .                                                                    
362300     EJECT                                                                
362400 J-SKAPA-FOLJEFL-TRANS SECTION.                                           
362500                                                                          
362600     SKIP3                                                                
362700     MOVE '++++'            TO  4341-MID-IDDISTR-IN                       
362800     MOVE WS-IDDISTR-NUM    TO  4341-MID-IDDISTR-UT                       
362900     MOVE '++++++'          TO  4341-MID-IDKUNDNR-IN                      
363000     MOVE WS-IDKUNDNR-NUM   TO  4341-MID-IDKUNDNR-UT                      
363100     MOVE '+++++'           TO  4341-MID-IDORDNR-IN                       
363200     MOVE WS-IDORDNR        TO  4341-MID-IDORDNR-UT                       
363300     MOVE '+++++'           TO  4341-MID-IDKOLLI-IN                       
363400     MOVE WS-IDKOLLI        TO  4341-MID-IDKOLLI-UT                       
363500     MOVE '+++++'           TO  4341-MID-IDKOLLI-TOM-IN                   
363600     MOVE ZERO              TO  4341-MID-IDKOLLI-TOM-UT                   
363700     MOVE '++'              TO  4341-MID-KDPRTVAL-IN                      
363800     MOVE MID-KDPRTVAL-FOLJEFL TO 4341-MID-KDPRTVAL-UT                    
363900     MOVE '++'              TO  4341-MID-IDDC-IN                          
364000     MOVE WS-IDDC           TO  4341-MID-IDDC-UT                          
364100     MOVE 'N'               TO  4341-MID-FL-SVENSK-FSEDEL                 
364200                                                                          
364300     MOVE 'W4T341U '        TO  MSG-KDTRANS-1                             
364400     MOVE '431D'            TO  MSG-IDTRANS-1                             
364500     MOVE WS-KDMFSFOR       TO  MSG-KDMFSFOR-1                            
364600     MOVE 4341-LAENGD       TO  MSG-KVLL                                  
364700     MOVE 4341-MID          TO  MSG-INDATA-MINUS-1-TRANSKOD               
364800                                                                          
364900     PERFORM IMS-CHANGE-ALTMSG                                            
365000     PERFORM IMS-INSERT-ALTMSG                                            
365100     PERFORM IMS-PURGE-ALTMSG                                             
365200                                                                          
365300     .                                                                    
365400     EJECT                                                                
365500 K-SKAPA-NY-MID SECTION.                                                  
365600                                                                          
365700     SKIP3                                                                
365800     MOVE 1                 TO MID-IDX2                                   
365900*                                                                         
366000     PERFORM UNTIL MID-IDX2 NOT < MID-MAX-RAD                             
366100        MOVE ALL '+'     TO NEXT-MID-RAD (MID-IDX2)                       
366200        ADD  1           TO MID-IDX2                                      
366300     END-PERFORM                                                          
366400     MOVE OMSTART           TO NEXT-MID-FLFORTSK                          
366500     MOVE ZERO              TO WS-ANTAL-I-MID                             
366600     MOVE 1                 TO MID-IDX1                                   
366700     MOVE 0                 TO MID-IDX2                                   
366800*                                                                         
366900     PERFORM UNTIL MID-IDX1 NOT < MID-MAX-RAD                             
367000        IF  MID-IDRADNR-FOM (MID-IDX1) = ALL '+'                          
367100            MOVE 13         TO MID-IDX1                                   
367200        ELSE                                                              
367300          IF  MID-IDRADNR-TOM (MID-IDX1) = ZERO                           
367400              ADD 1         TO WS-ANTAL-I-MID                             
367500          ELSE                                                            
367600            MOVE MID-IDRADNR-TOM (MID-IDX1)                               
367700                             TO HJ-IDRADNR-TOM                            
367800            MOVE MID-IDRADNR-FOM (MID-IDX1)                               
367900                             TO HJ-IDRADNR-FOM                            
368000            COMPUTE WS-ANTAL-I-MID = WS-ANTAL-I-MID                       
368100                                + HJ-IDRADNR-TOM-N                        
368200                                - HJ-IDRADNR-FOM-N                        
368300                                + 1                                       
368400            END-COMPUTE                                                   
368500          END-IF                                                          
368600        END-IF                                                            
368700        EVALUATE TRUE                                                     
368800        WHEN  WS-ANTAL-I-MID > WS-MAX-ANT-FOR-OMST                        
368900            COMPUTE MAX-RAD-ANTAL = MID-IDX1 + 1                          
369000            ADD 1           TO MID-IDX2                                   
369100            MOVE MID-IDRADNR-TOM (MID-IDX1)                               
369200                             TO HJ-IDRADNR-TOM                            
369300            COMPUTE HJ-IDRADNR-FOM-N = HJ-IDRADNR-TOM-N                   
369400                                  - WS-ANTAL-I-MID                        
369500                                  + WS-MAX-ANT-FOR-OMST                   
369600                                  + 1                                     
369700            END-COMPUTE                                                   
369800            MOVE HJ-IDRADNR-FOM                                           
369900                            TO NEXT-MID-IDRADNR-FOM (MID-IDX2)            
370000            MOVE MID-IDRADNR-TOM (MID-IDX1) TO                            
370100                             NEXT-MID-IDRADNR-TOM (MID-IDX2)              
370200            COMPUTE HJ-IDRADNR-TOM-N  =                                   
370300                             HJ-IDRADNR-FOM-N - 1                         
370400            END-COMPUTE                                                   
370500            MOVE HJ-IDRADNR-TOM TO MID-IDRADNR-TOM (MID-IDX1)             
370600            IF   NEXT-MID-IDRADNR-FOM (MID-IDX2) =                        
370700                 NEXT-MID-IDRADNR-TOM (MID-IDX2)                          
370800                MOVE ALL '+' TO NEXT-MID-IDRADNR-TOM (MID-IDX2)           
370900            END-IF                                                        
371000                                                                          
371100            ADD 1           TO MID-IDX1                                   
371200            ADD 1           TO MID-IDX2                                   
371300            PERFORM UNTIL MID-IDX1 NOT < MID-MAX-RAD                      
371400                 OR MID-IDRADNR-FOM (MID-IDX1) = ALL '+'                  
371500                    MOVE MID-IDRADNR-FOM (MID-IDX1) TO                    
371600                             NEXT-MID-IDRADNR-FOM (MID-IDX2)              
371700                    IF  MID-IDRADNR-TOM (MID-IDX1) NOT = ZERO             
371800                        MOVE MID-IDRADNR-TOM (MID-IDX1) TO                
371900                             NEXT-MID-IDRADNR-TOM (MID-IDX2)              
372000                    END-IF                                                
372100                    IF  MID-KVLEVART (MID-IDX1) NOT = ALL '+'             
372200                    AND MID-KVLEVART (MID-IDX1) NOT = ZERO                
372300                        MOVE MID-KVLEVART    (MID-IDX1) TO                
372400                             NEXT-MID-KVLEVART    (MID-IDX2)              
372500                    END-IF                                                
372600                    MOVE ALL '+' TO MID-RAD (MID-IDX1)                    
372700                    ADD 1           TO MID-IDX1                           
372800                    ADD 1           TO MID-IDX2                           
372900            END-PERFORM                                                   
373000            MOVE 13                 TO MID-IDX1                           
373100        WHEN  WS-ANTAL-I-MID = WS-MAX-ANT-FOR-OMST                        
373200            COMPUTE MAX-RAD-ANTAL = MID-IDX1 + 1                          
373300            END-COMPUTE                                                   
373400            ADD 1           TO MID-IDX1                                   
373500            ADD 1           TO MID-IDX2                                   
373600            PERFORM UNTIL MID-IDX1 NOT < MID-MAX-RAD                      
373700                 OR MID-IDRADNR-FOM (MID-IDX1) = ALL '+'                  
373800                    MOVE MID-IDRADNR-FOM (MID-IDX1) TO                    
373900                             NEXT-MID-IDRADNR-FOM (MID-IDX2)              
374000                    IF  MID-IDRADNR-TOM (MID-IDX1) NOT = ZERO             
374100                        MOVE MID-IDRADNR-TOM (MID-IDX1) TO                
374200                             NEXT-MID-IDRADNR-TOM (MID-IDX2)              
374300                    END-IF                                                
374400                    IF  MID-KVLEVART (MID-IDX1) NOT = ALL '+'             
374500                    AND MID-KVLEVART (MID-IDX1) NOT = ZERO                
374600                        MOVE MID-KVLEVART    (MID-IDX1) TO                
374700                             NEXT-MID-KVLEVART    (MID-IDX2)              
374800                    END-IF                                                
374900                    MOVE ALL '+'  TO MID-RAD (MID-IDX1)                   
375000                    ADD 1           TO MID-IDX1                           
375100                    ADD 1           TO MID-IDX2                           
375200            END-PERFORM                                                   
375300            MOVE 13                 TO MID-IDX1                           
375400        END-EVALUATE                                                      
375500        ADD 1                       TO MID-IDX1                           
375600     END-PERFORM                                                          
375700                                                                          
375800     .                                                                    
375900     SKIP2                                                                
376000 L-HAMTA-ADRESS SECTION.                                                  
376100                                                                          
376200     MOVE WS-IDDISTR-NUM  TO TEST-IDDISTR                                 
376300                            W-4A1-IDDISTR                                 
376400     MOVE WS-IDKUNDNR-NUM TO W-4A1-IDKUNDNR                               
376500     MOVE WS-IDORDNR      TO W-4A1-IDORDNR                                
376600     PERFORM IMS-GU-KUNDORDER-SEK                                         
376700     MOVE KORD-IDDISTR      TO W-401-IDDISTR                              
376800     MOVE KORD-IDKUNDNR     TO W-401-IDKUNDNR                             
376900     MOVE KORD-IDORDNR5     TO W-401-IDORDNR                              
377000     MOVE KORD-IDPRODNR     TO W-401-IDPRODNR                             
377100     MOVE KORD-IDPLKLST     TO W-401-IDPLKLST                             
377200     MOVE KORD-IDORDER      TO W-201-IDORDER                              
377300     .                                                                    
377400     SKIP2                                                                
377500 M-INIT-TRANS-LL92 SECTION.                                               
377600                                                                          
377700      MOVE '4314'                 TO MFS-IDTRANS                          
377800      MOVE 'W4O314N1'             TO MFS-IDMOD                            
377900      MOVE +96                    TO MSG-KVLL                             
378000      MOVE FEL                    TO WS-INDATA-TEST                       
378100     .                                                                    
378200     SKIP2                                                                
378300 N-INIT-TRANS-LL8 SECTION.                                                
378400                                                                          
378500     MOVE '4314'                 TO MFS-IDTRANS                           
378600     MOVE 'W4O314N1'             TO MFS-IDMOD                             
378700     MOVE +8                     TO MSG-KVLL                              
378800     MOVE FEL                    TO WS-INDATA-TEST                        
378900     .                                                                    
379000     SKIP2                                                                
379100 O-INIT-FEL-TRANS SECTION.                                                
379200                                                                          
379300      MOVE NEJ                    TO WS-STARTA-OM                         
379400      IF WS-ORDERVIS-TRANS                                                
379500        MOVE '0605'               TO MFS-IDTRANS                          
379600        MOVE 'W0T605U '           TO MSG-KDTRANS-1                        
379700        MOVE '4314'               TO MSG-IDTRANS-1                        
379800        MOVE WS-KDMFSFOR          TO MSG-KDMFSFOR-1                       
379900        MOVE 0605-LAENGD          TO MSG-KVLL                             
380000        MOVE MOD-TEMFSFEL         TO 0605-MID-TEMFSFEL                    
380100        MOVE NEJ                  TO 0605-MID-FLSVAR                      
380200        MOVE 0605-MID                                                     
380300                    TO MSG-INDATA-MINUS-1-TRANSKOD                        
380400      ELSE                                                                
380500        MOVE '4314'               TO MFS-IDTRANS                          
380600        MOVE MOD-W4O31401         TO MSG-AREA                             
380700      END-IF                                                              
380800     .                                                                    
380900     SKIP2                                                                
381000 P-EVALUATE-INSERT-MSG-TRANS SECTION.                                     
381100                                                                          
381200         EVALUATE TRUE                                                    
381300         WHEN MFS-IDTRANS = '4314'                                        
381400             IF  STARTA-OM                                                
381500                 PERFORM IMS-CHANGE-ALTMSG                                
381600                 PERFORM IMS-INSERT-ALTMSG                                
381700             ELSE                                                         
381800                 PERFORM IMS-INSERT-MSG                                   
381900             END-IF                                                       
382000         WHEN MFS-IDTRANS = '0605'                                        
382100             PERFORM IMS-INSERT-ALT0605MSG                                
382200         WHEN OTHER                                                       
382300             PERFORM IMS-CHANGE-ALTMSG                                    
382400             PERFORM IMS-INSERT-ALTMSG                                    
382500         END-EVALUATE                                                     
382600     .                                                                    
382700     SKIP2                                                                
382800* MFS SEKTIONER                                                           
382900                                                                          
383000 S02-RENSA-MOD-FALT   SECTION.                                            
383100                                                                          
383200     MOVE MFS-RENSA-FAELT             TO MOD-FLSISTAK                     
383300                                         MOD-FLFORTSK                     
383400                                         MOD-KDPRTVAL-FOLJEFL             
383500                                                                          
383600     MOVE +1  TO BILD-RAD                                                 
383700     MOVE +13 TO MAX-RAD-ANTAL                                            
383800     PERFORM UNTIL BILD-RAD NOT < MAX-RAD-ANTAL                           
383900         MOVE MFS-RENSA-FAELT       TO MOD-IDRADNR-FOM (BILD-RAD)         
384000                                       MOD-IDRADNR-TOM (BILD-RAD)         
384100                                       MOD-KVLEVART (BILD-RAD)            
384200         ADD +1 TO BILD-RAD                                               
384300     END-PERFORM                                                          
384400                                                                          
384500     .                                                                    
384600     SKIP3                                                                
384700 S04-ADD-LAES-IN-FAELT  SECTION.                                          
384800                                                                          
384900     MOVE +1 TO BILD-RAD                                                  
385000     PERFORM UNTIL BILD-RAD NOT < MAX-RAD-ANTAL                           
385100         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
385200                                 MOD-IDRADNR-FOM-ATTR (BILD-RAD)          
385300                                 MOD-IDRADNR-TOM-ATTR (BILD-RAD)          
385400                                 MOD-KVLEVART-ATTR (BILD-RAD)             
385500         ADD +1 TO BILD-RAD                                               
385600     END-PERFORM                                                          
385700                                                                          
385800     .                                                                    
385900     EJECT                                                                
386000 S10-UPPD-SPAR-KOLLI    SECTION.                                          
386100     SKIP3                                                                
386200     COMPUTE ARB-KOLLI-VKORDNTO ROUNDED = ARB-KOLLI-VKORDNTO +            
386300                    SPAR-PRAD-VKARTNTO * SPAR-PRAD-KVLEVART               
386400     END-COMPUTE                                                          
386500*                                                                         
386600     MOVE WS-IDDISTR-NUM              TO  TEST-IDDISTR                    
386700                                                                          
386800     IF DIST79-DEALER-PRICE                                               
386900      IF  ORAD-PRARTNTO-LOCPREL > 0                                       
387000       COMPUTE ARB-KOLLI-SUORDV-LOCPREL = ARB-KOLLI-SUORDV-LOCPREL        
387100           + SPAR-PRAD-PRARTNTO-LOCPREL * SPAR-PRAD-KVLEVART              
387200      ELSE                                                                
387300       COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC                
387400           + SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                  
387500      END-IF                                                              
387600     ELSE                                                                 
387800       IF DIST79-ECOM-PRICE                                               
387900         COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC              
388000             + SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                
388100       ELSE                                                               
388200         COMPUTE ARB-KOLLI-SUORDV = ARB-KOLLI-SUORDV +                    
388300           SPAR-PRAD-PRARTNTO * SPAR-PRAD-KVLEVART                        
388400         COMPUTE ARB-KOLLI-SUORDV-EXP = ARB-KOLLI-SUORDV-EXP +            
388500           SPAR-PRAD-PRAVCOST * SPAR-PRAD-KVLEVART                        
388600       END-IF                                                             
388700     END-IF                                                               
388800     MOVE SPAR-PRAD-KDVALISO     TO ARB-KOLLI-KDVALISO                    
388900     MOVE SPAR-PRAD-KDVALISO-EXP TO ARB-KOLLI-KDVALISO-EXP                
389000*                                                                         
389100     IF      SPAR-PRAD-KVFLAMP   >  ZERO                                  
389200        AND (SPAR-PRAD-KVFLAMP   <  ARB-KOLLI-KVFLAMP                     
389300        OR   ARB-KOLLI-KVFLAMP   =  ZERO)                                 
389400         MOVE SPAR-PRAD-KVFLAMP  TO ARB-KOLLI-KVFLAMP                     
389500     END-IF                                                               
389600*                                                                         
389700     IF      SPAR-PRAD-KDFARLIG  =  +2 OR +3 OR +4 OR +7                  
389800     AND     SPAR-PRAD-KDFARLIG  >  ARB-KOLLI-KDFARLIG                    
389900       MOVE  SPAR-PRAD-KDFARLIG TO ARB-KOLLI-KDFARLIG                     
390000     END-IF                                                               
390100                                                                          
390200     .                                                                    
390300     EJECT                                                                
390400 S11-UPPD-KOLLI-FRAN-ARB   SECTION.                                       
390500                                                                          
390600     ADD ARB-KOLLI-KVORDRAD      TO KOLLI-KVORDRAD                        
390700*                                                                         
390800     ADD ARB-KOLLI-VKORDNTO      TO KOLLI-VKORDNTO-KOLLI                  
390900*LK  ADD ARB-KOLLI-VKORDNTO      TO KOLLI-VKORDBTO-KOLLI                  
391000     ADD ARB-KOLLI-SUORDV        TO KOLLI-SUORDV-KOLLI                    
391100     ADD ARB-KOLLI-SUORDV-EXP    TO KOLLI-SUORDV-KLI-EXP                  
391200     ADD ARB-KOLLI-SUORDV-LOC    TO KOLLI-SUORDV-LOC                      
391300     ADD ARB-KOLLI-SUORDV-LOCPREL TO KOLLI-SUORDV-LOCPREL                 
391400     MOVE ARB-KOLLI-KDVALISO     TO KOLLI-KDVALISO                        
391500     MOVE ARB-KOLLI-KDVALISO-EXP TO KOLLI-KDVALISO-EXP                    
391600     ADD ARB-KOLLI-KVFALRAD      TO KOLLI-KVFALRAD                        
391700     MOVE ARB-KOLLI-KDORDKL      TO KOLLI-KDORDKL                         
391800     MOVE ARB-KOLLI-FLAUTFAK     TO KOLLI-FLAUTFAK                        
391900                                                                          
392000     IF MID-VKORDBTO-KOLLI = ALL '+'                                      
392100* FOR DIST03-SVERIGE GROSS WEIGHT WILL BE REPORTED TO                     
392200       ADD ARB-KOLLI-VKORDNTO      TO KOLLI-VKORDBTO-KOLLI                
392300     ELSE                                                                 
392400       MOVE WS-MOD-VKORDBTO        TO KOLLI-VKORDBTO-KOLLI                
392500     END-IF                                                               
392600                                                                          
392700     IF  KOLLI-FLBANDST = NEJ                                             
392800     AND KOLLI-VKORDNTO-KOLLI > KOLLI-VKORDBTO-KOLLI                      
392900         MOVE KOLLI-VKORDBTO-KOLLI TO KOLLI-VKORDNTO-KOLLI                
393000     END-IF                                                               
393100*                                                                         
393200     IF KOLLI-VKORDNTO-KOLLI >= KOLLI-VKORDBTO-KOLLI                      
393300       ADD 0.1                TO KOLLI-VKORDBTO-KOLLI                     
393400     END-IF                                                               
393500*                                                                         
393600     IF      ARB-KOLLI-KVFLAMP   >  ZERO                                  
393700        AND (ARB-KOLLI-KVFLAMP   <  KOLLI-KVFLAMP-KOLLI                   
393800        OR   KOLLI-KVFLAMP-KOLLI =  ZERO)                                 
393900         MOVE ARB-KOLLI-KVFLAMP  TO KOLLI-KVFLAMP-KOLLI                   
394000     END-IF                                                               
394100*                                                                         
394200     IF ARB-KOLLI-KDFARLIG       >  KOLLI-KDFARLIG-KOLLI                  
394300         MOVE ARB-KOLLI-KDFARLIG TO KOLLI-KDFARLIG-KOLLI                  
394400     END-IF                                                               
394500     .                                                                    
394600     EJECT                                                                
394700 S12-BERAEKNA-FG-FAELT SECTION.                                           
394800                                                                          
394900     COMPUTE TAB-VLFG(FG-INDX) = TAB-VLFG(FG-INDX)    +                   
395000                                 (SPAR-VLFG           *                   
395100                                  KKOLLI-KVLEVART)                        
395200     IF SPAR-IDPSN = 10 OR 11                                             
395300       COMPUTE TAB-VKART-FG(FG-INDX) = TAB-VKART-FG(FG-INDX) +            
395400                                       (SPAR-VKART-FG        *            
395500                                        KKOLLI-KVLEVART)                  
395600     ELSE                                                                 
395700       MOVE ZERO TO TAB-VKART-FG(FG-INDX)                                 
395800     END-IF                                                               
395900                                                                          
396000     MOVE 10 TO FG-INDX                                                   
396100     .                                                                    
396200     EJECT                                                                
396300                                                                          
396400 S13-DATA-TILL-DEL-NOTE SECTION.                                          
396500                                                                          
396600     MOVE WS-IDDISTR-NUM         TO TEST-IDDISTR                          
396700     IF DIST07-USA-RETAILER-DNOTE                                         
396800     OR DIST07-CAN-RETAILER                                               
396900                                                                          
397000        INITIALIZE DNOT-ORDER-INFO                                        
397100                                                                          
397200        MOVE PROGRAM-NAMN             TO DNOT-IDPGM                       
397300        MOVE WS-SPAR-IDORDER          TO DNOT-IDORDER                     
397400        MOVE WS-SPAR-IDARTNR          TO DNOT-IDARTNR                     
397500        MOVE WS-SPAR-IDDC             TO DNOT-IDDC                        
397600        MOVE WS-SPAR-BEART            TO DNOT-BEART-USA                   
397700        MOVE WS-SPAR-KVBEART          TO DNOT-KVBEART                     
397800        MOVE WS-SPAR-FLTILLK          TO DNOT-FLTILLK                     
397900        MOVE WS-SPAR-IDKUNDRF-RO      TO DNOT-IDKUNDRF-RO                 
398000        MOVE WS-SPAR-IDPURAD          TO DNOT-IDPURAD                     
398100        MOVE KKOLLI-IDKOLLI           TO DNOT-IDKOLLI                     
398200        MOVE KKOLLI-IDPRODNR          TO DNOT-IDPRODNR                    
398300        MOVE KKOLLI-KVLEVART          TO DNOT-KVLEVART                    
398400                                                                          
398500        CALL W411DNOT USING DNOT-W411DNOT                                 
398600                            DNOT-ORQP-PCB                                 
398700                            DNOT-ORQP2-PCB                                
398800                            DNOT-ORQP3-PCB                                
398900                            DNOT-4013-PCB                                 
399000                            DNOT-BENA-PCB                                 
399100     END-IF                                                               
399200     .                                                                    
399300     EJECT                                                                
399400                                                                          
399500 S19-CONVERT-LB-TO-KG                     SECTION.                        
399600                                                                          
399700     COMPUTE WS-MOD-VKORDBTO =                                            
399800             WS-MOD-VKORDBTO * CONV-LB-TO-KG                              
399900     END-COMPUTE                                                          
400000     .                                                                    
400100     SKIP2                                                                
400200 S21-SEND-OPEN SECTION.                                                   
400300                                                                          
400400     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
400500     MOVE 'OPEN'                  TO SEND-KDFUNC                          
400600     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
400700                                     SEND-OPEN-AREA                       
400800     IF SEND-KDRC > ZERO                                                  
400900       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
401000       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
401100       DELIMITED BY SIZE INTO FELTEXT                                     
401200       DISPLAY FELTEXT                                                    
401300       CALL FELLOG                                                        
401400     END-IF                                                               
401500     .                                                                    
401600     EJECT                                                                
401700 S22-PUT-HEADER        SECTION.                                           
401800                                                                          
401900     MOVE 001                    TO HDR-REQU-IDMSGVER                     
402000     MOVE 'R'                    TO HDR-REQU-KDPGMACT                     
402100     MOVE PROGRAM-NAMN           TO HDR-REQU-IDUSER                       
402200     MOVE 'WRONGWEIGHT'          TO HDR-IDOUTTYPE                         
402300     MOVE '003'                  TO HDR-IDOUTREC                          
402400     MOVE '003'                  TO HDR-IDLIST                            
402500     MOVE 'PUT'                  TO SEND-KDFUNC                           
402600     MOVE LENGTH OF HDR-AREA     TO SEND-KVDLEN                           
402700     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
402800                                    SEND-KVDLEN                           
402900                                    HDR-AREA                              
403000     IF SEND-KDRC > ZERO                                                  
403100       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
403200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
403300       DELIMITED BY SIZE INTO FELTEXT                                     
403400       DISPLAY FELTEXT                                                    
403500       CALL FELLOG                                                        
403600     END-IF                                                               
403700     .                                                                    
403800 S23-MOVE-LINEDATA SECTION.                                               
403900     MOVE WS-IDDISTR-NUM           TO HEAD-DIST                           
404000     MOVE WS-IDKUNDNR-NUM          TO HEAD-IDKUNDNR                       
404100     MOVE WS-IDORDNR               TO HEAD-IDORDER                        
404200     MOVE WS-IDKOLLI               TO HEAD-IDKOLLI                        
404300     MOVE WS-KDKOLLI               TO HEAD-KDKOLLI                        
404400     MOVE WS-IDANSTNR              TO HEAD-IDPLKLST                       
404500     MOVE WS-MOD-VKORDBTO          TO HEAD-VKORDBTO                       
404600     MOVE WS-DAGENS-DATUM          TO HEAD-DATE                           
404700     MOVE WS-TIDPUNKT              TO HEAD-TIME                           
404800     .                                                                    
404900 S24-WRITE-LINE  SECTION.                                                 
405000     MOVE 1 TO LINE-IX                                                    
405100     PERFORM UNTIL LINE-IX > 16                                           
405200        MOVE TAB-LINE(LINE-IX)      TO SEND-AREA                          
405300        PERFORM  S25-PUT-LINE                                             
405400        ADD 1 TO LINE-IX                                                  
405500     END-PERFORM                                                          
405600                                                                          
405700     MOVE 1 TO LINE-IX                                                    
405800     PERFORM UNTIL LINE-IX > MAX-TAB                                      
405900        MOVE LINE-TAB(LINE-IX)      TO SEND-AREA                          
406000        PERFORM  S25-PUT-LINE                                             
406100        ADD 1 TO LINE-IX                                                  
406200     END-PERFORM                                                          
406300                                                                          
406400      IF MAX-RAD > MAX-LINES                                              
406500        MOVE LINE-END     TO SEND-AREA                                    
406600        PERFORM  S25-PUT-LINE                                             
406700      END-IF                                                              
406800     .                                                                    
406900 S25-PUT-LINE     SECTION.                                                
407000                                                                          
407100     MOVE 'PUT'                     TO SEND-KDFUNC                        
407200     MOVE LENGTH OF SEND-AREA       TO SEND-KVDLEN                        
407300     CALL WZ01SEND               USING SEND-CONTROL-AREA                  
407400                                       SEND-KVDLEN                        
407500                                       SEND-AREA                          
407600     IF SEND-KDRC > ZERO                                                  
407700       MOVE SEND-KDRC               TO KDRC-DISPLAY                       
407800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
407900       DELIMITED BY SIZE INTO FELTEXT                                     
408000       DISPLAY FELTEXT                                                    
408100       CALL FELLOG                                                        
408200     END-IF                                                               
408300     .                                                                    
408400 S25-SEND-CLOSE SECTION.                                                  
408500                                                                          
408600     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
408700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
408800                                                                          
408900     IF SEND-KDRC > 0                                                     
409000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
409100       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
409200       DELIMITED BY SIZE INTO FELTEXT                                     
409300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
409400     END-IF                                                               
409500     .                                                                    
409600     EJECT                                                                
409700* IMS SEKTIONER                                                           
409800     SKIP1                                                                
409900 IMS-GET-MSG SECTION.                                                     
410000                                                                          
410100     MOVE '  QC' TO GODK-STATUSKODER                                      
410200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
410300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
410400     PERFORM IMS-STATUSKONTROLL                                           
410500     .                                                                    
410600     SKIP1                                                                
410700 IMS-INSERT-ALT0605MSG SECTION.                                           
410800                                                                          
410900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
411000     MOVE SPACE TO GODK-STATUSKODER                                       
411100     CALL CBLTDLI USING ISRT ALT0605-PCB MSG-IO-AREA                      
411200     MOVE ALT0605-STATUS-CODE TO STATUS-WS                                
411300     PERFORM IMS-STATUSKONTROLL                                           
411400     .                                                                    
411500     SKIP1                                                                
411600 IMS-INSERT-MSG SECTION.                                                  
411700                                                                          
411800     IF NOT ENGLISH-TEXT                                                  
411900       MOVE '0' TO MFS-KDHUVOMR                                           
412000     END-IF                                                               
412100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
412200     MOVE SPACE TO GODK-STATUSKODER                                       
412300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
412400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
412500     PERFORM IMS-STATUSKONTROLL                                           
412600     .                                                                    
412700     SKIP1                                                                
412800 IMS-CHANGE-ALTMSG       SECTION.                                         
412900     SKIP2                                                                
413000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
413100     MOVE '  ' TO GODK-STATUSKODER                                        
413200     CALL CBLTDLI USING CHNG                                              
413300                          ALT-PCB                                         
413400                          MSG-KDTRANS-1                                   
413500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
413600     PERFORM IMS-STATUSKONTROLL                                           
413700     .                                                                    
413800     SKIP1                                                                
413900 IMS-INSERT-ALTMSG SECTION.                                               
414000     SKIP1                                                                
414100                                                                          
414200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
414300     MOVE SPACE TO GODK-STATUSKODER                                       
414400     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
414500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
414600     PERFORM IMS-STATUSKONTROLL                                           
414700     .                                                                    
414800     SKIP1                                                                
414900 IMS-PURGE-ALTMSG SECTION.                                                
415000     SKIP1                                                                
415100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
415200     MOVE SPACE TO GODK-STATUSKODER                                       
415300     CALL CBLTDLI USING PURG ALT-PCB                                      
415400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
415500     PERFORM IMS-STATUSKONTROLL                                           
415600     .                                                                    
415700     EJECT                                                                
415800 IMS-PURGE-ALT4333-MSG  SECTION.                                          
415900     SKIP2                                                                
416000     MOVE LOW-VALUE TO 4333-MID-Z1 4333-MID-Z2                            
416100     MOVE '  ' TO GODK-STATUSKODER                                        
416200     CALL CBLTDLI USING PURG ALT4333-PCB                                  
416300                             4333-MID-IO-AREA                             
416400     MOVE ALT4333-STATUS-CODE TO STATUS-WS                                
416500     PERFORM IMS-STATUSKONTROLL                                           
416600     .                                                                    
416700     EJECT                                                                
416800 IMS-GU-KUNDORDER SECTION.                                                
416900     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
417000            DELIMITED BY SIZE INTO SSA1                                   
417100     MOVE '    ' TO GODK-STATUSKODER                                      
417200     CALL CBLTDLI USING GU     WDE4-PCB DLI-IO-E401 SSA1                  
417300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
417400     PERFORM IMS-STATUSKONTROLL                                           
417500     .                                                                    
417600     SKIP1                                                                
417700 IMS-GU-WDE601    SECTION.                                                
417800                                                                          
417900     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
418000            DELIMITED BY SIZE INTO SSA1                                   
418100     MOVE '  GE' TO GODK-STATUSKODER                                      
418200     CALL CBLTDLI USING GU    WDE62-PCB DLI-IO-E601 SSA1                  
418300     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
418400     PERFORM IMS-STATUSKONTROLL                                           
418500     SKIP3                                                                
418600     .                                                                    
418700 IMS-GU-RAD SECTION.                                                      
418800     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
418900            DELIMITED BY SIZE INTO SSA1                                   
419000     STRING 'WDE411  (IDPURAD  =' W-WDE420-IDPURAD-X ')'                  
419100            DELIMITED BY SIZE INTO SSA2                                   
419200     MOVE '  GE' TO GODK-STATUSKODER                                      
419300     CALL CBLTDLI USING GU     WDE4-PCB DLI-IO-E411 SSA1 SSA2             
419400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
419500     PERFORM IMS-STATUSKONTROLL                                           
419600     .                                                                    
419700     EJECT                                                                
419800 IMS-GU-KUNDORDER-SEK-INV SECTION.                                        
419900     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
420000                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
420100            DELIMITED BY SIZE INTO SSA1                                   
420200     MOVE 'WDE401 ' TO SSA2                                               
420300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
420400     CALL CBLTDLI USING GU    WDE4B-PCB DLI-IO-E401 SSA1 SSA2             
420500     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
420600     PERFORM IMS-STATUSKONTROLL                                           
420700     .                                                                    
420800     SKIP3                                                                
420900 IMS-GU-WDE411-01-BSEQ SECTION.                                           
421000     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
421100            DELIMITED BY SIZE INTO SSA1                                   
421200     MOVE 'WDE401 ' TO SSA2                                               
421300     MOVE '  ' TO GODK-STATUSKODER                                        
421400     CALL CBLTDLI USING GU   WDE4B-PCB DLI-IO-E401 SSA1 SSA2              
421500     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
421600     PERFORM IMS-STATUSKONTROLL                                           
421700     .                                                                    
421800     EJECT                                                                
421900 IMS-GHU-RAD-SEK  SECTION.                                                
422000     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
422100            DELIMITED BY SIZE INTO SSA1                                   
422200     MOVE '    ' TO GODK-STATUSKODER                                      
422300     CALL CBLTDLI USING GHU    WDE42-PCB DLI-IO-E411 SSA1                 
422400     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
422500     PERFORM IMS-STATUSKONTROLL                                           
422600     .                                                                    
422700     SKIP1                                                                
422800 IMS-GU-RAD-SEK  SECTION.                                                 
422900     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
423000            DELIMITED BY SIZE INTO SSA1                                   
423100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
423200     CALL CBLTDLI USING GU  WDE42-PCB DLI-IO-E411 SSA1                    
423300     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
423400     PERFORM IMS-STATUSKONTROLL                                           
423500     .                                                                    
423600     SKIP1                                                                
423700 IMS-GHN-RAD-SEK  SECTION.                                                
423800     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
423900                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
424000            DELIMITED BY SIZE INTO SSA1                                   
424100     MOVE '  ' TO GODK-STATUSKODER                                        
424200     CALL CBLTDLI USING GHN    WDE42-PCB DLI-IO-E411 SSA1                 
424300     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
424400     PERFORM IMS-STATUSKONTROLL                                           
424500     .                                                                    
424600     SKIP1                                                                
424700 IMS-GN-RAD-SEK  SECTION.                                                 
424800     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
424900                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
425000            DELIMITED BY SIZE INTO SSA1                                   
425100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
425200     CALL CBLTDLI USING GN  WDE42-PCB DLI-IO-E411 SSA1                    
425300     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
425400     PERFORM IMS-STATUSKONTROLL                                           
425500     .                                                                    
425600     SKIP1                                                                
425700 IMS-REPL-BEHANDLAD-RAD SECTION.                                          
425800     MOVE '    ' TO GODK-STATUSKODER                                      
425900     CALL CBLTDLI USING REPL WDE42-PCB DLI-IO-E411                        
426000     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
426100     PERFORM IMS-STATUSKONTROLL                                           
426200     .                                                                    
426300     EJECT                                                                
426400 IMS-GHNP-KOLLI-KOPPL    SECTION.                                         
426500     STRING 'WDE421  *F(WDE421KY =' W-WDE421-IDKOLLI-X ')'                
426600            DELIMITED BY SIZE INTO SSA1                                   
426700     MOVE '  ' TO GODK-STATUSKODER                                        
426800     CALL CBLTDLI USING GHNP WDE42-PCB DLI-IO-E421 SSA1                   
426900     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
427000     PERFORM IMS-STATUSKONTROLL                                           
427100     .                                                                    
427200     SKIP1                                                                
427300 IMS-GNP-KOLLI-KOPPL  SECTION.                                            
427400     STRING 'WDE421  (WDE421KY =' W-WDE421-IDKOLLI-X ')'                  
427500            DELIMITED BY SIZE INTO SSA1                                   
427600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
427700     CALL CBLTDLI USING GHNP WDE42-PCB DLI-IO-E421 SSA1                   
427800     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
427900     PERFORM IMS-STATUSKONTROLL                                           
428000     .                                                                    
428100     SKIP1                                                                
428200 IMS-REPL-KOLLI-KOPPL  SECTION.                                           
428300     MOVE '  '   TO GODK-STATUSKODER                                      
428400     CALL CBLTDLI USING REPL WDE42-PCB DLI-IO-E421                        
428500     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
428600     PERFORM IMS-STATUSKONTROLL                                           
428700     .                                                                    
428800     SKIP1                                                                
428900 IMS-ISRT-KOLLI-KOPPL  SECTION.                                           
429000     MOVE   'WDE421 '         TO   SSA1                                   
429100     MOVE '  II' TO GODK-STATUSKODER                                      
429200     CALL CBLTDLI USING ISRT WDE42-PCB DLI-IO-E421 SSA1                   
429300     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
429400     PERFORM IMS-STATUSKONTROLL                                           
429500     .                                                                    
429600     EJECT                                                                
429700 IMS-GHU-KOLLIREG SECTION.                                                
429800     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
429900            DELIMITED BY SIZE INTO SSA1                                   
430000     MOVE '  GE' TO GODK-STATUSKODER                                      
430100     CALL CBLTDLI USING GHU    WDE62-PCB DLI-IO-E601 SSA1                 
430200     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
430300     PERFORM IMS-STATUSKONTROLL                                           
430400     .                                                                    
430500     SKIP1                                                                
430600 IMS-GU-KOLLIREG SECTION.                                                 
430700     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
430800            DELIMITED BY SIZE INTO SSA1                                   
430900     MOVE '  GE' TO GODK-STATUSKODER                                      
431000     CALL CBLTDLI USING GU     WDE62-PCB DLI-IO-E601 SSA1                 
431100     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
431200     PERFORM IMS-STATUSKONTROLL                                           
431300     .                                                                    
431400     SKIP1                                                                
431500 IMS-REPL-KOLLIREG SECTION.                                               
431600     MOVE '    ' TO GODK-STATUSKODER                                      
431700     CALL CBLTDLI USING REPL WDE62-PCB DLI-IO-E601                        
431800     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
431900     PERFORM IMS-STATUSKONTROLL                                           
432000     .                                                                    
432100     SKIP1                                                                
432200 IMS-GHU-KOLLI    SECTION.                                                
432300     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
432400            DELIMITED BY SIZE INTO SSA1                                   
432500     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
432600            DELIMITED BY SIZE INTO SSA2                                   
432700     MOVE '  GE' TO GODK-STATUSKODER                                      
432800     CALL CBLTDLI USING GHU    WDE62-PCB DLI-IO-E611 SSA1 SSA2            
432900     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
433000     PERFORM IMS-STATUSKONTROLL                                           
433100     .                                                                    
433200     SKIP1                                                                
433300 IMS-REPL-KOLLI    SECTION.                                               
433400     MOVE '    ' TO GODK-STATUSKODER                                      
433500     CALL CBLTDLI USING REPL WDE62-PCB DLI-IO-E611                        
433600     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
433700     PERFORM IMS-STATUSKONTROLL                                           
433800     .                                                                    
433900     SKIP1                                                                
434000 IMS-ISRT-KOLLI   SECTION.                                                
434100     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
434200            DELIMITED BY SIZE INTO SSA1                                   
434300     MOVE   'WDE611 '         TO   SSA2                                   
434400     MOVE '  II' TO GODK-STATUSKODER                                      
434500     CALL CBLTDLI USING ISRT WDE62-PCB DLI-IO-E611 SSA1 SSA2              
434600     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
434700     PERFORM IMS-STATUSKONTROLL                                           
434800     .                                                                    
434900     EJECT                                                                
435000 IMS-GU-KUNDORDER-SEK SECTION.                                            
435100     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
435200            DELIMITED BY SIZE INTO SSA1                                   
435300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
435400     CALL CBLTDLI USING GU    WDE4A-PCB DLI-IO-E401 SSA1                  
435500     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
435600                              STATUS-KUNDORDER-SEK-WS                     
435700     PERFORM IMS-STATUSKONTROLL                                           
435800     .                                                                    
435900     SKIP1                                                                
436000 IMS-GN-SEQA-WDE4A1 SECTION.                                              
436100     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
436200            DELIMITED BY SIZE INTO SSA1                                   
436300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
436400     CALL CBLTDLI USING GN   WDE4A-PCB DLI-IO-E401 SSA1                   
436500     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
436600                               STATUS-KUNDORDER-SEK-WS                    
436700     PERFORM IMS-STATUSKONTROLL                                           
436800     SKIP2                                                                
436900     .                                                                    
437000 IMS-ISRT-ZZAC01 SECTION.                                                 
437100     MOVE 'WLZZAC01' TO SSA1                                              
437200     MOVE '  II'     TO GODK-STATUSKODER                                  
437300     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA5 SSA1                   
437400     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
437500     PERFORM IMS-STATUSKONTROLL                                           
437600     .                                                                    
437700     EJECT                                                                
437800 IMS-GU-ORQA01     SECTION.                                               
437900     STRING 'WLORQA01(WDQ301KY =' W-WDQ301-ORDERDEL-X ')'                 
438000            DELIMITED BY SIZE INTO SSA1                                   
438100     MOVE '    ' TO GODK-STATUSKODER                                      
438200     CALL CBLTDLI USING GU     ORQA-PCB DLI-IO-AREA3 SSA1                 
438300     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
438400     PERFORM IMS-STATUSKONTROLL                                           
438500     .                                                                    
438600     EJECT                                                                
438700 IMS-GHU-XXKW11       SECTION.                                            
438800     STRING 'WLXXKW01(WDGXKEY  =' W-4471-WDGXKEY-X ')'                    
438900            DELIMITED BY SIZE INTO SSA1                                   
439000     STRING 'WLXXKW11(KDSEGKEY =' W-4472-KDSEGKEY-X ')'                   
439100            DELIMITED BY SIZE INTO SSA2                                   
439200     MOVE '  GE' TO GODK-STATUSKODER                                      
439300     CALL CBLTDLI USING GHU XXKW-PCB DLI-IO-AREA4 SSA1 SSA2               
439400     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
439500     PERFORM IMS-STATUSKONTROLL                                           
439600     .                                                                    
439700     SKIP2                                                                
439800 IMS-REPL-XXKW11      SECTION.                                            
439900     MOVE '  ' TO GODK-STATUSKODER                                        
440000     CALL CBLTDLI USING REPL XXKW-PCB DLI-IO-AREA4                        
440100     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
440200     PERFORM IMS-STATUSKONTROLL                                           
440300     .                                                                    
440400     EJECT                                                                
440500 IMS-GU-XXLB          SECTION.                                            
440600     STRING 'WLXXLB01(WDGXKEY  =' W-4477-WDGXKEY-X ')'                    
440700            DELIMITED BY SIZE INTO SSA1                                   
440800     STRING 'WLXXLB11(WDGXKEY  =' W-4478-WDGXKEY-X ')'                    
440900            DELIMITED BY SIZE INTO SSA2                                   
441000     MOVE '  GE' TO GODK-STATUSKODER                                      
441100     CALL CBLTDLI USING GU XXLB-PCB DLI-IO-AREA4 SSA1 SSA2                
441200     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
441300     PERFORM IMS-STATUSKONTROLL                                           
441400     .                                                                    
441500     EJECT                                                                
441600 IMS-GHU-XXDU01      SECTION.                                             
441700                                                                          
441800     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
441900            DELIMITED BY SIZE INTO SSA1                                   
442000     MOVE '  GE' TO GODK-STATUSKODER                                      
442100     CALL CBLTDLI USING GHU XXDU-PCB DLI-IO-AREA7 SSA1                    
442200     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
442300     PERFORM IMS-STATUSKONTROLL                                           
442400     .                                                                    
442500                                                                          
442600 IMS-GHNP-XXDU11      SECTION.                                            
442700                                                                          
442800     STRING 'WLXXDU11(IDKOLLI  =' W-4302-IDKOLLI-X ')'                    
442900            DELIMITED BY SIZE INTO SSA1                                   
443000     MOVE '  GE' TO GODK-STATUSKODER                                      
443100     CALL CBLTDLI USING GHNP XXDU-PCB DLI-IO-AREA7 SSA1                   
443200     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
443300     PERFORM IMS-STATUSKONTROLL                                           
443400     .                                                                    
443500                                                                          
443600 IMS-GNP-XXDU11-FIRST SECTION.                                            
443700                                                                          
443800     MOVE 'WLXXDU11*F '  TO SSA1                                          
443900     MOVE '  GE' TO GODK-STATUSKODER                                      
444000     CALL CBLTDLI USING GNP XXDU-PCB DLI-IO-AREA7 SSA1                    
444100     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
444200     PERFORM IMS-STATUSKONTROLL                                           
444300     .                                                                    
444400                                                                          
444500 IMS-ISRT-XXDU01     SECTION.                                             
444600                                                                          
444700     MOVE 'WLXXDU01 '  TO SSA1                                            
444800     MOVE '    ' TO GODK-STATUSKODER                                      
444900     CALL CBLTDLI USING ISRT XXDU-PCB DLI-IO-AREA7 SSA1                   
445000     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
445100     PERFORM IMS-STATUSKONTROLL                                           
445200     .                                                                    
445300                                                                          
445400 IMS-GNP-XXDU11      SECTION.                                             
445500                                                                          
445600     STRING 'WLXXDU11(WDGXKEY  =' W-4302-WDGXKEY-X ')'                    
445700            DELIMITED BY SIZE INTO SSA1                                   
445800     MOVE '  GE' TO GODK-STATUSKODER                                      
445900     CALL CBLTDLI USING GNP   XXDU-PCB DLI-IO-AREA7 SSA1                  
446000     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
446100     PERFORM IMS-STATUSKONTROLL                                           
446200     .                                                                    
446300                                                                          
446400 IMS-ISRT-XXDU11     SECTION.                                             
446500                                                                          
446600     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
446700            DELIMITED BY SIZE INTO SSA1                                   
446800     MOVE 'WLXXDU11 '  TO SSA2                                            
446900     MOVE '    ' TO GODK-STATUSKODER                                      
447000     CALL CBLTDLI USING ISRT XXDU-PCB DLI-IO-AREA7 SSA1 SSA2              
447100     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
447200     PERFORM IMS-STATUSKONTROLL                                           
447300     .                                                                    
447400                                                                          
447500 IMS-DLET-XXDU       SECTION.                                             
447600                                                                          
447700     MOVE '  ' TO GODK-STATUSKODER                                        
447800     CALL CBLTDLI USING DLET XXDU-PCB DLI-IO-AREA7                        
447900     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
448000     PERFORM IMS-STATUSKONTROLL                                           
448100     .                                                                    
448200     EJECT                                                                
448300 IMS-ISRT-4322-SEGM SECTION.                                              
448400     STRING 'WLXXJK01(WDGXKEY  =' W-4321-IDHTYP-X ')'                     
448500            DELIMITED BY SIZE INTO SSA1                                   
448600     MOVE 'WLXXJK11*L' TO SSA2                                            
448700     MOVE '  ' TO GODK-STATUSKODER                                        
448800     CALL CBLTDLI USING ISRT XXJK-PCB DLI-IO-AREA7 SSA1 SSA2              
448900     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
449000     PERFORM IMS-STATUSKONTROLL                                           
449100     .                                                                    
449200     SKIP2                                                                
449300                                                                          
449400 IMS-GET-ORQI01-CSEQ SECTION.                                             
449500                                                                          
449600     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
449700             DELIMITED BY SIZE INTO    SSA1                               
449800     MOVE    '  GE'              TO    GODK-STATUSKODER                   
449900     CALL    CBLTDLI             USING GU   ORQL-PCB                      
450000                                            DLI-IO-AREA6 SSA1             
450100     MOVE    ORQL-STATUS-CODE    TO    STATUS-WS                          
450200     PERFORM IMS-STATUSKONTROLL                                           
450300     .                                                                    
450400     SKIP2                                                                
450500 IMS-GU-ORQI12      SECTION.                                              
450600                                                                          
450700     STRING  'WLORQI01(IDORDER  =' W-WDQ201-X ')'                         
450800             DELIMITED BY SIZE INTO    SSA1                               
450900     STRING  'WLORQI12(IDDC     =' W-IDDC-X ')'                           
451000             DELIMITED BY SIZE INTO    SSA2                               
451100     MOVE    '    '              TO    GODK-STATUSKODER                   
451200     CALL    CBLTDLI USING GU  ORQI-PCB DLI-IO-AREA6 SSA1 SSA2            
451300     MOVE    ORQI-STATUS-CODE    TO    STATUS-WS                          
451400     PERFORM IMS-STATUSKONTROLL                                           
451500     .                                                                    
451600     SKIP2                                                                
451700 IMS-GU-ORQI01      SECTION.                                              
451800                                                                          
451900     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
452000             DELIMITED BY SIZE INTO    SSA1                               
452100     MOVE    '    '              TO    GODK-STATUSKODER                   
452200     CALL    CBLTDLI USING GU  ORQI-PCB DLI-IO-AREA6 SSA1                 
452300     MOVE    ORQI-STATUS-CODE    TO    STATUS-WS                          
452400     PERFORM IMS-STATUSKONTROLL                                           
452500     .                                                                    
452600     SKIP2                                                                
452700 IMS-GHNP-ORQI12    SECTION.                                              
452800                                                                          
452900     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
453000            DELIMITED BY SIZE INTO SSA1                                   
453100     MOVE '    ' TO GODK-STATUSKODER                                      
453200     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-AREA6 SSA1                   
453300     MOVE ORQI-STATUS-CODE       TO STATUS-WS                             
453400     PERFORM IMS-STATUSKONTROLL                                           
453500     .                                                                    
453600     SKIP2                                                                
453700 IMS-REPL-ORQI12      SECTION.                                            
453800                                                                          
453900     MOVE '    ' TO GODK-STATUSKODER                                      
454000     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA6                        
454100     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
454200     PERFORM IMS-STATUSKONTROLL                                           
454300     .                                                                    
454400 IMS-GU-WDB601    SECTION.                                                
454500                                                                          
454600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
454700     DELIMITED BY SIZE INTO SSA1                                          
454800     MOVE '  GE' TO GODK-STATUSKODER                                      
454900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
455000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
455100     PERFORM IMS-STATUSKONTROLL                                           
455200     IF SEGMENT-SAKNAS                                                    
455300        MOVE SPACE TO DCS-KDDC                                            
455400     END-IF                                                               
455500     .                                                                    
455600     EJECT                                                                
455700 IMS-GHN-WDA6B SECTION.                                                   
455800     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
455900                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
456000            DELIMITED BY SIZE INTO SSA1                                   
456100     MOVE '  GEGB'               TO GODK-STATUSKODER                      
456200     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA-A601 SSA1           
456300     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
456400     PERFORM IMS-STATUSKONTROLL                                           
456500     .                                                                    
456600     SKIP2                                                                
456700 IMS-REPL-WDA6B SECTION.                                                  
456800     MOVE 'WDA601  '           TO SSA1                                    
456900     MOVE '    '               TO GODK-STATUSKODER                        
457000     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA-A601 SSA1            
457100     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
457200     PERFORM IMS-STATUSKONTROLL                                           
457300     .                                                                    
457400     SKIP2                                                                
457500 IMS-GU-WDK601  SECTION.                                                  
457600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
457700          DELIMITED BY SIZE INTO SSA1                                     
457800     MOVE '  GE' TO GODK-STATUSKODER                                      
457900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
458000     MOVE WDK6-STATUS-CODE TO STATUS-WS-WDK6                              
458100     PERFORM IMS-STATUSKONTROLL                                           
458200     .                                                                    
458300     SKIP3                                                                
458400 IMS-GNP-WDK611  SECTION.                                                 
458500     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
458600          DELIMITED BY SIZE INTO SSA2                                     
458700     MOVE '  GE' TO GODK-STATUSKODER                                      
458800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
458900     MOVE WDK6-STATUS-CODE TO STATUS-WS-WDK6                              
459000     PERFORM IMS-STATUSKONTROLL                                           
459100     .                                                                    
459200     SKIP3                                                                
459300 IMS-GU-WDK611  SECTION.                                                  
459400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
459500          DELIMITED BY SIZE INTO SSA1                                     
459600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
459700          DELIMITED BY SIZE INTO SSA2                                     
459800     MOVE '  GE' TO GODK-STATUSKODER                                      
459900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
460000     MOVE WDK6-STATUS-CODE TO STATUS-WS-WDK6                              
460100     PERFORM IMS-STATUSKONTROLL                                           
460200     .                                                                    
460300     SKIP3                                                                
460400 IMS-GU-WDK711 SECTION.                                                   
460500                                                                          
460600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
460700          DELIMITED BY SIZE INTO SSA1                                     
460800     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
460900          DELIMITED BY SIZE INTO SSA2                                     
461000     MOVE '  GE' TO GODK-STATUSKODER                                      
461100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
461200     MOVE WDK7-STATUS-CODE TO STATUS-WS-WDK7                              
461300     PERFORM IMS-STATUSKONTROLL                                           
461400     .                                                                    
461500     SKIP2                                                                
461600 IMS-ROLLBACK    SECTION.                                                 
461700     SKIP2                                                                
461800     CALL CBLTDLI USING ROLB    MSG-PCB                                   
461900     .                                                                    
462000     SKIP2                                                                
462100 IMS-STATUSKONTROLL SECTION.                                              
462200     SET STATUS-IX TO 1                                                   
462300     SEARCH GODK-STATUS AT END CALL FELLOG                                
462400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
462500     END-SEARCH                                                           
462600     .                                                                    
