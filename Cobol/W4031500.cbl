000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0122      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700     SKIP2                                                                
000800 PROGRAM-ID.     W4031500.                                                
000900 AUTHOR.         CAP GEMINI AB/EP.                                        
001000     DATE-WRITTEN.   DEC   85.                                            
001100     REMARKS.                                                             
001200*    FUNKTION.                                                            
001300*        KOLLIVIS PACKNING - RAPPORTERING AV VAD SOM LIGGER               
001400*        I VISST KOLLI.                                                   
001500*        DESSUTOM RAPPORTERAS UPPGIFTER OM KOLLIT.                        
001600*                                                                         
001700*        VAL 'U' VID UTSKRIFT AV KOLLIFLAGGA ELLER FÖLJESEDEL             
001800*        SKALL INTE GE NÅGON UTSKRIFT, MEN ÄR ETT GODKÄNT VAL.            
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W4T315                                              
002200*        MID:         W4I31501                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W4O31501                                            
002600*    SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP3                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200*    -- CHECKED BY WY2000                                                 
003300     SKIP3                                                                
003400 77   PROGRAM-NAMN               PIC X(8)  VALUE 'W4031500'.              
003500 77   KDRC-DISPLAY               PIC Z(5).                                
003600 77    FELTEXT                   PIC X(80) VALUE SPACE.                   
003700 77    JA                        PIC X       VALUE 'J'.                   
003800 77    YES                       PIC X       VALUE 'Y'.                   
003900 77    NEJ                       PIC X       VALUE 'N'.                   
004000 77    RAETT                     PIC X       VALUE 'R'.                   
004100 77    FEL                       PIC X       VALUE 'F'.                   
004200 77    SAKNAS                    PIC X       VALUE 'S'.                   
004300 77    SOEK-VIA-PRODNR           PIC X       VALUE 'N'.                   
004400 77    FILLER                    PIC X(8)    VALUE 'AAAAAAAA'.            
004500 77    IND1                      PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77    IND2                      PIC S9(9)   VALUE +0   COMP SYNC.        
004700 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77    INX                       PIC  9(9)   VALUE  0.                    
004900 77    LINE-IX                   PIC 9(3)    VALUE ZERO.                  
005000 77    MAX-TAB                   PIC 9(2)    VALUE ZERO.                  
005100 77    MAX-RAD                   PIC 9(3)    VALUE ZERO.                  
005200 77    MAX-LINES                 PIC S9(9)   VALUE +50  COMP-3.           
005300 77    ACK-KOLLI                 PIC S9(9)   VALUE +0.                    
005400 77    RAD-INX                   PIC S9(9)   VALUE +0   COMP SYNC.        
005500 77    REST-INX                  PIC S9(9)   VALUE +0   COMP SYNC.        
005600 77    BILD-RAD                  PIC S9(9)   VALUE +0   COMP SYNC.        
005700 77    FG-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.        
005800 77    FG-MAX-INDX               PIC S9(9)   VALUE +10  COMP SYNC.        
005900 77    FILLER                    PIC X(8)    VALUE 'BBBBBBBB'.            
006000 77    TMS-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
006100 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +400 COMP SYNC.        
006200 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +82  COMP SYNC.        
006300 77    0605-LAENGD               PIC S9(4)   VALUE +58  COMP SYNC.        
006400 77    WS-IDCOM                  PIC S9(9)   VALUE ZERO COMP-3.           
006500 77    FILLER                    PIC X(8)    VALUE 'CCCCCCCC'.            
006600 77    WS-TOT-ANT-RADER          PIC S9(3)   VALUE +0    COMP-3.          
006700 77    WS-ANT-RADER-INT          PIC S9(3)   VALUE +0    COMP-3.          
006800 77    WS-ANT-RAD-I-BILD         PIC S9(3)   VALUE +0    COMP-3.          
006900 77    WS-MAX-ANT-RAD-I-BILD     PIC S9(3)   VALUE +201  COMP-3.          
007000 77    WS-RAD-FOM                PIC S9(4)   VALUE +0.                    
007100 77    WS-RAD-TOM                PIC S9(4)   VALUE +0.                    
007200 77    WS-KVLOCK                 PIC S9(3)   VALUE +0    COMP-3.          
007300 77    WS-KVRAM                  PIC S9(3)   VALUE +0    COMP-3.          
007400 77    WS-KVPALL                 PIC S9(3)   VALUE +0    COMP-3.          
007500 77    WS-KDORDKL                PIC S9(1)   VALUE +0    COMP-3.          
007600 77    WS-KORD-IDORDER           PIC S9(7)   VALUE +0    COMP-3.          
007700 77    FILLER                    PIC X(8)    VALUE 'DDDDDDDD'.            
007800 77    WS-KDFRAKT                PIC 9(2)   VALUE ZERO.                   
007900 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
008000 77    WS-EMBPROF                PIC X(1)   VALUE SPACE.                  
008100 77    WS-FLAUTFAK               PIC X(1)   VALUE SPACE.                  
008200 77    WS-KDFAKTYP               PIC X(1)   VALUE SPACE.                  
008300 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
008400 77    WS-TIDPUNKT               PIC 9(8)   VALUE ZERO.                   
008500 77    WS-TISKPTID               PIC 9(6)   VALUE ZERO.                   
008600 77    WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
008700 77    WS-IDDISTR                PIC X(4)   VALUE SPACE.                  
008800 77    WS-IDDISTR-NUM            PIC 9(4)   VALUE ZERO.                   
008900 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
009000 77    WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                   
009100 77    WS-IDORDER                PIC 9(7)   VALUE ZERO.                   
009200 77    WS-IDPLKLST               PIC S9(3)  VALUE ZERO COMP-3.            
009300 77    WS-IDKOLLI                PIC X(5)   VALUE SPACE.                  
009400 77    FILLER                    PIC X(8)    VALUE 'EEEEEEEE'.            
009500 77    WS-IDKOLLI-LR             PIC 9(5)   VALUE ZERO.                   
009600 77    WS-IDKOLLI-NUM            PIC 9(5)   VALUE ZERO.                   
009700 77    WS-IDKOLLI-FOM            PIC 9(5)   VALUE ZERO.                   
009800 77    WS-IDKOLLI-TOM            PIC 9(5)   VALUE ZERO.                   
009900 77    WS-IDPRODNR               PIC X(7)   VALUE SPACE.                  
010000 77    WS-JFR-IDPRODNR           PIC X(7)   VALUE SPACE.                  
010100 77    WS-START-RAD              PIC 9(4)   VALUE ZERO.                   
010200 77    WS-SISTA-RAD              PIC 9(4)   VALUE ZERO.                   
010300 77    WS-AKTUELL-RAD            PIC 9(4)   VALUE ZERO.                   
010400 77    WS-KVLEVART               PIC 9(6)   VALUE ZERO.                   
010500 77    WS-IDKOLLI-SAMP           PIC 9(5)   VALUE ZERO.                   
010600 77    WS-KDKOLLI                PIC X(8)   VALUE SPACE.                  
010700 77    FILLER                    PIC X(8)    VALUE 'FFFFFFFF'.            
010800 77    WS-EMB-VKTARA-ONE-CASE    PIC S9(6)V9 VALUE ZERO  COMP-3.          
010900 77    WS-EMB-VKTARA-TOT-ORDER   PIC S9(6)V9 VALUE ZERO  COMP-3.          
011000 77    ONE-CASE-VKORDBTO         PIC S9(6)V9 VALUE ZERO  COMP-3.          
011100 77    WS-MOD-VKORDBTO           PIC 9(6)V999 VALUE ZERO.                 
011200 77    ACC-ORAD-VKORDNTO         PIC 9(6)V9(3) VALUE ZERO.                
011300 77    WS-ACC-ORAD-VKORDNTO      PIC 9(6)V9(3) VALUE ZERO.                
011400 77    WS-VLORDBTO               PIC 9(4)V9(3)  VALUE ZERO.               
011500 77    WS-KDEMBTYP               PIC 9(2)   VALUE ZERO.                   
011600 77    WS-DIKOLLIL               PIC 9(4)   VALUE ZERO.                   
011700 77    WS-MOD-DIKOLLIL           PIC 9(4)   VALUE ZERO.                   
011800 77    WS-DIKOLLIB               PIC 9(3)   VALUE ZERO.                   
011900 77    WS-MOD-DIKOLLIB           PIC 9(3)   VALUE ZERO.                   
012000 77    WS-DIKOLLIH               PIC 9(3)   VALUE ZERO.                   
012100 77    WS-MOD-DIKOLLIH           PIC 9(3)   VALUE ZERO.                   
012200 77    WS-KDKOLLID               PIC X(1)   VALUE 'L'.                    
012600 77    WS-REST                   PIC 9(4)   VALUE ZERO.                   
012700 77    FILLER                    PIC X(8)    VALUE 'GGGGGGGG'.            
012800 77    WS-ODEL-IDTRP             PIC X(5)   VALUE SPACE.                  
012900 77    WS-TRAEFF-PACKARE         PIC X(01).                               
013000 77    WS-TRAEFF-RAD             PIC X(01).                               
013100 77    WS-RADER-OK               PIC X(01)  VALUE 'N'.                    
013200 77    MAX-RAD-ANTAL-PLUS-1      PIC S9(3)  VALUE +13  COMP-3.            
013300 77    WS-KVPTID-MIN             PIC S9(7)  VALUE ZERO COMP-3.            
013400 77    WS-KVPTID-TIM             PIC S9(3)  VALUE ZERO COMP-3.            
013500 77    WS-PRT-KDSVAR-ADRESSFL    PIC X(1)   VALUE SPACE.                  
013600 77    WS-PRT-KDSVAR-FOLJEFL     PIC X(1)   VALUE SPACE.                  
013700 77    WS-TIORDREG-NUM6          PIC 9(6)   VALUE ZERO.                   
013800 77    WS-PACKARES-ODEL-REDAN-KLARA PIC X.                                
013900 77    WS-DARFS                  PIC 9(12) VALUE ZERO.                    
014000*                                        ANTAL FÄRDIGPACKADE RADER        
014100*                                        I ETT RAD-INTERVALL.             
014200 77    WS-RINT-ANT-FPACK-ORAD    PIC S9(5)  VALUE ZERO COMP-3.            
014300 77    FILLER                    PIC  X(08) VALUE 'HHHHHHHH'.             
014400 77    WS-SPAR-IDORDER           PIC S9(07) VALUE ZERO COMP-3.            
014500 77    WS-SPAR-IDARTNR           PIC S9(09) VALUE ZERO COMP-3.            
014600 77    WS-SPAR-IDDC              PIC  X(02) VALUE ZERO.                   
014700 77    WS-SPAR-BEART             PIC  X(25) VALUE SPACE.                  
014800 77    WS-SPAR-KVBEART           PIC S9(07) VALUE ZERO COMP-3.            
014900 77    WS-SPAR-FLTILLK           PIC  X(01) VALUE SPACE.                  
015000 77    WS-SPAR-IDKUNDRF-RO       PIC  X(10) VALUE SPACE.                  
015100 77    WS-SPAR-IDPURAD           PIC S9(05) VALUE ZERO COMP-3.            
015200*                                        ANTAL FÄRDIGPACKADE RADER        
015300*                                        I ETT RAD-INTERVALL.             
015400 77    FILLER                    PIC X(8)    VALUE 'IIIIIIII'.            
015500 77    WS-KDPRTVAL-FS            PIC X(2)   VALUE SPACE.                  
015600 77    WS-KDPRTVAL-ADR           PIC X(2)   VALUE SPACE.                  
015700 77    WS-IDLEVNR                PIC X(5)   VALUE SPACES.                 
015800 77    WS-ODEL-IDDC-EXP          PIC X(2)   VALUE SPACES.                 
015900                                                                          
015910 01   WS-IDTRPTNR               PIC S9(3)   VALUE ZERO.                   
015920 01   WS-ADFLGEO                PIC X(3)    VALUE SPACE.                  
015930 01   WS-ADFLOMR                PIC S9(3)   VALUE ZERO.                   
015940 01   WS-ADRUTNIV               PIC S9(3)   VALUE ZERO.                   
015950 01   WS-DIHMODUL               PIC S9(3)   VALUE ZERO.                   
015960 01   WS-DIDMODUL               PIC S9(3)   VALUE ZERO.                   
015970 01   WS-ADVMODUL               PIC S9(3)   VALUE ZERO.                   
015980 01   WS-ADHMODUL               PIC S9(3)   VALUE ZERO.                   
015990 01   WS-FLUTLAST               PIC X       VALUE SPACE.                  
015991 01   WS-IDDC-CROSS             PIC X(2)    VALUE SPACE.                  
015992                                                                          
016000 77    INF-WEIGHT-NOT-LESS-THAN  PIC X(50)                                
016100       VALUE 'WEIGHT CANNOT BE LESS THAN                   '.             
016200                                                                          
016300 77    ENTER-GROSS-WEIGHT        PIC X(50)                                
016400       VALUE 'ENTER GROSS WEIGHT                           '.             
016500                                                                          
016600 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
016700 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
016800 77  IX-MAX                      PIC S9(9)  VALUE +7    COMP SYNC.        
016900                                                                          
017000 01    FILLER                    PIC X(16)   VALUE 'WS-AREA'.             
017100 01    WS-AREA.                                                           
017200   03 WS-VKORDBTO-TOT            PIC 9(6).999  VALUE ZERO.                
017300                                                                          
017400 01  W-KVLL                      PIC 9(4)   VALUE ZERO.                   
017500*                                                                         
017600 01  TRANSFER-KUND               PIC 9(7).                                
017700     88 TRANSFER-KUNDNR          VALUE 0000511                            
017800                                       0000512                            
017900                                       0000513.                           
018000     88  RETUR-KUNDNR            VALUE 0000051.                           
018100*                                                                         
018200 01  WS-KDMATT                   PIC X.                                   
018300     88 US-MEASUREMENT           VALUE 'U'.                               
018400     88 SIS-MEASUREMENT          VALUE 'S'.                               
018500                                                                          
018600*                                                                         
018700 01  WS-DATUM-TID.                                                        
018800   03 WS-DATUM                   PIC X(8)    VALUE SPACE.                 
018900   03 WS-KLOCKAN                 PIC 9(10)   VALUE ZERO.                  
019000*                                                                         
019100*      --- VALID IDDD CODES                                               
019200*                                                                         
019300*01    -COPY WWDC99                                                       
019400       EJECT                                                              
019500                                                                          
019600 01  WS-IDPRTLST.                                                         
019700     03 WS-SYSTDEL               PIC X(1).                                
019800     03 WS-LISTTYP               PIC X(2).                                
019900     03 WS-DC                    PIC X(2).                                
020000     03 WS-KDPRT                 PIC X(3).                                
020100 01  WS-IDPRTJAP REDEFINES WS-IDPRTLST.                                   
020200     03 WS-LASER-BLANKETT        PIC X(6).                                
020300     03 WS-NDC-JAP-KDPRT         PIC X(2).                                
020400                                                                          
020500 77    ABEND-MED1                PIC X(80)  VALUE                         
020600      'ABEND-ORSAK:FEL I TRANS FRÅN ORDVIS,STARTA OM 4315 & 0605'.        
020700     SKIP2                                                                
020800                                                                          
020900 77    WS-IDTRANS                PIC X(04).                               
021000   88  WS-SAMMA-BILD                        VALUE '4315'.                 
021100   88  WS-GODKAND-BILD                      VALUE '4311' '4312'           
021200                                                  '4313' '4314'           
021300                                                  '4315' '4316'           
021400                                                  '4317' '4318'.          
021500   88  WS-ORDERVIS-TRANS                    VALUE '0605'.                 
021600     SKIP2                                                                
021700 77    WS-INDATA-TEST            PIC X(01).                               
021800   88  WS-INDATA-FEL                        VALUE 'F'.                    
021900   88  WS-INDATA-RATT                       VALUE 'R'.                    
022000     SKIP2                                                                
022100 77    WS-BEHANDLING-TEST        PIC X(01).                               
022200   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
022300   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
022400     SKIP2                                                                
022500 77    FL-RAD-INTERVALL          PIC X(01).                               
022600   88  INTERVALL-RAD                        VALUE 'J'.                    
022700   88  AVVIKELSE-RAD                        VALUE 'N'.                    
022800     SKIP2                                                                
022900 77    FL-KOLLI-INTERVALL        PIC X(01).                               
023000   88  KOLLI-INTERVALL                      VALUE 'J'.                    
023100   88  EJ-KOLLI-INTERVALL                   VALUE 'N'.                    
023200     SKIP2                                                                
023300 77    FL-RAD-INOM-INTERVALL     PIC X(01).                               
023400   88  RAD-FINNS-I-INTERVALL                VALUE 'J'.                    
023500     SKIP2                                                                
023600 77    FL-SLINGA-KLAR            PIC X(01).                               
023700   88  SLINGA-KLAR                          VALUE 'J'.                    
023800*                                                                         
023900 77    SW-KOLLI-HITTAT           PIC X(01).                               
024000   88  KOLLI-HITTAT                         VALUE 'J'.                    
024100                                                                          
024200 77    DIRLEV-KOLLI-SW           PIC X(01).                               
024300   88  DIRLEV-KOLLI                         VALUE 'J'.                    
024400*                                                                         
024500 77    NYA-NYCKLAR-SW            PIC X(01).                               
024600   88  NYA-NYCKLAR                          VALUE 'J'.                    
024700 77    WS-WEIGHT                 PIC X(01).                               
024800   88  WEIGHT-MISMATCH                      VALUE 'J'.                    
024900                                                                          
025000 77    SW-TIKLAR-UPPDATERAD      PIC X(01).                               
025100*                                                                         
025200 77    FILLER                    PIC X(8)    VALUE 'JJJJJJJJ'.            
025300 01     WS-TEMFSINF              PIC X(61)  VALUE SPACE.                  
025400     SKIP2                                                                
025500 01     WS-IDKUNDRF.                                                      
025600   03   WS-IDORDNR              PIC X(5).                                 
025700   03   FILLER                  PIC X(5)    VALUE SPACE.                  
025800 01     WS-JFR-IDANSTNR.                                                  
025900   03   FILLER                    PIC X(3).                               
026000   03   WS-JFR-IDANSTNR-5         PIC X(5).                               
026100     SKIP3                                                                
026200 01     WS-SUPTID-PRAPP         PIC 9(3)V99.                              
026300 01     FILLER REDEFINES WS-SUPTID-PRAPP.                                 
026400   03   WS-SUPTID-TIM           PIC 9(3).                                 
026500   03   WS-SUPTID-MIN           PIC 9(2).                                 
026600     SKIP3                                                                
026700                                                                          
026800 01     DYNAMISKA-SUBPROGRAM.                                             
026900   03   CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.             
027000   03   FELLOG                  PIC X(8)    VALUE 'FELLOG  '.             
027100   03   WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.             
027200   03   W005INIT                PIC X(8)    VALUE 'W005INIT'.             
027300   03   W403PLAT                PIC X(8)    VALUE 'W403PLAT'.             
027400   03   WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.             
027500   03   WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.             
027600   03   ABEND                   PIC X(8)    VALUE 'ABEND   '.             
027700   03   W403TMS1                PIC X(8)    VALUE 'W403TMS1'.             
027800     EJECT                                                                
027900*    --- PARAMETRAR TILL ABEND                                            
028000                                                                          
028100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
028200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
028300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
028400 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
028500     SKIP3                                                                
028600*    --- PARAMETERS TO WZ01SEND                                           
028700 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
028800     SKIP3                                                                
028900*01  -COPY WZ01SEND                                                       
029000     EJECT                                                                
029100 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
029200 01  HDR-AREA.                                                            
029300*    03  -COPY WZ01REQU  -PRE HDR-                                        
029400*    03  -COPY WZ04HDR                                                    
029500*                                                                         
029600 01  FILLER                      PIC X(9)    VALUE 'SEND-AREA'.           
029700 01  SEND-AREA                   PIC X(100)  VALUE SPACE.                 
029800*                                                                         
029900*TMS PACKNING INFO Ecom                                                   
030000 01  FILLER                      PIC X(8)   VALUE  'W403TMS1'.            
030100*    -COPY W403TMS1                                                       
030200*                                                                         
030300 01  GEMENSAMMA-SUBPROGRAM.                                               
030400     03  W006PRT                PIC X(8)    VALUE 'W006PRT '.             
030500*        PRINTERKONTROLL                                                  
030600*                                                                         
030700     03  W411DNOT               PIC X(8)    VALUE 'W411DNOT'.             
030800*                                                                         
030900                                                                          
031000*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
031100*                                                                         
031200 01  FILLER                     PIC X(16)   VALUE 'LÄNKAREOR'.            
031300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
031400*01 -COPY WMSGINIT                                                        
031500 01  FILLER                     PIC X(16)   VALUE 'W006PRT  '.            
031600*   -COPY W006PRT                                                         
031700     EJECT                                                                
031800 01  FILLER                     PIC X(10)   VALUE 'WDECAREA'.             
031900*01 -COPY WDECAREA                                                        
032000     EJECT                                                                
032100 01  FILLER                     PIC X(16)   VALUE 'W403PLAT '.            
032200*   -COPY W403PLAT                                                        
032300     EJECT                                                                
032400 01  FILLER                     PIC X(16)   VALUE 'W411DNOT '.            
032500*   -COPY W411DNOT                                                        
032600     EJECT                                                                
032700 01  FILLER                     PIC X(16)   VALUE 'WWOMVAND '.            
032800*   -COPY WWOMVAND                                                        
032900     SKIP3                                                                
034400 01     FILLER                  PIC X(11)   VALUE 'HJALP-AREOR'.          
034500 01     HJALP-ODEL-DARFS        PIC 9(12).                                
034600 01     FILLER                  REDEFINES HJALP-ODEL-DARFS.               
034700   03   FILLER                  PIC  9(2).                                
034800   03   HJALP-ODEL-DARFS-6      PIC  9(6).                                
034900   03   FILLER                  PIC  9(4).                                
035000     SKIP2                                                                
035100 01     HJALP-4472-TIRFS        PIC 9(11).                                
035200 01     FILLER                  REDEFINES HJALP-4472-TIRFS.               
035300   03   FILLER                  PIC  9(1).                                
035400   03   HJALP-4472-TIRFS-6      PIC  9(6).                                
035500   03   FILLER                  PIC  9(4).                                
035600     EJECT                                                                
035700 01     FILLER                  PIC X(10)   VALUE 'SPAR-AREOR'.           
035800 01     SPAR-AREOR.                                                       
035900   03   SPAR1-INTERVALL-AREA.                                             
036000     05 SPAR1-IDRADNR-FOM       PIC  9(5).                                
036100     05 SPAR1-IDRADNR-TOM       PIC  9(5).                                
036200     05 SPAR1-KVORAPP           PIC  9(7).                                
036300     05 SPAR1-FLNOLLJ           PIC  X(1).                                
036400*                                                                         
036500   03   SPAR2-INTERVALL-AREA.                                             
036600     05 SPAR2-IDRADNR-FOM       PIC  9(5).                                
036700     05 SPAR2-IDRADNR-TOM       PIC  9(5).                                
036800     05 SPAR2-KVORAPP           PIC  9(7).                                
036900     05 SPAR2-FLNOLLJ           PIC  X(1).                                
037000*                                                                         
037100   03   SPAR-PRAD-UPPG-AREA.                                              
037200     05 SPAR-PRAD-VKARTNTO      PIC  9(6)V9(3)    VALUE ZERO.             
037300     05 SPAR-PRAD-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
037400     05 SPAR-PRAD-KDFARLIG      PIC  S9           VALUE ZERO.             
037500     05 SPAR-PRAD-KVLEVART      PIC  S9(7)        VALUE ZERO.             
037600     05 SPAR-PRAD-PRARTNTO      PIC  S9(9)V9(2)   VALUE ZERO.             
037700     05 SPAR-PRAD-PRAVCOST      PIC  S9(9)V9(2)   VALUE ZERO.             
037800     05 SPAR-PRAD-PRARTNTO-LOC  PIC  S9(9)V9(2)   VALUE ZERO.             
037900     05 SPAR-PRAD-PRARTNTO-LOCPREL  PIC  S9(9)V9(2)   VALUE ZERO.         
038000     05 SPAR-PRAD-KDVALISO      PIC X(3)          VALUE SPACE.            
038100     05 SPAR-PRAD-KDVALISO-EXP  PIC X(3)          VALUE SPACE.            
038200*                                                                         
038300   03   SPAR-FARLIGT-GODS-DATA.                                           
038400     05 SPAR-IDPSN              PIC  9(3)                VALUE 0.         
038500     05 SPAR-VKART-FG           PIC  S9(7)        COMP-3 VALUE 0.         
038600     05 SPAR-VLFG               PIC  S9(4)V9(3)   COMP-3 VALUE 0.         
038700     05 SPAR-SUEQFG             PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
038800*                                                                         
038900     05 TOTAL-SUEQFG            PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
039000     EJECT                                                                
039100 01     FILLER                  PIC X(11)   VALUE 'ARBETSAREOR'.          
039200 01     ARBETSAREOR.                                                      
039300   03   ARB-AREA-RAD.                                                     
039400     05 ARB-RAD-FOM             PIC  9(4).                                
039500     05 ARB-RAD-TOM             PIC  9(4).                                
039600     05 ARB-RAD-AKTUELL         PIC  9(4).                                
039700     05 ARB-KVLEVART            PIC  9(6).                                
039800     SKIP2                                                                
039900   03   ARB-KOLLI-UPPG-AREA.                                              
040000     05 ARB-KOLLI-VKORDNTO       PIC  9(6)V9(3)   VALUE ZERO.             
040100     05 ARB-KOLLI-KVFLAMP        PIC  S9(2)V9(1)  VALUE ZERO.             
040200     05 ARB-KOLLI-KDFARLIG       PIC  S9          VALUE ZERO.             
040300     05 ARB-KOLLI-KVORDRAD       PIC  S9(5)       VALUE ZERO.             
040400     05 ARB-KOLLI-KVFALRAD       PIC  S9(5)       VALUE ZERO.             
040500     05 ARB-KOLLI-SUORDV         PIC  S9(9)V9(2)  VALUE ZERO.             
040600     05 ARB-KOLLI-SUORDV-EXP     PIC  S9(9)V9(2)  VALUE ZERO.             
040700     05 ARB-KOLLI-SUORDV-LOC     PIC  S9(9)V9(2)  VALUE ZERO.             
040800     05 ARB-KOLLI-SUORDV-LOCPREL PIC  S9(9)V9(2)  VALUE ZERO.             
040900     05 FILLER                   PIC X(8)    VALUE 'TTTTTTTT'.            
041000     05 ARB-KOLLI-KDVALISO       PIC X(3)    VALUE SPACE.                 
041100     05 ARB-KOLLI-KDVALISO-EXP   PIC X(3)    VALUE SPACE.                 
041200     SKIP2                                                                
041300   03   ARB-ADRESS.                                                       
041400     05 ARB-ADFLGEO             PIC  X(3)   VALUE SPACE.                  
041500     05 FILLER                  PIC  X(1)   VALUE SPACE.                  
041600     05 ARB-ADFLOMR             PIC  9(3)   VALUE ZERO.                   
041700     05 FILLER                  PIC  X(1)   VALUE SPACE.                  
041800     05 ARB-ADRUTNIV            PIC  9(3)   VALUE ZERO.                   
041900     SKIP2                                                                
042000   03   ARB-ANTAL-KOLLI-PLUS-1   PIC 9(5)    VALUE ZERO.                  
042100   03   ARB-ANTAL-KOLLI          PIC 9(5)    VALUE ZERO.                  
042200     SKIP2                                                                
042300 01     WS-TIDPUNKT-RED.                                                  
042400   03   WS-HHMMSS               PIC  9(6).                                
042500   03   WS-DD                   PIC  9(2).                                
042600     EJECT                                                                
042700 01     TEST-IDDISTR            PIC 9(5)              COMP-3.             
042800 01     FILLER REDEFINES TEST-IDDISTR.                                    
042900*  03   -COPY WWDIST03.                                                   
043000     SKIP2                                                                
043100 01     FILLER REDEFINES TEST-IDDISTR.                                    
043200*  03   -COPY WWDIST07.                                                   
043300     SKIP2                                                                
043400 01     FILLER REDEFINES TEST-IDDISTR.                                    
043500*  03   -COPY WWDIST08.                                                   
043600     SKIP2                                                                
043700 01     FILLER REDEFINES TEST-IDDISTR.                                    
043800*  03   -COPY WWDIST18.                                                   
043900     SKIP2                                                                
044000 01     FILLER REDEFINES TEST-IDDISTR.                                    
044100*  03   -COPY WWDIST19.                                                   
044200     SKIP2                                                                
044300 01     FILLER REDEFINES TEST-IDDISTR.                                    
044400*  03   -COPY WWDIST21.                                                   
044500     SKIP2                                                                
044600 01     FILLER REDEFINES TEST-IDDISTR.                                    
044700*  03   -COPY WWDIST35.                                                   
044800     SKIP2                                                                
044900 01     FILLER REDEFINES TEST-IDDISTR.                                    
045000*  03   -COPY WWDIST79.                                                   
045100     SKIP2                                                                
045200 01     FILLER REDEFINES TEST-IDDISTR.                                    
045300*  03   -COPY WWDIST85.                                                   
045400*    ----DISTR-DEALER-PRICE----                                           
045500     SKIP2                                                                
045600*01    FILLER  -COPY WWDIS128      -RED TEST-IDDISTR.                     
045700     EJECT                                                                
045800 01  FILLER                    PIC X(08) VALUE 'FRAK-010'.                
045900*01    FILLER  -COPY WWFRAKT1                                             
046000     SKIP2                                                                
046100 01  FILLER                    PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.        
046200 01    NYCKLAR-TILL-DLI.                                                  
046300*                                                                         
046400   03    W-WDE4A1-KUNDORDER-X.                                            
046500     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
046600     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
046700     05    W-4A1-IDKUNDRF.                                                
046800       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
046900       07  FILLER                PIC X(05)   VALUE SPACE.                 
047000*                                                                         
047100   03    W-WDE401-KUNDORDER-X.                                            
047200     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
047300     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
047400     05    W-401-IDKUNDRF.                                                
047500       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
047600       07  FILLER                PIC X(05)   VALUE SPACE.                 
047700     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
047800     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
047900*                                                                         
048000   03    W-WDE4B-KEYSEQ-MIN-X.                                            
048100     05    W-420-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
048200     05    W-420-IDPURAD-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
048300*                                                                         
048400   03    W-WDE4B-KEYSEQ-MAX-X.                                            
048500     05    W-420-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
048600     05    W-420-IDPURAD-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
048700*                                                                         
048800   03    W-WDE4B-KEYSEQ-X.                                                
048900     05    W-420-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
049000     05    W-420-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
049100*                                                                         
049200   03    W-WDE411-IDPURAD-X.                                              
049300     05    W-420-IDPURAD2        PIC S9(5)   VALUE ZERO  COMP-3.          
049400*                                                                         
049500   03    W-WDE421-IDKOLLI-X.                                              
049600     05    W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
049700     05    W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
049800*                                                                         
049900   03    W-WDE601-IDPRODNR-X.                                             
050000     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
050100*                                                                         
050200   03    W-WDE611-IDKOLLI-X.                                              
050300     05    W-611-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
050400*                                                                         
050500   03    W-4726-WDGXKEY-ROT-X.                                            
050600     05    W-4726-IDHTYP         PIC X(4)    VALUE '4726'.                
050700     05    W-4726-FLBATCH        PIC X(1)    VALUE SPACE.                 
050800     05    W-4726-LOWVALUE       PIC X(25)   VALUE LOW-VALUE.             
050900*                                                                         
051000   03    W-4726-WDGXKEY-UNDSEG-X.                                         
051100     05    W-4726-IDDISTR        PIC S9(5)   COMP-3.                      
051200     05    W-4726-IDKUNDNR       PIC S9(7)   COMP-3.                      
051300     05    W-4726-IDDC           PIC X(2).                                
051400     05    W-4726-KDFAKTYP       PIC X.                                   
051500*                                                                         
051600   03    W-KDKOLLI-WDK5          PIC X(8)    VALUE SPACE.                 
051700*                                                                         
051800   03    W-4301-WDGXKEY-X.                                                
051900     05    W-4301-IDHTYP         PIC X(4)    VALUE '4301'.                
052000     05    W-4301-IDPRODNR       PIC S9(7)   VALUE ZERO  COMP-3.          
052100     05    W-4301-NYCKEL-VALFRI  PIC X(22)   VALUE LOW-VALUE.             
052200*                                                                         
052300   03    W-4302-WDGXKEY-X.                                                
052400     05    W-4302-IDKOLLI        PIC S9(5)   VALUE ZERO  COMP-3.          
052500     05    W-4302-IDPLKLST       PIC S9(3)   VALUE ZERO  COMP-3.          
052600*                                                                         
052700   03    W-4321-IDHTYP-X.                                                 
052800         05  W-4321-IDHTYP         PIC X(4)  VALUE '4321'.                
052900         05  W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.             
053000*                                                                         
053100   03    W-WDQ201-X.                                                      
053200     05    W-201-IDORDER         PIC S9(7) COMP-3.                        
053300*                                                                         
053400   03    W-WDQ2CSEQ-X.                                                    
053500     05    W-WDQ2C-IDGMTREF-X.                                            
053600       07    W-WDQ2C-IDDISTR     PIC S9(5) COMP-3 VALUE +0.               
053700       07    W-WDQ2C-IDKUNDNR    PIC S9(7) COMP-3 VALUE +0.               
053800       07    W-WDQ2C-IDKUNDRF.                                            
053900         09    FILLER            PIC  9(2)        VALUE ZERO.             
054000         09    W-WDQ2C-IDORDNR5                                           
054100                                 PIC  9(5)        VALUE ZERO.             
054200         09    FILLER            PIC  X(3)        VALUE SPACE.            
054300*                                                                         
054400   03    W-WDQ301-ORDERDEL-X.                                             
054500     05    W-301-IDORDER         PIC S9(7) COMP-3.                        
054600     05    W-301-IDDC            PIC X(2).                                
054700     05    W-301-IDPRODNR        PIC S9(7) COMP-3.                        
054800     05    W-301-IDPLKLST        PIC S9(3) COMP-3.                        
054900*                                                                         
055000   03    W-4471-WDGXKEY-X.                                                
055100     05    W-4471-IDHTYP         PIC X(4)  VALUE '4471'.                  
055200     05    W-4471-IDDC           PIC X(2).                                
055300     05    W-4471-IDPRC.                                                  
055400       07    W-4471-IDPRCBAS     PIC X(3).                                
055500       07    W-4471-IDPRCVAR     PIC X(1).                                
055600     05    FILLER                PIC X(20) VALUE LOW-VALUE.               
055700*                                                                         
055800   03    W-4472-KDSEGKEY-X.                                               
055900     05    W-4472-KDSEGKEY       PIC X(1)  VALUE '1'.                     
056000*                                                                         
056100   03    W-4477-WDGXKEY-X.                                                
056200     05    W-4477-IDHTYP         PIC X(4)  VALUE '4477'.                  
056300     05    W-4477-IDDC           PIC X(2).                                
056400     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
056500*                                                                         
056600   03    W-4478-WDGXKEY-X.                                                
056700     05    W-4478-IDSHIFT        PIC X(1).                                
056800     05    W-4478-IDUSER         PIC X(8).                                
056900     05    FILLER                PIC X(1)  VALUE LOW-VALUE.               
057000*                                                                         
057100     03  W-IDDC-B6-X.                                                     
057200         05 W-IDDC-B6                  PIC X(2).                          
057300                                                                          
057400   03    W-WDA601KY-MIN-X.                                                
057500     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
057600     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
057700     05    W-A601KY-MIN-IDORDNR      PIC 9(07) VALUE ZERO.                
057800     05    FILLER                    PIC X(03) VALUE SPACE.               
057900     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
058000     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
058100     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
058200     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
058300     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
058400                                                                          
058500   03    W-WDA601KY-MAX-X.                                                
058600     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
058700     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
058800     05    W-A601KY-MAX-IDORDNR      PIC 9(07) VALUE ZERO.                
058900     05    FILLER                    PIC X(03) VALUE SPACE.               
059000     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
059100     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
059200     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
059300     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
059400     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
059500                                                                          
059600   03    W-IDARTNR-X.                                                     
059700     05    W-IDARTNR                 PIC S9(9) VALUE ZERO COMP-3.         
059800   03    W-IDDC-X.                                                        
059900     05    W-IDDC                    PIC X(2)  VALUE SPACE.               
060000   03    W-KDSEGKEY-X.                                                    
060100     05    W-KDSEGKEY                PIC X(1)  VALUE '1'.                 
060200     EJECT                                                                
060300 01  HEADER.                                                              
060400     03  FILLER.                                                          
060500        05  FILLER               PIC X(50)  VALUE                         
060600            'Gross weight input from screen does not match with'.         
060700        05  FILLER               PIC X(50)  VALUE                         
060800            ' weight in PULS system for below ORDER details.'.            
060900     03  FILLER.                                                          
061000        05  FILLER               PIC X(50)  VALUE                         
061100            'Please check the parts and emballage weight includ'.         
061200        05  FILLER               PIC X(50)  VALUE                         
061300            'ed in this case stated below.All weight are in KG.'.         
061400     03  FILLER.                                                          
061500        05  FILLER               PIC X(100) VALUE SPACE .                 
061600     03  FILLER.                                                          
061700        05  FILLER               PIC X(5)   VALUE SPACE.                  
061800        05  FILLER               PIC X(8)   VALUE 'District'.             
061900        05  FILLER               PIC X(4)   VALUE X'05050505'.            
062000        05  HEAD-DIST            PIC Z(5)   VALUE ZERO.                   
062100        05  FILLER               PIC X(78)  VALUE SPACE.                  
062200     03  FILLER.                                                          
062300        05  FILLER               PIC X(5)   VALUE SPACE.                  
062400        05  FILLER               PIC X(8)   VALUE 'Customer'.             
062500        05  FILLER               PIC X(4)   VALUE X'05050505'.            
062600        05  HEAD-IDKUNDNR        PIC Z(6)9  VALUE ZERO.                   
062700        05  FILLER               PIC X(76)  VALUE SPACE.                  
062800     03  FILLER.                                                          
062900        05  FILLER               PIC X(5)   VALUE SPACE.                  
063000        05  FILLER               PIC X(12)  VALUE 'Order number'.         
063100        05  FILLER               PIC X(3)   VALUE X'050505'.              
063200        05  HEAD-IDORDER         PIC Z(5)   VALUE ZERO.                   
063300        05  FILLER               PIC X(75)  VALUE SPACE.                  
063400     03  FILLER.                                                          
063500        05  FILLER               PIC X(5)   VALUE SPACE.                  
063600        05  FILLER               PIC X(11)  VALUE 'Case number'.          
063700        05  FILLER               PIC X(4)   VALUE X'05050505'.            
063800        05  HEAD-IDKOLLI         PIC Z(5)   VALUE SPACE.                  
063900        05  HEAD-FILLER          PIC X(1)   VALUE SPACE.                  
064000        05  HEAD-IDKOLLI2        PIC Z(5)   VALUE SPACE.                  
064100        05  FILLER               PIC X(69)  VALUE SPACE.                  
064200     03  FILLER.                                                          
064300        05  FILLER               PIC X(5)   VALUE SPACE.                  
064400        05  FILLER               PIC X(9)   VALUE 'Case code'.            
064500        05  FILLER               PIC X(4)   VALUE X'05050505'.            
064600        05  HEAD-KDKOLLI         PIC X(82)  VALUE SPACE.                  
064700     03  FILLER.                                                          
064800        05  FILLER               PIC X(5)   VALUE SPACE.                  
064900        05  FILLER               PIC X(10)  VALUE 'Picking ID'.           
065000        05  FILLER               PIC X(4)   VALUE X'05050505'.            
065100        05  HEAD-IDPLKLST        PIC Z(8)   VALUE ZERO.                   
065200        05  FILLER               PIC X(73)  VALUE SPACE.                  
065300     03  FILLER.                                                          
065400        05  FILLER               PIC X(5)   VALUE SPACE.                  
065500        05  FILLER               PIC X(12)  VALUE                         
065600                                            'Packing time'.               
065700        05  FILLER               PIC X(4)   VALUE X'05050505'.            
065800        05  HEAD-PACKTIME.                                                
065900           07 HEAD-DATE          PIC X(8)   VALUE SPACE.                  
066000           07 FILLER             PIC X(2)   VALUE SPACE.                  
066100           07 HEAD-TIME          PIC X(69)  VALUE SPACE.                  
066200     03  FILLER.                                                          
066300        05  FILLER               PIC X(5)   VALUE SPACE.                  
066400        05  FILLER               PIC X(17)  VALUE                         
066500                                            'Input case weight'.          
066600        05  FILLER               PIC X(3)   VALUE X'050505'.              
066700        05  HEAD-VKORDBTO        PIC Z(6).999 VALUE ZERO.                 
066800        05  FILLER               PIC X(67)  VALUE SPACE.                  
066900     03  FILLER.                                                          
067000        05  FILLER               PIC X(5)   VALUE SPACE.                  
067100        05  FILLER               PIC X(25)  VALUE                         
067200                                 'Calculated weight for the'.             
067300        05  FILLER               PIC X(2)   VALUE X'0505'.                
067400        05  HEAD-VKARTNTO        PIC Z(6).999 VALUE ZERO.                 
067500        05  FILLER               PIC X(60)  VALUE SPACE.                  
067600     03  FILLER.                                                          
067700        05  FILLER               PIC X(5)   VALUE SPACE.                  
067800        05  FILLER               PIC X(95)  VALUE                         
067900                                     'parts in the case'.                 
068000     03  FILLER.                                                          
068100        05  FILLER               PIC X(100) VALUE SPACE.                  
068200     03  FILLER.                                                          
068300        05  FILLER               PIC X(100) VALUE 'Order line'.           
068400     03  FILLER.                                                          
068500        05  FILLER               PIC X(2)   VALUE 'No'.                   
068600        05  FILLER               PIC X(2)   VALUE X'0505'.                
068700        05  FILLER               PIC X(7)   VALUE 'Part no'.              
068800        05  FILLER               PIC X(2)   VALUE X'0505'.                
068900        05  FILLER               PIC X(6)   VALUE SPACE.                  
069000        05  FILLER               PIC X(3)   VALUE 'Qty'.                  
069100        05  FILLER               PIC X(2)   VALUE X'0505'.                
069200        05  FILLER               PIC X(22)  VALUE                         
069300                                  'Part Weight + Part emb'.               
069400        05  FILLER               PIC X(2)   VALUE X'0505'.                
069500        05  FILLER               PIC X(11)  VALUE                         
069600                                  'Part Weight'.                          
069700        05  FILLER               PIC X(3)   VALUE X'050505'.              
069800        05  FILLER               PIC X(41)  VALUE 'Location'.             
069900 01  HEADER-TAB  REDEFINES HEADER.                                        
070000     03 TAB-LINE   OCCURS 16 TIMES.                                       
070100        05 FILLER  PIC X(100).                                            
070200                                                                          
070300 01  LINEDATA.                                                            
070400     03 LINE-TAB OCCURS 50 TIMES.                                         
070500        05  LINE1-NUMBER         PIC X(5)    VALUE SPACE.                 
070600        05  FILLER               PIC X(2)    VALUE X'0505'.               
070700        05  LINE1-IDARTNR        PIC Z(9)    VALUE ZERO.                  
070800        05  FILLER               PIC X(2)    VALUE X'0505'.               
070900        05  LINE1-KVAVBART       PIC Z(9)    VALUE ZERO.                  
071000        05  FILLER               PIC X(2)    VALUE X'0505'.               
071100        05  LINE1-VKOLDNET       PIC Z(9)9.999 VALUE ZERO.                
071200        05  FILLER               PIC X(3)    VALUE X'050505'.             
071300        05  LINE1-VKNEWNET       PIC Z(9)9.999 VALUE ZERO.                
071400        05  FILLER               PIC X(3)    VALUE X'050505'.             
071500        05  LINE1-ADLAGOMR       PIC Z(3)    VALUE ZERO.                  
071600        05  FILLER               PIC X(1)    VALUE '.'.                   
071700        05  LINE1-ADGANG         PIC 9(2)    VALUE ZERO.                  
071800        05  FILLER               PIC X(1)    VALUE '.'.                   
071900        05  LINE1-ADPLATS        PIC X(5)    VALUE SPACE.                 
072000        05  FILLER               PIC X(1)    VALUE '.'.                   
072100                                                                          
072200 01  LINE-END                    PIC X(100) VALUE                         
072300        'More parts exist.Please check the case.'.                        
072400                                                                          
072500 01  W-RAETT-1.                                                           
072600     03 RAETT-1-SVE              PIC X(21)                                
072700        VALUE 'KOLLIT UPPDATERAT    '.                                    
072800     03 RAETT-1-ENG              PIC X(21)                                
072900        VALUE 'CASE HAS BEEN UPDATED'.                                    
073000 01  FILLER REDEFINES W-RAETT-1.                                          
073100     03 RAETT-1  OCCURS 2        PIC X(21).                               
073200     SKIP3                                                                
073300 01  W-RAETT-2.                                                           
073400     03 RAETT-2-SVE              PIC X(35)                                
073500        VALUE 'FORTSÄTT REGISTRERA, DÄRFTER ENTER.'.                      
073600     03 RAETT-2-ENG              PIC X(35)                                
073700        VALUE 'CONTINUE REGISTRATION - THEN ENTER '.                      
073800 01  FILLER REDEFINES W-RAETT-2.                                          
073900     03 RAETT-2  OCCURS 2        PIC X(35).                               
074000     SKIP3                                                                
074100 01    MEDDELANDE.                                                        
074200   03   UPPLYSN-1.                                                        
074300     05 FILLER                   PIC X(61)  VALUE                         
074400        'MATA IN RADINTERVALL                   '.                        
074500     05 FILLER                   PIC X(61)  VALUE                         
074600        'ENTER LINE INTERVAL                    '.                        
074700   03    FILLER REDEFINES UPPLYSN-1.                                      
074800     05  UPPLYSNING-1  OCCURS 2   PIC X(61).                              
074900   03    FEL-1.                                                           
075000     05  FILLER                  PIC X(40)   VALUE                        
075100        '701 ORDERN SAKNAS                       '.                       
075200     05  FILLER                  PIC X(40)   VALUE                        
075300        '701 ORDER MISSING                       '.                       
075400   03    FILLER  REDEFINES  FEL-1.                                        
075500     05  FEL-701     OCCURS 2    PIC X(40).                               
075600*                                                                         
075700   03    FEL-2.                                                           
075800     05  FILLER                  PIC X(40)   VALUE                        
075900        '716 ORDERN EJ DELAD                     '.                       
076000     05  FILLER                  PIC X(40)   VALUE                        
076100        '716 ORDER HAS NOT BEEN SPLIT            '.                       
076200   03    FILLER  REDEFINES  FEL-2.                                        
076300     05  FEL-716     OCCURS 2    PIC X(40).                               
076400*                                                                         
076500   03    FEL-3.                                                           
076600     05  FILLER                  PIC X(40)   VALUE                        
076700        '715 EJ NUMERISKT                        '.                       
076800     05  FILLER                  PIC X(40)   VALUE                        
076900        '715 NOT NUMERIC                         '.                       
077000   03    FILLER  REDEFINES  FEL-3.                                        
077100     05  FEL-715     OCCURS 2    PIC X(40).                               
077200*                                                                         
077300   03    FEL-4.                                                           
077400     05  FILLER                  PIC X(40)   VALUE                        
077500        '710 ORDERN FÄRDIGRAPPORTERAD            '.                       
077600     05  FILLER                  PIC X(40)   VALUE                        
077700        '710 ORDER TOTALLY REPORTED              '.                       
077800   03    FILLER  REDEFINES  FEL-4.                                        
077900     05  FEL-718     OCCURS 2    PIC X(40).                               
078000*                                                                         
078100   03    FEL-5.                                                           
078200     05  FILLER                  PIC X(40)   VALUE                        
078300        '719 ANGIVEN PACKARE SAKNAS PÅ ORDERN    '.                       
078400     05  FILLER                  PIC X(40)   VALUE                        
078500        '719 PACKER AND ORDER DO NOT MATCH       '.                       
078600   03    FILLER  REDEFINES  FEL-5.                                        
078700     05  FEL-719     OCCURS 2    PIC X(40).                               
078800*                                                                         
078900   03    FEL-6.                                                           
079000     05  FILLER                  PIC X(40)   VALUE                        
079100        '720 ANGIVEN PACKARES ORDERDEL REDAN KLAR'.                       
079200     05  FILLER                  PIC X(40)   VALUE                        
079300        '720 ORDER PART OF PACKER READY          '.                       
079400   03    FILLER  REDEFINES  FEL-6.                                        
079500     05  FEL-720     OCCURS 2    PIC X(40).                               
079600*                                                                         
079700   03    FEL-7.                                                           
079800     05  FILLER                  PIC X(40)   VALUE                        
079900        '721 KOLLIT REDAN RAPPORTERAT            '.                       
080000     05  FILLER                  PIC X(40)   VALUE                        
080100        '721 CASE ALREADY REPORTED               '.                       
080200   03    FILLER  REDEFINES  FEL-7.                                        
080300     05  FEL-721     OCCURS 2    PIC X(40).                               
080400*                                                                         
080500   03    FEL-8.                                                           
080600     05  FILLER                  PIC X(40)   VALUE                        
080700        '722 INTERV. EL DELAR TILLHÖR EJ PACKAREN'.                       
080800     05  FILLER                  PIC X(40)   VALUE                        
080900        '722 INTERVAL DOES NOT BELONG TO PACKER  '.                       
081000   03    FILLER  REDEFINES  FEL-8.                                        
081100     05  FEL-722     OCCURS 2    PIC X(40).                               
081200*                                                                         
081300   03    FEL-9.                                                           
081400     05  FILLER                  PIC X(40)   VALUE                        
081500        '723 INTERVALLET EL DEL DÄRAV REDAN RAPP.'.                       
081600     05  FILLER                  PIC X(40)   VALUE                        
081700        '723 INTERVAL ALREADY REPORTED.          '.                       
081800   03    FILLER  REDEFINES  FEL-9.                                        
081900     05  FEL-723     OCCURS 2    PIC X(40).                               
082000*                                                                         
082100   03    FEL-10.                                                          
082200     05  FILLER                  PIC X(40)   VALUE                        
082300        '724 NOLL FÅR EJ ANGES                   '.                       
082400     05  FILLER                  PIC X(40)   VALUE                        
082500        '724 ZERO NOT ALLOWED.                   '.                       
082600   03    FILLER  REDEFINES  FEL-10.                                       
082700     05  FEL-724     OCCURS 2    PIC X(40).                               
082800*                                                                         
082900   03    FEL-11A.                                                         
083000     05  FILLER                  PIC X(40)   VALUE                        
083100        '725A FÖR STORT ANTAL                    '.                       
083200     05  FILLER                  PIC X(40)   VALUE                        
083300        '725A TOO LARGRE QUANTITY                '.                       
083400   03    FILLER  REDEFINES  FEL-11A.                                      
083500     05  FEL-725A    OCCURS 2    PIC X(40).                               
083600*                                                                         
083700   03    FEL-11B.                                                         
083800     05  FILLER                  PIC X(40)   VALUE                        
083900        '725B FÖR STORT ANTAL                    '.                       
084000     05  FILLER                  PIC X(40)   VALUE                        
084100        '725B TOO LARGRE QUANTITY                '.                       
084200   03    FILLER  REDEFINES  FEL-11B.                                      
084300     05  FEL-725B    OCCURS 2    PIC X(40).                               
084400*                                                                         
084500   03    FEL-11C.                                                         
084600     05  FILLER                  PIC X(40)   VALUE                        
084700        '725C FÖR STORT ANTAL                    '.                       
084800     05  FILLER                  PIC X(40)   VALUE                        
084900        '725C TOO LARGRE QUANTITY                '.                       
085000   03    FILLER  REDEFINES  FEL-11C.                                      
085100     05  FEL-725C    OCCURS 2    PIC X(40).                               
085200*                                                                         
085300   03    FEL-12.                                                          
085400     05  FILLER                  PIC X(40)   VALUE                        
085500        '726 KOLLIKOD SAKNAS                     '.                       
085600     05  FILLER                  PIC X(40)   VALUE                        
085700        '726 CASE CODE MISSING                   '.                       
085800   03    FILLER  REDEFINES  FEL-12.                                       
085900     05  FEL-726     OCCURS 2    PIC X(40).                               
086000*                                                                         
086100   03    FEL-13.                                                          
086200     05  FILLER                  PIC X(40)   VALUE                        
086300        '727 ANGE BRUTTOVIKT                     '.                       
086400     05  FILLER                  PIC X(40)   VALUE                        
086500        '727 ADD GROSS WEIGHT                    '.                       
086600   03    FILLER  REDEFINES  FEL-13.                                       
086700     05  FEL-727     OCCURS 2    PIC X(40).                               
086800*                                                                         
086900   03    FEL-14.                                                          
087000     05  FILLER                  PIC X(40)   VALUE                        
087100        '730 FEL I PLATSSÄTTNINGEN               '.                       
087200     05  FILLER                  PIC X(40)   VALUE                        
087300        '730 ERROR WHEN GENERATING ADDRESS       '.                       
087400   03    FILLER  REDEFINES  FEL-14.                                       
087500     05  FEL-730     OCCURS 2    PIC X(40).                               
087600*                                                                         
087700   03    FEL-15.                                                          
087800     05  FILLER                  PIC X(40)   VALUE                        
087900        '731 EJ BÅDE KOLLI OCH KOLLIINTERVALL    '.                       
088000     05  FILLER                  PIC X(40)   VALUE                        
088100        '731 BOTH CASE AND INTERVAL NOT ALLOWED  '.                       
088200   03    FILLER  REDEFINES  FEL-15.                                       
088300     05  FEL-731     OCCURS 2    PIC X(40).                               
088400*                                                                         
088500   03    FEL-16.                                                          
088600     05  FILLER                  PIC X(40)   VALUE                        
088700        '732 FEL I KOLLIINTERVALL                '.                       
088800     05  FILLER                  PIC X(40)   VALUE                        
088900        '732 ERROR IN CASE INTERVAL              '.                       
089000   03    FILLER  REDEFINES  FEL-16.                                       
089100     05  FEL-732     OCCURS 2    PIC X(40).                               
089200*                                                                         
089300   03    FEL-17.                                                          
089400     05  FILLER                  PIC X(40)   VALUE                        
089500        '733 ENDAST ETT RADINTERVALL GODKÄNT     '.                       
089600     05  FILLER                  PIC X(40)   VALUE                        
089700        '733 MORE THAN ONE INTERVAL NOT ALLOWED  '.                       
089800   03    FILLER  REDEFINES  FEL-17.                                       
089900     05  FEL-733     OCCURS 2    PIC X(40).                               
090000*                                                                         
090100   03    FEL-18.                                                          
090200     05  FILLER                  PIC X(40)   VALUE                        
090300        '734 ENDAST EN RAD I INTERVALLET GODKÄNT '.                       
090400     05  FILLER                  PIC X(40)   VALUE                        
090500        '734 ONLY ONE LINE IN INTERVAL ALLOWED   '.                       
090600   03    FILLER  REDEFINES  FEL-18.                                       
090700     05  FEL-734     OCCURS 2    PIC X(40).                               
090800*                                                                         
090900   03    FEL-19.                                                          
091000     05  FILLER                  PIC X(40)   VALUE                        
091100        '735 RADENS ANT. EJ JÄMNT DELBART I INT.V'.                       
091200     05  FILLER                  PIC X(40)   VALUE                        
091300        'LINE QTY. NOT EVENLY DIVIDED INTO CASES '.                       
091400   03    FILLER  REDEFINES  FEL-19.                                       
091500     05  FEL-735     OCCURS 2    PIC X(40).                               
091600*                                                                         
091700   03    FEL-21.                                                          
091800     05  FILLER                  PIC X(40)   VALUE                        
091900        '7381FELAKTIGA INTERVALLUPPGIFTER        '.                       
092000     05  FILLER                  PIC X(40)   VALUE                        
092100        '7381WRONG INTERVAL INFORMATION          '.                       
092200   03    FILLER  REDEFINES  FEL-21.                                       
092300     05  FEL-7381    OCCURS 2    PIC X(40).                               
092400*                                                                         
092500   03    FEL-21B.                                                         
092600     05  FILLER                  PIC X(40)   VALUE                        
092700        '7382FELAKTIGA INTERVALLUPPGIFTER        '.                       
092800     05  FILLER                  PIC X(40)   VALUE                        
092900        '7382WRONG INTERVAL INFORMATION          '.                       
093000   03    FILLER  REDEFINES  FEL-21B.                                      
093100     05  FEL-7382    OCCURS 2    PIC X(40).                               
093200*                                                                         
093300   03    FEL-21C.                                                         
093400     05  FILLER                  PIC X(40)   VALUE                        
093500        '7383FELAKTIGA INTERVALLUPPGIFTER        '.                       
093600     05  FILLER                  PIC X(40)   VALUE                        
093700        '7383WRONG INTERVAL INFORMATION          '.                       
093800   03    FILLER  REDEFINES  FEL-21C.                                      
093900     05  FEL-7383    OCCURS 2    PIC X(40).                               
094000*                                                                         
094100   03    FEL-21D.                                                         
094200     05  FILLER                  PIC X(40)   VALUE                        
094300        '7384FELAKTIGA INTERVALLUPPGIFTER        '.                       
094400     05  FILLER                  PIC X(40)   VALUE                        
094500        '7384WRONG INTERVAL INFORMATION          '.                       
094600   03    FILLER  REDEFINES  FEL-21D.                                      
094700     05  FEL-7384    OCCURS 2    PIC X(40).                               
094800*                                                                         
094900   03    FEL-22.                                                          
095000     05  FILLER                  PIC X(40)   VALUE                        
095100        '748 UPPLYSTA FÄLT FEL                   '.                       
095200     05  FILLER                  PIC X(40)   VALUE                        
095300        '748 HIGH-LIGHTED FIELDS WRONG           '.                       
095400   03    FILLER  REDEFINES  FEL-22.                                       
095500     05  FEL-748     OCCURS 2    PIC X(40).                               
095600*                                                                         
095700   03    FEL-23.                                                          
095800     05  FILLER                  PIC X(40)   VALUE                        
095900        '749 FEL NYCKEL                          '.                       
096000     05  FILLER                  PIC X(40)   VALUE                        
096100        '749 WRONG KEY                           '.                       
096200   03    FILLER  REDEFINES  FEL-23.                                       
096300     05  FEL-749     OCCURS 2    PIC X(40).                               
096400*                                                                         
096500   03    FEL-24.                                                          
096600     05  FILLER                  PIC X(40)   VALUE                        
096700        '728 KOMPLETTERA KOLLIUPPGIFTER          '.                       
096800     05  FILLER                  PIC X(40)   VALUE                        
096900        '728 MORE CASE INFORMATION NEEDED        '.                       
097000   03    FILLER  REDEFINES  FEL-24.                                       
097100     05  FEL-728     OCCURS 2    PIC X(40).                               
097200*                                                                         
097300   03    FEL-25.                                                          
097400     05  FILLER                  PIC X(40)   VALUE                        
097500        '729 EJ GODKÄND ADRESS                   '.                       
097600     05  FILLER                  PIC X(40)   VALUE                        
097700        '729 WRONG ADDRESS                       '.                       
097800   03    FILLER  REDEFINES  FEL-25.                                       
097900     05  FEL-729     OCCURS 2    PIC X(40).                               
098000*                                                                         
098100   03    FEL-26.                                                          
098200     05  FILLER                  PIC X(40)   VALUE                        
098300        '736 KOLLI I KOLLIINTERVALLET REDAN RAPP '.                       
098400     05  FILLER                  PIC X(40)   VALUE                        
098500        '736CASE IN CASEINTERVAL ALREADY REPORTED'.                       
098600   03    FILLER  REDEFINES  FEL-26.                                       
098700     05  FEL-736     OCCURS 2    PIC X(40).                               
098800*                                                                         
098900   03    FEL-27.                                                          
099000     05  FILLER                  PIC X(40)   VALUE                        
099100        '758 KOLLI SAKNAS                        '.                       
099200     05  FILLER                  PIC X(40)   VALUE                        
099300        '758 CASE MISSING                        '.                       
099400   03    FILLER  REDEFINES  FEL-27.                                       
099500     05  FEL-758     OCCURS 2    PIC X(40).                               
099600*                                                                         
099700   03    FEL-28.                                                          
099800     05  FILLER                  PIC X(40)   VALUE                        
099900        '804 AVVIKELSEKONTROLL PÅGÅR             '.                       
100000     05  FILLER                  PIC X(40)   VALUE                        
100100        '804 DEVIATION CONTROL IN PROGRESS       '.                       
100200   03    FILLER  REDEFINES  FEL-28.                                       
100300     05  FEL-804      OCCURS 2   PIC X(40).                               
100400*                                                                         
100500*  03    FEL-28.                                                          
100600*    05  FILLER                  PIC X(40)   VALUE                        
100700*       '804 FEL, SVARA JA EL.NEJ PÅ BILD 4318.  '.                       
100800*    05  FILLER                  PIC X(40)   VALUE                        
100900*       '804 ERROR, ANSWER YES OR NO ON 4318.    '.                       
101000*  03    FILLER  REDEFINES  FEL-28.                                       
101100*    05  FEL-804      OCCURS 2   PIC X(40).                               
101200*                                                                         
101300   03    FEL-29.                                                          
101400     05  FILLER.                                                          
101500         07  FILLER       PIC X(10) VALUE 'EJ MER ÄN '.                   
101600         07  FEL-PLATSIX-C1  PIC 9(3).                                    
101700         07  FILLER       PIC X(19) VALUE ' KOLLIN I INTERVALL'.          
101800         07  FILLER       PIC X(8)  VALUE SPACE.                          
101900     05  FILLER.                                                          
102000         07  FILLER       PIC X(5)  VALUE 'MAX. '.                        
102100         07  FEL-PLATSIX-C2  PIC 9(3).                                    
102200         07  FILLER       PIC X(18) VALUE ' CASES  / INTERVAL'.           
102300         07  FILLER       PIC X(14) VALUE SPACE.                          
102400   03    FILLER  REDEFINES  FEL-29.                                       
102500     05  FEL-827      OCCURS 2   PIC X(40).                               
102600*                                                                         
102700   03    FEL-30.                                                          
102800     05  FILLER                  PIC X(40)   VALUE                        
102900        '826 FÖR MÅNGA RADER PÅ BILD, MAX 200    '.                       
103000     05  FILLER                  PIC X(40)   VALUE                        
103100        '826 TOO MANY LINES, MAX. 200 LINES      '.                       
103200   03    FILLER  REDEFINES  FEL-30.                                       
103300     05  FEL-826      OCCURS 2   PIC X(40).                               
103400*                                                                         
103500   03    FEL-31.                                                          
103600     05  FILLER                  PIC X(40)   VALUE                        
103700        '830 RADEN NOLLAD AV NOLLJAGARE          '.                       
103800     05  FILLER                  PIC X(40)   VALUE                        
103900        '830 LINE ZEROED BY ZEROHUNTER           '.                       
104000   03    FILLER  REDEFINES  FEL-31.                                       
104100     05  FEL-830      OCCURS 2   PIC X(40).                               
104200*                                                                         
104300   03    FEL-33.                                                          
104400     05  FILLER                  PIC X(40)   VALUE                        
104500        '772 FELAKTIG PRINTER                    '.                       
104600     05  FILLER                  PIC X(40)   VALUE                        
104700        '772 WRONG PRINTER                       '.                       
104800   03    FILLER  REDEFINES  FEL-33.                                       
104900     05  FEL-772     OCCURS 2    PIC X(40).                               
105000*                                                                         
105100   03    FEL-34.                                                          
105200     05  FILLER                  PIC X(40)   VALUE                        
105300        '773 MAX 100KOLLI INT.VALL OCH EN PRINTER'.                       
105400     05  FILLER                  PIC X(40)   VALUE                        
105500        '773 MAX 100CASES IN INTERVAL & 1 PRINTER'.                       
105600   03    FILLER  REDEFINES  FEL-34.                                       
105700     05  FEL-773     OCCURS 2    PIC X(40).                               
105800*                                                                         
105900   03    FEL-35.                                                          
106000     05  FILLER                  PIC X(40)   VALUE                        
106100        '792 SAMKOLLI HAR ANNAN TRANSPORT        '.                       
106200     05  FILLER                  PIC X(40)   VALUE                        
106300        '792 MIXED CASE ON OTHER TRANSPORT       '.                       
106400   03    FILLER  REDEFINES  FEL-35.                                       
106500     05  FEL-792     OCCURS 2    PIC X(40).                               
106600*                                                                         
106700   03    FEL-36.                                                          
106800     05  FILLER                  PIC X(40)   VALUE                        
106900        '793 SAMKOLLI HAR INGEN TRANSPORT        '.                       
107000     05  FILLER                  PIC X(40)   VALUE                        
107100        '793 MIXED CASE HAS NO TRANSPORT         '.                       
107200   03    FILLER  REDEFINES  FEL-36.                                       
107300     05  FEL-793     OCCURS 2    PIC X(40).                               
107400*                                                                         
107500   03    FEL-37.                                                          
107600     05  FILLER                  PIC X(40)   VALUE                        
107700        '794 EJ FARLIGT GODS I SAMKOLLI          '.                       
107800     05  FILLER                  PIC X(40)   VALUE                        
107900        '794 NO DANGEROUS CARGO IN MIXED CASE '.                          
108000   03    FILLER  REDEFINES  FEL-37.                                       
108100     05  FEL-794     OCCURS 2    PIC X(40).                               
108200*                                                                         
108300   03    FEL-38.                                                          
108400     05  FILLER                  PIC X(40)   VALUE                        
108500        '795 LAGER-KOD SAKNAS                    '.                       
108600     05  FILLER                  PIC X(40)   VALUE                        
108700        '795 WAREHOUSE-CODE MISSING              '.                       
108800   03    FILLER  REDEFINES  FEL-38.                                       
108900     05  FEL-795     OCCURS 2    PIC X(40).                               
109000*                                                                         
109100   03    FEL-39.                                                          
109200     05  FILLER                  PIC X(40)   VALUE                        
109300        '796 MAX 50 KOLLI INT.VALL & AF+FS PRINT.'.                       
109400     05  FILLER                  PIC X(40)   VALUE                        
109500        '796 MAX 50CASES IN INTERVAL WITH TWOPRNT'.                       
109600   03    FILLER  REDEFINES  FEL-39.                                       
109700     05  FEL-796     OCCURS 2    PIC X(40).                               
109800*                                                                         
109900   03    FEL-40.                                                          
110000     05  FILLER                  PIC X(40)   VALUE                        
110100        '797 VIKT ÖVER 350 KG. RAPPORTERA BRUTTO.'.                       
110200     05  FILLER                  PIC X(40)   VALUE                        
110300        '797 WEIGHT > 350 KG. REPORT GROSS WEIGHT'.                       
110400   03    FILLER  REDEFINES  FEL-40.                                       
110500     05  FEL-797     OCCURS 2    PIC X(40).                               
110600*                                                                         
110700   03    FEL-41.                                                          
110800     05  FILLER                  PIC X(40)   VALUE                        
110900        '798 BRUTTO-VIKT ÄR MINDRE ÄN NETTO-VIKT.'.                       
111000     05  FILLER                  PIC X(40)   VALUE                        
111100        '798 GROSS WEIGHT MUST BE MORE THAN NET W'.                       
111200   03    FILLER  REDEFINES  FEL-41.                                       
111300     05  FEL-798     OCCURS 2    PIC X(40).                               
111400*                                                                         
111500   03    FEL-42.                                                          
111600     05  FILLER                  PIC X(40)   VALUE                        
111700        '799 DISTRIKT ÄR INTE SAMPACKN.DISTRIKT. '.                       
111800     05  FILLER                  PIC X(40)   VALUE                        
111900        '799 DISTRICT IS NOT A MIXED DISTRICT.   '.                       
112000   03    FILLER  REDEFINES  FEL-42.                                       
112100     05  FEL-799     OCCURS 2    PIC X(40).                               
112200*                                                                         
112300   03    FEL-50.                                                          
112400     05  FILLER                  PIC X(40)   VALUE                        
112500        '800 DIREKT LEV. KOLLI FÅR EJ PACK.PÅ4315'.                       
112600     05  FILLER                  PIC X(40)   VALUE                        
112700        '800 NOT ALLOW.TO PACK DIRECT SUPPL.CASE.'.                       
112800   03    FILLER  REDEFINES  FEL-50.                                       
112900     05  FEL-800     OCCURS 2    PIC X(40).                               
113000*                                                                         
113100**********************************************                            
113200   03    FEL-41.                                                          
113300     05  FILLER                  PIC X(40)   VALUE                        
113400        '7011 ORDERN SAKNAS                      '.                       
113500     05  FILLER                  PIC X(40)   VALUE                        
113600        '7011 ORDER MISSING                      '.                       
113700   03    FILLER  REDEFINES  FEL-41.                                       
113800     05  FEL-7011    OCCURS 2    PIC X(40).                               
113900**                                                                        
114000   03    FEL-42.                                                          
114100     05  FILLER                  PIC X(40)   VALUE                        
114200        '7012 ORDERN SAKNAS                      '.                       
114300     05  FILLER                  PIC X(40)   VALUE                        
114400        '7012 ORDER MISSING                      '.                       
114500   03    FILLER  REDEFINES  FEL-42.                                       
114600     05  FEL-7012    OCCURS 2    PIC X(40).                               
114700**                                                                        
114800   03    FEL-43.                                                          
114900     05  FILLER                  PIC X(40)   VALUE                        
115000        '7013 ORDERN SAKNAS                      '.                       
115100     05  FILLER                  PIC X(40)   VALUE                        
115200        '7013 ORDER MISSING                      '.                       
115300   03    FILLER  REDEFINES  FEL-43.                                       
115400     05  FEL-7013    OCCURS 2    PIC X(40).                               
115500**                                                                        
115600   03    FEL-44.                                                          
115700     05  FILLER                  PIC X(40)   VALUE                        
115800        '7014 ORDERN SAKNAS                      '.                       
115900     05  FILLER                  PIC X(40)   VALUE                        
116000        '7014 ORDER MISSING                      '.                       
116100   03    FILLER  REDEFINES  FEL-44.                                       
116200     05  FEL-7014    OCCURS 2    PIC X(40).                               
116300**                                                                        
116400   03    FEL-45.                                                          
116500     05  FILLER                  PIC X(40)   VALUE                        
116600        '7015 ORDERN SAKNAS                      '.                       
116700     05  FILLER                  PIC X(40)   VALUE                        
116800        '7015 ORDER MISSING                      '.                       
116900   03    FILLER  REDEFINES  FEL-45.                                       
117000     05  FEL-7015    OCCURS 2    PIC X(40).                               
117100**                                                                        
117200   03    FEL-46.                                                          
117300     05  FILLER                  PIC X(40)   VALUE                        
117400        '7016 ORDERN SKALL RAPPORTERAS PÅ WEB-LDC'.                       
117500     05  FILLER                  PIC X(40)   VALUE                        
117600        '7016 ORDER MUST BE REPORTED ON WEB-LDC  '.                       
117700   03    FILLER  REDEFINES  FEL-46.                                       
117800     05  FEL-7016    OCCURS 2    PIC X(40).                               
117900**                                                                        
118000   03    FEL-47.                                                          
118100     05  FILLER                  PIC X(40)   VALUE                        
118200        '7017 INTE KOLLI INTERVALL FÖR ECOM      '.                       
118300     05  FILLER                  PIC X(40)   VALUE                        
118400        '7017 NOT CASE INTERVALL FOR ECOM        '.                       
118500   03    FILLER  REDEFINES  FEL-47.                                       
118600     05  FEL-7017    OCCURS 2    PIC X(40).                               
118700**********************************************                            
118800*                                                                         
118900     EJECT                                                                
119000 01    FILLER                 PIC X(16) VALUE 'EMB-TABELL'.               
119100     SKIP3                                                                
119200 01    EMB-TABELL.                                                        
119300   03    EMB-TAB-X.                                                       
119400     05  KLASS        OCCURS 3   INDEXED BY KL-INDX.                      
119500         07  TYP      OCCURS 5   INDEXED BY TYP-INDX                      
119600                                         PIC S9(5)  COMP-3.               
119700   03    EMB-TAB   REDEFINES  EMB-TAB-X.                                  
119800     05  FILLER.                                                          
119900         07  PALLAR   OCCURS 5           PIC S9(5)  COMP-3.               
120000     05  FILLER.                                                          
120100         07  KRAGAR   OCCURS 5           PIC S9(5)  COMP-3.               
120200     05  FILLER.                                                          
120300         07  EMB-LOCK OCCURS 5           PIC S9(5)  COMP-3.               
120400     EJECT                                                                
120500 01    FILLER                 PIC X(16) VALUE 'FG-TABELL'.                
120600                                                                          
120700 01    FG-TABELL.                                                         
120800   03    TAB-POST OCCURS 10.                                              
120900                                                                          
121000     05  TAB-IDPSN            PIC 9(3)              VALUE ZERO.           
121100                                                                          
121200     05  TAB-VKART-FG         PIC S9(7)      COMP-3 VALUE ZERO.           
121300                                                                          
121400     05  TAB-VLFG             PIC S9(4)V9(3) COMP-3 VALUE ZERO.           
121500     EJECT                                                                
121600******************************************************************        
121700*                                                                *        
121800*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
121900*                                                                *        
122000******************************************************************        
122100 01    FILLER                 PIC X(16) VALUE 'MID W4I31501 MID'.         
122200     SKIP3                                                                
122300*01    MID -COPY W4I31501.                                                
122400     EJECT                                                                
122500 01    FILLER                 PIC X(16) VALUE 'MOD W4I31501 MOD'.         
122600*01    -COPY WMSGAREA                                                     
122700     EJECT                                                                
122800*  03    MOD -COPY W4O31501.                                              
122900     EJECT                                                                
123000 01    FILLER                 PIC X(16) VALUE 'MID W0I60502 MID'.         
123100*01      MID -COPY W0I60502  -PRE 0605-.                                  
123200     EJECT                                                                
123300 01    FILLER                 PIC X(16) VALUE 'MID W4I31801 MID'.         
123400*01      MID -COPY W4I31801  -PRE 4318-.                                  
123500     EJECT                                                                
123600 01    FILLER                 PIC X(16) VALUE 'MOD W4O31401 MOD'.         
123700*01      MOD -COPY W4O31401                 -PRE 4314-.                   
123800     EJECT                                                                
123900 01    FILLER                 PIC X(16) VALUE 'MOD W4O31701 MOD'.         
124000*01      MOD -COPY W4O31701                 -PRE 4317-.                   
124100     SKIP2                                                                
124200 01    FILLER                 PIC X(16) VALUE 'MOD W40636I1 MOD'.         
124300*01  -COPY W40636I1   -PRE MOD4636-                                       
124400     EJECT                                                                
124500 01    FILLER                 PIC X(16) VALUE 'MID W4I33301 MID'.         
124600 01  4333-MID-IO-AREA.                                                    
124700                                                                          
124800       03  4333-MID-LL           PIC S9(4)   COMP SYNC.                   
124900       03  4333-MID-Z1           PIC X.                                   
125000       03  4333-MID-Z2           PIC X.                                   
125100       03  4333-MID-TRANSKOD     PIC X(8).                                
125200       03  4333-MID-IDTRANS      PIC X(4).                                
125300       03  4333-MID-KDMFSFOR     PIC X.                                   
125400*      03  MID -COPY W4I33301  -PRE 4333-.                                
125500     SKIP2                                                                
125600   03  FILLER                 PIC X(16) VALUE 'MID W4I34101 MID'.         
125700   01  4341-MID-IO-AREA.                                                  
125800                                                                          
125900       03  4341-MID-LL           PIC S9(4)   COMP SYNC.                   
126000       03  4341-MID-Z1           PIC X.                                   
126100       03  4341-MID-Z2           PIC X.                                   
126200       03  4341-MID-TRANSKOD     PIC X(8).                                
126300       03  4341-MID-IDTRANS      PIC X(4).                                
126400       03  4341-MID-KDMFSFOR     PIC X.                                   
126500       03  4341-MID-DATA-AREA    PIC X(61).                               
126600*      03  MID -COPY W4I34101 -RED 4341-MID-DATA-AREA -PRE 4341-.         
126700     SKIP2                                                                
126800 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
126900     SKIP3                                                                
127000*01    -COPY WMFSAREA                                                     
127100     EJECT                                                                
127200*01  XXJK  -COPY WDGX4322    -PRE XXJK-                                   
127300     EJECT                                                                
127400******************************************************************        
127500*                                                                         
127600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
127700*                                                                         
127800 01    IMS-WS.                                                            
127900   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
128000     SKIP3                                                                
128100*                        **** STATUS-KOD FRÅN IMS                         
128200   03    STATUS-KUNDORDER-SEK-WS PIC X(02).                               
128300     88    KUNDORDER-SEK-FINNS               VALUE '  '.                  
128400     88    KUNDORDER-SEK-SAKNAS              VALUE 'GE' 'GB'.             
128500   03    STATUS-WS               PIC XX.                                  
128600     88    SEGMENT-FINNS                     VALUE '  '.                  
128700     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
128800     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
128900     88    END-OF-DATABASE                   VALUE 'GB'.                  
129000     SKIP3                                                                
129100   03    STATUS-WS-WDK6          PIC XX.                                  
129200     88    SEGMENT-FINNS-WDK6                VALUE '  '.                  
129300     88    SEGMENT-SAKNAS-WDK6               VALUE 'GE'.                  
129400     88    SEGMENT-FINNS-REDAN-WDK6          VALUE 'II'.                  
129500     88    END-OF-DATABASE-WDK6              VALUE 'GB'.                  
129600     SKIP3                                                                
129700   03    STATUS-WS-WDK7          PIC XX.                                  
129800     88    SEGMENT-FINNS-WDK7                VALUE '  '.                  
129900     88    SEGMENT-SAKNAS-WDK7               VALUE 'GE'.                  
130000     88    SEGMENT-FINNS-REDAN-WDK7          VALUE 'II'.                  
130100     88    END-OF-DATABASE-WDK7              VALUE 'GB'.                  
130200     SKIP3                                                                
130300   03    GODK-STATUSKODER.                                                
130400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
130500     SKIP3                                                                
130600 01    SSA1                      PIC X(120).                              
130700 01    SSA2                      PIC X(96).                               
130800 01    SSA3                      PIC X(96).                               
130900 01    SSA4                      PIC X(96).                               
131000     EJECT                                                                
131100*                            IMS FUNKTIONSKODER                           
131200*01    -COPY W0003                                                        
131300     EJECT                                                                
131400 01    FILLER    PIC X(16)  VALUE 'DLI-IO-K501 '.                         
131500 01    DLI-IO-K501.                                                       
131600*  03    EMBB01   -COPY WDK501                                            
131700     EJECT                                                                
131800 01    FILLER    PIC X(16)  VALUE 'DLI-IO-E401 '.                         
131900 01    DLI-IO-E401.                                                       
132000*  03    WDE401 -COPY WDE401                                              
132100     EJECT                                                                
132200 01    FILLER    PIC X(16)  VALUE 'DLI-IO-E411 '.                         
132300 01    DLI-IO-E411.                                                       
132400*  03    WDE411 -COPY WDE411                                              
132500     EJECT                                                                
132600 01    FILLER    PIC X(16)  VALUE 'DLI-IO-E421 '.                         
132700 01    DLI-IO-E421.                                                       
132800*  03    WDE421 -COPY WDE421                                              
132900     EJECT                                                                
133000 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA2'.               
133100 01    DLI-IO-AREA2.                                                      
133200   03    IO-AREA2                PIC X(400)  VALUE SPACE.                 
133300     SKIP3                                                                
133400*  03    WDE601   -COPY WDE601             -RED IO-AREA2.                 
133500     EJECT                                                                
133600*  03    WDE611   -COPY WDE611             -RED IO-AREA2.                 
133610     EJECT                                                                
133710 01  FILLER               PIC X(16)   VALUE 'WDE621  '.                   
133720 01  DLI-IO-WDE621.                                                       
133730*    03  -COPY WDE621                                                     
133740                                                                          
133800 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA3'.               
133900 01    DLI-IO-AREA3.                                                      
134000   03    IO-AREA3                PIC X(25)   VALUE SPACE.                 
134100     SKIP3                                                                
134200*  03    WLXXDV11 -COPY WDGX4726           -RED IO-AREA3.                 
134300     EJECT                                                                
134400*  03    WLXXDV21 -COPY WDGX4727           -RED IO-AREA3.                 
134500     EJECT                                                                
134600 01  DLI-IO-AREA4.                                                        
134700     03  IO-AREA4                PIC X(100)  VALUE SPACE.                 
134800*                                                                         
134900*    03  WLXXJK01  -COPY WDGX01      -PRE 4321-  -RED IO-AREA4            
135000*    03  WLXXJK11  -COPY WDGX4322    -RED IO-AREA4                        
135100     EJECT                                                                
135200 01  DLI-IO-AREA5.                                                        
135300     03  IO-AREA5                PIC X(256)  VALUE SPACE.                 
135400*                                                                         
135500*    03  WLORQA01  -COPY WDQ301      -RED IO-AREA5                        
135600     EJECT                                                                
135700 01  DLI-IO-AREA6.                                                        
135800     03  IO-AREA6                PIC X(1000) VALUE SPACE.                 
135900*                                                                         
136000*    03  WLXXKW11  -COPY WDGX4472    -RED IO-AREA6                        
136100     EJECT                                                                
136200*    03  WLXXLB11  -COPY WDGX4478    -RED IO-AREA6                        
136300     EJECT                                                                
136400 01    DLI-IO-AREA7.                                                      
136500   03    IO-AREA7                PIC X(150)  VALUE SPACE.                 
136600     SKIP3                                                                
136700*  03  WDGZ01     -COPY WDGZ01  -PRE LOGG-   -RED IO-AREA7.               
136800     EJECT                                                                
136900 01    FILLER                    PIC X(16) VALUE 'DLI-IO-WDQ201'.         
137000 01    DLI-IO-Q201.                                                       
137100*  03  -COPY WDQ201                                                       
137200 01    FILLER                    PIC X(16) VALUE 'DLI-IO-WDQ212'.         
137300 01    DLI-IO-Q212.                                                       
137400*  03  -COPY WDQ212                                                       
137500*                                                                         
137600 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA9'.          
137700 01    DLI-IO-AREA9.                                                      
137800   03    IO-AREA9                PIC X(100)  VALUE SPACE.                 
137900*  03    WLXXDU01  -COPY WDGX4301    -RED IO-AREA9.                       
138000     SKIP2                                                                
138100*  03    WLXXDU11  -COPY WDGX4302    -RED IO-AREA9.                       
138200     SKIP2                                                                
138300*01      WDGZRYK  -COPY WDGZRYK.                                          
138400     SKIP2                                                                
138500 01    FILLER                    PIC X(16) VALUE 'WDE4A-AREA'.            
138600 01    WDE4A-IO-AREA.                                                     
138700   03    WDE4A-AREA              PIC X(100)  VALUE SPACE.                 
138800*  03    WDE4A1    -COPY WDE4A1                                           
138900                                                                          
139000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
139100 01   DLI-IO-AREA-B601.                                                   
139200*     03  -COPY WDB601                                                    
139300                                                                          
139400 01  FILLER               PIC X(16)   VALUE 'WDA601 AREA'.                
139500 01   DLI-IO-AREA-A601.                                                   
139600*     03  -COPY WDA601                                                    
139700 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK601'.              
139800 01    DLI-IO-WDK601.                                                     
139900*      03  -COPY WDK601                                                   
140000     EJECT                                                                
140100 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK611'.              
140200 01    DLI-IO-WDK611.                                                     
140300*      03  -COPY WDK611                                                   
140400     EJECT                                                                
140500 01    FILLER             PIC X(16)   VALUE 'DLI-IO-WDK711'.              
140600 01    DLI-IO-WDK711.                                                     
140700*      03  -COPY WDK711                                                   
140800     EJECT                                                                
140900 LINKAGE SECTION.                                                         
141000*01    -COPY W0009     -PRE MSG-                                          
141100     EJECT                                                                
141200*01    -COPY W0009     -PRE ALT0605-                                      
141300     EJECT                                                                
141400*01    -COPY W0009     -PRE ALT4333-                                      
141500     EJECT                                                                
141600*01    -COPY W0009     -PRE ALT4341-                                      
141700     EJECT                                                                
141800*01    -COPY W0009     -PRE ALT-                                          
141900     EJECT                                                                
142000*01    -COPY W0009     -PRE DISTRDOC-                                     
142100     EJECT                                                                
142200 01  TMS-CRE-PCB                 PIC X.                                   
142300 01  TMS-DEL-PCB                 PIC X.                                   
142400     EJECT                                                                
142500 01  ATAB-PCB                    PIC X.                                   
142600     EJECT                                                                
142700*01    -COPY W0008     -PRE USEA-                                         
142800     05  FILLER                  PIC X.                                   
142900     EJECT                                                                
143000*01    -COPY W0008     -PRE WDE41-                                        
143100     05  FILLER                  PIC X.                                   
143200     SKIP2                                                                
143300*01    -COPY W0008     -PRE WDE4A-                                        
143400     05  FILLER                  PIC X.                                   
143500     EJECT                                                                
143600*01    -COPY W0008     -PRE WDE4-                                         
143700     05  FILLER                  PIC X.                                   
143800     EJECT                                                                
143900*01    -COPY W0008     -PRE WDE42-                                        
144000     05  FILLER                  PIC X.                                   
144100     EJECT                                                                
144200*01    -COPY W0008     -PRE WDE6-                                         
144300     05  FILLER                  PIC X.                                   
144400     EJECT                                                                
144500*01    -COPY W0008     -PRE EMBB-                                         
144600     05  FILLER                  PIC X.                                   
144700     EJECT                                                                
144800*01    -COPY W0008     -PRE XXDV-                                         
144900     05  FILLER                  PIC X.                                   
145000     EJECT                                                                
145100*01    -COPY W0008     -PRE ORQA-                                         
145200     05  FILLER                  PIC X.                                   
145300     EJECT                                                                
145400*01    -COPY W0008     -PRE XXKW-                                         
145500     05  FILLER                  PIC X.                                   
145600     EJECT                                                                
145700*01    -COPY W0008     -PRE XXLB-                                         
145800     05  FILLER                  PIC X.                                   
145900     EJECT                                                                
146000*01    -COPY W0008     -PRE XXJK-                                         
146100     05  FILLER                  PIC X.                                   
146200     EJECT                                                                
146300*01    -COPY W0008     -PRE ZZAC-                                         
146400     05  FILLER                  PIC X.                                   
146500     EJECT                                                                
146600*01    -COPY W0008     -PRE ORQI-                                         
146700     05  FILLER                  PIC X.                                   
146800     EJECT                                                                
146900*01  -COPY W0008       -PRE ORQL-                                         
147000     05  FILLER                  PIC X.                                   
147100     EJECT                                                                
147200*01    -COPY W0008     -PRE WDE62-                                        
147300     05  FILLER                  PIC X.                                   
147400     EJECT                                                                
147500*01  -COPY W0008       -PRE PLATS-DM-                                     
147600     05  FILLER                  PIC X.                                   
147700     EJECT                                                                
147800*01  -COPY W0008       -PRE PLATS-DN-                                     
147900     05  FILLER                  PIC X.                                   
148000     EJECT                                                                
148100*01  -COPY W0008       -PRE PLATS-DP-                                     
148200     05  FILLER                  PIC X.                                   
148300     EJECT                                                                
148400*01  -COPY W0008       -PRE PLATS-DO-                                     
148500     05  FILLER                  PIC X.                                   
148600     EJECT                                                                
148700*01  -COPY W0008       -PRE PLATS-WDE6C-                                  
148800     05  FILLER                  PIC X.                                   
148900     EJECT                                                                
149000*01  -COPY W0008       -PRE PLATS-GMTC-                                   
149100     05  FILLER                  PIC X.                                   
149200     EJECT                                                                
149300*01  -COPY W0008       -PRE PLATS-WDB6-                                   
149400     05  FILLER                  PIC X.                                   
149500     EJECT                                                                
149600*01  -COPY W0008       -PRE XXDU-                                         
149700     05  FILLER                  PIC X.                                   
149800     EJECT                                                                
149900*01  -COPY W0008       -PRE WDB6-                                         
150000     05  FILLER                  PIC X.                                   
150100     EJECT                                                                
150200*01  -COPY W0008       -PRE WDA6B-                                        
150300     05  FILLER                  PIC X.                                   
150400*01  -COPY W0008       -PRE WDK6-                                         
150500     05  FILLER                  PIC X.                                   
150600     EJECT                                                                
150700*01  -COPY W0008       -PRE WDK7-                                         
150800     05  FILLER                  PIC X.                                   
150900     EJECT                                                                
151000 01  DNOT-ORQP-PCB               PIC X.                                   
151100 01  DNOT-ORQP2-PCB              PIC X.                                   
151200 01  DNOT-ORQP3-PCB              PIC X.                                   
151300 01  DNOT-4013-PCB               PIC X.                                   
151400 01  DNOT-BENA-PCB               PIC X.                                   
151500 01  TMS-1165-PCB                PIC X.                                   
151600 01  TMS-4141-PCB                PIC X.                                   
151700 01  TMS-WDB2-PCB                PIC X.                                   
151800 01  TMS-WDB6-PCB                PIC X.                                   
151900 01  TMS-WDD3-PCB                PIC X.                                   
152000 01  TMS-WDB1-PCB                PIC X.                                   
152100 01  TMS-WDE4A-PCB               PIC X.                                   
152200 01  TMS-WDE4F-PCB               PIC X.                                   
152300 01  TMS-WDQ2-PCB                PIC X.                                   
152400 01  TMS-WDQ3-PCB                PIC X.                                   
152500 01  TMS-WDK6-PCB                PIC X.                                   
152600 01  TMS-WDE6-PCB                PIC X.                                   
152700 01  TMS-WDK5-PCB                PIC X.                                   
152800 01  TMS-WDQ2C-PCB               PIC X.                                   
152900     EJECT                                                                
153000  PROCEDURE DIVISION USING MSG-PCB ALT0605-PCB ALT4333-PCB                
153100                           ALT4341-PCB ALT-PCB                            
153200                           DISTRDOC-PCB TMS-CRE-PCB TMS-DEL-PCB           
153300                           ATAB-PCB USEA-PCB                              
153400                           WDE41-PCB WDE4A-PCB WDE4-PCB WDE42-PCB         
153500                           WDE6-PCB EMBB-PCB  XXDV-PCB ORQA-PCB           
153600                           XXKW-PCB XXLB-PCB  XXJK-PCB ZZAC-PCB           
153700                           ORQI-PCB ORQL-PCB WDE62-PCB                    
153800                           PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB         
153900                           PLATS-DO-PCB PLATS-WDE6C-PCB                   
154000                           PLATS-GMTC-PCB PLATS-WDB6-PCB                  
154100                           XXDU-PCB WDB6-PCB WDA6B-PCB                    
154200                           DNOT-ORQP-PCB                                  
154300                           DNOT-ORQP2-PCB                                 
154400                           DNOT-ORQP3-PCB                                 
154500                           DNOT-4013-PCB                                  
154600                           DNOT-BENA-PCB WDK6-PCB WDK7-PCB                
154700                          TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB          
154800                          TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB          
154900                          TMS-WDE4A-PCB TMS-WDE4F-PCB                     
155000                          TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB          
155100                          TMS-WDE6-PCB TMS-WDK5-PCB TMS-WDQ2C-PCB.        
155200     ENTRY 'DLITCBL' USING MSG-PCB ALT0605-PCB ALT4333-PCB                
155300                           ALT4341-PCB ALT-PCB                            
155400                           USEA-PCB DISTRDOC-PCB                          
155500                           TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB               
155600                           WDE41-PCB WDE4A-PCB WDE4-PCB WDE42-PCB         
155700                           WDE6-PCB EMBB-PCB  XXDV-PCB ORQA-PCB           
155800                           XXKW-PCB XXLB-PCB  XXJK-PCB ZZAC-PCB           
155900                           ORQI-PCB ORQL-PCB WDE62-PCB                    
156000                           PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB         
156100                           PLATS-DO-PCB PLATS-WDE6C-PCB                   
156200                           PLATS-GMTC-PCB PLATS-WDB6-PCB                  
156300                           XXDU-PCB WDB6-PCB WDA6B-PCB                    
156400                           DNOT-ORQP-PCB                                  
156500                           DNOT-ORQP2-PCB                                 
156600                           DNOT-ORQP3-PCB                                 
156700                           DNOT-4013-PCB                                  
156800                           DNOT-BENA-PCB WDK6-PCB WDK7-PCB                
156900                          TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB          
157000                          TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB          
157100                          TMS-WDE4A-PCB TMS-WDE4F-PCB                     
157200                          TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB          
157300                          TMS-WDE6-PCB TMS-WDK5-PCB TMS-WDQ2C-PCB.        
157400                                                                          
157500     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-DATUM                        
157600     MOVE FUNCTION CURRENT-DATE (9:8)  TO WS-KLOCKAN                      
157700                                                                          
157800     PERFORM IMS-GET-MSG                                                  
157900     IF SEGMENT-FINNS                                                     
158000         PERFORM A-INIT                                                   
158100*                                                                         
158200         EVALUATE TRUE                                                    
158300         WHEN   WS-SAMMA-BILD                                             
158400         OR     WS-ORDERVIS-TRANS                                         
158500             PERFORM B-GENERELL-KONTROLL                                  
158600*                                                                         
158700             IF WS-INDATA-RATT                                            
158800                 PERFORM C-RELATIONSKONTROLL                              
158900*                                                                         
159000                 IF WS-INDATA-RATT                                        
159100                     PERFORM D-LAGG-UPP-KOLLI-SEG                         
159200                                                                          
159300                     IF WS-SAMMA-BILD                                     
159400                     OR WS-ORDERVIS-TRANS                                 
159500                         MOVE +1 TO INX                                   
159600                         MOVE 1  TO LINE-IX                               
159700                         PERFORM UNTIL INX NOT <                          
159800                                        MAX-RAD-ANTAL-PLUS-1              
159900                             PERFORM F-BEHANDLA-RADER                     
160000                             ADD +1 TO INX                                
160100                         END-PERFORM                                      
160200                         PERFORM I-KONTROLLERA-WEIGHT                     
160300                         PERFORM L-HAMTA-ADRESS                           
160400                     END-IF                                               
160500                     IF WS-BEHANDLING-RATT                                
160600                       IF  NOT WS-ORDERVIS-TRANS                          
160700                           OR                                             
160800                          (WS-ORDERVIS-TRANS                              
160900                           AND WS-IDKOLLI < '99000')                      
161000                         PERFORM G-UPPDATERA-KOLLIREG                     
161100                         IF MID-IDRADNR-FOM (12) = ALL '+'                
161200                           IF  WS-SAMMA-BILD                              
161300                           AND WS-FLAUTFAK  = JA                          
161400                             IF DIST03-SVERIGE-EJ-778                     
161500                             OR DIST03-SVERIGE-2                          
161600                             OR DIST18-SKROT                              
161700                                PERFORM K-UPPDAT-4726-4727                
161800                             END-IF                                       
161900                           END-IF                                         
162000                         END-IF                                           
162100                         PERFORM S02-RENSA-MOD-FALT                       
162200                       END-IF                                             
162300                     END-IF                                               
162400                 ELSE                                                     
162500                     PERFORM S04-ADD-LAES-IN-FAELT                        
162600                 END-IF                                                   
162700             END-IF                                                       
162800             MOVE MAX-MOD-LAENGD TO  MSG-KVLL                             
162900                                                                          
163000         WHEN WS-GODKAND-BILD                                             
163100           PERFORM M-INIT-TRANS-LL92                                      
163200         WHEN OTHER                                                       
163300           PERFORM N-INIT-TRANS-LL8                                       
163400         END-EVALUATE                                                     
163500                                                                          
163600         IF WS-INDATA-RATT                                                
163700           PERFORM H-AVSLUT                                               
163800         ELSE                                                             
163900           PERFORM O-INIT-FEL-TRANS                                       
164000         END-IF                                                           
164100                                                                          
164200         PERFORM P-EVALUATE-INSERT-MSG-TRANS                              
164300     END-IF                                                               
164400                                                                          
164500     MOVE ZERO TO RETURN-CODE                                             
164600     GOBACK                                                               
164700     .                                                                    
164800     EJECT                                                                
164900 A-INIT             SECTION.                                              
165000                                                                          
165100     SKIP3                                                                
165200     IF MSG-DUBBLA-TRANSKODER                                             
165300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I31501               
165400       MOVE MSG-IDTRANS-2                 TO   MFS-IDTRANS                
165500                                               WS-IDTRANS                 
165600       MOVE MSG-KDMFSFOR-2                TO   MFS-KDMFSFOR               
165700                                               WS-KDMFSFOR                
165800       MOVE MSG-KDTRTYP                   TO   MFS-KDTRTYP                
165900     ELSE                                                                 
166000       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I31501               
166100       MOVE MSG-IDTRANS-1                 TO   MFS-IDTRANS                
166200                                               WS-IDTRANS                 
166300       MOVE MSG-KDMFSFOR-1                TO   MFS-KDMFSFOR               
166400                                               WS-KDMFSFOR                
166500       MOVE ' '                           TO   MFS-KDTRTYP                
166600     END-IF                                                               
166700*                                                                         
166800     MOVE LOW-VALUE                       TO   MSG-AREA                   
166900                                               MOD-W4O31501               
167000     MOVE 'W4O315N1'                      TO   MFS-IDMOD                  
167100     MOVE '4315'                          TO   MOD-IDTRANS                
167200                                               MOD-IDTRANS-START          
167300*                                                                         
167400     IF ENGLISH-TEXT                                                      
167500         MOVE +2                          TO   INDX                       
167600     ELSE                                                                 
167700         MOVE +1                          TO   INDX                       
167800     END-IF                                                               
167900     MOVE RAETT                           TO WS-INDATA-TEST               
168000                                             WS-BEHANDLING-TEST           
168100     PERFORM AA-FLYTTA-NYCKLAR                                            
168200     MOVE NEJ                   TO DIRLEV-KOLLI-SW                        
168300                                   NYA-NYCKLAR-SW                         
168400     INITIALIZE TMS-W403TMS1                                              
168500     IF WS-IDTRANS = '4313'                                               
168600       MOVE SPACE               TO MOD-IDDISTR-UT                         
168700                                   MOD-IDKUNDNR-UT                        
168800                                   MOD-IDORDNR-UT                         
168900                                   MOD-IDPRODNR-UT                        
169000                                   MOD-IDKOLLI-UT                         
169100                                   MOD-IDANSTNR-UT                        
169200     END-IF                                                               
169300                                                                          
169400     IF NOT WS-ORDERVIS-TRANS                                             
169500       IF WS-IDTRANS = '4312' OR                                          
169600          WS-IDTRANS = '4313' OR                                          
169700          WS-IDTRANS = '4314' OR                                          
169800          WS-IDTRANS = '4316' OR                                          
169900          WS-IDTRANS = '4317' OR                                          
170000          WS-IDTRANS = '4318'                                             
170100         PERFORM S02-RENSA-MOD-FALT                                       
170200       END-IF                                                             
170300       IF NOT WS-GODKAND-BILD                                             
170400         PERFORM S02-RENSA-MOD-FALT                                       
170500         MOVE SPACE               TO MOD-IDDISTR-UT                       
170600                                     MOD-IDKUNDNR-UT                      
170700                                     MOD-IDORDNR-UT                       
170800                                     MOD-IDPRODNR-UT                      
170900                                     MOD-IDKOLLI-UT                       
171000                                     MOD-IDANSTNR-UT                      
171100       END-IF                                                             
171200     END-IF                                                               
171300                                                                          
171400     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
171500     ACCEPT WS-TIDPUNKT                   FROM TIME                       
171600                                                                          
171700*    DISPLAY ' W4031500 '                                                 
171800*    DISPLAY ' WS-DAGENS-DATUM ' WS-DAGENS-DATUM                          
171900*    DISPLAY ' WS-TIDPUNKT     ' WS-TIDPUNKT                              
172000                                                                          
172100     MOVE WS-IDDC TO W-IDDC-B6                                            
172200                     W-IDDC                                               
172300     PERFORM IMS-GU-WDB601                                                
172400     IF NOT WS-ORDERVIS-TRANS                                             
172500       IF DCS-NDC-NA OR DCS-NDC-PF OR                                     
172600         (DCS-SDC AND DCS-IDLANDX2 = 'GB')                                
172700         MOVE ALL '+'           TO MSGI-WMSGINIT                          
172800         MOVE '011'             TO MSGI-KDCALL                            
172900         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
173000                                   MSGI-IDLTERM-USER                      
173100         MOVE WS-DAGENS-DATUM   TO MSGI-TILOKDAT                          
173200         MOVE WS-TIDPUNKT       TO MSGI-TILOKTID                          
173300         MOVE WS-TIDPUNKT (1:6) TO WS-TISKPTID                            
173400         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
173500         MOVE MSGI-TILOKDAT     TO WS-DAGENS-DATUM                        
173600         MOVE MSGI-TILOKTID     TO WS-TIDPUNKT (1:4)                      
173700*        MOVE MSGI-KDMFSFOR     TO WS-KDMFSFOR                            
173800       END-IF                                                             
173900     END-IF                                                               
174000                                                                          
174100     MOVE MFS-RENSA-FAELT                 TO   MOD-TEMFSFEL               
174200                                               MOD-IDANSTNR-IN            
174300                                               MOD-IDDISTR-IN             
174400                                               MOD-IDKUNDNR-IN            
174500                                               MOD-IDORDNR-IN             
174600                                               MOD-IDKOLLI-IN             
174700                                               MOD-IDPRODNR-IN            
174800                                               MOD-TEMFSINF               
174900     MOVE ZERO                      TO PALLAR (1)                         
175000                                       PALLAR (2)                         
175100                                       PALLAR (3)                         
175200                                       PALLAR (4)                         
175300                                       PALLAR (5)                         
175400                                                                          
175500     MOVE ZERO                      TO KRAGAR (1)                         
175600                                       KRAGAR (2)                         
175700                                       KRAGAR (3)                         
175800                                       KRAGAR (4)                         
175900                                       KRAGAR (5)                         
176000                                                                          
176100     MOVE ZERO                      TO EMB-LOCK (1)                       
176200                                       EMB-LOCK (2)                       
176300                                       EMB-LOCK (3)                       
176400                                       EMB-LOCK (4)                       
176500                                       EMB-LOCK (5)                       
176600                                       LOGG-IDLOGLOP                      
176700                                                                          
176800     IF SDC-NL OR SDC-ES OR SDC-IT OR SDC-AT                              
176900     OR NDC-US OR NDC-CA                                                  
177000        MOVE MFS-RENSA-FAELT          TO MOD-PRTVAL-FOLJEFL               
177100        MOVE MFS-CLOSE-FIELD-NOMOD    TO MOD-PRTVAL-FOLJEFL-ATTR          
177200     END-IF                                                               
177300     .                                                                    
177400     EJECT                                                                
177500 AA-FLYTTA-NYCKLAR  SECTION.                                              
177600                                                                          
177700     IF NOT WS-ORDERVIS-TRANS                                             
177800       MOVE ALL '+'           TO MSGI-WMSGINIT                            
177900       MOVE '001'             TO MSGI-KDCALL                              
178000       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
178100       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
178200       MOVE '4315'            TO MSGI-IDTRANS                             
178300                                                                          
178400       IF WS-SAMMA-BILD                                                   
178500         MOVE MID-PRTVAL-FOLJEFL     TO MSGI-KDPRTVAL-FS                  
178600         MOVE MID-PRTVAL-ADRESSFL    TO MSGI-KDPRTVAL-ADR                 
178700       END-IF                                                             
178800                                                                          
178900       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
179000                                                                          
179100       MOVE MSGI-KDMATT       TO WS-KDMATT                                
179200     END-IF                                                               
179300                                                                          
179400     IF MID-IDANSTNR-IN = ALL '+'                                         
179500         MOVE MID-IDANSTNR-UT             TO   WS-IDANSTNR                
179600         INSPECT WS-IDANSTNR REPLACING LEADING SPACE BY ZERO              
179700     ELSE                                                                 
179800*        MOVE JA                          TO   NYA-NYCKLAR-SW             
179900         MOVE MID-IDANSTNR-IN             TO   WS-IDANSTNR                
180000     END-IF                                                               
180100     SKIP2                                                                
180200     IF MID-IDDISTR-IN = ALL '+'                                          
180300         MOVE MID-IDDISTR-UT              TO   WS-IDDISTR                 
180400         INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO               
180500     ELSE                                                                 
180600         MOVE JA                          TO   NYA-NYCKLAR-SW             
180700         MOVE MID-IDDISTR-IN              TO   WS-IDDISTR                 
180800     END-IF                                                               
180900                                                                          
181000     IF MID-IDKUNDNR-IN = ALL '+'                                         
181100         MOVE MID-IDKUNDNR-UT             TO   WS-IDKUNDNR                
181200         INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
181300     ELSE                                                                 
181400         MOVE JA                          TO   NYA-NYCKLAR-SW             
181500         MOVE MID-IDKUNDNR-IN             TO   WS-IDKUNDNR                
181600     END-IF                                                               
181700                                                                          
181800     IF MID-IDORDNR-IN = ALL '+'                                          
181900         MOVE MID-IDORDNR-UT              TO   WS-IDORDNR                 
182000         INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO               
182100     ELSE                                                                 
182200         MOVE JA                          TO   NYA-NYCKLAR-SW             
182300         MOVE MID-IDORDNR-IN              TO   WS-IDORDNR                 
182400     END-IF                                                               
182500                                                                          
182600     IF MID-IDKOLLI-IN = ALL '+'                                          
182700         IF MID-IDKOLLI-FOM = ALL '+'                                     
182800             MOVE MID-IDKOLLI-UT          TO   WS-IDKOLLI                 
182900         ELSE                                                             
183000             MOVE ZERO                    TO   WS-IDKOLLI                 
183100         END-IF                                                           
183200         INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO               
183300     ELSE                                                                 
183400         MOVE MID-IDKOLLI-IN              TO   WS-IDKOLLI                 
183500     END-IF                                                               
183600                                                                          
183700     IF MID-IDPRODNR-IN = ALL '+'                                         
183800       IF NYA-NYCKLAR                                                     
183900         MOVE ZERO                        TO   WS-IDPRODNR                
184000         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
184100       ELSE                                                               
184200         MOVE MID-IDPRODNR-UT             TO   WS-IDPRODNR                
184300         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
184400       END-IF                                                             
184500     ELSE                                                                 
184600         MOVE MID-IDPRODNR-IN             TO   WS-IDPRODNR                
184700     END-IF                                                               
184800                                                                          
184900     IF WS-ORDERVIS-TRANS                                                 
185000       MOVE MID-IDDC-IN                   TO WS-IDDC                      
185100     ELSE                                                                 
185200       MOVE MSGI-IDDC                     TO WS-IDDC                      
185300     END-IF                                                               
185400                                                                          
185500     MOVE MSGI-KDPRTVAL-FS                TO WS-KDPRTVAL-FS               
185600     MOVE MSGI-KDPRTVAL-ADR               TO WS-KDPRTVAL-ADR              
185700                                                                          
185800     IF WS-IDDC IS > SPACE                                                
185900       CONTINUE                                                           
186000     ELSE                                                                 
186100       MOVE FEL                           TO WS-INDATA-TEST               
186200       MOVE FEL-795 (INDX)                TO MOD-TEMFSFEL                 
186300     END-IF                                                               
186400                                                                          
186500     MOVE WS-IDANSTNR                     TO   MOD-IDANSTNR-UT            
186600     INSPECT MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE              
186700     MOVE WS-IDDISTR                      TO   MOD-IDDISTR-UT             
186800     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
186900     MOVE WS-IDKUNDNR                     TO   MOD-IDKUNDNR-UT            
187000     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
187100     MOVE WS-IDORDNR                      TO   MOD-IDORDNR-UT             
187200     INSPECT MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE              
187300     MOVE WS-IDKOLLI                      TO   MOD-IDKOLLI-UT             
187400     INSPECT MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE              
187500     MOVE WS-IDPRODNR                     TO   MOD-IDPRODNR-UT            
187600     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
187700     MOVE WS-IDDC                         TO   MOD-IDDC-UT                
187800     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
187900                                                                          
188000     MOVE WS-KDPRTVAL-FS                  TO   MOD-PRTVAL-FOLJEFL         
188100     MOVE WS-KDPRTVAL-ADR                 TO   MOD-PRTVAL-ADRESSFL        
188200     .                                                                    
188300     EJECT                                                                
188400 B-GENERELL-KONTROLL  SECTION.                                            
188500                                                                          
188600     IF WS-IDDC NOT = W-IDDC-B6                                           
188700        MOVE WS-IDDC TO W-IDDC-B6                                         
188800        PERFORM IMS-GU-WDB601                                             
188900     END-IF                                                               
189000                                                                          
189100     IF WS-IDANSTNR NOT NUMERIC                                           
189200         MOVE FEL                       TO   WS-INDATA-TEST               
189300         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
189400     END-IF                                                               
189500                                                                          
189600     IF WS-IDDISTR  NOT NUMERIC                                           
189700         MOVE FEL                       TO   WS-INDATA-TEST               
189800         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
189900     ELSE                                                                 
190000         MOVE WS-IDDISTR                TO   WS-IDDISTR-NUM               
190100         MOVE WS-IDDISTR-NUM            TO   TEST-IDDISTR                 
190200* * * SATSORDER EJ TILLÅTNA ANNAT ÄN FRÅN BILD 4303 * * *                 
190300         IF DIST19-SATS AND NOT WS-ORDERVIS-TRANS                         
190400           MOVE FEL                     TO   WS-INDATA-TEST               
190500           MOVE FEL-749 (INDX)          TO   MOD-TEMFSFEL                 
190600         ELSE                                                             
190700           MOVE WS-IDDISTR              TO   W-4A1-IDDISTR                
190800         END-IF                                                           
190900* * * * * * * * * * * * * * * * *                                         
191000     END-IF                                                               
191100                                                                          
191200     IF WS-IDKUNDNR NOT NUMERIC                                           
191300         MOVE FEL                       TO   WS-INDATA-TEST               
191400         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
191500     ELSE                                                                 
191600         MOVE WS-IDKUNDNR               TO   W-4A1-IDKUNDNR               
191700     END-IF                                                               
191800                                                                          
191900     IF WS-IDORDNR  NOT NUMERIC                                           
192000         MOVE FEL                       TO   WS-INDATA-TEST               
192100         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
192200     ELSE                                                                 
192300         MOVE WS-IDORDNR                TO   W-4A1-IDORDNR                
192400     END-IF                                                               
192500                                                                          
192600     IF WS-IDKOLLI NOT NUMERIC                                            
192700         MOVE FEL                       TO   WS-INDATA-TEST               
192800         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
192900     ELSE                                                                 
193000         PERFORM BA-KONTROLLERA-IDKOLLI                                   
193100     END-IF                                                               
193200                                                                          
193300     IF WS-IDPRODNR NOT NUMERIC                                           
193400         MOVE FEL                       TO   WS-INDATA-TEST               
193500         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
193600     END-IF                                                               
193700                                                                          
193800     IF  WS-ORDERVIS-TRANS                                                
193900     AND MID-KDKOLLI        = ALL '+'                                     
194000     AND MID-KDEMBTYP       = ALL '+'                                     
194100     AND MID-DIKOLLIL       = ALL '+'                                     
194200     AND MID-DIKOLLIB       = ALL '+'                                     
194300     AND MID-DIKOLLIH       = ALL '+'                                     
194400     AND MID-VKORDBTO-KOLLI = ALL '+'                                     
194500         PERFORM BE-INIT-KOLLI                                            
194600     ELSE                                                                 
194700         PERFORM BB-KONTROLLERA-KOLLIKOD                                  
194800     END-IF                                                               
194900                                                                          
195000     MOVE +1                        TO INX                                
195100     MOVE ZERO                      TO RAD-INX                            
195200     PERFORM UNTIL INX NOT < MAX-RAD-ANTAL-PLUS-1                         
195300         MOVE MFS-ROER-EJ-FAELT     TO MOD-IDRADNR-FOM (INX)              
195400                                       MOD-IDRADNR-TOM (INX)              
195500                                       MOD-KVLEVART (INX)                 
195600         PERFORM BD-KONTROLLERA-RAD                                       
195700         ADD +1                     TO INX                                
195800     END-PERFORM                                                          
195900     COMPUTE MAX-RAD-ANTAL-PLUS-1 = RAD-INX + 1                           
196000                                                                          
196100     IF WS-INDATA-RATT                                                    
196200        IF  WS-ANT-RAD-I-BILD > WS-MAX-ANT-RAD-I-BILD                     
196300            MOVE FEL                      TO WS-INDATA-TEST               
196400            MOVE FEL-826 (INDX)           TO MOD-TEMFSFEL                 
196500        END-IF                                                            
196600     END-IF                                                               
196700                                                                          
196800     IF WS-ORDERVIS-TRANS                                                 
196900        IF MID-FLSISTAK    = ALL '+'                                      
197000            MOVE MFS-RENSA-FAELT          TO  MOD-FLSISTAK                
197100        ELSE                                                              
197200            MOVE MFS-ROER-EJ-FAELT        TO  MOD-FLSISTAK                
197300            IF  MID-FLSISTAK = 'J' OR 'N' OR 'Y'                          
197400                MOVE MFS-ALFA-FAELT-RAETT TO  MOD-FLSISTAK-ATTR           
197500            ELSE                                                          
197600              IF WS-INDATA-RATT                                           
197700                MOVE FEL                  TO  WS-INDATA-TEST              
197800                IF WS-ORDERVIS-TRANS                                      
197900                  MOVE '***POS1'          TO  MOD-TEMFSFEL                
198000                ELSE                                                      
198100                  MOVE FEL-748 (INDX)     TO  MOD-TEMFSFEL                
198200                END-IF                                                    
198300                MOVE MFS-ALFA-FAELT-FEL   TO  MOD-FLSISTAK-ATTR           
198400              END-IF                                                      
198500            END-IF                                                        
198600        END-IF                                                            
198700     ELSE                                                                 
198800        MOVE MFS-ROER-EJ-FAELT        TO  MOD-FLSISTAK                    
198900        IF  MID-FLSISTAK = 'J' OR 'N' OR 'Y'                              
199000            MOVE MFS-ALFA-FAELT-RAETT TO  MOD-FLSISTAK-ATTR               
199100        ELSE                                                              
199200          IF WS-INDATA-RATT                                               
199300            MOVE FEL                  TO  WS-INDATA-TEST                  
199400           IF WS-ORDERVIS-TRANS                                           
199500             MOVE '***POS2'           TO  MOD-TEMFSFEL                    
199600           ELSE                                                           
199700             MOVE FEL-748 (INDX)      TO  MOD-TEMFSFEL                    
199800           END-IF                                                         
199900            MOVE MFS-ALFA-FAELT-FEL   TO  MOD-FLSISTAK-ATTR               
200000          END-IF                                                          
200100        END-IF                                                            
200200     END-IF                                                               
200300                                                                          
200400     IF MID-VKORDBTO-KOLLI = ALL '+'                                      
200500         MOVE MFS-RENSA-FAELT             TO  MOD-VKORDBTO-KOLLI          
200600     ELSE                                                                 
200700         MOVE MFS-ROER-EJ-FAELT           TO  MOD-VKORDBTO-KOLLI          
200800         MOVE MFS-NUM-FAELT-RAETT         TO  MOD-VKORDBTO-ATTR           
200900     END-IF                                                               
201000                                                                          
201100     IF MID-ADFLGEO     = ALL '+'                                         
201200         MOVE MFS-RENSA-FAELT             TO  MOD-ADFLGEO                 
201300     ELSE                                                                 
201400         MOVE MFS-ROER-EJ-FAELT           TO  MOD-ADFLGEO                 
201500         MOVE MFS-ALFA-FAELT-RAETT        TO  MOD-ADFLGEO-ATTR            
201600     END-IF                                                               
201700                                                                          
201800     IF MID-ADFLOMR     = ALL '+'                                         
201900         MOVE MFS-RENSA-FAELT             TO  MOD-ADFLOMR                 
202000         MOVE MFS-NUM-FAELT-RAETT         TO  MOD-ADFLOMR-ATTR            
202100     ELSE                                                                 
202200         IF  MID-ADFLOMR NUMERIC                                          
202300             MOVE MFS-ROER-EJ-FAELT       TO  MOD-ADFLOMR                 
202400             MOVE MFS-NUM-FAELT-RAETT     TO  MOD-ADFLOMR-ATTR            
202500         ELSE                                                             
202600           IF WS-INDATA-RATT                                              
202700             MOVE MFS-ROER-EJ-FAELT       TO  MOD-ADFLOMR                 
202800             MOVE MFS-NUM-FAELT-FEL       TO  MOD-ADFLOMR-ATTR            
202900             MOVE FEL                     TO  WS-INDATA-TEST              
203000             IF WS-ORDERVIS-TRANS                                         
203100               MOVE '***POS3'             TO  MOD-TEMFSFEL                
203200             ELSE                                                         
203300               MOVE FEL-748 (INDX)        TO  MOD-TEMFSFEL                
203400             END-IF                                                       
203500           END-IF                                                         
203600         END-IF                                                           
203700     END-IF                                                               
203800                                                                          
203900     IF MID-ADRUTNIV    = ALL '+'                                         
204000         MOVE MFS-RENSA-FAELT             TO  MOD-ADRUTNIV                
204100         MOVE MFS-NUM-FAELT-RAETT         TO  MOD-ADRUTNIV-ATTR           
204200     ELSE                                                                 
204300         IF  MID-ADRUTNIV NUMERIC                                         
204400             MOVE MFS-ROER-EJ-FAELT       TO  MOD-ADRUTNIV                
204500             MOVE MFS-NUM-FAELT-RAETT     TO  MOD-ADRUTNIV-ATTR           
204600         ELSE                                                             
204700           IF WS-INDATA-RATT                                              
204800             MOVE MFS-ROER-EJ-FAELT       TO  MOD-ADRUTNIV                
204900             MOVE MFS-NUM-FAELT-FEL       TO  MOD-ADRUTNIV-ATTR           
205000             MOVE FEL                     TO  WS-INDATA-TEST              
205100             IF WS-ORDERVIS-TRANS                                         
205200               MOVE '***POS4'             TO  MOD-TEMFSFEL                
205300             ELSE                                                         
205400               MOVE FEL-748 (INDX)        TO  MOD-TEMFSFEL                
205500             END-IF                                                       
205600           END-IF                                                         
205700         END-IF                                                           
205800     END-IF                                                               
205900                                                                          
206000*KOLLIFLAGGA                                                              
206100     IF WS-KDPRTVAL-ADR = ALL '+'                                         
206200       IF (DCS-SDC AND (DCS-IDLANDX2 = 'IT' OR 'ES'))                     
206300       OR (DCS-NDC-NA AND DCS-IDLANDX2 = 'CA')                            
206400         IF WS-INDATA-RATT                                                
206500           MOVE FEL                 TO WS-INDATA-TEST                     
206600           MOVE FEL-772 (INDX)      TO MOD-TEMFSFEL                       
206700           MOVE MFS-ALFA-FAELT-FEL  TO MOD-PRTVAL-ADRESSFL-ATTR           
206800         END-IF                                                           
206900       ELSE                                                               
207000         IF LDC-GB-3A                                                     
207100         OR LDC-GB-2C                                                     
207200           MOVE MFS-RENSA-FAELT       TO MOD-PRTVAL-ADRESSFL              
207300           MOVE MFS-CLOSE-FIELD-NOMOD TO MOD-PRTVAL-ADRESSFL-ATTR         
207400         ELSE                                                             
207500           MOVE 'UU'                  TO WS-PRT-KDSVAR-ADRESSFL           
207600                                         MOD-PRTVAL-ADRESSFL              
207700           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-PRTVAL-ADRESSFL-ATTR         
207800         END-IF                                                           
207900       END-IF                                                             
208000     ELSE                                                                 
208100       IF LDC-GB-3A                                                       
208200         OR LDC-GB-2C                                                     
208300         MOVE MFS-RENSA-FAELT         TO MOD-PRTVAL-ADRESSFL              
208400         MOVE MFS-CLOSE-FIELD-NOMOD   TO MOD-PRTVAL-ADRESSFL-ATTR         
208500       ELSE                                                               
208600         IF MID-PRTVAL-ADRESSFL = 'U '                                    
208700         OR MID-PRTVAL-ADRESSFL = 'UU'                                    
208800           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-PRTVAL-ADRESSFL-ATTR         
208900         ELSE                                                             
209000                                                                          
209100           MOVE '4'              TO WS-SYSTDEL                            
209200           MOVE 'KF'             TO WS-LISTTYP                            
209300           MOVE WS-IDDC          TO WS-DC                                 
209400           MOVE WS-KDPRTVAL-ADR  TO WS-KDPRT                              
209500                                                                          
209600           MOVE 001              TO PRT-KDCALL                            
209700           MOVE WS-IDPRTLST      TO PRT-IDPRTLST                          
209800                                                                          
209900           CALL W006PRT USING PRT-W006PRT                                 
210000                                                                          
210100           IF PRT-KDSVAR = RAETT                                          
210200             MOVE PRT-KDSVAR           TO WS-PRT-KDSVAR-ADRESSFL          
210300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-PRTVAL-ADRESSFL-ATTR        
210400           ELSE                                                           
210500             IF WS-INDATA-RATT                                            
210600               MOVE FEL                TO WS-INDATA-TEST                  
210700               MOVE FEL-772 (INDX)     TO MOD-TEMFSFEL                    
210800             END-IF                                                       
210900             MOVE MFS-ALFA-FAELT-FEL   TO MOD-PRTVAL-ADRESSFL-ATTR        
211000           END-IF                                                         
211100         END-IF                                                           
211200       END-IF                                                             
211300       MOVE MFS-ROER-EJ-FAELT          TO MOD-PRTVAL-ADRESSFL             
211400     END-IF                                                               
211500                                                                          
211600*FOLJESSEDEL                                                              
211700     IF WS-KDPRTVAL-FS = ALL '+'                                          
211800       IF SDC-ES OR SDC-IT OR SDC-AT                                      
211900       OR NDC-US OR NDC-CA                                                
212000          MOVE MFS-RENSA-FAELT        TO MOD-PRTVAL-FOLJEFL               
212100          MOVE MFS-CLOSE-FIELD-NOMOD  TO MOD-PRTVAL-FOLJEFL-ATTR          
212200       ELSE                                                               
212300          MOVE MFS-ALFA-FAELT-RAETT   TO MOD-PRTVAL-FOLJEFL-ATTR          
212400          MOVE 'UU'                   TO WS-PRT-KDSVAR-FOLJEFL            
212500                                         MOD-PRTVAL-FOLJEFL               
212600       END-IF                                                             
212700     ELSE                                                                 
212800                                                                          
212900       IF SDC-ES OR SDC-IT OR SDC-AT                                      
213000       OR NDC-US OR NDC-CA                                                
213100         MOVE MFS-RENSA-FAELT         TO MOD-PRTVAL-FOLJEFL               
213200         MOVE MFS-CLOSE-FIELD-NOMOD   TO MOD-PRTVAL-FOLJEFL-ATTR          
213300       ELSE                                                               
213400         IF MID-PRTVAL-FOLJEFL = 'U '                                     
213500         OR MID-PRTVAL-FOLJEFL = 'UU'                                     
213600           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-PRTVAL-FOLJEFL-ATTR          
213700         ELSE                                                             
213800           MOVE '4'              TO WS-SYSTDEL                            
213900           MOVE 'FS'             TO WS-LISTTYP                            
214000           MOVE WS-KDPRTVAL-FS   TO WS-KDPRT                              
214100           MOVE WS-IDDC          TO WS-DC                                 
214200                                                                          
214300           MOVE 001              TO PRT-KDCALL                            
214400           MOVE WS-IDPRTLST      TO PRT-IDPRTLST                          
214500                                                                          
214600           CALL W006PRT USING PRT-W006PRT                                 
214700                                                                          
214800           IF PRT-KDSVAR = RAETT                                          
214900             MOVE PRT-KDSVAR           TO WS-PRT-KDSVAR-FOLJEFL           
215000             MOVE MFS-ALFA-FAELT-RAETT TO MOD-PRTVAL-FOLJEFL-ATTR         
215100           ELSE                                                           
215200             IF WS-INDATA-RATT                                            
215300               MOVE FEL                TO WS-INDATA-TEST                  
215400               MOVE FEL-772 (INDX)     TO MOD-TEMFSFEL                    
215500             END-IF                                                       
215600             MOVE MFS-ALFA-FAELT-FEL   TO MOD-PRTVAL-FOLJEFL-ATTR         
215700           END-IF                                                         
215800         END-IF                                                           
215900       END-IF                                                             
216000       MOVE MFS-ROER-EJ-FAELT          TO MOD-PRTVAL-FOLJEFL              
216100     END-IF                                                               
216200                                                                          
216300     .                                                                    
216400     EJECT                                                                
216500 BA-KONTROLLERA-IDKOLLI SECTION.                                          
216600                                                                          
216700     IF MID-IDKOLLI-FOM = ALL '+'                                         
216800         MOVE MFS-RENSA-FAELT           TO MOD-IDKOLLI-FOM                
216900         IF WS-IDKOLLI = ZERO                                             
217000             MOVE MFS-RENSA-FAELT       TO MOD-IDKOLLI-TOM                
217100             MOVE FEL                   TO WS-INDATA-TEST                 
217200             MOVE FEL-758 (INDX)        TO MOD-TEMFSFEL                   
217300         ELSE                                                             
217400             IF MID-IDKOLLI-TOM NOT = ALL '+'                             
217500                 MOVE MFS-ROER-EJ-FAELT TO MOD-IDKOLLI-TOM                
217600                 MOVE FEL               TO WS-INDATA-TEST                 
217700                 MOVE FEL-731 (INDX)    TO MOD-TEMFSFEL                   
217800                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-TOM-ATTR           
217900             ELSE                                                         
218000                 MOVE MFS-RENSA-FAELT   TO MOD-IDKOLLI-TOM                
218100             END-IF                                                       
218200         END-IF                                                           
218300     ELSE                                                                 
218400         MOVE MFS-ROER-EJ-FAELT         TO MOD-IDKOLLI-FOM                
218500                                           MOD-IDKOLLI-TOM                
218600         IF MID-IDKOLLI-IN NOT = ALL '+'                                  
218700             MOVE FEL                   TO WS-INDATA-TEST                 
218800             MOVE FEL-731 (INDX)        TO MOD-TEMFSFEL                   
218900             MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKOLLI-FOM-ATTR           
219000                                           MOD-IDKOLLI-TOM-ATTR           
219100         ELSE                                                             
219200             EVALUATE TRUE                                                
219300             WHEN MID-IDKOLLI-FOM NOT NUMERIC                             
219400                 MOVE FEL               TO WS-INDATA-TEST                 
219500                 IF WS-ORDERVIS-TRANS                                     
219600                   MOVE '***POS6'       TO  MOD-TEMFSFEL                  
219700                 ELSE                                                     
219800                   MOVE FEL-748 (INDX)  TO MOD-TEMFSFEL                   
219900                 END-IF                                                   
220000                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-FOM-ATTR           
220100             WHEN MID-IDKOLLI-TOM = ALL '+'                               
220200                 MOVE FEL               TO WS-INDATA-TEST                 
220300                 MOVE FEL-731 (INDX)    TO MOD-TEMFSFEL                   
220400                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-TOM-ATTR           
220500                 MOVE MFS-NUM-FAELT-RAETT TO  MOD-IDKOLLI-FOM-ATTR        
220600             WHEN MID-IDKOLLI-TOM NOT NUMERIC                             
220700                 MOVE FEL               TO WS-INDATA-TEST                 
220800                 IF WS-ORDERVIS-TRANS                                     
220900                   MOVE '***POS7'       TO  MOD-TEMFSFEL                  
221000                 ELSE                                                     
221100                   MOVE FEL-748 (INDX)  TO MOD-TEMFSFEL                   
221200                 END-IF                                                   
221300                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-TOM-ATTR           
221400             WHEN MID-IDKOLLI-TOM < MID-IDKOLLI-FOM OR                    
221500                    MID-IDKOLLI-TOM = MID-IDKOLLI-FOM                     
221600                 MOVE FEL               TO WS-INDATA-TEST                 
221700                 MOVE FEL-732 (INDX)    TO MOD-TEMFSFEL                   
221800                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-TOM-ATTR           
221900                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-FOM-ATTR           
222000             WHEN OTHER                                                   
222100                 MOVE MID-IDKOLLI-FOM   TO WS-IDKOLLI-FOM                 
222200                 MOVE MID-IDKOLLI-TOM   TO WS-IDKOLLI-TOM                 
222300                                                                          
222400               IF (WS-IDKOLLI-TOM - WS-IDKOLLI-FOM + 1 > 100 AND          
222500                  MID-PRTVAL-ADRESSFL NOT = 'UU') OR                      
222600                  (WS-IDKOLLI-TOM - WS-IDKOLLI-FOM + 1 > 100 AND          
222700                  MID-PRTVAL-FOLJEFL  NOT = 'UU')                         
222800                                                                          
222900                 MOVE FEL               TO WS-INDATA-TEST                 
223000                 MOVE FEL-773 (INDX)    TO MOD-TEMFSFEL                   
223100                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-TOM-ATTR           
223200                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-FOM-ATTR           
223300               ELSE                                                       
223400                                                                          
223500                 IF (WS-IDKOLLI-TOM - WS-IDKOLLI-FOM + 1 > 50 AND         
223600                    WS-KDPRTVAL-ADR NOT = 'UU') AND                       
223700                    (WS-IDKOLLI-TOM - WS-IDKOLLI-FOM + 1 > 50 AND         
223800                    WS-KDPRTVAL-FS     NOT = 'UU')                        
223900                                                                          
224000                   MOVE FEL               TO WS-INDATA-TEST               
224100                   MOVE FEL-796 (INDX)    TO MOD-TEMFSFEL                 
224200                   MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-TOM-ATTR         
224300                   MOVE MFS-NUM-FAELT-FEL TO MOD-IDKOLLI-FOM-ATTR         
224400                 ELSE                                                     
224500                   MOVE JA                  TO FL-KOLLI-INTERVALL         
224600                   MOVE MFS-NUM-FAELT-RAETT TO                            
224700                        MOD-IDKOLLI-FOM-ATTR                              
224800                        MOD-IDKOLLI-TOM-ATTR                              
224900                 END-IF                                                   
225000               END-IF                                                     
225100             END-EVALUATE                                                 
225200         END-IF                                                           
225300     END-IF                                                               
225400     .                                                                    
225500     EJECT                                                                
225600 BB-KONTROLLERA-KOLLIKOD    SECTION.                                      
225700                                                                          
225800     IF MID-KDKOLLI NOT = ALL '+'                                         
225900         MOVE MID-KDKOLLI               TO WS-KDKOLLI                     
226000         MOVE MFS-ROER-EJ-FAELT         TO MOD-KDKOLLI                    
226100         MOVE MFS-ALFA-FAELT-RAETT      TO MOD-KDKOLLI-ATTR               
226200     ELSE                                                                 
226300         MOVE MFS-RENSA-FAELT           TO MOD-KDKOLLI                    
226400         MOVE SPACE                     TO WS-KDKOLLI                     
226500         MOVE MFS-ALFA-FAELT-RAETT      TO MOD-KDKOLLI-ATTR               
226600*----------------------------------------OM EJ KOLLIKOD ANGIVEN           
226700*----------------------------------------SKALL EMBTYP,LÄNGD,HÖJD          
226800*----------------------------------------OCH BREDD ANGES                  
226900         IF  WS-IDDISTR NUMERIC                                           
227000         AND WS-IDDISTR < '0800'                                          
227100         AND WS-ORDERVIS-TRANS                                            
227200         AND WS-IDKOLLI NUMERIC                                           
227300         AND WS-IDKOLLI > '99000'                                         
227400             IF  MID-KDEMBTYP = ALL '+'                                   
227500                 MOVE FEL                   TO WS-INDATA-TEST             
227600                 MOVE FEL-728 (INDX)        TO MOD-TEMFSFEL               
227700                 MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDKOLLI-ATTR           
227800             END-IF                                                       
227900         ELSE                                                             
228000             IF  MID-KDEMBTYP = ALL '+'                                   
228100             OR  MID-DIKOLLIL = ALL '+'                                   
228200             OR  MID-DIKOLLIB = ALL '+'                                   
228300             OR  MID-DIKOLLIH = ALL '+'                                   
228400                 MOVE FEL                   TO WS-INDATA-TEST             
228500                 MOVE FEL-728 (INDX)        TO MOD-TEMFSFEL               
228600                 MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDKOLLI-ATTR           
228700             END-IF                                                       
228800         END-IF                                                           
228900     END-IF                                                               
229000     SKIP2                                                                
229100     PERFORM BBA-KONTROLLERA-UPPG                                         
229200     .                                                                    
229300     EJECT                                                                
229400 BBA-KONTROLLERA-UPPG   SECTION.                                          
229500                                                                          
229600     IF MID-KDEMBTYP NOT = ALL '+'                                        
229700                                                                          
229800         MOVE MFS-ROER-EJ-FAELT         TO MOD-KDEMBTYP                   
229900         IF MID-KDEMBTYP NUMERIC                                          
230000             MOVE MID-KDEMBTYP          TO WS-KDEMBTYP                    
230100             MOVE MFS-NUM-FAELT-RAETT   TO MOD-KDEMBTYP-ATTR              
230200             IF MID-VKORDBTO-KOLLI = ALL '+'                              
230300             AND WS-INDATA-RATT                                           
230400                MOVE FEL                TO WS-INDATA-TEST                 
230500                MOVE MFS-ADD-SET-CURSOR TO MOD-VKORDBTO-ATTR              
230600                MOVE ENTER-GROSS-WEIGHT TO MOD-TEMFSINF                   
230700             END-IF                                                       
230800         ELSE                                                             
230900           IF WS-INDATA-RATT                                              
231000             MOVE FEL                   TO WS-INDATA-TEST                 
231100             MOVE MFS-NUM-FAELT-FEL     TO MOD-KDEMBTYP-ATTR              
231200             IF WS-ORDERVIS-TRANS                                         
231300               MOVE '***POS5'        TO  MOD-TEMFSFEL                     
231400             ELSE                                                         
231500                 IF WS-ORDERVIS-TRANS                                     
231600                   MOVE '***POS8'       TO  MOD-TEMFSFEL                  
231700                 ELSE                                                     
231800                   MOVE FEL-748 (INDX)  TO MOD-TEMFSFEL                   
231900                 END-IF                                                   
232000             END-IF                                                       
232100           END-IF                                                         
232200         END-IF                                                           
232300     ELSE                                                                 
232400         MOVE MFS-ROER-EJ-FAELT         TO MOD-KDEMBTYP                   
232500         MOVE ZERO                      TO WS-KDEMBTYP                    
232600     END-IF                                                               
232700                                                                          
232800     IF MID-DIKOLLIL NOT = ALL '+'                                        
232900         MOVE MFS-ROER-EJ-FAELT         TO MOD-DIKOLLIL                   
233000         IF MID-DIKOLLIL NUMERIC                                          
233100             MOVE MID-DIKOLLIL          TO WS-DIKOLLIL                    
233200             IF US-MEASUREMENT                                            
233300               COMPUTE WS-DIKOLLIL ROUNDED =                              
233400                       WS-DIKOLLIL * CONV-IN-TO-CM                        
233500               END-COMPUTE                                                
233600             END-IF                                                       
233700             MOVE MFS-NUM-FAELT-RAETT   TO MOD-DIKOLLIL-ATTR              
233800         ELSE                                                             
233900           IF WS-INDATA-RATT                                              
234000             MOVE FEL                   TO WS-INDATA-TEST                 
234100             IF WS-ORDERVIS-TRANS                                         
234200               MOVE '***POS9'           TO  MOD-TEMFSFEL                  
234300             ELSE                                                         
234400               MOVE FEL-748 (INDX)      TO MOD-TEMFSFEL                   
234500             END-IF                                                       
234600             MOVE MFS-NUM-FAELT-FEL     TO MOD-DIKOLLIL-ATTR              
234700           END-IF                                                         
234800         END-IF                                                           
234900     ELSE                                                                 
235000         MOVE MFS-ROER-EJ-FAELT         TO MOD-DIKOLLIL                   
235100         MOVE ZERO                      TO WS-DIKOLLIL                    
235200     END-IF                                                               
235300                                                                          
235400     IF MID-DIKOLLIH NOT = ALL '+'                                        
235500         MOVE MFS-ROER-EJ-FAELT         TO MOD-DIKOLLIH                   
235600         IF MID-DIKOLLIH NUMERIC                                          
235700             MOVE MID-DIKOLLIH          TO WS-DIKOLLIH                    
235800             IF US-MEASUREMENT                                            
235900               COMPUTE WS-DIKOLLIH ROUNDED =                              
236000                       WS-DIKOLLIH * CONV-IN-TO-CM                        
236100               END-COMPUTE                                                
236200             END-IF                                                       
236300             MOVE MFS-NUM-FAELT-RAETT   TO MOD-DIKOLLIH-ATTR              
236400         ELSE                                                             
236500           IF WS-INDATA-RATT                                              
236600             MOVE FEL                   TO WS-INDATA-TEST                 
236700             IF WS-ORDERVIS-TRANS                                         
236800               MOVE '***POS10'          TO  MOD-TEMFSFEL                  
236900             ELSE                                                         
237000               MOVE FEL-748 (INDX)      TO MOD-TEMFSFEL                   
237100             END-IF                                                       
237200             MOVE MFS-NUM-FAELT-FEL     TO MOD-DIKOLLIH-ATTR              
237300           END-IF                                                         
237400         END-IF                                                           
237500     ELSE                                                                 
237600         MOVE MFS-ROER-EJ-FAELT         TO MOD-DIKOLLIH                   
237700         MOVE ZERO                      TO WS-DIKOLLIH                    
237800     END-IF                                                               
237900                                                                          
238000     IF MID-DIKOLLIB NOT = ALL '+'                                        
238100         MOVE MFS-ROER-EJ-FAELT         TO MOD-DIKOLLIB                   
238200         IF MID-DIKOLLIB NUMERIC                                          
238300             MOVE MID-DIKOLLIB          TO WS-DIKOLLIB                    
238400             IF US-MEASUREMENT                                            
238500               COMPUTE WS-DIKOLLIB ROUNDED =                              
238600                       WS-DIKOLLIB * CONV-IN-TO-CM                        
238700               END-COMPUTE                                                
238800             END-IF                                                       
238900             MOVE MFS-NUM-FAELT-RAETT   TO MOD-DIKOLLIB-ATTR              
239000         ELSE                                                             
239100           IF WS-INDATA-RATT                                              
239200             MOVE FEL                   TO WS-INDATA-TEST                 
239300             IF WS-ORDERVIS-TRANS                                         
239400               MOVE '***POS11'          TO  MOD-TEMFSFEL                  
239500             ELSE                                                         
239600               MOVE FEL-748 (INDX)      TO MOD-TEMFSFEL                   
239700             END-IF                                                       
239800             MOVE MFS-NUM-FAELT-FEL     TO MOD-DIKOLLIB-ATTR              
239900           END-IF                                                         
240000         END-IF                                                           
240100     ELSE                                                                 
240200         MOVE MFS-ROER-EJ-FAELT         TO MOD-DIKOLLIB                   
240300         MOVE ZERO                      TO WS-DIKOLLIB                    
240400     END-IF                                                               
240500     .                                                                    
240600     EJECT                                                                
240700 BD-KONTROLLERA-RAD     SECTION.                                          
240800     SKIP3                                                                
240900     IF MID-IDRADNR-FOM (INX) = ALL '+'                                   
241000         IF MID-IDRADNR-TOM (INX) = ALL '+'                               
241100             MOVE MFS-RENSA-FAELT      TO MOD-IDRADNR-FOM (INX)           
241200                                          MOD-IDRADNR-TOM (INX)           
241300                                          MOD-KVLEVART (INX)              
241400             IF INX = +1                                                  
241500               IF  WS-IDDISTR NUMERIC                                     
241600               AND WS-IDDISTR < '0800'                                    
241700               AND WS-ORDERVIS-TRANS                                      
241800               AND WS-IDKOLLI NUMERIC                                     
241900               AND WS-IDKOLLI > '99000'                                   
242000               CONTINUE                                                   
242100               ELSE                                                       
242200                 MOVE UPPLYSNING-1 (INDX) TO  MOD-TEMFSFEL                
242300                 MOVE FEL                 TO  WS-INDATA-TEST              
242400               END-IF                                                     
242500             END-IF                                                       
242600             MOVE +13                     TO  INX                         
242700         ELSE                                                             
242800             MOVE FEL                     TO  WS-INDATA-TEST              
242900             MOVE FEL-7381(INDX)          TO  MOD-TEMFSFEL                
243000             MOVE MFS-NUM-FAELT-FEL       TO                              
243100                                 MOD-IDRADNR-FOM-ATTR (INX)               
243200                                 MOD-IDRADNR-TOM-ATTR (INX)               
243300                                 MOD-KVLEVART-ATTR (INX)                  
243400         END-IF                                                           
243500     ELSE                                                                 
243600         IF MID-IDRADNR-FOM (INX) NUMERIC                                 
243700             IF MID-IDRADNR-FOM (INX) > ZERO                              
243800                 MOVE MFS-NUM-FAELT-RAETT TO                              
243900                                 MOD-IDRADNR-FOM-ATTR (INX)               
244000             ELSE                                                         
244100                 MOVE FEL               TO WS-INDATA-TEST                 
244200                 MOVE FEL-7382(INDX)    TO MOD-TEMFSFEL                   
244300                 MOVE MFS-NUM-FAELT-FEL TO                                
244400                                 MOD-IDRADNR-FOM-ATTR (INX)               
244500             END-IF                                                       
244600             IF MID-IDRADNR-TOM (INX) = ALL '+'                           
244700                 MOVE ZERO             TO MID-IDRADNR-TOM (INX)           
244800                 MOVE MFS-RENSA-FAELT  TO MOD-IDRADNR-TOM (INX)           
244900                 ADD +1                TO WS-ANT-RAD-I-BILD               
245000             ELSE                                                         
245100                 IF MID-IDRADNR-TOM (INX) NUMERIC                         
245200                     IF MID-IDRADNR-FOM (INX)                             
245300                                         < MID-IDRADNR-TOM (INX)          
245400                         MOVE MID-IDRADNR-FOM (INX) TO WS-RAD-FOM         
245500                         MOVE MID-IDRADNR-TOM (INX) TO WS-RAD-TOM         
245600                         COMPUTE WS-ANT-RAD-I-BILD =                      
245700                                 WS-ANT-RAD-I-BILD +                      
245800                                 WS-RAD-TOM        -                      
245900                                 WS-RAD-FOM        + 1                    
246000                         END-COMPUTE                                      
246100                         MOVE MFS-NUM-FAELT-RAETT TO                      
246200                                 MOD-IDRADNR-TOM-ATTR (INX)               
246300                     ELSE                                                 
246400                         MOVE FEL         TO  WS-INDATA-TEST              
246500                         MOVE FEL-7383(INDX) TO  MOD-TEMFSFEL             
246600                         MOVE MFS-NUM-FAELT-FEL TO                        
246700                                 MOD-IDRADNR-FOM-ATTR (INX)               
246800                                 MOD-IDRADNR-TOM-ATTR (INX)               
246900                     END-IF                                               
247000                 ELSE                                                     
247100                     MOVE FEL             TO  WS-INDATA-TEST              
247200                     IF WS-ORDERVIS-TRANS                                 
247300                       MOVE '***POS12'     TO  MOD-TEMFSFEL               
247400                     ELSE                                                 
247500                       MOVE FEL-748 (INDX) TO MOD-TEMFSFEL                
247600                     END-IF                                               
247700                     MOVE MFS-NUM-FAELT-FEL TO                            
247800                                       MOD-IDRADNR-TOM-ATTR (INX)         
247900                 END-IF                                                   
248000             END-IF                                                       
248100         ELSE                                                             
248200             MOVE FEL                     TO  WS-INDATA-TEST              
248300             IF WS-ORDERVIS-TRANS                                         
248400               MOVE '***POS13'          TO  MOD-TEMFSFEL                  
248500             ELSE                                                         
248600               MOVE FEL-748 (INDX)      TO MOD-TEMFSFEL                   
248700             END-IF                                                       
248800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-FOM-ATTR (INX)         
248900                                       MOD-IDRADNR-TOM-ATTR (INX)         
249000         END-IF                                                           
249100         ADD +1  TO  RAD-INX                                              
249200     END-IF                                                               
249300     SKIP2                                                                
249400     IF INX = 13                                                          
249500*        << KOLLA RESTEN AV BILDEN   >>                                   
249600         ADD RAD-INX +1                   GIVING REST-INX                 
249700         PERFORM UNTIL REST-INX NOT < MAX-RAD-ANTAL-PLUS-1                
249800             IF  MID-IDRADNR-FOM (REST-INX) NOT = ALL '+'                 
249900                 MOVE MFS-ROER-EJ-FAELT   TO                              
250000                          MOD-IDRADNR-FOM (REST-INX)                      
250100                 MOVE MFS-NUM-FAELT-FEL   TO                              
250200                          MOD-IDRADNR-FOM-ATTR (REST-INX)                 
250300                 MOVE FEL                 TO WS-INDATA-TEST               
250400                 IF WS-ORDERVIS-TRANS                                     
250500                   MOVE '***POS14'      TO  MOD-TEMFSFEL                  
250600                 ELSE                                                     
250700                   MOVE FEL-748 (INDX)  TO MOD-TEMFSFEL                   
250800                 END-IF                                                   
250900             ELSE                                                         
251000                 MOVE MFS-RENSA-FAELT     TO                              
251100                          MOD-IDRADNR-FOM (REST-INX)                      
251200                 MOVE MFS-NUM-FAELT-RAETT TO                              
251300                          MOD-IDRADNR-FOM-ATTR (REST-INX)                 
251400             END-IF                                                       
251500             IF  MID-IDRADNR-TOM (REST-INX) NOT = ALL '+'                 
251600                 MOVE MFS-ROER-EJ-FAELT   TO                              
251700                          MOD-IDRADNR-TOM (REST-INX)                      
251800                 MOVE MFS-NUM-FAELT-FEL   TO                              
251900                          MOD-IDRADNR-TOM-ATTR (REST-INX)                 
252000                 MOVE FEL                 TO WS-INDATA-TEST               
252100                 IF WS-ORDERVIS-TRANS                                     
252200                   MOVE '***POS15'      TO  MOD-TEMFSFEL                  
252300                 ELSE                                                     
252400                   MOVE FEL-748 (INDX)  TO MOD-TEMFSFEL                   
252500                 END-IF                                                   
252600             ELSE                                                         
252700                 MOVE MFS-RENSA-FAELT     TO                              
252800                          MOD-IDRADNR-TOM (REST-INX)                      
252900                 MOVE MFS-NUM-FAELT-RAETT TO                              
253000                          MOD-IDRADNR-TOM-ATTR (REST-INX)                 
253100             END-IF                                                       
253200             IF  MID-KVLEVART (REST-INX) NOT = ALL '+'                    
253300                 MOVE MFS-ROER-EJ-FAELT   TO                              
253400                          MOD-KVLEVART (REST-INX)                         
253500                 MOVE MFS-NUM-FAELT-FEL   TO                              
253600                          MOD-KVLEVART-ATTR (REST-INX)                    
253700                 MOVE FEL                 TO WS-INDATA-TEST               
253800                 IF WS-ORDERVIS-TRANS                                     
253900                   MOVE '***POS16'      TO  MOD-TEMFSFEL                  
254000                 ELSE                                                     
254100                   MOVE FEL-748 (INDX)  TO MOD-TEMFSFEL                   
254200                 END-IF                                                   
254300             ELSE                                                         
254400                 MOVE MFS-RENSA-FAELT     TO                              
254500                          MOD-KVLEVART (REST-INX)                         
254600                 MOVE MFS-NUM-FAELT-RAETT TO                              
254700                          MOD-KVLEVART-ATTR (REST-INX)                    
254800             END-IF                                                       
254900             ADD +1                       TO REST-INX                     
255000         END-PERFORM                                                      
255100     ELSE                                                                 
255200         IF MID-KVLEVART (INX) = ALL '+'                                  
255300             MOVE MFS-RENSA-FAELT         TO MOD-KVLEVART (INX)           
255400         ELSE                                                             
255500             IF MID-KVLEVART (INX) NUMERIC                                
255600                 IF MID-KVLEVART (INX) = ZERO                             
255700*----------------------------------------------NOLLADE AVVIKELSER         
255800*----------------------------------------------GODKÄNNAS SOM              
255900*----------------------------------------------ORAPPORTERAD RAD           
256000*----------------------------------------------PÅ ANNAN BILD              
256100                     MOVE FEL             TO  WS-INDATA-TEST              
256200                     MOVE FEL-724 (INDX)  TO  MOD-TEMFSFEL                
256300                     MOVE MFS-NUM-FAELT-FEL TO                            
256400                                          MOD-KVLEVART-ATTR (INX)         
256500                 ELSE                                                     
256600                     IF  MID-IDRADNR-TOM (INX) NOT = ALL '+'              
256700                     AND MID-IDRADNR-TOM (INX) NOT = ZERO                 
256800                         MOVE FEL         TO  WS-INDATA-TEST              
256900             IF WS-ORDERVIS-TRANS                                         
257000               MOVE '***POS17'          TO  MOD-TEMFSFEL                  
257100             ELSE                                                         
257200               MOVE FEL-748 (INDX)      TO MOD-TEMFSFEL                   
257300             END-IF                                                       
257400                         MOVE MFS-NUM-FAELT-FEL TO                        
257500                                          MOD-KVLEVART-ATTR (INX)         
257600                     ELSE                                                 
257700                         MOVE MFS-NUM-FAELT-RAETT TO                      
257800                                          MOD-KVLEVART-ATTR (INX)         
257900                     END-IF                                               
258000                 END-IF                                                   
258100             ELSE                                                         
258200                 MOVE FEL                 TO  WS-INDATA-TEST              
258300             IF WS-ORDERVIS-TRANS                                         
258400               MOVE '***POS18'          TO  MOD-TEMFSFEL                  
258500             ELSE                                                         
258600               MOVE FEL-748 (INDX)      TO MOD-TEMFSFEL                   
258700             END-IF                                                       
258800                 MOVE MFS-NUM-FAELT-FEL   TO                              
258900                                          MOD-KVLEVART-ATTR (INX)         
259000             END-IF                                                       
259100         END-IF                                                           
259200     END-IF                                                               
259300     .                                                                    
259400     EJECT                                                                
259500 BE-INIT-KOLLI         SECTION.                                           
259600     SKIP3                                                                
259700     MOVE SPACE              TO MOD-KDKOLLI                               
259800                                WS-KDKOLLI                                
259900     MOVE ZERO               TO WS-KDEMBTYP                               
260000                                WS-DIKOLLIL                               
260100                                WS-DIKOLLIB                               
260200                                WS-DIKOLLIH                               
260300                                WS-MOD-VKORDBTO                           
260400                                                                          
260500     MOVE ZERO               TO MOD-KDEMBTYP                              
260600                                MOD-DIKOLLIL                              
260700                                MOD-DIKOLLIB                              
260800                                MOD-DIKOLLIH                              
260900                                MOD-VKORDBTO-KOLLI                        
261000     .                                                                    
261100     EJECT                                                                
261200 C-RELATIONSKONTROLL  SECTION.                                            
261300                                                                          
261400     MOVE WS-IDKUNDNR           TO  WS-IDKUNDNR-NUM                       
261500*                                                                         
261600     IF WS-SAMMA-BILD                                                     
261800         PERFORM CA-KONTROLLERA-KUNDORDNR                                 
261900         IF WS-INDATA-RATT                                                
262000             PERFORM CB-KONTROLLERA-IDKOLLI                               
262100             IF WS-INDATA-RATT                                            
262200                 PERFORM CC-KONTROLLERA-PACKARE                           
262300             END-IF                                                       
262400             IF WS-INDATA-RATT                                            
262500                 PERFORM CD-KONTROLLERA-KOLLIUPPG                         
262600             END-IF                                                       
262700             IF WS-INDATA-RATT                                            
262800                 PERFORM CG-OVRIG-KONTROLL                                
262900             END-IF                                                       
264600         END-IF                                                           
264700     ELSE                                                                 
264800         IF WS-ORDERVIS-TRANS                                             
264900           MOVE WS-IDDISTR-NUM    TO W-4A1-IDDISTR                        
265000           MOVE WS-IDKUNDNR-NUM   TO W-4A1-IDKUNDNR                       
265100           MOVE WS-IDORDNR        TO W-4A1-IDORDNR                        
265200                                                                          
265300           PERFORM IMS-GU-KUNDORDER-SEK                                   
265400           MOVE KORD-IDORDER      TO WS-KORD-IDORDER                      
265500         END-IF                                                           
265600*                                                                         
265700         PERFORM CB-KONTROLLERA-IDKOLLI                                   
265800         IF WS-INDATA-RATT                                                
265900             PERFORM CD-KONTROLLERA-KOLLIUPPG                             
266000         END-IF                                                           
266500     END-IF                                                               
266600                                                                          
266700     IF WS-INDATA-RATT                                                    
266720         PERFORM CE-KOLLA-PLATSSATTNING                                   
266730     END-IF                                                               
266740                                                                          
266800     IF WS-INDATA-RATT                                                    
266900       IF WS-IDDC NOT = W-IDDC-B6                                         
267000          MOVE WS-IDDC TO W-IDDC-B6                                       
267100          PERFORM IMS-GU-WDB601                                           
267200       END-IF                                                             
267300     END-IF                                                               
267400     .                                                                    
267500     SKIP3                                                                
267600 CA-KONTROLLERA-KUNDORDNR SECTION.                                        
267700                                                                          
267800     IF MID-IDPRODNR-IN = ALL '+'                                         
267900         IF    MID-IDDISTR-IN  = ALL '+'                                  
268000           AND MID-IDKUNDNR-IN = ALL '+'                                  
268100           AND MID-IDORDNR-IN  = ALL '+'                                  
268200             IF WS-IDPRODNR > ZERO                                        
268300*                << ANVÄNDS GAMLA PRODNR: MID-IDPRODNR-UT >>              
268400                 MOVE JA              TO  SOEK-VIA-PRODNR                 
268500                 MOVE MFS-RENSA-FAELT TO  MOD-IDDISTR-UT                  
268600                                          MOD-IDKUNDNR-UT                 
268700                                          MOD-IDORDNR-UT                  
268800             ELSE                                                         
268900                 PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                        
269000                 MOVE MFS-RENSA-FAELT TO  MOD-IDPRODNR-UT                 
269100             END-IF                                                       
269200         ELSE                                                             
269300             PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                            
269400             MOVE MFS-RENSA-FAELT     TO  MOD-IDPRODNR-UT                 
269500         END-IF                                                           
269600     ELSE                                                                 
269700         MOVE JA                       TO SOEK-VIA-PRODNR                 
269800         MOVE MFS-RENSA-FAELT          TO MOD-IDDISTR-UT                  
269900                                          MOD-IDKUNDNR-UT                 
270000                                          MOD-IDORDNR-UT                  
270100     END-IF                                                               
270200                                                                          
270300     IF WS-INDATA-RATT                                                    
270400         MOVE WS-IDPRODNR               TO   W-601-IDPRODNR               
270500         PERFORM CAB-KOLLA-MOT-WDE411-BSEQ                                
270600     END-IF                                                               
270700     .                                                                    
270800     SKIP2                                                                
270900 CAA-HAMTA-PRODNR-I-WDE4-6  SECTION.                                      
271000*                                                                         
271100     MOVE WS-IDDISTR-NUM   TO W-4A1-IDDISTR                               
271200     MOVE WS-IDKUNDNR-NUM  TO W-4A1-IDKUNDNR                              
271300     MOVE WS-IDORDNR       TO W-4A1-IDORDNR                               
271400                                                                          
271500     PERFORM IMS-GU-KUNDORDER-SEK                                         
271600                                                                          
271700     IF KUNDORDER-SEK-FINNS                                               
271800       PERFORM UNTIL (KORD-IDDC = WS-IDDC AND                             
271900         KORD-KVORDRAD-LEVPL = ZERO) OR KUNDORDER-SEK-SAKNAS              
272000          PERFORM IMS-GN-SEQA-WDE4A1                                      
272100       END-PERFORM                                                        
272200                                                                          
272300       IF KUNDORDER-SEK-FINNS                                             
272400         MOVE KORD-IDPRODNR TO W-601-IDPRODNR                             
272500         PERFORM IMS-GU-WDE601                                            
272600         IF SEGMENT-FINNS                                                 
272700           MOVE VORD-IDPRODNR TO WS-IDPRODNR                              
272800         ELSE                                                             
272900           MOVE FEL                       TO   WS-INDATA-TEST             
273000           MOVE FEL-7011 (INDX)           TO   MOD-TEMFSFEL               
273100         END-IF                                                           
273200       ELSE                                                               
273300         MOVE FEL                          TO   WS-INDATA-TEST            
273400         MOVE FEL-7012 (INDX)              TO   MOD-TEMFSFEL              
273500       END-IF                                                             
273600     ELSE                                                                 
273700       MOVE FEL                          TO   WS-INDATA-TEST              
273800       MOVE FEL-7013 (INDX)              TO   MOD-TEMFSFEL                
273900     END-IF                                                               
274000     .                                                                    
274100     EJECT                                                                
274200 CAB-KOLLA-MOT-WDE411-BSEQ  SECTION.                                      
274300                                                                          
274400     MOVE W-601-IDPRODNR                TO W-420-IDPRODNR-MIN             
274500                                           W-420-IDPRODNR-MAX             
274600                                           W-420-IDPRODNR                 
274700     MOVE 1                             TO W-420-IDPURAD-MIN              
274800                                           W-420-IDPURAD                  
274900     MOVE 99999                         TO W-420-IDPURAD-MAX              
275000     PERFORM IMS-GU-KUNDORDER-SEK-INV                                     
275100                                                                          
275200     IF SEGMENT-FINNS                                                     
275300        MOVE KORD-IDDISTR               TO WS-IDDISTR-NUM                 
275400        MOVE WS-IDDISTR-NUM             TO MOD-IDDISTR-UT                 
275500        INSPECT MOD-IDDISTR-UT REPLACING                                  
275600                LEADING ZERO BY SPACE                                     
275700        MOVE KORD-IDKUNDNR              TO WS-IDKUNDNR-NUM                
275800        MOVE WS-IDKUNDNR-NUM            TO MOD-IDKUNDNR-UT                
275900        INSPECT MOD-IDKUNDNR-UT REPLACING                                 
276000                LEADING ZERO BY SPACE                                     
276100        MOVE KORD-IDKUNDRF              TO WS-IDKUNDRF                    
276200        MOVE WS-IDORDNR                 TO   MOD-IDORDNR-UT               
276300        INSPECT MOD-IDORDNR-UT  REPLACING                                 
276400                LEADING ZERO BY SPACE                                     
276500        MOVE KORD-KDFRAKT               TO PLATS-KDFRAKT                  
276600        MOVE KORD-KDORDKL               TO PLATS-KDORDKLX                 
276700                                           WS-KDORDKL                     
276800*                                                                         
276900        MOVE KORD-IDORDER               TO WS-KORD-IDORDER                
277000*                                                                         
277100*-----------------------------------------------ÄR ORDERN                 
277200*-----------------------------------------------FÄRDIGRAPPORTERAD         
277300     ELSE                                                                 
277400         MOVE FEL                       TO   WS-INDATA-TEST               
277500         MOVE FEL-7013 (INDX)           TO   MOD-TEMFSFEL                 
277600     END-IF                                                               
277700     .                                                                    
277800     EJECT                                                                
277900 CB-KONTROLLERA-IDKOLLI SECTION.                                          
278000                                                                          
278100     IF WS-IDDC NOT = W-IDDC-B6                                           
278200        MOVE WS-IDDC TO W-IDDC-B6                                         
278300        PERFORM IMS-GU-WDB601                                             
278400     END-IF                                                               
278500     IF KOLLI-INTERVALL                                                   
278600         MOVE WS-IDDISTR-NUM TO TEST-IDDISTR                              
278700         IF MID-IDRADNR-TOM (1) NOT = ZERO                                
278800           IF WS-INDATA-RATT                                              
278900             MOVE FEL                    TO   WS-INDATA-TEST              
279000             MOVE FEL-734 (INDX)         TO   MOD-TEMFSFEL                
279100             MOVE MFS-NUM-FAELT-FEL      TO                               
279200                                         MOD-IDRADNR-TOM-ATTR (1)         
279300           END-IF                                                         
279400         END-IF                                                           
279500         IF MID-IDRADNR-FOM (2) NOT = ALL '+'                             
279600           IF WS-INDATA-RATT                                              
279700             MOVE FEL                    TO   WS-INDATA-TEST              
279800             MOVE FEL-733 (INDX)         TO   MOD-TEMFSFEL                
279900             MOVE MFS-NUM-FAELT-FEL      TO                               
280000                                         MOD-IDRADNR-FOM-ATTR (2)         
280100           END-IF                                                         
280200         END-IF                                                           
280300         IF WS-INDATA-RATT                                                
280400             IF    WS-IDKOLLI-FOM       = ZERO                            
280500               OR  WS-IDKOLLI-TOM       = ZERO                            
280600                 MOVE FEL                TO   WS-INDATA-TEST              
280700                 MOVE FEL-749 (INDX)     TO   MOD-TEMFSFEL                
280800             END-IF                                                       
280900             COMPUTE ARB-ANTAL-KOLLI =                                    
281000                         WS-IDKOLLI-TOM - WS-IDKOLLI-FOM + 1              
281100             END-COMPUTE                                                  
281200             COMPUTE ARB-ANTAL-KOLLI-PLUS-1 =                             
281300                         ARB-ANTAL-KOLLI + 1                              
281400             END-COMPUTE                                                  
281500             MOVE 1                      TO   ACK-KOLLI                   
281600             PERFORM IMS-GHU-KOLLIREG                                     
281700                                                                          
281800             IF VORD-IDDC NOT = WS-IDDC                                   
281900                MOVE FEL                 TO   WS-INDATA-TEST              
282000                MOVE FEL-7014 (INDX)     TO   MOD-TEMFSFEL                
282100             END-IF                                                       
282200             IF VORD-KDORDSTA > 2                                         
282300                MOVE FEL                 TO   WS-INDATA-TEST              
282400                MOVE FEL-718 (INDX)      TO   MOD-TEMFSFEL                
282500             END-IF                                                       
282600                                                                          
282700             MOVE VORD-KDFRAKT           TO   PLATS-KDFRAKT               
282800             MOVE VORD-KDORDKL           TO   PLATS-KDORDKLX              
282900                                              WS-KDORDKL                  
283000             MOVE VORD-IDDC              TO   PLATS-IDDC                  
283100             MOVE VORD-IDDISTR           TO   PLATS-IDDISTR               
283200             MOVE VORD-IDKUNDNR          TO   PLATS-IDKUNDNR              
283300             MOVE VORD-FLAUTFAK          TO   WS-FLAUTFAK                 
283400             MOVE VORD-KDFAKTYP          TO   WS-KDFAKTYP                 
283500                                                                          
283600             IF   VORD-KDORDKL = 4                                        
283700               IF  VORD-IDDISTR = +00878                                  
283800               AND VORD-IDKUNDNR > +006000                                
283900                  MOVE 2                 TO   PLATS-KDCALL                
284000               ELSE                                                       
284100                  MOVE 1                 TO   PLATS-KDCALL                
284200               END-IF                                                     
284300             ELSE                                                         
284400                  MOVE 2                 TO   PLATS-KDCALL                
284500             END-IF                                                       
284600                                                                          
284700             MOVE WS-IDKOLLI-FOM         TO   WS-IDKOLLI-NUM              
284800             PERFORM UNTIL ACK-KOLLI NOT < ARB-ANTAL-KOLLI-PLUS-1         
284900                 MOVE WS-IDKOLLI-NUM     TO   W-611-IDKOLLI               
285000                 PERFORM IMS-GHNP-KOLLI                                   
285100                 IF SEGMENT-FINNS                                         
285200                     MOVE FEL            TO   WS-INDATA-TEST              
285300                     MOVE FEL-736 (INDX) TO   MOD-TEMFSFEL                
285400                 END-IF                                                   
285500                 ADD 1                   TO   ACK-KOLLI                   
285600                                              WS-IDKOLLI-NUM              
285700             END-PERFORM                                                  
285800         END-IF                                                           
285900     ELSE                                                                 
286000                                                                          
286100         IF WS-IDKOLLI                   =    ZERO                        
286200             MOVE FEL                    TO   WS-INDATA-TEST              
286300             MOVE FEL-749 (INDX)         TO   MOD-TEMFSFEL                
286400         END-IF                                                           
286500         IF WS-INDATA-RATT                                                
286600           MOVE WS-IDPRODNR              TO   W-601-IDPRODNR              
286700           MOVE WS-IDKOLLI               TO   W-421-IDKOLLI               
286800                                              W-611-IDKOLLI               
286900           PERFORM IMS-GHU-KOLLIREG                                       
287000             IF VORD-IDDC = WS-IDDC                                       
287100               MOVE VORD-KDFRAKT         TO   PLATS-KDFRAKT               
287200               MOVE VORD-KDORDKL         TO   PLATS-KDORDKLX              
287300                                              WS-KDORDKL                  
287400               MOVE VORD-IDDC            TO   PLATS-IDDC                  
287500               MOVE VORD-IDDISTR         TO   PLATS-IDDISTR               
287600               MOVE VORD-IDKUNDNR        TO   PLATS-IDKUNDNR              
287700               MOVE VORD-FLAUTFAK        TO   WS-FLAUTFAK                 
287800               MOVE VORD-KDFAKTYP        TO   WS-KDFAKTYP                 
287900                                                                          
288000               IF WS-KDORDKL = 4                                          
288100                 IF VORD-IDDISTR = +00878                                 
288200                 AND VORD-IDKUNDNR > +006000                              
288300                    MOVE 2               TO   PLATS-KDCALL                
288400                 ELSE                                                     
288500                    MOVE 1               TO   PLATS-KDCALL                
288600                 END-IF                                                   
288700               ELSE                                                       
288800                    MOVE 2               TO   PLATS-KDCALL                
288900               END-IF                                                     
289000                                                                          
289100               PERFORM IMS-GHNP-KOLLI                                     
289200               IF SEGMENT-FINNS                                           
289300                   MOVE FEL              TO   WS-INDATA-TEST              
289400                   MOVE FEL-721 (INDX)   TO   MOD-TEMFSFEL                
289500               ELSE                                                       
289600                   MOVE 1                TO   ARB-ANTAL-KOLLI             
289700               END-IF                                                     
289800             ELSE                                                         
289900*---------------------------------------------FELAKTIGT                   
290000*---------------------------------------------DC-LAGER                    
290100                MOVE FEL                 TO   WS-INDATA-TEST              
290200                MOVE FEL-7015 (INDX)     TO   MOD-TEMFSFEL                
290300                IF WS-ORDERVIS-TRANS                                      
290400                      MOVE ABEND-MED1   TO   FELTEXT                      
290500                  CALL FELLOG                                             
290600*FELLOG INLAGT 951219 FÖR ATT FÅ EN ABEND-DUMP VID FEL FRÅN               
290700*ORDERVIS PACKN.                                                          
290800                END-IF                                                    
290900             END-IF                                                       
291000         END-IF                                                           
291100     END-IF                                                               
291200     .                                                                    
291300     EJECT                                                                
291400 CC-KONTROLLERA-PACKARE SECTION.                                          
291500                                                                          
291600     MOVE NEJ                     TO WS-TRAEFF-PACKARE                    
291700     MOVE JA                      TO WS-PACKARES-ODEL-REDAN-KLARA         
291800*                                                                         
291900     MOVE WS-IDDISTR-NUM                TO W-4A1-IDDISTR                  
292000     MOVE WS-IDKUNDNR-NUM               TO W-4A1-IDKUNDNR                 
292100     MOVE WS-IDORDNR                    TO W-4A1-IDORDNR                  
292200     PERFORM IMS-GU-KUNDORDER-SEK                                         
292300     IF KUNDORDER-SEK-FINNS                                               
292400                                                                          
292500        PERFORM UNTIL KUNDORDER-SEK-SAKNAS                                
292600          MOVE KORD-IDDISTR             TO W-401-IDDISTR                  
292700          MOVE KORD-IDKUNDNR            TO W-401-IDKUNDNR                 
292800          MOVE KORD-IDORDNR5            TO W-401-IDORDNR                  
292900          MOVE KORD-IDPRODNR            TO W-401-IDPRODNR                 
293000          MOVE KORD-IDPLKLST            TO W-401-IDPLKLST                 
293100          MOVE KORD-IDORDER             TO WS-IDORDER                     
293200          PERFORM IMS-GU-WDE401                                           
293300          MOVE KORD-IDPRODNR         TO   WS-JFR-IDPRODNR                 
293400          MOVE KORD-IDUSER           TO   WS-JFR-IDANSTNR                 
293500          IF NOT WS-ORDERVIS-TRANS AND                                    
293600             WS-IDPRODNR = WS-JFR-IDPRODNR AND                            
293700             WS-IDANSTNR = WS-JFR-IDANSTNR-5                              
293800             MOVE 'J'                TO   WS-TRAEFF-PACKARE               
293900             PERFORM CCA-KONTOLLERA-PLOCKLISTA                            
294000          END-IF                                                          
294100          PERFORM IMS-GN-SEQA-WDE4A1                                      
294200        END-PERFORM                                                       
294300     END-IF                                                               
294400*                                                                         
294500     IF WS-TRAEFF-PACKARE  = 'N' AND                                      
294600        NOT WS-ORDERVIS-TRANS                                             
294700*----------------------------------------------SAKNAS ANGIVEN             
294800*----------------------------------------------PACKARE PÅ ORDERN          
294900        MOVE FEL                        TO   WS-INDATA-TEST               
295000        MOVE FEL-719 (INDX)             TO   MOD-TEMFSFEL                 
295100*----------------------------------------------ÄR ANGIVEN PACKARES        
295200*----------------------------------------------ORDERDEL REDAN KLAW        
295300     ELSE                                                                 
295400       IF WS-INDATA-RATT   AND                                            
295500          WS-PACKARES-ODEL-REDAN-KLARA = JA                               
295600         MOVE FEL               TO   WS-INDATA-TEST                       
295700         MOVE FEL-720 (INDX)    TO   MOD-TEMFSFEL                         
295800       END-IF                                                             
295900     END-IF                                                               
296000     .                                                                    
296100     EJECT                                                                
296200 CCA-KONTOLLERA-PLOCKLISTA               SECTION.                         
296300                                                                          
296400     IF KORD-KDPAKOLL NOT = ZERO                                          
296500*-----------------------------------------FÅR MAN EJ RAPPORTERA DÅ        
296600*-----------------------------------------AVVIKELSEKONTROLL PÅGÅR         
296700        MOVE FEL               TO   WS-INDATA-TEST                        
296800        MOVE FEL-804 (INDX)    TO   MOD-TEMFSFEL                          
296900     ELSE                                                                 
297000                                                                          
297100       IF ( (KORD-KVORDRAD-PACK NOT = KORD-KVORDRAD)       OR             
297200            (KORD-KVORDRAD-LEVPL    > 0                    AND            
297300             KORD-KVORDRAD-PACK NOT = KORD-KVORDRAD-LEVPL) )              
297400         MOVE NEJ             TO WS-PACKARES-ODEL-REDAN-KLARA             
297500       END-IF                                                             
297600     END-IF                                                               
297700     .                                                                    
297800     EJECT                                                                
297900 CD-KONTROLLERA-KOLLIUPPG  SECTION.                                       
298000                                                                          
298100     PERFORM CDA-KONTROLLERA-VIKT                                         
298200     MOVE MFS-ALFA-FAELT-RAETT          TO MOD-KDKOLLI-ATTR               
298300     MOVE MFS-NUM-FAELT-RAETT           TO MOD-KDEMBTYP-ATTR              
298400                                           MOD-DIKOLLIH-ATTR              
298500                                           MOD-DIKOLLIL-ATTR              
298600                                           MOD-DIKOLLIB-ATTR              
298700     IF WS-KDKOLLI NOT = SPACE                                            
298800         MOVE WS-KDKOLLI TO W-KDKOLLI-WDK5                                
298900         PERFORM IMS-GET-EMBB                                             
299000         IF SEGMENT-SAKNAS                                                
299100             MOVE FEL                   TO WS-INDATA-TEST                 
299200             MOVE FEL-726 (INDX)        TO MOD-TEMFSFEL                   
299300             MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDKOLLI-ATTR               
299400         ELSE                                                             
299500*------------------------------------------VISSA KOLLIKODER GER           
299600*------------------------------------------EJ ALLA MÅTT. DÅ SKALL         
299700*------------------------------------------DESSA KOMPLETTERAS.            
299800             MOVE EMB-KDKOLLID          TO WS-KDKOLLID                    
299900             MOVE EMB-VKTARA            TO WS-EMB-VKTARA-ONE-CASE         
300000                                                                          
300100             IF  EMB-EMBPROF NOT = SPACE                                  
300200                 MOVE EMB-EMBPROF       TO WS-EMBPROF                     
300300                 MOVE EMB-KVPALL        TO WS-KVPALL                      
300400                 MOVE EMB-KVLOCK        TO WS-KVLOCK                      
300500                 MOVE EMB-KVRAM         TO WS-KVRAM                       
300600             END-IF                                                       
300700                                                                          
300800             IF WS-KDEMBTYP > ZERO                                        
300900             AND NOT WS-ORDERVIS-TRANS                                    
301000                 MOVE FEL               TO WS-INDATA-TEST                 
301100                 IF WS-ORDERVIS-TRANS                                     
301200                   MOVE '***POS19'      TO  MOD-TEMFSFEL                  
301300                 ELSE                                                     
301400                   MOVE FEL-748 (INDX)  TO MOD-TEMFSFEL                   
301500                 END-IF                                                   
301600                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDEMBTYP-ATTR              
301700             ELSE                                                         
301800                 MOVE EMB-KDEMBTYP      TO WS-KDEMBTYP                    
301900             END-IF                                                       
302000             IF WS-DIKOLLIL = ZERO                                        
302100                 IF EMB-DIKOLLIL = ZERO                                   
302200                   IF  DIST03-SVERIGE                                     
302300                   AND WS-ORDERVIS-TRANS                                  
302400                   AND WS-IDKOLLI > '99000'                               
302500                   CONTINUE                                               
302600                   ELSE                                                   
302700                     MOVE FEL           TO WS-INDATA-TEST                 
302800                     MOVE FEL-728 (INDX) TO MOD-TEMFSFEL                  
302900                     MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIL-ATTR          
303000                   END-IF                                                 
303100                 ELSE                                                     
303200                     MOVE EMB-DIKOLLIL   TO WS-DIKOLLIL                   
303300                 END-IF                                                   
303400             END-IF                                                       
303500             IF WS-DIKOLLIH = ZERO                                        
303600                 IF EMB-DIKOLLIH = ZERO                                   
303700                   IF  DIST03-SVERIGE                                     
303800                   AND WS-ORDERVIS-TRANS                                  
303900                   AND WS-IDKOLLI > '99000'                               
304000                   CONTINUE                                               
304100                   ELSE                                                   
304200                     IF WS-INDATA-RATT                                    
304300                       MOVE FEL           TO WS-INDATA-TEST               
304400                       MOVE FEL-728 (INDX) TO MOD-TEMFSFEL                
304500                       MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIH-ATTR        
304600                     END-IF                                               
304700                   END-IF                                                 
304800                 ELSE                                                     
304900                     MOVE EMB-DIKOLLIH   TO WS-DIKOLLIH                   
305000                 END-IF                                                   
305100             END-IF                                                       
305200             IF WS-DIKOLLIB = ZERO                                        
305300                 IF EMB-DIKOLLIB = ZERO                                   
305400                   IF  DIST03-SVERIGE                                     
305500                   AND WS-ORDERVIS-TRANS                                  
305600                   AND WS-IDKOLLI > '99000'                               
305700                   CONTINUE                                               
305800                   ELSE                                                   
305900                     IF WS-INDATA-RATT                                    
306000                       MOVE FEL           TO WS-INDATA-TEST               
306100                       MOVE FEL-728 (INDX) TO MOD-TEMFSFEL                
306200                       MOVE MFS-NUM-FAELT-FEL TO MOD-DIKOLLIB-ATTR        
306300                     END-IF                                               
306400                   END-IF                                                 
306500                 ELSE                                                     
306600                     MOVE EMB-DIKOLLIB  TO WS-DIKOLLIB                    
306700                 END-IF                                                   
306800             END-IF                                                       
306900         END-IF                                                           
307000     END-IF                                                               
307100     IF WS-DIKOLLIL > ZERO                                                
307200       IF US-MEASUREMENT                                                  
307300         COMPUTE WS-MOD-DIKOLLIL ROUNDED =                                
307400                 WS-DIKOLLIL * CONV-CM-TO-IN                              
307500         END-COMPUTE                                                      
307600         MOVE WS-MOD-DIKOLLIL           TO  MOD-DIKOLLIL                  
307700       ELSE                                                               
307800         MOVE WS-DIKOLLIL               TO  MOD-DIKOLLIL                  
307900       END-IF                                                             
308000     END-IF                                                               
308100                                                                          
308200     IF WS-DIKOLLIH > ZERO                                                
308300       IF US-MEASUREMENT                                                  
308400         COMPUTE WS-MOD-DIKOLLIH ROUNDED =                                
308500                 WS-DIKOLLIH * CONV-CM-TO-IN                              
308600         END-COMPUTE                                                      
308700         MOVE WS-MOD-DIKOLLIH           TO  MOD-DIKOLLIH                  
308800       ELSE                                                               
308900         MOVE WS-DIKOLLIH               TO  MOD-DIKOLLIH                  
309000       END-IF                                                             
309100     END-IF                                                               
309200                                                                          
309300     IF WS-DIKOLLIB > ZERO                                                
309400       IF US-MEASUREMENT                                                  
309500         COMPUTE WS-MOD-DIKOLLIB ROUNDED =                                
309600                 WS-DIKOLLIB * CONV-CM-TO-IN                              
309700         END-COMPUTE                                                      
309800         MOVE WS-MOD-DIKOLLIB           TO  MOD-DIKOLLIB                  
309900       ELSE                                                               
310000         MOVE WS-DIKOLLIB               TO  MOD-DIKOLLIB                  
310100       END-IF                                                             
310200     END-IF                                                               
310300     .                                                                    
310400     EJECT                                                                
310500 CDA-KONTROLLERA-VIKT    SECTION.                                         
310600                                                                          
310700     MOVE MID-VKORDBTO-KOLLI         TO DEC-IDFRIDATA                     
310800     MOVE 5                          TO DEC-KVHELTAL                      
310900     MOVE 1                          TO DEC-KVDECIMAL                     
311000     CALL WDECEDIT USING DEC-WDECAREA                                     
311100                                                                          
311200       IF MID-VKORDBTO-KOLLI = ALL '+'                                    
311300           MOVE MFS-NUM-FAELT-RAETT  TO MOD-VKORDBTO-ATTR                 
311400           MOVE MFS-ROER-EJ-FAELT    TO MOD-VKORDBTO-KOLLI                
311500       ELSE                                                               
311600         IF DEC-KDSVAR-OK                                                 
311700         AND DEC-IDEDITDATA > ZERO                                        
311800                                                                          
311900             MOVE DEC-IDEDITDATA  TO WS-MOD-VKORDBTO                      
312000*                                                                         
312100             MOVE MFS-NUM-FAELT-RAETT TO MOD-VKORDBTO-ATTR                
312200             MOVE DEC-IDEDITDATA        TO WS-MOD-VKORDBTO                
312300                                         PLATS-VKORDNTO-KOLLI             
312400             IF US-MEASUREMENT                                            
312500               PERFORM S19-CONVERT-LB-TO-KG                               
312600             END-IF                                                       
312700             MOVE WS-MOD-VKORDBTO       TO PLATS-VKORDNTO-KOLLI           
312800             MOVE MFS-ROER-EJ-FAELT     TO MOD-VKORDBTO-KOLLI             
312900*          END-IF                                                         
313000*----------------------------------------ATT VI LADDAR NETTO-VIKT         
313100*----------------------------------------MED BRUTTO-VIKT ÄR OK            
313200         ELSE                                                             
313300           IF   DEC-KDSVAR-OK                                             
313400           AND  DEC-IDEDITDATA = ZERO                                     
313500               IF WS-INDATA-RATT                                          
313600                 MOVE FEL                 TO WS-INDATA-TEST               
313700                 IF WS-ORDERVIS-TRANS                                     
313800                   MOVE '***POS22'      TO  MOD-TEMFSFEL                  
313900                 ELSE                                                     
314000                   MOVE FEL-748 (INDX)  TO MOD-TEMFSFEL                   
314100*                  MOVE '****POS22'     TO MOD-TEMFSINF                   
314200                 END-IF                                                   
314300                 MOVE MFS-NUM-FAELT-FEL   TO MOD-VKORDBTO-ATTR            
314400               END-IF                                                     
314500           ELSE                                                           
314600             IF WS-INDATA-RATT                                            
314700               MOVE FEL                 TO WS-INDATA-TEST                 
314800               IF WS-ORDERVIS-TRANS                                       
314900                 MOVE '***POS23'        TO  MOD-TEMFSFEL                  
315000               ELSE                                                       
315100*                MOVE '****POS23'       TO MOD-TEMFSINF                   
315200                 MOVE FEL-748 (INDX)    TO MOD-TEMFSFEL                   
315300               END-IF                                                     
315400               MOVE MFS-NUM-FAELT-FEL   TO MOD-VKORDBTO-ATTR              
315500             END-IF                                                       
315600           END-IF                                                         
315700         END-IF                                                           
315800       END-IF                                                             
315900     .                                                                    
316000     EJECT                                                                
316100 CE-KOLLA-PLATSSATTNING SECTION.                                          
316200     SKIP3                                                                
316300     MOVE ZERO                          TO PLATS-ADVMODUL                 
316400                                           PLATS-ADHMODUL                 
316500                                           PLATS-ADFLOMR                  
316600                                           PLATS-ADRUTNIV                 
316700                                           PLATS-IDTRPTNR                 
316800                                           PLATS-DIHMODUL                 
316900     MOVE SPACE                         TO PLATS-ADFLGEO                  
317000                                           PLATS-FLUTLAST                 
317100                                           PLATS-IDDC-CROSS               
317200     MOVE WS-IDDC                       TO PLATS-IDDC                     
317300     MOVE WS-IDORDNR                    TO PLATS-IDORDNR                  
317400     MOVE WS-DIKOLLIL                   TO PLATS-DIKOLLIL                 
317500     MOVE WS-DIKOLLIB                   TO PLATS-DIKOLLIB                 
317600     MOVE WS-DIKOLLIH                   TO PLATS-DIKOLLIH                 
317700     MOVE WS-KDKOLLID                   TO PLATS-KDKOLLID                 
317800                                                                          
317900     IF WS-KDORDKL = +4                                                   
318000        IF VORD-IDDISTR = +00878                                          
318100        AND VORD-IDKUNDNR > +006000                                       
318200           MOVE 2                       TO PLATS-KDCALL                   
318300        ELSE                                                              
318400           MOVE 1                       TO PLATS-KDCALL                   
318500        END-IF                                                            
318600     ELSE                                                                 
318700        MOVE +2                         TO PLATS-KDCALL                   
318800     END-IF                                                               
318900                                                                          
319000     IF MID-ADFLGEO NOT = ALL '+'                                         
319100        MOVE MID-ADFLGEO                TO PLATS-ADFLGEO                  
319200                                           WS-ADFLGEO                     
319300     END-IF                                                               
319400     IF MID-ADFLOMR NOT = ALL '+'                                         
319500        MOVE MID-ADFLOMR                TO PLATS-ADFLOMR                  
319600                                           WS-ADFLOMR                     
319700     END-IF                                                               
319800     IF MID-ADRUTNIV NOT = ALL '+'                                        
319900        MOVE MID-ADRUTNIV               TO PLATS-ADRUTNIV                 
320000                                           WS-ADRUTNIV                    
320100     END-IF                                                               
320200                                                                          
320300     CALL W403PLAT USING PLATS-W403PLAT                                   
320400                         PLATS-DM-PCB                                     
320500                         PLATS-DN-PCB                                     
320600                         PLATS-DP-PCB                                     
320700                         PLATS-DO-PCB                                     
320800                         PLATS-WDE6C-PCB                                  
320900                         PLATS-GMTC-PCB                                   
321000                         PLATS-WDB6-PCB                                   
321100                                                                          
321200     IF PLATS-KDSVAR = SPACE                                              
321300        MOVE PLATS-IDTRPTNR   TO WS-IDTRPTNR                              
321500        MOVE PLATS-ADFLGEO    TO WS-ADFLGEO                               
321600                                 ARB-ADFLGEO                              
321800        MOVE PLATS-ADFLOMR    TO WS-ADFLOMR                               
321900                                 ARB-ADFLOMR                              
322100        MOVE PLATS-ADRUTNIV   TO WS-ADRUTNIV                              
322200                                 ARB-ADRUTNIV                             
322400        MOVE PLATS-DIHMODUL   TO WS-DIHMODUL                              
322500        MOVE PLATS-DIDMODUL   TO WS-DIDMODUL                              
322600        MOVE PLATS-ADVMODUL   TO WS-ADVMODUL                              
322700        MOVE PLATS-ADHMODUL   TO WS-ADHMODUL                              
322800        MOVE PLATS-FLUTLAST   TO WS-FLUTLAST                              
322900        MOVE PLATS-IDDC-CROSS TO WS-IDDC-CROSS                            
323000        EVALUATE TRUE                                                     
323100          WHEN DCS-CDC OR (DCS-SDC AND DCS-IDLANDX2 = 'SE')               
323200            STRING 'KOLLIT UPPD, NOTERA PLATS: ' ARB-ADRESS               
323300                    DELIMITED BY SIZE INTO MOD-TEMFSINF                   
323400            STRING 'FORTSÄTT REGISTRERA,DÄREFTER ENTER '                  
323500                   ' NOTERA PLATS: ' ARB-ADRESS                           
323600                    DELIMITED BY SIZE INTO WS-TEMFSINF                    
323700          WHEN OTHER                                                      
323800            STRING 'CASE UPDATED. ' ARB-ADRESS                            
323900                    DELIMITED BY SIZE INTO MOD-TEMFSINF                   
324000            STRING 'CONTINUE REPORT, THEN PRESS ENTER '                   
324100                    ARB-ADRESS                                            
324200                    DELIMITED BY SIZE INTO WS-TEMFSINF                    
324300        END-EVALUATE                                                      
324400     ELSE                                                                 
324500        MOVE LOW-VALUE            TO MOD-TEMFSINF                         
324600        IF    WS-ADFLOMR  = ZERO                                          
324700           OR WS-ADRUTNIV = ZERO                                          
324800           OR WS-ADFLGEO  = SPACE                                         
324900           MOVE FEL               TO WS-INDATA-TEST                       
325000           MOVE FEL-730 (INDX)    TO MOD-TEMFSFEL                         
325100           MOVE MFS-NUM-FAELT-FEL TO MOD-ADFLGEO-ATTR                     
325200        ELSE                                                              
325300           MOVE FEL               TO WS-INDATA-TEST                       
325400           MOVE FEL-729 (INDX)    TO MOD-TEMFSFEL                         
325500           MOVE MFS-NUM-FAELT-FEL TO MOD-ADFLGEO-ATTR                     
325600        END-IF                                                            
325700     END-IF                                                               
325800     .                                                                    
325900     SKIP3                                                                
326000 CG-OVRIG-KONTROLL      SECTION.                                          
326100*DIRLEV DC11                                                              
326200     IF VORD-IDDC NOT = W-IDDC-B6                                         
326300        MOVE VORD-IDDC TO W-IDDC-B6                                       
326400        PERFORM IMS-GU-WDB601                                             
326500     END-IF                                                               
326600     IF VORD-FLDIRLEV = NEJ OR DCS-CDC                                    
326700       CONTINUE                                                           
326800     ELSE                                                                 
326900       IF WS-INDATA-RATT                                                  
327000         MOVE FEL-800 (INDX)     TO MOD-TEMFSFEL                          
327100         MOVE FEL                TO WS-INDATA-TEST                        
327200         MOVE MFS-RENSA-FAELT    TO MOD-TEMFSINF                          
327300       END-IF                                                             
327400     END-IF                                                               
327500                                                                          
327600     MOVE WS-KORD-IDORDER        TO W-201-IDORDER                         
327700                                                                          
327800     PERFORM IMS-GU-ORQI01                                                
327900     .                                                                    
328000     SKIP3                                                                
328100 D-LAGG-UPP-KOLLI-SEG   SECTION.                                          
328200                                                                          
328300     IF KOLLI-INTERVALL                                                   
328400       MOVE +1                         TO INX                             
328500       MOVE WS-IDKOLLI-FOM             TO WS-IDKOLLI-NUM                  
328510                                          W-611-IDKOLLI                   
328600       PERFORM UNTIL INX NOT < ARB-ANTAL-KOLLI-PLUS-1                     
328700         PERFORM S09-SKAPA-KOLLI-SEGMENT                                  
328800                                                                          
328900         PERFORM IMS-ISRT-KOLLI                                           
329000         IF SEGMENT-FINNS-REDAN                                           
329100             MOVE FEL                  TO WS-BEHANDLING-TEST              
329200             MOVE FEL-721 (INDX)       TO MOD-TEMFSFEL                    
329210         ELSE                                                             
329220            IF  WS-IDDC-CROSS > SPACES                                    
329230              MOVE W-KDSEGKEY-X      TO CROSS-KDSEGKEY                    
329240              MOVE PLATS-IDDC        TO CROSS-IDDC-SEND                   
329250              MOVE WS-IDDC-CROSS     TO CROSS-IDDC-CROSS                  
329260              MOVE KOLLI-IDDISTR     TO CROSS-IDDISTR                     
329270              MOVE KOLLI-IDKUNDNR    TO CROSS-IDKUNDNR                    
329280              MOVE WS-IDPRODNR       TO CROSS-IDPRODNR                    
329290              MOVE WS-IDKOLLI-NUM    TO CROSS-IDKOLLI                     
329291              MOVE KOLLI-IDLEVNR     TO CROSS-IDLEVNR                     
329292              MOVE KOLLI-IDSUPREF    TO CROSS-IDSUPREF                    
329293              MOVE KOLLI-DARFS(3:6)  TO CROSS-TIRFSDAT                    
329294              MOVE 1                 TO  CROSS-KDKOLSTA-CROSS             
329295              MOVE ZERO         TO  CROSS-IDTRPTNR-CROSS                  
329296              MOVE ZERO         TO  CROSS-TIRECXDAT                       
329297              MOVE ZERO         TO  CROSS-TIRECXTID                       
329298              MOVE ZERO         TO  CROSS-TISKEPPN                        
329299              MOVE ZERO         TO  CROSS-IDSHIPM-CROSS                   
329300              MOVE SPACE        TO  CROSS-IDLBBET-CROSS                   
329301              PERFORM IMS-ISRT-WDE621                                     
329302            END-IF                                                        
329310         END-IF                                                           
329500         ADD 1                       TO WS-IDKOLLI-NUM                    
329510                                        W-611-IDKOLLI                     
329600                                        INX                               
329700       END-PERFORM                                                        
329800     ELSE                                                                 
329900       MOVE WS-IDKOLLI                 TO WS-IDKOLLI-NUM                  
330000       PERFORM S09-SKAPA-KOLLI-SEGMENT                                    
330100                                                                          
330200       PERFORM IMS-ISRT-KOLLI                                             
330300                                                                          
330400       IF SEGMENT-FINNS-REDAN                                             
330500           MOVE FEL                    TO WS-BEHANDLING-TEST              
330600           MOVE FEL-721 (INDX)         TO MOD-TEMFSFEL                    
330610       ELSE                                                               
330620           IF WS-IDDC-CROSS > SPACES                                      
330621             MOVE W-KDSEGKEY-X       TO CROSS-KDSEGKEY                    
330622             MOVE PLATS-IDDC         TO CROSS-IDDC-SEND                   
330623             MOVE WS-IDDC-CROSS      TO CROSS-IDDC-CROSS                  
330624             MOVE KOLLI-IDDISTR      TO CROSS-IDDISTR                     
330625             MOVE KOLLI-IDKUNDNR     TO CROSS-IDKUNDNR                    
330626             MOVE WS-IDPRODNR        TO CROSS-IDPRODNR                    
330627             MOVE WS-IDKOLLI-NUM     TO CROSS-IDKOLLI                     
330628             MOVE KOLLI-IDLEVNR      TO CROSS-IDLEVNR                     
330629             MOVE KOLLI-IDSUPREF     TO CROSS-IDSUPREF                    
330630             MOVE KOLLI-DARFS(3:6)   TO CROSS-TIRFSDAT                    
330631             MOVE 1                  TO  CROSS-KDKOLSTA-CROSS             
330632             MOVE ZERO          TO  CROSS-IDTRPTNR-CROSS                  
330633             MOVE ZERO          TO  CROSS-TIRECXDAT                       
330634             MOVE ZERO          TO  CROSS-TIRECXTID                       
330635             MOVE ZERO          TO  CROSS-TISKEPPN                        
330636             MOVE ZERO          TO  CROSS-IDSHIPM-CROSS                   
330637             MOVE SPACE         TO  CROSS-IDLBBET-CROSS                   
330638             PERFORM IMS-ISRT-WDE621                                      
330699           END-IF                                                         
330700       END-IF                                                             
330800       MOVE WS-IDDISTR-NUM TO TEST-IDDISTR                                
330900     END-IF                                                               
331000     .                                                                    
331100     EJECT                                                                
331200*                                                                         
331300 F-BEHANDLA-RADER     SECTION.                                            
331400                                                                          
331500     MOVE MID-IDRADNR-FOM (INX)         TO   ARB-RAD-FOM                  
331600     MOVE MID-IDRADNR-TOM (INX)         TO   ARB-RAD-TOM                  
331700                                                                          
331800     IF ARB-RAD-TOM                     =    ZERO                         
331900*-------------------------------------------ÄR DET EN DELAD RAD           
332000*-------------------------------------------ELLER EN AVVIKELSERAD         
332100         MOVE NEJ                       TO  FL-RAD-INTERVALL              
332200         MOVE ARB-RAD-FOM               TO  WS-START-RAD                  
332300                                            WS-SISTA-RAD                  
332400     ELSE                                                                 
332500         MOVE JA                        TO  FL-RAD-INTERVALL              
332600         COMPUTE WS-ANT-RADER-INT = ARB-RAD-TOM - ARB-RAD-FOM + 1         
332700         END-COMPUTE                                                      
332800         MOVE ARB-RAD-TOM               TO  WS-SISTA-RAD                  
332900         MOVE ARB-RAD-FOM               TO  WS-START-RAD                  
333000     END-IF                                                               
333100                                                                          
333200     IF  WS-SAMMA-BILD                                                    
333300         MOVE 'N'                       TO WS-RADER-OK                    
333400         MOVE 'N'                       TO WS-TRAEFF-RAD                  
333500*                                                                         
333600         MOVE WS-IDDISTR-NUM            TO W-4A1-IDDISTR                  
333700         MOVE WS-IDKUNDNR-NUM           TO W-4A1-IDKUNDNR                 
333800         MOVE WS-IDORDNR                TO W-4A1-IDORDNR                  
333900         PERFORM IMS-GU-KUNDORDER-SEK                                     
334000*                                                                         
334100         IF KUNDORDER-SEK-FINNS                                           
334200                                                                          
334300            PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                         
334400                          WS-RADER-OK = 'J'                               
334500              MOVE KORD-IDDISTR           TO W-401-IDDISTR                
334600              MOVE KORD-IDKUNDNR          TO W-401-IDKUNDNR               
334700              MOVE KORD-IDORDNR5          TO W-401-IDORDNR                
334800              MOVE KORD-IDPRODNR          TO W-401-IDPRODNR               
334900              MOVE KORD-IDPLKLST          TO W-401-IDPLKLST               
335000                                             WS-IDPLKLST                  
335100              PERFORM IMS-GU-WDE401                                       
335200              MOVE KORD-IDPRODNR          TO WS-JFR-IDPRODNR              
335300              MOVE KORD-IDUSER            TO WS-JFR-IDANSTNR              
335400                                                                          
335500              MOVE KORD-IDORDER           TO WS-SPAR-IDORDER              
335600              MOVE KORD-IDDC              TO WS-SPAR-IDDC                 
335700                                                                          
335800              IF WS-IDPRODNR = WS-JFR-IDPRODNR                            
335900                 IF MID-KVLEVART (INX) NOT NUMERIC                        
336000                    MOVE ZERO          TO   ARB-KVLEVART                  
336100                                            WS-KVLEVART                   
336200                 ELSE                                                     
336300                    MOVE MID-KVLEVART (INX) TO   ARB-KVLEVART             
336400                                                 WS-KVLEVART              
336500                 END-IF                                                   
336600                                                                          
336700                 PERFORM FA-KONTROLLERA-RADER                             
336800                                                                          
336900                 IF KOLLI-INTERVALL AND INX = +1 AND                      
337000                    WS-TRAEFF-RAD = JA                                    
337100                    PERFORM FC-KOLLA-ANTAL-PER-KOLLI                      
337200                 END-IF                                                   
337300              END-IF                                                      
337400              PERFORM IMS-GN-SEQA-WDE4A1                                  
337500            END-PERFORM                                                   
337600                                                                          
337700                                                                          
337800            IF WS-TRAEFF-RAD = 'N'                                        
337900              MOVE FEL               TO WS-BEHANDLING-TEST                
338000              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-FOM-ATTR (INX)        
338100              MOVE FEL-722 (INDX)    TO MOD-TEMFSFEL                      
338200                                                                          
338300            END-IF                                                        
338400         END-IF                                                           
338500     ELSE                                                                 
338600          IF MID-KVLEVART (INX) NOT NUMERIC                               
338700               MOVE ZERO          TO   ARB-KVLEVART                       
338800                                       WS-KVLEVART                        
338900          ELSE                                                            
339000               MOVE MID-KVLEVART (INX) TO   ARB-KVLEVART                  
339100                                       WS-KVLEVART                        
339200          END-IF                                                          
339300     END-IF                                                               
339400                                                                          
339500     IF WS-BEHANDLING-RATT                                                
339600       PERFORM FB-BEHANDLA-RAD-INOM-INTERVALL                             
339700                                                                          
339800                                                                          
339900       IF WS-BEHANDLING-RATT                                              
340000          MOVE WS-MOD-VKORDBTO        TO PLATS-VKORDNTO-KOLLI             
340100          MOVE WS-IDDISTR-NUM TO TEST-IDDISTR                             
340200          IF NOT DIST19-SATS                                              
340300             PERFORM FD-UPPDATERA-PRODTAB                                 
340400          END-IF                                                          
340500          IF NOT WS-ORDERVIS-TRANS                                        
340600             IF MID-IDRADNR-FOM (12) NOT = ALL '+'                        
340700                PERFORM FE-KOLLILAASNING                                  
340800             END-IF                                                       
340900          END-IF                                                          
341000       END-IF                                                             
341100     END-IF                                                               
341200     .                                                                    
341300     EJECT                                                                
341400 FA-KONTROLLERA-RADER SECTION.                                            
341500     SKIP3                                                                
341600     MOVE 'N'                         TO WS-RADER-OK                      
341700     MOVE 'N'                         TO FL-SLINGA-KLAR                   
341800     MOVE ARB-RAD-FOM                 TO ARB-RAD-AKTUELL                  
341900     MOVE ARB-RAD-AKTUELL             TO W-420-IDPURAD2                   
342000*                                                                         
342100     IF NOT WS-ORDERVIS-TRANS AND                                         
342200        WS-IDANSTNR = WS-JFR-IDANSTNR-5 OR                                
342300        WS-ORDERVIS-TRANS                                                 
342400                                                                          
342500        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
342600                      SLINGA-KLAR                                         
342700          PERFORM IMS-GU-RAD                                              
342800          IF SEGMENT-SAKNAS                                               
342900            MOVE 'J'               TO FL-SLINGA-KLAR                      
343000          ELSE                                                            
343100            MOVE ZERO              TO WS-ACC-ORAD-VKORDNTO                
343200*                                                                         
343300            IF WS-KVLEVART        = ZERO                                  
343400              COMPUTE WS-ACC-ORAD-VKORDNTO ROUNDED =                      
343500              ORAD-VKART-NTO-KG * (ORAD-KVAVBART - ORAD-KVLEVART)         
343600              END-COMPUTE                                                 
343700            ELSE                                                          
343800                                                                          
343900              COMPUTE WS-ACC-ORAD-VKORDNTO ROUNDED =                      
344000                   ORAD-VKART-NTO-KG *  WS-KVLEVART                       
344100              END-COMPUTE                                                 
344200            END-IF                                                        
344300            ADD WS-ACC-ORAD-VKORDNTO TO ACC-ORAD-VKORDNTO                 
344400*                                                                         
344500            MOVE 'J'                  TO WS-TRAEFF-RAD                    
344600            IF NOT WS-ORDERVIS-TRANS AND                                  
344700               WS-JFR-IDANSTNR-5 = '00000'                                
344800               MOVE 'J'               TO FL-SLINGA-KLAR                   
344900               MOVE FEL               TO WS-BEHANDLING-TEST               
345000               MOVE MFS-NUM-FAELT-FEL TO                                  
345100                    MOD-IDRADNR-FOM-ATTR (INX)                            
345200               MOVE FEL-716 (INDX)    TO MOD-TEMFSFEL                     
345300            ELSE                                                          
345400              IF ORAD-KDRADSTA > 3                                        
345500                 MOVE 'J'               TO FL-SLINGA-KLAR                 
345600                 MOVE FEL               TO WS-BEHANDLING-TEST             
345700                 MOVE MFS-NUM-FAELT-FEL TO                                
345800                      MOD-IDRADNR-FOM-ATTR (INX)                          
345900                 MOVE FEL-723 (INDX)    TO MOD-TEMFSFEL                   
346000              ELSE                                                        
346100                IF ORAD-FLNOLLJ = JA                                      
346200                   MOVE 'J'               TO FL-SLINGA-KLAR               
346300                   MOVE FEL               TO WS-BEHANDLING-TEST           
346400                   MOVE MFS-NUM-FAELT-FEL TO                              
346500                        MOD-IDRADNR-FOM-ATTR (INX)                        
346600                   MOVE FEL-830 (INDX)    TO MOD-TEMFSFEL                 
346700                END-IF                                                    
346800              END-IF                                                      
346900            END-IF                                                        
347000            MOVE ORAD-IDARTNR       TO W-IDARTNR                          
347100                                                                          
347200            IF LINE-IX <= MAX-LINES                                       
347300              IF CDC                                                      
347400                PERFORM IMS-GU-WDK611                                     
347500                MOVE CLAG-ADGANG     TO LINE1-ADGANG  (LINE-IX)           
347600                MOVE CLAG-ADPLATS    TO LINE1-ADPLATS (LINE-IX)           
347700                MOVE CLAG-ADLAGOMR   TO LINE1-ADLAGOMR(LINE-IX)           
347800              ELSE                                                        
347900                PERFORM IMS-GU-WDK711                                     
348000                MOVE SLAG-ADGANG     TO LINE1-ADGANG  (LINE-IX)           
348100                MOVE SLAG-ADPLATS    TO LINE1-ADPLATS (LINE-IX)           
348200                MOVE SLAG-ADLAGOMR   TO LINE1-ADLAGOMR(LINE-IX)           
348300              END-IF                                                      
348400                                                                          
348500              IF WS-KVLEVART      = ZERO                                  
348600                MOVE ORAD-KVAVBART   TO LINE1-KVAVBART(LINE-IX)           
348700              ELSE                                                        
348800                MOVE WS-KVLEVART     TO LINE1-KVAVBART(LINE-IX)           
348900              END-IF                                                      
349000              MOVE ARB-RAD-AKTUELL   TO LINE1-NUMBER (LINE-IX)            
349100              MOVE LINE-IX           TO MAX-TAB                           
349200                                                                          
349300              MOVE ORAD-IDARTNR      TO LINE1-IDARTNR (LINE-IX)           
349400              MOVE ORAD-VKARTNTO     TO LINE1-VKOLDNET(LINE-IX)           
349500              MOVE ORAD-VKART-NTO-KG TO LINE1-VKNEWNET(LINE-IX)           
349600              ADD 1 TO  LINE-IX                                           
349700            END-IF                                                        
349800            MOVE LINE-IX             TO MAX-RAD                           
349900                                                                          
350000          END-IF                                                          
350100                                                                          
350200          IF SEGMENT-FINNS AND                                            
350300             WS-TRAEFF-RAD = 'J'                                          
350400             COMPUTE ARB-RAD-AKTUELL = ARB-RAD-AKTUELL + 1                
350500             END-COMPUTE                                                  
350600             IF ARB-RAD-AKTUELL > ARB-RAD-TOM                             
350700                MOVE 'J' TO FL-SLINGA-KLAR                                
350800                MOVE 'J' TO WS-RADER-OK                                   
350900             ELSE                                                         
351000                MOVE ARB-RAD-AKTUELL TO W-420-IDPURAD2                    
351100             END-IF                                                       
351200          END-IF                                                          
351300                                                                          
351400          IF SEGMENT-SAKNAS AND                                           
351500             WS-TRAEFF-RAD = 'J'                                          
351600             IF ARB-RAD-AKTUELL NOT > ARB-RAD-TOM                         
351700                MOVE 'J'               TO FL-SLINGA-KLAR                  
351800                MOVE FEL               TO WS-BEHANDLING-TEST              
351900                MOVE MFS-NUM-FAELT-FEL TO                                 
352000                     MOD-IDRADNR-TOM-ATTR (INX)                           
352100                MOVE FEL-7384(INDX)    TO MOD-TEMFSFEL                    
352200                MOVE SPACE             TO MOD-TEMFSINF                    
352300             END-IF                                                       
352400          END-IF                                                          
352500        END-PERFORM                                                       
352600     END-IF                                                               
352700     .                                                                    
352800     EJECT                                                                
352900 I-KONTROLLERA-WEIGHT SECTION.                                            
353000                                                                          
353100     IF WS-MOD-VKORDBTO > ZERO                                            
353200       IF KOLLI-INTERVALL                                                 
353300         COMPUTE ONE-CASE-VKORDBTO ROUNDED =                              
353400                 ACC-ORAD-VKORDNTO / ARB-ANTAL-KOLLI                      
353500         END-COMPUTE                                                      
353600                                                                          
353700         COMPUTE WS-VKORDBTO-TOT ROUNDED =                                
353800                                   ONE-CASE-VKORDBTO                      
353900         END-COMPUTE                                                      
354000                                                                          
354100         IF WS-MOD-VKORDBTO < ONE-CASE-VKORDBTO                           
354200                                                                          
354300           MOVE FEL                 TO WS-BEHANDLING-TEST                 
354400           MOVE INF-WEIGHT-NOT-LESS-THAN TO MOD-TEMFSINF                  
354500           MOVE 'J' TO WS-WEIGHT                                          
354600           MOVE WS-VKORDBTO-TOT     TO HEAD-VKARTNTO                      
354700           IF US-MEASUREMENT                                              
354800             COMPUTE WS-VKORDBTO-TOT ROUNDED =                            
354900                  ONE-CASE-VKORDBTO * CONV-KG-TO-LB                       
355000             END-COMPUTE                                                  
355100             MOVE 'LBS'           TO MOD-TEMFSINF(41:3)                   
355200           ELSE                                                           
355300             MOVE 'KG'            TO MOD-TEMFSINF(41:2)                   
355400           END-IF                                                         
355500                                                                          
355600          INSPECT WS-VKORDBTO-TOT REPLACING LEADING ZERO BY SPACE         
355700                                                                          
355800           MOVE WS-VKORDBTO-TOT   TO MOD-TEMFSINF(29:10)                  
355900           MOVE MFS-NUM-FAELT-FEL TO MOD-VKORDBTO-ATTR                    
356000           MOVE MFS-ROER-EJ-FAELT TO MOD-VKORDBTO-KOLLI                   
356100           PERFORM S23-MOVE-LINEDATA                                      
356200         END-IF                                                           
356300                                                                          
356400       ELSE                                                               
356500         COMPUTE WS-VKORDBTO-TOT  ROUNDED =                               
356600                                   ACC-ORAD-VKORDNTO                      
356700         END-COMPUTE                                                      
356800         IF WS-MOD-VKORDBTO < ACC-ORAD-VKORDNTO                           
356900           MOVE FEL                   TO WS-BEHANDLING-TEST               
357000           MOVE INF-WEIGHT-NOT-LESS-THAN TO MOD-TEMFSINF                  
357100           MOVE 'J' TO WS-WEIGHT                                          
357200           MOVE WS-VKORDBTO-TOT     TO HEAD-VKARTNTO                      
357300                                                                          
357400           IF US-MEASUREMENT                                              
357500             COMPUTE WS-VKORDBTO-TOT ROUNDED =                            
357600                    ACC-ORAD-VKORDNTO * CONV-KG-TO-LB                     
357700             END-COMPUTE                                                  
357800             MOVE 'LBS'             TO MOD-TEMFSINF(41:3)                 
357900           ELSE                                                           
358000             MOVE 'KG'              TO MOD-TEMFSINF(41:2)                 
358100           END-IF                                                         
358200                                                                          
358300          INSPECT WS-VKORDBTO-TOT REPLACING LEADING ZERO BY SPACE         
358400                                                                          
358500           MOVE WS-VKORDBTO-TOT   TO MOD-TEMFSINF(29:10)                  
358600           MOVE MFS-NUM-FAELT-FEL TO MOD-VKORDBTO-ATTR                    
358700           MOVE MFS-ROER-EJ-FAELT TO MOD-VKORDBTO-KOLLI                   
358800           PERFORM S23-MOVE-LINEDATA                                      
358900         END-IF                                                           
359000       END-IF                                                             
359100     END-IF                                                               
359200     .                                                                    
359300     EJECT                                                                
359400 FB-BEHANDLA-RAD-INOM-INTERVALL SECTION.                                  
359500     SKIP3                                                                
359600     MOVE ZERO                   TO  WS-RINT-ANT-FPACK-ORAD               
359700                                                                          
359800     MOVE WS-IDPRODNR            TO  W-420-IDPRODNR-MIN                   
359900                                     W-420-IDPRODNR-MAX                   
360000                                     W-420-IDPRODNR                       
360100     MOVE WS-START-RAD           TO  W-420-IDPURAD-MIN                    
360200                                     W-420-IDPURAD                        
360300                                     WS-AKTUELL-RAD                       
360400     MOVE WS-SISTA-RAD           TO  W-420-IDPURAD-MAX                    
360500     MOVE JA                     TO  FL-RAD-INOM-INTERVALL                
360600                                                                          
360700     PERFORM IMS-GHU-RAD-SEK                                              
360800                                                                          
360900     PERFORM UNTIL (NOT RAD-FINNS-I-INTERVALL)                            
361000               OR  (NOT WS-BEHANDLING-RATT)                               
361100                                                                          
361200      PERFORM FBA-SPARA-RAD-INFO                                          
361300      IF WS-BEHANDLING-RATT                                               
361400                                                                          
361500                                                                          
361600       PERFORM FBB-UPPDATERA-RAD                                          
361700       IF WS-BEHANDLING-RATT                                              
361800         IF KOLLI-INTERVALL                                               
361900           MOVE 1                        TO   ACK-KOLLI                   
362000           MOVE WS-IDKOLLI-FOM           TO   WS-IDKOLLI-NUM              
362100           PERFORM UNTIL ACK-KOLLI NOT < ARB-ANTAL-KOLLI-PLUS-1           
362200               MOVE WS-IDKOLLI-NUM       TO   WS-IDKOLLI                  
362300               PERFORM FBC-LAGG-UPP-KOLLI-KOPPL                           
362400               ADD 1                     TO   ACK-KOLLI                   
362500                                            WS-IDKOLLI-NUM                
362600           END-PERFORM                                                    
362700           COMPUTE ARB-KOLLI-KVORDRAD                                     
362800                =    ARB-KOLLI-KVORDRAD                                   
362900                / (ARB-ANTAL-KOLLI-PLUS-1 - 1)                            
363000                                                                          
363100           COMPUTE ARB-KOLLI-KVFALRAD                                     
363200                =    ARB-KOLLI-KVFALRAD                                   
363300                / (ARB-ANTAL-KOLLI-PLUS-1 - 1)                            
363400         ELSE                                                             
363500           PERFORM FBC-LAGG-UPP-KOLLI-KOPPL                               
363600         END-IF                                                           
363700                                                                          
363800                                                                          
363900         ADD 1 TO WS-AKTUELL-RAD                                          
364000         IF WS-AKTUELL-RAD > WS-SISTA-RAD                                 
364100             MOVE NEJ TO FL-RAD-INOM-INTERVALL                            
364200         ELSE                                                             
364300             PERFORM IMS-GHN-RAD-SEK                                      
364400         END-IF                                                           
364500       END-IF                                                             
364600      END-IF                                                              
364700     END-PERFORM                                                          
364800     .                                                                    
364900     EJECT                                                                
365000 FBA-SPARA-RAD-INFO        SECTION.                                       
365100     MOVE ORAD-VKARTNTO         TO  SPAR-PRAD-VKARTNTO                    
365200     MOVE ORAD-KVFLAMP          TO  SPAR-PRAD-KVFLAMP                     
365300     MOVE ORAD-KDFARLIG         TO  SPAR-PRAD-KDFARLIG                    
365400     MOVE ORAD-PRARTNTO         TO  SPAR-PRAD-PRARTNTO                    
365500     MOVE ORAD-PRAVCOST         TO  SPAR-PRAD-PRAVCOST                    
365600     MOVE ORAD-PRARTNTO-LOC     TO  SPAR-PRAD-PRARTNTO-LOC                
365700     MOVE ORAD-PRARTNTO-LOCPREL TO  SPAR-PRAD-PRARTNTO-LOCPREL            
365800     MOVE ORAD-KDVALISO         TO  SPAR-PRAD-KDVALISO                    
365900     MOVE ORAD-KDVALISO-EXP     TO  SPAR-PRAD-KDVALISO-EXP                
366000*                                                                         
366100     MOVE ORAD-IDPSN            TO SPAR-IDPSN                             
366200     MOVE ORAD-VKART-FG         TO SPAR-VKART-FG                          
366300     MOVE ORAD-VLFG             TO SPAR-VLFG                              
366400     MOVE ORAD-SUEQFG           TO SPAR-SUEQFG                            
366500*                                                                         
366600     MOVE ORAD-IDARTNR          TO WS-SPAR-IDARTNR                        
366700     MOVE ORAD-BEART            TO WS-SPAR-BEART                          
366800     MOVE ORAD-KVBEART          TO WS-SPAR-KVBEART                        
366900     MOVE ORAD-FLTILLK          TO WS-SPAR-FLTILLK                        
367000     MOVE ORAD-IDKUNDRF-RO      TO WS-SPAR-IDKUNDRF-RO                    
367100     MOVE ORAD-IDPURAD          TO WS-SPAR-IDPURAD                        
367200*                                                                         
367300     IF WS-KVLEVART             >   ORAD-KVAVBART                         
367400         MOVE FEL               TO  WS-BEHANDLING-TEST                    
367500         MOVE MFS-NUM-FAELT-FEL TO  MOD-KVLEVART-ATTR (INX)               
367600         MOVE FEL-725A (INDX)   TO  MOD-TEMFSFEL                          
367700     ELSE                                                                 
367800         IF KOLLI-INTERVALL                                               
367900             MOVE ARB-KVLEVART  TO  SPAR-PRAD-KVLEVART                    
368000         ELSE                                                             
368100             IF WS-KVLEVART     =   ZERO                                  
368200               IF ORAD-KVLEVART > ZERO                                    
368300*                * RAD TIDIGARE DELRAPPORTERAD, LEVERERAT I DENNA         
368400*                * OMGÅNG SÄTTS SÅ ATT AVBOKAT UPPNÅS.                    
368500                 COMPUTE SPAR-PRAD-KVLEVART                               
368600                   = ORAD-KVAVBART - ORAD-KVLEVART                        
368700                 END-COMPUTE                                              
368800*PGA SATSORDERPROBLEM 031008                                              
368900                 IF  SPAR-PRAD-KVLEVART = ZERO                            
369000                 AND DIST19-SATS                                          
369100                 AND WS-ORDERVIS-TRANS                                    
369200                 AND ORAD-KVAVBART = ORAD-KVLEVART                        
369300                   MOVE ORAD-KVLEVART TO SPAR-PRAD-KVLEVART               
369400                 END-IF                                                   
369500*PGA SATSORDERPROBLEM                                                     
369600               ELSE                                                       
369700                 MOVE ORAD-KVAVBART TO SPAR-PRAD-KVLEVART                 
369800               END-IF                                                     
369900             ELSE                                                         
370000                 MOVE WS-KVLEVART TO SPAR-PRAD-KVLEVART                   
370100             END-IF                                                       
370200         END-IF                                                           
370300*                                                                         
370400*    OBS OBS OBS                                                          
370500*    ÖPPNA DENNA IF-SATS VID TESTER I BTS OCH                             
370600*    STÄNG MOTSV. I STYR-SECTION                                          
370700*   (BTS KLARAR EJ AV ROLL-BACK)                                          
370800*        PERFORM D-LAGG-UPP-KOLLI-SEG                                     
370900*    SLUT BTS-SPECIAL                                                     
371000         PERFORM S10-UPPD-SPAR-KOLLI                                      
371100     END-IF                                                               
371200     .                                                                    
371300     EJECT                                                                
371400 FBB-UPPDATERA-RAD         SECTION.                                       
371500                                                                          
371600     IF ORAD-IDLEVNR NOT = SPACE                                          
371700       MOVE JA                  TO DIRLEV-KOLLI-SW                        
371800     END-IF                                                               
371900*                                                                         
372000*               STRING '*' DIRLEV-KOLLI-SW                                
372100*               DELIMITED BY SIZE INTO MOD-TEMFSFEL                       
372200                                                                          
372300     IF  KOLLI-INTERVALL                                                  
372400         COMPUTE ORAD-KVLEVART = ORAD-KVLEVART                            
372500                               + SPAR-PRAD-KVLEVART                       
372600                               * (ARB-ANTAL-KOLLI-PLUS-1 - 1)             
372700     ELSE                                                                 
372800       IF ORAD-KVLEVART + SPAR-PRAD-KVLEVART > ORAD-KVAVBART              
372900                                                                          
373000         IF  DIST19-SATS                                                  
373100         AND WS-ORDERVIS-TRANS                                            
373200           MOVE ORAD-KVAVBART TO ORAD-KVLEVART                            
373300         ELSE                                                             
373400           MOVE FEL               TO  WS-BEHANDLING-TEST                  
373500           MOVE MFS-NUM-FAELT-FEL TO  MOD-KVLEVART-ATTR (INX)             
373600           MOVE FEL-725B (INDX)   TO  MOD-TEMFSFEL                        
373700         END-IF                                                           
373800       ELSE                                                               
373900         COMPUTE ORAD-KVLEVART = ORAD-KVLEVART                            
374000                               + SPAR-PRAD-KVLEVART                       
374100       END-IF                                                             
374200     END-IF                                                               
374300                                                                          
374400     IF WS-BEHANDLING-RATT                                                
374500       IF ORAD-KVLEVART = ORAD-KVAVBART                                   
374600          MOVE +4    TO ORAD-KDRADSTA                                     
374700          ADD 1 TO WS-TOT-ANT-RADER                                       
374800          ADD 1 TO  WS-RINT-ANT-FPACK-ORAD                                
374900                                                                          
375000          IF KORD-KDORDKL = +0                                            
375100             PERFORM FBBB-UPPDATERA-VOR-TIKLAR                            
375200          END-IF                                                          
375300       END-IF                                                             
375400*                                                                         
375500       PERFORM IMS-REPL-BEHANDLAD-RAD                                     
375600       PERFORM FBBA-EV-SKAPA-RYK-TRANS                                    
375700     END-IF                                                               
375800     .                                                                    
375900     EJECT                                                                
376000 FBBA-EV-SKAPA-RYK-TRANS SECTION.                                         
376100                                                                          
376200     IF ORAD-IDKUNDRF-RO NOT = '00000     ' AND                           
376300        ORAD-TIRODAT         > ZERO                                       
376400                                                                          
376500       IF LOGG-IDLOGLOP = 9                                               
376600         MOVE ZERO                TO   LOGG-IDLOGLOP                      
376700       END-IF                                                             
376800                                                                          
376900       ACCEPT  LOGG-TIAAMMDD      FROM DATE                               
377000       ACCEPT  LOGG-TIKLOCK       FROM TIME                               
377100       ADD     +1                 TO   LOGG-IDLOGLOP                      
377200                                                                          
377300       MOVE    WS-IDDISTR-NUM     TO   RYK-IDDISTR                        
377400                                       W-WDQ2C-IDDISTR                    
377500       MOVE    WS-IDKUNDNR-NUM    TO   RYK-IDKUNDNR                       
377600                                       W-WDQ2C-IDKUNDNR                   
377700       MOVE    ORAD-IDKUNDRF-RO(1:5)                                      
377800                                  TO   W-WDQ2C-IDORDNR5                   
377900                                                                          
378000*****  FIX-START-DEL1 930303 FÖR ATT TA HAND OM EN ORDER SOM              
378100*      SAKNAR ORDERHUVUD (WDQ2) OBS, VID ANVÄNDANDE ÖPPNA OCKSÅ           
378200*      FIX-DEL2 I SECTION S13.                                            
378300       PERFORM IMS-GET-ORQI01-CSEQ                                        
378400       IF SEGMENT-FINNS                                                   
378500          MOVE OHUV-IDORDER       TO   RYK-IDORDER                        
378600       ELSE                                                               
378700          MOVE +0                 TO   RYK-IDORDER                        
378800       END-IF                                                             
378900*****  FIX-END-DEL1 930303                                                
379000       MOVE    'RYK'              TO   RYK-IDPTYP                         
379100                                       LOGG-IDPTYP                        
379200                                                                          
379300       MOVE    ORAD-IDARTNR       TO   RYK-IDARTNR                        
379400       MOVE    LOGG-TIAAMMDD      TO   RYK-TIRODAT                        
379500       MOVE    ORAD-KVLEVART      TO   RYK-KVLEVART                       
379600       MOVE    ORAD-KVBEART       TO   RYK-KVBEART-Q                      
379700       MOVE    ORAD-KDORDKL       TO   RYK-KDORDKL                        
379800       MOVE    ORAD-KDPRODSL      TO   RYK-KDPRODSL                       
379900       MOVE    ZERO               TO   RYK-KDORDBEK                       
380000                                                                          
380100       MOVE    SPACE              TO   LOGG-SORTPOST                      
380200       MOVE    RYK-WDGZRYK        TO   LOGG-LOGGPOST                      
380300                                                                          
380400       PERFORM IMS-ISRT-ZZAC01                                            
380500                                                                          
380600       PERFORM UNTIL SEGMENT-FINNS                                        
380700         IF LOGG-IDLOGLOP = 9                                             
380800           MOVE ZERO              TO LOGG-IDLOGLOP                        
380900           ACCEPT  LOGG-TIKLOCK   FROM TIME                               
381000         END-IF                                                           
381100         ADD +1                   TO LOGG-IDLOGLOP                        
381200         PERFORM IMS-ISRT-ZZAC01                                          
381300       END-PERFORM                                                        
381400     END-IF                                                               
381500     .                                                                    
381600     EJECT                                                                
381700 FBBB-UPPDATERA-VOR-TIKLAR SECTION.                                       
381800     SKIP3                                                                
381900                                                                          
382000     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
382100     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
382200     MOVE KORD-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
382300                                    W-A601KY-MAX-IDDISTR                  
382400     MOVE KORD-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
382500                                    W-A601KY-MAX-IDKUNDNR                 
382600     MOVE KORD-IDORDNR5          TO W-A601KY-MIN-IDORDNR                  
382700                                    W-A601KY-MAX-IDORDNR                  
382800                                                                          
382900     MOVE NEJ                    TO SW-TIKLAR-UPPDATERAD                  
383000                                                                          
383100     PERFORM IMS-GHN-WDA6B                                                
383200     PERFORM UNTIL SEGMENT-SAKNAS                                         
383300                OR END-OF-DATABASE                                        
383400                OR SW-TIKLAR-UPPDATERAD = JA                              
383500                                                                          
383600         IF  VOR-IDARTNR = ORAD-IDARTNR                                   
383700         AND VOR-TIKLAR = +0                                              
383800                                                                          
383900             MOVE WS-DAGENS-DATUM  TO VOR-TIKLAR                          
384000             MOVE WS-TIDPUNKT      TO WS-TIDPUNKT-RED                     
384100             MOVE WS-HHMMSS        TO VOR-TIKLATID                        
384200             PERFORM IMS-REPL-WDA6B                                       
384300             MOVE JA               TO SW-TIKLAR-UPPDATERAD                
384400         END-IF                                                           
384500                                                                          
384600         PERFORM IMS-GHN-WDA6B                                            
384700     END-PERFORM                                                          
384800     .                                                                    
384900     EJECT                                                                
385000 FBC-LAGG-UPP-KOLLI-KOPPL  SECTION.                                       
385100     SKIP3                                                                
385200     MOVE WS-IDPRODNR        TO KKOLLI-IDPRODNR                           
385300     MOVE WS-IDKOLLI         TO KKOLLI-IDKOLLI                            
385400     MOVE SPAR-PRAD-KVLEVART TO KKOLLI-KVLEVART                           
385500     PERFORM IMS-ISRT-KOLLI-KOPPL                                         
385600     IF SEGMENT-FINNS-REDAN                                               
385700         MOVE WS-IDPRODNR    TO  W-421-IDPRODNR                           
385800         MOVE WS-IDKOLLI     TO  W-421-IDKOLLI                            
385900         PERFORM IMS-GHNP-KOLLI-KOPPL                                     
386000         ADD SPAR-PRAD-KVLEVART  TO KKOLLI-KVLEVART                       
386100         PERFORM IMS-REPL-KOLLI-KOPPL                                     
386200     ELSE                                                                 
386300         ADD +1                  TO ARB-KOLLI-KVORDRAD                    
386400                                                                          
386500         IF SPAR-PRAD-KDFARLIG = +4                                       
386600         OR SPAR-PRAD-KDFARLIG = +7                                       
386700             ADD +1              TO ARB-KOLLI-KVFALRAD                    
386800         END-IF                                                           
386900     END-IF                                                               
387000*                                                                         
387100     IF WS-IDDC NOT = W-IDDC-B6                                           
387200        MOVE WS-IDDC TO W-IDDC-B6                                         
387300        PERFORM IMS-GU-WDB601                                             
387400     END-IF                                                               
387500     IF DCS-NDC-NA                                                        
387600        PERFORM S20-DATA-TILL-DEL-NOTE                                    
387700     END-IF                                                               
387800*                                                                         
387900     IF SPAR-IDPSN > ZERO                                                 
388000       PERFORM FBCA-SPARA-FG-DATA                                         
388100     END-IF                                                               
388200     .                                                                    
388300     EJECT                                                                
388400 FBCA-SPARA-FG-DATA SECTION.                                              
388500     SKIP3                                                                
388600     MOVE +1 TO FG-INDX                                                   
388700     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
388800                                                                          
388900       IF TAB-IDPSN(FG-INDX) = ZERO                                       
389000         MOVE SPAR-IDPSN TO TAB-IDPSN(FG-INDX)                            
389100         PERFORM S17-BERAEKNA-FG-FAELT                                    
389200                                                                          
389300       ELSE                                                               
389400         IF SPAR-IDPSN = TAB-IDPSN(FG-INDX)                               
389500           PERFORM S17-BERAEKNA-FG-FAELT                                  
389600         END-IF                                                           
389700       END-IF                                                             
389800                                                                          
389900       ADD +1 TO FG-INDX                                                  
390000     END-PERFORM                                                          
390100                                                                          
390200     COMPUTE TOTAL-SUEQFG = TOTAL-SUEQFG      +                           
390300                            (SPAR-SUEQFG      *                           
390400                             KKOLLI-KVLEVART)                             
390500     .                                                                    
390600     EJECT                                                                
390700 FC-KOLLA-ANTAL-PER-KOLLI  SECTION.                                       
390800                                                                          
390900     PERFORM S06-KOLLA-AVBOKAT-ANTAL                                      
391000*    IF ARB-KVLEVART = ZERO                                               
391100*        MOVE 15                       TO ARB-KVLEVART                    
391200*        MOVE FEL                      TO WS-INDATA-TEST                  
391300*        MOVE MFS-NUM-FAELT-FEL        TO MOD-KVLEVART-ATTR (INX)         
391400*    END-IF                                                               
391500     SKIP2                                                                
391600*-------------------------------------------ARB-KVLEVART ANVÄNDS          
391700*-------------------------------------------FÖR ANTAL/KOLLI MEDAN         
391800*-------------------------------------------WS-KVLEVART AVSER DET         
391900*-------------------------------------------TOTALA ANTALET/RAD            
392000     MOVE WS-KVLEVART                  TO   ARB-KVLEVART                  
392100     DIVIDE ARB-KVLEVART BY ARB-ANTAL-KOLLI                               
392200                               GIVING ARB-KVLEVART                        
392300                               REMAINDER WS-REST                          
392400     IF WS-REST > ZERO                                                    
392500*-------------------------------------------ÄR INTE ANTALET JÄMNT         
392600*-------------------------------------------DELBART I KOLLIINTERV.        
392700         MOVE FEL                      TO WS-BEHANDLING-TEST              
392800         MOVE FEL-735 (INDX)           TO MOD-TEMFSFEL                    
392900         MOVE MFS-NUM-FAELT-FEL        TO MOD-KVLEVART-ATTR (INX)         
393000     END-IF                                                               
393100     .                                                                    
393200     EJECT                                                                
393300 FD-UPPDATERA-PRODTAB      SECTION.                                       
393400     SKIP3                                                                
393500     MOVE WS-IDPRODNR            TO W-420-IDPRODNR                        
393600     MOVE WS-IDPRODNR            TO W-420-IDPRODNR                        
393700     PERFORM IMS-GU-WDE42-KORD-BSEQ                                       
393800                                                                          
393900     MOVE KORD-IDORDER           TO W-301-IDORDER                         
394000     MOVE KORD-IDDC              TO W-301-IDDC                            
394100     MOVE KORD-IDPRODNR          TO W-301-IDPRODNR                        
394200     MOVE KORD-IDPLKLST          TO W-301-IDPLKLST                        
394300     PERFORM IMS-GU-ORQA01                                                
394400     IF  SEGMENT-FINNS                                                    
394500       MOVE ODEL-IDTRP           TO WS-ODEL-IDTRP                         
394600       MOVE ODEL-IDLEVNR         TO WS-IDLEVNR                            
394700       MOVE ODEL-IDDC-EXP        TO WS-ODEL-IDDC-EXP                      
394800     END-IF                                                               
394900                                                                          
395000     IF  WS-RINT-ANT-FPACK-ORAD > ZERO                                    
395100*      * RAD-INTERVALLET HAR FÄRDIGPACKADE ORADER                         
395200                                                                          
395300       PERFORM FDA-LAES-SHIFTTAB                                          
395400                                                                          
395500*CO      * PRODTAB UPPDATERAS FÖR ALLA PRODKL. (MARS 2013)                
395600       IF  ODEL-KDPRODKL = 'B'                                            
395700       OR  ODEL-KDPRODKL = 'C'                                            
395800*        * PRODTAB UPPDATERAS ENDAST FÖR PRODKL B OCH C.                  
395900                                                                          
396000         MOVE KORD-IDDC          TO W-4471-IDDC                           
396100         MOVE ODEL-IDPRCBAS      TO W-4471-IDPRCBAS                       
396200         MOVE ODEL-IDPRCVAR      TO W-4471-IDPRCVAR                       
396300         PERFORM IMS-GHU-XXKW11                                           
396400                                                                          
396500         IF  SEGMENT-FINNS                                                
396600*          * PRODTAB UPPDATERAS ENDAST OM ORDERDELENS PRC FINNS.          
396700                                                                          
396800           MOVE 1                TO IND1                                  
396900           MOVE W-4478-IDSHIFT   TO IND2                                  
397000           MOVE ODEL-DARFS       TO HJALP-ODEL-DARFS                      
397100           MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                     
397200                                                                          
397300           PERFORM UNTIL IND1 = 30 OR                                     
397400                         4472-TIRFS (IND1) = ZERO OR                      
397500                         HJALP-ODEL-DARFS-6 = HJALP-4472-TIRFS-6          
397600             ADD 1                TO IND1                                 
397700             MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                   
397800           END-PERFORM                                                    
397900                                                                          
398000           MOVE ODEL-DARFS (3:10) TO 4472-TIRFS (IND1)                    
398100           MOVE W-4478-IDSHIFT  TO 4472-IDSHIFT (IND1, IND2)              
398200           ADD WS-RINT-ANT-FPACK-ORAD                                     
398300                                TO 4472-KVRADER-PRAPP (IND1, IND2)        
398400           PERFORM FDB-ADDERA-TOTAL-PRODTID                               
398500           PERFORM IMS-REPL-XXKW11                                        
398600         END-IF                                                           
398700       END-IF                                                             
398800     END-IF                                                               
398900     .                                                                    
399000     EJECT                                                                
399100 FDA-LAES-SHIFTTAB         SECTION.                                       
399200*                                                                         
399300     MOVE KORD-IDDC         TO W-4477-IDDC                                
399400     MOVE '1'               TO W-4478-IDSHIFT                             
399500     MOVE KORD-IDUSER       TO W-4478-IDUSER                              
399600     PERFORM IMS-GU-XXLB                                                  
399700*                                                                         
399800     IF SEGMENT-SAKNAS                                                    
399900        MOVE '2'            TO W-4478-IDSHIFT                             
400000        PERFORM IMS-GU-XXLB                                               
400100*                                                                         
400200        IF SEGMENT-SAKNAS                                                 
400300           MOVE '3'         TO W-4478-IDSHIFT                             
400400           PERFORM IMS-GU-XXLB                                            
400500*                                                                         
400600           IF SEGMENT-SAKNAS                                              
400700              MOVE '1'      TO W-4478-IDSHIFT                             
400800           END-IF                                                         
400900        END-IF                                                            
401000     END-IF                                                               
401100     .                                                                    
401200     EJECT                                                                
401300 FDB-ADDERA-TOTAL-PRODTID             SECTION.                            
401400                                                                          
401500     MOVE 4472-SUPTID-PRAPP (IND1, IND2) TO WS-SUPTID-PRAPP               
401600                                                                          
401700     COMPUTE WS-KVPTID-MIN ROUNDED = WS-RINT-ANT-FPACK-ORAD *             
401800                                     ODEL-KVPTID                          
401900     END-COMPUTE                                                          
402000                                                                          
402100     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
402200     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
402300     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
402400                            (WS-KVPTID-TIM * 60)                          
402500                                                                          
402600     ADD  WS-SUPTID-MIN                  TO WS-KVPTID-MIN                 
402700     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
402800     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
402900     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
403000                            (WS-KVPTID-TIM * 60)                          
403100     MOVE WS-KVPTID-MIN                  TO WS-SUPTID-MIN                 
403200                                                                          
403300     MOVE WS-SUPTID-PRAPP TO 4472-SUPTID-PRAPP (IND1, IND2)               
403400     .                                                                    
403500     EJECT                                                                
403600 FE-KOLLILAASNING     SECTION.                                            
403700     MOVE WS-IDPRODNR TO W-4301-IDPRODNR                                  
403800     PERFORM IMS-GHU-XXDU01                                               
403900     IF SEGMENT-SAKNAS                                                    
404000       MOVE '4301'      TO 4301-IDHTYP                                    
404100       MOVE WS-IDPRODNR TO 4301-IDPRODNR                                  
404200       MOVE LOW-VALUE   TO 4301-LOWVALUE                                  
404300       PERFORM IMS-ISRT-XXDU01                                            
404400                                                                          
404500       MOVE WS-IDKOLLI  TO 4302-IDKOLLI                                   
404600       MOVE WS-IDPLKLST TO 4302-IDPLKLST                                  
404700       MOVE  ZERO       TO 4302-KDKOLSTA                                  
404800       MOVE  NEJ        TO 4302-FLBANDST                                  
404900       MOVE  SPACE      TO 4302-FILLER                                    
405000       PERFORM IMS-ISRT-XXDU11                                            
405100     ELSE                                                                 
405200       MOVE WS-IDKOLLI  TO W-4302-IDKOLLI                                 
405300       MOVE WS-IDPLKLST TO W-4302-IDPLKLST                                
405400       PERFORM IMS-GNP-XXDU11                                             
405500       IF SEGMENT-SAKNAS                                                  
405600          MOVE WS-IDKOLLI  TO 4302-IDKOLLI                                
405700          MOVE WS-IDPLKLST TO 4302-IDPLKLST                               
405800          MOVE  ZERO       TO 4302-KDKOLSTA                               
405900          MOVE  NEJ        TO 4302-FLBANDST                               
406000          MOVE  SPACE      TO 4302-FILLER                                 
406100          PERFORM IMS-ISRT-XXDU11                                         
406200       END-IF                                                             
406300     END-IF                                                               
406400     .                                                                    
406500     EJECT                                                                
406600 G-UPPDATERA-KOLLIREG      SECTION.                                       
406700                                                                          
406800     PERFORM IMS-GHU-KOLLIREG                                             
406900     MOVE VORD-IDDISTR       TO  TEST-IDDISTR                             
407000                                 DIS128-IDDISTR                           
407100     MOVE VORD-KDORDKL       TO  WS-KDORDKL                               
407200     MOVE VORD-FLAUTFAK      TO  WS-FLAUTFAK                              
407300     MOVE VORD-DARFS         TO  WS-DARFS                                 
407400     IF VORD-KDORDSTA        =   1                                        
407500         MOVE 2              TO  VORD-KDORDSTA                            
407600     END-IF                                                               
407700     COMPUTE VORD-KVKOLPAC = VORD-KVKOLPAC + ARB-ANTAL-KOLLI              
407800* FIX-START FÖR ATT TA HAND OM EN W4T315-TRANS SOM GÅTT FRÅN              
407900* 0605 EFTER DET ATT R4T397/98-TRANS HAR GÅTT.                            
408000*    IF NOT VORD-IDPRODNR = 0591848                                       
408100     COMPUTE VORD-KVORDRAD-PACK                                           
408200                         = VORD-KVORDRAD-PACK + WS-TOT-ANT-RADER          
408300*    END-IF                                                               
408400* FIX-SLUT                                                                
408500     IF MID-IDRADNR-FOM (12) = ALL '+'                                    
408600                                                                          
408700         ADD ARB-ANTAL-KOLLI      TO  VORD-KVKOLLI                        
408800         MOVE WS-DAGENS-DATUM     TO  VORD-TIPACKN-SK                     
408900                                                                          
409000         IF MID-VKORDBTO-KOLLI = ALL '+'                                  
409100*          COMPUTE VORD-VKORDBTO ROUNDED                                  
409200           COMPUTE VORD-VKORDBTO                                          
409300                 = VORD-VKORDBTO +                                        
409400                   ARB-KOLLI-VKORDNTO * ARB-ANTAL-KOLLI                   
409500           END-COMPUTE                                                    
409600* FOR DIST03-SVERIGE GROSS WEIGHT WILL BE REPORTED TO                     
409700             COMPUTE WS-EMB-VKTARA-TOT-ORDER                              
409800                   = WS-EMB-VKTARA-ONE-CASE * ARB-ANTAL-KOLLI             
409900             END-COMPUTE                                                  
410000             ADD WS-EMB-VKTARA-TOT-ORDER  TO VORD-VKORDBTO                
410100         ELSE                                                             
410200*          COMPUTE VORD-VKORDBTO  ROUNDED                                 
410300           COMPUTE VORD-VKORDBTO                                          
410400                 = VORD-VKORDBTO  +                                       
410500                   WS-MOD-VKORDBTO * ARB-ANTAL-KOLLI                      
410600           END-COMPUTE                                                    
410700         END-IF                                                           
410800                                                                          
410900         COMPUTE VORD-VLORDBTO    ROUNDED                                 
411000               = VORD-VLORDBTO + WS-VLORDBTO * ARB-ANTAL-KOLLI            
411100         END-COMPUTE                                                      
411200                                                                          
411300         COMPUTE VORD-SUORDV-PACK-LOC ROUNDED                             
411400               = VORD-SUORDV-PACK-LOC + ARB-KOLLI-SUORDV-LOC              
411500                                         * ARB-ANTAL-KOLLI                
411600         END-COMPUTE                                                      
411700                                                                          
411800         COMPUTE VORD-SUORDV-PACK-LOCPREL ROUNDED                         
411900               = VORD-SUORDV-PACK-LOCPREL +                               
412000               ARB-KOLLI-SUORDV-LOCPREL * ARB-ANTAL-KOLLI                 
412100         END-COMPUTE                                                      
412200                                                                          
412300         COMPUTE VORD-SUORDV-PACK ROUNDED                                 
412400             = VORD-SUORDV-PACK + ARB-KOLLI-SUORDV                        
412500                                 * ARB-ANTAL-KOLLI                        
412600         END-COMPUTE                                                      
412700         MOVE ARB-KOLLI-KDVALISO     TO VORD-KDVALISO                     
412800         MOVE ARB-KOLLI-KDVALISO-EXP TO VORD-KDVALISO-EXP                 
412900     END-IF                                                               
413000                                                                          
413100     IF  NOT WS-ORDERVIS-TRANS                                            
413200        IF WS-IDDC NOT = W-IDDC-B6                                        
413300           MOVE WS-IDDC TO W-IDDC-B6                                      
413400           PERFORM IMS-GU-WDB601                                          
413500        END-IF                                                            
413600        IF  VORD-KDORDSTA < +3                                            
413700        AND VORD-KVKOLLI  > +0                                            
413800        AND (DIST03-SVERIGE OR DIST35-CDC-LDC-REFILL)                     
413900        AND (DCS-CDC OR (DCS-SDC AND DCS-IDLANDX2 = 'SE'))                
414000                                                                          
414100          MOVE VORD-KDFRAKT   TO WS-KDFRAKT                               
414200          MOVE WS-KDFRAKT     TO FRAK01-KDFRAKT                           
414300          MOVE WS-IDKOLLI     TO WS-IDKOLLI-LR                            
414400                                                                          
414500          IF  FRAK01-SVERIGE2                                             
414600          OR  FRAK01-NORDEN                                               
414700          OR  FRAK01-KDFRAKT21                                            
414800          OR  FRAK01-KDFRAKT62                                            
414900          OR (WS-IDKOLLI-LR > 149 AND WS-IDKOLLI-LR < 200)                
415000          OR (WS-IDKOLLI-LR > 349 AND WS-IDKOLLI-LR < 400)                
415100                                                                          
415200             IF   VORD-FLDIRLEV = NEJ                                     
415300             AND  VORD-KDFRAKT  NOT = +17                                 
415400               IF NOT DIS128-FRAKTS                                       
415500                 MOVE JA          TO VORD-FLFRAKTS                        
415600               END-IF                                                     
415700             END-IF                                                       
415800           END-IF                                                         
415900        END-IF                                                            
416000     END-IF                                                               
416100                                                                          
416200     PERFORM IMS-REPL-KOLLIREG                                            
416300     SKIP2                                                                
416400     IF KOLLI-INTERVALL                                                   
416500         PERFORM GA-BEH-KOLLI-INTERVALL                                   
416600     ELSE                                                                 
416700         PERFORM GB-BEH-ENKELT-KOLLI                                      
416800     END-IF                                                               
416900     .                                                                    
417000     EJECT                                                                
417100 GA-BEH-KOLLI-INTERVALL   SECTION.                                        
417200                                                                          
417300     MOVE +1                      TO  INX                                 
417400     MOVE WS-IDKOLLI-FOM          TO  WS-IDKOLLI-NUM                      
417500     PERFORM UNTIL INX NOT < ARB-ANTAL-KOLLI-PLUS-1                       
417600         MOVE WS-IDKOLLI-NUM      TO  W-611-IDKOLLI                       
417700         PERFORM IMS-GHU-KOLLI                                            
417900         PERFORM S11-UPPD-KOLLI-FRAN-ARB                                  
418000         IF WS-SAMMA-BILD                                                 
418100             IF KOLLI-KDKOLSTA    =   ZERO                                
418200                 IF WS-PRT-KDSVAR-FOLJEFL = RAETT                         
418300                   IF WS-SAMMA-BILD                                       
418400                     PERFORM S18-SKAPA-FOLJESEDEL-TRANS                   
418500                   END-IF                                                 
418600                 END-IF                                                   
418700                 MOVE 1               TO KOLLI-KDKOLSTA                   
418800                 MOVE WS-DAGENS-DATUM TO KOLLI-TIPACKN                    
418900                 MOVE WS-TIDPUNKT     TO WS-TIDPUNKT-RED                  
419000                 MOVE WS-HHMMSS       TO KOLLI-TIPACTID                   
419100                 MOVE WS-DARFS        TO KOLLI-DARFS                      
419200                 PERFORM S12-SKAPA-4322                                   
419300                 PERFORM S13-UPPDAT-KDORDSTA                              
419400*LK TMS CASE INFO -CASE INTERVAL                                          
419500                 MOVE KOLLI-IDKOLLI   TO TMS-IDKOLLI(INX)                 
419600             END-IF                                                       
419700         END-IF                                                           
419800*                                                                         
419900         PERFORM S16-UPPD-FARLIGT-GODS-DATA                               
420000*                                                                         
420100         PERFORM IMS-REPL-KOLLI                                           
420200         ADD +1 TO WS-IDKOLLI-NUM                                         
420300                   INX                                                    
420400     END-PERFORM                                                          
420500     MOVE WS-IDDC            TO TMS-IDDC                                  
420600     MOVE KORD-IDDISTR       TO TMS-IDDISTR                               
420700     MOVE KORD-IDKUNDNR      TO TMS-IDKUNDNR                              
420800     MOVE KORD-IDORDNR5      TO TMS-IDORDNR7                              
420900     CALL W403TMS1 USING TMS-W403TMS1                                     
421000          TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                                
421100          TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                          
421200          TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                          
421300          TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                        
421400          TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                          
421500          TMS-WDK5-PCB TMS-WDQ2C-PCB                                      
421600                                                                          
421700     IF WS-PRT-KDSVAR-ADRESSFL = RAETT                                    
421800       PERFORM S14-EV-SEND-PRINTTRANS                                     
421900     END-IF                                                               
422000     MOVE WS-IDKOLLI-FOM          TO  WS-IDKOLLI-NUM                      
422100                                                                          
422200     IF WS-IDKOLLI-TOM NOT = ZERO                                         
422300         MOVE WS-IDKOLLI-TOM           TO MOD-IDKOLLI-UT                  
422400     END-IF                                                               
422500     .                                                                    
422600     EJECT                                                                
422700 GB-BEH-ENKELT-KOLLI      SECTION.                                        
422800                                                                          
422900     MOVE WS-IDKOLLI              TO  W-611-IDKOLLI                       
423000     PERFORM IMS-GHU-KOLLI                                                
423200     PERFORM S11-UPPD-KOLLI-FRAN-ARB                                      
423300     IF MID-IDRADNR-FOM (12) = ALL '+'                                    
423400         IF KOLLI-KDKOLSTA        =   ZERO                                
423500             IF WS-PRT-KDSVAR-ADRESSFL = RAETT                            
423600               PERFORM S14-EV-SEND-PRINTTRANS                             
423700             END-IF                                                       
423800             IF WS-PRT-KDSVAR-FOLJEFL = RAETT                             
423900               IF WS-SAMMA-BILD                                           
424000                 PERFORM S18-SKAPA-FOLJESEDEL-TRANS                       
424100               END-IF                                                     
424200             END-IF                                                       
424300             MOVE 1               TO KOLLI-KDKOLSTA                       
424400             MOVE WS-DAGENS-DATUM TO KOLLI-TIPACKN                        
424500             MOVE WS-TIDPUNKT     TO WS-TIDPUNKT-RED                      
424600             MOVE WS-HHMMSS       TO KOLLI-TIPACTID                       
424700             MOVE WS-DARFS        TO KOLLI-DARFS                          
424800             PERFORM S12-SKAPA-4322                                       
424900             PERFORM S13-UPPDAT-KDORDSTA                                  
425000                                                                          
425100         END-IF                                                           
425200     END-IF                                                               
425300*                                                                         
425400     PERFORM S16-UPPD-FARLIGT-GODS-DATA                                   
425500*                                                                         
425600     PERFORM IMS-REPL-KOLLI                                               
425700*                                                                         
425800*TMS PACKING INFO                                                         
425900       MOVE WS-IDDC               TO TMS-IDDC                             
426000       MOVE KORD-IDDISTR          TO TMS-IDDISTR                          
426100       MOVE KORD-IDKUNDNR         TO TMS-IDKUNDNR                         
426200       MOVE KORD-IDORDNR5         TO TMS-IDORDNR7                         
426300       MOVE KOLLI-IDKOLLI         TO TMS-IDKOLLI(1)                       
426400       CALL W403TMS1 USING TMS-W403TMS1                                   
426500           TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                               
426600           TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                         
426700           TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                         
426800           TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                       
426900           TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                         
427000           TMS-WDK5-PCB TMS-WDQ2C-PCB                                     
427100     .                                                                    
427200     EJECT                                                                
427300 H-AVSLUT             SECTION.                                            
427400     IF WS-IDDC NOT = W-IDDC-B6                                           
427500        MOVE WS-IDDC TO W-IDDC-B6                                         
427600        PERFORM IMS-GU-WDB601                                             
427700     END-IF                                                               
427800                                                                          
427900     IF WS-BEHANDLING-RATT                                                
428000       IF  MOD-TEMFSINF = SPACE                                           
428100       OR  MOD-TEMFSINF = LOW-VALUE                                       
428200           MOVE RAETT-1 (INDX)           TO  MOD-TEMFSINF                 
428300       END-IF                                                             
428400                                                                          
428500       IF  WS-ORDERVIS-TRANS                                              
428600           MOVE '0605'                   TO  MFS-IDTRANS                  
428700           MOVE 'W0T605U '               TO  MSG-KDTRANS-1                
428800           MOVE '4315'                   TO  MSG-IDTRANS-1                
428900           MOVE WS-KDMFSFOR              TO  MSG-KDMFSFOR-1               
429000           COMPUTE MSG-KVLL = LENGTH OF 0605-MID + 17                     
429100           MOVE SPACE                    TO  0605-MID-TEMFSFEL            
429200           MOVE JA                       TO  0605-MID-FLSVAR              
429300           MOVE 0605-MID                                                  
429400                            TO  MSG-INDATA-MINUS-1-TRANSKOD               
429500       ELSE                                                               
429600           IF MID-IDRADNR-FOM (12)       =   ALL '+'                      
429700             MOVE WS-IDKUNDNR-NUM        TO TRANSFER-KUND                 
429800             EVALUATE TRUE                                                
429900             WHEN (DCS-CDC AND (DIST08-URSP-RAPP     OR                   
430000                                DIST08-URSP-RAPP-CDC OR                   
430100                                DIST08-URSP-SPX))                         
430200               OR                                                         
430300                 ((DCS-NDC-NA OR DCS-NDC-PF) AND                          
430400                   TRANSFER-KUNDNR AND                                    
430500                   DIST08-URSP-TRANSFER-NDC)                              
430600               OR                                                         
430700                 ((DCS-NDC-NA OR DCS-NDC-PF) AND                          
430800                   RETUR-KUNDNR AND                                       
430900                   DIST08-URSP-RETUR-NDC)                                 
431000               OR                                                         
431100                  (DCS-NDC-NA AND DCS-IDLANDX2 = 'CA' AND                 
431200                   DIST08-URSP-RAPP-CDC)                                  
431300                                                                          
431400                 PERFORM HB-LADDA-4317                                    
431500                 MOVE '4315'             TO  MFS-IDTRANS                  
431600                 IF ENGLISH-TEXT                                          
431700                   MOVE 'W4O317N1'       TO  MFS-IDMOD                    
431800                 ELSE                                                     
431900                   MOVE 'W4O31701'       TO  MFS-IDMOD                    
432000                 END-IF                                                   
432100                 COMPUTE MSG-KVLL =                                       
432200                         LENGTH OF 4317-MOD-W4O31701 + 17                 
432300                 MOVE 4317-MOD           TO  MSG-AREA                     
432400             WHEN MID-FLSISTAK         = JA OR YES                        
432500                 MOVE '4318'             TO  MFS-IDTRANS                  
432600                 MOVE 'W4T318U '         TO  MSG-KDTRANS-1                
432700                 MOVE '4315'             TO  MSG-IDTRANS-1                
432800                 MOVE WS-KDMFSFOR        TO  MSG-KDMFSFOR-1               
432900                 COMPUTE MSG-KVLL =                                       
433000                         LENGTH OF 4318-MID-W4I31801 + 17                 
433100                 PERFORM HC-LADDA-4318-AREA                               
433200                 MOVE 4318-MID                                            
433300                            TO  MSG-INDATA-MINUS-1-TRANSKOD               
433400             WHEN OTHER                                                   
433500                 MOVE '4315'             TO  MFS-IDTRANS                  
433600                 MOVE MOD-W4O31501       TO  MSG-AREA                     
433700             END-EVALUATE                                                 
433800           ELSE                                                           
433900             PERFORM HA-LADDA-4314                                        
434000             MOVE '4315'                 TO  MFS-IDTRANS                  
434100                                                                          
434200             IF DCS-NDC-NA OR DCS-NDC-PF                                  
434300               CONTINUE                                                   
434400             ELSE                                                         
434500               MOVE MSGI-KDPRTVAL-FS       TO MOD-PRTVAL-FOLJEFL          
434600               MOVE MSGI-KDPRTVAL-ADR      TO MOD-PRTVAL-ADRESSFL         
434700             END-IF                                                       
434800                                                                          
434900             MOVE WS-KDMFSFOR            TO  MFS-KDMFSFOR                 
435000             IF ENGLISH-TEXT                                              
435100               MOVE 'W4O314N1'           TO   MFS-IDMOD                   
435200               IF 4314-MOD-FLFORTSK = JA                                  
435300                  MOVE YES TO 4314-MOD-FLFORTSK                           
435400               END-IF                                                     
435500             ELSE                                                         
435600               MOVE 'W4O31401'           TO   MFS-IDMOD                   
435700             END-IF                                                       
435800             COMPUTE MSG-KVLL = LENGTH OF 4314-MOD-W4O31401 + 17          
435900             MOVE 4314-MOD               TO  MSG-AREA                     
436000           END-IF                                                         
436100       END-IF                                                             
436200     ELSE                                                                 
436300       PERFORM IMS-ROLLBACK                                               
436400       IF  WS-ORDERVIS-TRANS                                              
436500           MOVE '0605'                   TO  MFS-IDTRANS                  
436600           MOVE 'W0T605U '               TO  MSG-KDTRANS-1                
436700           MOVE '4315'                   TO  MSG-IDTRANS-1                
436800           MOVE WS-KDMFSFOR              TO  MSG-KDMFSFOR-1               
436900           COMPUTE MSG-KVLL = LENGTH OF 0605-MID + 17                     
437000           MOVE NEJ                      TO  0605-MID-FLSVAR              
437100           MOVE MOD-TEMFSFEL             TO  0605-MID-TEMFSFEL            
437200           MOVE 0605-MID                                                  
437300                            TO  MSG-INDATA-MINUS-1-TRANSKOD               
437400       ELSE                                                               
437500           MOVE '4315'                   TO  MFS-IDTRANS                  
437600           IF DCS-NDC-NA OR DCS-NDC-PF                                    
437700             CONTINUE                                                     
437800           ELSE                                                           
437900             MOVE MSGI-KDPRTVAL-FS   TO MOD-PRTVAL-FOLJEFL                
438000             MOVE MSGI-KDPRTVAL-ADR  TO MOD-PRTVAL-ADRESSFL               
438100           END-IF                                                         
438200                                                                          
438300           MOVE MOD-W4O31501             TO  MSG-AREA                     
438400       END-IF                                                             
438500                                                                          
438600       IF WEIGHT-MISMATCH                                                 
438700          PERFORM S21-SEND-OPEN                                           
438800          PERFORM S22-PUT-HEADER                                          
438900          PERFORM S24-WRITE-LINE                                          
439000          PERFORM S25-SEND-CLOSE                                          
439100       END-IF                                                             
439200     END-IF                                                               
439300     .                                                                    
439400     EJECT                                                                
439500 HA-LADDA-4314        SECTION.                                            
439600                                                                          
439700     SKIP3                                                                
439800     MOVE LOW-VALUE                TO 4314-MOD                            
439900     MOVE '4314'                   TO 4314-MOD-IDTRANS                    
440000                                                                          
440100     IF WS-PRT-KDSVAR-FOLJEFL = RAETT                                     
440200       MOVE WS-KDPRTVAL-FS      TO 4314-MOD-KDPRTVAL-FOLJEFL              
440300     ELSE                                                                 
440400                                                                          
440500       MOVE 'UU'                 TO 4314-MOD-KDPRTVAL-FOLJEFL             
440600     END-IF                                                               
440700                                                                          
440800     IF WS-PRT-KDSVAR-ADRESSFL = RAETT                                    
440900       MOVE WS-KDPRTVAL-ADR     TO 4314-MOD-KDPRTVAL-ADRESSFL             
441000     ELSE                                                                 
441100                                                                          
441200       MOVE 'UU'                 TO 4314-MOD-KDPRTVAL-ADRESSFL            
441300     END-IF                                                               
441400                                                                          
441500     MOVE SPACE                    TO 4314-MOD-TEMFSFEL                   
441600     MOVE MFS-RENSA-FAELT          TO 4314-MOD-IDANSTNR-IN                
441700                                      4314-MOD-IDDISTR-IN                 
441800                                      4314-MOD-IDKUNDNR-IN                
441900                                      4314-MOD-IDORDNR-IN                 
442000                                      4314-MOD-IDKOLLI-IN                 
442100                                      4314-MOD-IDPRODNR-IN                
442200     MOVE WS-IDANSTNR              TO 4314-MOD-IDANSTNR-UT                
442300     INSPECT 4314-MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE         
442400     MOVE WS-IDDISTR-NUM           TO 4314-MOD-IDDISTR-UT                 
442500     INSPECT 4314-MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE         
442600     MOVE WS-IDKUNDNR-NUM          TO 4314-MOD-IDKUNDNR-UT                
442700     INSPECT 4314-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
442800     MOVE WS-IDORDNR               TO 4314-MOD-IDORDNR-UT                 
442900     INSPECT 4314-MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE         
443000     MOVE WS-IDKOLLI-NUM           TO 4314-MOD-IDKOLLI-UT                 
443100     INSPECT 4314-MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE         
443200     MOVE WS-IDPRODNR              TO 4314-MOD-IDPRODNR-UT                
443300     INSPECT 4314-MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE         
443400     MOVE '4315'                   TO 4314-MOD-IDTRANS-START              
443500                                                                          
443600     IF  MID-FLSISTAK = '+'                                               
443700         MOVE MFS-RENSA-FAELT      TO 4314-MOD-FLSISTAK                   
443800     ELSE                                                                 
443900         MOVE MID-FLSISTAK         TO 4314-MOD-FLSISTAK                   
444000     END-IF                                                               
444100     MOVE MFS-FORMATETS-ATTR       TO 4314-MOD-FLSISTAK-ATTR              
444200                                                                          
444300     IF  MID-VKORDBTO-KOLLI = ALL '+'                                     
444400         MOVE MFS-RENSA-FAELT      TO 4314-MOD-VKORDBTO-KOLLI             
444500         MOVE ZERO                 TO 4314-MOD-VKORDBTO-4315              
444600     ELSE                                                                 
444700         MOVE DEC-IDEDITDATA       TO 4314-MOD-VKORDBTO-KOLLI             
444800         MOVE WS-VKORDBTO-TOT      TO 4314-MOD-VKORDBTO-4315              
444900     END-IF                                                               
445000     MOVE MFS-FORMATETS-ATTR       TO 4314-MOD-VKORDBTO-ATTR              
445100                                      4314-MOD-VKORDBTO-4315-ATTR         
445200                                                                          
445300     MOVE JA                       TO 4314-MOD-FLFORTSK                   
445400     MOVE MID-IDRADNR-FOM (12)     TO 4314-MOD-IDRADNR-FOM-S              
445500     MOVE MID-IDRADNR-TOM (12)     TO 4314-MOD-IDRADNR-TOM-S              
445600     MOVE WS-KVLEVART              TO 4314-MOD-KVLEVART-S                 
445700                                                                          
445800     IF  DIST03-SVERIGE                                                   
445900          MOVE RAETT-2 (INDX)      TO 4314-MOD-TEMFSINF                   
446000     ELSE                                                                 
446100          MOVE WS-TEMFSINF         TO 4314-MOD-TEMFSINF                   
446200     END-IF                                                               
446300                                                                          
446400     MOVE +1                       TO INX                                 
446500     PERFORM UNTIL INX NOT < MAX-RAD-ANTAL-PLUS-1                         
446600         MOVE MFS-RENSA-FAELT      TO 4314-MOD-IDRADNR-FOM (INX)          
446700                                      4314-MOD-IDRADNR-TOM (INX)          
446800                                      4314-MOD-KVLEVART (INX)             
446900         ADD +1 TO INX                                                    
447000     END-PERFORM                                                          
447100     MOVE MFS-ADD-SAETT-CURSOR   TO 4314-MOD-IDRADNR-FOM-ATTR (1)         
447200     .                                                                    
447300     EJECT                                                                
447400 HB-LADDA-4317        SECTION.                                            
447500                                                                          
447600     MOVE LOW-VALUE                TO 4317-MOD                            
447700     MOVE '4317'                   TO 4317-MOD-IDTRANS                    
447800     MOVE SPACE                    TO 4317-MOD-TEMFSFEL                   
447900     MOVE MFS-RENSA-FAELT          TO 4317-MOD-IDANSTNR-IN                
448000                                      4317-MOD-IDDISTR-IN                 
448100                                      4317-MOD-IDKUNDNR-IN                
448200                                      4317-MOD-IDORDNR-IN                 
448300                                      4317-MOD-IDKOLLI-IN                 
448400                                      4317-MOD-IDPRODNR-IN                
448500                                      4317-MOD-IDDC-IN                    
448600                                      4317-MOD-FLSISTAK                   
448700                                      4317-MOD-IDRADNR-S                  
448800                                      4317-MOD-KDARTURS-S                 
448900     MOVE WS-IDANSTNR              TO 4317-MOD-IDANSTNR-UT                
449000     INSPECT 4317-MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE         
449100     MOVE WS-IDDISTR-NUM           TO 4317-MOD-IDDISTR-UT                 
449200     INSPECT 4317-MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE         
449300     MOVE WS-IDKUNDNR-NUM          TO 4317-MOD-IDKUNDNR-UT                
449400     INSPECT 4317-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
449500     MOVE WS-IDORDNR               TO 4317-MOD-IDORDNR-UT                 
449600     INSPECT 4317-MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE         
449700     MOVE WS-IDDC                  TO 4317-MOD-IDDC-UT                    
449800     IF WS-IDKOLLI-TOM NOT = ZERO                                         
449900         MOVE WS-IDKOLLI-TOM           TO 4317-MOD-IDKOLLI-UT             
450000     ELSE                                                                 
450100         MOVE WS-IDKOLLI               TO 4317-MOD-IDKOLLI-UT             
450200     END-IF                                                               
450300     INSPECT 4317-MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE         
450400     MOVE WS-IDPRODNR              TO 4317-MOD-IDPRODNR-UT                
450500     INSPECT 4317-MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE         
450600     MOVE '4315'                   TO 4317-MOD-IDTRANS-START              
450700     MOVE MOD-TEMFSINF             TO 4317-MOD-TEMFSINF                   
450800                                                                          
450900     IF  MID-FLSISTAK = '+'                                               
451000         MOVE LOW-VALUE            TO 4317-MOD-FLSISTAK                   
451100     ELSE                                                                 
451200         MOVE MID-FLSISTAK         TO 4317-MOD-FLSISTAK                   
451300     END-IF                                                               
451400                                                                          
451500     MOVE +1                       TO INX                                 
451600     PERFORM UNTIL INX NOT < 14                                           
451700         MOVE MFS-RENSA-FAELT      TO 4317-MOD-IDRADNR (INX)              
451800                                      4317-MOD-KDARTURS (INX)             
451900         ADD +1 TO INX                                                    
452000     END-PERFORM                                                          
452100     MOVE MFS-ADD-SAETT-CURSOR     TO 4317-MOD-IDRADNR-ATTR (1)           
452200     .                                                                    
452300     EJECT                                                                
452400 HC-LADDA-4318-AREA    SECTION.                                           
452500                                                                          
452600     MOVE LOW-VALUE                TO 4318-MID                            
452700     MOVE WS-IDANSTNR              TO 4318-MID-IDANSTNR-IN                
452800     MOVE SPACE                    TO 4318-MID-IDANSTNR-UT                
452900     MOVE WS-IDDISTR-NUM           TO 4318-MID-IDDISTR-IN                 
453000     MOVE SPACE                    TO 4318-MID-IDDISTR-UT                 
453100     MOVE WS-IDKUNDNR-NUM          TO 4318-MID-IDKUNDNR-IN                
453200     MOVE SPACE                    TO 4318-MID-IDKUNDNR-UT                
453300     MOVE WS-IDORDNR               TO 4318-MID-IDORDNR-IN                 
453400     MOVE SPACE                    TO 4318-MID-IDORDNR-UT                 
453500     MOVE WS-IDPRODNR              TO 4318-MID-IDPRODNR-IN                
453600     MOVE SPACE                    TO 4318-MID-IDPRODNR-UT                
453700     MOVE WS-IDDC                  TO 4318-MID-IDDC-IN                    
453800     MOVE SPACE                    TO 4318-MID-IDDC-UT                    
453900     IF WS-IDKOLLI-TOM NOT = ZERO                                         
454000         MOVE WS-IDKOLLI-TOM           TO 4318-MID-IDKOLLI-IN             
454100     ELSE                                                                 
454200         MOVE WS-IDKOLLI               TO 4318-MID-IDKOLLI-IN             
454300     END-IF                                                               
454400     MOVE ARB-ADFLGEO              TO 4318-MID-ADFLGEO                    
454500     MOVE ARB-ADFLOMR              TO 4318-MID-ADFLOMR                    
454600     MOVE ARB-ADRUTNIV             TO 4318-MID-ADRUTNIV                   
454700     MOVE SPACE                    TO 4318-MID-IDKOLLI-UT                 
454800     MOVE '4315'                   TO 4318-MID-IDTRANS-START              
454900     MOVE '+'                      TO 4318-MID-FLSVAR                     
455000     .                                                                    
455100     EJECT                                                                
455200 K-UPPDAT-4726-4727 SECTION.                                              
455300     SKIP2                                                                
455400                                                                          
455500       MOVE '4726'                  TO W-4726-IDHTYP                      
455600*      IF  DIST03-SVERIGE-EJ-778                                          
455700*      OR  DIST18-SKROT                                                   
455800*          MOVE JA                  TO W-4726-FLBATCH                     
455900*      ELSE                                                               
456000           MOVE NEJ                 TO W-4726-FLBATCH                     
456100*      END-IF                                                             
456200       MOVE LOW-VALUE               TO W-4726-LOWVALUE                    
456300                                                                          
456400       PERFORM IMS-GU-4726-ROT-KVAL                                       
456500                                                                          
456600       MOVE WS-IDDISTR-NUM          TO W-4726-IDDISTR                     
456700       MOVE WS-IDKUNDNR-NUM         TO W-4726-IDKUNDNR                    
456800       MOVE WS-IDDC                 TO W-4726-IDDC                        
456900       MOVE WS-KDFAKTYP             TO W-4726-KDFAKTYP                    
457000                                                                          
457100       PERFORM IMS-GNP-4726-UNDERSEG-KVAL                                 
457200                                                                          
457300       IF  SEGMENT-SAKNAS                                                 
457400           MOVE WS-IDDISTR-NUM      TO AUTFAKT-IDDISTR                    
457500           MOVE WS-IDKUNDNR-NUM     TO AUTFAKT-IDKUNDNR                   
457600           MOVE WS-IDDC             TO AUTFAKT-IDDC                       
457700           MOVE WS-KDFAKTYP         TO AUTFAKT-KDFAKTYP                   
457800                                                                          
457900           PERFORM IMS-INSERT-4726-UNDERSEG                               
458000       END-IF                                                             
458100       MOVE WS-IDPRODNR             TO AUTFAKT-IDPRODNR                   
458200       MOVE ZERO                    TO AUTFAKT-IDSKEPPN                   
458300                                       AUTFAKT-PRFRAKT                    
458400                                                                          
458500       IF  DIST03-SVERIGE                                                 
458600           MOVE NEJ                 TO AUTFAKT-FLLASTA                    
458700       ELSE                                                               
458800           MOVE JA                  TO AUTFAKT-FLLASTA                    
458900       END-IF                                                             
459000                                                                          
459100       PERFORM IMS-INSERT-4727                                            
459200                                                                          
459300                                                                          
459400     .                                                                    
459500     EJECT                                                                
459600 L-HAMTA-ADRESS SECTION.                                                  
459700                                                                          
459800     MOVE WS-IDDISTR-NUM  TO W-4A1-IDDISTR                                
459900                             TEST-IDDISTR                                 
460000     MOVE WS-IDKUNDNR-NUM TO W-4A1-IDKUNDNR                               
460100     MOVE WS-IDORDNR      TO W-4A1-IDORDNR                                
460200     PERFORM IMS-GU-KUNDORDER-SEK                                         
460300     MOVE KORD-IDDISTR    TO W-401-IDDISTR                                
460400     MOVE KORD-IDKUNDNR   TO W-401-IDKUNDNR                               
460500     MOVE KORD-IDORDNR5   TO W-401-IDORDNR                                
460600     MOVE KORD-IDPRODNR   TO W-401-IDPRODNR                               
460700     MOVE KORD-IDPLKLST   TO W-401-IDPLKLST                               
460800     MOVE KORD-IDORDER    TO W-201-IDORDER                                
460900     .                                                                    
461000     EJECT                                                                
461100 M-INIT-TRANS-LL92 SECTION.                                               
461200                                                                          
461300     MOVE '4315'                 TO MFS-IDTRANS                           
461400     MOVE 'W4O315N1'             TO MFS-IDMOD                             
461500     MOVE +400                   TO MSG-KVLL                              
461600     MOVE FEL                    TO WS-INDATA-TEST                        
461700     MOVE MFS-RENSA-FAELT        TO MOD-KDKOLLI                           
461800                                    MOD-VKORDBTO-KOLLI                    
461900                                    MOD-FLSISTAK                          
462000                                    MOD-KDEMBTYP                          
462100                                    MOD-DIKOLLIL                          
462200                                    MOD-DIKOLLIB                          
462300                                    MOD-DIKOLLIH                          
462400                                    MOD-ADFLGEO                           
462500                                    MOD-ADFLOMR                           
462600                                    MOD-ADRUTNIV                          
462700                                    MOD-IDKOLLI-FOM                       
462800                                    MOD-IDKOLLI-TOM                       
462900                                    MOD-PRTVAL-ADRESSFL                   
463000                                    MOD-PRTVAL-FOLJEFL                    
463100     .                                                                    
463200     SKIP2                                                                
463300 N-INIT-TRANS-LL8  SECTION.                                               
463400                                                                          
463500     MOVE '4315'                 TO MFS-IDTRANS                           
463600     MOVE 'W4O315N1'             TO MFS-IDMOD                             
463700     MOVE +8                     TO MSG-KVLL                              
463800     MOVE FEL                    TO WS-INDATA-TEST                        
463900     .                                                                    
464000     SKIP2                                                                
464100 O-INIT-FEL-TRANS  SECTION.                                               
464200                                                                          
464300     IF  WS-ORDERVIS-TRANS                                                
464400         MOVE '0605'              TO  MFS-IDTRANS                         
464500         MOVE 'W0T605U '          TO  MSG-KDTRANS-1                       
464600         MOVE '4315'              TO  MSG-IDTRANS-1                       
464700         MOVE WS-KDMFSFOR         TO  MSG-KDMFSFOR-1                      
464800         MOVE 0605-LAENGD         TO  MSG-KVLL                            
464900         MOVE MOD-TEMFSFEL        TO  0605-MID-TEMFSFEL                   
465000         MOVE NEJ                 TO  0605-MID-FLSVAR                     
465100         MOVE 0605-MID                                                    
465200                     TO  MSG-INDATA-MINUS-1-TRANSKOD                      
465300     ELSE                                                                 
465400         MOVE '4315'              TO  MFS-IDTRANS                         
465500         IF DCS-NDC-NA OR DCS-NDC-PF                                      
465600           CONTINUE                                                       
465700         ELSE                                                             
465800           MOVE MSGI-KDPRTVAL-FS           TO MOD-PRTVAL-FOLJEFL          
465900           MOVE MSGI-KDPRTVAL-ADR          TO MOD-PRTVAL-ADRESSFL         
466000         END-IF                                                           
466100                                                                          
466200         MOVE MOD-W4O31501        TO  MSG-AREA                            
466300     END-IF                                                               
466400     .                                                                    
466500     SKIP2                                                                
466600 P-EVALUATE-INSERT-MSG-TRANS  SECTION.                                    
466700                                                                          
466800     EVALUATE TRUE                                                        
466900     WHEN MFS-IDTRANS = '4315'                                            
467000         COMPUTE MSG-KVLL =                                               
467100                 LENGTH OF 4314-MOD-W4O31401 + 17                         
467200         PERFORM IMS-INSERT-MSG                                           
467300     WHEN MFS-IDTRANS = '0605'                                            
467400         PERFORM IMS-INSERT-ALT0605MSG                                    
467500     WHEN OTHER                                                           
467600         PERFORM IMS-CHANGE-ALTMSG                                        
467700         PERFORM IMS-INSERT-ALTMSG                                        
467800     END-EVALUATE                                                         
467900     .                                                                    
468000     SKIP2                                                                
468100* MFS SEKTIONER                                                           
468200     SKIP3                                                                
468300                                                                          
468400 S02-RENSA-MOD-FALT   SECTION.                                            
468500                                                                          
468600     MOVE MFS-RENSA-FAELT           TO MOD-FLSISTAK                       
468700                                       MOD-KDKOLLI                        
468800                                       MOD-VKORDBTO-KOLLI                 
468900                                       MOD-KDEMBTYP                       
469000                                       MOD-DIKOLLIL                       
469100                                       MOD-DIKOLLIB                       
469200                                       MOD-DIKOLLIH                       
469300                                       MOD-ADFLGEO                        
469400                                       MOD-ADFLOMR                        
469500                                       MOD-ADRUTNIV                       
469600                                       MOD-IDKOLLI-FOM                    
469700                                       MOD-IDKOLLI-TOM                    
469800     SKIP2                                                                
469900     MOVE +1  TO BILD-RAD                                                 
470000     MOVE +13 TO MAX-RAD-ANTAL-PLUS-1                                     
470100     PERFORM UNTIL BILD-RAD NOT < MAX-RAD-ANTAL-PLUS-1                    
470200         MOVE MFS-RENSA-FAELT       TO MOD-IDRADNR-FOM (BILD-RAD)         
470300                                       MOD-IDRADNR-TOM (BILD-RAD)         
470400                                       MOD-KVLEVART (BILD-RAD)            
470500         ADD +1 TO BILD-RAD                                               
470600     END-PERFORM                                                          
470700     .                                                                    
470800     EJECT                                                                
470900 S04-ADD-LAES-IN-FAELT  SECTION.                                          
471000     SKIP3                                                                
471100     MOVE +1 TO BILD-RAD                                                  
471200     PERFORM UNTIL BILD-RAD NOT < MAX-RAD-ANTAL-PLUS-1                    
471300         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
471400                                 MOD-IDRADNR-FOM-ATTR (BILD-RAD)          
471500                                 MOD-IDRADNR-TOM-ATTR (BILD-RAD)          
471600                                 MOD-KVLEVART-ATTR (BILD-RAD)             
471700         ADD +1 TO BILD-RAD                                               
471800     END-PERFORM                                                          
471900     .                                                                    
472000     EJECT                                                                
472100 S06-KOLLA-AVBOKAT-ANTAL    SECTION.                                      
472200     SKIP3                                                                
472300     MOVE WS-IDPRODNR            TO  W-420-IDPRODNR                       
472400     MOVE ARB-RAD-FOM            TO  W-420-IDPURAD                        
472500     SKIP2                                                                
472600     PERFORM IMS-GHU-RAD-SEK                                              
472700     SKIP2                                                                
472800     IF WS-KVLEVART = ZERO                                                
472900         COMPUTE WS-KVLEVART = ORAD-KVAVBART - ORAD-KVLEVART              
473000     END-IF                                                               
473100     SKIP2                                                                
473200     IF WS-KVLEVART > (ORAD-KVAVBART - ORAD-KVLEVART)                     
473300         MOVE FEL                TO  WS-BEHANDLING-TEST                   
473400         MOVE MFS-NUM-FAELT-FEL  TO  MOD-KVLEVART-ATTR (INX)              
473500         MOVE FEL-725C (INDX)    TO  MOD-TEMFSFEL                         
473600     END-IF                                                               
473700     .                                                                    
473800     EJECT                                                                
473900 S09-SKAPA-KOLLI-SEGMENT    SECTION.                                      
474000                                                                          
474100     MOVE WS-IDDISTR-NUM    TO   TEST-IDDISTR                             
474200     IF DIST19-SATS                                                       
474300       MOVE SPACE           TO  KOLLI-IDTRP                               
474400     ELSE                                                                 
474500*      MOVE WS-KORD-IDORDER TO W-201-IDORDER                              
474600*      MOVE WS-IDDC           TO  W-IDDC                                  
474700*      PERFORM IMS-GU-ORQI12                                              
474800       MOVE WS-ODEL-IDTRP   TO  KOLLI-IDTRP                               
474900     END-IF                                                               
475000                                                                          
475100     MOVE WS-IDKOLLI-NUM    TO  KOLLI-IDKOLLI                             
475200     MOVE WS-IDANSTNR       TO  KOLLI-IDPLOCK                             
475300     MOVE WS-IDDISTR-NUM    TO  KOLLI-IDDISTR                             
475400     MOVE WS-IDKUNDNR-NUM   TO  KOLLI-IDKUNDNR                            
475500     MOVE WS-ADFLOMR        TO  KOLLI-ADFLOMR                             
475600     MOVE WS-ADRUTNIV       TO  KOLLI-ADRUTNIV                            
475700     MOVE WS-DIKOLLIL       TO  KOLLI-DIKOLLIL                            
475800     MOVE WS-DIKOLLIB       TO  KOLLI-DIKOLLIB                            
475900     MOVE WS-DIKOLLIH       TO  KOLLI-DIKOLLIH                            
476000     MOVE WS-KDEMBTYP       TO  KOLLI-KDEMBTYP                            
476100     MOVE WS-MOD-VKORDBTO   TO  KOLLI-VKORDBTO-KOLLI                      
476200     MOVE WS-IDDC           TO  KOLLI-IDDC                                
476300     MOVE WS-ADFLGEO        TO  KOLLI-ADFLGEO                             
476400     MOVE WS-KDKOLLI        TO  KOLLI-KDKOLLI                             
476500*    MOVE WS-IDKOLLI-SAMP   TO  KOLLI-IDKOLLI-SAMP                        
476600     MOVE ZERO              TO  KOLLI-IDKOLLI-SAMP                        
476700     MOVE NEJ               TO  KOLLI-FLBANDST                            
476800                                KOLLI-FLFRSUTS                            
476900     MOVE ZERO              TO  KOLLI-IDKOLLI-FLER                        
477000                                KOLLI-IDTRPTNR                            
477100                                KOLLI-ADVMODUL                            
477200                                KOLLI-ADHMODUL                            
477300                                KOLLI-IDFAKLOP                            
477400                                KOLLI-IDFAKT                              
477500                                KOLLI-IDFAKT-EXP                          
477600                                KOLLI-DIDMODUL                            
477700                                KOLLI-DIHMODUL                            
477800                                KOLLI-KVFALRAD                            
477900                                KOLLI-KDKOLSTA                            
478000                                KOLLI-KDORDKL                             
478100                                KOLLI-TIFAKT                              
478200                                KOLLI-TIFAKT-EXP                          
478300                                KOLLI-TIFAKTID                            
478400                                KOLLI-TIFAKTID-EXP                        
478500                                KOLLI-TILASTN                             
478600                                KOLLI-TILASTID                            
478700                                KOLLI-TIPACKN                             
478800                                KOLLI-TIPACTID                            
478900                                KOLLI-TIPACTID                            
479000                                KOLLI-VKORDNTO-KOLLI                      
479100                                KOLLI-SUORDV-KOLLI                        
479200                                KOLLI-SUORDV-KLI-EXP                      
479300                                KOLLI-SUORDV-LOC                          
479400                                KOLLI-SUORDV-LOCPREL                      
479500                                KOLLI-KDARTURS-KOLLI                      
479600                                KOLLI-KVORDRAD                            
479700                                KOLLI-TIAAVVD-PATR                        
479800                                KOLLI-KDFARLIG-KOLLI                      
479900                                KOLLI-KVFLAMP-KOLLI                       
480000                                KOLLI-IDLASTN                             
480100* OBS - HÄR INITIERAS NYA DDGS-FÄLT RENT GENERELLT                        
480200* OBS - KONTROLLERA OM DETTA ÄR KORREKT!!!                                
480300                                KOLLI-DASUPREF                            
480400                                KOLLI-TISUPTID                            
480500                                KOLLI-KDVIA                               
480600                                KOLLI-IDTULLNR                            
480700                                KOLLI-RETULKS                             
480800                                KOLLI-IDSHIPM                             
480900     MOVE SPACE             TO  KOLLI-IDLEVNR                             
481000                                KOLLI-KDVALISO                            
481100                                KOLLI-KDVALISO-EXP                        
481110                                KOLLI-FILLERX2                            
481200*                                                                         
481300     MOVE +1 TO FG-INDX                                                   
481400     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
481500       MOVE ZERO            TO KOLLI-IDPSN(FG-INDX)                       
481600                               KOLLI-VKART-FG(FG-INDX)                    
481700                               KOLLI-VLFG(FG-INDX)                        
481800       ADD +1 TO FG-INDX                                                  
481900     END-PERFORM                                                          
482000     MOVE ZERO              TO KOLLI-SUEQFG                               
482100                               KOLLI-DARFS                                
482200*                                                                         
482300     IF  WS-IDDISTR NUMERIC                                               
482400     AND WS-IDDISTR < '0800'                                              
482500     AND WS-ORDERVIS-TRANS                                                
482600     AND WS-IDKOLLI NUMERIC                                               
482700     AND WS-IDKOLLI > '99000'                                             
482800         MOVE 999999        TO  KOLLI-TILASTN                             
482900                                KOLLI-TIFAKT                              
483000                                KOLLI-TIFAKT-EXP                          
483100                                KOLLI-TIPACKN                             
483200     END-IF                                                               
483300     MOVE SPACE             TO  KOLLI-FLUTLAST                            
483400                                KOLLI-IDSUPREF                            
483500                                KOLLI-FLAUTFAK                            
483600                                KOLLI-FLTULLG                             
483700                                KOLLI-IDLBBET                             
483800                                KOLLI-IDTULFTG                            
483900                                KOLLI-KDSTASKLI                           
484000                                                                          
484100     COMPUTE KOLLI-VLORDBTO-KOLLI ROUNDED =                               
484200       KOLLI-DIKOLLIL * KOLLI-DIKOLLIH * KOLLI-DIKOLLIB / 1000000         
484300*--------------------------------------- KOLLI BREDD, HÖJD OCH            
484400*--------------------------------------- LÄNGD ANGIVNA I CM MEDAN         
484500*--------------------------------------- BRUTTOVOLYM I KUBIK M.           
484600     MOVE KOLLI-VLORDBTO-KOLLI TO WS-VLORDBTO                             
484700     .                                                                    
484800     EJECT                                                                
484900 S10-UPPD-SPAR-KOLLI    SECTION.                                          
485000                                                                          
485100     MOVE WS-IDDISTR-NUM     TO TEST-IDDISTR                              
485200                                                                          
485300*    COMPUTE ARB-KOLLI-VKORDNTO ROUNDED = ARB-KOLLI-VKORDNTO +            
485400                                                                          
485500     COMPUTE ARB-KOLLI-VKORDNTO         = ARB-KOLLI-VKORDNTO +            
485600                    SPAR-PRAD-VKARTNTO * SPAR-PRAD-KVLEVART               
485700*                                                                         
485800     IF DIST79-DEALER-PRICE                                               
485900      IF  ORAD-PRARTNTO-LOCPREL > ZERO                                    
486000       COMPUTE ARB-KOLLI-SUORDV-LOCPREL = ARB-KOLLI-SUORDV-LOCPREL        
486100           + SPAR-PRAD-PRARTNTO-LOCPREL * SPAR-PRAD-KVLEVART              
486200      ELSE                                                                
486300       COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC                
486400           + SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                  
486500      END-IF                                                              
486600     ELSE                                                                 
486700       IF DIST79-ECOM-PRICE                                               
486800         COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC              
486900           + SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                  
487000       ELSE                                                               
487100         COMPUTE ARB-KOLLI-SUORDV = ARB-KOLLI-SUORDV +                    
487200           SPAR-PRAD-PRARTNTO * SPAR-PRAD-KVLEVART                        
487300         COMPUTE ARB-KOLLI-SUORDV-EXP = ARB-KOLLI-SUORDV-EXP +            
487400           SPAR-PRAD-PRAVCOST * SPAR-PRAD-KVLEVART                        
487500       END-IF                                                             
487600     END-IF                                                               
487700     MOVE SPAR-PRAD-KDVALISO     TO ARB-KOLLI-KDVALISO                    
487800     MOVE SPAR-PRAD-KDVALISO-EXP TO ARB-KOLLI-KDVALISO-EXP                
487900*                                                                         
488000     IF   SPAR-PRAD-KVFLAMP   >  ZERO                                     
488100     AND (SPAR-PRAD-KVFLAMP   <  ARB-KOLLI-KVFLAMP                        
488200     OR   ARB-KOLLI-KVFLAMP   =  ZERO)                                    
488300       MOVE SPAR-PRAD-KVFLAMP  TO ARB-KOLLI-KVFLAMP                       
488400     END-IF                                                               
488500                                                                          
488600     IF  SPAR-PRAD-KDFARLIG  =  +2 OR +3 OR +4 OR +7                      
488700     AND SPAR-PRAD-KDFARLIG  >  ARB-KOLLI-KDFARLIG                        
488800       MOVE SPAR-PRAD-KDFARLIG TO ARB-KOLLI-KDFARLIG                      
488900     END-IF                                                               
489000*                                                                         
489100     .                                                                    
489200     EJECT                                                                
489300 S11-UPPD-KOLLI-FRAN-ARB   SECTION.                                       
489400                                                                          
489500     ADD ARB-KOLLI-KVFALRAD      TO KOLLI-KVFALRAD                        
489600     ADD ARB-KOLLI-KVORDRAD      TO KOLLI-KVORDRAD                        
489700     ADD ARB-KOLLI-VKORDNTO      TO KOLLI-VKORDNTO-KOLLI                  
489800     ADD ARB-KOLLI-SUORDV-LOC    TO KOLLI-SUORDV-LOC                      
489900     ADD ARB-KOLLI-SUORDV-LOCPREL TO KOLLI-SUORDV-LOCPREL                 
490000     ADD ARB-KOLLI-SUORDV        TO KOLLI-SUORDV-KOLLI                    
490100     ADD ARB-KOLLI-SUORDV-EXP    TO KOLLI-SUORDV-KLI-EXP                  
490200     MOVE ARB-KOLLI-KDVALISO     TO KOLLI-KDVALISO                        
490300     MOVE ARB-KOLLI-KDVALISO-EXP TO KOLLI-KDVALISO-EXP                    
490400*                                                                         
490500     IF      ARB-KOLLI-KVFLAMP   >  ZERO                                  
490600        AND (ARB-KOLLI-KVFLAMP   <  KOLLI-KVFLAMP-KOLLI                   
490700        OR   KOLLI-KVFLAMP-KOLLI =  ZERO)                                 
490800         MOVE ARB-KOLLI-KVFLAMP  TO KOLLI-KVFLAMP-KOLLI                   
490900     END-IF                                                               
491000*                                                                         
491100     IF ARB-KOLLI-KDFARLIG       >  KOLLI-KDFARLIG-KOLLI                  
491200         MOVE ARB-KOLLI-KDFARLIG TO KOLLI-KDFARLIG-KOLLI                  
491300     END-IF                                                               
491400     SKIP2                                                                
491500     MOVE WS-KDKOLLI             TO KOLLI-KDKOLLI                         
491600     MOVE WS-KDORDKL             TO KOLLI-KDORDKL                         
491700     MOVE WS-FLAUTFAK            TO KOLLI-FLAUTFAK                        
491800     MOVE WS-DIKOLLIL            TO KOLLI-DIKOLLIL                        
491900     MOVE WS-DIKOLLIH            TO KOLLI-DIKOLLIH                        
492000     MOVE WS-DIKOLLIB            TO KOLLI-DIKOLLIB                        
492100     MOVE WS-KDEMBTYP            TO KOLLI-KDEMBTYP                        
492200*                                                                         
492300     IF MID-VKORDBTO-KOLLI = ALL '+'                                      
492400* FOR DIST03-SVERIGE GROSS WEIGHT WILL BE REPORTED TO                     
492500         COMPUTE KOLLI-VKORDBTO-KOLLI ROUNDED =                           
492600                 ARB-KOLLI-VKORDNTO + WS-EMB-VKTARA-ONE-CASE              
492700         END-COMPUTE                                                      
492800     ELSE                                                                 
492900       MOVE WS-MOD-VKORDBTO         TO KOLLI-VKORDBTO-KOLLI               
493000     END-IF                                                               
493100                                                                          
493200     IF   KOLLI-VKORDNTO-KOLLI > KOLLI-VKORDBTO-KOLLI                     
493300          MOVE KOLLI-VKORDBTO-KOLLI TO KOLLI-VKORDNTO-KOLLI               
493400     END-IF                                                               
493500*                                                                         
493600     IF KOLLI-VKORDNTO-KOLLI >= KOLLI-VKORDBTO-KOLLI                      
493700       ADD 0.1                      TO KOLLI-VKORDBTO-KOLLI               
493800     END-IF                                                               
493900*                                                                         
494000     MOVE WS-IDTRPTNR                 TO KOLLI-IDTRPTNR                   
494100     MOVE WS-ADFLGEO                  TO KOLLI-ADFLGEO                    
494200     MOVE WS-ADFLOMR                  TO KOLLI-ADFLOMR                    
494300     MOVE WS-ADRUTNIV                 TO KOLLI-ADRUTNIV                   
494400     MOVE WS-DIHMODUL                 TO KOLLI-DIHMODUL                   
494500     MOVE WS-DIDMODUL                 TO KOLLI-DIDMODUL                   
494600     MOVE WS-ADVMODUL                 TO KOLLI-ADVMODUL                   
494700     MOVE WS-ADHMODUL                 TO KOLLI-ADHMODUL                   
494900     IF KOLLI-IDKOLLI-SAMP          >  ZERO                               
495000        MOVE NEJ                    TO KOLLI-FLUTLAST                     
495100     ELSE                                                                 
495200        MOVE WS-FLUTLAST           TO KOLLI-FLUTLAST                      
495300        IF KOLLI-FLAUTFAK = JA AND DIST03-SVERIGE-2                       
495400          MOVE NEJ                  TO KOLLI-FLUTLAST                     
495500        END-IF                                                            
495600     END-IF                                                               
495700                                                                          
495800     IF KOLLI-KDFARLIG-KOLLI = +4                                         
495900     OR KOLLI-KDFARLIG-KOLLI = +7                                         
496000       MOVE +950                    TO KOLLI-ADFLOMR                      
496100     END-IF                                                               
496200     .                                                                    
496300     EJECT                                                                
496400 S12-SKAPA-4322 SECTION.                                                  
496500                                                                          
496600*SKAPA INFO TILL SVENSKA ÅF, SÄNDS VIA VR.                                
496700     IF DIST03-SVERIGE-100-799                                            
496800     OR DIST03-NORGE                                                      
496900     OR DIST03-DANMARK-900                                                
497000     OR DIST85-PU-VIA-VR                                                  
497100     OR DIST21-TYRE                                                       
497200        MOVE WS-IDPRODNR TO XXJK-4322-IDPRODNR                            
497300        MOVE KOLLI-IDKOLLI  TO XXJK-4322-IDKOLLI                          
497400        MOVE XXJK-4322-WDGX4322 TO 4322-WDGX4322                          
497500        PERFORM IMS-ISRT-4322-SEGM                                        
497600     END-IF                                                               
497700     .                                                                    
497800     EJECT                                                                
497900 S13-UPPDAT-KDORDSTA SECTION.                                             
498000     MOVE WS-IDDISTR-NUM            TO   TEST-IDDISTR                     
498100     IF DIST19-SATS                                                       
498200*****  FIX-START-DEL2 930303 FÖR ATT TA HAND OM EN ORDER SOM              
498300*      SAKNAR ORDERHUVUD (WDQ2) OBS, VID ANVÄNDANDE ÖPPNA OCKSÅ           
498400*      FIX-DEL1 I SECTION FBBA-.                                          
498500*       OR WS-IDPRODNR = '0531053'                                        
498600*****  FIX-END-DEL2  930303                                               
498700       CONTINUE                                                           
498800     ELSE                                                                 
498900       PERFORM IMS-GU-ORQI01                                              
499000       IF SEGMENT-FINNS                                                   
499100          MOVE WS-IDDC           TO  W-IDDC                               
499200          PERFORM IMS-GHNP-ORQI12                                         
499300          IF ARB-KDORDSTA = 'U '                                          
499400            MOVE 'U*' TO ARB-KDORDSTA                                     
499500            PERFORM IMS-REPL-ORQI12                                       
499600          END-IF                                                          
499700       ELSE                                                               
499800         CONTINUE                                                         
499900       END-IF                                                             
500000     END-IF                                                               
500100     .                                                                    
500200     SKIP2                                                                
500300 S14-EV-SEND-PRINTTRANS SECTION.                                          
500400                                                                          
500500     MOVE WS-IDDISTR-NUM TO TEST-IDDISTR                                  
500600                                                                          
500700     IF WS-IDDC NOT = W-IDDC-B6                                           
500800        MOVE WS-IDDC TO W-IDDC-B6                                         
500900        PERFORM IMS-GU-WDB601                                             
501000     END-IF                                                               
501100     IF (DCS-CDC AND                                                      
501200         DIST07-USA-SUPPL-FROM-CDC)                                       
501300                                                                          
501400       MOVE MFS-RENSA-FAELT        TO MOD-PRTVAL-ADRESSFL                 
501500                                      MOD-PRTVAL-FOLJEFL                  
501600     ELSE                                                                 
501700       MOVE WS-KDPRTVAL-ADR     TO 4333-MID-KDPRTVAL-UT                   
501800       PERFORM S15-SEND-PRINTTRANS                                        
501900     END-IF                                                               
502000     .                                                                    
502100     SKIP2                                                                
502200 S15-SEND-PRINTTRANS SECTION.                                             
502300                                                                          
502400     MOVE WS-IDDISTR-NUM           TO 4333-MID-IDDISTR-UT                 
502500     MOVE WS-IDKUNDNR-NUM          TO 4333-MID-IDKUNDNR-UT                
502600     MOVE WS-IDORDNR               TO 4333-MID-IDORDNR-UT                 
502700     IF KOLLI-INTERVALL                                                   
502800       MOVE WS-IDKOLLI-FOM         TO 4333-MID-IDKOLLI-UT                 
502900       MOVE WS-IDKOLLI-TOM         TO 4333-MID-IDKOLLI-TOM                
503000     ELSE                                                                 
503100       MOVE WS-IDKOLLI             TO 4333-MID-IDKOLLI-UT                 
503200       MOVE ZERO                   TO 4333-MID-IDKOLLI-TOM                
503300     END-IF                                                               
503400     MOVE WS-IDDC                  TO 4333-MID-IDDC-UT                    
503500                                                                          
503600     IF DIRLEV-KOLLI                                                      
503700       MOVE WS-IDPRODNR            TO 4333-MID-IDPRODNR-UT                
503800     ELSE                                                                 
503900       MOVE ZERO                   TO 4333-MID-IDPRODNR-UT                
504000     END-IF                                                               
504100     MOVE '++++'                   TO 4333-MID-IDDISTR-IN                 
504200     MOVE '++++++'                 TO 4333-MID-IDKUNDNR-IN                
504300     MOVE '+++++'                  TO 4333-MID-IDORDNR-IN                 
504400                                      4333-MID-IDKOLLI-IN                 
504500     MOVE '++'                     TO 4333-MID-IDDC-IN                    
504600     MOVE '+++++++'                TO 4333-MID-IDPRODNR-IN                
504700     MOVE '++'                     TO 4333-MID-KDPRTVAL-IN                
504800                                                                          
504900     COMPUTE 4333-MID-LL = LENGTH OF 4333-MID-W4I33301 + 17               
505000     MOVE 'W4T333  '               TO 4333-MID-TRANSKOD                   
505100     MOVE '431E'                   TO 4333-MID-IDTRANS                    
505200     MOVE WS-KDMFSFOR              TO 4333-MID-KDMFSFOR                   
505300                                                                          
505400     PERFORM IMS-PURGE-ALT4333-MSG                                        
505500     .                                                                    
505600     EJECT                                                                
505700 S16-UPPD-FARLIGT-GODS-DATA SECTION.                                      
505800     SKIP3                                                                
505900     MOVE +1 TO FG-INDX                                                   
506000     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
506100                                                                          
506200       IF TAB-IDPSN(FG-INDX) > ZERO                                       
506300         MOVE TAB-IDPSN(FG-INDX)    TO KOLLI-IDPSN(FG-INDX)               
506400         COMPUTE KOLLI-VKART-FG(FG-INDX) =                                
506500                                       TAB-VKART-FG(FG-INDX) /            
506600                                       ARB-ANTAL-KOLLI                    
506700         COMPUTE KOLLI-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) /                
506800                                       ARB-ANTAL-KOLLI                    
506900                                                                          
507000       ELSE                                                               
507100         MOVE ZERO                  TO KOLLI-IDPSN(FG-INDX)               
507200                                       KOLLI-VKART-FG(FG-INDX)            
507300                                       KOLLI-VLFG(FG-INDX)                
507400       END-IF                                                             
507500                                                                          
507600       ADD +1 TO FG-INDX                                                  
507700     END-PERFORM                                                          
507800                                                                          
507900     IF TAB-IDPSN(1) > ZERO                                               
508000       COMPUTE KOLLI-SUEQFG = TOTAL-SUEQFG / ARB-ANTAL-KOLLI              
508100     ELSE                                                                 
508200       MOVE ZERO TO KOLLI-SUEQFG                                          
508300     END-IF                                                               
508400     .                                                                    
508500     EJECT                                                                
508600 S17-BERAEKNA-FG-FAELT SECTION.                                           
508700     SKIP3                                                                
508800     COMPUTE TAB-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) +                      
508900                                 (SPAR-VLFG        *                      
509000                                  KKOLLI-KVLEVART)                        
509100     IF SPAR-IDPSN = 10 OR 11                                             
509200       COMPUTE TAB-VKART-FG(FG-INDX) = TAB-VKART-FG(FG-INDX) +            
509300                                       (SPAR-VKART-FG        *            
509400                                        KKOLLI-KVLEVART)                  
509500     ELSE                                                                 
509600       MOVE ZERO TO TAB-VKART-FG(FG-INDX)                                 
509700     END-IF                                                               
509800                                                                          
509900     MOVE 10 TO FG-INDX                                                   
510000     .                                                                    
510100     EJECT                                                                
510200 S18-SKAPA-FOLJESEDEL-TRANS SECTION.                                      
510300                                                                          
510400     MOVE '++++'             TO  4341-MID-IDDISTR-IN                      
510500     MOVE WS-IDDISTR-NUM     TO  4341-MID-IDDISTR-UT                      
510600     MOVE '++++++'           TO  4341-MID-IDKUNDNR-IN                     
510700     MOVE WS-IDKUNDNR-NUM    TO  4341-MID-IDKUNDNR-UT                     
510800     MOVE '+++++'            TO  4341-MID-IDORDNR-IN                      
510900     MOVE WS-IDORDNR         TO  4341-MID-IDORDNR-UT                      
511000     MOVE '+'                TO  4341-MID-IDPLKLST-IN                     
511100     MOVE ZERO               TO  4341-MID-IDPLKLST-UT                     
511200     IF  KOLLI-INTERVALL                                                  
511300         MOVE '+++++'        TO 4341-MID-IDKOLLI-IN                       
511400         MOVE WS-IDKOLLI-NUM TO 4341-MID-IDKOLLI-UT                       
511500         MOVE '+++++'        TO 4341-MID-IDKOLLI-TOM-IN                   
511600         MOVE ZERO           TO 4341-MID-IDKOLLI-TOM-UT                   
511700     ELSE                                                                 
511800         MOVE '+++++'        TO 4341-MID-IDKOLLI-IN                       
511900         MOVE WS-IDKOLLI     TO 4341-MID-IDKOLLI-UT                       
512000         MOVE '+++++'        TO 4341-MID-IDKOLLI-TOM-IN                   
512100         MOVE ZERO           TO 4341-MID-IDKOLLI-TOM-UT                   
512200     END-IF                                                               
512300     MOVE '++'               TO 4341-MID-KDPRTVAL-IN                      
512400     MOVE WS-KDPRTVAL-FS     TO 4341-MID-KDPRTVAL-UT                      
512500     MOVE '++'               TO 4341-MID-IDDC-IN                          
512600     MOVE WS-IDDC            TO 4341-MID-IDDC-UT                          
512700     MOVE 'N'                TO 4341-MID-FL-SVENSK-FSEDEL                 
512800                                                                          
512900     MOVE 'W4T341  '         TO 4341-MID-TRANSKOD                         
513000     MOVE '431E'             TO 4341-MID-IDTRANS                          
513100                                                                          
513200     COMPUTE 4341-MID-LL = LENGTH OF 4341-MID-W4I34101 + 17               
513300     MOVE WS-KDMFSFOR        TO 4341-MID-KDMFSFOR                         
513400                                                                          
513500     PERFORM IMS-PURGE-ALT4341-MSG                                        
513600     .                                                                    
513700     SKIP2                                                                
513800 S19-CONVERT-LB-TO-KG                     SECTION.                        
513900                                                                          
514000*    COMPUTE WS-MOD-VKORDBTO ROUNDED =                                    
514100     COMPUTE WS-MOD-VKORDBTO =                                            
514200             WS-MOD-VKORDBTO * CONV-LB-TO-KG                              
514300     END-COMPUTE                                                          
514400     .                                                                    
514500     SKIP2                                                                
514600                                                                          
514700 S20-DATA-TILL-DEL-NOTE SECTION.                                          
514800                                                                          
514900     MOVE WS-IDDISTR-NUM           TO TEST-IDDISTR                        
515000     IF DIST07-USA-RETAILER-DNOTE                                         
515100     OR DIST07-CAN-RETAILER                                               
515200        INITIALIZE DNOT-ORDER-INFO                                        
515300                                                                          
515400        MOVE PROGRAM-NAMN             TO DNOT-IDPGM                       
515500        MOVE WS-SPAR-IDORDER          TO DNOT-IDORDER                     
515600        MOVE WS-SPAR-IDARTNR          TO DNOT-IDARTNR                     
515700        MOVE WS-SPAR-IDDC             TO DNOT-IDDC                        
515800        MOVE WS-SPAR-BEART            TO DNOT-BEART-USA                   
515900        MOVE WS-SPAR-KVBEART          TO DNOT-KVBEART                     
516000        MOVE WS-SPAR-FLTILLK          TO DNOT-FLTILLK                     
516100        MOVE WS-SPAR-IDKUNDRF-RO      TO DNOT-IDKUNDRF-RO                 
516200        MOVE WS-SPAR-IDPURAD          TO DNOT-IDPURAD                     
516300        MOVE KKOLLI-IDKOLLI           TO DNOT-IDKOLLI                     
516400        MOVE KKOLLI-IDPRODNR          TO DNOT-IDPRODNR                    
516500        MOVE KKOLLI-KVLEVART          TO DNOT-KVLEVART                    
516600                                                                          
516700        CALL W411DNOT USING DNOT-W411DNOT                                 
516800                            DNOT-ORQP-PCB                                 
516900                            DNOT-ORQP2-PCB                                
517000                            DNOT-ORQP3-PCB                                
517100                            DNOT-4013-PCB                                 
517200                            DNOT-BENA-PCB                                 
517300     END-IF                                                               
517400     .                                                                    
517500                                                                          
517600     EJECT                                                                
517700 S21-SEND-OPEN SECTION.                                                   
517800                                                                          
517900     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
518000     MOVE 'OPEN'                  TO SEND-KDFUNC                          
518100     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
518200                                     SEND-OPEN-AREA                       
518300     IF SEND-KDRC > ZERO                                                  
518400       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
518500       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
518600       DELIMITED BY SIZE INTO FELTEXT                                     
518700       DISPLAY FELTEXT                                                    
518800       CALL FELLOG                                                        
518900     END-IF                                                               
519000     .                                                                    
519100     EJECT                                                                
519200 S22-PUT-HEADER        SECTION.                                           
519300                                                                          
519400     MOVE 001                    TO HDR-REQU-IDMSGVER                     
519500     MOVE 'R'                    TO HDR-REQU-KDPGMACT                     
519600     MOVE PROGRAM-NAMN           TO HDR-REQU-IDUSER                       
519700     MOVE 'WRONGWEIGHT'          TO HDR-IDOUTTYPE                         
519800     MOVE '002'                  TO HDR-IDOUTREC                          
519900     MOVE '002'                  TO HDR-IDLIST                            
520000     MOVE 'PUT'                  TO SEND-KDFUNC                           
520100     MOVE LENGTH OF HDR-AREA     TO SEND-KVDLEN                           
520200     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
520300                                    SEND-KVDLEN                           
520400                                    HDR-AREA                              
520500     IF SEND-KDRC > ZERO                                                  
520600       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
520700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
520800       DELIMITED BY SIZE INTO FELTEXT                                     
520900       DISPLAY FELTEXT                                                    
521000       CALL FELLOG                                                        
521100     END-IF                                                               
521200     .                                                                    
521300 S23-MOVE-LINEDATA SECTION.                                               
521400     MOVE WS-IDDISTR-NUM           TO HEAD-DIST                           
521500     MOVE WS-IDKUNDNR-NUM          TO HEAD-IDKUNDNR                       
521600     MOVE WS-IDORDNR               TO HEAD-IDORDER                        
521700     IF WS-IDKOLLI > ZERO                                                 
521800        MOVE WS-IDKOLLI            TO HEAD-IDKOLLI                        
521900     ELSE                                                                 
522000       IF WS-IDKOLLI-FOM > ZERO                                           
522100          MOVE WS-IDKOLLI-FOM      TO HEAD-IDKOLLI                        
522200          MOVE '-'                 TO HEAD-FILLER                         
522300          MOVE WS-IDKOLLI-TOM      TO HEAD-IDKOLLI2                       
522400       END-IF                                                             
522500     END-IF                                                               
522600     MOVE WS-KDKOLLI               TO HEAD-KDKOLLI                        
522700     MOVE WS-IDANSTNR              TO HEAD-IDPLKLST                       
522800     MOVE WS-MOD-VKORDBTO          TO HEAD-VKORDBTO                       
522900     MOVE WS-DAGENS-DATUM          TO HEAD-DATE                           
523000     MOVE WS-TIDPUNKT              TO HEAD-TIME                           
523100     .                                                                    
523200 S24-WRITE-LINE  SECTION.                                                 
523300     MOVE 1 TO LINE-IX                                                    
523400     PERFORM UNTIL LINE-IX > 16                                           
523500        MOVE TAB-LINE(LINE-IX)      TO SEND-AREA                          
523600        PERFORM  S25-PUT-LINE                                             
523700        ADD 1 TO LINE-IX                                                  
523800     END-PERFORM                                                          
523900                                                                          
524000     MOVE 1 TO LINE-IX                                                    
524100     PERFORM UNTIL LINE-IX > MAX-TAB                                      
524200        MOVE LINE-TAB(LINE-IX)      TO SEND-AREA                          
524300        PERFORM  S25-PUT-LINE                                             
524400        ADD 1 TO LINE-IX                                                  
524500     END-PERFORM                                                          
524600                                                                          
524700      IF MAX-RAD > MAX-LINES                                              
524800        MOVE LINE-END     TO SEND-AREA                                    
524900        PERFORM  S25-PUT-LINE                                             
525000      END-IF                                                              
525100     .                                                                    
525200 S25-PUT-LINE     SECTION.                                                
525300                                                                          
525400     MOVE 'PUT'                     TO SEND-KDFUNC                        
525500     MOVE LENGTH OF SEND-AREA       TO SEND-KVDLEN                        
525600     CALL WZ01SEND               USING SEND-CONTROL-AREA                  
525700                                       SEND-KVDLEN                        
525800                                       SEND-AREA                          
525900     IF SEND-KDRC > ZERO                                                  
526000       MOVE SEND-KDRC               TO KDRC-DISPLAY                       
526100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
526200       DELIMITED BY SIZE INTO FELTEXT                                     
526300       DISPLAY FELTEXT                                                    
526400       CALL FELLOG                                                        
526500     END-IF                                                               
526600     .                                                                    
526700 S25-SEND-CLOSE SECTION.                                                  
526800                                                                          
526900     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
527000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
527100                                                                          
527200     IF SEND-KDRC > 0                                                     
527300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
527400       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
527500       DELIMITED BY SIZE INTO FELTEXT                                     
527600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
527700     END-IF                                                               
527800     .                                                                    
527900     EJECT                                                                
528000* IMS SEKTIONER                                                           
528100                                                                          
528200 IMS-GET-MSG SECTION.                                                     
528300                                                                          
528400     MOVE '  QC' TO GODK-STATUSKODER                                      
528500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
528600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
528700     PERFORM IMS-STATUSKONTROLL                                           
528800     SKIP3                                                                
528900     .                                                                    
529000 IMS-INSERT-MSG SECTION.                                                  
529100                                                                          
529200     IF NOT ENGLISH-TEXT                                                  
529300       MOVE '0' TO MFS-KDHUVOMR                                           
529400     END-IF                                                               
529500*                                                                         
529600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
529700     MOVE SPACE TO GODK-STATUSKODER                                       
529800                                                                          
529900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
530000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
530100     PERFORM IMS-STATUSKONTROLL                                           
530200     .                                                                    
530300     EJECT                                                                
530400 IMS-INSERT-ALT0605MSG SECTION.                                           
530500                                                                          
530600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
530700     MOVE SPACE TO GODK-STATUSKODER                                       
530800     CALL CBLTDLI USING ISRT ALT0605-PCB MSG-IO-AREA                      
530900     MOVE ALT0605-STATUS-CODE TO STATUS-WS                                
531000     PERFORM IMS-STATUSKONTROLL                                           
531100     SKIP3                                                                
531200     .                                                                    
531300 IMS-PURGE-ALT4333-MSG SECTION.                                           
531400                                                                          
531500     MOVE LOW-VALUE TO 4333-MID-Z1 4333-MID-Z2                            
531600     MOVE SPACE TO GODK-STATUSKODER                                       
531700     CALL CBLTDLI USING PURG                                              
531800                        ALT4333-PCB                                       
531900                        4333-MID-IO-AREA                                  
532000     MOVE ALT4333-STATUS-CODE TO STATUS-WS                                
532100     PERFORM IMS-STATUSKONTROLL                                           
532200     SKIP3                                                                
532300     .                                                                    
532400 IMS-PURGE-ALT4341-MSG SECTION.                                           
532500                                                                          
532600     MOVE LOW-VALUE TO 4341-MID-Z1 4341-MID-Z2                            
532700     MOVE SPACE TO GODK-STATUSKODER                                       
532800     CALL CBLTDLI USING PURG                                              
532900                        ALT4341-PCB                                       
533000                        4341-MID-IO-AREA                                  
533100     MOVE ALT4341-STATUS-CODE TO STATUS-WS                                
533200     PERFORM IMS-STATUSKONTROLL                                           
533300     SKIP3                                                                
533400     .                                                                    
533500 IMS-CHANGE-ALTMSG       SECTION.                                         
533600                                                                          
533700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
533800     MOVE '  ' TO GODK-STATUSKODER                                        
533900     CALL CBLTDLI USING CHNG                                              
534000                          ALT-PCB                                         
534100                          MSG-KDTRANS-1                                   
534200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
534300     PERFORM IMS-STATUSKONTROLL                                           
534400     SKIP3                                                                
534500     .                                                                    
534600 IMS-INSERT-ALTMSG SECTION.                                               
534700                                                                          
534800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
534900     MOVE SPACE TO GODK-STATUSKODER                                       
535000     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
535100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
535200     PERFORM IMS-STATUSKONTROLL                                           
535300     .                                                                    
535400     EJECT                                                                
535500 IMS-GU-WDE401 SECTION.                                                   
535600     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
535700            DELIMITED BY SIZE INTO SSA1                                   
535800     MOVE '    ' TO GODK-STATUSKODER                                      
535900     CALL CBLTDLI USING GU     WDE41-PCB DLI-IO-E401 SSA1                 
536000     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
536100     PERFORM IMS-STATUSKONTROLL                                           
536200     SKIP3                                                                
536300     .                                                                    
536400 IMS-GU-WDE601    SECTION.                                                
536500                                                                          
536600     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
536700            DELIMITED BY SIZE INTO SSA1                                   
536800     MOVE '  GE' TO GODK-STATUSKODER                                      
536900     CALL CBLTDLI USING GU    WDE62-PCB DLI-IO-AREA2 SSA1                 
537000     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
537100     PERFORM IMS-STATUSKONTROLL                                           
537200     SKIP3                                                                
537300     .                                                                    
537400 IMS-GU-KUNDORDER-SEK-INV SECTION.                                        
537500     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
537600                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
537700            DELIMITED BY SIZE INTO SSA1                                   
537800     MOVE 'WDE401   ' TO SSA2                                             
537900     MOVE '  GE' TO GODK-STATUSKODER                                      
538000     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
538100     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
538200     PERFORM IMS-STATUSKONTROLL                                           
538300     .                                                                    
538400     SKIP3                                                                
538500 IMS-GU-WDE42-KORD-BSEQ SECTION.                                          
538600     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
538700            DELIMITED BY SIZE INTO SSA1                                   
538800     MOVE 'WDE401   ' TO SSA2                                             
538900     MOVE '  ' TO GODK-STATUSKODER                                        
539000     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
539100     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
539200     PERFORM IMS-STATUSKONTROLL                                           
539300     .                                                                    
539400     SKIP3                                                                
539500 IMS-GHU-RAD-SEK  SECTION.                                                
539600     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
539700            DELIMITED BY SIZE INTO SSA1                                   
539800     MOVE '    ' TO GODK-STATUSKODER                                      
539900     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-E411 SSA1                  
540000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
540100     PERFORM IMS-STATUSKONTROLL                                           
540200     SKIP3                                                                
540300     .                                                                    
540400 IMS-GHN-RAD-SEK  SECTION.                                                
540500     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
540600                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
540700            DELIMITED BY SIZE INTO SSA1                                   
540800     MOVE '  ' TO GODK-STATUSKODER                                        
540900     CALL CBLTDLI USING GHN    WDE4-PCB DLI-IO-E411 SSA1                  
541000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
541100     PERFORM IMS-STATUSKONTROLL                                           
541200     .                                                                    
541300     SKIP2                                                                
541400 IMS-REPL-BEHANDLAD-RAD SECTION.                                          
541500     MOVE '    ' TO GODK-STATUSKODER                                      
541600     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E411                         
541700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
541800     PERFORM IMS-STATUSKONTROLL                                           
541900     SKIP3                                                                
542000     .                                                                    
542100 IMS-GHNP-KOLLI-KOPPL    SECTION.                                         
542200     STRING 'WDE421  *F(WDE421KY =' W-WDE421-IDKOLLI-X ')'                
542300            DELIMITED BY SIZE INTO SSA1                                   
542400     MOVE '  ' TO GODK-STATUSKODER                                        
542500     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E421 SSA1                    
542600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
542700     PERFORM IMS-STATUSKONTROLL                                           
542800     SKIP3                                                                
542900     .                                                                    
543000 IMS-REPL-KOLLI-KOPPL  SECTION.                                           
543100     MOVE '  '   TO GODK-STATUSKODER                                      
543200     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E421                         
543300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
543400     PERFORM IMS-STATUSKONTROLL                                           
543500     SKIP2                                                                
543600     .                                                                    
543700 IMS-ISRT-KOLLI-KOPPL  SECTION.                                           
543800     MOVE   'WDE421   '       TO   SSA1                                   
543900     MOVE '  II' TO GODK-STATUSKODER                                      
544000     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-E421 SSA1                    
544100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
544200     PERFORM IMS-STATUSKONTROLL                                           
544300     .                                                                    
544400     SKIP2                                                                
544500 IMS-GHU-KOLLIREG SECTION.                                                
544600     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
544700            DELIMITED BY SIZE INTO SSA1                                   
544800     MOVE '    ' TO GODK-STATUSKODER                                      
544900     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-AREA2 SSA1                 
545000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
545100     PERFORM IMS-STATUSKONTROLL                                           
545200     .                                                                    
545300     SKIP2                                                                
545400 IMS-REPL-KOLLIREG SECTION.                                               
545500     MOVE '    ' TO GODK-STATUSKODER                                      
545600     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-AREA2                        
545700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
545800     PERFORM IMS-STATUSKONTROLL                                           
545900     SKIP3                                                                
546000     .                                                                    
546100 IMS-GHU-KOLLI    SECTION.                                                
546200     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
546300            DELIMITED BY SIZE INTO SSA1                                   
546400     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
546500            DELIMITED BY SIZE INTO SSA2                                   
546600     MOVE '  GE' TO GODK-STATUSKODER                                      
546700     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-AREA2 SSA1 SSA2            
546800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
546900     PERFORM IMS-STATUSKONTROLL                                           
547000     SKIP3                                                                
547100     .                                                                    
547200 IMS-GHNP-KOLLI    SECTION.                                               
547300     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
547400            DELIMITED BY SIZE INTO SSA1                                   
547500     MOVE '  GE' TO GODK-STATUSKODER                                      
547600     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-AREA2 SSA1                   
547700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
547800     PERFORM IMS-STATUSKONTROLL                                           
547900     SKIP3                                                                
548000     .                                                                    
548100 IMS-REPL-KOLLI    SECTION.                                               
548200     MOVE '    ' TO GODK-STATUSKODER                                      
548300     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-AREA2                        
548400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
548500     PERFORM IMS-STATUSKONTROLL                                           
548600     SKIP3                                                                
548700     .                                                                    
548800 IMS-ISRT-KOLLI   SECTION.                                                
548900     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
549000            DELIMITED BY SIZE INTO SSA1                                   
549100     MOVE   'WDE611   '       TO   SSA2                                   
549200     MOVE '  II' TO GODK-STATUSKODER                                      
549300     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-AREA2 SSA1 SSA2              
549400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
549500     PERFORM IMS-STATUSKONTROLL                                           
549600     .                                                                    
549700     SKIP2                                                                
549710 IMS-ISRT-WDE621 SECTION.                                                 
549720                                                                          
549730     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
549740          DELIMITED BY SIZE INTO SSA1                                     
549750     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
549760          DELIMITED BY SIZE INTO SSA2                                     
549770     MOVE 'WDE621 ' TO SSA3                                               
549780     MOVE '  II' TO GODK-STATUSKODER                                      
549790     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE621 SSA1 SSA2 SSA3        
549791     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
549792     PERFORM IMS-STATUSKONTROLL                                           
549793     .                                                                    
549794     EJECT                                                                
549800 IMS-GET-EMBB     SECTION.                                                
549900     STRING 'WLEMBB01(KDKOLLI  =' W-KDKOLLI-WDK5 ')'                      
550000            DELIMITED BY SIZE INTO SSA1                                   
550100     MOVE '  GE' TO GODK-STATUSKODER                                      
550200     CALL CBLTDLI USING GHU    EMBB-PCB DLI-IO-K501 SSA1                  
550300     MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
550400     PERFORM IMS-STATUSKONTROLL                                           
550500     .                                                                    
550600     SKIP2                                                                
550700 IMS-GU-KUNDORDER-SEK SECTION.                                            
550800     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
550900            DELIMITED BY SIZE INTO SSA1                                   
551000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
551100     CALL CBLTDLI USING GU   WDE4A-PCB DLI-IO-E401 SSA1                   
551200     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
551300                               STATUS-KUNDORDER-SEK-WS                    
551400     PERFORM IMS-STATUSKONTROLL                                           
551500     SKIP2                                                                
551600     .                                                                    
551700 IMS-GN-SEQA-WDE4A1 SECTION.                                              
551800     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
551900            DELIMITED BY SIZE INTO SSA1                                   
552000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
552100     CALL CBLTDLI USING GN   WDE4A-PCB DLI-IO-E401 SSA1                   
552200     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
552300                               STATUS-KUNDORDER-SEK-WS                    
552400     PERFORM IMS-STATUSKONTROLL                                           
552500     SKIP2                                                                
552600     .                                                                    
552700 IMS-GU-RAD         SECTION.                                              
552800     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
552900            DELIMITED BY SIZE INTO SSA1                                   
553000     STRING 'WDE411  (IDPURAD  =' W-WDE411-IDPURAD-X ')'                  
553100            DELIMITED BY SIZE INTO SSA2                                   
553200     MOVE '  GE' TO GODK-STATUSKODER                                      
553300     CALL CBLTDLI USING GU     WDE41-PCB DLI-IO-E411 SSA1 SSA2            
553400     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
553500     PERFORM IMS-STATUSKONTROLL                                           
553600     .                                                                    
553700     SKIP2                                                                
553800 IMS-GU-4726-ROT-KVAL SECTION.                                            
553900     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
554000            DELIMITED BY SIZE INTO SSA1                                   
554100     MOVE '  ' TO GODK-STATUSKODER                                        
554200     CALL CBLTDLI USING GU     XXDV-PCB DLI-IO-AREA3 SSA1                 
554300     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
554400     PERFORM IMS-STATUSKONTROLL                                           
554500     SKIP2                                                                
554600     .                                                                    
554700 IMS-GNP-4726-UNDERSEG-KVAL SECTION.                                      
554800     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
554900            DELIMITED BY SIZE INTO SSA1                                   
555000     MOVE '  GE' TO GODK-STATUSKODER                                      
555100     CALL CBLTDLI USING GNP    XXDV-PCB DLI-IO-AREA3 SSA1                 
555200     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
555300     PERFORM IMS-STATUSKONTROLL                                           
555400     SKIP2                                                                
555500     .                                                                    
555600 IMS-INSERT-4726-UNDERSEG SECTION.                                        
555700     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
555800            DELIMITED BY SIZE INTO SSA1                                   
555900     MOVE 'WLXXDV11 ' TO SSA2                                             
556000     MOVE '  ' TO GODK-STATUSKODER                                        
556100     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-AREA3 SSA1 SSA2              
556200     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
556300     PERFORM IMS-STATUSKONTROLL                                           
556400     SKIP2                                                                
556500     .                                                                    
556600 IMS-INSERT-4727 SECTION.                                                 
556700     STRING 'WLXXDV01(WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
556800            DELIMITED BY SIZE INTO SSA1                                   
556900     STRING 'WLXXDV11(WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
557000            DELIMITED BY SIZE INTO SSA2                                   
557100     MOVE 'WLXXDV21 ' TO SSA3                                             
557200     MOVE '  II' TO GODK-STATUSKODER                                      
557300     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-AREA3                        
557400                               SSA1 SSA2 SSA3                             
557500     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
557600     PERFORM IMS-STATUSKONTROLL                                           
557700     .                                                                    
557800     SKIP2                                                                
557900 IMS-ISRT-4322-SEGM SECTION.                                              
558000     STRING 'WLXXJK01(WDGXKEY  =' W-4321-IDHTYP-X ')'                     
558100            DELIMITED BY SIZE INTO SSA1                                   
558200     MOVE 'WLXXJK11*L' TO SSA2                                            
558300     MOVE '  ' TO GODK-STATUSKODER                                        
558400     CALL CBLTDLI USING ISRT XXJK-PCB DLI-IO-AREA4 SSA1 SSA2              
558500     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
558600     PERFORM IMS-STATUSKONTROLL                                           
558700     .                                                                    
558800     SKIP2                                                                
558900 IMS-GU-ORQA01    SECTION.                                                
559000     STRING 'WLORQA01(WDQ301KY =' W-WDQ301-ORDERDEL-X ')'                 
559100            DELIMITED BY SIZE INTO SSA1                                   
559200     MOVE '  ' TO GODK-STATUSKODER                                        
559300     CALL CBLTDLI USING GU    ORQA-PCB DLI-IO-AREA5 SSA1                  
559400     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
559500     PERFORM IMS-STATUSKONTROLL                                           
559600     .                                                                    
559700     SKIP2                                                                
559800 IMS-GU-ORQI01    SECTION.                                                
559900     STRING 'WLORQI01(IDORDER  =' W-WDQ201-X ')'                          
560000            DELIMITED BY SIZE INTO SSA1                                   
560100     MOVE '  GE' TO GODK-STATUSKODER                                      
560200     CALL CBLTDLI USING GU   ORQI-PCB DLI-IO-Q201 SSA1                    
560300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
560400     PERFORM IMS-STATUSKONTROLL                                           
560500     .                                                                    
560600 IMS-GHNP-ORQI12    SECTION.                                              
560700                                                                          
560800     STRING 'WLORQI12*F(IDDC     =' W-IDDC-X ')'                          
560900            DELIMITED BY SIZE INTO SSA1                                   
561000     MOVE '    ' TO GODK-STATUSKODER                                      
561100     CALL CBLTDLI USING GHNP ORQI-PCB DLI-IO-Q212 SSA1                    
561200     MOVE ORQI-STATUS-CODE       TO STATUS-WS                             
561300     PERFORM IMS-STATUSKONTROLL                                           
561400     .                                                                    
561500     SKIP2                                                                
561600 IMS-REPL-ORQI12      SECTION.                                            
561700                                                                          
561800     MOVE '    ' TO GODK-STATUSKODER                                      
561900     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-Q212                         
562000     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
562100     PERFORM IMS-STATUSKONTROLL                                           
562200     .                                                                    
562300 IMS-GHU-XXKW11       SECTION.                                            
562400     STRING 'WLXXKW01(WDGXKEY  =' W-4471-WDGXKEY-X ')'                    
562500            DELIMITED BY SIZE INTO SSA1                                   
562600     STRING 'WLXXKW11(KDSEGKEY =' W-4472-KDSEGKEY-X ')'                   
562700            DELIMITED BY SIZE INTO SSA2                                   
562800     MOVE '  GE' TO GODK-STATUSKODER                                      
562900     CALL CBLTDLI USING GHU    XXKW-PCB DLI-IO-AREA6 SSA1 SSA2            
563000     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
563100     PERFORM IMS-STATUSKONTROLL                                           
563200     .                                                                    
563300 IMS-REPL-XXKW11    SECTION.                                              
563400     MOVE '  '   TO GODK-STATUSKODER                                      
563500     CALL CBLTDLI USING REPL XXKW-PCB DLI-IO-AREA6                        
563600     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
563700     PERFORM IMS-STATUSKONTROLL                                           
563800     .                                                                    
563900     SKIP2                                                                
564000 IMS-GU-XXLB         SECTION.                                             
564100     STRING 'WLXXLB01(WDGXKEY  =' W-4477-WDGXKEY-X ')'                    
564200            DELIMITED BY SIZE INTO SSA1                                   
564300     STRING 'WLXXLB11(WDGXKEY  =' W-4478-WDGXKEY-X ')'                    
564400            DELIMITED BY SIZE INTO SSA2                                   
564500     MOVE '  GE' TO GODK-STATUSKODER                                      
564600     CALL CBLTDLI USING GU    XXLB-PCB DLI-IO-AREA6 SSA1 SSA2             
564700     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
564800     PERFORM IMS-STATUSKONTROLL                                           
564900     .                                                                    
565000     SKIP2                                                                
565100 IMS-ISRT-ZZAC01 SECTION.                                                 
565200     MOVE 'WLZZAC01' TO SSA1                                              
565300     MOVE '  II'     TO GODK-STATUSKODER                                  
565400     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA7 SSA1                   
565500     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
565600     PERFORM IMS-STATUSKONTROLL                                           
565700     .                                                                    
565800     SKIP2                                                                
565900                                                                          
566000 IMS-GET-ORQI01-CSEQ SECTION.                                             
566100                                                                          
566200     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
566300             DELIMITED BY SIZE INTO    SSA1                               
566400     MOVE    '  GE'              TO    GODK-STATUSKODER                   
566500     CALL    CBLTDLI             USING GU   ORQL-PCB                      
566600                                            DLI-IO-Q201 SSA1              
566700     MOVE    ORQL-STATUS-CODE    TO    STATUS-WS                          
566800     PERFORM IMS-STATUSKONTROLL                                           
566900     .                                                                    
567000     SKIP2                                                                
567100 IMS-GHU-XXDU01      SECTION.                                             
567200                                                                          
567300     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
567400            DELIMITED BY SIZE INTO SSA1                                   
567500     MOVE '  GE' TO GODK-STATUSKODER                                      
567600     CALL CBLTDLI USING GHU XXDU-PCB DLI-IO-AREA9 SSA1                    
567700     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
567800     PERFORM IMS-STATUSKONTROLL                                           
567900     .                                                                    
568000     SKIP2                                                                
568100 IMS-ISRT-XXDU01     SECTION.                                             
568200                                                                          
568300     MOVE 'WLXXDU01 '  TO SSA1                                            
568400     MOVE '    ' TO GODK-STATUSKODER                                      
568500     CALL CBLTDLI USING ISRT XXDU-PCB DLI-IO-AREA9 SSA1                   
568600     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
568700     PERFORM IMS-STATUSKONTROLL                                           
568800     .                                                                    
568900     SKIP2                                                                
569000 IMS-GNP-XXDU11      SECTION.                                             
569100                                                                          
569200     STRING 'WLXXDU11(WDGXKEY  =' W-4302-WDGXKEY-X ')'                    
569300            DELIMITED BY SIZE INTO SSA1                                   
569400     MOVE '  GE' TO GODK-STATUSKODER                                      
569500     CALL CBLTDLI USING GNP   XXDU-PCB DLI-IO-AREA9 SSA1                  
569600     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
569700     PERFORM IMS-STATUSKONTROLL                                           
569800     .                                                                    
569900     SKIP2                                                                
570000 IMS-ISRT-XXDU11     SECTION.                                             
570100                                                                          
570200     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
570300            DELIMITED BY SIZE INTO SSA1                                   
570400     MOVE 'WLXXDU11 '  TO SSA2                                            
570500     MOVE '    ' TO GODK-STATUSKODER                                      
570600     CALL CBLTDLI USING ISRT XXDU-PCB DLI-IO-AREA9 SSA1 SSA2              
570700     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
570800     PERFORM IMS-STATUSKONTROLL                                           
570900     .                                                                    
571000                                                                          
571100 IMS-GU-WDB601    SECTION.                                                
571200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
571300          DELIMITED BY SIZE INTO SSA1                                     
571400     MOVE '  '   TO GODK-STATUSKODER                                      
571500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
571600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
571700     PERFORM IMS-STATUSKONTROLL                                           
571800     .                                                                    
571900     SKIP2                                                                
572000 IMS-GHN-WDA6B SECTION.                                                   
572100     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
572200                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
572300            DELIMITED BY SIZE INTO SSA1                                   
572400     MOVE '  GEGB'               TO GODK-STATUSKODER                      
572500     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA-A601 SSA1           
572600     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
572700     PERFORM IMS-STATUSKONTROLL                                           
572800     .                                                                    
572900     SKIP2                                                                
573000 IMS-REPL-WDA6B SECTION.                                                  
573100     MOVE 'WDA601  '           TO SSA1                                    
573200     MOVE '    '               TO GODK-STATUSKODER                        
573300     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA-A601 SSA1            
573400     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
573500     PERFORM IMS-STATUSKONTROLL                                           
573600     .                                                                    
573700     SKIP2                                                                
573800                                                                          
573900 IMS-GU-WDK601  SECTION.                                                  
574000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
574100          DELIMITED BY SIZE INTO SSA1                                     
574200     MOVE '  GE' TO GODK-STATUSKODER                                      
574300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
574400     MOVE WDK6-STATUS-CODE TO STATUS-WS-WDK6                              
574500     PERFORM IMS-STATUSKONTROLL                                           
574600     .                                                                    
574700     SKIP3                                                                
574800 IMS-GNP-WDK611  SECTION.                                                 
574900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
575000          DELIMITED BY SIZE INTO SSA2                                     
575100     MOVE '  GE' TO GODK-STATUSKODER                                      
575200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
575300     MOVE WDK6-STATUS-CODE TO STATUS-WS-WDK6                              
575400     PERFORM IMS-STATUSKONTROLL                                           
575500     .                                                                    
575600     SKIP3                                                                
575700 IMS-GU-WDK611  SECTION.                                                  
575800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
575900          DELIMITED BY SIZE INTO SSA1                                     
576000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
576100          DELIMITED BY SIZE INTO SSA2                                     
576200     MOVE '  GE' TO GODK-STATUSKODER                                      
576300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
576400     MOVE WDK6-STATUS-CODE TO STATUS-WS-WDK6                              
576500     PERFORM IMS-STATUSKONTROLL                                           
576600     .                                                                    
576700     SKIP3                                                                
576800 IMS-GU-WDK711 SECTION.                                                   
576900                                                                          
577000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
577100          DELIMITED BY SIZE INTO SSA1                                     
577200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
577300          DELIMITED BY SIZE INTO SSA2                                     
577400     MOVE '  GE' TO GODK-STATUSKODER                                      
577500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
577600     MOVE WDK7-STATUS-CODE TO STATUS-WS-WDK7                              
577700     PERFORM IMS-STATUSKONTROLL                                           
577800     .                                                                    
577900     SKIP2                                                                
578000 IMS-ROLLBACK    SECTION.                                                 
578100     SKIP2                                                                
578200     MOVE '  ' TO GODK-STATUSKODER                                        
578300     CALL CBLTDLI USING ROLB    MSG-PCB                                   
578400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
578500     PERFORM IMS-STATUSKONTROLL                                           
578600     SKIP2                                                                
578700     .                                                                    
578800 IMS-STATUSKONTROLL SECTION.                                              
578900     SET STATUS-IX TO 1                                                   
579000     SEARCH GODK-STATUS AT END CALL FELLOG                                
579100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
579200     END-SEARCH                                                           
579300     CONTINUE                                                             
579400     .                                                                    
