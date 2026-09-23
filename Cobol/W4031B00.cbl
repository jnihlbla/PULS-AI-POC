000100*COMPOPT DB2BIND=YES                                                      
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4031B00.                                                
000400 AUTHOR.         MOGREN / SVENSSON                                        
000500 DATE-WRITTEN.   FEB   09.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000800*    NAME:       CARPARTS.3IV2.REQUCARRIERFINALIZATION                    
000900*                CARPARTS.3IV2.RESPCARRIERFINALIZATION                    
001000*    FUNKTION.                                                            
001100*        3IV TO PULS                                                      
001200*        KOLLIVIS PACKNING - RAPPORTERING AV VAD SOM LIGGER               
001300*        I VISST KOLLI.                                                   
001400*        DESSUTOM RAPPORTERAS UPPGIFTER OM KOLLIT.                        
001500*                                                                         
001600*    SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800 DATA DIVISION.                                                           
001900 WORKING-STORAGE SECTION.                                                 
002000*    -- CHECKED BY WY2000                                                 
002100     SKIP3                                                                
002200 77   PROGRAM-NAMN              PIC X(8)    VALUE 'W4031B00'.             
002300 77   FILLER                    PIC X(8)    VALUE 'ERR-TEXT'.             
002400 77   ERRORTEXT                 PIC X(80)   VALUE SPACE.                  
002500 01   WPOS                      PIC X(4)    VALUE SPACE.                  
002600 77   JA                        PIC X       VALUE 'J'.                    
002700 77   YES                       PIC X       VALUE 'Y'.                    
002800 77   NEJ                       PIC X       VALUE 'N'.                    
002900 77   RAETT                     PIC X       VALUE 'R'.                    
003000 77   FEL                       PIC X       VALUE 'F'.                    
003100 77   SAKNAS                    PIC X       VALUE 'S'.                    
003200 77   FILLER                    PIC X(8)    VALUE 'AAAAAAAA'.             
003300 77   IND1                      PIC S9(9)   VALUE +0   COMP SYNC.         
003400 77   IND2                      PIC S9(9)   VALUE +0   COMP SYNC.         
003500 77   INDX                      PIC S9(9)   VALUE +0   COMP SYNC.         
003600 77   INX                       PIC  9(9)   VALUE  0.                     
003700 77   MIX                       PIC S9(5)   VALUE +0   COMP SYNC.         
003800 77   ACK-KOLLI                 PIC S9(9)   VALUE +0.                     
003900 77   RAD-INX                   PIC S9(9)   VALUE +0   COMP SYNC.         
004000 77   REST-INX                  PIC S9(9)   VALUE +0   COMP SYNC.         
004100 77   BILD-RAD                  PIC S9(9)   VALUE +0   COMP SYNC.         
004200 77   FG-INDX                   PIC S9(9)   VALUE +0   COMP SYNC.         
004300 77   FG-MAX-INDX               PIC S9(9)   VALUE +10  COMP SYNC.         
004400 77   FILLER                    PIC X(8)    VALUE 'BBBBBBBB'.             
004500 77   MAX-MOD-LAENGD            PIC S9(4)   VALUE +400 COMP SYNC.         
004600 77   MIN-MOD-LAENGD            PIC S9(4)   VALUE +82  COMP SYNC.         
004700 77   FILLER                    PIC X(8)    VALUE 'BBBBCCBB'.             
004800 77   KDRC-DISP                 PIC 9(4)    VALUE ZERO.                   
004900 77   KDRC-DISPLAY              PIC X(60)   VALUE SPACE.                  
005000 77   FILLER                    PIC X(8)    VALUE 'CCCCCCCC'.             
005100 77   WS-TOT-ANT-RADER          PIC S9(3)   VALUE +0    COMP-3.           
005200 77   WS-MAX-ANT-RAD            PIC S9(3)   VALUE +200  COMP-3.           
005300 77   MAX-RAD-ANTAL-PLUS-1      PIC S9(3)   VALUE +1   COMP-3.            
005400 77   WS-KVLOCK                 PIC S9(3)   VALUE +0    COMP-3.           
005500 77   WS-KVRAM                  PIC S9(3)   VALUE +0    COMP-3.           
005600 77   WS-KVPALL                 PIC S9(3)   VALUE +0    COMP-3.           
005700 77   WS-KDORDKL                PIC S9(1)   VALUE +0    COMP-3.           
005800 77   WS-KORD-IDORDER           PIC S9(7)   VALUE +0    COMP-3.           
005900 77   FILLER                    PIC X(8)    VALUE 'DDDDDDDD'.             
006000 77   WS-KDFRAKT                PIC 9(2)   VALUE ZERO.                    
006100 77   WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                    
006200 77   WS-EMBPROF                PIC X(1)   VALUE SPACE.                   
006300 77   WS-FLAUTFAK               PIC X(1)   VALUE SPACE.                   
006400 77   WS-KDFAKTYP               PIC X(1)   VALUE SPACE.                   
006500 77   WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                    
006600 77   WS-TIDPUNKT               PIC 9(8)   VALUE ZERO.                    
006700 77   WS-TISKPTID               PIC 9(6)   VALUE ZERO.                    
006800 01   WS-TILOKDAT               PIC 9(6)   VALUE ZERO.                    
006900 01   WS-TILOKTID               PIC 9(4)   VALUE ZERO.                    
007000 01   WS-IDTRPTNR               PIC S9(3)   VALUE ZERO.                   
007100 01   WS-ADFLGEO                PIC X(3)    VALUE SPACE.                  
007200 01   WS-ADFLOMR                PIC S9(3)   VALUE ZERO.                   
007300 01   WS-ADRUTNIV               PIC S9(3)   VALUE ZERO.                   
007400 01   WS-DIHMODUL               PIC S9(3)   VALUE ZERO.                   
007500 01   WS-DIDMODUL               PIC S9(3)   VALUE ZERO.                   
007600 01   WS-ADVMODUL               PIC S9(3)   VALUE ZERO.                   
007700 01   WS-ADHMODUL               PIC S9(3)   VALUE ZERO.                   
007800 01   WS-FLUTLAST               PIC X       VALUE SPACE.                  
007810 01   WS-IDDC-CROSS             PIC X(2)    VALUE SPACE.                  
007900                                                                          
008000 77   WS-IDANSTNR               PIC X(5)   VALUE SPACE.                   
008100 77   WS-IDDISTR                PIC X(4)   VALUE SPACE.                   
008200 77   WS-IDDISTR-NUM            PIC 9(4)   VALUE ZERO.                    
008300 77   WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                    
008400 77   WS-IDPLKLST               PIC S9(3)  VALUE ZERO COMP-3.             
008500 77   WS-IDLOPNR-ORD            PIC S9(3)  VALUE ZERO COMP-3.             
008600 77   WS-IDKOLLI                PIC X(5)   VALUE SPACE.                   
008700 77   WS-IDPRC                  PIC X(4)   VALUE SPACE.                   
008800 77   FILLER                    PIC X(8)    VALUE 'EEEEEEEE'.             
008900 77   WS-IDKOLLI-LR             PIC 9(5)   VALUE ZERO.                    
009000 77   WS-IDKOLLI-NUM            PIC 9(5)   VALUE ZERO.                    
009100 77   WS-IDKOLLI-FOM            PIC 9(5)   VALUE ZERO.                    
009200 77   WS-IDKOLLI-TOM            PIC 9(5)   VALUE ZERO.                    
009300 77   WS-IDPRODNR               PIC X(7)   VALUE SPACE.                   
009400 77   WS-JFR-IDPRODNR           PIC X(7)   VALUE SPACE.                   
009500 77   WS-IDPURAD                PIC 9(4)   VALUE ZERO.                    
009600 77   WS-START-RAD              PIC 9(4)   VALUE ZERO.                    
009700 77   WS-SISTA-RAD              PIC 9(4)   VALUE ZERO.                    
009800 77   WS-AKTUELL-RAD            PIC 9(4)   VALUE ZERO.                    
009900 77   WS-KVLEVART               PIC 9(6)   VALUE ZERO.                    
010000 77   WS-IDKOLLI-SAMP           PIC 9(5)   VALUE ZERO.                    
010100 77   WS-KDKOLLI                PIC X(8)   VALUE SPACE.                   
010200 77   WS-FLSISTAK               PIC X(1)   VALUE SPACE.                   
010300 77   WS-FLPAFEL                PIC X(1)   VALUE SPACE.                   
010400 77   WS-FLNOLLJ                PIC X(1)   VALUE SPACE.                   
010500 77   WS-KVORAPP                PIC 9(6)   VALUE ZERO.                    
010600 77   FILLER                    PIC X(8)    VALUE 'FFFFFFFF'.             
010700 77   WS-EMB-VKTARA-ONE-CASE    PIC S9(6)V9 VALUE ZERO  COMP-3.           
010800 77   WS-EMB-VKTARA-TOT-ORDER   PIC S9(6)V9 VALUE ZERO  COMP-3.           
010900 77   WS-EMB-VKTARA-NUM         PIC  9(6)   VALUE ZERO.                   
011000 77   WS-KOLLI-VKORDNTO         PIC S9(6)V9 VALUE ZERO  COMP-3.           
011100 77   KOLLI-VKORDBTO-KOLLI-NUM  PIC  9(6)   VALUE ZERO.                   
011200 77   WS-MOD-VKORDBTO           PIC 9(6)V9 VALUE ZERO.                    
011300 77   WS-VLORDBTO               PIC 9(4)V9(3)  VALUE ZERO.                
011400 77   WS-KDEMBTYP               PIC 9(2)   VALUE ZERO.                    
011500 77   WS-DIKOLLIL               PIC 9(4)   VALUE ZERO.                    
011600 77   WS-MOD-DIKOLLIL           PIC 9(4)   VALUE ZERO.                    
011700 77   WS-DIKOLLIB               PIC 9(3)   VALUE ZERO.                    
011800 77   WS-MOD-DIKOLLIB           PIC 9(3)   VALUE ZERO.                    
011900 77   WS-DIKOLLIH               PIC 9(3)   VALUE ZERO.                    
012000 77   WS-MOD-DIKOLLIH           PIC 9(3)   VALUE ZERO.                    
012100 77   WS-KDKOLLID               PIC X(1)   VALUE 'L'.                     
012200 77   WS-REST                   PIC 9(4)   VALUE ZERO.                    
012300 77   FILLER                    PIC X(8)    VALUE 'GGGGGGGG'.             
012400 77   WS-ODEL-IDTRP             PIC X(5)   VALUE SPACE.                   
012500 77   WS-TRAEFF-PACKARE         PIC X(01).                                
012600 77   WS-TRAEFF-RAD             PIC X(01).                                
012700 77   WS-KVPTID-MIN             PIC S9(7)  VALUE ZERO COMP-3.             
012800 77   WS-KVPTID-TIM             PIC S9(3)  VALUE ZERO COMP-3.             
012900 77   WS-PRT-KDSVAR-ADRESSFL    PIC X(1)   VALUE SPACE.                   
013000 77   WS-PRT-KDSVAR-FOLJEFL     PIC X(1)   VALUE SPACE.                   
013100 77   WS-PACKARES-ODEL-REDAN-KLARA PIC X.                                 
013200 77   WS-DARFS                  PIC 9(12) VALUE ZERO.                     
013300*                                       ANTAL FÄRDIGPACKADE RADER         
013400*                                       I ETT RAD-INTERVALL.              
013500 77   WS-RINT-ANT-FPACK-ORAD    PIC S9(5)  VALUE ZERO COMP-3.             
013600 77   FILLER                    PIC  X(08) VALUE 'HHHHHHHH'.              
013700 77   WS-SPAR-IDORDER           PIC S9(07) VALUE ZERO COMP-3.             
013800 77   WS-SPAR-IDARTNR           PIC S9(09) VALUE ZERO COMP-3.             
013900 77   WS-SPAR-IDDC              PIC  X(02) VALUE ZERO.                    
014000 77   WS-SPAR-BEART             PIC  X(25) VALUE SPACE.                   
014100 77   WS-SPAR-KVBEART           PIC S9(07) VALUE ZERO COMP-3.             
014200 77   WS-SPAR-FLTILLK           PIC  X(01) VALUE SPACE.                   
014300 77   WS-SPAR-IDKUNDRF-RO       PIC  X(10) VALUE SPACE.                   
014400 77   WS-SPAR-IDPURAD           PIC S9(05) VALUE ZERO COMP-3.             
014500*                                        ANTAL FÄRDIGPACKADE RADER        
014600*                                        I ETT RAD-INTERVALL.             
014700 77   FILLER                    PIC X(8)    VALUE 'IIIIIIII'.             
014800                                                                          
014900 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
015000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  VALUE +1000 COMP SYNC.        
015100 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
015200 77  IX-MAX                      PIC S9(9)  VALUE +7    COMP SYNC.        
015300*                                                                         
015400*      WS-PACKN-OMRADE    FRÅN ORAD-ADLAGOMR                              
015500 77    WS-PACKN-OMRADE           PIC 9(02).                               
015600   88   WS-NOLLNING-PACK-OMR                 VALUE  14 40 41              
015700                                              43 44 45 48 49              
015800                                              57 59 70 71 72              
015900                                              73 75 76 85 90.             
016000 77  VBAR                        PIC X       VALUE X'BB'.                 
016100 01  FILLER                      PIC X(8)    VALUE 'FIELDS  '.            
016200 01  FIELD-LENGTHS.                                                       
016300*    --- W403REQU FIELDS                                                  
016400     03 LIDMSG3IV                PIC S9(4)   BINARY.                      
016500     03 LIDMVER3IV               PIC S9(4)   BINARY.                      
016600     03 LIDMTYP3IV               PIC S9(4)   BINARY.                      
016700     03 LTISTAMP3IV              PIC S9(4)   BINARY.                      
016800     03 LIDDC                    PIC S9(4)   BINARY.                      
016900     03 LIDANSTNR                PIC S9(4)   BINARY.                      
017000     03 LIDSNO3IV                PIC S9(4)   BINARY.                      
017100*    --- W403CARR FIELDS                                                  
017200     03 FILLER                   PIC XX      VALUE 'CA'.                  
017300     03 LIDRTYP3IV               PIC S9(4)   BINARY.                      
017400     03 LIDPRODNR                PIC S9(4)   BINARY.                      
017500     03 LIDPLKLST                PIC S9(4)   BINARY.                      
017600     03 LIDPRC                   PIC S9(4)   BINARY.                      
017700     03 LIDLOPNR-ORD             PIC S9(4)   BINARY.                      
017800     03 LFLSISTAK                PIC S9(4)   BINARY.                      
017900     03 LKDEMBTYP                PIC S9(4)   BINARY.                      
018000     03 LKDKOLLI                 PIC S9(4)   BINARY.                      
018100     03 LDIKOLLIH                PIC S9(4)   BINARY.                      
018200     03 LDIKOLLIL                PIC S9(4)   BINARY.                      
018300     03 LDIKOLLIB                PIC S9(4)   BINARY.                      
018400*    --- W403PICR FIELDS                                                  
018500     03 FILLER                   PIC XX      VALUE 'PI'.                  
018600     03 LIDPURAD                 PIC S9(4)   BINARY.                      
018700     03 LKVAVBART                PIC S9(4)   BINARY.                      
018800     03 LKVLEVART                PIC S9(4)   BINARY.                      
018900     03 LKDARTURS-NUM            PIC S9(4)   BINARY.                      
019000*    --- W403RESP FIELDS                                                  
019100     03 LKDRESP3IV               PIC S9(4)   BINARY.                      
019200     03 LBERESP3IV               PIC S9(4)   BINARY.                      
019300     03 LIDKOLLI                 PIC S9(4)   BINARY.                      
019400     03 LADFLLOC                 PIC S9(4)   BINARY.                      
019500*                                                                         
019600 01  WS-ADDISPXTRA               PIC X(20)  VALUE SPACE.                  
019700*                                                                         
019800*01  TRANSFER-KUND               PIC 9(7).                                
019900*    88 TRANSFER-KUNDNR          VALUE 0000511                            
020000*                                      0000512                            
020100*                                      0000513.                           
020200*    88  RETUR-KUNDNR            VALUE 0000051.                           
020300*                                                                         
020400 01  WS-KDMATT                   PIC X.                                   
020500     88 US-MEASUREMENT           VALUE 'U'.                               
020600     88 SIS-MEASUREMENT          VALUE 'S'.                               
020700*                                                                         
020800 01    WS-VKORDBTO-RED         PIC 9(6).9  VALUE ZERO.                    
020900 01    FILLER           REDEFINES WS-VKORDBTO-RED.                        
021000       05  WS-VKORDBTO-KG      PIC 9(6).                                  
021100       05  WS-VKORDBTO-PUNKT   PIC X.                                     
021200       05  WS-VKORDBTO-DEC     PIC 9.                                     
021300*      --- VALID IDDD CODES                                               
021400*                                                                         
021500*01    -COPY WWDC99                                                       
021600       EJECT                                                              
021700                                                                          
021800 77    WS-IDTRANS                PIC X(04).                               
021900   88  WS-GODKAND-BILD                      VALUE '4311' '4312'           
022000                                                  '4314' '431B'           
022100                                                  '4315' '4316'           
022200                                                  '4317' '4318'.          
022300     SKIP2                                                                
022400 01  FILLER                      PIC X(8)   VALUE 'INDATA--'.             
022500 77    WS-INDATA-TEST            PIC X(01).                               
022600   88  WS-INDATA-FEL                        VALUE 'F'.                    
022700   88  WS-INDATA-RATT                       VALUE 'R'.                    
022800     SKIP2                                                                
022900 77    WS-BEHANDLING-TEST        PIC X(01).                               
023000   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
023100   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
023200     SKIP2                                                                
023300 77    FL-AVRAPP-MED-KOLLI      PIC X(01).                                
023400   88  AVRAPP-MED-KOLLI                     VALUE 'J'.                    
023500   88  AVRAPP-UTAN-KOLLI                    VALUE 'N'.                    
023600     SKIP2                                                                
023700 77    FL-RAD-INTERVALL          PIC X(01).                               
023800   88  INTERVALL-RAD                        VALUE 'J'.                    
023900   88  AVVIKELSE-RAD                        VALUE 'N'.                    
024000     SKIP2                                                                
024100 77    FL-KOLLI-INTERVALL        PIC X(01).                               
024200   88  KOLLI-INTERVALL                      VALUE 'J'.                    
024300   88  EJ-KOLLI-INTERVALL                   VALUE 'N'.                    
024400     SKIP2                                                                
024500 77    FL-RAD-INOM-INTERVALL     PIC X(01).                               
024600   88  RAD-FINNS-I-INTERVALL                VALUE 'J'.                    
024700     SKIP2                                                                
024800 77    SW-TIKLAR-UPPDATERAD      PIC X(01).                               
024900                                                                          
025000*77    SW-KOLLI-HITTAT           PIC X(01).                               
025100*  88  KOLLI-HITTAT                         VALUE 'J'.                    
025200                                                                          
025300 77    DIRLEV-KOLLI-SW           PIC X(01).                               
025400   88  DIRLEV-KOLLI                         VALUE 'J'.                    
025500*                                                                         
025600 77    NYA-NYCKLAR-SW            PIC X(01).                               
025700   88  NYA-NYCKLAR                          VALUE 'J'.                    
025800*                                                                         
025900 77    FILLER                    PIC X(8)    VALUE 'JJJJJJJJ'.            
026000     SKIP2                                                                
026100 01     WS-IDKUNDRF.                                                      
026200   03   WS-IDORDNR              PIC X(5).                                 
026300   03   FILLER                  PIC X(5)    VALUE SPACE.                  
026400 01     WS-JFR-IDANSTNR.                                                  
026500   03   FILLER                    PIC X(3).                               
026600   03   WS-JFR-IDANSTNR-5         PIC X(5).                               
026700     SKIP3                                                                
026800 01     WS-SUPTID-PRAPP         PIC 9(3)V99.                              
026900 01     FILLER REDEFINES WS-SUPTID-PRAPP.                                 
027000   03   WS-SUPTID-TIM           PIC 9(3).                                 
027100   03   WS-SUPTID-MIN           PIC 9(2).                                 
027200     SKIP3                                                                
027300                                                                          
027400 01     DYNAMISKA-SUBPROGRAM.                                             
027500   03   CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.             
027600   03   FELLOG                  PIC X(8)    VALUE 'FELLOG  '.             
027700   03   ABEND                   PIC X(8)    VALUE 'ABEND   '.             
027800   03   WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.             
027900   03   W005INIT                PIC X(8)    VALUE 'W005INIT'.             
028000   03   W400ARTU                PIC X(8)    VALUE 'W400ARTU'.             
028100   03   W403PLAT                PIC X(8)    VALUE 'W403PLAT'.             
028200   03   WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.             
028300   03   WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.             
028400   03   WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.             
028500   03   WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.             
028600     EJECT                                                                
028700*                                                                         
028800 01  GEMENSAMMA-SUBPROGRAM.                                               
028900     03  W411DNOT               PIC X(8)    VALUE 'W411DNOT'.             
029000*                                                                         
029100*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
029200*                                                                         
029300 01  FILLER                     PIC X(16)   VALUE 'LÄNKAREOR'.            
029400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
029500*01 -COPY WMSGINIT                                                        
029600 01  FILLER                     PIC X(10)   VALUE 'WDECAREA'.             
029700*01 -COPY WDECAREA                                                        
029800     EJECT                                                                
029900 01  FILLER                     PIC X(10)   VALUE 'W400ARTU'.             
030000*01 -COPY W400ARTU                                                        
030100     EJECT                                                                
030200 01  FILLER                     PIC X(16)   VALUE 'W403PLAT '.            
030300*   -COPY W403PLAT                                                        
030400     EJECT                                                                
030500 01  FILLER                     PIC X(16)   VALUE 'W411DNOT '.            
030600*   -COPY W411DNOT                                                        
030700     EJECT                                                                
030800 01  FILLER                     PIC X(16)   VALUE 'WWOMVAND '.            
030900*   -COPY WWOMVAND                                                        
031000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
031100*   -COPY WMEDAREA                                                        
031200     EJECT                                                                
031300*   ---AREOR FÖR ANROP TILL WZ01 ----                                     
031400 01  FILLER                     PIC X(16)   VALUE 'WZ01SEND '.            
031500*   -COPY WZ01SEND                                                        
031600 01  SEND-AREA                  PIC X(1000) VALUE SPACE.                  
031700*   ---AREOR FÖR ANROP FRÅN WZ01 ----                                     
031800 01  FILLER                     PIC X(16)   VALUE 'WZ01RECV '.            
031900*   -COPY WZ01RECV                                                        
032000     SKIP3                                                                
032100 01  RECV-AREA                  PIC X(1000)  VALUE SPACE.                 
032200*                                                                         
032300 01     FILLER                  PIC X(11)   VALUE 'HJALP-AREOR'.          
032400 01     HJALP-ODEL-DARFS        PIC 9(12).                                
032500 01     FILLER                  REDEFINES HJALP-ODEL-DARFS.               
032600   03   FILLER                  PIC  9(2).                                
032700   03   HJALP-ODEL-DARFS-6      PIC  9(6).                                
032800   03   FILLER                  PIC  9(4).                                
032900     SKIP2                                                                
033000 01     HJALP-4472-TIRFS        PIC 9(11).                                
033100 01     FILLER                  REDEFINES HJALP-4472-TIRFS.               
033200   03   FILLER                  PIC  9(1).                                
033300   03   HJALP-4472-TIRFS-6      PIC  9(6).                                
033400   03   FILLER                  PIC  9(4).                                
033500     EJECT                                                                
033600 01     FILLER                  PIC X(10)   VALUE 'SPAR-AREOR'.           
033700 01     SPAR-AREOR.                                                       
033800   03   SPAR-PRAD-UPPG-AREA.                                              
033900     05 SPAR-PRAD-VKARTNTO      PIC  9(6)V9(3)    VALUE ZERO.             
034000     05 SPAR-PRAD-KVFLAMP       PIC  S9(2)V9(1)   VALUE ZERO.             
034100     05 SPAR-PRAD-KDFARLIG      PIC  S9           VALUE ZERO.             
034200     05 SPAR-PRAD-KVLEVART      PIC  S9(7)        VALUE ZERO.             
034300     05 SPAR-PRAD-PRARTNTO      PIC  S9(9)V9(2)   VALUE ZERO.             
034400     05 SPAR-PRAD-PRAVCOST      PIC  S9(9)V9(2)   VALUE ZERO.             
034500     05 SPAR-PRAD-PRARTNTO-LOC  PIC  S9(9)V9(2)   VALUE ZERO.             
034600     05 SPAR-PRAD-PRARTNTO-LOCPREL  PIC  S9(9)V9(2)   VALUE ZERO.         
034700     05 SPAR-PRAD-KDVALISO      PIC X(3)          VALUE SPACE.            
034800     05 SPAR-PRAD-KDVALISO-EXP  PIC X(3)          VALUE SPACE.            
034900*                                                                         
035000   03   SPAR-FARLIGT-GODS-DATA.                                           
035100     05 SPAR-IDPSN              PIC  9(3)                VALUE 0.         
035200     05 SPAR-VKART-FG           PIC  S9(7)        COMP-3 VALUE 0.         
035300     05 SPAR-VLFG               PIC  S9(4)V9(3)   COMP-3 VALUE 0.         
035400     05 SPAR-SUEQFG             PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
035500*                                                                         
035600     05 TOTAL-SUEQFG            PIC  S9(3)V9(4)   COMP-3 VALUE 0.         
035700     EJECT                                                                
035800 01     FILLER                  PIC X(11)   VALUE 'ARBETSAREOR'.          
035900 01     ARBETSAREOR.                                                      
036000   03   ARB-AREA-RAD.                                                     
036100     05 ARB-RAD-FOM             PIC  9(4).                                
036200     05 ARB-RAD-AKTUELL         PIC  9(4).                                
036300     05 ARB-KVLEVART            PIC  9(6).                                
036400     SKIP2                                                                
036500   03   ARB-KOLLI-UPPG-AREA.                                              
036600     05 ARB-KOLLI-VKORDNTO       PIC  9(6)V9(1)   VALUE ZERO.             
036700     05 ARB-KOLLI-KVFLAMP        PIC  S9(2)V9(1)  VALUE ZERO.             
036800     05 ARB-KOLLI-KDFARLIG       PIC  S9          VALUE ZERO.             
036900     05 ARB-KOLLI-KVORDRAD       PIC  S9(5)       VALUE ZERO.             
037000     05 ARB-KOLLI-KVFALRAD       PIC  S9(5)       VALUE ZERO.             
037100     05 ARB-KOLLI-SUORDV         PIC  S9(9)V9(2)  VALUE ZERO.             
037200     05 ARB-KOLLI-SUORDV-EXP     PIC  S9(9)V9(2)  VALUE ZERO.             
037300     05 ARB-KOLLI-SUORDV-LOC     PIC  S9(9)V9(2)  VALUE ZERO.             
037400     05 ARB-KOLLI-SUORDV-LOCPREL PIC  S9(9)V9(2)  VALUE ZERO.             
037500*    05 SUM-ORAD-VKORDNTO        PIC  9(6)V9(1)   VALUE ZERO.             
037600     05 FILLER                   PIC X(8)    VALUE 'TTTTTTTT'.            
037700     05 ARB-KOLLI-KDVALISO       PIC X(3)    VALUE SPACE.                 
037800     05 ARB-KOLLI-KDVALISO-EXP   PIC X(3)    VALUE SPACE.                 
037900     SKIP2                                                                
038000   03   ARB-ADRESS.                                                       
038100     05 ARB-ADFLGEO             PIC  X(3)   VALUE SPACE.                  
038200     05 FILLER                  PIC  X(1)   VALUE SPACE.                  
038300     05 ARB-ADFLOMR             PIC  9(3)   VALUE ZERO.                   
038400     05 FILLER                  PIC  X(1)   VALUE SPACE.                  
038500     05 ARB-ADRUTNIV            PIC  9(3)   VALUE ZERO.                   
038600     SKIP2                                                                
038700   03   ARB-ANTAL-KOLLI-PLUS-1   PIC 9(5)    VALUE ZERO.                  
038800   03   ARB-ANTAL-KOLLI          PIC 9(5)    VALUE ZERO.                  
038900     SKIP2                                                                
039000 01     WS-TIDPUNKT-RED.                                                  
039100   03   WS-HHMMSS               PIC  9(6).                                
039200   03   WS-DD                   PIC  9(2).                                
039300     EJECT                                                                
039400 01     TEST-IDDISTR            PIC 9(5)              COMP-3.             
039500 01     FILLER REDEFINES TEST-IDDISTR.                                    
039600*  03   -COPY WWDIST03.                                                   
039700     SKIP2                                                                
039800 01     FILLER REDEFINES TEST-IDDISTR.                                    
039900*  03   -COPY WWDIST07.                                                   
040000     SKIP2                                                                
040100 01     FILLER REDEFINES TEST-IDDISTR.                                    
040200*  03   -COPY WWDIST18.                                                   
040300     SKIP2                                                                
040400 01     FILLER REDEFINES TEST-IDDISTR.                                    
040500*  03   -COPY WWDIST19.                                                   
040600     SKIP2                                                                
040700 01     FILLER REDEFINES TEST-IDDISTR.                                    
040800*  03   -COPY WWDIST21.                                                   
040900     SKIP2                                                                
041000 01     FILLER REDEFINES TEST-IDDISTR.                                    
041100*  03   -COPY WWDIST35.                                                   
041200     SKIP2                                                                
041300 01     FILLER REDEFINES TEST-IDDISTR.                                    
041400*  03   -COPY WWDIST79.                                                   
041500     SKIP2                                                                
041600 01     FILLER REDEFINES TEST-IDDISTR.                                    
041700*  03   -COPY WWDIST85.                                                   
041800*    ----DISTR-DEALER-PRICE----                                           
041900     SKIP2                                                                
042000*01    FILLER  -COPY WWDIS128      -RED TEST-IDDISTR.                     
042100     EJECT                                                                
042200 01  FILLER                    PIC X(08) VALUE 'FRAK-010'.                
042300*01    FILLER  -COPY WWFRAKT1                                             
042400     SKIP2                                                                
042500 01  FILLER                    PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.        
042600 01    NYCKLAR-TILL-DLI.                                                  
042700*                                                                         
042800   03    W-WDE4A1-KUNDORDER-X.                                            
042900     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
043000     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
043100     05    W-4A1-IDKUNDRF.                                                
043200       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
043300       07  FILLER                PIC X(05)   VALUE SPACE.                 
043400*                                                                         
043500   03    W-WDE401-KUNDORDER-X.                                            
043600     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
043700     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
043800     05    W-401-IDKUNDRF.                                                
043900       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
044000       07  FILLER                PIC X(05)   VALUE SPACE.                 
044100     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
044200     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
044300*                                                                         
044400   03    W-WDE4B-KEYSEQ-MIN-X.                                            
044500     05    W-420-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
044600     05    W-420-IDPURAD-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
044700*                                                                         
044800   03    W-WDE4B-KEYSEQ-MAX-X.                                            
044900     05    W-420-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
045000     05    W-420-IDPURAD-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
045100*                                                                         
045200   03    W-WDE4B-KEYSEQ-X.                                                
045300     05    W-420-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
045400     05    W-420-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
045500*                                                                         
045600   03    W-WDE4E-KEYSEQ-X.                                                
045700     05    W-IDPRODNR-E4E        PIC S9(7)   VALUE ZERO  COMP-3.          
045800*                                                                         
045900   03    W-WDE411-IDPURAD-X.                                              
046000     05    W-420-IDPURAD2        PIC S9(5)   VALUE ZERO  COMP-3.          
046100*                                                                         
046200   03    W-WDE421-IDKOLLI-X.                                              
046300     05    W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
046400     05    W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
046500*                                                                         
046600   03    W-WDE601-IDPRODNR-X.                                             
046700     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
046800*                                                                         
046900   03    W-WDE611-IDKOLLI-X.                                              
047000     05    W-611-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
047100*                                                                         
047200   03    W-IDKOLLIS-X.                                                    
047300     05    W-IDKOLLIS            PIC S9(5)   VALUE ZERO  COMP-3.          
047400*                                                                         
047500   03    W-4726-WDGXKEY-ROT-X.                                            
047600     05    W-4726-IDHTYP         PIC X(4)    VALUE '4726'.                
047700     05    W-4726-FLBATCH        PIC X(1)    VALUE SPACE.                 
047800     05    W-4726-LOWVALUE       PIC X(25)   VALUE LOW-VALUE.             
047900*                                                                         
048000   03    W-4726-WDGXKEY-UNDSEG-X.                                         
048100     05    W-4726-IDDISTR        PIC S9(5)   COMP-3.                      
048200     05    W-4726-IDKUNDNR       PIC S9(7)   COMP-3.                      
048300     05    W-4726-IDDC           PIC X(2).                                
048400     05    W-4726-KDFAKTYP       PIC X.                                   
048500*                                                                         
048600   03    W-KDKOLLI-WDK5          PIC X(8)    VALUE SPACE.                 
048700*                                                                         
048800   03    W-4301-WDGXKEY-X.                                                
048900     05    W-4301-IDHTYP         PIC X(4)    VALUE '4301'.                
049000     05    W-4301-IDPRODNR       PIC S9(7)   VALUE ZERO  COMP-3.          
049100     05    W-4301-NYCKEL-VALFRI  PIC X(22)   VALUE LOW-VALUE.             
049200*                                                                         
049300   03    W-4302-WDGXKEY-X.                                                
049400     05    W-4302-IDKOLLI        PIC S9(5)   VALUE ZERO  COMP-3.          
049500     05    W-4302-IDPLKLST       PIC S9(3)   VALUE ZERO  COMP-3.          
049600*                                                                         
049700   03    W-4321-IDHTYP-X.                                                 
049800         05  W-4321-IDHTYP         PIC X(4)  VALUE '4321'.                
049900         05  W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.             
050000*                                                                         
050100   03    W-IDORDER-X.                                                     
050200     05    W-201-IDORDER         PIC S9(7) COMP-3.                        
050300*                                                                         
050400   03    W-IDDC-X.                                                        
050500     05    W-IDDC                PIC X(2).                                
050600*                                                                         
050700   03    W-WDQ2CSEQ-X.                                                    
050800     05    W-WDQ2C-IDGMTREF-X.                                            
050900       07    W-WDQ2C-IDDISTR     PIC S9(5) COMP-3 VALUE +0.               
051000       07    W-WDQ2C-IDKUNDNR    PIC S9(7) COMP-3 VALUE +0.               
051100       07    W-WDQ2C-IDKUNDRF.                                            
051200         09    FILLER            PIC  9(2)        VALUE ZERO.             
051300         09    W-WDQ2C-IDORDNR5                                           
051400                                 PIC  9(5)        VALUE ZERO.             
051500         09    FILLER            PIC  X(3)        VALUE SPACE.            
051600*                                                                         
051700   03    W-WDQ301-ORDERDEL-X.                                             
051800     05    W-301-IDORDER         PIC S9(7) COMP-3.                        
051900     05    W-301-IDDC            PIC X(2).                                
052000     05    W-301-IDPRODNR        PIC S9(7) COMP-3.                        
052100     05    W-301-IDPLKLST        PIC S9(3) COMP-3.                        
052200*                                                                         
052300   03    W-4471-WDGXKEY-X.                                                
052400     05    W-4471-IDHTYP         PIC X(4)  VALUE '4471'.                  
052500     05    W-4471-IDDC           PIC X(2).                                
052600     05    W-4471-IDPRC.                                                  
052700       07    W-4471-IDPRCBAS     PIC X(3).                                
052800       07    W-4471-IDPRCVAR     PIC X(1).                                
052900     05    FILLER                PIC X(20) VALUE LOW-VALUE.               
053000*                                                                         
053100   03    W-4472-KDSEGKEY-X.                                               
053200     05    W-4472-KDSEGKEY       PIC X(1)  VALUE '1'.                     
053300*                                                                         
053400   03    W-4477-WDGXKEY-X.                                                
053500     05    W-4477-IDHTYP         PIC X(4)  VALUE '4477'.                  
053600     05    W-4477-IDDC           PIC X(2).                                
053700     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
053800*                                                                         
053900   03    W-4478-WDGXKEY-X.                                                
054000     05    W-4478-IDSHIFT        PIC X(1).                                
054100     05    W-4478-IDUSER         PIC X(8).                                
054200     05    FILLER                PIC X(1)  VALUE LOW-VALUE.               
054300*                                                                         
054400     03  W-IDDC-B6-X.                                                     
054500         05 W-IDDC-B6                  PIC X(2).                          
054600     03  W-PRC-B6-X.                                                      
054700         05 W-PRC-B6                   PIC X(4).                          
054800                                                                          
054900   03    W-WDA601KY-MIN-X.                                                
055000     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
055100     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
055200     05    W-A601KY-MIN-IDORDNR      PIC 9(07) VALUE ZERO.                
055300     05    FILLER                    PIC X(03) VALUE SPACE.               
055400     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
055500     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
055600     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
055700     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
055800     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
055900     SKIP2                                                                
056000   03    W-WDA601KY-MAX-X.                                                
056100     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
056200     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
056300     05    W-A601KY-MAX-IDORDNR      PIC 9(07) VALUE ZERO.                
056400     05    FILLER                    PIC X(03) VALUE SPACE.               
056500     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
056600     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
056700     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
056800     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
056900     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
057000                                                                          
057100   03    W-KDSEGKEY-X.                                                    
057200     05    W-KDSEGKEY                PIC X(1)  VALUE '1'.                 
057300     EJECT                                                                
057400*                                                                         
057500   01    MESSAGE-CODES.                                                   
057600     03  ERR-PRODNR-MISSING      PIC X(3)    VALUE '280'.                 
057700     03  ERR-INVALID-DC          PIC X(3)    VALUE '440'.                 
057800     03  ERR-CASE-WITHOUT-LINES  PIC X(3)    VALUE '490'.                 
057900     03  ERR-INVALID-VALUE       PIC X(3)    VALUE '492'.                 
058000     03  ERR-VALUE-TOO-LONG      PIC X(3)    VALUE '493'.                 
058100     03  ERR-WRONG-RCD-TYPE      PIC X(3)    VALUE '495'.                 
058200     03  ERR-WRONG-MSG-TYPE      PIC X(3)    VALUE '497'.                 
058300     03  ERR-INV-MSG-STRUCTURE   PIC X(3)    VALUE '499'.                 
058400     03  ERR-ORDER-MISSING       PIC X(3)    VALUE '701'.                 
058500     03  ERR-ORDER-SPLIT         PIC X(3)    VALUE '716'.                 
058600     03  ERR-ORDER-PART-READY    PIC X(3)    VALUE '720'.                 
058700     03  ERR-CASE-ALREADY-REPOR  PIC X(3)    VALUE '721'.                 
058800     03  ERR-INTERV-ALREADY-REP  PIC X(3)    VALUE '723'.                 
058900     03  ERR-ZERO-NOT-ALLOWED    PIC X(3)    VALUE '724'.                 
059000     03  ERR-TOO-LARGE-QUANT     PIC X(3)    VALUE '725'.                 
059100     03  ERR-CASE-CODE-MISSING   PIC X(3)    VALUE '726'.                 
059200     03  ERR-DIM-ZERO            PIC X(3)    VALUE '728'.                 
059300     03  ERR-MORE-CASE-INF-NEED  PIC X(3)    VALUE '728'.                 
059400     03  ERR-GENERAT-ADDRESS     PIC X(3)    VALUE '730'.                 
059500     03  ERR-WRONG-INTERV-INF    PIC X(3)    VALUE '738'.                 
059600     03  ERR-WRONG-COUNTRY-CODE  PIC X(3)    VALUE '753'.                 
059700     03  ERR-LINE-ZEROED-BY-HUNT PIC X(3)    VALUE '774'.                 
059800     03  ERR-EJ-AVSLUT           PIC X(3)    VALUE '778'.                 
059900     03  ERR-START-CASENUMBER    PIC X(3)    VALUE '801'.                 
060000     03  ERR-PROD-KANAL          PIC X(3)    VALUE '802'.                 
060100     03  ERR-DEVIATION-CONTROL   PIC X(3)    VALUE '804'.                 
060200     03  ERR-TOO-MANYLINES       PIC X(3)    VALUE '826'.                 
060300*                                                                         
060400*                                                                         
060500**********************************************                            
060600*                                                                         
060700     EJECT                                                                
060800 01    FILLER                 PIC X(16) VALUE 'EMB-TABELL'.               
060900     SKIP3                                                                
061000 01    EMB-TABELL.                                                        
061100   03    EMB-TAB-X.                                                       
061200     05  KLASS        OCCURS 3   INDEXED BY KL-INDX.                      
061300         07  TYP      OCCURS 5   INDEXED BY TYP-INDX                      
061400                                         PIC S9(5)  COMP-3.               
061500   03    EMB-TAB   REDEFINES  EMB-TAB-X.                                  
061600     05  FILLER.                                                          
061700         07  PALLAR   OCCURS 5           PIC S9(5)  COMP-3.               
061800     05  FILLER.                                                          
061900         07  KRAGAR   OCCURS 5           PIC S9(5)  COMP-3.               
062000     05  FILLER.                                                          
062100         07  EMB-LOCK OCCURS 5           PIC S9(5)  COMP-3.               
062200     EJECT                                                                
062300 01    FILLER                 PIC X(16) VALUE 'FG-TABELL'.                
062400                                                                          
062500 01    FG-TABELL.                                                         
062600   03    TAB-POST OCCURS 10.                                              
062700                                                                          
062800     05  TAB-IDPSN            PIC 9(3)              VALUE ZERO.           
062900                                                                          
063000     05  TAB-VKART-FG         PIC S9(7)      COMP-3 VALUE ZERO.           
063100                                                                          
063200     05  TAB-VLFG             PIC S9(4)V9(3) COMP-3 VALUE ZERO.           
063300     EJECT                                                                
063400 01    FILLER                    PIC X(16)   VALUE '4397-AREA '.          
063500 01    4397-TRANSAREA.                                                    
063600   03    4397-IDPRODNR           PIC 9(7)    VALUE ZERO.                  
063700   03    4397-IDANSTNR           PIC 9(5)    VALUE ZERO.                  
063800   03    4397-IDPLKLST           PIC 9(3)    VALUE ZERO.                  
063900   03    4397-IDPURAD            PIC 9(5)    VALUE ZERO.                  
064000   03    FILLER                  PIC X(80)   VALUE SPACE.                 
064100     EJECT                                                                
064200 01    FILLER                    PIC X(16)   VALUE 'ALT-IO-AREA'.         
064300 01    ALT-IO-AREA.                                                       
064400   03    ALT-LL                  PIC S9(4)   COMP  SYNC.                  
064500   03    ALT-Z1                  PIC X(1).                                
064600   03    ALT-Z2                  PIC X(1).                                
064700   03    ALT-TRANSKOD            PIC X(8)    VALUE SPACE.                 
064800   03    ALT-IDTRANS             PIC X(4)    VALUE SPACE.                 
064900   03    ALT-KDMFSFOR            PIC X(1)    VALUE SPACE.                 
065000   03    ALT-AREA                PIC X(105)  VALUE SPACE.                 
065100     EJECT                                                                
065200******************************************************************        
065300*                                                                *        
065400*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
065500*                                                                *        
065600******************************************************************        
065700 01    FILLER                 PIC X(16) VALUE 'MID W403REQU'.             
065800     SKIP3                                                                
065900*01    -COPY W403REQU.                                                    
066000 01    FILLER                 PIC X(16) VALUE 'MID W403CARR'.             
066100     SKIP3                                                                
066200*01    -COPY W403CARR.                                                    
066300 01    FILLER                 PIC X(16) VALUE 'MID W403PICR'.             
066400     SKIP3                                                                
066500*01    -COPY W403PICR.                                                    
066600 01    FILLER                 PIC X(16) VALUE 'MOD W403RESP'.             
066700     SKIP3                                                                
066800*01    -COPY W403RESP.                                                    
066900 01    FILLER                 PIC X(16) VALUE 'MOD W403CRES'.             
067000     SKIP3                                                                
067100*01    -COPY W403CRES.                                                    
067200     EJECT                                                                
067300 01    FILLER                 PIC X(16) VALUE 'MOD W4I31501 MOD'.         
067400*01    -COPY WMSGAREA                                                     
067500     EJECT                                                                
067600 01    FILLER                 PIC X(16) VALUE 'MID TABELL      '.         
067700 01    MID-TABELL.                                                        
067800   03  MID-RAD   OCCURS 200.                                              
067900     05  MID-IDPURAD          PIC 9(4).                                   
068000     05  MID-KVAVBART         PIC 9(6).                                   
068100     05  MID-KVLEVART         PIC 9(6).                                   
068200     05  MID-KDARTURS-NUM     PIC 9(2).                                   
068300     05  MID-KDARTURS         PIC X(2).                                   
068400     SKIP2                                                                
068500 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
068600*01    -COPY WMFSAREA                                                     
068700     EJECT                                                                
068800*01  XXJK  -COPY WDGX4322    -PRE 4322-                                   
068900     EJECT                                                                
069000******************************************************************        
069100*                                                                         
069200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
069300*                                                                         
069400 01    IMS-WS.                                                            
069500   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
069600     SKIP3                                                                
069700*                        **** STATUS-KOD FRÅN IMS                         
069800   03    STATUS-KUNDORDER-SEK-WS PIC X(02).                               
069900     88    KUNDORDER-SEK-FINNS               VALUE '  '.                  
070000     88    KUNDORDER-SEK-SAKNAS              VALUE 'GE' 'GB'.             
070100   03    STATUS-WS               PIC XX.                                  
070200     88    SEGMENT-FINNS                     VALUE '  '.                  
070300     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
070400     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
070500     88    END-OF-DATABASE                   VALUE 'GB'.                  
070600     SKIP3                                                                
070700   03    GODK-STATUSKODER.                                                
070800     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
070900     SKIP3                                                                
071000 01    SSA1                      PIC X(120).                              
071100 01    SSA2                      PIC X(96).                               
071200 01    SSA3                      PIC X(96).                               
071300 01    SSA4                      PIC X(96).                               
071400     EJECT                                                                
071500*                            IMS FUNKTIONSKODER                           
071600*01    -COPY W0003                                                        
071700     EJECT                                                                
071800 01    FILLER    PIC X(16)  VALUE 'DLI-IO-K501 '.                         
071900 01    DLI-IO-K501.                                                       
072000*  03    WDK501   -COPY WDK501                                            
072100     EJECT                                                                
072200 01    FILLER    PIC X(16)  VALUE 'DLI-IO-E401 '.                         
072300 01    DLI-IO-E401.                                                       
072400*  03    WDE401 -COPY WDE401                                              
072500     EJECT                                                                
072600 01    FILLER    PIC X(16)  VALUE 'DLI-IO-E411 '.                         
072700 01    DLI-IO-E411.                                                       
072800*  03    WDE411 -COPY WDE411                                              
072900     EJECT                                                                
073000 01    FILLER    PIC X(16)  VALUE 'DLI-IO-E421 '.                         
073100 01    DLI-IO-E421.                                                       
073200*  03    WDE421 -COPY WDE421                                              
073300 01    FILLER    PIC X(16)  VALUE 'DLI-IO-E4E1'.                          
073400 01    DLI-IO-E4E1.                                                       
073500*  03    WDE4E1 -COPY WDE4E1   -PRE E4-                                   
073600     EJECT                                                                
073700 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA2'.               
073800 01    DLI-IO-AREA2.                                                      
073900   03    IO-AREA2                PIC X(400)  VALUE SPACE.                 
074000     SKIP3                                                                
074100*  03    WDE601   -COPY WDE601                                            
074200     EJECT                                                                
074300*  03    WDE611   -COPY WDE611                                            
074400     EJECT                                                                
074500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE621'.                      
074600 01  DLI-IO-WDE621.                                                       
074700*    03  -COPY WDE621                                                     
074800     EJECT                                                                
074900 01    FILLER    PIC X(25)  VALUE 'DLI INPUT-OUTPUT AREA3'.               
075000 01    DLI-IO-AREA3.                                                      
075100   03    IO-AREA3                PIC X(25)   VALUE SPACE.                 
075200     SKIP3                                                                
075300*  03    WLXXDV11 -COPY WDGX4726           -RED IO-AREA3.                 
075400     EJECT                                                                
075500*  03    WLXXDV21 -COPY WDGX4727           -RED IO-AREA3.                 
075600     EJECT                                                                
075700 01  DLI-IO-AREA4.                                                        
075800     03  IO-AREA4                PIC X(100)  VALUE SPACE.                 
075900*                                                                         
076000*    03  WLXXJK01  -COPY WDGX01      -PRE 4321-  -RED IO-AREA4            
076100*    03  WLXXJK11  -COPY WDGX4322    -RED IO-AREA4                        
076200     EJECT                                                                
076300 01  DLI-IO-AREA5.                                                        
076400     03  IO-AREA5                PIC X(192)  VALUE SPACE.                 
076500*                                                                         
076600*    03  WLORQA01  -COPY WDQ301      -RED IO-AREA5                        
076700     EJECT                                                                
076800 01  DLI-IO-AREA6.                                                        
076900     03  IO-AREA6                PIC X(1000) VALUE SPACE.                 
077000*                                                                         
077100*    03  WLXXKW11  -COPY WDGX4472    -RED IO-AREA6                        
077200     EJECT                                                                
077300*    03  WLXXLB11  -COPY WDGX4478    -RED IO-AREA6                        
077400     EJECT                                                                
077500 01    DLI-IO-AREA7.                                                      
077600   03    IO-AREA7                PIC X(150)  VALUE SPACE.                 
077700     SKIP3                                                                
077800*  03  WDGZ01     -COPY WDGZ01  -PRE LOGG-   -RED IO-AREA7.               
077900     EJECT                                                                
078000 01    DLI-IO-WDQ201.                                                     
078100*  03    WDQ201   -COPY WDQ201                                            
078200 01    DLI-IO-WDQ212.                                                     
078300*  03    WDQ212   -COPY WDQ212  -PRE WDQ2-                                
078400     EJECT                                                                
078500*01      WDGZRYK  -COPY WDGZRYK.                                          
078600     SKIP2                                                                
078700 01    FILLER                    PIC X(16) VALUE 'WDE4A-AREA'.            
078800 01    WDE4A-IO-AREA.                                                     
078900   03    WDE4A-AREA              PIC X(100)  VALUE SPACE.                 
079000*  03    WDE4A1    -COPY WDE4A1                                           
079100                                                                          
079200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
079300 01   DLI-IO-WDB601.                                                      
079400*     03  -COPY WDB601                                                    
079500     EJECT                                                                
079600 01  FILLER               PIC X(16)   VALUE 'WDB612 AREA'.                
079700 01   DLI-IO-WDB612.                                                      
079800*     03  -COPY WDB612                                                    
079900                                                                          
080000 01  FILLER               PIC X(16)   VALUE 'WDA601 AREA'.                
080100 01   DLI-IO-WDA601.                                                      
080200*     03  -COPY WDA601                                                    
080300     EJECT                                                                
080400 LINKAGE SECTION.                                                         
080500*01    -COPY W0009     -PRE MSG-                                          
080600     EJECT                                                                
080700*01    -COPY W0009     -PRE ALT4397-                                      
080800     EJECT                                                                
080900*01    -COPY W0009     -PRE ALT-                                          
081000     EJECT                                                                
081100*01    -COPY W0008     -PRE WDP7-                                         
081200     05  FILLER                  PIC X.                                   
081300     EJECT                                                                
081400*01    -COPY W0008     -PRE WDE41-                                        
081500     05  FILLER                  PIC X.                                   
081600     SKIP2                                                                
081700*01    -COPY W0008     -PRE WDE4A-                                        
081800     05  FILLER                  PIC X.                                   
081900     EJECT                                                                
082000*01    -COPY W0008     -PRE WDE4E-                                        
082100     05  FILLER                  PIC X.                                   
082200     EJECT                                                                
082300*01    -COPY W0008     -PRE WDE4-                                         
082400     05  FILLER                  PIC X.                                   
082500     EJECT                                                                
082600*01    -COPY W0008     -PRE WDE42-                                        
082700     05  FILLER                  PIC X.                                   
082800     EJECT                                                                
082900*01    -COPY W0008     -PRE WDE6-                                         
083000     05  FILLER                  PIC X.                                   
083100     EJECT                                                                
083200*01    -COPY W0008     -PRE WDK5-                                         
083300     05  FILLER                  PIC X.                                   
083400     EJECT                                                                
083500*01    -COPY W0008     -PRE 4726-                                         
083600     05  FILLER                  PIC X.                                   
083700     EJECT                                                                
083800*01    -COPY W0008     -PRE WDQ3-                                         
083900     05  FILLER                  PIC X.                                   
084000     EJECT                                                                
084100*01    -COPY W0008     -PRE 4472-                                         
084200     05  FILLER                  PIC X.                                   
084300     EJECT                                                                
084400*01    -COPY W0008     -PRE 4478-                                         
084500     05  FILLER                  PIC X.                                   
084600     EJECT                                                                
084700*01    -COPY W0008     -PRE 4322-                                         
084800     05  FILLER                  PIC X.                                   
084900     EJECT                                                                
085000*01    -COPY W0008     -PRE WDG6-                                         
085100     05  FILLER                  PIC X.                                   
085200     EJECT                                                                
085300*01    -COPY W0008     -PRE WDQ2-                                         
085400     05  FILLER                  PIC X.                                   
085500     EJECT                                                                
085600*01  -COPY W0008       -PRE ORQL-                                         
085700     05  FILLER                  PIC X.                                   
085800     EJECT                                                                
085900*01    -COPY W0008     -PRE WDE62-                                        
086000     05  FILLER                  PIC X.                                   
086100     EJECT                                                                
086200*01  -COPY W0008       -PRE PLATS-DM-                                     
086300     05  FILLER                  PIC X.                                   
086400     EJECT                                                                
086500*01  -COPY W0008       -PRE PLATS-DN-                                     
086600     05  FILLER                  PIC X.                                   
086700     EJECT                                                                
086800*01  -COPY W0008       -PRE PLATS-DP-                                     
086900     05  FILLER                  PIC X.                                   
087000     EJECT                                                                
087100*01  -COPY W0008       -PRE PLATS-DO-                                     
087200     05  FILLER                  PIC X.                                   
087300     EJECT                                                                
087400*01  -COPY W0008       -PRE PLATS-WDE6C-                                  
087500     05  FILLER                  PIC X.                                   
087600     EJECT                                                                
087700*01  -COPY W0008       -PRE PLATS-GMTC-                                   
087800     05  FILLER                  PIC X.                                   
087900     EJECT                                                                
088000*01  -COPY W0008       -PRE PLATS-WDB6-                                   
088100     05  FILLER                  PIC X.                                   
088200     EJECT                                                                
088300*01  -COPY W0008       -PRE WDB6-                                         
088400     05  FILLER                  PIC X.                                   
088500     EJECT                                                                
088600*01  -COPY W0008       -PRE WDA6B-                                        
088700     05  FILLER                  PIC X.                                   
088800     EJECT                                                                
088900 01  DNOT-ORQP-PCB               PIC X.                                   
089000 01  DNOT-ORQP2-PCB              PIC X.                                   
089100 01  DNOT-ORQP3-PCB              PIC X.                                   
089200 01  DNOT-4013-PCB               PIC X.                                   
089300 01  DNOT-BENA-PCB               PIC X.                                   
089400     EJECT                                                                
089500  PROCEDURE DIVISION USING MSG-PCB ALT4397-PCB ALT-PCB                    
089600                           WDP7-PCB                                       
089700                           WDE41-PCB WDE4A-PCB WDE4E-PCB                  
089800                           WDE4-PCB WDE42-PCB                             
089900                           WDE6-PCB  WDK5-PCB  4726-PCB WDQ3-PCB          
090000                           4472-PCB  4478-PCB  4322-PCB WDG6-PCB          
090100                           WDQ2-PCB  ORQL-PCB WDE62-PCB                   
090200                           PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB         
090300                           PLATS-DO-PCB PLATS-WDE6C-PCB                   
090400                           PLATS-GMTC-PCB PLATS-WDB6-PCB                  
090500                           WDB6-PCB  WDA6B-PCB                            
090600                           DNOT-ORQP-PCB                                  
090700                           DNOT-ORQP2-PCB                                 
090800                           DNOT-ORQP3-PCB                                 
090900                           DNOT-4013-PCB                                  
091000                           DNOT-BENA-PCB.                                 
091100     ENTRY 'DLITCBL' USING MSG-PCB ALT4397-PCB ALT-PCB                    
091200                           WDP7-PCB                                       
091300                           WDE41-PCB WDE4A-PCB WDE4E-PCB                  
091400                           WDE4-PCB WDE42-PCB                             
091500                           WDE6-PCB  WDK5-PCB  4726-PCB WDQ3-PCB          
091600                           4472-PCB  4478-PCB  4322-PCB WDG6-PCB          
091700                           WDQ2-PCB  ORQL-PCB WDE62-PCB                   
091800                           PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB         
091900                           PLATS-DO-PCB PLATS-WDE6C-PCB                   
092000                           PLATS-GMTC-PCB PLATS-WDB6-PCB                  
092100                           WDB6-PCB  WDA6B-PCB                            
092200                           DNOT-ORQP-PCB                                  
092300                           DNOT-ORQP2-PCB                                 
092400                           DNOT-ORQP3-PCB                                 
092500                           DNOT-4013-PCB                                  
092600                           DNOT-BENA-PCB.                                 
092700     MOVE SPACE     TO MED-IDMFSFEL                                       
092800     PERFORM A-LAS-IN-RADER                                               
092900*    NU ÄR ALLA RADERNA INLÄSTA , BÖRJA KOLLA                             
093000         MOVE 'INLÄ'    TO WPOS                                           
093100     IF WS-INDATA-RATT                                                    
093200       IF CARR-FLSISTAK = 0 OR 1                                          
093300          CONTINUE                                                        
093400       ELSE                                                               
093500         MOVE FEL                       TO WS-INDATA-TEST                 
093600         MOVE ERR-INVALID-VALUE         TO MED-IDMFSFEL                   
093700         PERFORM S03-WMEDKONV                                             
093800         MOVE 'SISTA-KOLLI-MARKERING' TO MED-TEMFSFEL (17:21)             
093900       END-IF                                                             
094000       IF CARR-KDEMBTYP NOT > 9                                           
094100          CONTINUE                                                        
094200       ELSE                                                               
094300         MOVE FEL                       TO WS-INDATA-TEST                 
094400         MOVE ERR-INVALID-VALUE         TO MED-IDMFSFEL                   
094500         PERFORM S03-WMEDKONV                                             
094600         MOVE 'EMBALLAGETYP'         TO MED-TEMFSFEL (17:12)              
094700       END-IF                                                             
094800     END-IF                                                               
094900     IF WS-INDATA-RATT                                                    
095000                                                                          
095100         PERFORM B-GENERELL-KONTROLL                                      
095200         PERFORM X-LAES-RAETT-ORDERDEL                                    
095300*                                                                         
095400         IF WS-INDATA-RATT                                                
095500           IF AVRAPP-UTAN-KOLLI                                           
095600             CONTINUE                                                     
095700           ELSE                                                           
095800             PERFORM C-RELATIONSKONTROLL                                  
095900*                                                                         
096000             IF WS-INDATA-RATT                                            
096100                 PERFORM D-LAGG-UPP-KOLLI-SEG                             
096200                                                                          
096300                 MOVE 1               TO INX                              
096400                 PERFORM UNTIL INX > MIX                                  
096500*                     ANTAL RADER I MID-TABELL                            
096600                   PERFORM F-BEHANDLA-RADER                               
096700                   ADD 1              TO INX                              
096800                 END-PERFORM                                              
096900**                                                                        
097000                 IF WS-BEHANDLING-RATT                                    
097100                     PERFORM G-UPPDATERA-KOLLIREG                         
097200                     IF  WS-FLAUTFAK    = JA                              
097300                       IF DIST03-SVERIGE-EJ-778                           
097400                       OR DIST18-SKROT                                    
097500                          PERFORM K-UPPDAT-4726-4727                      
097600                       END-IF                                             
097700                     END-IF                                               
097800                 END-IF                                                   
097900             END-IF                                                       
098000           END-IF                                                         
098100         END-IF                                                           
098200                                                                          
098300     END-IF                                                               
098400     PERFORM S01-RECEIVE-CLOSE                                            
098500                                                                          
098600     IF WS-INDATA-RATT                                                    
098700        IF WS-BEHANDLING-RATT                                             
098800           PERFORM N-EV-STARTA-4397                                       
098900        END-IF                                                            
099000     END-IF                                                               
099100     PERFORM M-SEND-SVAR                                                  
099200                                                                          
099300     MOVE ZERO TO RETURN-CODE                                             
099400     GOBACK                                                               
099500     .                                                                    
099600     EJECT                                                                
099700 A-LAS-IN-RADER  SECTION.                                                 
099800     PERFORM S01-RECEIVE-OPEN                                             
099900     IF RECV-KDRC =  0                                                    
100000       MOVE RECV-ADDISPXTRA       TO WS-ADDISPXTRA                        
100100       PERFORM S01-RECEIVE-MESSAGE                                        
100200       IF RECV-KDRC = 0                                                   
100300        IF REQU-IDMTYP3IV = 'RequestCarrierFinalization'                  
100400         PERFORM AA-INIT                                                  
100500         PERFORM AB-REQU-TILL-RESP                                        
100600*                                                                         
100700         PERFORM S01-RECEIVE-MESSAGE2                                     
100800         IF RECV-KDRC = 0                                                 
100900          IF CARR-IDRTYP3IV  = 'CarrierSummary'                           
101000           MOVE CARR-IDPRODNR     TO WS-IDPRODNR                          
101100           MOVE CARR-IDPLKLST     TO WS-IDPLKLST                          
101200           MOVE CARR-IDPRC        TO WS-IDPRC                             
101300           MOVE CARR-IDLOPNR-ORD  TO WS-IDLOPNR-ORD                       
101400           IF CARR-FLSISTAK = 0                                           
101500             MOVE NEJ             TO WS-FLSISTAK                          
101600           ELSE                                                           
101700             MOVE JA              TO WS-FLSISTAK                          
101800           END-IF                                                         
101900                                                                          
102000           MOVE CARR-KDEMBTYP     TO WS-KDEMBTYP                          
102100           MOVE CARR-KDKOLLI      TO WS-KDKOLLI                           
102200           MOVE CARR-DIKOLLIH     TO WS-DIKOLLIH                          
102300           MOVE CARR-DIKOLLIL     TO WS-DIKOLLIL                          
102400           MOVE CARR-DIKOLLIB     TO WS-DIKOLLIB                          
102500           PERFORM S01-RECEIVE-MESSAGE3                                   
102600           IF RECV-KDRC = 0                                               
102700            MOVE ZERO    TO MIX                                           
102800            PERFORM UNTIL RECV-KDRC  > 0                                  
102900*    LÄS IN ALLA RADER I KOLLIT                                           
103000             ADD 1 TO MIX                                                 
103100             IF PICR-IDRTYP3IV  = 'PickTaskResult'                        
103200              IF MIX > WS-MAX-ANT-RAD                                     
103300*              FÖR MÅNGA RADER                                            
103400               MOVE FEL                  TO WS-INDATA-TEST                
103500               MOVE ERR-TOO-MANYLINES    TO MED-IDMFSFEL                  
103600               PERFORM S03-WMEDKONV                                       
103700              ELSE                                                        
103800               MOVE PICR-IDPURAD       TO MID-IDPURAD (MIX)               
103900               MOVE PICR-KVAVBART      TO MID-KVAVBART (MIX)              
104000               MOVE PICR-KVLEVART      TO MID-KVLEVART (MIX)              
104100               MOVE PICR-KDARTURS-NUM  TO MID-KDARTURS-NUM (MIX)          
104200               MOVE SPACE              TO MID-KDARTURS (MIX)              
104300              END-IF                                                      
104400             END-IF                                                       
104500             PERFORM S01-RECEIVE-MESSAGE3                                 
104600            END-PERFORM                                                   
104700           END-IF                                                         
104800          ELSE                                                            
104900           MOVE FEL                       TO WS-INDATA-TEST               
105000           MOVE ERR-INV-MSG-STRUCTURE     TO MED-IDMFSFEL                 
105100           PERFORM S03-WMEDKONV                                           
105200          END-IF                                                          
105300         ELSE                                                             
105400           MOVE FEL                       TO WS-INDATA-TEST               
105500           MOVE ERR-INV-MSG-STRUCTURE     TO MED-IDMFSFEL                 
105600           PERFORM S03-WMEDKONV                                           
105700         END-IF                                                           
105800        ELSE                                                              
105900         MOVE FEL                         TO WS-INDATA-TEST               
106000         MOVE ERR-INV-MSG-STRUCTURE       TO MED-IDMFSFEL                 
106100         PERFORM S03-WMEDKONV                                             
106200        END-IF                                                            
106300       END-IF                                                             
106400                                                                          
106500       MOVE JA                            TO FL-AVRAPP-MED-KOLLI          
106600       IF MIX = ZERO                                                      
106700         IF CARR-KDEMBTYP = ZERO                                          
106800         AND CARR-KDKOLLI = SPACE                                         
106900         AND CARR-FLSISTAK = 1                                            
107000*         FÖR ATT AVSLUTA ORDER DÄR SISTA KOLLI EJ BLIVIT SATT            
107100*         MEN ALLA KOLLIN REDAN RAPPORTERADE                              
107200           MOVE NEJ                       TO FL-AVRAPP-MED-KOLLI          
107300         ELSE                                                             
107400           IF CARR-KDKOLLI NOT = SPACE                                    
107500             MOVE ERR-CASE-WITHOUT-LINES    TO MED-IDMFSFEL               
107600           ELSE                                                           
107700             MOVE ERR-INV-MSG-STRUCTURE     TO MED-IDMFSFEL               
107800           END-IF                                                         
107900           PERFORM S03-WMEDKONV                                           
108000           MOVE FEL                       TO WS-INDATA-TEST               
108100         END-IF                                                           
108200       END-IF                                                             
108300     ELSE                                                                 
108400        MOVE FEL                          TO WS-INDATA-TEST               
108500        MOVE ERR-INV-MSG-STRUCTURE        TO MED-IDMFSFEL                 
108600        PERFORM S03-WMEDKONV                                              
108700     END-IF                                                               
108800     .                                                                    
108900     EJECT                                                                
109000 AA-INIT             SECTION.                                             
109100                                                                          
109200*                                                                         
109300     MOVE LOW-VALUE                       TO   MSG-AREA                   
109400*                                                                         
109500     MOVE 'AA  '   TO WPOS                                                
109600     IF ENGLISH-TEXT                                                      
109700         MOVE +2                          TO   INDX                       
109800     ELSE                                                                 
109900         MOVE +1                          TO   INDX                       
110000     END-IF                                                               
110100     MOVE RAETT                           TO WS-INDATA-TEST               
110200                                             WS-BEHANDLING-TEST           
110300                                                                          
110400     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
110500     ACCEPT WS-TIDPUNKT                   FROM TIME                       
110600                                                                          
110700     PERFORM AA-FLYTTA-NYCKLAR                                            
110800     MOVE NEJ                   TO DIRLEV-KOLLI-SW                        
110900                                   NYA-NYCKLAR-SW                         
111000                                                                          
111100     MOVE WS-IDDC TO W-IDDC-B6                                            
111200     PERFORM IMS-GU-WDB601                                                
111300*     FEL 440 OM EJ DC 11                                                 
111400                                                                          
111500     MOVE ZERO                      TO PALLAR (1)                         
111600                                       PALLAR (2)                         
111700                                       PALLAR (3)                         
111800                                       PALLAR (4)                         
111900                                       PALLAR (5)                         
112000                                                                          
112100     MOVE ZERO                      TO KRAGAR (1)                         
112200                                       KRAGAR (2)                         
112300                                       KRAGAR (3)                         
112400                                       KRAGAR (4)                         
112500                                       KRAGAR (5)                         
112600                                                                          
112700     MOVE ZERO                      TO EMB-LOCK (1)                       
112800                                       EMB-LOCK (2)                       
112900                                       EMB-LOCK (3)                       
113000                                       EMB-LOCK (4)                       
113100                                       EMB-LOCK (5)                       
113200                                       LOGG-IDLOGLOP                      
113300                                                                          
113400     .                                                                    
113500     EJECT                                                                
113600 AA-FLYTTA-NYCKLAR  SECTION.                                              
113700                                                                          
113800     MOVE JA                              TO   NYA-NYCKLAR-SW             
113900     MOVE REQU-IDANSTNR                   TO   WS-IDANSTNR                
114000     MOVE 'AAA '  TO WPOS                                                 
114100                                                                          
114200*     HÄMTA IDDC FRÅN REQUESTHEADER, KOLLA MOT WDB6.                      
114300     MOVE REQU-IDDC                       TO WS-IDDC                      
114400                                                                          
114500     IF WS-IDDC NOT = '11'                                                
114600       MOVE FEL                           TO WS-INDATA-TEST               
114700       MOVE ERR-INVALID-DC                TO MED-IDMFSFEL                 
114800       PERFORM S03-WMEDKONV                                               
114900     END-IF                                                               
115000                                                                          
115100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
115200     MOVE '013'             TO MSGI-KDCALL                                
115300     MOVE 'WIDDC   '        TO MSGI-IDUSER                                
115400     MOVE WS-IDDC           TO MSGI-IDUSER(6:2)                           
115500*    MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
115600     MOVE '431B'            TO MSGI-IDTRANS                               
115700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
115800     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
115900                                                                          
116000     MOVE MSGI-TILOKDAT     TO WS-TILOKDAT                                
116100     MOVE WS-TILOKDAT       TO WS-DAGENS-DATUM                            
116200     MOVE MSGI-TILOKTID     TO WS-TILOKTID                                
116300     MOVE MSGI-TILOKTID     TO WS-TIDPUNKT (1:4)                          
116400     .                                                                    
116500     EJECT                                                                
116600 AB-REQU-TILL-RESP    SECTION.                                            
116700     MOVE 'ResponseCarrierFinalization'  TO RESP-IDMTYP3IV                
116800     MOVE REQU-IDMSG3IV      TO RESP-IDMSG3IV                             
116900     MOVE REQU-TISTAMP3IV    TO RESP-TISTAMP3IV                           
117000     MOVE REQU-IDDC          TO RESP-IDDC                                 
117100     MOVE REQU-IDANSTNR      TO RESP-IDANSTNR                             
117200     MOVE REQU-IDSNO3IV      TO RESP-IDSNO3IV                             
117300                                                                          
117400     .                                                                    
117500     EJECT                                                                
117600 B-GENERELL-KONTROLL  SECTION.                                            
117700                                                                          
117800     IF WS-IDANSTNR NOT NUMERIC                                           
117900         MOVE FEL                       TO WS-INDATA-TEST                 
118000         MOVE ERR-INVALID-VALUE         TO MED-IDMFSFEL                   
118100         PERFORM S03-WMEDKONV                                             
118200         MOVE 'PACKAREIDENTITET'   TO MED-TEMFSFEL (17:16)                
118300     END-IF                                                               
118400                                                                          
118500     IF WS-IDPRODNR NOT NUMERIC                                           
118600         MOVE FEL                       TO WS-INDATA-TEST                 
118700         MOVE ERR-INVALID-VALUE         TO MED-IDMFSFEL                   
118800         PERFORM S03-WMEDKONV                                             
118900         MOVE 'PRODUKTIONSNUMMER'   TO MED-TEMFSFEL (17:17)               
119000     END-IF                                                               
119100                                                                          
119200     IF WS-IDPLKLST NOT NUMERIC                                           
119300         MOVE FEL                       TO WS-INDATA-TEST                 
119400         MOVE ERR-INVALID-VALUE         TO MED-IDMFSFEL                   
119500         PERFORM S03-WMEDKONV                                             
119600         MOVE 'PLOCKLISTENUMMER'        TO MED-TEMFSFEL (17:16)           
119700     END-IF                                                               
119800                                                                          
119900     IF AVRAPP-MED-KOLLI                                                  
120000       PERFORM BA-NEXT-KOLLINR                                            
120100       PERFORM BB-KONTROLLERA-KOLLIKOD                                    
120200                                                                          
120300       MOVE +1                        TO INX                              
120400       PERFORM UNTIL INX > MIX                                            
120500         PERFORM BD-KONTROLLERA-RAD                                       
120600         ADD +1                     TO INX                                
120700       END-PERFORM                                                        
120800     END-IF                                                               
120900     .                                                                    
121000     EJECT                                                                
121100 BA-NEXT-KOLLINR        SECTION.                                          
121200*     HÄMTA NÄSTA KOLLINUMMER  TILL WS-IDKOLLI                            
121300     MOVE WS-IDPRC                     TO W-PRC-B6                        
121400     PERFORM IMS-GU-WDB612                                                
121500     IF WS-IDPRC = ZERO OR SEGMENT-SAKNAS                                 
121600*     FEL OM PRC SAKNAS                                                   
121700        MOVE FEL                       TO WS-INDATA-TEST                  
121800        MOVE ERR-PROD-KANAL            TO MED-IDMFSFEL                    
121900        PERFORM S03-WMEDKONV                                              
122000     ELSE                                                                 
122100       IF PRC-IDKOLLI-PRCSTA = ZERO                                       
122200*     FEL OM STARTKOLLI SAKNAS                                            
122300        MOVE FEL                       TO WS-INDATA-TEST                  
122400        MOVE ERR-START-CASENUMBER      TO MED-IDMFSFEL                    
122500        PERFORM S03-WMEDKONV                                              
122600       ELSE                                                               
122700*     LÄS FRAM TILL LEDIG IDKOLLI                                         
122800        MOVE WS-IDPRODNR               TO W-601-IDPRODNR                  
122900        MOVE PRC-IDKOLLI-PRCSTA        TO W-611-IDKOLLI                   
123000        PERFORM IMS-GU-WDE601                                             
123100        PERFORM UNTIL SEGMENT-SAKNAS                                      
123200         PERFORM IMS-GNP-WDE611                                           
123300         IF SEGMENT-FINNS                                                 
123400           ADD 1                        TO W-611-IDKOLLI                  
123500         END-IF                                                           
123600        END-PERFORM                                                       
123700       END-IF                                                             
123800     END-IF                                                               
123900     .                                                                    
124000     EJECT                                                                
124100 BB-KONTROLLERA-KOLLIKOD    SECTION.                                      
124200                                                                          
124300     IF WS-KDKOLLI NOT = SPACE                                            
124400         PERFORM BBA-KONTROLLERA-UPPG                                     
124500     ELSE                                                                 
124600         MOVE FEL                       TO WS-INDATA-TEST                 
124700         MOVE ERR-MORE-CASE-INF-NEED    TO MED-IDMFSFEL                   
124800         PERFORM S03-WMEDKONV                                             
124900         MOVE 'KOLLIKOD'            TO MED-TEMFSFEL (28:8)                
125000     END-IF                                                               
125100     .                                                                    
125200     EJECT                                                                
125300 BBA-KONTROLLERA-UPPG   SECTION.                                          
125400                                                                          
125500     IF WS-DIKOLLIL NOT = ZERO                                            
125600         IF WS-DIKOLLIL NUMERIC                                           
125700             IF US-MEASUREMENT                                            
125800               COMPUTE WS-DIKOLLIL ROUNDED =                              
125900                       WS-DIKOLLIL * CONV-IN-TO-CM                        
126000               END-COMPUTE                                                
126100             END-IF                                                       
126200         ELSE                                                             
126300           IF WS-INDATA-RATT                                              
126400             MOVE FEL                   TO WS-INDATA-TEST                 
126500             MOVE ERR-INVALID-VALUE     TO MED-IDMFSFEL                   
126600             PERFORM S03-WMEDKONV                                         
126700             MOVE 'LÄNGD'            TO MED-TEMFSFEL (17:5)               
126800           END-IF                                                         
126900         END-IF                                                           
127000     ELSE                                                                 
127100         MOVE ZERO                      TO WS-DIKOLLIL                    
127200     END-IF                                                               
127300                                                                          
127400     IF WS-DIKOLLIH NOT = ZERO                                            
127500         IF WS-DIKOLLIH NUMERIC                                           
127600             IF US-MEASUREMENT                                            
127700               COMPUTE WS-DIKOLLIH ROUNDED =                              
127800                       WS-DIKOLLIH * CONV-IN-TO-CM                        
127900               END-COMPUTE                                                
128000             END-IF                                                       
128100         ELSE                                                             
128200           IF WS-INDATA-RATT                                              
128300             MOVE FEL                   TO WS-INDATA-TEST                 
128400             MOVE ERR-INVALID-VALUE     TO MED-IDMFSFEL                   
128500             PERFORM S03-WMEDKONV                                         
128600             MOVE 'HÖJD'             TO MED-TEMFSFEL (17:4)               
128700           END-IF                                                         
128800         END-IF                                                           
128900     ELSE                                                                 
129000         MOVE ZERO                      TO WS-DIKOLLIH                    
129100     END-IF                                                               
129200                                                                          
129300     IF WS-DIKOLLIB NOT = ZERO                                            
129400         IF WS-DIKOLLIB NUMERIC                                           
129500*            MOVE WS-DIKOLLIB          TO WS-DIKOLLIB                     
129600             IF US-MEASUREMENT                                            
129700               COMPUTE WS-DIKOLLIB ROUNDED =                              
129800                       WS-DIKOLLIB * CONV-IN-TO-CM                        
129900               END-COMPUTE                                                
130000             END-IF                                                       
130100         ELSE                                                             
130200           IF WS-INDATA-RATT                                              
130300             MOVE FEL                   TO WS-INDATA-TEST                 
130400             MOVE ERR-INVALID-VALUE     TO MED-IDMFSFEL                   
130500             PERFORM S03-WMEDKONV                                         
130600             MOVE 'BREDD'           TO MED-TEMFSFEL (17:5)                
130700           END-IF                                                         
130800         END-IF                                                           
130900     ELSE                                                                 
131000         MOVE ZERO                      TO WS-DIKOLLIB                    
131100     END-IF                                                               
131200     .                                                                    
131300     EJECT                                                                
131400 BD-KONTROLLERA-RAD     SECTION.                                          
131500                                                                          
131600     IF MID-IDPURAD  (INX) NUMERIC                                        
131700       IF MID-IDPURAD  (INX) > ZERO                                       
131800          CONTINUE                                                        
131900       ELSE                                                               
132000          MOVE FEL                  TO WS-INDATA-TEST                     
132100          MOVE ERR-WRONG-INTERV-INF TO MED-IDMFSFEL                       
132200          PERFORM S03-WMEDKONV                                            
132300       END-IF                                                             
132400     ELSE                                                                 
132500       MOVE FEL                    TO  WS-INDATA-TEST                     
132600       MOVE ERR-INVALID-VALUE      TO MED-IDMFSFEL                        
132700       PERFORM S03-WMEDKONV                                               
132800       MOVE 'RADNUMMER PACKUNDERLAG' TO MED-TEMFSFEL (17:22)              
132900     END-IF                                                               
133000                                                                          
133100     IF MID-KVLEVART (INX) NUMERIC                                        
133200        IF MID-KVLEVART (INX) = ZERO                                      
133300           MOVE FEL                  TO  WS-INDATA-TEST                   
133400           MOVE ERR-ZERO-NOT-ALLOWED TO MED-IDMFSFEL                      
133500           PERFORM S03-WMEDKONV                                           
133600           MOVE 'PACKAD KVANTITET'   TO MED-TEMFSFEL (18:16)              
133700        END-IF                                                            
133800     ELSE                                                                 
133900        MOVE FEL                     TO  WS-INDATA-TEST                   
134000        MOVE ERR-INVALID-VALUE       TO MED-IDMFSFEL                      
134100        PERFORM S03-WMEDKONV                                              
134200        MOVE 'PACKAD KVANTITET'      TO MED-TEMFSFEL(17:16)               
134300     END-IF                                                               
134400                                                                          
134500     IF MID-KDARTURS-NUM (INX) > ZERO                                     
134600       MOVE MID-KDARTURS-NUM (INX) TO ARTU-KDARTURS-NUM                   
134700       MOVE SPACE                  TO ARTU-IDDC                           
134800       MOVE SPACE                  TO ARTU-KDARTURS                       
134900       MOVE ZERO                   TO ARTU-IDDISTR                        
135000       CALL W400ARTU USING ARTU-W400ARTU                                  
135100       MOVE ARTU-KDARTURS          TO MID-KDARTURS (INX)                  
135200     END-IF                                                               
135300     .                                                                    
135400     EJECT                                                                
135500 C-RELATIONSKONTROLL  SECTION.                                            
135600                                                                          
135700*                                                                         
135800     IF WS-INDATA-RATT                                                    
135900         PERFORM CB-KONTROLLERA-IDKOLLI                                   
136000     END-IF                                                               
136100     IF WS-INDATA-RATT                                                    
136200         PERFORM CC-KONTROLLERA-PACKARE                                   
136300     END-IF                                                               
136400     IF WS-INDATA-RATT                                                    
136500         PERFORM CD-KONTROLLERA-KOLLIUPPG                                 
136600     END-IF                                                               
136700     IF WS-INDATA-RATT                                                    
136800         PERFORM CE-KOLLA-PLATSSATTNING                                   
136900     END-IF                                                               
137000     .                                                                    
137100     EJECT                                                                
137200 CB-KONTROLLERA-IDKOLLI SECTION.                                          
137300                                                                          
137400     MOVE WS-IDPRODNR                    TO   W-601-IDPRODNR              
137500     PERFORM IMS-GHU-KOLLIREG                                             
137600     IF VORD-IDDC = WS-IDDC                                               
137700       MOVE VORD-KDFRAKT                 TO   PLATS-KDFRAKT               
137800       MOVE VORD-KDORDKL                 TO   PLATS-KDORDKLX              
137900                                              WS-KDORDKL                  
138000       MOVE VORD-IDDC                    TO   PLATS-IDDC                  
138100       MOVE VORD-IDDISTR                 TO   PLATS-IDDISTR               
138200       MOVE VORD-IDKUNDNR                TO   PLATS-IDKUNDNR              
138300       MOVE VORD-FLAUTFAK                TO   WS-FLAUTFAK                 
138400       MOVE VORD-KDFAKTYP                TO   WS-KDFAKTYP                 
138500                                                                          
138600       IF WS-KDORDKL = 4                                                  
138700         IF VORD-IDDISTR = +00878                                         
138800         AND VORD-IDKUNDNR > +006000                                      
138900            MOVE 2                       TO   PLATS-KDCALL                
139000         ELSE                                                             
139100            MOVE 1                       TO   PLATS-KDCALL                
139200         END-IF                                                           
139300       ELSE                                                               
139400            MOVE 2                       TO   PLATS-KDCALL                
139500       END-IF                                                             
139600     ELSE                                                                 
139700*---------------------------------------------FELAKTIGT                   
139800*---------------------------------------------DC-LAGER                    
139900        MOVE FEL                         TO   WS-INDATA-TEST              
140000        MOVE ERR-INVALID-VALUE           TO MED-IDMFSFEL                  
140100        PERFORM S03-WMEDKONV                                              
140200        MOVE 'LAGERIDENTITET'       TO MED-TEMFSFEL (17:14)               
140300     END-IF                                                               
140400     .                                                                    
140500     EJECT                                                                
140600 CC-KONTROLLERA-PACKARE SECTION.                                          
140700                                                                          
140800     PERFORM CCA-KONTROLLERA-PLOCKLISTA                                   
140900*                                                                         
141000     IF WS-INDATA-RATT     AND                                            
141100        WS-PACKARES-ODEL-REDAN-KLARA = JA                                 
141200        MOVE FEL                  TO   WS-INDATA-TEST                     
141300        MOVE ERR-ORDER-PART-READY TO MED-IDMFSFEL                         
141400        PERFORM S03-WMEDKONV                                              
141500     END-IF                                                               
141600     .                                                                    
141700     EJECT                                                                
141800 CCA-KONTROLLERA-PLOCKLISTA           SECTION.                            
141900                                                                          
142000     IF KORD-KDPAKOLL NOT = ZERO                                          
142100*-----------------------------------------FÅR MAN EJ RAPPORTERA DÅ        
142200*-----------------------------------------AVVIKELSEKONTROLL PÅGÅR         
142300        MOVE FEL                   TO   WS-INDATA-TEST                    
142400        MOVE ERR-DEVIATION-CONTROL TO MED-IDMFSFEL                        
142500        PERFORM S03-WMEDKONV                                              
142600     ELSE                                                                 
142700                                                                          
142800       MOVE JA                TO WS-PACKARES-ODEL-REDAN-KLARA             
142900       IF ( (KORD-KVORDRAD-PACK NOT = KORD-KVORDRAD)       OR             
143000            (KORD-KVORDRAD-LEVPL    > 0                    AND            
143100             KORD-KVORDRAD-PACK NOT = KORD-KVORDRAD-LEVPL) )              
143200         MOVE NEJ             TO WS-PACKARES-ODEL-REDAN-KLARA             
143300       END-IF                                                             
143400     END-IF                                                               
143500     .                                                                    
143600     EJECT                                                                
143700 CD-KONTROLLERA-KOLLIUPPG  SECTION.                                       
143800                                                                          
143900     MOVE WS-KDKOLLI TO W-KDKOLLI-WDK5                                    
144000     PERFORM IMS-GU-WDK5                                                  
144100     IF SEGMENT-SAKNAS                                                    
144200*        EMBALLAGEKOD SAKNAS                                              
144300         MOVE FEL                   TO WS-INDATA-TEST                     
144400         MOVE ERR-CASE-CODE-MISSING TO MED-IDMFSFEL                       
144500         PERFORM S03-WMEDKONV                                             
144600     ELSE                                                                 
144700*------------------------------------------VISSA KOLLIKODER GER           
144800*------------------------------------------EJ ALLA MÅTT. DÅ SKALL         
144900*------------------------------------------DESSA KOMPLETTERAS.            
145000         MOVE EMB-KDKOLLID              TO WS-KDKOLLID                    
145100         MOVE EMB-VKTARA                TO WS-EMB-VKTARA-ONE-CASE         
145200         MOVE EMB-KDEMBTYP              TO WS-KDEMBTYP                    
145300                                                                          
145400         IF      EMB-EMBPROF NOT = SPACE                                  
145500             MOVE EMB-EMBPROF           TO WS-EMBPROF                     
145600             MOVE EMB-KVPALL            TO WS-KVPALL                      
145700             MOVE EMB-KVLOCK            TO WS-KVLOCK                      
145800             MOVE EMB-KVRAM             TO WS-KVRAM                       
145900         END-IF                                                           
146000                                                                          
146100         IF EMB-DIKOLLIL = ZERO                                           
146200             IF WS-DIKOLLIL = ZERO                                        
146300                 MOVE FEL                TO WS-INDATA-TEST                
146400                 MOVE ERR-DIM-ZERO       TO MED-IDMFSFEL                  
146500                 PERFORM S03-WMEDKONV                                     
146600                 MOVE 'LÄNGD'           TO MED-TEMFSFEL (28:5)            
146700             END-IF                                                       
146800         ELSE                                                             
146900             MOVE EMB-DIKOLLIL           TO WS-DIKOLLIL                   
147000         END-IF                                                           
147100                                                                          
147200         IF EMB-DIKOLLIH = ZERO                                           
147300             IF WS-DIKOLLIH = ZERO                                        
147400                 MOVE FEL                TO WS-INDATA-TEST                
147500                 MOVE ERR-DIM-ZERO       TO MED-IDMFSFEL                  
147600                 PERFORM S03-WMEDKONV                                     
147700                 MOVE 'HÖJD'             TO MED-TEMFSFEL (28:4)           
147800             END-IF                                                       
147900         ELSE                                                             
148000             MOVE EMB-DIKOLLIH           TO WS-DIKOLLIH                   
148100         END-IF                                                           
148200                                                                          
148300         IF EMB-DIKOLLIB = ZERO                                           
148400             IF WS-DIKOLLIB = ZERO                                        
148500                 MOVE FEL                TO WS-INDATA-TEST                
148600                 MOVE ERR-DIM-ZERO       TO MED-IDMFSFEL                  
148700                 PERFORM S03-WMEDKONV                                     
148800                 MOVE 'BREDD'            TO MED-TEMFSFEL (28:5)           
148900             END-IF                                                       
149000         ELSE                                                             
149100             MOVE EMB-DIKOLLIB           TO WS-DIKOLLIB                   
149200         END-IF                                                           
149300     END-IF                                                               
149400     .                                                                    
149500     EJECT                                                                
149600 CE-KOLLA-PLATSSATTNING SECTION.                                          
149700     SKIP3                                                                
149800     MOVE ZERO                          TO PLATS-ADVMODUL                 
149900                                           PLATS-ADHMODUL                 
150000                                           PLATS-ADFLOMR                  
150100                                           PLATS-ADRUTNIV                 
150200                                           PLATS-IDTRPTNR                 
150300                                           PLATS-DIHMODUL                 
150400                                           PLATS-VKORDNTO-KOLLI           
150500     MOVE SPACE                         TO PLATS-ADFLGEO                  
150600                                           PLATS-FLUTLAST                 
150700                                           PLATS-IDDC-CROSS               
150800     MOVE WS-IDDC                       TO PLATS-IDDC                     
150900     MOVE WS-IDORDNR                    TO PLATS-IDORDNR                  
151000     MOVE WS-DIKOLLIL                   TO PLATS-DIKOLLIL                 
151100     MOVE WS-DIKOLLIB                   TO PLATS-DIKOLLIB                 
151200     MOVE WS-DIKOLLIH                   TO PLATS-DIKOLLIH                 
151300     MOVE WS-KDKOLLID                   TO PLATS-KDKOLLID                 
151400                                                                          
151500     IF WS-KDORDKL = +4                                                   
151600        IF VORD-IDDISTR = +00878                                          
151700        AND VORD-IDKUNDNR > +006000                                       
151800           MOVE 2                       TO PLATS-KDCALL                   
151900        ELSE                                                              
152000           MOVE 1                       TO PLATS-KDCALL                   
152100        END-IF                                                            
152200     ELSE                                                                 
152300        MOVE +2                         TO PLATS-KDCALL                   
152400     END-IF                                                               
152500                                                                          
152600     CALL W403PLAT USING PLATS-W403PLAT                                   
152700                         PLATS-DM-PCB                                     
152800                         PLATS-DN-PCB                                     
152900                         PLATS-DP-PCB                                     
153000                         PLATS-DO-PCB                                     
153100                         PLATS-WDE6C-PCB                                  
153200                         PLATS-GMTC-PCB                                   
153300                         PLATS-WDB6-PCB                                   
153400                                                                          
153500     IF PLATS-KDSVAR = SPACE                                              
153600        MOVE PLATS-IDTRPTNR              TO WS-IDTRPTNR                   
153700        MOVE PLATS-ADFLGEO               TO WS-ADFLGEO                    
153800        MOVE PLATS-ADFLOMR               TO WS-ADFLOMR                    
153900        MOVE PLATS-ADRUTNIV              TO WS-ADRUTNIV                   
154000        MOVE PLATS-ADVMODUL              TO WS-ADVMODUL                   
154100        MOVE PLATS-ADHMODUL              TO WS-ADHMODUL                   
154200        MOVE PLATS-DIHMODUL              TO WS-DIHMODUL                   
154300        MOVE PLATS-DIDMODUL              TO WS-DIDMODUL                   
154400        MOVE PLATS-FLUTLAST              TO WS-FLUTLAST                   
154400        MOVE PLATS-IDDC-CROSS            TO WS-IDDC-CROSS                 
154500     ELSE                                                                 
154600        MOVE FEL                 TO WS-INDATA-TEST                        
154700        MOVE ERR-GENERAT-ADDRESS TO MED-IDMFSFEL                          
154800        PERFORM S03-WMEDKONV                                              
154900     END-IF                                                               
155000     .                                                                    
155100     SKIP3                                                                
155200 D-LAGG-UPP-KOLLI-SEG   SECTION.                                          
155300                                                                          
155400     MOVE W-611-IDKOLLI                TO WS-IDKOLLI-NUM                  
155500     PERFORM S09-SKAPA-KOLLI-SEGMENT                                      
155600                                                                          
155700     PERFORM IMS-ISRT-KOLLI                                               
155800                                                                          
155900     IF SEGMENT-FINNS-REDAN                                               
156000         MOVE FEL                    TO WS-BEHANDLING-TEST                
156100         MOVE ERR-CASE-ALREADY-REPOR TO MED-IDMFSFEL                      
156200         PERFORM S03-WMEDKONV                                             
156300     ELSE                                                                 
156400         IF WS-IDDC-CROSS > SPACES                                        
156500          MOVE W-KDSEGKEY-X     TO  CROSS-KDSEGKEY                        
156600          MOVE PLATS-IDDC       TO  CROSS-IDDC-SEND                       
156700          MOVE WS-IDDC-CROSS    TO  CROSS-IDDC-CROSS                      
156800          MOVE KOLLI-IDDISTR    TO  CROSS-IDDISTR                         
156900          MOVE KOLLI-IDKUNDNR   TO  CROSS-IDKUNDNR                        
157000          MOVE WS-IDPRODNR      TO  CROSS-IDPRODNR                        
157100          MOVE KOLLI-IDKOLLI    TO  CROSS-IDKOLLI                         
157200          MOVE KOLLI-IDLEVNR    TO  CROSS-IDLEVNR                         
157300          MOVE KOLLI-IDSUPREF   TO  CROSS-IDSUPREF                        
157400          MOVE KOLLI-DARFS(3:6) TO  CROSS-TIRFSDAT                        
157500          MOVE ZERO             TO  CROSS-IDTRPTNR-CROSS                  
157600          MOVE ZERO             TO  CROSS-TIRECXDAT                       
157700          MOVE ZERO             TO  CROSS-TIRECXTID                       
157710          MOVE ZERO             TO  CROSS-TISKEPPN                        
157720          MOVE ZERO             TO  CROSS-IDSHIPM-CROSS                   
157730          MOVE SPACE            TO  CROSS-IDLBBET-CROSS                   
157800          MOVE 1                TO  CROSS-KDKOLSTA-CROSS                  
157900          PERFORM IMS-ISRT-WDE621                                         
158000         END-IF                                                           
158010     END-IF                                                               
158100                                                                          
158200     MOVE W-401-IDDISTR              TO WS-IDDISTR-NUM                    
158300     MOVE W-401-IDDISTR              TO TEST-IDDISTR                      
158400     .                                                                    
158500     EJECT                                                                
158600 F-BEHANDLA-RADER     SECTION.                                            
158700                                                                          
158800     MOVE MID-IDPURAD (INX)             TO   ARB-RAD-FOM                  
158900                                                                          
159000     MOVE NEJ                           TO  FL-RAD-INTERVALL              
159100     MOVE ARB-RAD-FOM                   TO  WS-START-RAD                  
159200                                            WS-SISTA-RAD                  
159300                                                                          
159400     MOVE 'N'                           TO WS-TRAEFF-RAD                  
159500*                                                                         
159600     MOVE KORD-IDDISTR                    TO W-401-IDDISTR                
159700     MOVE KORD-IDKUNDNR                   TO W-401-IDKUNDNR               
159800     MOVE KORD-IDORDNR5                   TO W-401-IDORDNR                
159900     MOVE KORD-IDPRODNR                   TO W-401-IDPRODNR               
160000*    -- RÄTT IDPLKLST FRÅN INDATA                                         
160100     MOVE CARR-IDPLKLST                   TO W-401-IDPLKLST               
160200                                             WS-IDPLKLST                  
160300     PERFORM IMS-GU-WDE401                                                
160400                                                                          
160500     MOVE KORD-IDPRODNR                   TO WS-JFR-IDPRODNR              
160600     MOVE KORD-IDUSER                     TO WS-JFR-IDANSTNR              
160700     MOVE KORD-IDORDER                    TO WS-SPAR-IDORDER              
160800     MOVE KORD-IDDC                       TO WS-SPAR-IDDC                 
160900                                                                          
161000     MOVE MID-KVLEVART (INX)              TO ARB-KVLEVART                 
161100                                             WS-KVLEVART                  
161200                                                                          
161300     IF WS-BEHANDLING-RATT                                                
161400        PERFORM FA-KONTROLLERA-RADER                                      
161500                                                                          
161600        IF WS-BEHANDLING-RATT                                             
161700          PERFORM FB-BEHANDLA-RAD-INOM-INTERVALL                          
161800                                                                          
161900          IF WS-BEHANDLING-RATT                                           
162000             MOVE WS-MOD-VKORDBTO     TO PLATS-VKORDNTO-KOLLI             
162100             MOVE WS-IDDISTR-NUM TO TEST-IDDISTR                          
162200             IF NOT DIST19-SATS                                           
162300                PERFORM FD-UPPDATERA-PRODTAB                              
162400             END-IF                                                       
162500          END-IF                                                          
162600        END-IF                                                            
162700     END-IF                                                               
162800     .                                                                    
162900     EJECT                                                                
163000 FA-KONTROLLERA-RADER SECTION.                                            
163100     SKIP3                                                                
163200     MOVE ARB-RAD-FOM                 TO ARB-RAD-AKTUELL                  
163300     MOVE ARB-RAD-AKTUELL             TO W-420-IDPURAD2                   
163400*                                                                         
163500     PERFORM IMS-GU-E411-RAD                                              
163600     IF SEGMENT-SAKNAS                                                    
163700       MOVE FEL                    TO WS-BEHANDLING-TEST                  
163800       MOVE ERR-WRONG-INTERV-INF   TO MED-IDMFSFEL                        
163900       PERFORM S03-WMEDKONV                                               
164000     ELSE                                                                 
164100       MOVE 'J'                       TO WS-TRAEFF-RAD                    
164200       IF WS-JFR-IDANSTNR-5 = '00000'                                     
164300          MOVE FEL                    TO WS-BEHANDLING-TEST               
164400          MOVE ERR-ORDER-SPLIT        TO MED-IDMFSFEL                     
164500          PERFORM S03-WMEDKONV                                            
164600       ELSE                                                               
164700         IF ORAD-KDRADSTA > 3                                             
164800            MOVE FEL                    TO WS-BEHANDLING-TEST             
164900            MOVE ERR-INTERV-ALREADY-REP TO MED-IDMFSFEL                   
165000            PERFORM S03-WMEDKONV                                          
165100         ELSE                                                             
165200           IF ORAD-FLNOLLJ = JA                                           
165300             MOVE FEL                     TO WS-BEHANDLING-TEST           
165400             MOVE ERR-LINE-ZEROED-BY-HUNT TO MED-IDMFSFEL                 
165500             PERFORM S03-WMEDKONV                                         
165600           END-IF                                                         
165700         END-IF                                                           
165800       END-IF                                                             
165900     END-IF                                                               
166000     .                                                                    
166100     EJECT                                                                
166200 FB-BEHANDLA-RAD-INOM-INTERVALL SECTION.                                  
166300                                                                          
166400*     OBS ENDAST EN RAD NU, INGET INTERVALL                               
166500     MOVE ZERO                   TO  WS-RINT-ANT-FPACK-ORAD               
166600                                                                          
166700     MOVE WS-IDPRODNR            TO  W-420-IDPRODNR-MIN                   
166800                                     W-420-IDPRODNR-MAX                   
166900                                     W-420-IDPRODNR                       
167000     MOVE WS-START-RAD           TO  W-420-IDPURAD-MIN                    
167100                                     W-420-IDPURAD                        
167200                                     WS-AKTUELL-RAD                       
167300     MOVE WS-SISTA-RAD           TO  W-420-IDPURAD-MAX                    
167400     MOVE JA                     TO  FL-RAD-INOM-INTERVALL                
167500                                                                          
167600     PERFORM IMS-GHU-RAD-SEK                                              
167700                                                                          
167800     PERFORM UNTIL (NOT RAD-FINNS-I-INTERVALL)                            
167900               OR  (NOT WS-BEHANDLING-RATT)                               
168000                                                                          
168100      PERFORM FBA-SPARA-RAD-INFO                                          
168200      IF WS-BEHANDLING-RATT                                               
168300                                                                          
168400                                                                          
168500       PERFORM FBB-UPPDATERA-RAD                                          
168600       IF WS-BEHANDLING-RATT                                              
168700         PERFORM FBC-LAGG-UPP-KOLLI-KOPPL                                 
168800         ADD 1 TO WS-AKTUELL-RAD                                          
168900         IF WS-AKTUELL-RAD > WS-SISTA-RAD                                 
169000             MOVE NEJ TO FL-RAD-INOM-INTERVALL                            
169100         ELSE                                                             
169200             PERFORM IMS-GHN-RAD-SEK                                      
169300         END-IF                                                           
169400       END-IF                                                             
169500      END-IF                                                              
169600     END-PERFORM                                                          
169700     .                                                                    
169800     EJECT                                                                
169900 FBA-SPARA-RAD-INFO        SECTION.                                       
170000     MOVE ORAD-VKARTNTO         TO  SPAR-PRAD-VKARTNTO                    
170100     MOVE ORAD-KVFLAMP          TO  SPAR-PRAD-KVFLAMP                     
170200     MOVE ORAD-KDFARLIG         TO  SPAR-PRAD-KDFARLIG                    
170300     MOVE ORAD-PRARTNTO         TO  SPAR-PRAD-PRARTNTO                    
170400     MOVE ORAD-PRAVCOST         TO  SPAR-PRAD-PRAVCOST                    
170500     MOVE ORAD-PRARTNTO-LOC     TO  SPAR-PRAD-PRARTNTO-LOC                
170600     MOVE ORAD-PRARTNTO-LOCPREL TO  SPAR-PRAD-PRARTNTO-LOCPREL            
170700     MOVE ORAD-KDVALISO         TO  SPAR-PRAD-KDVALISO                    
170800     MOVE ORAD-KDVALISO-EXP     TO  SPAR-PRAD-KDVALISO-EXP                
170900*                                                                         
171000     MOVE ORAD-IDPSN            TO SPAR-IDPSN                             
171100     MOVE ORAD-VKART-FG         TO SPAR-VKART-FG                          
171200     MOVE ORAD-VLFG             TO SPAR-VLFG                              
171300     MOVE ORAD-SUEQFG           TO SPAR-SUEQFG                            
171400*                                                                         
171500     MOVE ORAD-IDARTNR          TO WS-SPAR-IDARTNR                        
171600     MOVE ORAD-BEART            TO WS-SPAR-BEART                          
171700     MOVE ORAD-KVBEART          TO WS-SPAR-KVBEART                        
171800     MOVE ORAD-FLTILLK          TO WS-SPAR-FLTILLK                        
171900     MOVE ORAD-IDKUNDRF-RO      TO WS-SPAR-IDKUNDRF-RO                    
172000     MOVE ORAD-IDPURAD          TO WS-SPAR-IDPURAD                        
172100*                                                                         
172200     IF WS-KVLEVART             >   ORAD-KVAVBART                         
172300         MOVE FEL                 TO  WS-BEHANDLING-TEST                  
172400         MOVE ERR-TOO-LARGE-QUANT TO MED-IDMFSFEL                         
172500         PERFORM S03-WMEDKONV                                             
172600         MOVE 'PACKAD KVANTITET'  TO MED-TEMFSFEL (17:16)                 
172700     ELSE                                                                 
172800         MOVE WS-KVLEVART         TO SPAR-PRAD-KVLEVART                   
172900*                                                                         
173000*    OBS OBS OBS                                                          
173100*    ÖPPNA DENNA IF-SATS VID TESTER I BTS OCH                             
173200*    STÄNG MOTSV. I STYR-SECTION                                          
173300*   (BTS KLARAR EJ AV ROLL-BACK)                                          
173400*        PERFORM D-LAGG-UPP-KOLLI-SEG                                     
173500*    SLUT BTS-SPECIAL                                                     
173600         PERFORM S10-UPPD-SPAR-KOLLI                                      
173700     END-IF                                                               
173800     .                                                                    
173900     EJECT                                                                
174000 FBB-UPPDATERA-RAD         SECTION.                                       
174100                                                                          
174200     IF ORAD-KVLEVART + SPAR-PRAD-KVLEVART > ORAD-KVAVBART                
174300                                                                          
174400       MOVE FEL                  TO  WS-BEHANDLING-TEST                   
174500       MOVE ERR-TOO-LARGE-QUANT  TO MED-IDMFSFEL                          
174600       PERFORM S03-WMEDKONV                                               
174700       MOVE 'PACKAD KVANTITET'   TO MED-TEMFSFEL (17:16)                  
174800     ELSE                                                                 
174900       COMPUTE ORAD-KVLEVART = ORAD-KVLEVART                              
175000                             + SPAR-PRAD-KVLEVART                         
175100     END-IF                                                               
175200                                                                          
175300     IF WS-BEHANDLING-RATT                                                
175400       IF MID-KDARTURS (INX) NOT = SPACE                                  
175500         MOVE MID-KDARTURS (INX)   TO ORAD-KDARTURS                       
175600       END-IF                                                             
175700       IF ORAD-KVLEVART = ORAD-KVAVBART                                   
175800          MOVE +4    TO ORAD-KDRADSTA                                     
175900          ADD 1 TO WS-TOT-ANT-RADER                                       
176000          ADD 1 TO  WS-RINT-ANT-FPACK-ORAD                                
176100                                                                          
176200          IF KORD-KDORDKL = +0                                            
176300             PERFORM FBBB-UPPDATERA-VOR-TIKLAR                            
176400          END-IF                                                          
176500       END-IF                                                             
176600*                                                                         
176700       PERFORM IMS-REPL-BEHANDLAD-RAD                                     
176800       PERFORM FBBA-EV-SKAPA-RYK-TRANS                                    
176900     END-IF                                                               
177000     .                                                                    
177100     EJECT                                                                
177200 FBBA-EV-SKAPA-RYK-TRANS SECTION.                                         
177300                                                                          
177400     IF ORAD-IDKUNDRF-RO NOT = '00000     ' AND                           
177500        ORAD-TIRODAT         > ZERO                                       
177600                                                                          
177700       IF LOGG-IDLOGLOP = 9                                               
177800         MOVE ZERO                TO   LOGG-IDLOGLOP                      
177900       END-IF                                                             
178000                                                                          
178100       ACCEPT  LOGG-TIAAMMDD      FROM DATE                               
178200       ACCEPT  LOGG-TIKLOCK       FROM TIME                               
178300       ADD     +1                 TO   LOGG-IDLOGLOP                      
178400                                                                          
178500       MOVE    WS-IDDISTR-NUM     TO   RYK-IDDISTR                        
178600                                       W-WDQ2C-IDDISTR                    
178700       MOVE    WS-IDKUNDNR-NUM    TO   RYK-IDKUNDNR                       
178800                                       W-WDQ2C-IDKUNDNR                   
178900       MOVE    ORAD-IDKUNDRF-RO(1:5)                                      
179000                                  TO   W-WDQ2C-IDORDNR5                   
179100                                                                          
179200*****  FIX-START-DEL1 930303 FÖR ATT TA HAND OM EN ORDER SOM              
179300*      SAKNAR ORDERHUVUD (WDQ2) OBS, VID ANVÄNDANDE ÖPPNA OCKSÅ           
179400*      FIX-DEL2 I SECTION S13.                                            
179500       PERFORM IMS-GET-WDQ201-CSEQ                                        
179600       IF SEGMENT-FINNS                                                   
179700          MOVE OHUV-IDORDER       TO   RYK-IDORDER                        
179800       ELSE                                                               
179900          MOVE +0                 TO   RYK-IDORDER                        
180000       END-IF                                                             
180100*****  FIX-END-DEL1 930303                                                
180200       MOVE    'RYK'              TO   RYK-IDPTYP                         
180300                                       LOGG-IDPTYP                        
180400                                                                          
180500       MOVE    ORAD-IDARTNR       TO   RYK-IDARTNR                        
180600       MOVE    LOGG-TIAAMMDD      TO   RYK-TIRODAT                        
180700       MOVE    ORAD-KVLEVART      TO   RYK-KVLEVART                       
180800       MOVE    ORAD-KVBEART       TO   RYK-KVBEART-Q                      
180900       MOVE    ORAD-KDORDKL       TO   RYK-KDORDKL                        
181000       MOVE    ORAD-KDPRODSL      TO   RYK-KDPRODSL                       
181100       MOVE    ZERO               TO   RYK-KDORDBEK                       
181200                                                                          
181300       MOVE    SPACE              TO   LOGG-SORTPOST                      
181400       MOVE    RYK-WDGZRYK        TO   LOGG-LOGGPOST                      
181500                                                                          
181600       PERFORM IMS-ISRT-WDG601                                            
181700                                                                          
181800       PERFORM UNTIL SEGMENT-FINNS                                        
181900         IF LOGG-IDLOGLOP = 9                                             
182000           MOVE ZERO              TO LOGG-IDLOGLOP                        
182100           ACCEPT  LOGG-TIKLOCK   FROM TIME                               
182200         END-IF                                                           
182300         ADD +1                   TO LOGG-IDLOGLOP                        
182400         PERFORM IMS-ISRT-WDG601                                          
182500       END-PERFORM                                                        
182600     END-IF                                                               
182700     .                                                                    
182800     EJECT                                                                
182900 FBBB-UPPDATERA-VOR-TIKLAR SECTION.                                       
183000     SKIP3                                                                
183100                                                                          
183200     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
183300     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
183400     MOVE KORD-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
183500                                    W-A601KY-MAX-IDDISTR                  
183600     MOVE KORD-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
183700                                    W-A601KY-MAX-IDKUNDNR                 
183800     MOVE KORD-IDORDNR5          TO W-A601KY-MIN-IDORDNR                  
183900                                    W-A601KY-MAX-IDORDNR                  
184000                                                                          
184100     MOVE NEJ                    TO SW-TIKLAR-UPPDATERAD                  
184200                                                                          
184300     PERFORM IMS-GHN-WDA6B                                                
184400     PERFORM UNTIL SEGMENT-SAKNAS                                         
184500                OR END-OF-DATABASE                                        
184600                OR SW-TIKLAR-UPPDATERAD = JA                              
184700                                                                          
184800         IF  VOR-IDARTNR = ORAD-IDARTNR                                   
184900         AND VOR-TIKLAR = +0                                              
185000                                                                          
185100             MOVE WS-DAGENS-DATUM  TO VOR-TIKLAR                          
185200             MOVE WS-TIDPUNKT      TO WS-TIDPUNKT-RED                     
185300             MOVE WS-HHMMSS        TO VOR-TIKLATID                        
185400             PERFORM IMS-REPL-WDA6B                                       
185500             MOVE JA               TO SW-TIKLAR-UPPDATERAD                
185600         END-IF                                                           
185700                                                                          
185800         PERFORM IMS-GHN-WDA6B                                            
185900     END-PERFORM                                                          
186000     .                                                                    
186100     EJECT                                                                
186200 FBC-LAGG-UPP-KOLLI-KOPPL  SECTION.                                       
186300     SKIP3                                                                
186400     MOVE WS-IDPRODNR        TO KKOLLI-IDPRODNR                           
186500     MOVE WS-IDKOLLI-NUM     TO KKOLLI-IDKOLLI                            
186600     MOVE SPAR-PRAD-KVLEVART TO KKOLLI-KVLEVART                           
186700     PERFORM IMS-ISRT-KOLLI-KOPPL                                         
186800     IF SEGMENT-FINNS-REDAN                                               
186900         MOVE WS-IDPRODNR    TO  W-421-IDPRODNR                           
187000         MOVE WS-IDKOLLI-NUM TO  W-421-IDKOLLI                            
187100         PERFORM IMS-GHNP-KOLLI-KOPPL                                     
187200         ADD SPAR-PRAD-KVLEVART  TO KKOLLI-KVLEVART                       
187300         PERFORM IMS-REPL-KOLLI-KOPPL                                     
187400     ELSE                                                                 
187500         ADD +1                  TO ARB-KOLLI-KVORDRAD                    
187600                                                                          
187700         IF SPAR-PRAD-KDFARLIG = +4                                       
187800         OR SPAR-PRAD-KDFARLIG = +7                                       
187900             ADD +1              TO ARB-KOLLI-KVFALRAD                    
188000         END-IF                                                           
188100     END-IF                                                               
188200*                                                                         
188300     IF DCS-NDC-NA                                                        
188400        PERFORM S20-DATA-TILL-DEL-NOTE                                    
188500     END-IF                                                               
188600*                                                                         
188700     IF SPAR-IDPSN > ZERO                                                 
188800       PERFORM FBCA-SPARA-FG-DATA                                         
188900     END-IF                                                               
189000     .                                                                    
189100     EJECT                                                                
189200 FBCA-SPARA-FG-DATA SECTION.                                              
189300     SKIP3                                                                
189400     MOVE +1 TO FG-INDX                                                   
189500     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
189600                                                                          
189700       IF TAB-IDPSN(FG-INDX) = ZERO                                       
189800         MOVE SPAR-IDPSN TO TAB-IDPSN(FG-INDX)                            
189900         PERFORM S17-BERAEKNA-FG-FAELT                                    
190000                                                                          
190100       ELSE                                                               
190200         IF SPAR-IDPSN = TAB-IDPSN(FG-INDX)                               
190300           PERFORM S17-BERAEKNA-FG-FAELT                                  
190400         END-IF                                                           
190500       END-IF                                                             
190600                                                                          
190700       ADD +1 TO FG-INDX                                                  
190800     END-PERFORM                                                          
190900                                                                          
191000     COMPUTE TOTAL-SUEQFG = TOTAL-SUEQFG      +                           
191100                            (SPAR-SUEQFG      *                           
191200                             KKOLLI-KVLEVART)                             
191300     .                                                                    
191400     EJECT                                                                
191500 FD-UPPDATERA-PRODTAB      SECTION.                                       
191600     SKIP3                                                                
191700                                                                          
191800     MOVE WS-IDPRODNR            TO W-420-IDPRODNR                        
191900     PERFORM IMS-GU-WDE42-KORD-BSEQ                                       
192000                                                                          
192100     MOVE KORD-IDORDER           TO W-301-IDORDER                         
192200     MOVE KORD-IDDC              TO W-301-IDDC                            
192300     MOVE KORD-IDPRODNR          TO W-301-IDPRODNR                        
192400*    -- IDPLKLST FRÅN INDATA                                              
192500     MOVE CARR-IDPLKLST          TO W-301-IDPLKLST                        
192600     PERFORM IMS-GU-WDQ301                                                
192700     IF  SEGMENT-FINNS                                                    
192800       MOVE ODEL-IDTRP           TO WS-ODEL-IDTRP                         
192900     END-IF                                                               
193000                                                                          
193100     IF  WS-RINT-ANT-FPACK-ORAD > ZERO                                    
193200*      * RAD-INTERVALLET HAR FÄRDIGPACKADE ORADER                         
193300                                                                          
193400       PERFORM FDA-LAES-SHIFTTAB                                          
193500                                                                          
193600       IF  ODEL-KDPRODKL = 'B'                                            
193700       OR  ODEL-KDPRODKL = 'C'                                            
193800*        * PRODTAB UPPDATERAS ENDAST FÖR PRODKL B OCH C.                  
193900                                                                          
194000         MOVE KORD-IDDC          TO W-4471-IDDC                           
194100         MOVE ODEL-IDPRCBAS      TO W-4471-IDPRCBAS                       
194200         MOVE ODEL-IDPRCVAR      TO W-4471-IDPRCVAR                       
194300         PERFORM IMS-GHU-447211                                           
194400                                                                          
194500         IF  SEGMENT-FINNS                                                
194600*          * PRODTAB UPPDATERAS ENDAST OM ORDERDELENS PRC FINNS.          
194700                                                                          
194800           MOVE 1                TO IND1                                  
194900           MOVE W-4478-IDSHIFT   TO IND2                                  
195000           MOVE ODEL-DARFS       TO HJALP-ODEL-DARFS                      
195100           MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                     
195200                                                                          
195300           PERFORM UNTIL IND1 = 30 OR                                     
195400                         4472-TIRFS (IND1) = ZERO OR                      
195500                         HJALP-ODEL-DARFS-6 = HJALP-4472-TIRFS-6          
195600             ADD 1                TO IND1                                 
195700             MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                   
195800           END-PERFORM                                                    
195900                                                                          
196000           MOVE ODEL-DARFS (3:10) TO 4472-TIRFS (IND1)                    
196100           MOVE W-4478-IDSHIFT  TO 4472-IDSHIFT (IND1, IND2)              
196200           ADD WS-RINT-ANT-FPACK-ORAD                                     
196300                                TO 4472-KVRADER-PRAPP (IND1, IND2)        
196400           PERFORM FDB-ADDERA-TOTAL-PRODTID                               
196500           PERFORM IMS-REPL-447211                                        
196600         END-IF                                                           
196700       END-IF                                                             
196800     END-IF                                                               
196900     .                                                                    
197000     EJECT                                                                
197100 FDA-LAES-SHIFTTAB         SECTION.                                       
197200*                                                                         
197300     MOVE KORD-IDDC         TO W-4477-IDDC                                
197400     MOVE '1'               TO W-4478-IDSHIFT                             
197500     MOVE KORD-IDUSER       TO W-4478-IDUSER                              
197600     PERFORM IMS-GU-4478                                                  
197700*                                                                         
197800     IF SEGMENT-SAKNAS                                                    
197900        MOVE '2'            TO W-4478-IDSHIFT                             
198000        PERFORM IMS-GU-4478                                               
198100*                                                                         
198200        IF SEGMENT-SAKNAS                                                 
198300           MOVE '3'         TO W-4478-IDSHIFT                             
198400           PERFORM IMS-GU-4478                                            
198500*                                                                         
198600           IF SEGMENT-SAKNAS                                              
198700              MOVE '1'      TO W-4478-IDSHIFT                             
198800           END-IF                                                         
198900        END-IF                                                            
199000     END-IF                                                               
199100     .                                                                    
199200     EJECT                                                                
199300 FDB-ADDERA-TOTAL-PRODTID             SECTION.                            
199400                                                                          
199500     MOVE 4472-SUPTID-PRAPP (IND1, IND2) TO WS-SUPTID-PRAPP               
199600                                                                          
199700     COMPUTE WS-KVPTID-MIN ROUNDED = WS-RINT-ANT-FPACK-ORAD *             
199800                                     ODEL-KVPTID                          
199900     END-COMPUTE                                                          
200000                                                                          
200100     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
200200     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
200300     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
200400                            (WS-KVPTID-TIM * 60)                          
200500                                                                          
200600     ADD  WS-SUPTID-MIN                  TO WS-KVPTID-MIN                 
200700     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
200800     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
200900     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
201000                            (WS-KVPTID-TIM * 60)                          
201100     MOVE WS-KVPTID-MIN                  TO WS-SUPTID-MIN                 
201200                                                                          
201300     MOVE WS-SUPTID-PRAPP TO 4472-SUPTID-PRAPP (IND1, IND2)               
201400     .                                                                    
201500     EJECT                                                                
201600 G-UPPDATERA-KOLLIREG      SECTION.                                       
201700     MOVE 'G-UP'   TO WPOS                                                
201800     PERFORM IMS-GHU-KOLLIREG                                             
201900     MOVE VORD-IDDISTR       TO  TEST-IDDISTR                             
202000                                 DIS128-IDDISTR                           
202100     MOVE VORD-KDORDKL       TO  WS-KDORDKL                               
202200     MOVE VORD-FLAUTFAK      TO  WS-FLAUTFAK                              
202300     MOVE VORD-DARFS         TO  WS-DARFS                                 
202400     IF VORD-KDORDSTA        =   1                                        
202500         MOVE 2              TO  VORD-KDORDSTA                            
202600     END-IF                                                               
202700     COMPUTE VORD-KVKOLPAC = VORD-KVKOLPAC + 1                            
202800* FIX-START FÖR ATT TA HAND OM EN W4T315-TRANS SOM GÅTT FRÅN              
202900* 0605 EFTER DET ATT R4T397/98-TRANS HAR GÅTT.                            
203000*    IF NOT VORD-IDPRODNR = 0591848                                       
203100     COMPUTE VORD-KVORDRAD-PACK                                           
203200                         = VORD-KVORDRAD-PACK + WS-TOT-ANT-RADER          
203300*    END-IF                                                               
203400* FIX-SLUT                                                                
203500                                                                          
203600     ADD 1                        TO  VORD-KVKOLLI                        
203700     MOVE WS-DAGENS-DATUM         TO  VORD-TIPACKN-SK                     
203800                                                                          
203900*    COMPUTE VORD-VKORDBTO ROUNDED                                        
204000     COMPUTE VORD-VKORDBTO                                                
204100           = VORD-VKORDBTO +                                              
204200             ARB-KOLLI-VKORDNTO * 1                                       
204300     END-COMPUTE                                                          
204400* FOR DIST03-SVERIGE GROSS WEIGHT WILL BE REPORTED TO                     
204500*    IF NOT DIST03-SVERIGE                                                
204600*      COMPUTE WS-EMB-VKTARA-TOT-ORDER        ROUNDED                     
204700       COMPUTE WS-EMB-VKTARA-TOT-ORDER                                    
204800             = WS-EMB-VKTARA-ONE-CASE * 1                                 
204900       END-COMPUTE                                                        
205000       ADD WS-EMB-VKTARA-TOT-ORDER        TO VORD-VKORDBTO                
205100*    END-IF                                                               
205200                                                                          
205300     COMPUTE VORD-VLORDBTO        ROUNDED                                 
205400           = VORD-VLORDBTO + WS-VLORDBTO * 1                              
205500     END-COMPUTE                                                          
205600                                                                          
205700     COMPUTE VORD-SUORDV-PACK-LOC ROUNDED                                 
205800           = VORD-SUORDV-PACK-LOC + ARB-KOLLI-SUORDV-LOC                  
205900                                     * 1                                  
206000     END-COMPUTE                                                          
206100                                                                          
206200     COMPUTE VORD-SUORDV-PACK-LOCPREL ROUNDED                             
206300           = VORD-SUORDV-PACK-LOCPREL +                                   
206400           ARB-KOLLI-SUORDV-LOCPREL * 1                                   
206500     END-COMPUTE                                                          
206600                                                                          
206700     COMPUTE VORD-SUORDV-PACK ROUNDED                                     
206800         = VORD-SUORDV-PACK + ARB-KOLLI-SUORDV                            
206900                             * 1                                          
207000     END-COMPUTE                                                          
207100     MOVE ARB-KOLLI-KDVALISO        TO VORD-KDVALISO                      
207200     MOVE ARB-KOLLI-KDVALISO-EXP    TO VORD-KDVALISO-EXP                  
207300                                                                          
207400      IF    VORD-KDORDSTA < +3                                            
207500      AND VORD-KVKOLLI    > +0                                            
207600      AND (DIST03-SVERIGE OR DIST35-CDC-LDC-REFILL)                       
207700      AND (DCS-CDC OR (DCS-SDC AND DCS-IDLANDX2 = 'SE'))                  
207800                                                                          
207900        MOVE VORD-KDFRAKT     TO WS-KDFRAKT                               
208000        MOVE WS-KDFRAKT       TO FRAK01-KDFRAKT                           
208100        MOVE WS-IDKOLLI-NUM   TO WS-IDKOLLI-LR                            
208200                                                                          
208300        IF    FRAK01-SVERIGE2                                             
208400        OR    FRAK01-NORDEN                                               
208500        OR    FRAK01-KDFRAKT21                                            
208600        OR    FRAK01-KDFRAKT62                                            
208700        OR (WS-IDKOLLI-LR > 149 AND WS-IDKOLLI-LR < 200)                  
208800        OR (WS-IDKOLLI-LR > 349 AND WS-IDKOLLI-LR < 400)                  
208900                                                                          
209000           IF     VORD-KDFRAKT  NOT = +17                                 
209100             IF NOT DIS128-FRAKTS                                         
209200               MOVE JA            TO VORD-FLFRAKTS                        
209300             END-IF                                                       
209400           END-IF                                                         
209500         END-IF                                                           
209600      END-IF                                                              
209700                                                                          
209800     PERFORM IMS-REPL-KOLLIREG                                            
209900     SKIP2                                                                
210000     PERFORM GB-BEH-ENKELT-KOLLI                                          
210100     .                                                                    
210200     EJECT                                                                
210300 GB-BEH-ENKELT-KOLLI      SECTION.                                        
210400     MOVE 'GB  '   TO WPOS                                                
210500     MOVE WS-IDKOLLI-NUM          TO  W-611-IDKOLLI                       
210600     PERFORM IMS-GHU-KOLLI                                                
210700     PERFORM S11-UPPD-KOLLI-FRAN-ARB                                      
210800     IF KOLLI-KDKOLSTA            =   ZERO                                
210900         MOVE 1                   TO KOLLI-KDKOLSTA                       
211000         MOVE WS-DAGENS-DATUM     TO KOLLI-TIPACKN                        
211100         MOVE WS-TIDPUNKT         TO WS-TIDPUNKT-RED                      
211200         MOVE WS-HHMMSS           TO KOLLI-TIPACTID                       
211300         MOVE WS-DARFS            TO KOLLI-DARFS                          
211400         PERFORM S12-SKAPA-4322                                           
211500         PERFORM S13-UPPDAT-KDORDSTA                                      
211600     END-IF                                                               
211700*                                                                         
211800     PERFORM S16-UPPD-FARLIGT-GODS-DATA                                   
211900*                                                                         
212000     PERFORM IMS-REPL-KOLLI                                               
212100     .                                                                    
212200     EJECT                                                                
212300 K-UPPDAT-4726-4727 SECTION.                                              
212400      MOVE '4726'  TO WPOS                                                
212500                                                                          
212600       MOVE '4726'                  TO W-4726-IDHTYP                      
212700       MOVE NEJ                     TO W-4726-FLBATCH                     
212800       MOVE LOW-VALUE               TO W-4726-LOWVALUE                    
212900                                                                          
213000       PERFORM IMS-GU-4726-ROT-KVAL                                       
213100                                                                          
213200       MOVE WS-IDDISTR-NUM          TO W-4726-IDDISTR                     
213300       MOVE WS-IDKUNDNR-NUM         TO W-4726-IDKUNDNR                    
213400       MOVE WS-IDDC                 TO W-4726-IDDC                        
213500       MOVE WS-KDFAKTYP             TO W-4726-KDFAKTYP                    
213600                                                                          
213700       PERFORM IMS-GNP-4726-UNDERSEG-KVAL                                 
213800                                                                          
213900       IF  SEGMENT-SAKNAS                                                 
214000           MOVE WS-IDDISTR-NUM      TO AUTFAKT-IDDISTR                    
214100           MOVE WS-IDKUNDNR-NUM     TO AUTFAKT-IDKUNDNR                   
214200           MOVE WS-IDDC             TO AUTFAKT-IDDC                       
214300           MOVE WS-KDFAKTYP         TO AUTFAKT-KDFAKTYP                   
214400                                                                          
214500           PERFORM IMS-INSERT-4726-UNDERSEG                               
214600       END-IF                                                             
214700       MOVE WS-IDPRODNR             TO AUTFAKT-IDPRODNR                   
214800       MOVE ZERO                    TO AUTFAKT-IDSKEPPN                   
214900                                       AUTFAKT-PRFRAKT                    
215000                                                                          
215100       IF  DIST03-SVERIGE-2                                               
215200           MOVE NEJ                 TO AUTFAKT-FLLASTA                    
215300       ELSE                                                               
215400           MOVE JA                  TO AUTFAKT-FLLASTA                    
215500       END-IF                                                             
215600                                                                          
215700       PERFORM IMS-INSERT-4727                                            
215800     .                                                                    
215900     EJECT                                                                
216000 M-SEND-SVAR  SECTION.                                                    
216100     MOVE 'MSE '   TO WPOS                                                
216200     PERFORM S02-SEND-OPEN                                                
216300                                                                          
216400     MOVE REQU-IDMSG3IV         TO RESP-IDMSG3IV                          
216500     MOVE 'ResponseCarrierFinalization'  TO RESP-IDMTYP3IV                
216600     MOVE FUNCTION CURRENT-DATE(1:16)    TO RESP-TISTAMP3IV               
216700     MOVE '0'        TO RESP-TISTAMP3IV(17:1)                             
216800     MOVE REQU-IDDC             TO RESP-IDDC                              
216900     MOVE REQU-IDANSTNR         TO RESP-IDANSTNR                          
217000     MOVE REQU-IDSNO3IV         TO RESP-IDSNO3IV                          
217100     IF MED-IDMFSFEL = SPACE                                              
217200       MOVE ZERO                TO RESP-KDRESP3IV                         
217300       MOVE SPACE               TO RESP-BERESP3IV                         
217400     ELSE                                                                 
217500       MOVE MED-IDMFSFEL        TO RESP-KDRESP3IV                         
217600       MOVE MED-TEMFSFEL        TO RESP-BERESP3IV                         
217700     END-IF                                                               
217800     PERFORM S02-SEND-RESPONSE                                            
217900*                                                                         
218000     IF MED-IDMFSFEL = SPACE                                              
218100      MOVE 'CarrierResult'       TO CRES-IDRTYP3IV                        
218200      MOVE WS-IDPRODNR           TO CRES-IDPRODNR                         
218300      MOVE WS-IDPLKLST           TO CRES-IDPLKLST                         
218400      MOVE WS-IDLOPNR-ORD        TO CRES-IDLOPNR-ORD                      
218500      IF WS-INDATA-RATT AND WS-BEHANDLING-RATT                            
218600        MOVE WS-IDKOLLI-NUM      TO CRES-IDKOLLI                          
218700      ELSE                                                                
218800        MOVE ZERO                TO CRES-IDKOLLI                          
218900      END-IF                                                              
219000*    - ADFLLOC NOT USED AT THE MOMENT                                     
219100      MOVE SPACE                 TO CRES-ADFLLOC                          
219200                                                                          
219300      PERFORM S02-SEND-RESPONSE2                                          
219400     END-IF                                                               
219500*                                                                         
219600     PERFORM S02-SEND-CLOSE                                               
219700     .                                                                    
219800     EJECT                                                                
219900 N-EV-STARTA-4397                       SECTION.                          
220000     MOVE 'N   '   TO WPOS                                                
220100     IF WS-FLSISTAK = JA                                                  
220200       MOVE WS-IDDISTR-NUM   TO W-401-IDDISTR                             
220300       MOVE WS-IDKUNDNR-NUM  TO W-401-IDKUNDNR                            
220400       MOVE WS-IDORDNR       TO W-401-IDORDNR                             
220500       MOVE WS-IDPRODNR      TO W-401-IDPRODNR                            
220600       MOVE WS-IDPLKLST      TO W-401-IDPLKLST                            
220700       MOVE NEJ              TO WS-FLPAFEL                                
220800       PERFORM IMS-GHU-WDE401                                             
220900                                                                          
221000        IF (KORD-KDPAKOLL = 0 OR 1) AND AVRAPP-UTAN-KOLLI AND             
221100           ( (KORD-KVORDRAD-PACK  = KORD-KVORDRAD)    OR                  
221200            (KORD-KVORDRAD-LEVPL    > 0               AND                 
221300             KORD-KVORDRAD-PACK  = KORD-KVORDRAD-LEVPL) )                 
221400          MOVE FEL                   TO  WS-BEHANDLING-TEST               
221500          MOVE ERR-ORDER-PART-READY  TO  MED-IDMFSFEL                     
221600          PERFORM S03-WMEDKONV                                            
221700        ELSE                                                              
221800         PERFORM S22-KOLLA-RADER                                          
221900         IF WS-FLPAFEL = NEJ                                              
222000           PERFORM IMS-GHU-WDE401                                         
222100           MOVE +2                TO KORD-KDPAKOLL                        
222200           PERFORM IMS-REPL-WDE401                                        
222300           PERFORM NA-STARTA-4397                                         
222400         ELSE                                                             
222500           MOVE FEL                   TO  WS-BEHANDLING-TEST              
222600           MOVE ERR-EJ-AVSLUT         TO  MED-IDMFSFEL                    
222700           PERFORM S03-WMEDKONV                                           
222800         END-IF                                                           
222900        END-IF                                                            
223000     END-IF                                                               
223100     .                                                                    
223200     EJECT                                                                
223300 NA-STARTA-4397  SECTION.                                                 
223400     MOVE 'NA  '   TO WPOS                                                
223500     MOVE 'W4T397X '              TO  ALT-TRANSKOD                        
223600                                      ALT-LTERM-NAME                      
223700     MOVE WS-KDMFSFOR             TO  ALT-KDMFSFOR                        
223800     MOVE '431B'                  TO  ALT-IDTRANS                         
223900     MOVE +117                    TO  ALT-LL                              
224000     MOVE WS-IDPRODNR             TO  4397-IDPRODNR                       
224100*     KAN VI HÄMTA RÄTT ANSTNR???                                         
224200     MOVE KORD-IDUSER             TO  4397-IDANSTNR                       
224300     MOVE WS-IDPLKLST             TO  4397-IDPLKLST                       
224400     MOVE ZERO                    TO  4397-IDPURAD                        
224500     MOVE 4397-TRANSAREA          TO  ALT-AREA                            
224600     PERFORM IMS-INSERT-ALTMSG                                            
224700*    MOVE JA                      TO WS-4397-STARTAD-SW                   
224800     .                                                                    
224900     EJECT                                                                
225000 X-LAES-RAETT-ORDERDEL  SECTION.                                          
225100                                                                          
225200*    -- LÄS FÖRSTA BÄSTA ORDERDEL                                         
225300     MOVE WS-IDPRODNR        TO W-IDPRODNR-E4E                            
225400     PERFORM IMS-GU-SEQE-WDE401                                           
225500     IF SEGMENT-FINNS                                                     
225600*      -- LÄS RÄTT ORDERDEL                                               
225700       MOVE KORD-IDDISTR           TO W-401-IDDISTR                       
225800       MOVE KORD-IDKUNDNR          TO W-401-IDKUNDNR                      
225900       MOVE KORD-IDORDNR5          TO W-401-IDORDNR                       
226000       MOVE KORD-IDPRODNR          TO W-401-IDPRODNR                      
226100*      -- IDPLKLST FRÅN INDATA, INTE LÄST ORDERSEGMENT                    
226200       MOVE CARR-IDPLKLST          TO W-401-IDPLKLST                      
226300       PERFORM IMS-GU-WDE401                                              
226400       IF SEGMENT-FINNS                                                   
226500         MOVE KORD-IDDISTR         TO WS-IDDISTR-NUM                      
226600         MOVE KORD-IDKUNDNR        TO WS-IDKUNDNR-NUM                     
226700         MOVE KORD-IDKUNDRF        TO WS-IDKUNDRF                         
226800         MOVE KORD-KDFRAKT         TO PLATS-KDFRAKT                       
226900         MOVE KORD-KDORDKL         TO PLATS-KDORDKLX                      
227000                                      WS-KDORDKL                          
227100         MOVE KORD-IDORDER         TO WS-KORD-IDORDER                     
227200                                                                          
227300       ELSE                                                               
227400         MOVE FEL                  TO WS-INDATA-TEST                      
227500         MOVE ERR-PRODNR-MISSING   TO MED-IDMFSFEL                        
227600         PERFORM S03-WMEDKONV                                             
227700       END-IF                                                             
227800     ELSE                                                                 
227900       MOVE FEL                  TO  WS-INDATA-TEST                       
228000       MOVE ERR-PRODNR-MISSING   TO MED-IDMFSFEL                          
228100       PERFORM S03-WMEDKONV                                               
228200     END-IF                                                               
228300     .                                                                    
228400     EJECT                                                                
228500                                                                          
228600 S01-RECEIVE-OPEN  SECTION.                                               
228700     MOVE 'OPEN'                    TO RECV-KDFUNC                        
228800     MOVE 'CARPARTS.3IV2.REQUCARRIERFINALIZATION'                         
228900     TO RECV-ADDISPABS                                                    
229000     MOVE 'A2  '        TO WPOS                                           
229100     CALL WZ01RECV USING  RECV-CONTROL-AREA                               
229200                          RECV-OPEN-AREA                                  
229300     IF RECV-KDRC > 0                                                     
229400       MOVE RECV-KDRC                TO KDRC-DISP                         
229500       STRING 'WZ01RECV OPEN ERROR RC='  KDRC-DISPLAY                     
229600       DELIMITED BY SIZE INTO ERRORTEXT                                   
229700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
229800     END-IF                                                               
229900     .                                                                    
230000     EJECT                                                                
230100 S01-RECEIVE-MESSAGE  SECTION.                                            
230200     MOVE 'GET'                     TO RECV-KDFUNC                        
230300     MOVE LENGTH OF RECV-AREA       TO RECV-KVDLEN                        
230400     CALL WZ01RECV USING  RECV-CONTROL-AREA RECV-KVDLEN                   
230500                          RECV-AREA                                       
230600     IF RECV-KDRC > 1                                                     
230700       MOVE RECV-KDRC                TO KDRC-DISP                         
230800       STRING 'WZ01RECV GET ERROR RC='  KDRC-DISPLAY                      
230900       DELIMITED BY SIZE INTO ERRORTEXT                                   
231000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
231100     END-IF                                                               
231200     MOVE 'A4  '        TO WPOS                                           
231300     UNSTRING RECV-AREA (1:RECV-KVDLEN)                                   
231400       DELIMITED BY VBAR                                                  
231500       INTO REQU-IDMSG3IV    COUNT IN LIDMSG3IV                           
231600            REQU-IDMVER3IV   COUNT IN LIDMVER3IV                          
231700            REQU-IDMTYP3IV   COUNT IN LIDMTYP3IV                          
231800            REQU-TISTAMP3IV  COUNT IN LTISTAMP3IV                         
231900            REQU-IDDC        COUNT IN LIDDC                               
232000            REQU-IDANSTNR    COUNT IN LIDANSTNR                           
232100            REQU-IDSNO3IV    COUNT IN LIDSNO3IV                           
232200       OVERFLOW                                                           
232300         MOVE FEL                         TO WS-INDATA-TEST               
232400         MOVE ERR-INV-MSG-STRUCTURE       TO MED-IDMFSFEL                 
232500         PERFORM S03-WMEDKONV                                             
232600     END-UNSTRING                                                         
232700     MOVE 'A5  '        TO WPOS                                           
232800     IF LIDMSG3IV > LENGTH OF REQU-IDMSG3IV                               
232900*      MESSAGE TYPE VALUE IS TOO LONG                                     
233000       MOVE FEL                           TO WS-INDATA-TEST               
233100       MOVE ERR-VALUE-TOO-LONG            TO MED-IDMFSFEL                 
233200       PERFORM S03-WMEDKONV                                               
233300       MOVE 'MEDDELANDE-ID'          TO MED-TEMFSFEL (16:13)              
233400     END-IF                                                               
233500     IF REQU-IDMTYP3IV NOT = 'RequestCarrierFinalization'                 
233600     MOVE 'A7  '        TO WPOS                                           
233700       MOVE FEL                           TO WS-INDATA-TEST               
233800       MOVE ERR-INV-MSG-STRUCTURE         TO MED-IDMFSFEL                 
233900       PERFORM S03-WMEDKONV                                               
234000     END-IF                                                               
234100                                                                          
234200     .                                                                    
234300     EJECT                                                                
234400 S01-RECEIVE-MESSAGE2 SECTION.                                            
234500     MOVE 'GET'                     TO RECV-KDFUNC                        
234600     MOVE LENGTH OF RECV-AREA       TO RECV-KVDLEN                        
234700     MOVE 'A32 '        TO WPOS                                           
234800     MOVE SPACE                     TO RECV-AREA                          
234900     CALL WZ01RECV USING  RECV-CONTROL-AREA RECV-KVDLEN                   
235000                          RECV-AREA                                       
235100     IF RECV-KDRC > 1                                                     
235200       MOVE RECV-KDRC                TO KDRC-DISP                         
235300       STRING 'WZ01RECV GET ERROR RC='  KDRC-DISPLAY                      
235400       DELIMITED BY SIZE INTO ERRORTEXT                                   
235500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
235600     END-IF                                                               
235700     MOVE 'A42 '        TO WPOS                                           
235800                                                                          
235900      UNSTRING RECV-AREA (1:RECV-KVDLEN)                                  
236000       DELIMITED BY VBAR                                                  
236100       INTO CARR-IDRTYP3IV   COUNT IN LIDRTYP3IV                          
236200            CARR-IDPRODNR    COUNT IN LIDPRODNR                           
236300            CARR-IDPLKLST    COUNT IN LIDPLKLST                           
236400            CARR-IDPRC       COUNT IN LIDPRC                              
236500            CARR-IDLOPNR-ORD COUNT IN LIDLOPNR-ORD                        
236600            CARR-FLSISTAK    COUNT IN LFLSISTAK                           
236700            CARR-KDEMBTYP    COUNT IN LKDEMBTYP                           
236800            CARR-KDKOLLI     COUNT IN LKDKOLLI                            
236900            CARR-DIKOLLIH    COUNT IN LDIKOLLIH                           
237000            CARR-DIKOLLIL    COUNT IN LDIKOLLIL                           
237100            CARR-DIKOLLIB    COUNT IN LDIKOLLIB                           
237200       OVERFLOW                                                           
237300         MOVE 'TOO MANY FIELDS IN REQUEST MESSAGE2'                       
237400             TO ERRORTEXT                                                 
237500         MOVE FEL                         TO WS-INDATA-TEST               
237600         MOVE ERR-INV-MSG-STRUCTURE       TO MED-IDMFSFEL                 
237700         PERFORM S03-WMEDKONV                                             
237800      END-UNSTRING                                                        
237900     MOVE 'A82 '        TO WPOS                                           
238000     IF LIDRTYP3IV > LENGTH OF CARR-IDRTYP3IV                             
238100*      MESSAGE TYPE VALUE IS TOO LONG                                     
238200       MOVE FEL                           TO WS-INDATA-TEST               
238300       MOVE ERR-VALUE-TOO-LONG            TO MED-IDMFSFEL                 
238400       PERFORM S03-WMEDKONV                                               
238500       MOVE 'MESSAGERECORDTYPE'      TO MED-TEMFSFEL (16:17)              
238600     END-IF                                                               
238700     MOVE 'A6  '        TO WPOS                                           
238800      IF LIDPLKLST > LENGTH OF CARR-IDPLKLST                              
238900*       PICKING LIST VALUE IS TOO LONG                                    
239000       MOVE FEL                           TO WS-INDATA-TEST               
239100       MOVE ERR-VALUE-TOO-LONG            TO MED-IDMFSFEL                 
239200       PERFORM S03-WMEDKONV                                               
239300       MOVE 'PLOCKLISTENUMMER'         TO MED-TEMFSFEL (16:16)            
239400      END-IF                                                              
239500      IF CARR-IDPLKLST NOT NUMERIC                                        
239600*       EJ NUMERISK PLKLST                                                
239700        MOVE FEL                  TO WS-INDATA-TEST                       
239800        MOVE ERR-INVALID-VALUE    TO MED-IDMFSFEL                         
239900        PERFORM S03-WMEDKONV                                              
240000        MOVE 'PLOCKLISTENUMMER'   TO MED-TEMFSFEL(17:16)                  
240100      END-IF                                                              
240200     MOVE 'A9  '        TO WPOS                                           
240300      IF LIDPRODNR > LENGTH OF CARR-IDPRODNR                              
240400*       PRODUCTION NUMBER VALUE IS TOO LONG                               
240500       MOVE FEL                           TO WS-INDATA-TEST               
240600       MOVE ERR-VALUE-TOO-LONG            TO MED-IDMFSFEL                 
240700       PERFORM S03-WMEDKONV                                               
240800       MOVE 'PRODUKTIONSNUMMER'        TO MED-TEMFSFEL (16:17)            
240900      END-IF                                                              
241000      IF LIDPRC > LENGTH OF CARR-IDPRC                                    
241100*       PRODUCTION CHANNEL VALUE IS TOO LONG                              
241200       MOVE FEL                           TO WS-INDATA-TEST               
241300       MOVE ERR-VALUE-TOO-LONG            TO MED-IDMFSFEL                 
241400       PERFORM S03-WMEDKONV                                               
241500       MOVE 'PRODUKTIONSKANAL'         TO MED-TEMFSFEL (16:16)            
241600      END-IF                                                              
241700      IF LIDLOPNR-ORD > LENGTH OF CARR-IDLOPNR-ORD                        
241800*       ORDER SEQ NUMBER VALUE IS TOO LONG                                
241900       MOVE FEL                           TO WS-INDATA-TEST               
242000       MOVE ERR-VALUE-TOO-LONG            TO MED-IDMFSFEL                 
242100       PERFORM S03-WMEDKONV                                               
242200       MOVE 'LÖPNUMMER FÖR ORDERDEL'   TO MED-TEMFSFEL (16:23)            
242300      END-IF                                                              
242400      IF LFLSISTAK > LENGTH OF CARR-FLSISTAK                              
242500*       FLAG LAST CASE VALUE IS TOO LONG                                  
242600       MOVE FEL                           TO WS-INDATA-TEST               
242700       MOVE ERR-VALUE-TOO-LONG            TO MED-IDMFSFEL                 
242800       PERFORM S03-WMEDKONV                                               
242900       MOVE 'SISTA-KOLLI-MARKERING'    TO MED-TEMFSFEL (16:21)            
243000      END-IF                                                              
243100      IF LKDEMBTYP > LENGTH OF CARR-KDEMBTYP                              
243200*       CARRIER TYPE VALUE IS TOO LONG                                    
243300       MOVE FEL                           TO WS-INDATA-TEST               
243400       MOVE ERR-VALUE-TOO-LONG            TO MED-IDMFSFEL                 
243500       PERFORM S03-WMEDKONV                                               
243600       MOVE 'EMBALLAGEKOD'             TO MED-TEMFSFEL (16:12)            
243700      END-IF                                                              
243800      IF LKDKOLLI > LENGTH OF CARR-KDKOLLI                                
243900*       CARRIER TYPE CODE  VALUE IS TOO LONG                              
244000       MOVE FEL                           TO WS-INDATA-TEST               
244100       MOVE ERR-VALUE-TOO-LONG            TO MED-IDMFSFEL                 
244200       PERFORM S03-WMEDKONV                                               
244300       MOVE 'KOLLIKOD'                TO MED-TEMFSFEL (16:8)              
244400      END-IF                                                              
244500                                                                          
244600      IF LDIKOLLIH > LENGTH OF CARR-DIKOLLIH                              
244700*       HEIGHT VALUE IS TOO LONG                                          
244800       MOVE FEL                           TO WS-INDATA-TEST               
244900       MOVE ERR-VALUE-TOO-LONG            TO MED-IDMFSFEL                 
245000       PERFORM S03-WMEDKONV                                               
245100       MOVE 'KOLLITS HÖJD'            TO MED-TEMFSFEL (16:12)             
245200      END-IF                                                              
245300      IF CARR-DIKOLLIH NOT NUMERIC                                        
245400*       HEIGTH VALUE IS A MESS                                            
245500       MOVE FEL                           TO WS-INDATA-TEST               
245600       MOVE ERR-INVALID-VALUE             TO MED-IDMFSFEL                 
245700       PERFORM S03-WMEDKONV                                               
245800       MOVE 'KOLLITS HÖJD'            TO MED-TEMFSFEL (17:12)             
245900      END-IF                                                              
246000                                                                          
246100      IF LDIKOLLIL > LENGTH OF CARR-DIKOLLIL                              
246200*       LENGTH VALUE IS TOO LONG                                          
246300       MOVE FEL                           TO WS-INDATA-TEST               
246400       MOVE ERR-VALUE-TOO-LONG            TO MED-IDMFSFEL                 
246500       PERFORM S03-WMEDKONV                                               
246600       MOVE 'KOLLITS LÄNGD'           TO MED-TEMFSFEL (16:13)             
246700      END-IF                                                              
246800      IF CARR-DIKOLLIL NOT NUMERIC                                        
246900*       LENGTH VALUE IS A MESS                                            
247000       MOVE FEL                           TO WS-INDATA-TEST               
247100       MOVE ERR-INVALID-VALUE             TO MED-IDMFSFEL                 
247200       PERFORM S03-WMEDKONV                                               
247300       MOVE 'KOLLITS LÄNGD'           TO MED-TEMFSFEL (17:13)             
247400      END-IF                                                              
247500                                                                          
247600      IF LDIKOLLIB > LENGTH OF CARR-DIKOLLIB                              
247700*       WIDTH VALUE IS TOO LONG                                           
247800       MOVE FEL                           TO WS-INDATA-TEST               
247900       MOVE ERR-VALUE-TOO-LONG            TO MED-IDMFSFEL                 
248000       PERFORM S03-WMEDKONV                                               
248100       MOVE 'KOLLITS BREDD'           TO MED-TEMFSFEL (16:13)             
248200      END-IF                                                              
248300      IF LDIKOLLIB = 0                                                    
248400*       -- UNSTRING DOES NOT MOVE LAST FIELD IF EMPTY                     
248500        MOVE ZERO TO CARR-DIKOLLIB                                        
248600      END-IF                                                              
248700      IF CARR-DIKOLLIB NOT NUMERIC                                        
248800*       WIDTH VALUE IS A MESS                                             
248900       MOVE FEL                           TO WS-INDATA-TEST               
249000       MOVE ERR-INVALID-VALUE             TO MED-IDMFSFEL                 
249100       PERFORM S03-WMEDKONV                                               
249200       MOVE 'KOLLITS BREDD'          TO MED-TEMFSFEL (17:12)              
249300      END-IF                                                              
249400                                                                          
249500     IF CARR-IDRTYP3IV NOT = 'CarrierSummary'                             
249600       MOVE 'A92  '        TO WPOS                                        
249700       MOVE FEL                           TO WS-INDATA-TEST               
249800       MOVE ERR-INV-MSG-STRUCTURE         TO MED-IDMFSFEL                 
249900       PERFORM S03-WMEDKONV                                               
250000     END-IF                                                               
250100     .                                                                    
250200     EJECT                                                                
250300 S01-RECEIVE-MESSAGE3 SECTION.                                            
250400     MOVE 'GET'                     TO RECV-KDFUNC                        
250500     MOVE LENGTH OF RECV-AREA       TO RECV-KVDLEN                        
250600     MOVE SPACE                     TO RECV-AREA                          
250700     MOVE 'A33 '        TO WPOS                                           
250800     CALL WZ01RECV USING  RECV-CONTROL-AREA RECV-KVDLEN                   
250900                          RECV-AREA                                       
251000     IF RECV-KDRC > 1                                                     
251100       MOVE RECV-KDRC                TO KDRC-DISP                         
251200       STRING 'WZ01RECV GET ERROR RC='  KDRC-DISPLAY                      
251300       DELIMITED BY SIZE INTO ERRORTEXT                                   
251400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
251500     END-IF                                                               
251600     MOVE 'A43 '        TO WPOS                                           
251700     IF RECV-KDRC = 0                                                     
251800      UNSTRING RECV-AREA (1:RECV-KVDLEN)                                  
251900       DELIMITED BY VBAR                                                  
252000       INTO PICR-IDRTYP3IV    COUNT IN LIDRTYP3IV                         
252100            PICR-IDPURAD      COUNT IN LIDPURAD                           
252200            PICR-KVAVBART     COUNT IN LKVAVBART                          
252300            PICR-KVLEVART     COUNT IN LKVLEVART                          
252400            PICR-KDARTURS-NUM COUNT IN LKDARTURS-NUM                      
252500       OVERFLOW                                                           
252600         MOVE FEL                         TO WS-INDATA-TEST               
252700         MOVE ERR-INV-MSG-STRUCTURE       TO MED-IDMFSFEL                 
252800         PERFORM S03-WMEDKONV                                             
252900      END-UNSTRING                                                        
253000                                                                          
253100      MOVE 'A53 '        TO WPOS                                          
253200      IF PICR-IDRTYP3IV NOT = 'PickTaskResult'                            
253300*      MESSAGE TYPE INVALID VALUE                                         
253400       MOVE FEL                           TO WS-INDATA-TEST               
253500       MOVE ERR-INV-MSG-STRUCTURE         TO MED-IDMFSFEL                 
253600       PERFORM S03-WMEDKONV                                               
253700      END-IF                                                              
253800                                                                          
253900      IF PICR-IDPURAD  NOT NUMERIC                                        
254000*       LINE NUMBER IS A MESS                                             
254100       MOVE FEL                           TO WS-INDATA-TEST               
254200       MOVE ERR-INVALID-VALUE             TO MED-IDMFSFEL                 
254300       PERFORM S03-WMEDKONV                                               
254400       MOVE 'RADNUMMER PACKUNDERLAG'      TO MED-TEMFSFEL (17:18)         
254500      END-IF                                                              
254600      IF PICR-KVAVBART NOT NUMERIC                                        
254700*       ALLOCATED QUANTITY VALUE IS A MESS                                
254800       MOVE FEL                           TO WS-INDATA-TEST               
254900       MOVE ERR-INVALID-VALUE             TO MED-IDMFSFEL                 
255000       PERFORM S03-WMEDKONV                                               
255100       MOVE 'BEGÄRD KVANTITET'     TO MED-TEMFSFEL (17:16)                
255200      END-IF                                                              
255300      IF PICR-KVLEVART NOT NUMERIC                                        
255400*       SUPPLY QUANTITY VALUE IS A MESS                                   
255500       MOVE FEL                           TO WS-INDATA-TEST               
255600       MOVE ERR-INVALID-VALUE             TO MED-IDMFSFEL                 
255700       PERFORM S03-WMEDKONV                                               
255800       MOVE 'PACKAD KVANTITET'     TO MED-TEMFSFEL (17:16)                
255900      END-IF                                                              
256000      IF LKDARTURS-NUM = 0                                                
256100*       -- UNSTRING DOES NOT MOVE LAST FIELD IF EMPTY                     
256200        MOVE ZERO TO PICR-KDARTURS-NUM                                    
256300      END-IF                                                              
256400      IF PICR-KDARTURS-NUM NOT NUMERIC                                    
256500*       ARTICLE-ORIGIN VALUE IS A MESS                                    
256600       MOVE FEL                           TO WS-INDATA-TEST               
256700       MOVE ERR-INVALID-VALUE             TO MED-IDMFSFEL                 
256800       PERFORM S03-WMEDKONV                                               
256900       MOVE 'LANDSNUMMER'          TO MED-TEMFSFEL (17:11)                
257000      END-IF                                                              
257100     END-IF                                                               
257200     .                                                                    
257300     EJECT                                                                
257400 S01-RECEIVE-CLOSE  SECTION.                                              
257500     MOVE 'CLOSE'                   TO RECV-KDFUNC                        
257600     CALL WZ01RECV USING  RECV-CONTROL-AREA                               
257700     IF RECV-KDRC > 0                                                     
257800       MOVE RECV-KDRC                TO KDRC-DISP                         
257900       STRING 'WZ01RECV CLOSE ERROR RC='  KDRC-DISPLAY                    
258000       DELIMITED BY SIZE INTO ERRORTEXT                                   
258100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
258200     END-IF                                                               
258300     .                                                                    
258400     EJECT                                                                
258500 S02-SEND-OPEN  SECTION.                                                  
258600     MOVE 'OPEN'                    TO SEND-KDFUNC                        
258700     MOVE 'CARPARTS.3IV2.RESPCARRIERFINALIZATION'                         
258800     TO SEND-ADDISPABS                                                    
258900     MOVE WS-ADDISPXTRA                    TO SEND-ADDISPXTRA             
259000     CALL WZ01SEND USING  SEND-CONTROL-AREA                               
259100                          SEND-OPEN-AREA                                  
259200     IF SEND-KDRC > 0                                                     
259300       MOVE SEND-KDRC                TO KDRC-DISP                         
259400       STRING 'WZ01SEND OPEN ERROR RC='  KDRC-DISPLAY                     
259500       DELIMITED BY SIZE INTO ERRORTEXT                                   
259600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
259700     END-IF                                                               
259800     .                                                                    
259900     EJECT                                                                
260000 S02-SEND-RESPONSE SECTION.                                               
260100     PERFORM S02-RESPONSE-HEADER                                          
260200     MOVE 'PUT'                     TO SEND-KDFUNC                        
260300     CALL WZ01SEND USING  SEND-CONTROL-AREA                               
260400                          SEND-KVDLEN                                     
260500                          SEND-AREA                                       
260600     IF SEND-KDRC > 0                                                     
260700       MOVE SEND-KDRC                TO KDRC-DISP                         
260800       STRING 'WZ01SEND PUT ERROR RC='  KDRC-DISPLAY                      
260900       DELIMITED BY SIZE INTO ERRORTEXT                                   
261000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
261100     END-IF                                                               
261200     .                                                                    
261300     EJECT                                                                
261400 S02-RESPONSE-HEADER   SECTION.                                           
261500     MOVE SPACE      TO SEND-AREA                                         
261600     MOVE 1          TO SEND-KVDLEN                                       
261700     MOVE ZERO       TO LIDMSG3IV                                         
261800     INSPECT FUNCTION REVERSE(RESP-IDMSG3IV)                              
261900       TALLYING LIDMSG3IV FOR LEADING SPACE                               
262000     COMPUTE LIDMSG3IV = LENGTH OF RESP-IDMSG3IV - LIDMSG3IV              
262100     IF LIDMSG3IV   > 0                                                   
262200       STRING RESP-IDMSG3IV (1:LIDMSG3IV)  VBAR                           
262300         DELIMITED BY SIZE                                                
262400         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
262500     ELSE                                                                 
262600       STRING VBAR                                                        
262700         DELIMITED BY SIZE                                                
262800         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
262900     END-IF                                                               
263000*                                                                         
263100     MOVE ZERO       TO LIDMTYP3IV                                        
263200     INSPECT FUNCTION REVERSE(RESP-IDMTYP3IV)                             
263300       TALLYING LIDMTYP3IV FOR LEADING SPACE                              
263400     COMPUTE LIDMTYP3IV = LENGTH OF RESP-IDMTYP3IV - LIDMTYP3IV           
263500     IF LIDMTYP3IV  > 0                                                   
263600       STRING RESP-IDMTYP3IV (1:LIDMTYP3IV)  VBAR                         
263700         DELIMITED BY SIZE                                                
263800         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
263900     ELSE                                                                 
264000       STRING VBAR                                                        
264100         DELIMITED BY SIZE                                                
264200         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
264300     END-IF                                                               
264400*                                                                         
264500     MOVE ZERO       TO LTISTAMP3IV                                       
264600     INSPECT FUNCTION REVERSE(RESP-TISTAMP3IV)                            
264700       TALLYING LTISTAMP3IV FOR LEADING SPACE                             
264800     COMPUTE LTISTAMP3IV = LENGTH OF RESP-TISTAMP3IV - LTISTAMP3IV        
264900     IF LTISTAMP3IV > 0                                                   
265000       STRING RESP-TISTAMP3IV (1:LTISTAMP3IV)  VBAR                       
265100         DELIMITED BY SIZE                                                
265200         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
265300     ELSE                                                                 
265400       STRING VBAR                                                        
265500         DELIMITED BY SIZE                                                
265600         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
265700     END-IF                                                               
265800*                                                                         
265900     STRING RESP-IDDC  VBAR                                               
266000       DELIMITED BY SIZE                                                  
266100       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
266200*                                                                         
266300     MOVE 1          TO LIDANSTNR                                         
266400     INSPECT RESP-IDANSTNR                                                
266500       TALLYING LIDANSTNR FOR LEADING SPACE                               
266600     STRING RESP-IDANSTNR (LIDANSTNR:)  VBAR                              
266700       DELIMITED BY SIZE                                                  
266800       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
266900*                                                                         
267000     MOVE ZERO       TO LIDSNO3IV                                         
267100     INSPECT FUNCTION REVERSE(RESP-IDSNO3IV)                              
267200       TALLYING LIDSNO3IV FOR LEADING SPACE                               
267300     COMPUTE LIDSNO3IV = LENGTH OF RESP-IDSNO3IV - LIDSNO3IV              
267400     STRING RESP-IDSNO3IV (1:LIDSNO3IV)  VBAR                             
267500       DELIMITED BY SIZE                                                  
267600       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
267700*                                                                         
267800     MOVE 1          TO LKDRESP3IV                                        
267900     INSPECT RESP-KDRESP3IV                                               
268000       TALLYING LKDRESP3IV FOR LEADING SPACE                              
268100     STRING RESP-KDRESP3IV (LKDRESP3IV:)  VBAR                            
268200       DELIMITED BY SIZE                                                  
268300       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
268400*                                                                         
268500     MOVE ZERO       TO LBERESP3IV                                        
268600     INSPECT FUNCTION REVERSE(RESP-BERESP3IV)                             
268700       TALLYING LBERESP3IV FOR LEADING SPACE                              
268800     COMPUTE LBERESP3IV = LENGTH OF RESP-BERESP3IV - LBERESP3IV           
268900     IF LBERESP3IV > 0                                                    
269000       STRING RESP-BERESP3IV (1:LBERESP3IV)                               
269100         DELIMITED BY SIZE                                                
269200         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
269300     END-IF                                                               
269400                                                                          
269500*    -- SEND-KVDLEN "POINTS" TO AFTER LAST CHARACTER IN MSG               
269600*    -- ADJUST TO CORRECT LENGTH                                          
269700     SUBTRACT 1 FROM SEND-KVDLEN                                          
269800     .                                                                    
269900     EJECT                                                                
270000 S02-SEND-RESPONSE2 SECTION.                                              
270100     PERFORM S02-RESPONSE-RESULT                                          
270200     MOVE 'PUT'                     TO SEND-KDFUNC                        
270300     CALL WZ01SEND USING  SEND-CONTROL-AREA                               
270400                          SEND-KVDLEN                                     
270500                          SEND-AREA                                       
270600     IF SEND-KDRC > 0                                                     
270700       MOVE SEND-KDRC                TO KDRC-DISP                         
270800       STRING 'WZ01SEND PUT ERROR RC='  KDRC-DISPLAY                      
270900       DELIMITED BY SIZE INTO ERRORTEXT                                   
271000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
271100     END-IF                                                               
271200     .                                                                    
271300     EJECT                                                                
271400 S02-RESPONSE-RESULT  SECTION.                                            
271500     MOVE SPACE      TO SEND-AREA                                         
271600     MOVE 1          TO SEND-KVDLEN                                       
271700*                                                                         
271800     MOVE ZERO       TO LIDRTYP3IV                                        
271900     INSPECT FUNCTION REVERSE(CRES-IDRTYP3IV)                             
272000       TALLYING LIDRTYP3IV FOR LEADING SPACE                              
272100     COMPUTE LIDRTYP3IV = LENGTH OF CRES-IDRTYP3IV - LIDRTYP3IV           
272200     IF LIDRTYP3IV > 0                                                    
272300       STRING CRES-IDRTYP3IV (1:LIDRTYP3IV)  VBAR                         
272400         DELIMITED BY SIZE                                                
272500         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
272600     ELSE                                                                 
272700       STRING VBAR                                                        
272800         DELIMITED BY SIZE                                                
272900         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
273000     END-IF                                                               
273100*                                                                         
273200     MOVE 1          TO LIDPRODNR                                         
273300     INSPECT CRES-IDPRODNR                                                
273400       TALLYING LIDPRODNR FOR LEADING SPACE                               
273500     STRING CRES-IDPRODNR (LIDPRODNR:)  VBAR                              
273600       DELIMITED BY SIZE                                                  
273700       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
273800*                                                                         
273900     MOVE 1          TO LIDPLKLST                                         
274000     STRING CRES-IDPLKLST               VBAR                              
274100       DELIMITED BY SIZE                                                  
274200       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
274300*                                                                         
274400     MOVE 1          TO LIDLOPNR-ORD                                      
274500     INSPECT CRES-IDLOPNR-ORD                                             
274600       TALLYING LIDLOPNR-ORD FOR LEADING SPACE                            
274700     STRING CRES-IDLOPNR-ORD (LIDLOPNR-ORD:)  VBAR                        
274800       DELIMITED BY SIZE                                                  
274900       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
275000*                                                                         
275100     MOVE 1          TO LIDKOLLI                                          
275200     INSPECT CRES-IDKOLLI                                                 
275300       TALLYING LIDKOLLI  FOR LEADING SPACE                               
275400     STRING CRES-IDKOLLI  (LIDKOLLI:)  VBAR                               
275500       DELIMITED BY SIZE                                                  
275600       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
275700*                                                                         
275800     MOVE ZERO       TO LADFLLOC                                          
275900     INSPECT FUNCTION REVERSE(CRES-ADFLLOC)                               
276000       TALLYING LADFLLOC FOR LEADING SPACE                                
276100     COMPUTE LADFLLOC = LENGTH OF CRES-ADFLLOC - LADFLLOC                 
276200     IF LADFLLOC > 0                                                      
276300       STRING CRES-ADFLLOC (1:LADFLLOC)                                   
276400         DELIMITED BY SIZE                                                
276500         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
276600     END-IF                                                               
276700                                                                          
276800*    -- SEND-KVDLEN "POINTS" TO AFTER LAST CHARACTER IN MSG               
276900*    -- ADJUST TO CORRECT LENGTH                                          
277000     SUBTRACT 1 FROM SEND-KVDLEN                                          
277100     .                                                                    
277200     EJECT                                                                
277300 S02-SEND-CLOSE  SECTION.                                                 
277400     MOVE 'CLOSE'                   TO SEND-KDFUNC                        
277500     CALL WZ01SEND USING  SEND-CONTROL-AREA                               
277600     IF SEND-KDRC > 0                                                     
277700       MOVE SEND-KDRC                TO KDRC-DISP                         
277800       STRING 'WZ01SEND CLOSE ERROR RC='  KDRC-DISPLAY                    
277900       DELIMITED BY SIZE INTO ERRORTEXT                                   
278000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
278100     END-IF                                                               
278200     .                                                                    
278300     EJECT                                                                
278400 S03-WMEDKONV  SECTION.                                                   
278500     MOVE 'S  '             TO MED-IDSKYLT                                
278600     IF MED-IDMFSFEL NOT = SPACE                                          
278700         CALL WMEDKONV USING MED-WMEDAREA                                 
278800*        MOVE MED-MFSFEL TO MED-TEMFSFEL                                  
278900     END-IF                                                               
279000     .                                                                    
279100     EJECT                                                                
279200 S09-SKAPA-KOLLI-SEGMENT    SECTION.                                      
279300                                                                          
279400     MOVE WS-IDDISTR-NUM    TO   TEST-IDDISTR                             
279500     IF DIST19-SATS                                                       
279600       MOVE SPACE           TO  KOLLI-IDTRP                               
279700     ELSE                                                                 
279800       MOVE WS-ODEL-IDTRP   TO  KOLLI-IDTRP                               
279900     END-IF                                                               
280000                                                                          
280100     MOVE WS-IDKOLLI-NUM    TO  KOLLI-IDKOLLI                             
280200     MOVE WS-IDANSTNR       TO  KOLLI-IDPLOCK                             
280300     MOVE WS-IDDISTR-NUM    TO  KOLLI-IDDISTR                             
280400     MOVE WS-IDKUNDNR-NUM   TO  KOLLI-IDKUNDNR                            
280500     MOVE WS-ADFLOMR        TO  KOLLI-ADFLOMR                             
280600     MOVE WS-ADRUTNIV       TO  KOLLI-ADRUTNIV                            
280700     MOVE WS-DIKOLLIL       TO  KOLLI-DIKOLLIL                            
280800     MOVE WS-DIKOLLIB       TO  KOLLI-DIKOLLIB                            
280900     MOVE WS-DIKOLLIH       TO  KOLLI-DIKOLLIH                            
281000     MOVE WS-KDEMBTYP       TO  KOLLI-KDEMBTYP                            
281100     MOVE WS-MOD-VKORDBTO   TO  KOLLI-VKORDBTO-KOLLI                      
281200     MOVE WS-IDDC           TO  KOLLI-IDDC                                
281300     MOVE WS-ADFLGEO        TO  KOLLI-ADFLGEO                             
281400     MOVE WS-KDKOLLI        TO  KOLLI-KDKOLLI                             
281500     MOVE WS-IDKOLLI-SAMP   TO  KOLLI-IDKOLLI-SAMP                        
281600     MOVE NEJ               TO  KOLLI-FLBANDST                            
281700                                KOLLI-FLFRSUTS                            
281800     MOVE ZERO              TO  KOLLI-IDKOLLI-FLER                        
281900                                KOLLI-IDTRPTNR                            
282000                                KOLLI-ADVMODUL                            
282100                                KOLLI-ADHMODUL                            
282200                                KOLLI-IDFAKLOP                            
282300                                KOLLI-IDFAKT                              
282400                                KOLLI-IDFAKT-EXP                          
282500                                KOLLI-DIDMODUL                            
282600                                KOLLI-DIHMODUL                            
282700                                KOLLI-KVFALRAD                            
282800                                KOLLI-KDKOLSTA                            
282900                                KOLLI-KDORDKL                             
283000                                KOLLI-TIFAKT                              
283100                                KOLLI-TIFAKT-EXP                          
283200                                KOLLI-TIFAKTID                            
283300                                KOLLI-TIFAKTID-EXP                        
283400                                KOLLI-TILASTN                             
283500                                KOLLI-TILASTID                            
283600                                KOLLI-TIPACKN                             
283700                                KOLLI-TIPACTID                            
283800                                KOLLI-TIAAVVD-PATR                        
283900                                KOLLI-VKORDNTO-KOLLI                      
284000                                KOLLI-SUORDV-KOLLI                        
284100                                KOLLI-SUORDV-KLI-EXP                      
284200                                KOLLI-SUORDV-LOC                          
284300                                KOLLI-SUORDV-LOCPREL                      
284400                                KOLLI-KDARTURS-KOLLI                      
284500                                KOLLI-KVORDRAD                            
284600                                KOLLI-KDFARLIG-KOLLI                      
284700                                KOLLI-KVFLAMP-KOLLI                       
284800                                KOLLI-IDLASTN                             
284900* OBS - HÄR INITIERAS NYA DDGS-FÄLT RENT GENERELLT                        
285000* OBS - KONTROLLERA OM DETTA ÄR KORREKT!!!                                
285100                                KOLLI-DASUPREF                            
285200                                KOLLI-TISUPTID                            
285300                                KOLLI-KDVIA                               
285400                                KOLLI-IDTULLNR                            
285500                                KOLLI-RETULKS                             
285600                                KOLLI-IDSHIPM                             
285700     MOVE SPACE             TO  KOLLI-IDLEVNR                             
285800                                KOLLI-KDVALISO                            
285900                                KOLLI-KDVALISO-EXP                        
286000*                                                                         
286100     MOVE +1 TO FG-INDX                                                   
286200     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
286300       MOVE ZERO            TO KOLLI-IDPSN(FG-INDX)                       
286400                               KOLLI-VKART-FG(FG-INDX)                    
286500                               KOLLI-VLFG(FG-INDX)                        
286600       ADD +1 TO FG-INDX                                                  
286700     END-PERFORM                                                          
286800     MOVE ZERO              TO KOLLI-SUEQFG                               
286900                               KOLLI-DARFS                                
287000*                                                                         
287100     MOVE SPACE             TO  KOLLI-FLUTLAST                            
287200                                KOLLI-IDSUPREF                            
287300                                KOLLI-FLAUTFAK                            
287400                                KOLLI-FLTULLG                             
287500                                KOLLI-IDLBBET                             
287600                                KOLLI-IDTULFTG                            
287610                                KOLLI-FILLERX2                            
287700     COMPUTE KOLLI-VLORDBTO-KOLLI ROUNDED =                               
287800       KOLLI-DIKOLLIL * KOLLI-DIKOLLIH * KOLLI-DIKOLLIB / 1000000         
287900*--------------------------------------- KOLLI BREDD, HÖJD OCH            
288000*--------------------------------------- LÄNGD ANGIVNA I CM MEDAN         
288100*--------------------------------------- BRUTTOVOLYM I KUBIK M.           
288200     MOVE KOLLI-VLORDBTO-KOLLI TO WS-VLORDBTO                             
288300     .                                                                    
288400     EJECT                                                                
288500 S10-UPPD-SPAR-KOLLI    SECTION.                                          
288600     MOVE 'S10 '  TO WPOS                                                 
288700     MOVE WS-IDDISTR-NUM     TO TEST-IDDISTR                              
288800                                                                          
288900     COMPUTE ARB-KOLLI-VKORDNTO         = ARB-KOLLI-VKORDNTO +            
289000                    SPAR-PRAD-VKARTNTO * SPAR-PRAD-KVLEVART               
289100*                                                                         
289200     IF DIST79-DEALER-PRICE                                               
289300      IF  ORAD-PRARTNTO-LOCPREL > ZERO                                    
289400       COMPUTE ARB-KOLLI-SUORDV-LOCPREL = ARB-KOLLI-SUORDV-LOCPREL        
289500           + SPAR-PRAD-PRARTNTO-LOCPREL * SPAR-PRAD-KVLEVART              
289600      ELSE                                                                
289700       COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC                
289800           + SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                  
289900      END-IF                                                              
290000     ELSE                                                                 
290100       IF DIST79-ECOM-PRICE                                               
290200         COMPUTE ARB-KOLLI-SUORDV-LOC = ARB-KOLLI-SUORDV-LOC              
290300             + SPAR-PRAD-PRARTNTO-LOC * SPAR-PRAD-KVLEVART                
290400       ELSE                                                               
290500          COMPUTE ARB-KOLLI-SUORDV = ARB-KOLLI-SUORDV +                   
290600            SPAR-PRAD-PRARTNTO * SPAR-PRAD-KVLEVART                       
290700          COMPUTE ARB-KOLLI-SUORDV-EXP = ARB-KOLLI-SUORDV-EXP +           
290800            SPAR-PRAD-PRAVCOST * SPAR-PRAD-KVLEVART                       
290900       END-IF                                                             
291000     END-IF                                                               
291100     MOVE SPAR-PRAD-KDVALISO     TO ARB-KOLLI-KDVALISO                    
291200     MOVE SPAR-PRAD-KDVALISO-EXP TO ARB-KOLLI-KDVALISO-EXP                
291300*                                                                         
291400     IF   SPAR-PRAD-KVFLAMP   >  ZERO                                     
291500     AND (SPAR-PRAD-KVFLAMP   <  ARB-KOLLI-KVFLAMP                        
291600     OR   ARB-KOLLI-KVFLAMP   =  ZERO)                                    
291700       MOVE SPAR-PRAD-KVFLAMP  TO ARB-KOLLI-KVFLAMP                       
291800     END-IF                                                               
291900                                                                          
292000     IF  SPAR-PRAD-KDFARLIG  =  +2 OR +3 OR +4 OR +7                      
292100     AND SPAR-PRAD-KDFARLIG  >  ARB-KOLLI-KDFARLIG                        
292200       MOVE SPAR-PRAD-KDFARLIG TO ARB-KOLLI-KDFARLIG                      
292300     END-IF                                                               
292400*                                                                         
292500     .                                                                    
292600     EJECT                                                                
292700 S11-UPPD-KOLLI-FRAN-ARB   SECTION.                                       
292800     MOVE 'S11 '   TO WPOS                                                
292900     ADD ARB-KOLLI-KVFALRAD      TO KOLLI-KVFALRAD                        
293000     ADD ARB-KOLLI-KVORDRAD      TO KOLLI-KVORDRAD                        
293100     ADD ARB-KOLLI-VKORDNTO      TO KOLLI-VKORDNTO-KOLLI                  
293200     ADD ARB-KOLLI-SUORDV-LOC    TO KOLLI-SUORDV-LOC                      
293300     ADD ARB-KOLLI-SUORDV-LOCPREL TO KOLLI-SUORDV-LOCPREL                 
293400     ADD ARB-KOLLI-SUORDV        TO KOLLI-SUORDV-KOLLI                    
293500     ADD ARB-KOLLI-SUORDV-EXP    TO KOLLI-SUORDV-KLI-EXP                  
293600     MOVE ARB-KOLLI-KDVALISO     TO KOLLI-KDVALISO                        
293700     MOVE ARB-KOLLI-KDVALISO-EXP TO KOLLI-KDVALISO-EXP                    
293800*                                                                         
293900     IF      ARB-KOLLI-KVFLAMP   >  ZERO                                  
294000        AND (ARB-KOLLI-KVFLAMP   <  KOLLI-KVFLAMP-KOLLI                   
294100        OR   KOLLI-KVFLAMP-KOLLI =  ZERO)                                 
294200         MOVE ARB-KOLLI-KVFLAMP  TO KOLLI-KVFLAMP-KOLLI                   
294300     END-IF                                                               
294400*                                                                         
294500     IF ARB-KOLLI-KDFARLIG       >  KOLLI-KDFARLIG-KOLLI                  
294600         MOVE ARB-KOLLI-KDFARLIG TO KOLLI-KDFARLIG-KOLLI                  
294700     END-IF                                                               
294800     SKIP2                                                                
294900     MOVE WS-KDKOLLI             TO KOLLI-KDKOLLI                         
295000     MOVE WS-KDORDKL             TO KOLLI-KDORDKL                         
295100     MOVE WS-FLAUTFAK            TO KOLLI-FLAUTFAK                        
295200     MOVE WS-DIKOLLIL            TO KOLLI-DIKOLLIL                        
295300     MOVE WS-DIKOLLIH            TO KOLLI-DIKOLLIH                        
295400     MOVE WS-DIKOLLIB            TO KOLLI-DIKOLLIB                        
295500     MOVE WS-KDEMBTYP            TO KOLLI-KDEMBTYP                        
295600*                                                                         
295700     COMPUTE KOLLI-VKORDBTO-KOLLI =                                       
295800             ARB-KOLLI-VKORDNTO + WS-EMB-VKTARA-ONE-CASE                  
295900     END-COMPUTE                                                          
296000*                                                                         
296100     MOVE WS-IDTRPTNR                 TO KOLLI-IDTRPTNR                   
296200     MOVE WS-ADFLGEO                  TO KOLLI-ADFLGEO                    
296300     MOVE WS-ADFLOMR                  TO KOLLI-ADFLOMR                    
296400     MOVE WS-ADRUTNIV                 TO KOLLI-ADRUTNIV                   
296500     MOVE WS-DIHMODUL                 TO KOLLI-DIHMODUL                   
296600     MOVE WS-DIDMODUL                 TO KOLLI-DIDMODUL                   
296700     MOVE WS-ADVMODUL                 TO KOLLI-ADVMODUL                   
296800     MOVE WS-ADHMODUL                 TO KOLLI-ADHMODUL                   
296900     MOVE WS-FLUTLAST                 TO KOLLI-FLUTLAST                   
297000                                                                          
297100     IF KOLLI-FLAUTFAK = JA AND DIST03-SVERIGE-2                          
297200       MOVE NEJ                     TO KOLLI-FLUTLAST                     
297300     END-IF                                                               
297400                                                                          
297500     IF KOLLI-KDFARLIG-KOLLI = +4                                         
297600     OR KOLLI-KDFARLIG-KOLLI = +7                                         
297700       MOVE +950                    TO KOLLI-ADFLOMR                      
297800     END-IF                                                               
297900     .                                                                    
298000     EJECT                                                                
298100 S12-SKAPA-4322 SECTION.                                                  
298200     MOVE 'S12 '   TO WPOS                                                
298300*SKAPA INFO TILL SVENSKA ÅF, SÄNDS VIA VR.                                
298400     IF DIST03-SVERIGE-100-799                                            
298500     OR DIST03-NORGE                                                      
298600     OR DIST03-DANMARK-900                                                
298700     OR DIST85-PU-VIA-VR                                                  
298800     OR DIST21-TYRE                                                       
298900        MOVE WS-IDPRODNR TO 4322-4322-IDPRODNR                            
299000        MOVE KOLLI-IDKOLLI  TO 4322-4322-IDKOLLI                          
299100        MOVE 4322-4322-WDGX4322 TO 4322-WDGX4322                          
299200        PERFORM IMS-ISRT-4322-SEGM                                        
299300     END-IF                                                               
299400     .                                                                    
299500     EJECT                                                                
299600 S13-UPPDAT-KDORDSTA SECTION.                                             
299700     MOVE 'S13 '   TO WPOS                                                
299800     MOVE WS-IDDISTR-NUM            TO   TEST-IDDISTR                     
299900     IF DIST19-SATS                                                       
300000*****  FIX-START-DEL2 930303 FÖR ATT TA HAND OM EN ORDER SOM              
300100*      SAKNAR ORDERHUVUD (WDQ2) OBS, VID ANVÄNDANDE ÖPPNA OCKSÅ           
300200*      FIX-DEL1 I SECTION FBBA-.                                          
300300*       OR WS-IDPRODNR = '0531053'                                        
300400*****  FIX-END-DEL2  930303                                               
300500       CONTINUE                                                           
300600     ELSE                                                                 
300700       MOVE WS-KORD-IDORDER       TO W-201-IDORDER                        
300800       MOVE WS-IDDC               TO W-IDDC                               
300900       PERFORM IMS-GHU-WDQ212                                             
301000         IF WDQ2-ARB-KDORDSTA = 'U '                                      
301100           MOVE 'U*'         TO WDQ2-ARB-KDORDSTA                         
301200           PERFORM IMS-REPL-WDQ212                                        
301300         END-IF                                                           
301400     END-IF                                                               
301500     .                                                                    
301600     EJECT                                                                
301700 S16-UPPD-FARLIGT-GODS-DATA SECTION.                                      
301800     MOVE 'S16 '   TO WPOS                                                
301900     MOVE +1 TO FG-INDX                                                   
302000     PERFORM UNTIL FG-INDX > FG-MAX-INDX                                  
302100                                                                          
302200       IF TAB-IDPSN(FG-INDX) > ZERO                                       
302300         MOVE TAB-IDPSN(FG-INDX)    TO KOLLI-IDPSN(FG-INDX)               
302400         COMPUTE KOLLI-VKART-FG(FG-INDX) =                                
302500                                       TAB-VKART-FG(FG-INDX) /            
302600                                       1                                  
302700         COMPUTE KOLLI-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) /                
302800                                       1                                  
302900                                                                          
303000       ELSE                                                               
303100         MOVE ZERO                  TO KOLLI-IDPSN(FG-INDX)               
303200                                       KOLLI-VKART-FG(FG-INDX)            
303300                                       KOLLI-VLFG(FG-INDX)                
303400       END-IF                                                             
303500                                                                          
303600       ADD +1 TO FG-INDX                                                  
303700     END-PERFORM                                                          
303800                                                                          
303900     IF TAB-IDPSN(1) > ZERO                                               
304000       COMPUTE KOLLI-SUEQFG = TOTAL-SUEQFG / 1                            
304100     ELSE                                                                 
304200       MOVE ZERO TO KOLLI-SUEQFG                                          
304300     END-IF                                                               
304400     .                                                                    
304500     EJECT                                                                
304600 S17-BERAEKNA-FG-FAELT SECTION.                                           
304700     MOVE 'S17 '   TO WPOS                                                
304800     COMPUTE TAB-VLFG(FG-INDX) = TAB-VLFG(FG-INDX) +                      
304900                                 (SPAR-VLFG        *                      
305000                                  KKOLLI-KVLEVART)                        
305100     IF SPAR-IDPSN = 10 OR 11                                             
305200       COMPUTE TAB-VKART-FG(FG-INDX) = TAB-VKART-FG(FG-INDX) +            
305300                                       (SPAR-VKART-FG        *            
305400                                        KKOLLI-KVLEVART)                  
305500     ELSE                                                                 
305600       MOVE ZERO TO TAB-VKART-FG(FG-INDX)                                 
305700     END-IF                                                               
305800                                                                          
305900     MOVE 10 TO FG-INDX                                                   
306000     .                                                                    
306100     SKIP2                                                                
306200                                                                          
306300                                                                          
306400 S20-DATA-TILL-DEL-NOTE SECTION.                                          
306500     MOVE 'S20 '    TO WPOS                                               
306600     MOVE WS-IDDISTR-NUM           TO TEST-IDDISTR                        
306700     IF DIST07-USA-RETAILER-DNOTE                                         
306800     OR DIST07-CAN-RETAILER                                               
306900        INITIALIZE DNOT-ORDER-INFO                                        
307000                                                                          
307100        MOVE PROGRAM-NAMN             TO DNOT-IDPGM                       
307200        MOVE WS-SPAR-IDORDER          TO DNOT-IDORDER                     
307300        MOVE WS-SPAR-IDARTNR          TO DNOT-IDARTNR                     
307400        MOVE WS-SPAR-IDDC             TO DNOT-IDDC                        
307500        MOVE WS-SPAR-BEART            TO DNOT-BEART-USA                   
307600        MOVE WS-SPAR-KVBEART          TO DNOT-KVBEART                     
307700        MOVE WS-SPAR-FLTILLK          TO DNOT-FLTILLK                     
307800        MOVE WS-SPAR-IDKUNDRF-RO      TO DNOT-IDKUNDRF-RO                 
307900        MOVE WS-SPAR-IDPURAD          TO DNOT-IDPURAD                     
308000        MOVE KKOLLI-IDKOLLI           TO DNOT-IDKOLLI                     
308100        MOVE KKOLLI-IDPRODNR          TO DNOT-IDPRODNR                    
308200        MOVE KKOLLI-KVLEVART          TO DNOT-KVLEVART                    
308300                                                                          
308400        CALL W411DNOT USING DNOT-W411DNOT                                 
308500                            DNOT-ORQP-PCB                                 
308600                            DNOT-ORQP2-PCB                                
308700                            DNOT-ORQP3-PCB                                
308800                            DNOT-4013-PCB                                 
308900                            DNOT-BENA-PCB                                 
309000     END-IF                                                               
309100     .                                                                    
309200     EJECT                                                                
309300 S22-KOLLA-RADER        SECTION.                                          
309400     MOVE 'S22 '   TO WPOS                                                
309500     PERFORM IMS-GNP-RAD-OKVAL                                            
309600                                                                          
309700     PERFORM UNTIL SEGMENT-SAKNAS                                         
309800                                                                          
309900       IF ORAD-KDRADSTA < 4                                               
310000          COMPUTE WS-KVORAPP = ORAD-KVAVBART - ORAD-KVLEVART              
310100          MOVE ORAD-FLNOLLJ      TO WS-FLNOLLJ                            
310200          MOVE ORAD-ADLAGOMR     TO WS-PACKN-OMRADE                       
310300                                                                          
310400          EVALUATE TRUE                                                   
310500          WHEN DCS-CDC OR DCS-CDC-TR                                      
310600            IF WS-NOLLNING-PACK-OMR OR                                    
310700               ORAD-KVLEVART > ZERO OR                                    
310800               WS-KDORDKL < 3                                             
310900               CONTINUE                                                   
311000***         << INGET NOLLJAGNINGSKRAV OM OVANSTÅNDE VILLKOR ÄR            
311100***            UPPFYLLT.                                                  
311200            ELSE                                                          
311300               IF WS-FLNOLLJ = NEJ AND                                    
311400                  ORAD-KVLEVART = ZERO                                    
311500                  MOVE JA TO WS-FLPAFEL                                   
311600               END-IF                                                     
311700            END-IF                                                        
311800          WHEN DCS-SDC                                                    
311900            IF ORAD-KVLEVART > ZERO OR                                    
312000               WS-KDORDKL < 3                                             
312100               CONTINUE                                                   
312200***         << INGET NOLLJAGNINGSKRAV OM OVANSTÅNDE VILLKOR ÄR            
312300***            UPPFYLLT.                                                  
312400            ELSE                                                          
312500               IF WS-FLNOLLJ = NEJ AND                                    
312600                  ORAD-KVLEVART = ZERO                                    
312700                  MOVE JA TO WS-FLPAFEL                                   
312800               END-IF                                                     
312900            END-IF                                                        
313000          WHEN DCS-NDC-PF                                                 
313100            IF ORAD-KVLEVART > ZERO OR                                    
313200               WS-KDORDKL < 4                                             
313300               CONTINUE                                                   
313400***         << INGET NOLLJAGNINGSKRAV OM OVANSTÅNDE VILLKOR ÄR            
313500***            UPPFYLLT. NDC NOLLJAGNING SAMTLIGA ORDERKLASSER            
313600            ELSE                                                          
313700               IF WS-FLNOLLJ = NEJ AND                                    
313800                  ORAD-KVLEVART = ZERO                                    
313900                  MOVE JA TO WS-FLPAFEL                                   
314000               END-IF                                                     
314100            END-IF                                                        
314200          WHEN DCS-NDC-NA                                                 
314300            IF ORAD-KVLEVART > ZERO                                       
314400               CONTINUE                                                   
314500***         << INGET NOLLJAGNINGSKRAV OM OVANSTÅNDE VILLKOR ÄR            
314600***            UPPFYLLT. NDC NOLLJAGNING SAMTLIGA ORDERKLASSER            
314700            ELSE                                                          
314800               IF WS-FLNOLLJ = NEJ AND                                    
314900                  ORAD-KVLEVART = ZERO                                    
315000                  MOVE JA TO WS-FLPAFEL                                   
315100               END-IF                                                     
315200            END-IF                                                        
315300          WHEN OTHER                                                      
315400            CONTINUE                                                      
315500          END-EVALUATE                                                    
315600       END-IF                                                             
315700       PERFORM IMS-GNP-RAD-OKVAL                                          
315800     END-PERFORM                                                          
315900     .                                                                    
316000* IMS SEKTIONER                                                           
316100                                                                          
316200*IMS-GET-MSG SECTION.                                                     
316300*                                                                         
316400*    MOVE '  QC' TO GODK-STATUSKODER                                      
316500*    CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
316600*    MOVE MSG-STATUS-CODE TO STATUS-WS                                    
316700*    PERFORM IMS-STATUSKONTROLL                                           
316800*    SKIP3                                                                
316900*    .                                                                    
317000                                                                          
317100 IMS-INSERT-MSG SECTION.                                                  
317200                                                                          
317300     IF NOT ENGLISH-TEXT                                                  
317400       MOVE '0' TO MFS-KDHUVOMR                                           
317500     END-IF                                                               
317600*                                                                         
317700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
317800     MOVE SPACE TO GODK-STATUSKODER                                       
317900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
318000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
318100     PERFORM IMS-STATUSKONTROLL                                           
318200     .                                                                    
318300 IMS-CHANGE-ALTMSG       SECTION.                                         
318400                                                                          
318500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
318600     MOVE '  ' TO GODK-STATUSKODER                                        
318700     CALL CBLTDLI USING CHNG                                              
318800                          ALT-PCB                                         
318900                          MSG-KDTRANS-1                                   
319000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
319100     PERFORM IMS-STATUSKONTROLL                                           
319200     SKIP3                                                                
319300     .                                                                    
319400 IMS-INSERT-ALTMSG SECTION.                                               
319500                                                                          
319600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
319700     MOVE SPACE TO GODK-STATUSKODER                                       
319800     CALL CBLTDLI USING ISRT ALT4397-PCB ALT-IO-AREA                      
319900     MOVE ALT4397-STATUS-CODE TO STATUS-WS                                
320000     PERFORM IMS-STATUSKONTROLL                                           
320100     .                                                                    
320200     EJECT                                                                
320300 IMS-GU-WDE401 SECTION.                                                   
320400     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
320500            DELIMITED BY SIZE INTO SSA1                                   
320600     MOVE '  GE' TO GODK-STATUSKODER                                      
320700     CALL CBLTDLI USING GU     WDE41-PCB DLI-IO-E401 SSA1                 
320800     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
320900     PERFORM IMS-STATUSKONTROLL                                           
321000     SKIP3                                                                
321100     .                                                                    
321200 IMS-GHU-WDE401 SECTION.                                                  
321300     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
321400            DELIMITED BY SIZE INTO SSA1                                   
321500     MOVE '    ' TO GODK-STATUSKODER                                      
321600     CALL CBLTDLI USING GHU    WDE41-PCB DLI-IO-E401 SSA1                 
321700     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
321800     PERFORM IMS-STATUSKONTROLL                                           
321900     SKIP3                                                                
322000     .                                                                    
322100 IMS-REPL-WDE401 SECTION.                                                 
322200     MOVE '    ' TO GODK-STATUSKODER                                      
322300     CALL CBLTDLI USING REPL WDE41-PCB DLI-IO-E401                        
322400     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
322500     PERFORM IMS-STATUSKONTROLL                                           
322600     SKIP3                                                                
322700     .                                                                    
322800 IMS-GU-WDE601    SECTION.                                                
322900                                                                          
323000     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
323100            DELIMITED BY SIZE INTO SSA1                                   
323200     MOVE '  GE' TO GODK-STATUSKODER                                      
323300     CALL CBLTDLI USING GU    WDE62-PCB VORD-WDE601 SSA1                  
323400     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
323500     PERFORM IMS-STATUSKONTROLL                                           
323600     SKIP3                                                                
323700     .                                                                    
323800 IMS-GNP-WDE611    SECTION.                                               
323900                                                                          
324000     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
324100            DELIMITED BY SIZE INTO SSA1                                   
324200     MOVE '  GE' TO GODK-STATUSKODER                                      
324300     CALL CBLTDLI USING GNP   WDE62-PCB KOLLI-WDE611 SSA1                 
324400     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
324500     PERFORM IMS-STATUSKONTROLL                                           
324600     SKIP3                                                                
324700     .                                                                    
324800 IMS-GU-WDE4B-KUNDORDER SECTION.                                          
324900     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
325000                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
325100            DELIMITED BY SIZE INTO SSA1                                   
325200     MOVE 'WDE401   ' TO SSA2                                             
325300     MOVE '  GE' TO GODK-STATUSKODER                                      
325400     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
325500     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
325600     PERFORM IMS-STATUSKONTROLL                                           
325700     .                                                                    
325800     SKIP3                                                                
325900 IMS-GU-WDE42-KORD-BSEQ SECTION.                                          
326000     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
326100            DELIMITED BY SIZE INTO SSA1                                   
326200     MOVE 'WDE401   ' TO SSA2                                             
326300     MOVE '  ' TO GODK-STATUSKODER                                        
326400     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
326500     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
326600     PERFORM IMS-STATUSKONTROLL                                           
326700     .                                                                    
326800     SKIP3                                                                
326900 IMS-GHU-RAD-SEK  SECTION.                                                
327000     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
327100            DELIMITED BY SIZE INTO SSA1                                   
327200     MOVE '    ' TO GODK-STATUSKODER                                      
327300     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-E411 SSA1                  
327400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
327500     PERFORM IMS-STATUSKONTROLL                                           
327600     SKIP3                                                                
327700     .                                                                    
327800 IMS-GHN-RAD-SEK  SECTION.                                                
327900     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
328000                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
328100            DELIMITED BY SIZE INTO SSA1                                   
328200     MOVE '  ' TO GODK-STATUSKODER                                        
328300     CALL CBLTDLI USING GHN    WDE4-PCB DLI-IO-E411 SSA1                  
328400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
328500     PERFORM IMS-STATUSKONTROLL                                           
328600     .                                                                    
328700     SKIP2                                                                
328800 IMS-REPL-BEHANDLAD-RAD SECTION.                                          
328900     MOVE '    ' TO GODK-STATUSKODER                                      
329000     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E411                         
329100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
329200     PERFORM IMS-STATUSKONTROLL                                           
329300     SKIP3                                                                
329400     .                                                                    
329500 IMS-GHNP-KOLLI-KOPPL    SECTION.                                         
329600     STRING 'WDE421  *F(WDE421KY =' W-WDE421-IDKOLLI-X ')'                
329700            DELIMITED BY SIZE INTO SSA1                                   
329800     MOVE '  ' TO GODK-STATUSKODER                                        
329900     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E421 SSA1                    
330000     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
330100     PERFORM IMS-STATUSKONTROLL                                           
330200     SKIP3                                                                
330300     .                                                                    
330400 IMS-REPL-KOLLI-KOPPL  SECTION.                                           
330500     MOVE '  '   TO GODK-STATUSKODER                                      
330600     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E421                         
330700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
330800     PERFORM IMS-STATUSKONTROLL                                           
330900     SKIP2                                                                
331000     .                                                                    
331100 IMS-ISRT-KOLLI-KOPPL  SECTION.                                           
331200     MOVE   'WDE421   '       TO   SSA1                                   
331300     MOVE '  II' TO GODK-STATUSKODER                                      
331400     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-E421 SSA1                    
331500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
331600     PERFORM IMS-STATUSKONTROLL                                           
331700     .                                                                    
331800     SKIP2                                                                
331900 IMS-GNP-RAD-OKVAL SECTION.                                               
332000     MOVE   'WDE411'       TO   SSA1                                      
332100     MOVE '  GE' TO GODK-STATUSKODER                                      
332200     CALL CBLTDLI USING GNP    WDE41-PCB DLI-IO-E411 SSA1                 
332300     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
332400     PERFORM IMS-STATUSKONTROLL                                           
332500     .                                                                    
332600     SKIP2                                                                
332700 IMS-GHU-KOLLIREG SECTION.                                                
332800     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
332900            DELIMITED BY SIZE INTO SSA1                                   
333000     MOVE '    ' TO GODK-STATUSKODER                                      
333100     CALL CBLTDLI USING GHU    WDE6-PCB VORD-WDE601  SSA1                 
333200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
333300     PERFORM IMS-STATUSKONTROLL                                           
333400     .                                                                    
333500     SKIP2                                                                
333600 IMS-REPL-KOLLIREG SECTION.                                               
333700     MOVE '    ' TO GODK-STATUSKODER                                      
333800     CALL CBLTDLI USING REPL WDE6-PCB VORD-WDE601                         
333900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
334000     PERFORM IMS-STATUSKONTROLL                                           
334100     SKIP3                                                                
334200     .                                                                    
334300 IMS-GHU-KOLLI    SECTION.                                                
334400     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
334500            DELIMITED BY SIZE INTO SSA1                                   
334600     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
334700            DELIMITED BY SIZE INTO SSA2                                   
334800     MOVE '  GE' TO GODK-STATUSKODER                                      
334900     CALL CBLTDLI USING GHU    WDE6-PCB KOLLI-WDE611 SSA1 SSA2            
335000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
335100     PERFORM IMS-STATUSKONTROLL                                           
335200     SKIP3                                                                
335300     .                                                                    
335400 IMS-REPL-KOLLI    SECTION.                                               
335500     MOVE '    ' TO GODK-STATUSKODER                                      
335600     CALL CBLTDLI USING REPL WDE6-PCB KOLLI-WDE611                        
335700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
335800     PERFORM IMS-STATUSKONTROLL                                           
335900     SKIP3                                                                
336000     .                                                                    
336100 IMS-ISRT-KOLLI   SECTION.                                                
336200     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
336300            DELIMITED BY SIZE INTO SSA1                                   
336400     MOVE   'WDE611   '       TO   SSA2                                   
336500     MOVE '  II' TO GODK-STATUSKODER                                      
336600     CALL CBLTDLI USING ISRT WDE6-PCB KOLLI-WDE611 SSA1 SSA2              
336700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
336800     PERFORM IMS-STATUSKONTROLL                                           
336900     .                                                                    
337000     SKIP2                                                                
337100 IMS-ISRT-WDE621 SECTION.                                                 
337200                                                                          
337300     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
337400          DELIMITED BY SIZE INTO SSA1                                     
337500     STRING 'WDE611  (IDKOLLI  =' W-WDE611-IDKOLLI-X ')'                  
337600          DELIMITED BY SIZE INTO SSA2                                     
337700     MOVE 'WDE621 ' TO SSA3                                               
337800     MOVE '  II' TO GODK-STATUSKODER                                      
337900     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE621 SSA1 SSA2 SSA3        
338000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
338100     PERFORM IMS-STATUSKONTROLL                                           
338200     .                                                                    
338300     EJECT                                                                
338400 IMS-GU-WDK5     SECTION.                                                 
338500     STRING 'WDK501  (KDKOLLI  =' W-KDKOLLI-WDK5 ')'                      
338600            DELIMITED BY SIZE INTO SSA1                                   
338700     MOVE '  GE' TO GODK-STATUSKODER                                      
338800     CALL CBLTDLI USING GU    WDK5-PCB DLI-IO-K501 SSA1                   
338900     MOVE WDK5-STATUS-CODE TO STATUS-WS                                   
339000     PERFORM IMS-STATUSKONTROLL                                           
339100     .                                                                    
339200     SKIP2                                                                
339300 IMS-GU-KUNDORDER-SEK-INV SECTION.                                        
339400     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
339500                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
339600            DELIMITED BY SIZE INTO SSA1                                   
339700     MOVE 'WDE401   ' TO SSA2                                             
339800     MOVE '  GE' TO GODK-STATUSKODER                                      
339900     CALL CBLTDLI USING GU   WDE42-PCB DLI-IO-E401 SSA1 SSA2              
340000     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
340100     PERFORM IMS-STATUSKONTROLL                                           
340200     .                                                                    
340300     SKIP3                                                                
340400 IMS-GN-SEQA-WDE4A1 SECTION.                                              
340500     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
340600            DELIMITED BY SIZE INTO SSA1                                   
340700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
340800     CALL CBLTDLI USING GN   WDE4A-PCB DLI-IO-E401 SSA1                   
340900     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
341000                               STATUS-KUNDORDER-SEK-WS                    
341100     PERFORM IMS-STATUSKONTROLL                                           
341200     SKIP2                                                                
341300     .                                                                    
341400 IMS-GU-SEQE-WDE401   SECTION.                                            
341500     STRING 'WDE401  (WDE4ESEQ =' W-WDE4E-KEYSEQ-X ')'                    
341600            DELIMITED BY SIZE INTO SSA1                                   
341700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
341800     CALL CBLTDLI USING GU   WDE4E-PCB DLI-IO-E401 SSA1                   
341900     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
342000     PERFORM IMS-STATUSKONTROLL                                           
342100     SKIP2                                                                
342200     .                                                                    
342300 IMS-GU-E411-RAD    SECTION.                                              
342400     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
342500            DELIMITED BY SIZE INTO SSA1                                   
342600     STRING 'WDE411  (IDPURAD  =' W-WDE411-IDPURAD-X ')'                  
342700            DELIMITED BY SIZE INTO SSA2                                   
342800     MOVE '  GE' TO GODK-STATUSKODER                                      
342900     CALL CBLTDLI USING GU     WDE41-PCB DLI-IO-E411 SSA1 SSA2            
343000     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
343100     PERFORM IMS-STATUSKONTROLL                                           
343200     .                                                                    
343300     SKIP2                                                                
343400 IMS-GU-4726-ROT-KVAL SECTION.                                            
343500     STRING 'WDG701  (WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
343600            DELIMITED BY SIZE INTO SSA1                                   
343700     MOVE '  ' TO GODK-STATUSKODER                                        
343800     CALL CBLTDLI USING GU     4726-PCB DLI-IO-AREA3 SSA1                 
343900     MOVE 4726-STATUS-CODE TO STATUS-WS                                   
344000     PERFORM IMS-STATUSKONTROLL                                           
344100     SKIP2                                                                
344200     .                                                                    
344300 IMS-GNP-4726-UNDERSEG-KVAL SECTION.                                      
344400     STRING 'WDG717  (WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
344500            DELIMITED BY SIZE INTO SSA1                                   
344600     MOVE '  GE' TO GODK-STATUSKODER                                      
344700     CALL CBLTDLI USING GNP    4726-PCB DLI-IO-AREA3 SSA1                 
344800     MOVE 4726-STATUS-CODE TO STATUS-WS                                   
344900     PERFORM IMS-STATUSKONTROLL                                           
345000     SKIP2                                                                
345100     .                                                                    
345200 IMS-INSERT-4726-UNDERSEG SECTION.                                        
345300     STRING 'WDG701  (WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
345400            DELIMITED BY SIZE INTO SSA1                                   
345500     MOVE 'WDG717   ' TO SSA2                                             
345600     MOVE '  ' TO GODK-STATUSKODER                                        
345700     CALL CBLTDLI USING ISRT 4726-PCB DLI-IO-AREA3 SSA1 SSA2              
345800     MOVE 4726-STATUS-CODE TO STATUS-WS                                   
345900     PERFORM IMS-STATUSKONTROLL                                           
346000     SKIP2                                                                
346100     .                                                                    
346200 IMS-INSERT-4727 SECTION.                                                 
346300     STRING 'WDG701  (WDGXKEY  =' W-4726-WDGXKEY-ROT-X ')'                
346400            DELIMITED BY SIZE INTO SSA1                                   
346500     STRING 'WDG717  (WDGXKEY  =' W-4726-WDGXKEY-UNDSEG-X ')'             
346600            DELIMITED BY SIZE INTO SSA2                                   
346700     MOVE 'WDG718   ' TO SSA3                                             
346800     MOVE '  II' TO GODK-STATUSKODER                                      
346900     CALL CBLTDLI USING ISRT 4726-PCB DLI-IO-AREA3                        
347000                               SSA1 SSA2 SSA3                             
347100     MOVE 4726-STATUS-CODE TO STATUS-WS                                   
347200     PERFORM IMS-STATUSKONTROLL                                           
347300     .                                                                    
347400     SKIP2                                                                
347500 IMS-ISRT-4322-SEGM SECTION.                                              
347600     STRING 'WDG201  (WDGXKEY  =' W-4321-IDHTYP-X ')'                     
347700            DELIMITED BY SIZE INTO SSA1                                   
347800     MOVE 'WDG202  *L' TO SSA2                                            
347900     MOVE '  ' TO GODK-STATUSKODER                                        
348000     CALL CBLTDLI USING ISRT 4322-PCB DLI-IO-AREA4 SSA1 SSA2              
348100     MOVE 4322-STATUS-CODE TO STATUS-WS                                   
348200     PERFORM IMS-STATUSKONTROLL                                           
348300     .                                                                    
348400     SKIP2                                                                
348500 IMS-GU-WDQ301    SECTION.                                                
348600     STRING 'WDQ301  (WDQ301KY =' W-WDQ301-ORDERDEL-X ')'                 
348700            DELIMITED BY SIZE INTO SSA1                                   
348800     MOVE '  ' TO GODK-STATUSKODER                                        
348900     CALL CBLTDLI USING GU    WDQ3-PCB DLI-IO-AREA5 SSA1                  
349000     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
349100     PERFORM IMS-STATUSKONTROLL                                           
349200     .                                                                    
349300     SKIP2                                                                
349400 IMS-GHU-WDQ212    SECTION.                                               
349500     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
349600            DELIMITED BY SIZE INTO SSA1                                   
349700     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
349800          DELIMITED BY SIZE INTO SSA2                                     
349900     MOVE '    ' TO GODK-STATUSKODER                                      
350000     CALL CBLTDLI USING GHU  WDQ2-PCB DLI-IO-WDQ212 SSA1 SSA2             
350100     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
350200     PERFORM IMS-STATUSKONTROLL                                           
350300     .                                                                    
350400 IMS-REPL-WDQ212      SECTION.                                            
350500     MOVE '  ' TO GODK-STATUSKODER                                        
350600     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-WDQ212                       
350700     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
350800     PERFORM IMS-STATUSKONTROLL                                           
350900     .                                                                    
351000     SKIP2                                                                
351100 IMS-GHU-447211       SECTION.                                            
351200     STRING 'WDR401  (WDGXKEY  =' W-4471-WDGXKEY-X ')'                    
351300            DELIMITED BY SIZE INTO SSA1                                   
351400     STRING 'WDR460  (KDSEGKEY =' W-4472-KDSEGKEY-X ')'                   
351500            DELIMITED BY SIZE INTO SSA2                                   
351600     MOVE '  GE' TO GODK-STATUSKODER                                      
351700     CALL CBLTDLI USING GHU    4472-PCB DLI-IO-AREA6 SSA1 SSA2            
351800     MOVE 4472-STATUS-CODE TO STATUS-WS                                   
351900     PERFORM IMS-STATUSKONTROLL                                           
352000     .                                                                    
352100 IMS-REPL-447211    SECTION.                                              
352200     MOVE '  '   TO GODK-STATUSKODER                                      
352300     CALL CBLTDLI USING REPL 4472-PCB DLI-IO-AREA6                        
352400     MOVE 4472-STATUS-CODE TO STATUS-WS                                   
352500     PERFORM IMS-STATUSKONTROLL                                           
352600     .                                                                    
352700     SKIP2                                                                
352800 IMS-GU-4478         SECTION.                                             
352900     STRING 'WDR101  (WDGXKEY  =' W-4477-WDGXKEY-X ')'                    
353000            DELIMITED BY SIZE INTO SSA1                                   
353100     STRING 'WDR130  (WDGXKEY  =' W-4478-WDGXKEY-X ')'                    
353200            DELIMITED BY SIZE INTO SSA2                                   
353300     MOVE '  GE' TO GODK-STATUSKODER                                      
353400     CALL CBLTDLI USING GU    4478-PCB DLI-IO-AREA6 SSA1 SSA2             
353500     MOVE 4478-STATUS-CODE TO STATUS-WS                                   
353600     PERFORM IMS-STATUSKONTROLL                                           
353700     .                                                                    
353800     SKIP2                                                                
353900 IMS-ISRT-WDG601 SECTION.                                                 
354000*    WDG6                                                                 
354100     MOVE 'WDG601  ' TO SSA1                                              
354200     MOVE '  II'     TO GODK-STATUSKODER                                  
354300     CALL CBLTDLI USING ISRT WDG6-PCB DLI-IO-AREA7 SSA1                   
354400     MOVE WDG6-STATUS-CODE TO STATUS-WS                                   
354500     PERFORM IMS-STATUSKONTROLL                                           
354600     .                                                                    
354700     SKIP2                                                                
354800                                                                          
354900 IMS-GET-WDQ201-CSEQ SECTION.                                             
355000                                                                          
355100     STRING  'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
355200             DELIMITED BY SIZE INTO    SSA1                               
355300     MOVE    '  GE'              TO    GODK-STATUSKODER                   
355400     CALL    CBLTDLI             USING GU   ORQL-PCB                      
355500                                            DLI-IO-WDQ201 SSA1            
355600     MOVE    ORQL-STATUS-CODE    TO    STATUS-WS                          
355700     PERFORM IMS-STATUSKONTROLL                                           
355800     .                                                                    
355900     SKIP2                                                                
356000                                                                          
356100 IMS-GU-WDB601    SECTION.                                                
356200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
356300          DELIMITED BY SIZE INTO SSA1                                     
356400     MOVE '  '   TO GODK-STATUSKODER                                      
356500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
356600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
356700     PERFORM IMS-STATUSKONTROLL                                           
356800     .                                                                    
356900     SKIP2                                                                
357000 IMS-GU-WDB612    SECTION.                                                
357100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
357200          DELIMITED BY SIZE INTO SSA1                                     
357300     STRING 'WDB612  (IDPRC    =' W-PRC-B6-X ')'                          
357400          DELIMITED BY SIZE INTO SSA2                                     
357500     MOVE '  GE'   TO GODK-STATUSKODER                                    
357600     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-WDB612 SSA1 SSA2              
357700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
357800     PERFORM IMS-STATUSKONTROLL                                           
357900     .                                                                    
358000     SKIP2                                                                
358100 IMS-GHN-WDA6B SECTION.                                                   
358200     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
358300                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
358400            DELIMITED BY SIZE INTO SSA1                                   
358500     MOVE '  GEGB'               TO GODK-STATUSKODER                      
358600     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-WDA601 SSA1              
358700     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
358800     PERFORM IMS-STATUSKONTROLL                                           
358900     .                                                                    
359000     SKIP2                                                                
359100 IMS-REPL-WDA6B SECTION.                                                  
359200     MOVE 'WDA601  '           TO SSA1                                    
359300     MOVE '    '               TO GODK-STATUSKODER                        
359400     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-WDA601 SSA1               
359500     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
359600     PERFORM IMS-STATUSKONTROLL                                           
359700     .                                                                    
359800     SKIP2                                                                
359900 IMS-STATUSKONTROLL SECTION.                                              
360000     SET STATUS-IX TO 1                                                   
360100     SEARCH GODK-STATUS AT END CALL FELLOG                                
360200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
360300     END-SEARCH                                                           
360400     CONTINUE                                                             
360500     .                                                                    
360600                                                                          
