000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W4039700.                                                
000500 AUTHOR.         CAP GEMINI AB/EP.                                        
000600 DATE-WRITTEN.   JUNI 86.                                                 
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET BEHANDLAR ENDAST ETT VISST ANTAL                      
001200*        ORAPPORTERADE RADER OCH UPPDATERA PACKNINGS-,                    
001300*        ORDER-, ARTIKEL-, LÅSNINGS-, AVVIKELSE- OCH                      
001400*        AUTOMATFAKTURERINGSREGISTREN MED DE RADER SOM                    
001500*        PACKAREN HAR GODKÄNT PÅ 4318-BILDEN.                             
001600*        PROGRAMMET UPPDATERAR     WLLOGA (WDL9)                          
001700*        SALDOFÖRÄNDRINGAR *                                              
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W4T397X                                             
002100*                                                                         
002200*    UTDATA.                                                              
002300*        TRANS:       W4O31601                                            
002400*        SE&O                                                             
002500*                                                                         
002600*    CHANGE LOG                                                           
002700*                                                                         
002800*    DIGAMBAR/021011                                                      
002900*    STRUCTURE OF ACTION TRANSACTION 4487 IS CHANGED TO IMPROVE           
003000*    THE RESPONSE TIME OF THE SCREEN 4312.                                
003100*                                                                         
003200* ETRACK 1290414 INTERVALL FOR DISTRICT AND CUSTOMER                      
003300*                                                                         
003400* LINDA NILSSON DEC-2004                                                  
003500* ETRACKER: 1570221                                                       
003600* ETRACKER: 7450328  2008-HÖST  VOHF                                      
003700* ETRACKER: 10143273 2012-09  LOCAL SOURCING CHINA                        
003800* ETRACKER: 10130993 15-04-22                                             
003900*           REDUCE NUMBER OF DELIVERY SCHEDULES                           
004000* ETRACKER: 10254592 2015    DECOMISSION VOHF                             
004100* ETRACKER: 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2          
004200*                                                                         
004300 ENVIRONMENT DIVISION.                                                    
004400     SKIP3                                                                
004500 DATA DIVISION.                                                           
004600     SKIP2                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800*    -- CHECKED BY WY2000                                                 
004900     SKIP3                                                                
005000 77    IDPGM                     PIC X(8)    VALUE 'W4039700'.            
005100 77    FILLER                    PIC X(08)   VALUE 'ERRORTEX'.            
005200 77    ERRORTEX                  PIC X(64)   VALUE SPACE.                 
005300 77    FILLER                    PIC X(08)   VALUE 'CURRENT:'.            
005400 77    CURRENT-SECTION           PIC X(32)   VALUE SPACE.                 
005500 77    FILLER                    PIC X(08)   VALUE 'IMS-SEC:'.            
005600 77    CURRENT-IMS-SECTION       PIC X(32)   VALUE SPACE.                 
005700 77    C0                        PIC S9      VALUE +0 COMP-3.             
005800 77    JA                        PIC X       VALUE 'J'.                   
005900 77    YES                       PIC X       VALUE 'Y'.                   
006000 77    NEJ                       PIC X       VALUE 'N'.                   
006100 77    DEF-IDROLL                PIC X(5)    VALUE 'VOR99'.               
006200                                                                          
006300 77    WS-IDDC                   PIC X(2)    VALUE SPACE.                 
006400                                                                          
006500*01    -COPY WWDCKONS                                                     
006600                                                                          
006700 77    RAETT                     PIC X       VALUE 'R'.                   
006800 77    FEL                       PIC X       VALUE 'F'.                   
006900 77    IND1                      PIC S9(9)   VALUE +0   COMP SYNC.        
007000 77    IND2                      PIC S9(9)   VALUE +0   COMP SYNC.        
007200 77    INX                       PIC S9(9)   VALUE +0   COMP SYNC.        
007201 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
007210 77    IX-DCCLEAR-MAX            PIC S9(3)   COMP SYNC VALUE +99.         
007300 77    INX-TOT-ANT-RADER         PIC S9(3)   VALUE ZERO  COMP-3.          
007400 77    INX-ANT-RADER-MED-FYS-AVV PIC S9(3)   VALUE ZERO  COMP-3.          
007500 77    FILLER                    PIC X(8)   VALUE 'CCCCCCCC'.             
007600 77    SW-TIRODAT-LIKA-MED-ZERO  PIC  X(1)   VALUE 'N'.                   
007800 77    DATUM-SW                  PIC  X(1)   VALUE 'N'.                   
008000 77    MORE-LINES-SW             PIC  X(1)   VALUE 'N'.                   
008100 77    WS-RADER-PER-START        PIC S9(3)   VALUE +25   COMP-3.          
008200 77    WS-ANT-RADER-INT          PIC S9(3)   VALUE +0    COMP-3.          
008300 77    WS-KDORDSTA               PIC X(2)    VALUE SPACE.                 
008400 77    WS-KVORDRAD-PACK          PIC S9(5)   VALUE +0    COMP-3.          
008500 77    WS-KVORDRAD               PIC S9(5)   VALUE +0    COMP-3.          
008600 77    WS-KVORAPP-TOTAL          PIC S9(6)   VALUE +0.                    
008700 77    WS-KVORAPP-PACK           PIC S9(6)   VALUE +0.                    
008800 77    WS-KVPRERO                PIC S9(7)   VALUE +0.                    
008900 77    WS-AVVIKELSE-UTSKR        PIC S9(7)   VALUE +0.                    
009000 77    WS-SUMMA                  PIC S9(7)   VALUE +0.                    
009100 77    FILLER                    PIC X(8)   VALUE 'DDDDDDDD'.             
009200 77    WS-KDFAKTYP               PIC X(1)   VALUE SPACE.                  
009300 77    W-SPAR-IDDC               PIC X(2).                                
009400 77    WS-IDANSTNR               PIC 9(5)   VALUE ZERO.                   
009500 77    WS-IDPLKLST               PIC 9(3)   VALUE ZERO.                   
009600 77    WS-IDDISTR                PIC 9(5)   VALUE ZERO.                   
009700 77    WS-IDDISTR-NUM4           PIC 9(4)   VALUE ZERO.                   
009800 77    WS-SAVE-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
009900 77    WS-IDKUNDNR               PIC 9(6)   VALUE ZERO.                   
010000 77    WS-SAVE-IDKUNDNR          PIC S9(7) VALUE ZERO COMP-3.             
010100 77    WS-IDPRODNR               PIC 9(7)   VALUE ZERO.                   
010200 77    WS-IDPURAD                PIC 9(5)   VALUE ZERO.                   
010300 77    WS-JFR-IDPRODNR           PIC 9(7)   VALUE ZERO.                   
010400 77    WS-IDPRODNR-RED           PIC Z(6)9  VALUE ZERO.                   
010500 77    WS-MODNAMN                PIC X(8)   VALUE SPACE.                  
010600 77    FILLER                    PIC X(8)   VALUE 'EEEEEEEE'.             
010700 77    WS-KVLS                   PIC S9(7)  VALUE ZERO COMP-3.            
010800 77    WS-KVEFRS                 PIC S9(7)  VALUE ZERO COMP-3.            
010900 77    WS-LISTA                  PIC X(3)   VALUE SPACE.                  
011000 77    WS-KDFRAKT                PIC S9(3)  VALUE ZERO.                   
011100 77    WS-KDKOLSTA               PIC 9(1)   VALUE ZERO.                   
011200 77    WS-KDORDSTA-501           PIC S9(1)  VALUE ZERO.                   
011300 77    WS-KDORDKL                PIC S9(1)  VALUE ZERO.                   
011400 77    WS-FLLSBOK                PIC X(1)   VALUE SPACE.                  
011500 77    WS-FLORDSPE               PIC X(1)   VALUE SPACE.                  
011600 77    WS-FLOVRLEV               PIC X(1)   VALUE SPACE.                  
011700 77    WS-AKTUELL-RAD            PIC 9(4)   VALUE ZERO.                   
011800 77    WS-KVLEVART               PIC 9(6)   VALUE ZERO.                   
011900 77    FILLER                    PIC X(8)   VALUE 'FFFFFFFF'.             
012000 77    WS-KVSLATTAT              PIC S9(7)  COMP-3  VALUE ZERO.           
012100*KVSLATTAT = ENDAST KAMPANJREG. UPPDATERING.                              
012200 77    WS-SAMMANSLAGNING-RAD     PIC X.                                   
012300 77    SPAR-KART-KVRESS-ART      PIC S9(7)  COMP-3.                       
012400 77    WS-IDORDER                PIC S9(7)  COMP-3.                       
012500 77    WS-KDRAPRIO               PIC S9(3)  VALUE ZERO  COMP-3.           
012600 77    WS-KDTPOTYP               PIC S9     COMP-3.                       
012700 77    WS-TIDISPIN               PIC S9(7)  COMP-3.                       
012800 77    WS-TRAEFF-PACKARE         PIC X(1)   VALUE 'N'.                    
012900 77    WS-KDORDBEK               PIC S9(3)  COMP-3.                       
013000 77    FILLER                    PIC X(8)   VALUE 'GGGGGGGG'.             
013100 77    WS-KVPTID-MIN             PIC S9(7)  COMP-3.                       
013200 77    WS-KVPTID-TIM             PIC S9(3)  COMP-3.                       
013300 77    W-KVKOLLI                 PIC S9(7)  VALUE ZERO  COMP-3.           
013400 77    W-KVKOLLI-FAKT            PIC S9(7)  VALUE ZERO  COMP-3.           
013500 77    W-KVKOLLI-LAST            PIC S9(7)  VALUE ZERO  COMP-3.           
013600 77    W-KVKOLLI-FL              PIC S9(7)  VALUE ZERO  COMP-3.           
013700 77    RKOD-ABEND                PIC S9(4)  VALUE +33   COMP SYNC.        
013800 77    IX                        PIC S9(9)  VALUE ZERO  COMP SYNC.        
014000 77    IX-CD-OMR                 PIC S9(9)  VALUE ZERO  COMP-3.           
014100 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
014200 77    DAGENS-DATUM              PIC 9(9)   VALUE ZERO.                   
014300 77    WS-KLOCKAN                PIC 9(9)   VALUE ZERO.                   
014400 77    WS-FATTAS                 PIC S9(9)  VALUE +0.                     
014500 77    KDRC-DISP                 PIC 9(4)    VALUE ZERO.                  
014600 77    WS-IDCOM                  PIC S9(9)   VALUE ZERO COMP-3.           
014700 77    W-IDSHIPM                 PIC 9(7)    VALUE ZERO.                  
014800 77    WS-IDPRQUES               PIC S9(7)   VALUE ZERO.                  
014900 77    WS-VOR-TID-BRIST          PIC 9(9)   VALUE ZERO.                   
015000 77    S28-IDARTNR               PIC 9(9)    VALUE ZERO COMP-3.           
015100 77    S28-IDDC                  PIC X(2)    VALUE SPACE.                 
015200 77    S28-KVVORKO               PIC S9(7)   VALUE ZERO COMP-3.           
015300 77    S28-IDANSK                PIC 9(3)    VALUE ZERO COMP-3.           
015400 77    S28-IDLEVNR               PIC X(5)    VALUE SPACE.                 
015500 77    S27-IDDC                  PIC X(2)    VALUE SPACE.                 
015600 77    S27-IDLEVNR               PIC X(5)    VALUE SPACE.                 
015700 77    WS-KV402                  PIC S9(4)   VALUE ZERO.                  
015800 77    WS-IDANSK                 PIC 9(3)    VALUE ZERO.                  
015900 77    WS-XLAG-IDANSK            PIC 9(3)    VALUE ZERO.                  
016000 77    WS-PARTNER                PIC X(1)    VALUE SPACE.                 
016100*                                                                         
016110 01  W-GMT-IDDC-CLEAR-GRP.                                                
016120*                                 GRUPP AV IDDC-CLEAR                     
016130     03 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                                
016140                                 PIC X(2)    VALUE SPACE.                 
016200 01  KDRC-DISPLAY                PIC Z(5).                                
016300*                                                                         
016400 01  S27-IDANSK-X.                                                        
016500     03 S27-IDANSK               PIC 9(3).                                
016600                                                                          
016700 01  S27-IDARTNR-X.                                                       
016800     03 S27-IDARTNR              PIC 9(9).                                
016900                                                                          
017000 01  S27-IDDISTR-X.                                                       
017100     03 S27-IDDISTR              PIC 9(4).                                
017200                                                                          
017300 01  S27-IDKUNDNR-X.                                                      
017400     03 S27-IDKUNDNR             PIC 9(6).                                
017500                                                                          
017600 01    FILLER                    PIC X(8) VALUE 'HHHHHHHH'.               
017700                                                                          
017800 01  ARBETSFALT.                                                          
017900                                                                          
018000     03 WS-DAORDREG              PIC 9(8) VALUE ZERO.                     
018100     03 WS-DAORDREG-DELAR        REDEFINES WS-DAORDREG.                   
018200        05 WS-DAORDREG-TISS      PIC 9(2).                                
018300        05 WS-DAORDREG-TIAAMMDD  PIC 9(6).                                
018400                                                                          
018500     03 WS-DARODAT               PIC 9(8) VALUE ZERO.                     
018600     03 WS-DARODAT-DELAR         REDEFINES WS-DARODAT.                    
018700        05 WS-DARODAT-TISS       PIC 9(2).                                
018800        05 WS-DARODAT-TIAAMMDD   PIC 9(6).                                
018900                                                                          
019000     03 WS-TIAAAAMMDD            PIC  9(8).                               
019100     03 WS-TIAAAAMMDD-DELAR      REDEFINES WS-TIAAAAMMDD.                 
019200        05 WS-TIAA               PIC  9(2).                               
019300        05 WS-TIAAMMDD           PIC  9(6).                               
019400                                                                          
019500     03 WS-9KOMPL-GRUND          PIC 9(9) VALUE 999999999.                
019600                                                                          
019700 01  WS-TIMESTAMP.                                                        
019800   03 WS-AAAAMMDD-E              PIC 9(8)    VALUE ZERO.                  
019900   03 WS-TTMMSSTH-E              PIC 9(8)    VALUE ZERO.                  
020000   03 FILLER                     PIC X(10)   VALUE SPACE.                 
020100*                                                                         
020200       EJECT                                                              
020300 01  IDDC-USER.                                                           
020400     03  FILLER                  PIC X(5) VALUE 'WIDDC'.                  
020500     03  IDDC-XX                 PIC X(2).                                
020600     03  FILLER                  PIC X(1) VALUE SPACE.                    
020700*                                                                         
020800 77    WS-SLINGA-KLAR            PIC X(1).                                
020900   88  SLINGA-KLAR                          VALUE 'J'.                    
021000                                                                          
021100 77  SW-TIKLAR-UPPDATERAD        PIC X(01).                               
021200                                                                          
021300     SKIP2                                                                
021400 77    WS-KDMFSFOR               PIC X(1)   VALUE SPACE.                  
021500   88  SWEDISH-TEXT                         VALUE '1'.                    
021600   88  ENGLISH-TEXT                         VALUE '2'.                    
021700     SKIP2                                                                
021800*                                                                         
021900 77  FILLER                      PIC X(16)  VALUE 'FROM TRANS:'.          
022000*                                                                         
022100 77    WS-IDTRANS                PIC X(04).                               
022200   88  WS-SAMMA-BILD                        VALUE '4397'.                 
022300   88  WS-GODKAND-BILD                      VALUE '4318'                  
022400                                                  '431A'                  
022500                                                  '431B'                  
022600                                                  '4355'                  
022700                                                  '4359'                  
022800                                                  '4397'                  
022900                                                  'L123'                  
023000                                                  '4319'.                 
023100   88  CHECK-OUT-NOT-REPORTED-LINES         VALUE 'L123'                  
023200                                                  '4319'.                 
023300   88  PROCESS-BO-LINE-BILD                 VALUE 'L134'                  
023400                                                  '4375'.                 
023500     SKIP2                                                                
023600 77    WS-INDATA-TEST            PIC X(01).                               
023700   88  WS-INDATA-FEL                        VALUE 'F'.                    
023800   88  WS-INDATA-RATT                       VALUE 'R'.                    
023900     SKIP2                                                                
024000 77    WS-BEHANDLING-TEST        PIC X(01).                               
024100   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
024200   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
024300     SKIP2                                                                
024400*    PARAMETRAR FÖR EVENT                                                 
024500 77  EVENT-SW                     PIC X(4)   VALUE SPACE.                 
024600     88 EVENT-OK                             VALUE 'LYNB'                 
024700                                                   'LYND'                 
024800                                                   'LYNV'                 
024900                                                   'LYNK'                 
025000                                                   'TADB'                 
025100                                                   'TADD'                 
025200                                                   'TADV'                 
025300                                                   'TAD '                 
025400                                                   'POLE'                 
025410                                                   'ACC '                 
025420                                                   'APA '                 
025430                                                   'APB '                 
025440                                                   'APC '                 
025450                                                   'APD '                 
025460                                                   'APE '                 
025470                                                   'APF '                 
025480                                                   'APG '                 
025490                                                   'APH '                 
025491                                                   'API '                 
025492                                                   'APJ '                 
025510                                                   'ECOM'.                
025600                                                                          
025700 77  CREATE-EVENT-SW              PIC X(1)   VALUE 'N'.                   
025800   88 CREATE-EVENT                           VALUE 'J'.                   
025900                                                                          
026000 01  WS-IDEVENTORDREF.                                                    
026100     03 WS-IDDISTR-EVENT         PIC 9(4).                                
026200     03 WS-IDKUNDNR-EVENT        PIC 9(6).                                
026300     03 WS-IDORDNR7-EVENT        PIC 9(7).                                
026400     03 WS-TIREGDAT-EVENT        PIC 9(6).                                
026500                                                                          
026510 01  WS-ABENDDATA.                                                        
026520     03 WS-IDPRODNR-ABEND        PIC X(7).                                
026530     03 WS-IDANSTNR-ABEND        PIC X(5).                                
026540     03 WS-IDPLKLST-ABEND        PIC X(3).                                
026550     03 WS-IDPURAD-ABEND         PIC X(5).                                
026551     03 WS-TEXT-ABEND            PIC X(15).                               
026552     03 WS-KORD-IDUSER           PIC X(5).                                
026560                                                                          
026600     SKIP2                                                                
026700 77    WS-TID                    PIC 9(8)   VALUE ZERO.                   
026800     SKIP2                                                                
026900 77  WS-VORD-FARDIGPACKAD        PIC X(1).                                
027000     SKIP2                                                                
027100 01    WS-TID-W.                                                          
027200   03  WS-TTMMSS                 PIC 9(6).                                
027300   03  WS-HH                     PIC 9(2).                                
027400   EJECT                                                                  
027500 01     WS-IDKUNDRF.                                                      
027600   03   WS-IDORDNR               PIC X(5)   VALUE SPACE.                  
027700   03   FILLER                   PIC X(5)   VALUE SPACE.                  
027800                                                                          
027900 01     WS-IDKUNDRF-OLD.                                                  
028000   03   WS-IDORDNR5-OLD          PIC 9(5).                                
028100   03   FILLER                   PIC X(5)   VALUE SPACE.                  
028200                                                                          
028300 01     WS-IDKUNDRF-NEW.                                                  
028400   03   WS-IDORDNR7-NEW          PIC 9(7).                                
028500   03   FILLER                   PIC X(3)   VALUE SPACE.                  
028600                                                                          
028700 01     WS-JFR-IDANSTNR.                                                  
028800   03   FILLER                    PIC X(3).                               
028900   03   WS-JFR-IDANSTNR-5         PIC X(5).                               
029000                                                                          
029100 01     WS-SUPTID-PRAPP           PIC 9(3)V99.                            
029200 01     FILLER REDEFINES WS-SUPTID-PRAPP.                                 
029300   03   WS-SUPTID-TIM             PIC 9(3).                               
029400   03   WS-SUPTID-MIN             PIC 9(2).                               
029500     EJECT                                                                
029600 01     FILLER                  PIC X(11)   VALUE 'HJALP-AREOR'.          
029700 01     HJALP-ODEL-TIRFS        PIC 9(11).                                
029800 01     FILLER                  REDEFINES HJALP-ODEL-TIRFS.               
029900   03   HJALP-ODEL-TIRFS-7      PIC  X(7).                                
030000   03   FILLER                  PIC  X(4).                                
030100     SKIP2                                                                
030200 01     HJALP-4472-TIRFS        PIC 9(11).                                
030300 01     FILLER                  REDEFINES HJALP-4472-TIRFS.               
030400   03   HJALP-4472-TIRFS-7      PIC  X(7).                                
030500   03   FILLER                  PIC  X(4).                                
030600                                                                          
030700 01 DB2-LASNING.                                                          
030800     03 FILLER                   PIC X(16)   VALUE                        
030900                                             'WS-DB2-SEKTION'.            
031000     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
031100                                                                          
031200     EJECT                                                                
031300 01 NYCKLAR-TP4TRAN.                                                      
031400     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
031500                                                                          
031600                                                                          
031700 01    DYNAMISKA-SUBPROGRAM.                                              
031800       03  WDATKONV             PIC X(8)    VALUE 'WDATKONV'.             
031900       03  CBLTDLI              PIC X(8)    VALUE 'CBLTDLI '.             
032000       03  FELLOG               PIC X(8)    VALUE 'FELLOG  '.             
032100       03  ABEND                PIC X(8)    VALUE 'ABEND   '.             
032200       03  W005INIT             PIC X(8)    VALUE 'W005INIT'.             
032300       03  W009CIA              PIC X(8)    VALUE 'W009CIA'.              
032400       03  WZ01SEND             PIC X(8)    VALUE 'WZ01SEND'.             
032500       03  W006KOM              PIC X(8)    VALUE 'W006KOM '.             
032600     SKIP3                                                                
032700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
032800 01    FILLER                   PIC X(16)   VALUE 'WMSGINIT '.            
032900*01 -COPY WMSGINIT                                                        
033000 01  FILLER                     PIC X(16)   VALUE 'W009CIA  '.            
033100*01  -COPY W009CIA                                                        
033200                                                                          
033300 01  FILLER                      PIC X(16)   VALUE '*WZ01SEND**'.         
033400*01  -COPY WZ01SEND                                                       
033500                                                                          
033600 01  HDR-AREA.                                                            
033700*    03  -COPY WZ01REQU                                                   
033800*    03  -COPY WZ04HDR                                                    
033900                                                                          
034000 01  FILLER                      PIC X(16)   VALUE '*W402TACD**'.         
034100*01  -COPY W402TACD                                                       
034200                                                                          
034300     SKIP3                                                                
034400                                                                          
034500 01  GEMENSAMMA-SUBPROGRAM.                                               
034600     03  W411DNOT               PIC X(8)    VALUE 'W411DNOT'.             
034700*        DATA TILL DEL NOTE NDC                                           
034800     03  W335PRQU               PIC X(8)    VALUE 'W335PRQU'.             
034900     03  W335PRNO               PIC X(8)    VALUE 'W335PRNO'.             
035000*        PRISFRÅGA                                                        
035100     03  W403NILP               PIC X(8)    VALUE 'W403NILP'.             
035200*        NIL PICK MAIL                                                    
035300*                                                                         
035400                                                                          
035500 01 FILLER                      PIC X(8)    VALUE 'W411DNOT'.             
035600*   -COPY W411DNOT                                                        
035700     EJECT                                                                
035800 01 FILLER                      PIC X(8)    VALUE 'W335PRQU'.             
035900*   -COPY W335PRQU                                                        
036000     EJECT                                                                
036100 01 FILLER                      PIC X(8)    VALUE 'W335PRNO'.             
036200*   -COPY W335PRNO                                                        
036300     EJECT                                                                
036400 01 FILLER                      PIC X(8)    VALUE 'W403NILP'.             
036500*   -COPY W403NILP                                                        
036600     EJECT                                                                
036700                                                                          
036800 01    ABENDKODER.                                                        
036900       03  FILLER               PIC X(16) VALUE 'ABENDKODER'.             
037000       03  RKOD-ABEND-UTAN-DUMP PIC S9(4) COMP SYNC VALUE +16.            
037100       03  RKOD-ABEND-MED-DUMP  PIC S9(4) COMP SYNC VALUE +33.            
037200       03  RKOD-FELTEXT         PIC X(32) VALUE SPACE.                    
037300     EJECT                                                                
037400                                                                          
037500 01    TEST-IDDISTR              PIC 9(5)         COMP-3.                 
037600*01    FILLER  -COPY WWDIST03      -RED TEST-IDDISTR.                     
037700     EJECT                                                                
037800*01    FILLER  -COPY WWDIST07      -RED TEST-IDDISTR.                     
037900     EJECT                                                                
038000*01    FILLER  -COPY WWDIST18      -RED TEST-IDDISTR.                     
038100     EJECT                                                                
038200*01    FILLER  -COPY WWDIST19      -RED TEST-IDDISTR.                     
038300     EJECT                                                                
038400*01    FILLER  -COPY WWDIST20      -RED TEST-IDDISTR.                     
038500     EJECT                                                                
038600*01    FILLER  -COPY WWDIST35      -RED TEST-IDDISTR.                     
038700     EJECT                                                                
038800*01    FILLER  -COPY WWDIS103      -RED TEST-IDDISTR.                     
038900     EJECT                                                                
039000*    ----DISTR-DEALER-PRICE-----                                          
039100*01    FILLER  -COPY WWDIST79      -RED TEST-IDDISTR.                     
039200 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
039300*   -COPY WWDIST57                                                        
039400     EJECT                                                                
039500                                                                          
039600 01     DAGDAT.                                                           
039700   03   DAGDAT-AAMMD             PIC 9(6).                                
039800   03   DAGDAT-AAMMDD-X      REDEFINES DAGDAT-AAMMD.                      
039900     05 DAGDAT-AAMMDD-AA         PIC 9(2).                                
040000     05 DAGDAT-AAMMDD-MM         PIC 9(2).                                
040100     05 DAGDAT-AAMMDD-DD         PIC 9(2).                                
040200*                                                                         
040300   03   DAGDAT-AAVVD             PIC 9(5).                                
040400   03   DAGDAT-AAVVD-X       REDEFINES DAGDAT-AAVVD.                      
040500     05 DAGDAT-AAVVD-AA          PIC 9(2).                                
040600     05 DAGDAT-AAVVD-VV          PIC 9(2).                                
040700     05 DAGDAT-AAVVD-D           PIC 9(1).                                
040800     EJECT                                                                
040900*01  WDATAREA      -COPY WDATAREA.                                        
041000     EJECT                                                                
041100 01     FILLER                  PIC X(10)   VALUE 'FLAGGOR   '.           
041200*                                                                         
041300 01    FL-420-SEGMENT            PIC X(01)  VALUE 'N'.                    
041400   88  ORAPPORTERADE-RADER-FINNS            VALUE 'J'.                    
041500   88  ORAPPORTERADE-RADER-SAKNAS           VALUE 'N'.                    
041600     SKIP2                                                                
041700 01    WS-FLAUTFAK               PIC X(01).                               
041800   88  AUT-FAK-SKRIVS-EJ-UT                 VALUE 'N'.                    
041900   88  AUT-FAKTURA-SKRIVS-UT                VALUE 'J'.                    
042000     SKIP2                                                                
042100 01    FL-LASNINGSTRANS          PIC X(01).                               
042200   88  LASNINGSTRANS-TAS-EJ-BORT            VALUE 'N'.                    
042300   88  LASNINGSTRANS-TAS-BORT               VALUE 'J'.                    
042400     SKIP2                                                                
042500 01    FL-PACKADEORDERL          PIC X(01).                               
042600   88  SKRIV-EJ-PACKADEORDERL               VALUE 'N'.                    
042700   88  SKRIV-PACKADEORDERL                  VALUE 'J'.                    
042800     SKIP2                                                                
042900 01    KDORDSTA-SW               PIC X(01).                               
043000   88  KDORDSTA-KLAR                        VALUE 'J'.                    
043100   88  KDORDSTA-EJ-KLAR                     VALUE 'N'.                    
043200*                                                                         
043300 01  SW-UPD-KOLLI-REG            PIC X      VALUE ' '.                    
043400     88  SW-UPD-KOLLI-REG-JA                VALUE 'J'.                    
043500     88  SW-UPD-KOLLI-REG-NEJ               VALUE 'N'.                    
043600*                                                                         
043610 01  SW-LYNK-NON-API             PIC X(1)   VALUE 'N'.                    
043620     88 LYNK-NON-API                        VALUE 'J'.                    
043630                                                                          
043631 01  SW-VOR                      PIC X(1)   VALUE 'N'.                    
043632     88 VOR                                 VALUE 'J'.                    
043633                                                                          
043640                                                                          
043650                                                                          
043700 01  TRAFF-VORKO-SW              PIC X(1)    VALUE 'N'.                   
043800   88 TRAFF-VORKO                            VALUE 'J'.                   
043900     EJECT                                                                
044000 01     FILLER                  PIC X(11)   VALUE 'ARBETSAREOR'.          
044100 01     ARBETSAREOR.                                                      
044200   03   ARB-AREA-RAD.                                                     
044300     05 ARB-RAD-FOM             PIC  9(4)         VALUE ZERO.             
044400     05 ARB-RAD-TOM             PIC  9(4)         VALUE ZERO.             
044500     05 ARB-KVLEVART            PIC  9(6)         VALUE ZERO.             
044600     SKIP2                                                                
044700 01     FILLER                  PIC X(10)   VALUE 'SPAR-AREA '.           
044800 01     SPAR-AREA.                                                        
044900*                                                                         
045000   03   SPAR-AREA.                                                        
045100     05 SPAR-KVORDRAD           PIC  S9(5)        VALUE ZERO.             
045200     05 SPAR-KVORDRAD-PACK      PIC  S9(5)        VALUE ZERO.             
045300     05 SPAR-IDDISTR            PIC  S9(5)        VALUE ZERO.             
045400     05 SPAR-IDKUNDNR           PIC  S9(7)        VALUE ZERO.             
045500     05 SPAR-KDFRAKT            PIC  S9(3)        VALUE ZERO.             
045600     05 SPAR-KDFAKTYP           PIC  X(1)         VALUE SPACE.            
045700     05 SPAR-VKORDNTO           PIC  9(6)V9(1)    VALUE ZERO.             
045800*    05 SPAR-VKORDNTO-TOT       PIC  9(6)V9(1)    VALUE ZERO.             
045900     05 SPAR-VKORDNTO-DEL       PIC  9(6)V9(1)    VALUE ZERO.             
046000     05 SPAR-VLORDNTO           PIC  9(4)V9(3)    VALUE ZERO.             
046100*    05 SPAR-VLORDNTO-TOT       PIC  9(4)V9(3)    VALUE ZERO.             
046200     05 SPAR-VLORDNTO-DEL       PIC  9(4)V9(3)    VALUE ZERO.             
046300     05 SPAR-KVKOLLI            PIC  S9(5)        VALUE ZERO.             
046400     05 SPAR-KVKOLPAC           PIC  S9(5)        VALUE ZERO.             
046500     05 SPAR-KVKOLLI-FAKT       PIC  S9(5)        VALUE ZERO.             
046600     05 SPAR-KDORDKL            PIC  S9(1)        VALUE ZERO.             
046700     05 SPAR-SUORDV             PIC  S9(9)V9(2)   VALUE ZERO.             
046800     05 SPAR-SUORDV-LOC         PIC  S9(9)V9(2)   VALUE ZERO.             
046900     05 SPAR-SUORDV-LOCPREL     PIC  S9(9)V9(2)   VALUE ZERO.             
047000*    05 SPAR-SUORDV-TOT         PIC  S9(9)V9(2)   VALUE ZERO.             
047100*    05 SPAR-SUORDV-TOT-LOC     PIC  S9(9)V9(2)   VALUE ZERO.             
047200*    05 SPAR-SUORDV-TOT-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.             
047300     05 SPAR-SUORDV-DEL         PIC  S9(9)V9(2)   VALUE ZERO.             
047400     05 SPAR-SUORDV-DEL-LOC     PIC  S9(9)V9(2)   VALUE ZERO.             
047500     05 SPAR-SUORDV-DEL-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.             
047600     05 SPAR-IDKUNDRF           PIC X(10)         VALUE SPACE.            
047700     05 SPAR-BEVARREF           PIC X(10)         VALUE SPACE.            
047800     05 SPAR-BEKUNDRF           PIC X(15)         VALUE SPACE.            
047900     05 SPAR-IDPRODNR           PIC S9(7) COMP-3  VALUE ZERO.             
048000     05 SPAR-IDARTNR            PIC  9(9)         VALUE ZERO.             
048100     EJECT                                                                
048200 01    FILLER                   PIC X(20)  VALUE 'SPAR-ORAD-AREA'.        
048300*01      WDE411 -COPY WDE411    -PRE SPAR-.                               
048400     EJECT                                                                
048500 01    NYCKLAR-TILL-DLI.                                                  
048600*                                                                         
048700   03  W-WDQ2CSEQ-X.                                                      
048800       05  W-WDQ2CSEQ-IDGMTREF.                                           
048900           07  W-WDQ2CSEQ-IDDISTR                                         
049000                               PIC S9(5) COMP-3 VALUE +0.                 
049100           07  W-WDQ2CSEQ-IDKUNDNR                                        
049200                               PIC S9(7) COMP-3 VALUE +0.                 
049300           07  W-WDQ2CSEQ-IDKUNDRF                                        
049400                               PIC X(10)     VALUE '0000000   '.          
049500*                                                                         
049600   03    W-WDE4E1KY-MAX-X.                                                
049700     05    W-IDPRODNR-WDE4E-MAX  PIC S9(7) VALUE ZERO  COMP-3.            
049800     05    W-WDE4E1-MAX          PIC X(19)   VALUE HIGH-VALUE.            
049900                                                                          
050000   03    W-WDE4E1KY-MIN-X.                                                
050100     05    W-IDPRODNR-WDE4E-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
050200     05    W-WDE4E1-MIN          PIC X(19)   VALUE LOW-VALUE.             
050300                                                                          
050400*                                                                         
050500     03  W-WDE4F1KY-MIN-X.                                                
050600         05  W-IDPRODNR-MIN      PIC S9(7)   COMP-3 VALUE ZERO.           
050700         05  W-IDKOLLI-MIN       PIC S9(5)   COMP-3 VALUE ZERO.           
050800         05  FILLER              PIC X(22)   VALUE  LOW-VALUE.            
050900                                                                          
051000     03  W-WDE4F1KY-MAX-X.                                                
051100         05  W-IDPRODNR-MAX      PIC S9(7)   COMP-3 VALUE ZERO.           
051200         05  W-IDKOLLI-MAX       PIC S9(5)   COMP-3 VALUE ZERO.           
051300         05  FILLER              PIC X(22)   VALUE  HIGH-VALUE.           
051400                                                                          
051500     03  W-IDPURAD-MIN-X.                                                 
051600         05  W-IDPURAD-MIN       PIC S9(5)   COMP-3 VALUE ZERO.           
051700                                                                          
051800     03  W-IDPURAD-MAX-X.                                                 
051900         05  W-IDPURAD-MAX       PIC S9(5)   COMP-3 VALUE 99999.          
052000*                                                                         
052100     EJECT                                                                
052200   03    W-WDE401-KUNDORDER-X.                                            
052300     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
052400     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
052500     05    W-401-IDKUNDRF.                                                
052600       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
052700       07  FILLER                PIC X(05)   VALUE SPACE.                 
052800     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
052900     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
053000*                                                                         
053100   03  W-IDPLKLST-X.                                                      
053200     05  W-IDPLKLST-E4      PIC S9(3)   COMP-3 VALUE ZERO.                
053300*                                                                         
053400   03    W-WDE401KY-MAX-X.                                                
053500     05    W-401-IDDISTR-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
053600     05    W-401-IDKUNDNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
053700     05    W-401-IDKUNDRF-MAX.                                            
053800       07 W-401-IDORDNR-MAX      PIC  9(5)   VALUE ZERO.                  
053900       07 FILLER                 PIC X(05)   VALUE SPACE.                 
054000     05    W-401-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
054100     05    W-401-IDPLKLST-MAX    PIC S9(3)   VALUE +999  COMP-3.          
054200*                                                                         
054300   03    W-WDE401-X.                                                      
054400     05    W-WDE401-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
054500*                                                                         
054600   03    W-WDE4B-KEYSEQ-X.                                                
054700     05    W-420-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
054800     05    W-420-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
054900*                                                                         
055000   03    W-WDE411-IDPURAD-X.                                              
055100     05    W-WDE411-IDPURAD      PIC S9(5)   VALUE ZERO  COMP-3.          
055200*                                                                         
055300   03    W-WDE4B-KEYSEQ-MIN-X.                                            
055400     05    W-420-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
055500     05    W-420-IDPURAD-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
055600*                                                                         
055700   03    W-WDE4B-KEYSEQ-MAX-X.                                            
055800     05    W-420-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
055900     05    W-420-IDPURAD-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
056000*                                                                         
056100   03    W-WDE601-IDPRODNR-X.                                             
056200     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
056300*                                                                         
056400   03    W-IDKOLLI-E6-X.                                                  
056500     05    W-IDKOLLI-E6          PIC S9(5)   COMP-3 VALUE ZERO.           
056600*                                                                         
056700     03  W-KDKOLSTA-X.                                                    
056800         05  W-KDKOLSTA          PIC S9(1)   COMP-3 VALUE +0.             
056900*                                                                         
057000   03    W-WDQ301-KEY-X.                                                  
057100     05    W-WDQ301-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
057200     05    W-WDQ301-IDDC         PIC X(2).                                
057300     05    W-WDQ301-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
057400     05    W-WDQ301-IDPLKLST     PIC S9(3)   VALUE ZERO  COMP-3.          
057500                                                                          
057600   03  W-Q301-KEY-MIN-X.                                                  
057700         05  W-Q301-MIN-IDORDER  PIC S9(7)   COMP-3.                      
057800         05  W-Q301-MIN-IDDC     PIC X(2).                                
057900         05  W-Q301-MIN-IDPRODNR PIC S9(7)   COMP-3.                      
058000         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
058100                                                                          
058200   03  W-Q301-KEY-MAX-X.                                                  
058300         05  W-Q301-MAX-IDORDER  PIC S9(7)   COMP-3.                      
058400         05  W-Q301-MAX-IDDC     PIC X(2).                                
058500         05  W-Q301-MAX-IDPRODNR PIC S9(7)   COMP-3.                      
058600         05  FILLER              PIC X(2)    VALUE HIGH-VALUE.            
058700                                                                          
058800     03  W-WDQ301KY-MIN.                                                  
058900         05  W-Q301KY-MIN-IDORDER    PIC S9(7)  COMP-3.                   
059000         05  W-Q301KY-MIN-IDDC       PIC X(2).                            
059100         05  FILLER                  PIC X(6)   VALUE LOW-VALUE.          
059200                                                                          
059300     03  W-WDQ301KY-MAX.                                                  
059400         05  W-Q301KY-MAX-IDORDER    PIC S9(7)  COMP-3.                   
059500         05  W-Q301KY-MAX-IDDC       PIC X(2).                            
059600         05  FILLER                  PIC X(6)   VALUE HIGH-VALUE.         
059700                                                                          
059800     03  W-WDQ3DSEQ-MIN-X.                                                
059900         05  W-Q3DSEQ-IDPRODNR-MIN   PIC S9(7) VALUE ZERO COMP-3.         
060000         05  W-Q3DSEQ-IDPLKLST-MIN   PIC S9(3) VALUE ZERO COMP-3.         
060100                                                                          
060200     03  W-WDQ3DSEQ-MAX-X.                                                
060300         05  W-Q3DSEQ-IDPRODNR-MAX   PIC S9(7) VALUE ZERO COMP-3.         
060400         05  W-Q3DSEQ-IDPLKLST-MAX   PIC S9(3) VALUE ZERO COMP-3.         
060500                                                                          
060600     03    W-WDE4ASEQ-X.                                                  
060700      05   W-E4ASEQ-IDGMTREF         PIC X(17)  VALUE SPACE.              
060800*                                                                         
060900     03  W-KDODELST                  PIC X.                               
061000*                                                                         
061100     03  W-WDQ101KY-MIN-X.                                                
061200         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
061300         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
061400         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
061500         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
061600         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
061700                                                                          
061800     03  W-WDQ101KY-MAX-X.                                                
061900         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
062000         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
062100         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
062200         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
062300         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
062400*                                                                         
062500     03  W-IDORDER-X.                                                     
062600         05  W-IDORDER-Q2        PIC S9(7)   COMP-3.                      
062700     SKIP2                                                                
062710     03  W-IDDC-X.                                                        
062720         05  W-IDDC-Q2           PIC XX.                                  
062730     SKIP2                                                                
062800   03    W-WDA601KY-MIN-X.                                                
062900     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
063000     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
063100     05    W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.               
063200     05    FILLER REDEFINES W-A601KY-MIN-IDKUNDRF.                        
063300        07  W-A601KY-MIN-IDORDNR     PIC 9(07).                           
063400        07  FILLER                   PIC X(03).                           
063500     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
063600     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
063700     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
063800     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
063900     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
064000     SKIP2                                                                
064100   03    W-WDA601KY-MAX-X.                                                
064200     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
064300     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
064400     05    W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.               
064500     05    FILLER REDEFINES W-A601KY-MAX-IDKUNDRF.                        
064600        07  W-A601KY-MAX-IDORDNR     PIC 9(07).                           
064700        07  FILLER                   PIC X(03).                           
064800     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
064900     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
065000     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
065100     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
065200     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
065300*                                                                         
065400   03    W-WDA6BSEQ-MIN-X.                                                
065500     05    W-A6BSEQ-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
065600     05    W-A6BSEQ-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
065700     05    W-A6BSEQ-MIN-IDKUNDRF-LEV PIC X(10) VALUE SPACE.               
065800     05    W-A6BSEQ-MIN-TIREGDAT-LEV PIC S9(7) VALUE ZERO COMP-3.         
065900     05    W-A6BSEQ-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
066000     05    FILLER                    PIC X(14) VALUE SPACE.               
066100     SKIP2                                                                
066200   03    W-WDA6BSEQ-MAX-X.                                                
066300     05    W-A6BSEQ-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
066400     05    W-A6BSEQ-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
066500     05    W-A6BSEQ-MAX-IDKUNDRF-LEV PIC X(10) VALUE SPACE.               
066600     05    W-A6BSEQ-MAX-TIREGDAT-LEV PIC S9(7) VALUE ZERO COMP-3.         
066700     05    W-A6BSEQ-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
066800     05    FILLER                    PIC X(14) VALUE SPACE.               
066900     SKIP2                                                                
067000     03  W-4301-WDGXKEY-X.                                                
067100         05 W-4301-IDHTYP        PIC X(4)  VALUE '4301'.                  
067200         05 W-4301-IDPRODNR      PIC S9(7) VALUE ZERO COMP-3.             
067300         05 W-4301-NYCKEL-VALFRI PIC X(22) VALUE LOW-VALUE.               
067400*                                                                         
067500     03  W-4302-WDGXKEY-X.                                                
067600         05 W-4302-IDKOLLI-X.                                             
067700            07 W-4302-IDKOLLI    PIC S9(5) VALUE ZERO COMP-3.             
067800         05 W-4302-IDPLKLST-X.                                            
067900            07 W-4302-IDPLKLST   PIC S9(3) VALUE ZERO COMP-3.             
068000                                                                          
068100   03    W-2203-X.                                                        
068200     05    FILLER                PIC X(4) VALUE '2203'.                   
068300     05    W-2203-IDDC           PIC X(2).                                
068400     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
068500*                                                                         
068600   03    W-4305-X.                                                        
068700     05    FILLER                PIC X(4) VALUE '4305'.                   
068800     05    W-XXDJ-IDDC           PIC X(2).                                
068900     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
069000*                                                                         
069100   03    W-4306-X.                                                        
069200     05    W-XXDJ-IDPRODNR       PIC S9(7) COMP-3.                        
069300     05    FILLER                PIC X(6)  VALUE LOW-VALUE.               
069400*                                                                         
069500   03    W-4311-X.                                                        
069600     05    FILLER                PIC X(4) VALUE '4311'.                   
069700     05    W-XXDK-IDDC           PIC X(2).                                
069800     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
069900*                                                                         
070000   03    W-4312-X.                                                        
070100     05    W-XXDK-IDPRODNR       PIC S9(7) VALUE ZERO COMP-3.             
070200     05    FILLER                PIC X(6)  VALUE LOW-VALUE.               
070300*                                                                         
070400   03    W-4315-X.                                                        
070500     05    FILLER                PIC X(4)  VALUE '4315'.                  
070600     05    FILLER                PIC X(26) VALUE LOW-VALUE.               
070700*                                                                         
070800   03    W-4316-X-MIN.                                                    
070900     05    W-XXDL-IDPRODNR-MIN   PIC S9(7) VALUE ZERO COMP-3.             
071000     05    W-XXDL-IDPTYP-MIN     PIC X(3)  VALUE SPACE.                   
071100     05    FILLER                PIC X(13) VALUE LOW-VALUE.               
071200*                                                                         
071300   03    W-4316-X-MAX.                                                    
071400     05    W-XXDL-IDPRODNR-MAX   PIC S9(7) VALUE ZERO COMP-3.             
071500     05    W-XXDL-IDPTYP-MAX     PIC X(3)  VALUE SPACE.                   
071600     05    FILLER                PIC X(13) VALUE HIGH-VALUE.              
071700*                                                                         
071800   03    W-4726-X.                                                        
071900     05    FILLER                PIC X(4)  VALUE '4726'.                  
072000     05    W-XXDV-FLBATCH        PIC X(1)  VALUE SPACE.                   
072100     05    FILLER                PIC X(25) VALUE LOW-VALUE.               
072200     EJECT                                                                
072300   03    W-IDARTNR-X.                                                     
072400     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
072500*                                                                         
072600   03    W-KDSEGKEY-X.                                                    
072700     05    W-KDSEGKEY            PIC X(1)    VALUE '1'.                   
072800*                                                                         
072900   03    W-WDK711-IDDC-X.                                                 
073000     05    W-711-IDDC            PIC X(2).                                
073100*                                                                         
073200   03    W-IDLAND-X.                                                      
073300     05    W-IDLAND              PIC X(2).                                
073400*                                                                         
073500   03    W-WDK901-IDARTNR-X.                                              
073600     05    W-901-IDARTNR         PIC S9(9)   VALUE ZERO  COMP-3.          
073700*                                                                         
073800   03    W-WDG6KEY-X.                                                     
073900     05    W-RDG-TIAAMMDD        PIC 9(6)    VALUE ZERO.                  
074000     05    W-RDG-TIKLOCK         PIC 9(8)    VALUE ZERO.                  
074100     05    W-RDG-IDLOGLOP        PIC 9(1)    VALUE ZERO.                  
074200     05    W-RDG-IDPTYP          PIC X(3)    VALUE 'RY1'.                 
074300*                                                                         
074400   03    W-WDGX11-WDGXKEY-X.                                              
074500     05    W-RDG-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
074600     05    W-RDG-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
074700     05    W-RDG-IDDC            PIC X(2).                                
074800     05    W-RDG-KDFAKTYP        PIC X(1)    VALUE SPACE.                 
074900*                                                                         
075000   03    W-WDGX01.                                                        
075100     05    W-IDHTYP-N5           PIC X(04)   VALUE '4511'.                
075200     05    W-VALFRI-N5           PIC X(26)   VALUE SPACE.                 
075300*                                                                         
075400   03    W-WDGXKEY-N5-MIN.                                                
075500     05    FILLER                PIC X(10)   VALUE SPACE.                 
075600*                                                                         
075700   03    W-WDGXKEY-N5-MAX.                                                
075800     05    FILLER                PIC X(10)   VALUE SPACE.                 
075900*                                                                         
076000   03    W-KDRAPRIO-N5-MIN-X.                                             
076100     05    W-KDRAPRIO-N5-MIN     PIC S9(3) COMP-3 VALUE ZERO.             
076200*                                                                         
076300   03    W-KDRAPRIO-N5-MAX-X.                                             
076400     05    W-KDRAPRIO-N5-MAX     PIC S9(3) COMP-3 VALUE ZERO.             
076500*                                                                         
076600   03    W-KDTPOTYP-N5-X.                                                 
076700     05    W-KDTPOTYP-N5         PIC S9(1) COMP-3 VALUE ZERO.             
076800*                                                                         
076900   03    W-KDORDKL-N5-X.                                                  
077000     05    W-KDORDKL-N5          PIC S9(1) COMP-3 VALUE ZERO.             
077100*                                                                         
077200   03    W-IDDISTR-FOM-N5-X.                                              
077300     05    W-IDDISTR-FOM-N5      PIC S9(5) COMP-3 VALUE ZERO.             
077400*                                                                         
077500   03    W-IDDISTR-TOM-N5-X.                                              
077600     05    W-IDDISTR-TOM-N5      PIC S9(5) COMP-3 VALUE ZERO.             
078000     EJECT                                                                
078100   03    W1-WDA501KY-X.                                                   
078200     05    W1-IDDISTR             PIC S9(5)   COMP-3.                     
078300     05    W1-IDKUNDNR            PIC S9(7)   COMP-3.                     
078400     05    W1-IDKUNDRF            PIC X(10).                              
078500     05    W1-IDARTNR             PIC S9(9)   COMP-3.                     
078600     05    W1-IDLOPNR             PIC S9(3)   COMP-3.                     
078700*                                                                         
078800   03    W2-WDA501KY-X.                                                   
078900     05    W2-IDDISTR             PIC S9(5)   COMP-3.                     
079000     05    W2-IDKUNDNR            PIC S9(7)   COMP-3.                     
079100     05    W2-IDKUNDRF            PIC X(10).                              
079200     05    W2-IDARTNR             PIC S9(9)   COMP-3.                     
079300     05    W2-IDLOPNR             PIC S9(3)   COMP-3.                     
079400*                                                                         
079500   03  W-WDA501KY-A5-MIN-X.                                               
079600       05  W-IDDISTR-A5-MIN          PIC S9(5) VALUE ZERO COMP-3.         
079700       05  W-IDKUNDNR-A5-MIN         PIC S9(7) VALUE ZERO COMP-3.         
079800       05  FILLER                    PIC X(17) VALUE LOW-VALUE.           
079900                                                                          
080000   03  W-WDA501KY-A5-MAX-X.                                               
080100       05  W-IDDISTR-A5-MAX          PIC S9(5) VALUE ZERO COMP-3.         
080200       05  W-IDKUNDNR-A5-MAX         PIC S9(7) VALUE ZERO COMP-3.         
080300       05  FILLER                    PIC X(17) VALUE HIGH-VALUE.          
080400*                                                                         
080500   03  W-KDORDKL-X.                                                       
080600       05  W-KDORDKL                 PIC S9    VALUE ZERO COMP-3.         
080700                                                                          
080800   03    W-IDKUNDRF-LEV-X.                                                
080900     05  W-IDKUNDRF-LEV              PIC X(10)   VALUE SPACE.             
081000*                                                                         
081100   03    W-KDSTARAD-X.                                                    
081200     05    W-KDSTARAD             PIC X.                                  
081300*                                                                         
081400     EJECT                                                                
081500   03    W-4319-X.                                                        
081600     05    FILLER                 PIC X(4) VALUE '4319'.                  
081700     05    W-XXJD-TIREGDAT        PIC S9(7) COMP-3.                       
081800     05    FILLER                 PIC X(22) VALUE LOW-VALUE.              
081900*                                                                         
082000   03    W-4320-X.                                                        
082100     05    W-XXJD-IDDISTR         PIC S9(5) COMP-3.                       
082200     05    W-XXJD-IDKUNDNR        PIC S9(7) COMP-3.                       
082300     05    W-XXJD-IDORDNR         PIC S9(5) COMP-3.                       
082400     05    W-XXJD-IDPRODNR        PIC S9(7) COMP-3.                       
082500     05    W-XXJD-IDARTNR         PIC S9(9) COMP-3.                       
082600     05    W-XXJD-REKSIFFR        PIC S9    COMP-3.                       
082700     EJECT                                                                
082800   03    W-WDGXKEY-4541-X.                                                
082900     05    W-WDGXKEY4541          PIC X(4)  VALUE '4541'.                 
083000     05    FILLER                 PIC X(26) VALUE LOW-VALUE.              
083100*                                                                         
083200   03    W-WDGXKEY-4471-X.                                                
083300     05    W-4471-IDHTYP          PIC X(4)  VALUE '4471'.                 
083400     05    W-4471-IDDC            PIC X(2).                               
083500     05    W-4471-IDPRC.                                                  
083600        07 W-4471-IDPRCBAS        PIC X(3).                               
083700        07 W-4471-IDPRCVAR        PIC X(1).                               
083800     05    FILLER                 PIC X(20) VALUE LOW-VALUE.              
083900*                                                                         
084000   03    W-KDSEGKEY-4472-X.                                               
084100     05    W-4472-KDSEGKEY        PIC X(1)  VALUE '1'.                    
084200*                                                                         
084300   03    W-WDGXKEY-4477-X.                                                
084400     05    W-4477-IDHTYP          PIC X(4)  VALUE '4477'.                 
084500     05    W-4477-IDDC            PIC X(2).                               
084600     05    FILLER                 PIC X(24) VALUE LOW-VALUE.              
084700*                                                                         
084800   03    W-WDGXKEY-4478-X.                                                
084900     05    W-4478-IDSHIFT         PIC X(1).                               
085000     05    W-4478-IDUSER          PIC X(8).                               
085100     05    FILLER                 PIC X(1)  VALUE LOW-VALUE.              
085200*                                                                         
085300     EJECT                                                                
085400   03    W-WDM201-X.                                                      
085500     05    W-KAMP-IDKAMPRF      PIC S9(07)   VALUE ZERO COMP-3.           
085600     05    W-KAMP-IDDC          PIC X(02)    VALUE SPACE.                 
085700                                                                          
085800   03    W-WDM211-X.                                                      
085900     05    W-KART-IDARTNR       PIC S9(09)   VALUE ZERO COMP-3.           
086000                                                                          
086100   03    W-WDM221-X.                                                      
086200     05    W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.             
086300     05    W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.             
086400     05    W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.             
086500     05    W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.             
086600     EJECT                                                                
086700*                                                                         
086800   03    W-4447-X.                                                        
086900     05    FILLER                PIC X(4)  VALUE '4447'.                  
087000     05    W-4447-IDDC           PIC X(2).                                
087100     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
087200*                                                                         
087300   03    W-4448-X.                                                        
087400     05    W-4448-IDPRC          PIC X(4).                                
087500     05    FILLER                PIC X(1)  VALUE LOW-VALUE.               
087600*                                                                         
087700   03    W-4487-X.                                                        
087800     05    FILLER                PIC X(4)  VALUE '4487'.                  
087900     05    W-4487-IDDC           PIC X(2).                                
088000     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
088100*                                                                         
088200   03    W-4488-X.                                                        
088300     05    W-4488-KDPRCGRP       PIC X(5).                                
088400*                                                                         
088500   03    W-4490-X.                                                        
088600     05    W-4490-DARFS          PIC 9(12).                               
088700     05    W-4490-IDPRODNR       PIC S9(7)  COMP-3.                       
088800     05    W-4490-IDPLKLST       PIC S9(3)  COMP-3.                       
088900                                                                          
089000     03  W-IDDC-B6-X.                                                     
089100         05 W-IDDC-B6                  PIC X(2).                          
089200                                                                          
089300     03  W-IDGMT-X.                                                       
089400         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
089500         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
089600                                                                          
089700   03  W-IDDISTR-P4-X.                                                    
089800       05 W-IDDISTR-P4             PIC S9(5)   VALUE ZERO  COMP-3.        
089900     EJECT                                                                
090000******************************************************************        
090100*                                                                *        
090200*                                                                *        
090300*                                                                *        
090400******************************************************************        
090500     SKIP3                                                                
090600*    --- AREOR FÖR HANTERING AV API                                       
090700*01  FILLER                      PIC X(16)  VALUE 'MSG-KOM-WMSG'.         
090800 01  KOM-IO-AREA.                                                         
090900     03  -COPY WMSGKOM                                                    
091000                                                                          
091100*01  FILLER                      PIC X(16)   VALUE 'Z430-REQU-A '.        
091200*01  -COPY WZ0430I1  -PRE Z430-                                           
091300*    03  -COPY WAPIORD  -RED Z430-REQU-EVENT-DATA -PRE Z430-              
091400                                                                          
091500     SKIP3                                                                
091600 01    FILLER                 PIC X(16) VALUE 'MID W4I39701 MID'.         
091700 01    FILLER                 PIC X(16) VALUE 'W4T397X 43181'.            
091800     SKIP3                                                                
091900*01    MID -COPY W4I39701.                                                
092000     EJECT                                                                
092100 01    FILLER                 PIC X(08) VALUE 'WMSGAREA'.                 
092200*01    -COPY WMSGAREA                                                     
092300     EJECT                                                                
092400*  03    MOD -COPY W4O39701  -RED MSG-AREA.                               
092500     EJECT                                                                
092600 01    FILLER                    PIC X(16)   VALUE 'ALT-IO-AREA'.         
092700 01    ALT-IO-AREA.                                                       
092800   03    ALT-LL                  PIC S9(4)   COMP  SYNC.                  
092900   03    ALT-Z1                  PIC X(1).                                
093000   03    ALT-Z2                  PIC X(1).                                
093100   03    ALT-TRANSKOD            PIC X(6)    VALUE SPACE.                 
093200   03    FILLER                  PIC X(2)    VALUE SPACE.                 
093300   03    FILLER                  PIC X(4)    VALUE '4316'.                
093400   03    ALT-KDMFSFOR            PIC X(1)    VALUE SPACE.                 
093500   03    ALT-AREA                PIC X(105)  VALUE SPACE.                 
093600     SKIP3                                                                
093700     EJECT                                                                
093800 01  FILLER                     PIC X(16) VALUE 'ALT2-IO-AREA'.           
093900 01  ALT2-IO-AREA.                                                        
094000                                                                          
094100  03     ALT2-LL                 PIC S9(4) COMP SYNC.                     
094200  03     ALT2-Z1                 PIC X(1)  VALUE LOW-VALUE.               
094300  03     ALT2-Z2                 PIC X(1)  VALUE LOW-VALUE.               
094400  03     ALT2-TRANSKOD           PIC X(8)  VALUE 'W2T191X '.              
094500  03     ALT2-IDTRANS            PIC X(4)  VALUE '4397'.                  
094600  03     ALT2-SPRAK              PIC X(1).                                
094700* 03     MID -COPY W2I19101   -PRE ALT2-                                  
094800     SKIP2                                                                
094900*                                                                         
095000 01  FILLER                     PIC X(16) VALUE 'ALT5108-IO-AREA'.        
095100 01    ALT5108-IO-AREA.                                                   
095200     03    ALT5108-LL           PIC S9(4) COMP SYNC VALUE +63.            
095300     03    ALT5108-Z1           PIC X     VALUE LOW-VALUE.                
095400     03    ALT5108-Z2           PIC X     VALUE LOW-VALUE.                
095500     03    ALT5108-TRANSKOD     PIC X(8)  VALUE 'W5T108X '.               
095600     03    ALT5108-IDTRANS      PIC X(4)  VALUE '4397'.                   
095700     03    ALT5108-KDMFSFOR     PIC X     VALUE SPACE.                    
095800     03    ALT5108-IDARTNR-IN   PIC X(9).                                 
095900     03    ALT5108-IDARTNR-UT   PIC X(9)  VALUE ZERO.                     
096000     03    ALT5108-IDPW-IN      PIC X(8).                                 
096100     03    ALT5108-IDPW-UT      PIC X(8)  VALUE SPACE.                    
096200     03    ALT5108-IDDC-IN      PIC X(2)  VALUE SPACE.                    
096300     03    ALT5108-IDDC-UT      PIC X(2)  VALUE SPACE.                    
096400     03    ALT5108-IDPRODNR     PIC 9(7)  VALUE ZERO.                     
096500     03    ALT5108-KDORDKL      PIC 9     VALUE ZERO.                     
096600                                                                          
096700     EJECT                                                                
096800*********** ******************************************************        
096900*                                                                         
097000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
097100*                                                                         
097200 01    IMS-WS.                                                            
097300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS*****'.         
097400     SKIP3                                                                
097500*                        **** STATUS-KOD FRÅN IMS                         
097900   03    STATUS-WS               PIC XX.                                  
098000     88    SEGMENT-FINNS                     VALUE '  '.                  
098100     88    ISRT-OK                           VALUE '  '.                  
098200     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
098300     88    SEGMENT-SLUT                      VALUE 'GB'.                  
098400     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
098500     SKIP3                                                                
098600   03    GODK-STATUSKODER.                                                
098700     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
098800     SKIP3                                                                
098900 01    SSA1                      PIC X(200).                              
099000 01    SSA2                      PIC X(160).                              
099100 01    SSA3                      PIC X(128).                              
099200 01    SSA4                      PIC X(128).                              
099300     EJECT                                                                
099400*                            DB2 FUNKTIONSKODER                           
099500 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
099600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
099700                                                                          
099800 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
099900 01  DB2-WS.                                                              
100000     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
100100         88  CURSOR-OK                       VALUE 000.                   
100200         88  RADER-FINNS                     VALUE 000.                   
100300         88  RADER-SAKNAS                    VALUE 100.                   
100400         88  ATKOMST-FEL                     VALUE 904.                   
100500     03  GODK-SQLCODEKODER.                                               
100600         05  GODK-SQLCODE OCCURS 5                                        
100700             INDEXED BY SQLCODE-IX PIC 9(3).                              
100800 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
100900     EJECT                                                                
101000*                            IMS FUNKTIONSKODER                           
101100*01    -COPY W0003                                                        
101200   03    ROLB                PIC X(4)    VALUE 'ROLB'.                    
101300     EJECT                                                                
101400*                            DLI INPUT-OUTPUT AREA                        
101500 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
101600*01  WLLOGA01  -COPY WDL901                                               
101700     SKIP3                                                                
101800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E411'.         
101900 01  DLI-IO-E411.                                                         
102000*  03    WDE411 -COPY WDE411                                              
102100     EJECT                                                                
102200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E601'.         
102300 01  DLI-IO-E601.                                                         
102400*  03    WDE601 -COPY WDE601                                              
102500     EJECT                                                                
102600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E611'.         
102700 01  DLI-IO-E611.                                                         
102800*  03    WDE611 -COPY WDE611                                              
102900     EJECT                                                                
103000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-DJ01'.         
103100 01  DLI-IO-DJ01.                                                         
103200*  03    WLXXDJ01 -COPY WDGX4305                                          
103300     EJECT                                                                
103400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-DJ11'.         
103500 01  DLI-IO-DJ11.                                                         
103600*  03    WLXXDJ11 -COPY WDGX4306                                          
103700     EJECT                                                                
103800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-DJ11'.         
103900 01  DLI-IO-DL01.                                                         
104000*  03    WLXXDL01 -COPY WDG201  -PRE G201-                                
104100     EJECT                                                                
104200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
104300 01    DLI-IO-AREA2.                                                      
104400   03    IO-AREA2                PIC X(416)  VALUE SPACE.                 
104500                                                                          
104600*  03    WLXXDL11 -COPY WDGX4316           -RED IO-AREA2.                 
104700     EJECT                                                                
104800*  03    WLXXDK11 -COPY WDGX4312           -RED IO-AREA2.                 
104900     EJECT                                                                
105000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
105100 01    DLI-IO-AREA3.                                                      
105200   03    IO-AREA3                PIC X(224)  VALUE SPACE.                 
105300     SKIP3                                                                
105400*  03    WDGX2204 -COPY WDGX2204             -RED IO-AREA3.               
105500     EJECT                                                                
105600*  03    WDGX4726 -COPY WDGX4726             -RED IO-AREA3.               
105700     SKIP2                                                                
105800*  03    WDGX4727 -COPY WDGX4727             -RED IO-AREA3.               
105900     EJECT                                                                
106000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
106100 01    DLI-IO-AREA4.                                                      
106200   03    IO-AREA4                PIC X(224)  VALUE SPACE.                 
106300     SKIP3                                                                
106400*  03  WDGZ01     -COPY WDGZ01  -PRE LOGGA-   -RED IO-AREA4.              
106500     EJECT                                                                
106600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA5'.        
106700 01    DLI-IO-AREA5.                                                      
106800   03    IO-AREA5                PIC X(160)  VALUE SPACE.                 
106900     SKIP3                                                                
107000*  03  WDGX4512   -COPY WDGX4512 -PRE STYR-  -RED IO-AREA5.               
107100     EJECT                                                                
107200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
107300 01  DLI-IO-AREA6.                                                        
107400*  03  WDA501     -COPY WDA501                                            
107500     EJECT                                                                
107600                                                                          
107700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA7'.        
107800 01    DLI-IO-AREA7.                                                      
107900*  03  WDA501     -COPY WDA501   -PRE OLD-                                
108000     EJECT                                                                
108100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA8'.        
108200 01    DLI-IO-AREA8.                                                      
108300   03    IO-AREA8                PIC X(900)  VALUE SPACE.                 
108400     SKIP3                                                                
108500*  03    WLARTC01 -COPY WDK601               -RED IO-AREA8.               
108600     SKIP3                                                                
108700*  03    WLARTC11 -COPY WDK611               -RED IO-AREA8.               
108800     EJECT                                                                
108900 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
109000 01    DLI-IO-AREA17.                                                     
109100*  03    WDA601   -COPY WDA601                                            
109200     SKIP3                                                                
109300 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA11'.        
109400 01    DLI-IO-AREA11.                                                     
109500   03    IO-AREA11               PIC X(32)   VALUE SPACE.                 
109600     SKIP3                                                                
109700*  03  WDGX4487   -COPY WDGX4487       -RED IO-AREA11.                    
109800     EJECT                                                                
109900*  03  WDGX4490   -COPY WDGX4490       -RED IO-AREA11.                    
110000     EJECT                                                                
110100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA12'.        
110200 01  DLI-IO-AREA12.                                                       
110300     03  IO-AREA12               PIC X(320)  VALUE SPACE.                 
110400     SKIP2                                                                
110500     03  WDE601 -COPY WDE601     -PRE WDE62- -RED IO-AREA12.              
110600     EJECT                                                                
110700     03  WLORQA01 -COPY WDQ301   -PRE ORQA2- -RED IO-AREA12.              
110800     EJECT                                                                
110900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA13'.        
111000 01  DLI-IO-AREA13.                                                       
111100     03  IO-AREA13               PIC X(1200) VALUE SPACE.                 
111200     SKIP2                                                                
111300     03  WLXXLB11 -COPY WDGX4478  -RED IO-AREA13.                         
111400     EJECT                                                                
111500     03  WLXXKW11 -COPY WDGX4472  -RED IO-AREA13.                         
111600     EJECT                                                                
111700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA14'.        
111800 01  DLI-IO-AREA14.                                                       
111900     03  IO-AREA14               PIC X(96) VALUE SPACE.                   
112000     SKIP2                                                                
112100     03  WLXXDU01 -COPY WDGX4301  -RED IO-AREA14.                         
112200     EJECT                                                                
112300     03  WLXXDU11 -COPY WDGX4302  -RED IO-AREA14.                         
112400     EJECT                                                                
112500 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA15'.        
112600 01    DLI-IO-AREA15.                                                     
112700   03    IO-AREA15               PIC X(512)  VALUE SPACE.                 
112800     SKIP3                                                                
112900*  03    WDK711   -COPY WDK711            -  -RED IO-AREA15.              
113000     EJECT                                                                
113100 01  FILLER                      PIC X(16)  VALUE 'WDK722AREA'.           
113200 01  DLI-IO-WDK722.                                                       
113300*    03  WDK722-AREA -COPY WDK722                                         
113400     EJECT                                                                
113500 01  FILLER                      PIC X(16)  VALUE 'WDK712AREA'.           
113600 01  DLI-IO-WDK712.                                                       
113700*    03  WDK712-AREA -COPY WDK712                                         
113800     EJECT                                                                
113900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
114000 01  DLI-IO-WDM211.                                                       
114100*    03 -COPY WDM211                                                      
114200     EJECT                                                                
114300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
114400 01  DLI-IO-WDM221.                                                       
114500*    03 -COPY WDM221                                                      
114600     EJECT                                                                
114700 01  FILLER                      PIC X(16)  VALUE 'WDE4E-AREA'.           
114800 01  WDE401-AREA -COPY WDE401 -PRE E4E-.                                  
114900     SKIP2                                                                
115000 01  FILLER                      PIC X(16)  VALUE 'WDE4E1-AREA'.          
115100 01  WDE4E1-AREA -COPY WDE4E1.                                            
115200     EJECT                                                                
115300 01  FILLER                      PIC X(16)  VALUE 'WDE401-AREA'.          
115400*01      WDE401-AREA  -COPY WDE401.                                       
115500     EJECT                                                                
115600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE421'.         
115700 01  DLI-IO-WDE421.                                                       
115800*    03  -COPY WDE421                                                     
115900     EJECT                                                                
116000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE4F1'.         
116100 01  DLI-IO-WDE4F1.                                                       
116200*    03  -COPY WDE4F1                                                     
116300     EJECT                                                                
116400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDQ3D'.          
116500 01  DLI-IO-WDQ3D.                                                        
116600*    03  -COPY WDQ301   -PRE WDQ3D-                                       
116700     EJECT                                                                
116800 01  FILLER                      PIC X(16)  VALUE 'WDQ301-AREA'.          
116900*01      WLORQA01 -COPY WDQ301.                                           
117000     EJECT                                                                
117100 01  FILLER                      PIC X(16)  VALUE 'WDQ101-AREA'.          
117200*01      WLORQM01 -COPY WDQ101.                                           
117300     EJECT                                                                
117400 01  FILLER                      PIC X(16)  VALUE 'WDK901-AREA'.          
117500*01      WLARTM01 -COPY WDK901.                                           
117600     EJECT                                                                
117700 01  FILLER                      PIC X(16)  VALUE 'WDGZRY1-AREA'.         
117800*01      WDGZRY1  -COPY WDGZRY1.                                          
117900     EJECT                                                                
118000 01  FILLER                      PIC X(16) VALUE 'WDGZRY1S-AREA'.         
118100*01      WDGZRY1S -COPY WDGZRY1S.                                         
118200     EJECT                                                                
118300 01  FILLER                      PIC X(16)  VALUE 'WDGZRY6-AREA'.         
118400*01      WDGZRY6  -COPY WDGZRY6.                                          
118500     EJECT                                                                
118600 01  FILLER                      PIC X(16)  VALUE 'WDGZRYK-AREA'.         
118700*01      WDGZRYK  -COPY WDGZRYK.                                          
118800     EJECT                                                                
118900 01  FILLER                      PIC X(16) VALUE 'WDGX4542-AREA'.         
119000*01      WDGX4542 -COPY WDGX4542.                                         
119100     EJECT                                                                
119200 01  FILLER                      PIC X(16)  VALUE 'WDQ201-AREA'.          
119300*01      -COPY WDQ201.                                                    
119400     EJECT                                                                
119500 01  FILLER                      PIC X(16)  VALUE 'WDQ212-AREA'.          
119600*01      -COPY WDQ212.                                                    
119700     EJECT                                                                
119800 01  FILLER                      PIC X(16) VALUE 'WDGX4448-AREA'.         
119900*01      WLXXKH11 -COPY WDGX4448.                                         
120000     SKIP2                                                                
120100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-XXJD'.         
120200*01    WLXXJD01 -COPY WDGX4319.                                           
120300     EJECT                                                                
120400*01    WLXXJD11 -COPY WDGX4320.                                           
120500     EJECT                                                                
120600 01    FILLER                 PIC X(16) VALUE 'MID W4I34201 MID'.         
120700     SKIP3                                                                
120800*01    MID -COPY W4I34201  -PRE 4342-.                                    
120900     SKIP2                                                                
121000                                                                          
121100 01    FILLER                 PIC X(16) VALUE 'MID W4I34901 MID'.         
121200     SKIP3                                                                
121300*01    MID -COPY W4I34901  -PRE 4349-.                                    
121400     SKIP2                                                                
121500*---MSG-AERA FÖR HOPP TILL 4349-UTSKRIFT DELIVERY NOTE NA                 
121600 01  FILLER                PIC X(16)  VALUE '4349-MSG-IO-AREA'.           
121700 01  4349-MSG-IO-AREA.                                                    
121800     03  4349-LL              PIC S9(4)  VALUE +748 COMP SYNC.            
121900     03  4349-Z1              PIC X.                                      
122000     03  4349-Z2              PIC X.                                      
122100     03  4349-TRANSKOD        PIC X(8)   VALUE 'W4T349X '.                
122200     03  4349-IDTRANS         PIC X(4)   VALUE '4397'.                    
122300     03  4349-SPRAK           PIC X.                                      
122400     03  4349-FILLER          PIC X(731).                                 
122500                                                                          
122600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
122700 01   DLI-IO-AREA-B601.                                                   
122800*     03  -COPY WDB601                                                    
122900     EJECT                                                                
123000 01  FILLER                PIC X(16)   VALUE 'WDB201-AREA'.               
123100 01  DLI-IO-AREA-WDB201.                                                  
123200*     03  -COPY WDB201                                                    
123300                                                                          
123400 01  FILLER               PIC X(16)   VALUE 'WDP4A1 AREA'.                
123500 01   DLI-IO-AREA-WDP4A1.                                                 
123600*     03  -COPY WDP4A1                                                    
123700     EJECT                                                                
123800 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
123900                                                                          
124000*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
124100     EJECT                                                                
124200     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
124300     EJECT                                                                
124400                                                                          
124500                                                                          
124600 LINKAGE SECTION.                                                         
124700*01    -COPY W0009     -PRE MSG-                                          
124800     SKIP2                                                                
124900 01  0693-PCB     PIC X.                                                  
125000*01    -COPY W0009     -PRE ALT42-                                        
125100     SKIP2                                                                
125200*01    -COPY W0009     -PRE ALT49-                                        
125300     SKIP2                                                                
125400*01    -COPY W0009     -PRE ALT97-                                        
125500     SKIP2                                                                
125600*01    -COPY W0009     -PRE ALT2-                                         
125700     SKIP2                                                                
125800*01    -COPY W0009     -PRE ALT5108-                                      
125900     SKIP2                                                                
126000*01    -COPY W0009     -PRE MAIL-                                         
126100     SKIP2                                                                
126200*01    -COPY W0009     -PRE DISTRDOC-                                     
126300     SKIP2                                                                
126400*01    -COPY W0008     -PRE USEA-                                         
126500     05  FILLER                  PIC X.                                   
126600     SKIP2                                                                
126700*01    -COPY W0008     -PRE WDE4-                                         
126800     05  FILLER                  PIC X.                                   
126900     SKIP2                                                                
127000*01    -COPY W0008     -PRE WDE42-                                        
127100     05  FILLER                  PIC X.                                   
127200     SKIP2                                                                
127300*01    -COPY W0008     -PRE WDE43-                                        
127400     05  FILLER                  PIC X.                                   
127500     SKIP2                                                                
127600*01    -COPY W0008     -PRE WDE4E-                                        
127700     05  FILLER                  PIC X.                                   
127800     SKIP2                                                                
127900*01    -COPY W0008     -PRE WDE4F-                                        
128000     05  FILLER                  PIC X.                                   
128100     SKIP2                                                                
128200*01    -COPY W0008     -PRE WDE6-                                         
128300     05  FILLER                  PIC X.                                   
128400     SKIP2                                                                
128500*01    -COPY W0008     -PRE ZZAC-                                         
128600     05  FILLER                  PIC X.                                   
128700     SKIP2                                                                
128800*01    -COPY W0008     -PRE ARTC-                                         
128900     05  FILLER                  PIC X.                                   
129000     SKIP2                                                                
129100*01    -COPY W0008     -PRE XXDJ-                                         
129200     05  FILLER                  PIC X.                                   
129300     SKIP2                                                                
129400*01    -COPY W0008     -PRE XXDK-                                         
129500     05  FILLER                  PIC X.                                   
129600     SKIP2                                                                
129700*01    -COPY W0008     -PRE XXDL-                                         
129800     05  FILLER                  PIC X.                                   
129900     SKIP2                                                                
130000*01    -COPY W0008     -PRE AUTF-                                         
130100     05  FILLER                  PIC X.                                   
130200     SKIP2                                                                
130300*01    -COPY W0008     -PRE ORDP1-                                        
130400     05  FILLER                  PIC X.                                   
130500     SKIP2                                                                
130600*01    -COPY W0008     -PRE ORDP2-                                        
130700     05  FILLER                  PIC X.                                   
130800     SKIP2                                                                
130900*01    -COPY W0008     -PRE XXJN-                                         
131000     05  FILLER                  PIC X.                                   
131100     SKIP2                                                                
131200*01    -COPY W0008     -PRE XXJD-                                         
131300     05  FILLER                  PIC X.                                   
131400     SKIP2                                                                
131500*01    -COPY W0008     -PRE ORQA-                                         
131600     05  FILLER                  PIC X.                                   
131700     SKIP2                                                                
131800*01    -COPY W0008     -PRE ARTM-                                         
131900     05  FILLER                  PIC X.                                   
132000     SKIP2                                                                
132100*01    -COPY W0008     -PRE ORQM-                                         
132200     05  FILLER                  PIC X.                                   
132300     SKIP2                                                                
132400*01    -COPY W0008     -PRE 4541-                                         
132500     05  FILLER                  PIC X.                                   
132600     SKIP2                                                                
132700*01    -COPY W0008     -PRE 4487-                                         
132800     05  FILLER                  PIC X.                                   
132900     SKIP2                                                                
133000*01    -COPY W0008     -PRE WDE62-                                        
133100     05  FILLER                  PIC X.                                   
133200     SKIP2                                                                
133300*01    -COPY W0008     -PRE ORQA2-                                        
133400     05  FILLER                  PIC X.                                   
133500     SKIP2                                                                
133600*01    -COPY W0008     -PRE XXKW-                                         
133700     05  FILLER                  PIC X.                                   
133800     SKIP2                                                                
133900*01    -COPY W0008     -PRE XXLB-                                         
134000     05  FILLER                  PIC X.                                   
134100     SKIP2                                                                
134200*01    -COPY W0008     -PRE WDM2-                                         
134300     05  FILLER                  PIC X.                                   
134400     SKIP2                                                                
134500*01    -COPY W0008     -PRE ORQI-                                         
134600     05  FILLER                  PIC X.                                   
134700     SKIP2                                                                
134800*01    -COPY W0008     -PRE ORQICSQ-                                      
134900     05  FILLER                  PIC X.                                   
135000     SKIP2                                                                
135100*01    -COPY W0008     -PRE XXKH-                                         
135200     05  FILLER                  PIC X.                                   
135300     SKIP2                                                                
135400*01    -COPY W0008     -PRE WDE4A-                                        
135500     05  FILLER                  PIC X.                                   
135600     SKIP2                                                                
135700*01    -COPY W0008     -PRE XXDU-                                         
135800     05  FILLER                  PIC X.                                   
135900     SKIP2                                                                
136000*01    -COPY W0008     -PRE WDK7-                                         
136100     05  FILLER                  PIC X.                                   
136200     EJECT                                                                
136300*01    -COPY W0008     -PRE WLLOGA-                                       
136400     05  FILLER                  PIC X.                                   
136500     EJECT                                                                
136600*01    -COPY W0008     -PRE WDK6-                                         
136700     05  FILLER                  PIC X.                                   
136800     EJECT                                                                
136900*01    -COPY W0008     -PRE WDA6B-                                        
137000     05  FILLER                  PIC X.                                   
137100     EJECT                                                                
137200*01    -COPY W0008     -PRE WDA6-                                         
137300     05  FILLER                  PIC X.                                   
137400     EJECT                                                                
137500*01    -COPY W0008     -PRE WDB6-                                         
137600     05  FILLER                  PIC X.                                   
137700     EJECT                                                                
137800*01    -COPY W0008     -PRE WDB2-                                         
137900     05  FILLER                  PIC X.                                   
138000*01    -COPY W0008     -PRE WDQ3D-                                        
138100     05  FILLER                  PIC X.                                   
138200*01    -COPY W0008     -PRE WDP4A-                                        
138300     05  FILLER                  PIC X.                                   
138400                                                                          
138500 01  DNOT-ORQP-PCB               PIC X.                                   
138600 01  DNOT-ORQP2-PCB              PIC X.                                   
138700 01  DNOT-ORQP3-PCB              PIC X.                                   
138800 01  DNOT-4013-PCB               PIC X.                                   
138900 01  DNOT-BENA-PCB               PIC X.                                   
139000                                                                          
139100                                                                          
139200 01  PRQU-WDG2-PCB               PIC X.                                   
139300 01  PRQU-WDC7-PCB               PIC X.                                   
139400 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
139500                                                                          
139600 01  PRNO-3107-PCB               PIC X.                                   
139700     EJECT                                                                
139800*                                                                         
139900 01  KOM-WDP8-PCB                PIC X.                                   
140000*                                                                         
140100 PROCEDURE DIVISION USING MSG-PCB 0693-PCB                                
140200         ALT42-PCB ALT49-PCB                                              
140300         ALT97-PCB ALT2-PCB ALT5108-PCB MAIL-PCB DISTRDOC-PCB             
140400         USEA-PCB WDE4-PCB WDE42-PCB WDE43-PCB WDE4E-PCB WDE4F-PCB        
140500         WDE6-PCB ZZAC-PCB ARTC-PCB  XXDJ-PCB  XXDK-PCB                   
140600         XXDL-PCB AUTF-PCB ORDP1-PCB ORDP2-PCB XXJN-PCB                   
140700         XXJD-PCB ORQA-PCB  ARTM-PCB  ORQM-PCB 4541-PCB 4487-PCB          
140800         WDE62-PCB ORQA2-PCB XXKW-PCB XXLB-PCB WDM2-PCB                   
140900         ORQI-PCB ORQICSQ-PCB XXKH-PCB WDE4A-PCB XXDU-PCB                 
141000         WDK7-PCB WLLOGA-PCB                                              
141100         WDK6-PCB  WDA6B-PCB WDA6-PCB WDB6-PCB WDB2-PCB WDQ3D-PCB         
141200         WDP4A-PCB                                                        
141300         DNOT-ORQP-PCB                                                    
141400         DNOT-ORQP2-PCB                                                   
141500         DNOT-ORQP3-PCB                                                   
141600         DNOT-4013-PCB                                                    
141700         DNOT-BENA-PCB                                                    
141800         PRQU-WDG2-PCB                                                    
141900         PRQU-WDC7-PCB                                                    
142000         PRQU-SJKO-WDK6-PCB                                               
142100         PRNO-3107-PCB                                                    
142200         KOM-WDP8-PCB.                                                    
142300                                                                          
142400  MAIN SECTION.                                                           
142500     ENTRY 'DLITCBL' USING MSG-PCB 0693-PCB                               
142600         ALT42-PCB ALT49-PCB                                              
142700         ALT97-PCB ALT2-PCB ALT5108-PCB MAIL-PCB DISTRDOC-PCB             
142800         USEA-PCB WDE4-PCB WDE42-PCB WDE43-PCB WDE4E-PCB WDE4F-PCB        
142900         WDE6-PCB ZZAC-PCB ARTC-PCB  XXDJ-PCB  XXDK-PCB                   
143000         XXDL-PCB AUTF-PCB ORDP1-PCB ORDP2-PCB XXJN-PCB                   
143100         XXJD-PCB ORQA-PCB  ARTM-PCB  ORQM-PCB 4541-PCB 4487-PCB          
143200         WDE62-PCB ORQA2-PCB XXKW-PCB XXLB-PCB WDM2-PCB                   
143300         ORQI-PCB ORQICSQ-PCB XXKH-PCB WDE4A-PCB XXDU-PCB                 
143400         WDK7-PCB WLLOGA-PCB                                              
143500         WDK6-PCB  WDA6B-PCB WDA6-PCB WDB6-PCB WDB2-PCB WDQ3D-PCB         
143600         WDP4A-PCB                                                        
143700         DNOT-ORQP-PCB                                                    
143800         DNOT-ORQP2-PCB                                                   
143900         DNOT-ORQP3-PCB                                                   
144000         DNOT-4013-PCB                                                    
144100         DNOT-BENA-PCB                                                    
144200         PRQU-WDG2-PCB                                                    
144300         PRQU-WDC7-PCB                                                    
144400         PRQU-SJKO-WDK6-PCB                                               
144500         PRNO-3107-PCB                                                    
144600         KOM-WDP8-PCB.                                                    
144700                                                                          
144800     PERFORM IMS-GET-MSG                                                  
144900     IF SEGMENT-FINNS                                                     
145000       PERFORM A-INIT                                                     
145100       IF WS-INDATA-RATT AND PROCESS-BO-LINE-BILD                         
145200          PERFORM C-PROCESS-DUMMY-LINE                                    
145300       END-IF                                                             
145400       IF WS-GODKAND-BILD                                                 
145500           PERFORM B-GENERELL-KONTROLL                                    
145600       END-IF                                                             
145700       IF WS-INDATA-RATT AND WS-GODKAND-BILD                              
145800         PERFORM D-BEHANDLA-INTERVALL                                     
145900                                                                          
146000         IF ORAPPORTERADE-RADER-SAKNAS                                    
146100           PERFORM E-UPDATE-KUNDORDER                                     
146200         END-IF                                                           
146300                                                                          
146400         PERFORM G-UPDATE-KOLLIREG                                        
146500                                                                          
146600         IF ORAPPORTERADE-RADER-SAKNAS                                    
146700           PERFORM H-UPDATE-ORDERKO                                       
146800         END-IF                                                           
146900                                                                          
147000         PERFORM I-AVSLUT                                                 
147100                                                                          
147200         IF ORAPPORTERADE-RADER-FINNS                                     
147300         AND INX-TOT-ANT-RADER = WS-RADER-PER-START                       
147400           MOVE WS-IDPRODNR            TO  MID-IDPRODNR                   
147500           MOVE WS-IDANSTNR            TO  MID-IDANSTNR                   
147600           MOVE WS-IDPURAD             TO  MID-IDPURAD                    
147700           MOVE 'W4T397X '             TO  MSG-KDTRANS-1                  
147800           MOVE '4397'                 TO  MSG-IDTRANS-1                  
147900           MOVE WS-KDMFSFOR            TO  MSG-KDMFSFOR-1                 
148000           MOVE +44                    TO  MSG-KVLL                       
148100           MOVE MID                    TO                                 
148200                        MSG-INDATA-MINUS-1-TRANSKOD                       
148300           PERFORM IMS-INSERT-ALT97                                       
148400         END-IF                                                           
148500       END-IF                                                             
148600     END-IF                                                               
148700                                                                          
148800     MOVE ZERO TO RETURN-CODE                                             
148900     GOBACK                                                               
149000     .                                                                    
149100     EJECT                                                                
149200 A-INIT             SECTION.                                              
149300     SKIP3                                                                
149400     IF MSG-DUBBLA-TRANSKODER                                             
149500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I39701               
149600       MOVE MSG-IDTRANS-2                 TO   WS-IDTRANS                 
149700       MOVE MSG-KDMFSFOR-2                TO   WS-KDMFSFOR                
149800     ELSE                                                                 
149900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I39701               
150000       MOVE MSG-IDTRANS-1                 TO   WS-IDTRANS                 
150100       MOVE MSG-KDMFSFOR-1                TO   WS-KDMFSFOR                
150200     END-IF                                                               
150300*                                                                         
150400     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
150500     ACCEPT WS-TID-W                      FROM TIME                       
150600     ACCEPT WS-VOR-TID-BRIST              FROM TIME                       
150700     PERFORM S10-HAMTA-MASKINDATUM                                        
150800     MOVE DAT-TIAAMMDD                    TO   DAGDAT-AAMMD               
150900     MOVE DAT-TIAAVVD                     TO   DAGDAT-AAVVD               
151000*                                                                         
151100     MOVE LOW-VALUE                       TO   MSG-AREA                   
151200     MOVE 'W4O39701'                      TO   WS-MODNAMN                 
151300     MOVE '4397'                          TO   MOD-IDTRANS                
151400     MOVE '1'                             TO   ALT2-SPRAK                 
151500                                               W-KDSEGKEY                 
151600*                                                                         
151700     MOVE NEJ                             TO WS-FLAUTFAK                  
151800                                             FL-LASNINGSTRANS             
151900                                             FL-420-SEGMENT               
152000                                             FL-PACKADEORDERL             
152010                                             SW-LYNK-NON-API              
152020                                             SW-VOR                       
152100     MOVE RAETT                           TO WS-INDATA-TEST               
152200                                             WS-BEHANDLING-TEST           
152300     SKIP2                                                                
152400     INSPECT MID-IDPRODNR REPLACING LEADING SPACE BY ZERO                 
152500                                                                          
152600     IF WS-IDPRODNR NOT NUMERIC                                           
152700         MOVE FEL                       TO   WS-INDATA-TEST               
152800     ELSE                                                                 
152900         MOVE MID-IDPRODNR              TO   WS-IDPRODNR                  
153000     END-IF                                                               
153100                                                                          
153200     MOVE MID-IDANSTNR                  TO   WS-IDANSTNR                  
153300     MOVE MID-IDPLKLST                  TO   WS-IDPLKLST                  
153400     MOVE MID-IDPURAD                   TO   WS-IDPURAD                   
153500                                                                          
153600     MOVE ZERO                          TO   LOGGA-IDLOGLOP               
153700                                             WS-KDTPOTYP                  
153800                                             WS-TIDISPIN                  
153900                                             W-IDORDER-Q2                 
154000*                                                                         
154100     MOVE SPACE                      TO EVENT-SW                          
154200*                                                                         
154300*    -- INITIALIZE W006KOM AREAS WITH FIXED VALUES                        
154400*    -- FIELDS WITH VARYING CONTENT ARE SET LATER                         
154500     MOVE SPACE                      TO MSG-KOM-WMSGKOM                   
154600     MOVE LENGTH OF MSG-KOM-WMSGKOM  TO MSG-KOM-KVLL                      
154700     MOVE LOW-VALUE                  TO MSG-KOM-KDZ1                      
154800     MOVE LOW-VALUE                  TO MSG-KOM-KDZ2                      
154900     MOVE SPACE                      TO MSG-KOM-KDTRANS                   
155000     MOVE 'W4039700'                 TO MSG-KOM-IDSNDJOB                  
155100     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG-KOM-TIREGDAT                  
155200*    -- THIS IS THE START VALUE                                           
155300     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG-KOM-TIKLOCK                   
155400                                        WS-TTMMSSTH-E                     
155500                                                                          
155600     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-AAAAMMDD-E                     
155700                                                                          
155800*    -- INITIALIZE TARGET TRANSACTION AREA WITH FIXED VALUES              
155900     MOVE LOW-VALUE                  TO MSG-KDZ1                          
156000     MOVE LOW-VALUE                  TO MSG-KDZ2                          
156100     .                                                                    
156200     EJECT                                                                
156300 B-GENERELL-KONTROLL  SECTION.                                            
156400                                                                          
156500     MOVE 'B-GENERELL'           TO CURRENT-SECTION                       
156600                                                                          
156700     MOVE WS-IDPRODNR                  TO W-420-IDPRODNR-MIN              
156800                                          W-420-IDPRODNR-MAX              
156900                                          W-420-IDPRODNR                  
157000     MOVE 1                            TO W-420-IDPURAD-MIN               
157100                                          W-420-IDPURAD                   
157200     MOVE 99999                        TO W-420-IDPURAD-MAX               
157300     PERFORM IMS-GU-WDE411-BSEQ                                           
157400*                                                                         
157500     IF SEGMENT-FINNS                                                     
157600        MOVE 'N'                        TO WS-TRAEFF-PACKARE              
157700*                                                                         
157800        MOVE KORD-IDDISTR               TO WS-IDDISTR                     
157900                                           WS-IDDISTR-NUM4                
158000                                           TEST-IDDISTR                   
158100        MOVE KORD-IDKUNDNR              TO WS-IDKUNDNR                    
158200        MOVE KORD-IDKUNDRF              TO WS-IDKUNDRF                    
158300        MOVE KORD-IDDC                  TO W-SPAR-IDDC                    
158400                                           WS-IDDC                        
158410                                                                          
158500        IF WS-IDDC NOT = W-IDDC-B6                                        
158600           MOVE WS-IDDC TO W-IDDC-B6                                      
158700           PERFORM IMS-GU-WDB601                                          
158800        END-IF                                                            
158900*                                                                         
159000        IF DCS-NDC OR                                                     
159100          (DCS-SDC AND (DCS-ENGLAND OR DCS-CHINA))                        
159200          MOVE ALL '+'           TO MSGI-WMSGINIT                         
159300          MOVE '011'             TO MSGI-KDCALL                           
159400          MOVE WS-IDDC           TO IDDC-XX                               
159500          MOVE IDDC-USER         TO MSGI-IDUSER                           
159600          MOVE WS-DAGENS-DATUM   TO MSGI-TILOKDAT                         
159700          MOVE WS-TTMMSS(1:4)    TO MSGI-TILOKTID                         
159800          CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                      
159900          MOVE MSGI-TILOKDAT     TO WS-DAGENS-DATUM                       
160000                                    DAT-I-TIDATUM                         
160100                                    DAGDAT-AAMMD(1:4)                     
160200          MOVE MSGI-TILOKTID     TO WS-TTMMSS   (1:4)                     
160300                                                                          
160400          MOVE 'AAMMDD'          TO DAT-KDDATFORM                         
160500          CALL WDATKONV USING DAT-KDDATFORM                               
160600                              DAT-I-TIDATUM                               
160700                              DAT-O-TIDATUM                               
160800                              DAT-KDSVAR                                  
160900          MOVE DAT-TIAAVVD       TO DAGDAT-AAVVD                          
161000        END-IF                                                            
161100*                                                                         
161200        MOVE WS-IDDISTR                 TO W-401-IDDISTR                  
161300        MOVE WS-IDKUNDNR                TO W-401-IDKUNDNR                 
161400        MOVE WS-IDORDNR                 TO W-401-IDORDNR                  
161500        MOVE WS-IDPRODNR                TO W-401-IDPRODNR                 
161600        MOVE WS-IDPLKLST                TO W-401-IDPLKLST                 
161700        PERFORM IMS-GHU-WDE401                                            
161800*                                                                         
161900        MOVE KORD-IDPRODNR              TO WS-JFR-IDPRODNR                
162000        MOVE KORD-IDUSER                TO WS-JFR-IDANSTNR                
162100        MOVE KORD-IDORDER               TO WS-IDORDER                     
162200        IF WS-IDPRODNR = WS-JFR-IDPRODNR AND                              
162300           WS-IDANSTNR = WS-JFR-IDANSTNR-5                                
162400           MOVE 'J'               TO WS-TRAEFF-PACKARE                    
162500           IF (KORD-KVORDRAD-LEVPL = 0    AND                             
162600               KORD-KVORDRAD-PACK = KORD-KVORDRAD)                        
162700               OR                                                         
162800              (KORD-KVORDRAD-LEVPL > 0    AND                             
162900               KORD-KVORDRAD-PACK = KORD-KVORDRAD-LEVPL)                  
163000*-----------------------------------------------ANGIVEN PACKARES          
163100*-----------------------------------------------ORDERDEL KLAR             
163200             IF WS-IDTRANS NOT = '4359'                                   
163201               MOVE MID-IDPRODNR  TO WS-IDPRODNR-ABEND                    
163202               MOVE MID-IDANSTNR  TO WS-IDANSTNR-ABEND                    
163203               MOVE MID-IDPLKLST  TO WS-IDPLKLST-ABEND                    
163204               MOVE MID-IDPURAD   TO WS-IDPURAD-ABEND                     
163205               MOVE '4359'        TO WS-TEXT-ABEND                        
163206               MOVE WS-JFR-IDANSTNR-5 TO WS-KORD-IDUSER                   
163210               PERFORM S21-SEND-OPEN                                      
163220               PERFORM S23-PUT-HEADER-ABEND                               
163230               PERFORM S24-PUT-LINE-ABEND                                 
163240               PERFORM S29-SEND-CLOSE                                     
163300               MOVE FEL                   TO   WS-INDATA-TEST             
163400*LK            MOVE 'FEL I B-GEN PACKARENS ORDERDEL KLAR'                 
163500*LK                                       TO   ERRORTEX                   
163600*LK            CALL ABEND USING RKOD-ABEND-MED-DUMP                       
163700             END-IF                                                       
163800           END-IF                                                         
163900        END-IF                                                            
164000*                                                                         
164100     ELSE                                                                 
164200*-----------------------------------------------ÄR ORDERN                 
164300*-----------------------------------------------SAKNAD                    
164310        MOVE MID-IDPRODNR         TO WS-IDPRODNR-ABEND                    
164320        MOVE MID-IDANSTNR         TO WS-IDANSTNR-ABEND                    
164330        MOVE MID-IDPLKLST         TO WS-IDPLKLST-ABEND                    
164340        MOVE MID-IDPURAD          TO WS-IDPURAD-ABEND                     
164350        MOVE 'WRONGORDER'         TO WS-TEXT-ABEND                        
164351        MOVE WS-JFR-IDANSTNR-5    TO WS-KORD-IDUSER                       
164360        PERFORM S21-SEND-OPEN                                             
164370        PERFORM S23-PUT-HEADER-ABEND                                      
164380        PERFORM S24-PUT-LINE-ABEND                                        
164390        PERFORM S29-SEND-CLOSE                                            
164400        MOVE FEL                   TO   WS-INDATA-TEST                    
164500*LK     MOVE 'FEL I B-GEN ORDERN SAKNAS'                                  
164600*LK                                TO   ERRORTEX                          
164700*LK     CALL ABEND USING RKOD-ABEND-MED-DUMP                              
164800     END-IF                                                               
164900*                                                                         
165000     IF WS-TRAEFF-PACKARE = 'N'                                           
165100*-----------------------------------------------ANGIVEN PACKARE           
165200*-----------------------------------------------SAKNAS PÅ ORDERN          
165210        MOVE MID-IDPRODNR         TO WS-IDPRODNR-ABEND                    
165220        MOVE MID-IDANSTNR         TO WS-IDANSTNR-ABEND                    
165230        MOVE MID-IDPLKLST         TO WS-IDPLKLST-ABEND                    
165240        MOVE MID-IDPURAD          TO WS-IDPURAD-ABEND                     
165250        MOVE 'WRONGPACKER'        TO WS-TEXT-ABEND                        
165251        MOVE WS-JFR-IDANSTNR-5    TO WS-KORD-IDUSER                       
165260        PERFORM S21-SEND-OPEN                                             
165270        PERFORM S23-PUT-HEADER-ABEND                                      
165280        PERFORM S24-PUT-LINE-ABEND                                        
165290        PERFORM S29-SEND-CLOSE                                            
165291                                                                          
165300        MOVE FEL                   TO   WS-INDATA-TEST                    
165400*LK     MOVE 'FEL I B-GEN ANGIVEN PACKARE SAKNAS PÅ ORDERN'               
165500*LK                                TO   ERRORTEX                          
165600*LK     CALL ABEND USING RKOD-ABEND-MED-DUMP                              
165700     END-IF                                                               
165800     .                                                                    
165900     EJECT                                                                
166000 C-PROCESS-DUMMY-LINE     SECTION.                                        
166100     MOVE 'C-PROCESS'            TO CURRENT-SECTION                       
166200                                                                          
166300*                                                                         
166400*    CHECK WDQ3 IF MORE UNPROCESSED LINE EXITS                            
166500*    WHEN DUMMY LINE, IF ALL OTHER LINES PACKED,                          
166600*    SET CORRECT ORDER STATUS IN KOLLI-REG                                
166700*                                                                         
166800     MOVE WS-IDPRODNR            TO W-Q3DSEQ-IDPRODNR-MIN                 
166900                                    W-Q3DSEQ-IDPRODNR-MAX                 
167000                                    W-601-IDPRODNR                        
167100                                                                          
167200     MOVE ZERO                   TO W-Q3DSEQ-IDPLKLST-MIN                 
167300     MOVE +999                   TO W-Q3DSEQ-IDPLKLST-MAX                 
167400     PERFORM IMS-GU-WDQ3DSEQ                                              
167500     IF SEGMENT-FINNS                                                     
167600        MOVE JA                  TO SW-UPD-KOLLI-REG                      
167700        PERFORM UNTIL SEGMENT-SAKNAS                                      
167800          IF WDQ3D-ODEL-KDODELSTA                                         
167900                              NOT = 'P'                                   
168000             MOVE NEJ            TO SW-UPD-KOLLI-REG                      
168100          END-IF                                                          
168200          PERFORM IMS-GN-WDQ3DSEQ                                         
168300        END-PERFORM                                                       
168400     END-IF                                                               
168500     IF SW-UPD-KOLLI-REG-JA                                               
168600        PERFORM IMS-GHU-WDE601                                            
168700        IF SEGMENT-FINNS                                                  
168800           MOVE +3               TO VORD-KDORDSTA                         
168900           PERFORM IMS-REPL-WDE601                                        
169000        END-IF                                                            
169100     END-IF                                                               
169200     .                                                                    
169300     EJECT                                                                
169400 D-BEHANDLA-INTERVALL  SECTION.                                           
169500     MOVE 'D-BEHA   '            TO CURRENT-SECTION                       
169600                                                                          
169700     MOVE WS-IDDISTR                 TO W-401-IDDISTR                     
169800     MOVE WS-IDKUNDNR                TO W-401-IDKUNDNR                    
169900     MOVE WS-IDORDNR                 TO W-401-IDORDNR                     
170000     MOVE WS-IDPRODNR                TO W-401-IDPRODNR                    
170100     MOVE WS-IDPLKLST                TO W-401-IDPLKLST                    
170200*                                                                         
170300     MOVE    WS-IDORDER           TO W-IDORDER-Q2                         
170400     PERFORM IMS-GU-WDQ201                                                
170410     MOVE    WS-IDDC              TO W-IDDC-Q2                            
170420                                                                          
170430     IF SEGMENT-FINNS                                                     
170440       PERFORM S07-CHK-LYNK-NONAPI                                        
170460     END-IF                                                               
170500     PERFORM IMS-GHNP-WDQ212                                              
170600                                                                          
170700     IF SEGMENT-FINNS                                                     
170800       MOVE OHUV-BEKUNDRF        TO SPAR-BEKUNDRF                         
170900       MOVE OHUV-BEVARREF        TO SPAR-BEVARREF                         
171000     END-IF                                                               
171100                                                                          
171200     PERFORM IMS-GHU-WDE401                                               
171300*                                                                         
171400     MOVE KORD-IDPRODNR              TO WS-JFR-IDPRODNR                   
171500     MOVE KORD-IDUSER                TO WS-JFR-IDANSTNR                   
171600     IF WS-IDPRODNR = WS-JFR-IDPRODNR AND                                 
171700        WS-IDANSTNR = WS-JFR-IDANSTNR-5                                   
171800        MOVE KORD-IDDISTR             TO WS-SAVE-IDDISTR                  
171900        MOVE KORD-IDKUNDNR            TO WS-SAVE-IDKUNDNR                 
172000        MOVE KORD-IDDC                TO WS-IDDC                          
172100        MOVE KORD-FLLSBOK             TO WS-FLLSBOK                       
172200        MOVE KORD-FLORDSPE            TO WS-FLORDSPE                      
172300        MOVE KORD-FLOVRLEV            TO WS-FLOVRLEV                      
172400        MOVE KORD-KDFRAKT             TO WS-KDFRAKT                       
172500        MOVE KORD-KDFAKTYP            TO WS-KDFAKTYP                      
172600        MOVE KORD-KDORDKL             TO WS-KDORDKL                       
172700        MOVE KORD-KVORDRAD-PACK       TO WS-KVORDRAD-PACK                 
172800                                                                          
172900        COMPUTE WS-KVORDRAD = KORD-KVORDRAD +                             
173000                              KORD-KVORDRAD-LEVPL                         
173100*                                                                         
173200        IF WS-IDPURAD > 0                                                 
173310           MOVE WS-IDPURAD TO W-IDPURAD-MIN                               
173320           MOVE WS-IDPURAD TO W-IDPURAD-MAX                               
173400           PERFORM IMS-GHNP-WDE411                                        
173500        ELSE                                                              
173510           MOVE ZERO       TO W-IDPURAD-MIN                               
173520           MOVE 99999      TO W-IDPURAD-MAX                               
173600           PERFORM IMS-GHNP-WDE411                                        
173700        END-IF                                                            
173800                                                                          
173900        IF SEGMENT-FINNS                                                  
174000          IF CHECK-OUT-NOT-REPORTED-LINES                                 
174100            IF ORAD-KDRADSTA = 3                                          
174200*             * FYSISK AVVIKELSE VID PACKNING WL0123 DIREKT               
174300              PERFORM DM-BACKOUT-DEFAULT-CASE                             
174400            END-IF                                                        
174500          END-IF                                                          
174600        END-IF                                                            
174700*                                                                         
174800        IF WS-IDTRANS = '4319'                                            
174900         IF WS-IDPURAD > 0                                                
175100           MOVE ZERO       TO W-IDPURAD-MIN                               
175200           MOVE 99999      TO W-IDPURAD-MAX                               
175300           PERFORM IMS-GHNP-WDE411-FIRST                                  
175400            PERFORM UNTIL SEGMENT-SAKNAS                                  
175500                    OR MORE-LINES-SW = 'Y'                                
175600             IF ORAD-KDRADSTA <= 3                                        
175700                 IF ORAD-IDPURAD NOT = WS-IDPURAD                         
175800                   MOVE 'Y' TO MORE-LINES-SW                              
176000                 END-IF                                                   
176100             END-IF                                                       
176200             IF MORE-LINES-SW = 'N'                                       
176300               PERFORM IMS-GHNP-WDE411                                    
176400             END-IF                                                       
176500            END-PERFORM                                                   
176900         END-IF                                                           
177000        END-IF                                                            
177100                                                                          
177200        MOVE ZERO                 TO  INX-TOT-ANT-RADER                   
177300                                      INX-ANT-RADER-MED-FYS-AVV           
177400        IF MORE-LINES-SW = 'Y'                                            
177410           MOVE WS-IDPURAD TO W-IDPURAD-MIN                               
177420           MOVE WS-IDPURAD TO W-IDPURAD-MAX                               
177430           PERFORM IMS-GHNP-WDE411-FIRST                                  
177700           PERFORM S02-UPDATE-INTERVAL                                    
177800        ELSE                                                              
177810         MOVE WS-IDPURAD TO W-IDPURAD-MIN                                 
177811         MOVE 9999       TO W-IDPURAD-MAX                                 
177820         PERFORM IMS-GHNP-WDE411-FIRST                                    
177900           PERFORM UNTIL SEGMENT-SAKNAS                                   
178000                   OR INX-TOT-ANT-RADER NOT < WS-RADER-PER-START          
178100                                                                          
178200             PERFORM S02-UPDATE-INTERVAL                                  
178300             PERFORM IMS-GHNP-WDE411                                      
178400           END-PERFORM                                                    
178500        END-IF                                                            
178600                                                                          
178700        MOVE    SPAR-VKORDNTO       TO SPAR-VKORDNTO-DEL                  
178800                                                                          
178900        COMPUTE SPAR-VLORDNTO-DEL = SPAR-VLORDNTO / 1000000               
179000                                                                          
179100        MOVE    SPAR-SUORDV         TO SPAR-SUORDV-DEL                    
179200        MOVE    SPAR-SUORDV-LOC     TO SPAR-SUORDV-DEL-LOC                
179300        MOVE    SPAR-SUORDV-LOCPREL TO SPAR-SUORDV-DEL-LOCPREL            
179400                                                                          
179500        IF SEGMENT-FINNS                                                  
179510        OR MORE-LINES-SW = 'Y'                                            
179600          MOVE ORAD-IDPURAD TO WS-IDPURAD                                 
179700          MOVE JA           TO FL-420-SEGMENT                             
179800        END-IF                                                            
179900     END-IF                                                               
180000*                                                                         
180100     PERFORM DF-UPDATE-ORDERREG                                           
180200     EJECT                                                                
180300     .                                                                    
180400 DM-BACKOUT-DEFAULT-CASE     SECTION.                                     
180500     MOVE 'DM-BACKOUT-DEFAULT-CASE'  TO CURRENT-SECTION                   
180600                                                                          
180700*--- DELETE DEFAULT CASE (KDKOLSTA=0) IF THERE IS ANY                     
180800*--- DEFAULT KOLLI FINNS BARA EN PER ORDERDEL SKAPAT PÅ L138.             
180900*--- VID FÖRSTA AVVIKELSE RAPP. PÅ L199 TAS DEN BORT OCH                  
181000*--- DÄREFTER KAN DET INTE FINNAS LÄNGRE DEFAULT KOLLI                    
181100                                                                          
181200     MOVE WS-IDPRODNR             TO  W-IDPRODNR-MIN                      
181300     MOVE WS-IDPRODNR             TO  W-IDPRODNR-MAX                      
181400     MOVE WS-IDPRODNR             TO  W-601-IDPRODNR                      
181500     MOVE WS-IDPLKLST             TO  W-IDPLKLST-E4                       
181600     MOVE ZERO                    TO  W-IDKOLLI-MIN                       
181700     MOVE 99999                   TO  W-IDKOLLI-MAX                       
181800                                                                          
181900     PERFORM IMS-GU-WDE4F1-PLK                                            
182000     IF SEGMENT-FINNS                                                     
182100       MOVE SEQF-IDKOLLI  TO W-IDKOLLI-E6                                 
182200                                                                          
182300       PERFORM IMS-GHU-WDE611-DEF                                         
182400* KOLLISTATUS = 0 IN IMS-READ                                             
182500       IF SEGMENT-FINNS                                                   
182600       IF KOLLI-DIKOLLIL = ZERO                                           
182700         PERFORM IMS-DLET-WDE611                                          
182800         PERFORM IMS-GHU-WDE601                                           
182900         SUBTRACT 1 FROM VORD-KVKOLLI                                     
183000*                                                                         
183100         PERFORM IMS-REPL-WDE601                                          
184000*                                                                         
184100         MOVE ZERO         TO W-IDPURAD-MIN                               
184200         MOVE 99999        TO W-IDPURAD-MAX                               
184300         PERFORM IMS-GHNP-WDE411-FIRST                                    
184400                                                                          
184500         PERFORM UNTIL SEGMENT-SAKNAS                                     
184600           MOVE ZERO       TO ORAD-KVLEVART                               
184700           PERFORM IMS-REPL-WDE411                                        
184800                                                                          
184900           PERFORM IMS-GHNP-WDE421                                        
185000           PERFORM IMS-DLET-WDE421                                        
185100                                                                          
185200           PERFORM IMS-GHNP-WDE411                                        
185300         END-PERFORM                                                      
185310                                                                          
185600       END-IF                                                             
185700       END-IF                                                             
185800     END-IF                                                               
185900     .                                                                    
186000     EJECT                                                                
186100 DA-UPDATE-FYS-AVVIKELSE SECTION.                                         
186200     MOVE     'DA-UPDAT  '           TO CURRENT-SECTION                   
186300                                                                          
186400******************************************************************        
186500*                                                                         
186600*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
186700*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
186800*                                                                         
186900******************************************************************        
187000                                                                          
187100     MOVE WS-IDDISTR         TO W-TP4TRAN-IDDISTR                         
187200                                                                          
187300     PERFORM DB2-SELECT-TP4TRAN                                           
187400                                                                          
187500     MOVE WS-IDDISTR TO TEST-IDDISTR                                      
187600                                                                          
187700     IF  NOT DIST18-SKROT                                                 
187800     AND NOT DIST18-SCRAP-NDC                                             
187900     AND NOT DIST19-SATS                                                  
188000     AND NOT DIST20-EMBALLAGE                                             
188100     AND NOT DIST20-EMBALLAGE-NDC                                         
188200     AND NOT DIST35-REFILL                                                
188300     AND NOT DIST35-REFILL-INOM-NDC                                       
188400     AND NOT DIST35-REFILL-NA-JAP                                         
188500     AND NOT DIST35-NONVCC-CDC-REFILL                                     
188600     AND NOT DIST35-NA-TRANSFER                                           
188700     AND NOT DIST35-NA-NDC-RETURNS                                        
188800     AND NOT DIST35-PACIFIC-TRANSFER                                      
188900     AND NOT DIST35-REFILL-INOM-JP                                        
189000     AND NOT DIST35-CN-TRANSFER                                           
189100     AND NOT RADER-FINNS                                                  
189200                                                                          
189300         IF (ORAD-KDORDKL = 0 OR 1 OR 2 OR 3) AND                         
189400            ORAD-KVLEVART NOT = ORAD-KVAVBART                             
189500              PERFORM DAD-GET-REGDAT                                      
189600              PERFORM DAB-GEN-4320-BARN                                   
189700                                                                          
189800              IF SEGMENT-SAKNAS                                           
189900                  PERFORM DAA-GEN-4319-ROT                                
190000                  PERFORM DAB-GEN-4320-BARN                               
190100              ELSE                                                        
190200                  IF SEGMENT-FINNS-REDAN                                  
190300                      PERFORM DAC-REPL-4320-BARN                          
190400                  END-IF                                                  
190500              END-IF                                                      
190600              PERFORM DAE-NILPICK-MAIL                                    
190700         END-IF                                                           
190800     END-IF                                                               
190900     .                                                                    
191000     SKIP2                                                                
191100 DAA-GEN-4319-ROT SECTION.                                                
191200     MOVE 'DAA-GEN-  '           TO CURRENT-SECTION                       
191300                                                                          
191400     MOVE '4319'              TO 4319-IDHTYP                              
191500                                 MSGI-IDTRANS                             
191600     MOVE LOW-VALUE           TO 4319-LOWVALUE                            
191700     IF WS-IDDC NOT = W-IDDC-B6                                           
191800        MOVE WS-IDDC TO W-IDDC-B6                                         
191900        PERFORM IMS-GU-WDB601                                             
192000     END-IF                                                               
192100     IF DCS-NDC OR                                                        
192200       (DCS-SDC AND NOT DCS-SWEDEN)                                       
192300       MOVE MSGI-TILOKDAT     TO 4319-TIREGDAT                            
192400     ELSE                                                                 
192500       MOVE DAGDAT-AAMMD      TO 4319-TIREGDAT                            
192600     END-IF                                                               
192700                                                                          
192800     PERFORM IMS-4319-ISRT-ROT                                            
192900     .                                                                    
193000     SKIP3                                                                
193100 DAB-GEN-4320-BARN SECTION.                                               
193200     MOVE 'DAB-GEN-  '           TO CURRENT-SECTION                       
193300                                                                          
193400     MOVE WS-IDDISTR        TO 4320-IDDISTR                               
193500     MOVE WS-IDKUNDNR       TO 4320-IDKUNDNR                              
193600     MOVE WS-IDORDNR        TO 4320-IDORDNR                               
193700     MOVE WS-IDPRODNR       TO 4320-IDPRODNR                              
193800     MOVE ORAD-IDARTNR      TO 4320-IDARTNR                               
193900     MOVE ORAD-REKSIFFR     TO 4320-REKSIFFR                              
194000     MOVE ORAD-BEART        TO 4320-BEART                                 
194100     MOVE ORAD-KVAVBART     TO 4320-KVAVBART                              
194200     MOVE ORAD-KVLEVART     TO 4320-KVLEVART                              
194300     MOVE ORAD-KDORDKL      TO 4320-KDORDKL                               
194400                                                                          
194500     MOVE WS-IDDC           TO 4320-IDDC                                  
194600                                                                          
194700     MOVE SPAR-BEVARREF     TO 4320-BEVARREF                              
194800                                                                          
194900     PERFORM  IMS-4320-ISRT-BARN                                          
195000     .                                                                    
195100     EJECT                                                                
195200 DAC-REPL-4320-BARN SECTION.                                              
195300     SKIP2                                                                
195400     MOVE WS-IDDISTR        TO W-XXJD-IDDISTR                             
195500     MOVE WS-IDKUNDNR       TO W-XXJD-IDKUNDNR                            
195600     MOVE WS-IDORDNR        TO W-XXJD-IDORDNR                             
195700     MOVE WS-IDPRODNR       TO W-XXJD-IDPRODNR                            
195800     MOVE ORAD-IDARTNR      TO W-XXJD-IDARTNR                             
195900     MOVE ORAD-REKSIFFR     TO W-XXJD-REKSIFFR                            
196000                                                                          
196100     IF WS-IDDC NOT = W-IDDC-B6                                           
196200        MOVE WS-IDDC TO W-IDDC-B6                                         
196300        PERFORM IMS-GU-WDB601                                             
196400     END-IF                                                               
196500                                                                          
196600     IF DCS-NDC OR                                                        
196700       (DCS-SDC AND NOT DCS-SWEDEN)                                       
196800       MOVE MSGI-TILOKDAT     TO W-XXJD-TIREGDAT                          
196900     ELSE                                                                 
197000       MOVE DAGDAT-AAMMD      TO W-XXJD-TIREGDAT                          
197100     END-IF                                                               
197200     PERFORM IMS-GHU-4320-BARN                                            
197300                                                                          
197400     ADD ORAD-KVLEVART      TO 4320-KVLEVART                              
197500     ADD ORAD-KVAVBART      TO 4320-KVAVBART                              
197600                                                                          
197700     PERFORM IMS-REPL-4320-BARN                                           
197800     .                                                                    
197900     EJECT                                                                
198000 DAD-GET-REGDAT     SECTION.                                              
198100     MOVE 'DAD-GET-  '           TO CURRENT-SECTION                       
198200                                                                          
198300     IF WS-IDDC NOT = W-IDDC-B6                                           
198400        MOVE WS-IDDC TO W-IDDC-B6                                         
198500        PERFORM IMS-GU-WDB601                                             
198600     END-IF                                                               
198700                                                                          
198800     IF DCS-NDC OR                                                        
198900       (DCS-SDC AND NOT DCS-SWEDEN)                                       
199000       MOVE ALL '+'           TO MSGI-WMSGINIT                            
199100       MOVE '013'             TO MSGI-KDCALL                              
199200       MOVE WS-IDDC           TO IDDC-XX                                  
199300       MOVE IDDC-USER         TO MSGI-IDUSER                              
199400       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
199500                                                                          
199600       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
199700       MOVE MSGI-TILOKDAT     TO 4319-TIREGDAT                            
199800                                 W-XXJD-TIREGDAT                          
199900     ELSE                                                                 
200000       MOVE DAGDAT-AAMMD      TO 4319-TIREGDAT                            
200100                                 W-XXJD-TIREGDAT                          
200200     END-IF                                                               
200300     .                                                                    
200400     SKIP2                                                                
200500 DAE-NILPICK-MAIL   SECTION.                                              
200600     MOVE 'STA DAE-NILPICK'          TO CURRENT-SECTION                   
200700                                                                          
200800     MOVE OHUV-IDDISTR       TO W-IDDISTR-WDB2                            
200900     MOVE OHUV-IDKUNDNR      TO W-IDKUNDNR-WDB2                           
201000                                                                          
201100     PERFORM IMS-GU-GMTA-WDB201                                           
201200     IF GMT-FLLDCKND = JA                                                 
201210       MOVE +1 TO INDX                                                    
201220       PERFORM UNTIL INDX > IX-DCCLEAR-MAX                                
201221         IF OHUV-KDORDKL = 0                                              
201222           MOVE GMT-IDDC-VOR(INDX) TO W-GMT-IDDC-CLEAR(INDX)              
201224         END-IF                                                           
201225         IF OHUV-KDORDKL = 1                                              
201226           MOVE GMT-IDDC-DAY(INDX) TO W-GMT-IDDC-CLEAR(INDX)              
201228         END-IF                                                           
201229         IF OHUV-KDORDKL > 1                                              
201230           MOVE GMT-IDDC-BULK(INDX) TO W-GMT-IDDC-CLEAR(INDX)             
201232         END-IF                                                           
201233         ADD +1 TO INDX                                                   
201240       END-PERFORM                                                        
201241                                                                          
201250       MOVE 1 TO INDX                                                     
201260       PERFORM UNTIL INDX > 4 OR                                          
201270               W-GMT-IDDC-CLEAR (INDX) = WS-IDDC                          
201280         ADD 1 TO INDX                                                    
201290       END-PERFORM                                                        
201300                                                                          
201400       IF INDX < 4                                                        
201500         IF W-GMT-IDDC-CLEAR(INDX) NOT = WC-CDC-SE                        
201600           ADD 1 TO INDX                                                  
201700           IF W-GMT-IDDC-CLEAR(INDX) NOT = WC-CDC-SE                      
202300             MOVE WS-IDDISTR    TO NILP-IDDISTR                           
202400             MOVE WS-IDKUNDNR   TO NILP-IDKUNDNR                          
202500             MOVE WS-IDKUNDRF   TO NILP-IDKUNDRF                          
202600             MOVE WS-IDDC       TO NILP-IDDC                              
202700             MOVE WS-IDANSTNR   TO NILP-IDANSTNR                          
202800             MOVE ORAD-IDARTNR  TO NILP-IDARTNR                           
202900             MOVE ORAD-KVBEART  TO NILP-KVBEART                           
203000             MOVE ORAD-KVAVBART TO NILP-KVAVBART                          
203100             MOVE ORAD-KVANNANT TO NILP-KVANNANT                          
203200             MOVE ORAD-KVLEVART TO NILP-KVLEVART                          
203300                                                                          
203400             CALL W403NILP USING NILP-W403NILP DISTRDOC-PCB               
203500           END-IF                                                         
203510         END-IF                                                           
203600       END-IF                                                             
203800     END-IF                                                               
203900     .                                                                    
204000     SKIP2                                                                
204100 DB-BEHANDLA-RAD-INOM-INTERVALL SECTION.                                  
204200     MOVE 'DB-BEHAN  '           TO CURRENT-SECTION                       
204300                                                                          
204400     COMPUTE WS-KVORAPP-PACK  = ORAD-KVAVBART -                           
204500                             ORAD-KVLEVART                                
204600     END-COMPUTE                                                          
204700                                                                          
204800     COMPUTE WS-KVORAPP-TOTAL = ORAD-KVBEART -                            
204900                             ORAD-KVANNANT -                              
205000                             ORAD-KVLEVART                                
205100     END-COMPUTE                                                          
205200                                                                          
205300     IF WS-IDDC NOT = W-IDDC-B6                                           
205400        MOVE WS-IDDC TO W-IDDC-B6                                         
205500        PERFORM IMS-GU-WDB601                                             
205600     END-IF                                                               
205700                                                                          
205800     IF ORAD-KDRADSTA = 4   AND                                           
205900        ORAD-KVAVBART = 0   AND                                           
206000        ORAD-KVANNANT > 0                                                 
206100        MOVE 0               TO WS-KVPRERO                                
206200     ELSE                                                                 
206300        IF DCS-CDC                                                        
206400           COMPUTE WS-KVPRERO    = ORAD-KVBEART  -                        
206500                                   ORAD-KVANNANT -                        
206600                                   ORAD-KVAVBART                          
206700           END-COMPUTE                                                    
206800        ELSE                                                              
206900           MOVE 0               TO WS-KVPRERO                             
207000        END-IF                                                            
207100     END-IF                                                               
207200                                                                          
207300     MOVE ORAD-KVLEVART                 TO  ORAD-KVAVBART                 
207400     MOVE +4                            TO  ORAD-KDRADSTA                 
207500     MOVE 'Y'                           TO  DATUM-SW                      
207600     PERFORM S01-UPPD-SPAR-UPPGIFTER                                      
207700     IF KORD-KDORDKL = +0                                                 
207800        PERFORM DBA-UPPDATERA-VOR-TIKLAR                                  
207900     END-IF                                                               
208000     EJECT                                                                
208100     .                                                                    
208200 DBA-UPPDATERA-VOR-TIKLAR SECTION.                                        
208300     MOVE 'DBA-UPD-TIKLAR  '       TO CURRENT-SECTION                     
208400                                                                          
208500     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
208600     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
208700     MOVE KORD-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
208800                                    W-A601KY-MAX-IDDISTR                  
208900     MOVE KORD-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
209000                                    W-A601KY-MAX-IDKUNDNR                 
209100     MOVE SPACE                  TO W-A601KY-MIN-IDKUNDRF                 
209200                                    W-A601KY-MAX-IDKUNDRF                 
209300     MOVE KORD-IDORDNR5          TO W-A601KY-MIN-IDORDNR                  
209400                                    W-A601KY-MAX-IDORDNR                  
209500                                                                          
209600     MOVE NEJ                    TO SW-TIKLAR-UPPDATERAD                  
209700                                                                          
209800     PERFORM IMS-GHN-SEQB-WDA601                                          
209900     PERFORM UNTIL SEGMENT-SAKNAS                                         
210000                OR SEGMENT-SLUT                                           
210100                OR SW-TIKLAR-UPPDATERAD = JA                              
210200                                                                          
210300         IF  VOR-IDARTNR = ORAD-IDARTNR                                   
210400         AND VOR-TIKLAR = +0                                              
210500                                                                          
210600             MOVE WS-DAGENS-DATUM   TO VOR-TIKLAR                         
210700             MOVE WS-TTMMSS         TO VOR-TIKLATID                       
210800             PERFORM IMS-REPL-SEQB-WDA601                                 
210900             MOVE JA                TO SW-TIKLAR-UPPDATERAD               
211000         END-IF                                                           
211100                                                                          
211200         PERFORM IMS-GHN-SEQB-WDA601                                      
211300     END-PERFORM                                                          
211400     .                                                                    
211500 DE-UPDATE-ARTREG  SECTION.                                               
211600     MOVE 'DE-UPDAT  '           TO CURRENT-SECTION                       
211700                                                                          
211800     IF WS-IDDC NOT = W-IDDC-B6                                           
211900        MOVE WS-IDDC TO W-IDDC-B6                                         
212000        PERFORM IMS-GU-WDB601                                             
212100     END-IF                                                               
212200     IF SPAR-ORAD-FLDIRLEV = NEJ OR                                       
212300        SPAR-ORAD-FLRESTN  = JA                                           
212400                                                                          
212500        IF DCS-CDC                                                        
212600          PERFORM DEA-UPDATE-PRERO-WDK9                                   
212700          PERFORM DEB-UPDATE-SALDO-CDC                                    
212800        ELSE                                                              
212900          IF DCS-SDC                                                      
213000            PERFORM DEC-UPDATE-SALDO-SDC                                  
213100          ELSE                                                            
213200            IF DCS-NDC                                                    
213300              PERFORM DEF-UPDATE-SALDO-NDC                                
213400            END-IF                                                        
213500          END-IF                                                          
213600        END-IF                                                            
213700     END-IF                                                               
213800     PERFORM DED-EV-UPDATE-REFILL-SDC                                     
213900     EJECT                                                                
214000     .                                                                    
214100 DEA-UPDATE-PRERO-WDK9  SECTION.                                          
214200     MOVE 'DEA-UPDAT '           TO CURRENT-SECTION                       
214300                                                                          
214400     IF WS-FLLSBOK  = JA                                                  
214500       IF WS-FLORDSPE = NEJ AND WS-FLOVRLEV = NEJ                         
214600         MOVE SPAR-ORAD-IDARTNR       TO W-901-IDARTNR                    
214700         PERFORM IMS-GHU-WDK901                                           
214800                                                                          
214900         PERFORM DEAB-UPDATE-PRERO-CDC                                    
215000                                                                          
215100         PERFORM IMS-REPL-WDK901                                          
215200       END-IF                                                             
215300     END-IF                                                               
215400     .                                                                    
215500     EJECT                                                                
215600 DEAB-UPDATE-PRERO-CDC                SECTION.                            
215700     MOVE 'DEAB-UPDAT'           TO CURRENT-SECTION                       
215800                                                                          
215900     IF SPAR-ORAD-KDORDKL = 1                                             
216000       COMPUTE ART-KVPRERO-DAG =                                          
216100               ART-KVPRERO-DAG -                                          
216200               WS-KVPRERO                                                 
216300       END-COMPUTE                                                        
216400     ELSE                                                                 
216500       IF SPAR-ORAD-KDORDKL = 2 OR 3 OR 4                                 
216600         COMPUTE ART-KVPRERO-BULK =                                       
216700                 ART-KVPRERO-BULK -                                       
216800                 WS-KVPRERO                                               
216900         END-COMPUTE                                                      
217000       END-IF                                                             
217100     END-IF                                                               
217200     .                                                                    
217300     EJECT                                                                
217400 DEB-UPDATE-SALDO-CDC SECTION.                                            
217500     MOVE 'DEB-UPDAT '           TO CURRENT-SECTION                       
217600                                                                          
217700     IF WS-FLLSBOK = JA                                                   
217800        MOVE SPAR-ORAD-IDARTNR        TO W-IDARTNR                        
217900        PERFORM IMS-GHU-ARTC11                                            
218000        MOVE CLAG-IDANSK TO WS-IDANSK                                     
218100                                                                          
218200        MOVE CLAG-KVLS    TO WS-KVLS                                      
218300        MOVE CLAG-KVEFRS  TO WS-KVEFRS                                    
218400        IF SPAR-ORAD-FLDIRLEV = NEJ                                       
218500*SVS FROG                                                                 
218600           MOVE WS-IDDISTR   TO DIST20-IDDISTR                            
218700           IF DIST20-EMBALLAGE-SVS                                        
218800             MOVE KORD-IDORDER           TO W-WDQ301-IDORDER              
218900             MOVE KORD-IDDC              TO W-WDQ301-IDDC                 
219000             MOVE KORD-IDPRODNR          TO W-WDQ301-IDPRODNR             
219100             MOVE KORD-IDPLKLST          TO W-WDQ301-IDPLKLST             
219200             PERFORM IMS-GHU-ORQA01                                       
219300                                                                          
219400             IF ODEL-IDPRC = 2600                                         
219500                                                                          
219600               COMPUTE CLAG-KVLS-SVS =                                    
219700                       CLAG-KVLS-SVS + WS-KVORAPP-PACK                    
219800               END-COMPUTE                                                
219900             END-IF                                                       
220000           END-IF                                                         
220100                                                                          
220200           COMPUTE CLAG-KVLS    = CLAG-KVLS    + WS-KVORAPP-PACK          
220300           END-COMPUTE                                                    
220400           COMPUTE CLAG-KVEFRS  = CLAG-KVEFRS  - WS-KVORAPP-PACK          
220500           END-COMPUTE                                                    
220600                                                                          
220700           IF SPAR-ORAD-IDKAMPRF   >  0   AND                             
220800              SPAR-KART-KVRESS-ART >= 0                                   
220900             COMPUTE CLAG-KVRESS = CLAG-KVRESS + WS-KVSLATTAT             
221000             END-COMPUTE                                                  
221100           END-IF                                                         
221200        END-IF                                                            
221300                                                                          
221400        IF SPAR-ORAD-FLRESTN  = JA           AND                          
221500          (SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                         
221600                              - SPAR-ORAD-KVANNANT                        
221700                              - SPAR-ORAD-KVSLATT)                        
221800           IF SPAR-ORAD-KDORDKL > 0                                       
221900               PERFORM S14-EV-LARM-2191-MID                               
222000                                                                          
222100               COMPUTE CLAG-KVROS = CLAG-KVROS + WS-KVORAPP-TOTAL         
222200               END-COMPUTE                                                
222300           END-IF                                                         
222400                                                                          
222500           IF CLAG-KVROS = WS-KVORAPP-TOTAL                               
222600              MOVE DAGDAT-AAVVD TO CLAG-TIRODAT                           
222700           END-IF                                                         
222800        END-IF                                                            
222900        PERFORM IMS-REPL-ARTC                                             
223000        PERFORM DEBB-BERAKNA-SALDOLOGG-DATA                               
223100     END-IF                                                               
223200     .                                                                    
223300     EJECT                                                                
223400 DEBB-BERAKNA-SALDOLOGG-DATA SECTION.                                     
223500     MOVE 'DEBB-BERA '           TO CURRENT-SECTION                       
223600                                                                          
223700     PERFORM S17-FLYTTA-SALDOLOGG-DATA                                    
223800*    ---DB-SPECIFIK INFORMATION                                           
223900     MOVE W-IDARTNR           TO LOGG-IDARTNR                             
224000     MOVE WC-CDC-SE           TO LOGG-IDDC                                
224100     MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                              
224200     MOVE CLAG-KVLS           TO LOGG-KVLS                                
224300     MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
224400     COMPUTE LOGG-KVAKS       =  CLAG-KVAKS-CDC                           
224500                              +  CLAG-KVAKS-T                             
224600     MOVE '+'                 TO LOGG-IDTECKEN-KVLS                       
224700     MOVE '-'                 TO LOGG-IDTECKEN-KVEFRS                     
224800     PERFORM S16-ISRT-SALDOLOGG                                           
224900     .                                                                    
225000     EJECT                                                                
225100 DEC-UPDATE-SALDO-SDC      SECTION.                                       
225200     MOVE 'DEC-UPDAT '           TO CURRENT-SECTION                       
225300                                                                          
225400     IF WS-FLLSBOK = JA                                                   
225500        MOVE SPAR-ORAD-IDARTNR        TO W-IDARTNR                        
225600        MOVE WS-IDDC                  TO W-711-IDDC                       
225700        PERFORM IMS-GU-WDK722                                             
225800        IF SEGMENT-FINNS                                                  
225900          MOVE XLAG-IDANSK            TO WS-XLAG-IDANSK                   
226000        ELSE                                                              
226100          MOVE ZERO                   TO WS-XLAG-IDANSK                   
226200        END-IF                                                            
226300        PERFORM IMS-GHU-WDK711                                            
226400                                                                          
226500        MOVE SLAG-KVLS                TO WS-KVLS                          
226600        MOVE SLAG-KVEFRS              TO WS-KVEFRS                        
226700        COMPUTE SLAG-KVLS    = SLAG-KVLS    + WS-KVORAPP-PACK             
226800        END-COMPUTE                                                       
226900        COMPUTE SLAG-KVEFRS  = SLAG-KVEFRS  - WS-KVORAPP-PACK             
227000        END-COMPUTE                                                       
227100                                                                          
227200        IF NOT DCS-CHINA                                                  
227300          PERFORM IMS-REPL-WDK7                                           
227400          PERFORM S18-BERAKNA-SALDOLOGG-DATA                              
227500          PERFORM DECA-UPDATE-CLAG-KVROS                                  
227600        ELSE                                                              
227700          PERFORM DECB-UPDATE-SLAG-KVROS                                  
227800          PERFORM IMS-REPL-WDK7                                           
227900          PERFORM S18-BERAKNA-SALDOLOGG-DATA                              
228000        END-IF                                                            
228100     END-IF                                                               
228200     .                                                                    
228300     EJECT                                                                
228400 DECA-UPDATE-CLAG-KVROS    SECTION.                                       
228500     MOVE 'DECA-UPDAT'           TO CURRENT-SECTION                       
228600                                                                          
228700     IF SPAR-ORAD-KDORDKL > 0  AND                                        
228800        WS-KVORAPP-TOTAL > ZERO                                           
228900                                                                          
229000       IF SPAR-ORAD-FLRESTN  = JA           AND                           
229100         (SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                          
229200                             - SPAR-ORAD-KVANNANT                         
229300                             - SPAR-ORAD-KVSLATT)                         
229400                                                                          
229500          PERFORM IMS-GHU-ARTC11                                          
229600          MOVE DCS-IDDC    TO W-711-IDDC                                  
229700          MOVE CLAG-IDANSK TO WS-IDANSK                                   
229800                                                                          
229900          PERFORM S14-EV-LARM-2191-MID                                    
230000          COMPUTE CLAG-KVROS = CLAG-KVROS + WS-KVORAPP-PACK               
230100          END-COMPUTE                                                     
230200                                                                          
230300          IF CLAG-KVROS = WS-KVORAPP-TOTAL                                
230400             MOVE DAGDAT-AAVVD TO CLAG-TIRODAT                            
230500          END-IF                                                          
230600          PERFORM IMS-REPL-ARTC                                           
230700       END-IF                                                             
230800     END-IF                                                               
230900     .                                                                    
231000     EJECT                                                                
231100 DECB-UPDATE-SLAG-KVROS    SECTION.                                       
231200     MOVE 'DECB-UPDAT'           TO CURRENT-SECTION                       
231300                                                                          
231400     IF SPAR-ORAD-KDORDKL > 0  AND                                        
231500        WS-KVORAPP-TOTAL > ZERO                                           
231600                                                                          
231700       IF SPAR-ORAD-FLRESTN  = JA           AND                           
231800         (SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                          
231900                             - SPAR-ORAD-KVANNANT                         
232000                             - SPAR-ORAD-KVSLATT)                         
232100                                                                          
232200          IF SPAR-ORAD-IDDC-RO = WS-IDDC                                  
232300            IF  DCS-NDC AND DCS-CHINA                                     
232400            AND SLAG-IDDC-REF = SPACE                                     
232500               PERFORM S14-EV-LARM-2191-MID-CN-US                         
232600            END-IF                                                        
232700            IF SPAR-ORAD-KDORDKL > 1                                      
232800              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
232900                                      + WS-KVORAPP-PACK                   
233000            ELSE                                                          
233100              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
233200                                      + WS-KVORAPP-PACK                   
233300            END-IF                                                        
233400          ELSE                                                            
233500            PERFORM IMS-REPL-WDK7                                         
233600            MOVE SPAR-ORAD-IDDC-RO   TO W-711-IDDC                        
233700            PERFORM IMS-GU-WDK722                                         
233800            IF SEGMENT-FINNS                                              
233900              MOVE XLAG-IDANSK        TO WS-XLAG-IDANSK                   
234000            ELSE                                                          
234100              MOVE ZERO               TO WS-XLAG-IDANSK                   
234200            END-IF                                                        
234300            PERFORM IMS-GHU-WDK711                                        
234400            IF  DCS-NDC AND DCS-CHINA                                     
234500            AND SLAG-IDDC-REF = SPACE                                     
234600               PERFORM S14-EV-LARM-2191-MID-CN-US                         
234700            END-IF                                                        
234800            IF SPAR-ORAD-KDORDKL > 1                                      
234900              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
235000                                      + WS-KVORAPP-PACK                   
235100            ELSE                                                          
235200              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
235300                                      + WS-KVORAPP-PACK                   
235400              END-COMPUTE                                                 
235500            END-IF                                                        
235600            MOVE WS-IDDC             TO W-711-IDDC                        
235700          END-IF                                                          
235800       END-IF                                                             
235900     END-IF                                                               
236000     .                                                                    
236100     EJECT                                                                
236200 DEF-UPDATE-SALDO-NDC      SECTION.                                       
236300     MOVE 'DEF-UPDATE'           TO CURRENT-SECTION                       
236400                                                                          
236500     IF  WS-FLLSBOK = JA                                                  
236600     AND SPAR-ORAD-FLDIRLEV = NEJ                                         
236700        MOVE SPAR-ORAD-IDARTNR        TO W-IDARTNR                        
236800        MOVE WS-IDDC                  TO W-711-IDDC                       
236900        PERFORM IMS-GU-WDK722                                             
237000        IF SEGMENT-FINNS                                                  
237100          MOVE XLAG-IDANSK            TO WS-XLAG-IDANSK                   
237200        ELSE                                                              
237300          MOVE ZERO                   TO WS-XLAG-IDANSK                   
237400        END-IF                                                            
237500        PERFORM IMS-GHU-WDK711                                            
237600                                                                          
237700        MOVE SLAG-KVLS                TO WS-KVLS                          
237800        MOVE SLAG-KVEFRS              TO WS-KVEFRS                        
237900        COMPUTE SLAG-KVLS    = SLAG-KVLS    + WS-KVORAPP-PACK             
238000        END-COMPUTE                                                       
238100        COMPUTE SLAG-KVEFRS  = SLAG-KVEFRS  - WS-KVORAPP-PACK             
238200        END-COMPUTE                                                       
238300                                                                          
238400        IF  SPAR-ORAD-FLRESTN = JA                                        
238500        AND SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART                        
238600                               - SPAR-ORAD-KVANNANT                       
238700                               - SPAR-ORAD-KVSLATT                        
238800        AND SPAR-ORAD-KDORDKL > 0                                         
238900          IF  SPAR-ORAD-IDDC-RO = WS-IDDC                                 
239000            IF  DCS-NDC-CN                                                
239100            OR (DCS-NDC-NA AND DCS-USA)                                   
239200               IF SLAG-IDDC-REF = SPACE                                   
239300                  PERFORM S14-EV-LARM-2191-MID-CN-US                      
239400               END-IF                                                     
239500            END-IF                                                        
239600            IF SPAR-ORAD-KDORDKL > 1                                      
239700              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
239800                                      + WS-KVORAPP-TOTAL                  
239900              END-COMPUTE                                                 
240000            ELSE                                                          
240100              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
240200                                      + WS-KVORAPP-TOTAL                  
240300              END-COMPUTE                                                 
240400            END-IF                                                        
240500            PERFORM IMS-REPL-WDK7                                         
240600          ELSE                                                            
240700            IF SPAR-ORAD-IDDC-RO = '11'                                   
240800              MOVE 'SPAR-ORAD-IDDC-RO HAR FEL IDDC'  TO  ERRORTEX         
240900            END-IF                                                        
241000            PERFORM IMS-REPL-WDK7                                         
241100            MOVE SPAR-ORAD-IDDC-RO        TO W-711-IDDC                   
241200            PERFORM IMS-GU-WDK722                                         
241300            IF SEGMENT-FINNS                                              
241400              MOVE XLAG-IDANSK            TO WS-XLAG-IDANSK               
241500            ELSE                                                          
241600              MOVE ZERO                   TO WS-XLAG-IDANSK               
241700            END-IF                                                        
241800            PERFORM IMS-GHU-WDK711                                        
241900            IF  DCS-NDC-CN                                                
242000            OR (DCS-NDC-NA AND DCS-USA)                                   
242100               IF SLAG-IDDC-REF = SPACE                                   
242200                 PERFORM S14-EV-LARM-2191-MID-CN-US                       
242300               END-IF                                                     
242400            END-IF                                                        
242500            IF SPAR-ORAD-KDORDKL > 1                                      
242600              COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK                   
242700                                      + WS-KVORAPP-TOTAL                  
242800              END-COMPUTE                                                 
242900            ELSE                                                          
243000              COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG                    
243100                                      + WS-KVORAPP-TOTAL                  
243200              END-COMPUTE                                                 
243300            END-IF                                                        
243400            PERFORM IMS-REPL-WDK7                                         
243500            MOVE WS-IDDC                  TO W-711-IDDC                   
243600          END-IF                                                          
243700        ELSE                                                              
243800          PERFORM IMS-REPL-WDK7                                           
243900        END-IF                                                            
244000        PERFORM S18-BERAKNA-SALDOLOGG-DATA                                
244100     END-IF                                                               
244200     .                                                                    
244300     EJECT                                                                
244400 DED-EV-UPDATE-REFILL-SDC  SECTION.                                       
244500     MOVE 'DED-EV-UPDAT'         TO CURRENT-SECTION                       
244600*REFILLORDER                                                              
244700                                                                          
244800******************************************************************        
244900*                                                                         
245000*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
245100*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
245200*                                                                         
245300******************************************************************        
245400                                                                          
245500     MOVE KORD-IDDISTR         TO W-TP4TRAN-IDDISTR                       
245600                                                                          
245700     PERFORM DB2-SELECT-TP4TRAN                                           
245800                                                                          
245900       MOVE KORD-IDDISTR            TO TEST-IDDISTR                       
246000                                                                          
246100*NDC&SDC OCH INTE RESTNOTERING.                                           
246200       IF (DIST35-REFILL                                                  
246300        OR DIST35-REFILL-INOM-NDC                                         
246400        OR DIST35-REFILL-NA-JAP                                           
246500        OR DIST35-NA-TRANSFER                                             
246600        OR DIST35-NA-NDC-RETURNS                                          
246700        OR DIST35-PACIFIC-TRANSFER                                        
246800        OR DIST35-REFILL-INOM-JP                                          
246900        OR DIST35-CN-TRANSFER                                             
247000        OR RADER-FINNS)                                                   
247100       AND SPAR-ORAD-FLRESTN = NEJ                                        
247200       AND WS-KVORAPP-TOTAL > ZERO                                        
247300                                                                          
247400         IF RADER-FINNS                                                   
247500           MOVE TP4TRAN-IDDC-REC    TO W-711-IDDC                         
247600         ELSE                                                             
247700           PERFORM DEDA-GET-SDC-IDDC-VALUE                                
247800         END-IF                                                           
247900         MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                          
248000         PERFORM IMS-GHU-WDK711                                           
248100         SUBTRACT WS-KVORAPP-TOTAL  FROM SLAG-KVBEART                     
248200         PERFORM IMS-REPL-WDK7                                            
248300       ELSE                                                               
248400          IF  DIST35-NONVCC-CDC-REFILL                                    
248500          AND SPAR-ORAD-FLRESTN = NEJ                                     
248600          AND WS-KVORAPP-TOTAL > ZERO                                     
248700                                                                          
248800             PERFORM DEDA-GET-SDC-IDDC-VALUE                              
248900                                                                          
249000             MOVE SPAR-ORAD-IDARTNR  TO W-IDARTNR                         
249100             PERFORM IMS-GHU-WDK611                                       
249200             SUBTRACT WS-KVORAPP-TOTAL FROM CLAG-KVBEART                  
249300             PERFORM IMS-REPL-WDK611                                      
249400          END-IF                                                          
249500       END-IF                                                             
249600                                                                          
249700*NDC&SDC OCH RESTNOTERING (SLATTGRÄNSEN ÖVERSKRIDEN).                     
249800       IF (DIST35-REFILL                                                  
249900        OR DIST35-REFILL-INOM-NDC                                         
250000        OR DIST35-REFILL-NA-JAP                                           
250100        OR DIST35-NA-TRANSFER                                             
250200        OR DIST35-NA-NDC-RETURNS                                          
250300        OR DIST35-PACIFIC-TRANSFER                                        
250400        OR DIST35-REFILL-INOM-JP                                          
250500        OR DIST35-CN-TRANSFER                                             
250600        OR RADER-FINNS)                                                   
250700       AND SPAR-ORAD-FLRESTN = JA                                         
250800       AND WS-KVORAPP-TOTAL > ZERO                                        
250900       AND SPAR-ORAD-KVLEVART >= SPAR-ORAD-KVBEART -                      
251000                                 SPAR-ORAD-KVANNANT -                     
251100                                 SPAR-ORAD-KVSLATT                        
251200                                                                          
251300                                                                          
251400         IF RADER-FINNS                                                   
251500           MOVE TP4TRAN-IDDC-REC    TO W-711-IDDC                         
251600         ELSE                                                             
251700           PERFORM DEDA-GET-SDC-IDDC-VALUE                                
251800         END-IF                                                           
251900         MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                          
252000         PERFORM IMS-GHU-WDK711                                           
252100         SUBTRACT WS-KVORAPP-TOTAL FROM SLAG-KVBEART                      
252200         PERFORM IMS-REPL-WDK7                                            
252300       ELSE                                                               
252400       IF  DIST35-NONVCC-CDC-REFILL                                       
252500       AND SPAR-ORAD-FLRESTN = JA                                         
252600       AND WS-KVORAPP-TOTAL > ZERO                                        
252700       AND SPAR-ORAD-KVLEVART >= SPAR-ORAD-KVBEART -                      
252800                                 SPAR-ORAD-KVANNANT -                     
252900                                 SPAR-ORAD-KVSLATT                        
253000                                                                          
253100                                                                          
253200          PERFORM DEDA-GET-SDC-IDDC-VALUE                                 
253300          MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                         
253400          PERFORM IMS-GHU-WDK611                                          
253500          SUBTRACT WS-KVORAPP-TOTAL FROM CLAG-KVBEART                     
253600          PERFORM IMS-REPL-WDK611                                         
253700         END-IF                                                           
253800       END-IF                                                             
253900     SKIP2                                                                
254000     .                                                                    
254100 DEDA-GET-SDC-IDDC-VALUE            SECTION.                              
254200     MOVE 'DEDA-GET-SDC'         TO CURRENT-SECTION                       
254300                                                                          
254400     SEARCH ALL DIST57-REFILL-DC                                          
254500        AT END                                                            
254600           MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                           
254700                            TO ERRORTEX                                   
254800           CALL FELLOG                                                    
254900        WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR                 
255000           MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO W-711-IDDC              
255100     END-SEARCH                                                           
255200     .                                                                    
255300     EJECT                                                                
255400 DF-UPDATE-ORDERREG        SECTION.                                       
255500     MOVE 'DF-UPDATE   '         TO CURRENT-SECTION                       
255600                                                                          
255700     PERFORM IMS-GHU-WDE401                                               
255800                                                                          
255900     MOVE KORD-KDFAKTYP             TO  WS-KDFAKTYP                       
256000     COMPUTE KORD-VKORDNTO = KORD-VKORDNTO - SPAR-VKORDNTO-DEL            
256100     COMPUTE KORD-VLORDNTO = KORD-VLORDNTO - SPAR-VLORDNTO-DEL            
256200     IF SPAR-ORAD-FLDIRLEV = JA                                           
256300                                                                          
256400        COMPUTE KORD-SUORDV-LEVPL-LOC = KORD-SUORDV-LEVPL-LOC             
256500                                           - SPAR-SUORDV-DEL-LOC          
256600        COMPUTE KORD-SUORDV-LEVPL-LOC =                                   
256700               KORD-SUORDV-LEVPL-LOCPREL - SPAR-SUORDV-DEL-LOCPREL        
256800        COMPUTE KORD-SUORDV-LEVPL = KORD-SUORDV-LEVPL                     
256900                                     - SPAR-SUORDV-DEL                    
257000     ELSE                                                                 
257100        COMPUTE KORD-SUORDV-LOC = KORD-SUORDV-LOC                         
257200                                     - SPAR-SUORDV-DEL-LOC                
257300        COMPUTE KORD-SUORDV-LOCPREL = KORD-SUORDV-LOCPREL                 
257400                                     - SPAR-SUORDV-DEL-LOCPREL            
257500        COMPUTE KORD-SUORDV = KORD-SUORDV                                 
257600                                     - SPAR-SUORDV-DEL                    
257700     END-IF                                                               
257800     IF DATUM-SW = 'Y'                                                    
257900        MOVE WS-DAGENS-DATUM     TO KORD-TIBEGPAC                         
258000        MOVE 'N'                 TO DATUM-SW                              
258100     END-IF                                                               
258200     PERFORM IMS-REPL-WDE401                                              
258300*                                                                         
258400     MOVE ZERO TO SPAR-SUORDV-DEL                                         
258500     MOVE ZERO TO SPAR-SUORDV-DEL-LOC                                     
258600     MOVE ZERO TO SPAR-SUORDV-DEL-LOCPREL                                 
258700                  SPAR-VKORDNTO-DEL                                       
258800                  SPAR-VLORDNTO-DEL                                       
258900     .                                                                    
259000     EJECT                                                                
259100 DG-UPDATE-ROREG           SECTION.                                       
259200     MOVE 'DG-UPDATE   '         TO CURRENT-SECTION                       
259300                                                                          
259400     PERFORM DGE-HAMTA-TPOTYP-FRAN-ROREG                                  
259500     MOVE KORD-IDDISTR        TO TEST-IDDISTR                             
259600                                                                          
259700     IF WS-IDDC NOT = W-IDDC-B6                                           
259800        MOVE WS-IDDC TO W-IDDC-B6                                         
259900        PERFORM IMS-GU-WDB601                                             
260000     END-IF                                                               
260100                                                                          
260200     IF SPAR-ORAD-FLRESTN = JA AND                                        
260300        ( SPAR-ORAD-KVLEVART     < SPAR-ORAD-KVBEART                      
260400                                 - SPAR-ORAD-KVANNANT                     
260500                                 - SPAR-ORAD-KVSLATT )                    
260600                                                                          
260700        IF SPAR-ORAD-IDKUNDRF-RO = '00000     '                           
260800           PERFORM DGD-SAMMANSL-EJ-BIPACKAD-RAD                           
260900           IF WS-SAMMANSLAGNING-RAD = JA                                  
261000              IF DCS-CDC OR DCS-NDC                                       
261100                ADD WS-KVORAPP-TOTAL  TO OLD-RAD-KVART                    
261200              ELSE                                                        
261300                ADD WS-KVORAPP-PACK   TO OLD-RAD-KVART                    
261400              END-IF                                                      
261500              PERFORM IMS-REPL-ORDP01-OLD                                 
261600           ELSE                                                           
261700              PERFORM DGA-KATEGORI                                        
261800              PERFORM DGB-FLYTTA-WDA5-POSTER                              
261900              IF RAD-KVART > ZERO                                         
262000                PERFORM IMS-ISRT-ORDP01                                   
262100                PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                     
262200                   ADD +1               TO RAD-IDLOPNR                    
262300                   PERFORM IMS-ISRT-ORDP01                                
262400                END-PERFORM                                               
262500              END-IF                                                      
262600           END-IF                                                         
262700        ELSE                                                              
262800           MOVE WS-SAVE-IDDISTR       TO W1-IDDISTR                       
262900           MOVE WS-SAVE-IDKUNDNR      TO W1-IDKUNDNR                      
263000           MOVE SPAR-ORAD-IDKUNDRF-RO TO W1-IDKUNDRF                      
263100           MOVE SPAR-ORAD-IDARTNR     TO W1-IDARTNR                       
263200           MOVE SPAR-ORAD-IDLOPNR-RO  TO W1-IDLOPNR                       
263300                                                                          
263400           PERFORM IMS-GHU-ORDP01-GE                                      
263500           IF SEGMENT-SAKNAS                                              
263600*FIX START****************************************************            
263700*********** NÄR PGM ÅKER PÅ GE MOT WDA5 (ORDP01) *************            
263800                                                                          
263900             PERFORM DGA-KATEGORI                                         
264000             MOVE W1-IDARTNR          TO W-IDARTNR                        
264100             PERFORM IMS-GHU-ARTC11                                       
264200             MOVE DCS-IDDC            TO W-711-IDDC                       
264300             MOVE CLAG-IDANSK         TO WS-IDANSK                        
264400             IF DCS-NDC-CN OR (DCS-SDC AND DCS-CHINA)                     
264500               PERFORM IMS-GU-WDK722                                      
264600               IF SEGMENT-FINNS AND XLAG-IDANSK > 0                       
264700                  MOVE XLAG-IDANSK TO WS-IDANSK                           
264800               END-IF                                                     
264900             END-IF                                                       
265000                                                                          
265100             MOVE W1-IDDISTR          TO RAD-IDDISTR                      
265200             MOVE W1-IDKUNDNR         TO RAD-IDKUNDNR                     
265300             MOVE W1-IDKUNDRF         TO RAD-IDKUNDRF                     
265400             MOVE W1-IDARTNR          TO RAD-IDARTNR                      
265500             MOVE W1-IDLOPNR          TO RAD-IDLOPNR                      
265600             MOVE SPAR-ORAD-BERADREF  TO RAD-BERADREF                     
265700             MOVE NEJ                 TO RAD-FLERS                        
265800             MOVE WS-IDANSK           TO RAD-IDANSK                       
265900             MOVE SPAR-ORAD-IDANALYS  TO RAD-IDANALYS                     
266000             MOVE SPAR-ORAD-IDKONTO   TO RAD-IDKONTO                      
266100             MOVE SPAR-ORAD-IDKST     TO RAD-IDKST                        
266200             MOVE '00000     '        TO RAD-IDKUNDRF-LEV                 
266300             MOVE WS-IDDC             TO RAD-IDDC                         
266400             MOVE SPAR-ORAD-IDDC-RO   TO RAD-IDDC-RO                      
266500             MOVE SPAR-ORAD-KDDSP     TO RAD-KDDSP                        
266600             MOVE KORD-KDFAKTYP       TO RAD-KDFAKTYP                     
266700             MOVE SPAR-ORAD-KDFRAKT   TO RAD-KDFRAKT                      
266800             MOVE SPAR-ORAD-KDKVBRYT  TO RAD-KDKVBRYT                     
266900             MOVE SPAR-ORAD-KDOI      TO RAD-KDOI                         
267000             MOVE SPAR-ORAD-CLEARGROUP TO RAD-CLEARGROUP                  
267100             MOVE SPAR-ORAD-KDORDING  TO RAD-KDORDING                     
267200             MOVE KORD-KDORDKL        TO RAD-KDORDKL                      
267300             MOVE SPAR-ORAD-KDPRODSL  TO RAD-KDPRODSL                     
267400             MOVE WS-KDRAPRIO         TO RAD-KDRAPRIO                     
267500             MOVE +4                  TO RAD-KDROO                        
267600             MOVE '4'                 TO RAD-KDSTARAD                     
267700             MOVE WS-KDTPOTYP         TO RAD-KDTPOTYP                     
267800             MOVE SPAR-ORAD-KDVRINFO  TO RAD-KDVRINFO                     
267900             IF DCS-CDC OR DCS-NDC                                        
268000               MOVE WS-KVORAPP-TOTAL  TO RAD-KVART                        
268100                                              RAD-KVRO                    
268200             ELSE                                                         
268300               MOVE WS-KVORAPP-PACK   TO RAD-KVART                        
268400                                              RAD-KVRO                    
268500             END-IF                                                       
268600             MOVE SPAR-ORAD-PRARTNTO  TO RAD-PRARTNTO                     
268700* TL 030926  MOVE SPAR-ORAD-PRARTNTO-LOC                                  
268800*                                     TO RAD-PRARTNTO-LOC                 
268900*            MOVE SPAR-ORAD-PRARTNTO-LOCPREL                              
269000*                                     TO RAD-PRARTNTO-LOCPREL             
269100                                                                          
269200             MOVE SPAR-ORAD-DEAL-PR-LINE                                  
269300                                      TO RAD-DEAL-PR-LINE                 
269400*                                                                         
269500             MOVE SPAR-ORAD-REKSIFFR  TO RAD-REKSIFFR                     
269600             MOVE ZERO                TO RAD-TIAVBOKN                     
269700             MOVE DAT-TIAAMMDD        TO RAD-DARODAT                      
269800                                         RAD-TIREGDAT                     
269900             MOVE DAT-TISEKEL         TO RAD-DARODAT (1:2)                
270000             MOVE ZERO                TO RAD-TIRES                        
270100             MOVE +0                  TO RAD-TITPO                        
270200             MOVE SPAR-ORAD-KDPRTYP   TO RAD-KDPRTYP                      
270300             MOVE SPAR-ORAD-BEVOLREF  TO RAD-BEVOLREF                     
270400             MOVE SPAR-ORAD-FLINVEST  TO RAD-FLINVEST                     
270500             MOVE SPAR-ORAD-FLPRTILL  TO RAD-FLPRTILL                     
270600             MOVE JA                  TO RAD-FLTPOBEK                     
270700             MOVE SPAR-BEKUNDRF       TO RAD-BEKUNDRF                     
270800             MOVE SPAR-ORAD-IDKAMPRF  TO RAD-IDKAMPRF                     
270900             MOVE SPAR-ORAD-IDLEVNR   TO RAD-IDLEVNR                      
271000             MOVE SPAR-ORAD-IDSYSTEM  TO RAD-IDSYSTEM                     
271100             MOVE SPAR-ORAD-KVBEART   TO RAD-KVBEART-Q                    
271200             MOVE WS-TTMMSS           TO RAD-TIREGTID                     
271300             MOVE 0                   TO RAD-DASENDAT                     
271400             MOVE 0                   TO RAD-TISENBEK-KL                  
271500             MOVE OHUV-KDORDTYP-LDC   TO RAD-KDORDTYP-LDC                 
271600             MOVE OHUV-TIREPDAT       TO RAD-TIREPDAT                     
271700             MOVE SPAR-ORAD-IDKUNDRF-WIP TO RAD-IDKUNDRF-WIP              
271800             MOVE SPAR-ORAD-PRAVCOST  TO RAD-PRAVCOST                     
271900             MOVE ARB-KDROPACK        TO RAD-KDROPACK                     
272000             MOVE SPAR-ORAD-IDARBREF  TO RAD-IDARBREF                     
272100             PERFORM S36-ANDRA-WDC711                                     
272200                                                                          
272300             PERFORM IMS-ISRT-ORDP01                                      
272400              IF RAD-KVART > ZERO                                         
272500                PERFORM IMS-ISRT-ORDP01                                   
272600                PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                     
272700                   ADD +1               TO RAD-IDLOPNR                    
272800                   PERFORM IMS-ISRT-ORDP01                                
272900                END-PERFORM                                               
273000              END-IF                                                      
273100             PERFORM IMS-GHU-ORDP01                                       
273200           END-IF                                                         
273300*********** FIX SLUT  ****************************************            
273400                                                                          
273500           IF RAD-DARODAT = ZERO                                          
273600             MOVE JA TO SW-TIRODAT-LIKA-MED-ZERO                          
273700           END-IF                                                         
273800                                                                          
273900                                                                          
274000           PERFORM DGC-SAMMANSL-BIPACKAD-RAD                              
274100                                                                          
274200           IF WS-SAMMANSLAGNING-RAD = JA                                  
274300              IF DCS-CDC OR DCS-NDC                                       
274400                IF WS-KVORAPP-TOTAL = RAD-KVART                           
274500                  ADD WS-KVORAPP-TOTAL     TO OLD-RAD-KVART               
274600                  PERFORM IMS-REPL-ORDP01-OLD                             
274700                                                                          
274800                  MOVE KORD-IDDISTR          TO W1-IDDISTR                
274900                  MOVE KORD-IDKUNDNR         TO W1-IDKUNDNR               
275000                  MOVE SPAR-ORAD-IDKUNDRF-RO TO W1-IDKUNDRF               
275100                  MOVE SPAR-ORAD-IDARTNR     TO W1-IDARTNR                
275200                  MOVE SPAR-ORAD-IDLOPNR-RO  TO W1-IDLOPNR                
275300                  PERFORM IMS-GHU-ORDP01                                  
275400                  PERFORM IMS-DLET-ORDP01                                 
275500                                                                          
275600                ELSE                                                      
275700                  SUBTRACT WS-KVORAPP-TOTAL  FROM RAD-KVART               
275800                  PERFORM IMS-REPL-ORDP01                                 
275900                                                                          
276000                  ADD WS-KVORAPP-TOTAL       TO OLD-RAD-KVART             
276100                  PERFORM IMS-REPL-ORDP01-OLD                             
276200                END-IF                                                    
276300              ELSE                                                        
276400                IF WS-KVORAPP-PACK = RAD-KVART                            
276500                  ADD WS-KVORAPP-PACK    TO OLD-RAD-KVART                 
276600                  PERFORM IMS-REPL-ORDP01-OLD                             
276700                                                                          
276800                  MOVE KORD-IDDISTR          TO W1-IDDISTR                
276900                  MOVE KORD-IDKUNDNR         TO W1-IDKUNDNR               
277000                  MOVE SPAR-ORAD-IDKUNDRF-RO TO W1-IDKUNDRF               
277100                  MOVE SPAR-ORAD-IDARTNR     TO W1-IDARTNR                
277200                  MOVE SPAR-ORAD-IDLOPNR-RO  TO W1-IDLOPNR                
277300                  PERFORM IMS-GHU-ORDP01                                  
277400                  PERFORM IMS-DLET-ORDP01                                 
277500                ELSE                                                      
277600                  SUBTRACT WS-KVORAPP-PACK   FROM RAD-KVART               
277700                  PERFORM IMS-REPL-ORDP01                                 
277800                                                                          
277900                  ADD WS-KVORAPP-PACK        TO OLD-RAD-KVART             
278000                  PERFORM IMS-REPL-ORDP01-OLD                             
278100                END-IF                                                    
278200              END-IF                                                      
278300           ELSE                                                           
278400             IF DCS-CDC OR DCS-NDC                                        
278500              IF WS-KVORAPP-TOTAL = RAD-KVART                             
278600                 IF RAD-DARODAT = 0                                       
278700                    MOVE DAT-TIAAMMDD TO RAD-DARODAT                      
278800                    MOVE DAT-TISEKEL  TO RAD-DARODAT (1:2)                
278900                 END-IF                                                   
279000                                                                          
279100                 MOVE +0                TO RAD-TIRES                      
279200                 MOVE +0                TO RAD-TIAVBOKN                   
279300                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
279400                 MOVE SPAR-ORAD-CLEARGROUP                                
279500                                        TO RAD-CLEARGROUP                 
279600                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
279700                                           RAD-IDDC-RO                    
279800                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
279900                 MOVE '2'               TO RAD-KDSTARAD                   
280000                 PERFORM S36-ANDRA-WDC711                                 
280100                 PERFORM IMS-REPL-ORDP01                                  
280200              ELSE                                                        
280300                 SUBTRACT WS-KVORAPP-TOTAL FROM RAD-KVART                 
280400                 PERFORM IMS-REPL-ORDP01                                  
280500                                                                          
280600                 IF RAD-DARODAT = 0                                       
280700                    MOVE DAT-TIAAMMDD TO RAD-DARODAT                      
280800                    MOVE DAT-TISEKEL  TO RAD-DARODAT (1:2)                
280900                 END-IF                                                   
281000                                                                          
281100                 MOVE +0                TO RAD-TIRES                      
281200                 MOVE +0                TO RAD-TIAVBOKN                   
281300                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
281400                 MOVE SPAR-ORAD-CLEARGROUP                                
281500                                        TO RAD-CLEARGROUP                 
281600                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
281700                                           RAD-IDDC-RO                    
281800                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
281900                 MOVE WS-KVORAPP-TOTAL  TO RAD-KVART                      
282000                 MOVE '2'               TO RAD-KDSTARAD                   
282100                 ADD +1                 TO RAD-IDLOPNR                    
282200                 PERFORM S36-ANDRA-WDC711                                 
282300                 PERFORM IMS-ISRT-ORDP01                                  
282400                 PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                    
282500                   ADD +1             TO RAD-IDLOPNR                      
282600                   PERFORM IMS-ISRT-ORDP01                                
282700                 END-PERFORM                                              
282800              END-IF                                                      
282900             ELSE                                                         
283000              IF WS-KVORAPP-PACK = RAD-KVART                              
283100                 IF RAD-DARODAT = 0                                       
283200                    MOVE DAT-TIAAMMDD TO RAD-DARODAT                      
283300                    MOVE DAT-TISEKEL  TO RAD-DARODAT (1:2)                
283400                 END-IF                                                   
283500                                                                          
283600                 MOVE +0                TO RAD-TIRES                      
283700                 MOVE +0                TO RAD-TIAVBOKN                   
283800                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
283900                 MOVE SPAR-ORAD-CLEARGROUP                                
284000                                        TO RAD-CLEARGROUP                 
284100                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
284200                                           RAD-IDDC-RO                    
284300                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
284400                 MOVE '2'               TO RAD-KDSTARAD                   
284500                 PERFORM S36-ANDRA-WDC711                                 
284600                 PERFORM IMS-REPL-ORDP01                                  
284700              ELSE                                                        
284800                 SUBTRACT WS-KVORAPP-PACK FROM RAD-KVART                  
284900                 PERFORM IMS-REPL-ORDP01                                  
285000                                                                          
285100                 IF RAD-DARODAT = 0                                       
285200                    MOVE DAT-TIAAMMDD TO RAD-DARODAT                      
285300                    MOVE DAT-TISEKEL  TO RAD-DARODAT (1:2)                
285400                 END-IF                                                   
285500                                                                          
285600                 MOVE +0                TO RAD-TIRES                      
285700                 MOVE +0                TO RAD-TIAVBOKN                   
285800                 MOVE SPAR-ORAD-KDOI    TO RAD-KDOI                       
285900                 MOVE SPAR-ORAD-CLEARGROUP                                
286000                                        TO RAD-CLEARGROUP                 
286100                 MOVE SPAR-ORAD-IDDC-RO TO RAD-IDDC                       
286200                                           RAD-IDDC-RO                    
286300                 MOVE '00000     '      TO RAD-IDKUNDRF-LEV               
286400                 MOVE WS-KVORAPP-PACK   TO RAD-KVART                      
286500                 MOVE '2'               TO RAD-KDSTARAD                   
286600                 ADD +1                 TO RAD-IDLOPNR                    
286700                 PERFORM S36-ANDRA-WDC711                                 
286800                 PERFORM IMS-ISRT-ORDP01                                  
286900                 PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                    
287000                   ADD +1             TO RAD-IDLOPNR                      
287100                   PERFORM IMS-ISRT-ORDP01                                
287200                 END-PERFORM                                              
287300              END-IF                                                      
287400             END-IF                                                       
287500           END-IF                                                         
287600        END-IF                                                            
287700     END-IF                                                               
287800     EJECT                                                                
287900     .                                                                    
288000 DGA-KATEGORI              SECTION.                                       
288100     MOVE 'DGA-KATERGORI'        TO CURRENT-SECTION                       
288200                                                                          
288300*    I DENNA SEKTION LÄSES STYRREG FÖR ATT BESTÄMMA PRIO FÖR              
288400*    DEN NYA ORDERKLASSEN.                                                
288500                                                                          
288600     MOVE LOW-VALUE                  TO W-WDGXKEY-N5-MIN                  
288700     MOVE HIGH-VALUE                 TO W-WDGXKEY-N5-MAX                  
288800     MOVE LOW-VALUE                  TO W-KDRAPRIO-N5-MIN-X               
288900     MOVE HIGH-VALUE                 TO W-KDRAPRIO-N5-MAX-X               
289000     MOVE WS-KDTPOTYP                TO W-KDTPOTYP-N5                     
289100     MOVE KORD-IDDISTR               TO W-IDDISTR-FOM-N5                  
289200     MOVE KORD-IDDISTR               TO W-IDDISTR-TOM-N5                  
289300     MOVE SPAR-ORAD-KDORDKL          TO W-KDORDKL-N5                      
289400     MOVE '4511'                     TO W-IDHTYP-N5                       
289500     MOVE LOW-VALUE                  TO W-VALFRI-N5                       
289600                                                                          
289700     PERFORM IMS-GU-XXJN                                                  
289800                                                                          
289900     MOVE STYR-4512-KDRAPRIO         TO WS-KDRAPRIO                       
290000     EJECT                                                                
290100     .                                                                    
290200 DGB-FLYTTA-WDA5-POSTER    SECTION.                                       
290300     MOVE 'DGB-FLYTTA-  '        TO CURRENT-SECTION                       
290400                                                                          
290500     MOVE KORD-IDDISTR               TO RAD-IDDISTR                       
290600     MOVE KORD-IDKUNDNR              TO RAD-IDKUNDNR                      
290700     MOVE KORD-IDKUNDRF              TO RAD-IDKUNDRF                      
290800     MOVE SPAR-ORAD-IDARTNR          TO RAD-IDARTNR                       
290900     MOVE +1                         TO RAD-IDLOPNR                       
291000     MOVE SPAR-ORAD-BERADREF         TO RAD-BERADREF                      
291100     MOVE 'N'                        TO RAD-FLERS                         
291200                                                                          
291300     MOVE WS-IDANSK                  TO RAD-IDANSK                        
291400     MOVE SPAR-ORAD-IDKONTO          TO RAD-IDKONTO                       
291500     MOVE SPAR-ORAD-IDKST            TO RAD-IDKST                         
291600     MOVE SPAR-ORAD-IDANALYS         TO RAD-IDANALYS                      
291700     MOVE '00000     '               TO RAD-IDKUNDRF-LEV                  
291800     MOVE SPAR-ORAD-KDOI             TO RAD-KDOI                          
291900     MOVE SPAR-ORAD-CLEARGROUP       TO RAD-CLEARGROUP                    
292000     MOVE SPAR-ORAD-IDDC-RO          TO RAD-IDDC-RO                       
292100                                        RAD-IDDC                          
292200     MOVE SPAR-ORAD-KDDSP            TO RAD-KDDSP                         
292300     MOVE KORD-KDFAKTYP              TO RAD-KDFAKTYP                      
292400     MOVE SPAR-ORAD-KDFRAKT          TO RAD-KDFRAKT                       
292500     MOVE SPAR-ORAD-KDKVBRYT         TO RAD-KDKVBRYT                      
292600     MOVE SPAR-ORAD-KDORDING         TO RAD-KDORDING                      
292700     MOVE KORD-KDORDKL               TO RAD-KDORDKL                       
292800     MOVE SPAR-ORAD-KDPRODSL         TO RAD-KDPRODSL                      
292900     MOVE WS-KDRAPRIO                TO RAD-KDRAPRIO                      
293000     MOVE +4                         TO RAD-KDROO                         
293100     MOVE '2'                        TO RAD-KDSTARAD                      
293200     MOVE WS-KDTPOTYP                TO RAD-KDTPOTYP                      
293300     MOVE SPAR-ORAD-KDVRINFO         TO RAD-KDVRINFO                      
293400     IF DCS-CDC OR DCS-NDC                                                
293500       MOVE WS-KVORAPP-TOTAL         TO RAD-KVART                         
293600                                        RAD-KVRO                          
293700     ELSE                                                                 
293800       MOVE WS-KVORAPP-PACK          TO RAD-KVART                         
293900                                        RAD-KVRO                          
294000     END-IF                                                               
294100     MOVE SPAR-ORAD-PRARTNTO         TO RAD-PRARTNTO                      
294200     MOVE SPAR-ORAD-PRARTNTO-LOC     TO RAD-PRARTNTO-LOC                  
294300     MOVE SPAR-ORAD-PRARTNTO-LOCPREL TO RAD-PRARTNTO-LOCPREL              
294400     MOVE SPAR-ORAD-REKSIFFR         TO RAD-REKSIFFR                      
294500     MOVE ZERO                       TO RAD-TIAVBOKN                      
294600     MOVE DAT-TIAAMMDD               TO RAD-DARODAT                       
294700                                        RAD-TIREGDAT                      
294800     MOVE DAT-TISEKEL                TO RAD-DARODAT (1:2)                 
294900     MOVE ZERO                       TO RAD-TIRES                         
295000     MOVE +0                         TO RAD-TITPO                         
295100     MOVE JA                         TO RAD-FLTPOBEK                      
295200     MOVE SPAR-BEKUNDRF              TO RAD-BEKUNDRF                      
295300     MOVE SPAR-ORAD-BEVOLREF         TO RAD-BEVOLREF                      
295400     MOVE SPAR-ORAD-IDKAMPRF         TO RAD-IDKAMPRF                      
295500     MOVE SPAR-ORAD-IDLEVNR          TO RAD-IDLEVNR                       
295600     MOVE SPAR-ORAD-IDSYSTEM         TO RAD-IDSYSTEM                      
295700     MOVE SPAR-ORAD-KVBEART          TO RAD-KVBEART-Q                     
295800     MOVE WS-TTMMSS                  TO RAD-TIREGTID                      
295900     MOVE 0                          TO RAD-DASENDAT                      
296000     MOVE 0                          TO RAD-TISENBEK-KL                   
296100     MOVE SPAR-ORAD-KDPRTYP          TO RAD-KDPRTYP                       
296200     MOVE SPAR-ORAD-FLINVEST         TO RAD-FLINVEST                      
296300     MOVE SPAR-ORAD-FLPRTILL         TO RAD-FLPRTILL                      
296400     IF  SPAR-ORAD-PRARTBTO-LOC NUMERIC                                   
296500     AND SPAR-ORAD-IDPRQUES     NUMERIC                                   
296600       MOVE SPAR-ORAD-IDPRQUES       TO RAD-IDPRQUES                      
296700       MOVE SPAR-ORAD-PRARTBTO-LOC   TO RAD-PRARTBTO-LOC                  
296800       IF SPAR-ORAD-PRAVCOST > ZERO                                       
296900          MOVE SPAR-ORAD-KDVALISO-EXP TO RAD-KDVALISO                     
297000       ELSE                                                               
297100          MOVE SPAR-ORAD-KDVALISO     TO RAD-KDVALISO                     
297200       END-IF                                                             
297300       MOVE SPAR-ORAD-KDVAT          TO RAD-KDVAT                         
297400       MOVE SPAR-ORAD-RERAB          TO RAD-RERAB                         
297500       MOVE SPAR-ORAD-KDRAB          TO RAD-KDRAB                         
297600       MOVE SPAR-ORAD-BEART-VIPS     TO RAD-BEART-VIPS                    
297700     ELSE                                                                 
297800       MOVE ZERO                     TO RAD-IDPRQUES                      
297900       MOVE ZERO                     TO RAD-PRARTBTO-LOC                  
298000       MOVE SPACE                    TO RAD-KDVALISO                      
298100       MOVE SPACE                    TO RAD-KDVAT                         
298200       MOVE ZERO                     TO RAD-RERAB                         
298300       MOVE SPACE                    TO RAD-KDRAB                         
298400       MOVE SPACE                    TO RAD-BEART-VIPS                    
298500     END-IF                                                               
298600                                                                          
298700     MOVE OHUV-KDORDTYP-LDC          TO RAD-KDORDTYP-LDC                  
298800     MOVE OHUV-TIREPDAT              TO RAD-TIREPDAT                      
298900     MOVE SPAR-ORAD-IDKUNDRF-WIP     TO RAD-IDKUNDRF-WIP                  
299000     MOVE SPAR-ORAD-PRAVCOST         TO RAD-PRAVCOST                      
299100     MOVE ARB-KDROPACK               TO RAD-KDROPACK                      
299200     MOVE SPAR-ORAD-IDARBREF         TO RAD-IDARBREF                      
299300                                                                          
299400*                                                                         
299500     PERFORM DGBA-KOLLA-CROSS-DOCKING                                     
299600     SKIP2                                                                
299700     .                                                                    
299800     EJECT                                                                
299900 DGBA-KOLLA-CROSS-DOCKING   SECTION.                                      
300000     MOVE 'DGBA-KOLLA-  '    TO CURRENT-SECTION                           
300100                                                                          
300200     MOVE SPAR-ORAD-IDARTNR                                               
300300                             TO W-IDARTNR                                 
300400     PERFORM IMS-GHU-ARTC11                                               
300500     MOVE DCS-IDDC TO W-711-IDDC                                          
300600*                                                                         
300700     MOVE CLAG-IDANSK        TO WS-IDANSK                                 
300800     IF DCS-NDC-CN OR (DCS-SDC AND DCS-CHINA)                             
300900       PERFORM IMS-GU-WDK722                                              
301000       IF SEGMENT-FINNS AND XLAG-IDANSK > 0                               
301100          MOVE XLAG-IDANSK TO WS-IDANSK                                   
301200       END-IF                                                             
301300     END-IF                                                               
301400*                                                                         
301500     MOVE 1                  TO IX-CD-OMR                                 
301600     PERFORM UNTIL IX-CD-OMR > 4                                          
301700     OR SPAR-ORAD-ADLAGOMR = CLAG-ADLAGOMR-CD (IX-CD-OMR)                 
301800       ADD 1                 TO IX-CD-OMR                                 
301900     END-PERFORM                                                          
302000     IF IX-CD-OMR <= 4                                                    
302100*                                                                         
302200*BOKA UPP CROSS DOCKING SALDO                                             
302300*                                                                         
302400        ADD SPAR-ORAD-KVBEART                                             
302500                         TO CLAG-KVLS-CD (IX-CD-OMR)                      
302600     END-IF                                                               
302700     PERFORM IMS-REPL-ARTC                                                
302800     SKIP2                                                                
302900     .                                                                    
303000 DGC-SAMMANSL-BIPACKAD-RAD SECTION.                                       
303100     MOVE 'DGC-SAMMANSL-'        TO CURRENT-SECTION                       
303200                                                                          
303300     MOVE RAD-IDDISTR                TO W1-IDDISTR W2-IDDISTR             
303400     MOVE RAD-IDKUNDNR               TO W1-IDKUNDNR W2-IDKUNDNR           
303500     MOVE RAD-IDKUNDRF               TO W1-IDKUNDRF W2-IDKUNDRF           
303600     MOVE RAD-IDARTNR                TO W1-IDARTNR  W2-IDARTNR            
303700     MOVE +0                         TO W1-IDLOPNR                        
303800     MOVE +999                       TO W2-IDLOPNR                        
303900     MOVE '2'                        TO W-KDSTARAD                        
304000                                                                          
304100     PERFORM IMS-GHU-ORDP01-OLD                                           
304200                                                                          
304300     MOVE NEJ                        TO WS-SAMMANSLAGNING-RAD             
304400                                                                          
304500     PERFORM UNTIL NOT SEGMENT-FINNS   OR                                 
304600                       WS-SAMMANSLAGNING-RAD = JA                         
304700                                                                          
304800        IF  OLD-RAD-KDFRAKT  = RAD-KDFRAKT                                
304900          AND OLD-RAD-KDORDKL  = RAD-KDORDKL                              
305000          AND OLD-RAD-PRARTNTO = RAD-PRARTNTO                             
305100          AND OLD-RAD-DEAL-PR-LINE = RAD-DEAL-PR-LINE                     
305200          AND OLD-RAD-KDTPOTYP = RAD-KDTPOTYP                             
305300          AND OLD-RAD-IDKONTO  = RAD-IDKONTO                              
305400          AND OLD-RAD-IDKST    = RAD-IDKST                                
305500                                                                          
305600           MOVE JA                   TO WS-SAMMANSLAGNING-RAD             
305700        ELSE                                                              
305800           PERFORM IMS-GHN-ORDP01-OLD                                     
305900        END-IF                                                            
306000     END-PERFORM                                                          
306100     EJECT                                                                
306200     .                                                                    
306300 DGD-SAMMANSL-EJ-BIPACKAD-RAD SECTION.                                    
306400     MOVE 'DGD-SAMMANSL-'        TO CURRENT-SECTION                       
306500                                                                          
306600     MOVE KORD-IDDISTR               TO W1-IDDISTR  W2-IDDISTR            
306700     MOVE KORD-IDKUNDNR              TO W1-IDKUNDNR W2-IDKUNDNR           
306800     MOVE KORD-IDKUNDRF              TO W1-IDKUNDRF W2-IDKUNDRF           
306900     MOVE SPAR-ORAD-IDARTNR          TO W1-IDARTNR  W2-IDARTNR            
307000     MOVE +0                         TO W1-IDLOPNR                        
307100     MOVE +999                       TO W2-IDLOPNR                        
307200     MOVE '2'                        TO W-KDSTARAD                        
307300                                                                          
307400     MOVE NEJ                        TO WS-SAMMANSLAGNING-RAD             
307500                                                                          
307600     PERFORM IMS-GHU-ORDP01-OLD                                           
307700                                                                          
307800     PERFORM UNTIL NOT SEGMENT-FINNS      OR                              
307900                       WS-SAMMANSLAGNING-RAD = JA                         
308000                                                                          
308100        IF  OLD-RAD-KDFRAKT  = KORD-KDFRAKT                               
308200          AND OLD-RAD-KDORDKL  = KORD-KDORDKL                             
308300          AND OLD-RAD-PRARTNTO = SPAR-ORAD-PRARTNTO                       
308400          AND OLD-RAD-PRARTNTO-LOC = SPAR-ORAD-PRARTNTO-LOC               
308500          AND OLD-RAD-PRARTNTO-LOCPREL =                                  
308600                                     SPAR-ORAD-PRARTNTO-LOCPREL           
308700          AND OLD-RAD-KDTPOTYP = WS-KDTPOTYP                              
308800          AND OLD-RAD-IDKONTO  = SPAR-ORAD-IDKONTO                        
308900          AND OLD-RAD-IDKST    = SPAR-ORAD-IDKST                          
309000                                                                          
309100           MOVE JA                   TO WS-SAMMANSLAGNING-RAD             
309200        ELSE                                                              
309300           PERFORM IMS-GHN-ORDP01-OLD                                     
309400        END-IF                                                            
309500     END-PERFORM                                                          
309600     .                                                                    
309700     EJECT                                                                
309800                                                                          
309900 DGE-HAMTA-TPOTYP-FRAN-ROREG             SECTION.                         
310000     MOVE 'DGE-HAMATA-TPO'       TO CURRENT-SECTION                       
310100                                                                          
310200     MOVE KORD-IDDISTR               TO W1-IDDISTR  W2-IDDISTR            
310300     MOVE KORD-IDKUNDNR              TO W1-IDKUNDNR W2-IDKUNDNR           
310400                                                                          
310500     IF SPAR-ORAD-IDKUNDRF-RO = '00000     '                              
310600       MOVE KORD-IDKUNDRF            TO W1-IDKUNDRF W2-IDKUNDRF           
310700     ELSE                                                                 
310800       MOVE SPAR-ORAD-IDKUNDRF-RO    TO W1-IDKUNDRF W2-IDKUNDRF           
310900     END-IF                                                               
311000                                                                          
311100     MOVE SPAR-ORAD-IDARTNR          TO W1-IDARTNR  W2-IDARTNR            
311200     MOVE SPAR-ORAD-IDLOPNR-RO       TO W1-IDLOPNR  W2-IDLOPNR            
311300     MOVE '4'                        TO W-KDSTARAD                        
311400                                                                          
311500     PERFORM IMS-GHU-ORDP01-OLD                                           
311600                                                                          
311700     IF SEGMENT-FINNS                                                     
311800       MOVE OLD-RAD-KDTPOTYP         TO WS-KDTPOTYP                       
311900     ELSE                                                                 
312000       MOVE 0                        TO WS-KDTPOTYP                       
312100     END-IF                                                               
312200     .                                                                    
312300     EJECT                                                                
312400                                                                          
312500                                                                          
312600 DH-LAS-ARTREG            SECTION.                                        
312700     MOVE 'DH-LAS-ARTREG '    TO CURRENT-SECTION                          
312800                                                                          
312900     MOVE SPAR-ORAD-IDARTNR   TO W-IDARTNR                                
313000                                                                          
313100     PERFORM IMS-GU-ARTC11                                                
313200     MOVE KORD-IDDC           TO W-711-IDDC                               
313300*                                                                         
313400     MOVE CLAG-IDANSK         TO WS-IDANSK                                
313500     IF DCS-NDC-CN OR (DCS-SDC AND DCS-CHINA)                             
313600       PERFORM IMS-GU-WDK722                                              
313700       IF SEGMENT-FINNS AND XLAG-IDANSK > 0                               
313800         MOVE XLAG-IDANSK     TO WS-IDANSK                                
313900       END-IF                                                             
314000     END-IF                                                               
314100     .                                                                    
314200     EJECT                                                                
314300 DJ-BESTAM-ORDERBEKR-KOD  SECTION.                                        
314400     MOVE 'DJ-BESTAM-ORDER'      TO CURRENT-SECTION                       
314500                                                                          
314600     MOVE +000                         TO WS-KDORDBEK                     
314700     IF  SPAR-ORAD-FLSDCLEV = JA                                          
314800        MOVE +080                         TO WS-KDORDBEK                  
314900*           80 = INTE RESTNOTERING, GÖR NY BESTÄLLNING.                   
315000     ELSE                                                                 
315100       IF SPAR-ORAD-KDORDKL > 0                                           
315200                                                                          
315300          IF SPAR-ORAD-KVSLATT > 0                                        
315400                                                                          
315500             IF SPAR-ORAD-KVLEVART < SPAR-ORAD-KVBEART -                  
315600                                     SPAR-ORAD-KVANNANT -                 
315700                                     SPAR-ORAD-KVSLATT                    
315800                                                                          
315900                PERFORM DJA-SATT-KOD80-90-91                              
316000             ELSE                                                         
316100                MOVE +081                   TO WS-KDORDBEK                
316200*           81 = INTE RESTNOTERING, GÖR NY BESTÄLLNING, SLATT.            
316300             END-IF                                                       
316400          ELSE                                                            
316500             PERFORM DJA-SATT-KOD80-90-91                                 
316600          END-IF                                                          
316700       ELSE                                                               
316800         MOVE +093                        TO WS-KDORDBEK                  
316900*           93 = VOR, RESTNOTERAD KVANT, FYSISK AVVIKELSE                 
317000       END-IF                                                             
317100     END-IF                                                               
317200     .                                                                    
317300     EJECT                                                                
317400 DJA-SATT-KOD80-90-91     SECTION.                                        
317500     MOVE 'DJA-SATT-KOD80-'      TO CURRENT-SECTION                       
317600                                                                          
317700     IF SPAR-ORAD-FLRESTN = JA                                            
317800        IF SPAR-ORAD-IDKUNDRF-RO > '00000     '                           
317900           MOVE +091                      TO WS-KDORDBEK                  
318000*           91 = RESTNOTERAD IGEN.                                        
318100        ELSE                                                              
318200           MOVE +090                      TO WS-KDORDBEK                  
318300*           90 = RESTNOTERAD                                              
318400        END-IF                                                            
318500     ELSE                                                                 
318600        MOVE +080                         TO WS-KDORDBEK                  
318700*           80 = INTE RESTNOTERING, GÖR NY BESTÄLLNING.                   
318800     END-IF                                                               
318900     .                                                                    
319000     EJECT                                                                
319100 DK-UPDATE-EV-KAMP-REG     SECTION.                                       
319200     MOVE 'DK-UPDATE-EV-KA'      TO CURRENT-SECTION                       
319300                                                                          
319400     IF SPAR-ORAD-IDKAMPRF     > 0 AND                                    
319500        (WS-KDORDBEK           = 80 OR 81)                                
319600       COMPUTE WS-KVSLATTAT  = SPAR-ORAD-KVBEART -                        
319700                               SPAR-ORAD-KVLEVART -                       
319800                               SPAR-ORAD-KVANNANT                         
319900       PERFORM DKA-UPDATE-WDM211                                          
320000       PERFORM DKB-UPDATE-WDM221                                          
320100     END-IF                                                               
320200     .                                                                    
320300     EJECT                                                                
320400 DKA-UPDATE-WDM211   SECTION.                                             
320500     MOVE 'DKA-UPDATE-WDM211'    TO CURRENT-SECTION                       
320600                                                                          
320700     MOVE SPAR-ORAD-IDKAMPRF       TO W-KAMP-IDKAMPRF                     
320800     MOVE KORD-IDDC                TO W-KAMP-IDDC                         
320900     MOVE SPAR-ORAD-IDARTNR        TO W-KART-IDARTNR                      
321000     PERFORM IMS-GHU-WDM211                                               
321100     IF SEGMENT-FINNS                                                     
321200        MOVE KART-KVRESS-ART       TO SPAR-KART-KVRESS-ART                
321300                                                                          
321400        IF KART-KVBEART-KUND     >= WS-KVSLATTAT                          
321500            SUBTRACT WS-KVSLATTAT     FROM KART-KVBEART-KUND              
321600            COMPUTE KART-KVRESS-ART = KART-KVRESS-ART                     
321700                                    + WS-KVSLATTAT                        
321800         ELSE                                                             
321900            MOVE 'WDM211 KART-KVBEART-KUND BLIR NEGATIV'                  
322000                                   TO ERRORTEX                            
322100            CALL ABEND USING RKOD-ABEND-MED-DUMP                          
322200        END-IF                                                            
322300        PERFORM IMS-REPL-WDM211                                           
322400     END-IF                                                               
322500     .                                                                    
322600     EJECT                                                                
322700 DKB-UPDATE-WDM221   SECTION.                                             
322800     MOVE 'DKB-UPDATE-WDM221'    TO CURRENT-SECTION                       
322900                                                                          
323000     MOVE SPAR-ORAD-IDKAMPRF   TO W-KAMP-IDKAMPRF                         
323100     MOVE KORD-IDDC            TO W-KAMP-IDDC                             
323200     MOVE SPAR-ORAD-IDARTNR    TO W-KART-IDARTNR                          
323300     MOVE WS-IDDISTR           TO W-KMRK-IDDISTR-FOM                      
323400     MOVE WS-IDDISTR           TO W-KMRK-IDDISTR-TOM                      
323500     MOVE WS-IDKUNDNR          TO W-KMRK-IDKUNDNR-FOM                     
323600     MOVE WS-IDKUNDNR          TO W-KMRK-IDKUNDNR-TOM                     
323700                                                                          
323800     PERFORM S20-FINN-INTERVALL                                           
323900                                                                          
324000     PERFORM IMS-GHU-WDM221                                               
324100                                                                          
324200     IF SEGMENT-FINNS                                                     
324300         IF KMRK-KVBEART-KUND >= WS-KVSLATTAT                             
324400             SUBTRACT WS-KVSLATTAT                                        
324500                                FROM KMRK-KVBEART-KUND                    
324600          ELSE                                                            
324700             MOVE 'WDM221 KMRK-KVBEART-KUND BLIR NEGATIV'                 
324800                                TO ERRORTEX                               
324900             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
325000         END-IF                                                           
325100         PERFORM IMS-REPL-WDM221                                          
325200     END-IF                                                               
325300     .                                                                    
325400     EJECT                                                                
325500 DL-UPDATE-INVENTORY       SECTION.                                       
325600     MOVE 'DL-UPDATE-INVENTO88'  TO CURRENT-SECTION                       
325700                                                                          
325800     IF CHECK-OUT-NOT-REPORTED-LINES                                      
325900*                                                                         
326000*      KONTROLL OM FYSISK AVVIKELSE VID PACKNING ELLER AVVIKELSE          
326100*      VID UTSKRIFT HAR REDAN GJORTS I SEKTION D-BEHANDLA-INTERVAL        
326200*              ORAD-KVBEART  > ORAD-KVAVBART + ORAD-KVANNANT)             
326300*                                                                         
326400       IF SPAR-ORAD-FLFYSAVV = JA                                         
326500                                                                          
326600         MOVE 'NOLLJAG '             TO ALT5108-IDPW-IN                   
326700         MOVE 1                      TO ALT5108-KDMFSFOR                  
326800                                                                          
326900         MOVE SPAR-ORAD-IDARTNR      TO W-IDARTNR                         
327000         MOVE SPAR-ORAD-IDARTNR      TO SPAR-IDARTNR                      
327100         MOVE SPAR-IDARTNR           TO ALT5108-IDARTNR-IN                
327200         MOVE WS-IDDC                TO W-711-IDDC                        
327300                                                                          
327400         PERFORM IMS-GU-WDK711                                            
327500         IF SEGMENT-FINNS                                                 
327600           IF SLAG-KVUTRS = ZERO                                          
327700             MOVE WS-IDPRODNR        TO ALT5108-IDPRODNR                  
327800             MOVE SPAR-ORAD-KDORDKL  TO ALT5108-KDORDKL                   
327900             MOVE WS-IDDC            TO ALT5108-IDDC-IN                   
328000             PERFORM IMS-PURGE-ALT5108-MSG                                
328290           END-IF                                                         
328291         END-IF                                                           
328300       END-IF                                                             
328400     END-IF                                                               
328500     .                                                                    
328600     EJECT                                                                
328700 E-UPDATE-KUNDORDER SECTION.                                              
328800     MOVE 'E-UPDATE-KUNDORD'     TO CURRENT-SECTION                       
328900                                                                          
329000     MOVE WS-IDDISTR             TO W-401-IDDISTR                         
329100     MOVE WS-IDKUNDNR            TO W-401-IDKUNDNR                        
329200     MOVE WS-IDORDNR             TO W-401-IDORDNR                         
329300     MOVE WS-IDPRODNR            TO W-401-IDPRODNR                        
329400     MOVE WS-IDPLKLST            TO W-401-IDPLKLST                        
329500     PERFORM IMS-GHU-WDE401                                               
329600                                                                          
329700     MOVE ZERO                   TO KORD-KDPAKOLL                         
329800                                                                          
329900     COMPUTE KORD-KVORDRAD-PACK = KORD-KVORDRAD    +                      
330000                                  KORD-KVORDRAD-LEVPL                     
330100     END-COMPUTE                                                          
330200                                                                          
330300     PERFORM IMS-REPL-WDE401                                              
330400     .                                                                    
330500     EJECT                                                                
330600 G-UPDATE-KOLLIREG         SECTION.                                       
330700     MOVE 'G-UPDATE-KOLLIREG'    TO CURRENT-SECTION                       
330800                                                                          
330900     MOVE WS-IDPRODNR                TO  W-601-IDPRODNR                   
331000     PERFORM IMS-GHU-WDE601                                               
331100                                                                          
331200     IF SEGMENT-FINNS                                                     
331300         MOVE VORD-KVKOLPAC          TO  SPAR-KVKOLPAC                    
331400         MOVE VORD-KVKOLLI-FAKT      TO  SPAR-KVKOLLI-FAKT                
331500         COMPUTE VORD-KVORDRAD-PACK =                                     
331600                 VORD-KVORDRAD-PACK + INX-ANT-RADER-MED-FYS-AVV           
331700         PERFORM S11-UPPD-VORD-FRAN-SPAR                                  
331800                                                                          
331900         IF ORAPPORTERADE-RADER-SAKNAS                                    
332000             MOVE VORD-IDDC          TO WS-IDDC                           
332100             MOVE VORD-KDFAKTYP      TO WS-KDFAKTYP                       
332200             IF VORD-KVORDRAD-PACK   =   VORD-KVORDRAD                    
332300                 IF  (VORD-KVKOLLI = VORD-KVKOLPAC)                       
332500                 OR  WS-IDTRANS = '4319'                                  
332600                                                                          
332700                   PERFORM GA-KTRL-VORD-FARDIGPACKAD                      
332800                   IF  WS-VORD-FARDIGPACKAD = JA                          
332900*****************************************************************         
333000*** KOMMENTAR AV SVANTE B. 920219                             ***         
333100*****************************************************************         
333200*** EFTER DENNA KOMMENTAR GÖRS FYRA UPPDATERINGAR:            ***         
333300***                                                           ***         
333400*** A) VORD-TIPACKN-SK SAETTS                                 ***         
333500*** B) "S13-GENERERA-KLAR-SV4" SKAPAR EN "RY6"-TRANS.         ***         
333600*** C) VORD-KDORDSTA SÄTTS                                    ***         
333700*** D) "S04-PACK-ORDER-LISTA" STARTAR 4342                    ***         
333800***                                                           ***         
333900*** OM MAN TAR BORT EN ORDERDEL I ORDER-ENTRY FÖR EN ORDER    ***         
334000*** DÄR RESTERANDE ORDERDELAR REDAN ÄR PACKADE FÅR MAN ETT    ***         
334100*** LÄGE DÄR ORDERN GÅR FRÅN OPACKAD TILL PACKAD. MAN MÅSTE   ***         
334200*** DÅ I W413AVSO UTFÖRA PUNKT A-D.                           ***         
334300***                                                           ***         
334400*** OM MAN I 4397 ELLER 4398 ÄNDRAR DESSA UPPDATERINGAR ELLER ***         
334500*** LÄGGER TILL NYA MÅSTE MAN DÄRFÖR ÄVEN GÖRA DETTA I        ***         
334600*** W413AVSO.                                                 ***         
334700*****************************************************************         
334800                     IF VORD-KVKOLPAC = 0                                 
334900                         MOVE DAT-TIAAMMDD  TO VORD-TIPACKN-SK            
335000                     END-IF                                               
335100                                                                          
335200                     IF  VORD-KDORDSTA < +3                               
335300                     AND VORD-KVKOLLI  >  +0                              
335400                     AND (DIST03-SVERIGE OR DIS103-EMB-INFO)              
335500                         PERFORM S13-GENERERA-KLAR-SV4                    
335600                     END-IF                                               
335700                                                                          
335800                     MOVE +3         TO  VORD-KDORDSTA                    
335900                     IF CHECK-OUT-NOT-REPORTED-LINES                      
336000                       MOVE +3       TO  VORD-KDMETOD                     
336900                     END-IF                                               
337000                                                                          
337100                     MOVE JA         TO  FL-LASNINGSTRANS                 
337200                                                                          
337300                     IF VORD-FLAUTFAK =  JA     AND                       
337400                        VORD-KVKOLLI  >  ZERO                             
337500                         MOVE JA     TO  WS-FLAUTFAK                      
337600                     END-IF                                               
337700                                                                          
337800                     IF  VORD-KVKOLLI =  VORD-KVKOLLI-FL                  
337900                         MOVE +4     TO  VORD-KDORDSTA                    
338000                     END-IF                                               
338100                                                                          
338200                     IF  VORD-KVKOLLI =  VORD-KVKOLLI-FAKT                
338300                     AND VORD-KVKOLLI =  VORD-KVKOLLI-LAST                
338400                         MOVE +5     TO  VORD-KDORDSTA                    
338500                     END-IF                                               
338600                                                                          
338700                     IF WS-IDDC NOT = W-IDDC-B6                           
338800                        MOVE WS-IDDC TO W-IDDC-B6                         
338900                        PERFORM IMS-GU-WDB601                             
339000                     END-IF                                               
339100                     IF  DCS-KDPORDL = JA                                 
339200                         MOVE JA     TO  FL-PACKADEORDERL                 
339300                     END-IF                                               
339400                     IF DCS-NDC-NA                                        
339500                       PERFORM GB-STARTA-DEL-NOTE                         
339600                     END-IF                                               
339700                   END-IF                                                 
339800                 END-IF                                                   
339900             END-IF                                                       
340000         END-IF                                                           
340100                                                                          
340200         PERFORM IMS-REPL-WDE601                                          
340300     ELSE                                                                 
340400         MOVE FEL                       TO  WS-BEHANDLING-TEST            
340500     END-IF                                                               
340600     .                                                                    
340700 GA-KTRL-VORD-FARDIGPACKAD SECTION.                                       
340800     MOVE 'GA-KTRL-VORD-FARD'    TO CURRENT-SECTION                       
340900                                                                          
341000     MOVE JA                 TO WS-VORD-FARDIGPACKAD                      
341100                                                                          
341200     MOVE VORD-IDPRODNR      TO W-601-IDPRODNR                            
341300     PERFORM IMS-GU-WDE62-VORD                                            
341400                                                                          
341500     MOVE WDE62-VORD-IDDC        TO W-Q301-MIN-IDDC                       
341600                                    W-Q301-MAX-IDDC                       
341700     MOVE WDE62-VORD-IDDISTR     TO W-401-IDDISTR                         
341800                                    W-401-IDDISTR-MAX                     
341900     MOVE WDE62-VORD-IDKUNDNR    TO W-401-IDKUNDNR                        
342000                                    W-401-IDKUNDNR-MAX                    
342100     MOVE WS-IDKUNDRF            TO W-401-IDKUNDRF                        
342200                                    W-401-IDKUNDRF-MAX                    
342300     MOVE WDE62-VORD-IDPRODNR    TO W-WDE401-IDPRODNR                     
342400                                    W-401-IDPRODNR                        
342500                                    W-401-IDPRODNR-MAX                    
342600                                    W-Q301-MIN-IDPRODNR                   
342700                                    W-Q301-MAX-IDPRODNR                   
342800     MOVE +001                   TO W-401-IDPLKLST                        
342900     MOVE +999                   TO W-401-IDPLKLST-MAX                    
343000*                                                                         
343100     PERFORM IMS-GU-WDE401-ESEQ                                           
343200                                                                          
343300     IF  SEGMENT-FINNS                                                    
343400*      * IDORDER SPARAS FRÅN 1:A KORD FÖR NYCKEL TILL WDQ3                
343500                                                                          
343600       MOVE E4E-KORD-IDORDER TO W-Q301-MIN-IDORDER                        
343700                                  W-Q301-MAX-IDORDER                      
343800                                                                          
343900       PERFORM UNTIL SEGMENT-SAKNAS                                       
344000                  OR SEGMENT-SLUT                                         
344100                  OR WS-VORD-FARDIGPACKAD = NEJ                           
344200                                                                          
344300         IF SEGMENT-FINNS                                                 
344400         IF E4E-KORD-KVORDRAD-PACK IS <                                   
344500            (E4E-KORD-KVORDRAD + E4E-KORD-KVORDRAD-LEVPL)                 
344600                                                                          
344700           MOVE NEJ            TO WS-VORD-FARDIGPACKAD                    
344800         ELSE                                                             
345000           PERFORM IMS-GN-WDE401-ESEQ                                     
345100         END-IF                                                           
345200         END-IF                                                           
345300       END-PERFORM                                                        
345400                                                                          
345500       IF  WS-VORD-FARDIGPACKAD = JA                                      
345600                                                                          
345700         PERFORM IMS-GU-ORQA2-ODEL                                        
345800                                                                          
345900         PERFORM UNTIL ((NOT SEGMENT-FINNS)                               
346000                 OR   WS-VORD-FARDIGPACKAD = NEJ)                         
346100                                                                          
346200           IF  ORQA2-ODEL-KDODELSTA = 'R'                                 
346300             MOVE NEJ        TO WS-VORD-FARDIGPACKAD                      
346400           ELSE                                                           
346500             PERFORM IMS-GN-ORQA2-ODEL                                    
346600           END-IF                                                         
346700         END-PERFORM                                                      
346800                                                                          
346900       END-IF                                                             
347000     ELSE                                                                 
347100       MOVE NEJ            TO WS-VORD-FARDIGPACKAD                        
347200     END-IF                                                               
347300                                                                          
347400     .                                                                    
347500     EJECT                                                                
347600 GB-STARTA-DEL-NOTE SECTION.                                              
347700     MOVE 'GB-STARTA-DEL-NOTE'   TO CURRENT-SECTION                       
347800                                                                          
347900     IF DIST07-USA-PRINT-DNOTE                                            
348000     OR DIST07-CAN-PRINT-DNOTE                                            
348100*       TRANS TILL 4349 FÖR ATT STARTA UTSKRIFT                           
348200        MOVE WS-IDDISTR          TO 4349-MID-IDDISTR                      
348300        MOVE WS-IDKUNDNR         TO WS-SAVE-IDKUNDNR                      
348400        MOVE WS-SAVE-IDKUNDNR    TO 4349-MID-IDKUNDNR                     
348500        MOVE WS-IDORDNR          TO WS-IDORDNR7-NEW                       
348600        MOVE WS-IDORDNR7-NEW     TO 4349-MID-IDORDNR7                     
348700        MOVE WS-IDPRODNR         TO 4349-MID-IDPRODNR                     
348800        MOVE WS-IDDC             TO 4349-MID-IDDC                         
348900        MOVE WS-IDORDER          TO 4349-MID-IDORDER                      
349000        MOVE WS-KDMFSFOR         TO 4349-SPRAK                            
349100        MOVE 4349-MID-W4I34901   TO 4349-FILLER                           
349200                                                                          
349300        PERFORM IMS-INSERT-TRANS4349                                      
349400     END-IF                                                               
349500                                                                          
349600     IF DIST07-USA-RETAILER-DNOTE                                         
349700     OR DIST07-CAN-RETAILER                                               
349800        INITIALIZE DNOT-ORDER-INFO                                        
349900* *     UPPDATERA UTSKRIFTSDATUM PÅ WDQ5                                  
350000        MOVE IDPGM                    TO DNOT-IDPGM                       
350100        MOVE WS-IDORDER               TO DNOT-IDORDER                     
350200        MOVE WS-IDDC                  TO DNOT-IDDC                        
350300        CALL W411DNOT USING DNOT-W411DNOT                                 
350400                            DNOT-ORQP-PCB                                 
350500                            DNOT-ORQP2-PCB                                
350600                            DNOT-ORQP3-PCB                                
350700                            DNOT-4013-PCB                                 
350800                            DNOT-BENA-PCB                                 
350900     END-IF                                                               
351000     .                                                                    
351100     EJECT                                                                
351200 H-UPDATE-ORDERKO SECTION.                                                
351300     MOVE 'H-UPDATE-ORDERKO  '   TO CURRENT-SECTION                       
351400                                                                          
351500     IF NOT DIST19-SATS                                                   
351600       PERFORM HA-UPDATE-ORQA                                             
351700       PERFORM HB-UPDATE-ORQI                                             
351800       PERFORM HC-UPDATE-PRODTAB                                          
351900     END-IF                                                               
352000     .                                                                    
352100     EJECT                                                                
352200 HA-UPDATE-ORQA                          SECTION.                         
352300     MOVE 'HA-UPDATE-ORQA    '   TO CURRENT-SECTION                       
352400                                                                          
352500     MOVE KORD-IDORDER                   TO W-WDQ301-IDORDER              
352600                                            WS-IDORDER                    
352700     MOVE KORD-IDDC                      TO W-WDQ301-IDDC                 
352800     MOVE KORD-IDPRODNR                  TO W-WDQ301-IDPRODNR             
352900     MOVE KORD-IDPLKLST                  TO W-WDQ301-IDPLKLST             
353000     MOVE WS-IDDISTR                     TO DIST20-IDDISTR                
353100     IF DIST20-EMBALLAGE-SVS                                              
353200       PERFORM IMS-GHU-ORQA01                                             
353300     ELSE                                                                 
353400       PERFORM IMS-GHN-ORQA01                                             
353500     END-IF                                                               
353600                                                                          
353700     MOVE KORD-KVORDRAD-PACK             TO ODEL-KVPACKRAD-OD             
353800     MOVE 'P'                            TO ODEL-KDODELSTA                
353900     MOVE DAT-TIAAMMDD                   TO ODEL-TIPACKN                  
354000     MOVE WS-TTMMSS                      TO ODEL-TIPACTID                 
354100                                                                          
354200     MOVE ODEL-IDDC                      TO W-4447-IDDC                   
354300                                            W-4487-IDDC                   
354400     MOVE ODEL-IDPRC                     TO W-4448-IDPRC                  
354500     MOVE ODEL-DARFS                     TO W-4490-DARFS                  
354600                                                                          
354700     MOVE ODEL-IDPRODNR                  TO W-4490-IDPRODNR               
354800     MOVE ODEL-IDPLKLST                  TO W-4490-IDPLKLST               
354900                                                                          
355000     PERFORM IMS-REPL-ORQA01                                              
355100                                                                          
355200     PERFORM S06-BORTTAG-PRODTAB-LOCKTAB                                  
355300     .                                                                    
355400     EJECT                                                                
355500 HB-UPDATE-ORQI                          SECTION.                         
355600     MOVE 'HB-UPDATE-ORQI    '   TO CURRENT-SECTION                       
355700                                                                          
355800     MOVE NEJ                     TO KDORDSTA-SW                          
355900     MOVE KORD-IDORDER            TO W-Q301KY-MIN-IDORDER                 
356000                                     W-Q301KY-MAX-IDORDER                 
356100     MOVE WS-IDDC                 TO W-Q301KY-MIN-IDDC                    
356200                                     W-Q301KY-MAX-IDDC                    
356300                                                                          
356400     MOVE 'R'                     TO W-KDODELST                           
356500     PERFORM IMS-GU-ORQA-STATUS                                           
356600     IF SEGMENT-FINNS                                                     
356700        MOVE 'R*'                 TO WS-KDORDSTA                          
356800        MOVE JA                   TO KDORDSTA-SW                          
356900     ELSE                                                                 
357000                                                                          
357100        MOVE 'U'                  TO W-KDODELST                           
357200        PERFORM IMS-GU-ORQA-STATUS                                        
357300        IF SEGMENT-FINNS                                                  
357400           MOVE 'U*'              TO WS-KDORDSTA                          
357500           MOVE JA                TO KDORDSTA-SW                          
357600        ELSE                                                              
357700           PERFORM HBA-KOLLA-KVKOLLI                                      
357800           IF KDORDSTA-KLAR                                               
357900               CONTINUE                                                   
358000           ELSE                                                           
358100               PERFORM HBB-TA-FRAM-KDORDSTA                               
358200           END-IF                                                         
358300        END-IF                                                            
358400     END-IF                                                               
358500                                                                          
358700** GHNP-WDQ212 IS DONE IN D-SECTION !!                                    
359400     MOVE WS-KDORDSTA   TO ARB-KDORDSTA                                   
360500     PERFORM IMS-REPL-WDQ212                                              
360700     .                                                                    
360800     EJECT                                                                
360900 HBA-KOLLA-KVKOLLI SECTION.                                               
361000     MOVE 'HBA-KOLLA-KVKOLLI '   TO CURRENT-SECTION                       
361100                                                                          
361200     MOVE ZERO        TO SPAR-IDPRODNR                                    
361300     MOVE WS-IDPRODNR TO W-601-IDPRODNR                                   
361400     MOVE WS-IDPRODNR TO W-IDPRODNR-WDE4E-MAX                             
361500     MOVE WS-IDPRODNR TO W-IDPRODNR-WDE4E-MIN                             
361600     PERFORM IMS-GU-WDE4E1                                                
361700                                                                          
361800     MOVE SEQE-IDGMTREF      TO W-E4ASEQ-IDGMTREF                         
361900     PERFORM IMS-GU-WDE401-ASEQ                                           
362000                                                                          
362100     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
362200                   KDORDSTA-KLAR                                          
362300                                                                          
362400        IF KORD-IDPRODNR              = SPAR-IDPRODNR OR                  
362500           KORD-IDDC                  NOT = W-SPAR-IDDC                   
362600            CONTINUE                                                      
362700        ELSE                                                              
362800            MOVE KORD-IDPRODNR        TO W-601-IDPRODNR                   
362900            PERFORM IMS-GU-WDE601                                         
363000                                                                          
363100            IF (VORD-KVKOLLI-LAST     >  0    OR                          
363200                VORD-KVKOLLI-FAKT     >  0    OR                          
363300                VORD-KVKOLLI-FL       >  0)   OR                          
363400               VORD-KDORDSTA          =  5                                
363500                                                                          
363600               COMPUTE W-KVKOLLI      =  W-KVKOLLI + VORD-KVKOLLI         
363700               COMPUTE W-KVKOLLI-FAKT =                                   
363800                              W-KVKOLLI-FAKT + VORD-KVKOLLI-FAKT          
363900               COMPUTE W-KVKOLLI-LAST =                                   
364000                              W-KVKOLLI-LAST + VORD-KVKOLLI-LAST          
364100               COMPUTE W-KVKOLLI-FL   =                                   
364200                              W-KVKOLLI-FL   + VORD-KVKOLLI-FL            
364300            ELSE                                                          
364400               MOVE 'P'               TO WS-KDORDSTA                      
364500               MOVE JA                TO KDORDSTA-SW                      
364600            END-IF                                                        
364700            MOVE KORD-IDPRODNR        TO SPAR-IDPRODNR                    
364800        END-IF                                                            
364900        PERFORM IMS-GN-WDE401-ASEQ                                        
365000     END-PERFORM                                                          
365100     .                                                                    
365200     EJECT                                                                
365300 HBB-TA-FRAM-KDORDSTA        SECTION.                                     
365400     MOVE 'HBB-TA-FRAM-KDORDSTA' TO CURRENT-SECTION                       
365500                                                                          
365600     IF VORD-KDORDSTA          <  4                                       
365700        MOVE 'P*'              TO WS-KDORDSTA                             
365800     ELSE                                                                 
365900       IF W-KVKOLLI-FL            = W-KVKOLLI                             
366000         IF W-KVKOLLI-FAKT = ZERO AND                                     
366100            W-KVKOLLI-LAST = ZERO                                         
366200           MOVE 'S'          TO WS-KDORDSTA                               
366300         ELSE                                                             
366400           IF W-KVKOLLI-FAKT     NOT = W-KVKOLLI  AND                     
366500              W-KVKOLLI-LAST     NOT = W-KVKOLLI                          
366600             MOVE 'S*'       TO WS-KDORDSTA                               
366700           ELSE                                                           
366800             MOVE 'SF'       TO WS-KDORDSTA                               
366900           END-IF                                                         
367000         END-IF                                                           
367100       END-IF                                                             
367200     END-IF                                                               
367300     .                                                                    
367400     EJECT                                                                
367500 HC-UPDATE-PRODTAB                       SECTION.                         
367600     MOVE 'HC-UPDATE-PRODTAB   ' TO CURRENT-SECTION                       
367700                                                                          
367800     IF INX-ANT-RADER-MED-FYS-AVV > 0                                     
367900        PERFORM HCA-LAES-SHIFTTAB                                         
368000        IF ODEL-KDPRODKL = 'B' OR ODEL-KDPRODKL = 'C'                     
368100*        * PRODTAB UPDATES ENDAST FÖR PRODKL B OCH C.                     
368200                                                                          
368300           MOVE KORD-IDDC          TO W-4471-IDDC                         
368400           MOVE ODEL-IDPRCBAS      TO W-4471-IDPRCBAS                     
368500           MOVE ODEL-IDPRCVAR      TO W-4471-IDPRCVAR                     
368600           PERFORM IMS-GHU-XXKW11                                         
368700                                                                          
368800           IF SEGMENT-FINNS                                               
368900*          * PRODTAB UPDATES ENDAST OM ORDERDELENS PRC FINNS.             
369000                                                                          
369100              MOVE 1                 TO IND1                              
369200              MOVE W-4478-IDSHIFT    TO IND2                              
369300              MOVE ODEL-DARFS (3:10) TO HJALP-ODEL-TIRFS                  
369400              MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                  
369500                                                                          
369600              PERFORM UNTIL IND1 = 30 OR                                  
369700                         4472-TIRFS (IND1) = ZERO OR                      
369800                         HJALP-ODEL-TIRFS-7 = HJALP-4472-TIRFS-7          
369900                ADD 1                  TO IND1                            
370000                MOVE 4472-TIRFS (IND1) TO HJALP-4472-TIRFS                
370100              END-PERFORM                                                 
370200                                                                          
370300              MOVE ODEL-DARFS (3:10) TO 4472-TIRFS (IND1)                 
370400              MOVE W-4478-IDSHIFT  TO 4472-IDSHIFT (IND1, IND2)           
370500              ADD INX-ANT-RADER-MED-FYS-AVV                               
370600                                TO 4472-KVRADER-PRAPP (IND1, IND2)        
370700              PERFORM HCB-ADDERA-TOTAL-PRODTID                            
370800              PERFORM IMS-REPL-XXKW11                                     
370900           END-IF                                                         
371000        END-IF                                                            
371100     END-IF                                                               
371200     .                                                                    
371300 HCA-LAES-SHIFTTAB         SECTION.                                       
371400     MOVE 'HCA-LAES-SHIFTTAB   ' TO CURRENT-SECTION                       
371500                                                                          
371600     MOVE KORD-IDDC         TO W-4477-IDDC                                
371700     MOVE '1'               TO W-4478-IDSHIFT                             
371800     MOVE KORD-IDUSER       TO W-4478-IDUSER                              
371900     PERFORM IMS-GU-XXLB                                                  
372000                                                                          
372100     IF SEGMENT-SAKNAS                                                    
372200        MOVE '2'            TO W-4478-IDSHIFT                             
372300        PERFORM IMS-GU-XXLB                                               
372400                                                                          
372500        IF SEGMENT-SAKNAS                                                 
372600           MOVE '3'         TO W-4478-IDSHIFT                             
372700           PERFORM IMS-GU-XXLB                                            
372800                                                                          
372900           IF SEGMENT-SAKNAS                                              
373000              MOVE '1'      TO W-4478-IDSHIFT                             
373100           END-IF                                                         
373200        END-IF                                                            
373300     END-IF                                                               
373400     .                                                                    
373500     EJECT                                                                
373600 HCB-ADDERA-TOTAL-PRODTID             SECTION.                            
373700     MOVE 'HCB-ADDERA-TOTAL    ' TO CURRENT-SECTION                       
373800                                                                          
373900     MOVE 4472-SUPTID-PRAPP (IND1, IND2) TO WS-SUPTID-PRAPP               
374000                                                                          
374100     COMPUTE WS-KVPTID-MIN ROUNDED = INX-ANT-RADER-MED-FYS-AVV *          
374200                                     ODEL-KVPTID                          
374300     END-COMPUTE                                                          
374400                                                                          
374500     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
374600     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
374700     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
374800                            (WS-KVPTID-TIM * 60)                          
374900                                                                          
375000     ADD  WS-SUPTID-MIN                  TO WS-KVPTID-MIN                 
375100     DIVIDE WS-KVPTID-MIN BY 60 GIVING WS-KVPTID-TIM                      
375200     ADD  WS-KVPTID-TIM                  TO WS-SUPTID-TIM                 
375300     COMPUTE WS-KVPTID-MIN = WS-KVPTID-MIN -                              
375400                            (WS-KVPTID-TIM * 60)                          
375500     MOVE WS-KVPTID-MIN                  TO WS-SUPTID-MIN                 
375600                                                                          
375700     MOVE WS-SUPTID-PRAPP TO 4472-SUPTID-PRAPP (IND1, IND2)               
375800     .                                                                    
375900     EJECT                                                                
376000 I-AVSLUT             SECTION.                                            
376100     MOVE 'I-AVSLUT      '       TO CURRENT-SECTION                       
376200                                                                          
376300     IF WS-BEHANDLING-RATT                                                
376400                                                                          
376500         IF SKRIV-PACKADEORDERL                                           
376600             PERFORM S04-PACK-ORDER-LISTA                                 
376700         END-IF                                                           
376800                                                                          
376900         IF LASNINGSTRANS-TAS-BORT                                        
377000             PERFORM S03-BORTTAG-LASPOST                                  
377100         END-IF                                                           
377200                                                                          
377300         IF AUT-FAKTURA-SKRIVS-UT                                         
377400         AND SPAR-KVKOLPAC > SPAR-KVKOLLI-FAKT                            
377500           IF WS-IDTRANS = '4319'                                         
377600           AND ORAPPORTERADE-RADER-FINNS                                  
377700           OR (WS-IDTRANS NOT = '4319')                                   
377800                                                                          
377900              PERFORM S05-AUTOMATFAKTURERING                              
378000                                                                          
378100              IF SEGMENT-SAKNAS                                           
378200                 MOVE SPACE          TO  IO-AREA3                         
378300                 MOVE WS-IDDISTR     TO  AUTFAKT-IDDISTR                  
378400                 MOVE WS-IDKUNDNR    TO  AUTFAKT-IDKUNDNR                 
378500                 MOVE WS-IDDC        TO  AUTFAKT-IDDC                     
378600                 MOVE WS-KDFAKTYP    TO  AUTFAKT-KDFAKTYP                 
378700                 PERFORM IMS-ISRT-AUTFAKTURA-ROT                          
378800                 MOVE SPACE          TO  IO-AREA3                         
378900                 PERFORM S05-AUTOMATFAKTURERING                           
379000              END-IF                                                      
379100           END-IF                                                         
379200         END-IF                                                           
379300     END-IF                                                               
379400     .                                                                    
379500     EJECT                                                                
379600 S01-UPPD-SPAR-UPPGIFTER SECTION.                                         
379700     MOVE 'S01-UPPD-SPAR '       TO CURRENT-SECTION                       
379800     SKIP3                                                                
379900     MOVE KORD-IDDISTR   TO TEST-IDDISTR                                  
380000                                                                          
380100     COMPUTE SPAR-VKORDNTO ROUNDED = SPAR-VKORDNTO +                      
380200                         ORAD-VKARTNTO * WS-KVORAPP-PACK                  
380300*                                                                         
380400     COMPUTE SPAR-VLORDNTO = SPAR-VLORDNTO +                              
380500                         ORAD-VLARTNTO * WS-KVORAPP-PACK                  
380600*                                                                         
380700     IF DIST79-DEALER-PRICE                                               
380800       IF ORAD-PRARTNTO-LOCPREL  > 0                                      
380900         COMPUTE SPAR-SUORDV-LOCPREL ROUNDED =                            
381000             SPAR-SUORDV-LOCPREL + ORAD-PRARTNTO-LOCPREL                  
381100                                      * WS-KVORAPP-PACK                   
381200       ELSE                                                               
381300         COMPUTE SPAR-SUORDV-LOC ROUNDED = SPAR-SUORDV-LOC                
381400                      + ORAD-PRARTNTO-LOC * WS-KVORAPP-PACK               
381500       END-IF                                                             
381600     ELSE                                                                 
381800       IF DIST79-ECOM-PRICE                                               
381900         COMPUTE SPAR-SUORDV-LOC ROUNDED = SPAR-SUORDV-LOC                
382000                      + ORAD-PRARTNTO-LOC * WS-KVORAPP-PACK               
382100       ELSE                                                               
382200         COMPUTE SPAR-SUORDV ROUNDED = SPAR-SUORDV                        
382300                  + ORAD-PRARTNTO * WS-KVORAPP-PACK                       
382400       END-IF                                                             
382500     END-IF                                                               
382600*                                                                         
382700                                                                          
382800                                                                          
382900     EJECT                                                                
383000     .                                                                    
383100 S02-UPDATE-INTERVAL    SECTION.                                          
383200                                                                          
383300     IF ORAD-KDRADSTA <= 3                                                
383400     OR (ORAD-KDRADSTA >= 4                                               
383500     AND ORAD-KVBEART  > ORAD-KVAVBART + ORAD-KVANNANT)                   
383600                                                                          
383700*      * FYSISK AVVIKELSE VID PACKNING                                    
383800*      * ELLER AVVIKELSE VID UTSKRIFT                                     
383900        ADD +1                     TO  INX-TOT-ANT-RADER                  
384000                                                                          
384100        IF ORAD-KDRADSTA <= 3                                             
384200*         * FYSISK AVVIKELSE VID PACKNING                                 
384300          PERFORM DA-UPDATE-FYS-AVVIKELSE                                 
384400          MOVE JA                  TO ORAD-FLFYSAVV                       
384500          ADD +1                   TO INX-ANT-RADER-MED-FYS-AVV           
384600        END-IF                                                            
384700                                                                          
384800        MOVE ORAD-WDE411           TO SPAR-ORAD-WDE411                    
384900        IF SPAR-ORAD-PRARTBTO-LOC NOT NUMERIC                             
385000           MOVE ZERO               TO SPAR-ORAD-PRARTBTO-LOC              
385100        END-IF                                                            
385200        IF SPAR-ORAD-IDPRQUES NOT NUMERIC                                 
385300           MOVE ZERO               TO SPAR-ORAD-IDPRQUES                  
385400        END-IF                                                            
385500        MOVE ZERO                  TO WS-KVSLATTAT                        
385600                                SPAR-KART-KVRESS-ART                      
385700        PERFORM DB-BEHANDLA-RAD-INOM-INTERVALL                            
385800        PERFORM IMS-REPL-WDE411                                           
385900        PERFORM DH-LAS-ARTREG                                             
386000        PERFORM DJ-BESTAM-ORDERBEKR-KOD                                   
386100        PERFORM DK-UPDATE-EV-KAMP-REG                                     
386200        PERFORM DG-UPDATE-ROREG                                           
386300        PERFORM DE-UPDATE-ARTREG                                          
386400        PERFORM S12-GENERERA-AVVIKELSE-TRANS                              
386500        PERFORM DL-UPDATE-INVENTORY                                       
386600     END-IF                                                               
386700     .                                                                    
386800 S03-BORTTAG-LASPOST    SECTION.                                          
386900     MOVE 'S03-BORTTAG-LAS'       TO CURRENT-SECTION                      
387000                                                                          
387100     MOVE WS-IDPRODNR       TO  W-XXDJ-IDPRODNR                           
387200                                W-XXDK-IDPRODNR                           
387300                                W-XXDL-IDPRODNR-MIN                       
387400                                W-XXDL-IDPRODNR-MAX                       
387500                                4306-IDPRODNR                             
387600                                                                          
387700     MOVE WS-IDDC           TO  W-XXDJ-IDDC                               
387800                                W-XXDK-IDDC                               
387900     MOVE LOW-VALUE         TO  4306-LOWVALUE                             
388000*    BORTTAG AV 4306-SEGMENT (LÅSNING ORDERVIS/KOLLIVIS)                  
388100     PERFORM IMS-GU-XXDJ-ROT                                              
388200     IF SEGMENT-FINNS                                                     
388300       PERFORM IMS-GHNP-XXDJ-LASNING                                      
388400       IF SEGMENT-FINNS                                                   
388500         PERFORM IMS-DLET-XXDJ-LASNING                                    
388600       END-IF                                                             
388700     END-IF                                                               
388800                                                                          
388900*    BORTTAG AV 4316-SEGMENT (0605-TRANSAR)                               
389000     PERFORM IMS-GU-XXDL-ROT                                              
389100     MOVE '001' TO W-XXDL-IDPTYP-MIN                                      
389200     MOVE '004' TO W-XXDL-IDPTYP-MAX                                      
389300     PERFORM IMS-GET-XXDL-4316-INTERV                                     
389400                                                                          
389500     PERFORM UNTIL SEGMENT-SAKNAS                                         
389600         PERFORM IMS-DLET-XXDL                                            
389700         PERFORM IMS-GET-XXDL-4316-INTERV                                 
389800     END-PERFORM                                                          
389900                                                                          
390000                                                                          
390100*    BORTTAG AV 4312-SEGMENT (ORDER UNDER ARBETE)                         
390200     PERFORM IMS-GET-XXDK-ROT                                             
390300     IF SEGMENT-FINNS                                                     
390400       PERFORM IMS-GET-XXDK-4312                                          
390500                                                                          
390600       IF SEGMENT-FINNS                                                   
390700         PERFORM IMS-DLET-XXDK                                            
390800       END-IF                                                             
390900     END-IF                                                               
391000     EJECT                                                                
391100     .                                                                    
391200 S04-PACK-ORDER-LISTA   SECTION.                                          
391300     MOVE 'S04-PACK-ORDER '       TO CURRENT-SECTION                      
391400                                                                          
391500     MOVE LOW-VALUE                TO  4342-MID                           
391600     MOVE '+++++++'                TO  4342-MID-IDPRODNR-IN               
391700     MOVE WS-IDPRODNR              TO  WS-IDPRODNR-RED                    
391800     MOVE WS-IDPRODNR-RED          TO  4342-MID-IDPRODNR-UT               
391900     MOVE 'W4T342U '               TO  MSG-KDTRANS-1                      
392000     MOVE '439G'                   TO  MSG-IDTRANS-1                      
392100     MOVE WS-KDMFSFOR              TO  MSG-KDMFSFOR-1                     
392200     MOVE +31                      TO  MSG-KVLL                           
392300     MOVE 4342-MID                 TO  MSG-INDATA-MINUS-1-TRANSKOD        
392400     PERFORM IMS-INSERT-ALT42                                             
392500     EJECT                                                                
392600     .                                                                    
392700 S05-AUTOMATFAKTURERING      SECTION.                                     
392800     MOVE 'S05-AUTOMATFAK '       TO CURRENT-SECTION                      
392900     SKIP3                                                                
393000     MOVE WS-IDDISTR                      TO  W-RDG-IDDISTR               
393100     MOVE WS-IDKUNDNR                     TO  W-RDG-IDKUNDNR              
393200     MOVE WS-IDDC                         TO  W-RDG-IDDC                  
393300     MOVE WS-KDFAKTYP                     TO  W-RDG-KDFAKTYP              
393400     SKIP2                                                                
393500     MOVE WS-IDPRODNR                     TO  AUTFAKT-IDPRODNR            
393600     MOVE ZERO                            TO  AUTFAKT-PRFRAKT             
393700                                              AUTFAKT-IDSKEPPN            
393800     IF WS-IDDC NOT = W-IDDC-B6                                           
393900        MOVE WS-IDDC TO W-IDDC-B6                                         
394000        PERFORM IMS-GU-WDB601                                             
394100     END-IF                                                               
394200     IF DIST19-SATS                                                       
394300        MOVE NEJ                          TO  W-XXDV-FLBATCH              
394400     ELSE                                                                 
394500                                                                          
394600*       IF DIST03-SVERIGE                                                 
394700*                                                                         
394800*          IF  DCS-CDC                                                    
394900*          OR (DCS-SDC AND DCS-SWEDEN)                                    
395000*          OR DIST18-SKROT                                                
395100*             MOVE JA                     TO  W-XXDV-FLBATCH              
395200*          ELSE                                                           
395300*             MOVE NEJ                    TO  W-XXDV-FLBATCH              
395400*          END-IF                                                         
395500*       ELSE                                                              
395600           MOVE NEJ                       TO  W-XXDV-FLBATCH              
395700*       END-IF                                                            
395800     END-IF                                                               
395900                                                                          
396000     IF DIST03-SVERIGE                                                    
396100         MOVE NEJ                         TO  AUTFAKT-FLLASTA             
396200     ELSE                                                                 
396300         MOVE JA                          TO  AUTFAKT-FLLASTA             
396400     END-IF                                                               
396500     PERFORM IMS-ISRT-AUTFAKTURA                                          
396600     EJECT                                                                
396700     .                                                                    
396800 S06-BORTTAG-PRODTAB-LOCKTAB    SECTION.                                  
396900     MOVE 'S06-BORTTAG    '       TO CURRENT-SECTION                      
397000                                                                          
397100     MOVE ODEL-IDDC                      TO W-4447-IDDC                   
397200                                            W-4487-IDDC                   
397300     MOVE ODEL-IDPRC                     TO W-4448-IDPRC                  
397400     MOVE WS-IDPRODNR                    TO W-4301-IDPRODNR               
397500     MOVE WS-IDPLKLST                    TO W-4302-IDPLKLST               
397600                                                                          
397700     IF WS-IDDC NOT = W-IDDC-B6                                           
397800        MOVE WS-IDDC TO W-IDDC-B6                                         
397900        PERFORM IMS-GU-WDB601                                             
398000     END-IF                                                               
398100                                                                          
398200     PERFORM IMS-GHU-XXDU01                                               
398300                                                                          
398400     IF SEGMENT-FINNS                                                     
398500       PERFORM IMS-GHNP-XXDU11-IDPLKLST                                   
398600                                                                          
398700       IF SEGMENT-SAKNAS                                                  
398800         PERFORM S06A-BORTTAG-PRODTAB                                     
398900       ELSE                                                               
399000*        -- TA BORT IF OM VI HITTAR FELET MED BORTTAGEN 4301              
399100*        -- SOM ORSAKAR SEGMENT-SAKNAS OVAN.                              
399200*        -- MVH KA (05-01-19 KL 18:55)                                    
399300         IF DCS-SDC AND (DCS-SWEDEN OR DCS-ENGLAND)                       
399400           PERFORM S06A-BORTTAG-PRODTAB                                   
399500         END-IF                                                           
399600       END-IF                                                             
399700                                                                          
399800     ELSE                                                                 
399900       PERFORM S06A-BORTTAG-PRODTAB                                       
400000     END-IF                                                               
400100     .                                                                    
400200     SKIP2                                                                
400300 S06A-BORTTAG-PRODTAB SECTION.                                            
400400     MOVE 'S06A-BORTTAG-PRO'       TO CURRENT-SECTION                     
400500                                                                          
400600     PERFORM IMS-GU-XXKH11                                                
400700     MOVE 4448-KDPRCGRP          TO W-4488-KDPRCGRP                       
400800                                                                          
400900     PERFORM IMS-GHU-WDGX4490                                             
401000     IF SEGMENT-FINNS                                                     
401100        PERFORM IMS-DLET-WDGX4490                                         
401200     END-IF                                                               
401300     .                                                                    
401400     SKIP2                                                                
401500 S07-CHK-LYNK-NONAPI       SECTION.                                       
401501     MOVE 'S07-CHK-LYNK-NON'       TO CURRENT-SECTION                     
401503     MOVE OHUV-IDDISTR       TO W-IDDISTR-WDB2                            
401504     MOVE OHUV-IDKUNDNR      TO W-IDKUNDNR-WDB2                           
401505     MOVE NEJ                TO SW-LYNK-NON-API                           
401506                                SW-VOR                                    
401507                                                                          
401508     PERFORM IMS-GU-GMTA-WDB201                                           
401516     IF SEGMENT-FINNS                                                     
401517       IF GMT-KDKUNDKAT = 03                                              
401518         IF OHUV-IDSYSTEM(1:3) NOT = 'LYN'                                
401519           MOVE JA              TO SW-LYNK-NON-API                        
401520         END-IF                                                           
401521         IF OHUV-KDORDKL = 0                                              
401522           MOVE JA              TO SW-VOR                                 
401523         END-IF                                                           
401524       END-IF                                                             
401525     END-IF                                                               
401527     .                                                                    
401528     SKIP2                                                                
401530 S10-HAMTA-MASKINDATUM     SECTION.                                       
401600     MOVE 'S10-HAMTA-MASKIN'       TO CURRENT-SECTION                     
401700                                                                          
401800     MOVE 'IDAG'                 TO   DAT-KDDATFORM                       
401900     CALL WDATKONV USING DAT-KDDATFORM                                    
402000                         DAT-I-TIDATUM                                    
402100                         DAT-O-TIDATUM                                    
402200                         DAT-KDSVAR                                       
402300     EJECT                                                                
402400     .                                                                    
402500 S11-UPPD-VORD-FRAN-SPAR   SECTION.                                       
402600     MOVE 'S11-UPPDA-VORD- '       TO CURRENT-SECTION                     
402700     SKIP3                                                                
402800     SUBTRACT SPAR-VKORDNTO FROM VORD-VKORDNTO                            
402900     COMPUTE VORD-VLORDNTO = VORD-VLORDNTO - SPAR-VLORDNTO                
403000                               / 1000000                                  
403100     SUBTRACT SPAR-SUORDV-LOC       FROM VORD-SUORDV-LOC                  
403200     SUBTRACT SPAR-SUORDV-LOCPREL FROM VORD-SUORDV-LOCPREL                
403300     SUBTRACT SPAR-SUORDV             FROM VORD-SUORDV                    
403400*                                                                         
403500     EJECT                                                                
403600     .                                                                    
403700 S12-GENERERA-AVVIKELSE-TRANS SECTION.                                    
403800     MOVE 'S12-GENERERA-AVV'       TO CURRENT-SECTION                     
403900                                                                          
404000     IF WS-IDDC NOT = W-IDDC-B6                                           
404100        MOVE WS-IDDC TO W-IDDC-B6                                         
404200        PERFORM IMS-GU-WDB601                                             
404300     END-IF                                                               
404400                                                                          
404500     PERFORM S12A-UPDATE-RY1-POST                                         
404600                                                                          
404700     IF NOT DIST19-SATS                                                   
404800       IF  DCS-CDC OR DCS-NDC                                             
404900       OR  SPAR-ORAD-FLFYSAVV = JA                                        
405000       OR (DCS-SDC AND DCS-CHINA)                                         
405100**     OR  DCS-SDC (=LDC SAMT FICK EJ JA PÅ FLAGGAN)                      
405200         PERFORM S12B-UPDATE-ORDBEK-WDQ1                                  
405201                                                                          
405210         IF LYNK-NON-API                                                  
405220           PERFORM S12G-EVENTS-NON-API                                    
405230         ELSE                                                             
405300*NEW                                                                      
405400*'LYND' = DÖSKALLE  WDQ2C                                                 
405500           IF OHUV-IDSYSTEM = 'LYND' OR 'TADD'                            
405600             MOVE KORD-IDDISTR         TO   W-WDQ2CSEQ-IDDISTR            
405700             MOVE KORD-IDKUNDNR        TO   W-WDQ2CSEQ-IDKUNDNR           
405800             MOVE SPAR-ORAD-IDKUNDRF-RO(1:5)                              
405900                                      TO W-WDQ2CSEQ-IDKUNDRF(3:7)         
406000             PERFORM IMS-GU-ORQI01-CSEQ                                   
406100             IF SEGMENT-FINNS                                             
406200               MOVE OHUV-IDORDER       TO  RYK-IDORDER                    
406300                                                                          
406400               MOVE OHUV-IDDISTR       TO WS-IDDISTR-EVENT                
406500               MOVE OHUV-IDKUNDNR      TO WS-IDKUNDNR-EVENT               
406600               MOVE OHUV-IDORDNR7      TO WS-IDORDNR7-EVENT               
406700               MOVE OHUV-TIREGDAT      TO WS-TIREGDAT-EVENT               
406800****           MOVE KORD-TIORDREG      TO WS-TIREGDAT-EVENT               
406900               MOVE JA                 TO CREATE-EVENT-SW                 
407000             END-IF                                                       
407100           ELSE                                                           
407200*'LYNV' = VOR KUNDRF-LEV -A6                                              
407300             IF OHUV-IDSYSTEM = 'LYNV' OR 'TADV'                          
407400               MOVE LOW-VALUE          TO W-WDA6BSEQ-MIN-X                
407500               MOVE HIGH-VALUE         TO W-WDA6BSEQ-MAX-X                
407600                                                                          
407700               MOVE OHUV-IDDISTR       TO W-A6BSEQ-MIN-IDDISTR            
407800                                          W-A6BSEQ-MAX-IDDISTR            
407900               MOVE OHUV-IDKUNDNR      TO W-A6BSEQ-MIN-IDKUNDNR           
408000                                          W-A6BSEQ-MAX-IDKUNDNR           
408100               MOVE OHUV-IDKUNDRF     TO W-A6BSEQ-MIN-IDKUNDRF-LEV        
408200                                         W-A6BSEQ-MAX-IDKUNDRF-LEV        
408300               PERFORM IMS-GU-SEQB-WDA601                                 
408400               IF SEGMENT-FINNS                                           
408500                 MOVE OHUV-IDDISTR     TO WS-IDDISTR-EVENT                
408600                 MOVE OHUV-IDKUNDNR    TO WS-IDKUNDNR-EVENT               
408700                 MOVE VOR-IDORDNR7      TO WS-IDORDNR7-EVENT              
408800                 MOVE VOR-TIREGDAT-URSP TO WS-TIREGDAT-EVENT              
408900                 MOVE JA               TO CREATE-EVENT-SW                 
409000               END-IF                                                     
409100             ELSE                                                         
409200*                                                                         
409300*'LYNB' = VERKSTADS/REPARATIONS-ORDER -A5                                 
409400               IF OHUV-IDSYSTEM = 'LYNB' OR 'TADB'                        
409500                                                                          
409600                 MOVE OHUV-IDDISTR     TO W-IDDISTR-A5-MIN                
409700                                          W-IDDISTR-A5-MAX                
409800                 MOVE OHUV-IDKUNDNR    TO W-IDKUNDNR-A5-MIN               
409900                                          W-IDKUNDNR-A5-MAX               
410000                 MOVE OHUV-KDORDKL     TO W-KDORDKL                       
410100                 MOVE OHUV-IDORDNR7(3:5) TO W-IDKUNDRF-LEV                
410200                                                                          
410300                 PERFORM IMS-GU-WDA501                                    
410400                 IF SEGMENT-FINNS                                         
410500                   MOVE OHUV-IDDISTR   TO WS-IDDISTR-EVENT                
410600                   MOVE OHUV-IDKUNDNR  TO WS-IDKUNDNR-EVENT               
410700                   MOVE RAD-IDORDNR7   TO WS-IDORDNR7-EVENT               
410800                   MOVE RAD-TIREGDAT   TO WS-TIREGDAT-EVENT               
410900                   MOVE JA             TO CREATE-EVENT-SW                 
411000                 END-IF                                                   
411100               ELSE                                                       
411200                 MOVE KORD-IDDISTR     TO WS-IDDISTR-EVENT                
411300                 MOVE KORD-IDKUNDNR    TO WS-IDKUNDNR-EVENT               
411400                 MOVE KORD-IDORDNR7(1:5) TO WS-IDORDNR7-EVENT(3:5)        
411500                 MOVE ZERO             TO WS-IDORDNR7-EVENT(1:2)          
411600                 MOVE KORD-TIORDREG    TO WS-TIREGDAT-EVENT               
411700                 MOVE JA               TO CREATE-EVENT-SW                 
411800               END-IF                                                     
411900             END-IF                                                       
412000           END-IF                                                         
412010         END-IF                                                           
412100                                                                          
412200         IF CREATE-EVENT                                                  
412300           PERFORM S12F-CREATE-EVENT-HANDLING-152                         
412400           MOVE NEJ               TO CREATE-EVENT-SW                      
412500         END-IF                                                           
412600                                                                          
412700         IF  SPAR-ORAD-KDORDKL = 0                                        
412800         AND SPAR-ORAD-FLSDCLEV NOT = JA                                  
412900            IF DCS-NDC OR (DCS-SDC AND DCS-CHINA)                         
413000              PERFORM S12C-UPDATE-VOR-QUE                                 
413100            ELSE                                                          
413200              PERFORM S12D-UPDATE-VORKONY                                 
413300            END-IF                                                        
413400            PERFORM S19-DELETE-PRICE-Q-LINE                               
413500         END-IF                                                           
413600       END-IF                                                             
413700     END-IF                                                               
413800                                                                          
413900     IF WS-KDORDBEK              = 90 OR                                  
414000       (WS-KDORDBEK              = 91 AND                                 
414100        SW-TIRODAT-LIKA-MED-ZERO = JA)                                    
414200       PERFORM S12E-CREATE-RYK-TRANS                                      
414300     END-IF                                                               
414400     .                                                                    
414500     EJECT                                                                
414600 S12A-UPDATE-RY1-POST         SECTION.                                    
414700     MOVE 'S12A-UPDATE-RY1-PO'     TO CURRENT-SECTION                     
414800                                                                          
414900     IF LOGGA-IDLOGLOP = 9                                                
415000        MOVE ZERO                TO   LOGGA-IDLOGLOP                      
415100     END-IF                                                               
415200                                                                          
415300     ACCEPT LOGGA-TIAAMMDD                FROM DATE                       
415400     ACCEPT LOGGA-TIKLOCK                 FROM TIME                       
415500     ADD +1                               TO   LOGGA-IDLOGLOP             
415600     MOVE 'RY1'                           TO   RY1-IDPTYP                 
415700                                               LOGGA-IDPTYP               
415800     MOVE SPAR-ORAD-BERADREF              TO   RY1-BERADREF               
415900     MOVE SPAR-ORAD-BEVOLREF              TO   RY1-BEVOLREF               
416000     MOVE WS-IDKUNDRF                     TO   RY1-IDKUNDRF               
416100     MOVE SPAR-ORAD-IDARTNR               TO   RY1-IDARTNR                
416200     MOVE SPAR-ORAD-FLRESTN               TO   RY1-FLRESTN                
416300     MOVE SPAR-ORAD-FLDIRLEV              TO   RY1-FLDIRLEV               
416400     MOVE SPAR-ORAD-IDKUNDRF-RO           TO   RY1-IDKUNDRF-RO            
416500     MOVE SPAR-ORAD-FLTILLK               TO   RY1-FLTILLK                
416600                                                                          
416700     IF SPAR-ORAD-KDDSP = 0                                               
416800        MOVE 1                            TO  RY1-KDDSP                   
416900     ELSE                                                                 
417000        MOVE SPAR-ORAD-KDDSP              TO  RY1-KDDSP                   
417100     END-IF                                                               
417200     MOVE WS-KDFAKTYP                     TO  RY1-KDFAKTYP                
417300                                                                          
417400     IF   WS-KDORDBEK = 90 AND WS-KDTPOTYP =  6                           
417500       MOVE 91                            TO  RY1-KDORDBEK                
417600     ELSE                                                                 
417700       MOVE WS-KDORDBEK                   TO  RY1-KDORDBEK                
417800     END-IF                                                               
417900                                                                          
418000     MOVE SPAR-ORAD-KDORDING              TO  RY1-KDORDING                
418100     MOVE SPAR-ORAD-KDORDTYP              TO  RY1-KDORDTYP                
418200     MOVE SPAR-ORAD-KDKVBRYT              TO  RY1-KDKVBRYT                
418300     MOVE WS-KDTPOTYP                     TO  RY1-KDTPOTYP                
418400     MOVE SPAR-ORAD-KDVRINFO              TO  RY1-KDVRINFO                
418500     MOVE SPAR-ORAD-KVBEART               TO  RY1-KVBEART                 
418600     MOVE SPAR-ORAD-KVAVBART              TO  RY1-KVAVBART                
418700     MOVE WS-KVORAPP-TOTAL                TO  RY1-KVAVART                 
418800     MOVE SPAR-ORAD-KVLEVART              TO  RY1-KVLEVART                
418900     MOVE SPAR-ORAD-REKSIFFR              TO  RY1-REKSIFFR                
419000                                                                          
419100     MOVE SPAR-ORAD-IDARTNR               TO  W-IDARTNR                   
419200                                                                          
419300     PERFORM IMS-GU-ARTC11                                                
419400     MOVE DCS-IDDC TO W-711-IDDC                                          
419500*                                                                         
419600     MOVE CLAG-IDANSK         TO WS-IDANSK                                
419700     IF DCS-NDC-CN OR (DCS-SDC AND DCS-CHINA)                             
419800       PERFORM IMS-GU-WDK722                                              
419900       IF SEGMENT-FINNS AND XLAG-IDANSK > 0                               
420000          MOVE XLAG-IDANSK TO WS-IDANSK                                   
420100       END-IF                                                             
420200     END-IF                                                               
420300*                                                                         
420400     MOVE CLAG-TIDISPIN                   TO  RY1-TIDISPIN                
420500                                              WS-TIDISPIN                 
420600                                                                          
420700     MOVE SPAR-ORAD-TIUTSKR               TO  RY1-TIORDREG                
420800     MOVE SPAR-ORAD-TIRODAT               TO  RY1-TIRODAT                 
420900                                                                          
421300     MOVE WS-IDDISTR                      TO  RY1S-IDDISTR                
421400     MOVE WS-IDDC                         TO  RY1S-IDDC                   
421500     MOVE WS-IDKUNDNR                     TO  RY1S-IDKUNDNR               
421600     IF SEGMENT-FINNS                                                     
421700       IF OHUV-FLVORKO = JA                                               
421800       OR OHUV-FLVORKO = YES                                              
421900           MOVE JA                        TO  RY1S-FLVORKO                
422000       ELSE                                                               
422100           MOVE OHUV-FLVORKO              TO  RY1S-FLVORKO                
422200       END-IF                                                             
422300       MOVE OHUV-FLFORBI                  TO  RY1S-FLFORBI                
422400       MOVE OHUV-FLOVRLEV                 TO  RY1S-FLOVRLEV               
422500     ELSE                                                                 
422600       MOVE 'N'                           TO  RY1S-FLVORKO                
422700       MOVE 'N'                           TO  RY1S-FLFORBI                
422800       MOVE WS-FLOVRLEV                 TO  RY1S-FLOVRLEV                 
422900     END-IF                                                               
423000     MOVE SPAR-ORAD-KDPRODSL              TO  RY1S-KDPRODSL               
423100     MOVE SPAR-ORAD-IDSYSTEM              TO  RY1S-IDSYSTEM               
423200     MOVE WS-FLLSBOK                      TO  RY1S-FLLSBOK                
423300     MOVE WS-FLORDSPE                     TO  RY1S-FLORDSPE               
423400     MOVE SPAR-ORAD-KDFRAKT               TO  RY1S-KDFRAKT                
423500     MOVE SPAR-ORAD-KDORDKL               TO  RY1S-KDORDKL                
423600     MOVE SPAR-ORAD-KVANNANT              TO  RY1S-KVANNANT               
423700     MOVE SPAR-ORAD-KVSLATT               TO  RY1S-KVSLATT                
423800                                                                          
423900     MOVE RY1S-WDGZRY1S                   TO  LOGGA-SORTPOST              
424000     MOVE RY1-WDGZRY1                     TO  LOGGA-LOGGPOST              
424100*                                                                         
424200     PERFORM IMS-ISRT-AVVIKELSE                                           
424300*                                                                         
424400     PERFORM UNTIL SEGMENT-FINNS                                          
424500                                                                          
424600       IF LOGGA-IDLOGLOP = 9                                              
424700         MOVE ZERO              TO LOGGA-IDLOGLOP                         
424800         ACCEPT  LOGGA-TIKLOCK   FROM TIME                                
424900       END-IF                                                             
425000                                                                          
425100       ADD +1                   TO LOGGA-IDLOGLOP                         
425200       PERFORM IMS-ISRT-AVVIKELSE                                         
425300     END-PERFORM                                                          
425400     EJECT                                                                
425500     .                                                                    
425600 S12B-UPDATE-ORDBEK-WDQ1      SECTION.                                    
425700     MOVE 'S12B-UPDATE-ORD   '     TO CURRENT-SECTION                     
425800                                                                          
425900     MOVE KORD-IDORDER                    TO OBKR-IDORDER                 
426000     MOVE SPAR-ORAD-IDARTNR               TO OBKR-IDARTNR                 
426100*--- LÄS FRAM TILL FÖRSTA LEDIGA LÖPNR                                    
426200     MOVE OBKR-IDORDER                    TO W-IDORDER-Q1-MIN             
426300                                             W-IDORDER-Q1-MAX             
426400     MOVE OBKR-IDARTNR                    TO W-IDARTNR-Q1-MIN             
426500                                             W-IDARTNR-Q1-MAX             
426600     MOVE +1                              TO W-IDLOPNR-Q1-MIN             
426700                                             W-IDLOPNR-Q1-MAX             
426800                                             W-IDSEKVNR-Q1-MIN            
426900                                             W-IDSEKVNR-Q1-MAX            
427000     PERFORM IMS-GU-ORQM01                                                
427100     PERFORM UNTIL SEGMENT-SAKNAS                                         
427200        ADD +1                            TO W-IDLOPNR-Q1-MIN             
427300                                             W-IDLOPNR-Q1-MAX             
427400        PERFORM IMS-GU-ORQM01                                             
427500     END-PERFORM                                                          
427600     MOVE W-IDLOPNR-Q1-MIN                TO OBKR-IDLOPNR                 
427700     MOVE 1                               TO OBKR-IDSEKVNR                
427800     MOVE WS-IDDC                         TO OBKR-IDDC                    
427900     MOVE SPAR-ORAD-IDDC-RO               TO OBKR-IDDC-RO                 
428000     MOVE SPAR-ORAD-KDOI                  TO OBKR-KDOI                    
428100     MOVE SPAR-ORAD-CLEARGROUP            TO OBKR-CLEARGROUP              
428200     MOVE WS-KDORDBEK                     TO OBKR-KDORDBEK                
428300     MOVE IDPGM                           TO OBKR-IDPGM                   
428400     MOVE SPACE                           TO OBKR-BEERS                   
428500     MOVE SPAR-BEKUNDRF                   TO OBKR-BEKUNDRF                
428600     MOVE SPAR-ORAD-BERADREF              TO OBKR-BERADREF                
428700     MOVE SPAR-ORAD-BEVOLREF              TO OBKR-BEVOLREF                
428800     MOVE SPAR-ORAD-IDKAMPRF              TO OBKR-IDKAMPRF                
428900     MOVE 0                               TO OBKR-DIERS-KVOT              
429000     MOVE NEJ                             TO OBKR-FLAKPLOC                
429100     MOVE NEJ                             TO OBKR-FLSLATT                 
429200     MOVE SPAR-ORAD-FLINVEST              TO OBKR-FLINVEST                
429300     MOVE JA                              TO OBKR-FLOBOK                  
429400     MOVE NEJ                             TO OBKR-FLOBTRAN                
429500     MOVE NEJ                             TO OBKR-FLOBPRT                 
429600     MOVE SPAR-ORAD-FLPRTILL              TO OBKR-FLPRTILL                
429700     MOVE SPAR-ORAD-FLRESTN               TO OBKR-FLRESTN                 
429800     MOVE NEJ                             TO OBKR-FLTILLK                 
429900     MOVE 0                               TO OBKR-IDARTNR-TILLK           
430000     MOVE KORD-IDDISTR                    TO OBKR-IDDISTR                 
430100     MOVE KORD-IDKUNDNR                   TO OBKR-IDKUNDNR                
430200                                                                          
430300     MOVE KORD-IDKUNDRF                   TO WS-IDKUNDRF-OLD              
430400     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
430500     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF                
430600                                                                          
430700     MOVE SPAR-ORAD-IDKUNDRF-RO           TO WS-IDKUNDRF-OLD              
430800     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
430900     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF-RO             
431000     MOVE SPAR-ORAD-IDLEVNR               TO OBKR-IDLEVNR                 
431100     MOVE SPAR-ORAD-IDLOPNR-RO            TO OBKR-IDLOPNR-RO              
431200     MOVE SPAR-ORAD-IDSYSTEM              TO OBKR-IDSYSTEM                
431300     MOVE SPAR-ORAD-KDDSP                 TO OBKR-KDDSP                   
431400     MOVE 0                               TO OBKR-KDERS                   
431500     MOVE SPAR-ORAD-KDKVBRYT              TO OBKR-KDKVBRYT                
431600     MOVE SPAR-ORAD-KDPRTYP               TO OBKR-KDPRTYP                 
431700     MOVE 0                               TO OBKR-KDTPOTYP                
431800     MOVE SPAR-ORAD-KDVRINFO              TO OBKR-KDVRINFO                
431900                                                                          
432000     EVALUATE TRUE                                                        
432100       WHEN WS-KDORDBEK = 90 OR 91                                        
432200         MOVE 0                          TO OBKR-KVANNANT                 
432300       WHEN WS-KDORDBEK = 80 OR 81 OR 93                                  
432400         IF DCS-CDC OR DCS-NDC                                            
432500           COMPUTE OBKR-KVANNANT = SPAR-ORAD-KVBEART                      
432600                                 - SPAR-ORAD-KVLEVART                     
432700                                 - SPAR-ORAD-KVANNANT                     
432800           END-COMPUTE                                                    
432900         ELSE                                                             
433000           COMPUTE OBKR-KVANNANT = SPAR-ORAD-KVAVBART                     
433100                                 - SPAR-ORAD-KVLEVART                     
433200           END-COMPUTE                                                    
433300         END-IF                                                           
433400       WHEN OTHER                                                         
433500         MOVE 'FELAKTIG ORDERBEKR-KOD'   TO RKOD-FELTEXT                  
433600         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
433700     END-EVALUATE                                                         
433800                                                                          
433900     MOVE SPAR-ORAD-KVAVBART              TO OBKR-KVAVBART                
434000     MOVE SPAR-ORAD-KVBEART               TO OBKR-KVBEART                 
434100                                             OBKR-KVBEART-Q               
434200     MOVE 0                               TO OBKR-KVBEART-TILLK           
434300     MOVE 0                               TO OBKR-KVPREAVB                
434400     MOVE 0                               TO OBKR-KVPRERO                 
434500     MOVE CLAG-KVQPACK-1                  TO OBKR-KVQPACK                 
434600*                                                                         
434700     IF WS-KDORDBEK = 90 OR 91                                            
434800        IF DCS-CDC OR DCS-NDC                                             
434900          MOVE WS-KVORAPP-TOTAL             TO OBKR-KVRO                  
435000        ELSE                                                              
435100          MOVE WS-KVORAPP-PACK              TO OBKR-KVRO                  
435200        END-IF                                                            
435300        MOVE WS-DAGENS-DATUM              TO OBKR-TIRODAT                 
435400     ELSE                                                                 
435500        MOVE 0                            TO OBKR-KVRO                    
435600        MOVE 000000                       TO OBKR-TIRODAT                 
435700     END-IF                                                               
435800*                                                                         
435900     MOVE SPAR-ORAD-KVSLATT               TO OBKR-KVSLATT                 
436000     MOVE SPAR-ORAD-PRARTNTO              TO OBKR-PRARTNTO                
436100     MOVE SPAR-ORAD-PRARTNTO-LOC          TO OBKR-PRARTNTO-LOC            
436200     MOVE SPAR-ORAD-PRARTNTO-LOCPREL      TO OBKR-PRARTNTO-LOCPREL        
436300     MOVE 0                               TO OBKR-PRBPRIS                 
436400     MOVE SPAR-ORAD-REKSIFFR              TO OBKR-REKSIFFR                
436500     MOVE 0                               TO OBKR-REKSIFFR-TILLK          
436600     MOVE 0                               TO OBKR-RERF-RAD                
436700     MOVE WS-TIDISPIN                     TO OBKR-TIDISPIN                
436800     MOVE KORD-TIORDREG                   TO OBKR-TIORDREG                
436900     MOVE ZERO                            TO WS-DAORDREG                  
437000     MOVE FUNCTION CURRENT-DATE(1:2)      TO WS-DAORDREG-TISS             
437100     MOVE OBKR-TIORDREG                   TO WS-DAORDREG-TIAAMMDD         
437200     COMPUTE OBKR-TITIORDD-9KOMPL =                                       
437300             WS-9KOMPL-GRUND - WS-DAORDREG                                
437400     MOVE SPAR-ORAD-TIPRIS                TO OBKR-TIPRIS                  
437500     MOVE WS-DAGENS-DATUM                 TO OBKR-TIREGDAT                
437600     MOVE WS-TTMMSS                       TO OBKR-TIREGTID                
437700                                                                          
437800     IF WS-KDORDBEK = 90 OR 91                                            
437900        MOVE ZERO                         TO WS-DARODAT                   
438000        MOVE FUNCTION CURRENT-DATE(1:2)   TO WS-DARODAT-TISS              
438100        MOVE OBKR-TIRODAT                 TO WS-DARODAT-TIAAMMDD          
438200        COMPUTE OBKR-TITIREGD-9KOMPL =                                    
438300             WS-9KOMPL-GRUND - WS-DARODAT                                 
438400     ELSE                                                                 
438500        MOVE 0                            TO OBKR-TITIREGD-9KOMPL         
438600     END-IF                                                               
438700                                                                          
438800     MOVE 0                                TO OBKR-TITPO                  
438900     MOVE SPAR-ORAD-KDFRAKT                TO OBKR-KDFRAKT                
439000     MOVE SPAR-ORAD-KDORDKL                TO OBKR-KDORDKL                
439100     MOVE SPACE                            TO OBKR-IDBIL                  
439200     MOVE SPACE                         TO OBKR-KDORDTYP-LDC              
439300     MOVE SPAR-ORAD-IDKUNDRF-WIP        TO OBKR-IDKUNDRF-WIP              
439400     MOVE ZERO                          TO OBKR-TIREPDAT                  
439500     MOVE ZERO                          TO OBKR-TIDLEVDAT                 
439600     MOVE SPAR-ORAD-PRAVCOST            TO OBKR-PRAVCOST                  
439700     IF  SPAR-ORAD-PRARTBTO-LOC NUMERIC                                   
439800     AND SPAR-ORAD-IDPRQUES     NUMERIC                                   
439900     AND OBKR-PRARTBTO-LOC NUMERIC                                        
440000       MOVE SPAR-ORAD-IDPRQUES             TO OBKR-IDPRQUES               
440100       MOVE SPAR-ORAD-PRARTBTO-LOC         TO OBKR-PRARTBTO-LOC           
440200       IF SPAR-ORAD-PRAVCOST > ZERO                                       
440300         MOVE SPAR-ORAD-KDVALISO-EXP       TO OBKR-KDVALISO               
440400       ELSE                                                               
440500         MOVE SPAR-ORAD-KDVALISO           TO OBKR-KDVALISO               
440600       END-IF                                                             
440700       MOVE SPAR-ORAD-KDVAT                TO OBKR-KDVAT                  
440800       MOVE SPAR-ORAD-RERAB                TO OBKR-RERAB                  
440900       MOVE SPAR-ORAD-KDRAB                TO OBKR-KDRAB                  
441000       MOVE SPAR-ORAD-BEART-VIPS           TO OBKR-BEART-VIPS             
441100     ELSE                                                                 
441200       MOVE ZERO                           TO OBKR-IDPRQUES               
441300       MOVE ZERO                           TO OBKR-PRARTBTO-LOC           
441400       MOVE SPACE                          TO OBKR-KDVALISO               
441500       MOVE SPACE                          TO OBKR-KDVAT                  
441600       MOVE ZERO                           TO OBKR-RERAB                  
441700       MOVE SPACE                          TO OBKR-KDRAB                  
441800       MOVE SPACE                          TO OBKR-BEART-VIPS             
441900     END-IF                                                               
442000*                                                                         
442100     IF OBKR-KDORDBEK = 90 OR 91 OR 92 OR 93                              
442200        MOVE OBKR-IDARTNR         TO W-IDARTNR                            
442300        IF OBKR-IDDC NOT = DCS-IDDC                                       
442400           MOVE OBKR-IDDC             TO W-IDDC-B6                        
442500           PERFORM IMS-GU-WDB601                                          
442600        END-IF                                                            
442700        MOVE DCS-IDLANDX2         TO W-IDLAND                             
442800        PERFORM IMS-GU-WDK712                                             
442900        IF SEGMENT-FINNS AND LART-FLREFERAL = JA                          
443000           MOVE 98                TO OBKR-KDORDBEK                        
443100        END-IF                                                            
443200     END-IF                                                               
443300     PERFORM IMS-ISRT-ORQM01                                              
443400*                                                                         
443500                                                                          
443600     IF (OBKR-IDSYSTEM = 'LDC' OR 'TACD')                                 
443700        MOVE OHUV-IDDISTR       TO W-IDDISTR-WDB2                         
443800        MOVE OHUV-IDKUNDNR      TO W-IDKUNDNR-WDB2                        
443900                                                                          
444000        PERFORM IMS-GU-GMTA-WDB201                                        
444100        IF GMT-FLOBKR-TACD = JA                                           
444200*          CONTINUE                                                       
444300           PERFORM S12B1-CREATE-TACD-402                                  
444400        END-IF                                                            
444500     END-IF                                                               
444600     .                                                                    
444700     EJECT                                                                
444800 S12B1-CREATE-TACD-402 SECTION.                                           
444900     MOVE 'S12B1-UPDATE-VOR-QUE'   TO CURRENT-SECTION                     
445000     MOVE 'PU1'                   TO 402-IDPTYP                           
445100     MOVE 01                      TO 402-IDVTYP-TACDIS                    
445200     MOVE 20                      TO WS-TIAA                              
445300     MOVE WS-DAGENS-DATUM         TO WS-TIAAMMDD                          
445400     MOVE WS-TIAAAAMMDD           TO 402-DAREGDAT                         
445500     MOVE OBKR-IDDISTR            TO 402-IDDISTR                          
445600     MOVE OBKR-IDKUNDNR           TO 402-IDKUNDNR                         
445700     MOVE OBKR-IDORDNR7           TO 402-IDORDNR7                         
445800                                                                          
445900     MOVE 'VO '                   TO CIA-IDARTPRE-IN                      
446000     MOVE OBKR-IDARTNR            TO CIA-IDARTBET-IN                      
446100     CALL W009CIA              USING CIA-W009CIA                          
446200     MOVE CIA-IDARTBET-UT         TO 402-IDARTBET                         
446300                                                                          
446400     MOVE OBKR-KDORDBEK           TO 402-KDORDBEK                         
446500     MOVE OBKR-KVBEART            TO 402-KVBEART                          
446600     MOVE OBKR-IDSEKVNR           TO 402-IDSEKVNR                         
446700                                                                          
446800     MOVE 'VO '                   TO CIA-IDARTPRE-IN                      
446900     MOVE OBKR-IDARTNR-TILLK      TO CIA-IDARTBET-IN                      
447000     CALL W009CIA              USING CIA-W009CIA                          
447100     MOVE CIA-IDARTBET-UT         TO 402-IDARTBET-TILLK                   
447200                                                                          
447300     MOVE OBKR-KVBEART-TILLK      TO 402-KVLEVART                         
447400     MOVE OBKR-IDDC               TO 402-IDDC                             
447500     MOVE OBKR-TIDLEVDAT          TO 402-DADLEVDAT                        
447600     IF 402-DADLEVDAT > ZERO                                              
447700        ADD 20000000              TO 402-DADLEVDAT                        
447800     END-IF                                                               
447900                                                                          
448000*    IF WS-KV402 = 0                                                      
448100       PERFORM S21-SEND-OPEN                                              
448200       MOVE OBKR-IDKUNDNR         TO 402-IDKUNDNR                         
448300       MOVE OBKR-IDORDNR7         TO 402-IDORDNR7                         
448400       PERFORM S22-PUT-HEADER                                             
448500*    END-IF                                                               
448600     ADD +1                       TO WS-KV402                             
448700     PERFORM S25-PUT-LINE                                                 
448800     PERFORM S29-SEND-CLOSE                                               
448900     .                                                                    
449000     EJECT                                                                
449100                                                                          
449200 S12C-UPDATE-VOR-QUE          SECTION.                                    
449300     MOVE 'S12C-UPDATE-VOR-QUE'    TO CURRENT-SECTION                     
449400                                                                          
449500     IF WS-IDDC NOT = W-IDDC-B6                                           
449600        MOVE WS-IDDC TO W-IDDC-B6                                         
449700        PERFORM IMS-GU-WDB601                                             
449800     END-IF                                                               
449900                                                                          
450000     IF WS-FLORDSPE = NEJ AND WS-FLOVRLEV = NEJ                           
450100        MOVE SPAR-ORAD-BERADREF            TO 4542-BERADREF               
450200        MOVE KORD-IDDISTR                  TO 4542-IDDISTR                
450300        MOVE OBKR-IDDC                     TO 4542-IDDC                   
450400        MOVE WS-IDANSK                     TO 4542-IDANSK                 
450500        MOVE SPAR-ORAD-IDARTNR             TO 4542-IDARTNR                
450600        MOVE KORD-IDKUNDNR                 TO 4542-IDKUNDNR               
450700        MOVE KORD-IDKUNDRF                 TO WS-IDKUNDRF-OLD             
450800        MOVE WS-IDORDNR5-OLD               TO WS-IDORDNR7-NEW             
450900        MOVE WS-IDKUNDRF-NEW               TO 4542-IDKUNDRF               
451000        MOVE KORD-IDORDER                  TO 4542-IDORDER                
451100        MOVE SPACE                         TO 4542-IDUSER                 
451200        MOVE 93                            TO 4542-KDORDBEK               
451300        MOVE SPAR-ORAD-KDPRTYP             TO 4542-KDPRTYP                
451400        MOVE 0                             TO 4542-KDVORATG               
451500        MOVE SPAR-ORAD-KVBEART             TO 4542-KVBEART                
451600                                              4542-KVBEART-Q              
451700        IF SPAR-ORAD-FLFYSAVV = JA                                        
451800          IF DCS-SDC AND DCS-CHINA                                        
451900            MOVE ZERO                 TO WS-AVVIKELSE-UTSKR               
452000            COMPUTE WS-AVVIKELSE-UTSKR =                                  
452100                    SPAR-ORAD-KVBEART - SPAR-ORAD-KVAVBART                
452200            END-COMPUTE                                                   
452300            COMPUTE WS-SUMMA =                                            
452400                    SPAR-ORAD-KVLEVART + WS-AVVIKELSE-UTSKR               
452500            END-COMPUTE                                                   
452600            MOVE WS-SUMMA                  TO 4542-KVPREAVB               
452700          ELSE                                                            
452800            MOVE SPAR-ORAD-KVLEVART        TO 4542-KVPREAVB               
452900          END-IF                                                          
453000*         WS-SUMMA MINUS ORAD-KVBEART ÄR AVVIKELSE I PACKNINGEN           
453100*         VILKET ÄR DET SOM VISAS (RÄKNAS FRAM) PÅ 4224-BILDEN            
453200        ELSE                                                              
453300          MOVE SPAR-ORAD-KVLEVART         TO 4542-KVPREAVB                
453400        END-IF                                                            
453500        MOVE SPAR-ORAD-PRARTNTO           TO 4542-PRARTNTO                
453600        MOVE SPAR-ORAD-PRARTNTO-LOC       TO 4542-PRARTNTO-LOC            
453700        MOVE SPAR-ORAD-PRARTNTO-LOCPREL   TO 4542-PRARTNTO-LOCPREL        
453800        MOVE SPACE                        TO 4542-TEVORMRK                
453900        MOVE WS-DAGENS-DATUM              TO 4542-TIREGDAT                
454000        MOVE WS-TTMMSS                    TO 4542-TIREGTID                
454100        MOVE 0                            TO 4542-TIUPPDAT                
454200        MOVE 0                            TO 4542-TIUPPTID                
454300        MOVE 1                            TO 4542-IDLOPNR                 
454400        MOVE SPAR-ORAD-IDLEVNR            TO 4542-IDLEVNR                 
454500        IF ORAD-PRARTBTO-LOC NUMERIC                                      
454600        AND ORAD-IDPRQUES  NUMERIC                                        
454700          MOVE SPAR-ORAD-IDPRQUES     TO 4542-IDPRQUES                    
454800          MOVE SPAR-ORAD-PRARTBTO-LOC TO 4542-PRARTBTO-LOC                
454900          IF SPAR-ORAD-PRAVCOST > ZERO                                    
455000            MOVE SPAR-ORAD-KDVALISO-EXP TO 4542-KDVALISO                  
455100          ELSE                                                            
455200            MOVE SPAR-ORAD-KDVALISO     TO 4542-KDVALISO                  
455300          END-IF                                                          
455400          MOVE SPAR-ORAD-KDVAT        TO 4542-KDVAT                       
455500          MOVE SPAR-ORAD-RERAB        TO 4542-RERAB                       
455600          MOVE SPAR-ORAD-KDRAB        TO 4542-KDRAB                       
455700          MOVE SPAR-ORAD-BEART-VIPS   TO 4542-BEART-VIPS                  
455800        ELSE                                                              
455900          MOVE ZERO                   TO 4542-IDPRQUES                    
456000          MOVE ZERO                   TO 4542-PRARTBTO-LOC                
456100          MOVE SPACE                  TO 4542-KDVALISO                    
456200          MOVE SPACE                  TO 4542-KDVAT                       
456300          MOVE ZERO                   TO 4542-RERAB                       
456400          MOVE SPACE                  TO 4542-KDRAB                       
456500          MOVE SPACE                  TO 4542-BEART-VIPS                  
456600        END-IF                                                            
456700*                                                                         
456800        PERFORM IMS-ISRT-4542                                             
456900        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
457000          ADD +1                           TO 4542-IDLOPNR                
457100          PERFORM IMS-ISRT-4542                                           
457200        END-PERFORM                                                       
457300                                                                          
457400          IF SPAR-ORAD-IDLEVNR = SPACE AND                                
457500             WS-FLORDSPE = NEJ        AND                                 
457600             WS-FLOVRLEV = NEJ                                            
457700             IF  DCS-NDC OR                                               
457800                (SPAR-ORAD-FLFYSAVV = JA  AND                             
457900                     (DCS-SDC AND DCS-CHINA))                             
458000               MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                    
458100               MOVE WS-IDDC               TO W-711-IDDC                   
458200               PERFORM IMS-GHU-WDK711                                     
458300                                                                          
458400               IF  DCS-NDC                                                
458500                 COMPUTE SLAG-KVOKS-DAG =                                 
458600                         SLAG-KVOKS-DAG +                                 
458700                         (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)         
458800                 END-COMPUTE                                              
458900               ELSE                                                       
459000                 COMPUTE SLAG-KVOKS-DAG =                                 
459100                         SLAG-KVOKS-DAG +                                 
459200                        (SPAR-ORAD-KVAVBART - SPAR-ORAD-KVLEVART)         
459300                 END-COMPUTE                                              
459400               END-IF                                                     
459500               PERFORM IMS-REPL-WDK7                                      
459600               IF  DCS-NDC-CN                                             
459700               OR (DCS-NDC-NA AND DCS-USA)                                
459800                 IF SLAG-IDDC-REF = SPACE                                 
459900                                                                          
460000                    MOVE WS-IDDISTR        TO S27-IDDISTR                 
460100                    MOVE WS-IDKUNDNR       TO S27-IDKUNDNR                
460200                    MOVE SPAR-ORAD-IDARTNR TO S27-IDARTNR                 
460300                    MOVE CLAG-IDANSK       TO S27-IDANSK                  
460400                    PERFORM IMS-GU-WDK722                                 
460500                    IF SEGMENT-FINNS AND XLAG-IDANSK > 0                  
460600                      MOVE XLAG-IDANSK     TO S27-IDANSK                  
460700                    END-IF                                                
460800                    MOVE WS-IDDC           TO S27-IDDC                    
460900                    MOVE SLAG-IDLEVNR      TO S27-IDLEVNR                 
461000                    PERFORM S27-STARTA-W2T191X                            
461100                 END-IF                                                   
461200               END-IF                                                     
461300             END-IF                                                       
461400          END-IF                                                          
461500                                                                          
461600     END-IF                                                               
461700     .                                                                    
461800     EJECT                                                                
461900 S12D-UPDATE-VORKONY          SECTION.                                    
462000     MOVE 'S12D-UPDATE-VORKONY'    TO CURRENT-SECTION                     
462100                                                                          
462200     MOVE SPAR-ORAD-IDARTNR TO S28-IDARTNR                                
462300     MOVE DCS-IDDC          TO S28-IDDC                                   
462400     PERFORM S28-BESTAM-LENVR-ANSK                                        
462500                                                                          
462600*----------------------------------RADEN SKALL FINNAS PÅ VORKÖ            
462700                                                                          
462800     PERFORM S26-SOK-RAD-VORKO                                            
462900                                                                          
463000     IF  TRAFF-VORKO                                                      
463100         IF DCS-CDC                                                       
463200             COMPUTE VOR-KVPREAVB  = VOR-KVPREAVB                         
463300                                   - SPAR-ORAD-KVBEART                    
463400                                   + SPAR-ORAD-KVLEVART                   
463500             END-COMPUTE                                                  
463600             COMPUTE VOR-KVBEART-Q = VOR-KVBEART-Q                        
463700                                   - SPAR-ORAD-KVBEART                    
463800                                   + SPAR-ORAD-KVLEVART                   
463900             END-COMPUTE                                                  
464000         ELSE                                                             
464100             COMPUTE VOR-KVPREAVB  = VOR-KVPREAVB                         
464200                                   - SPAR-ORAD-KVAVBART                   
464300                                   + SPAR-ORAD-KVLEVART                   
464400             END-COMPUTE                                                  
464500             COMPUTE VOR-KVBEART-Q = VOR-KVBEART-Q                        
464600                                   - SPAR-ORAD-KVAVBART                   
464700                                   + SPAR-ORAD-KVLEVART                   
464800             END-COMPUTE                                                  
464900         END-IF                                                           
465000         IF  VOR-KVPREAVB = 0                                             
465100             MOVE '7'              TO VOR-KDVORATG                        
465200             MOVE 93               TO VOR-KDORDBEK                        
465300             IF VOR-TIKLAR = ZERO                                         
465400                MOVE WS-DAGENS-DATUM TO VOR-TIKLAR                        
465500                COMPUTE VOR-TIKLATID  = WS-VOR-TID-BRIST                  
465600                                      / 100                               
465700                END-COMPUTE                                               
465800             END-IF                                                       
465900         END-IF                                                           
466000         PERFORM IMS-REPL-SEQB-WDA601                                     
466100                                                                          
466200         MOVE WS-DAGENS-DATUM   TO VOR-TIREGDAT-AVV                       
466300         ADD +1                 TO WS-VOR-TID-BRIST                       
466400         MOVE WS-VOR-TID-BRIST  TO VOR-TIREGTID-AVV                       
466500         SUBTRACT WS-DAGENS-DATUM FROM 9999999                            
466600                                  GIVING VOR-TIREGDAT-AVV9                
466700         SUBTRACT WS-VOR-TID-BRIST FROM 999999999                         
466800                                   GIVING VOR-TIREGTID-AVV9               
466900         MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                       
467000         MOVE 0                 TO VOR-TIREGDAT-LEV                       
467100         MOVE 0                 TO VOR-TIREGTID-LEV                       
467200         IF DCS-CDC                                                       
467300             SUBTRACT SPAR-ORAD-KVLEVART                                  
467400                                  FROM SPAR-ORAD-KVBEART                  
467500                                  GIVING VOR-KVBEART                      
467600                                         VOR-KVBEART-Q                    
467700         ELSE                                                             
467800             SUBTRACT SPAR-ORAD-KVLEVART                                  
467900                                  FROM SPAR-ORAD-KVAVBART                 
468000                                  GIVING VOR-KVBEART                      
468100                                         VOR-KVBEART-Q                    
468200         END-IF                                                           
468300         MOVE 0                 TO VOR-KVPREAVB                           
468400         MOVE WS-IDDC           TO VOR-IDDC                               
468500         MOVE SPACE             TO VOR-IDUSER                             
468600         MOVE 93                TO VOR-KDORDBEK                           
468700         MOVE '0'               TO VOR-KDVORATG                           
468800         MOVE 0                 TO VOR-TIKLAR                             
468900         MOVE 0                 TO VOR-TIKLATID                           
469000                                                                          
469100         PERFORM IMS-ISRT-WDA601                                          
469200         PERFORM UNTIL ISRT-OK                                            
469300            ADD +1                TO WS-VOR-TID-BRIST                     
469400                                     VOR-TIREGTID-URSP                    
469500            MOVE WS-VOR-TID-BRIST TO VOR-TIREGTID-AVV                     
469600            SUBTRACT WS-VOR-TID-BRIST FROM 999999999                      
469700                                  GIVING VOR-TIREGTID-AVV9                
469800            PERFORM IMS-ISRT-WDA601                                       
469900         END-PERFORM                                                      
470000     ELSE                                                                 
470100*--------------------------------------- EJ TRÄFF, FEJKA ORG. RAD         
470200*                                        BORDE NOG INTE FÖREKOMMA         
470300       MOVE WS-IDDISTR      TO VOR-IDDISTR                                
470400       MOVE WS-IDKUNDNR     TO VOR-IDKUNDNR                               
470500       MOVE WS-IDKUNDRF         TO WS-IDKUNDRF-OLD                        
470600       MOVE WS-IDORDNR5-OLD     TO WS-IDORDNR7-NEW                        
470700       MOVE WS-IDKUNDRF-NEW     TO VOR-IDKUNDRF                           
470800       MOVE KORD-TIORDREG       TO VOR-TIREGDAT-URSP                      
470900       MOVE SPAR-ORAD-IDARTNR   TO VOR-IDARTNR                            
471000       ADD +1                   TO WS-VOR-TID-BRIST                       
471100       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-URSP                      
471200       MOVE 0                   TO VOR-TIREGDAT-AVV                       
471300       MOVE 0                   TO VOR-TIREGTID-AVV                       
471400       SUBTRACT 0            FROM 9999999                                 
471500                              GIVING VOR-TIREGDAT-AVV9                    
471600       SUBTRACT 0            FROM 999999999                               
471700                              GIVING VOR-TIREGTID-AVV9                    
471800       MOVE WS-IDKUNDRF-NEW     TO VOR-IDKUNDRF-LEV                       
471900       MOVE KORD-TIORDREG       TO VOR-TIREGDAT-LEV                       
472000       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-LEV                       
472100       MOVE S28-IDANSK          TO VOR-IDANSK                             
472200                                                                          
472300       MOVE VOR-IDDISTR         TO W-IDDISTR-P4                           
472400       PERFORM IMS-GU-WDP4A1                                              
472500       IF SEGMENT-SAKNAS                                                  
472600          MOVE DEF-IDROLL  TO SEQA-IDROLL                                 
472700       END-IF                                                             
472800       MOVE SEQA-IDROLL         TO VOR-IDROLL                             
472900                                                                          
473000       MOVE S28-IDLEVNR         TO VOR-IDLEVNR                            
473100       MOVE SPAR-ORAD-BERADREF  TO VOR-BERADREF                           
473200       IF  DCS-CDC                                                        
473300           SUBTRACT SPAR-ORAD-KVLEVART                                    
473400                                FROM SPAR-ORAD-KVBEART                    
473500                                GIVING VOR-KVBEART-URSP                   
473600                                       VOR-KVBEART                        
473700       ELSE                                                               
473800           SUBTRACT SPAR-ORAD-KVLEVART                                    
473900                                FROM SPAR-ORAD-KVAVBART                   
474000                                GIVING VOR-KVBEART-URSP                   
474100                                       VOR-KVBEART                        
474200       END-IF                                                             
474300       MOVE 0                   TO VOR-KVPREAVB                           
474400                                   VOR-KVBEART-Q                          
474500       MOVE WS-IDDC             TO VOR-IDDC                               
474600       MOVE SPACE               TO VOR-IDUSER                             
474700       MOVE 93                  TO VOR-KDORDBEK                           
474800       MOVE SPAR-ORAD-KDPRTYP   TO VOR-KDPRTYP                            
474900       MOVE '7'                  TO VOR-KDVORATG                          
475000       MOVE SPAR-ORAD-PRARTNTO  TO VOR-PRARTNTO                           
475100       MOVE '  '                TO VOR-TEVORMRK                           
475200*      MOVE '  '                TO VOR-TEVORMRK-SC                        
475300       MOVE 0                   TO VOR-TIUPPDAT                           
475400       MOVE 0                   TO VOR-TIUPPTID                           
475500       MOVE WS-DAGENS-DATUM     TO VOR-TIKLAR                             
475600       COMPUTE VOR-TIKLATID     = WS-VOR-TID-BRIST                        
475700                                / 100                                     
475800       END-COMPUTE                                                        
475900       MOVE SPAR-ORAD-DEAL-PR-LINE TO VOR-DEAL-PR-LINE                    
476000       MOVE NEJ                 TO VOR-FLVORFK                            
476100       PERFORM IMS-ISRT-WDA601                                            
476200       PERFORM UNTIL ISRT-OK                                              
476300          ADD +1              TO VOR-TIREGTID-URSP                        
476400          ADD +1              TO VOR-TIREGTID-LEV                         
476500          PERFORM IMS-ISRT-WDA601                                         
476600       END-PERFORM                                                        
476700                                                                          
476800*--------------------------------------- EJ TRÄFF, AVVIK RAD              
476900       MOVE WS-DAGENS-DATUM   TO VOR-TIREGDAT-AVV                         
477000       ADD +1                 TO WS-VOR-TID-BRIST                         
477100       MOVE WS-VOR-TID-BRIST  TO VOR-TIREGTID-AVV                         
477200       SUBTRACT WS-DAGENS-DATUM  FROM 9999999                             
477300                                 GIVING VOR-TIREGDAT-AVV9                 
477400       SUBTRACT WS-VOR-TID-BRIST FROM 999999999                           
477500                                 GIVING VOR-TIREGTID-AVV9                 
477600       MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                         
477700       MOVE 0                 TO VOR-TIREGDAT-LEV                         
477800       MOVE 0                 TO VOR-TIREGTID-LEV                         
477900       IF  DCS-CDC                                                        
478000           SUBTRACT SPAR-ORAD-KVLEVART                                    
478100                                FROM SPAR-ORAD-KVBEART                    
478200                                GIVING VOR-KVBEART-Q                      
478300       ELSE                                                               
478400           SUBTRACT SPAR-ORAD-KVLEVART                                    
478500                                FROM SPAR-ORAD-KVAVBART                   
478600                                GIVING VOR-KVBEART-Q                      
478700       END-IF                                                             
478800       MOVE 0                 TO VOR-KVPREAVB                             
478900       MOVE 93                TO VOR-KDORDBEK                             
479000       MOVE '0'               TO VOR-KDVORATG                             
479100       MOVE 0                 TO VOR-TIKLAR                               
479200       MOVE 0                 TO VOR-TIKLATID                             
479300                                                                          
479400       PERFORM IMS-ISRT-WDA601                                            
479500       PERFORM UNTIL ISRT-OK                                              
479600          ADD +1                TO WS-VOR-TID-BRIST                       
479700          MOVE WS-VOR-TID-BRIST TO VOR-TIREGTID-AVV                       
479800          SUBTRACT WS-VOR-TID-BRIST FROM 999999999                        
479900                                GIVING VOR-TIREGTID-AVV9                  
480000          PERFORM IMS-ISRT-WDA601                                         
480100       END-PERFORM                                                        
480200     END-IF                                                               
480300                                                                          
480400     COMPUTE CLAG-KVVORKO       = CLAG-KVVORKO                            
480500                                + VOR-KVBEART-Q                           
480600                                - VOR-KVPREAVB                            
480700     END-COMPUTE                                                          
480800                                                                          
480900     PERFORM IMS-REPL-WDK611                                              
481000                                                                          
481100     MOVE WS-IDDISTR        TO S27-IDDISTR                                
481200     MOVE WS-IDKUNDNR       TO S27-IDKUNDNR                               
481300     MOVE SPAR-ORAD-IDARTNR     TO S27-IDARTNR                            
481400     MOVE S28-IDANSK            TO S27-IDANSK                             
481500     MOVE WC-CDC-SE             TO S27-IDDC                               
481600     MOVE SPACE                 TO S27-IDLEVNR                            
481700     PERFORM S27-STARTA-W2T191X                                           
481800                                                                          
481900*------------------------------SAMMA EFTERHANTERING SOM I S12C            
482000     IF SPAR-ORAD-IDLEVNR = SPACE AND                                     
482100        WS-FLORDSPE = NEJ        AND                                      
482200        WS-FLOVRLEV = NEJ                                                 
482300                                                                          
482400        IF WS-IDDC NOT = W-IDDC-B6                                        
482500           MOVE WS-IDDC TO W-IDDC-B6                                      
482600           PERFORM IMS-GU-WDB601                                          
482700        END-IF                                                            
482800                                                                          
482900        IF  DCS-NDC                                                       
483000        OR (SPAR-ORAD-FLFYSAVV = JA AND DCS-SDC)                          
483100          MOVE SPAR-ORAD-IDARTNR     TO W-IDARTNR                         
483200          MOVE WS-IDDC               TO W-711-IDDC                        
483300          PERFORM IMS-GHU-WDK711                                          
483400                                                                          
483500          IF  DCS-NDC                                                     
483600            COMPUTE SLAG-KVOKS-DAG =                                      
483700                    SLAG-KVOKS-DAG +                                      
483800                    (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)              
483900            END-COMPUTE                                                   
484000          ELSE                                                            
484100            COMPUTE SLAG-KVOKS-DAG =                                      
484200                    SLAG-KVOKS-DAG +                                      
484300                   (SPAR-ORAD-KVAVBART - SPAR-ORAD-KVLEVART)              
484400            END-COMPUTE                                                   
484500          END-IF                                                          
484600                                                                          
484700          PERFORM IMS-REPL-WDK7                                           
484800        ELSE                                                              
484900          IF  DCS-CDC                                                     
485000            MOVE SPAR-ORAD-IDARTNR        TO W-901-IDARTNR                
485100            PERFORM IMS-GHU-WDK901                                        
485200                                                                          
485300            COMPUTE ART-KVOKS-VOR =                                       
485400                    ART-KVOKS-VOR +                                       
485500                    (SPAR-ORAD-KVBEART - SPAR-ORAD-KVLEVART)              
485600            END-COMPUTE                                                   
485700                                                                          
485800            PERFORM IMS-REPL-WDK901                                       
485900          END-IF                                                          
486000        END-IF                                                            
486100     END-IF                                                               
486200     .                                                                    
486300     EJECT                                                                
486400 S12F-CREATE-EVENT-HANDLING-152    SECTION.                               
486500     MOVE 'S12F-CREATE-EVENT-HANDLING-152'  TO  CURRENT-SECTION           
486600                                                                          
486700*    EVENT HANTERING                                                      
486800     MOVE OHUV-IDSYSTEM(1:4)   TO EVENT-SW                                
486900     IF EVENT-OK OR LYNK-NON-API                                          
487000                                                                          
487100       IF (OHUV-IDSYSTEM(1:3) = 'LYN') OR LYNK-NON-API                    
487200         MOVE 'L'              TO WS-PARTNER                              
487300       END-IF                                                             
487400       IF OHUV-IDSYSTEM(1:3) = 'POL'                                      
487500         MOVE 'P'              TO WS-PARTNER                              
487600       END-IF                                                             
487700       IF OHUV-IDSYSTEM(1:3) = 'ECO'                                      
487800         MOVE 'E'              TO WS-PARTNER                              
487900       END-IF                                                             
488000       IF OHUV-IDSYSTEM(1:3) = 'TAD'                                      
488100         MOVE 'T'              TO WS-PARTNER                              
488200       END-IF                                                             
488210       IF OHUV-IDSYSTEM(1:3) = 'ACC'                                      
488220         MOVE 'A'              TO WS-PARTNER                              
488230       END-IF                                                             
488240       IF OHUV-IDSYSTEM(1:3) = 'APA'                                      
488250         MOVE 'K'              TO WS-PARTNER                              
488260       END-IF                                                             
488270       IF OHUV-IDSYSTEM(1:3) = 'APB'                                      
488280         MOVE 'B'              TO WS-PARTNER                              
488290       END-IF                                                             
488291       IF OHUV-IDSYSTEM(1:3) = 'APC'                                      
488292         MOVE 'C'              TO WS-PARTNER                              
488293       END-IF                                                             
488294       IF OHUV-IDSYSTEM(1:3) = 'APD'                                      
488295         MOVE 'D'              TO WS-PARTNER                              
488296       END-IF                                                             
488297       IF OHUV-IDSYSTEM(1:3) = 'APE'                                      
488298         MOVE 'M'              TO WS-PARTNER                              
488299       END-IF                                                             
488300       IF OHUV-IDSYSTEM(1:3) = 'APF'                                      
488301         MOVE 'F'              TO WS-PARTNER                              
488302       END-IF                                                             
488303       IF OHUV-IDSYSTEM(1:3) = 'APG'                                      
488304         MOVE 'G'              TO WS-PARTNER                              
488305       END-IF                                                             
488306       IF OHUV-IDSYSTEM(1:3) = 'APH'                                      
488307         MOVE 'H'              TO WS-PARTNER                              
488308       END-IF                                                             
488309       IF OHUV-IDSYSTEM(1:3) = 'API'                                      
488310         MOVE 'I'              TO WS-PARTNER                              
488311       END-IF                                                             
488312       IF OHUV-IDSYSTEM(1:3) = 'APJ'                                      
488313         MOVE 'J'              TO WS-PARTNER                              
488314       END-IF                                                             
488320                                                                          
488400       MOVE SPACE               TO Z430-REQU-TIMESTAMP                    
488500       PERFORM S12FA-CREATE-EVENT-152                                     
488600                                                                          
488700     END-IF                                                               
488800     .                                                                    
488900     EJECT                                                                
489000 S12FA-CREATE-EVENT-152   SECTION.                                        
489100     MOVE 'S12FA-CREATE-EVENT-152'    TO CURRENT-SECTION                  
489200                                                                          
489300     MOVE '001'                 TO Z430-REQU-IDMSGVER                     
489310     IF LYNK-NON-API                                                      
489320        MOVE 'LYNK'             TO Z430-REQU-IDEVENTREC                   
489330     ELSE                                                                 
489340        MOVE OHUV-IDSYSTEM      TO Z430-REQU-IDEVENTREC                   
489350     END-IF                                                               
489500     MOVE 'PURCHASEORDER'       TO Z430-REQU-IDEVENT                      
489600     MOVE 'UPDATE'              TO Z430-REQU-IDEVENTTYP                   
489700***  MOVE WS-TIMESTAMP          TO Z430-REQU-TIMESTAMP                    
489800     MOVE FUNCTION CURRENT-DATE TO Z430-REQU-TIMESTAMP                    
489900     MOVE 'WAPIORD'             TO Z430-REQU-IDCPYTXT                     
490000     MOVE WS-IDEVENTORDREF      TO Z430-IDAPIORDREF                       
490100     MOVE '152'                 TO Z430-IDMSG                             
490200     MOVE 'ORDER LINE IS BACKORDERED AT PACKREPORTING'                    
490300                                TO Z430-TEMFSINF                          
490400                                                                          
490500*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
490600*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
490700     MOVE 'WZ0430X '           TO MSG-KDTRANS-1                           
490800     MOVE 'Z430'               TO MSG-IDTRANS-1                           
490900     MOVE '1'                  TO MSG-KDMFSFOR-1                          
491000     MOVE 'WZ0430I1'           TO MSG-KOM-IDCPYTXT                        
491100     STRING 'EVE' WS-PARTNER WS-IDDISTR-EVENT                             
491200          DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                         
491300                                                                          
491400     ADD  1                    TO MSG-KOM-TIKLOCK                         
491500     COMPUTE MSG-KVLL = LENGTH OF Z430-REQU-WZ0430I1 + 17                 
491600     MOVE Z430-REQU-WZ0430I1     TO MSG-INDATA-MINUS-1-TRANSKOD           
491700                                                                          
491800     CALL W006KOM USING MSG-PCB                                           
491900                        0693-PCB                                          
492000                        KOM-WDP8-PCB                                      
492100                        MSG-KOM-WMSGKOM                                   
492200                        MSG-IO-AREA                                       
492300                                                                          
492400     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
492500        MOVE                                                              
492600        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
492700                                     TO ERRORTEX                          
492800        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
492900     END-IF                                                               
493000     .                                                                    
493100     EJECT                                                                
493200                                                                          
493300 S12G-EVENTS-NON-API SECTION.                                             
493400                                                                          
493401     MOVE 'S12G-EVENTS-NON-'       TO CURRENT-SECTION                     
493402                                                                          
493403     IF VOR                                                               
493404       MOVE LOW-VALUE          TO W-WDA6BSEQ-MIN-X                        
493405       MOVE HIGH-VALUE         TO W-WDA6BSEQ-MAX-X                        
493406                                                                          
493407       MOVE OHUV-IDDISTR       TO W-A6BSEQ-MIN-IDDISTR                    
493408                                  W-A6BSEQ-MAX-IDDISTR                    
493409       MOVE OHUV-IDKUNDNR      TO W-A6BSEQ-MIN-IDKUNDNR                   
493410                                  W-A6BSEQ-MAX-IDKUNDNR                   
493411       MOVE OHUV-IDKUNDRF      TO W-A6BSEQ-MIN-IDKUNDRF-LEV               
493412                                  W-A6BSEQ-MAX-IDKUNDRF-LEV               
493413       PERFORM IMS-GU-SEQB-WDA601                                         
493414       IF SEGMENT-FINNS                                                   
493415         MOVE OHUV-IDDISTR     TO WS-IDDISTR-EVENT                        
493416         MOVE OHUV-IDKUNDNR    TO WS-IDKUNDNR-EVENT                       
493417         MOVE VOR-IDORDNR7      TO WS-IDORDNR7-EVENT                      
493418         MOVE VOR-TIREGDAT-URSP TO WS-TIREGDAT-EVENT                      
493419         MOVE JA               TO CREATE-EVENT-SW                         
493420       END-IF                                                             
493421     ELSE                                                                 
493422       IF (ORAD-IDKUNDRF-RO NOT = '0000000   ') AND                       
493423          (ORAD-IDKUNDRF-RO NOT = '00000     ')                           
493424         MOVE KORD-IDDISTR         TO   W-WDQ2CSEQ-IDDISTR                
493425         MOVE KORD-IDKUNDNR        TO   W-WDQ2CSEQ-IDKUNDNR               
493426         MOVE SPAR-ORAD-IDKUNDRF-RO(1:5)                                  
493427                                  TO W-WDQ2CSEQ-IDKUNDRF(3:7)             
493428         PERFORM IMS-GU-ORQI01-CSEQ                                       
493429         IF SEGMENT-FINNS                                                 
493430           MOVE OHUV-IDORDER       TO  RYK-IDORDER                        
493431                                                                          
493432           MOVE OHUV-IDDISTR       TO WS-IDDISTR-EVENT                    
493433           MOVE OHUV-IDKUNDNR      TO WS-IDKUNDNR-EVENT                   
493434           MOVE OHUV-IDORDNR7      TO WS-IDORDNR7-EVENT                   
493435           MOVE OHUV-TIREGDAT      TO WS-TIREGDAT-EVENT                   
493436****       MOVE KORD-TIORDREG      TO WS-TIREGDAT-EVENT                   
493437           MOVE JA                 TO CREATE-EVENT-SW                     
493438         END-IF                                                           
493439       ELSE                                                               
493440         MOVE OHUV-IDDISTR     TO W-IDDISTR-A5-MIN                        
493441                                  W-IDDISTR-A5-MAX                        
493442         MOVE OHUV-IDKUNDNR    TO W-IDKUNDNR-A5-MIN                       
493443                                  W-IDKUNDNR-A5-MAX                       
493444         MOVE OHUV-KDORDKL     TO W-KDORDKL                               
493445         MOVE OHUV-IDORDNR7(3:5) TO W-IDKUNDRF-LEV                        
493446                                                                          
493447         PERFORM IMS-GU-WDA501                                            
493448         IF SEGMENT-FINNS                                                 
493449           MOVE OHUV-IDDISTR   TO WS-IDDISTR-EVENT                        
493450           MOVE OHUV-IDKUNDNR  TO WS-IDKUNDNR-EVENT                       
493451           MOVE RAD-IDORDNR7   TO WS-IDORDNR7-EVENT                       
493452           MOVE RAD-TIREGDAT   TO WS-TIREGDAT-EVENT                       
493453           MOVE JA             TO CREATE-EVENT-SW                         
493454         END-IF                                                           
493455       END-IF                                                             
493456     END-IF                                                               
493457     IF CREATE-EVENT-SW = NEJ                                             
493458       MOVE KORD-IDDISTR     TO WS-IDDISTR-EVENT                          
493459       MOVE KORD-IDKUNDNR    TO WS-IDKUNDNR-EVENT                         
493460       MOVE KORD-IDORDNR7(1:5) TO WS-IDORDNR7-EVENT(3:5)                  
493461       MOVE ZERO             TO WS-IDORDNR7-EVENT(1:2)                    
493462       MOVE KORD-TIORDREG    TO WS-TIREGDAT-EVENT                         
493463       MOVE JA               TO CREATE-EVENT-SW                           
493464     END-IF                                                               
493465     .                                                                    
493466     EJECT                                                                
493467                                                                          
493468 S20-FINN-INTERVALL  SECTION.                                             
493470                                                                          
493500     PERFORM IMS-GU-WDM211                                                
493600     IF SEGMENT-FINNS                                                     
493700       PERFORM IMS-GNP-WDM221                                             
493800       PERFORM UNTIL SEGMENT-SAKNAS                                       
493900         IF  WS-IDDISTR > KMRK-IDDISTR-TOM                                
494000         OR  WS-IDDISTR < KMRK-IDDISTR-FOM                                
494100           CONTINUE                                                       
494200         ELSE                                                             
494300           IF  WS-IDDISTR  = KMRK-IDDISTR-TOM                             
494400           AND WS-IDKUNDNR > KMRK-IDKUNDNR-TOM                            
494500             CONTINUE                                                     
494600           ELSE                                                           
494700             IF  WS-IDDISTR  = KMRK-IDDISTR-FOM                           
494800             AND WS-IDKUNDNR < KMRK-IDKUNDNR-FOM                          
494900               CONTINUE                                                   
495000             ELSE                                                         
495100               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
495200               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
495300               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
495400               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
495500             END-IF                                                       
495600           END-IF                                                         
495700         END-IF                                                           
495800         PERFORM IMS-GNP-WDM221                                           
495900       END-PERFORM                                                        
496000     END-IF                                                               
496100     .                                                                    
496200     EJECT                                                                
496300                                                                          
496400 S26-SOK-RAD-VORKO SECTION.                                               
496500                                                                          
496600     IF WS-IDDC NOT = W-IDDC-B6                                           
496700        MOVE WS-IDDC TO W-IDDC-B6                                         
496800        PERFORM IMS-GU-WDB601                                             
496900     END-IF                                                               
497000     MOVE LOW-VALUE      TO W-WDA601KY-MIN-X                              
497100     MOVE HIGH-VALUE     TO W-WDA601KY-MAX-X                              
497200                                                                          
497300     MOVE WS-IDDISTR    TO W-A601KY-MIN-IDDISTR                           
497400                               W-A601KY-MAX-IDDISTR                       
497500     MOVE WS-IDKUNDNR   TO W-A601KY-MIN-IDKUNDNR                          
497600                               W-A601KY-MAX-IDKUNDNR                      
497700     MOVE WS-IDKUNDRF       TO WS-IDKUNDRF-OLD                            
497800     MOVE WS-IDORDNR5-OLD   TO WS-IDORDNR7-NEW                            
497900     MOVE WS-IDKUNDRF-NEW   TO W-A601KY-MIN-IDKUNDRF                      
498000                               W-A601KY-MAX-IDKUNDRF                      
498100     MOVE KORD-TIORDREG     TO W-A601KY-MIN-TIREGDAT                      
498200                               W-A601KY-MAX-TIREGDAT                      
498300     MOVE SPAR-ORAD-IDARTNR TO W-A601KY-MIN-IDARTNR                       
498400                               W-A601KY-MAX-IDARTNR                       
498500     MOVE NEJ               TO TRAFF-VORKO-SW                             
498600                                                                          
498700     PERFORM IMS-GHU-SEQB-WDA601                                          
498800     PERFORM UNTIL SEGMENT-SAKNAS                                         
498900                OR SEGMENT-SLUT                                           
499000                OR TRAFF-VORKO                                            
499100       IF  DCS-CDC                                                        
499200       AND SPAR-ORAD-KVBEART = VOR-KVPREAVB                               
499300           MOVE JA       TO TRAFF-VORKO-SW                                
499400       ELSE                                                               
499500         IF  SPAR-ORAD-KVAVBART = VOR-KVPREAVB                            
499600             MOVE JA       TO TRAFF-VORKO-SW                              
499700         ELSE                                                             
499800            PERFORM IMS-GHN-SEQB-WDA601                                   
499900         END-IF                                                           
500000       END-IF                                                             
500100     END-PERFORM                                                          
500200     .                                                                    
500300     EJECT                                                                
500400 S27-STARTA-W2T191X  SECTION.                                             
500500                                                                          
500600     COMPUTE ALT2-LL = LENGTH OF ALT2-MID-W2I19101 + 17                   
500700     MOVE +1                    TO ALT2-MID-KDCLAGER                      
500800     MOVE S27-IDARTNR-X         TO ALT2-MID-IDARTNR                       
500900     MOVE ZERO                  TO ALT2-MID-TISENBEK-DAG                  
501000                                   ALT2-MID-TISENBEK-KL                   
501100     MOVE SPACE                 TO ALT2-MID-IDKR                          
501200     MOVE S27-IDANSK-X          TO ALT2-MID-IDANSK                        
501300     MOVE '500'                 TO ALT2-MID-KDLARM                        
501400     MOVE S27-IDDISTR-X         TO ALT2-MID-IDDISTR                       
501500     MOVE S27-IDKUNDNR-X        TO ALT2-MID-IDKUNDNR                      
501600     MOVE WS-IDKUNDRF           TO WS-IDKUNDRF-OLD                        
501700     MOVE WS-IDORDNR5-OLD       TO WS-IDORDNR7-NEW                        
501800     MOVE WS-IDKUNDRF-NEW       TO ALT2-MID-IDKUNDRF                      
501900     MOVE 'J'                   TO ALT2-MID-FLNYLARM                      
502000     MOVE S27-IDDC              TO ALT2-MID-IDDC                          
502100     MOVE S27-IDLEVNR           TO ALT2-MID-IDLEVNR                       
502200                                                                          
502300     PERFORM IMS-PURG-ALT2-MSG                                            
502400                                                                          
502500     MOVE SPACE                 TO ALT2-MID-W2I19101                      
502600     .                                                                    
502700     EJECT                                                                
502800 S28-BESTAM-LENVR-ANSK SECTION.                                           
502900                                                                          
503000     MOVE S28-IDARTNR    TO W-IDARTNR                                     
503100     MOVE S28-IDDC       TO W-711-IDDC                                    
503200     PERFORM IMS-GU-WDK601                                                
503300     MOVE ART-IDLEVNR    TO S28-IDLEVNR                                   
503400                                                                          
503500     PERFORM IMS-GHNP-WDK611                                              
503600     MOVE CLAG-IDANSK    TO S28-IDANSK                                    
503700     MOVE CLAG-KVVORKO   TO S28-KVVORKO                                   
503800     .                                                                    
503900     EJECT                                                                
504000 S12E-CREATE-RYK-TRANS SECTION.                                           
504100                                                                          
504200     IF LOGGA-IDLOGLOP = 9                                                
504300       MOVE ZERO                TO   LOGGA-IDLOGLOP                       
504400     END-IF                                                               
504500                                                                          
504600     ACCEPT  LOGGA-TIAAMMDD     FROM DATE                                 
504700     ACCEPT  LOGGA-TIKLOCK      FROM TIME                                 
504800     ADD     +1                 TO   LOGGA-IDLOGLOP                       
504900                                                                          
505000     MOVE    'RYK'              TO   RYK-IDPTYP                           
505100                                     LOGGA-IDPTYP                         
505200                                                                          
505300     MOVE    KORD-IDDISTR       TO   RYK-IDDISTR                          
505400     MOVE    KORD-IDKUNDNR      TO   RYK-IDKUNDNR                         
505500                                                                          
505600     IF SPAR-ORAD-IDKUNDRF-RO = '00000     '                              
505700       MOVE  KORD-IDORDER       TO   RYK-IDORDER                          
505800     ELSE                                                                 
505900       MOVE KORD-IDDISTR        TO   W-WDQ2CSEQ-IDDISTR                   
506000       MOVE KORD-IDKUNDNR       TO   W-WDQ2CSEQ-IDKUNDNR                  
506100       MOVE SPAR-ORAD-IDKUNDRF-RO(1:5)                                    
506200                                TO   W-WDQ2CSEQ-IDKUNDRF(3:7)             
506300       PERFORM IMS-GU-ORQI01-CSEQ                                         
506400       IF SEGMENT-FINNS                                                   
506500         MOVE OHUV-IDORDER      TO   RYK-IDORDER                          
506600       ELSE                                                               
506700         MOVE ZERO              TO   RYK-IDORDER                          
506800       END-IF                                                             
506900     END-IF                                                               
507000                                                                          
507100     MOVE    SPAR-ORAD-IDARTNR  TO   RYK-IDARTNR                          
507200     MOVE    DAT-TIAAMMDD       TO   RYK-TIRODAT                          
507300     MOVE    SPAR-ORAD-KVLEVART TO   RYK-KVLEVART                         
507400     MOVE    SPAR-ORAD-KVBEART  TO   RYK-KVBEART-Q                        
507500     MOVE    SPAR-ORAD-KDORDKL  TO   RYK-KDORDKL                          
507600     MOVE    SPAR-ORAD-KDPRODSL TO   RYK-KDPRODSL                         
507700                                                                          
507800     MOVE    WS-KDORDBEK        TO   RYK-KDORDBEK                         
507900                                                                          
508000     MOVE    SPACE              TO   LOGGA-SORTPOST                       
508100     MOVE    RYK-WDGZRYK        TO   LOGGA-LOGGPOST                       
508200                                                                          
508300     PERFORM IMS-ISRT-AVVIKELSE                                           
508400                                                                          
508500     PERFORM UNTIL SEGMENT-FINNS                                          
508600                                                                          
508700       IF LOGGA-IDLOGLOP = 9                                              
508800         MOVE ZERO              TO LOGGA-IDLOGLOP                         
508900         ACCEPT  LOGGA-TIKLOCK   FROM TIME                                
509000       END-IF                                                             
509100       ADD +1                   TO LOGGA-IDLOGLOP                         
509200       PERFORM IMS-ISRT-AVVIKELSE                                         
509300     END-PERFORM                                                          
509400     .                                                                    
509500     EJECT                                                                
509600 S13-GENERERA-KLAR-SV4        SECTION.                                    
509700     MOVE 'S13-GENERERA-KLAR-SV4'  TO CURRENT-SECTION                     
509800                                                                          
509900     IF LOGGA-IDLOGLOP = 9                                                
510000       MOVE ZERO                TO   LOGGA-IDLOGLOP                       
510100     END-IF                                                               
510200                                                                          
510300     ACCEPT LOGGA-TIAAMMDD                FROM DATE                       
510400     ACCEPT LOGGA-TIKLOCK                 FROM TIME                       
510500     ADD +1                               TO   LOGGA-IDLOGLOP             
510600     MOVE 'RY6'                           TO   RY6-IDPTYP                 
510700                                               LOGGA-IDPTYP               
510800     MOVE VORD-IDDISTR                    TO   RY6-IDDISTR                
510900     MOVE VORD-IDKUNDNR                   TO   RY6-IDKUNDNR               
511000     MOVE WS-IDKUNDRF                     TO   RY6-IDKUNDRF               
511100     MOVE VORD-IDDC                       TO   RY6-IDDC                   
511200     MOVE VORD-IDPRODNR                   TO   RY6-IDPRODNR               
511300     MOVE SPACE                           TO  LOGGA-SORTPOST              
511400     MOVE RY6-WDGZRY6                     TO  LOGGA-LOGGPOST              
511500*                                                                         
511600     PERFORM IMS-ISRT-KLAR-SV4                                            
511700*                                                                         
511800     PERFORM UNTIL SEGMENT-FINNS                                          
511900                                                                          
512000       IF LOGGA-IDLOGLOP = 9                                              
512100         MOVE ZERO              TO LOGGA-IDLOGLOP                         
512200         ACCEPT  LOGGA-TIKLOCK   FROM TIME                                
512300       END-IF                                                             
512400       ADD +1                   TO LOGGA-IDLOGLOP                         
512500       PERFORM IMS-ISRT-KLAR-SV4                                          
512600     END-PERFORM                                                          
512700     .                                                                    
512800     EJECT                                                                
512900 S14-EV-LARM-2191-MID  SECTION.                                           
513000     MOVE 'S14-EV-LARM-2191-MID'   TO CURRENT-SECTION                     
513100                                                                          
513200** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
513300     IF CLAG-KVROS = 0                                                    
513400       IF CLAG-KVAKS-CDC = 0                                              
513500         COMPUTE ALT2-LL = LENGTH OF ALT2-MID-W2I19101 + 17               
513600         MOVE +1              TO ALT2-MID-KDCLAGER                        
513700         MOVE ORAD-IDARTNR    TO ALT2-MID-IDARTNR                         
513800         MOVE ZERO            TO ALT2-MID-TISENBEK-DAG                    
513900                                 ALT2-MID-TISENBEK-KL                     
514000         MOVE SPACE           TO ALT2-MID-IDKR                            
514100         MOVE WS-IDANSK       TO ALT2-MID-IDANSK                          
514200         MOVE 210             TO ALT2-MID-KDLARM                          
514300         MOVE WS-IDDISTR-NUM4 TO ALT2-MID-IDDISTR                         
514400         MOVE WS-IDKUNDNR     TO ALT2-MID-IDKUNDNR                        
514500         MOVE WS-IDKUNDRF     TO ALT2-MID-IDKUNDRF                        
514600         MOVE 'J'             TO ALT2-MID-FLNYLARM                        
514700         MOVE WC-CDC-SE       TO ALT2-MID-IDDC                            
514800         MOVE SPACE           TO ALT2-MID-IDLEVNR                         
514900                                                                          
515000         PERFORM IMS-PURG-ALT2-MSG                                        
515100       END-IF                                                             
515200     END-IF                                                               
515300     .                                                                    
515400     SKIP2                                                                
515500 S14-EV-LARM-2191-MID-CN-US SECTION.                                      
515600     MOVE 'S14-EV-LARM-2191-MID-CN-US' TO CURRENT-SECTION                 
515700                                                                          
515800** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
515900     IF SLAG-KVROS-DAG = 0 AND SLAG-KVROS-BULK = 0                        
516000       IF SLAG-KVAKS-PAV = 0 AND SLAG-KVAKS-SDC = 0                       
516100         COMPUTE ALT2-LL = LENGTH OF ALT2-MID-W2I19101 + 17               
516200         MOVE +1              TO ALT2-MID-KDCLAGER                        
516300         MOVE ORAD-IDARTNR    TO ALT2-MID-IDARTNR                         
516400         MOVE ZERO            TO ALT2-MID-TISENBEK-DAG                    
516500                                 ALT2-MID-TISENBEK-KL                     
516600         MOVE SPACE           TO ALT2-MID-IDKR                            
516700         MOVE CLAG-IDANSK     TO ALT2-MID-IDANSK                          
516800         IF WS-XLAG-IDANSK > 0                                            
516900           MOVE WS-XLAG-IDANSK TO ALT2-MID-IDANSK                         
517000         END-IF                                                           
517100         MOVE 210             TO ALT2-MID-KDLARM                          
517200         MOVE WS-IDDISTR-NUM4 TO ALT2-MID-IDDISTR                         
517300         MOVE WS-IDKUNDNR     TO ALT2-MID-IDKUNDNR                        
517400         MOVE WS-IDKUNDRF     TO ALT2-MID-IDKUNDRF                        
517500         MOVE 'J'             TO ALT2-MID-FLNYLARM                        
517600         MOVE SLAG-IDDC       TO ALT2-MID-IDDC                            
517700         MOVE SLAG-IDLEVNR    TO ALT2-MID-IDLEVNR                         
517800                                                                          
517900         PERFORM IMS-PURG-ALT2-MSG                                        
518000                                                                          
518100       END-IF                                                             
518200     END-IF                                                               
518300     .                                                                    
518400     SKIP2                                                                
518500 S16-ISRT-SALDOLOGG SECTION.                                              
518600     MOVE 'S16-ISRT-SALDOLOGG'     TO CURRENT-SECTION                     
518700                                                                          
518800     PERFORM IMS-ISRT-WDL901                                              
518900     IF SEGMENT-FINNS-REDAN                                               
519000       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
519100         SUBTRACT 1         FROM LOGG-IDSEKVNR                            
519200         PERFORM IMS-ISRT-WDL901                                          
519300       END-PERFORM                                                        
519400     END-IF                                                               
519500     .                                                                    
519600     EJECT                                                                
519700 S17-FLYTTA-SALDOLOGG-DATA SECTION.                                       
519800     MOVE 'S17-FLYTTA-SALDO  '     TO CURRENT-SECTION                     
519900                                                                          
520000     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
520100     ACCEPT WS-KLOCKAN               FROM TIME                            
520200     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
520300     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - WS-KLOCKAN                
520400     MOVE 9                   TO LOGG-IDSEKVNR                            
520500     MOVE 'OUTB'              TO LOGG-IDHUVTYP                            
520600     MOVE 'PAC'               TO LOGG-IDSUBTYP                            
520700     MOVE 'W4039700'          TO LOGG-IDPGM                               
520800     MOVE WS-IDTRANS          TO LOGG-IDTRANS                             
520900     MOVE WS-IDDC             TO IDDC-XX                                  
521000     MOVE IDDC-USER           TO LOGG-IDUSER                              
521100     MOVE SPACE               TO LOGG-REF                                 
521200     MOVE KORD-IDDISTR        TO LOGG-IDDISTR                             
521300     MOVE KORD-IDKUNDNR       TO LOGG-IDKUNDNR                            
521400     MOVE KORD-IDKUNDRF       TO LOGG-IDKUNDRF                            
521500     MOVE KORD-IDORDNR5       TO LOGG-IDORDNR5                            
521600     MOVE WS-IDPRODNR         TO LOGG-IDPRODNR                            
521700     MOVE WS-IDPLKLST         TO LOGG-IDPLKLST                            
521800     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
521900     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
522000     MOVE WS-KVORAPP-PACK     TO LOGG-KVART-SALDO                         
522100     MOVE '00000000'          TO LOGG-DAREGDAT-LADD                       
522200     .                                                                    
522300     EJECT                                                                
522400 S18-BERAKNA-SALDOLOGG-DATA SECTION.                                      
522500     MOVE 'S18-BERAKNA-SALDO '     TO CURRENT-SECTION                     
522600                                                                          
522700     PERFORM S17-FLYTTA-SALDOLOGG-DATA                                    
522800*    ---DB-SPECIFIK INFORMATION                                           
522900     MOVE W-IDARTNR           TO LOGG-IDARTNR                             
523000     MOVE W-711-IDDC          TO LOGG-IDDC                                
523100     MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                              
523200     MOVE SLAG-KVLS           TO LOGG-KVLS                                
523300     MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
523400     MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                               
523500     MOVE '-'                 TO LOGG-IDTECKEN-KVEFRS                     
523600     MOVE '+'                 TO LOGG-IDTECKEN-KVLS                       
523700     PERFORM S16-ISRT-SALDOLOGG                                           
523800     .                                                                    
523900     EJECT                                                                
524000*                                                                         
524100 S19-DELETE-PRICE-Q-LINE SECTION.                                         
524200     MOVE 'S19-DELETE-PRICE-Q'     TO CURRENT-SECTION                     
524300                                                                          
524400     IF DIST79-DEALER-PRICE                                               
524500       IF SPAR-ORAD-IDPRQUES > ZERO                                       
524600         INITIALIZE PRQU-W335PRQU                                         
524700         MOVE KORD-IDDISTR            TO PRQU-IDDISTR                     
524800         MOVE KORD-IDKUNDNR           TO PRQU-IDKUNDNR                    
524900         MOVE WS-IDKUNDRF-NEW         TO PRQU-IDKUNDRF                    
525000         MOVE SPAR-ORAD-IDPRQUES      TO PRQU-IDPRQUES                    
525100         MOVE 4                  TO PRQU-KDCALL                           
525200         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
525300                                            PRQU-WDC7-PCB                 
525400                                            PRQU-SJKO-WDK6-PCB            
525500       END-IF                                                             
525600     END-IF                                                               
525700     .                                                                    
525800     EJECT                                                                
525900 S21-SEND-OPEN SECTION.                                                   
526000     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
526100     MOVE 'OPEN'                  TO SEND-KDFUNC                          
526200     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
526300                                     SEND-OPEN-AREA                       
526400     IF SEND-KDRC > ZERO                                                  
526500       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
526600       STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                     
526700       DELIMITED BY SIZE INTO ERRORTEX                                    
526800       DISPLAY ERRORTEX                                                   
526900       CALL FELLOG                                                        
527000     END-IF                                                               
527100     .                                                                    
527200     EJECT                                                                
527300 S22-PUT-HEADER SECTION.                                                  
527400     MOVE 1                       TO REQU-IDMSGVER                        
527500     MOVE 'R'                     TO REQU-KDPGMACT                        
527600     MOVE IDPGM                   TO REQU-IDUSER                          
527700     MOVE 'ORDERCONF'             TO HDR-IDOUTTYPE                        
527800     MOVE OBKR-IDKUNDNR           TO WS-IDKUNDNR                          
527900     MOVE WS-IDKUNDNR             TO HDR-IDOUTREC                         
528000     MOVE OBKR-IDORDNR7           TO HDR-IDLIST                           
528100     MOVE 'PUT'                   TO SEND-KDFUNC                          
528200     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
528300     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
528400                                     SEND-KVDLEN                          
528500                                     HDR-AREA                             
528600     IF SEND-KDRC > ZERO                                                  
528700       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
528800       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
528900       DELIMITED BY SIZE       INTO ERRORTEX                              
529000       DISPLAY ERRORTEX                                                   
529100       CALL FELLOG                                                        
529200     END-IF                                                               
529300     .                                                                    
529400     EJECT                                                                
529416 S23-PUT-HEADER-ABEND SECTION.                                            
529420     MOVE 1                       TO REQU-IDMSGVER                        
529430     MOVE 'R'                     TO REQU-KDPGMACT                        
529440     MOVE IDPGM                   TO REQU-IDUSER                          
529450     MOVE 'WRONGPICK'             TO HDR-IDOUTTYPE                        
529460     MOVE '001'                   TO HDR-IDOUTREC                         
529470     MOVE '001'                   TO HDR-IDLIST                           
529490     MOVE 'PUT'                   TO SEND-KDFUNC                          
529491     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
529492     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
529493                                     SEND-KVDLEN                          
529494                                     HDR-AREA                             
529495     IF SEND-KDRC > ZERO                                                  
529496       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
529497       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
529498       DELIMITED BY SIZE       INTO ERRORTEX                              
529499       DISPLAY ERRORTEX                                                   
529500       CALL FELLOG                                                        
529501     END-IF                                                               
529502     .                                                                    
529503     EJECT                                                                
529504 S24-PUT-LINE-ABEND SECTION.                                              
529505     MOVE 'PUT'                   TO SEND-KDFUNC                          
529506     MOVE LENGTH OF WS-ABENDDATA  TO SEND-KVDLEN                          
529507     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
529508                                     SEND-KVDLEN                          
529509                                     WS-ABENDDATA                         
529510     IF SEND-KDRC > ZERO                                                  
529511       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
529512       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
529513       DELIMITED BY SIZE       INTO ERRORTEX                              
529514       DISPLAY ERRORTEX                                                   
529515       CALL FELLOG                                                        
529516     END-IF                                                               
529517     .                                                                    
529518     SKIP2                                                                
529519 S25-PUT-LINE SECTION.                                                    
529520     MOVE 'PUT'                   TO SEND-KDFUNC                          
529521     MOVE LENGTH OF 402-W402TACD  TO SEND-KVDLEN                          
529522     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
529523                                     SEND-KVDLEN                          
529524                                     402-W402TACD                         
529525     IF SEND-KDRC > ZERO                                                  
529526       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
529530       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
529540       DELIMITED BY SIZE       INTO ERRORTEX                              
529550       DISPLAY ERRORTEX                                                   
529560       CALL FELLOG                                                        
529570     END-IF                                                               
529580     .                                                                    
529590     SKIP2                                                                
531000 S29-SEND-CLOSE SECTION.                                                  
531100*LK  IF WS-KV402 > 0                                                      
531200       MOVE 'CLOSE'               TO SEND-KDFUNC                          
531300       CALL WZ01SEND           USING SEND-CONTROL-AREA                    
531400       IF SEND-KDRC > 0                                                   
531500         MOVE SEND-KDRC          TO KDRC-DISPLAY                          
531600         STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                  
531700         DELIMITED BY SIZE     INTO ERRORTEX                              
531800         DISPLAY ERRORTEX                                                 
531900         CALL FELLOG                                                      
532000       END-IF                                                             
532100*LK  END-IF                                                               
532200     .                                                                    
532300     EJECT                                                                
532400 S36-ANDRA-WDC711  SECTION.                                               
532500     MOVE 'S36-ANDRA-WDC711'       TO CURRENT-SECTION                     
532600     IF RAD-IDDISTR = 0778 AND RAD-KDORDKL < 3                            
532700       IF RAD-IDPRQUES > ZERO                                             
532800         INITIALIZE PRQU-W335PRQU                                         
532900         MOVE RAD-IDDISTR             TO PRQU-IDDISTR                     
533000         MOVE RAD-IDKUNDNR            TO PRQU-IDKUNDNR                    
533100         MOVE RAD-IDKUNDRF(1:5)       TO PRQU-IDKUNDRF(3:5)               
533200         MOVE '00'                    TO PRQU-IDKUNDRF(1:2)               
533300         MOVE RAD-IDPRQUES            TO PRQU-IDPRQUES                    
533400         MOVE 6                       TO PRQU-KDCALL                      
533500         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
533600                                            PRQU-WDC7-PCB                 
533700                                            PRQU-SJKO-WDK6-PCB            
533800         MOVE 'N'                     TO RAD-FLPRTILL                     
533900         IF RAD-PRARTNTO-LOC > +0                                         
534000           MOVE RAD-PRARTNTO-LOC      TO RAD-PRARTNTO-LOCPREL             
534100           MOVE ZERO                  TO RAD-PRARTNTO-LOC                 
534200         END-IF                                                           
534300         IF PRQU-KDCALL = -1                                              
534400           PERFORM S36-NY-FRAGA                                           
534500         END-IF                                                           
534600       ELSE                                                               
534700*CREATE NEW PRICE QUESTION                                                
534800         PERFORM S36-NY-FRAGA                                             
534900       END-IF                                                             
535000     END-IF                                                               
535100     .                                                                    
535200     EJECT                                                                
535300 S36-NY-FRAGA SECTION.                                                    
535400                                                                          
535500     MOVE ZERO                     TO WS-IDPRQUES                         
535600     IF WS-IDPRQUES                = +0                                   
535700        MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                    
535800        MOVE +1                    TO PRNO-KDCALL                         
535900                                                                          
536000        CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                   
536100                                                                          
536200                                                                          
536300        MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                       
536400                                      WS-IDPRQUES                         
536500        MOVE +1                    TO PRQU-KDCALL                         
536600     END-IF                                                               
536700     MOVE RAD-IDDISTR              TO PRQU-IDDISTR                        
536800     MOVE RAD-IDKUNDNR             TO PRQU-IDKUNDNR                       
536900     MOVE RAD-IDKUNDRF(1:5)        TO PRQU-IDKUNDRF(3:5)                  
537000     MOVE '00'                     TO PRQU-IDKUNDRF(1:2)                  
537100     MOVE ZERO                     TO PRQU-IDORDER                        
537200     MOVE RAD-KDORDKL              TO PRQU-KDORDKL                        
537300     IF RAD-IDDISTR = 0778 AND RAD-KDORDKL < 3                            
537400         AND RAD-DARODAT > 0                                              
537500         MOVE 4                    TO PRQU-KDORDKL                        
537600     END-IF                                                               
537700     MOVE 'Q'                      TO PRQU-KDPRSTA                        
537800     MOVE RAD-IDARTNR              TO PRQU-IDARTNR                        
537900     MOVE RAD-KVBEART-Q            TO PRQU-KVBEART-Q                      
538000     MOVE RAD-KDVALISO             TO PRQU-KDVALISO                       
538100     MOVE RAD-PRARTNTO-LOC         TO PRQU-PRARTNTO-LOC                   
538200     MOVE +0                       TO PRQU-PRARTNTO-LOCPREL               
538300     MOVE RAD-IDSYSTEM             TO PRQU-IDSYSTEM                       
538400*        PERFORM IMS-GU-GMTA-WDB201                                       
538500                                                                          
538600     CALL  W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                    
538700                                         PRQU-WDC7-PCB                    
538800                                         PRQU-SJKO-WDK6-PCB               
538900     MOVE PRQU-IDPRQUES            TO  RAD-IDPRQUES                       
539000                                       WS-IDPRQUES                        
539100     MOVE 'N'                      TO  RAD-FLPRTILL                       
539200                                                                          
539300     IF RAD-PRARTNTO-LOC = +0                                             
539400        MOVE PRQU-PRARTNTO-LOCPREL TO                                     
539500                       RAD-PRARTNTO-LOCPREL                               
539600     ELSE                                                                 
539700        MOVE RAD-PRARTNTO-LOC      TO RAD-PRARTNTO-LOCPREL                
539800        MOVE ZERO                  TO RAD-PRARTNTO-LOC                    
539900     END-IF                                                               
540000     MOVE WS-IDPRQUES              TO PRNO-IDPRQUES-IN                    
540100     MOVE +3                       TO PRNO-KDCALL                         
540200                                                                          
540300     CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                      
540400                                                                          
540500     .                                                                    
540600     EJECT                                                                
540700* IMS SEKTIONER ------------------------                                  
540800*                                                                         
540900 IMS-GET-MSG SECTION.                                                     
541000     SKIP2                                                                
541100     MOVE '  QC' TO GODK-STATUSKODER                                      
541200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
541300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
541400     PERFORM IMS-STATUSKONTROLL                                           
541500     SKIP2                                                                
541600     .                                                                    
541700 IMS-PURG-ALT2-MSG SECTION.                                               
541800     MOVE 'IMS-PURG-ALT2-MSG'    TO CURRENT-IMS-SECTION                   
541900*                                                                         
542000     MOVE LOW-VALUE TO ALT2-Z1 ALT2-Z2                                    
542100     MOVE '  '  TO GODK-STATUSKODER                                       
542200     CALL CBLTDLI USING PURG ALT2-PCB ALT2-IO-AREA                        
542300     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
542400     PERFORM IMS-STATUSKONTROLL                                           
542500     .                                                                    
542600     SKIP2                                                                
542700 IMS-INSERT-ALT42 SECTION.                                                
542800*                                                                         
542900     MOVE LOW-VALUE TO ALT-Z1 ALT-Z2                                      
543000     MOVE SPACE TO GODK-STATUSKODER                                       
543100     CALL CBLTDLI USING ISRT ALT42-PCB MSG-IO-AREA                        
543200     MOVE ALT42-STATUS-CODE TO STATUS-WS                                  
543300     PERFORM IMS-STATUSKONTROLL                                           
543400     .                                                                    
543500     SKIP2                                                                
543600 IMS-INSERT-ALT97 SECTION.                                                
543700     MOVE 'IMS-PURG-ALT2-MSG'    TO CURRENT-IMS-SECTION                   
543800*                                                                         
543900     MOVE LOW-VALUE TO ALT-Z1 ALT-Z2                                      
544000     MOVE SPACE TO GODK-STATUSKODER                                       
544100     CALL CBLTDLI USING ISRT ALT97-PCB MSG-IO-AREA                        
544200     MOVE ALT97-STATUS-CODE TO STATUS-WS                                  
544300     PERFORM IMS-STATUSKONTROLL                                           
544400     .                                                                    
544500     SKIP2                                                                
544600 IMS-GHU-WDE401    SECTION.                                               
544800     MOVE 'IMS-GHU-WDE401   '      TO CURRENT-IMS-SECTION                 
544900     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
545000            DELIMITED BY SIZE INTO SSA1                                   
545100     MOVE '    ' TO GODK-STATUSKODER                                      
545200     CALL CBLTDLI USING GHU    WDE4-PCB KORD-WDE401 SSA1                  
545300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
545400     PERFORM IMS-STATUSKONTROLL                                           
545500     SKIP2                                                                
545600     .                                                                    
545700 IMS-REPL-WDE401 SECTION.                                                 
545800     MOVE 'IMS-REPL-WDE401  '      TO CURRENT-IMS-SECTION                 
545900     MOVE '  '   TO GODK-STATUSKODER                                      
546000     CALL CBLTDLI USING REPL WDE4-PCB KORD-WDE401                         
546100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
546200     PERFORM IMS-STATUSKONTROLL                                           
546300     SKIP2                                                                
546400     .                                                                    
549000 IMS-REPL-WDE411        SECTION.                                          
549100     MOVE 'IMS-REPL-WDE411        ' TO CURRENT-IMS-SECTION                
549200     MOVE '    ' TO GODK-STATUSKODER                                      
549300     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E411                         
549400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
549500     PERFORM IMS-STATUSKONTROLL                                           
549600     .                                                                    
549700     SKIP2                                                                
549710 IMS-GHNP-WDE411 SECTION.                                                 
549720     MOVE 'IMS-GHNP-WDE411'   TO CURRENT-IMS-SECTION                      
549730                                                                          
549740     STRING 'WDE411  (IDPURAD >=' W-IDPURAD-MIN-X                         
549750                    '&IDPURAD <=' W-IDPURAD-MAX-X ')'                     
549760          DELIMITED BY SIZE INTO SSA1                                     
549770     MOVE '  GE' TO GODK-STATUSKODER                                      
549780     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E411 SSA1                    
549790     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
549791     PERFORM IMS-STATUSKONTROLL                                           
549792     .                                                                    
549793     SKIP3                                                                
549794 IMS-GHNP-WDE411-FIRST SECTION.                                           
549795     MOVE 'IMS-GHNP-WDE411-FIRST'  TO CURRENT-IMS-SECTION                 
549796                                                                          
549797     STRING 'WDE411  *F(IDPURAD >=' W-IDPURAD-MIN-X                       
549798                      '&IDPURAD <=' W-IDPURAD-MAX-X ')'                   
549799          DELIMITED BY SIZE INTO SSA1                                     
549800     MOVE '  GE' TO GODK-STATUSKODER                                      
549801     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E411 SSA1                    
549802     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
549803     PERFORM IMS-STATUSKONTROLL                                           
549804     .                                                                    
549805     SKIP3                                                                
549814 IMS-GHNP-WDE421  SECTION.                                                
549815     MOVE 'IMS-GHNP-WDE421'    TO CURRENT-IMS-SECTION                     
549816                                                                          
549817     MOVE 'WDE421' TO SSA1                                                
549818     MOVE ' GE'    TO GODK-STATUSKODER                                    
549819     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-WDE421 SSA1                  
549820     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
549821     PERFORM IMS-STATUSKONTROLL                                           
549822     SKIP3                                                                
549823     .                                                                    
549824 IMS-DLET-WDE421  SECTION.                                                
549825     MOVE 'IMS-DLET-WDE421'    TO CURRENT-IMS-SECTION                     
549826                                                                          
549827     MOVE '  GE'   TO GODK-STATUSKODER                                    
549828     CALL CBLTDLI USING DLET WDE4-PCB DLI-IO-WDE421                       
549829     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
549830     PERFORM IMS-STATUSKONTROLL                                           
549831     .                                                                    
549832     SKIP2                                                                
549840 IMS-GU-WDE411-BSEQ SECTION.                                              
549900     MOVE 'IMS-GU-WDE411-BSEQ  '   TO CURRENT-IMS-SECTION                 
550000     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
550100                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
550200            DELIMITED BY SIZE INTO SSA1                                   
550300     MOVE 'WDE401   '            TO SSA2                                  
550400     MOVE '  GE' TO GODK-STATUSKODER                                      
550500     CALL CBLTDLI USING GU   WDE42-PCB KORD-WDE401  SSA1 SSA2             
550600     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
550700     PERFORM IMS-STATUSKONTROLL                                           
550800     .                                                                    
550900     SKIP2                                                                
550910 IMS-GU-WDE401-ESEQ SECTION.                                              
550930     MOVE 'IMS-GU-WDE401-ESEQ    ' TO CURRENT-IMS-SECTION                 
550940     STRING 'WDE401  (WDE4ESEQ =' W-WDE401-X ')'                          
550950            DELIMITED BY SIZE INTO SSA1                                   
550960     MOVE '  GE' TO GODK-STATUSKODER                                      
550970     CALL CBLTDLI USING GU WDE43-PCB  E4E-WDE401-AREA SSA1                
550980     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
550990     PERFORM IMS-STATUSKONTROLL                                           
550991     .                                                                    
550992     SKIP2                                                                
550993 IMS-GN-WDE401-ESEQ SECTION.                                              
550995     MOVE 'IMS-GN-WDE401-ESEQ    ' TO CURRENT-IMS-SECTION                 
550996     STRING 'WDE401  (WDE4ESEQ =' W-WDE401-X ')'                          
550997            DELIMITED BY SIZE INTO SSA1                                   
550998     MOVE '  GEGB' TO GODK-STATUSKODER                                    
550999     CALL CBLTDLI USING GN WDE43-PCB  E4E-WDE401-AREA SSA1                
551000     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
551001     PERFORM IMS-STATUSKONTROLL                                           
551002     .                                                                    
551003     SKIP2                                                                
551004 IMS-GU-WDE4E1 SECTION.                                                   
551006     MOVE 'IMS-GU-WDE4E1       '   TO CURRENT-IMS-SECTION                 
551007     STRING 'WDE4E1  (WDE4E1KY>=' W-WDE4E1KY-MIN-X                        
551008                    '&WDE4E1KY<=' W-WDE4E1KY-MAX-X ')'                    
551009          DELIMITED BY SIZE INTO SSA1                                     
551010     MOVE '  ' TO GODK-STATUSKODER                                        
551011     CALL CBLTDLI USING GU WDE4E-PCB WDE4E1-AREA SSA1                     
551012     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
551013     PERFORM IMS-STATUSKONTROLL                                           
551014     SKIP2                                                                
551015     .                                                                    
551017 IMS-GU-WDE401-ASEQ SECTION.                                              
551019     MOVE 'IMS-GU-WDE401-ASEQ    ' TO CURRENT-IMS-SECTION                 
551020     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
551021            DELIMITED BY SIZE INTO SSA1                                   
551022     MOVE '  GE'                TO GODK-STATUSKODER                       
551023     CALL CBLTDLI USING GU WDE4A-PCB KORD-WDE401  SSA1                    
551024     MOVE WDE4A-STATUS-CODE     TO STATUS-WS                              
551025     PERFORM IMS-STATUSKONTROLL                                           
551026     .                                                                    
551027     SKIP2                                                                
551028 IMS-GN-WDE401-ASEQ SECTION.                                              
551030     MOVE 'IMS-GN-WDE401-ASEQ    ' TO CURRENT-IMS-SECTION                 
551031     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
551032            DELIMITED BY SIZE INTO SSA1                                   
551033     MOVE '  GEGB'              TO GODK-STATUSKODER                       
551034     CALL CBLTDLI USING GN WDE4A-PCB KORD-WDE401  SSA1                    
551035     MOVE WDE4A-STATUS-CODE     TO STATUS-WS                              
551036     PERFORM IMS-STATUSKONTROLL                                           
551037     .                                                                    
551038     SKIP2                                                                
551039 IMS-GU-WDE4F1-PLK SECTION.                                               
551040     MOVE 'IMS-GU-WDE4F1-PLK' TO CURRENT-IMS-SECTION                      
551041                                                                          
551042     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
551043                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X                        
551044                    '&IDPLKLST =' W-IDPLKLST-X ')'                        
551045          DELIMITED BY SIZE INTO SSA1                                     
551046     MOVE '  GE' TO GODK-STATUSKODER                                      
551047     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-WDE4F1 SSA1                   
551048     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
551049     PERFORM IMS-STATUSKONTROLL                                           
551050     .                                                                    
551051     SKIP3                                                                
551052 IMS-GU-WDE601 SECTION.                                                   
551053     MOVE 'IMS-GU-WDE601       '   TO CURRENT-IMS-SECTION                 
551054     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
551055            DELIMITED BY SIZE INTO SSA1                                   
551056     MOVE '    ' TO GODK-STATUSKODER                                      
551057     CALL CBLTDLI USING GU     WDE6-PCB DLI-IO-E601 SSA1                  
551058     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
551059     PERFORM IMS-STATUSKONTROLL                                           
551060     SKIP2                                                                
551070     .                                                                    
551080 IMS-GHU-WDE601 SECTION.                                                  
551090     MOVE 'IMS-GHU-WDE601      '   TO CURRENT-IMS-SECTION                 
551100     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
551101            DELIMITED BY SIZE INTO SSA1                                   
551102     MOVE '  GE' TO GODK-STATUSKODER                                      
551103     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-E601 SSA1                  
551104     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
551105     PERFORM IMS-STATUSKONTROLL                                           
551106     SKIP2                                                                
551107     .                                                                    
551108 IMS-REPL-WDE601   SECTION.                                               
551109     MOVE 'IMS-REPL-WDE601     '   TO CURRENT-IMS-SECTION                 
551110     MOVE '    ' TO GODK-STATUSKODER                                      
551111     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
551112     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
551113     PERFORM IMS-STATUSKONTROLL                                           
551114     SKIP2                                                                
551115     .                                                                    
551116 IMS-GU-WDE62-VORD SECTION.                                               
551117     MOVE 'IMS-GU-WDE62-VORD     ' TO CURRENT-IMS-SECTION                 
551118     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
551119            DELIMITED BY SIZE INTO SSA1                                   
551120     MOVE '  ' TO GODK-STATUSKODER                                        
551121     CALL CBLTDLI USING GU WDE62-PCB DLI-IO-AREA12 SSA1                   
551122     MOVE WDE62-STATUS-CODE TO STATUS-WS                                  
551123     PERFORM IMS-STATUSKONTROLL                                           
551124     .                                                                    
551125     SKIP3                                                                
551126 IMS-DLET-WDE611    SECTION.                                              
551127     MOVE 'IMS-DLET-WDE611'  TO CURRENT-IMS-SECTION                       
551128     MOVE '    ' TO GODK-STATUSKODER                                      
551129     CALL CBLTDLI USING DLET WDE6-PCB DLI-IO-E611                         
551130     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
551131     PERFORM IMS-STATUSKONTROLL                                           
551132     .                                                                    
551133     SKIP3                                                                
551134 IMS-GHU-WDE611-DEF SECTION.                                              
551135     MOVE 'IMS-GHU-WDE611-DEF'   TO CURRENT-IMS-SECTION                   
551136     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
551137          DELIMITED BY SIZE INTO SSA1                                     
551138     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-E6-X                          
551139                    '&KDKOLSTA =' W-KDKOLSTA-X ')'                        
551140          DELIMITED BY SIZE INTO SSA2                                     
551141     MOVE '  GE' TO GODK-STATUSKODER                                      
551142     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E611 SSA1 SSA2                
551143     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
551144     PERFORM IMS-STATUSKONTROLL                                           
551145     .                                                                    
551146     SKIP3                                                                
551147 IMS-GHN-ORQA01   SECTION.                                                
551150     MOVE 'IMS-GHN-ORQA01      '   TO CURRENT-IMS-SECTION                 
551200     STRING 'WLORQA01(WDQ301KY =' W-WDQ301-KEY-X ')'                      
551300            DELIMITED BY SIZE INTO SSA1                                   
551400     MOVE '  ' TO GODK-STATUSKODER                                        
551500     CALL CBLTDLI USING GHN    ORQA-PCB ODEL-WDQ301 SSA1                  
551600     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
551700     PERFORM IMS-STATUSKONTROLL                                           
551800     .                                                                    
551900 IMS-GHU-ORQA01   SECTION.                                                
552000     MOVE 'IMS-GHU-ORQA01      '   TO CURRENT-IMS-SECTION                 
552100     STRING 'WLORQA01(WDQ301KY =' W-WDQ301-KEY-X ')'                      
552200            DELIMITED BY SIZE INTO SSA1                                   
552300     MOVE '  ' TO GODK-STATUSKODER                                        
552400     CALL CBLTDLI USING GHU    ORQA-PCB ODEL-WDQ301 SSA1                  
552500     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
552600     PERFORM IMS-STATUSKONTROLL                                           
552700     .                                                                    
552800 IMS-GU-ORQA-STATUS    SECTION.                                           
552900                                                                          
553000     MOVE 'IMS-GU-ORQA-STATUS  '   TO CURRENT-IMS-SECTION                 
553100     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN                          
553200                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
553300                    '&KDODELST =' W-KDODELST     ')'                      
553400          DELIMITED BY SIZE INTO SSA1                                     
553500     MOVE '  GE' TO GODK-STATUSKODER                                      
553600     CALL CBLTDLI USING GU  ORQA-PCB ODEL-WDQ301 SSA1                     
553700     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
553800     PERFORM IMS-STATUSKONTROLL                                           
553900     .                                                                    
554000                                                                          
554100 IMS-REPL-ORQA01        SECTION.                                          
554200                                                                          
554300     MOVE 'IMS-REPL-ORQA01     '   TO CURRENT-IMS-SECTION                 
554400     MOVE '    ' TO GODK-STATUSKODER                                      
554500     CALL CBLTDLI USING REPL ORQA-PCB ODEL-WDQ301                         
554600     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
554700     PERFORM IMS-STATUSKONTROLL                                           
554800     .                                                                    
554900     SKIP2                                                                
555000 IMS-GU-ORQM01 SECTION.                                                   
555100                                                                          
555200     MOVE 'IMS-GU-ORQM01       '   TO CURRENT-IMS-SECTION                 
555300     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
555400                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
555500          DELIMITED BY SIZE INTO SSA1                                     
555600     MOVE '  GE'               TO GODK-STATUSKODER                        
555700     CALL CBLTDLI USING GU   ORQM-PCB OBKR-WDQ101 SSA1                    
555800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
555900     PERFORM IMS-STATUSKONTROLL                                           
556000     .                                                                    
556100     SKIP2                                                                
556200 IMS-ISRT-ORQM01     SECTION.                                             
556300                                                                          
556400     MOVE 'IMS-ISRT-ORQM01     '   TO CURRENT-IMS-SECTION                 
556500     MOVE 'WLORQM01 ' TO SSA1                                             
556600     MOVE '  '   TO GODK-STATUSKODER                                      
556700     CALL CBLTDLI USING ISRT ORQM-PCB OBKR-WDQ101 SSA1                    
556800     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
556900     PERFORM IMS-STATUSKONTROLL                                           
557000     .                                                                    
557100     SKIP2                                                                
561550 IMS-GU-XXDJ-ROT  SECTION.                                                
561700     MOVE 'IMS-GU-XXDJ-ROT     '   TO CURRENT-IMS-SECTION                 
561800     SKIP2                                                                
561900     STRING 'WLXXDJ01(WDGXKEY  =' W-4305-X ')'                            
562000            DELIMITED BY SIZE INTO SSA1                                   
562100     MOVE '  GE' TO GODK-STATUSKODER                                      
562200     CALL CBLTDLI USING GU XXDJ-PCB DLI-IO-DJ01 SSA1                      
562300     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
562400     PERFORM IMS-STATUSKONTROLL                                           
562500     SKIP2                                                                
562600     .                                                                    
562700 IMS-GHNP-XXDJ-LASNING SECTION.                                           
562800                                                                          
562900     MOVE 'IMS-GHNP-XXDJ-LASNING'  TO CURRENT-IMS-SECTION                 
563000     SKIP2                                                                
563100     STRING 'WLXXDJ11(WDGXKEY  =' W-4306-X ')'                            
563200            DELIMITED BY SIZE INTO SSA1                                   
563300     MOVE '  GE' TO GODK-STATUSKODER                                      
563400     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-DJ11 SSA1                    
563500     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
563600     PERFORM IMS-STATUSKONTROLL                                           
563700                                                                          
563800     .                                                                    
563900 IMS-DLET-XXDJ-LASNING SECTION.                                           
564000                                                                          
564100     MOVE 'IMS-DLET-XXDJ-LASNING'  TO CURRENT-IMS-SECTION                 
564200     SKIP2                                                                
564300     MOVE '  ' TO GODK-STATUSKODER                                        
564400     CALL CBLTDLI USING DLET XXDJ-PCB DLI-IO-DJ11                         
564500     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
564600     PERFORM IMS-STATUSKONTROLL                                           
564700     .                                                                    
564800     SKIP2                                                                
564900 IMS-GET-XXDK-ROT     SECTION.                                            
565000                                                                          
565100     MOVE 'IMS-GET-XXDK-ROT     '  TO CURRENT-IMS-SECTION                 
565200     STRING 'WLXXDK01(WDGXKEY  =' W-4311-X ')'                            
565300            DELIMITED BY SIZE INTO SSA1                                   
565400     MOVE '  GE' TO GODK-STATUSKODER                                      
565500     CALL CBLTDLI USING GU XXDK-PCB DLI-IO-AREA2 SSA1                     
565600     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
565700     PERFORM IMS-STATUSKONTROLL                                           
565800     .                                                                    
565900                                                                          
566000 IMS-GET-XXDK-4312    SECTION.                                            
566100                                                                          
566200     MOVE 'IMS-GET-XXDK-4312    '  TO CURRENT-IMS-SECTION                 
566300     STRING 'WLXXDK11(WDGXKEY  =' W-4312-X ')'                            
566400            DELIMITED BY SIZE INTO SSA1                                   
566500     MOVE '  GE' TO GODK-STATUSKODER                                      
566600     CALL CBLTDLI USING GHU XXDK-PCB DLI-IO-AREA2 SSA1                    
566700     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
566800     PERFORM IMS-STATUSKONTROLL                                           
566900     .                                                                    
567000                                                                          
567100 IMS-DLET-XXDK         SECTION.                                           
567200                                                                          
567300     MOVE 'IMS-DLET-XXDK        '  TO CURRENT-IMS-SECTION                 
567400     MOVE '  ' TO GODK-STATUSKODER                                        
567500     CALL CBLTDLI USING DLET XXDK-PCB DLI-IO-AREA2                        
567600     MOVE XXDK-STATUS-CODE TO STATUS-WS                                   
567700     PERFORM IMS-STATUSKONTROLL                                           
567800     .                                                                    
567900     SKIP2                                                                
568000 IMS-GU-XXDL-ROT     SECTION.                                             
568100                                                                          
568200     MOVE 'IMS-GU-XXDL-ROT      '  TO CURRENT-IMS-SECTION                 
568300     STRING 'WLXXDL01(WDGXKEY  =' W-4315-X ')'                            
568400            DELIMITED BY SIZE INTO SSA1                                   
568500     MOVE '  ' TO GODK-STATUSKODER                                        
568600     CALL CBLTDLI USING GU XXDL-PCB DLI-IO-DL01 SSA1                      
568700     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
568800     PERFORM IMS-STATUSKONTROLL                                           
568900     .                                                                    
569000                                                                          
569100 IMS-GET-XXDL-4316-INTERV  SECTION.                                       
569200                                                                          
569300     MOVE 'IMS-GET-XXDL-4316-INTE' TO CURRENT-IMS-SECTION                 
569400     STRING 'WLXXDL11(WDGXKEY >=' W-4316-X-MIN                            
569500                    '&WDGXKEY <=' W-4316-X-MAX ')'                        
569600            DELIMITED BY SIZE INTO SSA1                                   
569700     MOVE '  GE' TO GODK-STATUSKODER                                      
569800     CALL CBLTDLI USING GHNP XXDL-PCB DLI-IO-AREA2 SSA1                   
569900     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
570000     PERFORM IMS-STATUSKONTROLL                                           
570100     .                                                                    
570200                                                                          
570300 IMS-DLET-XXDL         SECTION.                                           
570400                                                                          
570500     MOVE 'IMS-DLET-XXDL         ' TO CURRENT-IMS-SECTION                 
570600     MOVE '  ' TO GODK-STATUSKODER                                        
570700     CALL CBLTDLI USING DLET XXDL-PCB DLI-IO-AREA2                        
570800     MOVE XXDL-STATUS-CODE TO STATUS-WS                                   
570900     PERFORM IMS-STATUSKONTROLL                                           
571000     .                                                                    
571100     SKIP2                                                                
571200 IMS-GU-ARTC11 SECTION.                                                   
571300                                                                          
571400     MOVE 'IMS-GU-ARTC11         ' TO CURRENT-IMS-SECTION                 
571500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
571600            DELIMITED BY SIZE INTO SSA1                                   
571700     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
571800          DELIMITED BY SIZE INTO SSA2                                     
571900     MOVE '  '     TO GODK-STATUSKODER                                    
572000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA8 SSA1 SSA2                
572100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
572200     PERFORM IMS-STATUSKONTROLL                                           
572300     SKIP3                                                                
572400     .                                                                    
572500 IMS-GHU-ARTC11     SECTION.                                              
572600                                                                          
572700     MOVE 'IMS-GHU-ARTC11        ' TO CURRENT-IMS-SECTION                 
572800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
572900            DELIMITED BY SIZE INTO SSA1                                   
573000     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
573100          DELIMITED BY SIZE INTO SSA2                                     
573200     MOVE '  '   TO GODK-STATUSKODER                                      
573300     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA8 SSA1 SSA2               
573400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
573500     PERFORM IMS-STATUSKONTROLL                                           
573600     SKIP2                                                                
573700     .                                                                    
573800 IMS-REPL-ARTC       SECTION.                                             
573900                                                                          
574000     MOVE 'IMS-REPL-ARTC         ' TO CURRENT-IMS-SECTION                 
574100     MOVE '  '   TO GODK-STATUSKODER                                      
574200     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA8                        
574300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
574400     PERFORM IMS-STATUSKONTROLL                                           
574500     .                                                                    
574600     SKIP2                                                                
574700 IMS-GHU-WDK711     SECTION.                                              
574800                                                                          
574900     MOVE 'IMS-GHU-WDK711        ' TO CURRENT-IMS-SECTION                 
575000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
575100            DELIMITED BY SIZE INTO SSA1                                   
575200     STRING 'WDK711  (IDDC     =' W-WDK711-IDDC-X ')'                     
575300            DELIMITED BY SIZE INTO SSA2                                   
575400     MOVE '  GE'   TO GODK-STATUSKODER                                    
575500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA15 SSA1 SSA2              
575600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
575700     PERFORM IMS-STATUSKONTROLL                                           
575800     SKIP2                                                                
575900     .                                                                    
576000 IMS-GU-WDK711        SECTION.                                            
576100                                                                          
576200     MOVE 'IMS-GU-WDK711         ' TO CURRENT-IMS-SECTION                 
576300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
576400            DELIMITED BY SIZE INTO SSA1                                   
576500     STRING 'WDK711  (IDDC     =' W-WDK711-IDDC-X ')'                     
576600            DELIMITED BY SIZE INTO SSA2                                   
576700     MOVE '  GE' TO GODK-STATUSKODER                                      
576800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA15 SSA1 SSA2               
576900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
577000     PERFORM IMS-STATUSKONTROLL                                           
577100     .                                                                    
577200     EJECT                                                                
577300 IMS-GU-WDK722        SECTION.                                            
577400                                                                          
577500     MOVE 'IMS-GU-WDK722         ' TO CURRENT-IMS-SECTION                 
577600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
577700            DELIMITED BY SIZE INTO SSA1                                   
577800     STRING 'WDK711  (IDDC     =' W-WDK711-IDDC-X ')'                     
577900            DELIMITED BY SIZE INTO SSA2                                   
578000     MOVE 'WDK722 '             TO SSA3                                   
578100     MOVE '  GE' TO GODK-STATUSKODER                                      
578200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
578300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
578400     PERFORM IMS-STATUSKONTROLL                                           
578500     .                                                                    
578600     EJECT                                                                
578700 IMS-GU-WDK712 SECTION.                                                   
578800                                                                          
578900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
579000          DELIMITED BY SIZE INTO SSA1                                     
579100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
579200          DELIMITED BY SIZE INTO SSA2                                     
579300     MOVE '  GE' TO GODK-STATUSKODER                                      
579400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
579500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
579600     PERFORM IMS-STATUSKONTROLL                                           
579700     .                                                                    
579800     EJECT                                                                
579900 IMS-REPL-WDK7       SECTION.                                             
580000                                                                          
580100     MOVE 'IMS-REPL-WDK7         ' TO CURRENT-IMS-SECTION                 
580200     MOVE '  '   TO GODK-STATUSKODER                                      
580300     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA15                       
580400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
580500     PERFORM IMS-STATUSKONTROLL                                           
580600     .                                                                    
580700     SKIP2                                                                
580800 IMS-GHU-WDK901 SECTION.                                                  
580900                                                                          
581000     MOVE 'IMS-GHU-WDK901        ' TO CURRENT-IMS-SECTION                 
581100     STRING 'WLARTM01(IDARTNR  =' W-WDK901-IDARTNR-X ')'                  
581200            DELIMITED BY SIZE INTO SSA1                                   
581300     MOVE '  '     TO GODK-STATUSKODER                                    
581400     CALL CBLTDLI USING GHU ARTM-PCB ART-WDK901 SSA1                      
581500     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
581600     PERFORM IMS-STATUSKONTROLL                                           
581700     SKIP3                                                                
581800     .                                                                    
581900 IMS-REPL-WDK901 SECTION.                                                 
582000                                                                          
582100     MOVE 'IMS-REPL-WDK901       ' TO CURRENT-IMS-SECTION                 
582200     MOVE '  '     TO GODK-STATUSKODER                                    
582300     CALL CBLTDLI USING REPL ARTM-PCB ART-WDK901                          
582400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
582500     PERFORM IMS-STATUSKONTROLL                                           
582600     SKIP3                                                                
582700     .                                                                    
582800 IMS-ISRT-AVVIKELSE  SECTION.                                             
582900                                                                          
583000     MOVE 'IMS-ISRT-AVVIKELSE    ' TO CURRENT-IMS-SECTION                 
583100     MOVE 'WLZZAC01'  TO SSA1                                             
583200     MOVE '  II' TO GODK-STATUSKODER                                      
583300     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA4 SSA1                   
583400     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
583500     PERFORM IMS-STATUSKONTROLL                                           
583600     SKIP2                                                                
583700     .                                                                    
583800 IMS-ISRT-KLAR-SV4   SECTION.                                             
583900                                                                          
584000     MOVE 'IMS-ISRT-KLAR-SV4     ' TO CURRENT-IMS-SECTION                 
584100     MOVE 'WLZZAC01'  TO SSA1                                             
584200     MOVE '  II' TO GODK-STATUSKODER                                      
584300     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA4 SSA1                   
584400     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
584500     PERFORM IMS-STATUSKONTROLL                                           
584600     SKIP2                                                                
584700     .                                                                    
584800 IMS-ISRT-AUTFAKTURA-ROT SECTION.                                         
584900                                                                          
585000     MOVE 'IMS-ISRT-AUTFAKTURA-RO' TO CURRENT-IMS-SECTION                 
585100     STRING 'WLXXDV01(WDGXKEY  =' W-4726-X ')'                            
585200            DELIMITED BY SIZE INTO SSA1                                   
585300     MOVE   'WLXXDV11 '         TO SSA2                                   
585400     MOVE '  '     TO GODK-STATUSKODER                                    
585500     CALL CBLTDLI USING ISRT AUTF-PCB DLI-IO-AREA3 SSA1 SSA2              
585600     MOVE AUTF-STATUS-CODE TO STATUS-WS                                   
585700     PERFORM IMS-STATUSKONTROLL                                           
585800     SKIP2                                                                
585900     .                                                                    
586000 IMS-ISRT-AUTFAKTURA     SECTION.                                         
586100                                                                          
586200     MOVE 'IMS-ISRT-AUTFAKTURA   ' TO CURRENT-IMS-SECTION                 
586300     STRING 'WLXXDV01(WDGXKEY  =' W-4726-X ')'                            
586400            DELIMITED BY SIZE INTO SSA1                                   
586500     STRING 'WLXXDV11(WDGXKEY  =' W-WDGX11-WDGXKEY-X ')'                  
586600            DELIMITED BY SIZE INTO SSA2                                   
586700     MOVE 'WLXXDV21 ' TO SSA3                                             
586800     MOVE '  IIGE' TO GODK-STATUSKODER                                    
586900     CALL CBLTDLI USING ISRT AUTF-PCB DLI-IO-AREA3                        
587000                                        SSA1 SSA2 SSA3                    
587100     MOVE AUTF-STATUS-CODE TO STATUS-WS                                   
587200     PERFORM IMS-STATUSKONTROLL                                           
587300     SKIP2                                                                
587400     .                                                                    
587500 IMS-GHU-ORDP01   SECTION.                                                
587600                                                                          
587700     MOVE 'IMS-GHU-ORDP01        ' TO CURRENT-IMS-SECTION                 
587800     STRING 'WLORDP01(WDA501KY =' W1-WDA501KY-X ')'                       
587900            DELIMITED BY SIZE INTO SSA1                                   
588000     MOVE '  '     TO GODK-STATUSKODER                                    
588100     CALL CBLTDLI USING GHU ORDP1-PCB DLI-IO-AREA6 SSA1                   
588200     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
588300     PERFORM IMS-STATUSKONTROLL                                           
588400     SKIP3                                                                
588500     .                                                                    
588600 IMS-GHU-ORDP01-GE           SECTION.                                     
588700                                                                          
588800     MOVE 'IMS-GHU-ORDP01-GE     ' TO CURRENT-IMS-SECTION                 
588900     STRING 'WLORDP01(WDA501KY =' W1-WDA501KY-X ')'                       
589000            DELIMITED BY SIZE INTO SSA1                                   
589100     MOVE '  GE'   TO GODK-STATUSKODER                                    
589200     CALL CBLTDLI USING GHU ORDP1-PCB DLI-IO-AREA6 SSA1                   
589300     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
589400     PERFORM IMS-STATUSKONTROLL                                           
589500     SKIP3                                                                
589600     .                                                                    
589700 IMS-REPL-ORDP01  SECTION.                                                
589800                                                                          
589900     MOVE 'IMS-REPL-ORDP01       ' TO CURRENT-IMS-SECTION                 
590000     MOVE '  '     TO GODK-STATUSKODER                                    
590100     CALL CBLTDLI USING REPL ORDP1-PCB DLI-IO-AREA6                       
590200     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
590300     PERFORM IMS-STATUSKONTROLL                                           
590400     SKIP3                                                                
590500     .                                                                    
590600 IMS-ISRT-ORDP01  SECTION.                                                
590700                                                                          
590800     MOVE 'IMS-ISRT-ORDP01       ' TO CURRENT-IMS-SECTION                 
590900     MOVE 'WLORDP01 ' TO SSA1                                             
591000     MOVE '  II'   TO GODK-STATUSKODER                                    
591100     CALL CBLTDLI USING ISRT ORDP1-PCB DLI-IO-AREA6 SSA1                  
591200     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
591300     PERFORM IMS-STATUSKONTROLL                                           
591400     SKIP3                                                                
591500     .                                                                    
591600*IMS-ISRT-ORDP01-OLD  SECTION.                                            
591700*                                                                         
591800*    MOVE 'WLORDP01 ' TO SSA1                                             
591900*    MOVE '  II'   TO GODK-STATUSKODER                                    
592000*    CALL CBLTDLI USING ISRT ORDP2-PCB DLI-IO-AREA7 SSA1                  
592100*    MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
592200*    PERFORM IMS-STATUSKONTROLL                                           
592300*    SKIP3                                                                
592400*    .                                                                    
592500 IMS-DLET-ORDP01  SECTION.                                                
592600                                                                          
592700     MOVE 'IMS-DLET-ORDP01       ' TO CURRENT-IMS-SECTION                 
592800     MOVE '  '     TO GODK-STATUSKODER                                    
592900     CALL CBLTDLI USING DLET ORDP1-PCB DLI-IO-AREA6                       
593000     MOVE ORDP1-STATUS-CODE TO STATUS-WS                                  
593100     PERFORM IMS-STATUSKONTROLL                                           
593200     SKIP2                                                                
593300     .                                                                    
593400 IMS-GHU-ORDP01-OLD SECTION.                                              
593500                                                                          
593600     MOVE 'IMS-GHU-ORDP01-OLD    ' TO CURRENT-IMS-SECTION                 
593700     STRING 'WLORDP01(WDA501KY=>' W1-WDA501KY-X                           
593800                    '&WDA501KY=<' W2-WDA501KY-X                           
593900                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
594000            DELIMITED BY SIZE INTO SSA1                                   
594100     MOVE '  GE'   TO GODK-STATUSKODER                                    
594200     CALL CBLTDLI USING GHU ORDP2-PCB DLI-IO-AREA7 SSA1                   
594300     MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
594400     PERFORM IMS-STATUSKONTROLL                                           
594500     SKIP3                                                                
594600     .                                                                    
594700 IMS-GHN-ORDP01-OLD SECTION.                                              
594800                                                                          
594900     MOVE 'IMS-GHN-ORDP01-OLD    ' TO CURRENT-IMS-SECTION                 
595000     STRING 'WLORDP01(WDA501KY=>' W1-WDA501KY-X                           
595100                    '&WDA501KY=<' W2-WDA501KY-X                           
595200                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
595300            DELIMITED BY SIZE INTO SSA1                                   
595400     MOVE '  GE'   TO GODK-STATUSKODER                                    
595500     CALL CBLTDLI USING GHN ORDP2-PCB DLI-IO-AREA7 SSA1                   
595600     MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
595700     PERFORM IMS-STATUSKONTROLL                                           
595800     SKIP3                                                                
595900     .                                                                    
596000 IMS-REPL-ORDP01-OLD  SECTION.                                            
596100                                                                          
596200     MOVE 'IMS-REPL-ORDP01-OLD   ' TO CURRENT-IMS-SECTION                 
596300     MOVE '  '     TO GODK-STATUSKODER                                    
596400     CALL CBLTDLI USING REPL ORDP2-PCB DLI-IO-AREA7                       
596500     MOVE ORDP2-STATUS-CODE TO STATUS-WS                                  
596600     PERFORM IMS-STATUSKONTROLL                                           
596700     SKIP2                                                                
596800     .                                                                    
596900 IMS-GU-XXJN     SECTION.                                                 
597000                                                                          
597100     MOVE 'IMS-GU-XXJN           ' TO CURRENT-IMS-SECTION                 
597200     STRING 'WLXXJN01(WDGXKEY  =' W-WDGX01 ')'                            
597300            DELIMITED BY SIZE INTO SSA1                                   
597400     STRING 'WLXXJN11(WDGXKEY >=' W-WDGXKEY-N5-MIN                        
597500                    '&WDGXKEY <=' W-WDGXKEY-N5-MAX                        
597600                    '&KDRAPRIO>=' W-KDRAPRIO-N5-MIN-X                     
597700                    '&KDRAPRIO<=' W-KDRAPRIO-N5-MAX-X                     
597800                    '&KDTPOTYP =' W-KDTPOTYP-N5-X                         
597900                    '&KDORDKL  =' W-KDORDKL-N5-X                          
598000                    '&IDDISTRF<=' W-IDDISTR-FOM-N5-X                      
598100                    '&IDDISTRT>=' W-IDDISTR-TOM-N5-X ')'                  
598200            DELIMITED BY SIZE INTO SSA2                                   
598300     MOVE '  ' TO GODK-STATUSKODER                                        
598400     CALL CBLTDLI USING GU XXJN-PCB DLI-IO-AREA5 SSA1 SSA2                
598500     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
598600     PERFORM IMS-STATUSKONTROLL                                           
598700     SKIP2                                                                
598800     .                                                                    
598900 IMS-4319-ISRT-ROT SECTION.                                               
599000                                                                          
599100     MOVE 'IMS-4319-ISRT-ROT     ' TO CURRENT-IMS-SECTION                 
599200     MOVE 'WLXXJD01 ' TO SSA1                                             
599300     MOVE '  ' TO GODK-STATUSKODER                                        
599400     CALL CBLTDLI USING ISRT XXJD-PCB 4319-WDGX4319 SSA1                  
599500     MOVE XXJD-STATUS-CODE TO STATUS-WS                                   
599600     PERFORM IMS-STATUSKONTROLL                                           
599700     .                                                                    
599800     SKIP2                                                                
599900                                                                          
600000 IMS-4320-ISRT-BARN SECTION.                                              
600100                                                                          
600200     MOVE 'IMS-4320-ISRT-BARN    ' TO CURRENT-IMS-SECTION                 
600300     STRING 'WLXXJD01(WDGXKEY  =' W-4319-X ')'                            
600400         DELIMITED BY SIZE INTO SSA1                                      
600500     MOVE 'WLXXJD11 ' TO SSA2                                             
600600     MOVE '  GEII' TO GODK-STATUSKODER                                    
600700     CALL CBLTDLI USING ISRT XXJD-PCB 4320-WDGX4320 SSA1 SSA2             
600800     MOVE XXJD-STATUS-CODE TO STATUS-WS                                   
600900     PERFORM IMS-STATUSKONTROLL                                           
601000     .                                                                    
601100     SKIP2                                                                
601200                                                                          
601300 IMS-GHU-4320-BARN SECTION.                                               
601400                                                                          
601500     MOVE 'IMS-GHU-4320-BARN     ' TO CURRENT-IMS-SECTION                 
601600     STRING 'WLXXJD01(WDGXKEY  =' W-4319-X ')'                            
601700         DELIMITED BY SIZE INTO SSA1                                      
601800     STRING 'WLXXJD11(WDGXKEY  =' W-4320-X ')'                            
601900         DELIMITED BY SIZE INTO SSA2                                      
602000     MOVE '  ' TO GODK-STATUSKODER                                        
602100     CALL CBLTDLI USING GHU XXJD-PCB 4320-WDGX4320 SSA1 SSA2              
602200     MOVE XXJD-STATUS-CODE TO STATUS-WS                                   
602300     PERFORM IMS-STATUSKONTROLL                                           
602400     .                                                                    
602500     SKIP2                                                                
602600 IMS-REPL-4320-BARN SECTION.                                              
602700                                                                          
602800     MOVE 'IMS-REPL-4320-BARN    ' TO CURRENT-IMS-SECTION                 
602900     MOVE '  ' TO GODK-STATUSKODER                                        
603000     CALL CBLTDLI USING REPL XXJD-PCB 4320-WDGX4320                       
603100     MOVE XXJD-STATUS-CODE TO STATUS-WS                                   
603200     PERFORM IMS-STATUSKONTROLL                                           
603300     .                                                                    
603400     SKIP2                                                                
603500 IMS-ISRT-4542 SECTION.                                                   
603600                                                                          
603700     MOVE 'IMS-ISRT-4542         ' TO CURRENT-IMS-SECTION                 
603800     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4541-X ')'                    
603900         DELIMITED BY SIZE INTO SSA1                                      
604000     MOVE 'WDGX4542 ' TO SSA2                                             
604100     MOVE '  II' TO GODK-STATUSKODER                                      
604200     CALL CBLTDLI USING ISRT 4541-PCB 4542-WDGX4542 SSA1 SSA2             
604300     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
604400     PERFORM IMS-STATUSKONTROLL                                           
604500     .                                                                    
604600     SKIP2                                                                
604700 IMS-GHU-WDGX4490 SECTION.                                                
604800                                                                          
604900     MOVE 'IMS-GHU-WDGX4490      ' TO CURRENT-IMS-SECTION                 
605000     STRING 'WDR401  (WDGXKEY  =' W-4487-X ')'                            
605100         DELIMITED BY SIZE INTO SSA1                                      
605200     STRING 'WDGX4488(KDPRCGRP =' W-4488-X ')'                            
605300         DELIMITED BY SIZE INTO SSA2                                      
605400     STRING 'WDGX4490(KY4490   =' W-4490-X ')'                            
605500         DELIMITED BY SIZE INTO SSA3                                      
605600     MOVE '  GE' TO GODK-STATUSKODER                                      
605700     CALL CBLTDLI USING GHU 4487-PCB DLI-IO-AREA11 SSA1 SSA2 SSA3         
605800     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
605900     PERFORM IMS-STATUSKONTROLL                                           
606000     .                                                                    
606100     SKIP2                                                                
606200 IMS-DLET-WDGX4490 SECTION.                                               
606300                                                                          
606400     MOVE 'IMS-DLET-WDGX4490     ' TO CURRENT-IMS-SECTION                 
606500     MOVE '  ' TO GODK-STATUSKODER                                        
606600     CALL CBLTDLI USING DLET 4487-PCB DLI-IO-AREA11                       
606700     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
606800     PERFORM IMS-STATUSKONTROLL                                           
606900     .                                                                    
607000     SKIP2                                                                
610400 IMS-GU-ORQA2-ODEL SECTION.                                               
610500                                                                          
610600     MOVE 'IMS-GU-ORQA2-ODEL     ' TO CURRENT-IMS-SECTION                 
610700     STRING 'WLORQA01(WDQ301KY >' W-Q301-KEY-MIN-X                        
610800                    '&WDQ301KY <' W-Q301-KEY-MAX-X ')'                    
610900            DELIMITED BY SIZE INTO SSA1                                   
611000     MOVE '  GE' TO GODK-STATUSKODER                                      
611100     CALL CBLTDLI USING GU ORQA2-PCB DLI-IO-AREA12 SSA1                   
611200     MOVE ORQA2-STATUS-CODE TO STATUS-WS                                  
611300     PERFORM IMS-STATUSKONTROLL                                           
611400     .                                                                    
611500     SKIP3                                                                
611600 IMS-GN-ORQA2-ODEL SECTION.                                               
611700                                                                          
611800     MOVE 'IMS-GN-ORQA2-ODEL     ' TO CURRENT-IMS-SECTION                 
611900     STRING 'WLORQA01(WDQ301KY >' W-Q301-KEY-MIN-X                        
612000                    '&WDQ301KY <' W-Q301-KEY-MAX-X ')'                    
612100            DELIMITED BY SIZE INTO SSA1                                   
612200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
612300     CALL CBLTDLI USING GN ORQA2-PCB DLI-IO-AREA12 SSA1                   
612400     MOVE ORQA2-STATUS-CODE TO STATUS-WS                                  
612500     PERFORM IMS-STATUSKONTROLL                                           
612600     .                                                                    
612700     SKIP2                                                                
612800 IMS-GHU-XXKW11       SECTION.                                            
612900                                                                          
613000     MOVE 'IMS-GHU-XXKW11        ' TO CURRENT-IMS-SECTION                 
613100     STRING 'WLXXKW01(WDGXKEY  =' W-WDGXKEY-4471-X ')'                    
613200            DELIMITED BY SIZE INTO SSA1                                   
613300     STRING 'WLXXKW11(KDSEGKEY =' W-KDSEGKEY-4472-X ')'                   
613400            DELIMITED BY SIZE INTO SSA2                                   
613500     MOVE '  GE' TO GODK-STATUSKODER                                      
613600     CALL CBLTDLI USING GHU    XXKW-PCB DLI-IO-AREA13 SSA1 SSA2           
613700     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
613800     PERFORM IMS-STATUSKONTROLL                                           
613900     .                                                                    
614000 IMS-REPL-XXKW11    SECTION.                                              
614100                                                                          
614200     MOVE 'IMS-REPL-XXKW11       ' TO CURRENT-IMS-SECTION                 
614300     MOVE '  '   TO GODK-STATUSKODER                                      
614400     CALL CBLTDLI USING REPL XXKW-PCB DLI-IO-AREA13                       
614500     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
614600     PERFORM IMS-STATUSKONTROLL                                           
614700     .                                                                    
614800     SKIP2                                                                
614900 IMS-GU-XXLB         SECTION.                                             
615000                                                                          
615100     MOVE 'IMS-GU-XXLB           ' TO CURRENT-IMS-SECTION                 
615200     STRING 'WLXXLB01(WDGXKEY  =' W-WDGXKEY-4477-X ')'                    
615300            DELIMITED BY SIZE INTO SSA1                                   
615400     STRING 'WLXXLB11(WDGXKEY  =' W-WDGXKEY-4478-X ')'                    
615500            DELIMITED BY SIZE INTO SSA2                                   
615600     MOVE '  GE' TO GODK-STATUSKODER                                      
615700     CALL CBLTDLI USING GU    XXLB-PCB DLI-IO-AREA13 SSA1 SSA2            
615800     MOVE XXLB-STATUS-CODE TO STATUS-WS                                   
615900     PERFORM IMS-STATUSKONTROLL                                           
616000     .                                                                    
616100     EJECT                                                                
616200 IMS-GU-WDM211 SECTION.                                                   
616300                                                                          
616400     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
616500          DELIMITED BY SIZE INTO SSA1                                     
616600     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
616700          DELIMITED BY SIZE INTO SSA2                                     
616800     MOVE '  GE'              TO GODK-STATUSKODER                         
616900     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
617000     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
617100     PERFORM IMS-STATUSKONTROLL                                           
617200     .                                                                    
617300                                                                          
617400 IMS-GNP-WDM221 SECTION.                                                  
617500                                                                          
617600     MOVE 'WDM221 '           TO SSA1                                     
617700     MOVE '    GE'            TO GODK-STATUSKODER                         
617800     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
617900     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
618000     PERFORM IMS-STATUSKONTROLL                                           
618100     .                                                                    
618200                                                                          
618300 IMS-GHU-WDM211 SECTION.                                                  
618400                                                                          
618500     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
618600          DELIMITED BY SIZE INTO SSA1                                     
618700     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
618800          DELIMITED BY SIZE INTO SSA2                                     
618900     MOVE '  GE'              TO GODK-STATUSKODER                         
619000     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
619100     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
619200     PERFORM IMS-STATUSKONTROLL                                           
619300     .                                                                    
619400                                                                          
619500 IMS-REPL-WDM211 SECTION.                                                 
619600                                                                          
619700     MOVE '  '             TO GODK-STATUSKODER                            
619800     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
619900     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
620000     PERFORM IMS-STATUSKONTROLL                                           
620100     .                                                                    
620200     EJECT                                                                
620300 IMS-GHU-WDM221 SECTION.                                                  
620400                                                                          
620500     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
620600          DELIMITED BY SIZE INTO SSA1                                     
620700     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
620800          DELIMITED BY SIZE INTO SSA2                                     
620900     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
621000          DELIMITED BY SIZE INTO SSA3                                     
621100     MOVE '  GE' TO GODK-STATUSKODER                                      
621200     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
621300     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
621400     PERFORM IMS-STATUSKONTROLL                                           
621500     .                                                                    
621600                                                                          
621700 IMS-REPL-WDM221 SECTION.                                                 
621800                                                                          
621900     MOVE '  '             TO GODK-STATUSKODER                            
622000     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
622100     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
622200     PERFORM IMS-STATUSKONTROLL                                           
622300     .                                                                    
622400                                                                          
622500 IMS-GU-ORQI01-CSEQ SECTION.                                              
622600                                                                          
622700     MOVE 'IMS-GU-ORQI01-CSEQ    ' TO CURRENT-IMS-SECTION                 
622800     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
622900             DELIMITED BY SIZE INTO SSA1                                  
623000     MOVE    '  GE'              TO GODK-STATUSKODER                      
623100     CALL    CBLTDLI USING       GU ORQICSQ-PCB OHUV-WDQ201               
623200                                    SSA1                                  
623300     MOVE    ORQICSQ-STATUS-CODE TO STATUS-WS                             
623400     PERFORM IMS-STATUSKONTROLL                                           
623500     .                                                                    
623600                                                                          
623700 IMS-GU-WDQ201 SECTION.                                                   
623800                                                                          
624000     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
624100            DELIMITED BY SIZE INTO SSA1                                   
624200     MOVE '  ' TO GODK-STATUSKODER                                        
624300     CALL CBLTDLI USING GU     ORQI-PCB OHUV-WDQ201 SSA1                  
624400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
624500     PERFORM IMS-STATUSKONTROLL                                           
624600     SKIP2                                                                
624700     .                                                                    
624800 IMS-GHNP-WDQ212 SECTION.                                                 
624900                                                                          
624910     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
624920            DELIMITED BY SIZE INTO SSA1                                   
625100     MOVE '  ' TO GODK-STATUSKODER                                        
625200     CALL CBLTDLI USING GHNP    ORQI-PCB ARB-WDQ212 SSA1                  
625300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
625400     PERFORM IMS-STATUSKONTROLL                                           
625500     SKIP2                                                                
625600     .                                                                    
625700                                                                          
625800 IMS-REPL-WDQ212 SECTION.                                                 
625900                                                                          
626100     MOVE '    ' TO GODK-STATUSKODER                                      
626200     CALL CBLTDLI USING REPL ORQI-PCB ARB-WDQ212                          
626300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
626400     PERFORM IMS-STATUSKONTROLL                                           
626500     .                                                                    
626600     SKIP2                                                                
626700 IMS-GU-XXKH11 SECTION.                                                   
626800                                                                          
626900     MOVE 'IMS-GU-XXKH11         ' TO CURRENT-IMS-SECTION                 
627000     STRING 'WLXXKH01(WDGXKEY  =' W-4447-X    ')'                         
627100            DELIMITED BY SIZE INTO SSA1                                   
627200     STRING 'WLXXKH11(WDGXKEY  =' W-4448-X    ')'                         
627300            DELIMITED BY SIZE INTO SSA2                                   
627400     MOVE '  '   TO GODK-STATUSKODER                                      
627500     CALL CBLTDLI USING GU XXKH-PCB 4448-WDGX4448-CTX SSA1 SSA2           
627600     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
627700     PERFORM IMS-STATUSKONTROLL                                           
627800     SKIP2                                                                
627900     .                                                                    
630200 IMS-GHU-XXDU01      SECTION.                                             
630300                                                                          
630400     MOVE 'IMS-GHU-XXDU01        ' TO CURRENT-IMS-SECTION                 
630500     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
630600            DELIMITED BY SIZE INTO SSA1                                   
630700     MOVE '  GE' TO GODK-STATUSKODER                                      
630800     CALL CBLTDLI USING GHU XXDU-PCB DLI-IO-AREA14 SSA1                   
630900     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
631000     PERFORM IMS-STATUSKONTROLL                                           
631100     .                                                                    
631200                                                                          
631300 IMS-GHNP-XXDU11-IDPLKLST    SECTION.                                     
631400*                                                                         
631500     MOVE 'IMS-GHNP-XXDU11-IDPLKL' TO CURRENT-IMS-SECTION                 
631600     STRING 'WLXXDU11(IDPLKLST =' W-4302-IDPLKLST-X ')'                   
631700            DELIMITED BY SIZE INTO SSA1                                   
631800     MOVE '  GE' TO GODK-STATUSKODER                                      
631900     CALL CBLTDLI USING GHNP XXDU-PCB DLI-IO-AREA14 SSA1                  
632000     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
632100     PERFORM IMS-STATUSKONTROLL                                           
632200     .                                                                    
632300 IMS-ISRT-WDL901 SECTION.                                                 
632400                                                                          
632500     MOVE 'IMS-ISRT-WDL901       ' TO CURRENT-IMS-SECTION                 
632600     MOVE 'WLLOGA01 ' TO SSA1                                             
632700     MOVE '  II' TO GODK-STATUSKODER                                      
632800     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
632900     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
633000     PERFORM IMS-STATUSKONTROLL                                           
633100     .                                                                    
633200     SKIP2                                                                
633300 IMS-INSERT-TRANS4349 SECTION.                                            
633400                                                                          
633500     MOVE 'IMS-INSERT-TRANS4349  ' TO CURRENT-IMS-SECTION                 
633600     MOVE LOW-VALUE            TO 4349-Z1                                 
633700                                  4349-Z2                                 
633800     MOVE SPACE                TO GODK-STATUSKODER                        
633900     CALL CBLTDLI USING ISRT ALT49-PCB 4349-MSG-IO-AREA                   
634000     MOVE ALT49-STATUS-CODE   TO STATUS-WS                                
634100     PERFORM IMS-STATUSKONTROLL                                           
634200     .                                                                    
634300     SKIP2                                                                
634400 IMS-GU-WDK601                 SECTION.                                   
634500                                                                          
634600     MOVE 'IMS-GU-WDK601         ' TO CURRENT-IMS-SECTION                 
634700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
634800            DELIMITED BY SIZE INTO SSA1                                   
634900     MOVE '  '                   TO GODK-STATUSKODER                      
635000     CALL  CBLTDLI  USING GU   WDK6-PCB DLI-IO-AREA8  SSA1                
635100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
635200     PERFORM IMS-STATUSKONTROLL                                           
635300     .                                                                    
635400     SKIP2                                                                
635500 IMS-GHNP-WDK611               SECTION.                                   
635600                                                                          
635700     MOVE 'IMS-GHNP-WDK611       ' TO CURRENT-IMS-SECTION                 
635800     MOVE 'WDK611  '           TO SSA1                                    
635900     MOVE '  '                 TO GODK-STATUSKODER                        
636000     CALL  CBLTDLI  USING GHNP WDK6-PCB DLI-IO-AREA8  SSA1                
636100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
636200     PERFORM IMS-STATUSKONTROLL                                           
636300     .                                                                    
636400     SKIP2                                                                
636500 IMS-GHU-WDK611               SECTION.                                    
636600                                                                          
636700     MOVE 'IMS-GHU-WDK611        ' TO CURRENT-IMS-SECTION                 
636800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
636900            DELIMITED BY SIZE INTO SSA1                                   
637000     MOVE 'WDK611  '           TO SSA2                                    
637100     MOVE '  '                 TO GODK-STATUSKODER                        
637200     CALL  CBLTDLI  USING GHU WDK6-PCB DLI-IO-AREA8  SSA1 SSA2            
637300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
637400     PERFORM IMS-STATUSKONTROLL                                           
637500     .                                                                    
637600     SKIP2                                                                
637700 IMS-REPL-WDK611               SECTION.                                   
637800                                                                          
637900     MOVE 'IMS-REPL-WDK611       ' TO CURRENT-IMS-SECTION                 
638000     MOVE 'WDK611  '           TO SSA1                                    
638100     MOVE '    '               TO GODK-STATUSKODER                        
638200     CALL  CBLTDLI  USING REPL WDK6-PCB DLI-IO-AREA8  SSA1                
638300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
638400     PERFORM IMS-STATUSKONTROLL                                           
638500     .                                                                    
638600     SKIP2                                                                
638700 IMS-GU-SEQB-WDA601 SECTION.                                              
638800     STRING 'WDA601  (WDA6BSEQ>=' W-WDA6BSEQ-MIN-X                        
638900                    '&WDA6BSEQ<=' W-WDA6BSEQ-MAX-X ')'                    
639000            DELIMITED BY SIZE INTO SSA1                                   
639100     MOVE '  GE'                 TO GODK-STATUSKODER                      
639200     CALL  CBLTDLI  USING GHU  WDA6B-PCB DLI-IO-AREA17 SSA1               
639300     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
639400     PERFORM IMS-STATUSKONTROLL                                           
639500     .                                                                    
639600     SKIP2                                                                
639700 IMS-GHU-SEQB-WDA601            SECTION.                                  
639800     MOVE 'IMS-GHU-SEQB-WDA601'  TO CURRENT-IMS-SECTION                   
639900                                                                          
640000     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
640100                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
640200            DELIMITED BY SIZE INTO SSA1                                   
640300     MOVE '  GE'                 TO GODK-STATUSKODER                      
640400     CALL  CBLTDLI  USING GHU   WDA6B-PCB DLI-IO-AREA17 SSA1              
640500     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
640600     PERFORM IMS-STATUSKONTROLL                                           
640700     .                                                                    
640800     SKIP2                                                                
640900 IMS-GHN-SEQB-WDA601            SECTION.                                  
641000     MOVE 'IMS-GHN-SEQB-WDA601'  TO CURRENT-IMS-SECTION                   
641100                                                                          
641200     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
641300                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
641400            DELIMITED BY SIZE INTO SSA1                                   
641500     MOVE '  GEGB'               TO GODK-STATUSKODER                      
641600     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA17 SSA1              
641700     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
641800     PERFORM IMS-STATUSKONTROLL                                           
641900     .                                                                    
642000     SKIP2                                                                
642100 IMS-REPL-SEQB-WDA601                 SECTION.                            
642200     MOVE 'IMS-REPL-SEQB-WDA601' TO CURRENT-IMS-SECTION                   
642300                                                                          
642400     MOVE 'WDA601  '           TO SSA1                                    
642500     MOVE '    '               TO GODK-STATUSKODER                        
642600     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA17 SSA1               
642700     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
642800     PERFORM IMS-STATUSKONTROLL                                           
642900     .                                                                    
643000     EJECT                                                                
643100 IMS-ISRT-WDA601            SECTION.                                      
643200     MOVE 'IMS-ISRT-WDA601'  TO CURRENT-IMS-SECTION                       
643300                                                                          
643400     MOVE   'WDA601  '         TO SSA1                                    
643500     MOVE '  IINI' TO GODK-STATUSKODER                                    
643600     CALL  CBLTDLI  USING ISRT WDA6-PCB DLI-IO-AREA17 SSA1                
643700     MOVE WDA6-STATUS-CODE     TO STATUS-WS                               
643800     PERFORM IMS-STATUSKONTROLL                                           
643900     .                                                                    
644000                                                                          
644100 IMS-GU-WDB601    SECTION.                                                
644200     MOVE 'IMS-GU-WDB601  '  TO CURRENT-IMS-SECTION                       
644300                                                                          
644400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
644500          DELIMITED BY SIZE INTO SSA1                                     
644600     MOVE '  GE' TO GODK-STATUSKODER                                      
644700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
644800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
644900     PERFORM IMS-STATUSKONTROLL                                           
645000     IF SEGMENT-SAKNAS                                                    
645100        MOVE SPACE TO DCS-KDDC                                            
645200     END-IF                                                               
645300     .                                                                    
645400     EJECT                                                                
657400 IMS-GU-GMTA-WDB201       SECTION.                                        
657500     MOVE 'IMS-GU-GMTA-WDB201  '   TO CURRENT-IMS-SECTION                 
657600                                                                          
657700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
657800                      DELIMITED BY SIZE INTO SSA1                         
657900     MOVE '  ' TO GODK-STATUSKODER                                        
658000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
658100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
658200     PERFORM IMS-STATUSKONTROLL                                           
658300     .                                                                    
658400     SKIP3                                                                
658500 IMS-PURGE-ALT5108-MSG    SECTION.                                        
658600     MOVE 'IMS-PURGE-ALT5108-MSG'   TO CURRENT-IMS-SECTION                
658700                                                                          
658800     MOVE LOW-VALUE        TO ALT5108-Z1 ALT5108-Z2                       
658900     MOVE '  '             TO GODK-STATUSKODER                            
659000     CALL CBLTDLI USING PURG ALT5108-PCB ALT5108-IO-AREA                  
659100     MOVE ALT5108-STATUS-CODE TO STATUS-WS                                
659200     PERFORM IMS-STATUSKONTROLL                                           
659300     .                                                                    
659400     SKIP3                                                                
659500 IMS-GU-WDQ3DSEQ SECTION.                                                 
659600     MOVE 'IMS-GU-WDQ3DSEQ'         TO CURRENT-IMS-SECTION                
659700                                                                          
659800     STRING 'WDQ301  (WDQ3DSEQ>=' W-WDQ3DSEQ-MIN-X                        
659900                    '&WDQ3DSEQ<=' W-WDQ3DSEQ-MAX-X ')'                    
660000          DELIMITED BY SIZE INTO SSA1                                     
660100     MOVE '  GE' TO GODK-STATUSKODER                                      
660200     CALL CBLTDLI USING GU WDQ3D-PCB DLI-IO-WDQ3D SSA1                    
660300     MOVE WDQ3D-STATUS-CODE TO STATUS-WS                                  
660400     PERFORM IMS-STATUSKONTROLL                                           
660500     .                                                                    
660600     SKIP3                                                                
660700 IMS-GN-WDQ3DSEQ SECTION.                                                 
660800     MOVE 'IMS-GN-WDQ3DSEQ'         TO CURRENT-IMS-SECTION                
660900                                                                          
661000     STRING 'WDQ301  (WDQ3DSEQ>=' W-WDQ3DSEQ-MIN-X                        
661100                    '&WDQ3DSEQ<=' W-WDQ3DSEQ-MAX-X ')'                    
661200          DELIMITED BY SIZE INTO SSA1                                     
661300     MOVE '  GE' TO GODK-STATUSKODER                                      
661400     CALL CBLTDLI USING GN WDQ3D-PCB DLI-IO-WDQ3D SSA1                    
661500     MOVE WDQ3D-STATUS-CODE TO STATUS-WS                                  
661600     PERFORM IMS-STATUSKONTROLL                                           
661700     .                                                                    
661800                                                                          
661900 IMS-GU-WDP4A1 SECTION.                                                   
662000                                                                          
662100     STRING 'WDP4A1  (IDDISTRF<=' W-IDDISTR-P4-X                          
662200                    '&IDDISTRT>=' W-IDDISTR-P4-X ')'                      
662300          DELIMITED BY SIZE INTO SSA1                                     
662400     MOVE '  GE' TO GODK-STATUSKODER                                      
662500     CALL CBLTDLI USING GU WDP4A-PCB DLI-IO-AREA-WDP4A1                   
662600                           SSA1                                           
662700     MOVE WDP4A-STATUS-CODE    TO STATUS-WS                               
662800     PERFORM IMS-STATUSKONTROLL                                           
662900     .                                                                    
663000                                                                          
663100     SKIP3                                                                
663200 IMS-GU-WDA501 SECTION.                                                   
663210                                                                          
663220     MOVE 'IMS-GU-WDA501'           TO CURRENT-IMS-SECTION                
663300                                                                          
663410     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-A5-MIN-X                     
663500                    '&WDA501KY<=' W-WDA501KY-A5-MAX-X                     
663600                    '&KDORDKL  =' W-KDORDKL-X                             
663700                    '&IDKNDRFL =' W-IDKUNDRF-LEV-X ')'                    
663800          DELIMITED BY SIZE INTO SSA1                                     
663900     MOVE '  GE'              TO GODK-STATUSKODER                         
664000     CALL CBLTDLI USING GU ORDP1-PCB DLI-IO-AREA6   SSA1                  
664100     MOVE ORDP1-STATUS-CODE   TO STATUS-WS                                
664200     PERFORM IMS-STATUSKONTROLL                                           
664300     .                                                                    
664400 DB2-SELECT-TP4TRAN     SECTION.                                          
664500     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
664600                                                                          
664700     MOVE 000100 TO GODK-SQLCODEKODER                                     
664800                                                                          
664900     EXEC SQL                                                             
665000           SELECT  DISTINCT                                               
665100                   IDDC_REC                                               
665200                                                                          
665300           INTO   :TP4TRAN-IDDC-REC                                       
665400                                                                          
665500           FROM    TP4TRAN                                                
665600                                                                          
665700           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
665800     END-EXEC                                                             
665900                                                                          
666000     MOVE SQLCODE TO SQLCODE-WS                                           
666100     PERFORM DB2-STATUSKONTROLL                                           
666200     .                                                                    
666300     EJECT                                                                
666400 IMS-STATUSKONTROLL SECTION.                                              
666500     SET STATUS-IX TO 1                                                   
666600     SEARCH GODK-STATUS AT END CALL FELLOG                                
666700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
666800     END-SEARCH                                                           
666900     .                                                                    
667000     EJECT                                                                
667100 DB2-STATUSKONTROLL  SECTION.                                             
667200                                                                          
667300     SET SQLCODE-IX TO 1                                                  
667400     SEARCH GODK-SQLCODE                                                  
667500       AT END                                                             
667600          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
667700          DELIMITED BY SIZE INTO ERRORTEX                                 
667800          CALL ABEND USING RKOD-ABEND-DB2                                 
667900       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
668000     END-SEARCH                                                           
668100     .                                                                    
